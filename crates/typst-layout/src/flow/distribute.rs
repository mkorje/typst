use std::convert::Infallible;

use ecow::EcoVec;
use typst_library::diag::SourceDiagnostic;
use typst_library::introspection::Tag;
use typst_library::layout::{
    Abs, Axes, FixedAlignment, Fr, Frame, FrameItem, PlacementScope, Point, Region,
    Regions, Rel, Size,
};
use typst_utils::Numeric;

use super::compose::{
    Composer, FloatStop, FootnoteStop, Migration, RelayoutResult, RelayoutStop,
};
use super::{
    BreakClass, BreakId, Child, ChildId, ComposeStop, LineChild, MultiChild, MultiLayout,
    MultiLayoutContext, MultiSpill, PlacedChild, SingleChild, StickyObservation,
    StickyScore, StickyTrace, Work,
};

/// The result type for internal distributor control flow.
///
/// The `Err(_)` variant incorporates control flow events for finishing and
/// relayouting regions.
type FlowResult<T> = Result<T, Stop>;

/// A control flow event during distribution.
enum Stop {
    /// Indicates that the current subregion should be finished.
    Finish(Finish),
    /// Indicates that the given scope should be relayouted.
    Relayout(PlacementScope),
    /// Another sticky choice is needed before distribution can finish.
    Sticky(StickyObservation),
    /// The actual layout of a fractional block requires its origin to move.
    /// Replay the column and finish immediately before this child.
    MigrateFractional(ChildId),
    /// A fatal error.
    Error(EcoVec<SourceDiagnostic>),
}

/// The reason why the current region should finish.
enum Finish {
    /// A lack of space.
    Soft,
    /// An explicit break.
    Forced,
}

impl From<EcoVec<SourceDiagnostic>> for Stop {
    fn from(error: EcoVec<SourceDiagnostic>) -> Self {
        Self::Error(error)
    }
}

impl From<ComposeStop> for Stop {
    fn from(stop: ComposeStop) -> Self {
        match stop {
            ComposeStop::Sticky(observation) => Self::Sticky(observation),
            ComposeStop::Error(error) => Self::Error(error),
        }
    }
}

impl From<FootnoteStop> for Stop {
    fn from(stop: FootnoteStop) -> Self {
        match stop {
            FootnoteStop::Relayout(()) => Self::Relayout(PlacementScope::Column),
            FootnoteStop::MigrateOrigin(()) => Self::Finish(Finish::Soft),
            FootnoteStop::Error(error) => Self::Error(error),
        }
    }
}

impl From<FloatStop> for Stop {
    fn from(stop: FloatStop) -> Self {
        match stop {
            FloatStop::Relayout(scope) => Self::Relayout(scope),
            FloatStop::MigrateOrigin(()) => Self::Finish(Finish::Soft),
            FloatStop::Error(error) => Self::Error(error),
        }
    }
}

/// Distributes as many children as fit from `composer.work` into the first
/// region and returns the resulting frame and the height actually used
/// by the inner contents (for column balancing).
pub fn distribute(
    composer: &mut Composer,
    regions: Regions,
    balancing_target: Option<Abs>,
) -> RelayoutResult<(Frame, Abs)> {
    composer.engine.sink.begin_effect_transaction();
    let fractional_break = composer.fractional_break.take();
    let fractional_forbid = composer.fractional_forbid.take();
    let mut distributor = Distributor {
        composer,
        regions,
        items: vec![],
        used: Size::zero(),
        balancing_target,
        restored_init: false,
        fractional_break,
        fractional_forbid,
    };
    let init = distributor.snapshot();
    let (flush, explicit) = match distributor.run() {
        Ok(()) => (distributor.composer.work.done(), false),
        Err(Stop::Finish(Finish::Soft)) => (false, false),
        Err(Stop::Finish(Finish::Forced)) => (true, true),
        Err(stop) => return Err(distributor.handle_stop(stop)),
    };
    let region = Region::new(regions.size, regions.expand);
    let result = distributor.finalize(region, init, flush, explicit);
    match result {
        Ok(output) => {
            if distributor.restored_init {
                distributor.engine_rollback();
            } else {
                distributor.engine_commit();
            }
            Ok(output)
        }
        Err(stop) => Err(distributor.handle_stop(stop)),
    }
}

/// State for distribution.
///
/// See [Composer] regarding lifetimes.
struct Distributor<'a, 'b, 'x, 'y, 'z> {
    /// The composer that is used to handle insertions.
    composer: &'z mut Composer<'a, 'b, 'x, 'y>,
    /// Regions which are continuously shrunk as new items are added.
    regions: Regions<'z>,
    /// Already laid out items, not yet aligned.
    items: Vec<Item<'a, 'b>>,
    /// Size used by laid out items.
    used: Size,
    /// The target height for column balancing.
    balancing_target: Option<Abs>,
    /// Whether finalization restored the empty region-entry snapshot.
    restored_init: bool,
    /// A fractional origin which should start in the following region.
    fractional_break: Option<ChildId>,
    /// A fractional origin which was already at a no-progress region top.
    fractional_forbid: Option<ChildId>,
}

/// A snapshot of the distribution state.
struct DistributionSnapshot<'a, 'b> {
    work: Work<'a, 'b>,
    items: usize,
    used: Size,
    trace: StickyTrace,
    choices: usize,
}

/// A laid out item in a distribution.
enum Item<'a, 'b> {
    /// An introspection tag.
    Tag(&'a Tag),
    /// Absolute spacing and its weakness level.
    Abs(Abs, u8),
    /// Fractional spacing or a fractional block.
    Fr(Fr, u8, Option<FrItem<'a, 'b>>),
    /// A frame for a laid out line or block.
    Frame(Frame, Axes<FixedAlignment>, FlowFrame),
    /// A frame for an absolutely (not floatingly) placed child.
    Placed(Frame, &'b PlacedChild<'a>),
}

#[derive(Copy, Clone)]
struct FrItem<'a, 'b> {
    child: ChildId,
    single: &'b SingleChild<'a>,
    preliminary_nonempty: bool,
}

#[derive(Copy, Clone)]
struct FlowFrame {
    child: ChildId,
    sticky: bool,
    complete: bool,
    nested_score: StickyScore,
}

impl Item<'_, '_> {
    /// Whether this item should be migrated to the next region if the region
    /// consists solely of such items.
    fn migratable(&self) -> bool {
        match self {
            Self::Tag(_) => true,
            Self::Frame(frame, ..) => {
                frame.size().is_zero()
                    && frame.items().all(|(_, item)| {
                        matches!(item, FrameItem::Link(_, _) | FrameItem::Tag(_))
                    })
            }
            Self::Placed(_, placed) => !placed.float,
            _ => false,
        }
    }
}

impl<'a, 'b> Distributor<'a, 'b, '_, '_, '_> {
    /// Distributes content into the region.
    fn run(&mut self) -> FlowResult<()> {
        // First, handle spill of a breakable block.
        if let Some(spill) = self.composer.work.spill.take() {
            self.multi_spill(spill)?;
        }

        // If spill are taken care of, process children until no space is left
        // or no children are left.
        while let Some((id, child)) = self.composer.work.head() {
            if self.fractional_break == Some(id) {
                self.fractional_break = None;

                // Repeated migration at an unchanged region top cannot
                // progress.
                if self.can_finish_before_fractional() {
                    return Err(Stop::Finish(Finish::Soft));
                }
                self.fractional_forbid = Some(id);
            }

            let class = if child.sticky() {
                Some(BreakClass::StickyStart)
            } else if self.composer.config.tail_breaks && child.optional_break_before() {
                Some(BreakClass::StickyTail)
            } else {
                None
            };

            if let Some(class) = class
                && self.can_break_before_sticky()
            {
                let id = BreakId {
                    path: self.composer.config.path.clone(),
                    child: id,
                    class,
                };
                let migrate =
                    self.composer.choices.decide(id).map_err(Stop::Sticky)?;
                if migrate {
                    return Err(Stop::Finish(Finish::Soft));
                }
            }

            self.child(id, child)?;
            self.composer.work.advance();
        }

        Ok(())
    }

    /// Processes a single child.
    ///
    /// - Returns `Ok(())` if the child was successfully processed.
    /// - Returns `Err(Stop::Finish)` if a region break should be triggered.
    /// - Returns `Err(Stop::Relayout(_))` if the region needs to be relayouted
    ///   due to an insertion (float/footnote).
    /// - Returns `Err(Stop::Sticky(_))` if a sticky choice is still pending.
    /// - Returns `Err(Stop::Error(_))` if there was a fatal error.
    fn child(&mut self, id: ChildId, child: &'b Child<'a>) -> FlowResult<()> {
        match child {
            Child::Tag(_) => self.tag(id),
            Child::Rel(amount, weakness) => self.rel(*amount, *weakness),
            Child::Fr(fr, weakness) => self.fr(*fr, *weakness),
            Child::Line(line) => self.line(id, line)?,
            Child::Single(single) => self.single(id, single)?,
            Child::Multi(multi) => self.multi(id, multi)?,
            Child::Placed(placed) => self.placed(id, placed)?,
            Child::Flush => self.flush()?,
            Child::Break(weak) => self.break_(*weak)?,
        }
        Ok(())
    }

    /// Whether finishing before a sticky child can change its placement. At
    /// the top of a repeating region, taking the choice would only create an
    /// infinite sequence of empty regions.
    fn can_break_before_sticky(&self) -> bool {
        let has_in_flow = self.items.iter().any(|item| match item {
            Item::Frame(frame, ..) => !frame.is_empty(),
            Item::Fr(_, _, Some(fr_item)) => fr_item.preliminary_nonempty,
            _ => false,
        });
        if has_in_flow {
            // Unlike overflow migration, a deliberate sticky break is useful
            // even when the next region has the same geometry: emitting the
            // current non-empty region is itself progress.
            return self.regions.may_break();
        }

        self.next_region_is_taller()
    }

    /// Whether replaying the column and stopping before a fractional origin
    /// makes concrete progress.
    fn can_finish_before_fractional(&self) -> bool {
        if !self.regions.may_break() {
            return false;
        }

        if self.items.iter().any(|item| !item.migratable()) {
            return true;
        }

        self.next_region_is_taller()
    }

    fn next_region_is_taller(&self) -> bool {
        self.regions
            .backlog
            .first()
            .copied()
            .or(self.regions.last)
            .is_some_and(|height| height > self.regions.size.y)
    }

    /// Processes a tag.
    fn tag(&mut self, id: ChildId) {
        self.composer.work.tags.push(id);
    }

    /// Generate items for pending tags.
    fn flush_tags(&mut self) {
        if !self.composer.work.tags.is_empty() {
            let tags = std::mem::take(&mut self.composer.work.tags);
            self.items
                .extend(tags.into_iter().map(|id| Item::Tag(self.composer.work.tag(id))));
        }
    }

    /// Mark the amount of height used and reduce the region height accordingly.
    fn use_height(&mut self, amount: Abs) {
        self.regions.size.y -= amount;
        self.used.y += amount;
    }

    /// Processes relative spacing.
    fn rel(&mut self, amount: Rel<Abs>, weakness: u8) {
        let amount = amount.relative_to(self.regions.base().y);
        if weakness > 0 && !self.keep_weak_rel_spacing(amount, weakness) {
            return;
        }

        self.use_height(amount);
        self.items.push(Item::Abs(amount, weakness));
    }

    /// Processes fractional spacing.
    fn fr(&mut self, fr: Fr, weakness: u8) {
        if weakness > 0 && !self.keep_weak_fr_spacing(fr, weakness) {
            return;
        }

        // If we decided to keep the fr spacing, it's safe to trim previous
        // spacing as no stronger fr spacing can exist.
        self.trim_spacing();

        self.items.push(Item::Fr(fr, weakness, None));
    }

    /// Decides whether to keep weak spacing based on previous items. If there
    /// is a preceding weak spacing, it might be patched in place.
    fn keep_weak_rel_spacing(&mut self, amount: Abs, weakness: u8) -> bool {
        for item in self.items.iter_mut().rev() {
            match *item {
                // When previous weak relative spacing exists that's at most as
                // weak, we reuse the old item, set it to the maximum of both,
                // and discard the new item.
                Item::Abs(prev_amount, prev_weakness @ 1..) => {
                    if weakness <= prev_weakness
                        && (weakness < prev_weakness || amount > prev_amount)
                    {
                        *item = Item::Abs(amount, weakness);
                        self.use_height(amount - prev_amount);
                    }
                    return false;
                }
                // These are "peeked beyond" for spacing collapsing purposes.
                Item::Tag(_) | Item::Abs(_, 0) | Item::Placed(..) => {}
                // Any kind of fractional spacing destructs weak relative
                // spacing.
                Item::Fr(.., None) => return false,
                // These naturally support the spacing.
                Item::Frame(..) | Item::Fr(.., Some(_)) => return true,
            }
        }
        false
    }

    /// Decides whether to keep weak fractional spacing based on previous items.
    /// If there is a preceding weak spacing, it might be patched in place.
    fn keep_weak_fr_spacing(&mut self, fr: Fr, weakness: u8) -> bool {
        for item in self.items.iter_mut().rev() {
            match *item {
                // When previous weak fr spacing exists that's at most as weak,
                // we reuse the old item, set it to the maximum of both, and
                // discard the new item.
                Item::Fr(prev_fr, prev_weakness @ 1.., None) => {
                    if weakness <= prev_weakness
                        && (weakness < prev_weakness || fr > prev_fr)
                    {
                        *item = Item::Fr(fr, weakness, None);
                    }
                    return false;
                }
                // These are "peeked beyond" for spacing collapsing purposes.
                // Weak absolute spacing, in particular, will be trimmed once
                // we push the fractional spacing.
                Item::Tag(_) | Item::Abs(..) | Item::Placed(..) => {}
                // For weak + strong fr spacing, we keep both, same as for
                // weak + strong rel spacing.
                Item::Fr(.., None) => return true,
                // These naturally support the spacing.
                Item::Frame(..) | Item::Fr(.., Some(_)) => return true,
            }
        }
        false
    }

    /// Trims trailing weak spacing from the items.
    fn trim_spacing(&mut self) {
        for (i, item) in self.items.iter().enumerate().rev() {
            match *item {
                Item::Abs(amount, 1..) => {
                    self.use_height(-amount);
                    self.items.remove(i);
                    break;
                }
                Item::Fr(_, 1.., None) => {
                    self.items.remove(i);
                    break;
                }
                Item::Tag(_) | Item::Abs(..) | Item::Placed(..) => {}
                Item::Frame(..) | Item::Fr(..) => break,
            }
        }
    }

    /// The amount of trailing weak spacing.
    fn weak_spacing(&mut self) -> Abs {
        for item in self.items.iter().rev() {
            match *item {
                Item::Abs(amount, 1..) => return amount,
                Item::Tag(_) | Item::Abs(..) | Item::Placed(..) => {}
                Item::Frame(..) | Item::Fr(..) => break,
            }
        }
        Abs::zero()
    }

    /// Whether the amount fits into the remaining region, taking into account
    /// column balancing limits.
    pub fn fits(&self, amount: Abs) -> bool {
        self.regions.size.y.fits(amount)
            && self
                .balancing_target
                // Add elements as long as the balancing target is not reached. By not including
                // the amount itself here, we avoid protruding items to cumulate in the last column.
                .is_none_or(|target| target.fits(self.used.y))
    }

    /// Processes a line of a paragraph.
    fn line(&mut self, id: ChildId, line: &'b LineChild) -> FlowResult<()> {
        // If the line doesn't fit and a followup region may improve things,
        // finish the region.
        if !self.fits(line.frame.height()) && self.regions.may_progress() {
            return Err(Stop::Finish(Finish::Soft));
        }

        // If the line's need, which includes its own height and that of
        // following lines grouped by widow/orphan prevention, does not fit into
        // the current region, but does fit into the next region, finish the
        // region.
        if !self.fits(line.need)
            && self
                .regions
                .iter()
                .nth(1)
                .is_some_and(|region| region.y.fits(line.need))
        {
            return Err(Stop::Finish(Finish::Soft));
        }

        self.frame(
            line.frame.clone(),
            line.align,
            false,
            FlowFrame {
                child: id,
                sticky: false,
                complete: true,
                nested_score: StickyScore::default(),
            },
        )?;
        Ok(())
    }

    /// Processes an unbreakable block.
    fn single(&mut self, id: ChildId, single: &'b SingleChild<'a>) -> FlowResult<()> {
        self.composer.engine.sink.begin_effect_transaction();

        // Lay out the block.
        let frame = match single.layout(
            self.composer.engine,
            Region::new(self.regions.base(), self.regions.expand),
            self.composer.attempt,
        ) {
            Ok(frame) => frame,
            Err(error) => {
                self.composer.engine.sink.commit_effect_transaction();
                return Err(Stop::Error(error));
            }
        };

        // Handle fractionally sized blocks.
        if let Some(fr) = single.fr {
            if self.composer.config.optimize {
                self.composer.engine.sink.rollback_effect_transaction();
            } else {
                // The sticky-free path discovers footnotes in the preliminary
                // frame.
                let result = self
                    .composer
                    .footnotes(
                        &self.regions,
                        &frame,
                        Abs::zero(),
                        false,
                        Migration::ALLOW,
                    )
                    .map_err(Stop::from);
                self.finish_child_effects(&result);
                result?;
            }
            self.flush_tags();
            self.items.push(Item::Fr(
                fr,
                0,
                Some(FrItem {
                    child: id,
                    single,
                    preliminary_nonempty: !frame.is_empty(),
                }),
            ));
            return Ok(());
        }

        // If the block doesn't fit and a followup region may improve things,
        // finish the region.
        if !self.fits(frame.height()) && self.regions.may_progress() {
            self.composer.engine.sink.rollback_effect_transaction();
            return Err(Stop::Finish(Finish::Soft));
        }

        let result = self.frame(
            frame,
            single.align,
            false,
            FlowFrame {
                child: id,
                sticky: single.sticky,
                complete: true,
                nested_score: StickyScore::default(),
            },
        );
        self.finish_child_effects(&result);
        result?;
        Ok(())
    }

    /// Processes a breakable block.
    fn multi(&mut self, id: ChildId, multi: &'b MultiChild<'a>) -> FlowResult<()> {
        let mut pod = self.regions;

        // For column balancing, reduce the region size for layout.
        if let Some(lim) = self.balancing_target {
            let remaining = lim - self.used.y;
            pod.size.y.set_min(remaining);
        }

        // Skip directly if the region is already (over)full. `line` and
        // `single` implicitly do this through their `fits` checks.
        if pod.is_full() {
            return Err(Stop::Finish(Finish::Soft));
        }

        self.composer.engine.sink.begin_effect_transaction();
        let choices_checkpoint = self.composer.choices.checkpoint();

        // Lay out the block.
        let context = MultiLayoutContext {
            attempt: self.composer.attempt,
            child: id,
            path: self.composer.config.path.clone(),
            controlled: self.composer.config.optimize,
            choices: self.composer.choices,
        };
        let output = match multi.layout(self.composer.engine, pod, context) {
            Ok(output) => output,
            Err(ComposeStop::Sticky(observation)) => {
                self.composer.engine.sink.rollback_effect_transaction();
                return Err(Stop::Sticky(observation));
            }
            Err(ComposeStop::Error(error)) => {
                self.composer.engine.sink.commit_effect_transaction();
                return Err(Stop::Error(error));
            }
        };
        let MultiLayout { frame, spill, completed_score: nested_score } = output;
        if frame.is_empty()
            && spill.as_ref().is_some_and(|s| s.exist_non_empty_frame)
            && self.regions.may_progress()
        {
            // If the first frame is empty, but there are non-empty frames in
            // the spill, the whole child should be put in the next region to
            // avoid any invisible orphans at the end of this region.
            self.composer.choices.restore(choices_checkpoint);
            self.composer.engine.sink.rollback_effect_transaction();
            return Err(Stop::Finish(Finish::Soft));
        }

        self.accept_multi(
            id,
            multi.sticky,
            multi.align,
            MultiLayout { frame, spill, completed_score: nested_score },
            choices_checkpoint,
            None,
        )
    }

    /// Processes spillover from a breakable block.
    fn multi_spill(&mut self, spill: MultiSpill<'a, 'b>) -> FlowResult<()> {
        let mut pod = self.regions;

        // For column balancing, reduce the region size for layout.
        if let Some(lim) = self.balancing_target {
            let remaining = lim - self.used.y;
            pod.size.y.set_min(remaining);
        }

        // Skip directly if the region is already (over)full.
        if pod.is_full() {
            self.composer.work.spill = Some(spill);
            return Err(Stop::Finish(Finish::Soft));
        }

        let original = spill.clone();
        let choices_checkpoint = self.composer.choices.checkpoint();
        self.composer.engine.sink.begin_effect_transaction();

        // Lay out the spilled remains.
        let child = spill.child_id();
        let sticky = spill.sticky();
        let align = spill.align();
        let output = match spill.layout(
            self.composer.engine,
            pod,
            self.composer.attempt,
            self.composer.choices,
        ) {
            Ok(output) => output,
            Err(ComposeStop::Sticky(observation)) => {
                self.composer.work.spill = Some(original);
                self.composer.choices.restore(choices_checkpoint);
                self.composer.engine.sink.rollback_effect_transaction();
                return Err(Stop::Sticky(observation));
            }
            Err(ComposeStop::Error(error)) => {
                self.composer.engine.sink.commit_effect_transaction();
                return Err(Stop::Error(error));
            }
        };
        self.accept_multi(
            child,
            sticky,
            align,
            output,
            choices_checkpoint,
            Some(original),
        )
    }

    fn accept_multi(
        &mut self,
        child: ChildId,
        sticky: bool,
        align: Axes<FixedAlignment>,
        output: MultiLayout<'a, 'b>,
        choices_checkpoint: usize,
        original: Option<MultiSpill<'a, 'b>>,
    ) -> FlowResult<()> {
        let continuation = original.is_some();
        let MultiLayout { frame, spill, completed_score } = output;
        let complete = spill.is_none();

        let result = self.frame(
            frame,
            align,
            true,
            FlowFrame {
                child,
                sticky,
                complete,
                nested_score: completed_score.unwrap_or_default(),
            },
        );
        if matches!(&result, Err(stop) if !matches!(stop, Stop::Error(_))) {
            if let Some(original) = original {
                self.composer.work.spill = Some(original);
            }
            self.composer.choices.restore(choices_checkpoint);
        }
        self.finish_child_effects(&result);
        result?;

        if let Some(spill) = spill {
            self.composer.work.spill = Some(spill);
            if !continuation {
                self.composer.work.advance();
            }
            return Err(Stop::Finish(Finish::Soft));
        }

        Ok(())
    }

    /// Processes an in-flow frame, generated from a line or block.
    fn frame(
        &mut self,
        frame: Frame,
        align: Axes<FixedAlignment>,
        breakable: bool,
        flow: FlowFrame,
    ) -> FlowResult<()> {
        self.composer.footnotes(
            &self.regions,
            &frame,
            frame.height(),
            breakable,
            Migration::ALLOW,
        )?;

        // Push an item for the frame.
        self.use_height(frame.height());
        self.used.x.set_max(frame.width());
        self.flush_tags();
        self.items.push(Item::Frame(frame, align, flow));
        Ok(())
    }

    /// Processes an absolutely or floatingly placed child.
    fn placed(&mut self, id: ChildId, placed: &'b PlacedChild<'a>) -> FlowResult<()> {
        self.composer.engine.sink.begin_effect_transaction();
        if placed.float {
            // If the element is floatingly placed, let the composer handle it.
            // It might require relayout because the area available for
            // distribution shrinks. We make the spacing occupied by weak
            // spacing temporarily available again because it can collapse if it
            // ends up at a break due to the float.
            let weak_spacing = self.weak_spacing();
            self.use_height(-weak_spacing);
            let result = self.composer.float(
                id,
                placed,
                &self.regions,
                self.items.iter().any(|item| matches!(item, Item::Frame(..))),
                Migration::ALLOW,
            );
            match result {
                Ok(()) => {
                    // A queued/skipped float has not been committed visually;
                    // it will be laid out again in the insertion area.
                    self.composer.engine.sink.rollback_effect_transaction();
                    self.use_height(weak_spacing);
                }
                Err(FloatStop::Relayout(scope)) => {
                    // `Composer::float` promoted only the insertion-owned
                    // effects. The surrounding child attempt is abandoned and
                    // will be replayed after the insertion fixed point settles.
                    self.composer.engine.sink.rollback_effect_transaction();
                    return Err(Stop::Relayout(scope));
                }
                Err(FloatStop::MigrateOrigin(())) => {
                    self.composer.engine.sink.rollback_effect_transaction();
                    return Err(Stop::Finish(Finish::Soft));
                }
                Err(FloatStop::Error(error)) => {
                    self.composer.engine.sink.commit_effect_transaction();
                    return Err(Stop::Error(error));
                }
            }
        } else {
            let frame = match placed.layout(
                self.composer.engine,
                self.regions.base(),
                self.composer.attempt,
            ) {
                Ok(frame) => frame,
                Err(error) => {
                    self.composer.engine.sink.commit_effect_transaction();
                    return Err(Stop::Error(error));
                }
            };
            let result = self
                .composer
                .footnotes(&self.regions, &frame, Abs::zero(), true, Migration::ALLOW)
                .map_err(Stop::from);
            self.finish_child_effects(&result);
            result?;
            self.flush_tags();
            self.items.push(Item::Placed(frame, placed));
        }
        Ok(())
    }

    /// Processes a float flush.
    fn flush(&mut self) -> FlowResult<()> {
        // If there are still pending floats, finish the region instead of
        // adding more content to it.
        if !self.composer.work.floats.is_empty() {
            return Err(Stop::Finish(Finish::Soft));
        }
        Ok(())
    }

    /// Processes a column break.
    fn break_(&mut self, weak: bool) -> FlowResult<()> {
        // If there is a region to break into, break into it.
        if (!weak || !self.items.is_empty())
            && (!self.regions.backlog.is_empty() || self.regions.last.is_some())
        {
            self.composer.work.advance();
            return Err(Stop::Finish(Finish::Forced));
        }
        Ok(())
    }

    /// Arranges the produced items into an output frame.
    ///
    /// This performs alignment and resolves fractional spacing and blocks.
    fn finalize(
        &mut self,
        region: Region,
        init: DistributionSnapshot<'a, 'b>,
        flush: bool,
        explicit: bool,
    ) -> FlowResult<(Frame, Abs)> {
        let trace_init = init.trace.clone();
        if flush {
            // If this is the very end of the flow, flush pending tags.
            self.flush_tags();
        } else if !self.items.is_empty() && self.items.iter().all(Item::migratable) {
            // Restore the initial state of all items are migratable.
            self.restore(init);
        }

        self.trim_spacing();

        let used_height_without_fr = self.used.y;

        // Determine the sum of fractionals.
        let mut frs = Fr::zero();
        let mut has_fr_child = false;
        for item in &self.items {
            if let Item::Fr(v, _, child) = item {
                frs += *v;
                has_fr_child |= child.is_some();
            }
        }

        // When we have fractional spacing, occupy the remaining space with it.
        let mut fr_space = Abs::zero();
        if frs.get() > 0.0 && region.size.y.is_finite() {
            fr_space = region.size.y - self.used.y;
            self.used.y = region.size.y;
        }

        // Lay out fractionally sized blocks.
        let mut fr_frames = vec![];
        if has_fr_child {
            // Preliminary layouts were rolled back in optimized flows.
            let fractional_attempt = if self.composer.config.optimize {
                self.composer.config.next_attempt()
            } else {
                self.composer.attempt
            };
            let fr_items: Vec<_> = self
                .items
                .iter()
                .filter_map(|item| match item {
                    Item::Fr(fr, _, Some(child)) => Some((*fr, *child)),
                    _ => None,
                })
                .collect();
            for (fr, fr_item) in fr_items {
                let length = fr.share(frs, fr_space);
                let pod = Region::new(Size::new(region.size.x, length), region.expand);
                let frame = fr_item.single.layout(
                    self.composer.engine,
                    pod,
                    fractional_attempt,
                )?;
                if self.composer.config.optimize {
                    self.fractional_footnotes(fr_item.child, &frame)?;
                }
                self.used.x.set_max(frame.width());
                fr_frames.push(frame);
            }
        }

        // Rebuild sticky state from the retained frames, using the allocated
        // frames for fractional children.
        self.composer.trace.restore(trace_init);
        let mut actual_fr_frames = fr_frames.iter();
        for item in &self.items {
            let (frame, flow) = match item {
                Item::Frame(frame, _, flow) => (frame, *flow),
                Item::Fr(_, _, Some(fr_item)) => {
                    let Some(frame) = actual_fr_frames.next() else {
                        unreachable!("fractional block replay lost its allocated frame");
                    };
                    (
                        frame,
                        FlowFrame {
                            child: fr_item.child,
                            sticky: fr_item.single.sticky,
                            complete: true,
                            nested_score: StickyScore::default(),
                        },
                    )
                }
                _ => continue,
            };

            self.composer.trace.begin(flow.child, flow.sticky);
            if !frame.is_empty() {
                self.composer.trace.place(flow.child);
            }
            if flow.complete {
                self.composer.trace.finish(flow.child);
                self.composer.trace.add_score(flow.nested_score);
            }
        }
        if explicit {
            self.composer.trace.force_break();
        }

        // Also consider the width of insertions for alignment.
        if !region.expand.x {
            self.used.x.set_max(self.composer.insertion_width());
        }

        // Determine the region's size.
        let size = region.expand.select(region.size, self.used.min(region.size));
        let free = size.y - self.used.y;

        let mut output = Frame::soft(size);
        let mut ruler = FixedAlignment::Start;
        let mut offset = Abs::zero();
        let mut fr_frames = fr_frames.into_iter();

        // Position all items.
        let mut baseline_set = false;
        for item in std::mem::take(&mut self.items) {
            match item {
                Item::Tag(tag) => {
                    let y = offset + ruler.position(free);
                    let pos = Point::with_y(y);
                    output.push(pos, FrameItem::Tag(tag.clone()));
                }
                Item::Abs(v, _) => {
                    offset += v;
                }
                Item::Fr(v, _, single) => {
                    let length = v.share(frs, fr_space);
                    if let Some(fr_item) = single {
                        let Some(frame) = fr_frames.next() else {
                            unreachable!(
                                "fractional block output lost its allocated frame"
                            );
                        };
                        let x = fr_item.single.align.x.position(size.x - frame.width());
                        let pos = Point::new(x, offset);
                        output.push_frame(pos, frame);
                    }
                    offset += length;
                }
                Item::Frame(frame, align, _) => {
                    ruler = ruler.max(align.y);

                    let x = align.x.position(size.x - frame.width());
                    let y = offset + ruler.position(free);
                    let pos = Point::new(x, y);
                    offset += frame.height();

                    // The baseline of the whole region will be the set to the
                    // baseline of the first in-flow frame. For example, of the
                    // first paragraph, if there is more than one. But also,
                    // inside the paragraph itself, this will be the first line
                    // (since each line is laid out as a separate frame).
                    if !baseline_set {
                        if frame.has_baseline() {
                            output.set_baseline(y + frame.baseline());
                        }
                        baseline_set = true;
                    }

                    output.push_frame(pos, frame);
                }
                Item::Placed(frame, placed) => {
                    let x = placed.align_x.position(size.x - frame.width());
                    let y = match placed.align_y.unwrap_or_default() {
                        Some(align) => align.position(size.y - frame.height()),
                        _ => offset + ruler.position(free),
                    };

                    let pos = Point::new(x, y)
                        + placed.delta.zip_map(size, Rel::relative_to).to_point();

                    output.push_frame(pos, frame);
                }
            }
        }

        Ok((output, used_height_without_fr))
    }

    /// Create a snapshot of the work and items.
    fn snapshot(&self) -> DistributionSnapshot<'a, 'b> {
        DistributionSnapshot {
            work: self.composer.work.clone(),
            items: self.items.len(),
            used: self.used,
            trace: self.composer.trace.checkpoint(),
            choices: self.composer.choices.checkpoint(),
        }
    }

    /// Restore a snapshot of the work and items.
    fn restore(&mut self, snapshot: DistributionSnapshot<'a, 'b>) {
        *self.composer.work = snapshot.work;
        self.items.truncate(snapshot.items);
        self.used = snapshot.used;
        self.composer.trace.restore(snapshot.trace);
        self.composer.choices.restore(snapshot.choices);
        self.restored_init = true;
    }

    /// Handle footnotes from an allocated fractional frame.
    fn fractional_footnotes(&mut self, child: ChildId, frame: &Frame) -> FlowResult<()> {
        if self.fractional_forbid == Some(child) {
            let result: Result<_, FootnoteStop<Infallible>> = self.composer.footnotes(
                &self.regions,
                frame,
                frame.height(),
                false,
                Migration::FORBID,
            );
            return match result {
                Ok(()) => {
                    self.fractional_forbid = None;
                    Ok(())
                }
                Err(FootnoteStop::Relayout(())) => {
                    Err(Stop::Relayout(PlacementScope::Column))
                }
                Err(FootnoteStop::MigrateOrigin(never)) => match never {},
                Err(FootnoteStop::Error(error)) => Err(Stop::Error(error)),
            };
        }

        match self.composer.footnotes(
            &self.regions,
            frame,
            frame.height(),
            false,
            Migration::ALLOW,
        ) {
            Ok(()) => Ok(()),
            Err(FootnoteStop::Relayout(())) => {
                Err(Stop::Relayout(PlacementScope::Column))
            }
            Err(FootnoteStop::MigrateOrigin(())) => Err(Stop::MigrateFractional(child)),
            Err(FootnoteStop::Error(error)) => Err(Stop::Error(error)),
        }
    }

    fn engine_commit(&mut self) {
        self.composer.engine.sink.commit_effect_transaction();
    }

    /// Retain a pending fractional migration across an insertion-driven
    /// column replay. Stable region completion intentionally drops it.
    fn preserve_fractional_migration(&mut self) {
        self.composer.fractional_break = self.fractional_break;
        self.composer.fractional_forbid = self.fractional_forbid;
    }

    fn engine_rollback(&mut self) {
        self.composer.engine.sink.rollback_effect_transaction();
    }

    fn handle_stop(&mut self, stop: Stop) -> RelayoutStop {
        match stop {
            Stop::Relayout(scope) => {
                self.preserve_fractional_migration();
                self.engine_rollback();
                RelayoutStop::Relayout(scope)
            }
            Stop::Error(error) => {
                self.engine_commit();
                RelayoutStop::Error(error)
            }
            Stop::Sticky(observation) => {
                self.engine_rollback();
                RelayoutStop::Sticky(observation)
            }
            Stop::MigrateFractional(child) => {
                self.composer.fractional_break = Some(child);
                self.engine_rollback();
                RelayoutStop::Relayout(PlacementScope::Column)
            }
            Stop::Finish(_) => {
                self.engine_rollback();
                unreachable!(
                    "flow distribution requested a late region finish without a replay target"
                );
            }
        }
    }

    fn finish_child_effects(&mut self, result: &FlowResult<()>) {
        if matches!(result, Ok(()) | Err(Stop::Error(_))) {
            self.engine_commit();
        } else {
            self.engine_rollback();
        }
    }
}
