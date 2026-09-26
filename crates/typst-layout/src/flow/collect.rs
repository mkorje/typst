use std::cell::LazyCell;
use std::fmt::Debug;
use std::sync::Arc;

use comemo::{Track, Tracked, TrackedMut};
use rustc_hash::FxHashMap;
use smallvec::SmallVec;
use typst_library::diag::{SourceResult, bail, warning};
use typst_library::engine::{Engine, Route, Sink, Traced};
use typst_library::foundations::{Packed, Resolve, Smart, Style, StyleChain, Styles};
use typst_library::introspection::{
    Introspector, Location, Locator, LocatorLink, SplitLocator, Tag, TagElem,
};
use typst_library::layout::{
    Abs, AlignElem, Alignment, Axes, BlockElem, ColbreakElem, FixedAlignment, FlushElem,
    Followup, Fr, Frame, FrameParent, Inherit, PagebreakElem, PlaceElem, PlacementScope,
    Ratio, Region, Regions, RegionsLink, Rel, Size, Sizing, Spacing, VElem,
};
use typst_library::model::ParElem;
use typst_library::routines::Pair;
use typst_library::text::TextElem;
use typst_library::{Library, World};
use typst_utils::{LazyHash, Protected, SliceExt};

use super::{
    BlockState, BlockStep, FlowCx, FlowMode, Restart, layout_multi_block,
    layout_single_block,
};
use crate::inline::ParSituation;
use crate::modifiers::{layout_and_modify, layout_with_modifiers};

/// Collects all elements of the flow into prepared children. These are much
/// simpler to handle than the raw elements.
///
/// The children don't borrow from the realized content: Their styles are
/// stored relative to the `base` style chain (see [`base_styles`]) and their
/// locators by their local hash. This allows them to outlive the realization
/// of the flow's content.
#[typst_macros::time]
pub fn collect<'a>(
    engine: &mut Engine,
    children: &[Pair<'a>],
    locator: Locator<'a>,
    base: StyleChain<'a>,
    size: Size,
    expand: bool,
    mode: FlowMode,
) -> SourceResult<Vec<Child>> {
    Collector {
        engine,
        base: size,
        expand,
        locator: locator.split(),
        outer: base.links().collect(),
        inner: FxHashMap::default(),
        output: Vec::with_capacity(children.len()),
        par_situation: ParSituation::First,
    }
    .run(children, mode)
}

/// The style chain that [`collect`] should store the styles of a flow's
/// children relative to, given the styles the flow was realized with.
///
/// If all children's styles extend the flow's `styles` (which holds for
/// content realized with them), it is `styles` itself. Otherwise, it is the
/// shared trunk of the children's styles, which lives as long as the realized
/// content.
pub fn base_styles<'a>(children: &[Pair<'a>], styles: StyleChain<'a>) -> StyleChain<'a> {
    let outer: Vec<_> = styles.links().collect();
    if children
        .iter()
        .filter(|(c, _)| !c.is::<TagElem>())
        .all(|&(_, chain)| inner_len(chain, &outer).is_some())
    {
        styles
    } else {
        StyleChain::trunk_from_pairs(children).unwrap_or(styles)
    }
}

/// The number of links that `chain` adds to the `outer` links, if they are a
/// suffix of its links (by identity).
fn inner_len(chain: StyleChain, outer: &[&[LazyHash<Style>]]) -> Option<usize> {
    let inner = chain.links().count().checked_sub(outer.len())?;
    chain
        .links()
        .skip(inner)
        .zip(outer)
        .all(|(a, b)| std::ptr::eq(a.as_ptr(), b.as_ptr()) && a.len() == b.len())
        .then_some(inner)
}

/// State for collection.
struct Collector<'a, 'x, 'y> {
    engine: &'x mut Engine<'y>,
    base: Size,
    expand: bool,
    locator: SplitLocator<'a>,
    /// The links of the flow's style chain, from innermost to outermost.
    outer: Vec<&'a [LazyHash<Style>]>,
    /// Deduplicates the inner styles of children, keyed by their links.
    inner: FxHashMap<Vec<(usize, usize)>, InnerStyles>,
    output: Vec<Child>,
    par_situation: ParSituation,
}

impl<'a> Collector<'a, '_, '_> {
    /// Perform the collection.
    fn run(mut self, children: &[Pair<'a>], mode: FlowMode) -> SourceResult<Vec<Child>> {
        // Extract leading and trailing tags.
        let (start, end) = children.split_prefix_suffix(|(c, _)| c.is::<TagElem>());
        let inner = &children[start..end];

        for (c, _) in &children[..start] {
            let elem = c.to_packed::<TagElem>().unwrap();
            self.output.push(Child::Tag(elem.tag.clone()));
        }

        match mode {
            FlowMode::Root | FlowMode::Block => self.run_block(inner)?,
            FlowMode::Inline => self.run_inline(inner)?,
        }

        for (c, _) in &children[end..] {
            let elem = c.to_packed::<TagElem>().unwrap();
            self.output.push(Child::Tag(elem.tag.clone()));
        }

        Ok(self.output)
    }

    /// Perform collection for block-level children.
    fn run_block(&mut self, children: &[Pair<'a>]) -> SourceResult<()> {
        for &(child, styles) in children {
            if let Some(elem) = child.to_packed::<TagElem>() {
                self.output.push(Child::Tag(elem.tag.clone()));
            } else if let Some(elem) = child.to_packed::<VElem>() {
                self.v(elem, styles);
            } else if let Some(elem) = child.to_packed::<ParElem>() {
                self.par(elem, styles)?;
            } else if let Some(elem) = child.to_packed::<BlockElem>() {
                let alone = children.len() == 1;
                self.block(elem, styles, alone);
            } else if let Some(elem) = child.to_packed::<PlaceElem>() {
                self.place(elem, styles)?;
            } else if child.is::<FlushElem>() {
                self.output.push(Child::Flush);
            } else if let Some(elem) = child.to_packed::<ColbreakElem>() {
                self.output.push(Child::Break(elem.weak.get(styles)));
                self.par_situation = ParSituation::First;
            } else if child.is::<PagebreakElem>() {
                bail!(
                    child.span(), "pagebreaks are not allowed inside of containers";
                    hint: "try using a `#colbreak()` instead";
                );
            } else {
                self.engine.sink.warn(warning!(
                    child.span(),
                    "{} was ignored during paged export",
                    child.func().name(),
                ));
            }
        }
        Ok(())
    }

    /// Perform collection for inline-level children.
    fn run_inline(&mut self, children: &[Pair<'a>]) -> SourceResult<()> {
        // Compute the shared styles.
        let styles = StyleChain::trunk_from_pairs(children).unwrap_or_default();

        // Layout the lines.
        let lines = crate::inline::layout_inline(
            self.engine,
            children,
            &mut self.locator,
            styles,
            self.base,
            self.expand,
        )?
        .into_frames();

        let leading = styles.resolve(ParElem::leading);
        self.lines(lines, leading, styles);

        Ok(())
    }

    /// Collect vertical spacing into a relative or fractional child.
    fn v(&mut self, elem: &'a Packed<VElem>, styles: StyleChain<'a>) {
        self.output.push(match elem.amount {
            Spacing::Rel(rel) => {
                Child::Rel(rel.resolve(styles), elem.weak.get(styles) as u8)
            }
            Spacing::Fr(fr) => Child::Fr(fr, elem.weak.get(styles) as u8),
        });
    }

    /// Collect a paragraph into [`LineChild`]ren. This already performs line
    /// layout since it is not dependent on the concrete regions.
    fn par(
        &mut self,
        elem: &'a Packed<ParElem>,
        styles: StyleChain<'a>,
    ) -> SourceResult<()> {
        let lines = crate::inline::layout_par(
            elem,
            self.engine,
            self.locator.next(&elem.span()),
            styles,
            self.base,
            self.expand,
            self.par_situation,
        )?
        .into_frames();

        let spacing = elem.spacing.resolve(styles);
        let leading = elem.leading.resolve(styles);

        self.output.push(Child::Rel(spacing.into(), 4));

        self.lines(lines, leading, styles);

        self.output.push(Child::Rel(spacing.into(), 4));
        self.par_situation = ParSituation::Consecutive;

        Ok(())
    }

    /// Collect laid-out lines.
    fn lines(&mut self, lines: Vec<Frame>, leading: Abs, styles: StyleChain<'a>) {
        let align = styles.resolve(AlignElem::alignment);
        let costs = styles.get(TextElem::costs);

        // Determine whether to prevent widow and orphans.
        let len = lines.len();
        let prevent_orphans =
            costs.orphan() > Ratio::zero() && len >= 2 && !lines[1].is_empty();
        let prevent_widows =
            costs.widow() > Ratio::zero() && len >= 2 && !lines[len - 2].is_empty();
        let prevent_all = len == 3 && prevent_orphans && prevent_widows;

        // Store the heights of lines at the edges because we'll potentially
        // need these later when `lines` is already moved.
        let height_at = |i| lines.get(i).map(Frame::height).unwrap_or_default();
        let front_1 = height_at(0);
        let front_2 = height_at(1);
        let back_2 = height_at(len.saturating_sub(2));
        let back_1 = height_at(len.saturating_sub(1));

        for (i, frame) in lines.into_iter().enumerate() {
            if i > 0 {
                self.output.push(Child::Rel(leading.into(), 5));
            }

            // To prevent widows and orphans, we require enough space for
            // - all lines if it's just three
            // - the first two lines if we're at the first line
            // - the last two lines if we're at the second to last line
            let need = if prevent_all && i == 0 {
                front_1 + leading + front_2 + leading + back_1
            } else if prevent_orphans && i == 0 {
                front_1 + leading + front_2
            } else if prevent_widows && i >= 2 && i + 2 == len {
                back_2 + leading + back_1
            } else {
                frame.height()
            };

            self.output.push(Child::Line(LineChild { frame, align, need }));
        }
    }

    /// Collect a block into a [`SingleChild`] or [`MultiChild`] depending on
    /// whether it is breakable.
    fn block(
        &mut self,
        elem: &'a Packed<BlockElem>,
        styles: StyleChain<'a>,
        alone: bool,
    ) {
        let locator = self.locator.next(&elem.span()).local();
        let align = styles.resolve(AlignElem::alignment);
        let sticky = elem.sticky.get(styles);
        let breakable = elem.breakable.get(styles);
        let fr = match elem.height.get(styles) {
            Sizing::Fr(fr) => Some(fr),
            _ => None,
        };

        let fallback = LazyCell::new(|| styles.resolve(ParElem::spacing));
        let spacing = |amount| match amount {
            Smart::Auto => Child::Rel((*fallback).into(), 4),
            Smart::Custom(Spacing::Rel(rel)) => Child::Rel(rel.resolve(styles), 3),
            Smart::Custom(Spacing::Fr(fr)) => Child::Fr(fr, 2),
        };

        let above = spacing(elem.above.get(styles));
        let below = spacing(elem.below.get(styles));
        self.output.push(above);

        let elem = elem.clone();
        let inner = self.relative(styles);
        if !breakable || fr.is_some() {
            self.output.push(Child::Single(Box::new(SingleChild {
                align,
                sticky,
                alone,
                fr,
                elem,
                styles: inner,
                locator,
            })));
        } else {
            self.output.push(Child::Multi(Box::new(MultiChild {
                align,
                sticky,
                alone,
                elem,
                styles: inner,
                locator,
            })));
        }

        self.output.push(below);
        self.par_situation = ParSituation::Other;
    }

    /// Collects a placed element into a [`PlacedChild`].
    fn place(
        &mut self,
        elem: &'a Packed<PlaceElem>,
        styles: StyleChain<'a>,
    ) -> SourceResult<()> {
        let alignment = elem.alignment.get(styles);
        let align_x = alignment.map_or(FixedAlignment::Center, |align| {
            align.x().unwrap_or_default().resolve(styles)
        });
        let align_y = alignment.map(|align| align.y().map(|y| y.resolve(styles)));
        let scope = elem.scope.get(styles);
        let float = elem.float.get(styles);

        match (float, align_y) {
            (true, Smart::Custom(None | Some(FixedAlignment::Center))) => bail!(
                elem.span(),
                "vertical floating placement must be `auto`, `top`, or `bottom`"
            ),
            (false, Smart::Auto) => bail!(
                elem.span(),
                "automatic positioning is only available for floating placement";
                hint: "you can enable floating placement with `place(float: true, ..)`";
            ),
            _ => {}
        }

        if !float && scope == PlacementScope::Parent {
            bail!(
                elem.span(),
                "parent-scoped positioning is currently only available for floating placement";
                hint: "you can enable floating placement with `place(float: true, ..)`";
            );
        }

        let locator = self.locator.next(&elem.span()).local();
        let clearance = elem.clearance.resolve(styles);
        let delta = Axes::new(elem.dx.get(styles), elem.dy.get(styles)).resolve(styles);
        let inner = self.relative(styles);
        self.output.push(Child::Placed(Box::new(PlacedChild {
            align_x,
            align_y,
            scope,
            float,
            clearance,
            delta,
            elem: elem.clone(),
            styles: inner,
            locator,
            alignment,
        })));

        Ok(())
    }

    /// Determines the styles of a child relative to the flow's styles.
    fn relative(&mut self, styles: StyleChain<'a>) -> InnerStyles {
        // Styles that don't extend the flow's styles are stored in full. This
        // doesn't occur for content realized with the flow's styles.
        let Some(len) = inner_len(styles, &self.outer) else {
            return InnerStyles::new(styles, usize::MAX, true);
        };
        if len == 0 {
            return InnerStyles::default();
        }

        let key: SmallVec<[_; 8]> = styles
            .links()
            .take(len)
            .map(|l| (l.as_ptr() as usize, l.len()))
            .collect();
        if let Some(inner) = self.inner.get(key.as_slice()) {
            return inner.clone();
        }

        let inner = InnerStyles::new(styles, len, false);
        self.inner.insert(key.into_vec(), inner.clone());
        inner
    }
}

/// The styles of a child, stored relative to the styles of its flow.
///
/// Holds the style links that were added within the flow's content, from
/// outermost to innermost. When the child is laid out, they are grafted onto
/// the flow's styles. This way, a child doesn't borrow from the styles its flow
/// was realized with and cloning is limited to the (typically few) links added
/// within the flow.
#[derive(Debug, Clone, Default)]
pub struct InnerStyles {
    links: Arc<[Styles]>,
    /// Whether the links are the complete chain rather than relative to the
    /// flow's styles, because the child's styles don't extend them.
    absolute: bool,
}

impl InnerStyles {
    /// Stores the first `len` links of `styles`.
    fn new(styles: StyleChain, len: usize, absolute: bool) -> Self {
        let mut links: Vec<_> = styles.links().take(len).map(Styles::from).collect();
        links.reverse();
        Self { links: links.into(), absolute }
    }

    /// Runs `f` with the full styles, given the styles of the flow.
    pub fn with<R>(&self, outer: StyleChain, f: impl FnOnce(StyleChain) -> R) -> R {
        fn graft<R>(
            chain: StyleChain,
            links: &[Styles],
            f: impl FnOnce(StyleChain) -> R,
        ) -> R {
            match links.split_first() {
                None => f(chain),
                Some((link, rest)) => graft(chain.chain(link), rest, f),
            }
        }

        let base = if self.absolute { StyleChain::default() } else { outer };
        graft(base, &self.links, f)
    }
}

/// A prepared child in flow layout.
///
/// The larger variants are boxed to keep the enum size down.
#[derive(Debug)]
pub enum Child {
    /// An introspection tag.
    Tag(Tag),
    /// Relative spacing with a specific weakness level.
    Rel(Rel<Abs>, u8),
    /// Fractional spacing with a specific weakness level.
    Fr(Fr, u8),
    /// An already layouted line of a paragraph.
    Line(LineChild),
    /// An unbreakable block.
    Single(Box<SingleChild>),
    /// A breakable block.
    Multi(Box<MultiChild>),
    /// An absolutely or floatingly placed element.
    Placed(Box<PlacedChild>),
    /// A place flush.
    Flush,
    /// An explicit column break.
    Break(bool),
}

/// A child that encapsulates a layouted line of a paragraph.
#[derive(Debug)]
pub struct LineChild {
    pub frame: Frame,
    pub align: Axes<FixedAlignment>,
    pub need: Abs,
}

/// A child that encapsulates a prepared unbreakable block.
#[derive(Debug)]
pub struct SingleChild {
    pub align: Axes<FixedAlignment>,
    pub sticky: bool,
    pub alone: bool,
    pub fr: Option<Fr>,
    elem: Packed<BlockElem>,
    styles: InnerStyles,
    locator: u128,
}

impl SingleChild {
    /// Build the child's frame given the region's base size.
    pub fn layout(
        &self,
        engine: &mut Engine,
        cx: &FlowCx,
        region: Region,
    ) -> SourceResult<Frame> {
        let mut region = region;
        // Vertical expansion is only kept if this block is the only child.
        region.expand.y &= self.alone;
        self.styles.with(cx.styles, |styles| {
            layout_single_impl(
                engine.world,
                engine.library,
                engine.introspector.into_raw(),
                engine.traced,
                TrackedMut::reborrow_mut(&mut engine.sink),
                engine.route.track(),
                &self.elem,
                cx.locator.with_local(self.locator).track(),
                styles,
                region,
            )
        })
    }
}

/// The cached, internal implementation of [`SingleChild::layout`].
#[comemo::memoize]
#[expect(clippy::too_many_arguments)]
fn layout_single_impl(
    world: Tracked<dyn World + '_>,
    library: &LazyHash<Library>,
    introspector: Tracked<dyn Introspector + '_>,
    traced: Tracked<Traced>,
    sink: TrackedMut<Sink>,
    route: Tracked<Route>,
    elem: &Packed<BlockElem>,
    locator: Tracked<Locator>,
    styles: StyleChain,
    region: Region,
) -> SourceResult<Frame> {
    let introspector = Protected::from_raw(introspector);
    let link = LocatorLink::new(locator);
    let locator = Locator::link(&link);
    let mut engine = Engine {
        library,
        world,
        introspector,
        traced,
        sink,
        route: Route::extend(route),
    };

    layout_and_modify(styles, |styles| {
        layout_single_block(elem, &mut engine, locator, styles, region)
    })
}

/// A child that encapsulates a prepared breakable block.
#[derive(Debug)]
pub struct MultiChild {
    pub align: Axes<FixedAlignment>,
    pub sticky: bool,
    alone: bool,
    elem: Packed<BlockElem>,
    styles: InnerStyles,
    locator: u128,
}

impl MultiChild {
    /// Build the child's frames given regions.
    ///
    /// The `index` is the child's index in the flow and the `subregion` is the
    /// index of the flow subregion into which the first frame will be placed.
    pub fn layout(
        &self,
        engine: &mut Engine,
        cx: &FlowCx,
        regions: Regions,
        index: usize,
        subregion: usize,
    ) -> SourceResult<(Frame, Option<MultiSpill>, bool)> {
        let (step, sink) = engine.isolate(|engine| self.step(engine, cx, regions, None));
        let step = step?;
        let spill = match step.next {
            Some(state) => Some(MultiSpill {
                index,
                state,
                steps: vec![Emitted {
                    state: None,
                    regions: RegionsDesc::new(regions),
                    frame: step.frame.clone(),
                    ahead: step.ahead,
                }],
                pending: sink,
                origin: subregion,
                count: 1,
                aligned: true,
            }),
            None => {
                engine.commit(sink);
                None
            }
        };
        Ok((step.frame, spill, step.exist_non_empty_frame))
    }

    /// The regions the block is laid out into, given the regions offered by
    /// the flow.
    fn pod<'r>(&self, mut regions: Regions<'r>) -> Regions<'r> {
        // Vertical expansion is only kept if this block is the only child.
        regions.expand.y &= self.alone;
        regions
    }

    /// Lays out one region of the block, continuing from `state`. See
    /// [`layout_multi_block`].
    fn step(
        &self,
        engine: &mut Engine,
        cx: &FlowCx,
        regions: Regions,
        state: Option<&BlockState>,
    ) -> SourceResult<BlockStep> {
        let regions = self.pod(regions);
        let regions = regions.track();
        self.styles.with(cx.styles, |styles| {
            layout_multi_step_impl(
                engine.world,
                engine.library,
                engine.introspector.into_raw(),
                engine.traced,
                TrackedMut::reborrow_mut(&mut engine.sink),
                engine.route.track(),
                &self.elem,
                cx.locator.with_local(self.locator).track(),
                styles,
                regions,
                state,
            )
        })
    }
}

/// The cached implementation of [`MultiChild::step`].
///
/// Continuations are cached by the identity of their state (see
/// [`MultiState`](typst_library::layout::MultiState)).
#[comemo::memoize]
#[expect(clippy::too_many_arguments)]
fn layout_multi_step_impl(
    world: Tracked<dyn World + '_>,
    library: &LazyHash<Library>,
    introspector: Tracked<dyn Introspector + '_>,
    traced: Tracked<Traced>,
    sink: TrackedMut<Sink>,
    route: Tracked<Route>,
    elem: &Packed<BlockElem>,
    locator: Tracked<Locator>,
    styles: StyleChain,
    regions: Tracked<Regions>,
    state: Option<&BlockState>,
) -> SourceResult<BlockStep> {
    let introspector = Protected::from_raw(introspector);
    let link = LocatorLink::new(locator);
    let locator = Locator::link(&link);
    let regions_link = RegionsLink::new(regions);
    let regions = Regions::link(&regions_link);
    let mut engine = Engine {
        library,
        world,
        introspector,
        traced,
        sink,
        route: Route::extend(route),
    };

    layout_with_modifiers(styles, |styles, modifiers| {
        layout_multi_block(elem, &mut engine, locator, styles, modifiers, regions, state)
    })
}

/// The spilled remains of a `MultiChild` that broke across two regions.
///
/// The child is laid out one region at a time. The frame emitted for a region
/// may depend on predictions of the upcoming regions. When the next region is
/// laid out and it (or the ones after it) differ from the prediction, the
/// previous region is laid out again with the actual regions to verify that
/// the emitted frame doesn't change. If it does, the child continues from
/// the state it was laid out with rather than from the one of the new layout,
/// since gluing a changed frame together with the next one could lose or
/// duplicate content. As the frame's decisions were based on mispredicted
/// regions, the spill first requests a [`Restart`] of flow layout if
/// possible. See DESIGN.md §53 for why this is sound.
///
/// A step may also have laid out content into the upcoming regions already,
/// with their predicted heights, and declare how much of them it uses up
/// ([`BlockStep::ahead`]). Verification doesn't cover such content if an
/// earlier step laid it out, or if the verification fails. So when the spill
/// reaches a region that doesn't fit the content laid out into it, it lays
/// out the steps since the earliest one that laid out content into the region
/// again, each with the regions it was laid out with, up to this region, and
/// the actual regions from there on. If the emitted frames stay the same, the
/// child continues from the new state. Otherwise, it restarts at the region
/// of the earliest frame, or continues from the old state if it can't.
///
/// The side effects of laying out the last emitted frame are held back until
/// it is verified, since the layout that verifies it may replace the state it
/// continues from. Then, only the side effects of the layout whose state is
/// kept are recorded, including those of any work it did ahead.
#[derive(Clone)]
pub struct MultiSpill {
    /// The index of the breakable child in the flow.
    pub(super) index: usize,
    /// Where the block continues.
    state: BlockState,
    /// The emitted steps that may have to be laid out again: the last one and
    /// those whose content ahead reaches the next region, in order.
    steps: Vec<Emitted>,
    /// The side effects of laying out the last emitted frame, which are
    /// recorded once it is verified.
    pending: Sink,
    /// The flow subregion into which the first frame was placed.
    origin: usize,
    /// The number of emitted frames.
    count: usize,
    /// Whether the emitted frames were placed into consecutive subregions,
    /// starting at `origin`. Only then do the child's regions line up with the
    /// flow's subregions, which is required for restarting.
    aligned: bool,
}

impl MultiSpill {
    /// Build the spill's next frame given regions, returning it and, if there
    /// is more, the remaining spill. If the frames that were already emitted
    /// are inconsistent with the actual size of this region, requests a
    /// restart of flow layout instead.
    ///
    /// The `target` describes the flow subregion into which the frame will be
    /// placed.
    pub fn layout(
        self,
        multi: &MultiChild,
        engine: &mut Engine,
        cx: &FlowCx,
        regions: Regions,
        target: SpillTarget,
    ) -> SourceResult<Result<(Frame, Option<MultiSpill>), Restart>> {
        Ok(self
            .advance(multi, engine, cx, regions, target)?
            .map(|(step, spill)| (step.frame, spill)))
    }

    /// Skips a subregion that is already full.
    ///
    /// To keep the child's regions lined up with the flow's subregions, the
    /// child is laid out into the full subregion, too. If the child continues
    /// and nothing of it was placed into the subregion, the frame is dropped.
    /// It then holds at most the child's decoration, which is not missed
    /// since the child continues with a decorated frame in the next
    /// subregion. Otherwise, the child is deferred to the next subregion as
    /// if the full one didn't exist. Returns the remaining spill or, like
    /// [`layout`](Self::layout), a request to restart.
    pub fn skip(
        mut self,
        multi: &MultiChild,
        engine: &mut Engine,
        cx: &FlowCx,
        mut regions: Regions,
        target: SpillTarget,
    ) -> SourceResult<Result<Option<MultiSpill>, Restart>> {
        if self.aligned {
            regions.at_least(Abs::zero());
            // The side effects of the layout only count if its result is kept.
            let (trial, sink) = engine.isolate(|engine| {
                self.clone().advance(multi, engine, cx, regions, target)
            });
            match trial? {
                Ok((step, Some(spill)))
                    if step.decoration_only && step.frame.height().approx_empty() =>
                {
                    engine.commit(sink);
                    return Ok(Ok(Some(spill)));
                }
                Ok(_) => {}
                Err(restart) => return Ok(Err(restart)),
            }
        }
        self.skipped();
        Ok(Ok(Some(self)))
    }

    /// Lays out the spill's next frame, returning the step and the remaining
    /// spill or a request to restart.
    fn advance(
        mut self,
        multi: &MultiChild,
        engine: &mut Engine,
        cx: &FlowCx,
        regions: Regions,
        target: SpillTarget,
    ) -> SourceResult<Result<(BlockStep, Option<MultiSpill>), Restart>> {
        // The next frame is always laid out with the actual regions.
        let used = RegionsDesc::new(regions);

        // Verify that the last frame doesn't change when laid out with the
        // regions it would have been laid out with if the upcoming regions
        // had been predicted correctly. Since steps are memoized, this is a
        // cache hit that returns the very same frame if the layout of the last
        // frame gets the same answers to its questions about the regions from
        // the actual ones. The regions are recreated from their description,
        // while those of the first frame may have been derived from the
        // flow's regions, so the frames may differ by floating-point error.
        let last = self.steps.last().unwrap();
        let verify = last.regions.followed_by(1, &used);
        if verify == last.regions {
            // If the upcoming regions were predicted correctly, the frame
            // would be laid out with the same regions again. That's a cache
            // hit, except for the first frame, whose regions may have been
            // derived from the flow's regions. Then, the frame could only
            // differ by floating-point error, which verification accepts.
            engine.commit(std::mem::take(&mut self.pending));
        } else {
            // Only the side effects of the layout whose state is kept are
            // recorded. Recording both would record the side effects of the
            // last frame again for each region, and for each level of nested
            // breakable blocks.
            let (verification, sink) = engine.isolate(|engine| {
                multi.step(engine, cx, verify.regions(), last.state.as_ref())
            });
            let verification = verification?;
            match verification.next {
                Some(next) if verification.frame.approx_identical(&last.frame) => {
                    self.state = next;
                    let last = self.steps.last_mut().unwrap();
                    last.regions = verify;
                    last.ahead = verification.ahead;
                    engine.commit(sink);
                }
                _ => {
                    // If the space in this subregion was mispredicted, restart
                    // with a better prediction. The subregion has more space
                    // than predicted if the prediction was learned from an
                    // earlier restart and insertions have moved since then.
                    // Raising the prediction requires that a restart is left to
                    // lower it again: If the layout alternates between two
                    // predictions, the last restart thus lowers it.
                    if self.aligned
                        && let Some(available) = target.available
                    {
                        let needed = match last.regions.prediction() {
                            Some(p) if !available.fits(p) => 1,
                            Some(p) if !p.fits(available) => 2,
                            _ => usize::MAX,
                        };
                        if target.restarts >= needed {
                            return Ok(Err(Restart {
                                from: self.origin + self.count - 1,
                                at: target.subregion,
                                height: available,
                            }));
                        }
                    }

                    // Otherwise, continue from the state the last frame was
                    // laid out with, which is consistent with it. Its decisions
                    // were based on the mispredicted regions, but continuing it
                    // with any regions neither loses nor duplicates content.
                    // And with the actual regions, the next frame fits and its
                    // lookahead uses every prediction learned so far.
                    engine.commit(std::mem::take(&mut self.pending));
                }
            }
        }

        // If the state already laid out content into this region that doesn't
        // fit, lay out the steps since the one that laid it out again. If that
        // changes an emitted frame, restart at its region, now knowing this
        // region's space. If that's not possible either, continue from the
        // state as is, whose content then overflows this region.
        if let Some(&needed) = self.steps.last().unwrap().ahead.first()
            && !regions.fits(needed)
            && let Err(start) = self.redo(multi, engine, cx, &used)?
            && self.aligned
            && let Some(available) = target.available
            && target.restarts >= 1
        {
            return Ok(Err(Restart {
                from: self.origin + start,
                at: target.subregion,
                height: available,
            }));
        }

        let (step, sink) = engine
            .isolate(|engine| multi.step(engine, cx, used.regions(), Some(&self.state)));
        let mut step = step?;
        let Some(state) = step.next.take() else {
            engine.commit(sink);
            return Ok(Ok((step, None)));
        };

        // Keep the steps that may have to be laid out again for the next
        // region: the new one and those whose content ahead reaches it.
        self.steps.push(Emitted {
            state: Some(std::mem::replace(&mut self.state, state)),
            regions: used,
            frame: step.frame.clone(),
            ahead: step.ahead.clone(),
        });
        self.count += 1;
        self.steps.drain(..self.reaching(self.count));
        self.pending = sink;
        Ok(Ok((step, Some(self))))
    }

    /// Lays out the steps since the earliest one that laid out content into
    /// the region the next frame goes into again, each with the regions it
    /// was laid out with up to that region, and the actual ones described by
    /// `used` from there on.
    ///
    /// The regions in between are the actual ones if the step was verified.
    /// Otherwise, they are the predictions that it was laid out with, which
    /// reproduce its frame, while the actual ones wouldn't.
    ///
    /// If the emitted frames don't change, the spill continues from the new
    /// state. Otherwise, returns the index of the earliest step's frame.
    fn redo(
        &mut self,
        multi: &MultiChild,
        engine: &mut Engine,
        cx: &FlowCx,
        used: &RegionsDesc,
    ) -> SourceResult<Result<(), usize>> {
        // The index of the next frame and of the frame of the first step.
        let next = self.count;
        let first = self.count - self.steps.len();
        let start = self.reaching(next);

        let mut state = self.steps[start].state.clone();
        let mut redone = Vec::with_capacity(self.steps.len() - start);
        for (i, emitted) in self.steps.iter().enumerate().skip(start) {
            let regions = emitted.regions.followed_by(next - (first + i), used);
            let step = multi.step(engine, cx, regions.regions(), state.as_ref())?;
            // The step's regions are recreated from their description now,
            // while they may have been derived from the flow's regions before,
            // so the frames may differ by floating-point error.
            let (Some(next_state), true) =
                (step.next, step.frame.approx_identical(&emitted.frame))
            else {
                return Ok(Err(first + start));
            };
            redone.push(Emitted {
                state,
                regions,
                frame: emitted.frame.clone(),
                ahead: step.ahead,
            });
            state = Some(next_state);
        }

        self.steps.splice(start.., redone);
        self.state = state.unwrap();
        Ok(Ok(()))
    }

    /// The position in `steps` of the earliest step whose content ahead
    /// reaches the region of the frame with the given index, or of the last
    /// step if none does.
    fn reaching(&self, index: usize) -> usize {
        let first = self.count - self.steps.len();
        self.steps
            .iter()
            .enumerate()
            .position(|(i, emitted)| first + i + emitted.ahead.len() >= index)
            .unwrap_or(self.steps.len() - 1)
    }

    /// Notes that a subregion was skipped without placing a frame of the
    /// spill into it. Then, the child's regions no longer line up with the
    /// flow's subregions, so restarts are disabled for this spill.
    pub fn skipped(&mut self) {
        self.aligned = false;
    }
}

/// Where the next frame of a [`MultiSpill`] is placed.
#[derive(Copy, Clone)]
pub struct SpillTarget {
    /// The index of the flow subregion into which the frame will be placed.
    pub subregion: usize,
    /// How many more restarts may be requested because the space in the
    /// subregion was mispredicted.
    pub restarts: usize,
    /// The height available in the subregion, if a restart may be requested.
    /// Unlike the height of the regions the spill is laid out into, this isn't
    /// limited by column balancing, since it serves as a prediction for the
    /// subregion when restarting. It isn't read otherwise, since that's a
    /// blunt question about the regions.
    pub available: Option<Abs>,
}

/// An emitted step of a [`MultiSpill`].
#[derive(Clone)]
struct Emitted {
    /// The state the step was laid out from. `None` for the first frame.
    state: Option<BlockState>,
    /// The regions the step was laid out with.
    regions: RegionsDesc,
    /// The emitted frame.
    frame: Frame,
    /// How much height the step's state laid out into the upcoming regions.
    ahead: Vec<Abs>,
}

/// An owned description of [`Regions`].
#[derive(Debug, Clone, PartialEq)]
struct RegionsDesc {
    size: Size,
    expand: Axes<bool>,
    full: Abs,
    followup: Followup,
}

impl RegionsDesc {
    /// Describe the given regions.
    fn new(regions: Regions) -> Self {
        Self {
            size: regions.size(),
            expand: regions.expand,
            full: regions.full(),
            followup: regions.followup(),
        }
    }

    /// Recreate the regions.
    fn regions(&self) -> Regions<'_> {
        Regions::new(self.size, self.full, &[], None, self.expand)
            .with_followup(&self.followup)
    }

    /// The same regions up to the one `at` breaks after the first one,
    /// followed by the given regions instead of the ones from there on.
    ///
    /// The given regions take on the kind of the region they replace: If it
    /// is a repetition of the final region, they are repetitions, too, since
    /// that affects whether moving on to them counts as progress.
    fn followed_by(&self, at: usize, regions: &RegionsDesc) -> Self {
        let Followup { backlog, predicted, last } = &self.followup;
        let kept = at - 1;
        let mut followup = regions.followup.clone().prepend([regions.size.y]);
        match last {
            Some(last) if kept >= backlog.len() => {
                let repeated = predicted
                    .iter()
                    .copied()
                    .chain(std::iter::repeat(*last))
                    .take(kept - backlog.len());
                followup.backlog.append(&mut followup.predicted);
                followup.predicted = repeated.chain(followup.backlog.drain(..)).collect();
                followup.backlog = backlog.clone();
            }
            _ => followup = followup.prepend(backlog.iter().take(kept).copied()),
        }
        Self { followup, ..self.clone() }
    }

    /// The predicted remaining height of the next region.
    fn prediction(&self) -> Option<Abs> {
        let Followup { backlog, predicted, last } = &self.followup;
        backlog.first().or(predicted.first()).copied().or(*last)
    }
}

/// A child that encapsulates a prepared placed element.
#[derive(Debug)]
pub struct PlacedChild {
    pub align_x: FixedAlignment,
    pub align_y: Smart<Option<FixedAlignment>>,
    pub scope: PlacementScope,
    pub float: bool,
    pub clearance: Abs,
    pub delta: Axes<Rel<Abs>>,
    elem: Packed<PlaceElem>,
    styles: InnerStyles,
    locator: u128,
    alignment: Smart<Alignment>,
}

impl PlacedChild {
    /// Build the child's frame given the region's base size.
    pub fn layout(
        &self,
        engine: &mut Engine,
        cx: &FlowCx,
        base: Size,
    ) -> SourceResult<Frame> {
        let align = self.alignment.unwrap_or_else(|| Alignment::CENTER);
        let aligned = AlignElem::alignment.set(align).wrap();

        let mut frame = self.styles.with(cx.styles, |styles| {
            let styles = styles.chain(&aligned);
            layout_and_modify(styles, |styles| {
                crate::layout_frame(
                    engine,
                    &self.elem.body,
                    cx.locator.with_local(self.locator),
                    styles,
                    Region::new(base, Axes::splat(false)),
                )
            })
        })?;

        if self.float {
            frame.set_parent(FrameParent::new(
                self.elem.location().unwrap(),
                Inherit::Yes,
            ));
        }

        Ok(frame)
    }

    /// The element's location.
    pub fn location(&self) -> Location {
        self.elem.location().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pt(v: f64) -> Abs {
        Abs::pt(v)
    }

    fn desc(
        height: f64,
        backlog: &[f64],
        predicted: &[f64],
        last: Option<f64>,
    ) -> RegionsDesc {
        RegionsDesc {
            size: Size::new(pt(100.0), pt(height)),
            expand: Axes::splat(true),
            full: pt(100.0),
            followup: Followup {
                backlog: backlog.iter().copied().map(pt).collect(),
                predicted: predicted.iter().copied().map(pt).collect(),
                last: last.map(pt),
            },
        }
    }

    #[test]
    fn test_regions_desc_followed_by() {
        let actual = desc(60.0, &[70.0], &[], Some(90.0));

        // Replacing repetitions of the final region: The given regions are
        // repetitions, too, and the kept ones stay repetitions.
        let repeated = desc(80.0, &[], &[95.0], Some(100.0));
        assert_eq!(
            repeated.followed_by(1, &actual),
            desc(80.0, &[], &[60.0, 70.0], Some(90.0)),
        );
        assert_eq!(
            repeated.followed_by(3, &actual),
            desc(80.0, &[], &[95.0, 100.0, 60.0, 70.0], Some(90.0)),
        );

        // Replacing backlog regions: The kept ones stay in the backlog.
        let backlog = desc(80.0, &[85.0, 75.0], &[], Some(100.0));
        assert_eq!(
            backlog.followed_by(1, &actual),
            desc(80.0, &[60.0, 70.0], &[], Some(90.0)),
        );
        assert_eq!(
            backlog.followed_by(2, &actual),
            desc(80.0, &[85.0, 60.0, 70.0], &[], Some(90.0)),
        );

        // Replacing the repetition after a backlog.
        assert_eq!(
            backlog.followed_by(3, &actual),
            desc(80.0, &[85.0, 75.0], &[60.0, 70.0], Some(90.0)),
        );
    }
}
