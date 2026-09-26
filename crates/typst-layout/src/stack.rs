use std::sync::Arc;

use either::Either;

use typst_library::diag::{SourceResult, bail};
use typst_library::engine::Engine;
use typst_library::foundations::{Content, Packed, Resolve, StyleChain, StyledElem};
use typst_library::introspection::Locator;
use typst_library::layout::{
    Abs, AlignElem, Axes, Axis, Dir, FixedAlignment, Fr, Frame, HElem, MultiState,
    MultiStep, Point, Regions, Spacing, StackChild, StackElem, VElem,
};
use typst_syntax::Span;
use typst_utils::{Get, Numeric};

/// Layout the stack, one region at a time.
#[typst_macros::time(span = elem.span())]
pub fn layout_stack(
    elem: &Packed<StackElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    regions: Regions,
    state: Option<&MultiState>,
) -> SourceResult<MultiStep> {
    layout_stack_internal::<
        fn(
            &mut Engine,
            StyleChain,
            Regions,
            Option<&MultiState>,
        ) -> SourceResult<MultiStep>,
        _,
    >(
        |start| elem.children[start..].iter().map(From::from),
        elem.span(),
        elem.spacing.get(styles),
        elem.dir.get(styles),
        engine,
        locator,
        styles,
        regions,
        state,
    )
}

/// Similar to a [`StackChild`], but with an additional variant that allows
/// specifying a custom layouter for a child. Useful when using stack layout to
/// create other layouters, such as that of lists.
pub enum StackLayoutChild<'a, F>
where
    F: Fn(
        &mut Engine,
        StyleChain,
        Regions,
        Option<&MultiState>,
    ) -> SourceResult<MultiStep>,
{
    /// A stack child with content or spacing.
    StackChild(&'a StackChild),
    /// A child with a custom layouter, producing its own frames one region at
    /// a time.
    CustomLayouter(F),
}

impl<'a, F> From<&'a StackChild> for StackLayoutChild<'a, F>
where
    F: Fn(
        &mut Engine,
        StyleChain,
        Regions,
        Option<&MultiState>,
    ) -> SourceResult<MultiStep>,
{
    fn from(value: &'a StackChild) -> Self {
        Self::StackChild(value)
    }
}

/// Where a stack continues.
struct StackState {
    /// The index of the child to continue with.
    child: usize,
    /// How to continue with the child.
    resume: Resume,
    /// The local hashes of the children's locators (see [`Locator::local`]).
    locals: Arc<[u128]>,
}

/// How a stack continues with a child.
enum Resume {
    /// The child is laid out from the start, directly in the region. The
    /// spacing before it was already laid out.
    Start,
    /// The child broke across regions and continues from the given state.
    Continue(MultiState),
}

/// Layout multiple cells like a stack, one region at a time. Requires only the
/// spacing to insert between blocks, the stack growth direction, its children,
/// as well as relevant layout information.
///
/// In particular, this doesn't require creating a stack element explicitly, as
/// it requires `Content`, which has restrictions as to which values it can
/// hold. In particular, elements, even if internal, cannot contain
/// borrows/lifetime generics, even though they can have custom layout
/// procedures. Therefore, calling this function allows customizing stack layout
/// more deeply, such as for lists, which need a custom layout function that
/// might borrow data from the environment for each list item (a stack child).
/// Each child receives relevant layout data from the stack as well.
///
/// When called with `state` set to `None` for the first region and to the
/// returned state for each following region, this produces a frame for each
/// region, laying out children that break across regions one region at a
/// time.
///
/// The `children` function yields the children from the given index onwards.
/// A continuation only asks for the children from the one it continues with,
/// so that each region costs time proportional to the children laid out in
/// it rather than to all children.
#[expect(clippy::too_many_arguments)]
pub fn layout_stack_internal<'a, F, I>(
    children: impl FnOnce(usize) -> I,
    span: Span,
    spacing: Option<Spacing>,
    dir: Dir,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    regions: Regions,
    state: Option<&MultiState>,
) -> SourceResult<MultiStep>
where
    F: Fn(
        &mut Engine,
        StyleChain,
        Regions,
        Option<&MultiState>,
    ) -> SourceResult<MultiStep>,
    I: IntoIterator<Item = StackLayoutChild<'a, F>>,
{
    let state = state.map(MultiState::get::<StackState>);
    let axis = dir.axis();

    // Children before the one we continue with were laid out in earlier
    // regions. In the first region, all children are needed to provide
    // unique locations to them. Their locators are prepared once and kept
    // for the following regions.
    let (start, children, locals) = match state {
        Some(state) => (
            state.child,
            Either::Left(children(state.child).into_iter()),
            state.locals.clone(),
        ),
        None => {
            let children: Vec<_> = children(0).into_iter().collect();
            let mut split = locator.relayout().split();
            let locals = children
                .iter()
                .map(|child| match child {
                    StackLayoutChild::StackChild(StackChild::Block(block))
                        if transparent_spacing(block, axis).is_none() =>
                    {
                        split.next(&block.span()).local()
                    }
                    _ => 0,
                })
                .collect();
            (0, Either::Right(children.into_iter()), locals)
        }
    };

    let mut layouter = StackLayouter::new(span, dir, locator, styles, regions);
    let mut deferred = None;

    for (i, child) in (start..).zip(children) {
        // How to lay out the child if it's the one we continue with.
        let resume = if i == start { state.map(|s| &s.resume) } else { None };

        let step = match child {
            StackLayoutChild::StackChild(StackChild::Spacing(kind)) => {
                layouter.layout_spacing(*kind);
                deferred = None;
                continue;
            }
            StackLayoutChild::StackChild(StackChild::Block(block)) => {
                if let Some(amount) = transparent_spacing(block, axis) {
                    layouter.layout_spacing(amount);
                    deferred = None;
                    continue;
                }

                let locator = layouter.locator.with_local(locals[i]);
                let Some(inner) = layouter.prepare(&mut deferred, resume) else {
                    return layouter.finish_early(i, locals);
                };

                // Block-axis alignment of the `AlignElem` is respected by
                // stacks.
                let align = if let Some(align) = block.to_packed::<AlignElem>() {
                    align.alignment.get(styles)
                } else if let Some(styled) = block.to_packed::<StyledElem>() {
                    styles.chain(&styled.styles).get(AlignElem::alignment)
                } else {
                    styles.get(AlignElem::alignment)
                }
                .resolve(styles);

                let step = crate::flow::layout_fragment_step(
                    engine,
                    block,
                    locator,
                    styles,
                    layouter.regions,
                    inner,
                )?;
                (step, align)
            }
            StackLayoutChild::CustomLayouter(custom_layouter) => {
                let Some(inner) = layouter.prepare(&mut deferred, resume) else {
                    return layouter.finish_early(i, locals);
                };

                let align = styles.get(AlignElem::alignment).resolve(styles);
                let step = custom_layouter(engine, styles, layouter.regions, inner)?;
                (step, align)
            }
        };

        let (MultiStep { frame, next, ahead }, align) = step;
        layouter.push_frame(align, frame);

        // If the child continues in the next region, so does the stack,
        // starting with the child and whatever it laid out ahead.
        if let Some(next) = next {
            let frame = layouter.finish_region()?;
            let next = StackState {
                child: i,
                resume: Resume::Continue(next),
                locals: locals.clone(),
            };
            return Ok(MultiStep { frame, next: Some(MultiState::new(next)), ahead });
        }

        deferred = spacing;
    }

    let frame = layouter.finish_region()?;
    Ok(MultiStep::new(frame, None))
}

/// The amount of spacing if a block is `h` or `v` spacing along the stack's
/// axis, which is handled transparently.
fn transparent_spacing(block: &Content, axis: Axis) -> Option<Spacing> {
    match axis {
        Axis::X => block.to_packed::<HElem>().map(|h| h.amount),
        Axis::Y => block.to_packed::<VElem>().map(|v| v.amount),
    }
}

/// Performs stack layout for one region.
struct StackLayouter<'a> {
    /// The span to raise errors at during layout.
    span: Span,
    /// The stacking direction.
    dir: Dir,
    /// The axis of the stacking direction.
    axis: Axis,
    /// The stack's locator, whose link the children's locators share.
    locator: Locator<'a>,
    /// The inherited styles.
    styles: StyleChain<'a>,
    /// The regions to layout children into.
    regions: Regions<'a>,
    /// Whether the stack itself should expand to fill the region.
    expand: Axes<bool>,
    /// The regions before we started using up the current region.
    initial: Regions<'a>,
    /// The generic size used by the frames for the current region.
    used: GenericSize<Abs>,
    /// The sum of fractions in the current region.
    fr: Fr,
    /// Already layouted items whose exact positions are not yet known due to
    /// fractional spacing.
    items: Vec<StackItem>,
}

/// A prepared item in a stack layout.
enum StackItem {
    /// Absolute spacing between other items.
    Absolute(Abs),
    /// Fractional spacing between other items.
    Fractional(Fr),
    /// A frame for a layouted block.
    Frame(Frame, Axes<FixedAlignment>),
}

impl<'a> StackLayouter<'a> {
    /// Create a new stack layouter.
    fn new(
        span: Span,
        dir: Dir,
        locator: Locator<'a>,
        styles: StyleChain<'a>,
        mut regions: Regions<'a>,
    ) -> Self {
        let axis = dir.axis();
        let expand = regions.expand;

        // Disable expansion along the block axis for children.
        regions.expand.set(axis, false);

        Self {
            span,
            dir,
            axis,
            locator,
            styles,
            regions,
            expand,
            initial: regions,
            used: GenericSize::zero(),
            fr: Fr::zero(),
            items: vec![],
        }
    }

    /// Add spacing along the spacing direction.
    fn layout_spacing(&mut self, spacing: Spacing) {
        match spacing {
            Spacing::Rel(v) => {
                // Resolve the spacing and limit it to the remaining space.
                let resolved = v
                    .resolve(self.styles)
                    .relative_to(self.regions.base().get(self.axis));
                let limited = match self.axis {
                    Axis::X => resolved.min(self.regions.width()),
                    Axis::Y => self.regions.limited(resolved),
                };
                if self.dir.axis() == Axis::Y {
                    self.regions.consume(limited);
                }
                self.used.main += limited;
                self.items.push(StackItem::Absolute(resolved));
            }
            Spacing::Fr(v) => {
                self.fr += v;
                self.items.push(StackItem::Fractional(v));
            }
        }
    }

    /// Prepares for laying out a block or custom layouter child, given how to
    /// resume it.
    ///
    /// Lays out the `deferred` spacing and returns the state to lay out the
    /// child from, if it should be laid out in this region. Returns `None` if
    /// the region is full and the child must be laid out in the next one.
    fn prepare<'s>(
        &mut self,
        deferred: &mut Option<Spacing>,
        resume: Option<&'s Resume>,
    ) -> Option<Option<&'s MultiState>> {
        match resume {
            Some(Resume::Start) => Some(None),
            Some(Resume::Continue(inner)) => Some(Some(inner)),
            None => {
                if let Some(kind) = deferred.take() {
                    self.layout_spacing(kind);
                }
                if self.regions.is_full() { None } else { Some(None) }
            }
        }
    }

    /// Finishes the region before laying out the `child`-th child, which then
    /// starts the next region.
    fn finish_early(
        mut self,
        child: usize,
        locals: Arc<[u128]>,
    ) -> SourceResult<MultiStep> {
        let frame = self.finish_region()?;
        let next = StackState { child, resume: Resume::Start, locals };
        Ok(MultiStep::new(frame, Some(MultiState::new(next))))
    }

    /// Store a laid out frame, coming from either a block or a custom
    /// layouter.
    fn push_frame(&mut self, align: Axes<FixedAlignment>, frame: Frame) {
        // Grow our size, shrink the region and save the frame for later.
        let specific_size = frame.size();
        if self.dir.axis() == Axis::Y {
            self.regions.consume(specific_size.y);
        }

        let generic_size = match self.axis {
            Axis::X => GenericSize::new(specific_size.y, specific_size.x),
            Axis::Y => GenericSize::new(specific_size.x, specific_size.y),
        };

        self.used.main += generic_size.main;
        self.used.cross.set_max(generic_size.cross);

        self.items.push(StackItem::Frame(frame, align));
    }

    /// Finish the region, producing its frame.
    fn finish_region(&mut self) -> SourceResult<Frame> {
        // Determine the size of the stack in this region depending on whether
        // the region expands.
        let used = self.used.into_axes(self.axis);
        let initial = self.initial;
        let mut size = initial.fit(used, self.expand);

        // Expand fully if there are fr spacings. The remaining space only
        // matters then.
        let mut remaining = Abs::zero();
        if self.fr.get() > 0.0 {
            let full = match self.axis {
                Axis::X => initial.width(),
                Axis::Y => initial.height(),
            };
            remaining = full - self.used.main;
            if full.is_finite() {
                self.used.main = full;
                size.set(self.axis, full);
            }
        }

        if !size.is_finite() {
            bail!(self.span, "stack spacing is infinite");
        }

        let mut output = Frame::soft(size);
        let mut cursor = Abs::zero();
        let mut ruler: FixedAlignment = self.dir.start().into();

        // Place all frames.
        for item in self.items.drain(..) {
            match item {
                StackItem::Absolute(v) => cursor += v,
                StackItem::Fractional(v) => cursor += v.share(self.fr, remaining),
                StackItem::Frame(frame, align) => {
                    if self.dir.is_positive() {
                        ruler = ruler.max(align.get(self.axis));
                    } else {
                        ruler = ruler.min(align.get(self.axis));
                    }

                    // Align along the main axis.
                    let parent = size.get(self.axis);
                    let child = frame.size().get(self.axis);
                    let main = ruler.position(parent - self.used.main)
                        + if self.dir.is_positive() {
                            cursor
                        } else {
                            self.used.main - child - cursor
                        };

                    // Align along the cross axis.
                    let other = self.axis.other();
                    let cross = align
                        .get(other)
                        .position(size.get(other) - frame.size().get(other));

                    let pos = GenericSize::new(cross, main).to_point(self.axis);
                    cursor += child;
                    output.push_frame(pos, frame);
                }
            }
        }

        Ok(output)
    }
}

/// A generic size with main and cross axes. The axes are generic, meaning the
/// main axis could correspond to either the X or the Y axis.
#[derive(Default, Copy, Clone, Eq, PartialEq, Hash)]
struct GenericSize<T> {
    /// The cross component, along the axis perpendicular to the main.
    pub cross: T,
    /// The main component.
    pub main: T,
}

impl<T> GenericSize<T> {
    /// Create a new instance from the two components.
    const fn new(cross: T, main: T) -> Self {
        Self { cross, main }
    }

    /// Convert to the specific representation, given the current main axis.
    fn into_axes(self, main: Axis) -> Axes<T> {
        match main {
            Axis::X => Axes::new(self.main, self.cross),
            Axis::Y => Axes::new(self.cross, self.main),
        }
    }
}

impl GenericSize<Abs> {
    /// The zero value.
    fn zero() -> Self {
        Self { cross: Abs::zero(), main: Abs::zero() }
    }

    /// Convert to a point.
    fn to_point(self, main: Axis) -> Point {
        self.into_axes(main).to_point()
    }
}
