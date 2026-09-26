use std::sync::Arc;

use comemo::Track;
use smallvec::smallvec;
use typst_library::diag::SourceResult;
use typst_library::engine::Engine;
use typst_library::foundations::{Content, Context, Depth, Packed, Resolve, StyleChain};
use typst_library::introspection::Locator;
use typst_library::layout::{
    Abs, Axes, Dir, Frame, Length, MultiState, MultiStep, Point, Region, Regions, Size,
};
use typst_library::model::{
    EnumElem, ListElem, Numbering, ParElem, ParbreakElem, PdfMarkerTag,
};
use typst_library::text::TextElem;
use typst_syntax::Span;

use crate::grid::is_empty_frame;
use crate::stack::{StackLayoutChild, layout_stack_internal};

/// Layout the list, one region at a time.
#[typst_macros::time(span = elem.span())]
pub fn layout_list(
    elem: &Packed<ListElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    regions: Regions,
    state: Option<&MultiState>,
) -> SourceResult<MultiStep> {
    let indent = elem.indent.get(styles);
    let body_indent = elem.body_indent.get(styles);
    let tight = elem.tight.get(styles);
    let gutter = elem.spacing.get(styles).unwrap_or_else(|| {
        if tight { styles.get(ParElem::leading) } else { styles.get(ParElem::spacing) }
    });
    let is_rtl = styles.get(TextElem::dir).resolve(styles) == Dir::RTL;

    // Use the user's preferred vertical alignment. Among other things, it
    // avoids '#set align' interference with the list.
    let marker_align = elem.marker_align.get(styles);
    let baseline_align = marker_align.y().is_none();

    let layouter = ListLayouter::new(
        gutter,
        elem.span(),
        indent,
        body_indent,
        baseline_align,
        is_rtl,
        styles,
    );

    let items = |engine: &mut Engine| list_items(elem, engine, styles);
    layout_items(layouter, items, engine, locator, styles, regions, state)
}

/// Prepares the content of the list's items.
fn list_items(
    elem: &Packed<ListElem>,
    engine: &mut Engine,
    styles: StyleChain,
) -> SourceResult<Vec<ItemContent>> {
    let tight = elem.tight.get(styles);
    let Depth(depth) = styles.get(ListElem::depth);
    let marker_align = elem.marker_align.get(styles);
    let marker = elem
        .marker
        .get_ref(styles)
        .resolve(engine, styles, depth)?
        .aligned(marker_align);

    let mut items = vec![];
    for item in &elem.children {
        // Text in wide lists shall always turn into paragraphs.
        let mut body = item.body.clone();
        if !tight {
            body += ParbreakElem::shared();
        }
        let body = body.set(ListElem::depth, Depth(1));

        let item = ItemContent {
            marker: PdfMarkerTag::ListItemLabel(marker.clone()),
            body: PdfMarkerTag::ListItemBody(body),
        };

        items.push(item);
    }

    Ok(items)
}

/// Layout the enumeration, one region at a time.
#[typst_macros::time(span = elem.span())]
pub fn layout_enum(
    elem: &Packed<EnumElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    regions: Regions,
    state: Option<&MultiState>,
) -> SourceResult<MultiStep> {
    let indent = elem.indent.get(styles);
    let body_indent = elem.body_indent.get(styles);
    let tight = elem.tight.get(styles);
    let gutter = elem.spacing.get(styles).unwrap_or_else(|| {
        if tight { styles.get(ParElem::leading) } else { styles.get(ParElem::spacing) }
    });
    let is_rtl = styles.get(TextElem::dir).resolve(styles) == Dir::RTL;

    // Horizontally align based on the given respective parameter.
    // Vertically align to the top to avoid inheriting `horizon` or `bottom`
    // alignment from the context and having the number be displaced in
    // relation to the item it refers to.
    let number_align = elem.number_align.get(styles);
    let baseline_align = number_align.y().is_none();

    let layouter = ListLayouter::new(
        gutter,
        elem.span(),
        indent,
        body_indent,
        baseline_align,
        is_rtl,
        styles,
    );

    let items = |engine: &mut Engine| enum_items(elem, engine, styles);
    layout_items(layouter, items, engine, locator, styles, regions, state)
}

/// Prepares the content of the enumeration's items.
fn enum_items(
    elem: &Packed<EnumElem>,
    engine: &mut Engine,
    styles: StyleChain,
) -> SourceResult<Vec<ItemContent>> {
    let numbering = elem.numbering.get_ref(styles);
    let reversed = elem.reversed.get(styles);
    let tight = elem.tight.get(styles);

    let mut items = vec![];
    let mut number = elem
        .start
        .get(styles)
        .unwrap_or_else(|| if reversed { elem.children.len() as u64 } else { 1 });
    let mut parents = styles.get_cloned(EnumElem::parents);

    let full = elem.full.get(styles);
    let number_align = elem.number_align.get(styles);

    for item in &elem.children {
        number = item.number.get(styles).unwrap_or(number);

        let context = Context::new(None, Some(styles));
        let resolved = if full {
            parents.push(number);
            let content = numbering
                .apply(engine, context.track(), item.span(), &parents)?
                .display();
            parents.pop();
            content
        } else {
            match numbering {
                Numbering::Pattern(pattern) => TextElem::packed(pattern.apply_kth(
                    engine,
                    item.span(),
                    parents.len(),
                    number,
                )),
                Numbering::Func(_) => numbering
                    .apply(engine, context.track(), item.span(), &[number])?
                    .display(),
            }
        };

        // Disable overhang as a workaround to end-aligned dots glitching
        // and decreasing spacing between numbers and items.
        let resolved = resolved.aligned(number_align).set(TextElem::overhang, false);

        // Text in wide enums shall always turn into paragraphs.
        let mut body = item.body.clone();
        if !tight {
            body += ParbreakElem::shared();
        }

        let body = body.set(EnumElem::parents, smallvec![number]);

        let item = ItemContent {
            marker: PdfMarkerTag::ListItemLabel(resolved),
            body: PdfMarkerTag::ListItemBody(body),
        };

        items.push(item);
        number =
            if reversed { number.saturating_sub(1) } else { number.saturating_add(1) };
    }

    Ok(items)
}

/// Structure with the content for each list item.
struct ItemContent {
    /// The item marker.
    marker: Content,
    /// The item body.
    body: Content,
}

/// Layout data shared across all list items.
struct ListLayouter {
    /// Gutter spacing between items.
    gutter: Length,
    /// Span of the list/enum element.
    span: Span,
    /// List indent from the text start.
    indent: Abs,
    /// Indent between the marker and the body.
    body_indent: Abs,
    /// Whether baseline alignment should be enabled. When disabled, markers
    /// control their own alignment.
    baseline_align: bool,
    /// Whether RTL was the chosen text direction.
    is_rtl: bool,
    /// Maximum measured width of a marker, so they may align horizontally
    /// relative to each other.
    marker_width: Abs,
    /// If the list may not expand to fill the whole region (e.g. if `width:
    /// auto` was used), then this holds the measured width of an item body, so
    /// they may align horizontally relative to each other.
    body_width: Option<Abs>,
}

impl ListLayouter {
    /// Create a new list layouter with defaults for values that will be
    /// calculated later.
    fn new(
        gutter: Length,
        span: Span,
        indent: Length,
        body_indent: Length,
        baseline_align: bool,
        is_rtl: bool,
        styles: StyleChain,
    ) -> Self {
        let indent = indent.resolve(styles);
        let body_indent = body_indent.resolve(styles);

        Self {
            gutter,
            span,
            indent,
            body_indent,
            baseline_align,
            is_rtl,

            // These will be calculated later.
            marker_width: Abs::zero(),
            body_width: None,
        }
    }
}

/// Layout list items.
///
/// This is done in 4 steps:
///
/// 1. Compute marker widths for horizontal marker alignment;
/// 2. If necessary (when within a `width: auto` block or page), compute body
///    widths as well for horizontal body alignment to work;
/// 3. Generate list item layouters, each of which is responsible for vertical
///    marker alignment (baseline-aligned or not, depending on user settings);
/// 4. Pass each list item to the stack layouter. The stack layouter makes it
///    possible for them to expand to the full available width, allowing for center
///    or right alignment to work within the item body.
#[typst_macros::time(span = layouter.span)]
fn layout_items(
    mut layouter: ListLayouter,
    items: impl FnOnce(&mut Engine) -> SourceResult<Vec<ItemContent>>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    regions: Regions,
    state: Option<&MultiState>,
) -> SourceResult<MultiStep> {
    // The items, the locators used during measuring, and the measurements are
    // prepared for the first region and kept for the following ones. The
    // locators are stored to ensure the same locators will be used later when
    // laying out. This is needed to make introspection work properly. The
    // measurements only depend on the first region.
    let state = state.map(MultiState::get::<ListState>);
    let (items, locals) = match state {
        Some(state) => {
            layouter.marker_width = state.marker_width;
            layouter.body_width = state.body_width;
            (state.items.clone(), state.locals.clone())
        }
        None => {
            let items: Arc<[ItemContent]> = items(engine)?.into();
            let mut split = locator.relayout().split();
            let locals: Arc<[(u128, u128)]> = items
                .iter()
                .map(|item| {
                    let marker = split.next(&item.marker.span()).local();
                    let body = split.next(&item.body.span()).local();
                    (marker, body)
                })
                .collect();

            let locators: Vec<_> = locals
                .iter()
                .map(|&(marker, body)| {
                    (locator.with_local(marker), locator.with_local(body))
                })
                .collect();

            layouter.marker_width =
                measure_markers(&layouter, &items, &locators, engine, styles, regions)?;

            if regions.width().to_raw().is_infinite() || !regions.expand.x {
                layouter.body_width = Some(measure_bodies(
                    &layouter, &items, &locators, engine, styles, regions,
                )?);
            }

            (items, locals)
        }
    };

    // The stack only asks for the items from the one it continues with.
    let layouter = &layouter;
    let base = &locator;
    let cells = |start: usize| {
        items[start..]
            .iter()
            .zip(&locals[start..])
            .map(|(item, &(marker, body))| {
                let marker_locator = base.with_local(marker);
                let body_locator = base.with_local(body);
                StackLayoutChild::CustomLayouter(move |engine, styles, regions, state| {
                    layout_item(
                        item,
                        layouter,
                        engine,
                        &marker_locator,
                        &body_locator,
                        styles,
                        regions,
                        state,
                    )
                })
            })
    };

    let step = layout_stack_internal(
        cells,
        layouter.span,
        Some(layouter.gutter.into()),
        Dir::TTB,
        engine,
        // This locator should not be used by cells.
        locator.relayout().split().next(&()),
        styles,
        regions,
        state.map(|state| &state.stack),
    )?;

    let next = step.next.map(|stack| {
        MultiState::new(ListState {
            items: items.clone(),
            locals: locals.clone(),
            marker_width: layouter.marker_width,
            body_width: layouter.body_width,
            stack,
        })
    });

    Ok(MultiStep { frame: step.frame, next, ahead: step.ahead })
}

/// Where a list continues.
struct ListState {
    /// The content of the items.
    items: Arc<[ItemContent]>,
    /// The local hashes of the locators of the items' markers and bodies (see
    /// [`Locator::local`]).
    locals: Arc<[(u128, u128)]>,
    /// The measured marker width. See [`ListLayouter::marker_width`].
    marker_width: Abs,
    /// The measured body width. See [`ListLayouter::body_width`].
    body_width: Option<Abs>,
    /// Where the stack of items continues.
    stack: MultiState,
}

/// Measure marker.
fn measure_markers<'a>(
    list: &ListLayouter,
    items: &[ItemContent],
    locators: &[(Locator<'a>, Locator<'a>)],
    engine: &mut Engine,
    styles: StyleChain,
    regions: Regions,
) -> SourceResult<Abs> {
    let available_width = regions.width() - list.indent - list.body_indent;

    // Measure markers, so we can align them horizontally relative to the
    // largest width.
    let mut marker_width = Abs::zero();
    for (item, (marker_locator, _)) in items.iter().zip(locators) {
        let marker = crate::layout_frame(
            engine,
            &item.marker,
            marker_locator.relayout(),
            styles,
            Region::new(Axes::new(available_width, Abs::inf()), Axes::splat(false)),
        )?;
        marker_width.set_max(marker.width());
    }

    // Redistribute width if necessary. It is okay to do this before measuring
    // the body (thus effectively checking for the width in two places) since a
    // non-zero-width body would cause an overlarge marker to surpass page width
    // regardless. That is, this check doesn't affect the semantics of the other
    // check, but it is necessary in case we don't measure the body at all.
    Ok(marker_width.min(available_width))
}

/// Infinite space or `width: auto` used. Both would prevent the list from
/// expanding to fit, breaking alignment. Therefore, restrict the list size to
/// the size of the largest item, prompting list items to align between
/// themselves instead of relative to the full page width.
fn measure_bodies(
    list: &ListLayouter,
    items: &[ItemContent],
    locators: &[(Locator, Locator)],
    engine: &mut Engine,
    styles: StyleChain,
    regions: Regions,
) -> SourceResult<Abs> {
    let available_width = regions.width() - list.indent - list.body_indent;
    let mut measured_body_width = Abs::zero();
    for (item, (_, body_locator)) in items.iter().zip(locators) {
        let body = crate::layout_frame(
            engine,
            &item.body,
            body_locator.relayout(),
            styles,
            Region::new(Axes::new(available_width, Abs::inf()), Axes::splat(false)),
        )?;
        measured_body_width.set_max(body.width());
    }

    // If marker and body together exceed the page width, the marker gets the
    // space it requested and the body the rest. This makes some sense since the
    // marker comes first, is unlikely to be large, and is unlikely to be able
    // to wrap. It also keeps consistency between the case where we measure the
    // body and the case where we don't.
    Ok(measured_body_width.min(available_width - list.marker_width))
}

/// Layout the item, with support for vertical marker alignment, one region at
/// a time.
#[typst_macros::time(span = item.body.span())]
#[expect(clippy::too_many_arguments)]
fn layout_item(
    item: &ItemContent,
    list: &ListLayouter,
    engine: &mut Engine,
    marker_locator: &Locator,
    body_locator: &Locator,
    styles: StyleChain,
    regions: Regions,
    state: Option<&MultiState>,
) -> SourceResult<MultiStep> {
    let mut layouter =
        ItemLayouter::new(item, list, marker_locator, body_locator, styles, regions);

    // Continue the body in the next region.
    if let Some(state) = state.map(MultiState::get::<ItemState>) {
        layouter.first_frame = state.first_frame;
        layouter.body_offset = state.body_offset;
        layouter.marker_offset = state.marker_offset;
        let step =
            layouter.step_body(layouter.body_regions, engine, Some(&state.body))?;
        let frame = layouter.finish_frame(&state.marker, step.frame, state.index);
        let next = step.next.map(|body| {
            MultiState::new(ItemState {
                marker: state.marker.clone(),
                index: state.index + 1,
                body,
                ..*state
            })
        });
        return Ok(MultiStep { frame, next, ahead: step.ahead });
    }

    let mut marker = layouter.layout_marker(
        Region::new(
            Axes::new(list.marker_width, regions.base().y),
            Axes::new(true, false),
        ),
        engine,
    )?;

    let mut body = layouter.layout_body(layouter.body_regions, engine)?;

    if layouter.list.baseline_align {
        layouter.baseline_align(&marker, &mut body, engine)?;
    } else {
        layouter.vertical_align(&mut marker, &body, engine)?;
    }

    let frame = layouter.finish_frame(&marker, body.first.frame, 0);
    let ahead = body.first.ahead;
    let next = body.first.next.map(|body| {
        MultiState::new(ItemState {
            marker,
            first_frame: layouter.first_frame,
            body_offset: layouter.body_offset,
            marker_offset: layouter.marker_offset,
            index: 1,
            body,
        })
    });

    Ok(MultiStep { frame, next, ahead })
}

/// Where a list item continues.
struct ItemState {
    /// The laid out marker.
    marker: Frame,
    /// See [`ItemLayouter::first_frame`].
    first_frame: usize,
    /// See [`ItemLayouter::body_offset`].
    body_offset: Point,
    /// See [`ItemLayouter::marker_offset`].
    marker_offset: Point,
    /// The index of the next frame.
    index: usize,
    /// Where the body continues.
    body: MultiState,
}

/// The start of a list item's body.
struct BodyStart {
    /// The body's first frame and where it continues.
    first: MultiStep,
    /// The body's frames after the first one, up to the first non-empty one,
    /// as laid out into the predicted regions. Only known if the first frame
    /// could be skipped.
    rest: Vec<Frame>,
}

impl BodyStart {
    /// The body's `i`-th frame, if known.
    fn get(&self, i: usize) -> Option<&Frame> {
        match i {
            0 => Some(&self.first.frame),
            _ => self.rest.get(i - 1),
        }
    }
}

/// Layout data for a specific item.
///
/// Its methods assume only `layout_marker` and `layout_body` are used to layout
/// the item's contents.
struct ItemLayouter<'a> {
    /// Item content to layout.
    item: &'a ItemContent,
    /// List-wide layout data.
    list: &'a ListLayouter,
    /// Locator used to measure and layout the marker (must be the same to avoid
    /// introspection bugs).
    marker_locator: &'a Locator<'a>,
    /// Locator used to measure and layout the body (must be the same to avoid
    /// introspection bugs).
    body_locator: &'a Locator<'a>,
    /// Stylechain at this item's location.
    styles: StyleChain<'a>,
    /// The first body's non-empty frame.
    first_frame: usize,
    /// The item's regions, but adapted for the body's expected width.
    body_regions: Regions<'a>,
    /// Total body indent from the left of the region, as well as its offset
    /// from the top of the region.
    ///
    /// Vertical offset is always zero without baseline alignment.
    body_offset: Point,
    /// Resolved marker indent from the left of the region, as well as its
    /// offset from the top of the region.
    ///
    /// Vertical offset is always zero without baseline alignment.
    marker_offset: Point,
}

impl<'a> ItemLayouter<'a> {
    /// Begin item layout by resolving default values of attributes.
    fn new(
        item: &'a ItemContent,
        list: &'a ListLayouter,
        marker_locator: &'a Locator<'a>,
        body_locator: &'a Locator<'a>,
        styles: StyleChain<'a>,
        regions: Regions<'a>,
    ) -> Self {
        let total_body_indent = list.indent + list.marker_width + list.body_indent;

        // Restrict the body to the available space.
        let mut body_regions = regions;
        if let Some(body_width) = list.body_width {
            body_regions = body_regions.with_width(body_width);
            body_regions.expand.x = true;
        } else {
            body_regions =
                body_regions.with_width(body_regions.width() - total_body_indent);
        }

        Self {
            item,
            list,
            marker_locator,
            body_locator,
            styles,
            first_frame: 0,
            body_regions,
            body_offset: Point::with_x(total_body_indent),
            marker_offset: Point::with_x(list.indent),
        }
    }

    /// Layout the list marker with the given region data.
    fn layout_marker(&self, region: Region, engine: &mut Engine) -> SourceResult<Frame> {
        crate::layout_frame(
            engine,
            &self.item.marker,
            self.marker_locator.relayout(),
            self.styles,
            region,
        )
    }

    /// Layout the start of the list body with the given region data.
    fn layout_body(
        &mut self,
        regions: Regions,
        engine: &mut Engine,
    ) -> SourceResult<BodyStart> {
        let first = self.step_body(regions, engine, None)?;

        // Update the first non-empty frame (ignoring a frame with only tags due
        // to a forced region break). If the first frame is not virtually empty,
        // then keep the default of 0. This requires looking at the following
        // frames, which we lay out into the predicted regions.
        let mut rest = vec![];
        if first.next.is_some() && is_empty_frame(&first.frame) {
            rest = crate::flow::peek_remaining(
                engine,
                regions,
                first.next.clone(),
                |engine, regions, state| self.step_body(regions, engine, Some(state)),
                |frame| !is_empty_frame(frame),
            )?;
            if rest.last().is_some_and(|f| !is_empty_frame(f)) {
                self.first_frame = 1;
            }
        }

        Ok(BodyStart { first, rest })
    }

    /// Layout one region of the list body.
    fn step_body(
        &self,
        regions: Regions,
        engine: &mut Engine,
        state: Option<&MultiState>,
    ) -> SourceResult<MultiStep> {
        crate::flow::layout_fragment_step(
            engine,
            &self.item.body,
            self.body_locator.relayout(),
            self.styles,
            regions,
            state,
        )
    }

    /// Ensure baselines are aligned by either increasing the marker's offset,
    /// if the marker is above the body and should thus be moved down, or by both
    /// increasing the body's offset and relayouting it with less space, if the
    /// body is above the marker.
    fn baseline_align(
        &mut self,
        marker: &Frame,
        body: &mut BodyStart,
        engine: &mut Engine,
    ) -> SourceResult<()> {
        // Difference between marker and body baselines, for alignment. A
        // positive 'diff' means that the marker is above and must move down,
        // whereas a negative 'diff' means that the marker is below, so the body
        // must be moved down instead.
        let diff = if marker.has_baseline()
            && let Some(first) = body.get(self.first_frame)
            && first.has_baseline()
        {
            first.baseline() - marker.baseline()
        } else {
            // One of the frames has no natural baseline, so baseline alignment
            // is disabled.
            Abs::zero()
        };

        if diff >= Abs::zero() {
            // Marker's baseline is above the body's baseline, so we can align
            // them by moving the baseline downwards, without moving the body.
            self.marker_offset.y = diff;
        } else {
            // Marker's baseline is below the body's baseline, so move the body
            // down to avoid overflowing the marker into something above the
            // list (item).
            //
            // To do this, we layout the body again but with '-diff' less space
            // (that is, '+diff' more space, as it is negative), and then move
            // the result '-diff' units downwards. Of course, this could
            // theoretically generate a new result that is even worse - but
            // there is only so much we can do with a finite number of
            // iterations.
            let mut regions = self.body_regions;
            regions.consume(-diff);
            *body = self.layout_body(regions, engine)?;

            self.body_offset.y = -diff;
        }

        Ok(())
    }

    /// Called when explicit marker alignment was specified. Re-layout the
    /// marker with the same height as the body's first frame so it may align
    /// itself vertically with the body.
    fn vertical_align(
        &mut self,
        marker: &mut Frame,
        body: &BodyStart,
        engine: &mut Engine,
    ) -> SourceResult<()> {
        // 'Measuring' the height of an 'auto row'.
        let height = if let Some(body_first) = body.get(self.first_frame) {
            // Don't align if the body is too short.
            body_first.height().max(marker.height())
        } else {
            // Body appears to be fully empty, so the marker should not align.
            marker.height()
        };

        let region =
            Region::new(Axes::new(self.list.marker_width, height), Axes::splat(true));

        *marker = self.layout_marker(region, engine)?;
        Ok(())
    }

    /// Finish a frame of the list item by indenting the body's frame and
    /// adding the marker if it is the first non-empty frame.
    fn finish_frame(&self, marker: &Frame, body_frame: Frame, i: usize) -> Frame {
        let width = self.body_offset.x + body_frame.width();

        let mut height = body_frame.height() + self.body_offset.y;
        if i == self.first_frame {
            // Also consider the marker height, but only for the frame into
            // which it will be placed.
            height.set_max(marker.height() + self.marker_offset.y);
        }

        let mut frame = Frame::soft(Size::new(width, height));

        let mut body_pos = self.body_offset;
        if self.list.is_rtl {
            // In RTL, items expand to the left, thus the position must
            // additionally be offset by the full width. However, since the
            // body is always at the end, it will expand to the right in
            // LTR, and therefore to the left in RTL. That is, its leftmost
            // corner must be at the left of the frame, since it now expands
            // to the left.
            //
            // Or, mathematically:
            // body_pos.x = width - (body_frame.width() + body_pos.x)
            //            = self.body_offset.x + body_frame.width() -
            //                  - (body_frame.width() + self.body_offset.x)
            //            = 0.
            body_pos.x = Abs::zero();
        }

        // Only place the marker on the first non-empty frame.
        if i == self.first_frame {
            let mut marker_pos = self.marker_offset;
            if self.list.is_rtl {
                marker_pos.x = width - (self.list.marker_width + marker_pos.x);
            }
            frame.push_frame(marker_pos, marker.clone());
        }

        frame.push_frame(body_pos, body_frame);
        frame
    }
}
