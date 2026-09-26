mod layouter;
mod lines;
mod repeated;
mod rowspans;

pub use self::layouter::GridLayouter;
pub(crate) use self::layouter::is_empty_frame;

use std::sync::Arc;

use typst_library::diag::SourceResult;
use typst_library::engine::Engine;
use typst_library::foundations::{Content, NativeElement, Packed, StyleChain};
use typst_library::introspection::{Location, Locator, SplitLocator, Tag, TagFlags};
use typst_library::layout::grid::resolve::{Cell, CellGrid};
use typst_library::layout::{
    Fragment, Frame, FrameItem, FrameParent, GridCell, GridElem, Inherit, MultiState,
    MultiStep, Point, Regions,
};
use typst_library::model::{TableCell, TableElem};
use typst_syntax::Span;

use self::layouter::{GridSnapshot, RowPiece};
use self::lines::{
    LineSegment, generate_line_segments, hline_stroke_at_column, vline_stroke_at_row,
};
use self::rowspans::{Rowspan, UnbreakableRowGroup};

/// Layout the cell into the given regions.
///
/// The `disambiguator` indicates which instance of this cell this should be
/// layouted as. For normal cells, it is always `0`, but for headers and
/// footers, it indicates the index of the header/footer among all. See the
/// [`Locator`] docs for more details on the concepts behind this.
pub fn layout_cell(
    cell: &Cell,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    regions: Regions,
    is_repeated: bool,
) -> SourceResult<Fragment> {
    // HACK: manually generate tags for table and grid cells. Ideally table and
    // grid cells could just be marked as locatable, but the tags are somehow
    // considered significant for layouting. This hack together with a check in
    // the grid layouter makes the test suite pass.
    let mut locator = locator.split();
    let tags = generate_cell_tags(cell, is_repeated, &mut locator, engine);

    let locator = locator.next(&cell.body.span());
    let fragment = crate::layout_fragment(engine, &cell.body, locator, styles, regions)?;

    // Manually insert tags.
    let mut frames = fragment.into_frames();
    if let Some(tags) = tags
        && let Some((first, remainder)) = frames.split_first_mut()
    {
        for frame in remainder.iter_mut() {
            frame.set_parent(FrameParent::new(tags.1, Inherit::Yes));
        }
        insert_cell_tags(first, tags, !remainder.is_empty());
    }

    Ok(Fragment::frames(frames))
}

/// Lays out one region of a cell, like [`layout_cell`] does for all regions.
///
/// When called with `state` set to `None` for the first region and to the
/// returned state for each following region, this produces the frames that
/// [`layout_cell`] would produce for these regions.
pub fn layout_cell_step(
    cell: &Cell,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    regions: Regions,
    is_repeated: bool,
    state: Option<&MultiState>,
) -> SourceResult<MultiStep> {
    let state = state.map(MultiState::get::<CellState>);

    // Set up the tags and locator in the same way as `layout_cell`. The tags
    // are only generated for the first region. The locator of the body
    // doesn't depend on them.
    let mut locator = locator.split();
    let tags = if state.is_none() {
        generate_cell_tags(cell, is_repeated, &mut locator, engine)
    } else {
        None
    };

    let locator = locator.next(&cell.body.span());
    let MultiStep { mut frame, next, ahead } = crate::flow::layout_fragment_step(
        engine,
        &cell.body,
        locator,
        styles,
        regions,
        state.map(|state| &state.body),
    )?;

    // Manually insert tags, like `layout_cell`.
    let loc = match (tags, state) {
        (Some(tags), _) => {
            let loc = tags.1;
            insert_cell_tags(&mut frame, tags, next.is_some());
            Some(loc)
        }
        (None, Some(CellState { loc: Some(loc), .. })) => {
            frame.set_parent(FrameParent::new(*loc, Inherit::Yes));
            Some(*loc)
        }
        (None, _) => None,
    };

    let next = next.map(|body| MultiState::new(CellState { body, loc }));
    Ok(MultiStep { frame, next, ahead })
}

/// Where a cell laid out with [`layout_cell_step`] continues.
struct CellState {
    /// Where the cell's body continues.
    body: MultiState,
    /// The location of the cell's tags, if it has any.
    loc: Option<Location>,
}

/// Generates the tags for a table or grid cell (see [`layout_cell`]).
fn generate_cell_tags(
    cell: &Cell,
    is_repeated: bool,
    locator: &mut SplitLocator,
    engine: &mut Engine,
) -> Option<(Content, Location, u128)> {
    if let Some(table_cell) = cell.body.to_packed::<TableCell>() {
        let mut table_cell = table_cell.clone();
        table_cell.is_repeated.set(is_repeated);
        Some(generate_tags(table_cell, locator, engine))
    } else if let Some(grid_cell) = cell.body.to_packed::<GridCell>() {
        let mut grid_cell = grid_cell.clone();
        grid_cell.is_repeated.set(is_repeated);
        Some(generate_tags(grid_cell, locator, engine))
    } else {
        None
    }
}

/// Inserts a cell's tags into its first frame.
///
/// If the cell `continues` in more frames, the logical parent of all of its
/// frames must be the cell, which converts them to group frames. Then, the
/// start and end tags containing no content are prepended. The first frame is
/// also a logical child to guarantee correct ordering in the introspector,
/// since logical children are currently inserted immediately after the start
/// tag of the parent element preceding any content within the parent
/// element's tags.
fn insert_cell_tags(
    frame: &mut Frame,
    (elem, loc, key): (Content, Location, u128),
    continues: bool,
) {
    let flags = TagFlags { introspectable: true, tagged: true };
    let start = FrameItem::Tag(Tag::Start(elem, flags));
    let end = FrameItem::Tag(Tag::End(loc, key, flags));
    if continues {
        frame.set_parent(FrameParent::new(loc, Inherit::Yes));
        frame.prepend_multiple([(Point::zero(), start), (Point::zero(), end)]);
    } else {
        frame.prepend(Point::zero(), start);
        frame.push(Point::zero(), end);
    }
}

fn generate_tags<T: NativeElement>(
    mut cell: Packed<T>,
    locator: &mut SplitLocator,
    engine: &mut Engine,
) -> (Content, Location, u128) {
    let key = typst_utils::hash128(&cell);
    let loc = locator.next_location(engine, key, cell.span());
    cell.set_location(loc);
    (cell.pack(), loc, key)
}

/// Layout the grid, one region at a time.
#[typst_macros::time(span = elem.span())]
pub fn layout_grid(
    elem: &Packed<GridElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    regions: Regions,
    state: Option<&MultiState>,
) -> SourceResult<MultiStep> {
    let grid = elem.grid.as_ref().unwrap();
    step_grid(grid, engine, locator, styles, regions, state, elem.span())
}

/// Layout the table, one region at a time.
#[typst_macros::time(span = elem.span())]
pub fn layout_table(
    elem: &Packed<TableElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    regions: Regions,
    state: Option<&MultiState>,
) -> SourceResult<MultiStep> {
    let grid = elem.grid.as_ref().unwrap();
    step_grid(grid, engine, locator, styles, regions, state, elem.span())
}

/// Where a grid continues.
///
/// Grid layout can't simply be paused at a region break: A row can span
/// multiple regions and rowspans are laid out into earlier regions once
/// their last row is known. Instead, the grid layouter is resumed from a
/// snapshot taken between two rows and lays out until the frame of the next
/// region can't change anymore.
///
/// The snapshot may thus already contain rows laid out into the regions that
/// were predicted to follow. The step declares how much of these regions
/// they use up ([`MultiStep::ahead`]), so that they are checked against the
/// actual regions.
struct GridState {
    /// The index of the region to produce a frame for next.
    region: usize,
    /// The snapshot to resume from. Taken in that region or a later one.
    snapshot: Arc<GridSnapshot>,
}

/// Lays out one region of a grid.
fn step_grid(
    grid: &CellGrid,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    regions: Regions,
    state: Option<&MultiState>,
    span: Span,
) -> SourceResult<MultiStep> {
    let state = state.map(MultiState::get::<GridState>);

    // The region to produce a frame for.
    let region = state.map_or(0, |s| s.region);

    // Resume from the snapshot or start from scratch.
    let mut layouter = match state {
        None => {
            let mut layouter = GridLayouter::new(grid, regions, locator, styles, span);
            layouter.start(engine)?;
            layouter
        }
        Some(state) => {
            // Advance to the region the snapshot was taken in.
            let mut regions = regions;
            for _ in region..state.snapshot.region {
                regions.next();
            }
            let mut layouter = GridLayouter::restore(
                grid,
                &state.snapshot,
                regions,
                locator,
                styles,
                span,
            );
            layouter.discard_until(region);
            layouter
        }
    };

    // Lay out until the region's frame can't change anymore.
    while !layouter.is_final(region) {
        layouter.advance(engine)?;
    }

    let frame = layouter.take_frame(region);
    if layouter.is_done() && layouter.region_index() == region + 1 {
        return Ok(MultiStep::new(frame, None));
    }

    // The height used up in the regions that the snapshot already laid out
    // rows into. Once the grid is done, there is no current region.
    layouter.discard_until(region + 1);
    let end = layouter.region_index() - usize::from(layouter.is_done());
    let ahead = (region + 1..=end).map(|r| layouter.used_in(r)).collect();

    let next = GridState {
        region: region + 1,
        snapshot: Arc::new(layouter.snapshot()),
    };
    Ok(MultiStep { frame, next: Some(MultiState::new(next)), ahead })
}
