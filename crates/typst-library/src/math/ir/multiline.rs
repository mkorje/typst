use bumpalo::{Bump, collections::Vec as BumpVec};
use typst_syntax::Span;

use super::item::*;
use crate::foundations::{Resolve, StyleChain};

/// Builds a `MultilineItem` from preprocessed items that contain linebreaks.
///
/// This function:
/// 1. Splits items at `Linebreak` markers into rows.
/// 2. Splits each row at `Align` markers into columns.
/// 3. Pads all rows to have the same number of columns.
/// 4. Wraps multi-item columns in GroupItems.
pub(super) fn build_multiline<'a>(
    items: BumpVec<'a, MathItem<'a>>,
    styles: StyleChain<'a>,
    bump: &'a Bump,
) -> MathItem<'a> {
    let span = items
        .iter()
        .find_map(|item| {
            let s = item.span();
            if s.is_detached() { None } else { Some(s) }
        })
        .unwrap_or(Span::detached());

    // Step 1+2: Split items into rows, then split each row at Align markers.
    let row_count = items
        .iter()
        .filter(|item| matches!(item, MathItem::Linebreak))
        .count()
        + 1;
    let mut cell_rows: BumpVec<'a, BumpVec<'a, BumpVec<'a, MathItem<'a>>>> =
        BumpVec::with_capacity_in(row_count, bump);
    let mut row = vec![];
    for item in items {
        if matches!(item, MathItem::Linebreak) {
            cell_rows.push(split_at_align(row.drain(..), bump));
        } else {
            row.push(item);
        }
    }
    cell_rows.push(split_at_align(row, bump));

    // Step 3: Pad rows to have the same number of columns.
    let max_cols = cell_rows.iter().map(|r| r.len()).max().unwrap_or(0);

    // Step 4: Wrap each column's items into a single MathItem.
    let bump_rows = BumpVec::from_iter_in(
        cell_rows.into_iter().map(|mut row| {
            while row.len() < max_cols {
                row.push(BumpVec::new_in(bump));
            }

            BumpVec::from_iter_in(
                row.into_iter().map(|cell| GroupItem::wrap_preprocessed(cell, styles)),
                bump,
            )
        }),
        bump,
    );

    MultilineItem::create(bump_rows, styles, span)
}

/// Splits preprocessed items at `Align` markers into columns, fixing up
/// spacing that was computed across column boundaries.
///
/// During preprocessing, Align markers are transparent, so lspace and
/// rspace may span alignment boundaries. When splitting into columns:
/// - lspace on the first non-ignorant item after an Align is moved to
///   the end of the previous column (matching layout_realized behavior).
/// - rspace on the last non-ignorant item in a right-aligned column
///   (even index) is moved to the paired left column (odd index) when
///   that column has no non-ignorant content, to prevent it from
///   inflating the right column's width across logical column boundaries.
///
pub(crate) fn split_at_align<'a, I>(
    items: I,
    bump: &'a Bump,
) -> BumpVec<'a, BumpVec<'a, MathItem<'a>>>
where
    I: IntoIterator<Item = MathItem<'a>>,
{
    let mut cols: BumpVec<'a, BumpVec<'a, MathItem<'a>>> = BumpVec::new_in(bump);
    cols.push(BumpVec::new_in(bump));

    let mut at_boundary = false;
    for mut item in items {
        if matches!(item, MathItem::Align) {
            // When crossing a logical column boundary (starting a new
            // even column), fix up rspace in the completed pair.
            if cols.len() % 2 == 0 {
                fixup_rspace_in_last_pair(&mut cols);
            }
            cols.push(BumpVec::new_in(bump));
            at_boundary = true;
            continue;
        }

        // If we just passed an alignment point, check if this item
        // has lspace that should be moved to the previous column.
        if at_boundary && !item.is_ignorant() {
            if let MathItem::Component(ref mut comp) = item
                && let Some(lspace) = comp.props.lspace.take()
            {
                // Move the lspace to the end of the previous
                // column as explicit spacing.
                let resolved = lspace.resolve(comp.styles);
                let idx = cols.len() - 2;
                cols[idx].push(MathItem::Spacing(resolved, false));
            }
            at_boundary = false;
        }
        cols.last_mut().unwrap().push(item);
    }

    // Fix up the last pair if complete.
    if cols.len() % 2 == 0 {
        fixup_rspace_in_last_pair(&mut cols);
    }
    cols
}

/// When the last two columns in `cols` form a complete (right, left) pair
/// and the left-aligned column has no non-ignorant content, moves rspace from
/// the last non-ignorant item in the right-aligned column to the left-aligned
/// column.
fn fixup_rspace_in_last_pair<'a>(cols: &mut [BumpVec<'a, MathItem<'a>>]) {
    let [.., right_aligned, left_aligned] = cols else { return };
    let left_has_content = left_aligned
        .iter()
        .any(|item| matches!(item, MathItem::Component(comp) if !comp.props.ignorant));
    if left_has_content {
        return;
    }
    let spacing = right_aligned.iter_mut().rev().find_map(|item| {
        if let MathItem::Component(comp) = item
            && !comp.props.ignorant
        {
            comp.props.rspace.take().map(|rs| rs.resolve(comp.styles))
        } else {
            None
        }
    });
    if let Some(resolved) = spacing {
        left_aligned.push(MathItem::Spacing(resolved, false));
    }
}
