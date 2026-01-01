//! Splitting math items into a 2D grid by linebreaks and alignment points.

use bumpalo::Bump;

use super::{FencePart, FencedItem, MathComponent, MathItem, MathKind};

/// A 2D grid of math items, split by linebreaks (rows) and alignment points (columns).
#[derive(Debug, Clone)]
pub struct MathGrid<'a> {
    /// The rows of the grid. Each row contains columns, and each column
    /// contains a slice of items.
    pub rows: Vec<MathRow<'a>>,
}

/// A single row in a math grid.
#[derive(Debug, Clone)]
pub struct MathRow<'a> {
    /// The columns in this row, separated by alignment points.
    pub columns: Vec<&'a [MathItem<'a>]>,
}

impl<'a> MathGrid<'a> {
    /// Returns true if the grid has more than one row.
    pub fn is_multiline(&self) -> bool {
        self.rows.len() > 1
    }

    /// Returns the number of rows in the grid.
    pub fn row_count(&self) -> usize {
        self.rows.len()
    }

    /// Returns the maximum number of columns across all rows.
    pub fn max_columns(&self) -> usize {
        self.rows.iter().map(|r| r.columns.len()).max().unwrap_or(0)
    }
}

impl<'a> MathItem<'a> {
    /// Split this math item into a 2D grid by linebreaks and alignment points.
    ///
    /// - Rows are separated by `MathItem::Linebreak`
    /// - Columns within each row are separated by `MathItem::Align`
    /// - Fenced items containing linebreaks are split, with each part
    ///   retaining a reference to the full body for height calculation.
    pub fn into_grid(&'a self, bump: &'a Bump) -> MathGrid<'a> {
        let items = self.as_slice();

        // First pass: split into rows by linebreaks
        let row_items = split_rows(items, bump);

        // Second pass: split each row into columns by alignment points
        let rows = row_items
            .into_iter()
            .map(|items| {
                let columns = split_columns(items, bump);
                MathRow { columns }
            })
            .collect();

        MathGrid { rows }
    }
}

/// Split items into rows by linebreaks, handling fenced items that span rows.
fn split_rows<'a>(items: &'a [MathItem<'a>], bump: &'a Bump) -> Vec<&'a [MathItem<'a>]> {
    // Check if there are any linebreaks (including in nested fences)
    if !has_linebreaks(items) {
        return vec![items];
    }

    // We need to build rows, potentially modifying items for split fences
    let mut rows: Vec<Vec<MathItem<'a>>> = vec![vec![]];

    split_rows_into(items, &mut rows, bump);

    // Convert Vec<Vec<MathItem>> to Vec<&[MathItem]> by allocating in bump
    rows.into_iter()
        .map(|row| bump.alloc_slice_fill_iter(row) as &[MathItem<'a>])
        .collect()
}

/// Recursively split items into rows, handling nested fences.
fn split_rows_into<'a>(
    items: &'a [MathItem<'a>],
    rows: &mut Vec<Vec<MathItem<'a>>>,
    bump: &'a Bump,
) {
    for item in items {
        match item {
            MathItem::Linebreak => {
                // Start a new row
                rows.push(vec![]);
            }
            MathItem::Component(comp) => {
                if let MathKind::Fenced(fence) = &comp.kind {
                    if fence_has_linebreaks(fence) {
                        // Split the fence across rows
                        split_fence_into_rows(fence, comp, rows, bump);
                    } else {
                        // Fence has no linebreaks, add as-is
                        rows.last_mut().unwrap().push(item.clone());
                    }
                } else {
                    // Non-fence component, add as-is
                    rows.last_mut().unwrap().push(item.clone());
                }
            }
            _ => {
                // Other items (Space, Spacing, Align, Tag), add to current row
                rows.last_mut().unwrap().push(item.clone());
            }
        }
    }
}

/// Check if any items contain linebreaks (including nested in fences).
fn has_linebreaks(items: &[MathItem]) -> bool {
    for item in items {
        match item {
            MathItem::Linebreak => return true,
            MathItem::Component(MathComponent { kind: MathKind::Fenced(fence), .. }) => {
                if fence_has_linebreaks(fence) {
                    return true;
                }
            }
            _ => {}
        }
    }
    false
}

/// Check if a fence has linebreaks in its body (recursively).
fn fence_has_linebreaks(fence: &FencedItem) -> bool {
    let body_items = fence.body.as_slice();
    for item in body_items {
        match item {
            MathItem::Linebreak => return true,
            MathItem::Component(MathComponent { kind: MathKind::Fenced(inner), .. }) => {
                if fence_has_linebreaks(inner) {
                    return true;
                }
            }
            _ => {}
        }
    }
    false
}

/// Split a fence across multiple rows.
fn split_fence_into_rows<'a>(
    fence: &'a FencedItem<'a>,
    comp: &MathComponent<'a>,
    rows: &mut Vec<Vec<MathItem<'a>>>,
    bump: &'a Bump,
) {
    let body_items = fence.body.as_slice();

    // Split the body items into segments by linebreaks
    let segments = split_body_segments(body_items, bump);
    let segment_count = segments.len();

    for (i, segment) in segments.into_iter().enumerate() {
        let part = if i == 0 {
            FencePart::Start
        } else if i == segment_count - 1 {
            FencePart::End
        } else {
            FencePart::Middle
        };

        // Create split fence for this segment
        let split_fence =
            fence.create_split(part, segment, comp.styles, comp.props.span, bump);

        if i == 0 {
            // Add to current row
            rows.last_mut().unwrap().push(split_fence);
        } else {
            // Start a new row with this segment
            rows.push(vec![split_fence]);
        }
    }
}

/// Split body items into segments by linebreaks, handling nested fences.
///
/// Returns segments where each segment is a slice of items between linebreaks.
/// For nested fences with linebreaks, the fence is split and placed in
/// appropriate segments.
fn split_body_segments<'a>(
    items: &'a [MathItem<'a>],
    bump: &'a Bump,
) -> Vec<&'a [MathItem<'a>]> {
    // Fast path: no linebreaks at all
    if !has_linebreaks(items) {
        return vec![items];
    }

    // Check if we only have top-level linebreaks (no nested fence linebreaks)
    let has_nested_fence_linebreaks = items.iter().any(|item| {
        matches!(
            item,
            MathItem::Component(MathComponent { kind: MathKind::Fenced(fence), .. })
            if fence_has_linebreaks(fence)
        )
    });

    if !has_nested_fence_linebreaks {
        // Simple case: just split by top-level linebreaks
        return split_at_top_level_linebreaks(items);
    }

    // Complex case: need to handle nested fence linebreaks
    let mut segments: Vec<Vec<MathItem<'a>>> = vec![vec![]];

    for item in items {
        match item {
            MathItem::Linebreak => {
                segments.push(vec![]);
            }
            MathItem::Component(comp) => {
                if let MathKind::Fenced(fence) = &comp.kind {
                    if fence_has_linebreaks(fence) {
                        // Recursively split the nested fence
                        let inner_segments = split_body_segments(fence.body.as_slice(), bump);
                        let inner_count = inner_segments.len();

                        for (i, inner_segment) in inner_segments.into_iter().enumerate() {
                            let part = if i == 0 {
                                FencePart::Start
                            } else if i == inner_count - 1 {
                                FencePart::End
                            } else {
                                FencePart::Middle
                            };

                            let split_fence = fence.create_split(
                                part,
                                inner_segment,
                                comp.styles,
                                comp.props.span,
                                bump,
                            );

                            if i == 0 {
                                segments.last_mut().unwrap().push(split_fence);
                            } else {
                                segments.push(vec![split_fence]);
                            }
                        }
                    } else {
                        segments.last_mut().unwrap().push(item.clone());
                    }
                } else {
                    segments.last_mut().unwrap().push(item.clone());
                }
            }
            _ => {
                segments.last_mut().unwrap().push(item.clone());
            }
        }
    }

    // Convert to slices
    segments
        .into_iter()
        .map(|seg| bump.alloc_slice_fill_iter(seg) as &[MathItem<'a>])
        .collect()
}

/// Split items at top-level linebreaks only (no nested fence handling).
/// Returns slices into the original array.
fn split_at_top_level_linebreaks<'a>(items: &'a [MathItem<'a>]) -> Vec<&'a [MathItem<'a>]> {
    let mut segments = vec![];
    let mut start = 0;

    for (i, item) in items.iter().enumerate() {
        if matches!(item, MathItem::Linebreak) {
            segments.push(&items[start..i]);
            start = i + 1;
        }
    }

    // Add the last segment
    if start <= items.len() {
        segments.push(&items[start..]);
    }

    segments
}

/// Split a row's items into columns by alignment points.
fn split_columns<'a>(items: &'a [MathItem<'a>], _bump: &'a Bump) -> Vec<&'a [MathItem<'a>]> {
    // Check if there are any alignment points
    let has_align = items.iter().any(|item| matches!(item, MathItem::Align));

    if !has_align {
        return vec![items];
    }

    // Split by alignment points
    let mut columns = vec![];
    let mut start = 0;

    for (i, item) in items.iter().enumerate() {
        if matches!(item, MathItem::Align) {
            columns.push(&items[start..i]);
            start = i + 1;
        }
    }

    // Add the last column
    if start <= items.len() {
        columns.push(&items[start..]);
    }

    // Note: We don't need bump allocation here since we're just slicing
    // the existing items array.
    columns
}
