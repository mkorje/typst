//! Shared row/column splitting for multiline math + structural views over
//! `\left ... \right` groups.
//!
//! `Linebreak`, `Align`, and `Mid` are first-class markers in the shallow
//! IR — they sit in the flat `Vec<MathChild>` sequence and the consumer
//! decides what to do with them. This module provides the canonical
//! "views" both layout pipelines (paged and HTML/MathML) use, so the
//! marker-aware logic and structural normalization live in one place.

use unicode_math_class::MathClass;

use super::item::MathChild;

/// A flat child sequence interpreted as a 2D grid.
///
/// Rows are split on `MathChild::Linebreak`, columns within each row on
/// `MathChild::Align`. The resulting grid is normalized so every row has
/// the same column count (shorter rows are padded with empty column
/// slices). A single trailing `Linebreak` is dropped to match author
/// expectations (`a \\` doesn't produce a trailing empty row).
///
/// All slices point into the original `&[MathChild]` — no children are
/// cloned.
#[derive(Debug)]
pub struct MultilineView<'a, 'c> {
    /// `rows[r][c]` is the slice of children for the cell at row `r`,
    /// column `c`.
    pub rows: Vec<Vec<&'c [MathChild<'a>]>>,
    /// Whether the source contained at least one explicit `Linebreak`.
    pub is_multiline: bool,
}

impl<'a, 'c> MultilineView<'a, 'c> {
    /// Build the 2D view from a flat child sequence.
    pub fn new(children: &'c [MathChild<'a>]) -> Self {
        // Strip a single trailing linebreak (matches what the old
        // `process_group` did).
        let effective = if let Some(MathChild::Linebreak) = children.last() {
            &children[..children.len() - 1]
        } else {
            children
        };

        let raw_rows = split_rows(effective);
        let is_multiline = raw_rows.len() > 1
            || children.len() != effective.len();

        let mut rows: Vec<Vec<&'c [MathChild<'a>]>> = raw_rows
            .into_iter()
            .map(split_columns)
            .collect();

        // Pad to uniform column count.
        let ncols = rows.iter().map(|r| r.len()).max().unwrap_or(0);
        let empty: &[MathChild<'a>] = &[];
        for row in &mut rows {
            row.resize(ncols, empty);
        }

        Self { rows, is_multiline }
    }

    /// Number of rows in the grid.
    pub fn nrows(&self) -> usize {
        self.rows.len()
    }

    /// Number of columns in the grid (uniform across rows).
    pub fn ncols(&self) -> usize {
        self.rows.first().map_or(0, |r| r.len())
    }

    /// Is this a single row with a single column? (No markers anywhere.)
    pub fn is_flat(&self) -> bool {
        !self.is_multiline && self.ncols() <= 1
    }
}

/// Split a flat child sequence on `MathChild::Linebreak`.
///
/// Always returns at least one row (possibly empty).
pub fn split_rows<'a, 'c>(
    children: &'c [MathChild<'a>],
) -> Vec<&'c [MathChild<'a>]> {
    let mut rows = Vec::new();
    let mut start = 0;
    for (i, child) in children.iter().enumerate() {
        if matches!(child, MathChild::Linebreak) {
            rows.push(&children[start..i]);
            start = i + 1;
        }
    }
    rows.push(&children[start..]);
    rows
}

/// Split a row on `MathChild::Align`.
///
/// Always returns at least one column (possibly empty).
pub fn split_columns<'a, 'c>(
    row: &'c [MathChild<'a>],
) -> Vec<&'c [MathChild<'a>]> {
    let mut cols = Vec::new();
    let mut start = 0;
    for (i, child) in row.iter().enumerate() {
        if matches!(child, MathChild::Align) {
            cols.push(&row[start..i]);
            start = i + 1;
        }
    }
    cols.push(&row[start..]);
    cols
}

// ===========================================================================
// FencedView: shared structural analysis for \left ... \right groups
// ===========================================================================

/// A structural view over the body of a `\left ... \right` group.
///
/// Both layout targets (paged frames, MathML output) need the same
/// preliminary analysis on an LR body: detect opening/closing delimiters
/// at the ends, find `Mid` markers, and split into rows + columns. They
/// differ only in the rendering: paged measures body height and stretches
/// delimiter glyphs to match; MathML emits `stretchy="true"` and lets the
/// browser stretch. This view captures the shared structure.
///
/// The view does *not* perform delimiter stretching or weak-spacing
/// removal — those are decisions tied to the per-target rendering. It
/// just normalizes "what's a delimiter", "where are the mids", and "how
/// is the body laid out structurally".
#[derive(Debug)]
pub struct FencedView<'a, 'c> {
    /// The opening delimiter, if the first non-ignorant child looks like
    /// one (math class `Opening` or `Fence`). Empty if none was detected.
    pub open: Option<&'c MathChild<'a>>,
    /// The closing delimiter, if any.
    pub close: Option<&'c MathChild<'a>>,
    /// Indices (within the original `children` slice) of each `Mid`
    /// marker. These map to glyphs that should be sized to the same
    /// stretch as the outer delimiters.
    pub mid_positions: Vec<usize>,
    /// The body between the delimiters as a 2D grid (rows × columns).
    /// Empty rows are dropped; trailing linebreaks are trimmed (same
    /// rules as `MultilineView`).
    pub body: MultilineView<'a, 'c>,
}

impl<'a, 'c> FencedView<'a, 'c> {
    /// Build the structural view from a flat child sequence (the LR
    /// body's children, in source order).
    pub fn new(children: &'c [MathChild<'a>]) -> Self {
        // Skip ignorant prefix/suffix items when looking for delimiters.
        let mut start = 0;
        while start < children.len() && children[start].is_ignorant() {
            start += 1;
        }
        let mut end = children.len();
        while end > start && children[end - 1].is_ignorant() {
            end -= 1;
        }

        let mut body_start = start;
        let mut body_end = end;

        let open = children.get(start).filter(|c| is_delimiter(c));
        if open.is_some() {
            body_start += 1;
        }

        // Only check for closing if there's at least one item left.
        let close = if body_end > body_start {
            children.get(body_end - 1).filter(|c| is_delimiter(c))
        } else {
            None
        };
        if close.is_some() {
            body_end -= 1;
        }

        let body_slice = &children[body_start..body_end];

        // Locate `Mid` markers in the *original* slice (absolute indices).
        let mid_positions: Vec<usize> = children
            .iter()
            .enumerate()
            .filter_map(|(i, c)| matches!(c, MathChild::Mid(_)).then_some(i))
            .collect();

        Self {
            open,
            close,
            mid_positions,
            body: MultilineView::new(body_slice),
        }
    }

    /// Is this fence multiline? (Body contains linebreaks.)
    pub fn is_multiline(&self) -> bool {
        self.body.is_multiline
    }
}

/// Classify whether a child looks like a delimiter at the start or end
/// of an LR body.
fn is_delimiter(child: &MathChild<'_>) -> bool {
    matches!(child.class(), MathClass::Opening | MathClass::Closing | MathClass::Fence)
}
