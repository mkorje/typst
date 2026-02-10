use typst_library::foundations::{Resolve, StyleChain};
use typst_library::layout::{Abs, Em, Frame, InlineItem, Point, Size};
use typst_library::math::{EquationElem, LeftRightAlternator, MathSize};
use typst_library::model::ParElem;
use unicode_math_class::MathClass;

use super::fragment::MathFragment;

/// Leading between rows in script and scriptscript size.
const TIGHT_LEADING: Em = Em::new(0.25);

/// Compute the leading (vertical spacing) between rows based on math size.
pub(super) fn math_leading(styles: StyleChain) -> Abs {
    if styles.get(EquationElem::size) >= MathSize::Text {
        styles.resolve(ParElem::leading)
    } else {
        TIGHT_LEADING.resolve(styles)
    }
}

/// Compute cumulative alignment points from column widths.
///
/// Returns `(points, total_width)`. When there are fewer than 2 columns,
/// `points` is empty and `total_width` is the width of the single column
/// (or zero).
pub(super) fn cumulative_alignment_points(widths: &[Abs]) -> (Vec<Abs>, Abs) {
    if widths.len() <= 1 {
        return (Vec::new(), widths.first().copied().unwrap_or_default());
    }
    let mut points = Vec::with_capacity(widths.len());
    let mut cumulative = Abs::zero();
    for &w in widths {
        cumulative += w;
        points.push(cumulative);
    }
    (points, cumulative)
}

pub trait MathFragmentsExt {
    fn into_frame(self) -> Frame;
    fn into_par_items(self) -> Vec<InlineItem>;
}

impl MathFragmentsExt for Vec<MathFragment> {
    /// Lay out [`MathFragment`]s into a one-row [`Frame`] sequentially.
    fn into_frame(self) -> Frame {
        sub_columns_into_line_frame(vec![self], &[], LeftRightAlternator::Right)
    }

    /// Convert this run of math fragments into a vector of inline items for
    /// paragraph layout. Creates multiple fragments when relation or binary
    /// operators are present to allow for line-breaking opportunities later.
    fn into_par_items(self) -> Vec<InlineItem> {
        let mut items = vec![];

        let mut x = Abs::zero();
        let mut ascent = Abs::zero();
        let mut descent = Abs::zero();
        let mut frame = Frame::soft(Size::zero());
        let mut empty = true;

        let finalize_frame = |frame: &mut Frame, x, ascent, descent| {
            frame.set_size(Size::new(x, ascent + descent));
            frame.set_baseline(Abs::zero());
            frame.translate(Point::with_y(ascent));
        };

        let mut space_is_visible = false;

        let is_space = |f: &MathFragment| matches!(f, MathFragment::Space(_));
        let is_line_break_opportunity = |class, next_fragment| match class {
            // Don't split when two relations are in a row or when preceding a
            // closing parenthesis.
            MathClass::Binary => next_fragment != Some(MathClass::Closing),
            MathClass::Relation => {
                !matches!(next_fragment, Some(MathClass::Relation | MathClass::Closing))
            }
            _ => false,
        };

        let mut iter = self.into_iter().peekable();
        while let Some(fragment) = iter.next() {
            if space_is_visible && is_space(&fragment) {
                items.push(InlineItem::Space(fragment.width(), true));
                continue;
            }

            let class = fragment.class();
            let y = fragment.ascent();

            ascent.set_max(y);
            descent.set_max(fragment.descent());

            let pos = Point::new(x, -y);
            x += fragment.width();
            frame.push_frame(pos, fragment.into_frame());
            empty = false;

            // Split our current frame when we encounter a binary operator or
            // relation so that there is a line-breaking opportunity.
            if is_line_break_opportunity(class, iter.peek().map(|f| f.class())) {
                let mut frame_prev =
                    std::mem::replace(&mut frame, Frame::soft(Size::zero()));

                finalize_frame(&mut frame_prev, x, ascent, descent);
                items.push(InlineItem::Frame(frame_prev));
                empty = true;

                x = Abs::zero();
                ascent = Abs::zero();
                descent = Abs::zero();

                space_is_visible = true;
                if let Some(f_next) = iter.peek()
                    && !is_space(f_next)
                {
                    items.push(InlineItem::Space(Abs::zero(), true));
                }
            } else {
                space_is_visible = false;
            }
        }

        // Don't use `frame.is_empty()` because even an empty frame can
        // contribute width (if it had hidden content).
        if !empty {
            finalize_frame(&mut frame, x, ascent, descent);
            items.push(InlineItem::Frame(frame));
        }

        items
    }
}

/// How the rows from the [`MathRun`] should be aligned and merged into a [`Frame`].
pub struct MathRunFrameBuilder {
    /// The size of the resulting frame.
    pub size: Size,
    /// Each row's frame, and the position where the frame should
    /// be pushed into the resulting frame.
    pub frames: Vec<(Frame, Point)>,
}

impl MathRunFrameBuilder {
    /// Consumes the builder and returns a [`Frame`].
    pub fn build(self) -> Frame {
        let mut frame = Frame::soft(self.size);
        for (sub, pos) in self.frames.into_iter() {
            frame.push_frame(pos, sub);
        }
        frame
    }
}

/// Build a frame from sub-column fragments positioned at alignment points.
///
/// Each sub-column's fragments are positioned using left/right alternation
/// relative to cumulative alignment points. With empty `points` and a single
/// sub-column, this reduces to simple sequential layout.
pub fn sub_columns_into_line_frame(
    sub_cols: Vec<Vec<MathFragment>>,
    points: &[Abs],
    mut alternator: LeftRightAlternator,
) -> Frame {
    let (ascent, descent) = sub_cols
        .iter()
        .flat_map(|sc| sc.iter())
        .filter(|f| !matches!(f, MathFragment::Tag(_)))
        .map(|f| (f.ascent(), f.descent()))
        .reduce(|(a1, d1), (a2, d2)| (a1.max(a2), d1.max(d2)))
        .unwrap_or_default();

    let mut frame = Frame::soft(Size::new(Abs::zero(), ascent + descent));
    frame.set_baseline(ascent);

    let mut prev_point = Abs::zero();
    let mut point_iter = points.iter().copied();
    let mut x_end = Abs::zero();

    for sc in sub_cols {
        let sc_w: Abs = sc.iter().map(|f| f.width()).sum();

        let cell_x = if let Some(point) = point_iter.next()
            && let Some(alt) = alternator.next()
        {
            let x = match alt {
                LeftRightAlternator::Right => point - sc_w,
                _ => prev_point,
            };
            prev_point = point;
            x
        } else {
            prev_point
        };

        let mut x = cell_x;
        for frag in sc {
            let y = ascent - frag.ascent();
            let w = frag.width();
            frame.push_frame(Point::new(x, y), frag.into_frame());
            x += w;
        }
        x_end = x;
    }

    frame.size_mut().x = x_end;
    frame
}
