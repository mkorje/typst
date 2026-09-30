use typst_library::diag::SourceResult;
use typst_library::engine::Engine;
use typst_library::foundations::{Packed, StyleChain};
use typst_library::introspection::Locator;
use typst_library::layout::{
    Abs, Frame, MultiState, MultiStep, PadElem, Point, Regions, Rel, Sides, Size,
};

/// Layout the padded content, one region at a time.
#[typst_macros::time(span = elem.span())]
pub fn layout_pad(
    elem: &Packed<PadElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    regions: Regions,
    state: Option<&MultiState>,
) -> SourceResult<MultiStep> {
    let padding = Sides::new(
        elem.left.resolve(styles),
        elem.top.resolve(styles),
        elem.right.resolve(styles),
        elem.bottom.resolve(styles),
    );

    let mut backlog = vec![];
    let pod = regions.map(&mut backlog, |size| shrink(size, &padding));

    // Layout child into padded regions.
    let mut step = crate::flow::layout_fragment_step(
        engine, &elem.body, locator, styles, pod, state,
    )?;
    grow(&mut step.frame, &padding);
    step.ahead = grow_ahead(step.ahead, &padding);

    Ok(step)
}

/// Grows the heights that a child laid out into the regions after the current
/// one (see [`MultiStep::ahead`]) by an inset, like [`grow`] grows the frames
/// around the child in these regions.
pub fn grow_ahead(ahead: Vec<Abs>, inset: &Sides<Rel<Abs>>) -> Vec<Abs> {
    let inset = inset.sum_by_axis().y;
    ahead.into_iter().map(|height| grown(height, inset)).collect()
}

/// Shrink a region size by an inset relative to the size itself.
pub fn shrink(size: Size, inset: &Sides<Rel<Abs>>) -> Size {
    size - inset.sum_by_axis().relative_to(size)
}

/// Shrink the components of possibly multiple `Regions` by an inset relative to
/// the regions themselves.
pub fn shrink_multiple(
    size: &mut Size,
    full: &mut Abs,
    backlog: &mut [Abs],
    last: &mut Option<Abs>,
    inset: &Sides<Rel<Abs>>,
) {
    let summed = inset.sum_by_axis();
    *size -= summed.relative_to(*size);
    *full -= summed.y.relative_to(*full);
    for item in backlog {
        *item -= summed.y.relative_to(*item);
    }
    *last = last.map(|v| v - summed.y.relative_to(v));
}

/// Grows a length by an inset relative to the grown length. See [`grow`].
fn grown(length: Abs, inset: Rel<Abs>) -> Abs {
    (length + inset.abs) / (1.0 - inset.rel.get())
}

/// Grow a frame's size by an inset relative to the grown size.
/// This is the inverse operation to `shrink()`.
///
/// For the horizontal axis the derivation looks as follows.
/// (Vertical axis is analogous.)
///
/// Let w be the grown target width,
///     s be the given width,
///     l be the left inset,
///     r be the right inset,
///     p = l + r.
///
/// We want that: w - l.resolve(w) - r.resolve(w) = s
///
/// Thus: w - l.resolve(w) - r.resolve(w) = s
///   <=> w - p.resolve(w) = s
///   <=> w - p.rel * w - p.abs = s
///   <=> (1 - p.rel) * w = s + p.abs
///   <=> w = (s + p.abs) / (1 - p.rel)
pub fn grow(frame: &mut Frame, inset: &Sides<Rel<Abs>>) {
    // Apply the padding inversely such that the grown size padded
    // yields the frame's size.
    let padded = frame.size().zip_map(inset.sum_by_axis(), grown);

    let inset = inset.relative_to(padded);
    let offset = Point::new(inset.left, inset.top);

    // Grow the frame and translate everything in the frame inwards.
    frame.set_size(padded);
    frame.translate(offset);
}

#[cfg(test)]
mod tests {
    use typst_library::layout::Ratio;

    use super::*;

    #[test]
    fn test_grow_ahead_like_frames() {
        // 5% padding at the top and bottom: A child that uses 180pt of a
        // future region grows to 200pt there, whatever the current region.
        let inset = Sides::new(
            Rel::zero(),
            Rel::new(Ratio::new(0.05), Abs::zero()),
            Rel::zero(),
            Rel::new(Ratio::new(0.05), Abs::pt(1.0)),
        );
        let ahead = grow_ahead(vec![Abs::pt(179.0)], &inset);
        let mut frame = Frame::soft(Size::new(Abs::pt(10.0), Abs::pt(179.0)));
        grow(&mut frame, &inset);
        assert_eq!(ahead, [frame.height()]);
        assert!(ahead[0].approx_eq(Abs::pt(200.0)));
    }
}
