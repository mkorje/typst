use crate::diag::SourceResult;
use crate::foundations::{elem, func, Content, NativeElement, Packed, StyleChain};
use crate::layout::{Abs, Em, FixedAlignment, Frame, FrameItem, Point, Size};
use crate::math::{
    alignments, scaled_font_size, style_cramped, style_for_subscript,
    style_for_superscript, AlignmentResult, FrameFragment, GlyphFragment, LayoutMath,
    LeftRightAlternator, MathContext, MathRun, Scaled,
};
use crate::syntax::Span;
use crate::text::TextElem;
use crate::visualize::{FixedStroke, Geometry};

const GAP: Em = Em::new(0.25);

/// A marker to distinguish under- vs. over-.
enum Position {
    Under,
    Over,
}

/// A horizontal line under content.
///
/// ```example
/// $ underline(1 + 2 + ... + 5) $
/// ```
#[elem(LayoutMath)]
pub struct UnderlineElem {
    /// The content above the line.
    #[required]
    pub body: Content,
}

impl LayoutMath for Packed<UnderlineElem> {
    #[typst_macros::time(name = "math.underline", span = self.span())]
    fn layout_math(&self, ctx: &mut MathContext, styles: StyleChain) -> SourceResult<()> {
        layout_underoverline(ctx, styles, self.body(), self.span(), Position::Under)
    }
}

/// A horizontal line over content.
///
/// ```example
/// $ overline(1 + 2 + ... + 5) $
/// ```
#[elem(LayoutMath)]
pub struct OverlineElem {
    /// The content below the line.
    #[required]
    pub body: Content,
}

impl LayoutMath for Packed<OverlineElem> {
    #[typst_macros::time(name = "math.overline", span = self.span())]
    fn layout_math(&self, ctx: &mut MathContext, styles: StyleChain) -> SourceResult<()> {
        layout_underoverline(ctx, styles, self.body(), self.span(), Position::Over)
    }
}

/// layout under- or overlined content
fn layout_underoverline(
    ctx: &mut MathContext,
    styles: StyleChain,
    body: &Content,
    span: Span,
    position: Position,
) -> SourceResult<()> {
    let (extra_height, content, line_pos, content_pos, baseline, bar_height);
    match position {
        Position::Under => {
            let sep = scaled!(ctx, styles, underbar_extra_descender);
            bar_height = scaled!(ctx, styles, underbar_rule_thickness);
            let gap = scaled!(ctx, styles, underbar_vertical_gap);
            extra_height = sep + bar_height + gap;

            content = ctx.layout_into_fragment(body, styles)?;

            line_pos = Point::with_y(content.height() + gap + bar_height / 2.0);
            content_pos = Point::zero();
            baseline = content.ascent()
        }
        Position::Over => {
            let sep = scaled!(ctx, styles, overbar_extra_ascender);
            bar_height = scaled!(ctx, styles, overbar_rule_thickness);
            let gap = scaled!(ctx, styles, overbar_vertical_gap);
            extra_height = sep + bar_height + gap;

            let cramped = style_cramped();
            content = ctx.layout_into_fragment(body, styles.chain(&cramped))?;

            line_pos = Point::with_y(sep + bar_height / 2.0);
            content_pos = Point::with_y(extra_height);
            baseline = content.ascent() + extra_height;
        }
    }

    let width = content.width();
    let height = content.height() + extra_height;
    let size = Size::new(width, height);

    let content_class = content.class();
    let mut frame = Frame::soft(size);
    frame.set_baseline(baseline);
    frame.push_frame(content_pos, content.into_frame());
    frame.push(
        line_pos,
        FrameItem::Shape(
            Geometry::Line(Point::with_x(width)).stroked(FixedStroke {
                paint: TextElem::fill_in(styles).as_decoration(),
                thickness: bar_height,
                ..FixedStroke::default()
            }),
            span,
        ),
    );

    ctx.push(FrameFragment::new(ctx, styles, frame).with_class(content_class));

    Ok(())
}

///
#[elem(LayoutMath)]
pub struct UnderElem {
    ///
    #[required]
    pub body: Content,

    ///
    #[required]
    pub delimiter: char,

    ///
    #[positional]
    pub annotation: Option<Content>,
}

impl LayoutMath for Packed<UnderElem> {
    #[typst_macros::time(name = "math.under", span = self.span())]
    fn layout_math(&self, ctx: &mut MathContext, styles: StyleChain) -> SourceResult<()> {
        layout_underoverspreader(
            ctx,
            styles,
            self.body(),
            *self.delimiter(),
            &self.annotation(styles),
            Position::Under,
            self.span(),
        )
    }
}

///
#[elem(LayoutMath)]
pub struct OverElem {
    ///
    #[required]
    pub body: Content,

    ///
    #[required]
    pub delimiter: char,

    ///
    #[positional]
    pub annotation: Option<Content>,
}

impl LayoutMath for Packed<OverElem> {
    #[typst_macros::time(name = "math.over", span = self.span())]
    fn layout_math(&self, ctx: &mut MathContext, styles: StyleChain) -> SourceResult<()> {
        layout_underoverspreader(
            ctx,
            styles,
            self.body(),
            *self.delimiter(),
            &self.annotation(styles),
            Position::Over,
            self.span(),
        )
    }
}

/// Layout an over- or underbrace-like object.
#[allow(clippy::too_many_arguments)]
fn layout_underoverspreader(
    ctx: &mut MathContext,
    styles: StyleChain,
    body: &Content,
    delimiter: char,
    annotation: &Option<Content>,
    position: Position,
    span: Span,
) -> SourceResult<()> {
    let font_size = scaled_font_size(ctx, styles);
    let gap = GAP.at(font_size);
    let body = ctx.layout_into_run(body, styles)?;
    let body_class = body.class();
    let body = body.into_fragment(ctx, styles);
    let glyph = GlyphFragment::new(ctx, styles, delimiter, span);
    let stretched = glyph.stretch_horizontal(ctx, body.width(), Abs::zero());

    let mut rows = vec![MathRun::new(vec![body]), stretched.into()];

    let (sub_style, super_style);
    let row_styles = match position {
        Position::Under => {
            sub_style = style_for_subscript(styles);
            styles.chain(&sub_style)
        }
        Position::Over => {
            super_style = style_for_superscript(styles);
            styles.chain(&super_style)
        }
    };

    rows.extend(
        annotation
            .as_ref()
            .map(|annotation| ctx.layout_into_run(annotation, row_styles))
            .transpose()?,
    );

    let baseline = match position {
        Position::Under => 0,
        Position::Over => {
            rows.reverse();
            rows.len() - 1
        }
    };

    let frame = stack(
        rows,
        FixedAlignment::Center,
        gap,
        baseline,
        LeftRightAlternator::Right,
        None,
    );
    ctx.push(FrameFragment::new(ctx, styles, frame).with_class(body_class));

    Ok(())
}

/// Stack rows on top of each other.
///
/// Add a `gap` between each row and uses the baseline of the `baseline`-th
/// row for the whole frame. `alternator` controls the left/right alternating
/// alignment behavior of `AlignPointElem` in the rows.
pub(super) fn stack(
    rows: Vec<MathRun>,
    align: FixedAlignment,
    gap: Abs,
    baseline: usize,
    alternator: LeftRightAlternator,
    minimum_ascent_descent: Option<(Abs, Abs)>,
) -> Frame {
    let rows: Vec<_> = rows.into_iter().flat_map(|r| r.rows()).collect();
    let AlignmentResult { points, width } = alignments(&rows);
    let rows: Vec<_> = rows
        .into_iter()
        .map(|row| row.into_line_frame(&points, alternator))
        .collect();

    let padded_height = |height: Abs| {
        height.max(minimum_ascent_descent.map_or(Abs::zero(), |(a, d)| a + d))
    };

    let mut frame = Frame::soft(Size::new(
        width,
        rows.iter().map(|row| padded_height(row.height())).sum::<Abs>()
            + rows.len().saturating_sub(1) as f64 * gap,
    ));

    let mut y = Abs::zero();
    for (i, row) in rows.into_iter().enumerate() {
        let x = align.position(width - row.width());
        let ascent_padded_part = minimum_ascent_descent
            .map_or(Abs::zero(), |(a, _)| (a - row.ascent()))
            .max(Abs::zero());
        let pos = Point::new(x, y + ascent_padded_part);
        if i == baseline {
            frame.set_baseline(y + row.baseline() + ascent_padded_part);
        }
        y += padded_height(row.height()) + gap;
        frame.push_frame(pos, row);
    }

    frame
}

/// A horizontal brace under content, with an optional annotation below.
///
/// ```example
/// $ underbrace(1 + 2 + ... + 5, "numbers") $
/// ```
#[func]
pub fn underbrace(
    /// The content above the brace.
    body: Content,
    /// The optional content below the brace.
    #[default]
    annotation: Option<Content>,
) -> Content {
    under(body, '⏟', annotation)
}

/// A horizontal brace over content, with an optional annotation above.
///
/// ```example
/// $ overbrace(1 + 2 + ... + 5, "numbers") $
/// ```
#[func]
pub fn overbrace(
    /// The content below the brace.
    body: Content,
    /// The optional content above the brace.
    #[default]
    annotation: Option<Content>,
) -> Content {
    over(body, '⏞', annotation)
}

/// A horizontal bracket under content, with an optional annotation below.
///
/// ```example
/// $ underbracket(1 + 2 + ... + 5, "numbers") $
/// ```
#[func]
pub fn underbracket(
    /// The content above the bracket.
    body: Content,
    /// The optional content below the bracket.
    #[default]
    annotation: Option<Content>,
) -> Content {
    under(body, '⎵', annotation)
}

/// A horizontal bracket over content, with an optional annotation above.
///
/// ```example
/// $ overbracket(1 + 2 + ... + 5, "numbers") $
/// ```
#[func]
pub fn overbracket(
    /// The content below the bracket.
    body: Content,
    /// The optional content above the bracket.
    #[default]
    annotation: Option<Content>,
) -> Content {
    over(body, '⎴', annotation)
}

/// A horizontal parenthesis under content, with an optional annotation below.
///
/// ```example
/// $ underparen(1 + 2 + ... + 5, "numbers") $
/// ```
#[func]
pub fn underparen(
    /// The content above the parenthesis.
    body: Content,
    /// The optional content below the parenthesis.
    #[default]
    annotation: Option<Content>,
) -> Content {
    under(body, '⏝', annotation)
}

/// A horizontal parenthesis over content, with an optional annotation above.
///
/// ```example
/// $ overparen(1 + 2 + ... + 5, "numbers") $
/// ```
#[func]
pub fn overparen(
    /// The content below the parenthesis.
    body: Content,
    /// The optional content above the parenthesis.
    #[default]
    annotation: Option<Content>,
) -> Content {
    over(body, '⏜', annotation)
}

/// A horizontal tortoise shell bracket under content, with an optional annotation below.
///
/// ```example
/// $ undershell(1 + 2 + ... + 5, "numbers") $
/// ```
#[func]
pub fn undershell(
    /// The content above the tortoise shell bracket.
    body: Content,
    /// The optional content below the tortoise shell bracket.
    #[default]
    annotation: Option<Content>,
) -> Content {
    under(body, '⏡', annotation)
}

/// A horizontal tortoise shell bracket over content, with an optional annotation above.
///
/// ```example
/// $ overshell(1 + 2 + ... + 5, "numbers") $
/// ```
#[func]
pub fn overshell(
    /// The content below the tortoise shell bracket.
    body: Content,
    /// The optional content above the tortoise shell bracket.
    #[default]
    annotation: Option<Content>,
) -> Content {
    over(body, '⏠', annotation)
}

fn under(
    body: Content,
    c: char,
    annotation: Option<Content>,
) -> Content {
    let span = body.span();
    let mut elem = UnderElem::new(body, c);
    elem.push_annotation(annotation);
    elem.pack().spanned(span)
}

fn over(
    body: Content,
    c: char,
    annotation: Option<Content>,
) -> Content {
    let span = body.span();
    let mut elem = OverElem::new(body, c);
    elem.push_annotation(annotation);
    elem.pack().spanned(span)
}
