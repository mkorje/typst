use unicode_math_class::MathClass;

use crate::diag::{bail, SourceResult};
use crate::engine::Engine;
use crate::foundations::{
    elem, Content, NativeElement, Packed, Show, ShowSet, Smart, StyleChain, Styles,
};
use crate::introspection::{Locatable, Locator};
use crate::layout::{Frame, InlineElem, InlineItem, Point, Size};
use crate::math::{scaled_font_size, LayoutMath, MathContext, MathSize, MathVariant};
use crate::model::ParElem;
use crate::syntax::Span;
use crate::text::{families, variant, Font, FontFamily, FontList, FontWeight, TextElem};
use crate::World;

/// An inline mathematical equation.
#[elem(Locatable, Show, ShowSet, LayoutMath)]
pub struct InlineEquationElem {
    /// The contents of the equation.
    #[required]
    pub body: Content,

    /// The size of the glyphs.
    #[internal]
    #[default(MathSize::Text)]
    #[ghost]
    pub size: MathSize,

    /// The style variant to select.
    #[internal]
    #[ghost]
    pub variant: MathVariant,

    /// Affects the height of exponents.
    #[internal]
    #[default(false)]
    #[ghost]
    pub cramped: bool,

    /// Whether to use bold glyphs.
    #[internal]
    #[default(false)]
    #[ghost]
    pub bold: bool,

    /// Whether to use italic glyphs.
    #[internal]
    #[ghost]
    pub italic: Smart<bool>,

    /// A forced class to use for all fragment.
    #[internal]
    #[ghost]
    pub class: Option<MathClass>,
}

impl Show for Packed<InlineEquationElem> {
    fn show(&self, _: &mut Engine, _: StyleChain) -> SourceResult<Content> {
        Ok(InlineElem::layouter(self.clone(), layout_equation_inline)
            .pack()
            .spanned(self.span()))
    }
}

impl ShowSet for Packed<InlineEquationElem> {
    fn show_set(&self, _: StyleChain) -> Styles {
        let mut out = Styles::new();
        out.set(InlineEquationElem::set_size(MathSize::Text));
        out.set(TextElem::set_weight(FontWeight::from_number(450)));
        out.set(TextElem::set_font(FontList(vec![FontFamily::new(
            "New Computer Modern Math",
        )])));
        out
    }
}

impl LayoutMath for Packed<InlineEquationElem> {
    #[typst_macros::time(name = "math.inline-equation", span = self.span())]
    fn layout_math(&self, ctx: &mut MathContext, styles: StyleChain) -> SourceResult<()> {
        self.body().layout_math(ctx, styles)
    }
}

/// Layout an inline equation (in a paragraph).
#[typst_macros::time(span = elem.span())]
fn layout_equation_inline(
    elem: &Packed<InlineEquationElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    region: Size,
) -> SourceResult<Vec<InlineItem>> {
    let font = find_math_font(engine, styles, elem.span())?;

    let mut ctx = MathContext::new(engine, locator, styles, region, &font);
    let run = ctx.layout_into_run(elem, styles)?;

    let mut items = if run.row_count() == 1 {
        run.into_par_items()
    } else {
        vec![InlineItem::Frame(run.into_fragment(&ctx, styles).into_frame())]
    };

    // An empty equation should have a height, so we still create a frame
    // (which is then resized in the loop).
    if items.is_empty() {
        items.push(InlineItem::Frame(Frame::soft(Size::zero())));
    }

    for item in &mut items {
        let InlineItem::Frame(frame) = item else { continue };

        let font_size = scaled_font_size(&ctx, styles);
        let slack = ParElem::leading_in(styles) * 0.7;
        let top_edge = TextElem::top_edge_in(styles).resolve(font_size, &font, None);
        let bottom_edge =
            -TextElem::bottom_edge_in(styles).resolve(font_size, &font, None);

        let ascent = top_edge.max(frame.ascent() - slack);
        let descent = bottom_edge.max(frame.descent() - slack);
        frame.translate(Point::with_y(ascent - frame.baseline()));
        frame.size_mut().y = ascent + descent;
    }

    Ok(items)
}

fn find_math_font(
    engine: &mut Engine<'_>,
    styles: StyleChain,
    span: Span,
) -> SourceResult<Font> {
    let variant = variant(styles);
    let world = engine.world;
    let Some(font) = families(styles).find_map(|family| {
        let id = world.book().select(family, variant)?;
        let font = world.font(id)?;
        let _ = font.ttf().tables().math?.constants?;
        Some(font)
    }) else {
        bail!(span, "current font does not support math");
    };
    Ok(font)
}
