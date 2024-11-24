use rustybuzz::Feature;
use ttf_parser::Tag;
use typst_library::diag::SourceResult;
use typst_library::engine::Engine;
use typst_library::foundations::{Content, Packed, StyleChain};
use typst_library::introspection::{SplitLocator, TagElem};
use typst_library::layout::{
    Axes, BoxElem, Em, Frame, HElem, PlaceElem, Region, Size, Spacing,
};
use typst_library::math::*;
use typst_library::routines::{Arenas, RealizationKind};
use typst_library::text::{features, Font, LinebreakElem, SpaceElem, TextElem, TextSize};
use unicode_math_class::MathClass;

use super::{
    scaled_font_size, FrameFragment, GlyphwiseSubsts, Limits, MathFragment, MathRun,
};

/// The context for math layout.
pub struct MathContext<'a, 'v, 'e> {
    // External.
    pub engine: &'v mut Engine<'e>,
    pub locator: &'v mut SplitLocator<'a>,
    pub region: Region,
    // Font-related.
    pub font: &'a Font,
    pub ttf: &'a ttf_parser::Face<'a>,
    pub table: ttf_parser::math::Table<'a>,
    pub constants: ttf_parser::math::Constants<'a>,
    pub dtls_table: Option<GlyphwiseSubsts<'a>>,
    pub flac_table: Option<GlyphwiseSubsts<'a>>,
    pub ssty_table: Option<GlyphwiseSubsts<'a>>,
    pub glyphwise_tables: Option<Vec<GlyphwiseSubsts<'a>>>,
    pub space_width: Em,
    // Realized content.
    pub content: Vec<Content>,
    // Mutable.
    pub fragments: Vec<MathFragment>,
}

impl<'a, 'v, 'e> MathContext<'a, 'v, 'e> {
    /// Create a new math context.
    pub fn new(
        engine: &'v mut Engine<'e>,
        locator: &'v mut SplitLocator<'a>,
        styles: StyleChain<'a>,
        base: Size,
        font: &'a Font,
    ) -> Self {
        let math_table = font.ttf().tables().math.unwrap();
        let gsub_table = font.ttf().tables().gsub;
        let constants = math_table.constants.unwrap();

        let feat = |tag: &[u8; 4]| {
            GlyphwiseSubsts::new(gsub_table, Feature::new(Tag::from_bytes(tag), 0, ..))
        };

        let features = features(styles);
        let glyphwise_tables = Some(
            features
                .into_iter()
                .filter_map(|feature| GlyphwiseSubsts::new(gsub_table, feature))
                .collect(),
        );

        let ttf = font.ttf();
        let space_width = ttf
            .glyph_index(' ')
            .and_then(|id| ttf.glyph_hor_advance(id))
            .map(|advance| font.to_em(advance))
            .unwrap_or(THICK);

        Self {
            engine,
            locator,
            region: Region::new(base, Axes::splat(false)),
            font,
            ttf,
            table: math_table,
            constants,
            dtls_table: feat(b"dtls"),
            flac_table: feat(b"flac"),
            ssty_table: feat(b"ssty"),
            glyphwise_tables,
            space_width,
            content: vec![],
            fragments: vec![],
        }
    }

    /// Push a fragment.
    pub fn push(&mut self, fragment: impl Into<MathFragment>) {
        self.fragments.push(fragment.into());
    }

    /// Push multiple fragments.
    pub fn extend(&mut self, fragments: impl IntoIterator<Item = MathFragment>) {
        self.fragments.extend(fragments);
    }

    /// Layout the given element and return the result as a [`MathRun`].
    pub fn layout_into_run(
        &mut self,
        elem: &Content,
        styles: StyleChain,
    ) -> SourceResult<MathRun> {
        Ok(MathRun::new(self.layout_into_fragments(elem, styles)?))
    }

    /// Layout the given element and return the resulting [`MathFragment`]s.
    pub fn layout_into_fragments(
        &mut self,
        elem: &Content,
        styles: StyleChain,
    ) -> SourceResult<Vec<MathFragment>> {
        // The element's layout_math() changes the fragments held in this
        // MathContext object, but for convenience this function shouldn't change
        // them, so we restore the MathContext's fragments after obtaining the
        // layout result.
        let prev_content = std::mem::take(&mut self.content);
        let prev_fragments = std::mem::take(&mut self.fragments);
        self.layout_into_self(elem, styles)?;
        let _ = std::mem::replace(&mut self.content, prev_content);
        Ok(std::mem::replace(&mut self.fragments, prev_fragments))
    }

    /// Layout the given element and return the result as a
    /// unified [`MathFragment`].
    pub fn layout_into_fragment(
        &mut self,
        elem: &Content,
        styles: StyleChain,
    ) -> SourceResult<MathFragment> {
        Ok(self.layout_into_run(elem, styles)?.into_fragment(self, styles))
    }

    /// Layout the given element and return the result as a [`Frame`].
    pub fn layout_into_frame(
        &mut self,
        elem: &Content,
        styles: StyleChain,
    ) -> SourceResult<Frame> {
        Ok(self.layout_into_fragment(elem, styles)?.into_frame())
    }

    /// Layout arbitrary content.
    pub fn layout_into_self(
        &mut self,
        content: &Content,
        styles: StyleChain,
    ) -> SourceResult<()> {
        let arenas = Arenas::default();
        let pairs = (self.engine.routines.realize)(
            RealizationKind::Math,
            self.engine,
            self.locator,
            &arenas,
            content,
            styles,
        )?;

        // let rows: Vec<_> = pairs
        //     .clone()
        //     .into_iter()
        //     .map(|(elem, _)| elem)
        //     .collect::<Vec<_>>()
        //     .split_inclusive(|elem| elem.is::<LinebreakElem>() || elem.is::<LineLabelElem>())
        //     .map(|slice| slice.to_vec())
        //     .collect();
        // println!("{:?}\n", rows);
        // for (elem, _) in &pairs {
        //     println!("\t{:?}", elem);
        // }
        let outer = styles;
        for (elem, styles) in pairs {
            self.content.push(elem.clone());

            // Hack because the font is fixed in math.
            if styles != outer && TextElem::font_in(styles) != TextElem::font_in(outer) {
                let frame = layout_external(elem, self, styles)?;
                self.push(FrameFragment::new(self, styles, frame).with_spaced(true));
                continue;
            }

            layout_realized(elem, self, styles)?;
        }

        Ok(())
    }
}

/// Lays out a leaf element resulting from realization.
fn layout_realized(
    elem: &Content,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    if let Some(elem) = elem.to_packed::<TagElem>() {
        ctx.push(MathFragment::Tag(elem.tag.clone()));
    } else if elem.is::<SpaceElem>() {
        let font_size = scaled_font_size(ctx, styles);
        ctx.push(MathFragment::Space(ctx.space_width.at(font_size)));
    } else if elem.is::<LinebreakElem>() {
        ctx.push(MathFragment::Linebreak(None));
    } else if let Some(elem) = elem.to_packed::<HElem>() {
        layout_h(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<TextElem>() {
        super::text::layout_text(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<BoxElem>() {
        layout_box(elem, ctx, styles)?;
    } else if elem.is::<AlignPointElem>() {
        ctx.push(MathFragment::Align);
    } else if let Some(elem) = elem.to_packed::<LineLabelElem>() {
        ctx.push(MathFragment::Linebreak(Some(*elem.value())));
    } else if let Some(elem) = elem.to_packed::<ClassElem>() {
        layout_class(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<AccentElem>() {
        super::accent::layout_accent(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<AttachElem>() {
        super::attach::layout_attach(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<PrimesElem>() {
        super::attach::layout_primes(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<ScriptsElem>() {
        super::attach::layout_scripts(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<LimitsElem>() {
        super::attach::layout_limits(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<CancelElem>() {
        super::cancel::layout_cancel(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<FracElem>() {
        super::frac::layout_frac(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<BinomElem>() {
        super::frac::layout_binom(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<LrElem>() {
        super::lr::layout_lr(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<MidElem>() {
        super::lr::layout_mid(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<VecElem>() {
        super::mat::layout_vec(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<MatElem>() {
        super::mat::layout_mat(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<CasesElem>() {
        super::mat::layout_cases(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<OpElem>() {
        layout_op(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<RootElem>() {
        super::root::layout_root(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<StretchElem>() {
        super::stretch::layout_stretch(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<UnderlineElem>() {
        super::underover::layout_underline(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<OverlineElem>() {
        super::underover::layout_overline(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<UnderbraceElem>() {
        super::underover::layout_underbrace(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<OverbraceElem>() {
        super::underover::layout_overbrace(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<UnderbracketElem>() {
        super::underover::layout_underbracket(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<OverbracketElem>() {
        super::underover::layout_overbracket(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<UnderparenElem>() {
        super::underover::layout_underparen(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<OverparenElem>() {
        super::underover::layout_overparen(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<UndershellElem>() {
        super::underover::layout_undershell(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<OvershellElem>() {
        super::underover::layout_overshell(elem, ctx, styles)?
    } else {
        let mut frame = layout_external(elem, ctx, styles)?;
        if !frame.has_baseline() {
            let axis = scaled!(ctx, styles, axis_height);
            frame.set_baseline(frame.height() / 2.0 + axis);
        }
        ctx.push(
            FrameFragment::new(ctx, styles, frame)
                .with_spaced(true)
                .with_ignorant(elem.is::<PlaceElem>()),
        );
    }

    Ok(())
}

/// Lays out an [`BoxElem`].
fn layout_box(
    elem: &Packed<BoxElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    let local = TextElem::set_size(TextSize(scaled_font_size(ctx, styles).into())).wrap();
    let frame = (ctx.engine.routines.layout_box)(
        elem,
        ctx.engine,
        ctx.locator.next(&elem.span()),
        styles.chain(&local),
        ctx.region.size,
    )?;
    ctx.push(FrameFragment::new(ctx, styles, frame).with_spaced(true));
    Ok(())
}

/// Lays out an [`HElem`].
fn layout_h(
    elem: &Packed<HElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    if let Spacing::Rel(rel) = elem.amount() {
        if rel.rel.is_zero() {
            ctx.push(MathFragment::Spacing(
                rel.abs.at(scaled_font_size(ctx, styles)),
                elem.weak(styles),
            ));
        }
    }
    Ok(())
}

/// Lays out a [`ClassElem`].
#[typst_macros::time(name = "math.op", span = elem.span())]
fn layout_class(
    elem: &Packed<ClassElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    let class = *elem.class();
    let style = EquationElem::set_class(Some(class)).wrap();
    let mut fragment = ctx.layout_into_fragment(elem.body(), styles.chain(&style))?;
    fragment.set_class(class);
    fragment.set_limits(Limits::for_class(class));
    ctx.push(fragment);
    Ok(())
}

/// Lays out an [`OpElem`].
#[typst_macros::time(name = "math.op", span = elem.span())]
fn layout_op(
    elem: &Packed<OpElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    let fragment = ctx.layout_into_fragment(elem.text(), styles)?;
    let italics = fragment.italics_correction();
    let accent_attach = fragment.accent_attach();
    let text_like = fragment.is_text_like();

    ctx.push(
        FrameFragment::new(ctx, styles, fragment.into_frame())
            .with_class(MathClass::Large)
            .with_italics_correction(italics)
            .with_accent_attach(accent_attach)
            .with_text_like(text_like)
            .with_limits(if elem.limits(styles) {
                Limits::Display
            } else {
                Limits::Never
            }),
    );
    Ok(())
}

/// Layout into a frame with normal layout.
fn layout_external(
    content: &Content,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<Frame> {
    let local = TextElem::set_size(TextSize(scaled_font_size(ctx, styles).into())).wrap();
    (ctx.engine.routines.layout_frame)(
        ctx.engine,
        content,
        ctx.locator.next(&content.span()),
        styles.chain(&local),
        ctx.region,
    )
}
