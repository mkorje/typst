#[macro_use]
mod shared;
mod accent;
mod attach;
mod cancel;
mod frac;
mod fragment;
mod lr;
mod mat;
mod root;
mod run;
mod stretch;
mod text;
mod underover;

use ttf_parser::gsub::{SingleSubstitution, SubstitutionSubtable};
use ttf_parser::opentype_layout::LookupIndex;
use ttf_parser::{GlyphId, Rect, Tag};
use typst_library::diag::SourceResult;
use typst_library::engine::Engine;
use typst_library::foundations::{Content, NativeElement, Packed, Resolve, StyleChain};
use typst_library::introspection::{Counter, Locator, SplitLocator, TagElem};
use typst_library::layout::{
    Abs, AlignElem, Axes, BlockElem, BoxElem, Em, FixedAlignment, Fragment, Frame, HElem,
    InlineItem, OuterHAlignment, PlaceElem, Point, Region, Regions, Size, Spacing,
    SpecificAlignment, VAlignment,
};
use typst_library::math::*;
use typst_library::model::ParElem;
use typst_library::routines::{Arenas, RealizationKind};
use typst_library::text::{
    families, features, variant, Font, LinebreakElem, SpaceElem, TextEdgeBounds, TextElem,
};
use typst_library::World;
use typst_utils::Numeric;
use unicode_math_class::MathClass;

use self::fragment::{
    FrameFragment, GlyphFragment, Limits, MathFragment, VariantFragment,
};
use self::run::{LeftRightAlternator, MathRun, MathRunFrameBuilder};
use self::shared::*;
use self::stretch::{stretch_fragment, stretch_glyph};

/// Layout an inline equation (in a paragraph).
#[typst_macros::time(span = elem.span())]
pub fn layout_equation_inline(
    elem: &Packed<EquationElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    region: Size,
) -> SourceResult<Vec<InlineItem>> {
    assert!(!elem.block(styles));

    let mut locator = locator.split();
    let mut ctx = MathContext::new(engine, &mut locator, styles, region);

    let scale_style = style_for_script_scale(&ctx);
    let styles = styles.chain(&scale_style);

    let run = ctx.layout_into_run(&elem.body, styles)?;

    let mut items = if run.row_count() == 1 {
        run.into_par_items()
    } else {
        vec![InlineItem::Frame(run.into_fragment(styles).into_frame())]
    };

    // An empty equation should have a height, so we still create a frame
    // (which is then resized in the loop).
    if items.is_empty() {
        items.push(InlineItem::Frame(Frame::soft(Size::zero())));
    }

    for item in &mut items {
        let InlineItem::Frame(frame) = item else { continue };

        let slack = ParElem::leading_in(styles) * 0.7;

        let (t, b) = ctx.font.edges(
            TextElem::top_edge_in(styles),
            TextElem::bottom_edge_in(styles),
            TextElem::size_in(styles),
            TextEdgeBounds::Frame(frame),
        );

        let ascent = t.max(frame.ascent() - slack);
        let descent = b.max(frame.descent() - slack);
        frame.translate(Point::with_y(ascent - frame.baseline()));
        frame.size_mut().y = ascent + descent;
    }

    Ok(items)
}

/// Layout a block-level equation (in a flow).
#[typst_macros::time(span = elem.span())]
pub fn layout_equation_block(
    elem: &Packed<EquationElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    regions: Regions,
) -> SourceResult<Fragment> {
    assert!(elem.block(styles));

    let span = elem.span();

    let mut locator = locator.split();
    let mut ctx = MathContext::new(engine, &mut locator, styles, regions.base());

    let scale_style = style_for_script_scale(&ctx);
    let styles = styles.chain(&scale_style);

    let full_equation_builder = ctx
        .layout_into_run(&elem.body, styles)?
        .multiline_frame_builder(styles);
    let width = full_equation_builder.size.x;

    let equation_builders = if BlockElem::breakable_in(styles) {
        let mut rows = full_equation_builder.frames.into_iter().peekable();
        let mut equation_builders = vec![];
        let mut last_first_pos = Point::zero();
        let mut regions = regions;

        loop {
            // Keep track of the position of the first row in this region,
            // so that the offset can be reverted later.
            let Some(&(_, first_pos)) = rows.peek() else { break };
            last_first_pos = first_pos;

            let mut frames = vec![];
            let mut height = Abs::zero();
            while let Some((sub, pos)) = rows.peek() {
                let mut pos = *pos;
                pos.y -= first_pos.y;

                // Finish this region if the line doesn't fit. Only do it if
                // we placed at least one line _or_ we still have non-last
                // regions. Crucially, we don't want to infinitely create
                // new regions which are too small.
                if !regions.size.y.fits(sub.height() + pos.y)
                    && (regions.may_progress()
                        || (regions.may_break() && !frames.is_empty()))
                {
                    break;
                }

                let (sub, _) = rows.next().unwrap();
                height = height.max(pos.y + sub.height());
                frames.push((sub, pos));
            }

            equation_builders
                .push(MathRunFrameBuilder { frames, size: Size::new(width, height) });
            regions.next();
        }

        // Append remaining rows to the equation builder of the last region.
        if let Some(equation_builder) = equation_builders.last_mut() {
            equation_builder.frames.extend(rows.map(|(frame, mut pos)| {
                pos.y -= last_first_pos.y;
                (frame, pos)
            }));

            let height = equation_builder
                .frames
                .iter()
                .map(|(frame, pos)| frame.height() + pos.y)
                .max()
                .unwrap_or(equation_builder.size.y);

            equation_builder.size.y = height;
        }

        // Ensure that there is at least one frame, even for empty equations.
        if equation_builders.is_empty() {
            equation_builders
                .push(MathRunFrameBuilder { frames: vec![], size: Size::zero() });
        }

        equation_builders
    } else {
        vec![full_equation_builder]
    };

    let Some(numbering) = (**elem).numbering(styles) else {
        let frames = equation_builders
            .into_iter()
            .map(MathRunFrameBuilder::build)
            .collect();
        return Ok(Fragment::frames(frames));
    };

    let pod = Region::new(regions.base(), Axes::splat(false));
    let counter = Counter::of(EquationElem::elem())
        .display_at_loc(engine, elem.location().unwrap(), styles, numbering)?
        .spanned(span);
    let number =
        (engine.routines.layout_frame)(engine, &counter, locator.next(&()), styles, pod)?;

    static NUMBER_GUTTER: Em = Em::new(0.5);
    let full_number_width = number.width() + NUMBER_GUTTER.resolve(styles);

    let number_align = match elem.number_align(styles) {
        SpecificAlignment::H(h) => SpecificAlignment::Both(h, VAlignment::Horizon),
        SpecificAlignment::V(v) => SpecificAlignment::Both(OuterHAlignment::End, v),
        SpecificAlignment::Both(h, v) => SpecificAlignment::Both(h, v),
    };

    // Add equation numbers to each equation region.
    let region_count = equation_builders.len();
    let frames = equation_builders
        .into_iter()
        .map(|builder| {
            if builder.frames.is_empty() && region_count > 1 {
                // Don't number empty regions, but do number empty equations.
                return builder.build();
            }
            add_equation_number(
                builder,
                number.clone(),
                number_align.resolve(styles),
                AlignElem::alignment_in(styles).resolve(styles).x,
                regions.size.x,
                full_number_width,
            )
        })
        .collect();

    Ok(Fragment::frames(frames))
}

fn add_equation_number(
    equation_builder: MathRunFrameBuilder,
    number: Frame,
    number_align: Axes<FixedAlignment>,
    equation_align: FixedAlignment,
    region_size_x: Abs,
    full_number_width: Abs,
) -> Frame {
    let first =
        equation_builder.frames.first().map_or(
            (equation_builder.size, Point::zero(), Abs::zero()),
            |(frame, pos)| (frame.size(), *pos, frame.baseline()),
        );
    let last =
        equation_builder.frames.last().map_or(
            (equation_builder.size, Point::zero(), Abs::zero()),
            |(frame, pos)| (frame.size(), *pos, frame.baseline()),
        );
    let line_count = equation_builder.frames.len();
    let mut equation = equation_builder.build();

    let width = if region_size_x.is_finite() {
        region_size_x
    } else {
        equation.width() + 2.0 * full_number_width
    };

    let is_multiline = line_count >= 2;
    let resizing_offset = resize_equation(
        &mut equation,
        &number,
        number_align,
        equation_align,
        width,
        is_multiline,
        [first, last],
    );
    equation.translate(Point::with_x(match (equation_align, number_align.x) {
        (FixedAlignment::Start, FixedAlignment::Start) => full_number_width,
        (FixedAlignment::End, FixedAlignment::End) => -full_number_width,
        _ => Abs::zero(),
    }));

    let x = match number_align.x {
        FixedAlignment::Start => Abs::zero(),
        FixedAlignment::End => equation.width() - number.width(),
        _ => unreachable!(),
    };
    let y = {
        let align_baselines = |(_, pos, baseline): (_, Point, Abs), number: &Frame| {
            resizing_offset.y + pos.y + baseline - number.baseline()
        };
        match number_align.y {
            FixedAlignment::Start => align_baselines(first, &number),
            FixedAlignment::Center if !is_multiline => align_baselines(first, &number),
            // In this case, the center lines (not baselines) of the number frame
            // and the equation frame shall be aligned.
            FixedAlignment::Center => (equation.height() - number.height()) / 2.0,
            FixedAlignment::End => align_baselines(last, &number),
        }
    };

    equation.push_frame(Point::new(x, y), number);
    equation
}

/// Resize the equation's frame accordingly so that it encompasses the number.
fn resize_equation(
    equation: &mut Frame,
    number: &Frame,
    number_align: Axes<FixedAlignment>,
    equation_align: FixedAlignment,
    width: Abs,
    is_multiline: bool,
    [first, last]: [(Axes<Abs>, Point, Abs); 2],
) -> Point {
    if matches!(number_align.y, FixedAlignment::Center if is_multiline) {
        // In this case, the center lines (not baselines) of the number frame
        // and the equation frame shall be aligned.
        return equation.resize(
            Size::new(width, equation.height().max(number.height())),
            Axes::<FixedAlignment>::new(equation_align, FixedAlignment::Center),
        );
    }

    let excess_above = Abs::zero().max({
        if !is_multiline || matches!(number_align.y, FixedAlignment::Start) {
            let (.., baseline) = first;
            number.baseline() - baseline
        } else {
            Abs::zero()
        }
    });
    let excess_below = Abs::zero().max({
        if !is_multiline || matches!(number_align.y, FixedAlignment::End) {
            let (size, .., baseline) = last;
            (number.height() - number.baseline()) - (size.y - baseline)
        } else {
            Abs::zero()
        }
    });

    // The vertical expansion is asymmetric on the top and bottom edges, so we
    // first align at the top then translate the content downward later.
    let resizing_offset = equation.resize(
        Size::new(width, equation.height() + excess_above + excess_below),
        Axes::<FixedAlignment>::new(equation_align, FixedAlignment::Start),
    );
    equation.translate(Point::with_y(excess_above));
    resizing_offset + Point::with_y(excess_above)
}

macro_rules! constants {
    () => {};
    ($name:ident; $($tail:tt)*) => {
        #[allow(dead_code)]
        fn $name(&self) -> Em {
            let value = self.font.ttf().tables().math.and_then(|table| table.constants).map(|constants| constants.$name().value).unwrap();
            self.font.to_em(value)
        }
        constants!($($tail)*);
    };
    (~ $name:ident; $($tail:tt)*) => {
        #[allow(dead_code)]
        fn $name(&self) -> Em {
            let value = self.font.ttf().tables().math.and_then(|table| table.constants).map(|constants| constants.$name()).unwrap();
            self.font.to_em(value)
        }
        constants!($($tail)*);
    };
    (% $name:ident; $($tail:tt)*) => {
        #[allow(dead_code)]
        fn $name(&self) -> i16 {
            self.font.ttf().tables().math.and_then(|table| table.constants).map(|constants| constants.$name()).unwrap()
        }
        constants!($($tail)*);
    };
    ($text:ident, $display:ident; $($tail:tt)*) => {
        #[allow(dead_code)]
        fn $text(&self, styles: StyleChain) -> Em {
            let value = self.font.ttf().tables().math.and_then(|table| table.constants).map(|constants| match EquationElem::size_in(styles) {
                MathSize::Display => constants.$display().value,
                _ => constants.$text().value,
            }).unwrap();
            self.font.to_em(value)
        }
        constants!($($tail)*);
    };
}

/// The context for math layout.
struct MathContext<'a, 'v, 'e> {
    // External.
    engine: &'v mut Engine<'e>,
    locator: &'v mut SplitLocator<'a>,
    region: Region,
    // Font-related.
    font: Font,
    flac: Option<LookupIndex>,
    dtls: Option<LookupIndex>,
    ssty: Option<LookupIndex>,
    glyphwise: Option<Vec<(LookupIndex, u32)>>,
    // Mutable.
    fragments: Vec<MathFragment>,
}

impl<'a, 'v, 'e> MathContext<'a, 'v, 'e> {
    /// Resolve the current font in `styles`.
    fn current_font(engine: &mut Engine, styles: StyleChain) -> Font {
        let variant = variant(styles);
        let world = engine.world;
        let id = families(styles)
            .find_map(|family| world.book().select(family.as_str(), variant))
            .unwrap();
        world.font(id).unwrap()
    }

    /// Create a new math context.
    fn new(
        engine: &'v mut Engine<'e>,
        locator: &'v mut SplitLocator<'a>,
        styles: StyleChain,
        base: Size,
    ) -> Self {
        let font = Self::current_font(engine, styles);

        let feat = |tag: &[u8; 4]| {
            font.ttf().tables().gsub.and_then(|gsub| {
                gsub.features.find(Tag::from_bytes(tag))?.lookup_indices.get(0)
            })
        };

        let features = features(styles);
        let glyphwise = Some(
            features
                .into_iter()
                .filter_map(|feature| {
                    let lookup = font.ttf().tables().gsub.and_then(|gsub| {
                        gsub.features.find(feature.tag)?.lookup_indices.get(0)
                    })?;
                    Some((lookup, feature.value))
                })
                .collect(),
        );

        Self {
            engine,
            locator,
            region: Region::new(base, Axes::splat(false)),
            flac: feat(b"flac"),
            dtls: feat(b"dtls"),
            ssty: feat(b"ssty"),
            glyphwise,
            font,
            fragments: vec![],
        }
    }

    /// Update the current font using `styles`. Currently does not check if the
    /// font has actually changed before proceeding, as features like stylistic
    /// sets may have also changed.
    fn update_font(&mut self, styles: StyleChain) {
        let font = Self::current_font(self.engine, styles);

        let feat = |tag: &[u8; 4]| {
            font.ttf().tables().gsub.and_then(|gsub| {
                gsub.features.find(Tag::from_bytes(tag))?.lookup_indices.get(0)
            })
        };
        self.flac = feat(b"flac");
        self.dtls = feat(b"dtls");
        self.ssty = feat(b"ssty");

        let features = features(styles);
        self.glyphwise = Some(
            features
                .into_iter()
                .filter_map(|feature| {
                    let lookup = font.ttf().tables().gsub.and_then(|gsub| {
                        gsub.features.find(feature.tag)?.lookup_indices.get(0)
                    })?;
                    Some((lookup, feature.value))
                })
                .collect(),
        );
        self.font = font;
    }

    ///
    fn advance_width(&self, id: GlyphId) -> Em {
        self.font
            .to_em(self.font.ttf().glyph_hor_advance(id).unwrap_or_default())
    }

    ///
    fn index(&self, code_point: char) -> Option<GlyphId> {
        self.font.ttf().glyph_index(code_point)
    }

    ///
    fn height(&self, id: GlyphId) -> (Em, Em) {
        let bbox = self.font.ttf().glyph_bounding_box(id).unwrap_or(Rect {
            x_min: 0,
            y_min: 0,
            x_max: 0,
            y_max: 0,
        });
        let ascent = self.font.to_em(bbox.y_max);
        let descent = -self.font.to_em(bbox.y_min);
        (ascent, descent)
    }

    /// Look up the italics correction for a glyph.
    fn italics_correction(&self, id: GlyphId) -> Em {
        self.font
            .ttf()
            .tables()
            .math
            .and_then(|table| table.glyph_info?.italic_corrections?.get(id))
            .map(|value| self.font.to_em(value.value))
            .unwrap_or_default()
    }

    /// Look up the top accent attachment position for a glyph.
    fn top_accent_attachment(&self, id: GlyphId) -> Em {
        self.font
            .ttf()
            .tables()
            .math
            .and_then(|table| table.glyph_info?.top_accent_attachments?.get(id))
            .map(|value| self.font.to_em(value.value))
            .unwrap_or_else(|| {
                (self.italics_correction(id) + self.advance_width(id)) / 2.0
            })
    }

    /// Look up whether a glyph is an extended shape.
    fn extended_shape(&self, id: GlyphId) -> bool {
        self.font
            .ttf()
            .tables()
            .math
            .and_then(|table| table.glyph_info?.extended_shapes?.get(id))
            .is_some()
    }

    ///
    fn min_connector_overlap(&self) -> Em {
        self.font
            .ttf()
            .tables()
            .math
            .and_then(|table| table.variants)
            .map(|variants| self.font.to_em(variants.min_connector_overlap))
            .unwrap_or_default()
    }

    // fn construction(&self, id: GlyphId, axis: Axis) -> ttf_parser::math::GlyphConstruction {
    //     self.font.ttf().tables().math.and_then(|table| table.variants).and_then(|variants| match axis {
    //         Axis::X => variants.horizontal_constructions,
    //         Axis::Y => variants.vertical_constructions,
    //     }.get(id)).unwrap_or(GlyphConstruction { assembly: None, variants: LazyArray16::new(&[]) })
    // }

    constants! {
        % script_percent_scale_down;
        % script_script_percent_scale_down;
        ~ delimited_sub_formula_min_height;
        ~ display_operator_min_height;
        math_leading;
        axis_height;
        accent_base_height;
        flattened_accent_base_height;
        subscript_shift_down;
        subscript_top_max;
        subscript_baseline_drop_min;
        superscript_shift_up;
        superscript_shift_up_cramped;
        superscript_bottom_min;
        superscript_baseline_drop_max;
        sub_superscript_gap_min;
        superscript_bottom_max_with_subscript;
        space_after_script;
        upper_limit_gap_min;
        upper_limit_baseline_rise_min;
        lower_limit_gap_min;
        lower_limit_baseline_drop_min;
        stack_top_shift_up, stack_top_display_style_shift_up;
        stack_bottom_shift_down, stack_bottom_display_style_shift_down;
        stack_gap_min, stack_display_style_gap_min;
        stretch_stack_top_shift_up;
        stretch_stack_bottom_shift_down;
        stretch_stack_gap_above_min;
        stretch_stack_gap_below_min;
        fraction_numerator_shift_up, fraction_numerator_display_style_shift_up;
        fraction_denominator_shift_down, fraction_denominator_display_style_shift_down;
        fraction_numerator_gap_min, fraction_num_display_style_gap_min;
        fraction_rule_thickness;
        fraction_denominator_gap_min, fraction_denom_display_style_gap_min;
        skewed_fraction_horizontal_gap;
        skewed_fraction_vertical_gap;
        overbar_vertical_gap;
        overbar_rule_thickness;
        overbar_extra_ascender;
        underbar_vertical_gap;
        underbar_rule_thickness;
        underbar_extra_descender;
        radical_vertical_gap, radical_display_style_vertical_gap;
        radical_rule_thickness;
        radical_extra_ascender;
        radical_kern_before_degree;
        radical_kern_after_degree;
        % radical_degree_bottom_raise_percent;
    }

    ///
    fn single_substitution(
        &self,
        index: Option<LookupIndex>,
        id: GlyphId,
    ) -> Option<GlyphId> {
        let Some(SubstitutionSubtable::Single(single)) =
            self.font.ttf().tables().gsub.and_then(|gsub| {
                gsub.lookups.get(index?)?.subtables.get::<SubstitutionSubtable>(0)
            })
        else {
            return None;
        };
        match single {
            SingleSubstitution::Format1 { coverage, delta } => {
                coverage.get(id).map(|_| GlyphId(id.0.wrapping_add(delta as u16)))
            }
            SingleSubstitution::Format2 { coverage, substitutes } => {
                coverage.get(id).and_then(|idx| substitutes.get(idx))
            }
        }
    }

    ///
    fn alternate_substitution(
        &self,
        index: Option<LookupIndex>,
        value: u16,
        id: GlyphId,
    ) -> Option<GlyphId> {
        let Some(SubstitutionSubtable::Alternate(alternate)) =
            self.font.ttf().tables().gsub.and_then(|gsub| {
                gsub.lookups.get(index?)?.subtables.get::<SubstitutionSubtable>(0)
            })
        else {
            return None;
        };
        alternate
            .coverage
            .get(id)
            .and_then(|idx| alternate.alternate_sets.get(idx)?.alternates.get(value))
    }

    ///
    fn glyphwise_substitution(
        &self,
        index: (LookupIndex, u32),
        id: GlyphId,
    ) -> Option<GlyphId> {
        match self.font.ttf().tables().gsub.and_then(|gsub| {
            gsub.lookups.get(index.0)?.subtables.get::<SubstitutionSubtable>(0)
        })? {
            SubstitutionSubtable::Single(single) => match single {
                SingleSubstitution::Format1 { coverage, delta } => {
                    coverage.get(id).map(|_| GlyphId(id.0.wrapping_add(delta as u16)))
                }
                SingleSubstitution::Format2 { coverage, substitutes } => {
                    coverage.get(id).and_then(|idx| substitutes.get(idx))
                }
            },
            SubstitutionSubtable::Alternate(alternate) => {
                alternate.coverage.get(id).and_then(|idx| {
                    alternate.alternate_sets.get(idx)?.alternates.get(index.1 as u16)
                })
            }
            _ => None,
        }
    }

    /// Push a fragment.
    fn push(&mut self, fragment: impl Into<MathFragment>) {
        self.fragments.push(fragment.into());
    }

    /// Push multiple fragments.
    fn extend(&mut self, fragments: impl IntoIterator<Item = MathFragment>) {
        self.fragments.extend(fragments);
    }

    /// Layout the given element and return the result as a [`MathRun`].
    fn layout_into_run(
        &mut self,
        elem: &Content,
        styles: StyleChain,
    ) -> SourceResult<MathRun> {
        Ok(MathRun::new(self.layout_into_fragments(elem, styles)?))
    }

    /// Layout the given element and return the resulting [`MathFragment`]s.
    fn layout_into_fragments(
        &mut self,
        elem: &Content,
        styles: StyleChain,
    ) -> SourceResult<Vec<MathFragment>> {
        // The element's layout_math() changes the fragments held in this
        // MathContext object, but for convenience this function shouldn't change
        // them, so we restore the MathContext's fragments after obtaining the
        // layout result.
        let prev = std::mem::take(&mut self.fragments);
        self.layout_into_self(elem, styles)?;
        Ok(std::mem::replace(&mut self.fragments, prev))
    }

    /// Layout the given element and return the result as a
    /// unified [`MathFragment`].
    fn layout_into_fragment(
        &mut self,
        elem: &Content,
        styles: StyleChain,
    ) -> SourceResult<MathFragment> {
        Ok(self.layout_into_run(elem, styles)?.into_fragment(styles))
    }

    /// Layout the given element and return the result as a [`Frame`].
    fn layout_into_frame(
        &mut self,
        elem: &Content,
        styles: StyleChain,
    ) -> SourceResult<Frame> {
        Ok(self.layout_into_fragment(elem, styles)?.into_frame())
    }

    /// Layout arbitrary content.
    fn layout_into_self(
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

        let outer = styles;
        for (elem, styles) in pairs {
            self.update_font(styles);
            layout_realized(elem, self, styles)?;
        }
        self.update_font(outer);

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
        ctx.push(MathFragment::Space(
            ctx.font.space_width().unwrap_or(THICK).resolve(styles),
        ));
    } else if elem.is::<LinebreakElem>() {
        ctx.push(MathFragment::Linebreak);
    } else if let Some(elem) = elem.to_packed::<HElem>() {
        layout_h(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<TextElem>() {
        self::text::layout_text(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<BoxElem>() {
        layout_box(elem, ctx, styles)?;
    } else if elem.is::<AlignPointElem>() {
        ctx.push(MathFragment::Align);
    } else if let Some(elem) = elem.to_packed::<ClassElem>() {
        layout_class(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<AccentElem>() {
        self::accent::layout_accent(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<AttachElem>() {
        self::attach::layout_attach(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<PrimesElem>() {
        self::attach::layout_primes(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<ScriptsElem>() {
        self::attach::layout_scripts(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<LimitsElem>() {
        self::attach::layout_limits(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<CancelElem>() {
        self::cancel::layout_cancel(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<FracElem>() {
        self::frac::layout_frac(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<BinomElem>() {
        self::frac::layout_binom(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<LrElem>() {
        self::lr::layout_lr(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<MidElem>() {
        self::lr::layout_mid(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<VecElem>() {
        self::mat::layout_vec(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<MatElem>() {
        self::mat::layout_mat(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<CasesElem>() {
        self::mat::layout_cases(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<OpElem>() {
        layout_op(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<RootElem>() {
        self::root::layout_root(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<StretchElem>() {
        self::stretch::layout_stretch(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<UnderlineElem>() {
        self::underover::layout_underline(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<OverlineElem>() {
        self::underover::layout_overline(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<UnderbraceElem>() {
        self::underover::layout_underbrace(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<OverbraceElem>() {
        self::underover::layout_overbrace(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<UnderbracketElem>() {
        self::underover::layout_underbracket(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<OverbracketElem>() {
        self::underover::layout_overbracket(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<UnderparenElem>() {
        self::underover::layout_underparen(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<OverparenElem>() {
        self::underover::layout_overparen(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<UndershellElem>() {
        self::underover::layout_undershell(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<OvershellElem>() {
        self::underover::layout_overshell(elem, ctx, styles)?
    } else {
        let mut frame = layout_external(elem, ctx, styles)?;
        if !frame.has_baseline() {
            let axis = ctx.axis_height().resolve(styles);
            frame.set_baseline(frame.height() / 2.0 + axis);
        }
        ctx.push(
            FrameFragment::new(styles, frame)
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
    let frame = (ctx.engine.routines.layout_box)(
        elem,
        ctx.engine,
        ctx.locator.next(&elem.span()),
        styles,
        ctx.region.size,
    )?;
    ctx.push(FrameFragment::new(styles, frame).with_spaced(true));
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
            ctx.push(MathFragment::Spacing(rel.abs.resolve(styles), elem.weak(styles)));
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
        FrameFragment::new(styles, fragment.into_frame())
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
    (ctx.engine.routines.layout_frame)(
        ctx.engine,
        content,
        ctx.locator.next(&content.span()),
        styles,
        ctx.region,
    )
}
