use std::num::NonZeroUsize;

use typst_utils::NonZeroExt;
use unicode_math_class::MathClass;

use crate::diag::SourceResult;
use crate::engine::Engine;
use crate::foundations::{
    elem, select_where, Cast, Content, Label, NativeElement, Packed, Selector, Show,
    ShowSet, Smart, StyleChain, Styles, Synthesize,
};
use crate::introspection::{Count, Counter, CounterUpdate, Locatable};
use crate::layout::{
    AlignElem, Alignment, BlockElem, InlineElem, OuterHAlignment, SpecificAlignment,
    VAlignment,
};
use crate::math::{MathSize, MathVariant};
use crate::model::{Numbering, Outlinable, ParLine, Refable, Supplement};
use crate::text::{FontFamily, FontList, FontWeight, LocalName, TextElem};

/// A mathematical equation.
///
/// Can be displayed inline with text or as a separate block.
///
/// # Example
/// ```example
/// #set text(font: "New Computer Modern")
///
/// Let $a$, $b$, and $c$ be the side
/// lengths of right-angled triangle.
/// Then, we know that:
/// $ a^2 + b^2 = c^2 $
///
/// Prove by induction:
/// $ sum_(k=1)^n k = (n(n+1)) / 2 $
/// ```
///
/// By default, block-level equations will not break across pages. This can be
/// changed through `{show math.equation: set block(breakable: true)}`.
///
/// # Syntax
/// This function also has dedicated syntax: Write mathematical markup within
/// dollar signs to create an equation. Starting and ending the equation with at
/// least one space lifts it into a separate block that is centered
/// horizontally. For more details about math syntax, see the
/// [main math page]($category/math).
#[elem(Locatable, Synthesize, Show, ShowSet, Count, LocalName, Refable, Outlinable)]
pub struct EquationElem {
    /// Whether the equation is displayed as a separate block.
    #[default(false)]
    pub block: bool,

    /// How to [number]($numbering) block-level equations.
    ///
    /// ```example
    /// #set math.equation(numbering: "(1)")
    ///
    /// We define:
    /// $ phi.alt := (1 + sqrt(5)) / 2 $ <ratio>
    ///
    /// With @ratio, we get:
    /// $ F_n = floor(1 / sqrt(5) phi.alt^n) $
    /// ```
    #[borrowed]
    pub numbering: Option<Numbering>,

    /// How to number equations.
    ///
    /// ```example
    ///
    /// ```
    #[default(NumberingMode::Equation)]
    pub numbering_mode: NumberingMode,

    /// The alignment of the equation numbering.
    ///
    /// By default, the alignment is `{end + horizon}`. For the horizontal
    /// component, you can use `{right}`, `{left}`, or `{start}` and `{end}`
    /// of the text direction; for the vertical component, you can use
    /// `{top}`, `{horizon}`, or `{bottom}`.
    ///
    /// ```example
    /// #set math.equation(numbering: "(1)", number-align: bottom)
    ///
    /// We can calculate:
    /// $ E &= sqrt(m_0^2 + p^2) \
    ///     &approx 125 "GeV" $
    /// ```
    #[default(SpecificAlignment::Both(OuterHAlignment::End, VAlignment::Horizon))]
    pub number_align: SpecificAlignment<OuterHAlignment, VAlignment>,

    /// A supplement for the equation.
    ///
    /// For references to equations, this is added before the referenced number.
    ///
    /// If a function is specified, it is passed the referenced equation and
    /// should return content.
    ///
    /// ```example
    /// #set math.equation(numbering: "(1)", supplement: [Eq.])
    ///
    /// We define:
    /// $ phi.alt := (1 + sqrt(5)) / 2 $ <ratio>
    ///
    /// With @ratio, we get:
    /// $ F_n = floor(1 / sqrt(5) phi.alt^n) $
    /// ```
    pub supplement: Smart<Option<Supplement>>,

    /// The contents of the equation.
    #[required]
    pub body: Content,

    /// Whether this is an equation created as a line within a block equation.
    ///
    /// This is used so that during realization we know not to unwrap this
    /// nested EquationElem.
    #[internal]
    #[parse(None)]
    #[default(false)]
    pub line: bool,

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

impl Synthesize for Packed<EquationElem> {
    fn synthesize(
        &mut self,
        engine: &mut Engine,
        styles: StyleChain,
    ) -> SourceResult<()> {
        let supplement = match self.as_ref().supplement(styles) {
            Smart::Auto => TextElem::packed(Self::local_name_in(styles)),
            Smart::Custom(None) => Content::empty(),
            Smart::Custom(Some(supplement)) => {
                supplement.resolve(engine, styles, [self.clone().pack()])?
            }
        };

        self.push_supplement(Smart::Custom(Some(Supplement::Content(supplement))));
        Ok(())
    }
}

impl Show for Packed<EquationElem> {
    fn show(&self, engine: &mut Engine, styles: StyleChain) -> SourceResult<Content> {
        if self.block(styles) {
            Ok(BlockElem::multi_layouter(
                self.clone(),
                engine.routines.layout_equation_block,
            )
            .pack()
            .spanned(self.span()))
        } else {
            Ok(InlineElem::layouter(self.clone(), engine.routines.layout_equation_inline)
                .pack()
                .spanned(self.span()))
        }
    }
}

impl ShowSet for Packed<EquationElem> {
    fn show_set(&self, styles: StyleChain) -> Styles {
        let mut out = Styles::new();
        if self.block(styles) {
            out.set(AlignElem::set_alignment(Alignment::CENTER));
            out.set(BlockElem::set_breakable(false));
            out.set(ParLine::set_numbering(None));
            out.set(EquationElem::set_size(MathSize::Display));
            // So line equations inherit the block-ness of their parent.
            out.set(EquationElem::set_block(true));
        } else {
            out.set(EquationElem::set_size(MathSize::Text));
        }
        out.set(TextElem::set_weight(FontWeight::from_number(450)));
        out.set(TextElem::set_font(FontList(vec![FontFamily::new(
            "New Computer Modern Math",
        )])));
        out
    }
}

impl Count for Packed<EquationElem> {
    fn update(&self, engine: &mut Engine) -> Option<CounterUpdate> {
        // (self.block(StyleChain::default()) && self.numbering().is_some())
        //     .then(|| CounterUpdate::Step(NonZeroUsize::ONE))
        if (**self).numbering(StyleChain::default()).is_none()
            || !self.block(StyleChain::default())
        {
            return None;
        }

        match (**self).numbering_mode(StyleChain::default()) {
            NumberingMode::Equation => {
                // Don't step counter if there is a do no label marker `<*>`
                // attached to it, or if this is not the first EquationElem
                // with the given label.
                if let Some(label) = self.label() {
                    if label.as_str() == "*" {
                        None
                    } else {
                        let equations = get_equations(engine, label);
                        if *self != equations[0] {
                            None
                        } else {
                            Some(CounterUpdate::Step(NonZeroUsize::ONE))
                        }
                    }
                } else {
                    Some(CounterUpdate::Step(NonZeroUsize::ONE))
                }
            }
            NumberingMode::Line => {
                // TODO
                if let Some(label) = self.label() {
                    if label.as_str() == "*" {
                        None
                    } else {
                        Some(CounterUpdate::Step(NonZeroUsize::ONE))
                    }
                } else {
                    Some(CounterUpdate::Step(NonZeroUsize::ONE))
                }
            }
            NumberingMode::Label => todo!(),
            NumberingMode::Reference => todo!(),
        }
    }
}

impl LocalName for Packed<EquationElem> {
    const KEY: &'static str = "equation";
}

impl Refable for Packed<EquationElem> {
    fn supplement(&self) -> Content {
        // After synthesis, this should always be custom content.
        match (**self).supplement(StyleChain::default()) {
            Smart::Custom(Some(Supplement::Content(content))) => content,
            _ => Content::empty(),
        }
    }

    fn counter(&self) -> Counter {
        Counter::of(EquationElem::elem())
    }

    fn numbering(&self) -> Option<&Numbering> {
        (**self).numbering(StyleChain::default()).as_ref()
    }
}

impl Outlinable for Packed<EquationElem> {
    fn outline(
        &self,
        engine: &mut Engine,
        styles: StyleChain,
    ) -> SourceResult<Option<Content>> {
        if !self.block(StyleChain::default()) {
            return Ok(None);
        }
        let Some(numbering) = self.numbering() else {
            return Ok(None);
        };

        // After synthesis, this should always be custom content.
        let mut supplement = match (**self).supplement(StyleChain::default()) {
            Smart::Custom(Some(Supplement::Content(content))) => content,
            _ => Content::empty(),
        };

        if !supplement.is_empty() {
            supplement += TextElem::packed("\u{a0}");
        }

        let numbers = self.counter().display_at_loc(
            engine,
            self.location().unwrap(),
            styles,
            numbering,
        )?;

        Ok(Some(supplement + numbers))
    }
}

/// How to number equations.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash, Cast)]
pub enum NumberingMode {
    /// Every equation is numbered.
    #[default]
    Equation,
    /// Every line is numbered.
    Line,
    /// Only labelled equations or labelled lines have a number. If both a line
    /// and an equation have a label, then subnumbering is enabled.
    Label,
    /// Only equations that are referenced in the document are numbered.
    /// Otherwise, this has the same logic as NumberingMode::Label.
    Reference,
}

pub fn get_equations(engine: &mut Engine, label: Label) -> Vec<Packed<EquationElem>> {
    let target = Selector::And(
        [Selector::Label(label), select_where!(EquationElem, Block => true)]
            .into_iter()
            .collect(),
    );

    engine
        .introspector
        .query(&target)
        .into_iter()
        .map(|x| x.into_packed::<EquationElem>().unwrap())
        .collect()
}
