/// The top-level item in the math IR.
#[derive(Debug, Clone)]
pub enum MathItem<'a> {
    // A layoutable component with properties.
    Component(MathComponent<'a>),
    // Special, non-component items.
    Spacing(Abs, bool),
    Space,
    Linebreak,
    Align,
    Tag(Tag),
}

impl<'a> From<MathComponent<'a>> for MathItem<'a> {
    fn from(comp: MathComponent<'a>) -> MathItem<'a> {
        MathItem::Component(comp)
    }
}

impl<'a> MathItem<'a> {
    pub fn set_limits(&mut self, limits: Limits) {
        if let Self::Component(comp) = self {
            comp.props.limits = limits;
        }
    }

    pub fn limits(&self) -> Limits {
        match self {
            Self::Component(comp) => comp.props.limits,
            _ => Limits::Never,
        }
    }

    pub fn set_class(&mut self, class: MathClass) {
        if let Self::Component(comp) = self {
            comp.props.class = class;
        }
    }

    pub fn class(&self) -> MathClass {
        match self {
            Self::Component(comp) => comp.props.class,
            Self::Spacing(_, _) | Self::Space | Self::Linebreak => MathClass::Space,
            Self::Align | Self::Tag(_) => MathClass::Special,
        }
    }

    pub fn size(&self) -> Option<MathSize> {
        match self {
            Self::Component(comp) => Some(comp.props.size),
            _ => None,
        }
    }

    pub fn set_lspace(&mut self, lspace: Option<Em>) {
        if let Self::Component(comp) = self {
            comp.props.lspace = lspace;
        }
    }

    pub fn set_rspace(&mut self, rspace: Option<Em>) {
        if let Self::Component(comp) = self {
            comp.props.rspace = rspace;
        }
    }

    pub fn is_spaced(&self) -> bool {
        if self.class() == MathClass::Fence {
            return true;
        }

        if let Self::Component(comp) = self
            && comp.props.spaced
            && matches!(comp.props.class, MathClass::Normal | MathClass::Alphabetic)
        {
            true
        } else {
            false
        }
    }

    pub fn is_ignorant(&self) -> bool {
        match self {
            Self::Component(comp) => comp.props.ignorant,
            Self::Tag(_) => true,
            _ => false,
        }
    }
}

/// A generic component that bundles a specific item with common properties.
#[derive(Debug, Clone)]
struct MathComponent<'a> {
    /// The specific item.
    kind: MathKind<'a>,
    /// The properties attached to this component.
    props: MathProperties<'a>,
}

/// A layoutable math item.
#[derive(Debug, Clone)]
enum MathKind<'a> {
    Group(GroupItem<'a>),
    Radical(RadicalItem<'a>),
    Fenced(FencedItem<'a>),
    Fraction(FractionItem<'a>),
    SkewedFraction(SkewedFractionItem<'a>),
    Table(TableItem<'a>),
    Scripts(ScriptsItem<'a>),
    Accent(AccentItem<'a>),
    Cancel(CancelItem<'a>),
    Line(LineItem<'a>),
    Primes(PrimesItem<'a>),
    Glyph(GlyphItem),
    Text(TextItem),
    External(ExternalItem<'a>),
    Box(BoxItem<'a>),
}

/// Shared properties for layoutable components.
#[derive(Debug, Clone)]
struct MathProperties<'a> {
    limits: Limits,
    class: MathClass,
    size: MathSize,
    ignorant: bool,
    text_like: bool,
    spaced: bool,
    lspace: Option<Em>,
    rspace: Option<Em>,
    styles: StyleChain<'a>,
    span: Span,
}

impl<'a> MathProperties<'a> {
    fn default(styles: StyleChain<'a>) -> MathProperties<'a> {
        Self {
            limits: Limits::Never,
            class: styles.get(EquationElem::class).unwrap_or(MathClass::Normal),
            size: styles.get(EquationElem::size),
            ignorant: false,
            text_like: false,
            spaced: false,
            lspace: None,
            rspace: None,
            styles,
            span: Span::detached(),
        }
    }

    fn with_limits(mut self, limits: Limits) -> Self {
        self.limits = limits;
        self
    }

    fn with_class(mut self, class: MathClass) -> Self {
        self.class = class;
        self
    }

    fn with_ignorant(mut self, ignorant: bool) -> Self {
        self.ignorant = ignorant;
        self
    }

    fn with_text_like(mut self, text_like: bool) -> Self {
        self.text_like = text_like;
        self
    }

    fn with_spaced(mut self, spaced: bool) -> Self {
        self.spaced = spaced;
        self
    }

    fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }
}

#[derive(Debug, Clone)]
pub struct GroupItem<'a> {
    pub items: MathRun<'a>,
}

impl<'a> GroupItem<'a> {
    pub fn new(items: Vec<MathItem<'a>>, styles: StyleChain<'a>) -> MathItem<'a> {
        let kind = MathKind::Group(Self { items: MathRun::new(items) });
        let props = MathProperties::default(styles);
        MathComponent { kind, props }.into()
    }
}

#[derive(Debug, Clone)]
pub struct RadicalItem<'a> {
    pub radicand: MathRun<'a>,
    pub index: Option<MathRun<'a>>,
    pub sqrt: MathRun<'a>,
}

impl<'a> RadicalItem<'a> {
    pub fn new(
        radicand: MathRun<'a>,
        index: Option<MathRun<'a>>,
        sqrt: MathRun<'a>,
        styles: StyleChain<'a>,
        span: Span,
    ) -> MathItem<'a> {
        let kind = MathKind::Radical(Self { radicand, index, sqrt });
        let props = MathProperties::default(styles).with_span(span);
        MathComponent { kind, props }.into()
    }
}

#[derive(Debug, Clone)]
pub struct FencedItem<'a> {
    pub open: Option<MathRun<'a>>,
    pub close: Option<MathRun<'a>>,
    pub body: MathRun<'a>,
    pub balanced: bool,
    pub target: Rel<Abs>,
}

impl<'a> FencedItem<'a> {
    pub fn new(
        open: Option<MathRun<'a>>,
        close: Option<MathRun<'a>>,
        body: MathRun<'a>,
        balanced: bool,
        target: Rel<Abs>,
        styles: StyleChain<'a>,
        span: Span,
    ) -> MathItem<'a> {
        let kind = MathKind::Fenced(Self { open, close, body, balanced, target });
        let props = MathProperties::default(styles).with_span(span);
        MathComponent { kind, props }.into()
    }
}

#[derive(Debug, Clone)]
pub struct FractionItem<'a> {
    pub numerator: MathRun<'a>,
    pub denominator: MathRun<'a>,
    pub line: bool,
}

impl<'a> FractionItem<'a> {
    pub fn new(
        numerator: MathRun<'a>,
        denominator: MathRun<'a>,
        line: bool,
        styles: StyleChain<'a>,
        span: Span,
    ) -> MathItem<'a> {
        let kind = MathKind::Fraction(Self { numerator, denominator, line });
        let props = MathProperties::default(styles).with_span(span);
        MathComponent { kind, props }.into()
    }
}

#[derive(Debug, Clone)]
pub struct SkewedFractionItem<'a> {
    pub numerator: MathRun<'a>,
    pub denominator: MathRun<'a>,
}

impl<'a> SkewedFractionItem<'a> {
    pub fn new(
        numerator: MathRun<'a>,
        denominator: MathRun<'a>,
        styles: StyleChain<'a>,
    ) -> MathItem<'a> {
        let kind = MathKind::SkewedFraction(Self { numerator, denominator });
        let props = MathProperties::default(styles);
        MathComponent { kind, props }.into()
    }
}

#[derive(Debug, Clone)]
pub struct TableItem<'a> {
    /// By row.
    pub cells: Vec<Vec<MathRun<'a>>>,
    pub gap: Axes<Rel<Abs>>,
    pub augment: Option<Augment<Abs>>,
    pub align: FixedAlignment,
    pub alternator: LeftRightAlternator,
}

impl<'a> TableItem<'a> {
    pub fn new(
        cells: Vec<Vec<MathRun<'a>>>,
        gap: Axes<Rel<Abs>>,
        augment: Option<Augment<Abs>>,
        align: FixedAlignment,
        alternator: LeftRightAlternator,
        styles: StyleChain<'a>,
        span: Span,
    ) -> MathItem<'a> {
        let kind = MathKind::Table(Self { cells, gap, augment, align, alternator });
        let props = MathProperties::default(styles).with_span(span);
        MathComponent { kind, props }.into()
    }
}

#[derive(Debug, Clone)]
pub struct ScriptsItem<'a> {
    pub base: MathRun<'a>,
    pub top: Option<MathRun<'a>>,
    pub bottom: Option<MathRun<'a>>,
    pub top_left: Option<MathRun<'a>>,
    pub bottom_left: Option<MathRun<'a>>,
    pub top_right: Option<MathRun<'a>>,
    pub bottom_right: Option<MathRun<'a>>,
}

impl<'a> ScriptsItem<'a> {
    pub fn new(
        base: MathRun<'a>,
        top: Option<MathRun<'a>>,
        bottom: Option<MathRun<'a>>,
        top_left: Option<MathRun<'a>>,
        bottom_left: Option<MathRun<'a>>,
        top_right: Option<MathRun<'a>>,
        bottom_right: Option<MathRun<'a>>,
        styles: StyleChain<'a>,
    ) -> MathItem<'a> {
        let kind = MathKind::Scripts(Self {
            base,
            top,
            bottom,
            top_left,
            bottom_left,
            top_right,
            bottom_right,
        });
        let props = MathProperties::default(styles).with_class(base.class());
        MathComponent { kind, props }.into()
    }
}

#[derive(Debug, Clone)]
pub struct AccentItem<'a> {
    pub base: MathRun<'a>,
    pub accent: MathRun<'a>,
    pub is_bottom: bool,
}

impl<'a> AccentItem<'a> {
    pub fn new(
        base: MathRun<'a>,
        accent: MathRun<'a>,
        is_bottom: bool,
        styles: StyleChain<'a>,
    ) -> MathItem<'a> {
        let kind = MathKind::Accent(Self { base, accent, is_bottom });
        let props = MathProperties::default(styles);
        MathComponent { kind, props }.into()
    }
}

#[derive(Debug, Clone)]
pub struct CancelItem<'a> {
    pub base: MathRun<'a>,
    pub length: Rel<Abs>,
    pub stroke: FixedStroke,
    pub cross: bool,
    pub invert_first_line: bool,
    pub angle: Smart<CancelAngle>,
}

impl<'a> CancelItem<'a> {
    pub fn new(
        base: MathRun<'a>,
        length: Rel<Abs>,
        stroke: FixedStroke,
        cross: bool,
        invert_first_line: bool,
        angle: Smart<CancelAngle>,
        styles: StyleChain<'a>,
        span: Span,
    ) -> MathItem<'a> {
        let kind = MathKind::Cancel(Self {
            base,
            length,
            stroke,
            cross,
            invert_first_line,
            angle,
        });
        let props = MathProperties::default(styles).with_span(span);
        MathComponent { kind, props }.into()
    }
}

#[derive(Debug, Clone)]
pub struct LineItem<'a> {
    pub base: MathRun<'a>,
    pub under: bool,
}

impl<'a> LineItem<'a> {
    pub fn new(
        base: MathRun<'a>,
        under: bool,
        styles: StyleChain<'a>,
        span: Span,
    ) -> MathItem<'a> {
        let kind = MathKind::Line(Self { base, under });
        let props = MathProperties::default(styles).with_span(span);
        MathComponent { kind, props }.into()
    }
}

#[derive(Debug, Clone)]
pub struct PrimesItem<'a> {
    pub prime: MathRun<'a>,
    pub count: usize,
}

impl<'a> PrimesItem<'a> {
    pub fn new(prime: MathRun<'a>, count: usize, styles: StyleChain<'a>) -> MathItem<'a> {
        let kind = MathKind::Primes(Self { prime, count });
        let props = MathProperties::default(styles).with_text_like(true);
        MathComponent { kind, props }.into()
    }
}

#[derive(Debug, Clone)]
pub struct TextItem {
    pub text: EcoString,
}

impl TextItem {
    pub fn new<'a>(text: EcoString, styles: StyleChain<'a>, span: Span) -> MathItem<'a> {
        let kind = MathKind::Text(Self { text });
        let props = MathProperties::default(styles).with_span(span);
        MathComponent { kind, props }.into()
    }
}

#[derive(Debug, Clone)]
pub struct GlyphItem {
    pub text: EcoString,
    pub stretch: Option<(Rel<Abs>, Option<Axis>)>,
    pub mid_stretched: Option<bool>,
}

impl GlyphItem {
    pub fn new<'a>(text: EcoString, styles: StyleChain<'a>, span: Span) -> MathItem<'a> {
        assert!(text.graphemes(true).count() == 1);

        let c = text.chars().next().unwrap();

        let limits = Limits::for_char(c);
        let class = styles
            .get(EquationElem::class)
            .or_else(|| default_math_class(c))
            .unwrap_or(MathClass::Normal);

        let kind = MathKind::Glyph(Self { text, stretch: None, mid_stretched: None });
        let props = MathProperties::default(styles)
            .with_limits(limits)
            .with_class(class)
            .with_span(span);
        MathComponent { kind, props }.into()
    }
}

#[derive(Debug, Clone)]
pub struct BoxItem<'a> {
    pub elem: &'a Packed<BoxElem>,
}

impl<'a> BoxItem<'a> {
    pub fn new(elem: &'a Packed<BoxElem>, styles: StyleChain<'a>) -> MathItem<'a> {
        let kind = MathKind::Box(Self { elem });
        let props = MathProperties::default(styles).with_spaced(true);
        MathComponent { kind, props }.into()
    }
}

#[derive(Debug, Clone)]
pub struct ExternalItem<'a> {
    pub content: &'a Content,
}

impl<'a> ExternalItem<'a> {
    pub fn new(content: &'a Content, styles: StyleChain<'a>) -> MathItem<'a> {
        let kind = MathKind::External(Self { content });
        let props = MathProperties::default(styles)
            .with_spaced(true)
            .with_ignorant(content.is::<PlaceElem>());
        MathComponent { kind, props }.into()
    }
}
