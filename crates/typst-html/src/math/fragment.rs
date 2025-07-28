/// Produce HTML nodes from content.
#[typst_macros::time(name = "mathml fragment")]
pub fn mathml_fragment(
    engine: &mut Engine,
    content: &Content,
    locator: Locator,
    styles: StyleChain,
) -> SourceResult<Vec<HtmlNode>> {
    mathml_fragment_impl(
        engine.routines,
        engine.world,
        engine.introspector,
        engine.traced,
        TrackedMut::reborrow_mut(&mut engine.sink),
        engine.route.track(),
        content,
        locator.track(),
        styles,
    )
}

/// The cached, internal implementation of [`mathml_fragment`].
#[comemo::memoize]
#[allow(clippy::too_many_arguments)]
fn mathml_fragment_impl(
    routines: &Routines,
    world: Tracked<dyn World + '_>,
    introspector: Tracked<Introspector>,
    traced: Tracked<Traced>,
    sink: TrackedMut<Sink>,
    route: Tracked<Route>,
    content: &Content,
    locator: Tracked<Locator>,
    styles: StyleChain,
) -> SourceResult<Vec<HtmlNode>> {
    let link = LocatorLink::new(locator);
    let mut locator = Locator::link(&link).split();
    let mut engine = Engine {
        routines,
        world,
        introspector,
        traced,
        sink,
        route: Route::extend(route),
    };

    engine.route.check_html_depth().at(content.span())?;

    let arenas = Arenas::default();
    let children = (engine.routines.realize)(
        RealizationKind::Math,
        &mut engine,
        &mut locator,
        &arenas,
        content,
        styles,
    )?;

    let mut nodes = Vec::new();
}

#[derive(Debug)]
pub enum MathNode<'a> {
    Align,
    Linebreak,
    Tag(&'a Tag),
    Spacing(Abs, bool),
    Space(StyleChain<'a>),
    Item(MathItem<'a>),
    Group(MathGroup<'a>),
}

impl<'a> From<MathItem<'a>> for MathNode<'a> {
    fn from(item: MathItem<'a>) -> Self {
        Self::Item(item)
    }
}

impl<'a> From<MathGroup<'a>> for MathNode<'a> {
    fn from(group: MathGroup<'a>) -> Self {
        Self::Group(group)
    }
}

impl MathNode<'_> {
    pub fn with_limits(mut self, limits: Limits) -> Self {
        match self {
            MathNode::Item(ref mut item) => item.properties.limits = Some(limits),
            MathNode::Group(ref mut group) => group.properties.limits = Some(limits),
            _ => {}
        };
        self
    }

    pub fn with_class(mut self, class: MathClass) -> Self {
        match self {
            MathNode::Item(ref mut item) => item.properties.class = Some(class),
            MathNode::Group(ref mut group) => group.properties.class = Some(class),
            _ => {}
        };
        self
    }
}

#[derive(Debug)]
pub struct MathItem<'a> {
    pub content: &'a Content,
    pub styles: StyleChain<'a>,
    pub properties: MathProperties,
}

#[derive(Debug)]
pub struct MathGroup<'a> {
    pub children: Vec<MathNode<'a>>,
    pub properties: MathProperties,
}
