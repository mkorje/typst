use crate::engine::Engine;
use crate::math::ir::resolve::MathBuilder;

pub use crate::math::ir::run::MathRun;

mod item;
mod resolve;
mod run;

pub fn create<'a>(
    elem: &'a Packed<EquationElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
) -> SourceResult<MathRun<'a>> {
    let mut locator = locator.split();
    let arenas = Arenas::default();
    let mut builder = MathBuilder::new(engine, &mut locator, arenas);
    builder.build(&elem.body, styles)
}
