//! Shallow intermediate representation for math.
//!
//! The math IR is a flat `Vec<MathChild>` produced by the [`Collector`].
//! Structural children carry unresolved [`Body`] handles (content + style
//! chain); recursion into bodies happens lazily during layout, mirroring how
//! flow's collector returns a `Vec<Child>` that nested elements only realize
//! when laid out.

mod collect;
mod item;
mod multiline;

pub use self::collect::collect;
pub use self::item::*;
pub use self::multiline::{FencedView, MultilineView, split_columns, split_rows};

use crate::diag::SourceResult;
use crate::engine::Engine;
use crate::foundations::{Packed, StyleChain};
use crate::introspection::Locator;
use crate::math::EquationElem;
use crate::routines::Arenas;

/// Collects an equation's body into a flat sequence of `MathChild`s.
///
/// This is the entry point that both `typst-layout` and `typst-html` call.
/// Both then walk the returned children, recursively collecting bodies as
/// they go.
#[typst_macros::time(name = "math ir collection")]
pub fn collect_equation<'a>(
    elem: &'a Packed<EquationElem>,
    engine: &mut Engine,
    locator: Locator<'a>,
    arenas: &'a Arenas,
    styles: StyleChain<'a>,
) -> SourceResult<Vec<MathChild<'a>>> {
    collect(&elem.body, styles, engine, locator, arenas)
}
