//! Mathematical formulas.

pub mod accent;
mod attach;
mod cancel;
mod equation;
mod frac;
mod lr;
mod matrix;
mod op;
mod root;
mod style;
mod underover;

pub use self::accent::{Accent, AccentElem};
pub use self::attach::*;
pub use self::cancel::*;
pub use self::equation::*;
pub use self::frac::*;
pub use self::lr::*;
pub use self::matrix::*;
pub use self::op::*;
pub use self::root::*;
pub use self::style::*;
pub use self::underover::*;

use typst_utils::singleton;
use unicode_math_class::MathClass;

use crate::foundations::{Content, Module, NativeElement, Scope, elem};
use crate::introspection::TagElem;
use crate::layout::{Em, HElem, PlaceElem};
use crate::routines::{Arenas, Pair};
use crate::text::{LinebreakElem, SpaceElem, TextElem};

// Spacings.
pub const THIN: Em = Em::new(1.0 / 6.0);
pub const MEDIUM: Em = Em::new(2.0 / 9.0);
pub const THICK: Em = Em::new(5.0 / 18.0);
pub const QUAD: Em = Em::new(1.0);
pub const WIDE: Em = Em::new(2.0);

/// Create a module with all math definitions.
pub fn module() -> Module {
    let mut math = Scope::deduplicating();
    math.start_category(crate::Category::Math);
    math.define_elem::<EquationElem>();
    math.define_elem::<TextElem>();
    math.define_elem::<LrElem>();
    math.define_elem::<MidElem>();
    math.define_elem::<AttachElem>();
    math.define_elem::<StretchElem>();
    math.define_elem::<ScriptsElem>();
    math.define_elem::<LimitsElem>();
    math.define_elem::<AccentElem>();
    math.define_elem::<UnderlineElem>();
    math.define_elem::<OverlineElem>();
    math.define_elem::<UnderbraceElem>();
    math.define_elem::<OverbraceElem>();
    math.define_elem::<UnderbracketElem>();
    math.define_elem::<OverbracketElem>();
    math.define_elem::<UnderparenElem>();
    math.define_elem::<OverparenElem>();
    math.define_elem::<UndershellElem>();
    math.define_elem::<OvershellElem>();
    math.define_elem::<CancelElem>();
    math.define_elem::<FracElem>();
    math.define_elem::<BinomElem>();
    math.define_elem::<VecElem>();
    math.define_elem::<MatElem>();
    math.define_elem::<CasesElem>();
    math.define_elem::<RootElem>();
    math.define_elem::<ClassElem>();
    math.define_elem::<OpElem>();
    math.define_elem::<PrimesElem>();
    math.define_func::<abs>();
    math.define_func::<norm>();
    math.define_func::<round>();
    math.define_func::<sqrt>();
    math.define_func::<upright>();
    math.define_func::<bold>();
    math.define_func::<italic>();
    math.define_func::<serif>();
    math.define_func::<sans>();
    math.define_func::<scr>();
    math.define_func::<cal>();
    math.define_func::<frak>();
    math.define_func::<mono>();
    math.define_func::<bb>();
    math.define_func::<display>();
    math.define_func::<inline>();
    math.define_func::<script>();
    math.define_func::<sscript>();

    // Text operators.
    op::define(&mut math);

    // Spacings.
    math.define("thin", HElem::new(THIN.into()).pack());
    math.define("med", HElem::new(MEDIUM.into()).pack());
    math.define("thick", HElem::new(THICK.into()).pack());
    math.define("quad", HElem::new(QUAD.into()).pack());
    math.define("wide", HElem::new(WIDE.into()).pack());

    // Symbols.
    crate::symbols::define_math(&mut math);

    Module::new("math", math)
}

/// Trait for recognizing math elements and auto-wrapping them in equations.
pub trait Mathy {}

/// A math alignment point: `&`, `&&`.
#[elem(title = "Alignment Point", Mathy)]
pub struct AlignPointElem {}

impl AlignPointElem {
    /// Get the globally shared alignment point element.
    pub fn shared() -> &'static Content {
        singleton!(Content, AlignPointElem::new().pack())
    }
}

/// Forced use of a certain math class.
///
/// This is useful to treat certain symbols as if they were of a different
/// class, e.g. to make a symbol behave like a relation. The class of a symbol
/// defines the way it is laid out, including spacing around it, and how its
/// scripts are attached by default. Note that the latter can always be
/// overridden using [`{limits}`](math.limits) and [`{scripts}`](math.scripts).
///
/// # Example
/// ```example
/// #let loves = math.class(
///   "relation",
///   sym.suit.heart,
/// )
///
/// $x loves y and y loves 5$
/// ```
#[elem(Mathy)]
pub struct ClassElem {
    /// The class to apply to the content.
    #[required]
    pub class: MathClass,

    /// The content to which the class is applied.
    #[required]
    pub body: Content,
}

/// Performs some basic processing on the pairs returned from realization.
pub fn prepare<'a>(arenas: &'a Arenas, children: &mut Vec<Pair<'a>>) {
    for (child, _styles) in children.iter_mut() {
        if let Some(elem) = child.to_packed::<LrElem>()
            && let Some(lr) = extract_from_eq(&elem.body).to_packed::<LrElem>()
            && !lr.size.is_set()
        {
            let mut new = LrElem::new(lr.body.clone());
            if elem.size.is_set() {
                new.size = elem.size.clone();
            };
            *child = arenas.content.alloc(new.pack().spanned(elem.span()));
        } else if let Some(elem) = child.to_packed::<AttachElem>()
            && let Some(attach) = extract_from_eq(&elem.base).to_packed::<AttachElem>()
        {
            let mut new = AttachElem::new(Content::empty());
            let mut base = attach.clone();

            macro_rules! merge {
                ($content:ident) => {
                    if elem.$content.is_set() {
                        if !base.$content.is_set() {
                            base.$content = elem.$content.clone();
                        } else {
                            new.$content = elem.$content.clone();
                        }
                    }
                };
            }

            merge!(t);
            merge!(b);
            merge!(tl);
            merge!(tr);
            merge!(bl);
            merge!(br);

            new.base = base.pack().spanned(attach.span());
            *child = arenas.content.alloc(new.pack().spanned(elem.span()));
        }
    }

    // println!("{:?}\n", children.iter().map(|x| x.0).collect::<Vec<_>>());
    /*let mut last = None;
    let mut space = None;
    for (child, styles) in children {
        // Keep space only if supported by spaced fragments.
        if child.is::<SpaceElem>() {
            if last.is_some() {
                space = Some(child);
            }
            continue;
        } else if let Some(elem) = child.to_packed::<HElem>() {
            last = None;
            space = None;

            // if let Spacing::Rel(rel) = elem.amount
            //     && rel.rel.is_zero()
            // {
            //     ctx.push(MathFragment::Spacing(rel.abs.resolve(styles), elem.weak.get(styles)));
            // }
            if elem.weak.get(*styles) {
                // check prev child
                // if None, continue
                // if weak spacing, then set prev child's spacing width to max with elem's width
                // else do nothing.
            }

            // push it
            continue;
        } else if child.is::<AlignPointElem>() {
            // push it
            continue;
        } else if child.is::<LinebreakElem>() {
            //push it
            space = None;
            last = None;
            continue;
        }

        // Convert variable operators to binary operators if something precedes
        // them and they are not preceded by a operator or comparator.
        // check if child's class is Vary and if
        // if true, then set the child's class to binary.

        // Insert spacing between the last and this non-ignornat item.
        if !child.is::<TagElem>() && !child.is::<PlaceElem>() {
            // if last is not none and spacing returns something
            // spacing(last, space.take(), child)
            // then insert after last the returned spacing.

            // last = set to index of current child.
        }
    }

    // pop last child if it was weak spacing
    children.pop_if(|(child, styles)| {
        child
            .to_packed::<HElem>()
            .map(|elem| elem.weak.get(*styles))
            .unwrap_or(false)
    });*/
}

/// Recursively extracts the body while the content is an [`EquationElem`].
fn extract_from_eq(content: &Content) -> &Content {
    let mut body = content;
    while let Some(equation) = body.to_packed::<EquationElem>() {
        body = &equation.body;
    }
    body
}
