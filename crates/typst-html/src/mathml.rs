//! MathML output for equations.
//!
//! Skeleton: this walks the new shallow [`MathChild`] IR and produces a
//! sequence of MathML `Content` nodes. The original (~1300 LoC) walker
//! over the recursive `MathItem` tree has been removed — it would need a
//! full port to the new IR. The structural shape lives here.
//!
//! The recursion strategy mirrors `typst-layout`: structural children
//! carry unresolved `Body<'a>` handles; this walker calls
//! [`typst_library::math::ir::collect`] to recurse into them.

use ecow::EcoString;
use typst_assets::mathml::*;
use typst_library::diag::SourceResult;
use typst_library::engine::Engine;
use typst_library::foundations::{Content, NativeElement, StyleChain};
use typst_library::introspection::{Locator, SplitLocator};
use typst_library::math::ir::{Body, MathChild, collect};
use typst_library::routines::Arenas;
use unicode_math_class::MathClass;

use crate::HtmlElem;
use crate::tag::mathml as tag;

const SPACE_WIDTH: typst_library::layout::Em =
    typst_library::layout::Em::new(4.0 / 18.0);

/// CSS styles that we attach to MathML output to nudge browsers toward
/// Typst's paged rendering. Re-exported so `document.rs` can inject it
/// into `<head>` for pages with equations.
///
/// The actual stylesheet would normally live here; for the skeleton, an
/// empty placeholder.
pub(crate) static EQUATION_CSS_STYLES: std::sync::LazyLock<EcoString> =
    std::sync::LazyLock::new(EcoString::new);

/// Convert a math IR sequence into a sequence of MathML nodes.
///
/// This is the new entry point — it takes the collected `Vec<MathChild>`
/// (rather than the old recursive `MathItem`) and walks it, recursing into
/// deferred bodies via [`collect`].
pub(crate) fn convert_math_to_nodes<'a>(
    children: &[MathChild<'a>],
    arenas: &'a Arenas,
    engine: &mut Engine,
    styles: StyleChain<'a>,
    block: bool,
) -> SourceResult<Vec<Content>> {
    let _ = SPACE_WIDTH;
    let mut ctx = MathmlContext::new(engine, arenas, styles, block);
    ctx.handle_children(children)
}

pub(crate) struct MathmlContext<'a, 'v, 'e> {
    engine: &'v mut Engine<'e>,
    arenas: &'a Arenas,
    locator: SplitLocator<'a>,
    styles: StyleChain<'a>,
    block: bool,
}

impl<'a, 'v, 'e> MathmlContext<'a, 'v, 'e> {
    pub fn new(
        engine: &'v mut Engine<'e>,
        arenas: &'a Arenas,
        styles: StyleChain<'a>,
        block: bool,
    ) -> Self {
        // Synthesize a top-level Locator the same way the equation rule
        // did before; the recursive collects fork off this via
        // `locator.next(span)`.
        let dummy_loc = Locator::root().split();
        Self {
            engine,
            arenas,
            locator: dummy_loc,
            styles,
            block,
        }
    }

    /// Walk a flat sequence and convert each child to MathML nodes.
    pub fn handle_children(
        &mut self,
        children: &[MathChild<'a>],
    ) -> SourceResult<Vec<Content>> {
        let mut nodes = Vec::with_capacity(children.len());
        for child in children {
            nodes.extend(self.handle_child(child)?);
        }
        Ok(nodes)
    }

    /// Resolve a deferred body, then convert it to nodes.
    ///
    /// This is the MathML side's recursion point — analogue of
    /// `typst-layout::math::MathContext::layout_body`.
    fn handle_body(&mut self, body: Body<'a>) -> SourceResult<Vec<Content>> {
        let span = body.content.span();
        let loc = self.locator.next(&span);
        let children =
            collect(body.content, body.styles, self.engine, loc, self.arenas)?;
        let prev = std::mem::replace(&mut self.styles, body.styles);
        let nodes = self.handle_children(&children);
        self.styles = prev;
        nodes
    }

    /// Dispatch a single child.
    ///
    /// As with the layout side: most variants are `todo!()` until the
    /// MathML-specific output is ported. The dispatch shape and the
    /// recursion pattern are what matter for the structural sketch.
    fn handle_child(&mut self, child: &MathChild<'a>) -> SourceResult<Vec<Content>> {
        match child {
            // Markers and tags: drop at this level. (Multiline/aligned
            // emission is done by the caller after splitting on them.)
            MathChild::Linebreak | MathChild::Align | MathChild::Mid(_)
            | MathChild::Tag(_) => Ok(vec![]),

            // Simple leaves -> trivial mo/mi/mn elements.
            MathChild::Space => Ok(vec![]),
            MathChild::Spacing(_, _, _) => todo!("emit <mspace>"),
            MathChild::Glyph(g) => Ok(vec![mi_or_mo(&g.text, g.class)]),
            MathChild::Text(t) => Ok(vec![mi(&t.text)]),
            MathChild::Number(n) => Ok(vec![mn(&n.text)]),
            MathChild::Primes(_) => todo!("emit primes as superscripted <mo>"),

            // Pass-through inline content.
            MathChild::Box(_) | MathChild::External(_) => {
                todo!("embed inline content via <mtext> or foreign-object")
            }
            MathChild::Mathml(_) => todo!("re-emit pre-built MathML body"),

            // Structural wrappers: recurse via `handle_body`.
            MathChild::Group(g) => {
                let nodes = self.handle_body(g.body)?;
                Ok(vec![wrap_mrow(nodes)])
            }
            MathChild::Override(o) => {
                // Class/limits/stretch don't change the MathML tag, but
                // they may toggle attributes (`movablelimits`,
                // `stretchy`). For the skeleton, just recurse.
                let nodes = self.handle_body(o.body)?;
                Ok(vec![wrap_mrow(nodes)])
            }
            MathChild::Frac(frac) => {
                let num = self.handle_body(frac.num)?;
                let denom = self.handle_body(frac.denom)?;
                Ok(vec![
                    HtmlElem::new(tag::mfrac)
                        .with_body(Some(Content::sequence([
                            wrap_mrow(num),
                            wrap_mrow(denom),
                        ])))
                        .pack(),
                ])
            }
            MathChild::Binom(_) | MathChild::SkewedFrac(_) => {
                todo!("emit binomial / skewed-fraction MathML")
            }
            MathChild::Attach(attach) => {
                // The base is already collected; the rest are deferred.
                let base = self.handle_children(&attach.base)?;
                let _t = attach.t.map(|b| self.handle_body(b)).transpose()?;
                let _b = attach.b.map(|b| self.handle_body(b)).transpose()?;
                let _tl = attach.tl.map(|b| self.handle_body(b)).transpose()?;
                let _tr = attach.tr.map(|b| self.handle_body(b)).transpose()?;
                let _bl = attach.bl.map(|b| self.handle_body(b)).transpose()?;
                let _br = attach.br.map(|b| self.handle_body(b)).transpose()?;
                let _ = base;
                todo!("emit <msub>/<msup>/<msubsup>/<mmultiscripts>")
            }
            MathChild::Accent(_) => todo!("emit <mover>/<munder> with accent='true'"),
            MathChild::Line(_) => todo!("emit <mover>/<munder> with line"),
            MathChild::Cancel(_) => todo!("emit <menclose notation='updiagonalstrike'>"),
            MathChild::Root(_) => todo!("emit <msqrt>/<mroot>"),
            MathChild::Lr(lr) => {
                // Collect the body ourselves so we can spot the
                // delimiters; same trick as the paged layouter.
                let span = lr.body.content.span();
                let loc = self.locator.next(&span);
                let body_children = collect(
                    lr.body.content,
                    lr.body.styles,
                    self.engine,
                    loc,
                    self.arenas,
                )?;
                // For the skeleton, just wrap as mrow.
                let prev = std::mem::replace(&mut self.styles, lr.body.styles);
                let nodes = self.handle_children(&body_children);
                self.styles = prev;
                Ok(vec![wrap_mrow(nodes?)])
            }
            MathChild::Table(_) => todo!("emit <mtable>/<mtr>/<mtd>"),
        }
    }
}

// ===========================================================================
// MathML element builders
// ===========================================================================

fn mi(text: &EcoString) -> Content {
    HtmlElem::new(tag::mi)
        .with_body(Some(text_node(text)))
        .pack()
}

fn mn(text: &EcoString) -> Content {
    HtmlElem::new(tag::mn)
        .with_body(Some(text_node(text)))
        .pack()
}

fn mo(text: &EcoString) -> Content {
    HtmlElem::new(tag::mo)
        .with_body(Some(text_node(text)))
        .pack()
}

/// Choose between `<mi>` and `<mo>` based on math class.
fn mi_or_mo(text: &EcoString, class: MathClass) -> Content {
    match class {
        MathClass::Alphabetic | MathClass::Normal => mi(text),
        _ => mo(text),
    }
}

fn text_node(text: &EcoString) -> Content {
    typst_library::text::TextElem::packed(text.clone())
}

fn wrap_mrow(nodes: Vec<Content>) -> Content {
    if nodes.len() == 1 {
        nodes.into_iter().next().unwrap()
    } else {
        HtmlElem::new(tag::mrow)
            .with_body(Some(Content::sequence(nodes)))
            .pack()
    }
}
