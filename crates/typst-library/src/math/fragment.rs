use comemo::{Track, Tracked, TrackedMut};
use unicode_math_class::MathClass;

use super::*;
use crate::World;
use crate::diag::{At, SourceResult};
use crate::engine::{Engine, Route, Sink, Traced};
use crate::foundations::{
    Content, Module, NativeElement, Resolve, Scope, StyleChain, elem,
};
use crate::introspection::{
    Introspector, Locator, LocatorLink, SplitLocator, Tag, TagElem,
};
use crate::layout::{Abs, Em, HElem, PlaceElem, Rel, Spacing};
use crate::routines::{Arenas, Pair, RealizationKind, Routines};
use crate::text::{LinebreakElem, SpaceElem, TextElem};

pub trait Collapse<'a> {
    fn collapse(self) -> Option<MathNode<'a>>;
}

impl<'a> Collapse<'a> for Vec<MathNode<'a>> {
    fn collapse(mut self) -> Option<MathNode<'a>> {
        match self.len() {
            0 => None,
            1 => self.pop(),
            _ => Some(
                MathGroup { children: self, properties: MathProperties::new() }.into(),
            ),
        }
    }
}

pub trait PushIf<T> {
    fn push_if(&mut self, value: Option<T>);
}

impl<T> PushIf<T> for Vec<T> {
    fn push_if(&mut self, value: Option<T>) {
        if let Some(value) = value {
            self.push(value);
        }
    }
}

#[derive(Debug, Clone)]
pub struct MathProperties {
    pub class: Option<MathClass>,
    pub size: Option<MathSize>,
    pub limits: Option<Limits>,
    pub stretch: Option<Rel<Abs>>,
}

impl MathProperties {
    pub fn new() -> MathProperties {
        MathProperties {
            class: None,
            size: None,
            limits: None,
            stretch: None,
        }
    }
}

/// Produce math nodes from content.
pub fn math_nodes<'a>(
    engine: &mut Engine,
    locator: &mut SplitLocator,
    arenas: &'a Arenas,
    content: &'a Content,
    styles: StyleChain<'a>,
) -> SourceResult<Vec<MathNode<'a>>> {
    let children = (engine.routines.realize)(
        RealizationKind::Math,
        engine,
        locator,
        arenas,
        content,
        styles,
    )?;

    let mut output = Vec::new();
    for (child, styles) in children {
        handle(engine, locator, arenas, child, styles, &mut output)?;
    }
    Ok(output)
}

/// Convert one element into math node(s).
fn handle<'a>(
    engine: &mut Engine,
    locator: &mut SplitLocator,
    arenas: &'a Arenas,
    child: &'a Content,
    styles: StyleChain<'a>,
    output: &mut Vec<MathNode<'a>>,
) -> SourceResult<()> {
    if child.is::<AlignPointElem>() {
        output.push(MathNode::Align);
    } else if child.is::<LinebreakElem>() {
        output.push(MathNode::Linebreak);
    } else if let Some(elem) = child.to_packed::<TagElem>() {
        output.push(MathNode::Tag(&elem.tag));
    } else if let Some(elem) = child.to_packed::<HElem>() {
        if let Spacing::Rel(rel) = elem.amount
            && rel.rel.is_zero()
        {
            output
                .push(MathNode::Spacing(rel.abs.resolve(styles), elem.weak.get(styles)));
        }
    } else if child.is::<SpaceElem>() {
        output.push(MathNode::Space(styles));
    } else if let Some(elem) = child.to_packed::<ScriptsElem>() {
        output.push_if(
            math_nodes(engine, locator, arenas, &elem.body, styles)?
                .collapse()
                .map(|node| node.with_limits(Limits::Never)),
        );
    } else if let Some(elem) = child.to_packed::<LimitsElem>() {
        let limits =
            if elem.inline.get(styles) { Limits::Always } else { Limits::Display };
        output.push_if(
            math_nodes(engine, locator, arenas, &elem.body, styles)?
                .collapse()
                .map(|node| node.with_limits(limits)),
        );
    } else if let Some(elem) = child.to_packed::<ClassElem>() {
        output.push_if(
            math_nodes(engine, locator, arenas, &elem.body, styles)?
                .collapse()
                .map(|node| {
                    node.with_class(elem.class).with_limits(Limits::for_class(elem.class))
                }),
        );
    } else if let Some(elem) = child.to_packed::<OpElem>() {
        let limits =
            if elem.limits.get(styles) { Limits::Display } else { Limits::Never };
        output.push_if(
            math_nodes(engine, locator, arenas, &elem.text, styles)?
                .collapse()
                .map(|node| node.with_class(MathClass::Large).with_limits(limits)),
        );
    } else if let Some(elem) = child.to_packed::<LrElem>()
        && let Some(lr) = extract_from_eq(&elem.body).to_packed::<LrElem>()
        && !lr.size.is_set()
    {
        // Collapse inner element created automatically.
        let mut new = LrElem::new(lr.body.clone());
        if elem.size.is_set() {
            new.size = elem.size.clone();
        };
        output.push(
            MathItem {
                content: arenas.content.alloc(new.pack().spanned(elem.span())),
                styles,
                properties: MathProperties::new(),
            }
            .into(),
        );
    } else if let Some(elem) = child.to_packed::<AttachElem>()
        && let Some(attach) = extract_from_eq(&elem.base).to_packed::<AttachElem>()
    {
        // Merge nested attachments where possible.
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
        output.push(
            MathItem {
                content: arenas.content.alloc(new.pack().spanned(elem.span())),
                styles,
                properties: MathProperties::new(),
            }
            .into(),
        );
    } else {
        output.push(
            MathItem {
                content: &child,
                styles,
                properties: MathProperties::new(),
            }
            .into(),
        );
    }
    Ok(())
}

/// Performs some basic processing on the pairs returned from realization.
pub fn prepare<'a>(arenas: &'a Arenas, children: &mut Vec<Pair<'a>>) {
    for (child, _styles) in children.iter_mut() {
        if let Some(elem) = child.to_packed::<LrElem>()
            && let Some(lr) = extract_from_eq(&elem.body).to_packed::<LrElem>()
            && !lr.size.is_set()
        {
            // Collapse inner element created automatically.
            let mut new = LrElem::new(lr.body.clone());
            if elem.size.is_set() {
                new.size = elem.size.clone();
            };
            *child = arenas.content.alloc(new.pack().spanned(elem.span()));
        } else if let Some(elem) = child.to_packed::<AttachElem>()
            && let Some(attach) = extract_from_eq(&elem.base).to_packed::<AttachElem>()
        {
            // Merge nested attachments where possible.
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
