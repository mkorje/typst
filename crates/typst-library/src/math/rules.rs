//! Desugaring show rules for math elements.
//!
//! These rules collapse "syntactic sugar" math elements (operators,
//! over/underbraces, etc.) into combinations of the core primitives
//! (`AccentElem`, `AttachElem`, `ClassElem`, `LimitsElem`, `ScriptsElem`,
//! `SymbolElem`). They are target-agnostic — both `typst-layout` (paged)
//! and `typst-html` register them — so each target only ever sees the
//! desugared primitives in its math IR.
//!
//! Compare the old IR resolvers `resolve_op` and `resolve_underoverspreader`
//! and friends, which previously did this work inline at IR-resolution
//! time, redundantly for each target.

use unicode_math_class::MathClass;

use crate::foundations::{NativeElement, ShowFn};
use crate::math::{
    Accent, AccentElem, AttachElem, ClassElem, LimitsElem, OpElem,
    OverbraceElem, OverbracketElem, OverparenElem, OvershellElem, ScriptsElem,
    UnderbraceElem, UnderbracketElem, UnderparenElem, UndershellElem,
};

// ---------------------------------------------------------------------------
// Operators
// ---------------------------------------------------------------------------

/// `op(text, limits: bool)` desugars to:
///
/// ```ignore
/// class(Large, body: limits(inline: false, body: text))    // limits=true
/// class(Large, body: scripts(body: text))                  // limits=false
/// ```
pub const OP_RULE: ShowFn<OpElem> = |elem, _engine, styles| {
    let body = elem.text.clone();
    let limited = if elem.limits.get(styles) {
        LimitsElem::new(body).pack()
    } else {
        ScriptsElem::new(body).pack()
    };
    Ok(ClassElem::new(MathClass::Large, limited).pack())
};

// ---------------------------------------------------------------------------
// Under/over-spreaders
// ---------------------------------------------------------------------------

/// Helper for the eight spreader elements. Each desugars to:
///
/// - without annotation: `accent(body, char)`
/// - with annotation, position=Below:
///   `attach(limits(inline: true, body: accent(body, char)), b: annotation)`
/// - with annotation, position=Above:
///   `attach(limits(inline: true, body: accent(body, char)), t: annotation)`
///
/// The `LimitsElem(inline: true)` wrap forces always-limits placement on
/// the attachment so the annotation lands directly above/below — matching
/// what the old IR's `ScriptsItem` did unconditionally for these
/// constructs.
fn spread(
    body: typst_library::foundations::Content,
    annotation: Option<typst_library::foundations::Content>,
    accent_char: char,
    above: bool,
) -> typst_library::foundations::Content {
    let accent = AccentElem::new(body, Accent(accent_char)).pack();
    match annotation {
        None => accent,
        Some(ann) => {
            let limited = LimitsElem::new(accent).with_inline(true).pack();
            let mut attach = AttachElem::new(limited);
            if above {
                attach = attach.with_t(Some(ann));
            } else {
                attach = attach.with_b(Some(ann));
            }
            attach.pack()
        }
    }
}

pub const UNDERBRACE_RULE: ShowFn<UnderbraceElem> = |elem, _engine, styles| {
    Ok(spread(elem.body.clone(), elem.annotation.get_cloned(styles), '⏟', false))
};

pub const OVERBRACE_RULE: ShowFn<OverbraceElem> = |elem, _engine, styles| {
    Ok(spread(elem.body.clone(), elem.annotation.get_cloned(styles), '⏞', true))
};

pub const UNDERBRACKET_RULE: ShowFn<UnderbracketElem> = |elem, _engine, styles| {
    Ok(spread(elem.body.clone(), elem.annotation.get_cloned(styles), '⎵', false))
};

pub const OVERBRACKET_RULE: ShowFn<OverbracketElem> = |elem, _engine, styles| {
    Ok(spread(elem.body.clone(), elem.annotation.get_cloned(styles), '⎴', true))
};

pub const UNDERPAREN_RULE: ShowFn<UnderparenElem> = |elem, _engine, styles| {
    Ok(spread(elem.body.clone(), elem.annotation.get_cloned(styles), '⏝', false))
};

pub const OVERPAREN_RULE: ShowFn<OverparenElem> = |elem, _engine, styles| {
    Ok(spread(elem.body.clone(), elem.annotation.get_cloned(styles), '⏜', true))
};

pub const UNDERSHELL_RULE: ShowFn<UndershellElem> = |elem, _engine, styles| {
    Ok(spread(elem.body.clone(), elem.annotation.get_cloned(styles), '⏡', false))
};

pub const OVERSHELL_RULE: ShowFn<OvershellElem> = |elem, _engine, styles| {
    Ok(spread(elem.body.clone(), elem.annotation.get_cloned(styles), '⏠', true))
};

// ---------------------------------------------------------------------------
// Registration
// ---------------------------------------------------------------------------

/// Register the math desugaring show rules for the given target.
///
/// Called by both `typst-layout` (for [`Target::Paged`]) and `typst-html`
/// (for [`Target::Html`]) so the same desugarings run in either pipeline.
pub fn register(
    rules: &mut crate::foundations::NativeRuleMap,
    target: crate::foundations::Target,
) {
    rules.register(target, OP_RULE);
    rules.register(target, UNDERBRACE_RULE);
    rules.register(target, OVERBRACE_RULE);
    rules.register(target, UNDERBRACKET_RULE);
    rules.register(target, OVERBRACKET_RULE);
    rules.register(target, UNDERPAREN_RULE);
    rules.register(target, OVERPAREN_RULE);
    rules.register(target, UNDERSHELL_RULE);
    rules.register(target, OVERSHELL_RULE);
}
