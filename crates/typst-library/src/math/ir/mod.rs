//! Intermediate representation for math layout.
//!
//! This module provides a two-phase approach to math layout:
//!
//! 1. **Resolution phase** ([`resolve_equation`]): Converts the content tree
//!    into a flat intermediate representation ([`MathItem`]) that captures all
//!    the information needed for layout. This phase handles style resolution,
//!    spacing computation, and structural transformations.
//!
//! 2. **Layout phase** (in `typst-layout`): Takes the resolved [`MathItem`]s
//!    and produces the final positioned frames.
//!
//! This separation allows for cleaner code organization and enables future
//! optimizations like caching the resolved representation.

pub use crate::math::ir::item::*;
pub use crate::math::ir::resolve::resolve_equation;

mod item;
mod resolve;
