use ecow::eco_format;
use typst_library::diag::{warning, At, SourceResult};
use typst_library::foundations::{Content, NativeElement, Repr, Symbol, Value};
use typst_library::math::{
    AlignPointElem, AttachElem, EquationElem, FracElem, LrElem, PrimesElem, RootElem,
};
use typst_library::text::{LinebreakElem, SpaceElem, TextElem};
use typst_syntax::ast::{self, AstNode};

use crate::{Eval, Vm};

impl Eval for ast::Math<'_> {
    type Output = Content;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        eval_math(vm, &mut self.exprs())
        //     Ok(Content::sequence(
        //         self.exprs()
        //             .map(|expr| expr.eval_display(vm))
        //             .collect::<SourceResult<Vec<_>>>()?,
        //     ))
    }
}

/// Evaluate a stream of math.
fn eval_math<'a>(
    vm: &mut Vm,
    exprs: &mut impl Iterator<Item = ast::Expr<'a>>,
) -> SourceResult<Content> {
    let flow = vm.flow.take();
    let mut exprs = exprs.peekable();

    let mut seq = Vec::new();
    let mut line = Vec::with_capacity(exprs.size_hint().1.unwrap_or_default());

    while let Some(expr) = exprs.next() {
        match expr {
            ast::Expr::Linebreak(_) => {
                seq.push(Content::sequence(line.to_owned()));
                line.clear();
                // if exprs.peek().is_none() && seq.is_empty() {
                //     line.push(expr.eval(vm)?.display().spanned(expr.span()));
                //     seq.push(Content::sequence(line.to_owned()));
                // } else {
                //     seq.push(eq(&line));
                //     seq.push(expr.eval(vm)?.display().spanned(expr.span()));
                // }
                // line.clear();
            }
            ast::Expr::NoNumberMarker(_) => {
                // if exprs.peek().is_none() && seq.is_empty() {
                //     vm.engine.sink.warn(warning!(
                //         expr.span(),
                //         "do not number marker is not attached to an equation line";
                //         hint: "do not number markers only have an affect on equation lines",
                //     ));
                //     continue;
                // }

                
                // println!("{:?} - {:?}", seq, line);
                // if let Some(elem) = seq.iter_mut().rev().find(|node| node.is::<EquationElem>()) {
                //     if line.is_empty() || (line.len() == 1 && line[0] == *SpaceElem::shared()) {
                //         elem.to_packed_mut::<EquationElem>().unwrap().push_numbering(None);
                //         line.clear();
                //         continue;
                //     }

                //     if exprs.peek().is_none() && !line.is_empty() {
                //         seq.push(EquationElem::new(Content::sequence(line.to_owned()))
                //             .with_line(true)
                //             .with_numbering(None)
                //             .pack());
                //         line.clear();
                //         continue;
                //     }
                    
                // }
                // vm.engine.sink.warn(warning!(
                //     expr.span(),
                //     "do not number marker is not attached to an equation line";
                //     hint: "do not number markers only have an affect on equation lines",
                // ));
            }
            expr => match expr.eval(vm)? {
                Value::Label(label) => {
                    if exprs.peek().is_none() && seq.is_empty() {
                        vm.engine.sink.warn(warning!(
                            expr.span(),
                            "ignoring label `{}` attached to line in a single line equation",
                            label.repr();
                            hint: "place the label after the equation, outside of the dollar signs",
                        ));

                        seq.push(Content::sequence(line.to_owned()));
                    } else {
                        seq.push(eq(&line).labelled(label));
                        seq.push(LinebreakElem::shared().clone().spanned(expr.span()));
                    }
                    line.clear();
                }
                value => line.push(value.display().spanned(expr.span())),
            },
        }

        if vm.flow.is_some() {
            break;
        }
    }

    if flow.is_some() {
        vm.flow = flow;
    }

    println!("{:?}", seq);

    if seq.is_empty() {
        // Don't wrap a single line of math in an EquationElem.
        Ok(Content::sequence(line.clone()))
    } else {
        if !line.is_empty() {
            seq.push(eq(&line));
        }
        Ok(Content::sequence(seq))
    }
}

fn eq(line: &[Content]) -> Content {
    EquationElem::new(Content::sequence(line.to_owned()))
        .with_line(true)
        .pack()
}

impl Eval for ast::MathIdent<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        vm.scopes.get_in_math(&self).cloned().at(self.span())
    }
}

impl Eval for ast::MathShorthand<'_> {
    type Output = Value;

    fn eval(self, _: &mut Vm) -> SourceResult<Self::Output> {
        Ok(Value::Symbol(Symbol::single(self.get().into())))
    }
}

impl Eval for ast::MathAlignPoint<'_> {
    type Output = Content;

    fn eval(self, _: &mut Vm) -> SourceResult<Self::Output> {
        Ok(AlignPointElem::shared().clone())
    }
}

impl Eval for ast::MathDelimited<'_> {
    type Output = Content;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        let open = self.open().eval_display(vm)?;
        let body = self.body().eval(vm)?;
        let close = self.close().eval_display(vm)?;
        Ok(LrElem::new(open + body + close).pack())
    }
}

impl Eval for ast::MathAttach<'_> {
    type Output = Content;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        let base = self.base().eval_display(vm)?;
        let mut elem = AttachElem::new(base);

        if let Some(expr) = self.top() {
            elem.push_t(Some(expr.eval_display(vm)?));
        }

        // Always attach primes in scripts style (not limits style),
        // i.e. at the top-right corner.
        if let Some(primes) = self.primes() {
            elem.push_tr(Some(primes.eval(vm)?));
        }

        if let Some(expr) = self.bottom() {
            elem.push_b(Some(expr.eval_display(vm)?));
        }

        Ok(elem.pack())
    }
}

impl Eval for ast::MathPrimes<'_> {
    type Output = Content;

    fn eval(self, _: &mut Vm) -> SourceResult<Self::Output> {
        Ok(PrimesElem::new(self.count()).pack())
    }
}

impl Eval for ast::MathFrac<'_> {
    type Output = Content;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        let num = self.num().eval_display(vm)?;
        let denom = self.denom().eval_display(vm)?;
        Ok(FracElem::new(num, denom).pack())
    }
}

impl Eval for ast::MathRoot<'_> {
    type Output = Content;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        let index = self.index().map(|i| TextElem::packed(eco_format!("{i}")));
        let radicand = self.radicand().eval_display(vm)?;
        Ok(RootElem::new(radicand).with_index(index).pack())
    }
}

trait ExprExt {
    fn eval_display(&self, vm: &mut Vm) -> SourceResult<Content>;
}

impl ExprExt for ast::Expr<'_> {
    fn eval_display(&self, vm: &mut Vm) -> SourceResult<Content> {
        Ok(self.eval(vm)?.display().spanned(self.span()))
    }
}
