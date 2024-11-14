#set page(width: auto, height: auto, margin: 1em)
#let test-accents = (
  sym.diamond.stroked,
  sym.diamond.stroked.medium,
  sym.diamond.stroked.small,
  sym.diamond.stroked.dot,
  sym.star.op,
  sym.star.filled,
  sym.star.stroked,
  sym.lozenge.stroked.small,
  sym.ast.triple,
  sym.ast.square,
  sym.triangle.stroked.small.r,
  sym.approx,
  sym.bot,
  sym.or.double,
  sym.Lambda,
  "1",
  "x",
  "f",
  sym.rho.alt,
  sym.hat,
  sym.arrow,
)

#for a in test-accents {
  $accent(Lambda, #a)$
}

#for a in test-accents {
  $accent(x, #a)$
}
