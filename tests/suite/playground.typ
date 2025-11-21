--- playground paged ---
#set page(width: auto, height: auto, margin: 1em)
// Test styling dotless i and j.
$
  dotless.i dotless.j,
  upright(dotless.i) upright(dotless.j),
  sans(dotless.i) sans(dotless.j),
  bold(dotless.i) bold(dotless.j),
  bb(dotless.i) bb(dotless.j),
  cal(dotless.i) cal(dotless.j),
  frak(dotless.i) frak(dotless.j),
  mono(dotless.i) mono(dotless.j),
  bold(frak(dotless.i)) upright(sans(dotless.j)),
  italic(bb(dotless.i)) frak(sans(dotless.j))
$

$
  upright(cal(i)) cal(upright(i)) upright(cal(dotless.i)) cal(upright(dotless.i))
$

// Test dotless glyph variants.
#let test(c) = {
  $grave(#c), acute(sans(#c)), hat(frak(#c)), tilde(mono(#c)),
  macron(bb(#c)), dot(cal(#c)), diaer(upright(#c)), breve(bold(#c)),
  circle(bold(upright(#c))), caron(upright(sans(#c))), arrow(bold(frak(#c)))$
}
$test(i) \ test(j)$

#show math.equation: set text(font: "STIX Two Math")

$bold(dotless.i), bold(dotless.j)$

$bold(i), bold(j)$

// Test styling dotless i and j.
$
  dotless.i dotless.j,
  upright(dotless.i) upright(dotless.j),
  sans(dotless.i) sans(dotless.j),
  bold(dotless.i) bold(dotless.j),
  bb(dotless.i) bb(dotless.j),
  cal(dotless.i) cal(dotless.j),
  frak(dotless.i) frak(dotless.j),
  mono(dotless.i) mono(dotless.j),
  bold(frak(dotless.i)) upright(sans(dotless.j)),
  italic(bb(dotless.i)) frak(sans(dotless.j))
$

// Test dotless glyph variants.
#let test(c) = {
  $grave(#c), acute(sans(#c)), hat(frak(#c)), tilde(mono(#c)),
  macron(bb(#c)), dot(cal(#c)), diaer(upright(#c)), breve(bold(#c)),
  circle(bold(upright(#c))), caron(upright(sans(#c))), arrow(bold(frak(#c)))$
}
$test(i) \ test(j)$

