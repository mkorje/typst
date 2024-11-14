// maybe 0.25ex? - this is in the accent latex package.
// Note: U+030B is the one accent in the list of supported accents (see
// typst-library) which doesn't have the Diacritic class (in fact, it
// doesn't have any class)
// limitations - can't pass style functions as it creates styled elem...

#set page(width: auto, height: auto, margin: 0.5cm)

--- math-accent-arbitrary ---
// Test arbitrary characters as accents.
$accent(u, 1)_3^bold(k), hat(accent(f, 𝑥)), accent(f, ast.triple)_1^1, accent(x, i), accent(text(#blue, sans(i)), text(#red, ast)), accent(upright(j), text(#purple, star)), Z^(accent(x, i))$
$ accent(Lambda, diamond.stroked.dot),
  accent(Lambda, text(#red, frak(dotless.i))),
  accent(Lambda, triangle.stroked.small.r),
  Z^(accent(Lambda, triangle.stroked.small.r)^2),
  attach(accent(x, a b c), t: a),
  attach(limits(accent(u, bold(u))), t: frak(u), tl: sans(u), tr: upright(u)) $

--- math-accent-stacked ---
// Test stacked accents.
$hat(accent(accent(accent(x, text(#green, star)), text(#teal, grave)), text(#maroon, lozenge.stroked.small))),$
$accent(accent(accent(v, text(#blue, <-)), text(#red, ->)), text(#green, arrow.l.r))$

$dot(x) Z^dot(x) Z^Z^dot(x)$
$Z^accent(x, i) #move($accent(x, i)$, dy: -0.3em, dx: -0.5em), Z^Z^accent(x,i)$

$sscript(accent(x, i)) accent(x, i)$


$accent(f, x) Z^accent(f, x) Z^Z^accent(f, x)$
