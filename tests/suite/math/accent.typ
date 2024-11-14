// Test math accents.

--- math-accent-sym-call ---
// Test function call.
$grave(a), acute(b), hat(f), tilde(§), macron(ä), diaer(a), ä \
 breve(\&), dot(!), circle(a), caron(@), arrow(Z), arrow.l(Z)$

--- math-accent-align ---
$ x &= p \ dot(x) &= v \ dot.double(x) &= a \ dot.triple(x) &= j \ dot.quad(x) &= s $

--- math-accent-func ---
// Test `accent` function.
$accent(ö, .), accent(v, <-), accent(ZZ, \u{0303})$

--- math-accent-bounds ---
// Test accent bounds.
$sqrt(tilde(T)) + hat(f)/hat(g)$

--- math-accent-wide-base ---
// Test wide base.
$arrow("ABC" + d), tilde(sum)$

--- math-accent-superscript ---
// Test effect of accent on superscript.
$A^x != hat(A)^x != hat(hat(A))^x$

--- math-accent-high-base ---
// Test high base.
$ tilde(integral), tilde(integral)_a^b, tilde(integral_a^b) $

--- math-accent-sized ---
// Test accent size.
$tilde(sum), tilde(sum, size: #50%), accent(H, hat, size: #200%)$

--- math-accent-sized-script ---
// Test accent size in script size.
$tilde(U, size: #1.1em), x^tilde(U, size: #1.1em), sscript(tilde(U, size: #1.1em))$

--- math-accent-dotless ---
// Test dotless glyph variants.
#let test(c) = $grave(#c), acute(sans(#c)), hat(frak(#c)), tilde(mono(#c)),
  macron(bb(#c)), dot(cal(#c)), diaer(upright(#c)), breve(bold(#c)),
  circle(bold(upright(#c))), caron(upright(sans(#c))), arrow(bold(frak(#c)))$
$test(i) \ test(j)$

--- math-accent-colored ---
// Test colored accents.
$text(#blue, tilde(v)), accent(text(#yellow, x), text(#green, hat)),
 accent(ö, text(#eastern, .)), accent(y, text(#red, ->))$

--- math-accent-arbitrary ---
// Test arbitrary characters as accents.
$accent(u, 1)_3^bold(k) = 0, hat(accent(f, 𝑥)), accent(f, ast.triple)_1^1, accent(x, i), accent(text(#blue, sans(i)), text(#red, ast)), accent(upright(j), text(#purple, star))$
$ accent(Lambda, diamond.stroked.dot), accent(Lambda, text(#red, frak(dotless.i))),
  Z^(accent(Lambda, triangle.stroked.small.r)^2) attach(accent(x, a b c), t: a) accent(u, bold(u)) $

--- math-accent-stacked ---
// Test stacked accents.
$hat(
  accent(
    accent(
      accent(x, text(#green, star))
      , text(#teal, grave)
    ), text(#maroon, lozenge.stroked.small)
  )),
accent(accent(accent(v, text(#blue, <-)), text(#red, ->)), text(#green, arrow.l.r))$
