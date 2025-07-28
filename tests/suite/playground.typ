--- testing html ---
$ x = -1 $
vs
$ x - 1, class("unary", AA)x $

--- playground-test render html ---
Wow a paragraph
$ mat(1, 2; 3, 4) $
continuing on...
$ a + mat(delim: #none, 1, 2; 3, 4) + b $
blah

new par $x + z$, but
$ 10 $
Wowzers

$ 20 $
Test

Test paragraph.
New line!

$ x + y + z $

--- playground html ---

#let misc-tests = (
  [Vary class spacing],
  ```typ
  $ x = - 1 quad x - 1 $
  ```,
  [Left/right],
  ```typ
  $ (x/2 + z) $
  ```,
  [Text spacing],
  ```typ
  This $a "test" b$ or this $a"test" b$ or this $a "test"b$ or this $a"test"b$ end. \
  This $a "t" b$ or this $a"t" b$ or this $a "t"b$ or this $a"t"b$ end.
  ```,
  // Some from https://fred-wang.github.io/TeXZilla/
  [Ellipse Equation],
  ```typ
  $ x^2/a^2 + y^2/b^2 = 1 $
  ```,
)

// https://fred-wang.github.io/MathFonts/mozilla_mathml_test/?fontFamily=NewComputerModern
// https://github.com/fred-wang/MathFonts
#let mozilla-mathml-torture-test = (
  ```typ
  $ x^2 y^2 $
  ```,
  ```typ
  $ attach(F, bl: 2, br: 3) $
  ```,
  ```typ
  $ (x + y^2) / (k + 1) $
  ```,
  ```typ
  $ x + y^(2 / (k + 1)) $
  ```,
  ```typ
  $ a / (b \/ 2) $
  ```,
  ```typ
  $
    a_0 + display(
      1/(a_1 + display(
        1/(a_2 + display(
          1/(a_3 + display(
            1/a_4
          ))
        ))
      ))
    )
  $
  ```,
  ```typ
  $ a_0 + 1/(a_1 + 1/(a_2 + 1/(a_3 + 1/a_4))) $
  ```,
  ```typ
  $ binom(n, k \/ 2) $
  ```,
  ```typ
  $ binom(p, 2) x^2 y^(p - 2) - 1 / (1 - x) 1 / (1 - x^2) $
  ```,
  ```typ
  $ sum_(0 <= i <= m \ 0 < j < n) P(i, j) $
  ```,
  ```typ
  $ x^(2y) $
  ```,
  ```typ
  $ sum_(i = 1)^p sum_(j = 1)^q sum_(k = 1)^r a_(i j) b_(j k) c_(k i) $
  ```,
  ```typ
  $ sqrt(1 + sqrt(1 + sqrt(1 + sqrt(1 + sqrt(1 + sqrt(1 + sqrt(1 + x))))))) $
  ```,
  ```typ
  $
    (partial^2 / (partial x^2)
      + partial^2 / (partial y^2)) abs(phi(x + i y))^2 = 0
  $
  ```,
  ```typ
  $ 2^2^2^x $
  ```,
  ```typ
  $ integral_1^x (dif t) / t $
  ```,
  ```typ
  $ integral.double_D dif x dif y $
  ```,
  ```typ
  $
    f(x) = cases(
      1\/3 & "if" 0 <= x <= 1\;,
      2\/3 & "if" 3 <= x <= 4\;,
      0 & "elsewhere".
    )
  $
  ```,
  ```typ
  $ overbrace(x + dots.c + x, k "times") $
  ```,
  ```typ
  $ y_(x^2) $
  ```,
  ```typ
  $ sum_(p "prime") f(p) = integral_(t > 1) f(t) dif pi(t) $
  ```,
  ```typ
  $
    {underbrace(
      overbrace(a\, ...\, a, k a's)\, overbrace(b\, ...\, b, ell b's),
      k + ell "elements"
    )}
  $
  ```,
  ```typ
  $ mat(mat(a, b; c, d), mat(e, f; g, h); 0, mat(i, j; k, l)) $
  ```,
  ```typ
  $
    det mat(delim: \|,
      c_0, c_1, c_2, dots.c, c_n;
      c_1, c_2, c_3, dots.c, c_(n + 1);
      c_2, c_3, c_4, dots.c, c_(n + 2);
      dots.v, dots.v, dots.v, , dots.v;
      c_n, c_(n + 1), c_(n + 2), dots.c, c_(2n)
    ) > 0
  $
  ```,
  ```typ
  $ y_x_2 $
  ```,
  ```typ
  $ x_92^31415 + pi $
  ```,
  ```typ
  $ x^(z^d_c)_(y_b^a) $
  ```,
  ```typ
  $ y'''_3 $
  ```,
  ```typ
  $ lim_(n -> +oo) sqrt(2 pi n)/n! (n/e)^n = 1 $
  ```,
  ```typ
  $
    det(A) = sum_(sigma in S_n) epsilon.alt(sigma)
      product_(i = 1)^n a_(i, sigma_i)
  $
  ```,
)

// http://eyeasme.com/Joe/MathML/MathML_browser_test.html
#let joes-mathml-browser-test = (
  [Axiom of power set],
  ```typ
  $
    forall A thin exists P thin forall B thin [B in P <==> forall C thin (C in B => C in A )]
  $
  ```,
)

// From https://fred-wang.github.io/TeXZilla/
#let mathjax = (
  [The Lorenz Equations],
  ```typ
  $
    dot(x) & = sigma (y - x) \
    dot(y) & = rho x - y - x z \
    dot(z) & = -beta z + x y
  $
  ```,
  [The Cauchy-Schwarz Inequality],
  ```typ
  $
    (sum_(k = 1)^n a_k b_k)^2
      <= (sum_(k = 1)^n a_k^2) (sum_(k = 1)^n b_k^2)
  $
  ```,
  [A Cross Product Formula],
  ```typ
  #let bv(var) = $bold(upright(var))$
  $
    bv(V)_1 times bv(V)_2 = mat(delim: \|,
      bv(i), bv(j), bv(k);
      (partial X)/(partial u), (partial Y)/(partial u), 0;
      (partial X)/(partial v), (partial Y)/(partial v), 0;
    )
  $
  ```,
  [The probability of getting k heads when flipping n coins],
  ```typ
  $
    P(E) = binom(n, k) p^k (1 - p)^(n - k)
  $
  ```,
  [An Identity of Ramanujan],
  ```typ
  $
    1/((sqrt(phi.alt sqrt(5)) - phi.alt) e^(25/pi))
      = 1 + e^(-2pi)/(
          1 + e^(-4pi)/(
            1 + e^(-6pi)/(
              1 + e^(-8pi)/(
                1 + dots.c
              )
            )
          )
        )
  $
  ```,
  [A Rogers-Ramanujan Identity],
  ```typ
  $
    1 + q^2/(1 - q) + q^6/((1-q)(1-q^2)) + dots.c
      = product_(j = 0)^oo 1/((1 - q^(5j + 2))(1 - q^(5j + 3))),
      wide "for" quad abs(q) < 1.
  $
  ```,
  [Maxwell's Equations],
  ```typ
  #let vec(var) = $arrow(bold(upright(var)))$
  $
    nabla times vec(B) - 1/c (partial vec(E))/(partial t) & = (4pi)/c vec(j) \
    nabla dot vec(E) &= 4pi rho \
    nabla times vec(E) + 1/c (partial vec(B))/(partial t) & = vec(0) \
    nabla dot vec(B) & = 0
  $
  ```,
)

// More tests:
// https://mathml.com/start
// https://developer.mozilla.org/en-US/docs/Web/MathML/Tutorials/For_beginners/Three_famous_mathematical_formulas
// https://katex.org/
// https://www.mathjax.org/#samples
// https://www.intmath.com/cg5/katex-mathjax-comparison.php

#let process(tests, numbering: false) = (
  if numbering {
    tests.enumerate(start: 1)
  } else {
    tests.chunks(2)
  }
    .map(x => {
      let eval = eval(x.at(1).text, mode: "markup")
      (
        [#x.at(0)],
        x.at(1),
        html.frame(eval),
        eval,
      )
    })
    .flatten()
)

#table(
  columns: 4,
  table.header(
    [*Test*],
    [*Typst code*],
    [*As rendered by Typst*],
    [*As rendered by your browser*],
  ),
  ..process(misc-tests),
  table.header(
    level: 2,
    table.cell(colspan: 4)[*Mozilla MathML Torture Test*],
  ),
  ..process(mozilla-mathml-torture-test, numbering: true),
  table.header(
    level: 2,
    table.cell(colspan: 4)[*Joe's MathML Browser Test*],
  ),
  ..process(joes-mathml-browser-test),
  table.header(
    level: 2,
    table.cell(colspan: 4)[*MathJax*],
  ),
  ..process(mathjax),
)

--- playground-2 html ---
Testing some math $a + b + c #box[blah] #html.frame($ sum_(i = 0)^oo $) + #html.div[Blah] + d$ a box!
// #let html = [
//   + wow
//   + a list
//   + so cool

//   *bold* _italic_ text!
// ]
// Testing html export $x^2 #h(2em + 1em) x + y = z upright(a) cal(F) scr(F) sans(alpha) bb(sum) #[WOW + a] html$ more text.

// $
//   sqrt(x) root(4, x) sqrt(x + y) root(x + y, 4)
// $


$a + b = #html.div[Blah] c$
