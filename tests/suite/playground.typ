--- playground-2 ---
$
  a & = b \
    & = c
$

--- playground html ---
#show: doc => [
  #import html: *
  #show: html
  #head[
    #meta(charset: "utf-8")
    #meta(name: "viewport", content: "width=device-width, initial-scale=1")
    #link(
      rel: "stylesheet",
      href: "https://mkorje.github.io/MathFonts/Typst/mathfonts.css",
    )
  ]
  #body(doc)
]

#let misc-tests = (
  [Paragraph],
  ```typ
  Let the three given straight lines be $A$, $B$, and $C$, and let the sum of
  any two of these be greater than the remaining one, namely, $A$ plus $B$
  greater than $C$, $A$ plus $C$ greater than $B$, and $B$ plus $C$ greater
  than $B$.
  ```,
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
  This $a "te" b$ or this $a"te" b$ or this $a "te"b$ or this $a"te"b$ e.\
  This $a "t" b$ or this $a"t" b$ or this $a "t"b$ or this $a"t"b$ e.
  ```,
  [Set builder notation],
  ```typ
  $ { x/2 in A mid(|) 1/sqrt(x) > 1 } $
  ```,
  [Set builder notation no mid],
  ```typ
  $ { x/2 in A | 1/sqrt(x) > 1 } $
  ```,
  [`mrow` needed?],
  ```typ
  $ sqrt((x/2)) $
  ```,
  [Escape `lr`],
  ```typ
  $ \(a + b/2) $
  ```,
  [Extra newline],
  ```typ
  $ x + y \ z \ $
  ```,
  [Another one],
  ```typ
  $ E = (2 pi h c^2) / (lambda^5 (e^(h c - lambda k_b T) - 1)) $
  ```,
  [Accents],
  ```typ
  $ accent(x + y, ⏞) overbrace(x + y) $
  ```,
  [Text],
  ```typ
  $ "bruh" $
  ```,
  [Roots],
  ```typ
  $ sqrt(x) root(4, x) sqrt(x + y) root(x + y, 4) $
  ```,
  [Mixing things],
  ```typ
  #let test = [
    + wow
    + a list
    + so cool
    *bold* _italic_ text!
  ]
  Testing html export $x^2 #h(2em + 1em) x + y = z upright(a) cal(F)
  scr(F) sans(alpha) bb(sum) #[WOW + a] test$ more text.
  ```,
  [div],
  ```typ
  $a + b = #html.div[Blah] c$
  ```,
  [boxes and frames],
  ```typ
  Testing some math $a + b + c #box[blah] #html.frame($ sum_(i = 0)^oo $)
  + #html.div[Blah] + d$ a box!
  ```,
  [frac style],
  ```typ
  $ frac(x, y, style: "vertical") $
  $ frac(x, y, style: "skewed") $
  $ frac(x, y, style: "horizontal") $

  #show math.equation.where(block: false): set math.frac(style: "horizontal")
  This $(x-y)/z = 3$ is inline math, and this is block math:
  $ (x-y)/z = 3 $
  ```,
  [Mozilla challenge],
  ```typ
  To solve the cubic equation $t^3 + p t + q = 0$ (where the real numbers
  $p, q$ satisfy $4p^3 + 27q^2 > 0$) one can use Cardano's formula:
  $
    root(3,
      -q/2
      +sqrt(q^2/4 + p^3/27)
    )+
    root(3,
      -q/2
      -sqrt(q^2/4 + p^3/27)
    )
  $
  For any $u_1, ..., u_n in CC$ and
  $v_1, ..., v_n in CC$, the Cauchy–Bunyakovsky–Schwarz
  inequality can be written as follows:
  $
    abs(sum_(k = 1)^n u_k dash(v_k))^2
    <=
    (sum_(k = 1)^n abs(u_k))^2
    (sum_(k = 1)^n abs(v_k))^2
  $
  Finally, the determinant of a Vandermonde matrix can be calculated
  using the following expression:
  $
    mat(delim: \|,
      1, x_1, x_1^2, ..., x_1^(n-1);
      1, x_2, x_2^2, ..., x_2^(n-1);
      1, x_3, x_3^2, ..., x_3^(n-1);
      dots.v, dots.v, dots.v, dots.down, dots.v;
      1, x_n, x_n^2, ..., x_n^(n-1);
    )
    = product_(1 <= i,j <= n) (x_i - x_j)
  $
  ```,
  [`scr` and `cal`],
  ```typ
  $ scr(A) cal(A) scr(a) cal(a) scr(bold(A)) cal(bold(A)) $
  ```,
  // Some from https://fred-wang.github.io/TeXZilla/
  [Ellipse Equation],
  ```typ
  $ x^2/a^2 + y^2/b^2 = 1 $
  ```,
  [Alignment],
  ```typ
  $ (3x + y) / 7 &= 9 && "given" \
    3x + y &= 63 & "multiply by 7" \
    3x &= 63 - y && "subtract y" \
    x &= 21 - y/3 & "divide by 3" $
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
    forall A thin exists P thin forall B thin [
      B in P <==> forall C thin (C in B => C in A )
    ]
  $
  ```,
  [De Morgan's law],
  ```typ
  Logic: $not (p and q) <==> (not p) or (not q)$

  Boolean algebra: $overline(display(union.big_(i = 1)^n) A_i) =
    display(inter.big_(i = 1)^n) overline(A_i)$
  ```,
  [Quadratic Formula],
  ```typ
  $
    x = (-b plus.minus sqrt(b^2 - 4a c)) / (2a)
  $
  ```,
  [Binomial Coefficient],
  ```typ
  $
    C(n, k) = C_k^n = attach(C, bl: n, br: k)
      = binom(n, k) = n! / (k! (n - k)!)
  $
  ```,
  [Sophomore's dream],
  ```typ
  $
    integral_0^1 x^x dif x = sum_(n = 1)^oo (-1)^(n+1) n^(-n)
  $
  ```,
  [Divergence],
  ```typ
  $
    gradient dot arrow(v) = (partial v_x) / (partial x)
      + (partial v_y) / (partial y) + (partial v_z) / (partial z)
  $
  ```,
  [Complex number],
  ```typ
  $
    c = overbrace(
      underbrace(a, "real") + underbrace(b upright(i), "imaginary"),
      "complex number",
    )
  $
  ```,
  [Moore determinant],
  ```typ
  $
    M = mat(delim: \[,
      alpha_1, alpha_1^q, dots.c, alpha_1^(q^(n - 1));
      alpha_2, alpha_2^q, dots.c, alpha_2^(q^(n - 1));
      dots.v, dots.v, dots.down, dots.v;
      alpha_m, alpha_m^q, dots.c, alpha_m^(q^(n - 1));
    )
  $
  ```,
  [Sphere volume],
  ```typ
  #show sym.integral.triple: math.limits
  Spherical coordinates derivation of the volume of a sphere
  $(4/3 pi R^3)$. \
  The formula $S$ for a sphere of radius $R$ in spherical coordinates is: \
  $S = {0 <= phi.alt <= 2pi, 0 <= theta <= pi, 0 <= rho <= R}$
  $
    "Volume" &= integral.triple_S rho^2 sin theta dif rho dif theta dif phi.alt \
      &= integral_0^(2pi) dif phi.alt integral_0^pi sin theta dif theta
        integral_0^R rho^2 dif rho \
      &= lr(phi.alt |)_0^(2pi) lr(zws (- cos theta)|)_0^pi lr(1/3 rho^3 |)_0^R \
      &= 2pi times 2 times 1/3 R^3 \
      &= 4/3 pi R^3
  $
  ```,
  [Schwinger-Dyson equation],
  ```typ
  $
    lr(chevron.l psi mid(|)
      cal(T){ delta / (delta phi.alt) F[phi.alt] }
    mid(|) psi chevron.r) = -upright(i) lr(chevron.l psi mid(|)
      cal(T){ F[phi.alt] delta / (delta phi.alt) S[phi.alt] }
    mid(|) psi chevron.r)
  $
  ```,
  [Differentiable Manifold (tangent vector)],
  ```typ
  $
    gamma_1 equiv gamma_2 <==> cases(
      gamma_1(0) = gamma_2(0) = p\, " and",
      lr(dif / (dif t) phi.alt compose gamma_1(t) |)_(t = 0) =
        lr(dif / (dif t) phi.alt compose gamma_2(t) |)_(t = 0),
    )
  $
  ```,
  [Cichoń's diagram],
  ```typ
  ```,
  [multiscripts & greek alphabet],
  ```typ
  $
    attach(product,
      bl: attach(frak(A)_delta^gamma, bl: alpha, tl: beta),
      tl: attach(frak(B)_theta^eta, bl: epsilon, tl: zeta),
      t: attach(frak(C)_mu^delta, bl: iota, tl: kappa),
      tr: attach(frak(D)_pi^omicron, bl: nu, tl: xi),
      br: attach(frak(E)_upsilon^tau, bl: rho, tl: sigma),
      b: attach(frak(F)_omega^psi, bl: phi.alt, tl: chi),
    )
  $
  ```,
  [nested roots],
  ```typ
  $
    (sqrt(1 + root(3, 2 + root(5, 3 + root(7, 4 + root(
      11, 5 + root(13, 6 + root(17, 7 + root(19, A)))
    )))))) / upright(e)^pi = x'''
  $
  ```,
  [nested matrices],
  ```typ
  ```,
  [font sizes],
  ```typ
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

#let width = 35em
#let process(tests, numbering: false) = (
  if numbering {
    tests.enumerate(start: 1)
  } else {
    tests.chunks(2)
  }
    .map(x => {
      let eval = eval(
        "#show: block.with(width: " + repr(width) + ")\n" + x.at(1).text,
        mode: "markup",
      )
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
  columns: (auto, 1fr, width, width),
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
