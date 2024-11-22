#set page(width: 18cm, height: 22cm, columns: 2)
#set par(justify: true)

= No numbering

When `numbering` is set to `none` (the default).
#set math.equation(numbering: none)

Equations are not numbered at all.
$ M(t) = E(e^(t X)) = sum_(x in S) e^(t x) f(x) $

We can still label equations, so that we can query
them, for example. Using the special label `<*>`
is no different than any other label. But trying
to reference them will throw an error, as
numbering is not enabled.
$ P(A union B) = P(A) + P(B) - P(A sect B) $ <nno:eq1>
// error: cannot reference equation without numbering
// @nno:eq1

$ V(a X + b Y) = a^2 V(X) + b^2 V(Y) $ <*>

```typ
#context query(<nno:eq1>)
#context query(<*>)
```
$a+b$ <nno:eq3>
$ x &= y \
    &= z $ <nno:eq2>
#context query(<nno:eq2>)
#context query(<nno:eq3>)

Labels within an equation don't do anything, and
just have their `repr` shown.
$ x <a>, y <*> $

// REMOVE {
// We can link two equations together, so that they
// share alignments, by giving them both the same
// label.
// $ &V(a X) &&= a^2 V(X) \
//   &V(a X + b Y) &&= a^2 V(x) + b^2 V(Y) $ <nno:eq2>

// Compare the alignments of the below two equations.
// $ &V(X + c) &&= V(X) $
// $ &V(X + c) &&= V(X) $ <nno:eq2>

// Trying to reference these equations will also
// throw an error.
// // error: cannot reference equation without numbering
// // @nno:eq2
// } REMOVE

= Numbering by equation

When `numbering-mode` is set to `"equation"` (the default) and `numbering` is not `none`.
#set math.equation(numbering: "(1)", numbering-mode: "equation")

Every equation is numbered by default.
$ a + b = c $

You can attach a label to an equation, so that you
can reference it later. See @neq:eq1.
$ a^2 + b^2 = c^2 $ <neq:eq1>

If you don't want to label a specific equation,
you can attach the special label `<*>` to it.
$ a^n + b^n = c^n $ <*>

We can give two equations the same number by attaching
the same label to both of them.

$ 0 &= a x^2 + b x + c $ <neq:eq2>
has solution
$ x &= (-b + sqrt(b^2 - 4 a c)) / (2 a). $ <neq:eq2>

Referencing this equation still works, despite the
label being used twice: @neq:eq2. It'll link to
the first equation with the label.

The counter will continue afterwards, as expected.
$ a x + b y = c $

We can even give another equation much later the same
label, and the numbering will still work.
$ x &= (-b - sqrt(b^2 - 4 a c)) / (2 a) $ <neq:eq2>

Labels within an equation don't do anything, and
just have their `repr()` shown.
$ x <a>, y <*> $

= Numbering by line

When `numbering-mode` is set to `"line"` and `numbering` is not `none`.
#set math.equation(numbering: "(1a)", numbering-mode: "line")

= Numbering by label

When `numbering-mode` is set to `"label"` and `numbering` is not `none`.
#set math.equation(numbering: "(1a)", numbering-mode: "label")

= Numbering by reference

When `numbering-mode` is set to `"reference"` and `numbering` is not `none`.
#set math.equation(numbering: "(1a)", numbering-mode: "reference")
