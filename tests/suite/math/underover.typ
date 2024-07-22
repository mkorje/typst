// Test under/over things.

--- math-underover-brace ---
// Test braces.
$ x = underbrace(
  1 + 2 + ... + 5,
  underbrace("numbers", x + y)
) $

--- math-underover-line-bracket ---
// Test lines and brackets.
$ x = overbracket(
  overline(underline(x + y)),
  1 + 2 + ... + 5,
) $

--- math-underover-lines ---
// Test lines.
$ underline([1, 2/3])
          arrow.l.r.double.long
  overline([4/5,6]) $

--- math-underover-braces ---
// Test braces.
$ underbrace([1, 2/3], "relevant stuff")
          arrow.l.r.double.long
  overbrace([4/5,6], "irrelevant stuff") $

--- math-underover-brackets ---
// Test brackets.
$ underbracket([1, 2/3], "relevant stuff")
          arrow.l.r.double.long
  overbracket([4/5,6], "irrelevant stuff") $

--- math-underover-parens ---
// Test parentheses.
$ underparen([1, 2/3], "relevant stuff")
          arrow.l.r.double.long
  overparen([4/5,6], "irrelevant stuff") $

--- math-underover-shells ---
// Test tortoise shell brackets.
$ undershell([1, 2/3], "relevant stuff")
          arrow.l.r.double.long
  overshell([4/5,6], "irrelevant stuff") $
