#set page(width: 5cm)

// Add NumberingMode to EquationElem
// Adds the `NumberingMode` struct. Defines and describes the four
// numbering modes to be supported. A proper description of this field on
// `math.equation` is still missing.
//
// A note for the future: reusing a label for two non-adjacent equations is
// problematic. Perhaps this should throw an error (but only when using a
// numbering mode where this becomes a problem).

// Add label parsing
// Implements code needed to parse and eval labels in Math mode. `<*>`,
// denoting "do not label", is evaluated as a label with String "*".
//
// Consider allowing references `@...` directly in Math mode, in the
// future.

#set math.equation(numbering: "1", numbering-mode: "equation")

$ a $ <x>
$ a $ <*> <x>
$ a $ <x> <*>

#context query(<x>).map(it => it.numbering)

// #let va = $c \ d$
// $ a + b va $ <y>

#context query(<y>)

$ a + b $ <z>
$ a + b \ $ <z>
// warning: ignoring label `<a>` attached to line in a single line equation
$ a + b <a> $ <z>

// warning: do not number marker is not attached to an equation line
$ a + b <*> $ <z>
// warning: do not number marker is not attached to an equation line
$ a + b \ <*> $ <z>
// warning: ignoring label `<a>` attached to line in a single line equation
// warning: do not number marker is not attached to an equation line
$ a + b <a> <*> $ <z>

#context query(<z>).map(x => x.body)

#eval(
  "abc/xyz \ C (a/2 \ b)",
  mode: "math",
  scope: (
    abc: $a + b + c$,
    xyz: $x + y + z$,
  ),
) <g>

#context query(<g>)
