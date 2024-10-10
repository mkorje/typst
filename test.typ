#set page(width: auto, height: auto)

#let test = context [
  #math.equation.size,
  #math.equation.variant,
  #math.equation.cramped,
  #math.equation.bold,
  #math.equation.italic,
  #math.equation.class,
]

#test

// math.equation.size
$ display(test) \
  inline(test) \
  script(test) \
  sscript(test) $

// math.equation.variant
$ serif(test) \
  sans(test) \
  frak(test) \
  mono(test) \
  bb(test) \
  cal(test) $

// math.equation.cramped
$ display(test, cramped: #true) \
  display(test, cramped: #false) $

// styles
// upright
// math.equation.bold
// math.equation.italic
$ upright(test) \
  bold(test) \
  italic(test) $

// math.equation.class
#let classes = (
  "normal",
  "punctuation",
  "opening",
  "closing",
  "fence",
  "large",
  "relation",
  "unary",
  "binary",
  "vary",
)
#let test-class = {
  for class in classes {
    [#math.class(class, test) \ ]
  }
}
$ #test-class $

// misc tests
$
  bold(A^(#context math.equation.bold))
  bold(A^(#context math.equation.size))
  cal(bold(A^(bb(#context math.equation.variant))))
$

#let func = context {
  let size = math.equation.size
  if size == "display" {
    $a$
  } else if size == "text" {
    $b$
  } else if size == "script" {
    $c$
  } else if size == "script-script" {
    $d$
  }
}

$A+func$
$ A + func
  A^func
  a^A^func $

#set math.equation(bold: true, italic: false, variant: "sans")
$a A$

#context math.equation.bold \
#context math.equation.italic \
#context math.equation.variant \

#set math.equation(bold: false, italic: auto, variant: "serif")

#set math.equation(class: "large")
$A class("normal", A)$
#set math.equation(class: none)

// Cramped and size don't work as expected.
// Though I guess that's to be expected as
// it is getting overwritten by the creation
// of an EquationElem.
#set math.equation(cramped: true)
$ sum_1^2 $
#set math.equation(cramped: false)
$sum_1^2$
#set math.equation(size: "script-script")
$ sum_1^2 $
#math.equation(size: "script-script", $sum$)
// This does work though.
#show math.equation: set math.equation(size: "script")
$a b$
// Perhaps the above behaviour is a good
// reason the style fields should live
// on another element, instead of on
// math.equation.

#math.equation(variant: "cal", $a$) <eq>
#context query(<eq>).first()

#context query(<eq>).first().fields()

#context query(math.equation.where(variant: "cal"))
