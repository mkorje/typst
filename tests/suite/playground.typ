--- playground ---
#set page(width: 30em, height: 12em, margin: 2em)
#set par(justify: true)
#set par(first-line-indent: (amount: 2em, all: true))
// #set math.equation(numbering: "(1.1)", number-align: left, breakable: false)
// #set par.line(numbering: "1")
#show math.equation: set par.line(numbering: "1")
// #set text(dir: rtl)
#show par: set text(red)

Some text in a paragraph $x + y$.
More text in par #lorem(20) $ t + s $.
$a+b$ wowzers.
$
      & a + b \
  c + & d \
      \
      & f     & + g
$
#set math.equation(align: left)
Still the same paragraph.
$
      & a + b \
  c + & d \
      & f     & + g
$
#set math.equation(align: right, number-align: right, breakable: true)
Now breakable (and still same par)!
$
      & a + b \
  c + & d \
      & f     & + g
$

#set math.equation(align: center)
$x + y$ #lorem(20)

$ x + y $
#lorem(10)

#[
  #set align(center)
  #block($ x + y $)
]
#block($ x + y $)
$ x + y $
#lorem(15)

$ x + y $

#block[
  Blah $ x + y $
]

/*#set math.equation(align: right)
$
      & a + b \
  c + & d \
      & f     & + g
$
Blah $x$ blah $y$

#set math.equation(align: center)

$ 9 + 10 = 21 $

$x + y$
$ x + z $

Blah

$x + y$ Blah.

#h(1em, weak: true)
$ 1 + 1 = 2 $

#h(1em, weak: true)
- a*/
/*
#lorem(10), for example,
$ integral x + y = z $
shows that the integral of $x + y$ is $z$.

#lorem(20)

#lorem(20)
$
  integral x + y = integral x + y = integral x + y = integral x + y = integral x + y = integral x + y = integral x + y =
$
#lorem(20)
$
  integral x + y = integral x + y = integral x + y = integral x + y = integral x + y = \
  integral
$
#lorem(20)
$
  a + b \
  c + d
$
#lorem(20)


#lorem(20)
$ integral x + y = z $

$ integral x + y = z $

A
$
  x + y & =      & c + d & z \
        & 9 + 10 &     a & integral
$
equation.

$
  x & = x \
  p & = p \
  d & = d \
  x & = x
$*/

--- playground-2 ---
#set par(justify: true)
#set align(center)

#lorem(20)
