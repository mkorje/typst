#set page(width: 5cm, height: auto)

#set math.vec(delim-gap: 0em)
#show math.vec: it => {
  set math.vec(delim-gap: 1em)
  it
}


$ vec(1, 2) vec(delim: "|", 1, 2) vec(delim: ||, 1, 2) vec(delim: angle.l, 1, 2) $
// #set math.vec(delim-gap: 0em)
// $ vec(1, 2) vec(delim: "|", 1, 2) vec(delim: ||, 1, 2) vec(delim: angle.l, 1, 2) $
// #set math.vec(delim-gap: 1em)
// $ vec(1, 2) vec(delim: "|", 1, 2) vec(delim: ||, 1, 2) vec(delim: angle.l, 1, 2) $

#let matrix = math.mat(delim: "|", ($1$, $2$, $3$), ($4$, $5$, $6$), ($7$, $8$, $9$))

$
  #matrix
  X^#matrix
$

$ mat(delim: "|", 1, 2, 3; a, b, c) $

#set text(size: 8pt)

$ mat(delim: "|", 1, 2, 3; 4, 5, 6; 7, 8, 9) $
$ mat(delim: "|", 1, 2, 3; a, b, c) $

#set par(leading: 0.2em)
We proceed by contradiction.
Suppose that there exists a set
of positive integers $a$, $b$, and
$c$ that satisfies the equation
$a^n + b^n = c^n$ for some
integer value of $n > 2$.

1 2 3 \
a b c
