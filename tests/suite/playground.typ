--- playground ---

#let test = {
  $ f(x) = abs(I) $
  $ dot(f)(x) = abs(dot(I)) $
  $ integral_x^x f_x^x $
  $ A B C x y z $
  $ H I J K L M N O P Q R S T U V W X Y Z $
  $ f"text" f "text" $

  [
    Some inline math nowa$f$a gow clo$dot(f)$se is it?
  ]

  $
    f(T), f(T), integral_a^b f(x) d x, 1/T,
  $

  pagebreak()
}

#test

#show math.equation: set text(font: "STIX Two Math")
#test

#show math.equation: set text(font: "Noto Sans Math")
#test
