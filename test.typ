#set page(width: 10cm, height: 18cm)

// $ a + b \
//   c + d <kk> $ <ll>

// #context query(<kk>)
// #context query(<ll>)

// #set math.equation(numbering: none, numbering-mode: "equation")

// #list(
//   [Foundations],
//   [Calculate],
//   [Construct],
//   [Data Loading],
// ) <a>

// #context query(<a>)

// $ a + b \
//   c + d <a>$ <b>
// $ a + b \
//   c + d <a> $ <b>

// #context query(<a>)

// #let bx(body) = box(body, stroke: blue+0.5pt, inset: (x:2pt, y:3pt))
// $
//   a \
//   bx(x y)  &&quad  bx(x (y z))  &quad  bx(x y^z) <d>
// $ <c>

// #context query(<d>).at(0)
// #context query(<d>)
// #math.equation({
//   [$ a $ <1> \ ]
//   $ b $
// }) <c>

// #context query(<c>).at(0).fields()
// #context query(<c>).at(0).fields().body.children.at(0).fields()

// I think there's another issue that needs addressing with regards to equation numbering: the distinction between math as an input mode, and how it is displayed in the document. I'm bringing this up as each line in an equation kind of needs to be its own element, that way you can reference and query it properly. As each line is math, this element should be an equation, but. Maybe I'm overthinking this, 

// #lorem(20)
// $ a &+ z <x>
//   c + &d \
//   &e + f <*>
//   g + h& \ $ <y>
// #lorem(20)

// #context query(<y>)
// // #context query(<y>).at(0).body.children.at(0).fields()
// #context query(<x>)
// // #context query(<*>)

// $ a $ <*>

// $ b $

// $ c $ <y>

// A

// $ c \
//   d <l> $ <y>

// #context query(<l>).at(0)

// @y
// // @l


// WORKING
// $ a + b $ <a>
// $ a + b \ $ <b>
// $ a + b <*> $ <c>
// $ a + b <e> $ <d>
// #context query(<a>)
// #context query(<b>)
// #context query(<c>)
// #context query(<d>)
// #context query(<e>)
// #context query(<*>)

// $a + b$
// $RR/CC$
// $a \ b$

$ a &+ z <x>
  c + &d \
  &e + f <*>
  g + h& $ <y>

#set math.equation(numbering: "(1a)", numbering-mode: "line")

#counter(math.equation).update(3)

$ a &+ z <x>
  c + &d \
  &e + f <*>
  g + h& $ <y>

#show math.equation: set align(end)

$ a &+ z <x>
  c + &d \
  &e + f <*>
  g + h& $ <y>

#show math.equation: set align(center)
#set math.equation(number-align: start)

$ a &+ z <x>
  c + &d \
  &e + f <*>
  g + h& $ <y>

#show math.equation: set align(start)

$ a &+ z <x>
  c + &d \
  &e + f <*>
  g + h& $ <y>


// $ a + b \ $ <z>
// $ \ $ <a>

// #context query(<a>)

// error: unclosed label
// $ 1 <2 $
// should maybe make the above not an error...


// #set math.equation(numbering: none, numbering-mode: "equation")

// #lorem(10)
// $ a + b \ $ <a>
// #lorem(10)
// $ x + y <d> $ <a>
// #lorem(10)
// $ a &+ z <x>
//   c + &d \ $ <b>
// #lorem(10)
// $ a &+ z <x>
//   c + &d <e> $ <b>
// #lorem(10)
// #context query(<e>).at(0)
// #lorem(10)
// #context query(<a>)
// // #context query(<b>)
// #context query(<x>)
// #context query(<e>).at(0).fields()
// #context query(<a>).at(0)
