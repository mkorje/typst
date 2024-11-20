#set page(width: 15cm, height: 18cm)

$ a + b \
  c + d <kk> $ <ll>

#context query(<kk>)
#context query(<ll>)

#set math.equation(numbering: "(1a)", numbering-mode: "equation")

// #list(
//   [Foundations],
//   [Calculate],
//   [Construct],
//   [Data Loading],
// ) <a>

// #context query(<a>)

// $ a + b \
//   c + d $ <b>

// #context query(<b>)

// #math.equation({
//   [$ a $ <1> \ ]
//   $ b $
// }) <c>

// #context query(<c>).at(0).fields()
// #context query(<c>).at(0).fields().body.children.at(0).fields()

// I think there's another issue that needs addressing with regards to equation numbering: the distinction between math as an input mode, and how it is displayed in the document. I'm bringing this up as each line in an equation kind of needs to be its own element, that way you can reference and query it properly. As each line is math, this element should be an equation, but. Maybe I'm overthinking this, 

#lorem(20)
$ a &+ z <x>
  c + &d \
  &e + f <*>
  g + h& \ $ <y>
#lorem(20)

#context query(<y>)
// #context query(<y>).at(0).body.children.at(0).fields()
#context query(<x>)
#context query(<*>)

$ a $ <*>

$ b $

$ c $ <y>

A

$ c \
  d <l> $ <y>

#context query(<l>).at(0)

@y
// @l


WORKING
$ a + b $ <a>
$ a + b \ $ <b>
$ a + b <*> $ <c>
$ a + b <e> $ <d>
#context query(<a>)
#context query(<b>)
#context query(<c>)
#context query(<d>)
#context query(<e>)
#context query(<*>)

$a + b$
$RR/CC$
$a \ b$
