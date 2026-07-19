// Test the `box` and `block` containers.

--- box paged ---
// Test box in paragraph.
A #box[B \ C] D.

// Test box with height.
Spaced \
#box(height: 0.5cm) \
Apart

--- block-sizing paged ---
// Test block sizing.
#set page(height: 120pt)
#set block(spacing: 0pt)
#block(width: 90pt, height: 80pt, fill: red)[
  #block(width: 60%, height: 60%, fill: green)
  #block(width: 50%, height: 60%, fill: blue)
]

--- box-fr-width paged ---
// Test fr box.
Hello #box(width: 1fr, rect(height: 0.7em, width: 100%)) World

--- block-fr-height paged ---
#set page(height: 100pt)
#rect(height: 10pt, width: 100%)
#align(center, block(height: 1fr, width: 20pt, stroke: 1pt))
#rect(height: 10pt, width: 100%)

--- block-fr-height-auto-width paged ---
// Test that the fr block can also expand its parent.
#set page(height: 100pt)
#set align(center)
#block(inset: 5pt, stroke: green)[
  #rect(height: 10pt)
  #block(height: 1fr, stroke: 1pt, inset: 5pt)[
    #set align(center + horizon)
    I am the widest
  ]
  #rect(height: 10pt)
]

--- block-fr-height-first-child paged ---
// Test that block spacing is not trimmed if only an fr block precedes it.
#set page(height: 100pt)
#rect(height: 1fr)
#rect()

--- block-fr-height-multiple paged ---
#set page(height: 100pt)
#rect(height: 1fr)
#rect()
#block(height: 1fr, line(length: 100%, angle: 90deg))

--- block-multiple-pages paged ---
// Test block over multiple pages.
#set page(height: 60pt)

First!

#block[
  But, soft! what light through yonder window breaks? It is the east, and Juliet
  is the sun.
]

--- block-multiple-pages-empty paged ---
#set page(height: 60pt)
A
#block(height: 30pt)
B

--- block-box-fill paged ---
#set page(height: 100pt)
#let words = lorem(18).split()
#block(inset: 8pt, width: 100%, fill: aqua, stroke: aqua.darken(30%))[
  #words.slice(0, 13).join(" ")
  #box(fill: teal, outset: 2pt)[tempor]
  #words.slice(13).join(" ")
]

--- block-spacing-basic paged ---
#set par(spacing: 10pt)
Hello

There

#block(spacing: 20pt)[Further down]

--- block-above-below-context paged empty ---
#context test(block.above, auto)
#set block(spacing: 20pt)
#context test(block.above, 20pt)
#context test(block.below, 20pt)

--- block-spacing-context paged ---
// The values for `above` and `below` might be different, so we cannot retrieve
// `spacing` directly
//
// Error: 16-23 function `block` does not contain field `spacing`
#context block.spacing

--- block-spacing-table paged ---
// Test that paragraph spacing loses against block spacing.
#set block(spacing: 100pt)
#show table: set block(above: 5pt, below: 5pt)
Hello
#table(columns: 4, fill: (x, y) => if calc.odd(x + y) { silver })[A][B][C][D]

--- block-spacing-maximum paged ---
// While we're at it, test the larger block spacing wins.
#set block(spacing: 0pt)
#show raw: set block(spacing: 15pt)
#show list: set block(spacing: 2.5pt)

```rust
fn main() {}
```

- List

Paragraph

--- block-spacing-collapse-text-style paged ---
// Test spacing collapsing with different font sizes.
#grid(columns: 2)[
  #text(size: 12pt, block(below: 1em)[A])
  #text(size: 8pt, block(above: 1em)[B])
][
  #text(size: 12pt, block(below: 1em)[A])
  #text(size: 8pt, block(above: 1.25em)[B])
]

--- block-fixed-height paged ---
#set page(height: 100pt)
#set align(center)

#lines(3)
#block(width: 80%, height: 60pt, fill: aqua)
#lines(2)
#block(
  breakable: false,
  width: 100%,
  inset: 4pt,
  fill: aqua,
  lines(3) + colbreak(),
)

--- block-consistent-width paged ---
// Test that block enforces consistent width across regions. Also use some
// introspection to check that measurement is working correctly.
#block(stroke: 1pt, inset: 5pt)[
  #align(right)[Hi]
  #colbreak()
  Hello @netwok
]

#show bibliography: none
#bibliography("/assets/bib/works.bib")

--- box-inset-ratio paged empty ---
#let body-width = 10pt
#context for inset in range(10).map(n => n / 10) {
  // If there's infinite available space, then:
  // ```
  // measured-width = body-width + measured-width × inset.
  // ```
  // (not counting truncation errors)
  let (width: measured-width) = measure(
    box(
      // Outset should not affect inset.
      outset: 137pt,
      inset: (left: 100% * inset),
      block(width: body-width)
    ),
    width: auto,
  )
  assert.eq(measured-width, body-width / (1 - inset))
}

--- block-sticky paged ---
#set page(height: 100pt)
#lines(3)
#block(sticky: true)[D]
#block(sticky: true)[E]
F

--- block-sticky-alone paged ---
#set page(height: 50pt)
#block(sticky: true)[A]

--- block-sticky-many paged ---
#set page(height: 80pt)
#set block(sticky: true)
#block[A]
#block[B]
#block[C]
#block[D]
E
#block[F]
#block[G]

--- block-sticky-grid-many paged ---
// Ensure that sticky blocks are not moved when moving can't improve their fit.
#set page(height: 45pt, width: 4cm, margin: 10pt)
#grid(columns: 1)[
  #set block(spacing: 0pt)
  #set block(height: 10pt, width: 100%)
  #set block(sticky: true, breakable: false)
  #block(fill: aqua)
  #block(fill: green)
  #block(fill: blue)
  #block(fill: red)
]

--- block-sticky-full-region-breakable-child paged ---
// Ensure that a sticky block migrates when a breakable child is reached after
// the current region has been filled exactly.
#set page(height: 30pt, margin: 0pt)
#set block(width: 100%, spacing: 0pt, breakable: false)

#block(height: 20pt, fill: red)
#block(height: 10pt, fill: green, sticky: true)
#block(height: 10pt, fill: blue, breakable: true)

--- block-sticky-current-insertion paged ---
// Ensure that insertions finalized with the current region are not reserved
// again when deciding whether a sticky block fits in the next region.
#set page(height: 100pt, margin: 0pt)
#set block(width: 100%, spacing: 0pt)

#place(bottom, float: true, clearance: 0pt, block(height: 80pt, fill: red))
#block(height: 10pt, fill: green, breakable: false, sticky: true)
#block(height: 15pt, fill: blue, breakable: false)

--- block-sticky-later-region paged ---
// Ensure that a sticky block migrates through a short column when it can fit
// together with its child on a later, full-height page.
#set page(height: 100pt, width: 100pt, margin: 0pt, columns: 2)
#set block(width: 100%, spacing: 0pt, breakable: false)

#place(
  bottom,
  float: true,
  scope: "parent",
  clearance: 0pt,
  block(height: 70pt, fill: red),
)
#block(height: 20pt, fill: aqua)
#block(height: 10pt, fill: green, sticky: true)
#block(height: 40pt, fill: blue)

--- block-sticky-queued-destination-insertion paged ---
// Ensure that a queued insertion shortening the immediate destination does not
// prevent a sticky block from migrating with its child to a later full region.
#set page(height: 100pt, width: 100pt, margin: 0pt)
#set block(width: 100%, spacing: 0pt, breakable: false)

#block(height: 60pt, fill: aqua)
#place(top, float: true, clearance: 0pt, block(height: 70pt, fill: red))
#block(height: 20pt, fill: green, sticky: true)[#metadata(none) <sticky>]
#block(height: 30pt, fill: blue)[#metadata(none) <child>]

#context {
  test(
    (locate(<sticky>).page(), locate(<child>).page()),
    (3, 3),
  )
}

--- block-sticky-empty-breakpoint-child paged ---
// Ensure that an empty child at the breakpoint does not detach a sticky block
// from the next non-empty child.
#set page(height: 30pt, margin: 0pt)
#set block(width: 100%, spacing: 0pt, breakable: false)

#block(height: 20pt, fill: red)
#block(height: 10pt, fill: green, sticky: true)
#block(height: 10pt)
#block(height: 10pt, fill: blue)

--- block-sticky-simulation-finite-fallback paged ---
// Ensure that simulating a spilling breakable block does not expose an
// artificial infinite region to nested layout callbacks.
#set page(height: 30pt, margin: 0pt)
#set block(width: 100%, spacing: 0pt, breakable: false)

#block(height: 20pt, fill: red)
#block(height: 10pt, fill: green, sticky: true)
#block(breakable: true)[
  #block(height: 25pt, fill: blue)
  #layout(size => {
    assert(size.height < 100pt)
    []
  })
]

--- block-sticky-finite-terminal-region paged ---
// Ensure that a finite final region retains its normal overflow behavior when
// deciding whether a sticky block can migrate with its attached child.
#set page(height: 30pt, width: 100pt, margin: 0pt)
#set block(width: 100%, spacing: 0pt, breakable: false)

#block(height: 60pt, breakable: true)[
  #block(height: 20pt)
  #block(height: 10pt, sticky: true)[#metadata(none) <finite-sticky>]
  #block(height: 25pt)[#metadata(none) <finite-child>]
]

#context {
  test(
    (locate(<finite-sticky>).page(), locate(<finite-child>).page()),
    (2, 2),
  )
}

--- block-sticky-pending-footnote-spill paged ---
// Ensure that sticky migration simulation carries a pending multi-region
// footnote spill, so later footnotes remain queued behind it.
#set page(height: 100pt, width: 100pt, margin: 0pt)
#set block(width: 100%, spacing: 0pt, breakable: false)
#set footnote.entry(separator: none, clearance: 0pt, gap: 0pt, indent: 0pt)
#show footnote.entry: it => it.note.body

#block(height: 0pt)[
  #footnote[
    #block(height: 80pt)
    #block(height: 80pt)
    #block(height: 30pt)
  ]
]
#block(height: 5pt, fill: aqua)
#block(height: 10pt, fill: green, sticky: true)[
  #metadata(none) <spill-sticky>
]
#block(height: 10pt, fill: blue)[
  #metadata(none) <spill-target>
  #footnote[#block(height: 90pt)]
]

#context {
  test(
    (locate(<spill-sticky>).page(), locate(<spill-target>).page()),
    (2, 2),
  )
}

--- block-sticky-pending-footnote-queue paged ---
// Ensure that sticky migration simulation carries a pending footnote queue,
// so later footnotes remain queued behind it.
#set page(height: 100pt, width: 100pt, margin: 0pt)
#set block(width: 100%, spacing: 0pt, breakable: false)
#set footnote.entry(separator: none, clearance: 0pt, gap: 0pt, indent: 0pt)
#show footnote.entry: it => it.note.body

#block(height: 85pt, fill: aqua)
#block(height: 0pt, breakable: true)[
  #footnote[
    #block(height: 80pt)
    #block(height: 80pt)
    #block(height: 30pt)
  ]
]
#block(height: 10pt, fill: green, sticky: true)[
  #metadata(none) <queue-sticky>
]
#block(height: 10pt, fill: blue)[
  #metadata(none) <queue-target>
  #footnote[#block(height: 90pt)]
]

#context {
  test(
    (locate(<queue-sticky>).page(), locate(<queue-target>).page()),
    (2, 2),
  )
}

--- block-sticky-empty-breakpoint-fr paged ---
// Ensure that a fractional block satisfies the distribution target after an
// empty breakpoint child, allowing the sticky block to migrate with it.
#set page(height: 30pt, margin: 0pt)
#set block(width: 100%, spacing: 0pt, breakable: false)

#block(height: 20pt, fill: red)
#block(height: 10pt, fill: green, sticky: true)[#metadata(none) <sticky>]
#block(height: 10pt)
#block(height: 1fr, fill: blue)[#metadata(none) <following>]

#context test(locate(<sticky>).page(), locate(<following>).page())

--- block-sticky-fr-footnote-migration paged ---
// Ensure that footnote migration from a fractional block retains a precise
// breakpoint, so a sticky block stays put when migrating cannot help it fit.
#set page(height: 30pt, width: 100pt, margin: 0pt)
#set text(size: 5pt)
#set block(width: 100%, spacing: 0pt, breakable: false)

#block(height: 1pt, fill: red)
#block(height: 29pt, fill: green, sticky: true)[#metadata(none) <fr-sticky>]
#block(height: 1fr, fill: blue)[
  X#metadata(none) <fr-following>
  #footnote(block(height: 10pt)[N])
]

#context {
  test(
    (locate(<fr-sticky>).page(), locate(<fr-following>).page()),
    (1, 2),
  )
}

--- block-sticky-float-footnote-migration paged ---
// Ensure that footnote migration from a float retains a precise breakpoint,
// so a sticky block stays put when migrating cannot help it fit.
#set page(height: 30pt, width: 100pt, margin: 0pt)
#set text(size: 5pt)
#set block(width: 100%, spacing: 0pt, breakable: false)

#block(height: 1pt, fill: red)
#block(height: 29pt, fill: green, sticky: true)[#metadata(none) <float-sticky>]
// The float itself fits in the exhausted region, so its footnote is what
// requests migration of the origin.
#place(
  top,
  float: true,
  clearance: 0pt,
  block(height: 0pt)[
    #metadata(none) <float-origin>
    #footnote(block(height: 10pt)[N])
  ],
)
#block(height: 1pt, fill: blue)[#metadata(none) <float-following>]

#context {
  test(
    (
      locate(<float-sticky>).page(),
      locate(<float-origin>).page(),
      locate(<float-following>).page(),
    ),
    (1, 2, 2),
  )
}

--- block-sticky-place-flush paged ---
// Ensure that a float flush, which produces no in-flow frame, does not detach a
// sticky block from its following in-flow child while a queued float is flushed.
#set page(height: 100pt, width: 100pt, margin: 0pt)
#set block(width: 100%, spacing: 0pt, breakable: false)

#block(height: 60pt, fill: aqua)
#place(
  top,
  float: true,
  clearance: 0pt,
  block(height: 70pt, fill: red)[#metadata(none) <flushed-float>],
)
#block(height: 20pt, fill: green, sticky: true)[#metadata(none) <flush-sticky>]
#place.flush()
#block(height: 30pt, fill: blue)[#metadata(none) <flush-following>]

#context {
  test(
    (
      locate(<flushed-float>).page(),
      locate(<flush-sticky>).page(),
      locate(<flush-following>).page(),
    ),
    (2, 3, 3),
  )
}

--- block-sticky-relative-child-need paged ---
// Ensure that a relative child is measured against the region it would migrate
// to, rather than against its enclosing block's full height.
#set page(height: 30pt, margin: 0pt)
#set block(width: 100%, spacing: 0pt, breakable: false)

#block(height: 10pt, fill: red)
#block(height: 50pt, breakable: true, fill: fuchsia)[
  #block(height: 0pt, fill: green)
  #block(height: 10pt, sticky: true, fill: blue)
  #block(height: 50%, fill: red)
]

--- block-sticky-colbreak paged ---
A
#block(sticky: true)[B]
#colbreak()
C

--- block-sticky-breakable paged ---
// Ensure that sticky blocks are still breakable.
#set page(height: 60pt)
#block(sticky: true, lines(4))
E

--- box-clip-rect paged ---
// Test box clipping with a rectangle
Hello #box(width: 1em, height: 1em, clip: false)[#rect(width: 3em, height: 3em, fill: red)]
world 1

Space

Hello #box(width: 1em, height: 1em, clip: true)[#rect(width: 3em, height: 3em, fill: red)]
world 2

--- block-clip-text paged ---
// Test clipping text
#block(width: 5em, height: 2em, clip: false, stroke: 1pt + black)[
  But, soft! what light through
]

#v(2em)

#block(width: 5em, height: 2em, clip: true, stroke: 1pt + black)[
  But, soft! what light through yonder window breaks? It is the east, and Juliet
  is the sun.
]

--- block-clip-svg-glyphs paged ---
// Test clipping svg glyphs
Emoji: #box(height: 0.5em, stroke: 1pt + black)[🐪, 🌋, 🏞]

Emoji: #box(height: 0.5em, clip: true, stroke: 1pt + black)[🐪, 🌋, 🏞]

--- block-clipping-multiple-pages paged ---
// Test block clipping over multiple pages.
#set page(height: 60pt)

First!

#block(height: 4em, clip: true, stroke: 1pt + black)[
  But, soft! what light through yonder window breaks? It is the east, and Juliet
  is the sun.
]

--- box-clip-radius paged ---
// Test clipping with `radius`.
#set page(height: 60pt)

#box(
  radius: 5pt,
  stroke: 2pt + black,
  width: 20pt,
  height: 20pt,
  clip: true,
  image("/assets/images/rhino.png", width: 30pt)
)

--- box-clip-radius-without-stroke paged ---
// Test clipping with `radius`, but without `stroke`.
#set page(height: 60pt)

#box(
  radius: 5pt,
  width: 20pt,
  height: 20pt,
  clip: true,
  image("/assets/images/rhino.png", width: 30pt)
)

--- box-clip-outset paged ---
// Test clipping with `outset`.
#set page(height: 60pt)

#box(
  outset: 5pt,
  stroke: 2pt + black,
  width: 20pt,
  height: 20pt,
  clip: true,
  image("/assets/images/rhino.png", width: 30pt)
)

--- box-baseline-context paged empty ---
#context test(box.baseline, (at: auto, shift: 0pt))

--- box-html-text html ---
Text #box[Span].

--- box-html-inline html ---
Text #box(html.strong[A])

--- box-html-multiple html ---
Text #box({
  html.strong[A]
  html.mark[B]
})

--- box-html-frame html ---
A
#box(html.frame(rect()))
#box(html.frame(rect()))
B

--- block-html-text html ---
Paragraph
#block[Div]

--- block-html-block html ---
Paragraph
#block(html.div[A])

--- block-html-inline html ---
Paragraph
#block(html.span[Top-level 1])
#block(html.span[Top-level 2])

--- block-html-multiple html ---
Paragraph
#block({
  html.strong[A]
  html.mark[B]
})

--- box-block-html html ---
Text #box(block(html.strong[A]))

--- block-box-html html ---
Text #block(box(html.div[A]))
Text #block(box(html.strong[B]))

--- block-block-html html ---
Text #block(block(html.div[A]))
Text #block(block(html.strong[B]))

--- block-html-frame html ---
Paragraph A
#html.frame(rect())
#html.frame(rect())
Paragraph B

--- block-display-html html ---
// Test how different HTML element's react to being promoted to block-level.
#block(html.script()) // display: none -> nothing
#block(html.div())    // display: block -> nothing
#block(html.table())  // display: table -> nothing
#block(html.li())     // display: list-item -> nothing
#block(html.span())   // display: inline -> block
#block(html.input())  // display: inline-block -> block
#block(html.slot())   // display: contents -> nothing
#block(html.ruby())   // display: ruby -> wrapped in div

--- block-invalid-html html ---
// These are currently wrapped in a div, but they are illegal in this context
// and should raise an error in the future.
#block(html.rt())
#block(html.thead())

--- box-invalid-html html ---
// This is illegal and should raise an error in the future. We do not even
// attempt to set a `display` property.
A #box(html.div()) B

--- container-layoutable-child paged ---
// Test box/block sizing with directly layoutable child.
//
// Ensure that the output respects the box size.
#let check(f) = f(
  width: 40pt, height: 25pt, fill: aqua,
  grid(rect(width: 5pt, height: 5pt, fill: blue)),
)

#stack(dir: ltr, spacing: 1fr, check(box), check(block))

--- issue-2128-block-width-box paged ---
// Test box in 100% width block.
#block(width: 100%, fill: red, box("a box"))
#block(width: 100%, fill: red, [#box("a box") #box()])

--- issue-2914-block-height-cut-off paged ---
// Ensure that breaking a block doesn't shrink its height.
#set page(height: 65pt)
#set block(fill: aqua, width: 25pt, height: 25pt, inset: 5pt)

#block[A]
#block[B]

--- issue-2914-block-fill-skip-nested paged ---
// Ensure that fill and stroke are skipped for an empty frame with a nested block.
#set page(height: 50pt)
A
#block(fill: aqua, stroke: blue, inset: 5pt, width: 100%, block[B])

--- issue-6304-block-skip-label paged ---
// Ensure that labeling is skipped for an empty orphan frame.
#set page(height: 60pt)
A
#block(sticky: true)[B]
#block[C] <label>

--- issue-6125-block-place-width-limited paged ---
// Ensure that the width of a placed block isn't limited by its siblings.
#set page(height: 70pt)
#let b = block({
  square(size: 20pt, fill: aqua)
  place(top, box(height: 10pt, width: 1fr, fill: blue))
})
#b
#b

--- issue-5296-block-sticky-in-block-at-top paged ---
#set page(height: 3cm)
#v(1.6cm)
#block(height: 2cm, breakable: true)[
  #block(sticky: true)[*A*]

  b
]

--- issue-5296-block-sticky-spaced-from-top-of-page paged ---
#set page(height: 3cm)
#v(2cm)

#block(sticky: true)[*A*]

b

--- issue-5296-block-sticky-weakly-spaced-from-top-of-page paged ---
#set page(height: 3cm)
#v(2cm, weak: true)

#block(sticky: true)[*A*]

b

--- issue-5262-block-negative-height paged ---
#block(height: -1pt)[]

--- issue-5262-block-negative-height-implicit paged ---
#set page(height: 10pt, margin: (top: 9pt))
#block(height: 100%)[]

--- issue-5262-block-negative-height-in-flow paged ---
// The contents after the block should be pushed upwards.
#set page(height: 60pt)
a
#block(height: -25pt)[b]
c

--- issue-6267-clip-anti-alias paged ---
#block(
  clip: true,
  radius: 100%,
  rect(fill: gray, height: 1cm, width: 1cm),
)
