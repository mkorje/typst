--- flow-fr paged ---
#set page(height: 2cm)
#set text(white)
#rect(fill: forest)[
          #v(1fr)
  #h(1fr) Hi you!
]

--- issue-flow-overlarge-frames paged ---
// In this bug, the first line of the second paragraph was on its page alone an
// the rest moved down. The reason was that the second block resulted in
// overlarge frames because the region wasn't finished properly.
#set page(height: 70pt)
#block(lines(3))
#block(lines(5))

--- issue-flow-trailing-leading paged ---
// In this bug, the first part of the paragraph moved down to the second page
// because trailing leading wasn't trimmed, resulting in an overlarge frame.
#set page(height: 60pt)
#v(19pt)
#block[
  But, soft! what light through yonder window breaks?
  It is the east, and Juliet is the sun.
]

--- issue-flow-weak-spacing paged ---
// In this bug, there was a bit of space below the heading because weak spacing
// directly before a layout-induced column or page break wasn't trimmed.
#set page(height: 60pt)
#rect(inset: 0pt, columns(2)[
  Text
  #v(12pt)
  Hi
  #v(10pt, weak: true)
  At column break.
])

--- issue-flow-frame-placement paged ---
// In this bug, a frame intended for the second region ended up in the first.
#set page(height: 105pt)
#block(lorem(20))

--- issue-flow-layout-index-out-of-bounds paged ---
// This bug caused an index-out-of-bounds panic when layouting paragraphs needed
// multiple reorderings.
#set page(height: 200pt)
#lines(10)

#figure(placement: auto, block(height: 100%))

#lines(3)

#lines(3)

--- issue-3641-float-loop paged ---
// Flow layout should terminate!
#set page(height: 40pt)

= Heading
#lines(2)

--- issue-3355-metadata-weak-spacing paged ---
#set page(height: 50pt)
#block(width: 100%, height: 30pt, fill: aqua)
#metadata(none)
#v(10pt, weak: true)
Hi

--- issue-3866-block-migration paged ---
#set page(height: 120pt)
#set text(costs: (widow: 0%, orphan: 0%))
#v(50pt)
#columns(2)[
  #lines(6)
  #block(rect(width: 80%, height: 80pt), breakable: false)
  #lines(6)
]

--- issue-5024-spill-backlog paged ---
#set page(columns: 2, height: 50pt)
#columns(2)[Hello]

--- flow-spill-restart-widow paged ---
// A float queued to the next page shrinks it. The block's first frame was laid
// out assuming the full next page (moving the widow pair "CCC" and "DDD"
// along), so it must be laid out again once the actual space is known.
// Otherwise, "CCC" would be lost.
#set page(width: 200pt, height: 200pt, margin: 10pt)
#set text(size: 10pt)
#for i in range(10) [Filler #i.\ ]
#place(auto, float: true, rect(width: 100%, height: 150pt, fill: aqua))
#block(breakable: true, stroke: red)[AAA.\ BBB.\ CCC.\ DDD.]

--- flow-spill-restart-footnote paged pdftags ---
// Like above, but the line whose placement depends on the next page's space
// has a footnote. Gluing frames from inconsistent layouts would unbalance the
// PDF tags.
#set page(width: 200pt, height: 200pt, margin: 10pt)
#set text(size: 10pt)
#set footnote.entry(clearance: 4pt, gap: 2pt)
#for i in range(10) [Filler #i.\ ]
#place(auto, float: true, rect(width: 100%, height: 138pt, fill: aqua))
#block(breakable: true, stroke: red)[AAA.\ BBB.\ CCC.#footnote[Note.]\ DDD.]

--- flow-spill-restart-table paged ---
// The same inside of a table cell.
#set page(width: 200pt, height: 200pt, margin: 10pt)
#set text(size: 10pt)
#for i in range(10) [Filler #i.\ ]
#place(auto, float: true, rect(width: 100%, height: 150pt, fill: aqua))
#table(columns: 1, inset: 0pt, stroke: red)[AAA.\ BBB.\ CCC.\ DDD.]

--- flow-spill-skip-full-region paged ---
// The block's remains have no height. When the page they spill into is taken
// up by a float, they must not be dropped, but move to the page after.
#set page(width: 120pt, height: 100pt, margin: 10pt)
Hello
#place(auto, float: true, rect(width: 100%, height: 100%, fill: aqua))
#block(breakable: true, stroke: blue, width: 100%)[
  #rect(width: 100%, height: 50pt)
  #line(length: 100%, stroke: red + 2pt)
]
After

--- flow-spill-block-width-consistent paged ---
// The block's first region checks that its frames have the same width with
// predictions of the upcoming regions. The float on the third page leaves no
// room for the widow pair, so the block's frame there is empty. It must still
// be as wide as the others.
#set page(width: 200pt, height: 100pt, margin: 10pt)
#set par(spacing: 4pt, leading: 4pt)
Hello
#v(36pt)
#place(auto, float: true, rect(width: 100%, height: 40pt, fill: aqua))
#place(auto, float: true, rect(width: 100%, height: 40pt, fill: teal))
#block(breakable: true, stroke: blue)[
  #for i in range(4) [#box(width: 150pt, height: 10pt, fill: gray)\ ]
  #box(width: 20pt, height: 10pt, fill: red)
]

--- flow-spill-fallback-actual-regions paged pdftags ---
// Footnotes make the space on one page alternate between two layouts, so its
// restarts run out and the block continues from the state its last frame was
// laid out with. It must continue with the actual space and the latest
// predictions: With the predicted regions of that frame, the restarts for the
// next page could never take effect, and the block's last line would overlap
// the footnotes there.
#set page(width: 200pt, height: 120pt, margin: 8pt)
#set text(size: 10pt)
#set footnote.entry(clearance: 6pt, gap: 1pt)
#let bar(height) = box(width: 60%, height: height, fill: luma(200))
#block(breakable: true, stroke: red)[A\ B#footnote[#bar(12pt)]\ C\ D\ E\ F#footnote[#bar(32pt)]\ G\ H
I#footnote[#bar(10pt)]\ J\ K\ L#footnote[#bar(14pt)]\ M\ N#footnote[#bar(10pt)]\ O]
P\ Q\ R\ S\
T\ U\
#block(breakable: true, stroke: red)[V\ W#footnote[#bar(10pt)]\ X#footnote[#bar(10pt)#footnote[N] #bar(22pt)]

Y\ Z\ AA\ AB

AC\ AD#footnote[#lorem(34)]\ AE#footnote[#bar(14pt)\ #bar(14pt)\ #bar(14pt)\ #bar(14pt)]

AF#footnote[#bar(18pt)]\ AG]
#block(breakable: true, stroke: red)[AH
AI]

--- flow-spill-restart-cap-nested paged pdftags large ---
// Nested breakable blocks whose footnotes make the space on page 7 alternate
// between two heights, depending on where the innermost block breaks. The page
// uses up its restarts, and the block continues from the state its last frame
// was laid out with, into the actual space. Every line must appear exactly
// once, with its footnote entry on the same page or after it. (`main` drops
// `P2.1.0`, since it glues together frames of layouts with different breaks,
// and puts the entry of `P2.1.2` on the page before the line.)
#set page(width: 160pt, height: 200pt, margin: 8pt)
#set text(size: 10pt)
#set footnote.entry(clearance: 2pt, gap: 2pt)
#let e(n) = ("e " * n).trim()
\ \ 
#place(auto, float: true, rect(width: 100%, height: 67pt, fill: aqua))
#block(breakable: true, stroke: blue, inset: 2pt)[\ P0.0.1z#footnote[E0.0.1 #e(1)] #block(breakable: true, stroke: red, inset: 4pt)[P0.1.0z#footnote[E0.1.0 #e(19)]\ P0.1.1z#footnote[E0.1.1 #e(19)]\ P0.1.2z#footnote[E0.1.2 #e(43)]\ P0.1.3z#footnote[E0.1.3 #e(1)]\ P0.1.4z#footnote[E0.1.4 #e(19)]\ P0.1.5z#footnote[E0.1.5 #e(19)]\ \ \ P0.1.8z#footnote[E0.1.8 #e(43)]\ \ \ P0.1.11z#footnote[E0.1.11 #e(43)]]]
\ F1.1.
#block(breakable: true, stroke: blue, inset: 0pt)[\ P1.0.1z#footnote[E1.0.1 #e(19)]\ P1.0.2z#footnote[E1.0.2 #e(19)] #block(breakable: true, stroke: red, inset: 0pt)[\ \ P1.1.2z#footnote[E1.1.2 #e(43)]\ P1.1.3z#footnote[E1.1.3 #e(19)]\ \ \ \ P1.1.7] P1.50.0z#footnote[E1.50.0 #e(42)]\ P1.50.1z#footnote[E1.50.1 #e(1)]]
\ \ \ \ \ \ \ \ \ F2.9.
#place(auto, float: true, rect(width: 100%, height: 103pt, fill: aqua))
#block(breakable: true, stroke: red, inset: 2pt)[P2.0.0z#footnote[E2.0.0 #e(43)] #block(breakable: true, stroke: green, inset: 2pt)[P2.1.0z#footnote[E2.1.0 #e(43)]\ P2.1.1\ P2.1.2z#footnote[E2.1.2 #e(19)]\ \ \ \  #block(breakable: true, stroke: blue, inset: 0pt)[\ \ \ \ \ \ \ ]] \ ]
