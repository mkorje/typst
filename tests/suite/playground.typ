--- playground ---
#set page(width: auto)

#show math.equation: set text(font: "Noto Sans Math")

#let abjad = $
  alef
  beh
  beh.dotless
  jeem
  dal
  heh
  waw
  zain
  hah
  tah
  yeh
  kaf
  lam
  meem
  noon
  noon.dotless
  seen
  ain
  feh
  feh.dotless
  sad
  qaf
  qaf.dotless
  reh
  sheen
  teh
  theh
  khah
  dad
  zah
  ghain
$

Upright (normal):
$ upright(abjad) $

Default (isolated for all except `jeem` and `heh`, which are initial): \
`beh` and `jeem` should go to their dotless forms...
$ abjad $

Double-struck (falls back to default):
$ bb(abjad) $

Isolated (falls back to default):
$ isolated(abjad) $

Initial (falls back to default):
$ initial(abjad) $

Tailed (falls back to default):
$ tailed(abjad) $

Stretched (falls back to default):
$ stretched(abjad) $

Looped (falls back to default):
$ looped(abjad) $
