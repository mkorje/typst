#set text(font: "Noto Sans Arabic", lang: "ar")
#show math.equation: set text(font: "Noto Sans Math")

// "" in math doesn't use the text font yet
// it's a little broken atm
#let mtext = text.with(font: "Noto Sans Arabic")

#let cases(..args) = math.cases(..args.pos().map(x => math.display(x)))

// Moroccan style
$ f(x) = cases(
  sum_(i = 1)^s x^i      quad &mtext("إذاكان") &  &x < 0,
  integral_1^s x^i dif x quad &mtext("إذاكان") &  &x in E,
  tg pi                  quad &mtext("غيرذلك") & (&pi tilde.eq 3,141 mtext("مع")),
) $

#show math.equation: set text(dir: rtl)

// Maghreb style
$ serif(dal))seen ( = cases(
  sum_(beh = 1)^sad seen^beh       quad &mtext("إذاكان") & &seen < 0,
  integral_1^sad seen^beh dal seen quad &mtext("إذاكان") & &seen in meem,
                                   quad &mtext("غيرذلك") & (&mtext("مع") pi tilde.eq 3,141),
) $

// Machrek style

// For now `serif()` is how you get the normal Arabic letters.
// TODO: These aren't shaped atm in math.
//
// TODO: The default mapping for beh and jeem should be their
// dotless form. A dotless form of beh is encoded but dotless
// form for jeem is simply hah.
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
  thal
  dad
  zah
  ghain
$

$ abjad \ serif(abjad) \ isolated(abjad) $

في هذا المثال، نريد إيجاد معدل تغير الإحداثي $𞸑$ لجسيم يتحرك على طول منحنى، كما هو موضح بالشكل، عند اللحظة التي تكون نقطة محددة $)𞸎،𞸑($ ومعطى معدل تغير الإحداثي $𞸎$ ثابتًا.
