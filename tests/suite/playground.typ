--- playground ---
#set page(width: 10cm)
#set text(lang: "ar", font: "Noto Sans Arabic")
#show math.equation: set text(font: "Noto Sans Math")
#let mtext = text.with(font: "Noto Sans Arabic")
#let cases(..args) = math.cases(..args.pos().map(x => math.display(x)))

$ (9 + 10) (a + b) / c (a + d) $

$
  upright(dal)(seen) = cases(
    sum_(beh = 1)^sad seen^beh quad & mtext("إذاكان") && seen < 0,
    integral_1^sad seen^beh dal seen quad & mtext("إذاكان") && seen in meem,
    quad & mtext("غيرذلك") & ( & mtext("مع") pi tilde.eq 3,141),
  )
$

#show math.equation: set text(dir: rtl)
$a + b$
$(𞸎،𞸑$
$(𞸎،𞸑)$

في هذا (المثال، نريد إيجاد معدل) تغير الإحداثي $𞸑$ لجسيم يتحرك على طول منحنى، كما هو موضح بالشكل، عند اللحظة التي تكون نقطة محددة $(𞸎،𞸑$ ومعطى معدل تغير الإحداثي $𞸎$ ثابتًا ۱۰۱۰.

$
  upright(dal)(seen) = cases(
    sum_(beh = 1)^sad seen^beh quad & mtext("إذاكان") && seen < 0,
    integral_1^sad seen^beh dal seen quad & mtext("إذاكان") && seen in meem,
    quad & mtext("غيرذلك") & ( & mtext("مع") pi tilde.eq 3,141),
  )
$

--- arabic-math-basic ---
#set page(width: 400pt, margin: 20pt)
#set text(lang: "ar", size: 11pt, font: "Noto Sans Arabic")
#show math.equation: set text(font: "Noto Sans Math", dir: rtl)
#set math.equation(numbering: "(١)")

في هذا المثال، نريد إيجاد معدل تغير الإحداثي $𞸑$ لجسيم يتحرك على طول منحنى، كما هو موضح بالشكل، عند اللحظة التي تكون نقطة محددة $(𞸎، 𞸑)$ ومعطى معدل تغير الإحداثي $𞸎$ ثابتًا.

على الرغم من أن الحل سيكون جبريًا في البداية، فإن الناتج النهائي سيكون عدديًا بعد أن نعوض بالقيم المعلومة.

من أجل إيجاد معدل تغير $𞸑$، $(𞸃 𞸑) / (𞸃 𞸍)$، نبدأ باشتقاق معادلة المنحنى ضمنيًا بالنسبة إلى $𞸍$.
بعد ذلك، نعوض بمعدل تغير الإحداثي $𞸎$، $(𞸃 𞸎) / (𞸃 𞸍) = ٢$، عند النقطة حيث $𞸎 = −١$ و $𞸑 = ٣$.

إذا أخذنا مشتقة معادلة المنحنى بالنسبة إلى $𞸍$، نجد أن:

$
  ٠ & = 𞸃 / (𞸃 𞸍) (٦𞸑^٢ + ٢𞸎^٢ − ٢𞸎 + ٥𞸑 − ١٣)                                   \
    & = ١٢𞸑 (𞸃 𞸑) / (𞸃 𞸍) + ٤𞸎 (𞸃 𞸎) / (𞸃 𞸍) − ٢ (𞸃 𞸎) / (𞸃 𞸍) + ٥ (𞸃 𞸑) / (𞸃 𞸍) \
    & = (١٢𞸑 + ٥) (𞸃 𞸑) / (𞸃 𞸍) + (٤𞸎 − ٢) (𞸃 𞸎) / (𞸃 𞸍).                        \
$

يمكننا الآن التعويض بالنقطة $𞸎 = −١$ و $𞸑 = ٣$ ومعدل تغير الإحداثي $𞸎$، $(𞸃 𞸎) / (𞸃 𞸍) = ٢$:

$
  ٠ & = (١٢ times ٣ + ٥) (𞸃 𞸑) / (𞸃 𞸍) + (٤ times −١ − ٢) times ٢ \
    & = ٤١ (𞸃 𞸑) / (𞸃 𞸍) − ١٢.
$

ثم بإعادة الترتيب لإيجاد قيمة $(𞸃 𞸑) / (𞸃 𞸍)$، نحصل على:

$
  (𞸃 𞸑) / (𞸃 𞸍) = ١٢/٤١.
$

إذن، معدل تغير الإحداثي $𞸑$ هو $١٢/٤١$.

--- arabic-math-complex ---
#set page(width: 400pt, margin: 20pt)
#set text(lang: "ar", size: 11pt, font: "Noto Sans Arabic")
#show math.equation: set text(font: "Noto Sans Math", dir: rtl)

#let mtext = text.with(font: "Noto Sans Arabic")
#let sin = math.op(mtext("جا"))
#let cos = math.op(mtext("جتا"))
#let tan = math.op(mtext("قا"))
#let cot = math.op(mtext("قتا"))
#let sec = math.op(mtext("ظا"))
#let csc = math.op(mtext("ظتا"))

باشتقاق المعادلة البارامترية للمتغيِّر $𞸎$ بالنسبة إلى $theta$، نحصل على:

$
  (𞸃 𞸎) / (𞸃 𝜃) & = 𞸃 / (𞸃 𝜃) (−٤ csc 𝜃 + ٣)           \
                & = −٤ 𞸃 / (𞸃 𝜃) csc 𝜃 + 𞸃 / (𞸃 𝜃) (٣) \
                & = −٤ (−cot 𝜃) + ٠                    \
                & = ٤ cot^٢ 𝜃.
$

باشتقاق معادلة $𞸑$، نحصل على:

$
  (𞸃 𞸑) / (𞸃 𝜃) & = 𞸃 / (𞸃 𝜃) (٣ sin^٢)                               \
                & = ٣ 𞸃 / (𞸃 𝜃) sin^٢ 𝜃 + sqrt(٢) + 𞸃 / (𞸃 𝜃) + tan 𝜃
$

علينا تطبيق قاعدة السلسلة لإيجاد مشتقة $sin(theta^٢)$.
يمكننا كتابة هذا التعبير على صورة التركيب $د compose 𞸓$؛ حيث $د(theta) = theta^٢$، $𞸓(theta) = sin theta$.
إذن $د'(theta) = ٢theta^(٢-١)$، $𞸓'(theta) = cos theta$، ما يؤدِّي إلى:

$
  𞸃 / (𞸃 𝜃) sin^٢ 𝜃 = د')𞸓)𝜃((𞸓')𝜃( = ٢ sin ٢𝜃 cos 𝜃.
$

بالتعويض بهذا التعبير وبمشتقة $tan 𝜃$ أيضًا في تعبير $(𞸃 𞸑) / (𞸃 𝜃)$، نحصل على:

$
  (𞸃 𞸑) / (𞸃 𝜃) = ٦ sin 𝜃 cos 𝜃 + sqrt(٢) tan 𝜃 sec 𝜃.
$

ومن ثَمَّ، بتطبيق الاشتقاق البارامتري يكون لدينا:

$
  (𞸃 𞸑) / (𞸃 𞸎) = ((𞸃 𞸑) / (𞸃 𝜃)) / ((𞸃 𞸎) / (𞸃 𝜃)) = (٦sin 𝜃 cos 𝜃 + sqrt(٢)tan 𝜃 sec 𝜃) / (٤ cot^٢𝜃)
$

وبما أن المستقيم عمودي عند قيمة البارامتر $theta = pi / ٤$، إذن علينا حساب قيمة التعبير السابق عند هذه النقطة:

$
  lr((𞸃 𞸑) / (𞸃 𞸎) |)_(theta = pi / ٤) & = (sin(pi/٤) cos(pi/٤) + sqrt(٢) + tan(pi/٤) + sec(pi/٤)) / (٤cot^٢ pi/٤) \
  & = (٦ times sqrt(٢) / ٢ times sqrt(٢) / ٢ + sqrt(٢) times ٢ / sqrt(٢) times ١) / (٤ times (٢ / sqrt(٢))^٢) \
  & = (٣ + ٢) / ٨ \
  & = ٥ / ٨.
$

--- arabic-math-style ---
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
