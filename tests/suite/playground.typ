--- playground ---
$ mat(delim: "|", 1, 2, 3; 4, 5, 6; 7, 8, 9) \
  mat(padding: #0em, delim: "|", 1, 2, 3; 4, 5, 6; 7, 8, 9) $

$ script(mat(delim: "|", 1, 2, 3; 4, 5, 6; 7, 8, 9)) $

$ sscript(mat(delim: "|", 1, 2, 3; 4, 5, 6; 7, 8, 9)) $

$mat(delim: "|", 1, 2, 3; 4, 5, 6; 7, 8, 9)$

$ | #box(stroke: .1pt, $mat(delim: #none, 1, 2, 3; 4, 5, 6; 7, 8, 9)$) | $
