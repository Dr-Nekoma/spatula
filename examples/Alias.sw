[defalias Mass Integer]

[defalias Queue [(a Star)] |List a|]

[define Gravity [+ 1 9]]

[defun print-weight [(x Mass)] : Unit
  [print !Integer [* x Gravity]]]

[defun print-queue [(x |Queue Integer|)] : Unit
  [print !|Queue Integer| x]]

[print-weight 60]

[print-queue '[1 2 3 4]]


