[defalias Mass Integer]

[define Gravity [+ 1 9]]

[defun print-weight [(x @Mass)] : Unit [print !Integer [* x Gravity]]]

[print-weight 60]
