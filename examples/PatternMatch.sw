[defalgebraic Anything
  A
  B]

[defalgebraic Something
  C
  D
  E]

[defalgebraic Tuple
  (Left Anything Something)]

[defun print-tuple [(tuple Tuple)] : Unit
  [match 10
    [[1] [print !String "This is the first branch 2"]]
    [[1] :when T [print !String "This is the first branch 1"]]
    [[_] [print !String "This is the first branch 3"]]]]    

[print-tuple [Left A C]]
