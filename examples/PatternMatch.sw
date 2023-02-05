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

[defun print-tuple2 [(tuple Tuple)] : Unit
  [match tuple
    [[!Left [!B] [:or [!C] [!D]]] [print !String "This is the first branch 2"]]
    [[!Left [!A] [:or [!E] [!C]]] [print !String "This is the first branch 3"]]
    [[_] [print !String "Third branch"]]]]    

[print-tuple2 [Left A C]]
