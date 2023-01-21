[defalgebraic Either[(T Star) (U Star)]
  (Left T)
  (Right U)]

[defun print-either [(either |Either String Integer|)] : Unit
  [match either
    [[Right y] [print !Integer y]]
    [[Left x] [print !String x]]]]

[defun match-unit [(value Unit)] : Unit
  [match value
    [[name] [print !String "First branch"]]
    [[_] [print !String x]]]]

[print-either [Left !String !Integer "This is a test"]]
