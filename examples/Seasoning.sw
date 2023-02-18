[defalgebraic Either[(T Star) (U Star)]
  (Left T)
  (Right U)]

[defalias Result [(T Star)] |Either String T|]

[defalgebraic Thing[(T Star)]
  Nothing
  (Something T)]