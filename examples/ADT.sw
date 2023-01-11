[defalgebraic Either[(T Star) (U Star)]
  (Left T)
  (Right U)]

[defalias Result [(T Star)] |Either String T|]

[print !|Result Integer| [Right !String !Integer 2]]
