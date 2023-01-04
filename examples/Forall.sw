[defun id [(T Star) (x @T)] : @T
  x]

[print !String [id !String "Lemos"]]

// [abc Integer '[1 2 3]]

{;
forall !T. T -> Unit
(ETypeAbstraction "T" * Nothing (EProgn [EApplication (ETypeApplication (EVariable "print") Integer) (ELiteral 2)]))
;}