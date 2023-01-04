[defun id [(T Star) (arg T)] : T arg]

[defun apply-id [(id (forall T. Star; (-> (T) T)))] : Unit [print !|List Integer| [id !|List Integer| '[1 2 3 4]]]]

[apply-id id]