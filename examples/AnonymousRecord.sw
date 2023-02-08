[defun abc [(xyz {|(x String) (y Integer)|})] : Unit
    [let-in [[jkl [setr xyz [(x "Wow") (y 104)]]]]
       [progn
         [print !Integer [getr jkl y]]
	 [print !Integer [getr xyz y]]
         [print !String [getr jkl x]]]]]

[abc {|(y 123) (x "Yay") |}]
