[defrecord Person
   (name String)
   (age Integer)]
  
[defun abc [(xyz Person)] : Unit
    [let-in [[jkl [setr xyz [(name "Wow") (age 104)]]]]
       [progn
         [print !Integer [getr jkl age]]
	     [print !Integer [getr xyz age]]
         [print !String [getr jkl name]]]]]

[abc { Person |(age 123) (name "Yay") |}]

[defrecord Person2[(T Star) (U Star)]
  (name T)
  (age U)]

[defun abc2 [(xyz |Person2 String Integer|)] : Unit
    [let-in [[jkl [setr xyz [(name "Wow") (age 104)]]]]
       [progn
         [print !Integer [getr jkl age]]
	     [print !Integer [getr xyz age]]
         [print !String [getr jkl name]]]]]

[abc2 { |Person2 String Integer| |(age 123) (name "Yay") |}]

