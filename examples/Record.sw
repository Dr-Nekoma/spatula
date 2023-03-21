[defrecord Person
   (name String)
   (age Integer)]
  
[defun abc [(xyz Person)] : Unit
         [print !Integer [getr jkl age]]]

[abc { Person |(age 123) (name "Yay") |}]
