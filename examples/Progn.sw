[defun inc [(x Integer)] : Integer [+ x 1]]

[defun abc [(x Integer)] : Unit 
    [if [= x 1]
        [progn 
            [print String "I love Postgresql <3"] 
            [print String "Hello :)"]]
        [progn
            [print String "I love SQL Server <3"] 
            [print String "Bye :)"]]]

[abc 1]
