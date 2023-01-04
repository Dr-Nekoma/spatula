[defun factorial [(x Integer)
                  (y Integer)]: Integer
    [if [< x 0]
        0
        [if [= x 0]
            1
            [if [= x 1]
                y
                [factorial [- x 1] [* x y]]]]]]

[defun secondFactorial [(x Integer)] : Integer
    [if [= x 0]
        1
        [if [= x 1]
            1
            [* x [secondFactorial [- x 1]]]]]]

[defun fibonacci [(x Integer)] : Integer 
    [if [= x 0] 
        0 
        [if [= x 1] 
            1 
            [+ [fibonacci [- x 1]] 
               [fibonacci [- x 2]]]]]]

[print !Integer [factorial 150 1]]