[print Integer [+ 1 2]]

{;
[print Integer [+ 1 2]]
[print Integer [+ 2 3]]
;}

[defun add [(a Integer) (b Integer)] : Integer 
    [print Integer 123]
    [+ a b]]

[defun add2 [(a Integer)] : Integer [add a 2]]

[print Integer [add 2 24]]

[print Integer [add2 24]]

// [print Integer [+ 1 2]]

[print Integer [* 2 12]]

{;
[print Integer [+ 1 2]]
[print Integer [+ 2 3]]
;}

// [print Integer [+ 1 2]]