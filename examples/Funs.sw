[defun add [(a Integer) (b Integer)] : Integer [+ a b]]

[defun add2 [(a Integer)] : Integer [add a 2]]

[print Integer [add2 4]]