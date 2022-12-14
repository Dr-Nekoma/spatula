[defun abc [(f (-> (Integer) Integer)) (y Integer)] : List|Integer|
  [let-in [[x '[1 2 3 4 y]]]
    [map Integer Integer f x]]]

[print List|Integer| [abc [lambda [(a Integer)] : Integer [+ a 1]] 2]]
[print String "test"]
    
