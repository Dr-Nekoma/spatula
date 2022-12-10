[defun abc [(f (-> (Integer) Integer)) (y Integer)] : List|Integer|
  [let-in [[x '[1 2 3 4 y]]]
    [map Integer Integer f x]]]
    