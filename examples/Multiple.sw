[+ 1 2]

[fold Integer Integer
  [lambda [(element Integer) (accumulator Integer)] : Integer [* element accumulator]]
  1
  '[1 2 3 4]]

[* 5 6]

[let-in [[x '[1 2 3]]
         [y [lambda [(a Integer)] [* a 5]]]]
   [map Integer Integer y x]]

[+ 11 1]
