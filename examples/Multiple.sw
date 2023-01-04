[print !Integer [+ 1 2]]

[print !Integer
  [fold !Integer !Integer
    [lambda [(element Integer) (accumulator Integer)] : Integer [* element accumulator]]
    1 
    '[1 2 3 4]]]

[print !Integer [* 5 6]]

[print !|List Integer|
  [let-in [[x '[1 2 3]]
           [y [lambda [(a Integer)] [* a 5]]]]
     [map !Integer !Integer y x]]]

[print !Integer [+ 11 1]]