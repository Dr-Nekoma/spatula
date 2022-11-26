[let-in [[x '[1 2 3]]
         [y [lambda [(a Integer)] [+ a 1]]]]
   [map Integer Integer y x]]