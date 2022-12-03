[lambda [(f (-> (Integer) Integer))
         (g (-> (Integer) Unit))]
   [lambda [(a Integer)] [g [f a]]]]

[let-in [[x '[1 2 3]]
         [y '[4 5 6]]
         [z '[7]]]
   [print List|Integer| [^ [cdr Integer x] z '[[car Integer y]]]]]

[readLines "examples/Read.sw"]   
