[print
 !Integer
 [fold-back
   !Integer
   !Integer
   [lambda [(element Integer)
            (accumulator Integer)] 
 	    : Integer
 	    [- element accumulator]]
   0
   '[1 2 3 4]]]