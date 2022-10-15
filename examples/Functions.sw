[lambda [(F. A (b A)): A]
  b]

(forall T. T)

[lambda [(arg (forall T. (-> (T) Integer)))]
  arg]

fun (arg: forall T. (T -> Integer)) -> arg

[[lambda
	[(x Integer)
	 (y Boolean)
	 (z (-> (Integer Integer) Integer))
	 :Integer]
	 [+ x 1]]
	 
	1
	
	[if [or F F F T]
	    T
	    F]
	    
	[lambda [(a Integer) (b Integer)] [+ a b]]]
