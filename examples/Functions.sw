[lambda [(F. A (b A)): A]
  b]

(forall T. T)

[lambda [(arg (forall T. (-> (T) Integer)))]
  arg]

[when] = [if] without else
[unless]


[let [[abc 1]
      [def 2]
	  [ghj 3]]
	[+ abc def]]

[let+ [[abc 1]
       [def abc]
	   [ghj def]]
	[+ abc def]]

[let* [[abc ghj]
       [def abc]
	   [ghj def]]
	[+ abc def]]

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
