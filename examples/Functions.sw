(x: Integer)

'T -> 'T -> Integer

[lambda [(arg (*))] arg]

[when] = [if] without else
[unless]

'[1 2 3 4 5]
[(abc (-> * *) *)]

[define abc (x Integer) 1]

[asdfa;skldjfa;sldkfjas;dfklasf]


[let-in [[x 1]
	     [x 2]
	     [y [+ x 1]]]
	[+ x 1]]

[let-fun abc [(x Integer)] 
	[+ x 1]]

[let+ [[abc 1]
       [def abc]
	   [ghj def]]
	[+ abc def]]

-- Unzip bindings
-- Pick expression and evaluate considering
--  - Or you halt. If that's the case back track to try again the hanging ones
--  - Or you stop at a point of having to use something that is not in the environment. Save that and skip
-- If at the end you have hanging ones, then throw error

[let* [[abc 1]]
	[let* [[abc ghj]
		   [def 1]
		   [ghj def]
		   [abc ???]]
		   [+ abc def]]]
		
fun (arg: forall T. (T -> Integer)) -> arg

[[lambda
	[(x *)
	 (y (-> (* *) *))
	 :Integer]
	 BODY]

!abc

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