[load "examples/Seasoning.sw"]

[defrecord ElfCarrier[(T Star)]
   (currentMax T)
   (current Integer)]

// Part 1

[defalias SingleElfCarrier |ElfCarrier Integer|]

[defun aux-fold-1 [(str-calories String)
                   (accumulator SingleElfCarrier)]
  		   : SingleElfCarrier
  [if [= str-calories ""]
     [let-in [[new-accumulator [setr accumulator [(current 0)]]]
              [candidate [getr accumulator current]]]
        [if [< [getr accumulator currentMax] candidate]
          [setr new-accumulator [(currentMax candidate)]]
          new-accumulator]]
     // Let plus is different!
     [let-plus [[number-calories [string-to-integer str-calories]]
                [new-current [+ number-calories [getr accumulator current]]] // This uses number-calories!
		[new-accumulator [setr accumulator [(current new-current)]]]]
        new-accumulator]]]

[defun part-1 [(input-path String)] : Unit
  [let-in [[lines [read-lines input-path]]
           [initial-accumulator { SingleElfCarrier |(currentMax 0) (current 0)|}]]
     [print !Integer [getr [fold !String !SingleElfCarrier aux-fold-1 initial-accumulator lines] currentMax]]]]

// Part 2

[defalgebraic Triplet (Values Integer Integer Integer)]

[defalias TripleElfCarrier |ElfCarrier Triplet|]

[define first-value -2]
[define second-value -1]
[define third-value 0]

[defun update-triplet [(candidate Integer)
       		       (elf-triplet TripleElfCarrier)]
                       : TripleElfCarrier
      [let-in [[triplet [getr elf-triplet currentMax]]]
	[match triplet
	 [[!Values [first] [second] [third]]
	    [if [and [< first second] [< first third]]
	       [if [< first candidate]
	          [setr elf-triplet [(currentMax [Values candidate second third])]]
		  elf-triplet]
               [if [and [< second first] [< second third]]
		  [if [< second candidate]
		     [setr elf-triplet [(currentMax [Values first candidate third])]]
		     elf-triplet]
		  [if [< third candidate]
		     [setr elf-triplet [(currentMax [Values first second candidate])]]
		     elf-triplet]]]]]]]

[defun aux-fold-2 [(str-calories String)
                   (accumulator TripleElfCarrier)]
  		   : TripleElfCarrier
  [if [= str-calories ""]
     [let-in [[new-accumulator [setr accumulator [(current 0)]]]
              [candidate [getr accumulator current]]]
	[update-triplet candidate new-accumulator]]
     [let-plus [[number-calories [string-to-integer str-calories]]
                [new-current [+ number-calories [getr accumulator current]]]
		[new-accumulator [setr accumulator [(current new-current)]]]]
        new-accumulator]]]

[defun part-2 [(input-path String)] : Unit
  [let-plus [[lines [read-lines input-path]]
             [initial-triplet [Values first-value second-value third-value]]
             [initial-accumulator { TripleElfCarrier |(currentMax initial-triplet) (current 0)|}]
	     [sum [lambda [(t Triplet)] : Integer
	             [match t
		      [[!Values [first] [second] [third]] [+ first second third]]]]]]
     [print !Integer
       [sum [getr [fold !String !TripleElfCarrier aux-fold-2 initial-accumulator lines] currentMax]]]]]

[defun solution [()] : Unit
 [progn
    [part-1 "aoc1-input.txt"]
    [part-2 "aoc1-input.txt"]]]

[solution nil]
