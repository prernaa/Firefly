open Stack

type element = 
		Asn_Op
	|	Int of int
	|   Flt of float
	|	Id of string
	|	Vec2_Op
	|	Add_Op
	|	On_Op
	|	Off_Op

let stck = Stack.create ()
let a = Stack.push (Int(1),"one","int") stck
let a = Stack.pop stck

let thrd (x,y,z) = z	

let evalTuple (x,y,z) = (match x with
		Int(v) ->  Stack.push (x,y,z) (stck)
	| 	Flt(v) -> Stack.push (x,y,z) (stck)
	|	Add_Op	-> let t1 = Stack.pop stck and t2 = Stack.pop stck in
					(
						let v1 = (thrd t1) and v2 = (thrd t2) in
						(
							if (v1 != v2) then
							( 
								raise ( Failure ("Type mismatch: " ^ (v1) ^ " and " ^ (v2)) ) 
							)
							else
							(
								Stack.push (x,y,v1) stck
							)
						)
					)
	|	_ -> ()
	)

let sa lst g = 	g.(9) <- ("alex","name");
				Stack.clear stck;
				List.iter evalTuple lst;
				(* evalTuple (Int(3),"three","int"); *)
				lst 
  
	