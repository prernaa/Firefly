open Stack

type element = 
		Asn_Op
	|	Int of int
	|   Flt of float
	|	Id of string
	|	Vec2_Op
	|	Add_Op
	|	Minus_Op
	|	On_Op
	|	Off_Op
	|	LessThan_Op
	|	LessThanEq_Op
	|	GreaterThan_Op
	|	GreaterThanEq_Op
	|	EqualsTo_Op

let stck = Stack.create ()
let a = Stack.push (Int(1),"one","int") stck
let a = Stack.pop stck

let frst (x,y,z) = x
let scnd (x,y,z) = y
let thrd (x,y,z) = z	

let evalTuple (x,y,z) g i = (match x with
		Int(v) ->  Stack.push (x,y,z) (stck)
	| 	Flt(v) -> Stack.push (x,y,z) (stck)
	|	Add_Op	| Minus_Op -> 
					let t1 = Stack.pop stck and t2 = Stack.pop stck in
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
	|	LessThan_Op	| LessThanEq_Op | GreaterThan_Op | GreaterThanEq_Op | 
		 EqualsTo_Op	->
					let t1 = Stack.pop stck and t2 = Stack.pop stck in
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
	|	Id(v)	-> 		if List.exists (fun s -> (fst s) = v) (Array.to_list g)
						then
						(
							let f = List.find (fun s -> (fst s) = v) (Array.to_list g) in							
							Stack.push (x,y,(snd f)) stck
						)
						else
						(
							g.(!i) <- (v, "TypeToInfer");							
							i := !i + 1;
							Stack.push (x,y,z) stck
						)				
	|	Asn_Op	->	let v = Stack.pop stck and e = Stack.pop stck in
					(
						if let isId = function Id(_) -> true | _ -> false in isId (frst v) then 
						(							
							Stack.push (x, y, thrd e) stck
						)															
						else raise ( Failure ("Invalid assignment: " ^ (scnd v) ^ " is not a variable") ) 						
					)
	|	_ -> ()
	)

let sa lst g i = 	g.(!i) <- ("alex","name"); 
					i := !i + 1;
					Stack.clear stck;
					List.iter (fun (x) -> evalTuple x g i) lst;
					[] 
  
	