open Stack

type element = 
		Asn_Op
	|	DAsn_Op
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

let tempStack = Stack.create ()
let a = Stack.push (Int(1),"one","int") tempStack
let a = Stack.pop tempStack

let semStack = Stack.create ()
let a = Stack.push (Int(1),"one","int") semStack
let a = Stack.pop semStack

let frst (x,y,z) = x
let scnd (x,y,z) = y
let thrd (x,y,z) = z	

let evalTuple (x,y,z) g i = (match x with
		Int(v) ->  Stack.push (x,y,z) (tempStack)
	| 	Flt(v) -> Stack.push (x,y,z) (tempStack)
	|	Add_Op	| Minus_Op -> 
					let t1 = Stack.pop tempStack and t2 = Stack.pop tempStack in
					(
						let v1 = (thrd t1) and v2 = (thrd t2) in
						(
							if (v1 != v2) then
							( 
								raise ( Failure ("Type mismatch: " ^ (v1) ^ " and " ^ (v2)) ) 
							)
							else
							(
								Stack.push t2 semStack;
								Stack.push t1 semStack;
								Stack.push (x,y,v1) tempStack
							)
						)
					)
	|	LessThan_Op	| LessThanEq_Op | GreaterThan_Op | GreaterThanEq_Op | 
		 EqualsTo_Op	->
					let t1 = Stack.pop tempStack and t2 = Stack.pop tempStack in
					(
						let v1 = (thrd t1) and v2 = (thrd t2) in
						(
							if (v1 != v2) then
							( 
								raise ( Failure ("Type mismatch: " ^ (v1) ^ " and " ^ (v2)) ) 
							)
							else
							(
								Stack.push (x,y,v1) tempStack
							)
						)
					)
	|	Id(v)	-> 		if List.exists (fun s -> (fst s) = "ID(" ^ v ^ ")") (Array.to_list g)
						then 
						(
							let f = List.find (fun s -> (fst s) = "ID(" ^ v ^ ")") (Array.to_list g) in							
							Stack.push (x,y,(snd f)) tempStack
						)
						else
						(
							(* List.iter (fun s -> print_endline ("Iter: " ^ (fst s))) (Array.to_list g); *)
							(*
							g.(!i) <- (v, "TypeToInfer");							
							i := !i + 1;
							*)
							Stack.push (x,y,z) tempStack
						)				
	|	Asn_Op	->	let v = Stack.pop tempStack and e = Stack.pop tempStack in
					(
						(* Check v is an actual ID *)
						if let isId = function Id(_) 
							-> true | 
							_ -> false 
						in not (isId (frst v)) then 
						(	
							raise ( Failure ("Invalid assignment: " ^ (scnd v) ^ " is not a variable") ) 						
						)
						else
						(
							Stack.push e semStack;
							if ((thrd v) = "TypeToInfer") then (* Declaration *)
							(
								g.(!i) <- ((scnd v), (thrd e));							
								i := !i + 1;
								print_endline ("Performing declaration for: " ^ (scnd v));
								Stack.push (DAsn_Op, "DAsn", (thrd e)) tempStack;
								Stack.push (frst v, scnd v, thrd e) semStack
							)
							else (* Assignment *)
							(
								if ((thrd v) <> (thrd e)) then
								(
									raise ( Failure ("Type mismatch: " ^ (scnd v) ^ " - assigning " ^ (thrd e) ^ " to " ^ (thrd v)))
								)
								else
								(
									Stack.push (x,y,(thrd v)) tempStack;
									Stack.push v semStack
								)
							);
							
							
							
						)															
						
					)
	|	Vec2_Op	->	let t1 = Stack.pop tempStack and t2 = Stack.pop tempStack in
					(
						let v1 = (thrd t1) and v2 = (thrd t2) in
						(
							if (v1 != v2) then
							( 
								raise ( Failure ("Type mismatch: " ^ (v1) ^ " and " ^ (v2)) ) 
							)
							else
							(
								Stack.push t2 semStack;
								Stack.push t1 semStack;
								Stack.push (x,y,"vec2cpp") tempStack
							)
						)
					)
	|	On_Op	->	let t1 = Stack.pop tempStack and t2 = Stack.pop tempStack in
					(
						let v1 = (thrd t1) and v2 = (thrd t2) in
						(
							if (v2 <> "float") then
							(
								print_endline ("v2 = " ^ (scnd t2) ^ " " ^ (thrd t2));
								raise ( Failure ("Invalid type: " ^ v2 ^ "; left-hand operand of ON must be float"))
							);
							
							if (v1 <> "vec2cpp") then
							(
								raise ( Failure ("Invalid type: " ^ v1 ^ "; right-hand operand of ON must be vec2"))
							);
							
							Stack.push t1 semStack;
							Stack.push t2 semStack;
							Stack.push (x,y,v1) tempStack
						)
					)
	|	_ -> ()
	)

let sa lst g i = 	Stack.clear tempStack;
					Stack.clear semStack;
					List.iter (fun (x) -> evalTuple x g i) lst;
					Stack.push (Stack.pop tempStack) semStack;
					let rec buildSemList (l) = 
						if Stack.is_empty semStack then
							l
						else
						(
							buildSemList ((Stack.pop semStack) ::  l)
						)
					in
					buildSemList []