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
	|	Multiply_Op
	|	Divide_Op
	|	On_Op
	|	Off_Op
	|	LessThan_Op
	|	LessThanEq_Op
	|	GreaterThan_Op
	|	GreaterThanEq_Op
	|	EqualsTo_Op
	|	NotEqualsTo_Op
	|	Or_Op of int
	|	OrDone_Op
	|	And_Op of int
	|	AndDone_Op
	|	Not_Op
	|	If_Op of int
	|	EndIf_Op	
	| 	Goto of int
	| 	GotoFun of string
	|	GotoReturn
	|	Lbl of int
	|	Flbl of string
	|	While_Op of int
	|	EndWhile_Op
	|   Sqrt
	|	Sin_Op
	|	Cos_Op
	|	Getx_Op
	|	Gety_Op
	|   Endfdef 
	| 	SetReturn of int
	
let tempStack = Stack.create ()
let a = Stack.push (Int(1),"one","int") tempStack
let a = Stack.pop tempStack

let semStack = Stack.create ()
let a = Stack.push (Int(1),"one","int") semStack
let a = Stack.pop semStack

let frst (x,y,z) = x
let scnd (x,y,z) = y
let thrd (x,y,z) = z	

(* This function takes a tuple; if the tuple refers to an undeclared var, throw an error.
Otherwise, return the tuple. *)
let checkUndeclaredVar v =
(
	match (frst v) with 
		Id(_) when (thrd v) = "TypeToInfer" -> 
			raise ( Failure ("Variable " ^ (String.sub (scnd v) (3) ((String.length (scnd v)) -4)) ^ " is undeclared!") ) | _ -> ();
	v
	
	(*
	if ((String.length (scnd v)) > 3) then (* Check we have a possible variable *)
	(
		let idprefix = String.sub (scnd v) (0) (3) and 
		varname = String.sub (scnd v) (3) ((String.length (scnd v)) -4) in
		if ((idprefix = "ID(") && ((thrd v) = "TypeToInfer")) then
		(
			raise ( Failure ("Variable " ^ (varname) ^ " is undeclared!") )
		)
	);*)
)
	

let evalTuple (x,y,z) g i = (match x with
		Int(v) ->  Stack.push (x,y,z) (tempStack);Stack.push (x,y,z) (semStack)
	| 	Flt(v) -> Stack.push (x,y,z) (tempStack);Stack.push (x,y,z) (semStack)
	|	Add_Op	| Minus_Op | Multiply_Op | Divide_Op -> 
					let t1 = checkUndeclaredVar (Stack.pop tempStack) 
					and t2 = checkUndeclaredVar (Stack.pop tempStack) in
					(
						let v1 = (thrd t1) and v2 = (thrd t2) in
						(
							if (v1 <> v2) then
							( 
								raise ( Failure ("Type mismatch: " ^ (v1) ^ " and " ^ (v2)) ) 
							)
							else
							(
								if (not (v1 = "vec2cpp" or v1 = "int" or v1 = "float")) then
								(
									raise ( Failure ("Invalid type: " ^ (v1) ^ ": Arithmetic operator must take int, float or vec2"))
								)
								else
								(
									(*Stack.push t2 semStack;*) (* temporary removed!!! *)
									(*Stack.push t1 semStack;*) (* temporary removed!!! *)
									Stack.push (x,y,v1) tempStack
									;Stack.push (x,y,v1) semStack (* temporary add! *)
								)
							)
						)
					)
	|	LessThan_Op	| LessThanEq_Op | GreaterThan_Op | GreaterThanEq_Op | 
		 EqualsTo_Op | NotEqualsTo_Op	->
					let t1 = checkUndeclaredVar (Stack.pop tempStack) 
					and t2 = checkUndeclaredVar (Stack.pop tempStack) in
					(
						let v1 = (thrd t1) and v2 = (thrd t2) in
						(
							if (v1 <> v2) then
							( 
								raise ( Failure ("Type mismatch: " ^ (v1) ^ " and " ^ (v2)) ) 
							)
							else
							(
								if (not (v1 = "int" or v1 = "float")) then
								(
									raise ( Failure ("Invalid type: " ^ (v1) ^ ": Relational operator must take int or float."))
								)
								else
								(
									(*Stack.push t2 semStack;*) (* temporary removed!!! *)
									(*Stack.push t1 semStack;*) (* temporary removed!!! *)
									Stack.push (x,y,z) tempStack
									;Stack.push (x,y,z) semStack (* temporary add!! *)
								)
							)
						)
					)
	|	OrDone_Op | AndDone_Op	->
					let t1 = checkUndeclaredVar (Stack.pop tempStack) and tNone = Stack.pop tempStack 
					and t2 = checkUndeclaredVar (Stack.pop tempStack)
					in
					(
						let v1 = (thrd t1) and v2 = (thrd t2) in
						(
							if (v2 <> "bool") then
							(
								if (x = OrDone_Op) then
									raise ( Failure ("Invalid type: " ^ v2 ^ "; left-hand operand of || must be bool"))
								else
									raise ( Failure ("Invalid type: " ^ v2 ^ "; left-hand operand of && must be bool"))
							);
							
							if (v1 <> "bool") then
							(
								if (x = OrDone_Op) then
									raise ( Failure ("Invalid type: " ^ v1 ^ "; right-hand operand of || must be bool"))
								else
									raise ( Failure ("Invalid type: " ^ v1 ^ "; right-hand operand of && must be bool"))
							);
							
							Stack.push (x,y,z) tempStack
							;Stack.push (x,y,z) semStack (* temporary add!! *)
						)
					)
	|	Or_Op(i) | And_Op(i) ->	Stack.push (x,y,z) tempStack; Stack.push (x,y,z) semStack
	|	Not_Op	->		let t1 = checkUndeclaredVar (Stack.pop tempStack) in
						(
							let v1 = (thrd t1) in
							(
								if (v1 <> "bool") then
								(
									raise ( Failure ("Invalid type: " ^ v1 ^ "; NOT must be applied to a bool.") )
								)
								else
								(
									Stack.push (x,y,z) tempStack
									;Stack.push (x,y,z) semStack
								)
							)
						)
	|   Sqrt ->  		let t1 = checkUndeclaredVar (Stack.pop tempStack) in
						(
							let v1 = (thrd t1) in
							(
								if (v1 <> "int" && v1 <> "float") then
								(
									raise ( Failure ("Invalid type: " ^ v1 ^ "; Sqrt can only be applied to an int or float.") )
								)
								else
								(
									Stack.push (x,y,v1) tempStack
									;Stack.push (x,y,v1) semStack
								)
							)
						)
	|   Sin_Op ->  		let t1 = checkUndeclaredVar (Stack.pop tempStack) in
						(
							let v1 = (thrd t1) in
							(
								if (v1 <> "int" && v1 <> "float") then
								(
									raise ( Failure ("Invalid type: " ^ v1 ^ "; Sqrt can only be applied to an int or float.") )
								)
								else
								(
									Stack.push (x,y,z) tempStack
									;Stack.push (x,y,z) semStack
								)
							)
						)
	|   Cos_Op ->  		let t1 = checkUndeclaredVar (Stack.pop tempStack) in
						(
							let v1 = (thrd t1) in
							(
								if (v1 <> "int" && v1 <> "float") then
								(
									raise ( Failure ("Invalid type: " ^ v1 ^ "; Sqrt can only be applied to an int or float.") )
								)
								else
								(
									Stack.push (x,y,z) tempStack
									;Stack.push (x,y,z) semStack
								)
							)
						)
	|   Getx_Op ->  	let t1 = checkUndeclaredVar (Stack.pop tempStack) in
						(
							let v1 = (thrd t1) in
							(
								if (v1 <> "vec2cpp") then
								(
									raise ( Failure ("Invalid type: " ^ v1 ^ "; You can only get x-element of a vec2.") )
								)
								else
								(
									Stack.push (x,y,z) tempStack
									;Stack.push (x,y,z) semStack
								)
							)
						)
	|   Gety_Op ->  	let t1 = checkUndeclaredVar (Stack.pop tempStack) in
						(
							let v1 = (thrd t1) in
							(
								if (v1 <> "vec2cpp") then
								(
									raise ( Failure ("Invalid type: " ^ v1 ^ "; You can only get y-element of a vec2.") )
								)
								else
								(
									Stack.push (x,y,z) tempStack
									;Stack.push (x,y,z) semStack
								)
							)
						)
	|	Id(v)	-> 		if List.exists (fun s -> (fst s) = "ID(" ^ v ^ ")") (Array.to_list g)
						then 
						(
							let f = List.find (fun s -> (fst s) = "ID(" ^ v ^ ")") (Array.to_list g) in							
							Stack.push (x,y,(snd f)) tempStack
							;Stack.push (x,y,(snd f)) semStack (* temporary add!! *)
							(* ^ We can add ID to semantic stack because we already know its type in this case. *)
						)
						else
						(
							(* List.iter (fun s -> print_endline ("Iter: " ^ (fst s))) (Array.to_list g); *)
							(*
							g.(!i) <- (v, "TypeToInfer");							
							i := !i + 1;
							*)
							Stack.push (x,y,z) tempStack
							(* We do NOT add ID to semantic stack because we don't know its type in this case. *)
						)				
	|	Asn_Op	->	let v = Stack.pop tempStack and e = checkUndeclaredVar(Stack.pop tempStack) in
					(
						(*
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
						*)
						
						(*Stack.push e semStack; *) (* temporary removal!!! *)
						(* ^ removed this line because expressions now push themselves to sem stack *)
						if ((thrd v) = "TypeToInfer") then (* Declaration *)
						(
							g.(!i) <- ((scnd v), (thrd e));							
							i := !i + 1;
							(*print_endline ("Performing declaration for: " ^ (scnd v));*)
							Stack.push (DAsn_Op, "DAsn", (thrd e)) tempStack
							;Stack.push (frst v, scnd v, thrd e) semStack 
							;Stack.push (DAsn_Op, "DAsn", (thrd e)) semStack (* temporary add!!! *)
							(* ^ added this line because expressions now push themselves to sem stack *)
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
								Stack.push (x,y,(thrd v)) semStack (* Temporary add!!! *)
								(* ^ added this line because expressions now push themselves to sem stack *)
								(*;Stack.push v semStack*) (* Temporary removal!!!*)
								(* ^ removed this line because an existing ID pushes itself to sem stack *)
							)
						)
					)
	|	Vec2_Op	->	let t1 = checkUndeclaredVar(Stack.pop tempStack) 
					and t2 = checkUndeclaredVar(Stack.pop tempStack) in
					(
						let v1 = (thrd t1) and v2 = (thrd t2) in
						(
							if (v1 <> v2) then
							( 
								raise ( Failure ("Type mismatch: " ^ (v1) ^ " and " ^ (v2)) ) 
							)
							else
							(
								(*Stack.push t2 semStack;*) (* temporary removed!!! *)
								(*Stack.push t1 semStack;*) (* temporary removed!!! *)
								Stack.push (x,y,"vec2cpp") tempStack
								;Stack.push (x,y,"vec2cpp") semStack (* temporary add! *)
							)
						)
					)
	|	On_Op	->	let t1 = checkUndeclaredVar(Stack.pop tempStack) 
					and t2 = checkUndeclaredVar(Stack.pop tempStack) in
					(
						let v1 = (thrd t1) and v2 = (thrd t2) in
						(
							if (v2 <> "float") then
							(
								raise ( Failure ("Invalid type: " ^ v2 ^ "; left-hand operand of ON must be float"))
							);
							
							if (v1 <> "vec2cpp") then
							(
								raise ( Failure ("Invalid type: " ^ v1 ^ "; right-hand operand of ON must be vec2"))
							);
							
							(*Stack.push t2 semStack;*) (*t1 is vec2*) (* temporary removed!!! *)
							(*Stack.push t1 semStack;*) (*t2 is distance*) (* temporary removed!!! *)
							(*So, vec2 is pushed before distance*)
							Stack.push (x,y,v1) tempStack
							;Stack.push (x,y,v1) semStack (* temporary add!!! *)
						)
					)
	|   Off_Op  ->  let t1 = checkUndeclaredVar(Stack.pop tempStack) 
					and t2 = checkUndeclaredVar(Stack.pop tempStack) in
					(
						let v1 = (thrd t1) and v2 = (thrd t2) in
						(
							if (v2 <> "float") then
							(
								raise ( Failure ("Invalid type: " ^ v2 ^ "; left-hand operand of OFF must be float"))
							);
							
							if (v1 <> "vec2cpp") then
							(
								raise ( Failure ("Invalid type: " ^ v1 ^ "; right-hand operand of OFF must be vec2"))
							);
							
							(*Stack.push t2 semStack;*) (*t1 is vec2*) (* temporary removed!!! *)
							(*Stack.push t1 semStack;*) (*t2 is distance*) (* temporary removed!!! *)
							(*So, vec2 is pushed before distance*)
							Stack.push (x,y,v1) tempStack
							;Stack.push (x,y,v1) semStack (* temporary add!!! *)
						)
					)
	|	If_Op(i)-> 	let t1 = checkUndeclaredVar(Stack.pop tempStack) in
						let v1 = (thrd t1) in
							if (v1 <> "bool") then
							(								
								raise ( Failure ("Invalid type: " ^ v1 ^ "; conditional expression for IF must be of type bool."))
							)
							else
							(
								(*Stack.push t1 semStack;*)																
								Stack.push (x,y,z) semStack;
							)
	|	EndIf_Op	->	Stack.push (x,y,z) semStack;
	|	While_Op(i)-> 	let t1 = checkUndeclaredVar(Stack.pop tempStack) in
						let v1 = (thrd t1) in
							if (v1 <> "bool") then
							(								
								raise ( Failure ("Invalid type: " ^ v1 ^ "; conditional expression for WHILE must be of type bool."))
							)
							else
							(
								(*Stack.push t1 semStack;*)																
								Stack.push (x,y,z) semStack;
							)
	|	EndWhile_Op	->	Stack.push (x,y,z) semStack;
	|	Endfdef	->	Stack.push (x,y,z) semStack;
	|	Goto(i)	->	Stack.push (x, y, z) semStack;																
	|	Goto(s)	->	Stack.push (x, y, z) semStack;
	|	GotoFun(fn) -> Stack.push (x, y, z) semStack;
	|	GotoReturn	->	Stack.push (x, y, z) semStack;																
	|	Lbl(i)	->	Stack.push (x, y, z) semStack;
	|	Flbl(s)	->	Stack.push (x, y, z) semStack;																
	| 	SetReturn(i)	->	Stack.push (x, y, z) semStack;
	|	_ -> ()
	)

let sa lst g i = 	Stack.clear tempStack;
					Stack.clear semStack;
					
					(* Printing Syntactic Stack *)
					print_endline "*****************";
					print_endline "SYNTACTIC STACK";
					print_endline "*****************";
					List.iter (fun (fs, sn, thr) -> 				
						print_endline (" (" ^ sn ^ "," ^ thr ^ ")")) lst; 
					
					List.iter (fun (x) -> evalTuple x g i) lst;					
					(* Stack.push (Stack.pop tempStack) semStack; *) (* temporary removed !!! *)
					(* ^ removed this because expressions push themselves onto sem stack now. *)
					let rec buildSemList (l) = 
						if Stack.is_empty semStack then
							l
						else
						(
							buildSemList ((Stack.pop semStack) ::  l)
						)
					in
					buildSemList []