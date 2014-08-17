open Stack

(* building blocks of SAST *)
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
	| 	GotoFun of string * int
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
	| 	SetLocal of string
	| 	GetLocal of int
	
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
)
	
(* perform semantic analysis on each syntax element *)
let evalTuple (x,y,z) g i f fi = (match x with
		Int(v) ->  Stack.push (x,y,z) (tempStack);Stack.push (x,y,z) (semStack)
	| 	Flt(v) -> Stack.push (x,y,z) (tempStack);Stack.push (x,y,z) (semStack)
	|	Add_Op	| Minus_Op | Multiply_Op | Divide_Op -> 
					let t1 = checkUndeclaredVar (Stack.pop tempStack) 
					and t2 = checkUndeclaredVar (Stack.pop tempStack) in
					(
						let v1 = (thrd t1) and v2 = (thrd t2) in
						(
							if (v1 <> v2) && (v1 <> "pointer") && (v2 <> "pointer") then
							( 
								raise ( Failure ("Type mismatch: " ^ (v1) ^ " and " ^ (v2)) ) 
							)
							else
							(
								if (not (v1 = "vec2cpp" or v1 = "int" or v1 = "float" or v1 = "pointer")) then
								(
									raise ( Failure ("Invalid type: " ^ (v1) ^ ": Arithmetic operator must take int, float or vec2"))
								)
								else
								(									
									Stack.push (x,y,v1) tempStack
									;Stack.push (x,y,v1) semStack 
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
							if (v1 <> v2) && (v1 <> "pointer") && (v2 <> "pointer") then
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
									Stack.push (x,y,z) tempStack
									;Stack.push (x,y,z) semStack
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
							;Stack.push (x,y,z) semStack 
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
							Stack.push (x,y,z) tempStack
							(* We do NOT add ID to semantic stack because we don't know its type in this case. *)
						)				
	|	Asn_Op	->	let v = Stack.pop tempStack and e = checkUndeclaredVar(Stack.pop tempStack) in
					(						
						if ((thrd v) = "TypeToInfer") then (* Declaration of new var *)
						(
							g.(!i) <- ((scnd v), (thrd e));							
							i := !i + 1;					
							Stack.push (DAsn_Op, "DAsn", (thrd e)) tempStack
							;Stack.push (frst v, scnd v, thrd e) semStack 
							;Stack.push (DAsn_Op, "DAsn", (thrd e)) semStack							
						)
						else (* Assignment of existing var *)
						(
							if ((thrd v) <> (thrd e)) && (thrd v <> "pointer") && (thrd e <> "pointer") then
							(
								raise ( Failure ("Type mismatch: " ^ (scnd v) ^ " - assigning " ^ (thrd e) ^ " to " ^ (thrd v)))
							)
							else
							(
								Stack.push (x,y,(thrd v)) tempStack;
								Stack.push (x,y,(thrd v)) semStack (* Temporary add!!! *)
							)
						)
					)
	|	Vec2_Op	->	let t1 = checkUndeclaredVar(Stack.pop tempStack) 
					and t2 = checkUndeclaredVar(Stack.pop tempStack) in
					(
						let v1 = (thrd t1) and v2 = (thrd t2) in
						(
							if (v1 <> v2) && (v1 <> "pointer") && (v2 <> "pointer") then
							( 
								raise ( Failure ("Type mismatch: " ^ (v1) ^ " and " ^ (v2)) ) 
							)
							else
							(								
								Stack.push (x,y,"vec2cpp") tempStack
								;Stack.push (x,y,"vec2cpp") semStack
							)
						)
					)
	|	On_Op	->	let t1 = checkUndeclaredVar(Stack.pop tempStack) 
					and t2 = checkUndeclaredVar(Stack.pop tempStack) in
					(
						let v1 = (thrd t1) and v2 = (thrd t2) in
						(
							if (v2 <> "float") && (v2 <> "pointer") then
							(
								raise ( Failure ("Invalid type: " ^ v2 ^ "; left-hand operand of ON must be float"))
							);
							
							if (v1 <> "vec2cpp") then
							(
								raise ( Failure ("Invalid type: " ^ v1 ^ "; right-hand operand of ON must be vec2"))
							);														
							Stack.push (x,y,v1) tempStack
							;Stack.push (x,y,v1) semStack 
						)
					)
	|   Off_Op  ->  let t1 = checkUndeclaredVar(Stack.pop tempStack) 
					and t2 = checkUndeclaredVar(Stack.pop tempStack) in
					(
						let v1 = (thrd t1) and v2 = (thrd t2) in
						(
							if (v2 <> "float") && (v2 <> "pointer") then
							(
								raise ( Failure ("Invalid type: " ^ v2 ^ "; left-hand operand of OFF must be float"))
							);
							
							if (v1 <> "vec2cpp") then
							(
								raise ( Failure ("Invalid type: " ^ v1 ^ "; right-hand operand of OFF must be vec2"))
							);														
							Stack.push (x,y,v1) tempStack
							;Stack.push (x,y,v1) semStack
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
								Stack.push (x,y,z) semStack;
							)
	|	EndWhile_Op	->	Stack.push (x,y,z) semStack;
	|	Endfdef	->	Stack.push (x,y,z) semStack;
	|	Goto(i)	->	Stack.push (x, y, z) semStack;																
	|	Goto(s)	->	Stack.push (x, y, z) semStack;
	|	GotoFun(fn, argcount) ->	(* Make sure function has been defined *)
						if List.exists (fun s -> (fst s) = fn) (Array.to_list f) then 
						(
							let m = List.find (fun s -> (fst s) = fn) (Array.to_list f) in
							(* Make sure function called with right number of arguments *)
							if snd m <> argcount then 
								raise ( Failure ("Invalid number of arguments for " ^ fn) )
							else Stack.push (x, y, "void") semStack
						)
						else
						(
							raise ( Failure ("Calling undefined function: " ^ fn))
						)
	|	GotoReturn	->	Stack.push (x, y, z) semStack;																
	|	Lbl(i)	->	Stack.push (x, y, z) semStack;
	|	Flbl(s)	->	Stack.push (x, y, z) semStack;																
	| 	SetReturn(i)	->	Stack.push (x, y, z) semStack;
	|	SetLocal(fn)	->	Stack.push (x, y, z) semStack;
	|	GetLocal(i)		->	Stack.push (x, y, z) tempStack;
							Stack.push (x, y, z) semStack;
	|	_ -> ()
	)

let sa lst g i f fi = 	Stack.clear tempStack;
						Stack.clear semStack;
						
						(* Printing Syntactic Stack *)
						print_endline "*****************";
						print_endline "SYNTACTIC STACK";
						print_endline "*****************";
						List.iter (fun (fs, sn, thr) -> 				
							print_endline (" (" ^ sn ^ "," ^ thr ^ ")")) lst; 					
						List.iter (fun (x) -> evalTuple x g i f fi) lst;										
						
						(* Convert SAST stack to SAST list *)
						let rec buildSemList (l) = 
							if Stack.is_empty semStack then
								l
							else
							(
								buildSemList ((Stack.pop semStack) ::  l)
							)
						in
						buildSemList []