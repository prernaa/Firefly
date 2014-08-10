open Semantics
open Stack

let stck = Stack.create ()
let a = Stack.push (0) stck
let a = Stack.pop stck

let c_statement (x, y, z) g ti li = match x with		
		Int(v) 	-> 	print_endline ("int t" ^ string_of_int !ti ^ " = " ^ string_of_int v ^ ";"); 
					ti := !ti + 1; 
					Stack.push (!ti - 1) stck
	| 	Flt(v) 	-> 	print_endline ("float t" ^ string_of_int !ti ^ " = " ^ string_of_float v ^ ";"); 
					ti := !ti + 1;
					Stack.push (!ti - 1) stck
	|	Add_Op	-> 	let i1 = Stack.pop stck and i2 = Stack.pop stck in
					print_endline ("int t" ^ string_of_int !ti ^ " = t" ^ string_of_int i2 ^ " + t" ^ string_of_int i1 ^ ";"); 
					ti := !ti + 1;
					Stack.push (!ti - 1) stck
	|	Minus_Op	-> 	let i1 = Stack.pop stck and i2 = Stack.pop stck in
					print_endline ("int t" ^ string_of_int !ti ^ " = t" ^ string_of_int i2 ^ " - t" ^ string_of_int i1 ^ ";"); 
					ti := !ti + 1;
					Stack.push (!ti - 1) stck
	|	LessThan_Op	-> 	let i1 = Stack.pop stck and i2 = Stack.pop stck in
					print_endline ("int t" ^ string_of_int !ti ^ " = t" ^ string_of_int i2 ^ " < t" ^ string_of_int i1 ^ ";"); 
					ti := !ti + 1;
					Stack.push (!ti - 1) stck
	|	LessThanEq_Op	-> 	let i1 = Stack.pop stck and i2 = Stack.pop stck in
					print_endline ("int t" ^ string_of_int !ti ^ " = t" ^ string_of_int i2 ^ " <= t" ^ string_of_int i1 ^ ";"); 
					ti := !ti + 1;
					Stack.push (!ti - 1) stck
	|	GreaterThan_Op	-> 	let i1 = Stack.pop stck and i2 = Stack.pop stck in
					print_endline ("int t" ^ string_of_int !ti ^ " = t" ^ string_of_int i2 ^ " > t" ^ string_of_int i1 ^ ";"); 
					ti := !ti + 1;
					Stack.push (!ti - 1) stck
	|	GreaterThanEq_Op	-> 	let i1 = Stack.pop stck and i2 = Stack.pop stck in
					print_endline ("int t" ^ string_of_int !ti ^ " = t" ^ string_of_int i2 ^ " >= t" ^ string_of_int i1 ^ ";"); 
					ti := !ti + 1;
					Stack.push (!ti - 1) stck
	|	EqualsTo_Op	-> 	let i1 = Stack.pop stck and i2 = Stack.pop stck in
					print_endline ("int t" ^ string_of_int !ti ^ " = t" ^ string_of_int i2 ^ " == t" ^ string_of_int i1 ^ ";"); 
					ti := !ti + 1;
					Stack.push (!ti - 1) stck
	|	Vec2_Op	-> 	let i1 = Stack.pop stck and i2 = Stack.pop stck in
					(* need to get exact c++ syntax for vec2 *)
					print_endline ("vec2 t" ^ string_of_int !ti ^ " = t" ^ string_of_int i2 ^ ", t" ^ string_of_int i1 ^ ";"); 
					ti := !ti + 1;
					Stack.push (!ti - 1) stck
	|	On_Op	-> 	let i1 = Stack.pop stck and i2 = Stack.pop stck in
					(* need to get exact c++ syntax for openGL line and/or define a c++ function -- e.g., c_on *)
					print_endline ("vec2 t" ^ string_of_int !ti ^ " = c_on(t" ^ string_of_int i2 ^ ", t" ^ string_of_int i1 ^ ")" ^ ";"); 
					ti := !ti + 1;
					Stack.push (!ti - 1) stck
	|	Id(v)	-> 	(* need to distinguish between LHS and RHS. Here, I'm treating the RHS case *)
					print_endline ("inferred_type t" ^ string_of_int !ti ^ " = glb_" ^ v ^ ";"); 
					ti := !ti + 1;
					Stack.push (!ti - 1) stck
	|	Asn_Op	->  () (* we need the semantic tree to split up this operator into declaration and assignment *)
	| 	_ 		->	()	




let generate_c lst g ti li = List.iter (fun (x) -> c_statement x g ti li) lst; ()