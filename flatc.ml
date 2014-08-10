open Semantics
open Stack

let stck = Stack.create ()
let a = Stack.push (0) stck
let a = Stack.pop stck

let c_statement (x, y, z) g ti li = match x with		
		Int(v) 	-> 	print_endline ("int t" ^ string_of_int !ti ^ " = " ^ string_of_int v ^ ";"); 
					ti := !ti + 1; 
					Stack.push (!ti - 1) stck
	| 	Flt(v) 	-> 	print_endline ("int t" ^ string_of_int !ti ^ " = " ^ string_of_float v ^ ";"); 
					ti := !ti + 1;
					Stack.push (!ti - 1) stck
	|	Add_Op	-> 	let i1 = Stack.pop stck and i2 = Stack.pop stck in
					print_endline ("int t" ^ string_of_int !ti ^ " = t" ^ string_of_int i1 ^ " + t" ^ string_of_int i2 ^ ";"); 
					ti := !ti + 1;
					Stack.push (!ti - 1) stck
	|	Vec2_Op	-> 	let i1 = Stack.pop stck and i2 = Stack.pop stck in
					(* need to get exact c++ syntax for vec2 *)
					print_endline ("vec2 t" ^ string_of_int !ti ^ " = t" ^ string_of_int i2 ^ ", t" ^ string_of_int i1 ^ ";"); 
					ti := !ti + 1;
					Stack.push (!ti - 1) stck
	|	Id(v)	-> 	()				
	|	Asn_Op	->  ()
	| 	_ 		->	()	




let generate_c lst g ti li = List.iter (fun (x) -> c_statement x g ti li) lst; ()