open Semantics

let c_statement (x,y,z) g ti li = match x with
		Int(v) 	-> 	print_endline ("int t" ^ string_of_int !ti ^ " = " ^ string_of_int v); 
		ti := !ti + 1; 
		()
	| 	Flt(v) 	-> 	print_endline ("int t" ^ string_of_int !ti ^ " = " ^ string_of_float v); 
					ti := !ti + 1;
					()
	|	Add_Op	-> 	()
	|	Id(v)	-> 	()				
	|	Asn_Op	->  ()
	| 	_ 		->	()	




let generate_c lst g ti li = List.iter (fun (x) -> c_statement x g ti li) lst; ()