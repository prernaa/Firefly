open Semantics
open Stack

let stck = Stack.create ()
let a = Stack.push ("hey") stck
let a = Stack.pop stck

let temp_counter = ref(0)
(*let firefly = ref (0.0,0.0) (* The compiler keeps a track of the firefly's position *) *)

(* We assume that a variable called _firefly is declared the initcpp code *)

let c_statement (x, y, z) ti li = match x with		
		Int(v) 	->  print_endline("int _t"^ string_of_int(!temp_counter)^ " = "^string_of_int(v)^";");
					Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					temp_counter:=!temp_counter+1;
					()	
	| 	Flt(v) 	-> 	print_endline("float _t"^ string_of_int(!temp_counter)^ " = "^string_of_float(v)^";");
					Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					temp_counter:=!temp_counter+1;
					()
	|	Add_Op	-> 	let op1 = Stack.pop stck and op2 = Stack.pop stck in
					print_endline(z^" _t"^ string_of_int(!temp_counter)^ " = "^op2^" + "^op1^";");
					Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					temp_counter:=!temp_counter+1;
					()
	|	Minus_Op -> let op1 = Stack.pop stck and op2 = Stack.pop stck in
					print_endline(z^" _t"^ string_of_int(!temp_counter)^ " = "^op2^" - "^op1^";");
					Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					temp_counter:=!temp_counter+1;
					()	
	|	LessThan_Op	-> () 	
	|	LessThanEq_Op	-> ()|	GreaterThan_Op	-> 	()
	|	GreaterThanEq_Op	-> ()	
	|	EqualsTo_Op	-> 	()
	|	Vec2_Op	-> 	let op1 = Stack.pop stck and op2 = Stack.pop stck in
					print_endline(z^" _t"^ string_of_int(!temp_counter)^ " = {"^op2^" , "^op1^"};");
					Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					temp_counter:=!temp_counter+1;
					()
	|	On_Op	-> 	(*STILL INCOMPLETE*)
					let dist = Stack.pop stck and dir = Stack.pop stck in 
					print_endline("float _t"^string_of_int(!temp_counter)^" = sqrt(("^dir^".x * "^dir^".x + "^dir^".y * "^dir^".y));
"^dir^".x = "^dir^".x/_t"^string_of_int(!temp_counter)^";
"^dir^".y = "^dir^".y/_t"^string_of_int(!temp_counter)^";
vec2cpp _t"^string_of_int(!temp_counter+1)^" = {"^dist^" * "^dir^".x + _ff.x , "^dist^" * "^dir^".y + _ff.y };
glBegin(GL_LINES);
\tglColor3f(0.0, 0.0, 0.0);
\tglVertex2f(_ff.x, _ff.y);
\tglVertex2f(_t"^string_of_int(!temp_counter+1)^".x , _t"^string_of_int(!temp_counter+1)^".y);
_ff.x = _t"^string_of_int(!temp_counter+1)^".x;
_ff.y = _t"^string_of_int(!temp_counter+1)^".y;
glEnd();
");
					temp_counter:=!temp_counter+1;
					temp_counter:=!temp_counter+1;
					()
	|   Off_Op	->  ()
	|   DAsn_Op ->  let op1 = Stack.pop stck and op2 = Stack.pop stck in
					print_endline(z^" "^ op1 ^ " = "^op2^";");
					Stack.push (op1) stck;
					()
	|	Asn_Op	->  let op1 = Stack.pop stck and op2 = Stack.pop stck in
					print_endline(op1 ^ " = "^op2^";");
					Stack.push (op1) stck;
					()
	|	Id(v)	-> 	let lenSubs = ((String.length y) -4) in 
					let varname = String.sub (y) (3) lenSubs in 
					Stack.push (varname) stck;
					()
	| 	_ 		->	()	




let generate_c lst ti li = 
	Stack.clear stck;
	List.iter (fun (x) -> c_statement x ti li) lst; 
	(*List.iter (fun (fs, sn, thr) -> 				
					print_endline ("PPP (" ^ sn ^ "," ^ thr ^ ")")) lst; 	*)
	()