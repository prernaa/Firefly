open Semantics
open Stack
open Printf

(*let file = "output.cpp"
let oc = open_out file*)


let stck = Stack.create ()
let a = Stack.push ("hey") stck
let a = Stack.pop stck

let temp_counter = ref(0)
(*let firefly = ref (0.0,0.0) (* The compiler keeps a track of the firefly's position *) *)

(* We assume that a variable called _firefly is declared the initcpp code *)

let c_statement (x, y, z) ti li oc= match x with		
		Int(v) 	->  (*seek_out oc flen;*)
					(*let flen = out_channel_length oc in 
					seek_out oc flen;
					print_endline("");*)
					fprintf oc "\n\t%s" ("int _t"^ string_of_int(!temp_counter)^ " = "^string_of_int(v)^";");
					Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					temp_counter:=!temp_counter+1;
					()	
	| 	Flt(v) 	-> 	fprintf oc "\n\t%s" ("float _t"^ string_of_int(!temp_counter)^ " = "^string_of_float(v)^";");
					Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					temp_counter:=!temp_counter+1;
					()
	|	Add_Op	-> 	let op1 = Stack.pop stck and op2 = Stack.pop stck in
					fprintf oc "\n\t%s" (z^" _t"^ string_of_int(!temp_counter)^ " = "^op2^" + "^op1^";");
					Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					temp_counter:=!temp_counter+1;
					()
	|	Minus_Op -> let op1 = Stack.pop stck and op2 = Stack.pop stck in
					fprintf oc "\n\t%s" (z^" _t"^ string_of_int(!temp_counter)^ " = "^op2^" - "^op1^";");
					Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					temp_counter:=!temp_counter+1;
					()	
	|	LessThan_Op	-> let op1 = Stack.pop stck and op2 = Stack.pop stck in
					   fprintf oc "\n\t%s" (z^" _t"^ string_of_int(!temp_counter)^ " = "^op2^" < "^op1^";");
					   Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					   temp_counter:=!temp_counter+1;
					   () 	
	|	LessThanEq_Op	-> let op1 = Stack.pop stck and op2 = Stack.pop stck in
					   	   fprintf oc "\n\t%s" (z^" _t"^ string_of_int(!temp_counter)^ " = "^op2^" <= "^op1^";");
					   	   Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					   	   temp_counter:=!temp_counter+1;
					   	   () 
	|   GreaterThan_Op -> let op1 = Stack.pop stck and op2 = Stack.pop stck in
						  fprintf oc "\n\t%s" (z^" _t"^ string_of_int(!temp_counter)^ " = "^op2^" > "^op1^";");
						  Stack.push ("_t"^string_of_int(!temp_counter)) stck;
						  temp_counter:=!temp_counter+1;
						  ()
	|	GreaterThanEq_Op -> let op1 = Stack.pop stck and op2 = Stack.pop stck in
					   	    fprintf oc "\n\t%s" (z^" _t"^ string_of_int(!temp_counter)^ " = "^op2^" >= "^op1^";");
					   	    Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					   	    temp_counter:=!temp_counter+1;
					   	    ()
	|	EqualsTo_Op	-> 	let op1 = Stack.pop stck and op2 = Stack.pop stck in
					   	fprintf oc "\n\t%s" (z^" _t"^ string_of_int(!temp_counter)^ " = "^op2^" == "^op1^";");
					    Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					   	temp_counter:=!temp_counter+1;
					   	()
	|	Vec2_Op	-> 	let op1 = Stack.pop stck and op2 = Stack.pop stck in
					fprintf oc "\n\t%s" (z^" _t"^ string_of_int(!temp_counter)^ " = {"^op2^" , "^op1^"};");
					Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					temp_counter:=!temp_counter+1;
					()
	|	On_Op	-> 	(*STILL INCOMPLETE*)
					let dist = Stack.pop stck and dir = Stack.pop stck in 
					fprintf oc "\n\t%s" ("float _t"^string_of_int(!temp_counter)^" = sqrt(("^dir^".x * "^dir^".x + "^dir^".y * "^dir^".y));
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
					fprintf oc "\n\t%s" (z^" "^ op1 ^ " = "^op2^";");
					Stack.push (op1) stck;
					()
	|	Asn_Op	->  let op1 = Stack.pop stck and op2 = Stack.pop stck in
					fprintf oc "\n\t%s" (op1 ^ " = "^op2^";");
					Stack.push (op1) stck;
					()
	|	Id(v)	-> 	let lenSubs = ((String.length y) -4) in 
					let varname = String.sub (y) (3) lenSubs in 
					Stack.push (varname) stck;
					()
	| 	_ 		->	()	




let generate_c lst ti li oc = 
	Stack.clear stck;
	List.iter (fun (x) -> c_statement x ti li oc) lst; 
	(*List.iter (fun (fs, sn, thr) -> 				
					print_endline ("PPP (" ^ sn ^ "," ^ thr ^ ")")) lst; 	*)
	()