open Semantics
open Stack
open Printf

(*let file = "output.cpp"
let oc = open_out file*)


let stck = Stack.create ()
let a = Stack.push ("hey") stck
let a = Stack.pop stck

let lstck = Stack.create ()
let a = Stack.push (-1) lstck
(*let a = Stack.pop lstck*)

let temp_counter = ref(0)
let lbl_counter = ref(0)

(*let rec globals_to_file g c i oc = 
	if i < c then (
		fprintf oc "\n\t%s" ((snd g.(i))^" "^(fst g.(i))^";\n");
		globals_to_file g c (i+1) oc
	)	
 	else ()*)

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
					let dir = Stack.pop stck and dist = Stack.pop stck in 
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
					Stack.push ("_t"^string_of_int(!temp_counter-1)) stck;
					()
	|   Off_Op	->  ()
	|   DAsn_Op ->  let op1 = Stack.pop stck and op2 = Stack.pop stck in
					fprintf oc "\n\t%s" (z^" "^ op1 ^ " = "^op2^";");
					(*fprintf oc "\n\t%s" (op1 ^ " = "^op2^";");*)
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
	|	If_Op	->	let e = Stack.pop stck and ll = Stack.pop lstck in 
						fprintf oc "\n\t%s" ("if (" ^ e ^ ") { goto _L" ^ string_of_int(!lbl_counter) ^ ";}{");
						Stack.push (!lbl_counter) lstck;
						lbl_counter := !lbl_counter + 2
	|	Goto(i)	->	let l = Stack.pop lstck in (
						fprintf oc "\n\t%s" ("goto _L" ^ string_of_int(l+1)^";");						
						Stack.push (l) lstck;
					)
	|	Lbl(i)	->	let l = Stack.pop lstck in 
					( 
						if i=0 then fprintf oc "\n\t%s" ("} _L"^string_of_int(l+i) ^ ": {");
						if i=1 then fprintf oc "\n\t%s" ("} _L"^string_of_int(l+i) ^ ": ");
						print_endline ("JJJJJJJJ" ^ y);
						Stack.push l lstck;
					)
	| 	_ 		->	()	




let generate_c lst ti li oc globals globals_count= 
	Stack.clear stck;
	(*globals_to_file globals globals_count 0 oc;*)
	List.iter (fun (fs, sn, thr) -> 				
					print_endline ("TTT (" ^ sn ^ "," ^ thr ^ ")")) lst; 
	List.iter (fun (x) -> c_statement x ti li oc) lst; 
	
	()