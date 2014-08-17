open Semantics
open Stack
open Printf

(*let file = "output.cpp"
let oc = open_out file*)

let tvarTbl = Hashtbl.create 100 
let a = Hashtbl.add tvarTbl 1 "int"
let a = Hashtbl.clear tvarTbl

let getTempType index = 
(
	if Hashtbl.mem tvarTbl index then
		Hashtbl.find tvarTbl index
	else
		""
)
		
let stck = Stack.create ()
let a = Stack.push ("hey") stck
let a = Stack.pop stck

(*we should delete this if not needed --RA *)
let lstck = Stack.create ()
let a = Stack.push (-1) lstck
(*let a = Stack.pop lstck*)

let temp_counter = ref(0)
(*we should delete this if not needed --RA *)
let lbl_counter = ref(0)

(*let rec globals_to_file g c i oc = 
	if i < c then (
		fprintf oc "\n\t%s" ((snd g.(i))^" "^(fst g.(i))^";\n");
		globals_to_file g c (i+1) oc
	)	
 	else ()*)

(*let firefly = ref (0.0,0.0) (* The compiler keeps a track of the firefly's position *) *)

(* We assume that a variable called _firefly is declared the initcpp code *)

let c_statement (x, y, z) ti li oc tvars = let tempType = getTempType(!temp_counter) in
	let typePrefix = match tempType with "" -> z | _ -> "" in
	let ti = "_t" ^ string_of_int(!temp_counter) in
	let ti_plus1 = "_t" ^ string_of_int(!temp_counter + 1) in
	let ti_plus2 = "_t" ^ string_of_int(!temp_counter + 2) in
	let id_ti = "ID(" ^ ti ^ ")" in				
	let id_ti_plus1 = "ID(" ^ ti_plus1 ^ ")" in
	let id_ti_plus2 = "ID(" ^ ti_plus2 ^ ")" in
	match x with		
		Int(v) 	->  tvars.(!temp_counter) <- (id_ti, typePrefix);
					fprintf oc "\n\t%s" (ti ^ " = " ^ string_of_int(v) ^ ";");
					Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					temp_counter := !temp_counter + 1;
					()	
	| 	Flt(v) 	-> 	tvars.(!temp_counter) <- (id_ti, typePrefix);
					fprintf oc "\n\t%s" (ti ^ " = " ^ string_of_float(v) ^ ";");
					Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					temp_counter:=!temp_counter+1;
					()
	|	Add_Op	-> 	tvars.(!temp_counter) <- (id_ti, typePrefix);
					let op1 = Stack.pop stck and op2 = Stack.pop stck in
					fprintf oc "\n\t%s" (ti ^ " = " ^ op2 ^ " + " ^ op1 ^ ";");
					Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					temp_counter:=!temp_counter+1;
					()
	|	Minus_Op -> tvars.(!temp_counter) <- (id_ti, typePrefix);
					let op1 = Stack.pop stck and op2 = Stack.pop stck in
					fprintf oc "\n\t%s" (ti ^ " = " ^ op2 ^ " - " ^ op1 ^ ";");
					Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					temp_counter:=!temp_counter+1;
					()
	|	Multiply_Op -> tvars.(!temp_counter) <- (id_ti, typePrefix);
					let op1 = Stack.pop stck and op2 = Stack.pop stck in
					fprintf oc "\n\t%s" ("_t"^ string_of_int(!temp_counter)^ " = "^op2^" * "^op1^";");
					Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					temp_counter:=!temp_counter+1;
					()
	|	Divide_Op -> tvars.(!temp_counter) <- (id_ti, typePrefix);
					let op1 = Stack.pop stck and op2 = Stack.pop stck in
					fprintf oc "\n\t%s" ("_t"^ string_of_int(!temp_counter)^ " = "^op2^" / "^op1^";");
					Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					temp_counter:=!temp_counter+1;
					()					
	|	LessThan_Op	-> tvars.(!temp_counter) <- (id_ti, typePrefix);
					let op1 = Stack.pop stck and op2 = Stack.pop stck in
					fprintf oc "\n\t%s" ("_t"^ string_of_int(!temp_counter)^ " = "^op2^" < "^op1^";");
					Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					temp_counter:=!temp_counter+1;
					() 	
	|	LessThanEq_Op	-> tvars.(!temp_counter) <- (id_ti, typePrefix);
							let op1 = Stack.pop stck and op2 = Stack.pop stck in
							fprintf oc "\n\t%s" ("_t"^ string_of_int(!temp_counter)^ " = "^op2^" <= "^op1^";");
							Stack.push ("_t"^string_of_int(!temp_counter)) stck;
							temp_counter:=!temp_counter+1;
							() 
	|   GreaterThan_Op -> 	tvars.(!temp_counter) <- (id_ti, typePrefix);
							let op1 = Stack.pop stck and op2 = Stack.pop stck in
							fprintf oc "\n\t%s" ("_t"^ string_of_int(!temp_counter)^ " = "^op2^" > "^op1^";");
							Stack.push ("_t"^string_of_int(!temp_counter)) stck;
							temp_counter:=!temp_counter+1;
						  ()
	|	GreaterThanEq_Op -> tvars.(!temp_counter) <- (id_ti, typePrefix);
							let op1 = Stack.pop stck and op2 = Stack.pop stck in
					   	    fprintf oc "\n\t%s" ("_t"^ string_of_int(!temp_counter)^ " = "^op2^" >= "^op1^";");
					   	    Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					   	    temp_counter:=!temp_counter+1;
					   	    ()
	|	EqualsTo_Op		-> 	tvars.(!temp_counter) <- (id_ti, typePrefix);
							let op1 = Stack.pop stck and op2 = Stack.pop stck in
							fprintf oc "\n\t%s" ("_t"^ string_of_int(!temp_counter)^ " = "^op2^" == "^op1^";");
							Stack.push ("_t"^string_of_int(!temp_counter)) stck;
							temp_counter:=!temp_counter+1;
							()
	|	NotEqualsTo_Op	-> 	tvars.(!temp_counter) <- (id_ti, typePrefix);
							let op1 = Stack.pop stck and op2 = Stack.pop stck in
							fprintf oc "\n\t%s" ("_t"^ string_of_int(!temp_counter)^ " = "^op2^" != "^op1^";");
							Stack.push ("_t"^string_of_int(!temp_counter)) stck;
							temp_counter:=!temp_counter+1;
							()
	|	Not_Op			->	tvars.(!temp_counter) <- (id_ti, typePrefix); 	
							let op1 = Stack.pop stck in
							fprintf oc "\n\t%s" ("_t"^ string_of_int(!temp_counter)^ " = !"^op1^";");
							Stack.push ("_t"^string_of_int(!temp_counter)) stck;
							temp_counter:=!temp_counter+1;
							()
	|   Sqrt    		->	tvars.(!temp_counter) <- (id_ti, typePrefix);
							let op1 = Stack.pop stck in
							fprintf oc "\n\t%s" ("_t"^ string_of_int(!temp_counter)^ " = sqrt("^op1^");");
							Stack.push ("_t"^string_of_int(!temp_counter)) stck;
							temp_counter:=!temp_counter+1;
							()
	|	Sin_Op		->	tvars.(!temp_counter) <- (id_ti, typePrefix);
						let op1 = Stack.pop stck in
						fprintf oc "\n\t%s" ("_t"^ string_of_int(!temp_counter)^ " = sin(M_PI / 180 * "^op1^");");
					    Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					   	temp_counter:=!temp_counter+1;
					   	()
	|	Cos_Op		->	tvars.(!temp_counter) <- (id_ti, typePrefix);
						let op1 = Stack.pop stck in
						fprintf oc "\n\t%s" ("_t"^ string_of_int(!temp_counter)^ " = cos(M_PI / 180 * "^op1^");");
					    Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					   	temp_counter:=!temp_counter+1;
					   	()
	|	Getx_Op		->	tvars.(!temp_counter) <- (id_ti, typePrefix);
						let op1 = Stack.pop stck in
						fprintf oc "\n\t%s" ("_t"^ string_of_int(!temp_counter)^ " = "^op1^".x;");
					    Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					   	temp_counter:=!temp_counter+1;
					   	()
	|	Gety_Op		->	tvars.(!temp_counter) <- (id_ti, typePrefix);
						let op1 = Stack.pop stck in
						fprintf oc "\n\t%s" ("_t"^ string_of_int(!temp_counter)^ " = "^op1^".y;");
					    Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					   	temp_counter:=!temp_counter+1;
					   	()
	|	Vec2_Op	-> 	tvars.(!temp_counter) <- (id_ti, typePrefix);
					let op1 = Stack.pop stck and op2 = Stack.pop stck in
					fprintf oc "\n\t%s" ("_t"^ string_of_int(!temp_counter)^ ".x = "^op2^";");
					fprintf oc "\n\t%s" ("_t"^ string_of_int(!temp_counter)^ ".y = "^op1^";");
					Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					temp_counter:=!temp_counter+1;
					()
	|	On_Op	-> 	tvars.(!temp_counter) <- (id_ti, "float");
					tvars.(!temp_counter + 1) <- (id_ti_plus1, "vec2cpp");
					tvars.(!temp_counter + 2) <- (id_ti_plus2, "vec2cpp");
					let dir = Stack.pop stck and dist = Stack.pop stck in 
					fprintf oc "\n\t%s" ("_t"^string_of_int(!temp_counter)^" = sqrt(("^dir^".x * "^dir^".x + "^dir^".y * "^dir^".y));	
"^dir^".x = "^dir^".x/_t"^string_of_int(!temp_counter)^";
"^dir^".y = "^dir^".y/_t"^string_of_int(!temp_counter)^";
_t"^string_of_int(!temp_counter+1)^".x = "^dist^" * "^dir^".x + _ff.x;
_t"^string_of_int(!temp_counter+1)^".y = "^dist^" * "^dir^".y + _ff.y;
glBegin(GL_LINES);
\tglColor3f(0.0, 0.0, 0.0);
\tglVertex2f(_ff.x, _ff.y);
\tglVertex2f(_t"^string_of_int(!temp_counter+1)^".x , _t"^string_of_int(!temp_counter+1)^".y);
glEnd();
_ff.x = _t"^string_of_int(!temp_counter+1)^".x;
_ff.y = _t"^string_of_int(!temp_counter+1)^".y;
");
					temp_counter:=!temp_counter+1;
					temp_counter:=!temp_counter+1;
					fprintf oc "\n\t%s" ("_t"^ string_of_int(!temp_counter)^ " = _ff;");
					Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					temp_counter:=!temp_counter+1;
					()
	|   Off_Op	-> 	tvars.(!temp_counter) <- (id_ti, "float");
					tvars.(!temp_counter + 1) <- (id_ti_plus1, "vec2cpp");
					tvars.(!temp_counter + 2) <- (id_ti_plus2, "vec2cpp");
					let dir = Stack.pop stck and dist = Stack.pop stck in 
					fprintf oc "\n\t%s" ("_t"^string_of_int(!temp_counter)^" = sqrt(("^dir^".x * "^dir^".x + "^dir^".y * "^dir^".y));	
"^dir^".x = "^dir^".x/_t"^string_of_int(!temp_counter)^";
"^dir^".y = "^dir^".y/_t"^string_of_int(!temp_counter)^";
_t"^string_of_int(!temp_counter+1)^".x = "^dist^" * "^dir^".x + _ff.x;
_t"^string_of_int(!temp_counter+1)^".y = "^dist^" * "^dir^".y + _ff.y;
_ff.x = _t"^string_of_int(!temp_counter+1)^".x;
_ff.y = _t"^string_of_int(!temp_counter+1)^".y;
");
					temp_counter:=!temp_counter+1;
					temp_counter:=!temp_counter+1;
					fprintf oc "\n\t%s" ("_t"^ string_of_int(!temp_counter)^ " = _ff;");
					Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					temp_counter:=!temp_counter+1;
					()
	|   DAsn_Op ->  
					let op1 = Stack.pop stck and op2 = Stack.pop stck in
					fprintf oc "\n\t%s" (op1 ^ " = " ^ op2 ^ ";");
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
	|	If_Op(i)->	let e = Stack.pop stck in 
						fprintf oc "\n\t%s" ("if (" ^ e ^ ") { goto _L" ^ string_of_int(i) ^ "; }");												
	(*|	EndIf_Op->	fprintf oc "\n\t%s" ("}}") *)
	|	Or_Op(i) ->	let e = Stack.pop stck in 
						fprintf oc "\n\t%s" ("if (" ^ e ^ ") { goto _L" ^ string_of_int(i) ^ "; }");												
    |	And_Op(i) ->	tvars.(!temp_counter) <- (id_ti, typePrefix);
						let e = Stack.pop stck in 
						fprintf oc "\n\t%s" ("bool _t"^string_of_int(!temp_counter+2)^" = false;");
						Hashtbl.replace tvarTbl (!temp_counter+2) "bool";
						fprintf oc "\n\t%s" ("if (!" ^ e ^ ") { goto _L" ^ string_of_int(i) ^ "; }");												
    |	OrDone_Op -> 	tvars.(!temp_counter) <- (id_ti, typePrefix);
						fprintf oc "\n\t%s" ("_t"^ string_of_int(!temp_counter)^ " = _t"^string_of_int(!temp_counter-4)^" || "^"_t"^string_of_int(!temp_counter-1)^";");
					    Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					   	temp_counter:=!temp_counter+1;
    |	AndDone_Op -> 	tvars.(!temp_counter) <- (id_ti, typePrefix);
						fprintf oc "\n\t%s" ("_t"^ string_of_int(!temp_counter)^ " = _t"^string_of_int(!temp_counter-4)^" && "^"_t"^string_of_int(!temp_counter-1)^";");
					    Stack.push ("_t"^string_of_int(!temp_counter)) stck;
					   	temp_counter:=!temp_counter+1;
	|	While_Op(i)	->	let e = Stack.pop stck in 
						fprintf oc "\n\t%s" ("if (!" ^ e ^ ") { goto _L" ^ string_of_int(i) ^ "; }");												
	(*|	EndWhile_Op ->	fprintf oc "\n\t%s" ("}")	*)
	|	Goto(i)		->	fprintf oc "\n\t%s" ("goto _L" ^ string_of_int(i)^";");																
	|	GotoFun(fn, argcount)	->	
						fprintf oc "\n\t%s" ("lv_frameptr = lv_index;");	
						fprintf oc "\n\t%s" ("goto _L" ^ fn ^ ";");	
	|	GotoReturn	->	tvars.(!temp_counter) <- (id_ti, "void *");
						fprintf oc "\n\t%s" ("_t"^string_of_int(!temp_counter)^" = ((fRecords.top()).retFunc);");
						fprintf oc "\n\t%s" ("fRecords.pop();\n");
						fprintf oc "\n\t%s" ("goto *_t"^string_of_int(!temp_counter)^";\n");
						temp_counter:=!temp_counter+1;
	|	Lbl(i)	->	if(i mod 2 = 0) then
						fprintf oc "\n\t%s" ("_L"^string_of_int(i) ^ ":");
					if(i mod 2 = 1) then
						fprintf oc "\n\t%s" ("_L"^string_of_int(i) ^ ":");
	(*|  	Endfdef ->  fprintf oc "\n\t%s" ("}")*)
	|	Flbl(s)	->	fprintf oc "\n\t%s" ("_L" ^ s ^ ":");					
	|   SetReturn(i)	->	tvars.(!temp_counter) <- (id_ti, "actRecord");
							fprintf oc "\n\t%s" ("_t"^string_of_int(!temp_counter)^";\n\t_t"^string_of_int(!temp_counter)^".retFunc = &&_L"^string_of_int i ^";\n\t fRecords.push(_t"^string_of_int(!temp_counter)^");");
							temp_counter := !temp_counter + 1;
	|	SetLocal(fn)	->	fprintf oc "\n\t%s" ("lclVars[lv_index] = (float)_t" ^ string_of_int(!temp_counter - 1) ^ ";");		
							fprintf oc "\n\t%s" ("lv_index = lv_index + 1;");
	| 	GetLocal(i)		->	tvars.(!temp_counter) <- (id_ti, "float");
							fprintf oc "\n\t%s" (ti ^ " = lclVars[lv_frameptr - " ^ string_of_int i ^ "];");
							Stack.push ti stck;
							temp_counter := !temp_counter + 1;
	| 	_ 		->	()	

let generate_c lst ti li oc globals globals_count tvars = 
	Stack.clear stck;
	(*globals_to_file globals globals_count 0 oc;*)
	print_endline "*****************";
	print_endline "SEMANTIC STACK";
	print_endline "*****************";
	List.iter (fun (fs, sn, thr) -> 				
					print_endline (" (" ^ sn ^ "," ^ thr ^ ")")) lst; 
	List.iter (fun (x) -> c_statement x ti li oc tvars) lst; 
	
	()