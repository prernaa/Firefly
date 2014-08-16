open Ast
open Printf
open Semantics
open Flatc
open Stack

(*global vars management *)
let globals_index = ref (0)
let globals = Array.make 1024 ("", "")

(*functions management *)
let fun_index = ref (0)
let funs = Array.make 1024 ( "", [""] )

(* temp vars management *)
let tvar_index = ref (0)
let tempvars = Array.make 1024 ("", "")

(* Label managment *)
let lbl_index = ref (0)
let lblStack = Stack.create ()
let a = Stack.push !lbl_index lblStack

(* CPP output files *)
let file_head = "input_head.cpp"
let file = "output.cpp"
let oc_init = open_out file
let file_decs = "output_decs.cpp"
let oc_decs = open_out file_decs
let file_TAC = "output_TAC.cpp"
let oc_TAC = open_out file_TAC

let read_file file =
  let ic = open_in file in
  let lines = ref [] in
  try
    while true do
      let line = input_line ic in
      lines := line :: !lines
    done; assert false
  with End_of_file ->
    String.concat "\n" (List.rev !lines)
											
let closeCppFile = function
	_ ->	
			fprintf oc_TAC "%s\n\n" "\n\n
	return 0;
}";
			close_out oc_TAC

let type_to_string = function
	  Constant(x) ->
		(match x with
			  Integer(x) ->	"int"	
			| Float(x) ->	"float")
	| NegConstant(x) ->
		(match x with
			  Integer(x) ->	"int"	
			| Float(x) ->	"float")	
	| Vec2(x,y) ->	"vec2cpp"	
	| _	-> "InvalidType"
	
let rec gen_expr = function
	Constant(x) -> 
		( match x with 
			Integer(x) -> [(Int(x),string_of_int x,"int")]
		  | Float(x)   -> [(Flt(x),string_of_float x,"float")] )
  | NegConstant(x) -> 
		( match x with 
			Integer(x) -> [(Int(-x),string_of_int (-1 * x),"int")]
		  | Float(x)   -> [(Flt(-1.0 *. x),string_of_float (-1.0 *. x),"float")] )
  | Vec2(x, y) -> gen_expr (x) @ gen_expr (y) @ [(Vec2_Op,"VEC","vec2cpp")]
  | Binop (e1, op, e2) -> let v1 = gen_expr e1 and v2 = gen_expr e2 in
		( match op with
			On -> v1 @ v2 @ [(On_Op,"ON","vec2cpp")]
		  | Off -> v1 @ v2 @ [(Off_Op,"OFF","vec2cpp")]
		  | Add -> v1 @ v2 @ [(Add_Op,"ADD","TypeToInfer")]
		  | Minus -> v1 @ v2 @ [(Minus_Op,"MINUS","TypeToInfer")]
		  | Multiply -> v1 @ v2 @ [(Multiply_Op,"MULTIPLY","TypeToInfer")]
		  | Divide -> v1 @ v2 @ [(Divide_Op,"DIVIDE","TypeToInfer")]
		  | LessThan -> v1 @ v2 @ [(LessThan_Op,"LESSTHAN","bool")]
		  | LessThanEq -> v1 @ v2 @ [(LessThanEq_Op,"LESSTHANEQ","bool")]
		  | GreaterThan -> v1 @ v2 @ [(GreaterThan_Op,"GREATERTHAN","bool")]
		  | GreaterThanEq -> v1 @ v2 @ [(GreaterThanEq_Op,"GREATERTHANEQ","bool")]
		  | EqualsTo -> v1 @ v2 @ [(EqualsTo_Op,"EQUALSTO","bool")]
		  | NotEqualsTo -> v1 @ v2 @ [(NotEqualsTo_Op,"NOTEQUALSTO","bool")]
		  | Or -> 		lbl_index := !lbl_index + 10;
						Stack.push !lbl_index lblStack;
						let li = Stack.top lblStack in
						v1 @ [(Or_Op(li),"OR " ^ string_of_int(li),"bool")] @ v2 
						@ [(Lbl(li), "LBL " ^ string_of_int(li), "void")] 
						@ [(OrDone_Op,"ORDONE","bool")]
		  | And -> 		lbl_index := !lbl_index + 10;
						Stack.push !lbl_index lblStack;
						let li = Stack.top lblStack in
						v1 @ [(And_Op(li),"AND " ^ string_of_int(li),"bool")] @ v2 
						@ [(Lbl(li), "LBL " ^ string_of_int(li), "void")] 
						@ [(AndDone_Op,"ANDDONE","bool")]
		)
  | Identifier(x) -> [(Id(x),"ID(" ^ x ^ ")","TypeToInfer")]
  | Assign(v,e) -> 	gen_expr e @ gen_expr (Identifier(v)) @ [(Asn_Op,"Asn","TypeToInfer")]					
  | Not(e)	->	gen_expr e @ [(Not_Op,"NOT","bool")]
  | Sqrt(e) -> gen_expr e @ [(Sqrt,"SQRT","TypeToInfer")]
  | Sin(e)	-> gen_expr e @ [(Sin_Op,"SIN","float")]
  | Cos(e)	-> gen_expr e @ [(Cos_Op,"COS","float")]
  | Getx(e)	-> gen_expr e @ [(Getx_Op,"GETX","float")]
  | Gety(e)	-> gen_expr e @ [(Gety_Op,"GETY","float")]
  | _ -> []  	

let rec gen_stmt = function
	Expr e 			-> 	gen_expr e
  | If(e, ts, fs) 	-> 	lbl_index := !lbl_index + 10;
						Stack.push !lbl_index lblStack;
						let li = Stack.top lblStack in
						gen_expr e 
						@ [(If_Op(li), "IF " ^ string_of_int(li), "bool")] 
						@ gen_stmt fs 
						@ [(Goto(li + 1), "GOTO " ^ string_of_int(li + 1), "void")] 
						@ [(Lbl(li), "LBL " ^ string_of_int(li), "void")] 
						@ gen_stmt ts @ [(EndIf_Op, "ENDIF", "bool")]
						@ [(Lbl(li + 1), "LBL " ^ string_of_int(li + 1), "void")]
  | While(e, ts)	->	lbl_index := !lbl_index + 10;
						Stack.push !lbl_index lblStack;
						let li = Stack.top lblStack in
						[(Lbl(li + 1), "LBL " ^ string_of_int(li + 1), "void")]
						@ gen_expr e 
						@ [(While_Op(li), "WHILE " ^ string_of_int(li), "bool")] 
						@ gen_stmt ts 
						@ [(Goto(li + 1), "GOTO " ^ string_of_int(li + 1), "void")] 
						@ [(Lbl(li), "LBL " ^ string_of_int(li), "void")] 
						@ [(EndWhile_Op, "ENDWHILE", "bool")]
  | Block(stmts)	->	List.concat (List.map gen_stmt stmts)  
  | Call(fn, args)	-> 	lbl_index := !lbl_index + 10;
						let li = !lbl_index in
						[SetReturn(li+1), "SET RET " ^ fn, "void"]
						@ [(GotoFun(fn), "GOTO FUN " ^ fn, "void")] 
						@ [(Lbl(li+1), "LBL " ^ string_of_int(li+1), "void")] 						
  |	Fdef(fname, args, body)	
					-> 	funs.(!fun_index) <- (fname, args);
						fun_index := !fun_index + 1;
						lbl_index := !lbl_index + 10;
						let li = !lbl_index in
						[(Goto(li), "GOTO " ^ string_of_int(li), "void")] 
						@ [(Flbl(fname), "FLBL " ^ fname, "void")] 
						@ (gen_stmt body)
						@ [(GotoReturn, "GOTO RET ", "void")] 
						@ [(Lbl(li), "LBL " ^ string_of_int(li), "void")] 					
						@ [(Endfdef, "ENDFDEF " ^ string_of_int(li), "bool")] 
  
(* helper function to print array values as c++ variable declarations *)			
let rec array_to_file g c i fl = 	
	(
		if i < c then (		
			let varname s = String.sub (s) (3) ((String.length s) -4) in 
				fprintf fl "\n\t%s" ((snd g.(i)) ^ " " ^ (varname (fst g.(i))) ^ ";");		
			array_to_file g c (i+1) fl
		)	
		else ()
	)
	
(* for each statement, this function runs semantic analysis and generates flat C (TAC-like) code *)
let print_gen x = match x with			
	_ 	->	let syntaxtree = gen_stmt x (* build AST *)
			in let sast = sa syntaxtree globals globals_index funs fun_index (* build SAST via semantics.ml *)
			in	generate_c sast tvar_index lbl_index oc_TAC globals !globals_index tempvars; (* build flat C++ via flatc.ml *)	
			print_endline ""		
			
let translate = function	
	stmts	-> 	fprintf oc_init "\n\t%s" (read_file file_head);																	
				List.iter print_gen (List.rev stmts);  				
				closeCppFile();
				array_to_file globals !globals_index 0 oc_decs;
				array_to_file tempvars (!temp_counter) 0 oc_decs;
				close_out oc_decs;											
				fprintf oc_init "\n\t%s" ( (read_file file_decs) ^ "\n" ^ (read_file file_TAC) );																	
				close_out oc_init