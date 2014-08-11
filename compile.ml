open Ast
open Printf
open Semantics
open Flatc

let globals_index = ref (0)
let globals = Array.make 10 ("","")
let tvar_index = ref (0)
let lbl_index = ref (0)

let file = "output.cpp"
let oc = open_out file

let initCppFile = function 
	_ ->	fprintf oc "%s\n\n\t" "\n\n
#include <iostream>
#include <GL/glut.h>  // GLUT, includes glu.h and gl.h
#include <math.h>
#ifdef _MSC_VER                         // Check if MS Visual C compiler
#  pragma comment(lib, \"opengl32.lib\")  // Compiler-specific directive to avoid manually configuration
#  pragma comment(lib, \"glu32.lib\")     // Link libraries
#endif			
using namespace std;
struct vec2cpp{
	float x;
	float y;
};
vec2cpp _ff = {0,0};
int myprogram();
void DrawAxes()
{
	glBegin(GL_LINES);
		glColor3f(1.0, 0.0, 0.0);
		glVertex2f(-1.0f, 0.0f);
		glVertex2f(+1.0f, 0.0f);
		glColor3f(0.0, 0.0, 1.0);
		glVertex2f(0.0f, -1.0f);
		glVertex2f(0.0f, +1.0f);
	glEnd();
}
void display() {
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); // Clear color and depth buffers
   	glMatrixMode(GL_MODELVIEW);     // To operate on model-view matrix

	// Render a color-cube consisting of 6 quads with different colors
   	glLoadIdentity();                 // Reset the model-view matrix

   	DrawAxes();
	myprogram();

  	glutSwapBuffers();  // Swap the front and back frame buffers (double buffering)
}
void init() {
	glClearColor(1.0, 1.0, 1.0, 1.0);
	glColor3f(0.0, 0.0, 0.0);
   	glClearDepth(1.0f);                   // Set background depth to farthest
   	glEnable(GL_DEPTH_TEST);   // Enable depth testing for z-culling
   	glDepthFunc(GL_LEQUAL);    // Set the type of depth-test
   	glShadeModel(GL_SMOOTH);   // Enable smooth shading
   	glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);  // Nice perspective corrections
}
void reshape(GLsizei width, GLsizei height) {  // GLsizei for non-negative integer
// Compute aspect ratio of the new window
   if (height == 0) height = 1;                // To prevent divide by 0
   GLfloat aspect = (GLfloat)width / (GLfloat)height;
 
   // Set the viewport to cover the new window
   glViewport(0, 0, width, height);
 
   // Set the aspect ratio of the clipping area to match the viewport
   glMatrixMode(GL_PROJECTION);  // To operate on the Projection matrix
   glLoadIdentity();
   if (width >= height) {
     // aspect >= 1, set the height from -1 to 1, with larger width
      gluOrtho2D(-1.0 * aspect, 1.0 * aspect, -1.0, 1.0);
   } else {
      // aspect < 1, set the width to -1 to 1, with larger height
     gluOrtho2D(-1.0, 1.0, -1.0 / aspect, 1.0 / aspect);
   }
}
int main(int argc, char** argv)
{
	glutInit(&argc, argv);          // Initialize GLUT
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);	// Enable double buffering
   	glutInitWindowSize(640, 480);   // Set the window's initial width & height - non-square
   	glutInitWindowPosition(50, 50); // Position the window's initial top-left corner
   	glutCreateWindow(\"Model Transform\");  // Create window with the given title
   	glutDisplayFunc(display);       // Register callback handler for window re-paint event
   	glutReshapeFunc(reshape);       // Register callback handler for window re-size event
   	init();                       // Our own OpenGL initialization
   	glutMainLoop();                 // Enter the infinite event-processing loop
	return 0;
}
int myprogram(){"
										
		
let closeCppFile = function
	_ ->	
			fprintf oc "%s\n\n" "\n\n
	return 0;
}";
			close_out oc
(*let firefly = ref (0.0,0.0);;


let tuple_of_vec = function
	Vec2(x,y)	->	(x,y)
let norm_tuple_of_vec = function
	Vec2(x,y) ->	let mag = sqrt ((x *. x) +. (y *. y)) in
						(x /. mag, y/. mag)  
*)

(*	
let rec eval_expr = function 
	  Constant(x) ->
		(match x with
		  Integer(x) -> string_of_int x
		| Float(x)	-> 	string_of_float x)
	| NegConstant(x) ->
		(match x with
		  Integer(x) -> string_of_int (-x)
		| Float(x) -> string_of_float (-.x))
*)	
	
(*			
let rec output_expr exp = match exp with
	  Constant(x) -> let v = eval_expr exp in
						fprintf oc "%s" v
	| NegConstant(x) -> let v = eval_expr exp in
						fprintf oc "%s" v
	| Vec2(x,y)	->	fprintf oc "%s" ("{"^ string_of_float (x+.0.0) ^"," ^ string_of_float (y+.0.0) ^ "}")
	| Identifier(x) -> fprintf oc "%s" (x);
	| Assign(v,e)	-> fprintf oc "%s" ((type_to_string e) ^" "^v^" = "); output_expr e; fprintf oc "%s" (";\n\t")
	| Binop(e1, op, e2) ->
			(match op with
			  On -> let onDist = float_of_string(eval_expr e1) and onDir = norm_tuple_of_vec e2 in 
			  			let newFirefly = (onDist*.(fst onDir)+.(fst !firefly), onDist*.(snd onDir)+.(snd !firefly)) in 
						fprintf oc "\n\t%s" ("glBegin(GL_LINES);\n\tglColor3f(0.0, 0.0, 0.0);");
						fprintf oc "\n\t%s" ("\tglVertex2f("^string_of_float (fst !firefly) ^"f, "^string_of_float (snd !firefly)^"f);\n\t\tglVertex2f("^string_of_float (fst newFirefly) ^"f, "^string_of_float (snd newFirefly)^"f);");
						fprintf oc "\n\t%s" ("glEnd();");
						firefly := newFirefly
						(*fprintf oc "\t%s\n\n" ("cout<<\"ON is working!"^ string_of_float (fst newFirefly) ^","^string_of_float (snd newFirefly) ^ "\";")
						*)
			  			(*fprintf oc "\t%s\n\n" ("cout<<\"ON is working!"^ string_of_float (fst onDir) ^","^string_of_float (snd onDir) ^" DIST:"^string_of_float onDist ^ "\";")
						*)
			| Off -> let offDist = float_of_string(eval_expr e1) and offDir = norm_tuple_of_vec e2 in 
			  			let newFirefly = (offDist*.(fst offDir)+.(fst !firefly), offDist*.(snd offDir)+.(snd !firefly)) in 
						firefly := newFirefly
			
			)
*)		

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
		  | Float(x)	-> 	[(Flt(x),string_of_float x,"float")] )
  | Vec2(x, y) -> gen_expr (x) @ gen_expr (y) @ [(Vec2_Op,"VEC","vec2cpp")]
  | Binop (e1, op, e2) -> let v1 = gen_expr e1 and v2 = gen_expr e2 in
		( match op with
			On -> v1 @ v2 @ [(On_Op,"ON","vec2cpp")]
		  | Off -> v1 @ v2 @ [(Off_Op,"OFF","vec2cpp")]
		  | Add -> v1 @ v2 @ [(Add_Op,"ADD","TypeToInfer")]
		  | Minus -> v1 @ v2 @ [(Minus_Op,"MINUS","TypeToInfer")]
		  | LessThan -> v1 @ v2 @ [(LessThan_Op,"LESSTHAN","bool")]
		  | LessThanEq -> v1 @ v2 @ [(LessThanEq_Op,"LESSTHANEQ","bool")]
		  | GreaterThan -> v1 @ v2 @ [(GreaterThan_Op,"GREATERTHAN","bool")]
		  | GreaterThanEq -> v1 @ v2 @ [(GreaterThanEq_Op,"GREATERTHANEQ","bool")]
		  | EqualsTo -> v1 @ v2 @ [(EqualsTo_Op,"EQUALSTO","bool")]
		)
  | Identifier(x) -> [(Id(x),"ID(" ^ x ^ ")","TypeToInfer")]
  | Assign(v,e) -> 	gen_expr e @ gen_expr (Identifier(v)) @ [(Asn_Op,"Asn","TypeToInfer")]
					(* globals.(!globals_index) <- (v, "Type"); 
					globals_index := !globals_index + 1; *)
  | _ -> []  	

let rec gen_stmt = function
	Expr e -> gen_expr e
  | If(e, ts, fs) -> gen_expr e @ [(If_Op, "IF", "bool")] @ gen_stmt fs @ gen_stmt ts
  
let print_gen x = match x with
	_ -> 	List.iter (fun (fs, sn, thr) -> 				
				(*print_endline ("XXX (" ^ sn ^ "," ^ thr ^ ")")) ( (gen_stmt x) );*)
				print_endline ("XXX (" ^ sn ^ "," ^ thr ^ ")")) (sa (gen_stmt x) (globals) globals_index); 
			(*generate_c (sa (gen_stmt x) (globals) globals_index) (tvar_index) (lbl_index) (oc);*)
			(*let _ = generate_c (sa (gen_stmt x) (globals) globals_index) tvar_index lbl_index in ();*)
			print_endline ""
			(* Array.iter (fun (v, t) -> print_endline (v ^ " ggg " ^ t)) globals *)

let translate = function
	 (*exprs -> initCppFile(); List.iter output_expr exprs;  closeCppFile() *)
	 stmts -> initCppFile(); List.iter print_gen stmts;  closeCppFile()