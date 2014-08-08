open Ast
open Printf

let file = "output.cpp"
let oc = open_out file

let initCppFile = function 
	_ ->	
			fprintf oc "%s\n\n\t" "\n\n
#include <iostream>
#include <GL/glut.h>  // GLUT, includes glu.h and gl.h
#include <math.h>
#ifdef _MSC_VER                         // Check if MS Visual C compiler
#  pragma comment(lib, \"opengl32.lib\")  // Compiler-specific directive to avoid manually configuration
#  pragma comment(lib, \"glu32.lib\")     // Link libraries
#endif			
using namespace std;
void DrawCircle(float cx, float cy, float r, int num_segments)
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

   	DrawCircle(0.0, 0.0, 0.5, 100);

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
   	glutInitWindowSize(640, 480);   // Set the window's initial width & height - non-square
   	glutInitWindowPosition(50, 50); // Position the window's initial top-left corner
   	glutCreateWindow(\"Model Transform\");  // Create window with the given title
   	glutDisplayFunc(display);       // Register callback handler for window re-paint event
   	glutReshapeFunc(reshape);       // Register callback handler for window re-size event
   	init();                       // Our own OpenGL initialization
   	glutMainLoop();                 // Enter the infinite event-processing loop"
										
		
let closeCppFile = function
	_ ->	
			fprintf oc "%s\n\n" "\n\n
	return 0;
}";
			close_out oc
let firefly = (0,0)

let tuple_of_vec = function
	Vec2(x,y)	->	(x,y)	

let type_to_string = function
	  Integer(x) ->	"int"	
	| Float(x) ->	"float"
	
let rec eval_expr = function 
	Integer(x) -> string_of_int x
let rec output_expr = function
	  Integer(x) ->	fprintf oc "%s" (string_of_int x)	
	| Identifier(x) -> fprintf oc "%s" (x);
	| Assign(v,e)	-> fprintf oc "%s" ((type_to_string e) ^" "^v^" = "); output_expr e; fprintf oc "%s" (";\n\t")
	| Binop(e1, op, e2) ->
			(match op with
			  On -> let v1 = int_of_string(eval_expr e1) and v2 = tuple_of_vec e2 in fprintf oc "\t%s\n\n" ("cout<<\"ON is working!"^"\";")
			)

let rec eval = function
	  Integer(x) ->	fprintf oc "\t%s\n\n" ("cout<<\"Integer is: " ^ (string_of_int x) ^"\";")		
	| Float(x) ->	fprintf oc "\t%s\n\n" ("cout<<\"Float is: " ^ (string_of_float x)^"\";")
	| Vec2(x,y)	->	fprintf oc "\t%s\n\n" ("cout<<\"Vec2 is: [" ^ (string_of_float x) ^ "," ^ (string_of_float y) ^ "]" ^ "\";")
	| Identifier(x) -> fprintf oc "\t%s\n\n" ("cout<<\"Identifier is: " ^ (x) ^"\";")
	| Assign(v,e)	-> fprintf oc "\t%s\n\n" ("cout<<\"Assigning: " ^ (v) ^"\";")		
	| Binop(e1, op, e2) ->
			(match op with
			  On -> fprintf oc "\t%s\n\n" ("cout<<\"ON is working!"^"\";")
			| Add -> fprintf oc "\t%s\n\n" ("cout<<\"Add is working!"^"\";")
			)
	| _	->			fprintf oc "\t%s\n\n" "cout<<\"base case\""
					
let translate = function
	 exprs -> initCppFile(); List.iter output_expr exprs;  closeCppFile()