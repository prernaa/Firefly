open Ast
open Printf

let file = "output.cpp"
let oc = open_out file

let initCppFile = function 
	_ ->	
			fprintf oc "%s\n\n" "\n
							\n
							#include <iostream>
							
							#include <GL/glut.h>  // GLUT, includes glu.h and gl.h

							#ifdef _MSC_VER                         // Check if MS Visual C compiler
							#  pragma comment(lib, \"opengl32.lib\")  // Compiler-specific directive to avoid manually configuration
							#  pragma comment(lib, \"glu32.lib\")     // Link libraries
							#endif			

							using namespace std;
							
							void display() {
							   glClearColor(0.0f, 0.0f, 0.0f, 1.0f); // Set background color to black and opaque
							   glClear(GL_COLOR_BUFFER_BIT);         // Clear the color buffer
							 
							   // Draw a Red 1x1 Square centered at origin		
								glBegin(GL_LINES);
									//axis'
									glColor3f(1, 0, 0); glVertex3f(0, 0, 0); glVertex3f(10, 0, 0);
									glColor3f(0, 1, 0); glVertex3f(0, 0, 0); glVertex3f(0, 10, 0);
									glColor3f(0, 0, 1); glVertex3f(0, 0, 0); glVertex3f(0, 0, 10);
									//shape
									glColor3f(1.0, 1.0, 1.0);
									int n = 2;
									for (int i = 0; i < n; i++)
									{
										glVertex3f(i*0.1, i*0.1, 0); glVertex3f((i+1)*0.1, (i+1)*0.1, 0);
									}
								glEnd();
							 
							   glFlush();  // Render now
							}			
			
							void init() {

							  // Set the current clear color to black and the current drawing color to
							  // white.
							  glClearColor(0.0, 0.0, 0.0, 1.0);
							  glColor3f(1.0, 1.0, 1.0);

							  // Set the camera lens to have a 60 degree (vertical) field of view, an
							  // aspect ratio of 4/3, and have everything closer than 1 unit to the
							  // camera and greater than 40 units distant clipped away.
							  glMatrixMode(GL_PROJECTION);
							  glLoadIdentity();
							  gluPerspective(60.0, 4.0/3.0, 1, 40);

							  // Position camera at (4, 6, 5) looking at (0, 0, 0) with the vector
							  // <0, 1, 0> pointing upward.
							  glMatrixMode(GL_MODELVIEW);
							  glLoadIdentity();
							  gluLookAt(4, 6, 5, 0, 0, 0, 0, 1, 0);
							}									
							
							int main(int argc, char** argv)
							{
								cout << \"Hello!\" << endl;
								glutInit(&argc, argv);                 // Initialize GLUT
								glutCreateWindow(\"OpenGL Setup Test\"); // Create a window with the given title
								glutInitWindowSize(800, 600);   // Set the window's initial width & height
								glutInitWindowPosition(80, 80); // Position the window's initial top-left corner
								glutDisplayFunc(display); // Register display callback handler for window re-paint
								init();
								glutMainLoop();           // Enter the infinitely event-processing loop"
										
		
let closeCppFile = function
	_ ->	
			fprintf oc "%s\n\n" "\n
							\n
							return 0;
							}";
			close_out oc
				
let eval = function
	Literal(x) ->	fprintf oc "%s\n\n" ("Value is: " ^ (string_of_int x));
					close_out oc
	| Vec2(x,y)	->	fprintf oc "%s\n\n" ("Value is: [" ^ (string_of_int x) ^ "," ^ (string_of_int y) ^ "]");
					close_out oc
	| _	->			fprintf oc "%s\n\n" "base case";
					close_out oc
					
let translate = function
	Print ->		initCppFile ();
					closeCppFile ()
	| Exp(x) ->	eval(x)

