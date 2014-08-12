


#include <iostream>
#include <GL/glut.h>  // GLUT, includes glu.h and gl.h
#include <math.h>
#ifdef _MSC_VER                         // Check if MS Visual C compiler
#  pragma comment(lib, "opengl32.lib")  // Compiler-specific directive to avoid manually configuration
#  pragma comment(lib, "glu32.lib")     // Link libraries
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
   	glutCreateWindow("Model Transform");  // Create window with the given title
   	glutDisplayFunc(display);       // Register callback handler for window re-paint event
   	glutReshapeFunc(reshape);       // Register callback handler for window re-size event
   	init();                       // Our own OpenGL initialization
   	glutMainLoop();                 // Enter the infinite event-processing loop
	return 0;
}
int myprogram(){

	
	int _t0 = 0;
	int var1 = _t0;
	int _t1 = 1;
	int _t2 = 2;
	bool _t3 = _t1 > _t2;
	if (_t3) { goto _L10; }
	int _t4 = 1001;
	int _t5 = 1002;
	bool _t6 = _t4 > _t5;
	if (_t6) { goto _L20; }
	int _t7 = 10;
	var1 = _t7;
	goto _L21;
	_L20:
	int _t8 = 0;
	var1 = _t8;
	_L21:
	goto _L11;
	_L10:
	int _t9 = 1;
	var1 = _t9;
	_L11:
	float _t10 = 0.4;
	int _t11 = 1;
	vec2cpp _t12 = {_t11 , var1};
	float _t13 = sqrt((_t12.x * _t12.x + _t12.y * _t12.y));	
_t12.x = _t12.x/_t13;
_t12.y = _t12.y/_t13;
vec2cpp _t14 = {_t10 * _t12.x + _ff.x , _t10 * _t12.y + _ff.y };
glBegin(GL_LINES);
	glColor3f(0.0, 0.0, 0.0);
	glVertex2f(_ff.x, _ff.y);
	glVertex2f(_t14.x , _t14.y);
_ff.x = _t14.x;
_ff.y = _t14.y;
glEnd();

	vec2cpp varline = _t14;


	return 0;
}

