


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
	int i = _t0;
	float _t1 = 0.3;
	float j = _t1;
	int _t2 = 3;
	int _t3 = 1;
	bool _t4 = _t2 > _t3;
	bool _t7 = false;
	if (!_t4) { goto _L40; }
	int _t5 = 4;
	int _t6 = 5;
	 _t7 = _t5 < _t6;
	_L40:;
	bool _t8 = _t4 && _t7;
	if (_t8) { goto _L10; } {
	float _t9 = 1.;
	int _t10 = -1;
	int _t11 = -1;
	vec2cpp _t12 = {_t10 , _t11};
	float _t13 = sqrt((_t12.x * _t12.x + _t12.y * _t12.y));	
_t12.x = _t12.x/_t13;
_t12.y = _t12.y/_t13;
vec2cpp _t14 = {_t9 * _t12.x + _ff.x , _t9 * _t12.y + _ff.y };
glBegin(GL_LINES);
	glColor3f(0.0, 0.0, 0.0);
	glVertex2f(_ff.x, _ff.y);
	glVertex2f(_t14.x , _t14.y);
_ff.x = _t14.x;
_ff.y = _t14.y;
glEnd();

	vec2cpp _t15 = _ff;
	goto _L11;
	_L10:;
	_L21:;
	int _t16 = 20;
	bool _t17 = _t16 == i;
	bool _t18 = !_t17;
	if (!_t18) { goto _L20; }{
	float _t19 = 0.05;
	float _t20 = 1.;
	vec2cpp _t21 = {_t20 , j};
	float _t22 = sqrt((_t21.x * _t21.x + _t21.y * _t21.y));	
_t21.x = _t21.x/_t22;
_t21.y = _t21.y/_t22;
vec2cpp _t23 = {_t19 * _t21.x + _ff.x , _t19 * _t21.y + _ff.y };
glBegin(GL_LINES);
	glColor3f(0.0, 0.0, 0.0);
	glVertex2f(_ff.x, _ff.y);
	glVertex2f(_t23.x , _t23.y);
_ff.x = _t23.x;
_ff.y = _t23.y;
glEnd();

	vec2cpp _t24 = _ff;
	int _t25 = 1;
	int _t26 = 2;
	int _t27 = _t25 * _t26;
	int _t28 = 2;
	int _t29 = _t27 / _t28;
	int _t30 = i + _t29;
	i = _t30;
	int _t31 = 10;
	bool _t32 = i > _t31;
	if (_t32) { goto _L30; } {
	float _t33 = 0.2;
	float _t34 = j + _t33;
	j = _t34;
	goto _L31;
	_L30:;
	float _t35 = 0.2;
	float _t36 = j - _t35;
	j = _t36;
	}
	_L31:;
	goto _L21;
	}
	_L20:;
	}
	_L11:;


	return 0;
}

