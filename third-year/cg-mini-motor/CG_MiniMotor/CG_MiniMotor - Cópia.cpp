// CC_MiniMotor.cpp : Defines the entry point for the console application.
//
#include "stdafx.h"

#include <stdlib.h>
#include <glut.h>
#define _USE_MATH_DEFINES
#include "model.h"
#include <fstream>




void normalKeys(unsigned char key, int x, int y);
void specialKeys(int key, int x, int y);
void renderScene(void);
void changeSize(int w, int h);

static CameraSphere camera = CameraSphere(10);
static Scene actualScene;
static XMLElement* root = NULL;
Figure f;

void init(int argc, char **argv){
	// parse argumets
	string xmlFile = "somewhere.xml";
	glutInit(&argc, argv);

	// otptions
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowPosition(100, 100);
	glutInitWindowSize(800, 800);
	glutCreateWindow("Rui OLiveirasss");
	
	//glPolygonMode(GL_BACK, GL_POINT); // GL_FILL, GL_LINE, GL_POINT
	glPolygonMode(GL_FRONT, GL_LINE); // GL_FILL, GL_LINE, GL_POINT

	//function listening
	glutDisplayFunc(renderScene);
	glutReshapeFunc(changeSize);
	glutSpecialFunc(specialKeys);
	glutKeyboardFunc(normalKeys);

	// data init
	//ifstream ifs(xmlFile);
	//actualScene.readFromXml(ifs);
//	f.fromFile("ficheiro");
//	actualScene.append(&f);
}

void _start(){
	XMLDocument doc;
	XMLNode* fstchild;

	int loadOk = doc.LoadFile("esfera.xml");
	if (loadOk != 0){
		printf("Erro!! Falha ao ler o ficheiro!\n");
	}
	else {
		root = doc.RootElement();
	}
	glutMainLoop();
}

void normalKeys(unsigned char key, int x, int y){
	switch (key)
	{
	case ' ':
		break;
	case 'w': camera.moveUp(0.01); camera.refresh();
		break;
	case 's': camera.moveDown(0.01); camera.refresh();
		break;
	case 'a': camera.moveRight(0.01); camera.refresh();
		break;
	case 'd': camera.moveLeft(0.01); camera.refresh();
		break;
	case 'r': camera.moveRadios(1); camera.refresh();
		break;
	case 'f': camera.moveRadios(-1); camera.refresh();
		break;
	default:
		break;
	}
	glutPostRedisplay();
}
void specialKeys(int key, int x, int y){

}


void Mouse(int button, int state, int x, int y)
{
	// mouse click event
}

void MouseMotion(int x, int y)
{
	// This variable is hack to stop glutWarpPointer from triggering an event callback to Mouse(...)
	// This avoids it being called recursively and hanging up the event loop
	static bool just_warped = false;

	if (just_warped) {
		just_warped = false;
		return;
	}
}

void renderScene(void){

	// clear buffers
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// pôr instruções de desenho aqui
	// actualScene.draw();
	actualScene.parseXML(root->FirstChild());

	// End of frame
	glutSwapBuffers();
}


void changeSize(int w, int h){
	actualScene.windowHeight = h;
	actualScene.windowWidth = w;
	// Prevent a divide by zero, when window is too short
	// (you cant make a window with zero width).
	if (h == 0)
		h = 1;

	// compute window's aspect ratio 
	float ratio = w * 1.0 / h;

	// Set the projection matrix as current
	glMatrixMode(GL_PROJECTION);
	// Load Identity Matrix
	glLoadIdentity();

	// Set the viewport to be the entire window
	glViewport(0, 0, w, h);

	// Set perspective
	gluPerspective(45.0f, ratio, 1.0f, 1000.0f);

	// return to the model view matrix mode
	glMatrixMode(GL_MODELVIEW);
}

int main(int argc, char **argv) {
	init(argc, argv);
	_start();

	return 1;
}

