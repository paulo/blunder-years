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
void MouseMotion(int x, int y);

static int cameraActual = 1;
static CameraSphere cameraSph = CameraSphere(10);
static CameraFP cameraFP = CameraFP();

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

	glPolygonMode(GL_FRONT, GL_LINE); // GL_FILL, GL_LINE, GL_POINT

	//function listening
	glutDisplayFunc(renderScene);
	//glutIdleFunc(renderScene);
	glutReshapeFunc(changeSize);
	glutSpecialFunc(specialKeys);
	glutKeyboardFunc(normalKeys);
	glutMotionFunc(MouseMotion);
	glutPassiveMotionFunc(MouseMotion);

	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);
	glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
}

void _start(){
	XMLDocument doc;
	XMLNode* fstchild;

	int loadOk = doc.LoadFile("exemplos/circulo.xml");
	if (loadOk != 0){
		printf("Erro!! Falha ao ler o ficheiro!\n");
	}
	else {
		root = doc.RootElement();
		actualScene.parseXML(root->FirstChild());
	}

	glutMainLoop();
}

/*
*Interpreta as teclas premidas no teclado
*
*@param key		tecla premida
*@param x		localização do ponteiro do rato relativamente à janela
*@param y		localização do ponteiro do rato relativamente à janela
*/
void normalKeys(unsigned char key, int x, int y){
	switch (key)
	{
	case '1':
		cameraActual = 1;
		cameraSph.refresh();
		break;
	case '2':
		cameraActual = 2;
		cameraFP.start();
		break;
	default:
		break;
	}

	if (cameraActual == 1){
		cameraSph.bindKey(key);
	}
	else if (cameraActual == 2){
		cameraFP.bindKey(key);
	}


	glutPostRedisplay();
}
void specialKeys(int key, int x, int y){

}


void Mouse(int button, int state, int x, int y)
{
	// mouse click event
}

/*
*Função invocada quando o rato é movido
*
*@param x	coordenadas do rato em relação à janela
*@param y	coordenadas do rato em relação à janela
*/
void MouseMotion(int x, int y)
{
	// This variable is hack to stop glutWarpPointer from triggering an event callback to Mouse(...)
	// This avoids it being called recursively and hanging up the event loop
	static bool just_warped = false;

	if (just_warped) {
		just_warped = false;
		return;
	}
	if (cameraActual == 2){
		cameraFP.moveMouse(x, y);
		cameraFP.refresh();
		just_warped = true;
		glutPostRedisplay();
	}
}

void viewOptions(int x){
	switch (x){
	case 1: glPolygonMode(GL_FRONT, GL_FILL);
		break;
	case 2: glPolygonMode(GL_FRONT, GL_LINE);
		break;
	case 3: glPolygonMode(GL_FRONT, GL_POINT);
		glPointSize(12);
		break;
	default: glPolygonMode(GL_FRONT, GL_LINE);
	}
	glutPostRedisplay();
}

/*
*Função de renderização
*/
void renderScene(void){

	// clear buffers
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// pôr instruções de desenho aqui
	// actualScene.draw();
	glLoadIdentity();
	if (cameraActual == 1){
		cameraSph.refresh();
	}
	else if (cameraActual == 2){
		cameraFP.refresh();
	}
	actualScene.draw();
	//create MENU
	glutCreateMenu(viewOptions); 
	glutAddMenuEntry("GL FILL", 1);
	glutAddMenuEntry("GL LINE", 2);
	glutAddMenuEntry("GL POINT", 3);
	glutAttachMenu(GLUT_RIGHT_BUTTON);


	//
	// End of frame
	glutSwapBuffers();
}

/*
*Altera as dimensões da janela
*
*@param w	largura da janela
*@param h	altura da janela
*/
void changeSize(int w, int h){
	cameraFP.setWindowSize(w, h);
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

/*
*/
int main(int argc, char **argv) {
	init(argc, argv);
	_start();

	return 1;
}

