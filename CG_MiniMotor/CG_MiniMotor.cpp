// CC_MiniMotor.cpp : Defines the entry point for the console application.
//

#include <stdio.h>
#include "stdafx.h"
#include <stdlib.h>
#define _USE_MATH_DEFINES
#include "model.h"
#include <fstream>

Point3D getCurrentCameraPosition();
void normalKeys(unsigned char key, int x, int y);
void specialKeys(int key, int x, int y);
void renderScene(void);
void changeSize(int w, int h);
void MouseMotion(int x, int y);
void viewOptions(int x);

static string filename;
static int cameraActual = 1;
static int xmlOk = 0;
static CameraSphere cameraSph = CameraSphere(10);
static CameraFP cameraFP = CameraFP();
static int frame = 0, time, timebase = 0;
static Scene actualScene;
static XMLElement* root = NULL;
Figure f = Figure(&actualScene);

void calcFPS(){
	int fps;
	char s[256];

	frame++;
	time = glutGet(GLUT_ELAPSED_TIME);
	if (time - timebase > 1000) {
		fps = frame*1000.0 / (time - timebase);
		sprintf_s(s, "%d", fps);
		
		glutSetWindowTitle(s);
		timebase = time;
		frame = 0;
	}
}

void init(int argc, char **argv){
	// parse argumets
	string xmlFile = "somewhere.xml";
	glutInit(&argc, argv);
	

	// options
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowPosition(100, 100);
	glutInitWindowSize(800, 800);
	glutCreateWindow("Motor");

	glewInit();

	glPolygonMode(GL_FRONT, GL_LINE); // GL_FILL, GL_LINE, GL_POINT
	glEnableClientState(GL_VERTEX_ARRAY);

	ilInit();
	ilEnable(IL_ORIGIN_SET);
	ilOriginFunc(IL_ORIGIN_LOWER_LEFT);

	//function listening
	glutDisplayFunc(renderScene);
	glutIdleFunc(renderScene);
	glutReshapeFunc(changeSize);
	glutSpecialFunc(specialKeys);
	glutKeyboardFunc(normalKeys);
	glutMotionFunc(MouseMotion);
	glutPassiveMotionFunc(MouseMotion);

	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);
	glEnable(GL_TEXTURE_2D);

	glClearColor(0.0f, 0.0f, 0.0f, 0.0f);

	//create MENU
	glutCreateMenu(viewOptions);
	glutAddMenuEntry("GL FILL", 1);
	glutAddMenuEntry("GL LINE", 2);
	glutAddMenuEntry("GL POINT", 3);
	glutAddMenuEntry("VBO MODE", 4);
	glutAddMenuEntry("DIRECT MODE", 5);
	glutAddMenuEntry("DRAW NORMAL", 6);
	glutAttachMenu(GLUT_RIGHT_BUTTON);

	actualScene.setDrawMode(Scene::DRAWMODE_VBO);
	actualScene.setDrawNormal(false);

}

void _start(const char* filename){
	XMLDocument doc;

	glDisable(GL_LIGHT0);
	glDisable(GL_LIGHT1);
	glDisable(GL_LIGHT2);
	glDisable(GL_LIGHT3);
	glDisable(GL_LIGHT4);
	glDisable(GL_LIGHT5);
	glDisable(GL_LIGHT6);
	glDisable(GL_LIGHT7);
	glDisable(GL_LIGHTING);

	int loadOk = doc.LoadFile(filename);
	if (loadOk != 0){
		printf("Erro!! Falha ao ler o ficheiro!\n");
	}
	else {
		//actualScene.getLights();
		actualScene.reset();
		actualScene.setCameraPosition(25, 25, 25);
		root = doc.RootElement();
		if (actualScene.parseXML(root->FirstChild(), &actualScene) != -1) {
			Point3D p = actualScene.getCameraPosition();
			cameraSph.setPos(p.x, p.y, p.y);
			glutMainLoop();
		}
		else {
			cout << "Erro na escrita do ficheiro!!" << endl;
		}
	}
}
	
/*
*Interpreta as teclas premidas no teclado
*
*@param key		tecla premida
*@param x		localização do ponteiro do rato relativamente à janela
*@param y		localização do ponteiro do rato relativamente à janela
*/
void normalKeys(unsigned char key, int x, int y){
	Point3D p;
	switch (key)
	{
	case '1':
		p = getCurrentCameraPosition();
		cameraActual = 1;
		cameraSph.setPos(p.x, p.y, p.z);
		cameraSph.refresh();	
		break;
	case '2':	
		p = getCurrentCameraPosition();
		if (cameraActual == 1) {
			cameraFP.setAngles(-cameraSph.getYaw(), M_PI + cameraSph.getPitch());
		}
		cameraActual = 2;
		cameraFP.setPos(p.x, p.y, p.z);
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

Point3D getCurrentCameraPosition(){
	if (cameraActual == 1){
		return cameraSph.getPos();
	}
	else if (cameraActual == 2){
		return cameraFP.getPos();
	}
	return{ 0, 0, 0 };
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
	case 4:
		actualScene.setDrawMode(Scene::DRAWMODE_VBO);
		_start(filename.c_str());
		glutPostRedisplay();
		break;
	case 5:
		actualScene.setDrawMode(Scene::DRAWMODE_DIRECT);
		_start(filename.c_str());
		glutPostRedisplay();
		break;
	case 6:
		actualScene.setDrawNormal(!actualScene.geDrawNormal());
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

	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);
	glEnableClientState(GL_TEXTURE_COORD_ARRAY);
	// pôr instruções de desenho aqui
	// actualScene.draw();
	glLoadIdentity();

	if (cameraActual == 1){
		cameraSph.refresh();
	}
	else if (cameraActual == 2){
		cameraFP.refresh();
	}

	/*
	GLfloat amb[3] = { 0.2, 0.2, 0.2 };
	GLfloat diff[3] = { 5.0,5.0, 5.0 };
	GLfloat posL[4] = { 3.0, 3.0, 3.0, 0.0 };
	// light position
	glLightfv(GL_LIGHT0, GL_POSITION, posL);
	// light colors 
	glLightfv(GL_LIGHT0, GL_AMBIENT, amb);
	glLightfv(GL_LIGHT0, GL_DIFFUSE, diff);

	
	float red[4] = { 0.8f, 0.2f, 0.2f, 1.0f };
	glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, red);
	*/
	
	//point3D p[] = { { 0, 1, 0 }, { 1, 1, 0 }, { 1, -1, 0 }, { 0, -1, 0 } };
	//genericDraw(p,4,20).draw();

	//glEnable(GL_LIGHTING);
	//float a[4] = { 0.9, 0.9, 0.9, 0.9 };
	//glLightfv(GL_LIGHT0, GL_AMBIENT,a);
	actualScene.draw();

	calcFPS();
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

	
	if (argc == 2) {
		_start(argv[1]);
	}
	else {
		cin >> filename;
		_start(filename.c_str());
	}
	
	return 1;
}

