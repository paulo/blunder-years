#include <GL/glut.h>
#include <math.h>


float posX = 0, posY = 0, posZ = 0;
float camX = 10, camY = 10, camZ = 10;
float anguloRotacaoX = 0, anguloRotacaoY = 0, anguloRotacaoZ = 0;


void drawPiramid(float altura, float lado){

	glRotatef(anguloRotacaoX, 1, 0, 0);
	glRotatef(anguloRotacaoY, 0, 1, 0);
	glRotatef(anguloRotacaoZ, 0, 0, 1);

	glBegin(GL_TRIANGLES);
	//base
	glColor3f(0, 1, 0);
	glVertex3f(lado/2, 0, lado/2);
	glVertex3f(-lado/2, 0, -lado/2);
	glVertex3f(lado/2, 0, -lado/2);
	
	glVertex3f(lado/2, 0, lado/2);
	glVertex3f(-lado/2, 0, lado/2);
	glVertex3f(-lado/2, 0, -lado/2);

	//lados
	glColor3f(1,0,0);
	glVertex3f(lado/2, 0, lado/2);
	glVertex3f(lado/2, 0, -lado/2);
	glVertex3f(0, altura, 0);
	glColor3f(0,0,1);
	glVertex3f(lado/2, 0, -lado/2);
	glVertex3f(-lado/2, 0, -lado/2);
	glVertex3f(0, altura, 0);
	glColor3f(1,1,0);
	glVertex3f(-lado/2, 0, -lado/2);
	glVertex3f(-lado/2, 0, lado/2);
	glVertex3f(0, altura, 0);
	glColor3f(1,1,1);
	glVertex3f(lado/2, 0, lado/2);
	glVertex3f(0, altura, 0);
	glVertex3f(-lado/2, 0, lado/2);


	glEnd();
}


void changeSize(int w, int h) {

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



void renderScene(void) {

	// clear buffers
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// set the camera
	glLoadIdentity();
	gluLookAt(camX, camY, camZ,
		0.0, 0.0, 0.0,
		0.0f, 1.0f, 0.0f);

	// pôr instruções de desenho aqui
	drawPiramid(4,3);

	// End of frame
	glutSwapBuffers();
}



// escrever função de processamento do teclado
void teclas_normais(unsigned char tecla, int x, int y){
	float value = 10;

	switch (tecla)
	{
	case 'a': anguloRotacaoY+=value;
		break;
	case 'd': anguloRotacaoY-=value;
		break;
	case 'w': anguloRotacaoZ+=value;
		break;
	case 's': anguloRotacaoZ-=value;
		break;
	default:
		break;
	}

	glutPostRedisplay();
}

void teclas_especiais(int tecla, int x, int y){
	float value = 3;

	switch (tecla)
	{
	case GLUT_KEY_UP: camX+=value;
		break;
	case GLUT_KEY_DOWN: camX-=value;
		break;
	case GLUT_KEY_LEFT: camY+=value;
		break;
	case GLUT_KEY_RIGHT: camY-=value;
		break;
	default:
		break;
	}
	glutPostRedisplay();

}




//void rato(int botão, int estado, int x, int y);




// escrever função de processamento do menu


//int glutCreateMenu(nome_função);

//glutPostRedisplay()

int main(int argc, char **argv) {

	// inicialização
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowPosition(100, 100);
	glutInitWindowSize(800, 800);
	glutCreateWindow("Aula 2");
	//glPolygonMode(GL_FRONT,GL_LINE);

	// registo de funções 
	glutDisplayFunc(renderScene);
	glutIdleFunc(renderScene);
	glutReshapeFunc(changeSize);

	// pôr aqui registo da funções do teclado e rato
	glutKeyboardFunc(teclas_normais);
	glutSpecialFunc(teclas_especiais);

	//glutMouseFunc(rato);


	//glutMotionFunc(nome_função);
	//glutPassiveMotionFunc(nome_função);

	// pôr aqui a criação do menu


	// alguns settings para OpenGL
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);

	// entrar no ciclo do GLUT 
	glutMainLoop();

	return 1;
}

