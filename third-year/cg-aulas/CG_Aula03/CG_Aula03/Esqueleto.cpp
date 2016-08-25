#include <GL/glut.h>
#define _USE_MATH_DEFINES
#include <math.h>

float posX = 0, posY = 0, posZ = 0;
float anguloRotacaoX = 0, anguloRotacaoY = 0, anguloRotacaoZ = 0;
float r = 15;
float angulo_h = 45;
float angulo_circ = 45;

typedef struct point3D {
	float x;
	float y;
	float z;
} Point3D;

void drawSphereAux(Point3D points[], int camadas, int fatias);

void drawSphere(float raio, int camadas, int fatias){

	int i;
	float meio = ((float)M_PI) / 2;
	float angulo = ((float) M_PI) / camadas;
	point3D *points = new point3D[camadas];

	for (i = 0; i < camadas; i++){
		points[i].x = raio * cos(meio);
		points[i].y = raio * sin(meio);

		meio -= angulo;
	}
	drawSphereAux(points, camadas, fatias);
}


void drawSphereAux(Point3D points[], int camadas, int fatias){

	int i, j;
	float angulo_circ = 2 * ((float)M_PI) / fatias; //angulo para calcular o tamanho de cada camada
	float circ_aux1, circ_aux2 = 0; // angulos auxiliares de fatias
	
	glRotatef(anguloRotacaoX, 1, 0, 0);
	glRotatef(anguloRotacaoY, 0, 1, 0);
	glRotatef(anguloRotacaoZ, 0, 0, 1);


	glBegin(GL_TRIANGLES);

	for (i = 0; i<fatias; i++){
		circ_aux1 = circ_aux2;
		circ_aux2 += angulo_circ;

		for (j = 0; j < camadas-1; j++) {

			glVertex3f( points[j].x * sin(circ_aux1), points[j].y, points[j].x * cos(circ_aux1) );
			glVertex3f( points[j + 1].x * sin(circ_aux1), points[j + 1].y, points[j + 1].x * cos(circ_aux1) );
			glVertex3f( points[j].x * sin(circ_aux2), points[j].y, points[j].x  * cos(circ_aux2) );

			glVertex3f( points[j + 1].x * sin(circ_aux1), points[j + 1].y, points[j + 1].x * cos(circ_aux1) );
			glVertex3f( points[j + 1].x * sin(circ_aux2), points[j + 1].y, points[j + 1].x * cos(circ_aux2) );
			glVertex3f( points[j].x * sin(circ_aux2), points[j].y, points[j].x * cos(circ_aux2) );
		}
	}

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
	gluLookAt(r*cos(angulo_h)*sin(angulo_circ), r*cos(angulo_h)*cos(angulo_circ), r*sin(angulo_h),
		0.0, 0.0, 0.0,
		0.0f, 1.0f, 0.0f);

	// pôr instruções de desenho aqui
	drawSphere(4,10,10);

	// End of frame
	glutSwapBuffers();
}



// escrever função de processamento do teclado
void teclas_normais(unsigned char tecla, int x, int y){
	float value = 0.7;

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
	case 'i': r-=value;
		break;
	case 'o': r+=value;
		break;
	default:
		break;
	}

	glutPostRedisplay();
}

void teclas_especiais(int tecla, int x, int y){
	float value = 0.01;

	switch (tecla)
	{
	case GLUT_KEY_UP: angulo_h+=value;
		break;
	case GLUT_KEY_DOWN: angulo_h-=value;
		break;
	case GLUT_KEY_LEFT: angulo_circ+=value;
		break;
	case GLUT_KEY_RIGHT: angulo_circ-=value;
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
	glutCreateWindow("Aula 3");
	glPolygonMode(GL_FRONT,GL_LINE);

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

