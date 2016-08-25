#include <stdlib.h>
#include <string.h>
#include <GL/glut.h>
#define _USE_MATH_DEFINES
#include <math.h>


float posX = 0, posY = 0, posZ = 0;
float camX = 10, camY = 10, camZ = 10;
float anguloRotacaoX = 0, anguloRotacaoY = 0, anguloRotacaoZ = 0;
int r=50;
float angulo_circ = 45, angulo_h = 45;
float outcircle = 0, incircle = 0;
//static CameraFP cameraFP = CameraFP();

void drawTree(float raio_base, float altura){

	glPushMatrix();
	glRotatef(90, -1, 0, 0);
	glColor3ub(51, 25, 0);
	glutSolidCone(raio_base/3,altura/2, 10, 10);
	glPopMatrix();
	glPushMatrix();
	glTranslatef(0, altura/3, 0);
	glRotatef(90, -1, 0, 0);
	glColor3ub(0, 103, 0);
	glutSolidCone(raio_base, altura/2, 10, 10);
	glPopMatrix();
}


//nao percebo muito bem as contas que se estão a passar aqui
void drawForest(int nmr_trees, float raio_base, float altura){

	float x, z;
	srand(100);
	while(nmr_trees>0){
		x = rand()%99;
		z = rand()%99;
	
		if(sqrt(x*x+z*z)<50){
			if(rand()%2==0)
				x = rand()%49+50;
			else
				z = rand()%49+50;
		}

		if(rand()%2==0)
			x=-x;
		if(rand()%2==0)
			z=-z;

		glPushMatrix();
			glTranslatef(x,0,z);
			drawTree(raio_base, altura);
		glPopMatrix();
		nmr_trees--;
	}
}

void drawPlano(float comp, float larg){

	glRotatef(anguloRotacaoX, 1, 0, 0);
	glRotatef(anguloRotacaoY, 0, 1, 0);
	glRotatef(anguloRotacaoZ, 0, 0, 1);

	glBegin(GL_TRIANGLES);

	glColor3f(0, 1, 0);
	glVertex3d(comp/2, 0, -larg/2);
	glVertex3d(-comp/2, 0, -larg/2);
	glVertex3d(-comp/2, 0, larg/2);

	glColor3f(0, 1, 0);
	glVertex3d(comp/2, 0, -larg/2);
	glVertex3d(-comp/2, 0, larg/2);
	glVertex3d(comp/2, 0, larg/2);

	glEnd();
}

//Funçao para desenhar os pots. Esta com um problema na rotaçao dos pots azuis, nao se viram para o lado certo
void drawTeapots(float raio, float dim, char *color, int quant){
	int i=0;
	float angrad = 2*M_PI/quant; //em radianos
	float anggra = 360/quant;//em graus
	for(i=0;i<quant;i++){
		glPushMatrix();
		if(strcmp(color, "blue") == 0) {
			glRotatef(incircle, 0, 1, 0);	//para rodar os pots a cada frame
			incircle+=0.01;
			glColor3f(0, 0, 1);
			glTranslatef(raio*sin(angrad*i),dim,raio*cos(angrad*i));				
			glRotatef(anggra*i,0,1,0);

		} else {
			glRotatef(outcircle, 0, 1, 0);	//para rodar os pots a cada frame
			outcircle-=0.01;
			glColor3f(1,0,0);
			glTranslatef(raio*sin(angrad*i),dim,raio*cos(angrad*i));
			glRotatef(anggra*i,0,1,0);
		}
		glutSolidTeapot(dim);
		glPopMatrix();
	}
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

void drawTorus(float in_raio, float out_raio){

	glPushMatrix();
	glColor3ub(255,102,255);
	glRotatef(90, 0, 1, 0);
	glutSolidTorus(in_raio, out_raio, 10, 10);
	glPopMatrix();

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
	drawPlano(200,200);
	drawTorus(1,2);
	drawTeapots(15, 1, "blue", 8);
	drawTeapots(35, 1, "red", 16);
	drawForest(100, 2, 6);
	// End of frame
	glutSwapBuffers();
}



// escrever função de processamento do teclado
void teclas_normais(unsigned char tecla, int x, int y){
	float value = 4;

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


void processaMenu(int option){
	return;
}

void processaCamaras(int option){
	return;
}

void processaOutros(int option){
	return;
}

void criarMenu(){

	int m_principal, m_camaras, m_outros;

	m_camaras = glutCreateMenu(processaCamaras);
	glutAddMenuEntry("First Person", 1);
	glutAddMenuEntry("Esferica", 2);

	m_outros = glutCreateMenu(processaOutros);
	glutAddMenuEntry("Outros 1", 1);
	glutAddMenuEntry("Outros 2", 2);
	glutAddMenuEntry("Outros 3", 3);
	glutAddMenuEntry("Outros 4", 4);
	
	m_principal = glutCreateMenu(processaMenu);
	glutAddSubMenu("Camaras", m_camaras);
	glutAddSubMenu("Outros", m_outros);

	glutAttachMenu(GLUT_RIGHT_BUTTON);

}

int main(int argc, char **argv) {

	// inicialização
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowPosition(100, 100);
	glutInitWindowSize(800, 800);
	glutCreateWindow("Aula 5");
	//glPolygonMode(GL_FRONT,GL_LINE);

	// registo de funções 
	glutDisplayFunc(renderScene);
	glutIdleFunc(renderScene);// mete muito lento
	glutReshapeFunc(changeSize);

	// pôr aqui registo da funções do teclado e rato
	glutKeyboardFunc(teclas_normais);
	glutSpecialFunc(teclas_especiais);
    //glutMouseFunc(rato);
    //glutMotionFunc(mov_rato);


	//menu
	criarMenu();



	// alguns settings para OpenGL
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);

	// entrar no ciclo do GLUT 
	glutMainLoop();

	return 1;
}

