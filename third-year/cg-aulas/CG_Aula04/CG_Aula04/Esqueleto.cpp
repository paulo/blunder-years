#include <stdlib.h>
#include <iostream>
#include <cstdio>
#include <glew.h>
#include <GL/glut.h>
#define _USE_MATH_DEFINES
#pragma comment(lib,"glew32.lib")

#include <math.h>

#define _PI_ 3.14159

float alpha = 0.0f, beta = 0.0f, radius = 5.0f;
float camX = 55, camY = 55, camZ = 55;

GLuint buffer[1];

typedef struct point3D {
	float x;
	float y;
	float z;
} Point3D;

int time, timebase, frame = 0, fps = 0;
char print[20] = "";

// declare variables for VBO id 
//...

void sphericalToCartesian() {

	camX = radius * cos(beta) * sin(alpha);
	camY = radius * sin(beta);
	camZ = radius * cos(beta) * cos(alpha);
}


void changeSize(int w, int h) {

	// Prevent a divide by zero, when window is too short
	// (you cant make a window with zero width).
	if(h == 0)
		h = 1;

	// compute window's aspect ratio 
	float ratio = w * 1.0 / h;

	// Reset the coordinate system before modifying
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	
	// Set the viewport to be the entire window
    glViewport(0, 0, w, h);

	// Set the correct perspective
	gluPerspective(45,ratio,1,1000);

	// return to the model view matrix mode
	glMatrixMode(GL_MODELVIEW);
}


void drawCilinderAux(Point3D points[], int camadas, int fatias){

	int i, j;
	float angulo_circ = 2 * ((float)M_PI) / fatias; //angulo para calcular o tamanho de cada camada
	float circ_aux1, circ_aux2 = 0; // angulos auxiliares de fatias

	//glBegin(GL_TRIANGLES);

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

	//glEnd();
}

void createCircleAux(float raio, int fatias, float altura, int orient){
	int i;
	float angulo_cir = 2 * ((float) M_PI) / fatias;

	//glBegin(GL_TRIANGLES);

	if (orient){
		for (i = 0; i < fatias; i++) {
			glVertex3f( 0, altura, 0 );
			glVertex3f( raio * sin(angulo_cir*i), altura, raio * cos(angulo_cir*i) );
			glVertex3f( raio * sin(angulo_cir*(i + 1)), altura, raio * cos(angulo_cir*(i + 1)) );
		}
	} else {
		for (i = 0; i < fatias; i++) {
			glVertex3f( 0, altura, 0 );
			glVertex3f( raio * sin(angulo_cir*(i + 1)), altura, raio * cos(angulo_cir*(i + 1)) );
			glVertex3f( raio * sin(angulo_cir*i), altura, raio * cos(angulo_cir*i) );
		}
	}

	//glEnd();
}

void drawCylinder(float raiob, float altura, int fatias, int camadas){

	int i, j;
	float tam_camadas = altura / camadas; // auxiliar da altura das camadas
	float meio = altura / 2;

	point3D *points = new point3D[camadas + 2];

	glBegin(GL_TRIANGLES);

	points[0].x = 0;
	points[0].y = meio;

	for (i = 0; i <= camadas; i++){
		points[i].x = raiob;
		points[i].y = meio - i * tam_camadas ;
	}

	points[i].x = 0;
	points[i].y = -meio;
	
	drawCilinderAux(points, camadas + 2, fatias);

	createCircleAux(raiob, fatias, meio, 1);


	glEnd();
}


/*----------------------------------------------------------------------------------- 
	Drawing cylinder with VBOs 
-----------------------------------------------------------------------------------*/


void drawCylinderVBO() {

//	Bind and semantics

	//glBindBuffer(GL_ARRAY_BUFFER,buffers[0]);
	//glVertexPointer(3,GL_FLOAT,0,0);

//  Draw





	//glDrawElements(GL_TRIANGLES, count ,GL_UNSIGNED_INT, indices); // with index list

	// glDrawArrays(GL_TRIANGLES, first, count); // without index list
}

/*----------------------------------------------------------------------------------- 
	Create the VBO for the cylinder
-----------------------------------------------------------------------------------*/

void prepareCylinder(float altura,float radius,int lados) {

//	Enable buffer
	glEnableClientState(GL_VERTEX_ARRAY);


// Allocate and fill vertex array

	// array for vertices
	float *vertexB;
	// fill arrays with vertex values
	// array for indices
	unsigned int *indices;
	// fill arrays with indices

// Generate VBOs
	/*GLuint buffers[n];
	
	glGenBuffers(n, buffers);
	glBindBuffer(GL_ARRAY_BUFFER,buffers[0]);
	glBufferData(GL_ARRAY_BUFFER,arraySize, vertexB, GL_STATIC_DRAW);*/


}

/*----------------------------------------------------------------------------------- 
		RENDER SCENE
-----------------------------------------------------------------------------------*/

void renderScene(void) {

	float pos[4] = {1.0, 1.0, 1.0, 0.0};

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glLoadIdentity();
	glLightfv(GL_LIGHT0, GL_POSITION, pos);
	gluLookAt(camX,camY,camZ, 
		      0.0,0.0,0.0,
			  0.0f,1.0f,0.0f);

	//drawCylinderVBO();
	drawCylinder(4, 6, 10, 10);

	frame=0;
	frame++;
	time=glutGet(GLUT_ELAPSED_TIME);
	if (time - timebase > 1000) {
		fps = frame*1000.0/(time-timebase);
		timebase = time;
		frame = 0;
		sprintf(print, "%d", fps);
		glutSetWindowTitle(print);
	}

// End of frame
	glutSwapBuffers();
}


// special keys processing function
void processKeys(int key, int xx, int yy) 
{
	switch(key) {
	
		case GLUT_KEY_RIGHT: 
						alpha -=0.1; break;

		case GLUT_KEY_LEFT: 
						alpha += 0.1; break;

		case GLUT_KEY_UP : 
						beta += 0.1f;
						if (beta > 1.5f)
							beta = 1.5f;
						break;

		case GLUT_KEY_DOWN: 
						beta -= 0.1f; 
						if (beta < -1.5f)
							beta = -1.5f;
						break;

		case GLUT_KEY_PAGE_UP : radius -= 0.1f; 
			if (radius < 0.1f)
				radius = 0.1f;
			break;

		case GLUT_KEY_PAGE_DOWN: radius += 0.1f; break;
	
	}
	sphericalToCartesian();
	glutPostRedisplay();
}


void main(int argc, char **argv) {
	

// initialization
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowPosition(100, 100);
	glutInitWindowSize(800, 800);
	glutCreateWindow("Aula 4");
	glPolygonMode(GL_FRONT,GL_LINE);
		
// callback registry 
	glutDisplayFunc(renderScene);
	glutIdleFunc(renderScene);
	glutReshapeFunc(changeSize);
	glutSpecialFunc(processKeys);

	glewInit();

	//drawCylinderVBO();

// OpenGL settings 
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);
	glClearColor(0.0f,0.0f,0.0f,0.0f);

// init
	sphericalToCartesian();
	prepareCylinder(2,1,10);

// enter GLUTs main cycle
	glutMainLoop();
}
