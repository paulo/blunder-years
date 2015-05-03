#include "stdafx.h"
#include "FigureFactory.h"

/*
*Cria uma figura com a forma esf�rica
*
*@param	raio	raio da esfera
*@param camadas n�mero de camadas
*@param	fatias	n�mero de fatias
*@return  figura com forma esf�rica criada
*/
Figure FigureFactory::createSphere(float raio, int camadas, int fatias){
/*	float angulo_circ = 2 * ((float)M_PI) / fatias; //angulo para calcular o tamanho de cada camada
	float angulo_alt = ((float)M_PI) / camadas; // angulo para calcular a altura de cada camada

	float altura_aux1, altura_aux2 = (float)M_PI_2; // angulos auxiliares de camadas
	float circ_aux1, circ_aux2 = 0; // angulos auxiliares de fatias
	int i, j;*/

	Figure f; int i;
	float meio = ((float)M_PI) / 2;
	float aux;
	float angulo = ((float) M_PI) / camadas;
	point3D *points = new point3D[camadas + 1];

	
	points[0].x = 0;
	points[0].y = raio;
	aux = meio - angulo;
	for (i = 1; i < camadas; i++){
		points[i].x = raio * cos(aux);
		points[i].y = raio * sin(aux);
		aux = meio - angulo * (i + 1);
	}
	points[camadas].x = 0;
	points[camadas].y = -raio;

	createRotate(&f, points, camadas  + 1, fatias);

	return f;
}

void FigureFactory::createRotate(Figure* f, Point3D points[], int camadas, int fatias){
	int i, j, ii, in; // nPoints not Common per Fatia 
	float angulo_circ = 2 * ((float)M_PI) / fatias; //angulo para calcular o tamanho de cada camada
	Point3D *normals = new Point3D[camadas];

	// initialize centrall points, and count the "nPoints not Common per Fatia"
	for (j = 0; j < camadas; j++) {
		normals[j].x = normals[j].y = 0;
		if (j + 1 < camadas) {
			normals[j].x += -(points[j + 1].y - points[j].y);
			normals[j].y += (points[j + 1].x - points[j].x);
		}
		if (j - 1 > 0) {
			normals[j].x += -(points[j].y - points[j-1].y);
			normals[j].y += (points[j].x - points[j-1].x);
		}
	}
	// Create all other points
	for (i = 0; i < fatias; i++){
		float circ = angulo_circ * (i + 1);
		for (j = 0; j < camadas; j++) {
			f->appendPoint({ points[j].x * sin(circ), points[j].y, points[j].x * cos(circ) });
			f->appendNormal({ normals[j].x * sin(circ), normals[j].y, normals[j].x * cos(circ) });
		}
	}
	// create trigangles
	ii = 0; in = camadas;
	for (i = 0; i < fatias; i++) {
		for (j = 0; j < camadas - 1; j++) {
			f->appendIndice(j + in);
			f->appendIndice(j + 1 + ii);
			f->appendIndice(j + ii);
			
			f->appendIndice(j + 1 + in);
			f->appendIndice(j + 1 + ii);
			f->appendIndice(j + in);
		}

		ii = in; in += camadas;
		// connect the last fatia to the first
		if (i + 2 == fatias){
			in = 0;
		}
	}
	delete(normals);
}
