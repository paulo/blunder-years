#include "stdafx.h"
#include "FigureFactory.h"

/*
*Cria uma figura com a forma esférica
*
*@param	raio	raio da esfera
*@param camadas número de camadas
*@param	fatias	número de fatias
*@return  figura com forma esférica criada
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
	int i, j, ii, in, nPNCperF = 0; // nPoints not Common per Fatia 
	float angulo_circ = 2 * ((float)M_PI) / fatias; //angulo para calcular o tamanho de cada camada
	int *auxIndex = new int[camadas];

	// initialize centrall points, and count the "nPoints not Common per Fatia"
	for (j = 0; j < camadas; j++) {
		if (points[j].x == 0){ auxIndex[j] = f->appendPoint({ 0, points[j].y, 0 }); }
		else { nPNCperF++; }
	}
	// initialize  other points
	for (j = 0; j < camadas; j++) {
		if (points[j].x != 0) { auxIndex[j] = f->appendPoint({ 0, points[j].y, points[j].x }); }
	}
	// Create all other points
	for (i = 0; i < fatias - 1; i++){
		float circ = angulo_circ * (i + 1);
		for (j = 0; j < camadas; j++) {
			if (points[j].x != 0) {f->appendPoint({ points[j].x * sin(circ), points[j].y, points[j].x * cos(circ) });}
		}
	}
	// create trigangles
	ii = 0; in = nPNCperF;
	for (i = 0; i < fatias; i++) {
		for (j = 0; j < camadas - 1; j++) {
			if (points[j].x != 0) {
				f->appendIndice(auxIndex[j] + ii);
				f->appendIndice((points[j + 1].x == 0) ? auxIndex[j + 1] : auxIndex[j + 1] + ii);
				f->appendIndice(auxIndex[j] + in);
			}
			if (points[j + 1].x != 0) {
				f->appendIndice((points[j].x == 0) ? auxIndex[j] : auxIndex[j] + in);
				f->appendIndice(auxIndex[j + 1] + ii);
				f->appendIndice(auxIndex[j + 1] + in);
			}
		}

		ii = in; in += nPNCperF;
		// connect the last fatia to the first
		if (i + 2 == fatias){
			in = 0;
		}
	}
	delete(auxIndex);
}
