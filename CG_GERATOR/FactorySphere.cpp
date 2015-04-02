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

int convertPointIndice(bool isXZero, int nPFatia, int nSharedP, int ifatia, int jP ){
	if (isXZero){
		return jP;
	}
	else{
		return jP - (nPFatia - nSharedP);
	}
}

void FigureFactory::createRotate(Figure* f, Point3D points[], int camadas, int fatias){
	int i, j, nPNCperF; // nPoints not Common per Fatia 
	float angulo_circ = 2 * ((float)M_PI) / fatias; //angulo para calcular o tamanho de cada camada
	int ii,in;


	// create all initial the points, 
	for (j = 0; j < camadas; j++) {
		if (points[j].x == 0){
			points[j].z = f->appendPoint({ 0, points[j].y, points[j].x });
		}
	}
	nPNCperF = 0;
	for (j = 0; j < camadas; j++) {
		if (points[j].x != 0){
			points[j].z = f->appendPoint({ 0, points[j].y, points[j].x });
			nPNCperF++;
		}
	}
	ii = 0; in = nPNCperF;
	for (i = 0; i<fatias; i++){
		//append dos pontos
		float circ = angulo_circ * (i + 1);

		// In the end connect to the init
		if (i + 1 == fatias){
			in = 0;
		}
		else {
			for (j = 0; j < camadas; j++) {
				//if points[j].x is 0 
				//		then the point is the same in all fatias
				//		then can be used the point created in the first first for
				if (points[j].x != 0){
					f->appendPoint({ points[j].x * sin(circ), points[j].y, points[j].x * cos(circ) });
				}
			}
		}

		//append das ligaçoes atravez indices
		
		for (j = 0; j < camadas - 1; j++) {
			if (points[j].x == 0) {
				f->appendIndice(points[j].z);
				f->appendIndice(points[j + 1].z + ii);
				f->appendIndice(points[j + 1].z + in);
				//f->appendIndice(j);
				//f->appendIndice(ii + j + 1);
				//f->appendIndice(in + j);
			}
			else if (points[j + 1].x == 0) {
				f->appendIndice(points[j].z  + ii);
				f->appendIndice(points[j + 1].z);
				f->appendIndice(points[j].z + in );
			} else {
				f->appendIndice(points[j].z + ii);
				f->appendIndice(points[j + 1].z + ii);
				f->appendIndice(points[j].z + in);

				f->appendIndice(points[j].z + in);
				f->appendIndice(points[j + 1].z + ii);
				f->appendIndice(points[j + 1].z + in);
			}
		}

		ii = in;
		in += nPNCperF;

		
		/*
		for (j = 0; j < camadas - 1; j++) {

			f->append({ points[j].x * sin(circ_aux1), points[j].y, points[j].x * cos(circ_aux1) });
			f->append({ points[j + 1].x * sin(circ_aux1), points[j + 1].y, points[j + 1].x * cos(circ_aux1) });
			f->append({ points[j].x * sin(circ_aux2), points[j].y, points[j].x  * cos(circ_aux2) });

			f->append({ points[j + 1].x * sin(circ_aux1), points[j + 1].y, points[j + 1].x * cos(circ_aux1) });
			f->append({ points[j + 1].x * sin(circ_aux2), points[j + 1].y, points[j + 1].x * cos(circ_aux2) });
			f->append({ points[j].x * sin(circ_aux2), points[j].y, points[j].x * cos(circ_aux2) });
		}
		*/
	}
}
