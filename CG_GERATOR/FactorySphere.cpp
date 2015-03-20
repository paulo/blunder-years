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
	float angulo = ((float) M_PI) / camadas;
	point3D *points = new point3D[camadas];

	for (i = 0; i < camadas; i++){
		points[i].x = raio * cos(meio);
		points[i].y = raio * sin(meio);

		meio -= angulo;
	}
	createRotate(&f, points, camadas, fatias);
	/*
	for (i = 0; i<camadas; i++){
		altura_aux1 = altura_aux2;
		altura_aux2 += angulo_alt;
		circ_aux2 = 0;

		for (j = 0; j<fatias; j++) {
			circ_aux1 = circ_aux2;
			circ_aux2 += angulo_circ;

			f.append({ raio * sin(circ_aux1) * cos(altura_aux1), raio * sin(altura_aux1), raio * cos(circ_aux1) * cos(altura_aux1) });
			f.append({ raio * sin(circ_aux1) * cos(altura_aux2), raio * sin(altura_aux2), raio * cos(circ_aux1) * cos(altura_aux2) });
			f.append({ raio * sin(circ_aux2) * cos(altura_aux1), raio * sin(altura_aux1), raio * cos(circ_aux2) * cos(altura_aux1) });

			f.append({ raio * sin(circ_aux1) * cos(altura_aux2), raio * sin(altura_aux2), raio * cos(circ_aux1) * cos(altura_aux2) });
			f.append({ raio * sin(circ_aux2) * cos(altura_aux2), raio * sin(altura_aux2), raio * cos(circ_aux2) * cos(altura_aux2) });
			f.append({ raio * sin(circ_aux2) * cos(altura_aux1), raio * sin(altura_aux1), raio * cos(circ_aux2) * cos(altura_aux1) });
		}
	}*/

	return f;
}

void FigureFactory::createRotate(Figure* f, Point3D points[], int camadas, int fatias){
	int i, j;
	float angulo_circ = 2 * ((float)M_PI) / fatias; //angulo para calcular o tamanho de cada camada
	float circ_aux1, circ_aux2 = 0; // angulos auxiliares de fatias
	
	for (i = 0; i<fatias; i++){
		circ_aux1 = circ_aux2;
		circ_aux2 += angulo_circ;

		for (j = 0; j < camadas - 1; j++) {

			f->append({ points[j].x * sin(circ_aux1), points[j].y, points[j].x * cos(circ_aux1) });
			f->append({ points[j + 1].x * sin(circ_aux1), points[j + 1].y, points[j + 1].x * cos(circ_aux1) });
			f->append({ points[j].x * sin(circ_aux2), points[j].y, points[j].x  * cos(circ_aux2) });

			f->append({ points[j + 1].x * sin(circ_aux1), points[j + 1].y, points[j + 1].x * cos(circ_aux1) });
			f->append({ points[j + 1].x * sin(circ_aux2), points[j + 1].y, points[j + 1].x * cos(circ_aux2) });
			f->append({ points[j].x * sin(circ_aux2), points[j].y, points[j].x * cos(circ_aux2) });
		}
	}
}
