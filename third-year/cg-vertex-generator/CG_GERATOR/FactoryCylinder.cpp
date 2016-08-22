#include "stdafx.h"
#include "FigureFactory.h"

/*
*Cria uma figura com a forma cilíndrica
*
*@param	raio	raio do base do cilindro
*@param altura	altura do cilindro
*@param	fatias	número de fatias
*@return  figura com forma cilíndrica criada
*/
Figure FigureFactory::createCylinder(float raiob, float altura, int fatias, int camadas){
	Figure f;
/*	
	float angulo_circ = 2 * ((float)M_PI) / fatias; // auxiliar para calcular o tamanho das fatias
	float meio = altura / 2;
	float altura_aux1, altura_aux2 = -meio; // auxiliares de pontos altura
*/

	int i, j;
	float tam_camadas = altura / camadas; // auxiliar da altura das camadas
	float meio = altura / 2;
	point3D *points = new point3D[camadas + 2];

	points[0].x = 0;
	points[0].y = meio;

	for (i = 1; i <= camadas; i++){
		points[i].x = raiob;
		points[i].y = meio - i * tam_camadas ;
	}

	points[camadas + 1].x = 0;
	points[camadas + 1].y = -meio;
	
	createRotate(&f, points, camadas + 2, fatias);


	//base
	/*createCircleAux(&f, raiob, fatias, -meio, 0);
	for (i = 0; i<camadas; i++){
		altura_aux1 = altura_aux2;
		altura_aux2 += tam_camadas;

		for (j = 0; j<fatias; j++){
			f.append({ raiob * sin(angulo_circ * j), altura_aux2, raiob * cos(angulo_circ * j) });
			f.append({ raiob * sin(angulo_circ * j), altura_aux1, raiob * cos(angulo_circ * j) });
			f.append({ raiob * sin(angulo_circ * (j + 1)), altura_aux2, raiob * cos(angulo_circ * (j + 1)) });

			f.append({ raiob * sin(angulo_circ * j), altura_aux1, raiob * cos(angulo_circ * j) });
			f.append({ raiob * sin(angulo_circ * (j + 1)), altura_aux1, raiob * cos(angulo_circ * (j + 1)) });
			f.append({ raiob * sin(angulo_circ * (j + 1)), altura_aux2, raiob * cos(angulo_circ * (j + 1)) });

		}
	}
	*/

	//baixo
	//createCircleAux(&f, raiob, fatias, meio, 1);
	return f;
}
