#include "stdafx.h"
#include "FigureFactory.h"

/*
*Cria uma figura com a forma cónica
*
*@param	raio	raio do base do cone
*@param altura	altura do cone
*@param	fatias	número de fatias
*@param camadas número de camadas
*@return  figura com forma cónica criada
*/
Figure FigureFactory::createCone(float raiob, float altura, int fatias, int camadas){
	Figure f;
	float angulo_circ = 2 * ((float)M_PI) / fatias; // auxiliar para calcular o tamanho das fatias
	float raiob_aux1, raiob_aux2 = raiob; // auxiliares de pontos da base
	float altura_aux1, altura_aux2 = 0; // auxiliares de pontos altura

	int i, j;
	float ind = 1, factor_raio = ind / camadas; // factor que determina a apóximação ao centro do cone
	float tam_camadas = altura / camadas; // auxiliar da altura das camadas

	createCircleAux(&f, raiob, fatias, altura_aux2, 1);

	for (i = 0; i<camadas; i++){
		altura_aux1 = altura_aux2;
		altura_aux2 += tam_camadas;
		raiob_aux1 = raiob_aux2;
		ind -= factor_raio;
		raiob_aux2 = ind * raiob;

		for (j = 0; j<fatias; j++){
			f.append({ raiob_aux2 * sin(angulo_circ * j), altura_aux2, raiob_aux2 * cos(angulo_circ * j) });
			f.append({ raiob_aux1 * sin(angulo_circ * j), altura_aux1, raiob_aux1 * cos(angulo_circ * j) });
			f.append({ raiob_aux2 * sin(angulo_circ * (j + 1)), altura_aux2, raiob_aux2 * cos(angulo_circ * (j + 1)) });

			f.append({ raiob_aux1 * sin(angulo_circ * j), altura_aux1, raiob_aux1 * cos(angulo_circ * j) });
			f.append({ raiob_aux1 * sin(angulo_circ * (j + 1)), altura_aux1, raiob_aux1 * cos(angulo_circ * (j + 1)) });
			f.append({ raiob_aux2 * sin(angulo_circ * (j + 1)), altura_aux2, raiob_aux2 * cos(angulo_circ * (j + 1)) });

		}
	}
	return f;
}