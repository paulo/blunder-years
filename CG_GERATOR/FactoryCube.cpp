#include "FigureFactory.h"


/*
*Cria uma figura com a forma cúbica
*
*@param	lado comprimento do lado do cubo
*@return  figura com forma cúbica criada
*/
Figure FigureFactory::createCube(float lado){
	Figure f;
	float meio = lado / 2;
	float p[3] = { meio, meio, meio };

	createPlaneAux(&f, p, 0, 1);
	p[2] = -p[2];
	createPlaneAux(&f, p, 2, 1);
	p[0] = -p[0];
	createPlaneAux(&f, p, 0, 1);
	p[2] = -p[2];
	createPlaneAux(&f, p, 2, 1);
	createPlaneAux(&f, p, 0, 2);
	p[1] = -p[1];
	createPlaneAux(&f, p, 2, 0);

	return f;
}
