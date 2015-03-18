#include "FigureFactory.h"

/*
*Cria uma figura com a forma paralelepipédica
*
*@param	x comprimento do paralelipípedo
*@param y altura do paralelipípedo
*@param z largura do paralelipípedo
*@return  figura com forma paralelepipédica criada
*/
Figure FigureFactory::createParallelepiped(float x, float y, float z){
	Figure f;
	float p[3] = { x/2, y/2, z/2 };

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