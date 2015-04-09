#include "stdafx.h"
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

	f.appendPoint({ -x / 2, y / 2, z / 2 }); // 0
	f.appendPoint({ -x / 2, y / 2, -z / 2 }); // 1
	f.appendPoint({ x / 2, y / 2, -z / 2 }); // 3
	f.appendPoint({ x / 2, y / 2, z / 2 });  // 2

	f.appendPoint({ -x / 2, -y / 2, z / 2 }); // 4
	f.appendPoint({ -x / 2, -y / 2, -z / 2 }); // 5
	f.appendPoint({ x / 2, -y / 2, -z / 2 }); // 7
	f.appendPoint({ x / 2, -y / 2, z / 2 }); // 6

	createIndiceStripe(&f, 1,0,2,3);
	createIndiceStripe(&f, 4,5,7,6);
	
	createIndiceStripe(&f, 1,5,0,4);
	createIndiceStripe(&f, 3,7,2,6);
	
	createIndiceStripe(&f, 2,6,1,5);
	createIndiceStripe(&f, 0,4,3,7);

	return f;
}

void FigureFactory::createIndiceStripe(Figure* f, float a1, float a2, float a3, float a4){
	f->appendIndice(a1); f->appendIndice(a2); f->appendIndice(a3);
	f->appendIndice(a3); f->appendIndice(a2); f->appendIndice(a4);
}

/*
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

*/