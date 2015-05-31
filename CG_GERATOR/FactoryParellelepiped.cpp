#include "stdafx.h"
#include "FigureFactory.h"


void createIndiceStripe1(Figure* f, Point3D a1, Point3D a2, Point3D a3, Point3D a4, Point3D normal){
	int i1, i2, i3, i4;
	
	f->appendPointTexture({0,1,0}); // 0
	f->appendNormal(normal);
	i1 = f->appendPoint(a1); // 0
	
	f->appendPointTexture({ 0, 0, 0 }); // 0
	f->appendNormal(normal);
	i2 = f->appendPoint(a2); // 1

	f->appendPointTexture({ 1, 1, 0 }); // 0
	f->appendNormal(normal);
	i3 = f->appendPoint(a3); // 3

	f->appendPointTexture({ 1, 0, 0 }); // 0
	f->appendNormal(normal);
	i4= f->appendPoint(a4);  // 2
	
	f->appendIndice(i1); f->appendIndice(i2); f->appendIndice(i3);
	f->appendIndice(i3); f->appendIndice(i2); f->appendIndice(i4);
}

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

	Point3D p0 = { -x / 2, y / 2, z / 2 };
	Point3D p1 = { -x / 2, y / 2, -z / 2 };
	Point3D p2 = { x / 2, y / 2, -z / 2 };
	Point3D p3 = { x / 2, y / 2, z / 2 };
	Point3D p4 = { -x / 2, -y / 2, z / 2 };
	Point3D p5 = { -x / 2, -y / 2, -z / 2 };
	Point3D p6 = { x / 2, -y / 2, -z / 2 };
	Point3D p7 = { x / 2, -y / 2, z / 2 };

	createIndiceStripe1(&f, p1, p0, p2, p3, { 0, 1, 0 });
	createIndiceStripe1(&f, p4, p5, p7, p6, { 0, -1, 0 });

	createIndiceStripe1(&f, p1, p5, p0, p4, { -1, 0, 0 });
	createIndiceStripe1(&f, p3, p7, p2, p6, { 1, 0, 0 });

	createIndiceStripe1(&f, p2, p6, p1, p5, { 0, 0, -1 });
	createIndiceStripe1(&f, p0, p4, p3, p7, { 0, 0, 1 });

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