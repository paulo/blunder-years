#include "stdafx.h"
#include "FigureFactory.h"

Figure FigureFactory::createTunnel(float insideRadius, float outsideRadius, int nsides, int rings){
	Figure f;
	point3D *points = new point3D[nsides + 2];
	float raio = (outsideRadius - insideRadius)/2;
	float angulo_cir = 2 * ((float)M_PI) / nsides;
	int i;

	for (i = 0; i < nsides; i++) {
		points[i].x = insideRadius + raio + raio* sin(i*angulo_cir);
		points[i].y = raio * cos(i*angulo_cir);
	}
	points[i].x = points[0].x;
	points[i].y = points[0].y;

	createRotate(&f, points, nsides + 1, rings);
	return f;
}