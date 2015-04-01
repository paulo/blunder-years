#ifndef __FIGURE_FACTORY_H__
#define __FIGURE_FACTORY_H__
#include "model.h"

class FigureFactory{
public:
	Figure createSphere(float radius, int slices, int laers);
	Figure createCube(float height);
	Figure createCone(float raiob, float altura, int fatias, int camadas);
	Figure createTunnel(float insideRadius, float outsideRadius, int nsides, int rings);
	Figure createParallelepiped(float x, float y, float z);
	Figure createPlane(float x,float z);
	Figure createCircle(float raio, int fatias);
	Figure createCylinder(float raiob, float altura, int fatias, int camadas);
	Figure createTree(float raiob, float alturab, float raioc, float altura, int fatias, int camadas);
private :
	void createPlaneAux(Figure* f, float p[3], int axisFirst, int axisSecond);
	void createCircleAux(Figure* f, float raio, int fatias, float altura, int orient);
	void createRotate(Figure* f, Point3D points[], int camadas, int fatias);
};
#endif