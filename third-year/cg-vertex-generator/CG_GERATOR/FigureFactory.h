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
	Figure createBezierSurface(Figure *f, std::string filename, int tess);
private :
	void createIndiceStripe(Figure* f, float a1, float a2, float a3, float a4);
	void createPlaneAux(Figure* f, float p[3], int axisFirst, int axisSecond);
	void createCircleAux(Figure* f, float raio, int fatias, float altura, int orient);
	void createRotate(Figure* f, Point3D points[], int camadas, int fatias);
};
#endif