#include "stdafx.h"
#include "FigureFactory.h"

Figure FigureFactory::createTree(float raiob, float alturab, float raioc, float altura, int fatias, int camadas){
	Point3D points[] = {
			{0,altura,0},
			{raioc,alturab,0},
			{raiob,alturab,0},
			{raiob, 0, 0 },
			{0,0,0}
	};
	Figure f;
	createRotate(&f, points, 5 , fatias);

	return f;
}