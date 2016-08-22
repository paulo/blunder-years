#ifndef __PATCH_H__
#define __PATCH_H__

#include "model.h"
#include <fstream>
#include <sstream>

class Patch{
public:
	Figure readPatch(string patch, int tess);
private:
	Point3D Calculate(float u, float v);
	Point3D CalculateV(float t, Point3D* pnts);
	Point3D CalculateU(float t, int linha);
};

#endif;