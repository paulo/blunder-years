#ifndef __MODEL_H__
#define __MODEL_H__
#include "stdafx.h"
#include <string>
#include <vector>
#include <iostream>

using namespace std;

typedef struct point3D {
	float x;
	float y;
	float z;
} Point3D;


class Figure {
	vector<Point3D> triangles;
public:
	void draw();
	void fromFile(string  file);
};

class Scene {
	vector<Figure> figures;
public:
	void parseXML(XMLNode* root);
	void append(Figure figure);
	void draw();
};

#endif