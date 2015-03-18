#ifndef __MODEL_H__
#define __MODEL_H__

#include <string>
#include <vector>
#include <iostream>

using namespace std;

typedef struct point3D {
	float x;
	float y;
	float z;
} Point3D;

class Figure{
	vector<Point3D> triangles;
public:
	void toFile(string  file);
	void append(Point3D p);
};

#endif