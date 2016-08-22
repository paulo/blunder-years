#ifndef __MODEL_H__
#define __MODEL_H__

#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>

using namespace std;

typedef struct point3D {
	float x;
	float y;
	float z;
} Point3D;

class Figure {
	vector<Point3D> triangles;
	vector<unsigned int> indices;
	vector<Point3D> normal;
	vector<Point3D> texture;
public:
	void toFile(string  file);
	void toFileVBO(string  file);
	void append(Point3D p);
	unsigned int appendIndice(unsigned int p);
	void appendNormal(Point3D p);
	int appendPoint(Point3D p);
	void appendPointTexture(Point3D p);
	vector<unsigned int> getIndexes();
	vector<Point3D>* getPoints();
	vector<Point3D>* getNormals();
};

#endif