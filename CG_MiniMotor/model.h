#ifndef __MODEL_H__
#define __MODEL_H__
#include "stdafx.h"
#include <string>
#include <vector>
#include <iostream>

using namespace std;

class Drawable {
public:
	virtual void draw()=0;
};

class GTransformation {
public:
	virtual void doTransformation()=0;
};

class Figure: public Drawable {
	vector<Point3D> triangles;
public:
	void draw();
	void fromFile(string  file);
};

class FigureVBO : public Drawable {
	GLuint index;
	GLuint *indices;
	GLuint nIndices;
public:
	void draw();
	void fromFile(string  file);
};


class Group: public Drawable {
	vector<Drawable*> elements;
	vector<GTransformation*> transformations;
public:
	void reset();
	void append(Drawable* element);
	void appendTransformation(GTransformation* element);
	void draw();
};

class Scene: public Group {
	int drawMode;
	float camX;
	float camY;
	float camZ;

public:
	void setCameraPosition(float x, float y, float z);
	Point3D getCameraPosition();
	static const int DRAWMODE_VBO;
	static const int DRAWMODE_DIRECT;
	int parseXML(XMLNode* root, Group* current);
	void setDrawMode(const int mode);
};

class Translation : public GTransformation {
	point3D transVector;
public:
	Translation(float x, float y, float z);
	void doTransformation();
};

class Rotation : public GTransformation {
	point3D p;
	float angle;
public:
	Rotation(float alfa, float x, float y, float z);
	void doTransformation();
};

class Scale : public GTransformation {
	point3D scale;
public:
	Scale(float x, float y, float z);
	void doTransformation();
};


#endif