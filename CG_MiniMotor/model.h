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

class Drawable {
public:
	virtual void draw()=0;
};

class Action {
public:
	virtual void doAction()=0;
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
	vector<Action*> actions;
public:
	void reset();
	void append(Drawable* element);
	void appendAction(Action* element);
	void draw();
};

class Scene: public Group {
	int drawMode;
public:
	static const int DRAWMODE_VBO;
	static const int DRAWMODE_DIRECT;
	int parseXML(XMLNode* root, Group* current);
	void setDrawMode(const int mode);
};

class Translation : public Action {
	point3D transVector;
public:
	Translation(float x, float y, float z);
	void doAction();
};

class Rotation : public Action {
	point3D p;
	float angle;
public:
	Rotation(float alfa, float x, float y, float z);
	void doAction();
};

class Scale : public Action {
	point3D scale;
public:
	Scale(float x, float y, float z);
	void doAction();
};


#endif