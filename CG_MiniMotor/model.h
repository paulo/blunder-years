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

class Component: public Drawable{
	int lightMode;
	float diff[3];
	float amb[3];
	float spec[3];
	float emit[3];
	string texture;
public:
    void setDiff(float r, float g, float b) { diff[0]=r; diff[1]=g; diff[2]=b; }
    void setAmb(float r, float g, float b) { amb[0]=r; amb[1]=g; amb[2]=b; }
    void setSpec(float r, float g, float b) { spec[0]=r; spec[1]=g; spec[2]=b; }
    void setEmit(float r, float g, float b) { emit[0]=r; emit[1]=g; emit[2]=b; }
    //void loadTexture(string file);
    void setTexture(string file) {texture = file;}
    virtual void fromFile(string file)=0;
};

class Figure: public Component {
	vector<Point3D> triangles;
public:
	void draw();
	void fromFile(string file);
};

class FigureVBO: public Component {
protected:
	GLuint index;
	GLuint *indices;
	GLuint nIndices;
public:
	void draw();
	void fromFile(string file);
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

class TimeTranslation : public GTransformation {
	vector<point3D> pointVector;
	float time;
	float elapseBefore;
public:
	TimeTranslation(float);
	void calculateTransformation(float, float*, int*,bool derivate);
	void doTransformation();
	void appendPoint(point3D);
	float giveIndex(int, int, int*);
	void normalizeVector(float *res);
	void crossProduct(float *vecR, float *vec1, float *vec2);
};

class Rotation : public GTransformation {
protected:
	point3D p;
	float angle;
public:
	Rotation(float alfa, float x, float y, float z);
	void doTransformation();
};

class TimeRotation : public Rotation {
	float time;
	float elapseBefore;
public:
	TimeRotation(float alfa, float time, float x, float y, float z ) ;
	void doTransformation();
};

class Scale : public GTransformation {
	point3D scale;
public:
	Scale(float x, float y, float z);
	void doTransformation();
};


#endif