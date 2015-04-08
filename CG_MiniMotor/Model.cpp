#pragma comment(lib,"glew32.lib")

#include "stdafx.h"
#include "model.h"
#include <iostream>
#include <fstream>
#include "stdio.h"


using namespace std;

/**
* Lê de um ficheiro e guarda os pontos lidos em estruturas Ponto3D.
*
* @param filename		nome do ficheiro a ler
*/
void Figure::fromFile(string filename){
	ifstream ifs; ifs.open(filename);
	int nPoints, nIndice, i; Point3D point;
	GLuint indice;
	
	ifs >> nPoints >> nIndice;
	float *triangles1 = new float[nPoints * 3];


	i = 0;
	while (!ifs.eof()
		&& i < nPoints
		&& ifs >> point.x >> point.y >> point.z
		) {
		triangles1[i * 3 + 0] = point.x;
		triangles1[i * 3 + 1] = point.y;
		triangles1[i * 3 + 2] = point.z;
		i++;
	}

	i = 0;
	while (!ifs.eof()
		&& i < nIndice
		&& ifs >> indice
		) {
			triangles.push_back({ triangles1[indice*3], triangles1[indice*3 +1], triangles1[indice*3 +2] });
		i++;
	}

	delete(triangles1);
}

/**
* Desenha a figura através das coordenadas dos seus pontos
*/
void Figure::draw(){
	
	glBegin(GL_TRIANGLES);
	std::vector<point3D>::iterator d = triangles.begin();
	glColor3f(0, 0, 255);
	while (d != triangles.end()){
		glVertex3f(d->x, d->y, d->z);
		d++;
		glVertex3f(d->x, d->y, d->z);
		d++;
		glVertex3f(d->x, d->y, d->z);
		d++;
	}
	glEnd();
}


void FigureVBO::fromFile(string filename) {
	ifstream ifs; ifs.open(filename);
	int nPoints, nIndice, i; Point3D point;
	
	ifs >> nPoints >> nIndice;
	this->nIndices = nIndice;
	this->indices = new GLuint[nIndice];
	float *triangles = new float[nPoints * 3];

	i = 0;
	while (!ifs.eof()		
		&& i < nPoints
		&& ifs >> point.x >> point.y >> point.z
	) {
		triangles[i * 3 + 0] = point.x;
		triangles[i * 3 + 1] = point.y;
		triangles[i * 3 + 2] = point.z;
		i++;
	}
	
	i = 0;
	while (!ifs.eof()
		&& i < nIndice
		&& ifs >> this->indices[i]
		) {
		i++;
	}
		
	glGenBuffers(1, &this->index);

	glBindBuffer(GL_ARRAY_BUFFER, this->index);
	glBufferData(GL_ARRAY_BUFFER, nPoints * 3 * sizeof(float), triangles, GL_STATIC_DRAW);
	
	delete(triangles);
}

void FigureVBO::draw() {
	glBindBuffer(GL_ARRAY_BUFFER, this->index);
	glVertexPointer(3, GL_FLOAT, 0, 0);
	glDrawElements(GL_TRIANGLES, this->nIndices, GL_UNSIGNED_INT, this->indices);
}

void Group::appendAction(Action* element){
	actions.push_back(element);
}

void Group::append(Drawable* figure) {
	elements.push_back(figure);
}

void Group::reset() {
	elements.clear();
	actions.clear();
}

void Group::draw(){
	glPushMatrix();
	std::vector<Action*>::iterator it;
	for (it = actions.begin(); it < actions.end(); it++)
		(**it).doAction();

	std::vector<Drawable*>::iterator d = elements.begin();
	while (d != elements.end()){
		(*d)->draw();
		d++;
	}
	glPopMatrix();
}

const int Scene::DRAWMODE_DIRECT = 1;
const int Scene::DRAWMODE_VBO = 2;

void Scene::setDrawMode(int mode){
	this->drawMode = mode;
}

/*
*Implementa um parser de leitura de um ficheiro XML
*
*@param root	raiz do documento
*/
void Scene::parseXML(XMLNode* root, Group* current){
	XMLNode* child;
	Figure f;
	float x, y, z;
	float angulo, eixoX, eixoY, eixoZ;

	for (child = root->FirstChild(); child; child = child->NextSibling()) {
		XMLElement *elem = child->ToElement();
		string tag = child->Value();

		// more than one model
		if (tag.compare("modelos") == 0){
			XMLNode* modelo;
			for (modelo = child->FirstChild(); modelo; modelo = modelo->NextSibling()) {
				if (tag.compare("modelo") == 0) {
					if (elem->Attribute("ficheiro")){
						if (this->drawMode == Scene::DRAWMODE_VBO){
							FigureVBO* ff = new FigureVBO();
							ff->fromFile(elem->Attribute("ficheiro"));
							current->append(ff);
						}
						else{
							Figure* ff = new Figure();
							ff->fromFile(elem->Attribute("ficheiro"));
							current->append(ff);
						}
						// be carefull f need to destroyed and recreated, they are doing pushback
					}
				}
			}
		}
		// only one model 
		if (tag.compare("modelo") == 0) {
			if (elem->Attribute("ficheiro")){
				if (this->drawMode == Scene::DRAWMODE_VBO){
					FigureVBO* ff = new FigureVBO();
					ff->fromFile(elem->Attribute("ficheiro"));
					current->append(ff);
				}
				else{
					Figure* ff = new Figure();
					ff->fromFile(elem->Attribute("ficheiro"));
					current->append(ff);
				}
			}
		}
		else if (tag.compare("grupo") == 0) {
			// glPushMatrix();
			// guardar, nao desenhar..
			Group *g = new Group;
			parseXML(child, g);
			current->append(g);
			// glPopMatrix();
		}
		else if (tag.compare("rotacao") == 0) {
			eixoX = eixoY = eixoZ = 0.0; angulo = 0.0;

			if (elem->Attribute("angulo")) angulo = elem->FloatAttribute("angulo");
			if (elem->Attribute("eixoX")) eixoX = elem->FloatAttribute("eixoX");
			if (elem->Attribute("eixoY")) eixoY = elem->FloatAttribute("eixoY");
			if (elem->Attribute("eixoZ")) eixoZ = elem->FloatAttribute("eixoZ");

			current->appendAction(new Rotation(angulo, eixoX, eixoY, eixoZ ));
	 	}
		else if (tag.compare("translacao") == 0){
			x = y = z = 0.0;
			if (elem->Attribute("X")) x = elem->FloatAttribute("X");
			if (elem->Attribute("Y")) y = elem->FloatAttribute("Y");
			if (elem->Attribute("Z")) z = elem->FloatAttribute("Z");
			current->appendAction(new Translation(x,y,z));
		}
		else if (tag.compare("escala") == 0){
			x = y = z = 1.0;
			if (elem->Attribute("X")) x = elem->FloatAttribute("X");
			if (elem->Attribute("Y")) y = elem->FloatAttribute("Y");
			if (elem->Attribute("Z")) z = elem->FloatAttribute("Z");

			current->appendAction(new Scale(x,y,z));
		}
	}
}

Translation::Translation(float x, float y, float z){
	this->transVector.x = x;
	this->transVector.y = y;
	this->transVector.z = z;
}

void Translation::doAction(){
	glTranslatef(this->transVector.x, this->transVector.y, transVector.z);
}

Rotation::Rotation(float angle, float x, float y, float z){
	this->angle = angle;
	this->p.x = x;
	this->p.y = y;
	this->p.z = z;
}
void Rotation::doAction(){
	glRotatef(angle, p.x, p.y, p.z);
}
Scale::Scale(float x, float y, float z){
	this->scale.x = x;
	this->scale.y = y;
	this->scale.z = z;

}
void Scale::doAction(){
	glScalef(scale.x, scale.y, scale.z);
}



/** NAO APAGAR!!! **/
/*Figure genericDraw(Point3D points[], int camadas, int fatias){
	int i, j;
	float angulo_circ = 2 * M_PI / fatias; //angulo para calcular o tamanho de cada camada
	float circ_aux1, circ_aux2 = 0; // angulos auxiliares de fatias
	Figure f;

	for (i = 0; i<fatias; i++){
		circ_aux1 = circ_aux2;
		circ_aux2 += angulo_circ;

		for (j = 0; j < camadas - 1; j++) {

			f.triangles.push_back({ points[j].x * sin(circ_aux1), points[j].y, points[j].x * cos(circ_aux1) });
			f.triangles.push_back({ points[j + 1].x * sin(circ_aux1), points[j + 1].y, points[j + 1].x * cos(circ_aux1) });
			f.triangles.push_back({ points[j].x * sin(circ_aux2), points[j].y, points[j].x  * cos(circ_aux2) });

			f.triangles.push_back({ points[j + 1].x * sin(circ_aux1), points[j + 1].y, points[j + 1].x * cos(circ_aux1) });
			f.triangles.push_back({ points[j + 1].x * sin(circ_aux2), points[j + 1].y, points[j + 1].x * cos(circ_aux2) });
			f.triangles.push_back({ points[j].x * sin(circ_aux2), points[j].y, points[j].x * cos(circ_aux2) });
		}
	}
	return f;
}
*/