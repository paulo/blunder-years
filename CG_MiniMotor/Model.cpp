#include "stdafx.h"
#include "model.h"
#include <glut.h>
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
	ifstream ifs;
	ifs.open(filename);
	Point3D point;
	string line;
	//float p1, p2, p3;
	while (!ifs.eof()) {
		if (ifs >> point.x >> point.y >> point.z){
			triangles.push_back(point);
		}
	}
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


void Scene::append(Figure figure) {
	figures.push_back(figure);
}

void Scene::draw(){
	std::vector<Figure>::iterator d = figures.begin();
	glColor3f(0, 0, 255);
	while (d != figures.end()){
		d->draw();
		d++;
	}
}

/*
*Implementa um parser de leitura de um ficheiro XML
*
*@param root	raiz do documento
*/
void Scene::parseXML(XMLNode* root){

	XMLNode* child;
	const XMLAttribute* atr;
	string tag;
	XMLElement *elem;
	Figure f;
	float x, y, z;
	float angulo, eixoX, eixoY, eixoZ;
	for (child = root->FirstChild(); child; child = child->NextSibling()) {
		elem = child->ToElement();
		tag = child->Value();

		if (tag.compare("modelo") == 0) {
			if (elem->Attribute("ficheiro")){
				f.fromFile(elem->Attribute("ficheiro"));
				append(f);
				//f.draw();
			}
		}
		else if (tag.compare("grupo") == 0) {
			// glPushMatrix();
			// guardar, nao desenhar..
			parseXML(child);
			// glPopMatrix();
		}
		else if (tag.compare("rotacao") == 0) {
			eixoX = eixoY = eixoZ = 0.0; angulo = 0.0;

			if (elem->Attribute("angulo")) angulo = elem->FloatAttribute("angulo");
			if (elem->Attribute("eixoX")) eixoX = elem->FloatAttribute("angulo");
			if (elem->Attribute("eixoY")) eixoY = elem->FloatAttribute("angulo");
			if (elem->Attribute("eixoZ")) eixoZ = elem->FloatAttribute("eixoZ");

			glRotatef(angulo, eixoX, eixoY, eixoZ);
	 	}
		else if (tag.compare("translação") == 0){
			x = y = z = 0.0;
			if (elem->Attribute("X")) x = elem->FloatAttribute("X");
			if (elem->Attribute("Y")) y = elem->FloatAttribute("Y");
			if (elem->Attribute("Z")) z = elem->FloatAttribute("Z");
			
			glTranslatef(x, y, z);
		}
		else if (tag.compare("escala") == 0){
			x = y = z = 1.0;
			if (elem->Attribute("X")) x = elem->FloatAttribute("X");
			if (elem->Attribute("Y")) y = elem->FloatAttribute("Y");
			if (elem->Attribute("Z")) z = elem->FloatAttribute("Z");

			glScalef(x, y, z);
		}
	}
}