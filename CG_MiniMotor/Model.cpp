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

void Group::appendTransformation(GTransformation* element){
	transformations.push_back(element);
}

void Group::append(Drawable* figure) {
	elements.push_back(figure);
}

void Group::reset() {
	elements.clear();
	transformations.clear();
}

void Group::draw(){
	glPushMatrix();
	std::vector<GTransformation*>::iterator it;
	for (it = transformations.begin(); it < transformations.end(); it++)
		(**it).doTransformation();

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

void Scene::setCameraPosition(float x, float y, float z){
	this->camX = x;
	this->camY = y;
	this->camZ = z;
}

Point3D Scene::getCameraPosition(){
	return{ camX, camY, camZ };
}

/*
*Implementa um parser de leitura de um ficheiro XML
*
*@param root	raiz do documento
*/
int Scene::parseXML(XMLNode* root, Group* current){
	XMLNode* child;
	Figure f;
	float x, y, z;
	float tempo, angulo, eixoX, eixoY, eixoZ;
	int rt = 0, timeRt=0, tr = 0, sc = 0, mdls = 0, grp = 0;
	int ok = 0;

	for (child = root->FirstChild(); child; child = child->NextSibling()) {
		XMLElement *elem = child->ToElement();
		string tag = child->Value();

		// more than one model
		if (tag.compare("modelos") == 0 && mdls == 0) {
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
							append(ff);
							mdls = 1;
							current->append(ff);
						}
						// be carefull f need to destroyed and recreated, they are doing pushback
					}
					
				}
			}
		}
		if (tag.compare("camera") == 0){
			x = y = z = 50;
			if (elem->Attribute("X")) x = elem->FloatAttribute("X");
			if (elem->Attribute("Y")) y = elem->FloatAttribute("Y");
			if (elem->Attribute("Z")) z = elem->FloatAttribute("Z");
			setCameraPosition(x, y, z);
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
		else 
			if (tag.compare("grupo") == 0) {
			// glPushMatrix();
			// guardar, nao desenhar..
			Group *g = new Group;
			if (( parseXML(child, g)) == -1){
				return -1;
			}
			current->append(g);
			grp = true;
			// glPopMatrix();
		}
		else if (tag.compare("rotacao") == 0 ) {
			if (rt == 0 && mdls == 0 && grp == 0 && timeRt==0){
				eixoX = eixoY = eixoZ = 0.0; angulo = 0.0;
				tempo = 0.0;

				if (elem->Attribute("angulo")) angulo = elem->FloatAttribute("angulo");
				if (elem->Attribute("eixoX")) eixoX = elem->FloatAttribute("eixoX");
				if (elem->Attribute("eixoY")) eixoY = elem->FloatAttribute("eixoY");
				if (elem->Attribute("eixoZ")) eixoZ = elem->FloatAttribute("eixoZ");
				if (elem->Attribute("tempo")) tempo = elem->FloatAttribute("tempo");

				if (tempo == 0.0){
					current->appendTransformation(new Rotation(angulo, eixoX, eixoY, eixoZ));
					rt = 1;
				} else {
					current->appendTransformation(new TimeRotation(angulo, tempo, eixoX, eixoY, eixoZ));
					timeRt = 1;
				}
				
			}
			else return -1;
	 	}
		else if (tag.compare("translacao") == 0){
			if (tr == 0 && mdls == 0 && grp == 0){
				x = y = z = 0.0;
				tempo = 0.0;
				XMLNode* pointchild;	
				if (elem->Attribute("X")) x = elem->FloatAttribute("X");
				if (elem->Attribute("Y")) y = elem->FloatAttribute("Y");
				if (elem->Attribute("Z")) z = elem->FloatAttribute("Z");
				if (elem->Attribute("tempo")) tempo = elem->FloatAttribute("tempo");
				if (!elem -> FirstChild()){ 
					current->appendTransformation(new Translation(x, y, z));				
				} else {
					TimeTranslation *tt = new TimeTranslation(tempo);
					for (pointchild = child->FirstChild(); pointchild; pointchild = pointchild->NextSibling()) {
						XMLElement *point = pointchild->ToElement();
						string tag2 = pointchild->Value();
						if (tag2.compare("ponto") == 0) {
							if (point->Attribute("X")) x = point->FloatAttribute("X"); 
							if (point->Attribute("Y")) y = point->FloatAttribute("Y");
							if (point->Attribute("Z")) z = point->FloatAttribute("Z");
							tt->appendPoint({x,y,z});
						}		
					}
					current->appendTransformation(tt);
				}
					tr = 1;
				}
				else return -1;
		}
		else if (tag.compare("escala") == 0 ){
			if (sc == 0 && mdls == 0 && grp == 0){
				x = y = z = 1.0;
				if (elem->Attribute("X")) x = elem->FloatAttribute("X");
				if (elem->Attribute("Y")) y = elem->FloatAttribute("Y");
				if (elem->Attribute("Z")) z = elem->FloatAttribute("Z");

				current->appendTransformation(new Scale(x, y, z));
				sc = 1;
			}
			else return -1;	
		}
	}
	return ok;
 }

Translation::Translation(float x, float y, float z){
	this->transVector.x = x;
	this->transVector.y = y;
	this->transVector.z = z;
}

void Translation::doTransformation(){
	glTranslatef(this->transVector.x, this->transVector.y, transVector.z);
}

TimeTranslation::TimeTranslation(float time) 
	: elapseBefore(0.0) {
	this->time=time;
}

void TimeTranslation::appendPoint(Point3D p3d){
	pointVector.push_back(p3d);
}


//talvez depois implementar a contar com a tensao
void TimeTranslation::doTransformation(){
	float elapsedNow = glutGet(GLUT_ELAPSED_TIME);
	
	float time = this->time * 1000;

	float res[3];
	int point_count = pointVector.size();
	float aux = ((elapsedNow / time) - (int)(elapsedNow / time));
	float aux2 = ((elapsedNow * point_count / time) - (int)(elapsedNow * point_count / time));
	int index = floor(aux * point_count);
	float matrix[16];
	float p[3],d[3],r[3],up[3];
	up[0] = 0;
	up[1] = 1;
	up[2] = 0;

	//int point_count = pointVector.size();
	//float t = deltaTime * point_count;
	//int index = floor(t);
	//t = t - index;

	int indexes[4];
	indexes[0] = (index + point_count-1) % point_count;	
	indexes[1] = (indexes[0]+1) % point_count;
	indexes[2] = (indexes[1]+1) % point_count; 
	indexes[3] = (indexes[2]+1) % point_count;

	// do transformation
	calculateTransformation(aux2, res, indexes,false);
	glTranslatef(res[0], res[1], res[2]);
	// do rotation matrix

	//obter P'(t) e normalizar esse vector
	calculateTransformation(aux2, d, indexes, true);
	normalizeVector(d);

	//obter vector r
	crossProduct(r, d, up);
	normalizeVector(r);

	//obter vector up
	crossProduct(up, r, d);
	normalizeVector(up);

	//calcular matriz a multiplicar(ja transposta)
	matrix[0] = r[0]; matrix[1] = r[1]; matrix[2] = r[2]; matrix[3] = 0.0f;
	matrix[4] = up[0]; matrix[5] = up[1]; matrix[6] = up[2]; matrix[7] = 0.0f;
	matrix[8] = d[0]; matrix[9] = d[1]; matrix[10] = d[2]; matrix[11] = 0.0f;
	matrix[12] = 0; matrix[13] = 0; matrix[14] = 0; matrix[15] = 1;
	//matrix[12] = p[0]; matrix[13] = p[1]; matrix[14] = p[2]; matrix[15] = 1.0f;
	glMultMatrixf(matrix);

    this->elapseBefore = elapsedNow;
}

void TimeTranslation::calculateTransformation(float x, float *res, int *indexes, bool derivate){
	double c1,c2,c3,c4;
	int i;
	float m[4][4] = {{0.0,1.0,0.0,0.0},{-0.5,0.0,0.5,0.0},{1.0,-2.5,2.0,-0.5},{-0.5,1.5,-1.5,0.5}}; 


	for(i=0;i<3;i++){
		c1 =  	      							m[0][1]*giveIndex(1, i, indexes);
		c2 = m[1][0]*giveIndex(0, i, indexes)								     + m[1][2]*giveIndex(2, i, indexes);
		c3 = m[2][0]*giveIndex(0, i, indexes) + m[2][1]*giveIndex(1, i, indexes) + m[2][2]*giveIndex(2, i, indexes) + m[2][3]*giveIndex(3, i, indexes);
		c4 = m[3][0]*giveIndex(0, i, indexes) + m[3][1]*giveIndex(1, i, indexes) + m[3][2]*giveIndex(2, i, indexes) + m[3][3]*giveIndex(3, i, indexes);

		if (derivate){
			res[i] = c4*x*x*3 + c3*x*2 + c2;
		}
		else {
			res[i] = (((c4*x + c3)*x + c2)*x + c1);
		}
	}
}

float TimeTranslation::giveIndex(int index, int point, int*indexes){
	if(point == 0) return pointVector[indexes[index]].x;
		else if(point == 1) return pointVector[indexes[index]].y;
			else return pointVector[indexes[index]].z;
}

void TimeTranslation::normalizeVector(float *res){
	float length = sqrt((res[0] * res[0]) + (res[1] * res[1]) + (res[2] * res[2]));

	for (int i = 0; i<3; i++) res[i] = res[i] / length;
}

void TimeTranslation::crossProduct(float *vecR, float *vec1, float *vec2){
	vecR[0] = vec1[1] * vec2[2] - vec2[1] * vec1[2];
	vecR[1] = vec1[2] * vec2[0] - vec2[2] * vec1[0];
	vecR[2] = vec1[0] * vec2[1] - vec2[0] * vec1[1];

}

Rotation::Rotation(float angle, float x, float y, float z){
	this->angle = angle;
	this->p.x = x;
	this->p.y = y;
	this->p.z = z;
}

void Rotation::doTransformation(){
	glRotatef(angle, p.x, p.y, p.z);
}

TimeRotation::TimeRotation(float angle, float time, float x, float y, float z) 
	: elapseBefore(0.0), Rotation(angle, x, y, z) {
	this->time = time;
}
void TimeRotation::doTransformation(){
	float elapsedNow = glutGet(GLUT_ELAPSED_TIME);
	float deltaTime = elapsedNow - this->elapseBefore;
    float anglePerMili = 360 / (this->time * 1000);
	this->angle = (deltaTime * anglePerMili + this->angle);
	while(this->angle > 360){
		this->angle -= 360;
	}

	glRotatef(angle, p.x, p.y, p.z);
    elapseBefore = elapsedNow;
}

Scale::Scale(float x, float y, float z){
	this->scale.x = x;
	this->scale.y = y;
	this->scale.z = z;

}


void Scale::doTransformation(){
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