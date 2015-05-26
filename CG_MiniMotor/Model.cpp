

#pragma comment(lib,"glew32.lib")
#include "stdafx.h"
#include "model.h"
#include <fstream>
#include "stdio.h"


using namespace std;

Component::Component(Scene* a)
	: actualScene(a), texID(-1){

}

void Component::draw(){
	vector<Light>::iterator itEnd = this->actualScene->getLights()->end();
	vector<Light>::iterator it = this->actualScene->getLights()->begin();
	for (; it != itEnd; it++) {
		
		glLightfv(GL_LIGHT0 + it->number, GL_DIFFUSE, this->diff);  
		glLightfv(GL_LIGHT0 + it->number, GL_AMBIENT, this->amb);
		glLightfv(GL_LIGHT0 + it->number, GL_SPECULAR, this->spec);
		glLightfv(GL_LIGHT0 + it->number, GL_EMISSION, this->emit);
	}
}

void Component::loadTexture(string filename){
	ILuint text, w, h;
	unsigned char *imageData;

	ilGenImages(1, &text); // unsigned int ima[...]
	ilBindImage(text);
	ilLoadImage((ILstring)filename.c_str());
	ilConvertImage(IL_RGBA, IL_UNSIGNED_BYTE);
	w = ilGetInteger(IL_IMAGE_WIDTH);
	h = ilGetInteger(IL_IMAGE_HEIGHT);

	imageData = ilGetData();
	// create a texture slot
	glGenTextures(1, &texID);
	// bind the slot
	glBindTexture(GL_TEXTURE_2D, texID);
	// define texture parameters
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	// send texture data to OpenGL
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, imageData);
}

Figure::Figure(Scene* a)
	: Component(a) {
}


/**
* Lê de um ficheiro e guarda os pontos lidos em estruturas Ponto3D.
*
* @param filename		nome do ficheiro a ler
*/
void Figure::fromFile(string filename){
	ifstream ifs; ifs.open(filename);
	int nPoints, nIndice, i; Point3D point, normal, texture;
	GLuint indice;

	ifs >> nPoints >> nIndice;
	float *triangles1 = new float[nPoints * 3];
	float *normals1 = new float[nPoints * 3];
	float *textures1 = new float[nPoints * 2];


	i = 0;
	while (!ifs.eof()
		&& i < nPoints
		&& ifs >> point.x >> point.y >> point.z
		>> normal.x >> normal.y >> normal.z
		>> texture.x >> texture.y
		) {
		triangles1[i * 3 + 0] = point.x;
		triangles1[i * 3 + 1] = point.y;
		triangles1[i * 3 + 2] = point.z;
		
		normals1[i * 3 + 0] = normal.x;
		normals1[i * 3 + 1] = normal.y;
		normals1[i * 3 + 2] = normal.z;

		textures1[i * 2 + 0] = texture.x;
		textures1[i * 2 + 1] = texture.y;

		
		
		i++;
	}

	i = 0;
	while (!ifs.eof()
		&& i < nIndice
		&& ifs >> indice
		) {
		triangles.push_back({ triangles1[indice * 3], triangles1[indice * 3 + 1], triangles1[indice * 3 + 2] });
		normals.push_back({ normals1[indice * 3], normals1[indice * 3 + 1], normals1[indice * 3 + 2] });
		textures.push_back({ textures1[indice * 2], textures1[indice * 2 + 1], 0 });
		i++;
	}

	delete(normals1);
	delete(textures1);
	delete(triangles1);
}

/**
* Desenha a figura através das coordenadas dos seus pontos
*/
void Figure::draw(){
	Component::draw();
	glBegin(GL_TRIANGLES);
	std::vector<point3D>::iterator d = triangles.begin();
	std::vector<point3D>::iterator n = normals.begin();
	std::vector<point3D>::iterator t = textures.begin();

	while (d != triangles.end()){
		glNormal3f(n->x, n->y, n->z); n++;
		glTexCoord2f(t->x, t->y); t++;
		glVertex3f(d->x, d->y, d->z); d++;
		
		glNormal3f(n->x, n->y, n->z); n++;
		glTexCoord2f(t->x, t->y); t++;
		glVertex3f(d->x, d->y, d->z); d++;
		
		glNormal3f(n->x, n->y, n->z); n++;
		glTexCoord2f(t->x, t->y); t++;
		glVertex3f(d->x, d->y, d->z); d++;
	}
	glEnd();
}

FigureVBO::FigureVBO(Scene* a)
	: Component(a) {
}

void FigureVBO::fromFile(string filename) {
	ifstream ifs; ifs.open(filename);
	int nPoints, nIndice, i; Point3D point, normal, texture;

	ifs >> nPoints >> nIndice;
	this->nIndices = nIndice;
	this->indices = new GLuint[nIndice];
	float *triangles = new float[nPoints * 3];
	float *normals = new float[nPoints * 3];
	float *textures = new float[nPoints * 2];
	float *normalsLines = new float[nPoints * 3 * 2];

	i = 0;
	while (!ifs.eof()
		&& i < nPoints
		&& ifs >> point.x >> point.y >> point.z
		>> normal.x >> normal.y >> normal.z
		>> texture.x >> texture.y
		) {
		triangles[i * 3 + 0] = point.x;
		triangles[i * 3 + 1] = point.y;
		triangles[i * 3 + 2] = point.z;

		normals[i * 3 + 0] = normal.x;
		normals[i * 3 + 1] = normal.y;
		normals[i * 3 + 2] = normal.z;

		textures[i * 2 + 0] = texture.x;
		textures[i * 2 + 1] = texture.y;

		normalsLines[i * 3 * 2 + 0] = point.x;
		normalsLines[i * 3 * 2 + 1] = point.y;
		normalsLines[i * 3 * 2 + 2] = point.z;
		normalsLines[i * 3 * 2 + 3] = point.x + normal.x;
		normalsLines[i * 3 * 2 + 4] = point.y + normal.y;
		normalsLines[i * 3 * 2 + 5] = point.z + normal.z;

		i++;
	}

	i = 0;
	while (!ifs.eof()
		&& i < nIndice
		&& ifs >> this->indices[i]
		) {

		i++;
	}

	glGenBuffers(4, this->index);

	glBindBuffer(GL_ARRAY_BUFFER, this->index[0]);
	glBufferData(GL_ARRAY_BUFFER, nPoints * 3 * sizeof(float), triangles, GL_STATIC_DRAW);
	glBindBuffer(GL_ARRAY_BUFFER, this->index[1]);
	glBufferData(GL_ARRAY_BUFFER, nPoints * 3 * sizeof(float), normals, GL_STATIC_DRAW);
	glBindBuffer(GL_ARRAY_BUFFER, this->index[2]);
	glBufferData(GL_ARRAY_BUFFER, nPoints * 3 * 2 * sizeof(float), normalsLines, GL_STATIC_DRAW);
	glBindBuffer(GL_ARRAY_BUFFER, this->index[3]);
	glBufferData(GL_ARRAY_BUFFER, nPoints * 2 * sizeof(float), textures, GL_STATIC_DRAW);

	delete(triangles);
	delete(normals);
	delete(textures);
	delete(normalsLines);
}

void FigureVBO::draw() {
	Component::draw();
	if (texID > 0){
		glBindTexture(GL_TEXTURE_2D, texID);
	}

	glBindBuffer(GL_ARRAY_BUFFER, this->index[0]);
	glVertexPointer(3, GL_FLOAT, 0, 0);
	glBindBuffer(GL_ARRAY_BUFFER, this->index[1]);
	glNormalPointer(GL_FLOAT, 0, 0);
	if (texID > 0){
		glBindBuffer(GL_ARRAY_BUFFER, this->index[3]);
		glTexCoordPointer(2, GL_FLOAT, 0, 0);
		glDrawElements(GL_TRIANGLES, this->nIndices, GL_UNSIGNED_INT, this->indices);
	}

	if (this->actualScene->geDrawNormal()) {
		glBindBuffer(GL_ARRAY_BUFFER, this->index[2]);
		glVertexPointer(3, GL_FLOAT, 0, 0);
		glDrawArrays(GL_LINES, 0, this->nIndices);
	}

	if (texID > 0){
		glBindTexture(GL_TEXTURE_2D, 0);
	}
}

void Group::appendTransformation(GTransformation* element){
	transformations.push_back(element);
}



void Group::append(Drawable* figure) {
	elements.push_back(figure);
}

void Scene::reset() {
	elements.clear();
	transformations.clear();
	lights.clear();
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

void Scene::draw(){
	std::vector<Light>::iterator it;
	for (it = lights.begin(); it < lights.end(); it++){
		glLightfv(GL_LIGHT0 + it->number, it->property, it->posCoords);
	}
	Group::draw();
}

void Scene::appendLight(Light l){
	lights.push_back(l);
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
	float x, y, z;
	float diff[3], amb[3], spec[3], emit[3];
	float tempo, angulo, eixoX, eixoY, eixoZ;
	int rt = 0, timeRt = 0, tr = 0, timeTr = 0, sc = 0, mdls = 0, grp = 0, type;
	int ok = 0, ln = 0, mn = 0;

	for (child = root->FirstChild(); child; child = child->NextSibling()) {
		XMLElement *elem = child->ToElement();
		string tag = child->Value();

		// more than one model
		if (tag.compare("modelos") == 0 && mdls == 0) {
			XMLNode* modelo;
			for (modelo = child->FirstChild(); modelo; modelo = modelo->NextSibling()) {
				if (tag.compare("modelo") == 0) {
					if (elem->Attribute("ficheiro")){
						if (this->drawMode == Scene::DRAWMODE_VBO) {
							FigureVBO* ff = new FigureVBO(this);
							ff->fromFile(elem->Attribute("ficheiro"));
							current->append(ff);
						}
						else{
							Figure* ff = new Figure(this);
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
				////////////////////////
				string file = elem->Attribute("ficheiro");
				string textura = "";
				diff[0] = diff[1] = diff[2] = 1.0;
				amb[0] = amb[1] = amb[2] = 0.0;
				spec[0] = spec[1] = spec[2] = 1.0;
				emit[0] = emit[1] = emit[2] = 0.0;

				if(elem->Attribute("textura")) textura = elem->Attribute("textura"); 
				if(elem->Attribute("diffR")) diff[0] = elem->FloatAttribute("diffR");
				if(elem->Attribute("diffG")) diff[1] = elem->FloatAttribute("diffG"); 
				if(elem->Attribute("diffB")) diff[2] = elem->FloatAttribute("diffB"); 
				if(elem->Attribute("ambR")) amb[0] = elem->FloatAttribute("ambR"); 
				if(elem->Attribute("ambG")) amb[1] = elem->FloatAttribute("ambG"); 
				if(elem->Attribute("ambB")) amb[2] = elem->FloatAttribute("ambB"); 
				if(elem->Attribute("specR")) spec[0] = elem->FloatAttribute("specR"); 
				if(elem->Attribute("specG")) spec[1] = elem->FloatAttribute("specG"); 
				if(elem->Attribute("specB")) spec[2] = elem->FloatAttribute("specB"); 
				if(elem->Attribute("emitR")) emit[0] = elem->FloatAttribute("emitR"); 
				if(elem->Attribute("emitG")) emit[1] = elem->FloatAttribute("emitG"); 
				if(elem->Attribute("emitB")) emit[2] = elem->FloatAttribute("emitB"); 

				Component* ff;
				if (this->drawMode == Scene::DRAWMODE_VBO){
					ff = new FigureVBO(this);
				}
				else{
					ff = new Figure(this);
				}

				ff->fromFile(file);
				ff->setDiff(diff[0], diff[1], diff[2]);
				ff->setAmb(amb[0], amb[1], amb[2]);
				ff->setSpec(spec[0], spec[1], spec[2]);
				ff->setEmit(emit[0], emit[1], emit[2]);
				if (textura != ""){
					ff->loadTexture(textura);
				}
				current->append(ff);

				/*if (this->drawMode == Scene::DRAWMODE_VBO){
					FigureVBO* ff = new FigureVBO();
					ff->fromFile(elem->Attribute("ficheiro"));
					current->append(ff);
				}
				else{
					Figure* ff = new Figure();
					ff->fromFile(elem->Attribute("ficheiro"));
					current->append(ff);
				}*/
			}
		}
		else
			if (tag.compare("grupo") == 0) {
				// glPushMatrix();
				// guardar, nao desenhar..
				Group *g = new Group;
				if ((parseXML(child, g)) == -1){
					return -1;
				}
				current->append(g);
				grp = true;
				// glPopMatrix();
			}
			else if (tag.compare("rotacao") == 0) {
				if ((rt == 0 || timeRt == 0) && mdls == 0 && grp == 0){
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
					}
					else {
						current->appendTransformation(new TimeRotation(angulo, tempo, eixoX, eixoY, eixoZ));
						timeRt = 1;
					}

				}
				else return -1;
			}
			else if (tag.compare("translacao") == 0){
				if ((tr == 0 || timeTr == 0) && mdls == 0 && grp == 0){
					x = y = z = 0.0;
					tempo = 0.0;
					XMLNode* pointchild;
					if (elem->Attribute("X")) x = elem->FloatAttribute("X");
					if (elem->Attribute("Y")) y = elem->FloatAttribute("Y");
					if (elem->Attribute("Z")) z = elem->FloatAttribute("Z");
					if (elem->Attribute("tempo")) tempo = elem->FloatAttribute("tempo");
					if (!elem->FirstChild()) {
						current->appendTransformation(new Translation(x, y, z));
						tr = 1;
					}
					else {
						TimeTranslation *tt = new TimeTranslation(tempo);
						for (pointchild = child->FirstChild(); pointchild; pointchild = pointchild->NextSibling()) {
							XMLElement *point = pointchild->ToElement();
							string tag2 = pointchild->Value();
							x = y = z = 0;
							if (tag2.compare("ponto") == 0) {
								if (point->Attribute("X")) x = point->FloatAttribute("X");
								if (point->Attribute("Y")) y = point->FloatAttribute("Y");
								if (point->Attribute("Z")) z = point->FloatAttribute("Z");
								tt->appendPoint({ x, y, z });
							}
						}
						current->appendTransformation(tt);
						timeTr = 1;
					}
				}
				else return -1;
			}
			else if (tag.compare("escala") == 0){
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
			else if (tag.compare("luzes") == 0){
				glEnable(GL_LIGHTING);
				XMLNode *light;
				for (light = child->FirstChild(), ln = 0; light; light = light->NextSibling(), ln++) 	{
					XMLElement *luz = light->ToElement();
					string tag3 = light->Value();

					if (tag3.compare("luz") == 0) {

						if (strcmp(luz->Attribute("tipo"), "POINT") == 0){
							type = 1.0;
						}
						else if (strcmp(luz->Attribute("tipo"), "DIRECTIONAL") == 0) {
							type = 0.0;
						}
						else {
							//error
						}
						x = y = z = 0;
						if (luz->Attribute("posX")) x = luz->FloatAttribute("posX");
						if (luz->Attribute("posY")) y = luz->FloatAttribute("posY");
						if (luz->Attribute("posZ")) z = luz->FloatAttribute("posZ");

						Light *mlight = new Light(x, y, z, type, ln, GL_POSITION);
						appendLight(*mlight);
						glEnable(GL_LIGHT0 + mlight->number);
					}
				}
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
	this->time = time;
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
	//possible way t = fmod(glutGet(GLUT_ELAPSED_TIME) / (time*1000.0f), 1);
	float aux = ((elapsedNow / time) - (int)(elapsedNow / time));
	float aux2 = ((elapsedNow * point_count / time) - (int)(elapsedNow * point_count / time));
	int index = floor(aux * point_count);
	float matrix[16];
	float p[3], d[3], r[3], up[3];
	up[0] = 0;
	up[1] = 1;
	up[2] = 0;

	//int point_count = pointVector.size();
	//float t = deltaTime * point_count;
	//int index = floor(t);
	//t = t - index;

	int indexes[4];
	indexes[0] = (index + point_count - 1) % point_count;
	indexes[1] = (indexes[0] + 1) % point_count;
	indexes[2] = (indexes[1] + 1) % point_count;
	indexes[3] = (indexes[2] + 1) % point_count;

	// do transformation
	calculateTransformation(aux2, res, indexes, false);
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
	double c1, c2, c3, c4;
	int i;
	float m[4][4] = { { 0.0, 1.0, 0.0, 0.0 }, { -0.5, 0.0, 0.5, 0.0 }, { 1.0, -2.5, 2.0, -0.5 }, { -0.5, 1.5, -1.5, 0.5 } };


	for (i = 0; i < 3; i++){
		c1 = m[0][1] * giveIndex(1, i, indexes);
		c2 = m[1][0] * giveIndex(0, i, indexes) + m[1][2] * giveIndex(2, i, indexes);
		c3 = m[2][0] * giveIndex(0, i, indexes) + m[2][1] * giveIndex(1, i, indexes) + m[2][2] * giveIndex(2, i, indexes) + m[2][3] * giveIndex(3, i, indexes);
		c4 = m[3][0] * giveIndex(0, i, indexes) + m[3][1] * giveIndex(1, i, indexes) + m[3][2] * giveIndex(2, i, indexes) + m[3][3] * giveIndex(3, i, indexes);

		if (derivate){
			res[i] = c4*x*x * 3 + c3*x * 2 + c2;
		}
		else {
			res[i] = (((c4*x + c3)*x + c2)*x + c1);
		}
	}
}

float TimeTranslation::giveIndex(int index, int point, int*indexes){
	if (point == 0) return pointVector[indexes[index]].x;
	else if (point == 1) return pointVector[indexes[index]].y;
	else return pointVector[indexes[index]].z;
}

void TimeTranslation::normalizeVector(float *res){
	float length = sqrt((res[0] * res[0]) + (res[1] * res[1]) + (res[2] * res[2]));

	for (int i = 0; i < 3; i++) res[i] = res[i] / length;
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

Light::Light(float x, float y, float z, float type, int number, int property){
	this->posCoords[0] = x;
	this->posCoords[1] = y;
	this->posCoords[2] = z;
	this->posCoords[3] = type;
	this->number = number;
	this->property = property;
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
	while (this->angle > 360){
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
