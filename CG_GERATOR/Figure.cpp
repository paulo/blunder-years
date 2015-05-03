#include "stdafx.h"
#include "model.h"

/**
* Escreve num ficheiro todas coordenadas dos pontos de uma figura.
*
* @param filename		nome do ficheiro a escrever
*/
void Figure::toFile(string filename){
	ofstream ofs;
	ofs.open(filename);
	for (std::vector<Point3D>::iterator d = triangles.begin(); d != triangles.end(); ++d){
		ofs << d->x << ' ' << d->y << ' ' << d->z << endl;
	}
}

void Figure::toFileVBO(string filename){
	ofstream ofs;
	ofs.open(filename);
	ofs << triangles.size() << ' ' << indices.size() << endl;
	std::vector<Point3D>::iterator n = normal.begin();
	std::vector<Point3D>::iterator d = triangles.begin();
	for (; d != triangles.end() && n != normal.end(); ++d, n++){
		ofs << d->x << ' ' << d->y << ' ' << d->z << ' ' << n->x << ' ' << n->y << ' ' << n->z << endl;
	}
	for (std::vector<unsigned int>::iterator d = indices.begin(); d != indices.end(); ++d){
		ofs << *d << endl;
	}
}

unsigned int Figure::appendIndice(unsigned int p){
	indices.push_back(p);
	return p;
}

int Figure::appendPoint(Point3D p){
	triangles.push_back(p);
	return triangles.size() - 1;
}

void Figure::appendNormal(Point3D p){
	normal.push_back(p);
}
/**
*Adiciona um ponto ao conjunto de pontos de uma figura.
*
*/
void Figure::append(Point3D p){
	triangles.push_back(p);
	indices.push_back(triangles.size() - 1);
}

vector<unsigned int> Figure::getIndexes(){
	return indices;
}

vector<Point3D>* Figure::getPoints(){
	return &triangles;
}
