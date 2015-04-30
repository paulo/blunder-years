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
	for (std::vector<Point3D>::iterator d = triangles.begin(); d != triangles.end(); ++d){
		ofs << d->x << ' ' << d->y << ' ' << d->z << endl;
	}
	for (std::vector<unsigned int>::iterator d = indices.begin(); d != indices.end(); ++d){
		ofs << *d << endl;
	}
}

void Figure::appendIndice(unsigned int p){
	indices.push_back(p);
}

int Figure::appendPoint(Point3D p){
	triangles.push_back(p);
	return triangles.size() - 1;
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