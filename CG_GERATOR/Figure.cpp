#include "stdafx.h"
#include "model.h"
#include <iostream>
#include <fstream>

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

/**
*Adiciona um ponto ao conjunto de pontos de uma figura.
*
*/
void Figure::append(Point3D p){
	triangles.push_back(p);
}