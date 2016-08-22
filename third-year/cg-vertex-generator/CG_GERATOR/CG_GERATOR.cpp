// CG_GERATOR.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include "FigureFactory.h"

static FigureFactory figureFactory;

/**
*Faz parse do comando recebido e chama as respetivas funções
*para geração de figuras
*
* @return
*/
int main(int argc, char* argv[]){

	string filename, args[12], form, patchFile;
	int i, tess;

	//gerador esfera 2.5 5 5 esfera.3d
	if (argc <= 100){
		string inputLine,token;
		getline(cin, inputLine);
		istringstream ss(inputLine);

		for (i = 0; getline(ss, token, ' ') && i < 12; i++) {
			args[i] = token;
		}
	}
	else {
		for (i = 0; i < argc; i++) {
			args[i] = argv[i];
		}
	}

	/*if (args[0] == "a"){
	args[0] = "gerador";
	args[1].assign("cubo");
	args[2].assign("10");
	args[3].assign("ficheiro");
	i = 4;
	}
	if (args[0] == "b"){
	Figure f;
	f.fromFile("ficheiro");
	return 1;
	}*/

	Figure f;
	try{

		if (i < 2) {
			cout << "O comando \"" << args[0] << "\" nao e reconhecido!" << endl
				<< "O comando deve ser do tipo \"gerador esfera 5 10 10 ficheiro.3d\"." << endl;
		}

		else if (args[1] == "esfera") {

			if (i == 6) {

				float rad = stof(args[2]);
				int slices = stoi(args[3]);
				int stacks = stoi(args[4]);
				filename = args[5];
				f = figureFactory.createSphere(rad, slices, stacks);
			}
			else {
				cout << "O número de argumentos do comando introduzido está incorreto!" << endl
					<< "O comando deve ser do tipo \"gerador esfera 5 10 10 ficheiro.3d\"." << endl;
			}
		}

		else if (args[1] == "cubo") {

			if (i == 4) {

				float size = stof(args[2]);
				filename = args[3];
				f = figureFactory.createCube(size);
			}
			else {
				cout << "O número de argumentos do comando introduzido está incorreto!" << endl
					<< "O comando deve ser do tipo \"gerador cubo 4 ficheiro.3d\"." << endl;
			}
		}
		else if (args[1] == "cone") {

			if (i == 7){

				float rad = stof(args[2]);
				float height = stof(args[3]);
				int slices = stoi(args[4]);
				int stacks = stoi(args[5]);
				filename = args[6];

				f = figureFactory.createCone(rad,height, slices, stacks);
			}
			else {
				cout << "O número de argumentos do comando introduzido está incorreto!" << endl
					<< "O comando deve ser do tipo \"gerador cone 4 6 10 10 ficheiro.3d\"." << endl;
			}
		}
		else if (args[1] == "anel") {

			if (i == 7) {

				float innerRad, outerRad;
				if ((innerRad = stof(args[2])) > (outerRad = stof(args[3]))) {
					float temp = innerRad;
					innerRad = outerRad;
					outerRad = temp;
				}
				int nSides = stoi(args[4]);
				int nRings = stoi(args[5]);
				filename = args[6];

				f = figureFactory.createTunnel(innerRad, outerRad, nSides, nRings);
			}
			else {
				cout << "O número de argumentos do comando introduzido está incorreto!" << endl
					<< "O comando deve ser do tipo \"gerador anel 4 6 10 10 ficheiro.3d\"." << endl;
			}
		}
		else if (args[1] == "paralelipipedo") {

			if (i == 6) {

				float height = stof(args[2]);
				float length = stof(args[3]);
				float width = stof(args[4]);
				filename = args[5];

				f = figureFactory.createParallelepiped(width, height, length);
			}
			else {
				cout << "O número de argumentos do comando introduzido está incorreto!" << endl
					<< "O comando deve ser do tipo \"gerador paralelipipedo 4 6 6 ficheiro.3d\"." << endl;
			}
		}
		else if (args[1] == "plano") {

			if (i == 5) {

				float length = stof(args[2]);
				float width = stof(args[3]);
				filename = args[4];

				f = figureFactory.createPlane(width, length);
			}
			else {
				cout << "O número de argumentos do comando introduzido está incorreto!" << endl
					<< "O comando deve ser do tipo \"gerador plano 10 15 ficheiro.3d\"." << endl;
			}
		}
		else if (args[1] == "cilindro") {

			if (i == 7) {

				float radios = stof(args[2]);
				float length = stof(args[3]);
				int slices = stoi(args[4]);
				int camadas = stoi(args[5]);
				filename = args[6];

				f = figureFactory.createCylinder(radios, length, slices, camadas);
			}
			else {
				cout << "O número de argumentos do comando introduzido está incorreto!" << endl
					<< "O comando deve ser do tipo \"gerador cilindro <raio> <lenght> <fatias> <camadas> ficheiro.3d\"." << endl;
			}
		}
		else if (args[1] == "circulo") {

			if (i == 5) {

				float radios = stof(args[2]);
				int slices = stoi(args[3]);
				filename = args[4];

				f = figureFactory.createCircle(radios, slices);
			}
			else {
				cout << "O número de argumentos do comando introduzido está incorreto!" << endl
					<< "O comando deve ser do tipo \"gerador circulo <raio> <fatias> ficheiro.3d\"." << endl;
			}
		}
		else if (args[1] == "arvore") {

				f = figureFactory.createTree(stof(args[2]), stof(args[3]), stof(args[4]), stof(args[5]), stoi(args[6]), 1);
				filename = args[7];
		}

		else if (args[1] == "patch") {

			if (i == 5){

				patchFile = args[2];
				filename = args[4];
				tess = stoi(args[3]);
				f = figureFactory.createBezierSurface(&f,patchFile, tess);
			}
			else{
				cout << "O número de argumentos do comando introduzido está incorreto!" << endl
					<< "O comando deve ser do tipo \"gerador patch ficheiro.patch <tesselação> ficheiro.3d\"." << endl;
			}
		}
		else if (args[1] == "patchZ") {

			if (i == 5){

				patchFile = args[2];
				filename = args[4];
				tess = stoi(args[3]);
				f = figureFactory.createBezierSurface(&f, patchFile, tess);
				//flit z to y and y to -z
				float aux;
				std::vector<Point3D>* vec = f.getPoints();
				std::vector<Point3D>::iterator it = vec->begin();
				std::vector<Point3D>* vec2 = f.getNormals();
				std::vector<Point3D>::iterator it2 = vec2->begin();
				while (it != vec->end() && it2 != vec2->end()){
					aux = it->y;		it->y = it->z;		it->z = -aux;
					aux = it2->y;	it2->y = it2->z;		it2->z = -aux;
					it++;it2++;
				}
			}
			else{
				cout << "O número de argumentos do comando introduzido está incorreto!" << endl
					<< "O comando deve ser do tipo \"gerador patch ficheiro.patch <tesselação> ficheiro.3d\"." << endl;
			}
		}
		else {
			cout << "A figura " << args[1] << " nao existe!" << endl;
		}
	}
	catch (invalid_argument) {
		cout << "Formato invalido dos argumentos inseridos!" << endl;
	}

	f.toFileVBO(filename);
}


