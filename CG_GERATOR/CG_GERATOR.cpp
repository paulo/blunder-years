// CG_GERATOR.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include "FigureFactory.h"

using namespace std;

static FigureFactory figureFactory;

/**
*Faz parse do comando recebido e chama as respetivas funções
*para geração de figuras
*
* @return
*/
int main(int argc, char* argv[]){

	string inputLine;
	getline(cin, inputLine);
	istringstream ss(inputLine);
	string token, filename, args[7], form;
	int i;

	//gerador esfera 2.5 5 5 esfera.3d
	if (argc == 0){
		for (i = 0; getline(ss, token, ' ') && i < 7; i++) {
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

		if (args[0] != "gerador" || i < 2) {
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
					<< "O comando deve ser do tipo \"gerador plano 10 15 ficheiro.3d\"." << endl;
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
					<< "O comando deve ser do tipo \"gerador plano 10 15 ficheiro.3d\"." << endl;
			}
		}
	}
	catch (invalid_argument) {
		cout << "Formato invalido dos argumentos inseridos!" << endl;
	}

	f.toFile(filename);
}
