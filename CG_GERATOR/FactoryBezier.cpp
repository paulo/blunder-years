#include "stdafx.h"
#include "FigureFactory.h"


Point3D cp[4][4]; 

/*
* Função auxiliar da função definePatches()
*/
Point3D CalculateU(float t, int linha) {

	Point3D p;

	float it = 1.0f - t;

	float m0 = it*it*it;
	float m1 = 3*t*it*it;
	float m2 = 3*t*t*it;
	float m3 = t*t*t;

	p.x = m0*cp[linha][0].x + m1*cp[linha][1].x + m2*cp[linha][2].x + m3*cp[linha][3].x;
	p.y = m0*cp[linha][0].y + m1*cp[linha][1].y + m2*cp[linha][2].y + m3*cp[linha][3].y;
	p.z = m0*cp[linha][0].z + m1*cp[linha][1].z + m2*cp[linha][2].z + m3*cp[linha][3].z;

	return p;
}


/*
* Função auxiliar da função definePatches()
*/
Point3D CalculateV(float t, Point3D* pnts) {

	Point3D p;

	float it = 1.0f - t;

	float m0 = it*it*it;
	float m1 = 3*t*it*it;
	float m2 = 3*t*t*it;
	float m3 = t*t*t;

	p.x = m0*pnts[0].x + m1*pnts[1].x + m2*pnts[2].x + m3*pnts[3].x;
	p.y = m0*pnts[0].y + m1*pnts[1].y + m2*pnts[2].y + m3*pnts[3].y;
	p.z = m0*pnts[0].z + m1*pnts[1].z + m2*pnts[2].z + m3*pnts[3].z;

	return p;
}

/*
* Para cada patch, calcula os pontos da superfície de Bézier a partir dos pontos de controlo
*@param aux		figura com os índices e os pontos recolhidos do ficheiro
*@param tess	grau de tesselação
*@param in		número de patches do ficheiro *.patch
*@param pn		número de pontos do ficheiro *.patch
*@return		figura com índices e os pontos a desenhar
*/
Figure definePatches(Figure aux, int tess, int in, int pn){

	Figure f;
	int i, j, k, row, col, tm = tess - 1;
	float u, v;
	Point3D cpoint;
	Point3D bzr[4];

	for (k = 0; k < in; k++){
		for (i = 0; i < 16; i++){
			cpoint = aux.getPoints()[aux.getIndexes()[k*16 + i]];
			row = (int)i / 4;
			col = i - 4 * row;
			cp[row][col].x = cpoint.x;
			cp[row][col].y = cpoint.y;
			cp[row][col].z = cpoint.z;
		}

		for (i = 0; i < tess; i++) {
			for (j = 0; j < tess; j++) {

				u = (float)i / tm;
				v = (float)j / tm;

				bzr[0] = CalculateU(u, 0);
				bzr[1] = CalculateU(u, 1);
				bzr[2] = CalculateU(u, 2);
				bzr[3] = CalculateU(u, 3);

				Point3D p = CalculateV(v, bzr);
				f.appendPoint(p);
			}
		}
	}
	int offi = 0;

	for (i = 0; i < in; i++){
		for (j = 0; j < tess - 1; j++) {
			for (k = 0; k < tess - 1; k++) {

				f.appendIndice(offi + (k*tess) + j + 1);
				f.appendIndice(offi + (k*tess) + j);
				f.appendIndice(offi + (k*tess) + j + tess);

				f.appendIndice(offi + (k*tess) + j + 1);
				f.appendIndice(offi + (k*tess) + j + tess);
				f.appendIndice(offi + (k*tess) + j + tess + 1);
			}
		}
		offi += tess * tess;
	}

	return f;
}

/*
*Analisa um ficheiro *.patch e processa a informação recolhida de modo a gerar os pontos para formar superficies de Bézier
*
*@param patchfile	nome do ficheiro	
*@param f			figura com os índices e os pontos lidos do ficheiro
*@param tess		grau de tesselação
*@return			figura com índices e os pontos a desenhar
*/
Figure FigureFactory::createBezierSurface(Figure* f, std::string patchfile, int tess) {

	Figure aux;
	int i, j, in, pn, ind;
	float coord, pnts[3];
	string line, field;

	Point3D point;

	ifstream filename(patchfile, ios::in);

	if (filename.is_open()) {

		filename >> in;
		getline(filename, line);

		for (i = 0; i < in && getline(filename, line); i++) {
			stringstream ss(line);
			for (j = 0; j < 16 && getline(ss, field, ','); j++) {
				stringstream fs(field);
				ind = 0;
				fs >> ind;
				aux.appendIndice(ind);

			}
		}

		filename >> pn;
		getline(filename, line);

		for (i = 0; i < pn && getline(filename, line); i++) {
			stringstream ss(line);
			for (j = 0; j < 3 && getline(ss, field, ','); j++) {
				stringstream fs(field);
				coord = 0.0;
				fs >> coord;
				pnts[j] = coord;
			}

			point.x = pnts[0];
			point.y = pnts[1];
			point.z = pnts[2];
			aux.appendPoint(point);
		}

		filename.close();
		*f = definePatches(aux, tess, in, pn);

	}
	else {
		cout << "Erro no ficheiro .patch!" << endl;
	}
		
	return *f;
}



