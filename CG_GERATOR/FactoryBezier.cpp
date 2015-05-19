#include "stdafx.h"
#include "FigureFactory.h"


Point3D cp[4][4]; 

/*
* Função auxiliar da função definePatches()
*/
Point3D CalcU(float t, int linha) {

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
Point3D CalculateSurfacePoint(float t, Point3D* pnts) {

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

float calcDist(Point3D pA, Point3D pB) {
	float dist, diffX, diffY, diffZ;

	diffX = pB.x - pA.x; 
	diffY = pB.y - pA.y; 
	diffZ = pB.z - pA.z;
	
	dist = pow(diffX, 2) + pow(diffY, 2) + pow(diffZ, 2);
	dist = sqrt(dist);

	return dist;
}

Point3D calcVector(Point3D p1, Point3D p2) {
	Point3D vector;
	
	vector.x = p1.x - p2.x;
	vector.y = p1.y - p2.y;
	vector.z = p1.z - p2.z;

	return vector;
}

Point3D calcNormal(Point3D v1, Point3D v2) {
	Point3D normal;
	float mag, x, y, z;
	
	x = v1.y * v2.z - v1.z * v2.y;
	y = v1.z * v2.x - v1.x * v2.z;
	z = v1.x * v2.y - v1.y * v2.x;

	mag = sqrt(pow(x, 2) + pow(y, 2) + pow(z, 2));

	normal.x = x / mag;
	normal.y = y / mag;
	normal.z = z / mag;

	return normal;
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
			cpoint = aux.getPoints()->at(aux.getIndexes()[k * 16 + i] - 1);
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

				bzr[0] = CalcU(u, 0);
				bzr[1] = CalcU(u, 1);
				bzr[2] = CalcU(u, 2);
				bzr[3] = CalcU(u, 3);

				Point3D p = CalculateSurfacePoint(v, bzr);
				f.appendPoint(p);
			}
		}
	}
	int offi = 0;
	float sumX, sumY, sumActualX, sumActualY;
	sumX = sumY = sumActualX = sumActualY = 0;
	for (i = 0; i < in; i++){
		for (j = 0; j < tess - 1; j++) {
			Point3D pA = f.getPoints()->at(j);
			Point3D pB = f.getPoints()->at(j + 1);

			Point3D pC = f.getPoints()->at(j*tess);
			Point3D pD = f.getPoints()->at((j + 1) * tess);
			
			sumX += calcDist(pA, pB);
			sumY += calcDist(pC, pD);
		}
		for (j = 0; j < tess; j++) {

			for (k = 0; k < tess; k++) {
				
				int cimaInd, baixoInd, direitaInd, esquerdaInd, pontoInd, size;
				size = f.getPoints()->size();

				pontoInd = offi + (k*tess) + j;
				cimaInd = offi + (k*tess) + j - tess;
				baixoInd = offi + (k*tess) + j + tess;
				direitaInd = offi + (k*tess) + j - 1;
				esquerdaInd = offi + (k*tess) + j + 1;

				if (cimaInd < 0 || cimaInd >= size) {
					cimaInd = pontoInd;
				}
				if (baixoInd < 0 || baixoInd >= size) {
					baixoInd = pontoInd;
				}
				if (direitaInd < 0 || direitaInd >= size) {
					direitaInd = pontoInd;
				}
				if (esquerdaInd < 0 || esquerdaInd >= size) {
					esquerdaInd = pontoInd;
				}

				Point3D cima = f.getPoints()->at(cimaInd);
				Point3D baixo = f.getPoints()->at(baixoInd);
				Point3D direita = f.getPoints()->at(direitaInd);
				Point3D esquerda = f.getPoints()->at(esquerdaInd);
				
				Point3D v1 = calcVector(baixo, cima);
				Point3D v2 = calcVector(direita, esquerda);

				Point3D norm = calcNormal(v1, v2);
				f.appendNormal(norm);

				Point3D texPoint;
				texPoint.x = sumActualX / sumX;
				texPoint.y = 0;
				texPoint.z = sumActualY / sumY;
				f.appendPointTexture(texPoint);

				if (k+1 < tess) {
					Point3D pA = f.getPoints()->at(k);
					Point3D pB = f.getPoints()->at(k + 1);
					sumActualY += calcDist(pA, pB);
				}				
			}
			sumActualY = 0;
			
			if (j + 1 < tess) {

				Point3D pC = f.getPoints()->at(j*tess);
				Point3D pD = f.getPoints()->at((j + 1) * tess);
				sumActualX += calcDist(pC, pD);
			}
		}
		offi += tess * tess;
	}

	offi = 0;

	for (i = 0; i < in; i++){
		for (j = 0; j < tess - 1; j++) {
			for (k = 0; k < tess -1; k++) {

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



