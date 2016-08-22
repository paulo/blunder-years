#include "stdafx.h"
#include "FigureFactory.h"



Point3D cp[4][4]; 

Point3D calcNormal(float* v1,float* v2) {
	Point3D normal;
	double mag, x, y, z;

	x = v1[1] * v2[2] - v1[2] * v2[1];
	y = v1[2] * v2[0] - v1[0] * v2[2];
	z = v1[0] * v2[1] - v1[1] * v2[0];

	mag = sqrt(pow(x, 2) + pow(y, 2) + pow(z, 2));
	if (mag != 0){
		normal.x = x / mag;
		normal.y = y / mag;
		normal.z = z / mag;
	}
	else {
		// division by zero, this is not corret but?
		normal.x = normal.z = 0;
		normal.y = 1;
	}
	return normal;
}

void CalcVM(float* x, float m[], int mSize){
	float res[4];

	for (int i = 0; i < mSize; i++){
		res[i] = x[0] * m[i*4] + x[1] * m[i*4+1] + x[2] * m[i*4+2] + x[3] * m[i*4+3];
	}
	for (int i = 0; i < mSize; i++){
		x[i] = res[i];
	}
}

Point3D CalcDerivade(float u, float v, float p[]){
	float m[16] = {
		-1, 3, -3, 1,
		3, -6, 3, 0,
		-3, 3, 0, 0,
		1, 0, 0, 0 
	};
	float uuu[] = { u*u*u, u*u, u, 1 };
	float uu[] = { 3 * u * u, 2 * u, 1, 0 };
	float vvv[] = { v * v * v, v * v, v, 1 };
	float vv[] = { 3 * v * v, 2 * v, 1, 0 };
	float aux[4];
	float resA[3], resB[3];

	for (int i = 0; i < 3; i++) {
		aux[0] = uu[0]; aux[1] = uu[1]; aux[2] = uu[2]; aux[3] = uu[3];
		CalcVM(aux, m, 4);
		CalcVM(aux, p + i * 16, 4);
		CalcVM(aux, m, 4);
		CalcVM(aux, vvv, 1);
		resA[i] = aux[0];

		aux[0] = uuu[0]; aux[1] = uuu[1]; aux[2] = uuu[2]; aux[3] = uuu[3];
		CalcVM(aux, m, 4);
		CalcVM(aux, p + i * 16, 4);
		CalcVM(aux, m, 4);
		CalcVM(aux, vv, 1);
		resB[i] = aux[0];
	}
	return calcNormal(resA, resB);
}

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
	float m[16 * 3];
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
			m[i + 00] = cpoint.x;
			m[i + 16] = cpoint.y;
			m[i + 32] = cpoint.z;
		}

		for (i = 0; i < tess; i++) {
			for (j = 0; j < tess; j++) {

				u = (float)i / tm;
				v = (float)j / tm;

				bzr[0] = CalcU(u, 0);
				bzr[1] = CalcU(u, 1);
				bzr[2] = CalcU(u, 2);
				bzr[3] = CalcU(u, 3);
				Point3D p;
				p = CalculateSurfacePoint(v, bzr);
				f.appendPoint(p);
				f.appendPointTexture({ (u -1) * -1, (v -1)*-1, 0 });
				
				p = CalcDerivade(u,v,m);
				f.appendNormal(p);
			}
		}
	}
	
	int offi = 0;

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



