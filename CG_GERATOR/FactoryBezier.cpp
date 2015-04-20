#include "stdafx.h"
#include "FigureFactory.h"


Point3D cp[4][4]; //control points structure

Point3D CalculateU(float t, int linha) {

	Point3D p;

	float it = 1.0 - t;
	float m0 = it*it*it;
	float m1 = 3 * t*it*it;
	float m2 = 3 * t*t*it;
	float m3 = t*t*t;

	p.x = m0*cp[linha][0].x + m1*cp[linha][1].x + m2*cp[linha][2].x + m3*cp[linha][3].x;
	p.y = m0*cp[linha][0].y + m1*cp[linha][1].y + m2*cp[linha][2].y + m3*cp[linha][3].y;
	p.z = m0*cp[linha][0].z + m1*cp[linha][1].z + m2*cp[linha][2].z + m3*cp[linha][3].z;

	return p;
}

Point3D CalculateV(float t, Point3D* pnts) {

	Point3D p;

	float it = 1.0f - t;
	float m0 = it*it*it;
	float m1 = 3 * t*it*it;
	float m2 = 3 * t*t*it;
	float m3 = t*t*t;

	p.x = m0*pnts[0].x + m1*pnts[1].x + m2*pnts[2].x + m3*pnts[3].x;
	p.y = m0*pnts[0].y + m1*pnts[1].y + m2*pnts[2].y + m3*pnts[3].y;
	p.z = m0*pnts[0].z + m1*pnts[1].z + m2*pnts[2].z + m3*pnts[3].z;

	return p;
}


Point3D Calculate(float u, float v) {

	Point3D temp[4];

	temp[0] = CalculateU(u, 0);
	temp[1] = CalculateU(u, 1);
	temp[2] = CalculateU(u, 2);
	temp[3] = CalculateU(u, 3);

	return CalculateV(v, temp);
}

Figure definePatches(Figure aux, int tess, int in, int pn){

	Figure f;
	int i, j, k, o=0;
	int row, col;
	Point3D cpoint;
	float u, v;

	for (k = 0; k < in; k++){
		for (i = 0; i < 16; i++){
			cpoint = aux.getPoints()[aux.getIndexes()[i]];
			row = (int)i / 4;
			col = i - 4 * row;
			cp[row][col].x = cpoint.x;
			cp[row][col].y = cpoint.y;
			cp[row][col].z = cpoint.z;
		}

		for (i = 0; i != tess; ++i) {
			u = (float)i / (tess - 1);
			for (j = 0; j != tess; ++j) {
				v = (float)j / (tess - 1);
				Point3D p = Calculate(u, v);
				f.appendPoint(p);
			}
		}
				
	}
	
	col = (tess ) * (tess );
	for (i = 0; i<in; i++){ //os ciclos estão bem pois in*tess*tess = 12800 = nro vertices
		for (j = 0; j<tess; j++) {
			for (k = 0; k<tess; k++) {
				
				f.appendIndice(i*col + j*(tess + 1) + k); 
				f.appendIndice(i*col + j*(tess + 1) + k + 1);
				f.appendIndice(i*col + (j + 1)*(tess + 1) + k);
				f.appendIndice(i*col + j*(tess + 1) + k + 1);
				f.appendIndice(i*col + (j + 1)*(tess + 1) + k + 1);
				f.appendIndice(i*col + (j + 1)*(tess + 1) + k);
			}
		}
	}

	return f;
}

Figure FigureFactory::createBezierSurface(Figure* f , std::string patchfile, int tess) {

	Figure aux;
	int i, j, in, pn, ind, row, col;
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

	}else {
		cout << "Erro no ficheiro .patch!" << endl;
	}
	
	 

	return *f;
}



