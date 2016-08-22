#include "stdafx.h"
#include "patch.h"

Point3D cp[4][4]; //control points structure

Point3D Patch::CalculateU(float t, int linha) {

	Point3D p;

	float it = 1.0 - t;
	float m0 = it*it*it;
	float m1 = 3*t*it*it;
	float m2 = 3*t*t*it;
	float m3 = t*t*t;

	p.x = m0*cp[linha][0].x + m1*cp[linha][1].x + m2*cp[linha][2].x + m3*cp[linha][3].x;
	p.y = m0*cp[linha][0].y + m1*cp[linha][1].y + m2*cp[linha][2].y + m3*cp[linha][3].y;
	p.z = m0*cp[linha][0].z + m1*cp[linha][1].z + m2*cp[linha][2].z + m3*cp[linha][3].z;

	return p;
}

Point3D Patch::CalculateV(float t, Point3D* pnts) {
	
	Point3D p;

	float it = 1.0f - t;
	float m0 = it*it*it;
	float m1 = 3*t*it*it;
	float m2 = 3*t*t*it;
	float m3 = t*t*t;

	p.x = m0*pnts[0].x + m1*pnts[1].x +	m2*pnts[2].x +	m3*pnts[3].x;
	p.y = m0*pnts[0].y + m1*pnts[1].y +	m2*pnts[2].y + m3*pnts[3].y;
	p.z = m0*pnts[0].z + m1*pnts[1].z + m2*pnts[2].z + m3*pnts[3].z;

	return p;
}


Point3D Patch::Calculate(float u, float v) {

	Point3D temp[4];

	temp[0] = CalculateU(u, 0);
	temp[1] = CalculateU(u, 1);
	temp[2] = CalculateU(u, 2);
	temp[3] = CalculateU(u, 3);

	return CalculateV(v, temp);
}


Figure Patch::readPatch(string pFile, int tess) {

	int i, j, ni, nv, ind, row, col;
	float coord, pnts[3];
	string line, field;

	Figure fig;
	Point3D point;

	ifstream patchFile(pFile, ios::in);

	//indices das patches?
	
	if (patchFile.is_open()) {

		patchFile >> ni; 
		getline(patchFile, line);


		for (i = 0; i < ni && getline(patchFile, line); i++) {
			stringstream ss(line);
			for (j = 0; j < 16 && getline(ss, field, ','); j++) {
				stringstream fs(field);
				ind = 0;
				fs >> ind;
				fig.appendIndice(ind);

			}
		}

		//ler pontos de controlo

		patchFile >> nv;
		getline(patchFile, line);

		// to get every line
		for (i = 0; i < nv && getline(patchFile, line); i++) {
			stringstream ss(line); 
			for (j = 0; j < 3 && getline(ss, field, ','); j++) { // each three floats in a line
				stringstream fs(field);
				coord = 0.0;
				fs >> coord;
				pnts[j] = coord;
			}

			row = (int)i / 4;
			col = i - 4 * row;
			
	
			point.x = cp[row][col].x = pnts[0];
			point.y = cp[row][col].y = pnts[1];
			point.z = cp[row][col].z = pnts[2];

			//fig.appendPoint(point);
		}
		patchFile.close();
	}
	else {
		cout << "Erro no ficheiro .patch!" << endl;
	}


	for (int i = 0; i != tess; ++i) {

		float u = (float)i / (tess - 1);

		for (int j = 0; j != tess; ++j) {

			float v = (float)j / (tess - 1);
			Point3D p = Calculate(u, v);
			fig.appendPoint(point);
		}
	}
	return fig;
}



