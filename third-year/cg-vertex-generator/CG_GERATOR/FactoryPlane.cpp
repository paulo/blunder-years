#include "stdafx.h"
#include "FigureFactory.h"

/*
*Cria um plano
*
*@param	x	comprimento do plano
*@param	z	largura do plano
*@return  figura com todos os pontos do plano criado
*/
Figure FigureFactory::createPlane(float x, float z){
	Figure f;
	float p[3] = {x/2, 0,z/2};
	createPlaneAux(&f,p,2,0);
	return f;
}

/*
*Função auxiliar à criação de planos, utilizada na criação de cubos e paralelipipedos
*
*@param	f endereço da figura destino
*@param p ponto inicial
*@param axisFirst	variável de auxílio na orientação
*@param axisSecond	variável de auxílio na orientação
*/
void FigureFactory::createPlaneAux(Figure* f, float p[3], int axisFirst, int axisSecond) {
	f->append({ p[0], p[1], p[2] });
	p[axisFirst] = -p[axisFirst];
	f->append({ p[0], p[1], p[2] });
	p[axisSecond] = -p[axisSecond];
	f->append({ p[0], p[1], p[2] });
	f->append({ p[0], p[1], p[2] });
	p[axisFirst] = -p[axisFirst];
	f->append({ p[0], p[1], p[2] });
	p[axisSecond] = -p[axisSecond];
	f->append({ p[0], p[1], p[2] });
}