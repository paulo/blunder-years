#include "stdafx.h"

/*
*Construtor da CameraSphere
*
*@param radios distância inicial que a camera se encontra do objeto
*/
CameraSphere::CameraSphere(float radios){
	this->radios = radios;
	pitch =	yaw = 0;
	lookX = lookY = lookZ = 0;
}

/*
*Altera a posição para onde a camera está a olhar
*
*@param x	coordenadas do ponto no eixo do x
*@param y	coordenadas do ponto no eixo do y
*@param z	coordenadas do ponto no eixo do z
*/
void CameraSphere::setLookAt(float x, float y, float z){
	lookX = x;
	lookY = y;
	lookZ = z;
}

/**
* Altera a posição da camera
*
*@param x	coordenadas do ponto no eixo do x
*@param y	coordenadas do ponto no eixo do y
*@param z	coordenadas do ponto no eixo do z
*/
void CameraSphere::setPos(float x, float y, float z){
	// radios will be the vectorial difference from this points
	radios = sqrt(pow(lookX - x, 2) + pow(lookY - y, 2) + pow(lookZ - z, 2));

	//the yaw yaw will be easy from (y = radios * sin(yaw))
	yaw = asin(y / radios);

	//the pitch will be easy using the yaw (x = radios * cos(yaw) * sin(pitch))
	pitch = asin(x/ (radios * cos(yaw)));
}

struct point3D CameraSphere::getPos(){
	float px = lookX + radios * cos(yaw) * sin(pitch);
	float py = lookY + radios * sin(yaw);
	float pz = lookZ + radios * cos(pitch) * cos(yaw);

	return{ px, py, pz};
}

float CameraSphere::getPitch(){
	return pitch;
}
float CameraSphere::getYaw(){
	return yaw;
}


/*
*Interpreta as teclas premidas no teclado
*
*@param key		tecla premida
*/
void CameraSphere::bindKey(unsigned char key){
	float alfa = 0.1;
	switch (key)
	{
	case ' ':
		break;
	case 'w':
		moveUp(alfa); refresh();
		break;
	case 's': moveDown(alfa); refresh();
		break;
	case 'a': moveRight(alfa); refresh();
		break;
	case 'd': moveLeft(alfa); refresh();
		break;
	case 'f': moveRadios(1); refresh();
		break;
	case 'r': moveRadios(-1); refresh();
		break;
	}
}

/*
*Atualizar a camera
*/
void CameraSphere::refresh(){
	float px = radios * cos(yaw) * sin(pitch);
	float py = radios * sin(yaw);	
	float pz = radios * cos(pitch) * cos(yaw);
	//printf("sphere %f %f\n",yaw,pitch);
	gluLookAt(
		lookX + px, lookY + py, lookZ + pz,
		lookX, lookY, lookZ,
		0, 1, 0
		);
}

/*
*Altera a distancia que a camera se encontra do objeto
*
*@param alfa	distancia a aproximar/afastar
*/
void CameraSphere::moveRadios(float alfa){
	radios += alfa;
	if (radios < 0){
		radios = 0;
	}
}

/*
*Move a câmara para cima
*
*@param alfa	distância a ser movida
*/
void CameraSphere::moveUp(float alfa){
	yaw += alfa;
	if (yaw > M_PI / 2 - 0.01) yaw = M_PI / 2 - 0.01;
	if (yaw < -(M_PI / 2 - 0.01)) yaw = -(M_PI / 2 - 0.01);
}

/*
*Move a câmara para baixo
*
*@param alfa	distância a ser movida
*/
void CameraSphere::moveDown(float alfa){
	yaw -= alfa;
	if (yaw > M_PI / 2 - 0.01) yaw = M_PI / 2 - 0.01;
	if (yaw < -(M_PI / 2 - 0.01)) yaw = -(M_PI / 2 - 0.01);
}

/*
*Move a câmara para a esquerda
*
*@param alfa	distância a ser movida
*/
void CameraSphere::moveLeft(float alfa){
	pitch += alfa;
}

/*
*Move a câmara para a direita
*
*@param alfa	distância a ser movida
*/
void CameraSphere::moveRight(float alfa){
	pitch -= alfa;
}
