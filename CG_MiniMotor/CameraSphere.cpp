#include "stdafx.h"
#include <glut.h>

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
	case 'r': moveRadios(1); refresh();
		break;
	case 'f': moveRadios(-1); refresh();
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
	glLoadIdentity();
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
}

/*
*Move a câmara para cima
*
*@param alfa	distância a ser movida
*/
void CameraSphere::moveUp(float alfa){
	yaw += alfa;
	if (yaw > 90) yaw = 90;
	if (yaw < -90) yaw = -90;
}

/*
*Move a câmara para baixo
*
*@param alfa	distância a ser movida
*/
void CameraSphere::moveDown(float alfa){
	yaw -= alfa;
	if (yaw > 90) yaw = 90;
	if (yaw < -90) yaw = -90;
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
