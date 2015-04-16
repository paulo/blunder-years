#include "stdafx.h"

/*
*Construtor da classe CameraFP
*/
CameraFP::CameraFP(){
	pitch = yaw = 0;
	posZ = posY = posX = 5;
}

/*
*Altera as dimensões da janela
*
*@param wWidth	largura da janela
*@param wHeight	altura da janela
*/
void CameraFP::setWindowSize(float wWidth, float wHeight){
	windowHeight = wHeight;
	windowWidth = wWidth;
}

/*
*Inicializar a camera
*
*/
void CameraFP::start(){
	//glutSetCursor(GLUT_CURSOR_NONE);
	glutWarpPointer(windowWidth / 2, windowHeight / 2);
}

void CameraFP::setPos(float x, float y, float z){
	this->posX = x; this->posY = y; this->posZ = z;
}

struct point3D CameraFP::getPos(){
	return{ this->posX, this->posY, this->posZ };
}

void CameraFP::setAngles(float yaw, float pitch){
	this->yaw = yaw;
	this->pitch = pitch;
}

/*
*Atualizar a camera
*/
void CameraFP::refresh(){
	float px = cos(yaw) * sin(pitch);
	float py = sin(yaw);
	float pz = cos(pitch) * cos(yaw);
	
	glMatrixMode(GL_MODELVIEW);
	//printf("sphere %f %f\n", yaw, pitch);
	glLoadIdentity();
	gluLookAt(
		posX, posY, posZ,
		posX + px, posY + py, posZ + pz,
		0, 1, 0
		);
}

/*
*Interpreta as teclas premidas no teclado
*
*@param key		tecla premida
*/
void CameraFP::bindKey(unsigned char key) {
	float alfa = 0.5;
	switch (key)
	{
	case ' ':
		break;
	case 'w': 
		moveFront(alfa); refresh();
		break;
	case 's': moveFront(-alfa); refresh();
		break;
	case 'a':  moveLeft(alfa/2); refresh();
		break;
	case 'd': moveRight(alfa/2); refresh();
		break;
	case 'r': moveUp(alfa); refresh();
		break;
	case 'f': moveDown(alfa); refresh();
		break;
	default:
		break;
	}
}

/*
*Interpreta os movimentos do rato
*
*@param x	posição do rato relativamente à janela
*@param y	posição do rato relativamente à janela
*/
void CameraFP::moveMouse(float x, float y) {
	yaw -= (y - windowWidth / 2) * M_PI / 180 * 0.02;
	pitch -= (x - windowHeight / 2) * M_PI / 180 * 0.02;
	if (yaw > M_PI / 2 - 0.01) yaw = M_PI / 2 - 0.01;
	if (yaw < -(M_PI / 2 - 0.01)) yaw = -(M_PI / 2 - 0.01);

	glutWarpPointer(windowHeight / 2, windowWidth / 2);
}

/*
*Move a câmara para a frente na nossa direção
*
*@param alfa distância a ser movida
*/
void CameraFP::moveFront(float alfa){
	float px = cos(yaw) * sin(pitch);
	float py = sin(yaw);
	float pz = cos(pitch) * cos(yaw);

	posX += px * alfa;
	posY += py * alfa;
	posZ += pz * alfa;
}

/*
*Move a câmara para cima
*
*@param alfa	distância a ser movida
*/
void CameraFP::moveUp(float alfa){
	posY += alfa;
}

/*
*Move a câmara para baixo
*
*@param alfa	distância a ser movida
*/
void CameraFP::moveDown(float alfa){
	posY -= alfa;
}

/*
*Move a câmara para a esquerda
*
*@param alfa	distância a ser movida
*/
void CameraFP::moveLeft(float alfa){
	// cross Product between Y axis and p Axis (p is the front)
	// y axis is 0,1,0
	float px = cos(yaw) * sin(pitch);
	float py = sin(yaw);
	float pz = cos(pitch) * cos(yaw);
	
	// V = V1 x V2 where
	//vx = v1y * v2z - v1z * v2y
	//vy = v1z * v2x - v1x * v2z
	//vz = v1x * v2y - v1y * v2x
	// in this case the v2y is the unique non-zero
	float lx = 1 * pz - 0 * py;
	float ly = 0 * px - 0 * pz;
	float lz = 0 * py - 1 * px;

	float l = sqrt(lx*lx + ly*ly + lz*lz);

	posX += (lx / l) * alfa; 
	posY += (ly / l) * alfa;
	posZ += (lz / l) * alfa;
}

/*
*Move a câmara para a direita
*
*@param alfa	distância a ser movida
*/
void CameraFP::moveRight(float alfa){
	float px = cos(yaw) * sin(pitch);
	float py = sin(yaw);
	float pz = cos(pitch) * cos(yaw);

	// V = V1 x V2 where
	//vx = v1y * v2z - v1z * v2y
	//vy = v1z * v2x - v1x * v2z
	//vz = v1x * v2y - v1y * v2x
	// in this case the v1y is the unique non-zero
	float lx = py * 0 - pz * 1;
	float ly = pz * 0 - px * 0;
	float lz = px * 1 - py * 0;

	float l = sqrt(lx*lx + ly*ly + lz*lz);

	posX += (lx / l) * alfa; 
	posY += (ly / l) * alfa;
	posZ += (lz / l) * alfa;
}