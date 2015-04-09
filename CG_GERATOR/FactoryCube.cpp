#include "stdafx.h"
#include "FigureFactory.h"


/*
*Cria uma figura com a forma cúbica
*
*@param	lado comprimento do lado do cubo
*@return  figura com forma cúbica criada
*/
Figure FigureFactory::createCube(float lado){
	float meio = lado / 2;
	return createParallelepiped(meio, meio, meio);
}
