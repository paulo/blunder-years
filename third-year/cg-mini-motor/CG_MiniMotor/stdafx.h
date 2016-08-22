// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//
#pragma comment(lib,"glew32.lib")
#pragma once

#include "tinyxml2.h"
#include "targetver.h"
#include "Cameras.h"
#include <stdio.h>
#include <tchar.h>
#define _USE_MATH_DEFINES 
#include <math.h>
#include <glew.h>
#include <GL/glut.h>
#include <IL/il.h>

typedef struct point3D {
	float x;
	float y;
	float z;
} Point3D;

using namespace tinyxml2;
using namespace std;


