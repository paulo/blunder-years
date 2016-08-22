#include <string>
using namespace std;

typedef struct point3D {
	float x;
	float y;
	float z;
} Point3D;

class Figure {
	int trgLen;
	Point3D trig[];
public:
	void toFile(string file);
	void fromFile();
	void draw();
};