typedef struct sCameraPosition{
	float x;
	float y;
}* CameraPosition;

class CameraSphere
{
	float lookX, lookY, lookZ;
	float radios;
	float yaw, pitch;

public:
	CameraSphere(float radios);
	void setLookAt(float x, float y, float z);

	void refresh();
	void bindKey(unsigned char a);
	void moveRadios(float alfa);
	void moveUp(float alfa);
	void moveDown(float alfa);
	void moveLeft(float alfa);
	void moveRight(float alfa);
};

class CameraFP
{
	float windowWidth, windowHeight;
	float posX, posY, posZ;
	float yaw, pitch;
public:
	CameraFP();
	void setWindowSize(float wWidth, float wHeight);
	void start();

	void refresh();
	void bindKey(unsigned char a);
	void moveMouse(float x, float y);
	void moveFront(float alfa);
	void moveUp(float alfa);
	void moveDown(float alfa);
	void moveLeft(float alfa);
	void moveRight(float alfa);
};