// include the basic windows header files and the Direct3D header file
#include <windows.h>
#include <windowsx.h>
#include <mmsystem.h>
#include <d3d9.h>
#include <d3dx9.h>
#include <dinput.h>
#include <io.h>
#include <stdio.h>
#include <stdlib.h>
#include <strsafe.h>
#include <sys/stat.h>
#include <list>



// define the screen resolution and keyboard macros
#define SCREEN_WIDTH	1024
#define SCREEN_HEIGHT	768
#define PI				3.14159265358979323846
#define KEY_DOWN(vk_code) ((GetAsyncKeyState(vk_code) & 0x8000) ? 1 : 0)
#define KEY_UP(vk_code) ((GetAsyncKeyState(vk_code) & 0x8000) ? 0 : 1)

// include the Direct3D Library file
#pragma comment (lib, "d3d9.lib")
#pragma comment (lib, "d3dx9.lib")
#pragma comment (lib, "dinput8.lib")
#pragma comment (lib, "dxguid.lib")

// global declarations
LPDIRECT3D9 d3d; // the pointer to our Direct3D interface
LPDIRECT3DDEVICE9 d3ddev; // the pointer to the device class
LPDIRECT3DVERTEXBUFFER9 t_buffer = NULL;    // the pointer to the vertex buffer
LPDIRECTINPUT8 din;    // the pointer to our DirectInput interface
LPDIRECTINPUTDEVICE8 dinkeyboard;    // the pointer to the keyboard device
LPDIRECTINPUTDEVICE8 dinmouse;    // the pointer to the mouse device
LPD3DXFONT m_font;  //text font
int glasses = 9;	//no. of glasses
double g = 0.098; // Gravity
bool keyStateLCtrl = FALSE;
bool keyStateLShift = FALSE;
// texture declarations
LPDIRECT3DTEXTURE9 texture_1;    // our first texture

// function prototypes
HRESULT initD3D(HWND hWnd); // sets up and initializes Direct3D
void render_frame(void); // renders a single frame
void cleanD3D(void); // closes Direct3D and releases memory
HRESULT init_graphics(void);    // 3D declarations
void init_light(void);    // sets up the light and the material
void initDInput(HINSTANCE hInstance, HWND hWnd);    // sets up and initializes DirectInput
//void detect_keys(void);    // gets the current keys being pressed
//void detect_mousepos(void);    // gets the mouse movement and updates the static variables
void cleanDInput(void);    // closes DirectInput and releases memory
void DisplaySomeText(float, float, float);
void setCamera(void);

struct CUSTOMVERTEX {FLOAT X, Y, Z; D3DVECTOR NORMAL; FLOAT U, V;};
#define CUSTOMFVF (D3DFVF_XYZ | D3DFVF_NORMAL | D3DFVF_TEX1)

// the WindowProc function prototype
LRESULT CALLBACK WindowProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);

class gameObject
{
	static D3DXMATRIX	RotX, RotY, RotZ;
public:
	LPD3DXMESH          g_pMesh; // Our mesh object in sysmem
	LPD3DXMESH          g_pMeshi; // inside of Our mesh object in sysmem - for open meshes
	D3DMATERIAL9*       g_pMeshMaterials; // Materials for our mesh
	LPDIRECT3DTEXTURE9* g_pMeshTextures; // Textures for our mesh
	DWORD               g_dwNumMaterials;   // Number of mesh materials
	wchar_t*			g_pathXFile;
	wchar_t*			g_pathXiFile;
	D3DXVECTOR3			dxdydz;
	D3DXMATRIX			xyz;
	double				Alpha, Beta, Gamma;
	double				dAlpha, dBeta, dGamma;
	double				mass;
	DWORD				pTimeFrame;
//	D3DXVECTOR3			axisRot, axisRoll;
//	bool				recal;
	class TouchyData{
	public:
		int objectType;
		gameObject *g;
		D3DXVECTOR3 relCPoint;
//		D3DXVECTOR3 relCAngle;
		double energyTransfd;
		TouchyData(int objectType, gameObject *g)
		{
			this->g=g;
			this->objectType=objectType;
		}
		TouchyData(int objectType)
		{
			this->g=g;
		}
		TouchyData(int objectType, gameObject *g, D3DXVECTOR3 relCPoint, double energyTransfd)
		{
			this->g=g;
			this->objectType=objectType;
			this->relCPoint=relCPoint;
			this->energyTransfd=energyTransfd;
		}
	};
	std::list <TouchyData> Touchies;

	gameObject(wchar_t* g_pathXFile, wchar_t* g_pathXiFile = L"")
	{
		this->g_pMesh = NULL; // Our mesh object in sysmem
		this->g_pMeshi = NULL; // Our mesh object in sysmem
		this->g_pMeshMaterials = NULL; // Materials for our mesh
		this->g_pMeshTextures = NULL; // Textures for our mesh
		this->g_dwNumMaterials = 0L;
		this->g_pathXFile = g_pathXFile;
		this->g_pathXiFile = g_pathXiFile;
	}

VOID Cleanup()
{
    if( g_pMeshMaterials != NULL )
        delete[] g_pMeshMaterials;
    if( g_pMeshTextures )
    {
        for( DWORD i = 0; i < g_dwNumMaterials; i++ )
        {
            if( g_pMeshTextures[i] )
                g_pMeshTextures[i]->Release();
        }
        delete[] g_pMeshTextures;
    }
    if( g_pMesh != NULL )
        g_pMesh->Release();
	if( g_pMeshi != NULL )
        g_pMeshi->Release();
}	
VOID drawObject()
{
//     tell Direct3D about our matrix
	Alpha+=dAlpha;
	Beta+=dBeta;
	Gamma+=dGamma;
	xyz._41+= dxdydz.x;
	xyz._42+= dxdydz.y;
	xyz._43+= dxdydz.z;
	D3DXMatrixRotationX(&RotX, Alpha);
	D3DXMatrixRotationY(&RotY, Beta);
	D3DXMatrixRotationZ(&RotZ, Gamma);
    d3ddev->SetTransform(D3DTS_WORLD, &(RotX * RotY * RotZ * xyz));
//	D3DXMatrixTranslation(&xyz, this->dxdydz.x,this->dxdydz.y,this->dxdydz.z);
//	d3ddev->SetTransform( D3DTS_WORLD, &xyz);
	if(g_pMeshi != NULL)
        for( DWORD i = 0; i < g_dwNumMaterials; i++ )
        {
            // Set the material and texture for this subset
            d3ddev->SetMaterial( &g_pMeshMaterials[i] );
            d3ddev->SetTexture( 0, g_pMeshTextures[i] );

            // Draw the mesh subset
            g_pMesh->DrawSubset( i );
			g_pMeshi->DrawSubset( i );
        }
	else
		for( DWORD i = 0; i < g_dwNumMaterials; i++ )
        {
            // Set the material and texture for this subset
            d3ddev->SetMaterial( &g_pMeshMaterials[i] );
            d3ddev->SetTexture( 0, g_pMeshTextures[i] );

            // Draw the mesh subset
            g_pMesh->DrawSubset( i );
        }
}

HRESULT InitGeometry()
{
    LPD3DXBUFFER pD3DXMtrlBuffer;

    const CHAR* strPrefix = ".\\";
    // Load the mesh from the specified file
    if( FAILED( D3DXLoadMeshFromX(	this->g_pathXFile, 
									D3DXMESH_SYSTEMMEM,
									d3ddev, 
									NULL,
									&pD3DXMtrlBuffer, 
									NULL, 
									&g_dwNumMaterials,
									&g_pMesh ) ) )
    {
        //CHAR strTexture[MAX_PATH];
        //StringCchCopyA( strTexture, MAX_PATH, strPrefix );
        //StringCchCatA( strTexture, MAX_PATH, g_pathXFile );
        // If model is not in current folder, try parent folder
        if( FAILED( D3DXLoadMeshFromX(	L"..\\glass.x", 
										D3DXMESH_SYSTEMMEM,
										d3ddev, 
										NULL,
										&pD3DXMtrlBuffer, 
										NULL, 
										&g_dwNumMaterials,
										&g_pMesh ) ) )
        {
            MessageBox( NULL, L"Could not find glass.X", L"Triangles.exe", MB_OK );
            return E_FAIL;
        }
    }

    // We need to extract the material properties and texture names from the 
    // pD3DXMtrlBuffer
    D3DXMATERIAL* d3dxMaterials = ( D3DXMATERIAL* )pD3DXMtrlBuffer->GetBufferPointer();
    g_pMeshMaterials = new D3DMATERIAL9[g_dwNumMaterials];
    if( g_pMeshMaterials == NULL )
        return E_OUTOFMEMORY;
    g_pMeshTextures = new LPDIRECT3DTEXTURE9[g_dwNumMaterials];
    if( g_pMeshTextures == NULL )
        return E_OUTOFMEMORY;

    for( DWORD i = 0; i < g_dwNumMaterials; i++ )
    {
        // Copy the material
        g_pMeshMaterials[i] = d3dxMaterials[i].MatD3D;

        // Set the ambient color for the material (D3DX does not do this)
        g_pMeshMaterials[i].Ambient = g_pMeshMaterials[i].Diffuse;

        g_pMeshTextures[i] = NULL;
        if( d3dxMaterials[i].pTextureFilename != NULL &&
            lstrlenA( d3dxMaterials[i].pTextureFilename ) > 0 )
        {
            // Create the texture
            if( FAILED( D3DXCreateTextureFromFileA( d3ddev,
                                                    d3dxMaterials[i].pTextureFilename,
                                                    &g_pMeshTextures[i] ) ) )
            {
                // If texture is not in current folder, try parent folder
                CHAR strTexture[MAX_PATH];
                StringCchCopyA( strTexture, MAX_PATH, strPrefix );
                StringCchCatA( strTexture, MAX_PATH, d3dxMaterials[i].pTextureFilename );
                // If texture is not in current folder, try parent folder
                if( FAILED( D3DXCreateTextureFromFileA( d3ddev,
                                                        strTexture,
                                                        &g_pMeshTextures[i] ) ) )
                {
                    MessageBox( NULL, L"Could not find texture map", L"Triangles.exe", MB_OK );
                }
            }
        }
    }

    // Done with the material buffer
    pD3DXMtrlBuffer->Release();

	if( FAILED( D3DXLoadMeshFromX(	this->g_pathXiFile, 
									D3DXMESH_SYSTEMMEM,
									d3ddev, 
									NULL,
									&pD3DXMtrlBuffer, 
									NULL, 
									&g_dwNumMaterials,
									&g_pMeshi ) ) )
    {
        // If model is not in current folder, try parent folder
        if( FAILED( D3DXLoadMeshFromX(	L"..\\glassi.x", 
										D3DXMESH_SYSTEMMEM,
										d3ddev, 
										NULL,
										&pD3DXMtrlBuffer, 
										NULL, 
										&g_dwNumMaterials,
										&g_pMeshi ) ) )
        {
            MessageBox( NULL, L"Could not find glassi.x", L"Triangles.exe", MB_OK );
			return S_OK;
        }
    }


    // We need to extract the material properties and texture names from the 
    // pD3DXMtrlBuffer
	d3dxMaterials = ( D3DXMATERIAL* )pD3DXMtrlBuffer->GetBufferPointer();
    g_pMeshMaterials = new D3DMATERIAL9[g_dwNumMaterials];
    if( g_pMeshMaterials == NULL )
        return E_OUTOFMEMORY;
    g_pMeshTextures = new LPDIRECT3DTEXTURE9[g_dwNumMaterials];
    if( g_pMeshTextures == NULL )
        return E_OUTOFMEMORY;

    for( DWORD i = 0; i < g_dwNumMaterials; i++ )
    {
        // Copy the material
        g_pMeshMaterials[i] = d3dxMaterials[i].MatD3D;

        // Set the ambient color for the material (D3DX does not do this)
        g_pMeshMaterials[i].Ambient = g_pMeshMaterials[i].Diffuse;

        g_pMeshTextures[i] = NULL;
        if( d3dxMaterials[i].pTextureFilename != NULL &&
            lstrlenA( d3dxMaterials[i].pTextureFilename ) > 0 )
        {
            // Create the texture
            if( FAILED( D3DXCreateTextureFromFileA( d3ddev,
                                                    d3dxMaterials[i].pTextureFilename,
                                                    &g_pMeshTextures[i] ) ) )
            {
                // If texture is not in current folder, try parent folder
                CHAR strTexture[MAX_PATH];
                StringCchCopyA( strTexture, MAX_PATH, strPrefix );
                StringCchCatA( strTexture, MAX_PATH, d3dxMaterials[i].pTextureFilename );
                // If texture is not in current folder, try parent folder
                if( FAILED( D3DXCreateTextureFromFileA( d3ddev,
                                                        strTexture,
                                                        &g_pMeshTextures[i] ) ) )
                {
                    MessageBox( NULL, L"Could not find texture map for second side", L"Triangles.exe", MB_OK );
                }
            }
        }
    }

    // Done with the material buffer
    pD3DXMtrlBuffer->Release();

    return S_OK;
}

};
class objCamera
{
public:
	double xl,zl, yl;
	DIMOUSESTATE mousestate;    // create a static storage for the mouse-states
	double xc,zc, yc, rXZ, rYZ;
	double dxl, dzl, dyl;
	double velCamera;
	D3DXMATRIX matProjection;     // the projection transform matrix
	D3DXMATRIX matView;    // the view transform matrix
	D3DXMATRIX matTranslate;    // a matrix to store the translation information

objCamera()
{
	velCamera=1;
	xl=0.0;
	zl=-40.0;
	yl=0.0;
	xc=xl;
	zc=zl;
	yc=yl-20.0;
	rXZ=0.0;
	rYZ=0.0;
	// store it to matTranslate
	D3DXMatrixTranslation(&matTranslate, 0.0f, 0.0f, 0.0f);
}
void setCamera(void)
{
// SET UP THE PIPELINE

	// tell Direct3D about our matrix
	d3ddev->SetTransform(D3DTS_WORLD, &matTranslate);
	static BYTE keystate[256];   // create a static storage for the key-states
    dinkeyboard->Acquire();    // get access if we don't have it already
    dinkeyboard->GetDeviceState(256, (LPVOID)keystate);    // fill keystate with values

//	*************Camera movement speed***************	
	//if((keystate[DIK_LSHIFT] & 0x80)&&(!keyStateLShift))
	//{
	//	velCamera*=2;
	//	keyStateLShift = TRUE;
	//}
	//if((keystate[DIK_LSHIFT] & 0x80)&&(keyStateLShift))
	//{
	//	velCamera/=2;
	//	keyStateLShift = FALSE;
	//}
	//if((keystate[DIK_LCONTROL] & 0x80)&&(!keyStateLCtrl))
	//{
	//	velCamera/=2;
	//	keyStateLCtrl = TRUE;
	//}
	//if((keystate[DIK_LCONTROL] & 0x80)&&(keyStateLCtrl))
	//{
	//	velCamera*=2;
	//	keyStateLCtrl = FALSE;
	//}
//	*************Camera movement and controls***************

	dxl=0.0;
	dzl=0.0;
	dyl=0.0;

	if(keystate[DIK_W] & 0x80)
	{
		dxl+=1*sin(rXZ)*cos(rYZ);
		dzl+=1*cos(rXZ)*cos(rYZ);
		dyl+=1*sin(rYZ);
	}
	if(keystate[DIK_A] & 0x80)
	{
		dzl+=1*sin(rXZ);
		dxl-=1*cos(rXZ);
	}
	if(keystate[DIK_D] & 0x80)
	{
		dzl-=1*sin(rXZ);
		dxl+=1*cos(rXZ);
	}
	if(keystate[DIK_S] & 0x80)
	{
		dxl-=1*sin(rXZ)*cos(rYZ);
		dzl-=1*cos(rXZ)*cos(rYZ);
		dyl-=1*sin(rYZ);
	}
	if(((keystate[DIK_W] & 0x80)^(keystate[DIK_S] & 0x80))&((keystate[DIK_A] & 0x80)^(keystate[DIK_D] & 0x80)))
	{
		dxl/=1.41421;
		dyl/=1.41421;
		dzl/=1.41421;
	}
	xl+=dxl*velCamera;
	yl+=dyl*velCamera;
	zl+=dzl*velCamera;

    dinmouse->Acquire();    // get access if we don't have it already

    // fill the mousestate with values
    dinmouse->GetDeviceState(sizeof(DIMOUSESTATE), (LPVOID)&mousestate);

	rXZ+=(double)mousestate.lX/120;
	xc=20*sin(rXZ);
	zc=20*cos(rXZ);

	rYZ-=(double)mousestate.lY/180;

	if(rYZ>1.570796)
		rYZ=1.570796;
	else if(rYZ<-1.570796)
		rYZ=-1.570796;

	yc=20*sin(rYZ);
	zc*=cos(rYZ);
	xc*=cos(rYZ);
	
	xc+=xl;
	yc+=yl;
	zc+=zl;

    D3DXMatrixLookAtLH(&matView,
                       &D3DXVECTOR3 (xl, yl, zl),    // the camera position
                       &D3DXVECTOR3 (xc, yc, zc),    // the look-at position
                       &D3DXVECTOR3 (0.0f, 1.0f, 0.0f));    // the up direction

	d3ddev->SetTransform(D3DTS_VIEW, &matView);    // set the view transform to matView



    D3DXMatrixPerspectiveFovLH(&matProjection,
                               D3DXToRadian(80),    // the horizontal field of view
                               (FLOAT)SCREEN_WIDTH / (FLOAT)SCREEN_HEIGHT, // aspect ratio
                               1.0f,    // the near view-plane
                               150.0f);    // the far view-plane

    d3ddev->SetTransform(D3DTS_PROJECTION, &matProjection);    // set the projection

}
};

//------------------------------------declaration of Game Objects----------------------------
gameObject **gObjects; 
D3DXMATRIX gameObject::RotX;
D3DXMATRIX gameObject::RotY;
D3DXMATRIX gameObject::RotZ;
objCamera  *mainCamera;
// the entry point for any Windows program

int WINAPI WinMain(HINSTANCE hInstance,
                   HINSTANCE hPrevInstance,
                   LPSTR lpCmdLine,
                   int nCmdShow)
{
    HWND hWnd;
    WNDCLASSEX wc;

    ZeroMemory(&wc, sizeof(WNDCLASSEX));

    wc.cbSize = sizeof(WNDCLASSEX);
    wc.style = CS_HREDRAW | CS_VREDRAW;
    wc.lpfnWndProc = (WNDPROC)WindowProc;
    wc.hInstance = hInstance;
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    // wc.hbrBackground = (HBRUSH)COLOR_WINDOW;    // not needed any more
    wc.lpszClassName = L"WindowClass";

    RegisterClassEx(&wc);

    hWnd = CreateWindowEx(NULL,
                          L"WindowClass",
                          L"Our Direct3D Program",
						  WS_OVERLAPPEDWINDOW,	//WS_EX_TOPMOST | WS_POPUP,    // fullscreen values
                          0, 0,    // the starting x and y positions should be 0
                          SCREEN_WIDTH, SCREEN_HEIGHT,    // set the window to 640 x 480
                          NULL,
                          NULL,
                          hInstance,
                          NULL);

    ShowWindow(hWnd, nCmdShow);
	gObjects = new gameObject*[glasses];
	mainCamera = new objCamera;
	for(DWORD i=0; i<glasses; i++)
	{
		gObjects[i] = new gameObject(L"glass.x", L"glassi.x");    // Initializing the Game Objects
		D3DXMatrixTranslation(&(gObjects[i]->xyz), 30*sin(0.79*i),(i/8)*22,30*cos(0.79*i));
		//gObjects[i]->dxdydz = D3DXVECTOR3(-2.7f, -3.1f, -2.7f);
		gObjects[i]->dxdydz = D3DXVECTOR3(0.0f, 0.0f, 0.0f);
		gObjects[i]->Alpha = i*0.5f-0.8f;
		gObjects[i]->Beta = i*0.3f-0.6f;
		gObjects[i]->Gamma = i*0.8f-1.2f;
		gObjects[i]->dAlpha = (glasses-i+1)*0.005f;
		gObjects[i]->dBeta = (i+1)*0.013f;
		gObjects[i]->dGamma = (i/glasses)*0.08f;
	}
    // set up and initialize Direct3D
    if( SUCCEEDED(initD3D(hWnd)))
	{
		initDInput(hInstance, hWnd);
		// enter the main loop:

		MSG msg;

		while(TRUE)
		{
			DWORD starting_point = GetTickCount();

			if (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
			{
				if (msg.message == WM_QUIT)
					break;
				TranslateMessage(&msg);
				DispatchMessage(&msg);
			}

			render_frame();

			// check the 'escape' key
			if(KEY_DOWN(VK_ESCAPE))
				PostMessage(hWnd, WM_DESTROY, 0, 0);

			while ((GetTickCount() - starting_point) < 17 );
		}
		// clean up DirectX and COM
		cleanD3D();
		cleanDInput();
		return msg.wParam;
	}
	return 0;
}


// this is the main message handler for the program
LRESULT CALLBACK WindowProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    switch(message)
    {
        case WM_DESTROY:
            {
                PostQuitMessage(0);
                return 0;
            } break;
    }
    return DefWindowProc (hWnd, message, wParam, lParam);
}


// this function initializes and prepares Direct3D for use
HRESULT initD3D(HWND hWnd)
{
    if( NULL == (d3d = Direct3DCreate9(D3D_SDK_VERSION))) // create the Direct3D interface
		return E_FAIL;

    D3DPRESENT_PARAMETERS d3dpp; // create a struct to hold various device information

    ZeroMemory(&d3dpp, sizeof(d3dpp));    // clear out the struct for use
    d3dpp.Windowed = TRUE; //FALSE;    // program fullscreen, not windowed
    d3dpp.SwapEffect = D3DSWAPEFFECT_DISCARD;    // discard old frames
    d3dpp.hDeviceWindow = hWnd;    // set the window to be used by Direct3D
    d3dpp.BackBufferFormat = D3DFMT_X8R8G8B8;    // set the back buffer format to 32-bit
    d3dpp.BackBufferWidth = SCREEN_WIDTH;    // set the width of the buffer
    d3dpp.BackBufferHeight = SCREEN_HEIGHT;    // set the height of the buffer
	d3dpp.EnableAutoDepthStencil = TRUE;
	d3dpp.AutoDepthStencilFormat = D3DFMT_D16;

    // create a device class using this information and the info from the d3dpp stuct
    if( FAILED( d3d->CreateDevice(D3DADAPTER_DEFAULT,
                      D3DDEVTYPE_HAL,
                      hWnd,
                      D3DCREATE_SOFTWARE_VERTEXPROCESSING,
                      &d3dpp,
                      &d3ddev)))
	{
		return E_FAIL;
	}

	// Create a D3DX font object
	D3DXCreateFont( d3ddev, 20, 0, FW_NORMAL, 0, FALSE, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH | FF_DONTCARE, TEXT("Arial"), &m_font );
	
	if( FAILED( init_graphics()))    // call the function to initialize the cube
		return E_FAIL;
	init_light();    // call the function to initialize the light and material

	d3ddev->SetRenderState(D3DRS_LIGHTING, TRUE);    // turn on the 3D lighting
	d3ddev->SetRenderState(D3DRS_ZENABLE, TRUE);    // turn on the z-buffer
	d3ddev->SetRenderState(D3DRS_AMBIENT, D3DCOLOR_XRGB(50, 50, 50));    // ambient light
	d3ddev->SetRenderState(D3DRS_NORMALIZENORMALS, TRUE);

	return S_OK;
}
// this is the function used to render a single frame
void render_frame(void)
{

    // clear the window to a deep blue
    d3ddev->Clear(0, NULL, D3DCLEAR_TARGET | D3DCLEAR_ZBUFFER, D3DCOLOR_XRGB(0, 0, 0), 1.0f, 0);

    d3ddev->BeginScene();    // begins the 3D scene

    // select which vertex format we are using
    d3ddev->SetFVF(CUSTOMFVF);
	
	//Set Camera position, view, up and Projection angles
	mainCamera->setCamera();
    // select the vertex buffer to display
    d3ddev->SetStreamSource(0, t_buffer, 0, sizeof(CUSTOMVERTEX));

	// set the texture
    d3ddev->SetTexture(0, texture_1);

    // copy the vertex buffer to the back buffer
	
	if(mainCamera->zl > -40)d3ddev->DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
	if(mainCamera->zl < +40)d3ddev->DrawPrimitive(D3DPT_TRIANGLESTRIP, 4, 2);
	if(mainCamera->yl < +40)d3ddev->DrawPrimitive(D3DPT_TRIANGLESTRIP, 8, 2);
	if(mainCamera->yl > -40)d3ddev->DrawPrimitive(D3DPT_TRIANGLESTRIP, 12, 2);
	if(mainCamera->xl < +40)d3ddev->DrawPrimitive(D3DPT_TRIANGLESTRIP, 16, 2);
	if(mainCamera->xl > -40)d3ddev->DrawPrimitive(D3DPT_TRIANGLESTRIP, 20, 2);
	for(DWORD i=0; i<glasses; i++)
	{
		gObjects[i]->drawObject();
	}
	DisplaySomeText(mainCamera->xl, mainCamera->yl, mainCamera->zl);

    d3ddev->EndScene();    // ends the 3D scene

    d3ddev->Present(NULL, NULL, NULL, NULL);   // displays the created frame on the screen

    return;
}
// this is the function that cleans up Direct3D and COM
void cleanD3D(void)
{
	if( t_buffer != NULL )
		t_buffer->Release();    // close and release the vertex buffer
	if( d3ddev != NULL )
        d3ddev->Release(); // close and release the 3D device
    if( d3d != NULL )
        d3d->Release(); // close and release Direct3D->Release();
	if( texture_1 != NULL )
		texture_1->Release();    // close and release the texture
	if( m_font != NULL )
		m_font->Release();
	for(DWORD i=0; i<glasses; i++)
	{
		gObjects[i]->Cleanup();
	}
    return;
}

// this is the function that puts the 3D models into video RAM
HRESULT init_graphics(void)
{

	// load the texture we will use
    D3DXCreateTextureFromFile(d3ddev,
                              L"wall.bmp",
                              &texture_1);

    // create the vertices using the CUSTOMVERTEX struct
    CUSTOMVERTEX t_vert[] =
    {
          // side 1
		{ 40.0f, 40.0f, -40.0f, 0, 0, 1, 1, 1, },
        { -40.0f, 40.0f, -40.0f, 0, 0, 1, 0, 1, },
        { 40.0f, -40.0f, -40.0f, 0, 0, 1, 1, 0, },
		{ -40.0f, -40.0f, -40.0f, 0, 0, 1, 0, 0, },

        // side 2
        { -40.0f, -40.0f, 40.0f, 0, 0, -1, 0, 0, },
        { -40.0f, 40.0f, 40.0f, 0, 0, -1, 0, 1, },
        { 40.0f, -40.0f, 40.0f, 0, 0, -1, 1, 0, },
		{ 40.0f, 40.0f, 40.0f, 0, 0, -1, 1, 1, },

        // side 3
        { 40.0f, 40.0f, 40.0f, 0, -1, 0, 1, 1, },
        { -40.0f, 40.0f, 40.0f, 0, -1, 0, 0, 1, },
        { 40.0f, 40.0f, -40.0f, 0, -1, 0, 1, 0, },
        { -40.0f, 40.0f, -40.0f, 0, -1, 0, 0, 0, },

        // side 4
        { -40.0f, -40.0f, -40.0f, 0, 1, 0, 0, 0, },
        { -40.0f, -40.0f, 40.0f, 0, 1, 0, 0, 1, },
        { 40.0f, -40.0f, -40.0f, 0, 1, 0, 1, 0, },
		{ 40.0f, -40.0f, 40.0f, 0, 1, 0, 1, 1, },

        // side 5
        { 40.0f, 40.0f, 40.0f, -1, 0, 0, 1, 1, },
        { 40.0f, 40.0f, -40.0f, -1, 0, 0, 1, 0, },
        { 40.0f, -40.0f, 40.0f, -1, 0, 0, 0, 1, },
        { 40.0f, -40.0f, -40.0f, -1, 0, 0, 0, 0, },

        // side 6
        { -40.0f, -40.0f, -40.0f, 1, 0, 0, 0, 0, },
        { -40.0f, 40.0f, -40.0f, 1, 0, 0, 1, 0, },
        { -40.0f, -40.0f, 40.0f, 1, 0, 0, 0, 1, },
		{ -40.0f, 40.0f, 40.0f, 1, 0, 0, 1, 1, },
    };

    // create a vertex buffer interface called t_buffer
    d3ddev->CreateVertexBuffer(24*sizeof(CUSTOMVERTEX),
                               0,
                               CUSTOMFVF,
                               D3DPOOL_MANAGED,
                               &t_buffer,
                               NULL);

    VOID* pVoid;    // a void pointer

    // lock t_buffer and load the vertices into it
    t_buffer->Lock(0, 0, (void**)&pVoid, 0);
    memcpy(pVoid, t_vert, sizeof(t_vert));
    t_buffer->Unlock();
	
	for(DWORD i=0; i<glasses; i++)
	{
		if( FAILED( gObjects[i]->InitGeometry()))
		{
			MessageBox( NULL, L"Could not Initialize Geometry", L"Triangle.exe", MB_OK );
			return E_FAIL;
		}
	}
    return S_OK;
}
// this is the function that sets up the lights and materials
void init_light(void)
{
    D3DLIGHT9 light;    // create the light struct
    D3DMATERIAL9 material;    // create the material struct


    ZeroMemory(&light, sizeof(light));    // clear out the light struct for use
    light.Type = D3DLIGHT_POINT;    // make the light type spot light'
    light.Diffuse.r = 0.6f;    // .5 red
    light.Diffuse.g = 0.6f;    // .5 green
    light.Diffuse.b = 0.6f;    // .5 blue
    light.Diffuse.a = 1.0f;    // full alpha (we'll get to that soon)
    light.Range = 80.0f;    // a range of 100
    light.Attenuation0 = 0.05f;    // no constant inverse attenuation
//    light.Attenuation1 = 0.025f;    // only .125 inverse attenuation
//    light.Attenuation2 = 0.0f;    // no square inverse attenuation
 //   light.Phi = D3DXToRadian(30.0f);    // set the outer cone to 30 degrees
 //   light.Theta = D3DXToRadian(20.0f);    // set the inner cone to 10 degrees
 //   light.Falloff = 0.1f;    // use the typical falloff

    D3DVECTOR vecPosition = {0.0f, 35.0f, 0.0f};    // the position of the light
    light.Position = vecPosition;    // set the position

//    D3DVECTOR vecDirection = {0.0f, 15.0f, 0.0f};    // the direction of the light
//    light.Direction = vecDirection;    // set the direction

    d3ddev->SetLight(0, &light);    // send the light struct properties to light #0
    d3ddev->LightEnable(0, TRUE);    // turn on light #0
	d3ddev->SetRenderState( D3DRS_LIGHTING, TRUE );

    ZeroMemory(&material, sizeof(D3DMATERIAL9));    // clear out the struct for use
    material.Diffuse.r = material.Ambient.r = 1.0f;    // set the material to full red
    material.Diffuse.g = material.Ambient.g = 1.0f;    // set the material to full green
    material.Diffuse.b = material.Ambient.b = 1.0f;    // set the material to full blue
    material.Diffuse.a = material.Ambient.a = 1.0f;    // set the material to full alpha

    d3ddev->SetMaterial(&material);    // set the globably-used material to &material

    return;
}
// this is the function that initializes DirectInput
void initDInput(HINSTANCE hInstance, HWND hWnd)
{
    // create the DirectInput interface
    DirectInput8Create(hInstance,    // the handle to the application
                       DIRECTINPUT_VERSION,    // the compatible version
                       IID_IDirectInput8,    // the DirectInput interface version
                       (void**)&din,    // the pointer to the interface
                       NULL);    // COM stuff, so we'll set it to NULL

    // create the keyboard device
    din->CreateDevice(GUID_SysKeyboard,    // the default keyboard ID being used
                      &dinkeyboard,    // the pointer to the device interface
                      NULL);    // COM stuff, so we'll set it to NULL

    // create the mouse device
    din->CreateDevice(GUID_SysMouse,    // the default mouse ID being used
                      &dinmouse,    // the pointer to the device interface
                      NULL);    // COM stuff, so we'll set it to NULL

    dinkeyboard->SetDataFormat(&c_dfDIKeyboard); // set the data format to keyboard format
    dinmouse->SetDataFormat(&c_dfDIMouse);    // set the data format to mouse format

    // set the control you will have over the keyboard
    dinkeyboard->SetCooperativeLevel(hWnd,
                                     DISCL_NONEXCLUSIVE | DISCL_BACKGROUND);
    // set the control you will have over the mouse
    dinmouse->SetCooperativeLevel(hWnd,
                                  DISCL_NONEXCLUSIVE | DISCL_BACKGROUND);

    dinmouse->Acquire();    // Acquire the mouse only once

    return;    // return to WinMain()
}
void cleanDInput(void)
{
    dinkeyboard->Unacquire();    // make sure the keyboard is unacquired
    dinmouse->Unacquire();    // make sure the mouse is unacquired
    din->Release();    // close DirectInput before exiting

    return;
}
void DisplaySomeText(float rX,float rY,float rZ)
{
	wchar_t displayText[50];
	char buf[50];
	sprintf(buf, "rX:%f  rY:%f rZ:%f", rX, rY, rZ);
	mbstowcs(displayText,buf,50);
	// Create a colour for the text - in this case blue
	D3DCOLOR fontColor = D3DCOLOR_ARGB(255,0,0,255);   

	// Create a rectangle to indicate where on the screen it should be drawn
	RECT rct;
	rct.left=2;
	rct.right=780;
	rct.top=10;
	rct.bottom=rct.top+20;

	// Draw some text
	m_font->DrawText(NULL, displayText, -1, &rct, 0, fontColor );
}



class Collision{
void generateTouchies()
{
	for(DWORD i=0; i<glasses; i++)
	{
		for(DWORD j=i+1; j<glasses; j++)
		{
			
		}
//		gObjects[i]->Touchies.push_back(new gameObject::TouchyData(0));
	}

}
};