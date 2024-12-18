/**********************************************************************************************
*
*   raylib v5.6-dev - A simple and easy-to-use library to enjoy videogames programming (www.raylib.com)
*
*   FEATURES:
*       - NO external dependencies, all required libraries included with raylib
*       - Multiplatform: Windows, Linux, FreeBSD, OpenBSD, NetBSD, DragonFly,
*                        MacOS, Haiku, Android, Raspberry Pi, DRM native, HTML5.
*       - Written in plain C code (C99) in PascalCase/camelCase notation
*       - Hardware accelerated with OpenGL (1.1, 2.1, 3.3, 4.3, ES2, ES3 - choose at compile)
*       - Unique OpenGL abstraction layer (usable as standalone module): [rlgl]
*       - Multiple Fonts formats supported (TTF, OTF, FNT, BDF, Sprite fonts)
*       - Outstanding texture formats support, including compressed formats (DXT, ETC, ASTC)
*       - Full 3d support for 3d Shapes, Models, Billboards, Heightmaps and more!
*       - Flexible Materials system, supporting classic maps and PBR maps
*       - Animated 3D models supported (skeletal bones animation) (IQM, M3D, GLTF)
*       - Shaders support, including Model shaders and Postprocessing shaders
*       - Powerful math module for Vector, Matrix and Quaternion operations: [raymath]
*       - Audio loading and playing with streaming support (WAV, OGG, MP3, FLAC, QOA, XM, MOD)
*       - VR stereo rendering with configurable HMD device parameters
*       - Bindings to multiple programming languages available!
*
*   NOTES:
*       - One default Font is loaded on InitWindow()->LoadFontDefault() [core, text]
*       - One default Texture2D is loaded on rlglInit(), 1x1 white pixel R8G8B8A8 [rlgl] (OpenGL 3.3 or ES2)
*       - One default Shader is loaded on rlglInit()->rlLoadShaderDefault() [rlgl] (OpenGL 3.3 or ES2)
*       - One default RenderBatch is loaded on rlglInit()->rlLoadRenderBatch() [rlgl] (OpenGL 3.3 or ES2)
*
*   DEPENDENCIES (included):
*       [rcore][GLFW] rglfw (Camilla Löwy - github.com/glfw/glfw) for window/context management and input
*       [rcore][RGFW] rgfw (ColleagueRiley - github.com/ColleagueRiley/RGFW) for window/context management and input
*       [rlgl] glad/glad_gles2 (David Herberth - github.com/Dav1dde/glad) for OpenGL 3.3 extensions loading
*       [raudio] miniaudio (David Reid - github.com/mackron/miniaudio) for audio device/context management
*
*   OPTIONAL DEPENDENCIES (included):
*       [rcore] msf_gif (Miles Fogle) for GIF recording
*       [rcore] sinfl (Micha Mettke) for DEFLATE decompression algorithm
*       [rcore] sdefl (Micha Mettke) for DEFLATE compression algorithm
*       [rcore] rprand (Ramon Snatamaria) for pseudo-random numbers generation
*       [rtextures] qoi (Dominic Szablewski - https://phoboslab.org) for QOI image manage
*       [rtextures] stb_image (Sean Barret) for images loading (BMP, TGA, PNG, JPEG, HDR...)
*       [rtextures] stb_image_write (Sean Barret) for image writing (BMP, TGA, PNG, JPG)
*       [rtextures] stb_image_resize2 (Sean Barret) for image resizing algorithms
*       [rtextures] stb_perlin (Sean Barret) for Perlin Noise image generation
*       [rtext] stb_truetype (Sean Barret) for ttf fonts loading
*       [rtext] stb_rect_pack (Sean Barret) for rectangles packing
*       [rmodels] par_shapes (Philip Rideout) for parametric 3d shapes generation
*       [rmodels] tinyobj_loader_c (Syoyo Fujita) for models loading (OBJ, MTL)
*       [rmodels] cgltf (Johannes Kuhlmann) for models loading (glTF)
*       [rmodels] m3d (bzt) for models loading (M3D, https://bztsrc.gitlab.io/model3d)
*       [rmodels] vox_loader (Johann Nadalutti) for models loading (VOX)
*       [raudio] dr_wav (David Reid) for WAV audio file loading
*       [raudio] dr_flac (David Reid) for FLAC audio file loading
*       [raudio] dr_mp3 (David Reid) for MP3 audio file loading
*       [raudio] stb_vorbis (Sean Barret) for OGG audio loading
*       [raudio] jar_xm (Joshua Reisenauer) for XM audio module loading
*       [raudio] jar_mod (Joshua Reisenauer) for MOD audio module loading
*       [raudio] qoa (Dominic Szablewski - https://phoboslab.org) for QOA audio manage
*
*
*   LICENSE: zlib/libpng
*
*   raylib is licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software:
*
*   Copyright (c) 2013-2024 Ramon Santamaria (@raysan5)
*
*   This software is provided "as-is", without any express or implied warranty. In no event
*   will the authors be held liable for any damages arising from the use of this software.
*
*   Permission is granted to anyone to use this software for any purpose, including commercial
*   applications, and to alter it and redistribute it freely, subject to the following restrictions:
*
*     1. The origin of this software must not be misrepresented; you must not claim that you
*     wrote the original software. If you use this software in a product, an acknowledgment
*     in the product documentation would be appreciated but is not required.
*
*     2. Altered source versions must be plainly marked as such, and must not be misrepresented
*     as being the original software.
*
*     3. This notice may not be removed or altered from any source distribution.
*
**********************************************************************************************/

#ifndef RAYLIB_H
#define RAYLIB_H

#include <stdarg.h>     // Required for: va_list - Only used by TraceLogCallback

#define RAYLIB_VERSION_MAJOR 5
#define RAYLIB_VERSION_MINOR 6
#define RAYLIB_VERSION_PATCH 0
#define RAYLIB_VERSION  "5.6-dev"

// Function specifiers in case library is build/used as a shared library
// NOTE: Microsoft specifiers to tell compiler that symbols are imported/exported from a .dll
// NOTE: visibility("default") attribute makes symbols "visible" when compiled with -fvisibility=hidden
#if defined(_WIN32)
    #if defined(__TINYC__)
        #define __declspec(x) __attribute__((x))
    #endif
    #if defined(BUILD_LIBTYPE_SHARED)
        #define RLAPI __declspec(dllexport)     // We are building the library as a Win32 shared library (.dll)
    #elif defined(USE_LIBTYPE_SHARED)
        #define RLAPI __declspec(dllimport)     // We are using the library as a Win32 shared library (.dll)
    #endif
#else
    #if defined(BUILD_LIBTYPE_SHARED)
        #define RLAPI __attribute__((visibility("default"))) // We are building as a Unix shared library (.so/.dylib)
    #endif
#endif

#ifndef RLAPI
    #define RLAPI       // Functions defined as 'extern' by default (implicit specifiers)
#endif

//----------------------------------------------------------------------------------
// Some basic Defines
//----------------------------------------------------------------------------------
#ifndef PI
    #define PI 3.14159265358979323846f
#endif
#ifndef DEG2RAD
    #define DEG2RAD (PI/180.0f)
#endif
#ifndef RAD2DEG
    #define RAD2DEG (180.0f/PI)
#endif

// Allow custom memory allocators
// NOTE: Require recompiling raylib sources
#ifndef RL_MALLOC
    #define RL_MALLOC(sz)       malloc(sz)
#endif
#ifndef RL_CALLOC
    #define RL_CALLOC(n,sz)     calloc(n,sz)
#endif
#ifndef RL_REALLOC
    #define RL_REALLOC(ptr,sz)  realloc(ptr,sz)
#endif
#ifndef RL_FREE
    #define RL_FREE(ptr)        free(ptr)
#endif

// NOTE: MSVC C++ compiler does not support compound literals (C99 feature)
// Plain structures in C++ (without constructors) can be initialized with { }
// This is called aggregate initialization (C++11 feature)
#if defined(__cplusplus)
    #define CLITERAL(type)      type
#else
    #define CLITERAL(type)      (type)
#endif

// Some compilers (mostly macos clang) default to C++98,
// where aggregate initialization can't be used
// So, give a more clear error stating how to fix this
#if !defined(_MSC_VER) && (defined(__cplusplus) && __cplusplus < 201103L)
    #error "C++11 or later is required. Add -std=c++11"
#endif

// NOTE: We set some defines with some data types declared by raylib
// Other modules (raymath, rlgl) also require some of those types, so,
// to be able to use those other modules as standalone (not depending on raylib)
// this defines are very useful for internal check and avoid type (re)definitions
#define RL_COLOR_TYPE
#define RL_RECTANGLE_TYPE
#define RL_VECTOR2_TYPE
#define RL_VECTOR3_TYPE
#define RL_VECTOR4_TYPE
#define RL_QUATERNION_TYPE
#define RL_MATRIX_TYPE

// Some Basic Colors
// NOTE: Custom raylib color palette for amazing visuals on WHITE background
#define LIGHTGRAY  CLITERAL(Color){ 200, 200, 200, 255 }   // Light Gray
#define GRAY       CLITERAL(Color){ 130, 130, 130, 255 }   // Gray
#define DARKGRAY   CLITERAL(Color){ 80, 80, 80, 255 }      // Dark Gray
#define YELLOW     CLITERAL(Color){ 253, 249, 0, 255 }     // Yellow
#define GOLD       CLITERAL(Color){ 255, 203, 0, 255 }     // Gold
#define ORANGE     CLITERAL(Color){ 255, 161, 0, 255 }     // Orange
#define PINK       CLITERAL(Color){ 255, 109, 194, 255 }   // Pink
#define RED        CLITERAL(Color){ 230, 41, 55, 255 }     // Red
#define MAROON     CLITERAL(Color){ 190, 33, 55, 255 }     // Maroon
#define GREEN      CLITERAL(Color){ 0, 228, 48, 255 }      // Green
#define LIME       CLITERAL(Color){ 0, 158, 47, 255 }      // Lime
#define DARKGREEN  CLITERAL(Color){ 0, 117, 44, 255 }      // Dark Green
#define SKYBLUE    CLITERAL(Color){ 102, 191, 255, 255 }   // Sky Blue
#define BLUE       CLITERAL(Color){ 0, 121, 241, 255 }     // Blue
#define DARKBLUE   CLITERAL(Color){ 0, 82, 172, 255 }      // Dark Blue
#define PURPLE     CLITERAL(Color){ 200, 122, 255, 255 }   // Purple
#define VIOLET     CLITERAL(Color){ 135, 60, 190, 255 }    // Violet
#define DARKPURPLE CLITERAL(Color){ 112, 31, 126, 255 }    // Dark Purple
#define BEIGE      CLITERAL(Color){ 211, 176, 131, 255 }   // Beige
#define BROWN      CLITERAL(Color){ 127, 106, 79, 255 }    // Brown
#define DARKBROWN  CLITERAL(Color){ 76, 63, 47, 255 }      // Dark Brown

#define WHITE      CLITERAL(Color){ 255, 255, 255, 255 }   // White
#define BLACK      CLITERAL(Color){ 0, 0, 0, 255 }         // Black
#define BLANK      CLITERAL(Color){ 0, 0, 0, 0 }           // Blank (Transparent)
#define MAGENTA    CLITERAL(Color){ 255, 0, 255, 255 }     // Magenta
#define RAYWHITE   CLITERAL(Color){ 245, 245, 245, 255 }   // My own White (raylib logo)

//----------------------------------------------------------------------------------
// Structures Definition
//----------------------------------------------------------------------------------
// Boolean type
#if (defined(__STDC__) && __STDC_VERSION__ >= 199901L) || (defined(_MSC_VER) && _MSC_VER >= 1800)
    #include <stdbool.h>
#elif !defined(__cplusplus) && !defined(bool)
    typedef enum bool { false = 0, true = !false } bool;
    #define RL_BOOL_TYPE
#endif

// Vector2, 2 components
typedef struct Vector2 {
    float x;                // Vector x component
    float y;                // Vector y component
} Vector2;

// Vector3, 3 components
typedef struct Vector3 {
    float x;                // Vector x component
    float y;                // Vector y component
    float z;                // Vector z component
} Vector3;

// Vector4, 4 components
typedef struct Vector4 {
    float x;                // Vector x component
    float y;                // Vector y component
    float z;                // Vector z component
    float w;                // Vector w component
} Vector4;

// Quaternion, 4 components (Vector4 alias)
typedef Vector4 Quaternion;

// Matrix, 4x4 components, column major, OpenGL style, right-handed
typedef struct Matrix {
    float m0, m4, m8, m12;  // Matrix first row (4 components)
    float m1, m5, m9, m13;  // Matrix second row (4 components)
    float m2, m6, m10, m14; // Matrix third row (4 components)
    float m3, m7, m11, m15; // Matrix fourth row (4 components)
} Matrix;

// Color, 4 components, R8G8B8A8 (32bit)
typedef struct Color {
    unsigned char r;        // Color red value
    unsigned char g;        // Color green value
    unsigned char b;        // Color blue value
    unsigned char a;        // Color alpha value
} Color;

// Rectangle, 4 components
typedef struct Rectangle {
    float x;                // Rectangle top-left corner position x
    float y;                // Rectangle top-left corner position y
    float width;            // Rectangle width
    float height;           // Rectangle height
} Rectangle;

// Image, pixel data stored in CPU memory (RAM)
typedef struct Image {
    void *data;             // Image raw data
    int width;              // Image base width
    int height;             // Image base height
    int mipmaps;            // Mipmap levels, 1 by default
    int format;             // Data format (PixelFormat type)
} Image;

// Texture, tex data stored in GPU memory (VRAM)
typedef struct Texture {
    unsigned int id;        // OpenGL texture id
    int width;              // Texture base width
    int height;             // Texture base height
    int mipmaps;            // Mipmap levels, 1 by default
    int format;             // Data format (PixelFormat type)
} Texture;

// Texture2D, same as Texture
typedef Texture Texture2D;

// TextureCubemap, same as Texture
typedef Texture TextureCubemap;

// RenderTexture, fbo for texture rendering
typedef struct RenderTexture {
    unsigned int id;        // OpenGL framebuffer object id
    Texture texture;        // Color buffer attachment texture
    Texture depth;          // Depth buffer attachment texture
} RenderTexture;

// RenderTexture2D, same as RenderTexture
typedef RenderTexture RenderTexture2D;

// NPatchInfo, n-patch layout info
typedef struct NPatchInfo {
    Rectangle source;       // Texture source rectangle
    int left;               // Left border offset
    int top;                // Top border offset
    int right;              // Right border offset
    int bottom;             // Bottom border offset
    int layout;             // Layout of the n-patch: 3x3, 1x3 or 3x1
} NPatchInfo;

// GlyphInfo, font characters glyphs info
typedef struct GlyphInfo {
    int value;              // Character value (Unicode)
    int offsetX;            // Character offset X when drawing
    int offsetY;            // Character offset Y when drawing
    int advanceX;           // Character advance position X
    Image image;            // Character image data
} GlyphInfo;

// Font, font texture and GlyphInfo array data
typedef struct Font {
    int baseSize;           // Base size (default chars height)
    int glyphCount;         // Number of glyph characters
    int glyphPadding;       // Padding around the glyph characters
    Texture2D texture;      // Texture atlas containing the glyphs
    Rectangle *recs;        // Rectangles in texture for the glyphs
    GlyphInfo *glyphs;      // Glyphs info data
} Font;

// Camera, defines position/orientation in 3d space
typedef struct Camera3D {
    Vector3 position;       // Camera position
    Vector3 target;         // Camera target it looks-at
    Vector3 up;             // Camera up vector (rotation over its axis)
    float fovy;             // Camera field-of-view aperture in Y (degrees) in perspective, used as near plane width in orthographic
    int projection;         // Camera projection: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
} Camera3D;

typedef Camera3D Camera;    // Camera type fallback, defaults to Camera3D

// Camera2D, defines position/orientation in 2d space
typedef struct Camera2D {
    Vector2 offset;         // Camera offset (displacement from target)
    Vector2 target;         // Camera target (rotation and zoom origin)
    float rotation;         // Camera rotation in degrees
    float zoom;             // Camera zoom (scaling), should be 1.0f by default
} Camera2D;

// Mesh, vertex data and vao/vbo
typedef struct Mesh {
    int vertexCount;        // Number of vertices stored in arrays
    int triangleCount;      // Number of triangles stored (indexed or not)

    // Vertex attributes data
    float *vertices;        // Vertex position (XYZ - 3 components per vertex) (shader-location = 0)
    float *texcoords;       // Vertex texture coordinates (UV - 2 components per vertex) (shader-location = 1)
    float *texcoords2;      // Vertex texture second coordinates (UV - 2 components per vertex) (shader-location = 5)
    float *normals;         // Vertex normals (XYZ - 3 components per vertex) (shader-location = 2)
    float *tangents;        // Vertex tangents (XYZW - 4 components per vertex) (shader-location = 4)
    unsigned char *colors;      // Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)
    unsigned short *indices;    // Vertex indices (in case vertex data comes indexed)

    // Animation vertex data
    float *animVertices;    // Animated vertex positions (after bones transformations)
    float *animNormals;     // Animated normals (after bones transformations)
    unsigned char *boneIds; // Vertex bone ids, max 255 bone ids, up to 4 bones influence by vertex (skinning) (shader-location = 6)
    float *boneWeights;     // Vertex bone weight, up to 4 bones influence by vertex (skinning) (shader-location = 7)
    Matrix *boneMatrices;   // Bones animated transformation matrices
    int boneCount;          // Number of bones

    // OpenGL identifiers
    unsigned int vaoId;     // OpenGL Vertex Array Object id
    unsigned int *vboId;    // OpenGL Vertex Buffer Objects id (default vertex data)
} Mesh;

// Shader
typedef struct Shader {
    unsigned int id;        // Shader program id
    int *locs;              // Shader locations array (RL_MAX_SHADER_LOCATIONS)
} Shader;

// MaterialMap
typedef struct MaterialMap {
    Texture2D texture;      // Material map texture
    Color color;            // Material map color
    float value;            // Material map value
} MaterialMap;

// Material, includes shader and maps
typedef struct Material {
    Shader shader;          // Material shader
    MaterialMap *maps;      // Material maps array (MAX_MATERIAL_MAPS)
    float params[4];        // Material generic parameters (if required)
} Material;

// Transform, vertex transformation data
typedef struct Transform {
    Vector3 translation;    // Translation
    Quaternion rotation;    // Rotation
    Vector3 scale;          // Scale
} Transform;

// Bone, skeletal animation bone
typedef struct BoneInfo {
    char name[32];          // Bone name
    int parent;             // Bone parent
} BoneInfo;

// Model, meshes, materials and animation data
typedef struct Model {
    Matrix transform;       // Local transform matrix

    int meshCount;          // Number of meshes
    int materialCount;      // Number of materials
    Mesh *meshes;           // Meshes array
    Material *materials;    // Materials array
    int *meshMaterial;      // Mesh material number

    // Animation data
    int boneCount;          // Number of bones
    BoneInfo *bones;        // Bones information (skeleton)
    Transform *bindPose;    // Bones base transformation (pose)
} Model;

// ModelAnimation
typedef struct ModelAnimation {
    int boneCount;          // Number of bones
    int frameCount;         // Number of animation frames
    BoneInfo *bones;        // Bones information (skeleton)
    Transform **framePoses; // Poses array by frame
    char name[32];          // Animation name
} ModelAnimation;

// Ray, ray for raycasting
typedef struct Ray {
    Vector3 position;       // Ray position (origin)
    Vector3 direction;      // Ray direction (normalized)
} Ray;

// RayCollision, ray hit information
typedef struct RayCollision {
    bool hit;               // Did the ray hit something?
    float distance;         // Distance to the nearest hit
    Vector3 point;          // Point of the nearest hit
    Vector3 normal;         // Surface normal of hit
} RayCollision;

// BoundingBox
typedef struct BoundingBox {
    Vector3 min;            // Minimum vertex box-corner
    Vector3 max;            // Maximum vertex box-corner
} BoundingBox;

// Wave, audio wave data
typedef struct Wave {
    unsigned int frameCount;    // Total number of frames (considering channels)
    unsigned int sampleRate;    // Frequency (samples per second)
    unsigned int sampleSize;    // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
    unsigned int channels;      // Number of channels (1-mono, 2-stereo, ...)
    void *data;                 // Buffer data pointer
} Wave;

// Opaque structs declaration
// NOTE: Actual structs are defined internally in raudio module
typedef struct rAudioBuffer rAudioBuffer;
typedef struct rAudioProcessor rAudioProcessor;

// AudioStream, custom audio stream
typedef struct AudioStream {
    rAudioBuffer *buffer;       // Pointer to internal data used by the audio system
    rAudioProcessor *processor; // Pointer to internal data processor, useful for audio effects

    unsigned int sampleRate;    // Frequency (samples per second)
    unsigned int sampleSize;    // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
    unsigned int channels;      // Number of channels (1-mono, 2-stereo, ...)
} AudioStream;

// Sound
typedef struct Sound {
    AudioStream stream;         // Audio stream
    unsigned int frameCount;    // Total number of frames (considering channels)
} Sound;

// Music, audio stream, anything longer than ~10 seconds should be streamed
typedef struct Music {
    AudioStream stream;         // Audio stream
    unsigned int frameCount;    // Total number of frames (considering channels)
    bool looping;               // Music looping enable

    int ctxType;                // Type of music context (audio filetype)
    void *ctxData;              // Audio context data, depends on type
} Music;

// VrDeviceInfo, Head-Mounted-Display device parameters
typedef struct VrDeviceInfo {
    int hResolution;                // Horizontal resolution in pixels
    int vResolution;                // Vertical resolution in pixels
    float hScreenSize;              // Horizontal size in meters
    float vScreenSize;              // Vertical size in meters
    float eyeToScreenDistance;      // Distance between eye and display in meters
    float lensSeparationDistance;   // Lens separation distance in meters
    float interpupillaryDistance;   // IPD (distance between pupils) in meters
    float lensDistortionValues[4];  // Lens distortion constant parameters
    float chromaAbCorrection[4];    // Chromatic aberration correction parameters
} VrDeviceInfo;

// VrStereoConfig, VR stereo rendering configuration for simulator
typedef struct VrStereoConfig {
    Matrix projection[2];           // VR projection matrices (per eye)
    Matrix viewOffset[2];           // VR view offset matrices (per eye)
    float leftLensCenter[2];        // VR left lens center
    float rightLensCenter[2];       // VR right lens center
    float leftScreenCenter[2];      // VR left screen center
    float rightScreenCenter[2];     // VR right screen center
    float scale[2];                 // VR distortion scale
    float scaleIn[2];               // VR distortion scale in
} VrStereoConfig;

// File path list
typedef struct FilePathList {
    unsigned int capacity;          // Filepaths max entries
    unsigned int count;             // Filepaths entries count
    char **paths;                   // Filepaths entries
} FilePathList;

// Automation event
typedef struct AutomationEvent {
    unsigned int frame;             // Event frame
    unsigned int type;              // Event type (AutomationEventType)
    int params[4];                  // Event parameters (if required)
} AutomationEvent;

// Automation event list
typedef struct AutomationEventList {
    unsigned int capacity;          // Events max entries (MAX_AUTOMATION_EVENTS)
    unsigned int count;             // Events entries count
    AutomationEvent *events;        // Events entries
} AutomationEventList;

//----------------------------------------------------------------------------------
// Enumerators Definition
//----------------------------------------------------------------------------------
// System/Window config flags
// NOTE: Every bit registers one state (use it with bit masks)
// By default all flags are set to 0
typedef enum {
    FLAG_VSYNC_HINT         = 0x00000040,   // Set to try enabling V-Sync on GPU
    FLAG_FULLSCREEN_MODE    = 0x00000002,   // Set to run program in fullscreen
    FLAG_WINDOW_RESIZABLE   = 0x00000004,   // Set to allow resizable window
    FLAG_WINDOW_UNDECORATED = 0x00000008,   // Set to disable window decoration (frame and buttons)
    FLAG_WINDOW_HIDDEN      = 0x00000080,   // Set to hide window
    FLAG_WINDOW_MINIMIZED   = 0x00000200,   // Set to minimize window (iconify)
    FLAG_WINDOW_MAXIMIZED   = 0x00000400,   // Set to maximize window (expanded to monitor)
    FLAG_WINDOW_UNFOCUSED   = 0x00000800,   // Set to window non focused
    FLAG_WINDOW_TOPMOST     = 0x00001000,   // Set to window always on top
    FLAG_WINDOW_ALWAYS_RUN  = 0x00000100,   // Set to allow windows running while minimized
    FLAG_WINDOW_TRANSPARENT = 0x00000010,   // Set to allow transparent framebuffer
    FLAG_WINDOW_HIGHDPI     = 0x00002000,   // Set to support HighDPI
    FLAG_WINDOW_MOUSE_PASSTHROUGH = 0x00004000, // Set to support mouse passthrough, only supported when FLAG_WINDOW_UNDECORATED
    FLAG_BORDERLESS_WINDOWED_MODE = 0x00008000, // Set to run program in borderless windowed mode
    FLAG_MSAA_4X_HINT       = 0x00000020,   // Set to try enabling MSAA 4X
    FLAG_INTERLACED_HINT    = 0x00010000    // Set to try enabling interlaced video format (for V3D)
} ConfigFlags;

// Trace log level
// NOTE: Organized by priority level
typedef enum {
    LOG_ALL = 0,        // Display all logs
    LOG_TRACE,          // Trace logging, intended for internal use only
    LOG_DEBUG,          // Debug logging, used for internal debugging, it should be disabled on release builds
    LOG_INFO,           // Info logging, used for program execution info
    LOG_WARNING,        // Warning logging, used on recoverable failures
    LOG_ERROR,          // Error logging, used on unrecoverable failures
    LOG_FATAL,          // Fatal logging, used to abort program: exit(EXIT_FAILURE)
    LOG_NONE            // Disable logging
} TraceLogLevel;

// Keyboard keys (US keyboard layout)
// NOTE: Use GetKeyPressed() to allow redefining
// required keys for alternative layouts
typedef enum {
    KEY_NULL            = 0,        // Key: NULL, used for no key pressed
    // Alphanumeric keys
    KEY_APOSTROPHE      = 39,       // Key: '
    KEY_COMMA           = 44,       // Key: ,
    KEY_MINUS           = 45,       // Key: -
    KEY_PERIOD          = 46,       // Key: .
    KEY_SLASH           = 47,       // Key: /
    KEY_ZERO            = 48,       // Key: 0
    KEY_ONE             = 49,       // Key: 1
    KEY_TWO             = 50,       // Key: 2
    KEY_THREE           = 51,       // Key: 3
    KEY_FOUR            = 52,       // Key: 4
    KEY_FIVE            = 53,       // Key: 5
    KEY_SIX             = 54,       // Key: 6
    KEY_SEVEN           = 55,       // Key: 7
    KEY_EIGHT           = 56,       // Key: 8
    KEY_NINE            = 57,       // Key: 9
    KEY_SEMICOLON       = 59,       // Key: ;
    KEY_EQUAL           = 61,       // Key: =
    KEY_A               = 65,       // Key: A | a
    KEY_B               = 66,       // Key: B | b
    KEY_C               = 67,       // Key: C | c
    KEY_D               = 68,       // Key: D | d
    KEY_E               = 69,       // Key: E | e
    KEY_F               = 70,       // Key: F | f
    KEY_G               = 71,       // Key: G | g
    KEY_H               = 72,       // Key: H | h
    KEY_I               = 73,       // Key: I | i
    KEY_J               = 74,       // Key: J | j
    KEY_K               = 75,       // Key: K | k
    KEY_L               = 76,       // Key: L | l
    KEY_M               = 77,       // Key: M | m
    KEY_N               = 78,       // Key: N | n
    KEY_O               = 79,       // Key: O | o
    KEY_P               = 80,       // Key: P | p
    KEY_Q               = 81,       // Key: Q | q
    KEY_R               = 82,       // Key: R | r
    KEY_S               = 83,       // Key: S | s
    KEY_T               = 84,       // Key: T | t
    KEY_U               = 85,       // Key: U | u
    KEY_V               = 86,       // Key: V | v
    KEY_W               = 87,       // Key: W | w
    KEY_X               = 88,       // Key: X | x
    KEY_Y               = 89,       // Key: Y | y
    KEY_Z               = 90,       // Key: Z | z
    KEY_LEFT_BRACKET    = 91,       // Key: [
    KEY_BACKSLASH       = 92,       // Key: '\'
    KEY_RIGHT_BRACKET   = 93,       // Key: ]
    KEY_GRAVE           = 96,       // Key: `
    // Function keys
    KEY_SPACE           = 32,       // Key: Space
    KEY_ESCAPE          = 256,      // Key: Esc
    KEY_ENTER           = 257,      // Key: Enter
    KEY_TAB             = 258,      // Key: Tab
    KEY_BACKSPACE       = 259,      // Key: Backspace
    KEY_INSERT          = 260,      // Key: Ins
    KEY_DELETE          = 261,      // Key: Del
    KEY_RIGHT           = 262,      // Key: Cursor right
    KEY_LEFT            = 263,      // Key: Cursor left
    KEY_DOWN            = 264,      // Key: Cursor down
    KEY_UP              = 265,      // Key: Cursor up
    KEY_PAGE_UP         = 266,      // Key: Page up
    KEY_PAGE_DOWN       = 267,      // Key: Page down
    KEY_HOME            = 268,      // Key: Home
    KEY_END             = 269,      // Key: End
    KEY_CAPS_LOCK       = 280,      // Key: Caps lock
    KEY_SCROLL_LOCK     = 281,      // Key: Scroll down
    KEY_NUM_LOCK        = 282,      // Key: Num lock
    KEY_PRINT_SCREEN    = 283,      // Key: Print screen
    KEY_PAUSE           = 284,      // Key: Pause
    KEY_F1              = 290,      // Key: F1
    KEY_F2              = 291,      // Key: F2
    KEY_F3              = 292,      // Key: F3
    KEY_F4              = 293,      // Key: F4
    KEY_F5              = 294,      // Key: F5
    KEY_F6              = 295,      // Key: F6
    KEY_F7              = 296,      // Key: F7
    KEY_F8              = 297,      // Key: F8
    KEY_F9              = 298,      // Key: F9
    KEY_F10             = 299,      // Key: F10
    KEY_F11             = 300,      // Key: F11
    KEY_F12             = 301,      // Key: F12
    KEY_LEFT_SHIFT      = 340,      // Key: Shift left
    KEY_LEFT_CONTROL    = 341,      // Key: Control left
    KEY_LEFT_ALT        = 342,      // Key: Alt left
    KEY_LEFT_SUPER      = 343,      // Key: Super left
    KEY_RIGHT_SHIFT     = 344,      // Key: Shift right
    KEY_RIGHT_CONTROL   = 345,      // Key: Control right
    KEY_RIGHT_ALT       = 346,      // Key: Alt right
    KEY_RIGHT_SUPER     = 347,      // Key: Super right
    KEY_KB_MENU         = 348,      // Key: KB menu
    // Keypad keys
    KEY_KP_0            = 320,      // Key: Keypad 0
    KEY_KP_1            = 321,      // Key: Keypad 1
    KEY_KP_2            = 322,      // Key: Keypad 2
    KEY_KP_3            = 323,      // Key: Keypad 3
    KEY_KP_4            = 324,      // Key: Keypad 4
    KEY_KP_5            = 325,      // Key: Keypad 5
    KEY_KP_6            = 326,      // Key: Keypad 6
    KEY_KP_7            = 327,      // Key: Keypad 7
    KEY_KP_8            = 328,      // Key: Keypad 8
    KEY_KP_9            = 329,      // Key: Keypad 9
    KEY_KP_DECIMAL      = 330,      // Key: Keypad .
    KEY_KP_DIVIDE       = 331,      // Key: Keypad /
    KEY_KP_MULTIPLY     = 332,      // Key: Keypad *
    KEY_KP_SUBTRACT     = 333,      // Key: Keypad -
    KEY_KP_ADD          = 334,      // Key: Keypad +
    KEY_KP_ENTER        = 335,      // Key: Keypad Enter
    KEY_KP_EQUAL        = 336,      // Key: Keypad =
    // Android key buttons
    KEY_BACK            = 4,        // Key: Android back button
    KEY_MENU            = 5,        // Key: Android menu button
    KEY_VOLUME_UP       = 24,       // Key: Android volume up button
    KEY_VOLUME_DOWN     = 25        // Key: Android volume down button
} KeyboardKey;

// Add backwards compatibility support for deprecated names
#define MOUSE_LEFT_BUTTON   MOUSE_BUTTON_LEFT
#define MOUSE_RIGHT_BUTTON  MOUSE_BUTTON_RIGHT
#define MOUSE_MIDDLE_BUTTON MOUSE_BUTTON_MIDDLE

// Mouse buttons
typedef enum {
    MOUSE_BUTTON_LEFT    = 0,       // Mouse button left
    MOUSE_BUTTON_RIGHT   = 1,       // Mouse button right
    MOUSE_BUTTON_MIDDLE  = 2,       // Mouse button middle (pressed wheel)
    MOUSE_BUTTON_SIDE    = 3,       // Mouse button side (advanced mouse device)
    MOUSE_BUTTON_EXTRA   = 4,       // Mouse button extra (advanced mouse device)
    MOUSE_BUTTON_FORWARD = 5,       // Mouse button forward (advanced mouse device)
    MOUSE_BUTTON_BACK    = 6,       // Mouse button back (advanced mouse device)
} MouseButton;

// Mouse cursor
typedef enum {
    MOUSE_CURSOR_DEFAULT       = 0,     // Default pointer shape
    MOUSE_CURSOR_ARROW         = 1,     // Arrow shape
    MOUSE_CURSOR_IBEAM         = 2,     // Text writing cursor shape
    MOUSE_CURSOR_CROSSHAIR     = 3,     // Cross shape
    MOUSE_CURSOR_POINTING_HAND = 4,     // Pointing hand cursor
    MOUSE_CURSOR_RESIZE_EW     = 5,     // Horizontal resize/move arrow shape
    MOUSE_CURSOR_RESIZE_NS     = 6,     // Vertical resize/move arrow shape
    MOUSE_CURSOR_RESIZE_NWSE   = 7,     // Top-left to bottom-right diagonal resize/move arrow shape
    MOUSE_CURSOR_RESIZE_NESW   = 8,     // The top-right to bottom-left diagonal resize/move arrow shape
    MOUSE_CURSOR_RESIZE_ALL    = 9,     // The omnidirectional resize/move cursor shape
    MOUSE_CURSOR_NOT_ALLOWED   = 10     // The operation-not-allowed shape
} MouseCursor;

// Gamepad buttons
typedef enum {
    GAMEPAD_BUTTON_UNKNOWN = 0,         // Unknown button, just for error checking
    GAMEPAD_BUTTON_LEFT_FACE_UP,        // Gamepad left DPAD up button
    GAMEPAD_BUTTON_LEFT_FACE_RIGHT,     // Gamepad left DPAD right button
    GAMEPAD_BUTTON_LEFT_FACE_DOWN,      // Gamepad left DPAD down button
    GAMEPAD_BUTTON_LEFT_FACE_LEFT,      // Gamepad left DPAD left button
    GAMEPAD_BUTTON_RIGHT_FACE_UP,       // Gamepad right button up (i.e. PS3: Triangle, Xbox: Y)
    GAMEPAD_BUTTON_RIGHT_FACE_RIGHT,    // Gamepad right button right (i.e. PS3: Circle, Xbox: B)
    GAMEPAD_BUTTON_RIGHT_FACE_DOWN,     // Gamepad right button down (i.e. PS3: Cross, Xbox: A)
    GAMEPAD_BUTTON_RIGHT_FACE_LEFT,     // Gamepad right button left (i.e. PS3: Square, Xbox: X)
    GAMEPAD_BUTTON_LEFT_TRIGGER_1,      // Gamepad top/back trigger left (first), it could be a trailing button
    GAMEPAD_BUTTON_LEFT_TRIGGER_2,      // Gamepad top/back trigger left (second), it could be a trailing button
    GAMEPAD_BUTTON_RIGHT_TRIGGER_1,     // Gamepad top/back trigger right (first), it could be a trailing button
    GAMEPAD_BUTTON_RIGHT_TRIGGER_2,     // Gamepad top/back trigger right (second), it could be a trailing button
    GAMEPAD_BUTTON_MIDDLE_LEFT,         // Gamepad center buttons, left one (i.e. PS3: Select)
    GAMEPAD_BUTTON_MIDDLE,              // Gamepad center buttons, middle one (i.e. PS3: PS, Xbox: XBOX)
    GAMEPAD_BUTTON_MIDDLE_RIGHT,        // Gamepad center buttons, right one (i.e. PS3: Start)
    GAMEPAD_BUTTON_LEFT_THUMB,          // Gamepad joystick pressed button left
    GAMEPAD_BUTTON_RIGHT_THUMB          // Gamepad joystick pressed button right
} GamepadButton;

// Gamepad axis
typedef enum {
    GAMEPAD_AXIS_LEFT_X        = 0,     // Gamepad left stick X axis
    GAMEPAD_AXIS_LEFT_Y        = 1,     // Gamepad left stick Y axis
    GAMEPAD_AXIS_RIGHT_X       = 2,     // Gamepad right stick X axis
    GAMEPAD_AXIS_RIGHT_Y       = 3,     // Gamepad right stick Y axis
    GAMEPAD_AXIS_LEFT_TRIGGER  = 4,     // Gamepad back trigger left, pressure level: [1..-1]
    GAMEPAD_AXIS_RIGHT_TRIGGER = 5      // Gamepad back trigger right, pressure level: [1..-1]
} GamepadAxis;

// Material map index
typedef enum {
    MATERIAL_MAP_ALBEDO = 0,        // Albedo material (same as: MATERIAL_MAP_DIFFUSE)
    MATERIAL_MAP_METALNESS,         // Metalness material (same as: MATERIAL_MAP_SPECULAR)
    MATERIAL_MAP_NORMAL,            // Normal material
    MATERIAL_MAP_ROUGHNESS,         // Roughness material
    MATERIAL_MAP_OCCLUSION,         // Ambient occlusion material
    MATERIAL_MAP_EMISSION,          // Emission material
    MATERIAL_MAP_HEIGHT,            // Heightmap material
    MATERIAL_MAP_CUBEMAP,           // Cubemap material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
    MATERIAL_MAP_IRRADIANCE,        // Irradiance material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
    MATERIAL_MAP_PREFILTER,         // Prefilter material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
    MATERIAL_MAP_BRDF               // Brdf material
} MaterialMapIndex;

#define MATERIAL_MAP_DIFFUSE      MATERIAL_MAP_ALBEDO
#define MATERIAL_MAP_SPECULAR     MATERIAL_MAP_METALNESS

// Shader location index
typedef enum {
    SHADER_LOC_VERTEX_POSITION = 0, // Shader location: vertex attribute: position
    SHADER_LOC_VERTEX_TEXCOORD01,   // Shader location: vertex attribute: texcoord01
    SHADER_LOC_VERTEX_TEXCOORD02,   // Shader location: vertex attribute: texcoord02
    SHADER_LOC_VERTEX_NORMAL,       // Shader location: vertex attribute: normal
    SHADER_LOC_VERTEX_TANGENT,      // Shader location: vertex attribute: tangent
    SHADER_LOC_VERTEX_COLOR,        // Shader location: vertex attribute: color
    SHADER_LOC_MATRIX_MVP,          // Shader location: matrix uniform: model-view-projection
    SHADER_LOC_MATRIX_VIEW,         // Shader location: matrix uniform: view (camera transform)
    SHADER_LOC_MATRIX_PROJECTION,   // Shader location: matrix uniform: projection
    SHADER_LOC_MATRIX_MODEL,        // Shader location: matrix uniform: model (transform)
    SHADER_LOC_MATRIX_NORMAL,       // Shader location: matrix uniform: normal
    SHADER_LOC_VECTOR_VIEW,         // Shader location: vector uniform: view
    SHADER_LOC_COLOR_DIFFUSE,       // Shader location: vector uniform: diffuse color
    SHADER_LOC_COLOR_SPECULAR,      // Shader location: vector uniform: specular color
    SHADER_LOC_COLOR_AMBIENT,       // Shader location: vector uniform: ambient color
    SHADER_LOC_MAP_ALBEDO,          // Shader location: sampler2d texture: albedo (same as: SHADER_LOC_MAP_DIFFUSE)
    SHADER_LOC_MAP_METALNESS,       // Shader location: sampler2d texture: metalness (same as: SHADER_LOC_MAP_SPECULAR)
    SHADER_LOC_MAP_NORMAL,          // Shader location: sampler2d texture: normal
    SHADER_LOC_MAP_ROUGHNESS,       // Shader location: sampler2d texture: roughness
    SHADER_LOC_MAP_OCCLUSION,       // Shader location: sampler2d texture: occlusion
    SHADER_LOC_MAP_EMISSION,        // Shader location: sampler2d texture: emission
    SHADER_LOC_MAP_HEIGHT,          // Shader location: sampler2d texture: height
    SHADER_LOC_MAP_CUBEMAP,         // Shader location: samplerCube texture: cubemap
    SHADER_LOC_MAP_IRRADIANCE,      // Shader location: samplerCube texture: irradiance
    SHADER_LOC_MAP_PREFILTER,       // Shader location: samplerCube texture: prefilter
    SHADER_LOC_MAP_BRDF,            // Shader location: sampler2d texture: brdf
    SHADER_LOC_VERTEX_BONEIDS,      // Shader location: vertex attribute: boneIds
    SHADER_LOC_VERTEX_BONEWEIGHTS,  // Shader location: vertex attribute: boneWeights
    SHADER_LOC_BONE_MATRICES        // Shader location: array of matrices uniform: boneMatrices
} ShaderLocationIndex;

#define SHADER_LOC_MAP_DIFFUSE      SHADER_LOC_MAP_ALBEDO
#define SHADER_LOC_MAP_SPECULAR     SHADER_LOC_MAP_METALNESS

// Shader uniform data type
typedef enum {
    SHADER_UNIFORM_FLOAT = 0,       // Shader uniform type: float
    SHADER_UNIFORM_VEC2,            // Shader uniform type: vec2 (2 float)
    SHADER_UNIFORM_VEC3,            // Shader uniform type: vec3 (3 float)
    SHADER_UNIFORM_VEC4,            // Shader uniform type: vec4 (4 float)
    SHADER_UNIFORM_INT,             // Shader uniform type: int
    SHADER_UNIFORM_IVEC2,           // Shader uniform type: ivec2 (2 int)
    SHADER_UNIFORM_IVEC3,           // Shader uniform type: ivec3 (3 int)
    SHADER_UNIFORM_IVEC4,           // Shader uniform type: ivec4 (4 int)
    SHADER_UNIFORM_SAMPLER2D        // Shader uniform type: sampler2d
} ShaderUniformDataType;

// Shader attribute data types
typedef enum {
    SHADER_ATTRIB_FLOAT = 0,        // Shader attribute type: float
    SHADER_ATTRIB_VEC2,             // Shader attribute type: vec2 (2 float)
    SHADER_ATTRIB_VEC3,             // Shader attribute type: vec3 (3 float)
    SHADER_ATTRIB_VEC4              // Shader attribute type: vec4 (4 float)
} ShaderAttributeDataType;

// Pixel formats
// NOTE: Support depends on OpenGL version and platform
typedef enum {
    PIXELFORMAT_UNCOMPRESSED_GRAYSCALE = 1, // 8 bit per pixel (no alpha)
    PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA,    // 8*2 bpp (2 channels)
    PIXELFORMAT_UNCOMPRESSED_R5G6B5,        // 16 bpp
    PIXELFORMAT_UNCOMPRESSED_R8G8B8,        // 24 bpp
    PIXELFORMAT_UNCOMPRESSED_R5G5B5A1,      // 16 bpp (1 bit alpha)
    PIXELFORMAT_UNCOMPRESSED_R4G4B4A4,      // 16 bpp (4 bit alpha)
    PIXELFORMAT_UNCOMPRESSED_R8G8B8A8,      // 32 bpp
    PIXELFORMAT_UNCOMPRESSED_R32,           // 32 bpp (1 channel - float)
    PIXELFORMAT_UNCOMPRESSED_R32G32B32,     // 32*3 bpp (3 channels - float)
    PIXELFORMAT_UNCOMPRESSED_R32G32B32A32,  // 32*4 bpp (4 channels - float)
    PIXELFORMAT_UNCOMPRESSED_R16,           // 16 bpp (1 channel - half float)
    PIXELFORMAT_UNCOMPRESSED_R16G16B16,     // 16*3 bpp (3 channels - half float)
    PIXELFORMAT_UNCOMPRESSED_R16G16B16A16,  // 16*4 bpp (4 channels - half float)
    PIXELFORMAT_COMPRESSED_DXT1_RGB,        // 4 bpp (no alpha)
    PIXELFORMAT_COMPRESSED_DXT1_RGBA,       // 4 bpp (1 bit alpha)
    PIXELFORMAT_COMPRESSED_DXT3_RGBA,       // 8 bpp
    PIXELFORMAT_COMPRESSED_DXT5_RGBA,       // 8 bpp
    PIXELFORMAT_COMPRESSED_ETC1_RGB,        // 4 bpp
    PIXELFORMAT_COMPRESSED_ETC2_RGB,        // 4 bpp
    PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA,   // 8 bpp
    PIXELFORMAT_COMPRESSED_PVRT_RGB,        // 4 bpp
    PIXELFORMAT_COMPRESSED_PVRT_RGBA,       // 4 bpp
    PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA,   // 8 bpp
    PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA    // 2 bpp
} PixelFormat;

// Texture parameters: filter mode
// NOTE 1: Filtering considers mipmaps if available in the texture
// NOTE 2: Filter is accordingly set for minification and magnification
typedef enum {
    TEXTURE_FILTER_POINT = 0,               // No filter, just pixel approximation
    TEXTURE_FILTER_BILINEAR,                // Linear filtering
    TEXTURE_FILTER_TRILINEAR,               // Trilinear filtering (linear with mipmaps)
    TEXTURE_FILTER_ANISOTROPIC_4X,          // Anisotropic filtering 4x
    TEXTURE_FILTER_ANISOTROPIC_8X,          // Anisotropic filtering 8x
    TEXTURE_FILTER_ANISOTROPIC_16X,         // Anisotropic filtering 16x
} TextureFilter;

// Texture parameters: wrap mode
typedef enum {
    TEXTURE_WRAP_REPEAT = 0,                // Repeats texture in tiled mode
    TEXTURE_WRAP_CLAMP,                     // Clamps texture to edge pixel in tiled mode
    TEXTURE_WRAP_MIRROR_REPEAT,             // Mirrors and repeats the texture in tiled mode
    TEXTURE_WRAP_MIRROR_CLAMP               // Mirrors and clamps to border the texture in tiled mode
} TextureWrap;

// Cubemap layouts
typedef enum {
    CUBEMAP_LAYOUT_AUTO_DETECT = 0,         // Automatically detect layout type
    CUBEMAP_LAYOUT_LINE_VERTICAL,           // Layout is defined by a vertical line with faces
    CUBEMAP_LAYOUT_LINE_HORIZONTAL,         // Layout is defined by a horizontal line with faces
    CUBEMAP_LAYOUT_CROSS_THREE_BY_FOUR,     // Layout is defined by a 3x4 cross with cubemap faces
    CUBEMAP_LAYOUT_CROSS_FOUR_BY_THREE     // Layout is defined by a 4x3 cross with cubemap faces
} CubemapLayout;

// Font type, defines generation method
typedef enum {
    FONT_DEFAULT = 0,               // Default font generation, anti-aliased
    FONT_BITMAP,                    // Bitmap font generation, no anti-aliasing
    FONT_SDF                        // SDF font generation, requires external shader
} FontType;

// Color blending modes (pre-defined)
typedef enum {
    BLEND_ALPHA = 0,                // Blend textures considering alpha (default)
    BLEND_ADDITIVE,                 // Blend textures adding colors
    BLEND_MULTIPLIED,               // Blend textures multiplying colors
    BLEND_ADD_COLORS,               // Blend textures adding colors (alternative)
    BLEND_SUBTRACT_COLORS,          // Blend textures subtracting colors (alternative)
    BLEND_ALPHA_PREMULTIPLY,        // Blend premultiplied textures considering alpha
    BLEND_CUSTOM,                   // Blend textures using custom src/dst factors (use rlSetBlendFactors())
    BLEND_CUSTOM_SEPARATE           // Blend textures using custom rgb/alpha separate src/dst factors (use rlSetBlendFactorsSeparate())
} BlendMode;

// Gesture
// NOTE: Provided as bit-wise flags to enable only desired gestures
typedef enum {
    GESTURE_NONE        = 0,        // No gesture
    GESTURE_TAP         = 1,        // Tap gesture
    GESTURE_DOUBLETAP   = 2,        // Double tap gesture
    GESTURE_HOLD        = 4,        // Hold gesture
    GESTURE_DRAG        = 8,        // Drag gesture
    GESTURE_SWIPE_RIGHT = 16,       // Swipe right gesture
    GESTURE_SWIPE_LEFT  = 32,       // Swipe left gesture
    GESTURE_SWIPE_UP    = 64,       // Swipe up gesture
    GESTURE_SWIPE_DOWN  = 128,      // Swipe down gesture
    GESTURE_PINCH_IN    = 256,      // Pinch in gesture
    GESTURE_PINCH_OUT   = 512       // Pinch out gesture
} Gesture;

// Camera system modes
typedef enum {
    CAMERA_CUSTOM = 0,              // Camera custom, controlled by user (UpdateCamera() does nothing)
    CAMERA_FREE,                    // Camera free mode
    CAMERA_ORBITAL,                 // Camera orbital, around target, zoom supported
    CAMERA_FIRST_PERSON,            // Camera first person
    CAMERA_THIRD_PERSON             // Camera third person
} CameraMode;

// Camera projection
typedef enum {
    CAMERA_PERSPECTIVE = 0,         // Perspective projection
    CAMERA_ORTHOGRAPHIC             // Orthographic projection
} CameraProjection;

// N-patch layout
typedef enum {
    NPATCH_NINE_PATCH = 0,          // Npatch layout: 3x3 tiles
    NPATCH_THREE_PATCH_VERTICAL,    // Npatch layout: 1x3 tiles
    NPATCH_THREE_PATCH_HORIZONTAL   // Npatch layout: 3x1 tiles
} NPatchLayout;

// Callbacks to hook some internal functions
// WARNING: These callbacks are intended for advanced users
typedef void (*TraceLogCallback)(int logLevel, const char *text, va_list args);  // Logging: Redirect trace log messages
typedef unsigned char *(*LoadFileDataCallback)(const char *fileName, int *dataSize);    // FileIO: Load binary data
typedef bool (*SaveFileDataCallback)(const char *fileName, void *data, int dataSize);   // FileIO: Save binary data
typedef char *(*LoadFileTextCallback)(const char *fileName);            // FileIO: Load text data
typedef bool (*SaveFileTextCallback)(const char *fileName, char *text); // FileIO: Save text data

//------------------------------------------------------------------------------------
// Global Variables Definition
//------------------------------------------------------------------------------------
// It's lonely here...

//------------------------------------------------------------------------------------
// Window and Graphics Device Functions (Module: core)
//------------------------------------------------------------------------------------

#if defined(__cplusplus)
extern "C" {            // Prevents name mangling of functions
#endif

// Window-related functions
RLAPI void InitWindow(int width, int height, const char *title);  // Initialize window and OpenGL context
RLAPI void CloseWindow(void);                                     // Close window and unload OpenGL context
RLAPI bool WindowShouldClose(void);                               // Check if application should close (KEY_ESCAPE pressed or windows close icon clicked)
RLAPI bool IsWindowReady(void);                                   // Check if window has been initialized successfully
RLAPI bool IsWindowFullscreen(void);                              // Check if window is currently fullscreen
RLAPI bool IsWindowHidden(void);                                  // Check if window is currently hidden
RLAPI bool IsWindowMinimized(void);                               // Check if window is currently minimized
RLAPI bool IsWindowMaximized(void);                               // Check if window is currently maximized
RLAPI bool IsWindowFocused(void);                                 // Check if window is currently focused
RLAPI bool IsWindowResized(void);                                 // Check if window has been resized last frame
RLAPI bool IsWindowState(unsigned int flag);                      // Check if one specific window flag is enabled
RLAPI void SetWindowState(unsigned int flags);                    // Set window configuration state using flags
RLAPI void ClearWindowState(unsigned int flags);                  // Clear window configuration state flags
RLAPI void ToggleFullscreen(void);                                // Toggle window state: fullscreen/windowed, resizes monitor to match window resolution
RLAPI void ToggleBorderlessWindowed(void);                        // Toggle window state: borderless windowed, resizes window to match monitor resolution
RLAPI void MaximizeWindow(void);                                  // Set window state: maximized, if resizable
RLAPI void MinimizeWindow(void);                                  // Set window state: minimized, if resizable
RLAPI void RestoreWindow(void);                                   // Set window state: not minimized/maximized
RLAPI void SetWindowIcon(Image image);                            // Set icon for window (single image, RGBA 32bit)
RLAPI void SetWindowIcons(Image *images, int count);              // Set icon for window (multiple images, RGBA 32bit)
RLAPI void SetWindowTitle(const char *title);                     // Set title for window
RLAPI void SetWindowPosition(int x, int y);                       // Set window position on screen
RLAPI void SetWindowMonitor(int monitor);                         // Set monitor for the current window
RLAPI void SetWindowMinSize(int width, int height);               // Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)
RLAPI void SetWindowMaxSize(int width, int height);               // Set window maximum dimensions (for FLAG_WINDOW_RESIZABLE)
RLAPI void SetWindowSize(int width, int height);                  // Set window dimensions
RLAPI void SetWindowOpacity(float opacity);                       // Set window opacity [0.0f..1.0f]
RLAPI void SetWindowFocused(void);                                // Set window focused
RLAPI void *GetWindowHandle(void);                                // Get native window handle
RLAPI int GetScreenWidth(void);                                   // Get current screen width
RLAPI int GetScreenHeight(void);                                  // Get current screen height
RLAPI int GetRenderWidth(void);                                   // Get current render width (it considers HiDPI)
RLAPI int GetRenderHeight(void);                                  // Get current render height (it considers HiDPI)
RLAPI int GetMonitorCount(void);                                  // Get number of connected monitors
RLAPI int GetCurrentMonitor(void);                                // Get current monitor where window is placed
RLAPI Vector2 GetMonitorPosition(int monitor);                    // Get specified monitor position
RLAPI int GetMonitorWidth(int monitor);                           // Get specified monitor width (current video mode used by monitor)
RLAPI int GetMonitorHeight(int monitor);                          // Get specified monitor height (current video mode used by monitor)
RLAPI int GetMonitorPhysicalWidth(int monitor);                   // Get specified monitor physical width in millimetres
RLAPI int GetMonitorPhysicalHeight(int monitor);                  // Get specified monitor physical height in millimetres
RLAPI int GetMonitorRefreshRate(int monitor);                     // Get specified monitor refresh rate
RLAPI Vector2 GetWindowPosition(void);                            // Get window position XY on monitor
RLAPI Vector2 GetWindowScaleDPI(void);                            // Get window scale DPI factor
RLAPI const char *GetMonitorName(int monitor);                    // Get the human-readable, UTF-8 encoded name of the specified monitor
RLAPI void SetClipboardText(const char *text);                    // Set clipboard text content
RLAPI const char *GetClipboardText(void);                         // Get clipboard text content
RLAPI Image GetClipboardImage(void);                              // Get clipboard image content
RLAPI void EnableEventWaiting(void);                              // Enable waiting for events on EndDrawing(), no automatic event polling
RLAPI void DisableEventWaiting(void);                             // Disable waiting for events on EndDrawing(), automatic events polling

// Cursor-related functions
RLAPI void ShowCursor(void);                                      // Shows cursor
RLAPI void HideCursor(void);                                      // Hides cursor
RLAPI bool IsCursorHidden(void);                                  // Check if cursor is not visible
RLAPI void EnableCursor(void);                                    // Enables cursor (unlock cursor)
RLAPI void DisableCursor(void);                                   // Disables cursor (lock cursor)
RLAPI bool IsCursorOnScreen(void);                                // Check if cursor is on the screen

// Drawing-related functions
RLAPI void ClearBackground(Color color);                          // Set background color (framebuffer clear color)
RLAPI void BeginDrawing(void);                                    // Setup canvas (framebuffer) to start drawing
RLAPI void EndDrawing(void);                                      // End canvas drawing and swap buffers (double buffering)
RLAPI void BeginMode2D(Camera2D camera);                          // Begin 2D mode with custom camera (2D)
RLAPI void EndMode2D(void);                                       // Ends 2D mode with custom camera
RLAPI void BeginMode3D(Camera3D camera);                          // Begin 3D mode with custom camera (3D)
RLAPI void EndMode3D(void);                                       // Ends 3D mode and returns to default 2D orthographic mode
RLAPI void BeginTextureMode(RenderTexture2D target);              // Begin drawing to render texture
RLAPI void EndTextureMode(void);                                  // Ends drawing to render texture
RLAPI void BeginShaderMode(Shader shader);                        // Begin custom shader drawing
RLAPI void EndShaderMode(void);                                   // End custom shader drawing (use default shader)
RLAPI void BeginBlendMode(int mode);                              // Begin blending mode (alpha, additive, multiplied, subtract, custom)
RLAPI void EndBlendMode(void);                                    // End blending mode (reset to default: alpha blending)
RLAPI void BeginScissorMode(int x, int y, int width, int height); // Begin scissor mode (define screen area for following drawing)
RLAPI void EndScissorMode(void);                                  // End scissor mode
RLAPI void BeginVrStereoMode(VrStereoConfig config);              // Begin stereo rendering (requires VR simulator)
RLAPI void EndVrStereoMode(void);                                 // End stereo rendering (requires VR simulator)

// VR stereo config functions for VR simulator
RLAPI VrStereoConfig LoadVrStereoConfig(VrDeviceInfo device);     // Load VR stereo config for VR simulator device parameters
RLAPI void UnloadVrStereoConfig(VrStereoConfig config);           // Unload VR stereo config

// Shader management functions
// NOTE: Shader functionality is not available on OpenGL 1.1
RLAPI Shader LoadShader(const char *vsFileName, const char *fsFileName);   // Load shader from files and bind default locations
RLAPI Shader LoadShaderFromMemory(const char *vsCode, const char *fsCode); // Load shader from code strings and bind default locations
RLAPI bool IsShaderValid(Shader shader);                                   // Check if a shader is valid (loaded on GPU)
RLAPI int GetShaderLocation(Shader shader, const char *uniformName);       // Get shader uniform location
RLAPI int GetShaderLocationAttrib(Shader shader, const char *attribName);  // Get shader attribute location
RLAPI void SetShaderValue(Shader shader, int locIndex, const void *value, int uniformType);               // Set shader uniform value
RLAPI void SetShaderValueV(Shader shader, int locIndex, const void *value, int uniformType, int count);   // Set shader uniform value vector
RLAPI void SetShaderValueMatrix(Shader shader, int locIndex, Matrix mat);         // Set shader uniform value (matrix 4x4)
RLAPI void SetShaderValueTexture(Shader shader, int locIndex, Texture2D texture); // Set shader uniform value for texture (sampler2d)
RLAPI void UnloadShader(Shader shader);                                    // Unload shader from GPU memory (VRAM)

// Screen-space-related functions
#define GetMouseRay GetScreenToWorldRay     // Compatibility hack for previous raylib versions
RLAPI Ray GetScreenToWorldRay(Vector2 position, Camera camera);         // Get a ray trace from screen position (i.e mouse)
RLAPI Ray GetScreenToWorldRayEx(Vector2 position, Camera camera, int width, int height); // Get a ray trace from screen position (i.e mouse) in a viewport
RLAPI Vector2 GetWorldToScreen(Vector3 position, Camera camera);        // Get the screen space position for a 3d world space position
RLAPI Vector2 GetWorldToScreenEx(Vector3 position, Camera camera, int width, int height); // Get size position for a 3d world space position
RLAPI Vector2 GetWorldToScreen2D(Vector2 position, Camera2D camera);    // Get the screen space position for a 2d camera world space position
RLAPI Vector2 GetScreenToWorld2D(Vector2 position, Camera2D camera);    // Get the world space position for a 2d camera screen space position
RLAPI Matrix GetCameraMatrix(Camera camera);                            // Get camera transform matrix (view matrix)
RLAPI Matrix GetCameraMatrix2D(Camera2D camera);                        // Get camera 2d transform matrix

// Timing-related functions
RLAPI void SetTargetFPS(int fps);                                 // Set target FPS (maximum)
RLAPI float GetFrameTime(void);                                   // Get time in seconds for last frame drawn (delta time)
RLAPI double GetTime(void);                                       // Get elapsed time in seconds since InitWindow()
RLAPI int GetFPS(void);                                           // Get current FPS

// Custom frame control functions
// NOTE: Those functions are intended for advanced users that want full control over the frame processing
// By default EndDrawing() does this job: draws everything + SwapScreenBuffer() + manage frame timing + PollInputEvents()
// To avoid that behaviour and control frame processes manually, enable in config.h: SUPPORT_CUSTOM_FRAME_CONTROL
RLAPI void SwapScreenBuffer(void);                                // Swap back buffer with front buffer (screen drawing)
RLAPI void PollInputEvents(void);                                 // Register all input events
RLAPI void WaitTime(double seconds);                              // Wait for some time (halt program execution)

// Random values generation functions
RLAPI void SetRandomSeed(unsigned int seed);                      // Set the seed for the random number generator
RLAPI int GetRandomValue(int min, int max);                       // Get a random value between min and max (both included)
RLAPI int *LoadRandomSequence(unsigned int count, int min, int max); // Load random values sequence, no values repeated
RLAPI void UnloadRandomSequence(int *sequence);                   // Unload random values sequence

// Misc. functions
RLAPI void TakeScreenshot(const char *fileName);                  // Takes a screenshot of current screen (filename extension defines format)
RLAPI void SetConfigFlags(unsigned int flags);                    // Setup init configuration flags (view FLAGS)
RLAPI void OpenURL(const char *url);                              // Open URL with default system browser (if available)

// NOTE: Following functions implemented in module [utils]
//------------------------------------------------------------------
RLAPI void TraceLog(int logLevel, const char *text, ...);         // Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR...)
RLAPI void SetTraceLogLevel(int logLevel);                        // Set the current threshold (minimum) log level
RLAPI void *MemAlloc(unsigned int size);                          // Internal memory allocator
RLAPI void *MemRealloc(void *ptr, unsigned int size);             // Internal memory reallocator
RLAPI void MemFree(void *ptr);                                    // Internal memory free

// Set custom callbacks
// WARNING: Callbacks setup is intended for advanced users
RLAPI void SetTraceLogCallback(TraceLogCallback callback);         // Set custom trace log
RLAPI void SetLoadFileDataCallback(LoadFileDataCallback callback); // Set custom file binary data loader
RLAPI void SetSaveFileDataCallback(SaveFileDataCallback callback); // Set custom file binary data saver
RLAPI void SetLoadFileTextCallback(LoadFileTextCallback callback); // Set custom file text data loader
RLAPI void SetSaveFileTextCallback(SaveFileTextCallback callback); // Set custom file text data saver

// Files management functions
RLAPI unsigned char *LoadFileData(const char *fileName, int *dataSize); // Load file data as byte array (read)
RLAPI void UnloadFileData(unsigned char *data);                   // Unload file data allocated by LoadFileData()
RLAPI bool SaveFileData(const char *fileName, void *data, int dataSize); // Save data to file from byte array (write), returns true on success
RLAPI bool ExportDataAsCode(const unsigned char *data, int dataSize, const char *fileName); // Export data to code (.h), returns true on success
RLAPI char *LoadFileText(const char *fileName);                   // Load text data from file (read), returns a '\0' terminated string
RLAPI void UnloadFileText(char *text);                            // Unload file text data allocated by LoadFileText()
RLAPI bool SaveFileText(const char *fileName, char *text);        // Save text data to file (write), string must be '\0' terminated, returns true on success
//------------------------------------------------------------------

// File system functions
RLAPI bool FileExists(const char *fileName);                      // Check if file exists
RLAPI bool DirectoryExists(const char *dirPath);                  // Check if a directory path exists
RLAPI bool IsFileExtension(const char *fileName, const char *ext); // Check file extension (including point: .png, .wav)
RLAPI int GetFileLength(const char *fileName);                    // Get file length in bytes (NOTE: GetFileSize() conflicts with windows.h)
RLAPI const char *GetFileExtension(const char *fileName);         // Get pointer to extension for a filename string (includes dot: '.png')
RLAPI const char *GetFileName(const char *filePath);              // Get pointer to filename for a path string
RLAPI const char *GetFileNameWithoutExt(const char *filePath);    // Get filename string without extension (uses static string)
RLAPI const char *GetDirectoryPath(const char *filePath);         // Get full path for a given fileName with path (uses static string)
RLAPI const char *GetPrevDirectoryPath(const char *dirPath);      // Get previous directory path for a given path (uses static string)
RLAPI const char *GetWorkingDirectory(void);                      // Get current working directory (uses static string)
RLAPI const char *GetApplicationDirectory(void);                  // Get the directory of the running application (uses static string)
RLAPI int MakeDirectory(const char *dirPath);                     // Create directories (including full path requested), returns 0 on success
RLAPI bool ChangeDirectory(const char *dir);                      // Change working directory, return true on success
RLAPI bool IsPathFile(const char *path);                          // Check if a given path is a file or a directory
RLAPI bool IsFileNameValid(const char *fileName);                 // Check if fileName is valid for the platform/OS
RLAPI FilePathList LoadDirectoryFiles(const char *dirPath);       // Load directory filepaths
RLAPI FilePathList LoadDirectoryFilesEx(const char *basePath, const char *filter, bool scanSubdirs); // Load directory filepaths with extension filtering and recursive directory scan. Use 'DIR' in the filter string to include directories in the result
RLAPI void UnloadDirectoryFiles(FilePathList files);              // Unload filepaths
RLAPI bool IsFileDropped(void);                                   // Check if a file has been dropped into window
RLAPI FilePathList LoadDroppedFiles(void);                        // Load dropped filepaths
RLAPI void UnloadDroppedFiles(FilePathList files);                // Unload dropped filepaths
RLAPI long GetFileModTime(const char *fileName);                  // Get file modification time (last write time)

// Compression/Encoding functionality
RLAPI unsigned char *CompressData(const unsigned char *data, int dataSize, int *compDataSize);        // Compress data (DEFLATE algorithm), memory must be MemFree()
RLAPI unsigned char *DecompressData(const unsigned char *compData, int compDataSize, int *dataSize);  // Decompress data (DEFLATE algorithm), memory must be MemFree()
RLAPI char *EncodeDataBase64(const unsigned char *data, int dataSize, int *outputSize);               // Encode data to Base64 string, memory must be MemFree()
RLAPI unsigned char *DecodeDataBase64(const unsigned char *data, int *outputSize);                    // Decode Base64 string data, memory must be MemFree()
RLAPI unsigned int ComputeCRC32(unsigned char *data, int dataSize);     // Compute CRC32 hash code
RLAPI unsigned int *ComputeMD5(unsigned char *data, int dataSize);      // Compute MD5 hash code, returns static int[4] (16 bytes)
RLAPI unsigned int *ComputeSHA1(unsigned char *data, int dataSize);     // Compute SHA1 hash code, returns static int[5] (20 bytes)


// Automation events functionality
RLAPI AutomationEventList LoadAutomationEventList(const char *fileName);                // Load automation events list from file, NULL for empty list, capacity = MAX_AUTOMATION_EVENTS
RLAPI void UnloadAutomationEventList(AutomationEventList list);                         // Unload automation events list from file
RLAPI bool ExportAutomationEventList(AutomationEventList list, const char *fileName);   // Export automation events list as text file
RLAPI void SetAutomationEventList(AutomationEventList *list);                           // Set automation event list to record to
RLAPI void SetAutomationEventBaseFrame(int frame);                                      // Set automation event internal base frame to start recording
RLAPI void StartAutomationEventRecording(void);                                         // Start recording automation events (AutomationEventList must be set)
RLAPI void StopAutomationEventRecording(void);                                          // Stop recording automation events
RLAPI void PlayAutomationEvent(AutomationEvent event);                                  // Play a recorded automation event

//------------------------------------------------------------------------------------
// Input Handling Functions (Module: core)
//------------------------------------------------------------------------------------

// Input-related functions: keyboard
RLAPI bool IsKeyPressed(int key);                             // Check if a key has been pressed once
RLAPI bool IsKeyPressedRepeat(int key);                       // Check if a key has been pressed again
RLAPI bool IsKeyDown(int key);                                // Check if a key is being pressed
RLAPI bool IsKeyReleased(int key);                            // Check if a key has been released once
RLAPI bool IsKeyUp(int key);                                  // Check if a key is NOT being pressed
RLAPI int GetKeyPressed(void);                                // Get key pressed (keycode), call it multiple times for keys queued, returns 0 when the queue is empty
RLAPI int GetCharPressed(void);                               // Get char pressed (unicode), call it multiple times for chars queued, returns 0 when the queue is empty
RLAPI void SetExitKey(int key);                               // Set a custom key to exit program (default is ESC)

// Input-related functions: gamepads
RLAPI bool IsGamepadAvailable(int gamepad);                                        // Check if a gamepad is available
RLAPI const char *GetGamepadName(int gamepad);                                     // Get gamepad internal name id
RLAPI bool IsGamepadButtonPressed(int gamepad, int button);                        // Check if a gamepad button has been pressed once
RLAPI bool IsGamepadButtonDown(int gamepad, int button);                           // Check if a gamepad button is being pressed
RLAPI bool IsGamepadButtonReleased(int gamepad, int button);                       // Check if a gamepad button has been released once
RLAPI bool IsGamepadButtonUp(int gamepad, int button);                             // Check if a gamepad button is NOT being pressed
RLAPI int GetGamepadButtonPressed(void);                                           // Get the last gamepad button pressed
RLAPI int GetGamepadAxisCount(int gamepad);                                        // Get gamepad axis count for a gamepad
RLAPI float GetGamepadAxisMovement(int gamepad, int axis);                         // Get axis movement value for a gamepad axis
RLAPI int SetGamepadMappings(const char *mappings);                                // Set internal gamepad mappings (SDL_GameControllerDB)
RLAPI void SetGamepadVibration(int gamepad, float leftMotor, float rightMotor, float duration); // Set gamepad vibration for both motors (duration in seconds)

// Input-related functions: mouse
RLAPI bool IsMouseButtonPressed(int button);                  // Check if a mouse button has been pressed once
RLAPI bool IsMouseButtonDown(int button);                     // Check if a mouse button is being pressed
RLAPI bool IsMouseButtonReleased(int button);                 // Check if a mouse button has been released once
RLAPI bool IsMouseButtonUp(int button);                       // Check if a mouse button is NOT being pressed
RLAPI int GetMouseX(void);                                    // Get mouse position X
RLAPI int GetMouseY(void);                                    // Get mouse position Y
RLAPI Vector2 GetMousePosition(void);                         // Get mouse position XY
RLAPI Vector2 GetMouseDelta(void);                            // Get mouse delta between frames
RLAPI void SetMousePosition(int x, int y);                    // Set mouse position XY
RLAPI void SetMouseOffset(int offsetX, int offsetY);          // Set mouse offset
RLAPI void SetMouseScale(float scaleX, float scaleY);         // Set mouse scaling
RLAPI float GetMouseWheelMove(void);                          // Get mouse wheel movement for X or Y, whichever is larger
RLAPI Vector2 GetMouseWheelMoveV(void);                       // Get mouse wheel movement for both X and Y
RLAPI void SetMouseCursor(int cursor);                        // Set mouse cursor

// Input-related functions: touch
RLAPI int GetTouchX(void);                                    // Get touch position X for touch point 0 (relative to screen size)
RLAPI int GetTouchY(void);                                    // Get touch position Y for touch point 0 (relative to screen size)
RLAPI Vector2 GetTouchPosition(int index);                    // Get touch position XY for a touch point index (relative to screen size)
RLAPI int GetTouchPointId(int index);                         // Get touch point identifier for given index
RLAPI int GetTouchPointCount(void);                           // Get number of touch points

//------------------------------------------------------------------------------------
// Gestures and Touch Handling Functions (Module: rgestures)
//------------------------------------------------------------------------------------
RLAPI void SetGesturesEnabled(unsigned int flags);      // Enable a set of gestures using flags
RLAPI bool IsGestureDetected(unsigned int gesture);     // Check if a gesture have been detected
RLAPI int GetGestureDetected(void);                     // Get latest detected gesture
RLAPI float GetGestureHoldDuration(void);               // Get gesture hold time in seconds
RLAPI Vector2 GetGestureDragVector(void);               // Get gesture drag vector
RLAPI float GetGestureDragAngle(void);                  // Get gesture drag angle
RLAPI Vector2 GetGesturePinchVector(void);              // Get gesture pinch delta
RLAPI float GetGesturePinchAngle(void);                 // Get gesture pinch angle

//------------------------------------------------------------------------------------
// Camera System Functions (Module: rcamera)
//------------------------------------------------------------------------------------
RLAPI void UpdateCamera(Camera *camera, int mode);      // Update camera position for selected mode
RLAPI void UpdateCameraPro(Camera *camera, Vector3 movement, Vector3 rotation, float zoom); // Update camera movement/rotation

//------------------------------------------------------------------------------------
// Basic Shapes Drawing Functions (Module: shapes)
//------------------------------------------------------------------------------------
// Set texture and rectangle to be used on shapes drawing
// NOTE: It can be useful when using basic shapes and one single font,
// defining a font char white rectangle would allow drawing everything in a single draw call
RLAPI void SetShapesTexture(Texture2D texture, Rectangle source);       // Set texture and rectangle to be used on shapes drawing
RLAPI Texture2D GetShapesTexture(void);                                 // Get texture that is used for shapes drawing
RLAPI Rectangle GetShapesTextureRectangle(void);                        // Get texture source rectangle that is used for shapes drawing

// Basic shapes drawing functions
RLAPI void DrawPixel(int posX, int posY, Color color);                                                   // Draw a pixel using geometry [Can be slow, use with care]
RLAPI void DrawPixelV(Vector2 position, Color color);                                                    // Draw a pixel using geometry (Vector version) [Can be slow, use with care]
RLAPI void DrawLine(int startPosX, int startPosY, int endPosX, int endPosY, Color color);                // Draw a line
RLAPI void DrawLineV(Vector2 startPos, Vector2 endPos, Color color);                                     // Draw a line (using gl lines)
RLAPI void DrawLineEx(Vector2 startPos, Vector2 endPos, float thick, Color color);                       // Draw a line (using triangles/quads)
RLAPI void DrawLineStrip(const Vector2 *points, int pointCount, Color color);                            // Draw lines sequence (using gl lines)
RLAPI void DrawLineBezier(Vector2 startPos, Vector2 endPos, float thick, Color color);                   // Draw line segment cubic-bezier in-out interpolation
RLAPI void DrawCircle(int centerX, int centerY, float radius, Color color);                              // Draw a color-filled circle
RLAPI void DrawCircleSector(Vector2 center, float radius, float startAngle, float endAngle, int segments, Color color);      // Draw a piece of a circle
RLAPI void DrawCircleSectorLines(Vector2 center, float radius, float startAngle, float endAngle, int segments, Color color); // Draw circle sector outline
RLAPI void DrawCircleGradient(int centerX, int centerY, float radius, Color inner, Color outer);         // Draw a gradient-filled circle
RLAPI void DrawCircleV(Vector2 center, float radius, Color color);                                       // Draw a color-filled circle (Vector version)
RLAPI void DrawCircleLines(int centerX, int centerY, float radius, Color color);                         // Draw circle outline
RLAPI void DrawCircleLinesV(Vector2 center, float radius, Color color);                                  // Draw circle outline (Vector version)
RLAPI void DrawEllipse(int centerX, int centerY, float radiusH, float radiusV, Color color);             // Draw ellipse
RLAPI void DrawEllipseLines(int centerX, int centerY, float radiusH, float radiusV, Color color);        // Draw ellipse outline
RLAPI void DrawRing(Vector2 center, float innerRadius, float outerRadius, float startAngle, float endAngle, int segments, Color color); // Draw ring
RLAPI void DrawRingLines(Vector2 center, float innerRadius, float outerRadius, float startAngle, float endAngle, int segments, Color color);    // Draw ring outline
RLAPI void DrawRectangle(int posX, int posY, int width, int height, Color color);                        // Draw a color-filled rectangle
RLAPI void DrawRectangleV(Vector2 position, Vector2 size, Color color);                                  // Draw a color-filled rectangle (Vector version)
RLAPI void DrawRectangleRec(Rectangle rec, Color color);                                                 // Draw a color-filled rectangle
RLAPI void DrawRectanglePro(Rectangle rec, Vector2 origin, float rotation, Color color);                 // Draw a color-filled rectangle with pro parameters
RLAPI void DrawRectangleGradientV(int posX, int posY, int width, int height, Color top, Color bottom);   // Draw a vertical-gradient-filled rectangle
RLAPI void DrawRectangleGradientH(int posX, int posY, int width, int height, Color left, Color right);   // Draw a horizontal-gradient-filled rectangle
RLAPI void DrawRectangleGradientEx(Rectangle rec, Color topLeft, Color bottomLeft, Color topRight, Color bottomRight); // Draw a gradient-filled rectangle with custom vertex colors
RLAPI void DrawRectangleLines(int posX, int posY, int width, int height, Color color);                   // Draw rectangle outline
RLAPI void DrawRectangleLinesEx(Rectangle rec, float lineThick, Color color);                            // Draw rectangle outline with extended parameters
RLAPI void DrawRectangleRounded(Rectangle rec, float roundness, int segments, Color color);              // Draw rectangle with rounded edges
RLAPI void DrawRectangleRoundedLines(Rectangle rec, float roundness, int segments, Color color);         // Draw rectangle lines with rounded edges
RLAPI void DrawRectangleRoundedLinesEx(Rectangle rec, float roundness, int segments, float lineThick, Color color); // Draw rectangle with rounded edges outline
RLAPI void DrawTriangle(Vector2 v1, Vector2 v2, Vector2 v3, Color color);                                // Draw a color-filled triangle (vertex in counter-clockwise order!)
RLAPI void DrawTriangleLines(Vector2 v1, Vector2 v2, Vector2 v3, Color color);                           // Draw triangle outline (vertex in counter-clockwise order!)
RLAPI void DrawTriangleFan(const Vector2 *points, int pointCount, Color color);                          // Draw a triangle fan defined by points (first vertex is the center)
RLAPI void DrawTriangleStrip(const Vector2 *points, int pointCount, Color color);                        // Draw a triangle strip defined by points
RLAPI void DrawPoly(Vector2 center, int sides, float radius, float rotation, Color color);               // Draw a regular polygon (Vector version)
RLAPI void DrawPolyLines(Vector2 center, int sides, float radius, float rotation, Color color);          // Draw a polygon outline of n sides
RLAPI void DrawPolyLinesEx(Vector2 center, int sides, float radius, float rotation, float lineThick, Color color); // Draw a polygon outline of n sides with extended parameters

// Splines drawing functions
RLAPI void DrawSplineLinear(const Vector2 *points, int pointCount, float thick, Color color);                  // Draw spline: Linear, minimum 2 points
RLAPI void DrawSplineBasis(const Vector2 *points, int pointCount, float thick, Color color);                   // Draw spline: B-Spline, minimum 4 points
RLAPI void DrawSplineCatmullRom(const Vector2 *points, int pointCount, float thick, Color color);              // Draw spline: Catmull-Rom, minimum 4 points
RLAPI void DrawSplineBezierQuadratic(const Vector2 *points, int pointCount, float thick, Color color);         // Draw spline: Quadratic Bezier, minimum 3 points (1 control point): [p1, c2, p3, c4...]
RLAPI void DrawSplineBezierCubic(const Vector2 *points, int pointCount, float thick, Color color);             // Draw spline: Cubic Bezier, minimum 4 points (2 control points): [p1, c2, c3, p4, c5, c6...]
RLAPI void DrawSplineSegmentLinear(Vector2 p1, Vector2 p2, float thick, Color color);                    // Draw spline segment: Linear, 2 points
RLAPI void DrawSplineSegmentBasis(Vector2 p1, Vector2 p2, Vector2 p3, Vector2 p4, float thick, Color color); // Draw spline segment: B-Spline, 4 points
RLAPI void DrawSplineSegmentCatmullRom(Vector2 p1, Vector2 p2, Vector2 p3, Vector2 p4, float thick, Color color); // Draw spline segment: Catmull-Rom, 4 points
RLAPI void DrawSplineSegmentBezierQuadratic(Vector2 p1, Vector2 c2, Vector2 p3, float thick, Color color); // Draw spline segment: Quadratic Bezier, 2 points, 1 control point
RLAPI void DrawSplineSegmentBezierCubic(Vector2 p1, Vector2 c2, Vector2 c3, Vector2 p4, float thick, Color color); // Draw spline segment: Cubic Bezier, 2 points, 2 control points

// Spline segment point evaluation functions, for a given t [0.0f .. 1.0f]
RLAPI Vector2 GetSplinePointLinear(Vector2 startPos, Vector2 endPos, float t);                           // Get (evaluate) spline point: Linear
RLAPI Vector2 GetSplinePointBasis(Vector2 p1, Vector2 p2, Vector2 p3, Vector2 p4, float t);              // Get (evaluate) spline point: B-Spline
RLAPI Vector2 GetSplinePointCatmullRom(Vector2 p1, Vector2 p2, Vector2 p3, Vector2 p4, float t);         // Get (evaluate) spline point: Catmull-Rom
RLAPI Vector2 GetSplinePointBezierQuad(Vector2 p1, Vector2 c2, Vector2 p3, float t);                     // Get (evaluate) spline point: Quadratic Bezier
RLAPI Vector2 GetSplinePointBezierCubic(Vector2 p1, Vector2 c2, Vector2 c3, Vector2 p4, float t);        // Get (evaluate) spline point: Cubic Bezier

// Basic shapes collision detection functions
RLAPI bool CheckCollisionRecs(Rectangle rec1, Rectangle rec2);                                           // Check collision between two rectangles
RLAPI bool CheckCollisionCircles(Vector2 center1, float radius1, Vector2 center2, float radius2);        // Check collision between two circles
RLAPI bool CheckCollisionCircleRec(Vector2 center, float radius, Rectangle rec);                         // Check collision between circle and rectangle
RLAPI bool CheckCollisionCircleLine(Vector2 center, float radius, Vector2 p1, Vector2 p2);               // Check if circle collides with a line created betweeen two points [p1] and [p2]
RLAPI bool CheckCollisionPointRec(Vector2 point, Rectangle rec);                                         // Check if point is inside rectangle
RLAPI bool CheckCollisionPointCircle(Vector2 point, Vector2 center, float radius);                       // Check if point is inside circle
RLAPI bool CheckCollisionPointTriangle(Vector2 point, Vector2 p1, Vector2 p2, Vector2 p3);               // Check if point is inside a triangle
RLAPI bool CheckCollisionPointLine(Vector2 point, Vector2 p1, Vector2 p2, int threshold);                // Check if point belongs to line created between two points [p1] and [p2] with defined margin in pixels [threshold]
RLAPI bool CheckCollisionPointPoly(Vector2 point, const Vector2 *points, int pointCount);                // Check if point is within a polygon described by array of vertices
RLAPI bool CheckCollisionLines(Vector2 startPos1, Vector2 endPos1, Vector2 startPos2, Vector2 endPos2, Vector2 *collisionPoint); // Check the collision between two lines defined by two points each, returns collision point by reference
RLAPI Rectangle GetCollisionRec(Rectangle rec1, Rectangle rec2);                                         // Get collision rectangle for two rectangles collision

//------------------------------------------------------------------------------------
// Texture Loading and Drawing Functions (Module: textures)
//------------------------------------------------------------------------------------

// Image loading functions
// NOTE: These functions do not require GPU access
RLAPI Image LoadImage(const char *fileName);                                                             // Load image from file into CPU memory (RAM)
RLAPI Image LoadImageRaw(const char *fileName, int width, int height, int format, int headerSize);       // Load image from RAW file data
RLAPI Image LoadImageAnim(const char *fileName, int *frames);                                            // Load image sequence from file (frames appended to image.data)
RLAPI Image LoadImageAnimFromMemory(const char *fileType, const unsigned char *fileData, int dataSize, int *frames); // Load image sequence from memory buffer
RLAPI Image LoadImageFromMemory(const char *fileType, const unsigned char *fileData, int dataSize);      // Load image from memory buffer, fileType refers to extension: i.e. '.png'
RLAPI Image LoadImageFromTexture(Texture2D texture);                                                     // Load image from GPU texture data
RLAPI Image LoadImageFromScreen(void);                                                                   // Load image from screen buffer and (screenshot)
RLAPI bool IsImageValid(Image image);                                                                    // Check if an image is valid (data and parameters)
RLAPI void UnloadImage(Image image);                                                                     // Unload image from CPU memory (RAM)
RLAPI bool ExportImage(Image image, const char *fileName);                                               // Export image data to file, returns true on success
RLAPI unsigned char *ExportImageToMemory(Image image, const char *fileType, int *fileSize);              // Export image to memory buffer
RLAPI bool ExportImageAsCode(Image image, const char *fileName);                                         // Export image as code file defining an array of bytes, returns true on success

// Image generation functions
RLAPI Image GenImageColor(int width, int height, Color color);                                           // Generate image: plain color
RLAPI Image GenImageGradientLinear(int width, int height, int direction, Color start, Color end);        // Generate image: linear gradient, direction in degrees [0..360], 0=Vertical gradient
RLAPI Image GenImageGradientRadial(int width, int height, float density, Color inner, Color outer);      // Generate image: radial gradient
RLAPI Image GenImageGradientSquare(int width, int height, float density, Color inner, Color outer);      // Generate image: square gradient
RLAPI Image GenImageChecked(int width, int height, int checksX, int checksY, Color col1, Color col2);    // Generate image: checked
RLAPI Image GenImageWhiteNoise(int width, int height, float factor);                                     // Generate image: white noise
RLAPI Image GenImagePerlinNoise(int width, int height, int offsetX, int offsetY, float scale);           // Generate image: perlin noise
RLAPI Image GenImageCellular(int width, int height, int tileSize);                                       // Generate image: cellular algorithm, bigger tileSize means bigger cells
RLAPI Image GenImageText(int width, int height, const char *text);                                       // Generate image: grayscale image from text data

// Image manipulation functions
RLAPI Image ImageCopy(Image image);                                                                      // Create an image duplicate (useful for transformations)
RLAPI Image ImageFromImage(Image image, Rectangle rec);                                                  // Create an image from another image piece
RLAPI Image ImageFromChannel(Image image, int selectedChannel);                                          // Create an image from a selected channel of another image (GRAYSCALE)
RLAPI Image ImageText(const char *text, int fontSize, Color color);                                      // Create an image from text (default font)
RLAPI Image ImageTextEx(Font font, const char *text, float fontSize, float spacing, Color tint);         // Create an image from text (custom sprite font)
RLAPI void ImageFormat(Image *image, int newFormat);                                                     // Convert image data to desired format
RLAPI void ImageToPOT(Image *image, Color fill);                                                         // Convert image to POT (power-of-two)
RLAPI void ImageCrop(Image *image, Rectangle crop);                                                      // Crop an image to a defined rectangle
RLAPI void ImageAlphaCrop(Image *image, float threshold);                                                // Crop image depending on alpha value
RLAPI void ImageAlphaClear(Image *image, Color color, float threshold);                                  // Clear alpha channel to desired color
RLAPI void ImageAlphaMask(Image *image, Image alphaMask);                                                // Apply alpha mask to image
RLAPI void ImageAlphaPremultiply(Image *image);                                                          // Premultiply alpha channel
RLAPI void ImageBlurGaussian(Image *image, int blurSize);                                                // Apply Gaussian blur using a box blur approximation
RLAPI void ImageKernelConvolution(Image *image, const float *kernel, int kernelSize);                    // Apply custom square convolution kernel to image
RLAPI void ImageResize(Image *image, int newWidth, int newHeight);                                       // Resize image (Bicubic scaling algorithm)
RLAPI void ImageResizeNN(Image *image, int newWidth,int newHeight);                                      // Resize image (Nearest-Neighbor scaling algorithm)
RLAPI void ImageResizeCanvas(Image *image, int newWidth, int newHeight, int offsetX, int offsetY, Color fill); // Resize canvas and fill with color
RLAPI void ImageMipmaps(Image *image);                                                                   // Compute all mipmap levels for a provided image
RLAPI void ImageDither(Image *image, int rBpp, int gBpp, int bBpp, int aBpp);                            // Dither image data to 16bpp or lower (Floyd-Steinberg dithering)
RLAPI void ImageFlipVertical(Image *image);                                                              // Flip image vertically
RLAPI void ImageFlipHorizontal(Image *image);                                                            // Flip image horizontally
RLAPI void ImageRotate(Image *image, int degrees);                                                       // Rotate image by input angle in degrees (-359 to 359)
RLAPI void ImageRotateCW(Image *image);                                                                  // Rotate image clockwise 90deg
RLAPI void ImageRotateCCW(Image *image);                                                                 // Rotate image counter-clockwise 90deg
RLAPI void ImageColorTint(Image *image, Color color);                                                    // Modify image color: tint
RLAPI void ImageColorInvert(Image *image);                                                               // Modify image color: invert
RLAPI void ImageColorGrayscale(Image *image);                                                            // Modify image color: grayscale
RLAPI void ImageColorContrast(Image *image, float contrast);                                             // Modify image color: contrast (-100 to 100)
RLAPI void ImageColorBrightness(Image *image, int brightness);                                           // Modify image color: brightness (-255 to 255)
RLAPI void ImageColorReplace(Image *image, Color color, Color replace);                                  // Modify image color: replace color
RLAPI Color *LoadImageColors(Image image);                                                               // Load color data from image as a Color array (RGBA - 32bit)
RLAPI Color *LoadImagePalette(Image image, int maxPaletteSize, int *colorCount);                         // Load colors palette from image as a Color array (RGBA - 32bit)
RLAPI void UnloadImageColors(Color *colors);                                                             // Unload color data loaded with LoadImageColors()
RLAPI void UnloadImagePalette(Color *colors);                                                            // Unload colors palette loaded with LoadImagePalette()
RLAPI Rectangle GetImageAlphaBorder(Image image, float threshold);                                       // Get image alpha border rectangle
RLAPI Color GetImageColor(Image image, int x, int y);                                                    // Get image pixel color at (x, y) position

// Image drawing functions
// NOTE: Image software-rendering functions (CPU)
RLAPI void ImageClearBackground(Image *dst, Color color);                                                // Clear image background with given color
RLAPI void ImageDrawPixel(Image *dst, int posX, int posY, Color color);                                  // Draw pixel within an image
RLAPI void ImageDrawPixelV(Image *dst, Vector2 position, Color color);                                   // Draw pixel within an image (Vector version)
RLAPI void ImageDrawLine(Image *dst, int startPosX, int startPosY, int endPosX, int endPosY, Color color); // Draw line within an image
RLAPI void ImageDrawLineV(Image *dst, Vector2 start, Vector2 end, Color color);                          // Draw line within an image (Vector version)
RLAPI void ImageDrawLineEx(Image *dst, Vector2 start, Vector2 end, int thick, Color color);              // Draw a line defining thickness within an image
RLAPI void ImageDrawCircle(Image *dst, int centerX, int centerY, int radius, Color color);               // Draw a filled circle within an image
RLAPI void ImageDrawCircleV(Image *dst, Vector2 center, int radius, Color color);                        // Draw a filled circle within an image (Vector version)
RLAPI void ImageDrawCircleLines(Image *dst, int centerX, int centerY, int radius, Color color);          // Draw circle outline within an image
RLAPI void ImageDrawCircleLinesV(Image *dst, Vector2 center, int radius, Color color);                   // Draw circle outline within an image (Vector version)
RLAPI void ImageDrawRectangle(Image *dst, int posX, int posY, int width, int height, Color color);       // Draw rectangle within an image
RLAPI void ImageDrawRectangleV(Image *dst, Vector2 position, Vector2 size, Color color);                 // Draw rectangle within an image (Vector version)
RLAPI void ImageDrawRectangleRec(Image *dst, Rectangle rec, Color color);                                // Draw rectangle within an image
RLAPI void ImageDrawRectangleLines(Image *dst, Rectangle rec, int thick, Color color);                   // Draw rectangle lines within an image
RLAPI void ImageDrawTriangle(Image *dst, Vector2 v1, Vector2 v2, Vector2 v3, Color color);               // Draw triangle within an image
RLAPI void ImageDrawTriangleEx(Image *dst, Vector2 v1, Vector2 v2, Vector2 v3, Color c1, Color c2, Color c3); // Draw triangle with interpolated colors within an image
RLAPI void ImageDrawTriangleLines(Image *dst, Vector2 v1, Vector2 v2, Vector2 v3, Color color);          // Draw triangle outline within an image
RLAPI void ImageDrawTriangleFan(Image *dst, Vector2 *points, int pointCount, Color color);               // Draw a triangle fan defined by points within an image (first vertex is the center)
RLAPI void ImageDrawTriangleStrip(Image *dst, Vector2 *points, int pointCount, Color color);             // Draw a triangle strip defined by points within an image
RLAPI void ImageDraw(Image *dst, Image src, Rectangle srcRec, Rectangle dstRec, Color tint);             // Draw a source image within a destination image (tint applied to source)
RLAPI void ImageDrawText(Image *dst, const char *text, int posX, int posY, int fontSize, Color color);   // Draw text (using default font) within an image (destination)
RLAPI void ImageDrawTextEx(Image *dst, Font font, const char *text, Vector2 position, float fontSize, float spacing, Color tint); // Draw text (custom sprite font) within an image (destination)

// Texture loading functions
// NOTE: These functions require GPU access
RLAPI Texture2D LoadTexture(const char *fileName);                                                       // Load texture from file into GPU memory (VRAM)
RLAPI Texture2D LoadTextureFromImage(Image image);                                                       // Load texture from image data
RLAPI TextureCubemap LoadTextureCubemap(Image image, int layout);                                        // Load cubemap from image, multiple image cubemap layouts supported
RLAPI RenderTexture2D LoadRenderTexture(int width, int height);                                          // Load texture for rendering (framebuffer)
RLAPI bool IsTextureValid(Texture2D texture);                                                            // Check if a texture is valid (loaded in GPU)
RLAPI void UnloadTexture(Texture2D texture);                                                             // Unload texture from GPU memory (VRAM)
RLAPI bool IsRenderTextureValid(RenderTexture2D target);                                                 // Check if a render texture is valid (loaded in GPU)
RLAPI void UnloadRenderTexture(RenderTexture2D target);                                                  // Unload render texture from GPU memory (VRAM)
RLAPI void UpdateTexture(Texture2D texture, const void *pixels);                                         // Update GPU texture with new data
RLAPI void UpdateTextureRec(Texture2D texture, Rectangle rec, const void *pixels);                       // Update GPU texture rectangle with new data

// Texture configuration functions
RLAPI void GenTextureMipmaps(Texture2D *texture);                                                        // Generate GPU mipmaps for a texture
RLAPI void SetTextureFilter(Texture2D texture, int filter);                                              // Set texture scaling filter mode
RLAPI void SetTextureWrap(Texture2D texture, int wrap);                                                  // Set texture wrapping mode

// Texture drawing functions
RLAPI void DrawTexture(Texture2D texture, int posX, int posY, Color tint);                               // Draw a Texture2D
RLAPI void DrawTextureV(Texture2D texture, Vector2 position, Color tint);                                // Draw a Texture2D with position defined as Vector2
RLAPI void DrawTextureEx(Texture2D texture, Vector2 position, float rotation, float scale, Color tint);  // Draw a Texture2D with extended parameters
RLAPI void DrawTextureRec(Texture2D texture, Rectangle source, Vector2 position, Color tint);            // Draw a part of a texture defined by a rectangle
RLAPI void DrawTexturePro(Texture2D texture, Rectangle source, Rectangle dest, Vector2 origin, float rotation, Color tint); // Draw a part of a texture defined by a rectangle with 'pro' parameters
RLAPI void DrawTextureNPatch(Texture2D texture, NPatchInfo nPatchInfo, Rectangle dest, Vector2 origin, float rotation, Color tint); // Draws a texture (or part of it) that stretches or shrinks nicely

// Color/pixel related functions
RLAPI bool ColorIsEqual(Color col1, Color col2);                            // Check if two colors are equal
RLAPI Color Fade(Color color, float alpha);                                 // Get color with alpha applied, alpha goes from 0.0f to 1.0f
RLAPI int ColorToInt(Color color);                                          // Get hexadecimal value for a Color (0xRRGGBBAA)
RLAPI Vector4 ColorNormalize(Color color);                                  // Get Color normalized as float [0..1]
RLAPI Color ColorFromNormalized(Vector4 normalized);                        // Get Color from normalized values [0..1]
RLAPI Vector3 ColorToHSV(Color color);                                      // Get HSV values for a Color, hue [0..360], saturation/value [0..1]
RLAPI Color ColorFromHSV(float hue, float saturation, float value);         // Get a Color from HSV values, hue [0..360], saturation/value [0..1]
RLAPI Color ColorTint(Color color, Color tint);                             // Get color multiplied with another color
RLAPI Color ColorBrightness(Color color, float factor);                     // Get color with brightness correction, brightness factor goes from -1.0f to 1.0f
RLAPI Color ColorContrast(Color color, float contrast);                     // Get color with contrast correction, contrast values between -1.0f and 1.0f
RLAPI Color ColorAlpha(Color color, float alpha);                           // Get color with alpha applied, alpha goes from 0.0f to 1.0f
RLAPI Color ColorAlphaBlend(Color dst, Color src, Color tint);              // Get src alpha-blended into dst color with tint
RLAPI Color ColorLerp(Color color1, Color color2, float factor);            // Get color lerp interpolation between two colors, factor [0.0f..1.0f]
RLAPI Color GetColor(unsigned int hexValue);                                // Get Color structure from hexadecimal value
RLAPI Color GetPixelColor(void *srcPtr, int format);                        // Get Color from a source pixel pointer of certain format
RLAPI void SetPixelColor(void *dstPtr, Color color, int format);            // Set color formatted into destination pixel pointer
RLAPI int GetPixelDataSize(int width, int height, int format);              // Get pixel data size in bytes for certain format

//------------------------------------------------------------------------------------
// Font Loading and Text Drawing Functions (Module: text)
//------------------------------------------------------------------------------------

// Font loading/unloading functions
RLAPI Font GetFontDefault(void);                                                            // Get the default Font
RLAPI Font LoadFont(const char *fileName);                                                  // Load font from file into GPU memory (VRAM)
RLAPI Font LoadFontEx(const char *fileName, int fontSize, int *codepoints, int codepointCount); // Load font from file with extended parameters, use NULL for codepoints and 0 for codepointCount to load the default character set, font size is provided in pixels height
RLAPI Font LoadFontFromImage(Image image, Color key, int firstChar);                        // Load font from Image (XNA style)
RLAPI Font LoadFontFromMemory(const char *fileType, const unsigned char *fileData, int dataSize, int fontSize, int *codepoints, int codepointCount); // Load font from memory buffer, fileType refers to extension: i.e. '.ttf'
RLAPI bool IsFontValid(Font font);                                                          // Check if a font is valid (font data loaded, WARNING: GPU texture not checked)
RLAPI GlyphInfo *LoadFontData(const unsigned char *fileData, int dataSize, int fontSize, int *codepoints, int codepointCount, int type); // Load font data for further use
RLAPI Image GenImageFontAtlas(const GlyphInfo *glyphs, Rectangle **glyphRecs, int glyphCount, int fontSize, int padding, int packMethod); // Generate image font atlas using chars info
RLAPI void UnloadFontData(GlyphInfo *glyphs, int glyphCount);                               // Unload font chars info data (RAM)
RLAPI void UnloadFont(Font font);                                                           // Unload font from GPU memory (VRAM)
RLAPI bool ExportFontAsCode(Font font, const char *fileName);                               // Export font as code file, returns true on success

// Text drawing functions
RLAPI void DrawFPS(int posX, int posY);                                                     // Draw current FPS
RLAPI void DrawText(const char *text, int posX, int posY, int fontSize, Color color);       // Draw text (using default font)
RLAPI void DrawTextEx(Font font, const char *text, Vector2 position, float fontSize, float spacing, Color tint); // Draw text using font and additional parameters
RLAPI void DrawTextPro(Font font, const char *text, Vector2 position, Vector2 origin, float rotation, float fontSize, float spacing, Color tint); // Draw text using Font and pro parameters (rotation)
RLAPI void DrawTextCodepoint(Font font, int codepoint, Vector2 position, float fontSize, Color tint); // Draw one character (codepoint)
RLAPI void DrawTextCodepoints(Font font, const int *codepoints, int codepointCount, Vector2 position, float fontSize, float spacing, Color tint); // Draw multiple character (codepoint)

// Text font info functions
RLAPI void SetTextLineSpacing(int spacing);                                                 // Set vertical line spacing when drawing with line-breaks
RLAPI int MeasureText(const char *text, int fontSize);                                      // Measure string width for default font
RLAPI Vector2 MeasureTextEx(Font font, const char *text, float fontSize, float spacing);    // Measure string size for Font
RLAPI int GetGlyphIndex(Font font, int codepoint);                                          // Get glyph index position in font for a codepoint (unicode character), fallback to '?' if not found
RLAPI GlyphInfo GetGlyphInfo(Font font, int codepoint);                                     // Get glyph font info data for a codepoint (unicode character), fallback to '?' if not found
RLAPI Rectangle GetGlyphAtlasRec(Font font, int codepoint);                                 // Get glyph rectangle in font atlas for a codepoint (unicode character), fallback to '?' if not found

// Text codepoints management functions (unicode characters)
RLAPI char *LoadUTF8(const int *codepoints, int length);                // Load UTF-8 text encoded from codepoints array
RLAPI void UnloadUTF8(char *text);                                      // Unload UTF-8 text encoded from codepoints array
RLAPI int *LoadCodepoints(const char *text, int *count);                // Load all codepoints from a UTF-8 text string, codepoints count returned by parameter
RLAPI void UnloadCodepoints(int *codepoints);                           // Unload codepoints data from memory
RLAPI int GetCodepointCount(const char *text);                          // Get total number of codepoints in a UTF-8 encoded string
RLAPI int GetCodepoint(const char *text, int *codepointSize);           // Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
RLAPI int GetCodepointNext(const char *text, int *codepointSize);       // Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
RLAPI int GetCodepointPrevious(const char *text, int *codepointSize);   // Get previous codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
RLAPI const char *CodepointToUTF8(int codepoint, int *utf8Size);        // Encode one codepoint into UTF-8 byte array (array length returned as parameter)

// Text strings management functions (no UTF-8 strings, only byte chars)
// NOTE: Some strings allocate memory internally for returned strings, just be careful!
RLAPI int TextCopy(char *dst, const char *src);                                             // Copy one string to another, returns bytes copied
RLAPI bool TextIsEqual(const char *text1, const char *text2);                               // Check if two text string are equal
RLAPI unsigned int TextLength(const char *text);                                            // Get text length, checks for '\0' ending
RLAPI const char *TextFormat(const char *text, ...);                                        // Text formatting with variables (sprintf() style)
RLAPI const char *TextSubtext(const char *text, int position, int length);                  // Get a piece of a text string
RLAPI char *TextReplace(const char *text, const char *replace, const char *by);             // Replace text string (WARNING: memory must be freed!)
RLAPI char *TextInsert(const char *text, const char *insert, int position);                 // Insert text in a position (WARNING: memory must be freed!)
RLAPI const char *TextJoin(const char **textList, int count, const char *delimiter);        // Join text strings with delimiter
RLAPI const char **TextSplit(const char *text, char delimiter, int *count);                 // Split text into multiple strings
RLAPI void TextAppend(char *text, const char *append, int *position);                       // Append text at specific position and move cursor!
RLAPI int TextFindIndex(const char *text, const char *find);                                // Find first text occurrence within a string
RLAPI const char *TextToUpper(const char *text);                      // Get upper case version of provided string
RLAPI const char *TextToLower(const char *text);                      // Get lower case version of provided string
RLAPI const char *TextToPascal(const char *text);                     // Get Pascal case notation version of provided string
RLAPI const char *TextToSnake(const char *text);                      // Get Snake case notation version of provided string
RLAPI const char *TextToCamel(const char *text);                      // Get Camel case notation version of provided string

RLAPI int TextToInteger(const char *text);                            // Get integer value from text (negative values not supported)
RLAPI float TextToFloat(const char *text);                            // Get float value from text (negative values not supported)

//------------------------------------------------------------------------------------
// Basic 3d Shapes Drawing Functions (Module: models)
//------------------------------------------------------------------------------------

// Basic geometric 3D shapes drawing functions
RLAPI void DrawLine3D(Vector3 startPos, Vector3 endPos, Color color);                                    // Draw a line in 3D world space
RLAPI void DrawPoint3D(Vector3 position, Color color);                                                   // Draw a point in 3D space, actually a small line
RLAPI void DrawCircle3D(Vector3 center, float radius, Vector3 rotationAxis, float rotationAngle, Color color); // Draw a circle in 3D world space
RLAPI void DrawTriangle3D(Vector3 v1, Vector3 v2, Vector3 v3, Color color);                              // Draw a color-filled triangle (vertex in counter-clockwise order!)
RLAPI void DrawTriangleStrip3D(const Vector3 *points, int pointCount, Color color);                      // Draw a triangle strip defined by points
RLAPI void DrawCube(Vector3 position, float width, float height, float length, Color color);             // Draw cube
RLAPI void DrawCubeV(Vector3 position, Vector3 size, Color color);                                       // Draw cube (Vector version)
RLAPI void DrawCubeWires(Vector3 position, float width, float height, float length, Color color);        // Draw cube wires
RLAPI void DrawCubeWiresV(Vector3 position, Vector3 size, Color color);                                  // Draw cube wires (Vector version)
RLAPI void DrawSphere(Vector3 centerPos, float radius, Color color);                                     // Draw sphere
RLAPI void DrawSphereEx(Vector3 centerPos, float radius, int rings, int slices, Color color);            // Draw sphere with extended parameters
RLAPI void DrawSphereWires(Vector3 centerPos, float radius, int rings, int slices, Color color);         // Draw sphere wires
RLAPI void DrawCylinder(Vector3 position, float radiusTop, float radiusBottom, float height, int slices, Color color); // Draw a cylinder/cone
RLAPI void DrawCylinderEx(Vector3 startPos, Vector3 endPos, float startRadius, float endRadius, int sides, Color color); // Draw a cylinder with base at startPos and top at endPos
RLAPI void DrawCylinderWires(Vector3 position, float radiusTop, float radiusBottom, float height, int slices, Color color); // Draw a cylinder/cone wires
RLAPI void DrawCylinderWiresEx(Vector3 startPos, Vector3 endPos, float startRadius, float endRadius, int sides, Color color); // Draw a cylinder wires with base at startPos and top at endPos
RLAPI void DrawCapsule(Vector3 startPos, Vector3 endPos, float radius, int slices, int rings, Color color); // Draw a capsule with the center of its sphere caps at startPos and endPos
RLAPI void DrawCapsuleWires(Vector3 startPos, Vector3 endPos, float radius, int slices, int rings, Color color); // Draw capsule wireframe with the center of its sphere caps at startPos and endPos
RLAPI void DrawPlane(Vector3 centerPos, Vector2 size, Color color);                                      // Draw a plane XZ
RLAPI void DrawRay(Ray ray, Color color);                                                                // Draw a ray line
RLAPI void DrawGrid(int slices, float spacing);                                                          // Draw a grid (centered at (0, 0, 0))

//------------------------------------------------------------------------------------
// Model 3d Loading and Drawing Functions (Module: models)
//------------------------------------------------------------------------------------

// Model management functions
RLAPI Model LoadModel(const char *fileName);                                                // Load model from files (meshes and materials)
RLAPI Model LoadModelFromMesh(Mesh mesh);                                                   // Load model from generated mesh (default material)
RLAPI bool IsModelValid(Model model);                                                       // Check if a model is valid (loaded in GPU, VAO/VBOs)
RLAPI void UnloadModel(Model model);                                                        // Unload model (including meshes) from memory (RAM and/or VRAM)
RLAPI BoundingBox GetModelBoundingBox(Model model);                                         // Compute model bounding box limits (considers all meshes)

// Model drawing functions
RLAPI void DrawModel(Model model, Vector3 position, float scale, Color tint);               // Draw a model (with texture if set)
RLAPI void DrawModelEx(Model model, Vector3 position, Vector3 rotationAxis, float rotationAngle, Vector3 scale, Color tint); // Draw a model with extended parameters
RLAPI void DrawModelWires(Model model, Vector3 position, float scale, Color tint);          // Draw a model wires (with texture if set)
RLAPI void DrawModelWiresEx(Model model, Vector3 position, Vector3 rotationAxis, float rotationAngle, Vector3 scale, Color tint); // Draw a model wires (with texture if set) with extended parameters
RLAPI void DrawModelPoints(Model model, Vector3 position, float scale, Color tint); // Draw a model as points
RLAPI void DrawModelPointsEx(Model model, Vector3 position, Vector3 rotationAxis, float rotationAngle, Vector3 scale, Color tint); // Draw a model as points with extended parameters
RLAPI void DrawBoundingBox(BoundingBox box, Color color);                                   // Draw bounding box (wires)
RLAPI void DrawBillboard(Camera camera, Texture2D texture, Vector3 position, float scale, Color tint);   // Draw a billboard texture
RLAPI void DrawBillboardRec(Camera camera, Texture2D texture, Rectangle source, Vector3 position, Vector2 size, Color tint); // Draw a billboard texture defined by source
RLAPI void DrawBillboardPro(Camera camera, Texture2D texture, Rectangle source, Vector3 position, Vector3 up, Vector2 size, Vector2 origin, float rotation, Color tint); // Draw a billboard texture defined by source and rotation

// Mesh management functions
RLAPI void UploadMesh(Mesh *mesh, bool dynamic);                                            // Upload mesh vertex data in GPU and provide VAO/VBO ids
RLAPI void UpdateMeshBuffer(Mesh mesh, int index, const void *data, int dataSize, int offset); // Update mesh vertex data in GPU for a specific buffer index
RLAPI void UnloadMesh(Mesh mesh);                                                           // Unload mesh data from CPU and GPU
RLAPI void DrawMesh(Mesh mesh, Material material, Matrix transform);                        // Draw a 3d mesh with material and transform
RLAPI void DrawMeshInstanced(Mesh mesh, Material material, const Matrix *transforms, int instances); // Draw multiple mesh instances with material and different transforms
RLAPI BoundingBox GetMeshBoundingBox(Mesh mesh);                                            // Compute mesh bounding box limits
RLAPI void GenMeshTangents(Mesh *mesh);                                                     // Compute mesh tangents
RLAPI bool ExportMesh(Mesh mesh, const char *fileName);                                     // Export mesh data to file, returns true on success
RLAPI bool ExportMeshAsCode(Mesh mesh, const char *fileName);                               // Export mesh as code file (.h) defining multiple arrays of vertex attributes

// Mesh generation functions
RLAPI Mesh GenMeshPoly(int sides, float radius);                                            // Generate polygonal mesh
RLAPI Mesh GenMeshPlane(float width, float length, int resX, int resZ);                     // Generate plane mesh (with subdivisions)
RLAPI Mesh GenMeshCube(float width, float height, float length);                            // Generate cuboid mesh
RLAPI Mesh GenMeshSphere(float radius, int rings, int slices);                              // Generate sphere mesh (standard sphere)
RLAPI Mesh GenMeshHemiSphere(float radius, int rings, int slices);                          // Generate half-sphere mesh (no bottom cap)
RLAPI Mesh GenMeshCylinder(float radius, float height, int slices);                         // Generate cylinder mesh
RLAPI Mesh GenMeshCone(float radius, float height, int slices);                             // Generate cone/pyramid mesh
RLAPI Mesh GenMeshTorus(float radius, float size, int radSeg, int sides);                   // Generate torus mesh
RLAPI Mesh GenMeshKnot(float radius, float size, int radSeg, int sides);                    // Generate trefoil knot mesh
RLAPI Mesh GenMeshHeightmap(Image heightmap, Vector3 size);                                 // Generate heightmap mesh from image data
RLAPI Mesh GenMeshCubicmap(Image cubicmap, Vector3 cubeSize);                               // Generate cubes-based map mesh from image data

// Material loading/unloading functions
RLAPI Material *LoadMaterials(const char *fileName, int *materialCount);                    // Load materials from model file
RLAPI Material LoadMaterialDefault(void);                                                   // Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)
RLAPI bool IsMaterialValid(Material material);                                              // Check if a material is valid (shader assigned, map textures loaded in GPU)
RLAPI void UnloadMaterial(Material material);                                               // Unload material from GPU memory (VRAM)
RLAPI void SetMaterialTexture(Material *material, int mapType, Texture2D texture);          // Set texture for a material map type (MATERIAL_MAP_DIFFUSE, MATERIAL_MAP_SPECULAR...)
RLAPI void SetModelMeshMaterial(Model *model, int meshId, int materialId);                  // Set material for a mesh

// Model animations loading/unloading functions
RLAPI ModelAnimation *LoadModelAnimations(const char *fileName, int *animCount);            // Load model animations from file
RLAPI void UpdateModelAnimation(Model model, ModelAnimation anim, int frame);               // Update model animation pose (CPU)
RLAPI void UpdateModelAnimationBones(Model model, ModelAnimation anim, int frame);          // Update model animation mesh bone matrices (GPU skinning)
RLAPI void UnloadModelAnimation(ModelAnimation anim);                                       // Unload animation data
RLAPI void UnloadModelAnimations(ModelAnimation *animations, int animCount);                // Unload animation array data
RLAPI bool IsModelAnimationValid(Model model, ModelAnimation anim);                         // Check model animation skeleton match

// Collision detection functions
RLAPI bool CheckCollisionSpheres(Vector3 center1, float radius1, Vector3 center2, float radius2);   // Check collision between two spheres
RLAPI bool CheckCollisionBoxes(BoundingBox box1, BoundingBox box2);                                 // Check collision between two bounding boxes
RLAPI bool CheckCollisionBoxSphere(BoundingBox box, Vector3 center, float radius);                  // Check collision between box and sphere
RLAPI RayCollision GetRayCollisionSphere(Ray ray, Vector3 center, float radius);                    // Get collision info between ray and sphere
RLAPI RayCollision GetRayCollisionBox(Ray ray, BoundingBox box);                                    // Get collision info between ray and box
RLAPI RayCollision GetRayCollisionMesh(Ray ray, Mesh mesh, Matrix transform);                       // Get collision info between ray and mesh
RLAPI RayCollision GetRayCollisionTriangle(Ray ray, Vector3 p1, Vector3 p2, Vector3 p3);            // Get collision info between ray and triangle
RLAPI RayCollision GetRayCollisionQuad(Ray ray, Vector3 p1, Vector3 p2, Vector3 p3, Vector3 p4);    // Get collision info between ray and quad

//------------------------------------------------------------------------------------
// Audio Loading and Playing Functions (Module: audio)
//------------------------------------------------------------------------------------
typedef void (*AudioCallback)(void *bufferData, unsigned int frames);

// Audio device management functions
RLAPI void InitAudioDevice(void);                                     // Initialize audio device and context
RLAPI void CloseAudioDevice(void);                                    // Close the audio device and context
RLAPI bool IsAudioDeviceReady(void);                                  // Check if audio device has been initialized successfully
RLAPI void SetMasterVolume(float volume);                             // Set master volume (listener)
RLAPI float GetMasterVolume(void);                                    // Get master volume (listener)

// Wave/Sound loading/unloading functions
RLAPI Wave LoadWave(const char *fileName);                            // Load wave data from file
RLAPI Wave LoadWaveFromMemory(const char *fileType, const unsigned char *fileData, int dataSize); // Load wave from memory buffer, fileType refers to extension: i.e. '.wav'
RLAPI bool IsWaveValid(Wave wave);                                    // Checks if wave data is valid (data loaded and parameters)
RLAPI Sound LoadSound(const char *fileName);                          // Load sound from file
RLAPI Sound LoadSoundFromWave(Wave wave);                             // Load sound from wave data
RLAPI Sound LoadSoundAlias(Sound source);                             // Create a new sound that shares the same sample data as the source sound, does not own the sound data
RLAPI bool IsSoundValid(Sound sound);                                 // Checks if a sound is valid (data loaded and buffers initialized)
RLAPI void UpdateSound(Sound sound, const void *data, int sampleCount); // Update sound buffer with new data
RLAPI void UnloadWave(Wave wave);                                     // Unload wave data
RLAPI void UnloadSound(Sound sound);                                  // Unload sound
RLAPI void UnloadSoundAlias(Sound alias);                             // Unload a sound alias (does not deallocate sample data)
RLAPI bool ExportWave(Wave wave, const char *fileName);               // Export wave data to file, returns true on success
RLAPI bool ExportWaveAsCode(Wave wave, const char *fileName);         // Export wave sample data to code (.h), returns true on success

// Wave/Sound management functions
RLAPI void PlaySound(Sound sound);                                    // Play a sound
RLAPI void StopSound(Sound sound);                                    // Stop playing a sound
RLAPI void PauseSound(Sound sound);                                   // Pause a sound
RLAPI void ResumeSound(Sound sound);                                  // Resume a paused sound
RLAPI bool IsSoundPlaying(Sound sound);                               // Check if a sound is currently playing
RLAPI void SetSoundVolume(Sound sound, float volume);                 // Set volume for a sound (1.0 is max level)
RLAPI void SetSoundPitch(Sound sound, float pitch);                   // Set pitch for a sound (1.0 is base level)
RLAPI void SetSoundPan(Sound sound, float pan);                       // Set pan for a sound (0.5 is center)
RLAPI Wave WaveCopy(Wave wave);                                       // Copy a wave to a new wave
RLAPI void WaveCrop(Wave *wave, int initFrame, int finalFrame);       // Crop a wave to defined frames range
RLAPI void WaveFormat(Wave *wave, int sampleRate, int sampleSize, int channels); // Convert wave data to desired format
RLAPI float *LoadWaveSamples(Wave wave);                              // Load samples data from wave as a 32bit float data array
RLAPI void UnloadWaveSamples(float *samples);                         // Unload samples data loaded with LoadWaveSamples()

// Music management functions
RLAPI Music LoadMusicStream(const char *fileName);                    // Load music stream from file
RLAPI Music LoadMusicStreamFromMemory(const char *fileType, const unsigned char *data, int dataSize); // Load music stream from data
RLAPI bool IsMusicValid(Music music);                                 // Checks if a music stream is valid (context and buffers initialized)
RLAPI void UnloadMusicStream(Music music);                            // Unload music stream
RLAPI void PlayMusicStream(Music music);                              // Start music playing
RLAPI bool IsMusicStreamPlaying(Music music);                         // Check if music is playing
RLAPI void UpdateMusicStream(Music music);                            // Updates buffers for music streaming
RLAPI void StopMusicStream(Music music);                              // Stop music playing
RLAPI void PauseMusicStream(Music music);                             // Pause music playing
RLAPI void ResumeMusicStream(Music music);                            // Resume playing paused music
RLAPI void SeekMusicStream(Music music, float position);              // Seek music to a position (in seconds)
RLAPI void SetMusicVolume(Music music, float volume);                 // Set volume for music (1.0 is max level)
RLAPI void SetMusicPitch(Music music, float pitch);                   // Set pitch for a music (1.0 is base level)
RLAPI void SetMusicPan(Music music, float pan);                       // Set pan for a music (0.5 is center)
RLAPI float GetMusicTimeLength(Music music);                          // Get music time length (in seconds)
RLAPI float GetMusicTimePlayed(Music music);                          // Get current music time played (in seconds)

// AudioStream management functions
RLAPI AudioStream LoadAudioStream(unsigned int sampleRate, unsigned int sampleSize, unsigned int channels); // Load audio stream (to stream raw audio pcm data)
RLAPI bool IsAudioStreamValid(AudioStream stream);                    // Checks if an audio stream is valid (buffers initialized)
RLAPI void UnloadAudioStream(AudioStream stream);                     // Unload audio stream and free memory
RLAPI void UpdateAudioStream(AudioStream stream, const void *data, int frameCount); // Update audio stream buffers with data
RLAPI bool IsAudioStreamProcessed(AudioStream stream);                // Check if any audio stream buffers requires refill
RLAPI void PlayAudioStream(AudioStream stream);                       // Play audio stream
RLAPI void PauseAudioStream(AudioStream stream);                      // Pause audio stream
RLAPI void ResumeAudioStream(AudioStream stream);                     // Resume audio stream
RLAPI bool IsAudioStreamPlaying(AudioStream stream);                  // Check if audio stream is playing
RLAPI void StopAudioStream(AudioStream stream);                       // Stop audio stream
RLAPI void SetAudioStreamVolume(AudioStream stream, float volume);    // Set volume for audio stream (1.0 is max level)
RLAPI void SetAudioStreamPitch(AudioStream stream, float pitch);      // Set pitch for audio stream (1.0 is base level)
RLAPI void SetAudioStreamPan(AudioStream stream, float pan);          // Set pan for audio stream (0.5 is centered)
RLAPI void SetAudioStreamBufferSizeDefault(int size);                 // Default size for new audio streams
RLAPI void SetAudioStreamCallback(AudioStream stream, AudioCallback callback); // Audio thread callback to request new data

RLAPI void AttachAudioStreamProcessor(AudioStream stream, AudioCallback processor); // Attach audio stream processor to stream, receives the samples as 'float'
RLAPI void DetachAudioStreamProcessor(AudioStream stream, AudioCallback processor); // Detach audio stream processor from stream

RLAPI void AttachAudioMixedProcessor(AudioCallback processor); // Attach audio stream processor to the entire audio pipeline, receives the samples as 'float'
RLAPI void DetachAudioMixedProcessor(AudioCallback processor); // Detach audio stream processor from the entire audio pipeline

#if defined(__cplusplus)
}
#endif

#endif // RAYLIB_H
/**********************************************************************************************
*
*   raymath v2.0 - Math functions to work with Vector2, Vector3, Matrix and Quaternions
*
*   CONVENTIONS:
*     - Matrix structure is defined as row-major (memory layout) but parameters naming AND all
*       math operations performed by the library consider the structure as it was column-major
*       It is like transposed versions of the matrices are used for all the maths
*       It benefits some functions making them cache-friendly and also avoids matrix
*       transpositions sometimes required by OpenGL
*       Example: In memory order, row0 is [m0 m4 m8 m12] but in semantic math row0 is [m0 m1 m2 m3]
*     - Functions are always self-contained, no function use another raymath function inside,
*       required code is directly re-implemented inside
*     - Functions input parameters are always received by value (2 unavoidable exceptions)
*     - Functions use always a "result" variable for return (except C++ operators)
*     - Functions are always defined inline
*     - Angles are always in radians (DEG2RAD/RAD2DEG macros provided for convenience)
*     - No compound literals used to make sure libray is compatible with C++
*
*   CONFIGURATION:
*       #define RAYMATH_IMPLEMENTATION
*           Generates the implementation of the library into the included file.
*           If not defined, the library is in header only mode and can be included in other headers
*           or source files without problems. But only ONE file should hold the implementation.
*
*       #define RAYMATH_STATIC_INLINE
*           Define static inline functions code, so #include header suffices for use.
*           This may use up lots of memory.
*
*       #define RAYMATH_DISABLE_CPP_OPERATORS
*           Disables C++ operator overloads for raymath types.
*
*   LICENSE: zlib/libpng
*
*   Copyright (c) 2015-2024 Ramon Santamaria (@raysan5)
*
*   This software is provided "as-is", without any express or implied warranty. In no event
*   will the authors be held liable for any damages arising from the use of this software.
*
*   Permission is granted to anyone to use this software for any purpose, including commercial
*   applications, and to alter it and redistribute it freely, subject to the following restrictions:
*
*     1. The origin of this software must not be misrepresented; you must not claim that you
*     wrote the original software. If you use this software in a product, an acknowledgment
*     in the product documentation would be appreciated but is not required.
*
*     2. Altered source versions must be plainly marked as such, and must not be misrepresented
*     as being the original software.
*
*     3. This notice may not be removed or altered from any source distribution.
*
**********************************************************************************************/

#ifndef RAYMATH_H
#define RAYMATH_H

#if defined(RAYMATH_IMPLEMENTATION) && defined(RAYMATH_STATIC_INLINE)
    #error "Specifying both RAYMATH_IMPLEMENTATION and RAYMATH_STATIC_INLINE is contradictory"
#endif

// Function specifiers definition
#if defined(RAYMATH_IMPLEMENTATION)
    #if defined(_WIN32) && defined(BUILD_LIBTYPE_SHARED)
        #define RMAPI __declspec(dllexport) extern inline // We are building raylib as a Win32 shared library (.dll)
    #elif defined(BUILD_LIBTYPE_SHARED)
        #define RMAPI __attribute__((visibility("default"))) // We are building raylib as a Unix shared library (.so/.dylib)
    #elif defined(_WIN32) && defined(USE_LIBTYPE_SHARED)
        #define RMAPI __declspec(dllimport)         // We are using raylib as a Win32 shared library (.dll)
    #else
        #define RMAPI extern inline // Provide external definition
    #endif
#elif defined(RAYMATH_STATIC_INLINE)
    #define RMAPI static inline // Functions may be inlined, no external out-of-line definition
#else
    #if defined(__TINYC__)
        #define RMAPI static inline // plain inline not supported by tinycc (See issue #435)
    #else
        #define RMAPI inline        // Functions may be inlined or external definition used
    #endif
#endif


//----------------------------------------------------------------------------------
// Defines and Macros
//----------------------------------------------------------------------------------
#ifndef PI
    #define PI 3.14159265358979323846f
#endif

#ifndef EPSILON
    #define EPSILON 0.000001f
#endif

#ifndef DEG2RAD
    #define DEG2RAD (PI/180.0f)
#endif

#ifndef RAD2DEG
    #define RAD2DEG (180.0f/PI)
#endif

// Get float vector for Matrix
#ifndef MatrixToFloat
    #define MatrixToFloat(mat) (MatrixToFloatV(mat).v)
#endif

// Get float vector for Vector3
#ifndef Vector3ToFloat
    #define Vector3ToFloat(vec) (Vector3ToFloatV(vec).v)
#endif

//----------------------------------------------------------------------------------
// Types and Structures Definition
//----------------------------------------------------------------------------------
#if !defined(RL_VECTOR2_TYPE)
// Vector2 type
typedef struct Vector2 {
    float x;
    float y;
} Vector2;
#define RL_VECTOR2_TYPE
#endif

#if !defined(RL_VECTOR3_TYPE)
// Vector3 type
typedef struct Vector3 {
    float x;
    float y;
    float z;
} Vector3;
#define RL_VECTOR3_TYPE
#endif

#if !defined(RL_VECTOR4_TYPE)
// Vector4 type
typedef struct Vector4 {
    float x;
    float y;
    float z;
    float w;
} Vector4;
#define RL_VECTOR4_TYPE
#endif

#if !defined(RL_QUATERNION_TYPE)
// Quaternion type
typedef Vector4 Quaternion;
#define RL_QUATERNION_TYPE
#endif

#if !defined(RL_MATRIX_TYPE)
// Matrix type (OpenGL style 4x4 - right handed, column major)
typedef struct Matrix {
    float m0, m4, m8, m12;      // Matrix first row (4 components)
    float m1, m5, m9, m13;      // Matrix second row (4 components)
    float m2, m6, m10, m14;     // Matrix third row (4 components)
    float m3, m7, m11, m15;     // Matrix fourth row (4 components)
} Matrix;
#define RL_MATRIX_TYPE
#endif

// NOTE: Helper types to be used instead of array return types for *ToFloat functions
typedef struct float3 {
    float v[3];
} float3;

typedef struct float16 {
    float v[16];
} float16;

#include <math.h>       // Required for: sinf(), cosf(), tan(), atan2f(), sqrtf(), floor(), fminf(), fmaxf(), fabsf()

//----------------------------------------------------------------------------------
// Module Functions Definition - Utils math
//----------------------------------------------------------------------------------

// Clamp float value
RMAPI float Clamp(float value, float min, float max)
{
    float result = (value < min)? min : value;

    if (result > max) result = max;

    return result;
}

// Calculate linear interpolation between two floats
RMAPI float Lerp(float start, float end, float amount)
{
    float result = start + amount*(end - start);

    return result;
}

// Normalize input value within input range
RMAPI float Normalize(float value, float start, float end)
{
    float result = (value - start)/(end - start);

    return result;
}

// Remap input value within input range to output range
RMAPI float Remap(float value, float inputStart, float inputEnd, float outputStart, float outputEnd)
{
    float result = (value - inputStart)/(inputEnd - inputStart)*(outputEnd - outputStart) + outputStart;

    return result;
}

// Wrap input value from min to max
RMAPI float Wrap(float value, float min, float max)
{
    float result = value - (max - min)*floorf((value - min)/(max - min));

    return result;
}

// Check whether two given floats are almost equal
RMAPI int FloatEquals(float x, float y)
{
#if !defined(EPSILON)
    #define EPSILON 0.000001f
#endif

    int result = (fabsf(x - y)) <= (EPSILON*fmaxf(1.0f, fmaxf(fabsf(x), fabsf(y))));

    return result;
}

//----------------------------------------------------------------------------------
// Module Functions Definition - Vector2 math
//----------------------------------------------------------------------------------

// Vector with components value 0.0f
RMAPI Vector2 Vector2Zero(void)
{
    Vector2 result = { 0.0f, 0.0f };

    return result;
}

// Vector with components value 1.0f
RMAPI Vector2 Vector2One(void)
{
    Vector2 result = { 1.0f, 1.0f };

    return result;
}

// Add two vectors (v1 + v2)
RMAPI Vector2 Vector2Add(Vector2 v1, Vector2 v2)
{
    Vector2 result = { v1.x + v2.x, v1.y + v2.y };

    return result;
}

// Add vector and float value
RMAPI Vector2 Vector2AddValue(Vector2 v, float add)
{
    Vector2 result = { v.x + add, v.y + add };

    return result;
}

// Subtract two vectors (v1 - v2)
RMAPI Vector2 Vector2Subtract(Vector2 v1, Vector2 v2)
{
    Vector2 result = { v1.x - v2.x, v1.y - v2.y };

    return result;
}

// Subtract vector by float value
RMAPI Vector2 Vector2SubtractValue(Vector2 v, float sub)
{
    Vector2 result = { v.x - sub, v.y - sub };

    return result;
}

// Calculate vector length
RMAPI float Vector2Length(Vector2 v)
{
    float result = sqrtf((v.x*v.x) + (v.y*v.y));

    return result;
}

// Calculate vector square length
RMAPI float Vector2LengthSqr(Vector2 v)
{
    float result = (v.x*v.x) + (v.y*v.y);

    return result;
}

// Calculate two vectors dot product
RMAPI float Vector2DotProduct(Vector2 v1, Vector2 v2)
{
    float result = (v1.x*v2.x + v1.y*v2.y);

    return result;
}

// Calculate two vectors cross product
RMAPI float Vector2CrossProduct(Vector2 v1, Vector2 v2)
{
    float result = (v1.x*v2.y - v1.y*v2.x);

    return result;
}

// Calculate distance between two vectors
RMAPI float Vector2Distance(Vector2 v1, Vector2 v2)
{
    float result = sqrtf((v1.x - v2.x)*(v1.x - v2.x) + (v1.y - v2.y)*(v1.y - v2.y));

    return result;
}

// Calculate square distance between two vectors
RMAPI float Vector2DistanceSqr(Vector2 v1, Vector2 v2)
{
    float result = ((v1.x - v2.x)*(v1.x - v2.x) + (v1.y - v2.y)*(v1.y - v2.y));

    return result;
}

// Calculate angle between two vectors
// NOTE: Angle is calculated from origin point (0, 0)
RMAPI float Vector2Angle(Vector2 v1, Vector2 v2)
{
    float result = 0.0f;

    float dot = v1.x*v2.x + v1.y*v2.y;
    float det = v1.x*v2.y - v1.y*v2.x;

    result = atan2f(det, dot);

    return result;
}

// Calculate angle defined by a two vectors line
// NOTE: Parameters need to be normalized
// Current implementation should be aligned with glm::angle
RMAPI float Vector2LineAngle(Vector2 start, Vector2 end)
{
    float result = 0.0f;

    // TODO(10/9/2023): Currently angles move clockwise, determine if this is wanted behavior
    result = -atan2f(end.y - start.y, end.x - start.x);

    return result;
}

// Scale vector (multiply by value)
RMAPI Vector2 Vector2Scale(Vector2 v, float scale)
{
    Vector2 result = { v.x*scale, v.y*scale };

    return result;
}

// Multiply vector by vector
RMAPI Vector2 Vector2Multiply(Vector2 v1, Vector2 v2)
{
    Vector2 result = { v1.x*v2.x, v1.y*v2.y };

    return result;
}

// Negate vector
RMAPI Vector2 Vector2Negate(Vector2 v)
{
    Vector2 result = { -v.x, -v.y };

    return result;
}

// Divide vector by vector
RMAPI Vector2 Vector2Divide(Vector2 v1, Vector2 v2)
{
    Vector2 result = { v1.x/v2.x, v1.y/v2.y };

    return result;
}

// Normalize provided vector
RMAPI Vector2 Vector2Normalize(Vector2 v)
{
    Vector2 result = { 0 };
    float length = sqrtf((v.x*v.x) + (v.y*v.y));

    if (length > 0)
    {
        float ilength = 1.0f/length;
        result.x = v.x*ilength;
        result.y = v.y*ilength;
    }

    return result;
}

// Transforms a Vector2 by a given Matrix
RMAPI Vector2 Vector2Transform(Vector2 v, Matrix mat)
{
    Vector2 result = { 0 };

    float x = v.x;
    float y = v.y;
    float z = 0;

    result.x = mat.m0*x + mat.m4*y + mat.m8*z + mat.m12;
    result.y = mat.m1*x + mat.m5*y + mat.m9*z + mat.m13;

    return result;
}

// Calculate linear interpolation between two vectors
RMAPI Vector2 Vector2Lerp(Vector2 v1, Vector2 v2, float amount)
{
    Vector2 result = { 0 };

    result.x = v1.x + amount*(v2.x - v1.x);
    result.y = v1.y + amount*(v2.y - v1.y);

    return result;
}

// Calculate reflected vector to normal
RMAPI Vector2 Vector2Reflect(Vector2 v, Vector2 normal)
{
    Vector2 result = { 0 };

    float dotProduct = (v.x*normal.x + v.y*normal.y); // Dot product

    result.x = v.x - (2.0f*normal.x)*dotProduct;
    result.y = v.y - (2.0f*normal.y)*dotProduct;

    return result;
}

// Get min value for each pair of components
RMAPI Vector2 Vector2Min(Vector2 v1, Vector2 v2)
{
    Vector2 result = { 0 };

    result.x = fminf(v1.x, v2.x);
    result.y = fminf(v1.y, v2.y);

    return result;
}

// Get max value for each pair of components
RMAPI Vector2 Vector2Max(Vector2 v1, Vector2 v2)
{
    Vector2 result = { 0 };

    result.x = fmaxf(v1.x, v2.x);
    result.y = fmaxf(v1.y, v2.y);

    return result;
}

// Rotate vector by angle
RMAPI Vector2 Vector2Rotate(Vector2 v, float angle)
{
    Vector2 result = { 0 };

    float cosres = cosf(angle);
    float sinres = sinf(angle);

    result.x = v.x*cosres - v.y*sinres;
    result.y = v.x*sinres + v.y*cosres;

    return result;
}

// Move Vector towards target
RMAPI Vector2 Vector2MoveTowards(Vector2 v, Vector2 target, float maxDistance)
{
    Vector2 result = { 0 };

    float dx = target.x - v.x;
    float dy = target.y - v.y;
    float value = (dx*dx) + (dy*dy);

    if ((value == 0) || ((maxDistance >= 0) && (value <= maxDistance*maxDistance))) return target;

    float dist = sqrtf(value);

    result.x = v.x + dx/dist*maxDistance;
    result.y = v.y + dy/dist*maxDistance;

    return result;
}

// Invert the given vector
RMAPI Vector2 Vector2Invert(Vector2 v)
{
    Vector2 result = { 1.0f/v.x, 1.0f/v.y };

    return result;
}

// Clamp the components of the vector between
// min and max values specified by the given vectors
RMAPI Vector2 Vector2Clamp(Vector2 v, Vector2 min, Vector2 max)
{
    Vector2 result = { 0 };

    result.x = fminf(max.x, fmaxf(min.x, v.x));
    result.y = fminf(max.y, fmaxf(min.y, v.y));

    return result;
}

// Clamp the magnitude of the vector between two min and max values
RMAPI Vector2 Vector2ClampValue(Vector2 v, float min, float max)
{
    Vector2 result = v;

    float length = (v.x*v.x) + (v.y*v.y);
    if (length > 0.0f)
    {
        length = sqrtf(length);

        float scale = 1;    // By default, 1 as the neutral element.
        if (length < min)
        {
            scale = min/length;
        }
        else if (length > max)
        {
            scale = max/length;
        }

        result.x = v.x*scale;
        result.y = v.y*scale;
    }

    return result;
}

// Check whether two given vectors are almost equal
RMAPI int Vector2Equals(Vector2 p, Vector2 q)
{
#if !defined(EPSILON)
    #define EPSILON 0.000001f
#endif

    int result = ((fabsf(p.x - q.x)) <= (EPSILON*fmaxf(1.0f, fmaxf(fabsf(p.x), fabsf(q.x))))) &&
                  ((fabsf(p.y - q.y)) <= (EPSILON*fmaxf(1.0f, fmaxf(fabsf(p.y), fabsf(q.y)))));

    return result;
}

// Compute the direction of a refracted ray
// v: normalized direction of the incoming ray
// n: normalized normal vector of the interface of two optical media
// r: ratio of the refractive index of the medium from where the ray comes
//    to the refractive index of the medium on the other side of the surface
RMAPI Vector2 Vector2Refract(Vector2 v, Vector2 n, float r)
{
    Vector2 result = { 0 };

    float dot = v.x*n.x + v.y*n.y;
    float d = 1.0f - r*r*(1.0f - dot*dot);

    if (d >= 0.0f)
    {
        d = sqrtf(d);
        v.x = r*v.x - (r*dot + d)*n.x;
        v.y = r*v.y - (r*dot + d)*n.y;

        result = v;
    }

    return result;
}


//----------------------------------------------------------------------------------
// Module Functions Definition - Vector3 math
//----------------------------------------------------------------------------------

// Vector with components value 0.0f
RMAPI Vector3 Vector3Zero(void)
{
    Vector3 result = { 0.0f, 0.0f, 0.0f };

    return result;
}

// Vector with components value 1.0f
RMAPI Vector3 Vector3One(void)
{
    Vector3 result = { 1.0f, 1.0f, 1.0f };

    return result;
}

// Add two vectors
RMAPI Vector3 Vector3Add(Vector3 v1, Vector3 v2)
{
    Vector3 result = { v1.x + v2.x, v1.y + v2.y, v1.z + v2.z };

    return result;
}

// Add vector and float value
RMAPI Vector3 Vector3AddValue(Vector3 v, float add)
{
    Vector3 result = { v.x + add, v.y + add, v.z + add };

    return result;
}

// Subtract two vectors
RMAPI Vector3 Vector3Subtract(Vector3 v1, Vector3 v2)
{
    Vector3 result = { v1.x - v2.x, v1.y - v2.y, v1.z - v2.z };

    return result;
}

// Subtract vector by float value
RMAPI Vector3 Vector3SubtractValue(Vector3 v, float sub)
{
    Vector3 result = { v.x - sub, v.y - sub, v.z - sub };

    return result;
}

// Multiply vector by scalar
RMAPI Vector3 Vector3Scale(Vector3 v, float scalar)
{
    Vector3 result = { v.x*scalar, v.y*scalar, v.z*scalar };

    return result;
}

// Multiply vector by vector
RMAPI Vector3 Vector3Multiply(Vector3 v1, Vector3 v2)
{
    Vector3 result = { v1.x*v2.x, v1.y*v2.y, v1.z*v2.z };

    return result;
}

// Calculate two vectors cross product
RMAPI Vector3 Vector3CrossProduct(Vector3 v1, Vector3 v2)
{
    Vector3 result = { v1.y*v2.z - v1.z*v2.y, v1.z*v2.x - v1.x*v2.z, v1.x*v2.y - v1.y*v2.x };

    return result;
}

// Calculate one vector perpendicular vector
RMAPI Vector3 Vector3Perpendicular(Vector3 v)
{
    Vector3 result = { 0 };

    float min = fabsf(v.x);
    Vector3 cardinalAxis = {1.0f, 0.0f, 0.0f};

    if (fabsf(v.y) < min)
    {
        min = fabsf(v.y);
        Vector3 tmp = {0.0f, 1.0f, 0.0f};
        cardinalAxis = tmp;
    }

    if (fabsf(v.z) < min)
    {
        Vector3 tmp = {0.0f, 0.0f, 1.0f};
        cardinalAxis = tmp;
    }

    // Cross product between vectors
    result.x = v.y*cardinalAxis.z - v.z*cardinalAxis.y;
    result.y = v.z*cardinalAxis.x - v.x*cardinalAxis.z;
    result.z = v.x*cardinalAxis.y - v.y*cardinalAxis.x;

    return result;
}

// Calculate vector length
RMAPI float Vector3Length(const Vector3 v)
{
    float result = sqrtf(v.x*v.x + v.y*v.y + v.z*v.z);

    return result;
}

// Calculate vector square length
RMAPI float Vector3LengthSqr(const Vector3 v)
{
    float result = v.x*v.x + v.y*v.y + v.z*v.z;

    return result;
}

// Calculate two vectors dot product
RMAPI float Vector3DotProduct(Vector3 v1, Vector3 v2)
{
    float result = (v1.x*v2.x + v1.y*v2.y + v1.z*v2.z);

    return result;
}

// Calculate distance between two vectors
RMAPI float Vector3Distance(Vector3 v1, Vector3 v2)
{
    float result = 0.0f;

    float dx = v2.x - v1.x;
    float dy = v2.y - v1.y;
    float dz = v2.z - v1.z;
    result = sqrtf(dx*dx + dy*dy + dz*dz);

    return result;
}

// Calculate square distance between two vectors
RMAPI float Vector3DistanceSqr(Vector3 v1, Vector3 v2)
{
    float result = 0.0f;

    float dx = v2.x - v1.x;
    float dy = v2.y - v1.y;
    float dz = v2.z - v1.z;
    result = dx*dx + dy*dy + dz*dz;

    return result;
}

// Calculate angle between two vectors
RMAPI float Vector3Angle(Vector3 v1, Vector3 v2)
{
    float result = 0.0f;

    Vector3 cross = { v1.y*v2.z - v1.z*v2.y, v1.z*v2.x - v1.x*v2.z, v1.x*v2.y - v1.y*v2.x };
    float len = sqrtf(cross.x*cross.x + cross.y*cross.y + cross.z*cross.z);
    float dot = (v1.x*v2.x + v1.y*v2.y + v1.z*v2.z);
    result = atan2f(len, dot);

    return result;
}

// Negate provided vector (invert direction)
RMAPI Vector3 Vector3Negate(Vector3 v)
{
    Vector3 result = { -v.x, -v.y, -v.z };

    return result;
}

// Divide vector by vector
RMAPI Vector3 Vector3Divide(Vector3 v1, Vector3 v2)
{
    Vector3 result = { v1.x/v2.x, v1.y/v2.y, v1.z/v2.z };

    return result;
}

// Normalize provided vector
RMAPI Vector3 Vector3Normalize(Vector3 v)
{
    Vector3 result = v;

    float length = sqrtf(v.x*v.x + v.y*v.y + v.z*v.z);
    if (length != 0.0f)
    {
        float ilength = 1.0f/length;

        result.x *= ilength;
        result.y *= ilength;
        result.z *= ilength;
    }

    return result;
}

//Calculate the projection of the vector v1 on to v2
RMAPI Vector3 Vector3Project(Vector3 v1, Vector3 v2)
{
    Vector3 result = { 0 };

    float v1dv2 = (v1.x*v2.x + v1.y*v2.y + v1.z*v2.z);
    float v2dv2 = (v2.x*v2.x + v2.y*v2.y + v2.z*v2.z);

    float mag = v1dv2/v2dv2;

    result.x = v2.x*mag;
    result.y = v2.y*mag;
    result.z = v2.z*mag;

    return result;
}

//Calculate the rejection of the vector v1 on to v2
RMAPI Vector3 Vector3Reject(Vector3 v1, Vector3 v2)
{
    Vector3 result = { 0 };

    float v1dv2 = (v1.x*v2.x + v1.y*v2.y + v1.z*v2.z);
    float v2dv2 = (v2.x*v2.x + v2.y*v2.y + v2.z*v2.z);

    float mag = v1dv2/v2dv2;

    result.x = v1.x - (v2.x*mag);
    result.y = v1.y - (v2.y*mag);
    result.z = v1.z - (v2.z*mag);

    return result;
}

// Orthonormalize provided vectors
// Makes vectors normalized and orthogonal to each other
// Gram-Schmidt function implementation
RMAPI void Vector3OrthoNormalize(Vector3 *v1, Vector3 *v2)
{
    float length = 0.0f;
    float ilength = 0.0f;

    // Vector3Normalize(*v1);
    Vector3 v = *v1;
    length = sqrtf(v.x*v.x + v.y*v.y + v.z*v.z);
    if (length == 0.0f) length = 1.0f;
    ilength = 1.0f/length;
    v1->x *= ilength;
    v1->y *= ilength;
    v1->z *= ilength;

    // Vector3CrossProduct(*v1, *v2)
    Vector3 vn1 = { v1->y*v2->z - v1->z*v2->y, v1->z*v2->x - v1->x*v2->z, v1->x*v2->y - v1->y*v2->x };

    // Vector3Normalize(vn1);
    v = vn1;
    length = sqrtf(v.x*v.x + v.y*v.y + v.z*v.z);
    if (length == 0.0f) length = 1.0f;
    ilength = 1.0f/length;
    vn1.x *= ilength;
    vn1.y *= ilength;
    vn1.z *= ilength;

    // Vector3CrossProduct(vn1, *v1)
    Vector3 vn2 = { vn1.y*v1->z - vn1.z*v1->y, vn1.z*v1->x - vn1.x*v1->z, vn1.x*v1->y - vn1.y*v1->x };

    *v2 = vn2;
}

// Transforms a Vector3 by a given Matrix
RMAPI Vector3 Vector3Transform(Vector3 v, Matrix mat)
{
    Vector3 result = { 0 };

    float x = v.x;
    float y = v.y;
    float z = v.z;

    result.x = mat.m0*x + mat.m4*y + mat.m8*z + mat.m12;
    result.y = mat.m1*x + mat.m5*y + mat.m9*z + mat.m13;
    result.z = mat.m2*x + mat.m6*y + mat.m10*z + mat.m14;

    return result;
}

// Transform a vector by quaternion rotation
RMAPI Vector3 Vector3RotateByQuaternion(Vector3 v, Quaternion q)
{
    Vector3 result = { 0 };

    result.x = v.x*(q.x*q.x + q.w*q.w - q.y*q.y - q.z*q.z) + v.y*(2*q.x*q.y - 2*q.w*q.z) + v.z*(2*q.x*q.z + 2*q.w*q.y);
    result.y = v.x*(2*q.w*q.z + 2*q.x*q.y) + v.y*(q.w*q.w - q.x*q.x + q.y*q.y - q.z*q.z) + v.z*(-2*q.w*q.x + 2*q.y*q.z);
    result.z = v.x*(-2*q.w*q.y + 2*q.x*q.z) + v.y*(2*q.w*q.x + 2*q.y*q.z)+ v.z*(q.w*q.w - q.x*q.x - q.y*q.y + q.z*q.z);

    return result;
}

// Rotates a vector around an axis
RMAPI Vector3 Vector3RotateByAxisAngle(Vector3 v, Vector3 axis, float angle)
{
    // Using Euler-Rodrigues Formula
    // Ref.: https://en.wikipedia.org/w/index.php?title=Euler%E2%80%93Rodrigues_formula

    Vector3 result = v;

    // Vector3Normalize(axis);
    float length = sqrtf(axis.x*axis.x + axis.y*axis.y + axis.z*axis.z);
    if (length == 0.0f) length = 1.0f;
    float ilength = 1.0f/length;
    axis.x *= ilength;
    axis.y *= ilength;
    axis.z *= ilength;

    angle /= 2.0f;
    float a = sinf(angle);
    float b = axis.x*a;
    float c = axis.y*a;
    float d = axis.z*a;
    a = cosf(angle);
    Vector3 w = { b, c, d };

    // Vector3CrossProduct(w, v)
    Vector3 wv = { w.y*v.z - w.z*v.y, w.z*v.x - w.x*v.z, w.x*v.y - w.y*v.x };

    // Vector3CrossProduct(w, wv)
    Vector3 wwv = { w.y*wv.z - w.z*wv.y, w.z*wv.x - w.x*wv.z, w.x*wv.y - w.y*wv.x };

    // Vector3Scale(wv, 2*a)
    a *= 2;
    wv.x *= a;
    wv.y *= a;
    wv.z *= a;

    // Vector3Scale(wwv, 2)
    wwv.x *= 2;
    wwv.y *= 2;
    wwv.z *= 2;

    result.x += wv.x;
    result.y += wv.y;
    result.z += wv.z;

    result.x += wwv.x;
    result.y += wwv.y;
    result.z += wwv.z;

    return result;
}

// Move Vector towards target
RMAPI Vector3 Vector3MoveTowards(Vector3 v, Vector3 target, float maxDistance)
{
    Vector3 result = { 0 };

    float dx = target.x - v.x;
    float dy = target.y - v.y;
    float dz = target.z - v.z;
    float value = (dx*dx) + (dy*dy) + (dz*dz);

    if ((value == 0) || ((maxDistance >= 0) && (value <= maxDistance*maxDistance))) return target;

    float dist = sqrtf(value);

    result.x = v.x + dx/dist*maxDistance;
    result.y = v.y + dy/dist*maxDistance;
    result.z = v.z + dz/dist*maxDistance;

    return result;
}

// Calculate linear interpolation between two vectors
RMAPI Vector3 Vector3Lerp(Vector3 v1, Vector3 v2, float amount)
{
    Vector3 result = { 0 };

    result.x = v1.x + amount*(v2.x - v1.x);
    result.y = v1.y + amount*(v2.y - v1.y);
    result.z = v1.z + amount*(v2.z - v1.z);

    return result;
}

// Calculate cubic hermite interpolation between two vectors and their tangents
// as described in the GLTF 2.0 specification: https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#interpolation-cubic
RMAPI Vector3 Vector3CubicHermite(Vector3 v1, Vector3 tangent1, Vector3 v2, Vector3 tangent2, float amount)
{
    Vector3 result = { 0 };

    float amountPow2 = amount*amount;
    float amountPow3 = amount*amount*amount;

    result.x = (2*amountPow3 - 3*amountPow2 + 1)*v1.x + (amountPow3 - 2*amountPow2 + amount)*tangent1.x + (-2*amountPow3 + 3*amountPow2)*v2.x + (amountPow3 - amountPow2)*tangent2.x;
    result.y = (2*amountPow3 - 3*amountPow2 + 1)*v1.y + (amountPow3 - 2*amountPow2 + amount)*tangent1.y + (-2*amountPow3 + 3*amountPow2)*v2.y + (amountPow3 - amountPow2)*tangent2.y;
    result.z = (2*amountPow3 - 3*amountPow2 + 1)*v1.z + (amountPow3 - 2*amountPow2 + amount)*tangent1.z + (-2*amountPow3 + 3*amountPow2)*v2.z + (amountPow3 - amountPow2)*tangent2.z;

    return result;
}

// Calculate reflected vector to normal
RMAPI Vector3 Vector3Reflect(Vector3 v, Vector3 normal)
{
    Vector3 result = { 0 };

    // I is the original vector
    // N is the normal of the incident plane
    // R = I - (2*N*(DotProduct[I, N]))

    float dotProduct = (v.x*normal.x + v.y*normal.y + v.z*normal.z);

    result.x = v.x - (2.0f*normal.x)*dotProduct;
    result.y = v.y - (2.0f*normal.y)*dotProduct;
    result.z = v.z - (2.0f*normal.z)*dotProduct;

    return result;
}

// Get min value for each pair of components
RMAPI Vector3 Vector3Min(Vector3 v1, Vector3 v2)
{
    Vector3 result = { 0 };

    result.x = fminf(v1.x, v2.x);
    result.y = fminf(v1.y, v2.y);
    result.z = fminf(v1.z, v2.z);

    return result;
}

// Get max value for each pair of components
RMAPI Vector3 Vector3Max(Vector3 v1, Vector3 v2)
{
    Vector3 result = { 0 };

    result.x = fmaxf(v1.x, v2.x);
    result.y = fmaxf(v1.y, v2.y);
    result.z = fmaxf(v1.z, v2.z);

    return result;
}

// Compute barycenter coordinates (u, v, w) for point p with respect to triangle (a, b, c)
// NOTE: Assumes P is on the plane of the triangle
RMAPI Vector3 Vector3Barycenter(Vector3 p, Vector3 a, Vector3 b, Vector3 c)
{
    Vector3 result = { 0 };

    Vector3 v0 = { b.x - a.x, b.y - a.y, b.z - a.z };   // Vector3Subtract(b, a)
    Vector3 v1 = { c.x - a.x, c.y - a.y, c.z - a.z };   // Vector3Subtract(c, a)
    Vector3 v2 = { p.x - a.x, p.y - a.y, p.z - a.z };   // Vector3Subtract(p, a)
    float d00 = (v0.x*v0.x + v0.y*v0.y + v0.z*v0.z);    // Vector3DotProduct(v0, v0)
    float d01 = (v0.x*v1.x + v0.y*v1.y + v0.z*v1.z);    // Vector3DotProduct(v0, v1)
    float d11 = (v1.x*v1.x + v1.y*v1.y + v1.z*v1.z);    // Vector3DotProduct(v1, v1)
    float d20 = (v2.x*v0.x + v2.y*v0.y + v2.z*v0.z);    // Vector3DotProduct(v2, v0)
    float d21 = (v2.x*v1.x + v2.y*v1.y + v2.z*v1.z);    // Vector3DotProduct(v2, v1)

    float denom = d00*d11 - d01*d01;

    result.y = (d11*d20 - d01*d21)/denom;
    result.z = (d00*d21 - d01*d20)/denom;
    result.x = 1.0f - (result.z + result.y);

    return result;
}

// Projects a Vector3 from screen space into object space
// NOTE: We are avoiding calling other raymath functions despite available
RMAPI Vector3 Vector3Unproject(Vector3 source, Matrix projection, Matrix view)
{
    Vector3 result = { 0 };

    // Calculate unprojected matrix (multiply view matrix by projection matrix) and invert it
    Matrix matViewProj = {      // MatrixMultiply(view, projection);
        view.m0*projection.m0 + view.m1*projection.m4 + view.m2*projection.m8 + view.m3*projection.m12,
        view.m0*projection.m1 + view.m1*projection.m5 + view.m2*projection.m9 + view.m3*projection.m13,
        view.m0*projection.m2 + view.m1*projection.m6 + view.m2*projection.m10 + view.m3*projection.m14,
        view.m0*projection.m3 + view.m1*projection.m7 + view.m2*projection.m11 + view.m3*projection.m15,
        view.m4*projection.m0 + view.m5*projection.m4 + view.m6*projection.m8 + view.m7*projection.m12,
        view.m4*projection.m1 + view.m5*projection.m5 + view.m6*projection.m9 + view.m7*projection.m13,
        view.m4*projection.m2 + view.m5*projection.m6 + view.m6*projection.m10 + view.m7*projection.m14,
        view.m4*projection.m3 + view.m5*projection.m7 + view.m6*projection.m11 + view.m7*projection.m15,
        view.m8*projection.m0 + view.m9*projection.m4 + view.m10*projection.m8 + view.m11*projection.m12,
        view.m8*projection.m1 + view.m9*projection.m5 + view.m10*projection.m9 + view.m11*projection.m13,
        view.m8*projection.m2 + view.m9*projection.m6 + view.m10*projection.m10 + view.m11*projection.m14,
        view.m8*projection.m3 + view.m9*projection.m7 + view.m10*projection.m11 + view.m11*projection.m15,
        view.m12*projection.m0 + view.m13*projection.m4 + view.m14*projection.m8 + view.m15*projection.m12,
        view.m12*projection.m1 + view.m13*projection.m5 + view.m14*projection.m9 + view.m15*projection.m13,
        view.m12*projection.m2 + view.m13*projection.m6 + view.m14*projection.m10 + view.m15*projection.m14,
        view.m12*projection.m3 + view.m13*projection.m7 + view.m14*projection.m11 + view.m15*projection.m15 };

    // Calculate inverted matrix -> MatrixInvert(matViewProj);
    // Cache the matrix values (speed optimization)
    float a00 = matViewProj.m0, a01 = matViewProj.m1, a02 = matViewProj.m2, a03 = matViewProj.m3;
    float a10 = matViewProj.m4, a11 = matViewProj.m5, a12 = matViewProj.m6, a13 = matViewProj.m7;
    float a20 = matViewProj.m8, a21 = matViewProj.m9, a22 = matViewProj.m10, a23 = matViewProj.m11;
    float a30 = matViewProj.m12, a31 = matViewProj.m13, a32 = matViewProj.m14, a33 = matViewProj.m15;

    float b00 = a00*a11 - a01*a10;
    float b01 = a00*a12 - a02*a10;
    float b02 = a00*a13 - a03*a10;
    float b03 = a01*a12 - a02*a11;
    float b04 = a01*a13 - a03*a11;
    float b05 = a02*a13 - a03*a12;
    float b06 = a20*a31 - a21*a30;
    float b07 = a20*a32 - a22*a30;
    float b08 = a20*a33 - a23*a30;
    float b09 = a21*a32 - a22*a31;
    float b10 = a21*a33 - a23*a31;
    float b11 = a22*a33 - a23*a32;

    // Calculate the invert determinant (inlined to avoid double-caching)
    float invDet = 1.0f/(b00*b11 - b01*b10 + b02*b09 + b03*b08 - b04*b07 + b05*b06);

    Matrix matViewProjInv = {
        (a11*b11 - a12*b10 + a13*b09)*invDet,
        (-a01*b11 + a02*b10 - a03*b09)*invDet,
        (a31*b05 - a32*b04 + a33*b03)*invDet,
        (-a21*b05 + a22*b04 - a23*b03)*invDet,
        (-a10*b11 + a12*b08 - a13*b07)*invDet,
        (a00*b11 - a02*b08 + a03*b07)*invDet,
        (-a30*b05 + a32*b02 - a33*b01)*invDet,
        (a20*b05 - a22*b02 + a23*b01)*invDet,
        (a10*b10 - a11*b08 + a13*b06)*invDet,
        (-a00*b10 + a01*b08 - a03*b06)*invDet,
        (a30*b04 - a31*b02 + a33*b00)*invDet,
        (-a20*b04 + a21*b02 - a23*b00)*invDet,
        (-a10*b09 + a11*b07 - a12*b06)*invDet,
        (a00*b09 - a01*b07 + a02*b06)*invDet,
        (-a30*b03 + a31*b01 - a32*b00)*invDet,
        (a20*b03 - a21*b01 + a22*b00)*invDet };

    // Create quaternion from source point
    Quaternion quat = { source.x, source.y, source.z, 1.0f };

    // Multiply quat point by unprojecte matrix
    Quaternion qtransformed = {     // QuaternionTransform(quat, matViewProjInv)
        matViewProjInv.m0*quat.x + matViewProjInv.m4*quat.y + matViewProjInv.m8*quat.z + matViewProjInv.m12*quat.w,
        matViewProjInv.m1*quat.x + matViewProjInv.m5*quat.y + matViewProjInv.m9*quat.z + matViewProjInv.m13*quat.w,
        matViewProjInv.m2*quat.x + matViewProjInv.m6*quat.y + matViewProjInv.m10*quat.z + matViewProjInv.m14*quat.w,
        matViewProjInv.m3*quat.x + matViewProjInv.m7*quat.y + matViewProjInv.m11*quat.z + matViewProjInv.m15*quat.w };

    // Normalized world points in vectors
    result.x = qtransformed.x/qtransformed.w;
    result.y = qtransformed.y/qtransformed.w;
    result.z = qtransformed.z/qtransformed.w;

    return result;
}

// Get Vector3 as float array
RMAPI float3 Vector3ToFloatV(Vector3 v)
{
    float3 buffer = { 0 };

    buffer.v[0] = v.x;
    buffer.v[1] = v.y;
    buffer.v[2] = v.z;

    return buffer;
}

// Invert the given vector
RMAPI Vector3 Vector3Invert(Vector3 v)
{
    Vector3 result = { 1.0f/v.x, 1.0f/v.y, 1.0f/v.z };

    return result;
}

// Clamp the components of the vector between
// min and max values specified by the given vectors
RMAPI Vector3 Vector3Clamp(Vector3 v, Vector3 min, Vector3 max)
{
    Vector3 result = { 0 };

    result.x = fminf(max.x, fmaxf(min.x, v.x));
    result.y = fminf(max.y, fmaxf(min.y, v.y));
    result.z = fminf(max.z, fmaxf(min.z, v.z));

    return result;
}

// Clamp the magnitude of the vector between two values
RMAPI Vector3 Vector3ClampValue(Vector3 v, float min, float max)
{
    Vector3 result = v;

    float length = (v.x*v.x) + (v.y*v.y) + (v.z*v.z);
    if (length > 0.0f)
    {
        length = sqrtf(length);

        float scale = 1;    // By default, 1 as the neutral element.
        if (length < min)
        {
            scale = min/length;
        }
        else if (length > max)
        {
            scale = max/length;
        }

        result.x = v.x*scale;
        result.y = v.y*scale;
        result.z = v.z*scale;
    }

    return result;
}

// Check whether two given vectors are almost equal
RMAPI int Vector3Equals(Vector3 p, Vector3 q)
{
#if !defined(EPSILON)
    #define EPSILON 0.000001f
#endif

    int result = ((fabsf(p.x - q.x)) <= (EPSILON*fmaxf(1.0f, fmaxf(fabsf(p.x), fabsf(q.x))))) &&
                 ((fabsf(p.y - q.y)) <= (EPSILON*fmaxf(1.0f, fmaxf(fabsf(p.y), fabsf(q.y))))) &&
                 ((fabsf(p.z - q.z)) <= (EPSILON*fmaxf(1.0f, fmaxf(fabsf(p.z), fabsf(q.z)))));

    return result;
}

// Compute the direction of a refracted ray
// v: normalized direction of the incoming ray
// n: normalized normal vector of the interface of two optical media
// r: ratio of the refractive index of the medium from where the ray comes
//    to the refractive index of the medium on the other side of the surface
RMAPI Vector3 Vector3Refract(Vector3 v, Vector3 n, float r)
{
    Vector3 result = { 0 };

    float dot = v.x*n.x + v.y*n.y + v.z*n.z;
    float d = 1.0f - r*r*(1.0f - dot*dot);

    if (d >= 0.0f)
    {
        d = sqrtf(d);
        v.x = r*v.x - (r*dot + d)*n.x;
        v.y = r*v.y - (r*dot + d)*n.y;
        v.z = r*v.z - (r*dot + d)*n.z;

        result = v;
    }

    return result;
}


//----------------------------------------------------------------------------------
// Module Functions Definition - Vector4 math
//----------------------------------------------------------------------------------

RMAPI Vector4 Vector4Zero(void)
{
    Vector4 result = { 0.0f, 0.0f, 0.0f, 0.0f };
    return result;
}

RMAPI Vector4 Vector4One(void)
{
    Vector4 result = { 1.0f, 1.0f, 1.0f, 1.0f };
    return result;
}

RMAPI Vector4 Vector4Add(Vector4 v1, Vector4 v2)
{
    Vector4 result = {
        v1.x + v2.x,
        v1.y + v2.y,
        v1.z + v2.z,
        v1.w + v2.w
    };
    return result;
}

RMAPI Vector4 Vector4AddValue(Vector4 v, float add)
{
    Vector4 result = {
        v.x + add,
        v.y + add,
        v.z + add,
        v.w + add
    };
    return result;
}

RMAPI Vector4 Vector4Subtract(Vector4 v1, Vector4 v2)
{
    Vector4 result = {
        v1.x - v2.x,
        v1.y - v2.y,
        v1.z - v2.z,
        v1.w - v2.w
    };
    return result;
}

RMAPI Vector4 Vector4SubtractValue(Vector4 v, float add)
{
    Vector4 result = {
        v.x - add,
        v.y - add,
        v.z - add,
        v.w - add
    };
    return result;
}

RMAPI float Vector4Length(Vector4 v)
{
    float result = sqrtf((v.x*v.x) + (v.y*v.y) + (v.z*v.z) + (v.w*v.w));
    return result;
}

RMAPI float Vector4LengthSqr(Vector4 v)
{
    float result = (v.x*v.x) + (v.y*v.y) + (v.z*v.z) + (v.w*v.w);
    return result;
}

RMAPI float Vector4DotProduct(Vector4 v1, Vector4 v2)
{
    float result = (v1.x*v2.x + v1.y*v2.y + v1.z*v2.z + v1.w*v2.w);
    return result;
}

// Calculate distance between two vectors
RMAPI float Vector4Distance(Vector4 v1, Vector4 v2)
{
    float result = sqrtf(
        (v1.x - v2.x)*(v1.x - v2.x) + (v1.y - v2.y)*(v1.y - v2.y) +
        (v1.z - v2.z)*(v1.z - v2.z) + (v1.w - v2.w)*(v1.w - v2.w));
    return result;
}

// Calculate square distance between two vectors
RMAPI float Vector4DistanceSqr(Vector4 v1, Vector4 v2)
{
    float result =
        (v1.x - v2.x)*(v1.x - v2.x) + (v1.y - v2.y)*(v1.y - v2.y) +
        (v1.z - v2.z)*(v1.z - v2.z) + (v1.w - v2.w)*(v1.w - v2.w);

    return result;
}

RMAPI Vector4 Vector4Scale(Vector4 v, float scale)
{
    Vector4 result = { v.x*scale, v.y*scale, v.z*scale, v.w*scale };
    return result;
}

// Multiply vector by vector
RMAPI Vector4 Vector4Multiply(Vector4 v1, Vector4 v2)
{
    Vector4 result = { v1.x*v2.x, v1.y*v2.y, v1.z*v2.z, v1.w*v2.w };
    return result;
}

// Negate vector
RMAPI Vector4 Vector4Negate(Vector4 v)
{
    Vector4 result = { -v.x, -v.y, -v.z, -v.w };
    return result;
}

// Divide vector by vector
RMAPI Vector4 Vector4Divide(Vector4 v1, Vector4 v2)
{
    Vector4 result = { v1.x/v2.x, v1.y/v2.y, v1.z/v2.z, v1.w/v2.w };
    return result;
}

// Normalize provided vector
RMAPI Vector4 Vector4Normalize(Vector4 v)
{
    Vector4 result = { 0 };
    float length = sqrtf((v.x*v.x) + (v.y*v.y) + (v.z*v.z) + (v.w*v.w));

    if (length > 0)
    {
        float ilength = 1.0f/length;
        result.x = v.x*ilength;
        result.y = v.y*ilength;
        result.z = v.z*ilength;
        result.w = v.w*ilength;
    }

    return result;
}

// Get min value for each pair of components
RMAPI Vector4 Vector4Min(Vector4 v1, Vector4 v2)
{
    Vector4 result = { 0 };

    result.x = fminf(v1.x, v2.x);
    result.y = fminf(v1.y, v2.y);
    result.z = fminf(v1.z, v2.z);
    result.w = fminf(v1.w, v2.w);

    return result;
}

// Get max value for each pair of components
RMAPI Vector4 Vector4Max(Vector4 v1, Vector4 v2)
{
    Vector4 result = { 0 };

    result.x = fmaxf(v1.x, v2.x);
    result.y = fmaxf(v1.y, v2.y);
    result.z = fmaxf(v1.z, v2.z);
    result.w = fmaxf(v1.w, v2.w);

    return result;
}

// Calculate linear interpolation between two vectors
RMAPI Vector4 Vector4Lerp(Vector4 v1, Vector4 v2, float amount)
{
    Vector4 result = { 0 };

    result.x = v1.x + amount*(v2.x - v1.x);
    result.y = v1.y + amount*(v2.y - v1.y);
    result.z = v1.z + amount*(v2.z - v1.z);
    result.w = v1.w + amount*(v2.w - v1.w);

    return result;
}

// Move Vector towards target
RMAPI Vector4 Vector4MoveTowards(Vector4 v, Vector4 target, float maxDistance)
{
    Vector4 result = { 0 };

    float dx = target.x - v.x;
    float dy = target.y - v.y;
    float dz = target.z - v.z;
    float dw = target.w - v.w;
    float value = (dx*dx) + (dy*dy) + (dz*dz) + (dw*dw);

    if ((value == 0) || ((maxDistance >= 0) && (value <= maxDistance*maxDistance))) return target;

    float dist = sqrtf(value);

    result.x = v.x + dx/dist*maxDistance;
    result.y = v.y + dy/dist*maxDistance;
    result.z = v.z + dz/dist*maxDistance;
    result.w = v.w + dw/dist*maxDistance;

    return result;
}

// Invert the given vector
RMAPI Vector4 Vector4Invert(Vector4 v)
{
    Vector4 result = { 1.0f/v.x, 1.0f/v.y, 1.0f/v.z, 1.0f/v.w };
    return result;
}

// Check whether two given vectors are almost equal
RMAPI int Vector4Equals(Vector4 p, Vector4 q)
{
#if !defined(EPSILON)
    #define EPSILON 0.000001f
#endif

    int result = ((fabsf(p.x - q.x)) <= (EPSILON*fmaxf(1.0f, fmaxf(fabsf(p.x), fabsf(q.x))))) &&
                  ((fabsf(p.y - q.y)) <= (EPSILON*fmaxf(1.0f, fmaxf(fabsf(p.y), fabsf(q.y))))) &&
                  ((fabsf(p.z - q.z)) <= (EPSILON*fmaxf(1.0f, fmaxf(fabsf(p.z), fabsf(q.z))))) &&
                  ((fabsf(p.w - q.w)) <= (EPSILON*fmaxf(1.0f, fmaxf(fabsf(p.w), fabsf(q.w)))));
    return result;
}


//----------------------------------------------------------------------------------
// Module Functions Definition - Matrix math
//----------------------------------------------------------------------------------

// Compute matrix determinant
RMAPI float MatrixDeterminant(Matrix mat)
{
    float result = 0.0f;

    // Cache the matrix values (speed optimization)
    float a00 = mat.m0, a01 = mat.m1, a02 = mat.m2, a03 = mat.m3;
    float a10 = mat.m4, a11 = mat.m5, a12 = mat.m6, a13 = mat.m7;
    float a20 = mat.m8, a21 = mat.m9, a22 = mat.m10, a23 = mat.m11;
    float a30 = mat.m12, a31 = mat.m13, a32 = mat.m14, a33 = mat.m15;

    result = a30*a21*a12*a03 - a20*a31*a12*a03 - a30*a11*a22*a03 + a10*a31*a22*a03 +
             a20*a11*a32*a03 - a10*a21*a32*a03 - a30*a21*a02*a13 + a20*a31*a02*a13 +
             a30*a01*a22*a13 - a00*a31*a22*a13 - a20*a01*a32*a13 + a00*a21*a32*a13 +
             a30*a11*a02*a23 - a10*a31*a02*a23 - a30*a01*a12*a23 + a00*a31*a12*a23 +
             a10*a01*a32*a23 - a00*a11*a32*a23 - a20*a11*a02*a33 + a10*a21*a02*a33 +
             a20*a01*a12*a33 - a00*a21*a12*a33 - a10*a01*a22*a33 + a00*a11*a22*a33;

    return result;
}

// Get the trace of the matrix (sum of the values along the diagonal)
RMAPI float MatrixTrace(Matrix mat)
{
    float result = (mat.m0 + mat.m5 + mat.m10 + mat.m15);

    return result;
}

// Transposes provided matrix
RMAPI Matrix MatrixTranspose(Matrix mat)
{
    Matrix result = { 0 };

    result.m0 = mat.m0;
    result.m1 = mat.m4;
    result.m2 = mat.m8;
    result.m3 = mat.m12;
    result.m4 = mat.m1;
    result.m5 = mat.m5;
    result.m6 = mat.m9;
    result.m7 = mat.m13;
    result.m8 = mat.m2;
    result.m9 = mat.m6;
    result.m10 = mat.m10;
    result.m11 = mat.m14;
    result.m12 = mat.m3;
    result.m13 = mat.m7;
    result.m14 = mat.m11;
    result.m15 = mat.m15;

    return result;
}

// Invert provided matrix
RMAPI Matrix MatrixInvert(Matrix mat)
{
    Matrix result = { 0 };

    // Cache the matrix values (speed optimization)
    float a00 = mat.m0, a01 = mat.m1, a02 = mat.m2, a03 = mat.m3;
    float a10 = mat.m4, a11 = mat.m5, a12 = mat.m6, a13 = mat.m7;
    float a20 = mat.m8, a21 = mat.m9, a22 = mat.m10, a23 = mat.m11;
    float a30 = mat.m12, a31 = mat.m13, a32 = mat.m14, a33 = mat.m15;

    float b00 = a00*a11 - a01*a10;
    float b01 = a00*a12 - a02*a10;
    float b02 = a00*a13 - a03*a10;
    float b03 = a01*a12 - a02*a11;
    float b04 = a01*a13 - a03*a11;
    float b05 = a02*a13 - a03*a12;
    float b06 = a20*a31 - a21*a30;
    float b07 = a20*a32 - a22*a30;
    float b08 = a20*a33 - a23*a30;
    float b09 = a21*a32 - a22*a31;
    float b10 = a21*a33 - a23*a31;
    float b11 = a22*a33 - a23*a32;

    // Calculate the invert determinant (inlined to avoid double-caching)
    float invDet = 1.0f/(b00*b11 - b01*b10 + b02*b09 + b03*b08 - b04*b07 + b05*b06);

    result.m0 = (a11*b11 - a12*b10 + a13*b09)*invDet;
    result.m1 = (-a01*b11 + a02*b10 - a03*b09)*invDet;
    result.m2 = (a31*b05 - a32*b04 + a33*b03)*invDet;
    result.m3 = (-a21*b05 + a22*b04 - a23*b03)*invDet;
    result.m4 = (-a10*b11 + a12*b08 - a13*b07)*invDet;
    result.m5 = (a00*b11 - a02*b08 + a03*b07)*invDet;
    result.m6 = (-a30*b05 + a32*b02 - a33*b01)*invDet;
    result.m7 = (a20*b05 - a22*b02 + a23*b01)*invDet;
    result.m8 = (a10*b10 - a11*b08 + a13*b06)*invDet;
    result.m9 = (-a00*b10 + a01*b08 - a03*b06)*invDet;
    result.m10 = (a30*b04 - a31*b02 + a33*b00)*invDet;
    result.m11 = (-a20*b04 + a21*b02 - a23*b00)*invDet;
    result.m12 = (-a10*b09 + a11*b07 - a12*b06)*invDet;
    result.m13 = (a00*b09 - a01*b07 + a02*b06)*invDet;
    result.m14 = (-a30*b03 + a31*b01 - a32*b00)*invDet;
    result.m15 = (a20*b03 - a21*b01 + a22*b00)*invDet;

    return result;
}

// Get identity matrix
RMAPI Matrix MatrixIdentity(void)
{
    Matrix result = { 1.0f, 0.0f, 0.0f, 0.0f,
                      0.0f, 1.0f, 0.0f, 0.0f,
                      0.0f, 0.0f, 1.0f, 0.0f,
                      0.0f, 0.0f, 0.0f, 1.0f };

    return result;
}

// Add two matrices
RMAPI Matrix MatrixAdd(Matrix left, Matrix right)
{
    Matrix result = { 0 };

    result.m0 = left.m0 + right.m0;
    result.m1 = left.m1 + right.m1;
    result.m2 = left.m2 + right.m2;
    result.m3 = left.m3 + right.m3;
    result.m4 = left.m4 + right.m4;
    result.m5 = left.m5 + right.m5;
    result.m6 = left.m6 + right.m6;
    result.m7 = left.m7 + right.m7;
    result.m8 = left.m8 + right.m8;
    result.m9 = left.m9 + right.m9;
    result.m10 = left.m10 + right.m10;
    result.m11 = left.m11 + right.m11;
    result.m12 = left.m12 + right.m12;
    result.m13 = left.m13 + right.m13;
    result.m14 = left.m14 + right.m14;
    result.m15 = left.m15 + right.m15;

    return result;
}

// Subtract two matrices (left - right)
RMAPI Matrix MatrixSubtract(Matrix left, Matrix right)
{
    Matrix result = { 0 };

    result.m0 = left.m0 - right.m0;
    result.m1 = left.m1 - right.m1;
    result.m2 = left.m2 - right.m2;
    result.m3 = left.m3 - right.m3;
    result.m4 = left.m4 - right.m4;
    result.m5 = left.m5 - right.m5;
    result.m6 = left.m6 - right.m6;
    result.m7 = left.m7 - right.m7;
    result.m8 = left.m8 - right.m8;
    result.m9 = left.m9 - right.m9;
    result.m10 = left.m10 - right.m10;
    result.m11 = left.m11 - right.m11;
    result.m12 = left.m12 - right.m12;
    result.m13 = left.m13 - right.m13;
    result.m14 = left.m14 - right.m14;
    result.m15 = left.m15 - right.m15;

    return result;
}

// Get two matrix multiplication
// NOTE: When multiplying matrices... the order matters!
RMAPI Matrix MatrixMultiply(Matrix left, Matrix right)
{
    Matrix result = { 0 };

    result.m0 = left.m0*right.m0 + left.m1*right.m4 + left.m2*right.m8 + left.m3*right.m12;
    result.m1 = left.m0*right.m1 + left.m1*right.m5 + left.m2*right.m9 + left.m3*right.m13;
    result.m2 = left.m0*right.m2 + left.m1*right.m6 + left.m2*right.m10 + left.m3*right.m14;
    result.m3 = left.m0*right.m3 + left.m1*right.m7 + left.m2*right.m11 + left.m3*right.m15;
    result.m4 = left.m4*right.m0 + left.m5*right.m4 + left.m6*right.m8 + left.m7*right.m12;
    result.m5 = left.m4*right.m1 + left.m5*right.m5 + left.m6*right.m9 + left.m7*right.m13;
    result.m6 = left.m4*right.m2 + left.m5*right.m6 + left.m6*right.m10 + left.m7*right.m14;
    result.m7 = left.m4*right.m3 + left.m5*right.m7 + left.m6*right.m11 + left.m7*right.m15;
    result.m8 = left.m8*right.m0 + left.m9*right.m4 + left.m10*right.m8 + left.m11*right.m12;
    result.m9 = left.m8*right.m1 + left.m9*right.m5 + left.m10*right.m9 + left.m11*right.m13;
    result.m10 = left.m8*right.m2 + left.m9*right.m6 + left.m10*right.m10 + left.m11*right.m14;
    result.m11 = left.m8*right.m3 + left.m9*right.m7 + left.m10*right.m11 + left.m11*right.m15;
    result.m12 = left.m12*right.m0 + left.m13*right.m4 + left.m14*right.m8 + left.m15*right.m12;
    result.m13 = left.m12*right.m1 + left.m13*right.m5 + left.m14*right.m9 + left.m15*right.m13;
    result.m14 = left.m12*right.m2 + left.m13*right.m6 + left.m14*right.m10 + left.m15*right.m14;
    result.m15 = left.m12*right.m3 + left.m13*right.m7 + left.m14*right.m11 + left.m15*right.m15;

    return result;
}

// Get translation matrix
RMAPI Matrix MatrixTranslate(float x, float y, float z)
{
    Matrix result = { 1.0f, 0.0f, 0.0f, x,
                      0.0f, 1.0f, 0.0f, y,
                      0.0f, 0.0f, 1.0f, z,
                      0.0f, 0.0f, 0.0f, 1.0f };

    return result;
}

// Create rotation matrix from axis and angle
// NOTE: Angle should be provided in radians
RMAPI Matrix MatrixRotate(Vector3 axis, float angle)
{
    Matrix result = { 0 };

    float x = axis.x, y = axis.y, z = axis.z;

    float lengthSquared = x*x + y*y + z*z;

    if ((lengthSquared != 1.0f) && (lengthSquared != 0.0f))
    {
        float ilength = 1.0f/sqrtf(lengthSquared);
        x *= ilength;
        y *= ilength;
        z *= ilength;
    }

    float sinres = sinf(angle);
    float cosres = cosf(angle);
    float t = 1.0f - cosres;

    result.m0 = x*x*t + cosres;
    result.m1 = y*x*t + z*sinres;
    result.m2 = z*x*t - y*sinres;
    result.m3 = 0.0f;

    result.m4 = x*y*t - z*sinres;
    result.m5 = y*y*t + cosres;
    result.m6 = z*y*t + x*sinres;
    result.m7 = 0.0f;

    result.m8 = x*z*t + y*sinres;
    result.m9 = y*z*t - x*sinres;
    result.m10 = z*z*t + cosres;
    result.m11 = 0.0f;

    result.m12 = 0.0f;
    result.m13 = 0.0f;
    result.m14 = 0.0f;
    result.m15 = 1.0f;

    return result;
}

// Get x-rotation matrix
// NOTE: Angle must be provided in radians
RMAPI Matrix MatrixRotateX(float angle)
{
    Matrix result = { 1.0f, 0.0f, 0.0f, 0.0f,
                      0.0f, 1.0f, 0.0f, 0.0f,
                      0.0f, 0.0f, 1.0f, 0.0f,
                      0.0f, 0.0f, 0.0f, 1.0f }; // MatrixIdentity()

    float cosres = cosf(angle);
    float sinres = sinf(angle);

    result.m5 = cosres;
    result.m6 = sinres;
    result.m9 = -sinres;
    result.m10 = cosres;

    return result;
}

// Get y-rotation matrix
// NOTE: Angle must be provided in radians
RMAPI Matrix MatrixRotateY(float angle)
{
    Matrix result = { 1.0f, 0.0f, 0.0f, 0.0f,
                      0.0f, 1.0f, 0.0f, 0.0f,
                      0.0f, 0.0f, 1.0f, 0.0f,
                      0.0f, 0.0f, 0.0f, 1.0f }; // MatrixIdentity()

    float cosres = cosf(angle);
    float sinres = sinf(angle);

    result.m0 = cosres;
    result.m2 = -sinres;
    result.m8 = sinres;
    result.m10 = cosres;

    return result;
}

// Get z-rotation matrix
// NOTE: Angle must be provided in radians
RMAPI Matrix MatrixRotateZ(float angle)
{
    Matrix result = { 1.0f, 0.0f, 0.0f, 0.0f,
                      0.0f, 1.0f, 0.0f, 0.0f,
                      0.0f, 0.0f, 1.0f, 0.0f,
                      0.0f, 0.0f, 0.0f, 1.0f }; // MatrixIdentity()

    float cosres = cosf(angle);
    float sinres = sinf(angle);

    result.m0 = cosres;
    result.m1 = sinres;
    result.m4 = -sinres;
    result.m5 = cosres;

    return result;
}


// Get xyz-rotation matrix
// NOTE: Angle must be provided in radians
RMAPI Matrix MatrixRotateXYZ(Vector3 angle)
{
    Matrix result = { 1.0f, 0.0f, 0.0f, 0.0f,
                      0.0f, 1.0f, 0.0f, 0.0f,
                      0.0f, 0.0f, 1.0f, 0.0f,
                      0.0f, 0.0f, 0.0f, 1.0f }; // MatrixIdentity()

    float cosz = cosf(-angle.z);
    float sinz = sinf(-angle.z);
    float cosy = cosf(-angle.y);
    float siny = sinf(-angle.y);
    float cosx = cosf(-angle.x);
    float sinx = sinf(-angle.x);

    result.m0 = cosz*cosy;
    result.m1 = (cosz*siny*sinx) - (sinz*cosx);
    result.m2 = (cosz*siny*cosx) + (sinz*sinx);

    result.m4 = sinz*cosy;
    result.m5 = (sinz*siny*sinx) + (cosz*cosx);
    result.m6 = (sinz*siny*cosx) - (cosz*sinx);

    result.m8 = -siny;
    result.m9 = cosy*sinx;
    result.m10= cosy*cosx;

    return result;
}

// Get zyx-rotation matrix
// NOTE: Angle must be provided in radians
RMAPI Matrix MatrixRotateZYX(Vector3 angle)
{
    Matrix result = { 0 };

    float cz = cosf(angle.z);
    float sz = sinf(angle.z);
    float cy = cosf(angle.y);
    float sy = sinf(angle.y);
    float cx = cosf(angle.x);
    float sx = sinf(angle.x);

    result.m0 = cz*cy;
    result.m4 = cz*sy*sx - cx*sz;
    result.m8 = sz*sx + cz*cx*sy;
    result.m12 = 0;

    result.m1 = cy*sz;
    result.m5 = cz*cx + sz*sy*sx;
    result.m9 = cx*sz*sy - cz*sx;
    result.m13 = 0;

    result.m2 = -sy;
    result.m6 = cy*sx;
    result.m10 = cy*cx;
    result.m14 = 0;

    result.m3 = 0;
    result.m7 = 0;
    result.m11 = 0;
    result.m15 = 1;

    return result;
}

// Get scaling matrix
RMAPI Matrix MatrixScale(float x, float y, float z)
{
    Matrix result = { x, 0.0f, 0.0f, 0.0f,
                      0.0f, y, 0.0f, 0.0f,
                      0.0f, 0.0f, z, 0.0f,
                      0.0f, 0.0f, 0.0f, 1.0f };

    return result;
}

// Get perspective projection matrix
RMAPI Matrix MatrixFrustum(double left, double right, double bottom, double top, double nearPlane, double farPlane)
{
    Matrix result = { 0 };

    float rl = (float)(right - left);
    float tb = (float)(top - bottom);
    float fn = (float)(farPlane - nearPlane);

    result.m0 = ((float)nearPlane*2.0f)/rl;
    result.m1 = 0.0f;
    result.m2 = 0.0f;
    result.m3 = 0.0f;

    result.m4 = 0.0f;
    result.m5 = ((float)nearPlane*2.0f)/tb;
    result.m6 = 0.0f;
    result.m7 = 0.0f;

    result.m8 = ((float)right + (float)left)/rl;
    result.m9 = ((float)top + (float)bottom)/tb;
    result.m10 = -((float)farPlane + (float)nearPlane)/fn;
    result.m11 = -1.0f;

    result.m12 = 0.0f;
    result.m13 = 0.0f;
    result.m14 = -((float)farPlane*(float)nearPlane*2.0f)/fn;
    result.m15 = 0.0f;

    return result;
}

// Get perspective projection matrix
// NOTE: Fovy angle must be provided in radians
RMAPI Matrix MatrixPerspective(double fovY, double aspect, double nearPlane, double farPlane)
{
    Matrix result = { 0 };

    double top = nearPlane*tan(fovY*0.5);
    double bottom = -top;
    double right = top*aspect;
    double left = -right;

    // MatrixFrustum(-right, right, -top, top, near, far);
    float rl = (float)(right - left);
    float tb = (float)(top - bottom);
    float fn = (float)(farPlane - nearPlane);

    result.m0 = ((float)nearPlane*2.0f)/rl;
    result.m5 = ((float)nearPlane*2.0f)/tb;
    result.m8 = ((float)right + (float)left)/rl;
    result.m9 = ((float)top + (float)bottom)/tb;
    result.m10 = -((float)farPlane + (float)nearPlane)/fn;
    result.m11 = -1.0f;
    result.m14 = -((float)farPlane*(float)nearPlane*2.0f)/fn;

    return result;
}

// Get orthographic projection matrix
RMAPI Matrix MatrixOrtho(double left, double right, double bottom, double top, double nearPlane, double farPlane)
{
    Matrix result = { 0 };

    float rl = (float)(right - left);
    float tb = (float)(top - bottom);
    float fn = (float)(farPlane - nearPlane);

    result.m0 = 2.0f/rl;
    result.m1 = 0.0f;
    result.m2 = 0.0f;
    result.m3 = 0.0f;
    result.m4 = 0.0f;
    result.m5 = 2.0f/tb;
    result.m6 = 0.0f;
    result.m7 = 0.0f;
    result.m8 = 0.0f;
    result.m9 = 0.0f;
    result.m10 = -2.0f/fn;
    result.m11 = 0.0f;
    result.m12 = -((float)left + (float)right)/rl;
    result.m13 = -((float)top + (float)bottom)/tb;
    result.m14 = -((float)farPlane + (float)nearPlane)/fn;
    result.m15 = 1.0f;

    return result;
}

// Get camera look-at matrix (view matrix)
RMAPI Matrix MatrixLookAt(Vector3 eye, Vector3 target, Vector3 up)
{
    Matrix result = { 0 };

    float length = 0.0f;
    float ilength = 0.0f;

    // Vector3Subtract(eye, target)
    Vector3 vz = { eye.x - target.x, eye.y - target.y, eye.z - target.z };

    // Vector3Normalize(vz)
    Vector3 v = vz;
    length = sqrtf(v.x*v.x + v.y*v.y + v.z*v.z);
    if (length == 0.0f) length = 1.0f;
    ilength = 1.0f/length;
    vz.x *= ilength;
    vz.y *= ilength;
    vz.z *= ilength;

    // Vector3CrossProduct(up, vz)
    Vector3 vx = { up.y*vz.z - up.z*vz.y, up.z*vz.x - up.x*vz.z, up.x*vz.y - up.y*vz.x };

    // Vector3Normalize(x)
    v = vx;
    length = sqrtf(v.x*v.x + v.y*v.y + v.z*v.z);
    if (length == 0.0f) length = 1.0f;
    ilength = 1.0f/length;
    vx.x *= ilength;
    vx.y *= ilength;
    vx.z *= ilength;

    // Vector3CrossProduct(vz, vx)
    Vector3 vy = { vz.y*vx.z - vz.z*vx.y, vz.z*vx.x - vz.x*vx.z, vz.x*vx.y - vz.y*vx.x };

    result.m0 = vx.x;
    result.m1 = vy.x;
    result.m2 = vz.x;
    result.m3 = 0.0f;
    result.m4 = vx.y;
    result.m5 = vy.y;
    result.m6 = vz.y;
    result.m7 = 0.0f;
    result.m8 = vx.z;
    result.m9 = vy.z;
    result.m10 = vz.z;
    result.m11 = 0.0f;
    result.m12 = -(vx.x*eye.x + vx.y*eye.y + vx.z*eye.z);   // Vector3DotProduct(vx, eye)
    result.m13 = -(vy.x*eye.x + vy.y*eye.y + vy.z*eye.z);   // Vector3DotProduct(vy, eye)
    result.m14 = -(vz.x*eye.x + vz.y*eye.y + vz.z*eye.z);   // Vector3DotProduct(vz, eye)
    result.m15 = 1.0f;

    return result;
}

// Get float array of matrix data
RMAPI float16 MatrixToFloatV(Matrix mat)
{
    float16 result = { 0 };

    result.v[0] = mat.m0;
    result.v[1] = mat.m1;
    result.v[2] = mat.m2;
    result.v[3] = mat.m3;
    result.v[4] = mat.m4;
    result.v[5] = mat.m5;
    result.v[6] = mat.m6;
    result.v[7] = mat.m7;
    result.v[8] = mat.m8;
    result.v[9] = mat.m9;
    result.v[10] = mat.m10;
    result.v[11] = mat.m11;
    result.v[12] = mat.m12;
    result.v[13] = mat.m13;
    result.v[14] = mat.m14;
    result.v[15] = mat.m15;

    return result;
}

//----------------------------------------------------------------------------------
// Module Functions Definition - Quaternion math
//----------------------------------------------------------------------------------

// Add two quaternions
RMAPI Quaternion QuaternionAdd(Quaternion q1, Quaternion q2)
{
    Quaternion result = {q1.x + q2.x, q1.y + q2.y, q1.z + q2.z, q1.w + q2.w};

    return result;
}

// Add quaternion and float value
RMAPI Quaternion QuaternionAddValue(Quaternion q, float add)
{
    Quaternion result = {q.x + add, q.y + add, q.z + add, q.w + add};

    return result;
}

// Subtract two quaternions
RMAPI Quaternion QuaternionSubtract(Quaternion q1, Quaternion q2)
{
    Quaternion result = {q1.x - q2.x, q1.y - q2.y, q1.z - q2.z, q1.w - q2.w};

    return result;
}

// Subtract quaternion and float value
RMAPI Quaternion QuaternionSubtractValue(Quaternion q, float sub)
{
    Quaternion result = {q.x - sub, q.y - sub, q.z - sub, q.w - sub};

    return result;
}

// Get identity quaternion
RMAPI Quaternion QuaternionIdentity(void)
{
    Quaternion result = { 0.0f, 0.0f, 0.0f, 1.0f };

    return result;
}

// Computes the length of a quaternion
RMAPI float QuaternionLength(Quaternion q)
{
    float result = sqrtf(q.x*q.x + q.y*q.y + q.z*q.z + q.w*q.w);

    return result;
}

// Normalize provided quaternion
RMAPI Quaternion QuaternionNormalize(Quaternion q)
{
    Quaternion result = { 0 };

    float length = sqrtf(q.x*q.x + q.y*q.y + q.z*q.z + q.w*q.w);
    if (length == 0.0f) length = 1.0f;
    float ilength = 1.0f/length;

    result.x = q.x*ilength;
    result.y = q.y*ilength;
    result.z = q.z*ilength;
    result.w = q.w*ilength;

    return result;
}

// Invert provided quaternion
RMAPI Quaternion QuaternionInvert(Quaternion q)
{
    Quaternion result = q;

    float lengthSq = q.x*q.x + q.y*q.y + q.z*q.z + q.w*q.w;

    if (lengthSq != 0.0f)
    {
        float invLength = 1.0f/lengthSq;

        result.x *= -invLength;
        result.y *= -invLength;
        result.z *= -invLength;
        result.w *= invLength;
    }

    return result;
}

// Calculate two quaternion multiplication
RMAPI Quaternion QuaternionMultiply(Quaternion q1, Quaternion q2)
{
    Quaternion result = { 0 };

    float qax = q1.x, qay = q1.y, qaz = q1.z, qaw = q1.w;
    float qbx = q2.x, qby = q2.y, qbz = q2.z, qbw = q2.w;

    result.x = qax*qbw + qaw*qbx + qay*qbz - qaz*qby;
    result.y = qay*qbw + qaw*qby + qaz*qbx - qax*qbz;
    result.z = qaz*qbw + qaw*qbz + qax*qby - qay*qbx;
    result.w = qaw*qbw - qax*qbx - qay*qby - qaz*qbz;

    return result;
}

// Scale quaternion by float value
RMAPI Quaternion QuaternionScale(Quaternion q, float mul)
{
    Quaternion result = { 0 };

    result.x = q.x*mul;
    result.y = q.y*mul;
    result.z = q.z*mul;
    result.w = q.w*mul;

    return result;
}

// Divide two quaternions
RMAPI Quaternion QuaternionDivide(Quaternion q1, Quaternion q2)
{
    Quaternion result = { q1.x/q2.x, q1.y/q2.y, q1.z/q2.z, q1.w/q2.w };

    return result;
}

// Calculate linear interpolation between two quaternions
RMAPI Quaternion QuaternionLerp(Quaternion q1, Quaternion q2, float amount)
{
    Quaternion result = { 0 };

    result.x = q1.x + amount*(q2.x - q1.x);
    result.y = q1.y + amount*(q2.y - q1.y);
    result.z = q1.z + amount*(q2.z - q1.z);
    result.w = q1.w + amount*(q2.w - q1.w);

    return result;
}

// Calculate slerp-optimized interpolation between two quaternions
RMAPI Quaternion QuaternionNlerp(Quaternion q1, Quaternion q2, float amount)
{
    Quaternion result = { 0 };

    // QuaternionLerp(q1, q2, amount)
    result.x = q1.x + amount*(q2.x - q1.x);
    result.y = q1.y + amount*(q2.y - q1.y);
    result.z = q1.z + amount*(q2.z - q1.z);
    result.w = q1.w + amount*(q2.w - q1.w);

    // QuaternionNormalize(q);
    Quaternion q = result;
    float length = sqrtf(q.x*q.x + q.y*q.y + q.z*q.z + q.w*q.w);
    if (length == 0.0f) length = 1.0f;
    float ilength = 1.0f/length;

    result.x = q.x*ilength;
    result.y = q.y*ilength;
    result.z = q.z*ilength;
    result.w = q.w*ilength;

    return result;
}

// Calculates spherical linear interpolation between two quaternions
RMAPI Quaternion QuaternionSlerp(Quaternion q1, Quaternion q2, float amount)
{
    Quaternion result = { 0 };

#if !defined(EPSILON)
    #define EPSILON 0.000001f
#endif

    float cosHalfTheta = q1.x*q2.x + q1.y*q2.y + q1.z*q2.z + q1.w*q2.w;

    if (cosHalfTheta < 0)
    {
        q2.x = -q2.x; q2.y = -q2.y; q2.z = -q2.z; q2.w = -q2.w;
        cosHalfTheta = -cosHalfTheta;
    }

    if (fabsf(cosHalfTheta) >= 1.0f) result = q1;
    else if (cosHalfTheta > 0.95f) result = QuaternionNlerp(q1, q2, amount);
    else
    {
        float halfTheta = acosf(cosHalfTheta);
        float sinHalfTheta = sqrtf(1.0f - cosHalfTheta*cosHalfTheta);

        if (fabsf(sinHalfTheta) < EPSILON)
        {
            result.x = (q1.x*0.5f + q2.x*0.5f);
            result.y = (q1.y*0.5f + q2.y*0.5f);
            result.z = (q1.z*0.5f + q2.z*0.5f);
            result.w = (q1.w*0.5f + q2.w*0.5f);
        }
        else
        {
            float ratioA = sinf((1 - amount)*halfTheta)/sinHalfTheta;
            float ratioB = sinf(amount*halfTheta)/sinHalfTheta;

            result.x = (q1.x*ratioA + q2.x*ratioB);
            result.y = (q1.y*ratioA + q2.y*ratioB);
            result.z = (q1.z*ratioA + q2.z*ratioB);
            result.w = (q1.w*ratioA + q2.w*ratioB);
        }
    }

    return result;
}

// Calculate quaternion cubic spline interpolation using Cubic Hermite Spline algorithm
// as described in the GLTF 2.0 specification: https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#interpolation-cubic
RMAPI Quaternion QuaternionCubicHermiteSpline(Quaternion q1, Quaternion outTangent1, Quaternion q2, Quaternion inTangent2, float t)
{
    float t2 = t*t;
    float t3 = t2*t;
    float h00 = 2*t3 - 3*t2 + 1;
    float h10 = t3 - 2*t2 + t;
    float h01 = -2*t3 + 3*t2;
    float h11 = t3 - t2;

    Quaternion p0 = QuaternionScale(q1, h00);
    Quaternion m0 = QuaternionScale(outTangent1, h10);
    Quaternion p1 = QuaternionScale(q2, h01);
    Quaternion m1 = QuaternionScale(inTangent2, h11);

    Quaternion result = { 0 };

    result = QuaternionAdd(p0, m0);
    result = QuaternionAdd(result, p1);
    result = QuaternionAdd(result, m1);
    result = QuaternionNormalize(result);

    return result;
}

// Calculate quaternion based on the rotation from one vector to another
RMAPI Quaternion QuaternionFromVector3ToVector3(Vector3 from, Vector3 to)
{
    Quaternion result = { 0 };

    float cos2Theta = (from.x*to.x + from.y*to.y + from.z*to.z);    // Vector3DotProduct(from, to)
    Vector3 cross = { from.y*to.z - from.z*to.y, from.z*to.x - from.x*to.z, from.x*to.y - from.y*to.x }; // Vector3CrossProduct(from, to)

    result.x = cross.x;
    result.y = cross.y;
    result.z = cross.z;
    result.w = 1.0f + cos2Theta;

    // QuaternionNormalize(q);
    // NOTE: Normalize to essentially nlerp the original and identity to 0.5
    Quaternion q = result;
    float length = sqrtf(q.x*q.x + q.y*q.y + q.z*q.z + q.w*q.w);
    if (length == 0.0f) length = 1.0f;
    float ilength = 1.0f/length;

    result.x = q.x*ilength;
    result.y = q.y*ilength;
    result.z = q.z*ilength;
    result.w = q.w*ilength;

    return result;
}

// Get a quaternion for a given rotation matrix
RMAPI Quaternion QuaternionFromMatrix(Matrix mat)
{
    Quaternion result = { 0 };

    float fourWSquaredMinus1 = mat.m0  + mat.m5 + mat.m10;
    float fourXSquaredMinus1 = mat.m0  - mat.m5 - mat.m10;
    float fourYSquaredMinus1 = mat.m5  - mat.m0 - mat.m10;
    float fourZSquaredMinus1 = mat.m10 - mat.m0 - mat.m5;

    int biggestIndex = 0;
    float fourBiggestSquaredMinus1 = fourWSquaredMinus1;
    if (fourXSquaredMinus1 > fourBiggestSquaredMinus1)
    {
        fourBiggestSquaredMinus1 = fourXSquaredMinus1;
        biggestIndex = 1;
    }

    if (fourYSquaredMinus1 > fourBiggestSquaredMinus1)
    {
        fourBiggestSquaredMinus1 = fourYSquaredMinus1;
        biggestIndex = 2;
    }

    if (fourZSquaredMinus1 > fourBiggestSquaredMinus1)
    {
        fourBiggestSquaredMinus1 = fourZSquaredMinus1;
        biggestIndex = 3;
    }

    float biggestVal = sqrtf(fourBiggestSquaredMinus1 + 1.0f)*0.5f;
    float mult = 0.25f/biggestVal;

    switch (biggestIndex)
    {
        case 0:
            result.w = biggestVal;
            result.x = (mat.m6 - mat.m9)*mult;
            result.y = (mat.m8 - mat.m2)*mult;
            result.z = (mat.m1 - mat.m4)*mult;
            break;
        case 1:
            result.x = biggestVal;
            result.w = (mat.m6 - mat.m9)*mult;
            result.y = (mat.m1 + mat.m4)*mult;
            result.z = (mat.m8 + mat.m2)*mult;
            break;
        case 2:
            result.y = biggestVal;
            result.w = (mat.m8 - mat.m2)*mult;
            result.x = (mat.m1 + mat.m4)*mult;
            result.z = (mat.m6 + mat.m9)*mult;
            break;
        case 3:
            result.z = biggestVal;
            result.w = (mat.m1 - mat.m4)*mult;
            result.x = (mat.m8 + mat.m2)*mult;
            result.y = (mat.m6 + mat.m9)*mult;
            break;
    }

    return result;
}

// Get a matrix for a given quaternion
RMAPI Matrix QuaternionToMatrix(Quaternion q)
{
    Matrix result = { 1.0f, 0.0f, 0.0f, 0.0f,
                      0.0f, 1.0f, 0.0f, 0.0f,
                      0.0f, 0.0f, 1.0f, 0.0f,
                      0.0f, 0.0f, 0.0f, 1.0f }; // MatrixIdentity()

    float a2 = q.x*q.x;
    float b2 = q.y*q.y;
    float c2 = q.z*q.z;
    float ac = q.x*q.z;
    float ab = q.x*q.y;
    float bc = q.y*q.z;
    float ad = q.w*q.x;
    float bd = q.w*q.y;
    float cd = q.w*q.z;

    result.m0 = 1 - 2*(b2 + c2);
    result.m1 = 2*(ab + cd);
    result.m2 = 2*(ac - bd);

    result.m4 = 2*(ab - cd);
    result.m5 = 1 - 2*(a2 + c2);
    result.m6 = 2*(bc + ad);

    result.m8 = 2*(ac + bd);
    result.m9 = 2*(bc - ad);
    result.m10 = 1 - 2*(a2 + b2);

    return result;
}

// Get rotation quaternion for an angle and axis
// NOTE: Angle must be provided in radians
RMAPI Quaternion QuaternionFromAxisAngle(Vector3 axis, float angle)
{
    Quaternion result = { 0.0f, 0.0f, 0.0f, 1.0f };

    float axisLength = sqrtf(axis.x*axis.x + axis.y*axis.y + axis.z*axis.z);

    if (axisLength != 0.0f)
    {
        angle *= 0.5f;

        float length = 0.0f;
        float ilength = 0.0f;

        // Vector3Normalize(axis)
        length = axisLength;
        if (length == 0.0f) length = 1.0f;
        ilength = 1.0f/length;
        axis.x *= ilength;
        axis.y *= ilength;
        axis.z *= ilength;

        float sinres = sinf(angle);
        float cosres = cosf(angle);

        result.x = axis.x*sinres;
        result.y = axis.y*sinres;
        result.z = axis.z*sinres;
        result.w = cosres;

        // QuaternionNormalize(q);
        Quaternion q = result;
        length = sqrtf(q.x*q.x + q.y*q.y + q.z*q.z + q.w*q.w);
        if (length == 0.0f) length = 1.0f;
        ilength = 1.0f/length;
        result.x = q.x*ilength;
        result.y = q.y*ilength;
        result.z = q.z*ilength;
        result.w = q.w*ilength;
    }

    return result;
}

// Get the rotation angle and axis for a given quaternion
RMAPI void QuaternionToAxisAngle(Quaternion q, Vector3 *outAxis, float *outAngle)
{
    if (fabsf(q.w) > 1.0f)
    {
        // QuaternionNormalize(q);
        float length = sqrtf(q.x*q.x + q.y*q.y + q.z*q.z + q.w*q.w);
        if (length == 0.0f) length = 1.0f;
        float ilength = 1.0f/length;

        q.x = q.x*ilength;
        q.y = q.y*ilength;
        q.z = q.z*ilength;
        q.w = q.w*ilength;
    }

    Vector3 resAxis = { 0.0f, 0.0f, 0.0f };
    float resAngle = 2.0f*acosf(q.w);
    float den = sqrtf(1.0f - q.w*q.w);

    if (den > EPSILON)
    {
        resAxis.x = q.x/den;
        resAxis.y = q.y/den;
        resAxis.z = q.z/den;
    }
    else
    {
        // This occurs when the angle is zero.
        // Not a problem: just set an arbitrary normalized axis.
        resAxis.x = 1.0f;
    }

    *outAxis = resAxis;
    *outAngle = resAngle;
}

// Get the quaternion equivalent to Euler angles
// NOTE: Rotation order is ZYX
RMAPI Quaternion QuaternionFromEuler(float pitch, float yaw, float roll)
{
    Quaternion result = { 0 };

    float x0 = cosf(pitch*0.5f);
    float x1 = sinf(pitch*0.5f);
    float y0 = cosf(yaw*0.5f);
    float y1 = sinf(yaw*0.5f);
    float z0 = cosf(roll*0.5f);
    float z1 = sinf(roll*0.5f);

    result.x = x1*y0*z0 - x0*y1*z1;
    result.y = x0*y1*z0 + x1*y0*z1;
    result.z = x0*y0*z1 - x1*y1*z0;
    result.w = x0*y0*z0 + x1*y1*z1;

    return result;
}

// Get the Euler angles equivalent to quaternion (roll, pitch, yaw)
// NOTE: Angles are returned in a Vector3 struct in radians
RMAPI Vector3 QuaternionToEuler(Quaternion q)
{
    Vector3 result = { 0 };

    // Roll (x-axis rotation)
    float x0 = 2.0f*(q.w*q.x + q.y*q.z);
    float x1 = 1.0f - 2.0f*(q.x*q.x + q.y*q.y);
    result.x = atan2f(x0, x1);

    // Pitch (y-axis rotation)
    float y0 = 2.0f*(q.w*q.y - q.z*q.x);
    y0 = y0 > 1.0f ? 1.0f : y0;
    y0 = y0 < -1.0f ? -1.0f : y0;
    result.y = asinf(y0);

    // Yaw (z-axis rotation)
    float z0 = 2.0f*(q.w*q.z + q.x*q.y);
    float z1 = 1.0f - 2.0f*(q.y*q.y + q.z*q.z);
    result.z = atan2f(z0, z1);

    return result;
}

// Transform a quaternion given a transformation matrix
RMAPI Quaternion QuaternionTransform(Quaternion q, Matrix mat)
{
    Quaternion result = { 0 };

    result.x = mat.m0*q.x + mat.m4*q.y + mat.m8*q.z + mat.m12*q.w;
    result.y = mat.m1*q.x + mat.m5*q.y + mat.m9*q.z + mat.m13*q.w;
    result.z = mat.m2*q.x + mat.m6*q.y + mat.m10*q.z + mat.m14*q.w;
    result.w = mat.m3*q.x + mat.m7*q.y + mat.m11*q.z + mat.m15*q.w;

    return result;
}

// Check whether two given quaternions are almost equal
RMAPI int QuaternionEquals(Quaternion p, Quaternion q)
{
#if !defined(EPSILON)
    #define EPSILON 0.000001f
#endif

    int result = (((fabsf(p.x - q.x)) <= (EPSILON*fmaxf(1.0f, fmaxf(fabsf(p.x), fabsf(q.x))))) &&
                  ((fabsf(p.y - q.y)) <= (EPSILON*fmaxf(1.0f, fmaxf(fabsf(p.y), fabsf(q.y))))) &&
                  ((fabsf(p.z - q.z)) <= (EPSILON*fmaxf(1.0f, fmaxf(fabsf(p.z), fabsf(q.z))))) &&
                  ((fabsf(p.w - q.w)) <= (EPSILON*fmaxf(1.0f, fmaxf(fabsf(p.w), fabsf(q.w)))))) ||
                 (((fabsf(p.x + q.x)) <= (EPSILON*fmaxf(1.0f, fmaxf(fabsf(p.x), fabsf(q.x))))) &&
                  ((fabsf(p.y + q.y)) <= (EPSILON*fmaxf(1.0f, fmaxf(fabsf(p.y), fabsf(q.y))))) &&
                  ((fabsf(p.z + q.z)) <= (EPSILON*fmaxf(1.0f, fmaxf(fabsf(p.z), fabsf(q.z))))) &&
                  ((fabsf(p.w + q.w)) <= (EPSILON*fmaxf(1.0f, fmaxf(fabsf(p.w), fabsf(q.w))))));

    return result;
}

// Decompose a transformation matrix into its rotational, translational and scaling components
RMAPI void MatrixDecompose(Matrix mat, Vector3 *translation, Quaternion *rotation, Vector3 *scale)
{
    // Extract translation.
    translation->x = mat.m12;
    translation->y = mat.m13;
    translation->z = mat.m14;

    // Extract upper-left for determinant computation
    const float a = mat.m0;
    const float b = mat.m4;
    const float c = mat.m8;
    const float d = mat.m1;
    const float e = mat.m5;
    const float f = mat.m9;
    const float g = mat.m2;
    const float h = mat.m6;
    const float i = mat.m10;
    const float A = e*i - f*h;
    const float B = f*g - d*i;
    const float C = d*h - e*g;

    // Extract scale
    const float det = a*A + b*B + c*C;
    Vector3 abc = { a, b, c };
    Vector3 def = { d, e, f };
    Vector3 ghi = { g, h, i };

    float scalex = Vector3Length(abc);
    float scaley = Vector3Length(def);
    float scalez = Vector3Length(ghi);
    Vector3 s = { scalex, scaley, scalez };

    if (det < 0) s = Vector3Negate(s);

    *scale = s;

    // Remove scale from the matrix if it is not close to zero
    Matrix clone = mat;
    if (!FloatEquals(det, 0))
    {
        clone.m0 /= s.x;
        clone.m4 /= s.x;
        clone.m8 /= s.x;
        clone.m1 /= s.y;
        clone.m5 /= s.y;
        clone.m9 /= s.y;
        clone.m2 /= s.z;
        clone.m6 /= s.z;
        clone.m10 /= s.z;

        // Extract rotation
        *rotation = QuaternionFromMatrix(clone);
    }
    else
    {
        // Set to identity if close to zero
        *rotation = QuaternionIdentity();
    }
}

#if defined(__cplusplus) && !defined(RAYMATH_DISABLE_CPP_OPERATORS)

// Optional C++ math operators
//-------------------------------------------------------------------------------

// Vector2 operators
static constexpr Vector2 Vector2Zeros = { 0, 0 };
static constexpr Vector2 Vector2Ones = { 1, 1 };
static constexpr Vector2 Vector2UnitX = { 1, 0 };
static constexpr Vector2 Vector2UnitY = { 0, 1 };

inline Vector2 operator + (const Vector2& lhs, const Vector2& rhs)
{
    return Vector2Add(lhs, rhs);
}

inline const Vector2& operator += (Vector2& lhs, const Vector2& rhs)
{
    lhs = Vector2Add(lhs, rhs);
    return lhs;
}

inline Vector2 operator - (const Vector2& lhs, const Vector2& rhs)
{
    return Vector2Subtract(lhs, rhs);
}

inline const Vector2& operator -= (Vector2& lhs, const Vector2& rhs)
{
    lhs = Vector2Subtract(lhs, rhs);
    return lhs;
}

inline Vector2 operator * (const Vector2& lhs, const float& rhs)
{
    return Vector2Scale(lhs, rhs);
}

inline const Vector2& operator *= (Vector2& lhs, const float& rhs)
{
    lhs = Vector2Scale(lhs, rhs);
    return lhs;
}

inline Vector2 operator * (const Vector2& lhs, const Vector2& rhs)
{
    return Vector2Multiply(lhs, rhs);
}

inline const Vector2& operator *= (Vector2& lhs, const Vector2& rhs)
{
    lhs = Vector2Multiply(lhs, rhs);
    return lhs;
}

inline Vector2 operator * (const Vector2& lhs, const Matrix& rhs)
{
    return Vector2Transform(lhs, rhs);
}

inline const Vector2& operator *= (Vector2& lhs, const Matrix& rhs)
{
    lhs = Vector2Transform(lhs, rhs);
    return lhs;
}

inline Vector2 operator / (const Vector2& lhs, const float& rhs)
{
    return Vector2Scale(lhs, 1.0f / rhs);
}

inline const Vector2& operator /= (Vector2& lhs, const float& rhs)
{
    lhs = Vector2Scale(lhs, 1.0f / rhs);
    return lhs;
}

inline Vector2 operator / (const Vector2& lhs, const Vector2& rhs)
{
    return Vector2Divide(lhs, rhs);
}

inline const Vector2& operator /= (Vector2& lhs, const Vector2& rhs)
{
    lhs = Vector2Divide(lhs, rhs);
    return lhs;
}

inline bool operator == (const Vector2& lhs, const Vector2& rhs)
{
    return FloatEquals(lhs.x, rhs.x) && FloatEquals(lhs.y, rhs.y);
}

inline bool operator != (const Vector2& lhs, const Vector2& rhs)
{
    return !FloatEquals(lhs.x, rhs.x) || !FloatEquals(lhs.y, rhs.y);
}

// Vector3 operators
static constexpr Vector3 Vector3Zeros = { 0, 0, 0 };
static constexpr Vector3 Vector3Ones = { 1, 1, 1 };
static constexpr Vector3 Vector3UnitX = { 1, 0, 0 };
static constexpr Vector3 Vector3UnitY = { 0, 1, 0 };
static constexpr Vector3 Vector3UnitZ = { 0, 0, 1 };

inline Vector3 operator + (const Vector3& lhs, const Vector3& rhs)
{
    return Vector3Add(lhs, rhs);
}

inline const Vector3& operator += (Vector3& lhs, const Vector3& rhs)
{
    lhs = Vector3Add(lhs, rhs);
    return lhs;
}

inline Vector3 operator - (const Vector3& lhs, const Vector3& rhs)
{
    return Vector3Subtract(lhs, rhs);
}

inline const Vector3& operator -= (Vector3& lhs, const Vector3& rhs)
{
    lhs = Vector3Subtract(lhs, rhs);
    return lhs;
}

inline Vector3 operator * (const Vector3& lhs, const float& rhs)
{
    return Vector3Scale(lhs, rhs);
}

inline const Vector3& operator *= (Vector3& lhs, const float& rhs)
{
    lhs = Vector3Scale(lhs, rhs);
    return lhs;
}

inline Vector3 operator * (const Vector3& lhs, const Vector3& rhs)
{
    return Vector3Multiply(lhs, rhs);
}

inline const Vector3& operator *= (Vector3& lhs, const Vector3& rhs)
{
    lhs = Vector3Multiply(lhs, rhs);
    return lhs;
}

inline Vector3 operator * (const Vector3& lhs, const Matrix& rhs)
{
    return Vector3Transform(lhs, rhs);
}

inline const Vector3& operator *= (Vector3& lhs, const Matrix& rhs)
{
    lhs = Vector3Transform(lhs, rhs);
    return lhs;
}

inline Vector3 operator / (const Vector3& lhs, const float& rhs)
{
    return Vector3Scale(lhs, 1.0f / rhs);
}

inline const Vector3& operator /= (Vector3& lhs, const float& rhs)
{
    lhs = Vector3Scale(lhs, 1.0f / rhs);
    return lhs;
}

inline Vector3 operator / (const Vector3& lhs, const Vector3& rhs)
{
    return Vector3Divide(lhs, rhs);
}

inline const Vector3& operator /= (Vector3& lhs, const Vector3& rhs)
{
    lhs = Vector3Divide(lhs, rhs);
    return lhs;
}

inline bool operator == (const Vector3& lhs, const Vector3& rhs)
{
    return FloatEquals(lhs.x, rhs.x) && FloatEquals(lhs.y, rhs.y) && FloatEquals(lhs.z, rhs.z);
}

inline bool operator != (const Vector3& lhs, const Vector3& rhs)
{
    return !FloatEquals(lhs.x, rhs.x) || !FloatEquals(lhs.y, rhs.y) || !FloatEquals(lhs.z, rhs.z);
}

// Vector4 operators
static constexpr Vector4 Vector4Zeros = { 0, 0, 0, 0 };
static constexpr Vector4 Vector4Ones = { 1, 1, 1, 1 };
static constexpr Vector4 Vector4UnitX = { 1, 0, 0, 0 };
static constexpr Vector4 Vector4UnitY = { 0, 1, 0, 0 };
static constexpr Vector4 Vector4UnitZ = { 0, 0, 1, 0 };
static constexpr Vector4 Vector4UnitW = { 0, 0, 0, 1 };

inline Vector4 operator + (const Vector4& lhs, const Vector4& rhs)
{
    return Vector4Add(lhs, rhs);
}

inline const Vector4& operator += (Vector4& lhs, const Vector4& rhs)
{
    lhs = Vector4Add(lhs, rhs);
    return lhs;
}

inline Vector4 operator - (const Vector4& lhs, const Vector4& rhs)
{
    return Vector4Subtract(lhs, rhs);
}

inline const Vector4& operator -= (Vector4& lhs, const Vector4& rhs)
{
    lhs = Vector4Subtract(lhs, rhs);
    return lhs;
}

inline Vector4 operator * (const Vector4& lhs, const float& rhs)
{
    return Vector4Scale(lhs, rhs);
}

inline const Vector4& operator *= (Vector4& lhs, const float& rhs)
{
    lhs = Vector4Scale(lhs, rhs);
    return lhs;
}

inline Vector4 operator * (const Vector4& lhs, const Vector4& rhs)
{
    return Vector4Multiply(lhs, rhs);
}

inline const Vector4& operator *= (Vector4& lhs, const Vector4& rhs)
{
    lhs = Vector4Multiply(lhs, rhs);
    return lhs;
}

inline Vector4 operator / (const Vector4& lhs, const float& rhs)
{
    return Vector4Scale(lhs, 1.0f / rhs);
}

inline const Vector4& operator /= (Vector4& lhs, const float& rhs)
{
    lhs = Vector4Scale(lhs, 1.0f / rhs);
    return lhs;
}

inline Vector4 operator / (const Vector4& lhs, const Vector4& rhs)
{
    return Vector4Divide(lhs, rhs);
}

inline const Vector4& operator /= (Vector4& lhs, const Vector4& rhs)
{
    lhs = Vector4Divide(lhs, rhs);
    return lhs;
}

inline bool operator == (const Vector4& lhs, const Vector4& rhs)
{
    return FloatEquals(lhs.x, rhs.x) && FloatEquals(lhs.y, rhs.y) && FloatEquals(lhs.z, rhs.z) && FloatEquals(lhs.w, rhs.w);
}

inline bool operator != (const Vector4& lhs, const Vector4& rhs)
{
    return !FloatEquals(lhs.x, rhs.x) || !FloatEquals(lhs.y, rhs.y) || !FloatEquals(lhs.z, rhs.z) || !FloatEquals(lhs.w, rhs.w);
}

// Quaternion operators
static constexpr Quaternion QuaternionZeros = { 0, 0, 0, 0 };
static constexpr Quaternion QuaternionOnes = { 1, 1, 1, 1 };
static constexpr Quaternion QuaternionUnitX = { 0, 0, 0, 1 };

inline Quaternion operator + (const Quaternion& lhs, const float& rhs)
{
    return QuaternionAddValue(lhs, rhs);
}

inline const Quaternion& operator += (Quaternion& lhs, const float& rhs)
{
    lhs = QuaternionAddValue(lhs, rhs);
    return lhs;
}

inline Quaternion operator - (const Quaternion& lhs, const float& rhs)
{
    return QuaternionSubtractValue(lhs, rhs);
}

inline const Quaternion& operator -= (Quaternion& lhs, const float& rhs)
{
    lhs = QuaternionSubtractValue(lhs, rhs);
    return lhs;
}

inline Quaternion operator * (const Quaternion& lhs, const Matrix& rhs)
{
    return QuaternionTransform(lhs, rhs);
}

inline const Quaternion& operator *= (Quaternion& lhs, const Matrix& rhs)
{
    lhs = QuaternionTransform(lhs, rhs);
    return lhs;
}

// Matrix operators
inline Matrix operator + (const Matrix& lhs, const Matrix& rhs)
{
    return MatrixAdd(lhs, rhs);
}

inline const Matrix& operator += (Matrix& lhs, const Matrix& rhs)
{
    lhs = MatrixAdd(lhs, rhs);
    return lhs;
}

inline Matrix operator - (const Matrix& lhs, const Matrix& rhs)
{
    return MatrixSubtract(lhs, rhs);
}

inline const Matrix& operator -= (Matrix& lhs, const Matrix& rhs)
{
    lhs = MatrixSubtract(lhs, rhs);
    return lhs;
}

inline Matrix operator * (const Matrix& lhs, const Matrix& rhs)
{
    return MatrixMultiply(lhs, rhs);
}

inline const Matrix& operator *= (Matrix& lhs, const Matrix& rhs)
{
    lhs = MatrixMultiply(lhs, rhs);
    return lhs;
}
//-------------------------------------------------------------------------------
#endif  // C++ operators

#endif  // RAYMATH_H
/**********************************************************************************************
*
*   rlgl v5.0 - A multi-OpenGL abstraction layer with an immediate-mode style API
*
*   DESCRIPTION:
*       An abstraction layer for multiple OpenGL versions (1.1, 2.1, 3.3 Core, 4.3 Core, ES 2.0, ES 3.0)
*       that provides a pseudo-OpenGL 1.1 immediate-mode style API (rlVertex, rlTranslate, rlRotate...)
*
*   ADDITIONAL NOTES:
*       When choosing an OpenGL backend different than OpenGL 1.1, some internal buffer are
*       initialized on rlglInit() to accumulate vertex data
*
*       When an internal state change is required all the stored vertex data is renderer in batch,
*       additionally, rlDrawRenderBatchActive() could be called to force flushing of the batch
*
*       Some resources are also loaded for convenience, here the complete list:
*          - Default batch (RLGL.defaultBatch): RenderBatch system to accumulate vertex data
*          - Default texture (RLGL.defaultTextureId): 1x1 white pixel R8G8B8A8
*          - Default shader (RLGL.State.defaultShaderId, RLGL.State.defaultShaderLocs)
*
*       Internal buffer (and resources) must be manually unloaded calling rlglClose()
*
*   CONFIGURATION:
*       #define GRAPHICS_API_OPENGL_11
*       #define GRAPHICS_API_OPENGL_21
*       #define GRAPHICS_API_OPENGL_33
*       #define GRAPHICS_API_OPENGL_43
*       #define GRAPHICS_API_OPENGL_ES2
*       #define GRAPHICS_API_OPENGL_ES3
*           Use selected OpenGL graphics backend, should be supported by platform
*           Those preprocessor defines are only used on rlgl module, if OpenGL version is
*           required by any other module, use rlGetVersion() to check it
*
*       #define RLGL_IMPLEMENTATION
*           Generates the implementation of the library into the included file
*           If not defined, the library is in header only mode and can be included in other headers
*           or source files without problems. But only ONE file should hold the implementation
*
*       #define RLGL_RENDER_TEXTURES_HINT
*           Enable framebuffer objects (fbo) support (enabled by default)
*           Some GPUs could not support them despite the OpenGL version
*
*       #define RLGL_SHOW_GL_DETAILS_INFO
*           Show OpenGL extensions and capabilities detailed logs on init
*
*       #define RLGL_ENABLE_OPENGL_DEBUG_CONTEXT
*           Enable debug context (only available on OpenGL 4.3)
*
*       rlgl capabilities could be customized just defining some internal
*       values before library inclusion (default values listed):
*
*       #define RL_DEFAULT_BATCH_BUFFER_ELEMENTS   8192    // Default internal render batch elements limits
*       #define RL_DEFAULT_BATCH_BUFFERS              1    // Default number of batch buffers (multi-buffering)
*       #define RL_DEFAULT_BATCH_DRAWCALLS          256    // Default number of batch draw calls (by state changes: mode, texture)
*       #define RL_DEFAULT_BATCH_MAX_TEXTURE_UNITS    4    // Maximum number of textures units that can be activated on batch drawing (SetShaderValueTexture())
*
*       #define RL_MAX_MATRIX_STACK_SIZE             32    // Maximum size of internal Matrix stack
*       #define RL_MAX_SHADER_LOCATIONS              32    // Maximum number of shader locations supported
*       #define RL_CULL_DISTANCE_NEAR              0.01    // Default projection matrix near cull distance
*       #define RL_CULL_DISTANCE_FAR             1000.0    // Default projection matrix far cull distance
*
*       When loading a shader, the following vertex attributes and uniform
*       location names are tried to be set automatically:
*
*       #define RL_DEFAULT_SHADER_ATTRIB_NAME_POSITION     "vertexPosition"    // Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_LOCATION_POSITION
*       #define RL_DEFAULT_SHADER_ATTRIB_NAME_TEXCOORD     "vertexTexCoord"    // Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_LOCATION_TEXCOORD
*       #define RL_DEFAULT_SHADER_ATTRIB_NAME_NORMAL       "vertexNormal"      // Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_LOCATION_NORMAL
*       #define RL_DEFAULT_SHADER_ATTRIB_NAME_COLOR        "vertexColor"       // Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_LOCATION_COLOR
*       #define RL_DEFAULT_SHADER_ATTRIB_NAME_TANGENT      "vertexTangent"     // Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_LOCATION_TANGENT
*       #define RL_DEFAULT_SHADER_ATTRIB_NAME_TEXCOORD2    "vertexTexCoord2"   // Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_LOCATION_TEXCOORD2
*       #define RL_DEFAULT_SHADER_ATTRIB_NAME_BONEIDS      "vertexBoneIds"     // Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_LOCATION_BONEIDS
*       #define RL_DEFAULT_SHADER_ATTRIB_NAME_BONEWEIGHTS  "vertexBoneWeights" // Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_LOCATION_BONEWEIGHTS
*       #define RL_DEFAULT_SHADER_UNIFORM_NAME_MVP         "mvp"               // model-view-projection matrix
*       #define RL_DEFAULT_SHADER_UNIFORM_NAME_VIEW        "matView"           // view matrix
*       #define RL_DEFAULT_SHADER_UNIFORM_NAME_PROJECTION  "matProjection"     // projection matrix
*       #define RL_DEFAULT_SHADER_UNIFORM_NAME_MODEL       "matModel"          // model matrix
*       #define RL_DEFAULT_SHADER_UNIFORM_NAME_NORMAL      "matNormal"         // normal matrix (transpose(inverse(matModelView)))
*       #define RL_DEFAULT_SHADER_UNIFORM_NAME_COLOR       "colDiffuse"        // color diffuse (base tint color, multiplied by texture color)
*       #define RL_DEFAULT_SHADER_UNIFORM_NAME_BONE_MATRICES  "boneMatrices"   // bone matrices
*       #define RL_DEFAULT_SHADER_SAMPLER2D_NAME_TEXTURE0  "texture0"          // texture0 (texture slot active 0)
*       #define RL_DEFAULT_SHADER_SAMPLER2D_NAME_TEXTURE1  "texture1"          // texture1 (texture slot active 1)
*       #define RL_DEFAULT_SHADER_SAMPLER2D_NAME_TEXTURE2  "texture2"          // texture2 (texture slot active 2)
*
*   DEPENDENCIES:
*      - OpenGL libraries (depending on platform and OpenGL version selected)
*      - GLAD OpenGL extensions loading library (only for OpenGL 3.3 Core, 4.3 Core)
*
*
*   LICENSE: zlib/libpng
*
*   Copyright (c) 2014-2024 Ramon Santamaria (@raysan5)
*
*   This software is provided "as-is", without any express or implied warranty. In no event
*   will the authors be held liable for any damages arising from the use of this software.
*
*   Permission is granted to anyone to use this software for any purpose, including commercial
*   applications, and to alter it and redistribute it freely, subject to the following restrictions:
*
*     1. The origin of this software must not be misrepresented; you must not claim that you
*     wrote the original software. If you use this software in a product, an acknowledgment
*     in the product documentation would be appreciated but is not required.
*
*     2. Altered source versions must be plainly marked as such, and must not be misrepresented
*     as being the original software.
*
*     3. This notice may not be removed or altered from any source distribution.
*
**********************************************************************************************/

#ifndef RLGL_H
#define RLGL_H

#define RLGL_VERSION  "5.0"

// Function specifiers in case library is build/used as a shared library
// NOTE: Microsoft specifiers to tell compiler that symbols are imported/exported from a .dll
// NOTE: visibility(default) attribute makes symbols "visible" when compiled with -fvisibility=hidden
#if defined(_WIN32) && defined(BUILD_LIBTYPE_SHARED)
    #define RLAPI __declspec(dllexport)     // We are building the library as a Win32 shared library (.dll)
#elif defined(BUILD_LIBTYPE_SHARED)
    #define RLAPI __attribute__((visibility("default"))) // We are building the library as a Unix shared library (.so/.dylib)
#elif defined(_WIN32) && defined(USE_LIBTYPE_SHARED)
    #define RLAPI __declspec(dllimport)     // We are using the library as a Win32 shared library (.dll)
#endif

// Function specifiers definition
#ifndef RLAPI
    #define RLAPI       // Functions defined as 'extern' by default (implicit specifiers)
#endif

// Support TRACELOG macros
#ifndef TRACELOG
    #define TRACELOG(level, ...) (void)0
    #define TRACELOGD(...) (void)0
#endif

// Allow custom memory allocators
#ifndef RL_MALLOC
    #define RL_MALLOC(sz)     malloc(sz)
#endif
#ifndef RL_CALLOC
    #define RL_CALLOC(n,sz)   calloc(n,sz)
#endif
#ifndef RL_REALLOC
    #define RL_REALLOC(n,sz)  realloc(n,sz)
#endif
#ifndef RL_FREE
    #define RL_FREE(p)        free(p)
#endif

// Security check in case no GRAPHICS_API_OPENGL_* defined
#if !defined(GRAPHICS_API_OPENGL_11) && \
    !defined(GRAPHICS_API_OPENGL_21) && \
    !defined(GRAPHICS_API_OPENGL_33) && \
    !defined(GRAPHICS_API_OPENGL_43) && \
    !defined(GRAPHICS_API_OPENGL_ES2) && \
    !defined(GRAPHICS_API_OPENGL_ES3)
        #define GRAPHICS_API_OPENGL_33
#endif

// Security check in case multiple GRAPHICS_API_OPENGL_* defined
#if defined(GRAPHICS_API_OPENGL_11)
    #if defined(GRAPHICS_API_OPENGL_21)
        #undef GRAPHICS_API_OPENGL_21
    #endif
    #if defined(GRAPHICS_API_OPENGL_33)
        #undef GRAPHICS_API_OPENGL_33
    #endif
    #if defined(GRAPHICS_API_OPENGL_43)
        #undef GRAPHICS_API_OPENGL_43
    #endif
    #if defined(GRAPHICS_API_OPENGL_ES2)
        #undef GRAPHICS_API_OPENGL_ES2
    #endif
#endif

// OpenGL 2.1 uses most of OpenGL 3.3 Core functionality
// WARNING: Specific parts are checked with #if defines
#if defined(GRAPHICS_API_OPENGL_21)
    #define GRAPHICS_API_OPENGL_33
#endif

// OpenGL 4.3 uses OpenGL 3.3 Core functionality
#if defined(GRAPHICS_API_OPENGL_43)
    #define GRAPHICS_API_OPENGL_33
#endif

// OpenGL ES 3.0 uses OpenGL ES 2.0 functionality (and more)
#if defined(GRAPHICS_API_OPENGL_ES3)
    #define GRAPHICS_API_OPENGL_ES2
#endif

// Support framebuffer objects by default
// NOTE: Some driver implementation do not support it, despite they should
#define RLGL_RENDER_TEXTURES_HINT

//----------------------------------------------------------------------------------
// Defines and Macros
//----------------------------------------------------------------------------------

// Default internal render batch elements limits
#ifndef RL_DEFAULT_BATCH_BUFFER_ELEMENTS
    #if defined(GRAPHICS_API_OPENGL_11) || defined(GRAPHICS_API_OPENGL_33)
        // This is the maximum amount of elements (quads) per batch
        // NOTE: Be careful with text, every letter maps to a quad
        #define RL_DEFAULT_BATCH_BUFFER_ELEMENTS  8192
    #endif
    #if defined(GRAPHICS_API_OPENGL_ES2)
        // We reduce memory sizes for embedded systems (RPI and HTML5)
        // NOTE: On HTML5 (emscripten) this is allocated on heap,
        // by default it's only 16MB!...just take care...
        #define RL_DEFAULT_BATCH_BUFFER_ELEMENTS  2048
    #endif
#endif
#ifndef RL_DEFAULT_BATCH_BUFFERS
    #define RL_DEFAULT_BATCH_BUFFERS                 1      // Default number of batch buffers (multi-buffering)
#endif
#ifndef RL_DEFAULT_BATCH_DRAWCALLS
    #define RL_DEFAULT_BATCH_DRAWCALLS             256      // Default number of batch draw calls (by state changes: mode, texture)
#endif
#ifndef RL_DEFAULT_BATCH_MAX_TEXTURE_UNITS
    #define RL_DEFAULT_BATCH_MAX_TEXTURE_UNITS       4      // Maximum number of textures units that can be activated on batch drawing (SetShaderValueTexture())
#endif

// Internal Matrix stack
#ifndef RL_MAX_MATRIX_STACK_SIZE
    #define RL_MAX_MATRIX_STACK_SIZE                32      // Maximum size of Matrix stack
#endif

// Shader limits
#ifndef RL_MAX_SHADER_LOCATIONS
    #define RL_MAX_SHADER_LOCATIONS                 32      // Maximum number of shader locations supported
#endif

// Projection matrix culling
#ifndef RL_CULL_DISTANCE_NEAR
    #define RL_CULL_DISTANCE_NEAR                 0.01      // Default near cull distance
#endif
#ifndef RL_CULL_DISTANCE_FAR
    #define RL_CULL_DISTANCE_FAR                1000.0      // Default far cull distance
#endif

// Texture parameters (equivalent to OpenGL defines)
#define RL_TEXTURE_WRAP_S                       0x2802      // GL_TEXTURE_WRAP_S
#define RL_TEXTURE_WRAP_T                       0x2803      // GL_TEXTURE_WRAP_T
#define RL_TEXTURE_MAG_FILTER                   0x2800      // GL_TEXTURE_MAG_FILTER
#define RL_TEXTURE_MIN_FILTER                   0x2801      // GL_TEXTURE_MIN_FILTER

#define RL_TEXTURE_FILTER_NEAREST               0x2600      // GL_NEAREST
#define RL_TEXTURE_FILTER_LINEAR                0x2601      // GL_LINEAR
#define RL_TEXTURE_FILTER_MIP_NEAREST           0x2700      // GL_NEAREST_MIPMAP_NEAREST
#define RL_TEXTURE_FILTER_NEAREST_MIP_LINEAR    0x2702      // GL_NEAREST_MIPMAP_LINEAR
#define RL_TEXTURE_FILTER_LINEAR_MIP_NEAREST    0x2701      // GL_LINEAR_MIPMAP_NEAREST
#define RL_TEXTURE_FILTER_MIP_LINEAR            0x2703      // GL_LINEAR_MIPMAP_LINEAR
#define RL_TEXTURE_FILTER_ANISOTROPIC           0x3000      // Anisotropic filter (custom identifier)
#define RL_TEXTURE_MIPMAP_BIAS_RATIO            0x4000      // Texture mipmap bias, percentage ratio (custom identifier)

#define RL_TEXTURE_WRAP_REPEAT                  0x2901      // GL_REPEAT
#define RL_TEXTURE_WRAP_CLAMP                   0x812F      // GL_CLAMP_TO_EDGE
#define RL_TEXTURE_WRAP_MIRROR_REPEAT           0x8370      // GL_MIRRORED_REPEAT
#define RL_TEXTURE_WRAP_MIRROR_CLAMP            0x8742      // GL_MIRROR_CLAMP_EXT

// Matrix modes (equivalent to OpenGL)
#define RL_MODELVIEW                            0x1700      // GL_MODELVIEW
#define RL_PROJECTION                           0x1701      // GL_PROJECTION
#define RL_TEXTURE                              0x1702      // GL_TEXTURE

// Primitive assembly draw modes
#define RL_LINES                                0x0001      // GL_LINES
#define RL_TRIANGLES                            0x0004      // GL_TRIANGLES
#define RL_QUADS                                0x0007      // GL_QUADS

// GL equivalent data types
#define RL_UNSIGNED_BYTE                        0x1401      // GL_UNSIGNED_BYTE
#define RL_FLOAT                                0x1406      // GL_FLOAT

// GL buffer usage hint
#define RL_STREAM_DRAW                          0x88E0      // GL_STREAM_DRAW
#define RL_STREAM_READ                          0x88E1      // GL_STREAM_READ
#define RL_STREAM_COPY                          0x88E2      // GL_STREAM_COPY
#define RL_STATIC_DRAW                          0x88E4      // GL_STATIC_DRAW
#define RL_STATIC_READ                          0x88E5      // GL_STATIC_READ
#define RL_STATIC_COPY                          0x88E6      // GL_STATIC_COPY
#define RL_DYNAMIC_DRAW                         0x88E8      // GL_DYNAMIC_DRAW
#define RL_DYNAMIC_READ                         0x88E9      // GL_DYNAMIC_READ
#define RL_DYNAMIC_COPY                         0x88EA      // GL_DYNAMIC_COPY

// GL Shader type
#define RL_FRAGMENT_SHADER                      0x8B30      // GL_FRAGMENT_SHADER
#define RL_VERTEX_SHADER                        0x8B31      // GL_VERTEX_SHADER
#define RL_COMPUTE_SHADER                       0x91B9      // GL_COMPUTE_SHADER

// GL blending factors
#define RL_ZERO                                 0           // GL_ZERO
#define RL_ONE                                  1           // GL_ONE
#define RL_SRC_COLOR                            0x0300      // GL_SRC_COLOR
#define RL_ONE_MINUS_SRC_COLOR                  0x0301      // GL_ONE_MINUS_SRC_COLOR
#define RL_SRC_ALPHA                            0x0302      // GL_SRC_ALPHA
#define RL_ONE_MINUS_SRC_ALPHA                  0x0303      // GL_ONE_MINUS_SRC_ALPHA
#define RL_DST_ALPHA                            0x0304      // GL_DST_ALPHA
#define RL_ONE_MINUS_DST_ALPHA                  0x0305      // GL_ONE_MINUS_DST_ALPHA
#define RL_DST_COLOR                            0x0306      // GL_DST_COLOR
#define RL_ONE_MINUS_DST_COLOR                  0x0307      // GL_ONE_MINUS_DST_COLOR
#define RL_SRC_ALPHA_SATURATE                   0x0308      // GL_SRC_ALPHA_SATURATE
#define RL_CONSTANT_COLOR                       0x8001      // GL_CONSTANT_COLOR
#define RL_ONE_MINUS_CONSTANT_COLOR             0x8002      // GL_ONE_MINUS_CONSTANT_COLOR
#define RL_CONSTANT_ALPHA                       0x8003      // GL_CONSTANT_ALPHA
#define RL_ONE_MINUS_CONSTANT_ALPHA             0x8004      // GL_ONE_MINUS_CONSTANT_ALPHA

// GL blending functions/equations
#define RL_FUNC_ADD                             0x8006      // GL_FUNC_ADD
#define RL_MIN                                  0x8007      // GL_MIN
#define RL_MAX                                  0x8008      // GL_MAX
#define RL_FUNC_SUBTRACT                        0x800A      // GL_FUNC_SUBTRACT
#define RL_FUNC_REVERSE_SUBTRACT                0x800B      // GL_FUNC_REVERSE_SUBTRACT
#define RL_BLEND_EQUATION                       0x8009      // GL_BLEND_EQUATION
#define RL_BLEND_EQUATION_RGB                   0x8009      // GL_BLEND_EQUATION_RGB   // (Same as BLEND_EQUATION)
#define RL_BLEND_EQUATION_ALPHA                 0x883D      // GL_BLEND_EQUATION_ALPHA
#define RL_BLEND_DST_RGB                        0x80C8      // GL_BLEND_DST_RGB
#define RL_BLEND_SRC_RGB                        0x80C9      // GL_BLEND_SRC_RGB
#define RL_BLEND_DST_ALPHA                      0x80CA      // GL_BLEND_DST_ALPHA
#define RL_BLEND_SRC_ALPHA                      0x80CB      // GL_BLEND_SRC_ALPHA
#define RL_BLEND_COLOR                          0x8005      // GL_BLEND_COLOR

#define RL_READ_FRAMEBUFFER                     0x8CA8      // GL_READ_FRAMEBUFFER
#define RL_DRAW_FRAMEBUFFER                     0x8CA9      // GL_DRAW_FRAMEBUFFER

// Default shader vertex attribute locations
#ifndef RL_DEFAULT_SHADER_ATTRIB_LOCATION_POSITION
    #define RL_DEFAULT_SHADER_ATTRIB_LOCATION_POSITION    0
#endif
#ifndef RL_DEFAULT_SHADER_ATTRIB_LOCATION_TEXCOORD
    #define RL_DEFAULT_SHADER_ATTRIB_LOCATION_TEXCOORD    1
#endif
#ifndef RL_DEFAULT_SHADER_ATTRIB_LOCATION_NORMAL
    #define RL_DEFAULT_SHADER_ATTRIB_LOCATION_NORMAL      2
#endif
#ifndef RL_DEFAULT_SHADER_ATTRIB_LOCATION_COLOR
    #define RL_DEFAULT_SHADER_ATTRIB_LOCATION_COLOR       3
#endif
    #ifndef RL_DEFAULT_SHADER_ATTRIB_LOCATION_TANGENT
#define RL_DEFAULT_SHADER_ATTRIB_LOCATION_TANGENT         4
#endif
#ifndef RL_DEFAULT_SHADER_ATTRIB_LOCATION_TEXCOORD2
    #define RL_DEFAULT_SHADER_ATTRIB_LOCATION_TEXCOORD2   5
#endif
#ifndef RL_DEFAULT_SHADER_ATTRIB_LOCATION_INDICES
    #define RL_DEFAULT_SHADER_ATTRIB_LOCATION_INDICES     6
#endif
#ifdef RL_SUPPORT_MESH_GPU_SKINNING
#ifndef RL_DEFAULT_SHADER_ATTRIB_LOCATION_BONEIDS
    #define RL_DEFAULT_SHADER_ATTRIB_LOCATION_BONEIDS     7
#endif
#ifndef RL_DEFAULT_SHADER_ATTRIB_LOCATION_BONEWEIGHTS
    #define RL_DEFAULT_SHADER_ATTRIB_LOCATION_BONEWEIGHTS 8
#endif
#endif

//----------------------------------------------------------------------------------
// Types and Structures Definition
//----------------------------------------------------------------------------------
#if (defined(__STDC__) && __STDC_VERSION__ >= 199901L) || (defined(_MSC_VER) && _MSC_VER >= 1800)
    #include <stdbool.h>
#elif !defined(__cplusplus) && !defined(bool) && !defined(RL_BOOL_TYPE)
    // Boolean type
typedef enum bool { false = 0, true = !false } bool;
#endif

#if !defined(RL_MATRIX_TYPE)
// Matrix, 4x4 components, column major, OpenGL style, right handed
typedef struct Matrix {
    float m0, m4, m8, m12;      // Matrix first row (4 components)
    float m1, m5, m9, m13;      // Matrix second row (4 components)
    float m2, m6, m10, m14;     // Matrix third row (4 components)
    float m3, m7, m11, m15;     // Matrix fourth row (4 components)
} Matrix;
#define RL_MATRIX_TYPE
#endif

// Dynamic vertex buffers (position + texcoords + colors + indices arrays)
typedef struct rlVertexBuffer {
    int elementCount;           // Number of elements in the buffer (QUADS)

    float *vertices;            // Vertex position (XYZ - 3 components per vertex) (shader-location = 0)
    float *texcoords;           // Vertex texture coordinates (UV - 2 components per vertex) (shader-location = 1)
    float *normals;             // Vertex normal (XYZ - 3 components per vertex) (shader-location = 2)
    unsigned char *colors;      // Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)
#if defined(GRAPHICS_API_OPENGL_11) || defined(GRAPHICS_API_OPENGL_33)
    unsigned int *indices;      // Vertex indices (in case vertex data comes indexed) (6 indices per quad)
#endif
#if defined(GRAPHICS_API_OPENGL_ES2)
    unsigned short *indices;    // Vertex indices (in case vertex data comes indexed) (6 indices per quad)
#endif
    unsigned int vaoId;         // OpenGL Vertex Array Object id
    unsigned int vboId[5];      // OpenGL Vertex Buffer Objects id (5 types of vertex data)
} rlVertexBuffer;

// Draw call type
// NOTE: Only texture changes register a new draw, other state-change-related elements are not
// used at this moment (vaoId, shaderId, matrices), raylib just forces a batch draw call if any
// of those state-change happens (this is done in core module)
typedef struct rlDrawCall {
    int mode;                   // Drawing mode: LINES, TRIANGLES, QUADS
    int vertexCount;            // Number of vertex of the draw
    int vertexAlignment;        // Number of vertex required for index alignment (LINES, TRIANGLES)
    //unsigned int vaoId;       // Vertex array id to be used on the draw -> Using RLGL.currentBatch->vertexBuffer.vaoId
    //unsigned int shaderId;    // Shader id to be used on the draw -> Using RLGL.currentShaderId
    unsigned int textureId;     // Texture id to be used on the draw -> Use to create new draw call if changes

    //Matrix projection;        // Projection matrix for this draw -> Using RLGL.projection by default
    //Matrix modelview;         // Modelview matrix for this draw -> Using RLGL.modelview by default
} rlDrawCall;

// rlRenderBatch type
typedef struct rlRenderBatch {
    int bufferCount;            // Number of vertex buffers (multi-buffering support)
    int currentBuffer;          // Current buffer tracking in case of multi-buffering
    rlVertexBuffer *vertexBuffer; // Dynamic buffer(s) for vertex data

    rlDrawCall *draws;          // Draw calls array, depends on textureId
    int drawCounter;            // Draw calls counter
    float currentDepth;         // Current depth value for next draw
} rlRenderBatch;

// OpenGL version
typedef enum {
    RL_OPENGL_11 = 1,           // OpenGL 1.1
    RL_OPENGL_21,               // OpenGL 2.1 (GLSL 120)
    RL_OPENGL_33,               // OpenGL 3.3 (GLSL 330)
    RL_OPENGL_43,               // OpenGL 4.3 (using GLSL 330)
    RL_OPENGL_ES_20,            // OpenGL ES 2.0 (GLSL 100)
    RL_OPENGL_ES_30             // OpenGL ES 3.0 (GLSL 300 es)
} rlGlVersion;

// Trace log level
// NOTE: Organized by priority level
typedef enum {
    RL_LOG_ALL = 0,             // Display all logs
    RL_LOG_TRACE,               // Trace logging, intended for internal use only
    RL_LOG_DEBUG,               // Debug logging, used for internal debugging, it should be disabled on release builds
    RL_LOG_INFO,                // Info logging, used for program execution info
    RL_LOG_WARNING,             // Warning logging, used on recoverable failures
    RL_LOG_ERROR,               // Error logging, used on unrecoverable failures
    RL_LOG_FATAL,               // Fatal logging, used to abort program: exit(EXIT_FAILURE)
    RL_LOG_NONE                 // Disable logging
} rlTraceLogLevel;

// Texture pixel formats
// NOTE: Support depends on OpenGL version
typedef enum {
    RL_PIXELFORMAT_UNCOMPRESSED_GRAYSCALE = 1,     // 8 bit per pixel (no alpha)
    RL_PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA,        // 8*2 bpp (2 channels)
    RL_PIXELFORMAT_UNCOMPRESSED_R5G6B5,            // 16 bpp
    RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8,            // 24 bpp
    RL_PIXELFORMAT_UNCOMPRESSED_R5G5B5A1,          // 16 bpp (1 bit alpha)
    RL_PIXELFORMAT_UNCOMPRESSED_R4G4B4A4,          // 16 bpp (4 bit alpha)
    RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8A8,          // 32 bpp
    RL_PIXELFORMAT_UNCOMPRESSED_R32,               // 32 bpp (1 channel - float)
    RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32,         // 32*3 bpp (3 channels - float)
    RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32A32,      // 32*4 bpp (4 channels - float)
    RL_PIXELFORMAT_UNCOMPRESSED_R16,               // 16 bpp (1 channel - half float)
    RL_PIXELFORMAT_UNCOMPRESSED_R16G16B16,         // 16*3 bpp (3 channels - half float)
    RL_PIXELFORMAT_UNCOMPRESSED_R16G16B16A16,      // 16*4 bpp (4 channels - half float)
    RL_PIXELFORMAT_COMPRESSED_DXT1_RGB,            // 4 bpp (no alpha)
    RL_PIXELFORMAT_COMPRESSED_DXT1_RGBA,           // 4 bpp (1 bit alpha)
    RL_PIXELFORMAT_COMPRESSED_DXT3_RGBA,           // 8 bpp
    RL_PIXELFORMAT_COMPRESSED_DXT5_RGBA,           // 8 bpp
    RL_PIXELFORMAT_COMPRESSED_ETC1_RGB,            // 4 bpp
    RL_PIXELFORMAT_COMPRESSED_ETC2_RGB,            // 4 bpp
    RL_PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA,       // 8 bpp
    RL_PIXELFORMAT_COMPRESSED_PVRT_RGB,            // 4 bpp
    RL_PIXELFORMAT_COMPRESSED_PVRT_RGBA,           // 4 bpp
    RL_PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA,       // 8 bpp
    RL_PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA        // 2 bpp
} rlPixelFormat;

// Texture parameters: filter mode
// NOTE 1: Filtering considers mipmaps if available in the texture
// NOTE 2: Filter is accordingly set for minification and magnification
typedef enum {
    RL_TEXTURE_FILTER_POINT = 0,        // No filter, just pixel approximation
    RL_TEXTURE_FILTER_BILINEAR,         // Linear filtering
    RL_TEXTURE_FILTER_TRILINEAR,        // Trilinear filtering (linear with mipmaps)
    RL_TEXTURE_FILTER_ANISOTROPIC_4X,   // Anisotropic filtering 4x
    RL_TEXTURE_FILTER_ANISOTROPIC_8X,   // Anisotropic filtering 8x
    RL_TEXTURE_FILTER_ANISOTROPIC_16X,  // Anisotropic filtering 16x
} rlTextureFilter;

// Color blending modes (pre-defined)
typedef enum {
    RL_BLEND_ALPHA = 0,                 // Blend textures considering alpha (default)
    RL_BLEND_ADDITIVE,                  // Blend textures adding colors
    RL_BLEND_MULTIPLIED,                // Blend textures multiplying colors
    RL_BLEND_ADD_COLORS,                // Blend textures adding colors (alternative)
    RL_BLEND_SUBTRACT_COLORS,           // Blend textures subtracting colors (alternative)
    RL_BLEND_ALPHA_PREMULTIPLY,         // Blend premultiplied textures considering alpha
    RL_BLEND_CUSTOM,                    // Blend textures using custom src/dst factors (use rlSetBlendFactors())
    RL_BLEND_CUSTOM_SEPARATE            // Blend textures using custom src/dst factors (use rlSetBlendFactorsSeparate())
} rlBlendMode;

// Shader location point type
typedef enum {
    RL_SHADER_LOC_VERTEX_POSITION = 0,  // Shader location: vertex attribute: position
    RL_SHADER_LOC_VERTEX_TEXCOORD01,    // Shader location: vertex attribute: texcoord01
    RL_SHADER_LOC_VERTEX_TEXCOORD02,    // Shader location: vertex attribute: texcoord02
    RL_SHADER_LOC_VERTEX_NORMAL,        // Shader location: vertex attribute: normal
    RL_SHADER_LOC_VERTEX_TANGENT,       // Shader location: vertex attribute: tangent
    RL_SHADER_LOC_VERTEX_COLOR,         // Shader location: vertex attribute: color
    RL_SHADER_LOC_MATRIX_MVP,           // Shader location: matrix uniform: model-view-projection
    RL_SHADER_LOC_MATRIX_VIEW,          // Shader location: matrix uniform: view (camera transform)
    RL_SHADER_LOC_MATRIX_PROJECTION,    // Shader location: matrix uniform: projection
    RL_SHADER_LOC_MATRIX_MODEL,         // Shader location: matrix uniform: model (transform)
    RL_SHADER_LOC_MATRIX_NORMAL,        // Shader location: matrix uniform: normal
    RL_SHADER_LOC_VECTOR_VIEW,          // Shader location: vector uniform: view
    RL_SHADER_LOC_COLOR_DIFFUSE,        // Shader location: vector uniform: diffuse color
    RL_SHADER_LOC_COLOR_SPECULAR,       // Shader location: vector uniform: specular color
    RL_SHADER_LOC_COLOR_AMBIENT,        // Shader location: vector uniform: ambient color
    RL_SHADER_LOC_MAP_ALBEDO,           // Shader location: sampler2d texture: albedo (same as: RL_SHADER_LOC_MAP_DIFFUSE)
    RL_SHADER_LOC_MAP_METALNESS,        // Shader location: sampler2d texture: metalness (same as: RL_SHADER_LOC_MAP_SPECULAR)
    RL_SHADER_LOC_MAP_NORMAL,           // Shader location: sampler2d texture: normal
    RL_SHADER_LOC_MAP_ROUGHNESS,        // Shader location: sampler2d texture: roughness
    RL_SHADER_LOC_MAP_OCCLUSION,        // Shader location: sampler2d texture: occlusion
    RL_SHADER_LOC_MAP_EMISSION,         // Shader location: sampler2d texture: emission
    RL_SHADER_LOC_MAP_HEIGHT,           // Shader location: sampler2d texture: height
    RL_SHADER_LOC_MAP_CUBEMAP,          // Shader location: samplerCube texture: cubemap
    RL_SHADER_LOC_MAP_IRRADIANCE,       // Shader location: samplerCube texture: irradiance
    RL_SHADER_LOC_MAP_PREFILTER,        // Shader location: samplerCube texture: prefilter
    RL_SHADER_LOC_MAP_BRDF              // Shader location: sampler2d texture: brdf
} rlShaderLocationIndex;

#define RL_SHADER_LOC_MAP_DIFFUSE       RL_SHADER_LOC_MAP_ALBEDO
#define RL_SHADER_LOC_MAP_SPECULAR      RL_SHADER_LOC_MAP_METALNESS

// Shader uniform data type
typedef enum {
    RL_SHADER_UNIFORM_FLOAT = 0,        // Shader uniform type: float
    RL_SHADER_UNIFORM_VEC2,             // Shader uniform type: vec2 (2 float)
    RL_SHADER_UNIFORM_VEC3,             // Shader uniform type: vec3 (3 float)
    RL_SHADER_UNIFORM_VEC4,             // Shader uniform type: vec4 (4 float)
    RL_SHADER_UNIFORM_INT,              // Shader uniform type: int
    RL_SHADER_UNIFORM_IVEC2,            // Shader uniform type: ivec2 (2 int)
    RL_SHADER_UNIFORM_IVEC3,            // Shader uniform type: ivec3 (3 int)
    RL_SHADER_UNIFORM_IVEC4,            // Shader uniform type: ivec4 (4 int)
    RL_SHADER_UNIFORM_UINT,             // Shader uniform type: unsigned int
    RL_SHADER_UNIFORM_UIVEC2,           // Shader uniform type: uivec2 (2 unsigned int)
    RL_SHADER_UNIFORM_UIVEC3,           // Shader uniform type: uivec3 (3 unsigned int)
    RL_SHADER_UNIFORM_UIVEC4,           // Shader uniform type: uivec4 (4 unsigned int)
    RL_SHADER_UNIFORM_SAMPLER2D         // Shader uniform type: sampler2d
} rlShaderUniformDataType;

// Shader attribute data types
typedef enum {
    RL_SHADER_ATTRIB_FLOAT = 0,         // Shader attribute type: float
    RL_SHADER_ATTRIB_VEC2,              // Shader attribute type: vec2 (2 float)
    RL_SHADER_ATTRIB_VEC3,              // Shader attribute type: vec3 (3 float)
    RL_SHADER_ATTRIB_VEC4               // Shader attribute type: vec4 (4 float)
} rlShaderAttributeDataType;

// Framebuffer attachment type
// NOTE: By default up to 8 color channels defined, but it can be more
typedef enum {
    RL_ATTACHMENT_COLOR_CHANNEL0 = 0,       // Framebuffer attachment type: color 0
    RL_ATTACHMENT_COLOR_CHANNEL1 = 1,       // Framebuffer attachment type: color 1
    RL_ATTACHMENT_COLOR_CHANNEL2 = 2,       // Framebuffer attachment type: color 2
    RL_ATTACHMENT_COLOR_CHANNEL3 = 3,       // Framebuffer attachment type: color 3
    RL_ATTACHMENT_COLOR_CHANNEL4 = 4,       // Framebuffer attachment type: color 4
    RL_ATTACHMENT_COLOR_CHANNEL5 = 5,       // Framebuffer attachment type: color 5
    RL_ATTACHMENT_COLOR_CHANNEL6 = 6,       // Framebuffer attachment type: color 6
    RL_ATTACHMENT_COLOR_CHANNEL7 = 7,       // Framebuffer attachment type: color 7
    RL_ATTACHMENT_DEPTH = 100,              // Framebuffer attachment type: depth
    RL_ATTACHMENT_STENCIL = 200,            // Framebuffer attachment type: stencil
} rlFramebufferAttachType;

// Framebuffer texture attachment type
typedef enum {
    RL_ATTACHMENT_CUBEMAP_POSITIVE_X = 0,   // Framebuffer texture attachment type: cubemap, +X side
    RL_ATTACHMENT_CUBEMAP_NEGATIVE_X = 1,   // Framebuffer texture attachment type: cubemap, -X side
    RL_ATTACHMENT_CUBEMAP_POSITIVE_Y = 2,   // Framebuffer texture attachment type: cubemap, +Y side
    RL_ATTACHMENT_CUBEMAP_NEGATIVE_Y = 3,   // Framebuffer texture attachment type: cubemap, -Y side
    RL_ATTACHMENT_CUBEMAP_POSITIVE_Z = 4,   // Framebuffer texture attachment type: cubemap, +Z side
    RL_ATTACHMENT_CUBEMAP_NEGATIVE_Z = 5,   // Framebuffer texture attachment type: cubemap, -Z side
    RL_ATTACHMENT_TEXTURE2D = 100,          // Framebuffer texture attachment type: texture2d
    RL_ATTACHMENT_RENDERBUFFER = 200,       // Framebuffer texture attachment type: renderbuffer
} rlFramebufferAttachTextureType;

// Face culling mode
typedef enum {
    RL_CULL_FACE_FRONT = 0,
    RL_CULL_FACE_BACK
} rlCullMode;

//------------------------------------------------------------------------------------
// Functions Declaration - Matrix operations
//------------------------------------------------------------------------------------

#if defined(__cplusplus)
extern "C" {            // Prevents name mangling of functions
#endif

RLAPI void rlMatrixMode(int mode);                      // Choose the current matrix to be transformed
RLAPI void rlPushMatrix(void);                          // Push the current matrix to stack
RLAPI void rlPopMatrix(void);                           // Pop latest inserted matrix from stack
RLAPI void rlLoadIdentity(void);                        // Reset current matrix to identity matrix
RLAPI void rlTranslatef(float x, float y, float z);     // Multiply the current matrix by a translation matrix
RLAPI void rlRotatef(float angle, float x, float y, float z); // Multiply the current matrix by a rotation matrix
RLAPI void rlScalef(float x, float y, float z);         // Multiply the current matrix by a scaling matrix
RLAPI void rlMultMatrixf(const float *matf);            // Multiply the current matrix by another matrix
RLAPI void rlFrustum(double left, double right, double bottom, double top, double znear, double zfar);
RLAPI void rlOrtho(double left, double right, double bottom, double top, double znear, double zfar);
RLAPI void rlViewport(int x, int y, int width, int height); // Set the viewport area
RLAPI void rlSetClipPlanes(double nearPlane, double farPlane);    // Set clip planes distances
RLAPI double rlGetCullDistanceNear(void);               // Get cull plane distance near
RLAPI double rlGetCullDistanceFar(void);                // Get cull plane distance far

//------------------------------------------------------------------------------------
// Functions Declaration - Vertex level operations
//------------------------------------------------------------------------------------
RLAPI void rlBegin(int mode);                           // Initialize drawing mode (how to organize vertex)
RLAPI void rlEnd(void);                                 // Finish vertex providing
RLAPI void rlVertex2i(int x, int y);                    // Define one vertex (position) - 2 int
RLAPI void rlVertex2f(float x, float y);                // Define one vertex (position) - 2 float
RLAPI void rlVertex3f(float x, float y, float z);       // Define one vertex (position) - 3 float
RLAPI void rlTexCoord2f(float x, float y);              // Define one vertex (texture coordinate) - 2 float
RLAPI void rlNormal3f(float x, float y, float z);       // Define one vertex (normal) - 3 float
RLAPI void rlColor4ub(unsigned char r, unsigned char g, unsigned char b, unsigned char a); // Define one vertex (color) - 4 byte
RLAPI void rlColor3f(float x, float y, float z);        // Define one vertex (color) - 3 float
RLAPI void rlColor4f(float x, float y, float z, float w); // Define one vertex (color) - 4 float

//------------------------------------------------------------------------------------
// Functions Declaration - OpenGL style functions (common to 1.1, 3.3+, ES2)
// NOTE: This functions are used to completely abstract raylib code from OpenGL layer,
// some of them are direct wrappers over OpenGL calls, some others are custom
//------------------------------------------------------------------------------------

// Vertex buffers state
RLAPI bool rlEnableVertexArray(unsigned int vaoId);     // Enable vertex array (VAO, if supported)
RLAPI void rlDisableVertexArray(void);                  // Disable vertex array (VAO, if supported)
RLAPI void rlEnableVertexBuffer(unsigned int id);       // Enable vertex buffer (VBO)
RLAPI void rlDisableVertexBuffer(void);                 // Disable vertex buffer (VBO)
RLAPI void rlEnableVertexBufferElement(unsigned int id); // Enable vertex buffer element (VBO element)
RLAPI void rlDisableVertexBufferElement(void);          // Disable vertex buffer element (VBO element)
RLAPI void rlEnableVertexAttribute(unsigned int index); // Enable vertex attribute index
RLAPI void rlDisableVertexAttribute(unsigned int index); // Disable vertex attribute index
#if defined(GRAPHICS_API_OPENGL_11)
RLAPI void rlEnableStatePointer(int vertexAttribType, void *buffer); // Enable attribute state pointer
RLAPI void rlDisableStatePointer(int vertexAttribType); // Disable attribute state pointer
#endif

// Textures state
RLAPI void rlActiveTextureSlot(int slot);               // Select and active a texture slot
RLAPI void rlEnableTexture(unsigned int id);            // Enable texture
RLAPI void rlDisableTexture(void);                      // Disable texture
RLAPI void rlEnableTextureCubemap(unsigned int id);     // Enable texture cubemap
RLAPI void rlDisableTextureCubemap(void);               // Disable texture cubemap
RLAPI void rlTextureParameters(unsigned int id, int param, int value); // Set texture parameters (filter, wrap)
RLAPI void rlCubemapParameters(unsigned int id, int param, int value); // Set cubemap parameters (filter, wrap)

// Shader state
RLAPI void rlEnableShader(unsigned int id);             // Enable shader program
RLAPI void rlDisableShader(void);                       // Disable shader program

// Framebuffer state
RLAPI void rlEnableFramebuffer(unsigned int id);        // Enable render texture (fbo)
RLAPI void rlDisableFramebuffer(void);                  // Disable render texture (fbo), return to default framebuffer
RLAPI unsigned int rlGetActiveFramebuffer(void);        // Get the currently active render texture (fbo), 0 for default framebuffer
RLAPI void rlActiveDrawBuffers(int count);              // Activate multiple draw color buffers
RLAPI void rlBlitFramebuffer(int srcX, int srcY, int srcWidth, int srcHeight, int dstX, int dstY, int dstWidth, int dstHeight, int bufferMask); // Blit active framebuffer to main framebuffer
RLAPI void rlBindFramebuffer(unsigned int target, unsigned int framebuffer); // Bind framebuffer (FBO)

// General render state
RLAPI void rlEnableColorBlend(void);                    // Enable color blending
RLAPI void rlDisableColorBlend(void);                   // Disable color blending
RLAPI void rlEnableDepthTest(void);                     // Enable depth test
RLAPI void rlDisableDepthTest(void);                    // Disable depth test
RLAPI void rlEnableDepthMask(void);                     // Enable depth write
RLAPI void rlDisableDepthMask(void);                    // Disable depth write
RLAPI void rlEnableBackfaceCulling(void);               // Enable backface culling
RLAPI void rlDisableBackfaceCulling(void);              // Disable backface culling
RLAPI void rlColorMask(bool r, bool g, bool b, bool a); // Color mask control
RLAPI void rlSetCullFace(int mode);                     // Set face culling mode
RLAPI void rlEnableScissorTest(void);                   // Enable scissor test
RLAPI void rlDisableScissorTest(void);                  // Disable scissor test
RLAPI void rlScissor(int x, int y, int width, int height); // Scissor test
RLAPI void rlEnableWireMode(void);                      // Enable wire mode
RLAPI void rlEnablePointMode(void);                     // Enable point mode
RLAPI void rlDisableWireMode(void);                     // Disable wire (and point) mode
RLAPI void rlSetLineWidth(float width);                 // Set the line drawing width
RLAPI float rlGetLineWidth(void);                       // Get the line drawing width
RLAPI void rlEnableSmoothLines(void);                   // Enable line aliasing
RLAPI void rlDisableSmoothLines(void);                  // Disable line aliasing
RLAPI void rlEnableStereoRender(void);                  // Enable stereo rendering
RLAPI void rlDisableStereoRender(void);                 // Disable stereo rendering
RLAPI bool rlIsStereoRenderEnabled(void);               // Check if stereo render is enabled

RLAPI void rlClearColor(unsigned char r, unsigned char g, unsigned char b, unsigned char a); // Clear color buffer with color
RLAPI void rlClearScreenBuffers(void);                  // Clear used screen buffers (color and depth)
RLAPI void rlCheckErrors(void);                         // Check and log OpenGL error codes
RLAPI void rlSetBlendMode(int mode);                    // Set blending mode
RLAPI void rlSetBlendFactors(int glSrcFactor, int glDstFactor, int glEquation); // Set blending mode factor and equation (using OpenGL factors)
RLAPI void rlSetBlendFactorsSeparate(int glSrcRGB, int glDstRGB, int glSrcAlpha, int glDstAlpha, int glEqRGB, int glEqAlpha); // Set blending mode factors and equations separately (using OpenGL factors)

//------------------------------------------------------------------------------------
// Functions Declaration - rlgl functionality
//------------------------------------------------------------------------------------
// rlgl initialization functions
RLAPI void rlglInit(int width, int height);             // Initialize rlgl (buffers, shaders, textures, states)
RLAPI void rlglClose(void);                             // De-initialize rlgl (buffers, shaders, textures)
RLAPI void rlLoadExtensions(void *loader);              // Load OpenGL extensions (loader function required)
RLAPI int rlGetVersion(void);                           // Get current OpenGL version
RLAPI void rlSetFramebufferWidth(int width);            // Set current framebuffer width
RLAPI int rlGetFramebufferWidth(void);                  // Get default framebuffer width
RLAPI void rlSetFramebufferHeight(int height);          // Set current framebuffer height
RLAPI int rlGetFramebufferHeight(void);                 // Get default framebuffer height

RLAPI unsigned int rlGetTextureIdDefault(void);         // Get default texture id
RLAPI unsigned int rlGetShaderIdDefault(void);          // Get default shader id
RLAPI int *rlGetShaderLocsDefault(void);                // Get default shader locations

// Render batch management
// NOTE: rlgl provides a default render batch to behave like OpenGL 1.1 immediate mode
// but this render batch API is exposed in case of custom batches are required
RLAPI rlRenderBatch rlLoadRenderBatch(int numBuffers, int bufferElements); // Load a render batch system
RLAPI void rlUnloadRenderBatch(rlRenderBatch batch);    // Unload render batch system
RLAPI void rlDrawRenderBatch(rlRenderBatch *batch);     // Draw render batch data (Update->Draw->Reset)
RLAPI void rlSetRenderBatchActive(rlRenderBatch *batch); // Set the active render batch for rlgl (NULL for default internal)
RLAPI void rlDrawRenderBatchActive(void);               // Update and draw internal render batch
RLAPI bool rlCheckRenderBatchLimit(int vCount);         // Check internal buffer overflow for a given number of vertex

RLAPI void rlSetTexture(unsigned int id);               // Set current texture for render batch and check buffers limits

//------------------------------------------------------------------------------------------------------------------------

// Vertex buffers management
RLAPI unsigned int rlLoadVertexArray(void);             // Load vertex array (vao) if supported
RLAPI unsigned int rlLoadVertexBuffer(const void *buffer, int size, bool dynamic); // Load a vertex buffer object
RLAPI unsigned int rlLoadVertexBufferElement(const void *buffer, int size, bool dynamic); // Load vertex buffer elements object
RLAPI void rlUpdateVertexBuffer(unsigned int bufferId, const void *data, int dataSize, int offset); // Update vertex buffer object data on GPU buffer
RLAPI void rlUpdateVertexBufferElements(unsigned int id, const void *data, int dataSize, int offset); // Update vertex buffer elements data on GPU buffer
RLAPI void rlUnloadVertexArray(unsigned int vaoId);     // Unload vertex array (vao)
RLAPI void rlUnloadVertexBuffer(unsigned int vboId);    // Unload vertex buffer object
RLAPI void rlSetVertexAttribute(unsigned int index, int compSize, int type, bool normalized, int stride, int offset); // Set vertex attribute data configuration
RLAPI void rlSetVertexAttributeDivisor(unsigned int index, int divisor); // Set vertex attribute data divisor
RLAPI void rlSetVertexAttributeDefault(int locIndex, const void *value, int attribType, int count); // Set vertex attribute default value, when attribute to provided
RLAPI void rlDrawVertexArray(int offset, int count);    // Draw vertex array (currently active vao)
RLAPI void rlDrawVertexArrayElements(int offset, int count, const void *buffer); // Draw vertex array elements
RLAPI void rlDrawVertexArrayInstanced(int offset, int count, int instances); // Draw vertex array (currently active vao) with instancing
RLAPI void rlDrawVertexArrayElementsInstanced(int offset, int count, const void *buffer, int instances); // Draw vertex array elements with instancing

// Textures management
RLAPI unsigned int rlLoadTexture(const void *data, int width, int height, int format, int mipmapCount); // Load texture data
RLAPI unsigned int rlLoadTextureDepth(int width, int height, bool useRenderBuffer); // Load depth texture/renderbuffer (to be attached to fbo)
RLAPI unsigned int rlLoadTextureCubemap(const void *data, int size, int format, int mipmapCount); // Load texture cubemap data
RLAPI void rlUpdateTexture(unsigned int id, int offsetX, int offsetY, int width, int height, int format, const void *data); // Update texture with new data on GPU
RLAPI void rlGetGlTextureFormats(int format, unsigned int *glInternalFormat, unsigned int *glFormat, unsigned int *glType); // Get OpenGL internal formats
RLAPI const char *rlGetPixelFormatName(unsigned int format);              // Get name string for pixel format
RLAPI void rlUnloadTexture(unsigned int id);                              // Unload texture from GPU memory
RLAPI void rlGenTextureMipmaps(unsigned int id, int width, int height, int format, int *mipmaps); // Generate mipmap data for selected texture
RLAPI void *rlReadTexturePixels(unsigned int id, int width, int height, int format); // Read texture pixel data
RLAPI unsigned char *rlReadScreenPixels(int width, int height);           // Read screen pixel data (color buffer)

// Framebuffer management (fbo)
RLAPI unsigned int rlLoadFramebuffer(void);                               // Load an empty framebuffer
RLAPI void rlFramebufferAttach(unsigned int fboId, unsigned int texId, int attachType, int texType, int mipLevel); // Attach texture/renderbuffer to a framebuffer
RLAPI bool rlFramebufferComplete(unsigned int id);                        // Verify framebuffer is complete
RLAPI void rlUnloadFramebuffer(unsigned int id);                          // Delete framebuffer from GPU

// Shaders management
RLAPI unsigned int rlLoadShaderCode(const char *vsCode, const char *fsCode);    // Load shader from code strings
RLAPI unsigned int rlCompileShader(const char *shaderCode, int type);           // Compile custom shader and return shader id (type: RL_VERTEX_SHADER, RL_FRAGMENT_SHADER, RL_COMPUTE_SHADER)
RLAPI unsigned int rlLoadShaderProgram(unsigned int vShaderId, unsigned int fShaderId); // Load custom shader program
RLAPI void rlUnloadShaderProgram(unsigned int id);                              // Unload shader program
RLAPI int rlGetLocationUniform(unsigned int shaderId, const char *uniformName); // Get shader location uniform
RLAPI int rlGetLocationAttrib(unsigned int shaderId, const char *attribName);   // Get shader location attribute
RLAPI void rlSetUniform(int locIndex, const void *value, int uniformType, int count); // Set shader value uniform
RLAPI void rlSetUniformMatrix(int locIndex, Matrix mat);                        // Set shader value matrix
RLAPI void rlSetUniformMatrices(int locIndex, const Matrix *mat, int count);    // Set shader value matrices
RLAPI void rlSetUniformSampler(int locIndex, unsigned int textureId);           // Set shader value sampler
RLAPI void rlSetShader(unsigned int id, int *locs);                             // Set shader currently active (id and locations)

// Compute shader management
RLAPI unsigned int rlLoadComputeShaderProgram(unsigned int shaderId);           // Load compute shader program
RLAPI void rlComputeShaderDispatch(unsigned int groupX, unsigned int groupY, unsigned int groupZ); // Dispatch compute shader (equivalent to *draw* for graphics pipeline)

// Shader buffer storage object management (ssbo)
RLAPI unsigned int rlLoadShaderBuffer(unsigned int size, const void *data, int usageHint); // Load shader storage buffer object (SSBO)
RLAPI void rlUnloadShaderBuffer(unsigned int ssboId);                           // Unload shader storage buffer object (SSBO)
RLAPI void rlUpdateShaderBuffer(unsigned int id, const void *data, unsigned int dataSize, unsigned int offset); // Update SSBO buffer data
RLAPI void rlBindShaderBuffer(unsigned int id, unsigned int index);             // Bind SSBO buffer
RLAPI void rlReadShaderBuffer(unsigned int id, void *dest, unsigned int count, unsigned int offset); // Read SSBO buffer data (GPU->CPU)
RLAPI void rlCopyShaderBuffer(unsigned int destId, unsigned int srcId, unsigned int destOffset, unsigned int srcOffset, unsigned int count); // Copy SSBO data between buffers
RLAPI unsigned int rlGetShaderBufferSize(unsigned int id);                      // Get SSBO buffer size

// Buffer management
RLAPI void rlBindImageTexture(unsigned int id, unsigned int index, int format, bool readonly);  // Bind image texture

// Matrix state management
RLAPI Matrix rlGetMatrixModelview(void);                                  // Get internal modelview matrix
RLAPI Matrix rlGetMatrixProjection(void);                                 // Get internal projection matrix
RLAPI Matrix rlGetMatrixTransform(void);                                  // Get internal accumulated transform matrix
RLAPI Matrix rlGetMatrixProjectionStereo(int eye);                        // Get internal projection matrix for stereo render (selected eye)
RLAPI Matrix rlGetMatrixViewOffsetStereo(int eye);                        // Get internal view offset matrix for stereo render (selected eye)
RLAPI void rlSetMatrixProjection(Matrix proj);                            // Set a custom projection matrix (replaces internal projection matrix)
RLAPI void rlSetMatrixModelview(Matrix view);                             // Set a custom modelview matrix (replaces internal modelview matrix)
RLAPI void rlSetMatrixProjectionStereo(Matrix right, Matrix left);        // Set eyes projection matrices for stereo rendering
RLAPI void rlSetMatrixViewOffsetStereo(Matrix right, Matrix left);        // Set eyes view offsets matrices for stereo rendering

// Quick and dirty cube/quad buffers load->draw->unload
RLAPI void rlLoadDrawCube(void);     // Load and draw a cube
RLAPI void rlLoadDrawQuad(void);     // Load and draw a quad

#if defined(__cplusplus)
}
#endif

#endif // RLGL_H

/***********************************************************************************
*
*   RLGL IMPLEMENTATION
*
************************************************************************************/

#if defined(RLGL_IMPLEMENTATION)

// Expose OpenGL functions from glad in raylib
#if defined(BUILD_LIBTYPE_SHARED)
    #define GLAD_API_CALL_EXPORT
    #define GLAD_API_CALL_EXPORT_BUILD
#endif

#if defined(GRAPHICS_API_OPENGL_11)
    #if defined(__APPLE__)
        #include <OpenGL/gl.h>          // OpenGL 1.1 library for OSX
        #include <OpenGL/glext.h>       // OpenGL extensions library
    #else
        // APIENTRY for OpenGL function pointer declarations is required
        #if !defined(APIENTRY)
            #if defined(_WIN32)
                #define APIENTRY __stdcall
            #else
                #define APIENTRY
            #endif
        #endif
        // WINGDIAPI definition. Some Windows OpenGL headers need it
        #if !defined(WINGDIAPI) && defined(_WIN32)
            #define WINGDIAPI __declspec(dllimport)
        #endif

        #include <GL/gl.h>              // OpenGL 1.1 library
    #endif
#endif

#if defined(GRAPHICS_API_OPENGL_33)
    #define GLAD_MALLOC RL_MALLOC
    #define GLAD_FREE RL_FREE

    #define GLAD_GL_IMPLEMENTATION
    #include "external/glad.h"          // GLAD extensions loading library, includes OpenGL headers
#endif

#if defined(GRAPHICS_API_OPENGL_ES3)
    #include <GLES3/gl3.h>              // OpenGL ES 3.0 library
    #define GL_GLEXT_PROTOTYPES
    #include <GLES2/gl2ext.h>           // OpenGL ES 2.0 extensions library
#elif defined(GRAPHICS_API_OPENGL_ES2)
    // NOTE: OpenGL ES 2.0 can be enabled on Desktop platforms,
    // in that case, functions are loaded from a custom glad for OpenGL ES 2.0
    #if defined(PLATFORM_DESKTOP_GLFW) || defined(PLATFORM_DESKTOP_SDL)
        #define GLAD_GLES2_IMPLEMENTATION
        #include "external/glad_gles2.h"
    #else
        #define GL_GLEXT_PROTOTYPES
        //#include <EGL/egl.h>          // EGL library -> not required, platform layer
        #include <GLES2/gl2.h>          // OpenGL ES 2.0 library
        #include <GLES2/gl2ext.h>       // OpenGL ES 2.0 extensions library
    #endif

    // It seems OpenGL ES 2.0 instancing entry points are not defined on Raspberry Pi
    // provided headers (despite being defined in official Khronos GLES2 headers)
    #if defined(PLATFORM_DRM)
    typedef void (GL_APIENTRYP PFNGLDRAWARRAYSINSTANCEDEXTPROC) (GLenum mode, GLint start, GLsizei count, GLsizei primcount);
    typedef void (GL_APIENTRYP PFNGLDRAWELEMENTSINSTANCEDEXTPROC) (GLenum mode, GLsizei count, GLenum type, const void *indices, GLsizei primcount);
    typedef void (GL_APIENTRYP PFNGLVERTEXATTRIBDIVISOREXTPROC) (GLuint index, GLuint divisor);
    #endif
#endif

#include <stdlib.h>                     // Required for: malloc(), free()
#include <string.h>                     // Required for: strcmp(), strlen() [Used in rlglInit(), on extensions loading]
#include <math.h>                       // Required for: sqrtf(), sinf(), cosf(), floor(), log()

//----------------------------------------------------------------------------------
// Defines and Macros
//----------------------------------------------------------------------------------
#ifndef PI
    #define PI 3.14159265358979323846f
#endif
#ifndef DEG2RAD
    #define DEG2RAD (PI/180.0f)
#endif
#ifndef RAD2DEG
    #define RAD2DEG (180.0f/PI)
#endif

#ifndef GL_SHADING_LANGUAGE_VERSION
    #define GL_SHADING_LANGUAGE_VERSION         0x8B8C
#endif

#ifndef GL_COMPRESSED_RGB_S3TC_DXT1_EXT
    #define GL_COMPRESSED_RGB_S3TC_DXT1_EXT     0x83F0
#endif
#ifndef GL_COMPRESSED_RGBA_S3TC_DXT1_EXT
    #define GL_COMPRESSED_RGBA_S3TC_DXT1_EXT    0x83F1
#endif
#ifndef GL_COMPRESSED_RGBA_S3TC_DXT3_EXT
    #define GL_COMPRESSED_RGBA_S3TC_DXT3_EXT    0x83F2
#endif
#ifndef GL_COMPRESSED_RGBA_S3TC_DXT5_EXT
    #define GL_COMPRESSED_RGBA_S3TC_DXT5_EXT    0x83F3
#endif
#ifndef GL_ETC1_RGB8_OES
    #define GL_ETC1_RGB8_OES                    0x8D64
#endif
#ifndef GL_COMPRESSED_RGB8_ETC2
    #define GL_COMPRESSED_RGB8_ETC2             0x9274
#endif
#ifndef GL_COMPRESSED_RGBA8_ETC2_EAC
    #define GL_COMPRESSED_RGBA8_ETC2_EAC        0x9278
#endif
#ifndef GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG
    #define GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG  0x8C00
#endif
#ifndef GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG
    #define GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG 0x8C02
#endif
#ifndef GL_COMPRESSED_RGBA_ASTC_4x4_KHR
    #define GL_COMPRESSED_RGBA_ASTC_4x4_KHR     0x93b0
#endif
#ifndef GL_COMPRESSED_RGBA_ASTC_8x8_KHR
    #define GL_COMPRESSED_RGBA_ASTC_8x8_KHR     0x93b7
#endif

#ifndef GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT
    #define GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT   0x84FF
#endif
#ifndef GL_TEXTURE_MAX_ANISOTROPY_EXT
    #define GL_TEXTURE_MAX_ANISOTROPY_EXT       0x84FE
#endif

#ifndef GL_PROGRAM_POINT_SIZE
    #define GL_PROGRAM_POINT_SIZE               0x8642
#endif

#ifndef GL_LINE_WIDTH
    #define GL_LINE_WIDTH                       0x0B21
#endif

#if defined(GRAPHICS_API_OPENGL_11)
    #define GL_UNSIGNED_SHORT_5_6_5             0x8363
    #define GL_UNSIGNED_SHORT_5_5_5_1           0x8034
    #define GL_UNSIGNED_SHORT_4_4_4_4           0x8033
#endif

#if defined(GRAPHICS_API_OPENGL_21)
    #define GL_LUMINANCE                        0x1909
    #define GL_LUMINANCE_ALPHA                  0x190A
#endif

#if defined(GRAPHICS_API_OPENGL_ES2)
    #define glClearDepth                 glClearDepthf
    #if !defined(GRAPHICS_API_OPENGL_ES3)
        #define GL_READ_FRAMEBUFFER         GL_FRAMEBUFFER
        #define GL_DRAW_FRAMEBUFFER         GL_FRAMEBUFFER
    #endif
#endif

// Default shader vertex attribute names to set location points
#ifndef RL_DEFAULT_SHADER_ATTRIB_NAME_POSITION
    #define RL_DEFAULT_SHADER_ATTRIB_NAME_POSITION     "vertexPosition"    // Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_NAME_POSITION
#endif
#ifndef RL_DEFAULT_SHADER_ATTRIB_NAME_TEXCOORD
    #define RL_DEFAULT_SHADER_ATTRIB_NAME_TEXCOORD     "vertexTexCoord"    // Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_NAME_TEXCOORD
#endif
#ifndef RL_DEFAULT_SHADER_ATTRIB_NAME_NORMAL
    #define RL_DEFAULT_SHADER_ATTRIB_NAME_NORMAL       "vertexNormal"      // Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_NAME_NORMAL
#endif
#ifndef RL_DEFAULT_SHADER_ATTRIB_NAME_COLOR
    #define RL_DEFAULT_SHADER_ATTRIB_NAME_COLOR        "vertexColor"       // Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_NAME_COLOR
#endif
#ifndef RL_DEFAULT_SHADER_ATTRIB_NAME_TANGENT
    #define RL_DEFAULT_SHADER_ATTRIB_NAME_TANGENT      "vertexTangent"     // Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_NAME_TANGENT
#endif
#ifndef RL_DEFAULT_SHADER_ATTRIB_NAME_TEXCOORD2
    #define RL_DEFAULT_SHADER_ATTRIB_NAME_TEXCOORD2    "vertexTexCoord2"   // Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_NAME_TEXCOORD2
#endif
#ifndef RL_DEFAULT_SHADER_ATTRIB_NAME_BONEIDS
    #define RL_DEFAULT_SHADER_ATTRIB_NAME_BONEIDS      "vertexBoneIds"     // Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_NAME_BONEIDS
#endif
#ifndef RL_DEFAULT_SHADER_ATTRIB_NAME_BONEWEIGHTS
    #define RL_DEFAULT_SHADER_ATTRIB_NAME_BONEWEIGHTS  "vertexBoneWeights" // Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_NAME_BONEWEIGHTS
#endif

#ifndef RL_DEFAULT_SHADER_UNIFORM_NAME_MVP
    #define RL_DEFAULT_SHADER_UNIFORM_NAME_MVP         "mvp"               // model-view-projection matrix
#endif
#ifndef RL_DEFAULT_SHADER_UNIFORM_NAME_VIEW
    #define RL_DEFAULT_SHADER_UNIFORM_NAME_VIEW        "matView"           // view matrix
#endif
#ifndef RL_DEFAULT_SHADER_UNIFORM_NAME_PROJECTION
    #define RL_DEFAULT_SHADER_UNIFORM_NAME_PROJECTION  "matProjection"     // projection matrix
#endif
#ifndef RL_DEFAULT_SHADER_UNIFORM_NAME_MODEL
    #define RL_DEFAULT_SHADER_UNIFORM_NAME_MODEL       "matModel"          // model matrix
#endif
#ifndef RL_DEFAULT_SHADER_UNIFORM_NAME_NORMAL
    #define RL_DEFAULT_SHADER_UNIFORM_NAME_NORMAL      "matNormal"         // normal matrix (transpose(inverse(matModelView))
#endif
#ifndef RL_DEFAULT_SHADER_UNIFORM_NAME_COLOR
    #define RL_DEFAULT_SHADER_UNIFORM_NAME_COLOR       "colDiffuse"        // color diffuse (base tint color, multiplied by texture color)
#endif
#ifndef RL_DEFAULT_SHADER_UNIFORM_NAME_BONE_MATRICES
    #define RL_DEFAULT_SHADER_UNIFORM_NAME_BONE_MATRICES  "boneMatrices"   // bone matrices
#endif
#ifndef RL_DEFAULT_SHADER_SAMPLER2D_NAME_TEXTURE0
    #define RL_DEFAULT_SHADER_SAMPLER2D_NAME_TEXTURE0  "texture0"          // texture0 (texture slot active 0)
#endif
#ifndef RL_DEFAULT_SHADER_SAMPLER2D_NAME_TEXTURE1
    #define RL_DEFAULT_SHADER_SAMPLER2D_NAME_TEXTURE1  "texture1"          // texture1 (texture slot active 1)
#endif
#ifndef RL_DEFAULT_SHADER_SAMPLER2D_NAME_TEXTURE2
    #define RL_DEFAULT_SHADER_SAMPLER2D_NAME_TEXTURE2  "texture2"          // texture2 (texture slot active 2)
#endif

//----------------------------------------------------------------------------------
// Types and Structures Definition
//----------------------------------------------------------------------------------
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
typedef struct rlglData {
    rlRenderBatch *currentBatch;            // Current render batch
    rlRenderBatch defaultBatch;             // Default internal render batch

    struct {
        int vertexCounter;                  // Current active render batch vertex counter (generic, used for all batches)
        float texcoordx, texcoordy;         // Current active texture coordinate (added on glVertex*())
        float normalx, normaly, normalz;    // Current active normal (added on glVertex*())
        unsigned char colorr, colorg, colorb, colora;   // Current active color (added on glVertex*())

        int currentMatrixMode;              // Current matrix mode
        Matrix *currentMatrix;              // Current matrix pointer
        Matrix modelview;                   // Default modelview matrix
        Matrix projection;                  // Default projection matrix
        Matrix transform;                   // Transform matrix to be used with rlTranslate, rlRotate, rlScale
        bool transformRequired;             // Require transform matrix application to current draw-call vertex (if required)
        Matrix stack[RL_MAX_MATRIX_STACK_SIZE];// Matrix stack for push/pop
        int stackCounter;                   // Matrix stack counter

        unsigned int defaultTextureId;      // Default texture used on shapes/poly drawing (required by shader)
        unsigned int activeTextureId[RL_DEFAULT_BATCH_MAX_TEXTURE_UNITS];    // Active texture ids to be enabled on batch drawing (0 active by default)
        unsigned int defaultVShaderId;      // Default vertex shader id (used by default shader program)
        unsigned int defaultFShaderId;      // Default fragment shader id (used by default shader program)
        unsigned int defaultShaderId;       // Default shader program id, supports vertex color and diffuse texture
        int *defaultShaderLocs;             // Default shader locations pointer to be used on rendering
        unsigned int currentShaderId;       // Current shader id to be used on rendering (by default, defaultShaderId)
        int *currentShaderLocs;             // Current shader locations pointer to be used on rendering (by default, defaultShaderLocs)

        bool stereoRender;                  // Stereo rendering flag
        Matrix projectionStereo[2];         // VR stereo rendering eyes projection matrices
        Matrix viewOffsetStereo[2];         // VR stereo rendering eyes view offset matrices

        // Blending variables
        int currentBlendMode;               // Blending mode active
        int glBlendSrcFactor;               // Blending source factor
        int glBlendDstFactor;               // Blending destination factor
        int glBlendEquation;                // Blending equation
        int glBlendSrcFactorRGB;            // Blending source RGB factor
        int glBlendDestFactorRGB;           // Blending destination RGB factor
        int glBlendSrcFactorAlpha;          // Blending source alpha factor
        int glBlendDestFactorAlpha;         // Blending destination alpha factor
        int glBlendEquationRGB;             // Blending equation for RGB
        int glBlendEquationAlpha;           // Blending equation for alpha
        bool glCustomBlendModeModified;     // Custom blending factor and equation modification status

        int framebufferWidth;               // Current framebuffer width
        int framebufferHeight;              // Current framebuffer height

    } State;            // Renderer state
    struct {
        bool vao;                           // VAO support (OpenGL ES2 could not support VAO extension) (GL_ARB_vertex_array_object)
        bool instancing;                    // Instancing supported (GL_ANGLE_instanced_arrays, GL_EXT_draw_instanced + GL_EXT_instanced_arrays)
        bool texNPOT;                       // NPOT textures full support (GL_ARB_texture_non_power_of_two, GL_OES_texture_npot)
        bool texDepth;                      // Depth textures supported (GL_ARB_depth_texture, GL_OES_depth_texture)
        bool texDepthWebGL;                 // Depth textures supported WebGL specific (GL_WEBGL_depth_texture)
        bool texFloat32;                    // float textures support (32 bit per channel) (GL_OES_texture_float)
        bool texFloat16;                    // half float textures support (16 bit per channel) (GL_OES_texture_half_float)
        bool texCompDXT;                    // DDS texture compression support (GL_EXT_texture_compression_s3tc, GL_WEBGL_compressed_texture_s3tc, GL_WEBKIT_WEBGL_compressed_texture_s3tc)
        bool texCompETC1;                   // ETC1 texture compression support (GL_OES_compressed_ETC1_RGB8_texture, GL_WEBGL_compressed_texture_etc1)
        bool texCompETC2;                   // ETC2/EAC texture compression support (GL_ARB_ES3_compatibility)
        bool texCompPVRT;                   // PVR texture compression support (GL_IMG_texture_compression_pvrtc)
        bool texCompASTC;                   // ASTC texture compression support (GL_KHR_texture_compression_astc_hdr, GL_KHR_texture_compression_astc_ldr)
        bool texMirrorClamp;                // Clamp mirror wrap mode supported (GL_EXT_texture_mirror_clamp)
        bool texAnisoFilter;                // Anisotropic texture filtering support (GL_EXT_texture_filter_anisotropic)
        bool computeShader;                 // Compute shaders support (GL_ARB_compute_shader)
        bool ssbo;                          // Shader storage buffer object support (GL_ARB_shader_storage_buffer_object)

        float maxAnisotropyLevel;           // Maximum anisotropy level supported (minimum is 2.0f)
        int maxDepthBits;                   // Maximum bits for depth component

    } ExtSupported;     // Extensions supported flags
} rlglData;

typedef void *(*rlglLoadProc)(const char *name);   // OpenGL extension functions loader signature (same as GLADloadproc)

#endif  // GRAPHICS_API_OPENGL_33 || GRAPHICS_API_OPENGL_ES2

//----------------------------------------------------------------------------------
// Global Variables Definition
//----------------------------------------------------------------------------------
static double rlCullDistanceNear = RL_CULL_DISTANCE_NEAR;
static double rlCullDistanceFar = RL_CULL_DISTANCE_FAR;

#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
static rlglData RLGL = { 0 };
#endif  // GRAPHICS_API_OPENGL_33 || GRAPHICS_API_OPENGL_ES2

#if defined(GRAPHICS_API_OPENGL_ES2) && !defined(GRAPHICS_API_OPENGL_ES3)
// NOTE: VAO functionality is exposed through extensions (OES)
static PFNGLGENVERTEXARRAYSOESPROC glGenVertexArrays = NULL;
static PFNGLBINDVERTEXARRAYOESPROC glBindVertexArray = NULL;
static PFNGLDELETEVERTEXARRAYSOESPROC glDeleteVertexArrays = NULL;

// NOTE: Instancing functionality could also be available through extension
static PFNGLDRAWARRAYSINSTANCEDEXTPROC glDrawArraysInstanced = NULL;
static PFNGLDRAWELEMENTSINSTANCEDEXTPROC glDrawElementsInstanced = NULL;
static PFNGLVERTEXATTRIBDIVISOREXTPROC glVertexAttribDivisor = NULL;
#endif

//----------------------------------------------------------------------------------
// Module specific Functions Declaration
//----------------------------------------------------------------------------------
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
static void rlLoadShaderDefault(void);      // Load default shader
static void rlUnloadShaderDefault(void);    // Unload default shader
#if defined(RLGL_SHOW_GL_DETAILS_INFO)
static const char *rlGetCompressedFormatName(int format); // Get compressed format official GL identifier name
#endif  // RLGL_SHOW_GL_DETAILS_INFO
#endif  // GRAPHICS_API_OPENGL_33 || GRAPHICS_API_OPENGL_ES2

static int rlGetPixelDataSize(int width, int height, int format);   // Get pixel data size in bytes (image or texture)

// Auxiliar matrix math functions
typedef struct rl_float16 {
    float v[16];
} rl_float16;
static rl_float16 rlMatrixToFloatV(Matrix mat);             // Get float array of matrix data
#define rlMatrixToFloat(mat) (rlMatrixToFloatV(mat).v)      // Get float vector for Matrix
static Matrix rlMatrixIdentity(void);                       // Get identity matrix
static Matrix rlMatrixMultiply(Matrix left, Matrix right);  // Multiply two matrices
static Matrix rlMatrixTranspose(Matrix mat);                // Transposes provided matrix
static Matrix rlMatrixInvert(Matrix mat);                   // Invert provided matrix

//----------------------------------------------------------------------------------
// Module Functions Definition - Matrix operations
//----------------------------------------------------------------------------------

#if defined(GRAPHICS_API_OPENGL_11)
// Fallback to OpenGL 1.1 function calls
//---------------------------------------
void rlMatrixMode(int mode)
{
    switch (mode)
    {
        case RL_PROJECTION: glMatrixMode(GL_PROJECTION); break;
        case RL_MODELVIEW: glMatrixMode(GL_MODELVIEW); break;
        case RL_TEXTURE: glMatrixMode(GL_TEXTURE); break;
        default: break;
    }
}

void rlFrustum(double left, double right, double bottom, double top, double znear, double zfar)
{
    glFrustum(left, right, bottom, top, znear, zfar);
}

void rlOrtho(double left, double right, double bottom, double top, double znear, double zfar)
{
    glOrtho(left, right, bottom, top, znear, zfar);
}

void rlPushMatrix(void) { glPushMatrix(); }
void rlPopMatrix(void) { glPopMatrix(); }
void rlLoadIdentity(void) { glLoadIdentity(); }
void rlTranslatef(float x, float y, float z) { glTranslatef(x, y, z); }
void rlRotatef(float angle, float x, float y, float z) { glRotatef(angle, x, y, z); }
void rlScalef(float x, float y, float z) { glScalef(x, y, z); }
void rlMultMatrixf(const float *matf) { glMultMatrixf(matf); }
#endif
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
// Choose the current matrix to be transformed
void rlMatrixMode(int mode)
{
    if (mode == RL_PROJECTION) RLGL.State.currentMatrix = &RLGL.State.projection;
    else if (mode == RL_MODELVIEW) RLGL.State.currentMatrix = &RLGL.State.modelview;
    //else if (mode == RL_TEXTURE) // Not supported

    RLGL.State.currentMatrixMode = mode;
}

// Push the current matrix into RLGL.State.stack
void rlPushMatrix(void)
{
    if (RLGL.State.stackCounter >= RL_MAX_MATRIX_STACK_SIZE) TRACELOG(RL_LOG_ERROR, "RLGL: Matrix stack overflow (RL_MAX_MATRIX_STACK_SIZE)");

    if (RLGL.State.currentMatrixMode == RL_MODELVIEW)
    {
        RLGL.State.transformRequired = true;
        RLGL.State.currentMatrix = &RLGL.State.transform;
    }

    RLGL.State.stack[RLGL.State.stackCounter] = *RLGL.State.currentMatrix;
    RLGL.State.stackCounter++;
}

// Pop lattest inserted matrix from RLGL.State.stack
void rlPopMatrix(void)
{
    if (RLGL.State.stackCounter > 0)
    {
        Matrix mat = RLGL.State.stack[RLGL.State.stackCounter - 1];
        *RLGL.State.currentMatrix = mat;
        RLGL.State.stackCounter--;
    }

    if ((RLGL.State.stackCounter == 0) && (RLGL.State.currentMatrixMode == RL_MODELVIEW))
    {
        RLGL.State.currentMatrix = &RLGL.State.modelview;
        RLGL.State.transformRequired = false;
    }
}

// Reset current matrix to identity matrix
void rlLoadIdentity(void)
{
    *RLGL.State.currentMatrix = rlMatrixIdentity();
}

// Multiply the current matrix by a translation matrix
void rlTranslatef(float x, float y, float z)
{
    Matrix matTranslation = {
        1.0f, 0.0f, 0.0f, x,
        0.0f, 1.0f, 0.0f, y,
        0.0f, 0.0f, 1.0f, z,
        0.0f, 0.0f, 0.0f, 1.0f
    };

    // NOTE: We transpose matrix with multiplication order
    *RLGL.State.currentMatrix = rlMatrixMultiply(matTranslation, *RLGL.State.currentMatrix);
}

// Multiply the current matrix by a rotation matrix
// NOTE: The provided angle must be in degrees
void rlRotatef(float angle, float x, float y, float z)
{
    Matrix matRotation = rlMatrixIdentity();

    // Axis vector (x, y, z) normalization
    float lengthSquared = x*x + y*y + z*z;
    if ((lengthSquared != 1.0f) && (lengthSquared != 0.0f))
    {
        float inverseLength = 1.0f/sqrtf(lengthSquared);
        x *= inverseLength;
        y *= inverseLength;
        z *= inverseLength;
    }

    // Rotation matrix generation
    float sinres = sinf(DEG2RAD*angle);
    float cosres = cosf(DEG2RAD*angle);
    float t = 1.0f - cosres;

    matRotation.m0 = x*x*t + cosres;
    matRotation.m1 = y*x*t + z*sinres;
    matRotation.m2 = z*x*t - y*sinres;
    matRotation.m3 = 0.0f;

    matRotation.m4 = x*y*t - z*sinres;
    matRotation.m5 = y*y*t + cosres;
    matRotation.m6 = z*y*t + x*sinres;
    matRotation.m7 = 0.0f;

    matRotation.m8 = x*z*t + y*sinres;
    matRotation.m9 = y*z*t - x*sinres;
    matRotation.m10 = z*z*t + cosres;
    matRotation.m11 = 0.0f;

    matRotation.m12 = 0.0f;
    matRotation.m13 = 0.0f;
    matRotation.m14 = 0.0f;
    matRotation.m15 = 1.0f;

    // NOTE: We transpose matrix with multiplication order
    *RLGL.State.currentMatrix = rlMatrixMultiply(matRotation, *RLGL.State.currentMatrix);
}

// Multiply the current matrix by a scaling matrix
void rlScalef(float x, float y, float z)
{
    Matrix matScale = {
        x, 0.0f, 0.0f, 0.0f,
        0.0f, y, 0.0f, 0.0f,
        0.0f, 0.0f, z, 0.0f,
        0.0f, 0.0f, 0.0f, 1.0f
    };

    // NOTE: We transpose matrix with multiplication order
    *RLGL.State.currentMatrix = rlMatrixMultiply(matScale, *RLGL.State.currentMatrix);
}

// Multiply the current matrix by another matrix
void rlMultMatrixf(const float *matf)
{
    // Matrix creation from array
    Matrix mat = { matf[0], matf[4], matf[8], matf[12],
                   matf[1], matf[5], matf[9], matf[13],
                   matf[2], matf[6], matf[10], matf[14],
                   matf[3], matf[7], matf[11], matf[15] };

    *RLGL.State.currentMatrix = rlMatrixMultiply(mat, *RLGL.State.currentMatrix);
}

// Multiply the current matrix by a perspective matrix generated by parameters
void rlFrustum(double left, double right, double bottom, double top, double znear, double zfar)
{
    Matrix matFrustum = { 0 };

    float rl = (float)(right - left);
    float tb = (float)(top - bottom);
    float fn = (float)(zfar - znear);

    matFrustum.m0 = ((float) znear*2.0f)/rl;
    matFrustum.m1 = 0.0f;
    matFrustum.m2 = 0.0f;
    matFrustum.m3 = 0.0f;

    matFrustum.m4 = 0.0f;
    matFrustum.m5 = ((float) znear*2.0f)/tb;
    matFrustum.m6 = 0.0f;
    matFrustum.m7 = 0.0f;

    matFrustum.m8 = ((float)right + (float)left)/rl;
    matFrustum.m9 = ((float)top + (float)bottom)/tb;
    matFrustum.m10 = -((float)zfar + (float)znear)/fn;
    matFrustum.m11 = -1.0f;

    matFrustum.m12 = 0.0f;
    matFrustum.m13 = 0.0f;
    matFrustum.m14 = -((float)zfar*(float)znear*2.0f)/fn;
    matFrustum.m15 = 0.0f;

    *RLGL.State.currentMatrix = rlMatrixMultiply(*RLGL.State.currentMatrix, matFrustum);
}

// Multiply the current matrix by an orthographic matrix generated by parameters
void rlOrtho(double left, double right, double bottom, double top, double znear, double zfar)
{
    // NOTE: If left-right and top-botton values are equal it could create a division by zero,
    // response to it is platform/compiler dependant
    Matrix matOrtho = { 0 };

    float rl = (float)(right - left);
    float tb = (float)(top - bottom);
    float fn = (float)(zfar - znear);

    matOrtho.m0 = 2.0f/rl;
    matOrtho.m1 = 0.0f;
    matOrtho.m2 = 0.0f;
    matOrtho.m3 = 0.0f;
    matOrtho.m4 = 0.0f;
    matOrtho.m5 = 2.0f/tb;
    matOrtho.m6 = 0.0f;
    matOrtho.m7 = 0.0f;
    matOrtho.m8 = 0.0f;
    matOrtho.m9 = 0.0f;
    matOrtho.m10 = -2.0f/fn;
    matOrtho.m11 = 0.0f;
    matOrtho.m12 = -((float)left + (float)right)/rl;
    matOrtho.m13 = -((float)top + (float)bottom)/tb;
    matOrtho.m14 = -((float)zfar + (float)znear)/fn;
    matOrtho.m15 = 1.0f;

    *RLGL.State.currentMatrix = rlMatrixMultiply(*RLGL.State.currentMatrix, matOrtho);
}
#endif

// Set the viewport area (transformation from normalized device coordinates to window coordinates)
// NOTE: We store current viewport dimensions
void rlViewport(int x, int y, int width, int height)
{
    glViewport(x, y, width, height);
}

// Set clip planes distances
void rlSetClipPlanes(double nearPlane, double farPlane)
{
    rlCullDistanceNear = nearPlane;
    rlCullDistanceFar = farPlane;
}

// Get cull plane distance near
double rlGetCullDistanceNear(void)
{
    return rlCullDistanceNear;
}

// Get cull plane distance far
double rlGetCullDistanceFar(void)
{
    return rlCullDistanceFar;
}

//----------------------------------------------------------------------------------
// Module Functions Definition - Vertex level operations
//----------------------------------------------------------------------------------
#if defined(GRAPHICS_API_OPENGL_11)
// Fallback to OpenGL 1.1 function calls
//---------------------------------------
void rlBegin(int mode)
{
    switch (mode)
    {
        case RL_LINES: glBegin(GL_LINES); break;
        case RL_TRIANGLES: glBegin(GL_TRIANGLES); break;
        case RL_QUADS: glBegin(GL_QUADS); break;
        default: break;
    }
}

void rlEnd(void) { glEnd(); }
void rlVertex2i(int x, int y) { glVertex2i(x, y); }
void rlVertex2f(float x, float y) { glVertex2f(x, y); }
void rlVertex3f(float x, float y, float z) { glVertex3f(x, y, z); }
void rlTexCoord2f(float x, float y) { glTexCoord2f(x, y); }
void rlNormal3f(float x, float y, float z) { glNormal3f(x, y, z); }
void rlColor4ub(unsigned char r, unsigned char g, unsigned char b, unsigned char a) { glColor4ub(r, g, b, a); }
void rlColor3f(float x, float y, float z) { glColor3f(x, y, z); }
void rlColor4f(float x, float y, float z, float w) { glColor4f(x, y, z, w); }
#endif
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
// Initialize drawing mode (how to organize vertex)
void rlBegin(int mode)
{
    // Draw mode can be RL_LINES, RL_TRIANGLES and RL_QUADS
    // NOTE: In all three cases, vertex are accumulated over default internal vertex buffer
    if (RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].mode != mode)
    {
        if (RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexCount > 0)
        {
            // Make sure current RLGL.currentBatch->draws[i].vertexCount is aligned a multiple of 4,
            // that way, following QUADS drawing will keep aligned with index processing
            // It implies adding some extra alignment vertex at the end of the draw,
            // those vertex are not processed but they are considered as an additional offset
            // for the next set of vertex to be drawn
            if (RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].mode == RL_LINES) RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexAlignment = ((RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexCount < 4)? RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexCount : RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexCount%4);
            else if (RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].mode == RL_TRIANGLES) RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexAlignment = ((RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexCount < 4)? 1 : (4 - (RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexCount%4)));
            else RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexAlignment = 0;

            if (!rlCheckRenderBatchLimit(RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexAlignment))
            {
                RLGL.State.vertexCounter += RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexAlignment;
                RLGL.currentBatch->drawCounter++;
            }
        }

        if (RLGL.currentBatch->drawCounter >= RL_DEFAULT_BATCH_DRAWCALLS) rlDrawRenderBatch(RLGL.currentBatch);

        RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].mode = mode;
        RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexCount = 0;
        RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].textureId = RLGL.State.defaultTextureId;
    }
}

// Finish vertex providing
void rlEnd(void)
{
    // NOTE: Depth increment is dependant on rlOrtho(): z-near and z-far values,
    // as well as depth buffer bit-depth (16bit or 24bit or 32bit)
    // Correct increment formula would be: depthInc = (zfar - znear)/pow(2, bits)
    RLGL.currentBatch->currentDepth += (1.0f/20000.0f);
}

// Define one vertex (position)
// NOTE: Vertex position data is the basic information required for drawing
void rlVertex3f(float x, float y, float z)
{
    float tx = x;
    float ty = y;
    float tz = z;

    // Transform provided vector if required
    if (RLGL.State.transformRequired)
    {
        tx = RLGL.State.transform.m0*x + RLGL.State.transform.m4*y + RLGL.State.transform.m8*z + RLGL.State.transform.m12;
        ty = RLGL.State.transform.m1*x + RLGL.State.transform.m5*y + RLGL.State.transform.m9*z + RLGL.State.transform.m13;
        tz = RLGL.State.transform.m2*x + RLGL.State.transform.m6*y + RLGL.State.transform.m10*z + RLGL.State.transform.m14;
    }

    // WARNING: We can't break primitives when launching a new batch
    // RL_LINES comes in pairs, RL_TRIANGLES come in groups of 3 vertices and RL_QUADS come in groups of 4 vertices
    // We must check current draw.mode when a new vertex is required and finish the batch only if the draw.mode draw.vertexCount is %2, %3 or %4
    if (RLGL.State.vertexCounter > (RLGL.currentBatch->vertexBuffer[RLGL.currentBatch->currentBuffer].elementCount*4 - 4))
    {
        if ((RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].mode == RL_LINES) &&
            (RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexCount%2 == 0))
        {
            // Reached the maximum number of vertices for RL_LINES drawing
            // Launch a draw call but keep current state for next vertices comming
            // NOTE: We add +1 vertex to the check for security
            rlCheckRenderBatchLimit(2 + 1);
        }
        else if ((RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].mode == RL_TRIANGLES) &&
            (RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexCount%3 == 0))
        {
            rlCheckRenderBatchLimit(3 + 1);
        }
        else if ((RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].mode == RL_QUADS) &&
            (RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexCount%4 == 0))
        {
            rlCheckRenderBatchLimit(4 + 1);
        }
    }

    // Add vertices
    RLGL.currentBatch->vertexBuffer[RLGL.currentBatch->currentBuffer].vertices[3*RLGL.State.vertexCounter] = tx;
    RLGL.currentBatch->vertexBuffer[RLGL.currentBatch->currentBuffer].vertices[3*RLGL.State.vertexCounter + 1] = ty;
    RLGL.currentBatch->vertexBuffer[RLGL.currentBatch->currentBuffer].vertices[3*RLGL.State.vertexCounter + 2] = tz;

    // Add current texcoord
    RLGL.currentBatch->vertexBuffer[RLGL.currentBatch->currentBuffer].texcoords[2*RLGL.State.vertexCounter] = RLGL.State.texcoordx;
    RLGL.currentBatch->vertexBuffer[RLGL.currentBatch->currentBuffer].texcoords[2*RLGL.State.vertexCounter + 1] = RLGL.State.texcoordy;

    // Add current normal
    RLGL.currentBatch->vertexBuffer[RLGL.currentBatch->currentBuffer].normals[3*RLGL.State.vertexCounter] = RLGL.State.normalx;
    RLGL.currentBatch->vertexBuffer[RLGL.currentBatch->currentBuffer].normals[3*RLGL.State.vertexCounter + 1] = RLGL.State.normaly;
    RLGL.currentBatch->vertexBuffer[RLGL.currentBatch->currentBuffer].normals[3*RLGL.State.vertexCounter + 2] = RLGL.State.normalz;

    // Add current color
    RLGL.currentBatch->vertexBuffer[RLGL.currentBatch->currentBuffer].colors[4*RLGL.State.vertexCounter] = RLGL.State.colorr;
    RLGL.currentBatch->vertexBuffer[RLGL.currentBatch->currentBuffer].colors[4*RLGL.State.vertexCounter + 1] = RLGL.State.colorg;
    RLGL.currentBatch->vertexBuffer[RLGL.currentBatch->currentBuffer].colors[4*RLGL.State.vertexCounter + 2] = RLGL.State.colorb;
    RLGL.currentBatch->vertexBuffer[RLGL.currentBatch->currentBuffer].colors[4*RLGL.State.vertexCounter + 3] = RLGL.State.colora;

    RLGL.State.vertexCounter++;
    RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexCount++;
}

// Define one vertex (position)
void rlVertex2f(float x, float y)
{
    rlVertex3f(x, y, RLGL.currentBatch->currentDepth);
}

// Define one vertex (position)
void rlVertex2i(int x, int y)
{
    rlVertex3f((float)x, (float)y, RLGL.currentBatch->currentDepth);
}

// Define one vertex (texture coordinate)
// NOTE: Texture coordinates are limited to QUADS only
void rlTexCoord2f(float x, float y)
{
    RLGL.State.texcoordx = x;
    RLGL.State.texcoordy = y;
}

// Define one vertex (normal)
// NOTE: Normals limited to TRIANGLES only?
void rlNormal3f(float x, float y, float z)
{
    float normalx = x;
    float normaly = y;
    float normalz = z;
    if (RLGL.State.transformRequired)
    {
        normalx = RLGL.State.transform.m0*x + RLGL.State.transform.m4*y + RLGL.State.transform.m8*z;
        normaly = RLGL.State.transform.m1*x + RLGL.State.transform.m5*y + RLGL.State.transform.m9*z;
        normalz = RLGL.State.transform.m2*x + RLGL.State.transform.m6*y + RLGL.State.transform.m10*z;
    }
    float length = sqrtf(normalx*normalx + normaly*normaly + normalz*normalz);
    if (length != 0.0f)
    {
        float ilength = 1.0f/length;
        normalx *= ilength;
        normaly *= ilength;
        normalz *= ilength;
    }
    RLGL.State.normalx = normalx;
    RLGL.State.normaly = normaly;
    RLGL.State.normalz = normalz;
}

// Define one vertex (color)
void rlColor4ub(unsigned char x, unsigned char y, unsigned char z, unsigned char w)
{
    RLGL.State.colorr = x;
    RLGL.State.colorg = y;
    RLGL.State.colorb = z;
    RLGL.State.colora = w;
}

// Define one vertex (color)
void rlColor4f(float r, float g, float b, float a)
{
    rlColor4ub((unsigned char)(r*255), (unsigned char)(g*255), (unsigned char)(b*255), (unsigned char)(a*255));
}

// Define one vertex (color)
void rlColor3f(float x, float y, float z)
{
    rlColor4ub((unsigned char)(x*255), (unsigned char)(y*255), (unsigned char)(z*255), 255);
}

#endif

//--------------------------------------------------------------------------------------
// Module Functions Definition - OpenGL style functions (common to 1.1, 3.3+, ES2)
//--------------------------------------------------------------------------------------

// Set current texture to use
void rlSetTexture(unsigned int id)
{
    if (id == 0)
    {
#if defined(GRAPHICS_API_OPENGL_11)
        rlDisableTexture();
#else
        // NOTE: If quads batch limit is reached, we force a draw call and next batch starts
        if (RLGL.State.vertexCounter >=
            RLGL.currentBatch->vertexBuffer[RLGL.currentBatch->currentBuffer].elementCount*4)
        {
            rlDrawRenderBatch(RLGL.currentBatch);
        }
#endif
    }
    else
    {
#if defined(GRAPHICS_API_OPENGL_11)
        rlEnableTexture(id);
#else
        if (RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].textureId != id)
        {
            if (RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexCount > 0)
            {
                // Make sure current RLGL.currentBatch->draws[i].vertexCount is aligned a multiple of 4,
                // that way, following QUADS drawing will keep aligned with index processing
                // It implies adding some extra alignment vertex at the end of the draw,
                // those vertex are not processed but they are considered as an additional offset
                // for the next set of vertex to be drawn
                if (RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].mode == RL_LINES) RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexAlignment = ((RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexCount < 4)? RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexCount : RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexCount%4);
                else if (RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].mode == RL_TRIANGLES) RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexAlignment = ((RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexCount < 4)? 1 : (4 - (RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexCount%4)));
                else RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexAlignment = 0;

                if (!rlCheckRenderBatchLimit(RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexAlignment))
                {
                    RLGL.State.vertexCounter += RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexAlignment;

                    RLGL.currentBatch->drawCounter++;
                }
            }

            if (RLGL.currentBatch->drawCounter >= RL_DEFAULT_BATCH_DRAWCALLS) rlDrawRenderBatch(RLGL.currentBatch);

            RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].textureId = id;
            RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].vertexCount = 0;
        }
#endif
    }
}

// Select and active a texture slot
void rlActiveTextureSlot(int slot)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    glActiveTexture(GL_TEXTURE0 + slot);
#endif
}

// Enable texture
void rlEnableTexture(unsigned int id)
{
#if defined(GRAPHICS_API_OPENGL_11)
    glEnable(GL_TEXTURE_2D);
#endif
    glBindTexture(GL_TEXTURE_2D, id);
}

// Disable texture
void rlDisableTexture(void)
{
#if defined(GRAPHICS_API_OPENGL_11)
    glDisable(GL_TEXTURE_2D);
#endif
    glBindTexture(GL_TEXTURE_2D, 0);
}

// Enable texture cubemap
void rlEnableTextureCubemap(unsigned int id)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    glBindTexture(GL_TEXTURE_CUBE_MAP, id);
#endif
}

// Disable texture cubemap
void rlDisableTextureCubemap(void)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    glBindTexture(GL_TEXTURE_CUBE_MAP, 0);
#endif
}

// Set texture parameters (wrap mode/filter mode)
void rlTextureParameters(unsigned int id, int param, int value)
{
    glBindTexture(GL_TEXTURE_2D, id);

#if !defined(GRAPHICS_API_OPENGL_11)
    // Reset anisotropy filter, in case it was set
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1.0f);
#endif

    switch (param)
    {
        case RL_TEXTURE_WRAP_S:
        case RL_TEXTURE_WRAP_T:
        {
            if (value == RL_TEXTURE_WRAP_MIRROR_CLAMP)
            {
#if !defined(GRAPHICS_API_OPENGL_11)
                if (RLGL.ExtSupported.texMirrorClamp) glTexParameteri(GL_TEXTURE_2D, param, value);
                else TRACELOG(RL_LOG_WARNING, "GL: Clamp mirror wrap mode not supported (GL_MIRROR_CLAMP_EXT)");
#endif
            }
            else glTexParameteri(GL_TEXTURE_2D, param, value);

        } break;
        case RL_TEXTURE_MAG_FILTER:
        case RL_TEXTURE_MIN_FILTER: glTexParameteri(GL_TEXTURE_2D, param, value); break;
        case RL_TEXTURE_FILTER_ANISOTROPIC:
        {
#if !defined(GRAPHICS_API_OPENGL_11)
            if (value <= RLGL.ExtSupported.maxAnisotropyLevel) glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, (float)value);
            else if (RLGL.ExtSupported.maxAnisotropyLevel > 0.0f)
            {
                TRACELOG(RL_LOG_WARNING, "GL: Maximum anisotropic filter level supported is %iX", id, (int)RLGL.ExtSupported.maxAnisotropyLevel);
                glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, (float)value);
            }
            else TRACELOG(RL_LOG_WARNING, "GL: Anisotropic filtering not supported");
#endif
        } break;
#if defined(GRAPHICS_API_OPENGL_33)
        case RL_TEXTURE_MIPMAP_BIAS_RATIO: glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_LOD_BIAS, value/100.0f);
#endif
        default: break;
    }

    glBindTexture(GL_TEXTURE_2D, 0);
}

// Set cubemap parameters (wrap mode/filter mode)
void rlCubemapParameters(unsigned int id, int param, int value)
{
#if !defined(GRAPHICS_API_OPENGL_11)
    glBindTexture(GL_TEXTURE_CUBE_MAP, id);

    // Reset anisotropy filter, in case it was set
    glTexParameterf(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1.0f);

    switch (param)
    {
        case RL_TEXTURE_WRAP_S:
        case RL_TEXTURE_WRAP_T:
        {
            if (value == RL_TEXTURE_WRAP_MIRROR_CLAMP)
            {
                if (RLGL.ExtSupported.texMirrorClamp) glTexParameteri(GL_TEXTURE_CUBE_MAP, param, value);
                else TRACELOG(RL_LOG_WARNING, "GL: Clamp mirror wrap mode not supported (GL_MIRROR_CLAMP_EXT)");
            }
            else glTexParameteri(GL_TEXTURE_CUBE_MAP, param, value);

        } break;
        case RL_TEXTURE_MAG_FILTER:
        case RL_TEXTURE_MIN_FILTER: glTexParameteri(GL_TEXTURE_CUBE_MAP, param, value); break;
        case RL_TEXTURE_FILTER_ANISOTROPIC:
        {
            if (value <= RLGL.ExtSupported.maxAnisotropyLevel) glTexParameterf(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAX_ANISOTROPY_EXT, (float)value);
            else if (RLGL.ExtSupported.maxAnisotropyLevel > 0.0f)
            {
                TRACELOG(RL_LOG_WARNING, "GL: Maximum anisotropic filter level supported is %iX", id, (int)RLGL.ExtSupported.maxAnisotropyLevel);
                glTexParameterf(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAX_ANISOTROPY_EXT, (float)value);
            }
            else TRACELOG(RL_LOG_WARNING, "GL: Anisotropic filtering not supported");
        } break;
#if defined(GRAPHICS_API_OPENGL_33)
        case RL_TEXTURE_MIPMAP_BIAS_RATIO: glTexParameterf(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_LOD_BIAS, value/100.0f);
#endif
        default: break;
    }

    glBindTexture(GL_TEXTURE_CUBE_MAP, 0);
#endif
}

// Enable shader program
void rlEnableShader(unsigned int id)
{
#if (defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2))
    glUseProgram(id);
#endif
}

// Disable shader program
void rlDisableShader(void)
{
#if (defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2))
    glUseProgram(0);
#endif
}

// Enable rendering to texture (fbo)
void rlEnableFramebuffer(unsigned int id)
{
#if (defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)) && defined(RLGL_RENDER_TEXTURES_HINT)
    glBindFramebuffer(GL_FRAMEBUFFER, id);
#endif
}

// return the active render texture (fbo)
unsigned int rlGetActiveFramebuffer(void)
{
    GLint fboId = 0;
#if (defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES3)) && defined(RLGL_RENDER_TEXTURES_HINT)
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &fboId);
#endif
    return fboId;
}

// Disable rendering to texture
void rlDisableFramebuffer(void)
{
#if (defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)) && defined(RLGL_RENDER_TEXTURES_HINT)
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
#endif
}

// Blit active framebuffer to main framebuffer
void rlBlitFramebuffer(int srcX, int srcY, int srcWidth, int srcHeight, int dstX, int dstY, int dstWidth, int dstHeight, int bufferMask)
{
#if (defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES3)) && defined(RLGL_RENDER_TEXTURES_HINT)
    glBlitFramebuffer(srcX, srcY, srcWidth, srcHeight, dstX, dstY, dstWidth, dstHeight, bufferMask, GL_NEAREST);
#endif
}

// Bind framebuffer object (fbo)
void rlBindFramebuffer(unsigned int target, unsigned int framebuffer)
{
#if (defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)) && defined(RLGL_RENDER_TEXTURES_HINT)
    glBindFramebuffer(target, framebuffer);
#endif
}

// Activate multiple draw color buffers
// NOTE: One color buffer is always active by default
void rlActiveDrawBuffers(int count)
{
#if ((defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES3)) && defined(RLGL_RENDER_TEXTURES_HINT))
    // NOTE: Maximum number of draw buffers supported is implementation dependant,
    // it can be queried with glGet*() but it must be at least 8
    //GLint maxDrawBuffers = 0;
    //glGetIntegerv(GL_MAX_DRAW_BUFFERS, &maxDrawBuffers);

    if (count > 0)
    {
        if (count > 8) TRACELOG(LOG_WARNING, "GL: Max color buffers limited to 8");
        else
        {
            unsigned int buffers[8] = {
#if defined(GRAPHICS_API_OPENGL_ES3)
                GL_COLOR_ATTACHMENT0_EXT,
                GL_COLOR_ATTACHMENT1_EXT,
                GL_COLOR_ATTACHMENT2_EXT,
                GL_COLOR_ATTACHMENT3_EXT,
                GL_COLOR_ATTACHMENT4_EXT,
                GL_COLOR_ATTACHMENT5_EXT,
                GL_COLOR_ATTACHMENT6_EXT,
                GL_COLOR_ATTACHMENT7_EXT,
#else
                GL_COLOR_ATTACHMENT0,
                GL_COLOR_ATTACHMENT1,
                GL_COLOR_ATTACHMENT2,
                GL_COLOR_ATTACHMENT3,
                GL_COLOR_ATTACHMENT4,
                GL_COLOR_ATTACHMENT5,
                GL_COLOR_ATTACHMENT6,
                GL_COLOR_ATTACHMENT7,
#endif
            };

#if defined(GRAPHICS_API_OPENGL_ES3)
            glDrawBuffersEXT(count, buffers);
#else
            glDrawBuffers(count, buffers);
#endif
        }
    }
    else TRACELOG(LOG_WARNING, "GL: One color buffer active by default");
#endif
}

//----------------------------------------------------------------------------------
// General render state configuration
//----------------------------------------------------------------------------------

// Enable color blending
void rlEnableColorBlend(void) { glEnable(GL_BLEND); }

// Disable color blending
void rlDisableColorBlend(void) { glDisable(GL_BLEND); }

// Enable depth test
void rlEnableDepthTest(void) { glEnable(GL_DEPTH_TEST); }

// Disable depth test
void rlDisableDepthTest(void) { glDisable(GL_DEPTH_TEST); }

// Enable depth write
void rlEnableDepthMask(void) { glDepthMask(GL_TRUE); }

// Disable depth write
void rlDisableDepthMask(void) { glDepthMask(GL_FALSE); }

// Enable backface culling
void rlEnableBackfaceCulling(void) { glEnable(GL_CULL_FACE); }

// Disable backface culling
void rlDisableBackfaceCulling(void) { glDisable(GL_CULL_FACE); }

// Set color mask active for screen read/draw
void rlColorMask(bool r, bool g, bool b, bool a) { glColorMask(r, g, b, a); }

// Set face culling mode
void rlSetCullFace(int mode)
{
    switch (mode)
    {
        case RL_CULL_FACE_BACK: glCullFace(GL_BACK); break;
        case RL_CULL_FACE_FRONT: glCullFace(GL_FRONT); break;
        default: break;
    }
}

// Enable scissor test
void rlEnableScissorTest(void) { glEnable(GL_SCISSOR_TEST); }

// Disable scissor test
void rlDisableScissorTest(void) { glDisable(GL_SCISSOR_TEST); }

// Scissor test
void rlScissor(int x, int y, int width, int height) { glScissor(x, y, width, height); }

// Enable wire mode
void rlEnableWireMode(void)
{
#if defined(GRAPHICS_API_OPENGL_11) || defined(GRAPHICS_API_OPENGL_33)
    // NOTE: glPolygonMode() not available on OpenGL ES
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
#endif
}

// Enable point mode
void rlEnablePointMode(void)
{
#if defined(GRAPHICS_API_OPENGL_11) || defined(GRAPHICS_API_OPENGL_33)
    // NOTE: glPolygonMode() not available on OpenGL ES
    glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
    glEnable(GL_PROGRAM_POINT_SIZE);
#endif
}

// Disable wire mode
void rlDisableWireMode(void)
{
#if defined(GRAPHICS_API_OPENGL_11) || defined(GRAPHICS_API_OPENGL_33)
    // NOTE: glPolygonMode() not available on OpenGL ES
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
#endif
}

// Set the line drawing width
void rlSetLineWidth(float width) { glLineWidth(width); }

// Get the line drawing width
float rlGetLineWidth(void)
{
    float width = 0;
    glGetFloatv(GL_LINE_WIDTH, &width);
    return width;
}

// Enable line aliasing
void rlEnableSmoothLines(void)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_11)
    glEnable(GL_LINE_SMOOTH);
#endif
}

// Disable line aliasing
void rlDisableSmoothLines(void)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_11)
    glDisable(GL_LINE_SMOOTH);
#endif
}

// Enable stereo rendering
void rlEnableStereoRender(void)
{
#if (defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2))
    RLGL.State.stereoRender = true;
#endif
}

// Disable stereo rendering
void rlDisableStereoRender(void)
{
#if (defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2))
    RLGL.State.stereoRender = false;
#endif
}

// Check if stereo render is enabled
bool rlIsStereoRenderEnabled(void)
{
#if (defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2))
    return RLGL.State.stereoRender;
#else
    return false;
#endif
}

// Clear color buffer with color
void rlClearColor(unsigned char r, unsigned char g, unsigned char b, unsigned char a)
{
    // Color values clamp to 0.0f(0) and 1.0f(255)
    float cr = (float)r/255;
    float cg = (float)g/255;
    float cb = (float)b/255;
    float ca = (float)a/255;

    glClearColor(cr, cg, cb, ca);
}

// Clear used screen buffers (color and depth)
void rlClearScreenBuffers(void)
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);     // Clear used buffers: Color and Depth (Depth is used for 3D)
    //glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);     // Stencil buffer not used...
}

// Check and log OpenGL error codes
void rlCheckErrors(void)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    int check = 1;
    while (check)
    {
        const GLenum err = glGetError();
        switch (err)
        {
            case GL_NO_ERROR: check = 0; break;
            case 0x0500: TRACELOG(RL_LOG_WARNING, "GL: Error detected: GL_INVALID_ENUM"); break;
            case 0x0501: TRACELOG(RL_LOG_WARNING, "GL: Error detected: GL_INVALID_VALUE"); break;
            case 0x0502: TRACELOG(RL_LOG_WARNING, "GL: Error detected: GL_INVALID_OPERATION"); break;
            case 0x0503: TRACELOG(RL_LOG_WARNING, "GL: Error detected: GL_STACK_OVERFLOW"); break;
            case 0x0504: TRACELOG(RL_LOG_WARNING, "GL: Error detected: GL_STACK_UNDERFLOW"); break;
            case 0x0505: TRACELOG(RL_LOG_WARNING, "GL: Error detected: GL_OUT_OF_MEMORY"); break;
            case 0x0506: TRACELOG(RL_LOG_WARNING, "GL: Error detected: GL_INVALID_FRAMEBUFFER_OPERATION"); break;
            default: TRACELOG(RL_LOG_WARNING, "GL: Error detected: Unknown error code: %x", err); break;
        }
    }
#endif
}

// Set blend mode
void rlSetBlendMode(int mode)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    if ((RLGL.State.currentBlendMode != mode) || ((mode == RL_BLEND_CUSTOM || mode == RL_BLEND_CUSTOM_SEPARATE) && RLGL.State.glCustomBlendModeModified))
    {
        rlDrawRenderBatch(RLGL.currentBatch);

        switch (mode)
        {
            case RL_BLEND_ALPHA: glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); glBlendEquation(GL_FUNC_ADD); break;
            case RL_BLEND_ADDITIVE: glBlendFunc(GL_SRC_ALPHA, GL_ONE); glBlendEquation(GL_FUNC_ADD); break;
            case RL_BLEND_MULTIPLIED: glBlendFunc(GL_DST_COLOR, GL_ONE_MINUS_SRC_ALPHA); glBlendEquation(GL_FUNC_ADD); break;
            case RL_BLEND_ADD_COLORS: glBlendFunc(GL_ONE, GL_ONE); glBlendEquation(GL_FUNC_ADD); break;
            case RL_BLEND_SUBTRACT_COLORS: glBlendFunc(GL_ONE, GL_ONE); glBlendEquation(GL_FUNC_SUBTRACT); break;
            case RL_BLEND_ALPHA_PREMULTIPLY: glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA); glBlendEquation(GL_FUNC_ADD); break;
            case RL_BLEND_CUSTOM:
            {
                // NOTE: Using GL blend src/dst factors and GL equation configured with rlSetBlendFactors()
                glBlendFunc(RLGL.State.glBlendSrcFactor, RLGL.State.glBlendDstFactor); glBlendEquation(RLGL.State.glBlendEquation);

            } break;
            case RL_BLEND_CUSTOM_SEPARATE:
            {
                // NOTE: Using GL blend src/dst factors and GL equation configured with rlSetBlendFactorsSeparate()
                glBlendFuncSeparate(RLGL.State.glBlendSrcFactorRGB, RLGL.State.glBlendDestFactorRGB, RLGL.State.glBlendSrcFactorAlpha, RLGL.State.glBlendDestFactorAlpha);
                glBlendEquationSeparate(RLGL.State.glBlendEquationRGB, RLGL.State.glBlendEquationAlpha);

            } break;
            default: break;
        }

        RLGL.State.currentBlendMode = mode;
        RLGL.State.glCustomBlendModeModified = false;
    }
#endif
}

// Set blending mode factor and equation
void rlSetBlendFactors(int glSrcFactor, int glDstFactor, int glEquation)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    if ((RLGL.State.glBlendSrcFactor != glSrcFactor) ||
        (RLGL.State.glBlendDstFactor != glDstFactor) ||
        (RLGL.State.glBlendEquation != glEquation))
    {
        RLGL.State.glBlendSrcFactor = glSrcFactor;
        RLGL.State.glBlendDstFactor = glDstFactor;
        RLGL.State.glBlendEquation = glEquation;

        RLGL.State.glCustomBlendModeModified = true;
    }
#endif
}

// Set blending mode factor and equation separately for RGB and alpha
void rlSetBlendFactorsSeparate(int glSrcRGB, int glDstRGB, int glSrcAlpha, int glDstAlpha, int glEqRGB, int glEqAlpha)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    if ((RLGL.State.glBlendSrcFactorRGB != glSrcRGB) ||
        (RLGL.State.glBlendDestFactorRGB != glDstRGB) ||
        (RLGL.State.glBlendSrcFactorAlpha != glSrcAlpha) ||
        (RLGL.State.glBlendDestFactorAlpha != glDstAlpha) ||
        (RLGL.State.glBlendEquationRGB != glEqRGB) ||
        (RLGL.State.glBlendEquationAlpha != glEqAlpha))
    {
        RLGL.State.glBlendSrcFactorRGB = glSrcRGB;
        RLGL.State.glBlendDestFactorRGB = glDstRGB;
        RLGL.State.glBlendSrcFactorAlpha = glSrcAlpha;
        RLGL.State.glBlendDestFactorAlpha = glDstAlpha;
        RLGL.State.glBlendEquationRGB = glEqRGB;
        RLGL.State.glBlendEquationAlpha = glEqAlpha;

        RLGL.State.glCustomBlendModeModified = true;
    }
#endif
}

//----------------------------------------------------------------------------------
// Module Functions Definition - OpenGL Debug
//----------------------------------------------------------------------------------
#if defined(RLGL_ENABLE_OPENGL_DEBUG_CONTEXT) && defined(GRAPHICS_API_OPENGL_43)
static void GLAPIENTRY rlDebugMessageCallback(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar *message, const void *userParam)
{
    // Ignore non-significant error/warning codes (NVidia drivers)
    // NOTE: Here there are the details with a sample output:
    // - #131169 - Framebuffer detailed info: The driver allocated storage for renderbuffer 2. (severity: low)
    // - #131185 - Buffer detailed info: Buffer object 1 (bound to GL_ELEMENT_ARRAY_BUFFER_ARB, usage hint is GL_ENUM_88e4)
    //             will use VIDEO memory as the source for buffer object operations. (severity: low)
    // - #131218 - Program/shader state performance warning: Vertex shader in program 7 is being recompiled based on GL state. (severity: medium)
    // - #131204 - Texture state usage warning: The texture object (0) bound to texture image unit 0 does not have
    //             a defined base level and cannot be used for texture mapping. (severity: low)
    if ((id == 131169) || (id == 131185) || (id == 131218) || (id == 131204)) return;

    const char *msgSource = NULL;
    switch (source)
    {
        case GL_DEBUG_SOURCE_API: msgSource = "API"; break;
        case GL_DEBUG_SOURCE_WINDOW_SYSTEM: msgSource = "WINDOW_SYSTEM"; break;
        case GL_DEBUG_SOURCE_SHADER_COMPILER: msgSource = "SHADER_COMPILER"; break;
        case GL_DEBUG_SOURCE_THIRD_PARTY: msgSource = "THIRD_PARTY"; break;
        case GL_DEBUG_SOURCE_APPLICATION: msgSource = "APPLICATION"; break;
        case GL_DEBUG_SOURCE_OTHER: msgSource = "OTHER"; break;
        default: break;
    }

    const char *msgType = NULL;
    switch (type)
    {
        case GL_DEBUG_TYPE_ERROR: msgType = "ERROR"; break;
        case GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR: msgType = "DEPRECATED_BEHAVIOR"; break;
        case GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR: msgType = "UNDEFINED_BEHAVIOR"; break;
        case GL_DEBUG_TYPE_PORTABILITY: msgType = "PORTABILITY"; break;
        case GL_DEBUG_TYPE_PERFORMANCE: msgType = "PERFORMANCE"; break;
        case GL_DEBUG_TYPE_MARKER: msgType = "MARKER"; break;
        case GL_DEBUG_TYPE_PUSH_GROUP: msgType = "PUSH_GROUP"; break;
        case GL_DEBUG_TYPE_POP_GROUP: msgType = "POP_GROUP"; break;
        case GL_DEBUG_TYPE_OTHER: msgType = "OTHER"; break;
        default: break;
    }

    const char *msgSeverity = "DEFAULT";
    switch (severity)
    {
        case GL_DEBUG_SEVERITY_LOW: msgSeverity = "LOW"; break;
        case GL_DEBUG_SEVERITY_MEDIUM: msgSeverity = "MEDIUM"; break;
        case GL_DEBUG_SEVERITY_HIGH: msgSeverity = "HIGH"; break;
        case GL_DEBUG_SEVERITY_NOTIFICATION: msgSeverity = "NOTIFICATION"; break;
        default: break;
    }

    TRACELOG(LOG_WARNING, "GL: OpenGL debug message: %s", message);
    TRACELOG(LOG_WARNING, "    > Type: %s", msgType);
    TRACELOG(LOG_WARNING, "    > Source = %s", msgSource);
    TRACELOG(LOG_WARNING, "    > Severity = %s", msgSeverity);
}
#endif

//----------------------------------------------------------------------------------
// Module Functions Definition - rlgl functionality
//----------------------------------------------------------------------------------

// Initialize rlgl: OpenGL extensions, default buffers/shaders/textures, OpenGL states
void rlglInit(int width, int height)
{
    // Enable OpenGL debug context if required
#if defined(RLGL_ENABLE_OPENGL_DEBUG_CONTEXT) && defined(GRAPHICS_API_OPENGL_43)
    if ((glDebugMessageCallback != NULL) && (glDebugMessageControl != NULL))
    {
        glDebugMessageCallback(rlDebugMessageCallback, 0);
        // glDebugMessageControl(GL_DEBUG_SOURCE_API, GL_DEBUG_TYPE_ERROR, GL_DEBUG_SEVERITY_HIGH, 0, 0, GL_TRUE);

        // Debug context options:
        //  - GL_DEBUG_OUTPUT - Faster version but not useful for breakpoints
        //  - GL_DEBUG_OUTPUT_SYNCHRONUS - Callback is in sync with errors, so a breakpoint can be placed on the callback in order to get a stacktrace for the GL error
        glEnable(GL_DEBUG_OUTPUT);
        glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
    }
#endif

#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    // Init default white texture
    unsigned char pixels[4] = { 255, 255, 255, 255 };   // 1 pixel RGBA (4 bytes)
    RLGL.State.defaultTextureId = rlLoadTexture(pixels, 1, 1, RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8A8, 1);

    if (RLGL.State.defaultTextureId != 0) TRACELOG(RL_LOG_INFO, "TEXTURE: [ID %i] Default texture loaded successfully", RLGL.State.defaultTextureId);
    else TRACELOG(RL_LOG_WARNING, "TEXTURE: Failed to load default texture");

    // Init default Shader (customized for GL 3.3 and ES2)
    // Loaded: RLGL.State.defaultShaderId + RLGL.State.defaultShaderLocs
    rlLoadShaderDefault();
    RLGL.State.currentShaderId = RLGL.State.defaultShaderId;
    RLGL.State.currentShaderLocs = RLGL.State.defaultShaderLocs;

    // Init default vertex arrays buffers
    // Simulate that the default shader has the location RL_SHADER_LOC_VERTEX_NORMAL to bind the normal buffer for the default render batch
    RLGL.State.currentShaderLocs[RL_SHADER_LOC_VERTEX_NORMAL] = RL_DEFAULT_SHADER_ATTRIB_LOCATION_NORMAL;
    RLGL.defaultBatch = rlLoadRenderBatch(RL_DEFAULT_BATCH_BUFFERS, RL_DEFAULT_BATCH_BUFFER_ELEMENTS);
    RLGL.State.currentShaderLocs[RL_SHADER_LOC_VERTEX_NORMAL] = -1;
    RLGL.currentBatch = &RLGL.defaultBatch;

    // Init stack matrices (emulating OpenGL 1.1)
    for (int i = 0; i < RL_MAX_MATRIX_STACK_SIZE; i++) RLGL.State.stack[i] = rlMatrixIdentity();

    // Init internal matrices
    RLGL.State.transform = rlMatrixIdentity();
    RLGL.State.projection = rlMatrixIdentity();
    RLGL.State.modelview = rlMatrixIdentity();
    RLGL.State.currentMatrix = &RLGL.State.modelview;
#endif  // GRAPHICS_API_OPENGL_33 || GRAPHICS_API_OPENGL_ES2

    // Initialize OpenGL default states
    //----------------------------------------------------------
    // Init state: Depth test
    glDepthFunc(GL_LEQUAL);                                 // Type of depth testing to apply
    glDisable(GL_DEPTH_TEST);                               // Disable depth testing for 2D (only used for 3D)

    // Init state: Blending mode
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);      // Color blending function (how colors are mixed)
    glEnable(GL_BLEND);                                     // Enable color blending (required to work with transparencies)

    // Init state: Culling
    // NOTE: All shapes/models triangles are drawn CCW
    glCullFace(GL_BACK);                                    // Cull the back face (default)
    glFrontFace(GL_CCW);                                    // Front face are defined counter clockwise (default)
    glEnable(GL_CULL_FACE);                                 // Enable backface culling

    // Init state: Cubemap seamless
#if defined(GRAPHICS_API_OPENGL_33)
    glEnable(GL_TEXTURE_CUBE_MAP_SEAMLESS);                 // Seamless cubemaps (not supported on OpenGL ES 2.0)
#endif

#if defined(GRAPHICS_API_OPENGL_11)
    // Init state: Color hints (deprecated in OpenGL 3.0+)
    glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);      // Improve quality of color and texture coordinate interpolation
    glShadeModel(GL_SMOOTH);                                // Smooth shading between vertex (vertex colors interpolation)
#endif

#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    // Store screen size into global variables
    RLGL.State.framebufferWidth = width;
    RLGL.State.framebufferHeight = height;

    TRACELOG(RL_LOG_INFO, "RLGL: Default OpenGL state initialized successfully");
    //----------------------------------------------------------
#endif

    // Init state: Color/Depth buffers clear
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);                   // Set clear color (black)
    glClearDepth(1.0f);                                     // Set clear depth value (default)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);     // Clear color and depth buffers (depth buffer required for 3D)
}

// Vertex Buffer Object deinitialization (memory free)
void rlglClose(void)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    rlUnloadRenderBatch(RLGL.defaultBatch);

    rlUnloadShaderDefault();          // Unload default shader

    glDeleteTextures(1, &RLGL.State.defaultTextureId); // Unload default texture
    TRACELOG(RL_LOG_INFO, "TEXTURE: [ID %i] Default texture unloaded successfully", RLGL.State.defaultTextureId);
#endif
}

// Load OpenGL extensions
// NOTE: External loader function must be provided
void rlLoadExtensions(void *loader)
{
#if defined(GRAPHICS_API_OPENGL_33)     // Also defined for GRAPHICS_API_OPENGL_21
    // NOTE: glad is generated and contains only required OpenGL 3.3 Core extensions (and lower versions)
    if (gladLoadGL((GLADloadfunc)loader) == 0) TRACELOG(RL_LOG_WARNING, "GLAD: Cannot load OpenGL extensions");
    else TRACELOG(RL_LOG_INFO, "GLAD: OpenGL extensions loaded successfully");

    // Get number of supported extensions
    GLint numExt = 0;
    glGetIntegerv(GL_NUM_EXTENSIONS, &numExt);
    TRACELOG(RL_LOG_INFO, "GL: Supported extensions count: %i", numExt);

#if defined(RLGL_SHOW_GL_DETAILS_INFO)
    // Get supported extensions list
    // WARNING: glGetStringi() not available on OpenGL 2.1
    TRACELOG(RL_LOG_INFO, "GL: OpenGL extensions:");
    for (int i = 0; i < numExt; i++) TRACELOG(RL_LOG_INFO, "    %s", glGetStringi(GL_EXTENSIONS, i));
#endif

#if defined(GRAPHICS_API_OPENGL_21)
    // Register supported extensions flags
    // Optional OpenGL 2.1 extensions
    RLGL.ExtSupported.vao = GLAD_GL_ARB_vertex_array_object;
    RLGL.ExtSupported.instancing = (GLAD_GL_EXT_draw_instanced && GLAD_GL_ARB_instanced_arrays);
    RLGL.ExtSupported.texNPOT = GLAD_GL_ARB_texture_non_power_of_two;
    RLGL.ExtSupported.texFloat32 = GLAD_GL_ARB_texture_float;
    RLGL.ExtSupported.texFloat16 = GLAD_GL_ARB_texture_float;
    RLGL.ExtSupported.texDepth = GLAD_GL_ARB_depth_texture;
    RLGL.ExtSupported.maxDepthBits = 32;
    RLGL.ExtSupported.texAnisoFilter = GLAD_GL_EXT_texture_filter_anisotropic;
    RLGL.ExtSupported.texMirrorClamp = GLAD_GL_EXT_texture_mirror_clamp;
#else
    // Register supported extensions flags
    // OpenGL 3.3 extensions supported by default (core)
    RLGL.ExtSupported.vao = true;
    RLGL.ExtSupported.instancing = true;
    RLGL.ExtSupported.texNPOT = true;
    RLGL.ExtSupported.texFloat32 = true;
    RLGL.ExtSupported.texFloat16 = true;
    RLGL.ExtSupported.texDepth = true;
    RLGL.ExtSupported.maxDepthBits = 32;
    RLGL.ExtSupported.texAnisoFilter = true;
    RLGL.ExtSupported.texMirrorClamp = true;
#endif

    // Optional OpenGL 3.3 extensions
    RLGL.ExtSupported.texCompASTC = GLAD_GL_KHR_texture_compression_astc_hdr && GLAD_GL_KHR_texture_compression_astc_ldr;
    RLGL.ExtSupported.texCompDXT = GLAD_GL_EXT_texture_compression_s3tc;  // Texture compression: DXT
    RLGL.ExtSupported.texCompETC2 = GLAD_GL_ARB_ES3_compatibility;        // Texture compression: ETC2/EAC
    #if defined(GRAPHICS_API_OPENGL_43)
    RLGL.ExtSupported.computeShader = GLAD_GL_ARB_compute_shader;
    RLGL.ExtSupported.ssbo = GLAD_GL_ARB_shader_storage_buffer_object;
    #endif

#endif  // GRAPHICS_API_OPENGL_33

#if defined(GRAPHICS_API_OPENGL_ES3)
    // Register supported extensions flags
    // OpenGL ES 3.0 extensions supported by default (or it should be)
    RLGL.ExtSupported.vao = true;
    RLGL.ExtSupported.instancing = true;
    RLGL.ExtSupported.texNPOT = true;
    RLGL.ExtSupported.texFloat32 = true;
    RLGL.ExtSupported.texFloat16 = true;
    RLGL.ExtSupported.texDepth = true;
    RLGL.ExtSupported.texDepthWebGL = true;
    RLGL.ExtSupported.maxDepthBits = 24;
    RLGL.ExtSupported.texAnisoFilter = true;
    RLGL.ExtSupported.texMirrorClamp = true;
    // TODO: Check for additional OpenGL ES 3.0 supported extensions:
    //RLGL.ExtSupported.texCompDXT = true;
    //RLGL.ExtSupported.texCompETC1 = true;
    //RLGL.ExtSupported.texCompETC2 = true;
    //RLGL.ExtSupported.texCompPVRT = true;
    //RLGL.ExtSupported.texCompASTC = true;
    //RLGL.ExtSupported.maxAnisotropyLevel = true;
    //RLGL.ExtSupported.computeShader = true;
    //RLGL.ExtSupported.ssbo = true;

#elif defined(GRAPHICS_API_OPENGL_ES2)

    #if defined(PLATFORM_DESKTOP_GLFW) || defined(PLATFORM_DESKTOP_SDL)
    // TODO: Support GLAD loader for OpenGL ES 3.0
    if (gladLoadGLES2((GLADloadfunc)loader) == 0) TRACELOG(RL_LOG_WARNING, "GLAD: Cannot load OpenGL ES2.0 functions");
    else TRACELOG(RL_LOG_INFO, "GLAD: OpenGL ES 2.0 loaded successfully");
    #endif

    // Get supported extensions list
    GLint numExt = 0;
    const char **extList = RL_MALLOC(512*sizeof(const char *)); // Allocate 512 strings pointers (2 KB)
    const char *extensions = (const char *)glGetString(GL_EXTENSIONS);  // One big const string

    // NOTE: We have to duplicate string because glGetString() returns a const string
    int size = strlen(extensions) + 1;      // Get extensions string size in bytes
    char *extensionsDup = (char *)RL_CALLOC(size, sizeof(char));
    strcpy(extensionsDup, extensions);
    extList[numExt] = extensionsDup;

    for (int i = 0; i < size; i++)
    {
        if (extensionsDup[i] == ' ')
        {
            extensionsDup[i] = '\0';
            numExt++;
            extList[numExt] = &extensionsDup[i + 1];
        }
    }

    TRACELOG(RL_LOG_INFO, "GL: Supported extensions count: %i", numExt);

#if defined(RLGL_SHOW_GL_DETAILS_INFO)
    TRACELOG(RL_LOG_INFO, "GL: OpenGL extensions:");
    for (int i = 0; i < numExt; i++) TRACELOG(RL_LOG_INFO, "    %s", extList[i]);
#endif

    // Check required extensions
    for (int i = 0; i < numExt; i++)
    {
        // Check VAO support
        // NOTE: Only check on OpenGL ES, OpenGL 3.3 has VAO support as core feature
        if (strcmp(extList[i], (const char *)"GL_OES_vertex_array_object") == 0)
        {
            // The extension is supported by our hardware and driver, try to get related functions pointers
            // NOTE: emscripten does not support VAOs natively, it uses emulation and it reduces overall performance...
            glGenVertexArrays = (PFNGLGENVERTEXARRAYSOESPROC)((rlglLoadProc)loader)("glGenVertexArraysOES");
            glBindVertexArray = (PFNGLBINDVERTEXARRAYOESPROC)((rlglLoadProc)loader)("glBindVertexArrayOES");
            glDeleteVertexArrays = (PFNGLDELETEVERTEXARRAYSOESPROC)((rlglLoadProc)loader)("glDeleteVertexArraysOES");
            //glIsVertexArray = (PFNGLISVERTEXARRAYOESPROC)loader("glIsVertexArrayOES");     // NOTE: Fails in WebGL, omitted

            if ((glGenVertexArrays != NULL) && (glBindVertexArray != NULL) && (glDeleteVertexArrays != NULL)) RLGL.ExtSupported.vao = true;
        }

        // Check instanced rendering support
        if (strstr(extList[i], (const char*)"instanced_arrays") != NULL)   // Broad check for instanced_arrays
        {
            // Specific check
            if (strcmp(extList[i], (const char *)"GL_ANGLE_instanced_arrays") == 0)      // ANGLE
            {
                glDrawArraysInstanced = (PFNGLDRAWARRAYSINSTANCEDEXTPROC)((rlglLoadProc)loader)("glDrawArraysInstancedANGLE");
                glDrawElementsInstanced = (PFNGLDRAWELEMENTSINSTANCEDEXTPROC)((rlglLoadProc)loader)("glDrawElementsInstancedANGLE");
                glVertexAttribDivisor = (PFNGLVERTEXATTRIBDIVISOREXTPROC)((rlglLoadProc)loader)("glVertexAttribDivisorANGLE");
            }
            else if (strcmp(extList[i], (const char *)"GL_EXT_instanced_arrays") == 0)   // EXT
            {
                glDrawArraysInstanced = (PFNGLDRAWARRAYSINSTANCEDEXTPROC)((rlglLoadProc)loader)("glDrawArraysInstancedEXT");
                glDrawElementsInstanced = (PFNGLDRAWELEMENTSINSTANCEDEXTPROC)((rlglLoadProc)loader)("glDrawElementsInstancedEXT");
                glVertexAttribDivisor = (PFNGLVERTEXATTRIBDIVISOREXTPROC)((rlglLoadProc)loader)("glVertexAttribDivisorEXT");
            }
            else if (strcmp(extList[i], (const char *)"GL_NV_instanced_arrays") == 0)    // NVIDIA GLES
            {
                glDrawArraysInstanced = (PFNGLDRAWARRAYSINSTANCEDEXTPROC)((rlglLoadProc)loader)("glDrawArraysInstancedNV");
                glDrawElementsInstanced = (PFNGLDRAWELEMENTSINSTANCEDEXTPROC)((rlglLoadProc)loader)("glDrawElementsInstancedNV");
                glVertexAttribDivisor = (PFNGLVERTEXATTRIBDIVISOREXTPROC)((rlglLoadProc)loader)("glVertexAttribDivisorNV");
            }

            // The feature will only be marked as supported if the elements from GL_XXX_instanced_arrays are present
            if ((glDrawArraysInstanced != NULL) && (glDrawElementsInstanced != NULL) && (glVertexAttribDivisor != NULL)) RLGL.ExtSupported.instancing = true;
        }
        else if (strstr(extList[i], (const char *)"draw_instanced") != NULL)
        {
            // GL_ANGLE_draw_instanced doesn't exist
            if (strcmp(extList[i], (const char *)"GL_EXT_draw_instanced") == 0)
            {
                glDrawArraysInstanced = (PFNGLDRAWARRAYSINSTANCEDEXTPROC)((rlglLoadProc)loader)("glDrawArraysInstancedEXT");
                glDrawElementsInstanced = (PFNGLDRAWELEMENTSINSTANCEDEXTPROC)((rlglLoadProc)loader)("glDrawElementsInstancedEXT");
            }
            else if (strcmp(extList[i], (const char*)"GL_NV_draw_instanced") == 0)
            {
                glDrawArraysInstanced = (PFNGLDRAWARRAYSINSTANCEDEXTPROC)((rlglLoadProc)loader)("glDrawArraysInstancedNV");
                glDrawElementsInstanced = (PFNGLDRAWELEMENTSINSTANCEDEXTPROC)((rlglLoadProc)loader)("glDrawElementsInstancedNV");
            }

            // But the functions will at least be loaded if only GL_XX_EXT_draw_instanced exist
            if ((glDrawArraysInstanced != NULL) && (glDrawElementsInstanced != NULL) && (glVertexAttribDivisor != NULL)) RLGL.ExtSupported.instancing = true;
        }

        // Check NPOT textures support
        // NOTE: Only check on OpenGL ES, OpenGL 3.3 has NPOT textures full support as core feature
        if (strcmp(extList[i], (const char *)"GL_OES_texture_npot") == 0) RLGL.ExtSupported.texNPOT = true;

        // Check texture float support
        if (strcmp(extList[i], (const char *)"GL_OES_texture_float") == 0) RLGL.ExtSupported.texFloat32 = true;
        if (strcmp(extList[i], (const char *)"GL_OES_texture_half_float") == 0) RLGL.ExtSupported.texFloat16 = true;

        // Check depth texture support
        if (strcmp(extList[i], (const char *)"GL_OES_depth_texture") == 0) RLGL.ExtSupported.texDepth = true;
        if (strcmp(extList[i], (const char *)"GL_WEBGL_depth_texture") == 0) RLGL.ExtSupported.texDepthWebGL = true;    // WebGL requires unsized internal format
        if (RLGL.ExtSupported.texDepthWebGL) RLGL.ExtSupported.texDepth = true;

        if (strcmp(extList[i], (const char *)"GL_OES_depth24") == 0) RLGL.ExtSupported.maxDepthBits = 24;   // Not available on WebGL
        if (strcmp(extList[i], (const char *)"GL_OES_depth32") == 0) RLGL.ExtSupported.maxDepthBits = 32;   // Not available on WebGL

        // Check texture compression support: DXT
        if ((strcmp(extList[i], (const char *)"GL_EXT_texture_compression_s3tc") == 0) ||
            (strcmp(extList[i], (const char *)"GL_WEBGL_compressed_texture_s3tc") == 0) ||
            (strcmp(extList[i], (const char *)"GL_WEBKIT_WEBGL_compressed_texture_s3tc") == 0)) RLGL.ExtSupported.texCompDXT = true;

        // Check texture compression support: ETC1
        if ((strcmp(extList[i], (const char *)"GL_OES_compressed_ETC1_RGB8_texture") == 0) ||
            (strcmp(extList[i], (const char *)"GL_WEBGL_compressed_texture_etc1") == 0)) RLGL.ExtSupported.texCompETC1 = true;

        // Check texture compression support: ETC2/EAC
        if (strcmp(extList[i], (const char *)"GL_ARB_ES3_compatibility") == 0) RLGL.ExtSupported.texCompETC2 = true;

        // Check texture compression support: PVR
        if (strcmp(extList[i], (const char *)"GL_IMG_texture_compression_pvrtc") == 0) RLGL.ExtSupported.texCompPVRT = true;

        // Check texture compression support: ASTC
        if (strcmp(extList[i], (const char *)"GL_KHR_texture_compression_astc_hdr") == 0) RLGL.ExtSupported.texCompASTC = true;

        // Check anisotropic texture filter support
        if (strcmp(extList[i], (const char *)"GL_EXT_texture_filter_anisotropic") == 0) RLGL.ExtSupported.texAnisoFilter = true;

        // Check clamp mirror wrap mode support
        if (strcmp(extList[i], (const char *)"GL_EXT_texture_mirror_clamp") == 0) RLGL.ExtSupported.texMirrorClamp = true;
    }

    // Free extensions pointers
    RL_FREE(extList);
    RL_FREE(extensionsDup);    // Duplicated string must be deallocated
#endif  // GRAPHICS_API_OPENGL_ES2

    // Check OpenGL information and capabilities
    //------------------------------------------------------------------------------
    // Show current OpenGL and GLSL version
    TRACELOG(RL_LOG_INFO, "GL: OpenGL device information:");
    TRACELOG(RL_LOG_INFO, "    > Vendor:   %s", glGetString(GL_VENDOR));
    TRACELOG(RL_LOG_INFO, "    > Renderer: %s", glGetString(GL_RENDERER));
    TRACELOG(RL_LOG_INFO, "    > Version:  %s", glGetString(GL_VERSION));
    TRACELOG(RL_LOG_INFO, "    > GLSL:     %s", glGetString(GL_SHADING_LANGUAGE_VERSION));

#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    // NOTE: Anisotropy levels capability is an extension
    #ifndef GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT
        #define GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT 0x84FF
    #endif
    glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, &RLGL.ExtSupported.maxAnisotropyLevel);

#if defined(RLGL_SHOW_GL_DETAILS_INFO)
    // Show some OpenGL GPU capabilities
    TRACELOG(RL_LOG_INFO, "GL: OpenGL capabilities:");
    GLint capability = 0;
    glGetIntegerv(GL_MAX_TEXTURE_SIZE, &capability);
    TRACELOG(RL_LOG_INFO, "    GL_MAX_TEXTURE_SIZE: %i", capability);
    glGetIntegerv(GL_MAX_CUBE_MAP_TEXTURE_SIZE, &capability);
    TRACELOG(RL_LOG_INFO, "    GL_MAX_CUBE_MAP_TEXTURE_SIZE: %i", capability);
    glGetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS, &capability);
    TRACELOG(RL_LOG_INFO, "    GL_MAX_TEXTURE_IMAGE_UNITS: %i", capability);
    glGetIntegerv(GL_MAX_VERTEX_ATTRIBS, &capability);
    TRACELOG(RL_LOG_INFO, "    GL_MAX_VERTEX_ATTRIBS: %i", capability);
    #if !defined(GRAPHICS_API_OPENGL_ES2)
    glGetIntegerv(GL_MAX_UNIFORM_BLOCK_SIZE, &capability);
    TRACELOG(RL_LOG_INFO, "    GL_MAX_UNIFORM_BLOCK_SIZE: %i", capability);
    glGetIntegerv(GL_MAX_DRAW_BUFFERS, &capability);
    TRACELOG(RL_LOG_INFO, "    GL_MAX_DRAW_BUFFERS: %i", capability);
    if (RLGL.ExtSupported.texAnisoFilter) TRACELOG(RL_LOG_INFO, "    GL_MAX_TEXTURE_MAX_ANISOTROPY: %.0f", RLGL.ExtSupported.maxAnisotropyLevel);
    #endif
    glGetIntegerv(GL_NUM_COMPRESSED_TEXTURE_FORMATS, &capability);
    TRACELOG(RL_LOG_INFO, "    GL_NUM_COMPRESSED_TEXTURE_FORMATS: %i", capability);
    GLint *compFormats = (GLint *)RL_CALLOC(capability, sizeof(GLint));
    glGetIntegerv(GL_COMPRESSED_TEXTURE_FORMATS, compFormats);
    for (int i = 0; i < capability; i++) TRACELOG(RL_LOG_INFO, "        %s", rlGetCompressedFormatName(compFormats[i]));
    RL_FREE(compFormats);

#if defined(GRAPHICS_API_OPENGL_43)
    glGetIntegerv(GL_MAX_VERTEX_ATTRIB_BINDINGS, &capability);
    TRACELOG(RL_LOG_INFO, "    GL_MAX_VERTEX_ATTRIB_BINDINGS: %i", capability);
    glGetIntegerv(GL_MAX_UNIFORM_LOCATIONS, &capability);
    TRACELOG(RL_LOG_INFO, "    GL_MAX_UNIFORM_LOCATIONS: %i", capability);
#endif  // GRAPHICS_API_OPENGL_43
#else   // RLGL_SHOW_GL_DETAILS_INFO

    // Show some basic info about GL supported features
    if (RLGL.ExtSupported.vao) TRACELOG(RL_LOG_INFO, "GL: VAO extension detected, VAO functions loaded successfully");
    else TRACELOG(RL_LOG_WARNING, "GL: VAO extension not found, VAO not supported");
    if (RLGL.ExtSupported.texNPOT) TRACELOG(RL_LOG_INFO, "GL: NPOT textures extension detected, full NPOT textures supported");
    else TRACELOG(RL_LOG_WARNING, "GL: NPOT textures extension not found, limited NPOT support (no-mipmaps, no-repeat)");
    if (RLGL.ExtSupported.texCompDXT) TRACELOG(RL_LOG_INFO, "GL: DXT compressed textures supported");
    if (RLGL.ExtSupported.texCompETC1) TRACELOG(RL_LOG_INFO, "GL: ETC1 compressed textures supported");
    if (RLGL.ExtSupported.texCompETC2) TRACELOG(RL_LOG_INFO, "GL: ETC2/EAC compressed textures supported");
    if (RLGL.ExtSupported.texCompPVRT) TRACELOG(RL_LOG_INFO, "GL: PVRT compressed textures supported");
    if (RLGL.ExtSupported.texCompASTC) TRACELOG(RL_LOG_INFO, "GL: ASTC compressed textures supported");
    if (RLGL.ExtSupported.computeShader) TRACELOG(RL_LOG_INFO, "GL: Compute shaders supported");
    if (RLGL.ExtSupported.ssbo) TRACELOG(RL_LOG_INFO, "GL: Shader storage buffer objects supported");
#endif  // RLGL_SHOW_GL_DETAILS_INFO

#endif  // GRAPHICS_API_OPENGL_33 || GRAPHICS_API_OPENGL_ES2
}

// Get current OpenGL version
int rlGetVersion(void)
{
    int glVersion = 0;
#if defined(GRAPHICS_API_OPENGL_11)
    glVersion = RL_OPENGL_11;
#endif
#if defined(GRAPHICS_API_OPENGL_21)
    glVersion = RL_OPENGL_21;
#elif defined(GRAPHICS_API_OPENGL_43)
    glVersion = RL_OPENGL_43;
#elif defined(GRAPHICS_API_OPENGL_33)
    glVersion = RL_OPENGL_33;
#endif
#if defined(GRAPHICS_API_OPENGL_ES3)
    glVersion = RL_OPENGL_ES_30;
#elif defined(GRAPHICS_API_OPENGL_ES2)
    glVersion = RL_OPENGL_ES_20;
#endif

    return glVersion;
}

// Set current framebuffer width
void rlSetFramebufferWidth(int width)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    RLGL.State.framebufferWidth = width;
#endif
}

// Set current framebuffer height
void rlSetFramebufferHeight(int height)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    RLGL.State.framebufferHeight = height;
#endif
}

// Get default framebuffer width
int rlGetFramebufferWidth(void)
{
    int width = 0;
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    width = RLGL.State.framebufferWidth;
#endif
    return width;
}

// Get default framebuffer height
int rlGetFramebufferHeight(void)
{
    int height = 0;
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    height = RLGL.State.framebufferHeight;
#endif
    return height;
}

// Get default internal texture (white texture)
// NOTE: Default texture is a 1x1 pixel UNCOMPRESSED_R8G8B8A8
unsigned int rlGetTextureIdDefault(void)
{
    unsigned int id = 0;
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    id = RLGL.State.defaultTextureId;
#endif
    return id;
}

// Get default shader id
unsigned int rlGetShaderIdDefault(void)
{
    unsigned int id = 0;
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    id = RLGL.State.defaultShaderId;
#endif
    return id;
}

// Get default shader locs
int *rlGetShaderLocsDefault(void)
{
    int *locs = NULL;
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    locs = RLGL.State.defaultShaderLocs;
#endif
    return locs;
}

// Render batch management
//------------------------------------------------------------------------------------------------
// Load render batch
rlRenderBatch rlLoadRenderBatch(int numBuffers, int bufferElements)
{
    rlRenderBatch batch = { 0 };

#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    // Initialize CPU (RAM) vertex buffers (position, texcoord, color data and indexes)
    //--------------------------------------------------------------------------------------------
    batch.vertexBuffer = (rlVertexBuffer *)RL_MALLOC(numBuffers*sizeof(rlVertexBuffer));

    for (int i = 0; i < numBuffers; i++)
    {
        batch.vertexBuffer[i].elementCount = bufferElements;

        batch.vertexBuffer[i].vertices = (float *)RL_MALLOC(bufferElements*3*4*sizeof(float));        // 3 float by vertex, 4 vertex by quad
        batch.vertexBuffer[i].texcoords = (float *)RL_MALLOC(bufferElements*2*4*sizeof(float));       // 2 float by texcoord, 4 texcoord by quad
        batch.vertexBuffer[i].normals = (float *)RL_MALLOC(bufferElements*3*4*sizeof(float));        // 3 float by vertex, 4 vertex by quad
        batch.vertexBuffer[i].colors = (unsigned char *)RL_MALLOC(bufferElements*4*4*sizeof(unsigned char));   // 4 float by color, 4 colors by quad
#if defined(GRAPHICS_API_OPENGL_33)
        batch.vertexBuffer[i].indices = (unsigned int *)RL_MALLOC(bufferElements*6*sizeof(unsigned int));      // 6 int by quad (indices)
#endif
#if defined(GRAPHICS_API_OPENGL_ES2)
        batch.vertexBuffer[i].indices = (unsigned short *)RL_MALLOC(bufferElements*6*sizeof(unsigned short));  // 6 int by quad (indices)
#endif

        for (int j = 0; j < (3*4*bufferElements); j++) batch.vertexBuffer[i].vertices[j] = 0.0f;
        for (int j = 0; j < (2*4*bufferElements); j++) batch.vertexBuffer[i].texcoords[j] = 0.0f;
        for (int j = 0; j < (3*4*bufferElements); j++) batch.vertexBuffer[i].normals[j] = 0.0f;
        for (int j = 0; j < (4*4*bufferElements); j++) batch.vertexBuffer[i].colors[j] = 0;

        int k = 0;

        // Indices can be initialized right now
        for (int j = 0; j < (6*bufferElements); j += 6)
        {
            batch.vertexBuffer[i].indices[j] = 4*k;
            batch.vertexBuffer[i].indices[j + 1] = 4*k + 1;
            batch.vertexBuffer[i].indices[j + 2] = 4*k + 2;
            batch.vertexBuffer[i].indices[j + 3] = 4*k;
            batch.vertexBuffer[i].indices[j + 4] = 4*k + 2;
            batch.vertexBuffer[i].indices[j + 5] = 4*k + 3;

            k++;
        }

        RLGL.State.vertexCounter = 0;
    }

    TRACELOG(RL_LOG_INFO, "RLGL: Render batch vertex buffers loaded successfully in RAM (CPU)");
    //--------------------------------------------------------------------------------------------

    // Upload to GPU (VRAM) vertex data and initialize VAOs/VBOs
    //--------------------------------------------------------------------------------------------
    for (int i = 0; i < numBuffers; i++)
    {
        if (RLGL.ExtSupported.vao)
        {
            // Initialize Quads VAO
            glGenVertexArrays(1, &batch.vertexBuffer[i].vaoId);
            glBindVertexArray(batch.vertexBuffer[i].vaoId);
        }

        // Quads - Vertex buffers binding and attributes enable
        // Vertex position buffer (shader-location = 0)
        glGenBuffers(1, &batch.vertexBuffer[i].vboId[0]);
        glBindBuffer(GL_ARRAY_BUFFER, batch.vertexBuffer[i].vboId[0]);
        glBufferData(GL_ARRAY_BUFFER, bufferElements*3*4*sizeof(float), batch.vertexBuffer[i].vertices, GL_DYNAMIC_DRAW);
        glEnableVertexAttribArray(RLGL.State.currentShaderLocs[RL_SHADER_LOC_VERTEX_POSITION]);
        glVertexAttribPointer(RLGL.State.currentShaderLocs[RL_SHADER_LOC_VERTEX_POSITION], 3, GL_FLOAT, 0, 0, 0);

        // Vertex texcoord buffer (shader-location = 1)
        glGenBuffers(1, &batch.vertexBuffer[i].vboId[1]);
        glBindBuffer(GL_ARRAY_BUFFER, batch.vertexBuffer[i].vboId[1]);
        glBufferData(GL_ARRAY_BUFFER, bufferElements*2*4*sizeof(float), batch.vertexBuffer[i].texcoords, GL_DYNAMIC_DRAW);
        glEnableVertexAttribArray(RLGL.State.currentShaderLocs[RL_SHADER_LOC_VERTEX_TEXCOORD01]);
        glVertexAttribPointer(RLGL.State.currentShaderLocs[RL_SHADER_LOC_VERTEX_TEXCOORD01], 2, GL_FLOAT, 0, 0, 0);

        // Vertex normal buffer (shader-location = 2)
        glGenBuffers(1, &batch.vertexBuffer[i].vboId[2]);
        glBindBuffer(GL_ARRAY_BUFFER, batch.vertexBuffer[i].vboId[2]);
        glBufferData(GL_ARRAY_BUFFER, bufferElements*3*4*sizeof(float), batch.vertexBuffer[i].normals, GL_DYNAMIC_DRAW);
        glEnableVertexAttribArray(RLGL.State.currentShaderLocs[RL_SHADER_LOC_VERTEX_NORMAL]);
        glVertexAttribPointer(RLGL.State.currentShaderLocs[RL_SHADER_LOC_VERTEX_NORMAL], 3, GL_FLOAT, 0, 0, 0);

        // Vertex color buffer (shader-location = 3)
        glGenBuffers(1, &batch.vertexBuffer[i].vboId[3]);
        glBindBuffer(GL_ARRAY_BUFFER, batch.vertexBuffer[i].vboId[3]);
        glBufferData(GL_ARRAY_BUFFER, bufferElements*4*4*sizeof(unsigned char), batch.vertexBuffer[i].colors, GL_DYNAMIC_DRAW);
        glEnableVertexAttribArray(RLGL.State.currentShaderLocs[RL_SHADER_LOC_VERTEX_COLOR]);
        glVertexAttribPointer(RLGL.State.currentShaderLocs[RL_SHADER_LOC_VERTEX_COLOR], 4, GL_UNSIGNED_BYTE, GL_TRUE, 0, 0);

        // Fill index buffer
        glGenBuffers(1, &batch.vertexBuffer[i].vboId[4]);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, batch.vertexBuffer[i].vboId[4]);
#if defined(GRAPHICS_API_OPENGL_33)
        glBufferData(GL_ELEMENT_ARRAY_BUFFER, bufferElements*6*sizeof(int), batch.vertexBuffer[i].indices, GL_STATIC_DRAW);
#endif
#if defined(GRAPHICS_API_OPENGL_ES2)
        glBufferData(GL_ELEMENT_ARRAY_BUFFER, bufferElements*6*sizeof(short), batch.vertexBuffer[i].indices, GL_STATIC_DRAW);
#endif
    }

    TRACELOG(RL_LOG_INFO, "RLGL: Render batch vertex buffers loaded successfully in VRAM (GPU)");

    // Unbind the current VAO
    if (RLGL.ExtSupported.vao) glBindVertexArray(0);
    //--------------------------------------------------------------------------------------------

    // Init draw calls tracking system
    //--------------------------------------------------------------------------------------------
    batch.draws = (rlDrawCall *)RL_MALLOC(RL_DEFAULT_BATCH_DRAWCALLS*sizeof(rlDrawCall));

    for (int i = 0; i < RL_DEFAULT_BATCH_DRAWCALLS; i++)
    {
        batch.draws[i].mode = RL_QUADS;
        batch.draws[i].vertexCount = 0;
        batch.draws[i].vertexAlignment = 0;
        //batch.draws[i].vaoId = 0;
        //batch.draws[i].shaderId = 0;
        batch.draws[i].textureId = RLGL.State.defaultTextureId;
        //batch.draws[i].RLGL.State.projection = rlMatrixIdentity();
        //batch.draws[i].RLGL.State.modelview = rlMatrixIdentity();
    }

    batch.bufferCount = numBuffers;    // Record buffer count
    batch.drawCounter = 1;             // Reset draws counter
    batch.currentDepth = -1.0f;         // Reset depth value
    //--------------------------------------------------------------------------------------------
#endif

    return batch;
}

// Unload default internal buffers vertex data from CPU and GPU
void rlUnloadRenderBatch(rlRenderBatch batch)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    // Unbind everything
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

    // Unload all vertex buffers data
    for (int i = 0; i < batch.bufferCount; i++)
    {
        // Unbind VAO attribs data
        if (RLGL.ExtSupported.vao)
        {
            glBindVertexArray(batch.vertexBuffer[i].vaoId);
            glDisableVertexAttribArray(RL_DEFAULT_SHADER_ATTRIB_LOCATION_POSITION);
            glDisableVertexAttribArray(RL_DEFAULT_SHADER_ATTRIB_LOCATION_TEXCOORD);
            glDisableVertexAttribArray(RL_DEFAULT_SHADER_ATTRIB_LOCATION_NORMAL);
            glDisableVertexAttribArray(RL_DEFAULT_SHADER_ATTRIB_LOCATION_COLOR);
            glBindVertexArray(0);
        }

        // Delete VBOs from GPU (VRAM)
        glDeleteBuffers(1, &batch.vertexBuffer[i].vboId[0]);
        glDeleteBuffers(1, &batch.vertexBuffer[i].vboId[1]);
        glDeleteBuffers(1, &batch.vertexBuffer[i].vboId[2]);
        glDeleteBuffers(1, &batch.vertexBuffer[i].vboId[3]);
        glDeleteBuffers(1, &batch.vertexBuffer[i].vboId[4]);

        // Delete VAOs from GPU (VRAM)
        if (RLGL.ExtSupported.vao) glDeleteVertexArrays(1, &batch.vertexBuffer[i].vaoId);

        // Free vertex arrays memory from CPU (RAM)
        RL_FREE(batch.vertexBuffer[i].vertices);
        RL_FREE(batch.vertexBuffer[i].texcoords);
        RL_FREE(batch.vertexBuffer[i].normals);
        RL_FREE(batch.vertexBuffer[i].colors);
        RL_FREE(batch.vertexBuffer[i].indices);
    }

    // Unload arrays
    RL_FREE(batch.vertexBuffer);
    RL_FREE(batch.draws);
#endif
}

// Draw render batch
// NOTE: We require a pointer to reset batch and increase current buffer (multi-buffer)
void rlDrawRenderBatch(rlRenderBatch *batch)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    // Update batch vertex buffers
    //------------------------------------------------------------------------------------------------------------
    // NOTE: If there is not vertex data, buffers doesn't need to be updated (vertexCount > 0)
    // TODO: If no data changed on the CPU arrays --> No need to re-update GPU arrays (use a change detector flag?)
    if (RLGL.State.vertexCounter > 0)
    {
        // Activate elements VAO
        if (RLGL.ExtSupported.vao) glBindVertexArray(batch->vertexBuffer[batch->currentBuffer].vaoId);

        // Vertex positions buffer
        glBindBuffer(GL_ARRAY_BUFFER, batch->vertexBuffer[batch->currentBuffer].vboId[0]);
        glBufferSubData(GL_ARRAY_BUFFER, 0, RLGL.State.vertexCounter*3*sizeof(float), batch->vertexBuffer[batch->currentBuffer].vertices);
        //glBufferData(GL_ARRAY_BUFFER, sizeof(float)*3*4*batch->vertexBuffer[batch->currentBuffer].elementCount, batch->vertexBuffer[batch->currentBuffer].vertices, GL_DYNAMIC_DRAW);  // Update all buffer

        // Texture coordinates buffer
        glBindBuffer(GL_ARRAY_BUFFER, batch->vertexBuffer[batch->currentBuffer].vboId[1]);
        glBufferSubData(GL_ARRAY_BUFFER, 0, RLGL.State.vertexCounter*2*sizeof(float), batch->vertexBuffer[batch->currentBuffer].texcoords);
        //glBufferData(GL_ARRAY_BUFFER, sizeof(float)*2*4*batch->vertexBuffer[batch->currentBuffer].elementCount, batch->vertexBuffer[batch->currentBuffer].texcoords, GL_DYNAMIC_DRAW); // Update all buffer

        // Normals buffer
        glBindBuffer(GL_ARRAY_BUFFER, batch->vertexBuffer[batch->currentBuffer].vboId[2]);
        glBufferSubData(GL_ARRAY_BUFFER, 0, RLGL.State.vertexCounter*3*sizeof(float), batch->vertexBuffer[batch->currentBuffer].normals);
        //glBufferData(GL_ARRAY_BUFFER, sizeof(float)*3*4*batch->vertexBuffer[batch->currentBuffer].elementCount, batch->vertexBuffer[batch->currentBuffer].normals, GL_DYNAMIC_DRAW); // Update all buffer

        // Colors buffer
        glBindBuffer(GL_ARRAY_BUFFER, batch->vertexBuffer[batch->currentBuffer].vboId[3]);
        glBufferSubData(GL_ARRAY_BUFFER, 0, RLGL.State.vertexCounter*4*sizeof(unsigned char), batch->vertexBuffer[batch->currentBuffer].colors);
        //glBufferData(GL_ARRAY_BUFFER, sizeof(float)*4*4*batch->vertexBuffer[batch->currentBuffer].elementCount, batch->vertexBuffer[batch->currentBuffer].colors, GL_DYNAMIC_DRAW);    // Update all buffer

        // NOTE: glMapBuffer() causes sync issue
        // If GPU is working with this buffer, glMapBuffer() will wait(stall) until GPU to finish its job
        // To avoid waiting (idle), you can call first glBufferData() with NULL pointer before glMapBuffer()
        // If you do that, the previous data in PBO will be discarded and glMapBuffer() returns a new
        // allocated pointer immediately even if GPU is still working with the previous data

        // Another option: map the buffer object into client's memory
        // Probably this code could be moved somewhere else...
        // batch->vertexBuffer[batch->currentBuffer].vertices = (float *)glMapBuffer(GL_ARRAY_BUFFER, GL_READ_WRITE);
        // if (batch->vertexBuffer[batch->currentBuffer].vertices)
        // {
            // Update vertex data
        // }
        // glUnmapBuffer(GL_ARRAY_BUFFER);

        // Unbind the current VAO
        if (RLGL.ExtSupported.vao) glBindVertexArray(0);
    }
    //------------------------------------------------------------------------------------------------------------

    // Draw batch vertex buffers (considering VR stereo if required)
    //------------------------------------------------------------------------------------------------------------
    Matrix matProjection = RLGL.State.projection;
    Matrix matModelView = RLGL.State.modelview;

    int eyeCount = 1;
    if (RLGL.State.stereoRender) eyeCount = 2;

    for (int eye = 0; eye < eyeCount; eye++)
    {
        if (eyeCount == 2)
        {
            // Setup current eye viewport (half screen width)
            rlViewport(eye*RLGL.State.framebufferWidth/2, 0, RLGL.State.framebufferWidth/2, RLGL.State.framebufferHeight);

            // Set current eye view offset to modelview matrix
            rlSetMatrixModelview(rlMatrixMultiply(matModelView, RLGL.State.viewOffsetStereo[eye]));
            // Set current eye projection matrix
            rlSetMatrixProjection(RLGL.State.projectionStereo[eye]);
        }

        // Draw buffers
        if (RLGL.State.vertexCounter > 0)
        {
            // Set current shader and upload current MVP matrix
            glUseProgram(RLGL.State.currentShaderId);

            // Create modelview-projection matrix and upload to shader
            Matrix matMVP = rlMatrixMultiply(RLGL.State.modelview, RLGL.State.projection);
            glUniformMatrix4fv(RLGL.State.currentShaderLocs[RL_SHADER_LOC_MATRIX_MVP], 1, false, rlMatrixToFloat(matMVP));

            if (RLGL.State.currentShaderLocs[RL_SHADER_LOC_MATRIX_PROJECTION] != -1)
            {
                glUniformMatrix4fv(RLGL.State.currentShaderLocs[RL_SHADER_LOC_MATRIX_PROJECTION], 1, false, rlMatrixToFloat(RLGL.State.projection));
            }

            // WARNING: For the following setup of the view, model, and normal matrices, it is expected that
            // transformations and rendering occur between rlPushMatrix() and rlPopMatrix()

            if (RLGL.State.currentShaderLocs[RL_SHADER_LOC_MATRIX_VIEW] != -1)
            {
                glUniformMatrix4fv(RLGL.State.currentShaderLocs[RL_SHADER_LOC_MATRIX_VIEW], 1, false, rlMatrixToFloat(RLGL.State.modelview));
            }

            if (RLGL.State.currentShaderLocs[RL_SHADER_LOC_MATRIX_MODEL] != -1)
            {
                glUniformMatrix4fv(RLGL.State.currentShaderLocs[RL_SHADER_LOC_MATRIX_MODEL], 1, false, rlMatrixToFloat(RLGL.State.transform));
            }

            if (RLGL.State.currentShaderLocs[RL_SHADER_LOC_MATRIX_NORMAL] != -1)
            {
                glUniformMatrix4fv(RLGL.State.currentShaderLocs[RL_SHADER_LOC_MATRIX_NORMAL], 1, false, rlMatrixToFloat(rlMatrixTranspose(rlMatrixInvert(RLGL.State.transform))));
            }

            if (RLGL.ExtSupported.vao) glBindVertexArray(batch->vertexBuffer[batch->currentBuffer].vaoId);
            else
            {
                // Bind vertex attrib: position (shader-location = 0)
                glBindBuffer(GL_ARRAY_BUFFER, batch->vertexBuffer[batch->currentBuffer].vboId[0]);
                glVertexAttribPointer(RLGL.State.currentShaderLocs[RL_SHADER_LOC_VERTEX_POSITION], 3, GL_FLOAT, 0, 0, 0);
                glEnableVertexAttribArray(RLGL.State.currentShaderLocs[RL_SHADER_LOC_VERTEX_POSITION]);

                // Bind vertex attrib: texcoord (shader-location = 1)
                glBindBuffer(GL_ARRAY_BUFFER, batch->vertexBuffer[batch->currentBuffer].vboId[1]);
                glVertexAttribPointer(RLGL.State.currentShaderLocs[RL_SHADER_LOC_VERTEX_TEXCOORD01], 2, GL_FLOAT, 0, 0, 0);
                glEnableVertexAttribArray(RLGL.State.currentShaderLocs[RL_SHADER_LOC_VERTEX_TEXCOORD01]);

                // Bind vertex attrib: normal (shader-location = 2)
                glBindBuffer(GL_ARRAY_BUFFER, batch->vertexBuffer[batch->currentBuffer].vboId[2]);
                glVertexAttribPointer(RLGL.State.currentShaderLocs[RL_SHADER_LOC_VERTEX_NORMAL], 3, GL_FLOAT, 0, 0, 0);
                glEnableVertexAttribArray(RLGL.State.currentShaderLocs[RL_SHADER_LOC_VERTEX_NORMAL]);

                // Bind vertex attrib: color (shader-location = 3)
                glBindBuffer(GL_ARRAY_BUFFER, batch->vertexBuffer[batch->currentBuffer].vboId[3]);
                glVertexAttribPointer(RLGL.State.currentShaderLocs[RL_SHADER_LOC_VERTEX_COLOR], 4, GL_UNSIGNED_BYTE, GL_TRUE, 0, 0);
                glEnableVertexAttribArray(RLGL.State.currentShaderLocs[RL_SHADER_LOC_VERTEX_COLOR]);

                glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, batch->vertexBuffer[batch->currentBuffer].vboId[4]);
            }

            // Setup some default shader values
            glUniform4f(RLGL.State.currentShaderLocs[RL_SHADER_LOC_COLOR_DIFFUSE], 1.0f, 1.0f, 1.0f, 1.0f);
            glUniform1i(RLGL.State.currentShaderLocs[RL_SHADER_LOC_MAP_DIFFUSE], 0);  // Active default sampler2D: texture0

            // Activate additional sampler textures
            // Those additional textures will be common for all draw calls of the batch
            for (int i = 0; i < RL_DEFAULT_BATCH_MAX_TEXTURE_UNITS; i++)
            {
                if (RLGL.State.activeTextureId[i] > 0)
                {
                    glActiveTexture(GL_TEXTURE0 + 1 + i);
                    glBindTexture(GL_TEXTURE_2D, RLGL.State.activeTextureId[i]);
                }
            }

            // Activate default sampler2D texture0 (one texture is always active for default batch shader)
            // NOTE: Batch system accumulates calls by texture0 changes, additional textures are enabled for all the draw calls
            glActiveTexture(GL_TEXTURE0);

            for (int i = 0, vertexOffset = 0; i < batch->drawCounter; i++)
            {
                // Bind current draw call texture, activated as GL_TEXTURE0 and Bound to sampler2D texture0 by default
                glBindTexture(GL_TEXTURE_2D, batch->draws[i].textureId);

                if ((batch->draws[i].mode == RL_LINES) || (batch->draws[i].mode == RL_TRIANGLES)) glDrawArrays(batch->draws[i].mode, vertexOffset, batch->draws[i].vertexCount);
                else
                {
    #if defined(GRAPHICS_API_OPENGL_33)
                    // We need to define the number of indices to be processed: elementCount*6
                    // NOTE: The final parameter tells the GPU the offset in bytes from the
                    // start of the index buffer to the location of the first index to process
                    glDrawElements(GL_TRIANGLES, batch->draws[i].vertexCount/4*6, GL_UNSIGNED_INT, (GLvoid *)(vertexOffset/4*6*sizeof(GLuint)));
    #endif
    #if defined(GRAPHICS_API_OPENGL_ES2)
                    glDrawElements(GL_TRIANGLES, batch->draws[i].vertexCount/4*6, GL_UNSIGNED_SHORT, (GLvoid *)(vertexOffset/4*6*sizeof(GLushort)));
    #endif
                }

                vertexOffset += (batch->draws[i].vertexCount + batch->draws[i].vertexAlignment);
            }

            if (!RLGL.ExtSupported.vao)
            {
                glBindBuffer(GL_ARRAY_BUFFER, 0);
                glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
            }

            glBindTexture(GL_TEXTURE_2D, 0);    // Unbind textures
        }

        if (RLGL.ExtSupported.vao) glBindVertexArray(0); // Unbind VAO

        glUseProgram(0);    // Unbind shader program
    }

    // Restore viewport to default measures
    if (eyeCount == 2) rlViewport(0, 0, RLGL.State.framebufferWidth, RLGL.State.framebufferHeight);
    //------------------------------------------------------------------------------------------------------------

    // Reset batch buffers
    //------------------------------------------------------------------------------------------------------------
    // Reset vertex counter for next frame
    RLGL.State.vertexCounter = 0;

    // Reset depth for next draw
    batch->currentDepth = -1.0f;

    // Restore projection/modelview matrices
    RLGL.State.projection = matProjection;
    RLGL.State.modelview = matModelView;

    // Reset RLGL.currentBatch->draws array
    for (int i = 0; i < RL_DEFAULT_BATCH_DRAWCALLS; i++)
    {
        batch->draws[i].mode = RL_QUADS;
        batch->draws[i].vertexCount = 0;
        batch->draws[i].textureId = RLGL.State.defaultTextureId;
    }

    // Reset active texture units for next batch
    for (int i = 0; i < RL_DEFAULT_BATCH_MAX_TEXTURE_UNITS; i++) RLGL.State.activeTextureId[i] = 0;

    // Reset draws counter to one draw for the batch
    batch->drawCounter = 1;
    //------------------------------------------------------------------------------------------------------------

    // Change to next buffer in the list (in case of multi-buffering)
    batch->currentBuffer++;
    if (batch->currentBuffer >= batch->bufferCount) batch->currentBuffer = 0;
#endif
}

// Set the active render batch for rlgl
void rlSetRenderBatchActive(rlRenderBatch *batch)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    rlDrawRenderBatch(RLGL.currentBatch);

    if (batch != NULL) RLGL.currentBatch = batch;
    else RLGL.currentBatch = &RLGL.defaultBatch;
#endif
}

// Update and draw internal render batch
void rlDrawRenderBatchActive(void)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    rlDrawRenderBatch(RLGL.currentBatch);    // NOTE: Stereo rendering is checked inside
#endif
}

// Check internal buffer overflow for a given number of vertex
// and force a rlRenderBatch draw call if required
bool rlCheckRenderBatchLimit(int vCount)
{
    bool overflow = false;

#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    if ((RLGL.State.vertexCounter + vCount) >=
        (RLGL.currentBatch->vertexBuffer[RLGL.currentBatch->currentBuffer].elementCount*4))
    {
        overflow = true;

        // Store current primitive drawing mode and texture id
        int currentMode = RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].mode;
        int currentTexture = RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].textureId;

        rlDrawRenderBatch(RLGL.currentBatch);    // NOTE: Stereo rendering is checked inside

        // Restore state of last batch so we can continue adding vertices
        RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].mode = currentMode;
        RLGL.currentBatch->draws[RLGL.currentBatch->drawCounter - 1].textureId = currentTexture;
    }
#endif

    return overflow;
}

// Textures data management
//-----------------------------------------------------------------------------------------
// Convert image data to OpenGL texture (returns OpenGL valid Id)
unsigned int rlLoadTexture(const void *data, int width, int height, int format, int mipmapCount)
{
    unsigned int id = 0;

    glBindTexture(GL_TEXTURE_2D, 0);    // Free any old binding

    // Check texture format support by OpenGL 1.1 (compressed textures not supported)
#if defined(GRAPHICS_API_OPENGL_11)
    if (format >= RL_PIXELFORMAT_COMPRESSED_DXT1_RGB)
    {
        TRACELOG(RL_LOG_WARNING, "GL: OpenGL 1.1 does not support GPU compressed texture formats");
        return id;
    }
#else
    if ((!RLGL.ExtSupported.texCompDXT) && ((format == RL_PIXELFORMAT_COMPRESSED_DXT1_RGB) || (format == RL_PIXELFORMAT_COMPRESSED_DXT1_RGBA) ||
        (format == RL_PIXELFORMAT_COMPRESSED_DXT3_RGBA) || (format == RL_PIXELFORMAT_COMPRESSED_DXT5_RGBA)))
    {
        TRACELOG(RL_LOG_WARNING, "GL: DXT compressed texture format not supported");
        return id;
    }
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    if ((!RLGL.ExtSupported.texCompETC1) && (format == RL_PIXELFORMAT_COMPRESSED_ETC1_RGB))
    {
        TRACELOG(RL_LOG_WARNING, "GL: ETC1 compressed texture format not supported");
        return id;
    }

    if ((!RLGL.ExtSupported.texCompETC2) && ((format == RL_PIXELFORMAT_COMPRESSED_ETC2_RGB) || (format == RL_PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA)))
    {
        TRACELOG(RL_LOG_WARNING, "GL: ETC2 compressed texture format not supported");
        return id;
    }

    if ((!RLGL.ExtSupported.texCompPVRT) && ((format == RL_PIXELFORMAT_COMPRESSED_PVRT_RGB) || (format == RL_PIXELFORMAT_COMPRESSED_PVRT_RGBA)))
    {
        TRACELOG(RL_LOG_WARNING, "GL: PVRT compressed texture format not supported");
        return id;
    }

    if ((!RLGL.ExtSupported.texCompASTC) && ((format == RL_PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA) || (format == RL_PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA)))
    {
        TRACELOG(RL_LOG_WARNING, "GL: ASTC compressed texture format not supported");
        return id;
    }
#endif
#endif  // GRAPHICS_API_OPENGL_11

    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

    glGenTextures(1, &id);              // Generate texture id

    glBindTexture(GL_TEXTURE_2D, id);

    int mipWidth = width;
    int mipHeight = height;
    int mipOffset = 0;          // Mipmap data offset, only used for tracelog

    // NOTE: Added pointer math separately from function to avoid UBSAN complaining
    unsigned char *dataPtr = NULL;
    if (data != NULL) dataPtr = (unsigned char *)data;

    // Load the different mipmap levels
    for (int i = 0; i < mipmapCount; i++)
    {
        unsigned int mipSize = rlGetPixelDataSize(mipWidth, mipHeight, format);

        unsigned int glInternalFormat, glFormat, glType;
        rlGetGlTextureFormats(format, &glInternalFormat, &glFormat, &glType);

        TRACELOGD("TEXTURE: Load mipmap level %i (%i x %i), size: %i, offset: %i", i, mipWidth, mipHeight, mipSize, mipOffset);

        if (glInternalFormat != 0)
        {
            if (format < RL_PIXELFORMAT_COMPRESSED_DXT1_RGB) glTexImage2D(GL_TEXTURE_2D, i, glInternalFormat, mipWidth, mipHeight, 0, glFormat, glType, dataPtr);
#if !defined(GRAPHICS_API_OPENGL_11)
            else glCompressedTexImage2D(GL_TEXTURE_2D, i, glInternalFormat, mipWidth, mipHeight, 0, mipSize, dataPtr);
#endif

#if defined(GRAPHICS_API_OPENGL_33)
            if (format == RL_PIXELFORMAT_UNCOMPRESSED_GRAYSCALE)
            {
                GLint swizzleMask[] = { GL_RED, GL_RED, GL_RED, GL_ONE };
                glTexParameteriv(GL_TEXTURE_2D, GL_TEXTURE_SWIZZLE_RGBA, swizzleMask);
            }
            else if (format == RL_PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA)
            {
#if defined(GRAPHICS_API_OPENGL_21)
                GLint swizzleMask[] = { GL_RED, GL_RED, GL_RED, GL_ALPHA };
#elif defined(GRAPHICS_API_OPENGL_33)
                GLint swizzleMask[] = { GL_RED, GL_RED, GL_RED, GL_GREEN };
#endif
                glTexParameteriv(GL_TEXTURE_2D, GL_TEXTURE_SWIZZLE_RGBA, swizzleMask);
            }
#endif
        }

        mipWidth /= 2;
        mipHeight /= 2;
        mipOffset += mipSize;       // Increment offset position to next mipmap
        if (data != NULL) dataPtr += mipSize;         // Increment data pointer to next mipmap

        // Security check for NPOT textures
        if (mipWidth < 1) mipWidth = 1;
        if (mipHeight < 1) mipHeight = 1;
    }

    // Texture parameters configuration
    // NOTE: glTexParameteri does NOT affect texture uploading, just the way it's used
#if defined(GRAPHICS_API_OPENGL_ES2)
    // NOTE: OpenGL ES 2.0 with no GL_OES_texture_npot support (i.e. WebGL) has limited NPOT support, so CLAMP_TO_EDGE must be used
    if (RLGL.ExtSupported.texNPOT)
    {
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);       // Set texture to repeat on x-axis
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);       // Set texture to repeat on y-axis
    }
    else
    {
        // NOTE: If using negative texture coordinates (LoadOBJ()), it does not work!
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);       // Set texture to clamp on x-axis
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);       // Set texture to clamp on y-axis
    }
#else
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);       // Set texture to repeat on x-axis
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);       // Set texture to repeat on y-axis
#endif

    // Magnification and minification filters
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);  // Alternative: GL_LINEAR
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);  // Alternative: GL_LINEAR

#if defined(GRAPHICS_API_OPENGL_33)
    if (mipmapCount > 1)
    {
        // Activate Trilinear filtering if mipmaps are available
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    }
#endif

    // At this point we have the texture loaded in GPU and texture parameters configured

    // NOTE: If mipmaps were not in data, they are not generated automatically

    // Unbind current texture
    glBindTexture(GL_TEXTURE_2D, 0);

    if (id > 0) TRACELOG(RL_LOG_INFO, "TEXTURE: [ID %i] Texture loaded successfully (%ix%i | %s | %i mipmaps)", id, width, height, rlGetPixelFormatName(format), mipmapCount);
    else TRACELOG(RL_LOG_WARNING, "TEXTURE: Failed to load texture");

    return id;
}

// Load depth texture/renderbuffer (to be attached to fbo)
// WARNING: OpenGL ES 2.0 requires GL_OES_depth_texture and WebGL requires WEBGL_depth_texture extensions
unsigned int rlLoadTextureDepth(int width, int height, bool useRenderBuffer)
{
    unsigned int id = 0;

#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    // In case depth textures not supported, we force renderbuffer usage
    if (!RLGL.ExtSupported.texDepth) useRenderBuffer = true;

    // NOTE: We let the implementation to choose the best bit-depth
    // Possible formats: GL_DEPTH_COMPONENT16, GL_DEPTH_COMPONENT24, GL_DEPTH_COMPONENT32 and GL_DEPTH_COMPONENT32F
    unsigned int glInternalFormat = GL_DEPTH_COMPONENT;

#if (defined(GRAPHICS_API_OPENGL_ES2) || defined(GRAPHICS_API_OPENGL_ES3))
    // WARNING: WebGL platform requires unsized internal format definition (GL_DEPTH_COMPONENT)
    // while other platforms using OpenGL ES 2.0 require/support sized internal formats depending on the GPU capabilities
    if (!RLGL.ExtSupported.texDepthWebGL || useRenderBuffer)
    {
        if (RLGL.ExtSupported.maxDepthBits == 32) glInternalFormat = GL_DEPTH_COMPONENT32_OES;
        else if (RLGL.ExtSupported.maxDepthBits == 24) glInternalFormat = GL_DEPTH_COMPONENT24_OES;
        else glInternalFormat = GL_DEPTH_COMPONENT16;
    }
#endif

    if (!useRenderBuffer && RLGL.ExtSupported.texDepth)
    {
        glGenTextures(1, &id);
        glBindTexture(GL_TEXTURE_2D, id);
        glTexImage2D(GL_TEXTURE_2D, 0, glInternalFormat, width, height, 0, GL_DEPTH_COMPONENT, GL_UNSIGNED_INT, NULL);

        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

        glBindTexture(GL_TEXTURE_2D, 0);

        TRACELOG(RL_LOG_INFO, "TEXTURE: Depth texture loaded successfully");
    }
    else
    {
        // Create the renderbuffer that will serve as the depth attachment for the framebuffer
        // NOTE: A renderbuffer is simpler than a texture and could offer better performance on embedded devices
        glGenRenderbuffers(1, &id);
        glBindRenderbuffer(GL_RENDERBUFFER, id);
        glRenderbufferStorage(GL_RENDERBUFFER, glInternalFormat, width, height);

        glBindRenderbuffer(GL_RENDERBUFFER, 0);

        TRACELOG(RL_LOG_INFO, "TEXTURE: [ID %i] Depth renderbuffer loaded successfully (%i bits)", id, (RLGL.ExtSupported.maxDepthBits >= 24)? RLGL.ExtSupported.maxDepthBits : 16);
    }
#endif

    return id;
}

// Load texture cubemap
// NOTE: Cubemap data is expected to be 6 images in a single data array (one after the other),
// expected the following convention: +X, -X, +Y, -Y, +Z, -Z
unsigned int rlLoadTextureCubemap(const void *data, int size, int format, int mipmapCount)
{
    unsigned int id = 0;

#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    int mipSize = size;

    // NOTE: Added pointer math separately from function to avoid UBSAN complaining
    unsigned char *dataPtr = NULL;
    if (data != NULL) dataPtr = (unsigned char *)data;

    unsigned int dataSize = rlGetPixelDataSize(size, size, format);

    glGenTextures(1, &id);
    glBindTexture(GL_TEXTURE_CUBE_MAP, id);

    unsigned int glInternalFormat, glFormat, glType;
    rlGetGlTextureFormats(format, &glInternalFormat, &glFormat, &glType);

    if (glInternalFormat != 0)
    {
        // Load cubemap faces/mipmaps
        for (int i = 0; i < 6*mipmapCount; i++)
        {
            int mipmapLevel = i/6;
            int face = i%6;

            if (data == NULL)
            {
                if (format < RL_PIXELFORMAT_COMPRESSED_DXT1_RGB)
                {
                    if ((format == RL_PIXELFORMAT_UNCOMPRESSED_R32) ||
                        (format == RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32A32) ||
                        (format == RL_PIXELFORMAT_UNCOMPRESSED_R16) ||
                        (format == RL_PIXELFORMAT_UNCOMPRESSED_R16G16B16A16)) TRACELOG(RL_LOG_WARNING, "TEXTURES: Cubemap requested format not supported");
                    else glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + face, mipmapLevel, glInternalFormat, mipSize, mipSize, 0, glFormat, glType, NULL);
                }
                else TRACELOG(RL_LOG_WARNING, "TEXTURES: Empty cubemap creation does not support compressed format");
            }
            else
            {
                if (format < RL_PIXELFORMAT_COMPRESSED_DXT1_RGB) glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + face, mipmapLevel, glInternalFormat, mipSize, mipSize, 0, glFormat, glType, (unsigned char *)dataPtr + face*dataSize);
                else glCompressedTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + face, mipmapLevel, glInternalFormat, mipSize, mipSize, 0, dataSize, (unsigned char *)dataPtr + face*dataSize);
            }

#if defined(GRAPHICS_API_OPENGL_33)
            if (format == RL_PIXELFORMAT_UNCOMPRESSED_GRAYSCALE)
            {
                GLint swizzleMask[] = { GL_RED, GL_RED, GL_RED, GL_ONE };
                glTexParameteriv(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_SWIZZLE_RGBA, swizzleMask);
            }
            else if (format == RL_PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA)
            {
#if defined(GRAPHICS_API_OPENGL_21)
                GLint swizzleMask[] = { GL_RED, GL_RED, GL_RED, GL_ALPHA };
#elif defined(GRAPHICS_API_OPENGL_33)
                GLint swizzleMask[] = { GL_RED, GL_RED, GL_RED, GL_GREEN };
#endif
                glTexParameteriv(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_SWIZZLE_RGBA, swizzleMask);
            }
#endif
            if (face == 5)
            {
                mipSize /= 2;
                if (data != NULL) dataPtr += dataSize*6; // Increment data pointer to next mipmap

                // Security check for NPOT textures
                if (mipSize < 1) mipSize = 1;

                dataSize = rlGetPixelDataSize(mipSize, mipSize, format);
            }
        }
    }

    // Set cubemap texture sampling parameters
    if (mipmapCount > 1) glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    else glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
#if defined(GRAPHICS_API_OPENGL_33)
    glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);  // Flag not supported on OpenGL ES 2.0
#endif

    glBindTexture(GL_TEXTURE_CUBE_MAP, 0);
#endif

    if (id > 0) TRACELOG(RL_LOG_INFO, "TEXTURE: [ID %i] Cubemap texture loaded successfully (%ix%i)", id, size, size);
    else TRACELOG(RL_LOG_WARNING, "TEXTURE: Failed to load cubemap texture");

    return id;
}

// Update already loaded texture in GPU with new data
// NOTE: We don't know safely if internal texture format is the expected one...
void rlUpdateTexture(unsigned int id, int offsetX, int offsetY, int width, int height, int format, const void *data)
{
    glBindTexture(GL_TEXTURE_2D, id);

    unsigned int glInternalFormat, glFormat, glType;
    rlGetGlTextureFormats(format, &glInternalFormat, &glFormat, &glType);

    if ((glInternalFormat != 0) && (format < RL_PIXELFORMAT_COMPRESSED_DXT1_RGB))
    {
        glTexSubImage2D(GL_TEXTURE_2D, 0, offsetX, offsetY, width, height, glFormat, glType, data);
    }
    else TRACELOG(RL_LOG_WARNING, "TEXTURE: [ID %i] Failed to update for current texture format (%i)", id, format);
}

// Get OpenGL internal formats and data type from raylib PixelFormat
void rlGetGlTextureFormats(int format, unsigned int *glInternalFormat, unsigned int *glFormat, unsigned int *glType)
{
    *glInternalFormat = 0;
    *glFormat = 0;
    *glType = 0;

    switch (format)
    {
    #if defined(GRAPHICS_API_OPENGL_11) || defined(GRAPHICS_API_OPENGL_21) || defined(GRAPHICS_API_OPENGL_ES2)
        // NOTE: on OpenGL ES 2.0 (WebGL), internalFormat must match format and options allowed are: GL_LUMINANCE, GL_RGB, GL_RGBA
        case RL_PIXELFORMAT_UNCOMPRESSED_GRAYSCALE: *glInternalFormat = GL_LUMINANCE; *glFormat = GL_LUMINANCE; *glType = GL_UNSIGNED_BYTE; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA: *glInternalFormat = GL_LUMINANCE_ALPHA; *glFormat = GL_LUMINANCE_ALPHA; *glType = GL_UNSIGNED_BYTE; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R5G6B5: *glInternalFormat = GL_RGB; *glFormat = GL_RGB; *glType = GL_UNSIGNED_SHORT_5_6_5; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8: *glInternalFormat = GL_RGB; *glFormat = GL_RGB; *glType = GL_UNSIGNED_BYTE; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R5G5B5A1: *glInternalFormat = GL_RGBA; *glFormat = GL_RGBA; *glType = GL_UNSIGNED_SHORT_5_5_5_1; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R4G4B4A4: *glInternalFormat = GL_RGBA; *glFormat = GL_RGBA; *glType = GL_UNSIGNED_SHORT_4_4_4_4; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8A8: *glInternalFormat = GL_RGBA; *glFormat = GL_RGBA; *glType = GL_UNSIGNED_BYTE; break;
        #if !defined(GRAPHICS_API_OPENGL_11)
        #if defined(GRAPHICS_API_OPENGL_ES3)
        case RL_PIXELFORMAT_UNCOMPRESSED_R32: if (RLGL.ExtSupported.texFloat32) *glInternalFormat = GL_R32F_EXT; *glFormat = GL_RED_EXT; *glType = GL_FLOAT; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32: if (RLGL.ExtSupported.texFloat32) *glInternalFormat = GL_RGB32F_EXT; *glFormat = GL_RGB; *glType = GL_FLOAT; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32A32: if (RLGL.ExtSupported.texFloat32) *glInternalFormat = GL_RGBA32F_EXT; *glFormat = GL_RGBA; *glType = GL_FLOAT; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R16: if (RLGL.ExtSupported.texFloat16) *glInternalFormat = GL_R16F_EXT; *glFormat = GL_RED_EXT; *glType = GL_HALF_FLOAT; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R16G16B16: if (RLGL.ExtSupported.texFloat16) *glInternalFormat = GL_RGB16F_EXT; *glFormat = GL_RGB; *glType = GL_HALF_FLOAT; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R16G16B16A16: if (RLGL.ExtSupported.texFloat16) *glInternalFormat = GL_RGBA16F_EXT; *glFormat = GL_RGBA; *glType = GL_HALF_FLOAT; break;
        #else
        case RL_PIXELFORMAT_UNCOMPRESSED_R32: if (RLGL.ExtSupported.texFloat32) *glInternalFormat = GL_LUMINANCE; *glFormat = GL_LUMINANCE; *glType = GL_FLOAT; break;            // NOTE: Requires extension OES_texture_float
        case RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32: if (RLGL.ExtSupported.texFloat32) *glInternalFormat = GL_RGB; *glFormat = GL_RGB; *glType = GL_FLOAT; break;                  // NOTE: Requires extension OES_texture_float
        case RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32A32: if (RLGL.ExtSupported.texFloat32) *glInternalFormat = GL_RGBA; *glFormat = GL_RGBA; *glType = GL_FLOAT; break;             // NOTE: Requires extension OES_texture_float
        #if defined(GRAPHICS_API_OPENGL_21)
        case RL_PIXELFORMAT_UNCOMPRESSED_R16: if (RLGL.ExtSupported.texFloat16) *glInternalFormat = GL_LUMINANCE; *glFormat = GL_LUMINANCE; *glType = GL_HALF_FLOAT_ARB; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R16G16B16: if (RLGL.ExtSupported.texFloat16) *glInternalFormat = GL_RGB; *glFormat = GL_RGB; *glType = GL_HALF_FLOAT_ARB; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R16G16B16A16: if (RLGL.ExtSupported.texFloat16) *glInternalFormat = GL_RGBA; *glFormat = GL_RGBA; *glType = GL_HALF_FLOAT_ARB; break;
        #else // defined(GRAPHICS_API_OPENGL_ES2)
        case RL_PIXELFORMAT_UNCOMPRESSED_R16: if (RLGL.ExtSupported.texFloat16) *glInternalFormat = GL_LUMINANCE; *glFormat = GL_LUMINANCE; *glType = GL_HALF_FLOAT_OES; break;   // NOTE: Requires extension OES_texture_half_float
        case RL_PIXELFORMAT_UNCOMPRESSED_R16G16B16: if (RLGL.ExtSupported.texFloat16) *glInternalFormat = GL_RGB; *glFormat = GL_RGB; *glType = GL_HALF_FLOAT_OES; break;         // NOTE: Requires extension OES_texture_half_float
        case RL_PIXELFORMAT_UNCOMPRESSED_R16G16B16A16: if (RLGL.ExtSupported.texFloat16) *glInternalFormat = GL_RGBA; *glFormat = GL_RGBA; *glType = GL_HALF_FLOAT_OES; break;    // NOTE: Requires extension OES_texture_half_float
        #endif
        #endif
        #endif
    #elif defined(GRAPHICS_API_OPENGL_33)
        case RL_PIXELFORMAT_UNCOMPRESSED_GRAYSCALE: *glInternalFormat = GL_R8; *glFormat = GL_RED; *glType = GL_UNSIGNED_BYTE; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA: *glInternalFormat = GL_RG8; *glFormat = GL_RG; *glType = GL_UNSIGNED_BYTE; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R5G6B5: *glInternalFormat = GL_RGB565; *glFormat = GL_RGB; *glType = GL_UNSIGNED_SHORT_5_6_5; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8: *glInternalFormat = GL_RGB8; *glFormat = GL_RGB; *glType = GL_UNSIGNED_BYTE; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R5G5B5A1: *glInternalFormat = GL_RGB5_A1; *glFormat = GL_RGBA; *glType = GL_UNSIGNED_SHORT_5_5_5_1; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R4G4B4A4: *glInternalFormat = GL_RGBA4; *glFormat = GL_RGBA; *glType = GL_UNSIGNED_SHORT_4_4_4_4; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8A8: *glInternalFormat = GL_RGBA8; *glFormat = GL_RGBA; *glType = GL_UNSIGNED_BYTE; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R32: if (RLGL.ExtSupported.texFloat32) *glInternalFormat = GL_R32F; *glFormat = GL_RED; *glType = GL_FLOAT; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32: if (RLGL.ExtSupported.texFloat32) *glInternalFormat = GL_RGB32F; *glFormat = GL_RGB; *glType = GL_FLOAT; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32A32: if (RLGL.ExtSupported.texFloat32) *glInternalFormat = GL_RGBA32F; *glFormat = GL_RGBA; *glType = GL_FLOAT; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R16: if (RLGL.ExtSupported.texFloat16) *glInternalFormat = GL_R16F; *glFormat = GL_RED; *glType = GL_HALF_FLOAT; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R16G16B16: if (RLGL.ExtSupported.texFloat16) *glInternalFormat = GL_RGB16F; *glFormat = GL_RGB; *glType = GL_HALF_FLOAT; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R16G16B16A16: if (RLGL.ExtSupported.texFloat16) *glInternalFormat = GL_RGBA16F; *glFormat = GL_RGBA; *glType = GL_HALF_FLOAT; break;
    #endif
    #if !defined(GRAPHICS_API_OPENGL_11)
        case RL_PIXELFORMAT_COMPRESSED_DXT1_RGB: if (RLGL.ExtSupported.texCompDXT) *glInternalFormat = GL_COMPRESSED_RGB_S3TC_DXT1_EXT; break;
        case RL_PIXELFORMAT_COMPRESSED_DXT1_RGBA: if (RLGL.ExtSupported.texCompDXT) *glInternalFormat = GL_COMPRESSED_RGBA_S3TC_DXT1_EXT; break;
        case RL_PIXELFORMAT_COMPRESSED_DXT3_RGBA: if (RLGL.ExtSupported.texCompDXT) *glInternalFormat = GL_COMPRESSED_RGBA_S3TC_DXT3_EXT; break;
        case RL_PIXELFORMAT_COMPRESSED_DXT5_RGBA: if (RLGL.ExtSupported.texCompDXT) *glInternalFormat = GL_COMPRESSED_RGBA_S3TC_DXT5_EXT; break;
        case RL_PIXELFORMAT_COMPRESSED_ETC1_RGB: if (RLGL.ExtSupported.texCompETC1) *glInternalFormat = GL_ETC1_RGB8_OES; break;                      // NOTE: Requires OpenGL ES 2.0 or OpenGL 4.3
        case RL_PIXELFORMAT_COMPRESSED_ETC2_RGB: if (RLGL.ExtSupported.texCompETC2) *glInternalFormat = GL_COMPRESSED_RGB8_ETC2; break;               // NOTE: Requires OpenGL ES 3.0 or OpenGL 4.3
        case RL_PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA: if (RLGL.ExtSupported.texCompETC2) *glInternalFormat = GL_COMPRESSED_RGBA8_ETC2_EAC; break;     // NOTE: Requires OpenGL ES 3.0 or OpenGL 4.3
        case RL_PIXELFORMAT_COMPRESSED_PVRT_RGB: if (RLGL.ExtSupported.texCompPVRT) *glInternalFormat = GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG; break;    // NOTE: Requires PowerVR GPU
        case RL_PIXELFORMAT_COMPRESSED_PVRT_RGBA: if (RLGL.ExtSupported.texCompPVRT) *glInternalFormat = GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG; break;  // NOTE: Requires PowerVR GPU
        case RL_PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA: if (RLGL.ExtSupported.texCompASTC) *glInternalFormat = GL_COMPRESSED_RGBA_ASTC_4x4_KHR; break;  // NOTE: Requires OpenGL ES 3.1 or OpenGL 4.3
        case RL_PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA: if (RLGL.ExtSupported.texCompASTC) *glInternalFormat = GL_COMPRESSED_RGBA_ASTC_8x8_KHR; break;  // NOTE: Requires OpenGL ES 3.1 or OpenGL 4.3
    #endif
        default: TRACELOG(RL_LOG_WARNING, "TEXTURE: Current format not supported (%i)", format); break;
    }
}

// Unload texture from GPU memory
void rlUnloadTexture(unsigned int id)
{
    glDeleteTextures(1, &id);
}

// Generate mipmap data for selected texture
// NOTE: Only supports GPU mipmap generation
void rlGenTextureMipmaps(unsigned int id, int width, int height, int format, int *mipmaps)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    glBindTexture(GL_TEXTURE_2D, id);

    // Check if texture is power-of-two (POT)
    bool texIsPOT = false;

    if (((width > 0) && ((width & (width - 1)) == 0)) &&
        ((height > 0) && ((height & (height - 1)) == 0))) texIsPOT = true;

    if ((texIsPOT) || (RLGL.ExtSupported.texNPOT))
    {
        //glHint(GL_GENERATE_MIPMAP_HINT, GL_DONT_CARE);   // Hint for mipmaps generation algorithm: GL_FASTEST, GL_NICEST, GL_DONT_CARE
        glGenerateMipmap(GL_TEXTURE_2D);    // Generate mipmaps automatically

        #define MIN(a,b) (((a)<(b))? (a):(b))
        #define MAX(a,b) (((a)>(b))? (a):(b))

        *mipmaps = 1 + (int)floor(log(MAX(width, height))/log(2));
        TRACELOG(RL_LOG_INFO, "TEXTURE: [ID %i] Mipmaps generated automatically, total: %i", id, *mipmaps);
    }
    else TRACELOG(RL_LOG_WARNING, "TEXTURE: [ID %i] Failed to generate mipmaps", id);

    glBindTexture(GL_TEXTURE_2D, 0);
#else
    TRACELOG(RL_LOG_WARNING, "TEXTURE: [ID %i] GPU mipmap generation not supported", id);
#endif
}

// Read texture pixel data
void *rlReadTexturePixels(unsigned int id, int width, int height, int format)
{
    void *pixels = NULL;

#if defined(GRAPHICS_API_OPENGL_11) || defined(GRAPHICS_API_OPENGL_33)
    glBindTexture(GL_TEXTURE_2D, id);

    // NOTE: Using texture id, we can retrieve some texture info (but not on OpenGL ES 2.0)
    // Possible texture info: GL_TEXTURE_RED_SIZE, GL_TEXTURE_GREEN_SIZE, GL_TEXTURE_BLUE_SIZE, GL_TEXTURE_ALPHA_SIZE
    //int width, height, format;
    //glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, &width);
    //glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, &height);
    //glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_INTERNAL_FORMAT, &format);

    // NOTE: Each row written to or read from by OpenGL pixel operations like glGetTexImage are aligned to a 4 byte boundary by default, which may add some padding
    // Use glPixelStorei to modify padding with the GL_[UN]PACK_ALIGNMENT setting
    // GL_PACK_ALIGNMENT affects operations that read from OpenGL memory (glReadPixels, glGetTexImage, etc.)
    // GL_UNPACK_ALIGNMENT affects operations that write to OpenGL memory (glTexImage, etc.)
    glPixelStorei(GL_PACK_ALIGNMENT, 1);

    unsigned int glInternalFormat, glFormat, glType;
    rlGetGlTextureFormats(format, &glInternalFormat, &glFormat, &glType);
    unsigned int size = rlGetPixelDataSize(width, height, format);

    if ((glInternalFormat != 0) && (format < RL_PIXELFORMAT_COMPRESSED_DXT1_RGB))
    {
        pixels = RL_MALLOC(size);
        glGetTexImage(GL_TEXTURE_2D, 0, glFormat, glType, pixels);
    }
    else TRACELOG(RL_LOG_WARNING, "TEXTURE: [ID %i] Data retrieval not suported for pixel format (%i)", id, format);

    glBindTexture(GL_TEXTURE_2D, 0);
#endif

#if defined(GRAPHICS_API_OPENGL_ES2)
    // glGetTexImage() is not available on OpenGL ES 2.0
    // Texture width and height are required on OpenGL ES 2.0, there is no way to get it from texture id
    // Two possible Options:
    // 1 - Bind texture to color fbo attachment and glReadPixels()
    // 2 - Create an fbo, activate it, render quad with texture, glReadPixels()
    // We are using Option 1, just need to care for texture format on retrieval
    // NOTE: This behaviour could be conditioned by graphic driver...
    unsigned int fboId = rlLoadFramebuffer();

    glBindFramebuffer(GL_FRAMEBUFFER, fboId);
    glBindTexture(GL_TEXTURE_2D, 0);

    // Attach our texture to FBO
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, id, 0);

    // We read data as RGBA because FBO texture is configured as RGBA, despite binding another texture format
    pixels = (unsigned char *)RL_MALLOC(rlGetPixelDataSize(width, height, RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8A8));
    glReadPixels(0, 0, width, height, GL_RGBA, GL_UNSIGNED_BYTE, pixels);

    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    // Clean up temporal fbo
    rlUnloadFramebuffer(fboId);
#endif

    return pixels;
}

// Read screen pixel data (color buffer)
unsigned char *rlReadScreenPixels(int width, int height)
{
    unsigned char *screenData = (unsigned char *)RL_CALLOC(width*height*4, sizeof(unsigned char));

    // NOTE 1: glReadPixels returns image flipped vertically -> (0,0) is the bottom left corner of the framebuffer
    // NOTE 2: We are getting alpha channel! Be careful, it can be transparent if not cleared properly!
    glReadPixels(0, 0, width, height, GL_RGBA, GL_UNSIGNED_BYTE, screenData);

    // Flip image vertically!
    unsigned char *imgData = (unsigned char *)RL_MALLOC(width*height*4*sizeof(unsigned char));

    for (int y = height - 1; y >= 0; y--)
    {
        for (int x = 0; x < (width*4); x++)
        {
            imgData[((height - 1) - y)*width*4 + x] = screenData[(y*width*4) + x];  // Flip line

            // Set alpha component value to 255 (no trasparent image retrieval)
            // NOTE: Alpha value has already been applied to RGB in framebuffer, we don't need it!
            if (((x + 1)%4) == 0) imgData[((height - 1) - y)*width*4 + x] = 255;
        }
    }

    RL_FREE(screenData);

    return imgData;     // NOTE: image data should be freed
}

// Framebuffer management (fbo)
//-----------------------------------------------------------------------------------------
// Load a framebuffer to be used for rendering
// NOTE: No textures attached
unsigned int rlLoadFramebuffer(void)
{
    unsigned int fboId = 0;

#if (defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)) && defined(RLGL_RENDER_TEXTURES_HINT)
    glGenFramebuffers(1, &fboId);       // Create the framebuffer object
    glBindFramebuffer(GL_FRAMEBUFFER, 0);   // Unbind any framebuffer
#endif

    return fboId;
}

// Attach color buffer texture to an fbo (unloads previous attachment)
// NOTE: Attach type: 0-Color, 1-Depth renderbuffer, 2-Depth texture
void rlFramebufferAttach(unsigned int fboId, unsigned int texId, int attachType, int texType, int mipLevel)
{
#if (defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)) && defined(RLGL_RENDER_TEXTURES_HINT)
    glBindFramebuffer(GL_FRAMEBUFFER, fboId);

    switch (attachType)
    {
        case RL_ATTACHMENT_COLOR_CHANNEL0:
        case RL_ATTACHMENT_COLOR_CHANNEL1:
        case RL_ATTACHMENT_COLOR_CHANNEL2:
        case RL_ATTACHMENT_COLOR_CHANNEL3:
        case RL_ATTACHMENT_COLOR_CHANNEL4:
        case RL_ATTACHMENT_COLOR_CHANNEL5:
        case RL_ATTACHMENT_COLOR_CHANNEL6:
        case RL_ATTACHMENT_COLOR_CHANNEL7:
        {
            if (texType == RL_ATTACHMENT_TEXTURE2D) glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + attachType, GL_TEXTURE_2D, texId, mipLevel);
            else if (texType == RL_ATTACHMENT_RENDERBUFFER) glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + attachType, GL_RENDERBUFFER, texId);
            else if (texType >= RL_ATTACHMENT_CUBEMAP_POSITIVE_X) glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + attachType, GL_TEXTURE_CUBE_MAP_POSITIVE_X + texType, texId, mipLevel);

        } break;
        case RL_ATTACHMENT_DEPTH:
        {
            if (texType == RL_ATTACHMENT_TEXTURE2D) glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, texId, mipLevel);
            else if (texType == RL_ATTACHMENT_RENDERBUFFER)  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, texId);

        } break;
        case RL_ATTACHMENT_STENCIL:
        {
            if (texType == RL_ATTACHMENT_TEXTURE2D) glFramebufferTexture2D(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_TEXTURE_2D, texId, mipLevel);
            else if (texType == RL_ATTACHMENT_RENDERBUFFER)  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, texId);

        } break;
        default: break;
    }

    glBindFramebuffer(GL_FRAMEBUFFER, 0);
#endif
}

// Verify render texture is complete
bool rlFramebufferComplete(unsigned int id)
{
    bool result = false;

#if (defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)) && defined(RLGL_RENDER_TEXTURES_HINT)
    glBindFramebuffer(GL_FRAMEBUFFER, id);

    GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);

    if (status != GL_FRAMEBUFFER_COMPLETE)
    {
        switch (status)
        {
            case GL_FRAMEBUFFER_UNSUPPORTED: TRACELOG(RL_LOG_WARNING, "FBO: [ID %i] Framebuffer is unsupported", id); break;
            case GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT: TRACELOG(RL_LOG_WARNING, "FBO: [ID %i] Framebuffer has incomplete attachment", id); break;
#if defined(GRAPHICS_API_OPENGL_ES2)
            case GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS: TRACELOG(RL_LOG_WARNING, "FBO: [ID %i] Framebuffer has incomplete dimensions", id); break;
#endif
            case GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT: TRACELOG(RL_LOG_WARNING, "FBO: [ID %i] Framebuffer has a missing attachment", id); break;
            default: break;
        }
    }

    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    result = (status == GL_FRAMEBUFFER_COMPLETE);
#endif

    return result;
}

// Unload framebuffer from GPU memory
// NOTE: All attached textures/cubemaps/renderbuffers are also deleted
void rlUnloadFramebuffer(unsigned int id)
{
#if (defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)) && defined(RLGL_RENDER_TEXTURES_HINT)
    // Query depth attachment to automatically delete texture/renderbuffer
    int depthType = 0, depthId = 0;
    glBindFramebuffer(GL_FRAMEBUFFER, id);   // Bind framebuffer to query depth texture type
    glGetFramebufferAttachmentParameteriv(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE, &depthType);

    // TODO: Review warning retrieving object name in WebGL
    // WARNING: WebGL: INVALID_ENUM: getFramebufferAttachmentParameter: invalid parameter name
    // https://registry.khronos.org/webgl/specs/latest/1.0/
    glGetFramebufferAttachmentParameteriv(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME, &depthId);

    unsigned int depthIdU = (unsigned int)depthId;
    if (depthType == GL_RENDERBUFFER) glDeleteRenderbuffers(1, &depthIdU);
    else if (depthType == GL_TEXTURE) glDeleteTextures(1, &depthIdU);

    // NOTE: If a texture object is deleted while its image is attached to the *currently bound* framebuffer,
    // the texture image is automatically detached from the currently bound framebuffer

    glBindFramebuffer(GL_FRAMEBUFFER, 0);
    glDeleteFramebuffers(1, &id);

    TRACELOG(RL_LOG_INFO, "FBO: [ID %i] Unloaded framebuffer from VRAM (GPU)", id);
#endif
}

// Vertex data management
//-----------------------------------------------------------------------------------------
// Load a new attributes buffer
unsigned int rlLoadVertexBuffer(const void *buffer, int size, bool dynamic)
{
    unsigned int id = 0;

#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    glGenBuffers(1, &id);
    glBindBuffer(GL_ARRAY_BUFFER, id);
    glBufferData(GL_ARRAY_BUFFER, size, buffer, dynamic? GL_DYNAMIC_DRAW : GL_STATIC_DRAW);
#endif

    return id;
}

// Load a new attributes element buffer
unsigned int rlLoadVertexBufferElement(const void *buffer, int size, bool dynamic)
{
    unsigned int id = 0;

#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    glGenBuffers(1, &id);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, id);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, size, buffer, dynamic? GL_DYNAMIC_DRAW : GL_STATIC_DRAW);
#endif

    return id;
}

// Enable vertex buffer (VBO)
void rlEnableVertexBuffer(unsigned int id)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    glBindBuffer(GL_ARRAY_BUFFER, id);
#endif
}

// Disable vertex buffer (VBO)
void rlDisableVertexBuffer(void)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    glBindBuffer(GL_ARRAY_BUFFER, 0);
#endif
}

// Enable vertex buffer element (VBO element)
void rlEnableVertexBufferElement(unsigned int id)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, id);
#endif
}

// Disable vertex buffer element (VBO element)
void rlDisableVertexBufferElement(void)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
#endif
}

// Update vertex buffer with new data
// NOTE: dataSize and offset must be provided in bytes
void rlUpdateVertexBuffer(unsigned int id, const void *data, int dataSize, int offset)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    glBindBuffer(GL_ARRAY_BUFFER, id);
    glBufferSubData(GL_ARRAY_BUFFER, offset, dataSize, data);
#endif
}

// Update vertex buffer elements with new data
// NOTE: dataSize and offset must be provided in bytes
void rlUpdateVertexBufferElements(unsigned int id, const void *data, int dataSize, int offset)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, id);
    glBufferSubData(GL_ELEMENT_ARRAY_BUFFER, offset, dataSize, data);
#endif
}

// Enable vertex array object (VAO)
bool rlEnableVertexArray(unsigned int vaoId)
{
    bool result = false;
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    if (RLGL.ExtSupported.vao)
    {
        glBindVertexArray(vaoId);
        result = true;
    }
#endif
    return result;
}

// Disable vertex array object (VAO)
void rlDisableVertexArray(void)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    if (RLGL.ExtSupported.vao) glBindVertexArray(0);
#endif
}

// Enable vertex attribute index
void rlEnableVertexAttribute(unsigned int index)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    glEnableVertexAttribArray(index);
#endif
}

// Disable vertex attribute index
void rlDisableVertexAttribute(unsigned int index)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    glDisableVertexAttribArray(index);
#endif
}

// Draw vertex array
void rlDrawVertexArray(int offset, int count)
{
    glDrawArrays(GL_TRIANGLES, offset, count);
}

// Draw vertex array elements
void rlDrawVertexArrayElements(int offset, int count, const void *buffer)
{
    // NOTE: Added pointer math separately from function to avoid UBSAN complaining
    unsigned short *bufferPtr = (unsigned short *)buffer;
    if (offset > 0) bufferPtr += offset;

    glDrawElements(GL_TRIANGLES, count, GL_UNSIGNED_SHORT, (const unsigned short *)bufferPtr);
}

// Draw vertex array instanced
void rlDrawVertexArrayInstanced(int offset, int count, int instances)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    glDrawArraysInstanced(GL_TRIANGLES, 0, count, instances);
#endif
}

// Draw vertex array elements instanced
void rlDrawVertexArrayElementsInstanced(int offset, int count, const void *buffer, int instances)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    // NOTE: Added pointer math separately from function to avoid UBSAN complaining
    unsigned short *bufferPtr = (unsigned short *)buffer;
    if (offset > 0) bufferPtr += offset;

    glDrawElementsInstanced(GL_TRIANGLES, count, GL_UNSIGNED_SHORT, (const unsigned short *)bufferPtr, instances);
#endif
}

#if defined(GRAPHICS_API_OPENGL_11)
// Enable vertex state pointer
void rlEnableStatePointer(int vertexAttribType, void *buffer)
{
    if (buffer != NULL) glEnableClientState(vertexAttribType);
    switch (vertexAttribType)
    {
        case GL_VERTEX_ARRAY: glVertexPointer(3, GL_FLOAT, 0, buffer); break;
        case GL_TEXTURE_COORD_ARRAY: glTexCoordPointer(2, GL_FLOAT, 0, buffer); break;
        case GL_NORMAL_ARRAY: if (buffer != NULL) glNormalPointer(GL_FLOAT, 0, buffer); break;
        case GL_COLOR_ARRAY: if (buffer != NULL) glColorPointer(4, GL_UNSIGNED_BYTE, 0, buffer); break;
        //case GL_INDEX_ARRAY: if (buffer != NULL) glIndexPointer(GL_SHORT, 0, buffer); break; // Indexed colors
        default: break;
    }
}

// Disable vertex state pointer
void rlDisableStatePointer(int vertexAttribType)
{
    glDisableClientState(vertexAttribType);
}
#endif

// Load vertex array object (VAO)
unsigned int rlLoadVertexArray(void)
{
    unsigned int vaoId = 0;
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    if (RLGL.ExtSupported.vao)
    {
        glGenVertexArrays(1, &vaoId);
    }
#endif
    return vaoId;
}

// Set vertex attribute
void rlSetVertexAttribute(unsigned int index, int compSize, int type, bool normalized, int stride, int offset)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    // NOTE: Data type could be: GL_BYTE, GL_UNSIGNED_BYTE, GL_SHORT, GL_UNSIGNED_SHORT, GL_INT, GL_UNSIGNED_INT
    // Additional types (depends on OpenGL version or extensions):
    //  - GL_HALF_FLOAT, GL_FLOAT, GL_DOUBLE, GL_FIXED,
    //  - GL_INT_2_10_10_10_REV, GL_UNSIGNED_INT_2_10_10_10_REV, GL_UNSIGNED_INT_10F_11F_11F_REV

    size_t offsetNative = offset;
    glVertexAttribPointer(index, compSize, type, normalized, stride, (void *)offsetNative);
#endif
}

// Set vertex attribute divisor
void rlSetVertexAttributeDivisor(unsigned int index, int divisor)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    glVertexAttribDivisor(index, divisor);
#endif
}

// Unload vertex array object (VAO)
void rlUnloadVertexArray(unsigned int vaoId)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    if (RLGL.ExtSupported.vao)
    {
        glBindVertexArray(0);
        glDeleteVertexArrays(1, &vaoId);
        TRACELOG(RL_LOG_INFO, "VAO: [ID %i] Unloaded vertex array data from VRAM (GPU)", vaoId);
    }
#endif
}

// Unload vertex buffer (VBO)
void rlUnloadVertexBuffer(unsigned int vboId)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    glDeleteBuffers(1, &vboId);
    //TRACELOG(RL_LOG_INFO, "VBO: Unloaded vertex data from VRAM (GPU)");
#endif
}

// Shaders management
//-----------------------------------------------------------------------------------------------
// Load shader from code strings
// NOTE: If shader string is NULL, using default vertex/fragment shaders
unsigned int rlLoadShaderCode(const char *vsCode, const char *fsCode)
{
    unsigned int id = 0;

#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    unsigned int vertexShaderId = 0;
    unsigned int fragmentShaderId = 0;

    // Compile vertex shader (if provided)
    // NOTE: If not vertex shader is provided, use default one
    if (vsCode != NULL) vertexShaderId = rlCompileShader(vsCode, GL_VERTEX_SHADER);
    else vertexShaderId = RLGL.State.defaultVShaderId;

    // Compile fragment shader (if provided)
    // NOTE: If not vertex shader is provided, use default one
    if (fsCode != NULL) fragmentShaderId = rlCompileShader(fsCode, GL_FRAGMENT_SHADER);
    else fragmentShaderId = RLGL.State.defaultFShaderId;

    // In case vertex and fragment shader are the default ones, no need to recompile, we can just assign the default shader program id
    if ((vertexShaderId == RLGL.State.defaultVShaderId) && (fragmentShaderId == RLGL.State.defaultFShaderId)) id = RLGL.State.defaultShaderId;
    else if ((vertexShaderId > 0) && (fragmentShaderId > 0))
    {
        // One of or both shader are new, we need to compile a new shader program
        id = rlLoadShaderProgram(vertexShaderId, fragmentShaderId);

        // We can detach and delete vertex/fragment shaders (if not default ones)
        // NOTE: We detach shader before deletion to make sure memory is freed
        if (vertexShaderId != RLGL.State.defaultVShaderId)
        {
            // WARNING: Shader program linkage could fail and returned id is 0
            if (id > 0) glDetachShader(id, vertexShaderId);
            glDeleteShader(vertexShaderId);
        }
        if (fragmentShaderId != RLGL.State.defaultFShaderId)
        {
            // WARNING: Shader program linkage could fail and returned id is 0
            if (id > 0) glDetachShader(id, fragmentShaderId);
            glDeleteShader(fragmentShaderId);
        }

        // In case shader program loading failed, we assign default shader
        if (id == 0)
        {
            // In case shader loading fails, we return the default shader
            TRACELOG(RL_LOG_WARNING, "SHADER: Failed to load custom shader code, using default shader");
            id = RLGL.State.defaultShaderId;
        }
        /*
        else
        {
            // Get available shader uniforms
            // NOTE: This information is useful for debug...
            int uniformCount = -1;
            glGetProgramiv(id, GL_ACTIVE_UNIFORMS, &uniformCount);

            for (int i = 0; i < uniformCount; i++)
            {
                int namelen = -1;
                int num = -1;
                char name[256] = { 0 };     // Assume no variable names longer than 256
                GLenum type = GL_ZERO;

                // Get the name of the uniforms
                glGetActiveUniform(id, i, sizeof(name) - 1, &namelen, &num, &type, name);

                name[namelen] = 0;
                TRACELOGD("SHADER: [ID %i] Active uniform (%s) set at location: %i", id, name, glGetUniformLocation(id, name));
            }
        }
        */
    }
#endif

    return id;
}

// Compile custom shader and return shader id
unsigned int rlCompileShader(const char *shaderCode, int type)
{
    unsigned int shader = 0;

#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    shader = glCreateShader(type);
    glShaderSource(shader, 1, &shaderCode, NULL);

    GLint success = 0;
    glCompileShader(shader);
    glGetShaderiv(shader, GL_COMPILE_STATUS, &success);

    if (success == GL_FALSE)
    {
        switch (type)
        {
            case GL_VERTEX_SHADER: TRACELOG(RL_LOG_WARNING, "SHADER: [ID %i] Failed to compile vertex shader code", shader); break;
            case GL_FRAGMENT_SHADER: TRACELOG(RL_LOG_WARNING, "SHADER: [ID %i] Failed to compile fragment shader code", shader); break;
            //case GL_GEOMETRY_SHADER:
        #if defined(GRAPHICS_API_OPENGL_43)
            case GL_COMPUTE_SHADER: TRACELOG(RL_LOG_WARNING, "SHADER: [ID %i] Failed to compile compute shader code", shader); break;
        #elif defined(GRAPHICS_API_OPENGL_33)
            case GL_COMPUTE_SHADER: TRACELOG(RL_LOG_WARNING, "SHADER: Compute shaders not enabled. Define GRAPHICS_API_OPENGL_43", shader); break;
        #endif
            default: break;
        }

        int maxLength = 0;
        glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &maxLength);

        if (maxLength > 0)
        {
            int length = 0;
            char *log = (char *)RL_CALLOC(maxLength, sizeof(char));
            glGetShaderInfoLog(shader, maxLength, &length, log);
            TRACELOG(RL_LOG_WARNING, "SHADER: [ID %i] Compile error: %s", shader, log);
            RL_FREE(log);
        }

        shader = 0;
    }
    else
    {
        switch (type)
        {
            case GL_VERTEX_SHADER: TRACELOG(RL_LOG_INFO, "SHADER: [ID %i] Vertex shader compiled successfully", shader); break;
            case GL_FRAGMENT_SHADER: TRACELOG(RL_LOG_INFO, "SHADER: [ID %i] Fragment shader compiled successfully", shader); break;
            //case GL_GEOMETRY_SHADER:
        #if defined(GRAPHICS_API_OPENGL_43)
            case GL_COMPUTE_SHADER: TRACELOG(RL_LOG_INFO, "SHADER: [ID %i] Compute shader compiled successfully", shader); break;
        #elif defined(GRAPHICS_API_OPENGL_33)
            case GL_COMPUTE_SHADER: TRACELOG(RL_LOG_WARNING, "SHADER: Compute shaders not enabled. Define GRAPHICS_API_OPENGL_43", shader); break;
        #endif
            default: break;
        }
    }
#endif

    return shader;
}

// Load custom shader strings and return program id
unsigned int rlLoadShaderProgram(unsigned int vShaderId, unsigned int fShaderId)
{
    unsigned int program = 0;

#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    GLint success = 0;
    program = glCreateProgram();

    glAttachShader(program, vShaderId);
    glAttachShader(program, fShaderId);

    // NOTE: Default attribute shader locations must be Bound before linking
    glBindAttribLocation(program, RL_DEFAULT_SHADER_ATTRIB_LOCATION_POSITION, RL_DEFAULT_SHADER_ATTRIB_NAME_POSITION);
    glBindAttribLocation(program, RL_DEFAULT_SHADER_ATTRIB_LOCATION_TEXCOORD, RL_DEFAULT_SHADER_ATTRIB_NAME_TEXCOORD);
    glBindAttribLocation(program, RL_DEFAULT_SHADER_ATTRIB_LOCATION_NORMAL, RL_DEFAULT_SHADER_ATTRIB_NAME_NORMAL);
    glBindAttribLocation(program, RL_DEFAULT_SHADER_ATTRIB_LOCATION_COLOR, RL_DEFAULT_SHADER_ATTRIB_NAME_COLOR);
    glBindAttribLocation(program, RL_DEFAULT_SHADER_ATTRIB_LOCATION_TANGENT, RL_DEFAULT_SHADER_ATTRIB_NAME_TANGENT);
    glBindAttribLocation(program, RL_DEFAULT_SHADER_ATTRIB_LOCATION_TEXCOORD2, RL_DEFAULT_SHADER_ATTRIB_NAME_TEXCOORD2);

#ifdef RL_SUPPORT_MESH_GPU_SKINNING
    glBindAttribLocation(program, RL_DEFAULT_SHADER_ATTRIB_LOCATION_BONEIDS, RL_DEFAULT_SHADER_ATTRIB_NAME_BONEIDS);
    glBindAttribLocation(program, RL_DEFAULT_SHADER_ATTRIB_LOCATION_BONEWEIGHTS, RL_DEFAULT_SHADER_ATTRIB_NAME_BONEWEIGHTS);
#endif

    // NOTE: If some attrib name is no found on the shader, it locations becomes -1

    glLinkProgram(program);

    // NOTE: All uniform variables are intitialised to 0 when a program links

    glGetProgramiv(program, GL_LINK_STATUS, &success);

    if (success == GL_FALSE)
    {
        TRACELOG(RL_LOG_WARNING, "SHADER: [ID %i] Failed to link shader program", program);

        int maxLength = 0;
        glGetProgramiv(program, GL_INFO_LOG_LENGTH, &maxLength);

        if (maxLength > 0)
        {
            int length = 0;
            char *log = (char *)RL_CALLOC(maxLength, sizeof(char));
            glGetProgramInfoLog(program, maxLength, &length, log);
            TRACELOG(RL_LOG_WARNING, "SHADER: [ID %i] Link error: %s", program, log);
            RL_FREE(log);
        }

        glDeleteProgram(program);

        program = 0;
    }
    else
    {
        // Get the size of compiled shader program (not available on OpenGL ES 2.0)
        // NOTE: If GL_LINK_STATUS is GL_FALSE, program binary length is zero
        //GLint binarySize = 0;
        //glGetProgramiv(id, GL_PROGRAM_BINARY_LENGTH, &binarySize);

        TRACELOG(RL_LOG_INFO, "SHADER: [ID %i] Program shader loaded successfully", program);
    }
#endif
    return program;
}

// Unload shader program
void rlUnloadShaderProgram(unsigned int id)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    glDeleteProgram(id);

    TRACELOG(RL_LOG_INFO, "SHADER: [ID %i] Unloaded shader program data from VRAM (GPU)", id);
#endif
}

// Get shader location uniform
int rlGetLocationUniform(unsigned int shaderId, const char *uniformName)
{
    int location = -1;
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    location = glGetUniformLocation(shaderId, uniformName);

    //if (location == -1) TRACELOG(RL_LOG_WARNING, "SHADER: [ID %i] Failed to find shader uniform: %s", shaderId, uniformName);
    //else TRACELOG(RL_LOG_INFO, "SHADER: [ID %i] Shader uniform (%s) set at location: %i", shaderId, uniformName, location);
#endif
    return location;
}

// Get shader location attribute
int rlGetLocationAttrib(unsigned int shaderId, const char *attribName)
{
    int location = -1;
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    location = glGetAttribLocation(shaderId, attribName);

    //if (location == -1) TRACELOG(RL_LOG_WARNING, "SHADER: [ID %i] Failed to find shader attribute: %s", shaderId, attribName);
    //else TRACELOG(RL_LOG_INFO, "SHADER: [ID %i] Shader attribute (%s) set at location: %i", shaderId, attribName, location);
#endif
    return location;
}

// Set shader value uniform
void rlSetUniform(int locIndex, const void *value, int uniformType, int count)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    switch (uniformType)
    {
        case RL_SHADER_UNIFORM_FLOAT: glUniform1fv(locIndex, count, (float *)value); break;
        case RL_SHADER_UNIFORM_VEC2: glUniform2fv(locIndex, count, (float *)value); break;
        case RL_SHADER_UNIFORM_VEC3: glUniform3fv(locIndex, count, (float *)value); break;
        case RL_SHADER_UNIFORM_VEC4: glUniform4fv(locIndex, count, (float *)value); break;
        case RL_SHADER_UNIFORM_INT: glUniform1iv(locIndex, count, (int *)value); break;
        case RL_SHADER_UNIFORM_IVEC2: glUniform2iv(locIndex, count, (int *)value); break;
        case RL_SHADER_UNIFORM_IVEC3: glUniform3iv(locIndex, count, (int *)value); break;
        case RL_SHADER_UNIFORM_IVEC4: glUniform4iv(locIndex, count, (int *)value); break;
    #if !defined(GRAPHICS_API_OPENGL_ES2)
        case RL_SHADER_UNIFORM_UINT: glUniform1uiv(locIndex, count, (unsigned int *)value); break;
        case RL_SHADER_UNIFORM_UIVEC2: glUniform2uiv(locIndex, count, (unsigned int *)value); break;
        case RL_SHADER_UNIFORM_UIVEC3: glUniform3uiv(locIndex, count, (unsigned int *)value); break;
        case RL_SHADER_UNIFORM_UIVEC4: glUniform4uiv(locIndex, count, (unsigned int *)value); break;
    #endif
        case RL_SHADER_UNIFORM_SAMPLER2D: glUniform1iv(locIndex, count, (int *)value); break;
        default: TRACELOG(RL_LOG_WARNING, "SHADER: Failed to set uniform value, data type not recognized");

        // TODO: Support glUniform1uiv(), glUniform2uiv(), glUniform3uiv(), glUniform4uiv()
    }
#endif
}

// Set shader value attribute
void rlSetVertexAttributeDefault(int locIndex, const void *value, int attribType, int count)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    switch (attribType)
    {
        case RL_SHADER_ATTRIB_FLOAT: if (count == 1) glVertexAttrib1fv(locIndex, (float *)value); break;
        case RL_SHADER_ATTRIB_VEC2: if (count == 2) glVertexAttrib2fv(locIndex, (float *)value); break;
        case RL_SHADER_ATTRIB_VEC3: if (count == 3) glVertexAttrib3fv(locIndex, (float *)value); break;
        case RL_SHADER_ATTRIB_VEC4: if (count == 4) glVertexAttrib4fv(locIndex, (float *)value); break;
        default: TRACELOG(RL_LOG_WARNING, "SHADER: Failed to set attrib default value, data type not recognized");
    }
#endif
}

// Set shader value uniform matrix
void rlSetUniformMatrix(int locIndex, Matrix mat)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    float matfloat[16] = {
        mat.m0, mat.m1, mat.m2, mat.m3,
        mat.m4, mat.m5, mat.m6, mat.m7,
        mat.m8, mat.m9, mat.m10, mat.m11,
        mat.m12, mat.m13, mat.m14, mat.m15
    };
    glUniformMatrix4fv(locIndex, 1, false, matfloat);
#endif
}

// Set shader value uniform matrix
void rlSetUniformMatrices(int locIndex, const Matrix *matrices, int count)
{
#if defined(GRAPHICS_API_OPENGL_33)
    glUniformMatrix4fv(locIndex, count, true, (const float *)matrices);
#elif defined(GRAPHICS_API_OPENGL_ES2)
    // WARNING: WebGL does not support Matrix transpose ("true" parameter)
    // REF: https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/uniformMatrix
    glUniformMatrix4fv(locIndex, count, false, (const float *)matrices);
#endif
}

// Set shader value uniform sampler
void rlSetUniformSampler(int locIndex, unsigned int textureId)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    // Check if texture is already active
    for (int i = 0; i < RL_DEFAULT_BATCH_MAX_TEXTURE_UNITS; i++)
    {
        if (RLGL.State.activeTextureId[i] == textureId)
        {
            glUniform1i(locIndex, 1 + i);
            return;
        }
    }

    // Register a new active texture for the internal batch system
    // NOTE: Default texture is always activated as GL_TEXTURE0
    for (int i = 0; i < RL_DEFAULT_BATCH_MAX_TEXTURE_UNITS; i++)
    {
        if (RLGL.State.activeTextureId[i] == 0)
        {
            glUniform1i(locIndex, 1 + i);              // Activate new texture unit
            RLGL.State.activeTextureId[i] = textureId; // Save texture id for binding on drawing
            break;
        }
    }
#endif
}

// Set shader currently active (id and locations)
void rlSetShader(unsigned int id, int *locs)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    if (RLGL.State.currentShaderId != id)
    {
        rlDrawRenderBatch(RLGL.currentBatch);
        RLGL.State.currentShaderId = id;
        RLGL.State.currentShaderLocs = locs;
    }
#endif
}

// Load compute shader program
unsigned int rlLoadComputeShaderProgram(unsigned int shaderId)
{
    unsigned int program = 0;

#if defined(GRAPHICS_API_OPENGL_43)
    GLint success = 0;
    program = glCreateProgram();
    glAttachShader(program, shaderId);
    glLinkProgram(program);

    // NOTE: All uniform variables are intitialised to 0 when a program links

    glGetProgramiv(program, GL_LINK_STATUS, &success);

    if (success == GL_FALSE)
    {
        TRACELOG(RL_LOG_WARNING, "SHADER: [ID %i] Failed to link compute shader program", program);

        int maxLength = 0;
        glGetProgramiv(program, GL_INFO_LOG_LENGTH, &maxLength);

        if (maxLength > 0)
        {
            int length = 0;
            char *log = (char *)RL_CALLOC(maxLength, sizeof(char));
            glGetProgramInfoLog(program, maxLength, &length, log);
            TRACELOG(RL_LOG_WARNING, "SHADER: [ID %i] Link error: %s", program, log);
            RL_FREE(log);
        }

        glDeleteProgram(program);

        program = 0;
    }
    else
    {
        // Get the size of compiled shader program (not available on OpenGL ES 2.0)
        // NOTE: If GL_LINK_STATUS is GL_FALSE, program binary length is zero
        //GLint binarySize = 0;
        //glGetProgramiv(id, GL_PROGRAM_BINARY_LENGTH, &binarySize);

        TRACELOG(RL_LOG_INFO, "SHADER: [ID %i] Compute shader program loaded successfully", program);
    }
#else
    TRACELOG(RL_LOG_WARNING, "SHADER: Compute shaders not enabled. Define GRAPHICS_API_OPENGL_43");
#endif

    return program;
}

// Dispatch compute shader (equivalent to *draw* for graphics pilepine)
void rlComputeShaderDispatch(unsigned int groupX, unsigned int groupY, unsigned int groupZ)
{
#if defined(GRAPHICS_API_OPENGL_43)
    glDispatchCompute(groupX, groupY, groupZ);
#endif
}

// Load shader storage buffer object (SSBO)
unsigned int rlLoadShaderBuffer(unsigned int size, const void *data, int usageHint)
{
    unsigned int ssbo = 0;

#if defined(GRAPHICS_API_OPENGL_43)
    glGenBuffers(1, &ssbo);
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, ssbo);
    glBufferData(GL_SHADER_STORAGE_BUFFER, size, data, usageHint? usageHint : RL_STREAM_COPY);
    if (data == NULL) glClearBufferData(GL_SHADER_STORAGE_BUFFER, GL_R8UI, GL_RED_INTEGER, GL_UNSIGNED_BYTE, NULL);    // Clear buffer data to 0
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, 0);
#else
    TRACELOG(RL_LOG_WARNING, "SSBO: SSBO not enabled. Define GRAPHICS_API_OPENGL_43");
#endif

    return ssbo;
}

// Unload shader storage buffer object (SSBO)
void rlUnloadShaderBuffer(unsigned int ssboId)
{
#if defined(GRAPHICS_API_OPENGL_43)
    glDeleteBuffers(1, &ssboId);
#else
    TRACELOG(RL_LOG_WARNING, "SSBO: SSBO not enabled. Define GRAPHICS_API_OPENGL_43");
#endif

}

// Update SSBO buffer data
void rlUpdateShaderBuffer(unsigned int id, const void *data, unsigned int dataSize, unsigned int offset)
{
#if defined(GRAPHICS_API_OPENGL_43)
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, id);
    glBufferSubData(GL_SHADER_STORAGE_BUFFER, offset, dataSize, data);
#endif
}

// Get SSBO buffer size
unsigned int rlGetShaderBufferSize(unsigned int id)
{
#if defined(GRAPHICS_API_OPENGL_43)
    GLint64 size = 0;
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, id);
    glGetBufferParameteri64v(GL_SHADER_STORAGE_BUFFER, GL_BUFFER_SIZE, &size);
    return (size > 0)? (unsigned int)size : 0;
#else
    return 0;
#endif
}

// Read SSBO buffer data (GPU->CPU)
void rlReadShaderBuffer(unsigned int id, void *dest, unsigned int count, unsigned int offset)
{
#if defined(GRAPHICS_API_OPENGL_43)
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, id);
    glGetBufferSubData(GL_SHADER_STORAGE_BUFFER, offset, count, dest);
#endif
}

// Bind SSBO buffer
void rlBindShaderBuffer(unsigned int id, unsigned int index)
{
#if defined(GRAPHICS_API_OPENGL_43)
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, index, id);
#endif
}

// Copy SSBO buffer data
void rlCopyShaderBuffer(unsigned int destId, unsigned int srcId, unsigned int destOffset, unsigned int srcOffset, unsigned int count)
{
#if defined(GRAPHICS_API_OPENGL_43)
    glBindBuffer(GL_COPY_READ_BUFFER, srcId);
    glBindBuffer(GL_COPY_WRITE_BUFFER, destId);
    glCopyBufferSubData(GL_COPY_READ_BUFFER, GL_COPY_WRITE_BUFFER, srcOffset, destOffset, count);
#endif
}

// Bind image texture
void rlBindImageTexture(unsigned int id, unsigned int index, int format, bool readonly)
{
#if defined(GRAPHICS_API_OPENGL_43)
    unsigned int glInternalFormat = 0, glFormat = 0, glType = 0;

    rlGetGlTextureFormats(format, &glInternalFormat, &glFormat, &glType);
    glBindImageTexture(index, id, 0, 0, 0, readonly? GL_READ_ONLY : GL_READ_WRITE, glInternalFormat);
#else
    TRACELOG(RL_LOG_WARNING, "TEXTURE: Image texture binding not enabled. Define GRAPHICS_API_OPENGL_43");
#endif
}

// Matrix state management
//-----------------------------------------------------------------------------------------
// Get internal modelview matrix
Matrix rlGetMatrixModelview(void)
{
    Matrix matrix = rlMatrixIdentity();
#if defined(GRAPHICS_API_OPENGL_11)
    float mat[16];
    glGetFloatv(GL_MODELVIEW_MATRIX, mat);
    matrix.m0 = mat[0];
    matrix.m1 = mat[1];
    matrix.m2 = mat[2];
    matrix.m3 = mat[3];
    matrix.m4 = mat[4];
    matrix.m5 = mat[5];
    matrix.m6 = mat[6];
    matrix.m7 = mat[7];
    matrix.m8 = mat[8];
    matrix.m9 = mat[9];
    matrix.m10 = mat[10];
    matrix.m11 = mat[11];
    matrix.m12 = mat[12];
    matrix.m13 = mat[13];
    matrix.m14 = mat[14];
    matrix.m15 = mat[15];
#else
    matrix = RLGL.State.modelview;
#endif
    return matrix;
}

// Get internal projection matrix
Matrix rlGetMatrixProjection(void)
{
#if defined(GRAPHICS_API_OPENGL_11)
    float mat[16];
    glGetFloatv(GL_PROJECTION_MATRIX,mat);
    Matrix m;
    m.m0 = mat[0];
    m.m1 = mat[1];
    m.m2 = mat[2];
    m.m3 = mat[3];
    m.m4 = mat[4];
    m.m5 = mat[5];
    m.m6 = mat[6];
    m.m7 = mat[7];
    m.m8 = mat[8];
    m.m9 = mat[9];
    m.m10 = mat[10];
    m.m11 = mat[11];
    m.m12 = mat[12];
    m.m13 = mat[13];
    m.m14 = mat[14];
    m.m15 = mat[15];
    return m;
#else
    return RLGL.State.projection;
#endif
}

// Get internal accumulated transform matrix
Matrix rlGetMatrixTransform(void)
{
    Matrix mat = rlMatrixIdentity();
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    // TODO: Consider possible transform matrices in the RLGL.State.stack
    // Is this the right order? or should we start with the first stored matrix instead of the last one?
    //Matrix matStackTransform = rlMatrixIdentity();
    //for (int i = RLGL.State.stackCounter; i > 0; i--) matStackTransform = rlMatrixMultiply(RLGL.State.stack[i], matStackTransform);
    mat = RLGL.State.transform;
#endif
    return mat;
}

// Get internal projection matrix for stereo render (selected eye)
Matrix rlGetMatrixProjectionStereo(int eye)
{
    Matrix mat = rlMatrixIdentity();
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    mat = RLGL.State.projectionStereo[eye];
#endif
    return mat;
}

// Get internal view offset matrix for stereo render (selected eye)
Matrix rlGetMatrixViewOffsetStereo(int eye)
{
    Matrix mat = rlMatrixIdentity();
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    mat = RLGL.State.viewOffsetStereo[eye];
#endif
    return mat;
}

// Set a custom modelview matrix (replaces internal modelview matrix)
void rlSetMatrixModelview(Matrix view)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    RLGL.State.modelview = view;
#endif
}

// Set a custom projection matrix (replaces internal projection matrix)
void rlSetMatrixProjection(Matrix projection)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    RLGL.State.projection = projection;
#endif
}

// Set eyes projection matrices for stereo rendering
void rlSetMatrixProjectionStereo(Matrix right, Matrix left)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    RLGL.State.projectionStereo[0] = right;
    RLGL.State.projectionStereo[1] = left;
#endif
}

// Set eyes view offsets matrices for stereo rendering
void rlSetMatrixViewOffsetStereo(Matrix right, Matrix left)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    RLGL.State.viewOffsetStereo[0] = right;
    RLGL.State.viewOffsetStereo[1] = left;
#endif
}

// Load and draw a quad in NDC
void rlLoadDrawQuad(void)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    unsigned int quadVAO = 0;
    unsigned int quadVBO = 0;

    float vertices[] = {
         // Positions         Texcoords
        -1.0f,  1.0f, 0.0f,   0.0f, 1.0f,
        -1.0f, -1.0f, 0.0f,   0.0f, 0.0f,
         1.0f,  1.0f, 0.0f,   1.0f, 1.0f,
         1.0f, -1.0f, 0.0f,   1.0f, 0.0f,
    };

    // Gen VAO to contain VBO
    glGenVertexArrays(1, &quadVAO);
    glBindVertexArray(quadVAO);

    // Gen and fill vertex buffer (VBO)
    glGenBuffers(1, &quadVBO);
    glBindBuffer(GL_ARRAY_BUFFER, quadVBO);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), &vertices, GL_STATIC_DRAW);

    // Bind vertex attributes (position, texcoords)
    glEnableVertexAttribArray(RL_DEFAULT_SHADER_ATTRIB_LOCATION_POSITION);
    glVertexAttribPointer(RL_DEFAULT_SHADER_ATTRIB_LOCATION_POSITION, 3, GL_FLOAT, GL_FALSE, 5*sizeof(float), (void *)0); // Positions
    glEnableVertexAttribArray(RL_DEFAULT_SHADER_ATTRIB_LOCATION_TEXCOORD);
    glVertexAttribPointer(RL_DEFAULT_SHADER_ATTRIB_LOCATION_TEXCOORD, 2, GL_FLOAT, GL_FALSE, 5*sizeof(float), (void *)(3*sizeof(float))); // Texcoords

    // Draw quad
    glBindVertexArray(quadVAO);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
    glBindVertexArray(0);

    // Delete buffers (VBO and VAO)
    glDeleteBuffers(1, &quadVBO);
    glDeleteVertexArrays(1, &quadVAO);
#endif
}

// Load and draw a cube in NDC
void rlLoadDrawCube(void)
{
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
    unsigned int cubeVAO = 0;
    unsigned int cubeVBO = 0;

    float vertices[] = {
         // Positions          Normals               Texcoords
        -1.0f, -1.0f, -1.0f,   0.0f,  0.0f, -1.0f,   0.0f, 0.0f,
         1.0f,  1.0f, -1.0f,   0.0f,  0.0f, -1.0f,   1.0f, 1.0f,
         1.0f, -1.0f, -1.0f,   0.0f,  0.0f, -1.0f,   1.0f, 0.0f,
         1.0f,  1.0f, -1.0f,   0.0f,  0.0f, -1.0f,   1.0f, 1.0f,
        -1.0f, -1.0f, -1.0f,   0.0f,  0.0f, -1.0f,   0.0f, 0.0f,
        -1.0f,  1.0f, -1.0f,   0.0f,  0.0f, -1.0f,   0.0f, 1.0f,
        -1.0f, -1.0f,  1.0f,   0.0f,  0.0f,  1.0f,   0.0f, 0.0f,
         1.0f, -1.0f,  1.0f,   0.0f,  0.0f,  1.0f,   1.0f, 0.0f,
         1.0f,  1.0f,  1.0f,   0.0f,  0.0f,  1.0f,   1.0f, 1.0f,
         1.0f,  1.0f,  1.0f,   0.0f,  0.0f,  1.0f,   1.0f, 1.0f,
        -1.0f,  1.0f,  1.0f,   0.0f,  0.0f,  1.0f,   0.0f, 1.0f,
        -1.0f, -1.0f,  1.0f,   0.0f,  0.0f,  1.0f,   0.0f, 0.0f,
        -1.0f,  1.0f,  1.0f,  -1.0f,  0.0f,  0.0f,   1.0f, 0.0f,
        -1.0f,  1.0f, -1.0f,  -1.0f,  0.0f,  0.0f,   1.0f, 1.0f,
        -1.0f, -1.0f, -1.0f,  -1.0f,  0.0f,  0.0f,   0.0f, 1.0f,
        -1.0f, -1.0f, -1.0f,  -1.0f,  0.0f,  0.0f,   0.0f, 1.0f,
        -1.0f, -1.0f,  1.0f,  -1.0f,  0.0f,  0.0f,   0.0f, 0.0f,
        -1.0f,  1.0f,  1.0f,  -1.0f,  0.0f,  0.0f,   1.0f, 0.0f,
         1.0f,  1.0f,  1.0f,   1.0f,  0.0f,  0.0f,   1.0f, 0.0f,
         1.0f, -1.0f, -1.0f,   1.0f,  0.0f,  0.0f,   0.0f, 1.0f,
         1.0f,  1.0f, -1.0f,   1.0f,  0.0f,  0.0f,   1.0f, 1.0f,
         1.0f, -1.0f, -1.0f,   1.0f,  0.0f,  0.0f,   0.0f, 1.0f,
         1.0f,  1.0f,  1.0f,   1.0f,  0.0f,  0.0f,   1.0f, 0.0f,
         1.0f, -1.0f,  1.0f,   1.0f,  0.0f,  0.0f,   0.0f, 0.0f,
        -1.0f, -1.0f, -1.0f,   0.0f, -1.0f,  0.0f,   0.0f, 1.0f,
         1.0f, -1.0f, -1.0f,   0.0f, -1.0f,  0.0f,   1.0f, 1.0f,
         1.0f, -1.0f,  1.0f,   0.0f, -1.0f,  0.0f,   1.0f, 0.0f,
         1.0f, -1.0f,  1.0f,   0.0f, -1.0f,  0.0f,   1.0f, 0.0f,
        -1.0f, -1.0f,  1.0f,   0.0f, -1.0f,  0.0f,   0.0f, 0.0f,
        -1.0f, -1.0f, -1.0f,   0.0f, -1.0f,  0.0f,   0.0f, 1.0f,
        -1.0f,  1.0f, -1.0f,   0.0f,  1.0f,  0.0f,   0.0f, 1.0f,
         1.0f,  1.0f,  1.0f,   0.0f,  1.0f,  0.0f,   1.0f, 0.0f,
         1.0f,  1.0f, -1.0f,   0.0f,  1.0f,  0.0f,   1.0f, 1.0f,
         1.0f,  1.0f,  1.0f,   0.0f,  1.0f,  0.0f,   1.0f, 0.0f,
        -1.0f,  1.0f, -1.0f,   0.0f,  1.0f,  0.0f,   0.0f, 1.0f,
        -1.0f,  1.0f,  1.0f,   0.0f,  1.0f,  0.0f,   0.0f, 0.0f
    };

    // Gen VAO to contain VBO
    glGenVertexArrays(1, &cubeVAO);
    glBindVertexArray(cubeVAO);

    // Gen and fill vertex buffer (VBO)
    glGenBuffers(1, &cubeVBO);
    glBindBuffer(GL_ARRAY_BUFFER, cubeVBO);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

    // Bind vertex attributes (position, normals, texcoords)
    glBindVertexArray(cubeVAO);
    glEnableVertexAttribArray(RL_DEFAULT_SHADER_ATTRIB_LOCATION_POSITION);
    glVertexAttribPointer(RL_DEFAULT_SHADER_ATTRIB_LOCATION_POSITION, 3, GL_FLOAT, GL_FALSE, 8*sizeof(float), (void *)0); // Positions
    glEnableVertexAttribArray(RL_DEFAULT_SHADER_ATTRIB_LOCATION_NORMAL);
    glVertexAttribPointer(RL_DEFAULT_SHADER_ATTRIB_LOCATION_NORMAL, 3, GL_FLOAT, GL_FALSE, 8*sizeof(float), (void *)(3*sizeof(float))); // Normals
    glEnableVertexAttribArray(RL_DEFAULT_SHADER_ATTRIB_LOCATION_TEXCOORD);
    glVertexAttribPointer(RL_DEFAULT_SHADER_ATTRIB_LOCATION_TEXCOORD, 2, GL_FLOAT, GL_FALSE, 8*sizeof(float), (void *)(6*sizeof(float))); // Texcoords
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);

    // Draw cube
    glBindVertexArray(cubeVAO);
    glDrawArrays(GL_TRIANGLES, 0, 36);
    glBindVertexArray(0);

    // Delete VBO and VAO
    glDeleteBuffers(1, &cubeVBO);
    glDeleteVertexArrays(1, &cubeVAO);
#endif
}

// Get name string for pixel format
const char *rlGetPixelFormatName(unsigned int format)
{
    switch (format)
    {
        case RL_PIXELFORMAT_UNCOMPRESSED_GRAYSCALE: return "GRAYSCALE"; break;         // 8 bit per pixel (no alpha)
        case RL_PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA: return "GRAY_ALPHA"; break;       // 8*2 bpp (2 channels)
        case RL_PIXELFORMAT_UNCOMPRESSED_R5G6B5: return "R5G6B5"; break;               // 16 bpp
        case RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8: return "R8G8B8"; break;               // 24 bpp
        case RL_PIXELFORMAT_UNCOMPRESSED_R5G5B5A1: return "R5G5B5A1"; break;           // 16 bpp (1 bit alpha)
        case RL_PIXELFORMAT_UNCOMPRESSED_R4G4B4A4: return "R4G4B4A4"; break;           // 16 bpp (4 bit alpha)
        case RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8A8: return "R8G8B8A8"; break;           // 32 bpp
        case RL_PIXELFORMAT_UNCOMPRESSED_R32: return "R32"; break;                     // 32 bpp (1 channel - float)
        case RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32: return "R32G32B32"; break;         // 32*3 bpp (3 channels - float)
        case RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32A32: return "R32G32B32A32"; break;   // 32*4 bpp (4 channels - float)
        case RL_PIXELFORMAT_UNCOMPRESSED_R16: return "R16"; break;                     // 16 bpp (1 channel - half float)
        case RL_PIXELFORMAT_UNCOMPRESSED_R16G16B16: return "R16G16B16"; break;         // 16*3 bpp (3 channels - half float)
        case RL_PIXELFORMAT_UNCOMPRESSED_R16G16B16A16: return "R16G16B16A16"; break;   // 16*4 bpp (4 channels - half float)
        case RL_PIXELFORMAT_COMPRESSED_DXT1_RGB: return "DXT1_RGB"; break;             // 4 bpp (no alpha)
        case RL_PIXELFORMAT_COMPRESSED_DXT1_RGBA: return "DXT1_RGBA"; break;           // 4 bpp (1 bit alpha)
        case RL_PIXELFORMAT_COMPRESSED_DXT3_RGBA: return "DXT3_RGBA"; break;           // 8 bpp
        case RL_PIXELFORMAT_COMPRESSED_DXT5_RGBA: return "DXT5_RGBA"; break;           // 8 bpp
        case RL_PIXELFORMAT_COMPRESSED_ETC1_RGB: return "ETC1_RGB"; break;             // 4 bpp
        case RL_PIXELFORMAT_COMPRESSED_ETC2_RGB: return "ETC2_RGB"; break;             // 4 bpp
        case RL_PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA: return "ETC2_RGBA"; break;       // 8 bpp
        case RL_PIXELFORMAT_COMPRESSED_PVRT_RGB: return "PVRT_RGB"; break;             // 4 bpp
        case RL_PIXELFORMAT_COMPRESSED_PVRT_RGBA: return "PVRT_RGBA"; break;           // 4 bpp
        case RL_PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA: return "ASTC_4x4_RGBA"; break;   // 8 bpp
        case RL_PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA: return "ASTC_8x8_RGBA"; break;   // 2 bpp
        default: return "UNKNOWN"; break;
    }
}

//----------------------------------------------------------------------------------
// Module specific Functions Definition
//----------------------------------------------------------------------------------
#if defined(GRAPHICS_API_OPENGL_33) || defined(GRAPHICS_API_OPENGL_ES2)
// Load default shader (just vertex positioning and texture coloring)
// NOTE: This shader program is used for internal buffers
// NOTE: Loaded: RLGL.State.defaultShaderId, RLGL.State.defaultShaderLocs
static void rlLoadShaderDefault(void)
{
    RLGL.State.defaultShaderLocs = (int *)RL_CALLOC(RL_MAX_SHADER_LOCATIONS, sizeof(int));

    // NOTE: All locations must be reseted to -1 (no location)
    for (int i = 0; i < RL_MAX_SHADER_LOCATIONS; i++) RLGL.State.defaultShaderLocs[i] = -1;

    // Vertex shader directly defined, no external file required
    const char *defaultVShaderCode =
#if defined(GRAPHICS_API_OPENGL_21)
    "#version 120                       \n"
    "attribute vec3 vertexPosition;     \n"
    "attribute vec2 vertexTexCoord;     \n"
    "attribute vec4 vertexColor;        \n"
    "varying vec2 fragTexCoord;         \n"
    "varying vec4 fragColor;            \n"
#elif defined(GRAPHICS_API_OPENGL_33)
    "#version 330                       \n"
    "in vec3 vertexPosition;            \n"
    "in vec2 vertexTexCoord;            \n"
    "in vec4 vertexColor;               \n"
    "out vec2 fragTexCoord;             \n"
    "out vec4 fragColor;                \n"
#endif

#if defined(GRAPHICS_API_OPENGL_ES3)
    "#version 300 es                    \n"
    "precision mediump float;           \n"     // Precision required for OpenGL ES3 (WebGL 2) (on some browsers)
    "in vec3 vertexPosition;            \n"
    "in vec2 vertexTexCoord;            \n"
    "in vec4 vertexColor;               \n"
    "out vec2 fragTexCoord;             \n"
    "out vec4 fragColor;                \n"
#elif defined(GRAPHICS_API_OPENGL_ES2)
    "#version 100                       \n"
    "precision mediump float;           \n"     // Precision required for OpenGL ES2 (WebGL) (on some browsers)
    "attribute vec3 vertexPosition;     \n"
    "attribute vec2 vertexTexCoord;     \n"
    "attribute vec4 vertexColor;        \n"
    "varying vec2 fragTexCoord;         \n"
    "varying vec4 fragColor;            \n"
#endif

    "uniform mat4 mvp;                  \n"
    "void main()                        \n"
    "{                                  \n"
    "    fragTexCoord = vertexTexCoord; \n"
    "    fragColor = vertexColor;       \n"
    "    gl_Position = mvp*vec4(vertexPosition, 1.0); \n"
    "}                                  \n";

    // Fragment shader directly defined, no external file required
    const char *defaultFShaderCode =
#if defined(GRAPHICS_API_OPENGL_21)
    "#version 120                       \n"
    "varying vec2 fragTexCoord;         \n"
    "varying vec4 fragColor;            \n"
    "uniform sampler2D texture0;        \n"
    "uniform vec4 colDiffuse;           \n"
    "void main()                        \n"
    "{                                  \n"
    "    vec4 texelColor = texture2D(texture0, fragTexCoord); \n"
    "    gl_FragColor = texelColor*colDiffuse*fragColor;      \n"
    "}                                  \n";
#elif defined(GRAPHICS_API_OPENGL_33)
    "#version 330       \n"
    "in vec2 fragTexCoord;              \n"
    "in vec4 fragColor;                 \n"
    "out vec4 finalColor;               \n"
    "uniform sampler2D texture0;        \n"
    "uniform vec4 colDiffuse;           \n"
    "void main()                        \n"
    "{                                  \n"
    "    vec4 texelColor = texture(texture0, fragTexCoord);   \n"
    "    finalColor = texelColor*colDiffuse*fragColor;        \n"
    "}                                  \n";
#endif

#if defined(GRAPHICS_API_OPENGL_ES3)
    "#version 300 es                    \n"
    "precision mediump float;           \n"     // Precision required for OpenGL ES3 (WebGL 2)
    "in vec2 fragTexCoord;              \n"
    "in vec4 fragColor;                 \n"
    "out vec4 finalColor;               \n"
    "uniform sampler2D texture0;        \n"
    "uniform vec4 colDiffuse;           \n"
    "void main()                        \n"
    "{                                  \n"
    "    vec4 texelColor = texture(texture0, fragTexCoord);   \n"
    "    finalColor = texelColor*colDiffuse*fragColor;        \n"
    "}                                  \n";
#elif defined(GRAPHICS_API_OPENGL_ES2)
    "#version 100                       \n"
    "precision mediump float;           \n"     // Precision required for OpenGL ES2 (WebGL)
    "varying vec2 fragTexCoord;         \n"
    "varying vec4 fragColor;            \n"
    "uniform sampler2D texture0;        \n"
    "uniform vec4 colDiffuse;           \n"
    "void main()                        \n"
    "{                                  \n"
    "    vec4 texelColor = texture2D(texture0, fragTexCoord); \n"
    "    gl_FragColor = texelColor*colDiffuse*fragColor;      \n"
    "}                                  \n";
#endif

    // NOTE: Compiled vertex/fragment shaders are not deleted,
    // they are kept for re-use as default shaders in case some shader loading fails
    RLGL.State.defaultVShaderId = rlCompileShader(defaultVShaderCode, GL_VERTEX_SHADER);     // Compile default vertex shader
    RLGL.State.defaultFShaderId = rlCompileShader(defaultFShaderCode, GL_FRAGMENT_SHADER);   // Compile default fragment shader

    RLGL.State.defaultShaderId = rlLoadShaderProgram(RLGL.State.defaultVShaderId, RLGL.State.defaultFShaderId);

    if (RLGL.State.defaultShaderId > 0)
    {
        TRACELOG(RL_LOG_INFO, "SHADER: [ID %i] Default shader loaded successfully", RLGL.State.defaultShaderId);

        // Set default shader locations: attributes locations
        RLGL.State.defaultShaderLocs[RL_SHADER_LOC_VERTEX_POSITION] = glGetAttribLocation(RLGL.State.defaultShaderId, RL_DEFAULT_SHADER_ATTRIB_NAME_POSITION);
        RLGL.State.defaultShaderLocs[RL_SHADER_LOC_VERTEX_TEXCOORD01] = glGetAttribLocation(RLGL.State.defaultShaderId, RL_DEFAULT_SHADER_ATTRIB_NAME_TEXCOORD);
        RLGL.State.defaultShaderLocs[RL_SHADER_LOC_VERTEX_COLOR] = glGetAttribLocation(RLGL.State.defaultShaderId, RL_DEFAULT_SHADER_ATTRIB_NAME_COLOR);

        // Set default shader locations: uniform locations
        RLGL.State.defaultShaderLocs[RL_SHADER_LOC_MATRIX_MVP] = glGetUniformLocation(RLGL.State.defaultShaderId, RL_DEFAULT_SHADER_UNIFORM_NAME_MVP);
        RLGL.State.defaultShaderLocs[RL_SHADER_LOC_COLOR_DIFFUSE] = glGetUniformLocation(RLGL.State.defaultShaderId, RL_DEFAULT_SHADER_UNIFORM_NAME_COLOR);
        RLGL.State.defaultShaderLocs[RL_SHADER_LOC_MAP_DIFFUSE] = glGetUniformLocation(RLGL.State.defaultShaderId, RL_DEFAULT_SHADER_SAMPLER2D_NAME_TEXTURE0);
    }
    else TRACELOG(RL_LOG_WARNING, "SHADER: [ID %i] Failed to load default shader", RLGL.State.defaultShaderId);
}

// Unload default shader
// NOTE: Unloads: RLGL.State.defaultShaderId, RLGL.State.defaultShaderLocs
static void rlUnloadShaderDefault(void)
{
    glUseProgram(0);

    glDetachShader(RLGL.State.defaultShaderId, RLGL.State.defaultVShaderId);
    glDetachShader(RLGL.State.defaultShaderId, RLGL.State.defaultFShaderId);
    glDeleteShader(RLGL.State.defaultVShaderId);
    glDeleteShader(RLGL.State.defaultFShaderId);

    glDeleteProgram(RLGL.State.defaultShaderId);

    RL_FREE(RLGL.State.defaultShaderLocs);

    TRACELOG(RL_LOG_INFO, "SHADER: [ID %i] Default shader unloaded successfully", RLGL.State.defaultShaderId);
}

#if defined(RLGL_SHOW_GL_DETAILS_INFO)
// Get compressed format official GL identifier name
static const char *rlGetCompressedFormatName(int format)
{
    switch (format)
    {
        // GL_EXT_texture_compression_s3tc
        case 0x83F0: return "GL_COMPRESSED_RGB_S3TC_DXT1_EXT"; break;
        case 0x83F1: return "GL_COMPRESSED_RGBA_S3TC_DXT1_EXT"; break;
        case 0x83F2: return "GL_COMPRESSED_RGBA_S3TC_DXT3_EXT"; break;
        case 0x83F3: return "GL_COMPRESSED_RGBA_S3TC_DXT5_EXT"; break;
        // GL_3DFX_texture_compression_FXT1
        case 0x86B0: return "GL_COMPRESSED_RGB_FXT1_3DFX"; break;
        case 0x86B1: return "GL_COMPRESSED_RGBA_FXT1_3DFX"; break;
        // GL_IMG_texture_compression_pvrtc
        case 0x8C00: return "GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG"; break;
        case 0x8C01: return "GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG"; break;
        case 0x8C02: return "GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG"; break;
        case 0x8C03: return "GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG"; break;
        // GL_OES_compressed_ETC1_RGB8_texture
        case 0x8D64: return "GL_ETC1_RGB8_OES"; break;
        // GL_ARB_texture_compression_rgtc
        case 0x8DBB: return "GL_COMPRESSED_RED_RGTC1"; break;
        case 0x8DBC: return "GL_COMPRESSED_SIGNED_RED_RGTC1"; break;
        case 0x8DBD: return "GL_COMPRESSED_RG_RGTC2"; break;
        case 0x8DBE: return "GL_COMPRESSED_SIGNED_RG_RGTC2"; break;
        // GL_ARB_texture_compression_bptc
        case 0x8E8C: return "GL_COMPRESSED_RGBA_BPTC_UNORM_ARB"; break;
        case 0x8E8D: return "GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM_ARB"; break;
        case 0x8E8E: return "GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT_ARB"; break;
        case 0x8E8F: return "GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT_ARB"; break;
        // GL_ARB_ES3_compatibility
        case 0x9274: return "GL_COMPRESSED_RGB8_ETC2"; break;
        case 0x9275: return "GL_COMPRESSED_SRGB8_ETC2"; break;
        case 0x9276: return "GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2"; break;
        case 0x9277: return "GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2"; break;
        case 0x9278: return "GL_COMPRESSED_RGBA8_ETC2_EAC"; break;
        case 0x9279: return "GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC"; break;
        case 0x9270: return "GL_COMPRESSED_R11_EAC"; break;
        case 0x9271: return "GL_COMPRESSED_SIGNED_R11_EAC"; break;
        case 0x9272: return "GL_COMPRESSED_RG11_EAC"; break;
        case 0x9273: return "GL_COMPRESSED_SIGNED_RG11_EAC"; break;
        // GL_KHR_texture_compression_astc_hdr
        case 0x93B0: return "GL_COMPRESSED_RGBA_ASTC_4x4_KHR"; break;
        case 0x93B1: return "GL_COMPRESSED_RGBA_ASTC_5x4_KHR"; break;
        case 0x93B2: return "GL_COMPRESSED_RGBA_ASTC_5x5_KHR"; break;
        case 0x93B3: return "GL_COMPRESSED_RGBA_ASTC_6x5_KHR"; break;
        case 0x93B4: return "GL_COMPRESSED_RGBA_ASTC_6x6_KHR"; break;
        case 0x93B5: return "GL_COMPRESSED_RGBA_ASTC_8x5_KHR"; break;
        case 0x93B6: return "GL_COMPRESSED_RGBA_ASTC_8x6_KHR"; break;
        case 0x93B7: return "GL_COMPRESSED_RGBA_ASTC_8x8_KHR"; break;
        case 0x93B8: return "GL_COMPRESSED_RGBA_ASTC_10x5_KHR"; break;
        case 0x93B9: return "GL_COMPRESSED_RGBA_ASTC_10x6_KHR"; break;
        case 0x93BA: return "GL_COMPRESSED_RGBA_ASTC_10x8_KHR"; break;
        case 0x93BB: return "GL_COMPRESSED_RGBA_ASTC_10x10_KHR"; break;
        case 0x93BC: return "GL_COMPRESSED_RGBA_ASTC_12x10_KHR"; break;
        case 0x93BD: return "GL_COMPRESSED_RGBA_ASTC_12x12_KHR"; break;
        case 0x93D0: return "GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR"; break;
        case 0x93D1: return "GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR"; break;
        case 0x93D2: return "GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR"; break;
        case 0x93D3: return "GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR"; break;
        case 0x93D4: return "GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR"; break;
        case 0x93D5: return "GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR"; break;
        case 0x93D6: return "GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR"; break;
        case 0x93D7: return "GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR"; break;
        case 0x93D8: return "GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR"; break;
        case 0x93D9: return "GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR"; break;
        case 0x93DA: return "GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR"; break;
        case 0x93DB: return "GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR"; break;
        case 0x93DC: return "GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR"; break;
        case 0x93DD: return "GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR"; break;
        default: return "GL_COMPRESSED_UNKNOWN"; break;
    }
}
#endif  // RLGL_SHOW_GL_DETAILS_INFO

#endif  // GRAPHICS_API_OPENGL_33 || GRAPHICS_API_OPENGL_ES2

// Get pixel data size in bytes (image or texture)
// NOTE: Size depends on pixel format
static int rlGetPixelDataSize(int width, int height, int format)
{
    int dataSize = 0;       // Size in bytes
    int bpp = 0;            // Bits per pixel

    switch (format)
    {
        case RL_PIXELFORMAT_UNCOMPRESSED_GRAYSCALE: bpp = 8; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA:
        case RL_PIXELFORMAT_UNCOMPRESSED_R5G6B5:
        case RL_PIXELFORMAT_UNCOMPRESSED_R5G5B5A1:
        case RL_PIXELFORMAT_UNCOMPRESSED_R4G4B4A4: bpp = 16; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8A8: bpp = 32; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8: bpp = 24; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R32: bpp = 32; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32: bpp = 32*3; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32A32: bpp = 32*4; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R16: bpp = 16; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R16G16B16: bpp = 16*3; break;
        case RL_PIXELFORMAT_UNCOMPRESSED_R16G16B16A16: bpp = 16*4; break;
        case RL_PIXELFORMAT_COMPRESSED_DXT1_RGB:
        case RL_PIXELFORMAT_COMPRESSED_DXT1_RGBA:
        case RL_PIXELFORMAT_COMPRESSED_ETC1_RGB:
        case RL_PIXELFORMAT_COMPRESSED_ETC2_RGB:
        case RL_PIXELFORMAT_COMPRESSED_PVRT_RGB:
        case RL_PIXELFORMAT_COMPRESSED_PVRT_RGBA: bpp = 4; break;
        case RL_PIXELFORMAT_COMPRESSED_DXT3_RGBA:
        case RL_PIXELFORMAT_COMPRESSED_DXT5_RGBA:
        case RL_PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA:
        case RL_PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA: bpp = 8; break;
        case RL_PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA: bpp = 2; break;
        default: break;
    }

    double bytesPerPixel = (double)bpp/8.0;
    dataSize = (int)(bytesPerPixel*width*height); // Total data size in bytes

    // Most compressed formats works on 4x4 blocks,
    // if texture is smaller, minimum dataSize is 8 or 16
    if ((width < 4) && (height < 4))
    {
        if ((format >= RL_PIXELFORMAT_COMPRESSED_DXT1_RGB) && (format < RL_PIXELFORMAT_COMPRESSED_DXT3_RGBA)) dataSize = 8;
        else if ((format >= RL_PIXELFORMAT_COMPRESSED_DXT3_RGBA) && (format < RL_PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA)) dataSize = 16;
    }

    return dataSize;
}

// Auxiliar math functions

// Get float array of matrix data
static rl_float16 rlMatrixToFloatV(Matrix mat)
{
    rl_float16 result = { 0 };

    result.v[0] = mat.m0;
    result.v[1] = mat.m1;
    result.v[2] = mat.m2;
    result.v[3] = mat.m3;
    result.v[4] = mat.m4;
    result.v[5] = mat.m5;
    result.v[6] = mat.m6;
    result.v[7] = mat.m7;
    result.v[8] = mat.m8;
    result.v[9] = mat.m9;
    result.v[10] = mat.m10;
    result.v[11] = mat.m11;
    result.v[12] = mat.m12;
    result.v[13] = mat.m13;
    result.v[14] = mat.m14;
    result.v[15] = mat.m15;

    return result;
}

// Get identity matrix
static Matrix rlMatrixIdentity(void)
{
    Matrix result = {
        1.0f, 0.0f, 0.0f, 0.0f,
        0.0f, 1.0f, 0.0f, 0.0f,
        0.0f, 0.0f, 1.0f, 0.0f,
        0.0f, 0.0f, 0.0f, 1.0f
    };

    return result;
}

// Get two matrix multiplication
// NOTE: When multiplying matrices... the order matters!
static Matrix rlMatrixMultiply(Matrix left, Matrix right)
{
    Matrix result = { 0 };

    result.m0 = left.m0*right.m0 + left.m1*right.m4 + left.m2*right.m8 + left.m3*right.m12;
    result.m1 = left.m0*right.m1 + left.m1*right.m5 + left.m2*right.m9 + left.m3*right.m13;
    result.m2 = left.m0*right.m2 + left.m1*right.m6 + left.m2*right.m10 + left.m3*right.m14;
    result.m3 = left.m0*right.m3 + left.m1*right.m7 + left.m2*right.m11 + left.m3*right.m15;
    result.m4 = left.m4*right.m0 + left.m5*right.m4 + left.m6*right.m8 + left.m7*right.m12;
    result.m5 = left.m4*right.m1 + left.m5*right.m5 + left.m6*right.m9 + left.m7*right.m13;
    result.m6 = left.m4*right.m2 + left.m5*right.m6 + left.m6*right.m10 + left.m7*right.m14;
    result.m7 = left.m4*right.m3 + left.m5*right.m7 + left.m6*right.m11 + left.m7*right.m15;
    result.m8 = left.m8*right.m0 + left.m9*right.m4 + left.m10*right.m8 + left.m11*right.m12;
    result.m9 = left.m8*right.m1 + left.m9*right.m5 + left.m10*right.m9 + left.m11*right.m13;
    result.m10 = left.m8*right.m2 + left.m9*right.m6 + left.m10*right.m10 + left.m11*right.m14;
    result.m11 = left.m8*right.m3 + left.m9*right.m7 + left.m10*right.m11 + left.m11*right.m15;
    result.m12 = left.m12*right.m0 + left.m13*right.m4 + left.m14*right.m8 + left.m15*right.m12;
    result.m13 = left.m12*right.m1 + left.m13*right.m5 + left.m14*right.m9 + left.m15*right.m13;
    result.m14 = left.m12*right.m2 + left.m13*right.m6 + left.m14*right.m10 + left.m15*right.m14;
    result.m15 = left.m12*right.m3 + left.m13*right.m7 + left.m14*right.m11 + left.m15*right.m15;

    return result;
}

// Transposes provided matrix
static Matrix rlMatrixTranspose(Matrix mat)
{
    Matrix result = { 0 };

    result.m0 = mat.m0;
    result.m1 = mat.m4;
    result.m2 = mat.m8;
    result.m3 = mat.m12;
    result.m4 = mat.m1;
    result.m5 = mat.m5;
    result.m6 = mat.m9;
    result.m7 = mat.m13;
    result.m8 = mat.m2;
    result.m9 = mat.m6;
    result.m10 = mat.m10;
    result.m11 = mat.m14;
    result.m12 = mat.m3;
    result.m13 = mat.m7;
    result.m14 = mat.m11;
    result.m15 = mat.m15;

    return result;
}

// Invert provided matrix
static Matrix rlMatrixInvert(Matrix mat)
{
    Matrix result = { 0 };

    // Cache the matrix values (speed optimization)
    float a00 = mat.m0, a01 = mat.m1, a02 = mat.m2, a03 = mat.m3;
    float a10 = mat.m4, a11 = mat.m5, a12 = mat.m6, a13 = mat.m7;
    float a20 = mat.m8, a21 = mat.m9, a22 = mat.m10, a23 = mat.m11;
    float a30 = mat.m12, a31 = mat.m13, a32 = mat.m14, a33 = mat.m15;

    float b00 = a00*a11 - a01*a10;
    float b01 = a00*a12 - a02*a10;
    float b02 = a00*a13 - a03*a10;
    float b03 = a01*a12 - a02*a11;
    float b04 = a01*a13 - a03*a11;
    float b05 = a02*a13 - a03*a12;
    float b06 = a20*a31 - a21*a30;
    float b07 = a20*a32 - a22*a30;
    float b08 = a20*a33 - a23*a30;
    float b09 = a21*a32 - a22*a31;
    float b10 = a21*a33 - a23*a31;
    float b11 = a22*a33 - a23*a32;

    // Calculate the invert determinant (inlined to avoid double-caching)
    float invDet = 1.0f/(b00*b11 - b01*b10 + b02*b09 + b03*b08 - b04*b07 + b05*b06);

    result.m0 = (a11*b11 - a12*b10 + a13*b09)*invDet;
    result.m1 = (-a01*b11 + a02*b10 - a03*b09)*invDet;
    result.m2 = (a31*b05 - a32*b04 + a33*b03)*invDet;
    result.m3 = (-a21*b05 + a22*b04 - a23*b03)*invDet;
    result.m4 = (-a10*b11 + a12*b08 - a13*b07)*invDet;
    result.m5 = (a00*b11 - a02*b08 + a03*b07)*invDet;
    result.m6 = (-a30*b05 + a32*b02 - a33*b01)*invDet;
    result.m7 = (a20*b05 - a22*b02 + a23*b01)*invDet;
    result.m8 = (a10*b10 - a11*b08 + a13*b06)*invDet;
    result.m9 = (-a00*b10 + a01*b08 - a03*b06)*invDet;
    result.m10 = (a30*b04 - a31*b02 + a33*b00)*invDet;
    result.m11 = (-a20*b04 + a21*b02 - a23*b00)*invDet;
    result.m12 = (-a10*b09 + a11*b07 - a12*b06)*invDet;
    result.m13 = (a00*b09 - a01*b07 + a02*b06)*invDet;
    result.m14 = (-a30*b03 + a31*b01 - a32*b00)*invDet;
    result.m15 = (a20*b03 - a21*b01 + a22*b00)*invDet;

    return result;
}

#endif  // RLGL_IMPLEMENTATION
