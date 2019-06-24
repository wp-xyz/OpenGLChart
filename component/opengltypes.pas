unit OpenGLTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  gl;

type
  TVector3f = record
    x, y, z: GLfloat;
  end;
  TPoint3f = TVector3f;
  TArray3f = array[0..2] of GLfloat;

  TVector4f = record
    x, y, z, w: GLfloat;
  end;
  TArray4f = array[0..3] of GLfloat;

  TRect3f = record
    a, b: TPoint3f;
  end;

  TMatrix4f = array[0..3, 0..3] of GLfloat;

  TQuad3f = array[0..7] of TVector3f;
    {              7               6
                   +---------------+              z
                 / .            5 /|              |   y
              4 +----------------+ |              | /
                |  . . . . . . . | +              +----x
                | . 3            |/ 2
                -----------------+
                0                1 }

  TQuadFace = array[0..3] of Integer;

  TAxisKind = (akX, akY, akZ);

  TFaceKind = (fkXY, fkYZ, fkXZ);

const
  CUBE_VERTICES: TQuad3f = (
    (x:-1.0; y:-1.0; z:-1.0),
    (x:+1.0; y:-1.0; z:-1.0),
    (x:+1.0; y:+1.0; z:-1.0),
    (x:-1.0; y:+1.0; z:-1.0),
    (x:-1.0; y:-1.0; z:+1.0),
    (x:+1.0; y:-1.0; z:+1.0),
    (x:+1.0; y:+1.0; z:+1.0),
    (x:-1.0; y:+1.0; z:+1.0)
  );

  QUAD_FACE_NORMALS: array[0..5] of TVector3f = (
    (x:-1.0; y: 0.0; z: 0.0),        // left face
    (x:+1.0; y: 0.0; z: 0.0),        // right face
    (x: 0.0; y:-1.0; z: 0.0),        // front face
    (x: 0.0; y:+1.0; z: 0.0),        // back face
    (x: 0.0; y: 0.0; z:-1.0),        // bottom face
    (x: 0.0; y: 0.0; z:+1.0)         // top face
  );

  QUAD_FACES: array[0..5] of TQuadFace = (
    // indices ordered so that the normal points outward (right-hand rule!)
    (0, 4, 7, 3),   // left face
    (1, 2, 6, 5),   // right face
    (0, 1, 5, 4),   // front face
    (3, 7, 6, 2),   // back face
    (0, 3, 2, 1),   // bottom face
    (4, 5, 6, 7)    // top face
  );

  QUAD_FACE_KINDS: array[0..5] of TFaceKind = (
    fkYZ,
    fkYZ,
    fkXZ,
    fkXZ,
    fkXY,
    fkXY
  );

  QUAD_FACE_NAMES: array[0..5] of string = (
    'left yz',
    'right yz',
    'front xz',
    'back xz',
    'bottom xy',
    'top xy'
  );


implementation

end.

