unit OpenGLUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, gl, glu, OpenGLTypes;

function IfThen(ACondition: Boolean; a, b: GLfloat): GLfloat; overload;
function InterpolateRGB(AColor1, AColor2: TColor; ACoeff: GLfloat): TColor;

procedure SetOpenGLColor(const AColor: TColor; Alpha: GLfloat = 1.0);
procedure ToOrtho(AWidth, AHeight: Integer);
procedure ToPerspective(AWidth, AHeight: Integer; AViewAngle, ANearClipDist, AFarClipDist: GLfloat);

operator =(const A, B: TMethod): Boolean; overload; inline;

implementation

uses
  Math, OpenGLMath;

function IfThen(ACondition: Boolean; a,b : GLfloat): GLfloat;
begin
  if ACondition then Result := a else Result := b;
end;

function InterpolateRGB(AColor1, AColor2: TColor; ACoeff: GLfloat): TColor;
type
  TBytes = packed array [1..4] of Byte;
var
  c1: TBytes absolute AColor1;
  c2: TBytes absolute AColor2;
  r: TBytes absolute Result;
  i: Integer;
begin
  ACoeff := EnsureRange(ACoeff, 0.0, 1.0);
  for i := 1 to 4 do
    r[i] := Round(c1[i]  + (c2[i] - c1[i]) * ACoeff);
end;

procedure SetOpenGLColor(const AColor: TColor; Alpha: GLfloat = 1.0);
var
  c: TArray4f;
begin
  c := Array4f(Red(AColor)/255, Green(AColor)/255, Blue(AColor)/255, Alpha);
  glColor4fv(@c);
end;

{-------------------------------------------------------------------------------
  Set projection matrix as orthogonal
-------------------------------------------------------------------------------}
procedure ToOrtho(AWidth, AHeight: Integer);
begin
  // set viewport to be the entire window of the OpenGLControl
  glViewport(0, 0, AWidth, AHeight);

  // set orthographic viewing frustum
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(0, AWidth, 0, AHeight, -1, 1);

  // switch to modelview matrix in order to set scene
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
end;

{-------------------------------------------------------------------------------
  Set the projection matrix as perspective
-------------------------------------------------------------------------------}
procedure ToPerspective(AWidth, AHeight: Integer;
  AViewAngle, ANearClipDist, AFarClipDist: GLfloat);
var
  aspect: GLFloat;
begin
  // set viewport to be the entire window of the OpenGLControl
  glViewport(0, 0, AWidth, AHeight);

  // set perspective viewing frustum
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  aspect := AWidth / AHeight;
  gluPerspective(AViewAngle, aspect, ANearClipDist, AFarClipDist);

  // switch to modelview matrix in order to set scene
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
end;

operator = (const A, B: TMethod): Boolean;
begin
  Result := (A.Code = B.Code) and (A.Data = B.Data);
end;

end.

