unit OpenGLUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, gl, glu, OpenGLTypes;

function IfThen(ACondition: Boolean; a, b: GLfloat): GLfloat; overload;
function InterpolateRGB(AColor1, AColor2: TColor; ACoeff: GLfloat): TColor;

function ProjectToScreen(P: TVector3f): TPoint;
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

{-------------------------------------------------------------------------------
  Determines the screen coordinates of a given 3d point
-------------------------------------------------------------------------------}
function ProjectToScreen(P: TVector3f): TPoint;
var
  VP: array[0..3] of Integer;    // Viewport
  MVM: array[0..15] of double;   // ModelView matrix, must be double
  PM: array[0..15] of double;    // Projection matrix
  xs, ys, zs: Double;            // MUST be double
begin
  // Get current ModelView matrix
  glGetDoublev(GL_MODELVIEW_MATRIX, @MVM);
  // Get current projection matrix
  glGetDoublev(GL_PROJECTION_MATRIX, @PM);
  // Get viewport
  glGetIntegerV(GL_VIEWPORT, @VP);
  // map object coordinates to window coordinates
  gluProject(double(P.x), double(P.y), double(P.z), @MVM, @PM, @VP, @xs, @ys, @zs);

  Result := Point(round(xs), round(ys));
end;

{ Sets the OpenGL color to that corresponding to the provided LCL TColor }
procedure SetOpenGLColor(const AColor: TColor; Alpha: GLfloat = 1.0);
const
  f = 1.0/255;
var
  c: TArray4f;
begin
  c := Array4f(Red(AColor)*f, Green(AColor)*f, Blue(AColor)*f, Alpha);
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

