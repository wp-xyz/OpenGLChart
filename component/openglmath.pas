unit OpenGLMath;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gl, OpenGLTypes;

function Vector3f(x, y, z: GLfloat): TVector3f;
function Cross(a, b: TVector3f): TVector3f;
function Dot(a, b: TVector3f): GLfloat;
function VLength(a: TVector3f): GLfloat;
function VNormalize(a: TVector3f): TVector3f;
function VRotate(v, a: TVector3f; phi: GLfloat): TVector3f;
operator +(a, b: TVector3f): TVector3f;
operator -(a, b: TVector3f): TVector3f;
operator *(a: TVector3f; b: GLfloat): TVector3f;
operator *(a: GLfloat; b: TVector3f): TVector3f;
operator =(a, b: TVector3f): boolean;

procedure Identity(out m: TMatrix4f);
procedure RotateX(var m: TMatrix4f; Angle: GLfloat);
procedure RotateY(var m: TMatrix4f; Angle: GLfloat);
procedure RotateZ(var m: TMatrix4f; Angle: GLfloat);
procedure Translate(var m: TMatrix4f; x, y, z: GLfloat);
operator *(const M: TMatrix4f; const v: TVector4f): TVector4f;
operator *(const M: TMatrix4f; const v: TVector3f): TVector3f;
operator *(const M: TMatrix4f; const a: GLfloat): TMatrix4f;


function Array3f(x, y, z: GLfloat): TArray3f;
function Array4f(x, y, z, w: GLfloat): TArray4f;
function Point3f(x, y, z: GLfloat): TPoint3f;

operator +(const a, b: TPoint): TPoint;
operator -(const a, b: TPoint): TPoint;
operator *(const a: TPoint; b: GLfloat): TPoint;

procedure GetMantisseAndExponent(AValue: GLfloat; out M: GLfloat; out Ex: Integer);

implementation

uses
  Math;

{ 3D vector }

function Vector3f(x, y, z: GLfloat): TVector3f;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

function Cross(a, b: TVector3f): TVector3f;
begin
  Result.x := a.y * b.z - a.z * b.y;
  Result.y := a.x * b.z - a.z * b.x;
  Result.z := a.x * b.y - a.y * b.x;
end;

function Dot(a, b: TVector3f): GLfloat;
begin
  Result := a.x * b.x + a.y * b.y + a.z * b.z;
end;

function VLength(a: TVector3f): GLfloat;
begin
  Result := sqrt(Dot(a, a));
end;

function VNormalize(a: TVector3f): TVector3f;
var
  L: GLfloat;
begin
  L := VLength(a);
  if L > 0.0 then
    Result := Vector3f(a.x / L, a.y / L, a.z / L);
end;

{ Rotates the vector v around the axis a by the angle Phi (in radians).
  a must be a unit vector. }
function VRotate(v, a: TVector3f; phi: GLfloat): TVector3f;
var
  sin_phi, cos_phi, cos_phi_1: GLfloat;
  v1, v2: TVector3f;
BEGIN
  SinCos(phi, sin_phi, cos_phi);
  cos_phi_1 := 1.0 - cos_phi;
  v1 := Cross(a, v);
  v2 := Cross(a, v1);
  Result := Vector3f(
    v.x + sin_phi * v1.x + cos_phi_1 * v2.x,
    v.y + sin_phi * v1.y + cos_phi_1 * v2.y,
    v.z + sin_phi * v1.z + cos_phi_1 * v2.z
  );
end;

operator +(a, b: TVector3f): TVector3f;
begin
  Result := Vector3f(a.x + b.x, a.y + b.y, a.z + b.z);
end;

operator -(a, b: TVector3f): TVector3f;
begin
  Result := Vector3f(a.x - b.x, a.y - b.y, a.z - b.z);
end;

operator *(a: TVector3f; b: GLfloat): TVector3f;
begin
  Result := Vector3f(a.x * b, a.y * b, a.z * b);
end;

operator *(a: GLfloat; b: TVector3f): TVector3f;
begin
  Result := Vector3f(a * b.x, a * b.y, a * b.z);
end;

operator =(a, b: TVector3f): boolean;
begin
  Result := (a.x = b.x) and (a.y = b.y) and (a.z = b.z);
end;


{ 4D matrix }

procedure Identity(out m: TMatrix4f);
const
  mi: TMatrix4f = ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1));
begin
  m := mi;
end;

procedure RotateX(var m: TMatrix4f; Angle: GLfloat);
var
  i: integer;
  y, z, c, s: GLfloat;
begin
  SinCos(Angle, s, c);
  for i := 0 to 2 do begin
    y := m[i, 1];
    z := m[i, 2];
    m[i, 1] := y * c - z * s;
    m[i, 2] := y * s + z * c;
  end;
end;

procedure RotateY(var m: TMatrix4f; Angle: GLfloat);
var
  i: integer;
  x, z, c, s: GLfloat;
begin
  SinCos(Angle, s, c);
  for i := 0 to 2 do begin
    x := m[i, 0];
    z := m[i, 2];
    m[i, 0] := x * c - z * s;
    m[i, 2] := x * s + z * c;
  end;
end;

procedure RotateZ(var m: TMatrix4f; Angle: GLfloat);
var
  i: integer;
  x, y, c, s: GLfloat;
begin
  SinCos(Angle, s, c);
  for i := 0 to 2 do begin
    x := m[i, 0];
    y := m[i, 1];
    m[i, 0] := x * c - y * s;
    m[i, 1] := x * s + y * c;
  end;
end;

procedure Translate(var m: TMatrix4f; x, y, z: GLfloat);
begin
  m[3, 0] := m[3, 0] + x;
  m[3, 1] := m[3, 1] + y;
  m[3, 2] := m[3, 2] + z;
end;

operator *(const M: TMatrix4f; const v: TVector4f): TVector4f;
begin
  Result.x := M[0, 0] * v.x + M[0, 1] * v.y + M[0, 2] * v.z + M[0, 3] * v.w;
  Result.y := M[1, 0] * v.x + M[1, 1] * v.y + M[1, 2] * v.z + M[1, 3] * v.w;
  Result.z := M[2, 0] * v.x + M[2, 1] * v.y + M[2, 2] * v.z + M[2, 3] * v.w;
  Result.w := M[3, 0] * v.x + M[3, 1] * v.y + M[3, 2] * v.z + M[3, 3] * v.w;
end;

operator *(const M: TMatrix4f; const v: TVector3f): TVector3f;
begin
  Result.x := M[0, 0] * v.x + M[0, 1] * v.y + M[0, 2] * v.z;
  Result.y := M[1, 0] * v.x + M[1, 1] * v.y + M[1, 2] * v.z;
  Result.z := M[2, 0] * v.x + M[2, 1] * v.y + M[2, 2] * v.z;
end;

operator *(const M: TMatrix4f; const a: GLfloat): TMatrix4f;
var
  i, j: Integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      Result[i, j] := M[i, j] * a;
end;


{ Misc }

function Array3f(x, y, z: GLfloat): TArray3f;
begin
  Result[0] := x;
  Result[1] := y;
  Result[2] := z;
end;

function Array4f(x, y, z, w: GLfloat): TArray4f;
begin
  Result[0] := x;
  Result[1] := y;
  Result[2] := z;
  Result[3] := w;
end;

function Point3f(x, y, z: GLfloat): TPoint3f;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

operator +(const a, b: TPoint): TPoint;
begin
  Result := Point(a.x + b.x, a.y + b.y);
end;

operator -(const a, b: TPoint): TPoint;
begin
  Result := Point(a.x - b.x, a.y - b.y);
end;

operator *(const a: TPoint; b: GLfloat): TPoint;
begin
  Result := Point(round(a.x * b), round(a.y * b));
end;

procedure GetMantisseAndExponent(AValue: GLfloat; out M: GLfloat; out Ex: Integer);
var
  s: String;
  p: Integer;
  res: Integer;
begin
  System.Str(AValue, s);
  p := pos('E', s);
  if p = 0 then p := pos('e', s);
  System.Val(copy(s, 1, p-1), M, res);
  System.Val(Copy(s, p+1, MaxInt), Ex, res);
end;

end.

