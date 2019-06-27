unit OpenGLUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, gl, OpenGLTypes;

function IfThen(ACondition: Boolean; a, b: GLfloat): GLfloat; overload;
function InterpolateRGB(AColor1, AColor2: TColor; ACoeff: GLfloat): TColor;
procedure SetOpenGLColor(const AColor: TColor; Alpha: GLfloat = 1.0);

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

operator = (const A, B: TMethod): Boolean;
begin
  Result := (A.Code = B.Code) and (A.Data = B.Data);
end;

end.

