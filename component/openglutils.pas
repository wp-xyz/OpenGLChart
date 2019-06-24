unit OpenGLUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gl;

function IfThen(ACondition: Boolean; a, b: GLfloat): GLfloat; overload;

implementation

function IfThen(ACondition: Boolean; a,b : GLfloat): GLfloat;
begin
  if ACondition then Result := a else Result := b;
end;

end.

