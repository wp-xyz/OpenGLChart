unit OpenGLUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gl;

function IfThen(condition: Boolean; a, b: GLfloat): GLfloat; overload;

implementation

function IfThen(condittion: Boolean; a,b : GLfloat): GLfloat;
begin
  if condition then Result := a else Result := b;
end;

end.

