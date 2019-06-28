unit OpenGLAxis;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  gl, OpenGLTypes;

type
  ToglChartAxis = class;

  ToglChartAxisTitle = class(ToglChartElement)
  private
    FAxis: ToglChartAxis;
    FFontName: String;
    FFontSize: GLfloat;
    FFontColor: TColor;
    FText: String;
    FVisible: Boolean;
    procedure SetFontColor(const AValue: TColor);
    procedure SetFontName(const AValue: String);
    procedure SetFontSize(const AValue: GLfloat);
    procedure SetText(const AValue: String);
    procedure SetVisible(const AValue: Boolean);
  public
    constructor Create(AAxis: ToglChartAxis);
  published
    property FontColor: TColor read FFontColor write SetFontColor;
    property FontName: String read FFontName write SetFontName;
    property FontSize: GLfloat read FFontSize write SetFontSize;
    property Text: String read FText write SetText;
    property Visible: Boolean read FVisible write SetVisible default true;
  end;

  ToglChartAxis = class(ToglChartElement)
  private
    FKind: TAxisKind;
    FLineColor: TColor;
    FLineWidth: Integer;
    FLineVisible: Boolean;
    FTitle: ToglChartAxisTitle;
    FVisible: boolean;
    procedure SetLineColor(const AValue: TColor);
    procedure SetLineVisible(const AValue: Boolean);
    procedure SetLineWidth(const AValue: Integer);
    procedure SetVisible(const AValue: Boolean);
  public
    constructor Create(AChart: ToglBasicChart; AKind: TAxisKind);
    destructor Destroy; override;
    procedure Draw(AStartPt, AEndPt: TPoint3f; AFaceIndex: Integer);
    property Kind: TAxisKind read FKind;
  published
    property LineColor: TColor read FLineColor write SetLineColor;
    property LineVisible: Boolean read FLineVisible write SetLineVisible default true;
    property LineWidth: Integer read FLineWidth write SetLineWidth default 1;
    property Title: ToglChartAxisTitle read FTitle write FTitle;
    property Visible: boolean read FVisible write SetVisible;
  end;


implementation

uses
  EasyLazFreeType,
  OpenGLMath, OpenGLUtils, OpenGLText;

{ ToglChartAxisTitle }

constructor ToglChartAxisTitle.Create(AAxis: ToglChartAxis);
begin
  inherited Create(AAxis.Chart);
  FAxis := AAxis;
  FFontName := 'Arial';
  FFontSize := 0.1;
  FFontColor := clWhite;
  FVisible := true;
  case FAxis.Kind of
    akX: FText := 'x axis';
    akY: FText := 'y axis';
    akZ: FText := 'z axis';
  end;
end;

procedure ToglChartAxisTitle.SetFontColor(const AValue: TColor);
begin
  if FFontColor = AValue then exit;
  FFontColor := AValue;
  Notify(self, ncInvalidate, nil);
end;

procedure ToglChartAxisTitle.SetFontName(const AValue: String);
begin
  if FFontName = AValue then exit;
  FFontName := AValue;
  Notify(self, ncInvalidate, nil);
end;

procedure ToglChartAxisTitle.SetFontSize(const AValue: GLfloat);
begin
  if FFontSize = AValue then exit;
  FFontSize := AValue;
  Notify(self, ncInvalidate, nil);
end;

procedure ToglChartAxisTitle.SetText(const AValue: String);
begin
  if FText = AValue then exit;
  FText := AValue;
  Notify(self, ncInvalidate, nil);
end;

procedure ToglChartAxisTitle.SetVisible(const AValue: Boolean);
begin
  if FVisible = AValue then exit;
  FVisible := AValue;
  Notify(self, ncInvalidate, nil);
end;


{ ToglChartAxis }

constructor ToglChartAxis.Create(AChart: ToglBasicChart; AKind: TAxisKind);
begin
  inherited Create(AChart);
  FKind := AKind;
  FLineColor := clWhite;
  FLineVisible := true;
  FLineWidth := 1;
  FVisible := true;
  FTitle := ToglChartAxisTitle.Create(Self);
end;

destructor ToglChartAxis.Destroy;
begin
  FTitle.Free;
  inherited;
end;

procedure ToglChartAxis.Draw(AStartPt, AEndPt: TPoint3f; AFaceIndex: Integer);
var
  hasLighting: Boolean;
  P: TVector3f;
  w, h: Integer;
  fk: TFacekind;
  align: TFreeTypeAlignments;
  rotAngle: GLfloat;
  rotAxis: TVector3f;
  inplaneAngle: GLfloat = 0;
  flip: Boolean;
begin
  if not FVisible then
    exit;


  if FKind = akX then
    WriteLn('x: ', QUAD_FACE_NAMES[AFaceIndex], ' (', AFaceIndex, ')  StartPt: ', AStartPt.x:0:3, '/', AStartPt.y:0:3,'/', AStartPt.z:0:3,
      ' EndPt: ', AEndPt.x:0:3,'/',AEndPt.y:0:3,'/', AEndPt.z:0:3);

  if FKind = akY then
    WriteLn('y: ', QUAD_FACE_NAMES[AFaceIndex], ' (', AFaceIndex, ')  StartPt: ', AStartPt.x:0:3, '/', AStartPt.y:0:3,'/', AStartPt.z:0:3,
      ' EndPt: ', AEndPt.x:0:3,'/',AEndPt.y:0:3,'/', AEndPt.z:0:3);

  if FKind = akZ then
    WriteLn('z: ', QUAD_FACE_NAMES[AFaceIndex], ' (', AFaceIndex, ')  StartPt: ', AStartPt.x:0:3, '/', AStartPt.y:0:3,'/', AStartPt.z:0:3,
      ' EndPt: ', AEndPt.x:0:3,'/',AEndPt.y:0:3,'/', AEndPt.z:0:3);


  hasLighting := glIsEnabled(GL_LIGHTING) = GL_TRUE;
  if hasLighting then glDisable(GL_LIGHTING);

  // Draw axis line
  if FLineVisible then begin
    SetOpenGLColor(FLineColor);
    glLineWidth(FLineWidth);
    glBegin(GL_LINES);
      glVertex3fv(@AStartPt);
      glVertex3fv(@AEndPt);
    glEnd;
  end;

  // Draw title
  SetFont(FTitle.FontName, 100); //FTitle.FontSize);
  SetOpenGLColor(FTitle.FontColor);
  {
  case FKind of
    akX: if odd(AFaceIndex) then align := [ftaTop] else align := [ftaBottom];
    akY: if AStartPt.X > 0 then align := [ftaTop] else align := [ftaBottom];
    akZ: if AStartPt.Z > 0 then align := [ftaBottom] else align := [ftaTop];
  end;
  }
  P := (AStartPt + AEndPt) * 0.5;
  {
  rotAngle := RadToDeg(arccos(Dot(QUAD_FACE_NORMALS[AFaceIndex], Vector3f(0, 0, -1))));
  rotAxis := Cross(QUAD_FACE_NORMALS[AFaceIndex], Vector3f(0, 0, -1));
  }
  case AFaceIndex of
    { y axis }
    0: begin      // left yz face
         rotAngle := 90;
         rotAxis := Vector3f(0, 0, 1);
         inplaneAngle := IfThen(AStartPt.Z > 0, 180, 0);
         flip := true;
       end;
    1: begin     // right yz face
         rotAngle := -90;
         rotAxis := Vector3f(0, 0, 1);
         inplaneAngle := 180;
         flip := true;
       end;

    { z axis }
    2: begin     // front xz face
         rotAngle := 90;
         rotAxis := Vector3f(1, 0, 0);
         inplaneAngle := IfThen(AStartPt.X > 0, -90, 90);
         inplaneAngle := -90;
         //flip := true;
         align := [ftaTop];
       end;
    3: begin     // back xz face
         rotAngle := -90;
         rotAxis := Vector3f(1, 0, 0);
         inPlaneAngle := IfThen(AStartPt.X > 0, 90, -90);
         inplaneAngle := 90;
         align := [ftaBottom];
       end;

    { x axis }
    4: begin    // bottom xy face
         rotAngle := 0;
         rotAxis := Vector3f(1, 0, 0);
  //       inplaneAngle := IfThen(AStartPt.Y > 0, 180, 0);
         inplaneAngle := IfThen(AStartPt.Y * AStartPt.Z > 0, 0, 180);
         flip := true;
         align := [ftaTop];
       end;
    5: begin    // top xy face
         rotAngle := 0;
         rotAxis := Vector3f(1, 0, 0);
//         inplaneAngle := IfThen(AStartPt.Y > 0, 180, 0);
         inplaneAngle := IfThen(AStartPt.Y * AStartPt.Z > 0, 180, 0);
         flip := false;
         align := [ftaBottom];
       end;
  end;
  Include(align, ftaCenter);

  glPushMatrix;
    //DrawText(FTitle.Text, (AStartPt + AEndPt) * 0.5, AEndPt - AStartPt, 180, 0.02, align);
    DrawText(FTitle.Text, P, rotAxis, rotAngle, inplaneAngle, 0.0015, align, flip);
  glPopMatrix;

  if hasLighting then glEnable(GL_LIGHTING);
end;

procedure ToglChartAxis.SetLineColor(const AValue: TColor);
begin
  if FLineColor = AValue then exit;
  FLineColor := AValue;
  Notify(self, ncInvalidate, nil);
end;

procedure ToglChartAxis.SetLineVisible(const AValue: Boolean);
begin
  if FLineVisible = AValue then exit;
  FLineVisible := AValue;
  Notify(self, ncInvalidate, nil);
end;

procedure ToglChartAxis.SetLineWidth(const AValue: Integer);
begin
  if FLineWidth = AValue then exit;
  if FLineWidth < 0 then FLineWidth := 0;
  FLineWidth := AValue;
  Notify(self, ncInvalidate, nil);
end;

procedure ToglChartAxis.SetVisible(const AValue: Boolean);
begin
  if FVisible = AValue then exit;
  FVisible := AValue;
  Notify(self, ncInvalidate, nil);
end;


end.

