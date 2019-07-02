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
    FDistance: GLfloat;
    FKind: TAxisKind;
    FLineColor: TColor;
    FLineWidth: Integer;
    FLineVisible: Boolean;
    FPosition: TAxisPosition;
    FTitle: ToglChartAxisTitle;
    FVisible: boolean;
    FStartIndex: Integer;
    FEndIndex: Integer;
    FWritingFaceNormal: TVector3f;
    FOtherWritingFaceNormal: TVector3f;
    procedure SetLineColor(const AValue: TColor);
    procedure SetLineVisible(const AValue: Boolean);
    procedure SetLineWidth(const AValue: Integer);
    procedure SetVisible(const AValue: Boolean);
  protected
  public
    constructor Create(AChart: ToglBasicChart; AKind: TAxisKind);
    destructor Destroy; override;
    procedure Draw; //(AStartPt, AEndPt: TPoint3f; AFaceIndex: Integer);
    function StartPt: TVector3f;
    function EndPt: TVector3f;
    procedure InitParams(AStartIndex, AEndIndex: Integer; APosition: TAxisPosition;
      AWritingFaceNormal, AOtherWritingFaceNormal: TVector3f);
    property Kind: TAxisKind read FKind;
    property Position: TAxisPosition read FPosition;
    property StartIndex: Integer read FStartIndex;
    property EndIndex: Integer read FEndIndex;
  published
    property LineColor: TColor read FLineColor write SetLineColor;
    property LineVisible: Boolean read FLineVisible write SetLineVisible default true;
    property LineWidth: Integer read FLineWidth write SetLineWidth default 1;
    property Title: ToglChartAxisTitle read FTitle write FTitle;
    property Visible: boolean read FVisible write SetVisible;
  end;


implementation

uses
  EasyLazFreeType, Math,
  OpenGLMath, OpenGLUtils, OpenGLText, OpenGLChart;

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
  FDistance := 10;  // percentage of max boundingbox dimension
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

procedure ToglChartAxis.Draw;
var
  hasLighting: Boolean;
  P1, P2: TVector3f;
  phi: GLfloat;
  axis: TVector3f;
  faceNormal: TVector3f;
  Ptxt: TVector3f;
  bboxsize: GLfloat;
  dist: GLfloat;
begin
  if not FVisible then
    exit;

  // This case can happen when looking on a face rather perpendicularly so that
  // one axis is detected twice and another axis is missed - should refine
  // Chart.InitAxes to correct this, but the missing axis is very short here
  // and can't be labeled nicely in any way.
  if (FStartIndex = -1) or (EndIndex = -1) then
    exit;

  hasLighting := glIsEnabled(GL_LIGHTING) = GL_TRUE;
  if hasLighting then glDisable(GL_LIGHTING);

  P1 := StartPt;
  P2 := EndPt;

  // Draw axis line
  if FLineVisible then begin
    SetOpenGLColor(FLineColor);
    glLineWidth(FLineWidth);
    glBegin(GL_LINES);
      glVertex3fv(@P1);
      glVertex3fv(@P2);
    glEnd;
  end;

  Ptxt := (P1 + P2) * 0.5 * 1.2;
  SetFont(FTitle.FontName, round(FTitle.FontSize), []);
  SetOpenGLColor(FTitle.FontColor);
  if FPosition = apLeft then
    DrawText2d(Ptxt.x, Ptxt.y, Ptxt.z, FTitle.Text, [ftaRight, ftaVerticalCenter])
  else
    DrawText2d(Ptxt.x, Ptxt.y, Ptxt.z, FTitle.Text, [ftaLeft, ftaVerticalCenter]);
                       (*

  glPushmatrix;

  case FKind of
    akX: glTranslatef(0, StartPt.y, StartPt.z);
    akY: glTranslatef(StartPt.x, 0, StartPt.z);
    akZ: glTranslatef(StartPt.x, StartPt.y, 0);
  end;

  // By default, text is written on the xy plane to be viewed along negative z axis.
  faceNormal := FWritingFaceNormal;  // or FOtherWritingFaceNormal...
  // Rotate text so that its normal points along the faceNormal
  // (1) Text plane is current writing plane -- nothing to do
  if faceNormal = Vector3f(0, 0, -1) then
  else
  // (2) Text plane is opposite to current writing plane --> rotate by 180 deg
  if faceNormal = Vector3f(0, 0, 1) then
    glRotatef(180, 0,0,1)  // or 1,0,0 ?
  else begin
  // (3) Any other plane
    phi := RadToDeg(arccos(Dot(Vector3f(0, -1, 0), faceNormal)));
    axis := Cross(Vector3f(0, 0, -1), faceNormal);
    glRotatef(phi, axis.x, axis.y, axis.z);
  end;

  case FKind of
    akX: if StartPt.y < 0 then glRotatef(180, 0, 1, 0);
  end;

  SetFont('Arial', 10, []);
  SetOpenGLColor(FTitle.FontColor);
  glScalef(0.01, 0.01, 0.01);
  DrawText(FTitle.Text, [ftaCenter]);

  glPopMatrix;
                    *)

  if hasLighting then glEnable(GL_LIGHTING);
end;

                        (*
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

         {
  if FKind = akX then
    WriteLn('x: ', QUAD_FACE_NAMES[AFaceIndex], ' (', AFaceIndex, ')  StartPt: ', AStartPt.x:0:3, '/', AStartPt.y:0:3,'/', AStartPt.z:0:3,
      ' EndPt: ', AEndPt.x:0:3,'/',AEndPt.y:0:3,'/', AEndPt.z:0:3);

  if FKind = akY then
    WriteLn('y: ', QUAD_FACE_NAMES[AFaceIndex], ' (', AFaceIndex, ')  StartPt: ', AStartPt.x:0:3, '/', AStartPt.y:0:3,'/', AStartPt.z:0:3,
      ' EndPt: ', AEndPt.x:0:3,'/',AEndPt.y:0:3,'/', AEndPt.z:0:3);

  if FKind = akZ then
    WriteLn('z: ', QUAD_FACE_NAMES[AFaceIndex], ' (', AFaceIndex, ')  StartPt: ', AStartPt.x:0:3, '/', AStartPt.y:0:3,'/', AStartPt.z:0:3,
      ' EndPt: ', AEndPt.x:0:3,'/',AEndPt.y:0:3,'/', AEndPt.z:0:3);
          }

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
                          *)

function ToglChartAxis.EndPt: TVector3f;
var
  P: TVector3f;
  ch: ToglChart;
begin
  if (FEndIndex < 0) or (FEndIndex > 7) then
    WriteLn('INDEX ERROR');

  P := CUBE_VERTICES[FEndIndex];
  ch := ToglChart(Chart);
  Result := Vector3f(
    IfThen(P.x < 0, ch.ImgExtent.a.x, ch.ImgExtent.b.x),
    IfThen(P.y < 0, ch.ImgExtent.a.y, ch.ImgExtent.b.y),
    IfThen(P.z < 0, ch.ImgExtent.a.z, ch.ImgExtent.b.z)
  );
end;

procedure ToglChartAxis.InitParams(AStartIndex, AEndIndex: Integer;
  APosition: TAxisPosition; AWritingFaceNormal, AOtherWritingFaceNormal: TVector3f);
begin
  FStartIndex := AStartIndex;
  FEndIndex := AEndIndex;
  FPosition := APosition;
  FWritingFaceNormal := AWritingFaceNormal;
  FOtherWritingFaceNormal := AOtherWritingFaceNormal;
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

function ToglChartAxis.StartPt: TVector3f;
var
  P: TVector3f;
  ch: ToglChart;
begin
  P := CUBE_VERTICES[FStartIndex];
  ch := ToglChart(Chart);
  Result := Vector3f(
    IfThen(P.x < 0, ch.ImgExtent.a.x, ch.ImgExtent.b.x),
    IfThen(P.y < 0, ch.ImgExtent.a.y, ch.ImgExtent.b.y),
    IfThen(P.z < 0, ch.ImgExtent.a.z, ch.ImgExtent.b.z)
  );
end;


end.

