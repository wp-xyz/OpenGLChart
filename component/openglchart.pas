unit OpenGLChart;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Graphics,
  gl, glu, openglcontext,
  OpenGLTypes, OpenGLMath, OpenGLAxis, OpenGLLightSources;

const
  DEFAULT_VIEW_ANGLE = 30;
  DEFAULT_DISTANCE = 8;
  DEFAULT_VERT_ROTATION = 30;
  DEFAULT_HOR_ROTATION = 20;
  NEAR_CLIP_DISTANCE = 0.1;
  FAR_CLIP_DISTANCE = 100.0;

type
  ToglBasicSeries = class(TComponent)
  private
    FActive: Boolean;
    FChart: ToglBasicChart;
    FTitle: String;
    procedure SetActive(const AValue: Boolean);
    procedure SetTitle(const AValue: String);
  protected
    FExtent: TRect3f;
    procedure EmptyExtent;
    procedure Notify(ASender: TObject; ACmd: TNotifyCmd; AParam: Pointer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw; virtual;
    function Extent: TRect3f; virtual;
    function ExtentIsEmpty: Boolean;
    procedure UpdatePalette; virtual;
    property Active: Boolean read FActive write SetActive default true;
    property Chart: ToglBasicChart read FChart;
    property Title: String read FTitle write SetTitle;
  published
  end;

  ToglViewParams = class(ToglChartElement)
  private
    FDistance: GLfloat;
    FHorRot: GLfloat;
    FVertRot: GLfloat;
    FDepthRot: GLfloat;
    FInteractive: Boolean;
    procedure SetDistance(const AValue: GLfloat);
    procedure SetDepthRot(const AValue: GLfloat);
    procedure SetHorRot(const AValue: GLfloat);
    procedure SetVertRot(const AValue: GLfloat);
  public
    constructor Create(AChart: ToglBasicChart);
  published
    property Distance: GLfloat read FDistance write SetDistance;
    property DepthRot: GLfloat read FDepthRot write SetDepthRot;
    property HorRot: GLfloat read FHorRot write SetHorRot;
    property Interactive: Boolean read FInteractive write FInteractive default true;
    property VertRot: GLfloat read FVertRot write SetVertRot;
  end;

  ToglChart = class(ToglBasicChart)
  private
    FBBoxColor: TColor;
    FBkColor: TColor;
    FFullExtent: TRect3f;
    FImgExtent: TRect3f;
    FBoundingBox: TQuad3f;
    FLightSources: ToglLightSources;
    FScaleX, FScaleY, FScaleZ: GLfloat;
    FScalingValid: Boolean;
    FSeriesList: TFPList;
    FShowAxes: Boolean;
    FShowBBox: Boolean;
    FViewParams: ToglViewParams;
    FBackWall: ToglWall;
    FLeftWall: ToglWall;
    FBottomWall: ToglWall;
    FXAxis: ToglChartAxis;
    FYAxis: ToglChartAxis;
    FZAxis: ToglChartAxis;
    function GetSeries(AIndex: Integer): ToglBasicSeries;
    function GetSeriesCount: Integer;
    procedure SetBBoxColor(const AValue: TColor);
    procedure SetBkColor(const AValue: TColor);
    procedure SetShowAxes(const AValue: Boolean);
    procedure SetShowBBox(const AValue: Boolean);
  protected
    FDistance: GLfloat;
    FInitDone: Boolean;
    FInitLightsDone: Boolean;
    FMousePos: TPoint;
    FViewMatrix: TMatrix4f;
    procedure CalcScaling;
    function CurrentBoundingBox: TQuad3f;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DoOnResize; override;
    procedure DrawAxes;
    procedure DrawBoundingBox;
    procedure DrawChart; virtual;
    procedure DrawSeries;
    procedure DrawWallsAndAxes;
    procedure DummyExtent;
    procedure EmptyExtent;
    procedure InitGL; virtual;
    procedure InitLights(Attachment: TLightAttachment);
    procedure InitProjection; virtual;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure RenewExtent;
    procedure Update(ASender: TObject; ACmd: TNotifyCmd; AParam: Pointer); override; //reintroduce;
    procedure UpdateExtent(ASeries: ToglBasicSeries);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddSeries(ASeries: ToglBasicSeries): Integer;
    procedure ClearSeries;
    procedure DeleteSeries(ASeries: ToglBasicSeries);

    procedure DoOnPaint; override;

    function GetAxis(AKind: TAxisKind): ToglChartAxis;

    function ImageToWorldX(X: GLfloat): GLfloat;
    function ImageToWorldY(Y: GLfloat): GLfloat;
    function ImageToWorldZ(Z: GLfloat): GLfloat;
    function ImageToWorld(P: TPoint3f): TPoint3f;

    function WorldToImageX(X: GLfloat): GLfloat;
    function WorldToImageY(Y: GLfloat): GLfloat;
    function WorldToImageZ(Z: GLfloat): GLfloat;
    function WorldToImage(P: TPoint3f): TPoint3f;

    property ImgExtent: TRect3f read FImgExtent;
    property Series[AIndex: Integer]: ToglBasicSeries read GetSeries;
    property SeriesCount: Integer read GetSeriesCount;
  published
    property BackWall: ToglWall read FBackWall write FBackWall;
    property BottomWall: ToglWall read FBottomWall write FBottomWall;
    property BoundingBoxColor: TColor read FBBoxColor write SetBBoxColor default clGray;
    property Color: TColor read FBkColor write SetBkColor default clBlack;
    property LeftWall: ToglWall read FLeftWall write FLeftWall;
    property LightSources: ToglLightSources read FLightSources write FLightSources;
    property ShowAxes: Boolean read FShowAxes write SetShowAxes default false;
    property ShowBoundingBox: Boolean read FShowBBox write SetShowBBox default false;
    property ViewParams: ToglViewParams read FViewParams write FViewParams;
    property XAxis: ToglChartAxis read FXAxis write FXAxis;
    property YAxis: ToglChartAxis read FYAxis write FYAxis;
    property ZAxis: ToglChartAxis read FZAxis write FZAxis;
  end;


implementation

uses
  Math, LCLIntf, EasyLazFreeType, OpenGLUtils, OpenGLSeries, OpenGLText;


{ ToglBasicSeries }

constructor ToglBasicSeries.Create(AOwner: TComponent);
begin
  inherited;
  FActive := true;
  EmptyExtent;
end;

destructor ToglBasicSeries.Destroy;
begin
  if Chart <> nil then ToglChart(Chart).DeleteSeries(self);
  inherited;
end;

procedure ToglBasicSeries.Draw;
begin
end;

procedure ToglBasicSeries.EmptyExtent;
begin
  FExtent.a := Point3f(Infinity, Infinity, Infinity);
  FExtent.b := Point3f(-Infinity, -Infinity, -Infinity);
end;

function ToglBasicSeries.Extent: TRect3f;
begin
  Result := FExtent;
end;

function ToglBasicSeries.ExtentIsEmpty: Boolean;
begin
  result := (FExtent.a = Point3f(Infinity, Infinity, Infinity)) and
            (FExtent.b = Point3f(-Infinity, -Infinity, -Infinity));
end;

procedure ToglBasicSeries.Notify(ASender: TObject; ACmd: TNotifyCmd; AParam: Pointer);
begin
  if FChart <> nil then
    FChart.Update(self, ACmd, AParam);
end;

procedure ToglBasicSeries.SetActive(const AValue: Boolean);
begin
  if AValue = FActive then exit;
  FActive := AValue;
  if FActive then
    Notify(self, ncUpdateExtent, self)
  else
    Notify(self, ncUpdateExtent, nil);
end;

procedure ToglBasicSeries.SetTitle(const AValue: String);
begin
  if AValue = FTitle then exit;
  FTitle := AValue;
  Notify(self, ncInvalidate, nil);
end;

procedure ToglBasicSeries.UpdatePalette;
begin
end;


{ ToglViewParams }

constructor ToglViewParams.Create(AChart: ToglBasicChart);
begin
  inherited Create(AChart);
  FDistance := DEFAULT_DISTANCE;
  FDepthRot := 0;
  FHorRot := DEFAULT_HOR_ROTATION;
  FVertRot := DEFAULT_VERT_ROTATION;
  FInteractive := true;
end;

procedure ToglViewParams.SetDistance(const AValue: GLfloat);
begin
  if FDistance = AValue then exit;
  FDistance := AValue;
  Notify(self, ncView, nil);
end;

procedure ToglViewParams.SetDepthRot(const AValue: GLfloat);
begin
  if FDepthRot = AValue then exit;
  FDepthRot := AValue;
  Notify(self, ncView, nil);
end;

procedure ToglViewParams.SetHorRot(const AValue: GLfloat);
begin
  if FHorRot = AValue then exit;
  FHorRot := AValue;
  Notify(self, ncView, nil);
end;

procedure ToglViewParams.SetVertRot(const AValue: GLfloat);
begin
  if FVertRot = AValue then exit;
  FVertRot := AValue;
  Notify(self, ncView, nil);
end;


{ ToglChart }

constructor ToglChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  InitFonts;

  FImgExtent.a := Point3f(-1.0, -1.0, -1.0);
  FImgExtent.b := Point3f(+1.0, +1.0, +1.0);
  EmptyExtent;

  FSeriesList := TFPList.Create;
  FLightSources := ToglLightSources.Create(self);
  FViewParams := ToglViewParams.Create(Self);

  FBackWall := ToglWall.Create(self, fkXZ);
  FLeftWall := ToglWall.Create(self, fkYZ);
  FBottomWall := ToglWall.Create(self, fkXY);

  FXAxis := ToglChartAxis.Create(self, akX);
  FYAxis := ToglChartAxis.Create(self, akY);
  FZAxis := ToglChartAxis.Create(self, akZ);

  FBBoxColor := clGray;
  FBkColor := clBlack;
  FShowAxes := false;
  FShowBBox := false;

  FDistance := FViewParams.Distance;
  Identity(FViewMatrix);
  RotateZ(FViewMatrix, DegToRad(FViewParams.DepthRot));
  RotateY(FViewMatrix, DegToRad(FViewParams.VertRot));
  RotateX(FViewMatrix, DegToRad(FViewParams.HorRot));
end;

destructor ToglChart.Destroy;
begin
  ClearSeries;
  FXAxis.Free;
  FYAxis.Free;
  FZAxis.Free;
  FBackWall.Free;
  FBottomWall.Free;
  FLeftWall.Free;
  FViewParams.Free;
  FLightSources.Free;
  FSeriesList.Free;
  inherited;
end;

function ToglChart.AddSeries(ASeries: ToglBasicSeries): Integer;
begin
  Result := FSeriesList.Add(ASeries);
  ASeries.FChart := Self;
  UpdateExtent(ASeries);
end;

procedure ToglChart.CalcScaling;
begin
  FScaleX := (FFullExtent.b.x - FFullExtent.a.x) / (FImgExtent.b.x - FImgExtent.a.x);
  FScaleY := (FFullExtent.b.y - FFullExtent.a.y) / (FImgExtent.b.y - FImgExtent.a.y);
  FScaleZ := (FFullExtent.b.z - FFullExtent.a.z) / (FImgExtent.b.z - FImgExtent.a.z);
  FScalingValid := true;
end;

procedure ToglChart.ClearSeries;
var
  i: Integer;
  ser: ToglBasicSeries;
begin
  for i := 0 to FSeriesList.Count - 1 do begin
    ser := ToglBasicSeries(FSeriesList[i]);
    DeleteSeries(ser);
    ser.Free;
  end;
  FSeriesList.Clear;
  EmptyExtent;
  if not (csDestroying in ComponentState) then
    Invalidate;
end;

function ToglChart.CurrentBoundingBox: TQuad3f;
var
  M: TMatrix4f;
begin
  glGetFloatv(GL_MODELVIEW_MATRIX, @M);
  with FImgExtent do begin
    Result[0] := M * Vector3f(a.x, a.y, a.z);
    Result[1] := M * Vector3f(b.x, a.y, a.z);
    Result[2] := M * Vector3f(b.x, b.y, a.z);
    Result[3] := M * Vector3f(a.x, b.y, a.z);
    Result[4] := M * Vector3f(a.x, a.y, b.z);
    Result[5] := M * Vector3f(b.x, a.y, b.z);
    Result[6] := M * Vector3f(b.x, b.y, b.z);
    Result[7] := M * Vector3f(a.x, b.y, b.z);
  end;
end;

procedure ToglChart.DeleteSeries(ASeries: ToglBasicSeries);
var
  idx: Integer;
begin
  idx := FSeriesList.IndexOf(ASeries);
  if idx = -1 then exit;
  FSeriesList.Delete(idx);
  RenewExtent;
end;

function ToglChart.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
const
  SPEED = 0.0005;
var
  f: GLfloat;
begin
  if FViewParams.Interactive then begin
    if (ssCtrl in Shift) then
      f := -1.0
    else
      f := 1.0;
    if (ssShift in Shift) then
      f := f * 2;
    FDistance := FDistance * (1.0 + f * WheelDelta * SPEED);
    Invalidate;
  end;

  Result := inherited;
end;

procedure ToglChart.DoOnPaint;
begin
  inherited;

  if not MakeCurrent then
    exit;

  InitGL;
  InitProjection;

  glClearColor(Red(FBkColor)/255, Green(FBkColor)/255, Blue(FBkColor)/255, 1.0);   // sets background color
  glClearDepth(1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  //glPushMatrix;

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  // Initialize the lights that are attached to the camera
  InitLights(latCamera);

  glTranslatef(0, 0, -FDistance);
  glMultMatrixf(@FViewMatrix);
  // Rotate world so that z points upward, x to the right, y into the screen
  glRotatef(-90, 1, 0, 0);

  // Initialize the lights that are attached to the model
  InitLights(latModel);
  //FInitLightsDone := true;

  DrawChart;

  //glPopMatrix;
  SwapBuffers;
end;

procedure ToglChart.DoOnResize;
begin
  if MakeCurrent then
    InitProjection;
  inherited;
end;

procedure ToglChart.DrawAxes;
const
  EPS = 1e-3;
  AXIS_LENGTH = 100;
var
  P: TPoint3f;
  hasBlend: Boolean;
  hasLighting: Boolean;
begin
  if not FShowAxes then
    exit;

  // Calculate position of origin in scaled coordinate system
  P := WorldToImage(Point3f(0, 0, 0));

  glLineWidth(3);
  hasBlend := (glIsEnabled(GL_BLEND) = GL_TRUE);
  glEnable(GL_BLEND);  // for line smoothing
  hasLighting := (glIsEnabled(GL_LIGHTING) = GL_TRUE);
  glDisable(GL_LIGHTING);

  SetOpenGLColor(clRed);  // x axis --> red
  glBegin(GL_LINES);
    glVertex3f(P.x, P.y, P.z);
    glVertex3f(P.x + AXIS_LENGTH, P.y, P.z);
  glEnd;

  SetOpenGLColor(clGreen);  // y axis --> green
  glBegin(GL_LINES);
    glVertex3f(P.x, P.y, P.z);
    glVertex3f(P.x, P.y + AXIS_LENGTH, P.z);
  glEnd;

  SetOpenGLColor(clBlue);  // z axis --> blue
  glBegin(GL_LINES);
    glVertex3f(P.x, P.y, P.z);
    glVertex3f(P.x, P.y, P.z + AXIS_LENGTH);
  glEnd;

  if not hasBlend then
    glDisable(GL_BLEND);
  if hasLighting then
    glEnable(GL_LIGHTING);
end;

procedure ToglChart.DrawBoundingBox;
var
  P1, P2: TPoint3f;
  hasBlend, hasLighting: Boolean;
begin
  if not ShowBoundingBox then exit;

  if (FFullExtent.a.X = Infinity) or (FFullExtent.b.X = -Infinity) or
     (FFullExtent.a.Y = Infinity) or (FFullExtent.b.Y = -Infinity) or
     (FFullExtent.a.z = Infinity) or (FFullExtent.b.z = -Infinity) then
  begin
    P1 := FImgExtent.a;
    P2 := FImgExtent.b;
  end else begin
    P1 := WorldToImage(FFullExtent.a);
    P2 := WorldToImage(FFullExtent.b);
  end;

  hasLighting := glIsEnabled(GL_LIGHTING) = GL_TRUE;
  if hasLighting then glDisable(GL_LIGHTING);

  hasBlend := glIsEnabled(GL_BLEND) = GL_TRUE;
  glEnable(GL_BLEND);  // for line smoothing
  glLineWidth(1);
  SetOpenGLColor(FBBoxColor);

  glBegin(GL_LINE_STRIP);
    glVertex3f(P1.x, P1.y, P1.z);
    glVertex3f(P2.x, P1.y, P1.z);
    glVertex3f(P2.x, P2.y, P1.z);
    glVertex3f(P1.x, P2.y, P1.z);
    glVertex3f(P1.x, P1.y, P1.z);
  glEnd;

  glBegin(GL_LINE_STRIP);
    glVertex3f(P1.x, P1.y, P2.z);
    glVertex3f(P2.x, P1.y, P2.z);
    glVertex3f(P2.x, P2.y, P2.z);
    glVertex3f(P1.x, P2.y, P2.z);
    glVertex3f(P1.x, P1.y, P2.z);
  glEnd;

  glBegin(GL_LINES);
    glVertex3f(P1.x, P1.y, P1.z);
    glVertex3f(P1.x, P1.y, P2.z);
  glEnd;

  glBegin(GL_LINES);
    glVertex3f(P2.x, P1.y, P1.z);
    glVertex3f(P2.x, P1.y, P2.z);
  glEnd;

  glBegin(GL_LINES);
    glVertex3f(P2.x, P2.y, P1.z);
    glVertex3f(P2.x, P2.y, P2.z);
  glEnd;

  glBegin(GL_LINES);
    glVertex3f(P1.x, P2.y, P1.z);
    glVertex3f(P1.x, P2.y, P2.z);
  glEnd;

  if not hasBlend then glDisable(GL_BLEND);
  if hasLighting then glEnable(GL_LIGHTING);
end;

procedure ToglChart.DrawChart;
begin
  if FSeriesList.Count = 0 then
    DummyExtent;

  FBoundingBox := CurrentBoundingBox;
  DrawWallsAndAxes;

  DrawSeries;

  DrawBoundingBox;
  DrawAxes;
end;

procedure ToglChart.DrawSeries;
var
  i: Integer;
  ser: ToglBasicSeries;
begin
  for i:=0 to FSeriesList.Count -1 do begin
    ser := GetSeries(i);
    if ser.Active then ser.Draw;
  end;
end;

procedure ToglChart.DrawWallsAndAxes;

  function GetVertex(AFaceIndex, AVertixIndex: Integer): TPoint3f;
  begin
    Result := CUBE_VERTICES[QUAD_FACES[AFaceIndex, AVertixIndex]];
    Result.x := IfThen(Result.x < 0, FImgExtent.a.x, FImgExtent.b.x);
    Result.y := IfThen(Result.y < 0, FImgExtent.a.y, FImgExtent.b.y);
    Result.z := IfThen(Result.z < 0, FImgExtent.a.z, FImgExtent.b.z);
  end;

  function GetWallColor(AFaceIndex: Integer; out AColor: TArray4f): Boolean;
  var
    wall: ToglWall;
  begin
    case QUAD_FACE_KINDS[AFaceIndex] of
      fkXY: wall := FBottomWall;
      fkXZ: wall := FBackWall;
      fkYZ: wall := FLeftWall;
    end;
    if wall.Visible then begin
      AColor := Array4f(Red(wall.Color)/255, Green(wall.Color)/255, Blue(wall.Color)/255, 1.0);
      Result := true;
    end else
      Result := false;
  end;

  procedure SelectAxis(AFaceIndex: Integer; ACommonPoint: TPoint3f;
    out AxisKind: TAxisKind; out P1, P2: TPoint3f);
  var
    x, y, z: GLfloat;
  begin
    case QUAD_FACE_KINDS[AFaceIndex] of
      fkXY: begin
              AxisKind := akX;
              y := IfThen(ACommonPoint.y = +1, FImgExtent.a.y, FImgExtent.b.y);
              z := IfThen(ACommonPoint.z = -1, FImgExtent.a.z, FImgExtent.b.z);
              P1 := Point3f(FImgExtent.a.x, y, z);
              P2 := Point3f(FImgExtent.b.x, y, z);
            end;
      fkYZ: begin
              AxisKind := akY;
              z := IfThen(ACommonPoint.z = -1, FImgExtent.a.z, FImgExtent.b.z);
              x := IfThen(ACommonPoint.x = +1, FImgExtent.a.x, FImgExtent.b.x);
              P1 := Point3f(x, FImgExtent.a.y, z);
              P2 := Point3f(x, FImgExtent.b.y, z);
            end;
      fkXZ: begin
              AxisKind := akZ;
              x := IfThen(ACommonPoint.x = +1, FImgExtent.a.x, FImgExtent.b.x);
              y := IfThen(ACommonPoint.y = -1, FImgExtent.a.y, FImgExtent.b.y);
              P1 := Point3f(x, y, FImgExtent.a.z);
              P2 := Point3f(x, y, FImgExtent.b.z);
            end;
    end;
  end;

const
  EPS = 1e-6;
var
  i: Integer;
  M: TMatrix4f;
  v, n: TVector3f;
  P1, P2, P3, P4: TPoint3f;
  c: TArray4f;
  hasColorMaterial: Boolean;
  hasPolygonOffsetFill: Boolean;
  faceVisible: array[0..5] of boolean;
  commonPoint: TPoint3f;    // vertex common to all three walls
  axisKind: TAxisKind;
begin
  hasColorMaterial := (glIsEnabled(GL_COLOR_MATERIAL) = GL_TRUE);
  if not hasColorMaterial then
    glEnable(GL_COLOR_MATERIAL);

  hasPolygonOffsetFill := (glIsEnabled(GL_POLYGON_OFFSET_FILL) = GL_TRUE);
  if not hasPolygonOffsetFill then begin
    glEnable(GL_POLYGON_OFFSET_FILL);
    glPolygonOffset(1.0, 1.0);   // move polygon backward
  end;

  // There are always two walls of each kind, on the positive or negative axis.
  // Select the one which is farthest away, i.e. the one who's normal points into
  // the screen.
  glGetFloatv(GL_MODELVIEW_MATRIX, @M);
  v := M * Vector3f(0, 0, -1);
  for i:=0 to High(QUAD_FACES) do begin
    n := QUAD_FACE_NORMALS[i];
    faceVisible[i] := (dot(v, n) > EPS);
    if faceVisible[i] then
      case QUAD_FACE_KINDS[i] of
        fkXY: commonPoint.z := IfThen(odd(i), +1, -1);
        fkYZ: commonPoint.x := IfThen(odd(i), +1, -1);
        fkXZ: commonPoint.y := IfThen(odd(i), +1, -1);
      end;
  end;

  for i:=0 to High(QUAD_FACES) do begin
    if faceVisible[i] then begin
      // Draw axis
      SelectAxis(i, commonPoint, axisKind, P1, P2);
      GetAxis(axisKind).Draw(P1, P2, i);

      WriteLn(' **** ', i, ' ', axisKind);

      // Draw walls
      if GetWallColor(i, c) then begin
        glColor3fv(@c);
        P1 := GetVertex(i, 0);
        P2 := GetVertex(i, 1);
        P3 := GetVertex(i, 2);
        P4 := GetVertex(i, 3);
        glBegin(GL_POLYGON);
          glNormal3fv(@n);
          glVertex3fv(@P1);
          glVertex3fv(@P2);
          glVertex3fv(@P3);
          glVertex3fv(@P4);
        glEnd;
      end;
    end;
  end;

  writeln;


  if not hasPolygonOffsetFill then
    glDisable(GL_POLYGON_OFFSET_FILL);
  if not hasColorMaterial then
    glDisable(GL_COLOR_MATERIAL);
end;

procedure ToglChart.DummyExtent;
begin
  FFullExtent := FImgExtent;
end;

procedure ToglChart.EmptyExtent;
begin
  FFullExtent.a := Point3f(+Infinity, +Infinity, +Infinity);
  FFullExtent.b := Point3f(-Infinity, -Infinity, -Infinity);
end;

function ToglChart.GetAxis(AKind: TAxisKind): ToglChartAxis;
begin
  case AKind of
    akX: Result := FXAxis;
    akY: Result := FYAxis;
    akZ: Result := FZAxis;
  end;
end;

function ToglChart.GetSeries(AIndex: Integer): ToglBasicSeries;
begin
  Result := ToglBasicSeries(FSeriesList[AIndex]);
end;

function ToglChart.GetSeriesCount: Integer;
begin
  Result := FSeriesList.Count;
end;

function ToglChart.ImageToWorldX(X: GLfloat): GLfloat;
begin
  if not FScalingValid then
    CalcScaling;
  Result := FFullExtent.a.x + (X - FImgExtent.a.x) * FScaleX;
end;

function ToglChart.ImageToWorldY(Y: GLfloat): GLfloat;
begin
  if not FScalingValid then
    CalcScaling;
  Result := FFullExtent.a.y + (Y - FImgExtent.a.y) * FScaleY;
end;

function ToglChart.ImageToWorldZ(Z: GLfloat): GLfloat;
begin
  if not FScalingValid then
    CalcScaling;
  Result := FFullExtent.a.z + (Z - FImgExtent.a.z) * FScaleZ;
end;

function ToglChart.ImageToWorld(P: TPoint3f): TPoint3f;
begin
  if not FScalingValid then
    CalcScaling;
  Result.X := FFullExtent.a.x + (P.X - FImgExtent.a.x) * FScaleX;
  Result.Y := FFullExtent.a.y + (P.Y - FImgExtent.a.y) * FScaleY;
  Result.Z := FFullExtent.a.z + (P.Z - FImgExtent.a.z) * FScaleZ;
end;

procedure ToglChart.InitGL;
begin
  {
  if FInitDone then
    exit;

  FInitDone := true;
  }
  glClearColor(Red(FBkColor)/255, Green(FBkColor)/255, Blue(FBkColor)/255, 1.0);   // sets background color
  glClearDepth(1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glDepthFunc(GL_LEQUAL);             // the type of depth test to do
  glEnable(GL_DEPTH_TEST);            // enables depth testing
  glShadeModel(GL_SMOOTH);            // enables smooth color shading

  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
end;

procedure ToglChart.InitLights(Attachment: TLightAttachment);
begin
  if not FInitLightsDone then
    FLightSources.Init(Attachment);
end;

procedure ToglChart.InitProjection;
var
  w, h: Integer;
  d: Double;
begin
  w := Width;
  h := Height;
  if Height <= 0 then exit;

  // Set viewport to window dimensions
  glViewport(0, 0, w, h);

  // Reset coordinate system
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;

  // Create perspective projection
  gluPerspective(
    DEFAULT_VIEW_ANGLE,   // field of view (degrees) in y direction
    w/h,                  // aspect ratio
    NEAR_CLIP_DISTANCE,   // distance to near clipping plane
    FAR_CLIP_DISTANCE     // distance to far clipping plane
  );

  // Reset current matrix to MODELVIEW
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure ToglChart.MouseDown(Button: TMouseButton; Shift:TShiftState;
  X,Y:Integer);
begin
  inherited;
  FMousePos := Point(X, Y);
end;

procedure ToglChart.MouseMove(Shift: TShiftState; X,Y: Integer);
const
  MOVE_STEP = 1.0 / 100;
  ROT_STEP = 100;
var
  phi1, phi2, dphi: Double;
  ctr: TPoint;
begin
  inherited;
  if FViewParams.Interactive then begin
    if Shift = [ssLeft] then begin
      RotateY(FViewMatrix, -(X - FMousePos.x) / ROT_STEP);
      RotateX(FViewMatrix, (Y - FMousePos.y) / ROT_STEP);
      Invalidate;
    end else
    if Shift = [ssRight] then begin
      Translate(FViewMatrix, (X - FMousePos.x) * MOVE_STEP, -(Y - FMousePos.y) * MOVE_STEP, 0.0);
      Invalidate;
    end else
    if Shift = [ssMiddle] then begin
      ctr := Point(Width div 2, Height div 2);
      phi1 := arctan2(FMousePos.Y - ctr.Y, FMousePos.X - ctr.X);
      phi2 := arctan2(Y - ctr.Y, X - ctr.X);
      dphi := phi2 - phi1;
      if (phi2 > pi/2) and (phi1 < -pi/2) then
        dphi := 2*pi - dphi
      else
      if (phi1 > pi/2) and (phi2 < -pi/2) then
        dphi := 2*pi + dphi;
      RotateZ(FViewMatrix, -RadToDeg(dphi) / ROT_STEP);
      Invalidate;
    end;
  end;
  FMousePos := Point(X, Y);
end;

procedure ToglChart.RenewExtent;
var
  i: Integer;
  ser: ToglBasicSeries;
begin
  EmptyExtent;
  for i := 0 to FSeriesList.Count - 1 do
    UpdateExtent(ToglBasicSeries(FSeriesList[i]));
  Invalidate;
end;

procedure ToglChart.SetBBoxColor(const AValue: TColor);
begin
  if AValue = FBBoxColor then exit;
  FBBoxColor := AValue;
  Invalidate;
end;

procedure ToglChart.SetBkColor(const AValue: TColor);
begin
  if AValue = FBkColor then exit;
  FBkColor := AValue;
  Invalidate;
end;

procedure ToglChart.SetShowBBox(const AValue: Boolean);
begin
  if AValue = FShowBBox then exit;
  FShowBBox := AValue;
  Invalidate;
end;

procedure ToglChart.SetShowAxes(const AValue: Boolean);
begin
  if AValue = FShowAxes then exit;
  FShowAxes := AValue;
  Invalidate;
end;

procedure ToglChart.Update(ASender: TObject; ACmd: TNotifyCmd; AParam: Pointer);
begin
  case ACmd of
    ncInvalidate:
      Invalidate;
    ncUpdateExtent:
      if (AParam = nil) or (TObject(AParam) is ToglBasicSeries) then
        UpdateExtent(ToglBasicSeries(AParam));
    ncInitLight:
      begin
        FInitLightsDone := false;    // Force running through lights init procedure
        Invalidate;
      end;
    ncView:
      begin
        Identity(FViewMatrix);
        RotateY(FViewMatrix, DegToRad(FViewParams.VertRot));
        RotateX(FViewMatrix, DegToRad(FViewParams.HorRot));
        RotateZ(FViewMatrix, DegToRad(FViewParams.DepthRot));
        FDistance := FViewParams.Distance;
        Invalidate;
      end;
  end;
end;

procedure ToglChart.UpdateExtent(ASeries: ToglBasicSeries);
var
  i: Integer;
  ser: ToglBasicSeries;
begin
  if ASeries = nil then begin
    EmptyExtent;
    for i := 0 to FSeriesList.Count -1 do begin
      ser := ToglBasicSeries(FSeriesList[i]);
      UpdateExtent(ser);
    end;
  end else
  if ASeries.Active then begin
    if ASeries.Extent.a.x < Infinity then FFullExtent.a.x := Min(FFullExtent.a.x, ASeries.Extent.a.x);
    if ASeries.Extent.b.x > -Infinity then FFullExtent.b.x := Max(FFullExtent.b.x, ASeries.Extent.b.x);
    if ASeries.Extent.a.y < Infinity then FFullExtent.a.y := Min(FFullExtent.a.y, ASeries.Extent.a.y);
    if ASeries.Extent.b.y > -Infinity then FFullExtent.b.y := Max(FFullExtent.b.y, ASeries.Extent.b.y);
    if ASeries.Extent.a.z < Infinity then FFullExtent.a.z := Min(FFullExtent.a.z, ASeries.Extent.a.z);
    if ASeries.Extent.b.z > -Infinity then FFullExtent.b.z := Max(FFullExtent.b.z, ASeries.Extent.b.z);
  end;
  FScalingValid := false;
  Invalidate;
end;

function ToglChart.WorldToImageX(X: GLfloat): GLfloat;
begin
  if not FScalingValid then
    CalcScaling;
  Result := FImgExtent.a.x + (X - FFullExtent.a.x) / FScaleX;
end;

function ToglChart.WorldToImageY(Y: GLfloat): GLfloat;
begin
  if not FScalingValid then
    CalcScaling;
  Result := FImgExtent.a.y + (Y - FFullExtent.a.y) / FScaleY;
end;

function ToglChart.WorldToImageZ(Z: GLfloat): GLfloat;
begin
  if not FScalingValid then
    CalcScaling;
  Result := FImgExtent.a.z + (Z - FFullExtent.a.z) / FScaleZ;
end;

function ToglChart.WorldToImage(P: TPoint3f): TPoint3f;
begin
  if not FScalingValid then
    CalcScaling;
  Result.X := FImgExtent.a.x + (P.X - FFullExtent.a.x) / FScaleX;
  Result.Y := FImgExtent.a.y + (P.Y - FFullExtent.a.y) / FScaleY;
  Result.Z := FImgExtent.a.z + (P.Z - FFullExtent.a.z) / FScaleZ;
end;

end.

