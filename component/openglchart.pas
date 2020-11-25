unit OpenGLChart;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Graphics,
  gl, glu, openglcontext,
  OpenGLTypes, OpenGLMath, OpenGLAxis, OpenGLLightSources, OpenGLColorTheme;

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
    FSeriesColor: TColor;
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
    FReferToData: Boolean;
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
    property ReferToData: Boolean read FReferToData write FReferToData default false;
    property VertRot: GLfloat read FVertRot write SetVertRot;
  end;

  ToglChart = class(ToglBasicChart)
  private
    FBkColor: TColor;
    FFrameColor: TColor;
    FFullExtent: TRect3f;
    FImgExtent: TRect3f;
    FBoundingBox: TQuad3f;
    FLightSources: ToglLightSources;
    FScaleX, FScaleY, FScaleZ: GLfloat;
    FScalingValid: Boolean;
    FSeriesList: TFPList;
    FShowAxes: Boolean;
    FShowFrame: Boolean;
    FViewParams: ToglViewParams;
    FBackWall: ToglWall;
    FLeftWall: ToglWall;
    FBottomWall: ToglWall;
    FXAxis: ToglChartAxis;
    FYAxis: ToglChartAxis;
    FZAxis: ToglChartAxis;
    FColorTheme: ToglChartColorTheme;
    function GetMaxImgExtent: GLfloat;
    function GetSeries(AIndex: Integer): ToglBasicSeries;
    function GetSeriesCount: Integer;
    procedure SetBkColor(const AValue: TColor);
    procedure SetFrameColor(const AValue: TColor);
    procedure SetColorTheme(const AValue: ToglChartColorTheme);
    procedure SetShowAxes(const AValue: Boolean);
    procedure SetShowFrame(const AValue: Boolean);
  protected
    FDistance: GLfloat;
    FInitDone: Boolean;
    FInitLightsDone: Boolean;
    FMousePos: TPoint;
    FViewMatrix: TMatrix4f;
    procedure ApplyColorTheme;
    procedure CalcScaling;
    procedure ColorThemeChangedHandler(Sender: TObject; AColor: ToglColorThemeItem);
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DoOnResize; override;
    procedure DrawAxes;
    procedure DrawChart; virtual;
    procedure DrawFrame;
    procedure DrawSeries;
    procedure DrawWallsAndAxes;
    procedure DummyExtent;
    procedure EmptyExtent;
    procedure InitAxes;
    procedure InitGL; virtual;
    procedure InitLights;
    procedure InitLightPositions(Attachment: TLightAttachment);
    procedure InitProjection; virtual;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure RenewExtent;
    procedure UpdateExtent(ASeries: ToglBasicSeries);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddSeries(ASeries: ToglBasicSeries): Integer;
    procedure ClearSeries;
    procedure DeleteSeries(ASeries: ToglBasicSeries);

    procedure DoOnPaint; override;
    procedure Update(ASender: TObject; ACmd: TNotifyCmd; AParam: Pointer); override;

    function CurrentBoundingBox: TQuad3f;
    function GetAxis(AKind: TAxisKind): ToglChartAxis;
    function GetProjectedBoundingBox: TProjectedQuad;

    function ImageToWorldX(X: GLfloat): GLfloat;
    function ImageToWorldY(Y: GLfloat): GLfloat;
    function ImageToWorldZ(Z: GLfloat): GLfloat;
    function ImageToWorld(P: TPoint3f): TPoint3f;

    function WorldToImageX(X: GLfloat): GLfloat;
    function WorldToImageY(Y: GLfloat): GLfloat;
    function WorldToImageZ(Z: GLfloat): GLfloat;
    function WorldToImage(P: TPoint3f): TPoint3f;

    property FullExtent: TRect3f read FFullExtent;
    property ImgExtent: TRect3f read FImgExtent;
    property MaxImgExtent: GLfloat read GetMaxImgExtent;
    property Series[AIndex: Integer]: ToglBasicSeries read GetSeries;
    property SeriesCount: Integer read GetSeriesCount;
  published
    property BackWall: ToglWall read FBackWall write FBackWall;
    property BottomWall: ToglWall read FBottomWall write FBottomWall;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clGray;
    property Color: TColor read FBkColor write SetBkColor default clBlack;
    property ColorTheme: ToglChartColorTheme read FColorTheme write SetColorTheme default nil;
    property LeftWall: ToglWall read FLeftWall write FLeftWall;
    property LightSources: ToglLightSources read FLightSources write FLightSources;
    property ShowAxes: Boolean read FShowAxes write SetShowAxes default false;
    property ShowFrame: Boolean read FShowFrame write SetShowFrame default false;
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
  FReferToData := false;
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

  FImgExtent.a := Point3f(-1.0, -1.0, -1.0); //-0.5);
  FImgExtent.b := Point3f(+1.0, +1.0, +1.0); //+0.5);
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

  FFrameColor := clGray;
  FBkColor := clBlack;
  FShowAxes := false;
  FShowFrame := false;

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

procedure ToglChart.ApplyColorTheme;

  procedure SetSeriesColors(ASeries: ToglBasicSeries; AColor1, AColor2: TColor);
  begin
    if ASeries is ToglLineSeries then begin
      ToglLineSeries(ASeries).SymbolColor := AColor1;
      ToglLineSeries(ASeries).LineColor := AColor2;
    end
    else if ASeries is ToglPointSeries then
      ToglPointSeries(ASeries).SymbolColor := AColor1
    else if ASeries is ToglFuncSeries then begin
      ToglFuncSeries(ASeries).FillColor := AColor1;
    end else
      raise Exception.Create('[ApplyColors] Series type not supported.');
  end;

var
  i, j: Integer;
begin
  FBkColor := FColorTheme.Background;
  FFrameColor := FColorTheme.Frame;
  FBackWall.Color := FColorTheme.BackWall;
  FLeftWall.Color := FColorTheme.LeftWall;
  FBottomWall.Color := FColorTheme.BottomWall;
  FXAxis.LineColor := FColorTheme.XAxisLine;
  FYAxis.LineColor := FColorTheme.YAxisLine;
  FZAxis.LineColor := FColorTheme.ZAxisLine;
  FXAxis.LabelFontColor := FColorTheme.XAxisLabels;
  FYAxis.LabelFontColor := FColorTheme.YAxisLabels;
  FZAxis.LabelFontColor := FColorTheme.ZAxisLabels;
  FXAxis.Title.FontColor := FColorTheme.XAxisTitle;
  FYAxis.Title.FontColor := FColorTheme.YAxisTitle;
  FZAxis.Title.FontColor := FColorTheme.ZAxisTitle;
  for i:=0 to SeriesCount-1 do begin
    j := i mod SeriesCount;
    SetSeriesColors(
      Series[j],
      FColorTheme[ToglColorThemeItem(j + ord(tiSeries1 ))],
      FColorTheme[ToglColorThemeItem(j + ord(tiSeries1a))]
    );
  end;
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

procedure ToglChart.ColorThemeChangedHandler(Sender: TObject;
  AColor: ToglColorThemeItem);
begin
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
  // Initialize the lights that are attached to the camera
  InitLightPositions(laCamera);

  glClearColor(Red(FBkColor)/255, Green(FBkColor)/255, Blue(FBkColor)/255, 1.0);   // sets background color
  glClearDepth(1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glPushMatrix;

  glTranslatef(0, 0, -FDistance);
  glMultMatrixf(@FViewMatrix);
  // Rotate world so that z points upward, x to the right, y into the screen
  glRotatef(-90, 1, 0, 0);

  // Initialize the lights that are attached to the model
  InitLightPositions(laModel);
  //FInitLightsDone := true;

  DrawChart;

  glPopMatrix;
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

procedure ToglChart.DrawFrame;
var
  P1, P2: TPoint3f;
  hasBlend, hasLighting: Boolean;
begin
  if not ShowFrame then exit;

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
  SetOpenGLColor(FFrameColor);

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

  InitAxes;

  WriteLn('x axis going from index ', FXAxis.StartIndex, ' to ', FXAxis.EndIndex);
  WriteLn('y axis going from index ', FYAxis.StartIndex, ' to ', FYAxis.EndIndex);
  WriteLn('z axis going from index ', FZAxis.StartIndex, ' to ', FZAxis.EndIndex);

  if FViewParams.ReferToData then
    glPushMatrix;

  DrawWallsAndAxes;
  DrawSeries;

  if FViewParams.ReferToData then
    glPopMatrix;

  DrawFrame;
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

  WriteLn('VISIBLE FACES:');
  for i:=0 to High(QUAD_FACES) do begin
    if faceVisible[i] then begin

      Writeln('  i=', i, ': ', QUAD_FACE_NAMES[i]);

      // Draw axis
      SelectAxis(i, commonPoint, axisKind, P1, P2);
      GetAxis(axisKind).Draw; //(P1, P2, i);

  //    WriteLn(' **** ', i, ' ', axisKind);

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

 // writeln;


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

function ToglChart.GetProjectedBoundingBox: TProjectedQuad;
var
  i: Integer;
  viewport: Array[0..3] of Integer;
  mProj: Array[0..3, 0..3] of double;       // MUST be double
  mModelView: Array[0..3, 0..3] of double;  // MUST be double
  x, y, z: Double;
  xs, ys, zs: Double;                       // MUST be double
begin
  // Get current modelview matrix
  glGetDoublev(GL_MODELVIEW_MATRIX, @mModelView);
  // Get current project matrix
  glGetDoublev(GL_PROJECTION_MATRIX, @mProj);
  // Get viewport
  glGetIntegerV(GL_VIEWPORT, @viewport);
  // map object coordinates to window coordinates
  for i:=0 to 7 do begin
    x := IfThen(CUBE_VERTICES[i].x < 0, FImgExtent.a.x, FImgExtent.b.x);
    y := IfThen(CUBE_VERTICES[i].y < 0, FImgExtent.a.y, FImgExtent.b.y);
    z := IfThen(CUBE_VERTICES[i].z < 0, FImgExtent.a.z, FImgExtent.b.z);
    gluProject(x, y, z, @mModelView, @mProj, @viewport, @xs, @ys, @zs);
    Result[i] := Point(round(xs), round(ys));
  end;
                        {
  SetFont('Arial', 12, []);
  SetOpenGLColor(clWhite);
  for i:=0 to 7 do begin
    x := IfThen(CUBE_VERTICES[i].x < 0, FImgExtent.a.x, FImgExtent.b.x)*1.1;
    y := IfThen(CUBE_VERTICES[i].y < 0, FImgExtent.a.y, FImgExtent.b.y)*1.1;
    z := IfThen(CUBE_VERTICES[i].z < 0, FImgExtent.a.z, FImgExtent.b.z)*1.1;
    DrawText2d(x, y, z, IntToStr(i), []);
  end;
  }
end;

{ Find the cube vertex indices by which each axis is defined. }
procedure ToglChart.InitAxes;
var
  Q: TProjectedQuad;
  QCtr: TPoint;
  TransformedNormals: Array[0..7] of TVector3f;
  x1,x2, y1,y2, z1,z2: Integer;

  procedure CalcTransformedNormals;
  var
    M: TMatrix4f;
    i: Integer;
  begin
    // Get current modelview matrix
    glGetFloatv(GL_MODELVIEW_MATRIX, @M);
    for i:=0 to 7 do
      TransformedNormals[i] := M * QUAD_FACE_NORMALS[i];
  end;

  function OnSameEdge(P1, P2: TPoint3f; out AxisKind: TAxisKind): Boolean;
  begin
    Result := true;
    if (P1.x = P2.x) and (P1.y = P2.y) and (P1.z = -P2.z) then begin
      AxisKind := akZ;
      exit;
    end;
    if (P1.y = P2.y) and (P1.z = P2.z) and (P1.x = -P2.x) then begin
      AxisKind := akX;
      exit;
    end;
    if (P1.z = P2.z) and (P1.x = P2.x) and (P1.y = -P2.y) then begin
      AxisKind := akY;
      exit;
    end;
    Result := false;
  end;

  // Angle to the negative y axis for vector going from P1 to P2.
  function VertAngle(P1, P2: TPoint): GLfloat;
  begin
    Result := arctan2(P2.Y - P1.Y, P2.X - P1.X) + pi/2;
    if Result < 0 then Result := Result + 2*pi;
  end;

  procedure MakeAxis(AxisKind: TAxisKind; idx, other: Integer);
  begin
    case AxisKind of
      akX: if CUBE_VERTICES[idx].x < 0 then
           begin
             x1 := idx; x2 := other;
           end else begin
             x1 := other; x2 := idx;
           end;
      akY: if CUBE_VERTICES[idx].y < 0 then begin
             y1 := idx; y2 := other;
           end else begin
             y1 := other; y2 := idx;
           end;
      akZ: if CUBE_VERTICES[idx].z < 0 then begin
             z1 := idx; z2 := other;
           end else begin
             z1 := other; z2 := idx;
           end;
    end;
  end;

  procedure FindWritingFace(AxisKind: TAxisKind; AStartIndex, AEndIndex: Integer;
    out AFaceNormal, AOtherFaceNormal: TVector3f);
  var
    i, j: Integer;
    found1, found2: Boolean;
    fni1, fni2: Integer;   // face normal index
    n1, n2: TVector3f;
    screenNormal: TVector3f;
  begin
    fni1 := -1;
    fni2 := -1;
    for i:=0 to 5 do begin
      found1 := false;
      for j:=0 to 3 do
        if (AStartIndex = QUAD_FACES[i, j]) then begin
          found1 := true;
          break;
        end;
      if found1 then begin
        found2 := false;
        for j := 0 to 3 do
          if (AEndIndex = QUAD_FACES[i, j]) then begin
            found2 := true;
            if fni1 = -1 then
              fni1 := i
            else
              fni2 := i;
            break;
          end;
      end;
      if found2 and (fni2 <> -1) then
        break;
    end;

    n1 := TransformedNormals[fni1];
    if n1.y > 0 then begin
      n1 := n1*(-1);
      if odd(fni1) then dec(fni1) else inc(fni1);
    end;

    n2 := TransformedNormals[fni2];
    if n2.y > 0 then begin
      n2 := n2*(-1);
      if odd(fni2) then dec(fni2) else inc(fni2);
    end;

    screenNormal := Vector3f(0, 1, 0);
    if Dot(n1, screenNormal) > Dot(n2, screenNormal) then begin
      AFaceNormal := QUAD_FACE_NORMALS[fni1];
      AOtherFaceNormal := QUAD_FACE_NORMALS[fni2];
    end else begin
      AFaceNormal := QUAD_FACE_NORMALS[fni2];
      AOtherFaceNormal := QUAD_FACE_NORMALS[fni1];
    end;
  end;

  function CalcAxisPosition(AStartIdx, AEndIdx: Integer): TAxisPosition;
  var
    c: TPoint;
  begin
    c.x := (Q[AStartIdx].x + Q[AEndIdx].x) div 2;
    c.y := (Q[AStartIdx].y + Q[AEndIdx].y) div 2;
    if (c.x < QCtr.x) then
      Result := apLeft
    else
      Result := apRight;
  end;

var
  i: Integer;
  a, b: Integer;
  L, R: Integer;
  AxisKind1, AxisKind2, AxisKind3, ak: TAxisKind;
//  akA, akB: TAxisKind;
  idx0, idx1, idx2, idx3: Integer;
  faceX1, faceX2, faceY1, faceY2, faceZ1, faceZ2: TVector3f;
  phi, phiMin: GLfloat;
begin
  x1 := -1; x2 := -1;
  y1 := -1; y2 := -1;
  z1 := -1; z2 := -1;

  // Calculate projected bounding box (ImgExtent quad), in screen coordinates.
  Q := GetProjectedBoundingBox;
  // Center of projected bounding box on screen.
  QCtr := (Q[0] + Q[1] + Q[2] + Q[3] + Q[4] + Q[5] + Q[6] + Q[7]) * (1/8);
  // Normals for bounding box after ModelView transformations
  CalcTransformedNormals;

  for i:=0 to 7 do
    WriteLn('i=', i, ': PROJ x=', Q[i].x, ' y=', Q[i].y, '   CUBE: x=', CUBE_VERTICES[i].x:0:0, ' y=', CUBE_VERTICES[i].y:0:0, ' z=', CUBE_VERTICES[i].z:0:0);

  // The axes are defined by points of the convex hull.
  // Find left-most point of the projected bounding box quad. This point must be
  // on the convex hull.
  idx0 := -1;
  L := MaxInt;
  for i:=0 to 7 do
    if Q[i].x < L then begin
      idx0 := i;
      L := Q[i].x;
    end;

  // Find the other point of the line connecting to the leftmost point. For being
  // on the convex hull, it must be the point for which the angle to the first
  // point measured from the down-pointing vertical is smallest.
  phiMin := 9999;
  for i := 0 to 7 do
    if (i <> idx0) and OnSameEdge(CUBE_VERTICES[idx0], CUBE_VERTICES[i], ak) then
    begin
      phi := VertAngle(Q[idx0], Q[i]);
      if phi < phiMin then begin
        idx1 := i;
        AxisKind1 := ak;
        phiMin := phi;
      end;
    end;

  // Find the next point on the convex hull by following the same strategy
  phiMin := 9999;
  for i := 0 to 7 do begin
    if (i = idx0) or (i = idx1) then
      Continue;
    if OnSameEdge(CUBE_VERTICES[idx1], CUBE_VERTICES[i], ak) then
    begin
      phi := VertAngle(Q[idx1], Q[i]);
      if phi < phiMin then begin
        idx2 := i;
        AxisKind2 := ak;
        phiMin := phi;
      end;
    end;
  end;

  // Find the last point on the convex hull
  phiMin := 9999;
  for i := 0 to 7 do begin
    if (i = idx0) or (i = idx1) or (i = idx2) then
      Continue;
    if OnSameEdge(CUBE_VERTICES[idx2], CUBE_VERTICES[i], ak) then
    begin
      phi := VertAngle(Q[idx2], Q[i]);
      if phi < phiMin then begin
        idx3 := i;
        AxisKind3 := ak;
        phiMin := phi;
      end;
    end;
  end;

  WriteLn('idx0=', idx0, ' idx1=',idx1, ' idx2=', idx2, ' idx3=', idx3);

  MakeAxis(AxisKind1, idx0, idx1);
  MakeAxis(AxisKind2, idx1, idx2);
  MakeAxis(AxisKind3, idx2, idx3);

  FindWritingFace(akX, x1, x2, faceX1, faceX2);
  FindWritingFace(akY, y1, y2, faceY1, faceY2);
  FindWritingFace(akZ, z1, z2, faceZ1, faceZ2);

  FXAxis.InitParams(x1, x2, CalcAxisPosition(x1, x2), faceX1, faceX2);
  FYAxis.InitParams(y1, y2, CalcAxisPosition(y1, y2), faceY1, faceY2);
  FZAxis.InitParams(z1, z2, CalcAxisPosition(z1, z2), faceZ1, faceZ2);

  WriteLn('Writing faces x axis: (',faceX1.x:0:0,',',faceX1.y:0:0,',',faceX1.z:0:0,'); (',faceX2.x:0:0,',',faceX2.y:0:0,',',faceX2.z:0:0);
  WriteLn('Writing faces y axis: (',faceY1.x:0:0,',',faceY1.y:0:0,',',faceY1.z:0:0,'); (',faceY2.x:0:0,',',faceY2.y:0:0,',',faceY2.z:0:0);
  WriteLn('Writing faces z axis: (',faceZ1.x:0:0,',',faceZ1.y:0:0,',',faceZ1.z:0:0,'); (',faceZ2.x:0:0,',',faceZ2.y:0:0,',',faceZ2.z:0:0);
end;

function ToglChart.GetMaxImgExtent: GLfloat;
begin
  Result := MaxValue([
    FImgExtent.b.x - FImgExtent.a.x,
    FImgExtent.b.y - FImgExtent.a.y,
    FImgExtent.b.z - FImgExtent.a.z
  ]);
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
  if FInitDone then
    exit;

  glClearColor(Red(FBkColor)/255, Green(FBkColor)/255, Blue(FBkColor)/255, 1.0);   // sets background color
  glClearDepth(1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glDepthFunc(GL_LEQUAL);             // the type of depth test to do
  glEnable(GL_DEPTH_TEST);            // enables depth testing
  glShadeModel(GL_SMOOTH);            // enables smooth color shading

  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

  InitLights;

  FInitDone := true;
end;

procedure ToglChart.InitLights;
begin
  if not FInitLightsDone then begin
    FLightSources.Init;
    FInitLightsDone := true;
  end;
end;

procedure ToglChart.InitLightPositions(Attachment: TLightAttachment);
begin
  FLightSources.InitPositions(Attachment);
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
  MOVE_SENS = 0.01;
  ROT_SENS = 100;
var
  phi1, phi2, dphi: Double;
  ctr: TPoint;
begin
  inherited;
  if FViewParams.Interactive then begin
    if Shift = [ssLeft] then begin
      RotateY(FViewMatrix, -(X - FMousePos.x) / ROT_SENS);
      RotateX(FViewMatrix, (Y - FMousePos.y) / ROT_SENS);
      Invalidate;
    end else
    if Shift = [ssRight] then begin
      Translate(FViewMatrix, (X - FMousePos.x) * MOVE_SENS, -(Y - FMousePos.y) * MOVE_SENS, 0.0);
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
      RotateZ(FViewMatrix, -RadToDeg(dphi) / ROT_SENS);
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

procedure ToglChart.SetBkColor(const AValue: TColor);
begin
  if AValue = FBkColor then exit;
  FBkColor := AValue;
  Invalidate;
end;

procedure ToglChart.SetColorTheme(const AValue: ToglChartColorTheme);
begin
  if AValue = FColorTheme then exit;
  FColorTheme := AValue;
  FColorTheme.OnChange := @ColorThemeChangedHandler;
  ApplyColorTheme;
end;

procedure ToglChart.SetFrameColor(const AValue: TColor);
begin
  if AValue = FFrameColor then exit;
  FFrameColor := AValue;
  Invalidate;
end;

procedure ToglChart.SetShowFrame(const AValue: Boolean);
begin
  if AValue = FShowFrame then exit;
  FShowFrame := AValue;
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

