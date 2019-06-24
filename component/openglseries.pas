unit OpenGLSeries;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Math, gl, glu,
  OpenGLTypes, OpenGLChart;

type
  ToglChartDataItem = record
    x, y, z: GLfloat;
  end;
  PoglChartDataItem = ^ToglChartDataItem;

  TExtentOperation = (eopAdd, eopDelete, eopClear);

  ToglPointSeries = class(ToglBasicSeries)
  private
    FData: TFPList;
    FSphere: PGLUQuadric;
    FSymbolSize: GLfloat;
    FSymbolColor: TColor;
    function GetCount: Integer;
    function GetValue(AIndex: Integer): TPoint3f;
    function GetXValue(AIndex: Integer): GLfloat;
    function GetYValue(AIndex: Integer): GLfloat;
    function GetZValue(AIndex: Integer): GLfloat;
    procedure SetSymbolColor(const AValue: TColor);
    procedure SetSymbolSize(const AValue: GLfloat);
  protected
    procedure DrawPoints;
    procedure UpdateExtent(Op: TExtentOperation;
      x: GLfloat = 0.0; y: GLfloat = 0.0; z: GLfloat = 0.0);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(x, y, z: GLfloat): Integer;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure Draw; override;
    property Count: Integer read GetCount;
    property Value[AIndex: Integer]: TPoint3f read GetValue;
    property XValue[AIndex: Integer]: GLfloat read GetXValue;
    property YValue[AIndex: Integer]: GLfloat read GetYValue;
    property ZValue[AIndex: Integer]: GLfloat read GetZValue;
  published
    property SymbolColor: TColor read FSymbolColor write SetSymbolColor default clRed;
    property SymbolSize: GLfloat read FSymbolSize write SetSymbolSize default 0.02;
    property Title;
  end;

  ToglLineSeriesStyle = (lssLines, lssPoints, lssLinesAndPoints);

  ToglLineSeries = class(ToglPointSeries)
  private
    FLineColor: TColor;
    FLineWidth: Integer;
    FStyle: ToglLineSeriesStyle;
    procedure SetLineColor(const AValue: TColor);
    procedure SetLineWidth(const AValue: Integer);
    procedure SetStyle(const AValue: ToglLineSeriesStyle);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Draw; override;
  published
    property LineColor: TColor read FLineColor write SetLineColor default clRed;
    property LineWidth: Integer read FLineWidth write SetLineWidth default 1;
    property Style: ToglLineSeriesStyle read FStyle write SetStyle default lssLines;
  end;



implementation

{ ToglPointSeries }

constructor ToglPointSeries.Create(AOwner: TComponent);
begin
  inherited;
  FData := TFPList.Create;
  FSymbolColor := clRed;
  FSymbolSize := 0.02;

  FSphere := gluNewQuadric();
  gluQuadricDrawStyle(FSphere, GLU_FILL);

  EmptyExtent;
end;

destructor ToglPointSeries.Destroy;
begin
  Chart.DeleteSeries(self);
  Clear;
  FData.Free;
  inherited;
end;

function ToglPointSeries.Add(x, y, z: GLfloat): Integer;
var
  item: PoglChartDataItem;
begin
  New(item);
  item^.x := x;
  item^.y := y;
  item^.z := z;
  Result := FData.Add(item);
  UpdateExtent(eopAdd, x, y, z);
end;

procedure ToglPointSeries.Clear;
var
  i: Integer;
  item: PoglChartDataItem;
begin
   for i := 0 to FData.Count - 1 do begin
    item := PoglChartDataItem(FData.Items[i]);
    Dispose(item);
  end;
  FData.Clear;
  if not (csDestroying in ComponentState) then
    UpdateExtent(eopClear);
end;

procedure ToglPointSeries.Delete(AIndex: Integer);
var
  item: PoglChartDataItem;
  x, y, z: GLfloat;
begin
  item := PoglChartDataItem(FData.Items[AIndex]);
  x := item^.X;  y := item^.y;   z := item^.z;
  Dispose(item);
  FData.Delete(AIndex);
  UpdateExtent(eopDelete, x, y, z);
end;

procedure ToglPointSeries.Draw;
begin
  DrawPoints;
end;

procedure ToglPointSeries.DrawPoints;
var
  i: Integer;
  item: PoglChartDataItem;
  P: TPoint3f;
  hasColorMaterial: Boolean;
begin
  hasColorMaterial := (glIsEnabled(GL_COLOR_MATERIAL) = GL_TRUE);
  //glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE);
  glEnable(GL_COLOR_MATERIAL);

  glColor3f(Red(FSymbolColor), Green(FSymbolColor), Blue(FSymbolColor));
  for i:=0 to FData.Count - 1 do begin
    item := PoglChartDataItem(FData[i]);
    P := Chart.WorldToImage(TPoint3f(item^));
    glPushMatrix;
    glTranslatef(P.x, P.y, P.z);
    gluSphere(FSphere, FSymbolSize, 18, 9);
    glPopMatrix;
  end;

  if not hasColorMaterial then glDisable(GL_COLOR_MATERIAL);
end;

function ToglPointSeries.GetCount: Integer;
begin
  Result := FData.Count;
end;

function ToglPointSeries.GetValue(AIndex: Integer): TPoint3f;
var
  item: PoglChartDataItem;
begin
  item := PoglChartDataItem(FData.Items[AIndex]);
  Result := TPoint3f(item^);
end;

function ToglPointSeries.GetXValue(AIndex: Integer): GLfloat;
var
  item: PoglChartDataItem;
begin
  item := PoglChartDataItem(FData.Items[AIndex]);
  Result := item^.x;
end;

function ToglPointSeries.GetYValue(AIndex: Integer): GLfloat;
var
  item: PoglChartDataItem;
begin
  item := PoglChartDataItem(FData.Items[AIndex]);
  Result := item^.y;
end;

function ToglPointSeries.GetZValue(AIndex: Integer): GLfloat;
var
  item: PoglChartDataItem;
begin
  item := PoglChartDataItem(FData.Items[AIndex]);
  Result := item^.z;
end;

procedure ToglPointSeries.SetSymbolColor(const AValue: TColor);
begin
  if FSymbolColor = AValue then exit;
  FSymbolColor := AValue;
  Notify(self, ncInvalidate, nil);
end;

procedure ToglPointSeries.SetSymbolSize(const AValue: TGLfloat);
begin
  if FSymbolSize = AValue then exit;
  FSymbolSize := AValue;
  Notify(self, ncInvalidate, nil);
end;

procedure ToglPointSeries.UpdateExtent(Op: TExtentOperation;
  x: GLfloat = 0.0; y: GLfloat = 0.0; z: GLfloat = 0.0);
var
  item: PoglChartDataItem;
  i: Integer;
begin
  if Op = eopAdd then begin
    FExtent.a.x := Min(FExtent.a.x, x);
    FExtent.b.x := Max(FExtent.b.x, x);

    FExtent.a.y := Min(FExtent.a.y, y);
    FExtent.b.y := Max(FExtent.b.y, y);

    FExtent.a.z := Min(FExtent.a.z, z);
    FExtent.b.z := Max(FExtent.b.z, z);
  end
  else
  if Op = eopClear then
    EmptyExtent
  else
  if Op = eopDelete then begin
    if FData.Count = 0 then
      EmptyExtent
    else begin
      if (x = FExtent.a.x) or (x = FExtent.b.x) or     // Delete current max or min
         (y = FExtent.a.y) or (y = FExtent.b.y) or
         (z = FExtent.a.z) or (z = FExtent.b.z) then
      begin
        EmptyExtent;
        for i := 0 to Count do begin
          item := PoglChartDataItem(FData.Items[i]);
          FExtent.a.x := Min(FExtent.a.x, item^.x);
          FExtent.b.x := Max(FExtent.b.x, item^.x);

          FExtent.a.y := Min(FExtent.a.y, item^.y);
          FExtent.b.y := Max(FExtent.b.y, item^.y);

          FExtent.a.z := Min(FExtent.a.z, item^.z);
          FExtent.b.z := Max(FExtent.b.z, item^.z);
        end;
      end;
    end;
  end;
  Notify(self, ncUpdateExtent, self);
end;


{ ToglLineSeries }

constructor ToglLineSeries.Create(AOwner: TComponent);
begin
  inherited;
  FLineColor := clRed;
  FLineWidth := 1;
  FStyle := lssLines;
end;

procedure ToglLineSeries.Draw;
var
  i: Integer;
  item: PoglChartDataItem;
  P: TPoint3f;
  hasLighting: Boolean;
  hasBlend: Boolean;
begin
  if FData.Count = 0 then
    exit;

  if FStyle in [lssLines, lssLinesAndPoints] then begin
    // Turn off lighting to display simple lines
    hasLighting := (glIsEnabled(GL_LIGHTING) = GL_TRUE);
    glDisable(GL_LIGHTING);

    // Turn on blending mode for smoother lines
    hasBlend := (glIsEnabled(GL_BLEND) = GL_TRUE);
    glEnable(GL_BLEND);

    glColor3f(Red(FSymbolColor), Green(FSymbolColor), Blue(FSymbolColor));
    glLineWidth(FLineWidth);
    glBegin(GL_LINE_STRIP);
    for i:=0 to FData.Count - 1 do begin
      item := PoglChartDataItem(FData[i]);
      P := Chart.WorldToImage(TPoint3f(item^));
      glVertex3f(P.x, P.y, P.z);
    end;
    glEnd;

    if hasLighting then glEnable(GL_LIGHTING);
    if not hasBlend then glDisable(GL_BLEND);
  end;

  if FStyle in [lssPoints, lssLinesAndPoints] then
    inherited;
end;

procedure ToglLineSeries.SetLineColor(const AValue: TColor);
begin
  if FLineColor = AValue then exit;
  FLineColor := AValue;
  Notify(self, ncInvalidate, nil);
end;

procedure ToglLineSeries.SetLineWidth(const AValue: Integer);
begin
  if FLineWidth = AValue then exit;
  FLineWidth := AValue;
  Notify(self, ncInvalidate, nil);
end;

procedure ToglLineSeries.SetStyle(const AValue: ToglLineSeriesStyle);
begin
  if FStyle = AValue then exit;
  FStyle := AValue;
  Notify(self, ncInvalidate, nil);
end;

end.

