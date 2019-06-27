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
    property Active;
    property SymbolColor: TColor read FSymbolColor write SetSymbolColor default clRed;
    property SymbolSize: GLfloat read FSymbolSize write SetSymbolSize default 0.02;
    property Title;
  end;

  ToglLineSeriesStyle = (lssLines, lssPoints, lssLinesAndPoints);

  ToglLineSeries = class(ToglPointSeries)
  private
    FLineColor: TColor;
    FLineWidth: GLfloat;
    FStyle: ToglLineSeriesStyle;
    procedure SetLineColor(const AValue: TColor);
    procedure SetLineWidth(const AValue: GLfloat);
    procedure SetStyle(const AValue: ToglLineSeriesStyle);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Draw; override;
  published
    property Active;
    property LineColor: TColor read FLineColor write SetLineColor default clRed;
    property LineWidth: Glfloat read FLineWidth write SetLineWidth default 1;
    property Style: ToglLineSeriesStyle read FStyle write SetStyle default lssLines;
  end;

  ToglPaletteItem = class(TCollectionItem)
  private
    FColor: TColor;
    FValue: GLfloat;
    FOnChange: TNotifyEvent;
    procedure SetColor(const AValue: TColor);
    procedure SetValue(const AValue: GLfloat);
  protected
    procedure Changed;
    function GetSeries: ToglBasicSeries;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(ASource: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property Value: GLfloat read FValue write SetValue default 0.0;
  end;

  ToglPalette = class(TCollection)
  private
    FLocked: Integer;
    function GetItem(AIndex: Integer): ToglPaletteItem;
    procedure SetItem(AIndex: Integer; const AValue: ToglPaletteItem);
  protected
    FSeries: ToglBasicSeries;
    procedure DoChanged(Sender: TObject);
  public
    constructor Create(ASeries: ToglBasicSeries);
    function Add: ToglPaletteItem;
    function Add(AValue: GLfloat; AColor: TColor): ToglPaletteItem;
    procedure BeginUpdate;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure EndUpdate;
    function Interpolate(AValue, AMinValue, AMaxValue: GLfloat): TColor;
    property Series: ToglBasicSeries read FSeries;
    property Items[AIndex: Integer]: ToglPaletteItem read GetItem write SetItem; default;
  published
  end;

  TCalculateEvent = function(X, Y: GLfloat): GLfloat of object;
  TDrawMode = (dmFilled, dmWireFrame, dmFilledAndWireFrame);

  ToglFuncSeries = class(ToglBasicSeries)
  private
    FXCount: Integer;
    FYCount: Integer;
    FVertices: array of array of TVector3f;
    FNormals: array of array of TVector3f;
    FColors: array of array of TArray4f;
    FDrawMode: TDrawMode;
    FFillColor: TColor;
    FPalette: ToglPalette;
    FUseColorPalette: Boolean;
    FValidNormals: Boolean;
    FValidColors: Boolean;
    FWireFrameLineWidth: Integer;
    FWireFrameLineColor: TColor;
    FOnCalculate: TCalculateEvent;
    function GetCount(AIndex: Integer): Integer;
    function GetMaxMin(AIndex: Integer): GLfloat;
    procedure SetCount(AIndex: Integer; const AValue: Integer);
    procedure SetDrawMode(const AValue: TDrawMode);
    procedure SetFillColor(const AValue: TColor);
    procedure SetMaxMin(AIndex: Integer; const AValue: GLfloat);
    procedure SetOnCalculate(const AValue: TCalculateEvent);
    procedure SetUseColorPalette(const AValue: Boolean);
    procedure SetWireFrameLineColor(const AValue: TColor);
    procedure SetWireFrameLineWidth(const AValue: Integer);
  protected
    procedure CalcColors;
    procedure CalcFunc;
    procedure CalcNormals;
    procedure DrawFilled;
    procedure DrawWireFrame;
    procedure UpdateExtent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw; override;
    procedure UpdatePalette; override;
  published
    property Active;
    property ColorPalette: ToglPalette read FPalette;
    property DrawMode: TDrawMode read FDrawMode write SetDrawMode default dmFilled;
    property FillColor: TColor read FFillColor write SetFillColor default $C0C0C0;
    property Title;
    property UseColorPalette: Boolean read FUseColorPalette write SetUseColorPalette default false;
    property WireFrameLineColor: TColor read FWireFrameLineColor write SetWireFrameLineColor default clBlack;
    property WireFrameLineWidth: Integer read FWireFrameLineWidth write SetWireFrameLineWidth default 2;

    property XCount: Integer index 0 read GetCount write SetCount;
    property YCount: Integer index 1 read GetCount write SetCount;

    property XMax: GLfloat index 0 read GetMaxMin write SetMaxMin;
    property XMin: GLfloat index 1 read GetMaxMin write SetMaxMin;
    property YMax: GLfloat index 2 read GetMaxMin write SetMaxMin;
    property YMin: GLfloat index 3 read GetMaxMin write SetMaxMin;

    property OnCalculate: TCalculateEvent read FOnCalculate write SetOnCalculate;
  end;


implementation

uses
  OpenGLUtils, OpenGLMath;


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

  SetOpenGLColor(FSymbolColor);
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

    SetOpenGLColor(FLineColor);
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

procedure ToglLineSeries.SetLineWidth(const AValue: GLfloat);
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


{ ToglPaletteItem }

constructor ToglPaletteItem.Create(ACollection: TCollection);
begin
  inherited;
  FColor := clBlack;
  FValue := 0.0;
end;

procedure ToglPaletteItem.Assign(ASource: TPersistent);
begin
  if ASource is ToglPaletteItem then begin
    FColor := ToglPaletteItem(ASource).Color;
    FValue := ToglPaletteItem(ASource).Value;
    Changed;
  end else
    inherited;
end;

procedure ToglPaletteItem.Changed;
begin
  if Assigned(FOnChange) then FOnChange(self);
end;

function ToglPaletteItem.GetSeries: ToglBasicSeries;
begin
  Result := (Collection as ToglPalette).Series;
end;

procedure ToglPaletteItem.SetColor(const AValue: TColor);
begin
  if FColor = AValue then exit;
  FColor := AValue;
  Changed;
end;

procedure ToglPaletteItem.SetValue(const AValue: GLfloat);
begin
  if FValue = AValue then exit;
  FValue := AValue;
  Changed;
end;


{ ToglPalette }

function ComparePalette(Item1, Item2: TCollectionItem): Integer;
var
  p1: ToglPaletteItem absolute Item1;
  p2: ToglPaletteItem absolute Item2;
begin
  Result := CompareValue(p1.Value, p2.Value);
end;

constructor ToglPalette.Create(ASeries: ToglBasicSeries);
begin
  inherited Create(ToglPaletteItem);
  FSeries := ASeries;
end;

function ToglPalette.Add: ToglPaletteItem;
begin
  Result := ToglPaletteItem(inherited Add);
  Result.OnChange := @DoChanged;
end;

function ToglPalette.Add(AValue: GLfloat; AColor: TColor): ToglPaletteItem;
begin
  Result := ToglPaletteItem(inherited Add);
  Result.OnChange := @DoChanged;
  Result.FColor := AColor;
  Result.FValue := AValue;
  if FLocked = 0 then DoChanged(Result);
end;

procedure ToglPalette.BeginUpdate;
begin
  inc(FLocked);
end;

procedure ToglPalette.Clear;
begin
  inherited Clear;
  if FLocked = 0 then FSeries.UpdatePalette;
end;

procedure ToglPalette.Delete(AIndex: Integer);
begin
  inherited Delete(AIndex);
  if FLocked = 0 then FSeries.UpdatePalette;
end;

procedure ToglPalette.DoChanged(Sender: TObject);
begin
  Sort(@ComparePalette);
  FSeries.UpdatePalette;
end;

procedure ToglPalette.EndUpdate;
begin
  dec(FLocked);
  if FLocked = 0 then DoChanged(nil);
end;

function ToglPalette.GetItem(AIndex: Integer): ToglPaletteItem;
begin
  Result := ToglPaletteItem(inherited Items[AIndex]);
end;

function ToglPalette.Interpolate(AValue, AMinValue, AMaxValue: GLfloat): TColor;
var
  i: Integer;
  coeff: Glfloat;
  itemsMinValue, itemsMaxValue, value: GLfloat;
  chart: ToglChart;
begin
  Result := clBlack;
  if Count = 0 then
    exit;
  if AMinValue = AMaxValue then
    exit;

  itemsMinValue := Items[0].Value;
  itemsMaxValue := Items[Count-1].Value;
  // convert AValue to range covered by colors
  value := (AValue - AMinValue) / (AMaxValue - AMinValue) * (itemsMaxValue - itemsMinValue) + itemsMinValue;

  if value <= itemsMinValue then
    Result := Items[0].Color
  else
  if value >= itemsMaxValue then
    Result := Items[Count-1].Color
  else
    for i:= Count - 2 downto 0 do
      if Items[i].Value < value then begin
        coeff := (value - Items[i].Value) / (Items[i+1].Value - Items[i].Value);
        Result := InterpolateRGB(Items[i].Color, Items[i+1].Color, coeff);
        exit;
      end;
  {
  if AValue <= Items[0].Value then
    Result := Items[0].Color
  else if AValue >= Items[Count-1].Value then
    Result := Items[Count-1].Color
  else
    for i:=Count - 2 downto 0 do
      if Items[i].Color < AValue then begin
        coeff := (AValue - Items[i].Value) / (Items[i+1].Value - Items[i].Value);
        Result := InterpolateRGB(Items[i].Color, Items[i+1].Color, coeff);
        exit;
      end;
      }
end;

procedure ToglPalette.SetItem(AIndex: Integer; const AValue: ToglPaletteItem);
begin
  (inherited Items[AIndex]).Assign(AValue);
end;


{ ToglFuncSeries }

constructor ToglFuncSeries.Create(AOwner: TComponent);
begin
  inherited;
  FDrawMode := dmFilled;
  FFillColor := $C0C0C0;
  FUseColorPalette := false;
  FWireFrameLineColor := clBlack;
  FWireFrameLineWidth := 2;
  FPalette := ToglPalette.Create(self);
end;

destructor ToglFuncSeries.Destroy;
begin
  FVertices := nil;
  FNormals := nil;
  FColors := nil;
  FPalette.Free;
  inherited;
end;

procedure ToglFuncSeries.CalcFunc;
var
  x, y, z: GLfloat;
  dx, dy: GLfloat;
  i, j: Integer;
  ok: Boolean;
begin
  ok := (FOnCalculate <> nil) and
    (FXCount > 1) and (FYCount > 1) and
    (FExtent.a.x < FExtent.b.x) and (FExtent.a.y < FExtent.b.y);

  if not ok then
  begin
    SetLength(FVertices, 0, 0);
    SetLength(FNormals, 0, 0);
    SetLength(FColors, 0, 0);
    exit;
  end;

  FValidNormals := false;
  if FUseColorPalette then FValidColors := false else FValidColors := true;

  dx := (FExtent.b.x - FExtent.a.x) / (FXCount - 1);
  dy := (FExtent.b.y - FExtent.a.y) / (FYCount - 1);

  // Calculate function values
  FExtent.a.z := Infinity;
  FExtent.b.z := -Infinity;
  SetLength(FVertices, FXCount, FYCount);
  for j := 0 to FYCount - 1 do begin
    y := FExtent.a.y + j*dy;
    for i := 0 to FXCount - 1 do begin
      x := FExtent.a.x + i*dx;
      z := FOnCalculate(x, y);
      FVertices[i, j].x := x;
      FVertices[i, j].y := y;
      FVertices[i, j].z := z;
      FExtent.a.z := Min(FExtent.a.z, z);
      FExtent.b.z := Max(FExtent.b.z, z);
    end;
  end;

              (*
  // Indices
  { For using GL_QUADS, connect vertices in clockwise orientation
             i, j+1    i+1, j+1
                +---->---+
                |        |
                ^        v
                |        |
                +----<---+
               i,j    i+1, j
  }
  SetLength(FIndices, (FXCount - 1) * (FYCount - 1));

  SetLength(FIndices, FXCount * FYCount * 4);
  k := 0;
  for j := 0 to FYCount - 2 do
    for i := 0 to FXCount - 2 do begin
      FIndices[k  ] :=   j   * FXCount + i;
      FIndices[k+1] := (j+1) * FXCount + i;
      FIndices[k+2] := (j+1) * FXCount + i + 1;
      FIndices[k+3] :=   j   * FXCount + i + 1;
      inc(k, 4);
    end;
              *)
end;

procedure ToglFuncSeries.CalcNormals;

  function ImgNormal(i1,j1, i2,j2, i3,j3: Integer): TVector3f;
  var
    img1, img2, img3: TVector3f;
  begin
    img1 := Chart.WorldToImage(FVertices[i1, j1]);
    img2 := Chart.WorldToImage(FVertices[i2, j2]);
    img3 := Chart.WorldToImage(FVertices[i3, j3]);
    Result := VNormalize(Cross(img2 - img1, img3 - img1));
  end;

var
  i, j: Integer;
begin
  // Calculate normals (in img coordinates. Watch out at last column/row!)
  SetLength(FNormals, FXCount, FYCount);
  for j := 0 to FYCount -2 do begin
    for i := 0 to FXCount - 2 do
      FNormals[i, j] := ImgNormal(i,j, i+1,j, i,j+1);
    FNormals[FXCount-1, j] := ImgNormal(FXCount-1,j, FXCount-1,j+1, FXCount-2,j);
  end;
  for i:= 0 to FXCount - 2 do
    FNormals[i, FYCount-1] := ImgNormal(i,FYCount-1, i,FYCount-2, i+1,FYCount-1);
  FNormals[FXCount-1, FYCount-1] := ImgNormal(FXCount-1,FYCount-1, FXCount-2,FYCount-1, FXCount-1,FYCount-2);

  FValidNormals := true;
end;


procedure ToglFuncSeries.CalcColors;
var
  i, j: Integer;
  c: TColor;
begin
  if Chart = nil then
    exit;
  SetLength(FColors, FXCount, FYCount);
  for j:=0 to FYCount-1 do
    for i:=0 to FXCount-1 do begin
      c := FPalette.Interpolate(FVertices[i,j].z, FExtent.a.z, FExtent.b.z);
      WriteLn(FVertices[i,j].x:20:3, FVertices[i,j].y:20:3, FVertices[i,j].z:20:3, c:20);
      FColors[i, j] := Array4f(Red(c)/255, Green(c)/255, Blue(c)/255, 1.0);
    end;
  FValidColors := true;
end;

procedure ToglFuncSeries.Draw;
begin
  if not FValidNormals then CalcNormals;
  if not FValidColors then CalcColors;

  case FDrawMode of
    dmFilled:
      DrawFilled;
    dmWireFrame:
      DrawWireFrame;
    dmFilledAndWireFrame:
      begin
        glEnable(GL_POLYGON_OFFSET_FILL);
        glPolygonOffset(1.0, 1.0);   // move polygon backward
        DrawFilled;
        glDisable(GL_POLYGON_OFFSET_FILL);
        // draw lines with VA
        DrawWireFrame;
      end;
  end;
end;

procedure ToglFuncSeries.DrawFilled;
var
  i, j: Integer;
  P: TVector3f;
  hasColorMaterial: Boolean;
begin
  hasColorMaterial := glIsEnabled(GL_COLOR_MATERIAL) = GL_TRUE;
  glEnable(GL_COLOR_MATERIAL);
  glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE);

  if FUseColorPalette then begin
    for j := 0 to FYCount - 2 do begin
      glBegin(GL_QUAD_STRIP);
        P := Chart.WorldToImage(FVertices[0, j+1]);
        glColor4fv(@FColors[0, j+1]);
        glNormal3fv(@FNormals[0, j+1]);
        glVertex3fv(@P);

        P := Chart.WorldToImage(FVertices[0, j]);
        glColor4fv(@FColors[0, j]);
        glNormal3fv(@FNormals[0, j]);
        glVertex3fv(@P);

        for i := 1 to FXCount - 1 do begin
          P := Chart.WorldToImage(FVertices[i, j+1]);
          glColor4fv(FColors[i, j+1]);
          glNormal3fv(@FNormals[i , j+1]);
          glVertex3fv(@P);

          P := Chart.WorldToImage(FVertices[i, j]);
          glColor4fv(FColors[i, j]);
          glNormal3fv(@FNormals[i, j]);
          glVertex3fv(@P);
        end;
      glEnd;
    end;
  end else
  begin
    SetOpenGLColor(FFillColor);
    for j := 0 to FYCount - 2 do begin
      glBegin(GL_QUAD_STRIP);
        P := Chart.WorldToImage(FVertices[0, j+1]);
        glNormal3fv(@FNormals[0, j+1]);
        glVertex3fv(@P);

        P := Chart.WorldToImage(FVertices[0, j]);
        glNormal3fv(@FNormals[0, j]);
        glVertex3fv(@P);

        for i := 1 to FXCount - 1 do begin
          P := Chart.WorldToImage(FVertices[i, j+1]);
          glNormal3fv(@FNormals[i , j+1]);
          glVertex3fv(@P);

          glNormal3fv(@FNormals[i, j]);
          P := Chart.WorldToImage(FVertices[i, j]);
          glVertex3fv(@P);
        end;
      glEnd;
    end;
  end;

  if not hasColorMaterial then
    glDisable(GL_COLOR_MATERIAL);
end;

procedure ToglFuncSeries.DrawWireFrame;
var
  hasLighting: Boolean;
  hasTexture: Boolean;
  i, j, k: Integer;
begin
  // set line width
  glLineWidth(FWireFrameLineWidth);

  { draw lines with vertex array, no shading }
  if FDrawMode = dmWireFrame then begin
    hasLighting := glIsEnabled(GL_LIGHTING) = GL_TRUE;
    hasTexture := glIsEnabled(GL_TEXTURE_2D) = GL_TRUE;
    glDisable(GL_LIGHTING);
    glDisable(GL_TEXTURE_2D);

    SetOpenGLColor(FWireFramelineColor);

    // horizontal lines
    k := 0;
    for j := 0 to FYCount - 1 do
      for i := 0 to FXCount - 1 do begin
        glBegin(GL_LINE_STRIP);
          glVertex3fv(@FVertices[k]);
          inc(k);
        glEnd;
      end;

    // vertical lines
    k := 0;
    for i := 0 to FXCount - 1 do
      for j := 0 to FYCount - 1 do begin
        glBegin(GL_LINE_STRIP);
          glVertex3fv(@FVertices[k]);
          inc(k, FXCount);
        glEnd;
      end;

    if hasLighting then glEnable(GL_LIGHTING);
    if hasTexture then glEnable(GL_TEXTURE_2D);
  end
  else
  { draw lines with vertex array, with shading }
  begin
    // horizontal lines
    k := 0;
    for j := 0 to FYCount - 1 do
      for i := 0 to FXCount - 1 do begin
        glBegin(GL_LINE_STRIP);
          glNormal3fv(@FNormals[k]);
          glVertex3fv(@FVertices[k]);
          inc(k);
        glEnd;
      end;

    // vertical lines
    k := 0;
    for i := 0 to FXCount - 1 do
      for j := 0 to FYCount - 1 do begin
        glBegin(GL_LINE_STRIP);
          glNormal3fv(@FNormals[k]);
          glVertex3fv(@FVertices[k]);
          inc(k, FXCount);
        glEnd;
      end;
  end;
end;

function ToglFuncSeries.GetCount(AIndex: Integer): Integer;
begin
  case AIndex of
    0: Result := FXCount;
    1: Result := FYCount;
  end;
end;

function ToglFuncSeries.GetMaxMin(AIndex: Integer): GLfloat;
begin
  case AIndex of
    0: Result := FExtent.b.x;
    1: Result := FExtent.a.x;
    2: Result := FExtent.b.y;
    3: Result := FExtent.a.y;
  end;
end;

procedure ToglFuncSeries.SetCount(AIndex: Integer; const AValue: Integer);
begin
  if GetCount(AIndex) = AValue then exit;
  case AIndex of
    0: FXCount := AValue;
    1: FYCount := AValue;
  end;
  CalcFunc;
end;

procedure ToglFuncSeries.SetDrawMode(const AValue: TDrawMode);
begin
  if FDrawMode = AValue then exit;
  FDrawMode := AValue;
  Notify(self, ncInvalidate, nil);
end;

procedure ToglFuncSeries.SetFillColor(const AValue: TColor);
begin
  if FFillColor = AValue then exit;
  FFillColor := AValue;
  Notify(self, ncInvalidate, nil);
end;

procedure ToglFuncSeries.SetMaxMin(AIndex: Integer; const AValue: GLfloat);
begin
  if GetMaxMin(AIndex) = AValue then
    exit;
  case AIndex of
    0: FExtent.b.x := AValue;
    1: FExtent.a.x := AValue;
    2: FExtent.b.y := AValue;
    3: FExtent.a.y := AValue;
  end;
  CalcFunc;
end;

procedure ToglFuncSeries.SetOnCalculate(const AValue: TCalculateEvent);
begin
  if TMethod(FOnCalculate) = TMethod(AValue) then exit;
  FOnCalculate := AValue;
  CalcFunc;
end;

procedure ToglFuncSeries.SetUseColorPalette(const AValue: Boolean);
begin
  if FUseColorPalette = AValue then exit;
  FUseColorPalette := AValue;
  if FUseColorPalette then
    FValidColors := false
  else begin
    FValidColors := true;
    SetLength(FColors, 0);
  end;
  Notify(self, ncInvalidate, nil);
end;

procedure ToglFuncSeries.SetWireFrameLineColor(const AValue: TColor);
begin
  if FWireFrameLineColor = AValue then exit;
  FWireFrameLineColor := AValue;
  Notify(self, ncInvalidate, nil);
end;

procedure ToglFuncSeries.SetWireFrameLineWidth(const AValue: Integer);
begin
  if FWireFrameLineWidth = AValue then exit;
  FWireFrameLineWidth := AValue;
  Notify(self, ncInvalidate, nil);
end;

procedure ToglFuncSeries.UpdateExtent;
var
  i, j: Integer;
begin
  FExtent.a.z := Infinity;
  FExtent.b.z := -Infinity;
  for j := 0 to FYCount-1 do
    for i:=0 to FXCount-1 do begin
      FExtent.a.z := Min(FExtent.a.z, FVertices[i,j].z);
      FExtent.b.z := Max(FExtent.b.z, FVertices[i,j].z);
  end;
  Notify(self, ncUpdateExtent, self);
end;

procedure ToglFuncSeries.UpdatePalette;
begin
  CalcColors;
  Notify(self, ncInvalidate, self);
end;

end.

