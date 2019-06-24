unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Spin, Buttons, gl, glu, OpenGLChart, OpenGLSeries;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    cbBackWallVisible: TCheckBox;
    cbLeftWallVisible: TCheckBox;
    cbBottomWallVisible: TCheckBox;
    cbAxisVisible: TCheckBox;
    cbAxisLineVisible: TCheckBox;
    clbBackWallColor: TColorButton;
    clbAxisLineColor: TColorButton;
    clbLeftWallColor: TColorButton;
    clbBottomWallColor: TColorButton;
    clbDiffuseLightColor: TColorButton;
    clbSpecularLightColor: TColorButton;
    cbShowBoundingBox: TCheckBox;
    cbShowAxes: TCheckBox;
    cbLightActive: TCheckBox;
    cbViewInteractive: TCheckBox;
    clbBackColor: TColorButton;
    cmbLightSelector: TComboBox;
    clbAmbientLightColor: TColorButton;
    cmbAxisKind: TComboBox;
    gbLeftWall: TGroupBox;
    gbBottomWall: TGroupBox;
    gbRotation: TGroupBox;
    gbLightPos: TGroupBox;
    gbLightColor: TGroupBox;
    gbBackWall: TGroupBox;
    gbAxisLine: TGroupBox;
    ImageList1: TImageList;
    lblHorRot: TLabel;
    Label3: TLabel;
    lblDepthRot: TLabel;
    lblHorRot1: TLabel;
    lblHorRot2: TLabel;
    lblHorRot3: TLabel;
    PageControl1: TPageControl;
    seDistance: TFloatSpinEdit;
    Label1: TLabel;
    Panel1: TPanel;
    pgViewParams: TTabSheet;
    pgLights: TTabSheet;
    seHorRot: TFloatSpinEdit;
    seLightPosX: TFloatSpinEdit;
    seLightPosY: TFloatSpinEdit;
    seLightPosZ: TFloatSpinEdit;
    seVertRot: TFloatSpinEdit;
    seDepthRot: TFloatSpinEdit;
    btnAddLight: TSpeedButton;
    pgMisc: TTabSheet;
    pgWalls: TTabSheet;
    pgAxes: TTabSheet;
    pgSeries: TTabSheet;
    procedure btnAddLightClick(Sender: TObject);
    procedure AxisChanged(Sender: TObject);
    procedure cmbAxisKindChange(Sender: TObject);
    procedure WallChanged(Sender: TObject);
    procedure LightChanged(Sender: TObject);
    procedure cmbLightSelectorChange(Sender: TObject);
    procedure cbShowAxesChange(Sender: TObject);
    procedure cbShowBoundingBoxChange(Sender: TObject);
    procedure ViewChanged(Sender: TObject);
    procedure clbBackColorColorChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FChart: ToglChart;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnAddLightClick(Sender: TObject);
var
  lightSrc: ToglLightSource;
  s: String;
  i: Integer;
begin
  lightSrc := FChart.LightSources.Add;
  if lightSrc = nil then exit;   // there are already 8 light sources

  for i:=0 to 7 do begin
    s := 'Light source #' + IntToStr(i);
    if cmbLightSelector.Items.IndexOf(s) = -1 then begin
      cmbLightSelector.Items.Add(s);
      cmbLightSelector.ItemIndex := i;
      cmbLightSelectorChange(nil);
      exit;
    end;
  end;
end;

procedure TForm1.AxisChanged(Sender: TObject);
var
  axis: ToglChartAxis;
begin
  case cmbAxisKind.ItemIndex of
    0: axis := FChart.XAxis;
    1: axis := FChart.YAxis;
    2: axis := FChart.ZAxis;
  end;
  if Sender = cbAxisVisible then
    axis.Visible := cbAxisVisible.Checked;
  if Sender = clbAxisLineColor then
    axis.LineColor := clbAxisLineColor.ButtonColor;
  if Sender = cbAxisLineVisible then
    axis.LineVisible := cbAxisLineVisible.Checked;
end;

procedure TForm1.WallChanged(Sender: TObject);
begin
  if Sender = cbBackWallVisible then
    FChart.BackWall.Visible := cbBackWallVisible.Checked;
  if Sender = cbBottomWallVisible then
    FChart.BottomWall.Visible := cbBottomWallVisible.Checked;
  if Sender = cbLeftWallVisible then
    FChart.LeftWall.Visible := cbLeftWallVisible.Checked;

  if Sender = clbBackWallColor then
    FChart.BackWall.Color := clbBackWallColor.ButtonColor;
  if Sender = clbBottomWallColor then
    FChart.BottomWall.Color := clbBottomWallColor.ButtonColor;
  if Sender = clbLeftWallColor then
    FChart.LeftWall.Color := clbLeftWallColor.ButtonColor;
end;

procedure TForm1.cbShowAxesChange(Sender: TObject);
begin
  FChart.ShowAxes := cbShowAxes.Checked;
end;

procedure TForm1.cbShowBoundingBoxChange(Sender: TObject);
begin
  FChart.ShowBoundingBox := cbShowBoundingBox.Checked;
end;

procedure TForm1.cmbLightSelectorChange(Sender: TObject);
var
  lightSrc: ToglLightSource;
  savedOnChange: TNotifyEvent;
begin
  // Avoid generate OnChange event when properties are copied to controls
  savedOnChange := cbLightActive.OnChange;
  cbLightActive.OnChange := nil;
  seLightPosX.OnChange := nil;
  seLightPosY.OnChange := nil;
  seLightPosZ.OnChange := nil;
  clbAmbientLightColor.OnColorChanged := nil;
  clbDiffuseLightColor.OnColorChanged := nil;
  clbSpecularLightColor.OnColorChanged := nil;

  lightSrc := FChart.LightSources[cmbLightSelector.ItemIndex];
  cbLightActive.Checked := lightSrc.Active;
  seLightPosX.Value := lightSrc.PosX;
  seLightPosY.Value := lightSrc.PosY;
  seLightPosZ.Value := lightSrc.PosZ;
  clbAmbientLightColor.ButtonColor := lightSrc.AmbientColor;
  clbDiffuseLightColor.ButtonColor := lightSrc.DiffuseColor;
  clbSpecularLightColor.ButtonColor := lightSrc.SpecularColor;

  cbLightActive.OnChange := savedOnChange;
  seLightPosX.OnChange := savedOnChange;
  seLightPosY.OnChange := savedOnChange;
  seLightPosZ.OnChange := savedOnChange;
  clbAmbientLightColor.OnColorChanged := savedOnChange;
  clbDiffuseLightColor.OnColorChanged := savedOnChange;
  clbSpecularLightColor.OnColorChanged := savedOnChange;
end;

procedure TForm1.clbBackColorColorChanged(Sender: TObject);
begin
  FChart.Color := clbBackColor.ButtonColor;
end;

procedure TForm1.cmbAxisKindChange(Sender: TObject);
var
  savedOnChange: TNotifyEvent;
  axis: ToglChartAxis;
begin
  case cmbAxisKind.ItemIndex of
    0: axis := FChart.xAxis;
    1: axis := FChart.yAxis;
    2: axis := FChart.zAxis;
  end;

  savedOnChange := cbAxisVisible.OnChange;
  cbAxisVisible.OnChange := nil;
  clbAxisLineColor.OnColorChanged := nil;
  cbAxisLineVisible.OnChange := nil;

  cbAxisVisible.Checked := axis.Visible;
  clbAxisLineColor.ButtonColor := axis.LineColor;
  cbAxisLineVisible.Checked := axis.LineVisible;

  cbAxisVisible.OnChange := savedOnChange;
  clbAxisLineColor.OnColorChanged := savedOnChange;
  cbAxisLineVisible.OnChange := savedOnChange;
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  N1 = 30;
  N2 = 50;
var
  ser: ToglPointSeries;
  i: Integer;
  t, x, y, z: GLfloat;
  axis: ToglChartAxis;
begin
  FChart := ToglChart.Create(self);
  FChart.Parent := self;
  FChart.Align := alClient;

  FChart.BackWall.Color := RgbToColor(255, 255, 220);
  FChart.BottomWall.Color := RgbToColor(120, 0, 0);

  FChart.XAxis.LineWidth := 3;   FChart.XAxis.Linecolor := RgbToColor(128, 0, 0);
  FChart.YAxis.LineWidth := 3;   FChart.YAxis.LineColor := RgbToColor(0, 128, 0);
  FChart.ZAxis.LineWidth := 3;   FChart.ZAxis.LineColor := RgbToColor(0, 0, 128);

  ser := ToglPointSeries.Create(self);
  ser.SymbolColor := clBlue;
  ser.SymbolSize := 0.05;
  ToglPointSeries(ser).Title := 'HScrew';
  for i := 0 to N1 do begin
    x := i / N1 * 20;
    y := cos(x) * 10;
    z := sin(x) * 10;
    ser.Add(x, y, z);
  end;
  FChart.AddSeries(ser);

  ser := ToglLineSeries.Create(self);
  ser.SymbolColor := clWhite; //Red;
  ser.SymbolSize := 0.1;
  ToglLineSeries(ser).LineWidth := 3;
  ToglLineSeries(ser).Title := 'VScrew';
  ToglLineSeries(ser).Style := lssLinesAndPoints;
  for i := 0 to N2 do begin
    t := i / N2 * 20;
    x := cos(t) * 10;
    y := sin(t) * 15;
    z := t;
    ser.Add(x, y, z);
  end;
  FChart.AddSeries(ser);

  clbBackColor.ButtonColor := FChart.Color;

  cbLightActive.Checked := FChart.LightSources[0].Active;
  seLightPosX.Value := FChart.LightSources[0].PosX;
  seLightPosY.Value := FChart.LightSources[0].PosY;
  seLightPosZ.Value := FChart.LightSources[0].PosZ;
  clbAmbientLightColor.ButtonColor := FChart.LightSources[0].AmbientColor;
  clbDiffuseLightColor.ButtonColor := FChart.LightSources[0].DiffuseColor;
  clbSpecularLightColor.ButtonColor := FChart.LightSources[0].SpecularColor;
  cbViewInteractive.Checked := FChart.ViewParams.Interactive;

  seDistance.Value := FChart.ViewParams.Distance;
  seHorRot.Value := FChart.ViewParams.HorRot;
  seVertRot.Value := FChart.Viewparams.VertRot;
  seDepthRot.Value := FChart.ViewParams.DepthRot;

  cbBackWallVisible.Checked := FChart.BackWall.Visible;
  cbBottomWallVisible.Checked := FChart.BottomWall.Visible;
  cbLeftWallVisible.Checked := FChart.LeftWall.Visible;
  clbBackWallColor.ButtonColor := FChart.BackWall.Color;
  clbBottomWallColor.ButtonColor := FChart.BottomWall.Color;
  clbLeftWallColor.ButtonColor := FChart.LeftWall.Color;

  case cmbAxisKind.ItemIndex of
    0: axis := FChart.xAxis;
    1: axis := FChart.yAxis;
    2: axis := FChart.zAxis;
  end;
  cbAxisVisible.Checked := axis.Visible;
  clbAxisLineColor.ButtonColor := axis.LineColor;
  cbAxisLineVisible.Checked := axis.LineVisible;
end;

procedure TForm1.LightChanged(Sender: TObject);
var
  lightsrc: ToglLightSource;
begin
  lightSrc := FChart.LightSources[cmbLightSelector.ItemIndex];
  if Sender = cbLightActive then
    lightSrc.Active := cbLightActive.Checked
  else if Sender = seLightPosX then
    lightSrc.PosX := seLightPosX.Value
  else if Sender = seLightPosY then
    lightSrc.PosY := seLightPosY.Value
  else if Sender = seLightPosZ then
    lightSrc.PosZ := seLightPosZ.Value
  else if Sender = clbAmbientLightColor then
    lightSrc.AmbientColor := clbAmbientLightColor.ButtonColor
  else if Sender = clbDiffuseLightColor then
    lightSrc.DiffuseColor := clbDiffuseLightColor.ButtonColor
  else if Sender = clbSpecularLightColor then
    lightSrc.SpecularColor := clbSpecularLightColor.ButtonColor;
end;

procedure TForm1.ViewChanged(Sender: TObject);
begin
  if Sender = cbViewInteractive then
    FChart.ViewParams.Interactive := cbViewInteractive.Checked
  else
  if Sender = seDistance then
    FChart.ViewParams.Distance := seDistance.Value
  else
  if Sender = seHorRot then
    FChart.ViewParams.HorRot := seHorRot.Value
  else
  if Sender = seVertRot then
    FChart.Viewparams.VertRot := seVertRot.Value
  else
  if Sender = seDepthRot then
    FChart.ViewParams.DepthRot := seDepthRot.Value;
end;


end.

