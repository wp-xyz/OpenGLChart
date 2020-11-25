unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Spin, Buttons, ValEdit, Types, Grids,
  gl, glu, OpenGLChart, OpenGLSeries, OpenGLColorTheme;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    btnSaveToBitmap: TButton;
    btnCopyToClipboard: TButton;
    btnBrightTheme: TButton;
    btnDarkTheme: TButton;
    cbAxisLabelsVisible: TCheckBox;
    cbBackWallVisible: TCheckBox;
    cbLeftWallVisible: TCheckBox;
    cbBottomWallVisible: TCheckBox;
    cbAxisVisible: TCheckBox;
    cbAxisLineVisible: TCheckBox;
    cbFuncUseColorPalette: TCheckBox;
    cbSeriesActive: TCheckBox;
    cbViewRefersToData: TCheckBox;
    cbAxisTitleVisible: TCheckBox;
    clbAxisLabelFontColor: TColorButton;
    clbFrameColor: TColorButton;
    clbBackWallColor: TColorButton;
    clbAxisLineColor: TColorButton;
    clbLeftWallColor: TColorButton;
    clbBottomWallColor: TColorButton;
    clbDiffuseLightColor: TColorButton;
    clbLineSymbolColor: TColorButton;
    clbLineColor: TColorButton;
    clbSpecularLightColor: TColorButton;
    cbShowFrame: TCheckBox;
    cbShowAxes: TCheckBox;
    cbLightActive: TCheckBox;
    cbViewInteractive: TCheckBox;
    clbBackColor: TColorButton;
    cmbAxisTitleFontName: TComboBox;
    cmbAxisTitleFontSize: TComboBox;
    cmbLightSelector: TComboBox;
    clbAmbientLightColor: TColorButton;
    cmbAxisKind: TComboBox;
    cmbSeriesList: TComboBox;
    clbFuncFillColor: TColorButton;
    clbPtSymbolColor: TColorButton;
    clbWireFrameLineColor: TColorButton;
    clbAxisTitleFontColor: TColorButton;
    ColorDialog1: TColorDialog;
    cmbFuncDrawMode: TComboBox;
    cmbAxisLabelFontName: TComboBox;
    cmbAxisLabelFontSize: TComboBox;
    edAxisTitleText: TEdit;
    gbAxisTitle: TGroupBox;
    gbLineSymbols: TGroupBox;
    gbLines: TGroupBox;
    gbAxisLabels: TGroupBox;
    Label11: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblFuncDrawMode: TLabel;
    lblLineSymbolSize: TLabel;
    lblLineWidth: TLabel;
    OpenDialog1: TOpenDialog;
    rgLightAttachedTo: TRadioGroup;
    rgLineseriesStyle: TRadioGroup;
    seLineWidth: TFloatSpinEdit;
    sePtSymbolSize: TFloatSpinEdit;
    gbX: TGroupBox;
    gbY: TGroupBox;
    gbPtSymbols: TGroupBox;
    Label2: TLabel;
    lblFuncYCount: TLabel;
    lblFuncXMax: TLabel;
    lblFuncXCount: TLabel;
    lblFuncYMax: TLabel;
    lblFuncYMin: TLabel;
    seFuncYMax: TFloatSpinEdit;
    seFuncXMin: TFloatSpinEdit;
    gbLeftWall: TGroupBox;
    gbBottomWall: TGroupBox;
    gbRotation: TGroupBox;
    gbLightPos: TGroupBox;
    gbLightColor: TGroupBox;
    gbBackWall: TGroupBox;
    gbAxisLine: TGroupBox;
    ImageList1: TImageList;
    lblFuncXMin: TLabel;
    lblHorRot: TLabel;
    Label3: TLabel;
    lblDepthRot: TLabel;
    lblLightPolarAngle: TLabel;
    lblLightAzimuthalAngle: TLabel;
    nbSeries: TNotebook;
    pgFuncSeries: TPage;
    pgLineSeries: TPage;
    pgPointSeries: TPage;
    PageControl1: TPageControl;
    seDistance: TFloatSpinEdit;
    Label1: TLabel;
    Panel1: TPanel;
    pgViewParams: TTabSheet;
    pgLights: TTabSheet;
    seFuncXMax: TFloatSpinEdit;
    seFuncYMin: TFloatSpinEdit;
    seHorRot: TFloatSpinEdit;
    seLightPolarAngle: TFloatSpinEdit;
    seLightAzimuthalAngle: TFloatSpinEdit;
    seLineSymbolSize: TFloatSpinEdit;
    seVertRot: TFloatSpinEdit;
    seDepthRot: TFloatSpinEdit;
    btnAddLight: TSpeedButton;
    pgMisc: TTabSheet;
    pgWalls: TTabSheet;
    pgAxes: TTabSheet;
    pgSeries: TTabSheet;
    seFuncXCount: TSpinEdit;
    seFuncYCount: TSpinEdit;
    seAxisLineWidth: TSpinEdit;
    vlePaletteEditor: TValueListEditor;
    procedure btnAddLightClick(Sender: TObject);
    procedure AxisChanged(Sender: TObject);
    procedure btnCopyToClipboardClick(Sender: TObject);
    procedure btnSaveToBitmapClick(Sender: TObject);
    procedure btnBrightThemeClick(Sender: TObject);
    procedure btnDarkThemeClick(Sender: TObject);
    procedure clbFrameColorColorChanged(Sender: TObject);
    procedure LightChanged(Sender: TObject);
    procedure LineSeriesChanged(Sender: TObject);
    procedure PointSeriesChanged(Sender: TObject);
    procedure WallChanged(Sender: TObject);
    procedure cmbAxisKindChange(Sender: TObject);
    procedure cmbSeriesListChange(Sender: TObject);
    procedure FuncSeriesChanged(Sender: TObject);
    procedure vlePaletteEditorDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure vlePaletteEditorEditButtonClick(Sender: TObject);
    procedure vlePaletteEditorEditingDone(Sender: TObject);
    procedure vlePaletteEditorPrepareCanvas(sender: TObject; aCol,
      aRow: Integer; aState: TGridDrawState);
    procedure vlePaletteEditorSelectEditor(Sender: TObject; aCol,
      aRow: Integer; var Editor: TWinControl);
    procedure cmbLightSelectorChange(Sender: TObject);
    procedure cbShowAxesChange(Sender: TObject);
    procedure cbShowFrameChange(Sender: TObject);
    procedure ViewChanged(Sender: TObject);
    procedure clbBackColorColorChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FChart: ToglChart;
    FBrightTheme: ToglChartColorTheme;
    FDarkTheme: ToglChartColorTheme;
    function CalcFunction(X, Y: GLfloat): GLfloat;
    procedure ChartToControls;
    procedure LightSourceToControls(ALightIndex: Integer);
    procedure UpdateColorControls;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Math, Clipbrd,
  OpenGLTypes, OpenGLUtils, OpenGLAxis, OpenGLLightSources;

{ TForm1 }

function TForm1.CalcFunction(X, Y: GLfloat): GLfloat;
var
  r: GLfloat;
begin
//  Result := x*y;

  r := sqrt(X*X + Y*Y);
  if r = 0 then
    Result := 1
  else
    Result := sin(r)/r;
end;

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

procedure TForm1.btnCopyToClipboardClick(Sender: TObject);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    OpenGLtoBitmap(bmp);
    Clipboard.Assign(bmp);
  finally
    bmp.Free;
  end;
end;

procedure TForm1.btnSaveToBitmapClick(Sender: TObject);
var
  bmp: TBitmap;
begin
  if OpenDialog1.FileName <> '' then
    OpenDialog1.InitialDir := ExtractFileDir(OpenDialog1.Filename);
  if OpenDialog1.Execute then begin
    bmp := TBitmap.Create;
    try
      OpenGLToBitmap(bmp);
      bmp.SaveToFile(OpenDialog1.Filename);
    finally
      bmp.Free;
    end;
  end;
end;

procedure TForm1.btnDarkThemeClick(Sender: TObject);
begin
  FChart.ColorTheme := FDarkTheme;
  UpdateColorControls;
end;

procedure TForm1.btnBrightThemeClick(Sender: TObject);
begin
  FChart.ColorTheme := FBrightTheme;
  UpdateColorControls;
end;

procedure TForm1.AxisChanged(Sender: TObject);
var
  axis: ToglChartAxis;
  v: GLfloat;
begin
  case cmbAxisKind.ItemIndex of
    0: axis := FChart.XAxis;
    1: axis := FChart.YAxis;
    2: axis := FChart.ZAxis;
  end;

  if Sender = cbAxisVisible then
    axis.Visible := cbAxisVisible.Checked
  else if Sender = clbAxisLineColor then
    axis.LineColor := clbAxisLineColor.ButtonColor
  else if Sender = cbAxisLineVisible then
    axis.LineVisible := cbAxisLineVisible.Checked
  else if Sender = seAxisLineWidth then
    axis.LineWidth := seAxisLineWidth.Value;

  if Sender = cbAxisTitleVisible then
    axis.Title.Visible := cbAxisTitleVisible.Checked
  else if Sender = edAxisTitleText then
    axis.Title.Text := edAxisTitleText.Text
  else if Sender = cmbAxisTitleFontName then
    axis.Title.FontName := cmbAxisTitleFontName.Items[cmbAxisTitleFontName.ItemIndex]
  else if (Sender = cmbAxisTitleFontSize) and TryStrToFloat(cmbAxisTitleFontSize.Text, v) then
    axis.Title.FontSize := v
  else if (Sender = clbAxisTitleFontColor) then
    axis.Title.FontColor := clbAxisTitleFontColor.ButtonColor;

  if (Sender = cbAxisLabelsVisible) then
    axis.LabelsVisible := cbAxisLabelsVisible.Checked
  else if (Sender = cmbAxisLabelFontName) then
    axis.LabelFontName := cmbAxisLabelFontName.Items[cmbAxisLabelFontName.ItemIndex]
  else if (Sender = cmbAxisLabelFontSize) and TryStrToFloat(cmbAxisLabelFontSize.Text, v) then
    axis.LabelFontSize := v
  else if (Sender = clbAxisLabelFontColor) then
    axis.LabelFontColor := clbAxisLabelFontColor.ButtonColor
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
    FChart.BackWall.Color := clbBackWallColor.ButtonColor
  else if Sender = clbBottomWallColor then
    FChart.BottomWall.Color := clbBottomWallColor.ButtonColor
  else
  if Sender = clbLeftWallColor then
    FChart.LeftWall.Color := clbLeftWallColor.ButtonColor;
end;

procedure TForm1.cbShowAxesChange(Sender: TObject);
begin
  FChart.ShowAxes := cbShowAxes.Checked;
end;

procedure TForm1.cbShowFrameChange(Sender: TObject);
begin
  FChart.ShowFrame := cbShowFrame.Checked;
end;

procedure TForm1.cmbLightSelectorChange(Sender: TObject);
begin
  LightSourceToControls(cmbLightSelector.ItemIndex);
end;

procedure TForm1.cmbSeriesListChange(Sender: TObject);
var
  ser: ToglBasicSeries;
  pser: ToglPointSeries;
  lser: ToglLineSeries;
  fser: ToglFuncSeries;
  i: Integer;
  s: String;
begin
  ser := ToglBasicSeries(cmbSeriesList.Items.Objects[cmbSeriesList.itemIndex]);
  nbSeries.PageIndex := -1;
  for i:=0 to nbSeries.PageCount-1 do begin
    s := 'Togl' + copy(nbSeries.Pages[i], 3, MaxInt);
    if SameText(s, ser.ClassName) then begin
      nbSeries.PageIndex := i;
      break;
    end;
  end;
  if nbSeries.PageIndex = -1 then begin
    MessageDlg('Notebook page for series not found.', mtError, [mbOK], 0);
    exit;
  end;

  cbSeriesActive.Checked := ser.Active;

  if (ser is ToglLineSeries) then
  begin
    lser := ToglLineSeries(ser);
    rgLineSeriesStyle.ItemIndex := ord(lser.Style);
    clbLineSymbolColor.ButtonColor := lser.SymbolColor;
    seLineSymbolSize.Value := lser.SymbolSize;
    clbLineColor.ButtonColor := lser.LineColor;
    seLineWidth.Value := lser.LineWidth;
  end else
  if (ser is ToglPointSeries) then
  begin
    pser := ToglPointSeries(ser);
    clbPtSymbolColor.ButtonColor := pser.SymbolColor;
    sePtSymbolSize.Value := pser.SymbolSize;
  end else
  if (ser is ToglFuncSeries) then
  begin
    fser := ToglFuncSeries(ser);
    seFuncXMin.Value := fser.XMin;
    seFuncXMax.Value := fser.XMax;
    seFuncYMin.Value := fser.YMin;
    seFuncYMax.Value := fser.YMax;
    seFuncXCount.Value := fser.XCount;
    seFuncYCount.Value := fser.YCount;
    cmbFuncDrawMode.ItemIndex := ord(fser.DrawMode);
    clbWireframeLineColor.ButtonColor := fser.WireFrameLineColor;
    clbWireFrameLineColor.Visible := fser.DrawMode = dmFilledAndWireFrame;
    cbFuncUseColorPalette.Checked := fser.UseColorPalette;
    clbFuncFillColor.ButtonColor := fser.FillColor;
    vlePaletteEditor.RowCount := 1 + fser.ColorPalette.Count;
    with fser.ColorPalette do
      for i:=0 to Count-1 do begin
        vlePaletteEditor.Cells[0, i+1] := FormatFloat('0.000', Items[i].Value);
        vlePaletteEditor.Cells[1, i+1] := IntToStr(Items[i].Color);
      end;
  end;

end;

procedure TForm1.clbBackColorColorChanged(Sender: TObject);
begin
  FChart.Color := clbBackColor.ButtonColor;
end;

procedure TForm1.clbFrameColorColorChanged(Sender: TObject);
begin
  FChart.FrameColor := clbFrameColor.ButtonColor;
end;

procedure TForm1.LineSeriesChanged(Sender: TObject);
var
  ser: ToglBasicSeries;
begin
  if cmbSeriesList.ItemIndex = -1 then
    exit;

  ser := cmbSeriesList.Items.Objects[cmbSeriesList.ItemIndex] as ToglBasicSeries;
  ser.Active := cbSeriesActive.Checked;

  if ser is ToglLineSeries then begin
    if Sender = clbLineSymbolColor then
      ToglLineSeries(ser).SymbolColor := clbLineSymbolColor.ButtonColor
    else
    if Sender = seLineSymbolSize then
      ToglLineSeries(ser).SymbolSize := seLineSymbolSize.Value
    else
    if Sender = clbLineColor then
      ToglLineSeries(ser).LineColor := clbLineColor.ButtonColor
    else
    if Sender = seLineWidth then
      ToglLineSeries(ser).LineWidth := seLineWidth.Value
    else
    if Sender = rgLineSeriesStyle then
      ToglLineSeries(ser).Style := ToglLineSeriesStyle(rgLineSeriesStyle.ItemIndex);
  end;
end;

procedure TForm1.PointSeriesChanged(Sender: TObject);
var
  ser: ToglBasicSeries;
begin
  if cmbSeriesList.ItemIndex = -1 then
    exit;

  ser := cmbSeriesList.Items.Objects[cmbSeriesList.ItemIndex] as ToglBasicSeries;
  ser.Active := cbSeriesActive.Checked;

  if ser is ToglPointSeries then begin
    if Sender = clbPtSymbolColor then
      ToglPointSeries(ser).SymbolColor := clbPtSymbolColor.ButtonColor
    else
    if Sender = sePtSymbolSize then
      ToglPointSeries(ser).SymbolSize := sePtSymbolSize.Value;
  end;
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
  seAxisLineWidth.OnChange := nil;
  cbAxisTitleVisible.OnChange := nil;
  edAxisTitleText.OnChange := nil;
  cmbAxisTitleFontName.OnChange := nil;
  cmbAxisTitleFontSize.OnChange := nil;
  clbAxisTitleFontColor.OnColorChanged := nil;
  cbAxisLabelsVisible.OnChange := nil;
  cmbAxisLabelFontName.OnChange := nil;
  cmbAxisLabelFontSize.OnChange := nil;
  clbAxisLabelFontColor.OnColorChanged := nil;

  cbAxisVisible.Checked := axis.Visible;
  clbAxisLineColor.ButtonColor := axis.LineColor;
  cbAxisLineVisible.Checked := axis.LineVisible;
  seAxisLineWidth.Value := axis.LineWidth;
  cbAxisTitleVisible.Checked := axis.Title.Visible;
  edAxisTitleText.Text := axis.Title.Text;
  cmbAxisTitleFontName.ItemIndex := Max(0, cmbAxisTitleFontName.Items.IndexOf(axis.Title.FontName));
  cmbAxisTitleFontSize.Text := FloatToStr(axis.Title.FontSize);
  clbAxisTitleFontColor.ButtonColor := axis.Title.FontColor;
  cbAxisLabelsVisible.Checked := axis.LabelsVisible;
  cmbAxisLabelFontName.ItemIndex := Max(0, cmbAxisLabelFontName.Items.IndexOf(axis.LabelFontName));
  cmbAxisLabelFontSize.Text := FloatToStr(axis.LabelFontSize);
  clbAxisLabelFontColor.ButtonColor := axis.LabelFontColor;

  cbAxisVisible.OnChange := savedOnChange;
  clbAxisLineColor.OnColorChanged := savedOnChange;
  cbAxisLineVisible.OnChange := savedOnChange;
  seAxisLineWidth.OnChange := savedOnChange;
  cbAxisTitleVisible.OnChange := savedOnChange;
  edAxisTitleText.OnChange := savedOnChange;
  cmbAxisTitleFontName.OnChange := savedOnChange;
  cmbAxisTitleFontSize.OnChange := savedOnChange;
  clbAxisTitleFontColor.OnColorChanged := savedOnChange;
  cbAxisLabelsVisible.OnChange := savedOnChange;
  cmbAxisLabelFontName.OnChange := savedOnChange;
  cmbAxisLabelFontSize.OnChange := savedOnChange;
  clbAxisLabelFontColor.OnColorChanged := savedOnChange;

end;

procedure TForm1.ChartToControls;
var
  axis: ToglChartAxis;
  i: Integer;
begin
  nbSeries.PageIndex := -1;
  cmbSeriesList.Items.Clear;
  for i:=0 to FChart.SeriesCount-1 do
    cmbSeriesList.Items.AddObject(FChart.Series[i].Title, FChart.Series[i]);
  if cmbSeriesList.Items.Count > 0 then begin
    cmbSeriesList.ItemIndex := 0;
    cmbSeriesListChange(nil);
  end;

  clbBackColor.ButtonColor := FChart.Color;
  clbFrameColor.ButtonColor := FChart.FrameColor;
  cbViewInteractive.Checked := FChart.ViewParams.Interactive;
  LightSourceToControls(0);

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

  { axis }
  case cmbAxisKind.ItemIndex of
    0: axis := FChart.xAxis;
    1: axis := FChart.yAxis;
    2: axis := FChart.zAxis;
  end;
  cbAxisVisible.Checked := axis.Visible;
  clbAxisLineColor.ButtonColor := axis.LineColor;
  cbAxisLineVisible.Checked := axis.LineVisible;
  seAxisLineWidth.Value := axis.LineWidth;

  cbAxisTitleVisible.Checked := axis.Title.Visible;
  edAxisTitleText.Text := axis.Title.Text;
  cmbAxisTitleFontName.Items.Assign(Screen.Fonts);
  i := Max(0, cmbAxisTitleFontName.Items.IndexOf(axis.Title.FontName));
  cmbAxisTitleFontName.ItemIndex := i;
  cmbAxisTitleFontSize.Text := FloatToStr(axis.Title.FontSize);
  clbAxisTitleFontColor.ButtonColor := axis.Title.FontColor;

  cbAxisLabelsVisible.Checked := axis.LabelsVisible;
  cmbAxisLabelFontName.Items.Assign(Screen.Fonts);
  i := Max(0, cmbAxisLabelFontName.Items.IndexOf(axis.LabelFontName));
  cmbAxisLabelFontName.ItemIndex := i;
  cmbAxisLabelFontSize.Text := FloatToStr(axis.LabelFontSize);
  clbAxisLabelFontColor.ButtonColor := axis.LabelFontColor;
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  N1 = 30;
  N2 = 50;
var
  ser: ToglPointSeries;
  funcSer: ToglFuncSeries;
  i: Integer;
  t, x, y, z: GLfloat;
  axis: ToglChartAxis;
begin
  FBrightTheme := ToglBrightChartColorTheme.Create(self);
  FDarkTheme := ToglDarkChartColorTheme.Create(self);

  FChart := ToglChart.Create(self);
  FChart.Parent := self;
  FChart.Align := alClient;

  FChart.BackWall.Color := RgbToColor(255, 255, 220);
  FChart.BottomWall.Color := RgbToColor(120, 0, 0);

  FChart.XAxis.LineWidth := 3;   FChart.XAxis.Linecolor := RgbToColor(128, 0, 0);   FChart.XAxis.Format := '%.0f';
  FChart.YAxis.LineWidth := 3;   FChart.YAxis.LineColor := RgbToColor(0, 128, 0);   FChart.YAxis.Format := '%.0f';
  FChart.ZAxis.LineWidth := 3;   FChart.ZAxis.LineColor := RgbToColor(0, 0, 128);   FChart.ZAxis.Format := '%.2f';

  ser := ToglPointSeries.Create(self);
  ser.SymbolColor := clBlue;
  ser.SymbolSize := 0.05;
  ser.Title := 'HScrew';
  for i := 0 to N1 do begin
    x := i / N1 * 20;
    y := cos(x);
    z := sin(x);
    ser.Add(x, y, z);
  end;
  FChart.AddSeries(ser);

  ser := ToglLineSeries.Create(self);
  ser.SymbolColor := clWhite; //Red;
  ser.SymbolSize := 0.1;
  ToglLineSeries(ser).LineWidth := 3;
  ser.Title := 'VScrew';
  ToglLineSeries(ser).Style := lssLinesAndPoints;
  for i := 0 to N2 do begin
    t := i / N2 * 20;
    x := cos(t) * 10;
    y := sin(t) * 15;
    z := i / N2;
    ser.Add(x, y, z);
  end;
  FChart.AddSeries(ser);

  funcser := ToglFuncSeries.Create(self);
  funcser.Title := 'Function';
  funcser.XCount := 31; //51;
  funcser.YCount := 31; //51;
  funcser.XMin := -10;
  funcser.XMax := +10;
  funcser.YMin := -10;
  funcser.YMax := +10;
  funcser.OnCalculate := @CalcFunction;
  funcser.ColorPalette.Add(0.00, clBlack);
  funcser.ColorPalette.Add(0.26, clBlue);
  funcser.ColorPalette.Add(0.53, clFuchsia);
  funcser.ColorPalette.Add(0.63, clRed);
  funcser.Colorpalette.Add(0.95, clYellow);
  funcser.ColorPalette.Add(1.00, clWhite);
  {
  funcser.ColorPalette.Add(-1.0, clBlack);
  funcser.ColorPalette.Add(-0.5, clBlue);
  funcser.ColorPalette.Add( 0.0, clGreen);
  funcser.ColorPalette.Add(0.25, clRed);
  funcser.ColorPalette.Add(0.5, clYellow);
  funcser.ColorPalette.Add(1.0, clWhite);
  }
  FChart.AddSeries(funcser);

  ChartToControls;
end;

procedure TForm1.LightChanged(Sender: TObject);
var
  lightsrc: ToglLightSource;
  theta, phi: GLfloat;
begin
  lightSrc := FChart.LightSources[cmbLightSelector.ItemIndex];
  if Sender = cbLightActive then
    lightSrc.Active := cbLightActive.Checked
  else if (Sender = seLightPolarAngle) or (Sender = seLightAzimuthalAngle) then
  begin
    theta := DegToRad(seLightPolarAngle.Value);
    phi := DegToRad(seLightAzimuthalAngle.Value);
    lightSrc.PosX := cos(theta)*cos(phi);
    lightSrc.PosY := cos(theta)*sin(phi);
    lightsrc.PosZ := sin(theta);
  end
  else if Sender = clbAmbientLightColor then
    lightSrc.AmbientColor := clbAmbientLightColor.ButtonColor
  else if Sender = clbDiffuseLightColor then
    lightSrc.DiffuseColor := clbDiffuseLightColor.ButtonColor
  else if Sender = clbSpecularLightColor then
    lightSrc.SpecularColor := clbSpecularLightColor.ButtonColor
  else if Sender = rgLightAttachedTo then
    lightSrc.AttachedTo := TLightAttachment(rgLightAttachedTo.ItemIndex);
end;

procedure TForm1.FuncSeriesChanged(Sender: TObject);
var
  ser: ToglBasicSeries;
  fser: ToglFuncSeries;
  pser: ToglPointSeries;
  lser: ToglLineSeries;
  i: Integer;
begin
  if cmbSeriesList.ItemIndex = -1 then
    exit;

  ser := cmbSeriesList.Items.Objects[cmbSeriesList.ItemIndex] as ToglBasicSeries;

  ser.Active := cbSeriesActive.Checked;

  if ser is ToglLineSeries then begin
    //
  end else
  if ser is ToglPointseries then begin
    //
  end else
  if ser is ToglFuncSeries then begin
    fser := ToglFuncSeries(ser);
    if Sender = seFuncXCount then
      fser.XCount := seFuncXCount.Value
    else if Sender = seFuncYCount then
      fser.YCount := seFuncYcount.Value
    else if Sender = seFuncXMax then
      fser.XMax := seFuncXMax.Value
    else if Sender = seFuncXMin then
      fser.XMin := sefuncXMin.Value
    else if Sender = seFuncYMax then
      fser.YMax := seFuncYMax.value
    else if sender  = seFuncYMin then
      fser.YMin := seFuncYMin.Value
    else if Sender = cbFuncUseColorPalette then begin
      fser.UseColorPalette := cbFuncUseColorPalette.Checked;
      clbFuncFillColor.Visible := not cbFuncUseColorPalette.Checked;
      if cbFuncUseColorPalette.Checked then
        with fser.ColorPalette do begin
          vlePaletteEditor.RowCount := Count + 1;
          for i:=0 to Count-1 do begin
            vlePaletteEditor.Cells[0, i+1] := FormatFloat('0.000', Items[i].Value);
            vlePaletteEditor.Cells[1, i+1] := IntToStr(Items[i].Color);
          end;
        end;
      vlePaletteEditor.Visible := cbFuncUseColorPalette.Checked;
    end
    else if Sender = clbFuncFillColor then
      fser.FillColor := clbFuncFillColor.ButtonColor
    else if Sender = cmbFuncDrawMode then
      fser.DrawMode := TDrawmode(cmbFuncDrawMode.ItemIndex)
    else if Sender = clbWireFrameLineColor then
      fser.WireframeLineColor := clbWireframeLineColor.ButtonColor;
    clbWireFrameLineColor.Visible := TDrawMode(cmbFuncDrawMode.ItemIndex) = dmFilledAndWireFrame;
  end;
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

procedure TForm1.vlePaletteEditorDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  s: String;
  clr: TColor;
begin
  if (ACol = 1) and (ARow > 0) then begin
    s := vlePaletteeditor.Cells[ACol, ARow];
    if s = '' then begin
      vlePaletteEditor.Canvas.Pen.Color := clGray;
      vlePaletteEditor.Canvas.Line(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
      vlePaletteEditor.Canvas.Line(Arect.Left, ARect.Bottom, ARect.Right, ARect.Top);
    end else
    if TryStrToInt(s, clr) then begin
      InflateRect(ARect, -2, -2);
      vlePaletteEditor.Canvas.Brush.Color := clr;
      vlePaletteEditor.Canvas.FillRect(ARect);
    end;
  end;
end;

procedure TForm1.vlePaletteEditorEditButtonClick(Sender: TObject);
var
  r, c: Integer;
  clr: TColor;
begin
  r := vlePaletteEditor.Row;
  c := vlePaletteEditor.Col;
  if not TryStrToInt(vlePaletteEditor.Cells[c, r], clr) then clr := clBlack;
  ColorDialog1.Color := clr;
  if Colordialog1.Execute then begin
    vlePaletteEditor.Cells[c, r] := IntToStr(ColorDialog1.Color);
    vlePaletteEditorEditingDone(nil);
  end;
end;

procedure TForm1.vlePaletteEditorEditingDone(Sender: TObject);
var
  ser: ToglBasicSeries;
  fser: ToglFuncSeries;
  i: Integer;
  val: GLfloat;
  clr: TColor;
begin
  ser := ToglBasicSeries(cmbSeriesList.Items.Objects[cmbSeriesList.itemIndex]);
  if not (ser is ToglFuncSeries) then exit;

  fser := ToglFuncSeries(ser);
  fser.ColorPalette.BeginUpdate;
  try
    fser.ColorPalette.Clear;
    with vlePaletteEditor do begin
      for i:=1 to RowCount-1 do begin
        if TryStrToFloat(Cells[0, i], val) and TryStrToInt(Cells[1, i], clr) then
          fser.ColorPalette.Add(val, clr);
      end;
    end;
  finally
    fser.ColorPalette.EndUpdate;
  end;

  WriteLn('PALETTE');
  for i:=0 to fser.ColorPalette.Count-1 do
    WriteLn(fser.ColorPalette[i].Value:15:3, fser.ColorPalette[i].Color:15);
end;

procedure TForm1.vlePaletteEditorPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
var
  ts: TTextStyle;
begin
  if ACol = 0 then begin
    ts := vlePaletteEditor.Canvas.TextStyle;
    ts.Alignment := taRightJustify;
    vlePaletteEditor.Canvas.TextStyle := ts;
  end;
end;

procedure TForm1.vlePaletteEditorSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
begin
  if ACol = 1 then Editor := vlePaletteEditor.EditorByStyle(cbsButton);
end;

procedure TForm1.LightSourceToControls(ALightIndex: Integer);
var
  x, y, z, r, rp: GLfloat;
  theta, phi: GLfloat;
  savedOnChange: TNotifyEvent;
begin
  // Avoid generating OnChange event when properties are copied to controls
  savedOnChange := cbLightActive.OnChange;
  cbLightActive.OnChange := nil;
  seLightPolarAngle.OnChange := nil;
  seLightAzimuthalAngle.OnChange := nil;
  clbAmbientLightColor.OnColorChanged := nil;
  clbDiffuseLightColor.OnColorChanged := nil;
  clbSpecularLightColor.OnColorChanged := nil;
  rgLightAttachedTo.OnClick := nil;

  with FChart.LightSources[ALightIndex] do begin
    cbLightActive.Checked := Active;
    rgLightAttachedTo.ItemIndex := ord(AttachedTo);

    r := sqrt(sqr(PosX) + sqr(PosY) + sqr(PosZ));
    theta := arcsin(PosZ/r);
    phi := arctan2(PosY, PosX);
    seLightPolarAngle.Value := RadToDeg(theta);
    seLightAzimuthalAngle.Value := RadToDeg(phi);

    clbAmbientLightColor.ButtonColor := AmbientColor;
    clbDiffuseLightColor.ButtonColor := DiffuseColor;
    clbSpecularLightColor.ButtonColor := SpecularColor;
  end;

  cbLightActive.OnChange := savedOnChange;
  seLightPolarAngle.OnChange := savedOnChange;
  seLightAzimuthalAngle.OnChange := savedOnChange;
  clbAmbientLightColor.OnColorChanged := savedOnChange;
  clbDiffuseLightColor.OnColorChanged := savedOnChange;
  clbSpecularLightColor.OnColorChanged := savedOnChange;
  rgLightAttachedTo.OnClick := savedOnChange;
end;

procedure TForm1.UpdateColorControls;
var
  axis: ToglChartAxis;
  ser: ToglBasicSeries;
begin
  clbBackColor.ButtonColor := FChart.Color;
  clbFrameColor.ButtonColor := FChart.FrameColor;

  clbBackWallColor.ButtonColor := FChart.BackWall.Color;
  clbBottomWallColor.ButtonColor := FChart.BottomWall.Color;
  clbLeftWallColor.ButtonColor := FChart.LeftWall.Color;

  { axis }
  case cmbAxisKind.ItemIndex of
    0: axis := FChart.xAxis;
    1: axis := FChart.yAxis;
    2: axis := FChart.zAxis;
  end;
  clbAxisLineColor.ButtonColor := axis.LineColor;
  clbAxisTitleFontColor.ButtonColor := axis.Title.FontColor;
  clbAxisLabelFontColor.ButtonColor := axis.LabelFontColor;

  { series }
  ser := ToglBasicSeries(cmbSeriesList.Items.Objects[cmbSeriesList.itemIndex]);
  if (ser is ToglLineSeries) then
  begin
    clbLineSymbolColor.ButtonColor := ToglLineSeries(ser).SymbolColor;
    clbLineColor.ButtonColor := ToglLineSeries(ser).LineColor;
  end else
  if (ser is ToglPointSeries) then
    clbPtSymbolColor.ButtonColor := ToglPointSeries(ser).SymbolColor
  else
  if (ser is ToglFuncSeries) then
    clbFuncFillColor.ButtonColor := ToglFuncSeries(ser).FillColor;
end;


end.

