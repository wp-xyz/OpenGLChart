unit OpenGLColorTheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  ToglColorThemeItem = (tiChartBackground, tiFrame,
    tiBackWall, tiLeftWall, tiBottomWall,
    tiXAxisLine, tiYAxisLine, tiZAxisLine,
    tiXAxisLabels, tiYAxisLabels, tiZAxisLabels,
    tiXAxisTitle, tiYAxisTitle, tiZAxisTitle,
    tiSeries1, tiSeries2, tiSeries3, tiSeries4,
    tiSeries5, tiSeries6, tiSeries7, tiSeries8,
    tiSeries1a, tiSeries2a, tiSeries3a, tiSeries4a,
    tiSeries5a, tiSeries6a, tiSeries7a, tiSeries8a
  );

  ToglColorChangeEvent = procedure (Sender: TObject; AIndex: ToglColorThemeItem) of object;

  ToglChartColorTheme = class(TComponent)
  private
    FColors: array[ToglColorThemeItem] of TColor;
    FOnChange: ToglColorChangeEvent;
    function GetColor(AIndex: ToglColorThemeItem): TColor;
    procedure SetColor(AIndex: ToglColorThemeItem; const AColor: TColor);
  protected
    procedure DoChange(AIndex: TOglColorThemeItem); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property Items[AIndex: ToglColorThemeItem]: TColor read GetColor write SetColor; default;
    property OnChange: ToglColorChangeEvent read FOnChange write FOnChange;
  published
    property Background: TColor index tiChartBackground read GetColor write SetColor;
    property Frame: TColor index tiFrame read GetColor write SetColor;
    property BackWall: TColor index tiBackWall read GetColor write SetColor;
    property LeftWall: TColor index tiLeftWall read GetColor write SetColor;
    property BottomWall: TColor index tiBottomWall read GetColor write SetColor;
    property XAxisLine: TColor index tiXAxisLine read GetColor write SetColor;
    property YAxisLine: TColor index tiYAxisLine read GetColor write SetColor;
    property ZAxisLine: TColor index tiZAxisLine read GetColor write SetColor;
    property XAxisLabels: TColor index tiXAxisLabels read GetColor write SetColor;
    property YAxisLabels: TColor index tiYAxisLabels read GetColor write SetColor;
    property ZAxisLabels: TColor index tiZAxisLabels read GetColor write SetColor;
    property XAxisTitle: TColor index tiXAxisTitle read GetColor write SetColor;
    property YAxisTitle: TColor index tiYAxisTitle read GetColor write SetColor;
    property ZAxisTitle: TColor index tiZAxisTitle read GetColor write SetColor;
    property Series1: TColor index tiSeries1 read GetColor write SetColor;
    property Series1a: TColor index tiSeries1a read GetColor write SetColor;
    property Series2: TColor index tiSeries2 read GetColor write SetColor;
    property Series2a: TColor index tiSeries2a read GetColor write SetColor;
    property Series3: TColor index tiSeries3 read GetColor write SetColor;
    property Series3a: TColor index tiSeries3a read GetColor write SetColor;
    property Series4: TColor index tiSeries4 read GetColor write SetColor;
    property Series4a: TColor index tiSeries4a read GetColor write SetColor;
    property Series5: TColor index tiSeries5 read GetColor write SetColor;
    property Series5a: TColor index tiSeries5a read GetColor write SetColor;
    property Series6: TColor index tiSeries6 read GetColor write SetColor;
    property Series6a: TColor index tiSeries6a read GetColor write SetColor;
    property Series7: TColor index tiSeries7 read GetColor write SetColor;
    property Series7a: TColor index tiSeries7a read GetColor write SetColor;
    property Series8: TColor index tiSeries8 read GetColor write SetColor;
    property Series8a: TColor index tiSeries8a read GetColor write SetColor;
  end;

  ToglBrightChartColorTheme = class(ToglChartColorTheme)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  ToglDarkChartColorTheme = class(ToglChartColorTheme)
  public
    constructor Create(AOwner: TComponent); override;
  end;


implementation

uses
  LCLIntf;

{ ToglChartColorTheme }

constructor ToglChartColorTheme.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure ToglChartColorTheme.DoChange(AIndex: ToglColorThemeItem);
begin
  if Assigned(FOnChange) then
    FOnChange(self, AIndex);
end;

function ToglChartColorTheme.GetColor(AIndex: ToglColorThemeItem): TColor;
begin
  Result := FColors[AIndex];
end;

procedure ToglChartColorTheme.SetColor(AIndex: ToglColorThemeItem;
  const AColor: TColor);
begin
  if FColors[AIndex] = AColor then exit;
  FColors[AIndex] := AColor;
  DoChange(AIndex);
end;


{ ToglBrightChartColorTheme }

constructor ToglBrightChartColorTheme.Create(AOwner: TComponent);
begin
  inherited;
  FColors[tiChartBackground] := clWhite;
  FColors[tiFrame] := clGray;
  FColors[tiBackWall] := clWhite; //rgb(250, 250, 250);
  FColors[tiLeftWall] := clWhite; //rgb(255, 255, 255);
  FColors[tiBottomWall] := clSilver; //rgb(250, 250, 224);
  FColors[tiXAxisLine] := clGray;
  FColors[tiYAxisLine] := clGray;
  FColors[tiZAxisLine] := clGray;
  FColors[tiXAxisLabels] := clBlack;
  FColors[tiYAxisLabels] := clBlack;
  FColors[tiZAxisLabels] := clBlack;
  FColors[tiXAxisTitle] := clBlack;
  FColors[tiYAxisTitle] := clBlack;
  FColors[tiZAxisTitle] := clBlack;

  FColors[tiSeries1] := clRed;
  FColors[tiSeries2] := clBlue;
  FColors[tiSeries3] := clGreen;
  FColors[tiSeries4] := clPurple;
  FColors[tiSeries5] := clTeal;
  FColors[tiSeries6] := clFuchsia;
  FColors[tiSeries7] := clMaroon;
  FColors[tiSeries8] := clAqua;

  FColors[tiSeries1a] := FColors[tiSeries1];
  FColors[tiSeries2a] := FColors[tiSeries2];
  FColors[tiSeries3a] := FColors[tiSeries3];
  FColors[tiSeries4a] := FColors[tiSeries4];
  FColors[tiSeries5a] := FColors[tiSeries5];
  FColors[tiSeries6a] := FColors[tiSeries6];
  FColors[tiSeries7a] := FColors[tiSeries7];
  FColors[tiSeries8a] := FColors[tiSeries8];

end;


{ ToglDarkChartColorTheme }

constructor ToglDarkChartColorTheme.Create(AOwner: TComponent);
begin
  inherited;
  FColors[tiChartBackground] := clBlack;
  FColors[tiFrame] := clGray;
  FColors[tiBackWall] := clMedGray;
  FColors[tiLeftWall] := clMedGray;
  FColors[tiBottomWall] := clGray;
  FColors[tiXAxisLine] := clMedGray;
  FColors[tiYAxisLine] := clMedGray;
  FColors[tiZAxisLine] := clMedGray;
  FColors[tiXAxisLabels] := clSilver;
  FColors[tiYAxisLabels] := clSilver;
  FColors[tiZAxisLabels] := clSilver;
  FColors[tiXAxisTitle] := clSilver;
  FColors[tiYAxisTitle] := clSilver;
  FColors[tiZAxisTitle] := clSilver;
  FColors[tiSeries1] := clRed;
  FColors[tiSeries2] := clBlue;
  FColors[tiSeries3] := clLime;
  FColors[tiSeries4] := clYellow;
  FColors[tiSeries5] := clWhite;
  FColors[tiSeries6] := clFuchsia;
  FColors[tiSeries7] := clSkyBlue;
  FColors[tiSeries8] := clAqua;
end;


end.

