unit OpenGLLightSources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  gl, OpenGLTypes, OpenGLMath;

type
  ToglWall = class(ToglChartElement)
  private
    FColor: TColor;
    FKind: TFaceKind;
    FVisible: Boolean;
    procedure SetColor(const AValue: TColor);
    procedure SetVisible(const AValue: Boolean);
  public
    constructor Create(AChart: ToglBasicChart; AKind: TFaceKind);
  published
    property Color: TColor read FColor write SetColor default $00dddddd;
    property Kind: TFaceKind read FKind;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  ToglLightSource = class(TCollectionItem)
  private
    FActive: Boolean;
    FAmbient: TArray4f;
    FDiffuse: TArray4f;
    FSpecular: TArray4f;
    FPos: TArray4f;
    FAttachedTo: TLightAttachment;
    function GetAmbient: TColor;
    function GetDiffuse: TColor;
    function GetPos(AIndex: Integer): GLfloat;
    function GetSpecular: TColor;
    procedure SetActive(const AValue: Boolean);
    procedure SetAmbient(const AValue: TColor);
    procedure SetAttachedTo(const AValue: TLightAttachment);
    procedure SetDiffuse(const AValue: TColor);
    procedure SetPos(AIndex: Integer; const AValue: GLfloat);
    procedure SetSpecular(const AValue: TColor);
  protected
    function GetChart: ToglBasicChart;
    procedure Notify(ASender: TObject; ACmd: TNotifyCmd; AParam: Pointer);
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(ASource: TPersistent); override;
    procedure Init(Attachment: TLightAttachment);
  published
    property Active: Boolean read FActive write SetActive default true;
    property AmbientColor: TColor read GetAmbient write SetAmbient;
    property AttachedTo: TLightAttachment read FAttachedTo write SetAttachedTo default latCamera;
    property DiffuseColor: TColor read GetDiffuse write SetDiffuse;
    property PosX: GLfloat index 0 read GetPos write SetPos default 10;
    property PosY: GLfloat index 1 read Getpos write SetPos default -10;
    property PosZ: GLfloat index 2 read GetPos write SetPos default 10;
    property SpecularColor: TColor read GetSpecular write SetSpecular;
  end;

  ToglLightSources = class(TCollection)
  private
    function GetItem(AIndex: Integer): ToglLightSource;
    procedure SetItem(AIndex: Integer; const AValue: ToglLightSource);
    procedure AddDefault;
  protected
    FChart: ToglBasicChart;
  public
    constructor Create(AChart: ToglBasicChart);
    function Add: ToglLightSource;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure Init(Attachment: TLightAttachment);
    property Items[AIndex: Integer]: ToglLightSource read GetItem write SetItem; default;
    property Chart: ToglBasicChart read FChart;
  published
  end;


implementation

const
  MAX_LIGHTSOURCES = 8;   // OpenGL supports up to 8 light sources GL_LIGHT0..GL_LIGHT7


{ ToglWall }

constructor ToglWall.Create(AChart: ToglBasicChart; AKind: TFaceKind);
begin
  inherited Create(AChart);
  FColor := $00dddddd;
  FKind := AKind;
  FVisible := true;
end;

procedure ToglWall.SetColor(const AValue: TColor);
begin
  if FColor = AValue then exit;
  FColor := AValue;
  Notify(self, ncInvalidate, nil);
end;

procedure ToglWall.SetVisible(const AValue: Boolean);
begin
  if FVisible = AValue then exit;
  FVisible := AValue;
  Notify(self, ncInvalidate, nil);
end;



  { ToglLightSource }

constructor ToglLightSource.Create(ACollection: TCollection);
begin
  inherited;
  FActive := true;
  FAmbient := Array4f(0.3, 0.3, 0.3, 1.0);
  FDiffuse := Array4f(0.7, 0.7, 0.7, 1.0);
  FSpecular := Array4f(1.0, 1.0, 1.0, 1.0);
  FPos := Array4f(1000.0, 0.0, 1000.0, 0.0);  // last parameter=0 --> directional source ("sun"); <>01 --> positional ("desk
  FAttachedTo := latCamera;
end;

procedure ToglLightSource.Assign(ASource: TPersistent);
begin
  if (ASource is ToglLightSource) then begin
    FActive := ToglLightSource(ASource).Active;
    FAmbient := ToglLightSource(ASource).FAmbient;
    FDiffuse := ToglLightSource(ASource).FDiffuse;
    FSpecular := ToglLightSource(ASource).FSpecular;
    FPos := ToglLightSource(ASource).FPos;
    FAttachedTo := ToglLightSource(ASource).FAttachedTo;
    Notify(self, ncInitLight, self);
  end else
    inherited;
end;

function ToglLightSource.GetAmbient: TColor;
begin
  Result := RGBToColor(
    round(FAmbient[0]*255),
    round(FAmbient[1]*255),
    round(FAmbient[2]*255)
  );
end;

function ToglLightSource.GetChart: ToglBasicChart;
begin
  Result := (Collection as ToglLightSources).Chart;
end;

function ToglLightSource.GetDiffuse: TColor;
begin
  Result := RGBToColor(
    round(FDiffuse[0]*255),
    round(FDiffuse[1]*255),
    round(FDiffuse[2]*255)
  );
end;

function ToglLightSource.GetPos(AIndex: Integer): GLfloat;
begin
  Result := FPos[AIndex];
end;

function ToglLightSource.GetSpecular: TColor;
begin
  Result := RGBToColor(
    round(FSpecular[0]*255),
    round(FSpecular[1]*255),
    round(FSpecular[2]*255)
  );
end;

procedure ToglLightSource.Init(Attachment: TLightAttachment);
begin
  if FAttachedTo <> Attachment then
    exit;

  // Set up light colors (ambient, diffuse, specular)
  glLightfv(GL_LIGHT0 + Index, GL_AMBIENT, @FAmbient);
  glLightfv(GL_LIGHT0 + Index, GL_DIFFUSE, @FDiffuse);
  glLightfv(GL_LIGHT0 + Index, GL_SPECULAR, @FSpecular);

  // Position the light
  glLightfv(GL_LIGHT0 + Index, GL_POSITION, @FPos);

  // Must enable each light source after configuration
  if FActive then
    glEnable(GL_LIGHT0 + Index)
  else
    glDisable(GL_LIGHT0 + Index);
end;

procedure ToglLightSource.Notify(ASender: TObject; ACmd: TNotifyCmd; AParam: Pointer);
var
  chart: ToglBasicChart;
begin
  chart := GetChart;
  if chart <> nil then chart.Update(ASender, ACmd, AParam);
end;

procedure ToglLightSource.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then exit;
  FActive := AValue;
  Notify(self, ncInitLight, nil);
end;

procedure ToglLightSource.SetAmbient(const AValue: TColor);
begin
  FAmbient[0] := Red(AValue) / 255;
  FAmbient[1] := Green(AValue) / 255;
  FAmbient[2] := Blue(AValue) / 255;
  FAmbient[3] := 1.0;
  Notify(self, ncInitLight, nil);
end;

procedure ToglLightSource.SetDiffuse(const AValue: TColor);
begin
  FDiffuse[0] := Red(AValue) / 255;
  FDiffuse[1] := Green(AValue) / 255;
  FDiffuse[2] := Blue(AValue) / 255;
  FDiffuse[3] := 1.0;
  Notify(self, ncInitLight, nil);
end;

procedure ToglLightSource.SetPos(AIndex: Integer; const AValue: TGLfloat);
begin
  if FPos[AIndex] = AValue then exit;
  FPos[AIndex] := AValue;
  Notify(self, ncInitLight, nil);
end;

procedure ToglLightSource.SetAttachedTo(const AValue: TLightAttachment);
begin
  if FAttachedTo = AValue then exit;
  FAttachedTo := AValue;
  Notify(self, ncInitLight, nil);
end;

procedure ToglLightSource.SetSpecular(const AValue: TColor);
begin
  FSpecular[0] := Red(AValue) / 255;
  FSpecular[1] := Green(AValue) / 255;
  FSpecular[2] := Blue(AValue) / 255;
  FSpecular[3] := 1.0;
  Notify(self, ncInitLight, nil);
end;


{ ToglLightSoures }

constructor ToglLightSources.Create(AChart: ToglBasicChart);
begin
  inherited Create(ToglLightSource);
  FChart := AChart;
  AddDefault;
end;

function ToglLightSources.Add: ToglLightSource;
begin
  if Count < MAX_LIGHTSOURCES then
    Result := ToglLightSource(inherited Add)
  else
    Result := nil;
end;

procedure ToglLightSources.AddDefault;
begin
  if Count = 0 then Add;
end;

procedure ToglLightSources.Clear;
begin
  inherited Clear;
  AddDefault;
end;

procedure ToglLightSources.Delete(AIndex: Integer);
begin
  inherited Delete(AIndex);
  if Count = 0 then AddDefault;
end;

function ToglLightSources.GetItem(AIndex: Integer): ToglLightSource;
begin
  Result := ToglLightSource(inherited Items[AIndex]);
end;

{ Initializes only the light sources attached to camera or model. }
procedure ToglLightSources.Init(Attachment: TLightAttachment);
var
  i: Integer;
  lsrc: ToglLightSource;
  AnyActive: Boolean;
begin
  AnyActive := false;
  for i := 0 to Count-1 do begin
    lsrc := ToglLightSource(Items[i]);
    if lsrc.Active then AnyActive := true;
    lSrc.Init(Attachment);
  end;
  if AnyActive then
    glEnable(GL_LIGHTING)
  else
    glDisable(GL_LIGHTING);
end;

procedure ToglLightSources.SetItem(AIndex: Integer; const AValue: ToglLightSource);
begin
  (inherited Items[AIndex]).Assign(AValue);
end;


end.

