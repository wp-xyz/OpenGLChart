unit OpenGLText;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, FPCanvas, FPImage,
  EasyLazFreeType, LazFreeTypeFPImageDrawer,       //fpwritepng,
  gl, glu,
  OpenGLTypes, OpenGLMath;

procedure DrawText(const AText: String; P, RotAxis: TVector3f; RotAngle: GLfloat;
  TextDir, Size: GLfloat; Alignment: TFreeTypeAlignments; Flipped: Boolean);

procedure DrawText(const AText: String; Alignment: TFreeTypeAlignments);
procedure DrawText2D(x, y, z: GLfloat; AText: String; Alignment: TFreeTypeAlignments);
procedure DrawText2D(x, y: Integer; AText: String; Alignment: TFreeTypeAlignments;
  WritingAngle: GLfloat = 0.0);
{
procedure DrawText(const AText: String; P, Dir: TVector3f; Angle, Size: GLfloat;
  Alignments: TFreeTypeAlignments);
  }
procedure SetFont(AFontName: String; AFontSize: Integer; AStyle: TFreeTypeStyles = []);
procedure TextExtent(const AText: String; out AWidth, AHeight: Integer);

procedure InitFonts(AFontDir: string = '');
function LoadFont(AFontName: String; AStyle: TFreeTypeStyles): TFreeTypeFont;

implementation

uses
  FileUtil, Math, LazFileUtils, LazFreeTypeFontCollection;

type
  TTextureCacheItem = class
    TextureID: Gluint;
    TextWidth: Integer;
    TextHeight: Integer;
  end;

  ToglFreeTypeHelper = class
  private
    FFont: TFreeTypeFont;
    FImg: TFPMemoryImage;
    FDrawer: TFPImageFreeTypeDrawer;
    FTextureCache: TStringList;
  protected
    function BuildTextureName(AText: String): String;
    procedure CreateTexture(AText: String; out ATextWidth, ATextHeight,
      ATextureWidth, ATextureHeight: Integer; out ATextureID: GLuint);
    function FindTexture(AText: String; out ATextWidth, ATextHeight,
      ATextureWidth, ATextureHeight: Integer): GLuint;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RenderText(AText: String; Alignments: TFreeTypeAlignments);
    procedure SetFont(AFontName: String; AFontSize: Integer;
      ABold: Boolean = false; AItalic: Boolean = false;
      AUnderline: Boolean = false; AStrikethrough: Boolean = false);
    procedure TextExtent(AText: String; out AWidth, AHeight: Integer);
  end;

var
  FontsInitialized: Boolean = false;
  oglFreeTypeHelper: ToglFreeTypeHelper = nil;

function NextPowerOf2(n: Integer): Integer;
begin
  Result := 1;
  while Result < n do
    Result := Result * 2;
end;

procedure PopulateFontDirList(AList: TStrings);
const
  CSIDL_FONTS = 20;
var
  s: String;
begin
  if AList = nil then
    raise Exception.Create('PopulateFontDirList: list not allocated.');

 {$IFDEF WINDOWS}
  s := SHGetFolderPathUTF8(CSIDL_FONTS);
  if s <> '' then
    AList.Add(s);
 {$ENDIF}
 {$IFDEF linux}
  AList.Add('/usr/share/cups/fonts/');
  AList.Add('/usr/share/fonts/truetype/');
  AList.Add('/usr/local/lib/X11/fonts/');
  AList.Add(GetUserDir + '.fonts/');
 {$ENDIF}
 {$IFDEF LCLCarbon}
  AList.Add('/Library/Fonts/');
  AList.Add('/System/Library/Fonts/');
  AList.Add('/Network/Library/Fonts/');
  AList.Add('~/Library/Fonts/');
 {$ENDIF}
 {$IFDEF LCLCocoa}
  AList.Add('/Library/Fonts/');
  AList.Add('/System/Library/Fonts/');
  AList.Add('/Network/Library/Fonts/');
  AList.Add('~/Library/Fonts/');
 {$ENDIF}
end;

function LoadFont(AFontName: String; AStyle: TFreeTypeStyles): TFreeTypeFont;
var
  familyItem: TCustomFamilyCollectionItem;
  fontItem: TCustomFontCollectionItem;
  style: String;
begin
  Result := nil;
  familyItem := FontCollection.Family[AFontName];
  if familyItem <> nil then begin
    style := '';
    if (ftsBold in AStyle) then style := 'Bold';
    if (ftsItalic in AStyle) then style := style + ' Italic';
    fontItem := familyItem.GetFont(style);
    if fontItem <> nil then begin
      Result := fontItem.CreateFont;
      Result.Style := AStyle;
    end;
  end;
end;

procedure InitFonts(AFontDir: String = '');

  { Duplicates functionality in FontCollection.AddFolder in order to be able to
    ignore exceptions due to font read errors (occur on Linux Mint with font
    NanumMyeongjo.ttf) }
  procedure AddFolder(AFolder: string);
  var
    files: TStringList;
    i: integer;
  begin
    AFolder := ExpandFileName(AFolder);
    if (length(AFolder) <> 0) and (AFolder[length(AFolder)] <> PathDelim) then
      AFolder += PathDelim;
    files := TStringList.Create;
    FontCollection.BeginUpdate;
    try
      FindAllFiles(files, AFolder, '*.ttf', true);
      files.Sort;
      for i := 0 to files.Count-1 do
        try
          FontCollection.AddFile(files[i]);
        except
        end;
    finally
      FontCollection.EndUpdate;
      files.Free;
    end;
  end;

var
  i: Integer;
  fontDirList: TStrings;
begin
  if FontsInitialized then
    exit;

  fontDirList := TStringList.Create;
  try
    PopulateFontDirList(fontDirList);
    if AFontDir <> '' then
      fontDirList.Text := AFontDir;
    for i:=0 to fontDirList.Count-1 do
      AddFolder(fontDirList[i]);
    FontsInitialized := true;

    if oglFreeTypeHelper = nil then
      oglFreeTypeHelper := ToglFreeTypeHelper.Create;
  finally
    fontDirList.Free;
  end;
end;


{ ToglFreeTypeHelper }

constructor ToglFreeTypeHelper.Create;
begin
  FImg := TFPMemoryImage.Create(8, 8);  // dummy size, will be updated when needed
  FDrawer := TFPImageFreeTypeDrawer.Create(FImg);
  FTextureCache := TStringList.Create;
  FTextureCache.Sorted := true;
end;

destructor ToglFreeTypeHelper.Destroy;
var
  i: Integer;
  item: TTextureCacheItem;
begin
  for i:=0 to FTextureCache.Count-1 do begin
    item := TTextureCacheItem(FTextureCache.Objects[i]);
    glDeleteTextures(1, @item.TextureID);
    item.Free;
  end;
  FTextureCache.Free;
  if FFont <> nil then FFont.Free;
  FDrawer.Free;
  FImg.Free;
  inherited;
end;

{ The texture items are stored in the FTextureCache list and can be identified
  by means of their name which is composed of the text and font parameters.
  The name of the texture items is calculated here. }
function ToglFreeTypeHelper.BuildTextureName(AText: String): String;
begin
  Result := Format('%s|%s|%d|%s', [
    AText, FFont.Family, round(FFont.SizeInPoints*100), FFont.StyleAsString
  ]);
end;

procedure ToglFreeTypeHelper.CreateTexture(AText: String; out ATextWidth, ATextHeight,
  ATextureWidth, ATextureHeight: Integer; out ATextureID: GLuint);
var
  expanded_data: packed array of byte;
  i, j: Integer;
  c: TFPColor;

//  writer: TFPWriterPNG;


begin
  if FFont = nil then
    raise Exception.Create('No font selected.');

  ATextWidth := round(FFont.TextWidth(AText));
  ATextHeight := round(FFont.TextHeight(AText));
  ATextureWidth := NextPowerOf2(ATextWidth);
  ATextureHeight := NextPowerOf2(ATextHeight);

  FImg.SetSize(ATextureWidth, ATextureHeight);
  FDrawer.FillPixels(colTransparent);
  FDrawer.DrawText(AText, FFont, 0,0, colRed, [ftaLeft, ftaTop]);
          {
  writer := TFPWriterPNG.Create;
  FDrawer.Image.SaveToFile('D:\test-ogl.png', writer);
  writer.Free;
           }
  SetLength(expanded_data, 2*ATextureWidth * ATextureHeight);
  for j:=0 to ATextureHeight-1 do
    for i:=0 to ATextureWidth-1 do
    begin
      expanded_data[2*(i + j*ATextureWidth)] := 255;     // Luminosity
      if (i > ATextWidth) or (j > ATextHeight) then
        expanded_data[2*(i + j*ATextureWidth) + 1] := 0  // Alpha
      else begin
        c := FImg.Colors[i,j];
        expanded_data[2*(i + j*ATextureWidth) + 1] := c.Alpha shr 8;
      end;
    end;

  // Set up texture parameters
  glGenTextures(1, @ATextureID);
  glBindTexture(GL_TEXTURE_2D, ATextureID);

  // Create the texture
  // Note that we are using GL_LUMINANCE_ALPHA to indicate that we are using
  // two-channel data
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, ATextureWidth, ATextureHeight, 0,
    GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @expanded_data[0]);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
end;

{ Finds the texture id for the given text. Returns the texture id and the
  size of text and texture. Note that the texture size must be a power of 2 and
  thus can be different from the text size. }
function ToglFreeTypeHelper.FindTexture(AText: String;
  out ATextWidth, ATextHeight, ATextureWidth, ATextureHeight: Integer): GLuint;
var
  idx: Integer;
  item: TTextureCacheItem;
  txname: String;
begin
  txname := BuildTextureName(AText);
  idx := FTextureCache.IndexOf(txname);
  if idx = -1 then begin
    CreateTexture(AText, ATextWidth, ATextHeight, ATextureWidth, ATextureHeight, Result);
    item := TTextureCacheItem.Create;
    item.TextureID := Result;
    item.TextWidth := ATextWidth;
    item.TextHeight := ATextHeight;
    FTextureCache.AddObject(txname, item);
  end else begin
    item := TTextureCacheItem(FTextureCache.Objects[idx]);
    result := item.TextureID;
    ATextWidth := item.TextWidth;
    ATextHeight := item.TextHeight;
    ATextureWidth := NextPowerOf2(ATextWidth);
    ATextureHeight := NextPowerOf2(ATextHeight);
  end;
end;

{ Writes the text an the origin of the xy plane, viewing direction +z. }
procedure ToglFreeTypeHelper.RenderText(AText: String;
  Alignments: TFreeTypeAlignments);
var
  textureID: GLuint;
  w, h: Integer;
  w2, h2: Integer;
  sx, sy: GLfloat;
  dx, dy: GLfloat;
  hasTexture2d: Boolean;
begin
  textureID := FindTexture(AText, w, h, w2, h2);
  sx := w / w2;
  sy := h / h2;

  // Note: The rendering process will produce centered text
  if (ftaCenter in Alignments) then dx := -w div 2
  else if (ftaRight in ALignments) then dx := -w
  else dx := 0;

  if (ftaVerticalCenter in Alignments) then dy := -h div 2
  else if (ftaBottom in Alignments) then dy := -h
  else if (ftaBaseline in Alignments) then dy := - h + round(FFont.Descent)
  else dy := 0;

  hasTexture2D := glIsEnabled(GL_TEXTURE_2D) = GL_TRUE;
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, textureID);
  glTranslatef(dx, dy, 0.0);   // Translate according to Alignments
  // write left/bottom-aligned text in x direction on xy plane;
  // the text is readable when looking in +z direction
  glBegin(GL_QUADS);
    glTexCoord2f(0.0, sy);  glVertex2f(0, h);
    glTexCoord2f(sx, sy);   glVertex2f(w, h);
    glTexCoord2f(sx, 0.0);  glVertex2f(w, 0);
    glTexCoord2f(0.0, 0.0); glVertex2f(0, 0);
  glEnd();
  if not hasTexture2D then
    glDisable(GL_TEXTURE_2D);
end;

procedure ToglFreeTypeHelper.SetFont(AFontName: String; AFontSize: Integer;
  ABold: Boolean = false; AItalic: Boolean = false;
  AUnderline: Boolean = false; AStrikethrough: Boolean = false);
var
  style: TFreeTypeStyles;
begin
  if oglFreeTypeHelper = nil then
    raise Exception.Create('InitFonts has not been called.');

  style := [];
  if ABold then Include(style, ftsBold);
  if AItalic then Include(style, ftsItalic);

  // Create a new font if not yet loaded
  if (FFont = nil) or (FFont.Family <> AFontName) or (FFont.Style <> style) then
  begin
    FreeAndNil(FFont);
    FFont := LoadFont(AFontName, style);
    if FFont = nil then
      raise Exception.CreateFmt('Font "%s" not found.', [AFontName]);
  end;

  // Set the requested font attributes.
  FFont.SizeInPoints := AFontSize;
  FFont.UnderlineDecoration := AUnderline;
  FFont.StrikeoutDecoration := AStrikethrough;
  FFont.Hinted := true;
  FFont.Quality := grqHighQuality;
  //FFont.ClearType := true;
end;

{ Returns the width and height of the specified text. If the text already has
  been handled with the same font parameters it is stored in the FTextureCache
  list. If not, the size is determined from the font. }
procedure ToglFreeTypeHelper.TextExtent(AText: String; out AWidth, AHeight: Integer);
var
  txname: String;
  idx: Integer;
  item: TTextureCacheItem;
  textureID: Gluint;
  w2, h2: Integer;
begin
  txname := BuildTextureName(AText);
  idx := FTextureCache.IndexOf(txname);
  if idx = -1 then begin
    CreateTexture(AText, AWidth, AHeight, w2, h2, textureID);
    item := TTextureCacheItem.Create;
    item.TextureID := textureID;
    item.TextWidth := AWidth;
    item.TextHeight := AHeight;
    idx := FTextureCache.AddObject(txname, item);
  end;

  item := TTextureCacheItem(FTextureCache.Objects[idx]);
  AWidth := item.TextWidth;
  AHeight := item.TextHeight;
end;


{ Public routines }
                                            {
procedure DrawText(const AText: String; P, Dir: TVector3f; Angle, Size: GLfloat;
  Alignments: TFreeTypeAlignments);
var
  base: TVector3f;
  rotAxis: TVector3f;
  rotAngle: GLfloat;
  w,h: Integer;
  dx, dy: GLfloat;
begin                     (*
  TextExtent(AText, w, h);
  if ftaLeft in Alignments then dx := +w div 2
    else if ftaRight in Alignments then dx := -w div 2;

  if ftaTop in Alignments then dy := +h div 2
    else if ftaBaseline in Alignments then dy := -h + FFont.Descent;
                            *)
  base := Vector3f(1, 0, 0);
  dir := VNormalize(Dir);
  if dir <> base then begin
    rotAxis := Cross(base, dir);
    rotAngle := arccos(Dot(rotAxis, base));
  end;

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glPushMatrix;
    glTranslatef(P.x, P.y, P.z);
    if (dir <> base) then
      glRotatef(RadToDeg(rotAngle), rotAxis.x, rotAxis.y, rotAxis.z);
    glRotatef(Angle, 1, 0, 0);
    glScalef(Size, Size, Size);
    oglFreeTypeHelper.RenderText(AText, Alignments);
  glPopMatrix;
end;
                                           }

{ Draws the text at position P
  By default the text is written in the xy plane such that it is readable when
  looking in the -z direction.
  - RotAxis and RotAngle: describes the rotation of the text around any direction
  - TextDir: inplane rotation of the text
  - Size: multiplier for the font size
  - Alignment: determine where the point P sits with respect to the text.
  - Flipped: The text is rotated by 180° around the writing direction. }
procedure DrawText(const AText: String; P, RotAxis: TVector3f; RotAngle: GLfloat;
  TextDir, Size: GLfloat; Alignment: TFreeTypeAlignments; Flipped: Boolean);
begin
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glPushMatrix;
    glTranslatef(P.x, P.y, P.z);
    glRotatef(RotAngle, RotAxis.X, RotAxis.Y, RotAxis.Z);
    glRotatef(TextDir, 0, 0, 1);         // inplane rotation
    glScalef(Size, Size, Size);
    if Flipped then glRotatef(180, 1, 0, 0);
    oglFreeTypeHelper.RenderText(AText, Alignment);
  glPopMatrix;
end;

{ Draws the text such that the origin is the reference point defined by
  Alignment }
procedure DrawText(const AText: String; Alignment: TFreeTypeAlignments);
var
  hasBlend: Boolean;
begin
  hasBlend := glIsEnabled(GL_BLEND) = GL_TRUE;
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glPushMatrix;
    oglFreeTypeHelper.RenderText(AText, Alignment);
  glPopMatrix;

  if not hasBlend then glDisable(GL_BLEND);
end;

{ x and y are SCREEN coordinates
  WritingAngle is the text direction, in radians, from horizontal axis, ccw}
procedure DrawText2D(x, y: Integer; AText: String; Alignment: TFreeTypeAlignments;
  WritingAngle: GLfloat = 0.0);
var
  VP: array[0..3] of Integer;
  hasLighting: Boolean;
  hasDepthTest: Boolean;
  hasBlend: Boolean;
begin
  // Get viewport
  glGetIntegerV(GL_VIEWPORT, @VP);

  // Set some OpenGL status variables
  hasLighting := glIsEnabled(GL_LIGHTING) = GL_TRUE;
  hasDepthTest := glIsEnabled(GL_DEPTH_TEST) = GL_TRUE;
  hasBlend := glIsEnabled(GL_BLEND) = GL_TRUE;
  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  // Save projection matrix
  glMatrixMode(GL_PROJECTION);
  glPushMatrix;

  // Switch to orthogonal view (2D)
  glLoadIdentity;
  glOrtho(0, VP[2], 0, VP[3], -1, 1);

  // Save ModelView matrix
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;

  // Prepare new ModelView matrix for 2D
  glLoadIdentity;

  // Draw text
  glTranslatef(x, y, 0);
  glRotatef(RadToDeg(WritingAngle), 0, 0, 1);
  glRotatef(180, 1, 0, 0);
  oglFreeTypeHelper.RenderText(AText, Alignment);

  // Restore projection matrix
  glMatrixMode(GL_PROJECTION);
  glPopMatrix;

  // Restore ModelView matrix
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;

  // Restore saved state
  if hasDepthTest then glEnable(GL_DEPTH_TEST);
  if hasLighting then glEnable(GL_LIGHTING);
  if not hasBlend then glDisable(GL_BLEND);
end;

{ x, y, z are 3d IMAGE coordinates }
procedure DrawText2D(x, y, z: GLfloat; AText: String; Alignment: TFreeTypeAlignments);
var
  VP: array[0..3] of Integer;   // Viewport
  MV: array[0..15] of double;   // ModelView matrix, must be double
  P: array[0..15] of double;    // Projection matrix
  {
  hasLighting: Boolean;
  hasDepthTest: Boolean;
  hasBlend: Boolean;
  }
  xs, ys, zs: Double;           // MUST be double
begin
  // Get current modelview matrix
  glGetDoublev(GL_MODELVIEW_MATRIX, @MV);
  // Get current project matrix
  glGetDoublev(GL_PROJECTION_MATRIX, @P);
  // Get viewport
  glGetIntegerV(GL_VIEWPORT, @VP);
  // map object coordinates to window coordinates
  gluProject(x, y, z, @MV, @P, @VP, @xs, @ys, @zs);

  DrawText2D(round(xs), round(ys), AText, Alignment, 0);
  {

  // Deactivate depth test and lighting
  hasLighting := glIsEnabled(GL_LIGHTING) = GL_TRUE;
  hasDepthTest := glIsEnabled(GL_DEPTH_TEST) = GL_TRUE;
  hasBlend := glIsEnabled(GL_BLEND) = GL_TRUE;
  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  // Save projection matrix
  glMatrixMode(GL_PROJECTION);
  glPushMatrix;

  // Switch to othogonal view (2D)
  glLoadIdentity;
  glOrtho(0, VP[2], 0, VP[3], -1, 1);

  // Save ModelView matrix
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;

  // Prepare new ModelView matrix for 2D
  glLoadIdentity;

  // Draw text;
  glTranslatef(xs, ys, 0);
  glRotatef(180, 1, 0, 0);
  oglFreeTypeHelper.RenderText(AText, Alignment);

  // Restore projection matrix
  glMatrixMode(GL_PROJECTION);
  glPopMatrix;

  // Restore ModelView matrix
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;

  // Restore saved state
  if hasDepthTest then glEnable(GL_DEPTH_TEST);
  if hasLighting then glEnable(GL_LIGHTING);
  if not hasBlend then glDisable(GL_BLEND);
  }
end;

procedure SetFont(AFontName: String; AFontSize: Integer; AStyle: TFreeTypeStyles = []);
begin
  if SameText(AFontName, 'default') then AFontName := 'Arial';
  if AFontSize = 0 then AFontSize := 9;
  oglFreeTypeHelper.SetFont(AFontName, AFontSize, ftsBold in AStyle, ftsItalic in AStyle);
end;

procedure TextExtent(const AText: String; out AWidth, AHeight: Integer);
begin
  oglFreeTypeHelper.TextExtent(AText, AWidth, AHeight);
end;

end.

