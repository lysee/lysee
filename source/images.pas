unit images;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

{$IFDEF MSWINDOWS}
{$IFNDEF WINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  {$IFNDEF FPC}Jpeg,{$ENDIF}
  {$IFNDEF FPC}PngImage,{$ENDIF}
  {$IFNDEF FPC}GifImg,{$ENDIF}
//{$IFNDEF FPC}LibTiffDelphi,{$ENDIF}
  {$IFNDEF FPC}Windows,{$ENDIF}
  SysUtils, Classes, Graphics, ExtCtrls, Basic;

type

  { TLyFileGraphic }

  TLyFileGraphic = class(TLyObject)
  private
    FFileName: string;
    FGraphic: TGraphic;
    FNext: TLyFileGraphic;
    FPrev: TLyFileGraphic;
    function GetWidth: integer;
    function GetHeight: integer;
    function GetIsGIF: boolean;
  public
    constructor Create;
    destructor Destroy;override;
    procedure SetImage(Image: TImage);
    property FileName: string read FFileName;
    property Graphic: TGraphic read FGraphic;
    property IsGIF: boolean read GetIsGIF;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property Next: TLyFileGraphic read FNext;
    property Prev: TLyFileGraphic read FPrev;
  end;

  {$IFDEF FPC}
  TPngImage = TPortableNetworkGraphic;
  {$ENDIF}

function IsImageFile(const Ext: string): boolean;
{$IFDEF FPC}
function LoadTIF(const FileName: string): {$IFDEF FPC}TTiffImage{$ELSE}TBitmap{$ENDIF};
{$ENDIF}
function LoadGIF(const FileName: string): TGifImage;
function LoadJPG(const FileName: string): TJpegImage;
function LoadBMP(const FileName: string): TBitmap;
function LoadICO(const FileName: string): TIcon;
function LoadPNG(const FileName: string): TPngImage;
function LoadWMF(const FileName: string): {$IFDEF FPC}TGraphic{$ELSE}TMetafile{$ENDIF};
function LoadEMF(const FileName: string): {$IFDEF FPC}TGraphic{$ELSE}TMetafile{$ENDIF};
function LoadGraphic(const FileName: string): TGraphic;
function TryLoadGraphic(const FileName: string): TGraphic;

function LoadImage(Image: TImage; const FileName: string): boolean;
function SetImageGraphic(Image: TImage; G: TGraphic): TGraphic;

function FindFileGraphic(const FileName: string): TLyFileGraphic;
function GetFileGraphic(const FileName: string): TLyFileGraphic;
procedure ReleaseFileGraphics(All: boolean);

implementation

var
  my_graphic: TLyFileGraphic;

function IsImageFile(const Ext: string): boolean;
begin
  Result := InStrings(LowerCase(Ext),
    ['.bmp', '.ico', '.wmf', '.emf', '.jpg', '.jpeg', '.tif', '.tiff',
     '.png', '.gif']);
end;

{$IFDEF FPC}
function LoadTIF(const FileName: string): {$IFDEF FPC}TTiffImage{$ELSE}TBitmap{$ENDIF};
{$IFDEF FPC}
begin
  Result := TTiffImage.Create;
  try
    Result.LoadFromFile(FileName);
  except
    Result.Free;
    raise;
  end;
end;
{$ELSE}
var
  F: PTIFF;
  W, H, V, I: cardinal;
  M: pcardinal;
begin
  F := TIFFOpen(FileName, 'r');
  if F = nil then
    raise Exception.Create('Unable to open file ''' + FileName + '''');
  TIFFGetField(F, TIFFTAG_IMAGEWIDTH, @W);
  TIFFGetField(F, TIFFTAG_IMAGELENGTH, @H);

  Result := TBitmap.Create;
  try
    Result.PixelFormat := pf32bit;
    Result.Width := W;
    Result.Height := H;
  except
    FreeAndNil(Result);
    TIFFClose(F);
    raise Exception.Create('Unable to create TBitmap buffer');
  end;

  TIFFReadRGBAImage(F, W, H, Result.Scanline[H - 1], 0);
  TIFFClose(F);
  M := Result.Scanline[H - 1];
  for I := 0 to W * H - 1 do
  begin
    V := M^;
    M^ := (V and $FF00FF00) or         // G and A
         ((V and $00FF0000) shr 16) or // B
         ((V and $000000FF) shl 16);   // R
    Inc(M);
  end;
end;
{$ENDIF}
{$ENDIF}

function LoadGIF(const FileName: string): TGifImage;
begin
  Result := TGifImage.Create;
  try
    Result.LoadFromFile(FileName);
  //Result.Animate := true;
  except
    Result.Free;
    raise;
  end;
end;

function LoadJPG(const FileName: string): TJpegImage;
begin
  Result := TJpegImage.Create;
  try
    Result.LoadFromFile(FileName);
  except
    Result.Free;
    raise;
  end;
end;

function LoadBMP(const FileName: string): TBitmap;
begin
  Result := TBitmap.Create;
  try
    Result.LoadFromFile(FileName);
  except
    Result.Free;
    raise;
  end;
end;

function LoadICO(const FileName: string): TIcon;
begin
  Result := TIcon.Create;
  try
    Result.LoadFromFile(FileName);
  except
    Result.Free;
    raise;
  end;
end;

function LoadPNG(const FileName: string): TPngImage;
begin
  Result := TPngImage.Create;
  try
    Result.LoadFromFile(FileName);
  except
    Result.Free;
    raise;
  end;
end;

function LoadWMF(const FileName: string): {$IFDEF FPC}TGraphic{$ELSE}TMetafile{$ENDIF};
begin
  {$IFDEF FPC}
  Result := nil;
  {$ELSE}
  Result := TMetafile.Create;
  try
    Result.LoadFromFile(FileName);
  except
    Result.Free;
    raise;
  end;
  {$ENDIF}
end;

function LoadEMF(const FileName: string): {$IFDEF FPC}TGraphic{$ELSE}TMetafile{$ENDIF};
begin
  Result := LoadWMF(FileName);
end;

function LoadGraphic(const FileName: string): TGraphic;
var
  X: string;
begin
  X := LowerCase(ExtractFileExt(FileName));
  if X = '.gif' then
    Result := LoadGIF(FileName) else
  if (X = '.jpg') or (X = '.jpeg') then
    Result := LoadJPG(FileName) else
  if X = '.png' then
    Result := LoadPNG(FileName) else
  {$IFDEF FPC}
  if (X = '.tif') or (X = '.tiff') then
    Result := LoadTIF(FileName) else
  {$ENDIF}
  if X = '.bmp' then
    Result := LoadBMP(FileName) else
  if X = '.wmf' then
    Result := LoadWMF(FileName) else
  if X = '.emf' then
    Result := LoadEMF(FileName) else
  if X = '.ico' then
    Result := LoadICO(FileName) else
    Result := nil;
end;

function TryLoadGraphic(const FileName: string): TGraphic;
begin
  try
    if FileExists(FileName) then
      Result := LoadGraphic(FileName) else
      Result := nil;
  except
    Result := nil;
  end;
end;

function LoadImage(Image: TImage; const FileName: string): boolean;
var
  G: TGraphic;
begin
  G := LoadGraphic(FileName);
  try
    Result := (G <> nil);
    SetImageGraphic(Image, G);
  finally
    FreeAndNil(G);
  end;
end;

function SetImageGraphic(Image: TImage; G: TGraphic): TGraphic;
begin
  {$IFNDEF FPC}
  if G <> nil then
    if G is TGifImage then
      (G as TGifImage).Animate := true;
  {$ENDIF}
  Image.Picture.Graphic := G;
  Result := G;
end;

function FindFileGraphic(const FileName: string): TLyFileGraphic;
var
  F: string;
begin
  F := ExpandFileName(SetPD(FileName));
  Result := my_graphic;
  while Result <> nil do
  begin
    if SameFileName(F, Result.FFileName) then Exit;
    Result := Result.FNext;
  end;
end;

function GetFileGraphic(const FileName: string): TLyFileGraphic;
var
  F: string;
  G: TGraphic;
begin
  F := ExpandFileName(SetPD(FileName));

  Result := my_graphic;
  while Result <> nil do
  begin
    if SameFileName(F, Result.FFileName) then Exit;
    Result := Result.FNext;
  end;

  G := TryLoadGraphic(F);
  if G <> nil then
  begin
    Result := TLyFileGraphic.Create;
    Result.FGraphic := G;
    Result.FFileName := F;
    Result.IncRefcount;
  end;
end;

procedure ReleaseFileGraphics(All: boolean);
var
  G, N: TLyFileGraphic;
begin
  G := my_graphic;
  while G <> nil do
  begin
    N := G.FNext;
    if All or (G.RefCount = 1) then
      G.DecRefcount;
    G := N;
  end;
end;

{ TLyFileGraphic }

constructor TLyFileGraphic.Create;
begin
  if my_graphic <> nil then
  begin
    FNext := my_graphic;
    FNext.FPrev := Self;
  end;
  my_graphic := Self;
end;

destructor TLyFileGraphic.Destroy;
begin
  if Self = my_graphic then
  begin
    my_graphic := FNext;
    if FNext <> nil then
      FNext.FPrev := nil;
  end
  else
  begin
    if FPrev <> nil then
      FPrev.FNext := FNext;
    if FNext <> nil then
      FNext.FPrev := FPrev;
  end;
  FFileName := '';
  FreeAndNil(FGraphic);
  inherited;
end;

function TLyFileGraphic.GetHeight: integer;
begin
  if FGraphic <> nil then
    Result := FGraphic.Height else
    Result := 0;
end;

function TLyFileGraphic.GetIsGIF: boolean;
begin
  Result := (FGraphic <> nil) and SameText('.gif', ExtractFileExt(FFileName));
end;

function TLyFileGraphic.GetWidth: integer;
begin
  if FGraphic <> nil then
    Result := FGraphic.Width else
    Result := 0;
end;

procedure TLyFileGraphic.SetImage(Image: TImage);
begin
  SetImageGraphic(Image, FGraphic);
end;

end.
