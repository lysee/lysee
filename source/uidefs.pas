unit UIDefs;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF FPC}LCLType, LCLIntf,{$ELSE}Windows, Messages,{$ENDIF}
  SysUtils, Classes, Controls, Graphics, StdCtrls, ExtCtrls, Forms;

const

  COLOR_TEXT               = clWindowText;
  COLOR_BACKGROUND         = clWindow;
  COLOR_SELECTEDTEXT       = clWindow;
  COLOR_SELECTEDBACKGROUND = $694D00; // clBlue;

  { tokens }

  TK_SPACE      =  0;
  TK_KEYWORD    =  1;
  TK_ID         =  2;
  TK_OPERATOR   =  3;
  TK_DELIMITER  =  4;
  TK_NUMBER     =  5; // $... or [0-9]...
  TK_ENUMBER    =  6;
  TK_CHAR       =  7; // #... or #$... or '.'
  TK_ECHAR      =  8;
  TK_STRING     =  9; // "..." or '...'
  TK_ESTRING    = 10;
  TK_COMMENT    = 11; // {...} or (*...*) or /*...*/ or //...
  TK_ECOMMENT   = 12; // {...  or /*...
  TK_ECOMMENTP  = 13; // (*...
  TK_DIRECTIVE  = 14; // {$...}
  TK_EDIRECTIVE = 15; // {$...
  TK_BOOL       = 16;
  TK_TAG        = 17;
  TK_MARK       = 18;
  TK_HILIGHT    = 19;
  TK_RAWDATA    = 20;
  TK_UNKNOWN    = 21;
  TK_CUSTOM     = 100; // start your tokens here

  { Font Style}

  FontStyleName: array[TFontStyle] of string = (
    'bold', 'italic', 'underline', 'strikeOut');

type

  TLyKeyDown = class;{forward}

  TLyHorzAlign = (haLeft, haCenter, haRight);
  TLyVertAlign = (vaTop, vaMiddle, vaBottom);

  TLyTokenID = smallint;

  { TLyPalette }

  TLyPalette = class
  private
    FTextColor: TColor;
    FBackground: TColor;
    FSelectedTextColor: TColor;
    FSelectedBackground: TColor;
    FLeftBarBackground: TColor;
    FChangedBackground: TColor;
    FActiveBackground: TColor;
    FCaretColor: TColor;
    FLine80Color: TColor;
    FLineMarkBackground: TColor;
    { token color }
    FUnknownColor: TColor;
    FKeywordColor: TColor;
    FIDColor: TColor;
    FIDStyle: TFontStyles;
    FSpaceColor: TColor;
    FOperatorColor: TColor;
    FDelimiterColor: TColor;
    FNumberColor: TColor;
    FENumberColor: TColor;
    FCharColor: TColor;
    FECharColor: TColor;
    FStringColor: TColor;
    FEStringColor: TColor;
    FBoolColor: TColor;
    FCommentColor: TColor;
    FECommentColor: TColor;
    FECommentPColor: TColor;
    FDirectiveColor: TColor;
    FEDirectiveColor: TColor;
    FTagColor: TColor;
    FMarkColor: TColor;
    FHilightColor: TColor;
    FRawDataColor: TColor;
    { token style }
    FUnknownStyle: TFontStyles;
    FKeywordStyle: TFontStyles;
    FSpaceStyle: TFontStyles;
    FOperatorStyle: TFontStyles;
    FDelimiterStyle: TFontStyles;
    FNumberStyle: TFontStyles;
    FENumberStyle: TFontStyles;
    FCharStyle: TFontStyles;
    FECharStyle: TFontStyles;
    FStringStyle: TFontStyles;
    FEStringStyle: TFontStyles;
    FBoolStyle: TFontStyles;
    FCommentStyle: TFontStyles;
    FECommentStyle: TFontStyles;
    FECommentPStyle: TFontStyles;
    FDirectiveStyle: TFontStyles;
    FEDirectiveStyle: TFontStyles;
    FTagStyle: TFontStyles;
    FMarkStyle: TFontStyles;
    FHilightStyle: TFontStyles;
    FRawDataStyle: TFontStyles;
  public
    constructor Create;virtual;
    function Apply(Font: TFont; Token: TLyTokenID; Selected: boolean): boolean;virtual;
    function GetStyle(Token: TLyTokenID): TFontStyles;virtual;
    function GetColor(Token: TLyTokenID): TColor;virtual;
    property TextColor: TColor read FTextColor write FTextColor;
    property Background: TColor read FBackground write FBackground;
    property SelectedTextColor: TColor read FSelectedTextColor write FSelectedTextColor;
    property SelectedBackground: TColor read FSelectedBackground write FSelectedBackground;
    { codeedit }
    property LeftBarBackground: TColor read FLeftBarBackground write FLeftBarBackground;
    property ChangedBackground: TColor read FChangedBackground write FChangedBackground;
    property ActiveBackground: TColor read FActiveBackground write FActiveBackground;
    property CaretColor: TColor read FCaretColor write FCaretColor;
    property Line80Color: TColor read FLine80Color write FLine80Color;
    property LineMarkBackground: TColor read FLineMarkBackground write FLineMarkBackground;
    { token color }
    property UnknownColor: TColor read FUnknownColor write FUnknownColor;
    property KeywordColor: TColor read FKeywordColor write FKeywordColor;
    property IDColor: TColor read FIDColor write FIDColor;
    property SpaceColor: TColor read FSpaceColor write FSpaceColor;
    property OperatorColor: TColor read FOperatorColor write FOperatorColor;
    property DelimiterColor: TColor read FDelimiterColor write FDelimiterColor;
    property NumberColor: TColor read FNumberColor write FNumberColor;
    property ENumberColor: TColor read FENumberColor write FENumberColor;
    property CharColor: TColor read FCharColor write FCharColor;
    property ECharColor: TColor read FECharColor write FECharColor;
    property StringColor: TColor read FStringColor write FStringColor;
    property EStringColor: TColor read FEStringColor write FEStringColor;
    property BoolColor: TColor read FBoolColor write FBoolColor;
    property CommentColor: TColor read FCommentColor write FCommentColor;
    property ECommentColor: TColor read FECommentColor write FECommentColor;
    property ECommentPColor: TColor read FECommentPColor write FECommentPColor;
    property DirectiveColor: TColor read FDirectiveColor write FDirectiveColor;
    property EDirectiveColor: TColor read FEDirectiveColor write FEDirectiveColor;
    property TagColor: TColor read FTagColor write FTagColor;
    property MarkColor: TColor read FMarkColor write FMarkColor;
    property HilightColor: TColor read FHilightColor write FHilightColor;
    property RawDataColor: TColor read FRawDataColor write FRawDataColor;
    { token style }
    property UnknownStyle: TFontStyles read FUnknownStyle write FUnknownStyle;
    property KeywordStyle: TFontStyles read FKeywordStyle write FKeywordStyle;
    property IDStyle: TFontStyles read FIDStyle write FIDStyle;
    property SpaceStyle: TFontStyles read FSpaceStyle write FSpaceStyle;
    property OperatorStyle: TFontStyles read FOperatorStyle write FOperatorStyle;
    property DelimiterStyle: TFontStyles read FDelimiterStyle write FDelimiterStyle;
    property NumberStyle: TFontStyles read FNumberStyle write FNumberStyle;
    property ENumberStyle: TFontStyles read FENumberStyle write FENumberStyle;
    property CharStyle: TFontStyles read FCharStyle write FCharStyle;
    property ECharStyle: TFontStyles read FECharStyle write FECharStyle;
    property StringStyle: TFontStyles read FStringStyle write FStringStyle;
    property EStringStyle: TFontStyles read FEStringStyle write FEStringStyle;
    property BoolStyle: TFontStyles read FBoolStyle write FBoolStyle;
    property CommentStyle: TFontStyles read FCommentStyle write FCommentStyle;
    property ECommentStyle: TFontStyles read FECommentStyle write FECommentStyle;
    property ECommentPStyle: TFontStyles read FECommentPStyle write FECommentPStyle;
    property DirectiveStyle: TFontStyles read FDirectiveStyle write FDirectiveStyle;
    property EDirectiveStyle: TFontStyles read FEDirectiveStyle write FEDirectiveStyle;
    property TagStyle: TFontStyles read FTagStyle write FTagStyle;
    property MarkStyle: TFontStyles read FMarkStyle write FMarkStyle;
    property HilightStyle: TFontStyles read FHilightStyle write FHilightStyle;
    property RawDataStyle: TFontStyles read FRawDataStyle write FRawDataStyle;
  end;

  { TLyKeyShift }

  TLyKeyEvent = procedure(Sender: TObject; Shift: TShiftState) of object;

  TLyKeyShift = class
  private
    FOwner: TLyKeyDown;
    FKey: Word;
    FShift: TShiftState;
    FHandler: TLyKeyEvent;
    FNext: TLyKeyShift;
  protected
    procedure Enter(AOwner: TLyKeyDown);
    procedure Leave;
  public
    constructor Create(AOwner: TLyKeyDown);virtual;
    destructor Destroy;override;
    property Owner: TLyKeyDown read FOwner write Enter;
    property Key: Word read FKey write FKey;
    property Shift: TShiftState read FShift write FShift;
    property Handler: TLyKeyEvent read FHandler write FHandler;
  end;

  { TLyKeyDown }

  TLyKeyDown = class
  private
    FOwner: TObject;
    FFirst: TLyKeyShift;
  public
    constructor Create(AOwner: TObject);virtual;
    destructor Destroy;override;
    function Add(Key: Word; Shift: TShiftState; Handler: TLyKeyEvent): TLyKeyShift;
    function Find(Key: Word; Shift: TShiftState): TLyKeyShift;
    function Process(Sender: TObject; Key: Word; Shift: TShiftState): boolean;
    property Owner: TObject read FOwner write FOwner;
  end;

function Palette: TLyPalette;
function TokenSetCanvas(Canvas: TCanvas; Token: TLyTokenID; Selected: boolean): boolean;overload;
function TokenSetCanvas(Canvas: TCanvas; Token: TLyTokenID): boolean;overload;
function TokenSetFont(Font: TFont; Token: TLyTokenID; Selected: boolean): boolean;overload;
function TokenSetFont(Font: TFont; Token: TLyTokenID): boolean;overload;
function TokenColor(Token: TLyTokenID): TColor;
function TokenSelectedColor(Token: TLyTokenID): TColor;
function TokenStyle(Token: TLyTokenID): TFontStyles;

{ font }

function FontStyleToStr(AStyle: TFontStyles): string;
function StrToFontStyle(const AStr: string): TFontStyles;

{ color }

function IDToColor(const ID: string; var Color: TColor): boolean;
function ColorToID(Color: TColor): string;

{ screen }

function ScreenWidth(MM: currency): integer;
function ScreenHeight(MM: currency): integer;

{ shortcut }

function StrToShortCut(const AStr: string): TShortCut;

implementation

uses
  UITypes, Menus;

var
  my_palette: TLyPalette;

function Palette: TLyPalette;
begin
  if my_palette = nil then
    my_palette := TLyPalette.Create;
  Result := my_palette;
end;

function TokenSetCanvas(Canvas: TCanvas; Token: TLyTokenID; Selected: boolean): boolean;
begin
  Result := Palette.Apply(Canvas.Font, Token, Selected);
end;

function TokenSetCanvas(Canvas: TCanvas; Token: TLyTokenID): boolean;
begin
  Result := Palette.Apply(Canvas.Font, Token, false);
end;

function TokenSetFont(Font: TFont; Token: TLyTokenID; Selected: boolean): boolean;
begin
  Result := Palette.Apply(Font, Token, Selected);
end;

function TokenSetFont(Font: TFont; Token: TLyTokenID): boolean;
begin
  Result := Palette.Apply(Font, Token, false);
end;

function TokenColor(Token: TLyTokenID): TColor;
begin
  Result := Palette.GetColor(Token);
end;

function TokenSelectedColor(Token: TLyTokenID): TColor;
begin
  Result := Palette.SelectedTextColor;
end;

function TokenStyle(Token: TLyTokenID): TFontStyles;
begin
  Result := Palette.GetStyle(Token);
end;

function FontStyleToStr(AStyle: TFontStyles): string;
var
  I: TFontStyle;
begin
  Result := '';
  for I := Low(TFontStyle) to High(TFontStyle) do
    if I in AStyle then
      if Result <> '' then
        Result := Result + ' ' + FontStyleName[I] else
        Result := FontStyleName[I];
end;

function StrToFontStyle(const AStr: string): TFontStyles;
var
  I: TFontStyle;
  S: string;
begin
  Result := [];
  S := LowerCase(AStr);
  for I := Low(TFontStyle) to High(TFontStyle) do
    if Pos(FontStyleName[I], S) > 0 then
      Result := Result + [I];
end;

const
  color_map: array [0..146] of TIdentMapEntry = (
  //(Value: integer($BBGGRR);     Name: 'color'),
    (Value: integer($FFF8F0);     Name: 'Aliceblue'),
    (Value: integer($D7EBFA);     Name: 'Antiquewhite'),
    (Value: integer(clAqua);      Name: 'Aqua'),
    (Value: integer($D4FF7F);     Name: 'Aquamarine'),
    (Value: integer($FFFFF0);     Name: 'Azure'),
    (Value: integer($DCF5F5);     Name: 'Beige'),
    (Value: integer($C4E4FF);     Name: 'Bisque'),
    (Value: integer(clBlack);     Name: 'Black';    ),
    (Value: integer($CDEBFF);     Name: 'Blanchedalmond'),
    (Value: integer(clBlue);      Name: 'Blue'),
    (Value: integer($E22B8A);     Name: 'Blueviolet'),
    (Value: integer($2A2AA5);     Name: 'Brown'),
    (Value: integer($87B8DE);     Name: 'Burlywood'),
    (Value: integer($A09E5F);     Name: 'Cadetblue'),
    (Value: integer($00FF7F);     Name: 'Chartreuse'),
    (Value: integer($1E69D2);     Name: 'Chocolate'),
    (Value: integer($507FFF);     Name: 'Coral'),
    (Value: integer($ED9564);     Name: 'Cornflowerblue'),
    (Value: integer($DCF8FF);     Name: 'Cornsilk'),
    (Value: integer($3C14DC);     Name: 'Crimson'),
    (Value: integer($FFFF00);     Name: 'Cyan'),
    (Value: integer($8B0000);     Name: 'Darkblue'),
    (Value: integer($8B8B00);     Name: 'Darkcyan'),
    (Value: integer($0B86B8);     Name: 'Darkgoldenrod'),
    (Value: integer(clDkGray);    Name: 'Darkgray'),
    (Value: integer($006400);     Name: 'Darkgreen'),
    (Value: integer($A9A9A9);     Name: 'Darkgrey'),
    (Value: integer($6BB7BD);     Name: 'Darkkhaki'),
    (Value: integer($8B008B);     Name: 'Darkmagenta'),
    (Value: integer($2F6B55);     Name: 'Darkolivegreen'),
    (Value: integer($008CFF);     Name: 'Darkorange'),
    (Value: integer($CC3299);     Name: 'Darkorchid'),
    (Value: integer($00008B);     Name: 'Darkred'),
    (Value: integer($7A96E9);     Name: 'Darksalmon'),
    (Value: integer($8FBC8F);     Name: 'Darkseagreen'),
    (Value: integer($8B3D48);     Name: 'Darkslateblue'),
    (Value: integer($4F4F2F);     Name: 'Darkslategray'),
    (Value: integer($4F4F2F);     Name: 'Darkslategrey'),
    (Value: integer($D1CE00);     Name: 'Darkturquoise'),
    (Value: integer($D30094);     Name: 'Darkviolet'),
    (Value: integer($9314FF);     Name: 'Deeppink'),
    (Value: integer($FFBF00);     Name: 'Deepskyblue'),
    (Value: integer($696969);     Name: 'Dimgray'),
    (Value: integer($696969);     Name: 'Dimgrey'),
    (Value: integer($FF901E);     Name: 'Dodgerblue'),
    (Value: integer($2222B2);     Name: 'Firebrick'),
    (Value: integer($F0FAFF);     Name: 'Floralwhite'),
    (Value: integer($228B22);     Name: 'Forestgreen'),
    (Value: integer(clFuchsia);   Name: 'Fuchsia'),
    (Value: integer($DCDCDC);     Name: 'Gainsboro'),
    (Value: integer($FFF8F8);     Name: 'Ghostwhite'),
    (Value: integer($00D7FF);     Name: 'Gold'),
    (Value: integer($20A5DA);     Name: 'Goldenrod'),
    (Value: integer(clGray);      Name: 'Gray'),
    (Value: integer(clGreen);     Name: 'Green'),
    (Value: integer($2FFFAD);     Name: 'Greenyellow'),
    (Value: integer($808080);     Name: 'Grey'),
    (Value: integer($F0FFF0);     Name: 'Honeydew'),
    (Value: integer($B469FF);     Name: 'Hotpink'),
    (Value: integer($5C5CCD);     Name: 'Indianred'),
    (Value: integer($82004B);     Name: 'Indigo'),
    (Value: integer($F0FFFF);     Name: 'Ivory'),
    (Value: integer($8CE6F0);     Name: 'Khaki'),
    (Value: integer($FAE6E6);     Name: 'Lavender'),
    (Value: integer($F5F0FF);     Name: 'Lavenderblush'),
    (Value: integer($00FC7C);     Name: 'Lawngreen'),
    (Value: integer($CDFAFF);     Name: 'Lemonchiffon'),
    (Value: integer($E6D8AD);     Name: 'Lightblue'),
    (Value: integer($8080F0);     Name: 'Lightcoral'),
    (Value: integer($FFFFE0);     Name: 'Lightcyan'),
    (Value: integer($D2FAFA);     Name: 'Lightgoldenrodyellow'),
    (Value: integer(clLtGray);    Name: 'Lightgray'),
    (Value: integer($90EE90);     Name: 'Lightgreen'),
    (Value: integer($D3D3D3);     Name: 'Lightgrey'),
    (Value: integer($C1B6FF);     Name: 'Lightpink'),
    (Value: integer($7AA0FF);     Name: 'Lightsalmon'),
    (Value: integer($AAB220);     Name: 'Lightseagreen'),
    (Value: integer($FACE87);     Name: 'Lightskyblue'),
    (Value: integer($998877);     Name: 'Lightslategray'),
    (Value: integer($998877);     Name: 'Lightslategrey'),
    (Value: integer($DEC4B0);     Name: 'Lightsteelblue'),
    (Value: integer($E0FFFF);     Name: 'Lightyellow'),
    (Value: integer(clLime);      Name: 'Lime'),
    (Value: integer($32CD32);     Name: 'Limegreen'),
    (Value: integer($E6F0FA);     Name: 'Linen'),
    (Value: integer($FF00FF);     Name: 'Magenta'),
    (Value: integer(clMaroon);    Name: 'Maroon'),
    (Value: integer($AACD66);     Name: 'Mediumaquamarine'),
    (Value: integer($CD0000);     Name: 'Mediumblue'),
    (Value: integer($D355BA);     Name: 'Mediumorchid'),
    (Value: integer($DB7093);     Name: 'Mediumpurple'),
    (Value: integer($71B33C);     Name: 'Mediumseagreen'),
    (Value: integer($EE687B);     Name: 'Mediumslateblue'),
    (Value: integer($9AFA00);     Name: 'Mediumspringgreen'),
    (Value: integer($CCD148);     Name: 'Mediumturquoise'),
    (Value: integer($8515C7);     Name: 'Mediumvioletred'),
    (Value: integer($701919);     Name: 'Midnightblue'),
    (Value: integer($FAFFF5);     Name: 'Mintcream'),
    (Value: integer($E1E4FF);     Name: 'Mistyrose'),
    (Value: integer($B5E4FF);     Name: 'Moccasin'),
    (Value: integer($ADDEFF);     Name: 'Navajowhite'),
    (Value: integer(clNavy);      Name: 'Navy'),
    (Value: integer($E6F5FD);     Name: 'Oldlace'),
    (Value: integer(clOlive);     Name: 'Olive'),
    (Value: integer($238E6B);     Name: 'Olivedrab'),
    (Value: integer($00A5FF);     Name: 'Orange'),
    (Value: integer($0045FF);     Name: 'Orangered'),
    (Value: integer($D670DA);     Name: 'Orchid'),
    (Value: integer($AAE8EE);     Name: 'Palegoldenrod'),
    (Value: integer($98FB98);     Name: 'Palegreen'),
    (Value: integer($EEEEAF);     Name: 'Paleturquoise'),
    (Value: integer($9370DB);     Name: 'Palevioletred'),
    (Value: integer($D5EFFF);     Name: 'Papayawhip'),
    (Value: integer($B9DAFF);     Name: 'Peachpuff'),
    (Value: integer($3F85CD);     Name: 'Peru'),
    (Value: integer($CBC0FF);     Name: 'Pink'),
    (Value: integer($DDA0DD);     Name: 'Plum'),
    (Value: integer($E6E0B0);     Name: 'Powderblue'),
    (Value: integer(clPurple);    Name: 'Purple'),
    (Value: integer(clRed);       Name: 'Red'),
    (Value: integer($8F8FBC);     Name: 'Rosybrown'),
    (Value: integer($E16941);     Name: 'Royalblue'),
    (Value: integer($13458B);     Name: 'Saddlebrown'),
    (Value: integer($7280FA);     Name: 'Salmon'),
    (Value: integer($60A4F4);     Name: 'Sandybrown'),
    (Value: integer($578B2E);     Name: 'Seagreen'),
    (Value: integer($EEF5FF);     Name: 'Seashell'),
    (Value: integer($2D52A0);     Name: 'Sienna'),
    (Value: integer(clSilver);    Name: 'Silver'),
    (Value: integer($EBCE87);     Name: 'Skyblue'),
    (Value: integer($CD5A6A);     Name: 'Slateblue'),
    (Value: integer($908070);     Name: 'Slategray'),
    (Value: integer($908070);     Name: 'Slategrey'),
    (Value: integer($FAFAFF);     Name: 'Snow'),
    (Value: integer($7FFF00);     Name: 'Springgreen'),
    (Value: integer($B48246);     Name: 'Steelblue'),
    (Value: integer($8CB4D2);     Name: 'Tan'),
    (Value: integer(clTeal);      Name: 'Teal'),
    (Value: integer($D8BFD8);     Name: 'Thistle'),
    (Value: integer($4763FF);     Name: 'Tomato'),
    (Value: integer($D0E040);     Name: 'Turquoise'),
    (Value: integer($EE82EE);     Name: 'Violet'),
    (Value: integer($B3DEF5);     Name: 'Wheat'),
    (Value: integer(clWhite);     Name: 'White'),
    (Value: integer($F5F5F5);     Name: 'Whitesmoke'),
    (Value: integer(clYellow);    Name: 'Yellow'),
    (Value: integer($32CD9A);     Name: 'Yellowgreen')
  );

function IDToColor(const ID: string; var Color: TColor): boolean;
var
  X: longint;
  V: integer;
begin
  Result := (ID <> '');
  if Result then
    if CharInSet(ID[1], ['#', '@']) then
    begin
      Result := TryStrToInt('$' + Copy(ID, 2, Length(ID)), V);
      if Result then
        Color := TColor(V) else
        Result := IDToColor(Copy(ID, 2, Length(ID) - 1), Color);
    end
    else
    begin
      Result := IdentToInt(ID, X, color_map);
      if Result then
        Color := TColor(X);
    end;
end;

function ColorToID(Color: TColor): string;
begin
  Result := '';
  if not IntToIdent(integer(Color), Result, color_map) then
    Result := Format('#%0.8x', [integer(Color)]);
end;

function ScreenWidth(MM: currency): integer;
begin
  Result := Round((MM * Screen.PixelsPerInch) / 25.4);
end;

function ScreenHeight(MM: currency): integer;
begin
  Result := Round((MM * Screen.PixelsPerInch) / 25.4);
end;

function StrToShortCut(const AStr: string): TShortCut;
const
  ALNUM = ['A'..'Z', 'a'..'z', '0'..'9'];
var
  I, B: integer;
  T: string;
  S: TShiftState;
  K: word;
begin
  S := [];
  K := 0;
  B := 1;
  I := 1;
  while I <= Length(AStr) do
  begin
    if not CharInSet(AStr[I], ALNUM) then
    begin
      T := Copy(AStr, B, I - B);
      if SameText(T, 'Alt') then Include(S, ssAlt) else
      if SameText(T, 'Alter') then Include(S, ssAlt) else
      {$IFNDEF FPC}
      if SameText(T, 'Command') then Include(S, ssCommand) else
      if SameText(T, 'Cmd') then Include(S, ssCommand) else
      {$ENDIF}
      if SameText(T, 'Ctrl') then Include(S, ssCtrl) else
      if SameText(T, 'Control') then Include(S, ssCtrl) else
      if SameText(T, 'Shift') then Include(S, ssShift);
      B := I + 1;
    end;
    Inc(I);
  end;
  T := Copy(AStr, B, I - B);
  if T <> '' then
    if Length(T) = 1 then K := Ord(UpperCase(T)[1]) else
    if SameText(T, 'F1') then K := VK_F1 else
    if SameText(T, 'F2') then K := VK_F2 else
    if SameText(T, 'F3') then K := VK_F3 else
    if SameText(T, 'F4') then K := VK_F4 else
    if SameText(T, 'F5') then K := VK_F5 else
    if SameText(T, 'F6') then K := VK_F6 else
    if SameText(T, 'F7') then K := VK_F7 else
    if SameText(T, 'F8') then K := VK_F8 else
    if SameText(T, 'F9') then K := VK_F9 else
    if SameText(T, 'F10') then K := VK_F10 else
    if SameText(T, 'F11') then K := VK_F11 else
    if SameText(T, 'F12') then K := VK_F12 else
    if SameText(T, 'Esc') then K := VK_ESCAPE else
    if SameText(T, 'Escape') then K := VK_ESCAPE else
    if SameText(T, 'Tab') then K := Ord(#9) else
    if SameText(T, 'Enter') then K := Ord(#10) else
    if SameText(T, 'Insert') then K := VK_INSERT else
    if SameText(T, 'Back') then K := VK_BACK else
    if SameText(T, 'BackSpace') then K := VK_BACK else
    if SameText(T, 'Del') then K := VK_DELETE else
    if SameText(T, 'Delete') then K := VK_DELETE else
    if SameText(T, 'Home') then K := VK_HOME else
    if SameText(T, 'End') then K := VK_END else
    if SameText(T, 'Up') then K := VK_UP else
    if SameText(T, 'Down') then K := VK_DOWN else
    if SameText(T, 'Left') then K := VK_LEFT else
    if SameText(T, 'Right') then K := VK_RIGHT else
    if SameText(T, 'PageUp') then K := VK_PRIOR else
    if SameText(T, 'PageDown') then K := VK_NEXT;
  Result := ShortCut(K, S);
end;

{ TLyKeyShift }

procedure TLyKeyShift.Enter(AOwner: TLyKeyDown);
begin
  Leave;
  FOwner := AOwner;
  FNext := FOwner.FFirst;
  FOwner.FFirst := Self;
end;

procedure TLyKeyShift.Leave;
var
  S: TLyKeyShift;
begin
  if (FOwner <> nil) and (FOwner.FFirst <> nil) then
    if Self <> FOwner.FFirst then
    begin
      S := FOwner.FFirst;
      while (S <> nil) and (S.FNext <> Self) do S := S.FNext;
      if S <> nil then
        S.FNext := FNext;
    end
    else FOwner.FFirst := FNext;
  FNext := nil;
  FOwner := nil;
end;

constructor TLyKeyShift.Create(AOwner: TLyKeyDown);
begin
  Enter(AOwner);
end;

destructor TLyKeyShift.Destroy;
begin
  Leave;
  inherited;
end;

{ TLyPalette }

constructor TLyPalette.Create;
begin
  FTextColor          := COLOR_TEXT;
  FBackground         := COLOR_BACKGROUND;
  FSelectedTextColor  := COLOR_SELECTEDTEXT;
  FSelectedBackground := COLOR_SELECTEDBACKGROUND;
  FLeftBarBackground  := clBtnFace;
  FChangedBackground  := clLime;
  FActiveBackground   := clSkyBlue;
  FCaretColor         := FTextColor;
  FLine80Color        := clGray;
  FLineMarkBackground := $437E07;
  { token color }
  FUnknownColor       := clRed;
  FKeywordColor       := clNavy;
  FIDColor            := COLOR_TEXT;
  FSpaceColor         := COLOR_TEXT;
  FOperatorColor      := COLOR_TEXT;
  FDelimiterColor     := COLOR_TEXT;
  FNumberColor        := clBlue;
  FENumberColor       := clRed;
  FCharColor          := clBlue;
  FECharColor         := clRed;
  FStringColor        := clBlue;
  FEStringColor       := clRed;
  FBoolColor          := COLOR_TEXT;
  FCommentColor       := clGreen;
  FECommentColor      := clGreen;
  FECommentPColor     := clGreen;
  FDirectiveColor     := clTeal;
  FEDirectiveColor    := clTeal;
  FTagColor           := clTeal; // clBlue;
  FMarkColor          := clPurple; //clBlue;
  FHilightColor       := clBlue;
  FRawDataColor       := clDkGray;
  { token style }
  FKeywordStyle       := [fsBold];
end;

function TLyPalette.Apply(Font: TFont; Token: TLyTokenID; Selected: boolean): boolean;
begin
  Result := (Font <> nil);
  if Result then
  begin
    Font.Style := Font.Style + GetStyle(Token);
    if Selected then
      Font.Color := FSelectedTextColor else
      Font.Color := GetColor(Token);
  end;
end;

function TLyPalette.GetStyle(Token: TLyTokenID): TFontStyles;
begin
  Result := [];
  case Token of
    TK_SPACE     : Result := FSpaceStyle;
    TK_KEYWORD   : Result := FKeywordStyle;
    TK_ID        : Result := FIDStyle;
    TK_OPERATOR  : Result := FOperatorStyle;
    TK_DELIMITER : Result := FDelimiterStyle;
    TK_NUMBER    : Result := FNumberStyle;
    TK_ENUMBER   : Result := FENumberStyle;
    TK_CHAR      : Result := FCharStyle;
    TK_ECHAR     : Result := FECharStyle;
    TK_STRING    : Result := FStringStyle;
    TK_ESTRING   : Result := FEStringStyle;
    TK_BOOL      : Result := FBoolStyle;
    TK_COMMENT   : Result := FCommentStyle;
    TK_ECOMMENT  : Result := FECommentStyle;
    TK_ECOMMENTP : Result := FECommentPStyle;
    TK_DIRECTIVE : Result := FDirectiveStyle;
    TK_EDIRECTIVE: Result := FEDirectiveStyle;
    TK_UNKNOWN   : Result := FUnknownStyle;
    TK_TAG       : Result := FTagStyle;
    TK_MARK      : Result := FMarkStyle;
    TK_HILIGHT   : Result := FHilightStyle;
    TK_RAWDATA   : Result := FRawDataStyle;
  end;
end;

function TLyPalette.GetColor(Token: TLyTokenID): TColor;
begin
  Result := FSpaceColor;
  case Token of
    TK_SPACE     : Result := FSpaceColor;
    TK_KEYWORD   : Result := FKeywordColor;
    TK_ID        : Result := FIDColor;
    TK_OPERATOR  : Result := FOperatorColor;
    TK_DELIMITER : Result := FDelimiterColor;
    TK_NUMBER    : Result := FNumberColor;
    TK_ENUMBER   : Result := FENumberColor;
    TK_CHAR      : Result := FCharColor;
    TK_ECHAR     : Result := FECharColor;
    TK_STRING    : Result := FStringColor;
    TK_ESTRING   : Result := FEStringColor;
    TK_BOOL      : Result := FBoolColor;
    TK_COMMENT   : Result := FCommentColor;
    TK_ECOMMENT  : Result := FECommentColor;
    TK_ECOMMENTP : Result := FECommentPColor;
    TK_DIRECTIVE : Result := FDirectiveColor;
    TK_EDIRECTIVE: Result := FEDirectiveColor;
    TK_UNKNOWN   : Result := FUnknownColor;
    TK_TAG       : Result := FTagColor;
    TK_MARK      : Result := FMarkColor;
    TK_HILIGHT   : Result := FHilightColor;
    TK_RAWDATA   : Result := FRawDataColor;
  end;
end;

{ TLyKeyDown }

constructor TLyKeyDown.Create(AOwner: TObject);
begin
  FOwner := AOwner;
end;

destructor TLyKeyDown.Destroy;
begin
  while FFirst <> nil do
    FFirst.Free;
  inherited;
end;

function TLyKeyDown.Add(Key: Word; Shift: TShiftState; Handler: TLyKeyEvent): TLyKeyShift;
begin
  Result := nil;
  if Assigned(Handler) then
  begin
    Result := Find(Key, Shift);
    if Result = nil then
    begin
      Result := TLyKeyShift.Create(Self);
      Result.FKey := Key;
      Result.FShift := Shift;
    end;
    Result.FHandler := Handler;
  end;
end;

function TLyKeyDown.Find(Key: Word; Shift: TShiftState): TLyKeyShift;
begin
  Result := FFirst;
  while Result <> nil do
  begin
    if Key = Result.FKey then
      if Shift = Result.FShift then
        Exit;
    Result := Result.FNext;
  end;
end;

function TLyKeyDown.Process(Sender: TObject; Key: Word; Shift: TShiftState): boolean;
var
  K: TLyKeyShift;
begin
  K := Find(Key, Shift);
  Result := (K <> nil);
  if Result then
    K.FHandler(Sender, Shift);
end;

end.
