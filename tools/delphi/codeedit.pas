{-------------------------------------------------------------------------------
 codeedit.pas
 ============
 Implements the liteweight and syntax hilighted UNICODE editor: TCodeEdit.
 Create by Li Yunjie<718956073@qq.com> on Jan 26th, 2017.
 Last modified on Feb 27th, 2017.
 Hosted at https://github.com/lysee/TCodeEdit.git.
 Released on Feb 11th, 2017 under the MIT license:
 =================================================
 Copyright (c) 2017 Li Yunjie<718956073@qq.com>.

 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all
 copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.
-------------------------------------------------------------------------------}
unit codeedit;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  {$IFDEF FPC}LCLType, LCLIntf,{$ELSE}UITypes,{$ENDIF}
  SysUtils, Classes, Messages, Controls, StdCtrls, ExtCtrls, Graphics, Menus;

const

  CE_VERSION    = '0.7.0';
  CE_FONTNAME   = 'Courier New';
  CE_FONTSIZE   = 10;
  CE_MINTABSIZE = 2;
  CE_MAXTABSIZE = 16;
  CE_DEFTABSIZE = 4;
  CE_LBDELTA    = 5;
  CE_HOMEPOS    = 1;

  { tokens }

  TK_UNKNOWN    =  0;
  TK_KEYWORD    =  1;
  TK_ID         =  2;
  TK_SPACE      =  3;
  TK_OPERATOR   =  4;
  TK_DELIMITER  =  5;
  TK_NUMBER     =  6; // $... or [0-9]...
  TK_ENUMBER    =  7;
  TK_CHAR       =  8; // #... or #$... or '.'
  TK_ECHAR      =  9;
  TK_STRING     = 10; // "..." or '...'
  TK_ESTRING    = 11;
  TK_COMMENT    = 12; // {...} or (*...*) or /*...*/ or //...
  TK_ECOMMENT   = 13; // {...  or /*...
  TK_ECOMMENTP  = 14; // (*...
  TK_DIRECTIVE  = 15; // {$...}
  TK_EDIRECTIVE = 16; // {$...
  TK_CUSTOM     = 20; // start your tokens here

  { charsets }

  CS_PRINT      = ['!'..'~'];
  CS_DIGIT      = ['0'..'9'];
  CS_UPPER      = ['A'..'Z'];
  CS_LOWER      = ['a'..'z'];
  CS_ALPHA      = CS_UPPER + CS_LOWER;
  CS_ALNUM      = CS_ALPHA + CS_DIGIT;
  CS_HEAD       = CS_ALPHA + ['_'];
  CS_ID         = CS_ALNUM + ['_'];
  CS_QUOTE      = ['"', ''''];
  CS_DELIMITER  = ['(', ')', '[', ']', '{', '}', '.', ',', ':', ';'];
  CS_OPERATOR   = CS_PRINT - CS_ID - CS_QUOTE - CS_DELIMITER;
  CS_SPACE      = [#1..' '];
  CS_HEX        = ['A'..'F', 'a'..'f'] + CS_DIGIT;

type

  TLyCodeEdit    = class;{forward}
  TLyCaret       = class;
  TLySelection   = class;
  TLyContextMenu = class;
  TLyLeftBar     = class;
  TLyLine        = class;
  TLyLineList    = class;
  TLyUpdate      = class;
  TLyUndoItem    = class;
  TLyUndoList    = class;
  TLyPalette     = class;
  TLySyntax      = class;

  TLySyntaxClass = class of TLySyntax;
  TLyCodeString  = {$IFDEF UNICODE}string{$ELSE}UnicodeString{$ENDIF};
  TLyCodeChar    = {$IFDEF UNICODE}char{$ELSE}UnicodeChar{$ENDIF};
  PLyCodeChar    = {$IFDEF UNICODE}pchar{$ELSE}PUnicodeChar{$ENDIF};
  TLyChangeType  = (ctChangeText, ctDeleteLine, ctAddLine);
  TLyChangeDeal  = (cdNone, cdUndo, cdRedo);
  TLyToken       = smallint;
  TLyLineMark    = (lmNone, lm0, lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, lm9);

  { TLyCodeEdit }

  TLyCodeEdit = class(TCustomControl)
  private
    FUpdate: TLyUpdate;
    FLines: TLyLineList;
    FUndos: TLyUndoList;
    FRedos: TLyUndoList;
    FCaret: TLyCaret;
    FSelection: TLySelection;
    FPalette: TLyPalette;
    FSyntax: TLySyntax;
    FContextMenu: TLyContextMenu;
    FLeftBar: TLyLeftBar;
    FVScrollBar: TScrollBar;
    FVScrollBox: TPanel;
    FHScrollBar: TScrollBar;
    FHScrollBox: TPanel;
    FReadonly: boolean;
    FModified: boolean;
    FTabSize: integer;
    FLineHeight: integer;
    FSpaceWidth: integer;
    FVisibleLines: integer;
    FLine80Pos: integer;
    FWheelTime: TDateTime;
    FAutoFileSyntax: boolean;
    FOnStatus: TNotifyEvent;
    procedure SetPalette(Value: TLyPalette);
    procedure SetTabSize(Value: integer);
    procedure SetModified(Value: boolean);
    procedure SetReadonly(Value: boolean);
    function GetContextMenu: TLyContextMenu;
  protected
    function GetLineAt(Y: integer): TLyLine;
    function GetCaretPosAt(X, Y: integer; var Line: TLyLine): integer;
    function InEditAreaX(X: integer): boolean;
    function InEditAreaY(Y: integer): boolean;
    function InEditArea(X, Y: integer): boolean;
    function LeftMargin: integer;
    function LineLeft: integer;
    function EditHeight: integer;
    function EditWidth: integer;
    function StatusOK: boolean;
  protected
    {$IFDEF FPC}
    procedure UTF8KeyPress(var Key: TUTF8Char);override;
    {$ELSE}
    procedure KeyPress(var Key: char);override;
    {$ENDIF}
    procedure KeyDown(var Key: Word; Shift: TShiftState);override;
    procedure KeyUp(var Key: Word; Shift: TShiftState);override;
    procedure PressKey(Key: TLyCodeChar);
    procedure PressHome(Shift: TShiftState);
    procedure PressEnd(Shift: TShiftState);
    procedure PressArrowLeft(Shift: TShiftState);
    procedure PressArrowRight(Shift: TShiftState);
    procedure PressArrowUp(Shift: TShiftState);
    procedure PressArrowDown(Shift: TShiftState);
    procedure PressPageUp(Shift: TShiftState);
    procedure PressPageDown(Shift: TShiftState);
    procedure PressBackSpace(Shift: TShiftState);
    procedure PressInsert(Shift: TShiftState);
    procedure PressDelete(Shift: TShiftState);
    procedure PressSetLineMark(AMark: TLyLineMark);
    procedure PressGoto(AMark: TLyLineMark);
    procedure PressDeleteLine;
    procedure PressSelectAll;
    procedure PressCopyToClipboard;
    procedure PressPasteFromClipboard;
    procedure PressCutToClipboard;
    procedure PressUndo;
    procedure PressRedo;
  protected
    procedure FontChange(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);
    procedure WMSetFocus(var Msg: TWmSetFocus);message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWmKillFocus);message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Msg: TWmGetDlgCode);message WM_GETDLGCODE;
    procedure CreateParams(var Params: TCreateParams);override;
    procedure Resize;override;
    procedure Paint;override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean);override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);override;
    function DoMouseWheelDown(Shift: TShiftState; XY: TPoint): boolean;override;
    function DoMouseWheelUp(Shift: TShiftState; XY: TPoint): boolean;override;
    procedure PaintLine80(Y: integer = 0; SingleLine: boolean = false);
    procedure PaintVisibleLines(OnlyNeeded: boolean; DoPaint: boolean = false);
    procedure MakeVisible(LineX, TextX: integer);
    procedure EnsureNotEmpty;
    procedure NotifyStatus;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    function FindNext(const AText: TLyCodeString): boolean;
    property Lines: TLyLineList read FLines;
    property Undos: TLyUndoList read FUndos;
    property Redos: TLyUndoList read FRedos;
    property Caret: TLyCaret read FCaret;
    property Selection: TLySelection read FSelection;
    property Palette: TLyPalette read FPalette write SetPalette;
    property Syntax: TLySyntax read FSyntax;
    property LeftBar: TLyLeftBar read FLeftBar;
    property ContextMenu: TLyContextMenu read GetContextMenu;
    property TabSize: integer read FTabSize write SetTabSize;
    property Readonly: boolean read FReadonly write SetReadonly;
    property Modified: boolean read FModified write SetModified;
    property AutoFileSyntax: boolean read FAutoFileSyntax write FAutoFileSyntax;
    property OnStatus: TNotifyEvent read FOnStatus write FOnStatus;
  end;

  { TLyUpdate }

  TLyUpdate = class
  private
    FEdit: TLyCodeEdit;
    FUpdateCount: integer;
    FEndUpdating: boolean;
    FChangedLineX: integer;
    FChangedLines: integer;
    FAddedLines: integer;
    FDeletedLines: integer;
    FVScrollPos: integer;
    FHScrollPos: integer;
    FUndoMode: TLyChangeDeal;
    function AllowPaint: boolean;
  protected
    function StatusOK: boolean;
    procedure Change(LineX: integer; AType: TLyChangeType = ctChangeText);
    procedure SaveChange(Line: integer; const S: TLyCodeString);
    procedure SaveDelete(Line: integer; const S: TLyCodeString);
    procedure SaveAdd(Line: integer);
  public
    constructor Create(AEdit: TLyCodeEdit);
    procedure BeginUpdate;
    procedure EndUpdate;
  end;

  { TLyCaret }

  TLyCaret = class
  private
    FEdit: TLyCodeEdit;
    FLineX: integer;
    FTextX: integer;
    FWidth: integer;
    FHeight: integer;
    FPos: TPoint;
    FMoveCount: integer;
    FCreated: boolean;
    FShowing: boolean;
    function GetActive: boolean;//
    function GetLine: TLyLine;//
  protected
    procedure BeginMove;
    procedure EndMove;
    procedure DestroyCaret;
    procedure HideCaret;
    function CreateCaret(Width, Height: integer): boolean;
    function ShowCaret: boolean;
    procedure MoveCaret;
    procedure Insert(Strings: TStrings);overload;
    procedure Insert(const Text: TLyCodeString);overload;
    function EditLine: TLyLine;
    function InEditArea: boolean;
  public
    constructor Create(AMemo: TLyCodeEdit);
    procedure MakeVisible;
    procedure MoveTo(ItemX, TextX: integer);
    procedure MoveToHead(Item: TLyLine);
    procedure MoveToTail(Item: TLyLine);
    procedure MoveToSelEnd;
    procedure MoveAfterInsert(Line: TLyLine; TextX: integer; const S: TLyCodeString);
    property Active: boolean read GetActive;
    property Showing: boolean read FShowing;
    property Line: TLyLine read GetLine;
    property LineIndex: integer read FLineX;
    property TextIndex: integer read FTextX;
    property Pos: TPoint read FPos;
    property Width: integer read FWidth;//
    property Height: integer read FHeight;//
  end;

  { TLySelection }

  TLySelection = class
  private
    FEdit: TLyCodeEdit;
    FLineX: integer;
    FTextX: integer;
    FStartLine: integer;
    FStartText: integer;
    FEndLine: integer;
    FEndText: integer;
    FSelecting: boolean;
    function GetText: TLyCodeString;
    procedure SetText(const S: TLyCodeString);
    function GetStartLine: TLyLine;
    function GetEndLine: TLyLine;
    function GetSelected: boolean;
    function GetSelectedAll: boolean;
    function GetSelectedPart: boolean;
    function GetSelectedLineCount: integer;
  public
    constructor Create(AEdit: TLyCodeEdit);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure PasteFromClipboard;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure Delete;
    procedure Select(LineX, TextX, EndLineX, EndTextX: integer);
    procedure SelectTo(LineX, TextX: integer);
    procedure SelectAll;
    procedure Unselect;
    procedure Paint;
    function IsStartLine(LineX: integer): boolean;
    function IsEndLine(LineX: integer): boolean;
    function HasLine(LineX: integer): boolean;
    function HasText(LineX, TextX: integer): boolean;
    function GetSelectedTextRange(LineX: integer; var StartX, EndX: integer): boolean;
    property Selected: boolean read GetSelected;
    property SelectedAll: boolean read GetSelectedAll;
    property SelectedPart: boolean read GetSelectedPart;
    property SelectedLineCount: integer read GetSelectedLineCount;
    property Text: TLyCodeString read GetText write SetText;
    property StartLineIndex: integer read FStartLine;
    property StartTextIndex: integer read FStartText;
    property StartLine: TLyLine read GetStartLine;
    property EndLineIndex: integer read FEndLine;
    property EndTextIndex: integer read FEndText;
    property EndLine: TLyLine read GetEndLine;
  end;

  { TLyContextMenu }

  TLyContextMenu = class(TPopupMenu)
  private
    FEdit: TLyCodeEdit;
    FLine: TLyLine;
    FUndo: TMenuItem;
    FRedo: TMenuItem;
    FCut: TMenuItem;
    FCopy: TMenuItem;
    FPaste: TMenuItem;
    FSelectAll: TMenuItem;
    FGotoMark: TMenuItem;
    FToggleMark: TMenuItem;
    FGotoMenus: array[lm0..lm9] of TMenuItem;
    FToggleMenus: array[lm0..lm9] of TMenuItem;
  protected
    procedure UndoClick(Sender: TObject);
    procedure RedoClick(Sender: TObject);
    procedure CutClick(Sender: TObject);
    procedure CopyClick(Sender: TObject);
    procedure PasteClick(Sender: TObject);
    procedure GotoMark(Sender: TObject);
    procedure ToggleMark(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);override;
    procedure Popup(X, Y: Integer);override;
    procedure UpdateChildMenus;
    function AddChild(const ACaption: string): TMenuItem;overload;
    function AddChild(const ACaption, ShortCut: string): TMenuItem;overload;
    function AddChild(Root: TMenuItem; const ACaption: string): TMenuItem;overload;
    function AddChild(Root: TMenuItem; const ACaption, ShortCut: string): TMenuItem;overload;
  end;

  { TLyLeftBar }

  TLyLeftBar = class(TCustomControl)
  private
    FEdit: TLyCodeEdit;
    FTextHeight: integer;
    function GetTextLeft: integer;
    function GetTextWidth: integer;
    function GetLineHeight: integer;
    function GetVertLinePos: integer;
  protected
    procedure Paint;override;
    procedure PaintLine(LineX: integer);virtual;
    function AdjustWidth: integer;virtual;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    property LineHeight: integer read GetLineHeight;
    property TextLeft: integer read GetTextLeft;
    property TextWidth: integer read GetTextWidth;
    property TextHeight: integer read FTextHeight;
    property VertLinePos: integer read GetVertLinePos;
  end;

  { TLyLine }

  TLyLine = class
  private
    FList: TLyLineList;
    FText: TLyCodeString;
    FTokens: array of TLyToken;
    FWidth: integer;
    FIndex: integer;
    FChanged: boolean;
    FNeedPaint: boolean;
    FLastToken: TLyToken;
    FLineMark: TLyLineMark;
    function GetLeft: integer;
    function GetTop: integer;
    function GetHeight: integer;
    procedure SetText(const Value: TLyCodeString);
    procedure SetChanged(Value: boolean);
    function GetPriorToken: TLyToken;
    procedure UseSelected(Token: TLyToken = TK_SPACE);
    procedure UseNormal(Token: TLyToken = TK_SPACE);
    procedure SetLineMark(AValue: TLyLineMark);
  protected
    procedure Paint(DoPaint: boolean = false);
    function TextWidth(const S: TLyCodeString): integer;
    function TextOut(X, Y: integer; const S: TLyCodeString): integer;
    function TextPos(TextX: integer): TPoint;
    function HitCaret(X: integer): integer;
    function Add(const S: TLyCodeString): integer;
    function Insert(TextX: integer; const S: TLyCodeString): integer;
    function Delete(TextX: integer; Count: integer = 1): integer;
    function CutFrom(TextX: integer): TLyCodeString;
    function BreakFrom(TextX: integer): TLyLine;
    function MergeNext: boolean;
    function Editor: TLyCodeEdit;
    function Canvas: TCanvas;
    function Selection: TLySelection;
    function Caret: TLyCaret;
    function Prior: TLyLine;
    function Next: TLyLine;
    function LeadingsSpaces: TLyCodeString;
    function SeekPriorWord(TextX: integer): integer;
    function SeekNextWord(TextX: integer): integer;
    function CharToken(TextX: integer): TLyToken;
    function PeekChar(TextX: integer): TLyCodeChar;
  public
    destructor Destroy;override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Change;
    function Pack: boolean;
    function Selected(TextX: integer): boolean;overload;
    function Selected: boolean;overload;
    function EndPos: integer;
    procedure ToggleLineMark(AMark: TLyLineMark);
    property Text: TLyCodeString read FText write SetText;
    property Left: integer read GetLeft;
    property Top: integer read GetTop;
    property Width: integer read FWidth;
    property Height: integer read GetHeight;
    property Index: integer read FIndex;
    property Changed: boolean read FChanged write SetChanged;
    property PriorToken: TLyToken read GetPriorToken;
    property LastToken: TLyToken read FLastToken;
    property LineMark: TLyLineMark read FLineMark write SetLineMark;
  end;

  { TLyLineList }

  TLyLineList = class
  private
    FEdit: TLyCodeEdit;
    FItems: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TLyLine;
    function GetFirst: TLyLine;
    function GetLast: TLyLine;
    function GetText: TLyCodeString;
    procedure SetText(const Value: TLyCodeString);
  protected
    function Has(LineX: integer): boolean;overload;
    function Has(LineX1, LineX2: integer): boolean;overload;
    function HasInOrder(StartX, EndX: integer): boolean;
    function TryGetLine(LineX: integer; var Line: TLyLine): boolean;
    function GetStrings: TStrings;
    procedure ResetIndex(StartX: integer = 0);
    procedure DecorateFrom(StartX: integer = 0);
    procedure SetNeedPaint(Value: boolean; StartX: integer = 0);
  public
    constructor Create(AEdit: TLyCodeEdit);
    destructor Destroy;override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure AssignStrings(Strings: TStrings);
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure LoadFromStrings(Strings: TStrings);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToStrings(Strings: TStrings);
    procedure Clear;
    procedure ClearToOneEmptyLine;
    procedure Delete(Index: integer);overload;
    procedure Delete(BegIndex, EndIndex: integer);overload;
    function Add(const S: TLyCodeString = ''): TLyLine;
    function Insert(X: integer; const S: TLyCodeString = ''): TLyLine;
    function InsertBefore(L: TLyLine; const S: TLyCodeString = ''): TLyLine;
    function InsertAfter(L: TLyLine; const S: TLyCodeString = ''): TLyLine;
    function Width: integer;
    function Height: integer;
    function FirstChanged: integer;
    function Pack: integer;
    function FindByMark(AMark: TLyLineMark): TLyLine;
    procedure ClearLineMark;
    property Count: integer read GetCount;
    property Items[Index: integer]: TLyLine read GetItem;default;
    property Text: TLyCodeString read GetText write SetText;
    property First: TLyLine read GetFirst;
    property Last: TLyLine read GetLast;
  end;

  { TLyUndoItem }

  TLyUndoItem = class
  private
    FList: TLyUndoList;
    FPrev: TLyUndoItem;
    FType: TLyChangeType;
    FLine: integer;
    FText: TLyCodeString;
    FMagic: cardinal;
    FCaret: TPoint;
    FFrom: TPoint;
    FStart: TPoint;
    FEnd: TPoint;
    procedure Apply;
  public
    constructor Create(AList: TLyUndoList);
    destructor Destroy;override;
    property Prev: TLyUndoItem read FPrev;
    property Line: integer read FLine;
    property Text: TLyCodeString read FText;
    property ChangeType: TLyChangeType read FType;
    property Magic: cardinal read FMagic;
  end;

  { TLyUndoList }

  TLyUndoList = class
  private
    FEdit: TLyCodeEdit;
    FLast: TLyUndoItem;
    FMagic: cardinal;
    FEnabled: boolean;
    function GetMode: TLyChangeDeal;
    procedure SetEnabled(Value: boolean);
  public
    constructor Create(AEdit: TLyCodeEdit);
    destructor Destroy;override;
    procedure Clear;
    procedure Apply;
    property Mode: TLyChangeDeal read GetMode;
    property Last: TLyUndoItem read FLast;
    property Enabled: boolean read FEnabled write SetEnabled;
  end;

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
    FCommentColor: TColor;
    FECommentColor: TColor;
    FECommentPColor: TColor;
    FDirectiveColor: TColor;
    FEDirectiveColor: TColor;
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
    FCommentStyle: TFontStyles;
    FECommentStyle: TFontStyles;
    FECommentPStyle: TFontStyles;
    FDirectiveStyle: TFontStyles;
    FEDirectiveStyle: TFontStyles;
  public
    constructor Create;virtual;
    function GetStyle(Token: TLyToken): TFontStyles;virtual;
    function GetColor(Token: TLyToken): TColor;virtual;
    property TextColor: TColor read FTextColor write FTextColor;
    property Background: TColor read FBackground write FBackground;
    property SelectedTextColor: TColor read FSelectedTextColor write FSelectedTextColor;
    property SelectedBackground: TColor read FSelectedBackground write FSelectedBackground;
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
    property CommentColor: TColor read FCommentColor write FCommentColor;
    property ECommentColor: TColor read FECommentColor write FECommentColor;
    property ECommentPColor: TColor read FECommentPColor write FECommentPColor;
    property DirectiveColor: TColor read FDirectiveColor write FDirectiveColor;
    property EDirectiveColor: TColor read FEDirectiveColor write FEDirectiveColor;
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
    property CommentStyle: TFontStyles read FCommentStyle write FCommentStyle;
    property ECommentStyle: TFontStyles read FECommentStyle write FECommentStyle;
    property ECommentPStyle: TFontStyles read FECommentPStyle write FECommentPStyle;
    property DirectiveStyle: TFontStyles read FDirectiveStyle write FDirectiveStyle;
    property EDirectiveStyle: TFontStyles read FEDirectiveStyle write FEDirectiveStyle;
  end;

  { TLySyntax }

  TLySyntax = class
  private
    FEdit: TLyCodeEdit;
    FLine: TLyCodeString;
    FSize: integer;
    FIndex: integer;
    FChar: TLyCodeChar;
    FTokenPos: integer;
    FPrior: TLyToken;
    function GetTokenSize: integer;
    function GetTokenStr: TLyCodeString;
    function GetToken(Index: integer): TLyToken;
    function GetClass: TLySyntaxClass;
    procedure SetClass(Value: TLySyntaxClass);
  protected
    function PeekChar: TLyCodeChar;
    function GetNextChar: boolean;
    function MatchChar(C: TLyCodeChar): boolean;overload;
    function MatchChar(Chars: TSysCharSet): boolean;overload;
    function MatchStr(const S: TLyCodeString): boolean;
    function MatchText(const S: TLyCodeString): boolean;
    function MatchPrior(AToken: TLyToken): boolean;
    function SkipChars(Chars: TSysCharSet): integer;
    function SkipNextChars(Chars: TSysCharSet): integer;
    function ParseToEnd(Token: TLyToken): TLyToken;
    function ParseOne(Token: TLyToken): TLyToken;
    function ParseTo(const EndStr: TLyCodeString; Token, FailToken: TLyToken): TLyToken;
    function ParseBlockComment(const EndStr: TLyCodeString; FailToken: TLyToken): TLyToken;
    function ParseLineComment: TLyToken;
  public
    constructor Create(AEdit: TLyCodeEdit);virtual;
    destructor Destroy;override;
    class function Language: string;virtual;
    class function FileExts: string;virtual;
    class function DefFileExt: string;virtual;
    class function Support(const FileExt: string): boolean;virtual;
    class function CaseSensitive: boolean;virtual;
    function Decorate(ALine: TLyLine): TLyToken;virtual;
    function ParseNextToken: TLyToken;virtual;
    function GetTokenColor(Token: TLyToken): TColor;virtual;
    function GetTokenStyle(Token: TLyToken): TFontStyles;virtual;
    function IsKeyword(const ID: TLyCodeString): boolean;virtual;
    function MatchID(const ID1, ID2: TLyCodeString): boolean;overload;
    function MatchID(const ID: TLyCodeString;
      const IDList: array of TLyCodeString): boolean;overload;
    property Line: TLyCodeString read FLine;
    property Curr: TLyCodeChar read FChar;
    property Size: integer read FSize;
    property Tokens[TextX: integer]: TLyToken read GetToken;default;
    property Prior: TLyToken read FPrior write FPrior;
    property TokenPos: integer read FTokenPos;
    property TokenSize: integer read GetTokenSize;
    property TokenStr: TLyCodeString read GetTokenStr;
    property SyntaxClass: TLySyntaxClass read GetClass write SetClass;
  end;

  { TPascalSyntax }

  TPascalSyntax = class(TLySyntax)
  protected
    function ParseDirective: TLyToken;virtual;
    function ParseComment: TLyToken;virtual;
    function ParseCommentP: TLyToken;virtual;
    function ParseSpace: TLyToken;virtual;
    function ParseString: TLyToken;virtual;
    function ParseChar: TLyToken;virtual;
    function ParseNumber: TLyToken;virtual;
    function ParseHex: TLyToken;virtual;
    function ParseKeywordID: TLyToken;virtual;
    function ParseDelimiter: TLyToken;virtual;
    function ParseOperator: TLyToken;virtual;
    function ParseUnknown: TLyToken;virtual;
  public
    class function Language: string;override;
    class function FileExts: string;override;
    class function DefFileExt: string;override;
    function ParseNextToken: TLyToken;override;
    function IsKeyword(const ID: TLyCodeString): boolean;override;
  end;

  { TLyseeSyntax }

  TLyseeSyntax = class(TPascalSyntax)
  protected
    function ParseString: TLyToken;override;
    function ParseStringL: TLyToken;
  public
    class function Language: string;override;
    class function FileExts: string;override;
    class function DefFileExt: string;override;
    function ParseNextToken: TLyToken;override;
    function GetTokenColor(Token: TLyToken): TColor;override;
    function IsKeyword(const ID: TLyCodeString): boolean;override;
  end;

function Palette: TLyPalette;
function SyntaxClasses: TList;
function FindSyntax(const Language: string): TLySyntaxClass;
function FindSyntaxByFileExt(const FileExt: string): TLySyntaxClass;
function FindFileSyntax(const FileName: string): TLySyntaxClass;
function PlaceACodeEdit(Host: TWinControl): TLyCodeEdit;overload;
function PlaceACodeEdit(Host: TWinControl; Syntax: TLySyntaxClass): TLyCodeEdit;overload;
function PlaceACodeEdit(Host: TWinControl; const LangOrFileExt: string): TLyCodeEdit;overload;
function MatchFileExt(const X, FileExt: string): boolean;overload;
function MatchFileExt(const X: string; const FileExts: array of string): boolean;overload;
function FetchFileExt(var FileExts: string): string;
function SplitFileExts(const FileExts: string): TStrings;
function TabSpaces(TextX, TabSize: integer): TLyCodeString;
function HasTextFormat: boolean;
function IsUTF8(const Str: string): boolean;
function CodeToStr(const Str: TLyCodeString): string; // UNICODE or UTF8
function StrToCode(const Str: string): TLyCodeString;
function WideToStr(const Str: WideString): string;
function StrToWide(const Str: string): WideString;
function StrToShortCut(const Str: string): TShortCut;
function PosEx(const SubStr, Str: TLyCodeString; Index: integer = 1): integer;
function ScreenWidth(MM: currency): integer;
function ScreenHeight(MM: currency): integer;
function ComparePos(LineX1, TextX1, LineX2, TextX2: integer): integer;

implementation

uses
  Forms, Types, Math, DateUtils, Clipbrd, basic;

var
  my_syntaxes: TList;
  my_palette: TLyPalette;

const

  { pascal }

  PascalKeywords: array[0..110] of TLyCodeString = (
    'absolute', 'abstract', 'and', 'array', 'as', 'asm', 'assembler',
    'automated', 'begin', 'case', 'cdecl', 'class', 'const', 'constructor',
    'contains', 'default', 'deprecated', 'destructor', 'dispid',
    'dispinterface', 'div', 'do', 'downto', 'dynamic', 'else', 'end', 'except',
    'export', 'exports', 'external', 'far', 'file', 'final', 'finalization',
    'finally', 'for', 'forward', 'function', 'goto', 'helper', 'if',
    'implementation', 'implements', 'in', 'index', 'inherited',
    'initialization', 'inline', 'interface', 'is', 'label', 'library',
    'message', 'mod', 'name', 'near', 'nil', 'nodefault', 'not', 'object', 'of',
    'on', 'operator', 'or', 'out', 'overload', 'override', 'package', 'packed',
    'pascal', 'platform', 'private', 'procedure', 'program', 'property',
    'protected', 'public', 'published', 'raise', 'read', 'readonly', 'record',
    'register', 'reintroduce', 'repeat', 'requires', 'resourcestring',
    'safecall', 'sealed', 'set', 'shl', 'shr', 'stdcall', 'stored', 'string',
    'stringresource', 'then', 'threadvar', 'to', 'try', 'type', 'unit', 'until',
    'uses', 'var', 'virtual', 'while', 'with', 'write', 'writeonly', 'xor');

  { lysee }

  TK_ESTRINGL = TK_CUSTOM;

  LyseeKeywords: array[0..50] of TLyCodeString = (
    'and', 'array', 'as', 'begin', 'boolean', 'case', 'class', 'const', 'def',
    'div', 'do', 'downto', 'elif', 'else', 'end', 'except', 'false', 'finally',
    'for', 'get', 'if', 'in', 'inherited', 'is', 'like', 'main', 'mod', 'nil',
    'not', 'object', 'of', 'or', 'raise', 'repeat', 'result', 'set', 'shl',
    'shr', 'string', 'system', 'then', 'to', 'true', 'try', 'type', 'until',
    'uses', 'var', 'variant', 'while', 'xor');

//------------------------------------------------------------------------------

function Palette: TLyPalette;
begin
  if my_palette = nil then
    my_palette := TLyPalette.Create;
  Result := my_palette;
end;

function SyntaxClasses: TList;
begin
  if my_syntaxes = nil then
    my_syntaxes := TList.Create;
  Result := my_syntaxes;
end;

function FindSyntax(const Language: string): TLySyntaxClass;
var
  I: integer;
begin
  for I := 0 to SyntaxClasses.Count - 1 do
  begin
    Result := TLySyntaxClass(SyntaxClasses[I]);
    if SameText(Language, Result.Language) then Exit;
  end;
  Result := nil;
end;

function FindSyntaxByFileExt(const FileExt: string): TLySyntaxClass;
var
  I: integer;
begin
  for I := 0 to SyntaxClasses.Count - 1 do
  begin
    Result := TLySyntaxClass(SyntaxClasses[I]);
    if Result.Support(FileExt) then Exit;
  end;
  Result := nil;
end;

function FindFileSyntax(const FileName: string): TLySyntaxClass;
begin
  Result := FindSyntaxByFileExt(ExtractFileExt(FileName));
end;

function PlaceACodeEdit(Host: TWinControl): TLyCodeEdit;
begin
  Result := TLyCodeEdit.Create(Host);
  Result.Parent := Host;
  Result.Align := alClient;
  Result.Visible := true;
end;

function PlaceACodeEdit(Host: TWinControl; Syntax: TLySyntaxClass): TLyCodeEdit;
begin
  Result := PlaceACodeEdit(Host);
  if Syntax <> nil then
    Result.Syntax.SyntaxClass := Syntax;
end;

function PlaceACodeEdit(Host: TWinControl; const LangOrFileExt: string): TLyCodeEdit;
begin
  if (LangOrFileExt <> '') and (LangOrFileExt[1] = '.') then
    Result := PlaceACodeEdit(Host, FindSyntaxByFileExt(LangOrFileExt)) else
    Result := PlaceACodeEdit(Host, FindSyntax(LangOrFileExt));
end;

function MatchFileExt(const X, FileExt: string): boolean;
begin
  Result := SameText(X, FileExt);
end;

function MatchFileExt(const X: string; const FileExts: array of string): boolean;overload;
var
  I: integer;
begin
  for I := 0 to Length(FileExts) - 1 do
    if MatchFileExt(X, FileExts[I]) then
    begin
      Result := true;
      Exit;
    end;
  Result := false;
end;

function FetchFileExt(var FileExts: string): string;
const
  E = [#1..' ', '.', ',', ';', '|', '/', '\', ':'];
var
  I, X, L: integer;
begin
  repeat
    L := Length(FileExts);
    I := 1;
    while (I <= L) and (FileExts[I] <> '.') do Inc(I);
    if I <= L then
    begin
      X := I + 1;
      while (X <= L) and not CharInSet(FileExts[X], E) do Inc(X);
      Result := Copy(FileExts, I, X - I);
      if Result = '.' then Result := '';
      FileExts := Trim(Copy(FileExts, X, L));
    end
    else
    begin
      Result := '';
      FileExts := '';
    end;
  until (Result <> '') or (FileExts = '');
end;

function SplitFileExts(const FileExts: string): TStrings;
var
  S, T: string;
begin
  Result := TStringList.Create;
  S := FileExts;
  T := FetchFileExt(S);
  while T <> '' do
  begin
    Result.Add(T);
    T := FetchFileExt(S);
  end;
end;

function TabSpaces(TextX, TabSize: integer): TLyCodeString;
var
  I, N: integer;
begin
  I := Max(0, TextX - 1);
  N := (I div TabSize + 1) * TabSize - I;
  SetLength(Result, N);
  for I := 1 to N do
    Result[I] := ' ';
end;

function HasTextFormat: boolean;
begin
  Result := Clipboard.HasFormat(CF_TEXT)
    {$IFNDEF FPC}
    or Clipboard.HasFormat(CF_OEMTEXT)
    {$ENDIF};
end;

function IsUTF8(const Str: string): boolean;
{$IFNDEF UNICODE}
var
  S: pchar;
  I, L, rest: integer;
  B: byte;
  ansi: boolean;
{$ENDIF}
begin
  Result := false;
  {$IFNDEF UNICODE}
  L := Length(Str);
  S := pchar(Str);
  ansi := true;
  rest := 0;
  for I := 0 to L - 1 do
  begin
    B := Ord(S^);
    if rest > 0 then // check following rest: 10XXXXXX
    begin
      if (B and $C0) <> $80 then Exit;
      Dec(rest);
    end
    else
    if B >= $80 then // head byte: 1XXXXXXX
    begin
      if (B >= $FC) and (B <= $FD) then rest := 5 else
      if (B >= $F8) then rest := 4 else
      if (B >= $F0) then rest := 3 else
      if (B >= $E0) then rest := 2 else
      if (B >= $C0) then rest := 1 else Exit;
      ansi := false;
    end;
    Inc(S);
  end;
  Result := not ansi and (rest = 0);
  {$ENDIF}
end;

function CodeToStr(const Str: TLyCodeString): string;
begin
  {$IF DEFINED(UNICODE) OR NOT DEFINED(FPC)}
  Result := Str;
  {$ELSE}
  Result := UTF8Encode(Str);
  {$ENDIF};
end;

function StrToCode(const Str: string): TLyCodeString;
begin
  {$IF DEFINED(UNICODE) OR NOT DEFINED(FPC)}
  Result := Str;
  {$ELSE}
  if IsUTF8(Str) then
    Result := UTF8Decode(Str) else
    Result := StrToWide(Str);
  {$ENDIF};
end;

function WideToStr(const Str: WideString): string;
{$IF NOT DEFINED(UNICODE) AND DEFINED(MSWINDOWS)}
var
  L: integer;
{$ENDIF}
begin
  {$IF DEFINED(UNICODE) OR NOT DEFINED(MSWINDOWS)}
  Result := string(Str);
  {$ELSE}
  L := Length(Str)* 2 + 1;
  SetLength(Result, L);
  L := WideCharToMultiByte(CP_ACP, 0, PWideChar(Str), -1, PChar(Result), L, nil, nil);
  SetLength(Result, L - 1);
  {$ENDIF}
end;

function StrToWide(const Str: string): WideString;
{$IF NOT DEFINED(UNICODE) AND DEFINED(MSWINDOWS)}
var
  L: integer;
{$ENDIF}
begin
  {$IF DEFINED(UNICODE) OR NOT DEFINED(MSWINDOWS)}
  Result := WideString(Str);
  {$ELSE}
  L := Length(Str)+ 1;
  SetLength(Result, L);
  L := MultiByteToWideChar(CP_ACP, 0, PChar(Str), -1, PWideChar(Result), L);
  SetLength(Result, L - 1);
  {$ENDIF}
end;

function StrToShortCut(const Str: string): TShortCut;
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
  while I <= Length(Str) do
  begin
    if not CharInSet(Str[I], CS_ALNUM) then
    begin
      T := Copy(Str, B, I - B);
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
  T := Copy(Str, B, I - B);
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

function PosEx(const SubStr, Str: TLyCodeString; Index: integer): integer;
var
  I, N, L, J: Integer;
  P, B: PLyCodeChar;
begin
  L := Length(SubStr);
  N := Length(Str) - Index - L + 1;
  if (Index > 0) and (N >= 0) and (L > 0) then
  begin
    P := @SubStr[1];
    B := @Str[1];
    Inc(B, Index - 1);
    for I := 0 to N do
    begin
      J := 0;
      while (J >= 0) and (J < L) and (B[I + J] = P[J]) do Inc(J);
      if J >= L then
        Exit(I + Index);
    end;
  end;
  Result := 0;
end;

function ScreenWidth(MM: currency): integer;
begin
  Result := Round((MM * Screen.PixelsPerInch) / 25.4);
end;

function ScreenHeight(MM: currency): integer;
begin
  Result := ScreenWidth(MM);
end;

function ComparePos(LineX1, TextX1, LineX2, TextX2: integer): integer;
begin
  Result := (LineX1 - LineX2);
  if Result = 0 then
    Result := (TextX1 - TextX2);
end;

function IsCtrlShift(S: TShiftState): boolean;
begin
  Result := (S - [ssShift, ssCtrl] = []);
end;

function KeyStateOK: boolean;
begin
  Result := (GetKeyState(VK_CONTROL) >= 0) and
            (GetKeyState(VK_MENU) >= 0) and
            (GetKeyState(VK_BACK) >= 0);
end;

procedure Check(OK: boolean; const ErrorMsg: string);
begin
  if not OK then raise Exception.Create(ErrorMsg);
end;

{ TLyCodeEdit }

constructor TLyCodeEdit.Create(AOwner: TComponent);

  function new_panel(P: TWinControl; A: TAlign): TPanel;
  begin
    Result := TPanel.Create(Self);
    Result.Parent := P;
    Result.Align := A;
    Result.BorderStyle := bsNone;
    Result.BevelOuter := bvNone;
    Result.BevelInner := bvNone;
    Result.Color := clBtnFace;
    Result.Caption := '';
    Result.Visible := true;
  end;

begin
  inherited;
  TabStop := true;
  Cursor := crIBeam;
  DoubleBuffered := true;
  FTabSize := CE_DEFTABSIZE;
  FAutoFileSyntax := true;
  FWheelTime := Now;
  Font.Name := CE_FONTNAME;
  Font.Size := CE_FONTSIZE;
  Font.OnChange := {$IFDEF FPC}@{$ENDIF}FontChange;
  FUpdate := TLyUpdate.Create(Self);
  FLines := TLyLineList.Create(Self);
  FUndos := TLyUndoList.Create(Self);
  FRedos := TLyUndoList.Create(Self);
  FSelection := TLySelection.Create(Self);
  FPalette := codeedit.Palette;
  FSyntax := TPascalSyntax.Create(Self);
  FCaret := TLyCaret.Create(Self);

  { left bar }

  FLeftBar := TLyLeftBar.Create(Self);
  FLeftBar.Parent := Self;
  FLeftBar.FEdit := Self;
  FLeftBar.Align := alLeft;
  FLeftBar.Width := 1;
  FLeftBar.Caption := '';
  FLeftBar.TabStop := false;
  FLeftBar.Visible := true;

  { vertical scroll bar }

  FVScrollBox := new_panel(Self, alRight);
  FVScrollBar := TScrollBar.Create(Self);
  FVScrollBar.Parent := FVScrollBox;
  FVScrollBar.Kind := sbVertical;
  FVScrollBar.Align := alRight;
  FVScrollBar.TabStop := false;
  FVScrollBar.SmallChange := 1;
  FVScrollBar.Visible := true;
  FVScrollBox.Width := FVScrollBar.Width;
  FVScrollBar.Align := alClient;

  { horizontal scroll bar }

  FHScrollBox := new_panel(Self, alBottom);
  FHScrollBar := TScrollBar.Create(Self);
  FHScrollBar.Parent := FHScrollBox;
  FHScrollBar.Kind := sbHorizontal;
  FHScrollBar.Align := alNone;
  FHScrollBar.Left := 0;
  FHScrollBar.Top := 0;
  FHScrollBar.TabStop := false;
  FHScrollBar.SmallChange := 1;
  FHScrollBar.Max := 2048;
  FHScrollBar.Visible := true;
  FHScrollBar.OnChange := {$IFDEF FPC}@{$ENDIF}ScrollBarChange;
  FHScrollBox.Height := FHScrollBar.Height;
  new_panel(FHScrollBox, alRight).Width := FVScrollBox.Width;
  FHScrollBar.Align := alClient;
end;

procedure TLyCodeEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_CLIPCHILDREN;
  {$IFDEF MSWINDOWS}
  Params.ExStyle := Params.ExStyle or WS_EX_COMPOSITED;
  {$ENDIF}
end;

destructor TLyCodeEdit.Destroy;
begin
  Inc(FUpdate.FUpdateCount); // prevent events
  Font.OnChange := nil;
  FVScrollBar.OnChange := nil;
  FVScrollBar.Position := 0;
  FLines.Clear;
  FreeAndNil(FLines);
  FreeAndNil(FCaret);
  FreeAndNil(FSelection);
  FreeAndNil(FUpdate);
  inherited;
end;

procedure TLyCodeEdit.Paint;

  function has_need_paint: boolean;
  var
    I, X: integer;
    L: TLyLine;
  begin
    L := nil;
    X := FVScrollBar.Position;
    for I := 0 to FVisibleLines do
      if not FLines.TryGetLine(X + I, L) then Break else
      if L.FNeedPaint then
      begin
        Result := true;
        Exit;
      end;
    Result := false;
  end;

begin
  PaintVisibleLines(has_need_paint, true);
end;

procedure TLyCodeEdit.PaintVisibleLines(OnlyNeeded: boolean; DoPaint: boolean);
var
  I, X, Y: integer;
  L: TLyLine;
begin
  L := nil;
  X := FVScrollBar.Position;
  if DoPaint or FUpdate.AllowPaint then
  begin
    EnsureNotEmpty;
    Y := 0;
    for I := 0 to FVisibleLines do
    begin
      FLeftBar.PaintLine(X);
      if FLines.TryGetLine(X, L) then
      begin
        if not OnlyNeeded or L.FNeedPaint then L.Paint(DoPaint);
        Inc(Y, FLineHeight);
      end;
      Inc(X);
    end;
    if Y < EditHeight then
    begin
      Canvas.Brush.Color := Palette.Background;
      Canvas.FillRect(Rect(LeftMargin, Y, EditWidth, EditHeight));
      PaintLine80(Y);
    end;
  end
  else FLines.SetNeedPaint(true, X);
end;

procedure TLyCodeEdit.Resize;
begin
  inherited;
  if FLineHeight < 1 then
    FontChange(Self) else
  if FUpdate.AllowPaint then
  begin
    FUpdate.BeginUpdate;
    try
      FLines.SetNeedPaint(true);
    finally
      FUpdate.EndUpdate;
    end;
  end
  else FLines.SetNeedPaint(true);
end;

procedure TLyCodeEdit.MakeVisible(LineX, TextX: integer);
var
  V, H, Y, X, I: integer;
begin
  if FLines.Has(LineX) and (TextX > 0) then
  begin
    { vertical }
    V := FVScrollBar.Position;
    Y := V;
    if LineX < 1 then Y := 0 else
    if LineX < V then Y := LineX else
    begin
      I := V + FVisibleLines - 1;
      if LineX > I then
        Y := Min(FVScrollBar.Max, V + (LineX - I));
    end;
    { horizontal }
    H := FHScrollBar.Position;
    X := H;
    if TextX < 2 then X := 0 else
    begin
      I := (FLines[LineX].TextPos(TextX).X - LineLeft) div FSpaceWidth;
      if I < H then X := I else
      begin
        I := I - (EditWidth - LineLeft) div FSpaceWidth;
        if I > 0 then
          X := Min(FHScrollBar.Max, H + I);
      end;
    end;
    { adjust }
    if (V <> Y) or (H <> X) then
    begin
      if V <> Y then
      begin
        FVScrollBar.OnChange := nil;
        FVScrollBar.Position := Y;
        FVScrollBar.OnChange := {$IFDEF FPC}@{$ENDIF}ScrollBarChange;
      end;
      if H <> X then
      begin
        FHScrollBar.OnChange := nil;
        FHScrollBar.Position := X;
        FHScrollBar.OnChange := {$IFDEF FPC}@{$ENDIF}ScrollBarChange;
      end;
      FCaret.MoveCaret;
      PaintVisibleLines(false);
    end;
  end;
end;

procedure TLyCodeEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  L: TLyLine;
  I: integer;
begin
  SetFocus;
  EnsureNotEmpty;
  FSelection.FSelecting := false;
  if (Button = mbLeft) and (Shift = [ssLeft]) then
  begin
    FUpdate.BeginUpdate;
    try
      FSelection.Unselect;
      I := GetCaretPosAt(X, Y, L);
      if L <> nil then
      begin
        FSelection.FSelecting := true;
        FCaret.MoveTo(L.FIndex, I);
        FCaret.MakeVisible;
      end;
    finally
      FUpdate.EndUpdate;
    end;
  end
  else inherited;
end;

procedure TLyCodeEdit.EnsureNotEmpty;
var
  L: TLyLine;
begin
  if FLines.Count = 0 then
  begin
    L := TLyLine.Create;
    L.FList := FLines;
    FLines.FItems.Add(L);
    FCaret.MoveToHead(L);
  end;
end;

procedure TLyCodeEdit.PressDelete(Shift: TShiftState);
var
  L: TLyLine;
begin
  if not FCaret.Active then Exit;
  FUpdate.BeginUpdate;
  try
    if Readonly then FSelection.Unselect else
    if Shift = [ssShift] then PressCutToClipboard else
    if Shift = [] then
      if FSelection.Selected then FSelection.Delete else
      begin
        L := FCaret.EditLine;
        if (L.FText = '') or (FCaret.FTextX > Length(L.FText)) then
          L.MergeNext else
          L.Delete(FCaret.FTextX);
      end;
  finally
    FUpdate.EndUpdate;
  end;
end;

procedure TLyCodeEdit.PressSetLineMark(AMark: TLyLineMark);
begin
  if FCaret.Active then
    FCaret.Line.ToggleLineMark(AMark);
end;

procedure TLyCodeEdit.PressGoto(AMark: TLyLineMark);
var
  L: TLyLine;
begin
  L := FLines.FindByMark(AMark);
  if L <> nil then
  begin
    if L <> FCaret.Line then
      FCaret.MoveToHead(L);
    FCaret.MakeVisible;
  end;
end;

procedure TLyCodeEdit.PressDeleteLine;
var
  L: TLyLine;
  I: integer;
begin
  if not FCaret.Active then Exit;
  FUpdate.BeginUpdate;
  try
    if FReadonly then FSelection.Unselect else
    begin
      L := FCaret.Line;
      FSelection.Unselect;
      if L.FIndex < FLines.Count - 1 then
      begin
        I := L.FIndex;
        FLines.Delete(L.FIndex);
        L := FLines[I];
      end
      else L.SetText('');
      FCaret.MoveToHead(L);
    end;
  finally
    FUpdate.EndUpdate;
  end;
end;

procedure TLyCodeEdit.KeyDown(var Key: Word; Shift: TShiftState);

  function IsCtrl(K: char): boolean;
  begin
    Result := (Shift = [ssCtrl]) and (Ord(K) = Key);
  end;

  function IsShiftCtrl(K: char): boolean;
  begin
    Result := (Shift = [ssShift, ssCtrl]) and (Ord(K) = Key);
  end;

begin
  EnsureNotEmpty;
  FSelection.FSelecting := false;
  if Key = VK_HOME then PressHome(Shift) else
  if Key = VK_END then PressEnd(Shift) else
  if Key = VK_PRIOR then PressPageUp(Shift) else
  if Key = VK_NEXT then PressPageDown(Shift) else
  if Key = VK_LEFT then PressArrowLeft(Shift) else
  if Key = VK_RIGHT then PressArrowRight(Shift) else
  if Key = VK_UP then PressArrowUp(Shift) else
  if Key = VK_DOWN then PressArrowDown(Shift) else
  if Key = VK_INSERT then PressInsert(Shift) else
  if Key = VK_BACK then PressBackSpace(Shift) else
  if Key = VK_DELETE then PressDelete(Shift) else
  if Key = VK_F5 then PaintVisibleLines(false) else
  if IsCtrl('A') then PressSelectAll else
  if IsCtrl('C') then PressCopyToClipboard else
  if IsCtrl('V') then PressPasteFromClipboard else
  if IsCtrl('X') then PressCutToClipboard else
  if IsCtrl('Y') then PressDeleteLine else
  if IsCtrl('Z') then PressUndo else
  if IsCtrl('0') then PressGoto(lm0) else
  if IsCtrl('1') then PressGoto(lm1) else
  if IsCtrl('2') then PressGoto(lm2) else
  if IsCtrl('3') then PressGoto(lm3) else
  if IsCtrl('4') then PressGoto(lm4) else
  if IsCtrl('5') then PressGoto(lm5) else
  if IsCtrl('6') then PressGoto(lm6) else
  if IsCtrl('7') then PressGoto(lm7) else
  if IsCtrl('8') then PressGoto(lm8) else
  if IsCtrl('9') then PressGoto(lm9) else
  if IsShiftCtrl('0') then PressSetLineMark(lm0) else
  if IsShiftCtrl('1') then PressSetLineMark(lm1) else
  if IsShiftCtrl('2') then PressSetLineMark(lm2) else
  if IsShiftCtrl('3') then PressSetLineMark(lm3) else
  if IsShiftCtrl('4') then PressSetLineMark(lm4) else
  if IsShiftCtrl('5') then PressSetLineMark(lm5) else
  if IsShiftCtrl('6') then PressSetLineMark(lm6) else
  if IsShiftCtrl('7') then PressSetLineMark(lm7) else
  if IsShiftCtrl('8') then PressSetLineMark(lm8) else
  if IsShiftCtrl('9') then PressSetLineMark(lm9) else
  if IsShiftCtrl('Z') then PressRedo else
  begin
    inherited KeyDown(Key, Shift);
    Exit;
  end;
  Key := 0;
end;

function TLyCodeEdit.GetLineAt(Y: integer): TLyLine;
begin
  Y := (Y div FLineHeight) + FVScrollBar.Position;
  if FLines.Has(Y) then
    Result := FLines[Y] else
    Result := nil;
end;

function TLyCodeEdit.GetCaretPosAt(X, Y: integer; var Line: TLyLine): integer;
begin
  if FLines.Count > 0 then
  begin
    Y := (Y div FLineHeight) + FVScrollBar.Position;
    Line := FLines[Max(0, Min(Y, FLines.Count - 1))];
    Result := Line.HitCaret(X);
  end
  else
  begin
    Line := nil;
    Result := -1;
  end;
end;

function TLyCodeEdit.InEditAreaX(X: integer): boolean;
begin
  Result := (X >= LeftMargin) and (X < EditWidth);
end;

function TLyCodeEdit.InEditAreaY(Y: integer): boolean;
begin
  Result := (Y >= 0) and (Y < EditHeight);
end;

function TLyCodeEdit.InEditArea(X, Y: integer): boolean;
begin
  Result := (X >= LeftMargin) and (X < EditWidth) and
            (Y >= 0) and (Y < EditHeight);
end;

procedure TLyCodeEdit.WMGetDlgCode(var Msg: TWmGetDlgCode);
begin
  inherited;
  Msg.Result := DLGC_WANTARROWS or DLGC_WANTCHARS or DLGC_WANTALLKEYS;
end;

procedure TLyCodeEdit.WMKillFocus(var Msg: TWmKillFocus);
begin
  FSelection.FSelecting := false;
  FCaret.HideCaret;
  inherited;
end;

procedure TLyCodeEdit.WMSetFocus(var Msg: TWmSetFocus);
begin
  inherited;
  EnsureNotEmpty;
  FCaret.MoveCaret;
end;

procedure TLyCodeEdit.FontChange(Sender: TObject);

  procedure deal_font_change;
  var
    I: integer;
    L: TLyLine;
  begin
    Canvas.Font.Assign(Font);
    Canvas.Font.Style := [];
    FLineHeight := Canvas.TextHeight('H');
    FSpaceWidth := Canvas.TextWidth(' ');
    FLeftBar.Canvas.Font.Assign(Font);
    FLeftBar.Canvas.Font.Size := Font.Size - 2;
    FLeftBar.FTextHeight := FLeftBar.Canvas.TextHeight('H');
    for I := 0 to FLines.Count - 1 do
    begin
      L := FLines[I];
      L.FNeedPaint := true;
      L.FWidth := L.TextWidth(L.FText);
    end;
  end;

begin
  if FUpdate.AllowPaint then
  begin
    FUpdate.BeginUpdate;
    try
      deal_font_change;
    finally
      FUpdate.EndUpdate;
    end;
  end
  else deal_font_change;
end;

function TLyCodeEdit.GetContextMenu: TLyContextMenu;
begin
  if FContextMenu = nil then
    FContextMenu := TLyContextMenu.Create(Self);
  Result := FContextMenu;
end;

function TLyCodeEdit.EditHeight: integer;
begin
  Result := Max(0, Height - FHScrollBox.Height);
end;

function TLyCodeEdit.EditWidth: integer;
begin
  Result := Max(0, Width - FVScrollBox.Width);
end;

function TLyCodeEdit.LeftMargin: integer;
begin
  if (FLeftBar <> nil) and FLeftBar.Visible then
    Result := FLeftBar.Width else
    Result := 0;
end;

function TLyCodeEdit.LineLeft: integer;
begin
  Result := LeftMargin - (FHScrollBar.Position * FSpaceWidth);
end;

function TLyCodeEdit.StatusOK: boolean;
begin
  Result := HandleAllocated and not (csDestroying in ComponentState);
end;

procedure TLyCodeEdit.ScrollBarChange(Sender: TObject);
begin
  if FUpdate.AllowPaint then
  begin
    FUpdate.BeginUpdate;
    try
      FLines.SetNeedPaint(true, FVScrollBar.Position);
    finally
      FUpdate.EndUpdate;
    end;
  end
  else FLines.SetNeedPaint(true, FVScrollBar.Position);
end;

function TLyCodeEdit.FindNext(const AText: TLyCodeString): boolean;

  function find_in(X1, X2: integer): boolean;
  var
    I, X: integer;
    L: TLyLine;
  begin
    for I := X1 to X2 do
    begin
      L := FLines[I];
      X := PosEx(AText, L.FText, 1);
      if X > 0 then
      begin
        FSelection.Select(I, X, I, X + Length(AText) - 1);
        Exit(true);
      end;
    end;
    Result := false;
  end;

  function find_after_caret: boolean;
  var
    L, I: integer;
  begin
    L := FCaret.FLineX;
    I := PosEx(AText, FLines[L].FText, FCaret.FTextX);
    Result := (I > 0);
    if Result then
      FSelection.Select(L, I, L, I + Length(AText) - 1);
  end;

begin
  FUpdate.BeginUpdate;
  try
    Result := (AText <> '') and FCaret.Active and (
      find_after_caret or
      find_in(FCaret.FLineX + 1, FLines.Count - 1) or
      find_in(0, FCaret.FLineX - 1));
    if Result then
    begin
      FCaret.MoveToSelEnd;
      FCaret.MakeVisible;
    end;
  finally
    FUpdate.EndUpdate;
  end;
end;

procedure TLyCodeEdit.PressArrowDown(Shift: TShiftState);
var
  L: TLyLine;
  I: integer;
begin
  if not FCaret.Active or not IsCtrlShift(Shift) then Exit;
  FUpdate.BeginUpdate;
  try
    if FCaret.FLineX < FLines.Count - 1 then
    begin
      I := FCaret.Line.TextPos(FCaret.FTextX).X;
      L := FLines[FCaret.FLineX + 1];
      I := L.HitCaret(I);
      if ssShift in Shift then
        FSelection.SelectTo(L.FIndex, I) else
        FSelection.Unselect;
      FCaret.MoveTo(L.FIndex, I);
    end;
  finally
    FUpdate.EndUpdate;
  end;
end;

procedure TLyCodeEdit.PressEnd(Shift: TShiftState);
var
  L: TLyLine;
  I: integer;
begin
  if not FCaret.Active or not IsCtrlShift(Shift) then Exit;
  FUpdate.BeginUpdate;
  try
    if ssCtrl in Shift then
      L := FLines.Last else
      L := FLines[FCaret.FLineX];
    I := L.EndPos;
    if ssShift in Shift then
      FSelection.SelectTo(L.FIndex, I) else
      FSelection.Unselect;
    FCaret.MoveTo(L.FIndex, I);
  finally
    FUpdate.EndUpdate;
  end;
end;

procedure TLyCodeEdit.PressHome(Shift: TShiftState);
var
  L: TLyLine;
  I: integer;
begin
  if not FCaret.Active or not IsCtrlShift(Shift) then Exit;
  FUpdate.BeginUpdate;
  try
    if ssCtrl in Shift then
      L := FLines.First else
      L := FLines[FCaret.FLineX];
    I := CE_HOMEPOS;
    if ssShift in Shift then
      FSelection.SelectTo(L.FIndex, I) else
      FSelection.Unselect;
    FCaret.MoveTo(L.FIndex, I);
  finally
    FUpdate.EndUpdate;
  end;
end;

procedure TLyCodeEdit.PressInsert(Shift: TShiftState);
begin
  if Shift = [ssShift] then PressPasteFromClipboard;
end;

procedure TLyCodeEdit.PressArrowLeft(Shift: TShiftState);
var
  L: TLyLine;
  I: integer;
begin
  if not FCaret.Active or not IsCtrlShift(Shift) then Exit;
  FUpdate.BeginUpdate;
  try
    L := FLines[FCaret.FLineX];
    if ssCtrl in Shift then
    begin
      I := L.SeekPriorWord(FCaret.FTextX);
      if I < 1 then
      begin
        if L.FIndex > 0 then
        begin
          L := L.Prior;
          I := L.EndPos;
        end
        else I := CE_HOMEPOS;
      end;
    end
    else
    begin
      I := FCaret.FTextX;
      if I > 1 then Dec(I);
    end;
    if (L.FIndex < FCaret.FLineX) or (I <> FCaret.FTextX) then
    begin
      if ssShift in Shift then
        FSelection.SelectTo(L.FIndex, I) else
        FSelection.Unselect;
      FCaret.MoveTo(L.FIndex, I);
    end;
  finally
    FUpdate.EndUpdate;
  end;
end;

procedure TLyCodeEdit.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  Handled := (PopupMenu = nil);
  if Handled then
    ContextMenu.Popup(MousePos.X, MousePos.Y);
end;

function TLyCodeEdit.DoMouseWheelDown(Shift: TShiftState; XY: TPoint): boolean;
var
  I, N: integer;
  T: TDateTime;
begin
  Result := true;
  T := FWheelTime;
  FWheelTime := Now;
  N := Max(1, 6 - MilliSecondsBetween(FWheelTime, T) div 100);
  I := Min(FVScrollBar.Max, FVScrollBar.Position + N);
  if I <> FVScrollBar.Position then
    FVScrollBar.Position := I;
end;

function TLyCodeEdit.DoMouseWheelUp(Shift: TShiftState; XY: TPoint): boolean;
var
  I, N: integer;
  T: TDateTime;
begin
  Result := true;
  T := FWheelTime;
  FWheelTime := Now;
  N := Max(1, 6 - MilliSecondsBetween(FWheelTime, T) div 100);
  I := Max(0, FVScrollBar.Position - N);
  if I <> FVScrollBar.Position then
    FVScrollBar.Position := I;
end;

procedure TLyCodeEdit.PaintLine80(Y: integer; SingleLine: boolean);
begin
  Canvas.Pen.Color := Palette.Line80Color;
  Canvas.MoveTo(FLine80Pos, Y);
  if SingleLine then
    Canvas.LineTo(FLine80Pos, Y + FLineHeight) else
    Canvas.LineTo(FLine80Pos, EditHeight);
end;

procedure TLyCodeEdit.PressPageDown(Shift: TShiftState);
var
  L: TLyLine;
  I: integer;
begin
  if not FCaret.Active or not IsCtrlShift(Shift) then Exit;
  FUpdate.BeginUpdate;
  try
    if FCaret.FLineX < FLines.Count - 1 then
    begin
      I := FCaret.Line.TextPos(FCaret.FTextX).X;
      L := FLines[Min(FLines.Count - 1, FCaret.FLineX + FVisibleLines)];
      I := L.HitCaret(I);
      if ssShift in Shift then
        FSelection.SelectTo(L.FIndex, I) else
        FSelection.Unselect;
      FCaret.MoveTo(L.FIndex, I);
    end;
  finally
    FUpdate.EndUpdate;
  end;
end;

procedure TLyCodeEdit.PressPageUp(Shift: TShiftState);
var
  L: TLyLine;
  I: integer;
begin
  if not FCaret.Active or not IsCtrlShift(Shift) then Exit;
  FUpdate.BeginUpdate;
  try
    if FCaret.FLineX > 0 then
    begin
      I := FCaret.Line.TextPos(FCaret.FTextX).X;
      L := FLines[Max(0, FCaret.FLineX - FVisibleLines)];
      I := L.HitCaret(I);
      if ssShift in Shift then
        FSelection.SelectTo(L.FIndex, I) else
        FSelection.Unselect;
      FCaret.MoveTo(L.FIndex, I);
    end;
  finally
    FUpdate.EndUpdate;
  end;
end;

procedure TLyCodeEdit.PressPasteFromClipboard;
begin
  if not FCaret.Active then Exit;
  FUpdate.BeginUpdate;
  try
    if FReadonly then
      FSelection.Unselect else
      FSelection.SetText(StrToCode(Clipboard.AsText));
  finally
    FUpdate.EndUpdate;
  end;
end;

procedure TLyCodeEdit.PressRedo;
begin
  FRedos.Apply;
end;

{$IFDEF FPC}
procedure TLyCodeEdit.UTF8KeyPress(var Key: TUTF8Char);
begin
  PressKey(UTF8Decode(Key)[1]);
  Key := '';
end;
{$ELSE}
procedure TLyCodeEdit.KeyPress(var Key: char);
begin
  PressKey(Key);
  Key := #0;
end;
{$ENDIF}

procedure TLyCodeEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  Key := 0;
  FSelection.FSelecting := false;
end;

procedure TLyCodeEdit.PressKey(Key: TLyCodeChar);
var
  L, N: TLyLine;
  I: integer;
begin
  EnsureNotEmpty;
  FSelection.FSelecting := false;
  if not FCaret.Active or not KeyStateOK then Exit;
  FUpdate.BeginUpdate;
  try
    if Readonly then FSelection.Unselect else
    begin
      FSelection.Delete;
      L := FCaret.EditLine;
      if (Key = #13) or (Key = #10) then
      begin
        N := L.BreakFrom(FCaret.FTextX);
        I := N.Insert(1, L.LeadingsSpaces);
        if I > 0 then
          FCaret.MoveTo(N.FIndex, I + 1) else
          FCaret.MoveToHead(N);
      end
      else
      if Key = #9 then // Tab
      begin
        I := L.Insert(FCaret.FTextX, TabSpaces(FCaret.FTextX, FTabSize));
        Inc(FCaret.FTextX, I);
      end
      else
      begin
        if Key < ' ' then Key := ' ';
        if L.FText = '' then
        begin
          L.SetText(Key);
          FCaret.MoveToTail(L);
        end
        else
        begin
          I := L.Insert(FCaret.FTextX, Key);
          Inc(FCaret.FTextX, I);
        end;
      end;
    end;
  finally
    FUpdate.EndUpdate;
  end;
end;

procedure TLyCodeEdit.PressSelectAll;
begin
  FSelection.SelectAll;
end;

procedure TLyCodeEdit.PressUndo;
begin
  FUndos.Apply;
end;

procedure TLyCodeEdit.PressArrowRight(Shift: TShiftState);
var
  L: TLyLine;
  I: integer;
begin
  if not FCaret.Active or not IsCtrlShift(Shift) then Exit;
  FUpdate.BeginUpdate;
  try
    L := FLines[FCaret.FLineX];
    if ssCtrl in Shift then
    begin
      I := L.SeekNextWord(FCaret.FTextX);
      if I < 1 then
      begin
        if L.FIndex < FLines.Count - 1 then
        begin
          L := L.Next;
          I := L.SeekNextWord(1);
          if I < 1 then I := L.EndPos;
        end
        else I := L.EndPos;
      end;
    end
    else I := Min(FCaret.FTextX + 1, FHScrollBar.Max);
    if (L.FIndex > FCaret.FLineX) or (I <> FCaret.FTextX) then
    begin
      if ssShift in Shift then
        FSelection.SelectTo(L.FIndex, I) else
        FSelection.Unselect;
      FCaret.MoveTo(L.FIndex, I);
    end;
  finally
    FUpdate.EndUpdate;
  end;
end;

procedure TLyCodeEdit.PressArrowUp(Shift: TShiftState);
var
  L: TLyLine;
  I: integer;
begin
  if not FCaret.Active or not IsCtrlShift(Shift) then Exit;
  FUpdate.BeginUpdate;
  try
    if FCaret.FLineX > 0 then
    begin
      I := FCaret.Line.TextPos(FCaret.FTextX).X;
      L := FLines[FCaret.FLineX - 1];
      I := L.HitCaret(I);
      if ssShift in Shift then
        FSelection.SelectTo(L.FIndex, I) else
        FSelection.Unselect;
      FCaret.MoveTo(L.FIndex, I);
    end;
  finally
    FUpdate.EndUpdate;
   end;
end;

procedure TLyCodeEdit.PressBackSpace(Shift: TShiftState);
var
  L, P: TLyLine;
  I: integer;
  S: TLyCodeString;

  function has_leading_space: boolean;
  var
    X: integer;
  begin
    for X := 1 to FCaret.FTextX - 1 do
      if L.FText[X] > ' ' then
      begin
        Result := false;
        Exit;
      end;
    Result := true;
  end;

begin
  if not FCaret.Active or (Shift - [ssCtrl] <> []) then Exit;
  FUpdate.BeginUpdate;
  try
    if Readonly then FSelection.Unselect else
    if FSelection.Selected then FSelection.Delete else
    begin
      L := FCaret.EditLine;
      I := FCaret.FTextX;
      if I > 1 then
      begin
        if ssCtrl in Shift then
        begin
          I := L.SeekPriorWord(I);
          if I < 1 then
          begin
             L.Delete(1, FCaret.FTextX - 1);
             FCaret.MoveToHead(L);
          end
          else
          begin
             L.Delete(I, FCaret.FTextX - I);
             FCaret.MoveTo(L.FIndex, I);
          end;
        end
        else
        if has_leading_space then
        begin
          P := L;
          repeat
            P := P.Prior;
            if P <> nil then
              S := P.LeadingsSpaces else
              S := '';
          until (P = nil) or (Length(S) < I - 1);
          L.Delete(Length(S) + 1, FCaret.FTextX - Length(S) - 1);
          if S <> '' then
            FCaret.MoveTo(L.FIndex, Length(S) + 1) else
            FCaret.MoveToHead(L);
        end
        else
        begin
          L.Delete(I - 1);
          if I = 2 then
            FCaret.MoveToHead(L) else
            FCaret.MoveTo(L.FIndex, I - 1);
        end;
      end
      else
      if (Shift = []) and (L.FIndex > 0) then
      begin
        L := FLines[L.FIndex - 1];
        FCaret.MoveToTail(L);
        L.MergeNext;
      end;
    end;
  finally
    FUpdate.EndUpdate;
  end;
end;

procedure TLyCodeEdit.PressCopyToClipboard;
begin
  FSelection.CopyToClipboard;
end;

procedure TLyCodeEdit.PressCutToClipboard;
begin
  if not FCaret.Active then Exit;
  FUpdate.BeginUpdate;
  try
    if FReadonly then FSelection.Unselect else
    begin
      FSelection.CopyToClipboard;
      FSelection.Delete;
    end;
  finally
    FUpdate.EndUpdate;
  end;
end;

procedure TLyCodeEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  L: TLyLine;
  I: integer;
begin
  if FSelection.FSelecting and (Shift = [ssLeft]) then
  begin
    FUpdate.BeginUpdate;
    try
      I := GetCaretPosAt(X, Y, L);
      if L <> nil then
      begin
        FSelection.SelectTo(L.FIndex, I);
        FCaret.MoveTo(L.FIndex, I);
        FCaret.MakeVisible;
      end;
    finally
      FUpdate.EndUpdate;
    end;
  end
  else
  begin
    FSelection.FSelecting := false;
    inherited;
  end;
end;

procedure TLyCodeEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelection.FSelecting := false;
  inherited;
end;

procedure TLyCodeEdit.NotifyStatus;
begin
  if Assigned(FOnStatus) then FOnStatus(Self);
end;

procedure TLyCodeEdit.SetModified(Value: boolean);
var
  I: integer;
begin
  if FModified <> Value then
  begin
    FModified := Value;
    if not FModified then
    begin
      for I := 0 to FLines.Count - 1 do
        FLines[I].FChanged := false;
      FLeftBar.Repaint;
    end;
  end;
end;

procedure TLyCodeEdit.SetReadonly(Value: boolean);
begin
  if FReadonly <> Value then
  begin
    FReadonly := Value;
    FRedos.Clear;
    FUndos.Clear;
  end;
end;

procedure TLyCodeEdit.SetTabSize(Value: integer);
begin
  FTabSize := Min(CE_MAXTABSIZE, Max(Value, CE_MINTABSIZE));
end;

procedure TLyCodeEdit.SetPalette(Value: TLyPalette);
begin
  if Value = nil then
    FPalette := codeedit.Palette else
    FPalette := Value;
end;

{ TLyUpdate }

function TLyUpdate.AllowPaint: boolean;
begin
  Result := not FEndUpdating and (FUpdateCount = 0);
end;

function TLyUpdate.StatusOK: boolean;
begin
  Result := FEdit.StatusOK;
end;

procedure TLyUpdate.Change(LineX: integer; AType: TLyChangeType);
begin
  FEdit.FModified := true;
  FChangedLineX := Min(LineX, FChangedLineX);
  Inc(FChangedLines);
  if AType = ctDeleteLine then Inc(FDeletedLines) else
  if AType = ctAddLine then Inc(FAddedLines);
end;

procedure TLyUpdate.SaveChange(Line: integer; const S: TLyCodeString);
var
  L: TLyUndoList;
  U: TLyUndoItem;
begin
  Change(Line, ctChangeText);
  with FEdit do if not FReadonly and (FUpdateCount > 0) then
  begin
    if FUndoMode = cdNone then FRedos.Clear;
    if FUndoMode = cdUndo then L := FRedos else L := FUndos;
    if L.Enabled then
    begin
      if (L.FLast <> nil) and (L.FLast.FType = ctChangeText) then
        if L.FLast.FLine = Line then
          Exit;
      U := TLyUndoItem.Create(L);
      U.FType := ctChangeText;
      U.FLine := Line;
      U.FText := S;
    end;
  end;
end;

procedure TLyUpdate.SaveDelete(Line: integer; const S: TLyCodeString);
var
  L: TLyUndoList;
  U: TLyUndoItem;
begin
  Change(Line, ctDeleteLine);
  with FEdit do if not FReadonly and (FUpdateCount > 0) then
  begin
    if FUndoMode = cdNone then FRedos.Clear;
    if FUndoMode = cdUndo then L := FRedos else L := FUndos;
    if L.Enabled then
    begin
      if (L.FLast <> nil) and (L.FLast.FLine = Line) then
        if L.FLast.FType = ctChangeText then
        begin
          L.FLast.FType := ctDeleteLine;
          Exit;
        end;
      U := TLyUndoItem.Create(L);
      U.FType := ctDeleteLine;
      U.FLine := Line;
      U.FText := S;
    end;
  end;
end;

procedure TLyUpdate.SaveAdd(Line: integer);
var
  L: TLyUndoList;
  U: TLyUndoItem;
begin
  Change(Line, ctAddLine);
  with FEdit do if not FReadonly and (FUpdateCount > 0) then
  begin
    if FUndoMode = cdNone then FRedos.Clear;
    if FUndoMode = cdUndo then L := FRedos else L := FUndos;
    if L.Enabled then
    begin
      U := TLyUndoItem.Create(L);
      U.FType := ctAddLine;
      U.FLine := Line;
    end;
  end;
end;

constructor TLyUpdate.Create(AEdit: TLyCodeEdit);
begin
  FEdit := AEdit;
end;

procedure TLyUpdate.BeginUpdate;
begin
  Check(not FEndUpdating, 'BeginUpdate: editor is end updating');
  Inc(FUpdateCount);
  if (FUpdateCount = 1) and StatusOK then with FEdit do
  begin
    FUndoMode := cdNone;
    Inc(FUndos.FMagic);
    Inc(FRedos.FMagic);
    EnsureNotEmpty;
    FChangedLines := 0;
    FAddedLines := 0;
    FDeletedLines := 0;
    FChangedLineX := MaxInt;
    FLines.SetNeedPaint(false);
    FCaret.BeginMove;
    FHScrollPos := FHScrollBar.Position;
    FVScrollPos := FVScrollBar.Position;
  end;
end;

procedure TLyUpdate.EndUpdate;
var
  I: integer;
begin
  Dec(FUpdateCount);
  if (FUpdateCount = 0) and StatusOK then with FEdit do
  try
    FEndUpdating := true;
    if FLeftBar <> nil then
      FLeftBar.AdjustWidth;
    FLine80Pos := (FSpaceWidth * 80) + LineLeft;
    FVisibleLines := Max(1, EditHeight div FLineHeight);
    FLines.Pack;
    if FChangedLines > 0 then
      FLines.DecorateFrom(Max(0, FChangedLineX - 1));
    // vertical scroll bar
    I := Max(0, FLines.Count - FVisibleLines);
    FVScrollBar.OnChange := nil;
    FVScrollBar.Max := I;
    FVScrollBar.LargeChange := FVisibleLines;
    FVScrollBar.OnChange := {$IFDEF FPC}@{$ENDIF}ScrollBarChange;
    // horizontal scroll bar
    I := Max(2048, (FLines.Width + Screen.Width) div FSpaceWidth);
    FHScrollBar.OnChange := nil;
    FHScrollBar.Max := I;
    FHScrollBar.LargeChange := Max(1, EditWidth div FSpaceWidth);
    FHScrollBar.OnChange := {$IFDEF FPC}@{$ENDIF}ScrollBarChange;
    // make caret visible
    FCaret.MakeVisible;
  finally
    FEndUpdating := false;
    FCaret.EndMove;
    PaintVisibleLines((FVScrollPos = FVScrollBar.Position) and
                      (FHScrollPos = FHScrollBar.Position) and
                      {not FSelection.Selected and}
                      (FChangedLines = 0) and
                      (FDeletedLines = 0) and
                      (FAddedLines = 0));
    NotifyStatus;
  end;
end;

{ TLyCaret }

procedure TLyCaret.BeginMove;
begin
  Inc(FMoveCount);
  HideCaret;
end;

constructor TLyCaret.Create(AMemo: TLyCodeEdit);
begin
  FEdit := AMemo;
  FWidth := -1;
  FHeight := -1;
  FLineX := 0;
  FTextX := 0;
end;

function TLyCaret.GetActive: boolean;
begin
  Result := FEdit.FLines.Has(FLineX);
  if Result and (FTextX < CE_HOMEPOS) then
    FTextX := CE_HOMEPOS;
end;

function TLyCaret.GetLine: TLyLine;
begin
  if Active then
    Result := FEdit.FLines[FLineX] else
    Result := nil;
end;

procedure TLyCaret.MoveAfterInsert(Line: TLyLine; TextX: integer; const S: TLyCodeString);
var
  X, L: integer;
begin
  X := Length(S);
  L := Length(Line.FText);
  if TextX > L then
  begin
    if X > 0 then Line.Add(S);
    MoveToTail(Line);
  end
  else
  if TextX > 1 then
  begin
    if X > 0 then Line.Insert(TextX, S);
    MoveTo(Line.FIndex, TextX + X);
  end
  else
  if X > 0 then
  begin
    Line.SetText(S + Line.FText);
    MoveTo(Line.FIndex, X + 1);
  end
  else MoveToHead(Line);
end;

procedure TLyCaret.MoveCaret;
var
  M: TLyLine;
begin
  if (FMoveCount = 0) and Active then
  begin
    M := GetLine;
    FPos := M.TextPos(FTextX);
    if (FPos.X >= 0) and (FPos.Y >= 0) then
    begin
      if CreateCaret(2, FEdit.FLineHeight) and ShowCaret then
      begin
        {$IFDEF FPC}
        SetCaretPosEx(FEdit.Handle, FPos.X, FPos.Y);
        {$ELSE}
        SetCaretPos(FPos.X, FPos.Y);
        {$ENDIF}
        Exit;
      end;
    end;
  end;
  DestroyCaret;
  FPos.X := -1;
  FPos.Y := -1;
end;

procedure TLyCaret.MoveToTail(Item: TLyLine);
begin
  if Item <> nil then
    MoveTo(Item.Index, Item.EndPos);
end;

procedure TLyCaret.MoveToHead(Item: TLyLine);
begin
  if Item <> nil then
    MoveTo(Item.Index, CE_HOMEPOS);
end;

procedure TLyCaret.MoveToSelEnd;
var
  L: TLyLine;
begin
  if FEdit.FSelection.Selected then
  begin
    L := FEdit.FLines[FEdit.FSelection.FEndLine];
    MoveTo(L.FIndex, Min(FEdit.FSelection.FEndText + 1, L.EndPos));
  end;
end;

procedure TLyCaret.MakeVisible;
begin
  if Active then
    FEdit.MakeVisible(FLineX, FTextX);
end;

function TLyCaret.CreateCaret(Width, Height: integer): boolean;
begin
  Result := FCreated and (FWidth = Width) and (FHeight = Height);
  if not Result then
  begin
    DestroyCaret;
    {$IFDEF MSWINDOWS}
    Result := Windows.CreateCaret(FEdit.Handle, 0, Width, Height);
    {$ELSE}
    Result := LCLIntf.CreateCaret(FMemo.Handle, 0, Width, Height);
    {$ENDIF}
    if Result then
    begin
      FCreated := true;
      FWidth := Width;
      FHeight := Height;
    end;
  end;
end;

function TLyCaret.ShowCaret: boolean;
begin
  if not FShowing and (FMoveCount = 0) and Active then
  begin
    {$IFDEF MSWINDOWS}
    FShowing := FCreated and Windows.ShowCaret(FEdit.Handle);
    {$ELSE}
    FShowing := FCreated and LCLIntf.ShowCaret(FMemo.Handle);
    {$ENDIF}
  end;
  Result := FShowing;
end;

procedure TLyCaret.HideCaret;
begin
  if FShowing and FEdit.StatusOK then
  begin
    {$IFDEF MSWINDOWS}
    Windows.HideCaret(FEdit.Handle);
    {$ELSE}
    LCLIntf.HideCaret(FMemo.Handle);
    {$ENDIF}
  end;
  FShowing := false;
end;

function TLyCaret.InEditArea: boolean;
begin
  Result := FEdit.InEditArea(FPos.X, FPos.Y);
end;

procedure TLyCaret.Insert(Strings: TStrings);
var
  L: TLyLine;
  S: TLyCodeString;
  I, X: integer;
begin
  if Active and (Strings.Count > 0) then
  begin
    FEdit.FUpdate.BeginUpdate;
    try
      if FEdit.FSelection.Selected then FEdit.FSelection.Delete;
      L := EditLine;
      if Strings.Count > 1 then
      begin
        S := L.CutFrom(FTextX);
        L.Add(StrToCode(Strings[0]));
        X := L.FIndex + 1;
        for I := 1 to Strings.Count - 2 do
        begin
          FEdit.FLines.Insert(X, StrToCode(Strings[I]));
          Inc(X);
        end;
        L := FEdit.FLines.Insert(X, S);
        MoveAfterInsert(L, 0, StrToCode(Strings[Strings.Count - 1]));
      end
      else MoveAfterInsert(L, FTextX, StrToCode(Strings[0]));
    finally
      FEdit.FUpdate.EndUpdate;
    end;
  end;
end;

procedure TLyCaret.Insert(const Text: TLyCodeString);
var
  L: TStrings;
begin
  if (Text <> '') and Active then
  begin
    L := TStringList.Create;
    try
      L.Text := CodeToStr(Text);
      Insert(L);
    finally
      L.Free;
    end;
  end;
end;

procedure TLyCaret.DestroyCaret;
begin
  if FCreated and FEdit.StatusOK then
  begin
    HideCaret;
    {$IFDEF MSWINDOWS}
    Windows.DestroyCaret;
    {$ELSE}
    LCLIntf.DestroyCaret(Memo.Handle);
    {$ENDIF}
  end;
  FCreated := false;
  FWidth := -1;
  FHeight := -1;
end;

function TLyCaret.EditLine: TLyLine;
var
  N: integer;
begin
  Result := Line;
  if Result <> nil then
  begin
    N := FTextX - Result.EndPos;
    if N > 0 then
      Result.FText := Result.FText + StrToCode(StringOfChar(' ', N));
  end;
end;

procedure TLyCaret.EndMove;
begin
  Dec(FMoveCount);
  if FMoveCount = 0 then
    MoveCaret;
end;

procedure TLyCaret.MoveTo(ItemX, TextX: integer);
begin
  if (ItemX <> FLineX) or (TextX <> FTextX) then
  begin
    FLineX := ItemX;
    FTextX := TextX;
    MoveCaret;
  end;
end;

{ TLySelection }

function TLySelection.GetStartLine: TLyLine;
begin
  if Selected then
    Result := FEdit.FLines[FStartLine] else
    Result := nil;
end;

function TLySelection.GetEndLine: TLyLine;
begin
  if Selected then
    Result := FEdit.FLines[FEndLine] else
    Result := nil;
end;

function TLySelection.GetSelected: boolean;
var
  L: TLyLineList;
begin
  L := FEdit.FLines;
  Result := L.HasInOrder(FStartLine, FEndLine) and
            (FStartText >= 0) and (FEndText >= 0) and
            ((FStartLine <> FEndLine) or
             (FStartText <= FEndText));
end;

function TLySelection.GetSelectedAll: boolean;
var
  L: TLyLineList;
begin
  L := FEdit.FLines;
  Result := (L.Count > 0) and (FStartLine = 0) and
    (FStartText = CE_HOMEPOS) and (FEndLine = L.Count - 1) and
    (FEndText = L.Last.EndPos);
end;

function TLySelection.GetSelectedLineCount: integer;
begin
  if Selected then
    Result := FEndLine - FStartLine + 1 else
    Result := 0;
end;

function TLySelection.GetSelectedPart: boolean;
begin
  Result := Selected and not SelectedAll;
end;

function TLySelection.GetSelectedTextRange(LineX: integer;
  var StartX, EndX: integer): boolean;
begin
  StartX := 0;
  EndX := 0;
  Result := HasLine(LineX);
  if Result then
    if FStartLine = FEndLine then
    begin
      StartX := FStartText;
      EndX := FEndText;
    end
    else
    if FStartLine = LineX then
    begin
      StartX := FStartText;
      EndX := Length(FEdit.FLines[LineX].FText);
    end
    else
    if FEndLine = LineX then
    begin
      StartX := 1;
      EndX := FEndText;
    end
    else
    begin
      StartX := 1;
      EndX := Length(FEdit.FLines[LineX].FText);
    end;
end;

function TLySelection.GetText: TLyCodeString;
var
  L: TLyLine;
  I: integer;
begin
  Result := '';
  if SelectedAll then
    Result := FEdit.FLines.Text else
  if Selected then
  begin
    L := FEdit.FLines[FStartLine];
    if FStartLine < FEndLine then
    begin
      Result := Copy(L.FText, Max(1, FStartText), Length(L.FText));
      for I := FStartLine + 1 to FEndLine - 1 do
        Result := Result + sLineBreak + FEdit.FLines[I].FText;
      L := FEdit.FLines[FEndLine];
      Result := Result + sLineBreak + Copy(L.FText, 1, FEndText);
    end
    else Result := Copy(L.FText, Max(1, FStartText), FEndText - FStartText + 1);
  end;
end;

function TLySelection.HasLine(LineX: integer): boolean;
begin
  Result := (LineX >= FStartLine) and (LineX <= FEndLine) and Selected;
end;

constructor TLySelection.Create(AEdit: TLyCodeEdit);
begin
  FEdit := AEdit;
  FStartLine := -1;
  FStartText := -1;
  FEndLine := -1;
  FEndText := -1;
end;

procedure TLySelection.CutToClipboard;
begin
  BeginUpdate;
  try
    CopyToClipboard;
    Delete;
  finally
    EndUpdate;
  end;
end;

procedure TLySelection.Delete;
var
  F, L: TLyLine;
  S, E: integer;
begin
  BeginUpdate;
  try
    if Selected then
    begin
      F := FEdit.FLines[FStartLine];
      S := FStartText;
      L := FEdit.FLines[FEndLine];
      E := FEndText;
      FEdit.FUpdate.SaveChange(F.FIndex, F.FText); // save selection
      Unselect;
      FEdit.FCaret.MoveTo(F.FIndex, S);
      if F <> L then
      begin
        F.Delete(S, Length(F.FText));
        F.Add(Copy(L.FText, E + 1, Length(L.FText)));
        FEdit.FLines.Delete(F.FIndex + 1, L.FIndex);
      end
      else F.Delete(S, E - S + 1);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TLySelection.SelectAll;
var
  L: TLyLineList;
begin
  BeginUpdate;
  try
    L := FEdit.FLines;
    if L.Count > 0 then
    begin
      FStartLine := 0;
      FStartText := CE_HOMEPOS;
      FEndLine := L.Count - 1;
      FEndText := L.Last.EndPos;
      FEdit.PaintVisibleLines(false);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TLySelection.EndUpdate;
begin
  FEdit.FUpdate.EndUpdate;
end;

procedure TLySelection.Unselect;
begin
  Select(-1, -1, -1, -1);
end;

procedure TLySelection.Select(LineX, TextX, EndLineX, EndTextX: integer);
begin
  BeginUpdate;
  try
    Paint;
    FStartLine := LineX;
    FStartText := TextX;
    FEndLine := EndLineX;
    FEndText := EndTextX;
    Paint;
  finally
    EndUpdate;
  end;
end;

procedure TLySelection.SetText(const S: TLyCodeString);
begin
  BeginUpdate;
  try
    Delete;
    FEdit.FCaret.Insert(S);
  finally
    EndUpdate;
  end;
end;

procedure TLySelection.SelectTo(LineX, TextX: integer);
var
  LX, TX, F: integer;
begin
  if not Selected then
    if FEdit.FCaret.Active then
    begin
      FLineX := FEdit.FCaret.FLineX;
      FTextX := FEdit.FCaret.FTextX;
    end
    else FLineX := -1;
  LX := FLineX;
  TX := FTextX;
  if (LX >= 0) and (TX >= 0) then
  begin
    F := ComparePos(LX, TX, LineX, TextX);
    if F > 0 then
    begin
      if TX > 0 then Dec(TX);
      Select(LineX, TextX, LX, TX);
    end
    else
    if F < 0 then
    begin
      if TextX > 0 then Dec(TextX);
      Select(LX, TX, LineX, TextX);
    end
    else Unselect;
  end;
end;

function TLySelection.HasText(LineX, TextX: integer): boolean;
begin
  Result := HasLine(LineX) and (TextX > 0) {and (TextX < FEdit.FLines[LineX].EndPos)};
  if Result then
    if FStartLine = FEndLine then
      Result := (TextX >= FStartText) and (TextX <= FEndText) else
    if FStartLine = LineX then
      Result := (TextX >= FStartText) else
    if FEndLine = LineX then
      Result := (TextX <= FEndText);
end;

function TLySelection.IsEndLine(LineX: integer): boolean;
begin
  Result := (LineX = FEndLine) and Selected;
end;

function TLySelection.IsStartLine(LineX: integer): boolean;
begin
  Result := (LineX = FStartLine) and Selected;
end;

procedure TLySelection.CopyToClipboard;
begin
  Clipboard.AsText := CodeToStr(GetText);
end;

procedure TLySelection.BeginUpdate;
begin
  FEdit.FUpdate.BeginUpdate;
end;

procedure TLySelection.Paint;
var
  I: integer;
begin
  if Selected then
  begin
    BeginUpdate;
    try
      for I := FStartLine to FEndLine do
        FEdit.FLines[I].Paint;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TLySelection.PasteFromClipboard;
begin
  BeginUpdate;
  try
    SetText(StrToCode(Clipboard.AsText));
  finally
    EndUpdate;
  end;
end;

{ TLyContextMenu }

function TLyContextMenu.AddChild(const ACaption: string): TMenuItem;
begin
  Result := TMenuItem.Create(Self);
  Result.Caption := ACaption;
  Items.Add(Result);
end;

function TLyContextMenu.AddChild(const ACaption, ShortCut: string): TMenuItem;
begin
  Result := AddChild(ACaption);
  Result.ShortCut := StrToShortCut(ShortCut);
end;

function TLyContextMenu.AddChild(Root: TMenuItem; const ACaption: string): TMenuItem;
begin
  Result := TMenuItem.Create(Self);
  Result.Caption := ACaption;
  Root.Add(Result);
end;

function TLyContextMenu.AddChild(Root: TMenuItem; const ACaption, ShortCut: string): TMenuItem;
begin
  Result := AddChild(Root, ACaption);
  Result.ShortCut := StrToShortCut(ShortCut);
end;

procedure TLyContextMenu.CopyClick(Sender: TObject);
begin
  FEdit.FSelection.CopyToClipboard;
end;

constructor TLyContextMenu.Create(AOwner: TComponent);
var
  I: TLyLineMark;
  X: string;
begin
  inherited;
  FEdit := AOwner as TLyCodeEdit;
  FUndo := AddChild('&Undo', 'CTRL-Z');
  FUndo.OnClick := {$IFDEF FPC}@{$ENDIF}UndoClick;
  FRedo := AddChild('&Redo', 'SHIFT-CTRL-Z');
  FRedo.OnClick := {$IFDEF FPC}@{$ENDIF}RedoClick;
  AddChild('-');
  FCut := AddChild('Cu&t', 'CTRL-X');
  FCut.OnClick := {$IFDEF FPC}@{$ENDIF}CutClick;
  FCopy := AddChild('&Copy', 'CTRL-C');
  FCopy.OnClick := {$IFDEF FPC}@{$ENDIF}CopyClick;
  FPaste := AddChild('&Paste', 'CTRL-V');
  FPaste.OnClick := {$IFDEF FPC}@{$ENDIF}PasteClick;
  AddChild('-');
  FGotoMark := AddChild('&Goto Linemark');
  FToggleMark := AddChild('&Toggle Linemark');
  for I := lm0 to lm9 do
  begin
    X := IntToStr(Ord(I) - 1);
    FGotoMenus[I] := AddChild(FGotoMark, 'Linemark &' + X, 'CTRL-' + X);
    FGotoMenus[I].OnClick := {$IFDEF FPC}@{$ENDIF}GotoMark;
    FGotoMenus[I].Tag := Ord(I);
    FToggleMenus[I] := AddChild(FToggleMark, 'Linemark &' + X, 'SHIFT-CTRL-' + X);
    FToggleMenus[I].OnClick := {$IFDEF FPC}@{$ENDIF}ToggleMark;
    FToggleMenus[I].Tag := Ord(I);
  end;
  AddChild('-');
  FSelectAll := AddChild('Select &All', 'CTRL-A');
  FSelectAll.OnClick := {$IFDEF FPC}@{$ENDIF}SelectAllClick;
end;

procedure TLyContextMenu.CutClick(Sender: TObject);
begin
  FEdit.FSelection.CutToClipboard;
end;

procedure TLyContextMenu.UpdateChildMenus;
var
  I: TLyLineMark;
  N: integer;
  L: TLyLine;
begin
  FUndo.Enabled := not FEdit.FReadonly and (FEdit.FUndos.FLast <> nil);
  FRedo.Enabled := not FEdit.FReadonly and (FEdit.FRedos.FLast <> nil);
  FCut.Enabled := not FEdit.FReadonly and FEdit.FSelection.Selected;
  FCopy.Enabled := FEdit.FSelection.Selected;
  FPaste.Enabled := not FEdit.FReadonly and FEdit.FCaret.Active and HasTextFormat;
  N := 0;
  for I := lm0 to lm9 do
  begin
    L := FEdit.FLines.FindByMark(I);
    if L <> nil then
    begin
      FGotoMenus[I].Caption := Format('Linemark& %d:%d', [Ord(I) - 1, L.FIndex + 1]);
      FToggleMenus[I].Caption := FGotoMenus[I].Caption;
      FGotoMenus[I].Enabled := true;
      FGotoMenus[I].Visible := true;
      Inc(N);
    end
    else
    begin
      FToggleMenus[I].Caption := Format('Linemark& %d', [Ord(I) - 1]);
      FGotoMenus[I].Enabled := false;
      FGotoMenus[I].Visible := false;
    end;
  end;
  FGotoMark.Enabled := (N > 0);
  FToggleMark.Enabled := (FLine <> nil);
end;

procedure TLyContextMenu.PasteClick(Sender: TObject);
begin
  FEdit.FSelection.PasteFromClipboard;
end;

procedure TLyContextMenu.GotoMark(Sender: TObject);
begin
  FEdit.PressGoto(TLyLineMark(TMenuItem(Sender).Tag));
end;

procedure TLyContextMenu.ToggleMark(Sender: TObject);
begin
  FLine.ToggleLineMark(TLyLineMark(TMenuItem(Sender).Tag));
end;

procedure TLyContextMenu.Popup(X, Y: Integer);
var
  P: TPoint;
begin
  FLine := FEdit.GetLineAt(Y);
  UpdateChildMenus;
  P := FEdit.ClientToScreen(Point(X, Y));
  inherited Popup(P.X, P.Y);
end;

procedure TLyContextMenu.RedoClick(Sender: TObject);
begin
  FEdit.FRedos.Apply;
end;

procedure TLyContextMenu.SelectAllClick(Sender: TObject);
begin
  FEdit.FSelection.SelectAll;
end;

procedure TLyContextMenu.UndoClick(Sender: TObject);
begin
  FEdit.FUndos.Apply;
end;

{ TLyLeftBar }

function TLyLeftBar.AdjustWidth: integer;
var
  S: string;
begin
  S := IntToStr(FEdit.FLines.Count + 9);
  Result := Canvas.TextWidth(S) + CE_LBDELTA + FTextHeight * 3;
  if Result <> Width then
    Width := Result;
end;

constructor TLyLeftBar.Create(AOwner: TComponent);
begin
  inherited;
  Color := clBtnFace;
end;

destructor TLyLeftBar.Destroy;
begin

  inherited;
end;

function TLyLeftBar.GetLineHeight: integer;
begin
  Result := FEdit.FLineHeight;
end;

function TLyLeftBar.GetTextLeft: integer;
begin
  Result := FTextHeight * 2;
end;

function TLyLeftBar.GetTextWidth: integer;
begin
  Result := Width - CE_LBDELTA - FTextHeight * 3;
end;

function TLyLeftBar.GetVertLinePos: integer;
begin
  Result := Width - TextHeight;
end;

procedure TLyLeftBar.Paint;
var
  I: integer;
begin
  for I := 0 to FEdit.FVisibleLines do
    PaintLine(FEdit.FVScrollBar.Position + I);
end;

procedure TLyLeftBar.PaintLine(LineX: integer);
var
  H, X, Y, I, P, W: integer;
  S: string;
  F: boolean;
  L: TLyLine;
begin
  H := FEdit.FLineHeight;
  Y := (LineX - FEdit.FVScrollBar.Position) * H;
  if (Y >= 0) and (Y < Height) then
  begin
    if FEdit.FLines.Has(LineX) then
      L := FEdit.FLines[LineX] else
      L := nil;
    F := (L <> nil) and (LineX = FEdit.FCaret.FLineX);

    // 1.draw line mark
    X := 0;
    P := Y + (H - FTextHeight) div 2;
    if (L <> nil) and (L.FLineMark <> lmNone) then
    begin
      S := IntToStr(Ord(L.FLineMark) - 1);
      W := Canvas.TextWidth(S) + 4;
      Canvas.Brush.Color := FEdit.Palette.LineMarkBackground;
      Canvas.Font.Color := FEdit.Palette.SelectedTextColor;
      Canvas.FillRect(Rect(X, Y, X + W, Y + H));
      Canvas.TextOut(2, P, S);
      Inc(X, W);
    end;

    // 2.draw line number
    I := VertLinePos;
    if F then
      Canvas.Brush.Color := FEdit.Palette.ActiveBackground else
      Canvas.Brush.Color := FEdit.Palette.LeftBarBackground;
    Canvas.FillRect(Rect(X, Y, I, Y + H));
    if L <> nil then
      if F or ((LineX + 1) mod 10 = 0) then
      begin
        S := IntToStr(LineX + 1);
        W := Canvas.TextWidth(S);
        if F then
          Canvas.Font.Color := FEdit.Palette.SpaceColor else
          Canvas.Font.Color := clGray;
        Canvas.TextOut(I - CE_LBDELTA - W, P, S);
      end
      else
      begin
        W := Canvas.TextWidth('9');
        if (LineX + 1) mod 5 = 0 then
          X := I - CE_LBDELTA - W else
          X := I - CE_LBDELTA - W div 2;
        Canvas.Pen.Color := clGray;
        Canvas.MoveTo(X, Y + H div 2);
        Canvas.LineTo(I - CE_LBDELTA - 1, Y + H div 2);
      end;

    // 3.draw changed bar
    if (L <> nil) and L.FChanged then
    begin
      Canvas.Pen.Color := FEdit.Palette.ChangedBackground;
      Canvas.MoveTo(I - 2, Y);
      Canvas.LineTo(I - 2, Y + H);
      Canvas.MoveTo(I - 3, Y);
      Canvas.LineTo(I - 3, Y + H);
      Canvas.MoveTo(I - 4, Y);
      Canvas.LineTo(I - 4, Y + H);
    end;

    // 4.draw vertical line
    Canvas.Pen.Color := clGray;
    Canvas.MoveTo(I, Y);
    Canvas.LineTo(I, Y + H);

    // 5.clear reserved right area
    Canvas.Brush.Color := FEdit.Palette.Background;
    Canvas.FillRect(Rect(I + 1, Y, Width, Y + H));
  end;
end;

{ TLyLine }

function TLyLine.Add(const S: TLyCodeString): integer;
begin
  Result := Length(S);
  if Result > 0 then
  begin
    Editor.FUpdate.SaveChange(FIndex, FText);
    FText := FText + S;
    Change;
  end;
end;

function TLyLine.TextOut(X, Y: integer; const S: TLyCodeString): integer;
begin
  Canvas.TextOut(X, Y, CodeToStr(S));
  Result := TextWidth(S);
end;

function TLyLine.TextPos(TextX: integer): TPoint;
begin
  Result.Y := Top;
  Result.X := Left + TextWidth(Copy(FText, 1, TextX - 1));
  // maybe after end position of line
  Dec(TextX, EndPos);
  if TextX > 0 then
    Inc(Result.X, TextWidth(' ') * TextX);
end;

function TLyLine.Delete(TextX: integer; Count: integer): integer;
var
  S: TLyCodeString;
begin
  S := FText;
  System.Delete(S, TextX, Count);
  Result := Length(FText) - Length(S);
  if Result > 0 then
  begin
    Editor.FUpdate.SaveChange(FIndex, FText);
    FText := S;
    Change;
  end;
end;

procedure TLyLine.BeginUpdate;
begin
  FList.BeginUpdate;
end;

function TLyLine.BreakFrom(TextX: integer): TLyLine;
begin
  BeginUpdate;
  try
    Result := FList.Insert(FIndex + 1, CutFrom(TextX));
  finally
    EndUpdate;
  end;
end;

function TLyLine.MergeNext: boolean;
var
  L: TLyLine;
begin
  L := Next;
  Result := (L <> nil);
  if Result then
  begin
    BeginUpdate;
    try
      Add(L.FText);
      FList.Delete(L.FIndex);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TLyLine.SetText(const Value: TLyCodeString);
begin
  if FText <> Value then
  begin
    BeginUpdate;
    try
      Editor.FUpdate.SaveChange(FIndex, FText);
      FText := Value;
      Change;
    finally
      EndUpdate;
    end;
  end;
end;

destructor TLyLine.Destroy;
begin
  FText := '';
  if FList <> nil then
  begin
    BeginUpdate;
    try
      FList.FItems.Remove(Self);
      Change;
      Inc(FList.FEdit.FUpdate.FDeletedLines);
      FList.ResetIndex(FIndex);
    finally
      EndUpdate;
    end;
  end;
  inherited;
end;

function TLyLine.EndPos: integer;
begin
  Result := Length(FText) + 1;
end;

procedure TLyLine.ToggleLineMark(AMark: TLyLineMark);
begin
  if FLineMark = AMark then
    SetLineMark(lmNone) else
    SetLineMark(AMark);
end;

procedure TLyLine.EndUpdate;
begin
  FList.EndUpdate;
end;

function TLyLine.LeadingsSpaces: TLyCodeString;
var
  I: integer;
begin
  for I := 1 to Length(FText) do
    if FText[I] <> ' ' then
    begin
      Result := Copy(FText, 1, I - 1);
      Exit;
    end;
  Result := FText;
end;

function TLyLine.HitCaret(X: integer): integer;
var
  L, I: integer;
begin
  L := Left;
  if L < X then
  begin
    I := L + TextWidth(FText);
    if I > X then
    begin
      for I := 1 to Length(FText) do
      begin
        Inc(L, TextWidth(FText[I]));
        if X < L then
        begin
          Result := I;
          Exit;
        end;
      end;
      Result := Length(FText) + 1;
    end
    else
    begin
      Result := Length(FText) + 1 + (X - I) div Editor.FSpaceWidth;
      if Result > Editor.FHScrollBar.Max then
        Result := Editor.FHScrollBar.Max;
    end;
  end
  else Result := 1;
end;

function TLyLine.Insert(TextX: integer; const S: TLyCodeString): integer;
begin
  Result := Length(S);
  if Result > 0 then
  begin
    Editor.FUpdate.SaveChange(FIndex, FText);
    if TextX <= 1 then FText := S + FText else
    if TextX > Length(FText) then FText := FText + S else
      System.Insert(S, FText, TextX);
    Change;
  end;
end;

function TLyLine.Prior: TLyLine;
var
  I: integer;
begin
  I := FIndex - 1;
  if I >= 0 then
    Result := FList[I] else
    Result := nil;
end;

function TLyLine.Canvas: TCanvas;
begin
  Result := FList.FEdit.Canvas;
end;

function TLyLine.Caret: TLyCaret;
begin
  Result := Editor.FCaret;
end;

procedure TLyLine.Change;
begin
  SetChanged(true);
end;

function TLyLine.CharToken(TextX: integer): TLyToken;
begin
  Dec(TextX);
  if (TextX >= 0) and (TextX < Length(FTokens)) then
    Result := FTokens[TextX] else
    Result := TK_SPACE;
end;

function TLyLine.CutFrom(TextX: integer): TLyCodeString;
begin
  BeginUpdate;
  try
    if TextX <= 1 then
    begin
      Result := FText;
      SetText('');
    end
    else
    if TextX <= Length(FText) then
    begin
      Editor.FUpdate.SaveChange(FIndex, FText);
      Result := Copy(FText, TextX, Length(FText));
      FText := Copy(FText, 1, TextX - 1);
      Change;
    end
    else Result := '';
  finally
    EndUpdate;
  end;
end;

function TLyLine.GetHeight: integer;
begin
  Result := Editor.FLineHeight;
end;

procedure TLyLine.SetLineMark(AValue: TLyLineMark);
var
  I: integer;
  L: TLyLine;
begin
  if FLineMark <> AValue then
  begin
    FLineMark := AValue;
    if FLineMark <> lmNone then
      for I := 0 to FList.Count - 1 do
      begin
        L := FList[I];
        if (L <> Self) and (L.FLineMark = FLineMark) then
        begin
          L.FLineMark := lmNone;
          FList.FEdit.FLeftBar.PaintLine(I);
        end;
      end;
    FList.FEdit.FLeftBar.PaintLine(FIndex);
  end;
end;

function TLyLine.GetPriorToken: TLyToken;
begin
  if FIndex = 0 then
    Result := TK_SPACE else
    Result := Prior.FLastToken;
end;

procedure TLyLine.UseSelected(Token: TLyToken);
begin
  Canvas.Font.Style := FList.FEdit.Syntax.GetTokenStyle(Token);
  Canvas.Font.Color := FList.FEdit.Palette.SelectedTextColor;
  Canvas.Brush.Color := FList.FEdit.Palette.SelectedBackground;
  Canvas.Pen.Color := Canvas.Brush.Color;
end;

procedure TLyLine.UseNormal(Token: TLyToken);
begin
  Canvas.Font.Style := FList.FEdit.Syntax.GetTokenStyle(Token);
  Canvas.Font.Color := FList.FEdit.Syntax.GetTokenColor(Token);
  Canvas.Brush.Color := FList.FEdit.Palette.Background;
  Canvas.Pen.Color := Canvas.Brush.Color;
end;

function TLyLine.GetLeft: integer;
begin
  Result := Editor.LineLeft;
end;

function TLyLine.TextWidth(const S: TLyCodeString): integer;
begin
  if (S <> '') and Editor.StatusOK then
   Result := Canvas.TextWidth(CodeToStr(S)) else
   Result := 0;
end;

function TLyLine.Selected(TextX: integer): boolean;
begin
  Result := Selection.HasText(FIndex, TextX);
end;

function TLyLine.Editor: TLyCodeEdit;
begin
  Result := FList.FEdit;
end;

function TLyLine.Pack: boolean;
begin
  Result := (FText <> '') and (FText[Length(Text)] <= ' ');
  if Result then
  begin
    FText := TrimRight(FText);
    SetLength(FTokens, Length(FText));
    FWidth := TextWidth(FText);
  end;
end;

procedure TLyLine.Paint(DoPaint: boolean);
var
  I, P, W, X, Y, L, R: integer;
  S: boolean;
  Z: TLyToken;
begin
  FNeedPaint := true;
  if DoPaint or Editor.FUpdate.AllowPaint then
  begin
    FNeedPaint := false;
    W := Editor.EditWidth;
    X := Left;
    Y := Top;
    if (X + FWidth) >= Editor.LeftMargin then
    begin
      if Editor.LeftMargin - X > Editor.EditWidth then
      begin
        I := HitCaret(Editor.LeftMargin);
        X := TextPos(I).X;
      end
      else I := 1;
      if I <= Length(FText) then
      begin
        L := 0;
        R := 0;
        Selection.GetSelectedTextRange(FIndex, L, R);
        while (I <= Length(FText)) and (X < W) do
        begin
          Z := CharToken(I);
          S := (I >= L) and (I <= R);
          if S then UseSelected(Z) else UseNormal(Z);
          P := I + 1;
          while (P <= Length(FText)) and (S = ((P >= L) and (P <= R)))
            and (Z = CharToken(P)) do Inc(P);
          Inc(X, TextOut(X, Y, Copy(FText, I,  P - I)));
          I := P;
        end
      end;
    end
    else X := Editor.LeftMargin;
    if X < W then
    begin
      if Selection.SelectedLineCount < 2 then UseNormal else
      if Selection.IsStartLine(FIndex) then
      begin
        if Selection.FStartText > EndPos then
        begin
          I := Max(X, TextPos(Selection.FStartText).X);
          UseNormal;
          Canvas.FillRect(Rect(X, Y, I, Y + Height));
          X := I;
        end;
        UseSelected;
      end
      else
      if Selection.IsEndLine(FIndex) then
      begin
        if Selection.FEndText >= EndPos then
        begin
          I := Max(X, TextPos(Selection.FEndText + 1).X);
          UseSelected;
          Canvas.FillRect(Rect(X, Y, I, Y + Height));
          X := I;
        end;
        UseNormal;
      end
      else
      if Selection.HasLine(FIndex) then
        UseSelected else
        UseNormal;
      Canvas.FillRect(Rect(X, Y, W, Y + Height));
    end;
    Editor.PaintLine80(Y);
  end;
end;

function TLyLine.PeekChar(TextX: integer): TLyCodeChar;
begin
  if (TextX > 0) and (TextX <= Length(FText)) then
    Result := FText[TextX] else
    Result := ' ';
end;

procedure TLyLine.SetChanged(Value: boolean);
begin
  if Value then
  begin
    BeginUpdate;
    try
      FLastToken := -1;
      FNeedPaint := true;
      FWidth := TextWidth(FText);
      FList.FEdit.FUpdate.Change(FIndex);
    finally
      EndUpdate;
    end;
  end;
  FChanged := Value;
end;

function TLyLine.SeekNextWord(TextX: integer): integer;
var
  I, L: integer;
begin
  L := Length(FText);
  I := TextX;
  if I > 0 then
  begin
    while (I <= L) and CharInSet(FText[I], CS_ID + ['$', '#']) do Inc(I);
    while (I <= L) and not CharInSet(FText[I], CS_ID + ['$', '#']) do Inc(I);
  end;
  if I <= L then
    Result := I else Result := 0;
end;

function TLyLine.SeekPriorWord(TextX: integer): integer;
var
  I: integer;
begin
  I := TextX - 1;
  while (I > 0) and not CharInSet(FText[I], CS_ID + ['$', '#']) do Dec(I);
  while (I > 1) and CharInSet(FText[I - 1], CS_ID + ['$']) do Dec(I);
  if I > 0 then
    Result := I else Result := 0;
end;

function TLyLine.Selected: boolean;
begin
  Result := Selection.HasLine(FIndex);
end;

function TLyLine.Selection: TLySelection;
begin
  Result := Editor.FSelection;
end;

function TLyLine.GetTop: integer;
begin
  Result := (FIndex - Editor.FVScrollBar.Position) * Height;
end;

function TLyLine.Next: TLyLine;
var
  I: integer;
begin
  I := FIndex + 1;
  if I < FList.Count then
    Result := FList[I] else
    Result := nil;
end;

{ TLyLineList }

function TLyLineList.GetCount: integer;
begin
  if FItems <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function TLyLineList.GetFirst: TLyLine;
begin
  Result := GetItem(0);
end;

function TLyLineList.GetItem(Index: integer): TLyLine;
begin
  Result := TLyLine(FItems[Index]);
end;

function TLyLineList.GetLast: TLyLine;
begin
  Result := GetItem(Count - 1);
end;

function TLyLineList.GetStrings: TStrings;
begin
  Result := TStringList.Create;
  SaveToStrings(Result);
end;

constructor TLyLineList.Create(AEdit: TLyCodeEdit);
begin
  FEdit := AEdit;
  FItems := TList.Create;
end;

procedure TLyLineList.Delete(BegIndex, EndIndex: integer);
var
  I: integer;
begin
  BeginUpdate;
  try
    for I := EndIndex downto BegIndex do Delete(I);
  finally
    EndUpdate;
  end;
end;

destructor TLyLineList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TLyLineList.Add(const S: TLyCodeString): TLyLine;
begin
  Result := Insert(Count, S);
end;

procedure TLyLineList.BeginUpdate;
begin
  FEdit.FUpdate.BeginUpdate;
end;

procedure TLyLineList.EndUpdate;
begin
  FEdit.FUpdate.EndUpdate;
end;

function TLyLineList.FirstChanged: integer;
var
  I: integer;
begin
  for I := 0 to GetCount - 1 do
    if GetItem(I).FChanged then
    begin
      Result := I;
      Exit;
    end;
  Result := 0;
end;

procedure TLyLineList.SaveToFile(const FileName: string);
var
  L: TStrings;
begin
  L := GetStrings;
  try
    if MatchID(ExtractFileExt(FileName), ['.ls', '.rivi']) then
    begin
      {$IFDEF UNICODE}
      l.WriteBOM := false;
      L.SaveToFile(FileName, TEncoding.UTF8);
      {$ELSE}
      L.SaveToFile(FileName);
      {$ENDIF}
    end
    else L.SaveToFile(FileName);
  finally
    L.Free;
  end;
end;

procedure TLyLineList.SaveToStream(AStream: TStream);
var
  L: TStrings;
begin
  L := GetStrings;
  try
    L.SaveToStream(AStream);
  finally
    L.Free;
  end;
end;

procedure TLyLineList.SaveToStrings(Strings: TStrings);
var
  I: integer;
begin
  for I := 0 to GetCount - 1 do
    Strings.Add(CodeToStr(GetItem(I).FText));
end;

procedure TLyLineList.SetNeedPaint(Value: boolean; StartX: integer);
var
  I: integer;
begin
  for I := StartX to Count - 1 do
    GetItem(I).FNeedPaint := Value;
end;

procedure TLyLineList.SetText(const Value: TLyCodeString);
var
  L: TStrings;
begin
  L := TStringList.Create;
  try
    L.Text := CodeToStr(Value);
    AssignStrings(L);
  finally
    L.Free;
  end;
end;

procedure TLyLineList.DecorateFrom(StartX: integer);
var
  S: TLySyntax;
  I: integer;
  L: TLyLine;
  T: TLyToken;
begin
  if Has(StartX) then
  begin
    S := FEdit.Syntax;
    S.Decorate(GetItem(StartX));
    for I := StartX + 1 to Count - 1 do
    begin
      L := GetItem(I);
      T := L.FLastToken;
      if T <> S.Decorate(L) then
        L.FNeedPaint := true else
        Exit;
    end;
  end;
end;

procedure TLyLineList.AssignStrings(Strings: TStrings);
var
  I: integer;
begin
  BeginUpdate;
  try
    ClearToOneEmptyLine;
    if (Strings <> nil) and (Strings.Count > 0) then
    begin
      First.SetText(StrToCode(TrimRight(Strings[0])));
      for I := 1 to Strings.Count - 1 do
        Add(StrToCode(TrimRight(Strings[I])));
    end;
  finally
    EndUpdate;
  end;
end;

procedure TLyLineList.Clear;
begin
  BeginUpdate;
  try
    FEdit.FSelection.Delete;
    Delete(0, Count - 1);
    FEdit.FCaret.MoveTo(-1, -1);
  finally
    EndUpdate;
  end;
end;

function TLyLineList.Pack: integer;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if I <> FEdit.FCaret.FLineX then
      if GetItem(I).Pack then
        Inc(Result);
end;

function TLyLineList.FindByMark(AMark: TLyLineMark): TLyLine;
var
  I: integer;
begin
  if AMark <> lmNone then
    for I := 0 to Count - 1 do
    begin
      Result := GetItem(I);
      if Result.FLineMark = AMark then Exit;
    end;
  Result := nil;
end;

procedure TLyLineList.ClearLineMark;
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    GetItem(I).SetLineMark(lmNone);
end;

procedure TLyLineList.Delete(Index: integer);
var
  L: TLyLine;
begin
  BeginUpdate;
  try
    L := GetItem(Index);
    FEdit.FUpdate.SaveDelete(Index, L.FText);
    FItems.Delete(Index);
    L.FList := nil;
    L.Free;
    ResetIndex(Index);
  finally
    EndUpdate;
  end;
end;

procedure TLyLineList.ResetIndex(StartX: integer);
var
  I: integer;
  L: TLyLine;
begin
  for I := StartX to GetCount - 1 do
  begin
    L := GetItem(I);
    L.FIndex := I;
    L.FNeedPaint := true;
  end;
end;

function TLyLineList.GetText: TLyCodeString;
var
  L: TStrings;
begin
  L := GetStrings;
  try
    Result := StrToCode(L.Text);
  finally
    L.Free;
  end;
end;

function TLyLineList.Height: integer;
begin
  Result := GetCount * FEdit.FLineHeight;
end;

function TLyLineList.Has(LineX: integer): boolean;
begin
  Result := (LineX >= 0) and (LineX < Count);
end;

function TLyLineList.Has(LineX1, LineX2: integer): boolean;
begin
  Result := Has(LineX1) and Has(LineX2);
end;

function TLyLineList.HasInOrder(StartX, EndX: integer): boolean;
begin
  Result := (StartX <= EndX) and Has(StartX, EndX);
end;

function TLyLineList.TryGetLine(LineX: integer; var Line: TLyLine): boolean;
begin
  Result := Has(LineX);
  if Result then
    Line := GetItem(LineX) else
    Line := nil;
end;

function TLyLineList.Insert(X: integer; const S: TLyCodeString): TLyLine;
begin
  Result := nil;
  if FEdit.StatusOK then
  begin
    X := Max(0, Min(Count, X));
    BeginUpdate;
    try
      Result := TLyLine.Create;
      Result.FList := Self;
      Result.FText := S;
      FItems.Insert(X, Result);
      ResetIndex(X);
      Result.Change;
      FEdit.FUpdate.SaveAdd(X);
    finally
      EndUpdate;
    end;
  end;
end;

function TLyLineList.InsertAfter(L: TLyLine; const S: TLyCodeString): TLyLine;
begin
  Result := Insert(L.FIndex + 1, S);
end;

function TLyLineList.InsertBefore(L: TLyLine; const S: TLyCodeString): TLyLine;
begin
  Result := Insert(L.FIndex, S);
end;

procedure TLyLineList.ClearToOneEmptyLine;
begin
  BeginUpdate;
  try
    FEdit.Selection.Delete;
    if Count > 0 then
    begin
      First.SetText('');
      First.SetLineMark(lmNone);
      Delete(1, Count - 1);
    end
    else Add;
    FEdit.FCaret.MoveToHead(First);
  finally
    EndUpdate;
  end;
end;

procedure TLyLineList.LoadFromFile(const FileName: string);
var
  L: TStrings;
  X: string;
begin
  L := TStringList.Create;
  try
    {$IFDEF UNICODE}
    if MatchID(ExtractFileExt(FileName), ['.ls', '.rivi']) then
      L.LoadFromFile(FileName, TEncoding.UTF8) else
      L.LoadFromFile(FileName);
    {$ELSE}
    L.LoadFromFile(FileName);
    {$ENDIF}
    LoadFromStrings(L);
    X := ExtractFileExt(FileName);
    if FEdit.FAutoFileSyntax then
      if not FEdit.Syntax.Support(X) then
        FEdit.Syntax.SyntaxClass := FindSyntaxByFileExt(X);
  finally
    L.Free;
  end;
end;

procedure TLyLineList.LoadFromStream(AStream: TStream);
var
  L: TStrings;
begin
  L := TStringList.Create;
  try
    L.LoadFromStream(AStream);
    LoadFromStrings(L);
  finally
    L.Free;
  end;
end;

procedure TLyLineList.LoadFromStrings(Strings: TStrings);
begin
  BeginUpdate;
  try
    AssignStrings(Strings);
    FEdit.SetModified(false);
  finally
    EndUpdate;
  end;
end;

function TLyLineList.Width: integer;
var
  I: integer;
  L: TLyLine;
begin
  Result := 0;
  for I := 0 to GetCount - 1 do
  begin
    L := GetItem(I);
    if L.FWidth > Result then
      Result := L.FWidth;
  end;
end;

{ TLyUndoItem }

constructor TLyUndoItem.Create(AList: TLyUndoList);
var
  Edit: TLyCodeEdit;
begin
  FList := AList;
  FPrev := FList.FLast;
  FList.FLast := Self;
  Edit := FList.FEdit;
  FMagic := FList.FMagic;
  FCaret.Y := Edit.FCaret.FLineX;
  FCaret.X := Edit.FCaret.FTextX;
  FFrom.Y := Edit.FSelection.FLineX;
  FFrom.X := Edit.FSelection.FTextX;
  FStart.Y := Edit.FSelection.FStartLine;
  FStart.X := Edit.FSelection.FStartText;
  FEnd.Y := Edit.FSelection.FEndLine;
  FEnd.X := Edit.FSelection.FEndText;
end;

destructor TLyUndoItem.Destroy;
begin
  if FList <> nil then
    FList.FLast := FPrev;
  inherited;
end;

procedure TLyUndoItem.Apply;
var
  Edit: TLyCodeEdit;
begin
  try
    Edit := FList.FEdit;
    case FType of
      ctChangeText: Edit.FLines[FLine].SetText(FText);
      ctDeleteLine: Edit.FLines.Insert(FLine, FText);
      ctAddLine   : Edit.FLines.Delete(FLine);
    end;
    if (FPrev = nil) or (FPrev.FMagic <> FMagic) then
    begin
      Edit.FCaret.MoveTo(FCaret.Y, FCaret.X);
      Edit.FSelection.FLineX := FFrom.Y;
      Edit.FSelection.FTextX := FFrom.X;
      Edit.FSelection.Select(FStart.Y, FStart.X, FEnd.Y, FEnd.X);
    end;
  finally
    Free;
  end;
end;

{ TLyUndoList }

function TLyUndoList.GetMode: TLyChangeDeal;
begin
  if Self = FEdit.FUndos then Result := cdUndo else
  if Self = FEdit.FRedos then Result := cdRedo else Result := cdNone;
end;

procedure TLyUndoList.SetEnabled(Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Clear;
    if Self = FEdit.FUndos then FEdit.FRedos.Clear;
    FEdit.FRedos.FEnabled := FEdit.FUndos.FEnabled and FEdit.FRedos.FEnabled;
  end;
end;

constructor TLyUndoList.Create(AEdit: TLyCodeEdit);
begin
  FEdit := AEdit;
  FMagic := 0;
  FEnabled := true;
end;

destructor TLyUndoList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TLyUndoList.Clear;
begin
  FMagic := 0;
  while FLast <> nil do FLast.Free;
end;

procedure TLyUndoList.Apply;
var
  M: cardinal;
begin
  if (FLast <> nil) and (FEdit.FUpdate.FUndoMode = cdNone) then
  try
    FEdit.FUpdate.BeginUpdate;
    try
      FEdit.FUpdate.FUndoMode := Mode;
      M := FLast.FMagic;
      repeat FLast.Apply until (FLast = nil) or (FLast.FMagic <> M);
      FEdit.FCaret.MakeVisible;
    finally
      FEdit.FUpdate.EndUpdate;
    end;
  finally
    FEdit.FUpdate.FUndoMode := cdNone;
  end;
end;

{ TLyPalette }

constructor TLyPalette.Create;
begin
  FTextColor          := clWindowText;
  FBackground         := clWindow;
  FSelectedTextColor  := clWhite;
  FSelectedBackground := $694D00;
  FLeftBarBackground  := clBtnFace;
  FChangedBackground  := clLime;
  FActiveBackground   := clSkyBlue;
  FCaretColor         := FTextColor;
  FLine80Color        := clGray;
  FLineMarkBackground := $437E07;
  { token color }
  FUnknownColor       := clRed;
  FKeywordColor       := clNavy;
  FIDColor            := FTextColor;
  FSpaceColor         := FTextColor;
  FOperatorColor      := FTextColor;
  FDelimiterColor     := FTextColor;
  FNumberColor        := clBlue;
  FENumberColor       := clRed;
  FCharColor          := clBlue;
  FECharColor         := clRed;
  FStringColor        := clBlue;
  FEStringColor       := clRed;
  FCommentColor       := clGreen;
  FECommentColor      := clGreen;
  FECommentPColor     := clGreen;
  FDirectiveColor     := clTeal;
  FEDirectiveColor    := clTeal;
  { token style }
  FUnknownStyle       := [];
  FKeywordStyle       := [fsBold];
  FIDStyle            := [];
  FSpaceStyle         := [];
  FOperatorStyle      := [];
  FDelimiterStyle     := [];
  FNumberStyle        := [];
  FENumberStyle       := [];
  FCharStyle          := [];
  FECharStyle         := [];
  FStringStyle        := [];
  FEStringStyle       := [];
  FCommentStyle       := [];
  FECommentStyle      := [];
  FECommentPStyle     := [];
  FDirectiveStyle     := [];
  FEDirectiveStyle    := [];
end;

function TLyPalette.GetStyle(Token: TLyToken): TFontStyles;
begin
  Result := [];
  case Token of
    TK_UNKNOWN   : Result := FUnknownStyle;
    TK_KEYWORD   : Result := FKeywordStyle;
    TK_ID        : Result := FIDStyle;
    TK_SPACE     : Result := FSpaceStyle;
    TK_OPERATOR  : Result := FOperatorStyle;
    TK_DELIMITER : Result := FDelimiterStyle;
    TK_NUMBER    : Result := FNumberStyle;
    TK_ENUMBER   : Result := FENumberStyle;
    TK_CHAR      : Result := FCharStyle;
    TK_ECHAR     : Result := FECharStyle;
    TK_STRING    : Result := FStringStyle;
    TK_ESTRING   : Result := FEStringStyle;
    TK_COMMENT   : Result := FCommentStyle;
    TK_ECOMMENT  : Result := FECommentStyle;
    TK_ECOMMENTP : Result := FECommentPStyle;
    TK_DIRECTIVE : Result := FDirectiveStyle;
    TK_EDIRECTIVE: Result := FEDirectiveStyle;
  end;
end;

function TLyPalette.GetColor(Token: TLyToken): TColor;
begin
  Result := FSpaceColor;
  case Token of
    TK_UNKNOWN   : Result := FUnknownColor;
    TK_KEYWORD   : Result := FKeywordColor;
    TK_ID        : Result := FIDColor;
    TK_SPACE     : Result := FSpaceColor;
    TK_OPERATOR  : Result := FOperatorColor;
    TK_DELIMITER : Result := FDelimiterColor;
    TK_NUMBER    : Result := FNumberColor;
    TK_ENUMBER   : Result := FENumberColor;
    TK_CHAR      : Result := FCharColor;
    TK_ECHAR     : Result := FECharColor;
    TK_STRING    : Result := FStringColor;
    TK_ESTRING   : Result := FEStringColor;
    TK_COMMENT   : Result := FCommentColor;
    TK_ECOMMENT  : Result := FECommentColor;
    TK_ECOMMENTP : Result := FECommentPColor;
    TK_DIRECTIVE : Result := FDirectiveColor;
    TK_EDIRECTIVE: Result := FEDirectiveColor;
  end;
end;

{ TLySyntax }

function TLySyntax.ParseTo(const EndStr: TLyCodeString; Token, FailToken: TLyToken): TLyToken;
var
  I: integer;
begin
  if EndStr <> '' then
  begin
    I := PosEx(EndStr, FLine, FIndex);
    if I > 0 then
    begin
      FIndex := I + Length(EndStr) - 1;
      GetNextChar;
      Result := Token;
      Exit;
    end;
  end;
  Result := ParseToEnd(FailToken);
end;

function TLySyntax.ParseToEnd(Token: TLyToken): TLyToken;
begin
  FIndex := FSize + 1;
  GetNextChar;
  Result := Token;
end;

function TLySyntax.ParseBlockComment(const EndStr: TLyCodeString; FailToken: TLyToken): TLyToken;
begin
  Result := ParseTo(EndStr, TK_COMMENT, FailToken);
end;

function TLySyntax.ParseLineComment: TLyToken;
begin
  Result := ParseToEnd(TK_COMMENT);
end;

function TLySyntax.GetNextChar: boolean;
begin
  FChar := PeekChar;
  Result := (FChar <> #0);
  if Result then
    Inc(FIndex) else
    FIndex := FSize + 1;
end;

function TLySyntax.GetToken(Index: integer): TLyToken;
begin
//Result := FTokens[Index - 1] else
  Result := TK_SPACE;
end;

function TLySyntax.GetTokenColor(Token: TLyToken): TColor;
begin
  Result := FEdit.Palette.GetColor(Token);
end;

function TLySyntax.GetTokenSize: integer;
begin
  Result := FIndex - FTokenPos;
end;

function TLySyntax.GetClass: TLySyntaxClass;
begin
  Result := TLySyntaxClass(ClassType);
end;

procedure TLySyntax.SetClass(Value: TLySyntaxClass);
var
  I: integer;
begin
  if Value = nil then Value := TLySyntax;
  if Value <> ClassType then
  begin
    with FEdit do
    begin
      FUpdate.BeginUpdate;
      try
        FSyntax := Value.Create(FEdit);
        for I := 0 to FLines.Count - 1 do
        begin
          FSyntax.Decorate(FLines[I]);
          FLines[I].FNeedPaint := true;
        end;
      finally
        FUpdate.EndUpdate;
      end;
    end;
    Free;
  end;
end;

function TLySyntax.GetTokenStr: TLyCodeString;
begin
  Result := Copy(FLine, FTokenPos, FIndex - FTokenPos);
end;

function TLySyntax.GetTokenStyle(Token: TLyToken): TFontStyles;
begin
  Result := FEdit.Palette.GetStyle(Token);
end;

destructor TLySyntax.Destroy;
begin
  FLine := '';
//  SetLength(FTokens, 0);
  inherited;
end;

class function TLySyntax.Language: string;
begin
  Result := '';
end;

class function TLySyntax.FileExts: string;
begin
  Result := '';
end;

class function TLySyntax.DefFileExt: string;
begin
  Result := '';
end;

class function TLySyntax.CaseSensitive: boolean;
begin
  Result := false;
end;

function TLySyntax.IsKeyword(const ID: TLyCodeString): boolean;
begin
  Result := false;
end;

function TLySyntax.MatchStr(const S: TLyCodeString): boolean;
begin
  Result := (S <> '') and (S = Copy(FLine, FIndex, Length(S)));
end;

function TLySyntax.MatchText(const S: TLyCodeString): boolean;
begin
  Result := (S <> '') and SysUtils.SameText(CodeToStr(S),
    CodeToStr(Copy(FLine, FIndex, Length(S))));
end;

function TLySyntax.PeekChar: TLyCodeChar;
begin
  if FIndex < FSize then
  begin
    Result := FLine[FIndex + 1];
    if Result < ' ' then Result := ' ';
  end
  else Result := #0;
end;

function TLySyntax.SkipChars(Chars: TSysCharSet): integer;
begin
  if MatchChar(Chars - [#0]) then
    Result := SkipNextChars(Chars) + 1 else
    Result := 0;
end;

function TLySyntax.SkipNextChars(Chars: TSysCharSet): integer;
begin
  Result := 0;
  while GetNextChar and MatchChar(Chars) do Inc(Result);
end;

class function TLySyntax.Support(const FileExt: string): boolean;
var
  S, X: string;
begin
  Result := (FileExt <> '');
  if Result then
  begin
    Result := SameText(FileExt, DefFileExt);
    if not Result then
    begin
      S := FileExts;
      X := FetchFileExt(S);
      while X <> '' do
      begin
        Result := SameText(X, FileExt);
        if Result then Exit;
        X := FetchFileExt(S);
      end;
    end;
  end;
end;

function TLySyntax.ParseOne(Token: TLyToken): TLyToken;
begin
  GetNextChar;
  Result := Token;
end;

function TLySyntax.MatchChar(Chars: TSysCharSet): boolean;
begin
  Result := CharInSet(FChar, Chars);
end;

function TLySyntax.MatchPrior(AToken: TLyToken): boolean;
begin
  Result := (FPrior = AToken);
end;

function TLySyntax.MatchChar(C: TLyCodeChar): boolean;
begin
  Result := (FChar = C);
end;

function TLySyntax.MatchID(const ID: TLyCodeString;
  const IDList: array of TLyCodeString): boolean;
var
  I: integer;
begin
  for I := 0 to Length(IDList) - 1 do
    if MatchID(ID, IDList[I]) then
    begin
      Result := true;
      Exit;
    end;
  Result := false;
end;

function TLySyntax.MatchID(const ID1, ID2: TLyCodeString): boolean;
begin
  if CaseSensitive then
    Result := (ID1 = ID2) else
    Result := SysUtils.SameText(CodeToStr(ID1), CodeToStr(ID2));
end;

constructor TLySyntax.Create(AEdit: TLyCodeEdit);
begin
  FEdit := AEdit;
end;

function TLySyntax.Decorate(ALine: TLyLine): TLyToken;
var
  I: integer;
begin
  FPrior := ALine.PriorToken;
  if FPrior < 0 then
    FPrior := Decorate(ALine.Prior);
  FLine := ALine.FText;
  FSize := Length(FLine);
  SetLength(ALine.FTokens, FSize);
  FIndex := 0;
  FChar := #0;
  if GetNextChar then
  repeat
    FTokenPos := FIndex;
    FPrior := ParseNextToken;
    for I := FTokenPos to FIndex - 1 do
      ALine.FTokens[I - 1] := FPrior;
  until FChar = #0;
  FLine := '';
  ALine.FLastToken := FPrior;
  Result := FPrior;
end;

function TLySyntax.ParseNextToken: TLyToken;
begin
  Result := ParseToEnd(TK_SPACE);
end;

{ TPascalSyntax }

class function TPascalSyntax.DefFileExt: string;
begin
  Result := '.pas';
end;

class function TPascalSyntax.FileExts: string;
begin
  Result := '.pas .pp .dpr .lpr .inc';
end;

function TPascalSyntax.IsKeyword(const ID: TLyCodeString): boolean;
begin
  Result := MatchID(ID, PascalKeywords);
end;

class function TPascalSyntax.Language: string;
begin
  Result := 'Pascal';
end;

function TPascalSyntax.ParseChar: TLyToken;
begin
  Result := TK_CHAR;
  GetNextChar;
  if SkipChars(CS_DIGIT) = 0 then
    if not MatchChar('$') or (SkipNextChars(CS_HEX) = 0) then
      Result := TK_UNKNOWN;
  if SkipChars(CS_ID) > 0 then
    Result := TK_UNKNOWN;
end;

function TPascalSyntax.ParseComment: TLyToken;
begin
  Result := ParseBlockComment('}', TK_ECOMMENT);
end;

function TPascalSyntax.ParseCommentP: TLyToken;
begin
  Result := ParseBlockComment('*)', TK_ECOMMENTP);
end;

function TPascalSyntax.ParseDelimiter: TLyToken;
begin
  Result := ParseOne(TK_DELIMITER);
end;

function TPascalSyntax.ParseDirective: TLyToken;
begin
  Result := ParseTo('}', TK_DIRECTIVE, TK_EDIRECTIVE);
end;

function TPascalSyntax.ParseHex: TLyToken;
begin
  if SkipNextChars(CS_HEX) = 0 then
    Result := TK_UNKNOWN else
    Result := TK_NUMBER;
  if SkipChars(CS_ID) > 0 then
    Result := TK_UNKNOWN;
end;

function TPascalSyntax.ParseKeywordID: TLyToken;
begin
  SkipNextChars(CS_ID);
  if IsKeyword(TokenStr) then
    Result := TK_KEYWORD else
    Result := TK_ID;
end;

function TPascalSyntax.ParseNextToken: TLyToken;
begin
  if MatchPrior(TK_EDIRECTIVE) then Result := ParseDirective else
  if MatchPrior(TK_ECOMMENT)   then Result := ParseComment else
  if MatchPrior(TK_ECOMMENTP)  then Result := ParseCommentP else
  if MatchStr('{$')            then Result := ParseDirective else
  if MatchChar('{')            then Result := ParseComment else
  if MatchStr('(*')            then Result := ParseCommentP else
  if MatchStr('//')            then Result := ParseLineComment else
  if MatchChar(CS_SPACE)       then Result := ParseSpace else
  if MatchChar(CS_HEAD)        then Result := ParseKeywordID else
  if MatchChar('''')           then Result := ParseString else
  if MatchChar('"')            then Result := ParseUnknown else
  if MatchChar('#')            then Result := ParseChar else
  if MatchChar('$')            then Result := ParseHex else
  if MatchChar(CS_DIGIT)       then Result := ParseNumber else
  if MatchChar(':')            then Result := ParseOperator else
  if MatchChar(CS_OPERATOR)    then Result := ParseOperator else
  if MatchChar(CS_DELIMITER)   then Result := ParseDelimiter else
                                    Result := ParseUnknown;
end;

function TPascalSyntax.ParseNumber: TLyToken;
begin
  SkipNextChars(CS_DIGIT);
  if MatchChar('.') and CharInSet(PeekChar, CS_DIGIT) then
    SkipNextChars(CS_DIGIT);
  Result := TK_NUMBER;
end;

function TPascalSyntax.ParseOperator: TLyToken;
begin
  Result := ParseOne(TK_OPERATOR);
end;

function TPascalSyntax.ParseSpace: TLyToken;
begin
  SkipNextChars(CS_SPACE);
  Result := TK_SPACE;
end;

function TPascalSyntax.ParseString: TLyToken;
begin
  Result := TK_STRING;
  repeat
    while GetNextChar and (FChar <> '''') do;
    if MatchChar(#0) then
      Result := TK_UNKNOWN else
      GetNextChar;
  until not MatchChar('''');
end;

function TPascalSyntax.ParseUnknown: TLyToken;
begin
  Result := ParseOne(TK_UNKNOWN);
end;

{ TLyseeSyntax }

class function TLyseeSyntax.DefFileExt: string;
begin
  Result := '.ls';
end;

class function TLyseeSyntax.FileExts: string;
begin
  Result := '.ls';
end;

function TLyseeSyntax.GetTokenColor(Token: TLyToken): TColor;
begin
  if Token in [TK_ESTRINGL, TK_ESTRING] then Token := TK_STRING;
  Result := inherited GetTokenColor(Token);
end;

function TLyseeSyntax.IsKeyword(const ID: TLyCodeString): boolean;
begin
  Result := MatchID(ID, LyseeKeywords);
end;

class function TLyseeSyntax.Language: string;
begin
  Result := 'Lysee';
end;

function TLyseeSyntax.ParseNextToken: TLyToken;
begin
  if MatchPrior(TK_ECOMMENT)  then Result := ParseComment else
  if MatchPrior(TK_ECOMMENTP) then Result := ParseCommentP else
  if MatchPrior(TK_ESTRING)   then Result := ParseString else
  if MatchPrior(TK_ESTRINGL)  then Result := ParseStringL else
  if MatchChar('{')           then Result := ParseComment else
  if MatchStr('(*')           then Result := ParseCommentP else
  if MatchChar('''')          then Result := ParseString else
  if MatchChar('"')           then Result := ParseStringL else
  if MatchStr('//')           then Result := ParseLineComment else
  if MatchChar(CS_SPACE)      then Result := ParseSpace else
  if MatchChar(CS_HEAD)       then Result := ParseKeywordID else
  if MatchChar('#')           then Result := ParseChar else
  if MatchChar('$')           then Result := ParseHex else
  if MatchChar(CS_DIGIT)      then Result := ParseNumber else
  if MatchChar(':')           then Result := ParseOperator else
  if MatchChar(CS_OPERATOR)   then Result := ParseOperator else
  if MatchChar(CS_DELIMITER)  then Result := ParseDelimiter else
                                   Result := ParseUnknown;
end;

function TLyseeSyntax.ParseString: TLyToken;
begin
  Result := TK_STRING;
  repeat
    while GetNextChar and (FChar <> '''') do;
    if MatchChar(#0) then
      Result := TK_ESTRING else
      GetNextChar;
  until not MatchChar('''');
end;

function TLyseeSyntax.ParseStringL: TLyToken;
begin
  Result := TK_STRING;
  repeat
    while GetNextChar and not MatchChar('"') do
      if MatchChar('\') then GetNextChar;
    if MatchChar(#0) then
      Result := TK_ESTRINGL else
      GetNextChar;
  until not MatchChar('"');
end;

initialization
begin
  SyntaxClasses.Add(TLySyntax);
  SyntaxClasses.Add(TPascalSyntax);
  SyntaxClasses.Add(TLyseeSyntax);
end;

end.
