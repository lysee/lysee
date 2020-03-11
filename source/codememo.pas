unit CodeMemo;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  {$IFDEF FPC}LCLType, LCLIntf,{$ELSE}UITypes, Messages,{$ENDIF}
  SysUtils, Classes, Controls, Graphics, StdCtrls, ExtCtrls, Forms,
  Types, UIDefs, Basic;

const

  CM_VERSION   = '0.7.0';
  CM_FONTNAME  = 'Courier New';
  CM_FONTSIZE  = 10;
  CM_TABSIZE   = 4;
  CM_LBDELTA   = 5;
  CM_HOMEPOS   = 1;

type

  TLyCodeMemo  = class;{forward}
  TLyCaret     = class;
  TLyUpdate    = class;
  TLySelection = class;
  TLyLine      = class;
  TLyLineList  = class;
  TLyGutter    = class;
  TLyUndoItem  = class;
  TLyUndoList  = class;
  TLyHilighter = class;

  { TLyCodeMemo }

  TLyChangeType = (ctChangeText, ctDeleteLine, ctAddLine);
  TLyChangeDeal = (cdNone, cdUndo, cdRedo);

  TLyMemoState = (msModified, msReadonly, msUndoRedo, msWrapLine, msLine80,
                  msGutter, msResizing, msFonting, msDestroying);
  TLyMemoStates = set of TLyMemoState;

  TLyCodeMemo = class(TCustomControl)
  private
    FState: TLyMemoStates;
    FLines: TLyLineList;
    FCaret: TLyCaret;
    FUpdate: TLyUpdate;
    FSelection: TLySelection;
    FScrollBar: TScrollBar;
    FHilighter: TLyHilighter;
    FGutter: TLyGutter;
    FMargin: integer; // left & right
    FLastBodyWidth: integer;
    FBodyWidth: integer;
    FBodyHeight: integer;
    FBodyLines: integer;
    FTextHeight: integer;
    FLineHeight: integer;
    FLineSpacing: integer;
    FSpaceWidth: integer;
    FOnStatus: TNotifyEvent;
    FOnChange: TNotifyEvent;
    function GetState(X: TLyMemoState): boolean;
    function GetUpdating: boolean;
    procedure SetState(X: TLyMemoState; Value: boolean);
    function GetCanUndo: boolean;
    function GetCanRedo: boolean;
    function GetSelAvail: boolean;
    function GetSelText: WideString;
    procedure SetSelText(const Value: WideString);
    function GetCaretY: integer;
    function GetCaretX: integer;
    function GetLineCount: integer;
    function GetScrollBar: TScrollBar;
    function StatusOK: boolean;
  protected
    { message }
    procedure WMSetFocus(var Msg: TWMSetFocus);message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus);message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode);message WM_GETDLGCODE;
    { mouse }
    procedure DblClick;override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): boolean;override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): boolean;override;
    { keyboard }
    procedure KeyDown(var Key: Word; Shift: TShiftState);override;
    procedure KeyUp(var Key: Word; Shift: TShiftState);override;
    {$IFDEF FPC}
    procedure UTF8KeyPress(var Key: TUTF8Char);override;
    {$ELSE}
    procedure KeyPress(var Key: char);override;
    {$ENDIF}
    { resize }
    procedure Resize;override;
    function ResizeFrom(ItemX: integer; OnlyChanged: boolean): integer;
    function EditAreaWidth: integer;
    procedure CheckFontStatus;
    procedure FontChange(Sender: TObject);
    { display }
    procedure Paint;override;
    procedure PaintFrom(AItemX: integer; OnlyChanged: boolean);
    procedure PaintLine80;
    procedure SetCanvas(Selected: boolean);
    procedure ScrollDown(Delta: integer);
    procedure ScrollUp(Delta: integer);
    procedure ScrollBarChange(Sender: TObject);
    { position }
    function HitLine(Y: integer): integer;
    function HitText(X, Y: integer; var TextX: integer): TLyLine;
    function TopOffset: integer;
    function TopCaretY: integer;
    function GetTopLine(var ItemX, LineX: integer): boolean;
    procedure MakeVisible(LineX: integer);
    procedure NotifyStatus;
    procedure NotifyChange;
    procedure CreateParams(var Params: TCreateParams);override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Keyboard(Key: Word; Shift: TShiftState);overload;
    procedure Keyboard(Key: WideChar);overload;
    procedure SelectAll;
    procedure UnSelect;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure MakeCaretVisible;
    procedure Print;
    procedure Undo;
    procedure Redo;
    function FindNext(const S: WideString; Rewind: boolean = true): boolean;
    property Font;
    property Lines: TLyLineList read FLines;
    property SelAvail: boolean read GetSelAvail;
    property SelText: WideString read GetSelText write SetSelText;
    property Hilighter: TLyHilighter read FHilighter;
    property CanUndo: boolean read GetCanUndo;
    property CanRedo: boolean read GetCanRedo;
    property LineHeight: integer read FLineHeight;
    property LineCount: integer read GetLineCount;
    property CaretY: integer read GetCaretY;
    property CaretX: integer read GetCaretX;
    property ScrollBar: TScrollBar read GetScrollBar;
    property Readonly: boolean index msReadonly read GetState write SetState;
    property Modified: boolean index msModified read GetState write SetState;
    property WrapLine: boolean index msWrapLine read GetState write SetState;
    property ShowGutter: boolean index msGutter read GetState write SetState;
    property ShowLine80: boolean index msLine80 read GetState write SetState;
    property UndoRedo: boolean index msUndoRedo read GetState write SetState;
    property Updating: boolean read GetUpdating;
    property OnStatus: TNotifyEvent read FOnStatus write FOnStatus;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TLyUpdate }

  TLyUpdate = class(TLyKeyDown)
  private
    FMemo: TLyCodeMemo;
    FUndos: TLyUndoList;
    FRedos: TLyUndoList;
    FCount: integer;
    FChanged: boolean;
    FEndUpdating: boolean;
    FUndoMode: TLyChangeDeal;
    FMagic: cardinal;
    FOrgCaretY: integer;
    FChangedItemX: integer;
    FChangedLines: integer;
    FAddedLines: integer;
    FDeletedLines: integer;
    function GetMemoOK: boolean;
    function GetCaret: TLyCaret;
    function GetLines: TLyLineList;
    function GetReadonly: boolean;
    function GetSelection: TLySelection;
    function GetUpdating: boolean;
    function GetCanRedo: boolean;
    function GetCanUndo: boolean;
  protected
    procedure KeyRefresh(AMemo: TObject; Shift: TShiftState);    // F5
    procedure KeySelectAll(AMemo: TObject; Shift: TShiftState);  // Ctrl-A
    procedure KeyCopy(AMemo: TObject; Shift: TShiftState);       // Ctrl-C
    procedure KeyCut(AMemo: TObject; Shift: TShiftState);        // Ctrl-X/Shift-DEL
    procedure KeyPaste(AMemo: TObject; Shift: TShiftState);      // Ctrl-V/Shift-INS
    procedure KeyLeft(AMemo: TObject; Shift: TShiftState);       // LEFT
    procedure KeyRight(AMemo: TObject; Shift: TShiftState);      // RIGHT
    procedure KeyUp(AMemo: TObject; Shift: TShiftState);         // UP
    procedure KeyDown(AMemo: TObject; Shift: TShiftState);       // DOWN
    procedure KeyPageUp(AMemo: TObject; Shift: TShiftState);     // PRIOR
    procedure KeyPageDown(AMemo: TObject; Shift: TShiftState);   // NEXT
    procedure KeyHome(AMemo: TObject; Shift: TShiftState);       // HOME
    procedure KeyEnd(AMemo: TObject; Shift: TShiftState);        // END
    procedure KeyBack(AMemo: TObject; Shift: TShiftState);       // BACK
    procedure KeyDelete(AMemo: TObject; Shift: TShiftState);     // DELETE
    procedure KeyDeleteLine(AMemo: TObject; Shift: TShiftState); // Ctrl-Y
    procedure KeyUndo(AMemo: TObject; Shift: TShiftState);       // Ctrl-Z
    procedure KeyRedo(AMemo: TObject; Shift: TShiftState);       // Shift-Ctrl-Z
  protected
    procedure UndoSaveChange(ItemX: integer; const S: WideString);
    procedure UndoSaveDelete(ItemX: integer; const S: WideString);
    procedure UndoSaveAdd(ItemX: integer);
  public
    constructor Create(ASender: TObject);override;
    destructor Destroy;override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Undo;
    procedure Redo;
    property MemoOK: boolean read GetMemoOK;
    property Readonly: boolean read GetReadonly;
    property Updating: boolean read GetUpdating;
    property EndUpdating: boolean read FEndUpdating;
    property Selection: TLySelection read GetSelection;
    property Caret: TLyCaret read GetCaret;
    property Lines: TLyLineList read GetLines;
    property CanUndo: boolean read GetCanUndo;
    property CanRedo: boolean read GetCanRedo;
  end;

  { TLyCaret }

  TLyCaret = class
  private
    FMemo: TLyCodeMemo;
    FMoveCount: integer;
    FItemX: integer;
    FTextX: integer;
    FPos: TPoint;
    FWidth: integer;
    FHeight: integer;
    FCreated: boolean;
    FShowing: boolean;
    function GetVisible: boolean;
    function GetActive: boolean;
    function GetItem: TLyLine;
    function GetAtTail: boolean;
  protected
    procedure BeginMove;
    procedure EndMove;
    procedure DestroyCaret;
    procedure HideCaret;
    function CreateCaret(Width, Height: integer): boolean;
    function ShowCaret: boolean;
    procedure MoveTo(ItemX, TextX: integer);
    procedure MoveToHead(Item: TLyLine);
    procedure MoveToTail(Item: TLyLine);
    procedure MoveToPrev(Item: TLyLine);
    procedure MoveToNext(Item: TLyLine);
    procedure MoveToSelEnd;
    procedure MoveCaret;
    procedure Insert(Strings: TStrings);overload;
    procedure Insert(const Text: WideString);overload;
    procedure MoveAfterInsert(Line: TLyLine; TextX: integer; const S: WideString);
    function ScrollUp(LineCount: integer; var TX: integer): TLyLine;
    function ScrollDown(LineCount: integer; var TX: integer): TLyLine;
    function PrevPos(var TX: integer; AWord: boolean): TLyLine;
    function NextPos(var TX: integer; AWord: boolean): TLyLine;
  public
    constructor Create(AMemo: TLyCodeMemo);
    procedure MakeVisible;
    property Active: boolean read GetActive;
    property Visible: boolean read GetVisible;
    property Showing: boolean read FShowing;
    property Item: TLyLine read GetItem;
    property ItemIndex: integer read FItemX;
    property TextIndex: integer read FTextX;
    property AtTail: boolean read GetAtTail;
    property Pos: TPoint read FPos;
  end;

  { RLyTextPos }

  RLyTextPos = packed record
    ItemX: integer;
    TextX: integer;
  end;
  PLyTextPos = ^RLyTextPos;

  { TLySelection }

  TLySelection = class
  private
    FMemo: TLyCodeMemo;
    FSelectAll: boolean;
    FFrom: RLyTextPos;
    FHead: RLyTextPos;
    FTail: RLyTextPos;
    FSelecting: boolean;
    function GetNotSelected: boolean;
    function GetSelected: boolean;
    procedure SetSelected(Value: boolean);
    procedure SetSelectAll(Value: boolean);
    function GetPartialSelected: boolean;
    function GetText: WideString;
    procedure SetText(const Value: WideString);
  protected
    procedure Reset;
    procedure MouseDown(P: TLyLine; TextX: integer);overload;
    procedure MouseMove(P: TLyLine; TextX: integer);overload;
    function Select(HI, HX, TI, TX: integer): boolean;
    function SelectTo(ItemX, TextX: integer): boolean;
    function Contains(ItemX, TextX: integer): boolean;overload;
    function Contains(ItemX: integer): boolean;overload;
    function GetRange(ItemX: integer; var X1, X2: integer): boolean;
    function ActiveParagraph: TLyLine;
    function ActualSelectAll: boolean;
  public
    constructor Create(AMemo: TLyCodeMemo);
    procedure UnSelect;
    procedure Delete;
    procedure SelectAll;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure MarkCode(IsCode: boolean);overload;
    procedure MarkCode(IsCode: boolean; BegLineX, EndLineX: integer);overload;
    property Selected: boolean read GetSelected write SetSelected;
    property NotSelected: boolean read GetNotSelected;
    property PartialSelected: boolean read GetPartialSelected;
    property Text: WideString read GetText write SetText;
    property StartPos: RLyTextPos read FHead;
    property EndPos: RLyTextPos read FTail;
    property AllSelected: boolean read FSelectAll write SetSelectAll;
  end;

  { TLyLineList }

  TLyLineList = class(TPersistent)
  private
    FMemo: TLyCodeMemo;
    FItems: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TLyLine;
    function GetText: WideString;
    procedure SetText(const Value: WideString);
    function GetFirst: TLyLine;
    function GetLast: TLyLine;
    function GetHeight: integer;
    function GetLineCount: integer;
  protected
    procedure DecorateFrom(StartX: integer = 0);
    procedure SetChanged(Value: boolean);
    procedure SetModified(Value: boolean);
    procedure SetNeedPaint(Value: boolean);
    procedure AssignTo(Source: TPersistent);override;
  public
    constructor Create(AMemo: TLyCodeMemo);
    destructor Destroy;override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure LoadFromFile(const FileName: string); // UTF8
    procedure LoadFromStream(const AStream: TStream); // UTF8
    procedure SaveToFile(const FileName: string); // UTF8
    procedure SaveToStream(const AStream: TStream); // UTF8
    procedure Assign(Source: TPersistent);override;
    procedure Clear;
    procedure Delete(Index: integer);overload;
    procedure Delete(BegIndex, EndIndex: integer);overload;
    function Add(const S: WideString = ''): TLyLine;
    function Insert(Index: integer; const S: WideString = ''): TLyLine;
    function InsertAfter(P: TLyLine; const S: WideString = ''): TLyLine;
    function IndexOf(P: TLyLine): integer;
    function IndexByLine(var LineIndex: integer): integer;
    function FindByLine(var LineIndex: integer; Anyway: boolean): TLyLine;
    function Has(Index: integer): boolean;
    function GetCaretLine(CaretY: integer; var LineX: integer): TLyLine;
    property Count: integer read GetCount;
    property Items[Index: integer]: TLyLine read GetItem;default;
    property Text: WideString read GetText write SetText;
    property First: TLyLine read GetFirst;
    property Last: TLyLine read GetLast;
    property LineCount: integer read GetLineCount;
    property Height: integer read GetHeight;
  end;

  { TLyLine }

  TLyLine = class
  private
    FList: TLyLineList;
    FText: WideString;
    FTop: integer;
    FL: array of integer;
    FW: array of integer;
    FModified: boolean;
    FChanged: boolean;
    FNeedPaint: boolean;
    FTokens: array of TLyTokenID;
    FLastToken: TLyTokenID;
    procedure SetText(const Value: WideString);
    function GetIndex: integer;
    function GetHeight: integer;
    function StartLine: integer;
    function LineCount: integer;
    function GetPriorToken: TLyTokenID;
    function GetLeft: integer;
    function GetCaretY: integer;
  protected
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Resize;
    function TextOut(X, Y: integer; const S: WideString): boolean;
    function OutText(X, Y: integer; const S: WideString): integer;
    function TextWidth(const S: WideString): integer;
    function Add(const S: WideString): integer;
    function Delete(TextX: integer; Count: integer = 1): integer;
    function Insert(TextX: integer; const S: WideString): integer;
    function CutFrom(TextX: integer): WideString;
    function BreakFrom(TextX: integer): TLyLine;
    function BreakFromTail: TLyLine;
    function MergeNext: boolean;
    function Merge(P: TLyLine): boolean;
    function HitCaret(X: integer): integer;
    function TextPos(TextX: integer): TPoint;
    function TextLine(TextX: integer): integer;overload;
    function TextLine(TextX: integer; var Col: integer): integer;overload;
    function FirstText(LineX: integer): integer;
    function HitText(X, LineX: integer): integer;
    function HeadTextX: integer;
    function TailTextX: integer;
    function Prev: TLyLine;
    function Next: TLyLine;
    function LeadingSpaces: WideString;
    function LeadingSpaceCount: integer;
    function SeekPriorWord(TextX: integer): integer;
    function SeekNextWord(TextX: integer): integer;
    function SeekCurrWord(TextX: integer; var Len: integer): integer;
    procedure DrawLine(LineX, X, Y, BegSel, EndSel: integer);
  public
    constructor Create(Lines: TLyLineList);
    destructor Destroy;override;
    procedure Assign(P: TLyLine);
    procedure Change;
    procedure Paint;
    procedure PaintAt(X, Y: integer);
    procedure PaintLine(LineX: integer);
    property Text: WideString read FText write SetText;
    property Left: integer read GetLeft;
    property Top: integer read FTop;
    property Height: integer read GetHeight;
    property CaretY: integer read GetCaretY;
    property Index: integer read GetIndex;
    property LastToken: TLyTokenID read FLastToken;
    property PriorToken: TLyTokenID read GetPriorToken;
  end;

  { TLyGutter }

  TLyGutter = class(TCustomControl)
  private
    FMemo: TLyCodeMemo;
    FTextHeight: integer;
    function GetLineHeight: integer;
    function GetVertLinePos: integer;
  protected
    procedure Paint;override;
    procedure DrawLine(LineX: integer);
    function AdjustWidth: integer;
  public
    constructor Create(AOwner: TComponent);override;
    property LineHeight: integer read GetLineHeight;
    property VertLinePos: integer read GetVertLinePos;
  end;

  { TLyUndoItem }

  TLyUndoItem = class
  private
    FList: TLyUndoList;
    FPrev: TLyUndoItem;
    FType: TLyChangeType;
    FMagic: cardinal;
    FLine: integer;
    FText: WideString;
    FCaret: TPoint;
    FFrom: RLyTextPos;
    FStart: RLyTextPos;
    FEnd: RLyTextPos;
    procedure Apply;
  public
    constructor Create(AList: TLyUndoList);
    destructor Destroy;override;
    property Prev: TLyUndoItem read FPrev;
    property Line: integer read FLine;
    property Text: WideString read FText;
    property ChangeType: TLyChangeType read FType;
    property Magic: cardinal read FMagic;
  end;

  { TLyUndoList }

  TLyUndoList = class
  private
    FMemo: TLyCodeMemo;
    FUpdate: TLyUpdate;
    FLast: TLyUndoItem;
    function GetMode: TLyChangeDeal;
  public
    constructor Create(AUpdate: TLyUpdate);
    destructor Destroy;override;
    procedure Clear;
    procedure Apply;
    property Mode: TLyChangeDeal read GetMode;
  end;

  { TLyHilighter }

  TLyHilighterClass = class of TLyHilighter;

  TLyHilighter = class
  private
    FMemo: TLyCodeMemo;
    FText: WideString;
    FSize: integer;
    FPosition: integer;
    FChar: WideChar;
    FTokenPos: integer;
    FTokenSize: integer;
    FPrior: TLyTokenID;
    function GetTokenSize: integer;
    function GetTokenStr: WideString;
    function GetClass: TLyHilighterClass;
    procedure SetClass(Value: TLyHilighterClass);
    procedure SetText(const Value: WideString);
    function GetEnded: boolean;
    function GetNotEnded: boolean;
  protected
    procedure Clear;virtual;
    procedure Stop;virtual;
    function First: boolean;virtual;
    function Next: boolean;overload;virtual;
    function GetChar: WideChar;virtual;
    { peek }
    function PeekNextChar: WideChar;
    function PeekNextString(Len: integer): WideString;
    function PeekString(Len: integer): WideString;
    { match }
    function MatchPrior(AToken: TLyTokenID): boolean;
    function MatchChar(Chars: TSysCharSet): boolean;overload;
    function MatchChar(Ch: WideChar): boolean;overload;
    function MatchString(const S: WideString): boolean;
    function MatchText(const S: WideString): boolean;
    function MatchSpace: boolean;
    function MatchHead: boolean;
    function MatchDigit: boolean;
    { seek }
    function Seek(Chars: TSysCharSet): boolean;overload;
    function Seek(Ch: WideChar): boolean;overload;
    function SeekSpace: boolean;
    function SeekLineBreak: boolean;
    { skip }
    function Skip(Count: integer): boolean;overload;
    function Skip(Chars: TSysCharSet): boolean;overload;
    function Skip(Ch: WideChar): boolean;overload;
    function SkipSpace: boolean;
    function SkipLineBreak: boolean;
    function SkipPString(OnQuote: boolean): boolean;
    function SkipCString(OnQuote: boolean): boolean;
    function SkipString: boolean;overload;
    function SkipString(Endc: WideChar): boolean;overload;
    function SkipTag(Endc: WideChar): boolean;
    function SkipChars(Chars: TSysCharSet): integer;
    function SkipNextChars(Chars: TSysCharSet): integer;
    { pick }
    function Pick(Chars: TSysCharSet): WideString;
    function PickID: WideString;
    function PickExID: WideString;
    function PickHex: WideString;
    function PickOctal: WideString;
    function PickDecimal: WideString;
    { parse }
    function ParsePString(var S: WideString): boolean;
    function ParseCString(var S: WideString): boolean;
    function ParseString(var S: WideString): boolean;
    function ParsePChar(var C: WideChar): boolean;
    function ParseCChar(var C: WideChar): boolean;
    function ParseChar(var C: WideChar): boolean;
    function ParseEscChar(var C: WideChar): boolean;
    function ParseHex(var I: cardinal; N: integer = MaxInt): integer;
    function ParseOctal(var I: cardinal; N: integer = MaxInt): integer;
    function ParseDecimal(var I: cardinal; N: integer = MaxInt): integer;
    function ParseToEnd(Token: TLyTokenID): TLyTokenID;
    function ParseOne(Token: TLyTokenID): TLyTokenID;
    function ParseTo(const EndStr: WideString; Token, FailToken: TLyTokenID): TLyTokenID;
    function ParseBlockComment(const EndStr: WideString; FailToken: TLyTokenID): TLyTokenID;
    function ParseLineComment: TLyTokenID;
    function ParseSpace: TLyTokenID;
  public
    constructor Create(AMemo: TLyCodeMemo);virtual;
    class function Language: string;virtual;
    class function FileExts: string;virtual;
    class function DefFileExt: string;virtual;
    class function Support(const FileExt: string): boolean;virtual;
    class function CaseSensitive: boolean;virtual;
    class function WordWrap: boolean;virtual;
    function Decorate(ALine: TLyLine): TLyTokenID;virtual;
    function GetNextToken: TLyTokenID;virtual;
    function GetTokenColor(Token: TLyTokenID): TColor;virtual;
    function GetTokenStyle(Token: TLyTokenID): TFontStyles;virtual;
    function IsKeyword(const ID: string): boolean;virtual;
    property Text: WideString read FText write SetText;
    property Size: integer read FSize;
    property Position: integer read FPosition;
    property Curr: WideChar read FChar;
    property Ended: boolean read GetEnded;
    property NotEnded: boolean read GetNotEnded;
    property Prior: TLyTokenID read FPrior write FPrior;
    property TokenPos: integer read FTokenPos;
    property TokenSize: integer read GetTokenSize;
    property TokenStr: WideString read GetTokenStr;
    property HilightClass: TLyHilighterClass read GetClass write SetClass;
  end;

  { TLyPascalHilighter }

  TLyPascalHilighter = class(TLyHilighter)
  protected
    function ParseDirective: TLyTokenID;virtual;
    function ParseComment: TLyTokenID;virtual;
    function ParseCommentP: TLyTokenID;virtual;
    function ParseString: TLyTokenID;virtual;
    function ParseChar: TLyTokenID;virtual;
    function ParseNumber: TLyTokenID;virtual;
    function ParseHex: TLyTokenID;virtual;
    function ParseKeywordID: TLyTokenID;virtual;
    function ParseDelimiter: TLyTokenID;virtual;
    function ParseOperator: TLyTokenID;virtual;
    function ParseUnknown: TLyTokenID;virtual;
  public
    class function Language: string;override;
    class function FileExts: string;override;
    class function DefFileExt: string;override;
    class function WordWrap: boolean;override;
    function GetNextToken: TLyTokenID;override;
    function IsKeyword(const ID: string): boolean;override;
  end;

  { TLyseeHilighter }

  TLyseeHilighter = class(TLyPascalHilighter)
  protected
    function ParseString: TLyTokenID;override;
    function ParseStringL: TLyTokenID;
  public
    class function Language: string;override;
    class function FileExts: string;override;
    class function DefFileExt: string;override;
    function GetNextToken: TLyTokenID;override;
    function GetTokenColor(Token: TLyTokenID): TColor;override;
    function IsKeyword(const ID: string): boolean;override;
  end;

  { TRiviHilighter }

  TLyRiviLine = (rlUnknown, rlDirective, rlScript, rlParagraph);

  { TLyRiviHilighter }

  TLyRiviHilighter = class(TLyseeHilighter)
  private
    FType: TLyRiviLine;
    function ParseParagraph: TLyTokenID;
    function ParseFirst: TLyTokenID;
  public
    class function Language: string;override;
    class function FileExts: string;override;
    class function DefFileExt: string;override;
    class function WordWrap: boolean;override;
    function Decorate(ALine: TLyLine): TLyTokenID;override;
    function GetNextToken: TLyTokenID;override;
    function IsKeyword(const ID: string): boolean;override;
  end;

  { TLyCHilighter }

  TLyCHilighter = class(TLyPascalHilighter)
  protected
    function ParseComment: TLyTokenID;override;
    function ParseString: TLyTokenID;override;
    function ParseChar: TLyTokenID;override;
    function ParseHex: TLyTokenID;override;
    function ParseOctal: TLyTokenID;virtual;
    function ParseBinary: TLyTokenID;virtual;
  public
    class function Language: string;override;
    class function FileExts: string;override;
    class function DefFileExt: string;override;
    class function CaseSensitive: boolean;override;
    class function WordWrap: boolean;override;
    function GetNextToken: TLyTokenID;override;
    function IsKeyword(const ID: string): boolean;override;
  end;

  { TLyCppHilighter }

  TLyCppHilighter = class(TLyCHilighter)
  public
    class function Language: string;override;
    class function FileExts: string;override;
    class function DefFileExt: string;override;
    function IsKeyword(const ID: string): boolean;override;
  end;

  { TLyJavaHilighter }

  TLyJavaHilighter = class(TLyCHilighter)
  public
    class function Language: string;override;
    class function FileExts: string;override;
    class function DefFileExt: string;override;
    function IsKeyword(const ID: string): boolean;override;
  end;

function HilightClasses: TList;
function FindHilight(const Language: string): TLyHilighterClass;
function FindHilightByFileExt(const FileExt: string): TLyHilighterClass;
function FindFileHilight(const FileName: string): TLyHilighterClass;
function PlaceACodeMemo(Host: TWinControl): TLyCodeMemo;
function KeyStateOK: boolean;
function IsCtrlShift(S: TShiftState): boolean;
function TabSpaces(TextX, TabSize: integer): WideString;
function HasTextFormat: boolean;
function FetchFileExt(var FileExts: string): string;
function PosEx(const SubStr, Str: WideString; Index: integer): integer;

implementation

uses
  Math, Clipbrd, Printers, Menus;

var
  my_Hilightes: TList;

const

  { pascal }

  PascalKeywords: array[0..110] of string = (
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
    'safecall', 'sealed', 'set', 'shl', 'shr', 'stdcall', 'stored', 'WideString',
    'stringresource', 'then', 'threadvar', 'to', 'try', 'type', 'unit', 'until',
    'uses', 'var', 'virtual', 'while', 'with', 'write', 'writeonly', 'xor');

  { lysee }

  TK_ESTRINGL = TK_CUSTOM;

  LyseeKeywords: array[0..51] of string = (
    'and', 'array', 'as', 'begin', 'boolean', 'case', 'class', 'const', 'def',
    'div', 'do', 'downto', 'elif', 'else', 'end', 'except', 'false', 'finally',
    'for', 'get', 'if', 'in', 'inherited', 'is', 'like', 'main', 'mod', 'nil',
    'not', 'object', 'of', 'or', 'raise', 'repeat', 'result', 'self', 'set', 'shl',
    'shr', 'WideString', 'system', 'then', 'to', 'true', 'try', 'type', 'until',
    'uses', 'var', 'variant', 'while', 'xor');

  { C }

  CKeywords: array[0..43] of string = (
    'auto', 'break', 'case', 'char', 'const', 'continue', 'default', 'do',
    'double', 'else', 'enum', 'extern', 'float', 'for', 'goto', 'if',
    'inline', 'int', 'long', 'register', 'restrict', 'return', 'short',
    'signed', 'sizeof', 'static', 'struct', 'switch', 'typedef', 'union',
    'unsigned', 'void', 'volatile', 'while', '_Alignas', '_Alignof',
    '_Atomic', '_Bool', '_Complex', '_Generic', '_Imaginary', '_Noreturn',
    '_Static_assert', '_Thread_local');

  { C++ }

  CppKeywords: array[0..94] of string = (
    'asm', 'auto', 'bool', 'break', 'case', 'catch', 'cdecl', 'char', 'class',
    'const', 'const_cast', 'continue', 'default', 'delete', 'do', 'double',
    'dynamic_cast', 'else', 'enum', 'explicit', 'extern', 'false', 'float',
    'for', 'friend', 'goto', 'if', 'inline', 'int', 'interface', 'long',
    'mutable', 'namespace', 'new', 'operator', 'pascal', 'private', 'protected',
    'public', 'register', 'reinterpret_cast', 'return', 'short', 'signed',
    'sizeof', 'static', 'static_cast', 'struct', 'switch', 'template', 'this',
    'throw', 'true', 'try', 'typedef', 'typeid', 'typename', 'union',
    'unsigned', 'using', 'virtual', 'void', 'volatile', 'wchar_t', 'while',
    '__asm', '__automated', '__cdecl', '__classid', '__closure', '__declspec',
    '__dispid', '__except', '__export', '__fastcall', '__finally', '__import',
    '__int16', '__int32', '__int64', '__int8', '__pascal', '__property',
    '__published', '__rtti', '__stdcall', '__thread', '__try', '_asm', '_cdecl',
    '_export', '_fastcall', '_import', '_pascal', '_stdcall');

  { Java }

  JavaKeywords: array[0..52] of string = (
    'abstract', 'assert', 'boolean', 'break', 'byte', 'case', 'catch', 'char',
    'class', 'const', 'continue', 'default', 'do', 'double', 'else', 'enum',
    'extends', 'false', 'final', 'finally', 'float', 'for', 'goto', 'if',
    'implements', 'import', 'instanceof', 'int', 'interface', 'long', 'native',
    'new', 'null', 'package', 'private', 'protected', 'public', 'return',
    'short', 'static', 'strictfp', 'super', 'switch', 'synchronized', 'this',
    'throw', 'throws', 'transient', 'true', 'try', 'void', 'volatile', 'while');

function HilightClasses: TList;
begin
  if my_Hilightes = nil then
    my_Hilightes := TList.Create;
  Result := my_Hilightes;
end;

function FindHilight(const Language: string): TLyHilighterClass;
var
  I: integer;
begin
  for I := 0 to HilightClasses.Count - 1 do
  begin
    Result := TLyHilighterClass(HilightClasses[I]);
    if MatchID(Language, Result.Language) then Exit;
  end;
  Result := nil;
end;

function FindHilightByFileExt(const FileExt: string): TLyHilighterClass;
var
  I: integer;
begin
  for I := 0 to HilightClasses.Count - 1 do
  begin
    Result := TLyHilighterClass(HilightClasses[I]);
    if Result.Support(FileExt) then Exit;
  end;
  Result := nil;
end;

function FindFileHilight(const FileName: string): TLyHilighterClass;
begin
  Result := FindHilightByFileExt(ExtractFileExt(FileName));
end;

function PlaceACodeMemo(Host: TWinControl): TLyCodeMemo;
begin
  Result := TLyCodeMemo.Create(Host);
  Result.Parent := Host;
  Result.Align := alClient;
  Result.Visible := true;
  Result.Modified := false;
end;

function KeyStateOK: boolean;
begin
  Result := (GetKeyState(VK_CONTROL) >= 0) and
            (GetKeyState(VK_MENU) >= 0) and
            (GetKeyState(VK_BACK) >= 0);
end;

function IsCtrlShift(S: TShiftState): boolean;
begin
  Result := (S - [ssShift, ssCtrl] = []);
end;

function TabSpaces(TextX, TabSize: integer): WideString;
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

function PosEx(const SubStr, Str: WideString; Index: integer): integer;
var
  I, N, L, J: Integer;
  P, B: PWideChar;
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

{ TLyCodeMemo }

constructor TLyCodeMemo.Create(AOwner: TComponent);
var
  L: TLyLine;
begin
  inherited;
  TabStop := true;
  Color := clWindow;
  Cursor := crIBeam;
  FMargin := ScreenWidth(1);
  FState := [msWrapLine, msUndoRedo, msGutter];

  FUpdate := TLyUpdate.Create(Self);
  FCaret := TLyCaret.Create(Self);
  FSelection := TLySelection.Create(Self);
  FLines := TLyLineList.Create(Self);
  FHilighter := TLyRiviHilighter.Create(Self);

  L := TLyLine.Create(nil);
  L.FList := FLines;
  FLines.FItems.Add(L);

  Font.Name := CM_FONTNAME;
  Font.Size := CM_FONTSIZE;
  Font.OnChange := {$IFDEF FPC}@{$ENDIF}FontChange;

  FGutter := TLyGutter.Create(Self);
  FGutter.Parent := Self;
  FGutter.FMemo := Self;
  FGutter.Align := alLeft;
  FGutter.Width := 10;
  FGutter.Caption := '';
  FGutter.TabStop := false;
  FGutter.Visible := true;
end;

procedure TLyCodeMemo.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
    Style := Style or WS_CLIPCHILDREN;
end;

procedure TLyCodeMemo.CutToClipboard;
begin
  FSelection.CutToClipboard;
end;

destructor TLyCodeMemo.Destroy;
begin
  FState := FState + [msDestroying];
  Inc(FCaret.FMoveCount);
  Inc(FUpdate.FCount);
  SetState(msReadonly, true);
  if FScrollBar <> nil then
    FScrollBar.OnChange := nil;
  FSelection.UnSelect;
  FCaret.MoveTo(0, 0);
  FLines.Clear;
  if FScrollBar <> nil then
  begin
    FScrollBar.OnChange := nil;
    FreeAndNil(FScrollBar);
  end;
  FreeAndNil(FSelection);
  FreeAndNil(FLines);
  FreeAndNil(FCaret);
  FreeAndNil(FHilighter);
  FreeAndNil(FScrollBar);
  FreeAndNil(FUpdate);
  inherited;
end;

procedure TLyCodeMemo.Paint;
begin
  if FUpdate.FCount = 0 then
    PaintFrom(0, false);
end;

procedure TLyCodeMemo.PaintLine80;
var
  X: integer;
begin
  if ShowLine80 then
  begin
    SetCanvas(false);
    X := FMargin + Canvas.TextWidth(' ') * 80;
    if FGutter.Visible then
      Inc(X, FGutter.Width);
    Canvas.Pen.Color := Palette.Line80Color;
    Canvas.MoveTo(X, 0);
    Canvas.LineTo(X, Height);
  end;
end;

procedure TLyCodeMemo.PasteFromClipboard;
begin
  FSelection.PasteFromClipboard;
end;

procedure TLyCodeMemo.Print;
var
  I, X, Y, PH, MT: integer;
  M: TLyLine;
begin
  Printer.BeginDoc;
  try
//    PW := Printer.PageWidth;
    PH := Printer.PageHeight;
    X := 0;
    Y := 0;
    for I := 0 to FLines.Count - 1 do
    begin
      M := FLines[I];
//      ML := M.Left;
      MT := M.FTop;

//      M.fleft := ML;
      M.FTop := Y;
//      M.FWidth := PW;
      M.Resize;
      M.PaintAt(X, Y);

      Inc(Y, M.Height);
      while Y > PH do
      begin
        Printer.NewPage;
        Y := (Y - PH) - M.Height;
        M.PaintAt(X, Y);
        Inc(Y, M.Height);
      end;

      M.FTop := MT;
//      M.FWidth := MW;
      M.Resize;
    end;
    Printer.EndDoc;
  except
    Printer.Abort;
    Resize;
    raise;
  end;
end;

procedure TLyCodeMemo.MakeCaretVisible;
begin
  MakeVisible(CaretY);
end;

procedure TLyCodeMemo.MakeVisible(LineX: integer);
var
  P: integer;
begin
  if LineX >= 0 then
  begin
    P := ScrollBar.Position;
    if (FBodyLines = 1) or (LineX < P) then
      ScrollBar.Position := LineX else
    if LineX >= P + FBodyLines then
      ScrollBar.Position := LineX - FBodyLines + 1;
  end;
end;

procedure TLyCodeMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TLyLine;
  I: integer;
begin
  SetFocus;
  FSelection.FSelecting := false;
  FCaret.BeginMove;
  try
    if (Button = mbLeft) and (Shift = [ssLeft]) then
    begin
      FSelection.Selected := false;
      {$IFDEF FPC}
      I := 0;
      {$ENDIF}
      P := HitText(X, Y, I);
      FSelection.MouseDown(P, I);
    end
    else inherited;
  finally
    FCaret.EndMove;
  end;
end;

procedure TLyCodeMemo.ScrollBarChange(Sender: TObject);
begin
  if (FScrollBar <> nil) and StatusOK then
  begin
    Inc(FUpdate.FCount);
    try
      FCaret.MoveCaret;
      Invalidate;
      FGutter.Invalidate;
    finally
      Dec(FUpdate.FCount);
    end;
  end;
end;

procedure TLyCodeMemo.ScrollDown(Delta: integer);
var
  P, V: integer;
begin
  if (Delta > 0) and (ScrollBar.Max > 0) then
  begin
    P := ScrollBar.Position;
    V := Min(P + Delta, ScrollBar.Max);
    if P <> V then
      ScrollBar.Position := V;
  end;
end;

procedure TLyCodeMemo.ScrollUp(Delta: integer);
var
  P, V: integer;
begin
  if (Delta > 0) and (ScrollBar.Max > 0) then
  begin
    P := ScrollBar.Position;
    V := Max(P - Delta, 0);
    if P <> V then
      ScrollBar.Position := V;
  end;
end;

procedure TLyCodeMemo.SelectAll;
begin
  FSelection.SelectAll;
end;

procedure TLyCodeMemo.SetCanvas(Selected: boolean);
begin
  Canvas.Font.Assign(Font);
  if Selected then
  begin
    Canvas.Brush.Color := Palette.SelectedBackground;
    Canvas.Font.Color := Palette.SelectedTextColor;
  end
  else Canvas.Brush.Color := Palette.Background;
end;

procedure TLyCodeMemo.PaintFrom(AItemX: integer; OnlyChanged: boolean);
var
  I, X, Y, N, B, E, ItemX, LineX: integer;
  L: TLyLine;
begin
  Y := 0;
  X := FMargin;
  if FGutter.Visible then
    Inc(X, FGutter.Width);

  N := FBodyLines;
  if N * FLineHeight < Height then Inc(N);

  ItemX := 0;
  LineX := 0;
  if GetTopLine(ItemX, LineX) then
  repeat
    L := FLines[ItemX];
    if L.FLastToken < 0 then
      FLines.DecorateFrom(ItemX);
    B := 0;
    E := 0;
    if ItemX >= AItemX then
      if not OnlyChanged or L.FChanged then
        FSelection.GetRange(ItemX, B, E);
    for I := LineX to L.LineCount - 1 do
      if N > 0 then
      begin
        if ItemX >= AItemX then
          if not OnlyChanged or L.FChanged then
            L.DrawLine(I, X, Y, B, E);
        Inc(Y, FLineHeight);
        Dec(N);
      end
      else Exit;
    LineX := 0;
    Inc(ItemX);
  until (N = 0) or (ItemX = FLines.Count);

  if N > 0 then
  begin
    SetCanvas(false);
    Canvas.FillRect(Rect(X, Y, Width - FScrollBar.Width, Height));
  end;

  PaintLine80;
end;

procedure TLyCodeMemo.BeginUpdate;
begin
  FUpdate.BeginUpdate;
end;

procedure TLyCodeMemo.CheckFontStatus;
begin
  if FSpaceWidth = 0 then FontChange(nil);
end;

procedure TLyCodeMemo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FCaret.Active and StatusOK then
  begin
    BeginUpdate;
    try
      if FUpdate.Process(Self, Key, Shift) then
        Key := 0;
    finally
      EndUpdate;
    end;
    if Key <> 0 then inherited;
  end;
end;

procedure TLyCodeMemo.DblClick;
var
  L: TLyLine;
  I, N: integer;
begin
  L := FCaret.Item;
  if L <> nil then
  begin
    N := 0;
    I := L.SeekCurrWord(FCaret.FTextX, N);
    if (I > 0) and (N > 0) then
    begin
      FCaret.MoveTo(FCaret.FItemX, I + N);
      FSelection.FFrom.ItemX := FCaret.FItemX;
      FSelection.FFrom.TextX := I;
      FSelection.Select(FCaret.FItemX, I, FCaret.FItemX, I + N - 1);
    end;
  end;
end;

procedure TLyCodeMemo.CopyToClipboard;
begin
  FSelection.CopyToClipboard;
end;

function TLyCodeMemo.GetCanRedo: boolean;
begin
  Result := FUpdate.CanRedo;
end;

function TLyCodeMemo.GetCanUndo: boolean;
begin
  Result := FUpdate.CanUndo;
end;

function TLyCodeMemo.GetScrollBar: TScrollBar;
begin
  if (FScrollBar = nil) and StatusOK then
  begin
    FScrollBar := TScrollBar.Create(Self);
    GiveName(FScrollBar);
    FScrollBar.Parent := Self;
    FScrollBar.Kind := sbVertical;
    FScrollBar.Align := alRight;
    FScrollBar.TabStop := false;
    FScrollBar.Enabled := false;
    FScrollBar.Visible := true;
    FScrollBar.OnChange := {$IFDEF FPC}@{$ENDIF}ScrollBarChange;
  end;
  Result := FScrollBar;
end;

function TLyCodeMemo.GetState(X: TLyMemoState): boolean;
begin
  Result := (X in FState);
end;

function TLyCodeMemo.GetUpdating: boolean;
begin
  Result := FUpdate.Updating or FUpdate.EndUpdating;
end;

function TLyCodeMemo.GetSelAvail: boolean;
begin
  Result := FSelection.Selected;
end;

function TLyCodeMemo.GetSelText: WideString;
begin
  Result := FSelection.Text;
end;

function TLyCodeMemo.GetTopLine(var ItemX, LineX: integer): boolean;
var
  I, Y: integer;
  L: TLyLine;
begin
  if not WrapLine then
  begin
    ItemX := TopCaretY;
    LineX := 0;
    Result := ItemX < FLines.Count;
  end
  else
  begin
    Y := TopCaretY;
    for I := 0 to FLines.Count - 1 do
    begin
      L := FLines[I];
      if Y < L.LineCount then
      begin
        Result := true;
        ItemX := I;
        LineX := Y;
        Exit;
      end;
      Dec(Y, L.LineCount);
    end;
    Result := false;
  end;
end;

procedure TLyCodeMemo.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TLyCodeMemo.WMKillFocus(var Msg: TWMKillFocus);
begin
  FSelection.FSelecting := false;
  FCaret.DestroyCaret;
  inherited;
end;

procedure TLyCodeMemo.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  FCaret.MoveCaret;
end;

function TLyCodeMemo.HitText(X, Y: integer; var TextX: integer): TLyLine;
var
  I: integer;
begin
  I := HitLine(Y);
  Result := FLines.FindByLine(I, true);
  TextX := Result.HitText(X, I);
end;

function TLyCodeMemo.HitLine(Y: integer): integer;
begin
  Result := ScrollBar.Position;
  if Y < 0 then
  begin
    Y := FLineHeight - 1 - Y;
    Dec(Result, Y div FLineHeight);
  end
  else Inc(Result, Y div FLineHeight);
end;

procedure TLyCodeMemo.EndUpdate;
begin
  FUpdate.EndUpdate;
end;

procedure TLyCodeMemo.Keyboard(Key: Word; Shift: TShiftState);
begin
  if FCaret.Active and StatusOK then
  begin
    BeginUpdate;
    try
      FUpdate.Process(Self, Key, Shift);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TLyCodeMemo.NotifyStatus;
begin
  if Assigned(FOnStatus) then FOnStatus(Self);
end;

procedure TLyCodeMemo.NotifyChange;
begin
  if Modified then
  begin
    if Assigned(FOnChange) then FOnChange(Self);
    NotifyStatus;
  end;
end;

function TLyCodeMemo.StatusOK: boolean;
begin
  Result := HandleAllocated and
            not (csDestroying in ComponentState) and
            not (msDestroying in FState);
end;

function TLyCodeMemo.FindNext(const S: WideString; Rewind: boolean): boolean;

  function find_in(X1, X2: integer): boolean;
  var
    I, X: integer;
    M: TLyLine;
  begin
    for I := X1 to X2 do
    begin
      M := FLines[I];
      X := PosEx(S, M.Text, 1);
      if X > 0 then
      begin
        FSelection.Select(I, X, I, X + Length(S) - 1);
        FCaret.MoveToSelEnd;
        FCaret.MakeVisible;
        Exit(true);
      end;
    end;
    Result := false;
  end;

var
  X, I, P: integer;
  M: TLyLine;
begin
  Result := (S <> '') and (FLines.Count > 0);
  if not Result then Exit;

  if FSelection.PartialSelected then
  begin
    I := FSelection.FTail.ItemX;
    X := FSelection.FTail.TextX + 1;
  end
  else
  if FCaret.GetActive then
  begin
    I := FCaret.FItemX;
    X := FCaret.FTextX;
  end
  else
  begin
    I := 0;
    X := 1;
  end;

  M := FLines[I];
  P := PosEx(S, M.Text, X);
  if P > 0 then
  begin
    FSelection.Select(I, P, I, P + Length(S) - 1);
    FCaret.MoveToSelEnd;
    FCaret.MakeVisible;
  end
  else Result := find_in(I + 1, FLines.Count - 1) or
    (Rewind and find_in(0, I - 1));
end;

procedure TLyCodeMemo.FontChange(Sender: TObject);

  function is_fixed_width_font: boolean;
  var
    I: char;
  begin
    for I := 'A' to 'z' do
      if FSpaceWidth <> Canvas.TextWidth(I) then
      begin
        Result := false;
        Exit;
      end;
    Result := true;
  end;

var
  X: integer;
begin
  if not (msFonting in FState) then
  try
    FState := FState + [msFonting];

    // 1. use fix-width font
    SetCanvas(false);
    FSpaceWidth := Canvas.TextWidth(' ');
    if not is_fixed_width_font then
    begin
      Font.Name := CM_FONTNAME;
      SetCanvas(false);
      FSpaceWidth := Canvas.TextWidth(' ');
    end;
    FGutter.AdjustWidth;

    // 2. collect editing informations
    FTextHeight := Canvas.TextHeight('H');
    FLineSpacing := Max(1, FTextHeight div 5);
    FLineHeight := FLineSpacing + FTextHeight + FLineSpacing;
    FBodyHeight := Max(0, Height);
    FBodyLines := Max(1, FBodyHeight div FLineHeight);

    // 3. resize all
    FBodyWidth := EditAreaWidth;
    X := Max(0, ResizeFrom(0, false) - FBodyLines);
    FLastBodyWidth := FBodyWidth;

    // 4. reset scroll bar
    ScrollBar.OnChange := nil;
    ScrollBar.Max := X;
    ScrollBar.SmallChange := 1;
    ScrollBar.LargeChange := FBodyLines;
    ScrollBar.Enabled := (X > 0);
    ScrollBar.OnChange := {$IFDEF FPC}@{$ENDIF}ScrollBarChange;

    // 5. refresh display
    Invalidate;
    FGutter.Invalidate;
  finally
    FState := FState - [msFonting];
    NotifyStatus;
  end;
end;

function TLyCodeMemo.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): boolean;
begin
  Result := true;
  ScrollDown(Max(1, FBodyLines div 3));
end;

function TLyCodeMemo.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): boolean;
begin
  Result := true;
  ScrollUp(Max(1, FBodyLines div 3));
end;

{$IFDEF FPC}
procedure TLyCodeMemo.UTF8KeyPress(var Key: TUTF8Char);
begin
  Keyboard(UTF8Decode(Key)[1]);
  Key := '';
end;
{$ELSE}
procedure TLyCodeMemo.KeyPress(var Key: char);
begin
  Keyboard(Key);
  Key := #0;
end;
{$ENDIF}

procedure TLyCodeMemo.Keyboard(Key: WideChar);

  procedure do_input(const S: WideString);
  var
    I: integer;
  begin
    if FCaret.FTextX < 1 then FCaret.FTextX := 1;
    I := FCaret.Item.Insert(FCaret.FTextX, S);
    Inc(FCaret.FTextX, I);
  end;

  procedure do_enter;
  var
    L, N: TLyLine;
    I: integer;
  begin
    L := FCaret.Item;
    if FCaret.FTextX <= 1 then
    begin
      FLines.Insert(FCaret.FItemX);
      FCaret.MoveTo(FCaret.FItemX + 1, FCaret.FTextX);
    end
    else
    begin
      N := L.BreakFrom(FCaret.FTextX);
      I := N.Insert(1, L.LeadingSpaces);
      if I > 0 then
        FCaret.MoveTo(N.Index, I + 1) else
        FCaret.MoveToHead(N);
    end;
  end;

begin
  if FCaret.Active and StatusOK then
    if (Key >= ' ') or CharInSet(Key, [#9, #10, #13]) then
      if Readonly then FSelection.UnSelect else
      begin
        BeginUpdate;
        try
          FSelection.Delete;
          if Key >= ' ' then do_input(Key) else
          if Key = #13 then do_enter else
          if Key = #10 then do_enter else
          if Key = #9 then do_input(TabSpaces(FCaret.FTextX, CM_TABSIZE));
        finally
          EndUpdate;
        end;
      end;
end;

procedure TLyCodeMemo.KeyUp(var Key: Word; Shift: TShiftState);
begin
  FSelection.FSelecting := false;
  inherited;
end;

procedure TLyCodeMemo.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TLyLine;
  I: integer;
begin
  if FSelection.FSelecting and (Shift = [ssLeft]) then
  begin
    FCaret.BeginMove;
    try
      {$IFDEF FPC}
      I := 0;
      {$ENDIF}
      P := HitText(X, Y, I);
      FSelection.MouseMove(P, I);
    finally
      FCaret.EndMove;
    end;
  end
  else
  begin
    FSelection.FSelecting := false;
    inherited;
  end;
end;

procedure TLyCodeMemo.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelection.FSelecting := false;
  inherited;
end;

procedure TLyCodeMemo.Redo;
begin
  FUpdate.Redo;
end;

procedure TLyCodeMemo.Resize;
var
  X, P: integer;
  F: boolean;
begin
  if not (msResizing in FState) then
  try
    FState := FState + [msResizing];
    CheckFontStatus;

    FBodyHeight := Max(0, Height);
    FBodyLines := Max(1, FBodyHeight div FLineHeight);

    FBodyWidth := EditAreaWidth;
    F := (FBodyWidth = FLastBodyWidth);
    X := Max(0, ResizeFrom(0, F or not WrapLine) - FBodyLines);
    FLastBodyWidth := FBodyWidth;

    P := ScrollBar.Position;
    ScrollBar.OnChange := nil;
    ScrollBar.Max := X;
    ScrollBar.SmallChange := 1;
    ScrollBar.LargeChange := FBodyLines;
    ScrollBar.Enabled := (X > 0);
    ScrollBar.OnChange := {$IFDEF FPC}@{$ENDIF}ScrollBarChange;

    if F or (P <> ScrollBar.Position) then
    begin
      Invalidate;
      FGutter.Invalidate;
    end;
  finally
    FState := FState - [msResizing];
    NotifyStatus;
  end;
end;

function TLyCodeMemo.GetCaretY: integer;
var
  L: TLyLine;
begin
  if FCaret.Active then
  begin
    L := FCaret.Item;
    Result := L.CaretY + L.TextLine(FCaret.FTextX);
  end
  else Result := -1;
end;

function TLyCodeMemo.GetLineCount: integer;
begin
  Result := FLines.LineCount;
end;

function TLyCodeMemo.EditAreaWidth: integer;
var
  W: integer;
begin
  W := Width - FMargin * 2 - ScrollBar.Width;
  if FGutter.Visible then
    Dec(W, FGutter.Width);
  Result := Max(FSpaceWidth * 2, (W div FSpaceWidth) * FSpaceWidth);
end;

function TLyCodeMemo.GetCaretX: integer;
begin
  Result := -1;
  if FCaret.Active then
    FCaret.Item.TextLine(FCaret.FTextX, Result);
end;

function TLyCodeMemo.TopCaretY: integer;
begin
  if ScrollBar.Max > 0 then
    Result := ScrollBar.Position else
    Result := 0;
end;

function TLyCodeMemo.TopOffset: integer;
begin
  Result := TopCaretY * FLineHeight;
end;

procedure TLyCodeMemo.Undo;
begin
  FUpdate.Undo;
end;

procedure TLyCodeMemo.UnSelect;
begin
  FSelection.UnSelect;
end;

function TLyCodeMemo.ResizeFrom(ItemX: integer; OnlyChanged: boolean): integer;
var
  I, T: integer;
  L: TLyLine;
begin
  Result := 0;
  T := 0;
  for I := 0 to FLines.Count - 1 do
  begin
    L := FLines[I];
    L.FTop := T;
    if (I >= ItemX) and (not OnlyChanged or L.FChanged or (L.LineCount = 0)) then
      L.Resize;
    Inc(T, L.Height);
    Inc(Result, L.LineCount);
  end;
end;

procedure TLyCodeMemo.SetState(X: TLyMemoState; Value: boolean);
begin
  if Value <> (X in FState) then
  begin
    if Value then FState := FState + [X] else FState := FState - [X];
    if X = msReadonly then
    begin
      FUpdate.FUndos.Clear;
      FUpdate.FRedos.Clear;
    end
    else
    if X = msUndoRedo then
    begin
      FUpdate.FUndos.Clear;
      FUpdate.FRedos.Clear;
    end
    else
    if X = msModified then
    begin
      if Value then
      FUpdate.FChanged := true else
        FLines.SetModified(false);
      NotifyStatus;
    end
    else
    if X = msWrapLine then
    begin
      FLastBodyWidth := 0;
      Resize;
    end
    else
    if X = msGutter then
    begin
      FGutter.Visible := Value;
      FLastBodyWidth := 0;
      Resize;
    end
    else
    if X = msLine80 then
      Invalidate;
  end;
end;

procedure TLyCodeMemo.SetSelText(const Value: WideString);
begin
  FSelection.Text := Value;
end;

{ TLyUpdate }

constructor TLyUpdate.Create(ASender: TObject);
begin
  inherited;
  FMemo := ASender as TLyCodeMemo;
  FUndos := TLyUndoList.Create(Self);
  FRedos := TLyUndoList.Create(Self);

  { keyboard }

  Add(Ord('A'), [ssCtrl], @KeySelectAll);
  Add(Ord('C'), [ssCtrl], @KeyCopy);
  Add(Ord('V'), [ssCtrl], @KeyPaste);
  Add(Ord('X'), [ssCtrl], @KeyCut);
  Add(Ord('Y'), [ssCtrl], @KeyDeleteLine);
  Add(Ord('Z'), [ssCtrl], @KeyUndo);
  Add(Ord('Z'), [ssCtrl, ssShift], @KeyRedo);
  Add(VK_F5, [], @KeyRefresh);
  Add(VK_LEFT, [], @KeyLeft);
  Add(VK_LEFT, [ssCtrl], @KeyLeft);
  Add(VK_LEFT, [ssShift], @KeyLeft);
  Add(VK_LEFT, [ssShift, ssCtrl], @KeyLeft);
  Add(VK_RIGHT, [], @KeyRight);
  Add(VK_RIGHT, [ssCtrl], @KeyRight);
  Add(VK_RIGHT, [ssShift], @KeyRight);
  Add(VK_RIGHT, [ssShift, ssCtrl], @KeyRight);
  Add(VK_UP, [], @KeyUp);
  Add(VK_UP, [ssCtrl], @KeyUp);
  Add(VK_UP, [ssShift], @KeyUp);
  Add(VK_UP, [ssShift, ssCtrl], @KeyUp);
  Add(VK_DOWN, [], @KeyDown);
  Add(VK_DOWN, [ssCtrl], @KeyDown);
  Add(VK_DOWN, [ssShift], @KeyDown);
  Add(VK_DOWN, [ssShift, ssCtrl], @KeyDown);
  Add(VK_PRIOR, [], @KeyPageUp);
  Add(VK_PRIOR, [ssCtrl], @KeyPageUp);
  Add(VK_PRIOR, [ssShift], @KeyPageUp);
  Add(VK_PRIOR, [ssShift, ssCtrl], @KeyPageUp);
  Add(VK_NEXT, [], @KeyPageDown);
  Add(VK_NEXT, [ssCtrl], @KeyPageDown);
  Add(VK_NEXT, [ssShift], @KeyPageDown);
  Add(VK_NEXT, [ssShift, ssCtrl], @KeyPageDown);
  Add(VK_HOME, [], @KeyHome);
  Add(VK_HOME, [ssCtrl], @KeyHome);
  Add(VK_HOME, [ssShift], @KeyHome);
  Add(VK_HOME, [ssShift, ssCtrl], @KeyHome);
  Add(VK_END, [], @KeyEnd);
  Add(VK_END, [ssCtrl], @KeyEnd);
  Add(VK_END, [ssShift], @KeyEnd);
  Add(VK_END, [ssShift, ssCtrl], @KeyEnd);
  Add(VK_BACK, [], @KeyBack);
  Add(VK_BACK, [ssCtrl], @KeyBack);
  Add(VK_DELETE, [], @KeyDelete);
  Add(VK_DELETE, [ssShift], @KeyCut);
  Add(VK_INSERT, [ssShift], @KeyPaste);
end;

destructor TLyUpdate.Destroy;
begin
  FreeAndNil(FUndos);
  FreeAndNil(FRedos);
  inherited;
end;

procedure TLyUpdate.BeginUpdate;
begin
  if not EndUpdating and MemoOK then
  begin
    Selection.FSelecting := false;
    Caret.BeginMove;
    Inc(FCount);
    if (FCount = 1) and MemoOK then
    begin
      FMemo.CheckFontStatus;
      FUndoMode := cdNone;
      Inc(FMagic);
      FOrgCaretY := FMemo.TopCaretY;
      FChanged := false;
      FChangedLines := 0;
      FAddedLines := 0;
      FDeletedLines := 0;
      FChangedItemX := MaxInt;
      Lines.SetChanged(false);
      Lines.SetNeedPaint(false);
    end;
  end;
end;

procedure TLyUpdate.EndUpdate;
var
  X: integer;
  L: TLyLine;
begin
  if not EndUpdating and MemoOk then
  try
    Dec(FCount);
    if (FCount = 0) and MemoOK then
    begin
      FEndUpdating := true;
      try
        if FMemo.FLines.Count = 0 then
        begin
          L := TLyLine.Create(nil);
          L.FList := Lines;
          Lines.FItems.Add(L);
        end;

        if FChangedLines > 0 then
        begin
          FMemo.SetCanvas(false);
          X := Max(0, FMemo.ResizeFrom(FChangedItemX, true) - FMemo.FBodyLines);
          if X <> FMemo.ScrollBar.Max then
          begin
            FMemo.ScrollBar.Max := X;
            FMemo.ScrollBar.Enabled := (X > 0);
          end;
          Caret.MakeVisible;
          if FOrgCaretY = FMemo.TopCaretY then
            FMemo.PaintFrom(Max(0, FChangedItemX), (FAddedLines + FDeletedLines) = 0);
        end;
      finally
        FEndUpdating := false;
        if FChanged and FMemo.Modified then
          FMemo.NotifyChange else
          FMemo.NotifyStatus;
      end;
    end;
  finally
    Caret.EndMove;
  end;
end;

procedure TLyUpdate.Undo;
begin
  FUndos.Apply;
end;

procedure TLyUpdate.Redo;
begin
  FRedos.Apply;
end;

function TLyUpdate.GetMemoOK: boolean;
begin
  Result := FMemo.StatusOK;
end;

function TLyUpdate.GetCaret: TLyCaret;
begin
  Result := FMemo.FCaret;
end;

function TLyUpdate.GetCanRedo: boolean;
begin
  Result := (FRedos.FLast <> nil) and (FUndoMode = cdNone);
end;

function TLyUpdate.GetCanUndo: boolean;
begin
  Result := (FUndos.FLast <> nil) and (FUndoMode = cdNone);
end;

procedure TLyUpdate.KeyRefresh(AMemo: TObject; Shift: TShiftState);
begin
  FMemo.Refresh;
end;

procedure TLyUpdate.KeySelectAll(AMemo: TObject; Shift: TShiftState);
begin
  FMemo.SelectAll;
end;

procedure TLyUpdate.KeyCopy(AMemo: TObject; Shift: TShiftState);
begin
  FMemo.CopyToClipboard;
end;

procedure TLyUpdate.KeyCut(AMemo: TObject; Shift: TShiftState);
begin
  if Selection.Selected then
    if Readonly then Selection.UnSelect else
    begin
      Selection.CopyToClipboard;
      Selection.Delete;
    end;
end;

procedure TLyUpdate.KeyPaste(AMemo: TObject; Shift: TShiftState);
begin
  if Readonly then Selection.UnSelect else
  begin
    Selection.Delete;
    Caret.Insert(StrToWide(Clipboard.AsText));
  end;
end;

procedure TLyUpdate.KeyLeft(AMemo: TObject; Shift: TShiftState);
var
  M: TLyLine;
  I: integer;
begin
  I := 0;
  M := Caret.PrevPos(I, ssCtrl in Shift);
  if M <> nil then
  begin
    if ssShift in Shift then
      Selection.SelectTo(M.Index, I) else
      Selection.UnSelect;
    Caret.MoveTo(M.Index, I);
    Caret.MakeVisible;
  end;
end;

procedure TLyUpdate.KeyRight(AMemo: TObject; Shift: TShiftState);
var
  M: TLyLine;
  I: integer;
begin
  I := 0;
  M := Caret.NextPos(I, ssCtrl in Shift);
  if M <> nil then
  begin
    if ssShift in Shift then
      Selection.SelectTo(M.Index, I) else
      Selection.UnSelect;
    Caret.MoveTo(M.Index, I);
    Caret.MakeVisible;
  end;
end;

procedure TLyUpdate.KeyUp(AMemo: TObject; Shift: TShiftState);
var
  M: TLyLine;
  I: integer;
begin
  I := 0;
  M := Caret.ScrollUp(1, I);
  if M <> nil then
  begin
    if ssShift in Shift then
      Selection.SelectTo(M.Index, I) else
      Selection.UnSelect;
    Caret.MoveTo(M.Index, I);
    Caret.MakeVisible;
  end;
end;

procedure TLyUpdate.KeyDown(AMemo: TObject; Shift: TShiftState);
var
  M: TLyLine;
  I: integer;
begin
  I := 0;
  M := Caret.ScrollDown(1, I);
  if M <> nil then
  begin
    if ssShift in Shift then
      Selection.SelectTo(M.Index, I) else
      Selection.UnSelect;
    Caret.MoveTo(M.Index, I);
    Caret.MakeVisible;
  end;
end;

procedure TLyUpdate.KeyPageUp(AMemo: TObject; Shift: TShiftState);
var
  M: TLyLine;
  I: integer;
begin
  I := 0;
  M := Caret.ScrollUp(FMemo.FBodyLines, I);
  if M <> nil then
  begin
    if ssShift in Shift then
      Selection.SelectTo(M.Index, I) else
      Selection.UnSelect;
    Caret.MoveTo(M.Index, I);
    Caret.MakeVisible;
  end;
end;

procedure TLyUpdate.KeyPageDown(AMemo: TObject; Shift: TShiftState);
var
  M: TLyLine;
  I: integer;
begin
  I := 0;
  M := Caret.ScrollDown(FMemo.FBodyLines, I);
  if M <> nil then
  begin
    if ssShift in Shift then
      Selection.SelectTo(M.Index, I) else
      Selection.UnSelect;
    Caret.MoveTo(M.Index, I);
    Caret.MakeVisible;
  end;
end;

procedure TLyUpdate.KeyHome(AMemo: TObject; Shift: TShiftState);
var
  M: TLyLine;
  X: integer;
begin
  if ssCtrl in Shift then
  begin
    M := Lines[0];
    X := M.HeadTextX;
  end
  else
  begin
    M := Caret.Item;
    X := M.FirstText(M.TextLine(Caret.FTextX));
  end;
  if ssShift in Shift then
    Selection.SelectTo(M.Index, X) else
    Selection.UnSelect;
  Caret.MoveTo(M.Index, X);
  Caret.MakeVisible;
end;

procedure TLyUpdate.KeyEnd(AMemo: TObject; Shift: TShiftState);
var
  M: TLyLine;
  X, I: integer;
begin
  if ssCtrl in Shift then
  begin
    M := Lines.Last;
    X := M.TailTextX;
  end
  else
  begin
    M := Caret.Item;
    I := M.TextLine(Caret.FTextX);
    X := M.FirstText(I) + M.FL[I];
  end;
  if ssShift in Shift then
    Selection.SelectTo(M.Index, X) else
    Selection.UnSelect;
  Caret.MoveTo(M.Index, X);
  Caret.MakeVisible;
end;

procedure TLyUpdate.KeyBack(AMemo: TObject; Shift: TShiftState);
var
  L: TLyLine;
  I: integer;
begin
  if Readonly then Selection.Unselect else
  if Selection.Selected then Selection.Delete else
  begin
    L := Caret.Item;
    I := Caret.FTextX;
    if (I > 1) and (ssCtrl in Shift) then
    begin
      I := L.SeekPriorWord(I);
      if I < 1 then
      begin
        L.Delete(1, Caret.FTextX - 1);
        Caret.MoveTo(Caret.FItemX, L.HeadTextX);
      end
      else
      begin
        L.Delete(I, Caret.FTextX - I);
        Caret.MoveTo(Caret.FItemX, I);
      end;
    end
    else
    if I > 1 then
    begin
      Caret.MoveTo(Caret.FItemX, I - 1);
      L.Delete(Caret.FTextX);
    end
    else
    if Caret.FItemX > 0 then
    begin
      L := Lines[Caret.FItemX - 1];
      Caret.MoveTo(Caret.FItemX - 1, L.TailTextX);
      L.MergeNext;
    end;
  end;
end;

procedure TLyUpdate.KeyDelete(AMemo: TObject; Shift: TShiftState);
var
  L: TLyLine;
begin
  if Readonly then Selection.UnSelect else
  if Selection.NotSelected then
  begin
    L := Caret.Item;
    if (L.FText = '') or (Caret.FTextX > Length(L.FText)) then
      L.MergeNext else
      L.Delete(Caret.FTextX);
  end
  else Selection.Delete;
end;

procedure TLyUpdate.KeyDeleteLine(AMemo: TObject; Shift: TShiftState);
var
  L: TLyLine;
  I, X: integer;
begin
  if Caret.Active then
    if Readonly then Selection.Unselect else
    begin
      Selection.Unselect;
      L := Caret.Item;
      if L.LineCount > 1 then
      begin
        Inc(FDeletedLines);
        I := L.TextLine(Caret.FTextX);
        X := L.FirstText(I);
        L.Delete(X, L.FL[I]);
        if I = 0 then
          Caret.FTextX := L.HeadTextX else
        if I = L.LineCount - 1 then
        begin
          X := L.Index;
          if X < Lines.Count - 1 then
          begin
            L := Lines[X + 1];
            Caret.MoveTo(X + 1, L.HeadTextX);
          end
          else Caret.FTextX := L.FirstText(I - 1);
        end
        else Caret.FTextX := X;
      end
      else
      if Lines.Count > 1 then
      begin
        I := L.Index;
        Lines.Delete(L.Index);
        if I = Lines.Count then
        begin
          L := Lines[I - 1];
          X := L.FirstText(L.LineCount - 1);
          Caret.MoveTo(I - 1, X);
        end
        else
        begin
          L := Lines[I];
          Caret.MoveToHead(L);
        end;
      end
      else
      if L.FText <> '' then
      begin
        L.SetText('');
        Caret.MoveToHead(L);
      end;
    end;
end;

procedure TLyUpdate.KeyUndo(AMemo: TObject; Shift: TShiftState);
begin
  FMemo.Undo;
end;

procedure TLyUpdate.KeyRedo(AMemo: TObject; Shift: TShiftState);
begin
  FMemo.Redo;
end;

function TLyUpdate.GetLines: TLyLineList;
begin
  Result := FMemo.FLines;
end;

function TLyUpdate.GetReadonly: boolean;
begin
  Result := FMemo.Readonly;
end;

function TLyUpdate.GetSelection: TLySelection;
begin
  Result := FMemo.FSelection;
end;

function TLyUpdate.GetUpdating: boolean;
begin
  Result := (FCount > 0);
end;

procedure TLyUpdate.UndoSaveChange(ItemX: integer; const S: WideString);
var
  L: TLyUndoList;
  U: TLyUndoItem;
begin
  FMemo.FState := FMemo.FState + [msModified];
  Inc(FChangedLines);
  FChangedItemX := Min(ItemX, FChangedItemX);
  if not Readonly and FMemo.UndoRedo and Updating then
  begin
    if FUndoMode = cdNone then FRedos.Clear;
    if FUndoMode = cdUndo then L := FRedos else L := FUndos;
    if (L.FLast <> nil) and (L.FLast.FType = ctChangeText) then
      if L.FLast.FLine = ItemX then
        Exit;
    U := TLyUndoItem.Create(L);
    U.FType := ctChangeText;
    U.FLine := ItemX;
    U.FText := S;
  end;
end;

procedure TLyUpdate.UndoSaveDelete(ItemX: integer; const S: WideString);
var
  L: TLyUndoList;
  U: TLyUndoItem;
begin
  FMemo.FState := FMemo.FState + [msModified];
  Inc(FChangedLines);
  Inc(FDeletedLines);
  FChangedItemX := Min(ItemX, FChangedItemX);
  if not Readonly and FMemo.UndoRedo and Updating then
  begin
    if FUndoMode = cdNone then FRedos.Clear;
    if FUndoMode = cdUndo then L := FRedos else L := FUndos;
    if (L.FLast <> nil) and (L.FLast.FLine = ItemX) then
      if L.FLast.FType = ctChangeText then
      begin
        L.FLast.FType := ctDeleteLine;
        Exit;
      end;
    U := TLyUndoItem.Create(L);
    U.FType := ctDeleteLine;
    U.FLine := ItemX;
    U.FText := S;
  end;
end;

procedure TLyUpdate.UndoSaveAdd(ItemX: integer);
var
  L: TLyUndoList;
  U: TLyUndoItem;
begin
  FMemo.FState := FMemo.FState + [msModified];
  Inc(FChangedLines);
  Inc(FAddedLines);
  FChangedItemX := Min(ItemX, FChangedItemX);
  if not Readonly and FMemo.UndoRedo and Updating then
  begin
    if FUndoMode = cdNone then FRedos.Clear;
    if FUndoMode = cdUndo then L := FRedos else L := FUndos;
    U := TLyUndoItem.Create(L);
    U.FType := ctAddLine;
    U.FLine := ItemX;
  end;
end;

{ TLyCaret }

procedure TLyCaret.BeginMove;
begin
  Inc(FMoveCount);
  HideCaret;
end;

constructor TLyCaret.Create(AMemo: TLyCodeMemo);
begin
  FMemo := AMemo;
  FWidth := -1;
  FHeight := -1;
  FItemX := 0;
  FTextX := 0;
end;

function TLyCaret.GetActive: boolean;
begin
  Result := (GetItem <> nil);
end;

function TLyCaret.GetAtTail: boolean;
var
  P: TLyLine;
begin
  P := GetItem;
  Result := (P <> nil) and (TextIndex >= P.TailTextX);
end;

function TLyCaret.GetItem: TLyLine;
begin
  if (FItemX >= 0) and (FItemX < FMemo.FLines.Count) then
    Result := FMemo.FLines[ItemIndex] else
    Result := nil;
end;

function TLyCaret.GetVisible: boolean;
begin
  Result := true;
end;

procedure TLyCaret.MoveAfterInsert(Line: TLyLine; TextX: integer; const S: WideString);
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
    MoveTo(Line.Index, TextX + X);
  end
  else
  if X > 0 then
  begin
    Line.SetText(S + Line.FText);
    MoveTo(Line.Index, X + 1);
  end
  else MoveToHead(Line);
end;

procedure TLyCaret.MoveCaret;
var
  M: TLyLine;
begin
  if (FMoveCount = 0) and Visible and Active then
  begin
    M := GetItem;
    FPos := M.TextPos(FTextX);
    FMemo.FGutter.Invalidate;
    if (FPos.X >= 0) and (FPos.Y >= 0) then
    begin
      if CreateCaret(2, FMemo.FTextHeight) and ShowCaret then
      begin
        {$IFDEF FPC}
        SetCaretPosEx(FMemo.Handle, FPos.X, FPos.Y - FMemo.TopOffset);
        {$ELSE}
        SetCaretPos(FPos.X, FPos.Y - FMemo.TopOffset);
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
    MoveTo(Item.Index, Item.TailTextX);
end;

function TLyCaret.NextPos(var TX: integer; AWord: boolean): TLyLine;
var
  LX: integer;
begin
  Result := GetItem;
  if Result <> nil then
    if AWord then
    begin
      TX := Result.SeekNextWord(FTextX);
      if TX = 0 then
      begin
        LX := Result.Index + 1;
        if LX < FMemo.FLines.Count then
        begin
          Result := FMemo.FLines[LX];
          TX := Result.HeadTextX;
        end
        else TX := Result.TailTextX;
      end;
    end
    else
    begin
      TX := FTextX;
      if TX < Result.TailTextX then Inc(TX) else
      begin
        LX := Result.Index + 1;
        if LX < FMemo.FLines.Count then
        begin
          Result := FMemo.FLines[LX];
          TX := Result.HeadTextX;
        end;
      end;
    end;
end;

function TLyCaret.PrevPos(var TX: integer; AWord: boolean): TLyLine;
var
  LX: integer;
begin
  Result := GetItem;
  if Result <> nil then
    if AWord then
    begin
      TX := Result.SeekPriorWord(FTextX);
      if TX = 0 then
      begin
        LX := Result.Index - 1;
        if LX >= 0 then
        begin
          Result := FMemo.FLines[LX];
          TX := Result.TailTextX;
        end
        else TX := Result.HeadTextX;
      end;
    end
    else
    begin
      TX := FTextX;
      if TX > 1 then Dec(TX) else
      begin
        LX := Result.Index - 1;
        if LX >= 0 then
        begin
          Result := FMemo.FLines[LX];
          TX := Result.TailTextX;
        end;
      end;
    end;
end;

procedure TLyCaret.MoveToHead(Item: TLyLine);
begin
  if Item <> nil then
    MoveTo(Item.Index, Item.HeadTextX);
end;

procedure TLyCaret.MoveToNext(Item: TLyLine);
begin
  Item := Item.Next;
  if Item <> nil then
    MoveToHead(Item);
end;

procedure TLyCaret.MoveToPrev(Item: TLyLine);
begin
  Item := Item.Prev;
  if Item <> nil then
    MoveToTail(Item);
end;

procedure TLyCaret.MoveToSelEnd;
var
  X, L: integer;
begin
  if (FMemo.FLines.Count > 0) and FMemo.FSelection.Selected then
    if not FMemo.FSelection.FSelectAll then
    begin
      X := FMemo.FSelection.FTail.TextX;
      L := Length(FMemo.FLines[FMemo.FSelection.FTail.ItemX].Text);
      if L > 0 then
        Inc(X) else X := 0;
      MoveTo(FMemo.FSelection.FTail.ItemX, X);
    end
    else MoveToTail(FMemo.FLines.Last);
end;

procedure TLyCaret.MakeVisible;
begin
  FMemo.MakeVisible(FMemo.CaretY);
end;

function TLyCaret.CreateCaret(Width, Height: integer): boolean;
begin
  Result := FCreated and (FWidth = Width) and (FHeight = Height);
  if not Result and FMemo.StatusOK and FMemo.Focused then
  begin
    DestroyCaret;
    {$IFDEF MSWINDOWS}
    Result := Windows.CreateCaret(FMemo.Handle, 0, Width, Height);
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

function TLyCaret.ScrollDown(LineCount: integer; var TX: integer): TLyLine;
var
  L: TLyLine;
  LX, IX: integer;
begin
  Result := nil;
  L := GetItem;
  if L = nil then Exit;

  if LineCount < 1 then
  begin
    Result := L;
    TX := FTextX;
    Exit;
  end;

  IX := FItemX;
  TX := 1;
  LX := L.TextLine(FTextX, TX);
  while (LineCount > 0) and (LX < L.LineCount - 1) do
  begin
    Dec(LineCount);
    Inc(LX);
  end;

  while (LineCount > 0) and (IX < FMemo.FLines.Count - 1) do
  begin
    Dec(LineCount);
    Inc(IX);
    L := FMemo.FLines[IX];
    LX := 0;
    while (LineCount > 0) and (LX < L.LineCount - 1) do
    begin
      Dec(LineCount);
      Inc(LX);
    end;
  end;

  if TX > L.FL[LX] + 1 then
    TX := L.FL[LX] + 1;
  for IX := 0 to LX - 1 do
    Inc(TX, L.FL[IX]);
  Result := L;
end;

function TLyCaret.ScrollUp(LineCount: integer; var TX: integer): TLyLine;
var
  L: TLyLine;
  LX, IX: integer;
begin
  Result := nil;
  L := GetItem;
  if L = nil then Exit;

  if LineCount < 1 then
  begin
    Result := L;
    TX := FTextX;
    Exit;
  end;

  IX := FItemX;
  TX := 1;
  LX := L.TextLine(FTextX, TX);
  while (LineCount > 0) and (LX > 0) do
  begin
    Dec(LineCount);
    Dec(LX);
  end;

  while (LineCount > 0) and (IX > 0) do
  begin
    Dec(LineCount);
    Dec(IX);
    L := FMemo.FLines[IX];
    LX := L.LineCount - 1;
    while (LineCount > 0) and (LX > 0) do
    begin
      Dec(LineCount);
      Dec(LX);
    end;
  end;

  if TX > L.FL[LX] + 1 then
    TX := L.FL[LX] + 1;
  for IX := 0 to LX - 1 do
    Inc(TX, L.FL[IX]);
  Result := L;
end;

function TLyCaret.ShowCaret: boolean;
begin
  if not FShowing and (FMoveCount = 0) and Visible and GetActive then
  begin
    {$IFDEF MSWINDOWS}
    FShowing := FCreated and Windows.ShowCaret(FMemo.Handle);
    {$ELSE}
    FShowing := FCreated and LCLIntf.ShowCaret(FMemo.Handle);
    {$ENDIF}
  end;
  Result := FShowing;
end;

procedure TLyCaret.HideCaret;
begin
  if FShowing and FMemo.StatusOK then
  begin
    {$IFDEF MSWINDOWS}
    Windows.HideCaret(FMemo.Handle);
    {$ELSE}
    LCLIntf.HideCaret(FMemo.Handle);
    {$ENDIF}
  end;
  FShowing := false;
end;

procedure TLyCaret.Insert(Strings: TStrings);
var
  L: TLyLine;
  S: WideString;
  I, X: integer;
begin
  if Active and (Strings.Count > 0) then
  begin
    FMemo.BeginUpdate;
    try
      if FMemo.FSelection.Selected then FMemo.FSelection.Delete;
      L := GetItem;
      if Strings.Count > 1 then
      begin
        S := L.CutFrom(FTextX);
        L.Add(StrToWide(Strings[0]));
        X := L.Index + 1;
        for I := 1 to Strings.Count - 2 do
        begin
          FMemo.FLines.Insert(X, StrToWide(Strings[I]));
          Inc(X);
        end;
        L := FMemo.FLines.Insert(X, S);
        MoveAfterInsert(L, 0, StrToWide(Strings[Strings.Count - 1]));
      end
      else MoveAfterInsert(L, FTextX, StrToWide(Strings[0]));
    finally
      FMemo.EndUpdate;
    end;
  end;
end;

procedure TLyCaret.Insert(const Text: WideString);
var
  L: TStrings;
begin
  if (Text <> '') and Active then
  begin
    L := TStringList.Create;
    try
      L.Text := WideToStr(Text);
      if CharInSet(Text[Length(Text)], [#13, #10]) then
         L.Add('');
      Insert(L);
    finally
      L.Free;
    end;
  end;
end;

procedure TLyCaret.DestroyCaret;
begin
  if FCreated and FMemo.StatusOK then
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

procedure TLyCaret.EndMove;
begin
  Dec(FMoveCount);
  if FMoveCount = 0 then
    MoveCaret;
end;

procedure TLyCaret.MoveTo(ItemX, TextX: integer);
begin
  if (ItemX <> FItemX) or (TextX <> FTextX) then
  begin
    FItemX := ItemX;
    FTextX := TextX;
    MoveCaret;
  end;
end;

{ TLySelection }

function TLySelection.GetPartialSelected: boolean;
begin
  Result := not FSelectAll and Selected;
end;

function TLySelection.GetSelected: boolean;
begin
  Result := FSelectAll or
            ((FHead.ItemX >= 0) and
             (FHead.TextX >= 0) and
             (FTail.ItemX >= FHead.ItemX) and
             (FTail.ItemX < FMemo.FLines.Count) and
             (FTail.TextX >= 0) and
             ((FHead.ItemX <> FTail.ItemX) or (FHead.TextX <= FTail.TextX)));
end;

function TLySelection.GetNotSelected: boolean;
begin
  Result := not GetSelected;
end;

function TLySelection.GetRange(ItemX: integer; var X1, X2: integer): boolean;
var
  L: integer;
begin
  Result := Contains(ItemX);
  if Result then
  begin
    L := Length(FMemo.FLines[ItemX].FText);
    if FSelectAll then
    begin
      X1 := 1;
      X2 := L;
    end
    else
    begin
      if ItemX = FHead.ItemX then
        X1 := Max(1, FHead.TextX) else
        X1 := 1;
      if ItemX = FTail.ItemX then
        X2 := Min(L, FTail.TextX) else
        X2 := L;
    end;
  end
  else
  begin
    X1 := -1;
    X2 := -1;
  end;
end;

function TLySelection.GetText: WideString;
var
  I: integer;
  M: TLyLine;
begin
  Result := '';
  if not Selected then Exit;

  if FSelectAll then
  begin
    Result := FMemo.FLines.Text;
    Exit;
  end;

  M := FMemo.FLines[FHead.ItemX];
  if FHead.ItemX = FTail.ItemX then
  begin
    Result := Copy(M.Text, FHead.TextX, FTail.TextX - FHead.TextX + 1);
    Exit;
  end;

  if FHead.TextX = 0 then
    Result := M.Text else
    Result := Copy(M.Text, FHead.TextX, Length(M.Text));

  for I := FHead.ItemX + 1 to FTail.ItemX - 1 do
    Result := Result + sLineBreak + FMemo.FLines[I].Text;

  M := FMemo.FLines[FTail.ItemX];
  if FTail.TextX = 0 then
    Result := Result + sLineBreak else
    Result := Result + sLineBreak + Copy(M.Text, 1, FTail.TextX);
end;

constructor TLySelection.Create(AMemo: TLyCodeMemo);
begin
  FMemo := AMemo;
  FHead.ItemX := -1;
  FHead.TextX := -1;
  FTail.ItemX := -1;
  FTail.TextX := -1;
end;

procedure TLySelection.CutToClipboard;
begin
  CopyToClipboard;
  Delete;
end;

procedure TLySelection.Delete;
var
  F, L: TLyLine;
  S, E: integer;
begin
  if Selected then
  begin
    FMemo.BeginUpdate;
    try
      F := FMemo.FLines[FHead.ItemX];
      S := FHead.TextX;
      L := FMemo.FLines[FTail.ItemX];
      E := FTail.TextX;
      FMemo.FUpdate.UndoSaveChange(F.Index, F.FText); // save selection
      Unselect;
      FMemo.FCaret.MoveTo(F.Index, S);
      if F <> L then
      begin
        F.Delete(S, Length(F.FText));
        F.Add(Copy(L.FText, E + 1, Length(L.FText)));
        FMemo.FLines.Delete(F.Index + 1, L.Index);
      end
      else F.Delete(S, E - S + 1);
    finally
      FMemo.EndUpdate;
    end;
  end;
end;

function TLySelection.ActiveParagraph: TLyLine;
begin
  if PartialSelected then
    Result := FMemo.FLines[FHead.ItemX] else
    Result := FMemo.FCaret.Item;
end;

function TLySelection.ActualSelectAll: boolean;
var
  I: integer;
begin
  Result := FSelectAll or (FMemo.FLines.Count = 0);
  if not Result then
  begin
    I := FMemo.FLines.Count - 1;
    Result := (FTail.ItemX = I) and
              (FTail.TextX >= Length(FMemo.FLines[I].Text)) and
              (FHead.ItemX = 0) and
              (FHead.TextX >= 0) and
              (FHead.TextX <= 1);
  end;
end;

procedure TLySelection.UnSelect;
begin
  if Selected then
  begin
    Reset;
    FMemo.Invalidate;
  end;
end;

function TLySelection.Select(HI, HX, TI, TX: integer): boolean;
var
  I, H, T: integer;
begin
  H := FHead.ItemX;
  T := FTail.ItemX;
  if (H <> HI) or (FHead.TextX <> HX) or (T <> TI) or (FTail.TextX <> TX) then
  begin
    FHead.ItemX := HI;
    FHead.TextX := HX;
    FTail.ItemX := TI;
    FTail.TextX := TX;
    for I := 0 to FMemo.FLines.Count - 1 do
      if ((I >= H) and (I <= T)) or ((I >= HI) and (I <= TI)) then
        FMemo.FLines[I].Paint;
    FMemo.PaintLine80;
  end;
  Result := Selected;
  FMemo.NotifyStatus;
end;

procedure TLySelection.SelectAll;
begin
  FSelectAll := true;
  FHead.ItemX := -1;
  FHead.TextX := -1;
  FTail.ItemX := -1;
  FTail.TextX := -1;
  FMemo.Invalidate;
end;

procedure TLySelection.SetSelectAll(Value: boolean);
begin
  if FSelectAll <> Value then
  begin
    FSelectAll := Value;
    if not FSelectAll then UnSelect;
  end;
end;

procedure TLySelection.SetSelected(Value: boolean);
begin
  if not Value then UnSelect;
end;

procedure TLySelection.SetText(const Value: WideString);
begin
  FMemo.BeginUpdate;
  try
    Delete;
    FMemo.FCaret.Insert(Value);
  finally
    FMemo.EndUpdate;
  end;
end;

function TLySelection.SelectTo(ItemX, TextX: integer): boolean;
var
  IX, TX, F: integer;
begin
  if FSelectAll then Reset;

  if not Selected then
  begin
    FFrom.ItemX := FMemo.FCaret.ItemIndex;
    FFrom.TextX := FMemo.FCaret.TextIndex;
  end;

  IX := FFrom.ItemX;
  TX := FFrom.TextX;

  if (IX >= 0) and (TX >= 0) then
  begin
    F := CompareTextPos(@FFrom, ItemX, TextX);
    if F > 0 then
    begin
      if TX > 0 then Dec(TX);
      Select(ItemX, TextX, IX, TX);
    end
    else
    if F < 0 then
    begin
      if TextX > 0 then Dec(TextX);
      Select(IX, TX, ItemX, TextX);
    end
    else UnSelect;
  end;

  Result := Selected;
end;

function TLySelection.Contains(ItemX, TextX: integer): boolean;
var
  L, R, V: int64;
  M: integer;
begin
  Result := Selected and
           (ItemX >= 0) and
           (ItemX < FMemo.FLines.Count) and
           (TextX >= 0) and
           (TextX <= Length(FMemo.FLines[ItemX].Text) + 1);
  if Result and not FSelectAll then
  begin
    M := Max(TextX, Max(FHead.TextX, FTail.TextX)) + 1;
    V := (ItemX * M) + TextX;
    L := (FHead.ItemX * M) + FHead.TextX;
    if FTail.TextX > 0 then
      R := (FTail.ItemX * M) + FTail.TextX else
      R := (FTail.ItemX * M);
    Result := (L <= V) and (V <= R);
  end;
end;

function TLySelection.Contains(ItemX: integer): boolean;
begin
  Result := FSelectAll or (Selected and
    (ItemX >= FHead.ItemX) and (ItemX <= FTail.ItemX));
end;

procedure TLySelection.CopyToClipboard;
begin
  if Selected then
    Clipboard.AsText := WideToStr(GetText);
end;

procedure TLySelection.MouseMove(P: TLyLine; TextX: integer);
begin
  if FSelecting then
  begin
    SelectTo(P.Index, TextX);
    FMemo.FCaret.MoveTo(P.Index, TextX);
    FMemo.FCaret.MakeVisible;
  end;
end;

procedure TLySelection.MarkCode(IsCode: boolean);
begin
  if FSelectAll then
    MarkCode(IsCode, 0, FMemo.FLines.Count - 1) else
  if Selected then
    MarkCode(IsCode, FHead.ItemX, FTail.ItemX) else
  if FMemo.FCaret.Active then
    MarkCode(IsCode, FMemo.FCaret.FItemX, FMemo.FCaret.FItemX);
end;

procedure TLySelection.MarkCode(IsCode: boolean; BegLineX, EndLineX: integer);
var
  L: TLyLine;
  S: WideString;
begin
  FMemo.BeginUpdate;
  try
    while BegLineX <= EndLineX do
    begin
      L := FMemo.FLines[BegLineX];
      S := L.Text;
      if IsCode then
      begin
        if (S = '') or (S[1] <> '%') then
          L.Text := '% ' + S;
      end
      else
      if (S <> '') and (S[1] = '%') then
      begin
        if (Length(S) > 1) and (S[2] <= ' ') then
          L.Text := Copy(S, 3, Length(S)) else
          L.Text := Copy(S, 2, Length(S));
      end;
      Inc(BegLineX);
    end;
  finally
    FMemo.EndUpdate;
  end;
end;

procedure TLySelection.MouseDown(P: TLyLine; TextX: integer);
begin
  UnSelect;
  FSelecting := true;
  FFrom.ItemX := P.Index;
  FFrom.TextX := TextX;
  FMemo.FCaret.MoveTo(FFrom.ItemX, TextX);
  FMemo.FCaret.MakeVisible;
  FMemo.NotifyStatus;
end;

procedure TLySelection.PasteFromClipboard;
begin
  if FMemo.FCaret.Active then
  begin
    FMemo.BeginUpdate;
    try
      Delete;
      FMemo.FCaret.Insert(StrToWide(Clipboard.AsText));
    finally
      FMemo.EndUpdate;
    end;
  end;
end;

procedure TLySelection.Reset;
begin
  FSelectAll := false;
  Select(-1, -1, -1, -1);
end;

{ TLyLineList }

function TLyLineList.Add(const S: WideString): TLyLine;
begin
  FMemo.BeginUpdate;
  try
    Result := TLyLine.Create(Self);
    FItems.Add(Result);
    Result.FText := S;
    Result.Change;
  finally
    FMemo.EndUpdate;
  end;
end;

procedure TLyLineList.Assign(Source: TPersistent);

  procedure assign_lines(Lines: TLyLineList);
  var
    I, X: integer;
  begin
    FMemo.BeginUpdate;
    try
      FMemo.FCaret.MoveTo(0, 1);
      X := FItems.Count;
      I := 0;
      while I < Lines.Count do
      begin
        if I < X then
          GetItem(I).Assign(Lines[I]) else
          Add.Assign(Lines[I]);
        Inc(I);
      end;

      while X > I do
      begin
        Dec(X);
        Delete(X);
      end;

      if I = 0 then Add;
      FMemo.Modified := true;
    finally
      FMemo.EndUpdate;
    end;
  end;

  procedure assign_strings(Lines: TStrings);
  var
    I, X: integer;
  begin
    FMemo.BeginUpdate;
    try
      FMemo.FCaret.MoveTo(0, 1);
      X := FItems.Count;
      I := 0;
      while I < Lines.Count do
      begin
        if I < X then
          GetItem(I).SetText(StrToWide(Lines[I])) else
          Add(StrToWide(Lines[I]));
        Inc(I);
      end;

      while X > I do
      begin
        Dec(X);
        Delete(X);
      end;

      if I = 0 then Add;
      FMemo.Modified := true;
    finally
      FMemo.EndUpdate;
    end;
  end;

begin
  if Source is TLyLineList then
    assign_lines(TLyLineList(Source)) else
  if Source is TStrings then
    assign_strings(TStrings(Source)) else
    inherited;
end;

procedure TLyLineList.Clear;
var
  I: integer;
begin
  BeginUpdate;
  try
    for I := GetCount - 1 downto 0 do
      Delete(I);
  finally
    EndUpdate;
  end;
end;

constructor TLyLineList.Create(AMemo: TLyCodeMemo);
begin
  FMemo := AMemo;
  FItems := TList.Create;
end;

procedure TLyLineList.Delete(Index: integer);
var
  L: TLyLine;
begin
  BeginUpdate;
  try
    L := GetItem(Index);
    FMemo.FUpdate.UndoSaveDelete(Index, L.FText);
    FItems.Delete(Index);
    L.FList := nil;
    L.Free;
  finally
    EndUpdate;
  end;
end;

procedure TLyLineList.DecorateFrom(StartX: integer);
var
  S: TLyHilighter;
  I: integer;
  L: TLyLine;
  T: TLyTokenID;
begin
  if Has(StartX) then
  begin
    S := FMemo.Hilighter;
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

procedure TLyLineList.Delete(BegIndex, EndIndex: integer);
var
  I: integer;
begin
  BeginUpdate;
  try
    for I := EndIndex downto BegIndex do
      Delete(I);
  finally
    EndUpdate;
  end;
end;

destructor TLyLineList.Destroy;
begin
  if FItems <> nil then
  begin
    Clear;
    FreeAndNil(FItems);
  end;
  inherited;
end;

procedure TLyLineList.EndUpdate;
begin
  FMemo.EndUpdate;
end;

function TLyLineList.FindByLine(var LineIndex: integer; Anyway: boolean): TLyLine;
var
  I: integer;
begin
  I := IndexByLine(LineIndex);
  if I >= 0 then
    Result := GetItem(I) else
    Result := nil;

  if (Result = nil) and Anyway then
    if LineIndex <= 0 then
    begin
      Result := First;
      LineIndex := 0;
    end
    else
    begin
      Result := Last;
      LineIndex := Result.LineCount - 1;
    end;
end;

function TLyLineList.GetCaretLine(CaretY: integer; var LineX: integer): TLyLine;
var
  I: integer;
begin
  if CaretY >= 0 then
    if not FMemo.WrapLine then
    begin
      if CaretY < GetCount then
      begin
        Result := GetItem(CaretY);
        LineX := 0;
        Exit;
      end;
    end
    else
    for I := 0 to GetCount - 1 do
    begin
      Result := GetItem(I);
      if Result.LineCount > CaretY then
      begin
        LineX := CaretY;
        Exit;
      end;
      Dec(CaretY, Result.LineCount);
    end;
  Result := nil;
end;

function TLyLineList.GetCount: integer;
begin
  if FItems <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function TLyLineList.GetFirst: TLyLine;
begin
  Result := TLyLine(FItems.First);
end;

function TLyLineList.GetHeight: integer;
begin
  Result := LineCount * FMemo.LineHeight;
end;

function TLyLineList.GetItem(Index: integer): TLyLine;
begin
  Result := TLyLine(FItems[Index]);
end;

function TLyLineList.GetLast: TLyLine;
begin
  Result := TLyLine(FItems.Last);
end;

function TLyLineList.GetLineCount: integer;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to FItems.Count - 1 do
    Inc(Result, GetItem(I).LineCount);
end;

function TLyLineList.IndexByLine(var LineIndex: integer): integer;
var
  I: integer;
  P: TLyLine;
begin
  if LineIndex >= 0 then
    for I := 0 to FItems.Count - 1 do
    begin
      P := GetItem(I);
      if LineIndex < P.LineCount then
      begin
        Result := I;
        Exit;
      end;
      Dec(LineIndex, P.LineCount);
    end;
  Result := -1;
end;

function TLyLineList.IndexOf(P: TLyLine): integer;
begin
  Result := FItems.IndexOf(P);
end;

function TLyLineList.Insert(Index: integer; const S: WideString): TLyLine;
var
  X: integer;
begin
  Result := nil;
  if FMemo.StatusOK then
  begin
    X := Max(0, Min(Count, Index));
    FMemo.BeginUpdate;
    try
      Result := TLyLine.Create(Self);
      FItems.Insert(X, Result);
      Result.FText := S;
      Result.Change;
      FMemo.FUpdate.UndoSaveAdd(X);
    finally
      FMemo.EndUpdate;
    end;
  end;
end;

function TLyLineList.InsertAfter(P: TLyLine; const S: WideString): TLyLine;
var
  I: integer;
begin
  if P <> nil then
  begin
    I := FItems.IndexOf(P);
    if I >= 0 then
      Result := Insert(I + 1, S) else
      Result := nil;
  end
  else Result := Add(S);
end;

procedure TLyLineList.LoadFromFile(const FileName: string);
var
  L: TStrings;
  F: boolean;
begin
  FMemo.BeginUpdate;
  try
    L := TStringList.Create;
    try
      L.Text := GetFileUTFS(FileName);
      F := FMemo.Readonly;
      FMemo.Readonly := true;
      Assign(L);
      FMemo.Readonly := F;
      FMemo.Modified := false;
    finally
      L.Free;
    end;
  finally
    FMemo.EndUpdate;
  end;
end;

procedure TLyLineList.LoadFromStream(const AStream: TStream);
var
  L: TStrings;
  F: boolean;
begin
  FMemo.BeginUpdate;
  try
    L := TStringList.Create;
    try
      L.LoadFromStream(AStream{$IFDEF UNICODE}, TEncoding.UTF8{$ENDIF});
      F := FMemo.Readonly;
      FMemo.Readonly := true;
      Assign(L);
      FMemo.Readonly := F;
      FMemo.Modified := false;
    finally
      L.Free;
      FMemo.Readonly := F;
    end;
  finally
    FMemo.EndUpdate;
  end;
end;

procedure TLyLineList.SetChanged(Value: boolean);
var
  I: integer;
  L: TLyLine;
begin
  for I := 0 to GetCount - 1 do
  begin
    L := GetItem(I);
    L.FChanged := Value;
    if Value then
      L.FLastToken := -1;
  end;
end;

procedure TLyLineList.SetModified(Value: boolean);
var
  I: integer;
  L: TLyLine;
begin
  for I := 0 to GetCount - 1 do
  begin
    L := GetItem(I);
    L.FModified := Value;
  end;
  FMemo.FGutter.Invalidate;
end;

procedure TLyLineList.SetNeedPaint(Value: boolean);
var
  I: integer;
begin
  for I := 0 to GetCount - 1 do
    GetItem(I).FNeedPaint := Value;
end;

procedure TLyLineList.SaveToFile(const FileName: string);
var
  L: TStrings;
begin
  L := TStringList.Create;
  try
    AssignTo(L);
    {$IFNDEF FPC}
    L.WriteBOM := false;
    {$ENDIF}
    L.SaveToFile(FileName{$IFDEF UNICODE}, TEncoding.UTF8{$ENDIF});
    FMemo.Modified := false;
  finally
    L.Free;
  end;
end;

procedure TLyLineList.SaveToStream(const AStream: TStream);
var
  L: TStrings;
begin
  L := TStringList.Create;
  try
    AssignTo(L);
    {$IFNDEF FPC}
    L.WriteBOM := false;
    {$ENDIF}
    L.SaveToStream(AStream{$IFDEF UNICODE}, TEncoding.UTF8{$ENDIF});
    FMemo.Modified := false;
  finally
    L.Free;
  end;
end;

procedure TLyLineList.AssignTo(Source: TPersistent);
var
  I: integer;
  L: TStrings;
begin
  if Source is TLyLineList then
    Source.Assign(Self) else
  if Source is TStrings then
  begin
    L := TStrings(Source);
    for I := 0 to GetCount - 1 do
      L.Add(WideToStr(GetItem(I).Text));
  end
  else inherited;
end;

procedure TLyLineList.BeginUpdate;
begin
  FMemo.BeginUpdate;
end;

procedure TLyLineList.SetText(const Value: WideString);
var
  L: TStrings;
begin
  L := TStringList.Create;
  try
    L.Text := WideToStr(Value);
    Assign(L);
  finally
    L.Free;
  end;
end;

function TLyLineList.GetText: WideString;
var
  I: integer;
begin
  if GetCount > 0 then
  begin
    Result := GetItem(0).Text;
    for I := 1 to GetCount - 1 do
      Result := Result + sLineBreak + GetItem(I).Text;
  end
  else Result := '';
end;

function TLyLineList.Has(Index: integer): boolean;
begin
  Result := (Index >= 0) and (Index < FItems.Count);
end;

{ TLyLine }

function TLyLine.TextPos(TextX: integer): TPoint;
var
  I, X: integer;
begin
  if TextX <= 1 then
  begin
    Result.X := Left;
    Result.Y := FTop + FList.FMemo.FLineSpacing;
  end
  else
  begin
    if TextX <= Length(FText) then
    begin
      Result.Y := FTop + FList.FMemo.FLineSpacing;
      X := 1;
      for I := 0 to LineCount - 1 do
      begin
        if TextX <= FL[I] then
        begin
          Result.X := Left;
          while TextX > 1 do
          begin
            Inc(Result.X, TextWidth(FText[X]));
            Inc(X);
            Dec(TextX);
          end;
          Exit;
        end;
        Inc(X, FL[I]);
        Dec(TextX, FL[I]);
        Inc(Result.Y, FList.FMemo.FLineHeight);
      end;
    end;
    I := LineCount - 1;
    Result.X := Left + FW[I];
    Result.Y := FTop + FList.FMemo.FLineSpacing + (I * FList.FMemo.FLineHeight);
  end;
end;

constructor TLyLine.Create(Lines: TLyLineList);
begin
  SetLength(FL, 1);
  FL[0] := 0;
  SetLength(FW, 1);
  FW[0] := 0;
  FLastToken := -1;
  FList := Lines;
  if FList <> nil then
  begin
    FChanged := true;
    FModified := true;
    FNeedPaint := true;
    Include(FList.FMemo.FState, msModified);
  end;
end;

function TLyLine.CutFrom(TextX: integer): WideString;
begin
  FList.FMemo.BeginUpdate;
  try
    if TextX <= 1 then
    begin
      Result := FText;
      SetText('');
    end
    else
    if TextX <= Length(FText) then
    begin
      FList.FMemo.FUpdate.UndoSaveChange(Index, FText);
      Result := Copy(FText, TextX, Length(FText));
      FText := Copy(FText, 1, TextX - 1);
      Change;
    end
    else Result := '';
  finally
    FList.FMemo.EndUpdate;
  end;
end;

function TLyLine.BreakFromTail: TLyLine;
begin
  Result := FList.InsertAfter(Self);
end;

procedure TLyLine.BeginUpdate;
begin
  FList.FMemo.BeginUpdate;
end;

function TLyLine.BreakFrom(TextX: integer): TLyLine;
begin
  FList.FMemo.BeginUpdate;
  try
    Result := FList.Insert(Index + 1, CutFrom(TextX));
  finally
    FList.FMemo.EndUpdate;
  end;
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

function TLyLine.SeekCurrWord(TextX: integer; var Len: integer): integer;
var
  L, I, X: integer;
begin
  Result := 0;
  Len := 0;
  L := Length(FText);
  if (L > 0) and (TextX <= L) then
  begin
    I := Max(TextX, 1);
    X := I + 1;
    if CharInSet(FText[I], CS_ID + ['$', '#']) then
    begin
      while (I > 1) and CharInSet(FText[I - 1], CS_ID + ['$', '#']) do Dec(I);
      while (X <= L) and CharInSet(FText[X], CS_ID + ['$', '#']) do Inc(X);
    end;
    Result := I;
    Len := X - I;
  end;
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

procedure TLyLine.SetText(const Value: WideString);
begin
  if FText <> Value then
  begin
    FList.BeginUpdate;
    try
      FList.FMemo.FUpdate.UndoSaveChange(Index, FText);
      FText := Value;
      SetLength(FL, 0);
      Change;
    finally
      FList.EndUpdate;
    end;
  end;
end;

function TLyLine.Delete(TextX: integer; Count: integer): integer;
var
  S: WideString;
begin
  S := FText;
  System.Delete(S, TextX, Count);
  Result := Length(FText) - Length(S);
  if Result > 0 then
  begin
    FList.FMemo.FUpdate.UndoSaveChange(Index, FText);
    FText := S;
    Change;
  end;
end;

destructor TLyLine.Destroy;
begin
  SetLength(FL, 0);
  SetLength(FW, 0);
  FText := '';
  if FList <> nil then
  begin
    FList.FMemo.BeginUpdate;
    try
      Include(FList.FMemo.FState, msModified);
      FList.FItems.Remove(Self);
    finally
      FList.FMemo.EndUpdate;
    end;
  end;
  inherited;
end;

procedure TLyLine.DrawLine(LineX, X, Y, BegSel, EndSel: integer);
var
  M: TLyCodeMemo;
  S, N: boolean;
  L, F, K: integer;
  P: TCanvas;
begin
  M := FList.FMemo;
  M.SetCanvas(false);
  P := M.Canvas;
  P.FillRect(Rect(X, Y, X + M.FBodyWidth, Y + M.FLineHeight));
  L := FL[LineX];
  if L > 0 then
  begin
    Inc(Y, M.FLineSpacing);
    K := FirstText(LineX);
    while L > 0 do
    begin
      S := (K >= BegSel) and (K <= EndSel);
      F := K;
      repeat
        Inc(K);
        Dec(L);
        N := (K >= BegSel) and (K <= EndSel);
      until (L = 0) or (S <> N) or (FTokens[F - 1] <> FTokens[K - 1]);
      FList.FMemo.SetCanvas(S);
      TokenSetCanvas(FList.FMemo.Canvas, FTokens[F - 1], S);
      Inc(X, OutText(X, Y, Copy(FText, F, K - F)));
    end;
  end;
end;

procedure TLyLine.EndUpdate;
begin
  FList.FMemo.EndUpdate;
end;

function TLyLine.HeadTextX: integer;
begin
  if FText <> '' then Result := 1 else Result := 0;
end;

function TLyLine.GetCaretY: integer;
{var
  I: integer;
  L: TLyLine;}
begin
  if FList.FMemo.FLineHeight > 0 then
    Result := (FTop div FList.FMemo.FLineHeight) else
    Result := 0;
{ Result := 0;
  for I := 0 to FList.Count - 1 do
  begin
    L := FList[I];
    if L <> Self then
      Inc(Result, L.LineCount) else
      Exit;
  end;}
end;

function TLyLine.GetHeight: integer;
begin
  Result := FList.FMemo.FLineHeight * LineCount;
end;

function TLyLine.GetIndex: integer;
begin
  Result := FList.IndexOf(Self);
end;

function TLyLine.GetLeft: integer;
begin
  Result := FList.FMemo.FMargin;
  if FList.FMemo.FGutter.Visible then
    Inc(Result, FList.FMemo.FGutter.Width);
end;

function TLyLine.GetPriorToken: TLyTokenID;
begin
  if Index = 0 then
    Result := TK_SPACE else
    Result := Prev.FLastToken;
end;

function TLyLine.LeadingSpaceCount: integer;
var
  I: integer;
begin
  for I := 1 to Length(FText) do
    if FText[I] <> ' ' then
    begin
      Result := I - 1;
      Exit;
    end;
  Result := 0;
end;

function TLyLine.LeadingSpaces: WideString;
begin
  Result := Copy(FText, 1, LeadingSpaceCount);
end;

function TLyLine.LineCount: integer;
begin
  Result := Length(FL);
end;

function TLyLine.StartLine: integer;
var
  I, X: integer;
  P: TLyLine;
begin
  Result := -1;
  X := 0;
  for I := 0 to FList.Count - 1 do
  begin
    P := FList[I];
    if P = Self then
    begin
      Result := X;
      Exit;
    end;
    Inc(X, P.LineCount);
  end;
end;

function TLyLine.Merge(P: TLyLine): boolean;
begin
  Result := (P <> nil) and (P <> Self);
  if Result then
  begin
    if P.FText <> '' then
    begin
      FText := FText + P.FText;
      Change;
    end;
    P.Free;
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
    FList.BeginUpdate;
    try
      Add(L.FText);
      FList.Delete(L.Index);
    finally
      FList.EndUpdate;
    end;
  end;
end;

function TLyLine.Next: TLyLine;
var
  I: integer;
begin
  I := FList.IndexOf(Self) + 1;
  if (I >= 1) and (I < FList.Count) then
    Result := FList[I] else
    Result := nil;
end;

function TLyLine.TextLine(TextX: integer): integer;
var
  I, L: integer;
begin
  L := LineCount;
  for I := 0 to L - 1 do
  begin
    if TextX <= FL[I] then
    begin
      Result := I;
      Exit;
    end;
    Dec(TextX, FL[I]);
  end;
  Result := L - 1;
end;

function TLyLine.TextLine(TextX: integer; var Col: integer): integer;
var
  I, L: integer;
begin
  L := LineCount;
  Col := TextX;
  for I := 0 to L - 2 do
  begin
    if Col <= FL[I] then
    begin
      Result := I;
      Exit;
    end;
    Dec(Col, FL[I]);
  end;
  Result := L - 1;
end;

function TLyLine.TextOut(X, Y: integer; const S: WideString): boolean;
begin
  Result := (S <> '');
  if Result then
    FList.FMemo.Canvas.TextOut(X, Y, WideToStr(S));
end;

function TLyLine.OutText(X, Y: integer; const S: WideString): integer;
begin
  if TextOut(X, Y, S) then
    Result := TextWidth(S) else
    Result := 0;
end;

function TLyLine.FirstText(LineX: integer): integer;
var
  I: integer;
begin
  if FText <> '' then
  begin
    Result := 1;
    for I := 0 to LineX - 1 do
      Inc(Result, FL[I]);
  end
  else Result := 0;
end;

procedure TLyLine.Change;
begin
  FChanged := true;
  FModified := true;
  FNeedPaint := true;
  FLastToken := -1;
  FList.FMemo.Modified := true;
end;

function TLyLine.Add(const S: WideString): integer;
begin
  Result := Length(S);
  if Result > 0 then
  begin
    FList.FMemo.FUpdate.UndoSaveChange(Index, FText);
    FText := FText + S;
    Change;
  end;
end;

procedure TLyLine.Assign(P: TLyLine);
begin
  SetText(P.FText);
end;

function TLyLine.TextWidth(const S: WideString): integer;
begin
  if S <> '' then
    Result := FList.FMemo.Canvas.TextWidth(WideToStr(S)) else
    Result := 0;
end;

function TLyLine.HitCaret(X: integer): integer;
var
  L, I: integer;
begin
  L := 0;
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
    else Result := Length(FText) + 1 + (X - I) div FList.FMemo.FSpaceWidth;
  end
  else Result := 1;
end;

function TLyLine.HitText(X, LineX: integer): integer;
var
  I, L, E: integer;
begin
  Result := 0;
  if FText <> '' then
  begin
    for I := 0 to LineX - 1 do Inc(Result, FL[I]);
    Inc(Result);
    L := Left;
    if X > L then
      if X < L + FW[LineX] then
      begin
        E := Result + FL[LineX];
        while Result < E do
        begin
          Inc(L, TextWidth(FText[Result]));
          if X < L then Exit;
          Inc(Result);
        end;
      end
      else Inc(Result, FL[LineX]);
  end;
end;

function TLyLine.Insert(TextX: integer; const S: WideString): integer;
begin
  Result := Length(S);
  if Result > 0 then
  begin
    FList.FMemo.FUpdate.UndoSaveChange(Index, FText);
    if TextX <= 1 then FText := S + FText else
    if TextX > Length(FText) then FText := FText + S else
      System.Insert(S, FText, TextX);
    Change;
  end;
end;

procedure TLyLine.Paint;
//var
//  I: integer;
begin
  if FLastToken < 0 then FList.DecorateFrom(Index);
//  for I := 0 to LineCount - 1 do
//    PaintLine(I);
  PaintAt(Left, FTop - FList.FMemo.TopOffset);
end;

procedure TLyLine.PaintAt(X, Y: integer);
var
  M: TLyCodeMemo;
  I, B, E: integer;
begin
  M := FList.FMemo;
  {$IFDEF FPC}
  B := 0;
  E := 0;
  {$ENDIF}
  M.FSelection.GetRange(Index, B, E);
  for I := 0 to LineCount - 1 do
  begin
    DrawLine(I, X, Y, B, E);
    Inc(Y, M.FLineHeight);
  end;
end;

procedure TLyLine.PaintLine(LineX: integer);
var
  M: TLyCodeMemo;
  B, E: integer;
begin
  M := FList.FMemo;
  {$IFDEF FPC}
  B := 0;
  E := 0;
  {$ENDIF}
  M.FSelection.GetRange(Index, B, E);
  DrawLine(LineX, Left, FTop - M.TopOffset + (LineX * M.FLineHeight), B, E);
end;

function TLyLine.Prev: TLyLine;
var
  I: integer;
begin
  I := FList.IndexOf(Self) - 1;
  if I >= 0 then
    Result := FList[I] else
    Result := nil;
end;

procedure TLyLine.Resize;
var
  X, Z, L, I, W, T: integer;
begin
  W := FList.FMemo.FBodyWidth;
  L := Length(FText);
  Z := TextWidth(FText);

  SetLength(FL, 1);
  SetLength(FW, 1);

  if (L <= 1) or (Z <= W) or not FList.FMemo.WrapLine then
  begin
    FL[0] := L;
    FW[0] := Z;
    Exit;
  end;

  FL[0] := 0;
  FW[0] := 0;
  X := 0;
  I := 1;
  repeat
    T := TextWidth(FText[I]);
    if (FW[X] + T > W) and (FL[X] > 0) then
    begin
      Inc(X);
      SetLength(FL, X + 1);
      SetLength(FW, X + 1);
      if Z <= W then
      begin
        FL[X] := L - I + 1;
        FW[X] := Z;
        Exit;
      end;
      FL[X] := 0;
      FW[X] := 0;
    end;
    Inc(FL[X]);
    Inc(FW[X], T);
    Dec(Z, T);
    Inc(I);
  until I > L;
end;

function TLyLine.TailTextX: integer;
begin
  Result := Length(FText);
  if Result > 0 then Inc(Result);
end;

{ TLyGutter }

function TLyGutter.AdjustWidth: integer;
begin
  Font.Assign(FMemo.Font);
  Font.Size := Font.Size - Max(1, Font.Size div 10);
  Canvas.Font.Assign(Font);
  FTextHeight := Canvas.TextHeight('H');
  Result := Canvas.TextWidth('99999') + CM_LBDELTA; { FTextHeight * 2; // without linemark}
  if Result <> Width then
  begin
    Invalidate;
    Width := Result;
  end;
end;

constructor TLyGutter.Create(AOwner: TComponent);
begin
  inherited;
  Color := clBtnFace;
end;

function TLyGutter.GetLineHeight: integer;
begin
  Result := FMemo.FLineHeight;
end;

function TLyGutter.GetVertLinePos: integer;
begin
  Result := Width - 1;
end;

procedure TLyGutter.Paint;
var
  I, X: integer;
begin
  X := FMemo.TopCaretY;
  for I := 0 to FMemo.FBodyLines do
    DrawLine(I + X);
end;

procedure TLyGutter.DrawLine(LineX: integer);
var
  X, Y, I, P, W: integer;
  S: string;
  F: boolean;
  L: TLyLine;
begin
  Y := (LineX - FMemo.TopCaretY) * LineHeight;
  if (Y >= 0) and (Y < Height) then
  begin
    {$IFDEF FPC}X := 0;{$ENDIF}
    L := FMemo.FLines.GetCaretLine(LineX, X);
    F := (L <> nil) and FMemo.FCaret.Visible and
      FMemo.FCaret.Active and
      (LineX = FMemo.FCaret.FPos.Y div LineHeight);

    // 1.draw line mark
    X := 0;
    P := Y + (LineHeight - FTextHeight) div 2;
    {
    if (L <> nil) and (L.FLineMark <> lmNone) then
    begin
      S := IntToStr(Ord(L.FLineMark) - 1);
      W := Canvas.TextWidth(S) + 4;
      Canvas.Brush.Color := FEdit.Palette.LineMarkBackground;
      Canvas.Font.Color := FEdit.Palette.SelectedTextColor;
      Canvas.FillRect(Rect(X, Y, X + W, Y + H));
      Canvas.TextOut(2, P, S);
      Inc(X, W);
    end; }

    // 2. clear background
    I := VertLinePos;
    if F then
      Canvas.Brush.Color := Palette.ActiveBackground else
      Canvas.Brush.Color := Palette.LeftBarBackground;
    Canvas.FillRect(Rect(X, Y, I, Y + LineHeight));

    // 3.draw line number
    if L <> nil then
      if F or ((LineX + 1) mod 10 = 0) then
      begin
        S := IntToStr(LineX + 1);
        W := Canvas.TextWidth(S);
        if F then
          Canvas.Font.Color := Palette.SpaceColor else
          Canvas.Font.Color := clGray;
        Canvas.TextOut(I - CM_LBDELTA - W, P, S);
      end
      else
      begin
        W := Canvas.TextWidth('9');
        if (LineX + 1) mod 5 = 0 then
          X := I - CM_LBDELTA - W else
          X := I - CM_LBDELTA - W div 2;
        Canvas.Pen.Color := clGray;
        Canvas.MoveTo(X, Y + LineHeight div 2);
        Canvas.LineTo(I - CM_LBDELTA - 1, Y + LineHeight div 2);
      end;

    // 4.draw changed bar
    if (L <> nil) and L.FModified then
    begin
      Canvas.Pen.Color := Palette.ChangedBackground;
      Canvas.MoveTo(I - 2, Y);
      Canvas.LineTo(I - 2, Y + LineHeight);
      Canvas.MoveTo(I - 3, Y);
      Canvas.LineTo(I - 3, Y + LineHeight);
//    Canvas.MoveTo(I - 4, Y);
//    Canvas.LineTo(I - 4, Y + LineHeight);
    end;

    // 5.draw vertical line
    Canvas.Pen.Color := clGray;
    Canvas.MoveTo(I, Y);
    Canvas.LineTo(I, Y + LineHeight);
  end;
end;

{ TLyUndoItem }

constructor TLyUndoItem.Create(AList: TLyUndoList);
var
  M: TLyCodeMemo;
begin
  FList := AList;
  FPrev := FList.FLast;
  FList.FLast := Self;
  M := FList.FMemo;
  FMagic := M.FUpdate.FMagic;
  FCaret.Y := M.FCaret.FItemX;
  FCaret.X := M.FCaret.FTextX;
  FFrom := M.FSelection.FFrom;
  FStart := M.FSelection.FHead;
  FEnd := M.FSelection.FTail;
end;

destructor TLyUndoItem.Destroy;
begin
  if FList <> nil then
    FList.FLast := FPrev;
  inherited;
end;

procedure TLyUndoItem.Apply;
var
  Edit: TLyCodeMemo;
begin
  try
    Edit := FList.FMemo;
    case FType of
      ctChangeText: Edit.Lines[FLine].SetText(FText);
      ctDeleteLine: Edit.Lines.Insert(FLine, FText);
      ctAddLine   : Edit.Lines.Delete(FLine);
    end;
    if (FPrev = nil) or (FPrev.FMagic <> FMagic) then
    begin
      Edit.FCaret.MoveTo(FCaret.Y, FCaret.X);
      Edit.FSelection.FFrom := FFrom;
      Edit.FSelection.Select(FStart.ItemX, FStart.TextX, FEnd.ItemX, FEnd.TextX);
    end;
  finally
    Free;
  end;
end;

{ TLyUndoList }

function TLyUndoList.GetMode: TLyChangeDeal;
begin
  if Self = FUpdate.FUndos then Result := cdUndo else
  if Self = FUpdate.FRedos then Result := cdRedo else Result := cdNone;
end;

constructor TLyUndoList.Create(AUpdate: TLyUpdate);
begin
  FUpdate := AUpdate;
  FMemo := FUpdate.FMemo;
end;

destructor TLyUndoList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TLyUndoList.Clear;
begin
  while FLast <> nil do FLast.Free;
end;

procedure TLyUndoList.Apply;
var
  M: cardinal;
begin
  if (FLast <> nil) and (FUpdate.FUndoMode = cdNone) then
  try
    FMemo.BeginUpdate;
    try
      FUpdate.FUndoMode := Mode;
      M := FLast.FMagic;
      repeat FLast.Apply until (FLast = nil) or (FLast.FMagic <> M);
    finally
      FMemo.EndUpdate;
      FMemo.MakeCaretVisible;
    end;
  finally
    FUpdate.FUndoMode := cdNone;
  end;
end;

{ TLyHilighter }

procedure TLyHilighter.Clear;
begin
  SetText('');
end;

function TLyHilighter.Next: boolean;
begin
  Result := (FPosition < FSize);
  if Result then
  begin
    Inc(FPosition);
    FChar := FText[FPosition];
  end
  else Stop;
end;

function TLyHilighter.ParseCString(var S: WideString): boolean;
var
  V: WideChar;
begin
  S := '';
  Result := (FChar = '"') and Next;
  if Result then
  begin
    while FChar <> '"' do
    begin
      {$IFDEF FPC}
      V := #0;
      {$ENDIF}
      Result := ParseEscChar(V) and NotEnded;
      if not Result then Exit;
      S := S + V;
    end;
    Result := (FChar = '"');
    if Result then Next;
  end;
end;

function TLyHilighter.ParseDecimal(var I: cardinal; N: integer): integer;
begin
  Result := 0;
  if (N > 0) and CharInSet(FChar, CS_DIGIT) then
  begin
    Dec(N);
    Inc(Result);
    I := Ord(FChar) - Ord('0');
    while Next and (N > 0) and CharInSet(FChar, CS_DIGIT) do
    begin
      I := (I * 10) + (Ord(FChar) - Ord('0'));
      Inc(Result);
      Dec(N);
    end;
  end;
end;

function TLyHilighter.ParseHex(var I: cardinal; N: integer): integer;
begin
  Result := 0;
  if (N > 0) and CharInSet(FChar, CS_HEX) then
  begin
    Dec(N);
    Inc(Result);
    I := HexValue(FChar);
    while Next and (N > 0) and CharInSet(FChar, CS_HEX) do
    begin
      I := (I * 16) + HexValue(FChar);
      Inc(Result);
      Dec(N);
    end;
  end;
end;

function TLyHilighter.First: boolean;
begin
  FPosition := 1;
  Result := (FPosition <= FSize);
  if Result then
    FChar := FText[FPosition] else
    FChar := #0;
end;

function TLyHilighter.GetEnded: boolean;
begin
  Result := (FPosition > FSize);
end;

function TLyHilighter.ParseCChar(var C: WideChar): boolean;
begin
  Result := (FChar = '''') and (GetChar <> '''');
  if Result then
  begin
    Result := ParseEscChar(C) and (FChar = '''');
    if Result then Next;
  end;
end;

function TLyHilighter.ParseChar(var C: WideChar): boolean;
begin
  if FChar = '#' then
    Result := ParsePChar(C) else
  if FChar = '''' then
    Result := ParseCChar(C) else
    Result := false;
end;

function TLyHilighter.ParseOctal(var I: cardinal; N: integer): integer;
begin
  Result := 0;
  if (N > 0) and CharInSet(FChar, CS_OCTAL) then
  begin
    Dec(N);
    Inc(Result);
    I := Ord(FChar) - Ord('0');
    while Next and (N > 0) and CharInSet(FChar, CS_OCTAL) do
    begin
      I := (I * 8) + (Ord(FChar) - Ord('0'));
      Inc(Result);
      Dec(N);
    end;
  end;
end;

function TLyHilighter.Seek(Chars: TSysCharSet): boolean;
begin
  while not CharInSet(FChar, Chars) and Next do;
  Result := NotEnded;
end;

function TLyHilighter.PeekNextChar: WideChar;
begin
  if FPosition < FSize then
    Result := FText[FPosition + 1] else
    Result := #0;
end;

function TLyHilighter.PeekNextString(Len: integer): WideString;
begin
  Result := Copy(FText, FPosition + 1, Len);
end;

function TLyHilighter.GetChar: WideChar;
begin
  Next;
  Result := FChar;
end;

function TLyHilighter.GetNotEnded: boolean;
begin
  Result := (FPosition <= FSize);
end;

function TLyHilighter.MatchChar(Chars: TSysCharSet): boolean;
begin
  Result := CharInSet(FChar, Chars);
end;

function TLyHilighter.MatchChar(Ch: WideChar): boolean;
begin
  Result := (FChar = Ch);
end;

function TLyHilighter.MatchDigit: boolean;
begin
  Result := MatchChar(CS_DIGIT);
end;

function TLyHilighter.MatchHead: boolean;
begin
  Result := MatchChar(CS_HEAD);
end;

function TLyHilighter.MatchSpace: boolean;
begin
  Result := MatchChar(CS_SPACE);
end;

function TLyHilighter.MatchString(const S: WideString): boolean;
begin
  Result := (S <> '') and (S = PeekString(Length(S)));
end;

function TLyHilighter.MatchText(const S: WideString): boolean;
begin
  Result := (S <> '') and SameText(WideToStr(S), WideToStr(PeekString(Length(S))));
end;

function TLyHilighter.Skip(Count: integer): boolean;
begin
  while (Count > 0) and Next do Dec(Count);
  Result := NotEnded;
end;

function TLyHilighter.ParsePChar(var C: WideChar): boolean;
var
  V: cardinal;
begin
  Result := (FChar = '#') and CharInSet(GetChar, CS_DIGIT + ['$']);
  if Result then
  begin
    {$IFDEF FPC}
    V := 0;
    {$ENDIF}
    if FChar = '$' then
      Result := Next and (ParseHex(V) > 0) else
      Result := (ParseDecimal(V) > 0);
    if Result then C := WideChar(V);
  end;
end;

function TLyHilighter.ParsePString(var S: WideString): boolean;
begin
  S := '';
  Result := (FChar = '''') and Next;
  if Result then
  begin
    repeat
      if (FChar = '''') and (GetChar <> '''') then Exit;
      S := S + FChar;
    until not Next;
    Result := (FChar = '''');
    if Result then Next;
  end;
end;

function TLyHilighter.ParseString(var S: WideString): boolean;
begin
  if FChar = '''' then
    Result := ParsePString(S) else
  if FChar = '"' then
    Result := ParseCString(S) else
    Result := false;
end;

function TLyHilighter.Pick(Chars: TSysCharSet): WideString;
var
  I: integer;
begin
  if CharInSet(FChar, Chars) then
  begin
    I := FPosition;
    while Next and CharInSet(FChar, Chars) do;
    Result := Copy(FText, I, FPosition - I);
  end
  else Result := '';
end;

function TLyHilighter.PickDecimal: WideString;
begin
  Result := Pick(CS_DIGIT);
end;

function TLyHilighter.PickHex: WideString;
begin
  Result := Pick(CS_HEX);
end;

function TLyHilighter.PickID: WideString;
begin
  Result := Pick(CS_ID);
end;

function TLyHilighter.PickExID: WideString;
{$IFDEF UNICODE}
var
  I: integer;
  {$ENDIF}
begin
  {$IFDEF UNICODE}
  if CharInSet(FChar, CS_ID) or (Ord(FChar) > 255) then
  begin
    I := FPosition;
    while Next and (CharInSet(FChar, CS_ID) or (Ord(FChar) > 255)) do;
    Result := Copy(FText, I, FPosition - I);
  end
  else Result := '';
  {$ELSE}
  Result := Pick(CS_ID);
  {$ENDIF}
end;

function TLyHilighter.PickOctal: WideString;
begin
  Result := Pick(CS_OCTAL);
end;

function TLyHilighter.PeekString(Len: integer): WideString;
begin
  Result := Copy(FText, FPosition, Len);
end;

function TLyHilighter.Seek(Ch: WideChar): boolean;
begin
  while (FChar <> Ch) and Next do;
  Result := NotEnded;
end;

procedure TLyHilighter.Stop;
begin
  FPosition := FSize + 1;
  FChar := #0;
end;

function TLyHilighter.ParseEscChar(var C: WideChar): boolean;
const
  CS_CESCAPE   = ['\', '''', '"', '?',
                  'n', 'r', 't', 'v', 'a', 'b', 'f', 'x', 'u',
                  '0'..'3'];
var
  V: cardinal;
begin
  if FChar <> '\' then
  begin
    Result := true;
    C := FChar;
    Next;
  end
  else
  begin
    Result := CharInSet(GetChar, CS_CESCAPE);
    if Result then
      if FChar = 'n' then begin C := #10; Next; end else // LF: LineFeed
      if FChar = 'r' then begin C := #13; Next; end else // CR: CarriageReturn
      if FChar = 't' then begin C := #9;  Next; end else // HT: Horz Tab
      if FChar = 'v' then begin C := #11; Next; end else // VT: Vert Tab
      if FChar = 'a' then begin C := #7;  Next; end else // BELL
      if FChar = 'b' then begin C := #8;  Next; end else // BS: BackSpace
      if FChar = 'f' then begin C := #12; Next; end else // FF: FormFeed
      if FChar = 'x' then // '\xXX'
      begin
        {$IFDEF FPC}
        V := 0;
        {$ENDIF}
        Result := Next and (ParseHex(V, 2) = 2);
        if Result then C := WideChar(V);
      end
      else
      if FChar = 'u' then // '\uXXXX'
      begin
        Result := Next and (ParseHex(V, 4) = 4);
        if Result then C := WideChar(V);
      end
      else
      if FChar = '0' then // '\0' or '\0oo'
      begin
        Result := (ParseOctal(V, 3) in [1, 3]);
        if Result then C := WideChar(V);
      end
      else
      if CharInSet(FChar, ['1'..'3']) then // '\ooo'
      begin
        Result := (ParseOctal(V, 3) = 3) and (V < 256);
        if Result then C := WideChar(V);
      end
      else
      begin
        C := FChar;  // '\', '''', '"', '?', ...
        Next;
      end;
  end;
end;

function TLyHilighter.SeekLineBreak: boolean;
begin
  while (FChar <> #13) and (FChar <> #10) and Next do;
  Result := NotEnded;
end;

function TLyHilighter.SeekSpace: boolean;
begin
  while (FChar > ' ') and Next do;
  Result := NotEnded;
end;

procedure TLyHilighter.SetText(const Value: WideString);
begin
  FText := Value;
  FSize := Length(FText);
  First;
end;

function TLyHilighter.Skip(Chars: TSysCharSet): boolean;
begin
  while CharInSet(FChar, Chars) and Next do;
  Result := NotEnded;
end;

function TLyHilighter.Skip(Ch: WideChar): boolean;
begin
  while (FChar = Ch) and Next do;
  Result := NotEnded;
end;

function TLyHilighter.SkipCString(OnQuote: boolean): boolean;
begin
  Result := not OnQuote or ((FChar = '"') and Next);
  if Result then
  begin
    while (FChar <> '"') and NotEnded do
    begin
      if FChar = '\' then Next;
      Next;
    end;
    Result := (FChar = '"');
    if Result then Next;
  end;
end;

function TLyHilighter.SkipLineBreak: boolean;
begin
  while ((FChar = #13) or (FChar = #10)) and Next do;
  Result := NotEnded;
end;

function TLyHilighter.SkipPString(OnQuote: boolean): boolean;
begin
  Result := not OnQuote or ((FChar = '''') and Next);
  if Result then
  begin
    repeat
      if FChar = '''' then
        if GetChar <> '''' then
          Exit;
    until not Next;
    Result := false;
  end;
end;

function TLyHilighter.SkipSpace: boolean;
begin
  while (FChar <= ' ') and Next do;
  Result := NotEnded;
end;

function TLyHilighter.SkipString(Endc: WideChar): boolean;
begin
  if Endc = '''' then
    Result := SkipPString(false) else
  if Endc = '"' then
    Result := SkipCString(false) else
  if Seek(Endc) then
  begin
    Result := true;
    Next;
  end
  else Result := false;
end;

function TLyHilighter.SkipTag(Endc: WideChar): boolean;
begin
  while Seek(['"', '''', char(Endc)]) and (FChar <> Endc) do SkipString;
  Result := (FChar = Endc);
  if Result then Next;
end;

function TLyHilighter.SkipString: boolean;
begin
  if FChar = '''' then
    Result := Next and SkipPString(false) else
  if FChar = '"' then
    Result := Next and SkipCString(false) else
    Result := true;
end;

function TLyHilighter.ParseTo(const EndStr: WideString; Token, FailToken: TLyTokenID): TLyTokenID;
var
  I, E: integer;
begin
  if EndStr <> '' then
  begin
    I := PosEx(EndStr, Text, Position);
    if I > 0 then
    begin
      E := I + Length(EndStr) - 1;
      while Position <= E do
        Next;
      Result := Token;
      Exit;
    end;
  end;
  Result := ParseToEnd(FailToken);
end;

function TLyHilighter.ParseToEnd(Token: TLyTokenID): TLyTokenID;
begin
  Stop;
  Result := Token;
end;

function TLyHilighter.ParseBlockComment(const EndStr: WideString; FailToken: TLyTokenID): TLyTokenID;
begin
  Result := ParseTo(EndStr, TK_COMMENT, FailToken);
end;

function TLyHilighter.ParseLineComment: TLyTokenID;
begin
  Result := ParseToEnd(TK_COMMENT);
end;

function TLyHilighter.GetTokenColor(Token: TLyTokenID): TColor;
begin
  Result := Palette.GetColor(Token);
end;

function TLyHilighter.GetTokenSize: integer;
begin
  Result := Position - FTokenPos;
end;

function TLyHilighter.GetClass: TLyHilighterClass;
begin
  Result := TLyHilighterClass(ClassType);
end;

procedure TLyHilighter.SetClass(Value: TLyHilighterClass);
var
  I: integer;
begin
  if Value = nil then Value := TLyHilighter;
  if Value <> ClassType then
  begin
    with FMemo do
    begin
      BeginUpdate;
      try
        FHilighter := Value.Create(FMemo);
        for I := 0 to FLines.Count - 1 do
        begin
          FHilighter.Decorate(FLines[I]);
          FLines[I].FNeedPaint := true;
        end;
      finally
        EndUpdate;
      end;
    end;
    Free;
  end;
end;

function TLyHilighter.GetTokenStr: WideString;
begin
  Result := Copy(FText, FTokenPos, Position - FTokenPos);
end;

function TLyHilighter.GetTokenStyle(Token: TLyTokenID): TFontStyles;
begin
  Result := Palette.GetStyle(Token);
end;

class function TLyHilighter.Language: string;
begin
  Result := '';
end;

class function TLyHilighter.FileExts: string;
begin
  Result := '';
end;

class function TLyHilighter.DefFileExt: string;
begin
  Result := '';
end;

class function TLyHilighter.CaseSensitive: boolean;
begin
  Result := false;
end;

class function TLyHilighter.WordWrap: boolean;
begin
  Result := true;
end;

function TLyHilighter.IsKeyword(const ID: string): boolean;
begin
  Result := false;
end;

function TLyHilighter.SkipChars(Chars: TSysCharSet): integer;
begin
  if MatchChar(Chars - [#0]) then
    Result := SkipNextChars(Chars) + 1 else
    Result := 0;
end;

function TLyHilighter.SkipNextChars(Chars: TSysCharSet): integer;
begin
  Result := 0;
  while Next and MatchChar(Chars) do Inc(Result);
end;

class function TLyHilighter.Support(const FileExt: string): boolean;
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

function TLyHilighter.ParseOne(Token: TLyTokenID): TLyTokenID;
begin
  Next;
  Result := Token;
end;

function TLyHilighter.ParseSpace: TLyTokenID;
begin
  Skip(CS_SPACE);
  Result := TK_SPACE;
end;

function TLyHilighter.MatchPrior(AToken: TLyTokenID): boolean;
begin
  Result := (FPrior = AToken);
end;

constructor TLyHilighter.Create(AMemo: TLyCodeMemo);
begin
  FMemo := AMemo;
end;

function TLyHilighter.Decorate(ALine: TLyLine): TLyTokenID;
var
  I: integer;
begin
  FPrior := ALine.PriorToken;
  if FPrior < 0 then
    FPrior := Decorate(ALine.Prev);
  Text := ALine.FText;
  SetLength(ALine.FTokens, Size);
  while Curr <> #0 do
  begin
    FTokenPos := FPosition;
    FPrior := GetNextToken;
    FTokenSize := FPosition - FTokenPos;
    for I := FTokenPos to Position - 1 do
      ALine.FTokens[I - 1] := FPrior;
  end;
  Text := '';
  Result := FPrior;
  ALine.FLastToken := FPrior;
end;

function TLyHilighter.GetNextToken: TLyTokenID;
begin
  Result := ParseToEnd(TK_SPACE);
end;

{ TLyPascalHilighter }

class function TLyPascalHilighter.DefFileExt: string;
begin
  Result := '.pas';
end;

class function TLyPascalHilighter.WordWrap: boolean;
begin
  Result := false;
end;

class function TLyPascalHilighter.FileExts: string;
begin
  Result := '.pas .pp .dpr .lpr .inc';
end;

function TLyPascalHilighter.IsKeyword(const ID: string): boolean;
begin
  Result := MatchID(ID, PascalKeywords);
end;

class function TLyPascalHilighter.Language: string;
begin
  Result := 'Pascal';
end;

function TLyPascalHilighter.ParseChar: TLyTokenID;
begin
  Result := TK_CHAR;
  Next;
  if SkipChars(CS_DIGIT) = 0 then
    if not MatchChar('$') or (SkipNextChars(CS_HEX) = 0) then
      Result := TK_UNKNOWN;
  if SkipChars(CS_ID) > 0 then
    Result := TK_UNKNOWN;
end;

function TLyPascalHilighter.ParseComment: TLyTokenID;
begin
  Result := ParseBlockComment('}', TK_ECOMMENT);
end;

function TLyPascalHilighter.ParseCommentP: TLyTokenID;
begin
  Result := ParseBlockComment('*)', TK_ECOMMENTP);
end;

function TLyPascalHilighter.ParseDelimiter: TLyTokenID;
begin
  Result := ParseOne(TK_DELIMITER);
end;

function TLyPascalHilighter.ParseDirective: TLyTokenID;
begin
  Result := ParseTo('}', TK_DIRECTIVE, TK_EDIRECTIVE);
end;

function TLyPascalHilighter.ParseHex: TLyTokenID;
begin
  if SkipNextChars(CS_HEX) = 0 then
    Result := TK_UNKNOWN else
    Result := TK_NUMBER;
  if SkipChars(CS_ID) > 0 then
    Result := TK_UNKNOWN;
end;

function TLyPascalHilighter.ParseKeywordID: TLyTokenID;
begin
  SkipNextChars(CS_ID);
  if IsKeyword(WideToStr(TokenStr)) then
    Result := TK_KEYWORD else
    Result := TK_ID;
end;

function TLyPascalHilighter.GetNextToken: TLyTokenID;
begin
  if MatchPrior(TK_EDIRECTIVE) then Result := ParseDirective else
  if MatchPrior(TK_ECOMMENT)   then Result := ParseComment else
  if MatchPrior(TK_ECOMMENTP)  then Result := ParseCommentP else
  if MatchString('{$')         then Result := ParseDirective else
  if MatchChar('{')            then Result := ParseComment else
  if MatchString('(*')         then Result := ParseCommentP else
  if MatchString('//')         then Result := ParseLineComment else
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

function TLyPascalHilighter.ParseNumber: TLyTokenID;
begin
  SkipNextChars(CS_DIGIT);
  if MatchChar('.') and CharInSet(PeekNextChar, CS_DIGIT) then
    SkipNextChars(CS_DIGIT);
  Result := TK_NUMBER;
end;

function TLyPascalHilighter.ParseOperator: TLyTokenID;
begin
  Result := ParseOne(TK_OPERATOR);
end;

function TLyPascalHilighter.ParseString: TLyTokenID;
begin
  Result := TK_STRING;
  repeat
    while Next and (Curr <> '''') do;
    if MatchChar(#0) then
      Result := TK_UNKNOWN else
      Next;
  until not MatchChar('''');
end;

function TLyPascalHilighter.ParseUnknown: TLyTokenID;
begin
  Result := ParseOne(TK_UNKNOWN);
end;

{ TLyseeHilighter }

class function TLyseeHilighter.DefFileExt: string;
begin
  Result := '.ls';
end;

class function TLyseeHilighter.FileExts: string;
begin
  Result := '.ls';
end;

function TLyseeHilighter.GetTokenColor(Token: TLyTokenID): TColor;
begin
  if Token in [TK_ESTRINGL, TK_ESTRING] then Token := TK_STRING;
  Result := inherited GetTokenColor(Token);
end;

function TLyseeHilighter.IsKeyword(const ID: string): boolean;
begin
  Result := MatchID(ID, LyseeKeywords);
end;

class function TLyseeHilighter.Language: string;
begin
  Result := 'Lysee';
end;

function TLyseeHilighter.GetNextToken: TLyTokenID;
begin
  if MatchPrior(TK_ECOMMENT)  then Result := ParseComment else
  if MatchPrior(TK_ECOMMENTP) then Result := ParseCommentP else
  if MatchPrior(TK_ESTRING)   then Result := ParseString else
  if MatchPrior(TK_ESTRINGL)  then Result := ParseStringL else
  if MatchChar('{')           then Result := ParseComment else
  if MatchString('(*')        then Result := ParseCommentP else
  if MatchChar('''')          then Result := ParseString else
  if MatchChar('"')           then Result := ParseStringL else
  if MatchString('//')        then Result := ParseLineComment else
  if MatchString('#!')        then Result := ParseToEnd(TK_DIRECTIVE) else
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

function TLyseeHilighter.ParseString: TLyTokenID;
begin
  Result := TK_STRING;
  repeat
    while Next and (Curr <> '''') do;
    if MatchChar(#0) then
      Result := TK_ESTRING else
      Next;
  until not MatchChar('''');
end;

function TLyseeHilighter.ParseStringL: TLyTokenID;
begin
  Result := TK_STRING;
  repeat
    while Next and not MatchChar('"') do
      if MatchChar('\') then Next;
    if MatchChar(#0) then
      Result := TK_ESTRINGL else
      Next;
  until not MatchChar('"');
end;

{ TLyRiviHilighter }

function TLyRiviHilighter.Decorate(ALine: TLyLine): TLyTokenID;
var
  T: TLyRiviLine;
begin
  T := FType;
  FType := rlUnknown;
  Result := inherited;
  FType := T;
end;

class function TLyRiviHilighter.DefFileExt: string;
begin
  Result := '.rivi';
end;

class function TLyRiviHilighter.WordWrap: boolean;
begin
  Result := true;
end;

class function TLyRiviHilighter.FileExts: string;
begin
  Result := '.rivi';
end;

function TLyRiviHilighter.IsKeyword(const ID: string): boolean;
begin
  Result := MatchID(ID, LyseeKeywords);
end;

class function TLyRiviHilighter.Language: string;
begin
  Result := 'rivi';
end;

function TLyRiviHilighter.GetNextToken: TLyTokenID;
begin
  if FType = rlUnknown then
    Result := ParseFirst else
  if FType = rlDirective then
    Result := ParseToEnd(TK_STRING) else
  if FType = rlParagraph then
    Result := ParseParagraph else
    Result := inherited;
end;

function TLyRiviHilighter.ParseParagraph: TLyTokenID;
begin
  if MatchChar('`') then
  begin
    Result := TK_RAWDATA;
    Next;
    Seek(['`']);
    Next;
  end
  else
  if MatchChar(['<', '>']) then
  begin
    Result := TK_TAG;
    SkipTag('>');
  end
  else
  if MatchChar(['[', '|', ']']) then
  begin
    if CharInSet(Text[1], ['[', '|']) then
    begin
      Result := TK_MARK;
      SkipChars(['|', '[', ']']);
      ParseSpace;
      FType := rlUnknown;
    end
    else
    begin
      Result := TK_SPACE;
      Seek(['`', '<', '>']);
    end;
  end
  else
  begin
    Result := TK_SPACE;
    Seek(['`', '<', '>', '[', '|', ']']);
  end;
end;

function TLyRiviHilighter.ParseFirst: TLyTokenID;
var
  H: char;
begin
  FType := rlParagraph;
  if MatchChar('@') then
  begin
    FType := rlDirective;
    if Seek([':']) then
    begin
      Next;
      Result := TK_MARK;
    end
    else Result := TK_UNKNOWN;
  end
  else
  if MatchChar('%') then
  begin
    FType := rlScript;
    Result := TK_MARK;
    Next;
    ParseSpace;
  end
  else
  if MatchChar('>') then
  begin
    Result := TK_MARK;
    SkipChars(['>', #1..' ']);
  end
  else
  if MatchChar(['=', '#', '*']) then
  begin
    Result := TK_MARK;
    H := Curr;
    SkipChars([H]);
    if CharInSet(H, ['#', '*']) then
      SkipChars(CS_ID + [H]);
  end
  else
  if MatchChar(CS_SPACE) then
  begin
    SkipSpace;
    if MatchChar(['#', '*']) then
    begin
      SkipChars(CS_ID + [char(Curr)]);
      SkipSpace;
      Result := TK_MARK;
    end
    else Result := TK_SPACE;
  end
  else Result := ParseParagraph;
end;

{ TLyCHilighter }

function TLyCHilighter.ParseString: TLyTokenID;
begin
  while Next and not MatchChar('"') do
    if MatchChar('\') then Next;
  if MatchChar('"') then
  begin
    Next;
    Result := TK_STRING;
  end
  else Result := TK_UNKNOWN;
end;

function TLyCHilighter.ParseBinary: TLyTokenID;
begin
  Next;
  if SkipNextChars(['0'..'1']) = 0 then
    Result := TK_UNKNOWN else
    Result := TK_NUMBER;
  if SkipChars(CS_ID) > 0 then
    Result := TK_UNKNOWN;
end;

function TLyCHilighter.ParseChar: TLyTokenID;
begin
  while Next and not MatchChar('''') do
    if MatchChar('\') then Next;
  if MatchChar('''') then
  begin
    Next;
    Result := TK_CHAR;
  end
  else Result := TK_UNKNOWN;
end;

function TLyCHilighter.ParseComment: TLyTokenID;
begin
  Result := ParseBlockComment('*/', TK_ECOMMENT);
end;

function TLyCHilighter.ParseHex: TLyTokenID;
begin
  Next;
  Result := inherited ParseHex;
end;

function TLyCHilighter.GetNextToken: TLyTokenID;
begin
  if MatchPrior(TK_ECOMMENT) then Result := ParseComment else
  if MatchString('/*')       then Result := ParseComment else
  if MatchString('//')       then Result := ParseLineComment else
  if MatchChar(CS_SPACE)     then Result := ParseSpace else
  if MatchChar(CS_HEAD)      then Result := ParseKeywordID else
  if MatchChar('"')          then Result := ParseString else
  if MatchChar('''')         then Result := ParseChar else
  if MatchText('0x')         then Result := ParseHex else
  if MatchText('0o')         then Result := ParseOctal else
  if MatchText('0b')         then Result := ParseBinary else
  if MatchChar(CS_DIGIT)     then Result := ParseNumber else
  if MatchChar(CS_OPERATOR)  then Result := ParseOperator else
  if MatchChar(CS_DELIMITER) then Result := ParseDelimiter else
                                   Result := ParseUnknown;
end;

function TLyCHilighter.ParseOctal: TLyTokenID;
begin
  Next;
  if SkipNextChars(['0'..'7']) = 0 then
    Result := TK_UNKNOWN else
    Result := TK_NUMBER;
  if SkipChars(CS_ID) > 0 then
    Result := TK_UNKNOWN;
end;

class function TLyCHilighter.Language: string;
begin
  Result := 'C';
end;

class function TLyCHilighter.FileExts: string;
begin
  Result := '.c .h';
end;

class function TLyCHilighter.DefFileExt: string;
begin
  Result := '.c';
end;

class function TLyCHilighter.CaseSensitive: boolean;
begin
  Result := true;
end;

class function TLyCHilighter.WordWrap: boolean;
begin
  Result := false;
end;

function TLyCHilighter.IsKeyword(const ID: string): boolean;
begin
  Result := MatchID(ID, CKeywords);
end;

{ TLyCppHilighter }

class function TLyCppHilighter.Language: string;
begin
  Result := 'C++';
end;

class function TLyCppHilighter.FileExts: string;
begin
  Result := '.c .h .cpp .hpp';
end;

class function TLyCppHilighter.DefFileExt: string;
begin
  Result := '.cpp';
end;

function TLyCppHilighter.IsKeyword(const ID: string): boolean;
begin
  Result := MatchID(ID, CppKeywords);
end;

{ TLyJavaHilighter }

class function TLyJavaHilighter.Language: string;
begin
  Result := 'Java';
end;

class function TLyJavaHilighter.FileExts: string;
begin
  Result := '.java';
end;

class function TLyJavaHilighter.DefFileExt: string;
begin
  Result := '.java';
end;

function TLyJavaHilighter.IsKeyword(const ID: string): boolean;
begin
  Result := MatchID(ID, JavaKeywords);
end;

initialization
begin
  HilightClasses.Add(TLyHilighter);
  HilightClasses.Add(TLyPascalHilighter);
  HilightClasses.Add(TLyseeHilighter);
  HilightClasses.Add(TLyRiviHilighter);
  HilightClasses.Add(TLyCHilighter);
  HilightClasses.Add(TLyCppHilighter);
  HilightClasses.Add(TLyJavaHilighter);
end;

end.
