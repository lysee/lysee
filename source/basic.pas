{==============================================================================}
{        UNIT: basic                                                           }
{ DESCRIPTION: lysee basic functions                                           }
{   COPYRIGHT: Copyright (c) 2003-2016, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/02/28                                                      }
{    MODIFIED: 2020/03/11                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit Basic;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, SyncObjs;

const

  CS_PATHDELIM = {$IFDEF MSWINDOWS}'\'{$ELSE}'/'{$ENDIF};
  CS_ALL       = [#0..#255];
  CS_SPACE     = [#1..' '];
  CS_PRINT     = ['!'..'~'];
  CS_QUOTE     = ['"', ''''];
  CS_DIGIT     = ['0'..'9'];
  CS_UPPER     = ['A'..'Z'];
  CS_LOWER     = ['a'..'z'];
  CS_ALPHA     = CS_UPPER + CS_LOWER;
  CS_ALNUM     = CS_ALPHA + CS_DIGIT;
  CS_UPNUM     = CS_UPPER + CS_DIGIT;
  CS_HEAD      = CS_ALPHA + ['_'];
  CS_ID        = CS_ALNUM + ['_'];
  CS_NOTID     = CS_ALL - CS_ID;
  CS_CONST     = CS_UPNUM + ['_'];
  CS_PUNCT     = ['!'..'~'] - CS_ALNUM;
  CS_CONTROL   = [#0..#31, #127];
  CS_HEX       = ['A'..'F', 'a'..'f'] + CS_DIGIT;
  CS_OCTAL     = ['0'..'7'];
  CS_LOWER_A   = Ord('a');
  CS_LOWER_F   = Ord('f');
  CS_LOWER_Z   = Ord('z');
  CS_UPPER_A   = Ord('A');
  CS_UPPER_F   = Ord('F');
  CS_UPPER_Z   = Ord('Z');
  CS_DISTANCE  = CS_LOWER_A - CS_UPPER_A;
  CS_DELIMITER = ['(', ')', '[', ']', '{', '}', '.', ',', ':', ';'];
  CS_OPERATOR  = CS_PRINT - CS_ID - CS_QUOTE - CS_DELIMITER;

//  LV_INVALIDHANDLE = {$IFDEF FPC}feInvalidHandle{$ELSE}INVALID_HANDLE_VALUE{$ENDIF};

type

  TLyFileEncoding = (
    feNone, feUTF8, feUTF16LE, feUTF16BE, feUCS4BE, feUCS4LE,
    feUCS4_2143, feUCS4_3412);

  TLyCompare  = (crEqual, crLess, crMore, crDiff);
  TLyCompares = set of TLyCompare;
  TLyIsString = (isNo, isPart, isYes);

  { TLyObject }

  TLyObject = class
  private
    FRefCount: integer;
  public
    function IncRefcount: integer;virtual;
    function DecRefcount: integer;virtual;
    property RefCount: integer read FRefCount;
  end;

  { TLyNameObject }

  TLyNameObject = class(TLyObject)
  protected
    FName: string;
  public
    constructor Create(const AName: string = '');
    function ToString: string;override;
    function NameIs(const AName: string): boolean;overload;
    function NameIs(const AName: array of string): boolean;overload;
    property Name: string read FName write FName;
  end;

  { TLyNameValue }

  TLyNameValue = class(TLyNameObject)
  private
    FValue: string;
  public
    constructor Create(const AName: string = ''; const AValue: string = '');
    function ToString: string;override;
    procedure Assign(Source: TLyNameValue);
    function IsTrueValue: boolean;
    property Value: string read FValue write FValue;
  end;

  { TLyAttr }

  TLyAttr = class(TLyNameValue)
  private
    FNext: TLyAttr;
  public
    function ToString: string;override;
    property Next: TLyAttr read FNext write FNext;
  end;

  { TLyTag }

  TLyTag = class(TLyNameValue)
  private
    FAttr: TLyAttr;
  public
    destructor Destroy;override;
    function ToString: string;override;
    procedure Assign(Source: TLyTag);
    procedure Clear;
    procedure ClearAttr;
    procedure Remove(const AName: string);
    function Find(const AName: string): TLyAttr;
    function FindByName(const AName: string): TLyAttr;
    function Has(const AName: string): boolean;
    function Get(const AName: string): string;overload;
    function Get(const AName: array of string): string;overload;
    function GetByName(const AName: string): string;
    function GetBoolean(const AName: string): boolean;
    function GetInteger(const AName: string): integer;
    function GetInt64(const AName: string): int64;
    function GetFloat(const AName: string): double;
    function GetCurrency(const AName: string): currency;
    function GetDate(const AName: string): TDate;
    function GetDateTime(const AName: string): TDateTime;
    function Put(const AName, AValue: string): TLyAttr;overload;
    function Put(const AName: string): TLyAttr;overload;
    function Put(const AName: string; AValue: integer): TLyAttr;overload;
    function PutBoolean(const AName: string; AValue: boolean): TLyAttr;
    function PutInteger(const AName: string; AValue: integer): TLyAttr;
    function PutInt64(const AName: string; AValue: int64): TLyAttr;
    function PutFloat(const AName: string; AValue: double): TLyAttr;
    function PutCurrency(const AName: string; AValue: Currency): TLyAttr;
    function PutDate(const AName: string; AValue: TDate): TLyAttr;
    function PutDateTime(const AName: string; AValue: TDateTime): TLyAttr;
    function Parse(const Text: string; var Index: integer; const Head: char = #0): boolean;
    property Attr: TLyAttr read FAttr;
  end;

  { TLyAgent }

  TLyAgent = class(TLyObject)
  private
    FHost: TObject;
    function GetActive: boolean;
  protected
    procedure SetHost(AHost: TObject);virtual;
  public
    property Host: TObject read FHost write SetHost;
    property Active: boolean read GetActive;
  end;

  { TLyStrings }

  TLyStrFunc = function(const S: string): string;

  TLyStrings = class(TLyAgent)
  private
    FStrings: TStrings;
    FCaseSensitive: Boolean;
    FSorted: Boolean;
    FIsStringList: boolean;
    FFreeStrings: boolean;
    function GetStringList: TStringList;
    function GetCount: integer;
    procedure SetCount(Value: integer);
    function GetItem(Index: integer): string;
    procedure SetItem(Index: integer; const Value: string);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetCommaText: string;
    procedure SetCommaText(const Value: string);
    function GetCaseSensitive: boolean;
    procedure SetCaseSensitive(Value: boolean);
    function GetSorted: boolean;
    procedure SetSorted(Value: boolean);
    function GetEmpty: boolean;
    function GetName(Index: integer): string;
    procedure SetName(Index: integer; const Value: string);
    function GetValue(const Name: string): string;
    procedure SetValue(const Name, Value: string);
    function GetVFX(Index: Integer): string;
    procedure SetVFX(Index: Integer; const Value: string);
  protected
    procedure SetHost(AHost: TObject);override;
    procedure SetStrings(Value: TStrings);virtual;
    function Compare(const S1, S2: string): integer;
    function Same(const S1, S2: string): boolean;
    procedure CheckSort;
  public
    constructor CreateFor(AStrings: TStrings);virtual;
    constructor Create;virtual;
    destructor Destroy;override;
    function ToString: string;override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure TryLoadFromFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure Assign(Source: TStrings);
    procedure Clear;
    procedure Delete(Index: integer);
    procedure DeleteLast;
    procedure DeleteEmptyLines;
    procedure DeleteMatchedLines(const Patten: string);
    procedure Remove(Index: integer);overload;
    procedure Remove(const S: string);overload;
    procedure RemoveLast;
    procedure RemoveEmptyLines;
    procedure RemoveMatchedLines(const Patten: string);
    procedure RemoveByName(const S: string);
    procedure RemoveByValue(const S: string);
    function Insert(Index: Integer; const S: string): integer;
    function Add(const S: string): integer;
    function IndexOf(const S: string; Index: integer): integer;overload;
    function IndexOf(const S: string): integer;overload;
    function IndexOfName(const S: string; Index: integer): integer;overload;
    function IndexOfName(const S: string): integer;overload;
    function IndexOfValue(const S: string; Index: integer): integer;overload;
    function IndexOfValue(const S: string): integer;overload;
    function Copy(Index, ItemCount: integer): TStrings;
    function CopyLeft(ItemCount: integer): TStrings;
    function CopyRight(ItemCount: integer): TStrings;
    function SelectMatched(const Patten: string): TStrings;
    function Process(StrFunc: TLyStrFunc): integer;
    procedure Pack;
    procedure Unique;
    procedure Reverse;
    procedure Sort;
    property Strings: TStrings read FStrings write SetStrings;
    property StringList: TStringList read GetStringList;
    property FreeStrings: boolean read FFreeStrings write FFreeStrings;
    property IsStringList: boolean read FIsStringList;
    property IsEmpty: boolean read GetEmpty;
    property Count: integer read GetCount write SetCount;
    property Items[Index: integer]: string read GetItem write SetItem;default;
    property Values[const Name: string]: string read GetValue write SetValue;
    property Names[Index: integer]: string read GetName write SetName;
    property ValueFromIndex[Index: Integer]: string read GetVFX write SetVFX;
    property Text: string read GetText write SetText;
    property CommaText: string read GetCommaText write SetCommaText;
    property Sorted: boolean read GetSorted write SetSorted;
    property CaseSensitive: boolean read GetCaseSensitive write SetCaseSensitive;
  end;

  { TLyStrings }

  TLyStringList = class(TLyStrings)
  public
    constructor CreateFor(AStrings: TStrings);override;
  end;

  { TLySpinLock }

  TLySpinLock = class(TLyObject)
  private
    FCriticalSection: TCriticalSection;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Enter;
    procedure Leave;
    function TryEnter: boolean;
  end;

  { TLyCharactor }

  TLyCharactor = class(TLyObject)
  private
    FText: string;
    FSize: integer;
    FPosition: integer;
    FChar: char;
    procedure SetText(const Value: string);
    function GetEnded: boolean;
    function GetNotEnded: boolean;
  public
    procedure Clear;virtual;
    procedure Stop;virtual;
    function First: boolean;virtual;
    function Next: boolean;overload;virtual;
    function Next(Count: integer): boolean;overload;virtual;
    function GetChar: char;virtual;
    { peek }
    function PeekNextChar: char;
    function PeekNextString(Len: integer): string;
    function PeekString(Len: integer): string;
    { match }
    function MatchChar(Chars: TSysCharSet): boolean;overload;
    function MatchChar(Ch: char): boolean;overload;
    function MatchString(const S: string): boolean;
    function MatchText(const S: string): boolean;
    function MatchSpace: boolean;
    function MatchHead: boolean;
    function MatchDigit: boolean;
    { seek }
    function Seek(Chars: TSysCharSet): boolean;overload;
    function Seek(Ch: char): boolean;overload;
    function SeekSpace: boolean;
    function SeekLineBreak: boolean;
    { skip }
    function Skip(Chars: TSysCharSet): boolean;overload;
    function Skip(Ch: char): boolean;overload;
    function SkipSpace: boolean;
    function SkipLineBreak: boolean;
    function SkipPString(OnQuote: boolean): boolean;
    function SkipCString(OnQuote: boolean): boolean;
    function SkipString: boolean;overload;
    function SkipString(Endc: char): boolean;overload;
    function SkipTag(Endc: char): boolean;
    { pick }
    function Pick(Chars: TSysCharSet): string;
    function PickID: string;
    function PickExID: string;
    function PickHex: string;
    function PickOctal: string;
    function PickDecimal: string;
    { parse }
    function ParsePString(var S: string): boolean;
    function ParseCString(var S: string): boolean;
    function ParseString(var S: string): boolean;
    function ParsePChar(var C: char): boolean;
    function ParseCChar(var C: char): boolean;
    function ParseChar(var C: char): boolean;
    function ParseEscChar(var C: char): boolean;
    function ParseHex(var I: cardinal; N: integer = MaxInt): integer;
    function ParseOctal(var I: cardinal; N: integer = MaxInt): integer;
    function ParseDecimal(var I: cardinal; N: integer = MaxInt): integer;
    { property }
    property Text: string read FText write SetText;
    property Size: integer read FSize;
    property Position: integer read FPosition;
    property Curr: char read FChar;
    property Ended: boolean read GetEnded;
    property NotEnded: boolean read GetNotEnded;
  end;

  { RLyTextPos }

  RLyTextPos = packed record
    ItemX: integer;
    TextX: integer;
  end;
  PLyTextPos = ^RLyTextPos;

  RLyEditInfo = packed record
    ei_process: int64;
    ei_window : int64;
    ei_file   : int64;
    ei_program: array[0..511- sizeof(THandle) * 3] of AnsiChar;
  end;
  PLyEditInfo = ^RLyEditInfo;

  { TLyFileEditLock }

  TLyFileEditLock = class
  private
    FFileName: string;
    FLockFileName: string;
    FHandle: THandle;
    function GetLocked: boolean;
  public
    constructor Create;
    destructor Destroy;override;
    class function ReadFor(const AFileName: string; var Buffer; Count: integer): integer;
    class function ReadEditInfo(const AFileName: string; var EI: RLyEditInfo): boolean;
    class function LockApplication: TLyFileEditLock;
    function LockForFile(const AFileName: string): boolean;
    function Unlock: boolean;
    function Write(const Buffer; Count: integer): integer;
    function WriteEditInfo(const EI: RLyEditInfo): boolean;
    property FileName: string read FFileName;
    property Locked: boolean read GetLocked;
  end;

function Addref(A: TLyObject): integer;
function Release(A: TLyObject): integer;
function ReleaseAndNil(var A): integer;

{ agent object }

function GetAgentHost(const Agent: TLyAgent): pointer;

{ exception handling }

function ExceptionStr: string;
procedure Throw(const Msg: string);overload;
procedure Throw(const Msg: string; const Args: array of const);overload;
procedure Check(OK: boolean; const Msg: string);overload;
procedure Check(OK: boolean; const Msg: string; const Args: array of const);overload;

{ environment }

function GetEnvCount: integer;
function GetEnv(Index: integer): string;overload;
function GetEnv(const ID: string): string;overload;
function GetEnv(const ID: array of string; const DefValue: string): string;overload;

{ memory }

function MemAlloc(Count: integer): pointer;
function MemAllocZero(Count: integer): pointer;
function MemFree(const Mem: pointer; Count: integer = 0): boolean;
function MemZero(const Mem: pointer; Count: integer): pointer;

{ charset }

function InChars(const S: string; Chars: TSysCharSet): boolean;
function SkipChar(const S: pchar; Chars: TSysCharSet): pchar;
function SeekChar(const S: pchar; Chars: TSysCharSet): pchar;
function UpperChar(ch: char): char;
function LowerChar(ch: char): char;
function UpperHead(const S: string): string;
function LowerHead(const S: string): string;
function IsUpperHead(const S: string): boolean;
function IsLowerHead(const S: string): boolean;
function MatchChar(C1, C2: char): boolean;

{ string }

function IsEmptyStr(const S: string): boolean;
function InStrings(const S: string; const List: array of string): boolean;
function RepeatString(const S: string; Times: integer): string;
function RepeatChar(C: char; Times: integer): string;
function StringOf(const S: pchar): string;overload;
function StringOf(const S: pchar; L: integer): string;overload;
function LengthOf(const S: pchar): integer;
function HashOf(S: pchar): cardinal;overload;
function HashOf(const S: string): cardinal;overload;
function TrimAll(const S: string): string;
function ReplaceAll(const S, Patten, NewString: string): string;
function PosLineBreak(const S: string): integer;
function HasLineBreak(const S: string): boolean;
function PosEx(const SubStr, Str: AnsiString; Index: integer): integer;overload;
function PosEx(const SubStr, Str: WideString; Index: integer): integer;overload;
function AddTrailingChar(const S: string; C: char): string;
function StartStr(const S, Patten: string): boolean;
function StartText(const S, Patten: string): boolean;
function SelectStr(const S1, S2: string): string;

{ string encoding }

function ParseCStr(const S: string; var X: integer; var CStr: string): TLyIsString;
function ParsePStr(const S: string; var X: integer; var PStr: string): TLyIsString;
function TryParseStr(const S: string; var X: integer; var Str: string): TLyIsString;
function EncodeCStr(const S: string): string;
function EncodePStr(const S: string): string;
function DoubleQuote(const S: string): string;
function SingleQuote(const S: string): string;

{ parse string }

function ExtractNameValue(const S: string; var V: string; const separator: string = '='): string;
function ExtractName(const S: string; const separator: string = '='): string;
function ExtractValue(const S: string; const separator: string = '='): string;
function ExtractNext(var S: string; const separator: string = ':'): string;
function ExtractNextInteger(var S: string): string;overload;
function ExtractNextInteger(const S: string; var I: integer): string;overload;
function ParseConfig(const S: string; var ID, Value: string): boolean;
function HexValue(ch: char): byte;overload;
function HexValue(c1, c2: char): byte;overload;

{ formating }

function IntToStrExt(Value: int64; const Ext: string): string;
function FloatToStrExt(Value: double; const Ext: string): string;
function CurrToStrExt(Value: currency; const Ext: string): string;

{ String Encoding }

function StrToSys(const S: string): string;
function StrToWide(const S: string): WideString;
function StrToUnicode(const S: string): UnicodeString;
function StrToAnsi(const S: string): AnsiString;
function StrToUTF8(const S: string): AnsiString;

function AnsiToStr(const S: AnsiString): string;
function AnsiToSys(const S: AnsiString): string;
function AnsiToWide(const S: AnsiString): WideString;
function AnsiToUnicode(const S: AnsiString): UnicodeString;
function AnsiToUTF8(const S: AnsiString): AnsiString;

function UTF8ToStr(const S: AnsiString): string;
function UTF8ToSys(const S: AnsiString): string;
function UTF8ToWide(const S: AnsiString): WideString;
function UTF8ToUnicode(const S: AnsiString): UnicodeString;
function UTF8ToAnsi(const S: AnsiString): AnsiString;

function WideToStr(const S: WideString): string;
function WideToSys(const S: WideString): string;
function WideToUnicode(const S: WideString): UnicodeString;
function WideToAnsi(const S: WideString): AnsiString;
function WideToUTF8(const S: WideString): AnsiString;

function UnicodeToStr(const S: UnicodeString): string;
function UnicodeToSys(const S: UnicodeString): string;
function UnicodeToWide(const S: UnicodeString): WideString;
function UnicodeToAnsi(const S: UnicodeString): AnsiString;
function UnicodeToUTF8(const S: UnicodeString): AnsiString;

function IsUTF8(const S: AnsiString): boolean;overload;
function IsUTF8(S: PAnsiChar; Count: integer): boolean;overload;
function IsUTF8Buffer(S: PByte; Count: integer): boolean;

{ File Encoding }

function GetBufferEncoding(const P: PByte; Len: int64): TLyFileEncoding;
function GetFileEncoding(const FileName: string): TLyFileEncoding;
function GetFileUTFS(const FileName: string; var Text: string): TLyFileEncoding;overload;
function GetFileUTFS(const FileName: string): string;overload;
function GetFileUTFSFromBuffer(const P: PByte; Len: int64): string;
function GetFileUTFSFromANSIBuffer(const P: PByte; Len: int64): string;
function GetFileUTFSFromUTF8Buffer(const P: PByte; Len: int64): string;
function GetFileUTFSFromUTF16BEBuffer(const P: PByte; Len: int64): string;
function GetFileUTFSFromUTF16LEBuffer(const P: PByte; Len: int64): string;
function GetFileWideText(const FileName: string): WideString;
function GetFileUTF8Text(const FileName: string): AnsiString;

{ identity }

function GenID: string;
function GenName: string;overload;
function GenName(P: pointer): string;overload;
function GiveName(AComponent: TComponent): string;
function IsID(const S: string): boolean;

{ MatchID }

function MatchIDA(const ID1, ID2: AnsiString): boolean;
function MatchIDW(const ID1, ID2: WideString): boolean;
function MatchID(const ID1, ID2: string): boolean;overload;
function MatchID(const ID: string; const IDList: array of string): boolean;overload;

{ compare }

function IntToCompare(I: integer): TLyCompare;overload;
function IntToCompare(I: int64): TLyCompare;overload;
function CompareFloat(V1, V2: double): TLyCompare;
function CompareInt64(V1, V2: int64): TLyCompare;
function CompareInteger(V1, V2: integer): TLyCompare;
function CompareMoney(V1, V2: currency): TLyCompare;
function CompareChar(V1, V2: char): TLyCompare;overload;
function CompareChar(V1, V2: char; CaseSensitive: boolean): TLyCompare;overload;
function CompareString(const S1, S2: string): TLyCompare;overload;
function CompareString(const S1, S2: string; CaseSensitive: boolean): TLyCompare;overload;
function CompareTextPos(ItemX1, TextX1, ItemX2, TextX2: integer): integer;overload;
function CompareTextPos(P1, P2: PLyTextPos): integer;overload;
function CompareTextPos(P1: PLyTextPos; ItemX2, TextX2: integer): integer;overload;


{ patten }

function MatchPatten(const S, Patten: string): boolean;

{ pointer }

function IntToPtr(Value: integer): pointer;
function PtrToInt(Value: pointer): integer;
function IncPtr(const P: pointer; Offset: integer): pointer;
function DecPtr(const P: pointer; Offset: integer): pointer;overload;
function DecPtr(const P1, P2: pointer): integer;overload;

{ file }

function FullPath(const Path: string): string;
function FullFileName(const FileName: string): string;
function RelativeFileName(const FileName, BaseFileName: string): string;
function MakeDir(const Dir: string): boolean;
function SetPD(const Path: string): string;
function IncPD(const FileName: string): string;
function ExcPD(const FileName: string): string;
function SetUD(const URL: string): string;
function OpenFileMode(const S: string): integer;
function OpenFileStream(const FileName: string; Mode: Word): TFileStream;
function GetRelativePath(const FileName, BaseFile: string): string;
function IsAbsoluteFileName(const FileName: string): boolean;

{ index }

function ResetIndex(Index, Length: integer; Check: boolean = false): integer;overload;
function ResetRange(Index, Length: integer; var Count: integer): integer;overload;
function ResetIndex(Index, Length: int64; Check: boolean = false): int64;overload;
function ResetRange(Index, Length: int64; var Count: int64): int64;overload;
function CheckIndex(Index, Length: int64): boolean;overload;
function CheckIndex(Index, MinX, MaxX: int64): boolean;overload;

{ ym }

function GetYm: integer;
function GetYmFrom(Date: TDateTime): integer;
function IsYm(Ym: integer): boolean;
function IsYmOf(Y, M: integer): boolean;
function IsYmIn(Ym, MinYM, MaxYM: integer): boolean;
function IsYmStr(const Ym: string): boolean;
function YmToStr(Ym: integer): string;
function YmToStrOf(Ym: integer; const Delimiter: string): string;
function StrToYm(const S: string; DefValue: integer = 0): integer;
function DecodeYm(Ym: integer; var Y, M: integer): boolean;
function PrevYm(Ym: integer; Offset: integer = 1): integer;
function PrevYmStr(const Ym: string): string;
function NextYm(Ym: integer; Offset: integer = 1): integer;
function NextYmStr(const Ym: string): string;

{ ymd }

function GetYmd: integer;
function GetYmdFrom(Date: TDateTime): integer;
function IsYmd(Ymd: integer): boolean;
function IsYmdOf(Y, M, D: integer): boolean;
function IsYmdStr(const Ymd: string): boolean;
function YmdToStr(Ymd: integer): string;
function YmdToStrOf(Ymd: integer; const Delimiter: string): string;
function StrToYmd(const S: string; DefValue: integer = 0): integer;
function YmdToDate(Ymd: integer): TDateTime;
function DecodeYmd(Ymd: integer; var Y, M, D: integer): boolean;
function NextYmd(Ymd: integer; Offset: integer = 1): integer;
function NextYmdStr(const Ymd: string): string;
function PrevYmd(Ymd: integer; Offset: integer = 1): integer;
function PrevYmdStr(const Ymd: string): string;

{ md5 }

function MD5SumBuffer(const Buffer: pointer; Count: integer): string;
function MD5SumString(const S: string): string;
function MD5SumFile(const FileName: string): string;
function MD5TrySumFile(const FileName: string): string;

function SHA1SumBuffer(const Buffer: pointer; Count: integer): string;
function SHA1SumString(const S: string): string;
function SHA1SumFile(const FileName: string): string;
function SHA1TrySumFile(const FileName: string): string;

{ object }

procedure FreeAll(const Objects: TList);overload;
procedure FreeAll(const Objects: array of TObject);overload;

{ library }

function LoadDLL(const FileName: string; var Handle: THandle): boolean;
function FreeDLL(Handle: THandle): boolean;
function GetProcAddr(Handle: THandle; const ProcName: string): pointer;

{ stdio }

function stdin: integer;
function stdout: integer;
function stderr: integer;

{ misc }

procedure Swap(var V1, V2: integer);

implementation

uses
  {$IFDEF MSWINDOWS}Windows,{$ELSE}dynlibs,{$ENDIF}
  {$IFDEF FPC}regexpr, MD5, SHA1,{$ELSE}Hash, RegularExpressions, HTTPApp,{$ENDIF}
  DateUtils, Math;

const
  FileEditLockExt = '.~lock';

function Addref(A: TLyObject): integer;
begin
  if A <> nil then
    Result := A.IncRefcount else
    Result := 0;
end;

function Release(A: TLyObject): integer;
begin
  if A <> nil then
    Result := A.DecRefcount else
    Result := 0;
end;

function ReleaseAndNil(var A): integer;
var
  T: TLyObject;
begin
  T := TLyObject(A);
  if T <> nil then
  begin
    TLyObject(A) := nil;
    Result := Release(T);
  end
  else Result := 0;
end;

function GetAgentHost(const Agent: TLyAgent): pointer;
begin
  if Agent <> nil then
    Result := Agent.Host else
    Result := nil;
end;

function ExceptionStr: string;
var
  E: TObject;
begin
  E := ExceptObject;
  if E = nil then Result := '' else
  if not (E is Exception) or (E is EAbort) then
    Result := Format('%s<%p> was raised', [E.ClassName, pointer(E)]) else
    Result := Exception(E).Message;
end;

procedure Throw(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

procedure Throw(const Msg: string; const Args: array of const);
begin
  Throw(Format(Msg, Args));
end;

procedure Check(OK: boolean; const Msg: string);
begin
  if not OK then Throw(Msg);
end;

procedure Check(OK: boolean; const Msg: string; const Args: array of const);
begin
  if not OK then Throw(Msg, Args);
end;

function GetEnvCount: integer;
{$IFNDEF FPC}
var
  H, P: PChar;
{$ENDIF}
begin
  {$IFDEF FPC}
  Result := SysUtils.GetEnvironmentVariableCount;
  {$ELSE}
  Result := 0;
  P := GetEnvironmentStrings;
  H := P;
  if H <> nil then
    while H^ <> #0 do
    begin
      Inc(Result);
      H := H + StrLen(H) + 1;
    end;
  FreeEnvironmentStrings(P);
  {$ENDIF}
end;

function GetEnv(Index: integer): string;
{$IFNDEF FPC}
var
  H, P: PChar;
{$ENDIF}
begin
  {$IFDEF FPC}
  Result := SysUtils.GetEnvironmentString(Index + 1);
  {$ELSE}
  Result := '';
  P := GetEnvironmentStrings;
  H := P;
  if H <> nil then
  begin
    while (H^ <> #0) and (Index > 0) do
    begin
      H := H + StrLen(H) + 1;
      Dec(Index);
    end;
    if (H^ <> #0) and (Index = 0) then
      Result := H;
  end;
  FreeEnvironmentStrings(P);
  {$ENDIF}
end;

function GetEnv(const ID: string): string;
begin
  Result := SysUtils.GetEnvironmentVariable(ID);
end;

function GetEnv(const ID: array of string; const DefValue: string): string;
var
  I: integer;
begin
  for I := 0 to length(ID) - 1 do
  begin
    Result := SysUtils.GetEnvironmentVariable(ID[I]);
    if Result <> '' then Exit;
  end;
  Result := DefValue;
end;

function GenID: string;
var
  G: TGuid;
  I: integer;
begin
  CreateGuid(G);
  Result := UpperCase(GuidToString(G));
  for I := Length(Result) downto 1 do
    if not CharInSet(Result[I], CS_HEX) then
      System.Delete(Result, I, 1);
end;

function GenName: string;
begin
  Result := '_' + GenID;
end;

function GenName(P: pointer): string;
begin
  Result := Format('_%p', [P]);
end;

function GiveName(AComponent: TComponent): string;
begin
  if AComponent <> nil then
  begin
    Result := Format('%s_%p', [AComponent.ClassName, pointer(AComponent)]);
    AComponent.Name := Result;
  end
  else Result := '';
end;

function HexValue(ch: char): byte;
begin
  case ch of
    '0'..'9': Result := Ord(ch) - Ord('0');
    'A'..'F': Result := Ord(ch) - Ord('A') + 10;
    'a'..'f': Result := Ord(ch) - Ord('a') + 10;
    else begin
      Result := 0;
      Throw('invalid HEX char: %c', [ch]);
    end;
  end;
end;

function HexValue(c1, c2: char): byte;
begin
  Result := (HexValue(c1) shl 4) or HexValue(c2);
end;

function StringOf(const S: pchar): string;
begin
  Result := StringOf(S, LengthOf(S));
end;

function StringOf(const S: pchar; L: integer): string;
begin
  if (S <> nil) and (L > 0) then
    SetString(Result, S, L) else
    Result := '';
end;

function LengthOf(const S: pchar): integer;
var
  P: pchar;
begin
  if (S <> nil) and (S^ <> #0) then
  begin
    P := S + 1;
    while P^ <> #0 do Inc(P);
    Result := (P - S);
  end
  else Result := 0;
end;

function MemAlloc(Count: integer): pointer;
begin
  Result := nil;
  if Count > 0 then
    GetMem(Result, Count);
end;

function MemAllocZero(Count: integer): pointer;
begin
  Result := MemAlloc(Count);
  if Result <> nil then
    FillChar(Result^, Count, 0);
end;

function MemFree(const Mem: pointer; Count: integer): boolean;
begin
  Result := (Mem <> nil) and (Count >= 0);
  if Result then
    if Count > 0 then
      FreeMem(Mem, Count) else
      FreeMem(Mem);
end;

function MemZero(const Mem: pointer; Count: integer): pointer;
begin
  FillChar(Mem^, Count, 0);
  Result := Mem;
end;

function FullFileName(const FileName: string): string;
begin
  if FileName <> '' then
    Result := ExpandFileName(SetPD(FileName)) else
    Result := '';
end;

function FullPath(const Path: string): string;
begin
  Result := ExpandFileName(SetPD(Path));
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

function RelativeFileName(const FileName, BaseFileName: string): string;
var
  F: string;
begin
  F := SetPD(FileName);
  if (F <> '') and (F[1] = '.') then
    F := ExtractFilePath(SetPD(BaseFileName)) + F;
  Result := ExpandFileName(F);
end;

function MakeDir(const Dir: string): boolean;
begin
  Result := ForceDirectories(Dir);
end;

function vary_index(index, length: int64): int64;
begin
  if index < 0 then
    Result := index + length else
    Result := index;
end;

function vary_range(index, length: int64; var count: int64): int64;
begin
  if index < 0 then
  begin
    Result := index + length;
    if Result < 0 then
    begin
      Inc(count, Result);
      Result := 0;
    end;
  end
  else Result := index;
  count := Max(0, Min(length - Result, count));
end;

function SetPD(const Path: string): string;
begin
  {$IFDEF MSWINDOWS}
  Result := ReplaceAll(Path, '/', '\');
  {$ELSE}
  Result := ReplaceAll(Path, '\', '/');
  {$ENDIF}
end;

function SetUD(const URL: string): string;
begin
  Result := ReplaceAll(URL, '\', '/');
end;

function IncPD(const FileName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FileName);
end;

function ExcPD(const FileName: string): string;
begin
  Result := ExcludeTrailingPathDelimiter(FileName);
end;

function OpenFileMode(const S: string): integer;
var
  I: integer;
  C, E, R, W: boolean;
begin
  Result := fmShareDenyWrite;
  C := false;
  E := false;
  R := false;
  W := false;
  for I := 1 to Length(S) do
    case S[I] of
      'c', 'C': C := true; // create
      'e', 'E': E := true; // exclusive
      'r', 'R': R := true; // read
      'w', 'W': W := true; // write
    end;
  if C or E or R or W then
  begin
    if C then
    begin
      Result := fmCreate;
//    R := true;
//    W := true;
    end
    else
    if R then
    begin
      if W then Result := fmOpenReadWrite or fmShareExclusive else
      if E then Result := fmOpenRead or fmShareExclusive else
                Result := fmShareDenyWrite;
    end
    else
    if W then
      Result := fmOpenWrite or fmShareExclusive else
      Result := fmOpenRead or fmShareExclusive;
  end
end;

function OpenFileStream(const FileName: string; Mode: Word): TFileStream;
begin
  Result := TFileStream.Create(FileName, Mode);
end;

function GetRelativePath(const FileName, BaseFile: string): string;
begin
  Result := ExtractRelativePath(BaseFile, FileName);
end;

function IsAbsoluteFileName(const FileName: string): boolean;
begin
  Result := (FileName <> '') and
    (CharInSet(FileName[1], ['/', '\']) or (ExtractFileDrive(FileName) <> ''));
end;

function ResetIndex(Index, Length: int64; Check: boolean): int64;
begin
  if Index < 0 then
    Result := Index + Length else
    Result := Index;
  if Check then
    CheckIndex(Result, Length);
end;

function ResetRange(Index, Length: int64; var Count: int64): int64;
begin
  if Index < 0 then
  begin
    Result := Index + Length;
    if Result < 0 then
    begin
      Inc(Count, Result);
      Result := 0;
    end;
  end
  else Result := Index;
  Count := Max(0, Min(Length - Result, Count));
end;

function ResetIndex(Index, Length: integer; Check: boolean): integer;
begin
  if Index < 0 then
    Result := Index + Length else
    Result := Index;
  if Check then
    CheckIndex(Result, Length);
end;

function ResetRange(Index, Length: integer; var Count: integer): integer;
begin
  if Index < 0 then
  begin
    Result := Index + Length;
    if Result < 0 then
    begin
      Inc(Count, Result);
      Result := 0;
    end;
  end
  else Result := Index;
  Count := Max(0, Min(Length - Result, Count));
end;

function CheckIndex(Index, Length: int64): boolean;
begin
  Result := (Index >= 0) and (Index < Length);
  if not Result then
    Throw('index %d is out of range %d', [Index, Length]);
end;

function CheckIndex(Index, MinX, MaxX: int64): boolean;
begin
  Result := (Index >= MinX) and (Index <= MaxX);
  if not Result then
    Throw('index %d is out of range %d..%d', [Index, MinX, MaxX]);
end;

function IntToPtr(Value: integer): pointer;
var
  P: PByte;
begin
  P := nil;
  Inc(P, Value);
  Result := P;
end;

function PtrToInt(Value: pointer): integer;
begin
  Result := PByte(Value) - PByte(nil);
end;

function IncPtr(const P: pointer; Offset: integer): pointer;
begin
  Result := PByte(P) + Offset;
end;

function DecPtr(const P: pointer; Offset: integer): pointer;
begin
  Result := PByte(P) - Offset;
end;

function DecPtr(const P1, P2: pointer): integer;
begin
  Result := PByte(P1) - PByte(P2);
end;

function InChars(const S: string; Chars: TSysCharSet): boolean;
var
  I: integer;
begin
  for I := 1 to Length(S) do
    if not CharInSet(S[I], Chars) then
    begin
      Result := false;
      Exit;
    end;
  Result := (S <> '');
end;

function SkipChar(const S: pchar; Chars: TSysCharSet): pchar;
begin
  Result := S;
  if Result <> nil then
  begin
    while (Result^ <> #0) and CharInSet(Result^, Chars) do Inc(Result);
    if Result^ = #0 then Result := nil;
  end;
end;

function SeekChar(const S: pchar; Chars: TSysCharSet): pchar;
begin
  Result := S;
  if Result <> nil then
  begin
    while (Result^ <> #0) and not CharInSet(Result^, Chars) do Inc(Result);
    if Result^ = #0 then Result := nil;
  end;
end;

function IsID(const S: string): boolean;
begin
  Result := (S <> '') and CharInSet(S[1], CS_HEAD) and InChars(S, CS_ID);
end;

function MatchIDA(const ID1, ID2: AnsiString): boolean;
var
  X: integer;
  P1, P2: PByte;
begin
  P1 := PByte(ID1);
  P2 := PByte(ID2);
  Result := (P1 = P2);
  if not Result and (P1 <> nil) and (P2 <> nil) then
  begin
    repeat
      X := P1^ - P2^;
      if X <> 0 then
        if X = CS_DISTANCE then
        begin
          if (P1^ < CS_LOWER_A) or (P1^ > CS_LOWER_Z) then Exit;
        end
        else
        if X = - CS_DISTANCE then
        begin
          if (P1^ < CS_UPPER_A) or (P1^ > CS_UPPER_Z) then Exit;
        end
        else Exit;
      Inc(P1);
      Inc(P2);
    until (P1^ = 0) and (P2^ = 0);
    Result := true;
  end;
end;

function MatchIDW(const ID1, ID2: WideString): boolean;
var
  X: integer;
  P1, P2: PWord;
begin
  P1 := PWord(ID1);
  P2 := PWord(ID2);
  Result := (P1 = P2);
  if not Result and (P1 <> nil) and (P2 <> nil) then
  begin
    repeat
      X := P1^ - P2^;
      if X <> 0 then
        if X = CS_DISTANCE then
        begin
          if (P1^ < CS_LOWER_A) or (P1^ > CS_LOWER_Z) then Exit;
        end
        else
        if X = - CS_DISTANCE then
        begin
          if (P1^ < CS_UPPER_A) or (P1^ > CS_UPPER_Z) then Exit;
        end
        else Exit;
      Inc(P1);
      Inc(P2);
    until (P1^ = 0) and (P2^ = 0);
    Result := true;
  end;
end;

function MatchID(const ID1, ID2: string): boolean;overload;
begin
  {$IFDEF UNICODE}
  Result := MatchIDW(ID1, ID2);
  {$ELSE}
  Result := MatchIDA(ID1, ID2);
  {$ENDIF}
end;

function MatchID(const ID: string; const IDList: array of string): boolean;overload;
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

function IsUpperHead(const S: string): boolean;
begin
  Result := (S <> '') and CharInSet(S[1], CS_UPPER);
end;

function IsLowerHead(const S: string): boolean;
begin
  Result := (S <> '') and CharInSet(S[1], CS_LOWER);
end;

function MatchChar(C1, C2: char): boolean;
begin
  Result := (C1 = C2);
  if not Result then
    if (C1 >= 'A') and (C1 <= 'Z') then
    begin
      if (C2 >= 'a') and (C2 <= 'z') then
        Result := (Ord(C1) + CS_DISTANCE) = Ord(C2);
    end
    else
    if (C1 >= 'a') and (C1 <= 'z') then
      if (C2 >= 'A') and (C2 <= 'Z') then
        Result := Ord(C1) = (Ord(C2) + CS_DISTANCE);
end;

function IsUTF8(const S: AnsiString): boolean;
begin
  Result := IsUTF8Buffer(PByte(PAnsiChar(S)), Length(S));
end;

function IsUTF8(S: PAnsiChar; Count: integer): boolean;
begin
  Result := IsUTF8Buffer(PByte(S), Count);
end;

function IsUTF8Buffer(S: PByte; Count: integer): boolean;
var
  I, REST: integer;
  A: byte;
  ANSI: boolean;
begin
  Result := false;
  ANSI := true;
  REST := 0;
  for I := 0 to Count - 1 do
  begin
    A := Ord(S^);
    if REST > 0 then // check following rest: 10XXXXXX
    begin
      if (A and $C0) <> $80 then Exit;
      Dec(REST);
    end
    else
    if A >= $80 then // head byte: 1XXXXXXX
    begin
      if (A >= $FC) and (A <= $FD) then REST := 5 else
      if (A >= $F8) then REST := 4 else
      if (A >= $F0) then REST := 3 else
      if (A >= $E0) then REST := 2 else
      if (A >= $C0) then REST := 1 else Exit;
      ANSI := false;
    end;
    Inc(S);
  end;
  Result := not ANSI and (REST = 0);
end;

function StrToAnsi(const S: string): AnsiString;
begin
  {$IFDEF UNICODE}
  Result := WideToAnsi(S);
  {$ELSE}
  Result := {$IFDEF FPC}UTF8ToAnsi(S){$ELSE}S{$ENDIF};
  {$ENDIF};
end;

function StrToUTF8(const S: string): AnsiString;
begin
  {$IFDEF UNICODE}
  Result := UTF8Encode(S);
  {$ELSE}
  Result := {$IFDEF FPC}S{$ELSE}AnsiToUTF8(S){$ENDIF};
  {$ENDIF};
end;

function AnsiToStr(const S: AnsiString): string;
begin
  {$IFDEF UNICODE}
  Result := AnsiToWide(S);
  {$ELSE}
  Result := S;
  {$ENDIF};
end;

function AnsiToSys(const S: AnsiString): string;
begin
  {$IFDEF UNICODE}
  Result := AnsiToWide(S);
  {$ELSE}
  Result := {$IFDEF MSWINDOWS}S{$ELSE}AnsiToUTF8(S){$ENDIF};
  {$ENDIF};
end;

function AnsiToWide(const S: AnsiString): WideString;
{$IFDEF MSWINDOWS}
var
  L: integer;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  L := Length(S)+ 1;
  SetLength(Result, L);
  L := MultiByteToWideChar(CP_ACP, 0, PAnsiChar(S), -1, PWideChar(Result), L);
  SetLength(Result, L - 1);
  {$ELSE}
  Result := WideString(S);
  {$ENDIF}
end;

function StrToSys(const S: string): string;
begin
  {$IFDEF UNICODE}
  Result := S;
  {$ELSE}
  Result := {$IFDEF MSWINDOWS}StrToAnsi{$ELSE}SrtToUTF8{$ENDIF}(S);
  {$ENDIF};
end;

function StrToWide(const S: string): WideString;
begin
  {$IFDEF UNICODE}
  Result := S;
  {$ELSE}
  Result := {$IFDEF FPC}UTF8Decode{$ELSE}AnsiToWide{$ENDIF}(S);
  {$ENDIF};
end;

function StrToUnicode(const S: string): UnicodeString;
begin
  Result := StrToWide(S);
end;

function AnsiToUnicode(const S: AnsiString): UnicodeString;
begin
  Result := AnsiToWide(S);
end;

function AnsiToUTF8(const S: AnsiString): AnsiString;
begin
  Result := UTF8Encode(AnsiToWide(S));
end;

function UTF8ToStr(const S: AnsiString): string;
begin
  {$IFDEF UNICODE}
  Result := UTF8ToString(S);
  {$ELSE}
  Result := S;
  {$ENDIF};
end;

function UTF8ToSys(const S: AnsiString): string;
begin
  {$IFDEF UNICODE}
  Result := UTF8ToString(S);
  {$ELSE}
  Result := {$IFDEF MSWINDOWS}UTF8ToAnsi(S){$ELSE}S{$ENDIF};
  {$ENDIF};
end;

function UTF8ToWide(const S: AnsiString): WideString;
begin
  {$IFDEF UNICODE}
  Result := UTF8ToWideString(S);
  {$ELSE}
  Result := UTF8Decode(S);
  {$ENDIF}
end;

function UTF8ToUnicode(const S: AnsiString): UnicodeString;
begin
  {$IFDEF UNICODE}
  Result := System.UTF8ToWideString(S);
  {$ELSE}
  Result := UTF8Decode(S);
  {$ENDIF}
end;

function UTF8ToAnsi(const S: AnsiString): AnsiString;
begin
  Result := WideToAnsi(UTF8ToWide(S));
end;

function WideToStr(const S: WideString): string;
begin
  {$IFDEF UNICODE}
  Result := S;
  {$ELSE}
  Result := {$IFDEF FPC}UTF8Encode(S){$ELSE}WideToAnsi(S){$ENDIF};
  {$ENDIF};
end;

function WideToSys(const S: WideString): string;
begin
  {$IFDEF UNICODE}
  Result := S;
  {$ELSE}
  Result := {$IFDEF MSWINDOWS}WideToAnsi(S){$ELSE}UTF8Encode(S){$ENDIF};
  {$ENDIF}
end;

function WideToUnicode(const S: WideString): UnicodeString;
begin
  Result := S;
end;

function WideToAnsi(const S: WideString): AnsiString;
{$IFDEF MSWINDOWS}
var
  L: integer;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  L := Length(S)* 2 + 1;
  SetLength(Result, L);
  L := WideCharToMultiByte(CP_ACP, 0, PWideChar(S), -1, PAnsiChar(Result), L, nil, nil);
  SetLength(Result, L - 1);
  {$ELSE}
  Result := AnsiString(S);
  {$ENDIF}
end;

function WideToUTF8(const S: WideString): AnsiString;
begin
  Result := UTF8Encode(S);
end;

function UnicodeToStr(const S: UnicodeString): string;
begin
  Result := WideToStr(S);
end;

function UnicodeToSys(const S: UnicodeString): string;
begin
  Result := WideToSys(S);
end;

function UnicodeToWide(const S: UnicodeString): WideString;
begin
  Result := S;
end;

function UnicodeToAnsi(const S: UnicodeString): AnsiString;
begin
  Result := WideToAnsi(S);
end;

function UnicodeToUTF8(const S: UnicodeString): AnsiString;
begin
  Result := UTF8Encode(S);
end;

function GetBufferEncoding(const P: PByte; Len: int64): TLyFileEncoding;
begin
  Result := feNone;
  if (Len > 1) and (P[0] = $FE) and (P[1] = $FF) then
  begin
    if (Len > 3) and (P[2] = $00) and (P[3] = $00) then
      Result := feUCS4_3412 else
      Result := feUTF16BE;
  end
  else
  if (Len > 1) and (P[0] = $FF) and (P[1] = $FE) then
  begin
    if (Len > 3) and (P[2] = $00) and (P[3] = $00) then
      Result := feUCS4LE else
      Result := feUTF16LE;
  end
  else
  if (Len > 2) and (P[0] = $EF) and (P[1] = $BB) and (P[2] = $BF) then
    Result := feUTF8 else
  if (Len > 3) and (P[0] = $00) and (P[1] = $00) then
  begin
    if (P[2] = $FE) and (P[3] = $FF) then
      Result := feUCS4BE else
    if (P[2] = $FF) and (P[3] = $FE) then
      Result := feUCS4_2143;
  end;
end;

function GetFileEncoding(const FileName: string): TLyFileEncoding;
var
  F: TStream;
  B: array[0..1024] of byte;
  L: integer;
begin
  F := TFileStream.Create(FileName, fmShareDenyWrite);
  try
    L := F.Read(B[0], 1024);
    Result := GetBufferEncoding(@B[0], L);
  finally
    F.Free;
  end;
end;

function GetFileUTFS(const FileName: string; var Text: string): TLyFileEncoding;
var
  F: TMemoryStream;
  B: PByte;
  L: int64;
begin
  Text := '';
  F := TMemoryStream.Create;
  try
    F.LoadFromFile(FileName);
    B := PByte(F.Memory);
    L := F.Size;
    Result := GetBufferEncoding(B, L);
    case Result of
      feNone     : Text := GetFileUTFSFromBuffer(B, L);
      feUTF8     : Text := GetFileUTFSFromUTF8Buffer(B + 3, L - 3);
      feUTF16LE  : Text := GetFileUTFSFromUTF16LEBuffer(B + 2, L - 2);
      feUTF16BE  : Text := GetFileUTFSFromUTF16BEBuffer(B + 2, L - 2);
//    feUCS4BE   :;
//    feUCS4LE   :;
//    feUCS4_2143:;
//    feUCS4_3412:;
    end;
  finally
    F.Free;
  end;
end;

function GetFileUTFS(const FileName: string): string;
begin
  Result := '';
  GetFileUTFS(FileName, Result);
end;

function GetFileUTFSFromBuffer(const P: PByte; Len: int64): string;
var
  I, REST: int64;
  B: byte;
  UTF8, ANSI, U2BE, U2LE: boolean;
begin
  Result := '';
  REST := 0;
  UTF8 := true;
  ANSI := true;
  U2BE := false;  // big-endian
  U2LE := false;  // little-endian
  for I := 0 to Len - 1 do
  begin
    B := P[I];
    if B = 0 then
    begin
      UTF8 := false;
      ANSI := false;
      if I mod 2 = 0 then
        U2BE := true else
        U2LE := true;
      if U2BE and U2LE then Exit;
    end
    else
    if UTF8 then
      if REST > 0 then // check following rest: 10XXXXXX
      begin
        if (B and $C0) <> $80 then
          UTF8 := false;
        Dec(REST);
      end
      else
      if B >= $80 then // head byte: 1XXXXXXX
      begin
        if (B >= $FC) and (B <= $FD) then REST := 5 else
        if (B >= $F8) then REST := 4 else
        if (B >= $F0) then REST := 3 else
        if (B >= $E0) then REST := 2 else
        if (B >= $C0) then REST := 1 else UTF8 := false;
        if REST > 0 then ANSI := false;
      end;
  end;

  if UTF8 then
    Result := GetFileUTFSFromUTF8Buffer(P, Len) else
  if ANSI then
    Result := GetFileUTFSFromANSIBuffer(P, Len) else
  if U2BE xor U2LE then
    if U2BE then
      Result := GetFileUTFSFromUTF16BEBuffer(P, Len) else
      Result := GetFileUTFSFromUTF16LEBuffer(P, Len);
end;

function GetFileUTFSFromANSIBuffer(const P: PByte; Len: int64): string;
var
  S: AnsiString;
begin
  SetLength(S, Len);
  System.Move(P^, pointer(S)^, Len);
  {$IFDEF UNICODE}
  Result := AnsiToWide(S);
  {$ELSE}
  Result := AnsiToUTF8(S);
  {$ENDIF}
end;

function GetFileUTFSFromUTF8Buffer(const P: PByte; Len: int64): string;
var
  S: AnsiString;
begin
  SetLength(S, Len);
  System.Move(P^, pointer(S)^, Len);
  {$IFDEF UNICODE}
  Result := UTF8ToString(S);
  {$ELSE}
  Result := S;
  {$ENDIF}
end;

function GetFileUTFSFromUTF16BEBuffer(const P: PByte; Len: int64): string;
var
  S: WideString;
  I, L: int64;
  B: PByte;
begin
  B := P;
  L := Len div 2;
  SetLength(S, L);
  for I := 1 to L do
  begin
    S[I] := WideChar(MakeWord(B[1], B[0]));
    Inc(B, 2);
  end;
  {$IFDEF UNICODE}
  Result := S;
  {$ELSE}
  Result := UTF8Encode(S);
  {$ENDIF}
end;

function GetFileUTFSFromUTF16LEBuffer(const P: PByte; Len: int64): string;
var
  S: WideString;
  I, L: int64;
  B: PByte;
begin
  B := P;
  L := Len div 2;
  SetLength(S, L);
  for I := 1 to L do
  begin
    S[I] := WideChar(MakeWord(B[0], B[1]));
    Inc(B, 2);
  end;
  {$IFDEF UNICODE}
  Result := S;
  {$ELSE}
  Result := UTF8Encode(S);
  {$ENDIF}
end;

function GetFileWideText(const FileName: string): WideString;
begin
  {$IFDEF UNICODE}
  Result := GetFileUTFS(FileName);
  {$ELSE}
  Result := UTF8Decode(GetFileUTFS(FileName));
  {$ENDIF}
end;

function GetFileUTF8Text(const FileName: string): AnsiString;
begin
  {$IFDEF UNICODE}
  Result := UTF8Encode(GetFileUTFS(FileName));
  {$ELSE}
  Result := GetFileUTFS(FileName);
  {$ENDIF}
end;

function IntToStrExt(Value: int64; const Ext: string): string;
begin
  if Value = 0 then
    Result := '' else
    Result := IntToStr(Value);
  if (Result <> '') and (Ext <> '') then
    Result := Result + Ext;
end;

function FloatToStrExt(Value: double; const Ext: string): string;
begin
  if IsZero(Value) then
    Result := '' else
    Result := FloatToStr(Value);
  if (Result <> '') and (Ext <> '') then
    Result := Result + Ext;
end;

function CurrToStrExt(Value: currency; const Ext: string): string;
begin
  if Value = 0 then
    Result := '' else
    Result := CurrToStr(Value);
  if (Result <> '') and (Ext <> '') then
    Result := Result + Ext;
end;

function PosLineBreak(const S: string): integer;
var
  I: integer;
begin
  for I := 1 to Length(S) do
    if CharInSet(S[I], [#10, #13]) then
    begin
      Result := I;
      Exit;
    end;
  Result := 0;
end;

function HasLineBreak(const S: string): boolean;
var
  I: integer;
begin
  for I := 1 to Length(S) do
    if CharInSet(S[I], [#10, #13])  then
    begin
      Result := true;
      Exit;
    end;
  Result := false;
end;

function PosEx(const SubStr, Str: AnsiString; Index: integer): integer;
var
  I, N, L, J: Integer;
  P, B: PAnsiChar;
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

function AddTrailingChar(const S: string; C: char): string;
begin
  if (S = '') or (S[Length(S)] <> C) then
    Result := S + C else
    Result := S;
end;

function StartStr(const S, Patten: string): boolean;
var
  L, P: integer;
begin
  P := Length(Patten);
  if P > 0 then
  begin
    L := Length(S);
    if L > P then
      Result := (Patten = Copy(S, 1, P)) else
    if L = P then
      Result := (Patten = S) else
      Result := false;
  end
  else Result := false;
end;

function StartText(const S, Patten: string): boolean;
var
  L, P: integer;
begin
  P := Length(Patten);
  if P > 0 then
  begin
    L := Length(S);
    if L > P then
      Result := SameText(Patten, Copy(S, 1, P)) else
    if L = P then
      Result := SameText(Patten, S) else
      Result := false;
  end
  else Result := false;
end;

function SelectStr(const S1, S2: string): string;
begin
  if S1 <> '' then Result := S1 else Result := S2;
end;

function ParseCStr(const S: string; var X: integer; var CStr: string): TLyIsString;
var
  L, V: integer;

  function on_ddd: boolean;
  begin
    Result := CharInSet(S[X], CS_OCTAL) and (X + 2 <= L) and
              CharInSet(S[X + 1], CS_OCTAL) and
              CharInSet(S[X + 2], CS_OCTAL);
  end;

begin
  CStr := '';
  L := Length(S);
  if (X > 0) and (X <= L) and (S[X] = '"') then
    Result := isPart else
    Result := isNo;
  if Result = isPart then
  begin
    Inc(X);
    while (X <= L) and (S[X] <> '"') do
    begin
      if S[X] = '\' then
      begin
        Inc(X);
        if X > L then Exit; // uncompleted C string
        case S[X] of
          'n': CStr := CStr + #10; // LF: LineFeed
          'r': CStr := CStr + #13; // CR: CarriageReturn
          't': CStr := CStr + #9;  // HT: Horz Tab
          'v': CStr := CStr + #11; // VT: Vert Tab
          'a': CStr := CStr + #7;  // BELL
          'b': CStr := CStr + #8;  // BS: BackSpace
          'f': CStr := CStr + #12; // FF: FormFeed
          'x': if (X + 1 <= L) and CharInSet(S[X + 1], CS_HEX) then
               begin
                 Inc(X);
                 V := HexValue(S[X]);
                 if (X + 1 <= L) and CharInSet(S[X + 1], CS_HEX) then
                 begin
                   Inc(X);
                   V := (V * 16) + HexValue(S[X]);
                   CStr := CStr + char(V);
                 end;
               end;
          else
          if on_ddd then
          begin
            V := (Ord(S[X]) - Ord('0')) * 64 +
                 (Ord(S[X + 1]) - Ord('0')) * 8 +
                 (Ord(S[X + 2]) - Ord('0'));
            CStr := CStr + char(V);
            Inc(X, 2);
          end
          else
          if S[X] <> '0' then
            CStr := CStr + S[X] else
            CStr := CStr + #0;
        end;
      end
      else CStr := CStr + S[X];
      Inc(X);
    end;
    if X <= L then
    begin
      Result := isYes;
      Inc(X);
    end;
  end;
end;

function ParsePStr(const S: string; var X: integer; var PStr: string): TLyIsString;
var
  L, V: integer;

  function on_sharp: boolean;
  begin
    Result := (S[X] = '#') and (X < L) and CharInSet(S[X + 1], ['$', '0'..'9']);
    if Result and (S[X + 1] = '$') then
      Result := (X + 1 < L) and CharInSet(S[X + 2], CS_HEX);
  end;

  function on_head: boolean;
  begin
    Result := (S[X] = '''') or on_sharp;
  end;

begin
  PStr := '';
  L := Length(S);
  if (X > 0) and (X <= L) and on_head then
    Result := isPart else
    Result := isNo;
  if Result = isPart then
  begin
    repeat
      if S[X] = '''' then
      begin
        Inc(X);
        while X <= L do
        begin
          if S[X] = '''' then
          begin
            if (X = L) or (S[X + 1] <> '''') then Break;
            Inc(X);
          end;
          PStr := PStr + S[X];
          Inc(X);
        end;
        if X > L then Exit;  // not completed
        Inc(X);
      end
      else
      begin
        V := 0;
        Inc(X);
        if S[X] = '$' then
        begin
          Inc(X);
          while (X <= L) and CharInSet(S[X], CS_HEX) do
          begin
            V := (V * 16) + HexValue(S[X]);
            Inc(X);
          end;
        end
        else
        while (X <= L) and CharInSet(S[X], CS_DIGIT) do
        begin
          V := (V * 10) + (Ord(S[X]) - Ord('0'));
          Inc(X);
        end;
        PStr := PStr + char(V);
      end;
    until (X > L) or not on_head;
    Result := isYes;
  end;
end;

function TryParseStr(const S: string; var X: integer; var Str: string): TLyIsString;
begin
  Result := ParseCStr(S, X, Str);
  if Result = isNo then
    Result := ParsePStr(S, X, Str);
end;

function EncodeCStr(const S: string): string;
var
  I, L, X: integer;
  H: string;
begin
  L := Length(S);
  if L = 0 then Result := '""' else
  begin
    SetLength(Result, (L * 4) + 2);
    X := 1;
    Result[X] := '"';
    for I := 1 to L do
    begin
      Inc(X);
      if CharInSet(S[I], ['"', '''', '\', '?', #0, #10, #13, #9, #11, #7, #8, #12]) then
      begin
        Result[X] := '\';
        Inc(X);
        case S[I] of
          #10: Result[X] := 'n'; // LF: LineFeed
          #13: Result[X] := 'r'; // CR: CarriageReturn
          #9 : Result[X] := 't'; // HT: Horz Tab
          #11: Result[X] := 'v'; // VT: Vert Tab
          #7 : Result[X] := 'a'; // BELL
          #8 : Result[X] := 'b'; // BS: BackSpace
          #12: Result[X] := 'f'; // FF: FormFeed
          #0 : Result[X] := '0';
          else Result[X] := S[I];
        end;
      end
      else
      if CharInSet(S[I], [#1..#127] - [' '..'~']) then
      begin
        H := Format('%.2x', [Ord(S[I])]);
        Result[X] := '\';
        Result[X + 1] := 'x';
        Result[X + 2] := H[1];
        Result[X + 3] := H[2];
        Inc(X, 3);
      end
      else Result[X] := S[I];
    end;
    Inc(X);
    Result[X] := '"';
    SetLength(Result, X);
  end;
end;

function EncodePStr(const S: string): string;
var
  I, L, X, P: integer;
  T: string;
  in_quote: boolean;
begin
  L := Length(S);
  if L = 0 then Result := '''''' else
  begin
    in_quote := false;
    SetLength(Result, (L * 5) + 2);
    X := 0;
    for I := 1 to L do
    begin
      Inc(X);
      if CharInSet(S[I], [#0..#127] - [' '..'~']) then
      begin
        if in_quote then
        begin
          in_quote := false;
          Result[X] := '''';
          Inc(X);
        end;
        Result[X] := '#';
        T := IntToStr(Ord(S[I]));
        for P := 1 to Length(T) do
          Result[X + P] := T[P];
        Inc(X, Length(T));
      end
      else
      begin
        if not in_quote then
        begin
          in_quote := true;
          Result[X] := '''';
          Inc(X);
        end;
        if S[I] = '''' then
        begin
          Result[X] := '''';
          Inc(X);
          Result[X] := '''';
        end
        else Result[X] := S[I];
      end;
    end;
    if in_quote then
    begin
      Inc(X);
      Result[X] := '''';
    end;
    SetLength(Result, X);
  end;
end;

function DoubleQuote(const S: string): string;
begin
  Result := EncodeCStr(S);
end;

function SingleQuote(const S: string): string;
begin
  Result := EncodePStr(S);
end;

function IntToCompare(I: integer): TLyCompare;
begin
  if I = 0 then Result := crEqual else
  if I < 0 then Result := crLess else
                Result := crMore;
end;

function IntToCompare(I: int64): TLyCompare;overload;
begin
  if I = 0 then Result := crEqual else
  if I < 0 then Result := crLess else
                Result := crMore;
end;

function CompareFloat(V1, V2: double): TLyCompare;
begin
  V1 := V1 - V2;
  if IsZero(V1) then Result := crEqual else
  if V1 < 0 then Result := crLess else
    Result := crMore;
end;

function CompareInt64(V1, V2: int64): TLyCompare;
begin
  if V1 = V2 then Result := crEqual else
  if V1 < V2 then Result := crLess else
                  Result := crMore;
end;

function CompareInteger(V1, V2: integer): TLyCompare;
begin
  if V1 = V2 then Result := crEqual else
  if V1 < V2 then Result := crLess else
                  Result := crMore;
end;

function CompareMoney(V1, V2: currency): TLyCompare;
begin
  if V1 = V2 then Result := crEqual else
  if V1 < V2 then Result := crLess else
                  Result := crMore;
end;

function CompareChar(V1, V2: char): TLyCompare;
begin
  Result := IntToCompare(Ord(V1) - Ord(V2));
end;

function CompareChar(V1, V2: char; CaseSensitive: boolean): TLyCompare;
begin
  if CaseSensitive then
    Result := IntToCompare(Ord(V1) - Ord(V2)) else
    Result := IntToCompare(Ord(LowerChar(V1)) - Ord(LowerChar(V2)));
end;

function CompareString(const S1, S2: string): TLyCompare;
begin
  Result := IntToCompare(SysUtils.CompareStr(S1, S2));
end;

function CompareString(const S1, S2: string; CaseSensitive: boolean): TLyCompare;
begin
  if CaseSensitive then
    Result := IntToCompare(SysUtils.CompareStr(S1, S2)) else
    Result := IntToCompare(SysUtils.CompareText(S1, S2));
end;

function CompareTextPos(ItemX1, TextX1, ItemX2, TextX2: integer): integer;
begin
  Result := (ItemX1 - ItemX2);
  if Result = 0 then
    Result := (TextX1 - TextX2);
end;

function CompareTextPos(P1, P2: PLyTextPos): integer;
begin
  Result :=CompareTextPos(P1^.ItemX, P1^.TextX, P2^.ItemX, P2^.TextX);
end;

function CompareTextPos(P1: PLyTextPos; ItemX2, TextX2: integer): integer;
begin
  Result :=CompareTextPos(P1^.ItemX, P1^.TextX, ItemX2, TextX2);
end;

function MatchPatten(const S, Patten: string): boolean;
begin
  {$IFDEF FPC}
  Result := regexpr.ExecRegExpr(Patten, S);
  {$ELSE}
  Result := TRegEx.IsMatch(S, Patten);
  {$ENDIF}
end;

function IsEmptyStr(const S: string): boolean;
begin
  Result := (S = '');
end;

function UpperChar(ch: char): char;
begin
  Result := ch;
  if CharInSet(Result, CS_LOWER) then
    Dec(Result, CS_DISTANCE);
end;

function LowerChar(ch: char): char;
begin
  Result := ch;
  if CharInSet(Result, CS_UPPER) then
    Inc(Result, CS_DISTANCE);
end;

function UpperHead(const S: string): string;
begin
  Result := S;
  if Result <> '' then
    Result[1] := UpperChar(Result[1]);
end;

function LowerHead(const S: string): string;
begin
  Result := S;
  if Result <> '' then
    Result[1] := LowerChar(Result[1]);
end;

function ExtractNameValue(const S: string; var V: string; const separator: string): string;
var
  X: integer;
begin
  X := Pos(separator, S);
  if X > 0 then
  begin
    V := Copy(S, X + Length(separator), Length(S));
    Result := Trim(Copy(S, 1, X - 1));
  end
  else
  begin
    V := '';
    Result := '';
  end;
end;

function ExtractName(const S, separator: string): string;
var
  X: integer;
begin
  X := Pos(separator, S);
  if X > 1 then
    Result := Trim(Copy(S, 1, X - 1)) else
    Result := '';
end;

function ExtractValue(const S, separator: string): string;
var
  X: integer;
begin
  X := Pos(separator, S);
  if X > 0 then
    Result := Copy(S, X + Length(separator), MaxInt) else
    Result := '';
end;

function ExtractNext(var S: string; const separator: string): string;
var
  X: integer;
begin
  X := Pos(separator, S);
  if X > 0 then
  begin
    Result := Copy(S, 1, X - 1);
    S := Copy(S, X + Length(separator), Length(S));
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;

function ExtractNextInteger(var S: string): string;
var
  I: integer;
begin
  I := 1;
  Result := ExtractNextInteger(S, I);
  S := Copy(S, I, Length(S));
end;

function ExtractNextInteger(const S: string; var I: integer): string;
var
  B: integer;
begin
  while I <= Length(S) do
  begin
    if CharInSet(S[I], CS_DIGIT) then
    begin
      B := I;
      repeat Inc(I) until (I > Length(S)) or not CharInSet(S[I], CS_DIGIT);
      Result := Copy(S, B, I - B);
      Exit;
    end;
    Inc(I);
  end;
  Result := '';
end;

function InStrings(const S: string; const List: array of string): boolean;
var
  T: string;
begin
  for T in List do
    if T = S then
    begin
      Result := true;
      Exit;
    end;
  Result := false;
end;

function HashOf(S: pchar): cardinal;
begin
  Result := 0;
  if (S <> nil) and (S^ <> #0) then
  repeat
    Result := ((Result shl 2) or (Result shr (sizeof(Result) * 8 - 2))) xor Ord(S^);
    Inc(S);
  until S^ = #0;
end;

function HashOf(const S: string): cardinal;
begin
  Result := HashOf(pchar(S));
end;

function ParseConfig(const S: string; var ID, Value: string): boolean;
begin
  ID := ExtractNameValue(S, Value);
  Result := (ID <> '');
  if Result then
    Value := Trim(Value);
end;

function RepeatString(const S: string; Times: integer): string;
begin
  Result := '';
  if S <> '' then
    while Times > 0 do
    begin
      Result := Result + S;
      Dec(Times);
    end;
end;

function TrimAll(const S: string): string;
var
  I, N, L, Z: integer;
begin
  L := Length(S);
  N := 0;
  for I := 1 to L do
    if S[I] <= ' ' then
      Inc(N);
  if N = 0 then Result := S else
  if N = L then Result := '' else
  begin
    Z := L - N;
    SetLength(Result, Z);
    N := 0;
    for I := 1 to L do
      if S[I] > ' ' then
      begin
        Inc(N);
        Result[N] := S[I];
        if N = Z then Exit;
      end;
  end;
end;

function ReplaceAll(const S, Patten, NewString: string): string;
begin
  Result := StringReplace(S, Patten, NewString, [rfReplaceAll]);
end;

function RepeatChar(C: char; Times: integer): string;
begin
  Result := StringOfChar(C, Times);
end;

function GetYm: integer;
begin
  Result := GetYmFrom(Date);
end;

function GetYmFrom(Date: TDateTime): integer;
var
  Y, M, D: word;
begin
  DecodeDate(Date, Y, M, D);
  Result := (Y * 100) + M;
end;

function IsYmOf(Y, M: integer): boolean;
begin
  Result := (Y >= 1) and (Y <= 9999) and (M >= 1) and (M <= 12);
end;

function IsYm(Ym: integer): boolean;
begin
  Result := IsYmOf(Ym div 100, Ym mod 100);
end;

function IsYmIn(Ym, MinYM, MaxYM: integer): boolean;
begin
  Result := (Ym >= MinYM) and (Ym <= MaxYM) and IsYm(Ym);
end;

function IsYmStr(const Ym: string): boolean;
begin
  Result := IsYm(StrToYm(Ym));
end;

function YmToStr(Ym: integer): string;
begin
  if Ym > 0 then
    Result := Format('%.6d', [Ym]) else
    Result := '';
end;

function YmToStrOf(Ym: integer; const Delimiter: string): string;
var
  Y, M: integer;
begin
  if Ym > 0 then
  begin
    M := Ym mod 100;
    Y := Ym div 100;
    Result := Format('%.4d%s%.2d', [Y, Delimiter, M]);
  end
  else Result := '';
end;

function StrToYm(const S: string; DefValue: integer): integer;
var
  L: integer;
begin
  L := Length(S);
  if L = 6 then  {yyyymm}
    Result := StrToIntDef(S, DefValue) else
  if L = 7 then  {yyyy-mm}
    Result := StrToIntDef(Copy(S, 1, 4) + Copy(S, 6, 2), DefValue) else
    Result := DefValue;
end;

function DecodeYm(Ym: integer; var Y, M: integer): boolean;
begin
  Y := Ym div 100;
  M := Ym mod 100;
  Result := IsYmOf(Y, M);
end;

function PrevYM(Ym, Offset: integer): integer;
begin
  Result := NextYm(ym, - Offset);
end;

function PrevYmStr(const Ym: string): string;
begin
  Result := YmToStr(PrevYm(StrToYm(Ym)));
end;

function NextYM(Ym, Offset: integer): integer;
var
  Y, M: integer;
begin
  {$IFDEF FPC}
  Y := 0; M := 0;
  {$ENDIF}
  if Offset = 0 then Result := Ym else
  if DecodeYm(Ym, Y, M) then
  begin
    Inc(Offset, Y * 12 + M - 1);
    Y := Offset div 12;
    M := Offset mod 12 + 1;
    Result := (Y * 100) + M;
  end
  else Result := 0;
end;

function NextYmStr(const Ym: string): string;
begin
  Result := YmToStr(NextYm(StrToYm(Ym)));
end;

function GetYmd: integer;
begin
  Result := GetYmdFrom(Date);
end;

function GetYmdFrom(Date: TDateTime): integer;
var
  Y, M, D: word;
begin
  DecodeDate(Date, Y, M, D);
  Result := (Y * 10000) + (M * 100) + D;
end;

function IsYmdOf(Y, M, D: integer): boolean;
var
  T: TDateTime;
begin
  Result := TryEncodeDate(Y, M, D, T);
end;

function IsYmd(Ymd: integer): boolean;
var
  Y, M, D: integer;
begin
  {$IFDEF FPC}
  Y := 0; M := 0; D := 0;
  {$ENDIF}
  Result := DecodeYmd(Ymd, Y, M, D);
end;

function IsYmdStr(const Ymd: string): boolean;
begin
  Result := IsYmd(StrToYmd(Ymd));
end;

function YmdToStr(Ymd: integer): string;
begin
  if Ymd > 0 then
    Result := Format('%.8d', [Ymd]) else
    Result := '';
end;

function YmdToStrOf(Ymd: integer; const Delimiter: string): string;
var
  Y, M, D: integer;
begin
  if Ymd > 0 then
  begin
    D := Ymd mod 100; Ymd := Ymd div 100;
    M := Ymd mod 100;
    Y := Ymd div 100;
    Result := Format('%.4d%s%.2d%s%.2d', [Y, Delimiter, M, Delimiter, D]);
  end
  else Result := '';
end;

function StrToYmd(const S: string; DefValue: integer): integer;
begin
  Result := StrToIntDef(S, 0);
  if not IsYmd(Result) then
    Result := DefValue;
end;

function YmdToDate(Ymd: integer): TDateTime;
var
  Y, M, D: integer;
begin
  D := Ymd mod 100; Ymd := Ymd div 100;
  M := Ymd mod 100;
  Y := Ymd div 100;
  if not TryEncodeDate(Y, M, D, Result) then Result := 0;
end;

function DecodeYmd(Ymd: integer; var Y, M, D: integer): boolean;
begin
  D := Ymd mod 100; Ymd := Ymd div 100;
  M := Ymd mod 100;
  Y := Ymd div 100;
  Result := IsYmdOf(Y, M, D);
end;

function NextYMD(Ymd, Offset: integer): integer;
var
  T: TDateTime;
begin
  if Offset = 0 then Result := Ymd else
  begin
    T := YmdToDate(Ymd);
    if T > 1 then
      Result := GetYmdFrom(IncDay(T, Offset)) else
      Result := 0;
  end;
end;

function NextYmdStr(const Ymd: string): string;
begin
  Result := YmdToStr(NextYmd(StrToYmd(Ymd)));
end;

function PrevYMD(Ymd, Offset: integer): integer;
begin
  Result := NextYMD(Ymd, - Offset);
end;

function PrevYmdStr(const Ymd: string): string;
begin
  Result := YmdToStr(PrevYmd(StrToYmd(Ymd)));
end;

{ md5 }

function MD5SumBuffer(const Buffer: pointer; Count: integer): string;
{$IFNDEF FPC}
var
  M: THashMD5;
{$ENDIF}
begin
  {$IFDEF FPC}
  Result := MD5Print(MD5Buffer(Buffer^, Count));
  {$ELSE}
  M := THashMD5.Create;
  M.Update(Buffer^, Count);
  Result := M.HashAsString;
  {$ENDIF}
end;

function MD5SumString(const S: string): string;
begin
  {$IFDEF FPC}
  Result := MD5Print(MD5String(S));
  {$ELSE}
  THashMD5.GetHashString(S);
  {$ENDIF}
end;

function MD5SumFile(const FileName: string): string;
{$IFNDEF FPC}
var
  F: TFileStream;
  B: array[0..4159] of byte;
  N: integer;
  M: THashMD5;
{$ENDIF}
begin
  Result := '';
  if FileExists(FileName) then
  begin
    {$IFDEF FPC}
    Result := MD5Print(MD5File(FileName));
    {$ELSE}
    F := TFileStream.Create(FileName, fmShareDenyWrite);
    try
      M := THashMD5.Create;
      N := F.Read(B[0], 4096);
      while N > 0 do
      begin
        M.Update(B[0], N);
        N := F.Read(B[0], 4096);
      end;
      Result := M.HashAsString;
    finally
      F.Free;
    end;
    {$ENDIF}
  end;
end;

function MD5TrySumFile(const FileName: string): string;
begin
  try
    if FileExists(FileName) then
      Result := MD5SumFile(FileName) else
      Result := '';
  except
    Result := '';
  end;
end;

function SHA1SumBuffer(const Buffer: pointer; Count: integer): string;
{$IFNDEF FPC}
var
  M: THashSHA1;
{$ENDIF}
begin
  {$IFDEF FPC}
  Result := SHA1Print(SHA1Buffer(Buffer^, Count));
  {$ELSE}
  M := THashSHA1.Create;
  M.Update(Buffer^, Count);
  Result := M.HashAsString;
  {$ENDIF}
end;

function SHA1SumString(const S: string): string;
begin
  {$IFDEF FPC}
  Result := SHA1Print(SHA1String(S));
  {$ELSE}
  THashSHA1.GetHashString(S);
  {$ENDIF}
end;

function SHA1SumFile(const FileName: string): string;
{$IFNDEF FPC}
var
  F: TFileStream;
  B: array[0..4159] of byte;
  N: integer;
  M: THashSHA1;
{$ENDIF}
begin
  Result := '';
  if FileExists(FileName) then
  begin
    {$IFDEF FPC}
    Result := SHA1Print(SHA1File(FileName));
    {$ELSE}
    F := TFileStream.Create(FileName, fmShareDenyWrite);
    try
      M := THashSHA1.Create;
      N := F.Read(B[0], 4096);
      while N > 0 do
      begin
        M.Update(B[0], N);
        N := F.Read(B[0], 4096);
      end;
      Result := M.HashAsString;
    finally
      F.Free;
    end;
    {$ENDIF}
  end;
end;

function SHA1TrySumFile(const FileName: string): string;
begin
  try
    if FileExists(FileName) then
      Result := SHA1SumFile(FileName) else
      Result := '';
  except
    Result := '';
  end;
end;

{ object }

procedure FreeAll(const Objects: TList);
var
  I: integer;
  M: TObject;
begin
  if Objects <> nil then
    for I := Objects.Count - 1 downto 0 do
      if I < Objects.Count then
      begin
        M := TObject(Objects[I]);
        Objects.Delete(I);
        if M <> nil then M.Free;
      end;
end;

procedure FreeAll(const Objects: array of TObject);
var
  I: integer;
  M: TObject;
begin
  for I := Length(Objects) - 1 downto 0 do
  begin
    M := TObject(Objects[I]);
    if M <> nil then M.Free;
  end;
end;

function LoadDLL(const FileName: string; var Handle: THandle): boolean;
begin
  Handle := LoadLibrary(PChar(FileName));
  Result := (Handle <> 0);
end;

function FreeDLL(Handle: THandle): boolean;
begin
  Result := (Handle <> 0);
  if Result then
    FreeLibrary(Handle);
end;

function GetProcAddr(Handle: THandle; const ProcName: string): pointer;
begin
  Result := GetProcAddress(Handle, PChar(ProcName));
end;

function stdin: integer;
begin
  {$IFDEF MSWINDOWS}
  Result := GetStdhandle(STD_INPUT_HANDLE);
  {$ELSE}
  Result := StdInputHandle;
  {$ENDIF}
end;

function stdout: integer;
begin
  {$IFDEF MSWINDOWS}
  Result := GetStdhandle(STD_OUTPUT_HANDLE);
  {$ELSE}
  Result := StdOutputHandle;
  {$ENDIF}
end;

function stderr: integer;
begin
  {$IFDEF MSWINDOWS}
  Result := GetStdhandle(STD_ERROR_HANDLE);
  {$ELSE}
  Result := StdErrorHandle;
  {$ENDIF}
end;

procedure Swap(var V1, V2: integer);
var
  T: integer;
begin
  T := V1;
  V1 := V2;
  V2 := T;
end;

{ TLyFileEditLock }

function TLyFileEditLock.GetLocked: boolean;
begin
  Result := (FHandle <> {$IFDEF FPC}feInvalidHandle
    {$ELSE}INVALID_HANDLE_VALUE{$ENDIF});
end;

constructor TLyFileEditLock.Create;
begin
  FHandle := {$IFDEF FPC}feInvalidHandle
    {$ELSE}INVALID_HANDLE_VALUE{$ENDIF};
end;

destructor TLyFileEditLock.Destroy;
begin
  Unlock;
  inherited;
end;

class function TLyFileEditLock.ReadFor(const AFileName: string;
  var Buffer; Count: integer): integer;
var
  F: string;
  H: THandle;
begin
  F := AFileName + FileEditLockExt;
  H := FileOpen(F, fmShareDenyNone);
  if H <> {$IFDEF FPC}feInvalidHandle{$ELSE}INVALID_HANDLE_VALUE{$ENDIF} then
  begin
    Result := FileRead(H, Buffer, Count);
    FileClose(H);
  end
  else Result := 0;
end;

class function TLyFileEditLock.ReadEditInfo(const AFileName: string;
  var EI: RLyEditInfo): boolean;
begin
  Result := (ReadFor(AFileName, EI, sizeof(EI)) = sizeof(EI));
end;

class function TLyFileEditLock.LockApplication: TLyFileEditLock;
begin
  Result := TLyFileEditLock.Create;
  try
    if not Result.LockForFile(ParamStr(0)) then
      FreeAndNil(Result);
  except
    FreeAndNil(Result);
  end;
end;

function TLyFileEditLock.LockForFile(const AFileName: string): boolean;
var
  F, L: string;
begin
  if AFileName <> '' then
  begin
    F := ExpandFileName(AFileName);
    if not SameFileName(FFileName, F) then
    begin
      Unlock;
      L := F + FileEditLockExt;
      FHandle := FileCreate(L, fmShareDenyWrite, 0);
      if FHandle <> {$IFDEF FPC}feInvalidHandle{$ELSE}INVALID_HANDLE_VALUE{$ENDIF} then
      begin
        FFileName := F;
        FLockFileName := L;
      end;
    end;
  end
  else Unlock;
  Result := GetLocked;
end;

function TLyFileEditLock.Unlock: boolean;
begin
  Result := (FHandle <> {$IFDEF FPC}feInvalidHandle{$ELSE}INVALID_HANDLE_VALUE{$ENDIF});
  if Result then
  begin
    SysUtils.FileClose(FHandle);
    FHandle := {$IFDEF FPC}feInvalidHandle{$ELSE}INVALID_HANDLE_VALUE{$ENDIF};
    SysUtils.DeleteFile(FLockFileName);
  end;
end;

function TLyFileEditLock.Write(const Buffer; Count: integer): integer;
begin
  Result := SysUtils.FileWrite(FHandle, Buffer, Count);
end;

function TLyFileEditLock.WriteEditInfo(const EI: RLyEditInfo): boolean;
begin
  Result := (Write(EI, sizeof(EI)) = sizeof(EI));
end;

{ TLyObject }

function TLyObject.DecRefcount: integer;
begin
  if Self <> nil then
  begin
    Dec(FRefCount);
    Result := FRefCount;
    if Result < 1 then Free;
  end
  else Result := 0;
end;

function TLyObject.IncRefcount: integer;
begin
  if Self <> nil then
  begin
    Inc(FRefCount);
    Result := FRefCount;
  end
  else Result := 0;
end;

{ TLyNameObject }

constructor TLyNameObject.Create(const AName: string);
begin
  FName := AName;
end;

function TLyNameObject.NameIs(const AName: array of string): boolean;
begin
  Result := MatchID(FName, AName);
end;

function TLyNameObject.ToString: string;
begin
  Result := FName;
end;

function TLyNameObject.NameIs(const AName: string): boolean;
begin
  Result := MatchID(FName, AName);
end;

{ TLyNameValue }

procedure TLyNameValue.Assign(Source: TLyNameValue);
begin
  FName := Source.FName;
  FValue := Source.FValue;
end;

constructor TLyNameValue.Create(const AName, AValue: string);
begin
  FName := AName;
  FValue := AValue;
end;

function TLyNameValue.IsTrueValue: boolean;
begin
  Result := MatchID(FValue, 'true') or (StrToIntDef(FValue, 0) <> 0);
end;

function TLyNameValue.ToString: string;
begin
  Result := FName + '=' + FValue;
end;

{ TLyAttr }

function TLyAttr.ToString: string;
begin
  if FName = '' then Result := '' else
  if FValue <> '' then
    Result := FName + '=' + DoubleQuote(FValue) else
    Result := FName;
end;

{ TLyTag }

procedure TLyTag.Assign(Source: TLyTag);
var
  T, S: TLyAttr;
begin
  Clear;
  FName := Source.FName;
  FValue := Source.FValue;
  S := Source.FAttr;
  if S <> nil then
  begin
    FAttr := TLyAttr.Create(S.FName, S.FValue);
    T := FAttr;
    while S.FNext <> nil do
    begin
      S := S.FNext;
      T.FNext := TLyAttr.Create(S.FName, S.FValue);
      T := T.FNext;
    end;
  end;
end;

procedure TLyTag.Clear;
begin
  FName := '';
  FValue := '';
  ClearAttr;
end;

procedure TLyTag.ClearAttr;
var
  A: TLyAttr;
begin
  while FAttr <> nil do
  begin
    A := FAttr;
    FAttr := FAttr.FNext;
    A.FNext := nil;
    A.Free;
  end;
end;

destructor TLyTag.Destroy;
begin
  Clear;
  inherited;
end;

function TLyTag.Find(const AName: string): TLyAttr;
begin
  Result := FAttr;
  while Result <> nil do
  begin
    if Result.NameIs(AName) then Exit;
    Result := Result.FNext;
  end;
end;

function TLyTag.FindByName(const AName: string): TLyAttr;
begin
  Result := Find(AName);
  if Result = nil then
    Throw('Attribute "%s" is not defined', [AName]);
end;

function TLyTag.Get(const AName: string): string;
var
  A: TLyAttr;
begin
  A := Find(AName);
  if A <> nil then
    Result := A.FValue else
    Result := '';
end;

function TLyTag.Has(const AName: string): boolean;
begin
  Result := (Find(AName) <> nil);
end;

function TLyTag.Parse(const Text: string; var Index: integer; const Head: char): boolean;
var
  L: integer;
  A: string;
  ch, begc, endc: char;

  function next_char: boolean;
  begin
    Result := (Index < L);
    if Result then
    begin
      Inc(Index);
      ch := Text[Index];
    end
    else ch := #0;
  end;

  function skip_space: boolean;
  begin
    while ch <= ' ' do if not next_char then Break;
    Result := (ch <> #0);
  end;

  function parse_expression: string;
  var
    I: integer;
  begin
    I := Index + 1;
    while next_char and (ch <> #0) do
      if ch = '''' then ParsePStr(Text, Index, Result) else
      if ch = '"' then ParseCStr(Text, Index, Result) else
      if ch = '%' then
        if next_char and (ch = endc) then Break;
    Result := Trim(Copy(Text, I, Index - I - 1));
  end;

  function parse_value(OnAttr: boolean): string;
  begin
    Result := '';
    if next_char then
      if TryParseStr(Text, Index, Result) <> isNo then
      begin
        if Index <= L then
          ch := Text[Index] else
          ch := #0;
      end
      else
      begin
        while (ch <> endc) and ((ch > ' ') or not OnAttr) do
        begin
          Result := Result + ch;
          if not next_char then Break;
        end;
        Result := Trim(Result);
      end;
  end;

  function parse_name: string;
  begin
    Result := '';
    while (ch > ' ') and (ch <> '=') and (ch <> endc) do
    begin
      Result := Result + ch;
      if not next_char then Exit;
    end;
  end;

begin
  Clear;
  L := Length(Text);
  Result := (Index < L) and CharInSet(Text[Index], ['<', '[', '{', '(']) and
    ((Head = #0) or (Head = Text[Index]));
  if Result then
  begin
    endc := #0;
    begc := Text[Index];
    case begc of
      '<': endc := '>';
      '[': endc := ']';
      '(': endc := ')';
      '{': endc := '}';
    end;
    ch := #0;
    if next_char and skip_space and (ch <> endc) then
    begin
      if ch = '%' then
      begin
        FName := '%';
        FValue := Trim(parse_expression);
      end
      else
      begin
        FName := parse_name;
        if skip_space and (ch = '=') then
          FValue := parse_value(false);
        while skip_space and (ch <> endc) do
        begin
          A := parse_name;
          if ch = '=' then
            Put(A, parse_value(true)) else
            Put(A);
        end;
      end;
      if (ch <> endc) or (FName = '') then Clear;
    end;
    Result := (FName <> '');
  end;
end;

function TLyTag.Put(const AName: string; AValue: integer): TLyAttr;
begin
  if AName <> '' then
    Result := Put(AName, IntToStr(AValue)) else
    Result := nil;
end;

function TLyTag.PutBoolean(const AName: string; AValue: boolean): TLyAttr;
begin
  if AValue then
    Result := Put(AName, 'true') else
    Result := Put(AName, 'false');
end;

function TLyTag.PutCurrency(const AName: string; AValue: Currency): TLyAttr;
begin
  Result := Put(AName, CurrToStr(AValue));
end;

function TLyTag.PutDate(const AName: string; AValue: TDate): TLyAttr;
begin
  Result := Put(AName, FormatDateTime('yyyy/mm/dd', AValue));
end;

function TLyTag.PutDateTime(const AName: string; AValue: TDateTime): TLyAttr;
begin
  Result := Put(AName, FormatDateTime('yyyy/mm/dd hh:nn:ss zzz', AValue));
end;

function TLyTag.PutFloat(const AName: string; AValue: double): TLyAttr;
begin
  Result := Put(AName, FloatToStr(AValue));
end;

function TLyTag.PutInt64(const AName: string; AValue: int64): TLyAttr;
begin
  Result := Put(AName, IntToStr(AValue));
end;

function TLyTag.PutInteger(const AName: string; AValue: integer): TLyAttr;
begin
  Result := Put(AName, IntToStr(AValue));
end;

procedure TLyTag.Remove(const AName: string);
var
  T, N: TLyAttr;
begin
  if FAttr <> nil then
    if FAttr.NameIs(AName) then
    begin
      T := FAttr;
      FAttr := T.FNext;
      T.FNext := nil;
      T.Free;
    end
    else
    begin
      T := FAttr;
      while T.FNext <> nil do
      begin
        if T.FNext.NameIs(AName) then
        begin
          N := T.FNext;
          T.FNext := N.FNext;
          N.FNext := nil;
          N.Free;
          Exit;
        end;
        T := T.FNext;
      end;
    end;
end;

function TLyTag.ToString: string;
var
  A: TLyAttr;
begin
  if FValue <> '' then
    Result := FName + '=' + DoubleQuote(FValue) else
    Result := FName;
  A := FAttr;
  while A <> nil do
  begin
    if A.FName <> '' then
      Result := Result + ' ' + A.ToString;
    A := A.FNext;
  end;
end;

function TLyTag.Put(const AName: string): TLyAttr;
begin
  Result := Put(AName, '');
end;

function TLyTag.Put(const AName, AValue: string): TLyAttr;
begin
  if FAttr = nil then
  begin
    FAttr := TLyAttr.Create(AName, AValue);
    Result := FAttr;
  end
  else
  if FAttr.NameIs(AName) then
  begin
    FAttr.FValue := AValue;
    Result := FAttr;
  end
  else
  begin
    Result := FAttr;
    while Result.FNext <> nil do
    begin
      Result := Result.FNext;
      if Result.NameIs(AName) then
      begin
        Result.FValue := AValue;
        Exit;
      end;
    end;
    Result.FNext := TLyAttr.Create(AName, AValue);
    Result := Result.FNext;
  end;
end;

function TLyTag.Get(const AName: array of string): string;
var
  I: integer;
  T: TLyAttr;
begin
  for I := 0 to Length(AName) - 1 do
  begin
    T := Find(AName[I]);
    if T <> nil then
    begin
      Result := T.FValue;
      Exit;
    end;
  end;
  Result := '';
end;

function TLyTag.GetBoolean(const AName: string): boolean;
var
  A: TLyAttr;
begin
  A := Find(AName);
  Result := (A <> nil) and A.IsTrueValue;
end;

function TLyTag.GetByName(const AName: string): string;
begin
  Result := FindByName(AName).FValue;
end;

function TLyTag.GetCurrency(const AName: string): currency;
var
  A: TLyAttr;
begin
  A := Find(AName);
  if A <> nil then
    Result := StrToCurrDef(A.FValue, 0) else
    Result := 0;
end;

function TLyTag.GetFloat(const AName: string): double;
var
  A: TLyAttr;
begin
  A := Find(AName);
  if A <> nil then
    Result := StrToFloatDef(A.FValue, 0) else
    Result := 0;
end;

function TLyTag.GetInt64(const AName: string): int64;
var
  A: TLyAttr;
begin
  A := Find(AName);
  if A <> nil then
    Result := StrToInt64Def(A.FValue, 0) else
    Result := 0;
end;

function TLyTag.GetInteger(const AName: string): integer;
var
  A: TLyAttr;
begin
  A := Find(AName);
  if A <> nil then
    Result := StrToIntDef(A.FValue, 0) else
    Result := 0;
end;

function TLyTag.GetDate(const AName: string): TDate;
var
  A: TLyAttr;
  I, Y, M, D: integer;
  T: TDateTime;
begin
  A := Find(AName);
  if A <> nil then
  begin
    I := 1;
    Y := StrToIntDef(ExtractNextInteger(A.FValue, I), 0);
    M := StrToIntDef(ExtractNextInteger(A.FValue, I), 0);
    D := StrToIntDef(ExtractNextInteger(A.FValue, I), 0);
    T := 0;
    if TryEncodeDate(Y, M, D, T) then
      Result := T else
      Result := 0;
  end
  else Result := 0;
end;

function TLyTag.GetDateTime(const AName: string): TDateTime;
var
  A: TLyAttr;
  I, Y, M, D, H, N, S, Z: integer;
  T: TDateTime;
begin
  A := Find(AName);
  if A <> nil then
  begin
    I := 1;
    Y := StrToIntDef(ExtractNextInteger(A.FValue, I), 0);
    M := StrToIntDef(ExtractNextInteger(A.FValue, I), 0);
    D := StrToIntDef(ExtractNextInteger(A.FValue, I), 0);
    H := StrToIntDef(ExtractNextInteger(A.FValue, I), 0);
    N := StrToIntDef(ExtractNextInteger(A.FValue, I), 0);
    S := StrToIntDef(ExtractNextInteger(A.FValue, I), 0);
    Z := StrToIntDef(ExtractNextInteger(A.FValue, I), 0);
    T := 0;
    if TryEncodeDateTime(Y, M, D, H, N, S, Z, T) then
      Result := T else
      Result := 0;
  end
  else Result := 0;
end;

{ TLyAgent }

function TLyAgent.GetActive: boolean;
begin
  Result := (FHost <> nil);
end;

procedure TLyAgent.SetHost(AHost: TObject);
begin
  FHost := AHost;
end;

{ TLyStrings }

function TLyStrings.Add(const S: string): integer;
var
  X, H, I, C: Integer;
begin
  if not FIsStringList and FSorted then
  begin
    X := 0;
    H := FStrings.Count - 1;
    while X <= H do
    begin
      I := (X + H) shr 1;
      C := Compare(FStrings[I], S);
      if C < 0 then X := I + 1 else
      if C > 0 then H := I - 1 else
      if C = 0 then
      begin
        FStrings.Insert(I, S);
        Result := I;
        Exit;
      end;
    end;
    FStrings.Insert(X, S);
    Result := X;
  end
  else Result := FStrings.Add(S);
end;

procedure TLyStrings.Assign(Source: TStrings);
begin
  FStrings.Assign(Source);
  CheckSort;
end;

procedure TLyStrings.BeginUpdate;
begin
  FStrings.BeginUpdate;
end;

procedure TLyStrings.CheckSort;
begin
  if not FIsStringList and FSorted then Sort;
end;

procedure TLyStrings.Clear;
begin
  FStrings.Clear;
end;

function TLyStrings.Compare(const S1, S2: string): integer;
begin
  if GetCaseSensitive then
    Result := AnsiCompareStr(S1, S2) else
    Result := AnsiCompareText(S1, S2);
end;

function TLyStrings.Copy(Index, ItemCount: integer): TStrings;
var
  I: integer;
begin
  Result := TStringList.Create;
  if Index < 0 then
  begin
    Inc(ItemCount, Index);
    Index := 0;
  end;
  ItemCount := Max(0, Min(FStrings.Count - Index, ItemCount));
  for I := 1 to ItemCount do
  begin
    Result.Add(FStrings[Index]);
    Inc(Index);
  end;
end;

constructor TLyStrings.Create;
begin
  FStrings := TStringList.Create;
  FIsStringList := true;
  FFreeStrings := true;
  FHost := FStrings;
end;

constructor TLyStrings.CreateFor(AStrings: TStrings);
begin
  SetStrings(AStrings);
end;

procedure TLyStrings.Delete(Index: integer);
begin
  FStrings.Delete(Index);
end;

procedure TLyStrings.DeleteEmptyLines;
var
  I: integer;
begin
  FStrings.BeginUpdate;
  try
    for I := FStrings.Count - 1 downto 0 do
      if FStrings[I] = '' then
        FStrings.Delete(I);
  finally
    FStrings.EndUpdate;
  end;
end;

procedure TLyStrings.DeleteLast;
var
  I: integer;
begin
  I := FStrings.Count - 1;
  if I >= 0 then
    FStrings.Delete(I);
end;

destructor TLyStrings.Destroy;
begin
  if (FStrings <> nil) and FFreeStrings then
    FreeAndNil(FStrings);
  inherited;
end;

function TLyStrings.Process(StrFunc: TLyStrFunc): integer;
var
  I: integer;
  F, S: string;
begin
  FStrings.BeginUpdate;
  try
    Result := 0;
    for I := 0 to FStrings.Count - 1 do
    begin
      F := FStrings[I];
      S := StrFunc(F); //Process(SysUtils.Trim);
      if F <> S then
      begin
        FStrings[I] := S;
        Inc(Result);
      end;
    end;
    CheckSort;
  finally
    FStrings.EndUpdate;
  end;
end;

procedure TLyStrings.EndUpdate;
begin
  FStrings.EndUpdate;
end;

function TLyStrings.GetCaseSensitive: boolean;
begin
  if FIsStringList then
    Result := (FStrings as TStringList).CaseSensitive else
    Result := FCaseSensitive;
end;

function TLyStrings.GetCommaText: string;
begin
  Result := FStrings.CommaText;
end;

function TLyStrings.GetCount: integer;
begin
  Result := FStrings.Count;
end;

function TLyStrings.GetVFX(Index: Integer): string;
begin
  Result := FStrings.ValueFromIndex[Index];
end;

function TLyStrings.GetItem(Index: integer): string;
begin
  Result := FStrings[Index];
end;

function TLyStrings.GetName(Index: integer): string;
begin
  Result := FStrings.Names[Index];
end;

function TLyStrings.GetSorted: boolean;
begin
  if FIsStringList then
    Result := (FStrings as TStringList).Sorted else
    Result := FSorted;
end;

function TLyStrings.GetText: string;
begin
  Result := FStrings.Text;
end;

function TLyStrings.GetStringList: TStringList;
begin
  if FIsStringList then
    Result := FStrings as TStringList else
    Result := nil;
end;

function TLyStrings.GetValue(const Name: string): string;
begin
  Result := FStrings.Values[Name];
end;

function TLyStrings.IndexOf(const S: string; Index: integer): integer;
begin
  if Index >= 0 then
    while Index < FStrings.Count do
    begin
      if Same(S, FStrings[Index]) then
      begin
        Result := Index;
        Exit;
      end;
      Inc(Index);
    end;
  Result := -1;
end;

function TLyStrings.IndexOf(const S: string): integer;
begin
  Result := FStrings.IndexOf(S);
end;

function TLyStrings.IndexOfName(const S: string; Index: integer): integer;
begin
  if Index >= 0 then
    while Index < FStrings.Count do
    begin
      if Same(S, FStrings.Names[Index]) then
      begin
        Result := Index;
        Exit;
      end;
      Inc(Index);
    end;
  Result := -1;
end;

function TLyStrings.IndexOfName(const S: string): integer;
begin
  Result := FStrings.IndexOfName(S);
end;

function TLyStrings.IndexOfValue(const S: string; Index: integer): integer;
begin
  if Index >= 0 then
    while Index < FStrings.Count do
    begin
      if Same(S, FStrings.ValueFromIndex[Index]) then
      begin
        Result := Index;
        Exit;
      end;
      Inc(Index);
    end;
  Result := -1;
end;

function TLyStrings.IndexOfValue(const S: string): integer;
begin
  Result := IndexOfValue(S, 0);
end;

function TLyStrings.Insert(Index: Integer; const S: string): integer;
begin
  if GetSorted then Result := Add(S) else
  begin
    FStrings.Insert(Index, S);
    Result := Index;
  end;
end;

function TLyStrings.GetEmpty: boolean;
begin
  Result := (FStrings.Count = 0);
end;

procedure TLyStrings.LoadFromFile(const FileName: string);
begin
  FStrings.LoadFromFile(FileName);
  FSorted := false;
end;

function TLyStrings.CopyLeft(ItemCount: integer): TStrings;
begin
  Result := Copy(0, ItemCount);
end;

procedure TLyStrings.Pack;
var
  I: integer;
  S, F: string;
begin
  FStrings.BeginUpdate;
  try
    for I := FStrings.Count - 1 downto 0 do
    begin
      F := FStrings[I];
      S := SysUtils.TrimRight(F);
      if S = '' then FStrings.Delete(I) else
      if S <> F then FStrings[I] := S;
    end;
    CheckSort;
  finally
    FStrings.EndUpdate;
  end;
end;

procedure TLyStrings.Remove(const S: string);
var
  I: integer;
begin
  I := FStrings.IndexOf(S);
  if I >= 0 then
    FStrings.Delete(I);
end;

procedure TLyStrings.Remove(Index: integer);
begin
  if (Index >= 0) and (Index < FStrings.Count) then
    FStrings.Delete(Index);
end;

procedure TLyStrings.RemoveByName(const S: string);
var
  I: integer;
begin
  I := FStrings.IndexOfName(S);
  if I >= 0 then
    FStrings.Delete(I);
end;

procedure TLyStrings.RemoveByValue(const S: string);
var
  I: integer;
begin
  I := IndexOfValue(S);
  if I >= 0 then
    FStrings.Delete(I);
end;

procedure TLyStrings.RemoveEmptyLines;
begin
  DeleteEmptyLines;
end;

procedure TLyStrings.RemoveLast;
begin
  DeleteLast;
end;

procedure TLyStrings.RemoveMatchedLines(const Patten: string);
begin
  DeleteMatchedLines(Patten);
end;

procedure TLyStrings.Reverse;
var
  L, R: integer;
begin
  FStrings.BeginUpdate;
  try
    SetSorted(false);
    L := 0;
    R := FStrings.Count - 1;
    while L < R do
    begin
      FStrings.Exchange(L, R);
      Inc(L);
      Dec(R);
    end;
  finally
    FStrings.EndUpdate;
  end;
end;

function TLyStrings.CopyRight(ItemCount: integer): TStrings;
begin
  Result := Copy(FStrings.Count - ItemCount, ItemCount);
end;

procedure TLyStrings.DeleteMatchedLines(const Patten: string);
var
  I: integer;
begin
  FStrings.BeginUpdate;
  try
    for I := FStrings.Count - 1 downto 0 do
      if MatchPatten(FStrings[I], Patten) then
        FStrings.Delete(I);
  finally
    FStrings.EndUpdate;
  end;
end;

function TLyStrings.Same(const S1, S2: string): boolean;
begin
  Result := (Compare(S1, S2) = 0);
end;

procedure TLyStrings.SaveToFile(const FileName: string);
begin
  FStrings.SaveToFile(FileName);
end;

function TLyStrings.SelectMatched(const Patten: string): TStrings;
var
  I: integer;
begin
  Result := TStringList.Create;
  for I := 0 to FStrings.Count - 1 do
    if MatchPatten(FStrings[I], Patten) then
      Result.Add(FStrings[I]);
end;

procedure TLyStrings.SetCaseSensitive(Value: boolean);
begin
  if FIsStringList then
    (FStrings as TStringList).CaseSensitive := Value else
  if FCaseSensitive <> Value then
  begin
    FCaseSensitive := Value;
    CheckSort;
  end;
end;

procedure TLyStrings.SetCommaText(const Value: string);
begin
  FStrings.CommaText := Value;
  CheckSort;
end;

procedure TLyStrings.SetCount(Value: integer);
begin
  if Value < 1 then Clear else
  begin
    while Value > GetCount do FStrings.Add('');
    while Value < GetCount do DeleteLast;
  end;
end;

procedure TLyStrings.SetHost(AHost: TObject);
begin
  if AHost <> nil then
    SetStrings(AHost as TStrings) else
    SetStrings(nil);
end;

procedure TLyStrings.SetVFX(Index: Integer; const Value: string);
begin
  FStrings.ValueFromIndex[Index] := Value;
  CheckSort;
end;

procedure TLyStrings.SetItem(Index: integer; const Value: string);
begin
  FStrings[Index] := Value;
  CheckSort;
end;

procedure TLyStrings.SetName(Index: integer; const Value: string);
begin
  FStrings[Index] := Value + '=' + FStrings.ValueFromIndex[Index];
  CheckSort;
end;

procedure TLyStrings.SetSorted(Value: boolean);
begin
  if FIsStringList then
    (FStrings as TStringList).Sorted := Value else
  if FSorted <> Value then
  begin
    FSorted := Value;
    if FSorted then Sort;
  end;
end;

procedure TLyStrings.SetStrings(Value: TStrings);
begin
  if FStrings <> Value then
  begin
    if FFreeStrings then
    begin
      FreeAndNil(FStrings);
      FHost := nil;
    end;

    FSorted := false;
    FCaseSensitive := false;

    if Value <> nil then
    begin
      FStrings := Value;
      FIsStringList := (FStrings is TStringList);
      FFreeStrings := false;
    end
    else
    begin
      FStrings := TStringList.Create;
      FIsStringList := true;
      FFreeStrings := true;
    end;

    FHost := FStrings;
  end;
end;

procedure TLyStrings.SetText(const Value: string);
begin
  FStrings.Text := Value;
  CheckSort;
end;

procedure TLyStrings.SetValue(const Name, Value: string);
begin
  FStrings.Values[Name] := Value;
  CheckSort;
end;

procedure TLyStrings.Sort;

  procedure do_sort(L, R: integer);
  var
    I, J, P: Integer;
  begin
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        while Compare(FStrings[I], FStrings[P]) < 0 do Inc(I);
        while Compare(FStrings[J], FStrings[P]) > 0 do Dec(J);
        if I <= J then
        begin
          if I <> J then FStrings.Exchange(I, J);
          if P = I then P := J else
          if P = J then P := I;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then do_sort(L, J);
      L := I;
    until I >= R;
  end;

begin
  if not FIsStringList or not (FStrings as TStringList).Sorted then
  begin
    FStrings.BeginUpdate;
    try
      do_sort(0, FStrings.Count - 1);
    finally
      FStrings.EndUpdate;
    end;
  end;
end;

function TLyStrings.ToString: string;
begin
  Result := FStrings.Text;
end;

procedure TLyStrings.TryLoadFromFile(const FileName: string);
begin
  if FileExists(FileName) then
  try
    LoadFromFile(FileName);
  except
    { nothing }
  end;
end;

procedure TLyStrings.Unique;
var
  I, X: integer;
begin
  FStrings.BeginUpdate;
  try
    if FStrings.Count > 1 then
      if GetSorted then
      begin
        for I := FStrings.Count - 2 downto 0 do
          if Same(FStrings[I], FStrings[I + 1]) then
            Delete(I + 1);
      end
      else
      begin
        for I := FStrings.Count - 2 downto 0 do
        begin
          X := IndexOf(FStrings[I], I + 1);
          if X >= 0 then
            Delete(X);
        end;
      end;
  finally
    FStrings.EndUpdate;
  end;
end;

{ TLyStringList }

constructor TLyStringList.CreateFor(AStrings: TStrings);
begin
  inherited CreateFor(AStrings as TStringList);
end;

{ TLySpinLock }

constructor TLySpinLock.Create;
begin
  FCriticalSection := SyncObjs.TCriticalSection.Create;
end;

destructor TLySpinLock.Destroy;
begin
  FreeAndNil(FCriticalSection);
end;

procedure TLySpinLock.Enter;
begin
  FCriticalSection.Enter;
end;

procedure TLySpinLock.Leave;
begin
  FCriticalSection.Leave;
end;

function TLySpinLock.TryEnter: boolean;
begin
  Result := FCriticalSection.TryEnter;
end;

{ TLyCharactor }

procedure TLyCharactor.Clear;
begin
  SetText('');
end;

function TLyCharactor.Next: boolean;
begin
  Result := (FPosition < FSize);
  if Result then
  begin
    Inc(FPosition);
    FChar := FText[FPosition];
  end
  else Stop;
end;

function TLyCharactor.ParseCString(var S: string): boolean;
var
  V: char;
begin
  S := '';
  Result := (FChar = '"') and Next;
  if Result then
  begin
    while FChar <> '"' do
    begin
      Result := ParseEscChar(V) and NotEnded;
      if not Result then Exit;
      S := S + V;
    end;
    Result := (FChar = '"');
    if Result then Next;
  end;
end;

function TLyCharactor.ParseDecimal(var I: cardinal; N: integer): integer;
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

function TLyCharactor.ParseHex(var I: cardinal; N: integer): integer;
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

function TLyCharactor.First: boolean;
begin
  FPosition := 1;
  Result := (FPosition <= FSize);
  if Result then
    FChar := FText[FPosition] else
    FChar := #0;
end;

function TLyCharactor.GetEnded: boolean;
begin
  Result := (FPosition > FSize);
end;

function TLyCharactor.ParseCChar(var C: char): boolean;
begin
  Result := (FChar = '''') and (GetChar <> '''');
  if Result then
  begin
    Result := ParseEscChar(C) and (FChar = '''');
    if Result then Next;
  end;
end;

function TLyCharactor.ParseChar(var C: char): boolean;
begin
  if FChar = '#' then
    Result := ParsePChar(C) else
  if FChar = '''' then
    Result := ParseCChar(C) else
    Result := false;
end;

function TLyCharactor.ParseOctal(var I: cardinal; N: integer): integer;
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

function TLyCharactor.Seek(Chars: TSysCharSet): boolean;
begin
  while not CharInSet(FChar, Chars) and Next do;
  Result := NotEnded;
end;

function TLyCharactor.PeekNextChar: char;
begin
  if FPosition < FSize then
    Result := FText[FPosition + 1] else
    Result := #0;
end;

function TLyCharactor.PeekNextString(Len: integer): string;
begin
  Result := Copy(FText, FPosition + 1, Len);
end;

function TLyCharactor.GetChar: char;
begin
  Next;
  Result := FChar;
end;

function TLyCharactor.GetNotEnded: boolean;
begin
  Result := (FPosition <= FSize);
end;

function TLyCharactor.MatchChar(Chars: TSysCharSet): boolean;
begin
  Result := CharInSet(FChar, Chars);
end;

function TLyCharactor.MatchChar(Ch: char): boolean;
begin
  Result := (FChar = Ch);
end;

function TLyCharactor.MatchDigit: boolean;
begin
  Result := MatchChar(CS_DIGIT);
end;

function TLyCharactor.MatchHead: boolean;
begin
  Result := MatchChar(CS_HEAD);
end;

function TLyCharactor.MatchSpace: boolean;
begin
  Result := MatchChar(CS_SPACE);
end;

function TLyCharactor.MatchString(const S: string): boolean;
begin
  Result := (S <> '') and (S = PeekString(Length(S)));
end;

function TLyCharactor.MatchText(const S: string): boolean;
begin
  Result := (S <> '') and SameText(S, PeekString(Length(S)));
end;

function TLyCharactor.Next(Count: integer): boolean;
begin
  Result := false;
  while (Count > 0) and Next do Dec(Count);
  Result := NotEnded;
end;

function TLyCharactor.ParsePChar(var C: char): boolean;
var
  V: cardinal;
begin
  Result := (FChar = '#') and CharInSet(GetChar, CS_DIGIT + ['$']);
  if Result then
  begin
    if FChar = '$' then
      Result := Next and (ParseHex(V) > 0) else
      Result := (ParseDecimal(V) > 0);
    if Result then C := char(V);
  end;
end;

function TLyCharactor.ParsePString(var S: string): boolean;
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

function TLyCharactor.ParseString(var S: string): boolean;
begin
  if FChar = '''' then
    Result := ParsePString(S) else
  if FChar = '"' then
    Result := ParseCString(S) else
    Result := false;
end;

function TLyCharactor.Pick(Chars: TSysCharSet): string;
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

function TLyCharactor.PickDecimal: string;
begin
  Result := Pick(CS_DIGIT);
end;

function TLyCharactor.PickHex: string;
begin
  Result := Pick(CS_HEX);
end;

function TLyCharactor.PickID: string;
begin
  Result := Pick(CS_ID);
end;

function TLyCharactor.PickExID: string;
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

function TLyCharactor.PickOctal: string;
begin
  Result := Pick(CS_OCTAL);
end;

function TLyCharactor.PeekString(Len: integer): string;
begin
  Result := Copy(FText, FPosition, Len);
end;

function TLyCharactor.Seek(Ch: char): boolean;
begin
  while (FChar <> Ch) and Next do;
  Result := NotEnded;
end;

procedure TLyCharactor.Stop;
begin
  FPosition := FSize + 1;
  FChar := #0;
end;

function TLyCharactor.ParseEscChar(var C: char): boolean;
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
        Result := Next and (ParseHex(V, 2) = 2);
        if Result then C := char(V);
      end
      else
      if FChar = 'u' then // '\uXXXX'
      begin
        Result := Next and (ParseHex(V, 4) = 4);
        if Result then C := char(V);
      end
      else
      if FChar = '0' then // '\0' or '\0oo'
      begin
        Result := (ParseOctal(V, 3) in [1, 3]);
        if Result then C := char(V);
      end
      else
      if CharInSet(FChar, ['1'..'3']) then // '\ooo'
      begin
        Result := (ParseOctal(V, 3) = 3) and (V < 256);
        if Result then C := char(V);
      end
      else
      begin
        C := FChar;  // '\', '''', '"', '?', ...
        Next;
      end;
  end;
end;

function TLyCharactor.SeekLineBreak: boolean;
begin
  while (FChar <> #13) and (FChar <> #10) and Next do;
  Result := NotEnded;
end;

function TLyCharactor.SeekSpace: boolean;
begin
  while (FChar > ' ') and Next do;
  Result := NotEnded;
end;

procedure TLyCharactor.SetText(const Value: string);
begin
  FText := Value;
  FSize := Length(FText);
  First;
end;

function TLyCharactor.Skip(Chars: TSysCharSet): boolean;
begin
  while CharInSet(FChar, Chars) and Next do;
  Result := NotEnded;
end;

function TLyCharactor.Skip(Ch: char): boolean;
begin
  while (FChar = Ch) and Next do;
  Result := NotEnded;
end;

function TLyCharactor.SkipCString(OnQuote: boolean): boolean;
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

function TLyCharactor.SkipLineBreak: boolean;
begin
  while ((FChar = #13) or (FChar = #10)) and Next do;
  Result := NotEnded;
end;

function TLyCharactor.SkipPString(OnQuote: boolean): boolean;
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

function TLyCharactor.SkipSpace: boolean;
begin
  while (FChar <= ' ') and Next do;
  Result := NotEnded;
end;

function TLyCharactor.SkipString(Endc: char): boolean;
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

function TLyCharactor.SkipTag(Endc: char): boolean;
begin
  while Seek(['"', '''', Endc]) and (FChar <> Endc) do SkipString;
  Result := (FChar = Endc);
  if Result then Next;
end;

function TLyCharactor.SkipString: boolean;
begin
  if FChar = '''' then
    Result := Next and SkipPString(false) else
  if FChar = '"' then
    Result := Next and SkipCString(false) else
    Result := true;
end;

end.
