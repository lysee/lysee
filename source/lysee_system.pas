{==============================================================================}
{        UNIT: lysee_system                                                    }
{ DESCRIPTION: lysee's system module                                           }
{   COPYRIGHT: Copyright (c) 2016-2018, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2016/12/17                                                      }
{    MODIFIED: 2020/03/11                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_system;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Classes,  Math, Inifiles, Basic, Lysee;

type

  { TLyTypeType }

  TLyTypeType = class(TLyType)
  private
    procedure MyName(const Param: TLyParam);
    procedure MyModule(const Param: TLyParam);
    procedure MyMethods(const Param: TLyParam);
    procedure MyIsTypeOf(const Param: TLyParam);
    procedure MyIsChildOf(const Param: TLyParam);
    procedure MyIsObject(const Param: TLyParam);
    procedure MyIsNil(const Param: TLyParam);
    procedure MyIsEnum(const Param: TLyParam);
    procedure MyIsEnumSet(const Param: TLyParam);
    procedure MyItemValues(const Param: TLyParam);
    procedure MyPrototype(const Param: TLyParam);
    procedure MyFindMethod(const Param: TLyParam);
  protected
    function AsString(Obj: pointer): string;override;
    procedure Setup;override;
  public
    procedure Convert(Value: TLyValue);override;
  end;

  { TLyErrorType }

  TLyErrorType = class(TLyType)
  private
    procedure MyErrno(const Param: TLyParam);
    procedure MyMsg(const Param: TLyParam);
    procedure MyText(const Param: TLyParam);
    procedure MyModule(const Param: TLyParam);
    procedure MyFileName(const Param: TLyParam);
    procedure MyRow(const Param: TLyParam);
    procedure MyCol(const Param: TLyParam);
    procedure MyExcepted(const Param: TLyParam);
  protected
    function AsString(Obj: pointer): string;override;
    procedure Setup;override;
  end;

  { TLyFuncType }

  TLyFuncType = class(TLyType)
  private
    FParamNamesType: TLyFuncType;
    FParamTypesType: TLyFuncType;
    procedure MyName(const Param: TLyParam);
    procedure MyPrototype(const Param: TLyParam);
    procedure MyParent(const Param: TLyParam);
    procedure MyModule(const Param: TLyParam);
    procedure MyIsMainFunc(const Param: TLyParam);
    procedure MyIsMethod(const Param: TLyParam);
    procedure MyIsConstructor(const Param: TLyParam);
    procedure MyParamCount(const Param: TLyParam);
    procedure MyResultType(const Param: TLyParam);
    procedure MyParamNames_Get(const Param: TLyParam);
    procedure MyParamNames_List(const Param: TLyParam);
    procedure MyParamTypes_Get(const Param: TLyParam);
    procedure MyParamTypes_List(const Param: TLyParam);
    procedure SetupParamNamesType;
    procedure SetupParamTypesType;
  protected
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function AsString(Obj: pointer): string;override;
    procedure Setup;override;
  end;

  { TLyModuleType }

  TLyModuleType = class(TLyType)
  private
    procedure MyName(const Param: TLyParam);
    procedure MyConsts(const Param: TLyParam);
    procedure MyTypes(const Param: TLyParam);
    procedure MyFuncs(const Param: TLyParam);
    procedure MyUsings(const Param: TLyParam);
    procedure MyFind(const Param: TLyParam);
  protected
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function AsString(Obj: pointer): string;override;
    procedure Setup;override;
  end;

  { TLyString }

  TLyString = class(TLyObject)
  private
    FData: string;
  public
    property Data: string read FData write FData;
  end;

  { TLyStringType }

  TLyStringType = class(TLyType)
  private
    procedure MyGet(const Param: TLyParam);
    procedure MySet(const Param: TLyParam);
    procedure MyName(const Param: TLyParam);
    procedure MyValue(const Param: TLyParam);
    procedure MyClone(const Param: TLyParam);
  protected
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function Compare(LValue, RValue:TLyValue): TLyCompare;override;
    function Generate(Obj: pointer): TLyGenerate;override;
    procedure Setup;override;
    { value }
    function AsString(Obj: pointer): string;override;
    function AsChar(Obj: pointer): char;override;
    function AsInteger(Obj: pointer): int64;override;
    function AsFloat(Obj: pointer): double;override;
    function AsCurrency(Obj: pointer): currency;override;
    function AsTime(Obj: pointer): TDateTime;override;
    function AsBoolean(Obj: pointer): boolean;override;
    { list }
    function GetLength(Obj: pointer): int64;override;
    function Has(Obj: pointer; Value: TLyValue): boolean;override;
    { operate }
    procedure ExecLike(LValue, RValue: TLyValue);override;
    procedure ExecMul(LValue, RValue: TLyValue);override;
    procedure ExecAdd(LValue, RValue: TLyValue);override;
    procedure ExecMod(LValue, RValue: TLyValue);override;
  public
    function DefValue: pointer;override;
    procedure Convert(Value: TLyValue);override;
  end;

  { TLyInteger }

  TLyInteger = class(TLyObject)
  private
    FData: int64;
  public
    property Data: int64 read FData;
  end;

  { TLyIntegerType }

  TLyIntegerType = class(TLyBasicType)
  protected
    function Compare(LValue, RValue:TLyValue): TLyCompare;override;
    function Generate(Obj: pointer): TLyGenerate;override;
    { list }
    function Has(Obj: pointer; Value: TLyValue): boolean;override;
    { value }
    function AsInteger(Obj: pointer): int64;override;
    function AsString(Obj: pointer): string;override;
    function AsChar(Obj: pointer): char;override;
    function AsFloat(Obj: pointer): double;override;
    function AsCurrency(Obj: pointer): currency;override;
    function AsTime(Obj: pointer): TDateTime;override;
    function AsBoolean(Obj: pointer): boolean;override;
    { operate }
    procedure ExecNeg(Value: TLyValue);override;
    procedure ExecNot(Value: TLyValue);override;
    procedure ExecShr(LValue, RValue: TLyValue);override;
    procedure ExecShl(LValue, RValue: TLyValue);override;
    procedure ExecXor(LValue, RValue: TLyValue);override;
    procedure ExecMod(LValue, RValue: TLyValue);override;
    procedure ExecDivf(LValue, RValue: TLyValue);override;
    procedure ExecDiv(LValue, RValue: TLyValue);override;
    procedure ExecMul(LValue, RValue: TLyValue);override;
    procedure ExecDec(LValue, RValue: TLyValue);override;
    procedure ExecAdd(LValue, RValue: TLyValue);override;
   public
    function DefValue: pointer;override;
    procedure Convert(Value: TLyValue);override;
  end;

  { TLyBoolean }

  TLyBoolean = class(TLyObject)
  private
    FData: boolean;
  public
    property Data: boolean read FData;
  end;

  { TLyBooleanType }

  TLyBooleanType = class(TLyBasicType)
  protected
    function Compare(LValue, RValue:TLyValue): TLyCompare;override;
    { value }
    function AsBoolean(Obj: pointer): boolean;override;
    function AsString(Obj: pointer): string;override;
    function AsInteger(Obj: pointer): int64;override;
    { operate }
    procedure ExecNot(Value: TLyValue);override;
    procedure ExecXor(LValue, RValue: TLyValue);override;
  public
    function DefValue: pointer;override;
    procedure Convert(Value: TLyValue);override;
  end;

  { TLyChar }

  TLyChar = class(TLyObject)
  private
    FData: char;
  public
    property Data: char read FData;
  end;

  { TLyCharType }

  TLyCharType = class(TLyBasicType)
  protected
    function Compare(LValue, RValue:TLyValue): TLyCompare;override;
    { value }
    function AsChar(Obj: pointer): char;override;
    function AsString(Obj: pointer): string;override;
    function AsInteger(Obj: pointer): int64;override;
    function AsFloat(Obj: pointer): double;override;
    function AsCurrency(Obj: pointer): currency;override;
    function AsBoolean(Obj: pointer): boolean;override;
    { operate }
    procedure ExecLike(LValue, RValue: TLyValue);override;
    procedure ExecMul(LValue, RValue: TLyValue);override;
    procedure ExecAdd(LValue, RValue: TLyValue);override;
  public
    function DefValue: pointer;override;
    procedure Convert(Value: TLyValue);override;
  end;

  { TLyFloat }

  TLyFloat = class(TLyObject)
  private
    FData: double;
  public
    property Data: double read FData;
  end;

  { TLyFloatType }

  TLyFloatType = class(TLyBasicType)
  protected
    function Compare(LValue, RValue:TLyValue): TLyCompare;override;
    { value }
    function AsFloat(Obj: pointer): double;override;
    function AsString(Obj: pointer): string;override;
    function AsInteger(Obj: pointer): int64;override;
    function AsCurrency(Obj: pointer): currency;override;
    function AsTime(Obj: pointer): TDateTime;override;
    function AsBoolean(Obj: pointer): boolean;override;
    { operate }
    procedure ExecNeg(Value: TLyValue);override;
    procedure ExecDivf(LValue, RValue: TLyValue);override;
    procedure ExecMul(LValue, RValue: TLyValue);override;
    procedure ExecDec(LValue, RValue: TLyValue);override;
    procedure ExecAdd(LValue, RValue: TLyValue);override;
  public
    function DefValue: pointer;override;
    procedure Convert(Value: TLyValue);override;
  end;

  { TLyCurrency }

  TLyCurrency = class(TLyObject)
  private
    FData: currency;
  public
    property Data: currency read FData;
  end;

  { TLyCurrencyType }

  TLyCurrencyType = class(TLyBasicType)
  protected
    function Compare(LValue, RValue:TLyValue): TLyCompare;override;
    { value }
    function AsCurrency(Obj: pointer): currency;override;
    function AsString(Obj: pointer): string;override;
    function AsInteger(Obj: pointer): int64;override;
    function AsFloat(Obj: pointer): double;override;
    function AsTime(Obj: pointer): TDateTime;override;
    function AsBoolean(Obj: pointer): boolean;override;
    { operate }
    procedure ExecNeg(Value: TLyValue);override;
    procedure ExecDivf(LValue, RValue: TLyValue);override;
    procedure ExecMul(LValue, RValue: TLyValue);override;
    procedure ExecDec(LValue, RValue: TLyValue);override;
    procedure ExecAdd(LValue, RValue: TLyValue);override;
  public
    function DefValue: pointer;override;
    procedure Convert(Value: TLyValue);override;
  end;

  { TLyTime }

  TLyTime = class(TLyObject)
  private
    FData: TDateTime;
  public
    property Data: TDateTime read FData;
  end;

  { TLyTimeType }

  TLyTimeType = class(TLyBasicType)
  protected
    function Compare(LValue, RValue:TLyValue): TLyCompare;override;
    { value }
    function AsTime(Obj: pointer): TDateTime;override;
    function AsString(Obj: pointer): string;override;
    function AsInteger(Obj: pointer): int64;override;
    function AsFloat(Obj: pointer): double;override;
    function AsCurrency(Obj: pointer): currency;override;
    function AsBoolean(Obj: pointer): boolean;override;
    { operate }
    procedure ExecDec(LValue, RValue: TLyValue);override;
    procedure ExecAdd(LValue, RValue: TLyValue);override;
  public
    function DefValue: pointer;override;
    procedure Convert(Value: TLyValue);override;
  end;

  { TLyArrayType }

  TLyArrayType = class(TLyType)
  private
    procedure MyIsEmpty(const Param: TLyParam);
    procedure MyIndexOf(const Param: TLyParam);
    procedure MyIndexOfName(const Param: TLyParam);
    procedure MyCopy(const Param: TLyParam);
    procedure MyLeft(const Param: TLyParam);
    procedure MyRight(const Param: TLyParam);
    procedure MyGetCount(const Param: TLyParam);
    procedure MyGet(const Param: TLyParam);
    procedure MyGetName(const Param: TLyParam);
  protected
    function InstanceClass: TClass;override;
    function CreateInstance: pointer;override;
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function Generate(Obj: pointer): TLyGenerate;override;
    procedure Setup;override;
    procedure MarkForSurvive(Obj: pointer);override;
    { value }
    function AsString(Obj: pointer): string;override;
    { list }
    function GetLength(Obj: pointer): int64;override;
    function Clear(Obj: pointer): boolean;override;
    function Add(Obj: pointer; Value: TLyValue): integer;override;
    function Has(Obj: pointer; Value: TLyValue): boolean;override;
    { operate }
    procedure ExecMul(LValue, RValue: TLyValue);override;
    procedure ExecDec(LValue, RValue: TLyValue);override;
    procedure ExecAdd(LValue, RValue: TLyValue);override;
  end;

  { TLyListType }

  TLyListType = class(TLyArrayType)
  private
    procedure MyClear(const Param: TLyParam);
    procedure MyDelete(const Param: TLyParam);
    procedure MyRemove(const Param: TLyParam);
    procedure MyExchange(const Param: TLyParam);
    procedure MyMove(const Param: TLyParam);
    procedure MySort(const Param: TLyParam);
    procedure MyInsert(const Param: TLyParam);
    procedure MyAdd(const Param: TLyParam);
    procedure MyAssign(const Param: TLyParam);
    procedure MySetCount(const Param: TLyParam);
    procedure MySet(const Param: TLyParam);
  protected
    procedure Setup;override;
  end;

  { TLyHashItem }

  TLyHashItem = class(TLyValue)
  private
    FKey: string;
    FNext: TLyHashItem;
  public
    function Format: string;
    property Key: string read FKey;
  end;

  { TLyHash }

  TLyEachHashItem = procedure(Item: TLyHashItem) of object;

  TLyHash = class(TLyGarbage)
  private
    FBuckets: array of TLyHashItem;
    FCount: integer;
    FFormating: boolean;
    FCaseSensitive: boolean;
    procedure FreeItem(H: TLyHashItem);
  protected
    function BucketCount: integer;
    function MatchName(const N1, N2: string): boolean;
    function HashIndex(const Name: string): integer;
  public
    constructor Create;override;
    destructor Destroy;override;
    function ToString: string;override;
    procedure MarkForSurvive;override;
    procedure Clear;override;
    procedure Resize(NewSize: integer);
    procedure Each(AProc: TLyEachHashItem);
    procedure ListKeys(List: TLyList);
    procedure ListValues(List: TLyList);
    function Add(const Name: string): TLyHashItem;
    function Get(const Name: string): TLyHashItem;
    function GetValue(const Name: string; Value: TLyValue): boolean;
    function Has(const Name: string): boolean;
    function Remove(const Name: string): boolean;
    function IsEmpty: boolean;
    function SetupItemList: TList;
    function DefConst(const Name, Value: string): TLyHashItem;overload;
    function DefConst(const Name: string; Value: int64): TLyHashItem;overload;
    function DefConst(const Name: string; Value: double): TLyHashItem;overload;
    function DefConst(const Name: string; Value: boolean): TLyHashItem;overload;
    property Count: integer read FCount;
    property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive;
  end;

  { TLyHashType }

  TLyHashType = class(TLyType)
  private
    procedure MyIsEmpty(const Param: TLyParam);
    procedure MyClear(const Param: TLyParam);
    procedure MyHas(const Param: TLyParam);
    procedure MyRemove(const Param: TLyParam);
    procedure MyKeys(const Param: TLyParam);
    procedure MyValues(const Param: TLyParam);
    procedure MyGet(const Param: TLyParam);
    procedure MySet(const Param: TLyParam);
  protected
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    procedure Setup;override;
    { list }
    procedure MarkForSurvive(Obj: pointer);override;
    function Clear(Obj: pointer): boolean;override;
    function Has(Obj: pointer; Value: TLyValue): boolean;override;
    { value }
    function AsString(Obj: pointer): string;override;
  end;

  { TLySearchRec }

  TLySearchRec = class(TLyObject)
  private
    FSR: TSearchRec;
    FOK: boolean;
    FPath: string;
    function GetName: string;
    function GetSize: int64;
    function GetAttr: integer;
    function GetPath: string;
    function GetFileName: string;
  public
    destructor Destroy;override;
    function FindFirst(const Path: string; Attr: integer): boolean;
    function FindNext: boolean;
    procedure FindClose;
    function Active: boolean;
    function IsFile: boolean;
    function IsDirectory: boolean;
    property Path: string read GetPath;
    property Name: string read GetName;
    property FileName: string read GetFileName;
    property Size: int64 read GetSize;
    property Attr: integer read GetAttr;
  end;

  { TLySearchRecGenerate }

  TLySearchRecGenerate = class(TLyGenerate)
  private
    FSR: TLySearchRec;
  public
    constructor CreateWith(const SR: TLySearchRec);
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLySearchRecType }

  TLySearchRecType = class(TLyType)
  private
    procedure MyFindFirst(const Param: TLyParam);
    procedure MyFindNext(const Param: TLyParam);
    procedure MyFindClose(const Param: TLyParam);
    procedure MyActive(const Param: TLyParam);
    procedure MyIsFile(const Param: TLyParam);
    procedure MyIsDirectory(const Param: TLyParam);
    procedure MyGetPath(const Param: TLyParam);
    procedure MyGetName(const Param: TLyParam);
    procedure MyGetFileName(const Param: TLyParam);
    procedure MyGetSize(const Param: TLyParam);
    procedure MyGetAttr(const Param: TLyParam);
  protected
    function InstanceClass: TClass;override;
    function CreateInstance: pointer;override;
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function Generate(Obj: pointer): TLyGenerate;override;
    procedure Setup;override;
    { value }
    function AsString(Obj: pointer): string;override;
    function AsBoolean(Obj: pointer): boolean;override;
  end;

  { TLyStringsGenerate }

  TLyStringsGenerate = class(TLyGenerate)
  private
    FL: TStrings;
    FIndex: integer;
  public
    constructor CreateIn(const List: TStrings);
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLyStringsType }

  TLyStringsType = class(TLyType)
  private
    FNamesType: TLyStringsType;
    FValuesType: TLyStringsType;
    FValueFromIndexType: TLyStringsType;
    procedure MyIsEmpty(const Param: TLyParam);
    procedure MyLoadFromFile(const Param: TLyParam);
    procedure MySaveToFile(const Param: TLyParam);
    procedure MyBeginUpdate(const Param: TLyParam);
    procedure MyEndUpdate(const Param: TLyParam);
    procedure MyAssign(const Param: TLyParam);
    procedure MyClear(const Param: TLyParam);
    procedure MyDelete(const Param: TLyParam);
    procedure MyDeleteLast(const Param: TLyParam);
    procedure MyDeleteEmptyLines(const Param: TLyParam);
    procedure MyRemove(const Param: TLyParam);
    procedure MyRemoveByName(const Param: TLyParam);
    procedure MyAdd(const Param: TLyParam);
    procedure MyInsert(const Param: TLyParam);
    procedure MyExchange(const Param: TLyParam);
    procedure MyMove(const Param: TLyParam);
    procedure MyPack(const Param: TLyParam);
    procedure MyIndexOf(const Param: TLyParam);
    procedure MyIndexOfObject(const Param: TLyParam);
    procedure MyIndexOfName(const Param: TLyParam);
    procedure MyEquals(const Param: TLyParam);
    procedure MyTrim(const Param: TLyParam);
    procedure MyTrimAll(const Param: TLyParam);
    procedure MyTrimLeft(const Param: TLyParam);
    procedure MyTrimRight(const Param: TLyParam);
    procedure MyCopy(const Param: TLyParam);
    procedure MyLeft(const Param: TLyParam);
    procedure MyRight(const Param: TLyParam);
    procedure MySelectMatchedLines(const Param: TLyParam);
    procedure MyDeleteMatchedLines(const Param: TLyParam);
    procedure MyReverse(const Param: TLyParam);
    procedure MyGetCount(const Param: TLyParam);
    procedure MySetCount(const Param: TLyParam);
    procedure MyGet(const Param: TLyParam);
    procedure MySet(const Param: TLyParam);
    procedure MyNames_Get(const Param: TLyParam);
    procedure MyNames_List(const Param: TLyParam);
    procedure MyValues_Get(const Param: TLyParam);
    procedure MyValues_Set(const Param: TLyParam);
    procedure MyValueFromIndex_Get(const Param: TLyParam);
    procedure MyGetText(const Param: TLyParam);
    procedure MySetText(const Param: TLyParam);
    procedure MyGetCommaText(const Param: TLyParam);
    procedure MySetCommaText(const Param: TLyParam);
    procedure MyGetDelimiter(const Param: TLyParam);
    procedure MySetDelimiter(const Param: TLyParam);
    procedure MyGetDelimitedText(const Param: TLyParam);
    procedure MySetDelimitedText(const Param: TLyParam);
    procedure MyGetLineBreak(const Param: TLyParam);
    procedure MySetLineBreak(const Param: TLyParam);
    procedure MyGetNameValueSeparator(const Param: TLyParam);
    procedure MySetNameValueSeparator(const Param: TLyParam);
    procedure MyGetQuoteChar(const Param: TLyParam);
    procedure MySetQuoteChar(const Param: TLyParam);
    procedure MyGetStrictDelimiter(const Param: TLyParam);
    procedure MySetStrictDelimiter(const Param: TLyParam);
    procedure MyGetWriteBOM(const Param: TLyParam);
    procedure MySetWriteBOM(const Param: TLyParam);
    procedure MyGetFirst(const Param: TLyParam);
    procedure MySetFirst(const Param: TLyParam);
    procedure MyGetLast(const Param: TLyParam);
    procedure MySetLast(const Param: TLyParam);
    procedure MyUnique(const Param: TLyParam);
    procedure MySort(const Param: TLyParam);
    procedure MyGetSorted(const Param: TLyParam);
    procedure MySetSorted(const Param: TLyParam);
    procedure MyGetCaseSensitive(const Param: TLyParam);
    procedure MySetCaseSensitive(const Param: TLyParam);
    procedure SetupNamesType;
    procedure SetupValuesType;
    procedure SetupValueFromIndexType;
  protected
    function InstanceClass: TClass;override;
    function CreateInstance: pointer;override;
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function Generate(Obj: pointer): TLyGenerate;override;
    procedure Setup;override;
    { list }
    function GetLength(Obj: pointer): int64;override;
    function Clear(Obj: pointer): boolean;override;
    function Add(Obj: pointer; Value: TLyValue): integer;override;
    { value }
    function AsString(Obj: pointer): string;override;
  end;

  { TLyStringListType }

  TLyStringListType = class(TLyStringsType)
  protected
    procedure Setup;override;
  public
    procedure Convert(Value: TLyValue);override;
  end;

  { TLyIniFile }

  TLyIniFile = class(TLyObject)
  private
    FFile: TIniFile;
  public
    destructor Destroy;override;
    procedure Open(const FileName: string);
  end;

  { TLyIniFileType }

  TLyIniFileType = class(TLyType)
  private
    procedure MySectionExists(const Param: TLyParam);
    procedure MyReadString(const Param: TLyParam);
    procedure MyReadInteger(const Param: TLyParam);
    procedure MyReadBool(const Param: TLyParam);
    procedure MyReadFloat(const Param: TLyParam);
    procedure MyReadDate(const Param: TLyParam);
    procedure MyReadDateTime(const Param: TLyParam);
    procedure MyReadTime(const Param: TLyParam);
    procedure MyWriteString(const Param: TLyParam);
    procedure MyWriteInteger(const Param: TLyParam);
    procedure MyWriteBool(const Param: TLyParam);
    procedure MyWriteFloat(const Param: TLyParam);
    procedure MyWriteDate(const Param: TLyParam);
    procedure MyWriteDateTime(const Param: TLyParam);
    procedure MyWriteTime(const Param: TLyParam);
    procedure MyReadSection(const Param: TLyParam);
    procedure MyReadSections(const Param: TLyParam);
    procedure MyReadSectionValues(const Param: TLyParam);
    procedure MyEraseSection(const Param: TLyParam);
    procedure MyDeleteKey(const Param: TLyParam);
    procedure MyUpdateFile(const Param: TLyParam);
    procedure MyValueExists(const Param: TLyParam);
    procedure MyGetFileName(const Param: TLyParam);
    procedure MyOpenFile(const Param: TLyParam);
  protected
    function InstanceClass: TClass;override;
    function CreateInstance: pointer;override;
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    procedure Setup;override;
    procedure MyCreate(const Param: TLyParam);override;
    function AsString(Obj: pointer): string;override;
  end;

var
  my_list: TLyListType;
  my_hash: TLyHashType;
  my_searchrec: TLySearchRecType;
  my_replace_flag: TLyEnumType;
  my_replace_flags: TLyEnumSetType;
  my_strings: TLyStringsType;
  my_strlist: TLyStringListType;
  my_inifile: TLyIniFileType;

procedure Setup;

function GetIntegerData(Value: int64): TLyInteger;
function GetBooleanData(Value: boolean): TLyBoolean;
function GetCharData(Value: char): TLyChar;
function GetFloatData(Value: double): TLyFloat;
function GetCurrencyData(Value: currency): TLyCurrency;
function GetTimeData(Value: TDateTime): TLyTime;
function GetStrings(V: TLyValue): TLyStrings;
function GetSearchRec(V: TLyValue): TLySearchRec;
function GetReplaceFlags(V: TLyValue): TReplaceFlags;
procedure SetReplaceFlags(V: TLyValue; Value: TReplaceFlags);

{ system }

procedure pp_Randomize(const Param: TLyParam);
procedure pp_Random(const Param: TLyParam);
procedure pp_Now(const Param: TLyParam);
procedure pp_GetEnvironmentStrings(const Param: TLyParam);
procedure pp_IsLeapYear(const Param: TLyParam);
procedure pp_Sleep(const Param: TLyParam);
procedure pp_GetEnvironmentVariable(const Param: TLyParam);
procedure pp_Ord(const Param: TLyParam);

{ string }

procedure pp_Format(const Param: TLyParam);
procedure pp_Trim(const Param: TLyParam);
procedure pp_TrimLeft(const Param: TLyParam);
procedure pp_TrimRight(const Param: TLyParam);
procedure pp_TrimAll(const Param: TLyParam);
procedure pp_LowerCase(const Param: TLyParam);
procedure pp_UpperCase(const Param: TLyParam);
procedure pp_Pos(const Param: TLyParam);
procedure pp_Lines(const Param: TLyParam);
procedure pp_Chars(const Param: TLyParam);
procedure pp_Delete(const Param: TLyParam);
procedure pp_Insert(const Param: TLyParam);
procedure pp_Chr(const Param: TLyParam);
procedure pp_GenGUID(const Param: TLyParam);
procedure pp_GenID(const Param: TLyParam);
procedure pp_IntToStr(const Param: TLyParam);
procedure pp_StrToInt(const Param: TLyParam);
procedure pp_IntToHex(const Param: TLyParam);
procedure pp_HexToInt(const Param: TLyParam);
procedure pp_CompareText(const Param: TLyParam);
procedure pp_SameText(const Param: TLyParam);
procedure pp_CompareStr(const Param: TLyParam);
procedure pp_SameStr(const Param: TLyParam);
procedure pp_StringReplace(const Param: TLyParam);
procedure pp_IsIdent(const Param: TLyParam);
procedure pp_IsAlpha(const Param: TLyParam);
procedure pp_IsAlnum(const Param: TLyParam);
procedure pp_IsCntrl(const Param: TLyParam);
procedure pp_IsDigit(const Param: TLyParam);
procedure pp_IsSpace(const Param: TLyParam);
procedure pp_IsHex(const Param: TLyParam);
procedure pp_IsLowerCase(const Param: TLyParam);
procedure pp_IsUpperCase(const Param: TLyParam);
procedure pp_SaveTextToFile(const Param: TLyParam);

{ math }

procedure pp_Abs(const Param: TLyParam);
procedure pp_Round(const Param: TLyParam);
procedure pp_Trunc(const Param: TLyParam);
procedure pp_Ceil(const Param: TLyParam);
procedure pp_Floor(const Param: TLyParam);
procedure pp_Max(const Param: TLyParam);
procedure pp_Min(const Param: TLyParam);

{ file/path }

procedure pp_FindFirst(const Param: TLyParam);
procedure pp_FindNext(const Param: TLyParam);
procedure pp_FindClose(const Param: TLyParam);
procedure pp_IncludeTrailingBackslash(const Param: TLyParam);
procedure pp_IncludeTrailingPathDelimiter(const Param: TLyParam);
procedure pp_ExcludeTrailingBackslash(const Param: TLyParam);
procedure pp_ExcludeTrailingPathDelimiter(const Param: TLyParam);
procedure pp_SetPathDelimiter(const Param: TLyParam);
procedure pp_DeleteFile(const Param: TLyParam);
procedure pp_RenameFile(const Param: TLyParam);
procedure pp_CopyFile(const Param: TLyParam);
procedure pp_RemoveDir(const Param: TLyParam);
procedure pp_CreateDir(const Param: TLyParam);
procedure pp_GetCurrentDir(const Param: TLyParam);
procedure pp_SetCurrentDir(const Param: TLyParam);
procedure pp_FileExists(const Param: TLyParam);
procedure pp_DirectoryExists(const Param: TLyParam);
procedure pp_ForceDirectories(const Param: TLyParam);
procedure pp_ChangeFileExt(const Param: TLyParam);
procedure pp_ExtractFilePath(const Param: TLyParam);
procedure pp_ExtractFileDir(const Param: TLyParam);
procedure pp_ExtractFileName(const Param: TLyParam);
procedure pp_ExtractFileExt(const Param: TLyParam);
procedure pp_ExpandFileName(const Param: TLyParam);
procedure pp_ExtractRelativePath(const Param: TLyParam);
{$IFNDEF FPC}
procedure pp_ChangeFilePath(const Param: TLyParam);
procedure pp_GetHomePath(const Param: TLyParam);
procedure pp_IsRelativePath(const Param: TLyParam);
{$ENDIF}
procedure pp_GetTempFileName(const Param: TLyParam);

{ ym }

procedure pp_GetYm(const Param: TLyParam);
procedure pp_GetYmFrom(const Param: TLyParam);
procedure pp_IsYm(const Param: TLyParam);
procedure pp_IsYmOf(const Param: TLyParam);
procedure pp_IsYmIn(const Param: TLyParam);
procedure pp_IsYmStr(const Param: TLyParam);
procedure pp_YmToStr(const Param: TLyParam);
procedure pp_YmToStrOf(const Param: TLyParam);
procedure pp_StrToYm(const Param: TLyParam);
procedure pp_PrevYm(const Param: TLyParam);
procedure pp_PrevYmStr(const Param: TLyParam);
procedure pp_NextYm(const Param: TLyParam);
procedure pp_NextYmStr(const Param: TLyParam);

{ ymd }

procedure pp_GetYmd(const Param: TLyParam);
procedure pp_GetYmdFrom(const Param: TLyParam);
procedure pp_IsYmd(const Param: TLyParam);
procedure pp_IsYmdOf(const Param: TLyParam);
procedure pp_IsYmdStr(const Param: TLyParam);
procedure pp_YmdToStr(const Param: TLyParam);
procedure pp_YmdToStrOf(const Param: TLyParam);
procedure pp_StrToYmd(const Param: TLyParam);
procedure pp_YmdToDate(const Param: TLyParam);
procedure pp_NextYmd(const Param: TLyParam);
procedure pp_NextYmdStr(const Param: TLyParam);
procedure pp_PrevYmd(const Param: TLyParam);
procedure pp_PrevYmdStr(const Param: TLyParam);

{ datetime }

procedure pp_EncodeDate(const Param: TLyParam);
procedure pp_EncodeTime(const Param: TLyParam);
procedure pp_DayOfWeek(const Param: TLyParam);
procedure pp_Date(const Param: TLyParam);
procedure pp_Time(const Param: TLyParam);
procedure pp_IncMonth(const Param: TLyParam);
procedure pp_DateToStr(const Param: TLyParam);
procedure pp_TimeToStr(const Param: TLyParam);
procedure pp_DateTimeToStr(const Param: TLyParam);
procedure pp_StrToDate(const Param: TLyParam);
procedure pp_StrToTime(const Param: TLyParam);
procedure pp_StrToDateTime(const Param: TLyParam);
procedure pp_FormatDateTime(const Param: TLyParam);
procedure pp_DateTimeToFileDate(const Param: TLyParam);
procedure pp_FileDateToDateTime(const Param: TLyParam);
procedure pp_StrToDateDef(const Param: TLyParam);
procedure pp_StrToTimeDef(const Param: TLyParam);
procedure pp_StrToDateTimeDef(const Param: TLyParam);
procedure pp_CurrentYear(const Param: TLyParam);
procedure pp_DateOf(const Param: TLyParam);
procedure pp_TimeOf(const Param: TLyParam);
procedure pp_IsInLeapYear(const Param: TLyParam);
procedure pp_IsPM(const Param: TLyParam);
procedure pp_IsValidDate(const Param: TLyParam);
procedure pp_IsValidTime(const Param: TLyParam);
procedure pp_IsValidDateTime(const Param: TLyParam);
procedure pp_IsValidDateDay(const Param: TLyParam);
procedure pp_IsValidDateWeek(const Param: TLyParam);
procedure pp_IsValidDateMonthWeek(const Param: TLyParam);
procedure pp_WeeksInYear(const Param: TLyParam);
procedure pp_WeeksInAYear(const Param: TLyParam);
procedure pp_DaysInYear(const Param: TLyParam);
procedure pp_DaysInAYear(const Param: TLyParam);
procedure pp_DaysInMonth(const Param: TLyParam);
procedure pp_DaysInAMonth(const Param: TLyParam);
procedure pp_Today(const Param: TLyParam);
procedure pp_Yesterday(const Param: TLyParam);
procedure pp_Tomorrow(const Param: TLyParam);
procedure pp_IsToday(const Param: TLyParam);
procedure pp_IsSameDay(const Param: TLyParam);
procedure pp_YearOf(const Param: TLyParam);
procedure pp_MonthOf(const Param: TLyParam);
procedure pp_WeekOf(const Param: TLyParam);
procedure pp_DayOf(const Param: TLyParam);
procedure pp_HourOf(const Param: TLyParam);
procedure pp_MinuteOf(const Param: TLyParam);
procedure pp_SecondOf(const Param: TLyParam);
procedure pp_MilliSecondOf(const Param: TLyParam);
procedure pp_StartOfTheYear(const Param: TLyParam);
procedure pp_EndOfTheYear(const Param: TLyParam);
procedure pp_StartOfAYear(const Param: TLyParam);
procedure pp_EndOfAYear(const Param: TLyParam);
procedure pp_StartOfTheMonth(const Param: TLyParam);
procedure pp_EndOfTheMonth(const Param: TLyParam);
procedure pp_StartOfAMonth(const Param: TLyParam);
procedure pp_EndOfAMonth(const Param: TLyParam);
procedure pp_StartOfTheWeek(const Param: TLyParam);
procedure pp_EndOfTheWeek(const Param: TLyParam);
procedure pp_StartOfAWeek(const Param: TLyParam);
procedure pp_EndOfAWeek(const Param: TLyParam);
procedure pp_StartOfTheDay(const Param: TLyParam);
procedure pp_EndOfTheDay(const Param: TLyParam);
procedure pp_StartOfADay(const Param: TLyParam);
procedure pp_EndOfADay(const Param: TLyParam);
procedure pp_MonthOfTheYear(const Param: TLyParam);
procedure pp_WeekOfTheYear(const Param: TLyParam);
procedure pp_DayOfTheYear(const Param: TLyParam);
procedure pp_HourOfTheYear(const Param: TLyParam);
procedure pp_MinuteOfTheYear(const Param: TLyParam);
procedure pp_SecondOfTheYear(const Param: TLyParam);
procedure pp_MilliSecondOfTheYear(const Param: TLyParam);
procedure pp_WeekOfTheMonth(const Param: TLyParam);
procedure pp_DayOfTheMonth(const Param: TLyParam);
procedure pp_HourOfTheMonth(const Param: TLyParam);
procedure pp_MinuteOfTheMonth(const Param: TLyParam);
procedure pp_SecondOfTheMonth(const Param: TLyParam);
procedure pp_MilliSecondOfTheMonth(const Param: TLyParam);
procedure pp_DayOfTheWeek(const Param: TLyParam);
procedure pp_HourOfTheWeek(const Param: TLyParam);
procedure pp_MinuteOfTheWeek(const Param: TLyParam);
procedure pp_SecondOfTheWeek(const Param: TLyParam);
procedure pp_MilliSecondOfTheWeek(const Param: TLyParam);
procedure pp_HourOfTheDay(const Param: TLyParam);
procedure pp_MinuteOfTheDay(const Param: TLyParam);
procedure pp_SecondOfTheDay(const Param: TLyParam);
procedure pp_MilliSecondOfTheDay(const Param: TLyParam);
procedure pp_MinuteOfTheHour(const Param: TLyParam);
procedure pp_SecondOfTheHour(const Param: TLyParam);
procedure pp_MilliSecondOfTheHour(const Param: TLyParam);
procedure pp_SecondOfTheMinute(const Param: TLyParam);
procedure pp_MilliSecondOfTheMinute(const Param: TLyParam);
procedure pp_MilliSecondOfTheSecond(const Param: TLyParam);
procedure pp_WithinPastYears(const Param: TLyParam);
procedure pp_WithinPastMonths(const Param: TLyParam);
procedure pp_WithinPastWeeks(const Param: TLyParam);
procedure pp_WithinPastDays(const Param: TLyParam);
procedure pp_WithinPastHours(const Param: TLyParam);
procedure pp_WithinPastMinutes(const Param: TLyParam);
procedure pp_WithinPastSeconds(const Param: TLyParam);
procedure pp_WithinPastMilliSeconds(const Param: TLyParam);
procedure pp_YearsBetween(const Param: TLyParam);
procedure pp_MonthsBetween(const Param: TLyParam);
procedure pp_WeeksBetween(const Param: TLyParam);
procedure pp_DaysBetween(const Param: TLyParam);
procedure pp_HoursBetween(const Param: TLyParam);
procedure pp_MinutesBetween(const Param: TLyParam);
procedure pp_SecondsBetween(const Param: TLyParam);
procedure pp_MilliSecondsBetween(const Param: TLyParam);
procedure pp_YearSpan(const Param: TLyParam);
procedure pp_MonthSpan(const Param: TLyParam);
procedure pp_WeekSpan(const Param: TLyParam);
procedure pp_DaySpan(const Param: TLyParam);
procedure pp_HourSpan(const Param: TLyParam);
procedure pp_MinuteSpan(const Param: TLyParam);
procedure pp_SecondSpan(const Param: TLyParam);
procedure pp_MilliSecondSpan(const Param: TLyParam);
procedure pp_IncYear(const Param: TLyParam);
procedure pp_IncWeek(const Param: TLyParam);
procedure pp_IncDay(const Param: TLyParam);
procedure pp_IncHour(const Param: TLyParam);
procedure pp_IncMinute(const Param: TLyParam);
procedure pp_IncSecond(const Param: TLyParam);
procedure pp_IncMilliSecond(const Param: TLyParam);
procedure pp_EncodeDateTime(const Param: TLyParam);
procedure pp_EncodeDateWeek(const Param: TLyParam);
procedure pp_EncodeDateDay(const Param: TLyParam);
procedure pp_EncodeDateMonthWeek(const Param: TLyParam);
procedure pp_RecodeYear(const Param: TLyParam);
procedure pp_RecodeMonth(const Param: TLyParam);
procedure pp_RecodeDay(const Param: TLyParam);
procedure pp_RecodeHour(const Param: TLyParam);
procedure pp_RecodeMinute(const Param: TLyParam);
procedure pp_RecodeSecond(const Param: TLyParam);
procedure pp_RecodeMilliSecond(const Param: TLyParam);
procedure pp_RecodeDate(const Param: TLyParam);
procedure pp_RecodeTime(const Param: TLyParam);
procedure pp_RecodeDateTime(const Param: TLyParam);
procedure pp_CompareDateTime(const Param: TLyParam);
procedure pp_CompareDate(const Param: TLyParam);
procedure pp_CompareTime(const Param: TLyParam);
procedure pp_SameDateTime(const Param: TLyParam);
procedure pp_SameDate(const Param: TLyParam);
procedure pp_SameTime(const Param: TLyParam);
procedure pp_NthDayOfWeek(const Param: TLyParam);
procedure pp_EncodeDayOfWeekInMonth(const Param: TLyParam);
procedure pp_InvalidDateTimeError(const Param: TLyParam);
procedure pp_InvalidDateWeekError(const Param: TLyParam);
procedure pp_InvalidDateDayError(const Param: TLyParam);
procedure pp_InvalidDateMonthWeekError(const Param: TLyParam);
procedure pp_InvalidDayOfWeekInMonthError(const Param: TLyParam);
procedure pp_DateTimeToJulianDate(const Param: TLyParam);
procedure pp_JulianDateToDateTime(const Param: TLyParam);
procedure pp_DateTimeToModifiedJulianDate(const Param: TLyParam);
procedure pp_ModifiedJulianDateToDateTime(const Param: TLyParam);
procedure pp_DateTimeToUnix(const Param: TLyParam);
procedure pp_UnixToDateTime(const Param: TLyParam);

{ SUM }

procedure pp_MD5SumFile(const Param: TLyParam);
procedure pp_MD5SumStr(const Param: TLyParam);
procedure pp_SHA1SumFile(const Param: TLyParam);
procedure pp_SHA1SumStr(const Param: TLyParam);
{$IFNDEF FPC}
procedure pp_SHA2SumFile(const Param: TLyParam);
procedure pp_SHA2SumStr(const Param: TLyParam);
{$ENDIF}

implementation

uses
  {$IFNDEF FPC}Hash,{$ENDIF}
  DateUtils;

const

  LSE_HASH_DELTA = 64;

var

  my_integer_zero: TLyInteger;
  my_boolean_datas: array[boolean] of TLyBoolean;
  my_char_datas: array[#0..#255] of TLyChar;
  my_float_zero: TLyFloat;
  my_currency_zero: TLyCurrency;
  my_time_zero: TLyTime;

procedure Setup;
begin
  if (my_list <> nil) or (my_system = nil) then Exit;

  my_list := TLyListType.Create('TList', my_system);
  my_hash := TLyHashType.Create('Hashed', my_system);
  my_searchrec := TLySearchRecType.Create('TSearchRec', my_system);
  my_replace_flag := TLyEnumType.Create('TReplaceFlag', my_system);
  my_replace_flag.AddItems(['rfReplaceAll', 'rfIgnoreCase']);
  my_replace_flags := my_replace_flag.NewEnumSetType('TReplaceFlags');
  my_strings := TLyStringsType.Create('TStrings', my_system);
  my_strlist := TLyStringListType.Create('TStringList', my_system);
  my_inifile := TLyIniFileType.Create('TIniFile', my_system);

  with my_system do
  begin
    { constants }

    {$IFNDEF FPC}
    Define('INVALID_HANDLE_VALUE', INVALID_HANDLE_VALUE);
    {$ENDIF}

    Define('fmOpenRead', fmOpenRead);
    Define('fmOpenWrite', fmOpenWrite);
    Define('fmOpenReadWrite', fmOpenReadWrite);
    Define('fmShareCompat', fmShareCompat);
    Define('fmShareExclusive', fmShareExclusive);
    Define('fmShareDenyWrite', fmShareDenyWrite);
    Define('fmShareDenyRead', fmShareDenyRead);
    Define('fmShareDenyNone', fmShareDenyNone);

    { system }

    AddFunc('Randomize', {$IFDEF FPC}@{$ENDIF}pp_Randomize);
    AddFunc('Random', my_int, ['_Range'], [my_int],
            {$IFDEF FPC}@{$ENDIF}pp_Random);
    AddFunc('Now', my_time,
            {$IFDEF FPC}@{$ENDIF}pp_Now);
    AddFunc('GetEnvironmentStrings', my_string,
            {$IFDEF FPC}@{$ENDIF}pp_GetEnvironmentStrings);
    AddFunc('IsLeapYear', my_bool, ['Year'], [my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IsLeapYear);
    AddFunc('Sleep', ['MilliSeconds'], [my_int],
            {$IFDEF FPC}@{$ENDIF}pp_Sleep);
    AddFunc('GetEnvironmentVariable', my_string,
            ['Name'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_GetEnvironmentVariable);
    AddFunc('Ord', my_int, ['Any'], [my_variant],
            {$IFDEF FPC}@{$ENDIF}pp_Ord);

    { string }

    AddFunc('Format', my_string,
            ['Fmt', 'Args'], [my_string, my_array],
            {$IFDEF FPC}@{$ENDIF}pp_Format);
    AddFunc('Trim', my_string, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_Trim);
    AddFunc('TrimLeft', my_string, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_TrimLeft);
    AddFunc('TrimRight', my_string, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_TrimRight);
    AddFunc('TrimAll', my_string, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_TrimAll);
    AddFunc('LowerCase', my_string, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_LowerCase);
    AddFunc('UpperCase', my_string, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_UpperCase);
    AddFunc('Pos', my_int, ['SubStr', 'Str'], [my_string, my_string],
            {$IFDEF FPC}@{$ENDIF}pp_Pos);
    AddFunc('Lines', my_strings, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_Lines);
    AddFunc('Chars', my_array, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_Chars);
    AddFunc('Delete', ['VarStr', 'Index', '_Count'],
            [my_string, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_Delete);
    AddFunc('Insert', ['SubStr', 'VarStr', 'Index'],
            [my_string, my_string, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_Insert);
    AddFunc('Chr', my_char, ['Value'], [my_variant],
            {$IFDEF FPC}@{$ENDIF}pp_Chr);
    AddFunc('GenGUID', my_string,
            {$IFDEF FPC}@{$ENDIF}pp_GenGUID);
    AddFunc('GenID', my_string,
            {$IFDEF FPC}@{$ENDIF}pp_GenID);
    AddFunc('IntToStr', my_string, ['Value'], [my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IntToStr);
    AddFunc('StrToInt', my_int, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_StrToInt);
    AddFunc('IntToHex', my_string,
            ['Value', '_Digits'], [my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IntToHex);
    AddFunc('HexToInt', my_int, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_HexToInt);
    AddFunc('CompareText', my_int,
            ['S1', 'S2'], [my_string, my_string],
            {$IFDEF FPC}@{$ENDIF}pp_CompareText);
    AddFunc('SameText', my_bool,
            ['S1', 'S2'], [my_string, my_string],
            {$IFDEF FPC}@{$ENDIF}pp_SameText);
    AddFunc('CompareStr', my_int,
            ['S1', 'S2'], [my_string, my_string],
            {$IFDEF FPC}@{$ENDIF}pp_CompareStr);
    AddFunc('SameStr', my_bool,
            ['S1', 'S2'], [my_string, my_string],
            {$IFDEF FPC}@{$ENDIF}pp_SameStr);
    AddFunc('StringReplace', my_string,
            ['S', 'Patten', '_NewStr', '_ReplaceFlags'],
            [my_string, my_string, my_string, my_replace_flags],
            {$IFDEF FPC}@{$ENDIF}pp_StringReplace);
    AddFunc('IsIdent', my_bool, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_IsIdent);
    AddFunc('IsAlpha', my_bool, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_IsAlpha);
    AddFunc('IsAlnum', my_bool, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_IsAlnum);
    AddFunc('IsCntrl', my_bool, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_IsCntrl);
    AddFunc('IsSpace', my_bool, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_IsSpace);
    AddFunc('IsDigit', my_bool, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_IsDigit);
    AddFunc('IsHex', my_bool, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_IsHex);
    AddFunc('IsLowerCase', my_bool, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_IsLowerCase);
    AddFunc('IsUpperCase', my_bool, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_IsUpperCase);
    AddFunc('SaveTextToFile', ['S', 'FileName'],
            [my_string, my_string],
            {$IFDEF FPC}@{$ENDIF}pp_SaveTextToFile);

    { math }

    AddFunc('Abs', my_variant, ['Any'], [my_variant],
            {$IFDEF FPC}@{$ENDIF}pp_Abs);
    AddFunc('Round', my_int, ['X'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_Round);
    AddFunc('Trunc', my_int, ['X'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_Trunc);
    AddFunc('Ceil', my_int, ['X'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_Ceil);
    AddFunc('Floor', my_int, ['X'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_Floor);
    AddFunc('Max', my_variant, ['V1', 'V2'],
            [my_variant, my_variant],
            {$IFDEF FPC}@{$ENDIF}pp_Max);
    AddFunc('Min', my_variant, ['V1', 'V2'],
            [my_variant, my_variant],
            {$IFDEF FPC}@{$ENDIF}pp_Min);

    { file/path }

    AddFunc('FindFirst', my_int, ['Path', 'Attr', 'SearchRec'],
            [my_string, my_int, my_searchrec],
            {$IFDEF FPC}@{$ENDIF}pp_FindFirst);
    AddFunc('FindNext', my_int, ['SearchRec'], [my_searchrec],
            {$IFDEF FPC}@{$ENDIF}pp_FindNext);
    AddFunc('FindClose', ['SearchRec'], [my_searchrec],
            {$IFDEF FPC}@{$ENDIF}pp_FindClose);
    AddFunc('IncludeTrailingBackslash', my_string, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_IncludeTrailingBackslash);
    AddFunc('IncludeTrailingPathDelimiter', my_string, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_IncludeTrailingPathDelimiter);
    AddFunc('ExcludeTrailingBackslash', my_string, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_ExcludeTrailingBackslash);
    AddFunc('ExcludeTrailingPathDelimiter', my_string, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_ExcludeTrailingPathDelimiter);
    AddFunc('SetPathDelimiter', my_string, ['FileName'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_SetPathDelimiter);
    AddFunc('DeleteFile', my_bool, ['FileName'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_DeleteFile);
    AddFunc('RenameFile', my_bool, ['OldName', 'NewName'],
            [my_string, my_string],
            {$IFDEF FPC}@{$ENDIF}pp_RenameFile);
    AddFunc('CopyFile', my_bool,
            ['SourceFile', 'DestiFile', '_FailIfDestiFileExists'],
            [my_string, my_string, my_bool],
            {$IFDEF FPC}@{$ENDIF}pp_CopyFile);
    AddFunc('RemoveDir', my_bool, ['Dir'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_RemoveDir);
    AddFunc('CreateDir', my_bool, ['Dir'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_CreateDir);
    AddFunc('GetCurrentDir', my_string,
            {$IFDEF FPC}@{$ENDIF}pp_GetCurrentDir);
    AddFunc('SetCurrentDir', my_bool, ['Dir'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_SetCurrentDir);
    AddFunc('FileExists', my_bool, ['FileName'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_FileExists);
    AddFunc('DirectoryExists', my_bool, ['Directory'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_DirectoryExists);
    AddFunc('ForceDirectories', my_bool, ['Dir'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_ForceDirectories);
    AddFunc('ChangeFileExt', my_string,
            ['FileName', 'Extension'], [my_string, my_string],
            {$IFDEF FPC}@{$ENDIF}pp_ChangeFileExt);
    AddFunc('ExtractFilePath', my_string, ['FileName'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_ExtractFilePath);
    AddFunc('ExtractFileDir', my_string, ['FileName'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_ExtractFileDir);
    AddFunc('ExtractFileName', my_string, ['FileName'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_ExtractFileName);
    AddFunc('ExtractFileExt', my_string, ['FileName'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_ExtractFileExt);
    AddFunc('ExpandFileName', my_string, ['FileName'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_ExpandFileName);
    AddFunc('ExtractRelativePath', my_string,
            ['BaseName', 'DestName'], [my_string, my_string],
            {$IFDEF FPC}@{$ENDIF}pp_ExtractRelativePath);
    {$IFNDEF FPC}
    AddFunc('ChangeFilePath', my_string,
            ['FileName', 'Path'], [my_string, my_string],
            {$IFDEF FPC}@{$ENDIF}pp_ChangeFilePath);
    AddFunc('GetHomePath', my_string,
            {$IFDEF FPC}@{$ENDIF}pp_GetHomePath);
    AddFunc('IsRelativePath', my_bool, ['Path'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_IsRelativePath);
    {$ENDIF}
    AddFunc('GetTempFileName', my_string, ['_FileExt'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_GetTempFileName);

    { ym }

    AddFunc('GetYm', my_int,
            {$IFDEF FPC}@{$ENDIF}pp_GetYm);
    AddFunc('GetYmFrom', my_int, ['Date'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_GetYmFrom);
    AddFunc('IsYm', my_bool, ['Ym'], [my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IsYm);
    AddFunc('IsYmOf', my_bool, ['Y', 'M'], [my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IsYmOf);
    AddFunc('IsYmIn', my_bool,
            ['Ym', 'MinYM', 'MaxYM'], [my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IsYmIn);
    AddFunc('IsYmStr', my_bool, ['Ym'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_IsYmStr);
    AddFunc('YmToStr', my_string, ['Ym'], [my_int],
            {$IFDEF FPC}@{$ENDIF}pp_YmToStr);
    AddFunc('YmToStrOf', my_string,
            ['Ym', 'Delimiter'], [my_int, my_string],
            {$IFDEF FPC}@{$ENDIF}pp_YmToStrOf);
    AddFunc('StrToYm', my_int, ['S', '_DefValue'], [my_string, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_StrToYm);
    AddFunc('PrevYm', my_int, ['Ym', '_Offset'], [my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_PrevYm);
    AddFunc('PrevYmStr', my_string, ['Ym'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_PrevYmStr);
    AddFunc('NextYm', my_int, ['Ym', '_Offset'], [my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_NextYm);
    AddFunc('NextYmStr', my_string, ['Ym'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_NextYmStr);

    { ymd }

    AddFunc('GetYmd', my_int,
            {$IFDEF FPC}@{$ENDIF}pp_GetYmd);
    AddFunc('GetYmdFrom', my_int, ['Date'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_GetYmdFrom);
    AddFunc('IsYmd', my_bool, ['Ymd'], [my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IsYmd);
    AddFunc('IsYmdOf', my_bool,
            ['Y', 'M', 'D'], [my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IsYmdOf);
    AddFunc('IsYmdStr', my_bool, ['Ymd'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_IsYmdStr);
    AddFunc('YmdToStr', my_string, ['Ymd'], [my_int],
            {$IFDEF FPC}@{$ENDIF}pp_YmdToStr);
    AddFunc('YmdToStrOf', my_string,
            ['Ymd', 'Delimiter'], [my_int, my_string],
            {$IFDEF FPC}@{$ENDIF}pp_YmdToStrOf);
    AddFunc('StrToYmd', my_int,
            ['S', '_DefValue'], [my_string, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_StrToYmd);
    AddFunc('YmdToDate', my_time, ['Ymd'], [my_int],
            {$IFDEF FPC}@{$ENDIF}pp_YmdToDate);
    AddFunc('NextYmd', my_int, ['Ymd', '_Offset'], [my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_NextYmd);
    AddFunc('NextYmdStr', my_string, ['Ymd'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_NextYmdStr);
    AddFunc('PrevYmd', my_int, ['Ymd', '_Offset'], [my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_PrevYmd);
    AddFunc('PrevYmdStr', my_string, ['Ymd'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_PrevYmdStr);

    { datetime }

    AddFunc('EncodeDate', my_time,
            ['Year', 'Month', 'Day'], [my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_EncodeDate);
    AddFunc('EncodeTime', my_time,
            ['Hour', 'Minute', 'Second', 'MilliSecond'],
            [my_int, my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_EncodeTime);
    AddFunc('DayOfWeek', my_int, ['DateTime'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_DayOfWeek);
    AddFunc('Date', my_time,
            {$IFDEF FPC}@{$ENDIF}pp_Date);
    AddFunc('Time', my_time,
            {$IFDEF FPC}@{$ENDIF}pp_Time);
    AddFunc('IncMonth', my_time,
            ['DateTime', 'NumberOfMonths'], [my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IncMonth);
    AddFunc('DateToStr', my_string, ['Date'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_DateToStr);
    AddFunc('TimeToStr', my_string, ['Time'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_TimeToStr);
    AddFunc('DateTimeToStr', my_string, ['DateTime'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_DateTimeToStr);
    AddFunc('StrToDate', my_time, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_StrToDate);
    AddFunc('StrToTime', my_time, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_StrToTime);
    AddFunc('StrToDateTime', my_time, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_StrToDateTime);
    AddFunc('FormatDateTime', my_string,
            ['FormatStr', 'DateTime'], [my_string, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_FormatDateTime);
    AddFunc('DateTimeToFileDate', my_int, ['DateTime'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_DateTimeToFileDate);
    AddFunc('FileDateToDateTime', my_time, ['Filedate'], [my_int],
            {$IFDEF FPC}@{$ENDIF}pp_FileDateToDateTime);
    AddFunc('StrToDateDef', my_time, ['S', 'Defvalue'], [my_string, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_StrToDateDef);
    AddFunc('StrToTimeDef', my_time, ['S', 'Defvalue'], [my_string, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_StrToTimeDef);
    AddFunc('StrToDateTimeDef', my_time,
            ['S', 'Defvalue'], [my_string, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_StrToDateTimeDef);
    AddFunc('CurrentYear', my_int,
            {$IFDEF FPC}@{$ENDIF}pp_CurrentYear);
    AddFunc('DateOf', my_time, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_DateOf);
    AddFunc('TimeOf', my_time, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_TimeOf);
    AddFunc('IsInLeapYear', my_bool, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_IsInLeapYear);
    AddFunc('IsPM', my_bool, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_IsPM);
    AddFunc('IsValidDate', my_bool,
            ['AYear', 'AMonth', 'ADay'], [my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IsValidDate);
    AddFunc('IsValidTime', my_bool,
            ['AHour', 'AMinute', 'ASecond', 'AMilliSecond'],
            [my_int, my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IsValidTime);
    AddFunc('IsValidDateTime', my_bool,
            ['AYear', 'AMonth', 'ADay', 'AHour', 'AMinute', 'ASecond', 'AMilliSecond'],
            [my_int, my_int, my_int, my_int, my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IsValidDateTime);
    AddFunc('IsValidDateDay', my_bool,
            ['AYear', 'ADayOfYear'], [my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IsValidDateDay);
    AddFunc('IsValidDateWeek', my_bool,
            ['AYear', 'AWeekOfYear', 'ADayOfWeek'], [my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IsValidDateWeek);
    AddFunc('IsValidDateMonthWeek', my_bool,
            ['AYear', 'AMonth', 'AWeekOfMonth', 'ADayOfWeek'],
            [my_int, my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IsValidDateMonthWeek);
    AddFunc('WeeksInYear', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_WeeksInYear);
    AddFunc('WeeksInAYear', my_int, ['AYear'], [my_int],
            {$IFDEF FPC}@{$ENDIF}pp_WeeksInAYear);
    AddFunc('DaysInYear', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_DaysInYear);
    AddFunc('DaysInAYear', my_int, ['AYear'], [my_int],
            {$IFDEF FPC}@{$ENDIF}pp_DaysInAYear);
    AddFunc('DaysInMonth', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_DaysInMonth);
    AddFunc('DaysInAMonth', my_int, ['AYear', 'AMonth'], [my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_DaysInAMonth);
    AddFunc('Today', my_time, {$IFDEF FPC}@{$ENDIF}pp_Today);
    AddFunc('Yesterday', my_time, {$IFDEF FPC}@{$ENDIF}pp_Yesterday);
    AddFunc('Tomorrow', my_time, {$IFDEF FPC}@{$ENDIF}pp_Tomorrow);
    AddFunc('IsToday', my_bool, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_IsToday);
    AddFunc('IsSameDay', my_bool, ['AValue', 'ABasis'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_IsSameDay);
    AddFunc('YearOf', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_YearOf);
    AddFunc('MonthOf', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MonthOf);
    AddFunc('WeekOf', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_WeekOf);
    AddFunc('DayOf', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_DayOf);
    AddFunc('HourOf', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_HourOf);
    AddFunc('MinuteOf', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MinuteOf);
    AddFunc('SecondOf', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_SecondOf);
    AddFunc('MilliSecondOf', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MilliSecondOf);
    AddFunc('StartOfTheYear', my_time, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_StartOfTheYear);
    AddFunc('EndOfTheYear', my_time, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_EndOfTheYear);
    AddFunc('StartOfAYear', my_time, ['AYear'], [my_int],
            {$IFDEF FPC}@{$ENDIF}pp_StartOfAYear);
    AddFunc('EndOfAYear', my_time, ['AYear'], [my_int],
            {$IFDEF FPC}@{$ENDIF}pp_EndOfAYear);
    AddFunc('StartOfTheMonth', my_time, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_StartOfTheMonth);
    AddFunc('EndOfTheMonth', my_time, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_EndOfTheMonth);
    AddFunc('StartOfAMonth', my_time, ['AYear', 'AMonth'], [my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_StartOfAMonth);
    AddFunc('EndOfAMonth', my_time, ['AYear', 'AMonth'], [my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_EndOfAMonth);
    AddFunc('StartOfTheWeek', my_time, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_StartOfTheWeek);
    AddFunc('EndOfTheWeek', my_time, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_EndOfTheWeek);
    AddFunc('StartOfAWeek', my_time,
            ['AYear', 'AWeekOfYear', 'ADayOfWeek'], [my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_StartOfAWeek);
    AddFunc('EndOfAWeek', my_time,
            ['AYear', 'AWeekOfYear', 'ADayOfWeek'], [my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_EndOfAWeek);
    AddFunc('StartOfTheDay', my_time, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_StartOfTheDay);
    AddFunc('EndOfTheDay', my_time, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_EndOfTheDay);
    AddFunc('StartOfADay', my_time,
            ['AYear', 'AMonth', 'ADay'], [my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_StartOfADay);
    AddFunc('EndOfADay', my_time,
            ['AYear', 'AMonth', 'ADay'], [my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_EndOfADay);
    AddFunc('MonthOfTheYear', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MonthOfTheYear);
    AddFunc('WeekOfTheYear', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_WeekOfTheYear);
    AddFunc('DayOfTheYear', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_DayOfTheYear);
    AddFunc('HourOfTheYear', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_HourOfTheYear);
    AddFunc('MinuteOfTheYear', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MinuteOfTheYear);
    AddFunc('SecondOfTheYear', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_SecondOfTheYear);
    AddFunc('MilliSecondOfTheYear', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MilliSecondOfTheYear);
    AddFunc('WeekOfTheMonth', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_WeekOfTheMonth);
    AddFunc('DayOfTheMonth', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_DayOfTheMonth);
    AddFunc('HourOfTheMonth', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_HourOfTheMonth);
    AddFunc('MinuteOfTheMonth', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MinuteOfTheMonth);
    AddFunc('SecondOfTheMonth', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_SecondOfTheMonth);
    AddFunc('MilliSecondOfTheMonth', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MilliSecondOfTheMonth);
    AddFunc('DayOfTheWeek', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_DayOfTheWeek);
    AddFunc('HourOfTheWeek', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_HourOfTheWeek);
    AddFunc('MinuteOfTheWeek', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MinuteOfTheWeek);
    AddFunc('SecondOfTheWeek', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_SecondOfTheWeek);
    AddFunc('MilliSecondOfTheWeek', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MilliSecondOfTheWeek);
    AddFunc('HourOfTheDay', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_HourOfTheDay);
    AddFunc('MinuteOfTheDay', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MinuteOfTheDay);
    AddFunc('SecondOfTheDay', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_SecondOfTheDay);
    AddFunc('MilliSecondOfTheDay', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MilliSecondOfTheDay);
    AddFunc('MinuteOfTheHour', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MinuteOfTheHour);
    AddFunc('SecondOfTheHour', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_SecondOfTheHour);
    AddFunc('MilliSecondOfTheHour', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MilliSecondOfTheHour);
    AddFunc('SecondOfTheMinute', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_SecondOfTheMinute);
    AddFunc('MilliSecondOfTheMinute', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MilliSecondOfTheMinute);
    AddFunc('MilliSecondOfTheSecond', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MilliSecondOfTheSecond);
    AddFunc('WithinPastYears', my_bool,
            ['ANow', 'AThen', 'AYears'], [my_time, my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_WithinPastYears);
    AddFunc('WithinPastMonths', my_bool,
            ['ANow', 'AThen', 'AMonths'], [my_time, my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_WithinPastMonths);
    AddFunc('WithinPastWeeks', my_bool,
            ['ANow', 'AThen', 'AWeeks'], [my_time, my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_WithinPastWeeks);
    AddFunc('WithinPastDays', my_bool,
            ['ANow', 'AThen', 'ADays'], [my_time, my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_WithinPastDays);
    AddFunc('WithinPastHours', my_bool,
            ['ANow', 'AThen', 'AHours'], [my_time, my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_WithinPastHours);
    AddFunc('WithinPastMinutes', my_bool,
            ['ANow', 'AThen', 'AMinutes'], [my_time, my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_WithinPastMinutes);
    AddFunc('WithinPastSeconds', my_bool,
            ['ANow', 'AThen', 'ASeconds'], [my_time, my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_WithinPastSeconds);
    AddFunc('WithinPastMilliSeconds', my_bool,
            ['ANow', 'AThen', 'AMilliSeconds'], [my_time, my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_WithinPastMilliSeconds);
    AddFunc('YearsBetween', my_int,
            ['ANow', 'AThen'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_YearsBetween);
    AddFunc('MonthsBetween', my_int, ['ANow', 'AThen'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MonthsBetween);
    AddFunc('WeeksBetween', my_int, ['ANow', 'AThen'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_WeeksBetween);
    AddFunc('DaysBetween', my_int, ['ANow', 'AThen'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_DaysBetween);
    AddFunc('HoursBetween', my_int, ['ANow', 'AThen'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_HoursBetween);
    AddFunc('MinutesBetween', my_int, ['ANow', 'AThen'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MinutesBetween);
    AddFunc('SecondsBetween', my_int, ['ANow', 'AThen'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_SecondsBetween);
    AddFunc('MilliSecondsBetween', my_int, ['ANow', 'AThen'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MilliSecondsBetween);
    AddFunc('YearSpan', my_float, ['ANow', 'AThen'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_YearSpan);
    AddFunc('MonthSpan', my_float, ['ANow', 'AThen'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MonthSpan);
    AddFunc('WeekSpan', my_float, ['ANow', 'AThen'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_WeekSpan);
    AddFunc('DaySpan', my_float, ['ANow', 'AThen'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_DaySpan);
    AddFunc('HourSpan', my_float, ['ANow', 'AThen'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_HourSpan);
    AddFunc('MinuteSpan', my_float, ['ANow', 'AThen'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MinuteSpan);
    AddFunc('SecondSpan', my_float, ['ANow', 'AThen'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_SecondSpan);
    AddFunc('MilliSecondSpan', my_float, ['ANow', 'AThen'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_MilliSecondSpan);
    AddFunc('IncYear', my_time, ['AValue', 'ANumberOfYears'], [my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IncYear);
    AddFunc('IncWeek', my_time, ['AValue', 'ANumberOfWeeks'], [my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IncWeek);
    AddFunc('IncDay', my_time, ['AValue', 'ANumberOfDays'], [my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IncDay);
    AddFunc('IncHour', my_time, ['AValue', 'ANumberOfHours'], [my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IncHour);
    AddFunc('IncMinute', my_time, ['AValue', 'ANumberOfMinutes'], [my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IncMinute);
    AddFunc('IncSecond', my_time, ['AValue', 'ANumberOfSeconds'], [my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IncSecond);
    AddFunc('IncMilliSecond', my_time,
            ['AValue', 'ANumberOfMilliSeconds'], [my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_IncMilliSecond);
    AddFunc('EncodeDateTime', my_time,
            ['AYear', 'AMonth', 'ADay', 'AHour', 'AMinute', 'ASecond', 'AMilliSecond'],
            [my_int, my_int, my_int, my_int, my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_EncodeDateTime);
    AddFunc('EncodeDateWeek', my_time,
            ['AYear', 'AWeekOfYear', 'ADayOfWeek'], [my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_EncodeDateWeek);
    AddFunc('EncodeDateDay', my_time,
            ['AYear', 'ADayOfYear'], [my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_EncodeDateDay);
    AddFunc('EncodeDateMonthWeek', my_time,
            ['AYear', 'AMonth', 'AWeekOfMonth', 'ADayOfWeek'],
            [my_int, my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_EncodeDateMonthWeek);
    AddFunc('RecodeYear', my_time, ['AValue', 'AYear'], [my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_RecodeYear);
    AddFunc('RecodeMonth', my_time, ['AValue', 'AMonth'], [my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_RecodeMonth);
    AddFunc('RecodeDay', my_time, ['AValue', 'ADay'], [my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_RecodeDay);
    AddFunc('RecodeHour', my_time, ['AValue', 'AHour'], [my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_RecodeHour);
    AddFunc('RecodeMinute', my_time, ['AValue', 'AMinute'], [my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_RecodeMinute);
    AddFunc('RecodeSecond', my_time, ['AValue', 'ASecond'], [my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_RecodeSecond);
    AddFunc('RecodeMilliSecond', my_time,
            ['AValue', 'AMilliSecond'], [my_time, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_RecodeMilliSecond);
    AddFunc('RecodeDate', my_time,
            ['AValue', 'AYear', 'AMonth', 'ADay'], [my_time, my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_RecodeDate);
    AddFunc('RecodeTime', my_time,
            ['AValue', 'AHour', 'AMinute', 'ASecond', 'AMilliSecond'],
            [my_time, my_int, my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_RecodeTime);
    AddFunc('RecodeDateTime', my_time,
            ['AValue', 'AYear', 'AMonth', 'ADay', 'AHour', 'AMinute', 'ASecond', 'AMilliSecond'],
            [my_time, my_int, my_int, my_int, my_int, my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_RecodeDateTime);
    AddFunc('CompareDateTime', my_int, ['A', 'B'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_CompareDateTime);
    AddFunc('CompareDate', my_int, ['A', 'B'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_CompareDate);
    AddFunc('CompareTime', my_int, ['A', 'B'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_CompareTime);
    AddFunc('SameDateTime', my_bool, ['A', 'B'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_SameDateTime);
    AddFunc('SameDate', my_bool, ['A', 'B'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_SameDate);
    AddFunc('SameTime', my_bool, ['A', 'B'], [my_time, my_time],
            {$IFDEF FPC}@{$ENDIF}pp_SameTime);
    AddFunc('NthDayOfWeek', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_NthDayOfWeek);
    AddFunc('EncodeDayOfWeekInMonth', my_time,
            ['AYear', 'AMonth', 'ANthDayOfWeek', 'ADayOfWeek'],
            [my_int, my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_EncodeDayOfWeekInMonth);
    AddFunc('InvalidDateTimeError', my_nil,
            ['AYear', 'AMonth', 'ADay', 'AHour', 'AMinute', 'ASecond', 'AMilliSecond'],
            [my_int, my_int, my_int, my_int, my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_InvalidDateTimeError);
    AddFunc('InvalidDateWeekError', my_nil,
            ['AYear', 'AWeekOfYear', 'ADayOfWeek'], [my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_InvalidDateWeekError);
    AddFunc('InvalidDateDayError', my_nil,
            ['AYear', 'ADayOfYear'], [my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_InvalidDateDayError);
    AddFunc('InvalidDateMonthWeekError', my_nil,
            ['AYear', 'AMonth', 'AWeekOfMonth', 'ADayOfWeek'],
            [my_int, my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_InvalidDateMonthWeekError);
    AddFunc('InvalidDayOfWeekInMonthError', my_nil,
            ['AYear', 'AMonth', 'ANthDayOfWeek', 'ADayOfWeek'],
            [my_int, my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_InvalidDayOfWeekInMonthError);
    AddFunc('DateTimeToJulianDate', my_float, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_DateTimeToJulianDate);
    AddFunc('JulianDateToDateTime', my_time, ['AValue'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_JulianDateToDateTime);
    AddFunc('DateTimeToModifiedJulianDate', my_float, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_DateTimeToModifiedJulianDate);
    AddFunc('ModifiedJulianDateToDateTime', my_time, ['AValue'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_ModifiedJulianDateToDateTime);
    AddFunc('DateTimeToUnix', my_int, ['AValue'], [my_time],
            {$IFDEF FPC}@{$ENDIF}pp_DateTimeToUnix);
    AddFunc('UnixToDateTime', my_time, ['AValue'], [my_int],
            {$IFDEF FPC}@{$ENDIF}pp_UnixToDateTime);

    { SUM }

    AddFunc('MD5SumFile', my_string, ['FileName'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_MD5SumFile);
    AddFunc('MD5SumStr', my_string, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_MD5SumStr);

    AddFunc('SHA1SumFile', my_string, ['FileName'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_SHA1SumFile);
    AddFunc('SHA1SumStr', my_string, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_SHA1SumStr);

    {$IFNDEF FPC}
    AddFunc('SHA2SumFile', my_string, ['FileName'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_SHA2SumFile);
    AddFunc('SHA2SumStr', my_string, ['S'], [my_string],
            {$IFDEF FPC}@{$ENDIF}pp_SHA2SumStr);
    {$ENDIF}
  end;
end;

function GetIntegerData(Value: int64): TLyInteger;
begin
  if Value <> 0 then
  begin
    Result := TLyInteger.Create;
    Result.FData := Value;
  end
  else Result := my_integer_zero;
end;

function GetBooleanData(Value: boolean): TLyBoolean;
begin
  Result := my_boolean_datas[Value];
end;

function GetCharData(Value: char): TLyChar;
begin
  if Value = #0 then Result := my_char_datas[#0] else
  if Value <= #255 then
  begin
    Result := my_char_datas[Value];
    if Result = nil then
    begin
      Result := TLyChar.Create;
      Result.FData := Value;
      Result.IncRefcount;
      my_char_datas[Value] := Result;
    end;
  end
  else
  begin
    Result := TLyChar.Create;
    Result.FData := Value;
  end;
end;

function GetFloatData(Value: double): TLyFloat;
begin
  Result := TLyFloat.Create;
  Result.FData := Value;
end;

function GetCurrencyData(Value: currency): TLyCurrency;
begin
  Result := TLyCurrency.Create;
  Result.FData := Value;
end;

function GetTimeData(Value: TDateTime): TLyTime;
begin
  if not IsZero(Value) then
  begin
    Result := TLyTime.Create;
    Result.FData := Value;
  end
  else Result := my_time_zero;
end;

function GetStrings(V: TLyValue): TLyStrings;
begin
  if V.VType = my_strings then
    Result := TLyStrings(V.Data) else
    Result := nil;
end;

function GetSearchRec(V: TLyValue): TLySearchRec;
begin
  if V.VType = my_searchrec then
    Result := TLySearchRec(V.Data) else
    Result := nil;
end;

function GetReplaceFlags(V: TLyValue): TReplaceFlags;
var
  S: TLyEnumSet;
begin
  V.Convert(my_replace_flags);
  S := V.AsEnumSet;
  Result := [];
  if S[Ord(rfReplaceAll)] then Include(Result, rfReplaceAll);
  if S[Ord(rfIgnoreCase)] then Include(Result, rfIgnoreCase);
end;

procedure SetReplaceFlags(V: TLyValue; Value: TReplaceFlags);
var
  S: TLyEnumSet;
begin
  S := my_replace_flags.NewEnumSet;
  V.AsEnumSet := S;
  S[Ord(rfReplaceAll)] := rfReplaceAll in Value;
  S[Ord(rfIgnoreCase)] := rfIgnoreCase in Value;
end;

{ system }

// procedure Randomize
procedure pp_Randomize(const Param: TLyParam);
begin
  Randomize;
end;

// function Random(Range): integer
procedure pp_Random(const Param: TLyParam);
begin
  if Param.Prmc = 1 then
    Param.Result.AsInteger := Random(Max(Param[0].AsInteger, 2)) else
    Param.Result.AsInteger := Random(324288328);
end;

// function Now: time
procedure pp_Now(const Param: TLyParam);
begin
  Param.Result.AsTime := Now;
end;

// function GetEnvironmentStrings: string
procedure pp_GetEnvironmentStrings(const Param: TLyParam);
var
  L: TStrings;
  {$IFDEF FPC}
  I, N: integer;
  {$ELSE}
  H, P: PChar;
  {$ENDIF}
begin
  L := TStringList.Create;
  try
    {$IFDEF FPC}
    N := SysUtils.GetEnvironmentVariableCount;
    for I := 1 to N do
      L.Add(SysUtils.GetEnvironmentString(I));
    {$ELSE}
    P := GetEnvironmentStrings;
    H := P;
    if H <> nil then
      while H^ <> #0 do
      begin
        L.Add(H);
        H := H + StrLen(H) + 1;
      end;
    FreeEnvironmentStrings(P);
    {$ENDIF}
    Param.Result.AsString := L.Text;
  finally
    L.Free;
  end;
end;

// function IsLeapYear(Year): boolean
procedure pp_IsLeapYear(const Param: TLyParam);
begin
  Param.Result.AsBoolean := IsLeapYear(Param[0].AsInteger);
end;

procedure pp_Sleep(const Param: TLyParam);
begin
  SysUtils.Sleep(Param[0].AsInteger);
end;

procedure pp_GetEnvironmentVariable(const Param: TLyParam);
begin
  Param.Result.AsString := SysUtils.GetEnvironmentVariable(Param[0].AsString);
end;

// function Ord(Value): integer
procedure pp_Ord(const Param: TLyParam);
begin
  if Param[0].VType = my_char then
    Param.Result.AsInteger := Ord(Param[0].AsChar) else
    Param.Result.AsInteger := Param[0].AsInteger;
end;

{ string }

// function Format(Patten, [...]): string
procedure pp_Format(const Param: TLyParam);
begin
  Param.Result.AsString := TLyList(Param[1].Data).Format(Param[0].AsString);
end;

// function Trim(S): string
procedure pp_Trim(const Param: TLyParam);
var
  S: string;
begin
  S := Param[0].AsString;
  Param.Result.AsString := Trim(S);
end;

// function TrimLeft(S): string
procedure pp_TrimLeft(const Param: TLyParam);
var
  S: string;
begin
  S := Param[0].AsString;
  Param.Result.AsString := TrimLeft(S);
end;

// function TrimRight(S): string
procedure pp_TrimRight(const Param: TLyParam);
var
  S: string;
begin
  S := Param[0].AsString;
  Param.Result.AsString := TrimRight(S);
end;

// function TrimAll(S): string
procedure pp_TrimAll(const Param: TLyParam);
var
  S: string;
begin
  S := Param[0].AsString;
  Param.Result.AsString := TrimAll(S);
end;

// function LowerCase(S): string
procedure pp_LowerCase(const Param: TLyParam);
var
  S: string;
begin
  S := Param[0].AsString;
  Param.Result.AsString := LowerCase(S);
end;

// function UpperCase(S): string
procedure pp_UpperCase(const Param: TLyParam);
var
  S: string;
begin
  S := Param[0].AsString;
  Param.Result.AsString := UpperCase(S);
end;

// function Pos(SubStr, Str): integer
procedure pp_Pos(const Param: TLyParam);
var
  Sub, Str: string;
begin
  Sub := Param[0].AsString;
  Str := Param[1].AsString;
  Param.Result.AsInteger := System.Pos(Sub, Str);
end;

// function Lines(S): array
procedure pp_Lines(const Param: TLyParam);
var
  A: TLyStrings;
begin
  A := TLyStrings.Create;
  Param.Result.Assign(my_strings, A);
  A.Strings.Text := Param[0].AsString;
end;

// function Chars(S): array
procedure pp_Chars(const Param: TLyParam);
var
  A: TLyList;
  S: string;
  I: integer;
begin
  A := TLyList.Create;
  Param.Result.Assign(my_array, A);
  S := Param[0].AsString;
  for I := 1 to Length(S) do
    A.Add.AsChar := S[I];
end;

// procedure Delete(VarStr, Index, _Count)
procedure pp_Delete(const Param: TLyParam);
var
  V: TLyValue;
  T: TLyType;
  S: string;
  N: int64;
begin
  V := Param.GetVarbValue(0, T);
  if V <> nil then
  begin
    S := V.AsString;
    if Param.Prmc > 2 then
      N := Max(0, Param[2].AsInteger) else
      N := 1;
    if N > 0 then
    begin
      System.Delete(S, Param[1].AsInteger, N);
      V.AsString := S;
    end;
  end
  else Param.Error('variable not specified');
end;

// procedure Insert(SubStr, VarStr, Index)
procedure pp_Insert(const Param: TLyParam);
var
  V: TLyValue;
  T: TLyType;
  S: string;
begin
  V := Param.GetVarbValue(1, T);
  if V <> nil then
  begin
    S := V.AsString;
    System.Insert(Param[0].AsString, S, Param[2].AsInteger);
    V.AsString := S;
  end
  else Param.Error('variable not specified');
end;

// function Chr(Value): char
procedure pp_Chr(const Param: TLyParam);
begin
  Param.Result.AsChar := Param[0].AsChar;
end;

procedure pp_GenGUID(const Param: TLyParam);
var
  G: TGuid;
begin
  CreateGuid(G);
  Param.Result.AsString := GuidToString(G);
end;

// function GenID: string
procedure pp_GenID(const Param: TLyParam);
begin
  Param.Result.AsString := GenID;
end;

procedure pp_IntToStr(const Param: TLyParam);
begin
  Param.Result.AsString := IntToStr(Param[0].AsInteger);
end;

procedure pp_StrToInt(const Param: TLyParam);
begin
  Param.Result.AsInteger := StrToInt64(Param[0].AsString);
end;

procedure pp_IntToHex(const Param: TLyParam);
begin
  Param.Result.AsString := IntToHex(Param[0].AsInteger, Param[1].AsInteger);
end;

procedure pp_HexToInt(const Param: TLyParam);
begin
  Param.Result.AsInteger := StrToInt64('$' + Param[0].AsString);
end;

procedure pp_CompareText(const Param: TLyParam);
begin
  Param.Result.AsInteger := SysUtils.CompareText(Param[0].AsString, Param[1].AsString);
end;

procedure pp_SameText(const Param: TLyParam);
begin
  Param.Result.AsBoolean := SysUtils.SameText(Param[0].AsString, Param[1].AsString);
end;

procedure pp_CompareStr(const Param: TLyParam);
begin
  Param.Result.AsInteger := SysUtils.CompareStr(Param[0].AsString, Param[1].AsString);
end;

procedure pp_SameStr(const Param: TLyParam);
begin
  Param.Result.AsBoolean := (Param[0].AsString = Param[1].AsString);
end;

// function StringReplace(S, Patten, NewPatten, Flags): string
procedure pp_StringReplace(const Param: TLyParam);
var
  S, P, N: string;
  flags: TReplaceFlags;
begin
  S := Param[0].AsString; // string
  P := Param[1].AsString; // patten
  N := Param[2].AsString; // new string
  if Param.Prmc > 3 then
    flags := GetReplaceFlags(Param[3]) else
    flags := [rfReplaceAll];
  Param.Result.AsString := StringReplace(S, P, N, flags);
end;

procedure pp_IsIdent(const Param: TLyParam);
begin
  Param.Result.AsBoolean := IsID(Param[0].AsString);
end;

procedure pp_IsAlpha(const Param: TLyParam);
begin
  Param.Result.AsBoolean := InChars(Param[0].AsString, CS_ALPHA);
end;

procedure pp_IsAlnum(const Param: TLyParam);
begin
  Param.Result.AsBoolean := InChars(Param[0].AsString, CS_ALNUM);
end;

procedure pp_IsCntrl(const Param: TLyParam);
begin
  Param.Result.AsBoolean := InChars(Param[0].AsString, CS_CONTROL);
end;

procedure pp_IsDigit(const Param: TLyParam);
begin
  Param.Result.AsBoolean := InChars(Param[0].AsString, CS_DIGIT);
end;

procedure pp_IsSpace(const Param: TLyParam);
begin
  Param.Result.AsBoolean := InChars(Param[0].AsString, CS_SPACE);
end;

procedure pp_IsHex(const Param: TLyParam);
begin
  Param.Result.AsBoolean := InChars(Param[0].AsString, CS_HEX);
end;

procedure pp_IsLowerCase(const Param: TLyParam);
begin
  Param.Result.AsBoolean := InChars(Param[0].AsString, CS_LOWER);
end;

procedure pp_IsUpperCase(const Param: TLyParam);
begin
  Param.Result.AsBoolean := InChars(Param[0].AsString, CS_UPPER);
end;

procedure pp_SaveTextToFile(const Param: TLyParam);
var
  L: TStrings;
begin
  L := TStringList.Create;
  try
    L.Add(Param[0].AsString);
    L.SaveToFile(Param[1].AsFileName);
  finally
    L.Free;
  end;
end;

{ math }

// function Abs(Value): variant
procedure pp_Abs(const Param: TLyParam);
var
  data: TLyValue;
  clss: TLyType;
begin
  data := Param[0];
  clss := data.VType;
  if clss = my_int then
    Param.Result.AsInteger := Abs(data.AsInteger) else
  if clss = my_float then
    Param.Result.AsFloat := Abs(data.AsFloat) else
  if clss = my_curr then
    Param.Result.AsCurrency := Abs(data.AsCurrency) else
    Param.Result.Assign(data);
end;

// function Round(Value): integer
procedure pp_Round(const Param: TLyParam);
begin
  Param.Result.AsInteger := Round(Param[0].AsFloat);
end;

// function Trunc(Value): integer
procedure pp_Trunc(const Param: TLyParam);
begin
  Param.Result.AsInteger := Trunc(Param[0].AsFloat);
end;

// function Ceil(Value): integer
procedure pp_Ceil(const Param: TLyParam);
begin
  Param.Result.AsInteger := Ceil(Param[0].AsFloat);
end;

// function Floor(Value): integer
procedure pp_Floor(const Param: TLyParam);
begin
  Param.Result.AsInteger := Floor(Param[0].AsFloat);
end;

// function Max(V1, V2, ...): variant
procedure pp_Max(const Param: TLyParam);
var
  V, T: TLyValue;
begin
  V := Param[0];
  T := Param[1];
  if V.Compare(T, [crLess]) then V := T;
  Param.Result.Assign(V);
end;

// function Min(V1, V2, ...): variant
procedure pp_Min(const Param: TLyParam);
var
  V, T: TLyValue;
begin
  V := Param[0];
  T := Param[1];
  if V.Compare(T, [crMore]) then V := T;
  Param.Result.Assign(V);
end;

{ file/path }

procedure pp_FindFirst(const Param: TLyParam);
var
  R: TLySearchRec;
begin
  R := GetSearchRec(Param.Params[2]);
  my_searchrec.Validate(R);
  if R.FindFirst(Param.Params[0].AsString, Param.Params[1].AsInteger) then
    Param.Result.AsInteger := 0 else
    Param.Result.AsInteger := 1;
end;

procedure pp_FindNext(const Param: TLyParam);
var
  R: TLySearchRec;
begin
  R := GetSearchRec(Param.Params[0]);
  my_searchrec.Validate(R);
  if R.FindNext then
    Param.Result.AsInteger := 0 else
    Param.Result.AsInteger := 1;
end;

procedure pp_FindClose(const Param: TLyParam);
var
  R: TLySearchRec;
begin
  R := GetSearchRec(Param.Params[0]);
  my_searchrec.Validate(R);
  R.FindClose;
end;

procedure pp_IncludeTrailingBackslash(const Param: TLyParam);
begin
  Param.Result.AsString := IncludeTrailingBackslash(Param[0].AsString);
end;

procedure pp_IncludeTrailingPathDelimiter(const Param: TLyParam);
begin
  Param.Result.AsString := IncludeTrailingPathDelimiter(Param[0].AsString);
end;

procedure pp_ExcludeTrailingBackslash(const Param: TLyParam);
begin
  Param.Result.AsString := ExcludeTrailingBackslash(Param[0].AsString);
end;

procedure pp_ExcludeTrailingPathDelimiter(const Param: TLyParam);
begin
  Param.Result.AsString := ExcludeTrailingPathDelimiter(Param[0].AsString);
end;

procedure pp_SetPathDelimiter(const Param: TLyParam);
begin
  Param.Result.AsString := SetPD(Param[0].AsString);
end;

procedure pp_DeleteFile(const Param: TLyParam);
begin
  Param.Result.AsBoolean := SysUtils.DeleteFile(Param[0].AsFileName);
end;

procedure pp_RenameFile(const Param: TLyParam);
var
  OldName: string;
  NewName: string;
begin
  OldName := Param[0].AsFileName;
  NewName := Param[1].AsFileName;
  Param.Result.AsBoolean := SysUtils.RenameFile(OldName, NewName);
end;

procedure pp_CopyFile(const Param: TLyParam);
var
  F1: string;
  F2: string;
begin
  F1 := Param[0].AsFileName;
  F2 := Param[1].AsFileName;
  Param.Result.AsBoolean := Windows.CopyFile(pchar(F1), pchar(F2), Param[2].AsBoolean);
end;

procedure pp_RemoveDir(const Param: TLyParam);
begin
  Param.Result.AsBoolean := RemoveDir(Param[0].AsFileName);
end;

procedure pp_CreateDir(const Param: TLyParam);
begin
  Param.Result.AsBoolean := CreateDir(Param[0].AsFileName);
end;

procedure pp_GetCurrentDir(const Param: TLyParam);
begin
  Param.Result.AsString := GetCurrentDir;
end;

procedure pp_SetCurrentDir(const Param: TLyParam);
begin
  Param.Result.AsBoolean := SetCurrentDir(Param[0].AsFileName);
end;

procedure pp_FileExists(const Param: TLyParam);
begin
  Param.Result.AsBoolean := FileExists(Param[0].AsFileName);
end;

procedure pp_DirectoryExists(const Param: TLyParam);
begin
  Param.Result.AsBoolean := DirectoryExists(Param[0].AsFileName);
end;

procedure pp_ForceDirectories(const Param: TLyParam);
begin
  Param.Result.AsBoolean := ForceDirectories(Param[0].AsFileName);
end;

procedure pp_ChangeFileExt(const Param: TLyParam);
begin
  Param.Result.AsString := ChangeFileExt(Param[0].AsFileName, Param[1].AsString);
end;

procedure pp_ExtractFilePath(const Param: TLyParam);
begin
  Param.Result.AsString := ExtractFilePath(Param[0].AsFileName);
end;

procedure pp_ExtractFileDir(const Param: TLyParam);
begin
  Param.Result.AsString := ExtractFileDir(Param[0].AsFileName);
end;

procedure pp_ExtractFileName(const Param: TLyParam);
begin
  Param.Result.AsString := ExtractFileName(Param[0].AsFileName);
end;

procedure pp_ExtractFileExt(const Param: TLyParam);
begin
  Param.Result.AsString := ExtractFileExt(Param[0].AsFileName);
end;

procedure pp_ExpandFileName(const Param: TLyParam);
begin
  Param.Result.AsString := ExpandFileName(Param[0].AsFileName);
end;

procedure pp_ExtractRelativePath(const Param: TLyParam);
begin
  Param.Result.AsString := ExtractRelativePath(Param[0].AsFileName, Param[1].AsFileName);
end;

{$IFNDEF FPC}
procedure pp_ChangeFilePath(const Param: TLyParam);
begin
  Param.Result.AsString := ChangeFilePath(Param[0].AsFileName, Param[1].AsFileName);
end;

procedure pp_GetHomePath(const Param: TLyParam);
begin
  Param.Result.AsString := GetHomePath;
end;

procedure pp_IsRelativePath(const Param: TLyParam);
begin
  Param.Result.AsBoolean := IsRelativePath(Param[0].AsFileName);
end;
{$ENDIF}

// function GetTempFileName(FileExt): string
procedure pp_GetTempFileName(const Param: TLyParam);
begin
  Param.Result.AsString := my_tmpath + GenID + Param[0].AsString;
end;

{ ym & ymd}

procedure pp_GetYm(const Param: TLyParam);
begin
  Param.Result.AsInteger := GetYm;
end;

procedure pp_GetYmFrom(const Param: TLyParam);
begin
  Param.Result.AsInteger := GetYmFrom(Param[0].AsTime);
end;

procedure pp_IsYm(const Param: TLyParam);
begin
  Param.Result.AsBoolean := IsYm(Param[0].AsInteger);
end;

procedure pp_IsYmOf(const Param: TLyParam);
begin
  Param.Result.AsBoolean := IsYmOf(Param[0].AsInteger, Param[1].AsInteger);
end;

procedure pp_IsYmIn(const Param: TLyParam);
begin
  Param.Result.AsBoolean := IsYmIn(Param[0].AsInteger, Param[1].AsInteger, Param[2].AsInteger);
end;

procedure pp_IsYmStr(const Param: TLyParam);
begin
  Param.Result.AsBoolean := IsYmStr(Param[0].AsString);
end;

procedure pp_YmToStr(const Param: TLyParam);
begin
  Param.Result.AsString := YmToStr(Param[0].AsInteger);
end;

procedure pp_YmToStrOf(const Param: TLyParam);
begin
  Param.Result.AsString := YmToStrOf(Param[0].AsInteger, Param[1].AsString);
end;

procedure pp_StrToYm(const Param: TLyParam);
begin
  if Param.Prmc > 1 then
    Param.Result.AsInteger := StrToYm(Param[0].AsString, Param[1].AsInteger) else
    Param.Result.AsInteger := StrToYm(Param[0].AsString);
end;

procedure pp_PrevYm(const Param: TLyParam);
begin
  if Param.Prmc > 1 then
    Param.Result.AsInteger := PrevYm(Param[0].AsInteger, Param[1].AsInteger) else
    Param.Result.AsInteger := PrevYm(Param[0].AsInteger);
end;

procedure pp_PrevYmStr(const Param: TLyParam);
begin
  Param.Result.AsString := PrevYmStr(Param[0].AsString);
end;

procedure pp_NextYm(const Param: TLyParam);
begin
  if Param.Prmc > 1 then
    Param.Result.AsInteger := NextYm(Param[0].AsInteger, Param[1].AsInteger) else
    Param.Result.AsInteger := NextYm(Param[0].AsInteger);
end;

procedure pp_NextYmStr(const Param: TLyParam);
begin
  Param.Result.AsString := NextYmStr(Param[0].AsString);
end;

procedure pp_GetYmd(const Param: TLyParam);
begin
  Param.Result.AsInteger := GetYmd;
end;

procedure pp_GetYmdFrom(const Param: TLyParam);
begin
  Param.Result.AsInteger := GetYmdFrom(Param[0].AsTime);
end;

procedure pp_IsYmd(const Param: TLyParam);
begin
  Param.Result.AsBoolean := IsYmd(Param[0].AsInteger);
end;

procedure pp_IsYmdOf(const Param: TLyParam);
begin
  Param.Result.AsBoolean := IsYmdOf(Param[0].AsInteger, Param[1].AsInteger, Param[2].AsInteger);
end;

procedure pp_IsYmdStr(const Param: TLyParam);
begin
  Param.Result.AsBoolean := IsYmdStr(Param[0].AsString);
end;

procedure pp_YmdToStr(const Param: TLyParam);
begin
  Param.Result.AsString := YmdToStr(Param[0].AsInteger);
end;

procedure pp_YmdToStrOf(const Param: TLyParam);
begin
  Param.Result.AsString := YmdToStrOf(Param[0].AsInteger, Param[1].AsString);
end;

procedure pp_StrToYmd(const Param: TLyParam);
begin
  if Param.Prmc > 1 then
    Param.Result.AsInteger := StrToYmd(Param[0].AsString, Param[1].AsInteger) else
    Param.Result.AsInteger := StrToYmd(Param[0].AsString);
end;

procedure pp_YmdToDate(const Param: TLyParam);
begin
  Param.Result.AsTime := YmdToDate(Param[0].AsInteger);
end;

procedure pp_NextYmd(const Param: TLyParam);
begin
  if Param.Prmc > 1 then
    Param.Result.AsInteger := NextYmd(Param[0].AsInteger, Param[1].AsInteger) else
    Param.Result.AsInteger := NextYmd(Param[0].AsInteger);
end;

procedure pp_NextYmdStr(const Param: TLyParam);
begin
  Param.Result.AsString := NextYmdStr(Param[0].AsString);
end;

procedure pp_PrevYmd(const Param: TLyParam);
begin
  if Param.Prmc > 1 then
    Param.Result.AsInteger := PrevYmd(Param[0].AsInteger, Param[1].AsInteger) else
    Param.Result.AsInteger := PrevYmd(Param[0].AsInteger);
end;

procedure pp_PrevYmdStr(const Param: TLyParam);
begin
  Param.Result.AsString := PrevYmdStr(Param[0].AsString);
end;

{ datetime }

procedure pp_EncodeDate(const Param: TLyParam);
begin
  Param.Result.AsTime := EncodeDate(word(Param[0].AsInteger),
    word(Param[1].AsInteger), word(Param[2].AsInteger));
end;

procedure pp_EncodeTime(const Param: TLyParam);
begin
  Param.Result.AsTime := EncodeTime(word(Param[0].AsInteger),
    word(Param[1].AsInteger), word(Param[2].AsInteger),
    word(Param[3].AsInteger));
end;

procedure pp_DayOfWeek(const Param: TLyParam);
begin
  Param.Result.AsInteger := DayOfWeek(Param[0].AsTime);
end;

procedure pp_Date(const Param: TLyParam);
begin
  Param.Result.AsTime := Date;
end;

procedure pp_Time(const Param: TLyParam);
begin
  Param.Result.AsTime := Time;
end;

procedure pp_IncMonth(const Param: TLyParam);
begin
  Param.Result.AsTime := IncMonth(
    Param[0].AsTime, integer(Param[1].AsInteger));
end;

procedure pp_DateToStr(const Param: TLyParam);
begin
  Param.Result.AsString := DateToStr(Param[0].AsTime);
end;

procedure pp_TimeToStr(const Param: TLyParam);
begin
  Param.Result.AsString := TimeToStr(Param[0].AsTime);
end;

procedure pp_DateTimeToStr(const Param: TLyParam);
begin
  Param.Result.AsString := DateTimeToStr(Param[0].AsTime);
end;

procedure pp_StrToDate(const Param: TLyParam);
begin
  Param.Result.AsTime := StrToDate(Param[0].AsString);
end;

procedure pp_StrToTime(const Param: TLyParam);
begin
  Param.Result.AsTime := StrToTime(Param[0].AsString);
end;

procedure pp_StrToDateTime(const Param: TLyParam);
begin
  Param.Result.AsTime := StrToDateTime(Param[0].AsString);
end;

procedure pp_FormatDateTime(const Param: TLyParam);
begin
  Param.Result.AsString := FormatDateTime(Param[0].AsString, Param[1].AsTime);
end;

procedure pp_DateTimeToFileDate(const Param: TLyParam);
begin
  Param.Result.AsInteger := DateTimeToFileDate(Param[0].AsTime);
end;

procedure pp_FileDateToDateTime(const Param: TLyParam);
begin
  Param.Result.AsTime := FileDateToDateTime(longint(Param[0].AsInteger));
end;

procedure pp_StrToDateDef(const Param: TLyParam);
begin
  Param.Result.AsTime := StrToDateDef(Param[0].AsString, Param[1].AsTime);
end;

procedure pp_StrToTimeDef(const Param: TLyParam);
begin
  Param.Result.AsTime := StrToTimeDef(Param[0].AsString, Param[1].AsTime);
end;

procedure pp_StrToDateTimeDef(const Param: TLyParam);
begin
  Param.Result.AsTime := StrToDateTimeDef(Param[0].AsString, Param[1].AsTime);
end;

procedure pp_CurrentYear(const Param: TLyParam);
begin
  Param.Result.AsInteger := int64(CurrentYear);
end;

procedure pp_DateOf(const Param: TLyParam);
begin
  Param.Result.AsTime := DateOf(Param[0].AsTime);
end;

procedure pp_TimeOf(const Param: TLyParam);
begin
  Param.Result.AsTime := TimeOf(Param[0].AsTime);
end;

procedure pp_IsInLeapYear(const Param: TLyParam);
begin
  Param.Result.AsBoolean := IsInLeapYear(Param[0].AsTime);
end;

procedure pp_IsPM(const Param: TLyParam);
begin
  Param.Result.AsBoolean := IsPM(Param[0].AsTime);
end;

procedure pp_IsValidDate(const Param: TLyParam);
begin
  Param.Result.AsBoolean := IsValidDate(word(Param[0].AsInteger),
    word(Param[1].AsInteger), word(Param[2].AsInteger));
end;

procedure pp_IsValidTime(const Param: TLyParam);
begin
  Param.Result.AsBoolean := IsValidTime(word(Param[0].AsInteger),
    word(Param[1].AsInteger), word(Param[2].AsInteger), word(Param[3].AsInteger));
end;

procedure pp_IsValidDateTime(const Param: TLyParam);
begin
  Param.Result.AsBoolean := IsValidDateTime(word(Param[0].AsInteger),
    word(Param[1].AsInteger), word(Param[2].AsInteger),
    word(Param[3].AsInteger), word(Param[4].AsInteger),
    word(Param[5].AsInteger), word(Param[6].AsInteger));
end;

procedure pp_IsValidDateDay(const Param: TLyParam);
begin
  Param.Result.AsBoolean := IsValidDateDay(
    word(Param[0].AsInteger), word(Param[1].AsInteger));
end;

procedure pp_IsValidDateWeek(const Param: TLyParam);
begin
  Param.Result.AsBoolean := IsValidDateWeek(word(Param[0].AsInteger),
    word(Param[1].AsInteger), word(Param[2].AsInteger));
end;

procedure pp_IsValidDateMonthWeek(const Param: TLyParam);
begin
  Param.Result.AsBoolean := IsValidDateMonthWeek(word(Param[0].AsInteger),
    word(Param[1].AsInteger), word(Param[2].AsInteger), word(Param[3].AsInteger));
end;

procedure pp_WeeksInYear(const Param: TLyParam);
begin
  Param.Result.AsInteger := WeeksInYear(Param[0].AsTime);
end;

procedure pp_WeeksInAYear(const Param: TLyParam);
begin
  Param.Result.AsInteger := WeeksInAYear(word(Param[0].AsInteger));
end;

procedure pp_DaysInYear(const Param: TLyParam);
begin
  Param.Result.AsInteger := DaysInYear(Param[0].AsTime);
end;

procedure pp_DaysInAYear(const Param: TLyParam);
begin
  Param.Result.AsInteger := DaysInAYear(word(Param[0].AsInteger));
end;

procedure pp_DaysInMonth(const Param: TLyParam);
begin
  Param.Result.AsInteger := DaysInMonth(Param[0].AsTime);
end;

procedure pp_DaysInAMonth(const Param: TLyParam);
begin
  Param.Result.AsInteger := DaysInAMonth(
    word(Param[0].AsInteger), word(Param[1].AsInteger));
end;

procedure pp_Today(const Param: TLyParam);
begin
  Param.Result.AsTime := Today;
end;

procedure pp_Yesterday(const Param: TLyParam);
begin
  Param.Result.AsTime := Yesterday;
end;

procedure pp_Tomorrow(const Param: TLyParam);
begin
  Param.Result.AsTime := Tomorrow;
end;

procedure pp_IsToday(const Param: TLyParam);
begin
  Param.Result.AsBoolean := IsToday(Param[0].AsTime);
end;

procedure pp_IsSameDay(const Param: TLyParam);
begin
  Param.Result.AsBoolean := IsSameDay(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_YearOf(const Param: TLyParam);
begin
  Param.Result.AsInteger := YearOf(Param[0].AsTime);
end;

procedure pp_MonthOf(const Param: TLyParam);
begin
  Param.Result.AsInteger := MonthOf(Param[0].AsTime);
end;

procedure pp_WeekOf(const Param: TLyParam);
begin
  Param.Result.AsInteger := WeekOf(Param[0].AsTime);
end;

procedure pp_DayOf(const Param: TLyParam);
begin
  Param.Result.AsInteger := DayOf(Param[0].AsTime);
end;

procedure pp_HourOf(const Param: TLyParam);
begin
  Param.Result.AsInteger := HourOf(Param[0].AsTime);
end;

procedure pp_MinuteOf(const Param: TLyParam);
begin
  Param.Result.AsInteger := MinuteOf(Param[0].AsTime);
end;

procedure pp_SecondOf(const Param: TLyParam);
begin
  Param.Result.AsInteger := SecondOf(Param[0].AsTime);
end;

procedure pp_MilliSecondOf(const Param: TLyParam);
begin
  Param.Result.AsInteger := MilliSecondOf(Param[0].AsTime);
end;

procedure pp_StartOfTheYear(const Param: TLyParam);
begin
  Param.Result.AsTime := StartOfTheYear(Param[0].AsTime);
end;

procedure pp_EndOfTheYear(const Param: TLyParam);
begin
  Param.Result.AsTime := EndOfTheYear(Param[0].AsTime);
end;

procedure pp_StartOfAYear(const Param: TLyParam);
begin
  Param.Result.AsTime := StartOfAYear(word(Param[0].AsInteger));
end;

procedure pp_EndOfAYear(const Param: TLyParam);
begin
  Param.Result.AsTime := EndOfAYear(word(Param[0].AsInteger));
end;

procedure pp_StartOfTheMonth(const Param: TLyParam);
begin
  Param.Result.AsTime := StartOfTheMonth(Param[0].AsTime);
end;

procedure pp_EndOfTheMonth(const Param: TLyParam);
begin
  Param.Result.AsTime := EndOfTheMonth(Param[0].AsTime);
end;

procedure pp_StartOfAMonth(const Param: TLyParam);
begin
  Param.Result.AsTime := StartOfAMonth(
    word(Param[0].AsInteger), word(Param[1].AsInteger));
end;

procedure pp_EndOfAMonth(const Param: TLyParam);
begin
  Param.Result.AsTime := EndOfAMonth(
    word(Param[0].AsInteger), word(Param[1].AsInteger));
end;

procedure pp_StartOfTheWeek(const Param: TLyParam);
begin
  Param.Result.AsTime := StartOfTheWeek(Param[0].AsTime);
end;

procedure pp_EndOfTheWeek(const Param: TLyParam);
begin
  Param.Result.AsTime := EndOfTheWeek(Param[0].AsTime);
end;

procedure pp_StartOfAWeek(const Param: TLyParam);
begin
  Param.Result.AsTime := StartOfAWeek(word(Param[0].AsInteger),
    word(Param[1].AsInteger), word(Param[2].AsInteger));
end;

procedure pp_EndOfAWeek(const Param: TLyParam);
begin
  Param.Result.AsTime := EndOfAWeek(word(Param[0].AsInteger),
    word(Param[1].AsInteger), word(Param[2].AsInteger));
end;

procedure pp_StartOfTheDay(const Param: TLyParam);
begin
  Param.Result.AsTime := StartOfTheDay(Param[0].AsTime);
end;

procedure pp_EndOfTheDay(const Param: TLyParam);
begin
  Param.Result.AsTime := EndOfTheDay(Param[0].AsTime);
end;

procedure pp_StartOfADay(const Param: TLyParam);
begin
  Param.Result.AsTime := StartOfADay(word(Param[0].AsInteger),
    word(Param[1].AsInteger), word(Param[2].AsInteger));
end;

procedure pp_EndOfADay(const Param: TLyParam);
begin
  Param.Result.AsTime := EndOfADay(word(Param[0].AsInteger),
    word(Param[1].AsInteger), word(Param[2].AsInteger));
end;

procedure pp_MonthOfTheYear(const Param: TLyParam);
begin
  Param.Result.AsInteger := MonthOfTheYear(Param[0].AsTime);
end;

procedure pp_WeekOfTheYear(const Param: TLyParam);
begin
  Param.Result.AsInteger := WeekOfTheYear(Param[0].AsTime);
end;

procedure pp_DayOfTheYear(const Param: TLyParam);
begin
  Param.Result.AsInteger := DayOfTheYear(Param[0].AsTime);
end;

procedure pp_HourOfTheYear(const Param: TLyParam);
begin
  Param.Result.AsInteger := HourOfTheYear(Param[0].AsTime);
end;

procedure pp_MinuteOfTheYear(const Param: TLyParam);
begin
  Param.Result.AsInteger := MinuteOfTheYear(Param[0].AsTime);
end;

procedure pp_SecondOfTheYear(const Param: TLyParam);
begin
  Param.Result.AsInteger := SecondOfTheYear(Param[0].AsTime);
end;

procedure pp_MilliSecondOfTheYear(const Param: TLyParam);
begin
  Param.Result.AsInteger := MilliSecondOfTheYear(Param[0].AsTime);
end;

procedure pp_WeekOfTheMonth(const Param: TLyParam);
begin
  Param.Result.AsInteger := WeekOfTheMonth(Param[0].AsTime);
end;

procedure pp_DayOfTheMonth(const Param: TLyParam);
begin
  Param.Result.AsInteger := DayOfTheMonth(Param[0].AsTime);
end;

procedure pp_HourOfTheMonth(const Param: TLyParam);
begin
  Param.Result.AsInteger := HourOfTheMonth(Param[0].AsTime);
end;

procedure pp_MinuteOfTheMonth(const Param: TLyParam);
begin
  Param.Result.AsInteger := MinuteOfTheMonth(Param[0].AsTime);
end;

procedure pp_SecondOfTheMonth(const Param: TLyParam);
begin
  Param.Result.AsInteger := SecondOfTheMonth(Param[0].AsTime);
end;

procedure pp_MilliSecondOfTheMonth(const Param: TLyParam);
begin
  Param.Result.AsInteger := MilliSecondOfTheMonth(Param[0].AsTime);
end;

procedure pp_DayOfTheWeek(const Param: TLyParam);
begin
  Param.Result.AsInteger := DayOfTheWeek(Param[0].AsTime);
end;

procedure pp_HourOfTheWeek(const Param: TLyParam);
begin
  Param.Result.AsInteger := HourOfTheWeek(Param[0].AsTime);
end;

procedure pp_MinuteOfTheWeek(const Param: TLyParam);
begin
  Param.Result.AsInteger := MinuteOfTheWeek(Param[0].AsTime);
end;

procedure pp_SecondOfTheWeek(const Param: TLyParam);
begin
  Param.Result.AsInteger := SecondOfTheWeek(Param[0].AsTime);
end;

procedure pp_MilliSecondOfTheWeek(const Param: TLyParam);
begin
  Param.Result.AsInteger := MilliSecondOfTheWeek(Param[0].AsTime);
end;

procedure pp_HourOfTheDay(const Param: TLyParam);
begin
  Param.Result.AsInteger := HourOfTheDay(Param[0].AsTime);
end;

procedure pp_MinuteOfTheDay(const Param: TLyParam);
begin
  Param.Result.AsInteger := MinuteOfTheDay(Param[0].AsTime);
end;

procedure pp_SecondOfTheDay(const Param: TLyParam);
begin
  Param.Result.AsInteger := SecondOfTheDay(Param[0].AsTime);
end;

procedure pp_MilliSecondOfTheDay(const Param: TLyParam);
begin
  Param.Result.AsInteger := MilliSecondOfTheDay(Param[0].AsTime);
end;

procedure pp_MinuteOfTheHour(const Param: TLyParam);
begin
  Param.Result.AsInteger := MinuteOfTheHour(Param[0].AsTime);
end;

procedure pp_SecondOfTheHour(const Param: TLyParam);
begin
  Param.Result.AsInteger := SecondOfTheHour(Param[0].AsTime);
end;

procedure pp_MilliSecondOfTheHour(const Param: TLyParam);
begin
  Param.Result.AsInteger := MilliSecondOfTheHour(Param[0].AsTime);
end;

procedure pp_SecondOfTheMinute(const Param: TLyParam);
begin
  Param.Result.AsInteger := SecondOfTheMinute(Param[0].AsTime);
end;

procedure pp_MilliSecondOfTheMinute(const Param: TLyParam);
begin
  Param.Result.AsInteger := MilliSecondOfTheMinute(Param[0].AsTime);
end;

procedure pp_MilliSecondOfTheSecond(const Param: TLyParam);
begin
  Param.Result.AsInteger := MilliSecondOfTheSecond(Param[0].AsTime);
end;

procedure pp_WithinPastYears(const Param: TLyParam);
begin
  Param.Result.AsBoolean := WithinPastYears(
    Param[0].AsTime, Param[1].AsTime, integer(Param[2].AsInteger));
end;

procedure pp_WithinPastMonths(const Param: TLyParam);
begin
  Param.Result.AsBoolean := WithinPastMonths(
    Param[0].AsTime, Param[1].AsTime, integer(Param[2].AsInteger));
end;

procedure pp_WithinPastWeeks(const Param: TLyParam);
begin
  Param.Result.AsBoolean := WithinPastWeeks(
    Param[0].AsTime, Param[1].AsTime, integer(Param[2].AsInteger));
end;

procedure pp_WithinPastDays(const Param: TLyParam);
begin
  Param.Result.AsBoolean := WithinPastDays(
    Param[0].AsTime, Param[1].AsTime, integer(Param[2].AsInteger));
end;

procedure pp_WithinPastHours(const Param: TLyParam);
begin
  Param.Result.AsBoolean := WithinPastHours(
    Param[0].AsTime, Param[1].AsTime, Param[2].AsInteger);
end;

procedure pp_WithinPastMinutes(const Param: TLyParam);
begin
  Param.Result.AsBoolean := WithinPastMinutes(
    Param[0].AsTime, Param[1].AsTime, Param[2].AsInteger);
end;

procedure pp_WithinPastSeconds(const Param: TLyParam);
begin
  Param.Result.AsBoolean := WithinPastSeconds(
    Param[0].AsTime, Param[1].AsTime, Param[2].AsInteger);
end;

procedure pp_WithinPastMilliSeconds(const Param: TLyParam);
begin
  Param.Result.AsBoolean := WithinPastMilliSeconds(
    Param[0].AsTime, Param[1].AsTime, Param[2].AsInteger);
end;

procedure pp_YearsBetween(const Param: TLyParam);
begin
  Param.Result.AsInteger := YearsBetween(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_MonthsBetween(const Param: TLyParam);
begin
  Param.Result.AsInteger := MonthsBetween(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_WeeksBetween(const Param: TLyParam);
begin
  Param.Result.AsInteger := WeeksBetween(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_DaysBetween(const Param: TLyParam);
begin
  Param.Result.AsInteger := DaysBetween(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_HoursBetween(const Param: TLyParam);
begin
  Param.Result.AsInteger := HoursBetween(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_MinutesBetween(const Param: TLyParam);
begin
  Param.Result.AsInteger := MinutesBetween(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_SecondsBetween(const Param: TLyParam);
begin
  Param.Result.AsInteger := SecondsBetween(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_MilliSecondsBetween(const Param: TLyParam);
begin
  Param.Result.AsInteger := MilliSecondsBetween(
    Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_YearSpan(const Param: TLyParam);
begin
  Param.Result.AsFloat := YearSpan(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_MonthSpan(const Param: TLyParam);
begin
  Param.Result.AsFloat := MonthSpan(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_WeekSpan(const Param: TLyParam);
begin
  Param.Result.AsFloat := WeekSpan(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_DaySpan(const Param: TLyParam);
begin
  Param.Result.AsFloat := DaySpan(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_HourSpan(const Param: TLyParam);
begin
  Param.Result.AsFloat := HourSpan(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_MinuteSpan(const Param: TLyParam);
begin
  Param.Result.AsFloat := MinuteSpan(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_SecondSpan(const Param: TLyParam);
begin
  Param.Result.AsFloat := SecondSpan(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_MilliSecondSpan(const Param: TLyParam);
begin
  Param.Result.AsFloat := MilliSecondSpan(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_IncYear(const Param: TLyParam);
begin
  Param.Result.AsTime := IncYear(Param[0].AsTime, integer(Param[1].AsInteger));
end;

procedure pp_IncWeek(const Param: TLyParam);
begin
  Param.Result.AsTime := IncWeek(Param[0].AsTime, integer(Param[1].AsInteger));
end;

procedure pp_IncDay(const Param: TLyParam);
begin
  Param.Result.AsTime := IncDay(Param[0].AsTime, integer(Param[1].AsInteger));
end;

procedure pp_IncHour(const Param: TLyParam);
begin
  Param.Result.AsTime := IncHour(Param[0].AsTime, Param[1].AsInteger);
end;

procedure pp_IncMinute(const Param: TLyParam);
begin
  Param.Result.AsTime := IncMinute(Param[0].AsTime, Param[1].AsInteger);
end;

procedure pp_IncSecond(const Param: TLyParam);
begin
  Param.Result.AsTime := IncSecond(Param[0].AsTime, Param[1].AsInteger);
end;

procedure pp_IncMilliSecond(const Param: TLyParam);
begin
  Param.Result.AsTime := IncMilliSecond(Param[0].AsTime, Param[1].AsInteger);
end;

procedure pp_EncodeDateTime(const Param: TLyParam);
begin
  Param.Result.AsTime := EncodeDateTime(word(Param[0].AsInteger),
    word(Param[1].AsInteger), word(Param[2].AsInteger),
    word(Param[3].AsInteger), word(Param[4].AsInteger),
    word(Param[5].AsInteger), word(Param[6].AsInteger));
end;

procedure pp_EncodeDateWeek(const Param: TLyParam);
begin
  Param.Result.AsTime := EncodeDateWeek(word(Param[0].AsInteger),
    word(Param[1].AsInteger), word(Param[2].AsInteger));
end;

procedure pp_EncodeDateDay(const Param: TLyParam);
begin
  Param.Result.AsTime := EncodeDateDay(
    word(Param[0].AsInteger), word(Param[1].AsInteger));
end;

procedure pp_EncodeDateMonthWeek(const Param: TLyParam);
begin
  Param.Result.AsTime := EncodeDateMonthWeek(word(Param[0].AsInteger),
    word(Param[1].AsInteger), word(Param[2].AsInteger),
    word(Param[3].AsInteger));
end;

procedure pp_RecodeYear(const Param: TLyParam);
begin
  Param.Result.AsTime := RecodeYear(Param[0].AsTime, word(Param[1].AsInteger));
end;

procedure pp_RecodeMonth(const Param: TLyParam);
begin
  Param.Result.AsTime := RecodeMonth(Param[0].AsTime, word(Param[1].AsInteger));
end;

procedure pp_RecodeDay(const Param: TLyParam);
begin
  Param.Result.AsTime := RecodeDay(Param[0].AsTime, word(Param[1].AsInteger));
end;

procedure pp_RecodeHour(const Param: TLyParam);
begin
  Param.Result.AsTime := RecodeHour(Param[0].AsTime, word(Param[1].AsInteger));
end;

procedure pp_RecodeMinute(const Param: TLyParam);
begin
  Param.Result.AsTime := RecodeMinute(Param[0].AsTime, word(Param[1].AsInteger));
end;

procedure pp_RecodeSecond(const Param: TLyParam);
begin
  Param.Result.AsTime := RecodeSecond(
    Param[0].AsTime, word(Param[1].AsInteger));
end;

procedure pp_RecodeMilliSecond(const Param: TLyParam);
begin
  Param.Result.AsTime := RecodeMilliSecond(
    Param[0].AsTime, word(Param[1].AsInteger));
end;

procedure pp_RecodeDate(const Param: TLyParam);
begin
  Param.Result.AsTime := RecodeDate(Param[0].AsTime, word(Param[1].AsInteger),
    word(Param[2].AsInteger), word(Param[3].AsInteger));
end;

procedure pp_RecodeTime(const Param: TLyParam);
begin
  Param.Result.AsTime := RecodeTime(Param[0].AsTime, word(Param[1].AsInteger),
    word(Param[2].AsInteger), word(Param[3].AsInteger),
    word(Param[4].AsInteger));
end;

procedure pp_RecodeDateTime(const Param: TLyParam);
begin
  Param.Result.AsTime := RecodeDateTime(Param[0].AsTime,
    word(Param[1].AsInteger), word(Param[2].AsInteger),
    word(Param[3].AsInteger), word(Param[4].AsInteger),
    word(Param[5].AsInteger), word(Param[6].AsInteger),
    word(Param[7].AsInteger));
end;

procedure pp_CompareDateTime(const Param: TLyParam);
begin
  Param.Result.AsInteger := CompareDateTime(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_CompareDate(const Param: TLyParam);
begin
  Param.Result.AsInteger := CompareDate(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_CompareTime(const Param: TLyParam);
begin
  Param.Result.AsInteger := CompareTime(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_SameDateTime(const Param: TLyParam);
begin
  Param.Result.AsBoolean := SameDateTime(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_SameDate(const Param: TLyParam);
begin
  Param.Result.AsBoolean := SameDate(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_SameTime(const Param: TLyParam);
begin
  Param.Result.AsBoolean := SameTime(Param[0].AsTime, Param[1].AsTime);
end;

procedure pp_NthDayOfWeek(const Param: TLyParam);
begin
  Param.Result.AsInteger := NthDayOfWeek(Param[0].AsTime);
end;

procedure pp_EncodeDayOfWeekInMonth(const Param: TLyParam);
begin
  Param.Result.AsTime := EncodeDayOfWeekInMonth(word(Param[0].AsInteger),
    word(Param[1].AsInteger), word(Param[2].AsInteger),
    word(Param[3].AsInteger));
end;

procedure pp_InvalidDateTimeError(const Param: TLyParam);
begin
  InvalidDateTimeError(word(Param[0].AsInteger), word(Param[1].AsInteger),
    word(Param[2].AsInteger), word(Param[3].AsInteger),
    word(Param[4].AsInteger), word(Param[5].AsInteger),
    word(Param[6].AsInteger));
end;

procedure pp_InvalidDateWeekError(const Param: TLyParam);
begin
  InvalidDateWeekError(word(Param[0].AsInteger),
    word(Param[1].AsInteger), word(Param[2].AsInteger));
end;

procedure pp_InvalidDateDayError(const Param: TLyParam);
begin
  InvalidDateDayError(word(Param[0].AsInteger), word(Param[1].AsInteger));
end;

procedure pp_InvalidDateMonthWeekError(const Param: TLyParam);
begin
  InvalidDateMonthWeekError(word(Param[0].AsInteger),
    word(Param[1].AsInteger), word(Param[2].AsInteger),
    word(Param[3].AsInteger));
end;

procedure pp_InvalidDayOfWeekInMonthError(const Param: TLyParam);
begin
  InvalidDayOfWeekInMonthError(word(Param[0].AsInteger),
    word(Param[1].AsInteger), word(Param[2].AsInteger),
    word(Param[3].AsInteger));
end;

procedure pp_DateTimeToJulianDate(const Param: TLyParam);
begin
  Param.Result.AsFloat := DateTimeToJulianDate(Param[0].AsTime);
end;

procedure pp_JulianDateToDateTime(const Param: TLyParam);
begin
  Param.Result.AsTime := JulianDateToDateTime(Param[0].AsFloat);
end;

procedure pp_DateTimeToModifiedJulianDate(const Param: TLyParam);
begin
  Param.Result.AsFloat := DateTimeToModifiedJulianDate(Param[0].AsTime);
end;

procedure pp_ModifiedJulianDateToDateTime(const Param: TLyParam);
begin
  Param.Result.AsTime := ModifiedJulianDateToDateTime(Param[0].AsFloat);
end;

procedure pp_DateTimeToUnix(const Param: TLyParam);
begin
  Param.Result.AsInteger := DateTimeToUnix(Param[0].AsTime);
end;

procedure pp_UnixToDateTime(const Param: TLyParam);
begin
  Param.Result.AsTime := UnixToDateTime(Param[0].AsInteger);
end;

{ Sum }

procedure pp_MD5SumFile(const Param: TLyParam);
begin
  Param.Result.AsString := MD5SumFile(Param[0].AsFileName);
end;

procedure pp_MD5SumStr(const Param: TLyParam);
begin
  Param.Result.AsString := MD5SumString(Param[0].AsString);
end;

procedure pp_SHA1SumFile(const Param: TLyParam);
begin
  Param.Result.AsString := SHA1SumFile(Param[0].AsFileName);
end;

procedure pp_SHA1SumStr(const Param: TLyParam);
begin
  Param.Result.AsString := SHA1SumString(Param[0].AsString);
end;

{$IFNDEF FPC}
procedure pp_SHA2SumFile(const Param: TLyParam);
var
  F: TFileStream;
  B: array[0..4159] of byte;
  N: integer;
  M: THashSHA2;
begin
  F := TFileStream.Create(Param[0].AsFileName, fmShareDenyWrite);
  try
    M := THashSHA2.Create;
    N := F.Read(B[0], 4096);
    while N > 0 do
    begin
      M.Update(B[0], N);
      N := F.Read(B[0], 4096);
    end;
    Param.Result.AsString := M.HashAsString;
  finally
    F.Free;
  end;
end;

procedure pp_SHA2SumStr(const Param: TLyParam);
begin
  Param.Result.AsString := THashSHA2.GetHashString(Param[0].AsString);
end;
{$ENDIF}

{ TLyTypeType }

procedure TLyTypeType.MyFindMethod(const Param: TLyParam);
begin
  Param.Result.Assign(my_func, Param[0].AsType.FindMethod(Param[1].AsString, nil));
end;

procedure TLyTypeType.MyIsChildOf(const Param: TLyParam);
begin
  Param.Result.AsBoolean := Param[0].AsType.IsChildOf(Param[1].AsType);
end;

procedure TLyTypeType.MyIsEnum(const Param: TLyParam);
begin
  Param.Result.AsBoolean := Param[0].AsType.IsEnumType;
end;

procedure TLyTypeType.MyIsEnumSet(const Param: TLyParam);
begin
  Param.Result.AsBoolean := Param[0].AsType.IsEnumSetType;
end;

procedure TLyTypeType.MyIsNil(const Param: TLyParam);
begin
  Param.Result.AsBoolean := (Param[0].AsType.Style = tsNil);
end;

procedure TLyTypeType.MyIsObject(const Param: TLyParam);
begin
  Param.Result.AsBoolean := (Param[0].AsType.Style = tsObject);
end;

procedure TLyTypeType.MyIsTypeOf(const Param: TLyParam);
begin
  Param.Result.AsBoolean := Param[0].AsType.IsTypeOf(Param[1].AsType);
end;

procedure TLyTypeType.MyItemValues(const Param: TLyParam);
var
  T: TLyType;
  O: TLyEnumType;
  L: TLyList;
  I: integer;
begin
  L := Param.Result.NewArray;
  T := Param[0].AsType;
  if T.IsEnumType then
  begin
    O := TLyEnumType(T);
    for I := 0 to O.Count - 1 do
      L.Add.Assign(O, O[I]);
  end;
end;

procedure TLyTypeType.MyMethods(const Param: TLyParam);
var
  T: TLyType;
  L: TLyList;
  I: integer;
begin
  L := Param.Result.NewArray;
  T := Param[0].AsType;
  for I := 0 to T.Methods.Count - 1 do
    L.Add.Assign(my_func, T.Methods[I]);
end;

procedure TLyTypeType.MyModule(const Param: TLyParam);
begin
  Param.Result.Assign(my_module, Param[0].AsType.Module);
end;

procedure TLyTypeType.MyName(const Param: TLyParam);
begin
  Param.Result.AsString := Param[0].AsType.Name;
end;

procedure TLyTypeType.MyPrototype(const Param: TLyParam);
begin
  Param.Result.AsString := Param[0].AsType.Prototype(Param[1].AsString);
end;

procedure TLyTypeType.Setup;
begin
  Method('Name', my_string, {$IFDEF FPC}@{$ENDIF}MyName);
  Method('Module', my_module, {$IFDEF FPC}@{$ENDIF}MyModule);
  Method('Methods', my_array, {$IFDEF FPC}@{$ENDIF}MyMethods);
  Method('IsTypeOf', my_bool, ['AType'], [my_type],
         {$IFDEF FPC}@{$ENDIF}MyIsTypeOf);
  Method('IsChildOf', my_bool, ['AType'], [my_type],
         {$IFDEF FPC}@{$ENDIF}MyIsChildOf);
  Method('IsObject', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsObject);
  Method('IsNil', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsNil);
  Method('IsEnum', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsEnum);
  Method('IsEnumSet', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsEnumSet);
  Method('ItemValues', my_array, {$IFDEF FPC}@{$ENDIF}MyItemValues);
  Method('Prototype', my_string, ['Name'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyPrototype);
  Method('FindMethod', my_func, ['Name'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyFindMethod);
  inherited;
end;

function TLyTypeType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyType(Obj).FullName else
    Result := '';
end;

procedure TLyTypeType.Convert(Value: TLyValue);
begin
  if Value.VType <> my_type then
    Value.Assign(my_type, Value.VType);
end;

{ TLyErrorType }

procedure TLyErrorType.MyErrno(const Param: TLyParam);
begin
  Param.Result.AsString := Param.Thread.Error.Errno;
end;

procedure TLyErrorType.MyMsg(const Param: TLyParam);
begin
  Param.Result.AsString := Param.Thread.Error.Msg;
end;

procedure TLyErrorType.MyText(const Param: TLyParam);
begin
  Param.Result.AsString := Param.Thread.Error.AsText;
end;

procedure TLyErrorType.MyModule(const Param: TLyParam);
begin
  Param.Result.AsString := Param.Thread.Error.Module;
end;

procedure TLyErrorType.MyFileName(const Param: TLyParam);
begin
  Param.Result.AsString := Param.Thread.Error.FileName;
end;

procedure TLyErrorType.MyRow(const Param: TLyParam);
begin
  Param.Result.AsInteger := Param.Thread.Error.Row;
end;

procedure TLyErrorType.MyCol(const Param: TLyParam);
begin
  Param.Result.AsInteger := Param.Thread.Error.Col;
end;

procedure TLyErrorType.MyExcepted(const Param: TLyParam);
begin
  Param.Result.AsBoolean := Param.Thread.Excepted;
end;

procedure TLyErrorType.Setup;
begin
  Method('Errno', my_string, {$IFDEF FPC}@{$ENDIF}MyErrno);
  Method('Msg', my_string, {$IFDEF FPC}@{$ENDIF}MyMsg);
  Method('Text', my_string, {$IFDEF FPC}@{$ENDIF}MyText);
  Method('Module', my_string, {$IFDEF FPC}@{$ENDIF}MyModule);
  Method('FileName', my_string, {$IFDEF FPC}@{$ENDIF}MyFileName);
  Method('Row', my_int, {$IFDEF FPC}@{$ENDIF}MyRow);
  Method('Col', my_int, {$IFDEF FPC}@{$ENDIF}MyCol);
  Method('Excepted', my_bool, {$IFDEF FPC}@{$ENDIF}MyExcepted);
  inherited;
end;

function TLyErrorType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyError(Obj).AsText else
    Result := '';
end;

{ TLyFuncType }

procedure TLyFuncType.MyIsConstructor(const Param: TLyParam);
var
  F: TLyFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsBoolean := F.IsConstructor;
end;

procedure TLyFuncType.MyIsMainFunc(const Param: TLyParam);
var
  F: TLyFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsBoolean := F.IsMainFunc;
end;

procedure TLyFuncType.MyIsMethod(const Param: TLyParam);
var
  F: TLyFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsBoolean := F.IsMethod;
end;

procedure TLyFuncType.MyParamNames_Get(const Param: TLyParam);
var
  F: TLyFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsString := F.Params[Param[1].AsInteger].Name;
end;

procedure TLyFuncType.MyParamTypes_Get(const Param: TLyParam);
var
  F: TLyFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.Assign(my_type, F.Params[Param[1].AsInteger].ValueType);
end;

procedure TLyFuncType.MyParamTypes_List(const Param: TLyParam);
var
  F: TLyFunc;
  L: TLyList;
  I: integer;
begin
  if Param.GetSelf(F) then
  begin
    L := Param.Result.NewArray;
    for I := 0 to F.Params.ParamCount - 1 do
      L.Add.Assign(my_type, F.Params[I].ValueType);
  end;
end;

procedure TLyFuncType.MyResultType(const Param: TLyParam);
var
  F: TLyFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.Assign(my_type, F.ResultType);
end;

procedure TLyFuncType.MyModule(const Param: TLyParam);
var
  F: TLyFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.Assign(my_module, F.Module);
end;

procedure TLyFuncType.MyName(const Param: TLyParam);
var
  F: TLyFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsString := F.Name;
end;

procedure TLyFuncType.MyParamCount(const Param: TLyParam);
var
  F: TLyFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsInteger := F.Params.ParamCount;
end;

procedure TLyFuncType.MyParamNames_List(const Param: TLyParam);
var
  F: TLyFunc;
  L: TLyList;
  I: integer;
begin
  if Param.GetSelf(F) then
  begin
    L := Param.Result.NewArray;
    for I := 0 to F.Params.ParamCount - 1 do
      L.Add.AsString := F.Params[I].Name;
  end;
end;

procedure TLyFuncType.MyParent(const Param: TLyParam);
var
  F: TLyFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.Assign(my_type, F.Parent);
end;

procedure TLyFuncType.MyPrototype(const Param: TLyParam);
var
  F: TLyFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsString := F.Prototype;
end;

procedure TLyFuncType.Setup;
begin
  Method('Name', my_string, {$IFDEF FPC}@{$ENDIF}MyName);
  Method('Prototype', my_string, {$IFDEF FPC}@{$ENDIF}MyPrototype);
  Method('Parent', my_type, {$IFDEF FPC}@{$ENDIF}MyParent);
  Method('Module', my_module, {$IFDEF FPC}@{$ENDIF}MyModule);
  Method('IsMainFunc', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsMainFunc);
  Method('IsMethod', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsMethod);
  Method('IsConstructor', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsConstructor);
  Method('ParamCount', my_int, {$IFDEF FPC}@{$ENDIF}MyParamCount);
  Method('ResultType', my_type, {$IFDEF FPC}@{$ENDIF}MyResultType);

  FParamNamesType := Branch('ParamNames') as TLyFuncType;
  FParamNamesType.SetupParamNamesType;

  FParamTypesType := Branch('ParamTypes') as TLyFuncType;
  FParamTypesType.SetupParamTypesType;

  inherited;
end;

procedure TLyFuncType.SetupParamNamesType;
begin
  Define(my_string, ['Index'], [my_int], {$IFDEF FPC}@{$ENDIF}MyParamNames_Get);
  Method('List', my_array, {$IFDEF FPC}@{$ENDIF}MyParamNames_List);
end;

procedure TLyFuncType.SetupParamTypesType;
begin
  Define(my_type, ['Index'], [my_int], {$IFDEF FPC}@{$ENDIF}MyParamTypes_Get);
  Method('List', my_array, {$IFDEF FPC}@{$ENDIF}MyParamTypes_List);
end;

function TLyFuncType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyFunc(Obj).Prototype else
    Result := '';
end;

function TLyFuncType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyFunc(Obj).DecRefcount else
    Result := 0;
end;

function TLyFuncType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyFunc(Obj).IncRefcount else
    Result := 0;
end;

{ TLyModuleType }

procedure TLyModuleType.MyConsts(const Param: TLyParam);
var
  M: TLyModule;
begin
  if Param.GetSelf(M) then
    Param.Result.NewArray.AssignNames(M.Consts);
end;

procedure TLyModuleType.MyFuncs(const Param: TLyParam);
var
  M: TLyModule;
  L: TLyList;
  I: integer;
begin
  if Param.GetSelf(M) then
  begin
    L := Param.Result.NewArray;
    for I := 0 to M.FuncList.Count - 1 do
      L.Add.Assign(my_func, TLyFunc(M.FuncList[I]));
  end;
end;

procedure TLyModuleType.MyFind(const Param: TLyParam);
var
  M: TLyModule;
begin
  if Param.GetSelf(M) then
    M.FindSave(Param[1].AsString, Param.Result);
end;

procedure TLyModuleType.MyName(const Param: TLyParam);
var
  M: TLyModule;
begin
  if Param.GetSelf(M) then
    Param.Result.AsString := M.Name;
end;

procedure TLyModuleType.MyTypes(const Param: TLyParam);
var
  M: TLyModule;
  L: TLyList;
  I: integer;
begin
  if Param.GetSelf(M) then
  begin
    L := Param.Result.NewArray;
    for I := 0 to M.TypeCount - 1 do
      L.Add.Assign(my_type, M.GetType(I));
  end;
end;

procedure TLyModuleType.MyUsings(const Param: TLyParam);
var
  M: TLyModule;
begin
  if Param.GetSelf(M) then
    if M.Modules <> nil then
      Param.Result.Assign(my_array, M.Modules.ToList) else
      Param.Result.NewArray;
end;

procedure TLyModuleType.Setup;
begin
  Method('Name', my_string, {$IFDEF FPC}@{$ENDIF}MyName);
  Method('Consts', my_array, {$IFDEF FPC}@{$ENDIF}MyConsts);
  Method('Types', my_array, {$IFDEF FPC}@{$ENDIF}MyTypes);
  Method('Funcs', my_array, {$IFDEF FPC}@{$ENDIF}MyFuncs);
  Method('Usings', my_array, {$IFDEF FPC}@{$ENDIF}MyUsings);
  Method('Find', my_variant, ['Name'], [my_string], {$IFDEF FPC}@{$ENDIF}MyFind);
  inherited;
end;

function TLyModuleType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyModule(Obj).Name else
    Result := '';
end;

function TLyModuleType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyModule(Obj).DecRefcount else
    Result := 0;
end;

function TLyModuleType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyModule(Obj).IncRefcount else
    Result := 0;
end;

{ TLyStringType }

procedure TLyStringType.MyClone(const Param: TLyParam);
var
  S, N: TLyString;
begin
  S := TLyString(Param[0].Data);
  if S <> nil then
  begin
    N := TLyString.Create;
    N.Data := S.FData;
  end
  else N := TLyString.Create;
  Param.Result.Assign(Self, N);
end;

procedure TLyStringType.MyGet(const Param: TLyParam);
var
  S: TLyString;
begin
  S := TLyString(Param[0].Data);
  Param.Result.AsChar := S.FData[Param[1].AsInteger];
end;

procedure TLyStringType.MyName(const Param: TLyParam);
var
  S: TLyString;
begin
  S := TLyString(Param[0].Data);
  Param.Result.AsString := ExtractName(S.FData, '=');
end;

procedure TLyStringType.MySet(const Param: TLyParam);
var
  S: TLyString;
begin
  S := TLyString(Param[0].Data);
  S.FData[Param[1].AsInteger] := Param[2].AsChar;
end;

procedure TLyStringType.MyValue(const Param: TLyParam);
var
  S: TLyString;
begin
  S := TLyString(Param[0].Data);
  Param.Result.AsString := ExtractValue(S.FData, '=');
end;

procedure TLyStringType.Setup;
begin
  Define(my_char, ['Index'], [my_int],
    {$IFDEF FPC}@{$ENDIF}MyGet, {$IFDEF FPC}@{$ENDIF}MySet);
  Define('Name', Self, {$IFDEF FPC}@{$ENDIF}MyName);
  Define('Value', Self, {$IFDEF FPC}@{$ENDIF}MyValue);
  Method('Clone', Self, {$IFDEF FPC}@{$ENDIF}MyClone);
  inherited;
end;

function TLyStringType.AsBoolean(Obj: pointer): boolean;
begin
  Result := (Obj <> nil) and (TLyString(Obj).Data <> '');
end;

function TLyStringType.AsChar(Obj: pointer): char;
begin
  if (Obj <> nil) and (TLyString(Obj).Data <> '') then
    Result := TLyString(Obj).Data[1] else
    Result := #0;
end;

function TLyStringType.AsFloat(Obj: pointer): double;
begin
  Result := StrToFloat(AsString(Obj));
end;

function TLyStringType.AsInteger(Obj: pointer): int64;
begin
  Result := StrToInt(AsString(Obj));
end;

function TLyStringType.AsCurrency(Obj: pointer): currency;
begin
  Result := StrToCurr(AsString(Obj));
end;

function TLyStringType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyString(Obj).Data else
    Result := '';
end;

function TLyStringType.AsTime(Obj: pointer): TDateTime;
begin
  Result := StrToDateTime(AsString(Obj));
end;

function TLyStringType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyString(Obj).DecRefcount else
    Result := 0;
end;

function TLyStringType.DefValue: pointer;
begin
  Result := TLyString.Create;
end;

procedure TLyStringType.ExecAdd(LValue, RValue: TLyValue);
begin
  LValue.AsString := LValue.AsString + RValue.AsString;
end;

function TLyStringType.Compare(LValue, RValue: TLyValue): TLyCompare;
begin
  if MatchStringType(RValue.VType) then
    Result := CompareString(LValue.AsString, RValue.AsString) else
    Result := crDiff;
end;

procedure TLyStringType.ExecLike(LValue, RValue: TLyValue);
begin
  if MatchStringType(RValue.VType) then
    LValue.AsBoolean := MatchPatten(LValue.AsString, RValue.AsString) else
    inherited;
end;

procedure TLyStringType.ExecMod(LValue, RValue: TLyValue);
var
  F: string;
  L: TLyList;
begin
  F := LValue.AsString;
  if RValue.VType.IsTypeOf(my_array) then
  begin
    L := TLyList(RValue.Data);
    LValue.AsString := L.Format(F);
  end
  else
  begin
    L := TLyList.Create;
    try
      L.Add(RValue);
      LValue.AsString := L.Format(F);
    finally
      L.Free;
    end;
  end;
end;

procedure TLyStringType.ExecMul(LValue, RValue: TLyValue);
var
  I: integer;
  S, T: string;
begin
  if RValue.VType = my_string then
  begin
    S := '';
    T := LValue.AsString;
    if T <> '' then
    begin
      I := RValue.AsInteger;
      while I > 0 do
      begin
        S := S + T;
        Dec(I);
      end;
    end;
    LValue.AsString := S;
  end
  else inherited;
end;

function TLyStringType.Generate(Obj: pointer): TLyGenerate;
var
  S: string;
begin
  S := AsString(Obj);
  if S <> '' then
    Result := TLyStringGenerate.CreateIn(S) else
    Result := nil;
end;

function TLyStringType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyString(Obj).IncRefcount else
    Result := 0;
end;

function TLyStringType.GetLength(Obj: pointer): int64;
begin
  Result := Length(AsString(Obj));
end;

function TLyStringType.Has(Obj: pointer; Value: TLyValue): boolean;
begin
  Result := (Obj <> nil) and
            MatchStringType(Value.VType) and
            (Pos(Value.AsString, TLyString(Obj).Data) > 0);
end;

procedure TLyStringType.Convert(Value: TLyValue);
begin
  if Value.VType <> my_string then
    Value.AsString := Value.AsString;
end;

{ TLyIntegerType }

function TLyIntegerType.AsBoolean(Obj: pointer): boolean;
begin
  if Obj <> nil then
    Result := (TLyInteger(Obj).FData <> 0) else
    Result := false;
end;

function TLyIntegerType.AsChar(Obj: pointer): char;
begin
  if Obj <> nil then
    Result := char(TLyInteger(Obj).FData) else
    Result := #0;
end;

function TLyIntegerType.AsFloat(Obj: pointer): double;
begin
  if Obj <> nil then
    Result := TLyInteger(Obj).FData else
    Result := 0;
end;

function TLyIntegerType.AsInteger(Obj: pointer): int64;
begin
  if Obj <> nil then
    Result := TLyInteger(Obj).FData else
    Result := 0;
end;

function TLyIntegerType.AsCurrency(Obj: pointer): currency;
begin
  if Obj <> nil then
    Result := TLyInteger(Obj).FData else
    Result := 0;
end;

function TLyIntegerType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := IntToStr(TLyInteger(Obj).FData) else
    Result := '0';
end;

function TLyIntegerType.AsTime(Obj: pointer): TDateTime;
begin
  if Obj <> nil then
    Result := TLyInteger(Obj).FData else
    Result := 0;
end;

function TLyIntegerType.Generate(Obj: pointer): TLyGenerate;
var
  N: int64;
begin
  N := AsInteger(Obj);
  if N > 0 then
    Result := TLyIntGenerate.CreateIn(N) else
    Result := nil;
end;

function TLyIntegerType.Has(Obj: pointer; Value: TLyValue): boolean;
var
  H, I: int64;
begin
  Result := (Obj <> nil) and (Value.VType = my_int);
  if Result then
  begin
    H := TLyInteger(Obj).FData;
    I := AsInteger(Value.Data);
    Result := (I >= 0) and (I < H);
  end;
end;

procedure TLyIntegerType.Convert(Value: TLyValue);
begin
  if Value.VType <> Self then
    Value.AsInteger := Value.AsInteger;
end;

function TLyIntegerType.DefValue: pointer;
begin
  Result := my_integer_zero;
end;

procedure TLyIntegerType.ExecAdd(LValue, RValue: TLyValue);
var
  T: TLyType;
begin
  T := RValue.VType;
  if T = my_int then
    LValue.AsInteger := LValue.AsInteger + RValue.AsInteger else
  if T = my_float then
    LValue.AsFloat := LValue.AsInteger + RValue.AsFloat else
  if T = my_curr then
    LValue.AsCurrency := LValue.AsInteger + RValue.AsCurrency else
    inherited;
end;

function TLyIntegerType.Compare(LValue, RValue: TLyValue): TLyCompare;
var
  T: TLyType;
begin
  T := RValue.VType;
  if T = my_int then
    Result := CompareInt64(LValue.AsInteger, RValue.AsInteger) else
  if T = my_float then
    Result := CompareFloat(LValue.AsInteger, RValue.AsFloat) else
  if T = my_curr then
    Result := CompareMoney(LValue.AsInteger, RValue.AsCurrency) else
  if T = my_time then
    Result := CompareFloat(LValue.AsInteger, RValue.AsTime) else
    Result := crDiff;
end;

procedure TLyIntegerType.ExecDec(LValue, RValue: TLyValue);
var
  T: TLyType;
begin
  T := RValue.VType;
  if T = my_int then
    LValue.AsInteger := LValue.AsInteger - RValue.AsInteger else
  if T = my_float then
    LValue.AsFloat := LValue.AsInteger - RValue.AsFloat else
  if T = my_curr then
    LValue.AsCurrency := LValue.AsInteger - RValue.AsCurrency else
    inherited;
end;

procedure TLyIntegerType.ExecDiv(LValue, RValue: TLyValue);
begin
  if RValue.VType = my_int then
    LValue.AsInteger := LValue.AsInteger div RValue.AsInteger else
    inherited;
end;

procedure TLyIntegerType.ExecDivf(LValue, RValue: TLyValue);
var
  T: TLyType;
begin
  T := RValue.VType;
  if T = my_int then
    LValue.AsFloat := LValue.AsInteger / RValue.AsInteger else
  if T = my_float then
    LValue.AsFloat := LValue.AsInteger / RValue.AsFloat else
  if T = my_curr then
    LValue.AsFloat := LValue.AsInteger / RValue.AsCurrency else
    inherited;
end;

procedure TLyIntegerType.ExecMod(LValue, RValue: TLyValue);
begin
  if RValue.VType = my_int then
    LValue.AsInteger := LValue.AsInteger mod RValue.AsInteger else
    inherited;
end;

procedure TLyIntegerType.ExecMul(LValue, RValue: TLyValue);
var
  T: TLyType;
begin
  T := RValue.VType;
  if T = my_int then
    LValue.AsInteger := LValue.AsInteger * RValue.AsInteger else
  if T = my_float then
    LValue.AsFloat := LValue.AsInteger * RValue.AsFloat else
  if T = my_curr then
    LValue.AsCurrency := LValue.AsInteger * RValue.AsCurrency else
    inherited;
end;

procedure TLyIntegerType.ExecNeg(Value: TLyValue);
begin
  Value.Assign(Self, GetIntegerData(- AsInteger(Value.Data)));
end;

procedure TLyIntegerType.ExecNot(Value: TLyValue);
begin
  Value.Assign(Self, GetIntegerData(not AsInteger(Value.Data)));
end;

procedure TLyIntegerType.ExecShl(LValue, RValue: TLyValue);
begin
  if RValue.VType = my_int then
    LValue.AsInteger := LValue.AsInteger shl RValue.AsInteger else
    inherited;
end;

procedure TLyIntegerType.ExecShr(LValue, RValue: TLyValue);
begin
  if RValue.VType = my_int then
    LValue.AsInteger := LValue.AsInteger shr RValue.AsInteger else
    inherited;
end;

procedure TLyIntegerType.ExecXor(LValue, RValue: TLyValue);
begin
  if RValue.VType = my_int then
    LValue.AsInteger := LValue.AsInteger xor RValue.AsInteger else
    inherited;
end;

{ TLyBooleanType }

function TLyBooleanType.AsBoolean(Obj: pointer): boolean;
begin
  Result := (Obj <> nil) and TLyBoolean(Obj).FData;
end;

function TLyBooleanType.AsInteger(Obj: pointer): int64;
begin
  if AsBoolean(Obj) then Result := 1 else Result := 0;
end;

function TLyBooleanType.AsString(Obj: pointer): string;
begin
  Result := my_boolean_strs[AsBoolean(Obj)];
end;

procedure TLyBooleanType.Convert(Value: TLyValue);
begin
  if Value.VType <> Self then
    Value.AsBoolean := Value.AsBoolean;
end;

function TLyBooleanType.DefValue: pointer;
begin
  Result := my_boolean_datas[false];
end;

function TLyBooleanType.Compare(LValue, RValue: TLyValue): TLyCompare;
begin
  if RValue.VType = my_bool then
    Result := CompareInt64(LValue.AsInteger, RValue.AsInteger) else
    Result := crDiff;
end;

procedure TLyBooleanType.ExecNot(Value: TLyValue);
begin
  Value.Assign(Self, GetBooleanData(not AsBoolean(Value.Data)));
end;

procedure TLyBooleanType.ExecXor(LValue, RValue: TLyValue);
begin
  if RValue.VType = my_bool then
    LValue.AsBoolean := LValue.AsBoolean xor RValue.AsBoolean else
    inherited;
end;

{ TLyCharType }

function TLyCharType.AsBoolean(Obj: pointer): boolean;
begin
  Result := (AsChar(Obj) <> #0);
end;

function TLyCharType.AsChar(Obj: pointer): char;
begin
  if Obj <> nil then
    Result := TLyChar(Obj).FData else
    Result := #0;
end;

function TLyCharType.AsFloat(Obj: pointer): double;
begin
  Result := AsInteger(Obj);
end;

function TLyCharType.AsInteger(Obj: pointer): int64;
begin
  Result := StrToInt(AsString(Obj));
end;

function TLyCharType.AsCurrency(Obj: pointer): currency;
begin
  Result := AsInteger(Obj);
end;

function TLyCharType.AsString(Obj: pointer): string;
begin
  if (Obj <> nil) and (TLyChar(Obj).FData <> #0) then
    Result := TLyChar(Obj).FData else
    Result := '';
end;

procedure TLyCharType.Convert(Value: TLyValue);
begin
  if Value.VType <> Self then
    Value.AsChar := Value.AsChar;
end;

function TLyCharType.DefValue: pointer;
begin
  Result := my_char_datas[#0];
end;

procedure TLyCharType.ExecAdd(LValue, RValue: TLyValue);
begin
  LValue.AsString := LValue.AsString + RValue.AsString;
end;

function TLyCharType.Compare(LValue, RValue: TLyValue): TLyCompare;
begin
  if RValue.VType = my_string then
    Result := CompareString(LValue.AsString, RValue.AsString) else
  if RValue.VType = my_char then
    Result := CompareChar(LValue.AsChar, RValue.AsChar) else
    Result := crDiff;
end;

procedure TLyCharType.ExecLike(LValue, RValue: TLyValue);
var
  L, R: string;
begin
  if MatchStringType(RValue.VType) then
  begin
    L := AsString(LValue.Data);
    R := RValue.AsString;
    LValue.AsBoolean := MatchPatten(L, R);
  end
  else inherited;
end;

procedure TLyCharType.ExecMul(LValue, RValue: TLyValue);
var
  I: integer;
  S, T: string;
begin
  if RValue.VType = my_string then
  begin
    S := '';
    T := LValue.AsString;
    if T <> '' then
    begin
      I := RValue.AsInteger;
      while I > 0 do
      begin
        S := S + T;
        Dec(I);
      end;
    end;
    LValue.AsString := S;
  end
  else inherited;
end;

{ TLyFloatType }

function TLyFloatType.AsBoolean(Obj: pointer): boolean;
begin
  Result := (Obj <> nil) and not IsZero(TLyFloat(Obj).FData);
end;

function TLyFloatType.AsFloat(Obj: pointer): double;
begin
  if Obj <> nil then
    Result := TLyFloat(Obj).FData else
    Result := 0;
end;

function TLyFloatType.AsInteger(Obj: pointer): int64;
begin
  Result := Trunc(AsFloat(Obj));
end;

function TLyFloatType.AsCurrency(Obj: pointer): currency;
begin
  Result := AsFloat(Obj);
end;

function TLyFloatType.AsString(Obj: pointer): string;
begin
  Result := FloatToStr(AsFloat(Obj));
end;

function TLyFloatType.AsTime(Obj: pointer): TDateTime;
begin
  Result := AsFloat(Obj);
end;

procedure TLyFloatType.Convert(Value: TLyValue);
begin
  if Value.VType <> Self then
    Value.AsFloat := Value.AsFloat;
end;

function TLyFloatType.DefValue: pointer;
begin
  Result := my_float_zero;
end;

procedure TLyFloatType.ExecAdd(LValue, RValue: TLyValue);
var
  T: TLyType;
begin
  T := RValue.VType;
  if T = my_int then
    LValue.AsFloat := LValue.AsFloat + RValue.AsInteger else
  if T = my_float then
    LValue.AsFloat := LValue.AsFloat + RValue.AsFloat else
  if T = my_curr then
    LValue.AsFloat := LValue.AsFloat + RValue.AsCurrency else
    inherited;
end;

function TLyFloatType.Compare(LValue, RValue: TLyValue): TLyCompare;
var
  T: TLyType;
begin
  T := RValue.VType;
  if T = my_int then
    Result := CompareFloat(LValue.AsFloat, RValue.AsInteger) else
  if T = my_float then
    Result := CompareFloat(LValue.AsFloat, RValue.AsFloat) else
  if T = my_curr then
    Result := CompareFloat(LValue.AsFloat, RValue.AsCurrency) else
  if T = my_time then
    Result := CompareFloat(LValue.AsFloat, RValue.AsTime) else
    Result := crDiff;
end;

procedure TLyFloatType.ExecDec(LValue, RValue: TLyValue);
var
  T: TLyType;
begin
  T := RValue.VType;
  if T = my_int then
    LValue.AsFloat := LValue.AsFloat - RValue.AsInteger else
  if T = my_float then
    LValue.AsFloat := LValue.AsFloat - RValue.AsFloat else
  if T = my_curr then
    LValue.AsFloat := LValue.AsFloat - RValue.AsCurrency else
    inherited;
end;

procedure TLyFloatType.ExecDivf(LValue, RValue: TLyValue);
var
  T: TLyType;
begin
  T := RValue.VType;
  if T = my_int then
    LValue.AsFloat := LValue.AsFloat / RValue.AsInteger else
  if T = my_float then
    LValue.AsFloat := LValue.AsFloat / RValue.AsFloat else
  if T = my_curr then
    LValue.AsFloat := LValue.AsFloat / RValue.AsCurrency else
    inherited;
end;

procedure TLyFloatType.ExecMul(LValue, RValue: TLyValue);
var
  T: TLyType;
begin
  T := RValue.VType;
  if T = my_int then
    LValue.AsFloat := LValue.AsFloat * RValue.AsInteger else
  if T = my_float then
    LValue.AsFloat := LValue.AsFloat * RValue.AsFloat else
  if T = my_curr then
    LValue.AsCurrency := LValue.AsFloat * RValue.AsCurrency else
    inherited;
end;

procedure TLyFloatType.ExecNeg(Value: TLyValue);
begin
  Value.Assign(Self, GetFloatData(- AsFloat(Value.Data)));
end;

{ TLyCurrencyType }

function TLyCurrencyType.AsBoolean(Obj: pointer): boolean;
begin
  Result := (AsCurrency(Obj) <> 0);
end;

function TLyCurrencyType.AsFloat(Obj: pointer): double;
begin
  Result := AsCurrency(Obj);
end;

function TLyCurrencyType.AsInteger(Obj: pointer): int64;
begin
  Result := Trunc(AsCurrency(Obj));
end;

function TLyCurrencyType.AsCurrency(Obj: pointer): currency;
begin
  if Obj <> nil then
    Result := TLyCurrency(Obj).FData else
    Result := 0;
end;

function TLyCurrencyType.AsString(Obj: pointer): string;
begin
  Result := CurrToStr(AsCurrency(Obj));
end;

function TLyCurrencyType.AsTime(Obj: pointer): TDateTime;
begin
  Result := AsCurrency(Obj);
end;

procedure TLyCurrencyType.Convert(Value: TLyValue);
begin
  if Value.VType <> Self then
    Value.AsCurrency := Value.AsCurrency;
end;

function TLyCurrencyType.DefValue: pointer;
begin
  Result := my_currency_zero;
end;

procedure TLyCurrencyType.ExecAdd(LValue, RValue: TLyValue);
var
  T: TLyType;
begin
  T := RValue.VType;
  if T = my_int then
    LValue.AsCurrency := LValue.AsCurrency + RValue.AsInteger else
  if T = my_float then
    LValue.AsCurrency := LValue.AsCurrency + RValue.AsFloat else
  if T = my_curr then
    LValue.AsCurrency := LValue.AsCurrency + RValue.AsCurrency else
    inherited;
end;

function TLyCurrencyType.Compare(LValue, RValue: TLyValue): TLyCompare;
var
  T: TLyType;
begin
  T := RValue.VType;
  if T = my_int then
    Result := CompareMoney(LValue.AsCurrency, RValue.AsInteger) else
  if T = my_float then
    Result := CompareFloat(LValue.AsCurrency, RValue.AsFloat) else
  if T = my_curr then
    Result := CompareMoney(LValue.AsCurrency, RValue.AsCurrency) else
  if T = my_time then
    Result := CompareFloat(LValue.AsCurrency, RValue.AsTime) else
    Result := crDiff;
end;

procedure TLyCurrencyType.ExecDec(LValue, RValue: TLyValue);
var
  T: TLyType;
begin
  T := RValue.VType;
  if T = my_int then
    LValue.AsCurrency := LValue.AsCurrency - RValue.AsInteger else
  if T = my_float then
    LValue.AsCurrency := LValue.AsCurrency - RValue.AsFloat else
  if T = my_curr then
    LValue.AsCurrency := LValue.AsCurrency - RValue.AsCurrency else
    inherited;
end;

procedure TLyCurrencyType.ExecDivf(LValue, RValue: TLyValue);
var
  T: TLyType;
begin
  T := RValue.VType;
  if T = my_int then
    LValue.AsCurrency := LValue.AsCurrency / RValue.AsInteger else
  if T = my_float then
    LValue.AsCurrency := LValue.AsCurrency / RValue.AsFloat else
  if T = my_curr then
    LValue.AsCurrency := LValue.AsCurrency / RValue.AsCurrency else
    inherited;
end;

procedure TLyCurrencyType.ExecMul(LValue, RValue: TLyValue);
var
  T: TLyType;
begin
  T := RValue.VType;
  if T = my_int then
    LValue.AsCurrency := LValue.AsCurrency * RValue.AsInteger else
  if T = my_float then
    LValue.AsCurrency := LValue.AsCurrency * RValue.AsFloat else
  if T = my_curr then
    LValue.AsCurrency := LValue.AsCurrency * RValue.AsCurrency else
    inherited;
end;

procedure TLyCurrencyType.ExecNeg(Value: TLyValue);
begin
  Value.Assign(Self, GetCurrencyData(- AsCurrency(Value.Data)));
end;

{ TLyTimeType }

function TLyTimeType.AsBoolean(Obj: pointer): boolean;
begin
  Result := (Obj <> nil);
end;

function TLyTimeType.AsCurrency(Obj: pointer): currency;
begin
  Result := AsTime(Obj);
end;

function TLyTimeType.AsFloat(Obj: pointer): double;
begin
  Result := AsTime(Obj);
end;

function TLyTimeType.AsInteger(Obj: pointer): int64;
begin
  Result := Trunc(AsTime(Obj));
end;

function TLyTimeType.AsString(Obj: pointer): string;
begin
  Result := DateTimeToStr(AsTime(Obj));
end;

function TLyTimeType.AsTime(Obj: pointer): TDateTime;
begin
  if Obj <> nil then
    Result := TLyTime(Obj).FData else
    Result := 0;
end;

procedure TLyTimeType.Convert(Value: TLyValue);
begin
  if Value.VType <> Self then
    Value.AsTime := Value.AsTime;
end;

function TLyTimeType.DefValue: pointer;
begin
  Result := my_time_zero;
end;

procedure TLyTimeType.ExecAdd(LValue, RValue: TLyValue);
var
  T: TLyType;
begin
  T := RValue.VType;
  if T = my_int then
    LValue.AsTime := LValue.AsTime + RValue.AsInteger else
  if T = my_float then
    LValue.AsTime := LValue.AsTime + RValue.AsFloat else
  if T = my_curr then
    LValue.AsTime := LValue.AsTime + RValue.AsCurrency else
    inherited;
end;

function TLyTimeType.Compare(LValue, RValue: TLyValue): TLyCompare;
var
  T: TLyType;
begin
  T := RValue.VType;
  if T = my_int then
    Result := CompareFloat(LValue.AsTime, RValue.AsInteger) else
  if T = my_float then
    Result := CompareFloat(LValue.AsTime, RValue.AsFloat) else
  if T = my_curr then
    Result := CompareFloat(LValue.AsTime, RValue.AsCurrency) else
  if T = my_time then
    Result := CompareFloat(LValue.AsTime, RValue.AsTime) else
    Result := crDiff;
end;

procedure TLyTimeType.ExecDec(LValue, RValue: TLyValue);
var
  T: TLyType;
begin
  T := RValue.VType;
  if T = my_int then
    LValue.AsTime := LValue.AsTime - RValue.AsInteger else
  if T = my_float then
    LValue.AsTime := LValue.AsTime - RValue.AsFloat else
  if T = my_curr then
    LValue.AsTime := LValue.AsTime - RValue.AsCurrency else
    inherited;
end;

{ TLyArrayType }

procedure TLyArrayType.MarkForSurvive(Obj: pointer);
begin
  if Obj <> nil then TLyList(Obj).MarkForSurvive;
end;

procedure TLyArrayType.MyCopy(const Param: TLyParam);
var
  L: TLyList;
  X: integer;
  N: integer;
begin
  if Param.GetSelf(L) then
  begin
    X := Param[1].AsInteger;
    N := Param[2].AsInteger;
    Param.Result.Assign(Self, L.Copy(X, N));
  end;
end;

procedure TLyArrayType.MyGet(const Param: TLyParam);
var
  L: TLyList;
  X: TLyValue;
begin
  if Param.GetSelf(L) then
  begin
    X := Param[1];
    if MatchIntType(X.VType) then
      Param.Result.Assign(L[Param[1].AsInteger]) else
    if MatchStringType(X.VType) then
      Param.Result.Assign(L.Values[X.AsString]) else
      Param.ErrorIndexType(X.VType);
  end;
end;

procedure TLyArrayType.MyGetCount(const Param: TLyParam);
var
  L: TLyList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.Count;
end;

procedure TLyArrayType.MyIndexOf(const Param: TLyParam);
var
  L: TLyList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.IndexOf(Param[1]);
end;

procedure TLyArrayType.MyIndexOfName(const Param: TLyParam);
var
  L: TLyList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.IndexOf(Param[1].AsString);
end;

procedure TLyArrayType.MyIsEmpty(const Param: TLyParam);
var
  L: TLyList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsBoolean := (L.Count = 0);
end;

procedure TLyArrayType.MyLeft(const Param: TLyParam);
var
  L: TLyList;
  N: integer;
begin
  if Param.GetSelf(L) then
  begin
    N := Param[1].AsInteger;
    Param.Result.Assign(Self, L.CopyLeft(N));
  end;
end;

procedure TLyArrayType.MyGetName(const Param: TLyParam);
var
  L: TLyList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.Names[Param[1].AsInteger];
end;

procedure TLyArrayType.MyRight(const Param: TLyParam);
var
  L: TLyList;
  N: integer;
begin
  if Param.GetSelf(L) then
  begin
    N := Param[1].AsInteger;
    Param.Result.Assign(Self, L.CopyRight(N));
  end;
end;

function TLyArrayType.CreateInstance: pointer;
begin
  Result := TLyList.Create;
end;

procedure TLyArrayType.Setup;
begin
  Method('IsEmpty', my_bool,
         {$IFDEF FPC}@{$ENDIF}MyIsEmpty);
  Method('IndexOf', my_int,
         ['Value'], [my_variant],
         {$IFDEF FPC}@{$ENDIF}MyIndexOf);
  Method('IndexOfName', my_int,
         ['Name'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyIndexOfName);
  Method('Copy', Self,
         ['Index', 'Count'], [my_int, my_int],
         {$IFDEF FPC}@{$ENDIF}MyCopy);
  Method('Left', Self,
         ['Count'], [my_int],
         {$IFDEF FPC}@{$ENDIF}MyLeft);
  Method('Right', Self,
         ['Count'], [my_int],
         {$IFDEF FPC}@{$ENDIF}MyRight);
  if Self = my_array then
  begin
    Define('Count', my_int, {$IFDEF FPC}@{$ENDIF}MyGetCount);
    Define(my_variant, ['Index'], [my_variant], {$IFDEF FPC}@{$ENDIF}MyGet);
    Method('Name', my_string, ['Index'], [my_int],
      {$IFDEF FPC}@{$ENDIF}MyGetName);
  end;
  inherited;
end;

function TLyArrayType.Add(Obj: pointer; Value: TLyValue): integer;
var
  L: TLyList;
begin
  Validate(Obj);
  L := TLyList(Obj);
  L.TestChange;
  Result := L.Count;
  L.Add(Value);
end;

function TLyArrayType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyList(Obj).ToString else
    Result := '';
end;

function TLyArrayType.Clear(Obj: pointer): boolean;
begin
  Result := (Obj <> nil);
  if Result then
  begin
    TLyList(Obj).TestChange;
    TLyList(Obj).Clear;
  end;
end;

function TLyArrayType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyList(Obj).DecRefcount else
    Result := 0;
end;

procedure TLyArrayType.ExecAdd(LValue, RValue: TLyValue);
var
  A, L: TLyList;
  I: integer;
begin
  if RValue.VType = Self then
  begin
    A := TLyList.Create;
    try
      L := TLyList(LValue.Data);
      for I := 0 to L.Count - 1 do
        A.Add(L[I]);
      L := TLyList(RValue.Data);
      for I := 0 to L.Count - 1 do
        A.Add(L[I]);
    finally
      LValue.Assign(Self, A);
    end;
  end
  else inherited;
end;

procedure TLyArrayType.ExecDec(LValue, RValue: TLyValue);
var
  A, L, R: TLyList;
  I: integer;
begin
  if RValue.VType = Self then
  begin
    A := TLyList.Create;
    try
      L := TLyList(LValue.Data);
      R := TLyList(RValue.Data);
      for I := 0 to L.Count - 1 do
        if (R = nil) or (R.IndexOf(L[I]) < 0) then
          if A.IndexOf(L[I]) < 0 then
            A.Add(L[I]);
    finally
      LValue.Assign(Self, A);
    end;
  end
  else inherited;
end;

procedure TLyArrayType.ExecMul(LValue, RValue: TLyValue);
var
  A, L, R: TLyList;
  I: integer;
begin
  if RValue.VType = Self then
  begin
    A := TLyList.Create;
    try
      R := TLyList(RValue.Data);
      if R <> nil then
      begin
        L := TLyList(LValue.Data);
        for I := 0 to L.Count - 1 do
          if R.IndexOf(L[I]) >= 0 then
            if A.IndexOf(L[I]) < 0 then
              A.Add(L[I]);
      end;
    finally
      LValue.Assign(RValue.VType, A);
    end;
  end
  else inherited;
end;

function TLyArrayType.Generate(Obj: pointer): TLyGenerate;
begin
  if (Obj <> nil) and (TLyList(Obj).Count > 0) then
    Result := TLyListGenerate.CreateIn(TLyList(Obj)) else
    Result := nil;
end;

function TLyArrayType.GetLength(Obj: pointer): int64;
begin
  if Obj <> nil then
    Result := TLyList(Obj).Count else
    Result := 0;
end;

function TLyArrayType.Has(Obj: pointer; Value: TLyValue): boolean;
begin
  Result := (Obj <> nil) and (TLyList(Obj).IndexOf(Value) >= 0);
end;

function TLyArrayType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyList(Obj).IncRefcount else
    Result := 0;
end;

function TLyArrayType.InstanceClass: TClass;
begin
  Result := TLyList;
end;

{ TLyListType }

procedure TLyListType.MyAdd(const Param: TLyParam);
var
  L: TLyList;
begin
  if Param.GetSelf(L) then
  begin
    L.Add(Param[1]);
    Param.Result.AsInteger := L.Count - 1;
  end;
end;

procedure TLyListType.MyAssign(const Param: TLyParam);
var
  L: TLyList;
begin
  if Param.GetSelf(L) then
    L.Assign(Param.Params[1]);
end;

procedure TLyListType.MyClear(const Param: TLyParam);
var
  L: TLyList;
begin
  if Param.GetSelf(L) then L.Clear;
end;

procedure TLyListType.MyDelete(const Param: TLyParam);
var
  L: TLyList;
begin
  if Param.GetSelf(L) then
    L.Delete(Param[1].AsInteger);
end;

procedure TLyListType.MyExchange(const Param: TLyParam);
var
  L: TLyList;
begin
  if Param.GetSelf(L) then
    L.Exchange(Param[1].AsInteger, Param[2].AsInteger);
end;

procedure TLyListType.MyInsert(const Param: TLyParam);
var
  L: TLyList;
begin
  if Param.GetSelf(L) then
    L.Insert(Param[1].AsInteger, Param[2]);
end;

procedure TLyListType.MyMove(const Param: TLyParam);
var
  L: TLyList;
begin
  if Param.GetSelf(L) then
    L.Move(Param[1].AsInteger, Param[2].AsInteger);
end;

procedure TLyListType.MyRemove(const Param: TLyParam);
var
  L: TLyList;
begin
  if Param.GetSelf(L) then
    L.Remove(Param[1]);
end;

procedure TLyListType.MySet(const Param: TLyParam);
var
  L: TLyList;
  X: TLyValue;
begin
  if Param.GetSelf(L) then
  begin
    X := Param[1];
    if MatchIntType(X.VType) then
      L[X.AsInteger].Assign(Param[2]) else
    if MatchStringType(X.VType) then
      L.Put(X.AsString, Param[2]) else
      Param.ErrorIndexType(X.VType);
  end;
end;

procedure TLyListType.MySetCount(const Param: TLyParam);
var
  L: TLyList;
begin
  if Param.GetSelf(L) then
    L.Count := Param[1].AsInteger;
end;

procedure TLyListType.MySort(const Param: TLyParam);
var
  L: TLyList;
begin
  if Param.GetSelf(L) then
    L.Sort;
end;

procedure TLyListType.Setup;
begin
  Parent := my_array;

  Method(LSE_CREATE, Self, {$IFDEF FPC}@{$ENDIF}MyCreate);
  Method('Assign', ['Source'], [my_variant], {$IFDEF FPC}@{$ENDIF}MyAssign);
  Method('Clear', {$IFDEF FPC}@{$ENDIF}MyClear);
  Method('Delete', ['Index'], [my_int], {$IFDEF FPC}@{$ENDIF}MyDelete);
  Method('Remove', ['Value'], [my_variant], {$IFDEF FPC}@{$ENDIF}MyRemove);
  Method('Exchange', ['X1', 'X2'], [my_int, my_int],
         {$IFDEF FPC}@{$ENDIF}MyExchange);
  Method('Move', ['FromX', 'ToX'], [my_int, my_int],
         {$IFDEF FPC}@{$ENDIF}MyMove);
  Method('Sort', {$IFDEF FPC}@{$ENDIF}MySort);
  Method('Insert', ['X', '_Value'], [my_int, my_variant],
         {$IFDEF FPC}@{$ENDIF}MyInsert);
  Method('Add', my_int, ['_Value'], [my_variant],
         {$IFDEF FPC}@{$ENDIF}MyAdd);
  Define('Count', my_int,
         {$IFDEF FPC}@{$ENDIF}MyGetCount,
         {$IFDEF FPC}@{$ENDIF}MySetCount);
  Define(my_variant,
         ['Index'], [my_variant],
         {$IFDEF FPC}@{$ENDIF}MyGet,
         {$IFDEF FPC}@{$ENDIF}MySet);
  inherited;
end;

{ TLyHashItem }

function TLyHashItem.Format: string;
begin
  Result := FormatString(FKey) + ':' + FormatValue(Self);
end;

{ TLyHash }

function TLyHash.Add(const Name: string): TLyHashItem;
var
  I: integer;
begin
  Result := Get(Name);
  if Result = nil then
  begin
    Result := TLyHashItem.Create;
    Result.FKey := Name;
    I := HashIndex(Result.FKey);
    Result.FNext := FBuckets[I];
    FBuckets[I] := Result;
    Inc(FCount);
    Resize(FCount div LSE_HASH_DELTA);
  end;
end;

function TLyHash.DefConst(const Name, Value: string): TLyHashItem;
begin
  Result := Add(Name);
  Result.AsString := Value;
end;

function TLyHash.DefConst(const Name: string; Value: int64): TLyHashItem;
begin
  Result := Add(Name);
  Result.AsInteger := Value;
end;

function TLyHash.DefConst(const Name: string; Value: double): TLyHashItem;
begin
  Result := Add(Name);
  Result.AsFloat := Value;
end;

function TLyHash.DefConst(const Name: string; Value: boolean): TLyHashItem;
begin
  Result := Add(Name);
  Result.AsBoolean := Value;
end;

function TLyHash.BucketCount: integer;
begin
  Result := Length(FBuckets);
end;

procedure TLyHash.Clear;
var
  I: integer;
  H: TLyHashItem;
begin
  for I := 0 to BucketCount - 1 do
    while FBuckets[I] <> nil do
    begin
      H := FBuckets[I];
      FBuckets[I] := H.FNext;
      FreeItem(H);
    end;
  SetLength(FBuckets, 1);
  inherited;
end;

constructor TLyHash.Create;
begin
  inherited;
  SetLength(FBuckets, 1);
  FBuckets[0] := nil;
end;

destructor TLyHash.Destroy;
begin
  Clear;
  SetLength(FBuckets, 0);
  inherited;
end;

procedure TLyHash.Each(AProc: TLyEachHashItem);
var
  I: integer;
  H: TLyHashItem;
begin
  if Assigned(AProc) then
    for I := 0 to BucketCount - 1 do
    begin
      H := FBuckets[I];
      while H <> nil do
      begin
        AProc(H);
        H := H.FNext;
      end;
    end;
end;

procedure TLyHash.FreeItem(H: TLyHashItem);
begin
  Dec(FCount);
  H.Free;
end;

function TLyHash.Has(const Name: string): boolean;
begin
  Result := (Get(Name) <> nil);
end;

function TLyHash.HashIndex(const Name: string): integer;
var
  N: integer;
begin
  N := BucketCount;
  if N = 1 then Result := 0 else
  if FCaseSensitive then
    Result := (basic.HashOf(Name) mod N) else
    Result := (basic.HashOf(LowerCase(Name)) mod N);
end;

function TLyHash.IsEmpty: boolean;
begin
  Result := (FCount = 0);
end;

function TLyHash.Get(const Name: string): TLyHashItem;
begin
  Result := FBuckets[HashIndex(Name)];
  while Result <> nil do
  begin
    if MatchName(Name, Result.FKey) then Exit;
    Result := Result.FNext;
  end;
end;

function TLyHash.GetValue(const Name: string; Value: TLyValue): boolean;
var
  V: TLyValue;
begin
  V := Get(Name);
  Result := (V <> nil);
  if Result and (Value <> nil) then
    Value.Assign(V);
end;

procedure TLyHash.ListKeys(List: TLyList);
var
  I: integer;
  L: TList;
begin
  if Self <> nil then
  begin
    L := SetupItemList;
    try
      for I := 0 to L.Count - 1 do
        List.Add.AsString := TLyHashItem(L[I]).FKey;
    finally
      L.Free;
    end;
  end;
end;

procedure TLyHash.ListValues(List: TLyList);
var
  I: integer;
  L: TList;
begin
  if Self <> nil then
  begin
    L := SetupItemList;
    try
      for I := 0 to L.Count - 1 do
        List.Add(TLyHashItem(L[I]));
    finally
      L.Free;
    end;
  end;
end;

procedure TLyHash.MarkForSurvive;
var
  H: TLyHashItem;
  I: integer;
begin
  if (Self <> nil) and not Survived then
  begin
    Survived := true;
    for I := 0 to BucketCount - 1 do
    begin
      H := FBuckets[I];
      while H <> nil do
      begin
        H.MarkForSurvive;
        H := H.FNext;
      end;
    end;
  end;
end;

function TLyHash.MatchName(const N1, N2: string): boolean;
begin
  if FCaseSensitive then
    Result := (N1 = N2) else
    Result := SysUtils.SameText(N1, N2);
end;

function TLyHash.Remove(const Name: string): boolean;
var
  X: cardinal;
  H, T: TLyHashItem;
begin
  Result := false;
  X := HashIndex(Name);
  H := FBuckets[X];
  if MatchName(Name, H.FKey) then
  begin
    FBuckets[X] := H.FNext;
    FreeItem(H);
    Result := true;
  end
  else
  while H.FNext <> nil do
  begin
    T := H.FNext;
    if MatchName(Name, T.FKey) then
    begin
      H.FNext := T.FNext;
      FreeItem(T);
      Result := true;
      Exit;
    end;
    H := T;
  end;
end;

procedure TLyHash.Resize(NewSize: integer);
var
  L: TList;
  I, X: integer;
  H: TLyHashItem;
begin
  if NewSize > 512 then NewSize := 512;
  X := BucketCount;
  if NewSize > X then
  begin
    L := SetupItemList;
    try
      while X < NewSize do X := X shl 1;
      SetLength(FBuckets, X);
      FillChar(FBuckets[0], sizeof(TLyHashItem) * X, 0);
      for I := 0 to L.Count - 1 do
      begin
        H := TLyHashItem(L[I]);
        X := HashIndex(H.FKey);
        H.FNext := FBuckets[X];
        FBuckets[X] := H;
      end;
    finally
      L.Free;
    end;
  end;
end;

function TLyHash.SetupItemList: TList;
var
  I: integer;
  H: TLyHashItem;
begin
  Result := TList.Create;
  for I := 0 to BucketCount - 1 do
  begin
    H := FBuckets[I];
    while H <> nil do
    begin
      Result.Add(H);
      H := H.FNext;
    end;
  end;
end;

function TLyHash.ToString: string;
var
  H: TLyHashItem;
  L: TList;
  I: integer;
begin
  if not FFormating then
  begin
    FFormating := true;
    try
      L := SetupItemList;
      try
        if L.Count > 0 then
        begin
          H := TLyHashItem(L.First);
          Result := '[' + H.Format;
          for I := 1 to L.Count - 1 do
          begin
            H := TLyHashItem(L[I]);
            Result := Result + ', ' + H.Format;
          end;
          Result := Result + ']';
        end
        else Result := '[:]';
      finally
        L.Free;
      end;
    finally
      FFormating := false;
    end;
  end
  else Result := '[:::]';
end;

{ TLyHashType }

procedure TLyHashType.MyClear(const Param: TLyParam);
var
  H: TLyHash;
begin
  if Param.GetSelf(H) then H.Clear;
end;

procedure TLyHashType.MyGet(const Param: TLyParam);
var
  H: TLyHash;
  V: TLyValue;
begin
  if Param.GetSelf(H) then
  begin
    V := H.Get(Param[1].AsString);
    if V <> nil then
      Param.Result.Assign(V);
  end;
end;

procedure TLyHashType.MyHas(const Param: TLyParam);
var
  H: TLyHash;
begin
  if Param.GetSelf(H) then
    Param.Result.AsBoolean := H.Has(Param[1].AsString);
end;

procedure TLyHashType.MyIsEmpty(const Param: TLyParam);
var
  H: TLyHash;
begin
  if Param.GetSelf(H) then
    Param.Result.AsBoolean := H.IsEmpty;
end;

procedure TLyHashType.MyKeys(const Param: TLyParam);
var
  H: TLyHash;
  L: TLyList;
begin
  if Param.GetSelf(H) then
  begin
    L := TLyList.Create;
    Param.Result.Assign(my_array, L);
    H.ListKeys(L);
  end;
end;

procedure TLyHashType.MyRemove(const Param: TLyParam);
var
  H: TLyHash;
begin
  if Param.GetSelf(H) then
    H.Remove(Param[1].AsString);
end;

procedure TLyHashType.MySet(const Param: TLyParam);
var
  H: TLyHash;
begin
  if Param.GetSelf(H) then
    H.Add(Param[1].AsString).Assign(Param[2]);
end;

procedure TLyHashType.MyValues(const Param: TLyParam);
var
  H: TLyHash;
  L: TLyList;
begin
  if Param.GetSelf(H) then
  begin
    L := TLyList.Create;
    Param.Result.Assign(my_array, L);
    H.ListValues(L);
  end;
end;

procedure TLyHashType.Setup;
begin
  Method(LSE_CREATE, Self, {$IFDEF FPC}@{$ENDIF}MyCreate);
  Method('IsEmpty', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsEmpty);
  Method('Clear', {$IFDEF FPC}@{$ENDIF}MyClear);
  Method('Has', my_bool, ['Key'], [my_string], {$IFDEF FPC}@{$ENDIF}MyHas);
  Method('Remove', ['Key'], [my_string], {$IFDEF FPC}@{$ENDIF}MyRemove);
  Method('Keys', my_array, {$IFDEF FPC}@{$ENDIF}MyKeys);
  Method('Values', my_array, {$IFDEF FPC}@{$ENDIF}MyValues);
  Define(my_variant, ['Key'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyGet,
         {$IFDEF FPC}@{$ENDIF}MySet);
  inherited;
end;

function TLyHashType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyHash(Obj).ToString else
    Result := '';
end;

function TLyHashType.Clear(Obj: pointer): boolean;
begin
  Result := (Obj <> nil);
  if Result then
    TLyHash(Obj).Clear;
end;

function TLyHashType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyHash(Obj).DecRefcount else
    Result := 0;
end;

procedure TLyHashType.MarkForSurvive(Obj: pointer);
begin
  if Obj <> nil then TLyHash(Obj).MarkForSurvive;
end;

function TLyHashType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyHash(Obj).IncRefcount else
    Result := 0;
end;

function TLyHashType.Has(Obj: pointer; Value: TLyValue): boolean;
begin
  Result := (Obj <> nil) and MatchStringType(Value.VType) and
            TLyHash(Obj).Has(Value.AsString)
end;

{ TLySearchRec }

function TLySearchRec.Active: boolean;
begin
  Result := FOK;
end;

destructor TLySearchRec.Destroy;
begin
  FindClose;
  inherited;
end;

procedure TLySearchRec.FindClose;
begin
  if FOK then
  begin
    FOK := false;
    FPath := '';
    SysUtils.FindClose(FSR);
  end;
end;

function TLySearchRec.FindFirst(const Path: string; Attr: integer): boolean;
var
  F: string;
begin
  FindClose;
  F := ExpandFileName(SetPD(Trim(Path)));
  FPath := ExtractFilePath(F);
  FOK := (SysUtils.FindFirst(F, Attr, FSR) = 0);
  while FOK and ((FSR.Name = '.') or (FSR.Name = '..')) do
    FOK := FindNext;
  Result := FOK;
end;

function TLySearchRec.FindNext: boolean;
begin
  if FOK then
  begin
    Result := (SysUtils.FindNext(FSR) = 0);
    while Result and ((FSR.Name = '.') or (FSR.Name = '..')) do
      Result := (SysUtils.FindNext(FSR) = 0);
    if not Result then
      FindClose;
  end
  else Result := false;
end;

function TLySearchRec.GetAttr: integer;
begin
  if FOK then
    Result := FSR.Attr else
    Result := 0;
end;

function TLySearchRec.GetFileName: string;
begin
  if FOK then
    Result := FPath + FSr.Name else
    Result := '';
end;

function TLySearchRec.GetName: string;
begin
  if FOK then
    Result := FSR.Name else
    Result := '';
end;

function TLySearchRec.GetPath: string;
begin
  if FOK then
    Result := FPath else
    Result := '';
end;

function TLySearchRec.GetSize: int64;
begin
  if FOK then
    Result := FSR.Size else
    Result := -1;
end;

function TLySearchRec.IsDirectory: boolean;
begin
  Result := FOK and ((FSR.Attr and faDirectory) <> 0);
end;

function TLySearchRec.IsFile: boolean;
begin
  Result := FOK and ((FSR.Attr and faDirectory) = 0);
end;

{ TLySearchRecGenerate }

constructor TLySearchRecGenerate.CreateWith(const SR: TLySearchRec);
begin
  inherited Create;
  FSR := SR;
end;

function TLySearchRecGenerate.GetNext: boolean;
begin
  Result := HasNext;
  if Result then
  begin
    AsString := FSR.FileName;
    FSR.FindNext;
  end
  else Clear;
end;

function TLySearchRecGenerate.HasNext: boolean;
begin
  Result := (FSR <> nil) and FSR.Active;
end;

{ TLySearchRecType }

function TLySearchRecType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLySearchRec(Obj).IncRefcount else
    Result := 0;
end;

function TLySearchRecType.InstanceClass: TClass;
begin
  Result := TLySearchRec;
end;

function TLySearchRecType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLySearchRec(Obj).DecRefcount else
    Result := 0;
end;

function TLySearchRecType.Generate(Obj: pointer): TLyGenerate;
begin
  if (Obj <> nil) and TLySearchRec(Obj).Active then
    Result := TLySearchRecGenerate.CreateWith(TLySearchRec(Obj)) else
    Result := nil;
end;

function TLySearchRecType.AsBoolean(Obj: pointer): boolean;
begin
  Result := (Obj <> nil) and TLySearchRec(Obj).Active;
end;

function TLySearchRecType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLySearchRec(Obj).FileName else
    Result := '';
end;

procedure TLySearchRecType.Setup;
begin
  Module.Define('faReadOnly', faReadOnly);
//Module.Define('faHidden', faHidden);
//Module.Define('faSysFile', faSysFile);
//Module.Define('faVolumeId', faVolumeId);
  Module.Define('faDirectory', faDirectory);
  Module.Define('faArchive', faArchive);
//Module.Define('faSymLink', faSymLink);
  Module.Define('faAnyFile', faAnyFile);

  Method(LSE_CREATE, Self, {$IFDEF FPC}@{$ENDIF}MyCreate);

  Method('FindFirst', my_bool,
         ['Path', '_Attr'], [my_string, my_int],
         {$IFDEF FPC}@{$ENDIF}MyFindFirst);
  Method('FindNext', my_bool,
         {$IFDEF FPC}@{$ENDIF}MyFindNext);
  Method('FindClose',
         {$IFDEF FPC}@{$ENDIF}MyFindClose);
  Method('Active', my_bool,
         {$IFDEF FPC}@{$ENDIF}MyActive);
  Method('IsFile', my_bool,
         {$IFDEF FPC}@{$ENDIF}MyIsFile);
  Method('IsDirectory', my_bool,
         {$IFDEF FPC}@{$ENDIF}MyIsDirectory);
  Method('Path', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetPath);
  Method('Name', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetName);
  Method('FileName', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetFileName);
  Method('Size', my_int,
         {$IFDEF FPC}@{$ENDIF}MyGetSize);
  Method('Attr', my_int,
         {$IFDEF FPC}@{$ENDIF}MyGetAttr);

  inherited;
end;

procedure TLySearchRecType.MyFindFirst(const Param: TLyParam);
var
  This: TLySearchRec;
  Path: string;
  Attr: integer;
begin
  if not Param.GetSelf(This) then Exit;
  Path := Param[1].AsString;
  if Param.Prmc > 2 then
    Attr := integer(Param[2].AsInteger) else
    Attr := faAnyFile;
  Param.Result.AsBoolean := This.FindFirst(Path, Attr);
end;

procedure TLySearchRecType.MyFindNext(const Param: TLyParam);
var
  This: TLySearchRec;
begin
  if not Param.GetSelf(This) then Exit;
  Param.Result.AsBoolean := This.FindNext;
end;

procedure TLySearchRecType.MyFindClose(const Param: TLyParam);
var
  This: TLySearchRec;
begin
  if not Param.GetSelf(This) then Exit;
  This.FindClose;
end;

procedure TLySearchRecType.MyActive(const Param: TLyParam);
var
  This: TLySearchRec;
begin
  if not Param.GetSelf(This) then Exit;
  Param.Result.AsBoolean := This.Active;
end;

procedure TLySearchRecType.MyIsFile(const Param: TLyParam);
var
  This: TLySearchRec;
begin
  if not Param.GetSelf(This) then Exit;
  Param.Result.AsBoolean := This.IsFile;
end;

function TLySearchRecType.CreateInstance: pointer;
begin
  Result := TLySearchRec.Create;
end;

procedure TLySearchRecType.MyIsDirectory(const Param: TLyParam);
var
  This: TLySearchRec;
begin
  if not Param.GetSelf(This) then Exit;
  Param.Result.AsBoolean := This.IsDirectory;
end;

procedure TLySearchRecType.MyGetPath(const Param: TLyParam);
var
  This: TLySearchRec;
begin
  if not Param.GetSelf(This) then Exit;
  Param.Result.AsString := This.Path;
end;

procedure TLySearchRecType.MyGetName(const Param: TLyParam);
var
  This: TLySearchRec;
begin
  if not Param.GetSelf(This) then Exit;
  Param.Result.AsString := This.Name;
end;

procedure TLySearchRecType.MyGetFileName(const Param: TLyParam);
var
  This: TLySearchRec;
begin
  if not Param.GetSelf(This) then Exit;
  Param.Result.AsString := This.FileName;
end;

procedure TLySearchRecType.MyGetSize(const Param: TLyParam);
var
  This: TLySearchRec;
begin
  if not Param.GetSelf(This) then Exit;
  Param.Result.AsInteger := This.Size;
end;

procedure TLySearchRecType.MyGetAttr(const Param: TLyParam);
var
  This: TLySearchRec;
begin
  if not Param.GetSelf(This) then Exit;
  Param.Result.AsInteger := int64(This.Attr);
end;

{ TLyStringsGenerate }

constructor TLyStringsGenerate.CreateIn(const List: TStrings);
begin
  inherited Create;
  FL := List;
  FIndex := -1;
end;

function TLyStringsGenerate.GetNext: boolean;
begin
  Result := HasNext;
  if Result then
  begin
    Inc(FIndex);
    AsString := FL[FIndex];
  end
  else Clear;
end;

function TLyStringsGenerate.HasNext: boolean;
begin
  Result := (FL <> nil) and (FIndex < FL.Count - 1);
end;

{ TLyStringsType }

procedure TLyStringsType.MyAdd(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.Add(Param[1].AsString);
end;

procedure TLyStringsType.MyAssign(const Param: TLyParam);
var
  L: TLyStrings;
  G: TLyGenerate;
begin
  if Param.GetSelf(L) then
  begin
    L.BeginUpdate;
    try
      L.Clear;
      G := GetGenerate(Param.Params[1]);
      if G <> nil then
      try
        while G.GetNext do L.Add(G.AsString);
      finally
        G.Free;
      end;
    finally
      L.EndUpdate;
    end;
  end;
end;

procedure TLyStringsType.MyBeginUpdate(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then L.BeginUpdate;
end;

procedure TLyStringsType.MyClear(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then L.Clear;
end;

procedure TLyStringsType.MyCopy(const Param: TLyParam);
var
  L, V: TLyStrings;
  R: TStrings;
begin
  if Param.GetSelf(L) then
  begin
    R := L.Copy(Param[1].AsInteger, Param[2].AsInteger);
    V := TLyStrings.CreateFor(R);
    V.FreeStrings := true;
    Param.Result.Assign(Self, R);
  end;
end;

procedure TLyStringsType.MyDelete(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.Delete(Param[1].AsInteger);
end;

procedure TLyStringsType.MyDeleteEmptyLines(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then L.DeleteEmptyLines;
end;

procedure TLyStringsType.MyDeleteLast(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then L.DeleteLast;
end;

procedure TLyStringsType.MyDeleteMatchedLines(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.DeleteMatchedLines(Param[1].AsString);
end;

procedure TLyStringsType.MyEndUpdate(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then L.EndUpdate;
end;

procedure TLyStringsType.MyEquals(const Param: TLyParam);
var
  L, S: TLyStrings;
begin
  if Param.GetSelf(L) then
  begin
    S := GetStrings(Param[1]);
    Param.Result.AsBoolean := (L = S) or ((S <> nil) and L.Strings.Equals(S.Strings));
  end
  else Param.Result.AsBoolean := (false);
end;

procedure TLyStringsType.MyExchange(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.Strings.Exchange(Param[1].AsInteger, Param[2].AsInteger);
end;

procedure TLyStringsType.MyGet(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.Strings[Param[1].AsInteger];
end;

procedure TLyStringsType.MyGetCaseSensitive(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsBoolean := L.CaseSensitive;
end;

procedure TLyStringsType.MyGetCommaText(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.Strings.CommaText;
end;

procedure TLyStringsType.MyGetCount(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.Strings.Count;
end;

procedure TLyStringsType.MyGetDelimitedText(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.Strings.DelimitedText;
end;

procedure TLyStringsType.MyGetDelimiter(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsChar := L.Strings.Delimiter;
end;

procedure TLyStringsType.MyGetFirst(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.Strings[0];
end;

procedure TLyStringsType.MyGetLast(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.Strings[L.Strings.Count - 1];
end;

procedure TLyStringsType.MyGetLineBreak(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.Strings.LineBreak;
end;

procedure TLyStringsType.MyNames_Get(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.Strings.Names[Param[1].AsInteger];
end;

procedure TLyStringsType.MyGetNameValueSeparator(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsChar := L.Strings.NameValueSeparator;
end;

procedure TLyStringsType.MyGetQuoteChar(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsChar := L.Strings.QuoteChar;
end;

procedure TLyStringsType.MyGetSorted(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsBoolean := L.Sorted;
end;

procedure TLyStringsType.MyGetStrictDelimiter(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsBoolean := L.Strings.StrictDelimiter;
end;

procedure TLyStringsType.MyGetText(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.Strings.Text;
end;

procedure TLyStringsType.MyValues_Get(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.Strings.Values[Param[1].AsString];
end;

procedure TLyStringsType.MyGetWriteBOM(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsBoolean := {$IFDEF FPC}false{$ELSE}L.Strings.WriteBOM{$ENDIF};
end;

procedure TLyStringsType.MyIndexOf(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.Strings.IndexOf(Param[1].AsString);
end;

procedure TLyStringsType.MyIndexOfName(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.Strings.IndexOfName(Param[1].AsString);
end;

procedure TLyStringsType.MyIndexOfObject(const Param: TLyParam);
var
  L: TLyStrings;
  V: TLyValue;
  I: integer;
begin
  if Param.GetSelf(L) then
  begin
    V := Param[1];
    for I := 0 to L.Strings.Count - 1 do
      if L.Strings.Objects[I] <> nil then
        if V.Same(TLyValue(L.Strings.Objects[I])) then
        begin
          Param.Result.AsInteger := I;
          Exit;
        end;
    Param.Result.AsInteger := -1;
  end;
end;

procedure TLyStringsType.MyInsert(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.Insert(Param[1].AsInteger, Param[2].AsString);
end;

procedure TLyStringsType.MyIsEmpty(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsBoolean := L.IsEmpty;
end;

procedure TLyStringsType.MyLeft(const Param: TLyParam);
var
  L, V: TLyStrings;
  R: TStrings;
begin
  if Param.GetSelf(L) then
  begin
    R := L.CopyLeft(Param[1].AsInteger);
    V := TLyStrings.CreateFor(R);
    V.FreeStrings := true;
    Param.Result.Assign(Self, R);
  end;
end;

procedure TLyStringsType.MyLoadFromFile(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.LoadFromFile(Param[1].AsFileName);
end;

procedure TLyStringsType.MyMove(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.Strings.Move(Param[1].AsInteger, Param[2].AsInteger);
end;

procedure TLyStringsType.MyNames_List(const Param: TLyParam);
var
  L: TLyStrings;
  N: TLyList;
  S: string;
  I: integer;
begin
  if Param.GetSelf(L) then
  begin
    N := TLyList.Create;
    Param.Result.Assign(my_array, N);
    for I := 0 to L.Count - 1 do
    begin
      S := L.Strings.Names[I];
      if S <> '' then
        N.Add.AsString := S;
    end;
  end;
end;

procedure TLyStringsType.MyPack(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then L.Pack;
end;

procedure TLyStringsType.MyRemove(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.Remove(Param[1].AsString);
end;

procedure TLyStringsType.MyRemoveByName(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.RemoveByName(Param[1].AsString);
end;

procedure TLyStringsType.MyReverse(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then L.Reverse;
end;

procedure TLyStringsType.MyRight(const Param: TLyParam);
var
  L, V: TLyStrings;
  R: TStrings;
begin
  if Param.GetSelf(L) then
  begin
    R := L.CopyRight(Param[1].AsInteger);
    V := TLyStrings.CreateFor(R);
    V.FreeStrings := true;
    Param.Result.Assign(Self, R);
  end;
end;

procedure TLyStringsType.MySaveToFile(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.SaveToFile(Param[1].AsFileName);
end;

procedure TLyStringsType.MySelectMatchedLines(const Param: TLyParam);
var
  L, V: TLyStrings;
  R: TStrings;
begin
  if Param.GetSelf(L) then
  begin
    R := L.SelectMatched(Param[1].AsString);
    V := TLyStrings.CreateFor(R);
    V.FreeStrings := true;
    Param.Result.Assign(Self, R);
  end;
end;

procedure TLyStringsType.MySet(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.Strings[Param[1].AsInteger] := Param[2].AsString;
end;

procedure TLyStringsType.MySetCaseSensitive(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.CaseSensitive := Param[1].AsBoolean;
end;

procedure TLyStringsType.MySetCommaText(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
  begin
    L.Clear;
    L.Strings.CommaText := Param[1].AsString;
  end;
end;

procedure TLyStringsType.MySetCount(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.Count := Param[1].AsInteger;
end;

procedure TLyStringsType.MySetDelimitedText(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
  begin
    L.Clear;
    L.Strings.DelimitedText := Param[1].AsString;
  end;
end;

procedure TLyStringsType.MySetDelimiter(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.Strings.Delimiter := Param[1].AsChar;
end;

procedure TLyStringsType.MySetFirst(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.Strings[0] := Param[1].AsString;
end;

procedure TLyStringsType.MySetLast(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.Strings[L.Strings.Count - 1] := Param[1].AsString;
end;

procedure TLyStringsType.MySetLineBreak(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.Strings.LineBreak := Param[1].AsString;
end;

procedure TLyStringsType.MySetNameValueSeparator(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.Strings.NameValueSeparator := Param[1].AsChar;
end;

procedure TLyStringsType.MySetQuoteChar(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.Strings.QuoteChar := Param[1].AsChar;
end;

procedure TLyStringsType.MySetSorted(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.Sorted := Param[1].AsBoolean;
end;

procedure TLyStringsType.MySetStrictDelimiter(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.Strings.StrictDelimiter := Param[1].AsBoolean;
end;

procedure TLyStringsType.MySetText(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
  begin
    L.Clear;
    L.Strings.Text := Param[1].AsString;
  end;
end;

procedure TLyStringsType.MyValues_Set(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    L.Values[Param[1].AsString] := Param[2].AsString;
end;

function TLyStringsType.CreateInstance: pointer;
begin
  Result := TLyStrings.Create;
end;

procedure TLyStringsType.MySetWriteBOM(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    {$IFNDEF FPC}
    L.Strings.WriteBOM := Param[1].AsBoolean
    {$ENDIF};
end;

procedure TLyStringsType.MySort(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then L.Sort;
end;

procedure TLyStringsType.MyTrim(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then L.Process({$IFDEF FPC}@{$ENDIF}SysUtils.Trim);
end;

procedure TLyStringsType.MyTrimAll(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then L.Process({$IFDEF FPC}@{$ENDIF}Basic.TrimAll);
end;

procedure TLyStringsType.MyTrimLeft(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then L.Process({$IFDEF FPC}@{$ENDIF}SysUtils.TrimLeft);
end;

procedure TLyStringsType.MyTrimRight(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then L.Process({$IFDEF FPC}@{$ENDIF}SysUtils.TrimRight);
end;

procedure TLyStringsType.MyUnique(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then L.Unique;
end;

procedure TLyStringsType.MyValueFromIndex_Get(const Param: TLyParam);
var
  L: TLyStrings;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.Strings.ValueFromIndex[Param[1].AsInteger];
end;

procedure TLyStringsType.Setup;
begin
  if (FNamesType <> nil) or (Module = nil) then Exit;

  Method(LSE_CREATE, Self, {$IFDEF FPC}@{$ENDIF}MyCreate);
  Method('IsEmpty', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsEmpty);
  Method('LoadFromFile', ['FileName'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyLoadFromFile);
  Method('SaveToFile', ['FileName'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MySaveToFile);
  Method('BeginUpdate', {$IFDEF FPC}@{$ENDIF}MyBeginUpdate);
  Method('EndUpdate', {$IFDEF FPC}@{$ENDIF}MyEndUpdate);
  Method('Clear', {$IFDEF FPC}@{$ENDIF}MyClear);
  Method('Delete', ['Index'], [my_int], {$IFDEF FPC}@{$ENDIF}MyDelete);
  Method('DeleteLast', {$IFDEF FPC}@{$ENDIF}MyDeleteLast);
  Method('DeleteEmptyLines', {$IFDEF FPC}@{$ENDIF}MyDeleteEmptyLines);
  Method('DeleteMatchedLines', ['Patten'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyDeleteMatchedLines);
  Method('Remove', ['S'], [my_string], {$IFDEF FPC}@{$ENDIF}MyRemove);
  Method('RemoveByName', ['Name'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyRemoveByName);
  Method('Add', my_int, ['_S'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyAdd);
  Method('Insert', ['Index', '_S'], [my_int, my_string],
         {$IFDEF FPC}@{$ENDIF}MyInsert);
  Method('Pack', {$IFDEF FPC}@{$ENDIF}MyPack);
  Method('Exchange', ['X1', 'X2'], [my_int, my_int],
         {$IFDEF FPC}@{$ENDIF}MyExchange);
  Method('Move', ['FromX', 'ToX'], [my_int, my_int],
         {$IFDEF FPC}@{$ENDIF}MyMove);
  Method('IndexOf', my_int, ['S'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyIndexOf);
  Method('IndexOfObject', my_int, ['Any'], [my_variant],
         {$IFDEF FPC}@{$ENDIF}MyIndexOfObject);
  Method('IndexOfName', my_int, ['Name'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyIndexOfName);
  Method('Assign', ['Source'], [my_variant],
         {$IFDEF FPC}@{$ENDIF}MyAssign);
  Method('Equals', my_bool, ['Strings'], [Self],
         {$IFDEF FPC}@{$ENDIF}MyEquals);
  Method('Trim', {$IFDEF FPC}@{$ENDIF}MyTrim);
  Method('TrimAll', {$IFDEF FPC}@{$ENDIF}MyTrimAll);
  Method('TrimLeft', {$IFDEF FPC}@{$ENDIF}MyTrimLeft);
  Method('TrimRight', {$IFDEF FPC}@{$ENDIF}MyTrimRight);
  Method('Copy', Self, ['Index', 'Count'], [my_int, my_int],
         {$IFDEF FPC}@{$ENDIF}MyCopy);
  Method('Left', Self, ['Count'], [my_int],
         {$IFDEF FPC}@{$ENDIF}MyLeft);
  Method('Right', Self, ['Count'], [my_int],
         {$IFDEF FPC}@{$ENDIF}MyRight);
  Method('SelectMatchedLines', Self, ['Patten'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MySelectMatchedLines);
  Method('Reverse', {$IFDEF FPC}@{$ENDIF}MyReverse);
  Method('Unique', {$IFDEF FPC}@{$ENDIF}MyUnique);
  Method('Sort', {$IFDEF FPC}@{$ENDIF}MySort);
  Define('Count', my_int,
         {$IFDEF FPC}@{$ENDIF}MyGetCount,
         {$IFDEF FPC}@{$ENDIF}MySetCount);
  Define(my_string, ['Index'], [my_int],
         {$IFDEF FPC}@{$ENDIF}MyGet,
         {$IFDEF FPC}@{$ENDIF}MySet);
  Define('Sorted', my_bool,
         {$IFDEF FPC}@{$ENDIF}MyGetSorted,
         {$IFDEF FPC}@{$ENDIF}MySetSorted);
  Define('CaseSensitive', my_bool,
         {$IFDEF FPC}@{$ENDIF}MyGetCaseSensitive,
         {$IFDEF FPC}@{$ENDIF}MySetCaseSensitive);

  FNamesType := Branch('Names') as TLyStringsType;
  FNamesType.SetupNamesType;

  FValuesType := Branch('Values') as TLyStringsType;
  FValuesType.SetupValuesType;

  FValueFromIndexType := Branch('ValueFromIndex') as TLyStringsType;
  FValueFromIndexType.SetupValueFromIndexType;

  Define('Text', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetText,
         {$IFDEF FPC}@{$ENDIF}MySetText);
  Define('CommaText', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetCommaText,
         {$IFDEF FPC}@{$ENDIF}MySetCommaText);
  Define('Delimiter', my_char,
         {$IFDEF FPC}@{$ENDIF}MyGetDelimiter,
         {$IFDEF FPC}@{$ENDIF}MySetDelimiter);
  Define('DelimitedText', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetDelimitedText,
         {$IFDEF FPC}@{$ENDIF}MySetDelimitedText);
  Define('LineBreak', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetLineBreak,
         {$IFDEF FPC}@{$ENDIF}MySetLineBreak);
  Define('NameValueSeparator', my_char,
         {$IFDEF FPC}@{$ENDIF}MyGetNameValueSeparator,
         {$IFDEF FPC}@{$ENDIF}MySetNameValueSeparator);
  Define('QuoteChar', my_char,
         {$IFDEF FPC}@{$ENDIF}MyGetQuoteChar,
         {$IFDEF FPC}@{$ENDIF}MySetQuoteChar);
  Define('StrictDelimiter', my_bool,
         {$IFDEF FPC}@{$ENDIF}MyGetStrictDelimiter,
         {$IFDEF FPC}@{$ENDIF}MySetStrictDelimiter);
  Define('WriteBOM', my_bool,
         {$IFDEF FPC}@{$ENDIF}MyGetWriteBOM,
         {$IFDEF FPC}@{$ENDIF}MySetWriteBOM);
  Define('First', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetFirst,
         {$IFDEF FPC}@{$ENDIF}MySetFirst);
  Define('Last', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetLast,
         {$IFDEF FPC}@{$ENDIF}MySetLast);
  inherited;
end;

procedure TLyStringsType.SetupNamesType;
begin
  Define(my_string, ['Index'], [my_int],
         {$IFDEF FPC}@{$ENDIF}MyNames_Get);
  Method('List', my_array,
         {$IFDEF FPC}@{$ENDIF}MyNames_List);
end;

procedure TLyStringsType.SetupValueFromIndexType;
begin
  Define(my_string, ['Index'], [my_int],
         {$IFDEF FPC}@{$ENDIF}MyValueFromIndex_Get);
end;

procedure TLyStringsType.SetupValuesType;
begin
  Define(my_string, ['Name'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyValues_Get,
         {$IFDEF FPC}@{$ENDIF}MyValues_Set);
end;

function TLyStringsType.Add(Obj: pointer; Value: TLyValue): integer;
var
  L: TLyStrings;
begin
  Validate(Obj);
  L := TLyStrings(Obj);
  Result := L.Strings.Add(Value.AsString);
end;

function TLyStringsType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyStrings(Obj).Strings.Text else
    Result := '';
end;

function TLyStringsType.Clear(Obj: pointer): boolean;
begin
  Result := (Obj <> nil);
  if Result then
    TLyStrings(Obj).Clear;
end;

function TLyStringsType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyStrings(Obj).DecRefcount else
    Result := 0;
end;

function TLyStringsType.Generate(Obj: pointer): TLyGenerate;
begin
  if (Obj <> nil) and (TLyStrings(Obj).Strings.Count > 0) then
    Result := TLyStringsGenerate.CreateIn(TLyStrings(Obj).Strings) else
    Result := nil;
end;

function TLyStringsType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyStrings(Obj).IncRefcount else
    Result := 0;
end;

function TLyStringsType.InstanceClass: TClass;
begin
  Result := TLyStrings;
end;

function TLyStringsType.GetLength(Obj: pointer): int64;
begin
  if Obj <> nil then
    Result := TLyStrings(Obj).Strings.Count else
    Result := 0;
end;

{ TLyStringListType }

procedure TLyStringListType.Convert(Value: TLyValue);
var
  L: TLyStrings;
begin
  if Value.VType = my_strings then
  begin
    L := TLyStrings(Value.Data);
    if (L = nil) or (L is TLyStringList) then
    begin
      Value.Assign(Self, L);
      Exit;
    end;
  end;
  inherited;
end;

procedure TLyStringListType.Setup;
begin
  Parent := my_strings;
  inherited;
end;

{ TLyIniFile }

destructor TLyIniFile.Destroy;
begin
  FreeAndNil(FFile);
  inherited;
end;

procedure TLyIniFile.Open(const FileName: string);
begin
  FreeAndNil(FFile);
  FFile := TIniFile.Create(FileName);
end;

{ TLyIniFileType }

function TLyIniFileType.AsString(Obj: pointer): string;
var
  F: TIniFile;
begin
  if Obj <> nil then
  begin
    F := TLyIniFile(Obj).FFile;
    if F <> nil then
    begin
      Result := F.FileName;
      Exit;
    end;
  end;
  Result := '';
end;

function TLyIniFileType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyIniFile(Obj).IncRefcount else
    Result := 0;
end;

function TLyIniFileType.InstanceClass: TClass;
begin
  Result := TLyIniFile;
end;

function TLyIniFileType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyIniFile(Obj).DecRefcount else
    Result := 0;
end;

procedure TLyIniFileType.Setup;
begin
  Method(LSE_CREATE, my_inifile, ['FileName'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyCreate);
  Method('Open', my_nil, ['FileName'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyOpenFile);
  Method('SectionExists', my_bool, ['Section'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MySectionExists);
  Method('ReadString', my_string, ['Section', 'ID', '_Defv'],
         [my_string, my_string, my_string],
         {$IFDEF FPC}@{$ENDIF}MyReadString);
  Method('ReadInteger', my_int, ['Section', 'ID', '_Defv'],
         [my_string, my_string, my_int],
         {$IFDEF FPC}@{$ENDIF}MyReadInteger);
  Method('ReadInt64', my_int, ['Section', 'ID', '_DefV'],
         [my_string, my_string, my_int],
         {$IFDEF FPC}@{$ENDIF}MyReadInteger);
  Method('ReadBool', my_bool, ['Section', 'ID', '_DefV'],
         [my_string, my_string, my_bool],
         {$IFDEF FPC}@{$ENDIF}MyReadBool);
  Method('ReadFloat', my_float,
         ['Section', 'ID', '_DefV'], [my_string, my_string, my_float],
         {$IFDEF FPC}@{$ENDIF}MyReadFloat);
  Method('ReadDate', my_time,
         ['Section', 'ID', '_DefV'], [my_string, my_string, my_time],
         {$IFDEF FPC}@{$ENDIF}MyReadDate);
  Method('ReadDateTime', my_time,
         ['Section', 'ID', '_DefV'], [my_string, my_string, my_time],
         {$IFDEF FPC}@{$ENDIF}MyReadDateTime);
  Method('ReadTime', my_time,
         ['Section', 'ID', '_DefV'], [my_string, my_string, my_time],
         {$IFDEF FPC}@{$ENDIF}MyReadTime);
  Method('WriteString', my_nil,
         ['Section', 'ID', 'Value'], [my_string, my_string, my_string],
         {$IFDEF FPC}@{$ENDIF}MyWriteString);
  Method('WriteInteger', my_nil,
         ['Section', 'ID', 'Value'], [my_string, my_string, my_int],
         {$IFDEF FPC}@{$ENDIF}MyWriteInteger);
  Method('WriteInt64', my_nil,
         ['Section', 'ID', 'Value'], [my_string, my_string, my_int],
         {$IFDEF FPC}@{$ENDIF}MyWriteInteger);
  Method('WriteBool', my_nil,
         ['Section', 'ID', 'Value'], [my_string, my_string, my_bool],
         {$IFDEF FPC}@{$ENDIF}MyWriteBool);
  Method('WriteFloat', my_nil,
         ['Section', 'ID', 'Value'], [my_string, my_string, my_float],
         {$IFDEF FPC}@{$ENDIF}MyWriteFloat);
  Method('WriteDate', my_nil,
         ['Section', 'ID', 'Value'], [my_string, my_string, my_time],
         {$IFDEF FPC}@{$ENDIF}MyWriteDate);
  Method('WriteDateTime', my_nil,
         ['Section', 'ID', 'Value'], [my_string, my_string, my_time],
         {$IFDEF FPC}@{$ENDIF}MyWriteDateTime);
  Method('WriteTime', my_nil,
         ['Section', 'ID', 'Value'], [my_string, my_string, my_time],
         {$IFDEF FPC}@{$ENDIF}MyWriteTime);
  Method('ReadSection', my_strlist, ['Section'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyReadSection);
  Method('ReadSections', my_strlist,
         {$IFDEF FPC}@{$ENDIF}MyReadSections);
  Method('ReadSectionValues', my_strlist, ['Section'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyReadSectionValues);
  Method('EraseSection', my_nil, ['Section'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyEraseSection);
  Method('DeleteKey', my_nil, ['Section', 'ID'], [my_string, my_string],
         {$IFDEF FPC}@{$ENDIF}MyDeleteKey);
  Method('UpdateFile', my_nil,
         {$IFDEF FPC}@{$ENDIF}MyUpdateFile);
  Method('ValueExists', my_bool, ['Section', 'ID'], [my_string, my_string],
         {$IFDEF FPC}@{$ENDIF}MyValueExists);
  Define('FileName', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetFileName);

  inherited;
end;

procedure TLyIniFileType.MyCreate(const Param: TLyParam);
begin
  inherited;
  MyOpenFile(Param);
end;

procedure TLyIniFileType.MySectionExists(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    Param.Result.AsBoolean := F.FFile.SectionExists(Param[1].AsString);
end;

procedure TLyIniFileType.MyReadString(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    Param.Result.AsString := F.FFile.ReadString(Param[1].AsString,
      Param[2].AsString, Param[3].AsString);
end;

procedure TLyIniFileType.MyWriteString(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    F.FFile.WriteString(Param[1].AsString, Param[2].AsString, Param[3].AsString);
end;

procedure TLyIniFileType.MyWriteInteger(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    F.FFile.WriteInteger(Param[1].AsString, Param[2].AsString, Param[3].AsInteger);
end;

procedure TLyIniFileType.MyWriteBool(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    F.FFile.WriteBool(Param[1].AsString, Param[2].AsString, Param[3].AsBoolean);
end;

procedure TLyIniFileType.MyWriteDate(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    F.FFile.WriteDate(Param[1].AsString, Param[2].AsString, Param[3].AsTime);
end;

procedure TLyIniFileType.MyReadInteger(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    Param.Result.AsInteger := F.FFile.ReadInteger(Param[1].AsString,
      Param[2].AsString, Param[3].AsInteger);
end;

procedure TLyIniFileType.MyReadBool(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    Param.Result.AsBoolean := F.FFile.ReadBool(Param[1].AsString,
      Param[2].AsString, Param[3].AsBoolean);
end;

procedure TLyIniFileType.MyReadFloat(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    Param.Result.AsFloat := F.FFile.ReadFloat(Param[1].AsString,
      Param[2].AsString, Param[3].AsFloat);
end;

procedure TLyIniFileType.MyReadDate(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    Param.Result.AsTime := F.FFile.ReadDate(Param[1].AsString,
      Param[2].AsString, Param[3].AsTime);
end;

procedure TLyIniFileType.MyReadDateTime(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    Param.Result.AsTime := F.FFile.ReadDateTime(Param[1].AsString,
      Param[2].AsString, Param[3].AsTime);
end;

procedure TLyIniFileType.MyReadTime(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    Param.Result.AsTime := F.FFile.ReadTime(Param[1].AsString,
      Param[2].AsString, Param[3].AsTime);
end;

procedure TLyIniFileType.MyWriteDateTime(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    F.FFile.WriteDateTime(Param[1].AsString, Param[2].AsString, Param[3].AsTime);
end;

procedure TLyIniFileType.MyWriteFloat(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    F.FFile.WriteFloat(Param[1].AsString, Param[2].AsString, Param[3].AsFloat);
end;

procedure TLyIniFileType.MyWriteTime(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    F.FFile.WriteTime(Param[1].AsString, Param[2].AsString, Param[3].AsTime);
end;

function TLyIniFileType.CreateInstance: pointer;
begin
  Result := TLyIniFile.Create;
end;

procedure TLyIniFileType.MyReadSection(const Param: TLyParam);
var
  F: TLyIniFile;
  L: TLyStringList;
begin
  if Param.GetSelf(F) then
  begin
    L := TLyStringList.Create;
    Param.Result.Assign(my_strlist, L);
    F.FFile.ReadSection(Param[1].AsString, L.Strings);
  end;
end;

procedure TLyIniFileType.MyReadSections(const Param: TLyParam);
var
  F: TLyIniFile;
  L: TLyStringList;
begin
  if Param.GetSelf(F) then
  begin
    L := TLyStringList.Create;
    Param.Result.Assign(my_strlist, L);
    F.FFile.ReadSections(L.Strings);
  end;
end;

procedure TLyIniFileType.MyReadSectionValues(const Param: TLyParam);
var
  F: TLyIniFile;
  L: TLyStringList;
begin
  if Param.GetSelf(F) then
  begin
    L := TLyStringList.Create;
    Param.Result.Assign(my_strlist, L);
    F.FFile.ReadSectionValues(Param[1].AsString, L.Strings);
  end;
end;

procedure TLyIniFileType.MyEraseSection(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    F.FFile.EraseSection(Param[1].AsString);
end;

procedure TLyIniFileType.MyDeleteKey(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    F.FFile.DeleteKey(Param[1].AsString, Param[2].AsString);
end;

procedure TLyIniFileType.MyUpdateFile(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    F.FFile.UpdateFile;
end;

procedure TLyIniFileType.MyValueExists(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    Param.Result.AsBoolean := F.FFile.ValueExists(Param[1].AsString,
      Param[2].AsString);
end;

procedure TLyIniFileType.MyGetFileName(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    Param.Result.AsString := F.FFile.FileName;
end;

procedure TLyIniFileType.MyOpenFile(const Param: TLyParam);
var
  F: TLyIniFile;
begin
  if Param.GetSelf(F) then
    F.Open(Param[1].AsFileName);
end;

initialization
begin
  my_integer_zero := TLyInteger.Create;
  my_integer_zero.FData := 0;
  my_integer_zero.IncRefcount;

  my_boolean_datas[false] := TLyBoolean.Create;
  my_boolean_datas[false].FData := false;
  my_boolean_datas[false].IncRefcount;

  my_boolean_datas[true] := TLyBoolean.Create;
  my_boolean_datas[true].FData := true;
  my_boolean_datas[true].IncRefcount;

  my_char_datas[#0] := TLyChar.Create;
  my_char_datas[#0].FData := #0;
  my_char_datas[#0].IncRefcount;

  my_float_zero := TLyFloat.Create;
  my_float_zero.FData := 0;
  my_float_zero.IncRefcount;

  my_currency_zero := TLyCurrency.Create;
  my_currency_zero.FData := 0;
  my_currency_zero.IncRefcount;

  my_time_zero := TLyTime.Create;
  my_time_zero.FData := 0;
  my_time_zero.IncRefcount;
end;

end.
