{==============================================================================}
{        UNIT: lysee                                                           }
{ DESCRIPTION: lysee script interpreter                                        }
{   COPYRIGHT: Copyright (c) 2003-2015, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/02/28                                                      }
{    MODIFIED: 2017/02/19                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  {$IFNDEF FPC}Types,{$ENDIF}
  SysUtils, Classes, DateUtils, Math, SyncObjs, basic;

const

  { LSE: Lysee Script Engine }

  LSE_NAME     = 'lysee';
  LSE_VERSION  = '2017.2.18';
  LSE_FILEEXT  = '.ls';
  LSE_CONFILE  = 'lysee.conf';
  LSE_SYSTE    = 'system';
  LSE_MAIN     = 'main';

  { TID: Lysee Type IDentity }

  TID_VARIANT   = 0;
  TID_NIL       = 1;
  TID_CHAR      = 2; { basic }
  TID_INTEGER   = 3;
  TID_FLOAT     = 4;
  TID_CURRENCY  = 5;
  TID_TIME      = 6;
  TID_BOOLEAN   = 7;
  TID_TYPE      = 8; { object }
  TID_EXCEPTION = 9;
  TID_STRING    = 10;
  TID_MODULE    = 11;
  TID_FUNCTION  = 12;
  TID_HASHLIST  = 13;
  TID_ARRAY     = 14;

  TID_NAMES: array[TID_VARIANT..TID_ARRAY] of string = (
    'variant', 'nil', 'char', 'integer', 'float', 'currency',
    'time', 'boolean', 'type', 'exception', 'string', 'module',
    'function', 'THash', 'array');

type

  { forward }

  TLysee            = class; //<--lysee script engine
  TLyseeError       = class;
  TLyseeType        = class;
  TLyseeEnumItem    = class;
  TLyseeEnumType    = class;
  TLyseeEnumSet     = class;
  TLyseeEnumSetType = class;
  TLyseeValue       = class;
  TLyseeParam       = class;
  TLyseeToken       = class;
  TLyseeTokenList   = class;
  TLyseeTokenizer   = class;
  TLyseeParser      = class;
  TLyseeSTMT        = class;
  TLyseeSTMTList    = class;
  TLyseeAssign      = class;
  TLyseeFor         = class;
  TLyseeIf          = class;
  TLyseeCase        = class;
  TLyseeTry         = class;
  TLyseeVarb        = class;
  TLyseeVarbList    = class;
  TLyseeFunc        = class;
  TLyseeFuncList    = class;
  TLyseeModule      = class;
  TLyseeModuleList  = class;
  TLyseeString      = class;
  TLyseeGarbage     = class;
  TLyseeCollect     = class;
  TLyseeList        = class;
  TLyseeHash        = class;
  TLyseeGenerate    = class;

  TLyseeProc = procedure(const Param: TLyseeParam);
  TLyseeObjectProc = procedure(const Param: TLyseeParam) of object;

  { TLysee }

  TLyseeReadln = procedure(Sender: TObject; var S: string) of object;
  TLyseeWrite = procedure(Sender: TObject; const S: string) of object;
  TLyseeResolve = function(const ID: string; Value: TLyseeValue): boolean of object;
  TLyseeGetModule = function(Sender: TObject; const Module: string): string of object;
  TLyseeState = (csTerminated, csReady, csRunning, csContinue, csBreak, csExit);

  TLysee = class(TComponent)
  private
    FMainFile: string;
    FError: TLyseeError;
    FState: TLyseeState;
    FTerminated: boolean;
    FExcepted: boolean;
    FHalted: boolean;
    FResult: TLyseeValue;
    FMainModule: TLyseeModule;
    FMainFunc: TLyseeFunc;
    FMainLocals: TLyseeList;
    FMainToken: TLyseeToken;
    FArgs: TLyseeList;
    FModules: TLyseeModuleList;
    FRollbacks: TList;
    FNameSeed: cardinal;
    FCurrent: TLyseeParam;
    FOnExecuting: TNotifyEvent;
    FOnExecuted: TNotifyEvent;
    FOnReadln: TLyseeReadln;
    FOnWrite: TLyseeWrite;
    FOnResolve: TLyseeResolve;
    FOnGetModuleFile: TLyseeGetModule;
    procedure SetMainFile(const Value: string);
    procedure SetResult(Value: TLyseeValue);
    function GetMainFunc: TLyseeFunc;
    function GetCodeModule: TLyseeModule;
    function GetCodeFunc: TLyseeFunc;
    function GetReady: boolean;
    function GetRunning: boolean;
    function GetTerminated: boolean;
    function GetTerminating: boolean;
    function GetMainModule: TLyseeModule;
    function GetMainToken: TLyseeToken;
    function Compile(const Code: string): TLyseeFunc;
  protected
    procedure MarkSurvived;
    procedure BeginExecute;
    procedure EndExecute;
    procedure ExecMain;
    procedure ForWhileRepeatEnded;
    procedure Rollback;
    procedure RollbackAdd(AObj: TObject);
    procedure RollbackRemove(AObj: TObject);
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure CheckNotRunning;
    procedure Clear(WillDestroy: boolean = false);
    function Resolve(const ID: string; Value: TLyseeValue): boolean;virtual;
    function GetModuleFile(const AName: string): string;
    { execute }
    function Execute(const Code: string): boolean;
    function ExecuteFile(const FileName: string): boolean;
    function ExecuteFrom(StartParamIndex: integer = 1): boolean;
    function Terminate: boolean;
    function StatusOK: boolean;
    { stdio }
    procedure Readln(var Text: string);virtual;
    procedure Write(const Text: string);overload;
    procedure Write(const Text: string; const Args: array of const);overload;
    procedure Writeln;overload;
    procedure Writeln(const Text: string);overload;
    procedure Writeln(const Text: string; const Args: array of const);overload;
    { property }
    property Error: TLyseeError read FError;
    property Ready: boolean read GetReady;
    property Running: boolean read GetRunning;
    property Terminating: boolean read GetTerminating;
    property Terminated: boolean read GetTerminated;
    property Halted: boolean read FHalted;
    property Excepted: boolean read FExcepted write FExcepted;
    property Modules: TLyseeModuleList read FModules;
    property MainFile: string read FMainFile write SetMainFile;
    property MainModule: TLyseeModule read GetMainModule;
    property MainFunc: TLyseeFunc read GetMainFunc;
    property MainToken: TLyseeToken read GetMainToken;
    property Current: TLyseeParam read FCurrent;
    property CodeModule: TLyseeModule read GetCodeModule;
    property CodeFunc: TLyseeFunc read GetCodeFunc;
    property Stack: TLyseeList read FMainLocals;
    property Result: TLyseeValue read FResult write SetResult;
    property Args: TLyseeList read FArgs;
    property Rollbacks: TList read FRollbacks write FRollbacks;
    property OnExecuting: TNotifyEvent read FOnExecuting write FOnExecuting;
    property OnExecuted: TNotifyEvent read FOnExecuted write FOnExecuted;
    property OnReadln: TLyseeReadln read FOnReadln write FOnReadln;
    property OnWrite: TLyseeWrite read FOnWrite write FOnWrite;
    property OnResolve: TLyseeResolve read FOnResolve write FOnResolve;
    property OnGetModuleFile: TLyseeGetModule read FOnGetModuleFile write FOnGetModuleFile;
  end;

  { TLyseeError }

  TLyseeError = class
  private
    FLysee: TLysee;
    FErrID: string;
    FMsg: string;
    FModule: string;
    FFileName: string;
    FRow: integer;
    FCol: integer;
    function GetText: string;
  public
    constructor Create(ALysee: TLysee);
    procedure Syntax(const Msg, Module, FileName: string; Row, Col: integer);
    procedure Runtime(const Msg, Module, FileName: string; Row, Col: integer);
    procedure Clear;
    property ErrID: string read FErrID;
    property EModule: string read FModule;
    property EFileName: string read FFileName;
    property ERow: integer read FRow;
    property ECol: integer read FCol;
    property EMsg: string read FMsg;
    property ErrorText: string read GetText;
  end;

  { TLyseeType }

  TLyseeOperator = (opAdd, opDec, opMul, opDiv, opDivf, opMod, opXor, opShl,
                    opShr, opShi, opFill, opIn, opLike, opIs, opAs,
                    {single}opNot, opNeg);

  TLyseeOperateProc = procedure(V1, V2: TLyseeValue);
  PLyseeOperate = ^RLyseeOperate;
  RLyseeOperate = packed record
    o_oper: TLyseeOperator;
    o_type: TLyseeType;
    o_operate: TLyseeOperateProc;
    o_next: PLyseeOperate;
  end;

  TLyseeCompareFunc = function(V1, V2: TLyseeValue): TCompare;
  PLyseeCompare = ^RLyseeCompare;
  RLyseeCompare = packed record
    c_type: TLyseeType;
    c_compare: TLyseeCompareFunc;
    c_next: PLyseeCompare;
  end;

  TLyseeTypeStyle = (tsVariant, tsNil, tsBasic, tsEnum, tsEnumSet, tsObject);

  TLyseeType = class
  private
    FModule: TLyseeModule;
    FParent: TLyseeType;
    FName: string;
    FTID: integer;
    FStyle: TLyseeTypeStyle;
    FMethods : TLyseeFuncList;
    FConstructer: TLyseeFunc;
    FOperate: array[TLyseeOperator] of PLyseeOperate;
    FCompare : PLyseeCompare;
    FProcs: array of TLyseeObjectProc;
    function GetFullName: string;
    function GetMethodCount: integer;
    function GetMethod(Index: integer): TLyseeFunc;
    procedure SetParent(const Value: TLyseeType);
  protected
    function RegisterProc(AProc: TLyseeObjectProc): integer;
    procedure Setup;virtual;
  public
    function IncRefcount(Obj: pointer): integer;virtual;
    function DecRefcount(Obj: pointer): integer;virtual;
    function AsString(Obj: pointer): string;virtual;
    function AsChar(Obj: pointer): char;virtual;
    function AsInteger(Obj: pointer): int64;virtual;
    function AsFloat(Obj: pointer): double;virtual;
    function AsCurrency(Obj: pointer): currency;virtual;
    function AsTime(Obj: pointer): TDateTime;virtual;
    function AsBoolean(Obj: pointer): boolean;virtual;
    function ConvertTo(Value: TLyseeValue; T: TLyseeType): boolean;virtual;
    procedure Convert(Value: TLyseeValue);virtual;
    procedure SetDefault(Value: TLyseeValue);virtual;
    procedure GcMark(Obj: pointer);virtual; // for garbage collection
    function Generate(Obj: pointer): TLyseeGenerate;virtual;
    function GetLength(Obj: pointer): int64;virtual;
    function Clear(Obj: pointer): boolean;virtual;
    function Add(Obj: pointer; Value: TLyseeValue): integer;virtual;
    procedure Validate(Obj: pointer);virtual;
  public
    function FindMethod(const AName: string): TLyseeFunc;
    function Method(const AName: string;
      const AType: TLyseeType;
      const ParamNames: array of string;
      const ParamTypes: array of TLyseeType;
      const AProc: TLyseeObjectProc): TLyseeFunc;overload;
    function Method(const AName: string;
      const ParamNames: array of string;
      const ParamTypes: array of TLyseeType;
      const AProc: TLyseeObjectProc): TLyseeFunc;overload;
    function Method(const AName: string;
      const AType: TLyseeType;
      const AProc: TLyseeObjectProc): TLyseeFunc;overload;
    function Method(const AName: string;
      const AProc: TLyseeObjectProc): TLyseeFunc;overload;
    { define property }
    function Define(const AProp: string; const AType: TLyseeType;
      const IndexNames: array of string; const IndexTypes: array of TLyseeType;
      const GetProc: TLyseeObjectProc;
      const SetProc: TLyseeObjectProc = nil): boolean;overload;
    function Define(const AProp: string; const AType: TLyseeType;
      const IndexName: string; const IndexType: TLyseeType;
      const GetProc: TLyseeObjectProc;
      const SetProc: TLyseeObjectProc = nil): boolean;overload;
    function Define(const AProp: string; const AType: TLyseeType;
      const GetProc: TLyseeObjectProc;
      const SetProc: TLyseeObjectProc = nil): boolean;overload;
  public
    constructor Create(const AName: string; AModule: TLyseeModule; AParent: TLyseeType = nil);virtual;
    destructor Destroy;override;
    function Prototype(const AName: string): string;
    function IsTypeOf(AType: TLyseeType): boolean;
    function IsChildTypeOf(AType: TLyseeType): boolean;
    function IsBasicValue: boolean;
    function IsEnum: boolean;
    function IsEnumSet: boolean;
    function IsObject: boolean;
    function IsNil: boolean;
    function AddOperate(OP: TLyseeOperator; AType: TLyseeType; AProc: TLyseeOperateProc): PLyseeOperate;
    function FindOperate(OP: TLyseeOperator; AType: TLyseeType): PLyseeOperate;
    function AddCompare(AType: TLyseeType; AFunc: TLyseeCompareFunc): PLyseeCompare;
    function FindCompare(AType: TLyseeType): PLyseeCompare;
    property Parent: TLyseeType read FParent write SetParent;
    property Module: TLyseeModule read FModule;
    property Name: string read FName;
    property FullName: string read GetFullName;
    property TID: integer read FTID;
    property Style: TLyseeTypeStyle read FStyle;
    property Constructer: TLyseeFunc read FConstructer;
    property MethodCount: integer read GetMethodCount;
    property Methods[Index: integer]: TLyseeFunc read GetMethod;default;
  end;

  { TLyseeVariantType }

  TLyseeVariantType = class(TLyseeType)
  public
    procedure Convert(Value: TLyseeValue);override;
    procedure SetDefault(Value: TLyseeValue);override;
  end;

  { TLyseeNilType }

  TLyseeNilType = class(TLyseeType)
  public
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function AsString(Obj: pointer): string;override;
    function AsChar(Obj: pointer): char;override;
    function AsInteger(Obj: pointer): int64;override;
    function AsFloat(Obj: pointer): double;override;
    function AsCurrency(Obj: pointer): currency;override;
    function AsTime(Obj: pointer): TDateTime;override;
    function AsBoolean(Obj: pointer): boolean;override;
    function ConvertTo(Value: TLyseeValue; T: TLyseeType): boolean;override;
    procedure Convert(Value: TLyseeValue);override;
    procedure SetDefault(Value: TLyseeValue);override;
  end;

  { TLyseeCharType }

  TLyseeCharType = class(TLyseeType)
  public
    function AsString(Obj: pointer): string;override;
    function AsChar(Obj: pointer): char;override;
    function AsInteger(Obj: pointer): int64;override;
    function AsFloat(Obj: pointer): double;override;
    function AsCurrency(Obj: pointer): currency;override;
    function AsBoolean(Obj: pointer): boolean;override;
    procedure Convert(Value: TLyseeValue);override;
    procedure SetDefault(Value: TLyseeValue);override;
  end;

  { TLyseeIntegerType }

  TLyseeIntegerType = class(TLyseeType)
  public
    function AsString(Obj: pointer): string;override;
    function AsChar(Obj: pointer): char;override;
    function AsInteger(Obj: pointer): int64;override;
    function AsFloat(Obj: pointer): double;override;
    function AsCurrency(Obj: pointer): currency;override;
    function AsTime(Obj: pointer): TDateTime;override;
    function AsBoolean(Obj: pointer): boolean;override;
    procedure Convert(Value: TLyseeValue);override;
    procedure SetDefault(Value: TLyseeValue);override;
    function Generate(Obj: pointer): TLyseeGenerate;override;
  end;

  { TLyseeFloatType }

  TLyseeFloatType = class(TLyseeType)
  public
    function AsString(Obj: pointer): string;override;
    function AsInteger(Obj: pointer): int64;override;
    function AsFloat(Obj: pointer): double;override;
    function AsCurrency(Obj: pointer): currency;override;
    function AsTime(Obj: pointer): TDateTime;override;
    function AsBoolean(Obj: pointer): boolean;override;
    procedure Convert(Value: TLyseeValue);override;
    procedure SetDefault(Value: TLyseeValue);override;
  end;

  { TLyseeCurrencyType }

  TLyseeCurrencyType = class(TLyseeType)
  public
    function AsString(Obj: pointer): string;override;
    function AsInteger(Obj: pointer): int64;override;
    function AsFloat(Obj: pointer): double;override;
    function AsCurrency(Obj: pointer): currency;override;
    function AsTime(Obj: pointer): TDateTime;override;
    function AsBoolean(Obj: pointer): boolean;override;
    procedure Convert(Value: TLyseeValue);override;
    procedure SetDefault(Value: TLyseeValue);override;
  end;

  { TLyseeTimeType }

  TLyseeTimeType = class(TLyseeType)
  public
    function AsString(Obj: pointer): string;override;
    function AsInteger(Obj: pointer): int64;override;
    function AsFloat(Obj: pointer): double;override;
    function AsTime(Obj: pointer): TDateTime;override;
    function AsBoolean(Obj: pointer): boolean;override;
    procedure Convert(Value: TLyseeValue);override;
    procedure SetDefault(Value: TLyseeValue);override;
  end;

  { TLyseeBoolType }

  TLyseeBoolType = class(TLyseeType)
  public
    function AsString(Obj: pointer): string;override;
    function AsInteger(Obj: pointer): int64;override;
    function AsBoolean(Obj: pointer): boolean;override;
    procedure Convert(Value: TLyseeValue);override;
    procedure SetDefault(Value: TLyseeValue);override;
  end;

  { TLyseeTypeType }

  TLyseeTypeType = class(TLyseeType)
  protected
    procedure MyName(const Param: TLyseeParam);
    procedure MyParent(const Param: TLyseeParam);
    procedure MyModule(const Param: TLyseeParam);
    procedure MyMethods(const Param: TLyseeParam);
    procedure MyIsTypeOf(const Param: TLyseeParam);
    procedure MyIsChildTypeOf(const Param: TLyseeParam);
    procedure MyIsObject(const Param: TLyseeParam);
    procedure MyIsNil(const Param: TLyseeParam);
    procedure MyIsEnum(const Param: TLyseeParam);
    procedure MyIsEnumSet(const Param: TLyseeParam);
    procedure MyItemValues(const Param: TLyseeParam);
    procedure MyPrototype(const Param: TLyseeParam);
    procedure MyFindMethod(const Param: TLyseeParam);
    procedure Setup;override;
  public
    function AsString(Obj: pointer): string;override;
    procedure Convert(Value: TLyseeValue);override;
  end;

  { TLyseeExceptionType }

  TLyseeExceptionType = class(TLyseeType)
  protected
    procedure MyID(const Param: TLyseeParam);
    procedure MyMsg(const Param: TLyseeParam);
    procedure MyText(const Param: TLyseeParam);
    procedure MyModule(const Param: TLyseeParam);
    procedure MyFileName(const Param: TLyseeParam);
    procedure MyRow(const Param: TLyseeParam);
    procedure MyCol(const Param: TLyseeParam);
    procedure MyExcepted(const Param: TLyseeParam);
    procedure Setup;override;
  public
    function AsString(Obj: pointer): string;override;
  end;

  { TLyseeStringType }

  TLyseeStringType = class(TLyseeType)
  protected
    procedure MyGet(const Param: TLyseeParam);
    procedure Setup;override;
  public
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function AsString(Obj: pointer): string;override;
    function AsChar(Obj: pointer): char;override;
    function AsInteger(Obj: pointer): int64;override;
    function AsFloat(Obj: pointer): double;override;
    function AsCurrency(Obj: pointer): currency;override;
    function AsTime(Obj: pointer): TDateTime;override;
    function AsBoolean(Obj: pointer): boolean;override;
    procedure Convert(Value: TLyseeValue);override;
    function Generate(Obj: pointer): TLyseeGenerate;override;
    function GetLength(Obj: pointer): int64;override;
  end;

  { TLyseeFuncType }

  TLyseeFuncType = class(TLyseeType)
  protected
    procedure MyName(const Param: TLyseeParam);
    procedure MyPrototype(const Param: TLyseeParam);
    procedure MyParent(const Param: TLyseeParam);
    procedure MyModule(const Param: TLyseeParam);
    procedure MyIsMainFunc(const Param: TLyseeParam);
    procedure MyIsMethod(const Param: TLyseeParam);
    procedure MyIsConstructor(const Param: TLyseeParam);
    procedure MyIsChangeAble(const Param: TLyseeParam);
    procedure MyParamCount(const Param: TLyseeParam);
    procedure MyGetParamName(const Param: TLyseeParam);
    procedure MySetParamName(const Param: TLyseeParam);
    procedure MyGetParamType(const Param: TLyseeParam);
    procedure MySetParamType(const Param: TLyseeParam);
    procedure MyParamNames(const Param: TLyseeParam);
    procedure MyParamTypes(const Param: TLyseeParam);
    procedure MyClear(const Param: TLyseeParam);
    procedure MyClearParams(const Param: TLyseeParam);
    procedure MyClearCodes(const Param: TLyseeParam);
    procedure MyAddParam(const Param: TLyseeParam);
    procedure MyAddCode(const Param: TLyseeParam);
    procedure MySetCode(const Param: TLyseeParam);
    procedure MyGetResultType(const Param: TLyseeParam);
    procedure MySetResultType(const Param: TLyseeParam);
    procedure Setup;override;
  public
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function AsString(Obj: pointer): string;override;
  end;

  { TLyseeModuleType }

  TLyseeModuleType = class(TLyseeType)
  protected
    procedure MyName(const Param: TLyseeParam);
    procedure MyConsts(const Param: TLyseeParam);
    procedure MyTypes(const Param: TLyseeParam);
    procedure MyFuncs(const Param: TLyseeParam);
    procedure MyUsings(const Param: TLyseeParam);
    procedure MyFind(const Param: TLyseeParam);
    procedure Setup;override;
  public
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function AsString(Obj: pointer): string;override;
  end;

  { TLyseeArrayType }

  TLyseeArrayType = class(TLyseeType)
  protected
    procedure MyCreate(const Param: TLyseeParam);
    procedure MyIsEmpty(const Param: TLyseeParam);
    procedure MyClear(const Param: TLyseeParam);
    procedure MyDelete(const Param: TLyseeParam);
    procedure MyRemove(const Param: TLyseeParam);
    procedure MyExchange(const Param: TLyseeParam);
    procedure MyMove(const Param: TLyseeParam);
    procedure MySort(const Param: TLyseeParam);
    procedure MyInsert(const Param: TLyseeParam);
    procedure MyAdd(const Param: TLyseeParam);
    procedure MyIndexOf(const Param: TLyseeParam);
    procedure MyCopy(const Param: TLyseeParam);
    procedure MyLeft(const Param: TLyseeParam);
    procedure MyRight(const Param: TLyseeParam);
    procedure MyAssign(const Param: TLyseeParam);
    procedure MyGetCount(const Param: TLyseeParam);
    procedure MySetCount(const Param: TLyseeParam);
    procedure MyGet(const Param: TLyseeParam);
    procedure MySet(const Param: TLyseeParam);
    procedure Setup;override;
  public
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function AsString(Obj: pointer): string;override;
    procedure GcMark(Obj: pointer);override;
    function Generate(Obj: pointer): TLyseeGenerate;override;
    function GetLength(Obj: pointer): int64;override;
    function Clear(Obj: pointer): boolean;override;
    function Add(Obj: pointer; Value: TLyseeValue): integer;override;
  end;

  { TLyseeHashType }

  TLyseeHashType = class(TLyseeType)
  protected
    procedure MyCreate(const Param: TLyseeParam);
    procedure MyIsEmpty(const Param: TLyseeParam);
    procedure MyClear(const Param: TLyseeParam);
    procedure MyHas(const Param: TLyseeParam);
    procedure MyRemove(const Param: TLyseeParam);
    procedure MyKeys(const Param: TLyseeParam);
    procedure MyValues(const Param: TLyseeParam);
    procedure MyGet(const Param: TLyseeParam);
    procedure MySet(const Param: TLyseeParam);
    procedure Setup;override;
  public
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function AsString(Obj: pointer): string;override;
    procedure GcMark(Obj: pointer);override;
    function Clear(Obj: pointer): boolean;override;
  end;

  { TLyseeEnumItem }

  TLyseeEnumItem = class
  private
    FParent: TLyseeEnumType;
    FName: string;
    FValue: integer;
  public
    procedure SetValue(Value: TLyseeValue);
    property Parent: TLyseeEnumType read FParent;
    property Name: string read FName;
    property Value: integer read FValue;
  end;

  { TLyseeEnumType }

  TLyseeEnumType = class(TLyseeType)
  private
    FItems: array of TLyseeEnumItem;
    function GetCount: integer;
    function GetItem(Index: integer): TLyseeEnumItem;
    function GetDefValue: TLyseeEnumItem;
  public
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function AsString(Obj: pointer): string;override;
    function AsInteger(Obj: pointer): int64;override;
    procedure Convert(Value: TLyseeValue);override;
    procedure SetDefault(Value: TLyseeValue);override;
  public
    constructor Create(const AName: string; AModule: TLyseeModule; AParent: TLyseeType);override;
    destructor Destroy;override;
    procedure AddItems(const ItemNames: array of string);
    function Find(const ItemName: string): TLyseeEnumItem;overload;
    function Find(ItemValue: integer): TLyseeEnumItem;overload;
    function ItemByName(const ItemName: string): TLyseeEnumItem;
    function ItemByValue(ItemValue: integer): TLyseeEnumItem;
    procedure SetValue(Value: TLyseeValue; Item: TLyseeEnumItem);overload;
    procedure SetValue(Value: TLyseeValue; ItemValue: integer);overload;
    procedure SetValue(Value: TLyseeValue; const ItemName: string);overload;
    function NewEnumSetType(const AName: string): TLyseeEnumSetType;
    property Count: integer read GetCount;
    property Items[Index: integer]: TLyseeEnumItem read GetItem;default;
    property DefValue: TLyseeEnumItem read GetDefValue;
  end;

  { TLyseeEnumSet }

  TLyseeEnumSet = class(TBasicObject)
  private
    FParent: TLyseeEnumSetType;
    FSets: array of boolean;
    function GetSource: TLyseeEnumType;
    function GetCount: integer;
    function Get(Index: integer): boolean;
    procedure Put(Index: integer; Value: boolean);
  public
    destructor Destroy;override;
    procedure SetValue(Value: TLyseeValue);
    function AsBoolean: boolean;
    function AsString: string;override;
    function Equal(S: TLyseeEnumSet): boolean;
    function Add(S: TLyseeEnumSet): TLyseeEnumSet;
    function Dec(S: TLyseeEnumSet): TLyseeEnumSet;
    function Mul(S: TLyseeEnumSet): TLyseeEnumSet;
    function NotAll: TLyseeEnumSet;
    property Parent: TLyseeEnumSetType read FParent;
    property Source: TLyseeEnumType read GetSource;
    property Count: integer read GetCount;
    property Sets[Index: integer]: boolean read Get write Put;default;
  end;

  { TLyseeEnumSetType }

  TLyseeEnumSetType = class(TLyseeType)
  private
    FSource: TLyseeEnumType;
    FDefValue: TLyseeEnumSet;
    function GetDefValue: TLyseeEnumSet;
  public
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function AsString(Obj: pointer): string;override;
    function AsBoolean(Obj: pointer): boolean;override;
    procedure Convert(Value: TLyseeValue);override;
    procedure SetDefault(Value: TLyseeValue);override;
  public
    destructor Destroy;override;
    procedure SetValue(Value: TLyseeValue; ASet: TLyseeEnumSet);
    function NewEnumSet: TLyseeEnumSet;
    property Source: TLyseeEnumType read FSource;
    property DefValue: TLyseeEnumSet read GetDefValue;
  end;

  { TLyseeValue }

  TLyseeFind = (fiNone, fiVarb, fiFunc, fiType, fiModule, fiValue);
  TLyseeFinds = set of TLyseeFind;

  RLyseeFind = packed record
    case f_find: TLyseeFind of
    fiNone  :(VNone: pointer);
    fiVarb  :(VVarb: TLyseeVarb);
    fiFunc  :(VFunc: TLyseeFunc);
    fiType  :(VType: TLyseeType);
    fiModule:(VModule: TLyseeModule);
    fiValue :(VValue: TLyseeValue);   // constant
  end;
  PLyseeFind = ^RLyseeFind;

  PLyseeValue = ^RLyseeValue;
  RLyseeValue = packed record
    case integer of
    TID_CHAR    : (VChar: array[0..1] of char);
    TID_INTEGER : (VInteger: int64);
    TID_FLOAT   : (VFloat: double);
    TID_CURRENCY: (VCurrency: currency);
    TID_TIME    : (VTime: TDateTime);
    TID_BOOLEAN : (VBoolean: boolean);
    TID_TYPE    : (VObject: pointer);
  end;

  TLyseeValue = class
  private
    FType: TLyseeType;
    FValue: RLyseeValue;
    function GetAsInteger: int64;
    function GetAsChar: char;
    function GetAsBoolean: boolean;
    function GetAsFloat: double;
    function GetAsCurrency: currency;
    function GetAsTime: TDateTime;
    function GetAsType: TLyseeType;
    function GetAsString: string;
    function GetAsFunc: TLyseeFunc;
    function GetAsArray: TLyseeList;
    function GetAsHash: TLyseeHash;
    function GetAsModule: TLyseeModule;
    function GetAsEnum: TLyseeEnumItem;
    function GetAsEnumSet: TLyseeEnumSet;
    procedure SetAsString(const Value: string);
    procedure SetAsInteger(Value: int64);
    procedure SetAsChar(Value: char);
    procedure SetAsBoolean(Value: boolean);
    procedure SetAsFloat(Value: double);
    procedure SetAsCurrency(Value: currency);
    procedure SetAsTime(Value: TDateTime);
    procedure SetAsType(Value: TLyseeType);
    procedure SetAsFunc(Value: TLyseeFunc);
    procedure SetAsArray(Value: TLyseeList);
    procedure SetAsHash(Value: TLyseeHash);
    procedure SetAsModule(Value: TLyseeModule);
    procedure SetAsEnum(Value: TLyseeEnumItem);
    procedure SetAsEnumSet(Value: TLyseeEnumSet);
    procedure SetParentType(VT: TLyseeType);
  public
    constructor Create;virtual;
    destructor Destroy;override;
    function IncRefcount: integer;
    function DecRefcount: integer;
    procedure MarkForSurvive;
    procedure SetValue(Value: TLyseeValue);
    procedure SetNil;
    procedure SetDefault(AType: TLyseeType);
    procedure Convert(AType: TLyseeType; Cntx: TLysee);
    procedure SetFind(Finded: PLyseeFind);
    function GetOA(Wanted: TLyseeType = nil): pointer; // get object|address
    function GetTOA(var OA: pointer): TLyseeType; // get type and object|address
    procedure SetTOA(T: TLyseeType; OA: pointer);
    procedure SetObject(AType: TLyseeType; Aobj: pointer);
    function IsBasicValue: boolean;
    function IsObject: boolean;
    function IsNil: boolean;
    function IsDefv: boolean;
    function IsBoolTrue: boolean;
    function IsFalse: boolean;
    function GetFileName: string;
    function GetString: TLyseeString;
    function GetFunc: TLyseeFunc;
    function GetModule: TLyseeModule;
    function GetArray: TLyseeList;
    function GetHashed: TLyseeHash;
    function GetSelf(var Aobj): boolean;
    function Operate(OP: TLyseeOperator; Value: TLyseeValue): boolean;
    function Compare(Value: TLyseeValue): TCompare;overload;
    function Compare(Value: TLyseeValue; Wanted: TCompares): boolean;overload;
    function Same(Value: TLyseeValue): boolean;
    function NewList: TLyseeList;
    property VType: TLyseeType read FType write SetParentType;
    property AsModule: TLyseeModule read GetAsModule write SetAsModule;
    property AsFunc: TLyseeFunc read GetAsFunc write SetAsFunc;
    property AsArray: TLyseeList read GetAsArray write SetAsArray;
    property AsHash: TLyseeHash read GetAsHash write SetAsHash;
    property AsString: string read GetAsString write SetAsString;
    property AsChar: char read GetAsChar write SetAsChar;
    property AsInteger: int64 read GetAsInteger write SetAsInteger;
    property AsBoolean: boolean read GetAsBoolean write SetAsBoolean;
    property AsFloat: double read GetAsFloat write SetAsFloat;
    property AsCurrency: currency read GetAsCurrency write SetAsCurrency;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsType: TLyseeType read GetAsType write SetAsType;
    property AsEnum: TLyseeEnumItem read GetAsEnum write SetAsEnum;
    property AsEnumSet: TLyseeEnumSet read GetAsEnumSet write SetAsEnumSet;
  end;

  { TLyseeParam }

  TLyseeParam = class
  private
    FLysee: TLysee;
    FFunc: TLyseeFunc;
    FToken: TLyseeToken;
    FParams: TLyseeList;
    FVarArgs: TLyseeList;
    FPrmc: integer; {<--actual parameters' count passed to function}
    FResult: TLyseeValue;
    FPrev: TLyseeParam;
    function GetCount: integer;
    function GetItem(Index: integer): TLyseeValue;
    function GetValue(const Name: string): TLyseeValue;
    procedure SetResult(Value: TLyseeValue);
    procedure SetArgs(Args: TLyseeList);
  public
    destructor Destroy;override;
    procedure Error(const Msg: string);overload;
    procedure Error(const Msg: string; const Args: array of const);overload;
    procedure ErrorOper(OP: TLyseeOperator; L, R: TLyseeType);
    procedure ErrorChangeFunc(AFunc: TLyseeFunc);
    function TestChangeFunc(AFunc: TLyseeFunc): boolean;
    procedure BeginExec(var Mark: integer; var Tmpv: TLyseeValue);overload;
    procedure BeginExec(var Mark: integer);overload;
    procedure EndExec(Mark: integer);
    function GetVarbValue(Index: integer; var VT: TLyseeType): TLyseeValue;
    function GetSelf(var Aobj): boolean;
    function GetChangeAbleFunc(var F: TLyseeFunc): boolean;
    property Context: TLysee read FLysee;
    property Func: TLyseeFunc read FFunc;
    property Params: TLyseeList read FParams;
    property Result: TLyseeValue read FResult write SetResult;
    property Token: TLyseeToken read FToken;
    property Prmc: integer read FPrmc;
    property Count: integer read GetCount;
    property Items[Index: integer]: TLyseeValue read GetItem;default;
    property Values[const Name: string]: TLyseeValue read GetValue;
    property VarArgs: TLyseeList read FVarArgs;
  end;

  { TLyseeToken }

  TLyseeSymbol = (syError,
    {keyword}syBegin, syUses, syConst, syFunc, syProc, syArray, syVar, syGet, sySet,
    syIf, syElif, syThen, syElse, syCase, syOf, syFor, syWhile, syRepeat,
    syUntil, syDo, syResult, syDiv, syMod, syTo, syDownto, syNot, syIn, syIs,
    syAs, syLike, syAnd, syOr, syXor, syShr, syShl, syTry, syExcept,
    syFinally, syRaise, syTrue, syFalse, syNil, syVArgs, syEnd,
    {operator} syBecome, syShi, syFill, syAdd, syReduce, syMul, syDivf,
    syLParen, syRParen, syLArray, syRArray, syDot, syRange, syColon,
    sySemic, syComma, syAt, sySame, syEQ, syNE, syLT, syLE, syMT, syME, syVert,
    {abstract} syID, syNeg, syFloat, syInt, syStr, syChar, syHash,
    syCall, syEOF
  );
  TLyseeSymbols = set of TLyseeSymbol;

  RLyseeTokenValue = packed record
    case TLyseeSymbol of
    syChar :(VChar: char);
    syInt  :(VInt: int64);
    syFloat:(VFloat: double);
    syVert :(VFunc: TLyseeFunc);
  end;

  TLyseeToken = class
  private
    FSym: TLyseeSymbol;       // syTrue, syFalse
    FRow: integer;
    FCol: integer;
    FName: string;         // syBecome, syID, syStr
    FValue: RLyseeTokenValue; // syChar, syInt, syFloat
    FRight: TLyseeToken;
    FLeft: TLyseeToken;
    FParams: TList;
    function GetParamCount: integer;
    function GetParam(Index: integer): TLyseeToken;
    function SetupParamList(Param: TLyseeParam; Host: TLyseeValue; Func: TLyseeFunc): TLyseeList;
    function GetAt(Param: TLyseeParam; Host, Outv: TLyseeValue): boolean;
    function GetProp(Param: TLyseeParam; Host, Outv: TLyseeValue): boolean;
    function TryFunc(Func: TLyseeFunc; Param: TLyseeParam; Outv: TLyseeValue): boolean;
    function TryMethod(Host: TLyseeValue; const Method: string;
      Param: TLyseeParam; Outv: TLyseeValue; LastParam: TLyseeToken): boolean;
  protected
    procedure ExecID(Param: TLyseeParam; Outv: TLyseeValue);
    procedure ExecGet(Param: TLyseeParam; Outv: TLyseeValue);
    procedure ExecSet(Param: TLyseeParam);
    procedure ExecCall(Param: TLyseeParam; Outv: TLyseeValue);
  public
    constructor Create(T: TLyseeToken = nil);
    constructor CreateWithLeft(LeftBranch: TLyseeToken; T: TLyseeToken = nil);
    destructor Destroy;override;
    procedure Assign(Source: TLyseeToken);
    procedure Read(Source: TLyseeToken);
    procedure Clear;
    procedure ClearParams;
    procedure AddParam(AParam: TLyseeToken);
    procedure Error(Param: TLyseeParam; const Msg: string);overload;
    procedure Error(Param: TLyseeParam; const Msg: string; const Args: array of const);overload;
    procedure FailGet(Param: TLyseeParam; const ID: string);overload;
    procedure FailGet(Param: TLyseeParam; const Host, Prop: string);overload;
    function ExecFunc(Func: TLyseeFunc; Param: TLyseeParam; Outv: TLyseeValue; Args: TLyseeList): boolean;
    function Execute(Param: TLyseeParam; Outv: TLyseeValue): boolean;
    function Decompile(Level: integer = 0): string;
    property ParamCount: integer read GetParamCount;
    property Params[Index: integer]: TLyseeToken read GetParam;
    property Left: TLyseeToken read FLeft write FLeft;
    property Right: TLyseeToken read FRight write FRight;
  end;

  { TLyseeTokenList }

  TLyseeTokenList = class(TBasicObject)
  private
    FItems: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TLyseeToken;
    function GetLast: TLyseeToken;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Reverse;
    procedure Clear;
    procedure ClearKeepLast;
    procedure DeleteLast(N: integer);
    function Add(Pos: TLyseeToken = nil): TLyseeToken;
    function AddToken(Token: TLyseeToken): TLyseeToken;overload;
    function AddToken(Sym: TLyseeSymbol; Pos: TLyseeToken): TLyseeToken;overload;
    function AddToken(Sym: TLyseeSymbol): TLyseeToken;overload;
    property Count: integer read GetCount;
    property Items[Index: integer]: TLyseeToken read GetItem;default;
    property Last: TLyseeToken read GetLast;
  end;

  { TLyseeTokenizer }

  TLyseeTokenizer = class(TBasicObject)
  private
    FTokens: TLyseeTokenList;
    FIndex: integer;
    FCurrent: TLyseeToken;
    FCode: string;
    FSize: integer;
    FPosition: integer;
    FChar: char;
    FRow: integer;
    FCol: integer;
    FEOF: boolean;
    function NextChar: char;
    function PrevChar: char;
    function GetChar: boolean;
    function GotoChar(Chars: TSysCharSet): boolean;
    function SkipSpaces: boolean;
    function ParseHex(var I: int64): boolean;
    function ParseChar(var C: char): boolean;
    function ParseString(var S: string): boolean;
    function GetToken(token: TLyseeToken): boolean;
    function GetCurrent: TLyseeToken;
  public
    constructor Create(const Script: string);
    destructor Destroy;override;
    function PackToCurrent: boolean;
    function GetNext: TLyseeToken;
    function PeekNextSymbol: TLyseeSymbol;
    function PeekThirdSymbol: TLyseeSymbol;
    function NextIsBecome(OnHead: boolean): boolean;
    property Current: TLyseeToken read GetCurrent;
    property Row: integer read FRow write FRow;
    property Col: integer read FCol write FCol;
    property Position: integer read FPosition;
    property Code: string read FCode;
  end;

  { TLyseeParser }

  TLyseeParser = class(TBasicObject)
  private
    FTokenizer: TLyseeTokenizer; {<--token analyzer}
    FLast: TLyseeToken;          {<--last token}
    FFunc: TLyseeFunc;           {<--current function}
    FModule: TLyseeModule;       {<--current module}
    FContext: TLysee;     {<--current context}
    FAfter: integer;
    function UseToken(Token: TLyseeToken): TLyseeToken;
  protected
    procedure EUnexpected(T: TLyseeToken = nil);
    procedure ERedeclared(T: TLyseeToken = nil);
    procedure ETypeNotFound(T: TLyseeToken = nil);
    procedure ESyntax(ERow, ECol: integer; const EMsg: string; const EArgs: array of const);
    { parsing }
    procedure ParseUsesConstFunc;
    procedure ParseUses;
    procedure ParseConst;
    procedure ParseFunc;
    procedure ParseBlock(EndSyms: TLyseeSymbols; SX: TLyseeSTMTList);
    procedure ParseStatement(OnHead: boolean; SX: TLyseeSTMTList);
    procedure ParseType(OnHead: boolean; var T: TLyseeType);
    procedure ParseArguments(EndSym: TLyseeSymbol);
    { statement }
    procedure ParseVar(SX: TLyseeSTMTList);
    procedure ParseIf(SX: TLyseeSTMTList);
    procedure ParseFor(SX: TLyseeSTMTList);
    procedure ParseWhile(SX: TLyseeSTMTList);
    procedure ParseRepeat(SX: TLyseeSTMTList);
    procedure ParseCase(SX: TLyseeSTMTList);
    procedure ParseTry(SX: TLyseeSTMTList);
    procedure ParseRaise(SX: TLyseeSTMTList);
    procedure ParsePuts(SX: TLyseeSTMTList);
    procedure ParseAny(SX: TLyseeSTMTList);
    { expression }
    function ParseExpr(OnHead: boolean; EndSyms: TLyseeSymbols; DoCheck: boolean): TLyseeToken;
    function ParseFact(Level: integer): TLyseeToken;
    function ParseTerm: TLyseeToken;
    { token }
    procedure SymGetNext;
    procedure SymGotoNext;
    procedure SymTestNext(Syms: TLyseeSymbols);
    procedure SymTestLast(Syms: TLyseeSymbols);
    procedure SymTestLastID;
    procedure SymTestNextID;
    function SymPeekNext: TLyseeSymbol;
  public
    constructor Create(AModule: TLyseeModule);
    destructor Destroy;override;
    function Parse(const Code: string; UsingModule: boolean = false): TLyseeFunc;
    function ParseAndFree(const Code: string; UsingModule: boolean = false): TLyseeFunc;
  end;

  { TLyseeSTMT }

  TLyseeSTMTStyle = (ssNormal, ssConst, ssAssign, ssResult, ssPuts, ssIf,
                     ssWhile, ssRepeat, ssFor, ssCase, ssTry, ssRaise);

  TLyseeSTMT = class
  private
    FStyle: TLyseeSTMTStyle;
    FParent: TLyseeSTMTList;
    FItems: TLyseeSTMTList;
    FExpr: TLyseeToken;
    function GetItems: TLyseeSTMTList;
    function GetCount: integer;
  protected
    procedure ExecNormal(Param: TLyseeParam);
    procedure ExecPuts(Param: TLyseeParam);
    procedure ExecWhile(Param: TLyseeParam);
    procedure ExecRepeat(Param: TLyseeParam);
    procedure ExecRaise(Param: TLyseeParam);
  public
    constructor Create(AParent: TLyseeSTMTList);virtual;
    destructor Destroy;override;
    function Execute(Param: TLyseeParam): boolean;virtual;
    procedure Decompile(Level: integer; Lines: TStrings);virtual;
    property Style: TLyseeSTMTStyle read FStyle;
    property Parent: TLyseeSTMTList read FParent;
    property Items: TLyseeSTMTList read GetItems;
    property Count: integer read GetCount;
    property Expr: TLyseeToken read FExpr;
  end;

  { TLyseeAssign }

  TLyseeAssign = class(TLyseeSTMT)
  private
    FVarb: string;
  protected
    procedure ExecConst(Param: TLyseeParam);
    procedure ExecAssign(Param: TLyseeParam);
    procedure ExecResult(Param: TLyseeParam);
  public
    constructor Create(AParent: TLyseeSTMTList);override;
    function Execute(Param: TLyseeParam): boolean;override;
    procedure Decompile(Level: integer; Lines: TStrings);override;
    property Varb: string read FVarb;
  end;

  { TLyseeFor }

  TLyseeFor = class(TLyseeAssign)
  private
    FUpTo: boolean;
    FEndValue: TLyseeToken;
  public
    constructor Create(AParent: TLyseeSTMTList);override;
    destructor Destroy;override;
    function Execute(Param: TLyseeParam): boolean;override;
    procedure Decompile(Level: integer; Lines: TStrings);override;
    property EndValue: TLyseeToken read FEndValue;
    property UpTo: boolean read FUpTo;
  end;

  { TLyseeIf }

  TLyseeIf = class(TLyseeSTMT)
  private
    FElseItems: TLyseeSTMTList;
    function GetElseItems: TLyseeSTMTList;
    function GetElseCount: integer;
  public
    constructor Create(AParent: TLyseeSTMTList);override;
    destructor Destroy;override;
    function Execute(Param: TLyseeParam): boolean;override;
    procedure Decompile(Level: integer; Lines: TStrings);override;
    function IsElif: boolean;
    property ElseCount: integer read GetElseCount;
    property ElseItems: TLyseeSTMTList read GetElseItems;
  end;

  { TLyseeCase }

  TLyseeCase = class(TLyseeIf)
  public
    constructor Create(AParent: TLyseeSTMTList);override;
    function Execute(Param: TLyseeParam): boolean;override;
    procedure Decompile(Level: integer; Lines: TStrings);override;
  end;

  { TLyseeTry }

  TLyseeTry = class(TLyseeIf)
  private
    FTryFinally: boolean;
  public
    constructor Create(AParent: TLyseeSTMTList);override;
    function Execute(Param: TLyseeParam): boolean;override;
    procedure Decompile(Level: integer; Lines: TStrings);override;
    property TryFinally: boolean read FTryFinally;
  end;

  { TLyseeSTMTList }

  TLyseeSTMTList = class
  private
    FItems: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TLyseeSTMT;
  public
    destructor Destroy;override;
    procedure Clear;
    procedure Delete(Index: integer);
    function Add(Style: TLyseeSTMTStyle): TLyseeSTMT;overload;
    function Add(STMT: TLyseeSTMT): integer;overload;
    function Execute(Param: TLyseeParam): boolean;
    procedure Decompile(Level: integer; Lines: TStrings);
    property Count: integer read GetCount;
    property Items[Index: integer]: TLyseeSTMT read GetItem;default;
  end;

  { TLyseeVarb }

  TLyseeVarb = class
  private
    FName: string;
    FType: TLyseeType;
  public
    constructor Create(const AName: string; AType: TLyseeType);
    destructor Destroy;override;
    function Prototype: string;
    property Name: string read FName;
    property ValueType: TLyseeType read FType;
  end;

  { TLyseeVarbList}

  TLyseeVarbList = class
  private
    FVarbs: array of TLyseeVarb;
    FLocalCount: integer;
    function GetCount: integer;
    function GetParamCount: integer;
    function GetVarb(Index: integer): TLyseeVarb;
    function DoAdd(const AName: string; AType: TLyseeType): TLyseeVarb;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    procedure Assign(Source: TLyseeVarbList);
    function Add(const AName: string; AType: TLyseeType): TLyseeVarb;
    function AddLocal(const AName: string; AType: TLyseeType): TLyseeVarb;
    function IndexOf(const AName: string): integer;overload;
    function IndexOf(const AVarb: TLyseeVarb): integer;overload;
    function Find(const AName: string): TLyseeVarb;
    property Count: integer read GetCount;
    property LocalCount: integer read FLocalCount;
    property ParamCount: integer read GetParamCount;
    property Varbs[Index: integer]: TLyseeVarb read GetVarb;default;
  end;

  { TLyseeFunc }

  TLyseeFunc = class(TBasicObject)
  private
    FModule: TLyseeModule;
    FName: string;
    FParent: TLyseeType;
    FParams: TLyseeVarbList;
    FResultType: TLyseeType;
    FProc: TLyseeProc;
    FData: pointer;
    FMinArgs: integer;
    FSTMTs: TLyseeSTMTList;
    function GetSTMTs: TLyseeSTMTList;
    function GetMinArgs: integer;
    function GetFullName: string;
  public
    constructor Create(const AName: string; M: TLyseeModule; Proc: TLyseeProc);
    constructor CreateMethod(const AName: string; P: TLyseeType; Proc: TLyseeProc);
    destructor Destroy;override;
    function Prototype: string;
    function FindInside(const ID: string; rec: PLyseeFind = nil): boolean;
    function FindBy(const ID: string; rec: PLyseeFind; Range: TLyseeFinds = []): boolean;
    function FindSave(const ID: string; Param: TLyseeParam; Outv: TLyseeValue): TLyseeFind;
    function Context: TLysee;
    function IsMainFunc: boolean;
    function IsMethod: boolean;
    function IsConstructor: boolean;
    function IsProcedure: boolean;
    function IsFunction: boolean;
    function IsConst: boolean;
    function MakeMethod: TLyseeFunc;
    function Executing: boolean;
    function ChangeAble: boolean;
    function AddCode(const Code: string): boolean;
    function SetCode(const Code: string): boolean;
    procedure Decompile(Level: integer; Lines: TStrings);
    function AsString: string;override;
    property Name: string read FName;
    property Parent: TLyseeType read FParent;
    property FullName: string read GetFullName;
    property MinArgs: integer read GetMinArgs;
    property Module: TLyseeModule read FModule;
    property ResultType: TLyseeType read FResultType write FResultType;
    property Params: TLyseeVarbList read FParams;
    property Proc: TLyseeProc read FProc write FProc;
    property Data: pointer read FData;
    property STMTs: TLyseeSTMTList read GetSTMTs;
  end;

  { TLyseeFuncList }

  TLyseeFuncList = class
  private
    FItems: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TLyseeFunc;
  public
    constructor Create;
    destructor Destroy;override;
    function Get(const Name: string): TLyseeFunc;
    procedure Clear;
    procedure Delete(Index: integer);
    property Count: integer read GetCount;
    property Items[Index: integer]: TLyseeFunc read GetItem;default;
  end;

  { TLyseeModule }

  TLyseeModule = class(TNamedObject)
  private
    FContext: TLysee;    {<--private: owner context}
    FModules: TLyseeModuleList; {<--private: modules used by this module}
    FImporters: TList;       {<--private: modules useing this module}
    FFileName: string;
    FTypeList: TList;
    FFuncList: TLyseeFuncList;
    FConsts: TLyseeHash;
    FOnSetup: TNotifyEvent;
    procedure InitModule;
    procedure DeleteFunctions;
    procedure DeleteTypes;
  protected
    property OnSetup: TNotifyEvent read FOnSetup write FOnSetup;
  public
    function AddFunc(const AName: string; T: TLyseeType;
      const ParamNames: array of string; const ParamTypes: array of TLyseeType;
      const Proc: TLyseeProc;
      const Data: pointer = nil): TLyseeFunc;overload;
    function AddFunc(const AName: string;
      const ParamNames: array of string; const ParamTypes: array of TLyseeType;
      const Proc: TLyseeProc;
      const Data: pointer = nil): TLyseeFunc;overload;
    function AddFunc(const AName: string; T: TLyseeType;
      const Proc: TLyseeProc;
      const Data: pointer = nil): TLyseeFunc;overload;
    function AddFunc(const AName: string;
      const Proc: TLyseeProc;
      const Data: pointer = nil): TLyseeFunc;overload;
  public
    constructor Create(const AName: string);override;
    constructor CreateEx(const AName: string; AContext: TLysee);
    destructor Destroy;override;
    procedure Setup;virtual;
    procedure Use(M: TLyseeModule);
    function EnsureName(const AName: string): string;
    function AddEnumType(const AName: string;
      const ItemNames: array of string): TLyseeEnumType;
    function IsMainModule: boolean;
    function SetupType(const T: TLyseeType): boolean;
    function TypeCount: integer;
    function GetType(Index: integer): TLyseeType;
    function FindModule(const ID: string; FindPossible: boolean): TLyseeModule;
    function FindType(const ID: string): TLyseeType;
    function FindTypeBy(const ID, AModule: string): TLyseeType;
    function FindFunc(const ID: string): TLyseeFunc;
    function FindFuncBy(const ID: string): TLyseeFunc;
    function Find(const ID: string; rec: PLyseeFind = nil): boolean;
    function FindBy(const ID, AModule: string; rec: PLyseeFind): boolean;
    function FindSave(const AName: string; Value: TLyseeValue): boolean;
    function FindSaveBy(const AName: string; Value: TLyseeValue): boolean;
    function UseModule(const AName: string): TLyseeModule;
    function AsString: string;override;
    property Modules: TLyseeModuleList read FModules;
    property FileName: string read FFileName write FFileName;
    property Context: TLysee read FContext;
    property Consts: TLyseeHash read FConsts;
  end;

  { TLyseeModuleList }

  TLyseeModuleList = class(TBasicObject)
  private
    FContext: TLysee;
    FImporter: TLyseeModule;
    FModules: TList;
    function GetModule(Index: integer): TLyseeModule;
    function GetCount: integer;
  public
    constructor Create(AContext: TLysee);
    destructor Destroy;override;
    procedure Setup;
    function IndexOf(AModule: TLyseeModule): integer;overload;
    function IndexOf(const Name: string): integer;overload;
    function Has(AModule: TLyseeModule): boolean;overload;
    function Has(const Name: string): boolean;overload;
    function Find(const Name: string): TLyseeModule;
    function Add(AModule: TLyseeModule): integer;
    procedure Delete(Index: integer);
    procedure Clear;
    procedure DeleteFunctions;
    procedure ClearConsts;
    function ToList: TLyseeList;
    property Count: integer read GetCount;
    property Modules[Index: integer]: TLyseeModule read GetModule;default;
  end;

  { TLyseeString }

  TLyseeString = class(TBasicObject)
  private
    FValue: string;
  public
    constructor Create(const S: string);
    constructor CreateIncRefcount(const S: string);
    function Length: integer;
    function AsString: string;override;
    property Value: string read FValue write FValue;
  end;

  { TLyseeGarbage }

  TLyseeGarbage = class(TBasicObject)
  private
    FPrev: TLyseeGarbage;
    FNext: TLyseeGarbage;
    FInChain: boolean;
    FSurvived: boolean;
  protected
    procedure MarkForSurvive;virtual;
  public
    constructor Create;virtual;
    destructor Destroy;override;
    procedure Clear;virtual;
    property Survived: boolean read FSurvived write FSurvived;
    property InChain: boolean read FInChain write FInChain;
  end;

  { TLyseeCollect }

  TLyseeCollect = class
  private
    FChain: TLyseeGarbage;
    FDead: TList;
    FContexts: TList;
  protected
    procedure MarkSurvived;virtual;
    procedure GcAdd(G: TLyseeGarbage);
    procedure GcRemove(G: TLyseeGarbage);
  public
    constructor Create;virtual;
    destructor Destroy;override;
    function Collect: integer;virtual;
  end;

  { TLyseeList }

  TLyseeList = class(TLyseeGarbage)
  private
    FItems: TList;
    FFormating: boolean;
    FReadonly: boolean;
    function GetCount: integer;
    procedure SetCount(NewCount: integer);
    function GetItem(Index: integer): TLyseeValue;
  protected
    procedure MarkForSurvive;override;
    procedure TestChange;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure Clear;override;
    procedure Delete(Index: integer);
    procedure DeleteLast;
    procedure Remove(Value: TLyseeValue);
    procedure Exchange(Index1, Index2: integer);
    procedure Move(CurIndex, NewIndex: integer);
    procedure GetFirstTwo(var V1, V2: TLyseeValue);
    procedure PrepareFor(Func: TLyseeFunc);
    procedure Sort;
    procedure Assign(AList: TLyseeList);
    procedure AddStrings(Strs: TStrings);
    function Add(Value: TLyseeValue = nil): TLyseeValue;
    function AddList: TLyseeList;
    function Insert(Index: integer; Value: TLyseeValue = nil): TLyseeValue;
    function IndexOf(Value: TLyseeValue): integer;
    function Copy(Index, ItemCount: integer): TLyseeList;
    function CopyLeft(ItemCount: integer): TLyseeList;
    function CopyRight(ItemCount: integer): TLyseeList;
    function Clone: TLyseeList;
    function First: TLyseeValue;
    function Last: TLyseeValue;
    function AsString: string;override;
    property Count: integer read GetCount write SetCount;
    property Items[Index: integer]: TLyseeValue read GetItem;default;
    property Readonly: boolean read FReadonly write FReadonly;
  end;

  { TLyseeHashItem }

  TLyseeHashItem = class(TLyseeValue)
  private
    FKey: string;
    FNext: TLyseeHashItem;
  public
    function Format: string;
    property Key: string read FKey;
  end;

  { TLyseeHash }

  TLyseeHash = class(TLyseeGarbage)
  private
    FBuckets: array of TLyseeHashItem;
    FCount: integer;
    FFormating: boolean;
    FCaseSensitive: boolean;
    procedure FreeItem(H: TLyseeHashItem);
  protected
    procedure MarkForSurvive;override;
    function BucketCount: integer;
    function MatchName(const N1, N2: string): boolean;
    function HashIndex(const Name: string): integer;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure Clear;override;
    procedure Resize(NewSize: integer);
    procedure ListKeys(List: TLyseeList);
    procedure ListValues(List: TLyseeList);
    function Add(const Name: string): TLyseeHashItem;
    function Get(const Name: string): TLyseeHashItem;
    function GetValue(const Name: string; Value: TLyseeValue): boolean;
    function Has(const Name: string): boolean;
    function Remove(const Name: string): boolean;
    function IsEmpty: boolean;
    function SetupItemList: TList;
    function DefConst(const Name, Value: string): TLyseeHashItem;overload;
    function DefConst(const Name: string; Value: int64): TLyseeHashItem;overload;
    function DefConst(const Name: string; Value: double): TLyseeHashItem;overload;
    function DefConst(const Name: string; Value: boolean): TLyseeHashItem;overload;
    function AsString: string;override;
    property Count: integer read FCount;
    property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive;
  end;

  { TLyseeGenerate }

  TLyseeGenerate = class(TLyseeValue)
  public
    function GetNext: boolean;virtual;
    function HasNext: boolean;virtual;
  end;

  { TLyseeStringGenerate }

  TLyseeStringGenerate = class(TLyseeGenerate)
  private
    FS: string;
    FIndex: integer;
  public
    constructor CreateIn(const S: string);
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLyseeListGenerate }

  TLyseeListGenerate = class(TLyseeGenerate)
  private
    FL: TLyseeList;
    FIndex: integer;
  public
    constructor CreateIn(const AList: TLyseeList);
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLyseeIntGenerate }

  TLyseeIntGenerate = class(TLyseeGenerate)
  private
    FV, FCount: int64;
    FUpto: boolean;
  public
    constructor CreateIn(V1, V2: int64; Upto: boolean);overload;
    constructor CreateIn(Range: int64);overload;
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLyseeCharGenerate }

  TLyseeCharGenerate = class(TLyseeGenerate)
  private
    FV: char;
    FCount: integer;
    FUpto: boolean;
  public
    constructor CreateIn(V1, V2: char; Upto: boolean);
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLyseeBoolGenerate }

  TLyseeBoolGenerate = class(TLyseeGenerate)
  private
    FV: boolean;
    FCount: integer;
    FUpto: boolean;
  public
    constructor CreateIn(V1, V2, Upto: boolean);
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLyseeEnumGenerate }

  TLyseeEnumGenerate = class(TLyseeGenerate)
  private
    FV: TLyseeEnumItem;
    FCount: integer;
    FUpto: boolean;
  public
    constructor CreateIn(V1, V2: TLyseeEnumItem; Upto: boolean);
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLyseeSystemModule }

  TLyseeSystemModule = class(TLyseeModule)
  private
    procedure DoSetup(Sender: TObject);
  public
    constructor Create(const AName: string);override;
  end;

function AcquireLock: boolean;
function ReleaseLock: boolean;
function AddModule(const Name: string): TLyseeModule;
function Command: integer;
function MatchID(const ID1, ID2: string): boolean;overload;
function MatchID(const ID: string; const IDList: array of string): boolean;overload;
function OperatorStr(OP: TLyseeOperator): string;
function Keywords: string;
function Hilights: string;
function ExtractNameModule(const ID: string; var Module: string): string;
function FormatChar(C: char): string;
function FormatString(const S: string): string;
function FormatValue(V: TLyseeValue): string;
function CompleteFileName(const FileName: string): string;
function SortValueCompare(V1, V2: pointer): integer;
procedure ErrorConvert(AType, NewType: TLyseeType);
function GetGenerate(Value: TLyseeValue): TLyseeGenerate;overload;
function GetGenerate(V1, V2: TLyseeValue; Upto: boolean): TLyseeGenerate;overload;
function Margin(Level: integer): string;
procedure FreeOperates(var Head: PLyseeOperate);
procedure FreeCompares(var Head: PLyseeCompare);
function ParamOK(const Names: array of string; const Types: array of TLyseeType;
  IsMethod: boolean): boolean;

function AddFunc(const Name: string; T: TLyseeType;
  const ParamNames: array of string; const ParamTypes: array of TLyseeType;
  Proc: TLyseeProc; Data: pointer = nil): TLyseeFunc;overload;

function AddFunc(const Name: string;
  const ParamNames: array of string; const ParamTypes: array of TLyseeType;
  Proc: TLyseeProc; Data: pointer = nil): TLyseeFunc;overload;

function AddFunc(const Name: string; T: TLyseeType;
  Proc: TLyseeProc; Data: pointer = nil): TLyseeFunc;overload;

function AddFunc(const Name: string;
  Proc: TLyseeProc; Data: pointer = nil): TLyseeFunc;overload;

function AddMethod(const AClass: TLyseeType; const AName: string;
  const AType: TLyseeType; const ParamNames: array of string;
  const ParamTypes: array of TLyseeType; const AProc: TLyseeProc): TLyseeFunc;overload;

function AddMethod(const AClass: TLyseeType; const AName: string;
  const ParamNames: array of string;
  const ParamTypes: array of TLyseeType; const AProc: TLyseeProc): TLyseeFunc;overload;

function AddMethod(const AClass: TLyseeType; const AName: string;
  const AType: TLyseeType; const AProc: TLyseeProc): TLyseeFunc;overload;

function AddMethod(const AClass: TLyseeType; const AName: string;
  const AProc: TLyseeProc): TLyseeFunc;overload;

function SetupProp(const AClass: TLyseeType;
  const AName: string; const AType: TLyseeType;
  const IndexNames: array of string;
  const IndexTypes: array of TLyseeType;
  const GetProc, SetProc: TLyseeProc;
  var GetFunc, SetFunc: TLyseeFunc): boolean;overload;

function SetupProp(const AClass: TLyseeType;
  const AName: string; const AType: TLyseeType;
  const IndexNames: array of string;
  const IndexTypes: array of TLyseeType;
  const GetProc: TLyseeProc;
  var GetFunc: TLyseeFunc): boolean;overload;

function SetupProp(const AClass: TLyseeType;
  const AName: string; const AType: TLyseeType;
  const IndexNames: array of string;
  const IndexTypes: array of TLyseeType;
  const GetProc: TLyseeProc;
  const SetProc: TLyseeProc = nil): boolean;overload;

function SetupProp(const AClass: TLyseeType;
  const AName: string; const AType: TLyseeType;
  const GetProc: TLyseeProc;
  const SetProc: TLyseeProc = nil): boolean;overload;

var
  my_program    : string;             {<--program file name}
  my_kernel     : string;             {<--kernel file name}
  my_gcman      : TLyseeCollect;      {<--unique garbage collection manager}
  my_system     : TLyseeSystemModule; {<--System module}
  my_modules    : TLyseeModuleList;   {<--public module list}
  my_types      : array[TID_VARIANT..TID_ARRAY] of TLyseeType;
  my_knpath     : string;             {<--kernel file path}
  my_kndir      : string;             {<--kernel file directory}
  my_home       : string;             {<--home path}
  my_tmpath     : string;             {<--temporary path}
  my_search_path: string;             {<--module search path list}
  my_variant    : TLyseeVariantType;
  my_nil        : TLyseeNilType;
  my_char       : TLyseeCharType;
  my_int        : TLyseeIntegerType;
  my_float      : TLyseeFloatType;
  my_curr       : TLyseeCurrencyType;
  my_time       : TLyseeTimeType;
  my_bool       : TLyseeBoolType;
  my_type       : TLyseeTypeType;
  my_exception  : TLyseeExceptionType;
  my_string     : TLyseeStringType;
  my_module     : TLyseeModuleType;
  my_func       : TLyseeFuncType;
  my_hash       : TLyseeHashType;
  my_array      : TLyseeArrayType;

implementation

{$IFDEF FPC}
type PBoolean = ^boolean;
{$ENDIF}

const

  LSE_HASH_DELTA = 64;

  Symbols: array[TLyseeSymbol] of packed record
    SY: TLyseeSymbol; // symbol value
    ID: string;    // symbol spelling
    SM: string;     // symbol description
  end = (
    (SY:syError;     ID:'';          SM:'error'),
    (SY:syBegin;     ID:'begin';     SM:'begin'),
    (SY:syUses;      ID:'uses';      SM:'uses module'),
    (SY:syConst;     ID:'const';     SM:'define constant'),
    (SY:syFunc;      ID:'function';  SM:'define function'),
    (SY:syProc;      ID:'procedure'; SM:'define procedure'),
    (SY:syArray;     ID:'array';     SM:'array'),
    (SY:syVar;       ID:'var';       SM:'var'),
    (SY:syGet;       ID:'get';       SM:'get'),
    (SY:sySet;       ID:'set';       SM:'set'),
    (SY:syIf;        ID:'if';        SM:'if'),
    (SY:syElif;      ID:'elif';      SM:'else if'),
    (SY:syThen;      ID:'then';      SM:'then'),
    (SY:syElse;      ID:'else';      SM:'else'),
    (SY:syCase;      ID:'case';      SM:'case'),
    (SY:syOf;        ID:'of';        SM:'of'),
    (SY:syFor;       ID:'for';       SM:'for'),
    (SY:syWhile;     ID:'while';     SM:'while'),
    (SY:syRepeat;    ID:'repeat';    SM:'repeat'),
    (SY:syUntil;     ID:'until';     SM:'until'),
    (SY:syDo;        ID:'do';        SM:'do'),
    (SY:syResult;    ID:'result';    SM:'result'),
    (SY:syDiv;       ID:'div';       SM:'integer decrease'),
    (SY:syMod;       ID:'mod';       SM:'mod'),
    (SY:syTo;        ID:'to';        SM:'to'),
    (SY:syDownto;    ID:'downto';    SM:'downto'),
    (SY:syNot;       ID:'not';       SM:'true <--> false'),
    (SY:syIn;        ID:'in';        SM:'in'),
    (SY:syIs;        ID:'is';        SM:'type checking'),
    (SY:syAs;        ID:'as';        SM:'type casting'),
    (sy:syLike;      ID:'like';      SM:'string like patten'),
    (SY:syAnd;       ID:'and';       SM:'logical and'),
    (SY:syOr;        ID:'or';        SM:'logical or'),
    (SY:syXor;       ID:'xor';       SM:'xor'),
    (SY:syShr;       ID:'shr';       SM:'shift right'),
    (SY:syShl;       ID:'shl';       SM:'shift left'),
    (SY:syTry;       ID:'try';       SM:'try'),
    (SY:syExcept;    ID:'except';    SM:'except'),
    (SY:syFinally;   ID:'finally';   SM:'finally'),
    (SY:syRaise;     ID:'raise';     SM:'raise exception'),
    (SY:syTrue;      ID:'true';      SM:'true'),
    (SY:syFalse;     ID:'false';     SM:'false'),
    (SY:syNil;       ID:'nil';       SM:'nil'),
    (SY:syVArgs;     ID:'vargs';     SM:'ellipsis'),
    (SY:syEnd;       ID:'end';       SM:'end'),
    (SY:syBecome;    ID:':=';        SM:'become'),
    (SY:syShi;       ID:'<<';        SM:'shift into'),
    (SY:syFill;      ID:'<<<';       SM:'fill'),
    (SY:syAdd;       ID:'+';         SM:'add'),
    (SY:syReduce;    ID:'-';         SM:'reduce'),
    (SY:syMul;       ID:'*';         SM:'mul'),
    (SY:syDivf;      ID:'/';         SM:'float div'),
    (SY:syLParen;    ID:'(';         SM:'left paren'),
    (SY:syRParen;    ID:')';         SM:'right paren'),
    (SY:syLArray;    ID:'[';         SM:'L: set item'),
    (SY:syRArray;    ID:']';         SM:'R: get item'),
    (SY:syDot;       ID:'.';         SM:'dot'),
    (SY:syRange;     ID:'..';        SM:'range to'),
    (SY:syColon;     ID:':';         SM:'colon'),
    (SY:sySemic;     ID:';';         SM:'semicolon'),
    (SY:syComma;     ID:',';         SM:'comma'),
    (SY:syAt;        ID:'@';         SM:'at'),
    (SY:sySame;      ID:'==';        SM:'same'),
    (SY:syEQ;        ID:'=';         SM:'equal'),
    (SY:syNE;        ID:'<>';        SM:'not equal'),
    (SY:syLT;        ID:'<';         SM:'less'),
    (SY:syLE;        ID:'<=';        SM:'less equal'),
    (SY:syMT;        ID:'>';         SM:'more'),
    (SY:syME;        ID:'>=';        SM:'more equal'),
    (SY:syVert;      ID:'|';         SM:'vertical bar'),
    (SY:syID;        ID:'';          SM:'identity name'),
    (SY:syNeg;       ID:'';          SM:'- value'),
    (SY:syFloat;     ID:'';          SM:'push float'),
    (SY:syInt;       ID:'';          SM:'push int'),
    (SY:syStr;       ID:'';          SM:'push string'),
    (SY:syChar;      ID:'';          SM:'push char'),
    (SY:syHash;      ID:'';          SM:'create hash table'),
    (SY:syCall;      ID:'';          SM:'call function'),
    (sy:syEOF;       ID:'';          SM:'end of file')
  );

  NameSyms = [syID, syStr, syCall, syGet, sySet];
  DataSyms = [syID, syResult, syFloat, syInt, syStr, syChar, syTrue,
              syFalse, syNil, syVArgs, syArray];
  ExprHead = DataSyms + [syLParen, syLArray, syNot, syReduce, syAt, syVert];

  OperSyms = [
    syMul, syDiv, syDivf, syMod,                                            // 0
    syAdd, syReduce,                                                        // 1
    syXor, syShl, syShr, syShi, syFill,                                     // 2
    sySame, syEQ, syNE, syLT, syLE, syMT, syME, syIn, syLike, syAs, syIs,   // 3
    syAnd, syOr];                                                           // 4

  OperLevel: array[0..4] of TLyseeSymbols = (
    [syMul, syDiv, syDivf, syMod],                                          // 0
    [syAdd, syReduce],                                                      // 1
    [syXor, syShl, syShr, syShi, syFill],                                   // 2
    [sySame, syEQ, syNE, syLT, syLE, syMT, syME, syIn, syLike, syAs, syIs], // 3
    [syAnd, syOr]                                                           // 4
  );

var

  my_spinlock: Syncobjs.TCriticalSection;
  my_keywords: string;
  my_hilights: string;
  my_TID_seed: integer = 0;

function IsParam(const ID: string; const T: TLyseeType): boolean;
begin
  Result := (T <> nil) and (T <> my_nil) and IsID(ID);
end;

procedure LyseeObjectProc(const Param: TLyseeParam);
begin
  Param.FFunc.FParent.FProcs[PtrToInt(Param.FFunc.FData)](Param);
end;

// operator --------------------------------------------------------------------

procedure variant_in_array(V1, V2: TLyseeValue);
var
  I: integer;
  A: TLyseeList;
begin
  A := TLyseeList(V2.GetOA);
  if A <> nil then
    I := A.IndexOf(V1) else
    I := -1;
  V1.AsBoolean := (I >= 0);
end;

procedure variant_is_type(V1, V2: TLyseeValue);
begin
  V1.SetAsBoolean(V1.AsType.IsTypeOf(V2.AsType));
end;

procedure variant_as_type(V1, V2: TLyseeValue);
begin
  V2.AsType.Convert(V1);
end;

procedure variant_shi_variant(V1, V2: TLyseeValue);
var
  J: pointer;
begin
  if V1.GetSelf(J) then
    V1.FType.Add(J, V2);
end;

procedure variant_fill_variant(V1, V2: TLyseeValue);
var
  G: TLyseeGenerate;
  J: pointer;
begin
  if V1.GetSelf(J) then
  begin
    G := GetGenerate(V2);
    if G <> nil then
    try
      while G.GetNext do
        V1.FType.Add(J, G);
    finally
      G.Free;
    end;
  end;
end;

procedure string_add_string(V1, V2: TLyseeValue);
begin
  V1.SetAsString(V1.AsString + V2.AsString);
end;

procedure string_mul_int(V1, V2: TLyseeValue);
var
  I: integer;
  S, T: string;
begin
  S := '';
  T := V1.AsString;
  if T <> '' then
  begin
    I := V2.AsInteger;
    while I > 0 do
    begin
      S := S + T;
      Dec(I);
    end;
  end;
  V1.SetAsString(S);
end;

procedure string_in_string(V1, V2: TLyseeValue);
begin
  V1.SetAsBoolean(Pos(V1.AsString, V2.AsString) > 0);
end;

procedure string_in_hashed(V1, V2: TLyseeValue);
var
  H: TLyseeHash;
begin
  H := V2.AsHash;
  V1.SetAsBoolean((H <> nil) and H.Has(V1.AsString));
end;

procedure string_like_string(V1, V2: TLyseeValue);
begin
  V1.SetAsBoolean(MatchPatten(V1.AsString, V2.AsString));
end;

procedure int_neg_int(V1, V2: TLyseeValue);
begin
  V1.FValue.VInteger := - V1.FValue.VInteger;
end;

procedure int_not_int(V1, V2: TLyseeValue);
begin
  V1.FValue.VInteger := not V1.FValue.VInteger;
end;

procedure int_add_int(V1, V2: TLyseeValue);
begin
  Inc(V1.FValue.VInteger, V2.FValue.VInteger);
end;

procedure int_add_float(V1, V2: TLyseeValue);
begin
  V1.FType := my_float;
  V1.FValue.VFloat := V1.FValue.VInteger + V2.FValue.VFloat;
end;

procedure int_add_money(V1, V2: TLyseeValue);
begin
  V1.FType := my_curr;
  V1.FValue.VCurrency := V1.FValue.VInteger + V2.FValue.VCurrency;
end;

procedure int_dec_int(V1, V2: TLyseeValue);
begin
  Dec(V1.FValue.VInteger, V2.FValue.VInteger);
end;

procedure int_dec_float(V1, V2: TLyseeValue);
begin
  V1.FType := my_float;
  V1.FValue.VFloat := V1.FValue.VInteger - V2.FValue.VFloat;
end;

procedure int_dec_money(V1, V2: TLyseeValue);
begin
  V1.FType := my_curr;
  V1.FValue.VCurrency := V1.FValue.VInteger - V2.FValue.VCurrency;
end;

procedure int_mul_int(V1, V2: TLyseeValue);
begin
  V1.FValue.VInteger := V1.FValue.VInteger * V2.FValue.VInteger;
end;

procedure int_mul_float(V1, V2: TLyseeValue);
begin
  V1.FType := my_float;
  V1.FValue.VFloat := V1.FValue.VInteger * V2.FValue.VFloat;
end;

procedure int_mul_money(V1, V2: TLyseeValue);
begin
  V1.FType := my_curr;
  V1.FValue.VCurrency := V1.FValue.VInteger * V2.FValue.VCurrency;
end;

procedure int_div_int(V1, V2: TLyseeValue);
begin
  V1.FType := my_int;
  V1.FValue.VInteger := V1.FValue.VInteger div V2.FValue.VInteger;
end;

procedure int_divf_int(V1, V2: TLyseeValue);
begin
  V1.FType := my_float;
  V1.FValue.VFloat := V1.FValue.VInteger / V2.FValue.VInteger;
end;

procedure int_divf_float(V1, V2: TLyseeValue);
begin
  V1.FType := my_float;
  V1.FValue.VFloat := V1.FValue.VInteger / V2.FValue.VFloat;
end;

procedure int_divf_money(V1, V2: TLyseeValue);
begin
  V1.FType := my_float;
  V1.FValue.VFloat := V1.FValue.VInteger / V2.FValue.VCurrency;
end;

procedure int_mod_int(V1, V2: TLyseeValue);
begin
  V1.FValue.VInteger := V1.FValue.VInteger mod V2.FValue.VInteger;
end;

procedure int_xor_int(V1, V2: TLyseeValue);
begin
  V1.FValue.VInteger := V1.FValue.VInteger xor V2.FValue.VInteger;
end;

procedure int_shl_int(V1, V2: TLyseeValue);
begin
  V1.FValue.VInteger := V1.FValue.VInteger shl V2.FValue.VInteger;
end;

procedure int_shr_int(V1, V2: TLyseeValue);
begin
  V1.FValue.VInteger := V1.FValue.VInteger shr V2.FValue.VInteger;
end;

procedure int_in_int(V1, V2: TLyseeValue);
begin
  V1.SetAsBoolean((V1.FValue.VInteger >= 0) and (V1.FValue.VInteger < V2.FValue.VInteger));
end;

procedure float_neg_float(V1, V2: TLyseeValue);
begin
  V1.FValue.VFloat := - V1.FValue.VFloat;
end;

procedure float_add_float(V1, V2: TLyseeValue);
begin
  V1.FValue.VFloat := V1.FValue.VFloat + V2.FValue.VFloat;
end;

procedure float_add_int(V1, V2: TLyseeValue);
begin
  V1.FValue.VFloat := V1.FValue.VFloat + V2.FValue.VInteger;
end;

procedure float_add_money(V1, V2: TLyseeValue);
begin
  V1.FType := my_curr;
  V1.FValue.VCurrency := V1.FValue.VFloat + V2.FValue.VCurrency;
end;

procedure float_dec_float(V1, V2: TLyseeValue);
begin
  V1.FValue.VFloat := V1.FValue.VFloat - V2.FValue.VFloat;
end;

procedure float_dec_int(V1, V2: TLyseeValue);
begin
  V1.FValue.VFloat := V1.FValue.VFloat - V2.FValue.VInteger;
end;

procedure float_dec_money(V1, V2: TLyseeValue);
begin
  V1.FType := my_curr;
  V1.FValue.VCurrency := V1.FValue.VFloat - V2.FValue.VCurrency;
end;

procedure float_mul_float(V1, V2: TLyseeValue);
begin
  V1.FValue.VFloat := V1.FValue.VFloat * V2.FValue.VFloat;
end;

procedure float_mul_int(V1, V2: TLyseeValue);
begin
  V1.FValue.VFloat := V1.FValue.VFloat * V2.FValue.VInteger;
end;

procedure float_mul_money(V1, V2: TLyseeValue);
begin
  V1.FType := my_curr;
  V1.FValue.VCurrency := V1.FValue.VFloat * V2.FValue.VCurrency;
end;

procedure float_divf_float(V1, V2: TLyseeValue);
begin
  V1.FValue.VFloat := V1.FValue.VFloat / V2.FValue.VFloat;
end;

procedure float_divf_int(V1, V2: TLyseeValue);
begin
  V1.FValue.VFloat := V1.FValue.VFloat / V2.FValue.VInteger;
end;

procedure float_divf_money(V1, V2: TLyseeValue);
begin
  V1.FValue.VFloat := V1.FValue.VFloat / V2.FValue.VCurrency;
end;

procedure money_neg_money(V1, V2: TLyseeValue);
begin
  V1.FValue.VCurrency := - V1.FValue.VCurrency;
end;

procedure money_add_money(V1, V2: TLyseeValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency + V2.FValue.VCurrency;
end;

procedure money_add_int(V1, V2: TLyseeValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency + V2.FValue.VInteger;
end;

procedure money_add_float(V1, V2: TLyseeValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency + V2.FValue.VFloat;
end;

procedure money_dec_money(V1, V2: TLyseeValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency - V2.FValue.VCurrency;
end;

procedure money_dec_int(V1, V2: TLyseeValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency - V2.FValue.VInteger;
end;

procedure money_dec_float(V1, V2: TLyseeValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency - V2.FValue.VFloat;
end;

procedure money_mul_money(V1, V2: TLyseeValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency * V2.FValue.VCurrency;
end;

procedure money_mul_int(V1, V2: TLyseeValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency * V2.FValue.VInteger;
end;

procedure money_mul_float(V1, V2: TLyseeValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency * V2.FValue.VFloat;
end;

procedure money_divf_money(V1, V2: TLyseeValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency / V2.FValue.VCurrency;
end;

procedure money_divf_int(V1, V2: TLyseeValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency / V2.FValue.VInteger;
end;

procedure money_divf_float(V1, V2: TLyseeValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency / V2.FValue.VFloat;
end;

procedure time_add_int(V1, V2: TLyseeValue);
begin
  V1.FValue.VTime := V1.FValue.VTime + V2.FValue.VInteger;
end;

procedure time_add_float(V1, V2: TLyseeValue);
begin
  V1.FValue.VTime := V1.FValue.VTime + V2.FValue.VFloat;
end;

procedure time_dec_int(V1, V2: TLyseeValue);
begin
  V1.FValue.VTime := V1.FValue.VTime - V2.FValue.VInteger;
end;

procedure time_dec_float(V1, V2: TLyseeValue);
begin
  V1.FValue.VTime := V1.FValue.VTime - V2.FValue.VFloat;
end;

procedure bool_not_bool(V1, V2: TLyseeValue);
begin
  V1.FValue.VBoolean := not V1.FValue.VBoolean;
end;

procedure bool_xor_bool(V1, V2: TLyseeValue);
begin
  V1.FValue.VBoolean := V1.FValue.VBoolean xor V2.FValue.VBoolean;
end;

procedure array_add_array(V1, V2: TLyseeValue);
var
  A, L: TLyseeList;
  I: integer;
  T: TLyseeType;
begin
  A := TLyseeList.Create;
  try
    T := V1.FType;
    L := TLyseeList(V1.GetOA);
    for I := 0 to L.Count - 1 do A.Add(L[I]);
    L := TLyseeList(V2.GetOA);
    for I := 0 to L.Count - 1 do A.Add(L[I]);
  finally
    V1.SetTOA(T, A);
  end;
end;

procedure array_dec_array(V1, V2: TLyseeValue);
var
  A, L, R: TLyseeList;
  I: integer;
  T: TLyseeType;
begin
  A := TLyseeList.Create;
  try
    T := V1.FType;
    L := TLyseeList(V1.GetOA);
    R := TLyseeList(V2.GetOA);
    for I := 0 to L.Count - 1 do
      if ((R = nil) or (R.IndexOf(L[I]) < 0)) and (A.IndexOf(L[I]) < 0) then
        A.Add(L[I]);
  finally
    V1.SetTOA(T, A);
  end;
end;

procedure array_mul_array(V1, V2: TLyseeValue);
var
  A, L, R: TLyseeList;
  I: integer;
  T: TLyseeType;
begin
  A := TLyseeList.Create;
  try
    T := V1.FType;
    L := TLyseeList(V1.GetOA);
    R := TLyseeList(V2.GetOA);
    for I := 0 to L.Count - 1 do
      if (R <> nil) and (R.IndexOf(L[I]) >= 0) and (A.IndexOf(L[I]) < 0) then
        A.Add(L[I]);
  finally
    V1.SetTOA(T, A);
  end;
end;

procedure array_shi_variant(V1, V2: TLyseeValue);
var
  L: TLyseeList;
begin
  if V1.GetSelf(L) then
  begin
    L.TestChange;
    L.Add(V2);
  end;
end;

procedure array_fill_variant(V1, V2: TLyseeValue);
var
  G: TLyseeGenerate;
  L: TLyseeList;
begin
  if V1.GetSelf(L) then
  begin
    L.TestChange;
    G := GetGenerate(V2);
    if G <> nil then
    try
      while G.GetNext do L.Add(G);
    finally
      G.Free;
    end;
  end;
end;

procedure enumset_add_enumset(V1, V2: TLyseeValue);
begin
  V1.AsEnumSet.Add(V2.AsEnumSet).SetValue(V1);
end;

procedure enumset_dec_enumset(V1, V2: TLyseeValue);
begin
  V1.AsEnumSet.Dec(V2.AsEnumSet).SetValue(V1);
end;

procedure enumset_mul_enumset(V1, V2: TLyseeValue);
begin
  V1.AsEnumSet.Mul(V2.AsEnumSet).SetValue(V1);
end;

procedure enumset_not_enumset(V1, V2: TLyseeValue);
begin
  V1.AsEnumSet.NotAll.SetValue(V1);
end;

// compare ---------------------------------------------------------------------

function compare_variant_variant(V1, V2: TLyseeValue): TCompare;
begin
  if (V1.FType = V2.FType) and V1.FType.IsObject and (V1.FValue.VObject = V2.FValue.VObject) then
    Result := crEqual else
    Result := crDiff;
end;

function compare_variant_nil(V1, V2: TLyseeValue): TCompare;
begin
  if V1.IsNil or (V1.IsObject and (V1.FValue.VObject = nil)) then
    Result := crEqual else
    Result := crDiff;
end;

function compare_nil_variant(V1, V2: TLyseeValue): TCompare;
begin
  if V2.IsNil or (V2.IsObject and (V2.FValue.VObject = nil)) then
    Result := crEqual else
    Result := crDiff;
end;

function compare_string_string(V1, V2: TLyseeValue): TCompare;
begin
  Result := CompareString(V1.AsString, V2.AsString);
end;

function compare_char_char(V1, V2: TLyseeValue): TCompare;
begin
  Result := CompareChar(V1.FValue.VChar[0], V2.FValue.VChar[0]);
end;

function compare_int_int(V1, V2: TLyseeValue): TCompare;
begin
  Result := CompareInt64(V1.FValue.VInteger, V2.FValue.VInteger);
end;

function compare_int_float(V1, V2: TLyseeValue): TCompare;
begin
  Result := CompareFloat(V1.FValue.VInteger, V2.FValue.VFloat);
end;

function compare_int_money(V1, V2: TLyseeValue): TCompare;
begin
  Result := CompareMoney(V1.FValue.VInteger, V2.FValue.VCurrency);
end;

function compare_int_time(V1, V2: TLyseeValue): TCompare;
begin
  Result := CompareFloat(V1.FValue.VInteger, V2.FValue.VTime);
end;

function compare_float_float(V1, V2: TLyseeValue): TCompare;
begin
  Result := CompareFloat(V1.FValue.VFloat, V2.FValue.VFloat);
end;

function compare_float_int(V1, V2: TLyseeValue): TCompare;
begin
  Result := CompareFloat(V1.FValue.VFloat, V2.FValue.VInteger);
end;

function compare_float_money(V1, V2: TLyseeValue): TCompare;
begin
  Result := CompareMoney(V1.FValue.VFloat, V2.FValue.VCurrency);
end;

function compare_float_time(V1, V2: TLyseeValue): TCompare;
begin
  Result := CompareFloat(V1.FValue.VFloat, V2.FValue.VTime);
end;

function compare_money_money(V1, V2: TLyseeValue): TCompare;
begin
  Result := CompareMoney(V1.FValue.VCurrency, V2.FValue.VCurrency);
end;

function compare_money_int(V1, V2: TLyseeValue): TCompare;
begin
  Result := CompareMoney(V1.FValue.VCurrency, V2.FValue.VInteger);
end;

function compare_money_float(V1, V2: TLyseeValue): TCompare;
begin
  Result := CompareMoney(V1.FValue.VCurrency, V2.FValue.VFloat);
end;

function compare_time_time(V1, V2: TLyseeValue): TCompare;
begin
  Result := CompareFloat(V1.FValue.VTime, V2.FValue.VTime);
end;

function compare_time_int(V1, V2: TLyseeValue): TCompare;
begin
  Result := CompareFloat(V1.FValue.VTime, V2.FValue.VInteger);
end;

function compare_time_float(V1, V2: TLyseeValue): TCompare;
begin
  Result := CompareFloat(V1.FValue.VTime, V2.FValue.VFloat);
end;

function compare_bool_bool(V1, V2: TLyseeValue): TCompare;
begin
  Result := CompareInteger(Ord(V1.FValue.VBoolean), Ord(V2.FValue.VBoolean));
end;

function compare_enum_enum(V1, V2: TLyseeValue): TCompare;
begin
  Result := CompareInteger(V1.AsEnum.FValue, V2.AsEnum.FValue);
end;

function compare_enumset_enumset(V1, V2: TLyseeValue): TCompare;
begin
  if V1.AsEnumSet.Equal(V2.AsEnumSet) then
    Result := crEqual else
    Result := crDiff;
end;

// system.function -------------------------------------------------------------

procedure pp_halt(const Param: TLyseeParam);
var
  cntx: TLysee;
begin
  cntx := Param.FLysee;
  if Param.FPrmc > 0 then
    cntx.FResult.SetValue(Param[0]);
  cntx.FHalted := true;
  cntx.FError.Clear;
  cntx.Terminate;
end;

procedure pp_exit(const Param: TLyseeParam);
begin
  Param.FLysee.FState := csExit;
  if Param.FPrmc > 0 then
    Param.FPrev.FResult.SetValue(Param[0]);
end;

procedure pp_break(const Param: TLyseeParam);
begin
  Param.FLysee.FState := csBreak;
end;

procedure pp_continue(const Param: TLyseeParam);
begin
  Param.FLysee.FState := csContinue;
end;

procedure pp_compile(const Param: TLyseeParam);
var
  F: TLyseeFunc;
  L: TStrings;
  I: integer;
  A: array of string;
  T: array of TLyseeType;
begin
  if Param.FPrmc = 1 then
  begin
    F := Param.FLysee.Compile(Param[0].AsString);
    Param.Result.AsFunc := F;
  end
  else
  begin
    L := TStringList.Create;
    try
      L.CommaText := TrimAll(Param[1].AsString);
      SetLength(A, L.Count + 1);
      SetLength(T, L.Count + 1);
      A[0] := '___';
      T[0] := my_variant;
      for I := 0 to L.Count - 1 do
      begin
        A[I + 1] := L[I];
        T[I + 1] := my_variant;
      end;
      if ParamOK(A, T, false) then
      begin
        F := Param.FLysee.Compile(Param[0].AsString);
        for I := 0 to L.Count - 1 do
          F.FParams.Add(L[I], my_variant);
        Param.Result.AsFunc := F;
      end
      else Param.Error('invalid arguments: ''%s''', [Param[1].AsString]);
    finally
      L.Free;
    end;
  end;
end;

procedure pp_eval(const Param: TLyseeParam);
var
  code: string;
  func: TLyseeFunc;
begin
  code := TrimRight(Param[0].AsString);
  if code <> '' then
  begin
    func := Param.FLysee.Compile('exit(' + code + ');');
    if func <> nil then
      Param.FToken.ExecFunc(func, Param, Param.FResult, nil);
  end
  else Param.Result.SetNil;
end;

procedure pp_find(const Param: TLyseeParam);
var
  S: string;
begin
  S := Trim(Param[0].AsString);
  if Param.FLysee.CodeFunc.FindSave(S, Param, Param.FResult) = fiNone then
    if not Param.FLysee.Resolve(S, Param.FResult) then
      Param.Result.SetNil;
end;

procedure pp_typeof(const Param: TLyseeParam);
begin
  Param.Result.AsType := Param[0].AsType;
end;

procedure pp_moduleof(const Param: TLyseeParam);
var
  M: TLyseeModule;
begin
  M := Param[0].AsModule;
  if M = nil then
    M := Param[0].FType.FModule;
  Param.Result.AsModule := M;
end;

procedure pp_fileof(const Param: TLyseeParam);
var
  M: TLyseeModule;
begin
  M := Param[0].AsModule;
  if M = nil then
    M := Param[0].FType.FModule;
  Param.Result.AsString := M.FFileName;
end;

procedure pp_collectGarbage(const Param: TLyseeParam);
begin
  Param.Result.AsInteger := my_gcman.Collect;
end;

procedure pp_inc(const Param: TLyseeParam);
var
  T: TLyseeType;
  V: TLyseeValue;
  X: int64;
begin
  V := Param.GetVarbValue(0, T);
  if V <> nil then
  begin
    if Param.FPrmc > 1 then
      X := Param[1].AsInteger else
      X := 1;
    if X <> 0 then
    begin
      T := V.FType;
      case T.FTID of
        TID_INTEGER: Inc(V.FValue.VInteger, X);
        TID_CHAR   : Inc(V.FValue.VChar[0], X);
        TID_FLOAT  : V.FValue.VFloat := V.FValue.VFloat + X;
        TID_BOOLEAN: V.FValue.VBoolean := not V.FValue.VBoolean;
       else Param.Error('failed increase value of type: %s', [T.FName]);
      end;
    end;
  end
  else Param.Error('variable not specified');
end;

procedure pp_dec(const Param: TLyseeParam);
var
  T: TLyseeType;
  V: TLyseeValue;
  X: int64;
begin
  V := Param.GetVarbValue(0, T);
  if V <> nil then
  begin
    if Param.FPrmc > 1 then
      X := Param[1].AsInteger else
      X := 1;
    if X <> 0 then
    begin
      T := V.FType;
      case T.FTID of
        TID_INTEGER: Dec(V.FValue.VInteger, X);
        TID_CHAR   : Dec(V.FValue.VChar[0], X);
        TID_FLOAT  : V.FValue.VFloat := V.FValue.VFloat - X;
        TID_BOOLEAN: V.FValue.VBoolean := not V.FValue.VBoolean;
        else Param.Error('failed decrease value of type: %s', [T.FName]);
      end;
    end;
  end
  else Param.Error('variable not specified');
end;

procedure pp_readln(const Param: TLyseeParam);
var
  S: string;
  T: TLyseeType;
  V: TLyseeValue;
begin
  Param.FLysee.Readln(S);
  V := Param.GetVarbValue(0, T);
  if V <> nil then
  begin
    V.SetAsString(S);
    T.Convert(V);
    Param.Result.SetValue(V);
  end
  else Param.Result.AsString := S;
end;

procedure pp_write(const Param: TLyseeParam);
begin
  Param.FLysee.Write(Param[0].AsString);
end;

procedure pp_writeln(const Param: TLyseeParam);
begin
  Param.FLysee.Write(Param[0].AsString);
  Param.FLysee.Write(sLineBreak);
end;

procedure pp_length(const Param: TLyseeParam);
var
  O: pointer;
  T: TLyseeType;
begin
  T := Param[0].GetTOA(O);
  Param.Result.AsInteger := T.GetLength(O);
end;

procedure pp_pass(const Param: TLyseeParam);
begin
  { do nothing }
end;

procedure pp_decompile(const Param: TLyseeParam);
var
  L: TStringList;
  T: TLyseeType;
  O: pointer;
begin
  L := TStringList.Create;
  try
    T := Param[0].GetTOA(O);
    if O <> nil then
      if T = my_func then
        TLyseeFunc(O).Decompile(0, L);
    Param.Result.AsString := L.Text;
  finally
    L.Free;
  end;
end;

procedure pp_exceptObject(const Param: TLyseeParam);
begin
  Param.Result.SetTOA(my_exception, Param.FLysee.FError);
end;

// kernel ----------------------------------------------------------------------

function AcquireLock: boolean;
begin
  Result := (my_spinlock <> nil);
  if Result then my_spinlock.Acquire;
end;

function ReleaseLock: boolean;
begin
  Result := (my_spinlock <> nil);
  if Result then my_spinlock.Release;
end;

function AddModule(const Name: string): TLyseeModule;
begin
  if IsID(Name) then
  begin
    Result := my_modules.Find(Name);
    if Result = nil then
      Result := TLyseeModule.Create(Name);
  end
  else Result := nil;
end;

function Command: integer;
var
  E: TLysee;
  spath, code, line, temp: string;
  pause: boolean;
  index: integer;

  procedure show_lysee;
  begin
    Writeln(Format('Interactive LYSEE script interpreter %s (%s)', [
      LSE_VERSION, {$IFDEF FPC}'Free Pascal'{$ELSE}'Delphi'{$ENDIF}]));
    Writeln('');
  end;

  procedure show_usage;
  begin
    show_lysee;
    Writeln('Usage: lysee [OPTION]... [FILE [ARGS]...]');
    Writeln('');
    Writeln('Option:');
    Writeln('  -v, --version           display the version of lysee and exit.');
    Writeln('  -h, --help              print this usage and exit.');
    Writeln('  -s, --search=PATH       set module search path.');
    Writeln('  -p, --pause             pause after executing a file.');
    Writeln('');
    Writeln('Args:');
    Writeln('  *                       arguments for file execution.');
  end;

  procedure show_version;
  begin
    show_lysee;
    Writeln('This program is distributed in the hope that it will be useful,');
    Writeln('but WITHOUT ANY WARRANTY; without even the implied warranty of');
    Writeln('MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.');
    Writeln('');
    Writeln('Enjoy it! I am libudi [li yunjie, zhengzhou, henan, china].');
  end;

  procedure do_restart;
  begin
    Writeln('*************** RESTARTED ***************');
    E.Clear;
    E.MainFile := '';
    code := '';
  end;

  procedure do_cancel;
  begin
    Writeln('*************** CANCELED ****************');
    code := '';
  end;

  procedure do_pause;
  begin
    Write('Press ENTER to continue: ');
    Readln;
  end;

  procedure do_execute(const S: string);
  begin
    try
      if S <> '' then
        if E.Execute(S + ';') then
        begin
          code := FormatValue(E.FResult);
          if code <> '' then
            Writeln(code);
          E.FResult.SetNil;
        end
        else Writeln(E.Error.ErrorText);
    finally
      code := '';
    end;
  end;

begin
  Result := 0; // OK
  pause := false;
  spath := '';
  index := 1;
  while index <= ParamCount do
  begin
    line := Trim(ParamStr(index));

    if (line = '-h') or (line = '--help') then
    begin
      show_usage;
      Exit;
    end;

    if (line = '-v') or (line = '--version') then
    begin
      show_version;
      Exit;
    end;

    if (line = '-s') or (line = '--search') then
    begin
      if index = ParamCount then
      begin
        Writeln('Error: search path is not specified.');
        Result := 1;
        Exit;
      end;
      Inc(index);
      spath := spath + ';' + Trim(ParamStr(index));
      Inc(index);
    end
    else
    if (line = '-p') or (line = '--pause') then
    begin
      pause := true;
      Inc(index);
    end
    else Break;
  end;

  E := TLysee.Create(nil);
  try
    if index <= ParamCount then
    begin
      try
        E.ExecuteFrom(index);
      finally
        if E.Excepted then
          Writeln(E.Error.ErrorText);
        if pause then do_pause;
      end;
    end
    else
    begin
      show_lysee;
      Writeln('           /C=CANCEL /Q=QUIT /R=RESTART');
      Writeln('');
      E.MainFile := ChangeFileExt(ParamStr(0), '.input');
      code := '';
      repeat
        if code = '' then Write('>>> ') else Write('  > ');
        Readln(line);
        temp := Trim(line);
        if SameText('/q', temp) then Break else
        if SameText('/r', temp) then do_restart else
        if SameText('/c', temp) then do_cancel else
        if temp = '' then
          do_execute(code) else
        if code <> '' then
          code := code + sLineBreak + line else
        if temp[Length(temp)] = ';' then
          do_execute(line) else
          code := line;
      until E.FHalted;
    end;
    if E.Excepted then Result := 1;
  finally
    E.Free;
  end;
end;

function MatchID(const ID1, ID2: string): boolean;
begin
  Result := SameText(ID1, ID2);
end;

function MatchID(const ID: string; const IDList: array of string): boolean;
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

function OperatorStr(OP: TLyseeOperator): string;
begin
  case OP of
    opAdd : Result := Symbols[syAdd].ID;
    opDec : Result := Symbols[syReduce].ID;
    opMul : Result := Symbols[syMul].ID;
    opDiv : Result := Symbols[syDiv].ID;
    opDivf: Result := Symbols[syDivf].ID;
    opMod : Result := Symbols[syMod].ID;
    opXor : Result := Symbols[syXor].ID;
    opShl : Result := Symbols[syShl].ID;
    opShr : Result := Symbols[syShr].ID;
    opShi : Result := Symbols[syShi].ID;
    opFill: Result := Symbols[syFill].ID;
    opIn  : Result := Symbols[syIn].ID;
    opLike: Result := Symbols[syLike].ID;
    opIs  : Result := Symbols[syIs].ID;
    opAs  : Result := Symbols[syAs].ID;
    opNot : Result := Symbols[syNot].ID;
    opNeg : Result := Symbols[syReduce].ID;
    else Result := '';
  end;
end;

function Keywords: string;
var
  I: TLyseeSymbol;
  L: TStringList;
begin
  if my_keywords = '' then
  begin
    L := TStringList.Create;
    try
      for I := syBegin to syEnd do
        L.Add(Symbols[I].ID);
      L.Sort;
      my_keywords := L.CommaText;
    finally
      L.Free;
    end;
  end;
  Result := my_keywords;
end;

function Hilights: string;
var
  L: TStringList;
begin
  if my_hilights = '' then
  begin
    L := TStringList.Create;
    try
      L.CommaText := Keywords;
      L.Add(LSE_SYSTE);
      L.Add(LSE_MAIN);
      L.Add(TID_NAMES[TID_STRING]);
      L.Add(TID_NAMES[TID_BOOLEAN]);
      L.Add(TID_NAMES[TID_VARIANT]);
      L.Add(TID_NAMES[TID_TYPE]);
      L.Sort;
      my_hilights := L.CommaText;
    finally
      L.Free;
    end;
  end;
  Result := my_hilights;
end;

function CompleteFileName(const FileName: string): string;
begin
  Result := FullFileName(FileName);
  if Result <> '' then
    if not FileExists(Result) then
    begin
      if ExtractFileExt(Result) = '' then
      begin
        Result := Result + '.ls';
        if FileExists(Result) then Exit;
      end;
      Result := '';
    end;
end;

function SortValueCompare(V1, V2: pointer): integer;
var
  R: TCompare;
begin
  R := TLyseeValue(V1).Compare(TLyseeValue(V2));
  if R = crLess then Result := -1 else
  if R = crMore then Result :=  1 else Result := 0;
end;

function ExtractNameModule(const ID: string; var Module: string): string;
var
  X: integer;
begin
  X := Pos('.', ID);
  if X > 0 then
  begin
    Result := Copy(ID, X + 1, Length(ID) - X);
    Module := Copy(ID, 1, X - 1);
  end
  else
  begin
    Result := ID;
    Module := '';
  end;
end;

function FormatChar(C: char): string;
begin
  if C < ' ' then
    Result := '#' + IntToStr(Ord(C)) else
    Result := '''' + C + '''';
end;

function FormatString(const S: string): string;
var
  I, L: integer;
begin
  Result := '';
  I := 1;
  L := Length(S);
  if I > L then Result := '''''' else
  repeat
    while (I <= L) and (S[I] < ' ') do
    begin
      Result := Result + '#' + IntToStr(Ord(S[I]));
      Inc(I);
    end;
    if I <= L then
    begin
      Result := Result + '''';
      while (I <= L) and (S[I] >= ' ') do
      begin
        Result := Result + S[I];
        Inc(I);
      end;
      Result := Result + '''';
    end;
  until I > L;
end;

function FormatValue(V: TLyseeValue): string;
begin
  if V.FType = my_char then
    Result := FormatChar(V.FValue.VChar[0]) else
  if V.FType = my_string then
    Result := FormatString(V.AsString) else
    Result := V.AsString;
end;

procedure ErrorConvert(AType, NewType: TLyseeType);
begin
  Throw('can not convert %s to %s', [AType.FName, NewType.FName]);
end;

function GetGenerate(Value: TLyseeValue): TLyseeGenerate;
begin
  if Value <> nil then
    Result := Value.FType.Generate(Value.GetOA) else
    Result := nil;
end;

function GetGenerate(V1, V2: TLyseeValue; Upto: boolean): TLyseeGenerate;
var
  P: TCompare;
begin
  Result := nil;
  if V1.FType = V2.FType then
  begin
    P := V1.Compare(V2);
    if (P = crEqual) or (Upto and (P = crLess)) or (not Upto and (P = crMore)) then
      case V1.FType.FTID of
        TID_INTEGER: Result := TLyseeIntGenerate.CreateIn(V1.AsInteger, V2.AsInteger, Upto);
        TID_CHAR   : Result := TLyseeCharGenerate.CreateIn(V1.AsChar, V2.AsChar, Upto);
        TID_BOOLEAN: Result := TLyseeBoolGenerate.CreateIn(V1.AsBoolean, V2.AsBoolean, Upto);
        else if V1.FType.IsEnum then
          Result := TLyseeEnumGenerate.CreateIn(V1.AsEnum, V2.AsEnum, Upto);
      end;
  end;
end;

function Margin(Level: integer): string;
begin
  Result := StringOfChar(' ', Level * 2);
end;

procedure FreeOperates(var Head: PLyseeOperate);
begin
  if Head <> nil then
  begin
    FreeOperates(Head^.o_next);
    MemFree(Head, sizeof(RLyseeOperate));
    Head := nil;
  end;
end;

procedure FreeCompares(var Head: PLyseeCompare);
begin
  if Head <> nil then
  begin
    FreeCompares(Head^.c_next);
    MemFree(Head, sizeof(RLyseeCompare));
    Head := nil;
  end;
end;

function ParamOK(const Names: array of string;
                 const Types: array of TLyseeType;
                 IsMethod: boolean): boolean;
var
  I, L, X: integer;
begin
  Result := false;
  L := Length(Names);
  if L = Length(Types) then
  begin
    for I := 0 to L - 1 do
    begin
      if Types[I] = nil then Exit;
      if IsMethod and MatchID('Self', Names[I]) then Exit;
      if IsID(Names[I]) then
      begin
        for X := I + 1 to L - 1 do
          if MatchID(Names[I], Names[X]) then Exit;
      end
      else Exit;
    end;
    Result := true;
  end;
end;

function AddFunc(const Name: string; T: TLyseeType;
  const ParamNames: array of string; const ParamTypes: array of TLyseeType;
  Proc: TLyseeProc; Data: pointer): TLyseeFunc;
begin
  Result := my_system.AddFunc(Name, T, ParamNames, ParamTypes, Proc, Data);
end;

function AddFunc(const Name: string;
  const ParamNames: array of string; const ParamTypes: array of TLyseeType;
  Proc: TLyseeProc; Data: pointer): TLyseeFunc;
begin
  Result := AddFunc(Name, my_nil, ParamNames, ParamTypes, Proc, Data);
end;

function AddFunc(const Name: string; T: TLyseeType;
  Proc: TLyseeProc; Data: pointer): TLyseeFunc;
begin
  Result := AddFunc(Name, T, [], [], Proc, Data);
end;

function AddFunc(const Name: string;
  Proc: TLyseeProc; Data: pointer): TLyseeFunc;
begin
  Result := AddFunc(Name, my_nil, [], [], Proc, Data);
end;

function AddMethod(const AClass: TLyseeType;
                   const AName: string;
                   const AType: TLyseeType;
                   const ParamNames: array of string;
                   const ParamTypes: array of TLyseeType;
                   const AProc: TLyseeProc): TLyseeFunc;
var
  I: integer;
begin
  Result := nil;
  if (AClass <> nil) and AClass.IsObject then
    if (AType <> nil) and (AName <> '') and Assigned(AProc) then
      if AClass.FindMethod(AName) = nil then
        if ParamOK(ParamNames, ParamTypes, true) then
          if not MatchID(AName, 'Create') or (AType = AClass) then
          begin
            Result := TLyseeFunc.CreateMethod(AName, AClass, AProc);
            Result.FResultType := AType;
            if MatchID(AName, 'Create') then
              AClass.FConstructer := Result else
              Result.FParams.Add('Self', AClass);
            for I := 0 to Length(ParamNames) - 1 do
              Result.FParams.Add(ParamNames[I], ParamTypes[I]);
          end;
end;

function AddMethod(const AClass: TLyseeType;
                   const AName: string;
                   const ParamNames: array of string;
                   const ParamTypes: array of TLyseeType;
                   const AProc: TLyseeProc): TLyseeFunc;
begin
  Result := AddMethod(AClass, AName, my_nil, ParamNames, ParamTypes, AProc);
end;

function AddMethod(const AClass: TLyseeType;
                   const AName: string;
                   const AType: TLyseeType;
                   const AProc: TLyseeProc): TLyseeFunc;
begin
  Result := AddMethod(AClass, AName, AType, [], [], AProc);
end;

function AddMethod(const AClass: TLyseeType;
                   const AName: string;
                   const AProc: TLyseeProc): TLyseeFunc;
begin
  Result := AddMethod(AClass, AName, my_nil, [], [], AProc);
end;

function SetupProp(const AClass: TLyseeType;
                   const AName: string;
                   const AType: TLyseeType;
                   const IndexNames: array of string;
                   const IndexTypes: array of TLyseeType;
                   const GetProc, SetProc: TLyseeProc;
                     var GetFunc, SetFunc: TLyseeFunc): boolean;

  procedure setup_get(const FuncName: string);
  var
    I: integer;
  begin
    if AClass.FindMethod(FuncName) = nil then
    begin
      GetFunc := TLyseeFunc.CreateMethod(FuncName, AClass, GetProc);
      GetFunc.FResultType := AType;
      GetFunc.FParams.Add('Self', AClass);
      for I := 0 to Length(IndexNames) - 1 do
        GetFunc.Params.Add(IndexNames[I], IndexTypes[I]);
    end;
  end;

  procedure setup_set(const FuncName: string);
  var
    I: integer;
  begin
    if GetFunc <> nil then
      if Assigned(SetProc) and (AClass.FindMethod(FuncName) = nil) then
      begin
        SetFunc := TLyseeFunc.CreateMethod(FuncName, AClass, SetProc);
        SetFunc.FResultType := my_nil;
        SetFunc.FParams.Add('Self', AClass);
        for I := 0 to Length(IndexNames) - 1 do
          SetFunc.FParams.Add(IndexNames[I], IndexTypes[I]);
        SetFunc.FParams.Add('NewValue', AType);
      end;
  end;

var
  L: integer;
begin
  GetFunc := nil;
  SetFunc := nil;
  L := Length(IndexNames);
  if (AClass <> nil) and AClass.IsObject then
    if (AType <> nil) and (AType <> my_nil) and Assigned(GetProc) then
      if IsID(AName) or ((AName = '') and (L > 0)) then
        if ParamOK(IndexNames, IndexTypes, true) then
          if L = 0 then      // (expression).prop
          begin
            setup_get(AName);
            setup_set('set_' + AName);
          end
          else
          if AName = '' then // (expression)[...]
          begin
            setup_get('get[]');
            setup_set('set[]');
          end
          else               // (expression).prop[...]
          begin
            setup_get('get_' + AName + '[]');
            setup_set('set_' + AName + '[]');
          end;
  Result := (GetFunc <> nil);
end;

function SetupProp(const AClass: TLyseeType;
                   const AName: string;
                   const AType: TLyseeType;
                   const IndexNames: array of string;
                   const IndexTypes: array of TLyseeType;
                   const GetProc: TLyseeProc;
                     var GetFunc: TLyseeFunc): boolean;
var
  S: TLyseeFunc;
begin
  Result := SetupProp(AClass, AName, AType, IndexNames, IndexTypes,
                      GetProc, nil, GetFunc, S);
end;

function SetupProp(const AClass: TLyseeType;
                   const AName: string;
                   const AType: TLyseeType;
                   const IndexNames: array of string;
                   const IndexTypes: array of TLyseeType;
                   const GetProc, SetProc: TLyseeProc): boolean;
var
  G, S: TLyseeFunc;
begin
  Result := SetupProp(AClass, AName, AType, IndexNames, IndexTypes,
                      GetProc, SetProc, G, S);
end;

function SetupProp(const AClass: TLyseeType;
                   const AName: string;
                   const AType: TLyseeType;
                   const GetProc, SetProc: TLyseeProc): boolean;
var
  G, S: TLyseeFunc;
begin
  Result := SetupProp(AClass, AName, AType, [], [],
                      GetProc, SetProc, G, S);
end;

{ TLysee }

procedure TLysee.BeginExecute;
begin
  if Assigned(FOnExecuting) then FOnExecuting(Self);
end;

procedure TLysee.EndExecute;
begin
  if Assigned(FOnExecuted) then FOnExecuted(Self);
end;

function TLysee.ExecuteFile(const FileName: string): boolean;
var
  F: string;
begin
  CheckNotRunning;
  F := CompleteFileName(FileName);
  if F <> '' then
  begin
    SetMainFile(F);
    Result := Execute(FileCode(FileName));
  end
  else Result := false;
end;

function TLysee.ExecuteFrom(StartParamIndex: integer): boolean;
var
  I: integer;
  F: string;
begin
  CheckNotRunning;
  I := Max(1, StartParamIndex);
  F := CompleteFileName(ParamStr(I));
  if F = '' then
    Throw('[ERROR]: File ''%s'' not exists!', [ParamStr(I)]);
  FArgs.Clear;
  FArgs.Add.SetAsString(F);
  for I := I + 1 to ParamCount do
    FArgs.Add.SetAsString(ParamStr(I));
  Result := ExecuteFile(F);
  if not Result then Throw(FError.ErrorText);
end;

procedure TLysee.Readln(var Text: string);
begin
  if Assigned(FOnReadln) then
    FOnReadln(Self, Text) else
    System.Readln(Text);
end;

procedure TLysee.RollbackRemove(AObj: TObject);
begin
  if FRollbacks <> nil then
    FRollbacks.Remove(AObj);
end;

procedure TLysee.SetMainFile(const Value: string);
begin
  FMainFile := ExpandFileName(Trim(Value));
end;

function TLysee.Terminate: boolean;
begin
  Result := FTerminated;
  FTerminated := true;
end;

procedure TLysee.Write(const Text: string);
begin
  if Assigned(FOnWrite) then
    FOnWrite(Self, Text) else
    System.Write(Text);
end;

procedure TLysee.Write(const Text: string; const Args: array of const);
begin
  Write(Format(Text, Args));
end;

procedure TLysee.Writeln;
begin
  Write(sLineBreak);
end;

procedure TLysee.Writeln(const Text: string);
begin
  Write(Text);
  Write(sLineBreak);
end;

procedure TLysee.Writeln(const Text: string; const Args: array of const);
begin
  Writeln(Format(Text, Args));
end;

function TLysee.GetTerminated: boolean;
begin
  Result := FTerminated or not GetRunning;
end;

function TLysee.GetTerminating: boolean;
begin
  Result := FTerminated and GetRunning;
end;

procedure TLysee.MarkSurvived;
var
  I: integer;
  M: TLyseeModule;
  P: TLyseeParam;
begin
  FArgs.MarkForSurvive;
  FResult.MarkForSurvive;
  FMainLocals.MarkForSurvive;

  P := FCurrent;
  while P <> nil do
  begin
    P.FResult.MarkForSurvive;
    P.FParams.MarkForSurvive;
    P.FVarArgs.MarkForSurvive;
    P := P.FPrev;
  end;

  for I := 0 to FModules.Count - 1 do
  begin
    M := FModules[I];
    if M.FContext = Self then
      M.FConsts.MarkForSurvive;
  end;
end;

procedure TLysee.Clear(WillDestroy: boolean);
begin
  CheckNotRunning;
  FNameSeed := 0;
  SetResult(nil);
  FArgs.Clear;
  FMainLocals.Clear;
  FMainModule := nil;
  FModules.Clear;
  if not WillDestroy then GetMainModule;
end;

constructor TLysee.Create(AOwner: TComponent);
begin
  inherited;
  my_system.Setup;
  FResult := TLyseeValue.Create;
  FError := TLyseeError.Create(Self);
  FArgs := TLyseeList.Create;
  FArgs.IncRefcount;
  FMainLocals := TLyseeList.Create;
  FMainLocals.IncRefcount;
  FModules := TLyseeModuleList.Create(Self);
  GetMainModule;
  FState := csTerminated;
  FTerminated := true;
  FHalted := false;
end;

destructor TLysee.Destroy;
begin
  Clear(true);
  FreeAndNil(FResult);
  FreeAndNil(FMainLocals);
  FreeAndNil(FMainModule);
  FreeAndNil(FModules);
  FreeAndNil(FArgs);
  FreeAndNil(FError);
  inherited;
end;

function TLysee.Compile(const Code: string): TLyseeFunc;
begin
  Result := nil;
  Check(FRollbacks = nil, 'invalid embeded compiling');
  FRollbacks := TList.Create;
  try
    if not GetRunning then
    begin
      FError.Clear;
      Result := TLyseeParser.Create(GetMainModule).ParseAndFree(Code);
      FreeAndNil(FRollbacks);
      FState := csReady;
    end
    else
    if StatusOK then
    begin
      Result := TLyseeParser.Create(GetCodeModule).ParseAndFree(Code);
      if FExcepted then
      begin
        Rollback;
        Result := nil;
      end
      else FreeAndNil(FRollbacks);
    end
    else Throw('context status is incorrect');
  except
    Rollback;
    raise;
  end;
end;

function TLysee.GetMainFunc: TLyseeFunc;
begin
  if FMainFunc = nil then
    FMainFunc := TLyseeFunc.Create(LSE_MAIN, FMainModule, nil);
  Result := FMainFunc;
end;

function TLysee.GetMainModule: TLyseeModule;
begin
  if FMainModule = nil then
  begin
    FMainModule := TLyseeModule.CreateEx(LSE_MAIN, Self);
    FMainModule.FFileName := FMainFile;
  end;
  Result := FMainModule;
end;

function TLysee.GetMainToken: TLyseeToken;
begin
  if FMainToken = nil then
  begin
    FMainToken := TLyseeToken.Create;
    FMainToken.FSym := syCall;
  end;
  Result := FMainToken;
end;

function TLysee.GetModuleFile(const AName: string): string;
var
  L, P: string;
begin
  if Assigned(FOnGetModuleFile) then
  begin
    Result := FOnGetModuleFile(Self, AName);
    if FileExists(Result) then Exit;
  end;
  L := Trim(my_search_path);
  while L <> '' do
  begin
    P := FullPath(Trim(ExtractNext(L, ';')));
    if P <> '' then
    begin
      Result := P + AName + LSE_FILEEXT;
      if FileExists(Result) then Exit;
    end;
  end;
  Result := '';
end;

function TLysee.GetReady: boolean;
begin
  Result := (FState = csReady);
end;

function TLysee.GetRunning: boolean;
begin
  Result := (FState >= csRunning);
end;

procedure TLysee.ExecMain;
begin
  CheckNotRunning;
  Check(GetReady, 'context is not ready');
  FResult.SetNil;
  FError.Clear;
  FCurrent := nil;
  FHalted := false;
  FExcepted := false;
  FTerminated := false;
  BeginExecute;
  try
    if not FTerminated then
    begin
      FMainLocals.PrepareFor(FMainFunc);
      FCurrent := TLyseeParam.Create;
      try
        FCurrent.FLysee := Self;
        FCurrent.FFunc := FMainFunc;
        FCurrent.FParams := FMainLocals;
        FCurrent.FResult := FResult;
        FCurrent.FToken := GetMainToken;
        FState := csRunning;
        FMainFunc.FSTMTs.Execute(FCurrent);
      finally
        FreeAndNil(FCurrent);
      end;
    end;
  finally
    FCurrent := nil;
    FTerminated := true;
    FState := csTerminated;
    FMainFunc.FSTMTs.Clear;
    if FExcepted then SetResult(nil) else FError.Clear;
    EndExecute;
  end;
end;

procedure TLysee.Rollback;
var
  I: integer;
begin
  if FRollbacks <> nil then
  try
    for I := FRollbacks.Count - 1 downto 0 do
      if I < FRollbacks.Count then
        TBasicObject(FRollbacks[I]).Free;
  finally
    FreeAndNil(FRollbacks);
  end;
end;

procedure TLysee.SetResult(Value: TLyseeValue);
begin
  if Value <> nil then
    FResult.SetValue(Value) else
    FResult.SetNil
end;

procedure TLysee.RollbackAdd(AObj: TObject);
begin
  if (FRollbacks <> nil) and (FRollbacks.IndexOf(AObj) < 0) then
    FRollbacks.Add(AObj);
end;

procedure TLysee.CheckNotRunning;
begin
  Check(not GetRunning, 'current context is running');
end;

function TLysee.StatusOK: boolean;
begin
  Result := (FState = csRunning) and not (FTerminated or FExcepted);
end;

function TLysee.Execute(const Code: string): boolean;
var
  F: TLyseeFunc;
begin
  try
    Result := false;
    F := Compile(Code);
    if F <> nil then
    begin
      if GetRunning then
        FCurrent.FToken.ExecFunc(F, FCurrent, nil, nil) else
        ExecMain;
      Result := not FExcepted;
    end;
  except
    Result := false;
  end;
end;

function TLysee.Resolve(const ID: string; Value: TLyseeValue): boolean;

  function resolve_reserved: boolean;
  begin
    Result := (Length(ID) > 2) and (ID[1] = '_') and (ID[2] = '_');
    if Result then
      if MatchID(ID, '__module')   then Value.SetAsModule(CodeModule) else
      if MatchID(ID, '__file')     then Value.SetAsString(CodeModule.FFileName) else
      if MatchID(ID, '__func')     then Value.SetAsFunc(CodeFunc) else
      if MatchID(ID, '__prmc')     then Value.SetAsInteger(FCurrent.FPrmc) else
      if MatchID(ID, '__args')     then Value.SetAsArray(FArgs) else
      if MatchID(ID, '__argc')     then Value.SetAsInteger(FArgs.Count) else
      if MatchID(ID, '__dir')      then Value.SetAsString(GetCurrentDir) else
      if MatchID(ID, '__modules')  then Value.SetAsArray(FModules.ToList) else
      if MatchID(ID, '__libs')     then Value.SetAsArray(my_modules.ToList) else
      if MatchID(ID, '__hilights') then Value.SetAsString(Hilights) else
      if MatchID(ID, '__main')     then Value.SetAsFunc(FMainFunc) else
      if MatchID(ID, '__in_main')  then Value.SetAsBoolean(CodeFunc.IsMainFunc) else
      if MatchID(ID, '__ename')    then Value.SetAsString(FError.ErrID) else
      if MatchID(ID, '__emsg')     then Value.SetAsString(FError.EMsg) else
      if MatchID(ID, '__erow')     then Value.SetAsInteger(FError.ERow) else
      if MatchID(ID, '__ecol')     then Value.SetAsInteger(FError.ECol) else
      if MatchID(ID, '__efile')    then Value.SetAsString(FError.EFileName) else
      if MatchID(ID, '__emodule')  then Value.SetAsString(FError.EModule) else
      if MatchID(ID, '__error')    then Value.SetAsString(FError.ErrorText) else
      if MatchID(ID, '__kernel')   then Value.SetAsString(my_kernel) else
      if MatchID(ID, '__knpath')   then Value.SetAsString(my_knpath) else
      if MatchID(ID, '__kndir')    then Value.SetAsString(my_kndir) else
      if MatchID(ID, '__home')     then Value.SetAsString(my_home) else
      if MatchID(ID, '__program')  then Value.SetAsString(my_program) else
      if MatchID(ID, '__tmpath')   then Value.SetAsString(my_tmpath) else
      if MatchID(ID, '__keywords') then Value.SetAsString(Keywords) else
      if MatchID(ID, '__search')   then Value.SetAsString(my_search_path) else
        Result := false;
  end;

begin
  Result := (ID <> '') and (resolve_reserved or
    (Assigned(FOnResolve) and FOnResolve(ID, Value)));
end;

procedure TLysee.ForWhileRepeatEnded;
begin
  if FState in [csBreak, csContinue] then FState := csRunning;
end;

function TLysee.GetCodeFunc: TLyseeFunc;
var
  P: TLyseeParam;
begin
  P := FCurrent;
  while P <> nil do
  begin
    Result := P.FFunc;
    if Result.FSTMTs <> nil then Exit;
    P := P.FPrev;
  end;
  Result := nil;
end;

function TLysee.GetCodeModule: TLyseeModule;
var
  F: TLyseeFunc;
begin
  F := CodeFunc;
  if F <> nil then
    Result := F.FModule else
    Result := nil;
end;

{ TLyseeError }

procedure TLyseeError.Clear;
begin
  FErrID := '';
  FMsg := '';
  FModule := '';
  FFileName := '';
  FRow := 0;
  FCol := 0;
end;

constructor TLyseeError.Create(ALysee: TLysee);
begin
  FLysee := ALysee;
end;

function TLyseeError.GetText: string;
begin
  if (FErrID <> '') and (FMsg <> '') then
  begin
    Result := EFileName;
    if Result <> '' then
      Result := ' file=' + Result;
    Result := Format('[%s]: (module=%s%s row=%d col=%d) %s',
      [FErrID, FModule, Result, FRow + 1, FCol + 1, FMsg]);
  end
  else Result := '';
end;

procedure TLyseeError.Runtime(const Msg, Module, FileName: string; Row, Col: integer);
begin
  FErrID := 'RuntimeError';
  FMsg := Msg;
  FModule := Module;
  FFileName := FileName;
  FRow := Row;
  FCol := Col;
  FLysee.FExcepted := true;
end;

procedure TLyseeError.Syntax(const Msg, Module, FileName: string; Row, Col: integer);
begin
  FErrID := 'SyntaxError';
  FMsg := Msg;
  FModule := Module;
  FFileName := FileName;
  FRow := Row;
  FCol := Col;
  FLysee.FExcepted := true;
  Abort;
end;

{ TLyseeType }

function TLyseeType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then Result := 1 else Result := 0;
end;

function TLyseeType.AddCompare(AType: TLyseeType; AFunc: TLyseeCompareFunc): PLyseeCompare;
begin
  Result := MemAllocZero(sizeof(RLyseeCompare));
  Result^.c_type := AType;
  Result^.c_compare := AFunc;
  Result^.c_next := FCompare;
  FCompare := Result;
end;

function TLyseeType.RegisterProc(AProc: TLyseeObjectProc): integer;
begin
  Result := Length(FProcs);
  SetLength(FProcs, Result + 1);
  FProcs[Result] := AProc;
end;

function TLyseeType.AddOperate(OP: TLyseeOperator; AType: TLyseeType; AProc: TLyseeOperateProc): PLyseeOperate;
begin
  Result := MemAllocZero(sizeof(RLyseeOperate));
  Result^.o_oper := OP;
  Result^.o_type := AType;
  Result^.o_operate := AProc;
  Result^.o_next := FOperate[OP];
  FOperate[OP] := Result;
end;

constructor TLyseeType.Create(const AName: string; AModule: TLyseeModule; AParent: TLyseeType);
begin
  if AParent <> nil then
    if ClassType.ClassParent = AParent.ClassType then
      FParent := AParent else
      Throw('%s is not parent of %s', [AParent.FName, AName]);

  FName := AName;
  FStyle := tsObject;
  FTID := my_TID_seed;
  Inc(my_TID_seed);
  FModule := AModule;
  if FModule <> nil then
  begin
    FModule.FTypeList.Add(Self);
    FMethods := TLyseeFuncList.Create;
  end;
end;

function TLyseeType.Define(const AProp: string; const AType: TLyseeType;
  const IndexName: string; const IndexType: TLyseeType;
  const GetProc, SetProc: TLyseeObjectProc): boolean;
begin
  Result := Define(AProp, AType, [IndexName], [IndexType], GetProc, SetProc);
end;

destructor TLyseeType.Destroy;
var
  I: TLyseeOperator;
begin
  for I := Low(TLyseeOperator) to High(TLyseeOperator) do
    FreeOperates(FOperate[I]);
  FreeCompares(FCompare);
  FreeAndNil(FMethods);
  inherited;
end;

function TLyseeType.GetFullName: string;
begin
  Result := FModule.FName + '.' + FName
end;

procedure TLyseeType.GcMark(Obj: pointer);
begin
  { do nothing }
end;

function TLyseeType.Generate(Obj: pointer): TLyseeGenerate;
begin
  Result := nil;
end;

function TLyseeType.GetLength(Obj: pointer): int64;
begin
  Result := 0;
end;

function TLyseeType.GetMethod(Index: integer): TLyseeFunc;
begin
  Result := TLyseeFunc(FMethods[Index]);
end;

function TLyseeType.GetMethodCount: integer;
begin
  if FMethods <> nil then
    Result := FMethods.Count else
    Result := 0;
end;

function TLyseeType.IsBasicValue: boolean;
begin
  Result := (FStyle in [tsBasic, tsNil]);
end;

function TLyseeType.IsEnum: boolean;
begin
  Result := (FStyle = tsEnum);
end;

function TLyseeType.IsEnumSet: boolean;
begin
  Result := (FStyle = tsEnumSet);
end;

function TLyseeType.IsChildTypeOf(AType: TLyseeType): boolean;
begin
  Result := (FParent <> nil) and FParent.IsTypeOf(AType);
end;

function TLyseeType.IsNil: boolean;
begin
  Result := (Self = my_nil);
end;

function TLyseeType.IsObject: boolean;
begin
  Result := (FStyle in [tsObject, tsEnum, tsEnumSet]);
end;

function TLyseeType.IsTypeOf(AType: TLyseeType): boolean;
begin
  Result := (Self = AType) or IsChildTypeOf(AType);
end;

function TLyseeType.Method(const AName: string;
                        const AProc: TLyseeObjectProc): TLyseeFunc;
begin
  Result := Method(AName, my_nil, [], [], AProc);
end;

function TLyseeType.Method(const AName: string;
                        const ParamNames: array of string;
                        const ParamTypes: array of TLyseeType;
                        const AProc: TLyseeObjectProc): TLyseeFunc;
begin
  Result := Method(AName, my_nil, ParamNames, ParamTypes, AProc);
end;

function TLyseeType.Method(const AName: string;
                        const AType: TLyseeType;
                        const ParamNames: array of string;
                        const ParamTypes: array of TLyseeType;
                        const AProc: TLyseeObjectProc): TLyseeFunc;
begin
  Result := nil;
  if Assigned(AProc) then
  begin
    Result := AddMethod(Self, AName, AType, ParamNames, ParamTypes,
                        {$IFDEF FPC}@{$ENDIF}LyseeObjectProc);
    if Result <> nil then
      Result.FData := IntToPtr(RegisterProc(AProc));
  end;
end;

function TLyseeType.Method(const AName: string;
                        const AType: TLyseeType;
                        const AProc: TLyseeObjectProc): TLyseeFunc;
begin
  Result := Method(AName, AType, [], [], AProc);
end;

function TLyseeType.Add(Obj: pointer; Value: TLyseeValue): integer;
begin
  Throw('%s is not container, can not add item value', [FName]);
  Result := -1;
end;

function TLyseeType.AsBoolean(Obj: pointer): boolean;
begin
  Result := (Obj <> nil);
end;

function TLyseeType.AsChar(Obj: pointer): char;
begin
  ErrorConvert(Self, my_char);
  Result := #0;
end;

function TLyseeType.AsFloat(Obj: pointer): double;
begin
  ErrorConvert(Self, my_float);
  Result := 0;
end;

function TLyseeType.AsInteger(Obj: pointer): int64;
begin
  ErrorConvert(Self, my_int);
  Result := 0;
end;

function TLyseeType.AsCurrency(Obj: pointer): currency;
begin
  ErrorConvert(Self, my_curr);
  Result := 0;
end;

function TLyseeType.AsString(Obj: pointer): string;
begin
  ErrorConvert(Self, my_string);
  Result := '';
end;

function TLyseeType.AsTime(Obj: pointer): TDateTime;
begin
  ErrorConvert(Self, my_time);
  Result := 0;
end;

function TLyseeType.Clear(Obj: pointer): boolean;
begin
  Result := false;
end;

function TLyseeType.FindCompare(AType: TLyseeType): PLyseeCompare;
var
  P: PLyseeCompare;
begin
  P := nil;
  Result := FCompare;
  while Result <> nil do
  begin
    if Result^.c_type = AType then Exit;
    if Result^.c_type = my_variant then P := Result;
    Result := Result^.c_next;
  end;

  if (P = nil) and (Self <> my_variant) then
  begin
    Result := my_variant.FCompare;
    while Result <> nil do
    begin
      if Result^.c_type = AType then Exit;
      if Result^.c_type = my_variant then P := Result;
      Result := Result^.c_next;
    end;
  end;
  Result := P;
end;

function TLyseeType.FindMethod(const AName: string): TLyseeFunc;
var
  I: integer;
begin
  if Self <> nil then
    for I := 0 to FMethods.Count - 1 do
    begin
      Result := GetMethod(I);
      if MatchID(AName, Result.FName) then Exit;
    end;
  Result := nil;
end;

function TLyseeType.FindOperate(OP: TLyseeOperator; AType: TLyseeType): PLyseeOperate;
var
  P: PLyseeOperate;
begin
  P := nil;
  Result := FOperate[OP];
  while Result <> nil do
  begin
    if Result^.o_type = AType then Exit;
    if Result^.o_type = my_variant then P := Result;
    Result := Result^.o_next;
  end;

  if (P = nil) and (Self <> my_variant) then
  begin
    Result := my_variant.FOperate[OP];
    while Result <> nil do
    begin
      if Result^.o_type = AType then Exit;
      if Result^.o_type = my_variant then P := Result;
      Result := Result^.o_next;
    end;
  end;
  Result := P;
end;

function TLyseeType.Prototype(const AName: string): string;
begin
  if Self <> my_variant then
    Result := AName + ':' + FName else
    Result := AName;
end;

function TLyseeType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then Result := 1 else Result := 0;
end;

procedure TLyseeType.SetParent(const Value: TLyseeType);
begin
  if (FParent = nil) and (Value <> nil) then
    if ClassType.ClassParent = Value.ClassType then
      FParent := Value else
      Throw('%s is not parent of %s', [Value.FName, FName]);
end;

procedure TLyseeType.Setup;
begin
  { setup class methods and properties here }
end;

function TLyseeType.Define(const AProp: string; const AType: TLyseeType;
  const IndexNames: array of string; const IndexTypes: array of TLyseeType;
  const GetProc, SetProc: TLyseeObjectProc): boolean;
var
  G, S: TLyseeFunc;
  F, P: TLyseeProc;
begin
  Result := Assigned(GetProc);
  if Result then
  begin
    G := nil;
    S := nil;
    F := {$IFDEF FPC}@{$ENDIF}LyseeObjectProc;
    if Assigned(SetProc) then P := F else P := nil;
    Result := SetupProp(Self, AProp, AType, IndexNames, IndexTypes, F, P, G, S);
    if Result then
    begin
      G.FData := IntToPtr(RegisterProc(GetProc));
      if S <> nil then
        S.FData := IntToPtr(RegisterProc(SetProc));
    end;
  end;
end;

procedure TLyseeType.SetDefault(Value: TLyseeValue);
begin
  Value.SetNil;
  Value.FType := Self;
end;

procedure TLyseeType.Validate(Obj: pointer);
begin
  if Obj = nil then
    Throw('invalid %s instance: nil', [FName]);
end;

procedure TLyseeType.Convert(Value: TLyseeValue);
begin
  if Value.FType <> Self then
    if not Value.FType.ConvertTo(Value, Self) then
      Value.SetParentType(Self);
end;

function TLyseeType.ConvertTo(Value: TLyseeValue; T: TLyseeType): boolean;
begin
  Result := (T = Self) or (T = my_variant);
end;

function TLyseeType.Define(const AProp: string; const AType: TLyseeType;
  const GetProc, SetProc: TLyseeObjectProc): boolean;
begin
  Result := Define(AProp, AType, [], [], GetProc, SetProc);
end;

{ TLyseeVariantType }

procedure TLyseeVariantType.SetDefault(Value: TLyseeValue);
begin
  Value.SetNil;
end;

procedure TLyseeVariantType.Convert(Value: TLyseeValue);
begin
  { do nothing }
end;

{ TLyseeNilType }

function TLyseeNilType.AsBoolean(Obj: pointer): boolean;
begin
  Result := false;
end;

function TLyseeNilType.AsChar(Obj: pointer): char;
begin
  Result := #0;
end;

function TLyseeNilType.AsFloat(Obj: pointer): double;
begin
  Result := 0;
end;

function TLyseeNilType.AsInteger(Obj: pointer): int64;
begin
  Result := 0;
end;

function TLyseeNilType.AsCurrency(Obj: pointer): currency;
begin
  Result := 0;
end;

function TLyseeNilType.AsString(Obj: pointer): string;
begin
  Result := '';
end;

function TLyseeNilType.AsTime(Obj: pointer): TDateTime;
begin
  Result := 0;
end;

procedure TLyseeNilType.Convert(Value: TLyseeValue);
begin
  Value.SetNil;
end;

function TLyseeNilType.ConvertTo(Value: TLyseeValue; T: TLyseeType): boolean;
begin
  T.SetDefault(Value);
  Result := true;
end;

procedure TLyseeNilType.SetDefault(Value: TLyseeValue);
begin
  Value.SetNil;
end;

function TLyseeNilType.DecRefcount(Obj: pointer): integer;
begin
  Result := 0;
end;

function TLyseeNilType.IncRefcount(Obj: pointer): integer;
begin
  Result := 0;
end;

{ TLyseeCharType }

function TLyseeCharType.AsBoolean(Obj: pointer): boolean;
begin
  Result := (PChar(Obj)^ <> #0);
end;

function TLyseeCharType.AsChar(Obj: pointer): char;
begin
  Result := PChar(Obj)^;
end;

function TLyseeCharType.AsFloat(Obj: pointer): double;
begin
  Result := AsInteger(Obj);
end;

function TLyseeCharType.AsInteger(Obj: pointer): int64;
begin
  Result := StrToInt(AsString(Obj));
end;

function TLyseeCharType.AsCurrency(Obj: pointer): currency;
begin
  Result := AsInteger(Obj);
end;

function TLyseeCharType.AsString(Obj: pointer): string;
begin
  Result := PChar(Obj)^;
end;

procedure TLyseeCharType.Convert(Value: TLyseeValue);
var
  tmpv: char;
begin
  if Value.FType <> Self then
  begin
    tmpv := Value.FType.AsChar(Value.GetOA);
    Value.FType.DecRefcount(Value.FValue.VObject);
    Value.FType := Self;
    Value.FValue.VChar[0] := tmpv;
    Value.FValue.VChar[1] := #0;
  end;
end;

procedure TLyseeCharType.SetDefault(Value: TLyseeValue);
begin
  inherited;
  Value.FValue.VChar[0] := #0;
  Value.FValue.VChar[1] := #0;
end;

{ TLyseeExceptionType }

procedure TLyseeExceptionType.MyID(const Param: TLyseeParam);
begin
  Param.Result.AsString := Param.FLysee.FError.FErrID;
end;

procedure TLyseeExceptionType.MyMsg(const Param: TLyseeParam);
begin
  Param.Result.AsString := Param.FLysee.FError.FMsg;
end;

procedure TLyseeExceptionType.MyText(const Param: TLyseeParam);
begin
  Param.Result.AsString := Param.FLysee.FError.ErrorText;
end;

procedure TLyseeExceptionType.MyModule(const Param: TLyseeParam);
begin
  Param.Result.AsString := Param.FLysee.FError.FModule;
end;

procedure TLyseeExceptionType.MyFileName(const Param: TLyseeParam);
begin
  Param.Result.AsString := Param.FLysee.FError.FFileName;
end;

procedure TLyseeExceptionType.MyRow(const Param: TLyseeParam);
begin
  Param.Result.AsInteger := Param.FLysee.FError.FRow;
end;

procedure TLyseeExceptionType.MyCol(const Param: TLyseeParam);
begin
  Param.Result.AsInteger := Param.FLysee.FError.FCol;
end;

procedure TLyseeExceptionType.MyExcepted(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := Param.FLysee.FExcepted;
end;

procedure TLyseeExceptionType.Setup;
begin
  Method('ID', my_string, {$IFDEF FPC}@{$ENDIF}MyID);
  Method('Msg', my_string, {$IFDEF FPC}@{$ENDIF}MyMsg);
  Method('Text', my_string, {$IFDEF FPC}@{$ENDIF}MyText);
  Method('Module', my_string, {$IFDEF FPC}@{$ENDIF}MyModule);
  Method('FileName', my_string, {$IFDEF FPC}@{$ENDIF}MyFileName);
  Method('Row', my_int, {$IFDEF FPC}@{$ENDIF}MyRow);
  Method('Col', my_int, {$IFDEF FPC}@{$ENDIF}MyCol);
  Method('Excepted', my_bool, {$IFDEF FPC}@{$ENDIF}MyExcepted);
  inherited;
end;

function TLyseeExceptionType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyseeError(Obj).ErrorText else
    Result := '';
end;

{ TLyseeStringType }

procedure TLyseeStringType.MyGet(const Param: TLyseeParam);
var
  S: string;
begin
  S := Param[0].AsString;
  Param.Result.AsChar := S[Param[1].AsInteger];
end;

procedure TLyseeStringType.Setup;
begin
  Define('', my_char, 'Index', my_int, {$IFDEF FPC}@{$ENDIF}MyGet);
  inherited;
end;

function TLyseeStringType.AsBoolean(Obj: pointer): boolean;
begin
  Result := (Obj <> nil) and (TLyseeString(Obj).FValue <> '');
end;

function TLyseeStringType.AsChar(Obj: pointer): char;
begin
  if (Obj <> nil) and (TLyseeString(Obj).FValue <> '') then
    Result := TLyseeString(Obj).FValue[1] else
    Result := #0;
end;

function TLyseeStringType.AsFloat(Obj: pointer): double;
begin
  Result := StrToFloat(AsString(Obj));
end;

function TLyseeStringType.AsInteger(Obj: pointer): int64;
begin
  Result := StrToInt(AsString(Obj));
end;

function TLyseeStringType.AsCurrency(Obj: pointer): currency;
begin
  Result := StrToCurr(AsString(Obj));
end;

function TLyseeStringType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyseeString(Obj).FValue else
    Result := '';
end;

function TLyseeStringType.AsTime(Obj: pointer): TDateTime;
begin
  Result := StrToDateTime(AsString(Obj));
end;

function TLyseeStringType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeString(Obj).DecRefcount else
    Result := 0;
end;

function TLyseeStringType.Generate(Obj: pointer): TLyseeGenerate;
var
  S: string;
begin
  S := AsString(Obj);
  if S <> '' then
    Result := TLyseeStringGenerate.CreateIn(S) else
    Result := nil;
end;

function TLyseeStringType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeString(Obj).IncRefcount else
    Result := 0;
end;

function TLyseeStringType.GetLength(Obj: pointer): int64;
begin
  Result := Length(AsString(Obj));
end;

procedure TLyseeStringType.Convert(Value: TLyseeValue);
var
  tmpv: string;
begin
  if Value.FType <> Self then
  begin
    tmpv := Value.FType.AsString(Value.GetOA);
    Value.FType.DecRefcount(Value.FValue.VObject);
    Value.FType := Self;
    Value.FValue.VObject := TLyseeString.CreateIncRefcount(tmpv);
  end;
end;

{ TLyseeIntegerType }

function TLyseeIntegerType.AsBoolean(Obj: pointer): boolean;
begin
  Result := (PInt64(Obj)^ <> 0);
end;

function TLyseeIntegerType.AsChar(Obj: pointer): char;
begin
  Result := char(PInt64(Obj)^);
end;

function TLyseeIntegerType.AsFloat(Obj: pointer): double;
begin
  Result := PInt64(Obj)^;
end;

function TLyseeIntegerType.AsInteger(Obj: pointer): int64;
begin
  Result := PInt64(Obj)^;
end;

function TLyseeIntegerType.AsCurrency(Obj: pointer): currency;
begin
  Result := PInt64(Obj)^;
end;

function TLyseeIntegerType.AsString(Obj: pointer): string;
begin
  Result := IntToStr(PInt64(Obj)^);
end;

function TLyseeIntegerType.AsTime(Obj: pointer): TDateTime;
begin
  Result := PInt64(Obj)^;
end;

function TLyseeIntegerType.Generate(Obj: pointer): TLyseeGenerate;
begin
  if PInt64(Obj)^ > 0 then
    Result := TLyseeIntGenerate.CreateIn(PInt64(Obj)^) else
    Result := nil;
end;

procedure TLyseeIntegerType.SetDefault(Value: TLyseeValue);
begin
  inherited;
  Value.FValue.VInteger := 0;
end;

procedure TLyseeIntegerType.Convert(Value: TLyseeValue);
var
  tmpv: int64;
begin
  if Value.FType <> Self then
  begin
    tmpv := Value.FType.AsInteger(Value.GetOA);
    Value.FType.DecRefcount(Value.FValue.VObject);
    Value.FType := Self;
    Value.FValue.VInteger := tmpv;
  end;
end;

{ TLyseeFloatType }

function TLyseeFloatType.AsBoolean(Obj: pointer): boolean;
begin
  Result := not IsZero(PDouble(Obj)^);
end;

function TLyseeFloatType.AsFloat(Obj: pointer): double;
begin
  Result := PDouble(Obj)^;
end;

function TLyseeFloatType.AsInteger(Obj: pointer): int64;
begin
  Result := Trunc(PDouble(Obj)^);
end;

function TLyseeFloatType.AsCurrency(Obj: pointer): currency;
begin
  Result := PDouble(Obj)^;
end;

function TLyseeFloatType.AsString(Obj: pointer): string;
begin
  Result := FloatToStr(PDouble(Obj)^);
end;

function TLyseeFloatType.AsTime(Obj: pointer): TDateTime;
begin
  Result := PDouble(Obj)^;
end;

procedure TLyseeFloatType.SetDefault(Value: TLyseeValue);
begin
  inherited;
  Value.FValue.VFloat := 0;
end;

procedure TLyseeFloatType.Convert(Value: TLyseeValue);
var
  tmpv: double;
begin
  if Value.FType <> Self then
  begin
    tmpv := Value.FType.AsFloat(Value.GetOA);
    Value.FType.DecRefcount(Value.FValue.VObject);
    Value.FType := Self;
    Value.FValue.VFloat := tmpv;
  end;
end;

{ TLyseeCurrencyType }

function TLyseeCurrencyType.AsBoolean(Obj: pointer): boolean;
begin
  Result := (PCurrency(Obj)^ <> 0);
end;

function TLyseeCurrencyType.AsFloat(Obj: pointer): double;
begin
  Result := PCurrency(Obj)^;
end;

function TLyseeCurrencyType.AsInteger(Obj: pointer): int64;
begin
  Result := Trunc(PCurrency(Obj)^);
end;

function TLyseeCurrencyType.AsCurrency(Obj: pointer): currency;
begin
  Result := PCurrency(Obj)^;
end;

function TLyseeCurrencyType.AsString(Obj: pointer): string;
begin
  Result := CurrToStr(PCurrency(Obj)^);
end;

function TLyseeCurrencyType.AsTime(Obj: pointer): TDateTime;
begin
  Result := PCurrency(Obj)^;
end;

procedure TLyseeCurrencyType.SetDefault(Value: TLyseeValue);
begin
  inherited;
  Value.FValue.VCurrency := 0;
end;

procedure TLyseeCurrencyType.Convert(Value: TLyseeValue);
var
  tmpv: currency;
begin
  if Value.FType <> Self then
  begin
    tmpv := Value.FType.AsCurrency(Value.GetOA);
    Value.FType.DecRefcount(Value.FValue.VObject);
    Value.FType := Self;
    Value.FValue.VCurrency := tmpv;
  end;
end;

{ TLyseeTimeType }

function TLyseeTimeType.AsBoolean(Obj: pointer): boolean;
begin
  Result := not IsZero(PDateTime(Obj)^);
end;

function TLyseeTimeType.AsFloat(Obj: pointer): double;
begin
  Result := PDateTime(Obj)^;
end;

function TLyseeTimeType.AsInteger(Obj: pointer): int64;
begin
  Result := Trunc(PDateTime(Obj)^);
end;

function TLyseeTimeType.AsString(Obj: pointer): string;
begin
  Result := DateTimeToStr(PDateTime(Obj)^);
end;

function TLyseeTimeType.AsTime(Obj: pointer): TDateTime;
begin
  Result := PDateTime(Obj)^;
end;

procedure TLyseeTimeType.SetDefault(Value: TLyseeValue);
begin
  inherited;
  Value.FValue.VTime := 0;
end;

procedure TLyseeTimeType.Convert(Value: TLyseeValue);
var
  tmpv: TDateTime;
begin
  if Value.FType <> Self then
  begin
    tmpv := Value.FType.AsTime(Value.GetOA);
    Value.FType.DecRefcount(Value.FValue.VObject);
    Value.FType := Self;
    Value.FValue.VTime := tmpv;
  end;
end;

{ TLyseeBoolType }

function TLyseeBoolType.AsBoolean(Obj: pointer): boolean;
begin
  Result := PBoolean(Obj)^;
end;

function TLyseeBoolType.AsInteger(Obj: pointer): int64;
begin
  Result := Ord(PBoolean(Obj)^);
end;

function TLyseeBoolType.AsString(Obj: pointer): string;
begin
  Result := IntToStr(Ord(PBoolean(Obj)^));
end;

procedure TLyseeBoolType.SetDefault(Value: TLyseeValue);
begin
  inherited;
  Value.FValue.VBoolean := false;
end;

procedure TLyseeBoolType.Convert(Value: TLyseeValue);
var
  tmpv: boolean;
begin
  if Value.FType <> Self then
  begin
    tmpv := Value.FType.AsBoolean(Value.GetOA);
    Value.FType.DecRefcount(Value.FValue.VObject);
    Value.FType := Self;
    Value.FValue.VBoolean := tmpv;
  end;
end;

{ TLyseeTypeType }

procedure TLyseeTypeType.MyFindMethod(const Param: TLyseeParam);
begin
  Param.Result.AsFunc := Param[0].AsType.FindMethod(Param[1].AsString);
end;

procedure TLyseeTypeType.MyIsChildTypeOf(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := Param[0].AsType.IsChildTypeOf(Param[1].AsType);
end;

procedure TLyseeTypeType.MyIsEnum(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := Param[0].AsType.IsEnum;
end;

procedure TLyseeTypeType.MyIsEnumSet(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := Param[0].AsType.IsEnumSet;
end;

procedure TLyseeTypeType.MyIsNil(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := Param[0].AsType.IsNil;
end;

procedure TLyseeTypeType.MyIsObject(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := Param[0].AsType.IsObject;
end;

procedure TLyseeTypeType.MyIsTypeOf(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := Param[0].AsType.IsTypeOf(Param[1].AsType);
end;

procedure TLyseeTypeType.MyItemValues(const Param: TLyseeParam);
var
  T: TLyseeType;
  O: TLyseeEnumType;
  L: TLyseeList;
  I: integer;
begin
  L := Param.Result.NewList;
  T := Param[0].AsType;
  if T.IsEnum then
  begin
    O := TLyseeEnumType(T);
    for I := 0 to O.GetCount - 1 do
      L.Add.SetTOA(O, O[I]);
  end;
end;

procedure TLyseeTypeType.MyMethods(const Param: TLyseeParam);
var
  T: TLyseeType;
  L: TLyseeList;
  I: integer;
begin
  L := Param.Result.NewList;
  T := Param[0].AsType;
  for I := 0 to T.GetMethodCount - 1 do
    L.Add.AsFunc := T.GetMethod(I);
end;

procedure TLyseeTypeType.MyModule(const Param: TLyseeParam);
begin
  Param.Result.AsModule := Param[0].AsType.Module;
end;

procedure TLyseeTypeType.MyName(const Param: TLyseeParam);
begin
  Param.Result.AsString := Param[0].AsType.Name;
end;

procedure TLyseeTypeType.MyParent(const Param: TLyseeParam);
begin
  Param.Result.AsType := Param[0].AsType.Parent;
end;

procedure TLyseeTypeType.MyPrototype(const Param: TLyseeParam);
begin
  Param.Result.AsString := Param[0].AsType.Prototype(Param[1].AsString);
end;

procedure TLyseeTypeType.Setup;
begin
  Method('Name', my_string, {$IFDEF FPC}@{$ENDIF}MyName);
  Method('Parent', Self, {$IFDEF FPC}@{$ENDIF}MyParent);
  Method('Module', my_module, {$IFDEF FPC}@{$ENDIF}MyModule);
  Method('Methods', my_array, {$IFDEF FPC}@{$ENDIF}MyMethods);
  Method('IsTypeOf', my_bool, ['AType'], [my_type],
         {$IFDEF FPC}@{$ENDIF}MyIsTypeOf);
  Method('IsChildTypeOf', my_bool, ['AType'], [my_type],
         {$IFDEF FPC}@{$ENDIF}MyIsChildTypeOf);
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

function TLyseeTypeType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyseeType(Obj).FullName else
    Result := '';
end;

procedure TLyseeTypeType.Convert(Value: TLyseeValue);
var
  tmpv: TLyseeType;
begin
  if Value.FType <> Self then
  begin
    tmpv := Value.FType;
    Value.FType.DecRefcount(Value.FValue.VObject);
    Value.FType := Self;
    Value.FValue.VObject := tmpv;
  end;
end;

{ TLyseeFuncType }

procedure TLyseeFuncType.MyIsConstructor(const Param: TLyseeParam);
var
  F: TLyseeFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsBoolean := F.IsConstructor;
end;

procedure TLyseeFuncType.MyIsMainFunc(const Param: TLyseeParam);
var
  F: TLyseeFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsBoolean := F.IsMainFunc;
end;

procedure TLyseeFuncType.MyIsMethod(const Param: TLyseeParam);
var
  F: TLyseeFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsBoolean := F.IsMethod;
end;

procedure TLyseeFuncType.MyAddCode(const Param: TLyseeParam);
var
  F: TLyseeFunc;
begin
  if Param.GetChangeAbleFunc(F) then
    Param.Result.AsBoolean := F.AddCode(Param[1].AsString);
end;

procedure TLyseeFuncType.MyAddParam(const Param: TLyseeParam);
var
  F: TLyseeFunc;
  S: string;
  T: TLyseeType;
begin
  if Param.GetChangeAbleFunc(F) then
    if F.FParams.LocalCount = 0 then
    begin
      S := Param[1].AsString;
      if Param.Prmc > 2 then
        T := Param[2].AsType else
        T := my_variant;
      if IsParam(S, T) and not F.FindInside(S) then
      begin
        F.FParams.Add(S, T);
        F.FMinArgs := -1;
        Param.Result.AsBoolean := true;
      end;
    end;
end;

procedure TLyseeFuncType.MyClear(const Param: TLyseeParam);
var
  F: TLyseeFunc;
begin
  if Param.GetChangeAbleFunc(F) then
  begin
    F.FMinArgs := -1;
    F.FParams.Clear;
    F.FSTMTs.Clear;
  end;
end;

procedure TLyseeFuncType.MyClearCodes(const Param: TLyseeParam);
var
  F: TLyseeFunc;
begin
  if Param.GetChangeAbleFunc(F) then
    F.FSTMTs.Clear;
end;

procedure TLyseeFuncType.MyClearParams(const Param: TLyseeParam);
var
  F: TLyseeFunc;
begin
  if Param.GetChangeAbleFunc(F) then
  begin
    F.FMinArgs := -1;
    F.FParams.Clear;
  end;
end;

procedure TLyseeFuncType.MyGetParamName(const Param: TLyseeParam);
var
  F: TLyseeFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsString := F.Params[Param[1].AsInteger].FName;
end;

procedure TLyseeFuncType.MyGetParamType(const Param: TLyseeParam);
var
  F: TLyseeFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsType := F.Params[Param[1].AsInteger].FType;
end;

procedure TLyseeFuncType.MyGetResultType(const Param: TLyseeParam);
var
  F: TLyseeFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsType := F.ResultType;
end;

procedure TLyseeFuncType.MyIsChangeAble(const Param: TLyseeParam);
var
  F: TLyseeFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsBoolean := F.ChangeAble;
end;

procedure TLyseeFuncType.MyModule(const Param: TLyseeParam);
var
  F: TLyseeFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsModule := F.FModule;
end;

procedure TLyseeFuncType.MyName(const Param: TLyseeParam);
var
  F: TLyseeFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsString := F.Name;
end;

procedure TLyseeFuncType.MyParamCount(const Param: TLyseeParam);
var
  F: TLyseeFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsInteger := F.Params.ParamCount;
end;

procedure TLyseeFuncType.MyParamNames(const Param: TLyseeParam);
var
  F: TLyseeFunc;
  L: TLyseeList;
  I: integer;
begin
  if Param.GetSelf(F) then
  begin
    L := Param.Result.NewList;
    for I := 0 to F.Params.ParamCount - 1 do
      L.Add.AsString := F.Params[I].FName;
  end;
end;

procedure TLyseeFuncType.MyParamTypes(const Param: TLyseeParam);
var
  F: TLyseeFunc;
  L: TLyseeList;
  I: integer;
begin
  if Param.GetSelf(F) then
  begin
    L := Param.Result.NewList;
    for I := 0 to F.FParams.ParamCount - 1 do
      L.Add.AsType := F.Params[I].FType;
  end;
end;

procedure TLyseeFuncType.MyParent(const Param: TLyseeParam);
var
  F: TLyseeFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsType := F.FParent;
end;

procedure TLyseeFuncType.MyPrototype(const Param: TLyseeParam);
var
  F: TLyseeFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsString := F.Prototype;
end;

procedure TLyseeFuncType.MySetCode(const Param: TLyseeParam);
var
  F: TLyseeFunc;
begin
  if Param.GetChangeAbleFunc(F) then
    Param.Result.AsBoolean := F.SetCode(Param[1].AsString);
end;

procedure TLyseeFuncType.MySetParamName(const Param: TLyseeParam);
var
  F: TLyseeFunc;
  S: string;
  X: integer;
begin
  if Param.GetChangeAbleFunc(F) then
  begin
    X := Param[1].AsInteger;
    S := Param[2].AsString;
    if not MatchID(S, F.Params[X].FName) then
      if IsParam(S, my_int) and not F.FindInside(S) then
      begin
        F.Params[X].FName := S;
        F.FMinArgs := -1;
      end
      else Param.Error('invalid param name: ' + S);
  end;
end;

procedure TLyseeFuncType.MySetParamType(const Param: TLyseeParam);
var
  F: TLyseeFunc;
  X: integer;
  T: TLyseeType;
begin
  if Param.GetChangeAbleFunc(F) then
  begin
    X := Param[1].AsInteger;
    T := Param[2].AsType;
    if T <> F.Params[X].FType then
      if T = my_nil then
        Param.Error('invalid param type: nil') else
        F.Params[X].FType := T;
  end;
end;

procedure TLyseeFuncType.MySetResultType(const Param: TLyseeParam);
var
  F: TLyseeFunc;
begin
  if Param.GetChangeAbleFunc(F) then
    F.ResultType := Param[1].AsType;
end;

procedure TLyseeFuncType.Setup;
begin
  Method('Name', my_string, {$IFDEF FPC}@{$ENDIF}MyName);
  Method('Prototype', my_string, {$IFDEF FPC}@{$ENDIF}MyPrototype);
  Method('Parent', my_type, {$IFDEF FPC}@{$ENDIF}MyParent);
  Method('Module', my_module, {$IFDEF FPC}@{$ENDIF}MyModule);
  Method('IsMainFunc', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsMainFunc);
  Method('IsMethod', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsMethod);
  Method('IsConstructor', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsConstructor);
  Method('IsChangeAble', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsChangeAble);
  Method('ParamCount', my_int, {$IFDEF FPC}@{$ENDIF}MyParamCount);
  Method('ParamNames', my_array, {$IFDEF FPC}@{$ENDIF}MyParamNames);
  Method('ParamTypes', my_array, {$IFDEF FPC}@{$ENDIF}MyParamTypes);
  Method('Clear', {$IFDEF FPC}@{$ENDIF}MyClear);
  Method('Clear', {$IFDEF FPC}@{$ENDIF}MyClearParams);
  Method('ClearCodes', {$IFDEF FPC}@{$ENDIF}MyClearCodes);
  Method('AddParam', my_bool, ['Name', '_Type'], [my_string, my_type],
         {$IFDEF FPC}@{$ENDIF}MyAddParam);
  Method('AddCode', my_bool, ['Code'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyAddCode);
  Method('SetCode', my_bool, ['Code'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MySetCode);
  Define('ResultType', my_type,
         {$IFDEF FPC}@{$ENDIF}MyGetResultType,
         {$IFDEF FPC}@{$ENDIF}MySetResultType);
  Define('ParamNames', my_string, 'Index', my_int,
         {$IFDEF FPC}@{$ENDIF}MyGetParamName,
         {$IFDEF FPC}@{$ENDIF}MySetParamName);
  Define('ParamTypes', my_type, 'Index', my_int,
         {$IFDEF FPC}@{$ENDIF}MyGetParamType,
         {$IFDEF FPC}@{$ENDIF}MySetParamType);
  inherited;
end;

function TLyseeFuncType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyseeFunc(Obj).Prototype else
    Result := '';
end;

function TLyseeFuncType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeFunc(Obj).DecRefcount else
    Result := 0;
end;

function TLyseeFuncType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeFunc(Obj).IncRefcount else
    Result := 0;
end;

{ TLyseeModuleType }

procedure TLyseeModuleType.MyConsts(const Param: TLyseeParam);
var
  M: TLyseeModule;
begin
  if Param.GetSelf(M) then
    M.FConsts.ListKeys(Param.Result.NewList);
end;

procedure TLyseeModuleType.MyFuncs(const Param: TLyseeParam);
var
  M: TLyseeModule;
  L: TLyseeList;
  I: integer;
begin
  if Param.GetSelf(M) then
  begin
    L := Param.Result.NewList;
    for I := 0 to M.FFuncList.Count - 1 do
      L.Add.SetAsFunc(TLyseeFunc(M.FFuncList[I]));
  end;
end;

procedure TLyseeModuleType.MyFind(const Param: TLyseeParam);
var
  M: TLyseeModule;
begin
  if Param.GetSelf(M) then
    M.FindSave(Param[1].AsString, Param.FResult);
end;

procedure TLyseeModuleType.MyName(const Param: TLyseeParam);
var
  M: TLyseeModule;
begin
  if Param.GetSelf(M) then
    Param.Result.AsString := M.FName;
end;

procedure TLyseeModuleType.MyTypes(const Param: TLyseeParam);
var
  M: TLyseeModule;
  L: TLyseeList;
  I: integer;
begin
  if Param.GetSelf(M) then
  begin
    L := Param.Result.NewList;
    for I := 0 to M.TypeCount - 1 do
      L.Add.AsType := M.GetType(I);
  end;
end;

procedure TLyseeModuleType.MyUsings(const Param: TLyseeParam);
var
  M: TLyseeModule;
begin
  if Param.GetSelf(M) then
    if M.FModules <> nil then
      Param.Result.AsArray := M.FModules.ToList else
      Param.Result.NewList;
end;

procedure TLyseeModuleType.Setup;
begin
  Method('Name', my_string, {$IFDEF FPC}@{$ENDIF}MyName);
  Method('Consts', my_array, {$IFDEF FPC}@{$ENDIF}MyConsts);
  Method('Types', my_array, {$IFDEF FPC}@{$ENDIF}MyTypes);
  Method('Funcs', my_array, {$IFDEF FPC}@{$ENDIF}MyFuncs);
  Method('Usings', my_array, {$IFDEF FPC}@{$ENDIF}MyUsings);
  Method('Find', my_variant, ['Name'], [my_string], {$IFDEF FPC}@{$ENDIF}MyFind);
  inherited;
end;

function TLyseeModuleType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyseeModule(Obj).FName else
    Result := '';
end;

function TLyseeModuleType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeModule(Obj).DecRefcount else
    Result := 0;
end;

function TLyseeModuleType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeModule(Obj).IncRefcount else
    Result := 0;
end;

{ TLyseeArrayType }

procedure TLyseeArrayType.MyAdd(const Param: TLyseeParam);
var
  L, A: TLyseeList;
  I: integer;
begin
  if Param.GetSelf(L) then
  begin
    L.TestChange;
    L.Add(Param[1]);
    Param.Result.AsInteger := L.Count - 1;
    A := Param.VarArgs;
    if A <> nil then
      for I := 0 to A.Count - 1 do
        L.Add(A[I]);
  end;
end;

procedure TLyseeArrayType.MyAssign(const Param: TLyseeParam);
var
  L, R: TLyseeList;
  I: integer;
  T: TLyseeType;
begin
  if Param.GetSelf(L) then
  begin
    L.TestChange;
    T := Param.Params[1].FType;
    if (T = my_array) or (T = Self) then
    begin
      R := TLyseeList(Param[1].GetOA);
      if L <> R then
      begin
        L.Clear;
        if R <> nil then
          for I := 0 to R.Count - 1 do
            L.Add(R[I]);
      end;
    end;
  end;
end;

procedure TLyseeArrayType.MyClear(const Param: TLyseeParam);
var
  L: TLyseeList;
begin
  if Param.GetSelf(L) then
  begin
    L.TestChange;
    L.Clear;
  end;
end;

procedure TLyseeArrayType.MyCopy(const Param: TLyseeParam);
var
  L: TLyseeList;
  X: integer;
  N: integer;
begin
  if Param.GetSelf(L) then
  begin
    X := Param[1].AsInteger;
    N := Param[2].AsInteger;
    Param.Result.SetTOA(Self, L.Copy(X, N));
  end;
end;

procedure TLyseeArrayType.MyCreate(const Param: TLyseeParam);
begin
  Param.Result.SetTOA(Self, TLyseeList.Create);
end;

procedure TLyseeArrayType.MyDelete(const Param: TLyseeParam);
var
  L: TLyseeList;
begin
  if Param.GetSelf(L) then
  begin
    L.TestChange;
    L.Delete(Param[1].AsInteger);
  end;
end;

procedure TLyseeArrayType.MyExchange(const Param: TLyseeParam);
var
  L: TLyseeList;
begin
  if Param.GetSelf(L) then
  begin
    L.TestChange;
    L.Exchange(Param[1].AsInteger, Param[2].AsInteger);
  end;
end;

procedure TLyseeArrayType.MyGet(const Param: TLyseeParam);
var
  L: TLyseeList;
begin
  if Param.GetSelf(L) then
    Param.Result.SetValue(L[Param[1].AsInteger]);
end;

procedure TLyseeArrayType.MyGetCount(const Param: TLyseeParam);
var
  L: TLyseeList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.Count;
end;

procedure TLyseeArrayType.MyIndexOf(const Param: TLyseeParam);
var
  L: TLyseeList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.IndexOf(Param[1]);
end;

procedure TLyseeArrayType.MyInsert(const Param: TLyseeParam);
var
  L: TLyseeList;
begin
  if Param.GetSelf(L) then
  begin
    L.TestChange;
    L.Insert(Param[1].AsInteger, Param[2]);
  end;
end;

procedure TLyseeArrayType.MyIsEmpty(const Param: TLyseeParam);
var
  L: TLyseeList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsBoolean := (L.Count = 0);
end;

procedure TLyseeArrayType.MyLeft(const Param: TLyseeParam);
var
  L: TLyseeList;
  N: integer;
begin
  if Param.GetSelf(L) then
  begin
    N := Param[1].AsInteger;
    Param.Result.SetTOA(Self, L.CopyLeft(N));
  end;
end;

procedure TLyseeArrayType.MyMove(const Param: TLyseeParam);
var
  L: TLyseeList;
begin
  if Param.GetSelf(L) then
  begin
    L.TestChange;
    L.Move(Param[1].AsInteger, Param[2].AsInteger);
  end;
end;

procedure TLyseeArrayType.MyRemove(const Param: TLyseeParam);
var
  L: TLyseeList;
begin
  if Param.GetSelf(L) then
  begin
    L.TestChange;
    L.Remove(Param[1]);
  end;
end;

procedure TLyseeArrayType.MyRight(const Param: TLyseeParam);
var
  L: TLyseeList;
  N: integer;
begin
  if Param.GetSelf(L) then
  begin
    N := Param[1].AsInteger;
    Param.Result.SetTOA(Self, L.CopyRight(N));
  end;
end;

procedure TLyseeArrayType.MySet(const Param: TLyseeParam);
var
  L: TLyseeList;
begin
  if Param.GetSelf(L) then
  begin
    L.TestChange;
    L[Param[1].AsInteger].SetValue(Param[2]);
  end;
end;

procedure TLyseeArrayType.MySetCount(const Param: TLyseeParam);
var
  L: TLyseeList;
begin
  if Param.GetSelf(L) then
  begin
    L.TestChange;
    L.SetCount(Param[1].AsInteger);
  end;
end;

procedure TLyseeArrayType.MySort(const Param: TLyseeParam);
var
  L: TLyseeList;
begin
  if Param.GetSelf(L) then
  begin
    L.TestChange;
    L.Sort;
  end;
end;

procedure TLyseeArrayType.Setup;
begin
  Method('Create', Self, {$IFDEF FPC}@{$ENDIF}MyCreate);
  Method('IsEmpty', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsEmpty);
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
  Method('Assign', ['Source'], [my_variant], {$IFDEF FPC}@{$ENDIF}MyAssign);
  Method('IndexOf', my_int, ['Value'], [my_variant],
         {$IFDEF FPC}@{$ENDIF}MyIndexOf);
  Method('Copy', Self, ['Index', 'Count'], [my_int, my_int],
         {$IFDEF FPC}@{$ENDIF}MyCopy);
  Method('Left', Self, ['Count'], [my_int], {$IFDEF FPC}@{$ENDIF}MyLeft);
  Method('Right', Self, ['Count'], [my_int], {$IFDEF FPC}@{$ENDIF}MyRight);
  Define('Count', my_int,
         {$IFDEF FPC}@{$ENDIF}MyGetCount,
         {$IFDEF FPC}@{$ENDIF}MySetCount);
  Define('', my_variant, 'Index', my_int,
         {$IFDEF FPC}@{$ENDIF}MyGet,
         {$IFDEF FPC}@{$ENDIF}MySet);

  my_variant.AddOperate(opIn, Self, {$IFDEF FPC}@{$ENDIF}variant_in_array);
  AddOperate(opAdd, Self, {$IFDEF FPC}@{$ENDIF}array_add_array);
  AddOperate(opDec, Self, {$IFDEF FPC}@{$ENDIF}array_dec_array);
  AddOperate(opMul, Self, {$IFDEF FPC}@{$ENDIF}array_mul_array);
  if Self <> my_array then
  begin
    AddOperate(opAdd, my_array, {$IFDEF FPC}@{$ENDIF}array_add_array);
    AddOperate(opDec, my_array, {$IFDEF FPC}@{$ENDIF}array_dec_array);
    AddOperate(opMul, my_array, {$IFDEF FPC}@{$ENDIF}array_mul_array);
    my_array.AddOperate(opAdd, Self, {$IFDEF FPC}@{$ENDIF}array_add_array);
    my_array.AddOperate(opDec, Self, {$IFDEF FPC}@{$ENDIF}array_dec_array);
    my_array.AddOperate(opMul, Self, {$IFDEF FPC}@{$ENDIF}array_mul_array);
  end;
  AddOperate(opShi, my_variant, {$IFDEF FPC}@{$ENDIF}array_shi_variant);
  AddOperate(opFill, my_variant, {$IFDEF FPC}@{$ENDIF}array_fill_variant);
  inherited;
end;

function TLyseeArrayType.Add(Obj: pointer; Value: TLyseeValue): integer;
var
  L: TLyseeList;
begin
  Validate(Obj);
  L := TLyseeList(Obj);
  TLyseeList(Obj).TestChange;
  Result := L.Count;
  L.Add(Value);
end;

function TLyseeArrayType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyseeList(Obj).AsString else
    Result := '';
end;

function TLyseeArrayType.Clear(Obj: pointer): boolean;
begin
  Result := (Obj <> nil);
  if Result then
  begin
    TLyseeList(Obj).TestChange;
    TLyseeList(Obj).Clear;
  end;
end;

function TLyseeArrayType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeList(Obj).DecRefcount else
    Result := 0;
end;

procedure TLyseeArrayType.GcMark(Obj: pointer);
begin
  if Obj <> nil then TLyseeList(Obj).MarkForSurvive;
end;

function TLyseeArrayType.Generate(Obj: pointer): TLyseeGenerate;
begin
  if (Obj <> nil) and (TLyseeList(Obj).Count > 0) then
    Result := TLyseeListGenerate.CreateIn(TLyseeList(Obj)) else
    Result := nil;
end;

function TLyseeArrayType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeList(Obj).IncRefcount else
    Result := 0;
end;

function TLyseeArrayType.GetLength(Obj: pointer): int64;
begin
  if Obj <> nil then
    Result := TLyseeList(Obj).Count else
    Result := 0;
end;

{ TLyseeHashType }

procedure TLyseeHashType.MyClear(const Param: TLyseeParam);
var
  H: TLyseeHash;
begin
  if Param.GetSelf(H) then H.Clear;
end;

procedure TLyseeHashType.MyCreate(const Param: TLyseeParam);
begin
  Param.Result.AsHash := TLyseeHash.Create;
end;

procedure TLyseeHashType.MyGet(const Param: TLyseeParam);
var
  H: TLyseeHash;
  V: TLyseeValue;
begin
  if Param.GetSelf(H) then
  begin
    V := H.Get(Param[1].AsString);
    if V <> nil then
      Param.Result.SetValue(V);
  end;
end;

procedure TLyseeHashType.MyHas(const Param: TLyseeParam);
var
  H: TLyseeHash;
begin
  if Param.GetSelf(H) then
    Param.Result.AsBoolean := H.Has(Param[1].AsString);
end;

procedure TLyseeHashType.MyIsEmpty(const Param: TLyseeParam);
var
  H: TLyseeHash;
begin
  if Param.GetSelf(H) then
    Param.Result.AsBoolean := H.IsEmpty;
end;

procedure TLyseeHashType.MyKeys(const Param: TLyseeParam);
var
  H: TLyseeHash;
  L: TLyseeList;
begin
  if Param.GetSelf(H) then
  begin
    L := TLyseeList.Create;
    Param.Result.AsArray := L;
    H.ListKeys(L);
  end;
end;

procedure TLyseeHashType.MyRemove(const Param: TLyseeParam);
var
  H: TLyseeHash;
begin
  if Param.GetSelf(H) then
    H.Remove(Param[1].AsString);
end;

procedure TLyseeHashType.MySet(const Param: TLyseeParam);
var
  H: TLyseeHash;
begin
  if Param.GetSelf(H) then
    H.Add(Param[1].AsString).SetValue(Param[2]);
end;

procedure TLyseeHashType.MyValues(const Param: TLyseeParam);
var
  H: TLyseeHash;
  L: TLyseeList;
begin
  if Param.GetSelf(H) then
  begin
    L := TLyseeList.Create;
    Param.Result.AsArray := L;
    H.ListValues(L);
  end;
end;

procedure TLyseeHashType.Setup;
begin
  Method('Create', Self, {$IFDEF FPC}@{$ENDIF}MyCreate);
  Method('IsEmpty', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsEmpty);
  Method('Clear', {$IFDEF FPC}@{$ENDIF}MyClear);
  Method('Has', my_bool, ['Key'], [my_string], {$IFDEF FPC}@{$ENDIF}MyHas);
  Method('Remove', ['Key'], [my_string], {$IFDEF FPC}@{$ENDIF}MyRemove);
  Method('Keys', my_array, {$IFDEF FPC}@{$ENDIF}MyKeys);
  Method('Values', my_array, {$IFDEF FPC}@{$ENDIF}MyValues);
  Define('', my_variant, 'Key', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGet,
         {$IFDEF FPC}@{$ENDIF}MySet);
  inherited;
end;

function TLyseeHashType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyseeHash(Obj).AsString else
    Result := '';
end;

function TLyseeHashType.Clear(Obj: pointer): boolean;
begin
  Result := (Obj <> nil);
  if Result then
    TLyseeHash(Obj).Clear;
end;

function TLyseeHashType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeHash(Obj).DecRefcount else
    Result := 0;
end;

procedure TLyseeHashType.GcMark(Obj: pointer);
begin
  if Obj <> nil then TLyseeHash(Obj).MarkForSurvive;
end;

function TLyseeHashType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeHash(Obj).IncRefcount else
    Result := 0;
end;

{ TLyseeEnumItem }

procedure TLyseeEnumItem.SetValue(Value: TLyseeValue);
begin
  Value.SetObject(FParent, Self);
end;

{ TLyseeEnumType }

procedure TLyseeEnumType.AddItems(const ItemNames: array of string);
var
  I: integer;
begin
  if Length(FItems) = 0 then
  begin
    SetLength(FItems, Length(ItemNames));
    for I := 0 to Length(FItems) - 1 do
    begin
      FItems[I] := TLyseeEnumItem.Create;
      FItems[I].FParent := Self;
      FItems[I].FName := ItemNames[I];
      FItems[I].FValue := I;
      FModule.FConsts.Add(ItemNames[I]).SetTOA(Self, FItems[I]);
    end;
  end;
end;

constructor TLyseeEnumType.Create(const AName: string; AModule: TLyseeModule; AParent: TLyseeType);
begin
  inherited;
  FStyle := tsEnum;
  if FModule <> nil then
    AddCompare(Self, {$IFDEF FPC}@{$ENDIF}compare_enum_enum);
end;

destructor TLyseeEnumType.Destroy;
var
  I: integer;
begin
  for I := GetCount - 1 downto 0 do
    FreeAndNil(FItems[I]);
  SetLength(FItems, 0);
  inherited;
end;

function TLyseeEnumType.Find(ItemValue: integer): TLyseeEnumItem;
var
  I: integer;
begin
  for I := 0 to GetCount - 1 do
  begin
    Result := GetItem(I);
    if ItemValue = Result.FValue then Exit;
  end;
  Result := nil;
end;

function TLyseeEnumType.Find(const ItemName: string): TLyseeEnumItem;
var
  I: integer;
begin
  for I := 0 to GetCount - 1 do
  begin
    Result := GetItem(I);
    if MatchID(ItemName, Result.FName) then Exit;
  end;
  Result := nil;
end;

function TLyseeEnumType.GetCount: integer;
begin
  Result := Length(FItems);
end;

function TLyseeEnumType.GetDefValue: TLyseeEnumItem;
begin
  Result := FItems[0];
end;

function TLyseeEnumType.GetItem(Index: integer): TLyseeEnumItem;
begin
  Result := FItems[Index];
end;

function TLyseeEnumType.ItemByName(const ItemName: string): TLyseeEnumItem;
begin
  Result := Find(ItemName);
  if Result = nil then
    Throw('%s value(name=''%s'') not exists', [Name, ItemName]);
end;

function TLyseeEnumType.ItemByValue(ItemValue: integer): TLyseeEnumItem;
begin
  Result := Find(ItemValue);
  if Result = nil then
    Throw('%s value(ordinal=%d) not exists', [Name, ItemValue]);
end;

procedure TLyseeEnumType.SetValue(Value: TLyseeValue; const ItemName: string);
begin
  Value.SetObject(Self, ItemByName(ItemName));
end;

function TLyseeEnumType.NewEnumSetType(const AName: string): TLyseeEnumSetType;
begin
  Result := TLyseeEnumSetType.Create(AName, FModule, nil);
  Result.FStyle := tsEnumSet;
  Result.FSource := Self;
  if FModule <> nil then
  begin
    Result.AddCompare(Result, {$IFDEF FPC}@{$ENDIF}compare_enumset_enumset);
    Result.AddOperate(opAdd, Result, {$IFDEF FPC}@{$ENDIF}enumset_add_enumset);
    Result.AddOperate(opDec, Result, {$IFDEF FPC}@{$ENDIF}enumset_dec_enumset);
    Result.AddOperate(opMul, Result, {$IFDEF FPC}@{$ENDIF}enumset_mul_enumset);
    Result.AddOperate(opNot, Result, {$IFDEF FPC}@{$ENDIF}enumset_not_enumset);
  end;
end;

procedure TLyseeEnumType.SetValue(Value: TLyseeValue; ItemValue: integer);
begin
  Value.SetObject(Self, ItemByValue(ItemValue));
end;

procedure TLyseeEnumType.SetValue(Value: TLyseeValue; Item: TLyseeEnumItem);
begin
  if Item = nil then
    Value.SetObject(Self, DefValue) else
    Value.SetObject(Self, Item);
end;

function TLyseeEnumType.AsInteger(Obj: pointer): int64;
begin
  if Obj <> nil then
    Result := TLyseeEnumItem(Obj).FValue else
    Result := DefValue.FValue;
end;

function TLyseeEnumType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyseeEnumItem(Obj).FName else
    Result := DefValue.FName;
end;

procedure TLyseeEnumType.Convert(Value: TLyseeValue);
begin
  if Value.FType <> Self then
    if Value.FType = my_int then
      SetValue(Value, Value.FValue.VInteger) else
      ErrorConvert(Value.FType, Self);
end;

procedure TLyseeEnumType.SetDefault(Value: TLyseeValue);
begin
  inherited;
  Value.FValue.VObject := DefValue;
end;

function TLyseeEnumType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then Result := 1 else Result := 0;
end;

function TLyseeEnumType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then Result := 1 else Result := 0;
end;

{ TLyseeEnumSet }

function TLyseeEnumSet.GetSource: TLyseeEnumType;
begin
  Result := FParent.FSource;
end;

function TLyseeEnumSet.GetCount: integer;
begin
  Result := Length(FSets);
end;

function TLyseeEnumSet.Get(Index: integer): boolean;
begin
  Result := FSets[Index];
end;

procedure TLyseeEnumSet.Put(Index: integer; Value: boolean);
begin
  FSets[Index] := Value;
end;

destructor TLyseeEnumSet.Destroy;
begin
  SetLength(FSets, 0);
  inherited;
end;

procedure TLyseeEnumSet.SetValue(Value: TLyseeValue);
begin
  Value.SetObject(FParent, Self);
end;

function TLyseeEnumSet.AsBoolean: boolean;
var
  I: integer;
begin
  for I := 0 to Length(FSets) - 1 do
    if FSets[I] then
    begin
      Result := true;
      Exit;
    end;
  Result := false;
end;

function TLyseeEnumSet.AsString: string;
var
  I: integer;
  F: boolean;
begin
  Result := '[';
  F := false;
  for I := 0 to Length(FSets) - 1 do
    if FSets[I] then
    begin
      if F then
        Result := Result + ', ' + Source[I].FName else
        Result := Result + Source[I].FName;
      F := true;
    end;
  Result := Result + ']';
end;

function TLyseeEnumSet.Equal(S: TLyseeEnumSet): boolean;
var
  I: integer;
begin
  if S = nil then S := FParent.DefValue;
  if S <> Self then
    for I := 0 to Length(FSets) - 1 do
      if FSets[I] <> S.FSets[I] then
      begin
        Result := false;
        Exit;
      end;
  Result := true;
end;

function TLyseeEnumSet.Add(S: TLyseeEnumSet): TLyseeEnumSet;
var
  I: integer;
begin
  Result := FParent.NewEnumSet;
  for I := 0 to Length(FSets) - 1 do
    Result.FSets[I] := FSets[I] or S.FSets[I];
end;

function TLyseeEnumSet.Dec(S: TLyseeEnumSet): TLyseeEnumSet;
var
  I: integer;
begin
  Result := FParent.NewEnumSet;
  for I := 0 to Length(FSets) - 1 do
    Result.FSets[I] := FSets[I] and not S.FSets[I];
end;

function TLyseeEnumSet.Mul(S: TLyseeEnumSet): TLyseeEnumSet;
var
  I: integer;
begin
  Result := FParent.NewEnumSet;
  for I := 0 to Length(FSets) - 1 do
    Result.FSets[I] := FSets[I] and S.FSets[I];
end;

function TLyseeEnumSet.NotAll: TLyseeEnumSet;
var
  I: integer;
begin
  Result := FParent.NewEnumSet;
  for I := 0 to Length(FSets) - 1 do
    Result.FSets[I] := not FSets[I];
end;

{ TLyseeEnumSetType }

function TLyseeEnumSetType.GetDefValue: TLyseeEnumSet;
begin
  if FDefValue = nil then
  begin
    FDefValue := TLyseeEnumSet.Create;
    FDefValue.IncRefcount;
    FDefValue.FParent := Self;
  end;
  Result := FDefValue;
end;

function TLyseeEnumSetType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeEnumSet(Obj).IncRefcount else
    Result := 0;
end;

function TLyseeEnumSetType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeEnumSet(Obj).DecRefcount else
    Result := 0;
end;

function TLyseeEnumSetType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyseeEnumSet(Obj).AsString else
    Result := '[]';
end;

function TLyseeEnumSetType.AsBoolean(Obj: pointer): boolean;
begin
  Result := (Obj <> nil) and TLyseeEnumSet(Obj).Asboolean;
end;

procedure TLyseeEnumSetType.Convert(Value: TLyseeValue);
var
  S: TLyseeEnumSet;
  L: TLyseeList;
  I: integer;
  V: TLyseeValue;
begin
  if Value.FType <> Self then
    if Value.FType = my_array then
    begin
      S := NewEnumSet;
      try
        L := Value.AsArray;
        if L <> nil then
          for I := 0 to L.Count - 1 do
          begin
            V := L[I];
            if V.FType = FSource then
              S.FSets[V.AsEnum.FValue] := true else
            if V.FType = my_int then
              S.FSets[FSource.ItemByValue(V.FValue.VInteger).FValue] := true else
              ErrorConvert(V.FType, FSource);
          end;
        Value.SetObject(Self, S);
      except
        S.Free;
        raise;
      end;
    end;
end;

procedure TLyseeEnumSetType.SetDefault(Value: TLyseeValue);
begin
  DefValue.SetValue(Value);
end;

destructor TLyseeEnumSetType.Destroy;
begin
  FreeAndNil(FDefValue);
  inherited;
end;

procedure TLyseeEnumSetType.SetValue(Value: TLyseeValue; ASet: TLyseeEnumSet);
begin
  if ASet = nil then
    Value.SetObject(Self, DefValue) else
    Value.SetObject(Self, ASet);
end;

function TLyseeEnumSetType.NewEnumSet: TLyseeEnumSet;
var
  I: integer;
begin
  Result := TLyseeEnumSet.Create;
  Result.FParent := Self;
  SetLength(Result.FSets, FSource.Count);
  for I := 0 to FSource.Count - 1 do
    Result.FSets[I] := false;
end;

{ TLyseeValue }

function TLyseeValue.IncRefcount: integer;
begin
  if FType.IsBasicValue then Result := 1 else
  if FValue.VObject = nil then Result := 0 else
    Result := FType.IncRefcount(FValue.VObject);
end;

function TLyseeValue.IsBasicValue: boolean;
begin
  Result := FType.IsBasicValue;
end;

function TLyseeValue.IsDefv: boolean;
begin
  case FType.FTID of
    TID_NIL    : Result := true;
    TID_STRING : Result := (FValue.VObject = nil);
    TID_CHAR   : Result := (FValue.VChar[0] = #0);
    TID_INTEGER    : Result := (FValue.VInteger = 0);
    TID_FLOAT  : Result := IsZero(FValue.VFloat);
    TID_CURRENCY  : Result := (FValue.VCurrency = 0);
    TID_TIME   : Result := IsZero(FValue.VTime);
    TID_BOOLEAN   : Result := not FValue.VBoolean;
    else Result := (FValue.VObject = nil) or
      ((FType = my_type) and (TLyseeType(FValue.VObject) = my_nil));
  end;
end;

function TLyseeValue.IsFalse: boolean;
begin
  Result := (FType = my_bool) and not FValue.VBoolean;
end;

function TLyseeValue.IsNil: boolean;
begin
  Result := FType.IsNil;
end;

function TLyseeValue.IsObject: boolean;
begin
  Result := FType.IsObject;
end;

function TLyseeValue.IsBoolTrue: boolean;
begin
  Result := (FType = my_bool) and FValue.VBoolean;
end;

procedure TLyseeValue.MarkForSurvive;
begin
  FType.GcMark(FValue.VObject);
end;

function TLyseeValue.NewList: TLyseeList;
begin
  SetNil;
  Result := TLyseeList.Create;
  Result.IncRefcount;
  FType := my_array;
  FValue.VObject := Result;
end;

function TLyseeValue.Operate(OP: TLyseeOperator; Value: TLyseeValue): boolean;
var
  R: PLyseeOperate;
begin
  R := FType.FindOperate(OP, Value.FType);
  Result := (R <> nil);
  if Result then
    R^.o_operate(Self, Value);
end;

function TLyseeValue.GetFileName: string;
begin
  if FType = my_string then
    Result := SetPD(Trim(GetAsString)) else
    Result := '';
end;

function TLyseeValue.GetFunc: TLyseeFunc;
begin
  if FType = my_func then
    Result := TLyseeFunc(FValue.VObject) else
    Result := nil;
end;

function TLyseeValue.GetHashed: TLyseeHash;
begin
  if FType = my_hash then
    Result := TLyseeHash(FValue.VObject) else
    Result := nil;
end;

function TLyseeValue.GetModule: TLyseeModule;
begin
  if FType = my_module then
    Result := TLyseeModule(FValue.VObject) else
    Result := nil;
end;

procedure TLyseeValue.SetValue(Value: TLyseeValue);
begin
  if Value <> Self then
  begin
    SetNil;
    if Value <> nil then
    begin
      Value.IncRefcount;
      FType := Value.FType;
      FValue := Value.FValue;
    end;
  end;
end;

procedure TLyseeValue.SetParentType(VT: TLyseeType);
begin
  if FType.IsTypeOf(VT) then
    FType := VT else
    ErrorConvert(FType, VT);
end;

function TLyseeValue.Compare(Value: TLyseeValue): TCompare;
var
  R: PLyseeCompare;
begin
  R := FType.FindCompare(Value.FType);
  if R <> nil then
    Result := R^.c_compare(Self, Value) else
    Result := crDiff;
end;

function TLyseeValue.Compare(Value: TLyseeValue; Wanted: TCompares): boolean;
begin
  Result := (Compare(Value) in Wanted);
end;

procedure TLyseeValue.Convert(AType: TLyseeType; Cntx: TLysee);
begin
  AType.Convert(Self);
end;

constructor TLyseeValue.Create;
begin
  FType := my_nil;
end;

destructor TLyseeValue.Destroy;
begin
  if Self <> nil then
  begin
    SetNil;
    inherited;
  end;
end;

function TLyseeValue.GetAsBoolean: boolean;
begin
  Result := FType.AsBoolean(GetOA);
end;

function TLyseeValue.GetArray: TLyseeList;
begin
  if FType = my_array then
    Result := TLyseeList(FValue.VObject) else
    Result := nil;
end;

function TLyseeValue.GetAsArray: TLyseeList;
begin
  if FType = my_array then Result := TLyseeList(FValue.VObject) else
  begin
    Result := nil;
    ErrorConvert(FType, my_array);
  end;
end;

function TLyseeValue.GetAsChar: char;
begin
  Result := FType.AsChar(GetOA);
end;

function TLyseeValue.GetAsModule: TLyseeModule;
begin
  if FType = my_module then Result := TLyseeModule(FValue.VObject) else
  begin
    Result := FType.FModule;
    if FValue.VObject <> nil then
      if FType = my_func then
        Result := TLyseeFunc(FValue.VObject).FModule else
      if FType = my_type then
        Result := TLyseeType(FValue.VObject).FModule;
  end;
end;

function TLyseeValue.GetAsCurrency: currency;
begin
  Result := FType.AsCurrency(GetOA);
end;

function TLyseeValue.GetOA(Wanted: TLyseeType): pointer;
begin
  Result := nil;
  if (Wanted = nil) or (Wanted = FType) or (Wanted = my_variant) then
    case FType.FTID of
      TID_NIL     : Result := nil;
      TID_CHAR    : Result := @FValue.VChar[0];
      TID_INTEGER : Result := @FValue.VInteger;
      TID_FLOAT   : Result := @FValue.VFloat;
      TID_CURRENCY: Result := @FValue.VCurrency;
      TID_TIME    : Result := @FValue.VTime;
      TID_BOOLEAN : Result := @FValue.VBoolean;
      else Result := FValue.VObject;
    end;
end;

function TLyseeValue.GetAsFloat: double;
begin
  Result := FType.AsFloat(GetOA);
end;

function TLyseeValue.GetAsFunc: TLyseeFunc;
begin
  if FType = my_func then Result := TLyseeFunc(FValue.VObject) else
  begin
    Result := nil;
    ErrorConvert(FType, my_func);
  end;
end;

function TLyseeValue.GetAsHash: TLyseeHash;
begin
  if FType = my_hash then Result := TLyseeHash(FValue.VObject) else
  begin
    Result := nil;
    ErrorConvert(FType, my_hash);
  end;
end;

function TLyseeValue.GetAsInteger: int64;
begin
  Result := FType.AsInteger(GetOA);
end;

function TLyseeValue.GetAsEnumSet: TLyseeEnumSet;
begin
  if FType.FStyle = tsEnumSet then
  begin
    Result := TLyseeEnumSet(FValue.VObject);
    if Result = nil then
      Result := TLyseeEnumSetType(FType).DefValue;
  end
  else Throw('%s is not enum set type', [FType.FullName]);
end;

function TLyseeValue.GetAsEnum: TLyseeEnumItem;
begin
  if FType.IsEnum then
  begin
    Result := TLyseeEnumItem(FValue.VObject);
    if Result = nil then
      Result := TLyseeEnumType(FType).DefValue;
  end
  else Throw('%s is not enum type', [FType.FullName]);
end;

procedure TLyseeValue.SetAsEnumSet(Value: TLyseeEnumSet);
begin
  if Value = nil then
    Throw('invalid enum set: nil') else
    SetTOA(Value.FParent, Value);
end;

function TLyseeValue.GetSelf(var Aobj): boolean;
begin
  Result := FType.IsObject;
  if Result then
  begin
    FType.Validate(FValue.VObject);
    pointer(Aobj) := FValue.VObject;
  end;
end;

function TLyseeValue.GetString: TLyseeString;
begin
  if FType = my_string then
    Result := TLyseeString(FValue.VObject) else
    Result := nil;
end;

function TLyseeValue.GetAsString: string;
begin
  Result := FType.AsString(GetOA);
end;

procedure TLyseeValue.SetAsEnum(Value: TLyseeEnumItem);
begin
  if Value = nil then
    Throw('invalid enum item: nil') else
    Value.SetValue(Self);
end;

function TLyseeValue.GetAsTime: TDateTime;
begin
  Result := FType.AsTime(GetOA);
end;

function TLyseeValue.GetTOA(var OA: pointer): TLyseeType;
begin
  Result := FType;
  OA := GetOA;
end;

function TLyseeValue.GetAsType: TLyseeType;
begin
  if FType = my_type then
  begin
    Result := TLyseeType(FValue.VObject);
    if Result = nil then
      Result := my_nil;
  end
  else Result := FType;
end;

function TLyseeValue.DecRefcount: integer;
begin
  if FType.IsBasicValue then Result := 1 else
  if FValue.VObject = nil then Result := 0 else
  begin
    Result := FType.DecRefcount(FValue.VObject);
    FValue.VObject := nil;
  end;
end;

procedure TLyseeValue.SetAsArray(Value: TLyseeList);
begin
  my_array.IncRefcount(Value);
  my_array.SetDefault(Self);
  FValue.VObject := Value;
end;

procedure TLyseeValue.SetAsBoolean(Value: boolean);
begin
  FType.DecRefcount(FValue.VObject);
  FType := my_bool;
  FValue.VBoolean := Value;
end;

procedure TLyseeValue.SetAsChar(Value: char);
begin
  FType.DecRefcount(FValue.VObject);
  FType := my_char;
  FValue.VChar[0] := Value;
  FValue.VChar[1] := #0;
end;

procedure TLyseeValue.SetAsModule(Value: TLyseeModule);
begin
  my_module.IncRefcount(Value);
  my_module.SetDefault(Self);
  FValue.VObject := Value;
end;

procedure TLyseeValue.SetAsCurrency(Value: currency);
begin
  FType.DecRefcount(FValue.VObject);
  FType := my_curr;
  FValue.VCurrency := Value;
end;

procedure TLyseeValue.SetAsInteger(Value: int64);
begin
  FType.DecRefcount(FValue.VObject);
  FType := my_int;
  FValue.VInteger := Value;
end;

procedure TLyseeValue.SetAsString(const Value: string);
begin
  my_string.SetDefault(Self);
  FValue.VObject := TLyseeString.Create(Value);
  TLyseeString(FValue.VObject).IncRefcount;
end;

procedure TLyseeValue.SetAsTime(Value: TDateTime);
begin
  FType.DecRefcount(FValue.VObject);
  FType := my_time;
  FValue.VTime := Value;
end;

procedure TLyseeValue.SetAsType(Value: TLyseeType);
begin
  if FType <> my_type then
    my_type.SetDefault(Self);
  if Value = nil then
    FValue.VObject := my_nil else
    FValue.VObject := Value;
end;

function TLyseeValue.Same(Value: TLyseeValue): boolean;
begin
  Result := (FType = Value.FType) and (Compare(Value) = crEqual);
end;

procedure TLyseeValue.SetDefault(AType: TLyseeType);
begin
  AType.SetDefault(Self);
end;

procedure TLyseeValue.SetFind(Finded: PLyseeFind);
begin
  case Finded^.f_find of
    fiFunc  : SetAsFunc(Finded^.VFunc);
    fiType  : SetAsType(Finded^.VType);
    fiModule: SetAsModule(Finded^.VModule);
    fiValue : SetValue(Finded^.VValue);
    else SetNil;
  end;
end;

procedure TLyseeValue.SetAsFloat(Value: double);
begin
  FType.DecRefcount(FValue.VObject);
  FType := my_float;
  FValue.VFloat := Value;
end;

procedure TLyseeValue.SetAsFunc(Value: TLyseeFunc);
begin
  my_func.IncRefcount(Value);
  my_func.SetDefault(Self);
  FValue.VObject := Value;
end;

procedure TLyseeValue.SetAsHash(Value: TLyseeHash);
begin
  my_hash.IncRefcount(Value);
  my_hash.SetDefault(Self);
  FValue.VObject := Value;
end;

procedure TLyseeValue.SetNil;
begin
  if FValue.VObject <> nil then
  begin
    FType.DecRefcount(FValue.VObject);
    FValue.VObject := nil;
  end;
  FType := my_nil;
end;

procedure TLyseeValue.SetTOA(T: TLyseeType; OA: pointer);
begin
  if T.IsObject then SetObject(T, OA) else
  case T.FTID of
    TID_CHAR    : SetAsChar(PChar(OA)^);
    TID_INTEGER : SetAsInteger(PInt64(OA)^);
    TID_FLOAT   : SetAsFloat(PDouble(OA)^);
    TID_CURRENCY: SetAsCurrency(PCurrency(OA)^);
    TID_TIME    : SetAsTime(PDateTime(OA)^);
    TID_BOOLEAN : SetAsBoolean(PBoolean(OA)^);
    else SetNil;
  end;
end;

procedure TLyseeValue.SetObject(AType: TLyseeType; Aobj: pointer);
begin
  AType.IncRefcount(Aobj);
  FType.DecRefcount(FValue.VObject);
  FType := AType;
  FValue.VObject := Aobj;
end;

{ TLyseeParam }

procedure TLyseeParam.Error(const Msg: string);
var
  F: TLyseeFunc;
  M: TLyseeModule;
  S: string;
begin
  if not FLysee.FExcepted then
  begin
    F := FFunc;
    if Msg = '' then
      S := F.Name + '() - ' + ExceptionStr else
      S := F.Name + '() - ' + Msg;
    if Assigned(F.FProc) then
    begin
      M := FLysee.CodeModule;
      FLysee.FError.Runtime(S, M.Name, M.FileName, FToken.FRow, FToken.FCol);
    end
    else FToken.Error(Self, S);
  end;
end;

procedure TLyseeParam.BeginExec(var Mark: integer; var Tmpv: TLyseeValue);
begin
  Mark := FParams.Count;
  Tmpv := FParams.Add;
end;

procedure TLyseeParam.BeginExec(var Mark: integer);
begin
  Mark := FParams.Count;
end;

procedure TLyseeParam.EndExec(Mark: integer);
var
  I: integer;
begin
  for I := FParams.Count - 1 downto Mark do
    FParams.Delete(I);
end;

procedure TLyseeParam.Error(const Msg: string; const Args: array of const);
begin
  Error(Format(Msg, Args));
end;

procedure TLyseeParam.ErrorChangeFunc(AFunc: TLyseeFunc);
begin
  Error('can not change function: ' + AFunc.FullName);
end;

procedure TLyseeParam.ErrorOper(OP: TLyseeOperator; L, R: TLyseeType);
begin
  if L = nil then
    Error('unknown operation: %s %s', [OperatorStr(OP), R.FName]) else
  if R = nil then
    Error('unknown operation: %s %s', [OperatorStr(OP), L.FName]) else
    Error('unknown operation: %s %s %s', [L.FName, OperatorStr(OP), R.FName]);
end;

function TLyseeParam.GetChangeAbleFunc(var F: TLyseeFunc): boolean;
begin
  F := GetItem(0).AsFunc;
  my_func.Validate(F);
  Result := (F <> nil) and TestChangeFunc(F);
end;

function TLyseeParam.GetCount: integer;
begin
  if FParams <> nil then
    Result := FParams.Count else
    Result := 0;
end;

function TLyseeParam.GetItem(Index: integer): TLyseeValue;
begin
  Result := FParams[Index];
end;

function TLyseeParam.GetSelf(var Aobj): boolean;
begin
  Result := GetItem(0).GetSelf(Aobj);
end;

function TLyseeParam.GetValue(const Name: string): TLyseeValue;
var
  I: integer;
begin
  I := FFunc.FParams.IndexOf(Name);
  if I >= 0 then
    Result := GetItem(I) else
    Result := nil;
end;

function TLyseeParam.GetVarbValue(Index: integer; var VT: TLyseeType): TLyseeValue;
var
  T: TLyseeToken;
begin
  Result := nil;
  VT := nil;
  if FToken.FSym = syCall then
  begin
    if (FToken.FLeft <> nil) and (FToken.FName <> '') and
      not FFunc.IsConstructor then
        Dec(Index);
    if (Index >= 0) and (Index < FPrmc) and (Index < FToken.GetParamCount) then
    begin
      T := TLyseeToken(FToken.FParams[Index]);
      if T.FSym = syID then
      begin
        Index := FPrev.Func.FParams.IndexOf(T.FName);
        if Index >= 0 then
        begin
          Result := FPrev.FParams[Index];
          VT := FPrev.Func.FParams[Index].FType;
        end;
      end
      else
      if T.FSym = syResult then
      begin
        Result := FPrev.FResult;
        VT := FPrev.Func.FResultType;
      end;
    end;
  end;
end;

procedure TLyseeParam.SetResult(Value: TLyseeValue);
begin
  FResult.SetValue(Value);
  FFunc.FResultType.Convert(FResult);
end;

procedure TLyseeParam.SetArgs(Args: TLyseeList);
var
  P: integer;
begin
  FParams := Args;
  FPrmc := FParams.Count;
  P := FFunc.FParams.ParamCount;
  if FPrmc > P then
  begin
    FVarArgs := FParams.CopyRight(FPrmc - P);
    FVarArgs.FReadonly := true;
    FVarArgs.IncRefcount;
    FParams.SetCount(P);
    FPrmc := P;
  end;
  FParams.PrepareFor(Func);
end;

destructor TLyseeParam.Destroy;
begin
  if FVarArgs <> nil then
    FVarArgs.DecRefcount;
  inherited;
end;

function TLyseeParam.TestChangeFunc(AFunc: TLyseeFunc): boolean;
begin
  Result := AFunc.ChangeAble;
  if not Result then
    ErrorChangeFunc(AFunc);
end;

{ TLyseeToken }

procedure TLyseeToken.AddParam(AParam: TLyseeToken);
begin
  if FParams = nil then
    FParams := TList.Create;
  FParams.Add(AParam);
end;

procedure TLyseeToken.Assign(Source: TLyseeToken);
begin
  FSym := Source.FSym;
  FRow := Source.FRow;
  FCol := Source.FCol;
  FName := Source.FName;
  FValue := Source.FValue;
end;

procedure TLyseeToken.Clear;
begin
  if FSym = syVert then
    if FValue.VFunc <> nil then
    begin
      FValue.VFunc.DecRefcount;
      FValue.VFunc := nil;
    end;
  FSym := syError;
  FRow := 0;
  FCol := 0;
  FName := '';
  FreeAndNil(FLeft);
  FreeAndNil(FRight);
  ClearParams;
end;

procedure TLyseeToken.ClearParams;
var
  I: integer;
  P: TLyseeToken;
begin
  for I := GetParamCount - 1 downto 0 do
  begin
    P := GetParam(I);
    FParams.Delete(I);
    P.Free;
  end;
  FreeAndNil(FParams);
end;

constructor TLyseeToken.Create(T: TLyseeToken);
begin
  if T <> nil then Read(T);
end;

constructor TLyseeToken.CreateWithLeft(LeftBranch: TLyseeToken; T: TLyseeToken);
begin
  Create(T);
  FLeft := LeftBranch;
end;

function TLyseeToken.Decompile(Level: integer): string;

  function decompile_params: string;
  var
    I: integer;
  begin
    if GetParamCount > 0 then
    begin
      Result := GetParam(0).Decompile;
      for I := 1 to GetParamCount - 1 do
        Result :=  Result + ', ' + GetParam(I).Decompile;
    end
    else Result := '';
  end;

  function decompile_call: string;
  begin
    if FLeft = nil then Result := FName else
    begin
      Result := FLeft.Decompile(Level + 1);
      if FName <> '' then
        Result := Result + '.' + FName;
    end;
    Result := Result + '(' + decompile_params + ')';
  end;

  function decompile_get: string;
  begin
    Result := FLeft.Decompile(Level + 1);
    if FName <> '' then
      Result := Result + '.' + FName;
    if GetParamCount > 0 then
      Result := Result + '[' + decompile_params + ']';
  end;

  function decompile_at: string;
  begin
    if FLeft <> nil then
      Result := '@' + FLeft.Decompile(Level + 1) + '.' + FName else
      Result := '@' + FName;
  end;

  function decompile_array: string;
  begin
    if GetParamCount > 0 then
      Result := '[' + decompile_params + ']' else
      Result := '[,]';
  end;

  function decompile_hash: string;
  var
    I: integer;
  begin
    if GetParamCount > 0 then
    begin
      Result := '[' + GetParam(0).Decompile + ':' + GetParam(1).Decompile;
      I := 2;
      while I < GetParamCount - 1 do
      begin
        Result := Result + ', ' + GetParam(I).Decompile + ':' + GetParam(I + 1).Decompile;
        Inc(I, 2);
      end;
      Result := Result + ']';
    end
    else Result := '[:]';
  end;

  function decompile_lambda: string;
  var
    F: TLyseeFunc;
    I: integer;
  begin
    F := FValue.VFunc;
    if F.FParams.ParamCount > 0 then
    begin
      Result := '|' + F.FParams[0].Prototype;
      for I := 1 to F.FParams.ParamCount - 1 do
        Result := Result + ', ' + F.FParams[I].Prototype;
    end
    else Result := '|';
    Result := Result + '| ' + F.FSTMTs[0].FExpr.Decompile;
  end;

begin
  case FSym of
    syID    : Result := FName;
    syResult: Result := 'Result';
    syCall  : Result := decompile_call;
    syGet   : Result := decompile_get;
    sySet   : Result := decompile_get + ' := ' + FRight.Decompile;
    syInt   : Result := IntToStr(FValue.VInt);
    syStr   : Result := FormatString(FName);
    syChar  : Result := FormatChar(FValue.VChar);
    syFloat : Result := FloatToStr(FValue.VFloat);
    syVert  : Result := decompile_lambda;
    syNot   : Result := 'not ' + FRight.Decompile;
    syNeg   : Result := '- ' + FRight.Decompile;
    syArray : Result := decompile_array;
    syHash  : Result := decompile_hash;
    syAt    : Result := decompile_at;
    else
    if FSym in [syNil, syTrue, syFalse, syVArgs] then
      Result := Symbols[FSym].ID else
    if FSym in OperSyms then
    begin
      Result := FLeft.Decompile(Level + 1) + ' ' +
                Symbols[FSym].ID + ' ' +
                FRight.Decompile(Level + 1);
      if Level > 0 then
        Result := '(' + Result + ')';
    end
    else Result := '';
  end;
end;

destructor TLyseeToken.Destroy;
begin
  if Self <> nil then
  begin
    Clear;
    inherited;
  end;
end;

procedure TLyseeToken.Error(Param: TLyseeParam; const Msg: string; const Args: array of const);
begin
  Error(Param, Format(Msg, Args));
end;

procedure TLyseeToken.FailGet(Param: TLyseeParam; const ID: string);
begin
  Error(Param, 'failed getting: ' + ID);
end;

procedure TLyseeToken.ExecCall(Param: TLyseeParam; Outv: TLyseeValue);
var
  tmpv: TLyseeValue;
  mark: integer;

  procedure exec_call;
  var
    T: TLyseeType;
  begin
    if tmpv.FType = my_func then
    begin
      if not TryFunc(TLyseeFunc(tmpv.FValue.VObject), Param, Outv) then
        Error(Param, 'got no function to call');
    end
    else
    if (tmpv.FType = my_type) and (GetParamCount = 1) then
    begin
      T := TLyseeType(tmpv.FValue.VObject);
      if T = nil then T := my_nil;
      if GetParam(0).Execute(Param, Outv) then
        T.Convert(Outv);
    end
    else Error(Param, 'invalid calling: %s()', [tmpv.FType.FullName]);
  end;

  procedure exec_ID;
  begin
    if (Param.FFunc.FindSave(FName, Param, tmpv) <> fiNone)
      or Param.FLysee.Resolve(FName, tmpv) then
        exec_call else FailGet(Param, FName);
  end;

  procedure exec_method;
  var
    M: TLyseeModule;
    T: TLyseeType;
  begin
    if not TryMethod(tmpv, FName, Param, Outv, nil) then
    if (tmpv.FType = my_module) and (tmpv.FValue.VObject <> nil) then
    begin
      M := TLyseeModule(tmpv.FValue.VObject);
      if M.FindSave(FName, tmpv) then
        exec_call else
        FailGet(Param, M.FName, FName);
    end
    else
    if (tmpv.FType = my_type) and (tmpv.FValue.VObject <> nil) then
    begin
      T := TLyseeType(tmpv.FValue.VObject);
      if not TryFunc(T.FindMethod(FName), Param, Outv) then
        FailGet(Param, T.FName, FName);
    end
    else FailGet(Param, tmpv.FType.FName, FName);
  end;

begin
  Param.BeginExec(mark, tmpv);
  try
    if FLeft = nil then exec_ID else    // ID(...)
    if FLeft.Execute(Param, tmpv) then
      if FName = '' then exec_call else // (expression)(...)
        exec_method;                    // (expression).method(...)
  finally
    Param.EndExec(mark);
  end;
end;

function TLyseeToken.TryFunc(Func: TLyseeFunc; Param: TLyseeParam; Outv: TLyseeValue): boolean;
var
  A: TLyseeList;
begin
  Result := (Func <> nil);
  if Result then
  begin
    A := SetupParamList(Param, nil, Func);
    if A <> nil then
    try
      ExecFunc(func, Param, Outv, A);
    finally
      A.Free;
    end;
  end;
end;

function TLyseeToken.ExecFunc(Func: TLyseeFunc; Param: TLyseeParam; Outv: TLyseeValue; Args: TLyseeList): boolean;

  function check_arguments: boolean;
  var
    X: integer;
  begin
    Result := false;
    if Func <> nil then
    begin
      if Args <> nil then
        X := Func.MinArgs - Args.Count else
        X := Func.MinArgs;
      Result := (X < 1);
      if not Result then
        Error(Param, '%s() needs at least %d arguments, %d passed',
          [Func.FullName, Func.MinArgs, Func.MinArgs - X]);
    end
    else Error(Param, 'function is nil');
  end;

var
  curr: TLyseeParam;
  mark: integer;
begin
  Result := false;
  if func = nil then Error(Param, 'invalid function: nil') else
  begin
    Func.IncRefcount;
    try
      if check_arguments then
      begin
        Param.BeginExec(mark);
        try
          if Outv = nil then Outv := Param.FParams.Add;
          if Args = nil then Args := Param.FParams.AddList;
          curr := TLyseeParam.Create;
          try
            curr.FPrev := Param.FLysee.FCurrent;
            curr.FLysee := Param.FLysee;
            curr.FFunc := Func;
            curr.FResult := Outv;
            curr.FToken := Self;
            curr.SetArgs(Args);
            Param.FLysee.FCurrent := curr;
            Func.FResultType.SetDefault(Outv);
            if Assigned(Func.FProc) then
              Func.FProc(curr) else
              Func.FSTMTs.Execute(curr);
            Func.FResultType.Convert(Outv);
          finally
            Param.FLysee.FCurrent := curr.FPrev;
            if Param.FLysee.FState = csExit then
              if not Assigned(Func.FProc) then
                Param.FLysee.FState := csRunning;
            curr.Free;
          end;
          Result := Param.FLysee.StatusOK;
        finally
          Param.EndExec(mark);
        end;
      end;
    finally
      Func.DecRefcount;
    end;
  end;
end;

procedure TLyseeToken.ExecGet(Param: TLyseeParam; Outv: TLyseeValue);
var
  tmpv: TLyseeValue;
  mark: integer;

  // (expression)[...] ==> (expression).get[](...)
  procedure exec_get_item;
  begin
    if not TryMethod(tmpv, 'get[]', Param, Outv, nil) then
      FailGet(Param, tmpv.FType.FName, 'get[]');
  end;

  // (expression).ID[...] ==> (expression).get_ID[](...)
  procedure exec_get_prop_item;
  begin
    if not TryMethod(tmpv, 'get_' + FName + '[]', Param, Outv, nil) then
      if GetProp(Param, tmpv, Outv) then
      begin
        tmpv.SetValue(Outv);
        exec_get_item;
      end;
  end;

begin
  Param.BeginExec(mark, tmpv);
  try
    if FLeft.Execute(Param, tmpv) then
      if FName = '' then
        exec_get_item else                  // (expression)[...]
      if GetParamCount = 0 then
        GetProp(Param, tmpv, Outv) else // (expression).ID
        exec_get_prop_item;                 // (expression).ID[...]
  finally
    Param.EndExec(mark);
  end;
end;

function TLyseeToken.GetProp(Param: TLyseeParam; Host, Outv: TLyseeValue): boolean;
var
  F: TLyseeFunc;
  A: TLyseeList;
begin
  Result := GetAt(Param, Host, Outv);
  if Result then
  begin
    F := Outv.GetFunc;
    if F <> nil then
    begin
      A := TLyseeList.Create;
      try
        if F.FParent <> nil then
          if not F.IsConstructor then
            if Host.FType.IsTypeOf(F.FParent) then
              A.Add(Host);
        Result := ExecFunc(F, Param, Outv, A);
      finally
        A.Free;
      end;
    end;
  end;
end;

procedure TLyseeToken.ExecID(Param: TLyseeParam; Outv: TLyseeValue);
var
  R: RLyseeFind;
  I: integer;
  F: TLyseeFunc;
begin
  if Param.FFunc.FindBy(FName, @R) then
  begin
    if R.f_find = fiVarb then
    begin
      I := Param.FFunc.FParams.IndexOf(R.VVarb);
      Outv.SetValue(Param.FParams[I]);
    end
    else
    if R.f_find = fiFunc then
      ExecFunc(R.VFunc, Param, Outv, nil) else
    if R.f_find = fiValue then
    begin
      F := R.VValue.GetFunc;
      if F <> nil then
        ExecFunc(F, Param, Outv, nil) else
        Outv.SetValue(R.VValue);
    end
    else Outv.SetFind(@R);
  end
  else
  if not Param.FLysee.Resolve(FName, Outv) then
    FailGet(Param, FName);
end;

procedure TLyseeToken.ExecSet(Param: TLyseeParam);
var
  tmpv: TLyseeValue;
  mark: integer;

  // (expression)[...] ==> (expression).set[](..., value)
  procedure exec_set_item;
  begin
    if not TryMethod(tmpv, 'set[]', Param, nil, FRight) then
      FailGet(Param, tmpv.FType.FName, 'set[]');
  end;

  // (expression).ID ==> (expression).set_ID(value)
  procedure exec_set_prop;
  begin
    if not TryMethod(tmpv, 'set_' + FName, Param, nil, FRight) then
      FailGet(Param, tmpv.FType.FName, 'set_' + FName);
  end;

  // (expression).ID[...] ==> (expression).set_ID[](..., value)
  procedure exec_set_prop_item;
  var
    data: TLyseeValue;
  begin
    if not TryMethod(tmpv, 'set_' + FName + '[]', Param, nil, FRight) then
    begin
      data := Param.FParams.Add;
      if GetProp(Param, tmpv, data) then
      begin
        tmpv.SetValue(data);
        exec_set_item;
      end;
    end;
  end;

begin
  Param.BeginExec(mark, tmpv);
  try
    if FLeft.Execute(Param, tmpv) then
      if FName = '' then
        exec_set_item else      // (expression)[...]
      if GetParamCount = 0 then
        exec_set_prop else      // (expression).ID
        exec_set_prop_item;     // (expression).ID[...]
  finally
    Param.EndExec(mark);
  end;
end;

function TLyseeToken.Execute(Param: TLyseeParam; Outv: TLyseeValue): boolean;
var
  tmpv: TLyseeValue;
  mark: integer;

  procedure exec_array;
  var
    A: TLyseeList;
    I, N: integer;
  begin
    A := TLyseeList.Create;
    A.FReadonly := true;
    Outv.SetAsArray(A);
    N := GetParamCount;
    I := 0;
    while (I < N) and GetParam(I).Execute(Param, tmpv) do
    begin
      A.Add(tmpv);
      Inc(I);
    end;
  end;

  procedure exec_hash;
  var
    hash: TLyseeHash;
    I, N: integer;
    key: string;
  begin
    hash := TLyseeHash.Create;
    Outv.SetAsHash(hash);
    N := GetParamCount;
    I := 0;
    while I < N do
    begin
      if GetParam(I).Execute(Param, tmpv) then
      begin
        key := tmpv.AsString;
        Inc(I);
        if GetParam(I).Execute(Param, tmpv) then
        begin
          hash.Add(key).SetValue(tmpv);
          Inc(I);
        end
        else Break;
      end
      else Break;
    end;
  end;

  procedure exec_at;
  begin
    if FLeft <> nil then
    begin
      if FLeft.Execute(Param, tmpv) then
        GetAt(Param, tmpv, Outv);
    end
    else
    if Param.FFunc.FindSave(FName, Param, Outv) = fiNone then
      if not Param.FLysee.Resolve(FName, Outv) then
        FailGet(Param, FName);
  end;

  function GetLRV(L, R: TLyseeValue): boolean;
  begin
    Result := FLeft.Execute(Param, L) and FRight.Execute(Param, R);
  end;

  procedure exec_oper(OP: TLyseeOperator);
  begin
    if OP in [opNot, opNeg] then
    begin
      if FRight.Execute(Param, Outv) then
        if not Outv.Operate(OP, Outv) then
          Param.ErrorOper(OP, Outv.FType, nil);
    end
    else
    if GetLRV(Outv, tmpv) then
      if not Outv.Operate(OP, tmpv) then
        Param.ErrorOper(OP, Outv.FType, tmpv.FType);
  end;

  procedure exec_and;
  begin
    if FLeft.Execute(Param, Outv) then
      if Outv.FType = my_int then
      begin
        if FRight.Execute(Param, tmpv) then
          if tmpv.FType = my_int then
            Outv.FValue.VInteger := Outv.FValue.VInteger and tmpv.FValue.VInteger else
            Outv.SetAsBoolean((Outv.FValue.VInteger <> 0) and tmpv.AsBoolean);
      end
      else Outv.SetAsBoolean(Outv.AsBoolean and
        (FRight.Execute(Param, Outv) and Outv.AsBoolean));
  end;

  procedure exec_or;
  begin
    if FLeft.Execute(Param, Outv) then
      if Outv.FType = my_int then
      begin
        if FRight.Execute(Param, tmpv) then
          if tmpv.FType = my_int then
            Outv.FValue.VInteger := Outv.FValue.VInteger or tmpv.FValue.VInteger else
            Outv.SetAsBoolean((Outv.FValue.VInteger <> 0) or tmpv.AsBoolean);
      end
      else Outv.SetAsBoolean(Outv.AsBoolean or
        (FRight.Execute(Param, Outv) and Outv.AsBoolean));
  end;

  procedure exec_comp(Wanted: TCompares);
  begin
    Outv.SetAsBoolean(GetLRV(Outv, tmpv) and Outv.Compare(tmpv, Wanted));
  end;

  procedure exec_same;
  begin
    Outv.SetAsBoolean(GetLRV(Outv, tmpv) and Outv.Same(tmpv));
  end;

  procedure exec_vargs;
  begin
    Outv.AsArray := Param.FVarArgs;
  end;

begin
  if Self <> nil then
  try
    if FSym = syID then ExecID(Param, Outv) else
    if FSym = syCall then ExecCall(Param, Outv) else
    if FSym = syGet then ExecGet(Param, Outv) else
    if FSym = sySet then ExecSet(Param) else
    if FSym = syNil then Outv.SetNil else
    if FSym = syTrue then Outv.SetAsBoolean(true) else
    if FSym = syFalse then Outv.SetAsBoolean(false) else
    if FSym = syInt then Outv.SetAsInteger(FValue.VInt) else
    if FSym = syStr then Outv.SetAsString(FName) else
    if FSym = syChar then Outv.SetAsChar(FValue.VChar) else
    if FSym = syFloat then Outv.SetAsFloat(FValue.VFloat) else
    if FSym = syVert then Outv.SetAsFunc(FValue.VFunc) else
    if FSym = syResult then Outv.SetValue(Param.FResult) else
    begin
      Param.BeginExec(mark, tmpv);
      try
        case FSym of
        { operator: 0}
          syMul   : exec_oper(opMul);
          syDiv   : exec_oper(opDiv);
          syDivf  : exec_oper(opDivf);
          syMod   : exec_oper(opMod);
        { operator: 1}
          syAdd   : exec_oper(opAdd);
          syReduce: exec_oper(opDec);
        { operator: 2}
          syXor   : exec_oper(opXor);
          syShr   : exec_oper(opShr);
          syShl   : exec_oper(opShl);
          syShi   : exec_oper(opShi);
          syFill  : exec_oper(opFill);
        { operator: 3}
          sySame  : exec_same;
          syEQ    : exec_comp([crEqual]);
          syNE    : exec_comp([crLess, crMore, crDiff]);
          syLT    : exec_comp([crLess]);
          syLE    : exec_comp([crLess, crEqual]);
          syMT    : exec_comp([crMore]);
          syME    : exec_comp([crMore, crEqual]);
          syIn    : exec_oper(opIn);
          syLike  : exec_oper(opLike);
          syIs    : exec_oper(opIs);
          syAs    : exec_oper(opAs);
        { operator: 4}
          syAnd   : exec_and;
          syOr    : exec_or;
        { operator: single }
          syNot   : exec_oper(opNot);
          syNeg   : exec_oper(opNeg);
        { complexed }
          syArray : exec_array;
          syHash  : exec_hash;
          syAt    : exec_at;
          syVArgs : exec_vargs;
        end;
      finally
        Param.EndExec(mark);
      end;
    end;
  except
    Error(Param, '');
  end;
  Result := Param.FLysee.StatusOK;
end;

procedure TLyseeToken.FailGet(Param: TLyseeParam; const Host, Prop: string);
begin
  FailGet(Param, Host + '.' + Prop);
end;

function TLyseeToken.GetAt(Param: TLyseeParam; Host, Outv: TLyseeValue): boolean;
var
  F: TLyseeFunc;
begin
  Result := false;
  F := Host.FType.FindMethod(FName);
  if F <> nil then
  begin
    Outv.SetAsFunc(F);
    Result := true;
  end
  else
  if Host.FValue.VObject <> nil then
    if Host.FType = my_module then
    begin
      Result := TLyseeModule(Host.FValue.VObject).FindSave(FName, Outv);
      if not Result then
        FailGet(Param, TLyseeModule(Host.FValue.VObject).FName, FName);
    end
    else
    if Host.FType = my_type then
    begin
      F := TLyseeType(Host.FValue.VObject).FindMethod(FName);
      Result := (F <> nil);
      if Result then
        Outv.SetAsFunc(F) else
        FailGet(Param, TLyseeType(Host.FValue.VObject).FName, FName);
    end;
  if not Result then
    if Param.FLysee.StatusOK then
      FailGet(Param, Host.FType.FName, FName);
end;

function TLyseeToken.GetParam(Index: integer): TLyseeToken;
begin
  Result := TLyseeToken(FParams[Index]);
end;

function TLyseeToken.GetParamCount: integer;
begin
  if FParams <> nil then
    Result := FParams.Count else
    Result := 0;
end;

procedure TLyseeToken.Error(Param: TLyseeParam; const Msg: string);
var
  M: TLyseeModule;
  S: string;
begin
  if not Param.FLysee.FExcepted then
  begin
    if Msg = '' then
      S := Param.FFunc.FName + '() - ' + ExceptionStr else
      S := Param.FFunc.FName + '() - ' + Msg;
    M := Param.FFunc.FModule;
    Param.FLysee.FError.Runtime(S, M.Name, M.FFileName, FRow, FCol);
  end;
end;

procedure TLyseeToken.Read(Source: TLyseeToken);
begin
  FSym := Source.FSym;
  case FSym of
    syChar : FValue.VChar := Source.FValue.VChar;
    syInt  : FValue.VInt := Source.FValue.VInt;
    syFloat: FValue.VFloat := Source.FValue.VFloat;
  end;
  if FSym in NameSyms then
    FName := Source.FName else
    FName := '';
  FRow := Source.FRow;
  FCol := Source.FCol;
end;

function TLyseeToken.SetupParamList(Param: TLyseeParam; Host: TLyseeValue; Func: TLyseeFunc): TLyseeList;
var
  I, N: integer;
begin
  Result := TLyseeList.Create;
  if Host <> nil then
    if func.IsMethod and not func.IsConstructor then
      Result.Add(Host);
  N := GetParamCount;
  for I := 0 to N - 1 do
    if not GetParam(I).Execute(Param, Result.Add) then
    begin
      FreeAndNil(Result);
      Exit;
    end;
end;

function TLyseeToken.TryMethod(Host: TLyseeValue; const Method: string;
  Param: TLyseeParam; Outv: TLyseeValue; LastParam: TLyseeToken): boolean;
var
  func: TLyseeFunc;
  args: TLyseeList;
  tmpv: TLyseeValue;
  mark: integer;

  function check_set_arguments: boolean;
  begin
    Result := (func.FParams.Count = args.Count + 1);
    if not Result then
      Error(Param, '%s() needs %d arguments, %d passed',
        [func.FullName, func.FParams.Count, args.Count + 1]) else
  end;

begin
  func := Host.FType.FindMethod(Method);
  Result := (func <> nil);
  if Result then
  begin
    args := SetupParamList(Param, Host, func);
    if args <> nil then
    try
      Param.BeginExec(mark);
      try
        if LastParam = nil then
          ExecFunc(func, Param, Outv, args) else
        if check_set_arguments then
        begin
          tmpv := Param.FParams.Add;
          if LastParam.Execute(Param, tmpv) then
          begin
            args.Add(tmpv);
            ExecFunc(func, Param, Outv, args);
          end;
        end;
      finally
        Param.EndExec(mark);
      end;
    finally
      args.Free;
    end;
  end;
end;

{ TLyseeTokenList }

function TLyseeTokenList.Add(Pos: TLyseeToken): TLyseeToken;
begin
  Result := TLyseeToken.Create;
  if Pos <> nil then
  begin
    Result.FRow := Pos.FRow;
    Result.FCol := Pos.FCol;
  end;
  FItems.Add(Result);
end;

function TLyseeTokenList.AddToken(Sym: TLyseeSymbol): TLyseeToken;
begin
  Result := AddToken(Sym, nil);
end;

procedure TLyseeTokenList.Clear;
var
  X: integer;
  T: TLyseeToken;
begin
  for X := GetCount - 1 downto 0 do
  begin
    T := TLyseeToken(FItems[X]);
    FItems.Delete(X);
    T.Free;
  end;
end;

procedure TLyseeTokenList.ClearKeepLast;
var
  X: integer;
  L: pointer;
begin
  X := GetCount - 1;
  if X > 0 then
  begin
    L := FItems[X];
    FItems.Delete(X);
    try
      Clear;
    finally
      FItems.Add(L);
    end;
  end;
end;

constructor TLyseeTokenList.Create;
begin
  FItems := TList.Create;
end;

procedure TLyseeTokenList.DeleteLast(N: integer);
var
  X: integer;
  T: TLyseeToken;
begin
  N := GetCount - N;
  for X := GetCount - 1 downto N do
  begin
    T := TLyseeToken(FItems[X]);
    FItems.Delete(X);
    T.Free;
  end;
end;

destructor TLyseeTokenList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TLyseeTokenList.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TLyseeTokenList.GetItem(Index: integer): TLyseeToken;
begin
  Result := TLyseeToken(FItems[Index]);
end;

function TLyseeTokenList.GetLast: TLyseeToken;
begin
  Result := TLyseeToken(FItems.Last);
end;

procedure TLyseeTokenList.Reverse;
var
  L, R: integer;
begin
  L := 0;
  R := FItems.Count - 1;
  while L < R do
  begin
    FItems.Exchange(L, R);
    Inc(L);
    Dec(R);
  end;
end;

function TLyseeTokenList.AddToken(Token: TLyseeToken): TLyseeToken;
begin
  Result := Add;
  Result.Assign(Token);
end;

function TLyseeTokenList.AddToken(Sym: TLyseeSymbol; Pos: TLyseeToken): TLyseeToken;
begin
  Result := Add(Pos);
  Result.FSym := Sym;
end;

{ TLyseeTokenizer }

constructor TLyseeTokenizer.Create(const Script: string);
var
  T: TLyseeToken;
begin
  FCode := Script;
  FSize := Length(FCode);
  FPosition := 1;
  FRow := 0;
  FCol := 0;
  FEOF := false;
  if FPosition <= FSize then
  begin
    FChar := FCode[FPosition];
    if (FChar = '#') and (NextChar = '!') then // #! ... head comment ...
      if GotoChar([#13, #10]) then
      begin
        if FChar = #13 then GetChar;
        if FChar = #10 then GetChar;
      end;
  end
  else FChar := #0;
  FCurrent := nil;
  FTokens := TLyseeTokenList.Create;
  repeat
    T := FTokens.Add(nil);
    GetToken(T);
  until T.FSym in [syEOF, syError];
  if T.FSym = syError then
    FTokens.ClearKeepLast else
    FTokens.Reverse;
  FIndex := FTokens.Count - 1;
end;

destructor TLyseeTokenizer.Destroy;
begin
  FreeAndNil(FTokens);
  inherited;
end;

function TLyseeTokenizer.GetChar: boolean;
var
  F13: boolean;
begin
  F13 := (FChar = #13);
  if F13 or (FChar = #10) then
  begin
    Inc(FRow);
    FCol := 0;
  end
  else Inc(FCol);
  Inc(FPosition);
  Result := (FPosition <= FSize);
  if Result then
  begin
    FChar := FCode[FPosition];
    if F13 and (FChar = #10) then
    begin
      Inc(FPosition);
      Result := (FPosition <= FSize);
      if Result then
        FChar := FCode[FPosition] else
        FChar := #0;
    end;
  end
  else FChar := #0;
end;

function TLyseeTokenizer.GetCurrent: TLyseeToken;
begin
  if FCurrent = nil then GetNext;
  Result := FCurrent;
end;

function TLyseeTokenizer.PackToCurrent: boolean;
begin
  Result := (FCurrent <> nil);
  if Result then
    FTokens.DeleteLast(FTokens.Count - (FIndex + 1));
end;

function TLyseeTokenizer.ParseChar(var C: char): boolean;
var
  V: cardinal;
begin
  Result := (FChar = '#') and GetChar and CharInSet(FChar, CS_DIGIT + ['$']);
  if Result then
    if CharInSet(FChar, CS_DIGIT) then
    begin
      V := Ord(FChar) - Ord('0');
      while GetChar and CharInSet(FChar, CS_DIGIT) do
        V := (V * 10) + Ord(FChar) - Ord('0');
      C := char(V);
    end
    else
    begin
      Result := GetChar and CharInSet(NextChar, CS_HEX);
      if Result then
      begin
        V := HexValue(FChar);
        while GetChar and CharInSet(FChar, CS_HEX) do
          V := (V * 16) + HexValue(FChar);
        C :=  char(V);
      end;
    end;
end;

function TLyseeTokenizer.ParseHex(var I: int64): boolean;
begin
  Result := (FChar = '$') and GetChar and CharInSet(FChar, CS_HEX);
  if Result then
  begin
    I := HexValue(FChar);
    while GetChar and CharInSet(FChar, CS_HEX) do
      I := (I * 16) + HexValue(FChar);
  end;
end;

function TLyseeTokenizer.ParseString(var S: string): boolean;
begin
  S := '';
  Result := (FChar = '''');
  if Result then
  begin
    while GetChar and ((FChar <> '''') or (NextChar = '''')) do
    begin
      S := S + FChar;
      if FChar = '''' then GetChar;
    end;
    Result := (FChar = '''');
    if Result then GetChar;
  end;
end;

function TLyseeTokenizer.GetNext: TLyseeToken;
begin
  if FCurrent <> nil then
    Dec(FIndex) else
    FIndex := FTokens.Count - 1;
  if FIndex >= 0 then
  begin
    FCurrent := FTokens[FIndex];
    Result := FCurrent;
  end
  else Result := nil;
end;

function TLyseeTokenizer.GetToken(token: TLyseeToken): boolean;

  function read_ID(const S: string): string;
  begin
    Result := S;
    while GetChar and CharInSet(FChar, CS_ID) do
      Result := Result + FChar;
  end;

  function is_reserved(const S: string): boolean;
  begin
    Result := (S[1] = '_');
  end;

  procedure parse_identity;
  var
    I: TLyseeSymbol;
  begin
    token.FName := read_ID(FChar);
    token.FSym := syID;
    for I := syBegin to syEnd do
      if MatchID(Symbols[I].ID, token.FName) then
      begin
        token.FSym := I;
        Break;
      end;
  end;

  procedure expand_string;
  var
    S: string;
  begin
    while (token.FSym = syStr) and CharInSet(FChar, ['#', '''']) do
    begin
      case FChar of
        '#': if ParseChar(token.FValue.VChar) then
               token.FName := token.FName + token.FValue.VChar else
               token.FSym := syError;
       '''': if ParseString(S) then
               token.FName := token.FName + S else
               token.FSym := syError;
      end;
    end;
  end;

  procedure parse_string(QuoteChar: char);
  begin
    if ParseString(token.FName) then
    begin
      token.FSym := syStr;
      if CharInSet(FChar, ['#', '''']) then
        expand_string else
      if (QuoteChar = '''') and (Length(token.FName) = 1) then
      begin
        token.FSym := syChar;
        token.FValue.VChar := token.FName[1];
        token.FName := '';
      end;
    end;
  end;

  procedure parse_char;
  begin
    if ParseChar(token.FValue.VChar) then
      if CharInSet(FChar, ['#', '''']) then
      begin
        token.FSym := syStr;
        token.FName := token.FValue.VChar;
        expand_string;
      end
      else token.FSym := syChar;
  end;

  procedure parse_hex;
  begin
    if ParseHex(token.FValue.VInt) then
      token.FSym := syInt;
  end;

  procedure parse_number(Neg: boolean);
  var
    S: string;
  begin
    if Neg then S := '-' else S := '';
    while CharInSet(FChar, CS_DIGIT) do
    begin
      S := S + FChar;
      GetChar;
    end;
    if (FChar = '.') and CharInSet(NextChar, CS_DIGIT) then
    begin
      S := S + '.';
      while GetChar and CharInSet(FChar, CS_DIGIT) do
        S := S + FChar;
      token.FValue.VFloat := StrToFloat(S);
      token.FSym := syFloat;
    end
    else
    begin
      token.FValue.VInt := StrToInt64(S);
      token.FSym := syInt;
    end;
  end;

  procedure parse_operator(DefSymbol: TLyseeSymbol;
    const next: array of char;
    const syms: array of TLyseeSymbol);
  var
    count, index: integer;
  begin
    if GetChar then
    begin
      count := Min(Length(next), Length(syms));
      for index := 0 to count - 1 do
        if FChar = next[index] then
        begin
          token.FSym := syms[index];
          token.FName := Symbols[token.FSym].ID;
          GetChar;
          Exit;
        end;
    end;
    token.FSym := DefSymbol;
    token.FName := Symbols[token.FSym].ID;
  end;

  procedure parse_set(Symbol: TLyseeSymbol);
  begin
    token.FSym := Symbol;
    token.FName := Symbols[token.FSym].ID;
    GetChar;
  end;

begin
  Result := (FChar <> #0) and SkipSpaces;
  token.Clear;
  token.FRow := FRow;
  token.FCol := FCol;
  if Result then
  begin
    if CharInSet(FChar, CS_HEAD) then parse_identity else
    if CharInSet(FChar, CS_DIGIT) then parse_number(false) else
    case FChar of
     '''': parse_string(FChar);
      '#': parse_char;
      '$': parse_hex;
      '+': if CharInSet(NextChar, CS_DIGIT) then
           begin
             GetChar;
             parse_number(false);
           end
           else parse_set(syAdd);
      '-': if CharInSet(NextChar, CS_DIGIT) then
           begin
             GetChar;
             parse_number(true);
           end
           else parse_set(syReduce);
      '*': parse_set(syMul);
      '/': parse_set(syDivf);
      '(': parse_set(syLParen);
      ')': parse_set(syRParen);
      '[': parse_set(syLArray);
      ']': parse_set(syRArray);
      '.': parse_operator(syDot, ['.'], [syRange]);
      ':': parse_operator(syColon, ['='], [syBecome]);
      ';': parse_set(sySemic);
      ',': parse_set(syComma);
      '@': parse_set(syAt);
      '|': parse_set(syVert);
      '=': parse_operator(syEQ, ['='], [sySame]);
      '<': parse_operator(syLT, ['=', '>', '<'], [syLE, syNE, syShi]);
      '>': parse_operator(syMT, ['='], [syME]);
      else token.FName := FChar;
    end;

    if (token.FSym = syShi) and (FChar = '<') then
    begin
      token.FSym := syFill;
      GetChar;
    end;

    if (token.FSym in OperSyms) and (FChar = '=') then
    begin
      Dec(FPosition);
      Dec(FCol);
      FChar := ':'; {<--convert '+=' to '+:=', and so on}
    end;

    if token.FSym <> syError then
      if not (token.FSym in [syID, syStr]) then
        token.FName := '';
  end
  else
  if not FEOF then
  begin
    FEOF := true;
    token.FSym := syEOF;
    Result := true;
  end;
end;

function TLyseeTokenizer.GotoChar(Chars: TSysCharSet): boolean;
begin
  repeat Result := CharInSet(FChar, Chars) until Result or not GetChar;
end;

function TLyseeTokenizer.NextChar: char;
begin
  if FPosition < FSize then
    Result := FCode[FPosition + 1] else
    Result := #0;
end;

function TLyseeTokenizer.PrevChar: char;
begin
  if FPosition > 1 then
    Result := FCode[FPosition - 1] else
    Result := #0;
end;

function TLyseeTokenizer.PeekNextSymbol: TLyseeSymbol;
begin
  if FIndex > 0 then
    Result := FTokens[FIndex - 1].FSym else
    Result := syError;
end;

function TLyseeTokenizer.NextIsBecome(OnHead: boolean): boolean;
var
  S: TLyseeSymbol;
begin
  if OnHead then
  begin
    Result := (FCurrent.FSym = syBecome);
    if not Result and (FCurrent.FSym in OperSyms) then
      Result := (PeekNextSymbol = syBecome);
  end
  else
  begin
    S := PeekNextSymBol;
    Result := (S = syBecome);
    if not Result and (S in OperSyms) then
      Result := (PeekThirdSymbol = syBecome);
  end;
end;

function TLyseeTokenizer.PeekThirdSymbol: TLyseeSymbol;
begin
  if FIndex > 1 then
    Result := FTokens[FIndex - 2].FSym else
    Result := syError;
end;

function TLyseeTokenizer.SkipSpaces: boolean;

  function skip_space: boolean;
  begin
    while FChar <= ' ' do if not GetChar then Break;
    Result := (FChar > ' ');
  end;

begin
  Result := false;
  while not Result and skip_space do
    if (FChar = '/') and (NextChar = '/') then
    begin
      GotoChar([#13, #10]);
      while CharInSet(FChar, [#13, #10]) do GetChar;
    end
    else
    if FChar = '{' then
    begin
      GotoChar(['}']);
      GetChar;
    end
    else
    if (FChar = '(') and (NextChar = '*') then
    begin
      GetChar; // *
      GetChar; // *
      GetChar; // )
      while GotoChar([')']) and (PrevChar <> '*') do GetChar;
    end
    else
    if (FChar = '/') and (NextChar = '*') then
    begin
      GetChar; // *
      GetChar; // *
      GetChar; // )
      while GotoChar(['/']) and (PrevChar <> '*') do GetChar;
    end
    else Result := true;
end;

{ TLyseeParser }

constructor TLyseeParser.Create(AModule: TLyseeModule);
begin
  FModule := AModule;
  FContext := FModule.FContext;
end;

destructor TLyseeParser.Destroy;
begin
  FreeAndNil(FTokenizer);
  inherited;
end;

procedure TLyseeParser.EUnexpected(T: TLyseeToken);
var
  S: string;
begin
  if T = nil then T := FLast;
  S := T.FName;
  if S = '' then
    S := Symbols[T.FSym].ID;
  ESyntax(T.FRow, T.FCol, 'unexpected symbol: %s', [S]);
end;

procedure TLyseeParser.ERedeclared(T: TLyseeToken);
begin
  if T = nil then T := FLast;
  ESyntax(T.FRow, T.FCol, 'object redeclared: %s', [T.FName]);
end;

function TLyseeParser.UseToken(Token: TLyseeToken): TLyseeToken;
var
  I: integer;
  T: TLyseeToken;
begin
  Result := Token;
  if Token <> FTokenizer.FCurrent then
  begin
    for I := FTokenizer.FTokens.Count - 1 downto FTokenizer.FIndex + 1 do
    begin
      T := FTokenizer.FTokens[I];
      if Token = T then
      begin
        FTokenizer.FTokens.FItems[I] := nil;
        Exit;
      end;
    end;
    Result := nil;
  end
  else FTokenizer.FTokens.FItems[FTokenizer.FIndex] := nil;
end;

procedure TLyseeParser.ETypeNotFound(T: TLyseeToken);
begin
  if T = nil then T := FLast;
  ESyntax(FTokenizer.FRow, FTokenizer.FCol, 'type not found: %s', [T.FName]);
end;

procedure TLyseeParser.ESyntax(ERow, ECol: integer;
  const EMsg: string; const EArgs: array of const);
begin
  FContext.FError.Syntax(Format(EMsg, EArgs), FModule.Name,
    FModule.FileName, ERow, ECol);
end;

procedure TLyseeParser.ParseUses;
begin
  FAfter := FLast.FCol;
  repeat
    SymTestNextID;
    if FModule.UseModule(FLast.FName) = nil then
      ESyntax(FLast.FRow, FLast.FCol, 'module not exists: %s', [FLast.FName]);
    SymTestNext([syComma, sySemic]);
  until FLast.FSym = sySemic;
  SymGetNext;
end;

procedure TLyseeParser.ParseConst;
var
  L: TLyseeToken;
  S: TLyseeAssign;
begin
  FAfter := FLast.FCol;
  SymTestNextID;
  repeat
    if FModule.Find(FLast.FName) then ERedeclared;
    L := FLast;
    SymTestNext([syEQ]);
    FFunc := TLyseeFunc.Create('', FModule, nil);
    FModule.FConsts.Add(L.FName).SetAsFunc(FFunc);
    S := FFunc.GetSTMTs.Add(ssConst) as TLyseeAssign;
    S.FVarb := L.FName;
    S.FExpr := ParseExpr(false, [sySemic], true);
    SymGetNext;
  until (FLast.FSym <> syID) or (FLast.FCol <= FAfter);
end;

procedure TLyseeParser.ParseFunc;
begin
  FAfter := FLast.FCol;
  SymTestNext([syID]);
  if FModule.Find(FLast.FName) then ERedeclared;
  FFunc := TLyseeFunc.Create(FLast.FName, FModule, nil);

  if FLast.FSym = syProc then
  begin
    FFunc.FResultType := my_nil;
    SymTestNext([syLParen, sySemic]);
  end
  else SymTestNext([syLParen, sySemic, syColon]);

  if FLast.FSym = syLParen then
  begin
    ParseArguments(syRParen);
    if FFunc.IsFunction then
      SymTestNext([sySemic, syColon]) else
      SymTestNext([sySemic]);
  end;

  if FLast.FSym = syColon then
  begin
    ParseType(false, FFunc.FResultType);
    SymTestNext([sySemic]);
  end;

  ParseBlock([], FFunc.GetSTMTs);
end;

function TLyseeParser.Parse(const Code: string; UsingModule: boolean): TLyseeFunc;
begin
  Result := nil;
  FAfter := -1;
  FreeAndNil(FTokenizer);
  FTokenizer := TLyseeTokenizer.Create(Code);
  try
    SymGetNext;
    if UsingModule then ParseUsesConstFunc else
    begin
      if not FContext.GetRunning then
      begin
        Result := FModule.FContext.GetMainFunc;
        Result.STMTs.Clear;
        ParseUsesConstFunc;
      end
      else Result := TLyseeFunc.Create('', FModule, nil);
      while FLast.FSym <> syEOF do
      begin
        FAfter := -1;
        FFunc := Result;
        ParseStatement(true, FFunc.STMTs);
        FTokenizer.PackToCurrent;
      end;
    end;
  finally
    FreeAndNil(FTokenizer);
  end;
end;

procedure TLyseeParser.SymTestLast(Syms: TLyseeSymbols);
begin
  if (Syms <> []) and not (FLast.FSym in Syms) then EUnexpected;
end;

procedure TLyseeParser.SymTestLastID;
begin
  SymTestLast([syID]);
  if not IsID(FLast.FName) then
    ESyntax(FLast.FRow, FLast.FCol,
      'invalid identity: %s', [FLast.FName]);
end;

procedure TLyseeParser.SymTestNext(Syms: TLyseeSymbols);
begin
  SymGotoNext;
  SymTestLast(Syms);
end;

procedure TLyseeParser.SymTestNextID;
begin
  SymGotoNext;
  SymTestLastID;
end;

procedure TLyseeParser.SymGetNext;
begin
  FLast := FTokenizer.GetNext;
  if FLast = nil then
    ESyntax(FTokenizer.FRow, FTokenizer.FCol,
      'symbol expected but reachs end of file', []);
end;

procedure TLyseeParser.SymGotoNext;
begin
  SymGetNext;
  if FLast.FCol <= FAfter then
    ESyntax(FLast.FRow, FLast.FCol,
      'symbol position(%d) should after %d',
      [FLast.FCol + 1, FAfter + 1]);
end;

procedure TLyseeParser.ParseBlock(EndSyms: TLyseeSymbols; SX: TLyseeSTMTList);
var
  Syms: TLyseeSymbols;
  after: integer;
begin
  after := FAfter;
  try
    Syms := EndSyms + [syEOF, syElse, syElif, syUntil, syExcept, syFinally];
    SymGetNext;
    while (FLast.FCol > FAfter) and not (FLast.FSym in Syms) do
    begin
      FAfter := after;
      ParseStatement(true, SX);
    end;
    if EndSyms <> [] then
      SymTestLast(EndSyms);
  finally
    FAfter := after;
  end;
end;

function TLyseeParser.ParseAndFree(const Code: string; UsingModule: boolean): TLyseeFunc;
begin
  try
    Result := Parse(Code, UsingModule);
  finally
    Free;
  end;
end;

function TLyseeParser.ParseExpr(OnHead: boolean; EndSyms: TLyseeSymbols; DoCheck: boolean): TLyseeToken;
begin
  if not OnHead then SymGotoNext;
  Result := ParseFact(High(OperLevel));
  if DoCheck then
    SymTestLast(EndSyms);
end;

function TLyseeParser.ParseFact(Level: integer): TLyseeToken;
begin
  if Level > 0 then
    Result := ParseFact(Level - 1) else
    Result := ParseTerm;
  while (FLast.FSym in OperLevel[Level]) and (SymPeekNext in ExprHead) do
  begin
    Result := TLyseeToken.CreateWithLeft(Result, FLast);
    SymGotoNext;
    if Level > 0 then
      Result.FRight := ParseFact(Level - 1) else
      Result.FRight := ParseTerm;
  end;
end;

procedure TLyseeParser.ParseStatement(OnHead: boolean; SX: TLyseeSTMTList);
begin
  if not OnHead then SymGotoNext;
  case FLast.FSym of
    syVar   : ParseVar(SX);
    syIf    : ParseIf(SX);
    syFor   : ParseFor(SX);
    syWhile : ParseWhile(SX);
    syRepeat: ParseRepeat(SX);
    syCase  : ParseCase(SX);
    syTry   : ParseTry(SX);
    syRaise : ParseRaise(SX);
    syEQ    : ParsePuts(SX);
    sySemic : SymGotoNext;
    else ParseAny(SX);
  end;
  FTokenizer.PackToCurrent;
end;

procedure TLyseeParser.ParseIf(SX: TLyseeSTMTList);
var
  after: integer;
  S: TLyseeIf;
begin
{ if CONDITION then ......
  elif CONDITION then ......
  else ...... }
  after := FAfter;
  try
    FAfter := FLast.FCol;
    S := SX.Add(ssIf) as TLyseeIf;
    S.FExpr := ParseExpr(false, [syThen], true);
    ParseBlock([], S.GetItems);
    if FLast.FCol = FAfter then
      if FLast.FSym = syElse then
        ParseBlock([], S.GetElseItems) else
      if FLast.FSym = syElif then
        ParseIf(S.GetElseItems);
  finally
    FAfter := after;
  end;
end;

procedure TLyseeParser.ParseUsesConstFunc;
begin
  while FLast.FSym in [syFunc, syProc, syConst, syUses] do
  begin
    FFunc := nil;
    FAfter := -1;
    if FLast.FSym = syFunc then ParseFunc else
    if FLast.FSym = syProc then ParseFunc else
    if FLast.FSym = syConst then ParseConst else
    if FLast.FSym = syUses then ParseUses;
    FTokenizer.PackToCurrent;
  end;
end;

procedure TLyseeParser.ParseVar(SX: TLyseeSTMTList);
var
  after: integer;
  S: TLyseeAssign;
  V: TLyseeVarb;
  T: TLyseeType;
  I: integer;
begin
{ var a, b, c, ...;
  var a, b, c, ...: type;
  var a, b, c, ...= expression;
  var a, b, c, ...: type = expression; }
  after := FAfter;
  try
    FAfter := FLast.FCol;
    SymTestNextID;
    repeat
      if FFunc.FindInside(FLast.FName) then ERedeclared;
      V := FFunc.FParams.AddLocal(FLast.FName, my_variant);
      SymTestNext([syComma, sySemic, syColon, syBecome]);

      while FLast.FSym = syComma do
      begin
        SymTestNextID;
        if FFunc.FindInside(FLast.FName) then ERedeclared;
        FFunc.FParams.AddLocal(FLast.FName, my_variant);
        SymTestNext([syComma, sySemic, syColon]);
      end;

      if FLast.FSym = syColon then
      begin
        ParseType(false, T);
        for I := FFunc.FParams.Count - 1 downto 0 do
        begin
          FFunc.FParams[I].FType := T;
          if FFunc.FParams[I] = V then Break;
        end;
        SymTestNext([sySemic, syBecome]);
      end;

      if FLast.FSym = syBecome then
      begin
        S := SX.Add(ssAssign) as TLyseeAssign;
        S.FVarb := FFunc.FParams[FFunc.FParams.Count - 1].FName;
        S.FExpr := ParseExpr(false, [sySemic], true);
      end;

      SymGetNext;
    until (FLast.FSym <> syID) or (FLast.FCol <= FAfter);
  finally
    FAfter := after;
  end;
end;

procedure TLyseeParser.ParseFor(SX: TLyseeSTMTList);
var
  after: integer;
  S: TLyseeFor;
begin
{ for a in range do ....
  for a := low to high do ....
  for a := high downto low do .... }
  after := FAfter;
  try
    FAfter := FLast.FCol;
    S := SX.Add(ssFor) as TLyseeFor;
    SymTestNextID;
    S.FVarb := FLast.FName;
    FFunc.FParams.AddLocal(FLast.FName, my_variant);
    SymTestNext([syIn, syBecome]);
    if FLast.FSym = syBecome then
    begin
      S.FExpr := ParseExpr(false, [syTo, syDownto], true);
      S.FUpTo := (FLast.FSym = syTo);
      S.FEndValue := ParseExpr(false, [syDo], true);
    end
    else S.FExpr := ParseExpr(false, [syDo], true);
    ParseBlock([], S.GetItems);
  finally
    FAfter := after;
  end;
end;

procedure TLyseeParser.ParseWhile(SX: TLyseeSTMTList);
var
  after: integer;
  S: TLyseeSTMT;
begin
{ while CONDITION do .... }
  after := FAfter;
  try
    FAfter := FLast.FCol;
    S := SX.Add(ssWhile);
    S.FExpr := ParseExpr(false, [syDo], true);
    ParseBlock([], S.GetItems);
  finally
    FAfter := after;
  end;
end;

procedure TLyseeParser.ParseRepeat(SX: TLyseeSTMTList);
var
  after: integer;
  S: TLyseeSTMT;
begin
{ repeat .... until CONDITION; }
  after := FAfter;
  try
    FAfter := FLast.FCol;
    S := SX.Add(ssRepeat);
    ParseBlock([syUntil], S.GetItems);
    if FLast.FCol = FAfter then
    begin
      S.FExpr := ParseExpr(false, [sySemic], true);
      SymGetNext;
    end
    else EUnexpected;
  finally
    FAfter := after;
  end;
end;

procedure TLyseeParser.ParseCase(SX: TLyseeSTMTList);
var
  after: integer;
  S: TLyseeCase;

  procedure parse_branch;
  var
    A: integer;
    T: TLyseeSTMT;
  begin
    SymTestLast(ExprHead);
    A := FAfter;
    try
      FAfter := FLast.FCol;
      T := S.GetItems.Add(ssNormal);
      T.FExpr := ParseExpr(true, [syColon], true);
      ParseBlock([], T.GetItems);
    finally
      FAfter := A;
    end;
    if FLast.FSym <> syEOF then
      if (FLast.FSym = syElse) and (FLast.FCol = FAfter) then
        ParseBlock([], S.GetElseItems) else
      if FLast.FCol > FAfter then
        parse_branch;
  end;

begin
{ case EXPRESSION of
    V1: ......
    V2: ......
   else ...... }
  after := FAfter;
  try
    FAfter := FLast.FCol;
    S := SX.Add(ssCase) as TLyseeCase;
    S.FExpr := ParseExpr(false, [syOf], true);
    SymGotoNext;
    parse_branch;
  finally
    FAfter := after;
  end;
end;

procedure TLyseeParser.ParseTry(SX: TLyseeSTMTList);
var
  after: integer;
  S, T: TLyseeTry;
begin
{ try ....
  except ....
  finally .... }
  after := FAfter;
  try
    FAfter := FLast.FCol;
    S := SX.Add(ssTry) as TLyseeTry;
    ParseBlock([syExcept, syFinally], S.GetItems);
    if FLast.FCol = FAfter then
    begin
      S.FTryFinally := (FLast.FSym = syFinally);
      ParseBlock([], S.GetElseItems);
      while (FLast.FSym in [syExcept, syFinally]) and (FLast.FCol = FAfter) do
      begin
        T := SX.Add(ssTry) as TLyseeTry;
        T.GetItems.FItems.Add(S);
        T.FTryFinally := (FLast.FSym = syFinally);
        ParseBlock([], T.GetElseItems);
        S := T;
      end;
    end;
  finally
    FAfter := after;
  end;
end;

procedure TLyseeParser.ParseType(OnHead: boolean; var T: TLyseeType);
var
  I, M: string;
begin
  if not OnHead then SymTestNext([syID, syArray]);
  if FLast.FSym = syArray then T := my_array else
  begin
    if SymPeekNext = syDot then
    begin
      M := FLast.FName;
      SymGotoNext;
      SymTestNextID;
    end
    else M := '';
    I := FLast.FName;
    T := FModule.FindTypeBy(I, M);
    if T = nil then ETypeNotFound;
  end;
end;

procedure TLyseeParser.ParseRaise(SX: TLyseeSTMTList);
var
  after: integer;
  S: TLyseeSTMT;
begin
{ raise EXPRESSION; | raise; }
  after := FAfter;
  try
    FAfter := FLast.FCol;
    S := SX.Add(ssRaise);
    SymGotoNext;
    if FLast.FSym <> sySemic then
      S.FExpr := ParseExpr(true, [sySemic], true);
    SymGetNext;
  finally
    FAfter := after;
  end;
end;

procedure TLyseeParser.ParsePuts(SX: TLyseeSTMTList);
var
  after: integer;
  S, T: TLyseeSTMT;
begin
  after := FAfter;
  try
    FAfter := FLast.FCol;
    S := SX.Add(ssPuts);
    repeat
      T := S.GetItems.Add(ssNormal);
      T.FExpr := ParseExpr(false, [syComma, sySemic], true);
    until FLast.FSym = sySemic;
    SymGetNext;
  finally
    FAfter := after;
  end;
end;

procedure TLyseeParser.ParseAny(SX: TLyseeSTMTList);
var
  after: integer;
  V: TLyseeToken;
  S: TLyseeSTMT;
begin
  after := FAfter;
  try
    FAfter := FLast.FCol;
    if FTokenizer.NextIsBecome(false) then
    begin
      SymTestLast([syID, syResult]);
      if FLast.FSym = syID then
      begin
        S := SX.Add(ssAssign);
        TLyseeAssign(S).FVarb := FLast.FName;
        FFunc.FParams.AddLocal(FLast.FName, my_variant);
      end
      else S := SX.Add(ssResult);
      V := FLast;
      SymGotoNext;
      if FLast.FSym <> syBecome then // +=, -=, *=, ....
      begin
        S.FExpr := TLyseeToken.Create(FLast);  // operator
        S.FExpr.FLeft := TLyseeToken.Create(V);
        SymGotoNext;
        S.FExpr.FRight := ParseExpr(false, [sySemic], true);
      end
      else S.FExpr := ParseExpr(false, [sySemic], true);
    end
    else
    begin
      S := SX.Add(ssNormal);
      S.FExpr := ParseExpr(true, [sySemic, syBecome], true);
      if FLast.FSym = syBecome then
        if S.FExpr.FSym = syGet then
        begin
          S.FExpr.FRight := ParseExpr(false, [sySemic], true); {<--set a[..] = v}
          S.FExpr.FSym := sySet;
        end
        else EUnexpected;
    end;
    SymGetNext;
  finally
    FAfter := after;
  end;
end;

procedure TLyseeParser.ParseArguments(EndSym: TLyseeSymbol);
var
  V: TLyseeVarb;
  I, count: integer;
begin
  count := 0;
  SymTestNext([syID, EndSym]);
  while FLast.FSym <> EndSym do
    if FFunc.FindInside(FLast.FName) then ERedeclared else
    begin
      V := FFunc.FParams.Add(FLast.FName, my_variant);
      SymTestNext([syComma, sySemic, EndSym, syColon]);
      Inc(count);
      if FLast.FSym = syColon then
      begin
        ParseType(false, V.FType);
        I := FFunc.FParams.Count - 2; // second last
        while count > 1 do
        begin
          FFunc.FParams[I].FType := V.FType;
          Dec(count);
          Dec(I);
        end;
        SymTestNext([sySemic, EndSym]);
      end;
      if FLast.FSym = sySemic then count := 0;
      if FLast.FSym <> EndSym then
        SymTestNext([syID]);
    end;
end;

function TLyseeParser.ParseTerm: TLyseeToken;

  procedure parse_array;
  begin
    while FLast.FSym <> syRArray do
      Result.AddParam(ParseExpr(false, [syComma, syRArray], true));
  end;

  procedure parse_hash;
  begin
    Result.FSym := syHash;
    Result.AddParam(ParseExpr(false, [syComma, syRArray], true));
    while FLast.FSym <> syRArray do
    begin
      Result.AddParam(ParseExpr(false, [syColon], true));
      Result.AddParam(ParseExpr(false, [syComma, syRArray], true));
    end;
  end;

  procedure parse_call(EndSym: TLyseeSymbol);
  begin
    SymGotoNext;
    if FLast.FSym <> EndSym then
    begin
      Result.AddParam(ParseExpr(true, [syComma, EndSym], true));
      while FLast.FSym <> EndSym do
        Result.AddParam(ParseExpr(false, [syComma, EndSym], true));
    end
    else if EndSym = syRArray then EUnexpected;
  end;

var
  F: TLyseeFunc;
begin
  Result := nil;

  { not term | - term }
  if FLast.FSym in [syNot, syReduce] then
  begin
    Result := UseToken(FLast);
    if Result.FSym = syReduce then Result.FSym := syNeg;
    SymGotoNext;
    Result.FRight := Self.ParseTerm;
    Exit;
  end;

  { @function | @object.method }
  if FLast.FSym = syAt then
  begin
    SymGotoNext;
    Result := Self.ParseTerm;
    if Result.FSym = syID then Result.FSym := syAt else
    if (Result.FSym = syGet) and (Result.FName <> '') and (Result.GetParamCount = 0) then
      Result.FSym := syAt else
      begin
        FreeAndNil(Result);
        EUnexpected(FLast);
      end;
    Exit;
  end;

  { ID | value | string | true | false | ... }
  if FLast.FSym in DataSyms then
  begin
    if FLast.FSym = syArray then
    begin
      FLast.FSym := syID;
      FLast.FName := 'array';
    end;
    Result := UseToken(FLast);
    if (FLast.FSym = syID) and (SymPeekNext = syLParen) then
    begin
      SymGotoNext;
      Result.FSym := syCall;
      parse_call(syRParen);
    end;
  end
  else
  if FLast.FSym = syLArray then // list or hashed
  begin
    Result := UseToken(FLast);
    Result.FSym := syArray;
    SymGotoNext;
    if FLast.FSym in [syComma, syColon] then
    begin
      if FLast.FSym = syColon then Result.FSym := syHash;
       SymTestNext([syRArray]);
    end
    else
    if FLast.FSym <> syRArray then
    begin
      Result.AddParam(ParseExpr(true, [syComma, syColon, syRArray], true));
      if FLast.FSym = syComma then parse_array else
      if FLast.FSym = syColon then parse_hash;
    end;
  end
  else
  if FLast.FSym = syVert then
  begin
    Result := UseToken(FLast);
    F := FFunc;
    try
      FFunc := TLyseeFunc.Create('', FModule, nil);
      ParseArguments(syVert);
      FFunc.STMTs.Add(ssResult).FExpr := ParseExpr(false, [], false);
      Result.FValue.VFunc := FFunc;
      FFunc.IncRefcount;
    finally
      FFunc := F;
    end;
  end
  else
  if FLast.FSym = syLParen then
    Result := ParseExpr(false, [syRParen], true) else
    EUnexpected(FLast);

  SymGetNext;
  while (FLast.FCol > FAfter) and (FLast.FSym in [syLParen, syDot, syLArray]) do
  begin
    if FLast.FSym = syLParen then // expression(...)
    begin
      Result := TLyseeToken.CreateWithLeft(Result, FLast);
      Result.FSym := syCall;
      Result.FName := '';
      parse_call(syRParen);
    end
    else
    if FLast.FSym = syLArray then // expression[...]
    begin
      Result := TLyseeToken.CreateWithLeft(Result, FLast);
      Result.FSym := syGet;
      Result.FName := '';
      parse_call(syRArray);
    end
    else // expression.ID
    begin
      SymTestNext([syBegin..syEnd, syID]);
      Result := TLyseeToken.CreateWithLeft(Result, FLast);
      Result.FSym := syGet;
      Result.FName := FLast.FName;
      if SymPeekNext = syLParen then // expression.ID(...)
      begin
        SymGotoNext;
        Result.FSym := syCall;
        parse_call(syRParen);
      end
      else
      if SymPeekNext = syLArray then // expression.ID[...]
      begin
        SymGotoNext;
        parse_call(syRArray);
      end;
    end;
    SymGetNext;
  end;
end;

function TLyseeParser.SymPeekNext: TLyseeSymbol;
begin
  Result := FTokenizer.PeekNextSymbol;
end;

{ TLyseeSTMT }

constructor TLyseeSTMT.Create(AParent: TLyseeSTMTList);
begin
  FStyle := ssNormal;
  FParent := AParent;
  FParent.FItems.Add(Self);
end;

procedure TLyseeSTMT.Decompile(Level: integer; Lines: TStrings);

  procedure decompile_normal;
  begin
    Lines.Add(Margin(Level) + FExpr.Decompile + ';');
  end;

  procedure decompile_puts;
  var
    S: string;
    I: integer;
  begin
    if GetCount > 0 then
    begin
      S := '= ' + FItems[0].FExpr.Decompile;
      for I := 1 to FItems.Count - 1 do
        S := S + ', ' + FItems[I].FExpr.Decompile;
      Lines.Add(Margin(Level) + S + ';');
    end;
  end;

  procedure decompile_while;
  begin
    Lines.Add(Margin(Level) + 'while ' + FExpr.Decompile  + ' do');
    if GetCount > 0 then
      FItems.Decompile(Level + 1, Lines) else
      Lines[Lines.Count - 1] := Lines[Lines.Count - 1] + ';';
  end;

  procedure decompile_repeat;
  begin
    Lines.Add(Margin(Level) + 'repeat');
    if GetCount > 0 then
      FItems.Decompile(Level + 1, Lines);
    Lines.Add(Margin(Level) + 'until ' + FExpr.Decompile  + ';');
  end;

  procedure decompile_raise;
  begin
    if FExpr <> nil then
      Lines.Add(Margin(Level) + 'raise ' + FExpr.Decompile + ';') else
      Lines.Add(Margin(Level) + 'raise;');
  end;

begin
  case FStyle of
    ssNormal: decompile_normal;
    ssPuts  : decompile_puts;
    ssWhile : decompile_while;
    ssRepeat: decompile_repeat;
    ssRaise : decompile_raise;
  end;
end;

destructor TLyseeSTMT.Destroy;
begin
  if FParent <> nil then
    FParent.FItems.Remove(Self);
  FreeAndNil(FExpr);
  FreeAndNil(FItems);
  inherited;
end;

procedure TLyseeSTMT.ExecNormal(Param: TLyseeParam);
var
  tmpv: TLyseeValue;
  mark: integer;
begin
  Param.BeginExec(mark, tmpv);
  try
    FExpr.Execute(Param, tmpv);
  finally
    Param.EndExec(mark);
  end;
end;

procedure TLyseeSTMT.ExecPuts(Param: TLyseeParam);
var
  tmpv: TLyseeValue;
  mark, I: integer;
begin
  Param.BeginExec(mark, tmpv);
  try
    if (FItems <> nil) and (FItems.Count > 0) then
      if FItems[0].FExpr.Execute(Param, tmpv) then
      begin
        Param.FLysee.Write(tmpv.AsString);
        for I := 1 to FItems.Count - 1 do
          if FItems[I].FExpr.Execute(Param, tmpv) then
          begin
            Param.FLysee.Write(' ');
            Param.FLysee.Write(tmpv.AsString);
          end
          else Break;
      end;
  finally
    Param.EndExec(mark);
  end;
end;

procedure TLyseeSTMT.ExecRaise(Param: TLyseeParam);
var
  tmpv: TLyseeValue;
  mark: integer;
begin
  if FExpr <> nil then
  begin
    Param.BeginExec(mark, tmpv);
    try
      if FExpr.Execute(Param, tmpv) then
        FExpr.Error(Param, tmpv.AsString);
    finally
      Param.EndExec(mark);
    end;
  end
  else Param.FLysee.FExcepted := true;
end;

procedure TLyseeSTMT.ExecRepeat(Param: TLyseeParam);
var
  cntx: TLysee;
  tmpv: TLyseeValue;
  mark: integer;
begin
  cntx := Param.FLysee;
  Param.BeginExec(mark, tmpv);
  try
    repeat
      if not FItems.Execute(Param) then
        if cntx.FState in [csBreak, csContinue] then
        begin
          if cntx.FState = csBreak then Break;
          cntx.FState := csRunning;
        end;
    until not cntx.StatusOK or not FExpr.Execute(Param, tmpv) or tmpv.AsBoolean;
  finally
    cntx.ForWhileRepeatEnded;
    Param.EndExec(mark);
  end;
end;

function TLyseeSTMT.Execute(Param: TLyseeParam): boolean;
begin
  case FStyle of
    ssNormal: ExecNormal(Param);
    ssPuts  : ExecPuts(Param);
    ssWhile : ExecWhile(Param);
    ssRepeat: ExecRepeat(Param);
    ssRaise : ExecRaise(Param);
  end;
  Result := Param.FLysee.StatusOK;
end;

procedure TLyseeSTMT.ExecWhile(Param: TLyseeParam);
var
  cntx: TLysee;
  tmpv: TLyseeValue;
  mark: integer;
begin
  cntx := Param.FLysee;
  Param.BeginExec(mark, tmpv);
  try
    while cntx.StatusOK and FExpr.Execute(Param, tmpv) and tmpv.AsBoolean do
      if not FItems.Execute(Param) then
        if cntx.FState in [csBreak, csContinue] then
        begin
          if cntx.FState = csBreak then Break;
          cntx.FState := csRunning;
        end;
  finally
    cntx.ForWhileRepeatEnded;
    Param.EndExec(mark);
  end;
end;

function TLyseeSTMT.GetCount: integer;
begin
  if FItems <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function TLyseeSTMT.GetItems: TLyseeSTMTList;
begin
  if FItems = nil then
    FItems := TLyseeSTMTList.Create;
  Result := FItems;
end;

{ TLyseeAssign }

constructor TLyseeAssign.Create(AParent: TLyseeSTMTList);
begin
  inherited;
  FStyle := ssAssign;
end;

procedure TLyseeAssign.Decompile(Level: integer; Lines: TStrings);

  procedure decompile_const;
  begin
    Lines.Add(Margin(Level) + 'const ' + FVarb + ' = ' + FExpr.Decompile + ';');
  end;

  procedure decompile_assign;
  begin
    Lines.Add(Margin(Level) + FVarb + ' := ' + FExpr.Decompile + ';');
  end;

  procedure decompile_result;
  begin
    Lines.Add(Margin(Level) + 'Result := ' + FExpr.Decompile + ';');
  end;

begin
  case FStyle of
    ssConst : decompile_const;
    ssAssign: decompile_assign;
    ssResult: decompile_result;
  end;
end;

procedure TLyseeAssign.ExecAssign(Param: TLyseeParam);
var
  I: integer;
  V: TLyseeValue;
begin
  I := Param.FFunc.FParams.IndexOf(FVarb);
  V := Param.FParams[I];
  if FExpr.Execute(Param, V) then
    Param.FFunc.FParams[I].FType.Convert(V);
end;

procedure TLyseeAssign.ExecConst(Param: TLyseeParam);
begin
  if FExpr.Execute(Param, Param.FResult) then
  begin
    Param.FFunc.FResultType.Convert(Param.FResult);
    Param.FFunc.FModule.FConsts.Add(FVarb).SetValue(Param.FResult);
  end;
end;

procedure TLyseeAssign.ExecResult(Param: TLyseeParam);
begin
  if FExpr.Execute(Param, Param.FResult) then
    Param.FFunc.FResultType.Convert(Param.FResult);
end;

function TLyseeAssign.Execute(Param: TLyseeParam): boolean;
begin
  case FStyle of
    ssConst : ExecConst(Param);
    ssAssign: ExecAssign(Param);
    ssResult: ExecResult(Param);
  end;
  Result := Param.FLysee.StatusOK;
end;

{ TLyseeFor }

constructor TLyseeFor.Create(AParent: TLyseeSTMTList);
begin
  inherited;
  FStyle := ssFor;
end;

procedure TLyseeFor.Decompile(Level: integer; Lines: TStrings);
var
  S: string;
begin
  S := 'for ' + FVarb;
  if FEndValue <> nil then
  begin
    S := S + ' := ' + FExpr.Decompile;
    if FUpTo then
      S := S + ' to ' + FEndValue.Decompile else
      S := S + ' downto ' + FEndValue.Decompile;
  end
  else S := S + ' in ' + FExpr.Decompile;
  Lines.Add(Margin(Level) + S + ' do');
  if GetCount > 0 then
    FItems.Decompile(Level + 1, Lines) else
    Lines[Lines.Count - 1] := Lines[Lines.Count - 1] + ';';
end;

destructor TLyseeFor.Destroy;
begin
  FreeAndNil(FEndValue);
  inherited;
end;

function TLyseeFor.Execute(Param: TLyseeParam): boolean;
var
  cntx: TLysee;
  mark, I: integer;
  G: TLyseeGenerate;
  V: TLyseeValue;
  T: TLyseeType;

  function get_generate: TLyseeGenerate;
  var
    begv, endv: TLyseeValue;
  begin
    Result := nil;
    begv := Param.FParams.Add;
    if FExpr.Execute(Param, begv) then
      if FEndValue <> nil then
      begin
        endv := Param.FParams.Add;
        if FEndValue.Execute(Param, endv) then
          Result := GetGenerate(begv, endv, FUpTo);
      end
      else Result := GetGenerate(begv);
  end;

begin
  cntx := Param.FLysee;
  Param.BeginExec(mark);
  try
    G := get_generate;
    if G <> nil then
    try
      I := Param.FFunc.FParams.IndexOf(FVarb);
      T := Param.FFunc.FParams[I].FType;
      V := Param.FParams[I];
      while cntx.StatusOK and G.GetNext do
      begin
        V.SetValue(G);
        T.Convert(V);
        if not FItems.Execute(Param) then
          if cntx.FState in [csBreak, csContinue] then
          begin
            if cntx.FState = csBreak then Break;
            cntx.FState := csRunning;
          end;
      end;
    finally
      G.Free;
    end;
  finally
    cntx.ForWhileRepeatEnded;
    Param.EndExec(mark);
  end;
  Result := cntx.StatusOK;
end;

{ TLyseeIfSTMT }

constructor TLyseeIf.Create(AParent: TLyseeSTMTList);
begin
  inherited;
  FStyle := ssIf;
end;

procedure TLyseeIf.Decompile(Level: integer; Lines: TStrings);

  procedure decompile_else(S: TLyseeIf);
  begin
    if S.IsElif then
    begin
      Lines.Add(Margin(Level) + 'elif ' + S.FExpr.Decompile + ' then');
      if S.GetCount > 0 then
        S.FItems.Decompile(Level + 1, Lines);
      decompile_else(S.FElseItems[0] as TLyseeIf);
    end
    else
    if GetElseCount > 0 then
    begin
      Lines.Add(Margin(Level) + 'else');
      FElseItems.Decompile(Level + 1, Lines);
    end;
  end;

begin
  Lines.Add(Margin(Level) + 'if ' + FExpr.Decompile + ' then');
  if GetCount > 0 then
    FItems.Decompile(Level + 1, Lines);
  decompile_else(Self);
end;

destructor TLyseeIf.Destroy;
begin
  FreeAndNil(FElseItems);
  inherited;
end;

function TLyseeIf.Execute(Param: TLyseeParam): boolean;
var
  tmpv: TLyseeValue;
  mark: integer;
begin
  Param.BeginExec(mark, tmpv);
  try
    Result := FExpr.Execute(Param, tmpv);
    if Result then
      if tmpv.AsBoolean then
        Result := FItems.Execute(Param) else
        Result := FElseItems.Execute(Param);
  finally
    Param.EndExec(mark);
  end;
end;

function TLyseeIf.GetElseCount: integer;
begin
  if FElseItems <> nil then
    Result := FElseItems.Count else
    Result := 0;
end;

function TLyseeIf.GetElseItems: TLyseeSTMTList;
begin
  if FElseItems = nil then
    FElseItems := TLyseeSTMTList.Create;
  Result := FElseItems;
end;

function TLyseeIf.IsElif: boolean;
begin
  Result := (GetElseCount = 1) and (FElseItems[0].FStyle = ssIf);
end;

{ TLyseeCase }

constructor TLyseeCase.Create(AParent: TLyseeSTMTList);
begin
  inherited;
  FStyle := ssCase;
end;

procedure TLyseeCase.Decompile(Level: integer; Lines: TStrings);
var
  I: integer;
  S: TLyseeSTMT;
begin
  Lines.Add(Margin(Level) + 'case ' + FExpr.Decompile + ' of');
  for I := 0 to GetCount - 1 do
  begin
    S := FItems[I];
    Lines.Add(Margin(Level + 1) + S.FExpr.Decompile + ':');
    S.FItems.Decompile(Level + 2, Lines);
  end;
  if GetElseCount > 0 then
  begin
    Lines.Add(Margin(Level) + 'else');
    FElseItems.Decompile(Level + 1, Lines);
  end;
end;

function TLyseeCase.Execute(Param: TLyseeParam): boolean;
var
  tmpv, T: TLyseeValue;
  mark, I: integer;
begin
  Result := false;;
  Param.BeginExec(mark, tmpv);
  try
    if FExpr.Execute(Param, tmpv) then
    begin
      T := Param.FParams.Add;
      for I := 0 to FItems.Count - 1 do
        if not FItems[I].FExpr.Execute(Param, T) then Break else
          if tmpv.Same(T) then
          begin
            FItems[I].FItems.Execute(Param);
            Result := true;
            Break;
          end;
      if Param.FLysee.StatusOK then
        Result := Result or FElseItems.Execute(Param) else
        Result := false;
    end;
  finally
    Param.EndExec(mark);
  end;
end;

{ TLyseeTry }

constructor TLyseeTry.Create(AParent: TLyseeSTMTList);
begin
  inherited;
  FStyle := ssTry;
end;

procedure TLyseeTry.Decompile(Level: integer; Lines: TStrings);
begin
  Lines.Add(Margin(Level) + 'try');
  FItems.Decompile(Level + 1, Lines);
  if FTryFinally then
    Lines.Add(Margin(Level) + 'finally') else
    Lines.Add(Margin(Level) + 'except');
  FElseItems.Decompile(Level + 1, Lines);
end;

function TLyseeTry.Execute(Param: TLyseeParam): boolean;
var
  E: boolean;
begin
  FItems.Execute(Param);
  E := Param.FLysee.FExcepted;
  Param.FLysee.FExcepted := false;
  if FTryFinally or E then
    FElseItems.Execute(Param);
  if FTryFinally and E then
    Param.FLysee.FExcepted := true;
  Result := Param.FLysee.StatusOK;
end;

{ TLyseeSTMTList }

function TLyseeSTMTList.Add(Style: TLyseeSTMTStyle): TLyseeSTMT;
begin
  if FItems = nil then FItems := TList.Create;
  Result := nil;
  case Style of
    ssNormal: Result := TLyseeSTMT.Create(Self);
    ssConst : Result := TLyseeAssign.Create(Self);
    ssAssign: Result := TLyseeAssign.Create(Self);
    ssResult: Result := TLyseeAssign.Create(Self);
    ssPuts  : Result := TLyseeSTMT.Create(Self);
    ssIf    : Result := TLyseeIf.Create(Self);
    ssWhile : Result := TLyseeSTMT.Create(Self);
    ssRepeat: Result := TLyseeSTMT.Create(Self);
    ssFor   : Result := TLyseeFor.Create(Self);
    ssCase  : Result := TLyseeCase.Create(Self);
    ssTry   : Result := TLyseeTry.Create(Self);
    ssRaise : Result := TLyseeSTMT.Create(Self);
  end;
  Result.FStyle := Style;
end;

function TLyseeSTMTList.Add(STMT: TLyseeSTMT): integer;
begin
  if FItems = nil then FItems := TList.Create;
  if STMT.FParent <> Self then
  begin
    STMT.FParent.FItems.Remove(STMT);
    STMT.FParent := Self;
  end;
  Result := FItems.Add(STMT);
end;

procedure TLyseeSTMTList.Clear;
var
  I: integer;
begin
  for I := GetCount - 1 downto 0 do Delete(I);
  FreeAndNil(FItems);
end;

procedure TLyseeSTMTList.Decompile(Level: integer; Lines: TStrings);
var
  I: integer;
begin
  if Level < 1 then Level := 1;
  for I := 0 to GetCount - 1 do
    GetItem(I).Decompile(Level, Lines);
end;

procedure TLyseeSTMTList.Delete(Index: integer);
var
  S: TLyseeSTMT;
begin
  S := GetItem(Index);
  FItems.Delete(Index);
  S.FParent := nil;
  S.Free;
end;

destructor TLyseeSTMTList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TLyseeSTMTList.Execute(Param: TLyseeParam): boolean;
var
  I: integer;
begin
  if Self <> nil then
    for I := 0 to GetCount - 1 do
    begin
      Result := GetItem(I).Execute(Param);
      if not Result then Exit;
    end;
  Result := true;
end;

function TLyseeSTMTList.GetCount: integer;
begin
  if FItems <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function TLyseeSTMTList.GetItem(Index: integer): TLyseeSTMT;
begin
  Result := TLyseeSTMT(FItems[Index]);
end;

{ TLyseeVarb }

constructor TLyseeVarb.Create(const AName: string; AType: TLyseeType);
begin
  FName := AName;
  FType := AType;
end;

destructor TLyseeVarb.Destroy;
begin
  FName := '';
  inherited;
end;

function TLyseeVarb.Prototype: string;
begin
  Result := FType.Prototype(FName);
end;

{ TLyseeVarbList }

function TLyseeVarbList.GetCount: integer;
begin
  Result := Length(FVarbs);
end;

function TLyseeVarbList.GetParamCount: integer;
begin
  Result := GetCount - FLocalCount;
end;

function TLyseeVarbList.GetVarb(Index: integer): TLyseeVarb;
begin
  Result := FVarbs[Index];
end;

function TLyseeVarbList.IndexOf(const AVarb: TLyseeVarb): integer;
var
  I: integer;
begin
  for I := 0 to Length(FVarbs) - 1 do
    if FVarbs[I] = AVarb then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function TLyseeVarbList.DoAdd(const AName: string; AType: TLyseeType): TLyseeVarb;
var
  I: integer;
begin
  Result := TLyseeVarb.Create(AName, AType);
  I := Length(FVarbs);
  SetLength(FVarbs, I + 1);
  FVarbs[I] := Result;
end;

constructor TLyseeVarbList.Create;
begin
  SetLength(FVarbs, 0);
  FLocalCount := 0;
end;

destructor TLyseeVarbList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLyseeVarbList.Clear;
var
  I: integer;
begin
  FLocalCount := 0;
  for I := 0 to Length(FVarbs) - 1 do
  begin
    FVarbs[I].FName := '';
    FVarbs[I].Free;
  end;
  SetLength(FVarbs, 0);
end;

function TLyseeVarbList.Add(const AName: string; AType: TLyseeType): TLyseeVarb;
begin
  Result := Find(AName);
  if Result = nil then
    Result := DoAdd(AName, AType);
end;

function TLyseeVarbList.AddLocal(const AName: string; AType: TLyseeType): TLyseeVarb;
begin
  Result := Find(AName);
  if Result = nil then
  begin
    Result := DoAdd(AName, AType);
    Inc(FLocalCount);
  end;
end;

procedure TLyseeVarbList.Assign(Source: TLyseeVarbList);
var
  I: integer;
begin
  Clear;
  for I := 0 to Source.GetCount - 1 do
    DoAdd(Source[I].FName, Source[I].FType);
  FLocalCount := Source.FLocalCount;
end;

function TLyseeVarbList.IndexOf(const AName: string): integer;
var
  I: integer;
begin
  for I := 0 to Length(FVarbs) - 1 do
    if MatchID(FVarbs[I].FName, AName) then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function TLyseeVarbList.Find(const AName: string): TLyseeVarb;
var
  I: integer;
begin
  I := IndexOf(AName);
  if (I >= 0) and (I < Length(FVarbs)) then
    Result := FVarbs[I] else
    Result := nil;
end;

{ TLyseeFunc }

constructor TLyseeFunc.Create(const AName: string; M: TLyseeModule; Proc: TLyseeProc);
begin
  FName := AName;
  if FName <> '' then
  begin
    IncRefcount;
    M.FFuncList.FItems.Add(Self);
  end;
  FModule := M;
  FParams := TLyseeVarbList.Create;
  FResultType := my_variant;
  FProc := Proc;
  FMinArgs := -1;
  if Context <> nil then
    if Context.FRollbacks <> nil then
      if Context.FRollbacks.IndexOf(FModule) < 0 then
        Context.FRollbacks.Add(Self);
end;

constructor TLyseeFunc.CreateMethod(const AName: string; P: TLyseeType; Proc: TLyseeProc);
begin
  FName := AName;
  FParent := P;
  FParent.FMethods.FItems.Add(Self);
  IncRefcount;
  FModule := FParent.FModule;
  FParams := TLyseeVarbList.Create;
  FResultType := my_variant;
  FProc := Proc;
  FMinArgs := -1;
end;

procedure TLyseeFunc.Decompile(Level: integer; Lines: TStrings);
begin
  Lines.Add(Margin(Level) + Prototype);
  if (FSTMTs <> nil) and not IsConst then
    FSTMTs.Decompile(Level + 1, Lines);
end;

destructor TLyseeFunc.Destroy;
begin
  if FModule <> nil then
    FModule.FFuncList.FItems.Remove(Self);

  if FParent <> nil then
  begin
    if FParent.FConstructer = Self then
      FParent.FConstructer := nil;
    if FParent.FMethods <> nil then
      FParent.FMethods.FItems.Remove(Self);
  end;

  FreeAndNil(FSTMTs);
  FreeAndNil(FParams);

  if Context <> nil then
  begin
    Context.RollbackRemove(Self);
    if IsMainFunc then
    begin
      Context.FMainFunc := nil;
      if Context.FMainLocals <> nil then
        Context.FMainLocals.Clear;
    end;
  end;

  inherited;
end;

function TLyseeFunc.Executing: boolean;
var
  P: TLyseeParam;
begin
  if FModule.FContext <> nil then
  begin
    P := FModule.FContext.FCurrent;
    while P <> nil do
    begin
      if P.FFunc = Self then
      begin
        Result := true;
        Exit;
      end;
      P := P.FPrev;
    end;
  end;
  Result := false;
end;

function TLyseeFunc.AddCode(const Code: string): boolean;
var
  P: TLyseeParser;
begin
  Result := ChangeAble;
  if Result and (Code <> '') then
  begin
    P := TLyseeParser.Create(FModule);
    try
      P.FTokenizer := TLyseeTokenizer.Create(Code);
      P.SymGetNext;
      while P.FLast.FSym <> syEOF do
      begin
        P.FAfter := -1;
        P.FFunc := Self;
        P.ParseStatement(true, STMTs);
        P.FTokenizer.PackToCurrent;
      end;
    finally
      P.Free;
    end;
  end;
end;

function TLyseeFunc.AsString: string;
begin
  Result := Prototype;
end;

function TLyseeFunc.ChangeAble: boolean;
begin
  Result := (FName = '') and not Assigned(FProc) and not Executing;
end;

function TLyseeFunc.Context: TLysee;
begin
  if FModule <> nil then
    Result := FModule.FContext else
    Result := nil;
end;

function TLyseeFunc.GetFullName: string;
begin
  if FParent <> nil then
    Result := FParent.FName + '.' + Name else
    Result := FModule.FName + '.' + Name;
end;

function TLyseeFunc.GetMinArgs: integer;
var
  I: integer;
begin
  if FMinArgs < 0 then
  begin
    FMinArgs := FParams.ParamCount;
    for I := 0 to FParams.ParamCount - 1 do
      if CharInSet(FParams[I].FName[1], ['_', '.']) then
      begin
        FMinArgs := I;
        Break;
      end;
  end;
  Result := FMinArgs;
end;

function TLyseeFunc.GetSTMTs: TLyseeSTMTList;
begin
  if FSTMTs = nil then
    FSTMTs := TLyseeSTMTList.Create;
  Result := FSTMTs;
end;

function TLyseeFunc.Prototype: string;
var
  X: integer;
  P: TLyseeVarb;
  N: string;
  L: TStrings;
begin
  if IsConst then
  begin
    L := TStringList.Create;
    try
      FSTMTs[0].Decompile(0, L);
      Result := L.Text;
    finally
      L.Free;
    end;
  end
  else
  begin
    X := Ord(FParent <> nil);
    if FParams.ParamCount > X then
    begin
      P := FParams[X];
      Result := '(' + P.FType.Prototype(P.FName);
      for X := X + 1 to FParams.ParamCount - 1 do
      begin
        P := FParams[X];
        Result := Result + ', ' + P.FType.Prototype(P.FName);
      end;
      Result := Result + ')';
    end
    else Result := '';
    if FParent <> nil then
      N := ' ' + FParent.FName + '.' + Name else
    if Name <> '' then
      N := ' ' + Name else
      N := '';
    if FResultType = my_nil then
      Result := 'procedure' + N + Result + ';' else
      Result := FResultType.Prototype('function' + N + Result) + ';';
  end;
end;

function TLyseeFunc.SetCode(const Code: string): boolean;
begin
  Result := ChangeAble;
  if Result then
  begin
    if FSTMTs <> nil then FSTMTs.Clear;
    Result := AddCode(Code);
  end;
end;

function TLyseeFunc.FindBy(const ID: string; rec: PLyseeFind; Range: TLyseeFinds): boolean;
var
  I, M: string;
begin
  Result := (ID <> '');
  if Result then
  begin
    I := ExtractNameModule(ID, M);
    if M <> '' then
      Result := FModule.FindBy(I, M, rec) else
      Result := FindInside(I, rec) or
                FModule.FindBy(I, '', rec);
    if Result then
      Result := (Range = []) or (rec^.f_find in Range);
  end;
end;

function TLyseeFunc.IsConst: boolean;
begin
  Result := (FSTMTs <> nil) and (FSTMTs.Count = 1) and (FSTMTs[0].FStyle = ssConst);
end;

function TLyseeFunc.IsConstructor: boolean;
begin
  Result := (Self <> nil) and (FParent <> nil) and (FParent.FConstructer = Self);
end;

function TLyseeFunc.IsFunction: boolean;
begin
  Result := (FResultType <> my_nil);
end;

function TLyseeFunc.IsMainFunc: boolean;
begin
  Result := (FModule.FContext <> nil) and
            (FModule.FContext.FMainFunc = Self);
end;

function TLyseeFunc.IsMethod: boolean;
begin
  Result := (Self <> nil) and (FParent <> nil);
end;

function TLyseeFunc.IsProcedure: boolean;
begin
  Result := (FResultType = my_nil);
end;

function TLyseeFunc.MakeMethod: TLyseeFunc;
var
  P: TLyseeType;
begin
  Result := nil;
  if (Self <> nil) and (FParent = nil) and Assigned(FProc) and (FParams.Count > 0) then
  begin
    P := FParams[0].FType;
    if (P <> my_nil) and (P <> my_variant) and (P.FindMethod(FName) = nil) then
    begin
      Result := TLyseeFunc.CreateMethod(FName, P, FProc);
      Result.FResultType := FResultType;
      Result.FParams.Assign(FParams);
      Result.FData := FData;
    end;
  end;
end;

function TLyseeFunc.FindInside(const ID: string; rec: PLyseeFind): boolean;
var
  findrec: RLyseeFind;
begin
  if rec = nil then rec := @findrec;
  if MatchID(ID, Name) then
  begin
    rec^.f_find := fiFunc;
    rec^.VFunc := Self;
  end
  else
  begin
    rec^.VVarb := FParams.Find(ID);
    if rec^.VVarb <> nil then
      rec^.f_find := fiVarb else
      rec^.f_find := fiNone;
  end;
  Result := (rec^.f_find <> fiNone);
end;

function TLyseeFunc.FindSave(const ID: string; Param: TLyseeParam; Outv: TLyseeValue): TLyseeFind;
var
  R: RLyseeFind;
begin
  if (ID <> '') and FindBy(ID, @R) then
  begin
    Result := R.f_find;
    if Result = fiVarb then
      Outv.SetValue(Param.FParams[FParams.IndexOf(R.VVarb)]) else
      Outv.SetFind(@R);
  end
  else Result := fiNone;
end;

{ TLyseeFuncList }

constructor TLyseeFuncList.Create;
begin
  FItems := TList.Create;
end;

destructor TLyseeFuncList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TLyseeFuncList.Get(const Name: string): TLyseeFunc;
var
  I: integer;
begin
  for I := 0 to GetCount - 1 do
  begin
    Result := self.GetItem(I);
    if MatchID(Name, Result.FName) then Exit;
  end;
  Result := nil;
end;

function TLyseeFuncList.GetCount: integer;
begin
  if FItems <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function TLyseeFuncList.GetItem(Index: integer): TLyseeFunc;
begin
  Result := TLyseeFunc(FItems[Index]);
end;

procedure TLyseeFuncList.Clear;
var
  I: integer;
begin
  for I := GetCount - 1 downto 0 do
    Delete(I);
end;

procedure TLyseeFuncList.Delete(Index: integer);
var
  F: TLyseeFunc;
begin
  F := GetItem(Index);
  FItems.Delete(Index);
  F.FModule := nil;
  F.FParent := nil;
  F.Free
end;

{ TLyseeModule }

function TLyseeModule.AddEnumType(const AName: string;
  const ItemNames: array of string): TLyseeEnumType;
begin
  Result := TLyseeEnumType.Create(AName, Self, nil);
  Result.AddItems(ItemNames);
end;

function TLyseeModule.AddFunc(const AName: string; T: TLyseeType;
  const ParamNames: array of string; const ParamTypes: array of TLyseeType;
  const Proc: TLyseeProc; const Data: pointer): TLyseeFunc;
var
  I: integer;
begin
  Result := nil;
  if (T <> nil) and (AName <> '') and Assigned(Proc) then
    if not Find(AName) and ParamOK(ParamNames, ParamTypes, false) then
    begin
      Result := TLyseeFunc.Create(AName, Self, Proc);
      Result.FResultType := T;
      Result.FData := Data;
      for I := 0 to Length(ParamNames) - 1 do
        Result.FParams.Add(ParamNames[I], ParamTypes[I]);
    end;
end;

function TLyseeModule.AddFunc(const AName: string;
  const ParamNames: array of string; const ParamTypes: array of TLyseeType;
  const Proc: TLyseeProc; const Data: pointer): TLyseeFunc;
begin
  Result := AddFunc(AName, my_nil, ParamNames, ParamTypes, Proc, Data);
end;

function TLyseeModule.AsString: string;
begin
  Result := FName;
end;

constructor TLyseeModule.Create(const AName: string);
begin
  inherited;
  InitModule;
  my_modules.Add(Self);
end;

constructor TLyseeModule.CreateEx(const AName: string; AContext: TLysee);
begin
  inherited Create(AName);
  InitModule;
  FContext := AContext;
  if FContext <> nil then
  begin
    FContext.FModules.Add(Self);
    FModules := TLyseeModuleList.Create(FContext);
    FModules.FImporter := Self;
    FImporters := TList.Create;
    if FContext.FRollbacks <> nil then
      FContext.FRollbacks.Add(Self);
    Use(my_system);
  end
  else my_modules.Add(Self);
end;

procedure TLyseeModule.InitModule;
begin
  IncRefcount;
  FFileName := my_kernel;
  FTypeList := TList.Create;
  FFuncList := TLyseeFuncList.Create;
  FConsts := TLyseeHash.Create;
  FConsts.CaseSensitive := false;
end;

destructor TLyseeModule.Destroy;
var
  A: integer;
  P: TLyseeModule;
begin
  if FContext <> nil then
  begin
    FContext.RollbackRemove(Self);
    for A := FContext.FModules.Count - 1 downto 0 do
    begin
      P := FContext.FModules[A];
      P.FImporters.Remove(Self);
      P.FModules.FModules.Remove(Self);
    end;
    FContext.FModules.FModules.Remove(Self);
    if Self = FContext.FMainModule then
      FContext.FMainModule := nil;
    FreeAndNil(FModules);
    FreeAndNil(FImporters);
  end;
  DeleteTypes;
  DeleteFunctions;
  FreeAndNil(FFuncList);
  FreeAndNil(FTypeList);
  FreeAndNil(FConsts);
  my_modules.FModules.Remove(Self);
  inherited;
end;

procedure TLyseeModule.DeleteFunctions;
begin
  FFuncList.Clear;
end;

procedure TLyseeModule.DeleteTypes;
var
  I: integer;
  T: TLyseeType;
begin
  for I := TypeCount - 1 downto 0 do
  begin
    T := GetType(I);
    FTypeList.Delete(I);
    T.Free;
  end;
end;

function TLyseeModule.GetType(Index: integer): TLyseeType;
begin
  Result := TLyseeType(FTypeList[Index]);
end;

function TLyseeModule.FindModule(const ID: string; FindPossible: boolean): TLyseeModule;
begin
  if FModules <> nil then
    Result := FModules.Find(ID) else
    Result := nil;
  if Result = nil then
    if FindPossible then
    begin
      if FContext <> nil then
        Result := FContext.FModules.Find(ID);
      if Result = nil then
        Result := my_modules.Find(ID);
    end
    else
    if MatchID(ID, 'system') then
      Result := my_system;
end;

function TLyseeModule.FindSave(const AName: string; Value: TLyseeValue): boolean;
var
  srec: RLyseeFind;
begin
  Result := (Self <> nil) and Find(AName, @srec);
  if Result then
    Value.SetFind(@srec);
end;

function TLyseeModule.FindSaveBy(const AName: string; Value: TLyseeValue): boolean;
var
  srec: RLyseeFind;
begin
  Result := (Self <> nil) and FindBy(AName, '', @srec);
  if Result then
    value.SetFind(@srec);
end;

procedure TLyseeModule.Use(M: TLyseeModule);
begin
  if (M <> nil) and (M <> Self) and (M <> my_system) then
  begin
    if (FModules <> nil) and not FModules.Has(M) then
    begin
      FModules.Add(M);
      if M.FImporters <> nil then
        M.FImporters.Add(Self);
    end;
    M.Setup;
  end;
end;

function TLyseeModule.IsMainModule: boolean;
begin
  Result := (FContext <> nil) and (FContext.FMainModule = Self);
end;

function TLyseeModule.EnsureName(const AName: string): string;
begin
  if AName = '' then
  begin
    Inc(FContext.FNameSeed);
    Result := Format('#%.3x', [FContext.FNameSeed]);
  end
  else Result := AName;
end;

function TLyseeModule.FindFunc(const ID: string): TLyseeFunc;
begin
  Result := TLyseeFunc(FFuncList.Get(ID));
end;

function TLyseeModule.FindFuncBy(const ID: string): TLyseeFunc;
var
  X: integer;
begin
  Result := FindFunc(ID);
  if (Result = nil) and (FModules <> nil) then
  begin
    for X := 0 to FModules.Count - 1 do
    begin
      Result := FModules.Modules[X].FindFunc(ID);
      if Result <> nil then Exit;
    end;
    Result := my_system.FindFunc(ID);
  end;
end;

function TLyseeModule.FindType(const ID: string): TLyseeType;
var
  X: integer;
begin
  for X := 0 to FTypeList.Count - 1 do
  begin
    Result := TLyseeType(FTypeList[X]);
    if MatchID(ID, Result.FName) then Exit;
  end;
  Result := nil;
end;

function TLyseeModule.FindTypeBy(const ID, AModule: string): TLyseeType;
var
  X: integer;
  M: TLyseeModule;
begin
  Result := nil;
  if AModule <> '' then
  begin
    M := FindModule(AModule, true);
    if M <> nil then
      Result := M.FindType(ID);
  end
  else
  begin
    Result := FindType(ID);
    if Result = nil then
    begin
      if FModules <> nil then
        for X := 0 to FModules.Count - 1 do
        begin
          M := FModules[X];
          if M <> Self then
          begin
            Result := M.FindType(ID);
            if Result <> nil then Exit;
          end;
        end;
      Result := my_system.FindType(ID);
    end;
  end;
end;

procedure TLyseeModule.Setup;
begin
  if Assigned(FOnSetup) then
  begin
    FOnSetup(Self);
    FOnSetup := nil;
  end;
end;

function TLyseeModule.SetupType(const T: TLyseeType): boolean;
begin
  Result := (T <> nil) and IsID(T.FName) and not Find(T.FName);
  if Result then
  begin
    T.FModule := Self;
    FTypeList.Add(T);
  end
  else T.Free;
end;

function TLyseeModule.Find(const ID: string; rec: PLyseeFind): boolean;
var
  FR: RLyseeFind;
begin
  if rec = nil then rec := @FR;
  rec^.f_find := fiNone;
  rec^.VModule := FindModule(ID, false);
  if rec^.VModule = nil then
  begin
    rec^.VValue := FConsts.Get(ID);
    if rec^.VValue = nil then
    begin
      rec^.VFunc := FindFunc(ID);
      if rec^.VFunc = nil then
      begin
        rec^.vtype := FindType(ID);
        if rec^.vtype <> nil then
          rec^.f_find := fiType;
      end
      else rec^.f_find := fiFunc;
    end
    else rec^.f_find := fiValue;
  end
  else rec^.f_find := fiModule;
  Result := (rec^.f_find <> fiNone);
end;

function TLyseeModule.FindBy(const ID, AModule: string; rec: PLyseeFind): boolean;
var
  X: integer;
  M: TLyseeModule;
begin
  if AModule <> '' then
  begin
    M := FindModule(AModule, true);
    Result := (M <> nil) and M.Find(ID, rec);
  end
  else
  begin
    Result := Find(ID, rec);
    if not Result and (FModules <> nil) then
    begin
      for X := 0 to FModules.Count - 1 do
      begin
        Result := FModules.Modules[X].Find(ID, rec);
        if Result then Exit;
      end;
      Result := my_system.Find(ID, rec);
    end;
  end;
end;

function TLyseeModule.TypeCount: integer;
begin
  Result := FTypeList.Count;
end;

function TLyseeModule.UseModule(const AName: string): TLyseeModule;
var
  F, S: string;
begin
  Result := FindModule(AName, true);
  if Result = nil then
  begin
    F := FContext.GetModuleFile(AName);
    if F <> '' then
    begin
      S := TrimRight(FileCode(F));
      Result := TLyseeModule.CreateEx(AName, FContext);
      try
        Result.FFileName := F;
        TLyseeParser.Create(Result).ParseAndFree(S, true);
      except
        FreeAndNil(Result);
        raise;
      end;
    end;
  end;
  Use(Result);
end;

function TLyseeModule.AddFunc(const AName: string; T: TLyseeType;
  const Proc: TLyseeProc; const Data: pointer): TLyseeFunc;
begin
  Result := AddFunc(AName, T, [], [], Proc, Data);
end;

function TLyseeModule.AddFunc(const AName: string; const Proc: TLyseeProc;
  const Data: pointer): TLyseeFunc;
begin
  Result := AddFunc(AName, my_nil, [], [], Proc, Data);
end;

{ TLyseeModuleList }

function TLyseeModuleList.Add(AModule: TLyseeModule): integer;
begin
  Result := IndexOf(AModule);
  if Result < 0 then
    Result := FModules.Add(AModule);
end;

procedure TLyseeModuleList.Clear;
var
  A: integer;
begin
  DeleteFunctions;
  ClearConsts;
  for A := FModules.Count - 1 downto 0 do
    Delete(A);
end;

procedure TLyseeModuleList.ClearConsts;
var
  I: integer;
begin
  if FImporter = nil then
    for I := 0 to GetCount - 1 do
      GetModule(I).FConsts.Clear;
end;

constructor TLyseeModuleList.Create(AContext: TLysee);
begin
  IncRefcount;
  FContext := AContext;
  FModules := TList.Create;
end;

procedure TLyseeModuleList.Delete(Index: integer);
var
  M: TLyseeModule;
begin
  M := GetModule(Index);
  FModules.Delete(Index);
  if FImporter = nil then M.Free; {<--TLysee.FModules/my_modules}
end;

destructor TLyseeModuleList.Destroy;
begin
  Clear;
  FreeAndNil(FModules);
  inherited;
end;

function TLyseeModuleList.Find(const Name: string): TLyseeModule;
var
  X: integer;
begin
  X := IndexOf(Name);
  if X >= 0 then
    Result := GetModule(X) else
    Result := nil;
end;

function TLyseeModuleList.GetCount: integer;
begin
  Result := FModules.Count;
end;

function TLyseeModuleList.GetModule(Index: integer): TLyseeModule;
begin
  Result := TLyseeModule(FModules[Index]);
end;

function TLyseeModuleList.Has(const Name: string): boolean;
begin
  Result := (IndexOf(Name) >= 0);
end;

function TLyseeModuleList.Has(AModule: TLyseeModule): boolean;
begin
  Result := (IndexOf(AModule) >= 0);
end;

function TLyseeModuleList.IndexOf(const Name: string): integer;
var
  X: integer;
  M: TLyseeModule;
begin
  if Name <> '' then
    for X := 0 to FModules.Count - 1 do
    begin
      M := TLyseeModule(FModules[X]);
      if MatchID(M.Name, Name) then
      begin
        Result := X;
        Exit;
      end;
    end;
  Result := -1;
end;

procedure TLyseeModuleList.Setup;
var
  I: integer;
begin
  for I := 0 to GetCount - 1 do
    GetModule(I).Setup;
end;

function TLyseeModuleList.IndexOf(AModule: TLyseeModule): integer;
begin
  Result := FModules.IndexOf(AModule);
end;

procedure TLyseeModuleList.DeleteFunctions;
var
  index: integer;
begin
  if FImporter = nil then
    for index := 0 to GetCount - 1 do
      GetModule(index).DeleteFunctions;
end;

function TLyseeModuleList.ToList: TLyseeList;
var
  index: integer;
begin
  Result := TLyseeList.Create;
  for index := 0 to GetCount - 1 do
    Result.Add.SetAsModule(GetModule(index));
end;

{ TLyseeString }

function TLyseeString.AsString: string;
begin
  Result := FValue;
end;

constructor TLyseeString.Create(const S: string);
begin
  FValue := S;
end;

constructor TLyseeString.CreateIncRefcount(const S: string);
begin
  FValue := S;
  IncRefcount;
end;

function TLyseeString.Length: integer;
begin
  if Self <> nil then
    Result := System.Length(FValue) else
    Result := 0;
end;

{ TLyseeGarbage }

procedure TLyseeGarbage.Clear;
begin

end;

constructor TLyseeGarbage.Create;
begin
  if my_gcman <> nil then my_gcman.GcAdd(Self);
end;

destructor TLyseeGarbage.Destroy;
begin
  if my_gcman <> nil then
  begin
    my_gcman.GcRemove(Self);
    my_gcman.FDead.Remove(Self);
  end;
  inherited;
end;

procedure TLyseeGarbage.MarkForSurvive;
begin
  { do nothing }
end;

{ TLyseeCollect }

function TLyseeCollect.Collect: integer;

  procedure change_ref(Increase: boolean);
  var
    I: integer;
    G: TLyseeGarbage;
  begin
    for I := 0 to FDead.Count - 1 do
      if I < FDead.Count then
      begin
        G := TLyseeGarbage(FDead[I]);
        if Increase then
          G.IncRefcount else
          G.DecRefcount;
      end;
  end;

var
  I: integer;
  G: TLyseeGarbage;
begin
  FDead.Clear;

  // clear container's marked flag
  G := FChain;
  while G <> nil do
  begin
    G.FSurvived := false;
    G := G.FNext;
  end;

  MarkSurvived;

  // list unmarked containers
  G := FChain;
  while G <> nil do
  begin
    if not G.FSurvived then FDead.Add(G);
    G := G.FNext;
  end;

  Result := FDead.Count;

  // clear all containers
  change_ref(true);
  try
    for I := FDead.Count - 1 downto 0 do
      if I < FDead.Count then
        TLyseeGarbage(FDead[I]).Clear;
  finally
    change_ref(false);
  end;

  // destroy rest containers
  for I := FDead.Count - 1 downto 0 do
    if I < FDead.Count then
    begin
      G := TLyseeGarbage(FDead[I]);
      FDead.Delete(I);
      G.Free;
    end;
end;

procedure TLyseeCollect.MarkSurvived;
var
  I: integer;
begin
  for I := 0 to FContexts.Count - 1 do
    TLysee(FContexts[I]).MarkSurvived;
end;

constructor TLyseeCollect.Create;
begin
  FContexts := TList.Create;
  FDead := TList.Create;
end;

destructor TLyseeCollect.Destroy;
begin
  FreeAndNil(FDead);
  FreeAndNil(FContexts);
  inherited;
end;

procedure TLyseeCollect.GcAdd(G: TLyseeGarbage);
begin
  if not G.FInChain then
  begin
    G.FPrev := nil;
    G.FNext := FChain;
    if FChain <> nil then
      FChain.FPrev := G;
    FChain := G;
    G.FInChain := true;
  end;
end;

procedure TLyseeCollect.GcRemove(G: TLyseeGarbage);
begin
  if G.FInChain then
  begin
    if G = FChain then
    begin
      FChain := G.FNext;
      if FChain <> nil then
        FChain.FPrev := nil;
    end
    else
    begin
      if G.FPrev <> nil then
        G.FPrev.FNext := G.FNext;
      if G.FNext <> nil then
        G.FNext.FPrev := G.FPrev;
    end;
    G.FPrev := nil;
    G.FNext := nil;
    G.FInChain := false;
  end;
end;

{ TLyseeList }

destructor TLyseeList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

procedure TLyseeList.PrepareFor(Func: TLyseeFunc);
var
  I, N: integer;
begin
  N := GetCount;
  SetCount(Func.FParams.Count);
  for I := 0 to Func.FParams.Count - 1 do
    if I < N then
      Func.FParams[I].FType.Convert(GetItem(I)) else
      Func.FParams[N].FType.SetDefault(GetItem(N));
end;

procedure TLyseeList.Remove(Value: TLyseeValue);
var
  I: integer;
begin
  I := FItems.IndexOf(Value);
  if I >= 0 then Delete(I);
end;

function TLyseeList.CopyRight(ItemCount: integer): TLyseeList;
begin
  Result := Copy(GetCount - ItemCount, ItemCount);
end;

function TLyseeList.AddList: TLyseeList;
var
  V: TLyseeValue;
begin
  Result := TLyseeList.Create;
  Result.IncRefcount;
  V := Add;
  V.FType := my_array;
  V.FValue.VObject := Result;
end;

procedure TLyseeList.AddStrings(Strs: TStrings);
var
  I: integer;
begin
  for I := 0 to Strs.Count - 1 do
    Add.AsString := Strs[I];
end;

procedure TLyseeList.Assign(AList: TLyseeList);
var
  I: integer;
begin
  Clear;
  for I := 0 to AList.Count - 1 do
    Add(AList.GetItem(I));
end;

function TLyseeList.AsString: string;
var
  X, N: integer;
begin
  if not FFormating then
  begin
    FFormating := true;
    try
      N := GetCount;
      if N > 0 then
      begin
        Result := '[' + FormatValue(GetItem(0));
        for X := 1 to N - 1 do
        begin
          Result := Result + ', ';
          Result := Result + FormatValue(GetItem(X));
        end;
        Result := Result + ']';
      end
      else Result := '[,]';
    finally
      FFormating := false;
    end;
  end
  else Result := '[,,,]';
end;

procedure TLyseeList.Clear;
var
  I: integer;
begin
  for I := GetCount - 1 downto 0 do
    Delete(I);
  inherited;
end;

function TLyseeList.Clone: TLyseeList;
begin
  Result := TLyseeList.Create;
  Result.Assign(Self);
end;

function TLyseeList.Copy(Index, ItemCount: integer): TLyseeList;
var
  I: integer;
begin
  Result := TLyseeList.Create;
  if Index < 0 then
  begin
    Inc(ItemCount, Index);
    Index := 0;
  end;
  ItemCount := Max(0, Min(GetCount - Index, ItemCount));
  for I := 1 to ItemCount do
  begin
    Result.Add(GetItem(Index));
    Inc(Index);
  end;
end;

constructor TLyseeList.Create;
begin
  inherited;
  FItems := TList.Create;
  FReadonly := false;
end;

procedure TLyseeList.Delete(Index: integer);
var
  V: TLyseeValue;
begin
  V := TLyseeValue(FItems[Index]);
  FItems.Delete(Index);
  V.Free;
end;

procedure TLyseeList.DeleteLast;
begin
  Delete(GetCount - 1);
end;

procedure TLyseeList.Exchange(Index1, Index2: integer);
begin
  FItems.Exchange(Index1, Index2);
end;

function TLyseeList.First: TLyseeValue;
begin
  Result := GetItem(0);
end;

function TLyseeList.GetCount: integer;
begin
  if Self <> nil then
    Result := FItems.Count else
    Result := 0;
end;

procedure TLyseeList.GetFirstTwo(var V1, V2: TLyseeValue);
var
  N: integer;
begin
  V1 := nil;
  V2 := nil;
  N := GetCount;
  if N > 0 then V1 := GetItem(0);
  if N > 1 then V2 := GetItem(1);
end;

function TLyseeList.GetItem(Index: integer): TLyseeValue;
begin
  Result := TLyseeValue(FItems[Index]);
end;

function TLyseeList.IndexOf(Value: TLyseeValue): integer;
var
  I: integer;
begin
  if Value <> nil then
    for I := 0 to GetCount - 1 do
      if Value.Same(GetItem(I)) then
      begin
        Result := I;
        Exit;
      end;
  Result := -1;
end;

function TLyseeList.Insert(Index: integer; Value: TLyseeValue): TLyseeValue;
begin
  Result := TLyseeValue.Create;
  FItems.Insert(Index, Result);
  if Value <> nil then
    Result.SetValue(Value);
end;

function TLyseeList.Last: TLyseeValue;
begin
  Result := GetItem(GetCount - 1);
end;

function TLyseeList.CopyLeft(ItemCount: integer): TLyseeList;
begin
  Result := Copy(0, ItemCount);
end;

procedure TLyseeList.MarkForSurvive;
var
  I: integer;
begin
  if (Self <> nil) and not FSurvived then
  begin
    FSurvived := true;
    for I := 0 to GetCount - 1 do
      GetItem(I).MarkForSurvive;
  end;
end;

procedure TLyseeList.Move(CurIndex, NewIndex: integer);
begin
  FItems.Move(CurIndex, NewIndex);
end;

function TLyseeList.Add(Value: TLyseeValue): TLyseeValue;
begin
  Result := TLyseeValue.Create;
  FItems.Add(Result);
  if Value <> nil then
    Result.SetValue(Value);
end;

procedure TLyseeList.SetCount(NewCount: integer);
begin
  if NewCount < 1 then Clear else
  begin
    while NewCount > GetCount do Add;
    while NewCount < GetCount do DeleteLast;
  end;
end;

procedure TLyseeList.Sort;
begin
  FItems.Sort(@SortValueCompare);
end;

procedure TLyseeList.TestChange;
begin
  if FReadonly then
    raise Exception.Create('array/list is readonly');
end;

{ TLyseeHashItem }

function TLyseeHashItem.Format: string;
begin
  Result := FormatString(FKey) + ':' + FormatValue(Self);
end;

{ TLyseeHash }

function TLyseeHash.Add(const Name: string): TLyseeHashItem;
var
  I: integer;
begin
  Result := Get(Name);
  if Result = nil then
  begin
    Result := TLyseeHashItem.Create;
    Result.FKey := Name;
    I := HashIndex(Result.FKey);
    Result.FNext := FBuckets[I];
    FBuckets[I] := Result;
    Inc(FCount);
    Resize(FCount div LSE_HASH_DELTA);
  end;
end;

function TLyseeHash.AsString: string;
var
  H: TLyseeHashItem;
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
          H := TLyseeHashItem(L.First);
          Result := '[' + H.Format;
          for I := 1 to L.Count - 1 do
          begin
            H := TLyseeHashItem(L[I]);
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

function TLyseeHash.DefConst(const Name, Value: string): TLyseeHashItem;
begin
  Result := Add(Name);
  Result.AsString := Value;
end;

function TLyseeHash.DefConst(const Name: string; Value: int64): TLyseeHashItem;
begin
  Result := Add(Name);
  Result.AsInteger := Value;
end;

function TLyseeHash.DefConst(const Name: string; Value: double): TLyseeHashItem;
begin
  Result := Add(Name);
  Result.AsFloat := Value;
end;

function TLyseeHash.DefConst(const Name: string; Value: boolean): TLyseeHashItem;
begin
  Result := Add(Name);
  Result.SetAsBoolean(Value);
end;

function TLyseeHash.BucketCount: integer;
begin
  Result := Length(FBuckets);
end;

procedure TLyseeHash.Clear;
var
  I: integer;
  H: TLyseeHashItem;
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

constructor TLyseeHash.Create;
begin
  inherited;
  SetLength(FBuckets, 1);
  FBuckets[0] := nil;
end;

destructor TLyseeHash.Destroy;
begin
  Clear;
  SetLength(FBuckets, 0);
  inherited;
end;

procedure TLyseeHash.FreeItem(H: TLyseeHashItem);
begin
  Dec(FCount);
  H.Free;
end;

function TLyseeHash.Has(const Name: string): boolean;
begin
  Result := (Get(Name) <> nil);
end;

function TLyseeHash.HashIndex(const Name: string): integer;
var
  N: integer;
begin
  N := BucketCount;
  if N = 1 then Result := 0 else
  if FCaseSensitive then
    Result := (basic.HashOf(Name) mod N) else
    Result := (basic.HashOf(LowerCase(Name)) mod N);
end;

function TLyseeHash.IsEmpty: boolean;
begin
  Result := (FCount = 0);
end;

function TLyseeHash.Get(const Name: string): TLyseeHashItem;
begin
  Result := FBuckets[HashIndex(Name)];
  while Result <> nil do
  begin
    if MatchName(Name, Result.FKey) then Exit;
    Result := Result.FNext;
  end;
end;

function TLyseeHash.GetValue(const Name: string; Value: TLyseeValue): boolean;
var
  V: TLyseeValue;
begin
  V := Get(Name);
  Result := (V <> nil);
  if Result and (Value <> nil) then
    Value.SetValue(V);
end;

procedure TLyseeHash.ListKeys(List: TLyseeList);
var
  I: integer;
  L: TList;
begin
  if Self <> nil then
  begin
    L := SetupItemList;
    try
      for I := 0 to L.Count - 1 do
        List.Add.SetAsString(TLyseeHashItem(L[I]).FKey);
    finally
      L.Free;
    end;
  end;
end;

procedure TLyseeHash.ListValues(List: TLyseeList);
var
  I: integer;
  L: TList;
begin
  if Self <> nil then
  begin
    L := SetupItemList;
    try
      for I := 0 to L.Count - 1 do
        List.Add(TLyseeHashItem(L[I]));
    finally
      L.Free;
    end;
  end;
end;

procedure TLyseeHash.MarkForSurvive;
var
  H: TLyseeHashItem;
  I: integer;
begin
  if (Self <> nil) and not FSurvived then
  begin
    FSurvived := true;
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

function TLyseeHash.MatchName(const N1, N2: string): boolean;
begin
  if FCaseSensitive then
    Result := (N1 = N2) else
    Result := SysUtils.SameText(N1, N2);
end;

function TLyseeHash.Remove(const Name: string): boolean;
var
  X: cardinal;
  H, T: TLyseeHashItem;
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

procedure TLyseeHash.Resize(NewSize: integer);
var
  L: TList;
  I, X: integer;
  H: TLyseeHashItem;
begin
  if NewSize > 512 then NewSize := 512;
  X := BucketCount;
  if NewSize > X then
  begin
    L := SetupItemList;
    try
      while X < NewSize do X := X shl 1;
      SetLength(FBuckets, X);
      FillChar(FBuckets[0], sizeof(TLyseeHashItem) * X, 0);
      for I := 0 to L.Count - 1 do
      begin
        H := TLyseeHashItem(L[I]);
        X := HashIndex(H.FKey);
        H.FNext := FBuckets[X];
        FBuckets[X] := H;
      end;
    finally
      L.Free;
    end;
  end;
end;

function TLyseeHash.SetupItemList: TList;
var
  I: integer;
  H: TLyseeHashItem;
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

{ TLyseeGenerate }

function TLyseeGenerate.GetNext: boolean;
begin
  Result := false;
end;

function TLyseeGenerate.HasNext: boolean;
begin
  Result := false;
end;

{ TLyseeStringGenerate }

constructor TLyseeStringGenerate.CreateIn(const S: string);
begin
  inherited Create;
  FS := S;
  FIndex := 0;
end;

function TLyseeStringGenerate.GetNext: boolean;
begin
  Result := HasNext;
  if Result then
  begin
    Inc(FIndex);
    SetAsChar(FS[FIndex]);
  end
  else SetNil;
end;

function TLyseeStringGenerate.HasNext: boolean;
begin
  Result := (FIndex < Length(FS));
end;

{ TLyseeListGenerate }

constructor TLyseeListGenerate.CreateIn(const AList: TLyseeList);
begin
  inherited Create;
  FL := AList;
  FIndex := -1;
end;

function TLyseeListGenerate.GetNext: boolean;
begin
  Result := HasNext;
  if Result then
  begin
    Inc(FIndex);
    SetValue(FL[FIndex]);
  end
  else SetNil;
end;

function TLyseeListGenerate.HasNext: boolean;
begin
  Result := (FL <> nil) and (FIndex < FL.Count - 1);
end;

{ TLyseeIntGenerate }

constructor TLyseeIntGenerate.CreateIn(V1, V2: int64; Upto: boolean);
begin
  inherited Create;
  FV := V1;
  FUpto := Upto;
  if FUpto then
    FCount := V2 - FV + 1 else
    FCount := FV - V2 + 1;
end;

constructor TLyseeIntGenerate.CreateIn(Range: int64);
begin
  CreateIn(0, Range - 1, true);
end;

function TLyseeIntGenerate.GetNext: boolean;
begin
  Result := HasNext;
  if Result then
  begin
    SetAsInteger(FV);
    if FUpto then Inc(FV) else Dec(FV);
    Dec(FCount);
  end
  else SetNil;
end;

function TLyseeIntGenerate.HasNext: boolean;
begin
  Result := (FCount > 0);
end;

{ TLyseeCharGenerate }

constructor TLyseeCharGenerate.CreateIn(V1, V2: char; Upto: boolean);
begin
  inherited Create;
  FV := V1;
  FUpto := Upto;
  if FUpto then
    FCount := Ord(V2) - Ord(FV) + 1 else
    FCount := Ord(FV) - Ord(V2) + 1;
end;

function TLyseeCharGenerate.GetNext: boolean;
begin
  Result := HasNext;
  if Result then
  begin
    SetAsChar(FV);
    if FUpto then Inc(FV) else Dec(FV);
    Dec(FCount);
  end
  else SetNil;
end;

function TLyseeCharGenerate.HasNext: boolean;
begin
  Result := (FCount > 0);
end;

{ TLyseeBoolGenerate }

constructor TLyseeBoolGenerate.CreateIn(V1, V2, Upto: boolean);
begin
  inherited Create;
  FV := V1;
  FUpto := Upto;
  if FUpto then
    FCount := Ord(V2) - Ord(FV) + 1 else
    FCount := Ord(FV) - Ord(V2) + 1;
end;

function TLyseeBoolGenerate.GetNext: boolean;
begin
  Result := HasNext;
  if Result then
  begin
    SetAsBoolean(FV);
    FV := not FV;
    Dec(FCount);
  end
  else SetNil;
end;

function TLyseeBoolGenerate.HasNext: boolean;
begin
  Result := (FCount > 0);
end;

{ TLyseeEnumGenerate }

constructor TLyseeEnumGenerate.CreateIn(V1, V2: TLyseeEnumItem; Upto: boolean);
begin
  inherited Create;
  FV := V1;
  FUpto := Upto;
  if (V1 = nil) or (V2 = nil) or (V1.FParent <> V2.FParent) then FCount := 0 else
  if FUpto then
    FCount := V2.FValue - FV.FValue + 1 else
    FCount := FV.FValue - V2.FValue + 1;
end;

function TLyseeEnumGenerate.GetNext: boolean;
begin
  Result := HasNext;
  if Result then
  begin
    FV.SetValue(Self);
    if FUpto then
      FV := FV.FParent.Find(FV.FValue + 1) else
      FV := FV.FParent.Find(FV.FValue - 1);
    Dec(FCount);
  end
  else SetNil;
end;

function TLyseeEnumGenerate.HasNext: boolean;
begin
  Result := (FV <> nil) and (FCount > 0);
end;

{ TLyseeSystemModule }

procedure TLyseeSystemModule.DoSetup(Sender: TObject);
var
  I: integer;
begin
  OnSetup := nil;
  for I := 0 to TypeCount - 1 do
    GetType(I).Setup;
end;

constructor TLyseeSystemModule.Create(const AName: string);
begin
  inherited;
  OnSetup := {$IFDEF FPC}@{$ENDIF}DoSetup;

  //-- types -----------------------------------------------------------------

  my_variant := TLyseeVariantType.Create(TID_NAMES[TID_VARIANT], Self, nil);
  my_variant.FStyle := tsVariant;
  my_variant.FTID := TID_VARIANT;
  my_types[TID_VARIANT] := my_variant;

  my_nil := TLyseeNilType.Create(TID_NAMES[TID_NIL], Self, nil);
  my_nil.FStyle := tsNil;
  my_nil.FTID := TID_NIL;
  my_types[TID_NIL] := my_nil;

  my_char := TLyseeCharType.Create(TID_NAMES[TID_CHAR], Self, nil);
  my_char.FStyle := tsBasic;
  my_char.FTID := TID_CHAR;
  my_types[TID_CHAR] := my_char;

  my_int := TLyseeIntegerType.Create(TID_NAMES[TID_INTEGER], Self, nil);
  my_int.FStyle := tsBasic;
  my_int.FTID := TID_INTEGER;
  my_types[TID_INTEGER] := my_int;

  my_float := TLyseeFloatType.Create(TID_NAMES[TID_FLOAT], Self, nil);
  my_float.FStyle := tsBasic;
  my_float.FTID := TID_FLOAT;
  my_types[TID_FLOAT] := my_float;

  my_curr := TLyseeCurrencyType.Create(TID_NAMES[TID_CURRENCY], Self, nil);
  my_curr.FStyle := tsBasic;
  my_curr.FTID := TID_CURRENCY;
  my_types[TID_CURRENCY] := my_curr;

  my_time := TLyseeTimeType.Create(TID_NAMES[TID_time], Self, nil);
  my_time.FStyle := tsBasic;
  my_time.FTID := TID_TIME;
  my_types[TID_time] := my_time;

  my_bool := TLyseeBoolType.Create(TID_NAMES[TID_BOOLEAN], Self, nil);
  my_bool.FStyle := tsBasic;
  my_bool.FTID := TID_BOOLEAN;
  my_types[TID_BOOLEAN] := my_bool;

  my_array := TLyseeArrayType.Create(TID_NAMES[TID_ARRAY], Self, nil);
  my_array.FTID := TID_ARRAY;
  my_types[TID_ARRAY] := my_array;

  my_hash := TLyseeHashType.Create(TID_NAMES[TID_HASHLIST], Self, nil);
  my_hash.FTID := TID_HASHLIST;
  my_types[TID_HASHLIST] := my_hash;

  my_string := TLyseeStringType.Create(TID_NAMES[TID_STRING], Self, nil);
  my_string.FTID := TID_STRING;
  my_types[TID_STRING] := my_string;

  my_type := TLyseeTypeType.Create(TID_NAMES[TID_TYPE], Self, nil);
  my_type.FTID := TID_TYPE;
  my_types[TID_TYPE] := my_type;

  my_exception := TLyseeExceptionType.Create(TID_NAMES[TID_EXCEPTION], Self, nil);
  my_exception.FTID := TID_EXCEPTION;
  my_types[TID_EXCEPTION] := my_exception;

  my_module := TLyseeModuleType.Create(TID_NAMES[TID_MODULE], Self, nil);
  my_module.FTID := TID_MODULE;
  my_types[TID_MODULE] := my_module;

  my_func := TLyseeFuncType.Create(TID_NAMES[TID_FUNCTION], Self, nil);
  my_func.FTID := TID_FUNCTION;
  my_types[TID_FUNCTION] := my_func;

  my_TID_seed := 100;

  //-- constant --------------------------------------------------------------

  FConsts.Add('MaxInt').SetAsInteger(High(int64));
  FConsts.Add('MinInt').SetAsInteger(Low(int64));
  FConsts.Add('PathDelim').SetAsChar(PathDelim);
  FConsts.Add('PathSep').SetAsChar(PathSep);
  FConsts.Add('PI').SetAsFloat(PI);
  FConsts.Add('PointerSize').SetAsInteger(sizeof(pointer));
  FConsts.Add('sLineBreak').SetAsString(sLineBreak);

  //-- operator --------------------------------------------------------------

  my_variant.AddOperate(opAdd, my_string, {$IFDEF FPC}@{$ENDIF}string_add_string);
  my_variant.AddOperate(opAdd, my_char, {$IFDEF FPC}@{$ENDIF}string_add_string);
  my_variant.AddOperate(opIs, my_type, {$IFDEF FPC}@{$ENDIF}variant_is_type);
  my_variant.AddOperate(opAs, my_type, {$IFDEF FPC}@{$ENDIF}variant_as_type);
  my_variant.AddOperate(opShi, my_variant, {$IFDEF FPC}@{$ENDIF}variant_shi_variant);
  my_variant.AddOperate(opFill, my_variant, {$IFDEF FPC}@{$ENDIF}variant_fill_variant);

  my_string.AddOperate(opAdd, my_variant, {$IFDEF FPC}@{$ENDIF}string_add_string);
  my_string.AddOperate(opMul, my_int, {$IFDEF FPC}@{$ENDIF}string_mul_int);
  my_string.AddOperate(opIn, my_string, {$IFDEF FPC}@{$ENDIF}string_in_string);
  my_string.AddOperate(opIn, my_hash, {$IFDEF FPC}@{$ENDIF}string_in_hashed);
  my_string.AddOperate(opLike, my_string, {$IFDEF FPC}@{$ENDIF}string_like_string);
  my_string.AddOperate(opLike, my_char, {$IFDEF FPC}@{$ENDIF}string_like_string);

  my_char.AddOperate(opAdd, my_variant, {$IFDEF FPC}@{$ENDIF}string_add_string);
  my_char.AddOperate(opMul, my_int, {$IFDEF FPC}@{$ENDIF}string_mul_int);
  my_char.AddOperate(opIn, my_string, {$IFDEF FPC}@{$ENDIF}string_in_string);
  my_char.AddOperate(opIn, my_hash, {$IFDEF FPC}@{$ENDIF}string_in_hashed);
  my_char.AddOperate(opLike, my_string, {$IFDEF FPC}@{$ENDIF}string_like_string);
  my_char.AddOperate(opLike, my_char, {$IFDEF FPC}@{$ENDIF}string_like_string);

  my_int.AddOperate(opNeg, my_int, {$IFDEF FPC}@{$ENDIF}int_neg_int);
  my_int.AddOperate(opNot, my_int, {$IFDEF FPC}@{$ENDIF}int_not_int);
  my_int.AddOperate(opAdd, my_int, {$IFDEF FPC}@{$ENDIF}int_add_int);
  my_int.AddOperate(opAdd, my_float, {$IFDEF FPC}@{$ENDIF}int_add_float);
  my_int.AddOperate(opAdd, my_curr, {$IFDEF FPC}@{$ENDIF}int_add_money);
  my_int.AddOperate(opDec, my_int, {$IFDEF FPC}@{$ENDIF}int_dec_int);
  my_int.AddOperate(opDec, my_float, {$IFDEF FPC}@{$ENDIF}int_dec_float);
  my_int.AddOperate(opDec, my_curr, {$IFDEF FPC}@{$ENDIF}int_dec_money);
  my_int.AddOperate(opMul, my_int, {$IFDEF FPC}@{$ENDIF}int_mul_int);
  my_int.AddOperate(opMul, my_float, {$IFDEF FPC}@{$ENDIF}int_mul_float);
  my_int.AddOperate(opMul, my_curr, {$IFDEF FPC}@{$ENDIF}int_mul_money);
  my_int.AddOperate(opDiv, my_int, {$IFDEF FPC}@{$ENDIF}int_div_int);
  my_int.AddOperate(opDivf, my_int, {$IFDEF FPC}@{$ENDIF}int_divf_int);
  my_int.AddOperate(opDivf, my_float, {$IFDEF FPC}@{$ENDIF}int_divf_float);
  my_int.AddOperate(opDivf, my_curr, {$IFDEF FPC}@{$ENDIF}int_divf_money);
  my_int.AddOperate(opMod, my_int, {$IFDEF FPC}@{$ENDIF}int_mod_int);
  my_int.AddOperate(opXor, my_int, {$IFDEF FPC}@{$ENDIF}int_xor_int);
  my_int.AddOperate(opShl, my_int, {$IFDEF FPC}@{$ENDIF}int_shl_int);
  my_int.AddOperate(opShr, my_int, {$IFDEF FPC}@{$ENDIF}int_shr_int);
  my_int.AddOperate(opIn, my_int, {$IFDEF FPC}@{$ENDIF}int_in_int);

  my_float.AddOperate(opNeg, my_float, {$IFDEF FPC}@{$ENDIF}float_neg_float);
  my_float.AddOperate(opAdd, my_float, {$IFDEF FPC}@{$ENDIF}float_add_float);
  my_float.AddOperate(opAdd, my_int, {$IFDEF FPC}@{$ENDIF}float_add_int);
  my_float.AddOperate(opAdd, my_curr, {$IFDEF FPC}@{$ENDIF}float_add_money);
  my_float.AddOperate(opDec, my_float, {$IFDEF FPC}@{$ENDIF}float_dec_float);
  my_float.AddOperate(opDec, my_int, {$IFDEF FPC}@{$ENDIF}float_dec_int);
  my_float.AddOperate(opDec, my_curr, {$IFDEF FPC}@{$ENDIF}float_dec_money);
  my_float.AddOperate(opMul, my_float, {$IFDEF FPC}@{$ENDIF}float_mul_float);
  my_float.AddOperate(opMul, my_int, {$IFDEF FPC}@{$ENDIF}float_mul_int);
  my_float.AddOperate(opMul, my_curr, {$IFDEF FPC}@{$ENDIF}float_mul_money);
  my_float.AddOperate(opDivf, my_float, {$IFDEF FPC}@{$ENDIF}float_divf_float);
  my_float.AddOperate(opDivf, my_int, {$IFDEF FPC}@{$ENDIF}float_divf_int);
  my_float.AddOperate(opDivf, my_curr, {$IFDEF FPC}@{$ENDIF}float_divf_money);

  my_curr.AddOperate(opNeg, my_curr, {$IFDEF FPC}@{$ENDIF}money_neg_money);
  my_curr.AddOperate(opAdd, my_curr, {$IFDEF FPC}@{$ENDIF}money_add_money);
  my_curr.AddOperate(opAdd, my_int, {$IFDEF FPC}@{$ENDIF}money_add_int);
  my_curr.AddOperate(opAdd, my_float, {$IFDEF FPC}@{$ENDIF}money_add_float);
  my_curr.AddOperate(opDec, my_curr, {$IFDEF FPC}@{$ENDIF}money_dec_money);
  my_curr.AddOperate(opDec, my_int, {$IFDEF FPC}@{$ENDIF}money_dec_int);
  my_curr.AddOperate(opDec, my_float, {$IFDEF FPC}@{$ENDIF}money_dec_float);
  my_curr.AddOperate(opMul, my_curr, {$IFDEF FPC}@{$ENDIF}money_mul_money);
  my_curr.AddOperate(opMul, my_int, {$IFDEF FPC}@{$ENDIF}money_mul_int);
  my_curr.AddOperate(opMul, my_float, {$IFDEF FPC}@{$ENDIF}money_mul_float);
  my_curr.AddOperate(opDivf, my_curr, {$IFDEF FPC}@{$ENDIF}money_divf_money);
  my_curr.AddOperate(opDivf, my_int, {$IFDEF FPC}@{$ENDIF}money_divf_int);
  my_curr.AddOperate(opDivf, my_float, {$IFDEF FPC}@{$ENDIF}money_divf_float);

  my_time.AddOperate(opAdd, my_int, {$IFDEF FPC}@{$ENDIF}time_add_int);
  my_time.AddOperate(opAdd, my_float, {$IFDEF FPC}@{$ENDIF}time_add_float);
  my_time.AddOperate(opDec, my_int, {$IFDEF FPC}@{$ENDIF}time_dec_int);
  my_time.AddOperate(opDec, my_float, {$IFDEF FPC}@{$ENDIF}time_dec_float);

  my_bool.AddOperate(opNot, my_bool, {$IFDEF FPC}@{$ENDIF}bool_not_bool);
  my_bool.AddOperate(opXor, my_bool, {$IFDEF FPC}@{$ENDIF}bool_xor_bool);

  //-- compare --------------------------------------------------------------

  my_variant.AddCompare(my_variant, {$IFDEF FPC}@{$ENDIF}compare_variant_variant);
  my_variant.AddCompare(my_nil, {$IFDEF FPC}@{$ENDIF}compare_variant_nil);
  my_nil.AddCompare(my_variant, {$IFDEF FPC}@{$ENDIF}compare_nil_variant);
  my_string.AddCompare(my_string, {$IFDEF FPC}@{$ENDIF}compare_string_string);
  my_string.AddCompare(my_char, {$IFDEF FPC}@{$ENDIF}compare_string_string);
  my_char.AddCompare(my_char, {$IFDEF FPC}@{$ENDIF}compare_char_char);
  my_char.AddCompare(my_string, {$IFDEF FPC}@{$ENDIF}compare_string_string);
  my_int.AddCompare(my_int, {$IFDEF FPC}@{$ENDIF}compare_int_int);
  my_int.AddCompare(my_float, {$IFDEF FPC}@{$ENDIF}compare_int_float);
  my_int.AddCompare(my_curr, {$IFDEF FPC}@{$ENDIF}compare_int_money);
  my_int.AddCompare(my_time, {$IFDEF FPC}@{$ENDIF}compare_int_time);
  my_float.AddCompare(my_float, {$IFDEF FPC}@{$ENDIF}compare_float_float);
  my_float.AddCompare(my_int, {$IFDEF FPC}@{$ENDIF}compare_float_int);
  my_float.AddCompare(my_curr, {$IFDEF FPC}@{$ENDIF}compare_float_money);
  my_float.AddCompare(my_time, {$IFDEF FPC}@{$ENDIF}compare_float_time);
  my_curr.AddCompare(my_curr, {$IFDEF FPC}@{$ENDIF}compare_money_money);
  my_curr.AddCompare(my_int, {$IFDEF FPC}@{$ENDIF}compare_money_int);
  my_curr.AddCompare(my_float, {$IFDEF FPC}@{$ENDIF}compare_money_float);
  my_time.AddCompare(my_time, {$IFDEF FPC}@{$ENDIF}compare_time_time);
  my_time.AddCompare(my_int, {$IFDEF FPC}@{$ENDIF}compare_time_int);
  my_time.AddCompare(my_float, {$IFDEF FPC}@{$ENDIF}compare_time_float);
  my_bool.AddCompare(my_bool, {$IFDEF FPC}@{$ENDIF}compare_bool_bool);

  //-- functions -------------------------------------------------------------

  AddFunc('ExceptObject', my_exception, {$IFDEF FPC}@{$ENDIF}pp_exceptObject);
  AddFunc('Halt', ['_Value'], [my_variant], {$IFDEF FPC}@{$ENDIF}pp_halt);
  AddFunc('Exit', ['_Value'], [my_variant], {$IFDEF FPC}@{$ENDIF}pp_exit);
  AddFunc('Break', {$IFDEF FPC}@{$ENDIF}pp_break);
  AddFunc('Continue', {$IFDEF FPC}@{$ENDIF}pp_continue);
  AddFunc('Compile', my_func, ['Code', '_Args'], [my_string, my_string],
          {$IFDEF FPC}@{$ENDIF}pp_compile);
  AddFunc('Eval', my_variant, ['Expression'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_eval);
  AddFunc('Find', my_variant, ['Name'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_find);
  AddFunc('TypeOf', my_type, ['Any'], [my_variant],
          {$IFDEF FPC}@{$ENDIF}pp_typeof);
  AddFunc('ModuleOf', my_module, ['Any'], [my_variant],
          {$IFDEF FPC}@{$ENDIF}pp_moduleof);
  AddFunc('FileOf', my_string, ['Any'], [my_variant],
          {$IFDEF FPC}@{$ENDIF}pp_fileof);
  AddFunc('CollectGarbage', {$IFDEF FPC}@{$ENDIF}pp_collectGarbage);
  AddFunc('Inc', ['Varb', '_Value'], [my_variant, my_int],
          {$IFDEF FPC}@{$ENDIF}pp_inc);
  AddFunc('Dec', ['Varb', '_Value'], [my_variant, my_int],
          {$IFDEF FPC}@{$ENDIF}pp_dec);
  AddFunc('Readln', my_string, ['_Varb'], [my_variant],
          {$IFDEF FPC}@{$ENDIF}pp_readln);
  AddFunc('Write', ['_S'], [my_string], {$IFDEF FPC}@{$ENDIF}pp_write);
  AddFunc('Writeln', ['_S'], [my_string], {$IFDEF FPC}@{$ENDIF}pp_writeln);
  AddFunc('Length', my_int, ['Any'], [my_variant],
          {$IFDEF FPC}@{$ENDIF}pp_length);
  AddFunc('Pass', {$IFDEF FPC}@{$ENDIF}pp_pass);
  AddFunc('Decompile', my_string, ['Any'], [my_variant],
          {$IFDEF FPC}@{$ENDIF}pp_decompile);
end;

initialization
begin
  Randomize;
  my_program := ParamStr(0);
  my_kernel := my_program;
  {$IFDEF MSWINDOWS}
  my_home := FullPath(GetEnv('HOMEDRIVER') + GetEnv('HOMEPATH'));
  {$ELSE}
  my_home := FullPath(GetEnv('HOME'));
  {$ENDIF}
  my_knpath := ExtractFilePath(my_kernel);
  my_kndir := ExcPD(my_knpath);
  my_search_path := my_knpath + 'modules';
  {$IFDEF MSWINDOWS}
  my_tmpath := GetEnv(['TEMP', 'TMP'], my_knpath + '..\temp\');
  {$ELSE}
  my_tmpath := GetEnv(['TEMP', 'TMP'], '/tmp/');
  {$ENDIF}
  my_spinlock := Syncobjs.TCriticalSection.Create;
  my_gcman := TLyseeCollect.Create;
  my_modules := TLyseeModuleList.Create(nil);
  my_system := TLyseeSystemModule.Create(LSE_SYSTE);
end;

finalization
try
  my_modules.Clear;
  FreeAndNil(my_modules);
  FreeAndNil(my_spinlock);
  FreeAndNil(my_gcman);
except
  { safe & quiet }
end;

end.

