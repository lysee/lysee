{==============================================================================}
{        UNIT: lysee                                                           }
{ DESCRIPTION: lysee script interpreter                                        }
{   COPYRIGHT: Copyright (c) 2003-2015, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/02/28                                                      }
{    MODIFIED: 2017/01/12                                                      }
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
  LSE_VERSION  = '2017.1.7';
  LSE_FILEEXT  = '.ls';
  LSE_CONFILE  = 'lysee.conf';
  LSE_SYSTE    = 'system';
  LSE_MAIN     = 'main';

  { TID: Lysee Type IDentity }

  TID_VARIANT  = 0;
  TID_NIL      = 1;
  TID_CHAR     = 2; { basic }
  TID_INTEGER  = 3;
  TID_FLOAT    = 4;
  TID_CURRENCY = 5;
  TID_TIME     = 6;
  TID_BOOLEAN  = 7;
  TID_TYPE     = 8; { object }
  TID_STRING   = 9;
  TID_MODULE   = 10;
  TID_FUNCTION = 11;
  TID_HASHLIST = 12;
  TID_LIST     = 13;

  TID_NAMES: array[TID_VARIANT..TID_LIST] of string = (
    'variant', 'nil', 'char', 'integer', 'float', 'currency',
    'time', 'boolean', 'type', 'string', 'module', 'function',
    'THash', 'TList');

type

  { forward }

  TLiError          = class;
  TLiContext        = class;{<--lysee script executer}
  TLiType           = class;
  TLiEnumItem       = class;
  TLiEnumType       = class;
  TLiEnumSet        = class;
  TLiEnumSetType    = class;
  TLiValue          = class;
  TLiParam          = class;
  TLiToken          = class;
  TLiTokenList      = class;
  TLiTokenizer      = class;
  TLiParser         = class;
  TLiSTMT           = class;
  TLiSTMT_assign    = class;
  TLiSTMT_for       = class;
  TLiSTMT_if        = class;
  TLiSTMT_case      = class;
  TLiSTMT_try       = class;
  TLiSTMTList       = class;
  TLiVarb           = class;
  TLiVarbList       = class;
  TLiFunc           = class;
  TLiFuncList       = class;
  TLiModule         = class;
  TLiModuleList     = class;
  TLiString         = class;
  TLiGarbage        = class;
  TLiGarbageCollect = class;
  TLiList           = class;
  TLiHash           = class;
  TLiGenerate       = class;

  { lysee procedure }

  TLiLyseeProc = procedure(const Param: TLiParam);
  TLiLyseeObjectProc = procedure(const Param: TLiParam) of object;

  { RLiFind }

  TLiFind = (fiNone, fiVarb, fiFunc, fiType, fiModule, fiValue);
  TLiFinds = set of TLiFind;

  RLiFind = packed record
    case f_find: TLiFind of
    fiNone  :(VNone: pointer);
    fiVarb  :(VVarb: TLiVarb);
    fiFunc  :(VFunc: TLiFunc);
    fiType  :(VType: TLiType);
    fiModule:(VModule: TLiModule);
    fiValue :(VValue: TLiValue);   // constant
  end;
  PLiFind = ^RLiFind;

  { TLiError }

  TLiError = class
  private
    FContext: TLiContext;
    FErrID: string;
    FMsg: string;
    FModule: string;
    FFileName: string;
    FRow: integer;
    FCol: integer;
    function GetText: string;
  public
    constructor Create(AContext: TLiContext);
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

  { TLiContext }

  TLiReadln = procedure(Sender: TObject; var S: string) of object;
  TLiWrite = procedure(Sender: TObject; const S: string) of object;
  TLiResolve = function(const ID: string; Value: TLiValue): boolean of object;
  TLiGetModuleFile = function(Sender: TObject; const Module: string): string of object;

  TLiContextState = (csTerminated, csReady, csRunning, csContinue, csBreak, csExit);

  TLiContext = class(TComponent)
  private
    FMainFile: string;
    FError: TLiError;
    FState: TLiContextState;
    FTerminated: boolean;
    FExcepted: boolean;
    FHalted: boolean;
    FResult: TLiValue;
    FMainModule: TLiModule;
    FMainFunc: TLiFunc;
    FMainLocals: TLiList;
    FMainToken: TLiToken;
    FArgs: TLiList;
    FModules: TLiModuleList;
    FRollbacks: TList;
    FNameSeed: cardinal;
    FCurrent: TLiParam;
    FOnExecuting: TNotifyEvent;
    FOnExecuted: TNotifyEvent;
    FOnReadln: TLiReadln;
    FOnWrite: TLiWrite;
    FOnResolve: TLiResolve;
    FOnGetModuleFile: TLiGetModuleFile;
    procedure SetMainFile(const Value: string);
    procedure SetResult(Value: TLiValue);
    function GetMainFunc: TLiFunc;
    function GetCodeModule: TLiModule;
    function GetCodeFunc: TLiFunc;
    function GetReady: boolean;
    function GetRunning: boolean;
    function GetTerminated: boolean;
    function GetTerminating: boolean;
    function GetMainModule: TLiModule;
    function GetMainToken: TLiToken;
    function Compile(const Code: string): TLiFunc;
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
    function Resolve(const ID: string; Value: TLiValue): boolean;virtual;
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
    property Error: TLiError read FError;
    property Ready: boolean read GetReady;
    property Running: boolean read GetRunning;
    property Terminating: boolean read GetTerminating;
    property Terminated: boolean read GetTerminated;
    property Halted: boolean read FHalted;
    property Excepted: boolean read FExcepted write FExcepted;
    property Modules: TLiModuleList read FModules;
    property MainFile: string read FMainFile write SetMainFile;
    property MainModule: TLiModule read GetMainModule;
    property MainFunc: TLiFunc read GetMainFunc;
    property MainToken: TLiToken read GetMainToken;
    property Current: TLiParam read FCurrent;
    property CodeModule: TLiModule read GetCodeModule;
    property CodeFunc: TLiFunc read GetCodeFunc;
    property Stack: TLiList read FMainLocals;
    property Result: TLiValue read FResult write SetResult;
    property Args: TLiList read FArgs;
    property Rollbacks: TList read FRollbacks write FRollbacks;
    property OnExecuting: TNotifyEvent read FOnExecuting write FOnExecuting;
    property OnExecuted: TNotifyEvent read FOnExecuted write FOnExecuted;
    property OnReadln: TLiReadln read FOnReadln write FOnReadln;
    property OnWrite: TLiWrite read FOnWrite write FOnWrite;
    property OnResolve: TLiResolve read FOnResolve write FOnResolve;
    property OnGetModuleFile: TLiGetModuleFile read FOnGetModuleFile write FOnGetModuleFile;
  end;

  { TLiType }

  TLiOperator = (opAdd, opDec, opMul, opDiv, opDivf, opMod, opXor, opShl,
                 opShr, opShi, opFill, opIn, opLike, opIs, opAs,
                 {single}opNot, opNeg);

  TLiOperateProc = procedure(V1, V2: TLiValue);
  PLiOperate = ^RLiOperate;
  RLiOperate = packed record
    o_oper: TLiOperator;
    o_type: TLiType;
    o_operate: TLiOperateProc;
    o_next: PLiOperate;
  end;

  TLiCompareFunc = function(V1, V2: TLiValue): TLiCompare;
  PLiCompare = ^RLiCompare;
  RLiCompare = packed record
    c_type: TLiType;
    c_compare: TLiCompareFunc;
    c_next: PLiCompare;
  end;

  TLiConvertProc = procedure(Value: TLiValue);
  PLiConvert = ^RLiConvert;
  RLiConvert = packed record
    c_type: TLiType;
    c_convert: TLiConvertProc;
    c_next: PLiConvert;
  end;

  TLiTypeStyle = (tsVariant, tsNil, tsBasic, tsEnum, tsEnumSet, tsObject);

  TLiType = class
  private
    FModule: TLiModule;
    FParent: TLiType;
    FName: string;
    FTID: integer;
    FStyle: TLiTypeStyle;
    FMethods : TLiFuncList;
    FConstructer: TLiFunc;
    FOperate: array[TLiOperator] of PLiOperate;
    FCompare : PLiCompare;
    FConvert: PLiConvert;
    FProcs: array of TLiLyseeObjectProc;
    function GetFullName: string;
    function GetMethodCount: integer;
    function GetMethod(Index: integer): TLiFunc;
    procedure SetParent(const Value: TLiType);
  protected
    function RegisterProc(AProc: TLiLyseeObjectProc): integer;
    procedure Setup;virtual;
  public
    function _IncRefcount(Obj: pointer): integer;virtual;
    function _DecRefcount(Obj: pointer): integer;virtual;
    function _AsString(Obj: pointer): string;virtual;
    function _AsChar(Obj: pointer): char;virtual;
    function _AsInteger(Obj: pointer): int64;virtual;
    function _AsFloat(Obj: pointer): double;virtual;
    function _AsCurrency(Obj: pointer): currency;virtual;
    function _AsTime(Obj: pointer): TDateTime;virtual;
    function _AsBoolean(Obj: pointer): boolean;virtual;
    procedure _Convert(Value: TLiValue);virtual;
    procedure _SetDefault(Value: TLiValue);virtual;
    procedure _GcMark(Obj: pointer);virtual; // for garbage collection
    function _Generate(Obj: pointer): TLiGenerate;virtual;
    function _Length(Obj: pointer): int64;virtual;
    function _Clear(Obj: pointer): boolean;virtual;
    function _Add(Obj: pointer; Value: TLiValue): integer;virtual;
    procedure _Validate(Obj: pointer);virtual;
  public
    function FindMethod(const AName: string): TLiFunc;
    function Method(const AName: string;
      const AType: TLiType;
      const ParamNames: array of string;
      const ParamTypes: array of TLiType;
      const AProc: TLiLyseeObjectProc): TLiFunc;overload;
    function Method(const AName: string;
      const ParamNames: array of string;
      const ParamTypes: array of TLiType;
      const AProc: TLiLyseeObjectProc): TLiFunc;overload;
    function Method(const AName: string;
      const AType: TLiType;
      const AProc: TLiLyseeObjectProc): TLiFunc;overload;
    function Method(const AName: string;
      const AProc: TLiLyseeObjectProc): TLiFunc;overload;
    { define property }
    function Define(const AProp: string; const AType: TLiType;
      const IndexNames: array of string; const IndexTypes: array of TLiType;
      const GetProc: TLiLyseeObjectProc;
      const SetProc: TLiLyseeObjectProc = nil): boolean;overload;
    function Define(const AProp: string; const AType: TLiType;
      const IndexName: string; const IndexType: TLiType;
      const GetProc: TLiLyseeObjectProc;
      const SetProc: TLiLyseeObjectProc = nil): boolean;overload;
    function Define(const AProp: string; const AType: TLiType;
      const GetProc: TLiLyseeObjectProc;
      const SetProc: TLiLyseeObjectProc = nil): boolean;overload;
  public
    constructor Create(const AName: string; AModule: TLiModule; AParent: TLiType = nil);virtual;
    destructor Destroy;override;
    function Prototype(const AName: string): string;
    function IsTypeOf(AType: TLiType): boolean;
    function IsChildTypeOf(AType: TLiType): boolean;
    function IsBasicValue: boolean;
    function IsEnum: boolean;
    function IsEnumSet: boolean;
    function IsObject: boolean;
    function IsNil: boolean;
    function AddOperate(OP: TLiOperator; AType: TLiType; AProc: TLiOperateProc): PLiOperate;
    function FindOperate(OP: TLiOperator; AType: TLiType): PLiOperate;
    function AddCompare(AType: TLiType; AFunc: TLiCompareFunc): PLiCompare;
    function FindCompare(AType: TLiType): PLiCompare;
    function AddConvert(AType: TLiType; AProc: TLiConvertProc): PLiConvert;
    function FindConvert(AType: TLiType): PLiConvert;
    property Parent: TLiType read FParent write SetParent;
    property Module: TLiModule read FModule;
    property Name: string read FName;
    property FullName: string read GetFullName;
    property TID: integer read FTID;
    property Style: TLiTypeStyle read FStyle;
    property Constructer: TLiFunc read FConstructer;
    property MethodCount: integer read GetMethodCount;
    property Methods[Index: integer]: TLiFunc read GetMethod;default;
  end;

  { TLiType_variant }

  TLiType_variant = class(TLiType)
  public
    procedure _Convert(Value: TLiValue);override;
    procedure _SetDefault(Value: TLiValue);override;
  end;

  { TLiType_nil }

  TLiType_nil = class(TLiType)
  public
    function _IncRefcount(Obj: pointer): integer;override;
    function _DecRefcount(Obj: pointer): integer;override;
    function _AsString(Obj: pointer): string;override;
    function _AsChar(Obj: pointer): char;override;
    function _AsInteger(Obj: pointer): int64;override;
    function _AsFloat(Obj: pointer): double;override;
    function _AsCurrency(Obj: pointer): currency;override;
    function _AsTime(Obj: pointer): TDateTime;override;
    function _AsBoolean(Obj: pointer): boolean;override;
    procedure _Convert(Value: TLiValue);override;
    procedure _SetDefault(Value: TLiValue);override;
  end;

  { TLiType_char }

  TLiType_char = class(TLiType)
  public
    function _AsString(Obj: pointer): string;override;
    function _AsChar(Obj: pointer): char;override;
    function _AsInteger(Obj: pointer): int64;override;
    function _AsFloat(Obj: pointer): double;override;
    function _AsCurrency(Obj: pointer): currency;override;
    function _AsBoolean(Obj: pointer): boolean;override;
    procedure _Convert(Value: TLiValue);override;
    procedure _SetDefault(Value: TLiValue);override;
  end;

  { TLiType_integer }

  TLiType_integer = class(TLiType)
  public
    function _AsString(Obj: pointer): string;override;
    function _AsChar(Obj: pointer): char;override;
    function _AsInteger(Obj: pointer): int64;override;
    function _AsFloat(Obj: pointer): double;override;
    function _AsCurrency(Obj: pointer): currency;override;
    function _AsTime(Obj: pointer): TDateTime;override;
    function _AsBoolean(Obj: pointer): boolean;override;
    procedure _Convert(Value: TLiValue);override;
    procedure _SetDefault(Value: TLiValue);override;
    function _Generate(Obj: pointer): TLiGenerate;override;
  end;

  { TLiType_float }

  TLiType_float = class(TLiType)
  public
    function _AsString(Obj: pointer): string;override;
    function _AsInteger(Obj: pointer): int64;override;
    function _AsFloat(Obj: pointer): double;override;
    function _AsCurrency(Obj: pointer): currency;override;
    function _AsTime(Obj: pointer): TDateTime;override;
    function _AsBoolean(Obj: pointer): boolean;override;
    procedure _Convert(Value: TLiValue);override;
    procedure _SetDefault(Value: TLiValue);override;
  end;

  { TLiType_currency }

  TLiType_currency = class(TLiType)
  public
    function _AsString(Obj: pointer): string;override;
    function _AsInteger(Obj: pointer): int64;override;
    function _AsFloat(Obj: pointer): double;override;
    function _AsCurrency(Obj: pointer): currency;override;
    function _AsTime(Obj: pointer): TDateTime;override;
    function _AsBoolean(Obj: pointer): boolean;override;
    procedure _Convert(Value: TLiValue);override;
    procedure _SetDefault(Value: TLiValue);override;
  end;

  { TLiType_time }

  TLiType_time = class(TLiType)
  public
    function _AsString(Obj: pointer): string;override;
    function _AsInteger(Obj: pointer): int64;override;
    function _AsFloat(Obj: pointer): double;override;
    function _AsTime(Obj: pointer): TDateTime;override;
    function _AsBoolean(Obj: pointer): boolean;override;
    procedure _Convert(Value: TLiValue);override;
    procedure _SetDefault(Value: TLiValue);override;
  end;

  { TLiType_boolean }

  TLiType_boolean = class(TLiType)
  public
    function _AsString(Obj: pointer): string;override;
    function _AsInteger(Obj: pointer): int64;override;
    function _AsBoolean(Obj: pointer): boolean;override;
    procedure _Convert(Value: TLiValue);override;
    procedure _SetDefault(Value: TLiValue);override;
  end;

  { TLiType_type }

  TLiType_type = class(TLiType)
  protected
    procedure MyName(const Param: TLiParam);
    procedure MyParent(const Param: TLiParam);
    procedure MyModule(const Param: TLiParam);
    procedure MyMethods(const Param: TLiParam);
    procedure MyIsTypeOf(const Param: TLiParam);
    procedure MyIsChildTypeOf(const Param: TLiParam);
    procedure MyIsObject(const Param: TLiParam);
    procedure MyIsNil(const Param: TLiParam);
    procedure MyIsEnum(const Param: TLiParam);
    procedure MyIsEnumSet(const Param: TLiParam);
    procedure MyItemValues(const Param: TLiParam);
    procedure MyPrototype(const Param: TLiParam);
    procedure MyFindMethod(const Param: TLiParam);
    procedure Setup;override;
  public
    function _AsString(Obj: pointer): string;override;
    procedure _Convert(Value: TLiValue);override;
  end;

  { TLiType_string }

  TLiType_string = class(TLiType)
  protected
    procedure MyGet(const Param: TLiParam);
    procedure Setup;override;
  public
    function _IncRefcount(Obj: pointer): integer;override;
    function _DecRefcount(Obj: pointer): integer;override;
    function _AsString(Obj: pointer): string;override;
    function _AsChar(Obj: pointer): char;override;
    function _AsInteger(Obj: pointer): int64;override;
    function _AsFloat(Obj: pointer): double;override;
    function _AsCurrency(Obj: pointer): currency;override;
    function _AsTime(Obj: pointer): TDateTime;override;
    function _AsBoolean(Obj: pointer): boolean;override;
    procedure _Convert(Value: TLiValue);override;
    function _Generate(Obj: pointer): TLiGenerate;override;
    function _Length(Obj: pointer): int64;override;
  end;

  { TLiType_func }

  TLiType_func = class(TLiType)
  protected
    procedure MyName(const Param: TLiParam);
    procedure MyPrototype(const Param: TLiParam);
    procedure MyParent(const Param: TLiParam);
    procedure MyModule(const Param: TLiParam);
    procedure MyIsMainFunc(const Param: TLiParam);
    procedure MyIsMethod(const Param: TLiParam);
    procedure MyIsConstructor(const Param: TLiParam);
    procedure MyIsChangeAble(const Param: TLiParam);
    procedure MyParamCount(const Param: TLiParam);
    procedure MyGetParamName(const Param: TLiParam);
    procedure MySetParamName(const Param: TLiParam);
    procedure MyGetParamType(const Param: TLiParam);
    procedure MySetParamType(const Param: TLiParam);
    procedure MyParamNames(const Param: TLiParam);
    procedure MyParamTypes(const Param: TLiParam);
    procedure MyClear(const Param: TLiParam);
    procedure MyClearParams(const Param: TLiParam);
    procedure MyClearCodes(const Param: TLiParam);
    procedure MyAddParam(const Param: TLiParam);
    procedure MyAddCode(const Param: TLiParam);
    procedure MySetCode(const Param: TLiParam);
    procedure MyGetResultType(const Param: TLiParam);
    procedure MySetResultType(const Param: TLiParam);
    procedure Setup;override;
  public
    function _IncRefcount(Obj: pointer): integer;override;
    function _DecRefcount(Obj: pointer): integer;override;
    function _AsString(Obj: pointer): string;override;
  end;

  { TLiType_module }

  TLiType_module = class(TLiType)
  protected
    procedure MyName(const Param: TLiParam);
    procedure MyConsts(const Param: TLiParam);
    procedure MyTypes(const Param: TLiParam);
    procedure MyFuncs(const Param: TLiParam);
    procedure MyUsings(const Param: TLiParam);
    procedure MyFind(const Param: TLiParam);
    procedure Setup;override;
  public
    function _IncRefcount(Obj: pointer): integer;override;
    function _DecRefcount(Obj: pointer): integer;override;
    function _AsString(Obj: pointer): string;override;
  end;

  { TLiType_list }

  TLiType_list = class(TLiType)
  protected
    procedure MyCreate(const Param: TLiParam);
    procedure MyIsEmpty(const Param: TLiParam);
    procedure MyClear(const Param: TLiParam);
    procedure MyDelete(const Param: TLiParam);
    procedure MyRemove(const Param: TLiParam);
    procedure MyExchange(const Param: TLiParam);
    procedure MyMove(const Param: TLiParam);
    procedure MySort(const Param: TLiParam);
    procedure MyInsert(const Param: TLiParam);
    procedure MyAdd(const Param: TLiParam);
    procedure MyIndexOf(const Param: TLiParam);
    procedure MyCopy(const Param: TLiParam);
    procedure MyLeft(const Param: TLiParam);
    procedure MyRight(const Param: TLiParam);
    procedure MyAssign(const Param: TLiParam);
    procedure MyGetCount(const Param: TLiParam);
    procedure MySetCount(const Param: TLiParam);
    procedure MyGet(const Param: TLiParam);
    procedure MySet(const Param: TLiParam);
    procedure Setup;override;
  public
    function _IncRefcount(Obj: pointer): integer;override;
    function _DecRefcount(Obj: pointer): integer;override;
    function _AsString(Obj: pointer): string;override;
    procedure _GcMark(Obj: pointer);override;
    function _Generate(Obj: pointer): TLiGenerate;override;
    function _Length(Obj: pointer): int64;override;
    function _Clear(Obj: pointer): boolean;override;
    function _Add(Obj: pointer; Value: TLiValue): integer;override;
  end;

  { TLiType_hash }

  TLiType_hash = class(TLiType)
  protected
    procedure MyCreate(const Param: TLiParam);
    procedure MyIsEmpty(const Param: TLiParam);
    procedure MyClear(const Param: TLiParam);
    procedure MyHas(const Param: TLiParam);
    procedure MyRemove(const Param: TLiParam);
    procedure MyKeys(const Param: TLiParam);
    procedure MyValues(const Param: TLiParam);
    procedure MyGet(const Param: TLiParam);
    procedure MySet(const Param: TLiParam);
    procedure Setup;override;
  public
    function _IncRefcount(Obj: pointer): integer;override;
    function _DecRefcount(Obj: pointer): integer;override;
    function _AsString(Obj: pointer): string;override;
    procedure _GcMark(Obj: pointer);override;
    function _Clear(Obj: pointer): boolean;override;
  end;

  { TLiEnumItem }

  TLiEnumItem = class
  private
    FParent: TLiEnumType;
    FName: string;
    FValue: integer;
  public
    procedure SetValue(Value: TLiValue);
    property Parent: TLiEnumType read FParent;
    property Name: string read FName;
    property Value: integer read FValue;
  end;

  { TLiEnumType }

  TLiEnumType = class(TLiType)
  private
    FItems: array of TLiEnumItem;
    function GetCount: integer;
    function GetItem(Index: integer): TLiEnumItem;
    function GetDefValue: TLiEnumItem;
  public
    function _IncRefcount(Obj: pointer): integer;override;
    function _DecRefcount(Obj: pointer): integer;override;
    function _AsString(Obj: pointer): string;override;
    function _AsInteger(Obj: pointer): int64;override;
    procedure _Convert(Value: TLiValue);override;
    procedure _SetDefault(Value: TLiValue);override;
  public
    constructor Create(const AName: string; AModule: TLiModule; AParent: TLiType);override;
    destructor Destroy;override;
    procedure Add(const ItemNames: array of string);
    function Find(const ItemName: string): TLiEnumItem;overload;
    function Find(ItemValue: integer): TLiEnumItem;overload;
    function ItemByName(const ItemName: string): TLiEnumItem;
    function ItemByValue(ItemValue: integer): TLiEnumItem;
    procedure SetValue(Value: TLiValue; Item: TLiEnumItem);overload;
    procedure SetValue(Value: TLiValue; ItemValue: integer);overload;
    procedure SetValue(Value: TLiValue; const ItemName: string);overload;
    function NewEnumSetType(const AName: string): TLiEnumSetType;
    property Count: integer read GetCount;
    property Items[Index: integer]: TLiEnumItem read GetItem;default;
    property DefValue: TLiEnumItem read GetDefValue;
  end;

  { TLiEnumSet }

  TLiEnumSet = class(TLiObject)
  private
    FParent: TLiEnumSetType;
    FSets: array of boolean;
    function GetSource: TLiEnumType;
    function GetCount: integer;
    function Get(Index: integer): boolean;
    procedure Put(Index: integer; Value: boolean);
  public
    destructor Destroy;override;
    procedure SetValue(Value: TLiValue);
    function AsBoolean: boolean;
    function AsString: string;override;
    function Equal(S: TLiEnumSet): boolean;
    function Add(S: TLiEnumSet): TLiEnumSet;
    function Dec(S: TLiEnumSet): TLiEnumSet;
    function Mul(S: TLiEnumSet): TLiEnumSet;
    function NotAll: TLiEnumSet;
    property Parent: TLiEnumSetType read FParent;
    property Source: TLiEnumType read GetSource;
    property Count: integer read GetCount;
    property Sets[Index: integer]: boolean read Get write Put;default;
  end;

  { TLiEnumSetType }

  TLiEnumSetType = class(TLiType)
  private
    FSource: TLiEnumType;
    FDefValue: TLiEnumSet;
    function GetDefValue: TLiEnumSet;
  public
    function _IncRefcount(Obj: pointer): integer;override;
    function _DecRefcount(Obj: pointer): integer;override;
    function _AsString(Obj: pointer): string;override;
    function _AsBoolean(Obj: pointer): boolean;override;
    procedure _Convert(Value: TLiValue);override;
    procedure _SetDefault(Value: TLiValue);override;
  public
    destructor Destroy;override;
    procedure SetValue(Value: TLiValue; ASet: TLiEnumSet);
    function NewEnumSet: TLiEnumSet;
    property Source: TLiEnumType read FSource;
    property DefValue: TLiEnumSet read GetDefValue;
  end;

  { TLiValue }

  PLiValue = ^RLiValue;
  RLiValue = packed record
    case integer of
    TID_CHAR    : (VChar: array[0..1] of char);
    TID_INTEGER : (VInteger: int64);
    TID_FLOAT   : (VFloat: double);
    TID_CURRENCY: (VCurrency: currency);
    TID_TIME    : (VTime: TDateTime);
    TID_BOOLEAN : (VBoolean: boolean);
    TID_TYPE    : (VObject: pointer);
  end;

  TLiValue = class
  private
    FType: TLiType;
    FValue: RLiValue;
    function GetAsInteger: int64;
    function GetAsChar: char;
    function GetAsBoolean: boolean;
    function GetAsFloat: double;
    function GetAsCurrency: currency;
    function GetAsTime: TDateTime;
    function GetAsType: TLiType;
    function GetAsString: string;
    function GetAsFunc: TLiFunc;
    function GetAsList: TLiList;
    function GetAsHash: TLiHash;
    function GetAsModule: TLiModule;
    function GetAsEnum: TLiEnumItem;
    function GetAsEnumSet: TLiEnumSet;
    procedure SetAsString(const Value: string);
    procedure SetAsInteger(Value: int64);
    procedure SetAsChar(Value: char);
    procedure SetAsBoolean(Value: boolean);
    procedure SetAsFloat(Value: double);
    procedure SetAsCurrency(Value: currency);
    procedure SetAsTime(Value: TDateTime);
    procedure SetAsType(Value: TLiType);
    procedure SetAsFunc(Value: TLiFunc);
    procedure SetAsList(Value: TLiList);
    procedure SetAsHash(Value: TLiHash);
    procedure SetAsModule(Value: TLiModule);
    procedure SetAsEnum(Value: TLiEnumItem);
    procedure SetAsEnumSet(Value: TLiEnumSet);
    procedure SetParentType(VT: TLiType);
  public
    constructor Create;virtual;
    destructor Destroy;override;
    function IncRefcount: integer;
    function DecRefcount: integer;
    procedure MarkForSurvive;
    procedure SetValue(Value: TLiValue);
    procedure SetNil;
    procedure SetDefault(AType: TLiType);
    procedure Convert(AType: TLiType; Cntx: TLiContext);
    procedure SetFind(Finded: PLiFind);
    function GetOA(Wanted: TLiType = nil): pointer; // get object|address
    function GetTOA(var OA: pointer): TLiType; // get type and object|address
    procedure SetTOA(T: TLiType; OA: pointer);
    procedure SetObject(AType: TLiType; Aobj: pointer);
    function IsBasicValue: boolean;
    function IsObject: boolean;
    function IsNil: boolean;
    function IsDefv: boolean;
    function IsBoolTrue: boolean;
    function IsFalse: boolean;
    function GetFileName: string;
    function GetString: TLiString;
    function GetFunc: TLiFunc;
    function GetModule: TLiModule;
    function GetList: TLiList;
    function GetHashed: TLiHash;
    function GetSelf(var Aobj): boolean;
    function Operate(OP: TLiOperator; Value: TLiValue): boolean;
    function Compare(Value: TLiValue): TLiCompare;overload;
    function Compare(Value: TLiValue; Wanted: TLiCompares): boolean;overload;
    function Same(Value: TLiValue): boolean;
    function NewList: TLiList;
    property VType: TLiType read FType write SetParentType;
    property AsModule: TLiModule read GetAsModule write SetAsModule;
    property AsFunc: TLiFunc read GetAsFunc write SetAsFunc;
    property AsList: TLiList read GetAsList write SetAsList;
    property AsHash: TLiHash read GetAsHash write SetAsHash;
    property AsString: string read GetAsString write SetAsString;
    property AsChar: char read GetAsChar write SetAsChar;
    property AsInteger: int64 read GetAsInteger write SetAsInteger;
    property AsBoolean: boolean read GetAsBoolean write SetAsBoolean;
    property AsFloat: double read GetAsFloat write SetAsFloat;
    property AsCurrency: currency read GetAsCurrency write SetAsCurrency;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsType: TLiType read GetAsType write SetAsType;
    property AsEnum: TLiEnumItem read GetAsEnum write SetAsEnum;
    property AsEnumSet: TLiEnumSet read GetAsEnumSet write SetAsEnumSet;
  end;

  { TLiParam }

  TLiParam = class
  private
    FContext: TLiContext;
    FFunc: TLiFunc;
    FToken: TLiToken;
    FParams: TLiList;
    FPrmc: integer;   {<--actual parameters' count passed to function}
    FResult: TLiValue;
    FPrev: TLiParam;
    function GetCount: integer;
    function GetItem(Index: integer): TLiValue;
    function GetValue(const Name: string): TLiValue;
    procedure SetResult(Value: TLiValue);
  public
    procedure Error(const Msg: string);overload;
    procedure Error(const Msg: string; const Args: array of const);overload;
    procedure ErrorOper(OP: TLiOperator; L, R: TLiType);
    procedure ErrorChangeFunc(AFunc: TLiFunc);
    function TestChangeFunc(AFunc: TLiFunc): boolean;
    procedure BeginExec(var Mark: integer; var Tmpv: TLiValue);overload;
    procedure BeginExec(var Mark: integer);overload;
    procedure EndExec(Mark: integer);
    function GetVarbValue(Index: integer; var VT: TLiType): TLiValue;
    function GetSelf(var Aobj): boolean;
    function GetChangeAbleFunc(var F: TLiFunc): boolean;
    property Context: TLiContext read FContext;
    property Func: TLiFunc read FFunc;
    property Params: TLiList read FParams;
    property Result: TLiValue read FResult write SetResult;
    property Token: TLiToken read FToken;
    property Prmc: integer read FPrmc;
    property Count: integer read GetCount;
    property Items[Index: integer]: TLiValue read GetItem;default;
    property Values[const Name: string]: TLiValue read GetValue;
  end;

  { TLiToken }

  TLiSymbol = (syError,
    {keyword}syBegin, syUses, syConst, syFunc, syProc, syVar, syGet, sySet,
    syIf, syElif, syThen, syElse, syCase, syOf, syFor, syWhile, syRepeat,
    syUntil, syDo, syResult, syDiv, syMod, syTo, syDownto, syNot, syIn, syIs,
    syAs, syLike, syAnd, syOr, syXor, syShr, syShl, syTry, syExcept,
    syFinally, syRaise, syTrue, syFalse, syNil, syEnd,
    {operator} syBecome, syShi, syFill, syAdd, syReduce, syMul, syDivf,
    syLParen, syRParen, syLArray, syRArray, syDot, syRange, syVarArg, syColon,
    sySemic, syComma, syAt, sySame, syEQ, syNE, syLT, syLE, syMT, syME, syVert,
    {abstract} syID, syNeg, syFloat, syInt, syStr, syChar, syList, syHash,
    syCall, syEOF
  );
  TLiSymbols = set of TLiSymbol;

  RLiTokenValue = packed record
    case TLiSymbol of
    syChar :(VChar: char);
    syInt  :(VInt: int64);
    syFloat:(VFloat: double);
    syVert :(VFunc: TLiFunc);
  end;

  TLiToken = class
  private
    FSym: TLiSymbol;       // syTrue, syFalse
    FRow: integer;
    FCol: integer;
    FName: string;         // syBecome, syID, syStr
    FValue: RLiTokenValue; // syChar, syInt, syFloat
    FRight: TLiToken;
    FLeft: TLiToken;
    FParams: TList;
    function GetParamCount: integer;
    function GetParam(Index: integer): TLiToken;
    function SetupParamList(Param: TLiParam; Host: TLiValue; Func: TLiFunc): TLiList;
    function GetAt(Param: TLiParam; Host, Outv: TLiValue): boolean;
    function GetProp(Param: TLiParam; Host, Outv: TLiValue): boolean;
    function TryFunc(Func: TLiFunc; Param: TLiParam; Outv: TLiValue): boolean;
    function TryMethod(Host: TLiValue; const Method: string;
      Param: TLiParam; Outv: TLiValue; LastParam: TLiToken): boolean;
  protected
    procedure ExecID(Param: TLiParam; Outv: TLiValue);
    procedure ExecGet(Param: TLiParam; Outv: TLiValue);
    procedure ExecSet(Param: TLiParam);
    procedure ExecCall(Param: TLiParam; Outv: TLiValue);
  public
    constructor Create(T: TLiToken = nil);
    constructor CreateWithLeft(LeftBranch: TLiToken; T: TLiToken = nil);
    destructor Destroy;override;
    procedure Assign(Source: TLiToken);
    procedure Read(Source: TLiToken);
    procedure Clear;
    procedure ClearParams;
    procedure AddParam(AParam: TLiToken);
    procedure Error(Param: TLiParam; const Msg: string);overload;
    procedure Error(Param: TLiParam; const Msg: string; const Args: array of const);overload;
    procedure FailGet(Param: TLiParam; const ID: string);overload;
    procedure FailGet(Param: TLiParam; const Host, Prop: string);overload;
    function ExecFunc(Func: TLiFunc; Param: TLiParam; Outv: TLiValue; Args: TLiList): boolean;
    function Execute(Param: TLiParam; Outv: TLiValue): boolean;
    function Decompile(Level: integer = 0): string;
    property ParamCount: integer read GetParamCount;
    property Params[Index: integer]: TLiToken read GetParam;
    property Left: TLiToken read FLeft write FLeft;
    property Right: TLiToken read FRight write FRight;
  end;

  { TLiTokenList }

  TLiTokenList = class(TLiObject)
  private
    FItems: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TLiToken;
    function GetLast: TLiToken;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Reverse;
    procedure Clear;
    procedure ClearKeepLast;
    procedure DeleteLast(N: integer);
    function Add(Pos: TLiToken = nil): TLiToken;
    function AddToken(Token: TLiToken): TLiToken;overload;
    function AddToken(Sym: TLiSymbol; Pos: TLiToken): TLiToken;overload;
    function AddToken(Sym: TLiSymbol): TLiToken;overload;
    property Count: integer read GetCount;
    property Items[Index: integer]: TLiToken read GetItem;default;
    property Last: TLiToken read GetLast;
  end;

  { TLiTokenizer }

  TLiTokenizer = class(TLiObject)
  private
    FTokens: TLiTokenList;
    FIndex: integer;
    FCurrent: TLiToken;
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
    function GetToken(token: TLiToken): boolean;
    function GetCurrent: TLiToken;
  public
    constructor Create(const Script: string);
    destructor Destroy;override;
    function PackToCurrent: boolean;
    function GetNext: TLiToken;
    function PeekNextSymbol: TLiSymbol;
    function PeekThirdSymbol: TLiSymbol;
    function NextIsBecome(OnHead: boolean): boolean;
    property Current: TLiToken read GetCurrent;
    property Row: integer read FRow write FRow;
    property Col: integer read FCol write FCol;
    property Position: integer read FPosition;
    property Code: string read FCode;
  end;

  { TLiParser }

  TLiParser = class(TLiObject)
  private
    FTokenizer: TLiTokenizer; {<--token analyzer}
    FLast: TLiToken;          {<--last token}
    FFunc: TLiFunc;           {<--current function}
    FModule: TLiModule;       {<--current module}
    FContext: TLiContext;     {<--current context}
    FAfter: integer;
    function UseToken(Token: TLiToken): TLiToken;
  protected
    procedure EUnexpected(T: TLiToken = nil);
    procedure ERedeclared(T: TLiToken = nil);
    procedure ETypeNotFound(T: TLiToken = nil);
    procedure ESyntax(ERow, ECol: integer; const EMsg: string; const EArgs: array of const);
    { parsing }
    procedure ParseUsesConstFunc;
    procedure ParseUses;
    procedure ParseConst;
    procedure ParseFunc;
    procedure ParseBlock(EndSyms: TLiSymbols; SX: TLiSTMTList);
    procedure ParseStatement(OnHead: boolean; SX: TLiSTMTList);
    procedure ParseType(OnHead: boolean; var T: TLiType);
    procedure ParseArguments(EndSym: TLiSymbol);
    { statement }
    procedure ParseVar(SX: TLiSTMTList);
    procedure ParseIf(SX: TLiSTMTList);
    procedure ParseFor(SX: TLiSTMTList);
    procedure ParseWhile(SX: TLiSTMTList);
    procedure ParseRepeat(SX: TLiSTMTList);
    procedure ParseCase(SX: TLiSTMTList);
    procedure ParseTry(SX: TLiSTMTList);
    procedure ParseRaise(SX: TLiSTMTList);
    procedure ParsePuts(SX: TLiSTMTList);
    procedure ParseAny(SX: TLiSTMTList);
    { expression }
    function ParseExpr(OnHead: boolean; EndSyms: TLiSymbols; DoCheck: boolean): TLiToken;
    function ParseFact(Level: integer): TLiToken;
    function ParseTerm: TLiToken;
    { token }
    procedure SymGetNext;
    procedure SymGotoNext;
    procedure SymTestNext(Syms: TLiSymbols);
    procedure SymTestLast(Syms: TLiSymbols);
    procedure SymTestLastID;
    procedure SymTestNextID;
    function SymPeekNext: TLiSymbol;
  public
    constructor Create(AModule: TLiModule);
    destructor Destroy;override;
    function Parse(const Code: string; UsingModule: boolean = false): TLiFunc;
    function ParseAndFree(const Code: string; UsingModule: boolean = false): TLiFunc;
  end;

  { TLiSTMT }

  TLiSTMTStyle = (ssNormal, ssConst, ssAssign, ssResult, ssPuts, ssIf, ssWhile,
                  ssRepeat, ssFor, ssCase, ssTry, ssRaise);

  TLiSTMT = class
  private
    FStyle: TLiSTMTStyle;
    FParent: TLiSTMTList;
    FItems: TLiSTMTList;
    FExpr: TLiToken;
    function GetItems: TLiSTMTList;
    function GetCount: integer;
  protected
    procedure ExecNormal(Param: TLiParam);
    procedure ExecPuts(Param: TLiParam);
    procedure ExecWhile(Param: TLiParam);
    procedure ExecRepeat(Param: TLiParam);
    procedure ExecRaise(Param: TLiParam);
  public
    constructor Create(AParent: TLiSTMTList);virtual;
    destructor Destroy;override;
    function Execute(Param: TLiParam): boolean;virtual;
    procedure Decompile(Level: integer; Lines: TStrings);virtual;
    property Style: TLiSTMTStyle read FStyle;
    property Parent: TLiSTMTList read FParent;
    property Items: TLiSTMTList read GetItems;
    property Count: integer read GetCount;
    property Expr: TLiToken read FExpr;
  end;

  { TLiSTMT_assign }

  TLiSTMT_assign = class(TLiSTMT)
  private
    FVarb: string;
  protected
    procedure ExecConst(Param: TLiParam);
    procedure ExecAssign(Param: TLiParam);
    procedure ExecResult(Param: TLiParam);
  public
    constructor Create(AParent: TLiSTMTList);override;
    function Execute(Param: TLiParam): boolean;override;
    procedure Decompile(Level: integer; Lines: TStrings);override;
    property Varb: string read FVarb;
  end;

  { TLiSTMT_for }

  TLiSTMT_for = class(TLiSTMT_assign)
  private
    FUpTo: boolean;
    FEndValue: TLiToken;
  public
    constructor Create(AParent: TLiSTMTList);override;
    destructor Destroy;override;
    function Execute(Param: TLiParam): boolean;override;
    procedure Decompile(Level: integer; Lines: TStrings);override;
    property EndValue: TLiToken read FEndValue;
    property UpTo: boolean read FUpTo;
  end;

  { TLiSTMT_if }

  TLiSTMT_if = class(TLiSTMT)
  private
    FElseItems: TLiSTMTList;
    function GetElseItems: TLiSTMTList;
    function GetElseCount: integer;
  public
    constructor Create(AParent: TLiSTMTList);override;
    destructor Destroy;override;
    function Execute(Param: TLiParam): boolean;override;
    procedure Decompile(Level: integer; Lines: TStrings);override;
    function IsElif: boolean;
    property ElseCount: integer read GetElseCount;
    property ElseItems: TLiSTMTList read GetElseItems;
  end;

  { TLiSTMT_case }

  TLiSTMT_case = class(TLiSTMT_if)
  public
    constructor Create(AParent: TLiSTMTList);override;
    function Execute(Param: TLiParam): boolean;override;
    procedure Decompile(Level: integer; Lines: TStrings);override;
  end;

  { TLiSTMT_try }

  TLiSTMT_try = class(TLiSTMT_if)
  private
    FTryFinally: boolean;
  public
    constructor Create(AParent: TLiSTMTList);override;
    function Execute(Param: TLiParam): boolean;override;
    procedure Decompile(Level: integer; Lines: TStrings);override;
    property TryFinally: boolean read FTryFinally;
  end;

  { TLiSTMTList }

  TLiSTMTList = class
  private
    FItems: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TLiSTMT;
  public
    destructor Destroy;override;
    procedure Clear;
    procedure Delete(Index: integer);
    function Add(Style: TLiSTMTStyle): TLiSTMT;overload;
    function Add(STMT: TLiSTMT): integer;overload;
    function Execute(Param: TLiParam): boolean;
    procedure Decompile(Level: integer; Lines: TStrings);
    property Count: integer read GetCount;
    property Items[Index: integer]: TLiSTMT read GetItem;default;
  end;

  { TLiVarb }

  TLiVarb = class
  private
    FName: string;
    FType: TLiType;
  public
    constructor Create(const AName: string; AType: TLiType);
    destructor Destroy;override;
    function Prototype: string;
    property Name: string read FName;
    property ValueType: TLiType read FType;
  end;

  { TLiVarbList}

  TLiVarbList = class
  private
    FVarbs: array of TLiVarb;
    FLocalCount: integer;
    function GetCount: integer;
    function GetParamCount: integer;
    function GetVarb(Index: integer): TLiVarb;
    function DoAdd(const AName: string; AType: TLiType): TLiVarb;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    procedure Assign(Source: TLiVarbList);
    function Add(const AName: string; AType: TLiType): TLiVarb;
    function AddLocal(const AName: string; AType: TLiType): TLiVarb;
    function IndexOf(const AName: string): integer;overload;
    function IndexOf(const AVarb: TLiVarb): integer;overload;
    function Find(const AName: string): TLiVarb;
    property Count: integer read GetCount;
    property LocalCount: integer read FLocalCount;
    property ParamCount: integer read GetParamCount;
    property Varbs[Index: integer]: TLiVarb read GetVarb;default;
  end;

  { TLiFunc }

  TLiFunc = class(TLiObject)
  private
    FModule: TLiModule;
    FName: string;
    FParent: TLiType;
    FParams: TLiVarbList;
    FResultType: TLiType;
    FProc: TLiLyseeProc;
    FData: pointer;
    FHasVarArg: boolean; // last param is ...
    FMinArgs: integer;
    FSTMTs: TLiSTMTList;
    function GetSTMTs: TLiSTMTList;
    function GetMinArgs: integer;
    function GetFullName: string;
  public
    constructor Create(const AName: string; M: TLiModule; Proc: TLiLyseeProc);
    constructor CreateMethod(const AName: string; P: TLiType; Proc: TLiLyseeProc);
    destructor Destroy;override;
    function Prototype: string;
    function FindInside(const ID: string; rec: PLiFind = nil): boolean;
    function FindBy(const ID: string; rec: PLiFind; Range: TLiFinds = []): boolean;
    function FindSave(const ID: string; Param: TLiParam; Outv: TLiValue): TLiFind;
    function Context: TLiContext;
    function IsMainFunc: boolean;
    function IsMethod: boolean;
    function IsConstructor: boolean;
    function IsProcedure: boolean;
    function IsFunction: boolean;
    function IsConst: boolean;
    function MakeMethod: TLiFunc;
    function Executing: boolean;
    function ChangeAble: boolean;
    function AddCode(const Code: string): boolean;
    function SetCode(const Code: string): boolean;
    procedure Decompile(Level: integer; Lines: TStrings);
    function AsString: string;override;
    property Name: string read FName;
    property HasVarArg: boolean read FHasVarArg;
    property Parent: TLiType read FParent;
    property FullName: string read GetFullName;
    property MinArgs: integer read GetMinArgs;
    property Module: TLiModule read FModule;
    property ResultType: TLiType read FResultType write FResultType;
    property Params: TLiVarbList read FParams;
    property Proc: TLiLyseeProc read FProc write FProc;
    property Data: pointer read FData;
    property STMTs: TLiSTMTList read GetSTMTs;
  end;

  { TLiFuncList }

  TLiFuncList = class
  private
    FItems: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TLiFunc;
  public
    constructor Create;
    destructor Destroy;override;
    function Get(const Name: string): TLiFunc;
    procedure Clear;
    procedure Delete(Index: integer);
    property Count: integer read GetCount;
    property Items[Index: integer]: TLiFunc read GetItem;default;
  end;

  { TLiModule }

  TLiModule = class(TLiNamedObject)
  private
    FContext: TLiContext;    {<--private: owner context}
    FModules: TLiModuleList; {<--private: modules used by this module}
    FImporters: TList;       {<--private: modules useing this module}
    FFileName: string;
    FTypeList: TList;
    FFuncList: TLiFuncList;
    FConsts: TLiHash;
    FNeedSetup: boolean;
    procedure InitModule;
    procedure DeleteFunctions;
    procedure DeleteTypes;
  public
    function AddFunc(const AName: string; T: TLiType;
      const ParamNames: array of string; const ParamTypes: array of TLiType;
      const Proc: TLiLyseeProc;
      const Data: pointer = nil): TLiFunc;overload;
    function AddFunc(const AName: string;
      const ParamNames: array of string; const ParamTypes: array of TLiType;
      const Proc: TLiLyseeProc;
      const Data: pointer = nil): TLiFunc;overload;
    function AddFunc(const AName: string; T: TLiType;
      const Proc: TLiLyseeProc;
      const Data: pointer = nil): TLiFunc;overload;
    function AddFunc(const AName: string;
      const Proc: TLiLyseeProc;
      const Data: pointer = nil): TLiFunc;overload;
  public
    constructor Create(const AName: string);override;
    constructor CreateEx(const AName: string; AContext: TLiContext);
    destructor Destroy;override;
    procedure Setup;virtual;
    procedure Use(M: TLiModule);
    function EnsureName(const AName: string): string;
    function AddEnumType(const AName: string;
      const ItemNames: array of string): TLiEnumType;
    function IsMainModule: boolean;
    function SetupType(const T: TLiType): boolean;
    function TypeCount: integer;
    function GetType(Index: integer): TLiType;
    function FindModule(const ID: string; FindPossible: boolean): TLiModule;
    function FindType(const ID: string): TLiType;
    function FindTypeBy(const ID, AModule: string): TLiType;
    function FindFunc(const ID: string): TLiFunc;
    function FindFuncBy(const ID: string): TLiFunc;
    function Find(const ID: string; rec: PLiFind = nil): boolean;
    function FindBy(const ID, AModule: string; rec: PLiFind): boolean;
    function FindSave(const AName: string; Value: TLiValue): boolean;
    function FindSaveBy(const AName: string; Value: TLiValue): boolean;
    function UseModule(const AName: string): TLiModule;
    function AsString: string;override;
    property Modules: TLiModuleList read FModules;
    property FileName: string read FFileName write FFileName;
    property Context: TLiContext read FContext;
    property Consts: TLiHash read FConsts;
  end;

  { TLiModuleList }

  TLiModuleList = class(TLiObject)
  private
    FContext: TLiContext;
    FImporter: TLiModule;
    FModules: TList;
    function GetModule(Index: integer): TLiModule;
    function GetCount: integer;
  public
    constructor Create(AContext: TLiContext);
    destructor Destroy;override;
    procedure Setup;
    function IndexOf(AModule: TLiModule): integer;overload;
    function IndexOf(const Name: string): integer;overload;
    function Has(AModule: TLiModule): boolean;overload;
    function Has(const Name: string): boolean;overload;
    function Find(const Name: string): TLiModule;
    function Add(AModule: TLiModule): integer;
    procedure Delete(Index: integer);
    procedure Clear;
    procedure DeleteFunctions;
    procedure ClearConsts;
    function ToList: TLiList;
    property Count: integer read GetCount;
    property Modules[Index: integer]: TLiModule read GetModule;default;
  end;

  { TLiString }

  TLiString = class(TLiObject)
  private
    FValue: string;
  public
    constructor Create(const S: string);
    constructor CreateIncRefcount(const S: string);
    function Length: integer;
    function AsString: string;override;
    property Value: string read FValue write FValue;
  end;

  { TLiGarbage }

  TLiGarbage = class(TLiObject)
  private
    FPrev: TLiGarbage;
    FNext: TLiGarbage;
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

  { TLiGarbageCollect }

  TLiGarbageCollect = class
  private
    FChain: TLiGarbage;
    FDead: TList;
    FContexts: TList;
  protected
    procedure MarkSurvived;virtual;
    procedure GcAdd(G: TLiGarbage);
    procedure GcRemove(G: TLiGarbage);
  public
    constructor Create;virtual;
    destructor Destroy;override;
    function Collect: integer;virtual;
  end;

  { TLiList }

  TLiList = class(TLiGarbage)
  private
    FItems: TList;
    FFormating: boolean;
    function GetCount: integer;
    procedure SetCount(NewCount: integer);
    function GetItem(Index: integer): TLiValue;
  protected
    procedure MarkForSurvive;override;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure Clear;override;
    procedure Delete(Index: integer);
    procedure DeleteLast;
    procedure Remove(Value: TLiValue);
    procedure Exchange(Index1, Index2: integer);
    procedure Move(CurIndex, NewIndex: integer);
    procedure GetFirstTwo(var V1, V2: TLiValue);
    procedure PrepareFor(Func: TLiFunc);
    procedure Sort;
    procedure AddStrings(List: TStrings);
    function Add(Value: TLiValue = nil): TLiValue;
    function AddList: TLiList;
    function Insert(Index: integer; Value: TLiValue = nil): TLiValue;
    function IndexOf(Value: TLiValue): integer;
    function Copy(Index, ItemCount: integer): TLiList;
    function CopyLeft(ItemCount: integer): TLiList;
    function CopyRight(ItemCount: integer): TLiList;
    function First: TLiValue;
    function Last: TLiValue;
    function AsString: string;override;
    property Count: integer read GetCount write SetCount;
    property Items[Index: integer]: TLiValue read GetItem;default;
  end;

  { TLiHashItem }

  TLiHashItem = class(TLiValue)
  private
    FKey: string;
    FNext: TLiHashItem;
  public
    function Format: string;
    property Key: string read FKey;
  end;

  { TLiHash }

  TLiHash = class(TLiGarbage)
  private
    FBuckets: array of TLiHashItem;
    FCount: integer;
    FFormating: boolean;
    FCaseSensitive: boolean;
    procedure FreeItem(H: TLiHashItem);
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
    procedure ListKeys(List: TLiList);
    procedure ListValues(List: TLiList);
    function Add(const Name: string): TLiHashItem;
    function Get(const Name: string): TLiHashItem;
    function GetValue(const Name: string; Value: TLiValue): boolean;
    function Has(const Name: string): boolean;
    function Remove(const Name: string): boolean;
    function IsEmpty: boolean;
    function SetupItemList: TList;
    function DefConst(const Name, Value: string): TLiHashItem;overload;
    function DefConst(const Name: string; Value: int64): TLiHashItem;overload;
    function DefConst(const Name: string; Value: double): TLiHashItem;overload;
    function DefConst(const Name: string; Value: boolean): TLiHashItem;overload;
    function AsString: string;override;
    property Count: integer read FCount;
    property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive;
  end;

  { TLiGenerate }

  TLiGenerate = class(TLiValue)
  public
    function GetNext: boolean;virtual;
    function HasNext: boolean;virtual;
  end;

  { TLiGenerate_string }

  TLiGenerate_string = class(TLiGenerate)
  private
    FS: string;
    FIndex: integer;
  public
    constructor CreateIn(const S: string);
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLiGenerate_list }

  TLiGenerate_list = class(TLiGenerate)
  private
    FL: TLiList;
    FIndex: integer;
  public
    constructor CreateIn(const AList: TLiList);
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLiGenerate_strlist }

  TLiGenerate_strlist = class(TLiGenerate)
  private
    FL: TStrings;
    FIndex: integer;
  public
    constructor CreateIn(const List: TStrings);
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLiGenerate_int64 }

  TLiGenerate_int64 = class(TLiGenerate)
  private
    FV, FCount: int64;
    FUpto: boolean;
  public
    constructor CreateIn(V1, V2: int64; Upto: boolean);overload;
    constructor CreateIn(Range: int64);overload;
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLiGenerate_char }

  TLiGenerate_char = class(TLiGenerate)
  private
    FV: char;
    FCount: integer;
    FUpto: boolean;
  public
    constructor CreateIn(V1, V2: char; Upto: boolean);
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLiGenerate_boolean }

  TLiGenerate_boolean = class(TLiGenerate)
  private
    FV: boolean;
    FCount: integer;
    FUpto: boolean;
  public
    constructor CreateIn(V1, V2, Upto: boolean);
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLiGenerate_enum }

  TLiGenerate_enum = class(TLiGenerate)
  private
    FV: TLiEnumItem;
    FCount: integer;
    FUpto: boolean;
  public
    constructor CreateIn(V1, V2: TLiEnumItem; Upto: boolean);
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

function AcquireLock: boolean;
function ReleaseLock: boolean;
function AddModule(const Name: string): TLiModule;
function Command: integer;
function MatchID(const ID1, ID2: string): boolean;overload;
function MatchID(const ID: string; const IDList: array of string): boolean;overload;
function OperatorStr(OP: TLiOperator): string;
function Keywords: string;
function Hilights: string;
function ExtractNameModule(const ID: string; var Module: string): string;
function FormatChar(C: char): string;
function FormatString(const S: string): string;
function FormatValue(V: TLiValue): string;
function CompleteFileName(const FileName: string): string;
function SortValueCompare(V1, V2: pointer): integer;
procedure ErrorConvert(AType, NewType: TLiType);
function GetGenerate(Value: TLiValue): TLiGenerate;overload;
function GetGenerate(V1, V2: TLiValue; Upto: boolean): TLiGenerate;overload;
function Margin(Level: integer): string;
procedure FreeOperates(var Head: PLiOperate);
procedure FreeCompares(var Head: PLiCompare);
procedure FreeConverts(var Head: PLiConvert);
function ParamOK(const Names: array of string; const Types: array of TLiType;
  IsMethod: boolean; var HasVarArg: boolean): boolean;

function AddFunc(const Name: string; T: TLiType;
  const ParamNames: array of string; const ParamTypes: array of TLiType;
  Proc: TLiLyseeProc; Data: pointer = nil): TLiFunc;overload;

function AddFunc(const Name: string;
  const ParamNames: array of string; const ParamTypes: array of TLiType;
  Proc: TLiLyseeProc; Data: pointer = nil): TLiFunc;overload;

function AddFunc(const Name: string; T: TLiType;
  Proc: TLiLyseeProc; Data: pointer = nil): TLiFunc;overload;

function AddFunc(const Name: string;
  Proc: TLiLyseeProc; Data: pointer = nil): TLiFunc;overload;

function AddMethod(const AClass: TLiType; const AName: string;
  const AType: TLiType; const ParamNames: array of string;
  const ParamTypes: array of TLiType; const AProc: TLiLyseeProc): TLiFunc;overload;

function AddMethod(const AClass: TLiType; const AName: string;
  const ParamNames: array of string;
  const ParamTypes: array of TLiType; const AProc: TLiLyseeProc): TLiFunc;overload;

function AddMethod(const AClass: TLiType; const AName: string;
  const AType: TLiType; const AProc: TLiLyseeProc): TLiFunc;overload;

function AddMethod(const AClass: TLiType; const AName: string;
  const AProc: TLiLyseeProc): TLiFunc;overload;

function SetupProp(const AClass: TLiType;
  const AName: string; const AType: TLiType;
  const IndexNames: array of string;
  const IndexTypes: array of TLiType;
  const GetProc, SetProc: TLiLyseeProc;
  var GetFunc, SetFunc: TLiFunc): boolean;overload;

function SetupProp(const AClass: TLiType;
  const AName: string; const AType: TLiType;
  const IndexNames: array of string;
  const IndexTypes: array of TLiType;
  const GetProc: TLiLyseeProc;
  var GetFunc: TLiFunc): boolean;overload;

function SetupProp(const AClass: TLiType;
  const AName: string; const AType: TLiType;
  const IndexNames: array of string;
  const IndexTypes: array of TLiType;
  const GetProc: TLiLyseeProc;
  const SetProc: TLiLyseeProc = nil): boolean;overload;

function SetupProp(const AClass: TLiType;
  const AName: string; const AType: TLiType;
  const GetProc: TLiLyseeProc;
  const SetProc: TLiLyseeProc = nil): boolean;overload;

var
  my_program    : string;            {<--program file name}
  my_kernel     : string;            {<--kernel file name}
  my_gcman      : TLiGarbageCollect; {<--unique garbage collection manager}
  my_system     : TLiModule;         {<--System module}
  my_modules    : TLiModuleList;     {<--public module list}
  my_types      : array[TID_VARIANT..TID_LIST] of TLiType;
  my_knpath     : string;            {<--kernel file path}
  my_kndir      : string;            {<--kernel file directory}
  my_home       : string;            {<--home path}
  my_tmpath     : string;            {<--temporary path}
  my_search_path: string;            {<--module search path list}
  my_variant    : TLiType_variant;
  my_nil        : TLiType_nil;
  my_char       : TLiType_char;
  my_int        : TLiType_integer;
  my_float      : TLiType_float;
  my_curr       : TLiType_currency;
  my_time       : TLiType_time;
  my_bool       : TLiType_boolean;
  my_type       : TLiType_type;
  my_string     : TLiType_string;
  my_module     : TLiType_module;
  my_func       : TLiType_func;
  my_hash       : TLiType_hash;
  my_list       : TLiType_list;

implementation

{$IFDEF FPC}
type PBoolean = ^boolean;
{$ENDIF}

const

  LSE_HASH_DELTA = 64;

  Symbols: array[TLiSymbol] of packed record
    SY: TLiSymbol; // symbol value
    ID: string;    // symbol spelling
    SM: string;     // symbol description
  end = (
    (SY:syError;     ID:'';          SM:'error'),
    (SY:syBegin;     ID:'begin';     SM:'begin'),
    (SY:syUses;      ID:'uses';      SM:'uses module'),
    (SY:syConst;     ID:'const';     SM:'define constant'),
    (SY:syFunc;      ID:'function';  SM:'define function'),
    (SY:syProc;      ID:'procedure'; SM:'define procedure'),
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
    (SY:syVarArg;    ID:'...';       SM:'ellipsis'),
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
    (SY:syList;      ID:'';          SM:'create variant list'),
    (SY:syHash;      ID:'';          SM:'create hash table'),
    (SY:syCall;      ID:'';          SM:'call function'),
    (sy:syEOF;       ID:'';          SM:'end of file')
  );

  NameSyms = [syID, syStr, syCall, syGet, sySet];
  DataSyms = [syID, syResult, syFloat, syInt, syStr, syChar, syTrue, syFalse, syNil, syVarArg];
  ExprHead = DataSyms + [syLParen, syLArray, syNot, syReduce, syAt, syVert];

  OperSyms = [
    syMul, syDiv, syDivf, syMod,                                            // 0
    syAdd, syReduce,                                                        // 1
    syXor, syShl, syShr, syShi, syFill,                                     // 2
    sySame, syEQ, syNE, syLT, syLE, syMT, syME, syIn, syLike, syAs, syIs,   // 3
    syAnd, syOr];                                                           // 4

  OperLevel: array[0..4] of TLiSymbols = (
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

function IsVarArg(const ID: string; T: TLiType): boolean;
begin
  Result := (ID = '...') and ((T = my_list) or (T = my_variant));
end;

function IsParam(const ID: string; const T: TLiType): boolean;
begin
  Result := (T <> nil) and (T <> my_nil) and (IsID(ID) or IsVarArg(ID, T));
end;

procedure LyseeObjectProc(const Param: TLiParam);
begin
  Param.FFunc.FParent.FProcs[PtrToInt(Param.FFunc.FData)](Param);
end;

// operator --------------------------------------------------------------------

procedure variant_in_list(V1, V2: TLiValue);
var
  I: integer;
  A: TLiList;
begin
  A := V2.AsList;
  if A <> nil then
    I := A.IndexOf(V1) else
    I := -1;
  V1.SetAsBoolean(I >= 0);
end;

procedure variant_is_type(V1, V2: TLiValue);
begin
  V1.SetAsBoolean(V1.AsType.IsTypeOf(V2.AsType));
end;

procedure variant_as_type(V1, V2: TLiValue);
begin
  V2.AsType._Convert(V1);
end;

procedure variant_shi_variant(V1, V2: TLiValue);
var
  J: pointer;
begin
  if V1.GetSelf(J) then
    V1.FType._Add(J, V2);
end;

procedure variant_fill_variant(V1, V2: TLiValue);
var
  G: TLiGenerate;
  J: pointer;
begin
  if V1.GetSelf(J) then
  begin
    G := GetGenerate(V2);
    if G <> nil then
    try
      while G.GetNext do
        V1.FType._Add(J, G);
    finally
      G.Free;
    end;
  end;
end;

procedure string_add_string(V1, V2: TLiValue);
begin
  V1.SetAsString(V1.AsString + V2.AsString);
end;

procedure string_mul_int(V1, V2: TLiValue);
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

procedure string_in_string(V1, V2: TLiValue);
begin
  V1.SetAsBoolean(Pos(V1.AsString, V2.AsString) > 0);
end;

procedure string_in_hashed(V1, V2: TLiValue);
var
  H: TLiHash;
begin
  H := V2.AsHash;
  V1.SetAsBoolean((H <> nil) and H.Has(V1.AsString));
end;

procedure string_like_string(V1, V2: TLiValue);
begin
  V1.SetAsBoolean(MatchPatten(V1.AsString, V2.AsString));
end;

procedure int_neg_int(V1, V2: TLiValue);
begin
  V1.FValue.VInteger := - V1.FValue.VInteger;
end;

procedure int_not_int(V1, V2: TLiValue);
begin
  V1.FValue.VInteger := not V1.FValue.VInteger;
end;

procedure int_add_int(V1, V2: TLiValue);
begin
  Inc(V1.FValue.VInteger, V2.FValue.VInteger);
end;

procedure int_add_float(V1, V2: TLiValue);
begin
  V1.FType := my_float;
  V1.FValue.VFloat := V1.FValue.VInteger + V2.FValue.VFloat;
end;

procedure int_add_money(V1, V2: TLiValue);
begin
  V1.FType := my_curr;
  V1.FValue.VCurrency := V1.FValue.VInteger + V2.FValue.VCurrency;
end;

procedure int_dec_int(V1, V2: TLiValue);
begin
  Dec(V1.FValue.VInteger, V2.FValue.VInteger);
end;

procedure int_dec_float(V1, V2: TLiValue);
begin
  V1.FType := my_float;
  V1.FValue.VFloat := V1.FValue.VInteger - V2.FValue.VFloat;
end;

procedure int_dec_money(V1, V2: TLiValue);
begin
  V1.FType := my_curr;
  V1.FValue.VCurrency := V1.FValue.VInteger - V2.FValue.VCurrency;
end;

procedure int_mul_int(V1, V2: TLiValue);
begin
  V1.FValue.VInteger := V1.FValue.VInteger * V2.FValue.VInteger;
end;

procedure int_mul_float(V1, V2: TLiValue);
begin
  V1.FType := my_float;
  V1.FValue.VFloat := V1.FValue.VInteger * V2.FValue.VFloat;
end;

procedure int_mul_money(V1, V2: TLiValue);
begin
  V1.FType := my_curr;
  V1.FValue.VCurrency := V1.FValue.VInteger * V2.FValue.VCurrency;
end;

procedure int_div_int(V1, V2: TLiValue);
begin
  V1.FType := my_int;
  V1.FValue.VInteger := V1.FValue.VInteger div V2.FValue.VInteger;
end;

procedure int_divf_int(V1, V2: TLiValue);
begin
  V1.FType := my_float;
  V1.FValue.VFloat := V1.FValue.VInteger / V2.FValue.VInteger;
end;

procedure int_divf_float(V1, V2: TLiValue);
begin
  V1.FType := my_float;
  V1.FValue.VFloat := V1.FValue.VInteger / V2.FValue.VFloat;
end;

procedure int_divf_money(V1, V2: TLiValue);
begin
  V1.FType := my_float;
  V1.FValue.VFloat := V1.FValue.VInteger / V2.FValue.VCurrency;
end;

procedure int_mod_int(V1, V2: TLiValue);
begin
  V1.FValue.VInteger := V1.FValue.VInteger mod V2.FValue.VInteger;
end;

procedure int_xor_int(V1, V2: TLiValue);
begin
  V1.FValue.VInteger := V1.FValue.VInteger xor V2.FValue.VInteger;
end;

procedure int_shl_int(V1, V2: TLiValue);
begin
  V1.FValue.VInteger := V1.FValue.VInteger shl V2.FValue.VInteger;
end;

procedure int_shr_int(V1, V2: TLiValue);
begin
  V1.FValue.VInteger := V1.FValue.VInteger shr V2.FValue.VInteger;
end;

procedure int_in_int(V1, V2: TLiValue);
begin
  V1.SetAsBoolean((V1.FValue.VInteger >= 0) and (V1.FValue.VInteger < V2.FValue.VInteger));
end;

procedure float_neg_float(V1, V2: TLiValue);
begin
  V1.FValue.VFloat := - V1.FValue.VFloat;
end;

procedure float_add_float(V1, V2: TLiValue);
begin
  V1.FValue.VFloat := V1.FValue.VFloat + V2.FValue.VFloat;
end;

procedure float_add_int(V1, V2: TLiValue);
begin
  V1.FValue.VFloat := V1.FValue.VFloat + V2.FValue.VInteger;
end;

procedure float_add_money(V1, V2: TLiValue);
begin
  V1.FType := my_curr;
  V1.FValue.VCurrency := V1.FValue.VFloat + V2.FValue.VCurrency;
end;

procedure float_dec_float(V1, V2: TLiValue);
begin
  V1.FValue.VFloat := V1.FValue.VFloat - V2.FValue.VFloat;
end;

procedure float_dec_int(V1, V2: TLiValue);
begin
  V1.FValue.VFloat := V1.FValue.VFloat - V2.FValue.VInteger;
end;

procedure float_dec_money(V1, V2: TLiValue);
begin
  V1.FType := my_curr;
  V1.FValue.VCurrency := V1.FValue.VFloat - V2.FValue.VCurrency;
end;

procedure float_mul_float(V1, V2: TLiValue);
begin
  V1.FValue.VFloat := V1.FValue.VFloat * V2.FValue.VFloat;
end;

procedure float_mul_int(V1, V2: TLiValue);
begin
  V1.FValue.VFloat := V1.FValue.VFloat * V2.FValue.VInteger;
end;

procedure float_mul_money(V1, V2: TLiValue);
begin
  V1.FType := my_curr;
  V1.FValue.VCurrency := V1.FValue.VFloat * V2.FValue.VCurrency;
end;

procedure float_divf_float(V1, V2: TLiValue);
begin
  V1.FValue.VFloat := V1.FValue.VFloat / V2.FValue.VFloat;
end;

procedure float_divf_int(V1, V2: TLiValue);
begin
  V1.FValue.VFloat := V1.FValue.VFloat / V2.FValue.VInteger;
end;

procedure float_divf_money(V1, V2: TLiValue);
begin
  V1.FValue.VFloat := V1.FValue.VFloat / V2.FValue.VCurrency;
end;

procedure money_neg_money(V1, V2: TLiValue);
begin
  V1.FValue.VCurrency := - V1.FValue.VCurrency;
end;

procedure money_add_money(V1, V2: TLiValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency + V2.FValue.VCurrency;
end;

procedure money_add_int(V1, V2: TLiValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency + V2.FValue.VInteger;
end;

procedure money_add_float(V1, V2: TLiValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency + V2.FValue.VFloat;
end;

procedure money_dec_money(V1, V2: TLiValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency - V2.FValue.VCurrency;
end;

procedure money_dec_int(V1, V2: TLiValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency - V2.FValue.VInteger;
end;

procedure money_dec_float(V1, V2: TLiValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency - V2.FValue.VFloat;
end;

procedure money_mul_money(V1, V2: TLiValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency * V2.FValue.VCurrency;
end;

procedure money_mul_int(V1, V2: TLiValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency * V2.FValue.VInteger;
end;

procedure money_mul_float(V1, V2: TLiValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency * V2.FValue.VFloat;
end;

procedure money_divf_money(V1, V2: TLiValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency / V2.FValue.VCurrency;
end;

procedure money_divf_int(V1, V2: TLiValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency / V2.FValue.VInteger;
end;

procedure money_divf_float(V1, V2: TLiValue);
begin
  V1.FValue.VCurrency := V1.FValue.VCurrency / V2.FValue.VFloat;
end;

procedure time_add_int(V1, V2: TLiValue);
begin
  V1.FValue.VTime := V1.FValue.VTime + V2.FValue.VInteger;
end;

procedure time_add_float(V1, V2: TLiValue);
begin
  V1.FValue.VTime := V1.FValue.VTime + V2.FValue.VFloat;
end;

procedure time_dec_int(V1, V2: TLiValue);
begin
  V1.FValue.VTime := V1.FValue.VTime - V2.FValue.VInteger;
end;

procedure time_dec_float(V1, V2: TLiValue);
begin
  V1.FValue.VTime := V1.FValue.VTime - V2.FValue.VFloat;
end;

procedure bool_not_bool(V1, V2: TLiValue);
begin
  V1.FValue.VBoolean := not V1.FValue.VBoolean;
end;

procedure bool_xor_bool(V1, V2: TLiValue);
begin
  V1.FValue.VBoolean := V1.FValue.VBoolean xor V2.FValue.VBoolean;
end;

procedure list_add_list(V1, V2: TLiValue);
var
  A, L: TLiList;
  I: integer;
begin
  A := TLiList.Create;
  try
    L := V1.AsList;
    for I := 0 to L.Count - 1 do A.Add(L[I]);
    L := V2.AsList;
    for I := 0 to L.Count - 1 do A.Add(L[I]);
  finally
    V1.SetAsList(A);
  end;
end;

procedure list_dec_list(V1, V2: TLiValue);
var
  A, L, R: TLiList;
  I: integer;
begin
  A := TLiList.Create;
  try
    L := V1.AsList;
    R := V2.AsList;
    for I := 0 to L.Count - 1 do
      if ((R = nil) or (R.IndexOf(L[I]) < 0)) and (A.IndexOf(L[I]) < 0) then
        A.Add(L[I]);
  finally
    V1.SetAsList(A);
  end;
end;

procedure list_mul_list(V1, V2: TLiValue);
var
  A, L, R: TLiList;
  I: integer;
begin
  A := TLiList.Create;
  try
    L := V1.AsList;
    R := V2.AsList;
    for I := 0 to L.Count - 1 do
      if (R <> nil) and (R.IndexOf(L[I]) >= 0) and (A.IndexOf(L[I]) < 0) then
        A.Add(L[I]);
  finally
    V1.SetAsList(A);
  end;
end;

procedure list_shi_variant(V1, V2: TLiValue);
var
  L: TLiList;
begin
  if V1.GetSelf(L) then L.Add(V2);
end;

procedure list_fill_variant(V1, V2: TLiValue);
var
  G: TLiGenerate;
  L: TLiList;
begin
  if V1.GetSelf(L) then
  begin
    G := GetGenerate(V2);
    if G <> nil then
    try
      while G.GetNext do L.Add(G);
    finally
      G.Free;
    end;
  end;
end;

procedure enumset_add_enumset(V1, V2: TLiValue);
begin
  V1.AsEnumSet.Add(V2.AsEnumSet).SetValue(V1);
end;

procedure enumset_dec_enumset(V1, V2: TLiValue);
begin
  V1.AsEnumSet.Dec(V2.AsEnumSet).SetValue(V1);
end;

procedure enumset_mul_enumset(V1, V2: TLiValue);
begin
  V1.AsEnumSet.Mul(V2.AsEnumSet).SetValue(V1);
end;

procedure enumset_not_enumset(V1, V2: TLiValue);
begin
  V1.AsEnumSet.NotAll.SetValue(V1);
end;

// compare ---------------------------------------------------------------------

function compare_variant_variant(V1, V2: TLiValue): TLiCompare;
begin
  if (V1.FType = V2.FType) and V1.FType.IsObject and (V1.FValue.VObject = V2.FValue.VObject) then
    Result := crEqual else
    Result := crDiff;
end;

function compare_variant_nil(V1, V2: TLiValue): TLiCompare;
begin
  if V1.IsNil or (V1.IsObject and (V1.FValue.VObject = nil)) then
    Result := crEqual else
    Result := crDiff;
end;

function compare_nil_variant(V1, V2: TLiValue): TLiCompare;
begin
  if V2.IsNil or (V2.IsObject and (V2.FValue.VObject = nil)) then
    Result := crEqual else
    Result := crDiff;
end;

function compare_string_string(V1, V2: TLiValue): TLiCompare;
begin
  Result := CompareString(V1.AsString, V2.AsString);
end;

function compare_char_char(V1, V2: TLiValue): TLiCompare;
begin
  Result := CompareChar(V1.FValue.VChar[0], V2.FValue.VChar[0]);
end;

function compare_int_int(V1, V2: TLiValue): TLiCompare;
begin
  Result := CompareInt64(V1.FValue.VInteger, V2.FValue.VInteger);
end;

function compare_int_float(V1, V2: TLiValue): TLiCompare;
begin
  Result := CompareFloat(V1.FValue.VInteger, V2.FValue.VFloat);
end;

function compare_int_money(V1, V2: TLiValue): TLiCompare;
begin
  Result := CompareMoney(V1.FValue.VInteger, V2.FValue.VCurrency);
end;

function compare_int_time(V1, V2: TLiValue): TLiCompare;
begin
  Result := CompareFloat(V1.FValue.VInteger, V2.FValue.VTime);
end;

function compare_float_float(V1, V2: TLiValue): TLiCompare;
begin
  Result := CompareFloat(V1.FValue.VFloat, V2.FValue.VFloat);
end;

function compare_float_int(V1, V2: TLiValue): TLiCompare;
begin
  Result := CompareFloat(V1.FValue.VFloat, V2.FValue.VInteger);
end;

function compare_float_money(V1, V2: TLiValue): TLiCompare;
begin
  Result := CompareMoney(V1.FValue.VFloat, V2.FValue.VCurrency);
end;

function compare_float_time(V1, V2: TLiValue): TLiCompare;
begin
  Result := CompareFloat(V1.FValue.VFloat, V2.FValue.VTime);
end;

function compare_money_money(V1, V2: TLiValue): TLiCompare;
begin
  Result := CompareMoney(V1.FValue.VCurrency, V2.FValue.VCurrency);
end;

function compare_money_int(V1, V2: TLiValue): TLiCompare;
begin
  Result := CompareMoney(V1.FValue.VCurrency, V2.FValue.VInteger);
end;

function compare_money_float(V1, V2: TLiValue): TLiCompare;
begin
  Result := CompareMoney(V1.FValue.VCurrency, V2.FValue.VFloat);
end;

function compare_time_time(V1, V2: TLiValue): TLiCompare;
begin
  Result := CompareFloat(V1.FValue.VTime, V2.FValue.VTime);
end;

function compare_time_int(V1, V2: TLiValue): TLiCompare;
begin
  Result := CompareFloat(V1.FValue.VTime, V2.FValue.VInteger);
end;

function compare_time_float(V1, V2: TLiValue): TLiCompare;
begin
  Result := CompareFloat(V1.FValue.VTime, V2.FValue.VFloat);
end;

function compare_bool_bool(V1, V2: TLiValue): TLiCompare;
begin
  Result := CompareInteger(Ord(V1.FValue.VBoolean), Ord(V2.FValue.VBoolean));
end;

function compare_enum_enum(V1, V2: TLiValue): TLiCompare;
begin
  Result := CompareInteger(V1.AsEnum.FValue, V2.AsEnum.FValue);
end;

function compare_enumset_enumset(V1, V2: TLiValue): TLiCompare;
begin
  if V1.AsEnumSet.Equal(V2.AsEnumSet) then
    Result := crEqual else
    Result := crDiff;
end;

// system.function -------------------------------------------------------------

procedure pp_system_halt(const Param: TLiParam);
var
  cntx: TLiContext;
begin
  cntx := Param.FContext;
  if Param.FPrmc > 0 then
    cntx.FResult.SetValue(Param[0]);
  cntx.FHalted := true;
  cntx.FError.Clear;
  cntx.Terminate;
end;

procedure pp_system_exit(const Param: TLiParam);
begin
  Param.FContext.FState := csExit;
  if Param.FPrmc > 0 then
    Param.FPrev.FResult.SetValue(Param[0]);
end;

procedure pp_system_break(const Param: TLiParam);
begin
  Param.FContext.FState := csBreak;
end;

procedure pp_system_continue(const Param: TLiParam);
begin
  Param.FContext.FState := csContinue;
end;

procedure pp_system_compile(const Param: TLiParam);
var
  F: TLiFunc;
  L: TStrings;
  I: integer;
  A: array of string;
  T: array of TLiType;
  V: boolean;
begin
  if Param.FPrmc = 1 then
  begin
    F := Param.FContext.Compile(Param[0].AsString);
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
      if ParamOK(A, T, false, V) then
      begin
        F := Param.FContext.Compile(Param[0].AsString);
        for I := 0 to L.Count - 1 do
          F.FParams.Add(L[I], my_variant);
        F.FHasVarArg := V;
        Param.Result.AsFunc := F;
      end
      else Param.Error('invalid arguments: ''%s''', [Param[1].AsString]);
    finally
      L.Free;
    end;
  end;
end;

procedure pp_system_eval(const Param: TLiParam);
var
  code: string;
  func: TLiFunc;
begin
  code := TrimRight(Param[0].AsString);
  if code <> '' then
  begin
    func := Param.FContext.Compile('exit(' + code + ');');
    if func <> nil then
      Param.FToken.ExecFunc(func, Param, Param.FResult, nil);
  end
  else Param.Result.SetNil;
end;

procedure pp_system_find(const Param: TLiParam);
var
  S: string;
begin
  S := Trim(Param[0].AsString);
  if Param.FContext.CodeFunc.FindSave(S, Param, Param.FResult) = fiNone then
    if not Param.FContext.Resolve(S, Param.FResult) then
      Param.Result.SetNil;
end;

procedure pp_system_typeof(const Param: TLiParam);
begin
  Param.Result.AsType := Param[0].AsType;
end;

procedure pp_system_moduleof(const Param: TLiParam);
var
  M: TLiModule;
begin
  M := Param[0].AsModule;
  if M = nil then
    M := Param[0].FType.FModule;
  Param.Result.AsModule := M;
end;

procedure pp_system_fileof(const Param: TLiParam);
var
  M: TLiModule;
begin
  M := Param[0].AsModule;
  if M = nil then
    M := Param[0].FType.FModule;
  Param.Result.AsString := M.FFileName;
end;

procedure pp_system_collectGarbage(const Param: TLiParam);
begin
  Param.Result.AsInteger := my_gcman.Collect;
end;

procedure pp_system_inc(const Param: TLiParam);
var
  T: TLiType;
  V: TLiValue;
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

procedure pp_system_dec(const Param: TLiParam);
var
  T: TLiType;
  V: TLiValue;
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

procedure pp_system_readln(const Param: TLiParam);
var
  S: string;
  T: TLiType;
  V: TLiValue;
begin
  Param.FContext.Readln(S);
  V := Param.GetVarbValue(0, T);
  if V <> nil then
  begin
    V.SetAsString(S);
    T._Convert(V);
    Param.Result.SetValue(V);
  end
  else Param.Result.AsString := S;
end;

procedure pp_system_write(const Param: TLiParam);
begin
  Param.FContext.Write(Param[0].AsString);
end;

procedure pp_system_writeln(const Param: TLiParam);
begin
  Param.FContext.Write(Param[0].AsString);
  Param.FContext.Write(sLineBreak);
end;

procedure pp_system_length(const Param: TLiParam);
var
  O: pointer;
  T: TLiType;
begin
  T := Param[0].GetTOA(O);
  Param.Result.AsInteger := T._Length(O);
end;

procedure pp_system_pass(const Param: TLiParam);
begin
  { do nothing }
end;

procedure pp_system_decompile(const Param: TLiParam);
var
  L: TStringList;
  T: TLiType;
  O: pointer;
begin
  L := TStringList.Create;
  try
    T := Param[0].GetTOA(O);
    if O <> nil then
      if T = my_func then
        TLiFunc(O).Decompile(0, L);
    Param.Result.AsString := L.Text;
  finally
    L.Free;
  end;
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

function AddModule(const Name: string): TLiModule;
begin
  if IsID(Name) then
  begin
    Result := my_modules.Find(Name);
    if Result = nil then
      Result := TLiModule.Create(Name);
  end
  else Result := nil;
end;

function Command: integer;
var
  E: TLiContext;
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

  E := TLiContext.Create(nil);
  try
    if index <= ParamCount then
    begin
      E.ExecuteFrom(index);
      if pause then do_pause;
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

function OperatorStr(OP: TLiOperator): string;
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
  I: TLiSymbol;
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
  R: TLiCompare;
begin
  R := TLiValue(V1).Compare(TLiValue(V2));
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

function FormatValue(V: TLiValue): string;
begin
  if V.FType = my_char then
    Result := FormatChar(V.FValue.VChar[0]) else
  if V.FType = my_string then
    Result := FormatString(V.AsString) else
    Result := V.AsString;
end;

procedure ErrorConvert(AType, NewType: TLiType);
begin
  Throw('can not convert %s to %s', [AType.FName, NewType.FName]);
end;

function GetGenerate(Value: TLiValue): TLiGenerate;
begin
  if Value <> nil then
    Result := Value.FType._Generate(Value.GetOA) else
    Result := nil;
end;

function GetGenerate(V1, V2: TLiValue; Upto: boolean): TLiGenerate;
var
  P: TLiCompare;
begin
  Result := nil;
  if V1.FType = V2.FType then
  begin
    P := V1.Compare(V2);
    if (P = crEqual) or (Upto and (P = crLess)) or (not Upto and (P = crMore)) then
      case V1.FType.FTID of
        TID_INTEGER: Result := TLiGenerate_Int64.CreateIn(V1.AsInteger, V2.AsInteger, Upto);
        TID_CHAR   : Result := TLiGenerate_Char.CreateIn(V1.AsChar, V2.AsChar, Upto);
        TID_BOOLEAN: Result := TLiGenerate_Boolean.CreateIn(V1.AsBoolean, V2.AsBoolean, Upto);
        else if V1.FType.IsEnum then
          Result := TLiGenerate_enum.CreateIn(V1.AsEnum, V2.AsEnum, Upto);
      end;
  end;
end;

function Margin(Level: integer): string;
begin
  Result := StringOfChar(' ', Level * 2);
end;

procedure FreeOperates(var Head: PLiOperate);
begin
  if Head <> nil then
  begin
    FreeOperates(Head^.o_next);
    MemFree(Head, sizeof(RLiOperate));
    Head := nil;
  end;
end;

procedure FreeCompares(var Head: PLiCompare);
begin
  if Head <> nil then
  begin
    FreeCompares(Head^.c_next);
    MemFree(Head, sizeof(RLiCompare));
    Head := nil;
  end;
end;

procedure FreeConverts(var Head: PLiConvert);
begin
  if Head <> nil then
  begin
    FreeConverts(Head^.c_next);
    MemFree(Head, sizeof(RLiConvert));
    Head := nil;
  end;
end;

function ParamOK(const Names: array of string;
                 const Types: array of TLiType;
                 IsMethod: boolean;
                 var HasVarArg: boolean): boolean;
var
  I, L, X: integer;
begin
  Result := false;
  HasVarArg := false;
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
      else
      if (I = L - 1) and IsVarArg(Names[I], Types[I]) then
        HasVarArg := true else
        Exit;
    end;
    Result := true;
  end;
end;

function AddFunc(const Name: string; T: TLiType;
  const ParamNames: array of string; const ParamTypes: array of TLiType;
  Proc: TLiLyseeProc; Data: pointer): TLiFunc;
begin
  Result := my_system.AddFunc(Name, T, ParamNames, ParamTypes, Proc, Data);
end;

function AddFunc(const Name: string;
  const ParamNames: array of string; const ParamTypes: array of TLiType;
  Proc: TLiLyseeProc; Data: pointer): TLiFunc;
begin
  Result := AddFunc(Name, my_nil, ParamNames, ParamTypes, Proc, Data);
end;

function AddFunc(const Name: string; T: TLiType;
  Proc: TLiLyseeProc; Data: pointer): TLiFunc;
begin
  Result := AddFunc(Name, T, [], [], Proc, Data);
end;

function AddFunc(const Name: string;
  Proc: TLiLyseeProc; Data: pointer): TLiFunc;
begin
  Result := AddFunc(Name, my_nil, [], [], Proc, Data);
end;

function AddMethod(const AClass: TLiType;
                   const AName: string;
                   const AType: TLiType;
                   const ParamNames: array of string;
                   const ParamTypes: array of TLiType;
                   const AProc: TLiLyseeProc): TLiFunc;
var
  I: integer;
  E: boolean;
begin
  Result := nil;
  E := false;
  if (AClass <> nil) and AClass.IsObject then
    if (AType <> nil) and (AName <> '') and Assigned(AProc) then
      if AClass.FindMethod(AName) = nil then
        if ParamOK(ParamNames, ParamTypes, true, E) then
          if not MatchID(AName, 'Create') or (AType = AClass) then
          begin
            Result := TLiFunc.CreateMethod(AName, AClass, AProc);
            Result.FResultType := AType;
            Result.FHasVarArg := E;
            if MatchID(AName, 'Create') then
              AClass.FConstructer := Result else
              Result.FParams.Add('Self', AClass);
            for I := 0 to Length(ParamNames) - 1 do
              Result.FParams.Add(ParamNames[I], ParamTypes[I]);
          end;
end;

function AddMethod(const AClass: TLiType;
                   const AName: string;
                   const ParamNames: array of string;
                   const ParamTypes: array of TLiType;
                   const AProc: TLiLyseeProc): TLiFunc;
begin
  Result := AddMethod(AClass, AName, my_nil, ParamNames, ParamTypes, AProc);
end;

function AddMethod(const AClass: TLiType;
                   const AName: string;
                   const AType: TLiType;
                   const AProc: TLiLyseeProc): TLiFunc;
begin
  Result := AddMethod(AClass, AName, AType, [], [], AProc);
end;

function AddMethod(const AClass: TLiType;
                   const AName: string;
                   const AProc: TLiLyseeProc): TLiFunc;
begin
  Result := AddMethod(AClass, AName, my_nil, [], [], AProc);
end;

function SetupProp(const AClass: TLiType;
                   const AName: string;
                   const AType: TLiType;
                   const IndexNames: array of string;
                   const IndexTypes: array of TLiType;
                   const GetProc, SetProc: TLiLyseeProc;
                     var GetFunc, SetFunc: TLiFunc): boolean;

  procedure setup_get(const FuncName: string);
  var
    I: integer;
  begin
    if AClass.FindMethod(FuncName) = nil then
    begin
      GetFunc := TLiFunc.CreateMethod(FuncName, AClass, GetProc);
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
        SetFunc := TLiFunc.CreateMethod(FuncName, AClass, SetProc);
        SetFunc.FResultType := my_nil;
        SetFunc.FParams.Add('Self', AClass);
        for I := 0 to Length(IndexNames) - 1 do
          SetFunc.FParams.Add(IndexNames[I], IndexTypes[I]);
        SetFunc.FParams.Add('NewValue', AType);
      end;
  end;

var
  E: boolean;
  L: integer;
begin
  GetFunc := nil;
  SetFunc := nil;
  E := false;
  L := Length(IndexNames);
  if (AClass <> nil) and AClass.IsObject then
    if (AType <> nil) and (AType <> my_nil) and Assigned(GetProc) then
      if IsID(AName) or ((AName = '') and (L > 0)) then
        if ParamOK(IndexNames, IndexTypes, true, E) and not E then
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

function SetupProp(const AClass: TLiType;
                   const AName: string;
                   const AType: TLiType;
                   const IndexNames: array of string;
                   const IndexTypes: array of TLiType;
                   const GetProc: TLiLyseeProc;
                     var GetFunc: TLiFunc): boolean;
var
  S: TLiFunc;
begin
  Result := SetupProp(AClass, AName, AType, IndexNames, IndexTypes,
                      GetProc, nil, GetFunc, S);
end;

function SetupProp(const AClass: TLiType;
                   const AName: string;
                   const AType: TLiType;
                   const IndexNames: array of string;
                   const IndexTypes: array of TLiType;
                   const GetProc, SetProc: TLiLyseeProc): boolean;
var
  G, S: TLiFunc;
begin
  Result := SetupProp(AClass, AName, AType, IndexNames, IndexTypes,
                      GetProc, SetProc, G, S);
end;

function SetupProp(const AClass: TLiType;
                   const AName: string;
                   const AType: TLiType;
                   const GetProc, SetProc: TLiLyseeProc): boolean;
var
  G, S: TLiFunc;
begin
  Result := SetupProp(AClass, AName, AType, [], [],
                      GetProc, SetProc, G, S);
end;

{ TLiError }

procedure TLiError.Clear;
begin
  FErrID := '';
  FMsg := '';
  FModule := '';
  FFileName := '';
  FRow := 0;
  FCol := 0;
end;

constructor TLiError.Create(AContext: TLiContext);
begin
  FContext := AContext;
end;

function TLiError.GetText: string;
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

procedure TLiError.Runtime(const Msg, Module, FileName: string; Row, Col: integer);
begin
  FErrID := 'RuntimeError';
  FMsg := Msg;
  FModule := Module;
  FFileName := FileName;
  FRow := Row;
  FCol := Col;
  FContext.FExcepted := true;
end;

procedure TLiError.Syntax(const Msg, Module, FileName: string; Row, Col: integer);
begin
  FErrID := 'SyntaxError';
  FMsg := Msg;
  FModule := Module;
  FFileName := FileName;
  FRow := Row;
  FCol := Col;
  FContext.FExcepted := true;
  Abort;
end;

{ TLiContext }

procedure TLiContext.BeginExecute;
begin
  if Assigned(FOnExecuting) then FOnExecuting(Self);
end;

procedure TLiContext.EndExecute;
begin
  if Assigned(FOnExecuted) then FOnExecuted(Self);
end;

function TLiContext.ExecuteFile(const FileName: string): boolean;
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

function TLiContext.ExecuteFrom(StartParamIndex: integer): boolean;
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

procedure TLiContext.Readln(var Text: string);
begin
  if Assigned(FOnReadln) then
    FOnReadln(Self, Text) else
    System.Readln(Text);
end;

procedure TLiContext.RollbackRemove(AObj: TObject);
begin
  if FRollbacks <> nil then
    FRollbacks.Remove(AObj);
end;

procedure TLiContext.SetMainFile(const Value: string);
begin
  FMainFile := ExpandFileName(Trim(Value));
end;

function TLiContext.Terminate: boolean;
begin
  Result := FTerminated;
  FTerminated := true;
end;

procedure TLiContext.Write(const Text: string);
begin
  if Assigned(FOnWrite) then
    FOnWrite(Self, Text) else
    System.Write(Text);
end;

procedure TLiContext.Write(const Text: string; const Args: array of const);
begin
  Write(Format(Text, Args));
end;

procedure TLiContext.Writeln;
begin
  Write(sLineBreak);
end;

procedure TLiContext.Writeln(const Text: string);
begin
  Write(Text);
  Write(sLineBreak);
end;

procedure TLiContext.Writeln(const Text: string; const Args: array of const);
begin
  Writeln(Format(Text, Args));
end;

function TLiContext.GetTerminated: boolean;
begin
  Result := FTerminated or not GetRunning;
end;

function TLiContext.GetTerminating: boolean;
begin
  Result := FTerminated and GetRunning;
end;

procedure TLiContext.MarkSurvived;
var
  I: integer;
  M: TLiModule;
  P: TLiParam;
begin
  FArgs.MarkForSurvive;
  FResult.MarkForSurvive;
  FMainLocals.MarkForSurvive;

  P := FCurrent;
  while P <> nil do
  begin
    P.FResult.MarkForSurvive;
    P.FParams.MarkForSurvive;
    P := P.FPrev;
  end;

  for I := 0 to FModules.Count - 1 do
  begin
    M := FModules[I];
    if M.FContext = Self then
      M.FConsts.MarkForSurvive;
  end;
end;

procedure TLiContext.Clear(WillDestroy: boolean);
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

constructor TLiContext.Create(AOwner: TComponent);
begin
  inherited;
  FResult := TLiValue.Create;
  FError := TLiError.Create(Self);
  FArgs := TLiList.Create;
  FArgs.IncRefcount;
  FMainLocals := TLiList.Create;
  FMainLocals.IncRefcount;
  FModules := TLiModuleList.Create(Self);
  GetMainModule;
  FState := csTerminated;
  FTerminated := true;
  FHalted := false;
end;

destructor TLiContext.Destroy;
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

function TLiContext.Compile(const Code: string): TLiFunc;
begin
  Result := nil;
  Check(FRollbacks = nil, 'invalid embeded compiling');
  FRollbacks := TList.Create;
  try
    if not GetRunning then
    begin
      FError.Clear;
      Result := TLiParser.Create(GetMainModule).ParseAndFree(Code);
      FreeAndNil(FRollbacks);
      FState := csReady;
    end
    else
    if StatusOK then
    begin
      Result := TLiParser.Create(GetCodeModule).ParseAndFree(Code);
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

function TLiContext.GetMainFunc: TLiFunc;
begin
  if FMainFunc = nil then
    FMainFunc := TLiFunc.Create(LSE_MAIN, FMainModule, nil);
  Result := FMainFunc;
end;

function TLiContext.GetMainModule: TLiModule;
begin
  if FMainModule = nil then
  begin
    FMainModule := TLiModule.CreateEx(LSE_MAIN, Self);
    FMainModule.FFileName := FMainFile;
  end;
  Result := FMainModule;
end;

function TLiContext.GetMainToken: TLiToken;
begin
  if FMainToken = nil then
  begin
    FMainToken := TLiToken.Create;
    FMainToken.FSym := syCall;
  end;
  Result := FMainToken;
end;

function TLiContext.GetModuleFile(const AName: string): string;
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

function TLiContext.GetReady: boolean;
begin
  Result := (FState = csReady);
end;

function TLiContext.GetRunning: boolean;
begin
  Result := (FState >= csRunning);
end;

procedure TLiContext.ExecMain;
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
      FCurrent := TLiParam.Create;
      try
        FCurrent.FContext := Self;
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

procedure TLiContext.Rollback;
var
  I: integer;
begin
  if FRollbacks <> nil then
  try
    for I := FRollbacks.Count - 1 downto 0 do
      if I < FRollbacks.Count then
        TLiObject(FRollbacks[I]).Free;
  finally
    FreeAndNil(FRollbacks);
  end;
end;

procedure TLiContext.SetResult(Value: TLiValue);
begin
  if Value <> nil then
    FResult.SetValue(Value) else
    FResult.SetNil
end;

procedure TLiContext.RollbackAdd(AObj: TObject);
begin
  if (FRollbacks <> nil) and (FRollbacks.IndexOf(AObj) < 0) then
    FRollbacks.Add(AObj);
end;

procedure TLiContext.CheckNotRunning;
begin
  Check(not GetRunning, 'current context is running');
end;

function TLiContext.StatusOK: boolean;
begin
  Result := (FState = csRunning) and not (FTerminated or FExcepted);
end;

function TLiContext.Execute(const Code: string): boolean;
var
  F: TLiFunc;
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

function TLiContext.Resolve(const ID: string; Value: TLiValue): boolean;

  function resolve_reserved: boolean;
  begin
    Result := (Length(ID) > 2) and (ID[1] = '_') and (ID[2] = '_');
    if Result then
      if MatchID(ID, '__module')   then Value.SetAsModule(CodeModule) else
      if MatchID(ID, '__file')     then Value.SetAsString(CodeModule.FFileName) else
      if MatchID(ID, '__func')     then Value.SetAsFunc(CodeFunc) else
      if MatchID(ID, '__prmc')     then Value.SetAsInteger(FCurrent.FPrmc) else
      if MatchID(ID, '__args')     then Value.SetAsList(FArgs) else
      if MatchID(ID, '__argc')     then Value.SetAsInteger(FArgs.Count) else
      if MatchID(ID, '__dir')      then Value.SetAsString(GetCurrentDir) else
      if MatchID(ID, '__modules')  then Value.SetAsList(FModules.ToList) else
      if MatchID(ID, '__libs')     then Value.SetAsList(my_modules.ToList) else
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

procedure TLiContext.ForWhileRepeatEnded;
begin
  if FState in [csBreak, csContinue] then FState := csRunning;
end;

function TLiContext.GetCodeFunc: TLiFunc;
var
  P: TLiParam;
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

function TLiContext.GetCodeModule: TLiModule;
var
  F: TLiFunc;
begin
  F := CodeFunc;
  if F <> nil then
    Result := F.FModule else
    Result := nil;
end;

{ TLiType }

function TLiType._IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then Result := 1 else Result := 0;
end;

function TLiType.AddCompare(AType: TLiType; AFunc: TLiCompareFunc): PLiCompare;
begin
  Result := MemAllocZero(sizeof(RLiCompare));
  Result^.c_type := AType;
  Result^.c_compare := AFunc;
  Result^.c_next := FCompare;
  FCompare := Result;
end;

function TLiType.AddConvert(AType: TLiType; AProc: TLiConvertProc): PLiConvert;
begin
  Result := MemAllocZero(sizeof(RLiConvert));
  Result^.c_type := AType;
  Result^.c_convert := AProc;
  Result^.c_next := FConvert;
  FConvert := Result;
end;

function TLiType.RegisterProc(AProc: TLiLyseeObjectProc): integer;
begin
  Result := Length(FProcs);
  SetLength(FProcs, Result + 1);
  FProcs[Result] := AProc;
end;

function TLiType.AddOperate(OP: TLiOperator; AType: TLiType; AProc: TLiOperateProc): PLiOperate;
begin
  Result := MemAllocZero(sizeof(RLiOperate));
  Result^.o_oper := OP;
  Result^.o_type := AType;
  Result^.o_operate := AProc;
  Result^.o_next := FOperate[OP];
  FOperate[OP] := Result;
end;

constructor TLiType.Create(const AName: string; AModule: TLiModule; AParent: TLiType);
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
    FModule.FNeedSetup := true;
    FMethods := TLiFuncList.Create;
  end;
end;

function TLiType.Define(const AProp: string; const AType: TLiType;
  const IndexName: string; const IndexType: TLiType;
  const GetProc, SetProc: TLiLyseeObjectProc): boolean;
begin
  Result := Define(AProp, AType, [IndexName], [IndexType], GetProc, SetProc);
end;

destructor TLiType.Destroy;
var
  I: TLiOperator;
begin
  for I := Low(TLiOperator) to High(TLiOperator) do
    FreeOperates(FOperate[I]);
  FreeCompares(FCompare);
  FreeConverts(FConvert);
  FreeAndNil(FMethods);
  inherited;
end;

function TLiType.GetFullName: string;
begin
  Result := FModule.FName + '.' + FName
end;

procedure TLiType._GcMark(Obj: pointer);
begin
  { do nothing }
end;

function TLiType._Generate(Obj: pointer): TLiGenerate;
begin
  Result := nil;
end;

function TLiType._Length(Obj: pointer): int64;
begin
  Result := 0;
end;

function TLiType.GetMethod(Index: integer): TLiFunc;
begin
  Result := TLiFunc(FMethods[Index]);
end;

function TLiType.GetMethodCount: integer;
begin
  if FMethods <> nil then
    Result := FMethods.Count else
    Result := 0;
end;

function TLiType.IsBasicValue: boolean;
begin
  Result := (FStyle in [tsBasic, tsNil]);
end;

function TLiType.IsEnum: boolean;
begin
  Result := (FStyle = tsEnum);
end;

function TLiType.IsEnumSet: boolean;
begin
  Result := (FStyle = tsEnumSet);
end;

function TLiType.IsChildTypeOf(AType: TLiType): boolean;
begin
  Result := (FParent <> nil) and FParent.IsTypeOf(AType);
end;

function TLiType.IsNil: boolean;
begin
  Result := (Self = my_nil);
end;

function TLiType.IsObject: boolean;
begin
  Result := (FStyle in [tsObject, tsEnum, tsEnumSet]);
end;

function TLiType.IsTypeOf(AType: TLiType): boolean;
begin
  Result := (Self = AType) or IsChildTypeOf(AType);
end;

function TLiType.Method(const AName: string;
                        const AProc: TLiLyseeObjectProc): TLiFunc;
begin
  Result := Method(AName, my_nil, [], [], AProc);
end;

function TLiType.Method(const AName: string;
                        const ParamNames: array of string;
                        const ParamTypes: array of TLiType;
                        const AProc: TLiLyseeObjectProc): TLiFunc;
begin
  Result := Method(AName, my_nil, ParamNames, ParamTypes, AProc);
end;

function TLiType.Method(const AName: string;
                        const AType: TLiType;
                        const ParamNames: array of string;
                        const ParamTypes: array of TLiType;
                        const AProc: TLiLyseeObjectProc): TLiFunc;
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

function TLiType.Method(const AName: string;
                        const AType: TLiType;
                        const AProc: TLiLyseeObjectProc): TLiFunc;
begin
  Result := Method(AName, AType, [], [], AProc);
end;

function TLiType._Add(Obj: pointer; Value: TLiValue): integer;
begin
  Throw('%s is not container, can not add item value', [FName]);
  Result := -1;
end;

function TLiType._AsBoolean(Obj: pointer): boolean;
begin
  Result := (Obj <> nil);
end;

function TLiType._AsChar(Obj: pointer): char;
begin
  ErrorConvert(Self, my_char);
  Result := #0;
end;

function TLiType._AsFloat(Obj: pointer): double;
begin
  ErrorConvert(Self, my_float);
  Result := 0;
end;

function TLiType._AsInteger(Obj: pointer): int64;
begin
  ErrorConvert(Self, my_int);
  Result := 0;
end;

function TLiType._AsCurrency(Obj: pointer): currency;
begin
  ErrorConvert(Self, my_curr);
  Result := 0;
end;

function TLiType._AsString(Obj: pointer): string;
begin
  ErrorConvert(Self, my_string);
  Result := '';
end;

function TLiType._AsTime(Obj: pointer): TDateTime;
begin
  ErrorConvert(Self, my_time);
  Result := 0;
end;

function TLiType._Clear(Obj: pointer): boolean;
begin
  Result := false;
end;

function TLiType.FindCompare(AType: TLiType): PLiCompare;
var
  P: PLiCompare;
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

function TLiType.FindConvert(AType: TLiType): PLiConvert;
begin
  Result := FConvert;
  while Result <> nil do
  begin
    if Result^.c_type = AType then Exit;
    Result := Result^.c_next;
  end;
end;

function TLiType.FindMethod(const AName: string): TLiFunc;
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

function TLiType.FindOperate(OP: TLiOperator; AType: TLiType): PLiOperate;
var
  P: PLiOperate;
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

function TLiType.Prototype(const AName: string): string;
begin
  if (Self <> my_variant) and (AName <> '...') then
    Result := AName + ':' + FName else
    Result := AName;
end;

function TLiType._DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then Result := 1 else Result := 0;
end;

procedure TLiType.SetParent(const Value: TLiType);
begin
  if (FParent = nil) and (Value <> nil) then
    if ClassType.ClassParent = Value.ClassType then
      FParent := Value else
      Throw('%s is not parent of %s', [Value.FName, FName]);
end;

procedure TLiType.Setup;
begin
  { do nothing }
end;

function TLiType.Define(const AProp: string; const AType: TLiType;
  const IndexNames: array of string; const IndexTypes: array of TLiType;
  const GetProc, SetProc: TLiLyseeObjectProc): boolean;
var
  G, S: TLiFunc;
  F, P: TLiLyseeProc;
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

procedure TLiType._SetDefault(Value: TLiValue);
begin
  Value.SetNil;
  Value.FType := Self;
end;

procedure TLiType._Validate(Obj: pointer);
begin
  if Obj = nil then
    Throw('invalid %s instance: nil', [FName]);
end;

procedure TLiType._Convert(Value: TLiValue);
var
  R: PLiConvert;
begin
  if Value.FType <> Self then
  begin
    R := FindConvert(Value.FType);
    if R = nil then
      Value.SetParentType(Self) else // allow inherited
      R^.c_convert(Value);
  end;
end;

function TLiType.Define(const AProp: string; const AType: TLiType;
  const GetProc, SetProc: TLiLyseeObjectProc): boolean;
begin
  Result := Define(AProp, AType, [], [], GetProc, SetProc);
end;

{ TLiType_variant }

procedure TLiType_variant._SetDefault(Value: TLiValue);
begin
  Value.SetNil;
end;

procedure TLiType_variant._Convert(Value: TLiValue);
begin
  { do nothing }
end;

{ TLiType_nil }

function TLiType_nil._AsBoolean(Obj: pointer): boolean;
begin
  Result := false;
end;

function TLiType_nil._AsChar(Obj: pointer): char;
begin
  Result := #0;
end;

function TLiType_nil._AsFloat(Obj: pointer): double;
begin
  Result := 0;
end;

function TLiType_nil._AsInteger(Obj: pointer): int64;
begin
  Result := 0;
end;

function TLiType_nil._AsCurrency(Obj: pointer): currency;
begin
  Result := 0;
end;

function TLiType_nil._AsString(Obj: pointer): string;
begin
  Result := '';
end;

function TLiType_nil._AsTime(Obj: pointer): TDateTime;
begin
  Result := 0;
end;

procedure TLiType_nil._Convert(Value: TLiValue);
begin
  Value.SetNil;
end;

procedure TLiType_nil._SetDefault(Value: TLiValue);
begin
  Value.SetNil;
end;

function TLiType_nil._DecRefcount(Obj: pointer): integer;
begin
  Result := 0;
end;

function TLiType_nil._IncRefcount(Obj: pointer): integer;
begin
  Result := 0;
end;

{ TLiType_char }

function TLiType_char._AsBoolean(Obj: pointer): boolean;
begin
  Result := (PChar(Obj)^ <> #0);
end;

function TLiType_char._AsChar(Obj: pointer): char;
begin
  Result := PChar(Obj)^;
end;

function TLiType_char._AsFloat(Obj: pointer): double;
begin
  Result := _AsInteger(Obj);
end;

function TLiType_char._AsInteger(Obj: pointer): int64;
begin
  Result := StrToInt(_AsString(Obj));
end;

function TLiType_char._AsCurrency(Obj: pointer): currency;
begin
  Result := _AsInteger(Obj);
end;

function TLiType_char._AsString(Obj: pointer): string;
begin
  Result := PChar(Obj)^;
end;

procedure TLiType_char._Convert(Value: TLiValue);
var
  tmpv: char;
begin
  if Value.FType <> Self then
  begin
    tmpv := Value.FType._AsChar(Value.GetOA);
    Value.FType._DecRefcount(Value.FValue.VObject);
    Value.FType := Self;
    Value.FValue.VChar[0] := tmpv;
    Value.FValue.VChar[1] := #0;
  end;
end;

procedure TLiType_char._SetDefault(Value: TLiValue);
begin
  inherited;
  Value.FValue.VChar[0] := #0;
  Value.FValue.VChar[1] := #0;
end;

{ TLiType_string }

procedure TLiType_string.MyGet(const Param: TLiParam);
var
  S: string;
begin
  S := Param[0].AsString;
  Param.Result.AsChar := S[Param[1].AsInteger];
end;

procedure TLiType_string.Setup;
begin
  Define('', my_char, 'Index', my_int, {$IFDEF FPC}@{$ENDIF}MyGet);
  inherited;
end;

function TLiType_string._AsBoolean(Obj: pointer): boolean;
begin
  Result := (Obj <> nil) and (TLiString(Obj).FValue <> '');
end;

function TLiType_string._AsChar(Obj: pointer): char;
begin
  if (Obj <> nil) and (TLiString(Obj).FValue <> '') then
    Result := TLiString(Obj).FValue[1] else
    Result := #0;
end;

function TLiType_string._AsFloat(Obj: pointer): double;
begin
  Result := StrToFloat(_AsString(Obj));
end;

function TLiType_string._AsInteger(Obj: pointer): int64;
begin
  Result := StrToInt(_AsString(Obj));
end;

function TLiType_string._AsCurrency(Obj: pointer): currency;
begin
  Result := StrToCurr(_AsString(Obj));
end;

function TLiType_string._AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLiString(Obj).FValue else
    Result := '';
end;

function TLiType_string._AsTime(Obj: pointer): TDateTime;
begin
  Result := StrToDateTime(_AsString(Obj));
end;

function TLiType_string._DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiString(Obj).DecRefcount else
    Result := 0;
end;

function TLiType_string._Generate(Obj: pointer): TLiGenerate;
var
  S: string;
begin
  S := _AsString(Obj);
  if S <> '' then
    Result := TLiGenerate_string.CreateIn(S) else
    Result := nil;
end;

function TLiType_string._IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiString(Obj).IncRefcount else
    Result := 0;
end;

function TLiType_string._Length(Obj: pointer): int64;
begin
  Result := Length(_AsString(Obj));
end;

procedure TLiType_string._Convert(Value: TLiValue);
var
  tmpv: string;
begin
  if Value.FType <> Self then
  begin
    tmpv := Value.FType._AsString(Value.GetOA);
    Value.FType._DecRefcount(Value.FValue.VObject);
    Value.FType := Self;
    Value.FValue.VObject := TLiString.CreateIncRefcount(tmpv);
  end;
end;

{ TLiType_integer }

function TLiType_integer._AsBoolean(Obj: pointer): boolean;
begin
  Result := (PInt64(Obj)^ <> 0);
end;

function TLiType_integer._AsChar(Obj: pointer): char;
begin
  Result := char(PInt64(Obj)^);
end;

function TLiType_integer._AsFloat(Obj: pointer): double;
begin
  Result := PInt64(Obj)^;
end;

function TLiType_integer._AsInteger(Obj: pointer): int64;
begin
  Result := PInt64(Obj)^;
end;

function TLiType_integer._AsCurrency(Obj: pointer): currency;
begin
  Result := PInt64(Obj)^;
end;

function TLiType_integer._AsString(Obj: pointer): string;
begin
  Result := IntToStr(PInt64(Obj)^);
end;

function TLiType_integer._AsTime(Obj: pointer): TDateTime;
begin
  Result := PInt64(Obj)^;
end;

function TLiType_integer._Generate(Obj: pointer): TLiGenerate;
begin
  if PInt64(Obj)^ > 0 then
    Result := TLiGenerate_int64.CreateIn(PInt64(Obj)^) else
    Result := nil;
end;

procedure TLiType_integer._SetDefault(Value: TLiValue);
begin
  inherited;
  Value.FValue.VInteger := 0;
end;

procedure TLiType_integer._Convert(Value: TLiValue);
var
  tmpv: int64;
begin
  if Value.FType <> Self then
  begin
    tmpv := Value.FType._AsInteger(Value.GetOA);
    Value.FType._DecRefcount(Value.FValue.VObject);
    Value.FType := Self;
    Value.FValue.VInteger := tmpv;
  end;
end;

{ TLiType_float }

function TLiType_float._AsBoolean(Obj: pointer): boolean;
begin
  Result := not IsZero(PDouble(Obj)^);
end;

function TLiType_float._AsFloat(Obj: pointer): double;
begin
  Result := PDouble(Obj)^;
end;

function TLiType_float._AsInteger(Obj: pointer): int64;
begin
  Result := Trunc(PDouble(Obj)^);
end;

function TLiType_float._AsCurrency(Obj: pointer): currency;
begin
  Result := PDouble(Obj)^;
end;

function TLiType_float._AsString(Obj: pointer): string;
begin
  Result := FloatToStr(PDouble(Obj)^);
end;

function TLiType_float._AsTime(Obj: pointer): TDateTime;
begin
  Result := PDouble(Obj)^;
end;

procedure TLiType_float._SetDefault(Value: TLiValue);
begin
  inherited;
  Value.FValue.VFloat := 0;
end;

procedure TLiType_float._Convert(Value: TLiValue);
var
  tmpv: double;
begin
  if Value.FType <> Self then
  begin
    tmpv := Value.FType._AsFloat(Value.GetOA);
    Value.FType._DecRefcount(Value.FValue.VObject);
    Value.FType := Self;
    Value.FValue.VFloat := tmpv;
  end;
end;

{ TLiType_currency }

function TLiType_currency._AsBoolean(Obj: pointer): boolean;
begin
  Result := (PCurrency(Obj)^ <> 0);
end;

function TLiType_currency._AsFloat(Obj: pointer): double;
begin
  Result := PCurrency(Obj)^;
end;

function TLiType_currency._AsInteger(Obj: pointer): int64;
begin
  Result := Trunc(PCurrency(Obj)^);
end;

function TLiType_currency._AsCurrency(Obj: pointer): currency;
begin
  Result := PCurrency(Obj)^;
end;

function TLiType_currency._AsString(Obj: pointer): string;
begin
  Result := CurrToStr(PCurrency(Obj)^);
end;

function TLiType_currency._AsTime(Obj: pointer): TDateTime;
begin
  Result := PCurrency(Obj)^;
end;

procedure TLiType_currency._SetDefault(Value: TLiValue);
begin
  inherited;
  Value.FValue.VCurrency := 0;
end;

procedure TLiType_currency._Convert(Value: TLiValue);
var
  tmpv: currency;
begin
  if Value.FType <> Self then
  begin
    tmpv := Value.FType._AsCurrency(Value.GetOA);
    Value.FType._DecRefcount(Value.FValue.VObject);
    Value.FType := Self;
    Value.FValue.VCurrency := tmpv;
  end;
end;

{ TLiType_time }

function TLiType_time._AsBoolean(Obj: pointer): boolean;
begin
  Result := not IsZero(PDateTime(Obj)^);
end;

function TLiType_time._AsFloat(Obj: pointer): double;
begin
  Result := PDateTime(Obj)^;
end;

function TLiType_time._AsInteger(Obj: pointer): int64;
begin
  Result := Trunc(PDateTime(Obj)^);
end;

function TLiType_time._AsString(Obj: pointer): string;
begin
  Result := DateTimeToStr(PDateTime(Obj)^);
end;

function TLiType_time._AsTime(Obj: pointer): TDateTime;
begin
  Result := PDateTime(Obj)^;
end;

procedure TLiType_time._SetDefault(Value: TLiValue);
begin
  inherited;
  Value.FValue.VTime := 0;
end;

procedure TLiType_time._Convert(Value: TLiValue);
var
  tmpv: TDateTime;
begin
  if Value.FType <> Self then
  begin
    tmpv := Value.FType._AsTime(Value.GetOA);
    Value.FType._DecRefcount(Value.FValue.VObject);
    Value.FType := Self;
    Value.FValue.VTime := tmpv;
  end;
end;

{ TLiType_boolean }

function TLiType_boolean._AsBoolean(Obj: pointer): boolean;
begin
  Result := PBoolean(Obj)^;
end;

function TLiType_boolean._AsInteger(Obj: pointer): int64;
begin
  Result := Ord(PBoolean(Obj)^);
end;

function TLiType_boolean._AsString(Obj: pointer): string;
begin
  Result := IntToStr(Ord(PBoolean(Obj)^));
end;

procedure TLiType_boolean._SetDefault(Value: TLiValue);
begin
  inherited;
  Value.FValue.VBoolean := false;
end;

procedure TLiType_boolean._Convert(Value: TLiValue);
var
  tmpv: boolean;
begin
  if Value.FType <> Self then
  begin
    tmpv := Value.FType._AsBoolean(Value.GetOA);
    Value.FType._DecRefcount(Value.FValue.VObject);
    Value.FType := Self;
    Value.FValue.VBoolean := tmpv;
  end;
end;

{ TLiType_type }

procedure TLiType_type.MyFindMethod(const Param: TLiParam);
begin
  Param.Result.AsFunc := Param[0].AsType.FindMethod(Param[1].AsString);
end;

procedure TLiType_type.MyIsChildTypeOf(const Param: TLiParam);
begin
  Param.Result.AsBoolean := Param[0].AsType.IsChildTypeOf(Param[1].AsType);
end;

procedure TLiType_type.MyIsEnum(const Param: TLiParam);
begin
  Param.Result.AsBoolean := Param[0].AsType.IsEnum;
end;

procedure TLiType_type.MyIsEnumSet(const Param: TLiParam);
begin
  Param.Result.AsBoolean := Param[0].AsType.IsEnumSet;
end;

procedure TLiType_type.MyIsNil(const Param: TLiParam);
begin
  Param.Result.AsBoolean := Param[0].AsType.IsNil;
end;

procedure TLiType_type.MyIsObject(const Param: TLiParam);
begin
  Param.Result.AsBoolean := Param[0].AsType.IsObject;
end;

procedure TLiType_type.MyIsTypeOf(const Param: TLiParam);
begin
  Param.Result.AsBoolean := Param[0].AsType.IsTypeOf(Param[1].AsType);
end;

procedure TLiType_type.MyItemValues(const Param: TLiParam);
var
  T: TLiType;
  O: TLiEnumType;
  L: TLiList;
  I: integer;
begin
  L := Param.Result.NewList;
  T := Param[0].AsType;
  if T.IsEnum then
  begin
    O := TLiEnumType(T);
    for I := 0 to O.GetCount - 1 do
      L.Add.SetTOA(O, O[I]);
  end;
end;

procedure TLiType_type.MyMethods(const Param: TLiParam);
var
  T: TLiType;
  L: TLiList;
  I: integer;
begin
  L := Param.Result.NewList;
  T := Param[0].AsType;
  for I := 0 to T.GetMethodCount - 1 do
    L.Add.AsFunc := T.GetMethod(I);
end;

procedure TLiType_type.MyModule(const Param: TLiParam);
begin
  Param.Result.AsModule := Param[0].AsType.Module;
end;

procedure TLiType_type.MyName(const Param: TLiParam);
begin
  Param.Result.AsString := Param[0].AsType.Name;
end;

procedure TLiType_type.MyParent(const Param: TLiParam);
begin
  Param.Result.AsType := Param[0].AsType.Parent;
end;

procedure TLiType_type.MyPrototype(const Param: TLiParam);
begin
  Param.Result.AsString := Param[0].AsType.Prototype(Param[1].AsString);
end;

procedure TLiType_type.Setup;
begin
  Method('Name', my_string, {$IFDEF FPC}@{$ENDIF}MyName);
  Method('Parent', Self, {$IFDEF FPC}@{$ENDIF}MyParent);
  Method('Module', my_module, {$IFDEF FPC}@{$ENDIF}MyModule);
  Method('Methods', my_list, {$IFDEF FPC}@{$ENDIF}MyMethods);
  Method('IsTypeOf', my_bool, ['AType'], [my_type],
         {$IFDEF FPC}@{$ENDIF}MyIsTypeOf);
  Method('IsChildTypeOf', my_bool, ['AType'], [my_type],
         {$IFDEF FPC}@{$ENDIF}MyIsChildTypeOf);
  Method('IsObject', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsObject);
  Method('IsNil', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsNil);
  Method('IsEnum', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsEnum);
  Method('IsEnumSet', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsEnumSet);
  Method('ItemValues', my_list, {$IFDEF FPC}@{$ENDIF}MyItemValues);
  Method('Prototype', my_string, ['Name'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyPrototype);
  Method('FindMethod', my_func, ['Name'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyFindMethod);
  inherited;
end;

function TLiType_type._AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLiType(Obj).FullName else
    Result := '';
end;

procedure TLiType_type._Convert(Value: TLiValue);
var
  tmpv: TLiType;
begin
  if Value.FType <> Self then
  begin
    tmpv := Value.FType;
    Value.FType._DecRefcount(Value.FValue.VObject);
    Value.FType := Self;
    Value.FValue.VObject := tmpv;
  end;
end;

{ TLiType_func }

procedure TLiType_func.MyIsConstructor(const Param: TLiParam);
var
  F: TLiFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsBoolean := F.IsConstructor;
end;

procedure TLiType_func.MyIsMainFunc(const Param: TLiParam);
var
  F: TLiFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsBoolean := F.IsMainFunc;
end;

procedure TLiType_func.MyIsMethod(const Param: TLiParam);
var
  F: TLiFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsBoolean := F.IsMethod;
end;

procedure TLiType_func.MyAddCode(const Param: TLiParam);
var
  F: TLiFunc;
begin
  if Param.GetChangeAbleFunc(F) then
    Param.Result.AsBoolean := F.AddCode(Param[1].AsString);
end;

procedure TLiType_func.MyAddParam(const Param: TLiParam);
var
  F: TLiFunc;
  S: string;
  T: TLiType;
begin
  if Param.GetChangeAbleFunc(F) then
    if not F.FHasVarArg and (F.FParams.LocalCount = 0) then
    begin
      S := Param[1].AsString;
      if Param.Prmc > 2 then
        T := Param[2].AsType else
        T := my_variant;
      if IsParam(S, T) and not F.FindInside(S) then
      begin
        F.FParams.Add(S, T);
        F.FHasVarArg := (S = '...');
        F.FMinArgs := -1;
        Param.Result.AsBoolean := true;
      end;
    end;
end;

procedure TLiType_func.MyClear(const Param: TLiParam);
var
  F: TLiFunc;
begin
  if Param.GetChangeAbleFunc(F) then
  begin
    F.FHasVarArg := false;
    F.FMinArgs := -1;
    F.FParams.Clear;
    F.FSTMTs.Clear;
  end;
end;

procedure TLiType_func.MyClearCodes(const Param: TLiParam);
var
  F: TLiFunc;
begin
  if Param.GetChangeAbleFunc(F) then
    F.FSTMTs.Clear;
end;

procedure TLiType_func.MyClearParams(const Param: TLiParam);
var
  F: TLiFunc;
begin
  if Param.GetChangeAbleFunc(F) then
  begin
    F.FHasVarArg := false;
    F.FMinArgs := -1;
    F.FParams.Clear;
  end;
end;

procedure TLiType_func.MyGetParamName(const Param: TLiParam);
var
  F: TLiFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsString := F.Params[Param[1].AsInteger].FName;
end;

procedure TLiType_func.MyGetParamType(const Param: TLiParam);
var
  F: TLiFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsType := F.Params[Param[1].AsInteger].FType;
end;

procedure TLiType_func.MyGetResultType(const Param: TLiParam);
var
  F: TLiFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsType := F.ResultType;
end;

procedure TLiType_func.MyIsChangeAble(const Param: TLiParam);
var
  F: TLiFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsBoolean := F.ChangeAble;
end;

procedure TLiType_func.MyModule(const Param: TLiParam);
var
  F: TLiFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsModule := F.FModule;
end;

procedure TLiType_func.MyName(const Param: TLiParam);
var
  F: TLiFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsString := F.Name;
end;

procedure TLiType_func.MyParamCount(const Param: TLiParam);
var
  F: TLiFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsInteger := F.Params.ParamCount;
end;

procedure TLiType_func.MyParamNames(const Param: TLiParam);
var
  F: TLiFunc;
  L: TLiList;
  I: integer;
begin
  if Param.GetSelf(F) then
  begin
    L := Param.Result.NewList;
    for I := 0 to F.Params.ParamCount - 1 do
      L.Add.AsString := F.Params[I].FName;
  end;
end;

procedure TLiType_func.MyParamTypes(const Param: TLiParam);
var
  F: TLiFunc;
  L: TLiList;
  I: integer;
begin
  if Param.GetSelf(F) then
  begin
    L := Param.Result.NewList;
    for I := 0 to F.FParams.ParamCount - 1 do
      L.Add.AsType := F.Params[I].FType;
  end;
end;

procedure TLiType_func.MyParent(const Param: TLiParam);
var
  F: TLiFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsType := F.FParent;
end;

procedure TLiType_func.MyPrototype(const Param: TLiParam);
var
  F: TLiFunc;
begin
  if Param.GetSelf(F) then
    Param.Result.AsString := F.Prototype;
end;

procedure TLiType_func.MySetCode(const Param: TLiParam);
var
  F: TLiFunc;
begin
  if Param.GetChangeAbleFunc(F) then
    Param.Result.AsBoolean := F.SetCode(Param[1].AsString);
end;

procedure TLiType_func.MySetParamName(const Param: TLiParam);
var
  F: TLiFunc;
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
        if X = F.Params.ParamCount - 1 then
          F.FHasVarArg := (S = '...');
      end
      else Param.Error('invalid param name: ' + S);
  end;
end;

procedure TLiType_func.MySetParamType(const Param: TLiParam);
var
  F: TLiFunc;
  X: integer;
  T: TLiType;
begin
  if Param.GetChangeAbleFunc(F) then
  begin
    X := Param[1].AsInteger;
    T := Param[2].AsType;
    if T <> F.Params[X].FType then
      if T = my_nil then
        Param.Error('invalid param type: nil') else
      if F.Params[X].FName = '...' then
        Param.Error('invalid VarArg type: %s', [T.FullName]) else
        F.Params[X].FType := T;
  end;
end;

procedure TLiType_func.MySetResultType(const Param: TLiParam);
var
  F: TLiFunc;
begin
  if Param.GetChangeAbleFunc(F) then
    F.ResultType := Param[1].AsType;
end;

procedure TLiType_func.Setup;
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
  Method('ParamNames', my_list, {$IFDEF FPC}@{$ENDIF}MyParamNames);
  Method('ParamTypes', my_list, {$IFDEF FPC}@{$ENDIF}MyParamTypes);
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

function TLiType_func._AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLiFunc(Obj).Prototype else
    Result := '';
end;

function TLiType_func._DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiFunc(Obj).DecRefcount else
    Result := 0;
end;

function TLiType_func._IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiFunc(Obj).IncRefcount else
    Result := 0;
end;

{ TLiType_module }

procedure TLiType_module.MyConsts(const Param: TLiParam);
var
  M: TLiModule;
begin
  if Param.GetSelf(M) then
    M.FConsts.ListKeys(Param.Result.NewList);
end;

procedure TLiType_module.MyFuncs(const Param: TLiParam);
var
  M: TLiModule;
  L: TLiList;
  I: integer;
begin
  if Param.GetSelf(M) then
  begin
    L := Param.Result.NewList;
    for I := 0 to M.FFuncList.Count - 1 do
      L.Add.SetAsFunc(TLiFunc(M.FFuncList[I]));
  end;
end;

procedure TLiType_module.MyFind(const Param: TLiParam);
var
  M: TLiModule;
begin
  if Param.GetSelf(M) then
    M.FindSave(Param[1].AsString, Param.FResult);
end;

procedure TLiType_module.MyName(const Param: TLiParam);
var
  M: TLiModule;
begin
  if Param.GetSelf(M) then
    Param.Result.AsString := M.FName;
end;

procedure TLiType_module.MyTypes(const Param: TLiParam);
var
  M: TLiModule;
  L: TLiList;
  I: integer;
begin
  if Param.GetSelf(M) then
  begin
    L := Param.Result.NewList;
    for I := 0 to M.TypeCount - 1 do
      L.Add.AsType := M.GetType(I);
  end;
end;

procedure TLiType_module.MyUsings(const Param: TLiParam);
var
  M: TLiModule;
begin
  if Param.GetSelf(M) then
    if M.FModules <> nil then
      Param.Result.AsList := M.FModules.ToList else
      Param.Result.NewList;
end;

procedure TLiType_module.Setup;
begin
  Method('Name', my_string, {$IFDEF FPC}@{$ENDIF}MyName);
  Method('Consts', my_list, {$IFDEF FPC}@{$ENDIF}MyConsts);
  Method('Types', my_list, {$IFDEF FPC}@{$ENDIF}MyTypes);
  Method('Funcs', my_list, {$IFDEF FPC}@{$ENDIF}MyFuncs);
  Method('Usings', my_list, {$IFDEF FPC}@{$ENDIF}MyUsings);
  Method('Find', my_variant, ['Name'], [my_string], {$IFDEF FPC}@{$ENDIF}MyFind);
  inherited;
end;

function TLiType_module._AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLiModule(Obj).FName else
    Result := '';
end;

function TLiType_module._DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiModule(Obj).DecRefcount else
    Result := 0;
end;

function TLiType_module._IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiModule(Obj).IncRefcount else
    Result := 0;
end;

{ TLiType_list }

procedure TLiType_list.MyAdd(const Param: TLiParam);
var
  L, A: TLiList;
  I: integer;
begin
  if Param.GetSelf(L) then
  begin
    L.Add(Param[1]);
    Param.Result.AsInteger := L.Count - 1;
    A := Param[2].AsList;
    if A <> nil then
      for I := 0 to A.Count - 1 do
        L.Add(A[I]);
  end;
end;

procedure TLiType_list.MyAssign(const Param: TLiParam);
var
  L, R: TLiList;
  I: integer;
begin
  if Param.GetSelf(L) then
  begin
    R := Param[1].AsList;
    if L <> R then
    begin
      L.Clear;
      if R <> nil then
        for I := 0 to R.Count - 1 do
          L.Add(R[I]);
    end;
  end;
end;

procedure TLiType_list.MyClear(const Param: TLiParam);
var
  L: TLiList;
begin
  if Param.GetSelf(L) then L.Clear;
end;

procedure TLiType_list.MyCopy(const Param: TLiParam);
var
  L: TLiList;
  X: integer;
  N: integer;
begin
  if Param.GetSelf(L) then
  begin
    X := Param[1].AsInteger;
    N := Param[2].AsInteger;
    Param.Result.AsList := L.Copy(X, N);
  end;
end;

procedure TLiType_list.MyCreate(const Param: TLiParam);
var
  L: TLiList;
begin
  L := TLiList.Create;
  Param.Result.AsList := L;
  L.Count := Max(0, Param[0].AsInteger);
end;

procedure TLiType_list.MyDelete(const Param: TLiParam);
var
  L: TLiList;
begin
  if Param.GetSelf(L) then
    L.Delete(Param[1].AsInteger);
end;

procedure TLiType_list.MyExchange(const Param: TLiParam);
var
  L: TLiList;
begin
  if Param.GetSelf(L) then
    L.Exchange(Param[1].AsInteger, Param[2].AsInteger);
end;

procedure TLiType_list.MyGet(const Param: TLiParam);
var
  L: TLiList;
begin
  if Param.GetSelf(L) then
    Param.Result.SetValue(L[Param[1].AsInteger]);
end;

procedure TLiType_list.MyGetCount(const Param: TLiParam);
var
  L: TLiList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.Count;
end;

procedure TLiType_list.MyIndexOf(const Param: TLiParam);
var
  L: TLiList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.IndexOf(Param[1]);
end;

procedure TLiType_list.MyInsert(const Param: TLiParam);
var
  L: TLiList;
begin
  if Param.GetSelf(L) then
    L.Insert(Param[1].AsInteger, Param[2]);
end;

procedure TLiType_list.MyIsEmpty(const Param: TLiParam);
var
  L: TLiList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsBoolean := (L.Count = 0);
end;

procedure TLiType_list.MyLeft(const Param: TLiParam);
var
  L: TLiList;
  N: integer;
begin
  if Param.GetSelf(L) then
  begin
    N := Param[1].AsInteger;
    Param.Result.AsList := L.CopyLeft(N);
  end;
end;

procedure TLiType_list.MyMove(const Param: TLiParam);
var
  L: TLiList;
begin
  if Param.GetSelf(L) then
    L.Move(Param[1].AsInteger, Param[2].AsInteger);
end;

procedure TLiType_list.MyRemove(const Param: TLiParam);
var
  L: TLiList;
begin
  if Param.GetSelf(L) then L.Remove(Param[1]);
end;

procedure TLiType_list.MyRight(const Param: TLiParam);
var
  L: TLiList;
  N: integer;
begin
  if Param.GetSelf(L) then
  begin
    N := Param[1].AsInteger;
    Param.Result.AsList := L.CopyRight(N);
  end;
end;

procedure TLiType_list.MySet(const Param: TLiParam);
var
  L: TLiList;
begin
  if Param.GetSelf(L) then
    L[Param[1].AsInteger].SetValue(Param[2]);
end;

procedure TLiType_list.MySetCount(const Param: TLiParam);
var
  L: TLiList;
begin
  if Param.GetSelf(L) then
    L.SetCount(Param[1].AsInteger);
end;

procedure TLiType_list.MySort(const Param: TLiParam);
var
  L: TLiList;
begin
  if Param.GetSelf(L) then L.Sort;
end;

procedure TLiType_list.Setup;
begin
  Method('Create', Self, ['_Count'], [my_int], {$IFDEF FPC}@{$ENDIF}MyCreate);
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
  Method('Add', my_int, ['_Value', '...'], [my_variant, Self],
         {$IFDEF FPC}@{$ENDIF}MyAdd);
  Method('IndexOf', my_int, ['Value'], [my_variant],
         {$IFDEF FPC}@{$ENDIF}MyIndexOf);
  Method('Copy', Self, ['Index', 'Count'], [my_int, my_int],
         {$IFDEF FPC}@{$ENDIF}MyCopy);
  Method('Left', Self, ['Count'], [my_int], {$IFDEF FPC}@{$ENDIF}MyLeft);
  Method('Right', Self, ['Count'], [my_int], {$IFDEF FPC}@{$ENDIF}MyRight);
  Method('Assign', ['Source'], [Self], {$IFDEF FPC}@{$ENDIF}MyAssign);
  Define('Count', my_int,
         {$IFDEF FPC}@{$ENDIF}MyGetCount,
         {$IFDEF FPC}@{$ENDIF}MySetCount);
  Define('', my_variant, 'Index', my_int,
         {$IFDEF FPC}@{$ENDIF}MyGet,
         {$IFDEF FPC}@{$ENDIF}MySet);
  inherited;
end;

function TLiType_list._Add(Obj: pointer; Value: TLiValue): integer;
var
  L: TLiList;
begin
  L := TLiList(Obj);
  Result := L.Count;
  L.Add(Value);
end;

function TLiType_list._AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLiList(Obj).AsString else
    Result := '';
end;

function TLiType_list._Clear(Obj: pointer): boolean;
begin
  Result := (Obj <> nil);
  if Result then
    TLiList(Obj).Clear;
end;

function TLiType_list._DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiList(Obj).DecRefcount else
    Result := 0;
end;

procedure TLiType_list._GcMark(Obj: pointer);
begin
  if Obj <> nil then TLiList(Obj).MarkForSurvive;
end;

function TLiType_list._Generate(Obj: pointer): TLiGenerate;
begin
  if (Obj <> nil) and (TLiList(Obj).Count > 0) then
    Result := TLiGenerate_list.CreateIn(TLiList(Obj)) else
    Result := nil;
end;

function TLiType_list._IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiList(Obj).IncRefcount else
    Result := 0;
end;

function TLiType_list._Length(Obj: pointer): int64;
begin
  if Obj <> nil then
    Result := TLiList(Obj).Count else
    Result := 0;
end;

{ TLiType_hash }

procedure TLiType_hash.MyClear(const Param: TLiParam);
var
  H: TLiHash;
begin
  if Param.GetSelf(H) then H.Clear;
end;

procedure TLiType_hash.MyCreate(const Param: TLiParam);
begin
  Param.Result.AsHash := TLiHash.Create;
end;

procedure TLiType_hash.MyGet(const Param: TLiParam);
var
  H: TLiHash;
  V: TLiValue;
begin
  if Param.GetSelf(H) then
  begin
    V := H.Get(Param[1].AsString);
    if V <> nil then
      Param.Result.SetValue(V);
  end;
end;

procedure TLiType_hash.MyHas(const Param: TLiParam);
var
  H: TLiHash;
begin
  if Param.GetSelf(H) then
    Param.Result.AsBoolean := H.Has(Param[1].AsString);
end;

procedure TLiType_hash.MyIsEmpty(const Param: TLiParam);
var
  H: TLiHash;
begin
  if Param.GetSelf(H) then
    Param.Result.AsBoolean := H.IsEmpty;
end;

procedure TLiType_hash.MyKeys(const Param: TLiParam);
var
  H: TLiHash;
  L: TLiList;
begin
  if Param.GetSelf(H) then
  begin
    L := TLiList.Create;
    Param.Result.AsList := L;
    H.ListKeys(L);
  end;
end;

procedure TLiType_hash.MyRemove(const Param: TLiParam);
var
  H: TLiHash;
begin
  if Param.GetSelf(H) then
    H.Remove(Param[1].AsString);
end;

procedure TLiType_hash.MySet(const Param: TLiParam);
var
  H: TLiHash;
begin
  if Param.GetSelf(H) then
    H.Add(Param[1].AsString).SetValue(Param[2]);
end;

procedure TLiType_hash.MyValues(const Param: TLiParam);
var
  H: TLiHash;
  L: TLiList;
begin
  if Param.GetSelf(H) then
  begin
    L := TLiList.Create;
    Param.Result.AsList := L;
    H.ListValues(L);
  end;
end;

procedure TLiType_hash.Setup;
begin
  Method('Create', Self, {$IFDEF FPC}@{$ENDIF}MyCreate);
  Method('IsEmpty', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsEmpty);
  Method('Clear', {$IFDEF FPC}@{$ENDIF}MyClear);
  Method('Has', my_bool, ['Key'], [my_string], {$IFDEF FPC}@{$ENDIF}MyHas);
  Method('Remove', ['Key'], [my_string], {$IFDEF FPC}@{$ENDIF}MyRemove);
  Method('Keys', my_list, {$IFDEF FPC}@{$ENDIF}MyKeys);
  Method('Values', my_list, {$IFDEF FPC}@{$ENDIF}MyValues);
  Define('', my_variant, 'Key', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGet,
         {$IFDEF FPC}@{$ENDIF}MySet);
  inherited;
end;

function TLiType_hash._AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLiHash(Obj).AsString else
    Result := '';
end;

function TLiType_hash._Clear(Obj: pointer): boolean;
begin
  Result := (Obj <> nil);
  if Result then
    TLiHash(Obj).Clear;
end;

function TLiType_hash._DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiHash(Obj).DecRefcount else
    Result := 0;
end;

procedure TLiType_hash._GcMark(Obj: pointer);
begin
  if Obj <> nil then TLiHash(Obj).MarkForSurvive;
end;

function TLiType_hash._IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiHash(Obj).IncRefcount else
    Result := 0;
end;

{ TLiEnumItem }

procedure TLiEnumItem.SetValue(Value: TLiValue);
begin
  Value.SetObject(FParent, Self);
end;

{ TLiEnumType }

procedure TLiEnumType.Add(const ItemNames: array of string);
var
  I: integer;
begin
  if Length(FItems) = 0 then
  begin
    SetLength(FItems, Length(ItemNames));
    for I := 0 to Length(FItems) - 1 do
    begin
      FItems[I] := TLiEnumItem.Create;
      FItems[I].FParent := Self;
      FItems[I].FName := ItemNames[I];
      FItems[I].FValue := I;
      FModule.FConsts.Add(ItemNames[I]).SetTOA(Self, FItems[I]);
    end;
  end;
end;

constructor TLiEnumType.Create(const AName: string; AModule: TLiModule; AParent: TLiType);
begin
  inherited;
  FStyle := tsEnum;
  if FModule <> nil then
    AddCompare(Self, {$IFDEF FPC}@{$ENDIF}compare_enum_enum);
end;

destructor TLiEnumType.Destroy;
var
  I: integer;
begin
  for I := GetCount - 1 downto 0 do
    FreeAndNil(FItems[I]);
  SetLength(FItems, 0);
  inherited;
end;

function TLiEnumType.Find(ItemValue: integer): TLiEnumItem;
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

function TLiEnumType.Find(const ItemName: string): TLiEnumItem;
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

function TLiEnumType.GetCount: integer;
begin
  Result := Length(FItems);
end;

function TLiEnumType.GetDefValue: TLiEnumItem;
begin
  Result := FItems[0];
end;

function TLiEnumType.GetItem(Index: integer): TLiEnumItem;
begin
  Result := FItems[Index];
end;

function TLiEnumType.ItemByName(const ItemName: string): TLiEnumItem;
begin
  Result := Find(ItemName);
  if Result = nil then
    Throw('%s value(name=''%s'') not exists', [Name, ItemName]);
end;

function TLiEnumType.ItemByValue(ItemValue: integer): TLiEnumItem;
begin
  Result := Find(ItemValue);
  if Result = nil then
    Throw('%s value(ordinal=%d) not exists', [Name, ItemValue]);
end;

procedure TLiEnumType.SetValue(Value: TLiValue; const ItemName: string);
begin
  Value.SetObject(Self, ItemByName(ItemName));
end;

function TLiEnumType.NewEnumSetType(const AName: string): TLiEnumSetType;
begin
  Result := TLiEnumSetType.Create(AName, FModule, nil);
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

procedure TLiEnumType.SetValue(Value: TLiValue; ItemValue: integer);
begin
  Value.SetObject(Self, ItemByValue(ItemValue));
end;

procedure TLiEnumType.SetValue(Value: TLiValue; Item: TLiEnumItem);
begin
  if Item = nil then
    Value.SetObject(Self, DefValue) else
    Value.SetObject(Self, Item);
end;

function TLiEnumType._AsInteger(Obj: pointer): int64;
begin
  if Obj <> nil then
    Result := TLiEnumItem(Obj).FValue else
    Result := DefValue.FValue;
end;

function TLiEnumType._AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLiEnumItem(Obj).FName else
    Result := DefValue.FName;
end;

procedure TLiEnumType._Convert(Value: TLiValue);
begin
  if Value.FType <> Self then
    if Value.FType = my_int then
      SetValue(Value, Value.FValue.VInteger) else
      ErrorConvert(Value.FType, Self);
end;

procedure TLiEnumType._SetDefault(Value: TLiValue);
begin
  inherited;
  Value.FValue.VObject := DefValue;
end;

function TLiEnumType._DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then Result := 1 else Result := 0;
end;

function TLiEnumType._IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then Result := 1 else Result := 0;
end;

{ TLiEnumSet }

function TLiEnumSet.GetSource: TLiEnumType;
begin
  Result := FParent.FSource;
end;

function TLiEnumSet.GetCount: integer;
begin
  Result := Length(FSets);
end;

function TLiEnumSet.Get(Index: integer): boolean;
begin
  Result := FSets[Index];
end;

procedure TLiEnumSet.Put(Index: integer; Value: boolean);
begin
  FSets[Index] := Value;
end;

destructor TLiEnumSet.Destroy;
begin
  SetLength(FSets, 0);
  inherited;
end;

procedure TLiEnumSet.SetValue(Value: TLiValue);
begin
  Value.SetObject(FParent, Self);
end;

function TLiEnumSet.AsBoolean: boolean;
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

function TLiEnumSet.AsString: string;
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

function TLiEnumSet.Equal(S: TLiEnumSet): boolean;
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

function TLiEnumSet.Add(S: TLiEnumSet): TLiEnumSet;
var
  I: integer;
begin
  Result := FParent.NewEnumSet;
  for I := 0 to Length(FSets) - 1 do
    Result.FSets[I] := FSets[I] or S.FSets[I];
end;

function TLiEnumSet.Dec(S: TLiEnumSet): TLiEnumSet;
var
  I: integer;
begin
  Result := FParent.NewEnumSet;
  for I := 0 to Length(FSets) - 1 do
    Result.FSets[I] := FSets[I] and not S.FSets[I];
end;

function TLiEnumSet.Mul(S: TLiEnumSet): TLiEnumSet;
var
  I: integer;
begin
  Result := FParent.NewEnumSet;
  for I := 0 to Length(FSets) - 1 do
    Result.FSets[I] := FSets[I] and S.FSets[I];
end;

function TLiEnumSet.NotAll: TLiEnumSet;
var
  I: integer;
begin
  Result := FParent.NewEnumSet;
  for I := 0 to Length(FSets) - 1 do
    Result.FSets[I] := not FSets[I];
end;

{ TLiEnumSetType }

function TLiEnumSetType.GetDefValue: TLiEnumSet;
begin
  if FDefValue = nil then
  begin
    FDefValue := TLiEnumSet.Create;
    FDefValue.IncRefcount;
    FDefValue.FParent := Self;
  end;
  Result := FDefValue;
end;

function TLiEnumSetType._IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiEnumSet(Obj).IncRefcount else
    Result := 0;
end;

function TLiEnumSetType._DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiEnumSet(Obj).DecRefcount else
    Result := 0;
end;

function TLiEnumSetType._AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLiEnumSet(Obj).AsString else
    Result := '[]';
end;

function TLiEnumSetType._AsBoolean(Obj: pointer): boolean;
begin
  Result := (Obj <> nil) and TLiEnumSet(Obj).Asboolean;
end;

procedure TLiEnumSetType._Convert(Value: TLiValue);
var
  S: TLiEnumSet;
  L: TLiList;
  I: integer;
  V: TLiValue;
begin
  if Value.FType <> Self then
    if Value.FType = my_list then
    begin
      S := NewEnumSet;
      try
        L := Value.AsList;
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

procedure TLiEnumSetType._SetDefault(Value: TLiValue);
begin
  DefValue.SetValue(Value);
end;

destructor TLiEnumSetType.Destroy;
begin
  FreeAndNil(FDefValue);
  inherited;
end;

procedure TLiEnumSetType.SetValue(Value: TLiValue; ASet: TLiEnumSet);
begin
  if ASet = nil then
    Value.SetObject(Self, DefValue) else
    Value.SetObject(Self, ASet);
end;

function TLiEnumSetType.NewEnumSet: TLiEnumSet;
var
  I: integer;
begin
  Result := TLiEnumSet.Create;
  Result.FParent := Self;
  SetLength(Result.FSets, FSource.Count);
  for I := 0 to FSource.Count - 1 do
    Result.FSets[I] := false;
end;

{ TLiValue }

function TLiValue.IncRefcount: integer;
begin
  if FType.IsBasicValue then Result := 1 else
  if FValue.VObject = nil then Result := 0 else
    Result := FType._IncRefcount(FValue.VObject);
end;

function TLiValue.IsBasicValue: boolean;
begin
  Result := FType.IsBasicValue;
end;

function TLiValue.IsDefv: boolean;
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
      ((FType = my_type) and (TLiType(FValue.VObject) = my_nil));
  end;
end;

function TLiValue.IsFalse: boolean;
begin
  Result := (FType = my_bool) and not FValue.VBoolean;
end;

function TLiValue.IsNil: boolean;
begin
  Result := FType.IsNil;
end;

function TLiValue.IsObject: boolean;
begin
  Result := FType.IsObject;
end;

function TLiValue.IsBoolTrue: boolean;
begin
  Result := (FType = my_bool) and FValue.VBoolean;
end;

procedure TLiValue.MarkForSurvive;
begin
  FType._GcMark(FValue.VObject);
end;

function TLiValue.NewList: TLiList;
begin
  SetNil;
  Result := TLiList.Create;
  Result.IncRefcount;
  FType := my_list;
  FValue.VObject := Result;
end;

function TLiValue.Operate(OP: TLiOperator; Value: TLiValue): boolean;
var
  R: PLiOperate;
begin
  R := FType.FindOperate(OP, Value.FType);
  Result := (R <> nil);
  if Result then
    R^.o_operate(Self, Value);
end;

function TLiValue.GetFileName: string;
begin
  if FType = my_string then
    Result := SetPD(Trim(GetAsString)) else
    Result := '';
end;

function TLiValue.GetFunc: TLiFunc;
begin
  if FType = my_func then
    Result := TLiFunc(FValue.VObject) else
    Result := nil;
end;

function TLiValue.GetHashed: TLiHash;
begin
  if FType = my_hash then
    Result := TLiHash(FValue.VObject) else
    Result := nil;
end;

function TLiValue.GetModule: TLiModule;
begin
  if FType = my_module then
    Result := TLiModule(FValue.VObject) else
    Result := nil;
end;

procedure TLiValue.SetValue(Value: TLiValue);
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

procedure TLiValue.SetParentType(VT: TLiType);
begin
  if FType.IsTypeOf(VT) then
    FType := VT else
    ErrorConvert(FType, VT);
end;

function TLiValue.Compare(Value: TLiValue): TLiCompare;
var
  R: PLiCompare;
begin
  R := FType.FindCompare(Value.FType);
  if R <> nil then
    Result := R^.c_compare(Self, Value) else
    Result := crDiff;
end;

function TLiValue.Compare(Value: TLiValue; Wanted: TLiCompares): boolean;
begin
  Result := (Compare(Value) in Wanted);
end;

procedure TLiValue.Convert(AType: TLiType; Cntx: TLiContext);
begin
  AType._Convert(Self);
end;

constructor TLiValue.Create;
begin
  FType := my_nil;
end;

destructor TLiValue.Destroy;
begin
  if Self <> nil then
  begin
    SetNil;
    inherited;
  end;
end;

function TLiValue.GetAsBoolean: boolean;
begin
  Result := FType._AsBoolean(GetOA);
end;

function TLiValue.GetList: TLiList;
begin
  if FType = my_list then
    Result := TLiList(FValue.VObject) else
    Result := nil;
end;

function TLiValue.GetAsList: TLiList;
begin
  if FType = my_list then Result := TLiList(FValue.VObject) else
  begin
    Result := nil;
    ErrorConvert(FType, my_list);
  end;
end;

function TLiValue.GetAsChar: char;
begin
  Result := FType._AsChar(GetOA);
end;

function TLiValue.GetAsModule: TLiModule;
begin
  if FType = my_module then Result := TLiModule(FValue.VObject) else
  begin
    Result := FType.FModule;
    if FValue.VObject <> nil then
      if FType = my_func then
        Result := TLiFunc(FValue.VObject).FModule else
      if FType = my_type then
        Result := TLiType(FValue.VObject).FModule;
  end;
end;

function TLiValue.GetAsCurrency: currency;
begin
  Result := FType._AsCurrency(GetOA);
end;

function TLiValue.GetOA(Wanted: TLiType): pointer;
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

function TLiValue.GetAsFloat: double;
begin
  Result := FType._AsFloat(GetOA);
end;

function TLiValue.GetAsFunc: TLiFunc;
begin
  if FType = my_func then Result := TLiFunc(FValue.VObject) else
  begin
    Result := nil;
    ErrorConvert(FType, my_func);
  end;
end;

function TLiValue.GetAsHash: TLiHash;
begin
  if FType = my_hash then Result := TLiHash(FValue.VObject) else
  begin
    Result := nil;
    ErrorConvert(FType, my_hash);
  end;
end;

function TLiValue.GetAsInteger: int64;
begin
  Result := FType._AsInteger(GetOA);
end;

function TLiValue.GetAsEnumSet: TLiEnumSet;
begin
  if FType.FStyle = tsEnumSet then
  begin
    Result := TLiEnumSet(FValue.VObject);
    if Result = nil then
      Result := TLiEnumSetType(FType).DefValue;
  end
  else Throw('%s is not enum set type', [FType.FullName]);
end;

function TLiValue.GetAsEnum: TLiEnumItem;
begin
  if FType.IsEnum then
  begin
    Result := TLiEnumItem(FValue.VObject);
    if Result = nil then
      Result := TLiEnumType(FType).DefValue;
  end
  else Throw('%s is not enum type', [FType.FullName]);
end;

procedure TLiValue.SetAsEnumSet(Value: TLiEnumSet);
begin
  if Value = nil then
    Throw('invalid enum set: nil') else
    SetTOA(Value.FParent, Value);
end;

function TLiValue.GetSelf(var Aobj): boolean;
begin
  Result := FType.IsObject;
  if Result then
  begin
    FType._Validate(FValue.VObject);
    pointer(Aobj) := FValue.VObject;
  end;
end;

function TLiValue.GetString: TLiString;
begin
  if FType = my_string then
    Result := TLiString(FValue.VObject) else
    Result := nil;
end;

function TLiValue.GetAsString: string;
begin
  Result := FType._AsString(GetOA);
end;

procedure TLiValue.SetAsEnum(Value: TLiEnumItem);
begin
  if Value = nil then
    Throw('invalid enum item: nil') else
    Value.SetValue(Self);
end;

function TLiValue.GetAsTime: TDateTime;
begin
  Result := FType._AsTime(GetOA);
end;

function TLiValue.GetTOA(var OA: pointer): TLiType;
begin
  Result := FType;
  OA := GetOA;
end;

function TLiValue.GetAsType: TLiType;
begin
  if FType = my_type then
  begin
    Result := TLiType(FValue.VObject);
    if Result = nil then
      Result := my_nil;
  end
  else Result := FType;
end;

function TLiValue.DecRefcount: integer;
begin
  if FType.IsBasicValue then Result := 1 else
  if FValue.VObject = nil then Result := 0 else
  begin
    Result := FType._DecRefcount(FValue.VObject);
    FValue.VObject := nil;
  end;
end;

procedure TLiValue.SetAsList(Value: TLiList);
begin
  my_list._IncRefcount(Value);
  my_list._SetDefault(Self);
  FValue.VObject := Value;
end;

procedure TLiValue.SetAsBoolean(Value: boolean);
begin
  FType._DecRefcount(FValue.VObject);
  FType := my_bool;
  FValue.VBoolean := Value;
end;

procedure TLiValue.SetAsChar(Value: char);
begin
  FType._DecRefcount(FValue.VObject);
  FType := my_char;
  FValue.VChar[0] := Value;
  FValue.VChar[1] := #0;
end;

procedure TLiValue.SetAsModule(Value: TLiModule);
begin
  my_module._IncRefcount(Value);
  my_module._SetDefault(Self);
  FValue.VObject := Value;
end;

procedure TLiValue.SetAsCurrency(Value: currency);
begin
  FType._DecRefcount(FValue.VObject);
  FType := my_curr;
  FValue.VCurrency := Value;
end;

procedure TLiValue.SetAsInteger(Value: int64);
begin
  FType._DecRefcount(FValue.VObject);
  FType := my_int;
  FValue.VInteger := Value;
end;

procedure TLiValue.SetAsString(const Value: string);
begin
  my_string._SetDefault(Self);
  FValue.VObject := TLiString.Create(Value);
  TLiString(FValue.VObject).IncRefcount;
end;

procedure TLiValue.SetAsTime(Value: TDateTime);
begin
  FType._DecRefcount(FValue.VObject);
  FType := my_time;
  FValue.VTime := Value;
end;

procedure TLiValue.SetAsType(Value: TLiType);
begin
  if FType <> my_type then
    my_type._SetDefault(Self);
  if Value = nil then
    FValue.VObject := my_nil else
    FValue.VObject := Value;
end;

function TLiValue.Same(Value: TLiValue): boolean;
begin
  Result := (FType = Value.FType) and (Compare(Value) = crEqual);
end;

procedure TLiValue.SetDefault(AType: TLiType);
begin
  AType._SetDefault(Self);
end;

procedure TLiValue.SetFind(Finded: PLiFind);
begin
  case Finded^.f_find of
    fiFunc  : SetAsFunc(Finded^.VFunc);
    fiType  : SetAsType(Finded^.VType);
    fiModule: SetAsModule(Finded^.VModule);
    fiValue : SetValue(Finded^.VValue);
    else SetNil;
  end;
end;

procedure TLiValue.SetAsFloat(Value: double);
begin
  FType._DecRefcount(FValue.VObject);
  FType := my_float;
  FValue.VFloat := Value;
end;

procedure TLiValue.SetAsFunc(Value: TLiFunc);
begin
  my_func._IncRefcount(Value);
  my_func._SetDefault(Self);
  FValue.VObject := Value;
end;

procedure TLiValue.SetAsHash(Value: TLiHash);
begin
  my_hash._IncRefcount(Value);
  my_hash._SetDefault(Self);
  FValue.VObject := Value;
end;

procedure TLiValue.SetNil;
begin
  if FValue.VObject <> nil then
  begin
    FType._DecRefcount(FValue.VObject);
    FValue.VObject := nil;
  end;
  FType := my_nil;
end;

procedure TLiValue.SetTOA(T: TLiType; OA: pointer);
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

procedure TLiValue.SetObject(AType: TLiType; Aobj: pointer);
begin
  AType._IncRefcount(Aobj);
  FType._DecRefcount(FValue.VObject);
  FType := AType;
  FValue.VObject := Aobj;
end;

{ TLiParam }

procedure TLiParam.Error(const Msg: string);
var
  F: TLiFunc;
  M: TLiModule;
  S: string;
begin
  if not FContext.FExcepted then
  begin
    F := FFunc;
    if Msg = '' then
      S := F.Name + '() - ' + ExceptionStr else
      S := F.Name + '() - ' + Msg;
    if Assigned(F.FProc) then
    begin
      M := FContext.CodeModule;
      FContext.FError.Runtime(S, M.Name, M.FileName, FToken.FRow, FToken.FCol);
    end
    else FToken.Error(Self, S);
  end;
end;

procedure TLiParam.BeginExec(var Mark: integer; var Tmpv: TLiValue);
begin
  Mark := FParams.Count;
  Tmpv := FParams.Add;
end;

procedure TLiParam.BeginExec(var Mark: integer);
begin
  Mark := FParams.Count;
end;

procedure TLiParam.EndExec(Mark: integer);
var
  I: integer;
begin
  for I := FParams.Count - 1 downto Mark do
    FParams.Delete(I);
end;

procedure TLiParam.Error(const Msg: string; const Args: array of const);
begin
  Error(Format(Msg, Args));
end;

procedure TLiParam.ErrorChangeFunc(AFunc: TLiFunc);
begin
  Error('can not change function: ' + AFunc.FullName);
end;

procedure TLiParam.ErrorOper(OP: TLiOperator; L, R: TLiType);
begin
  if L = nil then
    Error('unknown operation: %s %s', [OperatorStr(OP), R.FName]) else
  if R = nil then
    Error('unknown operation: %s %s', [OperatorStr(OP), L.FName]) else
    Error('unknown operation: %s %s %s', [L.FName, OperatorStr(OP), R.FName]);
end;

function TLiParam.GetChangeAbleFunc(var F: TLiFunc): boolean;
begin
  F := GetItem(0).AsFunc;
  my_func._Validate(F);
  Result := (F <> nil) and TestChangeFunc(F);
end;

function TLiParam.GetCount: integer;
begin
  if FParams <> nil then
    Result := FParams.Count else
    Result := 0;
end;

function TLiParam.GetItem(Index: integer): TLiValue;
begin
  Result := FParams[Index];
end;

function TLiParam.GetSelf(var Aobj): boolean;
begin
  Result := GetItem(0).GetSelf(Aobj);
end;

function TLiParam.GetValue(const Name: string): TLiValue;
var
  I: integer;
begin
  I := FFunc.FParams.IndexOf(Name);
  if I >= 0 then
    Result := GetItem(I) else
    Result := nil;
end;

function TLiParam.GetVarbValue(Index: integer; var VT: TLiType): TLiValue;
var
  T: TLiToken;
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
      T := TLiToken(FToken.FParams[Index]);
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

procedure TLiParam.SetResult(Value: TLiValue);
begin
  FResult.SetValue(Value);
  FFunc.FResultType._Convert(FResult);
end;

function TLiParam.TestChangeFunc(AFunc: TLiFunc): boolean;
begin
  Result := AFunc.ChangeAble;
  if not Result then
    ErrorChangeFunc(AFunc);
end;

{ TLiToken }

procedure TLiToken.AddParam(AParam: TLiToken);
begin
  if FParams = nil then
    FParams := TList.Create;
  FParams.Add(AParam);
end;

procedure TLiToken.Assign(Source: TLiToken);
begin
  FSym := Source.FSym;
  FRow := Source.FRow;
  FCol := Source.FCol;
  FName := Source.FName;
  FValue := Source.FValue;
end;

procedure TLiToken.Clear;
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

procedure TLiToken.ClearParams;
var
  I: integer;
  P: TLiToken;
begin
  for I := GetParamCount - 1 downto 0 do
  begin
    P := GetParam(I);
    FParams.Delete(I);
    P.Free;
  end;
  FreeAndNil(FParams);
end;

constructor TLiToken.Create(T: TLiToken);
begin
  if T <> nil then Read(T);
end;

constructor TLiToken.CreateWithLeft(LeftBranch: TLiToken; T: TLiToken);
begin
  Create(T);
  FLeft := LeftBranch;
end;

function TLiToken.Decompile(Level: integer): string;

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

  function decompile_list: string;
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
    F: TLiFunc;
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
    syList  : Result := decompile_list;
    syHash  : Result := decompile_hash;
    syAt    : Result := decompile_at;
    else
    if FSym in [syNil, syTrue, syFalse, syVarArg] then
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

destructor TLiToken.Destroy;
begin
  if Self <> nil then
  begin
    Clear;
    inherited;
  end;
end;

procedure TLiToken.Error(Param: TLiParam; const Msg: string; const Args: array of const);
begin
  Error(Param, Format(Msg, Args));
end;

procedure TLiToken.FailGet(Param: TLiParam; const ID: string);
begin
  Error(Param, 'failed getting: ' + ID);
end;

procedure TLiToken.ExecCall(Param: TLiParam; Outv: TLiValue);
var
  tmpv: TLiValue;
  mark: integer;

  procedure exec_call;
  var
    T: TLiType;
  begin
    if tmpv.FType = my_func then
    begin
      if not TryFunc(TLiFunc(tmpv.FValue.VObject), Param, Outv) then
        Error(Param, 'got no function to call');
    end
    else
    if (tmpv.FType = my_type) and (GetParamCount = 1) then
    begin
      T := TLiType(tmpv.FValue.VObject);
      if T = nil then T := my_nil;
      if GetParam(0).Execute(Param, Outv) then
        T._Convert(Outv);
    end
    else Error(Param, 'invalid calling: %s()', [tmpv.FType.FullName]);
  end;

  procedure exec_ID;
  begin
    if (Param.FFunc.FindSave(FName, Param, tmpv) <> fiNone)
      or Param.FContext.Resolve(FName, tmpv) then
        exec_call else FailGet(Param, FName);
  end;

  procedure exec_method;
  var
    M: TLiModule;
    T: TLiType;
  begin
    if not TryMethod(tmpv, FName, Param, Outv, nil) then
    if (tmpv.FType = my_module) and (tmpv.FValue.VObject <> nil) then
    begin
      M := TLiModule(tmpv.FValue.VObject);
      if M.FindSave(FName, tmpv) then
        exec_call else
        FailGet(Param, M.FName, FName);
    end
    else
    if (tmpv.FType = my_type) and (tmpv.FValue.VObject <> nil) then
    begin
      T := TLiType(tmpv.FValue.VObject);
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

function TLiToken.TryFunc(Func: TLiFunc; Param: TLiParam; Outv: TLiValue): boolean;
var
  A: TLiList;
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

function TLiToken.ExecFunc(Func: TLiFunc; Param: TLiParam; Outv: TLiValue; Args: TLiList): boolean;

  function prepare_arguments: integer;
  var
    I, L, P: integer;
    A: TLiList;
  begin
    L := Args.Count;
    P := Func.FParams.ParamCount;
    if L < P then Result := L else
    begin
      Result := P;
      if Func.FHasVarArg then
      begin
        A := TLiList.Create;
        A.Add(Args[P - 1]);
        Args[P - 1].SetAsList(A);
        for I := P to L - 1 do A.Add(Args[I]);
      end;
      Args.SetCount(P);
    end;
    Args.PrepareFor(Func);
  end;

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
  curr: TLiParam;
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
          curr := TLiParam.Create;
          try
            curr.FPrev := Param.FContext.FCurrent;
            curr.FContext := Param.FContext;
            curr.FFunc := Func;
            curr.FPrmc := prepare_arguments;
            curr.FParams := Args;
            curr.FResult := Outv;
            curr.FToken := Self;
            Param.FContext.FCurrent := curr;
            Func.FResultType._SetDefault(Outv);
            if Assigned(Func.FProc) then
              Func.FProc(curr) else
              Func.FSTMTs.Execute(curr);
            Func.FResultType._Convert(Outv);
          finally
            Param.FContext.FCurrent := curr.FPrev;
            if Param.FContext.FState = csExit then
              if not Assigned(Func.FProc) then
                Param.FContext.FState := csRunning;
            curr.Free;
          end;
          Result := Param.FContext.StatusOK;
        finally
          Param.EndExec(mark);
        end;
      end;
    finally
      Func.DecRefcount;
    end;
  end;
end;

procedure TLiToken.ExecGet(Param: TLiParam; Outv: TLiValue);
var
  tmpv: TLiValue;
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

function TLiToken.GetProp(Param: TLiParam; Host, Outv: TLiValue): boolean;
var
  F: TLiFunc;
  A: TLiList;
begin
  Result := GetAt(Param, Host, Outv);
  if Result then
  begin
    F := Outv.GetFunc;
    if F <> nil then
    begin
      A := TLiList.Create;
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

procedure TLiToken.ExecID(Param: TLiParam; Outv: TLiValue);
var
  R: RLiFind;
  I: integer;
  F: TLiFunc;
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
  if not Param.FContext.Resolve(FName, Outv) then
    FailGet(Param, FName);
end;

procedure TLiToken.ExecSet(Param: TLiParam);
var
  tmpv: TLiValue;
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
    data: TLiValue;
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

function TLiToken.Execute(Param: TLiParam; Outv: TLiValue): boolean;
var
  tmpv: TLiValue;
  mark: integer;

  procedure exec_list;
  var
    list: TLiList;
    I, N: integer;
  begin
    list := TLiList.Create;
    Outv.SetAsList(list);
    N := GetParamCount;
    I := 0;
    while (I < N) and GetParam(I).Execute(Param, tmpv) do
    begin
      list.Add(tmpv);
      Inc(I);
    end;
  end;

  procedure exec_hash;
  var
    hash: TLiHash;
    I, N: integer;
    key: string;
  begin
    hash := TLiHash.Create;
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
      if not Param.FContext.Resolve(FName, Outv) then
        FailGet(Param, FName);
  end;

  function GetLRV(L, R: TLiValue): boolean;
  begin
    Result := FLeft.Execute(Param, L) and FRight.Execute(Param, R);
  end;

  procedure exec_oper(OP: TLiOperator);
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

  procedure exec_comp(Wanted: TLiCompares);
  begin
    Outv.SetAsBoolean(GetLRV(Outv, tmpv) and Outv.Compare(tmpv, Wanted));
  end;

  procedure exec_same;
  begin
    Outv.SetAsBoolean(GetLRV(Outv, tmpv) and Outv.Same(tmpv));
  end;

  procedure exec_dot3;
  var
    I: integer;
  begin
    if Param.FFunc.FHasVarArg then
    begin
      I := Param.FFunc.FParams.ParamCount - 1;
      Outv.SetValue(Param.FParams[I]);
    end
    else Error(Param, '%s has no parameter: ... ', [Param.FFunc.FullName]);
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
          syList  : exec_list;
          syHash  : exec_hash;
          syAt    : exec_at;
          syVarArg: exec_dot3;
        end;
      finally
        Param.EndExec(mark);
      end;
    end;
  except
    Error(Param, '');
  end;
  Result := Param.FContext.StatusOK;
end;

procedure TLiToken.FailGet(Param: TLiParam; const Host, Prop: string);
begin
  FailGet(Param, Host + '.' + Prop);
end;

function TLiToken.GetAt(Param: TLiParam; Host, Outv: TLiValue): boolean;
var
  F: TLiFunc;
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
      Result := TLiModule(Host.FValue.VObject).FindSave(FName, Outv);
      if not Result then
        FailGet(Param, TLiModule(Host.FValue.VObject).FName, FName);
    end
    else
    if Host.FType = my_type then
    begin
      F := TLiType(Host.FValue.VObject).FindMethod(FName);
      Result := (F <> nil);
      if Result then
        Outv.SetAsFunc(F) else
        FailGet(Param, TLiType(Host.FValue.VObject).FName, FName);
    end;
  if not Result then
    if Param.FContext.StatusOK then
      FailGet(Param, Host.FType.FName, FName);
end;

function TLiToken.GetParam(Index: integer): TLiToken;
begin
  Result := TLiToken(FParams[Index]);
end;

function TLiToken.GetParamCount: integer;
begin
  if FParams <> nil then
    Result := FParams.Count else
    Result := 0;
end;

procedure TLiToken.Error(Param: TLiParam; const Msg: string);
var
  M: TLiModule;
  S: string;
begin
  if not Param.FContext.FExcepted then
  begin
    if Msg = '' then
      S := Param.FFunc.FName + '() - ' + ExceptionStr else
      S := Param.FFunc.FName + '() - ' + Msg;
    M := Param.FFunc.FModule;
    Param.FContext.FError.Runtime(S, M.Name, M.FFileName, FRow, FCol);
  end;
end;

procedure TLiToken.Read(Source: TLiToken);
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

function TLiToken.SetupParamList(Param: TLiParam; Host: TLiValue; Func: TLiFunc): TLiList;
var
  I, N: integer;
begin
  Result := TLiList.Create;
  if Host <> nil then
    if func.IsMethod and not func.IsConstructor then
      Result.Add(Host);
  if Func.FHasVarArg then
    N := GetParamCount else
    N := Min(GetParamCount, Func.FParams.ParamCount - Result.Count);
  for I := 0 to N - 1 do
    if not GetParam(I).Execute(Param, Result.Add) then
    begin
      FreeAndNil(Result);
      Exit;
    end;
end;

function TLiToken.TryMethod(Host: TLiValue; const Method: string;
  Param: TLiParam; Outv: TLiValue; LastParam: TLiToken): boolean;
var
  func: TLiFunc;
  args: TLiList;
  tmpv: TLiValue;
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

{ TLiTokenList }

function TLiTokenList.Add(Pos: TLiToken): TLiToken;
begin
  Result := TLiToken.Create;
  if Pos <> nil then
  begin
    Result.FRow := Pos.FRow;
    Result.FCol := Pos.FCol;
  end;
  FItems.Add(Result);
end;

function TLiTokenList.AddToken(Sym: TLiSymbol): TLiToken;
begin
  Result := AddToken(Sym, nil);
end;

procedure TLiTokenList.Clear;
var
  X: integer;
  T: TLiToken;
begin
  for X := GetCount - 1 downto 0 do
  begin
    T := TLiToken(FItems[X]);
    FItems.Delete(X);
    T.Free;
  end;
end;

procedure TLiTokenList.ClearKeepLast;
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

constructor TLiTokenList.Create;
begin
  FItems := TList.Create;
end;

procedure TLiTokenList.DeleteLast(N: integer);
var
  X: integer;
  T: TLiToken;
begin
  N := GetCount - N;
  for X := GetCount - 1 downto N do
  begin
    T := TLiToken(FItems[X]);
    FItems.Delete(X);
    T.Free;
  end;
end;

destructor TLiTokenList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TLiTokenList.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TLiTokenList.GetItem(Index: integer): TLiToken;
begin
  Result := TLiToken(FItems[Index]);
end;

function TLiTokenList.GetLast: TLiToken;
begin
  Result := TLiToken(FItems.Last);
end;

procedure TLiTokenList.Reverse;
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

function TLiTokenList.AddToken(Token: TLiToken): TLiToken;
begin
  Result := Add;
  Result.Assign(Token);
end;

function TLiTokenList.AddToken(Sym: TLiSymbol; Pos: TLiToken): TLiToken;
begin
  Result := Add(Pos);
  Result.FSym := Sym;
end;

{ TLiTokenizer }

constructor TLiTokenizer.Create(const Script: string);
var
  T: TLiToken;
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
  FTokens := TLiTokenList.Create;
  repeat
    T := FTokens.Add(nil);
    GetToken(T);
  until T.FSym in [syEOF, syError];
  if T.FSym = syError then
    FTokens.ClearKeepLast else
    FTokens.Reverse;
  FIndex := FTokens.Count - 1;
end;

destructor TLiTokenizer.Destroy;
begin
  FreeAndNil(FTokens);
  inherited;
end;

function TLiTokenizer.GetChar: boolean;
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

function TLiTokenizer.GetCurrent: TLiToken;
begin
  if FCurrent = nil then GetNext;
  Result := FCurrent;
end;

function TLiTokenizer.PackToCurrent: boolean;
begin
  Result := (FCurrent <> nil);
  if Result then
    FTokens.DeleteLast(FTokens.Count - (FIndex + 1));
end;

function TLiTokenizer.ParseChar(var C: char): boolean;
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

function TLiTokenizer.ParseHex(var I: int64): boolean;
begin
  Result := (FChar = '$') and GetChar and CharInSet(FChar, CS_HEX);
  if Result then
  begin
    I := HexValue(FChar);
    while GetChar and CharInSet(FChar, CS_HEX) do
      I := (I * 16) + HexValue(FChar);
  end;
end;

function TLiTokenizer.ParseString(var S: string): boolean;
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

function TLiTokenizer.GetNext: TLiToken;
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

function TLiTokenizer.GetToken(token: TLiToken): boolean;

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
    I: TLiSymbol;
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

  procedure parse_operator(DefSymbol: TLiSymbol;
    const next: array of char;
    const syms: array of TLiSymbol);
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

  procedure parse_set(Symbol: TLiSymbol);
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

    if (token.FSym = syRange) and (FChar = '.') then
    begin
      token.FSym := syVarArg;
      GetChar;
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

function TLiTokenizer.GotoChar(Chars: TSysCharSet): boolean;
begin
  repeat Result := CharInSet(FChar, Chars) until Result or not GetChar;
end;

function TLiTokenizer.NextChar: char;
begin
  if FPosition < FSize then
    Result := FCode[FPosition + 1] else
    Result := #0;
end;

function TLiTokenizer.PrevChar: char;
begin
  if FPosition > 1 then
    Result := FCode[FPosition - 1] else
    Result := #0;
end;

function TLiTokenizer.PeekNextSymbol: TLiSymbol;
begin
  if FIndex > 0 then
    Result := FTokens[FIndex - 1].FSym else
    Result := syError;
end;

function TLiTokenizer.NextIsBecome(OnHead: boolean): boolean;
var
  S: TLiSymbol;
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

function TLiTokenizer.PeekThirdSymbol: TLiSymbol;
begin
  if FIndex > 1 then
    Result := FTokens[FIndex - 2].FSym else
    Result := syError;
end;

function TLiTokenizer.SkipSpaces: boolean;

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

{ TLiParser }

constructor TLiParser.Create(AModule: TLiModule);
begin
  FModule := AModule;
  FContext := FModule.FContext;
end;

destructor TLiParser.Destroy;
begin
  FreeAndNil(FTokenizer);
  inherited;
end;

procedure TLiParser.EUnexpected(T: TLiToken);
var
  S: string;
begin
  if T = nil then T := FLast;
  S := T.FName;
  if S = '' then
    S := Symbols[T.FSym].ID;
  ESyntax(T.FRow, T.FCol, 'unexpected symbol: %s', [S]);
end;

procedure TLiParser.ERedeclared(T: TLiToken);
begin
  if T = nil then T := FLast;
  ESyntax(T.FRow, T.FCol, 'object redeclared: %s', [T.FName]);
end;

function TLiParser.UseToken(Token: TLiToken): TLiToken;
var
  I: integer;
  T: TLiToken;
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

procedure TLiParser.ETypeNotFound(T: TLiToken);
begin
  if T = nil then T := FLast;
  ESyntax(FTokenizer.FRow, FTokenizer.FCol, 'type not found: %s', [T.FName]);
end;

procedure TLiParser.ESyntax(ERow, ECol: integer;
  const EMsg: string; const EArgs: array of const);
begin
  FContext.FError.Syntax(Format(EMsg, EArgs), FModule.Name,
    FModule.FileName, ERow, ECol);
end;

procedure TLiParser.ParseUses;
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

procedure TLiParser.ParseConst;
var
  L: TLiToken;
  S: TLiSTMT_assign;
begin
  FAfter := FLast.FCol;
  SymTestNextID;
  repeat
    if FModule.Find(FLast.FName) then ERedeclared;
    L := FLast;
    SymTestNext([syEQ]);
    FFunc := TLiFunc.Create('', FModule, nil);
    FModule.FConsts.Add(L.FName).SetAsFunc(FFunc);
    S := FFunc.GetSTMTs.Add(ssConst) as TLiSTMT_assign;
    S.FVarb := L.FName;
    S.FExpr := ParseExpr(false, [sySemic], true);
    SymGetNext;
  until (FLast.FSym <> syID) or (FLast.FCol <= FAfter);
end;

procedure TLiParser.ParseFunc;
begin
  FAfter := FLast.FCol;
  SymTestNext([syID]);
  if FModule.Find(FLast.FName) then ERedeclared;
  FFunc := TLiFunc.Create(FLast.FName, FModule, nil);

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

function TLiParser.Parse(const Code: string; UsingModule: boolean): TLiFunc;
begin
  Result := nil;
  FAfter := -1;
  FreeAndNil(FTokenizer);
  FTokenizer := TLiTokenizer.Create(Code);
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
      else Result := TLiFunc.Create('', FModule, nil);
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

procedure TLiParser.SymTestLast(Syms: TLiSymbols);
begin
  if (Syms <> []) and not (FLast.FSym in Syms) then EUnexpected;
end;

procedure TLiParser.SymTestLastID;
begin
  SymTestLast([syID]);
  if not IsID(FLast.FName) then
    ESyntax(FLast.FRow, FLast.FCol,
      'invalid identity: %s', [FLast.FName]);
end;

procedure TLiParser.SymTestNext(Syms: TLiSymbols);
begin
  SymGotoNext;
  SymTestLast(Syms);
end;

procedure TLiParser.SymTestNextID;
begin
  SymGotoNext;
  SymTestLastID;
end;

procedure TLiParser.SymGetNext;
begin
  FLast := FTokenizer.GetNext;
  if FLast = nil then
    ESyntax(FTokenizer.FRow, FTokenizer.FCol,
      'symbol expected but reachs end of file', []);
end;

procedure TLiParser.SymGotoNext;
begin
  SymGetNext;
  if FLast.FCol <= FAfter then
    ESyntax(FLast.FRow, FLast.FCol,
      'symbol position(%d) should after %d',
      [FLast.FCol + 1, FAfter + 1]);
end;

procedure TLiParser.ParseBlock(EndSyms: TLiSymbols; SX: TLiSTMTList);
var
  Syms: TLiSymbols;
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

function TLiParser.ParseAndFree(const Code: string; UsingModule: boolean): TLiFunc;
begin
  try
    Result := Parse(Code, UsingModule);
  finally
    Free;
  end;
end;

function TLiParser.ParseExpr(OnHead: boolean; EndSyms: TLiSymbols; DoCheck: boolean): TLiToken;
begin
  if not OnHead then SymGotoNext;
  Result := ParseFact(High(OperLevel));
  if DoCheck then
    SymTestLast(EndSyms);
end;

function TLiParser.ParseFact(Level: integer): TLiToken;
begin
  if Level > 0 then
    Result := ParseFact(Level - 1) else
    Result := ParseTerm;
  while (FLast.FSym in OperLevel[Level]) and (SymPeekNext in ExprHead) do
  begin
    Result := TLiToken.CreateWithLeft(Result, FLast);
    SymGotoNext;
    if Level > 0 then
      Result.FRight := ParseFact(Level - 1) else
      Result.FRight := ParseTerm;
  end;
end;

procedure TLiParser.ParseStatement(OnHead: boolean; SX: TLiSTMTList);
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

procedure TLiParser.ParseIf(SX: TLiSTMTList);
var
  after: integer;
  S: TLiSTMT_if;
begin
{ if CONDITION then ......
  elif CONDITION then ......
  else ...... }
  after := FAfter;
  try
    FAfter := FLast.FCol;
    S := SX.Add(ssIf) as TLiSTMT_if;
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

procedure TLiParser.ParseUsesConstFunc;
begin
  while FLast.FSym in [syFunc, syConst, syUses] do
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

procedure TLiParser.ParseVar(SX: TLiSTMTList);
var
  after: integer;
  S: TLiSTMT_assign;
  V: TLiVarb;
  T: TLiType;
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
        S := SX.Add(ssAssign) as TLiSTMT_assign;
        S.FVarb := FFunc.FParams[FFunc.FParams.Count - 1].FName;
        S.FExpr := ParseExpr(false, [sySemic], true);
      end;

      SymGetNext;
    until (FLast.FSym <> syID) or (FLast.FCol <= FAfter);
  finally
    FAfter := after;
  end;
end;

procedure TLiParser.ParseFor(SX: TLiSTMTList);
var
  after: integer;
  S: TLiSTMT_for;
begin
{ for a in range do ....
  for a := low to high do ....
  for a := high downto low do .... }
  after := FAfter;
  try
    FAfter := FLast.FCol;
    S := SX.Add(ssFor) as TLiSTMT_for;
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

procedure TLiParser.ParseWhile(SX: TLiSTMTList);
var
  after: integer;
  S: TLiSTMT;
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

procedure TLiParser.ParseRepeat(SX: TLiSTMTList);
var
  after: integer;
  S: TLiSTMT;
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

procedure TLiParser.ParseCase(SX: TLiSTMTList);
var
  after: integer;
  S: TLiSTMT_case;

  procedure parse_branch;
  var
    A: integer;
    T: TLiSTMT;
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
    S := SX.Add(ssCase) as TLiSTMT_case;
    S.FExpr := ParseExpr(false, [syOf], true);
    SymGotoNext;
    parse_branch;
  finally
    FAfter := after;
  end;
end;

procedure TLiParser.ParseTry(SX: TLiSTMTList);
var
  after: integer;
  S, T: TLiSTMT_try;
begin
{ try ....
  except ....
  finally .... }
  after := FAfter;
  try
    FAfter := FLast.FCol;
    S := SX.Add(ssTry) as TLiSTMT_try;
    ParseBlock([syExcept, syFinally], S.GetItems);
    if FLast.FCol = FAfter then
    begin
      S.FTryFinally := (FLast.FSym = syFinally);
      ParseBlock([], S.GetElseItems);
      while (FLast.FSym in [syExcept, syFinally]) and (FLast.FCol = FAfter) do
      begin
        T := SX.Add(ssTry) as TLiSTMT_try;
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

procedure TLiParser.ParseType(OnHead: boolean; var T: TLiType);
var
  I, M: string;
begin
  if not OnHead then SymTestNext([syID]);
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

procedure TLiParser.ParseRaise(SX: TLiSTMTList);
var
  after: integer;
  S: TLiSTMT;
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

procedure TLiParser.ParsePuts(SX: TLiSTMTList);
var
  after: integer;
  S, T: TLiSTMT;
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

procedure TLiParser.ParseAny(SX: TLiSTMTList);
var
  after: integer;
  V: TLiToken;
  S: TLiSTMT;
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
        TLiSTMT_assign(S).FVarb := FLast.FName;
        FFunc.FParams.AddLocal(FLast.FName, my_variant);
      end
      else S := SX.Add(ssResult);
      V := FLast;
      SymGotoNext;
      if FLast.FSym <> syBecome then // +=, -=, *=, ....
      begin
        S.FExpr := TLiToken.Create(FLast);  // operator
        S.FExpr.FLeft := TLiToken.Create(V);
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

procedure TLiParser.ParseArguments(EndSym: TLiSymbol);
var
  V: TLiVarb;
  I, count: integer;
begin
  count := 0;
  SymTestNext([syID, syVarArg, EndSym]);
  while FLast.FSym <> EndSym do
  begin
    if FLast.FSym = syVarArg then
    begin
      FFunc.FParams.Add('...', my_list);
      FFunc.FHasVarArg := true;
      SymTestNext([EndSym]);
      Break;
    end
    else
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
        SymTestNext([syID, syVarArg]);
    end;
  end;
end;

function TLiParser.ParseTerm: TLiToken;

  procedure parse_list;
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

  procedure parse_call(EndSym: TLiSymbol);
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
  F: TLiFunc;
begin
  Result := nil;

  { not term | - term }
  if FLast.FSym in [syNot, syReduce] then
  begin
    Result := UseToken(FLast);
    if Result.FSym = syReduce then Result.FSym := syNeg;
    SymGotoNext;
    Result.FRight := ParseTerm;
    Exit;
  end;

  { @function | @object.method }
  if FLast.FSym = syAt then
  begin
    SymGotoNext;
    Result := ParseTerm;
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
    if (FLast.FSym = syVarArg) and not FFunc.FHasVarArg then
      EUnexpected(FLast);
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
    Result.FSym := syList;
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
      if FLast.FSym = syComma then parse_list else
      if FLast.FSym = syColon then parse_hash;
    end;
  end
  else
  if FLast.FSym = syVert then
  begin
    Result := UseToken(FLast);
    F := FFunc;
    try
      FFunc := TLiFunc.Create('', FModule, nil);
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
      Result := TLiToken.CreateWithLeft(Result, FLast);
      Result.FSym := syCall;
      Result.FName := '';
      parse_call(syRParen);
    end
    else
    if FLast.FSym = syLArray then // expression[...]
    begin
      Result := TLiToken.CreateWithLeft(Result, FLast);
      Result.FSym := syGet;
      Result.FName := '';
      parse_call(syRArray);
    end
    else // expression.ID
    begin
      SymTestNext([syBegin..syEnd, syID]);
      Result := TLiToken.CreateWithLeft(Result, FLast);
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

function TLiParser.SymPeekNext: TLiSymbol;
begin
  Result := FTokenizer.PeekNextSymbol;
end;

{ TLiSTMT }

constructor TLiSTMT.Create(AParent: TLiSTMTList);
begin
  FStyle := ssNormal;
  FParent := AParent;
  FParent.FItems.Add(Self);
end;

procedure TLiSTMT.Decompile(Level: integer; Lines: TStrings);

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

destructor TLiSTMT.Destroy;
begin
  if FParent <> nil then
    FParent.FItems.Remove(Self);
  FreeAndNil(FExpr);
  FreeAndNil(FItems);
  inherited;
end;

procedure TLiSTMT.ExecNormal(Param: TLiParam);
var
  tmpv: TLiValue;
  mark: integer;
begin
  Param.BeginExec(mark, tmpv);
  try
    FExpr.Execute(Param, tmpv);
  finally
    Param.EndExec(mark);
  end;
end;

procedure TLiSTMT.ExecPuts(Param: TLiParam);
var
  tmpv: TLiValue;
  mark, I: integer;
begin
  Param.BeginExec(mark, tmpv);
  try
    if (FItems <> nil) and (FItems.Count > 0) then
      if FItems[0].FExpr.Execute(Param, tmpv) then
      begin
        Param.FContext.Write(tmpv.AsString);
        for I := 1 to FItems.Count - 1 do
          if FItems[I].FExpr.Execute(Param, tmpv) then
          begin
            Param.FContext.Write(' ');
            Param.FContext.Write(tmpv.AsString);
          end
          else Break;
      end;
  finally
    Param.EndExec(mark);
  end;
end;

procedure TLiSTMT.ExecRaise(Param: TLiParam);
var
  tmpv: TLiValue;
  mark: integer;
begin
  if FExpr <> nil then
  begin
    Param.BeginExec(mark, tmpv);
    try
      FExpr.Error(Param, tmpv.AsString);
    finally
      Param.EndExec(mark);
    end;
  end
  else Param.FContext.FExcepted := true;
end;

procedure TLiSTMT.ExecRepeat(Param: TLiParam);
var
  cntx: TLiContext;
  tmpv: TLiValue;
  mark: integer;
begin
  cntx := Param.FContext;
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

function TLiSTMT.Execute(Param: TLiParam): boolean;
begin
  case FStyle of
    ssNormal: ExecNormal(Param);
    ssPuts  : ExecPuts(Param);
    ssWhile : ExecWhile(Param);
    ssRepeat: ExecRepeat(Param);
    ssRaise : ExecRaise(Param);
  end;
  Result := Param.FContext.StatusOK;
end;

procedure TLiSTMT.ExecWhile(Param: TLiParam);
var
  cntx: TLiContext;
  tmpv: TLiValue;
  mark: integer;
begin
  cntx := Param.FContext;
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

function TLiSTMT.GetCount: integer;
begin
  if FItems <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function TLiSTMT.GetItems: TLiSTMTList;
begin
  if FItems = nil then
    FItems := TLiSTMTList.Create;
  Result := FItems;
end;

{ TLiSTMT_assign }

constructor TLiSTMT_assign.Create(AParent: TLiSTMTList);
begin
  inherited;
  FStyle := ssAssign;
end;

procedure TLiSTMT_assign.Decompile(Level: integer; Lines: TStrings);

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

procedure TLiSTMT_assign.ExecAssign(Param: TLiParam);
var
  I: integer;
  V: TLiValue;
begin
  I := Param.FFunc.FParams.IndexOf(FVarb);
  V := Param.FParams[I];
  if FExpr.Execute(Param, V) then
    Param.FFunc.FParams[I].FType._Convert(V);
end;

procedure TLiSTMT_assign.ExecConst(Param: TLiParam);
begin
  if FExpr.Execute(Param, Param.FResult) then
  begin
    Param.FFunc.FResultType._Convert(Param.FResult);
    Param.FFunc.FModule.FConsts.Add(FVarb).SetValue(Param.FResult);
  end;
end;

procedure TLiSTMT_assign.ExecResult(Param: TLiParam);
begin
  if FExpr.Execute(Param, Param.FResult) then
    Param.FFunc.FResultType._Convert(Param.FResult);
end;

function TLiSTMT_assign.Execute(Param: TLiParam): boolean;
begin
  case FStyle of
    ssConst : ExecConst(Param);
    ssAssign: ExecAssign(Param);
    ssResult: ExecResult(Param);
  end;
  Result := Param.FContext.StatusOK;
end;

{ TLiSTMT_for }

constructor TLiSTMT_for.Create(AParent: TLiSTMTList);
begin
  inherited;
  FStyle := ssFor;
end;

procedure TLiSTMT_for.Decompile(Level: integer; Lines: TStrings);
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

destructor TLiSTMT_for.Destroy;
begin
  FreeAndNil(FEndValue);
  inherited;
end;

function TLiSTMT_for.Execute(Param: TLiParam): boolean;
var
  cntx: TLiContext;
  mark, I: integer;
  G: TLiGenerate;
  V: TLiValue;
  T: TLiType;

  function get_generate: TLiGenerate;
  var
    begv, endv: TLiValue;
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
  cntx := Param.FContext;
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
        T._Convert(V);
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

{ TLiIfSTMT }

constructor TLiSTMT_if.Create(AParent: TLiSTMTList);
begin
  inherited;
  FStyle := ssIf;
end;

procedure TLiSTMT_if.Decompile(Level: integer; Lines: TStrings);

  procedure decompile_else(S: TLiSTMT_if);
  begin
    if S.IsElif then
    begin
      Lines.Add(Margin(Level) + 'elif ' + S.FExpr.Decompile + ' then');
      if S.GetCount > 0 then
        S.FItems.Decompile(Level + 1, Lines);
      decompile_else(S.FElseItems[0] as TLiSTMT_if);
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

destructor TLiSTMT_if.Destroy;
begin
  FreeAndNil(FElseItems);
  inherited;
end;

function TLiSTMT_if.Execute(Param: TLiParam): boolean;
var
  tmpv: TLiValue;
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

function TLiSTMT_if.GetElseCount: integer;
begin
  if FElseItems <> nil then
    Result := FElseItems.Count else
    Result := 0;
end;

function TLiSTMT_if.GetElseItems: TLiSTMTList;
begin
  if FElseItems = nil then
    FElseItems := TLiSTMTList.Create;
  Result := FElseItems;
end;

function TLiSTMT_if.IsElif: boolean;
begin
  Result := (GetElseCount = 1) and (FElseItems[0].FStyle = ssIf);
end;

{ TLiSTMT_case }

constructor TLiSTMT_case.Create(AParent: TLiSTMTList);
begin
  inherited;
  FStyle := ssCase;
end;

procedure TLiSTMT_case.Decompile(Level: integer; Lines: TStrings);
var
  I: integer;
  S: TLiSTMT;
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

function TLiSTMT_case.Execute(Param: TLiParam): boolean;
var
  tmpv, T: TLiValue;
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
      if Param.FContext.StatusOK then
        Result := Result or FElseItems.Execute(Param) else
        Result := false;
    end;
  finally
    Param.EndExec(mark);
  end;
end;

{ TLiSTMT_try }

constructor TLiSTMT_try.Create(AParent: TLiSTMTList);
begin
  inherited;
  FStyle := ssTry;
end;

procedure TLiSTMT_try.Decompile(Level: integer; Lines: TStrings);
begin
  Lines.Add(Margin(Level) + 'try');
  FItems.Decompile(Level + 1, Lines);
  if FTryFinally then
    Lines.Add(Margin(Level) + 'finally') else
    Lines.Add(Margin(Level) + 'except');
  FElseItems.Decompile(Level + 1, Lines);
end;

function TLiSTMT_try.Execute(Param: TLiParam): boolean;
var
  E: boolean;
begin
  FItems.Execute(Param);
  E := Param.FContext.FExcepted;
  Param.FContext.FExcepted := false;
  if FTryFinally or E then
    FElseItems.Execute(Param);
  if FTryFinally and E then
    Param.FContext.FExcepted := true;
  Result := Param.FContext.StatusOK;
end;

{ TLiSTMTList }

function TLiSTMTList.Add(Style: TLiSTMTStyle): TLiSTMT;
begin
  if FItems = nil then FItems := TList.Create;
  Result := nil;
  case Style of
    ssNormal: Result := TLiSTMT.Create(Self);
    ssConst : Result := TLiSTMT_assign.Create(Self);
    ssAssign: Result := TLiSTMT_assign.Create(Self);
    ssResult: Result := TLiSTMT_assign.Create(Self);
    ssPuts  : Result := TLiSTMT.Create(Self);
    ssIf    : Result := TLiSTMT_if.Create(Self);
    ssWhile : Result := TLiSTMT.Create(Self);
    ssRepeat: Result := TLiSTMT.Create(Self);
    ssFor   : Result := TLiSTMT_for.Create(Self);
    ssCase  : Result := TLiSTMT_case.Create(Self);
    ssTry   : Result := TLiSTMT_try.Create(Self);
    ssRaise : Result := TLiSTMT.Create(Self);
  end;
  Result.FStyle := Style;
end;

function TLiSTMTList.Add(STMT: TLiSTMT): integer;
begin
  if FItems = nil then FItems := TList.Create;
  if STMT.FParent <> Self then
  begin
    STMT.FParent.FItems.Remove(STMT);
    STMT.FParent := Self;
  end;
  Result := FItems.Add(STMT);
end;

procedure TLiSTMTList.Clear;
var
  I: integer;
begin
  for I := GetCount - 1 downto 0 do Delete(I);
  FreeAndNil(FItems);
end;

procedure TLiSTMTList.Decompile(Level: integer; Lines: TStrings);
var
  I: integer;
begin
  if Level < 1 then Level := 1;
  for I := 0 to GetCount - 1 do
    GetItem(I).Decompile(Level, Lines);
end;

procedure TLiSTMTList.Delete(Index: integer);
var
  S: TLiSTMT;
begin
  S := GetItem(Index);
  FItems.Delete(Index);
  S.FParent := nil;
  S.Free;
end;

destructor TLiSTMTList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TLiSTMTList.Execute(Param: TLiParam): boolean;
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

function TLiSTMTList.GetCount: integer;
begin
  if FItems <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function TLiSTMTList.GetItem(Index: integer): TLiSTMT;
begin
  Result := TLiSTMT(FItems[Index]);
end;

{ TLiVarb }

constructor TLiVarb.Create(const AName: string; AType: TLiType);
begin
  FName := AName;
  FType := AType;
end;

destructor TLiVarb.Destroy;
begin
  FName := '';
  inherited;
end;

function TLiVarb.Prototype: string;
begin
  Result := FType.Prototype(FName);
end;

{ TLiVarbList }

function TLiVarbList.GetCount: integer;
begin
  Result := Length(FVarbs);
end;

function TLiVarbList.GetParamCount: integer;
begin
  Result := GetCount - FLocalCount;
end;

function TLiVarbList.GetVarb(Index: integer): TLiVarb;
begin
  Result := FVarbs[Index];
end;

function TLiVarbList.IndexOf(const AVarb: TLiVarb): integer;
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

function TLiVarbList.DoAdd(const AName: string; AType: TLiType): TLiVarb;
var
  I: integer;
begin
  Result := TLiVarb.Create(AName, AType);
  I := Length(FVarbs);
  SetLength(FVarbs, I + 1);
  FVarbs[I] := Result;
end;

constructor TLiVarbList.Create;
begin
  SetLength(FVarbs, 0);
  FLocalCount := 0;
end;

destructor TLiVarbList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLiVarbList.Clear;
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

function TLiVarbList.Add(const AName: string; AType: TLiType): TLiVarb;
begin
  Result := Find(AName);
  if Result = nil then
    Result := DoAdd(AName, AType);
end;

function TLiVarbList.AddLocal(const AName: string; AType: TLiType): TLiVarb;
begin
  Result := Find(AName);
  if Result = nil then
  begin
    Result := DoAdd(AName, AType);
    Inc(FLocalCount);
  end;
end;

procedure TLiVarbList.Assign(Source: TLiVarbList);
var
  I: integer;
begin
  Clear;
  for I := 0 to Source.GetCount - 1 do
    DoAdd(Source[I].FName, Source[I].FType);
  FLocalCount := Source.FLocalCount;
end;

function TLiVarbList.IndexOf(const AName: string): integer;
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

function TLiVarbList.Find(const AName: string): TLiVarb;
var
  I: integer;
begin
  I := IndexOf(AName);
  if (I >= 0) and (I < Length(FVarbs)) then
    Result := FVarbs[I] else
    Result := nil;
end;

{ TLiFunc }

constructor TLiFunc.Create(const AName: string; M: TLiModule; Proc: TLiLyseeProc);
begin
  FName := AName;
  if FName <> '' then
  begin
    IncRefcount;
    M.FFuncList.FItems.Add(Self);
  end;
  FModule := M;
  FParams := TLiVarbList.Create;
  FResultType := my_variant;
  FProc := Proc;
  FMinArgs := -1;
  if Context <> nil then
    if Context.FRollbacks <> nil then
      if Context.FRollbacks.IndexOf(FModule) < 0 then
        Context.FRollbacks.Add(Self);
end;

constructor TLiFunc.CreateMethod(const AName: string; P: TLiType; Proc: TLiLyseeProc);
begin
  FName := AName;
  FParent := P;
  FParent.FMethods.FItems.Add(Self);
  IncRefcount;
  FModule := FParent.FModule;
  FParams := TLiVarbList.Create;
  FResultType := my_variant;
  FProc := Proc;
  FMinArgs := -1;
end;

procedure TLiFunc.Decompile(Level: integer; Lines: TStrings);
begin
  Lines.Add(Margin(Level) + Prototype);
  if (FSTMTs <> nil) and not IsConst then
    FSTMTs.Decompile(Level + 1, Lines);
end;

destructor TLiFunc.Destroy;
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

function TLiFunc.Executing: boolean;
var
  P: TLiParam;
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

function TLiFunc.AddCode(const Code: string): boolean;
var
  P: TLiParser;
begin
  Result := ChangeAble;
  if Result and (Code <> '') then
  begin
    P := TLiParser.Create(FModule);
    try
      P.FTokenizer := TLiTokenizer.Create(Code);
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

function TLiFunc.AsString: string;
begin
  Result := Prototype;
end;

function TLiFunc.ChangeAble: boolean;
begin
  Result := (FName = '') and not Assigned(FProc) and not Executing;
end;

function TLiFunc.Context: TLiContext;
begin
  if FModule <> nil then
    Result := FModule.FContext else
    Result := nil;
end;

function TLiFunc.GetFullName: string;
begin
  if FParent <> nil then
    Result := FParent.FName + '.' + Name else
    Result := FModule.FName + '.' + Name;
end;

function TLiFunc.GetMinArgs: integer;
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

function TLiFunc.GetSTMTs: TLiSTMTList;
begin
  if FSTMTs = nil then
    FSTMTs := TLiSTMTList.Create;
  Result := FSTMTs;
end;

function TLiFunc.Prototype: string;
var
  X: integer;
  P: TLiVarb;
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

function TLiFunc.SetCode(const Code: string): boolean;
begin
  Result := ChangeAble;
  if Result then
  begin
    if FSTMTs <> nil then FSTMTs.Clear;
    Result := AddCode(Code);
  end;
end;

function TLiFunc.FindBy(const ID: string; rec: PLiFind; Range: TLiFinds): boolean;
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

function TLiFunc.IsConst: boolean;
begin
  Result := (FSTMTs <> nil) and (FSTMTs.Count = 1) and (FSTMTs[0].FStyle = ssConst);
end;

function TLiFunc.IsConstructor: boolean;
begin
  Result := (Self <> nil) and (FParent <> nil) and (FParent.FConstructer = Self);
end;

function TLiFunc.IsFunction: boolean;
begin
  Result := (FResultType <> my_nil);
end;

function TLiFunc.IsMainFunc: boolean;
begin
  Result := (FModule.FContext <> nil) and
            (FModule.FContext.FMainFunc = Self);
end;

function TLiFunc.IsMethod: boolean;
begin
  Result := (Self <> nil) and (FParent <> nil);
end;

function TLiFunc.IsProcedure: boolean;
begin
  Result := (FResultType = my_nil);
end;

function TLiFunc.MakeMethod: TLiFunc;
var
  P: TLiType;
begin
  Result := nil;
  if (Self <> nil) and (FParent = nil) and Assigned(FProc) and (FParams.Count > 0) then
  begin
    P := FParams[0].FType;
    if (P <> my_nil) and (P <> my_variant) and (P.FindMethod(FName) = nil) then
    begin
      Result := TLiFunc.CreateMethod(FName, P, FProc);
      Result.FResultType := FResultType;
      Result.FParams.Assign(FParams);
      Result.FHasVarArg := FHasVarArg;
      Result.FData := FData;
    end;
  end;
end;

function TLiFunc.FindInside(const ID: string; rec: PLiFind): boolean;
var
  findrec: RLiFind;
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

function TLiFunc.FindSave(const ID: string; Param: TLiParam; Outv: TLiValue): TLiFind;
var
  R: RLiFind;
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

{ TLiFuncList }

constructor TLiFuncList.Create;
begin
  FItems := TList.Create;
end;

destructor TLiFuncList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TLiFuncList.Get(const Name: string): TLiFunc;
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

function TLiFuncList.GetCount: integer;
begin
  if FItems <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function TLiFuncList.GetItem(Index: integer): TLiFunc;
begin
  Result := TLiFunc(FItems[Index]);
end;

procedure TLiFuncList.Clear;
var
  I: integer;
begin
  for I := GetCount - 1 downto 0 do
    Delete(I);
end;

procedure TLiFuncList.Delete(Index: integer);
var
  F: TLiFunc;
begin
  F := GetItem(Index);
  FItems.Delete(Index);
  F.FModule := nil;
  F.FParent := nil;
  F.Free
end;

{ TLiModule }

function TLiModule.AddEnumType(const AName: string;
  const ItemNames: array of string): TLiEnumType;
begin
  Result := TLiEnumType.Create(AName, Self, nil);
  Result.Add(ItemNames);
end;

function TLiModule.AddFunc(const AName: string; T: TLiType;
  const ParamNames: array of string; const ParamTypes: array of TLiType;
  const Proc: TLiLyseeProc; const Data: pointer): TLiFunc;
var
  I: integer;
  E: boolean;
begin
  Result := nil;
  E := false;
  if (T <> nil) and (AName <> '') and Assigned(Proc) then
    if not Find(AName) and ParamOK(ParamNames, ParamTypes, false, E) then
    begin
      Result := TLiFunc.Create(AName, Self, Proc);
      Result.FResultType := T;
      Result.FHasVarArg := E;
      Result.FData := Data;
      for I := 0 to Length(ParamNames) - 1 do
        Result.FParams.Add(ParamNames[I], ParamTypes[I]);
    end;
end;

function TLiModule.AddFunc(const AName: string;
  const ParamNames: array of string; const ParamTypes: array of TLiType;
  const Proc: TLiLyseeProc; const Data: pointer): TLiFunc;
begin
  Result := AddFunc(AName, my_nil, ParamNames, ParamTypes, Proc, Data);
end;

function TLiModule.AsString: string;
begin
  Result := FName;
end;

constructor TLiModule.Create(const AName: string);
begin
  inherited;
  InitModule;
  my_modules.Add(Self);
  FNeedSetup := true;
end;

constructor TLiModule.CreateEx(const AName: string; AContext: TLiContext);
begin
  inherited Create(AName);
  InitModule;
  FContext := AContext;
  if FContext <> nil then
  begin
    FContext.FModules.Add(Self);
    FModules := TLiModuleList.Create(FContext);
    FModules.FImporter := Self;
    FImporters := TList.Create;
    if FContext.FRollbacks <> nil then
      FContext.FRollbacks.Add(Self);
    Use(my_system);
  end
  else my_modules.Add(Self);
  FNeedSetup := true;
end;

procedure TLiModule.InitModule;
begin
  IncRefcount;
  FFileName := my_kernel;
  FTypeList := TList.Create;
  FFuncList := TLiFuncList.Create;
  FConsts := TLiHash.Create;
  FConsts.CaseSensitive := false;
end;

destructor TLiModule.Destroy;
var
  A: integer;
  P: TLiModule;
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

procedure TLiModule.DeleteFunctions;
begin
  FFuncList.Clear;
end;

procedure TLiModule.DeleteTypes;
var
  I: integer;
  T: TLiType;
begin
  for I := TypeCount - 1 downto 0 do
  begin
    T := GetType(I);
    FTypeList.Delete(I);
    T.Free;
  end;
end;

function TLiModule.GetType(Index: integer): TLiType;
begin
  Result := TLiType(FTypeList[Index]);
end;

function TLiModule.FindModule(const ID: string; FindPossible: boolean): TLiModule;
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

function TLiModule.FindSave(const AName: string; Value: TLiValue): boolean;
var
  srec: RLiFind;
begin
  Result := (Self <> nil) and Find(AName, @srec);
  if Result then
    Value.SetFind(@srec);
end;

function TLiModule.FindSaveBy(const AName: string; Value: TLiValue): boolean;
var
  srec: RLiFind;
begin
  Result := (Self <> nil) and FindBy(AName, '', @srec);
  if Result then
    value.SetFind(@srec);
end;

procedure TLiModule.Use(M: TLiModule);
begin
  if (M <> nil) and (M <> Self) and (M <> my_system) then
    if (FModules <> nil) and not FModules.Has(M) then
    begin
      FModules.Add(M);
      if M.FImporters <> nil then
        M.FImporters.Add(Self);
    end;
  if M.FNeedSetup then M.Setup;
end;

function TLiModule.IsMainModule: boolean;
begin
  Result := (FContext <> nil) and (FContext.FMainModule = Self);
end;

function TLiModule.EnsureName(const AName: string): string;
begin
  if AName = '' then
  begin
    Inc(FContext.FNameSeed);
    Result := Format('#%.3x', [FContext.FNameSeed]);
  end
  else Result := AName;
end;

function TLiModule.FindFunc(const ID: string): TLiFunc;
begin
  Result := TLiFunc(FFuncList.Get(ID));
end;

function TLiModule.FindFuncBy(const ID: string): TLiFunc;
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

function TLiModule.FindType(const ID: string): TLiType;
var
  X: integer;
begin
  for X := 0 to FTypeList.Count - 1 do
  begin
    Result := TLiType(FTypeList[X]);
    if MatchID(ID, Result.FName) then Exit;
  end;
  Result := nil;
end;

function TLiModule.FindTypeBy(const ID, AModule: string): TLiType;
var
  X: integer;
  M: TLiModule;
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

procedure TLiModule.Setup;
var
  I: integer;
  T: TLiType;
begin
  FNeedSetup := false;
  for I := 0 to FTypeList.Count - 1 do
  begin
    T := GetType(I);
    if T.IsObject and (T.MethodCount = 0) then
      T.Setup;
  end;
end;

function TLiModule.SetupType(const T: TLiType): boolean;
begin
  Result := (T <> nil) and IsID(T.FName) and not Find(T.FName);
  if Result then
  begin
    T.FModule := Self;
    FTypeList.Add(T);
  end
  else T.Free;
end;

function TLiModule.Find(const ID: string; rec: PLiFind): boolean;
var
  FR: RLiFind;
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

function TLiModule.FindBy(const ID, AModule: string; rec: PLiFind): boolean;
var
  X: integer;
  M: TLiModule;
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

function TLiModule.TypeCount: integer;
begin
  Result := FTypeList.Count;
end;

function TLiModule.UseModule(const AName: string): TLiModule;
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
      Result := TLiModule.CreateEx(AName, FContext);
      try
        Result.FFileName := F;
        TLiParser.Create(Result).ParseAndFree(S, true);
      except
        FreeAndNil(Result);
        raise;
      end;
    end;
  end;
  Use(Result);
end;

function TLiModule.AddFunc(const AName: string; T: TLiType;
  const Proc: TLiLyseeProc; const Data: pointer): TLiFunc;
begin
  Result := AddFunc(AName, T, [], [], Proc, Data);
end;

function TLiModule.AddFunc(const AName: string; const Proc: TLiLyseeProc;
  const Data: pointer): TLiFunc;
begin
  Result := AddFunc(AName, my_nil, [], [], Proc, Data);
end;

{ TLiModuleList }

function TLiModuleList.Add(AModule: TLiModule): integer;
begin
  Result := IndexOf(AModule);
  if Result < 0 then
    Result := FModules.Add(AModule);
end;

procedure TLiModuleList.Clear;
var
  A: integer;
begin
  DeleteFunctions;
  ClearConsts;
  for A := FModules.Count - 1 downto 0 do
    Delete(A);
end;

procedure TLiModuleList.ClearConsts;
var
  I: integer;
begin
  if FImporter = nil then
    for I := 0 to GetCount - 1 do
      GetModule(I).FConsts.Clear;
end;

constructor TLiModuleList.Create(AContext: TLiContext);
begin
  IncRefcount;
  FContext := AContext;
  FModules := TList.Create;
end;

procedure TLiModuleList.Delete(Index: integer);
var
  M: TLiModule;
begin
  M := GetModule(Index);
  FModules.Delete(Index);
  if FImporter = nil then M.Free; {<--TLiContext.FModules/my_modules}
end;

destructor TLiModuleList.Destroy;
begin
  Clear;
  FreeAndNil(FModules);
  inherited;
end;

function TLiModuleList.Find(const Name: string): TLiModule;
var
  X: integer;
begin
  X := IndexOf(Name);
  if X >= 0 then
    Result := GetModule(X) else
    Result := nil;
end;

function TLiModuleList.GetCount: integer;
begin
  Result := FModules.Count;
end;

function TLiModuleList.GetModule(Index: integer): TLiModule;
begin
  Result := TLiModule(FModules[Index]);
end;

function TLiModuleList.Has(const Name: string): boolean;
begin
  Result := (IndexOf(Name) >= 0);
end;

function TLiModuleList.Has(AModule: TLiModule): boolean;
begin
  Result := (IndexOf(AModule) >= 0);
end;

function TLiModuleList.IndexOf(const Name: string): integer;
var
  X: integer;
  M: TLiModule;
begin
  if Name <> '' then
    for X := 0 to FModules.Count - 1 do
    begin
      M := TLiModule(FModules[X]);
      if MatchID(M.Name, Name) then
      begin
        Result := X;
        Exit;
      end;
    end;
  Result := -1;
end;

procedure TLiModuleList.Setup;
var
  I: integer;
begin
  for I := 0 to GetCount - 1 do
    GetModule(I).Setup;
end;

function TLiModuleList.IndexOf(AModule: TLiModule): integer;
begin
  Result := FModules.IndexOf(AModule);
end;

procedure TLiModuleList.DeleteFunctions;
var
  index: integer;
begin
  if FImporter = nil then
    for index := 0 to GetCount - 1 do
      GetModule(index).DeleteFunctions;
end;

function TLiModuleList.ToList: TLiList;
var
  index: integer;
begin
  Result := TLiList.Create;
  for index := 0 to GetCount - 1 do
    Result.Add.SetAsModule(GetModule(index));
end;

{ TLiString }

function TLiString.AsString: string;
begin
  Result := FValue;
end;

constructor TLiString.Create(const S: string);
begin
  FValue := S;
end;

constructor TLiString.CreateIncRefcount(const S: string);
begin
  FValue := S;
  IncRefcount;
end;

function TLiString.Length: integer;
begin
  if Self <> nil then
    Result := System.Length(FValue) else
    Result := 0;
end;

{ TLiGarbage }

procedure TLiGarbage.Clear;
begin

end;

constructor TLiGarbage.Create;
begin
  if my_gcman <> nil then my_gcman.GcAdd(Self);
end;

destructor TLiGarbage.Destroy;
begin
  if my_gcman <> nil then
  begin
    my_gcman.GcRemove(Self);
    my_gcman.FDead.Remove(Self);
  end;
  inherited;
end;

procedure TLiGarbage.MarkForSurvive;
begin
  { do nothing }
end;

{ TLiGarbageCollect }

function TLiGarbageCollect.Collect: integer;

  procedure change_ref(Increase: boolean);
  var
    I: integer;
    G: TLiGarbage;
  begin
    for I := 0 to FDead.Count - 1 do
      if I < FDead.Count then
      begin
        G := TLiGarbage(FDead[I]);
        if Increase then
          G.IncRefcount else
          G.DecRefcount;
      end;
  end;

var
  I: integer;
  G: TLiGarbage;
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
        TLiGarbage(FDead[I]).Clear;
  finally
    change_ref(false);
  end;

  // destroy rest containers
  for I := FDead.Count - 1 downto 0 do
    if I < FDead.Count then
    begin
      G := TLiGarbage(FDead[I]);
      FDead.Delete(I);
      G.Free;
    end;
end;

procedure TLiGarbageCollect.MarkSurvived;
var
  I: integer;
begin
  for I := 0 to FContexts.Count - 1 do
    TLiContext(FContexts[I]).MarkSurvived;
end;

constructor TLiGarbageCollect.Create;
begin
  FContexts := TList.Create;
  FDead := TList.Create;
end;

destructor TLiGarbageCollect.Destroy;
begin
  FreeAndNil(FDead);
  FreeAndNil(FContexts);
  inherited;
end;

procedure TLiGarbageCollect.GcAdd(G: TLiGarbage);
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

procedure TLiGarbageCollect.GcRemove(G: TLiGarbage);
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

{ TLiList }

destructor TLiList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

procedure TLiList.PrepareFor(Func: TLiFunc);
var
  I, N: integer;
begin
  N := GetCount;
  SetCount(Func.FParams.Count);
  for I := 0 to Func.FParams.Count - 1 do
    if I < N then
      Func.FParams[I].FType._Convert(GetItem(I)) else
      Func.FParams[N].FType._SetDefault(GetItem(N));
end;

procedure TLiList.Remove(Value: TLiValue);
var
  I: integer;
begin
  I := FItems.IndexOf(Value);
  if I >= 0 then Delete(I);
end;

function TLiList.CopyRight(ItemCount: integer): TLiList;
begin
  Result := Copy(GetCount - ItemCount, ItemCount);
end;

function TLiList.AddList: TLiList;
var
  V: TLiValue;
begin
  Result := TLiList.Create;
  Result.IncRefcount;
  V := Add;
  V.FType := my_list;
  V.FValue.VObject := Result;
end;

procedure TLiList.AddStrings(List: TStrings);
var
  I: integer;
begin
  for I := 0 to List.Count - 1 do
    Add.AsString := List[I];
end;

function TLiList.AsString: string;
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

procedure TLiList.Clear;
var
  I: integer;
begin
  for I := GetCount - 1 downto 0 do
    Delete(I);
  inherited;
end;

function TLiList.Copy(Index, ItemCount: integer): TLiList;
var
  I: integer;
begin
  Result := TLiList.Create;
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

constructor TLiList.Create;
begin
  inherited;
  FItems := TList.Create;
end;

procedure TLiList.Delete(Index: integer);
var
  V: TLiValue;
begin
  V := TLiValue(FItems[Index]);
  FItems.Delete(Index);
  V.Free;
end;

procedure TLiList.DeleteLast;
begin
  Delete(GetCount - 1);
end;

procedure TLiList.Exchange(Index1, Index2: integer);
begin
  FItems.Exchange(Index1, Index2);
end;

function TLiList.First: TLiValue;
begin
  Result := GetItem(0);
end;

function TLiList.GetCount: integer;
begin
  if Self <> nil then
    Result := FItems.Count else
    Result := 0;
end;

procedure TLiList.GetFirstTwo(var V1, V2: TLiValue);
var
  N: integer;
begin
  V1 := nil;
  V2 := nil;
  N := GetCount;
  if N > 0 then V1 := GetItem(0);
  if N > 1 then V2 := GetItem(1);
end;

function TLiList.GetItem(Index: integer): TLiValue;
begin
  Result := TLiValue(FItems[Index]);
end;

function TLiList.IndexOf(Value: TLiValue): integer;
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

function TLiList.Insert(Index: integer; Value: TLiValue): TLiValue;
begin
  Result := TLiValue.Create;
  FItems.Insert(Index, Result);
  if Value <> nil then
    Result.SetValue(Value);
end;

function TLiList.Last: TLiValue;
begin
  Result := GetItem(GetCount - 1);
end;

function TLiList.CopyLeft(ItemCount: integer): TLiList;
begin
  Result := Copy(0, ItemCount);
end;

procedure TLiList.MarkForSurvive;
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

procedure TLiList.Move(CurIndex, NewIndex: integer);
begin
  FItems.Move(CurIndex, NewIndex);
end;

function TLiList.Add(Value: TLiValue): TLiValue;
begin
  Result := TLiValue.Create;
  FItems.Add(Result);
  if Value <> nil then
    Result.SetValue(Value);
end;

procedure TLiList.SetCount(NewCount: integer);
begin
  if NewCount < 1 then Clear else
  begin
    while NewCount > GetCount do Add;
    while NewCount < GetCount do DeleteLast;
  end;
end;

procedure TLiList.Sort;
begin
  FItems.Sort(@SortValueCompare);
end;

{ TLiHashItem }

function TLiHashItem.Format: string;
begin
  Result := FormatString(FKey) + ':' + FormatValue(Self);
end;

{ TLiHash }

function TLiHash.Add(const Name: string): TLiHashItem;
var
  I: integer;
begin
  Result := Get(Name);
  if Result = nil then
  begin
    Result := TLiHashItem.Create;
    Result.FKey := Name;
    I := HashIndex(Result.FKey);
    Result.FNext := FBuckets[I];
    FBuckets[I] := Result;
    Inc(FCount);
    Resize(FCount div LSE_HASH_DELTA);
  end;
end;

function TLiHash.AsString: string;
var
  H: TLiHashItem;
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
          H := TLiHashItem(L.First);
          Result := '[' + H.Format;
          for I := 1 to L.Count - 1 do
          begin
            H := TLiHashItem(L[I]);
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

function TLiHash.DefConst(const Name, Value: string): TLiHashItem;
begin
  Result := Add(Name);
  Result.AsString := Value;
end;

function TLiHash.DefConst(const Name: string; Value: int64): TLiHashItem;
begin
  Result := Add(Name);
  Result.AsInteger := Value;
end;

function TLiHash.DefConst(const Name: string; Value: double): TLiHashItem;
begin
  Result := Add(Name);
  Result.AsFloat := Value;
end;

function TLiHash.DefConst(const Name: string; Value: boolean): TLiHashItem;
begin
  Result := Add(Name);
  Result.SetAsBoolean(Value);
end;

function TLiHash.BucketCount: integer;
begin
  Result := Length(FBuckets);
end;

procedure TLiHash.Clear;
var
  I: integer;
  H: TLiHashItem;
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

constructor TLiHash.Create;
begin
  inherited;
  SetLength(FBuckets, 1);
  FBuckets[0] := nil;
end;

destructor TLiHash.Destroy;
begin
  Clear;
  SetLength(FBuckets, 0);
  inherited;
end;

procedure TLiHash.FreeItem(H: TLiHashItem);
begin
  Dec(FCount);
  H.Free;
end;

function TLiHash.Has(const Name: string): boolean;
begin
  Result := (Get(Name) <> nil);
end;

function TLiHash.HashIndex(const Name: string): integer;
var
  N: integer;
begin
  N := BucketCount;
  if N = 1 then Result := 0 else
  if FCaseSensitive then
    Result := (basic.HashOf(Name) mod N) else
    Result := (basic.HashOf(LowerCase(Name)) mod N);
end;

function TLiHash.IsEmpty: boolean;
begin
  Result := (FCount = 0);
end;

function TLiHash.Get(const Name: string): TLiHashItem;
begin
  Result := FBuckets[HashIndex(Name)];
  while Result <> nil do
  begin
    if MatchName(Name, Result.FKey) then Exit;
    Result := Result.FNext;
  end;
end;

function TLiHash.GetValue(const Name: string; Value: TLiValue): boolean;
var
  V: TLiValue;
begin
  V := Get(Name);
  Result := (V <> nil);
  if Result and (Value <> nil) then
    Value.SetValue(V);
end;

procedure TLiHash.ListKeys(List: TLiList);
var
  I: integer;
  L: TList;
begin
  if Self <> nil then
  begin
    L := SetupItemList;
    try
      for I := 0 to L.Count - 1 do
        List.Add.SetAsString(TLiHashItem(L[I]).FKey);
    finally
      L.Free;
    end;
  end;
end;

procedure TLiHash.ListValues(List: TLiList);
var
  I: integer;
  L: TList;
begin
  if Self <> nil then
  begin
    L := SetupItemList;
    try
      for I := 0 to L.Count - 1 do
        List.Add(TLiHashItem(L[I]));
    finally
      L.Free;
    end;
  end;
end;

procedure TLiHash.MarkForSurvive;
var
  H: TLiHashItem;
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

function TLiHash.MatchName(const N1, N2: string): boolean;
begin
  if FCaseSensitive then
    Result := (N1 = N2) else
    Result := SysUtils.SameText(N1, N2);
end;

function TLiHash.Remove(const Name: string): boolean;
var
  X: cardinal;
  H, T: TLiHashItem;
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

procedure TLiHash.Resize(NewSize: integer);
var
  L: TList;
  I, X: integer;
  H: TLiHashItem;
begin
  if NewSize > 512 then NewSize := 512;
  X := BucketCount;
  if NewSize > X then
  begin
    L := SetupItemList;
    try
      while X < NewSize do X := X shl 1;
      SetLength(FBuckets, X);
      FillChar(FBuckets[0], sizeof(TLiHashItem) * X, 0);
      for I := 0 to L.Count - 1 do
      begin
        H := TLiHashItem(L[I]);
        X := HashIndex(H.FKey);
        H.FNext := FBuckets[X];
        FBuckets[X] := H;
      end;
    finally
      L.Free;
    end;
  end;
end;

function TLiHash.SetupItemList: TList;
var
  I: integer;
  H: TLiHashItem;
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

{ TLiGenerate }

function TLiGenerate.GetNext: boolean;
begin
  Result := false;
end;

function TLiGenerate.HasNext: boolean;
begin
  Result := false;
end;

{ TLiGenerate_string }

constructor TLiGenerate_string.CreateIn(const S: string);
begin
  inherited Create;
  FS := S;
  FIndex := 0;
end;

function TLiGenerate_string.GetNext: boolean;
begin
  Result := HasNext;
  if Result then
  begin
    Inc(FIndex);
    SetAsChar(FS[FIndex]);
  end
  else SetNil;
end;

function TLiGenerate_string.HasNext: boolean;
begin
  Result := (FIndex < Length(FS));
end;

{ TLiGenerate_list }

constructor TLiGenerate_list.CreateIn(const AList: TLiList);
begin
  inherited Create;
  FL := AList;
  FIndex := -1;
end;

function TLiGenerate_list.GetNext: boolean;
begin
  Result := HasNext;
  if Result then
  begin
    Inc(FIndex);
    SetValue(FL[FIndex]);
  end
  else SetNil;
end;

function TLiGenerate_list.HasNext: boolean;
begin
  Result := (FL <> nil) and (FIndex < FL.Count - 1);
end;

{ TLiGenerate_int64 }

constructor TLiGenerate_int64.CreateIn(V1, V2: int64; Upto: boolean);
begin
  inherited Create;
  FV := V1;
  FUpto := Upto;
  if FUpto then
    FCount := V2 - FV + 1 else
    FCount := FV - V2 + 1;
end;

constructor TLiGenerate_int64.CreateIn(Range: int64);
begin
  CreateIn(0, Range - 1, true);
end;

function TLiGenerate_int64.GetNext: boolean;
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

function TLiGenerate_int64.HasNext: boolean;
begin
  Result := (FCount > 0);
end;

{ TLiGenerate_char }

constructor TLiGenerate_char.CreateIn(V1, V2: char; Upto: boolean);
begin
  inherited Create;
  FV := V1;
  FUpto := Upto;
  if FUpto then
    FCount := Ord(V2) - Ord(FV) + 1 else
    FCount := Ord(FV) - Ord(V2) + 1;
end;

function TLiGenerate_char.GetNext: boolean;
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

function TLiGenerate_char.HasNext: boolean;
begin
  Result := (FCount > 0);
end;

{ TLiGenerate_boolean }

constructor TLiGenerate_boolean.CreateIn(V1, V2, Upto: boolean);
begin
  inherited Create;
  FV := V1;
  FUpto := Upto;
  if FUpto then
    FCount := Ord(V2) - Ord(FV) + 1 else
    FCount := Ord(FV) - Ord(V2) + 1;
end;

function TLiGenerate_boolean.GetNext: boolean;
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

function TLiGenerate_boolean.HasNext: boolean;
begin
  Result := (FCount > 0);
end;

{ TLiGenerate_strlist }

constructor TLiGenerate_strlist.CreateIn(const List: TStrings);
begin
  inherited Create;
  FL := List;
  FIndex := -1;
end;

function TLiGenerate_strlist.GetNext: boolean;
begin
  Result := HasNext;
  if Result then
  begin
    Inc(FIndex);
    SetAsString(FL[FIndex]);
  end
  else SetNil;
end;

function TLiGenerate_strlist.HasNext: boolean;
begin
  Result := (FL <> nil) and (FIndex < FL.Count - 1);
end;

{ TLiGenerate_enum }

constructor TLiGenerate_enum.CreateIn(V1, V2: TLiEnumItem; Upto: boolean);
begin
  inherited Create;
  FV := V1;
  FUpto := Upto;
  if (V1 = nil) or (V2 = nil) or (V1.FParent <> V2.FParent) then FCount := 0 else
  if FUpto then
    FCount := V2.FValue - FV.FValue + 1 else
    FCount := FV.FValue - V2.FValue + 1;
end;

function TLiGenerate_enum.GetNext: boolean;
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

function TLiGenerate_enum.HasNext: boolean;
begin
  Result := (FV <> nil) and (FCount > 0);
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
  my_gcman := TLiGarbageCollect.Create;
  my_modules := TLiModuleList.Create(nil);
  my_system := TLiModule.Create(LSE_SYSTE);

  //-- types -----------------------------------------------------------------

  my_variant := TLiType_variant.Create(TID_NAMES[TID_VARIANT], my_system, nil);
  my_variant.FStyle := tsVariant;
  my_variant.FTID := TID_VARIANT;
  my_types[TID_VARIANT] := my_variant;

  my_nil := TLiType_nil.Create(TID_NAMES[TID_NIL], my_system, nil);
  my_nil.FStyle := tsNil;
  my_nil.FTID := TID_NIL;
  my_types[TID_NIL] := my_nil;

  my_char := TLiType_char.Create(TID_NAMES[TID_CHAR], my_system, nil);
  my_char.FStyle := tsBasic;
  my_char.FTID := TID_CHAR;
  my_types[TID_CHAR] := my_char;

  my_int := TLiType_integer.Create(TID_NAMES[TID_INTEGER], my_system, nil);
  my_int.FStyle := tsBasic;
  my_int.FTID := TID_INTEGER;
  my_types[TID_INTEGER] := my_int;

  my_float := TLiType_float.Create(TID_NAMES[TID_FLOAT], my_system, nil);
  my_float.FStyle := tsBasic;
  my_float.FTID := TID_FLOAT;
  my_types[TID_FLOAT] := my_float;

  my_curr := TLiType_currency.Create(TID_NAMES[TID_CURRENCY], my_system, nil);
  my_curr.FStyle := tsBasic;
  my_curr.FTID := TID_CURRENCY;
  my_types[TID_CURRENCY] := my_curr;

  my_time := TLiType_time.Create(TID_NAMES[TID_time], my_system, nil);
  my_time.FStyle := tsBasic;
  my_time.FTID := TID_TIME;
  my_types[TID_time] := my_time;

  my_bool := TLiType_boolean.Create(TID_NAMES[TID_BOOLEAN], my_system, nil);
  my_bool.FStyle := tsBasic;
  my_bool.FTID := TID_BOOLEAN;
  my_types[TID_BOOLEAN] := my_bool;

  my_list := TLiType_list.Create(TID_NAMES[TID_LIST], AddModule('Classes'), nil);
  my_list.FTID := TID_LIST;
  my_types[TID_LIST] := my_list;

  my_hash := TLiType_hash.Create(TID_NAMES[TID_HASHLIST], my_system, nil);
  my_hash.FTID := TID_HASHLIST;
  my_types[TID_HASHLIST] := my_hash;

  my_string := TLiType_string.Create(TID_NAMES[TID_STRING], my_system, nil);
  my_string.FTID := TID_STRING;
  my_types[TID_STRING] := my_string;

  my_type := TLiType_type.Create(TID_NAMES[TID_TYPE], my_system, nil);
  my_type.FTID := TID_TYPE;
  my_types[TID_TYPE] := my_type;

  my_module := TLiType_module.Create(TID_NAMES[TID_MODULE], my_system, nil);
  my_module.FTID := TID_MODULE;
  my_types[TID_MODULE] := my_module;

  my_func := TLiType_func.Create(TID_NAMES[TID_FUNCTION], my_system, nil);
  my_func.FTID := TID_FUNCTION;
  my_types[TID_FUNCTION] := my_func;

  my_TID_seed := 100;

  //-- constant --------------------------------------------------------------

  my_system.FConsts.Add('MaxInt').SetAsInteger(High(int64));
  my_system.FConsts.Add('MinInt').SetAsInteger(Low(int64));
  my_system.FConsts.Add('PathDelim').SetAsChar(PathDelim);
  my_system.FConsts.Add('PathSep').SetAsChar(PathSep);
  my_system.FConsts.Add('PI').SetAsFloat(PI);
  my_system.FConsts.Add('PointerSize').SetAsInteger(sizeof(pointer));
  my_system.FConsts.Add('sLineBreak').SetAsString(sLineBreak);

  //-- operator --------------------------------------------------------------

  my_variant.AddOperate(opAdd, my_string, {$IFDEF FPC}@{$ENDIF}string_add_string);
  my_variant.AddOperate(opAdd, my_char, {$IFDEF FPC}@{$ENDIF}string_add_string);
  my_variant.AddOperate(opIn, my_list, {$IFDEF FPC}@{$ENDIF}variant_in_list);
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

  my_list.AddOperate(opAdd, my_list, {$IFDEF FPC}@{$ENDIF}list_add_list);
  my_list.AddOperate(opDec, my_list, {$IFDEF FPC}@{$ENDIF}list_dec_list);
  my_list.AddOperate(opMul, my_list, {$IFDEF FPC}@{$ENDIF}list_mul_list);
  my_list.AddOperate(opShi, my_variant, {$IFDEF FPC}@{$ENDIF}list_shi_variant);
  my_list.AddOperate(opFill, my_variant, {$IFDEF FPC}@{$ENDIF}list_fill_variant);

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

  //-- system.functions ------------------------------------------------------

  AddFunc('Halt', ['_Value'], [my_variant],
          {$IFDEF FPC}@{$ENDIF}pp_system_halt);
  AddFunc('Exit', ['_Value'], [my_variant],
          {$IFDEF FPC}@{$ENDIF}pp_system_exit);
  AddFunc('Break', {$IFDEF FPC}@{$ENDIF}pp_system_break);
  AddFunc('Continue', {$IFDEF FPC}@{$ENDIF}pp_system_continue);
  AddFunc('Compile', my_func, ['Code', '_Args'], [my_string, my_string],
          {$IFDEF FPC}@{$ENDIF}pp_system_compile);
  AddFunc('Eval', my_variant, ['Expression'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_system_eval);
  AddFunc('Find', my_variant, ['Name'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_system_find);
  AddFunc('TypeOf', my_type, ['Any'], [my_variant],
          {$IFDEF FPC}@{$ENDIF}pp_system_typeof);
  AddFunc('ModuleOf', my_module, ['Any'], [my_variant],
          {$IFDEF FPC}@{$ENDIF}pp_system_moduleof);
  AddFunc('FileOf', my_string, ['Any'], [my_variant],
          {$IFDEF FPC}@{$ENDIF}pp_system_fileof);
  AddFunc('CollectGarbage',
          {$IFDEF FPC}@{$ENDIF}pp_system_collectGarbage);
  AddFunc('Inc', ['Varb', '_Value'], [my_variant, my_int],
          {$IFDEF FPC}@{$ENDIF}pp_system_inc);
  AddFunc('Dec', ['Varb', '_Value'], [my_variant, my_int],
          {$IFDEF FPC}@{$ENDIF}pp_system_dec);
  AddFunc('Readln', my_string, ['_Varb'], [my_variant],
          {$IFDEF FPC}@{$ENDIF}pp_system_readln);
  AddFunc('Write', ['_Str'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_system_write);
  AddFunc('Writeln', ['_Str'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_system_writeln);
  AddFunc('Length', my_int, ['Any'], [my_variant],
          {$IFDEF FPC}@{$ENDIF}pp_system_length);
  AddFunc('Pass', {$IFDEF FPC}@{$ENDIF}pp_system_pass);
  AddFunc('Decompile', my_string, ['Any'], [my_variant],
          {$IFDEF FPC}@{$ENDIF}pp_system_decompile);
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

