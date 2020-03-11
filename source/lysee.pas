{==============================================================================}
{        UNIT: lysee                                                           }
{ DESCRIPTION: lysee script interpreter                                        }
{   COPYRIGHT: Copyright (c) 2003-2015, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/02/28                                                      }
{    MODIFIED: 2020/03/11                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  {$IFNDEF FPC}Windows, Types,{$ENDIF}
  SysUtils, Classes, DateUtils, Math, SyncObjs, Basic;

const

  { LSE: Lysee Script Engine }

  LSE_NAME       = 'lysee';
  LSE_VERSION    = '1.0.5';
  LSE_FILEEXT    = '.ls';
  LSE_LIBEXT     = {$IFDEF MSWINDOWS}
                   '.dll'
                   {$ELSE}
                   {$IFDEF UNIX}'.so'{$ELSE}'.dynlib'{$ENDIF}
                   {$ENDIF};
  LSE_CONFILE    = 'lysee.conf';
  LSE_SYSTEM     = 'system';
  LSE_MAIN       = 'main';
  LSE_CREATE     = 'Create';
  LSE_GETIV      = 'Get[]';
  LSE_SETIV      = 'Set[]';

type

  TLysee         = class; {forward}
  TLyThread      = class;
  TLyError       = class;
  TLyType        = class;
  TLyEnumType    = class;
  TLyEnumSetType = class;
  TLyValue       = class;
  TLyParam       = class;
  TLyToken       = class;
  TLyParser      = class;
  TLySTMT        = class;
  TLySTMTList    = class;
  TLyVarb        = class;
  TLyFunc        = class;
  TLyFuncList    = class;
  TLyModule      = class;
  TLyModuleList  = class;
  TLyList        = class;
  TLyGenerate    = class;

  { TLysee }

  TLyExecute = procedure(Sender: TObject; AThread: TLyThread) of object;
  TLyReadln = procedure(Sender: TObject; var S: string) of object;
  TLyWrite = procedure(Sender: TObject; const S: string) of object;
  TLyResolve = function(const ID: string; Value: TLyValue): boolean of object;
  TLyGetModule = function(Sender: TObject; const Module: string): string of object;

  TLysee = class(TComponent)
  private
    FThreads: TThreadList;
    FModules: TLyModuleList;
    FPublics: TList;
    FNameSeed: cardinal;
    FRollbacks: TList;
    FMainThread: TLyThread;
    FWriteCount: int64;
    FLastWritenChar: char;
    FWriteStream: TStringStream;
    FHostObject: TObject;
    FOnExecuting: TLyExecute;
    FOnExecuted: TLyExecute;
    FOnReadln: TLyReadln;
    FOnWrite: TLyWrite;
    FOnError: TLyWrite;
    FOnResolve: TLyResolve;
    FOnGetModuleFile: TLyGetModule;
    function GetThreadCount: integer;
    function GetRunningThreadCount: integer;
    function GetThread(Index: integer): TLyThread;
  protected
    procedure MarkSurvived;
    procedure MarkMoreForSurvive;virtual;
    procedure Rollback;
    procedure RollbackAdd(AObj: TObject);
    procedure RollbackRemove(AObj: TObject);
    function Resolve(const ID: string; Value: TLyValue): boolean;virtual;
    procedure Error(const Msg: string);virtual;
    procedure CheckNotCompiling;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Clear(WillDestroy: boolean = false);
    { execute }
    procedure Terminate;
    function Execute(const Code: string; const FileURL: string = ''): boolean;
    function ExecuteFile(const FileName: string; const Args: array of string): boolean;
    function ExecuteFileFrom(StartParamIndex: integer = 1): boolean;
    { thread }
    function MainThread: TLyThread;
    function NewThread(const AName: string = ''): TLyThread;
    function ForceThread(const AName: string): TLyThread;
    function FindThread(const AName: string): TLyThread;
    { module }
    function MainModule: TLyModule;
    function FindModuleByFileName(const FileName: string): TLyModule;
    function FindModuleByName(const ModuleName: string): TLyModule;
    function LocateModuleFile(const ModuleName: string): string;
    { stdio }
    procedure Readln(var Text: string);virtual;
    procedure Write(const Text: string);overload;
    procedure Write(const Text: string; const Args: array of const);overload;
    procedure Writeln;overload;
    procedure Writeln(const Text: string);overload;
    procedure Writeln(const Text: string; const Args: array of const);overload;
    { property }
    property ThreadCount: integer read GetThreadCount;
    property RunningThreadCount: integer read GetRunningThreadCount;
    property Threads[Index: integer]: TLyThread read GetThread;
    property Modules: TLyModuleList read FModules;
    property WriteCount: int64 read FWriteCount;
    property LastWritenChar: char read FLastWritenChar;
    property WriteStream: TStringStream read FWriteStream write FWriteStream;
    property HostObject: TObject read FHostObject write FHostObject;
    property OnExecuting: TLyExecute read FOnExecuting write FOnExecuting;
    property OnExecuted: TLyExecute read FOnExecuted write FOnExecuted;
    property OnReadln: TLyReadln read FOnReadln write FOnReadln;
    property OnWrite: TLyWrite read FOnWrite write FOnWrite;
    property OnError: TLyWrite read FOnError write FOnError;
    property OnResolve: TLyResolve read FOnResolve write FOnResolve;
    property OnGetModuleFile: TLyGetModule read FOnGetModuleFile write FOnGetModuleFile;
  end;

  { TLyThread }

  TLyThreadState = (tsTerminated, tsReady, tsRunning, tsContinue, tsBreak, tsExit);

  TLyThread = class
  private
    FEngine: TLysee;
    FName: string;
    FArgs: TLyList;
    FMainModule: TLyModule;
    FMainFunc: TLyFunc;
    FMainLocals: TLyList;
    FMainParam: TLyParam;
    FError: TLyError;
    FCurrent: TLyParam;
    FState: TLyThreadState;
    FTerminated: boolean;
    FExcepted: boolean;
    FHalted: boolean;
    FResult: TLyValue;
    procedure SetResult(Value: TLyValue);
    function GetStatusOK: boolean;
    function GetTerminated: boolean;
    function GetReady: boolean;
    function GetRunning: boolean;
    function GetTerminating: boolean;
    function GetMainModule: TLyModule;
    function GetMainFunc: TLyFunc;
    function GetCodeFunc: TLyFunc;
    function GetCodeModule: TLyModule;
  protected
    procedure ExecuteMainFunc;
    procedure BeginExecute;
    procedure EndExecute;
    procedure ForWhileRepeatEnded;
    function Compile(const Code, FileName: string): TLyFunc;
    function Append(const Code: string; AModule: TLyModule): TLyFunc;
    function LoadModule(const ID, FileName: string; Quiet: boolean): TLyModule;
    function Resolve(const ID: string; Value: TLyValue): boolean;virtual;
    procedure CheckNotRunning;
    procedure CheckNotCompiling;
    procedure CheckStatusOK;
    procedure MarkSurvived;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    procedure Terminate;
    function SyntaxCheck(const Code: string; const FileURL: string = ''): boolean;
    function Execute(const Code: string; const FileURL: string = ''): boolean;
    function ExecuteFile(const FileName: string; const Args: array of string): boolean;
    function ExecuteFileFrom(StartParamIndex: integer = 1): boolean;
    function LoadModuleFromFile(const FileName: string): TLyModule;
    function LoadModuleFromCode(const ID, Code: string): TLyModule;
    property Engine: TLysee read FEngine;
    property Name: string read FName;
    property Args: TLyList read FArgs;
    property Ready: boolean read GetReady;
    property Running: boolean read GetRunning;
    property Terminating: boolean read GetTerminating;
    property Terminated: boolean read GetTerminated;
    property Excepted: boolean read FExcepted write FExcepted;
    property StatusOK: boolean read GetStatusOK;
    property Halted: boolean read FHalted;
    property Error: TLyError read FError;
    property Result: TLyValue read FResult write SetResult;
    property Current: TLyParam read FCurrent;
    property MainParam: TLyParam read FMainParam;
    property MainModule: TLyModule read GetMainModule;
    property MainFunc: TLyFunc read GetMainFunc;
    property CodeModule: TLyModule read GetCodeModule;
    property CodeFunc: TLyFunc read GetCodeFunc;
  end;

  { TLyError }

  TLyError = class
  private
    FThread: TLyThread;
    FErrID: string;
    FMsg: string;
    FModule: string;
    FFileName: string;
    FRow: integer;
    FCol: integer;
  protected
    procedure Syntax(const Msg, Module, FileName: string; Row, Col: integer);
    procedure Runtime(const Msg, Module, FileName: string; Row, Col: integer);
  public
    constructor Create(AThread: TLyThread);
    procedure Clear;
    function ToString: string;override;
    function AsText(Simple: boolean = true): string;
    property Errno: string read FErrID;
    property Module: string read FModule;
    property FileName: string read FFileName;
    property Row: integer read FRow;
    property Col: integer read FCol;
    property Msg: string read FMsg;
  end;

  { TLyType }

  TLyProc = procedure(const Param: TLyParam);
  TLyObjectProc = procedure(const Param: TLyParam) of object;

  TLyTypeStyle = (tsVariant, tsNil, tsBasic, tsEnum, tsEnumSet, tsObject);
  TLyTypeClass = class of TLyType;

  TLyType = class
  private
    FModule: TLyModule;
    FName: string;
    FParent: TLyType;
    FStyle: TLyTypeStyle;
    FMainType: TLyType;
    FMethods: TLyFuncList;
    FCreater: TLyFunc;
    FGetFunc: TLyFunc;
    FSetFunc: TLyFunc;
    function GetFullName: string;
    procedure NewPropProc(const Param: TLyParam);
    procedure SetParent(Value: TLyType);
  protected
    function InstanceClass: TClass;virtual;
    function CreateInstance: pointer;virtual;
    function IncRefcount(Obj: pointer): integer;virtual;
    function DecRefcount(Obj: pointer): integer;virtual;
    function Compare(LValue, RValue: TLyValue): TLyCompare;virtual;
    procedure Validate(Obj: pointer);virtual;
    function Generate(Obj: pointer): TLyGenerate;virtual;
    procedure Setup;virtual;
    procedure MyCreate(const Param: TLyParam);virtual;
    { list }
    procedure MarkForSurvive(Obj: pointer);virtual;
    function GetLength(Obj: pointer): int64;virtual;
    function Clear(Obj: pointer): boolean;virtual;
    function Add(Obj: pointer; Value: TLyValue): integer;virtual;
    function Has(Obj: pointer; Value: TLyValue): boolean;virtual;
    { field: set/set instance field }
    function Get(Obj: pointer; const AName: string; Value: TLyValue): boolean;virtual;
    function Put(Obj: pointer; const AName: string; Value: TLyValue): boolean;virtual;
    { value }
    function AsString(Obj: pointer): string;virtual;
    function AsChar(Obj: pointer): char;virtual;
    function AsInteger(Obj: pointer): int64;virtual;
    function AsFloat(Obj: pointer): double;virtual;
    function AsCurrency(Obj: pointer): currency;virtual;
    function AsTime(Obj: pointer): TDateTime;virtual;
    function AsBoolean(Obj: pointer): boolean;virtual;
    { operate }
    procedure ExecNeg(Value: TLyValue);virtual;
    procedure ExecNot(Value: TLyValue);virtual;
    procedure ExecLike(LValue, RValue: TLyValue);virtual;
    procedure ExecIn(LValue, RValue: TLyValue);virtual;
    procedure ExecShr(LValue, RValue: TLyValue);virtual;
    procedure ExecShl(LValue, RValue: TLyValue);virtual;
    procedure ExecXor(LValue, RValue: TLyValue);virtual;
    procedure ExecMod(LValue, RValue: TLyValue);virtual;
    procedure ExecDivf(LValue, RValue: TLyValue);virtual;
    procedure ExecDiv(LValue, RValue: TLyValue);virtual;
    procedure ExecMul(LValue, RValue: TLyValue);virtual;
    procedure ExecDec(LValue, RValue: TLyValue);virtual;
    procedure ExecAdd(LValue, RValue: TLyValue);virtual;
  public
    constructor Create(const AName: string; AModule: TLyModule);virtual;
    destructor Destroy;override;
    function InheriteClassType: TLyTypeClass;virtual;
    function Inherite(const AName: string; AModule: TLyModule): TLyType;virtual;
    function FindMethod(const AName: string; AObj: pointer): TLyFunc;virtual;
    function DefValue: pointer;virtual;
    procedure SetDefValue(Value: TLyValue);virtual;
    procedure Convert(Value: TLyValue);virtual;
    function IsTypeOf(AType: TLyType): boolean;
    function IsChildOf(AType: TLyType): boolean;
    function IsEnumType: boolean;
    function IsEnumSetType: boolean;
    function IsFinalType: boolean;
    function Prototype(const AName: string): string;
    { define method }
    function Method(const AName: string;
      const AType: TLyType;
      const ParamNames: array of string;
      const ParamTypes: array of TLyType;
      const AProc: TLyObjectProc): TLyFunc;overload;
    function Method(const AName: string;
      const ParamNames: array of string;
      const ParamTypes: array of TLyType;
      const AProc: TLyObjectProc): TLyFunc;overload;
    function Method(const AName: string;
      const AType: TLyType;
      const AProc: TLyObjectProc): TLyFunc;overload;
    function Method(const AName: string;
      const AProc: TLyObjectProc): TLyFunc;overload;
    { define property }
    function Define(const AProp: string; const AType: TLyType;
      const GetProc: TLyObjectProc;
      const SetProc: TLyObjectProc = nil): boolean;overload;
    function Define(const AType: TLyType;
      const IndexName: string;
      const IndexType: TLyType;
      const GetProc: TLyObjectProc;
      const SetProc: TLyObjectProc = nil): boolean;overload;
    function Define(const AType: TLyType;
      const IndexNames: array of string;
      const IndexTypes: array of TLyType;
      const GetProc: TLyObjectProc;
      const SetProc: TLyObjectProc = nil): boolean;overload;
    { define branch }
    function Branch(const PropName: string): TLyType;
    function MatchBranch(ABranch: TLyType): boolean;
    { properties }
    property Module: TLyModule read FModule;
    property Name: string read FName;
    property Style: TLyTypeStyle read FStyle;
    property FullName: string read GetFullName;
    property MainType: TLyType read FMainType;
    property Creater: TLyFunc read FCreater write FCreater;  ////////
    property GetFunc: TLyFunc read FGetFunc write FGetFunc;
    property SetFunc: TLyFunc read FSetFunc write FSetFunc;
    property Methods: TLyFuncList read FMethods;
    property Parent: TLyType read FParent write SetParent;
  end;

  { TLyAgentType }

  TLyAgentType = class(TLyType)
  protected
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
  end;

  { TLyBasicType }

  TLyBasicType = class(TLyType)
  public
    constructor Create(const AName: string; AModule: TLyModule);override;
  end;

  { TLyVariantType }

  TLyVariantType = class(TLyType)
  public
    constructor Create(const AName: string; AModule: TLyModule);override;
    procedure SetDefValue(Value: TLyValue);override;
    procedure Convert(Value: TLyValue);override;
  end;

  { TLyNilType }

  TLyNilType = class(TLyType)
  protected
    function AsString(Obj: pointer): string;override;
    function AsChar(Obj: pointer): char;override;
    function AsInteger(Obj: pointer): int64;override;
    function AsFloat(Obj: pointer): double;override;
    function AsCurrency(Obj: pointer): currency;override;
    function AsTime(Obj: pointer): TDateTime;override;
    function AsBoolean(Obj: pointer): boolean;override;
  public
    constructor Create(const AName: string; AModule: TLyModule);override;
    procedure SetDefValue(Value: TLyValue);override;
    procedure Convert(Value: TLyValue);override;
  end;

  { TLyObjectType }

  TLyObjectType = class(TLyType)
  protected
    function InstanceClass: TClass;override;
    function CreateInstance: pointer;override;
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function AsString(Obj: pointer): string;override;
    procedure MarkForSurvive(Obj: pointer);override;
    function Get(Obj: pointer; const AName: string; Value: TLyValue): boolean;override;
    function Put(Obj: pointer; const AName: string; Value: TLyValue): boolean;override;
    function MatchObject(T: TLyType): boolean;
    procedure Setup;override;
  public
    function FindMethod(const AName: string; AObj: pointer): TLyFunc;override;
  end;

  { TLyEnumItem }

  TLyEnumItem = class
  private
    FParent: TLyEnumType;
    FName: string;
    FValue: integer;
  public
    property Parent: TLyEnumType read FParent;
    property Name: string read FName;
    property Value: integer read FValue;
  end;

  { TLyEnumType }

  TLyEnumType = class(TLyType)
  private
    FItems: array of TLyEnumItem;
    function GetCount: integer;
    function GetItem(Index: integer): TLyEnumItem;
  protected
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function Compare(LValue, RValue:TLyValue): TLyCompare;override;
    { value }
    function AsString(Obj: pointer): string;override;
    function AsInteger(Obj: pointer): int64;override;
  public
    constructor Create(const AName: string; AModule: TLyModule);override;
    destructor Destroy;override;
    function DefValue: pointer;override;
    procedure Convert(Value: TLyValue);override;
    procedure AddItems(const Names: array of string);
    function Find(const AName: string): TLyEnumItem;overload;
    function Find(OrdValue: integer): TLyEnumItem;overload;
    function FindByName(const AName: string): TLyEnumItem;
    function FindByValue(OrdValue: integer): TLyEnumItem;
    procedure SetValue(Value: TLyValue; Item: TLyEnumItem);overload;
    procedure SetValue(Value: TLyValue; const AName: string);overload;
    procedure SetValue(Value: TLyValue; OrdValue: integer);overload;
    function NewEnumSetType(const AName: string): TLyEnumSetType;
    property Count: integer read GetCount;
    property Items[Index: integer]: TLyEnumItem read GetItem;default;
  end;

  { TLyEnumSet }

  TLyEnumSet = class(TLyObject)
  private
    FParent: TLyEnumSetType;
    FSets: array of boolean;
    function GetSource: TLyEnumType;
    function GetCount: integer;
    function Get(Index: integer): boolean;
    procedure Put(Index: integer; Value: boolean);
  protected
    function Equal(S: TLyEnumSet): boolean;
    function Add(S: TLyEnumSet): TLyEnumSet;
    function Dec(S: TLyEnumSet): TLyEnumSet;
    function Mul(S: TLyEnumSet): TLyEnumSet;
    function NotAll: TLyEnumSet;
  public
    destructor Destroy;override;
    function ToString: string;override;
    procedure Clear;
    procedure Assign(A: TLyList);
    function AsBoolean: boolean;
    function AsArray: TLyList;
    function IsSet(Item: TLyEnumItem): boolean;overload;
    function IsSet(Index: integer): boolean;overload;
    property Parent: TLyEnumSetType read FParent;
    property Source: TLyEnumType read GetSource;
    property Count: integer read GetCount;
    property Sets[Index: integer]: boolean read Get write Put;default;
  end;

  { TLyEnumSetType }

  TLyEnumSetType = class(TLyType)
  private
    FSource: TLyEnumType;
    FDefValue: TLyEnumSet;
  protected
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function Compare(LValue, RValue:TLyValue): TLyCompare;override;
    { value }
    function AsString(Obj: pointer): string;override;
    function AsBoolean(Obj: pointer): boolean;override;
    { operate }
    procedure ExecNot(Value: TLyValue);override;
    procedure ExecMul(LValue, RValue: TLyValue);override;
    procedure ExecDec(LValue, RValue: TLyValue);override;
    procedure ExecAdd(LValue, RValue: TLyValue);override;
  public
    constructor Create(const AName: string; AModule: TLyModule);override;
    destructor Destroy;override;
    function DefValue: pointer;override;
    procedure Convert(Value: TLyValue);override;
    procedure SetValue(Value: TLyValue; ASet: TLyEnumSet);
    function NewEnumSet: TLyEnumSet;
    function Has(Obj: pointer; Value: TLyValue): boolean;override;
    property Source: TLyEnumType read FSource;
  end;

  { TLyValue }

  TLyFind = (fiNone, fiVarb, fiFunc, fiType, fiModule, fiEnum, fiValue);
  TLyFinds = set of TLyFind;

  RLyFind = packed record
    case f_find: TLyFind of
    fiNone  :(VNone: pointer);
    fiVarb  :(VVarb: TLyVarb);
    fiFunc  :(VFunc: TLyFunc);
    fiType  :(VType: TLyType);
    fiModule:(VModule: TLyModule);
    fiEnum  :(VEnum: TLyEnumItem);
    fiValue :(VValue: TLyValue);
  end;
  PLyFind = ^RLyFind;

  TLyGetAt = (gaNone, gaMethod, gaData);

  TLyValue = class
  private
    FType: TLyType;
    FData: pointer;
    function GetAsInteger: int64;
    procedure SetAsInteger(Value: int64);
    function GetAsChar: char;
    procedure SetAsChar(Value: char);
    function GetAsBoolean: boolean;
    procedure SetAsBoolean(Value: boolean);
    function GetAsFloat: double;
    procedure SetAsFloat(Value: double);
    function GetAsCurrency: currency;
    procedure SetAsCurrency(Value: currency);
    function GetAsTime: TDateTime;
    procedure SetAsTime(Value: TDateTime);
    function GetAsType: TLyType;
    procedure SetAsType(Value: TLyType);
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetAsModule: TLyModule;
    procedure SetAsModule(Value: TLyModule);
    function GetAsEnum: TLyEnumItem;
    procedure SetAsEnum(Value: TLyEnumItem);
    function GetAsEnumSet: TLyEnumSet;
    procedure SetAsEnumSet(Value: TLyEnumSet);
    function GetAsFileName: string;
  public
    procedure ExecNeg;
    procedure ExecNot;
    procedure ExecIs(Value: TLyValue);
    procedure ExecAs(Value: TLyValue);
    procedure ExecLike(Value: TLyValue);
    procedure ExecIn(Value: TLyValue);
    procedure ExecShr(Value: TLyValue);
    procedure ExecShl(Value: TLyValue);
    procedure ExecXor(Value: TLyValue);
    procedure ExecMod(Value: TLyValue);
    procedure ExecDivf(Value: TLyValue);
    procedure ExecDiv(Value: TLyValue);
    procedure ExecMul(Value: TLyValue);
    procedure ExecDec(Value: TLyValue);
    procedure ExecAdd(Value: TLyValue);
    procedure ExecCompare(Value: TLyValue; Wanted: TLyCompares);
    procedure ExecSame(Value: TLyValue);
  public
    constructor Create;virtual;
    destructor Destroy;override;
    procedure MarkForSurvive;
    procedure Clear;
    procedure Assign(Value: TLyValue);overload;
    procedure Assign(AType: TLyType; AData: pointer);overload;
    procedure Assign(Finded: PLyFind);overload;
    procedure Convert(AType: TLyType);
    function Compare(Value: TLyValue): TLyCompare;overload;
    function Compare(Value: TLyValue; Wanted: TLyCompares): boolean;overload;
    function Same(Value: TLyValue): boolean;
    function NewArray: TLyList;
    function GetData(AType: TLyType): pointer;
    function GetFunc: TLyFunc;
    function GetModule: TLyModule;
    function GetEnumSet(T: TLyEnumSetType): TLyEnumSet;
    function GetEnum(T: TLyEnumType): TLyEnumItem;
    function GetSelf(var AObj): boolean;
    function GetHost(var AObj): boolean;
    function GetAt(const Prop: string; Outv: TLyValue): TLyGetAt;
    function IsNil: boolean;
    property VType: TLyType read FType;
    property Data: pointer read FData;
    property AsString: string read GetAsString write SetAsString;
    property AsFileName: string read GetAsFileName;
    property AsChar: char read GetAsChar write SetAsChar;
    property AsInteger: int64 read GetAsInteger write SetAsInteger;
    property AsBoolean: boolean read GetAsBoolean write SetAsBoolean;
    property AsFloat: double read GetAsFloat write SetAsFloat;
    property AsCurrency: currency read GetAsCurrency write SetAsCurrency;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsModule: TLyModule read GetAsModule write SetAsModule;
    property AsType: TLyType read GetAsType write SetAsType;
    property AsEnum: TLyEnumItem read GetAsEnum write SetAsEnum;
    property AsEnumSet: TLyEnumSet read GetAsEnumSet write SetAsEnumSet;
  end;

  { TLyParam }

  TLyParam = class
  private
    FThread: TLyThread;
    FFunc: TLyFunc;
    FToken: TLyToken;
    FParams: TLyList;
    FPrmc: integer; {<--actual parameters' count passed to function}
    FResult: TLyValue;
    FPrev: TLyParam;
    function GetCount: integer;
    function GetItem(Index: integer): TLyValue;
    function GetValue(const Name: string): TLyValue;
    procedure SetResult(Value: TLyValue);
    procedure SetArgs(Args: TLyList);
    function GetThis: pointer;
  public
    procedure Error(const Msg: string);overload;
    procedure Error(const Msg: string; const Args: array of const);overload;
    procedure ErrorChangeFunc(AFunc: TLyFunc);
    procedure ErrorIndexType(AType: TLyType);
    procedure BeginExec(var Mark: integer; var Tmpv: TLyValue);overload;
    procedure BeginExec(var Mark: integer);overload;
    procedure EndExec(Mark: integer);
    function GetVarbValue(Index: integer; var VT: TLyType): TLyValue;
    function GetSelf(var Aobj): boolean;
    function GetHost(var AObj): boolean;
    function ExecFunc(Func: TLyFunc; Outv: TLyValue; Args: TLyList; var ErrStr: string): boolean;
    property Thread: TLyThread read FThread;
    property Func: TLyFunc read FFunc;
    property Params: TLyList read FParams;
    property Result: TLyValue read FResult write SetResult;
    property Token: TLyToken read FToken;
    property Prmc: integer read FPrmc;
    property Count: integer read GetCount;
    property Items[Index: integer]: TLyValue read GetItem;default;
    property Values[const Name: string]: TLyValue read GetValue;
    property This: pointer read GetThis;
  end;

  { TLyToken }

  TLySymbol = (syError,
    { keyword }
    syBegin, syUses, syConst, syDef, syClass, sySelf, syObject, syInherited, syArray, syVar, syGet,
    sySet, syIf, syElif, syThen, syElse, syCase, syOf, syFor, syWhile, syRepeat,
    syUntil, syDo, syResult, syDiv, syMod, syTo, syDownto, syNot, syIn, syIs,
    syAs, syLike, syAnd, syOr, syXor, syShr, syShl, syTry, syExcept, syFinally,
    syRaise, syTrue, syFalse, syNil, syEnd,
    { operator }
    syBecome, syAdd, syReduce, syMul, syDivf, syLParen, syRParen, syLArray,
    syRArray, syDot, syRange, syColon, sySemic, syComma, syAt, sySame, syEQ,
    syNE, syLT, syLE, syMT, syME, syVert,
    { abstract }
    syID, syNeg, syFloat, syInt, syStr, syChar, syHash, syCall, syEOF
  );
  TLySymbols = set of TLySymbol;

  RLyToken = packed record
    case TLySymbol of
    syChar :(VChar: char);
    syInt  :(VInt: int64);
    syFloat:(VFloat: double);
    syVert :(VFunc: TLyFunc);
  end;

  TLyTokenProc = procedure(Token: TLyToken; Param: TLyParam; Outv: TLyValue);

  TLyToken = class
  private
    FSym: TLySymbol;   // syTrue, syFalse
    FRow: integer;
    FCol: integer;
    FName: string;     // syBecome, syID, syStr
    FValue: RLyToken;  // syChar, syInt, syFloat
    FRight: TLyToken;
    FLeft: TLyToken;
    FParams: TList;
    function GetParamCount: integer;
    function GetParam(Index: integer): TLyToken;
  protected
    function SetupParamList(Param: TLyParam; Host: TLyValue): TLyList;
    function GetAt(Param: TLyParam; Host, Outv: TLyValue): TLyGetAt;
    function GetProp(Param: TLyParam; Host, Outv: TLyValue): boolean;
    function TryFunc(Func: TLyFunc; Param: TLyParam; Outv: TLyValue): boolean;
    function TryMethod(Host: TLyValue; Method: TLyFunc;
      Param: TLyParam; Outv: TLyValue; LastParam: TLyToken): boolean;
  public
    constructor Create(T: TLyToken = nil);
    constructor CreateWithLeft(LeftBranch: TLyToken; T: TLyToken = nil);
    destructor Destroy;override;
    procedure Assign(Source: TLyToken);
    procedure Read(Source: TLyToken);
    procedure Clear;
    procedure ClearParams;
    procedure AddParam(AParam: TLyToken);
    procedure Error(Param: TLyParam; const Msg: string);overload;
    procedure Error(Param: TLyParam; const Msg: string; const Args: array of const);overload;
    procedure FailGet(Param: TLyParam; const ID: string);overload;
    procedure FailGet(Param: TLyParam; const Host, Prop: string);overload;
    function ExecFunc(Func: TLyFunc; Param: TLyParam; Outv: TLyValue; Args: TLyList): boolean;
    function Execute(Param: TLyParam; Outv: TLyValue): boolean;
    function Decompile(Level: integer = 0): string;
    property ParamCount: integer read GetParamCount;
    property Params[Index: integer]: TLyToken read GetParam;
    property Left: TLyToken read FLeft write FLeft;
    property Right: TLyToken read FRight write FRight;
  end;

  { TLyTokenList }

  TLyTokenList = class(TLyObject)
  private
    FItems: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TLyToken;
    function GetLast: TLyToken;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Reverse;
    procedure Clear;
    procedure ClearKeepLast;
    procedure DeleteLast(N: integer);
    function Add(Pos: TLyToken = nil): TLyToken;
    function AddToken(Token: TLyToken): TLyToken;overload;
    function AddToken(Sym: TLySymbol; Pos: TLyToken): TLyToken;overload;
    function AddToken(Sym: TLySymbol): TLyToken;overload;
    property Count: integer read GetCount;
    property Items[Index: integer]: TLyToken read GetItem;default;
    property Last: TLyToken read GetLast;
  end;

  { TLyTokenizer }

  TLyTokenizer = class(TLyObject)
  private
    FTokens: TLyTokenList;
    FIndex: integer;
    FCurrent: TLyToken;
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
    function ParseChar(var C: char): boolean;
    function ParseString(var S: string): boolean;
    function GetToken(token: TLyToken): boolean;
    function GetCurrent: TLyToken;
  protected
    function ParseHex(var I: int64): boolean;
  public
    constructor Create(const Script: string);
    destructor Destroy;override;
    function PackToCurrent: boolean;
    function GetNext: TLyToken;
    function PeekNextSymbol: TLySymbol;
    function PeekThirdSymbol: TLySymbol;
    function NextIsBecome(OnHead: boolean): boolean;
    property Current: TLyToken read GetCurrent;
    property Row: integer read FRow write FRow;
    property Col: integer read FCol write FCol;
    property Position: integer read FPosition;
    property Code: string read FCode;
  end;

  { TLyParser }

  TLyParser = class(TLyObject)
  private
    FTokenizer: TLyTokenizer; {<--token analyzer}
    FLast: TLyToken;          {<--last token}
    FFunc: TLyFunc;           {<--current function}
    FModule: TLyModule;       {<--current module}
    FClass: TLyType;
    FError: TLyError;
    FAfter: integer;
    function UseToken(Token: TLyToken): TLyToken;
  protected
    procedure EUnexpected(T: TLyToken = nil);
    procedure ERedeclared(T: TLyToken = nil);
    procedure ETypeNotFound(T: TLyToken = nil);
    procedure ESyntax(ERow, ECol: integer; const EMsg: string; const EArgs: array of const);
    { parsing }
    procedure ParseUsesConstFunc;
    procedure ParseUses;
    procedure ParseConst;
    procedure ParseDef;
    procedure ParseClass;
    procedure ParseBlock(EndSyms: TLySymbols; SX: TLySTMTList);
    procedure ParseStatement(OnHead: boolean; SX: TLySTMTList);
    procedure ParseArguments(EndSym: TLySymbol);//@
    procedure ParseType(OnHead: boolean; var T: TLyType);
    { statement }
    procedure ParseVar(SX: TLySTMTList);//@
    procedure ParseIf(SX: TLySTMTList);//@
    procedure ParseFor(SX: TLySTMTList);//@
    procedure ParseWhile(SX: TLySTMTList);//@
    procedure ParseRepeat(SX: TLySTMTList);//@
    procedure ParseCase(SX: TLySTMTList);//@
    procedure ParseTry(SX: TLySTMTList);
    procedure ParseRaise(SX: TLySTMTList);
    procedure ParsePuts(SX: TLySTMTList);//@
    procedure ParseAny(SX: TLySTMTList);//@
    { expression }
    function ParseExpr(OnHead: boolean; EndSyms: TLySymbols; DoCheck: boolean): TLyToken;
    function ParseFact(Level: integer): TLyToken;
    function ParseTerm: TLyToken;
    { token }
    procedure SymGetNext;
    procedure SymGotoNext;
    procedure SymTestNext(Syms: TLySymbols);
    procedure SymTestLast(Syms: TLySymbols);
    procedure SymTestLastID;
    procedure SymTestNextID;
    procedure SymTestLastAfter;
    function SymPeekNext: TLySymbol;
  public
    constructor Create(AModule: TLyModule; AError: TLyError);
    destructor Destroy;override;
    function Parse(const Code: string; UsingModule: boolean = false): TLyFunc;
    function ParseAndFree(const Code: string; UsingModule: boolean = false): TLyFunc;
  end;

  { TLySTMT }

  TLySTMTStyle = (ssNormal, ssConst, ssAssign, ssResult, ssPuts, ssIf, ssWhile,
                  ssRepeat, ssFor, ssCase, ssTry, ssRaise);

  TLySTMT = class
  private
    FStyle: TLySTMTStyle;
    FParent: TLySTMTList;
    FExpr: TLyToken;
    FItems: TLySTMTList;
    function GetItems: TLySTMTList;
    function GetCount: integer;
  protected
    procedure ExecNormal(Param: TLyParam);
    procedure ExecPuts(Param: TLyParam);
    procedure ExecWhile(Param: TLyParam);
    procedure ExecRepeat(Param: TLyParam);
    procedure ExecRaise(Param: TLyParam);
  public
    constructor Create(AParent: TLySTMTList);virtual;
    destructor Destroy;override;
    function Execute(Param: TLyParam): boolean;virtual;
    procedure Decompile(Level: integer; Lines: TStrings);virtual;
    property Style: TLySTMTStyle read FStyle;
    property Parent: TLySTMTList read FParent;
    property Items: TLySTMTList read GetItems;
    property Count: integer read GetCount;
    property Expr: TLyToken read FExpr;
  end;

  { TLyAssign }

  TLyAssign = class(TLySTMT)
  private
    FVarb: string;
    function GetVarbID: string;
    procedure SetVarb(Param: TLyParam; const Varb: string; Value: TLyValue);
    procedure SetResult(Param: TLyParam; Value: TLyValue);
    procedure MultiAssign(Param: TLyParam);
  protected
    procedure ExecConst(Param: TLyParam);
    procedure ExecAssign(Param: TLyParam);
    procedure ExecResult(Param: TLyParam);
  public
    constructor Create(AParent: TLySTMTList);override;
    function Execute(Param: TLyParam): boolean;override;
    procedure Decompile(Level: integer; Lines: TStrings);override;
    property Varb: string read FVarb;
    property VarbID: string read GetVarbID;
  end;

  { TLyFor }

  TLyFor = class(TLyAssign)
  private
    FUpTo: boolean;
    FEndValue: TLyToken;
  public
    constructor Create(AParent: TLySTMTList);override;
    destructor Destroy;override;
    function Execute(Param: TLyParam): boolean;override;
    procedure Decompile(Level: integer; Lines: TStrings);override;
    property EndValue: TLyToken read FEndValue;
    property UpTo: boolean read FUpTo;
  end;

  { TLyIf }

  TLyIf = class(TLySTMT)
  private
    FElseItems: TLySTMTList;
    function GetElseItems: TLySTMTList;
    function GetElseCount: integer;
  public
    constructor Create(AParent: TLySTMTList);override;
    destructor Destroy;override;
    function Execute(Param: TLyParam): boolean;override;
    procedure Decompile(Level: integer; Lines: TStrings);override;
    function IsElif: boolean;
    property ElseCount: integer read GetElseCount;
    property ElseItems: TLySTMTList read GetElseItems;
  end;

  { TLyCase }

  TLyCase = class(TLyIf)
  public
    constructor Create(AParent: TLySTMTList);override;
    function Execute(Param: TLyParam): boolean;override;
    procedure Decompile(Level: integer; Lines: TStrings);override;
  end;

  { TLyTry }

  TLyTry = class(TLyIf)
  private
    FTryFinally: boolean;
  public
    constructor Create(AParent: TLySTMTList);override;
    function Execute(Param: TLyParam): boolean;override;
    procedure Decompile(Level: integer; Lines: TStrings);override;
    property TryFinally: boolean read FTryFinally;
  end;

  { TLySTMTList }

  TLySTMTList = class
  private
    FItems: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TLySTMT;
  public
    destructor Destroy;override;
    procedure Clear;
    procedure Delete(Index: integer);
    function Add(Style: TLySTMTStyle): TLySTMT;overload;
    function Add(STMT: TLySTMT): integer;overload;
    function Execute(Param: TLyParam): boolean;
    procedure Decompile(Level: integer; Lines: TStrings);
    property Count: integer read GetCount;
    property Items[Index: integer]: TLySTMT read GetItem;default;
  end;

  { TLyVarb }

  TLyVarb = class
  private
    FName: string;
    FType: TLyType;
  public
    constructor Create(const AName: string; AType: TLyType);
    destructor Destroy;override;
    function Prototype: string;
    property Name: string read FName write FName;
    property ValueType: TLyType read FType write FType;
  end;

  { TLyVarbList}

  TLyVarbList = class
  private
    FVarbs: array of TLyVarb;
    FLocalCount: integer;
    function GetCount: integer;
    function GetParamCount: integer;
    function GetVarb(Index: integer): TLyVarb;
    function DoAdd(const AName: string; AType: TLyType): TLyVarb;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    procedure Assign(Source: TLyVarbList);
    function Add(const AName: string; AType: TLyType): TLyVarb;
    function AddLocal(const AName: string; AType: TLyType): TLyVarb;
    function IndexOf(const AName: string): integer;overload;
    function IndexOf(const AVarb: TLyVarb): integer;overload;
    function Find(const AName: string): TLyVarb;
    property Count: integer read GetCount;
    property LocalCount: integer read FLocalCount;
    property ParamCount: integer read GetParamCount;
    property Varbs[Index: integer]: TLyVarb read GetVarb;default;
  end;

  { TLyFunc }

  TLyFunc = class(TLyObject)
  private
    FModule: TLyModule;
    FName: string;
    FParent: TLyType;
    FParams: TLyVarbList;
    FResultType: TLyType;
    FSTMTs: TLySTMTList;
    FMethod: TLyObjectProc;
    FProc: TLyProc;
    FMinArgs: integer;
    function GetMinArgs: integer;
    function GetFullName: string;
  public
    constructor Create(const AName: string; M: TLyModule; Proc: TLyProc);
    constructor CreateMethod(const AName: string; P: TLyType; Proc: TLyProc);
    destructor Destroy;override;
    function ToString: string;override;
//  procedure Execute(Param: TLyParam); {<--see TLyToken.ExecFunc}
    function Prototype: string;
    function FindInside(const ID: string; rec: PLyFind = nil): boolean;
    function FindBy(const ID: string; rec: PLyFind; Range: TLyFinds = []): boolean;
    function FindSave(const ID: string; Param: TLyParam; Outv: TLyValue): TLyFind;
    function Context: TLysee;
    function IsMainFunc: boolean;
    function IsMethod: boolean;
    function IsConstructor: boolean;
    function IsProcedure: boolean;
    function IsFunction: boolean;
    function IsConst: boolean;
    procedure Decompile(Level: integer; Lines: TStrings);
    function STMTs: TLySTMTList;
    property Module: TLyModule read FModule;
    property Name: string read FName;
    property FullName: string read GetFullName;
    property Parent: TLyType read FParent;
    property ResultType: TLyType read FResultType write FResultType;
    property MinArgs: integer read GetMinArgs;
    property Params: TLyVarbList read FParams;
  end;

  { TLyFuncList }

  TLyFuncList = class
  private
    FItems: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TLyFunc;
  public
    constructor Create;
    destructor Destroy;override;
    function Find(const Name: string): TLyFunc;
    procedure Clear;
    procedure Delete(Index: integer);
    property Count: integer read GetCount;
    property Items[Index: integer]: TLyFunc read GetItem;default;
  end;

  { TLyModule }

  TLyModule = class(TLyNameObject)
  private
    FEngine: TLysee;           {<--owner context}
    FThread: TLyThread;        {<--owner thread}
    FModules: TLyModuleList;   {<--modules used by this module}
    FImporters: TLyModuleList; {<--modules useing this module}
    FFileName: string;
    FHandle: THandle;
    FTypeList: TList;
    FEnumTypes: TList;
    FFuncList: TLyFuncList;
    FConsts: TLyList;
    FPublic: boolean;
    FOnSetup: TNotifyEvent;
    procedure InitModule;
    procedure DeleteFunctions;
    procedure DeleteTypes;
    procedure SetPublic(Value: boolean);
  protected
    property OnSetup: TNotifyEvent read FOnSetup write FOnSetup;
  public
    function AddFunc(const AName: string; T: TLyType;
      const ParamNames: array of string; const ParamTypes: array of TLyType;
      const Proc: TLyProc): TLyFunc;overload;
    function AddFunc(const AName: string;
      const ParamNames: array of string; const ParamTypes: array of TLyType;
      const Proc: TLyProc): TLyFunc;overload;
    function AddFunc(const AName: string; T: TLyType;
      const Proc: TLyProc): TLyFunc;overload;
    function AddFunc(const AName: string;
      const Proc: TLyProc): TLyFunc;overload;
  public
    constructor Create(const AName: string);virtual;
    constructor CreateEx(const AName: string; AContext: TLysee);
    destructor Destroy;override;
    procedure Setup;virtual;
    function Use(const AModule: string): boolean;overload;
    function Use(M: TLyModule): boolean;overload;
    function EnsureName(const AName: string): string;
    function AddEnumType(const AName: string;
      const ItemNames: array of string): TLyEnumType;
    function IsMainModule: boolean;
    function SetupType(const T: TLyType): boolean;
    function TypeCount: integer;
    function GetType(Index: integer): TLyType;
    function FindModule(const ID: string; FindPossible: boolean): TLyModule;
    function FindType(const ID: string): TLyType;
    function FindTypeBy(const ID, AModule: string): TLyType;
    function FindFunc(const ID: string): TLyFunc;
    function FindFuncBy(const ID: string; FindLibrary: boolean): TLyFunc;
    function Find(const ID: string; rec: PLyFind = nil): boolean;
    function FindBy(const ID, AModule: string; rec: PLyFind): boolean;
    function FindSave(const AName: string; Value: TLyValue): boolean;
    function FindSaveBy(const AName: string; Value: TLyValue): boolean;
    function UseModule(const AName, AFile: string; AThread: TLyThread): TLyModule;
    procedure Define(const AName: string; Value: int64);overload;
    procedure Define(const AName, Value: string);overload;
    property Modules: TLyModuleList read FModules;
    property Importers: TLyModuleList read FImporters;
    property FuncList: TLyFuncList read FFuncList;
    property FileName: string read FFileName write FFileName;
    property Handle: THandle read FHandle write FHandle;
    property IsPublic: boolean read FPublic write SetPublic;
    property Engine: TLysee read FEngine;
    property Consts: TLyList read FConsts;
  end;

  { TLyModuleList }

  TLyModuleList = class(TLyObject)
  private
    FContext: TLysee;
    FImporter: TLyModule;
    FModules: TList;
    function GetModule(Index: integer): TLyModule;
    function GetCount: integer;
  public
    constructor Create(AContext: TLysee);
    destructor Destroy;override;
    procedure Setup;
    function IndexOf(AModule: TLyModule): integer;overload;
    function IndexOf(const Name: string): integer;overload;
    function Has(AModule: TLyModule): boolean;overload;
    function Has(const Name: string): boolean;overload;
    function Find(const Name: string): TLyModule;
    function Add(AModule: TLyModule): integer;
    procedure Delete(Index: integer);
    procedure Clear;
    procedure DeleteFunctions;
    procedure ClearConsts;
    function ToList: TLyList;
    property Count: integer read GetCount;
    property Modules[Index: integer]: TLyModule read GetModule;default;
  end;

  { TLyGarbage }

  TLyGarbage = class(TLyObject)
  private
    FPrev: TLyGarbage;
    FNext: TLyGarbage;
    FInChain: boolean;
    FSurvived: boolean;
  public
    constructor Create;virtual;
    destructor Destroy;override;
    procedure MarkForSurvive;virtual;
    procedure Clear;virtual;
    property Survived: boolean read FSurvived write FSurvived;
    property InChain: boolean read FInChain write FInChain;
  end;

  { TLyCollect }

  TLyCollect = class
  private
    FChain: TLyGarbage;
    FDead: TList;
    FContexts: TList;
  protected
    procedure MarkSurvived;virtual;
    procedure GcAdd(G: TLyGarbage);
    procedure GcRemove(G: TLyGarbage);
  public
    constructor Create;virtual;
    destructor Destroy;override;
    function Collect: integer;virtual;
  end;

  { TLyList }

  TLyList = class(TLyGarbage)
  private
    FItems: TStringList;
    FFormating: boolean;
    FReadonly: boolean;
    function GetCount: integer;
    procedure SetCount(NewCount: integer);
    function GetItem(Index: integer): TLyValue;
    procedure SetItem(Index: integer; const Value: TLyValue);
    function GetName(Index: integer): string;
    procedure SetName(Index: integer; const Value: string);
    function GetValue(const Name: string): TLyValue;
    procedure SetValue(const Name: string; Value: TLyValue);
    function GetFirst: TLyValue;
    function GetLast: TLyValue;
  protected
    procedure ListNames(AList: TLyList);
  public
    constructor Create;override;
    destructor Destroy;override;
    function ToString: string;override;
    function Format(const Fmt: string): string;
    procedure MarkForSurvive;override;
    procedure TestChange;
    procedure Clear;override;
    procedure Delete(Index: integer);
    procedure DeleteLast;
    procedure Remove(const Name: string);overload;
    procedure Remove(Value: TLyValue);overload;
    function IndexOf(const Name: string): integer;overload;
    function IndexOf(Value: TLyValue): integer;overload;
    procedure Exchange(Index1, Index2: integer);
    procedure Move(CurIndex, NewIndex: integer);
    procedure GetFirstTwo(var V1, V2: TLyValue);
    procedure PrepareFor(Func: TLyFunc);
    procedure Sort;
    procedure SortByName;
    { assign }
    procedure Assign(AList: TLyList);overload;
    procedure Assign(AList: TLyValue);overload;
    procedure AssignNames(AList: TLyList);
    procedure AssignValues(AList: TLyList);
    { add }
    function Add(AType: TLyType; AData: pointer): TLyValue;overload;
    function Add(Strs: TStrings): integer;overload;
    function Add(const Name: string; Value: TLyValue): TLyValue;overload;
    function Add(const Name: string): TLyValue;overload;
    function Add(Value: TLyValue): TLyValue;overload;
    function Add: TLyValue;overload;
    { insert }
    function Insert(Index: integer; const Name: string; Value: TLyValue): TLyValue;overload;
    function Insert(Index: integer; const Name: string): TLyValue;overload;
    function Insert(Index: integer; Value: TLyValue): TLyValue;overload;
    function Insert(Index: integer): TLyValue;overload;
    { change }
    function Put(const Name: string; Value: TLyValue): TLyValue;overload;
    function Put(const Name, Value: string): TLyValue;overload;
    function Put(const Name: string; Value: int64): TLyValue;overload;
    function Put(const Name: string; Value: double): TLyValue;overload;
    function Put(const Name: string; Value: currency): TLyValue;overload;
    function Put(const Name: string; Value: boolean): TLyValue;overload;
    { copy }
    function Copy(Index, ItemCount: integer): TLyList;
    function CopyLeft(ItemCount: integer): TLyList;
    function CopyRight(ItemCount: integer): TLyList;
    function Clone: TLyList;
    { enum }
    function EnumType: TLyEnumType;
    function EnumSetType: TLyEnumSetType;
    function AsEnumSet: TLyEnumSet;
    { property }
    property Count: integer read GetCount write SetCount;
    property Items[Index: integer]: TLyValue read GetItem write SetItem;default;
    property Names[Index: integer]: string read GetName write SetName;
    property Values[const Name: string]: TLyValue read GetValue write SetValue;
    property First: TLyValue read GetFirst;
    property Last: TLyValue read GetLast;
    property Readonly: boolean read FReadonly write FReadonly;
  end;

  { TLyGenerate }

  TLyGenerate = class(TLyValue)
  public
    function GetNext: boolean;virtual;
    function HasNext: boolean;virtual;
  end;

  { TLyStringGenerate }

  TLyStringGenerate = class(TLyGenerate)
  private
    FS: string;
    FIndex: integer;
  public
    constructor CreateIn(const S: string);
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLyListGenerate }

  TLyListGenerate = class(TLyGenerate)
  private
    FL: TLyList;
    FIndex: integer;
  public
    constructor CreateIn(const AList: TLyList);
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLyIntGenerate }

  TLyIntGenerate = class(TLyGenerate)
  private
    FV, FCount: int64;
    FUpto: boolean;
  public
    constructor CreateIn(V1, V2: int64; Upto: boolean);overload;
    constructor CreateIn(Range: int64);overload;
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLyCharGenerate }

  TLyCharGenerate = class(TLyGenerate)
  private
    FV: char;
    FCount: integer;
    FUpto: boolean;
  public
    constructor CreateIn(V1, V2: char; Upto: boolean);
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLyBoolGenerate }

  TLyBoolGenerate = class(TLyGenerate)
  private
    FV: boolean;
    FCount: integer;
    FUpto: boolean;
  public
    constructor CreateIn(V1, V2, Upto: boolean);
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLyEnumGenerate }

  TLyEnumGenerate = class(TLyGenerate)
  private
    FV: TLyEnumItem;
    FCount: integer;
    FUpto: boolean;
  public
    constructor CreateIn(V1, V2: TLyEnumItem; Upto: boolean);
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLySystemModule }

  TLySystemModule = class(TLyModule)
  private
    procedure DoSetup(Sender: TObject);
  public
    constructor Create(const AName: string);override;
  end;

function LyseeFileCode(const FileName: string): string;
function AcquireLock: boolean;
function ReleaseLock: boolean;
function AddModule(const Name: string): TLyModule;
function Keywords: string;
function Hilights: string;
function Hilight(const ID: string): boolean;
function ExtractNameModule(const ID: string; var Module: string): string;
function FormatChar(C: char): string;
function FormatString(const S: string): string;
function FormatValue(V: TLyValue): string;
function IsResolvingID(const ID: string): boolean;
function IsParam(const ID: string; const T: TLyType): boolean;
function CompleteFileName(const FileName: string): string;
procedure ErrorConvert(AType, NewType: TLyType);
procedure ErrorOperation(L: TLyType; const OP: string; R: TLyType);overload;
procedure ErrorOperation(const OP: string; R: TLyType);overload;
function MatchStringType(AType: TLyType): boolean;
function MatchIntType(AType: TLyType): boolean;
function GetGenerate(Value: TLyValue): TLyGenerate;overload;
function GetGenerate(V1, V2: TLyValue; Upto: boolean): TLyGenerate;overload;
function Margin(Level: integer): string;
function GetSymbolText(Sym: TLySymbol): string;
procedure SetupTypes(const TypeList: array of TLyType);

var
  my_program     : string;             {<--program file name}
  my_kernel      : string;             {<--kernel file name}
  my_knpath      : string;             {<--kernel file path}
  my_kndir       : string;             {<--kernel file directory}
  my_home        : string;             {<--home path}
  my_tmpath      : string;             {<--temporary path}
  my_module_path : string;             {<--module search path list}
  my_gcman       : TLyCollect;         {<--unique garbage collection manager}
  my_system      : TLySystemModule;    {<--System module}
  my_modules     : TLyModuleList;      {<--binary modules}
  my_loadlib     : function(const Name, FileName: string): TLyModule;
  my_publics     : TList;              {<--public binary library}
  my_boolean_strs: array[boolean] of string = ('false', 'true');
  my_object      : TLyObjectType;
  my_variant     : TLyVariantType;
  my_nil         : TLyNilType;
  my_char, my_int, my_float, my_curr, my_time,
  my_bool, my_type, my_error, my_string, my_module, my_func,
  my_array: TLyType;

implementation

uses
  {$IFDEF FPC} {$ELSE}lysee_adodb,{$ENDIF}
  lysee_system, lysee_db, lysee_pmc;

procedure Exec_ID(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
var
  K: string;
  R: RLyFind;
  I: integer;
  F: TLyFunc;
begin
  K := Token.FName;
  if K[1] = '$' then
  begin
    if not Param.FThread.Resolve(K, Outv) then
      Outv.AsString := GetEnvironmentVariable(
        Copy(K, 2, Length(K)));
  end
  else
  if Param.FFunc.FindBy(K, @R) then
  begin
    if R.f_find = fiVarb then
    begin
      I := Param.FFunc.FParams.IndexOf(R.VVarb);
      Outv.Assign(Param.FParams[I]);
    end
    else
    if R.f_find = fiFunc then
      Token.ExecFunc(R.VFunc, Param, Outv, nil) else
    if R.f_find = fiValue then
    begin
      F := R.VValue.GetFunc;
      if F <> nil then
        Token.ExecFunc(F, Param, Outv, nil) else
      if R.VValue.FType = my_string then
        Outv.AsString := R.VValue.AsString else // protect constant strings
        Outv.Assign(R.VValue);
    end
    else Outv.Assign(@R);
  end
  else
  if not Param.FThread.Resolve(K, Outv) then
    Token.FailGet(Param, K);
end;

procedure Exec_Call(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
var
  tmpv: TLyValue;
  mark: integer;

  procedure exec_call;
  var
    T: TLyType;
    F: TLyFunc;
  begin
    if tmpv.FType = my_func then
    begin
      F := TLyFunc(tmpv.FData);
      if not Token.TryFunc(F, Param, Outv) then
        Token.Error(Param, 'got no function to call');
    end
    else
    if tmpv.FType = my_type then
    begin
      T := TLyType(tmpv.FData);
      if (T <> nil) and (T <> my_variant) and (T <> my_nil) then
      begin
        F := T.FindMethod(LSE_CREATE, nil);
        if F <> nil then
        begin
          Outv.Assign(T, T.CreateInstance);
          Token.TryMethod(Outv, F, Param, tmpv, nil);
        end
        else Token.Error(Param, 'type %s has no constructor', [T.FName]);
      end
      else Outv.Clear;
    end
    else Token.Error(Param, 'invalid calling: %s()', [tmpv.FType.FullName]);
  end;

var
  K: string;
  T: TLyType;
  F: TLyFunc;
  G: TLyGetAt;
begin
  Param.BeginExec(mark, tmpv);
  try
    K := Token.FName;
    if Token.FLeft = nil then
    begin
      // something()
      if (Param.FFunc.FindSave(K, Param, tmpv) <> fiNone)
        or Param.FThread.Resolve(K, tmpv) then
          exec_call else
          Token.FailGet(Param, K);
    end
    else
    if K = '' then
    begin
      // (expression)(...)
      if Token.FLeft.Execute(Param, tmpv) then
        exec_call;
    end
    else
    if Token.FLeft.Execute(Param, tmpv) then
    begin
      G := tmpv.GetAt(K, Outv);
      if G = gaMethod then
      begin
        // (expression).method(...)
        Token.TryMethod(tmpv, TLyFunc(Outv.FData), Param, Outv, nil);
      end
      else
      if G = gaData then
      begin
        if Outv.FType = my_func then
        begin
          F := TLyFunc(Outv.FData);
          if F.IsConstructor then
          begin
            T := TLyType(tmpv.FData);
            Outv.Assign(T, T.CreateInstance);
            Token.TryMethod(Outv, F, Param, tmpv, nil);
          end
          else Token.TryFunc(F, Param, Outv);
        end
        else exec_call();
      end
      else Token.FailGet(Param, tmpv.FType.FName, K);
    end;
  finally
    Param.EndExec(mark);
  end;
end;

procedure Exec_Int(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
begin
  Outv.SetAsInteger(Token.FValue.VInt);
end;

procedure Exec_String(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
begin
  Outv.SetAsString(Token.FName);
end;

procedure Exec_True(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
begin
  Outv.SetAsBoolean(true);
end;

procedure Exec_False(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
begin
  Outv.SetAsBoolean(false);
end;

procedure Exec_Get(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
var
  tmpv: TLyValue;
  mark: integer;
begin
  Param.BeginExec(mark, tmpv);
  try
    if Token.FLeft.Execute(Param, tmpv) then
      if not Token.GetProp(Param, tmpv, Outv) then
        Token.FailGet(Param, tmpv.FType.FName, Token.FName);
  finally
    Param.EndExec(mark);
  end;
end;

procedure Exec_Set(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
var
  tmpv, rval: TLyValue;
  mark: integer;
  func: TLyFunc;
begin
  Param.BeginExec(mark, tmpv);
  try
    // (expression).ID ==> (expression).SetID(value)
    if Token.FLeft.Execute(Param, tmpv) then
    begin
      func := tmpv.FType.FindMethod('Set' + Token.FName, tmpv.FData);
      if func = nil then
      begin
        rval := Param.FParams.Add;
        if Token.FRight.Execute(Param, rval) then
          if not tmpv.FType.Put(tmpv.FData, Token.FName, rval) then
            Token.Error(Param, 'failed setting: %s.%s', [tmpv.FType.FName, Token.FName]);
      end
      else Token.TryMethod(tmpv, func, Param, nil, Token.FRight);
    end;
  finally
    Param.EndExec(mark);
  end;
end;

procedure Exec_At(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
var
  tmpv: TLyValue;
  mark: integer;
begin
  if Token.FLeft <> nil then
  begin
    Param.BeginExec(mark, tmpv);
    try
      if Token.FLeft.Execute(Param, tmpv) then
        Token.GetAt(Param, tmpv, Outv);
    finally
      Param.EndExec(mark);
    end;
  end
  else
  if Param.FFunc.FindSave(Token.FName, Param, Outv) = fiNone then
    if not Param.FThread.Resolve(Token.FName, Outv) then
      Token.FailGet(Param, Token.FName);
end;

procedure Exec_Inherited(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
var
  V: TLyValue;
  T: TLyType;
begin
  if (Param.Func.Params.ParamCount > 0) and (Param.Params.Count > 0) then
  begin
    V := Param[0];
    T := V.VType;
    if T.FParent = nil then
      Param.Error('%s type has no father', [T.FName]) else
      Outv.Assign(T.Parent, V.FData);
  end
  else Param.Error('function %s has no parametres', [Param.FFunc.Name]);
end;

procedure Exec_Array(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
var
  A: TLyList;
begin
  A := Token.SetupParamList(Param, nil);
  if A <> nil then
  begin
    A.FReadonly := true;
    Outv.Assign(my_array, A);
  end;
end;

procedure Exec_Hash(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
var
  H: TLyList;
  I, N: integer;
  K: string;
begin
  H := TLyList.Create;
  N := Token.GetParamCount;
  I := 0;
  while (I < N) and Token.GetParam(I).Execute(Param, Outv) do
  begin
    K := Outv.AsString;
    if Token.GetParam(I + 1).Execute(Param, Outv) then
      H.Put(K, Outv) else
      Break;
    Inc(I, 2);
  end;
  if Param.FThread.StatusOK then
    Outv.Assign(my_array, H) else
    H.Free;
end;

procedure Exec_nil(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
begin
  Outv.Clear;
end;

procedure Exec_Self(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
begin
  if Param.Func.FParams.ParamCount > 0 then
    Outv.Assign(Param[0]) else
    Token.FailGet(Param, 'Self');
end;

procedure Exec_Object(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
begin
  Outv.Assign(my_object, my_object.CreateInstance);
end;

procedure Exec_Char(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
begin
  Outv.SetAsChar(Token.FValue.VChar);
end;

procedure Exec_Float(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
begin
  Outv.SetAsFloat(Token.FValue.VFloat);
end;

procedure Exec_Lambda(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
begin
  Outv.Assign(my_func, Token.FValue.VFunc);
end;

procedure Exec_Result(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
begin
  Outv.Assign(Param.FResult);
end;

procedure Exec_Neg(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
var
  M: integer;
begin
  Param.BeginExec(M);
  try
    if Token.FRight.Execute(Param, Outv) then
      Outv.ExecNeg;
  finally
    Param.EndExec(M);
  end;
end;

procedure Exec_Not(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
var
  M: integer;
begin
  Param.BeginExec(M);
  try
    if Token.FRight.Execute(Param, Outv) then
      Outv.ExecNot;
  finally
    Param.EndExec(M);
  end;
end;

procedure Exec_And(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
var
  V: TLyValue;
  M: integer;
begin
  Param.BeginExec(M, V);
  try
    if Token.FLeft.Execute(Param, Outv) then
      if Outv.FType = my_int then
      begin
        if Token.FRight.Execute(Param, V) then
          if V.FType = my_int then
            Outv.AsInteger := Outv.AsInteger and V.AsInteger else
            Outv.SetAsBoolean((Outv.AsInteger <> 0) and V.AsBoolean);
      end
      else Outv.SetAsBoolean(Outv.AsBoolean and
        (Token.FRight.Execute(Param, Outv) and Outv.AsBoolean));
  finally
    Param.EndExec(M);
  end;
end;

procedure Exec_Or(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
var
  V: TLyValue;
  M: integer;
begin
  Param.BeginExec(M, V);
  try
    if Token.FLeft.Execute(Param, Outv) then
      if Outv.FType = my_int then
      begin
        if Token.FRight.Execute(Param, V) then
          if V.FType = my_int then
            Outv.AsInteger := Outv.AsInteger or V.AsInteger else
            Outv.SetAsBoolean((Outv.AsInteger <> 0) or V.AsBoolean);
      end
      else Outv.SetAsBoolean(Outv.AsBoolean or
        (Token.FRight.Execute(Param, Outv) and Outv.AsBoolean));
  finally
    Param.EndExec(M);
  end;
end;

procedure Exec_Dual(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
var
  V: TLyValue;
  M: integer;
begin
  Param.BeginExec(M, V);
  try
    if Token.FLeft.Execute(Param, Outv) and
       Token.FRight.Execute(Param, V) then
    case Token.FSym of
    { operator: 0}
      syMul   : Outv.ExecMul(V);
      syDiv   : Outv.ExecDiv(V);
      syDivf  : Outv.ExecDivf(V);
      syMod   : Outv.ExecMod(V);
    { operator: 1}
      syAdd   : Outv.ExecAdd(V);
      syReduce: Outv.ExecDec(V);
    { operator: 2}
      syXor   : Outv.ExecXor(V);
      syShr   : Outv.ExecShr(V);
      syShl   : Outv.ExecShl(V);
    { operator: 3}
      syEQ    : Outv.ExecCompare(V, [crEqual]);
      syNE    : Outv.ExecCompare(V, [crLess, crMore, crDiff]);
      syLT    : Outv.ExecCompare(V, [crLess]);
      syLE    : Outv.ExecCompare(V, [crLess, crEqual]);
      syMT    : Outv.ExecCompare(V, [crMore]);
      syME    : Outv.ExecCompare(V, [crMore, crEqual]);
      sySame  : Outv.ExecSame(V);
      syIn    : Outv.ExecIn(V);
      syLike  : Outv.ExecLike(V);
      syIs    : Outv.ExecIs(V);
      syAs    : Outv.ExecAs(V);
    end;
  finally
    Param.EndExec(M);
  end;
end;

procedure Exec_Error(Token: TLyToken; Param: TLyParam; Outv: TLyValue);
begin
  Token.Error(Param, 'Unknown operation: ' +
    Copy(GetSymbolText(Token.FSym), 3, 64));
end;

const

  Symbols: array[TLySymbol] of packed record
    SY: string;       // symbol text
    ID: string;       // symbol spelling
    TX: TLyTokenProc; // used by TLyToken.Execute(...)
  end = (
    (SY:'syError';     ID:'';          TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syBegin';     ID:'begin';     TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syUses';      ID:'uses';      TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syConst';     ID:'const';     TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syDef';       ID:'def';       TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syClass';     ID:'class';     TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'sySelf';      ID:'self';      TX:{$IFDEF FPC}@{$ENDIF}Exec_Self),
    (SY:'syObject';    ID:'object';    TX:{$IFDEF FPC}@{$ENDIF}Exec_Object),
    (SY:'syInherited'; ID:'inherited'; TX:{$IFDEF FPC}@{$ENDIF}Exec_Inherited),
    (SY:'syArray';     ID:'array';     TX:{$IFDEF FPC}@{$ENDIF}Exec_Array),
    (SY:'syVar';       ID:'var';       TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syGet';       ID:'get';       TX:{$IFDEF FPC}@{$ENDIF}Exec_Get),
    (SY:'sySet';       ID:'set';       TX:{$IFDEF FPC}@{$ENDIF}Exec_Set),
    (SY:'syIf';        ID:'if';        TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syElif';      ID:'elif';      TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syThen';      ID:'then';      TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syElse';      ID:'else';      TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syCase';      ID:'case';      TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syOf';        ID:'of';        TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syFor';       ID:'for';       TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syWhile';     ID:'while';     TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syRepeat';    ID:'repeat';    TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syUntil';     ID:'until';     TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syDo';        ID:'do';        TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syResult';    ID:'result';    TX:{$IFDEF FPC}@{$ENDIF}Exec_Result),
    (SY:'syDiv';       ID:'div';       TX:{$IFDEF FPC}@{$ENDIF}Exec_Dual),
    (SY:'syMod';       ID:'mod';       TX:{$IFDEF FPC}@{$ENDIF}Exec_Dual),
    (SY:'syTo';        ID:'to';        TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syDownto';    ID:'downto';    TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syNot';       ID:'not';       TX:{$IFDEF FPC}@{$ENDIF}Exec_Not),
    (SY:'syIn';        ID:'in';        TX:{$IFDEF FPC}@{$ENDIF}Exec_Dual),
    (SY:'syIs';        ID:'is';        TX:{$IFDEF FPC}@{$ENDIF}Exec_Dual),
    (SY:'syAs';        ID:'as';        TX:{$IFDEF FPC}@{$ENDIF}Exec_Dual),
    (SY:'syLike';      ID:'like';      TX:{$IFDEF FPC}@{$ENDIF}Exec_Dual),
    (SY:'syAnd';       ID:'and';       TX:{$IFDEF FPC}@{$ENDIF}Exec_And),
    (SY:'syOr';        ID:'or';        TX:{$IFDEF FPC}@{$ENDIF}Exec_Or),
    (SY:'syXor';       ID:'xor';       TX:{$IFDEF FPC}@{$ENDIF}Exec_Dual),
    (SY:'syShr';       ID:'shr';       TX:{$IFDEF FPC}@{$ENDIF}Exec_Dual),
    (SY:'syShl';       ID:'shl';       TX:{$IFDEF FPC}@{$ENDIF}Exec_Dual),
    (SY:'syTry';       ID:'try';       TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syExcept';    ID:'except';    TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syFinally';   ID:'finally';   TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syRaise';     ID:'raise';     TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syTrue';      ID:'true';      TX:{$IFDEF FPC}@{$ENDIF}Exec_True),
    (SY:'syFalse';     ID:'false';     TX:{$IFDEF FPC}@{$ENDIF}Exec_False),
    (SY:'syNil';       ID:'nil';       TX:{$IFDEF FPC}@{$ENDIF}Exec_nil),
    (SY:'syEnd';       ID:'end';       TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syBecome';    ID:':=';        TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syAdd';       ID:'+';         TX:{$IFDEF FPC}@{$ENDIF}Exec_Dual),
    (SY:'syReduce';    ID:'-';         TX:{$IFDEF FPC}@{$ENDIF}Exec_Dual),
    (SY:'syMul';       ID:'*';         TX:{$IFDEF FPC}@{$ENDIF}Exec_Dual),
    (SY:'syDivf';      ID:'/';         TX:{$IFDEF FPC}@{$ENDIF}Exec_Dual),
    (SY:'syLParen';    ID:'(';         TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syRParen';    ID:')';         TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syLArray';    ID:'[';         TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syRArray';    ID:']';         TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syDot';       ID:'.';         TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syRange';     ID:'..';        TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syColon';     ID:':';         TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'sySemic';     ID:';';         TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syComma';     ID:',';         TX:{$IFDEF FPC}@{$ENDIF}Exec_Error),
    (SY:'syAt';        ID:'@';         TX:{$IFDEF FPC}@{$ENDIF}Exec_At),
    (SY:'sySame';      ID:'==';        TX:{$IFDEF FPC}@{$ENDIF}Exec_Dual),
    (SY:'syEQ';        ID:'=';         TX:{$IFDEF FPC}@{$ENDIF}Exec_Dual),
    (SY:'syNE';        ID:'<>';        TX:{$IFDEF FPC}@{$ENDIF}Exec_Dual),
    (SY:'syLT';        ID:'<';         TX:{$IFDEF FPC}@{$ENDIF}Exec_Dual),
    (SY:'syLE';        ID:'<=';        TX:{$IFDEF FPC}@{$ENDIF}Exec_Dual),
    (SY:'syMT';        ID:'>';         TX:{$IFDEF FPC}@{$ENDIF}Exec_Dual),
    (SY:'syME';        ID:'>=';        TX:{$IFDEF FPC}@{$ENDIF}Exec_Dual),
    (SY:'syVert';      ID:'|';         TX:{$IFDEF FPC}@{$ENDIF}Exec_Lambda),
    (SY:'syID';        ID:'';          TX:{$IFDEF FPC}@{$ENDIF}Exec_ID),
    (SY:'syNeg';       ID:'';          TX:{$IFDEF FPC}@{$ENDIF}Exec_Neg),
    (SY:'syFloat';     ID:'';          TX:{$IFDEF FPC}@{$ENDIF}Exec_Float),
    (SY:'syInt';       ID:'';          TX:{$IFDEF FPC}@{$ENDIF}Exec_Int),
    (SY:'syStr';       ID:'';          TX:{$IFDEF FPC}@{$ENDIF}Exec_String),
    (SY:'syChar';      ID:'';          TX:{$IFDEF FPC}@{$ENDIF}Exec_Char),
    (SY:'syHash';      ID:'';          TX:{$IFDEF FPC}@{$ENDIF}Exec_Hash),
    (SY:'syCall';      ID:'';          TX:{$IFDEF FPC}@{$ENDIF}Exec_Call),
    (SY:'syEOF';       ID:'';          TX:{$IFDEF FPC}@{$ENDIF}Exec_Error)
  );

  NameSyms = [syID, syStr, syCall, syGet, sySet];
  DataSyms = [syID, syResult, syFloat, syInt, syStr, syChar, sySelf, syObject,
              syInherited, syTrue, syFalse, syNil, syArray];
  ExprHead = DataSyms + [syLParen, syLArray, syNot, syReduce, syAt, syVert];

  OperSyms = [
    syMul, syDiv, syDivf, syMod,                                            // 0
    syAdd, syReduce,                                                        // 1
    syXor, syShl, syShr,                                                    // 2
    sySame, syEQ, syNE, syLT, syLE, syMT, syME, syIn, syLike, syAs, syIs,   // 3
    syAnd, syOr];                                                           // 4

  OperLevel: array[0..4] of TLySymbols = (
    [syMul, syDiv, syDivf, syMod],                                          // 0
    [syAdd, syReduce],                                                      // 1
    [syXor, syShl, syShr],                                                  // 2
    [sySame, syEQ, syNE, syLT, syLE, syMT, syME, syIn, syLike, syAs, syIs], // 3
    [syAnd, syOr]                                                           // 4
  );

var

  my_spinlock: Syncobjs.TCriticalSection;
  my_keywords: string;
  my_hilights: string;

procedure LyseeObjectProc(const Param: TLyParam);
begin
  Param.FFunc.FMethod(Param);
end;

// system.function -------------------------------------------------------------

procedure pp_halt(const Param: TLyParam);
var
  T: TLyThread;
begin
  T := Param.FThread;
  if Param.FPrmc > 0 then
    T.FResult.Assign(Param[0]);
  T.FHalted := true;
  T.FError.Clear;
  T.Terminate;
end;

procedure pp_exit(const Param: TLyParam);
begin
  Param.FThread.FState := tsExit;
  if Param.FPrmc > 0 then
    Param.FPrev.FResult.Assign(Param[0]);
end;

procedure pp_break(const Param: TLyParam);
begin
  Param.FThread.FState := tsBreak;
end;

procedure pp_continue(const Param: TLyParam);
begin
  Param.FThread.FState := tsContinue;
end;

procedure pp_compile(const Param: TLyParam);
var
  F: TLyFunc;
begin
  F := Param.FThread.Compile(Param[0].AsString, '');
  Param.Result.Assign(my_func, F);
end;

procedure pp_eval(const Param: TLyParam);
var
  code: string;
  func: TLyFunc;
begin
  code := TrimRight(Param[0].AsString);
  if code <> '' then
  begin
    func := Param.FThread.Compile(AddTrailingChar(code, ';'), '');
    if func <> nil then
      Param.FToken.ExecFunc(func, Param, Param.FResult, nil);
  end
  else Param.Result.Clear;
end;

procedure pp_Get(const Param: TLyParam);
var
  S: string;
begin
  S := Trim(Param[0].AsString);
  if Param.FThread.CodeFunc.FindSave(S, Param, Param.FResult) = fiNone then
    if not Param.FThread.Resolve(S, Param.FResult) then
      Param.Result.AsString := GetEnvironmentVariable(S);
end;

procedure pp_typeof(const Param: TLyParam);
begin
  Param.Result.Assign(my_type, Param[0].AsType);
end;

procedure pp_moduleof(const Param: TLyParam);
var
  M: TLyModule;
begin
  M := Param[0].AsModule;
  if M = nil then
    M := Param[0].FType.FModule;
  Param.Result.Assign(my_module, M);
end;

procedure pp_fileof(const Param: TLyParam);
var
  M: TLyModule;
begin
  M := Param[0].AsModule;
  if M = nil then
    M := Param[0].FType.FModule;
  Param.Result.AsString := M.FFileName;
end;

procedure pp_collectGarbage(const Param: TLyParam);
begin
  Param.Result.AsInteger := my_gcman.Collect;
end;

procedure pp_inc(const Param: TLyParam);
var
  T: TLyType;
  V: TLyValue;
  I, X: int64;
  C: char;
  B: boolean;
begin
  V := Param.GetVarbValue(0, T);
  if T = my_variant then
    T := V.FType;

  if Param.FPrmc > 1 then
    X := Param[1].AsInteger else
    X := 1;

  if T = my_int then
    V.AsInteger := V.AsInteger + X else
  if T = my_float then
    V.AsFloat := V.AsFloat + X else
  if T = my_curr then
    V.AsCurrency := V.AsCurrency + X else
  if T = my_char then
  begin
    C := V.AsChar;
    Inc(C, X);
    V.AsChar := C;
  end
  else
  if T = my_bool then
  begin
    B := V.AsBoolean;
    Inc(B, X);
    V.AsBoolean := B;
  end
  else
  if T.IsEnumType then
  begin
    I := V.AsEnum.FValue + X;
    if I < 0 then I := 0 else
    if I > TLyEnumType(T).Count - 1 then
      I := TLyEnumType(T).Count - 1;
    V.AsEnum := TLyEnumType(T)[I];
  end
  else Param.Error('failed increase %s value', [T.FName]);
end;

procedure pp_dec(const Param: TLyParam);
var
  T: TLyType;
  V: TLyValue;
  I, X: int64;
  B: boolean;
  C: char;
begin
  V := Param.GetVarbValue(0, T);
  if T = my_variant then
    T := V.FType;

  if Param.FPrmc > 1 then
    X := Param[1].AsInteger else
    X := 1;

  if T = my_int then
    V.AsInteger := V.AsInteger - X else
  if T = my_float then
    V.AsFloat := V.AsFloat - X else
  if T = my_curr then
    V.AsCurrency := V.AsCurrency - X else
  if T = my_char then
  begin
    C := V.AsChar;
    Dec(C, X);
    V.AsChar := C;
  end
  else
  if T = my_bool then
  begin
    B := V.AsBoolean;
    Dec(B, X);
    V.AsBoolean := B;
  end
  else
  if T.IsEnumType then
  begin
    I := V.AsEnum.FValue - X;
    if I < 0 then I := 0 else
    if I > TLyEnumType(T).Count - 1 then
      I := TLyEnumType(T).Count - 1;
    V.AsEnum := TLyEnumType(T)[I];
  end
  else Param.Error('failed decrease %s value', [T.FName]);
end;

procedure pp_readln(const Param: TLyParam);
var
  S: string;
begin
  S := '';
  Param.FThread.FEngine.Readln(S);
  Param.Result.AsString := S;
end;

procedure pp_write(const Param: TLyParam);
begin
  Param.FThread.FEngine.Write(Param[0].AsString);
end;

procedure pp_writeln(const Param: TLyParam);
begin
  Param.FThread.FEngine.Write(Param[0].AsString);
  Param.FThread.FEngine.Write(sLineBreak);
end;

procedure pp_length(const Param: TLyParam);
begin
  Param.Result.AsInteger := Param[0].FType.GetLength(Param[0].FData);
end;

procedure pp_pass(const Param: TLyParam);
begin
  { do nothing }
end;

procedure pp_decompile(const Param: TLyParam);
var
  F: TLyFunc;
  L: TStringList;
begin
  F := Param[0].GetFunc;
  if F <> nil then
  begin
    L := TStringList.Create;
    try
      F.Decompile(0, L);
      Param.Result.AsString := L.Text;
    finally
      L.Free;
    end;
  end;
end;

procedure pp_exceptObject(const Param: TLyParam);
begin
  Param.Result.Assign(my_error, Param.FThread.FError);
end;

// kernel ----------------------------------------------------------------------

function LyseeFileCode(const FileName: string): string;
begin
  Result := GetFileUTFS(FileName);
end;

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

function AddModule(const Name: string): TLyModule;
begin
  if IsID(Name) then
  begin
    Result := my_modules.Find(Name);
    if Result = nil then
      Result := TLyModule.Create(Name);
  end
  else Result := nil;
end;

function Keywords: string;
var
  I: TLySymbol;
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
      L.Add(LSE_SYSTEM);
      L.Add(LSE_MAIN);
      L.Add('string');
      L.Add('boolean');
      L.Add('variant');
      L.Add('type');
      L.Sort;
      my_hilights := L.CommaText;
    finally
      L.Free;
    end;
  end;
  Result := my_hilights;
end;

function Hilight(const ID: string): boolean;
var
  I: TLySymbol;
begin
  for I := syBegin to syEnd do
    if MatchID(Symbols[I].ID, ID) then
    begin
      Result := true;
      Exit;
    end;
  Result := MatchID(ID, ['string', 'boolean', 'variant', 'type']);
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

function FormatValue(V: TLyValue): string;
begin
  if V.FType = my_char then
    Result := FormatChar(V.AsChar) else
  if V.FType = my_string then
    Result := FormatString(V.AsString) else
    Result := V.AsString;
end;

function IsResolvingID(const ID: string): boolean;
begin
  Result := (Length(ID) > 1) and (ID[1] = '_') and (ID[2] = '_');
end;

function IsParam(const ID: string; const T: TLyType): boolean;
begin
  Result := (T <> nil) and (T <> my_nil) and IsID(ID);
end;

procedure ErrorConvert(AType, NewType: TLyType);
begin
  Throw('can not convert %s to %s', [AType.FName, NewType.FName]);
end;

procedure ErrorOperation(L: TLyType; const OP: string; R: TLyType);
begin
  Throw('invalid operation: %s %s %s', [L.FName, OP, R.FName]);
end;

procedure ErrorOperation(const OP: string; R: TLyType);
begin
  Throw('invalid operation: %s %s', [OP, R.FName]);
end;

function MatchStringType(AType: TLyType): boolean;
begin
  Result := (AType = my_string) or (AType = my_char);
end;

function MatchIntType(AType: TLyType): boolean;
begin
  Result := (AType = my_int) or (AType = my_bool) or AType.IsEnumType;
end;

function GetGenerate(Value: TLyValue): TLyGenerate;
begin
  if Value <> nil then
    Result := Value.FType.Generate(Value.FData) else
    Result := nil;
end;

function GetGenerate(V1, V2: TLyValue; Upto: boolean): TLyGenerate;
var
  P: TLyCompare;
begin
  Result := nil;
  if V1.FType = V2.FType then
  begin
    P := V1.Compare(V2);
    if (P = crEqual) or (Upto and (P = crLess)) or (not Upto and (P = crMore)) then
      if V1.FType = my_int then
        Result := TLyIntGenerate.CreateIn(V1.AsInteger, V2.AsInteger, Upto) else
      if V1.FType = my_char then
        Result := TLyCharGenerate.CreateIn(V1.AsChar, V2.AsChar, Upto) else
      if V1.FType = my_bool then
        Result := TLyBoolGenerate.CreateIn(V1.AsBoolean, V2.AsBoolean, Upto) else
      if V1.FType.IsEnumType then
        Result := TLyEnumGenerate.CreateIn(V1.AsEnum, V2.AsEnum, Upto);
  end;
end;

function Margin(Level: integer): string;
begin
  Result := StringOfChar(' ', Level * 2);
end;

function GetSymbolText(Sym: TLySymbol): string;
begin
  Result := Symbols[Sym].SY;
end;

procedure SetupTypes(const TypeList: array of TLyType);
var
  I: integer;
begin
  for I := 0 to Length(TypeList) - 1 do
    TypeList[I].Setup;
end;

{ TLysee }

procedure TLysee.Readln(var Text: string);
begin
  if Assigned(FOnReadln) then
    FOnReadln(Self, Text) else
    System.Readln(Text);
end;

procedure TLysee.Write(const Text: string);
var
  L: integer;
begin
  L := Length(Text);
  if L > 0 then
  begin
    FLastWritenChar := Text[L];
    Inc(FWriteCount);
    if FWriteStream <> nil then
      FWriteStream.WriteString(Text) else
    if Assigned(FOnWrite) then
      FOnWrite(Self, Text) else
    if IsConsole then
      System.Write(Text);
  end;
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

procedure TLysee.MarkSurvived;
var
  L: TList;
  I: integer;
  M: TLyModule;
begin
  L := FThreads.LockList;
  try
    for I := 0 to L.Count - 1 do
      TLyThread(L[I]).MarkSurvived;

    for I := 0 to FModules.Count - 1 do
    begin
      M := FModules[I];
      if M.FEngine = Self then
        M.FConsts.MarkForSurvive;
    end;

    MarkMoreForSurvive;
  finally
    FThreads.UnlockList;
  end;
end;

function TLysee.NewThread(const AName: string): TLyThread;
var
  L: TList;
  I: integer;
begin
  L := FThreads.LockList;
  try
    if AName <> '' then
      for I := 0 to L.Count - 1 do
        if MatchID(TLyThread(L[I]).FName, AName) then
          Throw('Thread %s already exists', [AName]);
    Result := TLyThread.Create;
    Result.FEngine := Self;
    if AName = '' then
      Result.FName := Format('thread_%p', [pointer(Result)]) else
      Result.FName := AName;
    L.Add(Result);
  finally
    FThreads.UnlockList;
  end;
end;

procedure TLysee.CheckNotCompiling;
begin
  Check(FRollbacks = nil, 'Invalid embeded compiling');
end;

procedure TLysee.Clear(WillDestroy: boolean);
var
  L: TList;
  I, N: integer;
  T: TLyThread;
begin
  L := FThreads.LockList;
  try
    N := 0;
    for I := 0 to L.Count - 1 do
      if TLyThread(L[I]).Running then
        Inc(N);
    Check(N = 0, '%d threads are still running', [N]);
    for I := 0 to L.Count - 1 do
    begin
      T := TLyThread(L[I]);
      T.Clear;
      L.Delete(I);
      T.Free;
    end;
    FNameSeed := 0;
    FMainThread := nil;
    FModules.Clear;
    if not WillDestroy then MainModule;
  finally
    FThreads.UnlockList;
  end;
end;

constructor TLysee.Create(AOwner: TComponent);
begin
  inherited;
  my_system.Setup;
  FPublics := TList.Create;
  FModules := TLyModuleList.Create(Self);
  FThreads := TThreadList.Create;
  NewThread(LSE_MAIN);
end;

destructor TLysee.Destroy;
begin
  Clear(true);
  FreeAndNil(FThreads);
  FreeAndNil(FModules);
  FreeAndNil(FPublics);
  inherited;
end;

procedure TLysee.MarkMoreForSurvive;
begin
  { nothing }
end;

procedure TLysee.Error(const Msg: string);
begin
  if (FWriteStream = nil) and Assigned(FOnError) then
    FOnError(Self, Msg) else
    Write(Msg);
end;

function TLysee.Execute(const Code, FileURL: string): boolean;
var
  T: TLyThread;
begin
  CheckNotCompiling;

  if FileURL <> '' then
  begin
    MainThread.CheckNotRunning;
    MainModule.FFileName := FileURL;
    T := MainThread;
  end
  else
  if MainThread.Running then
  begin
    T := NewThread;
    T.FMainModule := MainModule;
  end
  else T := MainThread;

  try
    try
      FRollbacks := TList.Create;
      try
        TLyParser.Create(T.FMainModule, T.FError).ParseAndFree(Code);
        FreeAndNil(FRollbacks);
        if not T.Running then
          T.FState := tsReady;
      except
        Rollback;
        raise;
      end;
      T.ExecuteMainFunc;
      Result := not T.FExcepted;
    except
      Result := false;
      if not T.FExcepted then
        T.FError.Runtime(ExceptionStr, T.MainModule.FName,
          T.MainModule.FFileName, 0, 0);
    end;

    if not Result then
      Error(T.FError.AsText);
  finally
    if T <> MainThread then
    begin
      T.FMainModule := nil;
      T.FMainFunc.FName := '';
      T.FMainFunc.DecRefcount;
      T.FMainFunc := nil;
      T.Free;
    end;
  end;
end;

function TLysee.ExecuteFile(const FileName: string; const Args: array of string): boolean;
begin
  Clear;
  Result := MainThread.ExecuteFile(FileName, Args);
end;

function TLysee.ExecuteFileFrom(StartParamIndex: integer): boolean;
begin
  Clear;
  Result := MainThread.ExecuteFileFrom(StartParamIndex);
end;

function TLysee.MainModule: TLyModule;
begin
  Result := MainThread.MainModule;
end;

function TLysee.MainThread: TLyThread;
begin
  if FMainThread = nil then
    FMainThread := ForceThread(LSE_MAIN);
  Result := FMainThread;
end;

function TLysee.LocateModuleFile(const ModuleName: string): string;
begin
  if Assigned(FOnGetModuleFile) then
  begin
    Result := FOnGetModuleFile(Self, ModuleName);
    if FileExists(Result) then Exit;
  end;

  Result := my_module_path + ModuleName + LSE_FILEEXT;
  if not FileExists(Result) then
  begin
    Result := my_module_path + ModuleName + LSE_LIBEXT;
    if not FileExists(Result) then
      Result := '';
  end;
end;

function TLysee.GetThread(Index: integer): TLyThread;
var
  L: TList;
begin
  L := FThreads.LockList;
  try
    if (Index >= 0) and (Index < L.Count) then
      Result := TLyThread(L[Index]) else
      Result := nil;
  finally
    FThreads.UnlockList;
  end;
end;

function TLysee.FindModuleByFileName(const FileName: string): TLyModule;
var
  I: integer;
begin
  for I := 0 to FModules.Count - 1 do
  begin
    Result := FModules[I];
    if SameFileName(FileName, Result.FFileName) then Exit;
  end;

  for I := 0 to my_modules.Count - 1 do
  begin
    Result := my_modules[I];
    if SameFileName(FileName, Result.FFileName) then Exit;
  end;

  Result := nil;
end;

function TLysee.FindModuleByName(const ModuleName: string): TLyModule;
var
  I: integer;
begin
  for I := 0 to FModules.Count - 1 do
  begin
    Result := FModules[I];
    if MatchID(ModuleName, Result.FName) then Exit;
  end;

  for I := 0 to my_modules.Count - 1 do
  begin
    Result := my_modules[I];
    if MatchID(ModuleName, Result.FName) then Exit;
  end;

  Result := nil;
end;

function TLysee.FindThread(const AName: string): TLyThread;
var
  L: TList;
  I: integer;
begin
  Result := nil;
  if AName <> '' then
  begin
    L := FThreads.LockList;
    try
      for I := 0 to L.Count - 1 do
        if SameText(TLyThread(L[I]).FName, AName) then
        begin
          Result := TLyThread(L[I]);
          Break;
        end;
    finally
      FThreads.UnlockList;
    end;
  end;
end;

function TLysee.ForceThread(const AName: string): TLyThread;
var
  L: TList;
  I: integer;
begin
  L := FThreads.LockList;
  try
    Result := nil;
    if AName <> '' then
      for I := 0 to L.Count - 1 do
        if MatchID(TLyThread(L[I]).FName, AName) then
        begin
          Result := TLyThread(L[I]);
          Break;
        end;
    if Result = nil then
    begin
      Result := TLyThread.Create;
      Result.FEngine := Self;
      if AName = '' then
        Result.FName := Format('thread_%p', [pointer(Result)]) else
        Result.FName := AName;
      L.Add(Result);
    end;
  finally
    FThreads.UnlockList;
  end;
end;

function TLysee.GetThreadCount: integer;
begin
  Result := FThreads.LockList.Count;
  FThreads.UnlockList;
end;

function TLysee.Resolve(const ID: string; Value: TLyValue): boolean;
begin
  Result := (ID <> '') and Assigned(FOnResolve) and FOnResolve(ID, Value);
end;

procedure TLysee.Rollback;
var
  I: integer;
begin
  if FRollbacks <> nil then
  try
    for I := FRollbacks.Count - 1 downto 0 do
      if I < FRollbacks.Count then
        TLyObject(FRollbacks[I]).Free;
  finally
    FreeAndNil(FRollbacks);
  end;
end;

procedure TLysee.RollbackAdd(AObj: TObject);
begin
  if (FRollbacks <> nil) and (AObj <> nil) then
    if FRollbacks.IndexOf(AObj) < 0 then
      FRollbacks.Add(AObj);
end;

procedure TLysee.RollbackRemove(AObj: TObject);
begin
  if (FRollbacks <> nil) and (AObj <> nil) then
    FRollbacks.Remove(AObj);
end;

procedure TLysee.Terminate;
var
  L: TList;
  I: integer;
begin
  L := FThreads.LockList;
  try
    for I := 0 to L.Count - 1 do
      TLyThread(L[I]).Terminate;
  finally
    FThreads.UnlockList;
  end;
end;

function TLysee.GetRunningThreadCount: integer;
var
  L: TList;
  I: integer;
begin
  Result := 0;
  L := FThreads.LockList;
  try
    for I := 0 to L.Count - 1 do
      if TLyThread(L[I]).Running then
        Inc(Result);
  finally
    FThreads.UnlockList;
  end;
end;

{ TLyThread }

procedure TLyThread.BeginExecute;
begin
  if Assigned(FEngine.FOnExecuting) then
    FEngine.FOnExecuting(FEngine, Self);
end;

procedure TLyThread.CheckNotCompiling;
begin
  FEngine.CheckNotCompiling;
end;

procedure TLyThread.CheckNotRunning;
begin
  Check(not Running, 'Current thread is executing');
end;

procedure TLyThread.CheckStatusOK;
begin
  Check(StatusOK, 'Thread status is incorrect');
end;

procedure TLyThread.Clear;
begin
  CheckNotRunning;
  CheckNotCompiling;
  FResult.Clear;
  FArgs.Clear;
  FMainLocals.Clear;
  FError.Clear;
  FreeAndNil(FMainFunc);
  FreeAndNil(FMainModule);
end;

function TLyThread.Compile(const Code, FileName: string): TLyFunc;
begin
  Result := nil;
  CheckNotCompiling;
  FEngine.FRollbacks := TList.Create;
  try
    if not Running then
    begin
      FError.Clear;
      Result := TLyParser.Create(MainModule, FError).ParseAndFree(Code);
      FreeAndNil(FEngine.FRollbacks);
      FState := tsReady;
    end
    else
    if StatusOK then
    begin
      Result := TLyParser.Create(CodeModule, FError).ParseAndFree(Code);
      if FExcepted then
      begin
        FEngine.Rollback;
        Result := nil;
      end
      else FreeAndNil(FEngine.FRollbacks);
    end
    else Throw('Thread status is incorrect');
  except
    FEngine.Rollback;
    raise;
  end;
end;

constructor TLyThread.Create;
begin
  FState := tsTerminated;
  FTerminated := true;
  FHalted := false;
  FError := TLyError.Create(Self);
  FArgs := TLyList.Create;
  FArgs.IncRefcount;
  FResult := TLyValue.Create;
  FMainLocals := TLyList.Create;
  FMainLocals.IncRefcount;
end;

destructor TLyThread.Destroy;
begin
  if FEngine <> nil then
  begin
    if FEngine.FMainThread = Self then
      FEngine.FMainThread := nil;
    FEngine.FThreads.Remove(Self);
  end;
  FreeAndNil(FMainFunc);
  FreeAndNil(FMainModule);
  FreeAndNil(FResult);
  FreeAndNil(FArgs);
  FreeAndNil(FMainLocals);
  FreeAndNil(FError);
  inherited;
end;

procedure TLyThread.EndExecute;
begin
  if Assigned(FEngine.FOnExecuted) then
    FEngine.FOnExecuted(FEngine, Self);
end;

function TLyThread.Execute(const Code, FileURL: string): boolean;
var
  F: TLyFunc;
begin
  if FileURL <> '' then CheckNotRunning;
  CheckNotCompiling;
  try
    if (FileURL <> '') or not Running then
    begin
      FEngine.FRollbacks := TList.Create;
      try
        if FileURL <> '' then
          MainModule.FFileName := FileURL;
        TLyParser.Create(MainModule, FError).ParseAndFree(Code);
        FreeAndNil(FEngine.FRollbacks);
        FState := tsReady;
      except
        FEngine.Rollback;
        raise;
      end;
      ExecuteMainFunc;
    end
    else
    begin
      CheckStatusOK;
      FEngine.FRollbacks := TList.Create;
      try
        F := TLyParser.Create(CodeModule, FError).ParseAndFree(Code);
        FreeAndNil(FEngine.FRollbacks);
      except
        FEngine.Rollback;
        raise;
      end;
      FCurrent.FToken.ExecFunc(F, FCurrent, nil, nil);
    end;
    Result := not FExcepted;
  except
    Result := false;
    if not FExcepted then
      FError.Runtime(ExceptionStr, MainModule.FName,
        MainModule.FFileName, 0, 0);
  end;

  if not Result and (FEngine.FWriteStream <> nil) then
    FEngine.Error(FError.AsText);
end;

function TLyThread.ExecuteFileFrom(StartParamIndex: integer): boolean;
var
  I: integer;
  F, S: string;
begin
  I := Max(1, StartParamIndex);
  S := ParamStr(I);
  F := CompleteFileName(S);
  Check(F <> '', 'File does not exists', [S]);
  S := LyseeFileCode(S);
  Clear;
  FArgs.Add.AsString := F;
  for I := I + 1 to ParamCount do
    FArgs.Add.AsString := ParamStr(I);
  Result := Execute(S, F);
end;

function TLyThread.ExecuteFile(const FileName: string; const Args: array of string): boolean;
var
  I: integer;
  F, S: string;
begin
  F := CompleteFileName(FileName);
  Check(F <> '', 'File does not exists', [FileName]);
  S := LyseeFileCode(F);
  Clear;
  FArgs.Add.SetAsString(F);
  for I := 0 to Length(Args) - 1 do
    FArgs.Add.SetAsString(Args[I]);
  Result := Execute(S, F);
end;

procedure TLyThread.ExecuteMainFunc;
begin
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
      FMainParam := TLyParam.Create;
      try
        FCurrent := FMainParam;
        FCurrent.FThread := Self;
        FCurrent.FFunc := FMainFunc;
        FCurrent.FParams := FMainLocals;
        FCurrent.FResult := FResult;
        FCurrent.FToken := TLyToken.Create;
        FCurrent.FToken.FSym := syCall;
        FState := tsRunning;
        FMainFunc.FSTMTs.Execute(FCurrent);
      finally
        FCurrent := nil;
        FreeAndNil(FMainParam.FToken);
        FreeAndNil(FMainParam);
      end;
    end;
  finally
    FTerminated := true;
    FState := tsTerminated;
    FMainFunc.FSTMTs.Clear;
    if FExcepted then
      SetResult(nil) else FError.Clear;
    EndExecute;
  end;
end;

procedure TLyThread.ForWhileRepeatEnded;
begin
  if FState in [tsBreak, tsContinue] then
    FState := tsRunning;
end;

function TLyThread.LoadModuleFromFile(const FileName: string): TLyModule;
var
  F: string;
begin
  F := ExpandFileName(FileName);
  Result := FEngine.FindModuleByFileName(F);
  if Result = nil then
  begin
    CheckNotRunning;
    CheckNotCompiling;
    FEngine.FRollbacks := TList.Create;
    try
      F := ChangeFileExt(ExtractFileName(FileName), '');
      Result := LoadModule(F, FileName, false);
    except
      FEngine.Rollback;
      raise;
    end;
  end;
end;

procedure TLyThread.MarkSurvived;
var
  P: TLyParam;
begin
  P := FCurrent;
  while P <> nil do
  begin
    P.FResult.MarkForSurvive;
    P.FParams.MarkForSurvive;
    P := P.FPrev;
  end;
  FArgs.MarkForSurvive;
  FResult.MarkForSurvive;
  FMainLocals.MarkForSurvive;
end;

function TLyThread.Append(const Code: string; AModule: TLyModule): TLyFunc;
begin
  if AModule = nil then AModule := MainModule;
  Result := TLyParser.Create(AModule, FError).ParseAndFree(Code, true);
end;

function TLyThread.Resolve(const ID: string; Value: TLyValue): boolean;
begin
  Result := FEngine.Resolve(ID, Value);
  if not Result then
  begin
    Result := IsResolvingID(ID);
    if Result then
      if MatchID(ID, '__version') then Value.SetAsString(LSE_VERSION) else
      if MatchID(ID, '__error') then Value.Assign(my_error, FError) else
      if MatchID(ID, '__module') then Value.Assign(my_module, CodeModule) else
      if MatchID(ID, '__file') then Value.SetAsString(CodeModule.FFileName) else
      if MatchID(ID, '__func') then Value.Assign(my_func, CodeFunc) else
      if MatchID(ID, '__prmc') then Value.SetAsInteger(FCurrent.FPrmc) else
      if MatchID(ID, '__args') then Value.Assign(my_array, FArgs) else
      if MatchID(ID, '__argc') then Value.SetAsInteger(FArgs.Count) else
      if MatchID(ID, '__dir') then Value.SetAsString(GetCurrentDir) else
      if MatchID(ID, '__modules') then Value.Assign(my_array, FEngine.FModules.ToList) else
      if MatchID(ID, '__libs') then Value.Assign(my_array, my_modules.ToList) else
      if MatchID(ID, '__hilights') then Value.SetAsString(Hilights) else
      if MatchID(ID, '__main_func') then Value.Assign(my_func, FMainFunc) else
      if MatchID(ID, '__in_main_func') then Value.SetAsBoolean(CodeFunc.IsMainFunc) else
      if MatchID(ID, '__kernel') then Value.SetAsString(my_kernel) else
      if MatchID(ID, '__kernel_path') then Value.SetAsString(my_knpath) else
      if MatchID(ID, '__kernel_dir') then Value.SetAsString(my_kndir) else
      if MatchID(ID, '__home') then Value.SetAsString(my_home) else
      if MatchID(ID, '__program') then Value.SetAsString(my_program) else
      if MatchID(ID, '__tmpath') then Value.SetAsString(my_tmpath) else
      if MatchID(ID, '__keywords') then Value.SetAsString(Keywords) else
      if MatchID(ID, '__module_path') then Value.SetAsString(my_module_path) else
        Result := false;
  end;
end;

function TLyThread.GetCodeFunc: TLyFunc;
var
  P: TLyParam;
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

function TLyThread.GetCodeModule: TLyModule;
var
  F: TLyFunc;
begin
  F := CodeFunc;
  if F <> nil then
    Result := F.FModule else
    Result := nil;
end;

function TLyThread.GetMainFunc: TLyFunc;
begin
  if FMainFunc = nil then
    FMainFunc := TLyFunc.Create(FName, GetMainModule, nil);
  Result := FMainFunc;
end;

function TLyThread.GetMainModule: TLyModule;
begin
  if FMainModule = nil then
  begin
    FMainModule := TLyModule.CreateEx(FName, FEngine);
    FMainModule.FThread := Self;
  end;
  Result := FMainModule;
end;

function TLyThread.GetReady: boolean;
begin
  Result := (FState = tsReady);
end;

function TLyThread.GetRunning: boolean;
begin
  Result := (FState >= tsRunning);
end;

function TLyThread.GetStatusOK: boolean;
begin
  Result := (FState = tsRunning) and not (FTerminated or FExcepted);
end;

function TLyThread.GetTerminated: boolean;
begin
  Result := FTerminated or not Running;
end;

function TLyThread.GetTerminating: boolean;
begin
  Result := FTerminated and Running;
end;

procedure TLyThread.SetResult(Value: TLyValue);
begin
  FResult.Assign(Value);
end;

function TLyThread.SyntaxCheck(const Code, FileURL: string): boolean;
begin
  Clear;
  FEngine.FRollbacks := TList.Create;
  try
    if FileURL <> '' then
      MainModule.FFileName := FileURL;
    TLyParser.Create(MainModule, FError).ParseAndFree(Code);
    FreeAndNil(FEngine.FRollbacks);
    FState := tsReady;
    Result := true;
  except
    FEngine.Rollback;
    Result := false;
  end;
end;

function TLyThread.LoadModuleFromCode(const ID, Code: string): TLyModule;
var
  F: boolean;
begin
  Check(IsID(ID), 'Invalid module name: %s', [ID]);

  Result := FEngine.FindModuleByName(ID);
  if Result = nil then
  begin
    Result := TLyModule.CreateEx(ID, FEngine);
    F := true;
  end
  else F := false;

  if Code <> '' then
  begin
    CheckNotRunning;
    CheckNotCompiling;
    FEngine.FRollbacks := TList.Create;
    try
      Append(Code, Result);
    except
      if F then FreeAndNil(Result);
      FEngine.Rollback;
      Throw(FError.AsText);
    end;
  end;
end;

procedure TLyThread.Terminate;
begin
  FTerminated := true;
end;

function TLyThread.LoadModule(const ID, FileName: string; Quiet: boolean): TLyModule;
var
  F, X: string;
begin
  Check(IsID(ID), 'Invalid module name: %s', [ID]);
  Result := FEngine.FindModuleByName(ID);
  if Result <> nil then Exit;

  if FileName <> '' then
  begin
    Check(FileExists(FileName), 'File not found: %s', [FileName]);
    F := ExpandFileName(FileName);
    Result := FEngine.FindModuleByFileName(F);
    if Result <> nil then Exit;
  end
  else F := FEngine.LocateModuleFile(ID);

  X := ExtractFileExt(F);
  if MatchID(X, LSE_FILEEXT) then
  begin
    Result := TLyModule.CreateEx(ID, FEngine);
    try
      Result.FFileName := F;
      Append(TrimRight(LyseeFileCode(F)), Result);
    except
      FreeAndNil(Result);
      if Quiet then raise else Throw(FError.AsText);
    end;
  end
  else
  if Assigned(my_loadlib) then
  begin
    Result := my_loadlib(ID, F);
    if Result <> nil then
      Result.FFileName := F;
  end;
end;

{ TLyError }

procedure TLyError.Clear;
begin
  FErrID := '';
  FMsg := '';
  FModule := '';
  FFileName := '';
  FRow := 0;
  FCol := 0;
end;

constructor TLyError.Create(AThread: TLyThread);
begin
  FThread := AThread;
end;

function TLyError.AsText(Simple: boolean): string;
begin
  if (FErrID <> '') and (FMsg <> '') then
  begin
    Result := FileName;
    if Result <> '' then
      if Simple then
        Result := ' file=' + ExtractFileName(Result) else
        Result := ' file=' + Result;
    Result := Format('[%s]: (module=%s%s row=%d col=%d) %s',
      [FErrID, FModule, Result, FRow + 1, FCol + 1, FMsg]);
  end
  else Result := '';
end;

procedure TLyError.Runtime(const Msg, Module, FileName: string; Row, Col: integer);
begin
  FErrID := 'RuntimeError';
  FMsg := Msg;
  FModule := Module;
  FFileName := FileName;
  FRow := Row;
  FCol := Col;
  FThread.FExcepted := true;
end;

procedure TLyError.Syntax(const Msg, Module, FileName: string; Row, Col: integer);
begin
  FThread.FExcepted := true;
  FErrID := 'SyntaxError';
  FMsg := Msg;
  FModule := Module;
  FFileName := FileName;
  FRow := Row;
  FCol := Col;
  Abort;
end;

function TLyError.ToString: string;
begin
  Result := AsText;
end;

{ TLyType }

function TLyType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then Result := 1 else Result := 0;
end;

function TLyType.Inherite(const AName: string; AModule: TLyModule): TLyType;
begin
  Result := InheriteClassType.Create(AName, AModule);
  Result.SetParent(Self);
end;

function TLyType.InheriteClassType: TLyTypeClass;
begin
  if (FCreater <> nil) and (FStyle = tsObject) and (FMainType = nil) then
    Result := TLyTypeClass(ClassType) else
    Result := nil;
end;

constructor TLyType.Create(const AName: string; AModule: TLyModule);
begin
  FModule := AModule;
  if FModule <> nil then
    FModule.FTypeList.Add(Self);
  FName := AName;
  FStyle := tsObject;
  FMethods := TLyFuncList.Create;
end;

function TLyType.Define(const AType: TLyType;
  const IndexNames: array of string;
  const IndexTypes: array of TLyType;
  const GetProc, SetProc: TLyObjectProc): boolean;
begin
  Result := (FGetFunc = nil);
  if Result then
  begin
    FGetFunc := Method(LSE_GETIV, AType, IndexNames, IndexTypes, GetProc);
    Result := (FGetFunc <> nil);
    if Result and Assigned(SetProc) then
    begin
      FSetFunc := Method(LSE_SETIV, my_nil, IndexNames, IndexTypes, SetProc);
      FSetFunc.FParams.Add('NewValue', AType);
    end;
  end;
end;

function TLyType.DefValue: pointer;
begin
  Result := nil;
end;

destructor TLyType.Destroy;
begin
  if FModule <> nil then
    FModule.FTypeList.Remove(Self);
  FreeAndNil(FMethods);
  inherited;
end;

procedure TLyType.ExecAdd(LValue, RValue: TLyValue);
begin
  if MatchStringType(LValue.FType) or MatchStringType(RValue.FType) then
    LValue.AsString := LValue.AsString + RValue.AsString else
    ErrorOperation(Self, '+', RValue.FType);
end;

function TLyType.Compare(LValue, RValue: TLyValue): TLyCompare;
begin
  if ((Self = my_nil) or (RValue.FType = Self) or (RValue.FType = my_nil)) and
    (LValue.FData = RValue.FData) then
      Result := crEqual else
      Result := crDiff;
end;

procedure TLyType.ExecDec(LValue, RValue: TLyValue);
begin
  ErrorOperation(Self, '-', RValue.FType);
end;

procedure TLyType.ExecDiv(LValue, RValue: TLyValue);
begin
  ErrorOperation(Self, 'div', RValue.FType);
end;

procedure TLyType.ExecDivf(LValue, RValue: TLyValue);
begin
  ErrorOperation(Self, '/', RValue.FType);
end;

procedure TLyType.ExecIn(LValue, RValue: TLyValue);
begin
  LValue.AsBoolean := RValue.FType.Has(RValue.FData, LValue);
end;

procedure TLyType.ExecLike(LValue, RValue: TLyValue);
begin
  LValue.AsBoolean := false;
end;

procedure TLyType.ExecMod(LValue, RValue: TLyValue);
begin
  ErrorOperation(Self, 'mod', RValue.FType);
end;

procedure TLyType.ExecMul(LValue, RValue: TLyValue);
begin
  ErrorOperation(Self, '*', RValue.FType);
end;

procedure TLyType.ExecNeg(Value: TLyValue);
begin
  ErrorOperation('-', Self);
end;

procedure TLyType.ExecNot(Value: TLyValue);
begin
  Value.AsBoolean := not Value.AsBoolean;
//ErrorOperation('not', Self);
end;

procedure TLyType.ExecShl(LValue, RValue: TLyValue);
begin
  ErrorOperation(Self, 'shl', RValue.FType);
end;

procedure TLyType.ExecShr(LValue, RValue: TLyValue);
begin
  ErrorOperation(Self, 'shr', RValue.FType);
end;

procedure TLyType.ExecXor(LValue, RValue: TLyValue);
begin
  ErrorOperation(Self, 'xor', RValue.FType);
end;

function TLyType.FindMethod(const AName: string; AObj: pointer): TLyFunc;
var
  P: TLyType;
begin
  Result := FMethods.Find(AName);
  if Result = nil then
  begin
    P := FParent;
    while (P <> nil) and (Result = nil) do
    begin
      Result := P.FMethods.Find(AName);
      P := P.FParent;
    end;
  end;
end;

function TLyType.Get(Obj: pointer; const AName: string; Value: TLyValue): boolean;
var
  T, P: TLyType;
  F: TLyFunc;
begin
  Result := true;

  if Self = my_type then
  begin
    T := TLyType(Obj);
    if T = nil then T := my_nil;

    F := T.FindMethod(AName, nil);
    if F <> nil then
    begin
      Value.Assign(my_func, F);
      Exit;
    end;

    if MatchID(AName, 'ToString') then
    begin
      Value.SetAsString(T.FullName);
      Exit;
    end;
  end
  else
  begin
    if (Self = my_module) and (Obj <> nil) then
      if TLyModule(Obj).FindSave(AName, Value) then
        Exit;

    if MatchID(AName, 'ToString') then
    begin
      Value.SetAsString(AsString(Obj));
      Exit;
    end;

    if FParent <> nil then
    begin
      P := FParent;
      repeat
        if MatchID(AName, P.FName) then
        begin
          Value.Assign(P, Obj);
          Exit;
        end;
        P := P.FParent;
      until P = nil;
    end;

    T := Self;
  end;

  if MatchID(AName, 'ClassName') then Value.SetAsString(T.FName) else
  if MatchID(AName, 'ClassType') then Value.SetAsType(T) else
  if MatchID(AName, 'ClassInfo') then Value.SetAsType(T) else
  if MatchID(AName, 'ClassParent') then Value.SetAsType(T.FParent) else
    Result := false;
end;

function TLyType.GetFullName: string;
begin
  Result := FModule.FName + '.' + FName
end;

function TLyType.InstanceClass: TClass;
begin
  Result := nil;
end;

procedure TLyType.MarkForSurvive(Obj: pointer);
begin
  { do nothing }
end;

function TLyType.Generate(Obj: pointer): TLyGenerate;
begin
  Result := nil;
end;

function TLyType.GetLength(Obj: pointer): int64;
begin
  Result := 0;
end;

function TLyType.IsEnumType: boolean;
begin
  Result := (FStyle = tsEnum);
end;

function TLyType.IsFinalType: boolean;
begin
  Result := (InheriteClassType = nil);
end;

function TLyType.IsEnumSetType: boolean;
begin
  Result := (FStyle = tsEnumSet);
end;

function TLyType.IsChildOf(AType: TLyType): boolean;
var
  P: TLyType;
begin
  Result := (AType <> nil);
  if Result then
  begin
    P := FParent;
    while P <> nil do
    begin
      if P = AType then Exit;
      P := P.FParent;
    end;
    Result := false;
  end;
end;

function TLyType.IsTypeOf(AType: TLyType): boolean;
begin
  Result := (Self = AType) or IsChildOf(AType);
end;

function TLyType.MatchBranch(ABranch: TLyType): boolean;
begin
  if FMainType <> nil then
    Result := (FMainType = ABranch) or (FMainType = ABranch.FMainType) else
    Result := (Self = ABranch) or (Self = ABranch.FMainType);
end;

function TLyType.Method(const AName: string;
                        const AProc: TLyObjectProc): TLyFunc;
begin
  Result := Method(AName, my_nil, [], [], AProc);
end;

procedure TLyType.MyCreate(const Param: TLyParam);
begin
  Param.Result.Assign(Self, Param[0].Data);
end;

function TLyType.CreateInstance: pointer;
begin
  Result := nil;
end;

procedure TLyType.NewPropProc(const Param: TLyParam);
begin
  Param.Result.Assign(Param.FFunc.FResultType, Param.FParams[0].FData);
end;

function TLyType.Branch(const PropName: string): TLyType;
begin
  Result := TLyTypeClass(ClassType).Create(FName + '.' + PropName, nil);
  Result.FMainType := Self;
  Result.FModule := FModule;
  Define(PropName, Result, {$IFDEF FPC}@{$ENDIF}NewPropProc);
end;

function TLyType.Method(const AName: string;
                        const ParamNames: array of string;
                        const ParamTypes: array of TLyType;
                        const AProc: TLyObjectProc): TLyFunc;
begin
  Result := Method(AName, my_nil, ParamNames, ParamTypes, AProc);
end;

function TLyType.Method(const AName: string;
                        const AType: TLyType;
                        const ParamNames: array of string;
                        const ParamTypes: array of TLyType;
                        const AProc: TLyObjectProc): TLyFunc;
var
  I: integer;
begin
  if (FMethods.Find(AName) = nil) and
     (not MatchID(AName, LSE_CREATE) or (AType = Self)) then
  begin
    Result := TLyFunc.CreateMethod(AName, Self, TLyProc(nil));
    Result.FResultType := AType;
    Result.FMethod := AProc;
    Result.FParams.Add('Self', Self);
    if MatchID(AName, LSE_CREATE) then
      FCreater := Result;
    for I := 0 to Length(ParamNames) - 1 do
      Result.FParams.Add(ParamNames[I], ParamTypes[I]);
  end
  else Result := nil;
end;

function TLyType.Method(const AName: string;
                        const AType: TLyType;
                        const AProc: TLyObjectProc): TLyFunc;
begin
  Result := Method(AName, AType, [], [], AProc);
end;

function TLyType.Add(Obj: pointer; Value: TLyValue): integer;
begin
  Throw('can not add item into: %s', [FName]);
  Result := -1;
end;

function TLyType.AsBoolean(Obj: pointer): boolean;
begin
  Result := (Obj <> nil);
end;

function TLyType.AsChar(Obj: pointer): char;
begin
  ErrorConvert(Self, my_char);
  Result := #0;
end;

function TLyType.AsFloat(Obj: pointer): double;
begin
  ErrorConvert(Self, my_float);
  Result := 0;
end;

function TLyType.AsInteger(Obj: pointer): int64;
begin
  ErrorConvert(Self, my_int);
  Result := 0;
end;

function TLyType.AsCurrency(Obj: pointer): currency;
begin
  ErrorConvert(Self, my_curr);
  Result := 0;
end;

function TLyType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TObject(Obj).ToString else
    Result := '';
end;

function TLyType.AsTime(Obj: pointer): TDateTime;
begin
  ErrorConvert(Self, my_time);
  Result := 0;
end;

function TLyType.Clear(Obj: pointer): boolean;
begin
  Result := false;
end;

function TLyType.Has(Obj: pointer; Value: TLyValue): boolean;
begin
  Result := false;
end;

function TLyType.Prototype(const AName: string): string;
begin
  if Self <> my_variant then
    Result := AName + ':' + FName else
    Result := AName;
end;

function TLyType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then Result := 1 else Result := 0;
end;

function TLyType.Define(const AType: TLyType; const IndexName: string;
  const IndexType: TLyType; const GetProc, SetProc: TLyObjectProc): boolean;
begin
  Result := Define(AType, [IndexName], [IndexType], GetProc, SetProc);
end;

procedure TLyType.Setup;
begin
  { add your class methods and properties here }
end;

procedure TLyType.SetDefValue(Value: TLyValue);
begin
  Value.Assign(Self, DefValue);
end;

procedure TLyType.SetParent(Value: TLyType);
begin
  if (FParent = nil) and (Value <> nil) then
    FParent := Value;
end;

function TLyType.Put(Obj: pointer; const AName: string; Value: TLyValue): boolean;
begin
  Result := false;
end;

procedure TLyType.Validate(Obj: pointer);
begin
  if Obj = nil then
    Throw('invalid %s instance: nil', [FName]);
end;

procedure TLyType.Convert(Value: TLyValue);
begin
  if Self <> Value.FType then
    if Value.FType.IsChildOf(Self) then
      Value.FType := Self else
      ErrorConvert(Value.FType, Self);
end;

function TLyType.Define(const AProp: string; const AType: TLyType;
  const GetProc, SetProc: TLyObjectProc): boolean;
begin
  Result := (Method(AProp, AType, [], [], GetProc) <> nil);
  if Result and Assigned(SetProc) then
    Method('Set' + AProp, my_nil, ['NewValue'], [AType], SetProc);
end;

{ TLyAgentType }

function TLyAgentType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyAgent(Obj).DecRefcount else
    Result := 0;
end;

function TLyAgentType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyAgent(Obj).IncRefcount else
    Result := 0;
end;

{ TLyBasicType }

constructor TLyBasicType.Create(const AName: string; AModule: TLyModule);
begin
  inherited;
  FStyle := tsBasic;
end;

{ TLyVariantType }

constructor TLyVariantType.Create(const AName: string; AModule: TLyModule);
begin
  inherited;
  FStyle := tsVariant;
end;

procedure TLyVariantType.SetDefValue(Value: TLyValue);
begin
  Value.Clear;
end;

procedure TLyVariantType.Convert(Value: TLyValue);
begin
  { do nothing }
end;

{ TLyNilType }

function TLyNilType.AsBoolean(Obj: pointer): boolean;
begin
  Result := false;
end;

function TLyNilType.AsChar(Obj: pointer): char;
begin
  Result := #0;
end;

function TLyNilType.AsFloat(Obj: pointer): double;
begin
  Result := 0;
end;

function TLyNilType.AsInteger(Obj: pointer): int64;
begin
  Result := 0;
end;

function TLyNilType.AsCurrency(Obj: pointer): currency;
begin
  Result := 0;
end;

function TLyNilType.AsString(Obj: pointer): string;
begin
  Result := '';
end;

function TLyNilType.AsTime(Obj: pointer): TDateTime;
begin
  Result := 0;
end;

procedure TLyNilType.Convert(Value: TLyValue);
begin
  Value.Clear;
end;

constructor TLyNilType.Create(const AName: string; AModule: TLyModule);
begin
  inherited;
  FStyle := tsNil;
end;

procedure TLyNilType.SetDefValue(Value: TLyValue);
begin
  Value.Clear;
end;

{ TLyObjectType }

function TLyObjectType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyList(Obj).ToString else
    Result := '';
end;

function TLyObjectType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyList(Obj).DecRefcount else
    Result := 0;
end;

function TLyObjectType.FindMethod(const AName: string; AObj: pointer): TLyFunc;
var
  V: TLyValue;
  F: TLyFunc;
  T: TLyType;
begin
  Result := inherited;
  if (Result = nil) and (AObj <> nil) then
  begin
    V := TLyList(AObj).Values[AName];
    if (V <> nil) and (V.VType = my_func) and (V.FData <> nil) then
    begin
      F := TLyFunc(V.FData);
      if F.Params.ParamCount > 0 then
      begin
        T := F.Params[0].ValueType;
        if (T = my_variant) or T.IsTypeOf(Self) then
           Result := F;
      end;
    end;
  end;
end;

function TLyObjectType.Get(Obj: pointer; const AName: string; Value: TLyValue): boolean;
var
  I: integer;
  L: TLyList;
begin
  if Obj <> nil then
  begin
    L := TLyList(Obj);
    I := L.IndexOf(AName);
    if I >= 0 then
    begin
      Value.Assign(L[I]);
      Result := true;
      Exit;
    end;
  end;
  Result := inherited;
end;

function TLyObjectType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyList(Obj).IncRefcount else
    Result := 0;
end;

function TLyObjectType.InstanceClass: TClass;
begin
  Result := TLyList;
end;

procedure TLyObjectType.MarkForSurvive(Obj: pointer);
begin
  if Obj <> nil then TLyList(Obj).MarkForSurvive;
end;

function TLyObjectType.MatchObject(T: TLyType): boolean;
begin
  Result := (T = my_variant) or (T is TLyObjectType);
end;

function TLyObjectType.CreateInstance: pointer;
begin
  Result := TLyList.Create;
end;

function TLyObjectType.Put(Obj: pointer; const AName: string; Value: TLyValue): boolean;
begin
  if (Obj <> nil) and IsID(AName) then
  begin
    TLyList(Obj).Put(AName, Value);
    Result := true;
  end
  else Result := inherited;
end;

procedure TLyObjectType.Setup;
begin
  Method(LSE_CREATE, Self, {$IFDEF FPC}@{$ENDIF}MyCreate);
  inherited;
end;

{ TLyEnumType }

procedure TLyEnumType.AddItems(const Names: array of string);
var
  I: integer;
begin
  if Length(FItems) = 0 then
  begin
    SetLength(FItems, Length(Names));
    for I := 0 to Length(FItems) - 1 do
    begin
      FItems[I] := TLyEnumItem.Create;
      FItems[I].FParent := Self;
      FItems[I].FName := Names[I];
      FItems[I].FValue := I;
//    FModule.FConsts.Add(Names[I]).SetTOA(Self, FItems[I]);
    end;
  end;
end;

constructor TLyEnumType.Create(const AName: string; AModule: TLyModule);
begin
  inherited;
  FStyle := tsEnum;
  FModule.FEnumTypes.Add(Self);
end;

destructor TLyEnumType.Destroy;
var
  I: integer;
begin
  FModule.FEnumTypes.Remove(Self);
  for I := GetCount - 1 downto 0 do
    FreeAndNil(FItems[I]);
  SetLength(FItems, 0);
  inherited;
end;

function TLyEnumType.Compare(LValue, RValue: TLyValue): TLyCompare;
begin
  if Self = RValue.FType then
    Result := CompareInteger(LValue.AsEnum.FValue, RValue.AsEnum.FValue) else
    Result := crDiff;
end;

function TLyEnumType.Find(OrdValue: integer): TLyEnumItem;
begin
  if (OrdValue >= 0) and (OrdValue < Length(FItems)) then
    Result := FItems[OrdValue] else
    Result := nil;
end;

function TLyEnumType.Find(const AName: string): TLyEnumItem;
var
  I: integer;
begin
  for I := 0 to GetCount - 1 do
  begin
    Result := GetItem(I);
    if MatchID(AName, Result.FName) then Exit;
  end;
  Result := nil;
end;

function TLyEnumType.GetCount: integer;
begin
  Result := Length(FItems);
end;

function TLyEnumType.DefValue: pointer;
begin
  Result := FItems[0];
end;

function TLyEnumType.GetItem(Index: integer): TLyEnumItem;
begin
  Result := FItems[Index];
end;

function TLyEnumType.FindByName(const AName: string): TLyEnumItem;
begin
  Result := Find(AName);
  if Result = nil then
    Throw('%s value(name=''%s'') not exists', [Name, AName]);
end;

function TLyEnumType.FindByValue(OrdValue: integer): TLyEnumItem;
begin
  Result := Find(OrdValue);
  if Result = nil then
    Throw('Enum value(ordinal=%d) of type %s does not exists', [OrdValue, FName]);
end;

procedure TLyEnumType.SetValue(Value: TLyValue; const AName: string);
begin
  Value.Assign(Self, FindByName(AName));
end;

function TLyEnumType.NewEnumSetType(const AName: string): TLyEnumSetType;
begin
  Check(not FModule.Find(AName), 'Can not declare enumset type: %s', [AName]);
  Result := TLyEnumSetType.Create(AName, FModule);
  Result.FSource := Self;
end;

procedure TLyEnumType.SetValue(Value: TLyValue; OrdValue: integer);
begin
  Value.Assign(Self, FindByValue(OrdValue));
end;

procedure TLyEnumType.SetValue(Value: TLyValue; Item: TLyEnumItem);
begin
  if Item = nil then
    Value.Assign(Self, DefValue) else
    Value.Assign(Self, Item);
end;

function TLyEnumType.AsInteger(Obj: pointer): int64;
begin
  if Obj = nil then
    Result := TLyEnumItem(DefValue).FValue else
    Result := TLyEnumItem(Obj).FValue;
end;

function TLyEnumType.AsString(Obj: pointer): string;
begin
  if Obj = nil then
    Result := TLyEnumItem(DefValue).FName else
    Result := TLyEnumItem(Obj).FName;
end;

procedure TLyEnumType.Convert(Value: TLyValue);
begin
  if Value.FType <> Self then
    if Value.FType = my_int then
      SetValue(Value, Value.AsInteger) else
      inherited;
end;

function TLyEnumType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then Result := 1 else Result := 0;
end;

function TLyEnumType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then Result := 1 else Result := 0;
end;

{ TLyEnumSet }

function TLyEnumSet.GetSource: TLyEnumType;
begin
  Result := FParent.FSource;
end;

function TLyEnumSet.IsSet(Index: integer): boolean;
begin
  Result := (Self <> nil) and
            (Index >= 0) and
            (Index < Length(FSets)) and
            FSets[Index];
end;

function TLyEnumSet.IsSet(Item: TLyEnumItem): boolean;
begin
  Result := (Self <> nil) and
            (Item <> nil) and
            (Item.FParent = FParent.FSource) and
            FSets[Item.FValue];
end;

function TLyEnumSet.GetCount: integer;
begin
  Result := Length(FSets);
end;

function TLyEnumSet.Get(Index: integer): boolean;
begin
  Result := FSets[Index];
end;

procedure TLyEnumSet.Put(Index: integer; Value: boolean);
begin
  FSets[Index] := Value;
end;

destructor TLyEnumSet.Destroy;
begin
  SetLength(FSets, 0);
  inherited;
end;

procedure TLyEnumSet.Assign(A: TLyList);
var
  T: TLyEnumType;
  I: integer;
  V: TLyValue;
begin
  Clear;
  if A <> nil then
  begin
    T := FParent.FSource;
    for I := 0 to A.Count - 1 do
    begin
      V := A[I];
      if V.FType = T then
        FSets[V.AsEnum.FValue] := true else
      if V.FType = my_int then
        FSets[V.AsInteger] := true else
        ErrorConvert(V.FType, T);
    end;
  end;
end;

function TLyEnumSet.ToString: string;
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

function TLyEnumSet.AsArray: TLyList;
var
  I: integer;
begin
  Result := TLyList.Create;
  for I := 0 to Length(FSets) - 1 do
    if FSets[I] then
      Result.Add.Assign(FParent.FSource, FParent.FSource[I]);
end;

function TLyEnumSet.AsBoolean: boolean;
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

procedure TLyEnumSet.Clear;
begin
  FillChar(FSets[0], Length(FSets) * sizeof(boolean), 0);
end;

function TLyEnumSet.Equal(S: TLyEnumSet): boolean;
var
  I: integer;
begin
  if S = nil then S := TLyEnumSet(FParent.DefValue);
  Result := (S = Self);
  if not Result then
  begin
    for I := 0 to Length(FSets) - 1 do
      if FSets[I] <> S.FSets[I] then
        Exit;
    Result := true;
  end;
end;

function TLyEnumSet.Add(S: TLyEnumSet): TLyEnumSet;
var
  I: integer;
begin
  Result := FParent.NewEnumSet;
  for I := 0 to Length(FSets) - 1 do
    Result.FSets[I] := FSets[I] or S.FSets[I];
end;

function TLyEnumSet.Dec(S: TLyEnumSet): TLyEnumSet;
var
  I: integer;
begin
  Result := FParent.NewEnumSet;
  for I := 0 to Length(FSets) - 1 do
    Result.FSets[I] := FSets[I] and not S.FSets[I];
end;

function TLyEnumSet.Mul(S: TLyEnumSet): TLyEnumSet;
var
  I: integer;
begin
  Result := FParent.NewEnumSet;
  for I := 0 to Length(FSets) - 1 do
    Result.FSets[I] := FSets[I] and S.FSets[I];
end;

function TLyEnumSet.NotAll: TLyEnumSet;
var
  I: integer;
begin
  Result := FParent.NewEnumSet;
  for I := 0 to Length(FSets) - 1 do
    Result.FSets[I] := not FSets[I];
end;

{ TLyEnumSetType }

function TLyEnumSetType.DefValue: pointer;
begin
  if FDefValue = nil then
  begin
    FDefValue := NewEnumSet;
    FDefValue.IncRefcount;
  end;
  Result := FDefValue;
end;

function TLyEnumSetType.Has(Obj: pointer; Value: TLyValue): boolean;
begin
  Result := (Obj <> nil);
  if Result then
    if Value.FType = FSource then
      Result := TLyEnumSet(Obj).IsSet(Value.GetEnum(nil)) else
    if Value.FType = my_int then
      Result := TLyEnumSet(Obj).IsSet(Value.AsInteger) else
      Result := false;
end;

function TLyEnumSetType.IncRefcount(Obj: pointer): integer;
begin
  if Obj = nil then Result := 0 else
  if Obj = pointer(FDefValue) then Result := 1 else
    Result := TLyEnumSet(Obj).IncRefcount;
end;

function TLyEnumSetType.DecRefcount(Obj: pointer): integer;
begin
  if Obj = nil then Result := 0 else
  if Obj = pointer(FDefValue) then Result := 1 else
    Result := TLyEnumSet(Obj).DecRefcount;
end;

function TLyEnumSetType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyEnumSet(Obj).ToString else
    Result := '[]';
end;

function TLyEnumSetType.AsBoolean(Obj: pointer): boolean;
begin
  Result := (Obj <> nil) and TLyEnumSet(Obj).Asboolean;
end;

procedure TLyEnumSetType.Convert(Value: TLyValue);
var
  S: TLyEnumSet;
  L: TLyList;
begin
  if Value.FType <> Self then
    if Value.FType.IsTypeOf(my_array) then
    begin
      L := TLyList(Value.Data);
      if (L <> nil) and (L.Count > 0) then
      begin
        S := NewEnumSet;
        try
          S.Assign(L);
          Value.Assign(Self, S);
        except
          S.Free;
          raise;
        end;
      end
      else SetDefValue(Value);
    end
    else inherited;
end;

constructor TLyEnumSetType.Create(const AName: string; AModule: TLyModule);
begin
  inherited;
  FStyle := tsEnumSet;
end;

destructor TLyEnumSetType.Destroy;
begin
  FreeAndNil(FDefValue);
  inherited;
end;

procedure TLyEnumSetType.ExecAdd(LValue, RValue: TLyValue);
begin
  if Self = RValue.FType then
    LValue.Assign(Self, LValue.AsEnumSet.Add(RValue.AsEnumSet)) else
    inherited;
end;

function TLyEnumSetType.Compare(LValue, RValue: TLyValue): TLyCompare;
begin
  if (Self = RValue.FType) and LValue.AsEnumSet.Equal(RValue.AsEnumSet) then
    Result := crEqual else
    Result := crDiff;
end;

procedure TLyEnumSetType.ExecDec(LValue, RValue: TLyValue);
begin
  if Self = RValue.FType then
    LValue.Assign(Self, LValue.AsEnumSet.Dec(RValue.AsEnumSet)) else
    inherited;
end;

procedure TLyEnumSetType.ExecMul(LValue, RValue: TLyValue);
begin
  if Self = RValue.FType then
    LValue.Assign(Self, LValue.AsEnumSet.Mul(RValue.AsEnumSet)) else
    inherited;
end;

procedure TLyEnumSetType.ExecNot(Value: TLyValue);
begin
  Value.Assign(Self, Value.AsEnumSet.NotAll);
end;

procedure TLyEnumSetType.SetValue(Value: TLyValue; ASet: TLyEnumSet);
begin
  if ASet = nil then
    Value.Assign(Self, DefValue) else
    Value.Assign(Self, ASet);
end;

function TLyEnumSetType.NewEnumSet: TLyEnumSet;
begin
  Result := TLyEnumSet.Create;
  Result.FParent := Self;
  SetLength(Result.FSets, FSource.Count);
  Result.Clear;
end;

{ TLyValue }

procedure TLyValue.MarkForSurvive;
begin
  FType.MarkForSurvive(FData);
end;

function TLyValue.NewArray: TLyList;
begin
  Result := TLyList.Create;
  Assign(my_array, Result);
end;

function TLyValue.GetAsFileName: string;
begin
  if FType = my_string then
    Result := SetPD(Trim(GetAsString)) else
    Result := '';
end;

function TLyValue.GetFunc: TLyFunc;
begin
  Result := TLyFunc(GetData(my_func));
end;

function TLyValue.GetHost(var AObj): boolean;
var
  A: TLyAgent;
begin
  Result := GetSelf(A);
  if Result then
  begin
    if A.Host = nil then
      Throw('invalid %s host object: nil', [FType.FName]);
    pointer(AObj) := A.Host;
  end;
end;

function TLyValue.GetAt(const Prop: string; Outv: TLyValue): TLyGetAt;
var
  F: TLyFunc;
begin
  F := FType.FindMethod(Prop, FData);
  if F <> nil then
  begin
    Outv.Assign(my_func, F);
    Result := gaMethod;
  end
  else
  if FType.Get(FData, Prop, Outv) then
    Result := gaData else
    Result := gaNone;
end;

function TLyValue.GetModule: TLyModule;
begin
  Result := TLyModule(GetData(my_module));
end;

procedure TLyValue.Assign(Value: TLyValue);
begin
  if Value <> nil then
    Assign(Value.FType, Value.FData) else
    Clear;
end;

function TLyValue.Compare(Value: TLyValue): TLyCompare;
begin
  Result := FType.Compare(Self, Value);
end;

function TLyValue.Compare(Value: TLyValue; Wanted: TLyCompares): boolean;
begin
  Result := (Compare(Value) in Wanted);
end;

procedure TLyValue.Convert(AType: TLyType);
begin
  AType.Convert(Self);
end;

constructor TLyValue.Create;
begin
  FType := my_nil;
end;

destructor TLyValue.Destroy;
begin
  Clear;
  inherited;
end;

procedure TLyValue.ExecAdd(Value: TLyValue);
begin
  FType.ExecAdd(Self, Value);
end;

procedure TLyValue.ExecAs(Value: TLyValue);
begin
  if Value.FType <> my_type then
    Throw('invalid as operation: no type specified') else
    Value.AsType.Convert(Self);
end;

procedure TLyValue.ExecCompare(Value: TLyValue; Wanted: TLyCompares);
begin
  SetAsBoolean(Compare(Value, Wanted));
end;

procedure TLyValue.ExecDec(Value: TLyValue);
begin
  FType.ExecDec(Self, Value);
end;

procedure TLyValue.ExecDiv(Value: TLyValue);
begin
  FType.ExecDiv(Self, Value);
end;

procedure TLyValue.ExecDivf(Value: TLyValue);
begin
  FType.ExecDivf(Self, Value);
end;

procedure TLyValue.ExecIn(Value: TLyValue);
begin
  SetAsBoolean(Value.FType.Has(Value.FData, Self));
//FType.ExecIn(Self, Value);
end;

procedure TLyValue.ExecIs(Value: TLyValue);
begin
  if Value.FType <> my_type then
    Throw('invalid is operation: no type specified') else
    SetAsBoolean(AsType.IsTypeOf(Value.AsType));
end;

procedure TLyValue.ExecLike(Value: TLyValue);
begin
  FType.ExecLike(Self, Value);
end;

procedure TLyValue.ExecMod(Value: TLyValue);
begin
  FType.ExecMod(Self, Value);
end;

procedure TLyValue.ExecMul(Value: TLyValue);
begin
  FType.ExecMul(Self, Value);
end;

procedure TLyValue.ExecNeg;
begin
  FType.ExecNeg(Self);
end;

procedure TLyValue.ExecNot;
begin
  FType.ExecNot(Self);
end;

procedure TLyValue.ExecSame(Value: TLyValue);
begin
  SetAsBoolean(Same(Value));
end;

procedure TLyValue.ExecShl(Value: TLyValue);
begin
  FType.ExecShl(Self, Value);
end;

procedure TLyValue.ExecShr(Value: TLyValue);
begin
  FType.ExecShr(Self, Value);
end;

procedure TLyValue.ExecXor(Value: TLyValue);
begin
  FType.ExecXor(Self, Value);
end;

function TLyValue.GetAsBoolean: boolean;
begin
  Result := FType.AsBoolean(FData);
end;

function TLyValue.GetAsChar: char;
begin
  Result := FType.AsChar(FData);
end;

function TLyValue.GetAsModule: TLyModule;
begin
  if FType <> my_module then
  begin
    Result := FType.FModule;
    if FData <> nil then
      if FType = my_func then
        Result := TLyFunc(FData).FModule else
      if FType = my_type then
        Result := TLyType(FData).FModule;
  end
  else Result := TLyModule(FData);
end;

function TLyValue.GetAsCurrency: currency;
begin
  Result := FType.AsCurrency(FData);
end;

function TLyValue.GetAsFloat: double;
begin
  Result := FType.AsFloat(FData);
end;

function TLyValue.GetAsInteger: int64;
begin
  Result := FType.AsInteger(FData);
end;

function TLyValue.GetAsEnumSet: TLyEnumSet;
begin
  Result := GetEnumSet(nil);
  if Result = nil then
    Throw('%s is not enum set type', [FType.FullName]);
end;

function TLyValue.GetAsEnum: TLyEnumItem;
begin
  Result := GetEnum(nil);
  if Result = nil then
    Throw('%s is not enum type', [FType.FullName]);
end;

procedure TLyValue.SetAsEnumSet(Value: TLyEnumSet);
begin
  if Value = nil then
    Throw('Invalid enum set: nil');
  Assign(Value.FParent, Value);
end;

function TLyValue.GetSelf(var AObj): boolean;
begin
  FType.Validate(FData);
  pointer(AObj) := FData;
  Result := true;
end;

function TLyValue.IsNil: boolean;
begin
  Result := (FType = my_nil) or (FType = nil);
end;

function TLyValue.GetAsString: string;
begin
  Result := FType.AsString(FData);
end;

procedure TLyValue.SetAsEnum(Value: TLyEnumItem);
begin
  if Value = nil then
    Throw('Invalid enum item: nil');
  Assign(Value.FParent, Value);
end;

function TLyValue.GetAsTime: TDateTime;
begin
  Result := FType.AsTime(FData);
end;

function TLyValue.GetAsType: TLyType;
begin
  if FType = my_type then
  begin
    Result := TLyType(FData);
    if Result = nil then
      Result := my_nil;
  end
  else Result := FType;
end;

function TLyValue.GetData(AType: TLyType): pointer;
begin
  if FType.IsTypeOf(AType) then
    Result := FData else
    Result := nil;
end;

function TLyValue.GetEnum(T: TLyEnumType): TLyEnumItem;
begin
  if FType.IsEnumType and ((T = nil) or (T = FType)) then
  begin
    Result := TLyEnumItem(FData);
    if Result = nil then
      Result := TLyEnumItem(TLyEnumType(FType).DefValue);
  end
  else Result := nil;
end;

function TLyValue.GetEnumSet(T: TLyEnumSetType): TLyEnumSet;
begin
  if FType.IsEnumSetType and ((T = nil) or (T = FType)) then
  begin
    Result := TLyEnumSet(FData);
    if Result = nil then
      Result := TLyEnumSet(TLyEnumSetType(FType).DefValue);
  end
  else Result := nil;
end;

procedure TLyValue.SetAsBoolean(Value: boolean);
begin
  Assign(my_bool, GetBooleanData(Value));
end;

procedure TLyValue.SetAsChar(Value: char);
begin
  Assign(my_char, GetCharData(Value));
end;

procedure TLyValue.SetAsCurrency(Value: currency);
begin
  Assign(my_curr, GetCurrencyData(Value));
end;

procedure TLyValue.SetAsInteger(Value: int64);
begin
  Assign(my_int, GetIntegerData(Value));
end;

procedure TLyValue.SetAsModule(Value: TLyModule);
begin
  Assign(my_module, Value);
end;

procedure TLyValue.SetAsString(const Value: string);
var
  S: TLyString;
begin
  S := TLyString.Create;
  S.Data := Value;
  Assign(my_string, S);
end;

procedure TLyValue.SetAsTime(Value: TDateTime);
begin
  Assign(my_time, GetTimeData(Value));
end;

procedure TLyValue.SetAsType(Value: TLyType);
begin
  Assign(my_type, Value);
end;

function TLyValue.Same(Value: TLyValue): boolean;
begin
  Result := (FType = Value.FType) and (Compare(Value) = crEqual);
end;

procedure TLyValue.Assign(Finded: PLyFind);
begin
  case Finded^.f_find of
    fiFunc  : Assign(my_func, Finded^.VFunc);
    fiType  : Assign(my_type, Finded^.VType);
    fiModule: Assign(my_module, Finded^.VModule);
    fiEnum  : Assign(Finded^.VEnum.FParent, Finded^.VEnum);
    fiValue : if Finded^.VValue.FType = my_string then
                SetAsString(Finded^.VValue.AsString) else
                Assign(Finded^.VValue);
    else Clear;
  end;
end;

procedure TLyValue.SetAsFloat(Value: double);
begin
  Assign(my_float, GetFloatData(Value));
end;

procedure TLyValue.Clear;
begin
  if FData <> nil then
  begin
    FType.DecRefcount(FData);
    FData := nil;
  end;
  FType := my_nil;
end;

procedure TLyValue.Assign(AType: TLyType; AData: pointer);
begin
  AType.IncRefcount(AData);
  FType.DecRefcount(FData);
  FType := AType;
  FData := AData;
end;

{ TLyParam }

procedure TLyParam.Error(const Msg: string);
var
  F: TLyFunc;
  M: TLyModule;
  S: string;
begin
  if not FThread.FExcepted then
  begin
    F := FFunc;
    if Msg = '' then
      S := F.Name + '() - ' + ExceptionStr else
      S := F.Name + '() - ' + Msg;
    if Assigned(F.FProc) then
    begin
      M := FThread.CodeModule;
      FThread.FError.Runtime(S, M.Name, M.FileName, FToken.FRow, FToken.FCol);
    end
    else FToken.Error(Self, S);
  end;
end;

procedure TLyParam.BeginExec(var Mark: integer; var Tmpv: TLyValue);
begin
  Mark := FParams.Count;
  Tmpv := FParams.Add;
end;

procedure TLyParam.BeginExec(var Mark: integer);
begin
  Mark := FParams.Count;
end;

procedure TLyParam.EndExec(Mark: integer);
var
  I: integer;
begin
  for I := FParams.Count - 1 downto Mark do
    FParams.Delete(I);
end;

procedure TLyParam.Error(const Msg: string; const Args: array of const);
begin
  Error(Format(Msg, Args));
end;

procedure TLyParam.ErrorChangeFunc(AFunc: TLyFunc);
begin
  Error('can not change function: ' + AFunc.FullName);
end;

procedure TLyParam.ErrorIndexType(AType: TLyType);
begin
  Error('Invalid index type: %s', [AType.Name]);
end;

function TLyParam.ExecFunc(Func: TLyFunc; Outv: TLyValue; Args: TLyList; var ErrStr: string): boolean;
begin
  try
    try
      Result := FToken.ExecFunc(Func, Self, Outv, Args);
      if not Result then
        ErrStr := FThread.Error.ToString;
    finally
      FThread.Excepted := false;
    end;
  except
    ErrStr := ExceptionStr;
    Result := false;
  end;
end;

function TLyParam.GetCount: integer;
begin
  if FParams <> nil then
    Result := FParams.Count else
    Result := 0;
end;

function TLyParam.GetHost(var AObj): boolean;
begin
  Result := GetItem(0).GetHost(Aobj);
end;

function TLyParam.GetItem(Index: integer): TLyValue;
begin
  Result := FParams[Index];
end;

function TLyParam.GetSelf(var Aobj): boolean;
begin
  Result := GetItem(0).GetSelf(Aobj);
end;

function TLyParam.GetThis: pointer;
begin
  Result := GetItem(0).FData;
end;

function TLyParam.GetValue(const Name: string): TLyValue;
var
  I: integer;
begin
  I := FFunc.FParams.IndexOf(Name);
  if I >= 0 then
    Result := GetItem(I) else
    Result := nil;
end;

function TLyParam.GetVarbValue(Index: integer; var VT: TLyType): TLyValue;
var
  T: TLyToken;
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
      T := TLyToken(FToken.FParams[Index]);
      if T.FSym = syID then
      begin
        Index := FPrev.Func.FParams.IndexOf(T.FName);
        if Index >= 0 then
        begin
          Result := FPrev.FParams[Index];
          VT := FPrev.Func.FParams[Index].FType;
          Exit;
        end;
      end
      else
      if T.FSym = syResult then
      begin
        Result := FPrev.FResult;
        VT := FPrev.Func.FResultType;
        Exit;
      end;
    end;
  end;
  Throw('variable not specified');
end;

procedure TLyParam.SetResult(Value: TLyValue);
begin
  FResult.Assign(Value);
  FFunc.FResultType.Convert(FResult);
end;

procedure TLyParam.SetArgs(Args: TLyList);
begin
  FParams := Args;
  FPrmc := Min(FParams.Count, FFunc.FParams.ParamCount);
  if FPrmc < FParams.Count then
    FParams.SetCount(FPrmc);
  FParams.PrepareFor(Func);
end;

{ TLyToken }

procedure TLyToken.AddParam(AParam: TLyToken);
begin
  if FParams = nil then
    FParams := TList.Create;
  FParams.Add(AParam);
end;

procedure TLyToken.Assign(Source: TLyToken);
begin
  FSym := Source.FSym;
  FRow := Source.FRow;
  FCol := Source.FCol;
  FName := Source.FName;
  FValue := Source.FValue;
end;

procedure TLyToken.Clear;
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

procedure TLyToken.ClearParams;
var
  I: integer;
  P: TLyToken;
begin
  for I := GetParamCount - 1 downto 0 do
  begin
    P := GetParam(I);
    FParams.Delete(I);
    P.Free;
  end;
  FreeAndNil(FParams);
end;

constructor TLyToken.Create(T: TLyToken);
begin
  if T <> nil then Read(T);
end;

constructor TLyToken.CreateWithLeft(LeftBranch: TLyToken; T: TLyToken);
begin
  Create(T);
  FLeft := LeftBranch;
end;

function TLyToken.Decompile(Level: integer): string;

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
    F: TLyFunc;
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
    syID     : Result := FName;
    syResult : Result := 'Result';
    syCall   : Result := decompile_call;
    syGet    : Result := decompile_get;
    sySet    : Result := decompile_get + ' := ' + FRight.Decompile;
    syInt    : Result := IntToStr(FValue.VInt);
    syStr    : Result := FormatString(FName);
    syChar   : Result := FormatChar(FValue.VChar);
    syFloat  : Result := FloatToStr(FValue.VFloat);
    syVert   : Result := decompile_lambda;
    syNot    : Result := 'not ' + FRight.Decompile;
    syNeg    : Result := '- ' + FRight.Decompile;
    syArray  : Result := decompile_array;
    syHash   : Result := decompile_hash;
    syAt     : Result := decompile_at;
    else
    if FSym in [syNil, sySelf, syObject, syInherited, syTrue, syFalse] then
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

destructor TLyToken.Destroy;
begin
  if Self <> nil then
  begin
    Clear;
    inherited;
  end;
end;

procedure TLyToken.Error(Param: TLyParam; const Msg: string;
  const Args: array of const);
begin
  Error(Param, Format(Msg, Args));
end;

procedure TLyToken.FailGet(Param: TLyParam; const ID: string);
begin
  Error(Param, 'failed getting: ' + ID);
end;

function TLyToken.TryFunc(Func: TLyFunc; Param: TLyParam; Outv: TLyValue): boolean;
var
  A: TLyList;
  M: integer;
begin
  Result := (Func <> nil);
  if Result then
  begin
    A := SetupParamList(Param, nil);
    if A <> nil then
    try
      Param.BeginExec(M);
      try
        ExecFunc(func, Param, Outv, A);
      finally
        Param.EndExec(M);
      end;
    finally
      A.Free;
    end;
  end;
end;

function TLyToken.TryMethod(Host: TLyValue; Method: TLyFunc; Param: TLyParam;
  Outv: TLyValue; LastParam: TLyToken): boolean;
var
  args: TLyList;
  tmpv: TLyValue;
  mark: integer;

  function check_set_arguments: boolean;
  begin
    Result := (Method.FParams.ParamCount = args.Count + 1);
    if not Result then
      Error(Param, '%s() needs %d arguments, %d passed',
        [Method.FullName, Method.FParams.ParamCount, args.Count + 1]) else
  end;

begin
  Result := (Method <> nil);
  if Result then
  begin
    args := SetupParamList(Param, Host);
    if args <> nil then
    try
      Param.BeginExec(mark);
      try
        if LastParam = nil then
          ExecFunc(Method, Param, Outv, args) else
        if check_set_arguments then
        begin
          tmpv := Param.FParams.Add;
          if LastParam.Execute(Param, tmpv) then
          begin
            args.Add(tmpv);
            ExecFunc(Method, Param, Outv, args);
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

function TLyToken.ExecFunc(Func: TLyFunc; Param: TLyParam; Outv: TLyValue; Args: TLyList): boolean;
var
  curr: TLyParam;
  mark: integer;
begin
  Result := false;

  if func = nil then
  begin
    Error(Param, 'Invalid function: nil');
    Exit;
  end;

  if Args <> nil then
    mark := Func.MinArgs - Args.Count else
    mark := Func.MinArgs;
  if mark > 0 then
  begin
    Error(Param, '%s() needs at least %d arguments, %d passed',
      [Func.FullName, Func.MinArgs, Func.MinArgs - mark]);
    Exit;
  end;

  Func.IncRefcount;
  try
    Param.BeginExec(mark);
    try
      if Outv = nil then Outv := Param.FParams.Add;
      if Args = nil then
      begin
        Args := TLyList.Create;
        Param.FParams.Add(my_array, Args);
      end;
      curr := TLyParam.Create;
      try
        curr.FPrev := Param.FThread.FCurrent;
        curr.FThread := Param.FThread;
        curr.FFunc := Func;
        curr.FResult := Outv;
        curr.FToken := Self;
        curr.SetArgs(Args);
        Param.FThread.FCurrent := curr;
        Func.FResultType.SetDefValue(Outv);
        // begin function
        if Assigned(Func.FMethod) then
          Func.FMethod(curr) else
        if Assigned(Func.FProc) then
          Func.FProc(curr) else
          Func.STMTs.Execute(curr);
        // end function
        Func.FResultType.Convert(Outv);
      finally
        Param.FThread.FCurrent := curr.FPrev;
        if Param.FThread.FState = tsExit then
          if not Assigned(Func.FProc) then
            Param.FThread.FState := tsRunning;
        curr.Free;
      end;
      Result := Param.FThread.StatusOK;
    finally
      Param.EndExec(mark);
    end;
  finally
    Func.DecRefcount;
  end;
end;


function TLyToken.GetProp(Param: TLyParam; Host, Outv: TLyValue): boolean;
var
  F: TLyFunc;
  T: TLyType;
  G: TLyGetAt;
begin
  G := GetAt(Param, Host, Outv);
  Result := (G <> gaNone);
  if Result and (Outv.FType = my_func) and (Outv.FData <> nil) then
  begin
    // (expression).ID ==> (expression).ID()
    F := TLyFunc(Outv.FData);
    if G = gaMethod then
      TryMethod(Host, F, Param, Outv, nil) else
    if Host.FType = my_type then
      if F.IsConstructor then
      begin
        T := TLyType(Host.FData);
        Host.Assign(T, T.CreateInstance);
        TryMethod(Host, F, Param, Outv, nil);
      end;
  end;
end;

function TLyToken.Execute(Param: TLyParam; Outv: TLyValue): boolean;
begin
  try
    Symbols[FSym].TX(Self, Param, Outv);
  except
    Error(Param, '');
  end;
  Result := Param.FThread.StatusOK;
end;

procedure TLyToken.FailGet(Param: TLyParam; const Host, Prop: string);
begin
  FailGet(Param, Host + '.' + Prop);
end;

function TLyToken.GetAt(Param: TLyParam; Host, Outv: TLyValue): TLyGetAt;
begin
  Result := Host.GetAt(FName, Outv);
  if Result = gaNone then
    FailGet(Param, Host.FType.FName, FName);
end;

function TLyToken.GetParam(Index: integer): TLyToken;
begin
  Result := TLyToken(FParams[Index]);
end;

function TLyToken.GetParamCount: integer;
begin
  if FParams <> nil then
    Result := FParams.Count else
    Result := 0;
end;

procedure TLyToken.Error(Param: TLyParam; const Msg: string);
var
  M: TLyModule;
  S: string;
begin
  if not Param.FThread.FExcepted then
  begin
    if Msg = '' then
      S := Param.FFunc.FName + '() - ' + ExceptionStr else
      S := Param.FFunc.FName + '() - ' + Msg;
    M := Param.FFunc.FModule;
    Param.FThread.FError.Runtime(S, M.Name, M.FFileName, FRow, FCol);
  end;
end;

procedure TLyToken.Read(Source: TLyToken);
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

function TLyToken.SetupParamList(Param: TLyParam; Host: TLyValue): TLyList;
var
  I, N: integer;
begin
  Result := TLyList.Create;
  if Host <> nil then
    Result.Add(Host);
  N := GetParamCount;
  for I := 0 to N - 1 do
    if not GetParam(I).Execute(Param, Result.Add) then
    begin
      FreeAndNil(Result);
      Exit;
    end;
end;

{ TLyTokenList }

function TLyTokenList.Add(Pos: TLyToken): TLyToken;
begin
  Result := TLyToken.Create;
  if Pos <> nil then
  begin
    Result.FRow := Pos.FRow;
    Result.FCol := Pos.FCol;
  end;
  FItems.Add(Result);
end;

function TLyTokenList.AddToken(Sym: TLySymbol): TLyToken;
begin
  Result := AddToken(Sym, nil);
end;

procedure TLyTokenList.Clear;
var
  X: integer;
  T: TLyToken;
begin
  for X := GetCount - 1 downto 0 do
  begin
    T := TLyToken(FItems[X]);
    FItems.Delete(X);
    T.Free;
  end;
end;

procedure TLyTokenList.ClearKeepLast;
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

constructor TLyTokenList.Create;
begin
  FItems := TList.Create;
end;

procedure TLyTokenList.DeleteLast(N: integer);
var
  X: integer;
  T: TLyToken;
begin
  N := GetCount - N;
  for X := GetCount - 1 downto N do
  begin
    T := TLyToken(FItems[X]);
    FItems.Delete(X);
    T.Free;
  end;
end;

destructor TLyTokenList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TLyTokenList.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TLyTokenList.GetItem(Index: integer): TLyToken;
begin
  Result := TLyToken(FItems[Index]);
end;

function TLyTokenList.GetLast: TLyToken;
begin
  Result := TLyToken(FItems.Last);
end;

procedure TLyTokenList.Reverse;
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

function TLyTokenList.AddToken(Token: TLyToken): TLyToken;
begin
  Result := Add;
  Result.Assign(Token);
end;

function TLyTokenList.AddToken(Sym: TLySymbol; Pos: TLyToken): TLyToken;
begin
  Result := Add(Pos);
  Result.FSym := Sym;
end;

{ TLyTokenizer }

constructor TLyTokenizer.Create(const Script: string);
var
  T: TLyToken;
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
  FTokens := TLyTokenList.Create;
  repeat
    T := FTokens.Add(nil);
    GetToken(T);
  until T.FSym in [syEOF, syError];
  if T.FSym = syError then
    FTokens.ClearKeepLast else
    FTokens.Reverse;
  FIndex := FTokens.Count - 1;
end;

destructor TLyTokenizer.Destroy;
begin
  FreeAndNil(FTokens);
  inherited;
end;

function TLyTokenizer.GetChar: boolean;
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

function TLyTokenizer.GetCurrent: TLyToken;
begin
  if FCurrent = nil then GetNext;
  Result := FCurrent;
end;

function TLyTokenizer.PackToCurrent: boolean;
begin
  Result := (FCurrent <> nil);
  if Result then
    FTokens.DeleteLast(FTokens.Count - (FIndex + 1));
end;

function TLyTokenizer.ParseChar(var C: char): boolean;
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

function TLyTokenizer.ParseHex(var I: int64): boolean;
begin
  Result := (FChar = '$') and GetChar and CharInSet(FChar, CS_HEX);
  if Result then
  begin
    I := HexValue(FChar);
    while GetChar and CharInSet(FChar, CS_HEX) do
      I := (I * 16) + HexValue(FChar);
  end;
end;

function TLyTokenizer.ParseString(var S: string): boolean;
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

function TLyTokenizer.GetNext: TLyToken;
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

function TLyTokenizer.GetToken(token: TLyToken): boolean;

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
    I: TLySymbol;
  begin
    token.FName := read_ID(FChar);
    if not IsResolvingID(token.FName) then
      for I := syBegin to syEnd do
        if not (I in [syGet, sySet]) then
          if MatchID(Symbols[I].ID, token.FName) then
          begin
            token.FSym := I;
            Exit;
          end;
    token.FSym := syID;
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

  procedure parse_hex_env;
  var
    H: boolean;
  begin
    H := true;
    if GetChar and CharInSet(FChar, CS_ID) then
    begin
      token.FName := '$' + FChar;
      if not CharInSet(FChar, CS_HEX) then H := false;
      while GetChar and CharInSet(FChar, CS_ID) do
      begin
        token.FName := token.FName + FChar;
        if not CharInSet(FChar, CS_HEX) then H := false;
      end;
      if H then
      begin
        token.FSym := syInt;
        token.FValue.VInt := StrToInt(token.FName);
      end
      else token.FSym := syID; // env
    end;
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

  procedure parse_operator(DefSymbol: TLySymbol;
    const next: array of char;
    const syms: array of TLySymbol);
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

  procedure parse_set(Symbol: TLySymbol);
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
      '$': parse_hex_env;
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
      '%': parse_set(syMod);
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
      '<': parse_operator(syLT, ['=', '>', '<'], [syLE, syNE, syShl]);
      '>': parse_operator(syMT, ['=', '>'], [syME, syShr]);
      else token.FName := FChar;
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

function TLyTokenizer.GotoChar(Chars: TSysCharSet): boolean;
begin
  repeat Result := CharInSet(FChar, Chars) until Result or not GetChar;
end;

function TLyTokenizer.NextChar: char;
begin
  if FPosition < FSize then
    Result := FCode[FPosition + 1] else
    Result := #0;
end;

function TLyTokenizer.PrevChar: char;
begin
  if FPosition > 1 then
    Result := FCode[FPosition - 1] else
    Result := #0;
end;

function TLyTokenizer.PeekNextSymbol: TLySymbol;
begin
  if FIndex > 0 then
    Result := FTokens[FIndex - 1].FSym else
    Result := syError;
end;

function TLyTokenizer.NextIsBecome(OnHead: boolean): boolean;
var
  S: TLySymbol;
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

function TLyTokenizer.PeekThirdSymbol: TLySymbol;
begin
  if FIndex > 1 then
    Result := FTokens[FIndex - 2].FSym else
    Result := syError;
end;

function TLyTokenizer.SkipSpaces: boolean;

  function skip_space: boolean;
  begin
    while FChar <= ' ' do if not GetChar then Break;
    Result := (FChar > ' ');
  end;

begin
  Result := false;
  while not Result and skip_space do
    if (FChar = '#') and (NextChar = '!') then
    begin
      GotoChar([#13, #10]);
      while CharInSet(FChar, [#13, #10]) do GetChar;
    end
    else
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

{ TLyParser }

constructor TLyParser.Create(AModule: TLyModule; AError: TLyError);
begin
  FModule := AModule;
  FError := AError;
end;

destructor TLyParser.Destroy;
begin
  FreeAndNil(FTokenizer);
  inherited;
end;

procedure TLyParser.EUnexpected(T: TLyToken);
var
  S: string;
begin
  if T = nil then T := FLast;
  S := T.FName;
  if S = '' then
    S := Symbols[T.FSym].ID;
  ESyntax(T.FRow, T.FCol, 'unexpected symbol: %s', [S]);
end;

procedure TLyParser.ERedeclared(T: TLyToken);
begin
  if T = nil then T := FLast;
  ESyntax(T.FRow, T.FCol, 'object redeclared: %s', [T.FName]);
end;

function TLyParser.UseToken(Token: TLyToken): TLyToken;
var
  I: integer;
  T: TLyToken;
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

procedure TLyParser.ETypeNotFound(T: TLyToken);
begin
  if T = nil then T := FLast;
  ESyntax(FTokenizer.FRow, FTokenizer.FCol, 'type not found: %s', [T.FName]);
end;

procedure TLyParser.ESyntax(ERow, ECol: integer;
  const EMsg: string; const EArgs: array of const);
begin
  FError.Syntax(Format(EMsg, EArgs), FModule.Name, FModule.FileName, ERow, ECol);
end;

procedure TLyParser.ParseUses;
var
  T: TLyType;
begin
  FAfter := FLast.FCol;
  repeat
    SymTestNextID;
    T := FModule.FindTypeBy(FLast.FName, '');
    if T <> nil then
    begin
      SymTestNext([syAs, syComma, sySemic]);
      if FLast.FSym = syAS then
      begin
        SymTestNextID;
        SymTestNext([syComma, sySemic]);
      end;
    end
    else
    begin
      if FModule.UseModule(FLast.FName, '', FError.FThread) = nil then
        ESyntax(FLast.FRow, FLast.FCol, 'module not exists: %s', [FLast.FName]);
      SymTestNext([syComma, sySemic]);
    end;
  until FLast.FSym = sySemic;
  SymGetNext;
end;

procedure TLyParser.ParseConst;
var
  L: TLyToken;
  S: TLyAssign;
begin
  FAfter := FLast.FCol;
  SymTestNextID;
  repeat
    if FModule.Find(FLast.FName) then ERedeclared;
    L := FLast;
    SymTestNext([syEQ]);
    FFunc := TLyFunc.Create('', FModule, nil);
    FModule.FConsts.Add(L.FName).Assign(my_func, FFunc);
    S := FFunc.STMTs.Add(ssConst) as TLyAssign;
    S.FVarb := L.FName;
    S.FExpr := ParseExpr(false, [sySemic], true);
    SymGetNext;
  until (FLast.FSym <> syID) or (FLast.FCol <= FAfter);
end;

procedure TLyParser.ParseDef;
var
  A: integer;
begin
  A := FAfter;
  try
    FAfter := FLast.FCol;
    SymTestNext([syID]);
    if FClass = nil then
    begin
      if FModule.Find(FLast.FName) then ERedeclared;
      FFunc := TLyFunc.Create(FLast.FName, FModule, nil);
    end
    else
    begin
      if MatchID(FLast.FName, FClass.FName) or
        (FClass.FMethods.Find(FLast.FName) <> nil) then
          ERedeclared;
      FFunc := TLyFunc.CreateMethod(FLast.FName, FClass, nil);
      FFunc.FParams.Add('Self', FClass);
    end;
    SymTestNext([syLParen]);
    ParseArguments(syRParen);
    ParseBlock([], FFunc.STMTs);
  finally
    FAfter := A;
  end;
end;

function TLyParser.Parse(const Code: string; UsingModule: boolean): TLyFunc;
begin
  Result := nil;
  FAfter := -1;
  FreeAndNil(FTokenizer);
  FTokenizer := TLyTokenizer.Create(Code);
  try
    SymGetNext;
    if UsingModule then ParseUsesConstFunc else
    begin
      if not FError.FThread.Running then
      begin
        Result := FError.FThread.MainFunc;
        Result.STMTs.Clear;
        ParseUsesConstFunc;
      end
      else Result := TLyFunc.Create('', FModule, nil);
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

procedure TLyParser.SymTestLast(Syms: TLySymbols);
begin
  if (Syms <> []) and not (FLast.FSym in Syms) then EUnexpected;
end;

procedure TLyParser.SymTestLastAfter;
begin
  if FLast.FCol <= FAfter then
    ESyntax(FLast.FRow, FLast.FCol,
      'symbol position(%d) should after %d',
      [FLast.FCol + 1, FAfter + 1]);
end;

procedure TLyParser.SymTestLastID;
begin
  SymTestLast([syID]);
  if not IsID(FLast.FName) then
    ESyntax(FLast.FRow, FLast.FCol,
      'invalid identity: %s', [FLast.FName]);
end;

procedure TLyParser.SymTestNext(Syms: TLySymbols);
begin
  SymGotoNext;
  SymTestLast(Syms);
end;

procedure TLyParser.SymTestNextID;
begin
  SymGotoNext;
  SymTestLastID;
end;

procedure TLyParser.SymGetNext;
begin
  FLast := FTokenizer.GetNext;
  if FLast = nil then
    ESyntax(FTokenizer.FRow, FTokenizer.FCol,
      'symbol expected but reachs end of file', []);
end;

procedure TLyParser.SymGotoNext;
begin
  SymGetNext;
  SymTestLastAfter;
end;

procedure TLyParser.ParseBlock(EndSyms: TLySymbols; SX: TLySTMTList);
var
  E: TLySymbols;
begin
  E := EndSyms + [syEOF, syElse, syElif, syUntil, syExcept, syFinally];
  SymGotoNext;
  while (FLast.FCol > FAfter) and not (FLast.FSym in E) do
    ParseStatement(true, SX);
  if EndSyms <> [] then
  begin
    SymTestLast(EndSyms);
    if FLast.FCol < FAfter then
      SymTestLastAfter;
  end;
end;

function TLyParser.ParseAndFree(const Code: string; UsingModule: boolean): TLyFunc;
begin
  try
    Result := Parse(Code, UsingModule);
  finally
    Free;
  end;
end;

function TLyParser.ParseExpr(OnHead: boolean; EndSyms: TLySymbols; DoCheck: boolean): TLyToken;
begin
  if not OnHead then SymGotoNext;
  Result := ParseFact(High(OperLevel));
  if DoCheck then
    SymTestLast(EndSyms);
end;

function TLyParser.ParseFact(Level: integer): TLyToken;
begin
  if Level > 0 then
    Result := ParseFact(Level - 1) else
    Result := ParseTerm;
  while (FLast.FSym in OperLevel[Level]) and (SymPeekNext in ExprHead) do
  begin
    Result := TLyToken.CreateWithLeft(Result, FLast);
    SymGotoNext;
    if Level > 0 then
      Result.FRight := ParseFact(Level - 1) else
      Result.FRight := ParseTerm;
  end;
end;

procedure TLyParser.ParseStatement(OnHead: boolean; SX: TLySTMTList);
var
  A: integer;
begin
  if not OnHead then SymGotoNext;
  A := FAfter;
  try
    FAfter := FLast.FCol;
    case FLast.FSym of
      sySemic : repeat SymGetNext until FLast.FSym <> sySemic;
      syVar   : ParseVar(SX);
      syIf    : ParseIf(SX);
      syFor   : ParseFor(SX);
      syWhile : ParseWhile(SX);
      syRepeat: ParseRepeat(SX);
      syCase  : ParseCase(SX);
      syTry   : ParseTry(SX);
      syRaise : ParseRaise(SX);
      syEQ    : ParsePuts(SX);
      else ParseAny(SX);
    end;
    FTokenizer.PackToCurrent;
  finally
    FAfter := A;
  end;
end;

procedure TLyParser.ParseIf(SX: TLySTMTList);
var
  S: TLyIf;
begin
{ if CONDITION then ... else ... | }
  S := SX.Add(ssIf) as TLyIf;
  S.FExpr := ParseExpr(false, [syThen], true);
  ParseBlock([], S.GetItems);
  if (FLast.FSym in [syElse, syElif]) and (FLast.FCol = FAfter) then
  begin
    FAfter := FLast.FCol;
    if FLast.FSym = syElse then
      ParseBlock([], S.GetElseItems) else
      ParseIf(S.GetElseItems);
  end;
end;

procedure TLyParser.ParseUsesConstFunc;
begin
  while FLast.FSym in [syDef, syClass, syConst, syUses] do
  begin
    FClass := nil;
    FFunc := nil;
    FAfter := -1;
    if FLast.FSym = syDef then ParseDef else
    if FLast.FSym = syClass then ParseClass else
    if FLast.FSym = syConst then ParseConst else
    if FLast.FSym = syUses then ParseUses;
    FTokenizer.PackToCurrent;
  end;
end;

procedure TLyParser.ParseVar(SX: TLySTMTList);
var
  S: TLyAssign;
  V: TLyVarb;
begin
{ var a, b = expression, c; }
  repeat
    SymTestNextID;
    if FFunc.FindInside(FLast.FName) then ERedeclared;
    V := FFunc.FParams.AddLocal(FLast.FName, my_variant);
    SymTestNext([sySemic, syComma, syBecome]);
    if FLast.FSym = syBecome then
    begin
      S := SX.Add(ssAssign) as TLyAssign;
      S.FVarb := V.FName;
      S.FExpr := ParseExpr(false, [sySemic, syComma], true);
    end;
  until FLast.FSym = sySemic;
  SymGetNext;
end;

procedure TLyParser.ParseFor(SX: TLySTMTList);
var
  S: TLyFor;
begin
{ for a in range do ... |
  for a := low to high do ... |
  for a := high downto low do ... | }
  S := SX.Add(ssFor) as TLyFor;
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
end;

procedure TLyParser.ParseWhile(SX: TLySTMTList);
var
  S: TLySTMT;
begin
{ while CONDITION do ... | }
  S := SX.Add(ssWhile);
  S.FExpr := ParseExpr(false, [syDo], true);
  ParseBlock([], S.GetItems);
end;

procedure TLyParser.ParseRepeat(SX: TLySTMTList);
var
  S: TLySTMT;
begin
{ repeat .... until CONDITION | }
  S := SX.Add(ssRepeat);
  ParseBlock([syUntil], S.GetItems);
  if FLast.FCol = FAfter then
  begin
    S.FExpr := ParseExpr(false, [sySemic], true);
    SymGetNext;
  end
  else EUnexpected;
end;

procedure TLyParser.ParseCase(SX: TLySTMTList);
var
  S: TLyCase;
  T: TLySTMT;
  A: integer;
begin
{ case EXPRESSION of
    V1: ......
    V2: ......
   else ......}
  A := FAfter;
  S := SX.Add(ssCase) as TLyCase;
  S.FExpr := ParseExpr(false, [syOf], true);
  SymGotoNext;
  while FLast.FCol > A do
  begin
    FAfter := FLast.FCol;
    if FLast.FSym <> syElse then
    begin
      T := S.GetItems.Add(ssNormal);
      T.FExpr := ParseExpr(true, [syColon], true);
      ParseBlock([], T.GetItems);
    end
    else ParseBlock([], S.GetElseItems);
    FAfter := A;
  end;
end;

procedure TLyParser.ParseClass;
var
  K: string;
  F: TLyFunc;
  T: TLyType;
begin
// class NEW_CLASS:PARENT_CLASS(Constructing-Args)
  FAfter := FLast.FCol;
  SymTestNext([syID]);
  if not FModule.Find(FLast.FName) then
  begin
    K := FLast.FName;
    T := my_object;

    // seek parent class
    SymTestNext([syColon, syLParen]);
    if FLast.FSym <> syLParen then
    begin
      ParseType(false, T);
      if T.InheriteClassType = nil then
        ESyntax(FLast.FRow, FLast.FCol,
          'can not define class from: %s', [FLast.FName]);
      SymTestNext([syLParen]);
    end;

    // setup new class
    FClass := T.Inherite(K, FModule);
    FClass.Method(LSE_CREATE, FClass, TLyObjectProc(nil));
    FClass.Setup;

    T := FClass;
    F := FClass.FCreater;
    FFunc := F;
    FFunc.STMTs; // setup statement list

    // setup arguments and class body
    ParseArguments(syRParen);
    SymGotoNext;
    while (FLast.FCol > FAfter) and (FLast.FSym <> syEOF) do
    begin
      FClass := T;
      FFunc := F;
      if FLast.FSym = syDef then
        ParseDef else
        ParseStatement(true, FFunc.STMTs);
    end;
  end
  else ERedeclared;
end;

procedure TLyParser.ParseTry(SX: TLySTMTList);
var
  S, T: TLyTry;
begin
{ try ....
  except ....
  finally .... }
  S := SX.Add(ssTry) as TLyTry;
  ParseBlock([syExcept, syFinally], S.GetItems);
  if FLast.FCol = FAfter then
  begin
    S.FTryFinally := (FLast.FSym = syFinally);
    ParseBlock([], S.GetElseItems);
    while (FLast.FSym in [syExcept, syFinally]) and (FLast.FCol = FAfter) do
    begin
      SX.FItems.Remove(S);
      T := SX.Add(ssTry) as TLyTry;
      T.GetItems.Add(S);
      T.FTryFinally := (FLast.FSym = syFinally);
      ParseBlock([], T.GetElseItems);
      S := T;
    end;
  end
  else EUnexpected;
end;

procedure TLyParser.ParseType(OnHead: boolean; var T: TLyType);
var
  I, M: string;
begin
  if not OnHead then SymTestNext([syID, syArray, syObject]);
  if FLast.FSym = syObject then T := my_object else
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

procedure TLyParser.ParseRaise(SX: TLySTMTList);
var
  S: TLySTMT;
begin
{ raise EXPRESSION; | raise; }
  S := SX.Add(ssRaise);
  SymGotoNext;
  if FLast.FSym <> sySemic then
    S.FExpr := ParseExpr(true, [sySemic], true) else
    SymTestLast([sySemic]);
  SymGetNext;
end;

procedure TLyParser.ParsePuts(SX: TLySTMTList);
var
  S, T: TLySTMT;
begin
{ = ..., ..., ..., ...; }
  S := SX.Add(ssPuts);
  repeat
    T := S.GetItems.Add(ssNormal);
    T.FExpr := ParseExpr(false, [sySemic, syComma], true);
  until FLast.FSym = sySemic;
  SymGetNext;
end;

procedure TLyParser.ParseAny(SX: TLySTMTList);
var
  V: TLyToken;
  S: TLySTMT;
  A, T: TLyAssign;
  I: integer;
begin
  if FTokenizer.NextIsBecome(false) then
  begin
    SymTestLast([syID, syResult]);
    if FLast.FSym = syID then
    begin
      A := TLyAssign(SX.Add(ssAssign));
      A.FVarb := FLast.FName;
      FFunc.FParams.AddLocal(FLast.FName, my_variant);
    end
    else A := TLyAssign(SX.Add(ssResult));

    V := FLast;
    SymGotoNext;

    if FLast.FSym <> syBecome then // +=, -=, *=, ....
    begin
      A.FExpr := TLyToken.Create(FLast);  // operator
      A.FExpr.FLeft := TLyToken.Create(V);
      SymGotoNext;
      A.FExpr.FRight := ParseExpr(false, [sySemic], true);
    end
    else
    begin
      SymGotoNext;
      { a := b := expression }
      while (FLast.FSym in [syID, syResult]) and (SymPeekNext = syBecome) do
      begin
        if FLast.FSym = syID then
        begin
          T := TLyAssign(A.GetItems.Add(ssAssign));
          T.FVarb := FLast.FName;
          FFunc.FParams.AddLocal(FLast.FName, my_variant);
        end
        else A.GetItems.Add(ssResult);
        SymGotoNext;
        SymGotoNext;
      end;
      A.FExpr := ParseExpr(true, [sySemic], true);
    end;
  end
  else
  if (FLast.FSym in [syID, syResult]) and (SymPeekNext = syComma) then
  begin
    { a, b, ... := expression1, expression2, ... }
    T := TLyAssign(SX.Add(ssAssign));
    while FLast.FSym in [syID, syResult] do
    begin
      if FLast.FSym = syID then
      begin
        A := TLyAssign(T.GetItems.Add(ssAssign));
        A.FVarb := FLast.FName;
        FFunc.FParams.AddLocal(FLast.FName, my_variant);
      end
      else T.GetItems.Add(ssResult);
      SymTestNext([syComma, syBecome]);
      if FLast.FSym = syComma then
        SymTestNext([syID, syResult]) else
        Break;
    end;
    for I := 0 to T.FItems.Count - 1 do
      if I = T.FItems.Count - 1 then
        T.FItems[I].FExpr := ParseExpr(false, [sySemic], true) else
        T.FItems[I].FExpr := ParseExpr(false, [syComma], true);
  end
  else
  begin
    S := SX.Add(ssNormal);
    S.FExpr := ParseExpr(true, [sySemic, syBecome], true);
    if FLast.FSym = syBecome then
      if S.FExpr.FSym = syGet then
      begin
        S.FExpr.FRight := ParseExpr(false, [sySemic], true);
        S.FExpr.FSym := sySet;
      end
      else
      if (S.FExpr.FSym = syCall) and MatchID(S.FExpr.FName, LSE_GETIV) then
      begin
        // (expression)[...] ==> (expression).Set[](..., value)
        S.FExpr.FName := LSE_SETIV;
        S.FExpr.AddParam(ParseExpr(false, [sySemic], true));
      end
      else EUnexpected;
  end;
  SymGetNext;
end;

procedure TLyParser.ParseArguments(EndSym: TLySymbol);
begin
  SymTestNext([sySelf, syID, EndSym]);
  if FLast.FSym = sySelf then
  begin
    FLast.FSym := syID;
    FLast.FName := Symbols[sySelf].ID;
  end;
  while FLast.FSym = syID do
    if FFunc.FindInside(FLast.FName) then ERedeclared else
    begin
      FFunc.FParams.Add(FLast.FName, my_variant);
      SymTestNext([syComma, EndSym]);
      if FLast.FSym = syComma then
        SymTestNextID;
    end;
end;

function TLyParser.ParseTerm: TLyToken;

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

  procedure parse_call(EndSym: TLySymbol);
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
  F: TLyFunc;
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
  { [a, b, ...] or [a:'...', b:'...', ...] }
  if FLast.FSym = syLArray then
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
  { |a, b, ...| expression }
  if FLast.FSym = syVert then
  begin
    Result := UseToken(FLast);
    F := FFunc;
    try
      FFunc := TLyFunc.Create('', FModule, nil);
      ParseArguments(syColon);
      FFunc.STMTs.Add(ssResult).FExpr := ParseExpr(false, [syVert], true);
      Result.FValue.VFunc := FFunc;
      FFunc.IncRefcount;
    finally
      FFunc := F;
    end;
  end
  else
  { (expression) }
  if FLast.FSym = syLParen then
    Result := ParseExpr(false, [syRParen], true) else
    EUnexpected(FLast);

  SymGetNext;
  while (FLast.FCol > FAfter) and (FLast.FSym in [syLParen, syDot, syLArray]) do
  begin
    if FLast.FSym = syLParen then // expression(...)
    begin
      Result := TLyToken.CreateWithLeft(Result, FLast);
      Result.FSym := syCall;
      Result.FName := '';
      parse_call(syRParen);
    end
    else
    if FLast.FSym = syLArray then // expression[...]
    begin
      Result := TLyToken.CreateWithLeft(Result, FLast);
      Result.FSym := syCall;
      Result.FName := LSE_GETIV;
      parse_call(syRArray);
    end
    else // expression.ID
    begin
      SymTestNext([syBegin..syEnd, syID]);
      if FLast.FName = '' then
        if FLast.FSym <> syID then
          FLast.FName := Symbols[FLast.FSym].ID;
      Result := TLyToken.CreateWithLeft(Result, FLast);
      Result.FSym := syGet;
      Result.FName := FLast.FName;
      if SymPeekNext = syLParen then // expression.ID(...)
      begin
        SymGotoNext;
        Result.FSym := syCall;
        parse_call(syRParen);
      end;
    end;
    SymGetNext;
  end;
end;

function TLyParser.SymPeekNext: TLySymbol;
begin
  Result := FTokenizer.PeekNextSymbol;
end;

{ TLySTMT }

constructor TLySTMT.Create(AParent: TLySTMTList);
begin
  FStyle := ssNormal;
  FParent := AParent;
  FParent.FItems.Add(Self);
end;

procedure TLySTMT.Decompile(Level: integer; Lines: TStrings);

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

destructor TLySTMT.Destroy;
begin
  if FParent <> nil then
    FParent.FItems.Remove(Self);
  FreeAndNil(FExpr);
  FreeAndNil(FItems);
  inherited;
end;

procedure TLySTMT.ExecNormal(Param: TLyParam);
var
  tmpv: TLyValue;
  mark: integer;
begin
  Param.BeginExec(mark, tmpv);
  try
    FExpr.Execute(Param, tmpv);
  finally
    Param.EndExec(mark);
  end;
end;

procedure TLySTMT.ExecPuts(Param: TLyParam);
var
  tmpv: TLyValue;
  mark, I: integer;
begin
  Param.BeginExec(mark, tmpv);
  try
    if (FItems <> nil) and (FItems.Count > 0) then
      if FItems[0].FExpr.Execute(Param, tmpv) then
      begin
        Param.FThread.FEngine.Write(tmpv.AsString);
        for I := 1 to FItems.Count - 1 do
          if FItems[I].FExpr.Execute(Param, tmpv) then
          begin
            Param.FThread.FEngine.Write(' ');
            Param.FThread.FEngine.Write(tmpv.AsString);
          end
          else Break;
      end;
  finally
    Param.EndExec(mark);
  end;
end;

procedure TLySTMT.ExecRaise(Param: TLyParam);
var
  tmpv: TLyValue;
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
  else Param.FThread.FExcepted := true;
end;

procedure TLySTMT.ExecRepeat(Param: TLyParam);
var
  T: TLyThread;
  tmpv: TLyValue;
  mark: integer;
begin
  T := Param.FThread;
  Param.BeginExec(mark, tmpv);
  try
    repeat
      if not FItems.Execute(Param) then
        if T.FState in [tsBreak, tsContinue] then
        begin
          if T.FState = tsBreak then Break;
          T.FState := tsRunning;
        end;
    until not T.StatusOK or not FExpr.Execute(Param, tmpv) or tmpv.AsBoolean;
  finally
    T.ForWhileRepeatEnded;
    Param.EndExec(mark);
  end;
end;

function TLySTMT.Execute(Param: TLyParam): boolean;
begin
  case FStyle of
    ssNormal: ExecNormal(Param);
    ssPuts  : ExecPuts(Param);
    ssWhile : ExecWhile(Param);
    ssRepeat: ExecRepeat(Param);
    ssRaise : ExecRaise(Param);
  end;
  Result := Param.FThread.StatusOK;
end;

procedure TLySTMT.ExecWhile(Param: TLyParam);
var
  T: TLyThread;
  tmpv: TLyValue;
  mark: integer;
begin
  T := Param.FThread;
  Param.BeginExec(mark, tmpv);
  try
    while T.StatusOK and FExpr.Execute(Param, tmpv) and tmpv.AsBoolean do
      if not FItems.Execute(Param) then
        if T.FState in [tsBreak, tsContinue] then
        begin
          if T.FState = tsBreak then Break;
          T.FState := tsRunning;
        end;
  finally
    T.ForWhileRepeatEnded;
    Param.EndExec(mark);
  end;
end;

function TLySTMT.GetCount: integer;
begin
  if FItems <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function TLySTMT.GetItems: TLySTMTList;
begin
  if FItems = nil then
    FItems := TLySTMTList.Create;
  Result := FItems;
end;

{ TLyAssign }

procedure TLyAssign.SetVarb(Param: TLyParam; const Varb: string; Value: TLyValue);
var
  I: integer;
begin
  I := Param.FFunc.FParams.IndexOf(Varb);
  Param.FFunc.FParams[I].FType.Convert(Value);
  if Value.FType = my_string then
    Param.FParams[I].AsString := Value.AsString else
    Param.FParams[I].Assign(Value);
end;

procedure TLyAssign.SetResult(Param: TLyParam; Value: TLyValue);
begin
  Param.FFunc.FResultType.Convert(Value);
  Param.FResult.Assign(Value);
end;

constructor TLyAssign.Create(AParent: TLySTMTList);
begin
  inherited;
  FStyle := ssAssign;
end;

procedure TLyAssign.Decompile(Level: integer; Lines: TStrings);

  procedure decompile_const;
  begin
    Lines.Add(Margin(Level) + 'const ' + FVarb + ' = ' + FExpr.Decompile + ';');
  end;

  procedure decompile_multi_assign;
  var
    S: string;
    I: integer;
  begin
    if FExpr = nil then // a, b, c := c, b, a;
    begin
      S := Margin(Level) + TLyAssign(FItems[0]).VarbID;
      for I := 1 to FItems.Count - 1 do
        S := S + ', ' + TLyAssign(FItems[I]).VarbID;
      S := S + ' := ' + TLyAssign(FItems[0]).FExpr.Decompile;
      for I := 1 to FItems.Count - 1 do
        S := S + ', ' + FItems[I].FExpr.Decompile;
    end
    else // a := b := c := ...
    begin
      S := Margin(Level) + VarbID;
      for I := 0 to FItems.Count - 1 do
        S := S + ' := ' + TLyAssign(FItems[I]).VarbID;
      S := S + ' := ' + FExpr.Decompile;
    end;
    Lines.Add(S + ';');
  end;

  procedure decompile_assign;
  begin
    if (FItems = nil) or (FItems.Count = 0) then
      Lines.Add(Margin(Level) + FVarb + ' := ' + FExpr.Decompile + ';') else
      decompile_multi_assign;
  end;

  procedure decompile_result;
  begin
    if (FItems = nil) or (FItems.Count = 0) then
      Lines.Add(Margin(Level) + 'Result := ' + FExpr.Decompile + ';') else
      decompile_multi_assign;
  end;

begin
  case FStyle of
    ssConst : decompile_const;
    ssAssign: decompile_assign;
    ssResult: decompile_result;
  end;
end;

procedure TLyAssign.ExecAssign(Param: TLyParam);
var
  P: integer;
  V: TLyValue;
begin
  if (FItems = nil) or (FItems.Count = 0) then
  begin
    Param.BeginExec(P, V);
    try
      if FExpr.Execute(Param, V) then
        SetVarb(Param, FVarb, V);
    finally
      Param.EndExec(P);
    end;
  end
  else MultiAssign(Param);
end;

procedure TLyAssign.ExecConst(Param: TLyParam);
begin
  if FExpr.Execute(Param, Param.FResult) then
  begin
    Param.FFunc.FResultType.Convert(Param.FResult);
    Param.FFunc.FModule.FConsts.Add(FVarb).Assign(Param.FResult);
  end;
end;

procedure TLyAssign.MultiAssign(Param: TLyParam);
var
  P, I: integer;
  V: TLyValue;
  A: TLyAssign;
begin
  Param.BeginExec(P, V);
  try
    if FExpr = nil then // a, b, c := c, b, a;
    begin
      for I := 1 to FItems.Count - 1 do
        Param.FParams.Add;

      for I := 0 to FItems.Count - 1 do
      begin
        A := TLyAssign(FItems[I]);
        A.FExpr.Execute(Param, Param.FParams[P + I]);
      end;

      for I := 0 to FItems.Count - 1 do
      begin
        V := Param.FParams[P + I];
        A := TLyAssign(FItems[I]);
        if A.FStyle = ssResult then
          SetResult(Param, V) else
          SetVarb(Param, A.FVarb, V);
      end;
    end
    else
    if FExpr.Execute(Param, V) then // a := b := c := ...
    begin
      for I := FItems.Count - 1 downto 0 do
      begin
        A := TLyAssign(FItems[I]);
        if A.FStyle = ssResult then
          SetResult(Param, V) else
          SetVarb(Param, A.FVarb, V);
      end;
      if A.FStyle = ssResult then
        SetResult(Param, V) else
        SetVarb(Param, FVarb, V);
    end;
  finally
    Param.EndExec(P);
  end;
end;

procedure TLyAssign.ExecResult(Param: TLyParam);
var
  P: integer;
  V: TLyValue;
begin
  if (FItems = nil) or (FItems.Count = 0) then
  begin
    Param.BeginExec(P, V);
    try
      if FExpr.Execute(Param, V) then
        SetResult(Param, V);
    finally
      Param.EndExec(P);
    end;
  end
  else MultiAssign(Param);
end;

function TLyAssign.Execute(Param: TLyParam): boolean;
begin
  case FStyle of
    ssConst : ExecConst(Param);
    ssAssign: ExecAssign(Param);
    ssResult: ExecResult(Param);
  end;
  Result := Param.FThread.StatusOK;
end;

function TLyAssign.GetVarbID: string;
begin
  if FStyle = ssResult then
    Result := 'Result' else
    Result := FVarb;
end;

{ TLyFor }

constructor TLyFor.Create(AParent: TLySTMTList);
begin
  inherited;
  FStyle := ssFor;
end;

procedure TLyFor.Decompile(Level: integer; Lines: TStrings);
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

destructor TLyFor.Destroy;
begin
  FreeAndNil(FEndValue);
  inherited;
end;

function TLyFor.Execute(Param: TLyParam): boolean;
var
  cntx: TLyThread;
  mark, I: integer;
  G: TLyGenerate;
  V: TLyValue;
  T: TLyType;

  function get_generate: TLyGenerate;
  var
    begv, endv: TLyValue;
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
  cntx := Param.FThread;
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
        V.Assign(G);
        T.Convert(V);
        if not FItems.Execute(Param) then
          if cntx.FState in [tsBreak, tsContinue] then
          begin
            if cntx.FState = tsBreak then Break;
            cntx.FState := tsRunning;
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

constructor TLyIf.Create(AParent: TLySTMTList);
begin
  inherited;
  FStyle := ssIf;
end;

procedure TLyIf.Decompile(Level: integer; Lines: TStrings);

  procedure decompile_else(S: TLyIf);
  begin
    if S.IsElif then
    begin
      Lines.Add(Margin(Level) + 'elif ' + S.FExpr.Decompile + ' then');
      if S.GetCount > 0 then
        S.FItems.Decompile(Level + 1, Lines);
      decompile_else(S.FElseItems[0] as TLyIf);
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

destructor TLyIf.Destroy;
begin
  FreeAndNil(FElseItems);
  inherited;
end;

function TLyIf.Execute(Param: TLyParam): boolean;
var
  tmpv: TLyValue;
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

function TLyIf.GetElseCount: integer;
begin
  if FElseItems <> nil then
    Result := FElseItems.Count else
    Result := 0;
end;

function TLyIf.GetElseItems: TLySTMTList;
begin
  if FElseItems = nil then
    FElseItems := TLySTMTList.Create;
  Result := FElseItems;
end;

function TLyIf.IsElif: boolean;
begin
  Result := (GetElseCount = 1) and (FElseItems[0].FStyle = ssIf);
end;

{ TLyCase }

constructor TLyCase.Create(AParent: TLySTMTList);
begin
  inherited;
  FStyle := ssCase;
end;

procedure TLyCase.Decompile(Level: integer; Lines: TStrings);
var
  I: integer;
  S: TLySTMT;
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

function TLyCase.Execute(Param: TLyParam): boolean;
var
  tmpv, T: TLyValue;
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
      if Param.FThread.StatusOK then
        Result := Result or FElseItems.Execute(Param) else
        Result := false;
    end;
  finally
    Param.EndExec(mark);
  end;
end;

{ TLyTry }

constructor TLyTry.Create(AParent: TLySTMTList);
begin
  inherited;
  FStyle := ssTry;
end;

procedure TLyTry.Decompile(Level: integer; Lines: TStrings);
begin
  Lines.Add(Margin(Level) + 'try');
  FItems.Decompile(Level + 1, Lines);
  if FTryFinally then
    Lines.Add(Margin(Level) + 'finally') else
    Lines.Add(Margin(Level) + 'except');
  FElseItems.Decompile(Level + 1, Lines);
end;

function TLyTry.Execute(Param: TLyParam): boolean;
var
  E: boolean;
begin
  FItems.Execute(Param);
  E := Param.FThread.FExcepted;
  Param.FThread.FExcepted := false;
  if FTryFinally or E then
    FElseItems.Execute(Param);
  if FTryFinally and E then
    Param.FThread.FExcepted := true;
  Result := Param.FThread.StatusOK;
end;

{ TLySTMTList }

function TLySTMTList.Add(Style: TLySTMTStyle): TLySTMT;
begin
  if FItems = nil then FItems := TList.Create;
  Result := nil;
  case Style of
    ssNormal: Result := TLySTMT.Create(Self);
    ssConst : Result := TLyAssign.Create(Self);
    ssAssign: Result := TLyAssign.Create(Self);
    ssResult: Result := TLyAssign.Create(Self);
    ssPuts  : Result := TLySTMT.Create(Self);
    ssIf    : Result := TLyIf.Create(Self);
    ssWhile : Result := TLySTMT.Create(Self);
    ssRepeat: Result := TLySTMT.Create(Self);
    ssFor   : Result := TLyFor.Create(Self);
    ssCase  : Result := TLyCase.Create(Self);
    ssTry   : Result := TLyTry.Create(Self);
    ssRaise : Result := TLySTMT.Create(Self);
  end;
  Result.FStyle := Style;
end;

function TLySTMTList.Add(STMT: TLySTMT): integer;
begin
  if FItems = nil then FItems := TList.Create;
  if STMT.FParent <> Self then
  begin
    STMT.FParent.FItems.Remove(STMT);
    STMT.FParent := Self;
  end;
  Result := FItems.Add(STMT);
end;

procedure TLySTMTList.Clear;
var
  I: integer;
begin
  for I := GetCount - 1 downto 0 do Delete(I);
  FreeAndNil(FItems);
end;

procedure TLySTMTList.Decompile(Level: integer; Lines: TStrings);
var
  I: integer;
begin
  if Level < 1 then Level := 1;
  for I := 0 to GetCount - 1 do
    GetItem(I).Decompile(Level, Lines);
end;

procedure TLySTMTList.Delete(Index: integer);
var
  S: TLySTMT;
begin
  S := GetItem(Index);
  FItems.Delete(Index);
  S.FParent := nil;
  S.Free;
end;

destructor TLySTMTList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TLySTMTList.Execute(Param: TLyParam): boolean;
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

function TLySTMTList.GetCount: integer;
begin
  if FItems <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function TLySTMTList.GetItem(Index: integer): TLySTMT;
begin
  Result := TLySTMT(FItems[Index]);
end;

{ TLyVarb }

constructor TLyVarb.Create(const AName: string; AType: TLyType);
begin
  FName := AName;
  FType := AType;
end;

destructor TLyVarb.Destroy;
begin
  FName := '';
  inherited;
end;

function TLyVarb.Prototype: string;
begin
  Result := FType.Prototype(FName);
end;

{ TLyVarbList }

function TLyVarbList.GetCount: integer;
begin
  Result := Length(FVarbs);
end;

function TLyVarbList.GetParamCount: integer;
begin
  Result := GetCount - FLocalCount;
end;

function TLyVarbList.GetVarb(Index: integer): TLyVarb;
begin
  Result := FVarbs[Index];
end;

function TLyVarbList.IndexOf(const AVarb: TLyVarb): integer;
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

function TLyVarbList.DoAdd(const AName: string; AType: TLyType): TLyVarb;
var
  I: integer;
begin
  Result := TLyVarb.Create(AnsiString(AName), AType);
  I := Length(FVarbs);
  SetLength(FVarbs, I + 1);
  FVarbs[I] := Result;
end;

constructor TLyVarbList.Create;
begin
  SetLength(FVarbs, 0);
  FLocalCount := 0;
end;

destructor TLyVarbList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLyVarbList.Clear;
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

function TLyVarbList.Add(const AName: string; AType: TLyType): TLyVarb;
begin
  Result := Find(AName);
  if Result = nil then
    Result := DoAdd(AName, AType);
end;

function TLyVarbList.AddLocal(const AName: string; AType: TLyType): TLyVarb;
begin
  Result := Find(AName);
  if Result = nil then
  begin
    Result := DoAdd(AName, AType);
    Inc(FLocalCount);
  end;
end;

procedure TLyVarbList.Assign(Source: TLyVarbList);
var
  I: integer;
begin
  Clear;
  for I := 0 to Source.GetCount - 1 do
    DoAdd(Source[I].FName, Source[I].FType);
  FLocalCount := Source.FLocalCount;
end;

function TLyVarbList.IndexOf(const AName: string): integer;
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

function TLyVarbList.Find(const AName: string): TLyVarb;
var
  I: integer;
begin
  I := IndexOf(AName);
  if (I >= 0) and (I < Length(FVarbs)) then
    Result := FVarbs[I] else
    Result := nil;
end;

{ TLyFunc }

constructor TLyFunc.Create(const AName: string; M: TLyModule; Proc: TLyProc);
begin
  FName := AName;
  if FName <> '' then
  begin
    IncRefcount;
    M.FFuncList.FItems.Add(Self);
  end;
  FModule := M;
  FParams := TLyVarbList.Create;
  FResultType := my_variant;
  FProc := Proc;
  FMinArgs := -1;
  if Context <> nil then
    if Context.FRollbacks <> nil then
      if Context.FRollbacks.IndexOf(FModule) < 0 then
        Context.FRollbacks.Add(Self);
end;

constructor TLyFunc.CreateMethod(const AName: string; P: TLyType; Proc: TLyProc);
begin
  FName := AName;
  FParent := P;
  FParent.FMethods.FItems.Add(Self);
  IncRefcount;
  FModule := FParent.FModule;
  FParams := TLyVarbList.Create;
  FResultType := my_variant;
  FProc := Proc;
  FMinArgs := -1;
end;

procedure TLyFunc.Decompile(Level: integer; Lines: TStrings);
begin
  Lines.Add(Margin(Level) + Prototype);
  if (FSTMTs <> nil) and not IsConst then
    FSTMTs.Decompile(Level + 1, Lines);
end;

destructor TLyFunc.Destroy;
begin
  if (FModule <> nil) and (FModule.FuncList <> nil) then   // #####
  begin
    FModule.FFuncList.FItems.Remove(Self);
    if FModule.FThread <> nil then
      if FModule.FThread.FMainFunc = Self then
      begin
        FModule.FThread.FMainFunc := nil;
        FModule.FThread.FMainLocals.Clear;
      end;
  end;

  if FParent <> nil then
  begin
    if FParent.FCreater = Self then
      FParent.FCreater := nil;
    if FParent.FMethods <> nil then
      FParent.FMethods.FItems.Remove(Self);
  end;

  FreeAndNil(FSTMTs);
  FreeAndNil(FParams);

  if Context <> nil then
    Context.RollbackRemove(Self);

  inherited;
end;

function TLyFunc.Context: TLysee;
begin
  if FModule <> nil then
    Result := FModule.FEngine else
    Result := nil;
end;

function TLyFunc.GetFullName: string;
begin
  if FParent <> nil then
    Result := FParent.FName + '.' + Name else
    Result := FModule.FName + '.' + Name;
end;

function TLyFunc.GetMinArgs: integer;
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

function TLyFunc.STMTs: TLySTMTList;
begin
  if FSTMTs = nil then
    FSTMTs := TLySTMTList.Create;
  Result := FSTMTs;
end;

function TLyFunc.ToString: string;
begin
  Result := Prototype;
end;

function TLyFunc.Prototype: string;
var
  X: integer;
  P: TLyVarb;
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
      N := '(' + P.FType.Prototype(P.FName);
      for X := X + 1 to FParams.ParamCount - 1 do
      begin
        P := FParams[X];
        N := N + ', ' + P.FType.Prototype(P.FName);
      end;
      N := N + ')';
    end
    else N := '';
    if FParent <> nil then
      Result := 'def ' + FParent.FName + '.' + FName + N else
    if FName <> '' then
      Result := 'def ' + FName + N else
      Result := 'def' + N;
    if (FResultType <> my_nil) and (FResultType <> my_variant) then
      Result := FResultType.Prototype(Result);
  end;
end;

function TLyFunc.FindBy(const ID: string; rec: PLyFind; Range: TLyFinds): boolean;
var
  I, M: string;
begin
  Result := (ID <> '');
  if Result then
  begin
    I := ExtractNameModule(ID, M);
    if M <> '' then
      Result := FModule.FindBy(I, M, rec) else
      Result := FindInside(I, rec) or FModule.FindBy(I, '', rec);
    if Result then
      Result := (Range = []) or (rec^.f_find in Range);
  end;
end;

function TLyFunc.IsConst: boolean;
begin
  Result := (FSTMTs <> nil) and (FSTMTs.Count = 1) and (FSTMTs[0].FStyle = ssConst);
end;

function TLyFunc.IsConstructor: boolean;
begin
  Result := (Self <> nil) and (FParent <> nil) and (FParent.FCreater = Self);
end;

function TLyFunc.IsFunction: boolean;
begin
  Result := (FResultType <> my_nil);
end;

function TLyFunc.IsMainFunc: boolean;
begin
  Result := FModule.IsMainModule and (FModule.FThread.FMainFunc = Self);
end;

function TLyFunc.IsMethod: boolean;
begin
  Result := (Self <> nil) and (FParent <> nil);
end;

function TLyFunc.IsProcedure: boolean;
begin
  Result := (FResultType = my_nil);
end;

function TLyFunc.FindInside(const ID: string; rec: PLyFind): boolean;
var
  R: RLyFind;
begin
  if rec = nil then rec := @R;
  rec^.VVarb := FParams.Find(ID);
  if rec^.VVarb <> nil then
  begin
    rec^.f_find := fiVarb;
    Result := true;
  end
  else
  if MatchID(ID, Name) then
  begin
    rec^.f_find := fiFunc;
    rec^.VFunc := Self;
    Result := true;
  end
  else Result := false;
end;

function TLyFunc.FindSave(const ID: string; Param: TLyParam; Outv: TLyValue): TLyFind;
var
  R: RLyFind;
begin
  if FindBy(ID, @R) then
  begin
    Result := R.f_find;
    if Result = fiVarb then
      Outv.Assign(Param.FParams[FParams.IndexOf(R.VVarb)]) else
      Outv.Assign(@R);
  end
  else Result := fiNone;
end;

{ TLyFuncList }

constructor TLyFuncList.Create;
begin
  FItems := TList.Create;
end;

destructor TLyFuncList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TLyFuncList.Find(const Name: string): TLyFunc;
var
  I: integer;
begin
  for I := 0 to GetCount - 1 do
  begin
    Result := GetItem(I);
    if MatchID(Name, Result.FName) then Exit;
  end;
  Result := nil;
end;

function TLyFuncList.GetCount: integer;
begin
  if FItems <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function TLyFuncList.GetItem(Index: integer): TLyFunc;
begin
  Result := TLyFunc(FItems[Index]);
end;

procedure TLyFuncList.Clear;
var
  I: integer;
begin
  for I := GetCount - 1 downto 0 do
    Delete(I);
end;

procedure TLyFuncList.Delete(Index: integer);
var
  F: TLyFunc;
begin
  F := GetItem(Index);
  FItems.Delete(Index);
  F.FModule := nil;
  F.FParent := nil;
  F.Free
end;

{ TLyModule }

function TLyModule.AddEnumType(const AName: string;
  const ItemNames: array of string): TLyEnumType;
begin
  Result := TLyEnumType.Create(AName, Self);
  Result.AddItems(ItemNames);
end;

function TLyModule.AddFunc(const AName: string; T: TLyType;
  const ParamNames: array of string; const ParamTypes: array of TLyType;
  const Proc: TLyProc): TLyFunc;
var
  I: integer;
begin
  Result := TLyFunc.Create(AName, Self, Proc);
  if T = nil then
    Result.FResultType := my_nil else
    Result.FResultType := T;
  for I := 0 to Length(ParamNames) - 1 do
    Result.FParams.Add(ParamNames[I], ParamTypes[I]);
end;

function TLyModule.AddFunc(const AName: string;
  const ParamNames: array of string; const ParamTypes: array of TLyType;
  const Proc: TLyProc): TLyFunc;
begin
  Result := AddFunc(AName, my_nil, ParamNames, ParamTypes, Proc);
end;

constructor TLyModule.Create(const AName: string);
begin
  inherited Create(AName);
  InitModule;
  if IsID(AName) then
    my_modules.Add(Self);
end;

constructor TLyModule.CreateEx(const AName: string; AContext: TLysee);
begin
  inherited Create(AName);
  InitModule;
  FEngine := AContext;
  if FEngine <> nil then
  begin
    FEngine.FModules.Add(Self);
    FModules := TLyModuleList.Create(FEngine);
    FModules.FImporter := Self;
    FImporters := TLyModuleList.Create(FEngine);
    FImporters.FImporter := Self;
    if FEngine.FRollbacks <> nil then
      FEngine.FRollbacks.Add(Self);
    Use(my_system);
  end
  else my_modules.Add(Self);
end;

procedure TLyModule.InitModule;
begin
  IncRefcount;
  FFileName := my_kernel;
  FTypeList := TList.Create;
  FEnumTypes := TList.Create;
  FFuncList := TLyFuncList.Create;
  FConsts := TLyList.Create;
//FConsts.CaseSensitive := false;
end;

destructor TLyModule.Destroy;
var
  A: integer;
  P: TLyModule;
begin
  SetPublic(false);
  if FEngine <> nil then
  begin
    FEngine.FModules.FModules.Remove(Self);
    FEngine.RollbackRemove(Self);

    for A := FEngine.FModules.Count - 1 downto 0 do
    begin
      P := FEngine.FModules[A];
      P.FImporters.FModules.Remove(Self);
      P.FModules.FModules.Remove(Self);
    end;

    if FThread <> nil then
      if FThread.FMainModule = Self then
      begin
        FThread.FMainModule := nil;
        FThread.FMainFunc := nil;
      end;

    FreeAndNil(FModules);
    FreeAndNil(FImporters);
  end;
  DeleteTypes;
  DeleteFunctions;
  FreeAndNil(FFuncList);
  FreeAndNil(FTypeList);
  FreeAndNil(FEnumTypes);
  FreeAndNil(FConsts);
  my_modules.FModules.Remove(Self);
  if FHandle <> 0 then FreeDLL(FHandle);
  inherited;
end;

procedure TLyModule.Define(const AName: string; Value: int64);
begin
  FConsts.Add(AName).AsInteger := Value;
end;

procedure TLyModule.Define(const AName, Value: string);
begin
  FConsts.Add(AName).AsString := Value;
end;

procedure TLyModule.DeleteFunctions;
begin
  FFuncList.Clear;
end;

procedure TLyModule.DeleteTypes;
var
  I: integer;
  T: TLyType;
begin
  for I := TypeCount - 1 downto 0 do
  begin
    T := GetType(I);
    FTypeList.Delete(I);
    T.Free;
  end;
end;

function TLyModule.GetType(Index: integer): TLyType;
begin
  Result := TLyType(FTypeList[Index]);
end;

function TLyModule.FindModule(const ID: string; FindPossible: boolean): TLyModule;
var
  I: integer;
begin
  if FModules <> nil then
  begin
    Result := FModules.Find(ID);
    if Result <> nil then Exit;

    for I := 0 to FEngine.FPublics.Count - 1 do
    begin
      Result := TLyModule(FEngine.FPublics[I]);
      if MatchID(ID, Result.FName) then Exit;
    end;

    for I := 0 to my_publics.Count - 1 do
    begin
      Result := TLyModule(my_publics[I]);
      if MatchID(ID, Result.FName) then Exit;
    end;
  end;

  Result := nil;

  if FindPossible then
  begin
    if FEngine <> nil then
      Result := FEngine.FModules.Find(ID);
    if Result = nil then
      Result := my_modules.Find(ID);
  end;
end;

function TLyModule.FindSave(const AName: string; Value: TLyValue): boolean;
var
  srec: RLyFind;
begin
  Result := (Self <> nil) and Find(AName, @srec);
  if Result then
    Value.Assign(@srec);
end;

function TLyModule.FindSaveBy(const AName: string; Value: TLyValue): boolean;
var
  srec: RLyFind;
begin
  Result := (Self <> nil) and FindBy(AName, '', @srec);
  if Result then
    value.Assign(@srec);
end;

function TLyModule.Use(M: TLyModule): boolean;
begin
  Result := (M <> nil) and (M <> Self);
  if Result and (M <> my_system) then
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

function TLyModule.IsMainModule: boolean;
begin
  Result := (FThread <> nil);
end;

function TLyModule.EnsureName(const AName: string): string;
begin
  if AName = '' then
  begin
    Inc(FEngine.FNameSeed);
    Result := Format('#%.3x', [FEngine.FNameSeed]);
  end
  else Result := AName;
end;

function TLyModule.FindFunc(const ID: string): TLyFunc;
begin
  Result := TLyFunc(FFuncList.Find(ID));
end;

function TLyModule.FindFuncBy(const ID: string; FindLibrary: boolean): TLyFunc;
var
  I: integer;
  M: TLyModule;
begin
  Result := FindFunc(ID);
  if Result <> nil then Exit;

  if FModules <> nil then
  begin
    for I := 0 to FModules.Count - 1 do
    begin
      Result := FModules.Modules[I].FindFunc(ID);
      if Result <> nil then Exit;
    end;

    for I := 0 to FEngine.FPublics.Count - 1 do
    begin
      M := TLyModule(FEngine.FPublics[I]);
      if M <> Self then
      begin
        Result := M.FindFunc(ID);
        if Result <> nil then Exit;
      end;
    end;
  end;

  if FindLibrary then
    for I := 0 to my_publics.Count - 1 do
    begin
      Result := TLyModule(my_publics[I]).FindFunc(ID);
      if Result <> nil then Exit;
    end;

  Result := nil;
end;

function TLyModule.FindType(const ID: string): TLyType;
var
  I: integer;
begin
  for I := 0 to FTypeList.Count - 1 do
  begin
    Result := TLyType(FTypeList[I]);
    if MatchID(ID, Result.FName) then Exit;
  end;
  Result := nil;
end;

function TLyModule.FindTypeBy(const ID, AModule: string): TLyType;
var
  I: integer;
  M: TLyModule;
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
    if Result <> nil then Exit;

    if FModules <> nil then
    begin
      for I := 0 to FModules.Count - 1 do
      begin
        Result := FModules[I].FindType(ID);
        if Result <> nil then Exit;
      end;

      for I := 0 to FEngine.FPublics.Count - 1 do
      begin
        M := TLyModule(FEngine.FPublics[I]);
        if M <> Self then
        begin
          Result := M.FindType(ID);
          if Result <> nil then Exit;
        end;
      end;
    end;

    for I := 0 to my_publics.Count - 1 do
    begin
      Result := TLyModule(my_publics[I]).FindType(ID);
      if Result <> nil then Exit;
    end;

    Result := nil;
  end;
end;

procedure TLyModule.SetPublic(Value: boolean);
begin
  if FPublic <> Value then
  begin
    FPublic := Value;
    if FPublic then
    begin
      if FEngine <> nil then
        FEngine.FPublics.Add(Self) else
        my_publics.Add(Self);
    end
    else
    if FEngine <> nil then
      FEngine.FPublics.Remove(Self) else
      my_publics.Remove(Self);
  end;
end;

procedure TLyModule.Setup;
begin
  if Assigned(FOnSetup) then
  try
    FOnSetup(Self);
  finally
    FOnSetup := nil;
  end;
end;

function TLyModule.SetupType(const T: TLyType): boolean;
begin
  Result := (T <> nil) and IsID(T.FName) and not Find(T.FName);
  if Result then
  begin
    T.FModule := Self;
    FTypeList.Add(T);
  end
  else T.Free;
end;

function TLyModule.Find(const ID: string; rec: PLyFind): boolean;
var
  FR: RLyFind;
  I: integer;
begin
  if rec = nil then rec := @FR;
  rec^.f_find := fiNone;
  Result := true;

  rec^.VModule := FindModule(ID, false);
  if rec^.VModule <> nil then
  begin
    rec^.f_find := fiModule;
    Exit;
  end;

  rec^.VFunc := FindFunc(ID);
  if rec^.VFunc <> nil then
  begin
    rec^.f_find := fiFunc;
    Exit;
  end;

  rec^.vtype := FindType(ID);
  if rec^.vtype <> nil then
  begin
    rec^.f_find := fiType;
    Exit;
  end;

  for I := 0 to FEnumTypes.Count - 1 do
  begin
    rec^.VEnum := TLyEnumType(FEnumTypes[I]).Find(ID);
    if rec^.vtype <> nil then
    begin
      rec^.f_find := fiEnum;
      Exit;
    end;
  end;

  rec^.VValue := FConsts.Values[ID];
  if rec^.VValue <> nil then
  begin
    rec^.f_find := fiValue;
    Exit;
  end;

  Result := false;
end;

function TLyModule.FindBy(const ID, AModule: string; rec: PLyFind): boolean;
var
  I: integer;
  M: TLyModule;
begin
  if AModule <> '' then
  begin
    M := FindModule(AModule, true);
    Result := (M <> nil) and M.Find(ID, rec);
  end
  else
  begin
    Result := Find(ID, rec);
    if Result then Exit;

    if FModules <> nil then
    begin
      for I := 0 to FModules.Count - 1 do
      begin
        Result := FModules.Modules[I].Find(ID, rec);
        if Result then Exit;
      end;

      for I := 0 to FEngine.FPublics.Count - 1 do
      begin
        M := TLyModule(FEngine.FPublics[I]);
        if M <> Self then
        begin
          Result := M.Find(ID, rec);
          if Result then Exit;
        end;
      end;
    end;

    for I := 0 to my_publics.Count - 1 do
    begin
      Result := TLyModule(my_publics[I]).Find(ID, rec);
      if Result then Exit;
    end;

    Result := false;
  end;
end;

function TLyModule.TypeCount: integer;
begin
  Result := FTypeList.Count;
end;

function TLyModule.Use(const AModule: string): boolean;
begin
  if FEngine <> nil then
  begin
    Result := Use(FEngine.FModules.Find(AModule));
    if Result then Exit;
  end;
  Result := Use(my_modules.Find(AModule));
end;

function TLyModule.UseModule(const AName, AFile: string; AThread: TLyThread): TLyModule;
begin
  Result := AThread.LoadModule(AName, AFile, true);
  Use(Result);
end;

function TLyModule.AddFunc(const AName: string; T: TLyType;
  const Proc: TLyProc): TLyFunc;
begin
  Result := AddFunc(AName, T, [], [], Proc);
end;

function TLyModule.AddFunc(const AName: string; const Proc: TLyProc): TLyFunc;
begin
  Result := AddFunc(AName, my_nil, [], [], Proc);
end;

{ TLyModuleList }

function TLyModuleList.Add(AModule: TLyModule): integer;
begin
  Result := IndexOf(AModule);
  if Result < 0 then
    Result := FModules.Add(AModule);
end;

procedure TLyModuleList.Clear;
var
  A: integer;
begin
  DeleteFunctions;
  ClearConsts;
  for A := FModules.Count - 1 downto 0 do
    Delete(A);
end;

procedure TLyModuleList.ClearConsts;
var
  I: integer;
begin
  if FImporter = nil then
    for I := 0 to GetCount - 1 do
      GetModule(I).FConsts.Clear;
end;

constructor TLyModuleList.Create(AContext: TLysee);
begin
  IncRefcount;
  FContext := AContext;
  FModules := TList.Create;
end;

procedure TLyModuleList.Delete(Index: integer);
var
  M: TLyModule;
begin
  M := GetModule(Index);
  FModules.Delete(Index);
  if FImporter = nil then M.Free; {<--TLysee.FModules/my_modules}
end;

destructor TLyModuleList.Destroy;
begin
  Clear;
  FreeAndNil(FModules);
  inherited;
end;

function TLyModuleList.Find(const Name: string): TLyModule;
var
  X: integer;
begin
  X := IndexOf(Name);
  if X >= 0 then
    Result := GetModule(X) else
    Result := nil;
end;

function TLyModuleList.GetCount: integer;
begin
  Result := FModules.Count;
end;

function TLyModuleList.GetModule(Index: integer): TLyModule;
begin
  Result := TLyModule(FModules[Index]);
end;

function TLyModuleList.Has(const Name: string): boolean;
begin
  Result := (IndexOf(Name) >= 0);
end;

function TLyModuleList.Has(AModule: TLyModule): boolean;
begin
  Result := (IndexOf(AModule) >= 0);
end;

function TLyModuleList.IndexOf(const Name: string): integer;
var
  X: integer;
  M: TLyModule;
begin
  if Name <> '' then
    for X := 0 to FModules.Count - 1 do
    begin
      M := TLyModule(FModules[X]);
      if MatchID(M.Name, Name) then
      begin
        Result := X;
        Exit;
      end;
    end;
  Result := -1;
end;

procedure TLyModuleList.Setup;
var
  I: integer;
begin
  for I := 0 to GetCount - 1 do
    GetModule(I).Setup;
end;

function TLyModuleList.IndexOf(AModule: TLyModule): integer;
begin
  Result := FModules.IndexOf(AModule);
end;

procedure TLyModuleList.DeleteFunctions;
var
  index: integer;
begin
  if FImporter = nil then
    for index := 0 to GetCount - 1 do
      GetModule(index).DeleteFunctions;
end;

function TLyModuleList.ToList: TLyList;
var
  index: integer;
begin
  Result := TLyList.Create;
  for index := 0 to GetCount - 1 do
    Result.Add.Assign(my_module, GetModule(index));
end;

{ TLyGarbage }

procedure TLyGarbage.Clear;
begin

end;

constructor TLyGarbage.Create;
begin
  if my_gcman <> nil then my_gcman.GcAdd(Self);
end;

destructor TLyGarbage.Destroy;
begin
  if my_gcman <> nil then
  begin
    my_gcman.GcRemove(Self);
    my_gcman.FDead.Remove(Self);
  end;
  inherited;
end;

procedure TLyGarbage.MarkForSurvive;
begin
  { do nothing }
end;

{ TLyCollect }

function TLyCollect.Collect: integer;

  procedure change_ref(Increase: boolean);
  var
    I: integer;
    G: TLyGarbage;
  begin
    for I := 0 to FDead.Count - 1 do
      if I < FDead.Count then
      begin
        G := TLyGarbage(FDead[I]);
        if Increase then
          G.IncRefcount else
          G.DecRefcount;
      end;
  end;

var
  I: integer;
  G: TLyGarbage;
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
        TLyGarbage(FDead[I]).Clear;
  finally
    change_ref(false);
  end;

  // destroy rest containers
  for I := FDead.Count - 1 downto 0 do
    if I < FDead.Count then
    begin
      G := TLyGarbage(FDead[I]);
      FDead.Delete(I);
      G.Free;
    end;
end;

procedure TLyCollect.MarkSurvived;
var
  I: integer;
begin
  for I := 0 to FContexts.Count - 1 do
    TLysee(FContexts[I]).MarkSurvived;
end;

constructor TLyCollect.Create;
begin
  FContexts := TList.Create;
  FDead := TList.Create;
end;

destructor TLyCollect.Destroy;
begin
  FreeAndNil(FDead);
  FreeAndNil(FContexts);
  inherited;
end;

procedure TLyCollect.GcAdd(G: TLyGarbage);
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

procedure TLyCollect.GcRemove(G: TLyGarbage);
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

{ TLyList }

destructor TLyList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

procedure TLyList.PrepareFor(Func: TLyFunc);
var
  I, N: integer;
  F: TLyType;
  V: TLyValue;
begin
  N := GetCount;
  SetCount(Func.FParams.Count);
  for I := 0 to Func.FParams.Count - 1 do
  begin
    F := Func.FParams[I].FType;
    V := GetItem(I);
    if I >= N then F.SetDefValue(V) else
    if (F <> my_variant) and (F <> V.FType) then
      if not V.FType.IsChildOf(F) then
        F.Convert(V);
  end;
end;

function TLyList.Put(const Name: string; Value: int64): TLyValue;
begin
  Result := Put(Name, TLyValue(nil));
  Result.AsInteger := Value;
end;

function TLyList.Put(const Name, Value: string): TLyValue;
begin
  Result := Put(Name, TLyValue(nil));
  Result.AsString := Value;
end;

function TLyList.Put(const Name: string; Value: double): TLyValue;
begin
  Result := Put(Name, TLyValue(nil));
  Result.AsFloat := Value;
end;

function TLyList.Put(const Name: string; Value: boolean): TLyValue;
begin
  Result := Put(Name, TLyValue(nil));
  Result.AsBoolean := Value;
end;

procedure TLyList.Remove(const Name: string);
var
  I: integer;
begin
  I := IndexOf(Name);
  if I >= 0 then Delete(I);
end;

function TLyList.Put(const Name: string; Value: currency): TLyValue;
begin
  Result := Put(Name, TLyValue(nil));
  Result.AsCurrency := Value;
end;

function TLyList.Put(const Name: string; Value: TLyValue): TLyValue;
var
  I: integer;
begin
  I := FItems.IndexOf(Name);
  if I >= 0 then
  begin
    Result := GetItem(I);
    Result.Assign(Value);
  end
  else Result := Add(Name, Value);
end;

procedure TLyList.Remove(Value: TLyValue);
var
  I: integer;
begin
  I := IndexOf(Value);
  if I >= 0 then Delete(I);
end;

function TLyList.CopyRight(ItemCount: integer): TLyList;
begin
  Result := Copy(GetCount - ItemCount, ItemCount);
end;

function TLyList.Add(Value: TLyValue): TLyValue;
begin
  Result := Add('', Value);
end;

function TLyList.Add: TLyValue;
begin
  Result := Add('', nil);
end;

function TLyList.Add(AType: TLyType; AData: pointer): TLyValue;
begin
  Result := Add('', nil);
  Result.Assign(AType, AData);
end;

function TLyList.Add(const Name: string): TLyValue;
begin
  Result := Add(Name, nil);
end;

function TLyList.Add(Strs: TStrings): integer;
var
  I: integer;
begin
  Result := Strs.Count;
  for I := 0 to Result - 1 do
    Add('', nil).AsString := Strs[I];
end;

function TLyList.AsEnumSet: TLyEnumSet;
var
  T: TLyEnumSetType;
begin
  T := EnumSetType;
  if T <> nil then
  begin
    Result := T.NewEnumSet;
    Result.Assign(Self);
  end
  else Result := nil;
end;

procedure TLyList.Assign(AList: TLyValue);
var
  G: TLyGenerate;
begin
  G := GetGenerate(AList);
  if G <> nil then
  try
    while G.GetNext do
      Add(G);
  finally
    G.Free;
  end;
end;

procedure TLyList.AssignNames(AList: TLyList);
begin
  Clear;
  Add(AList.FItems);
end;

procedure TLyList.Assign(AList: TLyList);
var
  I: integer;
begin
  Clear;
  for I := 0 to AList.Count - 1 do
    Add(AList.FItems[I], AList.GetItem(I));
end;

function TLyList.Format(const Fmt: string): string;
const
  CS_FMTINT    = ['d', 'u', 'x'];
  CS_FMTFLOAT  = ['e', 'f', 'g', 'n', 'm'];
  CS_FMTCHAR   = ['c'];
  CS_FMTSTRING = ['s'];
  CS_FMTPTR    = ['p'];
  CS_FORMAT    = CS_FMTINT + CS_FMTFLOAT + CS_FMTCHAR + CS_FMTSTRING + CS_FMTPTR;
var
  fmts: string;
  args: array of TVarRec;
  exts: array of Extended;
  ints: array of int64;
  strs: array of string;
  argc, X: integer;
  data: TLyValue;
  fmtc: char;

  function format_chars(const F: pchar): string;
  var
    next: pchar;
  begin
    Result := '';
    next := SeekChar(F, ['%']);
    while next <> nil do
    begin
      Inc(next);
      if next^ <> '%' then
      begin
        while (next^ <> #0) and CharInSet(next^, ['-', ':', '.', '0'..'9']) do Inc(next);
        if not CharInSet(next^, CS_FORMAT) then
          Throw('invalid format string: ''%s''', [F]);
        Result := Result + next^;
      end;
      Inc(next);
      next := SeekChar(next, ['%']);
    end;
  end;

begin
  fmts := format_chars(PChar(Fmt));
  if fmts = '' then
  begin
    Result := Fmt;
    Exit;
  end;

  argc := Min(Count, Length(fmts));
  if argc < Length(fmts) then
  begin
    Throw('need %d more formating arguments', [Length(fmts) - argc]);
    Exit;
  end;

  SetLength(args, argc);
  SetLength(exts, argc);
  SetLength(strs, argc);
  SetLength(ints, argc);
  try
    for X := 0 to argc - 1 do
    begin
      data := GetItem(X);
      fmtc := fmts[X + 1];
      if CharInSet(fmtc, CS_FMTSTRING) then
      begin
        strs[X] := data.AsString;
        {$IFDEF UNICODE}
        args[X].VType := vtPWideChar;
        args[X].VPWideChar := pchar(strs[X]);
        {$ELSE}
        args[X].VType := vtPChar;
        args[X].VPChar := pchar(strs[X]);
        {$ENDIF}
      end
      else
      if CharInSet(fmtc, CS_FMTCHAR) then
      begin
        args[X].VType := vtChar;
        {$IFDEF UNICODE}
        args[X].VType := vtWideChar;
        args[X].VWideChar := data.AsChar;
        {$ELSE}
        args[X].VType := vtChar;
        args[X].VChar := data.AsChar;
        {$ENDIF}
      end
      else
      if CharInSet(fmtc, CS_FMTINT) then
      begin
        ints[X] := data.AsInteger;
        args[X].VType := vtInt64;
        args[X].VInt64 := @ints[X];
      end
      else
      if CharInSet(fmtc, CS_FMTFLOAT) then
      begin
        exts[X] := data.AsFloat;
        args[X].VType := vtExtended;
        args[X].VExtended := @exts[X];
      end
      else
      if CharInSet(fmtc, CS_FMTPTR) then
      begin
        args[X].VType := vtPointer;
        args[X].VPointer := data.FData;
      end
      else
      begin
        Throw('unknown format: ''%s''', [fmtc]);
        Break;
      end;
    end;
    Result := SysUtils.Format(Fmt, args);
  finally
    SetLength(args, 0);
    SetLength(exts, 0);
    SetLength(strs, 0);
    SetLength(ints, 0);
  end;
end;

procedure TLyList.Clear;
var
  I: integer;
begin
  for I := GetCount - 1 downto 0 do
    Delete(I);
  inherited;
end;

function TLyList.Clone: TLyList;
begin
  Result := TLyList.Create;
  Result.Assign(Self);
end;

function TLyList.Copy(Index, ItemCount: integer): TLyList;
var
  I: integer;
begin
  Result := TLyList.Create;
  if Index < 0 then
  begin
    Inc(ItemCount, Index);
    Index := 0;
  end;
  ItemCount := Max(0, Min(GetCount - Index, ItemCount));
  for I := 1 to ItemCount do
  begin
    Result.Add(FItems[Index], GetItem(Index));
    Inc(Index);
  end;
end;

constructor TLyList.Create;
begin
  inherited;
  FItems := TStringList.Create;
  FReadonly := false;
end;

procedure TLyList.Delete(Index: integer);
var
  V: TLyValue;
begin
  V := GetItem(Index);
  FItems.Delete(Index);
  V.Free;
end;

procedure TLyList.DeleteLast;
var
  I: integer;
begin
  I := GetCount - 1;
  if I >= 0 then Delete(I);
end;

function TLyList.EnumSetType: TLyEnumSetType;
var
  E: TLyEnumType;
  T: TLyType;
  I: integer;
begin
  E := EnumType;
  if E <> nil then
    for I := 0 to E.FModule.TypeCount - 1 do
    begin
      T := E.FModule.GetType(I);
      if T.IsEnumSetType then
      begin
        Result := TLyEnumSetType(T);
        if Result.FSource = E then Exit;
      end;
    end;
  Result := nil;
end;

function TLyList.EnumType: TLyEnumType;
var
  I: integer;
  V: TLyValue;
begin
  Result := nil;
  if Self <> nil then
    for I := 0 to Count - 1 do
    begin
      V := GetItem(I);
      if V.FType.IsEnumType then
      begin
        if Result = nil then
          Result := TLyEnumType(V.FType) else
        if Result <> V.FType then
        begin
          Result := nil;
          Exit;
        end;
      end
      else
      if V.FType <> my_int then
      begin
        Result := nil;
        Exit;
      end;
    end;
end;

procedure TLyList.Exchange(Index1, Index2: integer);
begin
  FItems.Exchange(Index1, Index2);
end;

function TLyList.GetFirst: TLyValue;
begin
  Result := GetItem(0);
end;

function TLyList.GetCount: integer;
begin
  if Self <> nil then
    Result := FItems.Count else
    Result := 0;
end;

procedure TLyList.GetFirstTwo(var V1, V2: TLyValue);
var
  N: integer;
begin
  V1 := nil;
  V2 := nil;
  N := GetCount;
  if N > 0 then V1 := GetItem(0);
  if N > 1 then V2 := GetItem(1);
end;

function TLyList.GetItem(Index: integer): TLyValue;
begin
  Result := TLyValue(FItems.Objects[Index]);
end;

function TLyList.GetName(Index: integer): string;
begin
  Result := FItems[Index];
end;

function TLyList.GetValue(const Name: string): TLyValue;
var
  I: integer;
begin
  I := FItems.IndexOf(Name);
  if I >= 0 then
    Result := GetItem(I) else
    Result := nil;
end;

function TLyList.IndexOf(Value: TLyValue): integer;
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

function TLyList.Insert(Index: integer; const Name: string; Value: TLyValue): TLyValue;
begin
  Result := TLyValue.Create;
  FItems.InsertObject(Index, Name, Result);
  if Value <> nil then
    Result.Assign(Value);
end;

function TLyList.Insert(Index: integer; Value: TLyValue): TLyValue;
begin
  Result := Insert(Index, '', Value);
end;

function TLyList.Insert(Index: integer; const Name: string): TLyValue;
begin
  Result := Insert(Index, Name, nil);
end;

function TLyList.GetLast: TLyValue;
begin
  Result := GetItem(GetCount - 1);
end;

function TLyList.CopyLeft(ItemCount: integer): TLyList;
begin
  Result := Copy(0, ItemCount);
end;

procedure TLyList.MarkForSurvive;
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

procedure TLyList.Move(CurIndex, NewIndex: integer);
begin
  FItems.Move(CurIndex, NewIndex);
end;

function TLyList.Add(const Name: string; Value: TLyValue): TLyValue;
begin
  Result := TLyValue.Create;
  FItems.AddObject(Name, Result);
  if Value <> nil then
    Result.Assign(Value);
end;

procedure TLyList.SetCount(NewCount: integer);
begin
  if NewCount < 1 then Clear else
  begin
    while NewCount > GetCount do Add;
    while NewCount < GetCount do DeleteLast;
  end;
end;

procedure TLyList.SetItem(Index: integer; const Value: TLyValue);
begin
  GetItem(Index).Assign(Value);
end;

procedure TLyList.SetName(Index: integer; const Value: string);
begin
  FItems[Index] := Value;
end;

procedure TLyList.SetValue(const Name: string; Value: TLyValue);
begin
  Put(Name, Value);
end;

procedure TLyList.Sort;

  procedure do_sort(L, R: integer);
  var
    I, J, P: Integer;
    V: TLyValue;
  begin
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        V := GetItem(P);
        while GetItem(I).Compare(V) = crLess do Inc(I);
        while GetItem(J).Compare(V) = crMore do Dec(J);
        if I <= J then
        begin
          if I <> J then FItems.Exchange(I, J);
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
  do_sort(0, GetCount - 1);
end;

procedure TLyList.SortByName;
begin
  FItems.Sort;
end;

procedure TLyList.TestChange;
begin
  if FReadonly then
    raise Exception.Create('array/list is readonly');
end;

function TLyList.ToString: string;

  function format_item(I: integer): string;
  begin
    if FItems[I] <> '' then
      Result := FormatString(FItems[I]) + '=' + FormatValue(GetItem(I)) else
      Result := FormatValue(GetItem(I));
  end;

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
        Result := '[' + format_item(0);
        for X := 1 to N - 1 do
        begin
          Result := Result + ', ';
          Result := Result + format_item(X);
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

function TLyList.IndexOf(const Name: string): integer;
begin
  Result := FItems.IndexOf(Name);
end;

function TLyList.Insert(Index: integer): TLyValue;
begin
  Result := Insert(Index, '', nil);
end;

procedure TLyList.ListNames(AList: TLyList);
begin
  AList.Add(FItems);
end;

procedure TLyList.AssignValues(AList: TLyList);
var
  I: integer;
begin
  Clear;
  for I := 0 to AList.GetCount - 1 do
    Add(AList.GetItem(I));
end;

{ TLyGenerate }

function TLyGenerate.GetNext: boolean;
begin
  Result := false;
end;

function TLyGenerate.HasNext: boolean;
begin
  Result := false;
end;

{ TLyStringGenerate }

constructor TLyStringGenerate.CreateIn(const S: string);
begin
  inherited Create;
  FS := S;
  FIndex := 0;
end;

function TLyStringGenerate.GetNext: boolean;
begin
  Result := HasNext;
  if Result then
  begin
    Inc(FIndex);
    SetAsChar(FS[FIndex]);
  end
  else Clear;
end;

function TLyStringGenerate.HasNext: boolean;
begin
  Result := (FIndex < Length(FS));
end;

{ TLyListGenerate }

constructor TLyListGenerate.CreateIn(const AList: TLyList);
begin
  inherited Create;
  FL := AList;
  FIndex := -1;
end;

function TLyListGenerate.GetNext: boolean;
begin
  Result := HasNext;
  if Result then
  begin
    Inc(FIndex);
    Assign(FL[FIndex]);
  end
  else Clear;
end;

function TLyListGenerate.HasNext: boolean;
begin
  Result := (FL <> nil) and (FIndex < FL.Count - 1);
end;

{ TLyIntGenerate }

constructor TLyIntGenerate.CreateIn(V1, V2: int64; Upto: boolean);
begin
  inherited Create;
  FV := V1;
  FUpto := Upto;
  if FUpto then
    FCount := V2 - FV + 1 else
    FCount := FV - V2 + 1;
end;

constructor TLyIntGenerate.CreateIn(Range: int64);
begin
  CreateIn(0, Range - 1, true);
end;

function TLyIntGenerate.GetNext: boolean;
begin
  Result := HasNext;
  if Result then
  begin
    SetAsInteger(FV);
    if FUpto then Inc(FV) else Dec(FV);
    Dec(FCount);
  end
  else Clear;
end;

function TLyIntGenerate.HasNext: boolean;
begin
  Result := (FCount > 0);
end;

{ TLyCharGenerate }

constructor TLyCharGenerate.CreateIn(V1, V2: char; Upto: boolean);
begin
  inherited Create;
  FV := V1;
  FUpto := Upto;
  if FUpto then
    FCount := Ord(V2) - Ord(FV) + 1 else
    FCount := Ord(FV) - Ord(V2) + 1;
end;

function TLyCharGenerate.GetNext: boolean;
begin
  Result := HasNext;
  if Result then
  begin
    SetAsChar(FV);
    if FUpto then Inc(FV) else Dec(FV);
    Dec(FCount);
  end
  else Clear;
end;

function TLyCharGenerate.HasNext: boolean;
begin
  Result := (FCount > 0);
end;

{ TLyBoolGenerate }

constructor TLyBoolGenerate.CreateIn(V1, V2, Upto: boolean);
begin
  inherited Create;
  FV := V1;
  FUpto := Upto;
  if FUpto then
    FCount := Ord(V2) - Ord(FV) + 1 else
    FCount := Ord(FV) - Ord(V2) + 1;
end;

function TLyBoolGenerate.GetNext: boolean;
begin
  Result := HasNext;
  if Result then
  begin
    SetAsBoolean(FV);
    FV := not FV;
    Dec(FCount);
  end
  else Clear;
end;

function TLyBoolGenerate.HasNext: boolean;
begin
  Result := (FCount > 0);
end;

{ TLyEnumGenerate }

constructor TLyEnumGenerate.CreateIn(V1, V2: TLyEnumItem; Upto: boolean);
begin
  inherited Create;
  FV := V1;
  FUpto := Upto;
  if (V1 = nil) or (V2 = nil) or (V1.FParent <> V2.FParent) then FCount := 0 else
  if FUpto then
    FCount := V2.FValue - FV.FValue + 1 else
    FCount := FV.FValue - V2.FValue + 1;
end;

function TLyEnumGenerate.GetNext: boolean;
begin
  Result := HasNext;
  if Result then
  begin
    SetAsEnum(FV);
    if FUpto then
      FV := FV.FParent.Find(FV.FValue + 1) else
      FV := FV.FParent.Find(FV.FValue - 1);
    Dec(FCount);
  end
  else Clear;
end;

function TLyEnumGenerate.HasNext: boolean;
begin
  Result := (FV <> nil) and (FCount > 0);
end;

{ TLySystemModule }

procedure TLySystemModule.DoSetup(Sender: TObject);
var
  I: integer;
begin
  OnSetup := nil;
  lysee_system.Setup;
  lysee_db.Setup;
  {$IFNDEF FPC}
  lysee_adodb.Setup;
  {$ENDIF}
  lysee_pmc.Setup;
  for I := 0 to TypeCount - 1 do
    GetType(I).Setup;
end;

constructor TLySystemModule.Create(const AName: string);
begin
  inherited;
  OnSetup := {$IFDEF FPC}@{$ENDIF}DoSetup;

  //-- types -----------------------------------------------------------------

  my_variant := TLyVariantType.Create('variant', Self);
  my_nil := TLyNilType.Create('nil', Self);
  my_char := TLyCharType.Create('char', Self);
  my_int := TLyIntegerType.Create('integer', Self);
  my_float := TLyFloatType.Create('double', Self);
  my_curr := TLyCurrencyType.Create('currency', Self);
  my_time := TLyTimeType.Create('TDateTime', Self);
  my_bool := TLyBooleanType.Create('boolean', Self);
  my_object := TLyObjectType.Create('object', Self);
  my_array := TLyArrayType.Create('array', Self);
  my_string := TLyStringType.Create('string', Self);
  my_type := TLyTypeType.Create('type', Self);
  my_error := TLyErrorType.Create('exception', Self);
  my_module := TLyModuleType.Create('module', Self);
  my_func := TLyFuncType.Create('function', Self);

  //-- constant --------------------------------------------------------------

  Define('MaxInt', High(int64));
  Define('MinInt', Low(int64));
  Define('PathDelim', PathDelim);
  Define('PathSep', PathSep);
  FConsts.Add('PI').SetAsFloat(PI);
  Define('PointerSize', sizeof(pointer));
  Define('CharSize', sizeof(char));
  Define('sLineBreak', sLineBreak);

  //-- functions -------------------------------------------------------------

  AddFunc('ExceptObject', my_error, {$IFDEF FPC}@{$ENDIF}pp_exceptObject);
  AddFunc('Halt', ['_Value'], [my_variant], {$IFDEF FPC}@{$ENDIF}pp_halt);
  AddFunc('Exit', ['_Value'], [my_variant], {$IFDEF FPC}@{$ENDIF}pp_exit);
  AddFunc('Break', {$IFDEF FPC}@{$ENDIF}pp_break);
  AddFunc('Continue', {$IFDEF FPC}@{$ENDIF}pp_continue);
  AddFunc('Compile', my_func, ['Code'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_compile);
  AddFunc('Eval', my_variant, ['Expression'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_eval);
  AddFunc('Find', my_variant, ['Name'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_get);
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
  AddFunc('Readln', my_string,
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
  my_module_path := IncludeTrailingPathDelimiter(my_knpath + 'modules');
  {$IFDEF MSWINDOWS}
  my_tmpath := GetEnv(['TEMP', 'TMP'], my_knpath + '..\temp\');
  {$ELSE}
  my_tmpath := GetEnv(['TEMP', 'TMP'], '/tmp/');
  {$ENDIF}
  my_spinlock := Syncobjs.TCriticalSection.Create;
  my_gcman := TLyCollect.Create;
  my_modules := TLyModuleList.Create(nil);
  my_publics := TList.Create;
  my_system := TLySystemModule.Create(LSE_SYSTEM);
  my_system.SetPublic(true);
  my_system.Setup;
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

