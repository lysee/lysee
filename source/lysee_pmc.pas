{==============================================================================}
{        UNIT: lysee_pmc                                                       }
{ DESCRIPTION: lysee module compiler                                           }
{   COPYRIGHT: Copyright (c) 2012-2016, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2012/02/17                                                      }
{    MODIFIED: 2020/02/16                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_pmc;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, Types, basic, Lysee, lysee_system;

type

  TLyPasTranslater = class; {forward}

  { TLyLseType }

  TLyLseType = class
  private
    FType: TLyType;       // mis: my_string
    FMyName: string;      //     'my_string'
    FLyNameType: string;  //     'TLyStringType'
    FPasTypeName: string; //     'string'
    function GetName: string;
  public
    constructor Create(AType: TLyType; const AMyName, APasTypeName, ALyNameType: string);
    property VType: TLyType read FType;
    property Name: string read GetName;
    property MyName: string read FMyName;
    property TLyNameType: string read FLyNameType;
    property PasTypeName: string read FPasTypeName;
  end;

  { TLyLseTypeList }

  TLyLseTypeList = class
  private
    FItems: TList;
    function GetItem(Index: integer): TLyLseType;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    function Add(AType: TLyType; const MyName, PasTypeName, ALyNameType: string): TLyLseType;
    function Find(AType: TLyType): TLyLseType;overload;
    function Find(const AName: string): TLyLseType;overload;
    property Count: integer read GetCount;
    property Items[Index: integer]: TLyLseType read GetItem;default;
  end;

  { TLyPasType }

  TLyPasType = class
  private
    FName: string;
    FLseType: TLyLseType;
    function GetLseTypeName: string;
  public
    constructor Create(const TypeName: string);
    property Name: string read FName;
    property LseType: TLyLseType read FLseType;
    property LseTypeName: string read GetLseTypeName;
  end;

  { TLyPasTypeList }

  TLyPasTypeList = class
  private
    FItems: TList;
    function GetItem(Index: integer): TLyPasType;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    function Add(AType: TLyLseType; const AName: string): TLyPasType;overload;
    procedure Add(AType: TLyType; const Types: array of string);overload;
    function Find(const AName: string): TLyPasType;
    function FindByName(const AName: string): TLyPasType;
    property Count: integer read GetCount;
    property Items[Index: integer]: TLyPasType read GetItem;default;
  end;

  { TLyPasVarb }

  TLyPasVarb = class
  private
    FName: string;
    FPasType: TLyPasType;
    function GetLseType: TLyLseType;
    function GetLseTypeName: string;
    function GetPasTypeName: string;
  public
    constructor Create(const AName: string; AType: TLyPasType);virtual;
    property Name: string read FName;
    property PasType: TLyPasType read FPasType write FPasType;
    property PasTypeName: string read GetPasTypeName;
    property LseType: TLyLseType read GetLseType;
    property LseTypeName: string read GetLseTypeName;
  end;

  { TLyPasVarbList }

  TLyPasVarbList = class
  private
    FItems: TList;
    function GetItem(Index: integer): TLyPasVarb;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    function Add(const AName: string; AType: TLyPasType): TLyPasVarb;overload;
    function Add(const AName: string): TLyPasVarb;overload;
    property Count: integer read GetCount;
    property Items[Index: integer]: TLyPasVarb read GetItem;default;
  end;

  { TLyPasFunc }

  TLyFuncStyle = (fsNormal, fsMethod, fsGet, fsSet, fsCreate);

  TLyPasFunc = class(TLyPasVarb)
  private
    FParent: TLyPasType;
    FParams: TLyPasVarbList;
    FStyle: TLyFuncStyle;
    FIsDefault: boolean;
    FGetSet: TLyPasFunc;
  public
    procedure GenBodyCode(Codes: TStrings);
    procedure GenSetupCode(Codes: TStrings; const MyModule: string);
    procedure Clear;
    function OK: boolean;
  public
    constructor Create(const AName: string; AType: TLyPasType);override;
    destructor Destroy;override;
    function Clone: TLyPasFunc;
    function HasResult: boolean;
    function LyseeProcName: string;
    function LyseeProcPrototype(UToL: boolean): string;
    function IsMethod: boolean;
    property Params: TLyPasVarbList read FParams;
    property Parent: TLyPasType read FParent;
    property Style: TLyFuncStyle read FStyle;
  end;

  { TLyPasFuncList }

  TLyPasFuncList = class
  private
    FItems: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TLyPasFunc;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    procedure Remove(AFunc: TLyPasFunc);
    function Add(const AName: string; AType: TLyPasType): TLyPasFunc;overload;
    function Add(const AName: string): TLyPasFunc;overload;
    function ListMethods(AType: TLyLseType): TList;
    property Count: integer read GetCount;
    property Items[Index: integer]: TLyPasFunc read GetItem;default;
  end;

  { TLyPasToken}

  TLyPasSymbol = (psUnit, psInterface, psUses, psType, psClass, psPacked,
    psRecord, psConst, psVar, psArray, psProp, psFunc, psProc,
    psConstructor, psDestructor, psIn, psOut, psOf, psPrivate,
    psProtected, psPublic, psPublished, psInline, psPlatform,
    psImplementation, psBegin, psEnd, psEqual, psLParen, psRParen,
    psLArray, psRArray, psColon, psSemic, psComma, psID, psNone, psEOF);
  TLyPasSymbols = set of TLyPasSymbol;

  TLyPasToken = class
  private
    FSym : TLyPasSymbol;
    FName: string;
  public
    property Sym: TLyPasSymbol read FSym write FSym;
    property Name: string read FName write FName;
  end;

  { TLyPasTokenizer }

  TLyPasTokenizer = class(TLyPasToken)
  private
    FCode: string;
    FSize: integer;
    FPosition: integer;
    FChar: char;
    procedure SetCode(const Value: string);
  protected
    function SkipSpaces: boolean;
    function GetChar: boolean;
    function PeekChar: char;
    function GotoChar(Chars: TSysCharSet): boolean;
    function GetToken(T: TLyPasToken): boolean;
  public
    constructor Create;
    function GetNext: boolean;overload;
    function GetNext(ASym: TLyPasSymbol): boolean;overload;
    function GetNext(Sym1, Sym2: TLyPasSymbol): boolean;overload;
    function GetNext(Syms: TLyPasSymbols): boolean;overload;
    function GotoToken(Syms: TLyPasSymbols): boolean;
    function GotoID(const ID: string): boolean;
    function PeekNext: TLyPasSymbol;
    property Code: string read FCode write SetCode;
  end;

  { TLyPasTranslater }

  TLyPasTranslater = class(TLyPasTokenizer)
  private
    FModule: string;
    FUses: string;
    FM: TLyModule;
    FNewLseTypes: TLyLseTypeList;
    FNewPasTypes: TLyPasTypeList;
    FConstants: TStringList;
    FFuncs: TLyPasFuncList;
    FTemp: TLyPasFunc;
    FSourceCodes: TStrings;
    FResultCodes: TStrings;
    FMyModule: string;
    FUnitToLibrary: boolean;
    function GetResultUnitName: string;
  protected
    function ParseVarSection: boolean;
    function ParseConstSection: boolean;
    function ParseTypeSection: boolean;
    function ParseClass(const AName: string): boolean;
    function ParseFunc(FuncSym: TLyPasSymbol; AClass: TLyPasType): boolean;
    function ParseProp(AClass: TLyPasType): boolean;
    function ParseField(AClass: TLyPasType): boolean;
    function ParseInterface(const AName: string): boolean;
    function ParseToSemic: boolean;
    procedure AddConst(const AName: string);
    function AddFunc(const FuncName: string): TLyPasFunc;
    procedure GenerateCodes;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    procedure Execute;
    property Module: string read FModule;
    property ResultUnitName: string read GetResultUnitName;
    property SourceCodes: TStrings read FSourceCodes;
    property ResultCodes: TStrings read FResultCodes;
    property UnitToLibrary: boolean read FUnitToLibrary write FUnitToLibrary;
  end;

procedure Setup;

function GetKeyword(const ID: string): TLyPasSymbol;
function ExtractDirective(var T: string): string;
function Fetch(var T: string; const M: string = ':'): string;

var
  my_ltypes: TLyLseTypeList;
  my_ptypes: TLyPasTypeList;
  my_pnil : TLyPasType;

implementation

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}Math;

procedure pp_pmcToLib(const Param: TLyParam);
var
  P: TLyPasTranslater;
begin
  P := TLyPasTranslater.Create;
  try
    P.SourceCodes.LoadFromFile(Param[0].AsFileName);
    P.Execute;
    Param.Result.AsString := P.ResultCodes.Text;
  finally
    P.Free;
  end;
end;

procedure pp_pmcToUnit(const Param: TLyParam);
var
  P: TLyPasTranslater;
begin
  P := TLyPasTranslater.Create;
  try
    P.FUnitToLibrary := false;
    P.SourceCodes.LoadFromFile(Param[0].AsFileName);
    P.Execute;
    Param.Result.AsString := P.ResultCodes.Text;
  finally
    P.Free;
  end;
end;

procedure Setup;
begin
  my_system.AddFunc('PmcToLib', my_string,
    ['PascalFileName'], [my_string],
    {$IFDEF FPC}@{$ENDIF}pp_pmcToLib);
  my_system.AddFunc('PmcToUnit', my_string,
    ['PascalFileName'], [my_string],
    {$IFDEF FPC}@{$ENDIF}pp_pmcToUnit);

  my_ltypes := TLyLseTypeList.Create;
  my_ptypes := TLyPasTypeList.Create;

  my_ltypes.Add(my_nil, 'my_nil', '', 'TLyNilType');
  my_ltypes.Add(my_char, 'my_char', 'char', 'TLyCharType');
  my_ltypes.Add(my_int, 'my_int', 'int64', 'TLyIntegerType');
  my_ltypes.Add(my_float, 'my_float', 'double', 'TLyFloatType');
  my_ltypes.Add(my_curr, 'my_curr', 'currency', 'TLyCurrencyType');
  my_ltypes.Add(my_time, 'my_time', 'TDateTime', 'TLyTimeType');
  my_ltypes.Add(my_bool, 'my_bool', 'boolean', 'TLyBooleanType');
  my_ltypes.Add(my_type, 'my_type', 'TLyType', 'TLyTypeType');
  my_ltypes.Add(my_string, 'my_string', 'string', 'TLyStringType');
  my_ltypes.Add(my_module, 'my_module', 'TLyModule', 'TLyModuleType');
  my_ltypes.Add(my_func, 'my_func', 'TLyFunc', 'TLyFuncType');
  my_ltypes.Add(my_hash, 'my_hash', 'TLyHash', 'TLyHashType');
  my_ltypes.Add(my_list, 'my_list', 'TLyList', 'TLyListType');
  my_ltypes.Add(my_strings, 'my_strings', 'TLyStrings', 'TLyStringsType');
  my_ltypes.Add(my_strlist, 'my_strlist', 'TLyStringList', 'TLyStringListType');

  my_pnil := my_ptypes.Add(my_ltypes.Find(my_nil), '');
  my_ptypes.Add(my_char, ['char', 'AnsiChar', 'WideChar', 'UnicodeChar']);
  my_ptypes.Add(my_int, ['int64', 'integer', 'cardinal', 'THandle', 'dword',
    'longword', 'word', 'byte', 'shortint', 'smallint', 'longint', 'qword',
    'largeint', 'large_int', 'largeuint', 'large_uint', 'cint8', 'cuint8',
    'cchar', 'cschar', 'cuchar', 'cint16', 'cuint16', 'cshort', 'csshort',
    'cushort', 'cint32', 'cuint32', 'cint', 'csint', 'cuint', 'csigned',
    'cunsigned', 'cint64', 'cuint64', 'clonglong', 'cslonglong', 'culonglong',
    'clong', 'cslong', 'culong', 'csize_t', 'u_long', 'u_short', 'cff_t']);
  my_ptypes.Add(my_float, ['double', 'extended', 'float', 'single', 'real',
    'cfloat', 'cdouble', 'clongdouble', 'longdouble', 'ValReal']);
  my_ptypes.Add(my_curr, ['currency']);
  my_ptypes.Add(my_time, ['TDateTime', 'TDate', 'TTime']);
  my_ptypes.Add(my_bool, ['boolean', 'bool', 'bytebool', 'wordbool', 'longbool', 'cbool']);
  my_ptypes.Add(my_string, ['string', 'AnsiString', 'shortstring', 'WideString',
    'UnicodeString', 'PChar', 'PAnsiChar', 'PWideChar', 'PUnicodeChar']);
  my_ptypes.Add(my_module, ['TLyModule']);
  my_ptypes.Add(my_func, ['TLyFunc']);
  my_ptypes.Add(my_type, ['TLyType']);
  my_ptypes.Add(my_list, ['TLyList']);
  my_ptypes.Add(my_hash, ['TLyHash']);
  my_ptypes.Add(my_strings, ['TLyStrings']);
  my_ptypes.Add(my_strlist, ['TLyStringList']);
end;

const
  Symbols: array[TLyPasSymbol] of string = (
    'unit', 'interface', 'uses', 'type',
    'class', 'packed', 'record', 'const', 'var', 'array', 'property',
    'function', 'procedure', 'constructor', 'destructor', 'in', 'out', 'of',
    'private', 'protected', 'public', 'published', 'inline', 'platform',
    'implementation', 'begin', 'end',  '=', '(', ')', '[', ']', ':', ';',
    ',', 'ID', 'NONE', 'EOF'
  );

function GetKeyword(const ID: string): TLyPasSymbol;
var
  X: TLyPasSymbol;
  K: string;
begin
  K := LowerCase(ID);
  for X := psUnit to psEnd do
    if K = Symbols[X] then
    begin
      Result := X;
      Exit;
    end;
  Result := psID;
end;

function ExtractDirective(var T: string): string;
var
  I: integer;
begin
  T := Trim(T);
  for I := 1 to Length(T) do
    if T[I] <= ' ' then
    begin
      Result := Copy(T, 1, I - 1);
      T := Trim(Copy(T, I + 1, Length(T)));
      Exit;
    end;
  Result := T;
  T := '';
end;

function Fetch(var T: string; const M: string = ':'): string;
begin
  Result := Trim(ExtractNext(T, M));
  T := Trim(T);
end;

{ TLyLseType }

constructor TLyLseType.Create(AType: TLyType; const AMyName, APasTypeName, ALyNameType: string);
begin
  FType := AType;
  FMyName := AMyName;
  FLyNameType := ALyNameType;
  FPasTypeName := APasTypeName;
end;

function TLyLseType.GetName: string;
begin
  Result := FType.Name;
end;

{ TLyLseTypeList }

function TLyLseTypeList.GetCount: integer;
begin
  if FItems <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function TLyLseTypeList.GetItem(Index: integer): TLyLseType;
begin
  Result := TLyLseType(FItems[Index]);
end;

constructor TLyLseTypeList.Create;
begin
  FItems := TList.Create;
end;

destructor TLyLseTypeList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TLyLseTypeList.Find(AType: TLyType): TLyLseType;
var
  I: integer;
begin
  for I := 0 to GetCount - 1 do
  begin
    Result := GetItem(I);
    if Result.FType = AType then Exit;
  end;
  if Self <> my_ltypes then
    Result := my_ltypes.Find(AType) else
    Result := nil;
end;

function TLyLseTypeList.Add(AType: TLyType; const MyName, PasTypeName, ALyNameType: string): TLyLseType;
begin
  Result := Find(AType);
  if Result = nil then
  begin
    Result := TLyLseType.Create(AType, MyName, PasTypeName, ALyNameType);
    FItems.Add(Result);
  end;
end;

procedure TLyLseTypeList.Clear;
var
  I: integer;
  T: TLyLseType;
begin
  for I := GetCount - 1 downto 0 do
  begin
    T := GetItem(I);
    FItems.Delete(I);
    T.Free;
  end;
end;

function TLyLseTypeList.Find(const AName: string): TLyLseType;
var
  I: integer;
begin
  for I := 0 to GetCount - 1 do
  begin
    Result := GetItem(I);
    if MatchID(Result.FType.Name, AName) then Exit;
  end;
  if Self <> my_ltypes then
    Result := my_ltypes.Find(AName) else
    Result := nil;
end;

{ TLyPasType }

constructor TLyPasType.Create(const TypeName: string);
begin
  FName := TypeName;
end;

function TLyPasType.GetLseTypeName: string;
begin
  Result := FLseType.Name;
end;

{ TLyPasTypeList }

function TLyPasTypeList.GetCount: integer;
begin
  if FItems <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function TLyPasTypeList.GetItem(Index: integer): TLyPasType;
begin
  Result := TLyPasType(FItems[Index]);
end;

constructor TLyPasTypeList.Create;
begin
  FItems := TList.Create;
end;

destructor TLyPasTypeList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TLyPasTypeList.Add(AType: TLyLseType; const AName: string): TLyPasType;
begin
  Result := Find(AName);
  if (Result = nil) and (AType <> nil) then
  begin
    Result := TLyPasType.Create(AName);
    Result.FLseType := AType;
    FItems.Add(Result);
  end;
end;

procedure TLyPasTypeList.Clear;
var
  I: integer;
  T: TLyPasType;
begin
  for I := GetCount - 1 downto 0 do
  begin
    T := GetItem(I);
    FItems.Delete(I);
    T.Free;
  end;
end;

procedure TLyPasTypeList.Add(AType: TLyType; const Types: array of string);
var
  I: integer;
  T: TLyLseType;
begin
  T := my_ltypes.Find(AType);
  if T <> nil then
    for I := 0 to Length(Types) - 1 do
      Add(T, Types[I]);
end;

function TLyPasTypeList.Find(const AName: string): TLyPasType;
var
  I: integer;
begin
  for I := 0 to GetCount - 1 do
  begin
    Result := GetItem(I);
    if MatchID(Result.FName, AName) then Exit;
  end;
  if Self <> my_ptypes then
    Result := my_ptypes.Find(AName) else
    Result := nil;
end;

function TLyPasTypeList.FindByName(const AName: string): TLyPasType;
begin
  Result := Find(AName);
  if Result = nil then
    Throw('unknown data type: %s', [AName]);
end;

{ TLyPasVarb }

constructor TLyPasVarb.Create(const AName: string; AType: TLyPasType);
begin
  FName := AName;
  FPasType := AType;
end;

function TLyPasVarb.GetLseType: TLyLseType;
begin
  Result := FPasType.FLseType;
end;

function TLyPasVarb.GetLseTypeName: string;
begin
  Result := LseType.Name;
end;

function TLyPasVarb.GetPasTypeName: string;
begin
  Result := FPasType.Name;
end;

{ TLyPasNodeList }

function TLyPasVarbList.GetCount: integer;
begin
  if FItems <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function TLyPasVarbList.GetItem(Index: integer): TLyPasVarb;
begin
  Result := TLyPasVarb(FItems[Index]);
end;

constructor TLyPasVarbList.Create;
begin
  FItems := TList.Create;
end;

destructor TLyPasVarbList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TLyPasVarbList.Clear;
var
  I: integer;
  V: TLyPasVarb;
begin
  for I := Count - 1 downto 0 do
  begin
    V := GetItem(I);
    FItems.Delete(I);
    V.Free;
  end;
end;

function TLyPasVarbList.Add(const AName: string; AType: TLyPasType): TLyPasVarb;
begin
  Result := TLyPasVarb.Create(AName, AType);
  FItems.Add(Result);
end;

function TLyPasVarbList.Add(const AName: string): TLyPasVarb;
begin
  Result := Add(AName, my_pnil);
end;

{ TLyPasFunc }

procedure TLyPasFunc.Clear;
begin
  FName := '';
  FPasType := my_pnil;
  FParent := nil;
  FStyle := fsNormal;
  FIsDefault := false;
  FGetSet := nil;
  FParams.Clear;
end;

function TLyPasFunc.OK: boolean;
begin
  Result := (FName <> '');
end;

function TLyPasFunc.Clone: TLyPasFunc;
var
  I: integer;
begin
  Result := TLyPasFunc.Create(FName, FPasType);
  Result.FParent := FParent;
  for I := 0 to FParams.Count - 1 do
    Result.FParams.Add(FParams[I].FName, FParams[I].FPasType);
  Result.FStyle := FStyle;
  Result.FIsDefault := FIsDefault;
end;

constructor TLyPasFunc.Create(const AName: string; AType: TLyPasType);
begin
  inherited;
  FParams := TLyPasVarbList.Create;
  FStyle := fsNormal;
end;

destructor TLyPasFunc.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

procedure TLyPasFunc.GenBodyCode(Codes: TStrings);

  procedure Add(const S: string);overload;
  begin
    if (S <> '') or ((Codes.Count > 0) and (Codes[Codes.Count - 1] <> '')) then
      Codes.Add(S);
  end;

  procedure Fmt(const S: string; const Args: array of const);
  begin
    Add(Format(S, Args));
  end;

  function get_param(X: integer): string;
  var
    P: TLyPasType;
    L: TLyLseType;
    T: TLyType;
  begin
    P := FParams[X].FPasType;
    L := P.FLseType;
    T := L.FType;
    if T = my_char then
      Result := Format('P[%d].AsChar', [X]) else
    if T = my_int then
      Result := Format('P[%d].AsInteger', [X]) else
    if T = my_float then
      Result := Format('P[%d].AsFloat', [X]) else
    if T = my_curr then
      Result := Format('P[%d].AsCurrency', [X]) else
    if T = my_time then
      Result := Format('P[%d].AsTime', [X]) else
    if T = my_bool then
      Result := Format('P[%d].AsBoolean', [X]) else
    if T = my_type then
      Result := Format('P[%d].AsType', [X]) else
    if T = my_string then
      Result := Format('P[%d].AsString', [X]) else
    if T = my_array then
      Result := Format('P[%d].AsArray', [X]) else
    if T = my_module then
      Result := Format('P[%d].AsModule', [X]) else
    if T = my_func then
      Result := Format('P[%d].AsFunc', [X]) else
    if T = my_hash then
      Result := Format('P[%d].AsHash', [X]) else
      Result := Format('P[%d].GetData(%s)', [X, L.FMyName]);
    if not MatchID(L.FPasTypeName, P.FName)  then
      Result := P.FName + '(' + Result + ')';
  end;

  function get_call: string;
  var
    X, N: integer;
    P: TLyPasType;
    L: TLyLseType;
    T: TLyType;
    V: string;
  begin
    N := FParams.Count;
    if FStyle in [fsGet, fsSet] then
    begin
      V := 'X.' + Name;
      if FStyle = fsSet then Dec(N);
      if N > 1 then
      begin
        V := V + '[' + get_param(1);
        for X := 2 to N - 1 do
          V := V + ', ' + get_param(X);
        V := V + ']';
      end;
      if FStyle = fsSet then
        V := V + ' := ' + get_param(N);
    end
    else
    if FStyle = fsMethod then
    begin
      V := 'X.' + Name;
      if N > 1 then
      begin
        V := V + '(' + get_param(1);
        for X := 2 to N - 1 do
          V := V + ', ' + get_param(X);
        V := V + ')';
      end;
    end
    else
    begin
      if FStyle = fsCreate then
        V := FParent.Name + '.' + Name else
        V := Name;
      if N > 0 then
      begin
        V := V + '(' + get_param(0);
        for X := 1 to N - 1 do
          V := V + ', ' + get_param(X);
        V := V + ')';
      end;
    end;

    if HasResult then
    begin
      P := FPasType;
      L := P.FLseType;
      T := L.FType;
      if not MatchID(L.FPasTypeName, P.FName)  then
        V := L.FPasTypeName + '(' + V + ')';
      if T = my_char then
        V := Format('P.Result.AsChar := %s', [V]) else
      if T = my_int then
        V := Format('P.Result.AsInteger := %s', [V]) else
      if T = my_float then
        V := Format('P.Result.AsFloat := %s', [V]) else
      if T = my_curr then
        V := Format('P.Result.AsCurrency := %s', [V]) else
      if T = my_time then
        V := Format('P.Result.AsTime := %s', [V]) else
      if T = my_bool then
        V := Format('P.Result.AsBoolean := %s', [V]) else
      if T = my_type then
        V := Format('P.Result.AsType := %s', [V]) else
      if T = my_string then
        V := Format('P.Result.AsString := %s', [V]) else
      if T = my_array then
        V := Format('P.Result.AsArray := %s', [V]) else
      if T = my_module then
        V := Format('P.Result.AsModule := %s', [V]) else
      if T = my_func then
        V := Format('P.Result.AsFunc := %s', [V]) else
      if T = my_hash then
        V := Format('P.Result.AsHash := %s', [V]) else
        V := Format('P.Result.Assign(%s, %s)', [L.FMyName, V]);
    end;

    Result := V + ';';
  end;

begin
  if FStyle in [fsMethod, fsGet, fsSet] then
  begin
    Add('var');
    Fmt('  X: %s;', [FParams[0].PasType.Name]);
  end;

  Add('begin');
  if FStyle in [fsMethod, fsGet, fsSet] then
  begin
    Add('  if P.GetSelf(X) then');
    Add('    ' + get_call);
  end
  else Add('  ' + get_call);
  Add('end;');
end;

procedure TLyPasFunc.GenSetupCode(Codes: TStrings; const MyModule: string);

  function param_names(Start: integer): string;
  var
    I: integer;
    P: TLyPasVarb;
  begin
    Result := '';
    for I := Start to FParams.Count - 1 do
    begin
      P := FParams[I];
      if Result <> '' then
        Result := Result + ', ''' + P.FName + '''' else
        Result := '''' + P.FName + '''';
    end;
  end;

  function param_types(Start: integer): string;
  var
    I: integer;
    P: TLyPasVarb;
  begin
    Result := '';
    for I := Start to FParams.Count - 1 do
    begin
      P := FParams[I];
      if Result <> '' then
        Result := Result + ', ' + P.FPasType.FLseType.FMyName else
        Result := P.FPasType.FLseType.FMyName;
    end;
  end;

  function setup_normal: string;
  var
    P: string;
  begin
    if FParams.Count > 0 then
      P := Format('[%s], [%s], ', [param_names(0), param_types(0)]) else
      P := '';
    Result := Format('  my_system.AddFunc(''%s'', %s, %s{$IFDEF FPC}@{$ENDIF}%s);',
      [FName, LseType.FMyName, P, LyseeProcName]);
  end;

  function setup_method: string;
  var
    P: string;
  begin
    if FParams.Count > 1 then
      P := Format('[%s], [%s], ', [param_names(1), param_types(1)]) else
      P := '';
    Result := Format('  Method(''%s'', %s, %s{$IFDEF FPC}@{$ENDIF}%s);',
      [FName, LseType.FMyName, P, LyseeProcName]);
  end;

  function setup_property: string;
  var
    H: string;
  var
    P: string;
  begin
    if FIsDefault then
      H := '  Define('''', ' + FPasType.LseType.FMyName + ', ' else
      H := '  Define(''' + Name + ''', ' + FPasType.LseType.FMyName + ', ';
    if FParams.Count > 1 then
      P := Format('[%s], [%s], ', [param_names(1), param_types(1)]) else
      P := '';
    Result := H + P + '{$IFDEF FPC}@{$ENDIF}' + LyseeProcName;
    if FGetSet = nil then
      Result := Result + ');' else
      Result := Result + ',' + '{$IFDEF FPC}@{$ENDIF}' + FGetSet.LyseeProcName + ');';
  end;

  function setup_create: string;
  var
    P: string;
  begin
    if FParams.Count > 0 then
      P := Format('[%s], [%s], ', [param_names(0), param_types(0)]) else
      P := '';
    Result := Format('  Method(''%s'', %s, %s{$IFDEF FPC}@{$ENDIF}%s);',
      [LSE_CREATE, FParent.FLseType.FMyName, P, LyseeProcName]);
  end;

var
  S: string;
begin
  S := '';
  case FStyle of
    fsNormal: S := setup_normal;
    fsMethod: S := setup_method;
    fsGet   : S := setup_property;
    fsCreate: S := setup_create;
  end;
  if S <> '' then Codes.Add(S);
end;

function TLyPasFunc.HasResult: boolean;
begin
  Result := (FStyle <> fsSet) and (FPasType <> my_pnil);
end;

function TLyPasFunc.IsMethod: boolean;
begin
  Result := (FStyle <> fsNormal) and (FParent <> nil);
end;

function TLyPasFunc.LyseeProcName: string;
begin
  if FStyle = fsCreate then
    Result := 'MyCreate' else
  if FStyle = fsMethod then
      Result := 'My' + FName else
  if FStyle = fsGet then
  begin
    if FIsDefault then
      Result := 'MyGet' else
      Result := 'MyGet' + FName;
  end
  else
  if FStyle = fsSet then
  begin
    if FIsDefault then
      Result := 'MySet' else
      Result := 'MySet' + FName;
  end
  else Result := 'pp_' + FName;
end;

function TLyPasFunc.LyseeProcPrototype(UToL: boolean): string;
begin
  if UToL then
    Result := 'procedure ' + LyseeProcName + '(const P: PLyParam);' else
    Result := 'procedure ' + LyseeProcName + '(const P: TLyParam);';
end;

{ TLyPasFuncList }

function TLyPasFuncList.Add(const AName: string;
  AType: TLyPasType): TLyPasFunc;
begin
  Result := TLyPasFunc.Create(AName, AType);
  FItems.Add(Result);
end;

function TLyPasFuncList.Add(const AName: string): TLyPasFunc;
begin
  Result := Add(AName, my_pnil);
end;

procedure TLyPasFuncList.Clear;
var
  I: integer;
  F: TLyPasFunc;
begin
  for I := GetCount - 1 downto 0 do
  begin
    F := GetItem(I);
    FItems.Delete(I);
    F.Free;
  end;
end;

constructor TLyPasFuncList.Create;
begin
  FItems := TList.Create;
end;

destructor TLyPasFuncList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TLyPasFuncList.GetCount: integer;
begin
  if FItems <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function TLyPasFuncList.GetItem(Index: integer): TLyPasFunc;
begin
  Result := TLyPasFunc(FItems[Index]);
end;

function TLyPasFuncList.ListMethods(AType: TLyLseType): TList;
var
  I: integer;
  F: TLyPasFunc;
begin
  Result := TList.Create;
  for I := 0 to GetCount - 1 do
  begin
    F := GetItem(I);
    if F.IsMethod then
      if F.FParent.FLseType = AType then
        Result.Add(F);
  end;
end;

procedure TLyPasFuncList.Remove(AFunc: TLyPasFunc);
begin
  if AFunc <> nil then
  begin
    FItems.Remove(AFunc);
    AFunc.Free;
  end;
end;

{ TLyPasTokenizer }

constructor TLyPasTokenizer.Create;
begin
  SetCode('');
end;

function TLyPasTokenizer.GetChar: boolean;
var
  F13: boolean;
begin
  F13 := (FChar = #13);
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

function TLyPasTokenizer.GetNext(Sym1, Sym2: TLyPasSymbol): boolean;
begin
  Result := GetNext(Sym1) and GetNext(Sym2);
end;

function TLyPasTokenizer.GetNext(ASym: TLyPasSymbol): boolean;
begin
  if GetNext() then
    Result := FSym = ASym else
    Result := false;
end;

function TLyPasTokenizer.GetNext(Syms: TLyPasSymbols): boolean;
begin
  if GetNext() then
    Result := FSym in Syms else
    Result := false;
end;

function TLyPasTokenizer.GetNext: boolean;
begin
  Result := GetToken(Self);
end;

function TLyPasTokenizer.GetToken(T: TLyPasToken): boolean;
begin
  Result := (FChar <> #0) and SkipSpaces;
  if Result then
  begin
    T.FSym := psNone;
    T.FName := '';
    if CharInSet(FChar, CS_HEAD) then
    begin
      T.FName := FChar;
      while GetChar and CharInSet(FChar, CS_ID) do
        T.FName := T.FName + FChar;
      T.FSym := GetKeyword(T.FName);
      if T.FSym = psImplementation then
      begin
        FPosition := FSize + 1;
        FChar := #0;
        T.FSym := psEOF;
      end;
    end
    else
    if FChar = '''' then
    begin
      GetChar;
      GotoChar(['''']);
      GetChar;
    end
    else
    if CharInSet(FChar, ['0'..'9', '$', '#']) then
    begin
      GetChar;
      while CharInSet(FChar, CS_ID + ['$', '#', '.']) do GetChar;
    end
    else
    begin
      case FChar of
        '(': T.FSym := psLParen;
        ')': T.FSym := psRParen;
        '[': T.FSym := psLArray;
        ']': T.FSym := psRArray;
        ':': T.FSym := psColon;
        ';': T.FSym := psSemic;
        ',': T.FSym := psComma;
        '=': T.FSym := psEqual;
      end;
      GetChar;
    end;
  end
  else T.FSym := psEOF;
end;

function TLyPasTokenizer.GotoChar(Chars: TSysCharSet): boolean;
begin
  repeat Result := CharInSet(FChar, Chars)
  until Result or not GetChar;
end;

procedure TLyPasTokenizer.SetCode(const Value: string);
begin
  FSym := psNone;
  FName := '';
  FCode := Value;
  FSize := Length(FCode);
  FPosition := 1;
  if FPosition <= FSize then
    FChar := FCode[FPosition] else
    FChar := #0;
end;

function TLyPasTokenizer.GotoToken(Syms: TLyPasSymbols): boolean;
begin
  Result := (FSym in Syms);
  while not Result and GetNext() do
    Result := (FSym in Syms);
end;

function TLyPasTokenizer.GotoID(const ID: string): boolean;
begin
  Result := false;
  while GotoToken([psID]) do
    if MatchID(ID, FName) then
    begin
      Result := true;
      Exit;
    end
    else GetNext();
end;

function TLyPasTokenizer.PeekNext: TLyPasSymbol;
var
  T: TLyPasToken;
  P: integer;
  H: char;
begin
  T := TLyPasToken.Create;
  P := FPosition;
  H := FChar;
  try
    GetToken(T);
    Result := T.FSym;
  finally
    FPosition := P;
    FChar := H;
    T.Free;
  end;
end;

function TLyPasTokenizer.PeekChar: char;
begin
  if FPosition < FSize then
    Result := FCode[FPosition + 1] else
    Result := #0;
end;

function TLyPasTokenizer.SkipSpaces: boolean;
begin
  Result := false;
  while not Result and (FChar <> #0) do
    if FChar <= ' ' then GetChar else
    if FChar = '{' then
    begin
      GotoChar(['}']);
      GetChar;
    end
    else
    if (FChar = '(') and (PeekChar = '*') then
    begin
      GetChar;
      GetChar;
      while GotoChar(['*']) do
      begin
        GetChar;
        if FChar = ')' then Break;
      end;
      GetChar;
    end
    else
    if (FChar = '/') and (PeekChar = '/') then
    begin
      GotoChar([#13, #10]);
      GetChar;
    end
    else Result := true;
end;

{ TLyPasParser }

function TLyPasTranslater.AddFunc(const FuncName: string): TLyPasFunc;
begin
  Result := FFuncs.Add(FuncName);
  Result.FStyle := fsNormal;
end;

procedure TLyPasTranslater.Clear;
begin
  SetCode('');
  FModule := '';
  FUses := '';
  FMyModule := '';
  FResultCodes.Clear;
  FConstants.Clear;
  FFuncs.Clear;
  FNewLseTypes.Clear;
  FNewPasTypes.Clear;
end;

constructor TLyPasTranslater.Create;
begin
  FConstants := TStringList.Create;
  FConstants.CaseSensitive := false;
  FM := TLyModule.Create('$');
  FNewLseTypes := TLyLseTypeList.Create;
  FNewPasTypes := TLyPasTypeList.Create;
  FFuncs := TLyPasFuncList.Create;
  FTemp := TLyPasFunc.Create('', my_pnil);
  FSourceCodes := TStringList.Create;
  FResultCodes := TStringList.Create;
  FUnitToLibrary := true;
end;

destructor TLyPasTranslater.Destroy;
begin
  Clear;
  FreeAll([FNewLseTypes, FConstants, FNewPasTypes, FFuncs, FTemp,
           FSourceCodes, FResultCodes, FM]);
  inherited;
end;

function TLyPasTranslater.ParseToSemic: boolean;
var
  N: integer;
begin
  Result := (FSym = psSemic);
  if not Result then
  begin
    N := 0;
    while (FSym <> psSemic) or (N <> 0) do
    begin
      if FSym in [psLParen, psLArray, psRecord] then Inc(N) else
      if FSym in [psRParen, psRArray, psEnd] then Dec(N);
      if not GetNext() then Exit;
    end;
    Result := (FSym = psSemic);
  end;
end;

function TLyPasTranslater.ParseConstSection: boolean;
begin
  Result := GetNext(psID);
  while Result and (FSym = psID) do
  begin
    AddConst(FName);
    Result := ParseToSemic and GetNext();
  end;
end;

function TLyPasTranslater.ParseTypeSection: boolean;
var
  K: string;
  P: TLyPasType;
begin
  Result := false;
  if not GetNext(psID) then Exit;

  while FSym = psID do
  begin
    K := FName;
    if not GetNext(psEqual) or not GetNext() then Exit;
    case FSym of
      psClass    : if not ParseClass(K) then Exit;
      psInterface: if not ParseInterface(K) then Exit;
      psID       : begin
                     P := FNewPasTypes.Find(FName);
                     if P <> nil then
                       FNewPasTypes.Add(P.FLseType, K);
                     if not ParseToSemic then Exit;
                   end;
      else if not ParseToSemic then Exit;
    end;
    if not GetNext() then Exit;
  end;

  Result := true;
end;

function TLyPasTranslater.ParseVarSection: boolean;
begin
  Result := GetNext(psID);
  while Result and (FSym = psID) do
    Result := ParseToSemic and GetNext();
end;

function TLyPasTranslater.ParseClass(const AName: string): boolean;
const
  H = [psPublic, psPublished, psProtected, psPrivate, psID, psClass,
       psConstructor, psDestructor, psFunc, psProc, psProp, psEnd];
var
  X, N: string;
  T: TLyType;
  L: TLyLseType;
  P: TLyPasType;
begin
  Result := false;

  if not GetNext(H + [psLParen, psSemic]) then Exit;
  if FSym = psLParen then
    if not GotoToken([psRParen]) or not GetNext(H + [psSemic]) then
      Exit;
  if FSym = psSemic then Exit(true);

  if (Length(AName) > 3) and (Copy(AName, 1, 3) = 'TLy') then
    N := Copy(AName, 4, Length(AName)) else
    N := Copy(AName, 2, Length(AName));
  X := 'T' + N;
  T := TLyType.Create(X, FM);
  L := FNewLseTypes.Add(T, 'my_' + N, AName, 'TLy' + N + 'Type');
  P := FNewPasTypes.Add(L, AName);

  while FSym <> psEnd do
  begin
    case FSym of
      psConstructor: if not ParseFunc(FSym, P) then Exit;
      psDestructor : if not ParseFunc(FSym, P) then Exit;
      psFunc       : if not ParseFunc(FSym, P) then Exit;
      psProc       : if not ParseFunc(FSym, P) then Exit;
      psProp       : if not ParseProp(P) then Exit;
      psID         : if not ParseField(P) then Exit;
      psPublic     : if not GetNext() then Exit;
      psPublished  : if not GetNext() then Exit;
      psPrivate    : if not GotoToken([psPublic, psPublished, psEnd]) then Exit;
      psProtected  : if not GotoToken([psPublic, psPublished, psEnd]) then Exit;
      psClass      : if not GetNext([psFunc, psProc]) then Exit;
    end;
    if not GotoToken(H) then Exit;
  end;
  Result := GetNext(psSemic);
end;

procedure TLyPasTranslater.AddConst(const AName: string);
begin
  FConstants.Values[AName] := AName;
end;

function TLyPasTranslater.GetResultUnitName: string;
begin
  Result := 'lysee_' + FModule;
end;

function TLyPasTranslater.ParseField(AClass: TLyPasType): boolean;
var
  I: integer;
  F, S: TLyPasFunc;
  T: TLyPasType;
begin
  Result := false;

  // 1. get first field
  FTemp.Clear;
  FTemp.FParams.Add(FName);

  // 2. get following fields
  if not GetNext([psComma, psColon]) then Exit;
  while FSym = psComma do
  begin
    if not GetNext(psID) then Exit;
    FTemp.FParams.Add(FName);
    if not GetNext([psComma, psColon]) then Exit;
  end;

  // 3. get field type
  if GetNext(psID) then
  begin
    T := FNewPasTypes.Find(FName);
    if T <> nil then
      for I := 0 to FTemp.FParams.Count - 1 do
      begin
        F := AddFunc(UpperHead(FTemp.FParams[I].FName));
        F.FParent := AClass;
        F.FStyle := fsGet;
        F.FPasType := T;
        F.FParams.Add('this', AClass);
        S := AddFunc(F.Name);
        S.FParent := AClass;
        S.FStyle := fsSet;
        F.FGetSet := S;
        S.FGetSet := F;
        F.FParams.Add('this', AClass);
        S.FParams.Add('Value', T);
      end;
  end;

  Result := ParseToSemic;
end;

function TLyPasTranslater.ParseFunc(FuncSym: TLyPasSymbol; AClass: TLyPasType): boolean;

  function peek_decorate: boolean;
  var
    T: TLyPasToken;
    P: integer;
    H: char;
  begin
    Result := false;
    P := FPosition;
    H := FChar;
    T := TLyPasToken.Create;
    try
      Result := GetToken(T) and (T.FSym <> psEnd);
      if Result then
        Result := GetToken(T) and (T.FSym = psSemic);
    finally
      if Result then
      begin
        FSym := T.FSym;
        FName := T.FName;
      end
      else
      begin
        FPosition := P;
        FChar := H;
      end;
      T.Free;
    end;
  end;

  function skip_func(InParam: boolean): boolean;
  begin
    if InParam and GotoToken([psRParen]) then GetNext();
    Result := ParseToSemic;
    if Result then
      while peek_decorate do;
  end;

var
  T: TLyPasType;
  I: integer;
  M: TLyPasSymbol;
begin
  Result := false;

  // 1. get function name and setup function
  if not GetNext(psID) then Exit;
  FTemp.Clear;
  FTemp.FName := FName;
  FTemp.FParent := AClass;
  if FuncSym = psConstructor then
  begin
    FTemp.FPasType := AClass;
    FTemp.FStyle := fsCreate;
  end
  else
  if AClass <> nil then
  begin
    FTemp.FParams.Add('this', AClass);
    FTemp.FStyle := fsMethod;
  end;

  // 2. parse arguments
  if FuncSym = psFunc then M := psColon else M := psSemic;
  if not GetNext([psLParen, M]) then Exit;
  if FSym = psLParen then
  begin
    if not GetNext([psConst, psVar, psOut, psID, psRParen]) then Exit;
    if FSym in [psVar, psOut] then
      Exit(skip_func(true));

    while FSym <> psRParen do
    begin
      I := FTemp.FParams.Count;
      if (FSym = psConst) and not GetNext(psID) then Exit;
      FTemp.FParams.Add(FName);

      if not GetNext([psComma, psColon, psSemic, psRParen]) then Exit;
      while FSym = psComma do
      begin
        if not GetNext(psID) then Exit;
        FTemp.FParams.Add(FName);
        if not GetNext([psComma, psColon, psSemic, psRParen]) then Exit;
      end;

      if (FSym in [psSemic, psRParen]) or not GetNext(psID) then
        Exit(skip_func(true));

      T := FNewPasTypes.Find(FName);
      if T = nil then
        Exit(skip_func(true));

      while I < FTemp.FParams.Count do
      begin
        FTemp.FParams[I].FPasType := T;
        Inc(I);
      end;

      if not GotoToken([psSemic, psRParen]) then Exit;
      if FSym = psSemic then
      begin
        if not GetNext([psConst, psVar, psOut, psID]) then Exit;
        if FSym in [psVar, psOut] then
          Exit(skip_func(true));
      end;
    end;

    if not GetNext(M) then Exit;
  end;

  // 3. parse function type
  if FSym = psColon then
  begin
    if not GetNext(psID) then
      Exit(skip_func(false));
    FTemp.FPasType := FNewPasTypes.Find(FName);
    if FTemp.FPasType = nil then
      Exit(skip_func(false));
    if not GetNext(psSemic) then Exit;
  end;

  if FuncSym <> psDestructor then
    FFuncs.FItems.Add(FTemp.Clone);
  FTemp.Clear;
  Result := true;

  while peek_decorate do;
end;

function TLyPasTranslater.ParseInterface(const AName: string): boolean;
begin
  Result := GetNext();
  if not Result then Exit;

  if FSym = psLParen then
  begin
    Result := GotoToken([psRParen]) and GetNext();
    if not Result then Exit;
  end;

  if FSym <> psSemic then
    Result := GotoToken([psEnd]) and GetNext(psSemic);
end;

function TLyPasTranslater.ParseProp(AClass: TLyPasType): boolean;

  function peek_default: boolean;
  var
    T: TLyPasToken;
    P: integer;
    H: char;
  begin
    Result := false;
    P := FPosition;
    H := FChar;
    T := TLyPasToken.Create;
    try
      Result := GetToken(T) and MatchID(T.FName, 'default');
      if Result then
        Result := GetToken(T) and (T.FSym = psSemic);
    finally
      if Result then
      begin
        FSym := T.FSym;
        FName := T.FName;
      end
      else
      begin
        FPosition := P;
        FChar := H;
      end;
      T.Free;
    end;
  end;

  function skip_prop(InParam: boolean): boolean;
  begin
    if InParam and GotoToken([psRArray]) then GetNext();
    Result := ParseToSemic;
    if Result then
      peek_default;
  end;

var
  F, S: TLyPasFunc;
  I: integer;
  T: TLyPasType;
begin
  Result := false;

  // 1. get property name and setup function
  if not GetNext(psID) then Exit;
  FTemp.Clear;
  FTemp.FName := FName;
  FTemp.FParent := AClass;
  FTemp.FParams.Add('this', AClass);
  FTemp.FStyle := fsGet;

  // 2. parse arguments
  if not GetNext([psLArray, psColon, psSemic]) then Exit;
  if FSym = psSemic then Exit(true);
  if FSym = psLArray then
  begin
    if not GetNext([psConst, psID]) then Exit;
    while FSym <> psRArray do
    begin
      I := FTemp.FParams.Count;
      if (FSym = psConst) and not GetNext(psID) then Exit;
      FTemp.FParams.Add(FName);

      if not GetNext([psComma, psColon]) then Exit;
      while FSym = psComma do
      begin
        if not GetNext(psID) then Exit;
        FTemp.FParams.Add(FName);
        if not GetNext([psComma, psColon]) then Exit;
      end;

      if not GetNext(psID) then
        Exit(skip_prop(true));
      T := FNewPasTypes.Find(FName);
      if T = nil then
        Exit(skip_prop(true));

      while I < FTemp.FParams.Count do
      begin
        FTemp.FParams[I].FPasType := T;
        Inc(I);
      end;

      if not GetNext([psSemic, psRArray]) then Exit;
      if FSym = psSemic then
        if not GetNext([psConst, psID]) then Exit;
    end;

    if not GetNext(psColon) then Exit;
  end;

  // 3. get property type
  if not GetNext(psID) then
    Exit(skip_prop(false));
  FTemp.FPasType := FNewPasTypes.Find(FName);
  if FTemp.FPasType = nil then
    Exit(skip_prop(false));

  // 4. check writing function
  while GetNext() and (FSym <> psSemic) do
    if FTemp.FGetSet = nil then
      if (FSym = psID) and MatchID(FName, 'write') then
        FTemp.FGetSet := FTemp;

  // 5. check default
  FTemp.FIsDefault := (FTemp.FParams.Count > 1) and peek_default;

  // 6. setup property methods
  F := FTemp.Clone;
  FFuncs.FItems.Add(F);
  if FTemp.FGetSet <> nil then
  begin
    S := F.Clone;
    FFuncs.FItems.Add(S);
    S.FStyle := fsSet;
    F.FGetSet := S;
    S.FGetSet := F;
    S.FPasType := my_pnil;
    S.FParams.Add('Value', F.PasType);
  end;
  FTemp.Clear;
  Result := true;
end;

procedure TLyPasTranslater.Execute;
var
  I: integer;
begin
  Clear;
  SetCode(FSourceCodes.Text);

  // 1.get unit name
  if not GetNext(psUnit, psID) then Exit;
  FModule := FName;
  FMyModule := 'my_' + FModule;
  if not GetNext(psSemic, psInterface) then Exit;

  // 2.get used unit list
  if GetNext(psUses) then
  begin
    I := FPosition;
    if not GotoToken([psSemic]) then Exit;
    FUses := Trim(Copy(FCode, I, FPosition - I - 1));
  end;

  // 3.parse constants, classes, functions and procedures
  while GotoToken([psType, psConst, psVar, psFunc, psProc]) do
    case FSym of
      psConst: if not ParseConstSection then Exit;
      psType : if not ParseTypeSection then Exit;
      psFunc : if not ParseFunc(FSym, nil) then Exit;
      psProc : if not ParseFunc(FSym, nil) then Exit;
      psVar  : if not ParseVarSection then Exit;
    end;

  GenerateCodes;
end;

procedure TLyPasTranslater.GenerateCodes;

  procedure Add(const S: string);overload;
  var
    T: string;
  begin
    T := TrimRight(S);
    if (T <> '') or ((FResultCodes.Count > 0) and (FResultCodes[FResultCodes.Count - 1] <> '')) then
      FResultCodes.Add(T);
  end;

  procedure Add(const S: string; const Args: array of const);overload;
  begin
    Add(Format(S, Args));
  end;

  procedure Add_uses(const Units: array of string);
  var
    I: integer;
    S: string;
  begin
    S := FUses;
    for I := 0 to Length(Units) - 1 do
      if S <> '' then
      begin
        SetCode(S);
        if not GotoID(Units[I]) then
          S := S + ', ' + Units[I];
      end
      else S := Units[I];
    Add('uses');
    Add('  ' + S + ';');
  end;

  procedure Add_type_defs;
  var
    I, X: integer;
    T: TLyLseType;
    N: string;
    L: TList;
    F: TLyPasFunc;
  begin
    if FNewLseTypes.Count > 0 then
    begin
      Add('type');
      for I := 0 to FNewLseTypes.Count - 1 do
      begin
        T := FNewLseTypes[I];
        N := T.TLyNameType;
        L := FFuncs.ListMethods(T);
        try
          Add('');
          Add('  { %s }', [N]);
          Add('');
          Add('  %s = class(TLyType)', [N]);
          if L.Count > 0 then
          begin
            Add('  private');
            for X := 0 to L.Count - 1 do
            begin
              F := TLyPasFunc(L[X]);
              Add('    procedure %s(const Param: TLyParam);', [F.LyseeProcName]);
            end;
          end;
          Add('  protected');
          Add('    function IncRefcount(Obj: pointer): integer;override;');
          Add('    function DecRefcount(Obj: pointer): integer;override;');
          Add('    procedure Setup;override;');
          Add('    function AsString(Obj: pointer): string;override;');
          Add('  end;');
        finally
          L.Free;
        end;
      end;
    end;
  end;

  procedure Add_var;
  var
    I: integer;
    L: TLyLseType;
  begin
    Add('var');
    for I := 0 to FNewLseTypes.Count - 1 do
    begin
      L := FNewLseTypes[I];
      Add('  %s: %s;', [L.FMyName, L.FLyNameType]);
    end;
  end;

  procedure Add_type_methods;
  var
    I, X: integer;
    T: TLyLseType;
    N: string;
    L: TList;
    F: TLyPasFunc;
  begin
    for I := 0 to FNewLseTypes.Count - 1 do
    begin
      T := FNewLseTypes[I];
      N := T.TLyNameType;
      L := FFuncs.ListMethods(T);
      try
        Add('');
        Add('{ %s }', [N]);
        Add('');
        Add('function %s.AsString(Obj: pointer): string;', [N]);
        Add('begin');
        Add('  if Obj <> nil then');
        Add('    Result := %s(Obj).ToString else', [FNewLseTypes[I].FPasTypeName]);
        Add('    Result := '''';');
        Add('end;');
        Add('');
        Add('function %s.IncRefcount(Obj: pointer): integer;', [N]);
        Add('begin');
        Add('  if Obj <> nil then');
        Add('    Result := %s(Obj).IncRefcount else', [FNewLseTypes[I].FPasTypeName]);
        Add('    Result := 0;');
        Add('end;');
        Add('');
        Add('function %s.DecRefcount(Obj: pointer): integer;', [N]);
        Add('begin');
        Add('  if Obj <> nil then');
        Add('    Result := %s(Obj).DecRefcount else', [FNewLseTypes[I].FPasTypeName]);
        Add('    Result := 0;');
        Add('end;');
        Add('');
        Add('function %s.Setup;', [N]);
        Add('begin');
        for X := 0 to L.Count - 1 do
        begin
          F := TLyPasFunc(L[X]);
          if F.FStyle <> fsSet then
            F.GenSetupCode(FResultCodes, FMyModule);
        end;
        Add('end;');
        for X := 0 to L.Count - 1 do
        begin
          Add('');
          F := TLyPasFunc(L[X]);
          Add('procedure %s.%s(const Param: TLyParam);', [N, F.LyseeProcName]);
          F.GenBodyCode(FResultCodes);
        end;
      finally
        L.Free;
      end;
    end;
  end;

  procedure Add_lysee_procs_prototype;
  var
    I: integer;
    F: TLyPasFunc;
  begin
    for I := 0 to FFuncs.Count - 1 do
    begin
      F := FFuncs[I];
      if not F.IsMethod then
        Add(F.LyseeProcPrototype(FUnitToLibrary));
    end;
  end;

  procedure Add_lysee_procs;
  var
    I: integer;
    F: TLyPasFunc;
  begin
    for I := 0 to FFuncs.Count - 1 do
    begin
      F := FFuncs[I];
      if not F.IsMethod then
      begin
        Add('');
        Add(F.LyseeProcPrototype(FUnitToLibrary));
        try
          F.GenBodyCode(FResultCodes);
        except
          Writeln(F.FName);
        end;
      end;
    end;
  end;

  procedure Add_setup_types;
  var
    I: integer;
    L: TLyLseType;
  begin
    for I := 0 to FNewLseTypes.Count - 1 do
    begin
      L := FNewLseTypes[I];
      Add('  %s := %s.Create(''%s'', my_system);',
        [L.FMyName, L.FLyNameType, L.Name]);
    end;
    Add('');
    for I := 0 to FNewLseTypes.Count - 1 do
      Add('  %s.Setup;', [FNewLseTypes[I].FMyName]);
  end;

  procedure Add_setup_constants;
  var
    I: integer;
    N, V: string;
  begin
    for I := 0 to FConstants.Count - 1 do
    begin
      N := ExtractNameValue(FConstants[I], V);
      Add('  %s.Contants.DefConst(''%s'', %s);', [FMyModule, N, V]);
    end;
  end;

  procedure Add_setup_lysee_procs;
  var
    I: integer;
  begin
    for I := 0 to FFuncs.Count - 1 do
      if not FFuncs[I].IsMethod then
        FFuncs[I].GenSetupCode(FResultCodes, FMyModule);
  end;

begin
  FResultCodes.Clear;

  if FUnitToLibrary then
    Add('library %s;', [ResultUnitName]) else
    Add('unit %s;', [ResultUnitName]);

  Add('');
  Add('{$IFDEF FPC}');
  Add('{$MODE objfpc}{$H+}');
  Add('{$ENDIF}');
  Add('');

  if FUnitToLibrary then
  begin
    Add_uses(['Classes', 'SysUtils', 'ilysee', FModule]);
  end
  else
  begin
    Add('interface');
    Add('');
    Add_uses(['Classes', 'SysUtils', 'basic', 'lysee', FModule]);
    Add('');
    Add_type_defs;
    Add('');
    Add_var;
    Add('');
    Add_lysee_procs_prototype;
    Add('');
    Add('procedure Setup;');
    Add('');
    Add('implementation');
  end;

  Add('');
  Add_lysee_procs;
  Add('');

  if FUnitToLibrary then
  begin
    Add('procedure Setup(const P: PLyParam);');
    Add('begin');
    Add_setup_lysee_procs;
    Add('end;');
    Add('');
    Add('begin');
    Add('  ilysee.my_setup := {$IFDEF FPC}@{$ENDIF}Setup;');
    Add('end.');
  end
  else
  begin
    Add('procedure Setup;');
    Add('begin');
    Add_setup_types;
    Add('');
    Add_setup_constants;
    Add('');
    Add_setup_lysee_procs;
    Add('end;');
    Add('');
    Add_type_methods;
    Add('');
    Add('end.');
  end;
end;

initialization
begin
  { do nothing }
end;

finalization
begin
  FreeAndNil(my_ptypes);
  FreeAndNil(my_ltypes);
end;

end.
