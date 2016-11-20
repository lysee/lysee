{==============================================================================}
{        UNIT: lysee_pmc                                                       }
{ DESCRIPTION: lysee module compiler                                           }
{   COPYRIGHT: Copyright (c) 2012-2016, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2012/02/17                                                      }
{    MODIFIED: 2016/11/20                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_pmc;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, basic, lysee;

type

  TLiPasTranslater = class; {forward}

  TLiCompileEnv = (cePascal, ceFPC, ceDelphi);

  { TLiLseType }

  TLiLseType = class
  private
    FType: TLiType;
    FMyName: string; // my_string, my_char, ...
    FPasTypeName: string;
    FEnumItems: string;
    FCE: TLiCompileEnv;
    function GetName: string;
  public
    constructor Create(AType: TLiType; const AMyName, APasTypeName: string);
    destructor Destroy;override;
    function IsClass: boolean;
    function IsEnum: boolean;
    function IsEnumSet: boolean;
    function EnumItemList: TStrings;
    property VType: TLiType read FType;
    property Name: string read GetName;
    property MyName: string read FMyName;
    property PasTypeName: string read FPasTypeName;
  end;

  { TLiLseTypeList }

  TLiLseTypeList = class
  private
    FItems: TList;
    function GetItem(Index: integer): TLiLseType;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    function Add(AType: TLiType; const MyName, PasTypeName: string): TLiLseType;
    function Find(AType: TLiType): TLiLseType;overload;
    function Find(const AName: string): TLiLseType;overload;
    function ClassCount: integer;
    function FirstEnumSetIndex: integer;
    property Count: integer read GetCount;
    property Items[Index: integer]: TLiLseType read GetItem;default;
  end;

  { TLiPasType }

  TLiPasType = class
  private
    FName: string;
    FLseType: TLiLseType;
    FCE: TLiCompileEnv;
    function GetLseTypeName: string;
  public
    constructor Create(const TypeName: string);
    property Name: string read FName;
    property LseType: TLiLseType read FLseType;
    property LseTypeName: string read GetLseTypeName;
  end;

  { TLiPasTypeList }

  TLiPasTypeList = class
  private
    FItems: TList;
    function GetItem(Index: integer): TLiPasType;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    function Add(AType: TLiLseType; const AName: string): TLiPasType;overload;
    procedure Add(AType: TLiType; const Types: array of string);overload;
    function Find(const AName: string): TLiPasType;
    function FindByName(const AName: string): TLiPasType;
    property Count: integer read GetCount;
    property Items[Index: integer]: TLiPasType read GetItem;default;
  end;

  { TLiPasVarb }

  TLiPasVarb = class
  private
    FName: string;
    FPasType: TLiPasType;
    function GetLseType: TLiLseType;
    function GetLseTypeName: string;
    function GetPasTypeName: string;
  public
    constructor Create(const AName: string; AType: TLiPasType);virtual;
    property Name: string read FName;
    property PasType: TLiPasType read FPasType write FPasType;
    property PasTypeName: string read GetPasTypeName;
    property LseType: TLiLseType read GetLseType;
    property LseTypeName: string read GetLseTypeName;
  end;

  { TLiPasVarbList }

  TLiPasVarbList = class
  private
    FItems: TList;
    function GetItem(Index: integer): TLiPasVarb;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    function Add(const AName: string; AType: TLiPasType): TLiPasVarb;overload;
    function Add(const AName: string): TLiPasVarb;overload;
    property Count: integer read GetCount;
    property Items[Index: integer]: TLiPasVarb read GetItem;default;
  end;

  { TLiPasFunc }

  TLiFuncStyle = (fsNormal, fsMethod, fsGet, fsSet, fsCreate);

  TLiPasFunc = class(TLiPasVarb)
  private
    FStyle: TLiFuncStyle;
    FParams: TLiPasVarbList;
    FParent: TLiPasType;
    FIsDefault: boolean;
    FUnitName: string;
    FGetSet: TLiPasFunc;
    FCE: TLiCompileEnv;
    procedure GenBodyCode(Codes: TStrings);
    procedure GenSetupCode(Codes: TStrings; const MyModule: string);
  public
    constructor Create(const AName: string; AType: TLiPasType);override;
    destructor Destroy;override;
    function HasResult: boolean;
    function LyseeProcName: string;
    function LyseeProcPrototype: string;
    property Params: TLiPasVarbList read FParams;
    property Parent: TLiPasType read FParent;
    property Style: TLiFuncStyle read FStyle;
end;

  { TLiPasFuncList }

  TLiPasFuncList = class
  private
    FItems: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TLiPasFunc;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    procedure Remove(AFunc: TLiPasFunc);
    function Add(const AName: string; AType: TLiPasType): TLiPasFunc;overload;
    function Add(const AName: string): TLiPasFunc;overload;
    property Count: integer read GetCount;
    property Items[Index: integer]: TLiPasFunc read GetItem;default;
  end;

  { TLiPasTokenizer }

  TLiPasSymbol = (psUnit, psProgram, psLibrary, psInterface,
    psUses, psType, psClass, psPacked, psRecord, psConst, psVar, psProp,
    psFunc, psProc, psConstructor, psDestructor, psIn, psOut, psSet, psOf,
    psPrivate, psProtected, psPublic, psPublished, psVirtual, psDynamic,
    psAbstract, psDefault, psOverload, psOverride, psInline, psPlatform,
    psImplementation, psBegin, psEnd, psEqual, psLParen, psRParen,
    psLArray, psRArray, psColon, psSemic, psComma, psID, psNone, psEOF);
  TPascalSymbols = set of TLiPasSymbol;

  RLiPasToken = packed record
    sym : TLiPasSymbol;
    name: string;
    row : integer;
    col : integer;
  end;
  PLiPasToken = ^RLiPasToken;

  TLiPasTokenizer = class
  private
    FTokens: array[0..3] of RLiPasToken;
    FIndex: integer;
    FCode: string;
    FSize: integer;
    FPosition: integer;
    FRow: integer;
    FCol: integer;
    FChar: char;
    FParser: TLiPasTranslater;
    FCE: TLiCompileEnv;
    function GetChar: boolean;
    function PeekChar: char;
    function PrevChar: char;
    function GotoChar(Chars: TSysCharSet): boolean;
    function SkipSpaces: boolean;
    procedure SetCode(const Value: string);
    procedure Stop;
  public
    constructor Create(AParser: TLiPasTranslater; const Source: string);
    destructor Destroy;override;
    function GetToken(T: PLiPasToken): boolean;
    function GetNextToken: PLiPasToken;
    function GetNextNotNoneToken: PLiPasToken;
    function GotoToken(Syms: TPascalSymbols): boolean;
    function GotoID(const ID: string): boolean;
    function PeekSymbol: TLiPasSymbol;
    function Current: PLiPasToken;
    property Position: integer read FPosition;
    property Code: string read FCode write SetCode;
  end;

  { TLiPasTranslater }

  TLiPasTranslater = class
  private
    FTokenizer: TLiPasTokenizer;
    FModule: string;
    FUses: string;
    FNewLseTypes: TLiLseTypeList;
    FNewPasTypes: TLiPasTypeList;
    FConstants: array[TLiCompileEnv] of TStringList;
    FFuncs: TLiPasFuncList;
    FErrors: TStrings;
    FSourceCodes: TStrings;
    FResultCodes: TStrings;
    FMyModule: string;
    function GetResultUnitName: string;
  protected
    procedure ParseConst;
    procedure ParseType;
    procedure ParseFunc(Clss: TLiPasType);
    procedure ParseProp(Clss: TLiPasType);
    procedure ParseTo(Syms: TPascalSymbols);
    function Last: PLiPasToken;
    function GetNextToken: PLiPasToken;
    function GotoToken(Syms: TPascalSymbols): boolean;
    function SymTestNext(Syms: TPascalSymbols): PLiPasToken;
    function SymTestLast(Syms: TPascalSymbols): PLiPasToken;
    procedure AddConst(const AName: string);
    function FindFunc(const AName: string; Parent: TLiPasType): TLiPasFunc;
    function AddFunc(const AName: string): TLiPasFunc;
    procedure GenerateCodes;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear(ClearSourceCodes: boolean);
    procedure Process;
    function NewClassCount: integer;
    property Module: string read FModule;
    property ResultUnitName: string read GetResultUnitName;
    property Errors: TStrings read FErrors;
    property SourceCodes: TStrings read FSourceCodes;
    property ResultCodes: TStrings read FResultCodes;
  end;

function GetKeyword(const ID: string): TLiPasSymbol;
function ExtractDirective(var T: string): string;
function Fetch(var T: string; const M: string = ':'): string;
function CommandLMU: boolean;

var
  LseTypes: TLiLseTypeList;
  LseNil: TLiLseType;
  PasTypes: TLiPasTypeList;
  PasNil : TLiPasType;

implementation

{$IFNDEF FPC}
uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}Math, Types;
{$ENDIF}

procedure pp_pmc_parseFile(const Param: TLiParam);
var
  P: TLiPasTranslater;
  L: TLiStringList;
begin
  P := TLiPasTranslater.Create;
  try
    P.SourceCodes.LoadFromFile(Param[0].GetFileName);
    P.Process;
    L := TLiStringList.Create;
    L.StringList.Assign(P.ResultCodes);
    Param.Result.AsStringList := L;
  finally
    P.Free;
  end;
end;

const
  Symbols: array[TLiPasSymbol] of string = (
    'unit', 'program', 'library', 'interface', 'uses', 'type', 'class',
    'packed', 'record', 'const', 'var', 'property', 'function',
    'procedure', 'constructor', 'destructor', 'in', 'out', 'set', 'of',
    'private', 'protected', 'public', 'published', 'virtual', 'dynamic',
    'abstract', 'default', 'overload', 'override', 'inline', 'platform',
    'implementation', 'begin', 'end',  '=', '(', ')', '[', ']', ':', ';',
    ',', 'ID', 'NONE', 'EOF'
  );

function GetKeyword(const ID: string): TLiPasSymbol;
var
  X: TLiPasSymbol;
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

function CommandLMU: boolean;
var
  I: integer;
  F: string;
  T: TLiPasTranslater;
begin
  Result := true;
  try
    if ParamCount > 0 then
    begin
      T := TLiPasTranslater.Create;
      try
        for I := 1 to ParamCount do
        begin
          F := ExpandFileName(Trim(ParamStr(I)));
          Write('process ' + F + ' ... ');
          T.SourceCodes.LoadFromFile(F);
          T.Process;
          Writeln('done');
          T.ResultCodes.SaveToFile(ExtractFilePath(F) + T.ResultUnitName + '.pas');
          if T.Errors.Count > 0 then
            Writeln(T.Errors.Text);
        end;
      finally
        T.Free;
      end;
    end
    else
    begin
      F := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
      Writeln(Format('Usage: %s pmc-files ...', [F]));
      Result := false;
    end;
  except
    Writeln(ExceptionStr);
    Result := false;
  end;
end;

{ TLyLseType }

constructor TLiLseType.Create(AType: TLiType; const AMyName, APasTypeName: string);
begin
  FType := AType;
  FMyName := AMyName;
  FPasTypeName := APasTypeName;
end;

destructor TLiLseType.Destroy;
begin
  if FType.Module = nil then
    FreeAndNil(FType);
  inherited Destroy;
end;

function TLiLseType.EnumItemList: TStrings;
begin
  Result := TStringList.Create;
  Result.CommaText := ReplaceAll(TrimAll(FEnumItems), '''', '');
end;

function TLiLseType.GetName: string;
begin
  Result := FType.Name;
end;

function TLiLseType.IsClass: boolean;
begin
  Result := (FType.Style = tsObject);
end;

function TLiLseType.IsEnum: boolean;
begin
  Result := FType.IsEnum;
end;

function TLiLseType.IsEnumSet: boolean;
begin
  Result := FType.IsEnumSet;
end;

{ TLyLseTypeList }

function TLiLseTypeList.GetCount: integer;
begin
  if FItems <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function TLiLseTypeList.GetItem(Index: integer): TLiLseType;
begin
  Result := TLiLseType(FItems[Index]);
end;

constructor TLiLseTypeList.Create;
begin
  FItems := TList.Create;
end;

destructor TLiLseTypeList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TLiLseTypeList.Find(AType: TLiType): TLiLseType;
var
  I: integer;
begin
  for I := 0 to GetCount - 1 do
  begin
    Result := GetItem(I);
    if Result.FType = AType then Exit;
  end;
  if Self <> LseTypes then
    Result := LseTypes.Find(AType) else
    Result := nil;
end;

function TLiLseTypeList.Add(AType: TLiType; const MyName, PasTypeName: string): TLiLseType;
begin
  Result := Find(AType);
  if Result = nil then
  begin
    Result := TLiLseType.Create(AType, MyName, PasTypeName);
    FItems.Add(Result);
  end;
end;

function TLiLseTypeList.ClassCount: integer;
var
  I: integer;
  T: TLiLseType;
begin
  Result := 0;
  for I := 0 to GetCount - 1 do
  begin
    T := GetItem(I);
    if T.IsClass then Inc(Result);
  end;
end;

procedure TLiLseTypeList.Clear;
var
  I: integer;
  T: TLiLseType;
begin
  for I := GetCount - 1 downto 0 do
  begin
    T := GetItem(I);
    FItems.Delete(I);
    T.Free;
  end;
end;

function TLiLseTypeList.Find(const AName: string): TLiLseType;
var
  I: integer;
begin
  for I := 0 to GetCount - 1 do
  begin
    Result := GetItem(I);
    if MatchID(Result.FType.Name, AName) then Exit;
  end;
  if Self <> LseTypes then
    Result := LseTypes.Find(AName) else
    Result := nil;
end;

function TLiLseTypeList.FirstEnumSetIndex: integer;
var
  I: integer;
begin
  for I := 0 to GetCount - 1 do
    if GetItem(I).IsEnumSet then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

{ TLyPasType }

constructor TLiPasType.Create(const TypeName: string);
begin
  FName := TypeName;
end;

function TLiPasType.GetLseTypeName: string;
begin
  Result := FLseType.Name;
end;

{ TLyPasTypeList }

function TLiPasTypeList.GetCount: integer;
begin
  if FItems <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function TLiPasTypeList.GetItem(Index: integer): TLiPasType;
begin
  Result := TLiPasType(FItems[Index]);
end;

constructor TLiPasTypeList.Create;
begin
  FItems := TList.Create;
end;

destructor TLiPasTypeList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TLiPasTypeList.Add(AType: TLiLseType; const AName: string): TLiPasType;
begin
  Result := Find(AName);
  if (Result = nil) and (AType <> nil) then
  begin
    Result := TLiPasType.Create(AName);
    Result.FLseType := AType;
    FItems.Add(Result);
  end;
end;

procedure TLiPasTypeList.Clear;
var
  I: integer;
  T: TLiPasType;
begin
  for I := GetCount - 1 downto 0 do
  begin
    T := GetItem(I);
    FItems.Delete(I);
    T.Free;
  end;
end;

procedure TLiPasTypeList.Add(AType: TLiType; const Types: array of string);
var
  I: integer;
  T: TLiLseType;
begin
  T := LseTypes.Find(AType);
  if T <> nil then
    for I := 0 to Length(Types) - 1 do
      Add(T, Types[I]);
end;

function TLiPasTypeList.Find(const AName: string): TLiPasType;
var
  I: integer;
begin
  for I := 0 to GetCount - 1 do
  begin
    Result := GetItem(I);
    if MatchID(Result.FName, AName) then Exit;
  end;
  if Self <> PasTypes then
    Result := PasTypes.Find(AName) else
    Result := nil;
end;

function TLiPasTypeList.FindByName(const AName: string): TLiPasType;
begin
  Result := Find(AName);
  if Result = nil then
    Throw('unknown data type: %s', [AName]);
end;

{ TLiPasVarb }

constructor TLiPasVarb.Create(const AName: string; AType: TLiPasType);
begin
  FName := AName;
  FPasType := AType;
end;

function TLiPasVarb.GetLseType: TLiLseType;
begin
  Result := FPasType.FLseType;
end;

function TLiPasVarb.GetLseTypeName: string;
begin
  Result := LseType.Name;
end;

function TLiPasVarb.GetPasTypeName: string;
begin
  Result := FPasType.Name;
end;

{ TLyPasNodeList }

function TLiPasVarbList.GetCount: integer;
begin
  if FItems <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function TLiPasVarbList.GetItem(Index: integer): TLiPasVarb;
begin
  Result := TLiPasVarb(FItems[Index]);
end;

constructor TLiPasVarbList.Create;
begin
  FItems := TList.Create;
end;

destructor TLiPasVarbList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TLiPasVarbList.Clear;
var
  I: integer;
  V: TLiPasVarb;
begin
  for I := Count - 1 downto 0 do
  begin
    V := GetItem(I);
    FItems.Delete(I);
    V.Free;
  end;
end;

function TLiPasVarbList.Add(const AName: string; AType: TLiPasType): TLiPasVarb;
begin
  Result := TLiPasVarb.Create(AName, AType);
  FItems.Add(Result);
end;

function TLiPasVarbList.Add(const AName: string): TLiPasVarb;
begin
  Result := Add(AName, PasNil);
end;

{ TLiPasFunc }

constructor TLiPasFunc.Create(const AName: string; AType: TLiPasType);
begin
  inherited;
  FParams := TLiPasVarbList.Create;
  FStyle := fsNormal;
end;

destructor TLiPasFunc.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

procedure TLiPasFunc.GenBodyCode(Codes: TStrings);

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
    T: TLiLseType;
    P: TLiPasType;
  begin
    P := FParams[X].FPasType;
    T := P.FLseType;
    Result := '';
    case T.FType.TID of
    //TID_VARIANT : Result := '';
    //TID_NIL     : Result := '';
      TID_CHAR    : Result := Format('Param[%d].AsChar', [X]);
      TID_INTEGER : Result := Format('Param[%d].AsInteger', [X]);
      TID_FLOAT   : Result := Format('Param[%d].AsFloat', [X]);
      TID_CURRENCY: Result := Format('Param[%d].AsCurrency', [X]);
      TID_TIME    : Result := Format('Param[%d].AsTime', [X]);
      TID_BOOLEAN : Result := Format('Param[%d].AsBoolean', [X]);
      TID_TYPE    : Result := Format('Param[%d].AsType', [X]);
      TID_STRING  : Result := Format('Param[%d].AsString', [X]);
      TID_MODULE  : Result := Format('Param[%d].AsModule', [X]);
      TID_FUNCTION: Result := Format('Param[%d].AsFunc', [X]);
      TID_HASHLIST: Result := Format('Param[%d].AsHashList', [X]);
      TID_LIST    : Result := Format('Param[%d].AsList', [X]);
      TID_STRLIST : Result := Format('Param[%d].AsStringList', [X]);
      else
      if T.FType.IsEnum then
        Result := Format('%s(Param[%d].AsInteger)', [P.FName, X]) else
      if T.FType.IsEnumSet then
        Result := Format('Get%s(Param[%d])', [P.FName, X]) else
        Result := Format('%s(Param[%d].GetOA(%s))', [P.FName, X, T.FMyName]);
    end;
    if Result <> '' then
      if not MatchID(T.FPasTypeName, P.FName)  then
        Result := P.FName + '(' + Result + ')';
  end;

  function get_call: string;
  var
    X, N: integer;
  begin
    N := FParams.Count;
    if FStyle in [fsGet, fsSet] then
    begin
      Result := FParams[0].Name + '.' + Name;
      if FStyle = fsSet then Dec(N);
      if N > 1 then
      begin
        Result := Result + '[' + FParams[1].Name;
        for X := 2 to N - 1 do
          Result := Result + ', ' + FParams[X].Name;
        Result := Result + ']';
      end;
      if FStyle = fsSet then
        Result := Result + ' := ' + FParams[N].Name;
    end
    else
    if FStyle = fsMethod then
    begin
      Result := FParams[0].Name + '.' + Name;
      if N > 1 then
      begin
        Result := Result + '(' + FParams[1].Name;
        for X := 2 to N - 1 do
          Result := Result + ', ' + FParams[X].Name;
        Result := Result + ')';
      end;
    end
    else
    begin
      if FStyle = fsCreate then
        Result := FParent.Name + '.' + Name else
      if FUnitName <> '' then
        Result := FUnitName + '.' + Name else
        Result := Name;
      if N > 0 then
      begin
        Result := Result + '(' + FParams[0].Name;
        for X := 1 to N - 1 do
          Result := Result + ', ' + FParams[X].Name;
        Result := Result + ')';
      end;
    end;
  end;

  function set_result: string;
  var
    T: TLiLseType;
    P: TLiPasType;
    V: string;
  begin
    P := FPasType;
    T := P.FLseType;
    if not MatchID(T.FPasTypeName, P.FName)  then
      V := T.FPasTypeName + '(' + get_call + ')' else
      V := get_call;
    Result := '';
    case T.FType.TID of
    //TID_VARIANT : Result := '';
    //TID_NIL     : Result := '';
      TID_CHAR    : Result := Format('Param.Result.AsChar := %s', [V]);
      TID_INTEGER : Result := Format('Param.Result.AsInteger := %s', [V]);
      TID_FLOAT   : Result := Format('Param.Result.AsFloat := %s', [V]);
      TID_CURRENCY: Result := Format('Param.Result.AsCurrency := %s', [V]);
      TID_TIME    : Result := Format('Param.Result.AsTime := %s', [V]);
      TID_BOOLEAN : Result := Format('Param.Result.AsBoolean := %s', [V]);
      TID_TYPE    : Result := Format('Param.Result.AsType := %s', [V]);
      TID_STRING  : Result := Format('Param.Result.AsString := %s', [V]);
      TID_MODULE  : Result := Format('Param.Result.AsModule := %s', [V]);
      TID_FUNCTION: Result := Format('Param.Result.AsFunc := %s', [V]);
      TID_HASHLIST: Result := Format('Param.Result.AsHashList := %s', [V]);
      TID_LIST    : Result := Format('Param.Result.AsList := %s', [V]);
      TID_STRLIST : Result := Format('Param.Result.AsStringList := %s', [V]);
      else
      if T.FType.IsEnum then
        Result := Format('%s.SetValue(Param.Result, Ord(%s))', [T.FMyName, V]) else
      if T.FType.IsEnumSet then
        Result := Format('Set%s(Param.Result, %s)', [P.FName, V]) else
        Result := Format('Param.Result.SetTOA(%s, pointer(%s))', [T.FMyName, V]);
    end;
  end;

var
  I: integer;
  P: TLiPasVarb;
begin
  Add('');
  if FCE = ceFPC then Add('{$IFDEF FPC}') else
  if FCE = ceDelphi then Add('{$IFNDEF FPC}');
  Add(LyseeProcPrototype);
  if FParams.Count > 0 then
  begin
    Add('var');
    for I := 0 to FParams.Count - 1 do
    begin
      P := FParams[I];
      Fmt('  %s: %s;', [P.Name, P.PasType.Name]);
    end;
  end;
  Add('begin');
  for I := 0 to FParams.Count - 1 do
  begin
    P := FParams[I];
    if (I = 0) and (FStyle in [fsMethod, fsGet, fsSet]) then
      Fmt('  if not Param.GetSelf(%s) then Exit;', [P.Name]) else
      Fmt('  %s := %s;', [P.Name, get_param(I)]);
  end;
  if HasResult then
    Add('  ' + set_result + ';') else
    Add('  ' + get_call + ';');
  Add('end;');
  if FCE <> cePascal then Add('{$ENDIF}');
end;

procedure TLiPasFunc.GenSetupCode(Codes: TStrings; const MyModule: string);

  function param_names(Start: integer; Comma: boolean): string;
  var
    I: integer;
    P: TLiPasVarb;
  begin
    Result := '';
    for I := Start to FParams.Count - 1 do
    begin
      P := FParams[I];
      if Comma or (Result <> '') then
        Result := Result + ', ''' + P.FName + '''' else
        Result := '''' + P.FName + '''';
    end;
  end;

  function param_types(Start: integer; Comma: boolean): string;
  var
    I: integer;
    P: TLiPasVarb;
  begin
    Result := '';
    for I := Start to FParams.Count - 1 do
    begin
      P := FParams[I];
      if Comma or (Result <> '') then
        Result := Result + ', ' + P.FPasType.FLseType.FMyName else
        Result := P.FPasType.FLseType.FMyName;
    end;
  end;

  function setup_normal: string;
  var
    H: string;
  begin
    H := '  ' + MyModule + '.AddFunc([''';
    if FName[1] = '_' then
      Result := H + Copy(FName, 2, Length(FName)) + '''' else
      Result := H + FName + '''';
    Result := Result + param_names(0, true);
    Result := Result + '], [';
    if HasResult then
       Result := Result + FPasType.LseType.FMyName else
       Result := Result + 'my_nil';
    Result := Result + param_types(0, true);
    Result := Result + '],' + sLineBreak +
       StringOfChar(' ', Length(H) - 2) +
       '{$IFDEF FPC}@{$ENDIF}' + LyseeProcName + ');';
  end;

  function setup_method: string;
  var
    H: string;
  begin
    H := '  ' + FParent.FLseType.FMyName + '.AddMethod([''';
    Result := H + FName + '''';
    Result := Result + param_names(1, true);
    if HasResult then
      Result := Result + '], [' + FPasType.LseType.FMyName else
      Result := Result + '], [my_nil';
    Result := Result + param_types(1, true);
    Result := Result + '],' + sLineBreak +
       StringOfChar(' ', Length(H) - 2) +
       '{$IFDEF FPC}@{$ENDIF}' + LyseeProcName + ');';
  end;

  function setup_property: string;
  var
    H: string;
  begin
    H := '  ' + FParent.FLseType.FMyName + '.SetupProp(''';
    if FIsDefault then
      Result := H + ''', ' else
      Result := H + FName + ''', ';
    Result := Result + FPasType.LseType.FMyName + ', [';
    Result := Result + param_names(1, false);
    Result := Result + '], [';
    Result := Result + param_types(1, false);
    Result := Result + '],' + sLineBreak + StringOfChar(' ', Length(H) - 2) +
      '{$IFDEF FPC}@{$ENDIF}' + LyseeProcName;
    if FGetSet = nil then
      Result := Result + ', nil' else
      Result := Result + ',' + sLineBreak + StringOfChar(' ', Length(H) - 2) +
        '{$IFDEF FPC}@{$ENDIF}' + FGetSet.LyseeProcName;
    Result := Result + ');';
  end;

  function setup_create: string;
  var
    H: string;
  begin
    H := '  ' + FParent.FLseType.FMyName + '.AddMethod([''';
    Result := H + 'create''';
    Result := Result + param_names(0, true);
    Result := Result + '], [' + FPasType.LseType.FMyName;
    Result := Result + param_types(0, true);
    Result := Result + '],' + sLineBreak + StringOfChar(' ', Length(H) - 2) +
      '{$IFDEF FPC}@{$ENDIF}' + LyseeProcName + ');';
  end;

var
  S: string;
begin
  if FCE = ceFPC then Codes.Add('  {$IFDEF FPC}') else
  if FCE = ceDelphi then Codes.Add('  {$IFNDEF FPC}');
  S := '';
  case FStyle of
    fsNormal: S := setup_normal;
    fsMethod: S := setup_method;
    fsGet   : S := setup_property;
    fsCreate: S := setup_create;
  end;
  if S <> '' then Codes.Add(S);
  if FCE <> cePascal then Codes.Add('  {$ENDIF}');
end;

function TLiPasFunc.HasResult: boolean;
begin
  Result := (FStyle <> fsSet) and (FPasType <> PasNil);
end;

function TLiPasFunc.LyseeProcName: string;
begin
  if FStyle = fsCreate then
    Result := FParent.LseTypeName + '_create' else
  if FStyle = fsMethod then
      Result := FParent.LseTypeName + '_' + LowerHead(Name) else
  if FStyle = fsGet then
  begin
    Result := FParent.LseTypeName + '_get';
    if not FIsDefault then
      Result := Result + UpperHead(Name);
  end
  else
  if FStyle = fsSet then
  begin
    Result := FParent.LseTypeName + '_set';
    if not FIsDefault then
      Result := Result + UpperHead(Name);
  end
  else Result := LowerHead(Name);
  Result := 'pp_' + Result;
end;

function TLiPasFunc.LyseeProcPrototype: string;
begin
  Result := 'procedure ' + LyseeProcName + '(const Param: TLiParam);';
end;

{ TLiPasFuncList }

function TLiPasFuncList.Add(const AName: string;
  AType: TLiPasType): TLiPasFunc;
begin
  Result := TLiPasFunc.Create(AName, AType);
  FItems.Add(Result);
end;

function TLiPasFuncList.Add(const AName: string): TLiPasFunc;
begin
  Result := Add(AName, PasNil);
end;

procedure TLiPasFuncList.Clear;
var
  I: integer;
  F: TLiPasFunc;
begin
  for I := GetCount - 1 downto 0 do
  begin
    F := GetItem(I);
    FItems.Delete(I);
    F.Free;
  end;
end;

constructor TLiPasFuncList.Create;
begin
  FItems := TList.Create;
end;

destructor TLiPasFuncList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TLiPasFuncList.GetCount: integer;
begin
  if FItems <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function TLiPasFuncList.GetItem(Index: integer): TLiPasFunc;
begin
  Result := TLiPasFunc(FItems[Index]);
end;

procedure TLiPasFuncList.Remove(AFunc: TLiPasFunc);
begin
  if AFunc <> nil then
  begin
    FItems.Remove(AFunc);
    AFunc.Free;
  end;
end;

{ TLiPasTokenizer }

constructor TLiPasTokenizer.Create(AParser: TLiPasTranslater; const Source: string);
begin
  FParser := AParser;
  FillChar(FTokens, sizeof(FTokens), 0);
  SetCode(Source);
end;

function TLiPasTokenizer.Current: PLiPasToken;
begin
  if FIndex >= 0 then
    Result := @FTokens[FIndex] else
    Result := GetNextToken;
end;

destructor TLiPasTokenizer.Destroy;
var
  I: integer;
begin
  for I := 0 to Length(FTokens) - 1 do
    FTokens[I].name := '';
  inherited;
end;

function TLiPasTokenizer.GetChar: boolean;
var
  F13: boolean;
begin
  F13 := (FChar = #13);
  if F13 or (FChar = #10) then
  begin
    Inc(FRow);
    FCol := 1;
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

function TLiPasTokenizer.GetNextNotNoneToken: PLiPasToken;
begin
  Result := GetNextToken;
  while Result^.sym = psNone do
    GetToken(Result);
end;

function TLiPasTokenizer.GetNextToken: PLiPasToken;
begin
  FIndex := (FIndex + 1) mod Length(FTokens);
  Result := @FTokens[FIndex];
  GetToken(Result);
end;

function TLiPasTokenizer.GetToken(T: PLiPasToken): boolean;

  procedure get_ident;
  begin
    T^.name := FChar;
    while GetChar and CharInSet(FChar, CS_ID) do
      T^.name := T^.name + FChar;
    T^.sym := GetKeyword(T^.name);
    if T^.sym = psImplementation then Stop;
  end;

  procedure skip_string;
  begin
    while FChar = '''' do
    begin
      GetChar;
      GotoChar(['''']);
      GetChar;
    end;
    T^.sym := psNone;
  end;

  procedure skip_number;
  begin
    while CharInSet(FChar, CS_ID + ['$', '#']) do GetChar;
    T^.sym := psNone;
  end;

  procedure get_symbol(Sym: TLiPasSymbol);
  begin
    T^.sym := Sym;
    GetChar;
  end;

  function get_token: boolean;
  begin
    Result := (FChar <> #0) and SkipSpaces;
    if Result then
    begin
      T^.sym := psNone;
      T^.name := '';
      T^.row := FRow;
      T^.col := FCol;
      if CharInSet(FChar, CS_HEAD) then get_ident else
      case FChar of
        ''''    : skip_string;
        '0'..'9': skip_number;
        '$'     : skip_number;
        '#'     : skip_number;
        '('     : get_symbol(psLParen);
        ')'     : get_symbol(psRParen);
        '['     : get_symbol(psLArray);
        ']'     : get_symbol(psRArray);
        ':'     : get_symbol(psColon);
        ';'     : get_symbol(psSemic);
        ','     : get_symbol(psComma);
        '='     : get_symbol(psEqual);
        else      get_symbol(psNone);
      end;
    end
    else
    begin
      T^.sym := psEOF;
      Stop;
    end;
  end;

begin
  Result := get_token;
end;

function TLiPasTokenizer.GotoChar(Chars: TSysCharSet): boolean;
begin
  repeat Result := CharInSet(FChar, Chars)
  until Result or not GetChar;
end;

procedure TLiPasTokenizer.SetCode(const Value: string);
begin
  FCE := cePascal;
  FCode := Value;
  FSize := Length(FCode);
  FIndex := -1;
  FPosition := 1;
  FRow := 1;
  FCol := 1;
  if FPosition <= FSize then
    FChar := FCode[FPosition] else
    FChar := #0;
end;

function TLiPasTokenizer.GotoToken(Syms: TPascalSymbols): boolean;
var
  T: PLiPasToken;
begin
  T := Current;
  Result := (T^.sym in Syms);
  if not Result then
  begin
    while not (T^.sym in Syms) do if not GetToken(T) then Exit;
    Result := (T^.sym in Syms);
  end;
end;

function TLiPasTokenizer.GotoID(const ID: string): boolean;
begin
  Result := false;
  while GotoToken([psID]) do
    if MatchID(ID, Current^.name) then
    begin
      Result := true;
      Exit;
    end
    else
    if not GetToken(Current) then Exit;
end;

function TLiPasTokenizer.PeekSymbol: TLiPasSymbol;
var
  T: PLiPasToken;
  P: integer;
  H: char;
begin
  P := FPosition;
  H := FChar;
  try
    T := @FTokens[(FIndex + 1) mod Length(FTokens)];
    GetToken(T);
    Result := T^.sym;
  finally
    FPosition := P;
    FChar := H;
  end;
end;

function TLiPasTokenizer.PeekChar: char;
begin
  if FPosition < FSize then
    Result := FCode[FPosition + 1] else
    Result := #0;
end;

function TLiPasTokenizer.PrevChar: char;
var
  I: integer;
begin
  I := FPosition - 1;
  if (I > 0) and (I <= FSize) then
    Result := FCode[I] else
    Result := #0;
end;

procedure TLiPasTokenizer.Stop;
begin
  FCode := '';
  FSize := 0;
  FChar := #0;
end;

function TLiPasTokenizer.SkipSpaces: boolean;

  function match_ce(const S: string): boolean;
  var
    L: integer;
  begin
    L := Length(S);
    Result := MatchID(S, Copy(FCode, FPosition, L));
    if Result then
    begin
      Inc(FPosition, L - 1);
      GetChar;
    end;
  end;

begin
  Result := false;
  while not Result and (FChar <> #0) do
    if FChar <= ' ' then GetChar else
    if FChar = '[' then
    begin
      if match_ce('[pascal]') then FCE := cePascal else
      if match_ce('[free-pascal]') then FCE := ceFPC else
      if match_ce('[delphi]') then FCE := ceDelphi else
        Result := true;
    end
    else
    if FChar = '{' then
    begin
      if GotoChar(['}']) then GetChar else Exit;
    end
    else
    if (FChar = '(') and (PeekChar = '*') then
    begin
      while GotoChar([')']) and (PrevChar <> '*') do
        if not GetChar then Exit;
      GetChar;
    end
    else
    if (FChar = '/') and (PeekChar = '/') then
      GotoChar([#13, #10]) else
      Result := true;
end;

{ TLyPasParser }

function TLiPasTranslater.AddFunc(const AName: string): TLiPasFunc;
begin
  Result := FFuncs.Add(AName);
  Result.FStyle := fsNormal;
  Result.FCE := FTokenizer.FCE;;
end;

procedure TLiPasTranslater.Clear(ClearSourceCodes: boolean);
begin
  if ClearSourceCodes then FSourceCodes.Clear;
  FResultCodes.Clear;
  FTokenizer.SetCode('');
  FConstants[cePascal].Clear;
  FConstants[ceDelphi].Clear;
  FConstants[ceFPC].Clear;
  FFuncs.Clear;
  FNewLseTypes.Clear;
  FNewPasTypes.Clear;
  FErrors.Clear;
  FModule := '';
  FUses := '';
  FMyModule := '';
end;

constructor TLiPasTranslater.Create;
begin
  FTokenizer := TLiPasTokenizer.Create(Self, '');
  FConstants[cePascal] := TStringList.Create;
  FConstants[cePascal].CaseSensitive := false;
  FConstants[ceDelphi] := TStringList.Create;
  FConstants[ceDelphi].CaseSensitive := false;
  FConstants[ceFPC] := TStringList.Create;
  FConstants[ceFPC].CaseSensitive := false;
  FNewLseTypes := TLiLseTypeList.Create;
  FNewPasTypes := TLiPasTypeList.Create;
  FFuncs := TLiPasFuncList.Create;
  FErrors := TStringList.Create;
  FSourceCodes := TStringList.Create;
  FResultCodes := TStringList.Create;
end;

destructor TLiPasTranslater.Destroy;
begin
  Clear(true);
  FreeAll([FNewLseTypes, FConstants[cePascal], FConstants[ceDelphi],
    FConstants[ceFPC], FNewPasTypes, FFuncs, FErrors,
    FSourceCodes, FResultCodes, FTokenizer]);
  inherited;
end;

procedure TLiPasTranslater.ParseTo(Syms: TPascalSymbols);
var
  N: integer;
begin
  N := 0;
  repeat
    if Last^.sym in [psRecord, psInterface, psClass] then
    begin
      GetNextToken;
      if not (Last^.sym in [psSemic, psOf]) then Inc(N);
    end
    else
    if Last^.sym <> psEOF then
    begin
      case Last^.sym of
        psLParen, psLArray: Inc(N);
        psRParen, psRArray, psEnd: Dec(N);
      end;
      GetNextToken;
    end;
  until (Last^.sym = psEOF) or ((Last^.sym in Syms) and (N = 0));
end;

procedure TLiPasTranslater.ParseConst;
begin
  SymTestNext([psID]);
  repeat
    AddConst(Last^.name);
    ParseTo([psSemic]);
    GotoToken([psType, psConst, psFunc, psProc, psID]);
  until Last^.sym <> psID;
end;

procedure TLiPasTranslater.ParseType;

  function parse_enum_items: string;
  begin
    SymTestNext([psID]);
    Result := '''' + Last^.name + '''';
    SymTestNext([psComma, psRParen]);
    while Last^.sym = psComma do
    begin
      SymTestNext([psID]);
      Result := Result + ', ''' + Last^.name + '''';
      SymTestNext([psComma, psRParen]);
    end;
  end;

var
  K, X: string;
  T: TLiType;
  L: TLiLseType;
  P: TLiPasType;
begin
  SymTestNext([psID]);
  repeat
    K := Last^.name;
    SymTestNext([psEqual]);
    SymTestNext([psClass, psID, psLParen, psSet]);
    if Last^.sym = psID then
    begin
      P := FNewPasTypes.Find(Last^.name);
      if P <> nil then
      begin
        P := FNewPasTypes.Add(P.FLseType, K);
        P.FCE := FTokenizer.FCE;
      end
      else FErrors.Add('unknown type: ' + Last^.name);
      SymTestNext([psSemic]);
    end
    else
    if Last^.sym = psLParen then
    begin
      T := TLiEnumType.Create(K, nil, nil);
      L := FNewLseTypes.Add(T, 'my_' + K, K);
      L.FCE := FTokenizer.FCE;
      P := FNewPasTypes.Add(L, K);
      P.FCE := FTokenizer.FCE;
      L.FEnumItems := parse_enum_items;
      SymTestNext([psSemic]);
    end
    else
    if Last^.sym = psSet then
    begin
      SymTestNext([psOf]);
      SymTestNext([psID, psLParen]);
      if Last^.sym = psID then
      begin
        L := FNewLseTypes.Find(Last^.name);
        if (L <> nil) and L.IsEnum then
        begin
          T := TLiEnumType(L.FType).NewEnumSetType(K);
          L := FNewLseTypes.Add(T, 'my_' + K, K);
          L.FCE := FTokenizer.FCE;
          P := FNewPasTypes.Add(L, K);
          P.FCE := FTokenizer.FCE;
        end
        else
        if L = nil then
          FErrors.Add('unknown type: ' + Last^.name) else
          FErrors.Add('unknown enum type: ' + L.Name);
      end
      else
      begin
        T := TLiEnumType.Create('#' + K, nil, nil);
        L := FNewLseTypes.Add(T, 'my_' + K + 'Item', '#' + K);
        L.FCE := FTokenizer.FCE;
        L.FEnumItems := parse_enum_items;
        T := TLiEnumType(T).NewEnumSetType(K);
        L := FNewLseTypes.Add(T, 'my_' + K, K);
        L.FCE := FTokenizer.FCE;
        P := FNewPasTypes.Add(L, K);
        P.FCE := FTokenizer.FCE;
      end;
      SymTestNext([psSemic]);
    end
    else
    begin
      if (Length(K) > 3) and (Copy(K, 1, 3) = 'TLi') and CharInSet(K[4], CS_UPPER) then
        X := 'T' + Copy(K, 4, Length(K)) else
        X := K;
      T := TLiType.Create(X, nil, nil);
      L := FNewLseTypes.Add(T, 'my_' + X, K);
      L.FCE := FTokenizer.FCE;
      P := FNewPasTypes.Add(L, K);
      P.FCE := FTokenizer.FCE;
      while GotoToken([psConstructor, psFunc, psProc, psProp, psEnd]) do
        case Last^.sym of
          psFunc, psProc, psConstructor: ParseFunc(P);
          psProp: ParseProp(P);
          else Break;
        end;
    end;
    GotoToken([psType, psConst, psFunc, psProc, psID]);
  until Last^.sym <> psID;
end;

procedure TLiPasTranslater.AddConst(const AName: string);
begin
  FConstants[FTokenizer.FCE].Values[AName] := AName;
end;

function TLiPasTranslater.FindFunc(const AName: string; Parent: TLiPasType): TLiPasFunc;
var
  I: integer;
begin
  for I := 0 to FFuncs.Count - 1 do
  begin
    Result := FFuncs[I];
    if Result.FParent = Parent then
      if MatchID(AName, Result.FName) then
        Exit;
  end;
  Result := nil;
end;

function TLiPasTranslater.GetNextToken: PLiPasToken;
begin
  Result := FTokenizer.GetNextToken;
end;

function TLiPasTranslater.GetResultUnitName: string;
begin
  Result := 'lm_' + FModule;
end;

function TLiPasTranslater.GotoToken(Syms: TPascalSymbols): boolean;
var
  T: PLiPasToken;
begin
  Result := Last^.sym in Syms;
  if not Result then
  begin
    T := GetNextToken;
    while not (T^.sym in Syms + [psEOF]) do
      T := GetNextToken;
    Result := T^.sym in Syms;
  end;
end;

function TLiPasTranslater.Last: PLiPasToken;
begin
  Result := FTokenizer.Current;
end;

function TLiPasTranslater.NewClassCount: integer;
begin
  Result := FNewLseTypes.ClassCount;
end;

procedure TLiPasTranslater.ParseFunc(Clss: TLiPasType);
var
  S: TLiPasSymbol;
  T: TLiPasType;
  F: TLiPasFunc;
  V: TLiPasVarb;
  I: integer;
begin
  F := nil;
  try
    S := Last^.sym;
    SymTestNext([psID]);
    if FindFunc(Last^.name, Clss) <> nil then
    begin
      ParseTo([psSemic]);
      Exit;
    end;
    F := AddFunc(Last^.name);

    // 1. setup function
    F.FParent := Clss;
    if S = psConstructor then
    begin
      F.FPasType := Clss;
      F.FStyle := fsCreate;
    end
    else
    if Clss <> nil then
    begin
      F.FParams.Add('self', Clss);
      F.FStyle := fsMethod;
    end;

    // 2. parse arguments
    SymTestNext([psLParen, psColon, psSemic]);
    if Last^.sym = psLParen then
    begin
      GotoToken([psID, psRParen]);
      while Last^.sym = psID do
      begin
        repeat
          F.FParams.Add(Last^.name);
          GetNextToken;
          GotoToken([psID, psColon]);
        until Last^.sym = psColon;
        SymTestNext([psID]);
        T := FNewPasTypes.FindByName(Last^.name);
        for I := F.FParams.Count - 1 downto 0 do
        begin
          V := F.FParams[I];
          if V.PasType = PasNil then V.FPasType := T else Break;
        end;
        GotoToken([psSemic, psRParen]);
        if Last^.sym = psSemic then
          GotoToken([psID]);
      end;
      SymTestNext([psSemic, psColon]);
    end;

    // 3. parse function type
    if Last^.sym = psColon then
    begin
      SymTestNext([psID]);
      F.FPasType := FNewPasTypes.FindByName(Last^.name);
      SymTestNext([psSemic]);
    end;
  except
    FErrors.Add(ExceptionStr);
    FFuncs.Remove(F);
  end;
end;

procedure TLiPasTranslater.ParseProp(Clss: TLiPasType);
var
  F, S: TLiPasFunc;
  P: TLiPasVarb;
  I: integer;
  T: TLiPasType;
begin
  F := nil;
  try
    SymTestNext([psID]);
    F := AddFunc(Last^.name);

    // 1. setup reading function
    F.FParent := Clss;
    F.FParams.Add('Self', Clss);
    F.FStyle := fsGet;

    // 2. parse arguments
    GotoToken([psLArray, psColon]);
    if Last^.sym = psLArray then
    repeat
      GotoToken([psID]);
      while Last^.sym = psID do
      begin
        F.FParams.Add(Last^.name);
        GetNextToken;
        GotoToken([psID, psColon]);
      end;
      SymTestNext([psID]);
      T := FNewPasTypes.FindByName(Last^.name);
      for I := F.FParams.Count - 1 downto 0 do
      begin
        P := F.FParams[I];
        if P.PasType = PasNil then P.FPasType := T else Break;
      end;
      GotoToken([psSemic, psRArray]);
    until Last^.sym = psRarray;

    // 3. parse property type
    GotoToken([psID]);
    F.FPasType := FNewPasTypes.FindByName(Last^.name);
    GetNextToken;

    // 4. setup writing function
    S := nil;
    while GotoToken([psID, psSemic]) do
      if Last^.sym = psSemic then Break else
      if LowerCase(Last^.name) = 'write' then
      begin
        S := AddFunc(F.Name);
        S.FParent := F.FParent;
        S.FStyle := fsSet;
        F.FGetSet := S;
        S.FGetSet := F;
        for I := 0 to F.FParams.Count - 1 do
          S.FParams.Add(F.FParams[I].Name, F.FParams[I].PasType);
        S.FParams.Add('Value', F.PasType);
        GotoToken([psSemic]);
        Break;
      end
      else GetNextToken;

    // 5. check default
    if F.FParams.Count > 1 then
      if FTokenizer.PeekSymbol = psDefault then
      begin
        F.FIsDefault := true;
        if S <> nil then
          S.FIsDefault := true;
      end;
  except
    FErrors.Add(ExceptionStr);
    FFuncs.Remove(F);
  end;
end;

procedure TLiPasTranslater.Process;
var
  I: integer;
begin
  Clear(false);
  FTokenizer.SetCode(FSourceCodes.Text);
  try
    // 1. get module name
    SymTestNext([psID]);
    FModule := Last^.name;
    FMyModule := 'my_' + FModule;
    SymTestNext([psColon]);

    // 2. get used unit list
    SymTestNext([psUses]);
    I := FTokenizer.FPosition;
    while FTokenizer.FChar <> ';' do
      if not FTokenizer.GetChar then
        Exit;
    FUses := Trim(Copy(FTokenizer.FCode, I, FTokenizer.FPosition - I));

    // 3. parse constants, classes, functions and procedures
    while GotoToken([psType, psConst, psFunc, psProc]) do
      case Last^.sym of
        psConst: ParseConst;
        psType : ParseType;
        psFunc : ParseFunc(nil);
        psProc : ParseFunc(nil);
      end;

    // 4. generate wraping codes
    GenerateCodes;
  except
    FErrors.Add(ExceptionStr);
  end;
end;

procedure TLiPasTranslater.GenerateCodes;

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
        FTokenizer.SetCode(S);
        if not FTokenizer.GotoID(Units[I]) then
          S := S + ', ' + Units[I];
      end
      else S := Units[I];
    Add('uses');
    Add('  ' + S + ';');
  end;

  procedure Add_type_defs;
  var
    I: integer;
    T: string;
    L: TLiLseType;
  begin
    if NewClassCount > 0 then
    begin
      Add('type');
      for I := 0 to FNewLseTypes.Count - 1 do
      begin
        L := FNewLseTypes[I];
        if L.IsClass then
        begin
          T := 'TLiType_' + L.Name;
          Add('');
          Add('  { %s }', [T]);
          Add('');
          if L.FCE = ceFPC then Add('  {$IFDEF FPC}') else
          if L.FCE = ceDelphi then Add('  {$IFNDEF FPC}');
          Add('  %s = class(TLiType)', [T]);
          Add('  protected');
          Add('    function _IncRefcount(Obj: pointer): integer;override;');
          Add('    function _DecRefcount(Obj: pointer): integer;override;');
          Add('    function _AsString(Obj: pointer): string;override;');
          Add('  end;');
          if L.FCE <> cePascal then Add('  {$ENDIF}');
        end;
      end;
    end;
  end;

  procedure Add_var;
  var
    I: integer;
    L: TLiLseType;
  begin
    Add('var');
    Add('  %s: TLiModule;', [FMyModule]);
    for I := 0 to FNewLseTypes.Count - 1 do
    begin
      L := FNewLseTypes[I];
      if L.FCE = ceFPC then Add('  {$IFDEF FPC}') else
      if L.FCE = ceDelphi then Add('  {$IFNDEF FPC}');
      if L.IsClass then
        Add('  %s: TLiType_%s;', [L.FMyName, L.Name]) else
      if L.FEnumItems <> '' then
        Add('  %s: TLiEnumType;', [L.FMyName]) else
        Add('  %s: TLiEnumSetType;', [L.FMyName]);
      if L.FCE <> cePascal then Add('  {$ENDIF}');
    end;
  end;

  procedure Add_type_methods;
  var
    I: integer;
    T: string;
    L: TLiLseType;
  begin
    for I := 0 to FNewLseTypes.Count - 1 do
    begin
      L := FNewLseTypes[I];
      if L.IsClass then
      begin
        T := 'TLiType_' + L.Name;
        Add('');
        Add('{ %s }', [T]);
        Add('');
        if L.FCE = ceFPC then Add('{$IFDEF FPC}') else
        if L.FCE = ceDelphi then Add('{$IFNDEF FPC}');
        Add('function %s._IncRefcount(Obj: pointer): integer;', [T]);
        Add('begin');
        Add('  if Obj <> nil then');
        Add('    Result := %s(Obj).IncRefcount else', [FNewLseTypes[I].FPasTypeName]);
        Add('    Result := 0;');
        Add('end;');
        Add('');
        Add('function %s._DecRefcount(Obj: pointer): integer;', [T]);
        Add('begin');
        Add('  if Obj <> nil then');
        Add('    Result := %s(Obj).DecRefcount else', [FNewLseTypes[I].FPasTypeName]);
        Add('    Result := 0;');
        Add('end;');
        Add('');
        Add('function %s._AsString(Obj: pointer): string;', [T]);
        Add('begin');
        Add('  if Obj <> nil then');
        Add('    Result := %s(Obj).AsString else', [FNewLseTypes[I].FPasTypeName]);
        Add('    Result := '''';');
        Add('end;');
        if L.FCE <> cePascal then Add('{$ENDIF}');
      end;
    end;
  end;

  procedure Add_lysee_procs;
  var
    I: integer;
  begin
    Add('{ lysee procedures }');
    Add('');
    for I := 0 to FFuncs.Count - 1 do
      FFuncs[I].GenBodyCode(FResultCodes);
  end;

  procedure Add_setup_module_and_types;
  var
    I: integer;
    L: TLiLseType;
    T: string;
  begin
    I := Pos('_', FModule);
    if I > 1 then
      T := Copy(FModule, 1, I - 1) else
      T := FModule;
    Add('  %s := SetupModule(''%s'');', [FMyModule, T]);
    for I := 0 to FNewLseTypes.Count - 1 do
    begin
      L := FNewLseTypes[I];
      if L.FCE = ceFPC then Add('  {$IFDEF FPC}') else
      if L.FCE = ceDelphi then Add('  {$IFNDEF FPC}');
      if L.IsClass then
        Add('  %s := TLiType_%s.Create(''%s'', %s, nil);',
          [L.FMyName, L.Name, L.Name, FMyModule]) else
      if L.FEnumItems <> '' then
      begin
        Add('  %s := TLiEnumType.Create(''%s'', %s, nil);',
          [L.FMyName, L.Name, FMyModule]);
        Add('  %s.Add([%s]);', [L.FMyName, L.FEnumItems]);
      end
      else Add('  %s := %s.NewEnumSetType(''%s'');',
          [L.FMyName, FNewLseTypes.Find(TLiEnumSetType(L.FType).Source).FMyName, L.Name]);
      if L.FCE <> cePascal then Add('  {$ENDIF}');
    end;
  end;

  procedure Add_setup_constants;
  var
    I, F, D: integer;
    N, V: string;
  begin
    for I := 0 to FConstants[cePascal].Count - 1 do
    begin
      N := ExtractNameValue(FConstants[cePascal][I], V);
      Add('  %s.Consts.DefConst(''%s'', %s);', [FMyModule, N, V]);
    end;

    F := FConstants[ceFPC].Count;
    D := FConstants[ceDelphi].Count;

    if F > 0 then Add('  {$IFDEF FPC}') else
    if D > 0 then Add('  {$IFNDEF FPC}') ;

    for I := 0 to FConstants[ceFPC].Count - 1 do
    begin
      N := ExtractNameValue(FConstants[ceFPC][I], V);
      Add('  %s.Consts.DefConst(''%s'', %s);', [FMyModule, N, V]);
    end;

    if (F > 0) and (D > 0) then Add('  {$ELSE}');

    for I := 0 to FConstants[ceDelphi].Count - 1 do
    begin
      N := ExtractNameValue(FConstants[ceDelphi][I], V);
      Add('  %s.Consts.DefConst(''%s'', %s);', [FMyModule, N, V]);
    end;

    if (FConstants[ceFPC].Count > 0)
      or (FConstants[ceDelphi].Count > 0) then
        Add('  {$ENDIF}');
  end;

  procedure Add_setup_lysee_procs;
  var
    I: integer;
  begin
    for I := 0 to FFuncs.Count - 1 do
      FFuncs[I].GenSetupCode(FResultCodes, FMyModule);
  end;

  procedure Add_sets_procs(InInterface: boolean);
  var
    I, X: integer;
    T: TLiLseType;
    L: TStrings;
  begin
    I := FNewLseTypes.FirstEnumSetIndex;
    if I < 0 then Exit;

    while I < FNewLseTypes.GetCount do
    begin
      T := FNewLseTypes[I];
      if T.IsEnumSet then
      begin
        if not InInterface then Add('');
        if T.FCE = ceFPC then Add('{$IFDEF FPC}') else
        if T.FCE = ceDelphi then Add('{$IFNDEF FPC}');
        if InInterface then
          L := nil else
          L := FNewLseTypes.Find(TLiEnumSetType(T.FType).Source).EnumItemList;
        try
          Add('function Get%s(V: TLiValue): %s;', [T.Name, T.Name]);
          if not InInterface then
          begin
            Add('var');
            Add(' S: TLiEnumSet;');
            Add('begin');
            Add('  Result := [];');
            Add('  S := V.AsEnumSet;');
            for X := 0 to L.Count - 1 do
              Add('  if S[Ord(%s)] then Include(Result, %s);', [L[X], L[X]]);
            Add('end;');
            Add('');
          end;
          Add('procedure Set%s(V: TLiValue; Value: %s);', [T.Name, T.Name]);
          if not InInterface then
          begin
            Add('var');
            Add(' S: TLiEnumSet;');
            Add('begin');
            Add('  S := %s.NewEnumSet;', [T.FMyName]);
            Add('  S.SetValue(V);');
            for X := 0 to L.Count - 1 do
              Add('  S[Ord(%s)] := (%s in Value);', [L[X], L[X]]);
            Add('end;');
          end;
        finally
          FreeAndNil(L);
        end;
        if T.FCE <> cePascal then Add('{$ENDIF}');
      end;
      Inc(I);
    end;
    if InInterface then Add('');
  end;

begin
  FResultCodes.Clear;
  Add('unit %s;', [ResultUnitName]);
  Add('');
  Add('{$IFDEF FPC}');
  Add('{$MODE objfpc}{$H+}');
  Add('{$ENDIF}');
  Add('');
  Add('interface');
  Add('');
  Add_uses(['Classes', 'SysUtils', 'basic', 'lysee']);
  Add('');
  Add_type_defs;
  Add('');
  Add_var;
  Add('');
  Add_sets_procs(true);
  Add('implementation');
  Add_sets_procs(false);
  Add('');
  Add_lysee_procs;
  Add('');
  Add_type_methods;
  Add('');
  Add('initialization');
  Add('begin');
  Add_setup_module_and_types;
  Add('');
  Add_setup_constants;
  Add('');
  Add_setup_lysee_procs;
  Add('end;');
  Add('');
  Add('end.');
end;

function TLiPasTranslater.SymTestLast(Syms: TPascalSymbols): PLiPasToken;
begin
  Result := Last;
  if not (Result^.sym in Syms) then
    Throw('Unexpected symbol(%d, %d): %s',
      [Result^.row, Result^.col, Symbols[Result^.sym]]);
end;

function TLiPasTranslater.SymTestNext(Syms: TPascalSymbols): PLiPasToken;
begin
  GetNextToken;
  Result := SymTestLast(Syms);
end;

initialization
begin
  my_system.AddFunc(['pmcFile', 'fileName'], [my_strlist, my_string],
                    {$IFDEF FPC}@{$ENDIF}pp_pmc_parseFile);

  LseTypes := TLiLseTypeList.Create;
  LseNil := LseTypes.Add(my_nil, 'my_nil', '');
  LseTypes.Add(my_char, 'my_char', 'char');
  LseTypes.Add(my_int, 'my_int', 'int64');
  LseTypes.Add(my_float, 'my_float', 'double');
  LseTypes.Add(my_curr, 'my_curr', 'currency');
  LseTypes.Add(my_time, 'my_time', 'TDateTime');
  LseTypes.Add(my_bool, 'my_bool', 'boolean');
  LseTypes.Add(my_type, 'my_type', 'TLiType');
  LseTypes.Add(my_string, 'my_string', 'string');
  LseTypes.Add(my_module, 'my_module', 'TLiModule');
  LseTypes.Add(my_func, 'my_func', 'TLiFunc');
  LseTypes.Add(my_hash, 'my_hashed', 'TLiHashed');
  LseTypes.Add(my_list, 'my_list', 'TLiList');
  LseTypes.Add(my_strlist, 'my_strlist', 'TLiStringList');

  PasTypes := TLiPasTypeList.Create;
  PasNil := PasTypes.Add(LseNil, '');
  PasTypes.Add(my_char, ['char', 'AnsiChar', 'WideChar', 'UnicodeChar']);
  PasTypes.Add(my_int, ['int64', 'integer', 'cardinal', 'THandle', 'dword',
    'longword', 'word', 'byte', 'shortint', 'smallint', 'longint', 'qword',
    'largeint', 'large_int', 'largeuint', 'large_uint', 'cint8', 'cuint8',
    'cchar', 'cschar', 'cuchar', 'cint16', 'cuint16', 'cshort', 'csshort',
    'cushort', 'cint32', 'cuint32', 'cint', 'csint', 'cuint', 'csigned',
    'cunsigned', 'cint64', 'cuint64', 'clonglong', 'cslonglong', 'culonglong',
    'clong', 'cslong', 'culong', 'csize_t', 'u_long', 'u_short', 'cff_t']);
  PasTypes.Add(my_float, ['double', 'extended', 'float', 'single', 'real',
    'cfloat', 'cdouble', 'clongdouble', 'longdouble', 'ValReal']);
  PasTypes.Add(my_curr, ['currency']);
  PasTypes.Add(my_time, ['TDateTime', 'TDate', 'TTime']);
  PasTypes.Add(my_bool, ['boolean', 'bool', 'bytebool', 'wordbool', 'longbool', 'cbool']);
  PasTypes.Add(my_string, ['string', 'AnsiString', 'shortstring', 'WideString',
    'UnicodeString', 'PChar', 'PAnsiChar', 'PWideChar', 'PUnicodeChar']);
  PasTypes.Add(my_module, ['TLiModule']);
  PasTypes.Add(my_func, ['TLiFunc']);
  PasTypes.Add(my_func, ['TLiType']);
  PasTypes.Add(my_list, ['TLiList']);
  PasTypes.Add(my_hash, ['TLiHashed']);
  PasTypes.Add(my_strlist, ['TLiStringList']);
end;

finalization
begin
  FreeAndNil(PasTypes);
  FreeAndNil(LseTypes);
end;

end.
