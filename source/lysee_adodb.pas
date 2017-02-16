{==============================================================================}
{        UNIT: lysee_adodb                                                     }
{ DESCRIPTION: ADO database                                                    }
{   COPYRIGHT: Copyright (c) 2012, Li Yun Jie. All Rights Reserved.            }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2012/05/19                                                      }
{    MODIFIED: 2017/01/04                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_adodb;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$IFNDEF WINDOWS}
{$IFDEF MSWINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  Classes, SysUtils, Db, adodb, basic, lysee, lysee_db;

const
  CS_SQLSERVER = 'Provider=SQLOLEDB.1;Persist Security Info=True;' +
                 'User ID={user};Password={password};' +
                 'Initial Catalog={dbname};Data Source={host}';

  CS_ACCESS    = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source={fname};' +
                 'Persist Security Info=False';

  CS_EXCEL_R   = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source={fname};' +
                 'Extended Properties=Excel 8.0;Persist Security Info=False;' +
                 'Mode=Read';

  CS_EXCEL_W   = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source={fname};' +
                 'Extended Properties=Excel 8.0;Persist Security Info=False;' +
                 'Mode=ReadWrite';

  CS_EXCELX    = 'Provider=Microsoft.ACE.OLEDB.12.0;Data Source={fname};' +
                 'Extended Properties=Excel 12.0;Persist Security Info=False;' +
                 'Mode=Read';

  CS_ORACLE    = 'Provider=OraOLEDB.Oracle.1;Persist Security Info=True;' +
                 'User ID={user};Password={password};Data Source={host}';

  CS_TEXT      = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source={dir};' +
                 'Mode=Share Deny None;Extended Properties=TEXT;' +
                 'Persist Security Info=False';

  CS_DBASE     = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source={dir};' +
                 'Mode=Share Deny None;Extended Properties=DBase 5.0;' +
                 'Persist Security Info=False';

  CS_PARADOX   = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source={dir};' +
                 'Mode=Share Deny None;Extended Properties=Paradox 7.X;' +
                 'Persist Security Info=False';
type

  { TLiADOQuery }

  TLiADOQuery = class(TLiDataSet)
  private
    FQuery: TADOQuery;
  public
    function ExecSQL(const SQL: string): integer;
    procedure OpenSQL(const SQL: string);
  end;

  { TLiADOQueryType }

  TLiADOQueryType = class(TLiDataSetType)
  protected
    procedure MyOpenSQL(const Param: TLiParam);
    procedure MyExecSQL(const Param: TLiParam);
    procedure Setup;override;
  end;

  { TLiADOConnection }

  TLiADOConnection = class(TLiDataBase)
  private
    FADODB: TADOConnection;
    FQuery: TADOQuery;
    function GetConnectionString: string;
    procedure SetConnectionString(const Value: string);
    function GetCursorLocation: TCursorLocation;
    procedure SetCursorLocation(Value: TCursorLocation);
    function GetMode: TConnectMode;
    procedure SetMode(Value: TConnectMode);
  public
    destructor Destroy;override;
    procedure Connect(const AConnectionString: string);overload;
    procedure Connect(const AConnectionString: string;
      const Tags, Values: array of string);overload;
    function InTransaction: boolean;override;
    procedure Transact;override;
    procedure Commit;override;
    procedure Rollback;override;
    procedure GetTableNames(List: TStrings; SystemTables: boolean);override;
    procedure GetProcedureNames(List: TStrings);override;
    procedure GetFieldNames(List: TStrings; const Table: string);override;
    function OpenSQL(const SQL: string): TLiADOQuery;
    function ExecSQL(const SQL: string): integer;
    function NewQuery: TLiADOQuery;
    property ConnectionString: string read GetConnectionString write SetConnectionString;
    property CursorLocation: TCursorLocation read GetCursorLocation write SetCursorLocation;
    property Mode: TConnectMode read GetMode write SetMode;
  end;

  { TLiADOConnectionType }

  TLiADOConnectionType = class(TLiDataBaseType)
  protected
    procedure MyCreate(const Param: TLiParam);
    procedure MyOpenEx(const Param: TLiParam);
    procedure MyConnectSQLServer(const Param: TLiParam);
    procedure MyConnectAccess(const Param: TLiParam);
    procedure MyConnectExcelForRead(const Param: TLiParam);
    procedure MyConnectExcelForReadWrite(const Param: TLiParam);
    procedure MyConnectOracle(const Param: TLiParam);
    procedure MyConnectTextDB(const Param: TLiParam);
    procedure MyConnectDBase(const Param: TLiParam);
    procedure MyConnectParadox(const Param: TLiParam);
    procedure MyOpenSQL(const Param: TLiParam);
    procedure MyExecSQL(const Param: TLiParam);
    procedure MyGetConnectionString(const Param: TLiParam);
    procedure MySetConnectionString(const Param: TLiParam);
    procedure MyGetConnectMode(const Param: TLiParam);
    procedure MySetConnectMode(const Param: TLiParam);
    procedure MyGetConnectOption(const Param: TLiParam);
    procedure MySetConnectOption(const Param: TLiParam);
    procedure MyGetCursorLocation(const Param: TLiParam);
    procedure MySetCursorLocation(const Param: TLiParam);
    procedure MyGetCommandTimeout(const Param: TLiParam);
    procedure MySetCommandTimeout(const Param: TLiParam);
    procedure MyGetConnectionTimeout(const Param: TLiParam);
    procedure MySetConnectionTimeout(const Param: TLiParam);
    procedure MyGetIsolationLevel(const Param: TLiParam);
    procedure MySetIsolationLevel(const Param: TLiParam);
    procedure MyGetKeepConnection(const Param: TLiParam);
    procedure MySetKeepConnection(const Param: TLiParam);
    procedure MyGetProvider(const Param: TLiParam);
    procedure MySetProvider(const Param: TLiParam);
    procedure MyGetDefaultDatabase(const Param: TLiParam);
    procedure MySetDefaultDatabase(const Param: TLiParam);
    procedure Setup;override;
  end;

var

  my_adodb: TLiModule;
  my_TADOConnection: TLiADOConnectionType;
  my_TADOQuery: TLiADOQueryType;
  my_TCursorLocation: TLiEnumType;
  my_TConnectOption: TLiEnumType;
  my_TConnectMode: TLiEnumType;
  my_TIsolationLevel: TLiEnumType;
  my_default: TLiADOConnection;

function NewConnection: TLiADOConnection;
function DefConnection: TLiADOConnection;
function SelectStr(const S1, S2: string): string;
function ReplaceTag(const S, Tag, Value: string): string;
function ReplaceTags(const S: string; const Tags, Values: array of string): string;

implementation

uses
  ActiveX;

function NewConnection: TLiADOConnection;
var
  A: TADOConnection;
begin
  A := TADOConnection.Create(nil);
  try
    Result := TLiADOConnection.Create(A);
    Result.FADODB := A;
    A.LoginPrompt := false;
  except
    A.Free;
    raise;
  end;
end;

function DefConnection: TLiADOConnection;
begin
  if my_default = nil then
  begin
    my_default := NewConnection;
    my_default.IncRefcount;
  end;
  Result := my_default;
end;

function SelectStr(const S1, S2: string): string;
begin
  if S1 <> '' then
    Result := S1 else
    Result := S2;
end;

function ReplaceTag(const S, Tag, Value: string): string;
begin
  Result := StringReplace(S, '{' + Tag + '}', Value, [rfReplaceAll, rfIgnoreCase]);
end;

function ReplaceTags(const S: string; const Tags, Values: array of string): string;
var
  I: integer;
begin
  Result := S;
  for I := 0 to Length(Tags) - 1 do
    Result := ReplaceTag(Result, Tags[I], Values[I]);
end;

//-- my_adodb ------------------------------------------------------------------

procedure pp_adodb_adb(const Param: TLiParam);
begin
  Param.Result.SetTOA(my_TADOConnection, DefConnection);
end;

{ TLiADOQuery }

procedure TLiADOQuery.OpenSQL(const SQL: string);
begin
  Close;
  FQuery.SQL.Text := SQL;
  Open;
end;

function TLiADOQuery.ExecSQL(const SQL: string): integer;
begin
  Close;
  FQuery.SQL.Text := SQL;
  try
    Result := FQuery.ExecSQL;
  finally
    Close;
  end;
end;

{ TLiADOQueryType }

procedure TLiADOQueryType.MyExecSQL(const Param: TLiParam);
var
  Q: TLiADOQuery;
begin
  if Param.GetSelf(Q) then
    Param.Result.AsInteger := Q.ExecSQL(Param[1].AsString);
end;

procedure TLiADOQueryType.MyOpenSQL(const Param: TLiParam);
var
  Q: TLiADOQuery;
begin
  if Param.GetSelf(Q) then
    Q.OpenSQL(Param[1].AsString);
end;

procedure TLiADOQueryType.Setup;
begin
  Method('OpenSQL', my_nil, ['SQL'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyOpenSQL);
  Method('ExecSQL', my_int, ['SQL'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyExecSQL);
  inherited;
end;

{ TLiADOConnection }

destructor TLiADOConnection.Destroy;
begin
  Close;
  FreeAndNil(FQuery);
  inherited Destroy;
end;

function TLiADOConnection.ExecSQL(const SQL: string): integer;
begin
  if FQuery = nil then
  begin
    FQuery := TADOQuery.Create(nil);
    FQuery.Connection := FADODB;
  end;
  FQuery.SQL.Text := SQL;
  try
    FQuery.ExecSQL;
    Result := FQuery.RowsAffected;
  finally
    FQuery.Close;
  end;
end;

function TLiADOConnection.GetConnectionString: string;
begin
  Result := FADODB.ConnectionString;
end;

function TLiADOConnection.GetCursorLocation: TCursorLocation;
begin
  Result := FADODB.CursorLocation;
end;

procedure TLiADOConnection.GetProcedureNames(List: TStrings);
begin
  FADODB.GetProcedureNames(List);
end;

procedure TLiADOConnection.GetFieldNames(List: TStrings; const Table: string);
begin
  FADODB.GetFieldNames(Table, List);
end;

function TLiADOConnection.GetMode: TConnectMode;
begin
  Result := FADODB.Mode;
end;

procedure TLiADOConnection.GetTableNames(List: TStrings; SystemTables: boolean);
begin
  FADODB.GetTableNames(List, SystemTables);
end;

procedure TLiADOConnection.Connect(const AConnectionString: string);
begin
  Close;
  FADODB.ConnectionString := AConnectionString;
  Open;
end;

procedure TLiADOConnection.Connect(const AConnectionString: string;
  const Tags, Values: array of string);
begin
  Connect(ReplaceTags(AConnectionString, Tags, Values));
end;

function TLiADOConnection.OpenSQL(const SQL: string): TLiADOQuery;
begin
  Result := NewQuery;
  try
    Result.OpenSQL(SQL);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TLiADOConnection.InTransaction: boolean;
begin
  Result := FADODB.InTransaction;
end;

function TLiADOConnection.NewQuery: TLiADOQuery;
var
  Q: TADOQuery;
begin
  Q := TADOQuery.Create(nil);
  Q.Connection := FADODB;
  Result := TLiADOQuery.Create(Q, Self);
  Result.FQuery := Q;
end;

procedure TLiADOConnection.Transact;
begin
  FADODB.BeginTrans;
end;

procedure TLiADOConnection.Commit;
begin
  FADODB.CommitTrans;
end;

procedure TLiADOConnection.Rollback;
begin
  FADODB.RollbackTrans;
end;

procedure TLiADOConnection.SetConnectionString(const Value: string);
begin
  FADODB.ConnectionString := Value;
end;

procedure TLiADOConnection.SetCursorLocation(Value: TCursorLocation);
begin
  FADODB.CursorLocation := Value;
end;

procedure TLiADOConnection.SetMode(Value: TConnectMode);
begin
  FADODB.Mode := Value;
end;

{ TLiADOConnectionType }

procedure TLiADOConnectionType.MyConnectAccess(const Param: TLiParam);
var
  C: TLiADOConnection;
  S: string;
begin
  if Param.GetSelf(C) then
  begin
    S := ReplaceTag(CS_ACCESS, 'fname', Param[1].GetFileName);
    if Param.Prmc > 2 then
      S := S + ';Jet OLEDB:Database Password=' + Param[2].AsString;
     {S := S + ';Password=' + Param[2].AsString;}
    C.Connect(S);
  end;
end;

procedure TLiADOConnectionType.MyConnectDBase(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
  begin
    C.FADODB.CursorLocation := clUseServer;
    C.Connect(CS_DBASE, ['dir'], [Param[1].GetFileName]);
  end;
end;

procedure TLiADOConnectionType.MyConnectExcelForRead(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.Connect(CS_EXCEL_R, ['fname'], [Param[1].GetFileName]);
end;

procedure TLiADOConnectionType.MyConnectExcelForReadWrite(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.Connect(CS_EXCEL_W, ['fname'], [Param[1].GetFileName]);
end;

procedure TLiADOConnectionType.MyConnectOracle(const Param: TLiParam);
var
  C: TLiADOConnection;
  H, U, P: string;
begin
  if Param.GetSelf(C) then
  begin
    H := SelectStr(Trim(Param[1].AsString), '127.0.0.1');
    U := Param[2].AsString;
    P := Param[3].AsString;
    C.Connect(CS_ORACLE, ['user', 'password', 'host'], [U, P, H]);
  end;
end;

procedure TLiADOConnectionType.MyConnectParadox(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
  begin
    C.FADODB.CursorLocation := clUseServer;
    C.Connect(CS_PARADOX, ['dir'], [Param[1].GetFileName]);
  end;
end;

procedure TLiADOConnectionType.MyConnectSQLServer(const Param: TLiParam);
var
  C: TLiADOConnection;
  H, D, U, P: string;
begin
  if Param.GetSelf(C) then
  begin
    H := SelectStr(Trim(Param[1].AsString), '127.0.0.1');
    D := SelectStr(Trim(Param[2].AsString), 'master');
    U := SelectStr(Trim(Param[3].AsString), 'sa');
    P := Param[4].AsString;
    C.Connect(CS_SQLSERVER, ['user', 'password', 'dbname', 'host'], [U, P, D, H]);
  end;
end;

procedure TLiADOConnectionType.MyConnectTextDB(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.Connect(CS_TEXT, ['dir'], [Param[1].GetFileName]);
end;

procedure TLiADOConnectionType.MyCreate(const Param: TLiParam);
begin
  Param.Result.SetTOA(Self, NewConnection);
end;

procedure TLiADOConnectionType.MyExecSQL(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsInteger := C.ExecSQL(Param[1].AsString);
end;

procedure TLiADOConnectionType.MyGetCommandTimeout(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsInteger := C.FADODB.CommandTimeout;
end;

procedure TLiADOConnectionType.MyGetConnectionString(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsString := C.ConnectionString;
end;

procedure TLiADOConnectionType.MyGetConnectionTimeout(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsInteger := C.FADODB.ConnectionTimeout;
end;

procedure TLiADOConnectionType.MyGetConnectMode(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    my_TConnectMode.SetValue(Param.Result, Ord(C.FADODB.Mode));
end;

procedure TLiADOConnectionType.MyGetConnectOption(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    my_TConnectOption.SetValue(Param.Result,
      Ord(C.FADODB.ConnectOptions));
end;

procedure TLiADOConnectionType.MyGetCursorLocation(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    my_TCursorLocation.SetValue(Param.Result,
      Ord(C.FADODB.CursorLocation));
end;

procedure TLiADOConnectionType.MyGetDefaultDatabase(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsString := C.FADODB.DefaultDatabase;
end;

procedure TLiADOConnectionType.MyGetIsolationLevel(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    my_TIsolationLevel.SetValue(Param.Result,
      Ord(C.FADODB.IsolationLevel));
end;

procedure TLiADOConnectionType.MyGetKeepConnection(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsBoolean := C.FADODB.KeepConnection;
end;

procedure TLiADOConnectionType.MyGetProvider(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsString := C.FADODB.Provider;
end;

procedure TLiADOConnectionType.MyOpenEx(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    if Param.Prmc > 1 then
      C.Connect(Param[1].AsString) else
      C.Open;
end;

procedure TLiADOConnectionType.MyOpenSQL(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.SetTOA(my_TADOQuery, C.OpenSQL(Param[1].AsString));
end;

procedure TLiADOConnectionType.MySetCommandTimeout(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.CommandTimeout := Param[1].AsInteger;
end;

procedure TLiADOConnectionType.MySetConnectionString(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.ConnectionString := Param[1].AsString;
end;

procedure TLiADOConnectionType.MySetConnectionTimeout(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.ConnectionTimeout := Param[1].AsInteger;
end;

procedure TLiADOConnectionType.MySetConnectMode(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.Mode := TConnectMode(Param[1].AsInteger);
end;

procedure TLiADOConnectionType.MySetConnectOption(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.ConnectOptions := TConnectOption(Param[1].AsInteger);
end;

procedure TLiADOConnectionType.MySetCursorLocation(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.CursorLocation := TCursorLocation(Param[1].AsInteger);
end;

procedure TLiADOConnectionType.MySetDefaultDatabase(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.DefaultDatabase := Param[1].AsString;
end;

procedure TLiADOConnectionType.MySetIsolationLevel(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.IsolationLevel := TIsolationLevel(Param[1].AsInteger);
end;

procedure TLiADOConnectionType.MySetKeepConnection(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.KeepConnection := Param[1].AsBoolean;
end;

procedure TLiADOConnectionType.MySetProvider(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.Provider := Param[1].AsString;
end;

procedure TLiADOConnectionType.Setup;
begin
  Method('Create', Self,
         {$IFDEF FPC}@{$ENDIF}MyCreate);
  Method('Open', my_nil,
         ['_ConnectionString'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyOpenEx);
  Method('ConnectSQLServer', my_nil,
         ['Host', 'DBName', 'UserID', 'Password'],
         [my_string, my_string, my_string, my_string],
         {$IFDEF FPC}@{$ENDIF}MyConnectSQLServer);
  Method('ConnectAccess', my_nil,
         ['MdbFileName', '_Password'], [my_string, my_string],
         {$IFDEF FPC}@{$ENDIF}MyConnectAccess);
  Method('ConnectExcelForRead', my_nil,
         ['XlsFileName'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyConnectExcelForRead);
  Method('ConnectExcelForReadWrite', my_nil,
         ['XlsFileName'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyConnectExcelForReadWrite);
  Method('ConnectOracle', my_nil,
         ['Host', 'User', 'Password'],
         [my_string, my_string, my_string],
         {$IFDEF FPC}@{$ENDIF}MyConnectOracle);
  Method('ConnectTextDB', my_nil,
         ['TxtFileDir'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyConnectTextDB);
  Method('ConnectDBase', my_nil,
         ['DbfFileDir'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyConnectDBase);
  Method('ConnectParadox', my_nil,
         ['DbFileDir'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyConnectParadox);
  Method('OpenSQL', my_TADOQuery,
         ['SQL'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyOpenSQL);
  Method('ExecSQL', my_int,
         ['SQL'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyExecSQL);
  Define('ConnectionString', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetConnectionString,
         {$IFDEF FPC}@{$ENDIF}MySetConnectionString);
  Define('Mode', my_TConnectMode,
         {$IFDEF FPC}@{$ENDIF}MyGetConnectMode,
         {$IFDEF FPC}@{$ENDIF}MySetConnectMode);
  Define('ConnectOption', my_TConnectOption,
         {$IFDEF FPC}@{$ENDIF}MyGetConnectOption,
         {$IFDEF FPC}@{$ENDIF}MySetConnectOption);
  Define('CursorLocation', my_TCursorLocation,
         {$IFDEF FPC}@{$ENDIF}MyGetCursorLocation,
         {$IFDEF FPC}@{$ENDIF}MySetCursorLocation);
  Define('CommandTimeout', my_int,
         {$IFDEF FPC}@{$ENDIF}MyGetCommandTimeout,
         {$IFDEF FPC}@{$ENDIF}MySetCommandTimeout);
  Define('ConnectionTimeout', my_int,
         {$IFDEF FPC}@{$ENDIF}MyGetConnectionTimeout,
         {$IFDEF FPC}@{$ENDIF}MySetConnectionTimeout);
  Define('IsolationLevel', my_TIsolationLevel,
         {$IFDEF FPC}@{$ENDIF}MyGetIsolationLevel,
         {$IFDEF FPC}@{$ENDIF}MySetIsolationLevel);
  Define('KeepConnection', my_bool,
         {$IFDEF FPC}@{$ENDIF}MyGetKeepConnection,
         {$IFDEF FPC}@{$ENDIF}MySetKeepConnection);
  Define('Provider', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetProvider,
         {$IFDEF FPC}@{$ENDIF}MySetProvider);
  Define('DefaultDatabase', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetDefaultDatabase,
         {$IFDEF FPC}@{$ENDIF}MySetDefaultDatabase);
  inherited;
end;

initialization
begin
  CoInitialize(nil);
  my_adodb := AddModule('adodb');

  //-- enum types --------------------------------------------------------------

  my_TCursorLocation := my_adodb.AddEnumType('TCursorLocation',
    ['clUseServer', 'clUseClient']);

  my_TConnectOption := my_adodb.AddEnumType('TConnectOption',
    ['coConnectUnspecified', 'coAsyncConnect']);

  my_TConnectMode := my_adodb.AddEnumType('TConnectMode',
    ['cmUnknown', 'cmRead', 'cmWrite', 'cmReadWrite', 'cmShareDenyRead',
     'cmShareDenyWrite', 'cmShareExclusive', 'cmShareDenyNone']);

  my_TIsolationLevel := my_adodb.AddEnumType('TIsolationLevel',
    ['ilUnspecified', 'ilChaos', 'ilReadUncommitted', 'ilBrowse',
     'ilCursorStability', 'ilReadCommitted', 'ilRepeatableRead',
     'ilSerializable', 'ilIsolated']);

  //-- types -------------------------------------------------------------------

  my_TADOQuery := TLiADOQueryType.Create('TADOQuery', my_adodb, my_dataset);
  my_TADOConnection := TLiADOConnectionType.Create('TADOConnection', my_adodb, my_database);

  //-- functions ---------------------------------------------------------------

  my_adodb.AddFunc('Adb', my_TADOConnection, {$IFDEF FPC}@{$ENDIF}pp_adodb_adb);

  //-- constants ---------------------------------------------------------------

  my_adodb.Consts.Add('CS_SQLSERVER').AsString := CS_SQLSERVER;
  my_adodb.Consts.Add('CS_ACCESS').AsString := CS_ACCESS;
  my_adodb.Consts.Add('CS_ORACLE').AsString := CS_ORACLE;
  my_adodb.Consts.Add('CS_EXCEL_R').AsString := CS_EXCEL_R;
  my_adodb.Consts.Add('CS_EXCEL_W').AsString := CS_EXCEL_W;
  my_adodb.Consts.Add('CS_EXCELX').AsString := CS_EXCELX;
  my_adodb.Consts.Add('CS_TEXT').AsString := CS_TEXT;
  my_adodb.Consts.Add('CS_DBASE').AsString := CS_DBASE;
  my_adodb.Consts.Add('CS_PARADOX').AsString := CS_PARADOX;
end;

finalization
begin
  if my_default <> nil then my_default.DecRefcount;
end;

end.

