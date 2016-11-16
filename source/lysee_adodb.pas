{==============================================================================}
{        UNIT: lysee_adodb                                                     }
{ DESCRIPTION: ADO database                                                    }
{   COPYRIGHT: Copyright (c) 2012, Li Yun Jie. All Rights Reserved.            }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2012/05/19                                                      }
{    MODIFIED: 2016/11/05                                                      }
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

  TLiADOConnection = class;

  { TLiADOQuery }

  TLiADOQuery = class(TLiDataSet)
  private
    FQuery: TADOQuery;
  public
    function ExecSQL(const SQL: string): integer;
    procedure OpenSQL(const SQL: string);
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

  { TLiType_TADOQuery }

  TLiType_TADOQuery = class(TLiType_TDataSet);

  { TLiType_TADOConnection }

  TLiType_TADOConnection = class(TLiType_TDataBase);

var

  my_adodb: TLiModule;
  my_TADOConnection: TLiType_TADOConnection;
  my_TADOQuery: TLiType_TADOQuery;
  my_TCursorLocation: TLiEnumType;
  my_TConnectOption: TLiEnumType;
  my_TConnectMode: TLiEnumType;
  my_TIsolationLevel: TLiEnumType;
  my_default: TLiADOConnection;

function NewConnection: TLiADOConnection;
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

procedure CheckDefaultConnection;
begin
  Check(my_default <> nil, 'default ADO connection is nil');
end;

procedure SetupDefaultConnection;
begin
  if my_default = nil then
  begin
    my_default := NewConnection;
    my_default.IncRefcount;
  end;
end;

//-- my_adodb ------------------------------------------------------------------

procedure pp_adodb_makeDefault(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  Param.Result.SetTOA(my_TADOConnection, my_default);
  C := Param[0].GetOA(my_TADOConnection);
  if C <> my_default then
  begin
    my_TADOConnection._DecRefcount(my_default);
    my_TADOConnection._IncRefcount(C);
    my_default := C;
  end;
end;

procedure pp_adodb_defaultConnection(const Param: TLiParam);
begin
  Param.Result.SetTOA(my_TADOConnection, my_default);
end;

procedure pp_adodb_open(const Param: TLiParam);
begin
  if Param.Prmc > 0 then
  begin
    SetupDefaultConnection;
    my_default.Connect(Param[0].AsString);
  end
  else
  begin
    CheckDefaultConnection;
    my_default.Open;
  end;
end;

procedure pp_adodb_close(const Param: TLiParam);
begin
  if my_default <> nil then
    my_default.Close;
end;

procedure pp_adodb_connectSQLServer(const Param: TLiParam);
var
  H, D, U, P: string;
begin
  SetupDefaultConnection;
  H := SelectStr(Trim(Param[0].AsString), '127.0.0.1');
  D := SelectStr(Trim(Param[1].AsString), 'master');
  U := SelectStr(Trim(Param[2].AsString), 'sa');
  P := Param[3].AsString;
  my_default.Connect(CS_SQLSERVER,
    ['user', 'password', 'dbname', 'host'], [U, P, D, H]);
end;

procedure pp_adodb_connectAccess(const Param: TLiParam);
var
  S: string;
begin
  SetupDefaultConnection;
  S := ReplaceTag(CS_ACCESS, 'fname', Param[0].GetFileName);
  if Param.Prmc > 1 then
    S := S + ';Jet OLEDB:Database Password=' + Param[1].AsString;
   {S := S + ';Password=' + Param[1].AsString;}
  my_default.Connect(S);
end;

procedure pp_adodb_connectExcelForRead(const Param: TLiParam);
begin
  SetupDefaultConnection;
  my_default.Connect(CS_EXCEL_R, ['fname'], [Param[0].GetFileName]);
end;

procedure pp_adodb_connectExcelForReadWrite(const Param: TLiParam);
begin
  SetupDefaultConnection;
  my_default.Connect(CS_EXCEL_W, ['fname'], [Param[0].GetFileName]);
end;

procedure pp_adodb_connectOracle(const Param: TLiParam);
var
  H, U, P: string;
begin
  SetupDefaultConnection;
  H := SelectStr(Trim(Param[0].AsString), '127.0.0.1');
  U := Param[1].AsString;
  P := Param[2].AsString;
  my_default.Connect(CS_ORACLE, ['user', 'password', 'host'], [U, P, H]);
end;

procedure pp_adodb_connectTextDB(const Param: TLiParam);
begin
  SetupDefaultConnection;
  my_default.Connect(CS_TEXT, ['dir'], [Param[0].GetFileName]);
end;

procedure pp_adodb_connectDBase(const Param: TLiParam);
begin
  SetupDefaultConnection;
  my_default.FADODB.CursorLocation := clUseServer;
  my_default.Connect(CS_DBASE, ['dir'], [Param[0].GetFileName]);
end;

procedure pp_adodb_connectParadox(const Param: TLiParam);
begin
  SetupDefaultConnection;
  my_default.FADODB.CursorLocation := clUseServer;
  my_default.Connect(CS_PARADOX, ['dir'], [Param[0].GetFileName]);
end;

procedure pp_adodb_inTransaction(const Param: TLiParam);
begin
  CheckDefaultConnection;
  Param.Result.AsBoolean := my_default.InTransaction;
end;

procedure pp_adodb_transact(const Param: TLiParam);
begin
  CheckDefaultConnection;
  my_default.Transact;
end;

procedure pp_adodb_commit(const Param: TLiParam);
begin
  CheckDefaultConnection;
  my_default.Commit;
end;

procedure pp_adodb_rollback(const Param: TLiParam);
begin
  CheckDefaultConnection;
  my_default.Rollback;
end;

procedure pp_adodb_openSQL(const Param: TLiParam);
begin
  CheckDefaultConnection;
  Param.Result.SetTOA(my_TADOQuery, my_default.OpenSQL(Param[0].AsString));
end;

procedure pp_adodb_execSQL(const Param: TLiParam);
begin
  CheckDefaultConnection;
  Param.Result.AsInteger := my_default.ExecSQL(Param[0].AsString);
end;

//-- my_adoquery ---------------------------------------------------------------

procedure pp_query_create(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.SetTOA(my_TADOQuery, C.NewQuery);
end;

procedure pp_query_openSQL(const Param: TLiParam);
var
  Q: TLiADOQuery;
begin
  if Param.GetSelf(Q) then
    Q.OpenSQL(Param[1].AsString);
end;

procedure pp_query_execSQL(const Param: TLiParam);
var
  Q: TLiADOQuery;
begin
  if Param.GetSelf(Q) then
    Param.Result.AsInteger := Q.ExecSQL(Param[1].AsString);
end;

//-- my_adoconn ----------------------------------------------------------------

procedure pp_connection_create(const Param: TLiParam);
begin
  Param.Result.SetTOA(my_TADOConnection, NewConnection);
end;

procedure pp_connection_open(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    if Param.Prmc > 1 then
      C.Connect(Param[1].AsString) else
      C.Open;
end;

procedure pp_connection_connectSQLServer(const Param: TLiParam);
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

procedure pp_connection_connectAccess(const Param: TLiParam);
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

procedure pp_connection_connectExcelForRead(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.Connect(CS_EXCEL_R, ['fname'], [Param[1].GetFileName]);
end;

procedure pp_connection_connectExcelForReadWrite(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.Connect(CS_EXCEL_W, ['fname'], [Param[1].GetFileName]);
end;

procedure pp_connection_connectOracle(const Param: TLiParam);
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

procedure pp_connection_connectTextDB(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.Connect(CS_TEXT, ['dir'], [Param[1].GetFileName]);
end;

procedure pp_connection_connectDBase(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
  begin
    C.FADODB.CursorLocation := clUseServer;
    C.Connect(CS_DBASE, ['dir'], [Param[1].GetFileName]);
  end;
end;

procedure pp_connection_connectParadox(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
  begin
    C.FADODB.CursorLocation := clUseServer;
    C.Connect(CS_PARADOX, ['dir'], [Param[1].GetFileName]);
  end;
end;

procedure pp_connection_openSQL(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.SetTOA(my_TADOQuery, C.OpenSQL(Param[1].AsString));
end;

procedure pp_connection_execSQL(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsInteger := C.ExecSQL(Param[1].AsString);
end;

procedure pp_connection_getConnectionString(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsString := C.ConnectionString;
end;

procedure pp_connection_setConnectionString(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.ConnectionString := Param[1].AsString;
end;

procedure pp_connection_getMode(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    my_TConnectMode.SetValue(Param.Result, Ord(C.FADODB.Mode));
end;

procedure pp_connection_setMode(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.Mode := TConnectMode(Param[1].AsInteger);
end;

procedure pp_connection_getConnectOptions(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    my_TConnectOption.SetValue(Param.Result,
      Ord(C.FADODB.ConnectOptions));
end;

procedure pp_connection_setConnectOptions(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.ConnectOptions := TConnectOption(Param[1].AsInteger);
end;

procedure pp_connection_getCursorLocation(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    my_TCursorLocation.SetValue(Param.Result,
      Ord(C.FADODB.CursorLocation));
end;

procedure pp_connection_setCursorLocation(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.CursorLocation := TCursorLocation(Param[1].AsInteger);
end;

procedure pp_connection_getCommandTimeout(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsInteger := C.FADODB.CommandTimeout;
end;

procedure pp_connection_setCommandTimeout(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.CommandTimeout := Param[1].AsInteger;
end;

procedure pp_connection_getConnectionTimeout(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsInteger := C.FADODB.ConnectionTimeout;
end;

procedure pp_connection_setConnectionTimeout(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.ConnectionTimeout := Param[1].AsInteger;
end;

procedure pp_connection_getIsolationLevel(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    my_TIsolationLevel.SetValue(Param.Result,
      Ord(C.FADODB.IsolationLevel));
end;

procedure pp_connection_setIsolationLevel(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.IsolationLevel := TIsolationLevel(Param[1].AsInteger);
end;

procedure pp_connection_getKeepConnection(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsBoolean := C.FADODB.KeepConnection;
end;

procedure pp_connection_setKeepConnection(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.KeepConnection := Param[1].AsBoolean;
end;

procedure pp_connection_getProvider(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsString := C.FADODB.Provider;
end;

procedure pp_connection_setProvider(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.Provider := Param[1].AsString;
end;

procedure pp_connection_getDefaultDatabase(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsString := C.FADODB.DefaultDatabase;
end;

procedure pp_connection_setDefaultDatabase(const Param: TLiParam);
var
  C: TLiADOConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.DefaultDatabase := Param[1].AsString;
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

initialization
begin
  CoInitialize(nil);
  my_adodb := TLiModule.Create('adodb');
  my_TADOConnection := TLiType_TADOConnection.Create('TADOConnection', my_adodb, my_database);
  my_TADOQuery := TLiType_TADOQuery.Create('TADOQuery', my_adodb, my_dataset);

  { TCursorLocation }

  my_TCursorLocation := TLiEnumType.Create('TCursorLocation', my_adodb, nil);
  my_TCursorLocation.Add(['clUseServer', 'clUseClient']);

  { TConnectOption }

  my_TConnectOption := TLiEnumType.Create('TConnectOption', my_adodb, nil);
  my_TConnectOption.Add(['coConnectUnspecified', 'coAsyncConnect']);

  { TConnectMode }

  my_TConnectMode := TLiEnumType.Create('TConnectMode', my_adodb, nil);
  my_TConnectMode.Add([
    'cmUnknown', 'cmRead', 'cmWrite', 'cmReadWrite', 'cmShareDenyRead',
    'cmShareDenyWrite', 'cmShareExclusive', 'cmShareDenyNone']);

  { TIsolationLevel }

  my_TIsolationLevel := TLiEnumType.Create('TIsolationLevel', my_adodb, nil);
  my_TIsolationLevel.Add([
    'ilUnspecified', 'ilChaos', 'ilReadUncommitted', 'ilBrowse',
    'ilCursorStability', 'ilReadCommitted', 'ilRepeatableRead',
    'ilSerializable', 'ilIsolated']);

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

  //-- my_adodb ----------------------------------------------------------------

  my_adodb.AddFunc(['new'], [my_TADOConnection],
                   {$IFDEF FPC}@{$ENDIF}pp_connection_create);
  my_adodb.AddFunc(['makeDefault', 'connection'],
                   [my_TADOConnection, my_TADOConnection],
                   {$IFDEF FPC}@{$ENDIF}pp_adodb_makeDefault);
  my_adodb.AddFunc(['defaultConnection'], [my_TADOConnection],
                   {$IFDEF FPC}@{$ENDIF}pp_adodb_defaultConnection);
  my_adodb.AddFunc(['open', '_connectionString'],
                   [my_nil, my_string],
                   {$IFDEF FPC}@{$ENDIF}pp_adodb_open);
  my_adodb.AddFunc(['close'], [my_nil],
                   {$IFDEF FPC}@{$ENDIF}pp_adodb_close);
  my_adodb.AddFunc(['connectSQLServer', 'host', 'dbname', 'userID', 'password'],
                   [my_nil, my_string, my_string, my_string, my_string],
                   {$IFDEF FPC}@{$ENDIF}pp_adodb_connectSQLServer);
  my_adodb.AddFunc(['connectAccess', 'mdbFileName', '_password'],
                   [my_nil, my_string, my_string],
                   {$IFDEF FPC}@{$ENDIF}pp_adodb_connectAccess);
  my_adodb.AddFunc(['connectExcelForRead', 'xlsFileName'],
                   [my_nil, my_string],
                   {$IFDEF FPC}@{$ENDIF}pp_adodb_connectExcelForRead);
  my_adodb.AddFunc(['connectExcelForReadWrite', 'xlsFileName'],
                   [my_nil, my_string],
                   {$IFDEF FPC}@{$ENDIF}pp_adodb_connectExcelForReadWrite);
  my_adodb.AddFunc(['connectOracle', 'host', 'user', 'password'],
                   [my_nil, my_string, my_string, my_string, my_string],
                   {$IFDEF FPC}@{$ENDIF}pp_adodb_connectOracle);
  my_adodb.AddFunc(['connectTextDB', 'txtFileDir'],
                   [my_nil, my_string],
                   {$IFDEF FPC}@{$ENDIF}pp_adodb_connectTextDB);
  my_adodb.AddFunc(['connectDBase', 'dbfFileDir'],
                   [my_nil, my_string],
                   {$IFDEF FPC}@{$ENDIF}pp_adodb_connectDBase);
  my_adodb.AddFunc(['connectParadox', 'dbFileDir'],
                   [my_nil, my_string],
                   {$IFDEF FPC}@{$ENDIF}pp_adodb_connectParadox);
  my_adodb.AddFunc(['inTransaction'], [my_bool],
                   {$IFDEF FPC}@{$ENDIF}pp_adodb_inTransaction);
  my_adodb.AddFunc(['transact'], [my_nil],
                   {$IFDEF FPC}@{$ENDIF}pp_adodb_transact);
  my_adodb.AddFunc(['commit'], [my_nil],
                   {$IFDEF FPC}@{$ENDIF}pp_adodb_commit);
  my_adodb.AddFunc(['rollback'], [my_nil],
                   {$IFDEF FPC}@{$ENDIF}pp_adodb_rollback);
  my_adodb.AddFunc(['openSQL', 'SQL'], [my_TADOQuery, my_string],
                   {$IFDEF FPC}@{$ENDIF}pp_adodb_openSQL);
  my_adodb.AddFunc(['execSQL', 'SQL'], [my_int, my_string],
                   {$IFDEF FPC}@{$ENDIF}pp_adodb_execSQL);

  //-- my_adoquery -------------------------------------------------------------

  my_TADOQuery.AddMethod(['create', 'connection'],
                         [my_TADOQuery, my_TADOConnection],
                         {$IFDEF FPC}@{$ENDIF}pp_query_create);
  my_TADOQuery.AddMethod(['openSQL', 'SQL'], [my_nil, my_string],
                         {$IFDEF FPC}@{$ENDIF}pp_query_openSQL);
  my_TADOQuery.AddMethod(['execSQL', 'SQL'], [my_int, my_string],
                         {$IFDEF FPC}@{$ENDIF}pp_query_execSQL);

  //-- my_adoconn --------------------------------------------------------------

  my_TADOConnection.AddMethod(['create'], [my_TADOConnection],
                              {$IFDEF FPC}@{$ENDIF}pp_connection_create);
  my_TADOConnection.AddMethod(['open', '_connectionString'],
                              [my_nil, my_string],
                              {$IFDEF FPC}@{$ENDIF}pp_connection_open);
  my_TADOConnection.AddMethod(['connectSQLServer', 'host', 'dbname', 'userID', 'password'],
                              [my_nil, my_string, my_string, my_string, my_string],
                              {$IFDEF FPC}@{$ENDIF}pp_connection_connectSQLServer);
  my_TADOConnection.AddMethod(['connectAccess', 'mdbFileName', '_password'],
                              [my_nil, my_string, my_string],
                              {$IFDEF FPC}@{$ENDIF}pp_connection_connectAccess);
  my_TADOConnection.AddMethod(['connectExcelForRead', 'xlsFileName'],
                              [my_nil, my_string],
                              {$IFDEF FPC}@{$ENDIF}pp_connection_connectExcelForRead);
  my_TADOConnection.AddMethod(['connectExcelForReadWrite', 'xlsFileName'],
                              [my_nil, my_string],
                              {$IFDEF FPC}@{$ENDIF}pp_connection_connectExcelForReadWrite);
  my_TADOConnection.AddMethod(['connectOracle', 'host', 'user', 'password'],
                              [my_nil, my_string, my_string, my_string, my_string],
                              {$IFDEF FPC}@{$ENDIF}pp_connection_connectOracle);
  my_TADOConnection.AddMethod(['connectTextDB', 'txtFileDir'],
                              [my_nil, my_string],
                              {$IFDEF FPC}@{$ENDIF}pp_connection_connectTextDB);
  my_TADOConnection.AddMethod(['connectDBase', 'dbfFileDir'],
                              [my_nil, my_string],
                              {$IFDEF FPC}@{$ENDIF}pp_connection_connectDBase);
  my_TADOConnection.AddMethod(['connectParadox', 'dbFileDir'],
                              [my_nil, my_string],
                              {$IFDEF FPC}@{$ENDIF}pp_connection_connectParadox);
  my_TADOConnection.AddMethod(['openSQL', 'SQL'], [my_TADOQuery, my_string],
                              {$IFDEF FPC}@{$ENDIF}pp_connection_openSQL);
  my_TADOConnection.AddMethod(['execSQL', 'SQL'], [my_int, my_string],
                              {$IFDEF FPC}@{$ENDIF}pp_connection_execSQL);
  my_TADOConnection.SetupProp('connectionString', my_string,
                              {$IFDEF FPC}@{$ENDIF}pp_connection_getConnectionString,
                              {$IFDEF FPC}@{$ENDIF}pp_connection_setConnectionString);
  my_TADOConnection.SetupProp('Mode', my_TConnectMode,
                              {$IFDEF FPC}@{$ENDIF}pp_connection_getMode,
                              {$IFDEF FPC}@{$ENDIF}pp_connection_setMode);
  my_TADOConnection.SetupProp('ConnectOption', my_TConnectOption,
                              {$IFDEF FPC}@{$ENDIF}pp_connection_getConnectOptions,
                              {$IFDEF FPC}@{$ENDIF}pp_connection_setConnectOptions);
  my_TADOConnection.SetupProp('CursorLocation', my_TCursorLocation,
                              {$IFDEF FPC}@{$ENDIF}pp_connection_getCursorLocation,
                              {$IFDEF FPC}@{$ENDIF}pp_connection_setCursorLocation);
  my_TADOConnection.SetupProp('CommandTimeout', my_int,
                              {$IFDEF FPC}@{$ENDIF}pp_connection_getCommandTimeout,
                              {$IFDEF FPC}@{$ENDIF}pp_connection_setCommandTimeout);
  my_TADOConnection.SetupProp('ConnectionTimeout', my_int,
                              {$IFDEF FPC}@{$ENDIF}pp_connection_getConnectionTimeout,
                              {$IFDEF FPC}@{$ENDIF}pp_connection_setConnectionTimeout);
  my_TADOConnection.SetupProp('IsolationLevel', my_TIsolationLevel,
                              {$IFDEF FPC}@{$ENDIF}pp_connection_getIsolationLevel,
                              {$IFDEF FPC}@{$ENDIF}pp_connection_setIsolationLevel);
  my_TADOConnection.SetupProp('KeepConnection', my_bool,
                              {$IFDEF FPC}@{$ENDIF}pp_connection_getKeepConnection,
                              {$IFDEF FPC}@{$ENDIF}pp_connection_setKeepConnection);
  my_TADOConnection.SetupProp('Provider', my_string,
                              {$IFDEF FPC}@{$ENDIF}pp_connection_getProvider,
                              {$IFDEF FPC}@{$ENDIF}pp_connection_setProvider);
  my_TADOConnection.SetupProp('DefaultDatabase', my_string,
                              {$IFDEF FPC}@{$ENDIF}pp_connection_getDefaultDatabase,
                              {$IFDEF FPC}@{$ENDIF}pp_connection_setDefaultDatabase);
end;

finalization
begin
  my_TADOConnection._DecRefcount(my_default)
end;

end.

