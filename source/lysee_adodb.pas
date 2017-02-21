{==============================================================================}
{        UNIT: lysee_adodb                                                     }
{ DESCRIPTION: ADO database                                                    }
{   COPYRIGHT: Copyright (c) 2012, Li Yun Jie. All Rights Reserved.            }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2012/05/19                                                      }
{    MODIFIED: 2017/02/21                                                      }
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

  { TLyseeAdoQuery }

  TLyseeAdoQuery = class(TLyseeDataSet)
  private
    FQuery: TADOQuery;
  public
    function ExecSQL(const SQL: string): integer;
    procedure OpenSQL(const SQL: string);
  end;

  { TLyseeAdoQueryType }

  TLyseeAdoQueryType = class(TLyseeDataSetType)
  protected
    procedure MyOpenSQL(const Param: TLyseeParam);
    procedure MyExecSQL(const Param: TLyseeParam);
    procedure Setup;override;
  end;

  { TLyseeAdoConnection }

  TLyseeAdoConnection = class(TLyseeDataBase)
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
    function OpenSQL(const SQL: string): TLyseeAdoQuery;
    function ExecSQL(const SQL: string): integer;
    function NewQuery: TLyseeAdoQuery;
    property ConnectionString: string read GetConnectionString write SetConnectionString;
    property CursorLocation: TCursorLocation read GetCursorLocation write SetCursorLocation;
    property Mode: TConnectMode read GetMode write SetMode;
  end;

  { TLyseeAdoConnectionType }

  TLyseeAdoConnectionType = class(TLyseeDataBaseType)
  protected
    procedure MyCreate(const Param: TLyseeParam);
    procedure MyOpenEx(const Param: TLyseeParam);
    procedure MyConnectSQLServer(const Param: TLyseeParam);
    procedure MyConnectAccess(const Param: TLyseeParam);
    procedure MyConnectExcelForRead(const Param: TLyseeParam);
    procedure MyConnectExcelForReadWrite(const Param: TLyseeParam);
    procedure MyConnectOracle(const Param: TLyseeParam);
    procedure MyConnectTextDB(const Param: TLyseeParam);
    procedure MyConnectDBase(const Param: TLyseeParam);
    procedure MyConnectParadox(const Param: TLyseeParam);
    procedure MyOpenSQL(const Param: TLyseeParam);
    procedure MyExecSQL(const Param: TLyseeParam);
    procedure MyGetConnectionString(const Param: TLyseeParam);
    procedure MySetConnectionString(const Param: TLyseeParam);
    procedure MyGetConnectMode(const Param: TLyseeParam);
    procedure MySetConnectMode(const Param: TLyseeParam);
    procedure MyGetConnectOption(const Param: TLyseeParam);
    procedure MySetConnectOption(const Param: TLyseeParam);
    procedure MyGetCursorLocation(const Param: TLyseeParam);
    procedure MySetCursorLocation(const Param: TLyseeParam);
    procedure MyGetCommandTimeout(const Param: TLyseeParam);
    procedure MySetCommandTimeout(const Param: TLyseeParam);
    procedure MyGetConnectionTimeout(const Param: TLyseeParam);
    procedure MySetConnectionTimeout(const Param: TLyseeParam);
    procedure MyGetIsolationLevel(const Param: TLyseeParam);
    procedure MySetIsolationLevel(const Param: TLyseeParam);
    procedure MyGetKeepConnection(const Param: TLyseeParam);
    procedure MySetKeepConnection(const Param: TLyseeParam);
    procedure MyGetProvider(const Param: TLyseeParam);
    procedure MySetProvider(const Param: TLyseeParam);
    procedure MyGetDefaultDatabase(const Param: TLyseeParam);
    procedure MySetDefaultDatabase(const Param: TLyseeParam);
    procedure Setup;override;
  end;

  { TLyseeAdodbModule }

  TLyseeAdodbModule = class(TLyseeModule)
  private
    procedure DoSetup(Sender: TObject);
  public
    constructor Create(const AName: string);override;
  end;

var
  my_adodb: TLyseeAdodbModule;
  my_defdb: TLyseeAdoConnection;
  my_connection: TLyseeAdoConnectionType;
  my_query: TLyseeAdoQueryType;
  my_cursorLocation: TLyseeEnumType;
  my_connectOption: TLyseeEnumType;
  my_connectMode: TLyseeEnumType;
  my_isolationLevel: TLyseeEnumType;

function NewConnection: TLyseeAdoConnection;
function DefConnection: TLyseeAdoConnection;
function SelectStr(const S1, S2: string): string;
function ReplaceTag(const S, Tag, Value: string): string;
function ReplaceTags(const S: string; const Tags, Values: array of string): string;

implementation

uses
  ActiveX;

function NewConnection: TLyseeAdoConnection;
var
  A: TADOConnection;
begin
  A := TADOConnection.Create(nil);
  try
    Result := TLyseeAdoConnection.Create(A);
    Result.FADODB := A;
    A.LoginPrompt := false;
  except
    A.Free;
    raise;
  end;
end;

function DefConnection: TLyseeAdoConnection;
begin
  if my_defdb = nil then
  begin
    my_defdb := NewConnection;
    my_defdb.IncRefcount;
  end;
  Result := my_defdb;
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

procedure pp_adodb_adb(const Param: TLyseeParam);
begin
  Param.Result.SetTOA(my_connection, DefConnection);
end;

{ TLyseeAdoQuery }

procedure TLyseeAdoQuery.OpenSQL(const SQL: string);
begin
  Close;
  FQuery.SQL.Text := SQL;
  Open;
end;

function TLyseeAdoQuery.ExecSQL(const SQL: string): integer;
begin
  Close;
  FQuery.SQL.Text := SQL;
  try
    Result := FQuery.ExecSQL;
  finally
    Close;
  end;
end;

{ TLyseeAdoQueryType }

procedure TLyseeAdoQueryType.MyExecSQL(const Param: TLyseeParam);
var
  Q: TLyseeAdoQuery;
begin
  if Param.GetSelf(Q) then
    Param.Result.AsInteger := Q.ExecSQL(Param[1].AsString);
end;

procedure TLyseeAdoQueryType.MyOpenSQL(const Param: TLyseeParam);
var
  Q: TLyseeAdoQuery;
begin
  if Param.GetSelf(Q) then
    Q.OpenSQL(Param[1].AsString);
end;

procedure TLyseeAdoQueryType.Setup;
begin
  Method('OpenSQL', my_nil, ['SQL'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyOpenSQL);
  Method('ExecSQL', my_int, ['SQL'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyExecSQL);
  inherited;
end;

{ TLyseeAdoConnection }

destructor TLyseeAdoConnection.Destroy;
begin
  Close;
  FreeAndNil(FQuery);
  inherited Destroy;
end;

function TLyseeAdoConnection.ExecSQL(const SQL: string): integer;
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

function TLyseeAdoConnection.GetConnectionString: string;
begin
  Result := FADODB.ConnectionString;
end;

function TLyseeAdoConnection.GetCursorLocation: TCursorLocation;
begin
  Result := FADODB.CursorLocation;
end;

procedure TLyseeAdoConnection.GetProcedureNames(List: TStrings);
begin
  FADODB.GetProcedureNames(List);
end;

procedure TLyseeAdoConnection.GetFieldNames(List: TStrings; const Table: string);
begin
  FADODB.GetFieldNames(Table, List);
end;

function TLyseeAdoConnection.GetMode: TConnectMode;
begin
  Result := FADODB.Mode;
end;

procedure TLyseeAdoConnection.GetTableNames(List: TStrings; SystemTables: boolean);
begin
  FADODB.GetTableNames(List, SystemTables);
end;

procedure TLyseeAdoConnection.Connect(const AConnectionString: string);
begin
  Close;
  FADODB.ConnectionString := AConnectionString;
  Open;
end;

procedure TLyseeAdoConnection.Connect(const AConnectionString: string;
  const Tags, Values: array of string);
begin
  Connect(ReplaceTags(AConnectionString, Tags, Values));
end;

function TLyseeAdoConnection.OpenSQL(const SQL: string): TLyseeAdoQuery;
begin
  Result := NewQuery;
  try
    Result.OpenSQL(SQL);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TLyseeAdoConnection.InTransaction: boolean;
begin
  Result := FADODB.InTransaction;
end;

function TLyseeAdoConnection.NewQuery: TLyseeAdoQuery;
var
  Q: TADOQuery;
begin
  Q := TADOQuery.Create(nil);
  Q.Connection := FADODB;
  Result := TLyseeAdoQuery.Create(Q, Self);
  Result.FQuery := Q;
end;

procedure TLyseeAdoConnection.Transact;
begin
  FADODB.BeginTrans;
end;

procedure TLyseeAdoConnection.Commit;
begin
  FADODB.CommitTrans;
end;

procedure TLyseeAdoConnection.Rollback;
begin
  FADODB.RollbackTrans;
end;

procedure TLyseeAdoConnection.SetConnectionString(const Value: string);
begin
  FADODB.ConnectionString := Value;
end;

procedure TLyseeAdoConnection.SetCursorLocation(Value: TCursorLocation);
begin
  FADODB.CursorLocation := Value;
end;

procedure TLyseeAdoConnection.SetMode(Value: TConnectMode);
begin
  FADODB.Mode := Value;
end;

{ TLyseeAdoConnectionType }

procedure TLyseeAdoConnectionType.MyConnectAccess(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
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

procedure TLyseeAdoConnectionType.MyConnectDBase(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
  begin
    C.FADODB.CursorLocation := clUseServer;
    C.Connect(CS_DBASE, ['dir'], [Param[1].GetFileName]);
  end;
end;

procedure TLyseeAdoConnectionType.MyConnectExcelForRead(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    C.Connect(CS_EXCEL_R, ['fname'], [Param[1].GetFileName]);
end;

procedure TLyseeAdoConnectionType.MyConnectExcelForReadWrite(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    C.Connect(CS_EXCEL_W, ['fname'], [Param[1].GetFileName]);
end;

procedure TLyseeAdoConnectionType.MyConnectOracle(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
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

procedure TLyseeAdoConnectionType.MyConnectParadox(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
  begin
    C.FADODB.CursorLocation := clUseServer;
    C.Connect(CS_PARADOX, ['dir'], [Param[1].GetFileName]);
  end;
end;

procedure TLyseeAdoConnectionType.MyConnectSQLServer(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
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

procedure TLyseeAdoConnectionType.MyConnectTextDB(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    C.Connect(CS_TEXT, ['dir'], [Param[1].GetFileName]);
end;

procedure TLyseeAdoConnectionType.MyCreate(const Param: TLyseeParam);
begin
  Param.Result.SetTOA(Self, NewConnection);
end;

procedure TLyseeAdoConnectionType.MyExecSQL(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsInteger := C.ExecSQL(Param[1].AsString);
end;

procedure TLyseeAdoConnectionType.MyGetCommandTimeout(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsInteger := C.FADODB.CommandTimeout;
end;

procedure TLyseeAdoConnectionType.MyGetConnectionString(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsString := C.ConnectionString;
end;

procedure TLyseeAdoConnectionType.MyGetConnectionTimeout(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsInteger := C.FADODB.ConnectionTimeout;
end;

procedure TLyseeAdoConnectionType.MyGetConnectMode(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    my_connectMode.SetValue(Param.Result, Ord(C.FADODB.Mode));
end;

procedure TLyseeAdoConnectionType.MyGetConnectOption(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    my_connectOption.SetValue(Param.Result,
      Ord(C.FADODB.ConnectOptions));
end;

procedure TLyseeAdoConnectionType.MyGetCursorLocation(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    my_cursorLocation.SetValue(Param.Result,
      Ord(C.FADODB.CursorLocation));
end;

procedure TLyseeAdoConnectionType.MyGetDefaultDatabase(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsString := C.FADODB.DefaultDatabase;
end;

procedure TLyseeAdoConnectionType.MyGetIsolationLevel(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    my_isolationLevel.SetValue(Param.Result,
      Ord(C.FADODB.IsolationLevel));
end;

procedure TLyseeAdoConnectionType.MyGetKeepConnection(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsBoolean := C.FADODB.KeepConnection;
end;

procedure TLyseeAdoConnectionType.MyGetProvider(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsString := C.FADODB.Provider;
end;

procedure TLyseeAdoConnectionType.MyOpenEx(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    if Param.Prmc > 1 then
      C.Connect(Param[1].AsString) else
      C.Open;
end;

procedure TLyseeAdoConnectionType.MyOpenSQL(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.SetTOA(my_query, C.OpenSQL(Param[1].AsString));
end;

procedure TLyseeAdoConnectionType.MySetCommandTimeout(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.CommandTimeout := Param[1].AsInteger;
end;

procedure TLyseeAdoConnectionType.MySetConnectionString(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    C.ConnectionString := Param[1].AsString;
end;

procedure TLyseeAdoConnectionType.MySetConnectionTimeout(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.ConnectionTimeout := Param[1].AsInteger;
end;

procedure TLyseeAdoConnectionType.MySetConnectMode(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.Mode := TConnectMode(Param[1].AsInteger);
end;

procedure TLyseeAdoConnectionType.MySetConnectOption(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.ConnectOptions := TConnectOption(Param[1].AsInteger);
end;

procedure TLyseeAdoConnectionType.MySetCursorLocation(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.CursorLocation := TCursorLocation(Param[1].AsInteger);
end;

procedure TLyseeAdoConnectionType.MySetDefaultDatabase(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.DefaultDatabase := Param[1].AsString;
end;

procedure TLyseeAdoConnectionType.MySetIsolationLevel(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.IsolationLevel := TIsolationLevel(Param[1].AsInteger);
end;

procedure TLyseeAdoConnectionType.MySetKeepConnection(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.KeepConnection := Param[1].AsBoolean;
end;

procedure TLyseeAdoConnectionType.MySetProvider(const Param: TLyseeParam);
var
  C: TLyseeAdoConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.Provider := Param[1].AsString;
end;

procedure TLyseeAdoConnectionType.Setup;
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
  Method('OpenSQL', my_query,
         ['SQL'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyOpenSQL);
  Method('ExecSQL', my_int,
         ['SQL'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyExecSQL);
  Define('ConnectionString', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetConnectionString,
         {$IFDEF FPC}@{$ENDIF}MySetConnectionString);
  Define('Mode', my_connectMode,
         {$IFDEF FPC}@{$ENDIF}MyGetConnectMode,
         {$IFDEF FPC}@{$ENDIF}MySetConnectMode);
  Define('ConnectOption', my_connectOption,
         {$IFDEF FPC}@{$ENDIF}MyGetConnectOption,
         {$IFDEF FPC}@{$ENDIF}MySetConnectOption);
  Define('CursorLocation', my_cursorLocation,
         {$IFDEF FPC}@{$ENDIF}MyGetCursorLocation,
         {$IFDEF FPC}@{$ENDIF}MySetCursorLocation);
  Define('CommandTimeout', my_int,
         {$IFDEF FPC}@{$ENDIF}MyGetCommandTimeout,
         {$IFDEF FPC}@{$ENDIF}MySetCommandTimeout);
  Define('ConnectionTimeout', my_int,
         {$IFDEF FPC}@{$ENDIF}MyGetConnectionTimeout,
         {$IFDEF FPC}@{$ENDIF}MySetConnectionTimeout);
  Define('IsolationLevel', my_isolationLevel,
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

{ TLyseeAdodbModule }

constructor TLyseeAdodbModule.Create(const AName: string);
begin
  inherited;
  OnSetup := {$IFDEF FPC}@{$ENDIF}DoSetup;
end;

procedure TLyseeAdodbModule.DoSetup(Sender: TObject);
begin
  OnSetup := nil;
  my_db.Setup;

  CoInitialize(nil);

  Consts.Add('CS_SQLSERVER').AsString := CS_SQLSERVER;
  Consts.Add('CS_ACCESS').AsString := CS_ACCESS;
  Consts.Add('CS_ORACLE').AsString := CS_ORACLE;
  Consts.Add('CS_EXCEL_R').AsString := CS_EXCEL_R;
  Consts.Add('CS_EXCEL_W').AsString := CS_EXCEL_W;
  Consts.Add('CS_EXCELX').AsString := CS_EXCELX;
  Consts.Add('CS_TEXT').AsString := CS_TEXT;
  Consts.Add('CS_DBASE').AsString := CS_DBASE;
  Consts.Add('CS_PARADOX').AsString := CS_PARADOX;

  my_cursorLocation := AddEnumType('TCursorLocation',
    ['clUseServer', 'clUseClient']);

  my_connectOption := AddEnumType('TConnectOption',
    ['coConnectUnspecified', 'coAsyncConnect']);

  my_connectMode := AddEnumType('TConnectMode',
    ['cmUnknown', 'cmRead', 'cmWrite', 'cmReadWrite', 'cmShareDenyRead',
     'cmShareDenyWrite', 'cmShareExclusive', 'cmShareDenyNone']);

  my_isolationLevel := AddEnumType('TIsolationLevel',
    ['ilUnspecified', 'ilChaos', 'ilReadUncommitted', 'ilBrowse',
     'ilCursorStability', 'ilReadCommitted', 'ilRepeatableRead',
     'ilSerializable', 'ilIsolated']);

  my_query := TLyseeAdoQueryType.Create('TADOQuery', Self, my_dataset);
  my_connection := TLyseeAdoConnectionType.Create('TADOConnection', Self, my_database);
  my_query.Setup;
  my_connection.Setup;

  AddFunc('Adb', my_connection, {$IFDEF FPC}@{$ENDIF}pp_adodb_adb);
end;

initialization
begin
  my_adodb := TLyseeAdodbModule.Create('adodb');
  my_defdb := nil;
end;

finalization
begin
  if my_defdb <> nil then
    my_defdb.DecRefcount;
end;

end.

