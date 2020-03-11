{==============================================================================}
{        UNIT: lysee_adodb                                                     }
{ DESCRIPTION: ADO database                                                    }
{   COPYRIGHT: Copyright (c) 2012, Li Yun Jie. All Rights Reserved.            }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2012/05/19                                                      }
{    MODIFIED: 2019/03/02                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_adodb;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Db, adodb, Basic, Lysee, lysee_db;

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

  { TLyAdoQuery }

  TLyAdoQuery = class(TLyDataSet)
  private
    FQuery: TADOQuery;
  public
    function ExecSQL(const SQL: string): integer;
    procedure OpenSQL(const SQL: string);
  end;

  { TLyAdoQueryType }

  TLyAdoQueryType = class(TLyDataSetType)
  private
    procedure MyOpenSQL(const Param: TLyParam);
    procedure MyExecSQL(const Param: TLyParam);
  protected
    procedure Setup;override;
  public
    function InheriteClassType: TLyTypeClass;override;
    procedure Convert(Value: TLyValue);override;
  end;

  { TLyAdoConnection }

  TLyAdoConnection = class(TLyDataBase)
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
    function OpenSQL(const SQL: string): TLyAdoQuery;
    function ExecSQL(const SQL: string): integer;
    function NewQuery: TLyAdoQuery;
    property ConnectionString: string read GetConnectionString write SetConnectionString;
    property CursorLocation: TCursorLocation read GetCursorLocation write SetCursorLocation;
    property Mode: TConnectMode read GetMode write SetMode;
  end;

  { TLyAdoConnectionType }

  TLyAdoConnectionType = class(TLyDataBaseType)
  private
    procedure MyOpenEx(const Param: TLyParam);
    procedure MyConnectSQLServer(const Param: TLyParam);
    procedure MyConnectAccess(const Param: TLyParam);
    procedure MyConnectExcelForRead(const Param: TLyParam);
    procedure MyConnectExcelForReadWrite(const Param: TLyParam);
    procedure MyConnectOracle(const Param: TLyParam);
    procedure MyConnectTextDB(const Param: TLyParam);
    procedure MyConnectDBase(const Param: TLyParam);
    procedure MyConnectParadox(const Param: TLyParam);
    procedure MyOpenSQL(const Param: TLyParam);
    procedure MyExecSQL(const Param: TLyParam);
    procedure MyGetConnectionString(const Param: TLyParam);
    procedure MySetConnectionString(const Param: TLyParam);
    procedure MyGetConnectMode(const Param: TLyParam);
    procedure MySetConnectMode(const Param: TLyParam);
    procedure MyGetConnectOption(const Param: TLyParam);
    procedure MySetConnectOption(const Param: TLyParam);
    procedure MyGetCursorLocation(const Param: TLyParam);
    procedure MySetCursorLocation(const Param: TLyParam);
    procedure MyGetCommandTimeout(const Param: TLyParam);
    procedure MySetCommandTimeout(const Param: TLyParam);
    procedure MyGetConnectionTimeout(const Param: TLyParam);
    procedure MySetConnectionTimeout(const Param: TLyParam);
    procedure MyGetIsolationLevel(const Param: TLyParam);
    procedure MySetIsolationLevel(const Param: TLyParam);
    procedure MyGetKeepConnection(const Param: TLyParam);
    procedure MySetKeepConnection(const Param: TLyParam);
    procedure MyGetProvider(const Param: TLyParam);
    procedure MySetProvider(const Param: TLyParam);
    procedure MyGetDefaultDatabase(const Param: TLyParam);
    procedure MySetDefaultDatabase(const Param: TLyParam);
  protected
    function InstanceClass: TClass;override;
    function CreateInstance: pointer;override;
    procedure Setup;override;
  public
    procedure Convert(Value: TLyValue);override;
  end;

var
  my_connection: TLyAdoConnectionType;
  my_query: TLyAdoQueryType;
  my_cursorLocation: TLyEnumType;
  my_connectOption: TLyEnumType;
  my_connectMode: TLyEnumType;
  my_isolationLevel: TLyEnumType;

procedure Setup;

function NewConnection: TLyAdoConnection;
function DefConnection: TLyAdoConnection;
function SelectStr(const S1, S2: string): string;
function ReplaceTag(const S, Tag, Value: string): string;
function ReplaceTags(const S: string;
  const Tags, Values: array of string): string;

implementation

uses
  ActiveX;

var
  my_adb: TLyAdoConnection;

procedure pp_adodb_adb(const Param: TLyParam);
begin
  Param.Result.Assign(my_connection, DefConnection);
end;

procedure Setup;
begin
  my_system.Consts.Add('CS_SQLSERVER').AsString := CS_SQLSERVER;
  my_system.Consts.Add('CS_ACCESS').AsString := CS_ACCESS;
  my_system.Consts.Add('CS_ORACLE').AsString := CS_ORACLE;
  my_system.Consts.Add('CS_EXCEL_R').AsString := CS_EXCEL_R;
  my_system.Consts.Add('CS_EXCEL_W').AsString := CS_EXCEL_W;
  my_system.Consts.Add('CS_EXCELX').AsString := CS_EXCELX;
  my_system.Consts.Add('CS_TEXT').AsString := CS_TEXT;
  my_system.Consts.Add('CS_DBASE').AsString := CS_DBASE;
  my_system.Consts.Add('CS_PARADOX').AsString := CS_PARADOX;

  my_cursorLocation := my_system.AddEnumType(
    'TCursorLocation',
    ['clUseServer', 'clUseClient']);

  my_connectOption := my_system.AddEnumType(
    'TConnectOption',
    ['coConnectUnspecified', 'coAsyncConnect']);

  my_connectMode := my_system.AddEnumType(
    'TConnectMode',
    ['cmUnknown', 'cmRead', 'cmWrite', 'cmReadWrite', 'cmShareDenyRead',
     'cmShareDenyWrite', 'cmShareExclusive', 'cmShareDenyNone']);

  my_isolationLevel := my_system.AddEnumType(
    'TIsolationLevel',
    ['ilUnspecified', 'ilChaos', 'ilReadUncommitted', 'ilBrowse',
     'ilCursorStability', 'ilReadCommitted', 'ilRepeatableRead',
     'ilSerializable', 'ilIsolated']);

  my_query := TLyAdoQueryType.Create('TADOQuery', my_system);
  my_connection := TLyAdoConnectionType.Create('TADOConnection', my_system);
  my_system.AddFunc('Adb', my_connection, {$IFDEF FPC}@{$ENDIF}pp_adodb_adb);
end;

function NewConnection: TLyAdoConnection;
var
  A: TADOConnection;
begin
  A := TADOConnection.Create(nil);
  try
    Result := TLyAdoConnection.Create(A);
    Result.FADODB := A;
    A.LoginPrompt := false;
  except
    A.Free;
    raise;
  end;
end;

function DefConnection: TLyAdoConnection;
begin
  if my_adb = nil then
  begin
    my_adb := NewConnection;
    my_adb.IncRefcount;
  end;
  Result := my_adb;
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

function ReplaceTags(const S: string;
  const Tags, Values: array of string): string;
var
  I: integer;
begin
  Result := S;
  for I := 0 to Length(Tags) - 1 do
    Result := ReplaceTag(Result, Tags[I], Values[I]);
end;

{ TLyAdoQuery }

procedure TLyAdoQuery.OpenSQL(const SQL: string);
begin
  Close;
  FQuery.SQL.Text := SQL;
  Open;
end;

function TLyAdoQuery.ExecSQL(const SQL: string): integer;
begin
  Close;
  FQuery.SQL.Text := SQL;
  try
    Result := FQuery.ExecSQL;
  finally
    Close;
  end;
end;

{ TLyAdoQueryType }

procedure TLyAdoQueryType.Convert(Value: TLyValue);
begin
  if Value.VType <> Self then
    if Value.VType is TLyAdoQueryType then
      Value.Assign(Self, Value.Data) else
      inherited;
end;

function TLyAdoQueryType.InheriteClassType: TLyTypeClass;
begin
  Result := nil;
end;

procedure TLyAdoQueryType.MyExecSQL(const Param: TLyParam);
var
  Q: TLyAdoQuery;
begin
  if Param.GetSelf(Q) then
    Param.Result.AsInteger := Q.ExecSQL(Param[1].AsString);
end;

procedure TLyAdoQueryType.MyOpenSQL(const Param: TLyParam);
var
  Q: TLyAdoQuery;
begin
  if Param.GetSelf(Q) then
    Q.OpenSQL(Param[1].AsString);
end;

procedure TLyAdoQueryType.Setup;
begin
  Parent := my_dataset;
  inherited;
  Method('OpenSQL', my_nil, ['SQL'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyOpenSQL);
  Method('ExecSQL', my_int, ['SQL'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyExecSQL);
end;

{ TLyAdoConnection }

destructor TLyAdoConnection.Destroy;
begin
  Close;
  FreeAndNil(FQuery);
  inherited Destroy;
end;

function TLyAdoConnection.ExecSQL(const SQL: string): integer;
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

function TLyAdoConnection.GetConnectionString: string;
begin
  Result := FADODB.ConnectionString;
end;

function TLyAdoConnection.GetCursorLocation: TCursorLocation;
begin
  Result := FADODB.CursorLocation;
end;

procedure TLyAdoConnection.GetProcedureNames(List: TStrings);
begin
  FADODB.GetProcedureNames(List);
end;

procedure TLyAdoConnection.GetFieldNames(List: TStrings; const Table: string);
begin
  FADODB.GetFieldNames(Table, List);
end;

function TLyAdoConnection.GetMode: TConnectMode;
begin
  Result := FADODB.Mode;
end;

procedure TLyAdoConnection.GetTableNames(List: TStrings; SystemTables: boolean);
begin
  FADODB.GetTableNames(List, SystemTables);
end;

procedure TLyAdoConnection.Connect(const AConnectionString: string);
begin
  Close;
  FADODB.ConnectionString := AConnectionString;
  Open;
end;

procedure TLyAdoConnection.Connect(const AConnectionString: string;
  const Tags, Values: array of string);
begin
  Connect(ReplaceTags(AConnectionString, Tags, Values));
end;

function TLyAdoConnection.OpenSQL(const SQL: string): TLyAdoQuery;
begin
  Result := NewQuery;
  try
    Result.OpenSQL(SQL);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TLyAdoConnection.InTransaction: boolean;
begin
  Result := FADODB.InTransaction;
end;

function TLyAdoConnection.NewQuery: TLyAdoQuery;
var
  Q: TADOQuery;
begin
  Q := TADOQuery.Create(nil);
  Q.Connection := FADODB;
  Result := TLyAdoQuery.Create(Q, Self);
  Result.FQuery := Q;
end;

procedure TLyAdoConnection.Transact;
begin
  FADODB.BeginTrans;
end;

procedure TLyAdoConnection.Commit;
begin
  FADODB.CommitTrans;
end;

procedure TLyAdoConnection.Rollback;
begin
  FADODB.RollbackTrans;
end;

procedure TLyAdoConnection.SetConnectionString(const Value: string);
begin
  FADODB.ConnectionString := Value;
end;

procedure TLyAdoConnection.SetCursorLocation(Value: TCursorLocation);
begin
  FADODB.CursorLocation := Value;
end;

procedure TLyAdoConnection.SetMode(Value: TConnectMode);
begin
  FADODB.Mode := Value;
end;

{ TLyAdoConnectionType }

procedure TLyAdoConnectionType.Convert(Value: TLyValue);
begin
  if Value.VType <> Self then
    if Value.VType is TLyAdoConnectionType then
      Value.Assign(Self, Value.Data) else
      inherited;
end;

function TLyAdoConnectionType.InstanceClass: TClass;
begin
  Result := TLyAdoConnection;
end;

procedure TLyAdoConnectionType.MyConnectAccess(const Param: TLyParam);
var
  C: TLyAdoConnection;
  S: string;
begin
  if Param.GetSelf(C) then
  begin
    S := ReplaceTag(CS_ACCESS, 'fname', Param[1].AsFileName);
    if Param.Prmc > 2 then
      S := S + ';Jet OLEDB:Database Password=' + Param[2].AsString;
     {S := S + ';Password=' + Param[2].AsString;}
    C.Connect(S);
  end;
end;

procedure TLyAdoConnectionType.MyConnectDBase(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
  begin
    C.FADODB.CursorLocation := clUseServer;
    C.Connect(CS_DBASE, ['dir'], [Param[1].AsFileName]);
  end;
end;

procedure TLyAdoConnectionType.MyConnectExcelForRead(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    C.Connect(CS_EXCEL_R, ['fname'], [Param[1].AsFileName]);
end;

procedure TLyAdoConnectionType.MyConnectExcelForReadWrite(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    C.Connect(CS_EXCEL_W, ['fname'], [Param[1].AsFileName]);
end;

procedure TLyAdoConnectionType.MyConnectOracle(const Param: TLyParam);
var
  C: TLyAdoConnection;
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

procedure TLyAdoConnectionType.MyConnectParadox(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
  begin
    C.FADODB.CursorLocation := clUseServer;
    C.Connect(CS_PARADOX, ['dir'], [Param[1].AsFileName]);
  end;
end;

procedure TLyAdoConnectionType.MyConnectSQLServer(const Param: TLyParam);
var
  C: TLyAdoConnection;
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

procedure TLyAdoConnectionType.MyConnectTextDB(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    C.Connect(CS_TEXT, ['dir'], [Param[1].AsFileName]);
end;

procedure TLyAdoConnectionType.MyExecSQL(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsInteger := C.ExecSQL(Param[1].AsString);
end;

procedure TLyAdoConnectionType.MyGetCommandTimeout(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsInteger := C.FADODB.CommandTimeout;
end;

procedure TLyAdoConnectionType.MyGetConnectionString(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsString := C.ConnectionString;
end;

procedure TLyAdoConnectionType.MyGetConnectionTimeout(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsInteger := C.FADODB.ConnectionTimeout;
end;

procedure TLyAdoConnectionType.MyGetConnectMode(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    my_connectMode.SetValue(Param.Result, Ord(C.FADODB.Mode));
end;

procedure TLyAdoConnectionType.MyGetConnectOption(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    my_connectOption.SetValue(Param.Result,
      Ord(C.FADODB.ConnectOptions));
end;

procedure TLyAdoConnectionType.MyGetCursorLocation(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    my_cursorLocation.SetValue(Param.Result,
      Ord(C.FADODB.CursorLocation));
end;

procedure TLyAdoConnectionType.MyGetDefaultDatabase(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsString := C.FADODB.DefaultDatabase;
end;

procedure TLyAdoConnectionType.MyGetIsolationLevel(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    my_isolationLevel.SetValue(Param.Result,
      Ord(C.FADODB.IsolationLevel));
end;

procedure TLyAdoConnectionType.MyGetKeepConnection(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsBoolean := C.FADODB.KeepConnection;
end;

procedure TLyAdoConnectionType.MyGetProvider(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.AsString := C.FADODB.Provider;
end;

procedure TLyAdoConnectionType.MyOpenEx(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    if Param.Prmc > 1 then
      C.Connect(Param[1].AsString) else
      C.Open;
end;

procedure TLyAdoConnectionType.MyOpenSQL(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    Param.Result.Assign(my_query, C.OpenSQL(Param[1].AsString));
end;

procedure TLyAdoConnectionType.MySetCommandTimeout(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.CommandTimeout := Param[1].AsInteger;
end;

procedure TLyAdoConnectionType.MySetConnectionString(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    C.ConnectionString := Param[1].AsString;
end;

procedure TLyAdoConnectionType.MySetConnectionTimeout(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.ConnectionTimeout := Param[1].AsInteger;
end;

procedure TLyAdoConnectionType.MySetConnectMode(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.Mode := TConnectMode(Param[1].AsInteger);
end;

procedure TLyAdoConnectionType.MySetConnectOption(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.ConnectOptions := TConnectOption(Param[1].AsInteger);
end;

procedure TLyAdoConnectionType.MySetCursorLocation(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.CursorLocation := TCursorLocation(Param[1].AsInteger);
end;

procedure TLyAdoConnectionType.MySetDefaultDatabase(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.DefaultDatabase := Param[1].AsString;
end;

procedure TLyAdoConnectionType.MySetIsolationLevel(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.IsolationLevel := TIsolationLevel(Param[1].AsInteger);
end;

procedure TLyAdoConnectionType.MySetKeepConnection(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.KeepConnection := Param[1].AsBoolean;
end;

procedure TLyAdoConnectionType.MySetProvider(const Param: TLyParam);
var
  C: TLyAdoConnection;
begin
  if Param.GetSelf(C) then
    C.FADODB.Provider := Param[1].AsString;
end;

function TLyAdoConnectionType.CreateInstance: pointer;
begin
  Result := NewConnection;
end;

procedure TLyAdoConnectionType.Setup;
begin
  Parent := my_database;

  Method(LSE_CREATE, Self, {$IFDEF FPC}@{$ENDIF}MyCreate);
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

initialization
begin
  CoInitialize(nil);
end;

finalization
begin
  if my_adb <> nil then
    my_adb.DecRefcount;
end;

end.

