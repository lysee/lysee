{==============================================================================}
{        UNIT: lysee_odbc                                                      }
{ DESCRIPTION: ODBC database                                                   }
{   COPYRIGHT: Copyright (c) 2012, Li Yun Jie. All Rights Reserved.            }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2012/05/19                                                      }
{    MODIFIED: 2012/05/28                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lmodbc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Db, sqldb, odbcconn, lysee;

{@pmc-description accessing ODBC databases}

{@pmc-object database => database}
{@pmc-set database.t_getpv => db_getpv}
{@pmc-set database.t_setpv => db_setpv}

{@pmc-object dataset => dataset}
{@pmc-set dataset.t_length => ds_length}
{@pmc-set dataset.t_getiv => ds_getiv}
{@pmc-set dataset.t_getpv => ds_getpv}
{@pmc-set dataset.t_eachp => ds_eachp}
{@pmc-set dataset.t_otos => ds_otos}

{@pmc-bind varlist => varlist:
  varlist(LyObject(Param^.p_param[%d])):
  LyReturn(Param, my_varlist, fres)
}

type

  varlist = pointer;
  database = class;

  { dataset }

  dataset = class(TLyObject)
  private
    FDB: database;
    FQuery: TSQLQuery;
    function ToStr: string;
  public
    constructor Create(ADB: database);
    destructor Destroy;override;
    function OpenSQL(const SQL: string): integer;
    procedure Close;
    function Active: boolean;
    function RowCount: integer;
    function FieldCount: integer;
    function FieldName(Index: integer): string;
    function FieldNames(const Param: PLyParam): varlist;
    function FieldType(Index: integer): PLyType;
    function IndexOf(const Field: string): integer;
    function Bof: boolean;
    function Eof: boolean;
    procedure First;
    procedure Last;
    procedure Prior;
    procedure Next;
  end;

  { database }

  database = class(TLyObject)
  private
    FConn: TODBCConnection;
    FTran: TSQLTransaction;
    FQuery: TSQLQuery;
    FConnectedCount: integer;
  public
    constructor Create;
    destructor Destroy;override;
    procedure ConnecTo(const DSN, UserName, Password: string);
    procedure ConnecToFile(const FileDSN, UserName, Password: string);
    procedure Close;
    procedure Reopen;
    function Connected: boolean;
    function ExecSQL(const SQL: string): integer;
    function OpenSQL(const SQL: string): dataset;
    procedure Transact;
    function Transacting: boolean;
    procedure Commit;
    procedure CommitRetaining;
    procedure CommitAndTransact;
    procedure Rollback;
    procedure RollbackRetaining;
    procedure RollbackAndTransact;
    function Tables(SystemTables: boolean; const Param: PLyParam): varlist;
    function Procedures(const Param: PLyParam): varlist;
  end;

{@pmc-end}

function db_getpv(obj: pointer; const prop: PLyString; value: PLyValue): integer;
function db_setpv(obj: pointer; const prop: PLyString; value: PLyValue): integer;

function ds_length(obj: pointer): integer;
function ds_getiv(obj: pointer; index: integer; value: PLyValue): integer;
function ds_getpv(obj: pointer; const prop: PLyString; value: PLyValue): integer;
function ds_eachp(obj: pointer; EachProc: TLyEnumProp; data: pointer): integer;
function ds_otos(obj: pointer): PLyString;

function  getft(AType: TFieldType): TLyValue;
function  getfs(F: TField): string;
procedure setfv(F: TField; V: PLyValue);

implementation

function getft(AType: TFieldType): TLyValue;
begin
  case AType of
    ftString       : Result := lSV_STRING;
    ftSmallint     : Result := LSV_INT;
    ftInteger      : Result := LSV_INT;
    ftWord         : Result := LSV_INT;
    ftBoolean      : Result := LSV_INT;
    ftFloat        : Result := LSV_FLOAT;
    ftCurrency     : Result := LSV_FLOAT;
    ftBCD          : Result := LSV_FLOAT;
    ftDate         : Result := LSV_FLOAT;
    ftTime         : Result := LSV_FLOAT;
    ftDateTime     : Result := LSV_FLOAT;
    ftAutoInc      : Result := LSV_INT;
    ftMemo         : Result := lSV_STRING;
    ftFmtMemo      : Result := lSV_STRING;
    ftFixedChar    : Result := lSV_STRING;
    ftWideString   : Result := lSV_STRING;
    ftLargeint     : Result := LSV_INT;
    ftGuid         : Result := lSV_STRING;
    ftFMTBcd       : Result := LSV_FLOAT;
    ftFixedWideChar: Result := lSV_STRING;
    ftWideMemo     : Result := lSV_STRING;
                else Result := LSV_VOID;
  end;
  { ftUnknown, ftBytes, ftVarBytes, ftBlob, ftGraphic,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor,
    ftADT, ftArray, ftReference, ftDataSet, ftOraBlob,
    ftOraClob, ftVariant, ftInterface, ftIDispatch,
    ftTimeStamp}
end;

function getfs(F: TField): string;
const
  BOOLEAN_STR: array[boolean] of string = ('0', '1');
begin
  try
    if F.IsNull then Result := '' else
    if F.DataType = ftBoolean then
      Result := BOOLEAN_STR[F.AsBoolean] else
      Result := F.AsString;
  except
    Result := '';
  end;
end;

procedure setfv(F: TField; V: PLyValue);
begin
  if F.IsNull then LyClearValue(V) else
  case getft(F.DataType) of
    LSV_STRING: LySetString(V, F.AsString);
    LSV_INT: LySetInt(V, F.AsInteger);
    LSV_FLOAT: LySetFloat(V, F.AsFloat);
    else LyClearValue(V);
  end;
end;

function db_getpv(obj: pointer; const prop: PLyString; value: PLyValue): integer;
var
  K: string;
  B: database;
begin
  Result := 0;
  if obj <> nil then
  begin
    B := database(obj);
    K := LowerCase(LyStrecString(prop));
    if K = 'username' then
      LySetString(value, B.FConn.UserName) else
    if K = 'password' then
      LySetString(value, B.FConn.Password) else
    if K = 'dsn' then
      LySetString(value, B.FConn.DatabaseName) else
    if K = 'filedsn' then
      LySetString(value, B.FConn.FileDSN) else
      Exit;
    Result := 1;
  end;
end;

function db_setpv(obj: pointer; const prop: PLyString; value: PLyValue): integer;
var
  K, S: string;
  B: database;
begin
  Result := 0;
  if (obj <> nil) and (value^.vtype = my_string) then
  begin
    B := database(obj);
    K := LowerCase(LyStrecString(prop));
    S := LyString(value);
    if K = 'username' then
      B.FConn.UserName := S else
    if K = 'password' then
      B.FConn.Password := S else
    if K = 'dsn' then
    begin
      B.FConn.FileDSN := '';
      B.FConn.DatabaseName := S;
    end
    else
    if K = 'filedsn' then
    begin
      B.FConn.DatabaseName := '';
      B.FConn.FileDSN := S;
    end
    else Exit;
    Result := 1;
  end;
end;

function ds_length(obj: pointer): integer;
var
  Q: TSQLQuery;
begin
  Result := 0;
  if obj <> nil then
  begin
    Q := dataset(obj).FQuery;
    if Q.Active then
      Result := Q.FieldCount;
  end;
end;

function ds_getiv(obj: pointer; index: integer; value: PLyValue): integer;
var
  Q: TSQLQuery;
begin
  Result := 0;
  if obj <> nil then
  begin
    Q := dataset(obj).FQuery;
    if Q.Active then
    begin
      index := LyResetIndex(index, Q.FieldCount);
      if (index >= 0) and (index < Q.FieldCount) then
      begin
        setfv(Q.Fields[index], value);
        Result := 1;
      end;
    end;
  end;
end;

function ds_getpv(obj: pointer; const prop: PLyString; value: PLyValue): integer;
var
  Q: TSQLQuery;
  F: TField;
begin
  Result := 0;
  if obj <> nil then
  begin
    Q := dataset(obj).FQuery;
    if Q.Active then
    begin
      F := Q.FindField(LyStrecString(prop));
      if F <> nil then
      begin
        setfv(F, value);
        Result := 1;
      end;
    end;
  end;
end;

function ds_eachp(obj: pointer; EachProc: TLyEnumProp; data: pointer): integer;
var
  Q: TSQLQuery;
  X: integer;
  S: PLyString;
begin
  Result := 0;
  if (obj <> nil) and Assigned(EachProc) then
  try
    Q := dataset(obj).FQuery;
    if Q.Active then
      for X := 0 to Q.FieldCount - 1 do
      begin
        S := LyStrecAlloc(Q.Fields[X].FieldName);
        LyStrecAddref(S);
        EachProc(S, data);
        Inc(Result);
        LyStrecRelease(S);
      end;
  except
    { do nothing }
  end;
end;

function ds_otos(obj: pointer): PLyString;
begin
  if obj = nil then Result := nil else
  try
    Result := LyStrecAlloc(dataset(obj).ToStr);
  except
    Result := nil;
  end;
end;

{ dataset }

constructor dataset.Create(ADB: database);
begin
  FDB := ADB;
  FQuery := TSQLQuery.Create(nil);
  FQuery.DataBase := FDB.FConn;
end;

destructor dataset.Destroy;
begin
  FreeAndNil(FQuery);
  inherited Destroy;
end;

function dataset.OpenSQL(const SQL: string): integer;
begin
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.Open;
  Result := FQuery.RecordCount;
end;

procedure dataset.Close;
begin
  FQuery.Close;
end;

function dataset.Active: boolean;
begin
  Result := FQuery.Active;
end;

function dataset.RowCount: integer;
begin
  if FQuery.Active then
    Result := FQuery.RecordCount else
    Result := 0;
end;

function dataset.FieldCount: integer;
begin
  if FQuery.Active then
    Result := FQuery.FieldCount else
    Result := 0;
end;

function dataset.FieldName(Index: integer): string;
begin
  Result := FQuery.Fields[Index].FieldName;
end;

function dataset.FieldNames(const Param: PLyParam): varlist;
var
  X: integer;
  V: RLyValue;
begin
  with my_entry^ do
    Result := cik_varlist_create(LyEngine(Param));
  try
    if FQuery.Active then
    begin
      LyInitValue(@V);
      for X := 0 to FQuery.FieldCount - 1 do
      begin
        LySetString(@V, FQuery.Fields[X].FieldName);
        my_varlist^.t_add(Result, @V);
      end;
      LyClearValue(@V);
    end;
  except
    { do nothing }
  end;
end;

function dataset.FieldType(Index: integer): PLyType;
begin
  case getft(FQuery.Fields[Index].DataType) of
    LSV_STRING: Result := my_string;
    LSV_INT: Result := my_int;
    LSV_FLOAT: Result := my_float;
    else Result := my_void;
  end;
end;

function dataset.IndexOf(const Field: string): integer;
var
  X: integer;
begin
  if FQuery.Active then
    for X := 0 to FQuery.FieldCount - 1 do
      if AnsiSameText(Field, FQuery.Fields[X].FieldName) then
      begin
        Result := X;
        Exit;
      end;
  Result := -1;
end;

function dataset.Bof: boolean;
begin
  Result := FQuery.BOF;
end;

function dataset.Eof: boolean;
begin
  Result := FQuery.EOF;
end;

procedure dataset.First;
begin
  FQuery.First;
end;

procedure dataset.Last;
begin
  FQuery.Last;
end;

procedure dataset.Prior;
begin
  FQuery.Prior;
end;

procedure dataset.Next;
begin
  FQuery.Next;
end;

function dataset.ToStr: string;
var
  B: TBookMark;
  M: TStringStream;
  X, N: integer;
begin
  Result := '';
  if not FQuery.Active then Exit;
  B := FQuery.GetBookmark;
  try
    M := TStringStream.Create('');
    try
      N := FQuery.FieldCount;

      for X := 0 to N - 1 do
      begin
        if X > 0 then M.WriteString(',');
        M.WriteString(FQuery.Fields[X].FieldName);
      end;
      M.WriteString(sLineBreak);

      FQuery.First;
      while not FQuery.EOF do
      begin
        for X := 0 to N - 1 do
        begin
          if X > 0 then M.WriteString(',');
          M.WriteString(getfs(FQuery.Fields[X]));
        end;
        M.WriteString(sLineBreak);
        FQuery.Next;
      end;

      Result := M.DataString;
    finally
      M.Free;
    end;
  finally
    FQuery.GotoBookmark(B);
    FQuery.FreeBookmark(B);
  end;
end;

{ database }

constructor database.Create;
begin
  FTran := TSQLTransaction.Create(nil);
  FConn := TODBCConnection.Create(nil);
  FConn.Transaction := FTran;
end;

destructor database.Destroy;
begin
  FConn.Close;
  FreeAndNil(FQuery);
  FreeAndNil(FTran);
  FreeAndNil(FConn);
  inherited Destroy;
end;

function database.ExecSQL(const SQL: string): integer;
begin
  if FQuery = nil then
  begin
    FQuery := TSQLQuery.Create(nil);
    FQuery.DataBase := FConn;
    FQuery.UsePrimaryKeyAsKey := false;
  end;
  FQuery.SQL.Text := SQL;
  try
    FQuery.ExecSQL;
    Result := FQuery.RowsAffected;
  finally
    FQuery.Close;
  end;
end;

function database.OpenSQL(const SQL: string): dataset;
begin
  Result := dataset.Create(Self);
  try
    Result.OpenSQL(SQL);
  except
    Result.Free;
    raise;
  end;
end;

procedure database.ConnecTo(const DSN, UserName, Password: string);
begin
  Close;
  Inc(FConnectedCount);
  FConn.FileDSN := '';
  FConn.DatabaseName := DSN;
  FConn.UserName := UserName;
  FConn.Password := Password;
  FConn.LoginPrompt := false;
  FConn.Open;
end;

procedure database.ConnecToFile(const FileDSN, UserName, Password: string);
begin
  Close;
  Inc(FConnectedCount);
  FConn.DatabaseName := '';
  FConn.FileDSN := FileDSN;
  FConn.UserName := UserName;
  FConn.Password := Password;
  FConn.LoginPrompt := false;
  FConn.Open;
end;

procedure database.Close;
begin
  if FConn.Connected then FConn.Close;
end;

procedure database.Reopen;
begin
  if not FConn.Connected then FConn.Open;
end;

function database.Connected: boolean;
begin
  Result := FConn.Connected;
end;

function database.Transacting: boolean;
begin
  Result := FTran.Active;
end;

procedure database.Transact;
begin
  FConn.StartTransaction;
end;

procedure database.Commit;
begin
  FTran.Commit;
end;

procedure database.CommitRetaining;
begin
  FTran.CommitRetaining;
end;

procedure database.CommitAndTransact;
begin
  CommitRetaining;
  Transact;
end;

procedure database.Rollback;
begin
  FTran.Rollback;
end;

procedure database.RollbackRetaining;
begin
  FTran.RollbackRetaining;
end;

procedure database.RollbackAndTransact;
begin
  RollbackRetaining;
  Transact;
end;

function database.Tables(SystemTables: boolean; const Param: PLyParam): varlist;
var
  L: TStringList;
  X: integer;
  V: RLyValue;
begin
  with my_entry^ do
    Result := cik_varlist_create(LyEngine(Param));
  try
    if Connected then
    begin
      LyInitValue(@V);
      L := TStringList.Create;
      try
        FConn.GetTableNames(L, SystemTables);
        L.Sort;
        for X := 0 to L.Count - 1 do
        begin
          LySetString(@V, L[X]);
          my_varlist^.t_add(Result, @V);
        end;
      finally
        L.Free;
        LyClearValue(@V);
      end;
    end;
  except
    { do nothing }
  end;
end;

function database.Procedures(const Param: PLyParam): varlist;
var
  L: TStringList;
  X: integer;
  V: RLyValue;
begin
  with my_entry^ do
    Result := cik_varlist_create(LyEngine(Param));
  try
    if Connected then
    begin
      LyInitValue(@V);
      L := TStringList.Create;
      try
        FConn.GetProcedureNames(L);
        L.Sort;
        for X := 0 to L.Count - 1 do
        begin
          LySetString(@V, L[X]);
          my_varlist^.t_add(Result, @V);
        end;
      finally
        L.Free;
        LyClearValue(@V);
      end;
    end;
  except
    { do nothing }
  end;
end;

end.

