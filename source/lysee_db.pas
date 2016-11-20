{==============================================================================}
{        UNIT: lysee_db                                                        }
{ DESCRIPTION: ADO database                                                    }
{   COPYRIGHT: Copyright (c) 2012, Li Yun Jie. All Rights Reserved.            }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2012/05/19                                                      }
{    MODIFIED: 2016/11/19                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_db;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Db, lysee, basic;

type

  TLiDataBase = class;{forward}
  TLiDataSet  = class;

  { TLiField }

  TLiField = class(TLiObject)
  private
    FField: TField;
    function GetIndex: integer;
    function GetAsBoolean: boolean;
    function GetAsCurrency: currency;
    function GetAsFloat: double;
    function GetAsInteger: int64;
    function GetAsTime: TDateTime;
    function GetAsChar: char;
    function GetName: string;
    function GetType: TLiType;
  public
    constructor Create(AField: TField);
    procedure SaveValueTo(Value: TLiValue);
    function IsNull: boolean;
    function AsString: string;override;
    property Field: TField read FField;
    property FieldName: string read GetName;
    property FieldType: TLiType read GetType;
    property FieldIndex: integer read GetIndex;
    property AsChar: char read GetAsChar;
    property AsInteger: int64 read GetAsInteger;
    property AsFloat: double read GetAsFloat;
    property AsCurrency: currency read GetAsCurrency;
    property AsTime: TDateTime read GetAsTime;
    property AsBoolean: boolean read GetAsBoolean;
  end;

  { TLiDataSet }

  TLiDataSet = class(TLiObject)
  private
    FDataBase: TLiDataBase;
    FDataSet: TDataSet;
    FFields: TList;
    function GetFieldCount: integer;
    function GetField(Index: integer): TLiField;
    function GetActive: boolean;
    procedure SetActive(Value: boolean);
    function GetEof: boolean;
    function GetBof: boolean;
    function GetRecordCount: integer;
  protected
    procedure SetupFields;
    procedure ClearFields;
    procedure DeleteField(Index: integer);
    procedure OnClose(Sender: TObject);
  public
    constructor Create(ADataSet: TDataSet; ADataBase: TLiDataBase);virtual;
    destructor Destroy;override;
    procedure Close;
    procedure Open;
    procedure First;
    procedure Last;
    procedure Prior;
    procedure Next;
    function IndexOf(const Name: string): integer;
    function FindField(const Name: string): TLiField;
    function FieldByName(const Name: string): TLiField;
    function AsString: string;override;
    property DataSet: TDataSet read FDataSet;
    property Active: boolean read GetActive write SetActive;
    property RecordCount: integer read GetRecordCount;
    property FieldCount: integer read GetFieldCount;
    property Fields[Index: integer]: TLiField read GetField;default;
    property Bof: boolean read GetBof;
    property Eof: boolean read GetEof;
  end;

  { TLiDataBase }

  TLiDataBase = class(TLiObject)
  private
    FConnection: TCustomConnection;
    FDataSets: TList;
    function GetDataSetCount: integer;
    function GetDataSet(Index: integer): TLiDataSet;
    function GetConnected: boolean;
    procedure SetConnected(Value: boolean);
    function GetLoginPrompt: boolean;
    procedure SetLoginPrompt(Value: boolean);
  protected
    procedure CloseDataSets;
    procedure ClearDataSets;
    procedure DeleteDataSet(Index: integer);
    procedure Disconnected(Sender: TObject);
  public
    constructor Create(AConnection: TCustomConnection);virtual;
    destructor Destroy;override;
    procedure Open;virtual;
    procedure Close;virtual;
    function InTransaction: boolean;virtual;
    procedure Transact;virtual;
    procedure Commit;virtual;
    procedure Rollback;virtual;
    procedure CommitRetaining;
    procedure CommitAndTransact;
    procedure RollbackRetaining;
    procedure RollbackAndTransact;
    procedure GetTableNames(List: TStrings; SystemTables: boolean);virtual;
    procedure GetFieldNames(List: TStrings; const Table: string);virtual;
    procedure GetProcedureNames(List: TStrings);virtual;
    property Connection: TCustomConnection read FConnection;
    property Connected: boolean read GetConnected write SetConnected;
    property DataSetCount: integer read GetDataSetCount;
    property DataSets[Index: integer]: TLiDataSet read GetDataSet;default;
    property LoginPrompt: boolean read GetLoginPrompt write SetLoginPrompt;
  end;

  { TLiType_TField }

  TLiType_TField = class(TLiType)
  protected
    function _IncRefcount(Obj: pointer): integer;override;
    function _DecRefcount(Obj: pointer): integer;override;
    function _AsString(Obj: pointer): string;override;
    function _AsChar(Obj: pointer): char;override;
    function _AsInteger(Obj: pointer): int64;override;
    function _AsFloat(Obj: pointer): double;override;
    function _AsCurrency(Obj: pointer): currency;override;
    function _AsTime(Obj: pointer): TDateTime;override;
    function _AsBoolean(Obj: pointer): boolean;override;
    procedure _Validate(Obj: pointer);override;
  end;

  { TLiType_TDataSet }

  TLiType_TDataSet = class(TLiType)
  protected
    function _IncRefcount(Obj: pointer): integer;override;
    function _DecRefcount(Obj: pointer): integer;override;
    function _AsString(Obj: pointer): string;override;
    function _AsBoolean(Obj: pointer): boolean;override;
    function _Length(Obj: pointer): int64;override;
    procedure _Validate(Obj: pointer);override;
  end;

  { TLiType_TDataBase }

  TLiType_TDataBase = class(TLiType)
  protected
    function _IncRefcount(Obj: pointer): integer;override;
    function _DecRefcount(Obj: pointer): integer;override;
    function _AsString(Obj: pointer): string;override;
    procedure _Validate(Obj: pointer);override;
  end;

var

  my_db: TLiModule;
  my_field: TLiType_TField;
  my_dataset: TLiType_TDataSet;
  my_database: TLiType_TDataBase;

function FieldTID(AType: TFieldType): integer;overload;
function FieldTID(AField: TField): integer;overload;
function FieldToType(AType: TFieldType): TLiType;overload;
function FieldToType(AField: TField): TLiType;overload;
function FieldToStr(AField: TField): string;
function FieldToChar(AField: TField): char;
function FieldToInt(AField: TField): int64;
function FieldToBool(AField: TField): boolean;
function FieldToFloat(AField: TField): double;
function FieldToCurr(AField: TField): currency;
function FieldToTime(AField: TField): TDateTime;
procedure GetFieldValue(AField: TField; Value: TLiValue);

implementation

uses
  Types, Math;

function FieldTID(AType: TFieldType): integer;
begin
  Result := TID_NIL;
  case AType of
    ftString       : Result := TID_STRING;
    ftSmallint     : Result := TID_INTEGER;
    ftInteger      : Result := TID_INTEGER;
    ftWord         : Result := TID_INTEGER;
    ftBoolean      : Result := TID_BOOLEAN;
    ftFloat        : Result := TID_FLOAT;
    ftCurrency     : Result := TID_CURRENCY;
    ftBCD          : Result := TID_FLOAT;
    ftDate         : Result := TID_TIME;
    ftTime         : Result := TID_TIME;
    ftDateTime     : Result := TID_TIME;
    ftAutoInc      : Result := TID_INTEGER;
    ftMemo         : Result := TID_STRING;
    ftFmtMemo      : Result := TID_STRING;
    ftFixedChar    : Result := TID_STRING;
    ftWideString   : Result := TID_STRING;
    ftLargeint     : Result := TID_INTEGER;
    ftGuid         : Result := TID_STRING;
    ftFMTBcd       : Result := TID_FLOAT;
    ftFixedWideChar: Result := TID_STRING;
    ftWideMemo     : Result := TID_STRING;
  end;
  { ftUnknown, ftBytes, ftVarBytes, ftBlob, ftGraphic,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor,
    ftADT, ftArray, ftReference, ftDataSet, ftOraBlob,
    ftOraClob, ftVariant, ftInterface, ftIDispatch,
    ftTimeStamp }
end;

function FieldTID(AField: TField): integer;
begin
  Result := FieldTID(AField.DataType);
end;

function FieldToType(AType: TFieldType): TLiType;
begin
  Result := my_nil;
  case FieldTID(AType) of
    TID_STRING  : Result := my_string;
    TID_INTEGER : Result := my_int;
    TID_FLOAT   : Result := my_float;
    TID_CURRENCY: Result := my_curr;
    TID_BOOLEAN : Result := my_bool;
    TID_TIME    : Result := my_time;
  end;
end;

function FieldToType(AField: TField): TLiType;
begin
  Result := FieldToType(AField.DataType);
end;

function FieldToStr(AField: TField): string;
begin
  if AField.IsNull then Result := '' else
  case FieldTID(AField.DataType) of
    TID_STRING  : Result := AField.AsString;
    TID_INTEGER : Result := IntToStr(AField.AsInteger);
    TID_FLOAT   : Result := FloatToStr(AField.AsFloat);
    TID_CURRENCY: Result := CurrToStr(AField.AsCurrency);
    TID_BOOLEAN : Result := IntToStr(Ord(AField.AsBoolean));
    TID_TIME    : Result := DateTimeToStr(AField.AsDateTime);
    else Result := AField.AsString;
  end;
end;

function FieldToChar(AField: TField): char;
var
  T: TLiType;
  S: string;
begin
  Result := #0;
  if not AField.IsNull then
  begin
    T := FieldToType(AField);
    if T = my_string then
    begin
      S := AField.AsString;
      if S <> '' then Result := S[1];
    end
    else
    if T = my_int then
      Result := char(AField.AsInteger) else
      ErrorConvert(T, my_char);
  end;
end;

function FieldToInt(AField: TField): int64;
begin
  if AField.IsNull then Result := 0 else
  case FieldTID(AField.DataType) of
    TID_STRING  : Result := StrToInt64(AField.AsString);
    TID_INTEGER : Result := AField.AsInteger;
    TID_FLOAT   : Result := Trunc(AField.AsFloat);
    TID_CURRENCY: Result := Trunc(AField.AsCurrency);
    TID_BOOLEAN : Result := Ord(AField.AsBoolean);
    TID_TIME    : Result := Trunc(AField.AsDateTime);
    else Result := AField.AsInteger;
  end;
end;

function FieldToBool(AField: TField): boolean;
begin
  if AField.IsNull then Result := false else
  case FieldTID(AField.DataType) of
    TID_STRING  : Result := (AField.AsString <> '');
    TID_INTEGER : Result := (AField.AsInteger <> 0);
    TID_FLOAT   : Result := not IsZero(AField.AsFloat);
    TID_CURRENCY: Result := (AField.AsCurrency <> 0);
    TID_BOOLEAN : Result := AField.AsBoolean;
    TID_TIME    : Result := not IsZero(AField.AsDateTime);
    else Result := AField.AsBoolean;
  end;
end;

function FieldToFloat(AField: TField): double;
begin
  if AField.IsNull then Result := 0 else
  case FieldTID(AField.DataType) of
    TID_STRING  : Result := StrToFloat(AField.AsString);
    TID_INTEGER : Result := AField.AsInteger;
    TID_FLOAT   : Result := AField.AsFloat;
    TID_CURRENCY: Result := AField.AsCurrency;
    TID_BOOLEAN : Result := AField.AsFloat;
    TID_TIME    : Result := AField.AsDateTime;
    else Result := AField.AsFloat;
  end;
end;

function FieldToCurr(AField: TField): currency;
begin
  if AField.IsNull then Result := 0 else
  case FieldTID(AField.DataType) of
    TID_STRING  : Result := StrToCurr(AField.AsString);
    TID_INTEGER : Result := AField.AsInteger;
    TID_FLOAT   : Result := AField.AsFloat;
    TID_CURRENCY: Result := AField.AsCurrency;
    TID_BOOLEAN : Result := AField.AsCurrency;
    TID_TIME    : Result := AField.AsCurrency;
    else Result := AField.AsCurrency;
  end;
end;

function FieldToTime(AField: TField): TDateTime;
begin
  if AField.IsNull then Result := 0 else
  case FieldTID(AField.DataType) of
    TID_STRING  : Result := StrToDateTime(AField.AsString);
    TID_INTEGER : Result := AField.AsInteger;
    TID_FLOAT   : Result := AField.AsFloat;
    TID_CURRENCY: Result := AField.AsCurrency;
    TID_BOOLEAN : Result := AField.AsDateTime;
    TID_TIME    : Result := AField.AsDateTime;
    else Result := AField.AsDateTime;
  end;
end;

procedure GetFieldValue(AField: TField; Value: TLiValue);
begin
  if AField.IsNull then Value.SetNil else
  case FieldTID(AField.DataType) of
    TID_STRING  : Value.AsString := AField.AsString;
    TID_INTEGER : Value.AsInteger := AField.AsInteger;
    TID_FLOAT   : Value.AsFloat := AField.AsFloat;
    TID_CURRENCY: Value.AsCurrency := AField.AsCurrency;
    TID_BOOLEAN : Value.AsBoolean := AField.AsBoolean;
    TID_TIME    : Value.AsTime := AField.AsDateTime;
    else Value.SetNil;
  end;
end;

//-- my_field ------------------------------------------------------------------

procedure pp_field_name(const Param: TLiParam);
var
  F: TLiField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsString := F.FieldName;
end;

procedure pp_field_type(const Param: TLiParam);
var
  F: TLiField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsType := F.FieldType;
end;

procedure pp_field_index(const Param: TLiParam);
var
  F: TLiField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsInteger := F.FieldIndex;
end;

procedure pp_field_value(const Param: TLiParam);
var
  F: TLiField;
begin
  if Param.GetSelf(F) then
    GetFieldValue(F.FField, Param.Result);
end;

procedure pp_field_isNull(const Param: TLiParam);
var
  F: TLiField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsBoolean := F.IsNull;
end;

procedure pp_field_string(const Param: TLiParam);
var
  F: TLiField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsString := F.AsString;
end;

procedure pp_field_char(const Param: TLiParam);
var
  F: TLiField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsChar := F.AsChar;
end;

procedure pp_field_int(const Param: TLiParam);
var
  F: TLiField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsInteger := F.AsInteger;
end;

procedure pp_field_float(const Param: TLiParam);
var
  F: TLiField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsFloat := F.AsFloat;
end;

procedure pp_field_curr(const Param: TLiParam);
var
  F: TLiField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsCurrency := F.AsCurrency;
end;

procedure pp_field_bool(const Param: TLiParam);
var
  F: TLiField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsBoolean := F.AsBoolean;
end;

procedure pp_field_time(const Param: TLiParam);
var
  F: TLiField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsTime := F.AsTime;
end;

//-- my_dataset ----------------------------------------------------------------

procedure pp_dataset_open(const Param: TLiParam);
var
  S: TLiDataSet;
begin
  if Param.GetSelf(S) then
    S.Open;
end;

procedure pp_dataset_close(const Param: TLiParam);
var
  S: TLiDataSet;
begin
  if Param.GetSelf(S) then
    S.Close;
end;

procedure pp_dataset_first(const Param: TLiParam);
var
  S: TLiDataSet;
begin
  if Param.GetSelf(S) then
    S.First;
end;

procedure pp_dataset_last(const Param: TLiParam);
var
  S: TLiDataSet;
begin
  if Param.GetSelf(S) then
    S.Last;
end;

procedure pp_dataset_prior(const Param: TLiParam);
var
  S: TLiDataSet;
begin
  if Param.GetSelf(S) then
    S.Prior;
end;

procedure pp_dataset_next(const Param: TLiParam);
var
  S: TLiDataSet;
begin
  if Param.GetSelf(S) then
    S.Next;
end;

procedure pp_dataset_bof(const Param: TLiParam);
var
  S: TLiDataSet;
begin
  if Param.GetSelf(S) then
    Param.Result.AsBoolean := S.Bof;
end;

procedure pp_dataset_eof(const Param: TLiParam);
var
  S: TLiDataSet;
begin
  if Param.GetSelf(S) then
    Param.Result.AsBoolean := S.Eof;
end;

procedure pp_dataset_findField(const Param: TLiParam);
var
  S: TLiDataSet;
  F: TLiField;
begin
  if Param.GetSelf(S) then
  begin
    F := S.FindField(Param[1].AsString);
    Param.Result.SetTOA(my_field, F);
  end;
end;

procedure pp_dataset_fieldByName(const Param: TLiParam);
var
  S: TLiDataSet;
  F: TLiField;
begin
  if Param.GetSelf(S) then
  begin
    F := S.FieldByName(Param[1].AsString);
    Param.Result.SetTOA(my_field, F);
  end;
end;

procedure pp_dataset_fieldCount(const Param: TLiParam);
var
  S: TLiDataSet;
begin
  if Param.GetSelf(S) then
    Param.Result.AsInteger := S.FieldCount;
end;

procedure pp_dataset_recordCount(const Param: TLiParam);
var
  S: TLiDataSet;
begin
  if Param.GetSelf(S) then
    Param.Result.AsInteger := S.RecordCount;
end;

procedure pp_dataset_getActive(const Param: TLiParam);
var
  S: TLiDataSet;
begin
  if Param.GetSelf(S) then
    Param.Result.AsBoolean := S.Active;
end;

procedure pp_dataset_setActive(const Param: TLiParam);
var
  S: TLiDataSet;
begin
  if Param.GetSelf(S) then
    S.Active := Param[1].AsBoolean;
end;

procedure pp_dataset_get(const Param: TLiParam);
var
  S: TLiDataSet;
  X: TLiValue;
  F: TLiField;
begin
  if Param.GetSelf(S) then
  begin
    X := Param[1];
    if X.VType.TID in [TID_STRING, TID_CHAR, TID_INTEGER] then
    begin
      if X.VType = my_int then
        F := S.Fields[X.AsInteger] else
        F := S.FieldByName(X.AsString);
      Param.Result.SetTOA(my_field, F);
    end
    else Param.Error('invalid field index: %s', [X.VType.Name])
  end;
end;

procedure pp_dataset_getValue(const Param: TLiParam);
var
  S: TLiDataSet;
  X: TLiValue;
  F: TLiField;
begin
  if Param.GetSelf(S) then
  begin
    X := Param[1];
    if X.VType.TID in [TID_STRING, TID_CHAR, TID_INTEGER] then
    begin
      if X.VType = my_int then
        F := S.Fields[X.AsInteger] else
        F := S.FieldByName(X.AsString);
      F.SaveValueTo(Param.Result);
    end
    else Param.Error('invalid field index: %s', [X.VType.Name])
  end;
end;

//-- my_database ---------------------------------------------------------------

procedure pp_database_open(const Param: TLiParam);
var
  C: TLiDataBase;
begin
  if Param.GetSelf(C) then C.Open;
end;

procedure pp_database_close(const Param: TLiParam);
var
  C: TLiDataBase;
begin
  if Param.GetSelf(C) then C.Close;
end;

procedure pp_database_inTransaction(const Param: TLiParam);
var
  C: TLiDataBase;
begin
  if Param.GetSelf(C) then
    Param.Result.AsBoolean := C.InTransaction;
end;

procedure pp_database_transact(const Param: TLiParam);
var
  C: TLiDataBase;
begin
  if Param.GetSelf(C) then C.Transact;
end;

procedure pp_database_commit(const Param: TLiParam);
var
  C: TLiDataBase;
begin
  if Param.GetSelf(C) then C.Commit;
end;

procedure pp_database_rollback(const Param: TLiParam);
var
  C: TLiDataBase;
begin
  if Param.GetSelf(C) then C.Rollback;
end;

procedure pp_database_tableFieldNames(const Param: TLiParam);
var
  C: TLiDataBase;
  L: TLiStringList;
begin
  if Param.GetSelf(C) then
  begin
    L := TLiStringList.Create;
    Param.Result.AsStringList := L;
    C.GetFieldNames(L.StringList, Param[1].AsString);
  end;
end;

procedure pp_database_userTableNames(const Param: TLiParam);
var
  C: TLiDataBase;
  L: TLiStringList;
begin
  if Param.GetSelf(C) then
  begin
    L := TLiStringList.Create;
    Param.Result.AsStringList := L;
    C.GetTableNames(L.StringList, false);
  end;
end;

procedure pp_database_systemTableNames(const Param: TLiParam);
var
  C: TLiDataBase;
  L: TLiStringList;
  U: TStrings;
  I: integer;
begin
  if Param.GetSelf(C) then
  begin
    L := TLiStringList.Create;
    Param.Result.AsStringList := L;
    C.GetTableNames(L.StringList, true);
    U := TStringList.Create;
    try
      C.GetTableNames(U, false);
      for I := 0 to U.Count - 1 do
        L.Remove(U[I], false);
    finally
      U.Free;
    end;
  end;
end;

procedure pp_database_tableNames(const Param: TLiParam);
var
  C: TLiDataBase;
  L: TLiStringList;
begin
  if Param.GetSelf(C) then
  begin
    L := TLiStringList.Create;
    Param.Result.AsStringList := L;
    C.GetTableNames(L.StringList, true);
  end;
end;

procedure pp_database_procedures(const Param: TLiParam);
var
  C: TLiDataBase;
  L: TLiStringList;
begin
  if Param.GetSelf(C) then
  begin
    L := TLiStringList.Create;
    Param.Result.AsStringList := L;
    C.GetProcedureNames(L.StringList);
  end;
end;

procedure pp_database_getConnected(const Param: TLiParam);
var
  C: TLiDataBase;
begin
  if Param.GetSelf(C) then
    Param.Result.AsBoolean := C.Connected;
end;

procedure pp_database_setConnected(const Param: TLiParam);
var
  C: TLiDataBase;
begin
  if Param.GetSelf(C) then
    C.Connected := Param[1].AsBoolean;
end;

procedure pp_database_getLoginPrompt(const Param: TLiParam);
var
  C: TLiDataBase;
begin
  if Param.GetSelf(C) then
    Param.Result.AsBoolean := C.LoginPrompt;
end;

procedure pp_database_setLoginPrompt(const Param: TLiParam);
var
  C: TLiDataBase;
begin
  if Param.GetSelf(C) then
    C.LoginPrompt := Param[1].AsBoolean;
end;

{ TLiField }

constructor TLiField.Create(AField: TField);
begin
  FField := AField;
end;

function TLiField.GetAsBoolean: boolean;
begin
  Result := FieldToBool(FField);
end;

function TLiField.GetAsChar: char;
begin
  Result := FieldToChar(FField);
end;

function TLiField.GetAsCurrency: currency;
begin
  Result := FieldToCurr(FField);
end;

function TLiField.GetAsFloat: double;
begin
  Result := FieldToFloat(FField);
end;

function TLiField.GetAsInteger: int64;
begin
  Result := FieldToInt(FField);
end;

function TLiField.AsString: string;
begin
  Result := FieldToStr(FField);
end;

function TLiField.GetAsTime: TDateTime;
begin
  Result := FieldToTime(FField);
end;

function TLiField.GetIndex: integer;
begin
  Result := FField.Index;
end;

function TLiField.GetName: string;
begin
  Result := FField.FieldName;
end;

function TLiField.GetType: TLiType;
begin
  Result := FieldToType(FField);
end;

function TLiField.IsNull: boolean;
begin
  Result := FField.IsNull;
end;

procedure TLiField.SaveValueTo(Value: TLiValue);
begin
  GetFieldValue(FField, Value);
end;

{ TLiDataSet }

procedure TLiDataSet.ClearFields;
var
  I: integer;
begin
  for I := GetFieldCount - 1 downto 0 do
    DeleteField(I);
end;

procedure TLiDataSet.Close;
begin
  ClearFields;
  FDataSet.Close;
end;

constructor TLiDataSet.Create(ADataSet: TDataSet; ADataBase: TLiDataBase);
begin
  FDataBase := ADataBase;
  if FDataBase <> nil then
    FDataBase.FDataSets.Add(Self);
  FDataSet := ADataSet;
  FFields := TList.Create;
  SetupFields;
end;

procedure TLiDataSet.DeleteField(Index: integer);
var
  F: TLiField;
begin
  F := GetField(Index);
  FFields.Delete(Index);
  F.FField := nil;
  F.DecRefcount;
end;

destructor TLiDataSet.Destroy;
begin
  if FDataBase <> nil then
    FDataBase.FDataSets.Remove(Self);
  ClearFields;
  FreeAndNil(FFields);
  FreeAndNil(FDataSet);
  inherited;
end;

function TLiDataSet.FieldByName(const Name: string): TLiField;
begin
  Result := FindField(Name);
  if Result = nil then
    Throw('field not found: %s', [Name]);
end;

function TLiDataSet.FindField(const Name: string): TLiField;
var
  I: integer;
begin
  I := IndexOf(Name);
  if I >= 0 then
    Result := GetField(I) else
    Result := nil;
end;

procedure TLiDataSet.First;
begin
  FDataSet.First;
end;

function TLiDataSet.GetActive: boolean;
begin
  Result := FDataSet.Active;
end;

function TLiDataSet.AsString: string;
var
  I, N: integer;
begin
  Result := '';
  if Active then
  begin
    N := FDataSet.FieldCount;
    Result := FDataSet.Fields[0].FieldName;
    for I := 1 to N - 1 do
      Result := Result + ',' + FDataSet.Fields[I].FieldName;
    Result := Result + sLineBreak;
    Result := Result + FieldToStr(FDataSet.Fields[0]);
    for I := 1 to N - 1 do
      Result := Result + ',' + FieldToStr(FDataSet.Fields[I]);
  end;
end;

function TLiDataSet.GetBof: boolean;
begin
  Result := FDataSet.Bof;
end;

function TLiDataSet.GetEof: boolean;
begin
  Result := FDataSet.Eof;
end;

function TLiDataSet.GetField(Index: integer): TLiField;
begin
  Result := TLiField(FFields[Index]);
end;

function TLiDataSet.GetFieldCount: integer;
begin
  Result := FFields.Count;
end;

function TLiDataSet.GetRecordCount: integer;
begin
  Result := FDataSet.RecordCount;
end;

function TLiDataSet.IndexOf(const Name: string): integer;
var
  I: integer;
begin
  for I := 0 to GetFieldCount - 1 do
    if MatchID(Name, GetField(I).FField.Name) then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

procedure TLiDataSet.Last;
begin
  FDataSet.Last;
end;

procedure TLiDataSet.Next;
begin
  FDataSet.Next;
end;

procedure TLiDataSet.OnClose(Sender: TObject);
begin
  ClearFields;
end;

procedure TLiDataSet.Open;
begin
  if not Active then
  begin
    ClearFields;
    FDataSet.Open;
    SetupFields;
  end;
end;

procedure TLiDataSet.Prior;
begin
  FDataSet.Prior;
end;

procedure TLiDataSet.SetActive(Value: boolean);
begin
  if Value then Open else Close;
end;

procedure TLiDataSet.SetupFields;
var
  I: integer;
  F: TLiField;
begin
  ClearFields;
  if FDataSet.Active then
    for I := 0 to FDataSet.FieldCount - 1 do
    begin
      F := TLiField.Create(FDataSet.Fields[I]);
      F.IncRefcount;
      FFields.Add(F);
    end;
end;

{ TLiDataBase }

procedure TLiDataBase.ClearDataSets;
var
  I: integer;
begin
  for I := GetDataSetCount - 1 downto 0 do
    DeleteDataSet(I);
end;

procedure TLiDataBase.Close;
begin
  CloseDataSets;
  FConnection.Close;
end;

procedure TLiDataBase.CloseDataSets;
var
  I: integer;
begin
  for I := GetDataSetCount - 1 downto 0 do
  begin
    GetDataSet(I).ClearFields;
    GetDataSet(I).Close;
  end;
end;

procedure TLiDataBase.Commit;
begin
  { nothing }
end;

procedure TLiDataBase.CommitAndTransact;
begin
  CommitRetaining;
  Transact;
end;

procedure TLiDataBase.CommitRetaining;
begin
  if InTransaction then Commit;
end;

constructor TLiDataBase.Create(AConnection: TCustomConnection);
begin
  FDataSets := TList.Create;
  FConnection := AConnection;
  FConnection.BeforeDisconnect := {$IFDEF FPC}@{$ENDIF}Disconnected;
  FConnection.AfterDisconnect := {$IFDEF FPC}@{$ENDIF}Disconnected;
end;

procedure TLiDataBase.DeleteDataSet(Index: integer);
var
  D: TLiDataSet;
begin
  D := GetDataSet(Index);
  FDataSets.Delete(Index);
  D.FDataBase := nil;
  D.ClearFields;
  D.Close;
  FreeAndNil(D.FDataSet);
end;

destructor TLiDataBase.Destroy;
begin
  Close;
  ClearDataSets;
  FreeAndNil(FDataSets);
  FreeAndNIl(FConnection);
  inherited;
end;

procedure TLiDataBase.Disconnected(Sender: TObject);
begin
  CloseDataSets;
end;

function TLiDataBase.GetConnected: boolean;
begin
  Result := FConnection.Connected;
end;

function TLiDataBase.GetDataSet(Index: integer): TLiDataSet;
begin
  Result := TLiDataSet(FDataSets[Index]);
end;

function TLiDataBase.GetDataSetCount: integer;
begin
  if FDataSets <> nil then
    Result := FDataSets.Count else
    Result := 0;
end;

function TLiDataBase.GetLoginPrompt: boolean;
begin
  Result := FConnection.LoginPrompt;
end;

procedure TLiDataBase.GetProcedureNames(List: TStrings);
begin
  { nothing }
end;

procedure TLiDataBase.GetFieldNames(List: TStrings; const Table: string);
begin
  { nothing }
end;

procedure TLiDataBase.GetTableNames(List: TStrings; SystemTables: boolean);
begin
  { nothing }
end;

function TLiDataBase.InTransaction: boolean;
begin
  Result := false;
end;

procedure TLiDataBase.Open;
begin
  FConnection.Open;
end;

procedure TLiDataBase.Rollback;
begin
  { nothing }
end;

procedure TLiDataBase.RollbackAndTransact;
begin
  RollbackRetaining;
  Transact;
end;

procedure TLiDataBase.RollbackRetaining;
begin
  if InTransaction then Rollback;
end;

procedure TLiDataBase.SetConnected(Value: boolean);
begin
  if Value then Open else Close;
end;

procedure TLiDataBase.SetLoginPrompt(Value: boolean);
begin
  FConnection.LoginPrompt := Value;
end;

procedure TLiDataBase.Transact;
begin
  { nothing }
end;

{ TLiType_TField }

function TLiType_TField._AsBoolean(Obj: pointer): boolean;
begin
  Result := (Obj <> nil) and TLiField(Obj).AsBoolean;
end;

function TLiType_TField._AsChar(Obj: pointer): char;
begin
  Result := TLiField(Obj).AsChar;
end;

function TLiType_TField._AsFloat(Obj: pointer): double;
begin
  Result := TLiField(Obj).AsFloat;
end;

function TLiType_TField._AsInteger(Obj: pointer): int64;
begin
  Result := TLiField(Obj).AsInteger;
end;

function TLiType_TField._AsCurrency(Obj: pointer): currency;
begin
  Result := TLiField(Obj).AsCurrency;
end;

function TLiType_TField._AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLiField(Obj).AsString else
    Result := '';
end;

function TLiType_TField._AsTime(Obj: pointer): TDateTime;
begin
  Result := TLiField(Obj).AsTime;
end;

function TLiType_TField._DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiField(Obj).DecRefcount else
    Result := 0;
end;

function TLiType_TField._IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiField(Obj).IncRefcount else
    Result := 0;
end;

procedure TLiType_TField._Validate(Obj: pointer);
begin
  inherited;
  Check(TLiField(Obj).FField <> nil, 'field has been released');
end;

{ TLiType_TDataSet }

function TLiType_TDataSet._AsBoolean(Obj: pointer): boolean;
begin
  Result := (Obj <> nil) and TLiDataSet(Obj).Active;
end;

function TLiType_TDataSet._AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLiDataSet(Obj).AsString else
    Result := '';
end;

function TLiType_TDataSet._DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiDataSet(Obj).DecRefcount else
    Result := 0;
end;

function TLiType_TDataSet._IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiDataSet(Obj).IncRefcount else
    Result := 0;
end;

function TLiType_TDataSet._Length(Obj: pointer): int64;
begin
  if Obj <> nil then
    Result := TLiDataSet(Obj).FieldCount else
    Result := 0;
end;

procedure TLiType_TDataSet._Validate(Obj: pointer);
begin
  inherited;
  Check(TLiDataSet(Obj).FDataSet <> nil, 'dataset has been released');
end;

{ TLiType_TDataBase }

function TLiType_TDataBase._AsString(Obj: pointer): string;
begin
  Result := '';
end;

function TLiType_TDataBase._DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiDataBase(Obj).DecRefcount else
    Result := 0;
end;

function TLiType_TDataBase._IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiDataBase(Obj).IncRefcount else
    Result := 0;
end;

procedure TLiType_TDataBase._Validate(Obj: pointer);
begin
  inherited;
  Check(TLiDataBase(Obj).FConnection <> nil, 'invalid database connection');
end;

initialization
begin
  my_db := TLiModule.Create('db');
  my_field := TLiType_TField.Create('TField', my_db, nil);
  my_dataset := TLiType_TDataSet.Create('TDataSet', my_db, nil);
  my_database := TLiType_TDataBase.Create('TDataBase', my_db, nil);

  //-- my_field ----------------------------------------------------------------

  my_field.AddMethod(['name'], [my_string],
                     {$IFDEF FPC}@{$ENDIF}pp_field_name);
  my_field.AddMethod(['type'], [my_type],
                     {$IFDEF FPC}@{$ENDIF}pp_field_type);
  my_field.AddMethod(['index'], [my_int],
                     {$IFDEF FPC}@{$ENDIF}pp_field_index);
  my_field.AddMethod(['value'], [my_variant],
                     {$IFDEF FPC}@{$ENDIF}pp_field_value);
  my_field.AddMethod(['isNull'], [my_bool],
                     {$IFDEF FPC}@{$ENDIF}pp_field_isNull);
  my_field.AddMethod(['asString'], [my_string],
                     {$IFDEF FPC}@{$ENDIF}pp_field_string);
  my_field.AddMethod(['asChar'], [my_char],
                     {$IFDEF FPC}@{$ENDIF}pp_field_char);
  my_field.AddMethod(['asInteger'], [my_int],
                     {$IFDEF FPC}@{$ENDIF}pp_field_int);
  my_field.AddMethod(['asFloat'], [my_float],
                     {$IFDEF FPC}@{$ENDIF}pp_field_float);
  my_field.AddMethod(['asCurrency'], [my_curr],
                     {$IFDEF FPC}@{$ENDIF}pp_field_curr);
  my_field.AddMethod(['asBoolean'], [my_bool],
                     {$IFDEF FPC}@{$ENDIF}pp_field_bool);
  my_field.AddMethod(['asTime'], [my_time],
                     {$IFDEF FPC}@{$ENDIF}pp_field_time);

  //-- my_dataset --------------------------------------------------------------

  my_dataset.AddMethod(['open'], [my_nil],
                       {$IFDEF FPC}@{$ENDIF}pp_dataset_open);
  my_dataset.AddMethod(['close'], [my_nil],
                       {$IFDEF FPC}@{$ENDIF}pp_dataset_close);
  my_dataset.AddMethod(['first'], [my_nil],
                       {$IFDEF FPC}@{$ENDIF}pp_dataset_first);
  my_dataset.AddMethod(['last'], [my_nil],
                       {$IFDEF FPC}@{$ENDIF}pp_dataset_last);
  my_dataset.AddMethod(['prior'], [my_nil],
                       {$IFDEF FPC}@{$ENDIF}pp_dataset_prior);
  my_dataset.AddMethod(['next'], [my_nil],
                       {$IFDEF FPC}@{$ENDIF}pp_dataset_next);
  my_dataset.AddMethod(['bof'], [my_bool],
                       {$IFDEF FPC}@{$ENDIF}pp_dataset_bof);
  my_dataset.AddMethod(['eof'], [my_bool],
                       {$IFDEF FPC}@{$ENDIF}pp_dataset_eof);
  my_dataset.AddMethod(['findField', 'fieldName'], [my_field, my_string],
                       {$IFDEF FPC}@{$ENDIF}pp_dataset_findField);
  my_dataset.AddMethod(['fieldByName', 'fieldName'], [my_field, my_string],
                       {$IFDEF FPC}@{$ENDIF}pp_dataset_fieldByName);
  my_dataset.AddMethod(['recordCount'], [my_int],
                       {$IFDEF FPC}@{$ENDIF}pp_dataset_recordCount);
  my_dataset.AddMethod(['fieldCount'], [my_int],
                       {$IFDEF FPC}@{$ENDIF}pp_dataset_fieldCount);
  my_dataset.SetupProp('active', my_bool,
                       {$IFDEF FPC}@{$ENDIF}pp_dataset_getActive,
                       {$IFDEF FPC}@{$ENDIF}pp_dataset_setActive);
  my_dataset.SetupProp('fields', my_field, ['index'], [my_variant],
                       {$IFDEF FPC}@{$ENDIF}pp_dataset_get, nil);
  my_dataset.SetupProp('values', my_variant, ['index'], [my_variant],
                       {$IFDEF FPC}@{$ENDIF}pp_dataset_getValue, nil);
  my_dataset.SetupProp('', my_variant, ['index'], [my_variant],
                       {$IFDEF FPC}@{$ENDIF}pp_dataset_getValue, nil);

  //-- my_connection -----------------------------------------------------------

  my_database.AddMethod(['open'], [my_nil],
                        {$IFDEF FPC}@{$ENDIF}pp_database_open);
  my_database.AddMethod(['close'], [my_nil],
                        {$IFDEF FPC}@{$ENDIF}pp_database_close);
  my_database.AddMethod(['inTransaction'], [my_bool],
                        {$IFDEF FPC}@{$ENDIF}pp_database_inTransaction);
  my_database.AddMethod(['transact'], [my_nil],
                        {$IFDEF FPC}@{$ENDIF}pp_database_transact);
  my_database.AddMethod(['commit'], [my_nil],
                        {$IFDEF FPC}@{$ENDIF}pp_database_commit);
  my_database.AddMethod(['rollback'], [my_nil],
                        {$IFDEF FPC}@{$ENDIF}pp_database_rollback);
  my_database.AddMethod(['userTableNames'], [my_strlist],
                        {$IFDEF FPC}@{$ENDIF}pp_database_userTableNames);
  my_database.AddMethod(['systemTableNames'], [my_strlist],
                        {$IFDEF FPC}@{$ENDIF}pp_database_systemTableNames);
  my_database.AddMethod(['tableNames'], [my_strlist],
                        {$IFDEF FPC}@{$ENDIF}pp_database_tableNames);
  my_database.AddMethod(['tableFieldNames', 'table'], [my_strlist, my_string],
                        {$IFDEF FPC}@{$ENDIF}pp_database_tableFieldNames);
  my_database.AddMethod(['procedureNames'], [my_strlist],
                        {$IFDEF FPC}@{$ENDIF}pp_database_procedures);
  my_database.SetupProp('connected', my_bool,
                        {$IFDEF FPC}@{$ENDIF}pp_database_getConnected,
                        {$IFDEF FPC}@{$ENDIF}pp_database_setConnected);
  my_database.SetupProp('loginPrompt', my_bool,
                        {$IFDEF FPC}@{$ENDIF}pp_database_getLoginPrompt,
                        {$IFDEF FPC}@{$ENDIF}pp_database_setLoginPrompt);
end;

end.
