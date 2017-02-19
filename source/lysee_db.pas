{==============================================================================}
{        UNIT: lysee_db                                                        }
{ DESCRIPTION: ADO database                                                    }
{   COPYRIGHT: Copyright (c) 2012, Li Yun Jie. All Rights Reserved.            }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2012/05/19                                                      }
{    MODIFIED: 2017/02/19                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_db;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Db, basic, lysee;

type

  TLyseeDataBase = class;{forward}
  TLyseeDataSet  = class;

  { TLyseeField }

  TLyseeField = class(TBasicObject)
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
    function GetType: TLyseeType;
  public
    constructor Create(AField: TField);
    procedure SaveValueTo(Value: TLyseeValue);
    function IsNull: boolean;
    function AsString: string;override;
    property AsChar: char read GetAsChar;
    property AsInteger: int64 read GetAsInteger;
    property AsFloat: double read GetAsFloat;
    property AsCurrency: currency read GetAsCurrency;
    property AsTime: TDateTime read GetAsTime;
    property AsBoolean: boolean read GetAsBoolean;
    property Field: TField read FField;
    property FieldName: string read GetName;
    property FieldType: TLyseeType read GetType;
    property FieldIndex: integer read GetIndex;
  end;

  { TLyseeFieldType }

  TLyseeFieldType = class(TLyseeType)
  protected
    procedure MyName(const Param: TLyseeParam);
    procedure MyType(const Param: TLyseeParam);
    procedure MyIndex(const Param: TLyseeParam);
    procedure MyValue(const Param: TLyseeParam);
    procedure MyIsNull(const Param: TLyseeParam);
    procedure MyAsString(const Param: TLyseeParam);
    procedure MyAsChar(const Param: TLyseeParam);
    procedure MyAsInteger(const Param: TLyseeParam);
    procedure MyAsFloat(const Param: TLyseeParam);
    procedure MyAsCurrency(const Param: TLyseeParam);
    procedure MyAsBoolean(const Param: TLyseeParam);
    procedure MyAsTime(const Param: TLyseeParam);
    procedure Setup;override;
  public
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function AsString(Obj: pointer): string;override;
    function AsChar(Obj: pointer): char;override;
    function AsInteger(Obj: pointer): int64;override;
    function AsFloat(Obj: pointer): double;override;
    function AsCurrency(Obj: pointer): currency;override;
    function AsTime(Obj: pointer): TDateTime;override;
    function AsBoolean(Obj: pointer): boolean;override;
    procedure Validate(Obj: pointer);override;
  end;

  { TLyseeDataSet }

  TLyseeDataSet = class(TBasicObject)
  private
    FDataBase: TLyseeDataBase;
    FDataSet: TDataSet;
    FFields: TList;
    function GetFieldCount: integer;
    function GetField(Index: integer): TLyseeField;
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
    constructor Create(ADataSet: TDataSet; ADataBase: TLyseeDataBase);virtual;
    destructor Destroy;override;
    procedure Close;
    procedure Open;
    procedure First;
    procedure Last;
    procedure Prior;
    procedure Next;
    function IndexOf(const Name: string): integer;
    function FindField(const Name: string): TLyseeField;
    function FieldByName(const Name: string): TLyseeField;
    function AsString: string;override;
    property DataSet: TDataSet read FDataSet;
    property Active: boolean read GetActive write SetActive;
    property RecordCount: integer read GetRecordCount;
    property FieldCount: integer read GetFieldCount;
    property Fields[Index: integer]: TLyseeField read GetField;default;
    property Bof: boolean read GetBof;
    property Eof: boolean read GetEof;
  end;

  { TLyseeDataSetType }

  TLyseeDataSetType = class(TLyseeType)
  protected
    procedure MyOpen(const Param: TLyseeParam);
    procedure MyClose(const Param: TLyseeParam);
    procedure MyFirst(const Param: TLyseeParam);
    procedure MyLast(const Param: TLyseeParam);
    procedure MyPrior(const Param: TLyseeParam);
    procedure MyNext(const Param: TLyseeParam);
    procedure MyBof(const Param: TLyseeParam);
    procedure MyEof(const Param: TLyseeParam);
    procedure MyFindField(const Param: TLyseeParam);
    procedure MyFieldByName(const Param: TLyseeParam);
    procedure MyFieldCount(const Param: TLyseeParam);
    procedure MyRecordCount(const Param: TLyseeParam);
    procedure MyGetActive(const Param: TLyseeParam);
    procedure MySetActive(const Param: TLyseeParam);
    procedure MyGetField(const Param: TLyseeParam);
    procedure MyGetValue(const Param: TLyseeParam);
    procedure Setup;override;
  public
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function AsString(Obj: pointer): string;override;
    function AsBoolean(Obj: pointer): boolean;override;
    function GetLength(Obj: pointer): int64;override;
    procedure Validate(Obj: pointer);override;
  end;

  { TLyseeDataBase }

  TLyseeDataBase = class(TBasicObject)
  private
    FConnection: TCustomConnection;
    FDataSets: TList;
    function GetDataSetCount: integer;
    function GetDataSet(Index: integer): TLyseeDataSet;
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
    property DataSets[Index: integer]: TLyseeDataSet read GetDataSet;default;
    property LoginPrompt: boolean read GetLoginPrompt write SetLoginPrompt;
  end;

  { TLyseeDataBaseType }

  TLyseeDataBaseType = class(TLyseeType)
  protected
    procedure MyOpen(const Param: TLyseeParam);
    procedure MyClose(const Param: TLyseeParam);
    procedure MyInTransaction(const Param: TLyseeParam);
    procedure MyTransact(const Param: TLyseeParam);
    procedure MyCommit(const Param: TLyseeParam);
    procedure MyRollback(const Param: TLyseeParam);
    procedure MyTableNames(const Param: TLyseeParam);
    procedure MyUserTableNames(const Param: TLyseeParam);
    procedure MySystemTableNames(const Param: TLyseeParam);
    procedure MyTableFieldNames(const Param: TLyseeParam);
    procedure MyProcedureNames(const Param: TLyseeParam);
    procedure MyGetConnected(const Param: TLyseeParam);
    procedure MySetConnected(const Param: TLyseeParam);
    procedure MyGetLoginPrompt(const Param: TLyseeParam);
    procedure MySetLoginPrompt(const Param: TLyseeParam);
    procedure Setup;override;
  public
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function AsString(Obj: pointer): string;override;
    procedure Validate(Obj: pointer);override;
  end;

  { TLyseeDbModule }

  TLyseeDbModule = class(TLyseeModule)
  private
    procedure DoSetup(Sender: TObject);
  public
    constructor Create(const AName: string);override;
  end;

var
  my_db: TLyseeDbModule;
  my_field: TLyseeFieldType;
  my_dataset: TLyseeDataSetType;
  my_database: TLyseeDataBaseType;

function FieldTID(AType: TFieldType): integer;overload;
function FieldTID(AField: TField): integer;overload;
function FieldToType(AType: TFieldType): TLyseeType;overload;
function FieldToType(AField: TField): TLyseeType;overload;
function FieldToStr(AField: TField): string;
function FieldToChar(AField: TField): char;
function FieldToInt(AField: TField): int64;
function FieldToBool(AField: TField): boolean;
function FieldToFloat(AField: TField): double;
function FieldToCurr(AField: TField): currency;
function FieldToTime(AField: TField): TDateTime;
procedure GetFieldValue(AField: TField; Value: TLyseeValue);

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

function FieldToType(AType: TFieldType): TLyseeType;
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

function FieldToType(AField: TField): TLyseeType;
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
  T: TLyseeType;
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

procedure GetFieldValue(AField: TField; Value: TLyseeValue);
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

{ TLyseeField }

constructor TLyseeField.Create(AField: TField);
begin
  FField := AField;
end;

function TLyseeField.GetAsBoolean: boolean;
begin
  Result := FieldToBool(FField);
end;

function TLyseeField.GetAsChar: char;
begin
  Result := FieldToChar(FField);
end;

function TLyseeField.GetAsCurrency: currency;
begin
  Result := FieldToCurr(FField);
end;

function TLyseeField.GetAsFloat: double;
begin
  Result := FieldToFloat(FField);
end;

function TLyseeField.GetAsInteger: int64;
begin
  Result := FieldToInt(FField);
end;

function TLyseeField.AsString: string;
begin
  Result := FieldToStr(FField);
end;

function TLyseeField.GetAsTime: TDateTime;
begin
  Result := FieldToTime(FField);
end;

function TLyseeField.GetIndex: integer;
begin
  Result := FField.Index;
end;

function TLyseeField.GetName: string;
begin
  Result := FField.FieldName;
end;

function TLyseeField.GetType: TLyseeType;
begin
  Result := FieldToType(FField);
end;

function TLyseeField.IsNull: boolean;
begin
  Result := FField.IsNull;
end;

procedure TLyseeField.SaveValueTo(Value: TLyseeValue);
begin
  GetFieldValue(FField, Value);
end;

{ TLyseeFieldType }

procedure TLyseeFieldType.MyAsBoolean(const Param: TLyseeParam);
var
  F: TLyseeField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsBoolean := F.AsBoolean;
end;

procedure TLyseeFieldType.MyAsChar(const Param: TLyseeParam);
var
  F: TLyseeField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsChar := F.AsChar;
end;

procedure TLyseeFieldType.MyAsCurrency(const Param: TLyseeParam);
var
  F: TLyseeField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsCurrency := F.AsCurrency;
end;

procedure TLyseeFieldType.MyAsFloat(const Param: TLyseeParam);
var
  F: TLyseeField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsFloat := F.AsFloat;
end;

procedure TLyseeFieldType.MyAsInteger(const Param: TLyseeParam);
var
  F: TLyseeField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsInteger := F.AsInteger;
end;

procedure TLyseeFieldType.MyAsString(const Param: TLyseeParam);
var
  F: TLyseeField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsString := F.AsString;
end;

procedure TLyseeFieldType.MyAsTime(const Param: TLyseeParam);
var
  F: TLyseeField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsTime := F.AsTime;
end;

procedure TLyseeFieldType.MyIndex(const Param: TLyseeParam);
var
  F: TLyseeField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsInteger := F.FieldIndex;
end;

procedure TLyseeFieldType.MyIsNull(const Param: TLyseeParam);
var
  F: TLyseeField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsBoolean := F.IsNull;
end;

procedure TLyseeFieldType.MyName(const Param: TLyseeParam);
var
  F: TLyseeField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsString := F.FieldName;
end;

procedure TLyseeFieldType.MyType(const Param: TLyseeParam);
var
  F: TLyseeField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsType := F.FieldType;
end;

procedure TLyseeFieldType.MyValue(const Param: TLyseeParam);
var
  F: TLyseeField;
begin
  if Param.GetSelf(F) then
    GetFieldValue(F.FField, Param.Result);
end;

procedure TLyseeFieldType.Setup;
begin
  Method('Name', my_string, {$IFDEF FPC}@{$ENDIF}MyName);
  Method('Type', my_type, {$IFDEF FPC}@{$ENDIF}MyType);
  Method('Index', my_int, {$IFDEF FPC}@{$ENDIF}MyIndex);
  Method('Value', my_variant, {$IFDEF FPC}@{$ENDIF}MyValue);
  Method('IsNull', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsNull);
  Method('AsString', my_string, {$IFDEF FPC}@{$ENDIF}MyAsString);
  Method('AsChar', my_char, {$IFDEF FPC}@{$ENDIF}MyAsChar);
  Method('AsInteger', my_int, {$IFDEF FPC}@{$ENDIF}MyAsInteger);
  Method('AsFloat', my_float, {$IFDEF FPC}@{$ENDIF}MyAsFloat);
  Method('AsCurrency', my_curr, {$IFDEF FPC}@{$ENDIF}MyAsCurrency);
  Method('AsBoolean', my_bool, {$IFDEF FPC}@{$ENDIF}MyAsBoolean);
  Method('AsTime', my_time, {$IFDEF FPC}@{$ENDIF}MyAsTime);
  inherited;
end;

function TLyseeFieldType.AsBoolean(Obj: pointer): boolean;
begin
  Result := (Obj <> nil) and TLyseeField(Obj).AsBoolean;
end;

function TLyseeFieldType.AsChar(Obj: pointer): char;
begin
  Result := TLyseeField(Obj).AsChar;
end;

function TLyseeFieldType.AsFloat(Obj: pointer): double;
begin
  Result := TLyseeField(Obj).AsFloat;
end;

function TLyseeFieldType.AsInteger(Obj: pointer): int64;
begin
  Result := TLyseeField(Obj).AsInteger;
end;

function TLyseeFieldType.AsCurrency(Obj: pointer): currency;
begin
  Result := TLyseeField(Obj).AsCurrency;
end;

function TLyseeFieldType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyseeField(Obj).AsString else
    Result := '';
end;

function TLyseeFieldType.AsTime(Obj: pointer): TDateTime;
begin
  Result := TLyseeField(Obj).AsTime;
end;

function TLyseeFieldType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeField(Obj).DecRefcount else
    Result := 0;
end;

function TLyseeFieldType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeField(Obj).IncRefcount else
    Result := 0;
end;

procedure TLyseeFieldType.Validate(Obj: pointer);
begin
  inherited;
  Check(TLyseeField(Obj).FField <> nil, 'field has been released');
end;

{ TLyseeDataSet }

procedure TLyseeDataSet.ClearFields;
var
  I: integer;
begin
  for I := GetFieldCount - 1 downto 0 do
    DeleteField(I);
end;

procedure TLyseeDataSet.Close;
begin
  ClearFields;
  FDataSet.Close;
end;

constructor TLyseeDataSet.Create(ADataSet: TDataSet; ADataBase: TLyseeDataBase);
begin
  FDataBase := ADataBase;
  if FDataBase <> nil then
    FDataBase.FDataSets.Add(Self);
  FDataSet := ADataSet;
  FFields := TList.Create;
  SetupFields;
end;

procedure TLyseeDataSet.DeleteField(Index: integer);
var
  F: TLyseeField;
begin
  F := GetField(Index);
  FFields.Delete(Index);
  F.FField := nil;
  F.DecRefcount;
end;

destructor TLyseeDataSet.Destroy;
begin
  if FDataBase <> nil then
    FDataBase.FDataSets.Remove(Self);
  ClearFields;
  FreeAndNil(FFields);
  FreeAndNil(FDataSet);
  inherited;
end;

function TLyseeDataSet.FieldByName(const Name: string): TLyseeField;
begin
  Result := FindField(Name);
  if Result = nil then
    Throw('field not found: %s', [Name]);
end;

function TLyseeDataSet.FindField(const Name: string): TLyseeField;
var
  I: integer;
begin
  I := IndexOf(Name);
  if I >= 0 then
    Result := GetField(I) else
    Result := nil;
end;

procedure TLyseeDataSet.First;
begin
  FDataSet.First;
end;

function TLyseeDataSet.GetActive: boolean;
begin
  Result := FDataSet.Active;
end;

function TLyseeDataSet.AsString: string;
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

function TLyseeDataSet.GetBof: boolean;
begin
  Result := FDataSet.Bof;
end;

function TLyseeDataSet.GetEof: boolean;
begin
  Result := FDataSet.Eof;
end;

function TLyseeDataSet.GetField(Index: integer): TLyseeField;
begin
  Result := TLyseeField(FFields[Index]);
end;

function TLyseeDataSet.GetFieldCount: integer;
begin
  Result := FFields.Count;
end;

function TLyseeDataSet.GetRecordCount: integer;
begin
  Result := FDataSet.RecordCount;
end;

function TLyseeDataSet.IndexOf(const Name: string): integer;
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

procedure TLyseeDataSet.Last;
begin
  FDataSet.Last;
end;

procedure TLyseeDataSet.Next;
begin
  FDataSet.Next;
end;

procedure TLyseeDataSet.OnClose(Sender: TObject);
begin
  ClearFields;
end;

procedure TLyseeDataSet.Open;
begin
  if not Active then
  begin
    ClearFields;
    FDataSet.Open;
    SetupFields;
  end;
end;

procedure TLyseeDataSet.Prior;
begin
  FDataSet.Prior;
end;

procedure TLyseeDataSet.SetActive(Value: boolean);
begin
  if Value then Open else Close;
end;

procedure TLyseeDataSet.SetupFields;
var
  I: integer;
  F: TLyseeField;
begin
  ClearFields;
  if FDataSet.Active then
    for I := 0 to FDataSet.FieldCount - 1 do
    begin
      F := TLyseeField.Create(FDataSet.Fields[I]);
      F.IncRefcount;
      FFields.Add(F);
    end;
end;

{ TLyseeDataSetType }

procedure TLyseeDataSetType.MyFieldByName(const Param: TLyseeParam);
var
  S: TLyseeDataSet;
  F: TLyseeField;
begin
  if Param.GetSelf(S) then
  begin
    F := S.FieldByName(Param[1].AsString);
    Param.Result.SetTOA(my_field, F);
  end;
end;

procedure TLyseeDataSetType.MyFieldCount(const Param: TLyseeParam);
var
  S: TLyseeDataSet;
begin
  if Param.GetSelf(S) then
    Param.Result.AsInteger := S.FieldCount;
end;

procedure TLyseeDataSetType.MyBof(const Param: TLyseeParam);
var
  S: TLyseeDataSet;
begin
  if Param.GetSelf(S) then
    Param.Result.AsBoolean := S.Bof;
end;

procedure TLyseeDataSetType.MyClose(const Param: TLyseeParam);
var
  S: TLyseeDataSet;
begin
  if Param.GetSelf(S) then S.Close;
end;

procedure TLyseeDataSetType.MyEof(const Param: TLyseeParam);
var
  S: TLyseeDataSet;
begin
  if Param.GetSelf(S) then
    Param.Result.AsBoolean := S.Eof;
end;

procedure TLyseeDataSetType.MyFindField(const Param: TLyseeParam);
var
  S: TLyseeDataSet;
  F: TLyseeField;
begin
  if Param.GetSelf(S) then
  begin
    F := S.FindField(Param[1].AsString);
    Param.Result.SetTOA(my_field, F);
  end;
end;

procedure TLyseeDataSetType.MyFirst(const Param: TLyseeParam);
var
  S: TLyseeDataSet;
begin
  if Param.GetSelf(S) then S.First;
end;

procedure TLyseeDataSetType.MyGetActive(const Param: TLyseeParam);
var
  S: TLyseeDataSet;
begin
  if Param.GetSelf(S) then
    Param.Result.AsBoolean := S.Active;
end;

procedure TLyseeDataSetType.MyGetField(const Param: TLyseeParam);
var
  S: TLyseeDataSet;
  X: TLyseeValue;
  F: TLyseeField;
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

procedure TLyseeDataSetType.MyGetValue(const Param: TLyseeParam);
var
  S: TLyseeDataSet;
  X: TLyseeValue;
  F: TLyseeField;
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

procedure TLyseeDataSetType.MyLast(const Param: TLyseeParam);
var
  S: TLyseeDataSet;
begin
  if Param.GetSelf(S) then S.Last;
end;

procedure TLyseeDataSetType.MyNext(const Param: TLyseeParam);
var
  S: TLyseeDataSet;
begin
  if Param.GetSelf(S) then S.Next;
end;

procedure TLyseeDataSetType.MyOpen(const Param: TLyseeParam);
var
  S: TLyseeDataSet;
begin
  if Param.GetSelf(S) then S.Open;
end;

procedure TLyseeDataSetType.MyPrior(const Param: TLyseeParam);
var
  S: TLyseeDataSet;
begin
  if Param.GetSelf(S) then S.Prior;
end;

procedure TLyseeDataSetType.MyRecordCount(const Param: TLyseeParam);
var
  S: TLyseeDataSet;
begin
  if Param.GetSelf(S) then
    Param.Result.AsInteger := S.RecordCount;
end;

procedure TLyseeDataSetType.MySetActive(const Param: TLyseeParam);
var
  S: TLyseeDataSet;
begin
  if Param.GetSelf(S) then
    S.Active := Param[1].AsBoolean;
end;

procedure TLyseeDataSetType.Setup;
begin
  Method('Open', {$IFDEF FPC}@{$ENDIF}MyOpen);
  Method('Close', {$IFDEF FPC}@{$ENDIF}MyClose);
  Method('First', {$IFDEF FPC}@{$ENDIF}MyFirst);
  Method('Last', {$IFDEF FPC}@{$ENDIF}MyLast);
  Method('Prior', {$IFDEF FPC}@{$ENDIF}MyPrior);
  Method('Next', {$IFDEF FPC}@{$ENDIF}MyNext);
  Method('Bof', my_bool, {$IFDEF FPC}@{$ENDIF}MyBof);
  Method('Eof', my_bool, {$IFDEF FPC}@{$ENDIF}MyEof);
  Method('RecordCount', my_int, {$IFDEF FPC}@{$ENDIF}MyRecordCount);
  Method('FieldCount', my_int, {$IFDEF FPC}@{$ENDIF}MyFieldCount);
  Method('FindField', my_field, ['FieldName'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyFindField);
  Method('FieldByName', my_field, ['FieldName'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyFieldByName);
  Define('Active', my_bool,
         {$IFDEF FPC}@{$ENDIF}MyGetActive,
         {$IFDEF FPC}@{$ENDIF}MySetActive);
  Define('Fields', my_field, 'Index', my_variant,
         {$IFDEF FPC}@{$ENDIF}MyGetField);
  Define('', my_field, 'Index', my_variant,
         {$IFDEF FPC}@{$ENDIF}MyGetField);
  Define('Values', my_variant, 'Index', my_variant,
         {$IFDEF FPC}@{$ENDIF}MyGetValue);
  inherited;
end;

function TLyseeDataSetType.AsBoolean(Obj: pointer): boolean;
begin
  Result := (Obj <> nil) and TLyseeDataSet(Obj).Active;
end;

function TLyseeDataSetType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyseeDataSet(Obj).AsString else
    Result := '';
end;

function TLyseeDataSetType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeDataSet(Obj).DecRefcount else
    Result := 0;
end;

function TLyseeDataSetType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeDataSet(Obj).IncRefcount else
    Result := 0;
end;

function TLyseeDataSetType.GetLength(Obj: pointer): int64;
begin
  if Obj <> nil then
    Result := TLyseeDataSet(Obj).FieldCount else
    Result := 0;
end;

procedure TLyseeDataSetType.Validate(Obj: pointer);
begin
  inherited;
  Check(TLyseeDataSet(Obj).FDataSet <> nil, 'dataset has been released');
end;

{ TLyseeDataBase }

procedure TLyseeDataBase.ClearDataSets;
var
  I: integer;
begin
  for I := GetDataSetCount - 1 downto 0 do
    DeleteDataSet(I);
end;

procedure TLyseeDataBase.Close;
begin
  CloseDataSets;
  FConnection.Close;
end;

procedure TLyseeDataBase.CloseDataSets;
var
  I: integer;
begin
  for I := GetDataSetCount - 1 downto 0 do
  begin
    GetDataSet(I).ClearFields;
    GetDataSet(I).Close;
  end;
end;

procedure TLyseeDataBase.Commit;
begin
  { nothing }
end;

procedure TLyseeDataBase.CommitAndTransact;
begin
  CommitRetaining;
  Transact;
end;

procedure TLyseeDataBase.CommitRetaining;
begin
  if InTransaction then Commit;
end;

constructor TLyseeDataBase.Create(AConnection: TCustomConnection);
begin
  FDataSets := TList.Create;
  FConnection := AConnection;
  FConnection.BeforeDisconnect := {$IFDEF FPC}@{$ENDIF}Disconnected;
  FConnection.AfterDisconnect := {$IFDEF FPC}@{$ENDIF}Disconnected;
end;

procedure TLyseeDataBase.DeleteDataSet(Index: integer);
var
  D: TLyseeDataSet;
begin
  D := GetDataSet(Index);
  FDataSets.Delete(Index);
  D.FDataBase := nil;
  D.ClearFields;
  D.Close;
  FreeAndNil(D.FDataSet);
end;

destructor TLyseeDataBase.Destroy;
begin
  Close;
  ClearDataSets;
  FreeAndNil(FDataSets);
  FreeAndNIl(FConnection);
  inherited;
end;

procedure TLyseeDataBase.Disconnected(Sender: TObject);
begin
  CloseDataSets;
end;

function TLyseeDataBase.GetConnected: boolean;
begin
  Result := FConnection.Connected;
end;

function TLyseeDataBase.GetDataSet(Index: integer): TLyseeDataSet;
begin
  Result := TLyseeDataSet(FDataSets[Index]);
end;

function TLyseeDataBase.GetDataSetCount: integer;
begin
  if FDataSets <> nil then
    Result := FDataSets.Count else
    Result := 0;
end;

function TLyseeDataBase.GetLoginPrompt: boolean;
begin
  Result := FConnection.LoginPrompt;
end;

procedure TLyseeDataBase.GetProcedureNames(List: TStrings);
begin
  { nothing }
end;

procedure TLyseeDataBase.GetFieldNames(List: TStrings; const Table: string);
begin
  { nothing }
end;

procedure TLyseeDataBase.GetTableNames(List: TStrings; SystemTables: boolean);
begin
  { nothing }
end;

function TLyseeDataBase.InTransaction: boolean;
begin
  Result := false;
end;

procedure TLyseeDataBase.Open;
begin
  FConnection.Open;
end;

procedure TLyseeDataBase.Rollback;
begin
  { nothing }
end;

procedure TLyseeDataBase.RollbackAndTransact;
begin
  RollbackRetaining;
  Transact;
end;

procedure TLyseeDataBase.RollbackRetaining;
begin
  if InTransaction then Rollback;
end;

procedure TLyseeDataBase.SetConnected(Value: boolean);
begin
  if Value then Open else Close;
end;

procedure TLyseeDataBase.SetLoginPrompt(Value: boolean);
begin
  FConnection.LoginPrompt := Value;
end;

procedure TLyseeDataBase.Transact;
begin
  { nothing }
end;

{ TLyseeDataBaseType }

procedure TLyseeDataBaseType.MyClose(const Param: TLyseeParam);
var
  C: TLyseeDataBase;
begin
  if Param.GetSelf(C) then C.Close;
end;

procedure TLyseeDataBaseType.MyCommit(const Param: TLyseeParam);
var
  C: TLyseeDataBase;
begin
  if Param.GetSelf(C) then C.Commit;
end;

procedure TLyseeDataBaseType.MyGetConnected(const Param: TLyseeParam);
var
  C: TLyseeDataBase;
begin
  if Param.GetSelf(C) then
    Param.Result.AsBoolean := C.Connected;
end;

procedure TLyseeDataBaseType.MyGetLoginPrompt(const Param: TLyseeParam);
var
  C: TLyseeDataBase;
begin
  if Param.GetSelf(C) then
    Param.Result.AsBoolean := C.LoginPrompt;
end;

procedure TLyseeDataBaseType.MyInTransaction(const Param: TLyseeParam);
var
  C: TLyseeDataBase;
begin
  if Param.GetSelf(C) then
    Param.Result.AsBoolean := C.InTransaction;
end;

procedure TLyseeDataBaseType.MyOpen(const Param: TLyseeParam);
var
  C: TLyseeDataBase;
begin
  if Param.GetSelf(C) then C.Open;
end;

procedure TLyseeDataBaseType.MyProcedureNames(const Param: TLyseeParam);
var
  C: TLyseeDataBase;
  L: TLyseeList;
  T: TStrings;
begin
  if Param.GetSelf(C) then
  begin
    L := TLyseeList.Create;
    Param.Result.SetTOA(my_array, L);
    T := TStringList.Create;
    try
      C.GetProcedureNames(T);
      L.AddStrings(T);
    finally
      T.Free;
    end;
  end;
end;

procedure TLyseeDataBaseType.MyRollback(const Param: TLyseeParam);
var
  C: TLyseeDataBase;
begin
  if Param.GetSelf(C) then C.Rollback;
end;

procedure TLyseeDataBaseType.MySetConnected(const Param: TLyseeParam);
var
  C: TLyseeDataBase;
begin
  if Param.GetSelf(C) then
    C.Connected := Param[1].AsBoolean;
end;

procedure TLyseeDataBaseType.MySetLoginPrompt(const Param: TLyseeParam);
var
  C: TLyseeDataBase;
begin
  if Param.GetSelf(C) then
    C.LoginPrompt := Param[1].AsBoolean;
end;

procedure TLyseeDataBaseType.MySystemTableNames(const Param: TLyseeParam);
var
  C: TLyseeDataBase;
  L: TLyseeList;
  T, U: TStrings;
  I, X: integer;
begin
  if Param.GetSelf(C) then
  begin
    L := TLyseeList.Create;
    Param.Result.SetTOA(my_array, L);
    T := TStringList.Create;
    U := TStringList.Create;
    try
      C.GetTableNames(T, true);
      C.GetTableNames(U, false);
      for I := 0 to U.Count - 1 do
      begin
        X := T.IndexOf(U[I]);
        if X >= 0 then T.Delete(X);
      end;
      L.AddStrings(T);
    finally
      U.Free;
      T.Free;
    end;
  end;
end;

procedure TLyseeDataBaseType.MyTableFieldNames(const Param: TLyseeParam);
var
  C: TLyseeDataBase;
  L: TLyseeList;
  T: TStrings;
begin
  if Param.GetSelf(C) then
  begin
    L := TLyseeList.Create;
    Param.Result.SetTOA(my_array, L);
    T := TStringList.Create;
    try
      C.GetFieldNames(T, Param[1].AsString);
      L.AddStrings(T);
    finally
      T.Free;
    end;
  end;
end;

procedure TLyseeDataBaseType.MyTableNames(const Param: TLyseeParam);
var
  C: TLyseeDataBase;
  L: TLyseeList;
  T: TStrings;
begin
  if Param.GetSelf(C) then
  begin
    L := TLyseeList.Create;
    Param.Result.SetTOA(my_array, L);
    T := TStringList.Create;
    try
      C.GetTableNames(T, true);
      L.AddStrings(T);
    finally
      T.Free;
    end;
  end;
end;

procedure TLyseeDataBaseType.MyTransact(const Param: TLyseeParam);
var
  C: TLyseeDataBase;
begin
  if Param.GetSelf(C) then C.Transact;
end;

procedure TLyseeDataBaseType.MyUserTableNames(const Param: TLyseeParam);
var
  C: TLyseeDataBase;
  L: TLyseeList;
  T: TStrings;
begin
  if Param.GetSelf(C) then
  begin
    L := TLyseeList.Create;
    Param.Result.SetTOA(my_array, L);
    T := TStringList.Create;
    try
      C.GetTableNames(T, false);
      L.AddStrings(T);
    finally
      T.Free;
    end;
  end;
end;

procedure TLyseeDataBaseType.Setup;
begin
  Method('Open', {$IFDEF FPC}@{$ENDIF}MyOpen);
  Method('Close', {$IFDEF FPC}@{$ENDIF}MyClose);
  Method('InTransaction', my_bool, {$IFDEF FPC}@{$ENDIF}MyInTransaction);
  Method('Transact', {$IFDEF FPC}@{$ENDIF}MyTransact);
  Method('Commit', {$IFDEF FPC}@{$ENDIF}MyCommit);
  Method('Rollback', {$IFDEF FPC}@{$ENDIF}MyRollback);
  Method('TableNames', my_array, {$IFDEF FPC}@{$ENDIF}MyTableNames);
  Method('UserTableNames', my_array, {$IFDEF FPC}@{$ENDIF}MyUserTableNames);
  Method('SystemTableNames', my_array, {$IFDEF FPC}@{$ENDIF}MySystemTableNames);
  Method('ProcedureNames', my_array, {$IFDEF FPC}@{$ENDIF}MyProcedureNames);
  Method('TableFieldNames', my_array, ['table'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyTableFieldNames);
  Define('Connected', my_bool,
         {$IFDEF FPC}@{$ENDIF}MyGetConnected,
         {$IFDEF FPC}@{$ENDIF}MySetConnected);
  Define('LoginPrompt', my_bool,
         {$IFDEF FPC}@{$ENDIF}MyGetLoginPrompt,
         {$IFDEF FPC}@{$ENDIF}MySetLoginPrompt);
  inherited;
end;

function TLyseeDataBaseType.AsString(Obj: pointer): string;
begin
  Result := '';
end;

function TLyseeDataBaseType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeDataBase(Obj).DecRefcount else
    Result := 0;
end;

function TLyseeDataBaseType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeDataBase(Obj).IncRefcount else
    Result := 0;
end;

procedure TLyseeDataBaseType.Validate(Obj: pointer);
begin
  inherited;
  Check(TLyseeDataBase(Obj).FConnection <> nil, 'invalid database connection');
end;

{ TLyseeDbModule }

constructor TLyseeDbModule.Create(const AName: string);
begin
  inherited;
  OnSetup := {$IFDEF FPC}@{$ENDIF}DoSetup;
end;

procedure TLyseeDbModule.DoSetup(Sender: TObject);
begin
  OnSetup := nil;
  my_field := TLyseeFieldType.Create('TField', my_db, nil);
  my_dataset := TLyseeDataSetType.Create('TDataSet', my_db, nil);
  my_database := TLyseeDataBaseType.Create('TDataBase', my_db, nil);
  my_field.Setup;
  my_dataset.Setup;
  my_database.Setup;
end;

initialization
begin
  my_db := TLyseeDbModule.Create('Db');
end;

end.
