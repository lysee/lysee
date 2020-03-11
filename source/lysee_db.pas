{==============================================================================}
{        UNIT: lysee_db                                                        }
{ DESCRIPTION: lysee database                                                  }
{   COPYRIGHT: Copyright (c) 2012, Li Yun Jie. All Rights Reserved.            }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2012/05/19                                                      }
{    MODIFIED: 2020/02/15                                                      }
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

  TLyDataBase = class;{forward}
  TLyDataSet  = class;

  { TLyField }

  TLyField = class(TLyObject)
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
    function GetType: TLyType;
  public
    constructor Create(AField: TField);
    function ToString: string;override;
    procedure SaveValueTo(Value: TLyValue);
    function IsNull: boolean;
    property AsChar: char read GetAsChar;
    property AsString: string read ToString;
    property AsInteger: int64 read GetAsInteger;
    property AsFloat: double read GetAsFloat;
    property AsCurrency: currency read GetAsCurrency;
    property AsTime: TDateTime read GetAsTime;
    property AsBoolean: boolean read GetAsBoolean;
    property Field: TField read FField;
    property FieldName: string read GetName;
    property FieldType: TLyType read GetType;
    property FieldIndex: integer read GetIndex;
  end;

  { TLyFieldType }

  TLyFieldType = class(TLyType)
  private
    procedure MyName(const Param: TLyParam);
    procedure MyType(const Param: TLyParam);
    procedure MyIndex(const Param: TLyParam);
    procedure MyValue(const Param: TLyParam);
    procedure MyIsNull(const Param: TLyParam);
    procedure MyAsString(const Param: TLyParam);
    procedure MyAsChar(const Param: TLyParam);
    procedure MyAsInteger(const Param: TLyParam);
    procedure MyAsFloat(const Param: TLyParam);
    procedure MyAsCurrency(const Param: TLyParam);
    procedure MyAsBoolean(const Param: TLyParam);
    procedure MyAsTime(const Param: TLyParam);
  protected
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
    procedure Setup;override;
  public
    function InheriteClassType: TLyTypeClass;override;
  end;

  { TLyDataSet }

  TLyDataSet = class(TLyObject)
  private
    FDataBase: TLyDataBase;
    FDataSet: TDataSet;
    FFields: TList;
    function GetFieldCount: integer;
    function GetField(Index: integer): TLyField;
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
    constructor Create(ADataSet: TDataSet; ADataBase: TLyDataBase);virtual;
    destructor Destroy;override;
    function ToString: string;override;
    procedure Close;
    procedure Open;
    procedure First;
    procedure Last;
    procedure Prior;
    procedure Next;
    function IndexOf(const Name: string): integer;
    function FindField(const Name: string): TLyField;
    function FieldByName(const Name: string): TLyField;
    property DataSet: TDataSet read FDataSet;
    property Active: boolean read GetActive write SetActive;
    property RecordCount: integer read GetRecordCount;
    property FieldCount: integer read GetFieldCount;
    property Fields[Index: integer]: TLyField read GetField;default;
    property Bof: boolean read GetBof;
    property Eof: boolean read GetEof;
  end;

  { TLyDataSetType }

  TLyDataSetType = class(TLyType)
  private
    FFieldsType: TLyDataSetType;
    FValuesType: TLyDataSetType;
    procedure MyOpen(const Param: TLyParam);
    procedure MyClose(const Param: TLyParam);
    procedure MyFirst(const Param: TLyParam);
    procedure MyLast(const Param: TLyParam);
    procedure MyPrior(const Param: TLyParam);
    procedure MyNext(const Param: TLyParam);
    procedure MyBof(const Param: TLyParam);
    procedure MyEof(const Param: TLyParam);
    procedure MyFindField(const Param: TLyParam);
    procedure MyFieldByName(const Param: TLyParam);
    procedure MyFieldCount(const Param: TLyParam);
    procedure MyRecordCount(const Param: TLyParam);
    procedure MyGetActive(const Param: TLyParam);
    procedure MySetActive(const Param: TLyParam);
    procedure MyFields_Get(const Param: TLyParam);
    procedure MyValues_Get(const Param: TLyParam);
    procedure SetupFieldsType;
    procedure SetupValuesType;
  protected
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function AsString(Obj: pointer): string;override;
    function AsBoolean(Obj: pointer): boolean;override;
    function GetLength(Obj: pointer): int64;override;
    procedure Validate(Obj: pointer);override;
    procedure Setup;override;
  public
    function InheriteClassType: TLyTypeClass;override;
  end;

  { TLyDataBase }

  TLyDataBase = class(TLyObject)
  private
    FConnection: TCustomConnection;
    FDataSets: TList;
    function GetDataSetCount: integer;
    function GetDataSet(Index: integer): TLyDataSet;
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
    property DataSets[Index: integer]: TLyDataSet read GetDataSet;default;
    property LoginPrompt: boolean read GetLoginPrompt write SetLoginPrompt;
  end;

  { TLyDataBaseType }

  TLyDataBaseType = class(TLyType)
  private
    procedure MyOpen(const Param: TLyParam);
    procedure MyClose(const Param: TLyParam);
    procedure MyInTransaction(const Param: TLyParam);
    procedure MyTransact(const Param: TLyParam);
    procedure MyCommit(const Param: TLyParam);
    procedure MyRollback(const Param: TLyParam);
    procedure MyTableNames(const Param: TLyParam);
    procedure MyUserTableNames(const Param: TLyParam);
    procedure MySystemTableNames(const Param: TLyParam);
    procedure MyTableFieldNames(const Param: TLyParam);
    procedure MyProcedureNames(const Param: TLyParam);
    procedure MyGetConnected(const Param: TLyParam);
    procedure MySetConnected(const Param: TLyParam);
    procedure MyGetLoginPrompt(const Param: TLyParam);
    procedure MySetLoginPrompt(const Param: TLyParam);
  protected
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function AsString(Obj: pointer): string;override;
    procedure Validate(Obj: pointer);override;
    procedure Setup;override;
  public
    function InheriteClassType: TLyTypeClass;override;
  end;

var
  my_field: TLyFieldType;
  my_dataset: TLyDataSetType;
  my_database: TLyDataBaseType;

function FieldToType(AType: TFieldType): TLyType;overload;
function FieldToType(AField: TField): TLyType;overload;
function FieldToStr(AField: TField): string;
function FieldToChar(AField: TField): char;
function FieldToInt(AField: TField): int64;
function FieldToBool(AField: TField): boolean;
function FieldToFloat(AField: TField): double;
function FieldToCurr(AField: TField): currency;
function FieldToTime(AField: TField): TDateTime;
procedure GetFieldValue(AField: TField; Value: TLyValue);
procedure Setup;

implementation

uses
  Types, Math;

function FieldToType(AType: TFieldType): TLyType;
begin
  Result := my_nil;
  case AType of
    ftString       : Result := my_string;
    ftSmallint     : Result := my_int;
    ftInteger      : Result := my_int;
    ftWord         : Result := my_int;
    ftBoolean      : Result := my_bool;
    ftFloat        : Result := my_float;
    ftCurrency     : Result := my_curr;
    ftBCD          : Result := my_float;
    ftDate         : Result := my_time;
    ftTime         : Result := my_time;
    ftDateTime     : Result := my_time;
    ftAutoInc      : Result := my_int;
    ftMemo         : Result := my_string;
    ftFmtMemo      : Result := my_string;
    ftFixedChar    : Result := my_string;
    ftWideString   : Result := my_string;
    ftLargeint     : Result := my_int;
    ftGuid         : Result := my_string;
    ftFMTBcd       : Result := my_float;
    ftFixedWideChar: Result := my_string;
    ftWideMemo     : Result := my_string;
  end;
  { ftUnknown, ftBytes, ftVarBytes, ftBlob, ftGraphic,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor,
    ftADT, ftArray, ftReference, ftDataSet, ftOraBlob,
    ftOraClob, ftVariant, ftInterface, ftIDispatch,
    ftTimeStamp }
end;

function FieldToType(AField: TField): TLyType;
begin
  Result := FieldToType(AField.DataType);
end;

function FieldToStr(AField: TField): string;
var
  T: TLyType;
begin
  if AField.IsNull then Result := '' else
  begin
    T := FieldToType(AField);
    if T = my_string then
      Result := AField.AsString else
    if T = my_int then
      Result := IntToStr(AField.AsInteger) else
    if T = my_float then
      Result := FloatToStr(AField.AsFloat) else
    if T = my_curr then
      Result := CurrToStr(AField.AsCurrency) else
    if T = my_bool then
      Result := my_boolean_strs[AField.AsBoolean] else
    if T = my_time then
      Result := DateTimeToStr(AField.AsDateTime) else
      Result := AField.AsString;
  end;
end;

function FieldToChar(AField: TField): char;
var
  T: TLyType;
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
var
  T: TLyType;
begin
  if AField.IsNull then Result := 0 else
  begin
    T := FieldToType(AField);
    if T = my_string then
      Result := StrToInt64(AField.AsString) else
    if T = my_int then
      Result := AField.AsInteger else
    if T = my_float then
      Result := Trunc(AField.AsFloat) else
    if T = my_curr then
      Result := Trunc(AField.AsCurrency) else
    if T = my_bool then
      Result := Ord(AField.AsBoolean) else
    if T = my_time then
      Result := Trunc(AField.AsDateTime) else
      Result := AField.AsInteger;
  end;
end;

function FieldToBool(AField: TField): boolean;
var
  T: TLyType;
begin
  if AField.IsNull then Result := false else
  begin
    T := FieldToType(AField);
    if T = my_string then
      Result := (AField.AsString <> '') else
    if T = my_int then
      Result := (AField.AsInteger <> 0) else
    if T = my_float then
      Result := not IsZero(AField.AsFloat) else
    if T = my_curr then
      Result := (AField.AsCurrency <> 0) else
      Result := AField.AsBoolean;
  end;
end;

function FieldToFloat(AField: TField): double;
var
  T: TLyType;
begin
  if AField.IsNull then Result := 0 else
  begin
    T := FieldToType(AField);
    if T = my_string then
      Result := StrToFloat(AField.AsString) else
    if T = my_int then
      Result := AField.AsInteger else
    if T = my_float then
      Result := AField.AsFloat else
    if T = my_curr then
      Result := AField.AsCurrency else
    if T = my_bool then
      Result := Ord(AField.AsBoolean) else
    if T = my_time then
      Result := AField.AsDateTime else
      Result := AField.AsFloat;
  end;
end;

function FieldToCurr(AField: TField): currency;
var
  T: TLyType;
begin
  if AField.IsNull then Result := 0 else
  begin
    T := FieldToType(AField);
    if T = my_string then
      Result := StrToCurr(AField.AsString) else
    if T = my_int then
      Result := AField.AsInteger else
    if T = my_float then
      Result := AField.AsFloat else
    if T = my_curr then
      Result := AField.AsCurrency else
    if T = my_bool then
      Result := Ord(AField.AsBoolean) else
    if T = my_time then
      Result := AField.AsDateTime else
      Result := AField.AsCurrency;
  end;
end;

function FieldToTime(AField: TField): TDateTime;
var
  T: TLyType;
begin
  if AField.IsNull then Result := 0 else
  begin
    T := FieldToType(AField);
    if T = my_string then
      Result := StrToDateTime(AField.AsString) else
    if T = my_int then
      Result := AField.AsInteger else
    if T = my_float then
      Result := AField.AsFloat else
    if T = my_curr then
      Result := AField.AsCurrency else
      Result := AField.AsDateTime;
  end;
end;

procedure GetFieldValue(AField: TField; Value: TLyValue);
var
  T: TLyType;
begin
  if AField.IsNull then Value.Clear else
  begin
    T := FieldToType(AField);
    if T = my_string then
      Value.AsString := AField.AsString else
    if T = my_int then
      Value.AsInteger := AField.AsInteger else
    if T = my_float then
      Value.AsFloat := AField.AsFloat else
    if T = my_curr then
      Value.AsCurrency := AField.AsCurrency else
    if T = my_bool then
      Value.AsBoolean := AField.AsBoolean else
    if T = my_time then
      Value.AsTime := AField.AsDateTime else
      Value.Clear;
  end;
end;

procedure Setup;
begin
  my_field := TLyFieldType.Create('TField', my_system);
  my_dataset := TLyDataSetType.Create('TDataSet', my_system);
  my_database := TLyDataBaseType.Create('TDataBase', my_system);
end;

{ TLyField }

constructor TLyField.Create(AField: TField);
begin
  FField := AField;
end;

function TLyField.GetAsBoolean: boolean;
begin
  Result := FieldToBool(FField);
end;

function TLyField.GetAsChar: char;
begin
  Result := FieldToChar(FField);
end;

function TLyField.GetAsCurrency: currency;
begin
  Result := FieldToCurr(FField);
end;

function TLyField.GetAsFloat: double;
begin
  Result := FieldToFloat(FField);
end;

function TLyField.GetAsInteger: int64;
begin
  Result := FieldToInt(FField);
end;

function TLyField.GetAsTime: TDateTime;
begin
  Result := FieldToTime(FField);
end;

function TLyField.GetIndex: integer;
begin
  Result := FField.Index;
end;

function TLyField.GetName: string;
begin
  Result := FField.FieldName;
end;

function TLyField.GetType: TLyType;
begin
  Result := FieldToType(FField);
end;

function TLyField.IsNull: boolean;
begin
  Result := FField.IsNull;
end;

procedure TLyField.SaveValueTo(Value: TLyValue);
begin
  GetFieldValue(FField, Value);
end;

function TLyField.ToString: string;
begin
  Result := FieldToStr(FField);
end;

{ TLyFieldType }

procedure TLyFieldType.MyAsBoolean(const Param: TLyParam);
var
  F: TLyField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsBoolean := F.AsBoolean;
end;

procedure TLyFieldType.MyAsChar(const Param: TLyParam);
var
  F: TLyField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsChar := F.AsChar;
end;

procedure TLyFieldType.MyAsCurrency(const Param: TLyParam);
var
  F: TLyField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsCurrency := F.AsCurrency;
end;

procedure TLyFieldType.MyAsFloat(const Param: TLyParam);
var
  F: TLyField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsFloat := F.AsFloat;
end;

procedure TLyFieldType.MyAsInteger(const Param: TLyParam);
var
  F: TLyField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsInteger := F.AsInteger;
end;

procedure TLyFieldType.MyAsString(const Param: TLyParam);
var
  F: TLyField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsString := F.ToString;
end;

procedure TLyFieldType.MyAsTime(const Param: TLyParam);
var
  F: TLyField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsTime := F.AsTime;
end;

procedure TLyFieldType.MyIndex(const Param: TLyParam);
var
  F: TLyField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsInteger := F.FieldIndex;
end;

procedure TLyFieldType.MyIsNull(const Param: TLyParam);
var
  F: TLyField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsBoolean := F.IsNull;
end;

procedure TLyFieldType.MyName(const Param: TLyParam);
var
  F: TLyField;
begin
  if Param.GetSelf(F) then
    Param.Result.AsString := F.FieldName;
end;

procedure TLyFieldType.MyType(const Param: TLyParam);
var
  F: TLyField;
begin
  if Param.GetSelf(F) then
    Param.Result.Assign(my_type, F.FieldType);
end;

procedure TLyFieldType.MyValue(const Param: TLyParam);
var
  F: TLyField;
begin
  if Param.GetSelf(F) then
    GetFieldValue(F.FField, Param.Result);
end;

procedure TLyFieldType.Setup;
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

function TLyFieldType.AsBoolean(Obj: pointer): boolean;
begin
  Result := (Obj <> nil) and TLyField(Obj).AsBoolean;
end;

function TLyFieldType.AsChar(Obj: pointer): char;
begin
  Result := TLyField(Obj).AsChar;
end;

function TLyFieldType.AsFloat(Obj: pointer): double;
begin
  Result := TLyField(Obj).AsFloat;
end;

function TLyFieldType.AsInteger(Obj: pointer): int64;
begin
  Result := TLyField(Obj).AsInteger;
end;

function TLyFieldType.AsCurrency(Obj: pointer): currency;
begin
  Result := TLyField(Obj).AsCurrency;
end;

function TLyFieldType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyField(Obj).AsString else
    Result := '';
end;

function TLyFieldType.AsTime(Obj: pointer): TDateTime;
begin
  Result := TLyField(Obj).AsTime;
end;

function TLyFieldType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyField(Obj).DecRefcount else
    Result := 0;
end;

function TLyFieldType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyField(Obj).IncRefcount else
    Result := 0;
end;

function TLyFieldType.InheriteClassType: TLyTypeClass;
begin
  Result := nil;
end;

procedure TLyFieldType.Validate(Obj: pointer);
begin
  inherited;
  Check(TLyField(Obj).FField <> nil, 'field has been released');
end;

{ TLyDataSet }

procedure TLyDataSet.ClearFields;
var
  I: integer;
begin
  for I := GetFieldCount - 1 downto 0 do
    DeleteField(I);
end;

procedure TLyDataSet.Close;
begin
  ClearFields;
  FDataSet.Close;
end;

constructor TLyDataSet.Create(ADataSet: TDataSet; ADataBase: TLyDataBase);
begin
  FDataBase := ADataBase;
  if FDataBase <> nil then
    FDataBase.FDataSets.Add(Self);
  FDataSet := ADataSet;
  FFields := TList.Create;
  SetupFields;
end;

procedure TLyDataSet.DeleteField(Index: integer);
var
  F: TLyField;
begin
  F := GetField(Index);
  FFields.Delete(Index);
  F.FField := nil;
  F.DecRefcount;
end;

destructor TLyDataSet.Destroy;
begin
  if FDataBase <> nil then
    FDataBase.FDataSets.Remove(Self);
  ClearFields;
  FreeAndNil(FFields);
  FreeAndNil(FDataSet);
  inherited;
end;

function TLyDataSet.FieldByName(const Name: string): TLyField;
begin
  Result := FindField(Name);
  if Result = nil then
    Throw('field not found: %s', [Name]);
end;

function TLyDataSet.FindField(const Name: string): TLyField;
var
  I: integer;
begin
  I := IndexOf(Name);
  if I >= 0 then
    Result := GetField(I) else
    Result := nil;
end;

procedure TLyDataSet.First;
begin
  FDataSet.First;
end;

function TLyDataSet.GetActive: boolean;
begin
  Result := FDataSet.Active;
end;

function TLyDataSet.GetBof: boolean;
begin
  Result := FDataSet.Bof;
end;

function TLyDataSet.GetEof: boolean;
begin
  Result := FDataSet.Eof;
end;

function TLyDataSet.GetField(Index: integer): TLyField;
begin
  Result := TLyField(FFields[Index]);
end;

function TLyDataSet.GetFieldCount: integer;
begin
  Result := FFields.Count;
end;

function TLyDataSet.GetRecordCount: integer;
begin
  Result := FDataSet.RecordCount;
end;

function TLyDataSet.IndexOf(const Name: string): integer;
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

procedure TLyDataSet.Last;
begin
  FDataSet.Last;
end;

procedure TLyDataSet.Next;
begin
  FDataSet.Next;
end;

procedure TLyDataSet.OnClose(Sender: TObject);
begin
  ClearFields;
end;

procedure TLyDataSet.Open;
begin
  if not Active then
  begin
    ClearFields;
    FDataSet.Open;
    SetupFields;
  end;
end;

procedure TLyDataSet.Prior;
begin
  FDataSet.Prior;
end;

procedure TLyDataSet.SetActive(Value: boolean);
begin
  if Value then Open else Close;
end;

procedure TLyDataSet.SetupFields;
var
  I: integer;
  F: TLyField;
begin
  ClearFields;
  if FDataSet.Active then
    for I := 0 to FDataSet.FieldCount - 1 do
    begin
      F := TLyField.Create(FDataSet.Fields[I]);
      F.IncRefcount;
      FFields.Add(F);
    end;
end;

function TLyDataSet.ToString: string;
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

{ TLyDataSetType }

procedure TLyDataSetType.MyFieldByName(const Param: TLyParam);
var
  S: TLyDataSet;
  F: TLyField;
begin
  if Param.GetSelf(S) then
  begin
    F := S.FieldByName(Param[1].AsString);
    Param.Result.Assign(my_field, F);
  end;
end;

procedure TLyDataSetType.MyFieldCount(const Param: TLyParam);
var
  S: TLyDataSet;
begin
  if Param.GetSelf(S) then
    Param.Result.AsInteger := S.FieldCount;
end;

procedure TLyDataSetType.MyBof(const Param: TLyParam);
var
  S: TLyDataSet;
begin
  if Param.GetSelf(S) then
    Param.Result.AsBoolean := S.Bof;
end;

procedure TLyDataSetType.MyClose(const Param: TLyParam);
var
  S: TLyDataSet;
begin
  if Param.GetSelf(S) then S.Close;
end;

procedure TLyDataSetType.MyEof(const Param: TLyParam);
var
  S: TLyDataSet;
begin
  if Param.GetSelf(S) then
    Param.Result.AsBoolean := S.Eof;
end;

procedure TLyDataSetType.MyFindField(const Param: TLyParam);
var
  S: TLyDataSet;
  F: TLyField;
begin
  if Param.GetSelf(S) then
  begin
    F := S.FindField(Param[1].AsString);
    Param.Result.Assign(my_field, F);
  end;
end;

procedure TLyDataSetType.MyFirst(const Param: TLyParam);
var
  S: TLyDataSet;
begin
  if Param.GetSelf(S) then S.First;
end;

procedure TLyDataSetType.MyGetActive(const Param: TLyParam);
var
  S: TLyDataSet;
begin
  if Param.GetSelf(S) then
    Param.Result.AsBoolean := S.Active;
end;

procedure TLyDataSetType.MyFields_Get(const Param: TLyParam);
var
  S: TLyDataSet;
  X: TLyValue;
  F: TLyField;
  T: TLyType;
begin
  if Param.GetSelf(S) then
  begin
    X := Param[1];
    T := X.VType;
    if (T = my_string) or (T = my_char) or (T = my_int) then
    begin
      if X.VType = my_int then
        F := S.Fields[X.AsInteger] else
        F := S.FieldByName(X.AsString);
      Param.Result.Assign(my_field, F);
    end
    else Param.Error('invalid field index: %s', [X.VType.Name])
  end;
end;

procedure TLyDataSetType.MyValues_Get(const Param: TLyParam);
var
  S: TLyDataSet;
  X: TLyValue;
  F: TLyField;
  T: TLyType;
begin
  if Param.GetSelf(S) then
  begin
    X := Param[1];
    T := X.VType;
    if (T = my_string) or (T = my_char) or (T = my_int) then
    begin
      if X.VType = my_int then
        F := S.Fields[X.AsInteger] else
        F := S.FieldByName(X.AsString);
      F.SaveValueTo(Param.Result);
    end
    else Param.Error('invalid field index: %s', [X.VType.Name])
  end;
end;

procedure TLyDataSetType.MyLast(const Param: TLyParam);
var
  S: TLyDataSet;
begin
  if Param.GetSelf(S) then S.Last;
end;

procedure TLyDataSetType.MyNext(const Param: TLyParam);
var
  S: TLyDataSet;
begin
  if Param.GetSelf(S) then S.Next;
end;

procedure TLyDataSetType.MyOpen(const Param: TLyParam);
var
  S: TLyDataSet;
begin
  if Param.GetSelf(S) then S.Open;
end;

procedure TLyDataSetType.MyPrior(const Param: TLyParam);
var
  S: TLyDataSet;
begin
  if Param.GetSelf(S) then S.Prior;
end;

procedure TLyDataSetType.MyRecordCount(const Param: TLyParam);
var
  S: TLyDataSet;
begin
  if Param.GetSelf(S) then
    Param.Result.AsInteger := S.RecordCount;
end;

procedure TLyDataSetType.MySetActive(const Param: TLyParam);
var
  S: TLyDataSet;
begin
  if Param.GetSelf(S) then
    S.Active := Param[1].AsBoolean;
end;

procedure TLyDataSetType.Setup;
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

  FFieldsType := Branch('Fields') as TLyDataSetType;
  FFieldsType.SetupFieldsType;

  FValuesType := Branch('Values') as TLyDataSetType;
  FValuesType.SetupValuesType;

  inherited;
end;

procedure TLyDataSetType.SetupFieldsType;
begin
  Define(my_field, ['Index'], [my_variant],
    {$IFDEF FPC}@{$ENDIF}MyFields_Get);
end;

procedure TLyDataSetType.SetupValuesType;
begin
  Define(my_variant, ['Index'], [my_variant],
    {$IFDEF FPC}@{$ENDIF}MyValues_Get);
end;

function TLyDataSetType.AsBoolean(Obj: pointer): boolean;
begin
  Result := (Obj <> nil) and TLyDataSet(Obj).Active;
end;

function TLyDataSetType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyDataSet(Obj).ToString else
    Result := '';
end;

function TLyDataSetType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyDataSet(Obj).DecRefcount else
    Result := 0;
end;

function TLyDataSetType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyDataSet(Obj).IncRefcount else
    Result := 0;
end;

function TLyDataSetType.InheriteClassType: TLyTypeClass;
begin
  Result := nil;
end;

function TLyDataSetType.GetLength(Obj: pointer): int64;
begin
  if Obj <> nil then
    Result := TLyDataSet(Obj).FieldCount else
    Result := 0;
end;

procedure TLyDataSetType.Validate(Obj: pointer);
begin
  inherited;
  Check(TLyDataSet(Obj).FDataSet <> nil, 'dataset has been released');
end;

{ TLyDataBase }

procedure TLyDataBase.ClearDataSets;
var
  I: integer;
begin
  for I := GetDataSetCount - 1 downto 0 do
    DeleteDataSet(I);
end;

procedure TLyDataBase.Close;
begin
  CloseDataSets;
  FConnection.Close;
end;

procedure TLyDataBase.CloseDataSets;
var
  I: integer;
begin
  for I := GetDataSetCount - 1 downto 0 do
  begin
    GetDataSet(I).ClearFields;
    GetDataSet(I).Close;
  end;
end;

procedure TLyDataBase.Commit;
begin
  { nothing }
end;

procedure TLyDataBase.CommitAndTransact;
begin
  CommitRetaining;
  Transact;
end;

procedure TLyDataBase.CommitRetaining;
begin
  if InTransaction then Commit;
end;

constructor TLyDataBase.Create(AConnection: TCustomConnection);
begin
  FDataSets := TList.Create;
  FConnection := AConnection;
  FConnection.BeforeDisconnect := {$IFDEF FPC}@{$ENDIF}Disconnected;
  FConnection.AfterDisconnect := {$IFDEF FPC}@{$ENDIF}Disconnected;
end;

procedure TLyDataBase.DeleteDataSet(Index: integer);
var
  D: TLyDataSet;
begin
  D := GetDataSet(Index);
  FDataSets.Delete(Index);
  D.FDataBase := nil;
  D.ClearFields;
  D.Close;
  FreeAndNil(D.FDataSet);
end;

destructor TLyDataBase.Destroy;
begin
  Close;
  ClearDataSets;
  FreeAndNil(FDataSets);
  FreeAndNIl(FConnection);
  inherited;
end;

procedure TLyDataBase.Disconnected(Sender: TObject);
begin
  CloseDataSets;
end;

function TLyDataBase.GetConnected: boolean;
begin
  Result := FConnection.Connected;
end;

function TLyDataBase.GetDataSet(Index: integer): TLyDataSet;
begin
  Result := TLyDataSet(FDataSets[Index]);
end;

function TLyDataBase.GetDataSetCount: integer;
begin
  if FDataSets <> nil then
    Result := FDataSets.Count else
    Result := 0;
end;

function TLyDataBase.GetLoginPrompt: boolean;
begin
  Result := FConnection.LoginPrompt;
end;

procedure TLyDataBase.GetProcedureNames(List: TStrings);
begin
  { nothing }
end;

procedure TLyDataBase.GetFieldNames(List: TStrings; const Table: string);
begin
  { nothing }
end;

procedure TLyDataBase.GetTableNames(List: TStrings; SystemTables: boolean);
begin
  { nothing }
end;

function TLyDataBase.InTransaction: boolean;
begin
  Result := false;
end;

procedure TLyDataBase.Open;
begin
  FConnection.Open;
end;

procedure TLyDataBase.Rollback;
begin
  { nothing }
end;

procedure TLyDataBase.RollbackAndTransact;
begin
  RollbackRetaining;
  Transact;
end;

procedure TLyDataBase.RollbackRetaining;
begin
  if InTransaction then Rollback;
end;

procedure TLyDataBase.SetConnected(Value: boolean);
begin
  if Value then Open else Close;
end;

procedure TLyDataBase.SetLoginPrompt(Value: boolean);
begin
  FConnection.LoginPrompt := Value;
end;

procedure TLyDataBase.Transact;
begin
  { nothing }
end;

{ TLyDataBaseType }

procedure TLyDataBaseType.MyClose(const Param: TLyParam);
var
  C: TLyDataBase;
begin
  if Param.GetSelf(C) then C.Close;
end;

procedure TLyDataBaseType.MyCommit(const Param: TLyParam);
var
  C: TLyDataBase;
begin
  if Param.GetSelf(C) then C.Commit;
end;

procedure TLyDataBaseType.MyGetConnected(const Param: TLyParam);
var
  C: TLyDataBase;
begin
  if Param.GetSelf(C) then
    Param.Result.AsBoolean := C.Connected;
end;

procedure TLyDataBaseType.MyGetLoginPrompt(const Param: TLyParam);
var
  C: TLyDataBase;
begin
  if Param.GetSelf(C) then
    Param.Result.AsBoolean := C.LoginPrompt;
end;

procedure TLyDataBaseType.MyInTransaction(const Param: TLyParam);
var
  C: TLyDataBase;
begin
  if Param.GetSelf(C) then
    Param.Result.AsBoolean := C.InTransaction;
end;

procedure TLyDataBaseType.MyOpen(const Param: TLyParam);
var
  C: TLyDataBase;
begin
  if Param.GetSelf(C) then C.Open;
end;

procedure TLyDataBaseType.MyProcedureNames(const Param: TLyParam);
var
  C: TLyDataBase;
  L: TLyList;
  T: TStrings;
begin
  if Param.GetSelf(C) then
  begin
    L := TLyList.Create;
    Param.Result.Assign(my_array, L);
    T := TStringList.Create;
    try
      C.GetProcedureNames(T);
      L.Add(T);
    finally
      T.Free;
    end;
  end;
end;

procedure TLyDataBaseType.MyRollback(const Param: TLyParam);
var
  C: TLyDataBase;
begin
  if Param.GetSelf(C) then C.Rollback;
end;

procedure TLyDataBaseType.MySetConnected(const Param: TLyParam);
var
  C: TLyDataBase;
begin
  if Param.GetSelf(C) then
    C.Connected := Param[1].AsBoolean;
end;

procedure TLyDataBaseType.MySetLoginPrompt(const Param: TLyParam);
var
  C: TLyDataBase;
begin
  if Param.GetSelf(C) then
    C.LoginPrompt := Param[1].AsBoolean;
end;

procedure TLyDataBaseType.MySystemTableNames(const Param: TLyParam);
var
  C: TLyDataBase;
  L: TLyList;
  T, U: TStrings;
  I, X: integer;
begin
  if Param.GetSelf(C) then
  begin
    L := TLyList.Create;
    Param.Result.Assign(my_array, L);
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
      L.Add(T);
    finally
      U.Free;
      T.Free;
    end;
  end;
end;

procedure TLyDataBaseType.MyTableFieldNames(const Param: TLyParam);
var
  C: TLyDataBase;
  L: TLyList;
  T: TStrings;
begin
  if Param.GetSelf(C) then
  begin
    L := TLyList.Create;
    Param.Result.Assign(my_array, L);
    T := TStringList.Create;
    try
      C.GetFieldNames(T, Param[1].AsString);
      L.Add(T);
    finally
      T.Free;
    end;
  end;
end;

procedure TLyDataBaseType.MyTableNames(const Param: TLyParam);
var
  C: TLyDataBase;
  L: TLyList;
  T: TStrings;
begin
  if Param.GetSelf(C) then
  begin
    L := TLyList.Create;
    Param.Result.Assign(my_array, L);
    T := TStringList.Create;
    try
      C.GetTableNames(T, true);
      L.Add(T);
    finally
      T.Free;
    end;
  end;
end;

procedure TLyDataBaseType.MyTransact(const Param: TLyParam);
var
  C: TLyDataBase;
begin
  if Param.GetSelf(C) then C.Transact;
end;

procedure TLyDataBaseType.MyUserTableNames(const Param: TLyParam);
var
  C: TLyDataBase;
  L: TLyList;
  T: TStrings;
begin
  if Param.GetSelf(C) then
  begin
    L := TLyList.Create;
    Param.Result.Assign(my_array, L);
    T := TStringList.Create;
    try
      C.GetTableNames(T, false);
      L.Add(T);
    finally
      T.Free;
    end;
  end;
end;

procedure TLyDataBaseType.Setup;
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

function TLyDataBaseType.AsString(Obj: pointer): string;
begin
  Result := '';
end;

function TLyDataBaseType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyDataBase(Obj).DecRefcount else
    Result := 0;
end;

function TLyDataBaseType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyDataBase(Obj).IncRefcount else
    Result := 0;
end;

function TLyDataBaseType.InheriteClassType: TLyTypeClass;
begin
  Result := nil;
end;

procedure TLyDataBaseType.Validate(Obj: pointer);
begin
  inherited;
  Check(TLyDataBase(Obj).FConnection <> nil, 'invalid database connection');
end;

end.
