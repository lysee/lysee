{==============================================================================}
{        UNIT: lysee_classes                                                   }
{ DESCRIPTION: lysee's classes module functions                                }
{   COPYRIGHT: Copyright (c) 2016-2016, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2016/12/18                                                      }
{    MODIFIED: 2017/02/19                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_classes;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, basic, lysee;

type

  { TLyseeStringList }

  TLyseeStringList = class(TLyseeGarbage)
  private
    FStringList: TStringList;
    function GetValue(const Name: string): string;
    procedure SetValue(const Name, Value: string);
    function GetCount: integer;
    procedure SetCount(Value: integer);
  protected
    procedure MarkForSurvive;override;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure Assign(Source: TLyseeStringList);
    procedure Clear;override;
    procedure ClearObjects;
    procedure Delete(Index: integer);
    procedure DeleteLast;
    procedure DeleteEmptyLines;
    procedure DeleteMatchedLines(const Patten: string);
    procedure Remove(const S: string);
    procedure RemoveByName(const S: string);
    procedure Unique;
    procedure Pack;
    procedure Trim;
    procedure TrimAll;
    procedure TrimLeft;
    procedure TrimRight;
    procedure Insert(Index: Integer; const S: string; V: TLyseeValue);
    function Add(const S: string; V: TLyseeValue): integer;
    procedure AddHashed(Hashed: TLyseeHash);
    procedure AddList(List: TLyseeList);
    function MatchStrs(const S1, S2: string): boolean;
    function IndexOfValue(const S: string): integer;
    function Copy(Index, ItemCount: integer): TLyseeStringList;
    function CopyLeft(ItemCount: integer): TLyseeStringList;
    function CopyRight(ItemCount: integer): TLyseeStringList;
    function SelectMatched(const Patten: string): TLyseeStringList;
    procedure Rename(const Name, NewName: string);
    procedure Reverse;
    function AsString: string;override;
    function AsLyseeList: TLyseeList;
    function AsLyseeHash: TLyseeHash;
    function IsEmpty: boolean;
    property Count: integer read GetCount write SetCount;
    property Values[const Name: string]: string read GetValue write SetValue;
    property StringList: TStringList read FStringList;
  end;

  { TLyseeStringListGenerate }

  TLyseeStringListGenerate = class(TLyseeGenerate)
  private
    FL: TStrings;
    FIndex: integer;
  public
    constructor CreateIn(const List: TStrings);
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLyseeStringListType }

  TLyseeStringListType = class(TLyseeType)
  protected
    procedure MyCreate(const Param: TLyseeParam);
    procedure MyIsEmpty(const Param: TLyseeParam);
    procedure MyLoadFromFile(const Param: TLyseeParam);
    procedure MySaveToFile(const Param: TLyseeParam);
    procedure MyBeginUpdate(const Param: TLyseeParam);
    procedure MyEndUpdate(const Param: TLyseeParam);
    procedure MyAssign(const Param: TLyseeParam);
    procedure MyClear(const Param: TLyseeParam);
    procedure MyClearObjects(const Param: TLyseeParam);
    procedure MyDelete(const Param: TLyseeParam);
    procedure MyDeleteLast(const Param: TLyseeParam);
    procedure MyDeleteEmptyLines(const Param: TLyseeParam);
    procedure MyRemove(const Param: TLyseeParam);
    procedure MyRemoveByName(const Param: TLyseeParam);
    procedure MyAdd(const Param: TLyseeParam);
    procedure MyAddObject(const Param: TLyseeParam);
    procedure MyInsert(const Param: TLyseeParam);
    procedure MyInsertObject(const Param: TLyseeParam);
    procedure MyExchange(const Param: TLyseeParam);
    procedure MyMove(const Param: TLyseeParam);
    procedure MySort(const Param: TLyseeParam);
    procedure MyUnique(const Param: TLyseeParam);
    procedure MyPack(const Param: TLyseeParam);
    procedure MyIndexOf(const Param: TLyseeParam);
    procedure MyIndexOfObject(const Param: TLyseeParam);
    procedure MyIndexOfName(const Param: TLyseeParam);
    procedure MyIndexOfValue(const Param: TLyseeParam);
    procedure MyRename(const Param: TLyseeParam);
    procedure MyEquals(const Param: TLyseeParam);
    procedure MyTrim(const Param: TLyseeParam);
    procedure MyTrimAll(const Param: TLyseeParam);
    procedure MyTrimLeft(const Param: TLyseeParam);
    procedure MyTrimRight(const Param: TLyseeParam);
    procedure MyCopy(const Param: TLyseeParam);
    procedure MyLeft(const Param: TLyseeParam);
    procedure MyRight(const Param: TLyseeParam);
    procedure MySelectMatchedLines(const Param: TLyseeParam);
    procedure MyDeleteMatchedLines(const Param: TLyseeParam);
    procedure MyReverse(const Param: TLyseeParam);
    procedure MyGetCount(const Param: TLyseeParam);
    procedure MySetCount(const Param: TLyseeParam);
    procedure MyGet(const Param: TLyseeParam);
    procedure MySet(const Param: TLyseeParam);
    procedure MyNames(const Param: TLyseeParam);
    procedure MyGetName(const Param: TLyseeParam);
    procedure MyGetValue(const Param: TLyseeParam);
    procedure MySetValue(const Param: TLyseeParam);
    procedure MyGetObject(const Param: TLyseeParam);
    procedure MySetObject(const Param: TLyseeParam);
    procedure MyValueFromIndex(const Param: TLyseeParam);
    procedure MyGetText(const Param: TLyseeParam);
    procedure MySetText(const Param: TLyseeParam);
    procedure MyGetCommaText(const Param: TLyseeParam);
    procedure MySetCommaText(const Param: TLyseeParam);
    procedure MyGetDelimiter(const Param: TLyseeParam);
    procedure MySetDelimiter(const Param: TLyseeParam);
    procedure MyGetDelimitedText(const Param: TLyseeParam);
    procedure MySetDelimitedText(const Param: TLyseeParam);
    procedure MyGetLineBreak(const Param: TLyseeParam);
    procedure MySetLineBreak(const Param: TLyseeParam);
    procedure MyGetNameValueSeparator(const Param: TLyseeParam);
    procedure MySetNameValueSeparator(const Param: TLyseeParam);
    procedure MyGetQuoteChar(const Param: TLyseeParam);
    procedure MySetQuoteChar(const Param: TLyseeParam);
    procedure MyGetStrictDelimiter(const Param: TLyseeParam);
    procedure MySetStrictDelimiter(const Param: TLyseeParam);
    procedure MyGetWriteBOM(const Param: TLyseeParam);
    procedure MySetWriteBOM(const Param: TLyseeParam);
    procedure MyGetSorted(const Param: TLyseeParam);
    procedure MySetSorted(const Param: TLyseeParam);
    procedure MyGetCaseSensitive(const Param: TLyseeParam);
    procedure MySetCaseSensitive(const Param: TLyseeParam);
    procedure MyGetFirst(const Param: TLyseeParam);
    procedure MySetFirst(const Param: TLyseeParam);
    procedure MyGetLast(const Param: TLyseeParam);
    procedure MySetLast(const Param: TLyseeParam);
    procedure Setup;override;
  public
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function AsString(Obj: pointer): string;override;
    function Generate(Obj: pointer): TLyseeGenerate;override;
    function GetLength(Obj: pointer): int64;override;
    function Clear(Obj: pointer): boolean;override;
    function Add(Obj: pointer; Value: TLyseeValue): integer;override;
    function ConvertTo(Value: TLyseeValue; T: TLyseeType): boolean;override;
    procedure Convert(Value: TLyseeValue);override;
  end;

  { TLyseeListType }

  TLyseeListType = class(TLyseeArrayType)
  public
    function ConvertTo(Value: TLyseeValue; T: TLyseeType): boolean;override;
    procedure Convert(Value: TLyseeValue);override;
  end;

  { TLyseeClassesModule }

  TLyseeClassesModule = class(TLyseeModule)
  private
    procedure DoSetup(Sender: TObject);
  public
    constructor Create(const AName: string);override;
  end;

var
  my_classes: TLyseeClassesModule;
  my_strlist: TLyseeStringListType;
  my_list: TLyseeListType;

implementation

uses
  Math;

procedure strlist_shi_variant(V1, V2: TLyseeValue);
var
  L: TLyseeStringList;
begin
  if V1.GetSelf(L) then
    L.StringList.Add(V2.AsString);
end;

procedure strlist_fill_variant(V1, V2: TLyseeValue);
var
  L: TLyseeStringList;
  G: TLyseeGenerate;
begin
  if V1.GetSelf(L) then
  begin
    G := GetGenerate(V2);
    if G <> nil then
    try
      while G.GetNext do
        L.StringList.Add(G.AsString);
    finally
      G.Free;
    end;
  end;
end;

{ TLyseeStringList }

procedure TLyseeStringList.AddHashed(Hashed: TLyseeHash);
var
  L: TList;
  I: integer;
  V: TLyseeHashItem;
begin
  if Hashed <> nil then
  begin
    L := Hashed.SetupItemList;
    BeginUpdate;
    try
      for I := 0 to L.Count - 1 do
      begin
        V := TLyseeHashItem(L[I]);
        Add(V.Key, V);
      end;
    finally
      EndUpdate;
      L.Free;
    end;
  end;
end;

procedure TLyseeStringList.AddList(List: TLyseeList);
var
  I: integer;
begin
  if List <> nil then
  begin
    BeginUpdate;
    try
      for I := 0 to List.Count - 1 do
        FStringList.Add(List[I].AsString);
    finally
      EndUpdate;
    end;
  end;
end;

function TLyseeStringList.Add(const S: string; V: TLyseeValue): integer;
var
  T: TLyseeValue;
begin
  if (V <> nil) and not V.IsNil then
  begin
    T := TLyseeValue.Create;
    Result := FStringList.AddObject(S, T);
    T.SetValue(V);
  end
  else Result := FStringList.Add(S);
end;

function TLyseeStringList.AsLyseeHash: TLyseeHash;
var
  I: integer;
begin
  Result := TLyseeHash.Create;
  for I := 0 to FStringList.Count - 1 do
    Result.Add(FStringList[I]).SetValue(TLyseeValue(FStringList.Objects[I]));
end;

function TLyseeStringList.AsLyseeList: TLyseeList;
var
  I: integer;
begin
  Result := TLyseeList.Create;
  for I := 0 to FStringList.Count - 1 do
    Result.Add.AsString := FStringList[I];
end;

procedure TLyseeStringList.Assign(Source: TLyseeStringList);
var
  I: integer;
begin
  if Self <> Source then
  begin
    BeginUpdate;
    try
      Clear;
      if Source <> nil then
        for I := 0 to Source.FStringList.Count - 1 do
          Add(Source.FStringList[I],
            TLyseeValue(Source.FStringList.Objects[I]));
    finally
      EndUpdate;
    end;
  end;
end;

function TLyseeStringList.AsString: string;
begin
  Result := FStringList.Text;
end;

procedure TLyseeStringList.BeginUpdate;
begin
  FStringList.BeginUpdate;
end;

procedure TLyseeStringList.Clear;
var
  I: integer;
begin
  BeginUpdate;
  try
    for I := FStringList.Count - 1 downto 0 do
      Delete(I);
    FStringList.Clear;
  finally
    EndUpdate;
  end;
end;

procedure TLyseeStringList.ClearObjects;
var
  I: integer;
  V: TLyseeValue;
begin
  BeginUpdate;
  try
    for I := FStringList.Count - 1 downto 0 do
      if FStringList.Objects[I] <> nil then
      begin
        V := TLyseeValue(FStringList.Objects[I]);
        FStringList.Objects[I] := nil;
        V.Free;
      end;
  finally
    EndUpdate;
  end;
end;

function TLyseeStringList.Copy(Index, ItemCount: integer): TLyseeStringList;
var
  I: integer;
begin
  Result := TLyseeStringList.Create;
  if Index < 0 then
  begin
    Inc(ItemCount, Index);
    Index := 0;
  end;
  ItemCount := Max(0, Min(FStringList.Count - Index, ItemCount));
  for I := 1 to ItemCount do
  begin
    Result.Add(FStringList[Index], TLyseeValue(FStringList.Objects[Index]));
    Inc(Index);
  end;
end;

constructor TLyseeStringList.Create;
begin
  inherited;
  FStringList := TStringList.Create;
end;

procedure TLyseeStringList.Delete(Index: integer);
var
  V: TLyseeValue;
begin
  V := TLyseeValue(FStringList.Objects[Index]);
  FStringList.Delete(Index);
  if V <> nil then V.Free;
end;

procedure TLyseeStringList.DeleteEmptyLines;
var
  I: integer;
begin
  BeginUpdate;
  try
    for I := FStringList.Count - 1 downto 0 do
      if FStringList[I] = '' then
        Delete(I);
  finally
    EndUpdate;
  end;
end;

procedure TLyseeStringList.DeleteLast;
begin
  Delete(FStringList.Count - 1);
end;

destructor TLyseeStringList.Destroy;
begin
  Clear;
  FreeAndNil(FStringList);
  inherited;
end;

procedure TLyseeStringList.EndUpdate;
begin
  FStringList.EndUpdate;
end;

function TLyseeStringList.GetCount: integer;
begin
  Result := FStringList.Count;
end;

function TLyseeStringList.GetValue(const Name: string): string;
begin
  Result := FStringList.Values[Name];
end;

function TLyseeStringList.IndexOfValue(const S: string): integer;
var
  I: integer;
begin
  for I := 0 to FStringList.Count - 1 do
    if MatchStrs(S, FStringList.ValueFromIndex[I]) then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

procedure TLyseeStringList.Insert(Index: Integer; const S: string; V: TLyseeValue);
var
  T: TLyseeValue;
begin
  if (V <> nil) and not V.IsNil then
  begin
    T := TLyseeValue.Create;
    FStringList.InsertObject(Index, S, T);
    T.SetValue(V);
  end
  else FStringList.Insert(Index, S);
end;

function TLyseeStringList.IsEmpty: boolean;
begin
  Result := (FStringList.Count = 0);
end;

procedure TLyseeStringList.LoadFromFile(const FileName: string);
begin
  BeginUpdate;
  try
    Clear;
    FStringList.LoadFromFile(FileName);
  finally
    EndUpdate;
  end;
end;

function TLyseeStringList.CopyLeft(ItemCount: integer): TLyseeStringList;
begin
  Result := Copy(0, ItemCount);
end;

procedure TLyseeStringList.MarkForSurvive;
var
  I: integer;
  V: TLyseeValue;
begin
  if (Self <> nil) and not Survived then
  begin
    Survived := true;
    for I := 0 to FStringList.Count - 1 do
      if FStringList.Objects[I] <> nil then
      begin
        V := TLyseeValue(FStringList.Objects[I]);
        V.MarkForSurvive;
      end;
  end;
end;

function TLyseeStringList.MatchStrs(const S1, S2: string): boolean;
begin
  if FStringList.CaseSensitive then
    Result := (SysUtils.CompareStr(S1, S2) = 0) else
    Result := SysUtils.SameText(S1, S2);
end;

procedure TLyseeStringList.Pack;
begin
  BeginUpdate;
  try
    TrimRight;
    DeleteEmptyLines;
  finally
    EndUpdate;
  end;
end;

procedure TLyseeStringList.Remove(const S: string);
var
  I: integer;
begin
  I := FStringList.IndexOf(S);
  if I >= 0 then Delete(I);
end;

procedure TLyseeStringList.RemoveByName(const S: string);
var
  I: integer;
begin
  I := FStringList.IndexOfName(S);
  if I >= 0 then Delete(I);
end;

procedure TLyseeStringList.Rename(const Name, NewName: string);
var
  I, X: integer;
  V: string;
begin
  if not MatchStrs(Name, NewName) then
  begin
    I := FStringList.IndexOfName(Name);
    if I >= 0 then
    begin
      BeginUpdate;
      try
        X := FStringList.IndexOfName(NewName);
        if X >= 0 then
        begin
          Delete(X);
          if X < I then Dec(I);
        end;
        V := FStringList.ValueFromIndex[I];
        FStringList[I] := NewName + FStringList.NameValueSeparator + V;
      finally
        EndUpdate;
      end;
    end;
  end;
end;

procedure TLyseeStringList.Reverse;
var
  L, R: integer;
begin
  FStringList.Sorted := false;
  L := 0;
  R := FStringList.Count - 1;
  while L < R do
  begin
    FStringList.Exchange(L, R);
    Inc(L);
    Dec(R);
  end;
end;

function TLyseeStringList.CopyRight(ItemCount: integer): TLyseeStringList;
begin
  Result := Copy(FStringList.Count - ItemCount, ItemCount);
end;

procedure TLyseeStringList.DeleteMatchedLines(const Patten: string);
var
  I: integer;
begin
  BeginUpdate;
  try
    for I := FStringList.Count - 1 downto 0 do
      if MatchPatten(FStringList[I], Patten) then
        Delete(I);
  finally
    EndUpdate;
  end;
end;

procedure TLyseeStringList.SaveToFile(const FileName: string);
begin
  FStringList.SaveToFile(FileName);
end;

function TLyseeStringList.SelectMatched(const Patten: string): TLyseeStringList;
var
  I: integer;
begin
  Result := TLyseeStringList.Create;
  for I := 0 to FStringList.Count - 1 do
    if MatchPatten(FStringList[I], Patten) then
      Result.Add(FStringList[I], TLyseeValue(FStringList.Objects[I]));
end;

procedure TLyseeStringList.SetCount(Value: integer);
begin
  if Value < 1 then Clear else
  begin
    while Value > GetCount do FStringList.Add('');
    while Value < GetCount do DeleteLast;
  end;
end;

procedure TLyseeStringList.SetValue(const Name, Value: string);
begin
  if Value <> '' then
    FStringList.Values[Name] := Value else
    RemoveByName(Name);
end;

procedure TLyseeStringList.Trim;
var
  I: integer;
begin
  BeginUpdate;
  try
    for I := 0 to FStringList.Count - 1 do
      FStringList[I] := SysUtils.Trim(FStringList[I]);
  finally
    EndUpdate;
  end;
end;

procedure TLyseeStringList.TrimAll;
var
  I: integer;
begin
  BeginUpdate;
  try
    for I := 0 to FStringList.Count - 1 do
      FStringList[I] := basic.TrimAll(FStringList[I]);
  finally
    EndUpdate;
  end;
end;

procedure TLyseeStringList.TrimLeft;
var
  I: integer;
begin
  BeginUpdate;
  try
    for I := 0 to FStringList.Count - 1 do
      FStringList[I] := SysUtils.TrimLeft(FStringList[I]);
  finally
    EndUpdate;
  end;
end;

procedure TLyseeStringList.TrimRight;
var
  I: integer;
begin
  BeginUpdate;
  try
    for I := 0 to FStringList.Count - 1 do
      FStringList[I] := SysUtils.TrimRight(FStringList[I]);
  finally
    EndUpdate;
  end;
end;

procedure TLyseeStringList.Unique;
var
  I: integer;
  S: string;
  L: TStringList;
begin
  BeginUpdate;
  try
    L := TStringList.Create;
    try
      L.CaseSensitive := FStringList.CaseSensitive;
      for I := FStringList.Count - 1 downto 0 do
      begin
        S := FStringList[I];
        if L.IndexOf(S) >= 0 then Delete(I) else L.Add(S);
      end;
    finally
      L.Free;
    end;
  finally
    EndUpdate;
  end;
end;

{ TLyseeStringListGenerate }

constructor TLyseeStringListGenerate.CreateIn(const List: TStrings);
begin
  inherited Create;
  FL := List;
  FIndex := -1;
end;

function TLyseeStringListGenerate.GetNext: boolean;
begin
  Result := HasNext;
  if Result then
  begin
    Inc(FIndex);
    AsString := FL[FIndex];
  end
  else SetNil;
end;

function TLyseeStringListGenerate.HasNext: boolean;
begin
  Result := (FL <> nil) and (FIndex < FL.Count - 1);
end;

{ TLyseeStringListType }

procedure TLyseeStringListType.MyAdd(const Param: TLyseeParam);
var
  L: TLyseeStringList;
  A: TLyseeList;
  I: integer;
begin
  if Param.GetSelf(L) then
  begin
    Param.Result.AsInteger := (L.Add(Param[1].AsString, nil));
    A := Param[2].AsArray;
    if A <> nil then
      for I := 0 to A.Count - 1 do
        L.Add(A[I].AsString, nil);
  end;
end;

procedure TLyseeStringListType.MyAddObject(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.Add(Param[1].AsString, Param[2]);
end;

procedure TLyseeStringListType.MyAssign(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.Assign(TLyseeStringList(Param[1].GetOA(Self)));
end;

procedure TLyseeStringListType.MyBeginUpdate(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then L.BeginUpdate;
end;

procedure TLyseeStringListType.MyClear(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then L.Clear;
end;

procedure TLyseeStringListType.MyClearObjects(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then L.ClearObjects;
end;

procedure TLyseeStringListType.MyCopy(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.SetTOA(Self, L.Copy(Param[1].AsInteger, Param[2].AsInteger));
end;

procedure TLyseeStringListType.MyCreate(const Param: TLyseeParam);
begin
  Param.Result.SetTOA(Self, TLyseeStringList.Create);
end;

procedure TLyseeStringListType.MyDelete(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.Delete(Param[1].AsInteger);
end;

procedure TLyseeStringListType.MyDeleteEmptyLines(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then L.DeleteEmptyLines;
end;

procedure TLyseeStringListType.MyDeleteLast(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then L.DeleteLast;
end;

procedure TLyseeStringListType.MyDeleteMatchedLines(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.DeleteMatchedLines(Param[1].AsString);
end;

procedure TLyseeStringListType.MyEndUpdate(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then L.EndUpdate;
end;

procedure TLyseeStringListType.MyEquals(const Param: TLyseeParam);
var
  L, S: TLyseeStringList;
begin
  if Param.GetSelf(L) then
  begin
    S := TLyseeStringList(Param[1].GetOA(Self));
    Param.Result.AsBoolean := (L = S) or ((S <> nil) and L.StringList.Equals(S.StringList));
  end
  else Param.Result.AsBoolean := (false);
end;

procedure TLyseeStringListType.MyExchange(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.StringList.Exchange(Param[1].AsInteger, Param[2].AsInteger);
end;

procedure TLyseeStringListType.MyGet(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.StringList[Param[1].AsInteger];
end;

procedure TLyseeStringListType.MyGetCommaText(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.StringList.CommaText;
end;

procedure TLyseeStringListType.MyGetCount(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.StringList.Count;
end;

procedure TLyseeStringListType.MyGetDelimitedText(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.StringList.DelimitedText;
end;

procedure TLyseeStringListType.MyGetDelimiter(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsChar := L.StringList.Delimiter;
end;

procedure TLyseeStringListType.MyGetFirst(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.StringList[0];
end;

procedure TLyseeStringListType.MyGetLast(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.StringList[L.StringList.Count - 1];
end;

procedure TLyseeStringListType.MyGetLineBreak(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.StringList.LineBreak;
end;

procedure TLyseeStringListType.MyGetName(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.StringList.Names[Param[1].AsInteger];
end;

procedure TLyseeStringListType.MyGetNameValueSeparator(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsChar := L.StringList.NameValueSeparator;
end;

procedure TLyseeStringListType.MyGetObject(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.SetValue(TLyseeValue(L.StringList.Objects[Param[1].AsInteger]));
end;

procedure TLyseeStringListType.MyGetQuoteChar(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsChar := L.StringList.QuoteChar;
end;

procedure TLyseeStringListType.MyGetCaseSensitive(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsBoolean := L.StringList.CaseSensitive;
end;

procedure TLyseeStringListType.MyGetSorted(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsBoolean := L.StringList.Sorted;
end;

procedure TLyseeStringListType.MyGetStrictDelimiter(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsBoolean := L.StringList.StrictDelimiter;
end;

procedure TLyseeStringListType.MyGetText(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.StringList.Text;
end;

procedure TLyseeStringListType.MyGetValue(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.StringList.Values[Param[1].AsString];
end;

procedure TLyseeStringListType.MyGetWriteBOM(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsBoolean := {$IFDEF FPC}false{$ELSE}L.StringList.WriteBOM{$ENDIF};
end;

procedure TLyseeStringListType.MyIndexOf(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.StringList.IndexOf(Param[1].AsString);
end;

procedure TLyseeStringListType.MyIndexOfName(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.StringList.IndexOfName(Param[1].AsString);
end;

procedure TLyseeStringListType.MyIndexOfObject(const Param: TLyseeParam);
var
  L: TLyseeStringList;
  V: TLyseeValue;
  I: integer;
begin
  if Param.GetSelf(L) then
  begin
    V := Param[1];
    for I := 0 to L.StringList.Count - 1 do
      if L.StringList.Objects[I] <> nil then
        if V.Same(TLyseeValue(L.StringList.Objects[I])) then
        begin
          Param.Result.AsInteger := I;
          Exit;
        end;
    Param.Result.AsInteger := -1;
  end;
end;

procedure TLyseeStringListType.MyIndexOfValue(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.IndexOfValue(Param[1].AsString);
end;

procedure TLyseeStringListType.MyInsert(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.Insert(Param[1].AsInteger, Param[2].AsString, nil);
end;

procedure TLyseeStringListType.MyInsertObject(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.Insert(Param[1].AsInteger, Param[2].AsString, Param[3]);
end;

procedure TLyseeStringListType.MyIsEmpty(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsBoolean := L.IsEmpty;
end;

procedure TLyseeStringListType.MyLeft(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.SetTOA(Self, L.CopyLeft(Param[1].AsInteger));
end;

procedure TLyseeStringListType.MyLoadFromFile(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.LoadFromFile(Param[1].GetFileName);
end;

procedure TLyseeStringListType.MyMove(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.StringList.Move(Param[1].AsInteger, Param[2].AsInteger);
end;

procedure TLyseeStringListType.MyNames(const Param: TLyseeParam);
var
  L: TLyseeStringList;
  N: TLyseeList;
  S: string;
  I: integer;
begin
  if Param.GetSelf(L) then
  begin
    N := TLyseeList.Create;
    Param.Result.AsArray := N;
    for I := 0 to L.Count - 1 do
    begin
      S := L.StringList.Names[I];
      if S <> '' then
        N.Add.AsString := S;
    end;
  end;
end;

procedure TLyseeStringListType.MyPack(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then L.Pack;
end;

procedure TLyseeStringListType.MyRemove(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.Remove(Param[1].AsString);
end;

procedure TLyseeStringListType.MyRemoveByName(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.RemoveByName(Param[1].AsString);
end;

procedure TLyseeStringListType.MyRename(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.Rename(Param[1].AsString, Param[2].AsString);
end;

procedure TLyseeStringListType.MyReverse(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then L.Reverse;
end;

procedure TLyseeStringListType.MyRight(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.SetTOA(Self, L.CopyRight(Param[1].AsInteger));
end;

procedure TLyseeStringListType.MySaveToFile(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.SaveToFile(Param[1].GetFileName);
end;

procedure TLyseeStringListType.MySelectMatchedLines(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.SetTOA(Self, L.SelectMatched(Param[1].AsString));
end;

procedure TLyseeStringListType.MySet(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.StringList[Param[1].AsInteger] := Param[2].AsString;
end;

procedure TLyseeStringListType.MySetCommaText(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
  begin
    L.Clear;
    L.StringList.CommaText := Param[1].AsString;
  end;
end;

procedure TLyseeStringListType.MySetCount(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.Count := Param[1].AsInteger;
end;

procedure TLyseeStringListType.MySetDelimitedText(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
  begin
    L.Clear;
    L.StringList.DelimitedText := Param[1].AsString;
  end;
end;

procedure TLyseeStringListType.MySetDelimiter(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.StringList.Delimiter := Param[1].AsChar;
end;

procedure TLyseeStringListType.MySetFirst(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.StringList[0] := Param[1].AsString;
end;

procedure TLyseeStringListType.MySetLast(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.StringList[L.StringList.Count - 1] := Param[1].AsString;
end;

procedure TLyseeStringListType.MySetLineBreak(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.StringList.LineBreak := Param[1].AsString;
end;

procedure TLyseeStringListType.MySetNameValueSeparator(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.StringList.NameValueSeparator := Param[1].AsChar;
end;

procedure TLyseeStringListType.MySetObject(const Param: TLyseeParam);
var
  L: TLyseeStringList;
  I: integer;
  V: TLyseeValue;
begin
  if Param.GetSelf(L) then
  begin
    I := Param[1].AsInteger;
    V := TLyseeValue(L.StringList.Objects[I]);
    if V = nil then
    begin
      V := TLyseeValue.Create;
      L.StringList.Objects[I] := V;
    end;
    V.SetValue(Param[2]);
  end;
end;

procedure TLyseeStringListType.MySetQuoteChar(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.StringList.QuoteChar := Param[1].AsChar;
end;

procedure TLyseeStringListType.MySetCaseSensitive(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.StringList.CaseSensitive := Param[1].AsBoolean;
end;

procedure TLyseeStringListType.MySetSorted(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.StringList.Sorted := Param[1].AsBoolean;
end;

procedure TLyseeStringListType.MySetStrictDelimiter(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.StringList.StrictDelimiter := Param[1].AsBoolean;
end;

procedure TLyseeStringListType.MySetText(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
  begin
    L.Clear;
    L.StringList.Text := Param[1].AsString;
  end;
end;

procedure TLyseeStringListType.MySetValue(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.Values[Param[1].AsString] := Param[2].AsString;
end;

procedure TLyseeStringListType.MySetWriteBOM(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    {$IFNDEF FPC}
    L.StringList.WriteBOM := Param[1].AsBoolean
    {$ENDIF};
end;

procedure TLyseeStringListType.MySort(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    L.StringList.Sort;
end;

procedure TLyseeStringListType.MyTrim(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then L.Trim;
end;

procedure TLyseeStringListType.MyTrimAll(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then L.TrimAll;
end;

procedure TLyseeStringListType.MyTrimLeft(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then L.TrimLeft;
end;

procedure TLyseeStringListType.MyTrimRight(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then L.TrimRight;
end;

procedure TLyseeStringListType.MyUnique(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then L.Unique;
end;

procedure TLyseeStringListType.MyValueFromIndex(const Param: TLyseeParam);
var
  L: TLyseeStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.StringList.ValueFromIndex[Param[1].AsInteger];
end;

procedure TLyseeStringListType.Setup;
begin
  Method('Create', Self, {$IFDEF FPC}@{$ENDIF}MyCreate);
  Method('IsEmpty', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsEmpty);
  Method('LoadFromFile', ['FileName'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyLoadFromFile);
  Method('SaveToFile', ['FileName'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MySaveToFile);
  Method('BeginUpdate', {$IFDEF FPC}@{$ENDIF}MyBeginUpdate);
  Method('EndUpdate', {$IFDEF FPC}@{$ENDIF}MyEndUpdate);
  Method('Clear', {$IFDEF FPC}@{$ENDIF}MyClear);
  Method('ClearObjects', {$IFDEF FPC}@{$ENDIF}MyClearObjects);
  Method('Delete', ['Index'], [my_int], {$IFDEF FPC}@{$ENDIF}MyDelete);
  Method('DeleteLast', {$IFDEF FPC}@{$ENDIF}MyDeleteLast);
  Method('DeleteEmptyLines', {$IFDEF FPC}@{$ENDIF}MyDeleteEmptyLines);
  Method('DeleteMatchedLines', ['Patten'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyDeleteMatchedLines);
  Method('Remove', ['S'], [my_string], {$IFDEF FPC}@{$ENDIF}MyRemove);
  Method('RemoveByName', ['Name'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyRemoveByName);
  Method('Add', my_int, ['_S', '...'], [my_string, my_array],
         {$IFDEF FPC}@{$ENDIF}MyAdd);
  Method('AddObject', my_int, ['S', 'Any'], [my_string, my_variant],
         {$IFDEF FPC}@{$ENDIF}MyAddObject);
  Method('Insert', ['Index', '_S'], [my_int, my_string],
         {$IFDEF FPC}@{$ENDIF}MyInsert);
  Method('InsertObject', ['Index', 'S', 'Any'], [my_int, my_string, my_variant],
         {$IFDEF FPC}@{$ENDIF}MyInsertObject);
  Method('Unique', {$IFDEF FPC}@{$ENDIF}MyUnique);
  Method('Pack', {$IFDEF FPC}@{$ENDIF}MyPack);
  Method('Exchange', ['X1', 'X2'], [my_int, my_int],
         {$IFDEF FPC}@{$ENDIF}MyExchange);
  Method('Move', ['FromX', 'ToX'], [my_int, my_int],
         {$IFDEF FPC}@{$ENDIF}MyMove);
  Method('Sort', {$IFDEF FPC}@{$ENDIF}MySort);
  Method('IndexOf', my_int, ['S'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyIndexOf);
  Method('IndexOfObject', my_int, ['Any'], [my_variant],
         {$IFDEF FPC}@{$ENDIF}MyIndexOfObject);
  Method('IndexOfName', my_int, ['Name'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyIndexOfName);
  Method('IndexOfValue', my_int, ['s'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MyIndexOfValue);
  Method('Rename', ['Name', 'NewName'], [my_string, my_string],
         {$IFDEF FPC}@{$ENDIF}MyRename);
  Method('Assign', ['Source'], [Self],
         {$IFDEF FPC}@{$ENDIF}MyAssign);
  Method('Equals', my_bool, ['Strings'], [Self],
         {$IFDEF FPC}@{$ENDIF}MyEquals);
  Method('Trim', {$IFDEF FPC}@{$ENDIF}MyTrim);
  Method('TrimAll', {$IFDEF FPC}@{$ENDIF}MyTrimAll);
  Method('TrimLeft', {$IFDEF FPC}@{$ENDIF}MyTrimLeft);
  Method('TrimRight', {$IFDEF FPC}@{$ENDIF}MyTrimRight);
  Method('Copy', Self, ['Index', 'Count'], [my_int, my_int],
         {$IFDEF FPC}@{$ENDIF}MyCopy);
  Method('Left', Self, ['Count'], [my_int],
         {$IFDEF FPC}@{$ENDIF}MyLeft);
  Method('Right', Self, ['Count'], [my_int],
         {$IFDEF FPC}@{$ENDIF}MyRight);
  Method('SelectMatchedLines', Self, ['Patten'], [my_string],
         {$IFDEF FPC}@{$ENDIF}MySelectMatchedLines);
  Method('Reverse', {$IFDEF FPC}@{$ENDIF}MyReverse);
  Method('Names', my_array, {$IFDEF FPC}@{$ENDIF}MyNames);
  Define('Count', my_int,
         {$IFDEF FPC}@{$ENDIF}MyGetCount,
         {$IFDEF FPC}@{$ENDIF}MySetCount);
  Define('', my_string, 'Index', my_int,
         {$IFDEF FPC}@{$ENDIF}MyGet,
         {$IFDEF FPC}@{$ENDIF}MySet);
  Define('Names', my_string, 'Index', my_int,
         {$IFDEF FPC}@{$ENDIF}MyGetName);
  Define('Values', my_string, 'Name', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetValue,
         {$IFDEF FPC}@{$ENDIF}MySetValue);
  Define('Objects', my_variant, 'Index', my_int,
         {$IFDEF FPC}@{$ENDIF}MyGetObject,
         {$IFDEF FPC}@{$ENDIF}MySetObject);
  Define('ValueFromIndex', my_string, 'Index', my_int,
         {$IFDEF FPC}@{$ENDIF}MyValueFromIndex);
  Define('Text', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetText,
         {$IFDEF FPC}@{$ENDIF}MySetText);
  Define('CommaText', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetCommaText,
         {$IFDEF FPC}@{$ENDIF}MySetCommaText);
  Define('Delimiter', my_char,
         {$IFDEF FPC}@{$ENDIF}MyGetDelimiter,
         {$IFDEF FPC}@{$ENDIF}MySetDelimiter);
  Define('DelimitedText', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetDelimitedText,
         {$IFDEF FPC}@{$ENDIF}MySetDelimitedText);
  Define('LineBreak', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetLineBreak,
         {$IFDEF FPC}@{$ENDIF}MySetLineBreak);
  Define('NameValueSeparator', my_char,
         {$IFDEF FPC}@{$ENDIF}MyGetNameValueSeparator,
         {$IFDEF FPC}@{$ENDIF}MySetNameValueSeparator);
  Define('QuoteChar', my_char,
         {$IFDEF FPC}@{$ENDIF}MyGetQuoteChar,
         {$IFDEF FPC}@{$ENDIF}MySetQuoteChar);
  Define('StrictDelimiter', my_bool,
         {$IFDEF FPC}@{$ENDIF}MyGetStrictDelimiter,
         {$IFDEF FPC}@{$ENDIF}MySetStrictDelimiter);
  Define('WriteBOM', my_bool,
         {$IFDEF FPC}@{$ENDIF}MyGetWriteBOM,
         {$IFDEF FPC}@{$ENDIF}MySetWriteBOM);
  Define('Sorted', my_bool,
         {$IFDEF FPC}@{$ENDIF}MyGetSorted,
         {$IFDEF FPC}@{$ENDIF}MySetSorted);
  Define('CaseSensitive', my_bool,
         {$IFDEF FPC}@{$ENDIF}MyGetCaseSensitive,
         {$IFDEF FPC}@{$ENDIF}MySetCaseSensitive);
  Define('First', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetFirst,
         {$IFDEF FPC}@{$ENDIF}MySetFirst);
  Define('Last', my_string,
         {$IFDEF FPC}@{$ENDIF}MyGetLast,
         {$IFDEF FPC}@{$ENDIF}MySetLast);

  AddOperate(opShi, my_variant, {$IFDEF FPC}@{$ENDIF}strlist_shi_variant);
  AddOperate(opFill, my_variant, {$IFDEF FPC}@{$ENDIF}strlist_fill_variant);

  inherited;
end;

function TLyseeStringListType.Add(Obj: pointer; Value: TLyseeValue): integer;
begin
  Result := TLyseeStringList(Obj).StringList.Add(Value.AsString);
end;

function TLyseeStringListType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyseeStringList(Obj).StringList.Text else
    Result := '';
end;

function TLyseeStringListType.Clear(Obj: pointer): boolean;
begin
  Result := (Obj <> nil);
  if Result then
    TLyseeStringList(Obj).Clear;
end;

procedure TLyseeStringListType.Convert(Value: TLyseeValue);
var
  T: TLyseeType;
  S: TLyseeStringList;
begin
  T := Value.VType;
  if T <> Self then
    if (T = my_array) or (T = my_list) then // Array/TList => TStringList
    begin
      S := TLyseeStringList.Create;
      S.AddList(TLyseeList(Value.GetOA));
      Value.SetTOA(Self, S);
    end
    else
    if T = my_string then // string => TStringList
    begin
      S := TLyseeStringList.Create;
      S.StringList.Text := Value.AsString;
      Value.SetTOA(Self, S);
    end
    else
    if T = my_hash then // THash => TStringList
    begin
      S := TLyseeStringList.Create;
      S.AddHashed(TLyseeHash(Value.GetOA));
      Value.SetTOA(Self, S);
    end
    else inherited;
end;

function TLyseeStringListType.ConvertTo(Value: TLyseeValue; T: TLyseeType): boolean;
var
  S: TLyseeStringList;
begin
  if (T = my_array) or (T = my_list) then // TStringList => Array/TList
  begin
    S := TLyseeStringList(Value.GetOA);
    if S <> nil then
      Value.SetTOA(T, S.AsLyseeList) else
      Value.SetTOA(T, nil);
    Result := true;
  end
  else
  if T = my_hash then  // TStringList => THashed
  begin
    S := TLyseeStringList(Value.GetOA);
    if S <> nil then
      Value.SetTOA(T, S.AsLyseeHash) else
      Value.SetTOA(T, nil);
    Result := true;
  end
  else
  if T = my_string then // TStringList => string
  begin
    S := TLyseeStringList(Value.GetOA);
    if S <> nil then
      Value.AsString := S.AsString else
      Value.AsString := '';
    Result := true;
  end
  else Result := inherited;
end;

function TLyseeStringListType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeStringList(Obj).DecRefcount else
    Result := 0;
end;

function TLyseeStringListType.Generate(Obj: pointer): TLyseeGenerate;
begin
  if (Obj <> nil) and (TLyseeStringList(Obj).StringList.Count > 0) then
    Result := TLyseeStringListGenerate.CreateIn(TLyseeStringList(Obj).StringList) else
    Result := nil;
end;

function TLyseeStringListType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeStringList(Obj).IncRefcount else
    Result := 0;
end;

function TLyseeStringListType.GetLength(Obj: pointer): int64;
begin
  if Obj <> nil then
    Result := TLyseeStringList(Obj).StringList.Count else
    Result := 0;
end;

{ TLyseeListType }

procedure TLyseeListType.Convert(Value: TLyseeValue);
var
  T: TLyseeType;
  S: TLyseeStringList;
  L: TLyseeList;
begin
  T := Value.VType;
  if T <> Self then
    if T = my_array then // Array => TList
    begin
      L := TLyseeList(Value.GetOA);
      Value.SetTOA(Self, L);
    end
    else
    if T = my_strlist then // TStringList => TList
    begin
      S := TLyseeStringList(Value.GetOA);
      if S <> nil then
        Value.SetTOA(Self, S.AsLyseeList) else
        Value.SetTOA(Self, nil);
    end
    else inherited;
end;

function TLyseeListType.ConvertTo(Value: TLyseeValue; T: TLyseeType): boolean;
var
  S: TLyseeStringList;
  L: TLyseeList;
begin
  if T = my_array then // TList => Array
  begin
    Value.SetTOA(my_array, Value.GetOA);
    Result := true;
  end
  else
  if T = my_strlist then // TList => TStringList
  begin
    S := TLyseeStringList.Create;
    S.AddList(TLyseeList(Value.GetOA));
    Value.SetTOA(my_strlist, S);
    Result := true;
  end
  else
  if T = my_string then // TList => string
  begin
    L := TLyseeList(Value.GetOA);
    if L <> nil then
      Value.AsString := L.AsString else
      Value.AsString := '';
    Result := true;
  end
  else Result := inherited;
end;

{ TLyseeClassesModule }

constructor TLyseeClassesModule.Create(const AName: string);
begin
  inherited;
  OnSetup := {$IFDEF FPC}@{$ENDIF}DoSetup;
end;

procedure TLyseeClassesModule.DoSetup(Sender: TObject);
begin
  OnSetup := nil;
  my_strlist := TLyseeStringListType.Create('TStringList', Self);
  my_list := TLyseeListType.Create('TList', Self, my_array);
  my_strlist.Setup;
  my_list.Setup;
end;

initialization
begin
  my_classes := TLyseeClassesModule.Create('Classes');
end;

end.
