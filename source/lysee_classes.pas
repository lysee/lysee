{==============================================================================}
{        UNIT: lysee_classes                                                   }
{ DESCRIPTION: lysee's classes module functions                                }
{   COPYRIGHT: Copyright (c) 2016-2016, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2016/12/18                                                      }
{    MODIFIED: 2017/01/07                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_classes;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, Basic, lysee;

type

  { TLiStringList }

  TLiStringList = class(TLiGarbage)
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
    procedure Assign(Source: TLiStringList);
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
    procedure Insert(Index: Integer; const S: string; V: TLiValue);
    function Add(const S: string; V: TLiValue): integer;
    procedure AddHashed(Hashed: TLiHash);
    procedure AddList(List: TLiList);
    function MatchStrs(const S1, S2: string): boolean;
    function IndexOfValue(const S: string): integer;
    function Copy(Index, ItemCount: integer): TLiStringList;
    function CopyLeft(ItemCount: integer): TLiStringList;
    function CopyRight(ItemCount: integer): TLiStringList;
    function SelectMatched(const Patten: string): TLiStringList;
    procedure Rename(const Name, NewName: string);
    procedure Reverse;
    function AsString: string;override;
    function IsEmpty: boolean;
    property Count: integer read GetCount write SetCount;
    property Values[const Name: string]: string read GetValue write SetValue;
    property StringList: TStringList read FStringList;
  end;

  { TLiStringListType }

  TLiStringListType = class(TLiType)
  protected
    procedure MyCreate(const Param: TLiParam);
    procedure MyIsEmpty(const Param: TLiParam);
    procedure MyLoadFromFile(const Param: TLiParam);
    procedure MySaveToFile(const Param: TLiParam);
    procedure MyBeginUpdate(const Param: TLiParam);
    procedure MyEndUpdate(const Param: TLiParam);
    procedure MyAssign(const Param: TLiParam);
    procedure MyClear(const Param: TLiParam);
    procedure MyClearObjects(const Param: TLiParam);
    procedure MyDelete(const Param: TLiParam);
    procedure MyDeleteLast(const Param: TLiParam);
    procedure MyDeleteEmptyLines(const Param: TLiParam);
    procedure MyRemove(const Param: TLiParam);
    procedure MyRemoveByName(const Param: TLiParam);
    procedure MyAdd(const Param: TLiParam);
    procedure MyAddObject(const Param: TLiParam);
    procedure MyInsert(const Param: TLiParam);
    procedure MyInsertObject(const Param: TLiParam);
    procedure MyExchange(const Param: TLiParam);
    procedure MyMove(const Param: TLiParam);
    procedure MySort(const Param: TLiParam);
    procedure MyUnique(const Param: TLiParam);
    procedure MyPack(const Param: TLiParam);
    procedure MyIndexOf(const Param: TLiParam);
    procedure MyIndexOfObject(const Param: TLiParam);
    procedure MyIndexOfName(const Param: TLiParam);
    procedure MyIndexOfValue(const Param: TLiParam);
    procedure MyRename(const Param: TLiParam);
    procedure MyEquals(const Param: TLiParam);
    procedure MyTrim(const Param: TLiParam);
    procedure MyTrimAll(const Param: TLiParam);
    procedure MyTrimLeft(const Param: TLiParam);
    procedure MyTrimRight(const Param: TLiParam);
    procedure MyCopy(const Param: TLiParam);
    procedure MyLeft(const Param: TLiParam);
    procedure MyRight(const Param: TLiParam);
    procedure MySelectMatchedLines(const Param: TLiParam);
    procedure MyDeleteMatchedLines(const Param: TLiParam);
    procedure MyReverse(const Param: TLiParam);
    procedure MyGetCount(const Param: TLiParam);
    procedure MySetCount(const Param: TLiParam);
    procedure MyGet(const Param: TLiParam);
    procedure MySet(const Param: TLiParam);
    procedure MyNames(const Param: TLiParam);
    procedure MyGetName(const Param: TLiParam);
    procedure MyGetValue(const Param: TLiParam);
    procedure MySetValue(const Param: TLiParam);
    procedure MyGetObject(const Param: TLiParam);
    procedure MySetObject(const Param: TLiParam);
    procedure MyValueFromIndex(const Param: TLiParam);
    procedure MyGetText(const Param: TLiParam);
    procedure MySetText(const Param: TLiParam);
    procedure MyGetCommaText(const Param: TLiParam);
    procedure MySetCommaText(const Param: TLiParam);
    procedure MyGetDelimiter(const Param: TLiParam);
    procedure MySetDelimiter(const Param: TLiParam);
    procedure MyGetDelimitedText(const Param: TLiParam);
    procedure MySetDelimitedText(const Param: TLiParam);
    procedure MyGetLineBreak(const Param: TLiParam);
    procedure MySetLineBreak(const Param: TLiParam);
    procedure MyGetNameValueSeparator(const Param: TLiParam);
    procedure MySetNameValueSeparator(const Param: TLiParam);
    procedure MyGetQuoteChar(const Param: TLiParam);
    procedure MySetQuoteChar(const Param: TLiParam);
    procedure MyGetStrictDelimiter(const Param: TLiParam);
    procedure MySetStrictDelimiter(const Param: TLiParam);
    procedure MyGetWriteBOM(const Param: TLiParam);
    procedure MySetWriteBOM(const Param: TLiParam);
    procedure MyGetSorted(const Param: TLiParam);
    procedure MySetSorted(const Param: TLiParam);
    procedure MyGetCaseSensitive(const Param: TLiParam);
    procedure MySetCaseSensitive(const Param: TLiParam);
    procedure MyGetFirst(const Param: TLiParam);
    procedure MySetFirst(const Param: TLiParam);
    procedure MyGetLast(const Param: TLiParam);
    procedure MySetLast(const Param: TLiParam);
    procedure Setup;override;
  public
    function _IncRefcount(Obj: pointer): integer;override;
    function _DecRefcount(Obj: pointer): integer;override;
    function _AsString(Obj: pointer): string;override;
    function _Generate(Obj: pointer): TLiGenerate;override;
    function _Length(Obj: pointer): int64;override;
    function _Clear(Obj: pointer): boolean;override;
    function _Add(Obj: pointer; Value: TLiValue): integer;override;
  end;

var
  my_classes: TLiModule;
  my_strlist: TLiStringListType;

implementation

uses
  Math, lysee_load;

procedure strlist_shi_variant(V1, V2: TLiValue);
var
  L: TLiStringList;
begin
  if V1.GetSelf(L) then
    L.StringList.Add(V2.AsString);
end;

procedure strlist_fill_variant(V1, V2: TLiValue);
var
  L: TLiStringList;
  G: TLiGenerate;
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

procedure strlist_as_list(Value: TLiValue);
var
  A: TLiList;
  L: TLiStringList;
  I: integer;
begin
  A := TLiList.Create;
  L := TLiStringList(Value.GetOA);
  if L <> nil then
    for I := 0 to L.StringList.Count - 1 do
      A.Add.AsString := L.StringList[I];
  Value.SetTOA(my_list, A);
end;

procedure strlist_as_hashed(Value: TLiValue);
var
  H: TLiHash;
  L: TLiStringList;
  I: integer;
begin
  H := TLiHash.Create;
  L := TLiStringList(Value.GetOA);
  if L <> nil then
    for I := 0 to L.StringList.Count - 1 do
      H.Add(L.StringList[I]).SetValue(TLiValue(L.StringList.Objects[I]));
  Value.AsHash := H;
end;

procedure string_as_strlist(Value: TLiValue);
var
  L: TLiStringList;
begin
  L := TLiStringList.Create;
  L.StringList.Text := Value.AsString;
  Value.SetTOA(my_strlist, L);
end;

procedure list_as_strlist(Value: TLiValue);
var
  L: TLiStringList;
begin
  L := TLiStringList.Create;
  L.AddList(TLiList(Value.GetOA));
  Value.SetTOA(my_strlist, L);
end;

procedure hashed_as_strlist(Value: TLiValue);
var
  L: TLiStringList;
begin
  L := TLiStringList.Create;
  L.AddHashed(TLiHash(Value.GetOA));
  Value.SetTOA(my_strlist, L);
end;

{ TLiStringList }

procedure TLiStringList.AddHashed(Hashed: TLiHash);
var
  L: TList;
  I: integer;
  V: TLiHashItem;
begin
  if Hashed <> nil then
  begin
    L := Hashed.SetupItemList;
    BeginUpdate;
    try
      for I := 0 to L.Count - 1 do
      begin
        V := TLiHashItem(L[I]);
        Add(V.Key, V);
      end;
    finally
      EndUpdate;
      L.Free;
    end;
  end;
end;

procedure TLiStringList.AddList(List: TLiList);
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

function TLiStringList.Add(const S: string; V: TLiValue): integer;
var
  T: TLiValue;
begin
  if (V <> nil) and not V.IsNil then
  begin
    T := TLiValue.Create;
    Result := FStringList.AddObject(S, T);
    T.SetValue(V);
  end
  else Result := FStringList.Add(S);
end;

procedure TLiStringList.Assign(Source: TLiStringList);
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
            TLiValue(Source.FStringList.Objects[I]));
    finally
      EndUpdate;
    end;
  end;
end;

function TLiStringList.AsString: string;
begin
  Result := FStringList.Text;
end;

procedure TLiStringList.BeginUpdate;
begin
  FStringList.BeginUpdate;
end;

procedure TLiStringList.Clear;
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

procedure TLiStringList.ClearObjects;
var
  I: integer;
  V: TLiValue;
begin
  BeginUpdate;
  try
    for I := FStringList.Count - 1 downto 0 do
      if FStringList.Objects[I] <> nil then
      begin
        V := TLiValue(FStringList.Objects[I]);
        FStringList.Objects[I] := nil;
        V.Free;
      end;
  finally
    EndUpdate;
  end;
end;

function TLiStringList.Copy(Index, ItemCount: integer): TLiStringList;
var
  I: integer;
begin
  Result := TLiStringList.Create;
  if Index < 0 then
  begin
    Inc(ItemCount, Index);
    Index := 0;
  end;
  ItemCount := Max(0, Min(FStringList.Count - Index, ItemCount));
  for I := 1 to ItemCount do
  begin
    Result.Add(FStringList[Index], TLiValue(FStringList.Objects[Index]));
    Inc(Index);
  end;
end;

constructor TLiStringList.Create;
begin
  inherited;
  FStringList := TStringList.Create;
end;

procedure TLiStringList.Delete(Index: integer);
var
  V: TLiValue;
begin
  V := TLiValue(FStringList.Objects[Index]);
  FStringList.Delete(Index);
  if V <> nil then V.Free;
end;

procedure TLiStringList.DeleteEmptyLines;
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

procedure TLiStringList.DeleteLast;
begin
  Delete(FStringList.Count - 1);
end;

destructor TLiStringList.Destroy;
begin
  Clear;
  FreeAndNil(FStringList);
  inherited;
end;

procedure TLiStringList.EndUpdate;
begin
  FStringList.EndUpdate;
end;

function TLiStringList.GetCount: integer;
begin
  Result := FStringList.Count;
end;

function TLiStringList.GetValue(const Name: string): string;
begin
  Result := FStringList.Values[Name];
end;

function TLiStringList.IndexOfValue(const S: string): integer;
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

procedure TLiStringList.Insert(Index: Integer; const S: string; V: TLiValue);
var
  T: TLiValue;
begin
  if (V <> nil) and not V.IsNil then
  begin
    T := TLiValue.Create;
    FStringList.InsertObject(Index, S, T);
    T.SetValue(V);
  end
  else FStringList.Insert(Index, S);
end;

function TLiStringList.IsEmpty: boolean;
begin
  Result := (FStringList.Count = 0);
end;

procedure TLiStringList.LoadFromFile(const FileName: string);
begin
  BeginUpdate;
  try
    Clear;
    FStringList.LoadFromFile(FileName);
  finally
    EndUpdate;
  end;
end;

function TLiStringList.CopyLeft(ItemCount: integer): TLiStringList;
begin
  Result := Copy(0, ItemCount);
end;

procedure TLiStringList.MarkForSurvive;
var
  I: integer;
  V: TLiValue;
begin
  if (Self <> nil) and not Survived then
  begin
    Survived := true;
    for I := 0 to FStringList.Count - 1 do
      if FStringList.Objects[I] <> nil then
      begin
        V := TLiValue(FStringList.Objects[I]);
        V.MarkForSurvive;
      end;
  end;
end;

function TLiStringList.MatchStrs(const S1, S2: string): boolean;
begin
  if FStringList.CaseSensitive then
    Result := (SysUtils.CompareStr(S1, S2) = 0) else
    Result := SysUtils.SameText(S1, S2);
end;

procedure TLiStringList.Pack;
begin
  BeginUpdate;
  try
    TrimRight;
    DeleteEmptyLines;
  finally
    EndUpdate;
  end;
end;

procedure TLiStringList.Remove(const S: string);
var
  I: integer;
begin
  I := FStringList.IndexOf(S);
  if I >= 0 then Delete(I);
end;

procedure TLiStringList.RemoveByName(const S: string);
var
  I: integer;
begin
  I := FStringList.IndexOfName(S);
  if I >= 0 then Delete(I);
end;

procedure TLiStringList.Rename(const Name, NewName: string);
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

procedure TLiStringList.Reverse;
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

function TLiStringList.CopyRight(ItemCount: integer): TLiStringList;
begin
  Result := Copy(FStringList.Count - ItemCount, ItemCount);
end;

procedure TLiStringList.DeleteMatchedLines(const Patten: string);
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

procedure TLiStringList.SaveToFile(const FileName: string);
begin
  FStringList.SaveToFile(FileName);
end;

function TLiStringList.SelectMatched(const Patten: string): TLiStringList;
var
  I: integer;
begin
  Result := TLiStringList.Create;
  for I := 0 to FStringList.Count - 1 do
    if MatchPatten(FStringList[I], Patten) then
      Result.Add(FStringList[I], TLiValue(FStringList.Objects[I]));
end;

procedure TLiStringList.SetCount(Value: integer);
begin
  if Value < 1 then Clear else
  begin
    while Value > GetCount do FStringList.Add('');
    while Value < GetCount do DeleteLast;
  end;
end;

procedure TLiStringList.SetValue(const Name, Value: string);
begin
  if Value <> '' then
    FStringList.Values[Name] := Value else
    RemoveByName(Name);
end;

procedure TLiStringList.Trim;
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

procedure TLiStringList.TrimAll;
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

procedure TLiStringList.TrimLeft;
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

procedure TLiStringList.TrimRight;
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

procedure TLiStringList.Unique;
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

{ TLiStringListType }

procedure TLiStringListType.MyAdd(const Param: TLiParam);
var
  L: TLiStringList;
  A: TLiList;
  I: integer;
begin
  if Param.GetSelf(L) then
  begin
    Param.Result.AsInteger := (L.Add(Param[1].AsString, nil));
    A := Param[2].AsList;
    if A <> nil then
      for I := 0 to A.Count - 1 do
        L.Add(A[I].AsString, nil);
  end;
end;

procedure TLiStringListType.MyAddObject(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.Add(Param[1].AsString, Param[2]);
end;

procedure TLiStringListType.MyAssign(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.Assign(TLiStringList(Param[1].GetOA(Self)));
end;

procedure TLiStringListType.MyBeginUpdate(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then L.BeginUpdate;
end;

procedure TLiStringListType.MyClear(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then L.Clear;
end;

procedure TLiStringListType.MyClearObjects(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then L.ClearObjects;
end;

procedure TLiStringListType.MyCopy(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.SetTOA(Self, L.Copy(Param[1].AsInteger, Param[2].AsInteger));
end;

procedure TLiStringListType.MyCreate(const Param: TLiParam);
begin
  Param.Result.SetTOA(Self, TLiStringList.Create);
end;

procedure TLiStringListType.MyDelete(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.Delete(Param[1].AsInteger);
end;

procedure TLiStringListType.MyDeleteEmptyLines(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then L.DeleteEmptyLines;
end;

procedure TLiStringListType.MyDeleteLast(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then L.DeleteLast;
end;

procedure TLiStringListType.MyDeleteMatchedLines(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.DeleteMatchedLines(Param[1].AsString);
end;

procedure TLiStringListType.MyEndUpdate(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then L.EndUpdate;
end;

procedure TLiStringListType.MyEquals(const Param: TLiParam);
var
  L, S: TLiStringList;
begin
  if Param.GetSelf(L) then
  begin
    S := TLiStringList(Param[1].GetOA(Self));
    Param.Result.AsBoolean := ((S <> nil) and L.StringList.Equals(S.StringList));
  end
  else Param.Result.AsBoolean := (false);
end;

procedure TLiStringListType.MyExchange(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.StringList.Exchange(Param[1].AsInteger, Param[2].AsInteger);
end;

procedure TLiStringListType.MyGet(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.StringList[Param[1].AsInteger];
end;

procedure TLiStringListType.MyGetCommaText(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.StringList.CommaText;
end;

procedure TLiStringListType.MyGetCount(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.StringList.Count;
end;

procedure TLiStringListType.MyGetDelimitedText(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.StringList.DelimitedText;
end;

procedure TLiStringListType.MyGetDelimiter(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsChar := L.StringList.Delimiter;
end;

procedure TLiStringListType.MyGetFirst(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.StringList[0];
end;

procedure TLiStringListType.MyGetLast(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.StringList[L.StringList.Count - 1];
end;

procedure TLiStringListType.MyGetLineBreak(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.StringList.LineBreak;
end;

procedure TLiStringListType.MyGetName(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.StringList.Names[Param[1].AsInteger];
end;

procedure TLiStringListType.MyGetNameValueSeparator(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsChar := L.StringList.NameValueSeparator;
end;

procedure TLiStringListType.MyGetObject(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.SetValue(TLiValue(L.StringList.Objects[Param[1].AsInteger]));
end;

procedure TLiStringListType.MyGetQuoteChar(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsChar := L.StringList.QuoteChar;
end;

procedure TLiStringListType.MyGetCaseSensitive(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsBoolean := L.StringList.CaseSensitive;
end;

procedure TLiStringListType.MyGetSorted(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsBoolean := L.StringList.Sorted;
end;

procedure TLiStringListType.MyGetStrictDelimiter(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsBoolean := L.StringList.StrictDelimiter;
end;

procedure TLiStringListType.MyGetText(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.StringList.Text;
end;

procedure TLiStringListType.MyGetValue(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.StringList.Values[Param[1].AsString];
end;

procedure TLiStringListType.MyGetWriteBOM(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsBoolean := {$IFDEF FPC}false{$ELSE}L.StringList.WriteBOM{$ENDIF};
end;

procedure TLiStringListType.MyIndexOf(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.StringList.IndexOf(Param[1].AsString);
end;

procedure TLiStringListType.MyIndexOfName(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.StringList.IndexOfName(Param[1].AsString);
end;

procedure TLiStringListType.MyIndexOfObject(const Param: TLiParam);
var
  L: TLiStringList;
  V: TLiValue;
  I: integer;
begin
  if Param.GetSelf(L) then
  begin
    V := Param[1];
    for I := 0 to L.StringList.Count - 1 do
      if L.StringList.Objects[I] <> nil then
        if V.Same(TLiValue(L.StringList.Objects[I])) then
        begin
          Param.Result.AsInteger := I;
          Exit;
        end;
    Param.Result.AsInteger := -1;
  end;
end;

procedure TLiStringListType.MyIndexOfValue(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsInteger := L.IndexOfValue(Param[1].AsString);
end;

procedure TLiStringListType.MyInsert(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.Insert(Param[1].AsInteger, Param[2].AsString, nil);
end;

procedure TLiStringListType.MyInsertObject(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.Insert(Param[1].AsInteger, Param[2].AsString, Param[3]);
end;

procedure TLiStringListType.MyIsEmpty(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsBoolean := L.IsEmpty;
end;

procedure TLiStringListType.MyLeft(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.SetTOA(Self, L.CopyLeft(Param[1].AsInteger));
end;

procedure TLiStringListType.MyLoadFromFile(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.LoadFromFile(Param[1].GetFileName);
end;

procedure TLiStringListType.MyMove(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.StringList.Move(Param[1].AsInteger, Param[2].AsInteger);
end;

procedure TLiStringListType.MyNames(const Param: TLiParam);
var
  L: TLiStringList;
  N: TLiList;
  S: string;
  I: integer;
begin
  if Param.GetSelf(L) then
  begin
    N := TLiList.Create;
    Param.Result.AsList := N;
    for I := 0 to L.Count - 1 do
    begin
      S := L.StringList.Names[I];
      if S <> '' then
        N.Add.AsString := S;
    end;
  end;
end;

procedure TLiStringListType.MyPack(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then L.Pack;
end;

procedure TLiStringListType.MyRemove(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.Remove(Param[1].AsString);
end;

procedure TLiStringListType.MyRemoveByName(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.RemoveByName(Param[1].AsString);
end;

procedure TLiStringListType.MyRename(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.Rename(Param[1].AsString, Param[2].AsString);
end;

procedure TLiStringListType.MyReverse(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then L.Reverse;
end;

procedure TLiStringListType.MyRight(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.SetTOA(Self, L.CopyRight(Param[1].AsInteger));
end;

procedure TLiStringListType.MySaveToFile(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.SaveToFile(Param[1].GetFileName);
end;

procedure TLiStringListType.MySelectMatchedLines(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.SetTOA(Self, L.SelectMatched(Param[1].AsString));
end;

procedure TLiStringListType.MySet(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.StringList[Param[1].AsInteger] := Param[2].AsString;
end;

procedure TLiStringListType.MySetCommaText(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
  begin
    L.Clear;
    L.StringList.CommaText := Param[1].AsString;
  end;
end;

procedure TLiStringListType.MySetCount(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.Count := Param[1].AsInteger;
end;

procedure TLiStringListType.MySetDelimitedText(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
  begin
    L.Clear;
    L.StringList.DelimitedText := Param[1].AsString;
  end;
end;

procedure TLiStringListType.MySetDelimiter(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.StringList.Delimiter := Param[1].AsChar;
end;

procedure TLiStringListType.MySetFirst(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.StringList[0] := Param[1].AsString;
end;

procedure TLiStringListType.MySetLast(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.StringList[L.StringList.Count - 1] := Param[1].AsString;
end;

procedure TLiStringListType.MySetLineBreak(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.StringList.LineBreak := Param[1].AsString;
end;

procedure TLiStringListType.MySetNameValueSeparator(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.StringList.NameValueSeparator := Param[1].AsChar;
end;

procedure TLiStringListType.MySetObject(const Param: TLiParam);
var
  L: TLiStringList;
  I: integer;
  V: TLiValue;
begin
  if Param.GetSelf(L) then
  begin
    I := Param[1].AsInteger;
    V := TLiValue(L.StringList.Objects[I]);
    if V = nil then
    begin
      V := TLiValue.Create;
      L.StringList.Objects[I] := V;
    end;
    V.SetValue(Param[2]);
  end;
end;

procedure TLiStringListType.MySetQuoteChar(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.StringList.QuoteChar := Param[1].AsChar;
end;

procedure TLiStringListType.MySetCaseSensitive(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.StringList.CaseSensitive := Param[1].AsBoolean;
end;

procedure TLiStringListType.MySetSorted(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.StringList.Sorted := Param[1].AsBoolean;
end;

procedure TLiStringListType.MySetStrictDelimiter(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.StringList.StrictDelimiter := Param[1].AsBoolean;
end;

procedure TLiStringListType.MySetText(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
  begin
    L.Clear;
    L.StringList.Text := Param[1].AsString;
  end;
end;

procedure TLiStringListType.MySetValue(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.Values[Param[1].AsString] := Param[2].AsString;
end;

procedure TLiStringListType.MySetWriteBOM(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    {$IFNDEF FPC}
    L.StringList.WriteBOM := Param[1].AsBoolean
    {$ENDIF};
end;

procedure TLiStringListType.MySort(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    L.StringList.Sort;
end;

procedure TLiStringListType.MyTrim(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then L.Trim;
end;

procedure TLiStringListType.MyTrimAll(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then L.TrimAll;
end;

procedure TLiStringListType.MyTrimLeft(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then L.TrimLeft;
end;

procedure TLiStringListType.MyTrimRight(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then L.TrimRight;
end;

procedure TLiStringListType.MyUnique(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then L.Unique;
end;

procedure TLiStringListType.MyValueFromIndex(const Param: TLiParam);
var
  L: TLiStringList;
begin
  if Param.GetSelf(L) then
    Param.Result.AsString := L.StringList.ValueFromIndex[Param[1].AsInteger];
end;

procedure TLiStringListType.Setup;
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
  Method('Add', my_int, ['_S', '...'], [my_string, my_list],
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
  Method('Names', my_list, {$IFDEF FPC}@{$ENDIF}MyNames);
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
  AddConvert(my_string, {$IFDEF FPC}@{$ENDIF}string_as_strlist);
  AddConvert(my_list, {$IFDEF FPC}@{$ENDIF}list_as_strlist);
  AddConvert(my_hash, {$IFDEF FPC}@{$ENDIF}hashed_as_strlist);
  my_list.AddConvert(Self, {$IFDEF FPC}@{$ENDIF}strlist_as_list);
  my_hash.AddConvert(Self, {$IFDEF FPC}@{$ENDIF}strlist_as_hashed);

  inherited;
end;

function TLiStringListType._Add(Obj: pointer; Value: TLiValue): integer;
begin
  Result := TLiStringList(Obj).StringList.Add(Value.AsString);
end;

function TLiStringListType._AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLiStringList(Obj).StringList.Text else
    Result := '';
end;

function TLiStringListType._Clear(Obj: pointer): boolean;
begin
  Result := (Obj <> nil);
  if Result then
    TLiStringList(Obj).Clear;
end;

function TLiStringListType._DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiStringList(Obj).DecRefcount else
    Result := 0;
end;

function TLiStringListType._Generate(Obj: pointer): TLiGenerate;
begin
  if (Obj <> nil) and (TLiStringList(Obj).StringList.Count > 0) then
    Result := TLiGenerate_strlist.CreateIn(TLiStringList(Obj).StringList) else
    Result := nil;
end;

function TLiStringListType._IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiStringList(Obj).IncRefcount else
    Result := 0;
end;

function TLiStringListType._Length(Obj: pointer): int64;
begin
  if Obj <> nil then
    Result := TLiStringList(Obj).StringList.Count else
    Result := 0;
end;

initialization
begin
  my_classes := AddModule('Classes');
  my_strlist := TLiStringListType.Create('TStringList', my_classes, nil);
end;

end.
