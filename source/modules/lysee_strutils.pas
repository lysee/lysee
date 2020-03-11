unit lysee_strutils;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, StrUtils, basic, lysee;

type

  { TLiType_TStrbuf }

  TLiType_TStrbuf = class(TLiType)
  protected
    function _IncRefcount(Obj: pointer): integer;override;
    function _DecRefcount(Obj: pointer): integer;override;
    function _AsString(Obj: pointer): string;override;
  end;

  { TLiType_strcut }

  TLiType_strcut = class(TLiType)
  protected
    function _IncRefcount(Obj: pointer): integer;override;
    function _DecRefcount(Obj: pointer): integer;override;
    function _AsString(Obj: pointer): string;override;
  end;

var
  my_strutils: TLiModule;
  my_TStrbuf: TLiType_TStrbuf;
  my_strcut: TLiType_strcut;

implementation

{ TLiType_TStrbuf }

function TLiType_TStrbuf._IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiStrbuf(Obj).IncRefcount else
    Result := 0;
end;

function TLiType_TStrbuf._DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLiStrbuf(Obj).DecRefcount else
    Result := 0;
end;

function TLiType_TStrbuf._AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLiStrbuf(Obj).AsString else
    Result := '';
end;

{ TLiType_strcut }

function TLiType_strcut._IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := strcut(Obj).IncRefcount else
    Result := 0;
end;

function TLiType_strcut._DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := strcut(Obj).DecRefcount else
    Result := 0;
end;

function TLiType_strcut._AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := strcut(Obj).AsString else
    Result := '';
end;

{ lysee procedures }

procedure pp_TStrbuf_create(const Param: TLiParam);
var
  AStr: string;
begin
  AStr := Param[0].AsString;
  Param.Result.SetTOA(my_TStrbuf, TLiStrbuf.Create(AStr));
end;

procedure pp_TStrbuf_replace(const Param: TLiParam);
var
  self: TLiStrbuf;
  patten: string;
  newStr: string;
  IgnoreCase: boolean;
  ReplaceFirstOnly: boolean;
begin
  if not Param.GetSelf(self) then Exit;
  patten := Param[1].AsString;
  newStr := Param[2].AsString;
  IgnoreCase := Param[3].AsBoolean;
  ReplaceFirstOnly := Param[4].AsBoolean;
  self.Replace(patten, newStr, IgnoreCase, ReplaceFirstOnly);
end;

procedure pp_TStrbuf_pos(const Param: TLiParam);
var
  self: TLiStrbuf;
  SubStr: string;
  IgnoreCase: boolean;
  Start: integer;
begin
  if not Param.GetSelf(self) then Exit;
  SubStr := Param[1].AsString;
  IgnoreCase := Param[2].AsBoolean;
  Start := integer(Param[3].AsInteger);
  Param.Result.AsInteger := int64(self.Pos(SubStr, IgnoreCase, Start));
end;

procedure pp_TStrbuf_lastPos(const Param: TLiParam);
var
  self: TLiStrbuf;
  SubStr: string;
  IgnoreCase: boolean;
  Start: integer;
begin
  if not Param.GetSelf(self) then Exit;
  SubStr := Param[1].AsString;
  IgnoreCase := Param[2].AsBoolean;
  Start := integer(Param[3].AsInteger);
  Param.Result.AsInteger := int64(self.LastPos(SubStr, IgnoreCase, Start));
end;

procedure pp_TStrbuf_trim(const Param: TLiParam);
var
  self: TLiStrbuf;
begin
  if not Param.GetSelf(self) then Exit;
  self.Trim;
end;

procedure pp_TStrbuf_trimLeft(const Param: TLiParam);
var
  self: TLiStrbuf;
begin
  if not Param.GetSelf(self) then Exit;
  self.TrimLeft;
end;

procedure pp_TStrbuf_trimRight(const Param: TLiParam);
var
  self: TLiStrbuf;
begin
  if not Param.GetSelf(self) then Exit;
  self.TrimRight;
end;

procedure pp_TStrbuf_trimAll(const Param: TLiParam);
var
  self: TLiStrbuf;
begin
  if not Param.GetSelf(self) then Exit;
  self.TrimAll;
end;

procedure pp_TStrbuf_lower(const Param: TLiParam);
var
  self: TLiStrbuf;
begin
  if not Param.GetSelf(self) then Exit;
  self.Lower;
end;

procedure pp_TStrbuf_upper(const Param: TLiParam);
var
  self: TLiStrbuf;
begin
  if not Param.GetSelf(self) then Exit;
  self.Upper;
end;

procedure pp_TStrbuf_delete(const Param: TLiParam);
var
  self: TLiStrbuf;
  Index: integer;
  Count: integer;
begin
  if not Param.GetSelf(self) then Exit;
  Index := integer(Param[1].AsInteger);
  Count := integer(Param[2].AsInteger);
  self.Delete(Index, Count);
end;

procedure pp_TStrbuf_insert(const Param: TLiParam);
var
  self: TLiStrbuf;
  SubStr: string;
  Index: integer;
begin
  if not Param.GetSelf(self) then Exit;
  SubStr := Param[1].AsString;
  Index := integer(Param[2].AsInteger);
  self.Insert(SubStr, Index);
end;

procedure pp_TStrbuf_join(const Param: TLiParam);
var
  self: TLiStrbuf;
  S: string;
begin
  if not Param.GetSelf(self) then Exit;
  S := Param[1].AsString;
  self.Join(S);
end;

procedure pp_TStrbuf_reverse(const Param: TLiParam);
var
  self: TLiStrbuf;
begin
  if not Param.GetSelf(self) then Exit;
  self.Reverse;
end;

procedure pp_TStrbuf_randomize(const Param: TLiParam);
var
  self: TLiStrbuf;
begin
  if not Param.GetSelf(self) then Exit;
  self.Randomize;
end;

procedure pp_TStrbuf_contains(const Param: TLiParam);
var
  self: TLiStrbuf;
  SubStr: string;
  IgnoreCase: boolean;
begin
  if not Param.GetSelf(self) then Exit;
  SubStr := Param[1].AsString;
  IgnoreCase := Param[2].AsBoolean;
  Param.Result.AsBoolean := self.Contains(SubStr, IgnoreCase);
end;

procedure pp_TStrbuf_copy(const Param: TLiParam);
var
  self: TLiStrbuf;
  Index: int64;
  Count: int64;
begin
  if not Param.GetSelf(self) then Exit;
  Index := Param[1].AsInteger;
  Count := Param[2].AsInteger;
  Param.Result.AsString := self.Copy(Index, Count);
end;

procedure pp_TStrbuf_getBuffer(const Param: TLiParam);
var
  Self: TLiStrbuf;
begin
  if not Param.GetSelf(Self) then Exit;
  Param.Result.AsString := Self.Buffer;
end;

procedure pp_TStrbuf_setBuffer(const Param: TLiParam);
var
  Self: TLiStrbuf;
  Value: string;
begin
  if not Param.GetSelf(Self) then Exit;
  Value := Param[1].AsString;
  Self.Buffer := Value;
end;

procedure pp_TStrbuf_getSize(const Param: TLiParam);
var
  Self: TLiStrbuf;
begin
  if not Param.GetSelf(Self) then Exit;
  Param.Result.AsInteger := int64(Self.Size);
end;

procedure pp_TStrbuf_setSize(const Param: TLiParam);
var
  Self: TLiStrbuf;
  Value: integer;
begin
  if not Param.GetSelf(Self) then Exit;
  Value := integer(Param[1].AsInteger);
  Self.Size := Value;
end;

procedure pp_strcut_create(const Param: TLiParam);
var
  AHeader: string;
  ADelimiter: string;
begin
  AHeader := Param[0].AsString;
  ADelimiter := Param[1].AsString;
  Param.Result.SetTOA(my_strcut, strcut.Create(AHeader, ADelimiter));
end;

procedure pp_strcut_clear(const Param: TLiParam);
var
  self: strcut;
begin
  if not Param.GetSelf(self) then Exit;
  self.Clear;
end;

procedure pp_strcut_clearValues(const Param: TLiParam);
var
  self: strcut;
begin
  if not Param.GetSelf(self) then Exit;
  self.ClearValues;
end;

procedure pp_strcut_init(const Param: TLiParam);
var
  self: strcut;
  AHeader: string;
  ADelimiter: string;
begin
  if not Param.GetSelf(self) then Exit;
  AHeader := Param[1].AsString;
  ADelimiter := Param[2].AsString;
  self.Init(AHeader, ADelimiter);
end;

procedure pp_strcut_parse(const Param: TLiParam);
var
  self: strcut;
  S: string;
begin
  if not Param.GetSelf(self) then Exit;
  S := Param[1].AsString;
  self.Parse(S);
end;

procedure pp_strcut_indexOf(const Param: TLiParam);
var
  self: strcut;
  Key: string;
begin
  if not Param.GetSelf(self) then Exit;
  Key := Param[1].AsString;
  Param.Result.AsInteger := int64(self.IndexOf(Key));
end;

procedure pp_strcut_valueAt(const Param: TLiParam);
var
  self: strcut;
  Index: integer;
begin
  if not Param.GetSelf(self) then Exit;
  Index := integer(Param[1].AsInteger);
  Param.Result.AsString := self.ValueAt(Index);
end;

procedure pp_strcut_keyList(const Param: TLiParam);
var
  self: strcut;
begin
  if not Param.GetSelf(self) then Exit;
  Param.Result.AsString := self.KeyList;
end;

procedure pp_strcut_valueList(const Param: TLiParam);
var
  self: strcut;
begin
  if not Param.GetSelf(self) then Exit;
  Param.Result.AsString := self.ValueList;
end;

procedure pp_strcut_readMatched(const Param: TLiParam);
var
  self: strcut;
  Source: strcut;
begin
  if not Param.GetSelf(self) then Exit;
  Source := Param[1].GetOA(my_strcut);
  Param.Result.AsInteger := int64(self.ReadMatched(Source));
end;

procedure pp_strcut_getKey(const Param: TLiParam);
var
  self: strcut;
  Index: integer;
begin
  if not Param.GetSelf(self) then Exit;
  Index := integer(Param[1].AsInteger);
  Param.Result.AsString := self.GetKey(Index);
end;

procedure pp_strcut_setKey(const Param: TLiParam);
var
  self: strcut;
  Index: integer;
  Key: string;
begin
  if not Param.GetSelf(self) then Exit;
  Index := integer(Param[1].AsInteger);
  Key := Param[2].AsString;
  self.SetKey(Index, Key);
end;

procedure pp_strcut_renameKey(const Param: TLiParam);
var
  self: strcut;
  Key: string;
  NewKey: string;
begin
  if not Param.GetSelf(self) then Exit;
  Key := Param[1].AsString;
  NewKey := Param[2].AsString;
  self.RenameKey(Key, NewKey);
end;

procedure pp_strcut_getValue(const Param: TLiParam);
var
  self: strcut;
  Name: string;
begin
  if not Param.GetSelf(self) then Exit;
  Name := Param[1].AsString;
  Param.Result.AsString := self.GetValue(Name);
end;

procedure pp_strcut_setValue(const Param: TLiParam);
var
  self: strcut;
  Name: string;
  Value: string;
begin
  if not Param.GetSelf(self) then Exit;
  Name := Param[1].AsString;
  Value := Param[2].AsString;
  self.SetValue(Name, Value);
end;

procedure pp_strcut_exchange(const Param: TLiParam);
var
  self: strcut;
  X1: integer;
  X2: integer;
begin
  if not Param.GetSelf(self) then Exit;
  X1 := integer(Param[1].AsInteger);
  X2 := integer(Param[2].AsInteger);
  self.Exchange(X1, X2);
end;

procedure pp_strcut_exchangeKey(const Param: TLiParam);
var
  self: strcut;
  Key1: string;
  Key2: string;
begin
  if not Param.GetSelf(self) then Exit;
  Key1 := Param[1].AsString;
  Key2 := Param[2].AsString;
  self.ExchangeKey(Key1, Key2);
end;

procedure pp_strcut_move(const Param: TLiParam);
var
  self: strcut;
  X1: integer;
  X2: integer;
begin
  if not Param.GetSelf(self) then Exit;
  X1 := integer(Param[1].AsInteger);
  X2 := integer(Param[2].AsInteger);
  self.Move(X1, X2);
end;

procedure pp_strcut_moveKey(const Param: TLiParam);
var
  self: strcut;
  Key1: string;
  Key2: string;
begin
  if not Param.GetSelf(self) then Exit;
  Key1 := Param[1].AsString;
  Key2 := Param[2].AsString;
  self.MoveKey(Key1, Key2);
end;

procedure pp_strcut_delete(const Param: TLiParam);
var
  self: strcut;
  X: integer;
begin
  if not Param.GetSelf(self) then Exit;
  X := integer(Param[1].AsInteger);
  self.Delete(X);
end;

procedure pp_strcut_remove(const Param: TLiParam);
var
  self: strcut;
  Key: string;
begin
  if not Param.GetSelf(self) then Exit;
  Key := Param[1].AsString;
  self.Remove(Key);
end;

procedure pp_strcut_mask(const Param: TLiParam);
var
  self: strcut;
begin
  if not Param.GetSelf(self) then Exit;
  Param.Result.AsString := self.Mask;
end;

procedure pp_strcut_delimiter(const Param: TLiParam);
var
  self: strcut;
begin
  if not Param.GetSelf(self) then Exit;
  Param.Result.AsString := self.Delimiter;
end;

procedure pp_hex(const Param: TLiParam);
var
  a: aa;
begin
  a := aa(Param[0].AsInteger);
  Param.Result.AsInteger := int64(hex(a));
end;

initialization
begin
  my_strutils := SetupModule('strutils');
  my_TStrbuf := TLiType_TStrbuf.Create('TStrbuf', my_strutils, nil);
  my_strcut := TLiType_strcut.Create('strcut', my_strutils, nil);

  my_TLiStrbuf.AddMethod(['create', 'AStr'], [my_TStrbuf, my_string],
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_create);
  my_TLiStrbuf.AddMethod(['Replace', 'patten', 'newStr', 'IgnoreCase', 'ReplaceFirstOnly'], [my_nil, my_string, my_string, my_bool, my_bool],
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_replace);
  my_TLiStrbuf.AddMethod(['Pos', 'SubStr', 'IgnoreCase', 'Start'], [my_int, my_string, my_bool, my_int],
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_pos);
  my_TLiStrbuf.AddMethod(['LastPos', 'SubStr', 'IgnoreCase', 'Start'], [my_int, my_string, my_bool, my_int],
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_lastPos);
  my_TLiStrbuf.AddMethod(['Trim'], [my_nil],
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_trim);
  my_TLiStrbuf.AddMethod(['TrimLeft'], [my_nil],
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_trimLeft);
  my_TLiStrbuf.AddMethod(['TrimRight'], [my_nil],
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_trimRight);
  my_TLiStrbuf.AddMethod(['TrimAll'], [my_nil],
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_trimAll);
  my_TLiStrbuf.AddMethod(['Lower'], [my_nil],
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_lower);
  my_TLiStrbuf.AddMethod(['Upper'], [my_nil],
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_upper);
  my_TLiStrbuf.AddMethod(['Delete', 'Index', 'Count'], [my_nil, my_int, my_int],
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_delete);
  my_TLiStrbuf.AddMethod(['Insert', 'SubStr', 'Index'], [my_nil, my_string, my_int],
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_insert);
  my_TLiStrbuf.AddMethod(['Join', 'S'], [my_nil, my_string],
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_join);
  my_TLiStrbuf.AddMethod(['Reverse'], [my_nil],
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_reverse);
  my_TLiStrbuf.AddMethod(['Randomize'], [my_nil],
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_randomize);
  my_TLiStrbuf.AddMethod(['Contains', 'SubStr', 'IgnoreCase'], [my_bool, my_string, my_bool],
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_contains);
  my_TLiStrbuf.AddMethod(['Copy', 'Index', 'Count'], [my_string, my_int, my_int],
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_copy);
  my_TLiStrbuf.SetupProp(['Buffer', my_string, [], [],
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_getBuffer,
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_setBuffer);
  my_TLiStrbuf.SetupProp(['Size', my_int, [], [],
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_getSize,
                         {$IFDEF FPC}@{$ENDIF}pp_TStrbuf_setSize);
  my_strcut.AddMethod(['create', 'AHeader', 'ADelimiter'], [my_strcut, my_string, my_string],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_create);
  my_strcut.AddMethod(['Clear'], [my_nil],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_clear);
  my_strcut.AddMethod(['ClearValues'], [my_nil],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_clearValues);
  my_strcut.AddMethod(['Init', 'AHeader', 'ADelimiter'], [my_nil, my_string, my_string],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_init);
  my_strcut.AddMethod(['Parse', 'S'], [my_nil, my_string],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_parse);
  my_strcut.AddMethod(['IndexOf', 'Key'], [my_int, my_string],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_indexOf);
  my_strcut.AddMethod(['ValueAt', 'Index'], [my_string, my_int],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_valueAt);
  my_strcut.AddMethod(['KeyList'], [my_string],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_keyList);
  my_strcut.AddMethod(['ValueList'], [my_string],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_valueList);
  my_strcut.AddMethod(['ReadMatched', 'Source'], [my_int, my_strcut],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_readMatched);
  my_strcut.AddMethod(['GetKey', 'Index'], [my_string, my_int],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_getKey);
  my_strcut.AddMethod(['SetKey', 'Index', 'Key'], [my_nil, my_int, my_string],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_setKey);
  my_strcut.AddMethod(['RenameKey', 'Key', 'NewKey'], [my_nil, my_string, my_string],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_renameKey);
  my_strcut.AddMethod(['GetValue', 'Name'], [my_string, my_string],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_getValue);
  my_strcut.AddMethod(['SetValue', 'Name', 'Value'], [my_nil, my_string, my_string],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_setValue);
  my_strcut.AddMethod(['Exchange', 'X1', 'X2'], [my_nil, my_int, my_int],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_exchange);
  my_strcut.AddMethod(['ExchangeKey', 'Key1', 'Key2'], [my_nil, my_string, my_string],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_exchangeKey);
  my_strcut.AddMethod(['Move', 'X1', 'X2'], [my_nil, my_int, my_int],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_move);
  my_strcut.AddMethod(['MoveKey', 'Key1', 'Key2'], [my_nil, my_string, my_string],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_moveKey);
  my_strcut.AddMethod(['Delete', 'X'], [my_nil, my_int],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_delete);
  my_strcut.AddMethod(['Remove', 'Key'], [my_nil, my_string],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_remove);
  my_strcut.AddMethod(['Mask'], [my_string],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_mask);
  my_strcut.AddMethod(['Delimiter'], [my_string],
                      {$IFDEF FPC}@{$ENDIF}pp_strcut_delimiter);
  my_strutils.AddFunc(['hex', 'a'], [my_int, my_int],
                      {$IFDEF FPC}@{$ENDIF}pp_hex);
end;

end.
