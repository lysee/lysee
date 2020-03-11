{==============================================================================}
{        UNIT: lysee_strutils                                                  }
{ DESCRIPTION: string utility functions (FPC)                                  }
{   COPYRIGHT: Copyright (c) 2003-2012, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/12/10                                                      }
{    MODIFIED: 2013/09/20                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lmstrutils;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$IFNDEF WINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  Classes, SysUtils, StrUtils, lbasic, lysee;

{@pmc-description string utilities}

type

  {@pmc-object strbuf => strbuf}
  {@pmc-set strbuf.t_otos => strbuf_otos}
  {@pmc-set strbuf.t_stoo => strbuf_stoo}
  {@pmc-set strbuf.t_length => strbuf_length}
  {@pmc-set strbuf.t_add => strbuf_add}
  {@pmc-set strbuf.t_getiv => strbuf_getiv}
  {@pmc-set strbuf.t_setiv => strbuf_setiv}

  strbuf = class(TLiObject)
  private
    FBuffer: string;
    function GetSize: integer;
    procedure SetSize(Size: integer);
  public
    constructor Create(const AStr: string);
    procedure Replace(const patten, newStr:string; IgnoreCase, ReplaceFirstOnly: boolean);
    function Pos(const SubStr:string; IgnoreCase:boolean; Start: integer): integer;
    function LastPos(const SubStr:string; IgnoreCase:boolean; Start: integer): integer;
    procedure Trim;
    procedure TrimLeft;
    procedure TrimRight;
    procedure TrimAll;
    procedure Lower;
    procedure Upper;
    procedure Delete(Index, Count:integer);
    procedure Insert(const SubStr:string; Index: integer);
    procedure Join(const S: string);
    procedure Reverse;
    procedure Randomize;
    function Contains(const SubStr: string; IgnoreCase: boolean): boolean;
    function Copy(Index, Count: int64): string;
    property Buffer: string read FBuffer write FBuffer;
    property Size: integer read GetSize write SetSize;
  end;

  {@pmc-object strcut => strcut}
  {@pmc-set strcut.t_length => strcut_length}
  {@pmc-set strcut.t_getiv => strcut_getiv}
  {@pmc-set strcut.t_setiv => strcut_setiv}
  {@pmc-set strcut.t_getpv => strcut_getpv}
  {@pmc-set strcut.t_setpv => strcut_setpv}

  strcut = class(TLiObject)
  private
    FDelimiter: char;
    FMask: array of integer;
    FKeyList: array of string;
    FValueList: array of string;
    function GetCount: integer;
    function ParseNext(var S: pchar): string;
    function DeQuote(var S: pchar): string;
  public
    constructor Create(const AHeader, ADelimiter: string);
    procedure Clear;
    procedure ClearValues;
    procedure Init(const AHeader, ADelimiter: string);
    procedure Parse(const S: string);
    function IndexOf(const Key: string): integer;
    function ValueAt(Index: integer): string;
    function KeyList: string;
    function ValueList: string;
    function ReadMatched(Source: strcut): integer;
    function GetKey(Index: integer): string;
    procedure SetKey(Index: integer; const Key: string);
    procedure RenameKey(const Key, NewKey: string);
    function GetValue(const Name: string): string;
    procedure SetValue(const Name, Value: string);
    procedure Exchange(X1, X2: integer);
    procedure ExchangeKey(const Key1, Key2: string);
    procedure Move(X1, X2: Integer);
    procedure MoveKey(const Key1, Key2: string);
    procedure Delete(X: integer);
    procedure Remove(const Key: string);
    function Mask: string;
    function Delimiter: string;
  end;

  {@pmc-object strings => strings}
  {@pmc-set strings.t_otos => strings_otos}
  {@pmc-set strings.t_stoo => strings_stoo}
  {@pmc-set strings.t_add => strings_add}
  {@pmc-set strings.t_insert => strings_insert}
  {@pmc-set strings.t_length => strings_length}
  {@pmc-set strings.t_delete => strings_delete}
  {@pmc-set strings.t_clear => strings_clear}
  {@pmc-set strings.t_getiv => strings_getiv}
  {@pmc-set strings.t_setiv => strings_setiv}
  {@pmc-set strings.t_getpv => strings_getpv}
  {@pmc-set strings.t_setpv => strings_setpv}
  {@pmc-set strings.t_remove => strings_remove}

  { strings }

  strings = class(TLiObject)
  private
    FStrings: TStringList;
    function GetCount: integer;
    function GetName(Index: integer): string;
    function GetCommaText: string;
    procedure SetCommaText(const AValue: string);
    function GetText: string;
    procedure SetText(const AValue: string);
    function GetValue(const Name: string): string;
    procedure SetValue(const Name: string; const AValue: string);
    function GetCaseSensitive: boolean;
    procedure SetCaseSensitive(AValue: boolean);
    function Get(Index: integer): string;
    procedure Put(Index: integer; const Value: string);
  public
    constructor Create;
    destructor Destroy;override;
    procedure AddStrings(Source: strings);
    procedure Assign(Source: strings);
    function EqualWith(Source: strings): boolean;
    function IndexOf(const S: string): integer;
    function IndexOfName(const Name: string): integer;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    function Add(const S: string): integer;
    procedure Clear;
    procedure Delete(Index: integer);
    procedure Move(CurIndex, NewIndex: integer);
    procedure Exchange(Index1, Index2: integer);
    procedure Insert(Index: integer; const S: string);
    procedure Sort;
    function Copy(Index, ItemCount: integer): strings;
    function Left(ItemCount: integer): strings;
    function Right(ItemCount: integer): strings;
    property CommaText: string read GetCommaText write SetCommaText;
    property Names[Index: integer]: string read GetName;
    property Values[const Name: string]: string read GetValue write SetValue;
    property Text: string read GetText write SetText;
    property CaseSensitive: boolean read GetCaseSensitive write SetCaseSensitive;
    property Count: integer read GetCount;
    property Strings[Index: integer]: string read Get write Put; default;
  end;

{$IFDEF PMC_AVAILABLE}
{$ENDIF}

{@pmc-end}

function strbuf_otos(obj: pointer): PLiString;
function strbuf_stoo(const str: PLiString): pointer;
function strbuf_add(obj: pointer; value: PLiValue): integer;
function strbuf_length(obj: pointer): integer;
function strbuf_getiv(obj: pointer; index: integer; value: PLiValue): integer;
function strbuf_setiv(obj: pointer; index: integer; value: PLiValue): integer;

function strcut_getiv(obj: pointer; index: integer; value: PLiValue): integer;
function strcut_setiv(obj: pointer; index: integer; value: PLiValue): integer;
function strcut_length(obj: pointer): integer;
function strcut_getpv(obj: pointer; const prop: PLiString; value: PLiValue): integer;
function strcut_setpv(obj: pointer; const prop: PLiString; value: PLiValue): integer;

function strings_otos(obj: pointer): PLiString;
function strings_stoo(const str: PLiString): pointer;
function strings_add(obj: pointer; value: PLiValue): integer;
function strings_getiv(obj: pointer; index: integer; value: PLiValue): integer;
function strings_setiv(obj: pointer; index: integer; value: PLiValue): integer;
function strings_length(obj: pointer): integer;
function strings_clear(obj: pointer): integer;
function strings_getpv(obj: pointer; const prop: PLiString; value: PLiValue): integer;
function strings_setpv(obj: pointer; const prop: PLiString; value: PLiValue): integer;
function strings_insert(obj: pointer; X: integer; V: PLiValue): integer;
function strings_delete(obj: pointer; X: integer): integer;
function strings_remove(obj: pointer; const Prop: PLiString): integer;
function strings_eachp(obj: pointer; EnumProc: TLiEnumProp; data: pointer): integer;

implementation

function strbuf_otos(obj: pointer): PLiString;
begin
  if obj <> nil then
    Result := string_Alloc(strbuf(obj).FBuffer) else
    Result := nil;
end;

function strbuf_stoo(const str: PLiString): pointer;
begin
  Result := strbuf.Create(string_s(str));
end;

function strbuf_add(obj: pointer; value: PLiValue): integer;
var
  S: strbuf;
begin
  Result := 0;
  if obj <> nil then
  begin
    S := strbuf(obj);
    S.FBuffer := S.FBuffer + value_String(value);
    Result := 1;
  end;
end;

function strbuf_length(obj: pointer): integer;
begin
  if obj <> nil then
    Result := Length(strbuf(obj).FBuffer) else
    Result := 0;
end;

function strbuf_getiv(obj: pointer; index: integer; value: PLiValue): integer;
var
  S: strbuf;
  L: integer;
begin
  Result := 0;
  if obj <> nil then
  begin
    S := strbuf(obj);
    L := S.GetSize;
    if index < 0 then Inc(index, L);
    if (index >= 0) and (index < L) then
    begin
      value_SetChar(value, S.FBuffer[index + 1]);
      Result := 1;
    end;
  end;
end;

function strbuf_setiv(obj: pointer; index: integer; value: PLiValue): integer;
var
  S: strbuf;
  L: integer;
begin
  Result := 0;
  if obj <> nil then
  begin
    S := strbuf(obj);
    L := S.GetSize;
    if index < 0 then Inc(index, L);
    if (index >= 0) and (index < L) then
    begin
      S.FBuffer[index + 1] := value_Char(value);
      Result := 1;
    end;
  end;
end;

function strcut_getiv(obj: pointer; index: integer; value: PLiValue): integer;
var
  S: strcut;
  L: integer;
begin
  Result := 0;
  if obj <> nil then
  begin
    S := strcut(obj);
    L := S.GetCount;
    if index < 0 then Inc(index, L);
    if (index >= 0) and (index < L) then
    begin
      value_SetString(value, S.ValueAt(index));
      Result := 1;
    end;
  end;
end;

function strcut_setiv(obj: pointer; index: integer; value: PLiValue): integer;
var
  S: strcut;
  L: integer;
begin
  Result := 0;
  if obj <> nil then
  begin
    S := strcut(obj);
    L := S.GetCount;
    if index < 0 then Inc(index, L);
    if (index >= 0) and (index < L) then
    begin
      S.FValueList[index] := value_String(value);
      Result := 1;
    end;
  end;
end;

function strcut_length(obj: pointer): integer;
begin
  if obj <> nil then
    Result := strcut(obj).GetCount else
    Result := 0;
end;

function strcut_getpv(obj: pointer; const prop: PLiString; value: PLiValue): integer;
var
  S: strcut;
  X: integer;
begin
  Result := 0;
  if obj <> nil then
  begin
    S := strcut(obj);
    X := S.IndexOf(string_s(prop));
    if X >= 0 then
    begin
      value_SetString(value, S.ValueAt(X));
      Result := 1;
    end;
  end;
end;

function strcut_setpv(obj: pointer; const prop: PLiString; value: PLiValue): integer;
var
  S: strcut;
  X: integer;
begin
  Result := 0;
  if obj <> nil then
  begin
    S := strcut(obj);
    X := S.IndexOf(string_s(prop));
    if X >= 0 then
    begin
      S.FValueList[X] := value_String(value);
      Result := 1;
    end;
  end;
end;

function strings_otos(obj: pointer): PLiString;
begin
  if obj <> nil then
    Result := string_Alloc(strings(obj).Text) else
    Result := nil;
end;

function strings_stoo(const str: PLiString): pointer;
var
  L: strings;
begin
  L := strings.Create;
  L.Text := string_s(str);
  Result := L;
end;

function strings_add(obj: pointer; value: PLiValue): integer;
begin
  if obj <> nil then
  begin
    strings(obj).FStrings.Add(value_String(value));
    Result := 1;
  end
  else Result := 0;
end;

function strings_getiv(obj: pointer; index: integer; value: PLiValue): integer;
var
  S: strings;
  L: integer;
begin
  Result := 0;
  if obj <> nil then
  begin
    S := strings(obj);
    L := S.FStrings.Count;
    if index < 0 then Inc(index, L);
    if (index >= 0) and (index < L) then
    begin
      value_SetString(value, S.FStrings[index]);
      Result := 1;
    end;
  end;
end;

function strings_setiv(obj: pointer; index: integer; value: PLiValue): integer;
var
  S: strings;
  L: integer;
begin
  Result := 0;
  if obj <> nil then
  begin
    S := strings(obj);
    L := S.FStrings.Count;
    if index < 0 then Inc(index, L);
    if (index >= 0) and (index < L) then
    begin
      S.FStrings[index] := value_String(value);
      Result := 1;
    end;
  end;
end;

function strings_length(obj: pointer): integer;
begin
  if obj <> nil then
    Result := strings(obj).FStrings.Count else
    Result := 0;
end;

function strings_clear(obj: pointer): integer;
begin
  if obj <> nil then
  begin
    Result := strings(obj).Count;
    strings(obj).FStrings.Clear;
  end
  else Result := 0;
end;

function strings_getpv(obj: pointer; const prop: PLiString; value: PLiValue): integer;
var
  S: strings;
  X: integer;
begin
  Result := 0;
  if obj <> nil then
  begin
    S := strings(obj);
    X := S.IndexOfName(string_s(prop));
    if X >= 0 then
    begin
      value_SetString(value, ExtractValue(S.FStrings[X]));
      Result := 1;
    end;
  end;
end;

function strings_setpv(obj: pointer; const prop: PLiString; value: PLiValue): integer;
begin
  if obj <> nil then
  begin
    strings(obj).Values[string_s(prop)] := value_String(value);
    Result := 1;
  end
  else Result := 0;
end;

function strings_insert(obj: pointer; X: integer; V: PLiValue): integer;
var
  S: strings;
  L: integer;
begin
  Result := 0;
  if obj <> nil then
  begin
    S := strings(obj);
    L := S.FStrings.Count;
    if X < 0 then Inc(X, L);
    if (X >= 0) and (X <= L) then
    begin
      S.FStrings.Insert(X, value_String(V));
      Result := 1;
    end;
  end;
end;

function strings_delete(obj: pointer; X: integer): integer;
var
  S: strings;
  L: integer;
begin
  Result := 0;
  if obj <> nil then
  begin
    S := strings(obj);
    L := S.FStrings.Count;
    if X < 0 then Inc(X, L);
    if (X >= 0) and (X < L) then
    begin
      S.FStrings.Delete(X);
      Result := 1;
    end;
  end;
end;

function strings_remove(obj: pointer; const Prop: PLiString): integer;
var
  S: strings;
  X: integer;
begin
  Result := 0;
  if obj <> nil then
  begin
    S := strings(obj);
    X := S.FStrings.IndexOfName(string_s(prop));
    if X >= 0 then
      S.FStrings.Delete(X);
    Result := 1;
  end;
end;

function strings_eachp(obj: pointer; EnumProc: TLiEnumProp; data: pointer): integer;
var
  S: strings;
  X: integer;
  V: PLiString;
  P: string;
  F: boolean;
begin
  Result := 0;
  if (obj <> nil) and Assigned(EnumProc) then
  begin
    S := strings(obj);
    for X := 0 to S.FStrings.Count - 1 do
    begin
      P := S.FStrings.Names[X];
      if P <> '' then
      begin
        V := string_Alloc(P);
        string_Addref(V);
        try
          Inc(Result);
          F := EnumProc(V, data);
        finally
          string_Release(V);
        end;
        if not F then Exit;
      end;
    end;
  end;
end;

{ strbuf }

constructor strbuf.Create(const AStr: string);
begin
  inherited Create;
  FBuffer := AStr;
end;

procedure strbuf.TrimAll;
var
  slen, L, M, R: integer;
  base, next: pchar;
  temp: string;
begin
  base := pchar(FBuffer);
  slen := Length(FBuffer);
  if GetTabs(base, slen, L, M, R) > 0 then
  begin
    SetLength(temp, slen - (L + M + R));
    next := pchar(temp);
    while base^ <> #0  do
    begin
      if not InChars(base^, CS_SPACE) then
      begin
        next^ := base^;
        Inc(next);
      end;
      Inc(base);
    end;
    FBuffer := temp;
  end;
end;

procedure strbuf.Lower;
begin
  FBuffer := LowerCase(FBuffer);
end;

procedure strbuf.Upper;
begin
  FBuffer := UpperCase(FBuffer);
end;

procedure strbuf.Delete(Index, Count: integer);
begin
  if Count = 0 then Count := 1;
  Index := ResetIndex(Index, GetSize);
  if Index < 0 then
  begin
    Inc(Count, Index);
    Index := 0;
  end;
  if (Count > 0) and (Index < GetSize) then
    System.Delete(FBuffer, Index + 1, count);
end;

procedure strbuf.Insert(const SubStr: string; Index: integer);
begin
  if SubStr <> '' then
  begin
    Index := ResetIndex(Index, GetSize);
    if (Index >= 0) and (index <= GetSize) then
      System.Insert(SubStr, FBuffer, Index + 1);
  end;
end;

procedure strbuf.Join(const S: string);
begin
  FBuffer := FBuffer + S;
end;

procedure strbuf.Reverse;
begin
  FBuffer := ReverseString(FBuffer);
end;

procedure strbuf.Randomize;
var
  I, L, X: integer;
  ch: char;
begin
  L := GetSize;
  if L > 1 then for I := 1 to L do
  begin
    X := random(L) + 1;
    if I <> X then
    begin
      ch := FBuffer[X];
      FBuffer[X] := FBuffer[I];
      FBuffer[I] := ch;
    end;
  end;
end;

function strbuf.Contains(const SubStr: string; IgnoreCase: boolean): boolean;
begin
  Result := (Pos(SubStr, IgnoreCase, 0) >= 0);
end;

function strbuf.Copy(Index, Count: int64): string;
begin
  if FBuffer <> '' then
  begin
    Index := ResetRange(Index, GetSize, Count);
    Result := System.Copy(FBuffer, Index + 1, Count);
  end
  else Result := '';
end;

function strbuf.GetSize: integer;
begin
  Result := Length(FBuffer);
end;

procedure strbuf.SetSize(Size: integer);
begin
  if Size > 0 then
    SetLength(FBuffer, Size) else
    FBuffer := '';
end;

procedure strbuf.Replace(const patten, newStr: string; IgnoreCase,
  ReplaceFirstOnly: boolean);
var
  flags: TReplaceFlags;
begin
  flags := [rfReplaceAll];
  if IgnoreCase then
    flags := flags + [rfIgnoreCase];
  if ReplaceFirstOnly then
    flags := flags - [rfReplaceAll];
  FBuffer := StringReplace(FBuffer, patten, newStr, flags);
end;

function strbuf.Pos(const SubStr: string;
  IgnoreCase: boolean; Start: integer): integer;
var
  base, str: pchar;
  I: integer;
begin
  start := ResetIndex(Start, GetSize);
  if (start >= 0) and (start < GetSize) then
  begin
    base := pchar(FBuffer);
    str := base + start;
    I := -1; //lse_posL(str, GetSize, pchar(SubStr), Length(SubStr), not IgnoreCase);
    if I >= 0 then
    begin
      Result := start + I;
      Exit;
    end;
  end;
  Result := -1;
end;

function strbuf.LastPos(const SubStr: string;
  IgnoreCase: boolean; Start: integer): integer;
var
  base, str: pchar;
  endx, I: integer;
begin
  if Start = 0 then
    endx := GetSize else
    endx := Start;
  if (endx > 0) and (endx <= GetSize) then
  begin
    base := pchar(FBuffer);
    str := base;
    I := -1;//lse_posR(str, endx, pchar(SubStr), Length(SubStr), not IgnoreCase);
    if I >= 0 then
    begin
      Result := (str - base) + I;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure strbuf.Trim;
begin
  FBuffer := SysUtils.Trim(FBuffer);
end;

procedure strbuf.TrimLeft;
begin
  FBuffer := SysUtils.TrimLeft(FBuffer);
end;

procedure strbuf.TrimRight;
begin
  FBuffer := SysUtils.TrimRight(FBuffer);
end;

{ strcut }

procedure strcut.Clear;
var
  index: integer;
begin
  FDelimiter := ',';
  SetLength(FMask, 0);
  for index := 0 to Length(FKeyList) - 1 do
    FKeyList[index] := '';
  SetLength(FKeyList, 0);
  ClearValues;
  SetLength(FValueList, 0);
end;

procedure strcut.ClearValues;
var
  index: integer;
begin
  for index := 0 to Length(FValueList) - 1 do
    FValueList[index] := '';
end;

constructor strcut.Create(const AHeader, ADelimiter: string);
begin
  Init(AHeader, ADelimiter);
end;

procedure strcut.Delete(X: integer);
var
  index, N: integer;
begin
  CheckIndex(X, GetCount);
  N := GetCount - 1;
  for index := X to N - 1 do
  begin
    FKeyList[index] := FKeyList[index + 1];
    FValueList[index] := FValueList[index + 1];
  end;
  SetLength(FKeyList, N);
  SetLength(FValueList, N);
  for index := 0 to Length(FMask) - 1 do
    if FMask[index] = X then FMask[index] := -1 else
    if FMask[index] > X then Dec(FMask[index]);
end;

function strcut.DeQuote(var S: pchar): string;
var
  quotec: char;
begin
  Result := '';
  if (S <> nil) and (S^ <> #0) then
  begin
    quotec := S^;
    Inc(S);
    while S^ <> #0 do
    begin
      if S^ = quotec then // "" ''
      begin
        Inc(S);
        if S^ <> quotec then Exit;
      end;
      Result := Result + S^;
      Inc(S);
    end;
    if S^ = quotec then Inc(S);
  end;
end;

procedure strcut.Exchange(X1, X2: integer);
var
  index: integer;
  temp: string;
begin
  if X1 <> X2 then
  begin
    CheckIndex(X1, GetCount);
    CheckIndex(X2, GetCount);

    temp := FKeyList[X1];
    FKeyList[X1] := FKeyList[X2];
    FKeyList[X2] := temp;

    temp := FValueList[X1];
    FValueList[X1] := FValueList[X2];
    FValueList[X2] := temp;

    for index := 0 to Length(FMask) - 1 do
      if FMask[index] = X1 then FMask[index] := X2 else
      if FMask[index] = X2 then FMask[index] := X1;
  end;
end;

procedure strcut.ExchangeKey(const Key1, Key2: string);
begin
  Exchange(IndexOf(Key1), IndexOf(Key2));
end;

function strcut.GetCount: integer;
begin
  Result := Length(FKeyList);
end;

function strcut.GetValue(const Name: string): string;
begin
  Result := ValueAt(IndexOf(Name));
end;

function strcut.IndexOf(const Key: string): integer;
var
  index: integer;
begin
  for index := 0 to Length(FKeyList) - 1 do
    if AnsiSameText(Key, FKeyList[index]) then
    begin
      Result := index;
      Exit;
    end;
  Result := -1;
end;

procedure strcut.Init(const AHeader, ADelimiter: string);
var
  source: pchar;
  column: string;
  total, index: integer;
begin
  Clear;
  if ADelimiter <> '' then
    FDelimiter := ADelimiter[1] else
    FDelimiter := ',';
  total := 0;
  index := 0;
  source := pchar(AHeader);
  while (source <> nil) and (source^ <> #0) do
  begin
    column := Trim(ParseNext(source));
    SetLength(FMask, total + 1);
    if column <> '' then
    begin
      FMask[total] := index;
      SetLength(FKeyList, index + 1);
      FKeyList[index] := column;
      SetLength(FValueList, index + 1);
      FValueList[index] := '';
      Inc(index);
    end
    else FMask[total] := -1;
    Inc(total);
  end;
end;

procedure strcut.Move(X1, X2: integer);
begin
  if X1 <> X2 then
  begin
    CheckIndex(X1, GetCount);
    CheckIndex(X2, GetCount);

    while X1 < X2 do
    begin
      Exchange(X1, X1 + 1);
      Inc(X1);
    end;

    while X1 > X2 do
    begin
      Exchange(X1, X1 - 1);
      Dec(X1);
    end;
  end;
end;

procedure strcut.MoveKey(const Key1, Key2: string);
begin
  Move(IndexOf(Key1), IndexOf(Key2));
end;

function strcut.KeyList: string;
var
  index: integer;
begin
  if GetCount > 0 then
  begin
    Result := FKeyList[0];
    for index := 1 to GetCount - 1 do
      Result := Result + FDelimiter + FKeyList[index];
  end
  else Result := '';
end;

procedure strcut.Parse(const S: string);
var
  source: pchar;
  value: string;
  index, N: integer;
begin
  ClearValues;
  index := 0;
  N := GetCount;
  source := pchar(S);
  while (N > 0) and (source <> nil) and (source^ <> #0) do
  begin
    value := ParseNext(source);
    if FMask[index] >= 0 then
    begin
      FValueList[FMask[index]] := value;
      Dec(N);
    end;
    Inc(index);
  end;
end;

function strcut.ParseNext(var S: pchar): string;
var
  base: pchar;
begin
  Result := '';
  if S = nil then Exit;
  if S^ = #0 then
  begin
    S := nil;
    Exit;
  end;

  if FDelimiter = ' ' then
  begin
    S := SkipChar(S, CS_SPACE);
    if S^ = #0 then
    begin
      S := nil;
      Exit;
    end;
  end
  else
  if S^ = FDelimiter then
  begin
    Inc(S);
    Exit;
  end;

  if S^ in ['"', ''''] then
  begin
    Result := DeQuote(S);
    if (S^ <> #0) and ((FDelimiter <> ' ') or not InChars(S^, CS_SPACE)) then
      Result := Result + ParseNext(S) else
      S := nil;
  end
  else
  begin
    base := S;
    while not (S^ in [FDelimiter, #0]) do Inc(S);
    SetString(Result, base, S - base);
    if S^ = #0 then
      S := nil else
      Inc(S);
  end;
end;

function strcut.ReadMatched(Source: strcut): integer;
var
  index, X: integer;
begin
  Result := 0;

  if (Source = nil) or (Source = Self) or (Source.GetCount = 0)
  or (GetCount = 0) then Exit;

  for index := 0 to GetCount - 1 do
  begin
    X := Source.IndexOf(FKeyList[index]);
    if X >= 0 then
    begin
      FValueList[index] := Source.FValueList[X];
      Inc(Result);
    end;
  end;
end;

function strcut.GetKey(Index: integer): string;
begin
  Index := ResetIndex(Index, GetCount);
  Result := FKeyList[Index];
end;

procedure strcut.SetKey(Index: integer; const Key: string);
begin
  Index := ResetIndex(Index, GetCount);
  FKeyList[Index] := Key;
end;

procedure strcut.RenameKey(const Key, NewKey: string);
begin
  FKeyList[IndexOf(Key)] := NewKey;
end;

procedure strcut.Remove(const Key: string);
var
  index: integer;
begin
  index := IndexOf(Key);
  if index >= 0 then
    Delete(index);
end;

function strcut.Mask: string;
var
  index, X: integer;
begin
  Result := '';
  for index := 0 to Length(FMask) - 1 do
  begin
    X := FMask[index];
    if X >= 0 then
      Result := Result + FKeyList[X] + FDelimiter else
      Result := Result + FDelimiter;
  end;
  X := Length(Result);
  if (X > 1) and (Result[X - 1] <> FDelimiter) then
    SetLength(Result, X - 1);
end;

function strcut.Delimiter: string;
begin
  Result := FDelimiter;
end;

procedure strcut.SetValue(const Name, Value: string);
begin
  FValueList[IndexOf(Name)] := Value;
end;

function strcut.ValueAt(Index: integer): string;
begin
  Result := FValueList[Index];
end;

function strcut.ValueList: string;
var
  index: integer;
begin
  if GetCount > 0 then
  begin
    Result := FValueList[0];
    for index := 1 to GetCount - 1 do
      Result := Result + FDelimiter + FValueList[index];
  end
  else Result := '';
end;

{ strings }

function strings.GetCommaText: string;
begin
  Result := FStrings.CommaText;
end;

procedure strings.SetCaseSensitive(AValue: boolean);
begin
  Fstrings.CaseSensitive := AValue;;
end;

function strings.GetName(Index: integer): string;
begin
  Result := FStrings.Names[ResetIndex(Index, GetCount)];
end;

function strings.GetCount: integer;
begin
  Result := FStrings.Count;
end;

function strings.Get(Index: integer): string;
begin

end;

function strings.GetCaseSensitive: boolean;
begin
  Result := FStrings.CaseSensitive;
end;

function strings.GetText: string;
begin
  Result := FStrings.Text;
end;

function strings.GetValue(const Name: string): string;
begin
  Result := FStrings.Values[Name];
end;

procedure strings.SetCommaText(const AValue: string);
begin
  FStrings.CommaText := AValue;
end;

procedure strings.SetText(const AValue: string);
begin
  FStrings.Text := AValue;
end;

procedure strings.SetValue(const Name: string; const AValue: string);
begin
  FStrings.Values[Name] := AValue;
end;

constructor strings.Create;
begin
  FStrings := TStringList.Create;
end;

destructor strings.Destroy;
begin
  FreeAndNil(FStrings);
  inherited Destroy;
end;

procedure strings.AddStrings(Source: strings);
begin
  if (Source <> nil) and (Source <> Self) then
    FStrings.AddStrings(Source.FStrings);
end;

procedure strings.Assign(Source: strings);
begin
  if Source <> Self then
    if Source <> nil then
      FStrings.Assign(Source.FStrings) else
      FStrings.Clear;
end;

function strings.EqualWith(Source: strings): boolean;
begin
  Result := (Self = Source);
  if not Result and (Source <> nil) then
    Result := FStrings.Equals(Source.FStrings);
end;

function strings.IndexOf(const S: string): integer;
begin
  Result := FStrings.IndexOf(S);
end;

function strings.IndexOfName(const Name: string): integer;
begin
  Result := FStrings.IndexOfName(Name);
end;

procedure strings.LoadFromFile(const FileName: string);
begin
  FStrings.LoadFromFile(FileName);
end;

procedure strings.SaveToFile(const FileName: string);
begin
  FStrings.SaveToFile(FileName);
end;

function strings.Add(const S: string): integer;
begin
  Result := FStrings.Add(S);
end;

procedure strings.Clear;
begin
  FStrings.Clear;
end;

procedure strings.Delete(Index: integer);
begin
  FStrings.Delete(ResetIndex(Index, GetCount));
end;

procedure strings.Move(CurIndex, NewIndex: integer);
begin
  FStrings.Move(ResetIndex(CurIndex, GetCount),
    ResetIndex(NewIndex, GetCount));
end;

procedure strings.Put(Index: integer; const Value: string);
begin

end;

procedure strings.Exchange(Index1, Index2: integer);
begin
  FStrings.Exchange(ResetIndex(Index1, GetCount),
    ResetIndex(Index2, GetCount));
end;

procedure strings.Insert(Index: integer; const S: string);
begin
  FStrings.Insert(ResetIndex(Index, GetCount), S);
end;

procedure strings.Sort;
begin
  FStrings.Sort;
end;

function strings.Copy(Index, ItemCount: integer): strings;
begin
  Result := strings.Create;
  Index := ResetRange(Index, GetCount, ItemCount);
  while (Index < GetCount) and (ItemCount > 0) do
  begin
    Result.Add(FStrings[Index]);
    Inc(Index);
    Dec(ItemCount);
  end;
end;

function strings.Left(ItemCount: integer): strings;
begin
  Result := Copy(0, ItemCount);
end;

function strings.Right(ItemCount: integer): strings;
begin
  Result := Copy(-ItemCount, ItemCount);
end;

end.

