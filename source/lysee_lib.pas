{==============================================================================}
{        UNIT: lysee_lib                                                       }
{ DESCRIPTION: load lysee module from library                                  }
{   COPYRIGHT: Copyright (c) 2019, Li Yun Jie. All Rigrhts Reserved.           }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2019/03/30                                                      }
{    MODIFIED: 2020/03/11                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_lib;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, iLysee, Lysee;

function LoadFromLibrary(const Name, FileName: string): TLyModule;

implementation

uses
  Basic, lysee_system;

function my_find_by_id(ID: integer): pointer;
begin
  Result := nil;
  case ID of
    LID_SYSTEM_MODULE : Result := lysee.my_system;
    LID_INTEGER_TYPE  : Result := lysee.my_int;
    LID_STRING_TYPE   : Result := lysee.my_string;
    LID_CHAR_TYPE     : Result := lysee.my_char;
    LID_BOOLEAN_TYPE  : Result := lysee.my_bool;
    LID_FLOAT_TYPE    : Result := lysee.my_float;
    LID_CURRENCY_TYPE : Result := lysee.my_curr;
    LID_DATETIME_TYPE : Result := lysee.my_time;
    LID_VARIANT_TYPE  : Result := lysee.my_variant;
    LID_NIL_TYPE      : Result := lysee.my_nil;
    LID_TYPE_TYPE     : Result := lysee.my_type;
    LID_EXCEPTION_TYPE: Result := lysee.my_error;
    LID_MODULE_TYPE   : Result := lysee.my_module;
    LID_FUNC_TYPE     : Result := lysee.my_func;
    LID_OBJECT_TYPE   : Result := lysee.my_object;
    LID_ARRAY_TYPE    : Result := lysee.my_array;
    LID_LIST_TYPE     : Result := lysee_system.my_list;
    LID_HASH_TYPE     : Result := lysee_system.my_hash;
    LID_STRINGS_TYPE  : Result := lysee_system.my_strings;
    LID_STRLIST_TYPE  : Result := lysee_system.my_strlist;
  end;
end;

function my_find_by_name(const Name: PAnsiChar): pointer;

  function find_in(M: TLyModule; const ID: string): pointer;
  begin
    Result := M.FindType(ID);
    if Result = nil then
      Result := M.FindFunc(ID);
  end;

var
  K: string;
  I: integer;
  M: TLyModule;
begin
  Result := nil;
  K := LyAnsiToStr(Name);
  if K <> '' then
  begin
    I := Pos('.', K);
    if I > 0 then
    begin
      M := my_modules.Find(Copy(K, 1, I - 1));
      if M <> nil then
      begin
        K := Copy(K, I + 1, Length(K));
        Result := find_in(M, K);
      end;
    end
    else
    begin
      Result := my_modules.Find(K);
      if Result <> nil then Exit;

      Result := find_in(lysee.my_system, K);
      if Result <> nil then Exit;

      for I := 0 to my_modules.Count - 1 do
      begin
        M := my_modules[I];
        if (M <> lysee.my_system) and M.IsPublic then
        begin
          Result := find_in(M, K);
          if Result <> nil then Exit;
        end;
      end;
    end;
  end;
end;

function my_find_module(const Name: PAnsiChar): PLyModule;
begin
  Result := my_modules.Find(LyAnsiToStr(Name));
end;

function my_find_type(const Name: PAnsiChar): PLyType;
var
  K: string;
  I: integer;
  M: TLyModule;
begin
  Result := nil;
  K := LyAnsiToStr(Name);
  if K <> '' then
  begin
    I := Pos('.', K);
    if I > 0 then
    begin
      M := my_modules.Find(Copy(K, 1, I - 1));
      if M <> nil then
      begin
        K := Copy(K, I + 1, Length(K));
        Result := M.FindType(K);
      end;
    end
    else
    begin
      Result := lysee.my_system.FindType(K);
      if Result = nil then
        for I := 0 to my_modules.Count - 1 do
        begin
          M := my_modules[I];
          if (M <> lysee.my_system) and M.IsPublic then
          begin
            Result := M.FindType(K);
            if Result <> nil then Exit;
          end;
        end;
    end;
  end;
end;

function my_add_func(M: PLyModule; const Name: PAnsiChar; T: PLyType; Proc: TLyseeProc): PLyFunc;
var
  L: TLyModule;
  N: string;
begin
  Result := nil;
  N := LyAnsiToStr(Name);
  if IsID(N) and Assigned(Proc) then
  begin
    if M = nil then
      L := lysee.my_system else
      L := TLyModule(M);
    if L.FindFunc(N) = nil then
      Result := L.AddFunc(N, TLyType(T), [], [], TLyProc(Proc));
  end;
end;

function my_add_param(F: PLyFunc; const Name: PAnsiChar; T: PLyType): boolean;
var
  N: string;
begin
  Result := (F <> nil) and (T <> nil) and (TLyType(T) <> lysee.my_nil);
  if Result then
  begin
    N := LyAnsiToStr(Name);
    TLyFunc(F).Params.Add(N, TLyType(T));
  end;
end;

function my_add_method(AType: PLyType; const Name: PAnsiChar; T: PLyType; Proc: TLyseeProc): PLyFunc;
var
  P: TLyType;
  N: string;
  F: TLyFunc;
begin
  Result := nil;
  if (AType <> nil) and Assigned(Proc) then
  begin
    P := TLyType(AType);
    N := LyAnsiToStr(Name);
    if (N <> '') and (P.FindMethod(N, nil) = nil) then
    begin
      if T = nil then T := lysee.my_nil;
      if not MatchID(N, LSE_CREATE) or (P = TLyType(T)) then
      begin
        F := TLyFunc.CreateMethod(N, P, TLyProc(Proc));
        F.ResultType := TLyType(T);
        F.Params.Add('Self', P);
        if MatchID(N, 'get[]') then P.GetFunc := F else
        if MatchID(N, 'set[]') then P.SetFunc := F;
        if MatchID(N, LSE_CREATE) then P.Creater := F;
        Result := F;
      end;
    end;
  end;
end;

{ param }

function my_param_count(P: PLyParam): integer;
begin
  if P <> nil then
    Result := TLyParam(P).Count else
    Result := 0;
end;

function my_param_prmc(P: PLyParam): integer;
begin
  if P <> nil then
    Result := TLyParam(P).Prmc else
    Result := 0;
end;

function my_param_type(P: PLyParam; X: integer): PLyType;
begin
  if P = nil then Result := nil else
  if (X >= 0) and (X < TLyParam(P).Count) then
    Result := TLyParam(P).Params[X].VType else
    Result := nil;
end;

function my_param_integer(P: PLyParam; X: integer): int64;
begin
  if P = nil then Result := 0 else
  if (X >= 0) and (X < TLyParam(P).Count) then
    Result := TLyParam(P).Params[X].AsInteger else
    Result := 0;
end;

function my_param_boolean(P: PLyParam; X: integer): boolean;
begin
  if P = nil then Result := false else
  if (X >= 0) and (X < TLyParam(P).Count) then
    Result := TLyParam(P).Params[X].AsBoolean else
    Result := false;
end;

function my_param_double(P: PLyParam; X: integer): double;
begin
  if P = nil then Result := 0 else
  if (X >= 0) and (X < TLyParam(P).Count) then
    Result := TLyParam(P).Params[X].AsFloat else
    Result := 0;
end;

function my_param_currency(P: PLyParam; X: integer): currency;
begin
  if P = nil then Result := 0 else
  if (X >= 0) and (X < TLyParam(P).Count) then
    Result := TLyParam(P).Params[X].AsCurrency else
    Result := 0;
end;

function my_param_time(P: PLyParam; X: integer): TDateTime;
begin
  if P = nil then Result := 0 else
  if (X >= 0) and (X < TLyParam(P).Count) then
    Result := TLyParam(P).Params[X].AsTime else
    Result := 0;
end;

function my_param_charA(P: PLyParam; X: integer): AnsiChar;
begin
  if P = nil then Result := #0 else
  if (X >= 0) and (X < TLyParam(P).Count) then
    Result := AnsiChar(TLyParam(P).Params[X].AsChar) else
    Result := #0;
end;

function my_param_charW(P: PLyParam; X: integer): WideChar;
begin
  if P = nil then Result := #0 else
  if (X >= 0) and (X < TLyParam(P).Count) then
    Result := WideChar(TLyParam(P).Params[X].AsChar) else
    Result := #0;
end;

function my_param_pchar(P: PLyParam; X: integer): pchar;
begin
  if P = nil then Result := #0 else
  if (X >= 0) and (X < TLyParam(P).Count) then
    Result := PChar(TLyParam(P).Params[X].AsString) else
    Result := #0;
end;

procedure my_param_ansistr(P: PLyParam; X: integer; SetAS: TLySetAnsiStr; Outs: PAnsiString);
var
  S: AnsiString;
begin
  if (P <> nil) and (X >= 0) and (X < TLyParam(P).Count) then
  begin
    {$IFDEF UNICODE}
    S := WideToUTF8(TLyParam(P).Params[X].AsString);
    {$ELSE}
    S := TLyParam(P).Params[X].AsString;
    {$ENDIF}
  end
  else S := '';
  SetAS(PAnsiChar(S), Length(S), Outs);
end;

procedure my_param_widestr(P: PLyParam; X: integer; SetWS: TLySetWideStr; Outs: PWideString);
var
  S: WideString;
begin
  if (P <> nil) and (X >= 0) and (X < TLyParam(P).Count) then
  begin
    {$IFDEF UNICODE}
    S := TLyParam(P).Params[X].AsString;
    {$ELSE}
    S := UTF8ToWide(TLyParam(P).Params[X].AsString);
    {$ENDIF}
  end
  else S := '';
  SetWS(PWideChar(S), Length(S), Outs);
end;

function my_param_data(P: PLyParam; X: integer): pointer;
begin
  if P = nil then Result := nil else
  if (X >= 0) and (X < TLyParam(P).Count) then
    Result := TLyParam(P).Params[X].Data else
    Result := nil;
end;

function my_param_error(P: PLyParam; const Msg: PAnsiChar): boolean;
begin
  Result := P <> nil;
  if Result then
    TLyParam(P).Error(LyAnsiToStr(Msg));
end;

{ set result }

procedure my_result_integer(P: PLyParam; Value: int64);
begin
  if P <> nil then
    TLyParam(P).Result.AsInteger := Value;
end;

procedure my_result_boolean(P: PLyParam; Value: boolean);
begin
  if P <> nil then
    TLyParam(P).Result.AsBoolean := Value;
end;

procedure my_result_double(P: PLyParam; Value: double);
begin
  if P <> nil then
    TLyParam(P).Result.AsFloat := Value;
end;

procedure my_result_curr(P: PLyParam; Value: currency);
begin
  if P <> nil then
    TLyParam(P).Result.AsCurrency := Value;
end;

procedure my_result_time(P: PLyParam; Value: TDateTime);
begin
  if P <> nil then
    TLyParam(P).Result.AsTime := Value;
end;

procedure my_result_charA(P: PLyParam; Value: AnsiChar);
begin
  if P <> nil then
    TLyParam(P).Result.AsChar := char(Value);
end;

procedure my_result_charW(P: PLyParam; Value: WideChar);
begin
  if P <> nil then
    TLyParam(P).Result.AsChar := char(Value);
end;

procedure my_result_pcharA(P: PLyParam; Buf: PAnsiChar; Len: integer);
var
  S: {$IFDEF UNICODE}AnsiString{$ELSE}string{$ENDIF};
begin
  {$IFDEF UNICODE}
  S := '';
  if (Len > 0) and (Buf <> nil) then
  begin
    SetLength(S, Len);
    Move(Buf[0], S[1], Len);
  end;
  TLyParam(P).Result.AsString := UTF8ToWide(S);
  {$ELSE}
  SetString(S, Buf, Len);
  TLyParam(P).Result.AsString := S;
  {$ENDIF}
end;

procedure my_result_pcharW(P: PLyParam; Buf: PWideChar; Len: integer);
var
  S: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF};
begin
  {$IFDEF UNICODE}
  SetString(S, Buf, Len);
  TLyParam(P).Result.AsString := S;
  {$ELSE}
  S := '';
  if (Len > 0) and (Buf <> nil) then
  begin
    SetLength(S, Len);
    Move(Buf[0], S[1], Len * sizeof(WideChar));
  end;
  TLyParam(P).Result.AsString := WideToUTF8(S);
  {$ENDIF}
end;

procedure my_result_data(P: PLyParam; T: PLyType; Data: pointer);
begin
  if P <> nil then
    TLyParam(P).Result.Assign(TLyType(T), Data);
end;

//
// -----------------------------------------------------------------------------
//

const

  my_lysee: RLysee = (
    size          : sizeof(RLysee);
    unicode       : {$IFDEF UNICODE}true{$ELSE}false{$ENDIF};
    find1         : {$IFDEF FPC}@{$ENDIF}my_find_by_id;
    find2         : {$IFDEF FPC}@{$ENDIF}my_find_by_name;
    find_module   : {$IFDEF FPC}@{$ENDIF}my_find_module;
    find_type     : {$IFDEF FPC}@{$ENDIF}my_find_type;
    add_func      : {$IFDEF FPC}@{$ENDIF}my_add_func;
    add_param     : {$IFDEF FPC}@{$ENDIF}my_add_param;
    add_method    : {$IFDEF FPC}@{$ENDIF}my_add_method;
    { param }
    param_count   : {$IFDEF FPC}@{$ENDIF}my_param_count;
    param_prmc    : {$IFDEF FPC}@{$ENDIF}my_param_prmc;
    param_type    : {$IFDEF FPC}@{$ENDIF}my_param_type;
    param_integer : {$IFDEF FPC}@{$ENDIF}my_param_integer;
    param_boolean : {$IFDEF FPC}@{$ENDIF}my_param_boolean;
    param_double  : {$IFDEF FPC}@{$ENDIF}my_param_double;
    param_curr    : {$IFDEF FPC}@{$ENDIF}my_param_currency;
    param_time    : {$IFDEF FPC}@{$ENDIF}my_param_time;
    param_charA   : {$IFDEF FPC}@{$ENDIF}my_param_charA;
    param_charW   : {$IFDEF FPC}@{$ENDIF}my_param_charW;
    param_pcharA  : {$IFDEF UNICODE}nil{$ELSE}{$IFDEF FPC}@{$ENDIF}my_param_pchar{$ENDIF};
    param_pcharW  : {$IFDEF UNICODE}{$IFDEF FPC}@{$ENDIF}my_param_pchar{$ELSE}nil{$ENDIF};
    param_ansistr : {$IFDEF FPC}@{$ENDIF}my_param_ansistr;
    param_widestr : {$IFDEF FPC}@{$ENDIF}my_param_widestr;
    param_data    : {$IFDEF FPC}@{$ENDIF}my_param_data;
    param_error   : {$IFDEF FPC}@{$ENDIF}my_param_error;
    { result }
    result_integer: {$IFDEF FPC}@{$ENDIF}my_result_integer;
    result_boolean: {$IFDEF FPC}@{$ENDIF}my_result_boolean;
    result_double : {$IFDEF FPC}@{$ENDIF}my_result_double;
    result_curr   : {$IFDEF FPC}@{$ENDIF}my_result_curr;
    result_time   : {$IFDEF FPC}@{$ENDIF}my_result_time;
    result_charA  : {$IFDEF FPC}@{$ENDIF}my_result_charA;
    result_charW  : {$IFDEF FPC}@{$ENDIF}my_result_charW;
    result_pcharA : {$IFDEF FPC}@{$ENDIF}my_result_pcharA;
    result_pcharW : {$IFDEF FPC}@{$ENDIF}my_result_pcharW;
    result_data   : {$IFDEF FPC}@{$ENDIF}my_result_data;
  );

function LoadFromLibrary(const Name, FileName: string): TLyModule;
var
  H: THandle;
  I: TLyInitModule;
begin
  Result := nil;
  H := 0;
  if LoadDLL(FileName, H) then
  begin
    I := TLyInitModule(GetProcAddr(H, 'LyInitModule'));
    if Assigned(I) then
    begin
      Result := TLyModule.Create(Name);
      Result.Handle := H;
      I(Result, @my_lysee);
    end
    else FreeDLL(H)
  end;
end;

initialization
begin
  my_loadlib := {$IFDEF FPC}@{$ENDIF}LoadFromLibrary;
  LyInitModule(lysee.my_system, @my_lysee);
end;

end.
