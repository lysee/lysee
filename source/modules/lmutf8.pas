unit lmutf8;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lysee;

{@pmc-description UTF8 string functions}
{@pmc-call pp_length => length:int(UTF8:string)}
{@pmc-call pp_get => get:string(UTF8:string, index:int)}
{@pmc-call pp_set => set:string(UTF8:string, index:int, value:string)}
{@pmc-call pp_copy => copy:string(UTF8:string, index:int, count:int)}
{@pmc-call pp_left => left:string(UTF8:string, count:int)}
{@pmc-call pp_delete => delete:string(UTF8:string, index:int, count:int)}
{@pmc-call pp_encode => encode:string(ANSI:string)}
{@pmc-call pp_decode => decode:string(UTF8:string)}
{@pmc-call pp_chars => chars:varlist(UTF8:string)}
{@pmc-end}

procedure pp_length(const Param: PLyParam);
procedure pp_get(const Param: PLyParam);
procedure pp_delete(const Param: PLyParam);
procedure pp_set(const Param: PLyParam);
procedure pp_copy(const Param: PLyParam);
procedure pp_left(const Param: PLyParam);
procedure pp_encode(const Param: PLyParam);
procedure pp_decode(const Param: PLyParam);
procedure pp_chars(const Param: PLyParam);

{
function char_size_UTF8(const S: PLseChar; L: integer): integer;
function copy_pos_UTF8(const S: PLseChar; L, X: integer): PLseChar;
function copy_size_UTF8(const S: PLseChar; L, N: integer): integer;
function length_UTF8(const S: PLseChar; L: integer): integer;
function encode_UTF8(const S: TLseString): TLseString;
function decode_UTF8(const S: TLseString): TLseString;
}

implementation

function char_size_UTF8(const S: pchar; L: integer): integer;
var
  B: pbyte;

  function C080(V: byte): boolean;
  begin
    Result := ((V and $C0) = $80);
  end;

begin
  if L > 0 then
  begin
    Result := 1;
    B := pbyte(S);
    if (B^ >= $C0) and (L > 1) and C080(B[1]) then
      if (B^ and $E0) = $C0 then Result := 2 else
      if (B^ and $F0) = $E0 then
      begin
        if (L > 2) and C080(B[2]) then Result := 3;
      end
      else
      if (B^ and $F8) = $F0 then
      begin
        if (L > 3) and C080(B[2]) and C080(B[3]) then Result := 4;
      end;
  end
  else Result:=0;
end;

function copy_pos_UTF8(const S: pchar; L, X: integer): pchar;
var
  Z: integer;
begin
  if (X >= 0) and (X < L) then
  begin
    Result := S;
    if X = 0 then Exit;
    repeat
      Z := char_size_UTF8(Result, L);
      Inc(Result, Z);
      Dec(X);
      if X = 0 then Exit;
      Dec(L, Z);
    until L <= X;
    Result := nil;
  end
  else Result := nil;
end;

function copy_size_UTF8(const S: pchar; L, N: integer): integer;
var
  P: pchar;
  Z: integer;
begin
  Result := 0;
  P := S;
  while (L > 0) and (N > 0) do
  begin
    Dec(N);
    Z := char_size_UTF8(P, L);
    Dec(L, Z);
    Inc(P, Z);
    Inc(Result, Z);
  end;
end;

function length_UTF8(const S: pchar; L: integer): integer;
var
  Z: integer;
  P: pchar;
begin
  Result := 0;
  P := S;
  while L > 0 do
  begin
    Inc(Result);
    Z := char_size_UTF8(P, L);
    Inc(P, Z);
    Dec(L, Z);
  end;
end;

function encode_UTF8(const S: string): string;
begin
  {$IFDEF UNICODE}
  Result := string(AnsiToUtf8(lse_stou(S)));
  {$ELSE}
  Result := AnsiToUtf8(S);
  {$ENDIF}
end;

function decode_UTF8(const S: string): string;
begin
  {$IFDEF UNICODE}
  Result := lse_utos(Utf8ToAnsi(RawByteString(S)));
  {$ELSE}
  Result := Utf8ToAnsi(S);
  {$ENDIF}
end;

procedure pp_length(const Param: PLyParam);
var
  S: PLyString;
  P: pchar;
  L: integer;
begin
  S := LyStrec(Param, 0);
  P := LyStrecData(S);
  L := LyStrecLength(S);
  LyReturn(Param, length_UTF8(P, L));
end;

procedure pp_get(const Param: PLyParam);
var
  S: PLyString;
  P, T: pchar;
  L, X: integer;
begin
  S := LyStrec(Param, 0);
  P := LyStrecData(S);
  L := LyStrecLength(S);
  X := LyInt(Param, 1);
  if X < 0 then
    X := LyResetIndex(X, length_UTF8(P, L));
  T := copy_pos_UTF8(P, L, X);
  if T <> nil then
  begin
    X := char_size_UTF8(T, L - (T - P));
    if X = L then
      LyReturn(Param, S) else
      LyReturn(Param, LyStrecAlloc(T, X));
  end;
end;

procedure pp_delete(const Param: PLyParam);
var
  S: PLyString;
  P, T, M: pchar;
  L, X, N: integer;
begin
  S := LyStrec(Param, 0);
  N := LyInt(Param, 2);
  if N > 0 then
  begin
    P := LyStrecData(S);
    L := LyStrecLength(S);
    X := LyInt(Param, 1);
    if X < 0 then
      X := LyResetIndex(X, length_UTF8(P, L));
    T := copy_pos_UTF8(P, L, X);
    if T <> nil then
    begin
      X := copy_size_UTF8(T, L - (T - P), N);
      if X < L then
      begin
        S := LyStrecAlloc(nil, L - X);
        M := LyStrecData(S);
        N := T - P;
        System.Move(P^, M^, N);
        Inc(M, N);
        Inc(T, X);
        System.Move(T^, M^, L - X - N);
      end
      else S := nil;
    end;
  end;
  LyReturn(Param, S);
end;

procedure pp_set(const Param: PLyParam);
var
  S, V: PLyString;
  P, T, M: pchar;
  L, X, N, Z: integer;
begin
  S := LyStrec(Param, 0);
  P := LyStrecData(S);
  L := LyStrecLength(S);
  X := LyInt(Param, 1);
  if X < 0 then
    X := LyResetIndex(X, length_UTF8(P, L));
  T := copy_pos_UTF8(P, L, X);
  if T <> nil then
  begin
    V := LyStrec(Param, 2);
    X := char_size_UTF8(T, L - (T - P));
    if X < L then
    begin
      Z := LyStrecLength(V);
      S := LyStrecAlloc(nil, L - X + Z);
      M := LyStrecData(S);
      N := T - P;
      System.Move(P^, M^, N);
      Inc(M, N + Z);
      Inc(T, X);
      System.Move(T^, M^, L - X - N);
      Dec(M, Z);
      System.Move(LyStrecData(V)^, M^, Z);
    end
    else S := V;
  end;
  LyReturn(Param, S);
end;

procedure pp_copy(const Param: PLyParam);
var
  S: PLyString;
  P, T: pchar;
  L, X, N: integer;
begin
  N := LyInt(Param, 2);
  if N > 0 then
  begin
    S := LyStrec(Param, 0);
    P := LyStrecData(S);
    L := LyStrecLength(S);
    X := LyInt(Param, 1);
    if X < 0 then
      X := LyResetIndex(X, length_UTF8(P, L));
    T := copy_pos_UTF8(P, L, X);
    if T <> nil then
    begin
      X := copy_size_UTF8(T, L - (T - P), N);
      if X = L then
        LyReturn(Param, S) else
        LyReturn(Param, LyStrecAlloc(T, X));
    end;
  end;
end;

procedure pp_left(const Param: PLyParam);
var
  S: PLyString;
  P, T: pchar;
  L, X, N: integer;
begin
  N := LyInt(Param, 1);
  if N > 0 then
  begin
    S := LyStrec(Param, 0);
    P := LyStrecData(S);
    L := LyStrecLength(S);
    T := copy_pos_UTF8(P, L, 0);
    if T <> nil then
    begin
      X := copy_size_UTF8(T, L - (T - P), N);
      if X = L then
        LyReturn(Param, S) else
        LyReturn(Param, LyStrecAlloc(T, X));
    end;
  end;
end;

procedure pp_encode(const Param: PLyParam);
begin
  LyReturn(Param, encode_UTF8(LyString(Param, 0)));
end;

procedure pp_decode(const Param: PLyParam);
begin
  if Param^.p_count > 0 then
    LyReturn(Param, decode_UTF8(LyString(Param, 0)));
end;

procedure pp_chars(const Param: PLyParam);
var
  L: HLyVarlist;
  S: PLyString;
  P: pchar;
  N, X: integer;
begin
  L := LyVarlistCreateAsResult(Param);
  S := LyStrec(Param, 0);
  P := LyStrecData(S);
  N := LyStrecLength(S);
  while N > 0 do
  begin
    X := char_size_UTF8(P, N);
    LyVarlistAdd(L, LyStrPas(P, X));
    Dec(N, X);
    Inc(P, X);
  end;
end;

end.

