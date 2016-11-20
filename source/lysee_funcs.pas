{==============================================================================}
{        UNIT: lysee_funcs                                                     }
{ DESCRIPTION: lysee module functions and class wrapers                        }
{   COPYRIGHT: Copyright (c) 2003-2016, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/02/28                                                      }
{    MODIFIED: 2016/11/19                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_funcs;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, StrUtils, IniFiles, {$IFDEF FPC}Process,{$ENDIF}
  basic, lysee;

type

  { TLiIniFile }

  TLiIniFile = class(TIniFile) //=> inifiles.lm
  private
    FRefcount: integer;
  public
    function IncRefcount: integer;
    function DecRefcount: integer;
    function AsString: string;
  end;

  { TLiSearchRec }

  TLiSearchRec = class(TLiObject)
  private
    FNode: TSearchRec;
    FPath: string;
    FActive: boolean;
    function IsDD(const S: string): boolean;
    function GetIsFile: boolean;
    function GetIsDirectory: boolean;
    function GetSize: int64;
    function GetPath: string;
    function GetName: string;
    function GetFileName: string;
    function GetAttr: integer;
  public
    destructor Destroy;override;
    function FindFirst(const APath: string; Attr: integer): boolean;
    function FindNext: boolean;
    function FindClose: boolean;
    property FileName: string read GetFileName;
    property Name: string read GetName;
    property Path: string read GetPath;
    property Attr: integer read GetAttr;
    property Size: int64 read GetSize;
    property IsFile: boolean read GetIsFile;
    property IsDirectory: boolean read GetIsDirectory;
    property Active: boolean read FActive;
  end;

  { TLiStrbuf }

  TLiStrbuf = class(TLiObject)
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

  { TLiStrcut }

  TLiStrcut = class(TLiObject)
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
    function ReadMatched(Source: TLiStrcut): integer;
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

  { TLiProcess }

  {$IFDEF FPC}
  TLiProcess = class(TProcess) //=> lysee_process.lm
  private
    FRefcount: integer;
  public
    constructor CreateEx;
    function IncRefcount: integer;
    function DecRefcount: integer;
    function AsString: string;
  end;
  {$ENDIF}

function _FindFirst(const Path: string; Attr: integer; Sr: TLiSearchRec): integer;
function _FindNext(Sr: TLiSearchRec): longint;
procedure _FindClose(Sr: TLiSearchrec);
function ChangeFilePath(const FileName, Path: string): string;
function GetHomePath: string;
function IsRelativePath(const Path: string): boolean;

function GenModuleUnit(const UnitFile: string): string;

implementation

uses
  lysee_pmc;

function _FindFirst(const Path: string; Attr: integer; Sr: TLiSearchRec): integer;
begin
  if Sr.FindFirst(Path, Attr) then Result := 0 else Result := 1;
end;

function _FindNext(Sr: TLiSearchRec): longint;
begin
  if Sr.FindNext then Result := 0 else Result := 1;
end;

procedure _FindClose(Sr: TLiSearchrec);
begin
  Sr.FindClose;
end;

function ChangeFilePath(const FileName, Path: string): string;
begin
  {$IFDEF FPC}
  Result := IncludeTrailingPathDelimiter(Path) + ExtractFileName(FileName);
  {$ELSE}
  Result := SysUtils.ChangeFilePath(FileName, Path);
  {$ENDIF}
end;

function GetHomePath: string;
begin
  {$IFDEF FPC}
    {$IFDEF MSWINDOWS}
    Result := FullPath(GetEnv('HOMEDRIVER') + GetEnv('HOMEPATH'));
    {$ELSE}
    Result := FullPath(GetEnv('HOME'));
    {$ENDIF}
  {$ELSE}
    Result := SysUtils.GetHomePath;
  {$ENDIF}
end;

function IsRelativePath(const Path: string): boolean;
begin
  {$IFDEF FPC}
    Result := (Length(Path) > 0) and (Path[1] <> PathDelim);
    {$IFDEF MSWINDOWS}
    Result := Result and (Length(Path) > 1) and (Path[2] <> ':')
    {$ENDIF}
  {$ELSE}
  Result := SysUtils.IsRelativePath(Path);
  {$ENDIF}
end;

function GenModuleUnit(const UnitFile: string): string;
var
  P: TLiPasTranslater;
  F: string;
begin
  P := TLiPasTranslater.Create;
  try
    F := ExpandFileName(Trim(UnitFile));
    P.SourceCodes.LoadFromFile(F);
    P.Process;
    Result := ExtractFilePath(F) + P.ResultUnitName + '.pas';
    P.ResultCodes.SaveToFile(Result);
  finally
    P.Free;
  end;
end;

{ TLiIniFile }

function TLiIniFile.IncRefcount: integer;
begin
  if Self <> nil then
  begin
    Inc(FRefCount);
    Result := FRefCount;
  end
  else Result := 0;
end;

function TLiIniFile.DecRefcount: integer;
begin
  if Self <> nil then
  begin
    Dec(FRefCount);
    Result := FRefCount;
    if Result = 0 then Free;
  end
  else Result := 0;
end;

function TLiIniFile.AsString: string;
begin
  Result := '';
end;

{ TLiSearchRec }

function TLiSearchRec.GetPath: string;
begin
  if (Self <> nil) and FActive then
    Result := FPath else
    Result := '';
end;

function TLiSearchRec.GetName: string;
begin
  if (Self <> nil) and FActive then
    Result := FNode.Name else
    Result := '';
end;

function TLiSearchRec.GetAttr: integer;
begin
  if (Self <> nil) and FActive then
    Result := FNode.Attr else
    Result := 0;
end;

function TLiSearchRec.GetFileName: string;
begin
  if (Self <> nil) and FActive then
    Result := FPath + FNode.Name else
    Result := '';
end;

function TLiSearchRec.GetIsFile: boolean;
begin
  Result := (Self <> nil) and FActive and ((FNode.Attr and faDirectory) = 0);
end;

function TLiSearchRec.GetIsDirectory: boolean;
begin
  Result := (Self <> nil) and FActive and ((FNode.Attr and faDirectory) <> 0);
end;

function TLiSearchRec.GetSize: int64;
begin
  if (Self <> nil) and FActive then
    Result := FNode.Size else
    Result := 0;
end;

destructor TLiSearchRec.Destroy;
begin
  FPath := '';
  FindClose;
  inherited;
end;

function TLiSearchRec.FindClose: boolean;
begin
  Result := false;
  if (Self <> nil) and FActive then
  begin
    FActive := false;
    SysUtils.FindClose(FNode);
  end;
end;

function TLiSearchRec.FindFirst(const APath: string; Attr: integer): boolean;
begin
  Result := false;
  if Self <> nil then
  begin
    FindClose;
    FActive := (SysUtils.FindFirst(APath, Attr, FNode) = 0);
    if FActive then
    begin
      while FActive and IsDD(FNode.Name) do
        FActive := (SysUtils.FindNext(FNode) = 0);
      if FActive then
        FPath := ExtractFilePath(ExpandFileName(APath)) else
        SysUtils.FindClose(FNode);
    end;
    Result := FActive;
  end;
end;

function TLiSearchRec.FindNext: boolean;
begin
  Result := false;
  if (Self <> nil) and FActive then
  begin
    FActive := (SysUtils.FindNext(FNode) = 0);
    while FActive and IsDD(FNode.Name) do
      FActive := (SysUtils.FindNext(FNode) = 0);
    if not FActive then
      SysUtils.FindClose(FNode);
    Result := FActive;
  end;
end;

function TLiSearchRec.IsDD(const S: string): boolean;
begin
  Result := (Self <> nil) and (S = '.') or (S = '..');
end;

{ TLiStrbuf }

constructor TLiStrbuf.Create(const AStr: string);
begin
  inherited Create;
  FBuffer := AStr;
end;

procedure TLiStrbuf.TrimAll;
begin
  FBuffer := basic.TrimAll(FBuffer);
end;

procedure TLiStrbuf.Lower;
begin
  FBuffer := LowerCase(FBuffer);
end;

procedure TLiStrbuf.Upper;
begin
  FBuffer := UpperCase(FBuffer);
end;

procedure TLiStrbuf.Delete(Index, Count: integer);
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

procedure TLiStrbuf.Insert(const SubStr: string; Index: integer);
begin
  if SubStr <> '' then
  begin
    Index := ResetIndex(Index, GetSize);
    if (Index >= 0) and (index <= GetSize) then
      System.Insert(SubStr, FBuffer, Index + 1);
  end;
end;

procedure TLiStrbuf.Join(const S: string);
begin
  FBuffer := FBuffer + S;
end;

procedure TLiStrbuf.Reverse;
begin
  FBuffer := ReverseString(FBuffer);
end;

procedure TLiStrbuf.Randomize;
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

function TLiStrbuf.Contains(const SubStr: string; IgnoreCase: boolean): boolean;
begin
  Result := (Pos(SubStr, IgnoreCase, 0) >= 0);
end;

function TLiStrbuf.Copy(Index, Count: int64): string;
begin
  if FBuffer <> '' then
  begin
    Index := ResetRange(Index, GetSize, Count);
    Result := System.Copy(FBuffer, Index + 1, Count);
  end
  else Result := '';
end;

function TLiStrbuf.GetSize: integer;
begin
  Result := Length(FBuffer);
end;

procedure TLiStrbuf.SetSize(Size: integer);
begin
  if Size > 0 then
    SetLength(FBuffer, Size) else
    FBuffer := '';
end;

procedure TLiStrbuf.Replace(const patten, newStr: string; IgnoreCase,
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

function TLiStrbuf.Pos(const SubStr: string;
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

function TLiStrbuf.LastPos(const SubStr: string;
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

procedure TLiStrbuf.Trim;
begin
  FBuffer := SysUtils.Trim(FBuffer);
end;

procedure TLiStrbuf.TrimLeft;
begin
  FBuffer := SysUtils.TrimLeft(FBuffer);
end;

procedure TLiStrbuf.TrimRight;
begin
  FBuffer := SysUtils.TrimRight(FBuffer);
end;

{ TLiStrcut }

procedure TLiStrcut.Clear;
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

procedure TLiStrcut.ClearValues;
var
  index: integer;
begin
  for index := 0 to Length(FValueList) - 1 do
    FValueList[index] := '';
end;

constructor TLiStrcut.Create(const AHeader, ADelimiter: string);
begin
  Init(AHeader, ADelimiter);
end;

procedure TLiStrcut.Delete(X: integer);
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

function TLiStrcut.DeQuote(var S: pchar): string;
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

procedure TLiStrcut.Exchange(X1, X2: integer);
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

procedure TLiStrcut.ExchangeKey(const Key1, Key2: string);
begin
  Exchange(IndexOf(Key1), IndexOf(Key2));
end;

function TLiStrcut.GetCount: integer;
begin
  Result := Length(FKeyList);
end;

function TLiStrcut.GetValue(const Name: string): string;
begin
  Result := ValueAt(IndexOf(Name));
end;

function TLiStrcut.IndexOf(const Key: string): integer;
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

procedure TLiStrcut.Init(const AHeader, ADelimiter: string);
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

procedure TLiStrcut.Move(X1, X2: integer);
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

procedure TLiStrcut.MoveKey(const Key1, Key2: string);
begin
  Move(IndexOf(Key1), IndexOf(Key2));
end;

function TLiStrcut.KeyList: string;
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

procedure TLiStrcut.Parse(const S: string);
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

function TLiStrcut.ParseNext(var S: pchar): string;
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

function TLiStrcut.ReadMatched(Source: TLiStrcut): integer;
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

function TLiStrcut.GetKey(Index: integer): string;
begin
  Index := ResetIndex(Index, GetCount);
  Result := FKeyList[Index];
end;

procedure TLiStrcut.SetKey(Index: integer; const Key: string);
begin
  Index := ResetIndex(Index, GetCount);
  FKeyList[Index] := Key;
end;

procedure TLiStrcut.RenameKey(const Key, NewKey: string);
begin
  FKeyList[IndexOf(Key)] := NewKey;
end;

procedure TLiStrcut.Remove(const Key: string);
var
  index: integer;
begin
  index := IndexOf(Key);
  if index >= 0 then
    Delete(index);
end;

function TLiStrcut.Mask: string;
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

function TLiStrcut.Delimiter: string;
begin
  Result := FDelimiter;
end;

procedure TLiStrcut.SetValue(const Name, Value: string);
begin
  FValueList[IndexOf(Name)] := Value;
end;

function TLiStrcut.ValueAt(Index: integer): string;
begin
  Result := FValueList[Index];
end;

function TLiStrcut.ValueList: string;
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

{ TLiProcess }

{$IFDEF FPC}
constructor TLiProcess.CreateEx;
begin
  inherited Create(nil);
end;

function TLiProcess.IncRefcount: integer;
begin
  if Self <> nil then
  begin
    Inc(FRefCount);
    Result := FRefCount;
  end
  else Result := 0;
end;

function TLiProcess.DecRefcount: integer;
begin
  if Self <> nil then
  begin
    Dec(FRefCount);
    Result := FRefCount;
    if Result = 0 then Free;
  end
  else Result := 0;
end;

function TLiProcess.AsString: string;
begin
  Result := ApplicationName;
end;
{$ENDIF}

end.

