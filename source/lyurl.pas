{==============================================================================}
{        UNIT: url                                                             }
{ DESCRIPTION: Uniform Resource Locator                                        }
{   COPYRIGHT: Copyright (c) 2018, Li Yun Jie. All Rights Reserved.            }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2018/05/24                                                      }
{    MODIFIED: 2020/02/23                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lyurl;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, Basic;

const

  URL_ABOUTBLANK = 'about:blank';

type

  { TLyURL }

  TLyURL = class
  private
    FURL: string; { Scheme://Authority/Path?Query#Fragment }
    FScheme: string;
    FAuthority: string;
    FPath: string;
    FQuery: string; { field1=value1&field2=value2$...}
    FFields: TStrings;
    FFragment: string;
    function GetReference: string;
    procedure SetReference(const Value: string);
    procedure SetScheme(const Value: string);
    procedure SetAuthority(const Value: string);
    procedure SetPath(const Value: string);
    procedure SetQuery(const Value: string);
    procedure SetFragment(const Value: string);
    function GetFileName: string;
    procedure SetFileName(const AFile: string);
  public
    constructor Create(const AURL: string);
    destructor Destroy;override;
    procedure Clear;virtual;
    procedure Analyze(const AURL: string);virtual;
    procedure Assign(Source: TLyURL);
    function Clone: TLyURL;
    function SetURL(const AURL: string): boolean;
    function UpdateWith(const ARelativURL: string): boolean;
    function CompleteURL(const AURL: string): string;
    function IsValid: boolean;virtual;
    function IsHttp: boolean;
    function IsHttps: boolean;
    function IsFile: boolean;
    function IsFtp: boolean;
    function IsAbout: boolean;
    function PathFileName: string;
    function DownloadFile(const APath: string): string;overload;
    function DownloadFile: string;overload;
    function HasQuery: boolean;
    function HasField(const FieldName: string): boolean;
    function GetField(const FieldName: string; var Value: string): boolean;
    property URL: string read FURL;
    property Reference: string read GetReference write SetReference;
    property Scheme: string read FScheme write SetScheme;
    property Authority: string read FAuthority write SetAuthority;
    property Path: string read FPath write SetPath;
    property Query: string read FQuery write SetQuery;
    property Fragment: string read FFragment write SetFragment;
    property Fields: TStrings read FFields;
    property FileName: string read GetFileName write SetFileName;
  end;

var
  my_cache_path: string;

function UrlCachePath: string;overload;
function UrlCachePath(const Domain: string): string;overload;
function UrlIsValid(const AURL: string): boolean;
function UrlArrangeFileName(const FileName: string): string;
function UrlGetScheme(const AURL: string): string;
function UrlGetAthority(const AURL: string): string;
function UrlGetPath(const AURL: string): string;
function UrlGetQuery(const AURL: string): string;
function UrlGetFragment(const AURL: string): string;
function UrlGetFileName(const AURL: string): string;
function UrlSetPathDelimiter(const APath: string): string; // \ => /
function UrlGetRelativePath(const APath, BasePath: string): string;
function UrlEncodeFile(const AFile: string; IncLocalHost: boolean = false): string;
function UrlEncodeFileToPath(const AFile: string): string;
function UrlDecodePathToFile(const APath: string): string;
function UrlPackPath(const APath: string): string;
function UrlGetNameValue(const S: string; var V: string): string;
function UrlGetNextField(var Query: string): string;
function UrlEncodeQuery(L: TStrings): string;
function UrlDecodeQuery(const Query: string; L: TStrings): integer;
function UrlEncodeField(const Name, Value: string): string;overload;
function UrlEncodeField(const Field: string): string;overload;
function UrlDecodeField(const Field: string; var Name, Value: string): boolean;
function UrlDeleteQueryFragment(const AURL: string): string;
function UrlDeleteQuery(const AURL: string): string;
function UrlDeleteFragment(const AURL: string): string;
function UrlDownloadFile(const AURL, FileName: string): boolean;

implementation

uses
  {$IFDEF FPC}HTTPDefs, HttpProtocol,{$ELSE}HttpApp, NetEncoding,{$ENDIF}
  Math, UrlMon;
//UriUtils;

function UrlCachePath: string;
var
  T: string;
begin
  if my_cache_path = '' then
  begin
    T := ExtractFilePath(ParamStr(0)) + 'Caches';
    Check(ForceDirectories(T), 'Failed creating directory: ' + T);
    my_cache_path := IncludeTrailingPathDelimiter(T);
  end;
  Result := my_cache_path;
end;

function UrlCachePath(const Domain: string): string;
var
  T: string;
begin
  T := IncludeTrailingPathDelimiter(UrlCachePath + UrlArrangeFileName(Domain));
  if not DirectoryExists(T) then
    if not ForceDirectories(T) then
      T := '';
  Result := T;
end;

function UrlIsValid(const AURL: string): boolean;
var
  U: TLyURL;
begin
  U := TLyURL.Create(AURL);
  Result := U.IsValid;
  U.Free;
end;

function UrlArrangeFileName(const FileName: string): string;
var
  I: integer;
begin
  Result := LowerCase(FileName);
  for I := Length(Result) downto 1 do
    if Result[I] <= #255 then
      if not CharInSet(Result[I], CS_ID + ['.', '-']) then
        Result[I] := '_';
end;

function UrlGetScheme(const AURL: string): string;
var
  U: TLyURL;
begin
  U := TLyURL.Create(AURL);
  Result := U.FScheme;
  U.Free;
end;

function UrlGetAthority(const AURL: string): string;
var
  U: TLyURL;
begin
  U := TLyURL.Create(AURL);
  Result := U.FAuthority;
  U.Free;
end;

function UrlGetPath(const AURL: string): string;
var
  U: TLyURL;
begin
  U := TLyURL.Create(AURL);
  Result := U.FPath;
  U.Free;
end;

function UrlGetQuery(const AURL: string): string;
var
  I: integer;
begin
  I := Pos('?', AURL);
  if I > 0 then
    Result := UrlDeleteFragment(Copy(AURL, I + 1, Length(AURL))) else
    Result := '';
end;

function UrlGetFragment(const AURL: string): string;
var
  I: integer;
begin
  I := Pos('#', AURL);
  if I > 0 then
    Result := Copy(AURL, I + 1, Length(AURL)) else
    Result := '';
end;

function UrlGetFileName(const AURL: string): string;
var
  U: TLyURL;
begin
  U := TLyURL.Create(AURL);
  Result := U.FileName;
  U.Free;
end;

function UrlSetPathDelimiter(const APath: string): string;
var
  I: integer;
begin
  Result := APath;
  for I := 1 to Length(APath) do
    if APath[I] = '\' then
      Result[I] := '/';
end;

function UrlGetRelativePath(const APath, BasePath: string): string;
var
  I, X, L: integer;
  B, P: string;
begin
  B := UrlSetPathDelimiter(BasePath);
  P := UrlSetPathDelimiter(APath);
  L := Min(Length(B), Length(P));
  X := 0;
  I := 1;
  while (I <= L) and MatchChar(B[I], P[I]) do
  begin
    if B[I] = '/' then X := I;
    Inc(I);
  end;
  if X > 0 then
  begin
    Result := Copy(P, X + 1, Length(P));
    for I := X + 1 to Length(B) do
      if B[I] = '/' then
        Result := '../' + Result;
  end
  else Result := P;
end;

function UrlEncodeFile(const AFile: string; IncLocalHost: boolean): string;
const
  K = CS_ID + [':', '/', '!', '''', '(', ')', '*', '-', '.', '~'];
var
  I, L, X: Integer;
  S: string;
begin
  if AFile <> '' then
  begin
    if IncLocalHost then
      Result := 'file://localhost/' else
      Result := 'file:///';
    S := ReplaceAll(AFile, '\', '/');
    L := Length(S);
    I := 1;
    while I <= L do
    begin
      X := I;
      if CharInSet(S[I], K) then
      begin
        Inc(I);
        while (I <= L) and CharInSet(S[I], K) do Inc(I);
        Result := Result + Copy(S, X, I - X);
      end
      else
      begin
        Inc(I);
        while (I <= L) and not CharInSet(S[I], K) do Inc(I);
        {$IFDEF UNICODE}
        Result := Result + UTF8ToStr(HttpEncode(StrToUTF8(Copy(S, X, I - X))));
        {$ELSE}
        Result := Result + HttpEncode(Copy(S, X, I - X));
        {$ENDIF}
      end;
    end;
  end
  else Result := '';
end;

function UrlEncodeQuery(L: TStrings): string;
var
  I: integer;
  F: string;
begin
  Result := '';
  for I := 0 to L.Count - 1 do
  begin
    F := UrlEncodeField(L[I]);
    if F <> '' then
      if Result = '' then
        Result := F else
        Result := Result + '&' + F;
  end;
end;

function UrlDecodeQuery(const Query: string; L: TStrings): integer;
var
  I: integer;
  S, N, V: string;
begin
  L.Clear;
  S := Query;
  while S <> '' do
  begin
    I := Pos('&', S);
    if I > 0 then
    begin
      N := UrlGetNameValue(Copy(S, 1, I - 1), V);
      S := Copy(S, I + 1, Length(S));
    end
    else
    begin
      N := UrlGetNameValue(S, V);
      S := '';
    end;
    if N <> '' {IsID(N)} then
      {$IFDEF UNICODE}
      L.Add(N + '=' + UTF8ToStr(HttpDecode(StrToUTF8(V))));
      {$ELSE}
      L.Add(N + '=' + HttpDecode(V));
      {$ENDIF}
  end;
  Result := L.Count;
end;

function UrlEncodeField(const Name, Value: string): string;
begin
  if Name <> '' then
    {$IFDEF UNICODE}
    Result := Name + '=' + UTF8ToStr(HttpEncode(StrToUTF8(Value))) else
    {$ELSE}
    Result := Name + '=' + HttpEncode(Value) else
    {$ENDIF}
    Result := '';
end;

function UrlEncodeField(const Field: string): string;
var
  N, V: string;
begin
  N := UrlGetNameValue(Field, V);
  Result := UrlEncodeField(N, V);
end;

function UrlDecodeField(const Field: string; var Name, Value: string): boolean;
begin
  Name := UrlGetNameValue(Field, Value);
  {$IFDEF UNICODE}
  Value := UTF8ToStr(HttpDecode(StrToUTF8(Value)));
  {$ELSE}
  Value := HttpDecode(Value);
  {$ENDIF}
  Result := (Name <> '');
end;

function UrlEncodeFileToPath(const AFile: string): string;
begin
  Result := UrlSetPathDelimiter(Trim(AFile));
  if (Result <> '') and (Result[1] <> '/') then
    Result := '/' + Result;
end;

function UrlDecodePathToFile(const APath: string): string;
begin
  {$IFDEF UNICODE}
  Result := UTF8ToStr(HttpDecode(StrToUTF8(APath)));
  {$ELSE}
  Result := HttpDecode(APath);
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  if (Result <> '') and (Result[1] = '/') then
    Result := SetPD(Copy(Result, 2, Length(APath)));
  {$ENDIF}
end;

function UrlPackPath(const APath: string): string;
var
  I, X: integer;
  P: string;
begin
  P := ReplaceAll(UrlSetPathDelimiter(Trim(APath)), '/./', '/');
  if P <> '' then
  begin
    while StartStr(P, './') do Delete(P, 1, 2);
    I := Pos('/..', P);
    while I > 0 do
    begin
      X := I - 1;
      while (X >= 1) and (P[X] <> '/') do Dec(X);
      if X > 0 then
        Delete(P, X, I - X + 3) else
        Break;
      I := Pos('/..', P);
    end;
  end;
  Result := P;
end;

function UrlGetNameValue(const S: string; var V: string): string;
var
  X: integer;
begin
  X := Pos('=', S);
  if X > 0 then
  begin
    V := Copy(S, X + 1, Length(S));
    Result := Trim(Copy(S, 1, X - 1));
  end
  else
  begin
    V := '';
    Result := Trim(S);
  end;
end;

function UrlGetNextField(var Query: string): string;
var
  X: integer;
begin
  X := Pos('&', Query);
  if X > 0 then
  begin
    Result := Copy(Query, 1, X - 1);
    Query := Copy(Query, X + 1, Length(Query));
  end
  else
  begin
    Result := Query;
    Query := '';
  end;
end;

function UrlDeleteQueryFragment(const AURL: string): string;
var
  I: integer;
begin
  I := Pos('?', AURL);
  if I < 1 then I := Pos('#', AURL);
  if I > 0 then
    Result := Copy(AURL, 1, I - 1) else
    Result := AURL;
end;

function UrlDeleteQuery(const AURL: string): string;
var
  I: integer;
begin
  I := Pos('?', AURL);
  if I > 0 then
  begin
    Result := Copy(AURL, 1, I - 1);
    I := Pos('#', AURL);
    if I > 0 then
      Result := Result + Copy(AURL, I, Length(AURL));
  end
  else Result := AURL;
end;

function UrlDeleteFragment(const AURL: string): string;
var
  I: integer;
begin
  I := Pos('#', AURL);
  if I > 0 then
    Result := Copy(AURL, 1, I - 1) else
    Result := AURL;
end;

function UrlDownloadFile(const AURL, FileName: string): boolean;
begin
  try
    Result := UrlMon.URLDownloadToFile(nil,
      pchar(AURL), pchar(FileName), 0, nil) = 0;
  except
    Result := false;
  end;
end;

{ TLyURL }

procedure TLyURL.Assign(Source: TLyURL);
begin
  if Self <> Source then
  begin
    Clear;
    if Source <> nil then
    begin
      FURL := Source.FURL;
      FScheme := Source.FScheme;
      FAuthority := Source.FAuthority;
      FPath := Source.FPath;
      FQuery := Source.FQuery;
      FFields.Assign(Source.FFields);
      FFragment := Source.FFragment;
    end;
  end;
end;

procedure TLyURL.Clear;
begin
  FURL := '';
  FScheme := '';
  FAuthority := '';
  FPath := '';
  FQuery := '';
  FFields.Clear;
  FFragment := '';
end;

function TLyURL.Clone: TLyURL;
begin
  Result := TLyURL.Create('');
  Result.Assign(Self);
end;

function TLyURL.CompleteURL(const AURL: string): string;
var
  U: TLyURL;
begin
  U := Clone;
  try
    if U.UpdateWith(AURL) then
      Result := U.Reference else
      Result := AURL;
  finally
    U.Free;
  end;
end;

constructor TLyURL.Create(const AURL: string);
begin
  FFields := TStringList.Create;
  if AURL <> '' then Analyze(AURL);
end;

destructor TLyURL.Destroy;
begin
  Clear;
  FreeAndNil(FFields);
  inherited;
end;

function TLyURL.DownloadFile: string;
begin
  Result := DownloadFile(UrlCachePath(FAuthority));
end;

procedure TLyURL.Analyze(const AURL: string);
var
  I, L: Integer;
  S: string;
begin
  Clear;

  FURL := UrlSetPathDelimiter(Trim(AURL));
  S := FURL;
  L := Length(S);
  if L = 0 then Exit;

  { scheme://authority/path?query#FRAGMENT }
  I := Pos('#', S);
  if I > 0 then
  begin
    FFragment := Copy(S, I + 1, L);
    S := Copy(S, 1, I - 1);
    if S = '' then Exit;
  end;

  { scheme://authority/path?QUERY }
  I := Pos('?', S);
  if I > 0 then
  begin
    FQuery := Copy(S, I + 1, L);
    UrlDecodeQuery(FQuery, FFields);
    S := Copy(S, 1, I - 1);
    if S = '' then Exit;
  end;

  { SCHEME://authority/path }
  I := Pos('://', S);
  if I > 0 then
  begin
    FScheme := Copy(S, 1, I - 1);
    S := Copy(S, I + 3, L);
    { AUTHORITY/path }
    I := Pos('/', S);
    if I > 0 then
    begin
      FAuthority := Copy(S, 1, I - 1);
      FPath := Copy(S, I, L);
    end
    else FAuthority := S;
    Exit;
  end;

  { SCHEME:path }
  I := Pos(':', S);
  if (I > 0) and (I <> 2) then
  begin
    FScheme := Copy(S, 1, I - 1); //
    S := Copy(S, I + 1, L);
  end;
  FPath := S;
end;

function TLyURL.DownloadFile(const APath: string): string;
begin
  Result := '';
  if IsValid and not IsAbout then
    if IsFile then
    begin
      Result := GetFileName;
      if not FileExists(Result) then
        Result := '';
    end
    else
    if ExtractFileExt(FPath) <> '' then
    begin
      Result := IncludeTrailingPathDelimiter(APath) + UrlArrangeFileName(FPath);
      if not FileExists(Result) then
        if not UrlDownloadFile(Reference, Result) then
          Result := '';
    end;
end;

function TLyURL.GetField(const FieldName: string; var Value: string): boolean;
var
  I: integer;
begin
  if FieldName <> '' then
  begin
    I := FFields.IndexOfName(FieldName);
    if I >= 0 then
    begin
      Value := FFields.ValueFromIndex[I];
      Result := true;
      Exit;
    end;
  end;
  Result := false;
end;

function TLyURL.GetFileName: string;
begin
  if MatchID(FScheme, 'file') or (FScheme = '') then
    Result := UrlDecodePathToFile(FPath) else
    Result := '';
end;

function TLyURL.IsAbout: boolean;
begin
  Result := MatchID(FScheme, 'about');
end;

function TLyURL.IsFile: boolean;
begin
  Result := MatchID(FScheme, 'file');
end;

function TLyURL.IsFtp: boolean;
begin
  Result := MatchID(FScheme, 'ftp');
end;

function TLyURL.IsHttp: boolean;
begin
  Result := MatchID(FScheme, 'http');
end;

function TLyURL.IsHttps: boolean;
begin
  Result := MatchID(FScheme, 'https');
end;

function TLyURL.IsValid: boolean;
begin
  Result := IsID(FScheme);
  if Result and not IsAbout then
    if IsFile then
      Result := (FPath <> '') else
    if IsHttp or IsHttps then
      Result := (FAuthority <> '') else
    if IsFtp then
      Result := (FAuthority <> '') and (FPath <> '') else
      Result := (FAuthority <> '');
end;

function TLyURL.PathFileName: string;
begin
  Result := UrlArrangeFileName(FPath);
end;

function TLyURL.GetReference: string;
begin
  Result := FScheme + ':';
  if (FAuthority <> '') or IsFile then
    Result := Result + '//' + FAuthority;
  Result := Result + FPath;
  if FQuery <> '' then
    Result := Result + '?' + FQuery;
  if FFragment <> '' then
    Result := Result + '#' + FFragment;
end;

function TLyURL.HasField(const FieldName: string): boolean;
begin
  if FieldName <> '' then
    Result := (FFields.IndexOfName(FieldName) >= 0) else
    Result := false;
end;

function TLyURL.HasQuery: boolean;
begin
  Result := (FFields.Count > 0);
end;

procedure TLyURL.SetAuthority(const Value: string);
begin
  FAuthority := Trim(Value);
end;

procedure TLyURL.SetFileName(const AFile: string);
begin
  SetReference(UrlEncodeFile(AFile));
end;

procedure TLyURL.SetFragment(const Value: string);
begin
  FFragment := Trim(Value);
end;

procedure TLyURL.SetPath(const Value: string);
begin
  FPath := UrlSetPathDelimiter(Trim(Value));
end;

procedure TLyURL.SetQuery(const Value: string);
begin
  FQuery := Trim(Value);
  UrlDecodeQuery(FQuery, FFields);
end;

procedure TLyURL.SetReference(const Value: string);
begin
  Analyze(Value);
end;

procedure TLyURL.SetScheme(const Value: string);
begin
  FScheme := Trim(Value);
end;

function TLyURL.SetURL(const AURL: string): boolean;
begin
  Analyze(AURL);
  Result := IsValid;
end;

function TLyURL.UpdateWith(const ARelativURL: string): boolean;
var
  U: TLyURL;
  P: string;
begin
  if IsValid then
  begin
    U := TLyURL.Create(ARelativURL);
    try
      Result := U.IsValid;
      if Result then Assign(U) else
      if (U.FScheme = '') and (U.FAuthority = '') and (U.FPath <> '') then
      begin
        if (U.FPath[1] <> '/') and (Pos(':', U.FPath) < 1) then
        begin
          P := UrlSetPathDelimiter(
            IncludeTrailingPathDelimiter(ExtractFilePath(SetPD(FPath))));
          U.FPath := UrlPackPath(P + U.FPath);
        end;
        if FAuthority <> '' then
          FPath := UrlEncodeFileToPath(U.FPath) else
          FPath := U.FPath;
        FQuery := U.FQuery;
        FFields.Assign(U.FFields);
        FFragment := U.FFragment;
        FURL := GetReference;
        Result := true;
      end;
    finally
      U.Free;
    end;
  end
  else Result := SetURL(ARelativURL);
end;

end.
