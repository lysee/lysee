{==============================================================================}
{        UNIT: lysee_sysutils                                                  }
{ DESCRIPTION: lysee's sysutils module functions                               }
{   COPYRIGHT: Copyright (c) 2016-2016, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2016/12/18                                                      }
{    MODIFIED: 2017/02/19                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_sysutils;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, Basic, lysee;

type

  { TLyseeSearchRec }

  TLyseeSearchRec = class(TBasicObject)
  private
    FSR: TSearchRec;
    FOK: boolean;
    FPath: string;
    function GetName: string;
    function GetSize: int64;
    function GetAttr: integer;
    function GetPath: string;
    function GetFileName: string;
  public
    constructor Create(const Path: string; Attr: integer);
    destructor Destroy;override;
    function FindFirst(const Path: string; Attr: integer): boolean;
    function FindNext: boolean;
    procedure FindClose;
    function Active: boolean;
    function IsFile: boolean;
    function IsDirectory: boolean;
    property Path: string read GetPath;
    property Name: string read GetName;
    property FileName: string read GetFileName;
    property Size: int64 read GetSize;
    property Attr: integer read GetAttr;
  end;

  { TLyseeSearchRecGenerate }

  TLyseeSearchRecGenerate = class(TLyseeGenerate)
  private
    FSR: TLyseeSearchRec;
  public
    constructor CreateWith(const SR: TLyseeSearchRec);
    function GetNext: boolean;override;
    function HasNext: boolean;override;
  end;

  { TLyseeSearchRecType }

  TLyseeSearchRecType = class(TLyseeType)
  public
    function IncRefcount(Obj: pointer): integer;override;
    function DecRefcount(Obj: pointer): integer;override;
    function AsString(Obj: pointer): string;override;
    function AsBoolean(Obj: pointer): boolean;override;
    function Generate(Obj: pointer): TLyseeGenerate;override;
  protected
    procedure Setup;override;
    procedure MyCreate(const Param: TLyseeParam);
    procedure MyFindFirst(const Param: TLyseeParam);
    procedure MyFindNext(const Param: TLyseeParam);
    procedure MyFindClose(const Param: TLyseeParam);
    procedure MyActive(const Param: TLyseeParam);
    procedure MyIsFile(const Param: TLyseeParam);
    procedure MyIsDirectory(const Param: TLyseeParam);
    procedure MyGetPath(const Param: TLyseeParam);
    procedure MyGetName(const Param: TLyseeParam);
    procedure MyGetFileName(const Param: TLyseeParam);
    procedure MyGetSize(const Param: TLyseeParam);
    procedure MyGetAttr(const Param: TLyseeParam);
  end;

  { TLyseeSysUtilsModule }

  TLyseeSysUtilsModule = class(TLyseeModule)
  private
    procedure DoSetup(Sender: TObject);
  public
    constructor Create(const AName: string);override;
  end;

var
  my_sysutils: TLyseeSysUtilsModule;
  my_searchRec: TLyseeSearchRecType;
  my_replaceFlag: TLyseeEnumType;
  my_replaceFlags: TLyseeEnumSetType;

function GetReplaceFlags(V: TLyseeValue): TReplaceFlags;
procedure SetReplaceFlags(V: TLyseeValue; Value: TReplaceFlags);

implementation

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF} Math;

function GetReplaceFlags(V: TLyseeValue): TReplaceFlags;
var
  S: TLyseeEnumSet;
begin
  Result := [];
  S := V.AsEnumSet;
  if S <> nil then
  begin
    if S[Ord(rfReplaceAll)] then Include(Result, rfReplaceAll);
    if S[Ord(rfIgnoreCase)] then Include(Result, rfIgnoreCase);
  end;
end;

procedure SetReplaceFlags(V: TLyseeValue; Value: TReplaceFlags);
var
  S: TLyseeEnumSet;
begin
  S := my_replaceFlags.NewEnumSet;
  S.SetValue(V);
  S[Ord(rfReplaceAll)] := rfReplaceAll in Value;
  S[Ord(rfIgnoreCase)] := rfIgnoreCase in Value;
end;

// sysutils.functions ----------------------------------------------------------

procedure pp_Trim(const Param: TLyseeParam);
var
  S: string;
begin
  S := Param[0].AsString;
  Param.Result.AsString := Trim(S);
end;

procedure pp_TrimLeft(const Param: TLyseeParam);
var
  S: string;
begin
  S := Param[0].AsString;
  Param.Result.AsString := TrimLeft(S);
end;

procedure pp_TrimRight(const Param: TLyseeParam);
var
  S: string;
begin
  S := Param[0].AsString;
  Param.Result.AsString := TrimRight(S);
end;

procedure pp_TrimAll(const Param: TLyseeParam);
var
  S: string;
begin
  S := Param[0].AsString;
  Param.Result.AsString := TrimAll(S);
end;

procedure pp_LowerCase(const Param: TLyseeParam);
var
  S: string;
begin
  S := Param[0].AsString;
  Param.Result.AsString := LowerCase(S);
end;

procedure pp_UpperCase(const Param: TLyseeParam);
var
  S: string;
begin
  S := Param[0].AsString;
  Param.Result.AsString := UpperCase(S);
end;

procedure pp_StringReplace(const Param: TLyseeParam);
var
  S, P, N: string;
  flags: TReplaceFlags;
begin
  S := Param[0].AsString;         // string
  P := Param[1].AsString;         // patten
  N := Param[2].AsString;         // new string
  if Param.Prmc > 3 then
    flags := GetReplaceFlags(Param[3]) else
    flags := [rfReplaceAll];
  Param.Result.AsString := StringReplace(S, P, N, flags);
end;

procedure pp_Now(const Param: TLyseeParam);
begin
  Param.Result.AsTime := Now;
end;

procedure pp_GetEnvironmentStrings(const Param: TLyseeParam);
var
  L: TStrings;
  {$IFDEF FPC}
  I, N: integer;
  {$ELSE}
  H, P: PChar;
  {$ENDIF}
begin
  L := TStringList.Create;
  try
    {$IFDEF FPC}
    N := SysUtils.GetEnvironmentVariableCount;
    for I := 1 to N do
      L.Add(SysUtils.GetEnvironmentString(I));
    {$ELSE}
    P := GetEnvironmentStrings;
    H := P;
    if H <> nil then
      while H^ <> #0 do
      begin
        L.Add(H);
        H := H + StrLen(H) + 1;
      end;
    FreeEnvironmentStrings(P);
    {$ENDIF}
    Param.Result.AsString := L.Text;
  finally
    L.Free;
  end;
end;

procedure pp_IsLeapYear(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := IsLeapYear(Param[0].AsInteger);
end;

procedure pp_GetTempFileName(const Param: TLyseeParam);
begin
  Param.Result.AsString := my_tmpath + GenID + Param[0].AsString;
end;

procedure pp_IntToStr(const Param: TLyseeParam);
begin
  Param.Result.AsString := IntToStr(Param[0].AsInteger);
end;

procedure pp_StrToInt(const Param: TLyseeParam);
begin
  Param.Result.AsInteger := StrToInt64(Param[0].AsString);
end;

procedure pp_IntToHex(const Param: TLyseeParam);
begin
  Param.Result.AsString := IntToHex(Param[0].AsInteger, Param[1].AsInteger);
end;

procedure pp_HexToInt(const Param: TLyseeParam);
begin
  Param.Result.AsInteger := StrToInt64('$' + Param[0].AsString);
end;

procedure pp_CompareText(const Param: TLyseeParam);
begin
  Param.Result.AsInteger := SysUtils.CompareText(Param[0].AsString, Param[1].AsString);
end;

procedure pp_SameText(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := SysUtils.SameText(Param[0].AsString, Param[1].AsString);
end;

procedure pp_CompareStr(const Param: TLyseeParam);
begin
  Param.Result.AsInteger := SysUtils.CompareStr(Param[0].AsString, Param[1].AsString);
end;

procedure pp_SameStr(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := (Param[0].AsString = Param[1].AsString);
end;

procedure pp_IsIdent(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := IsID(Param[0].AsString);
end;

procedure pp_IsAlpha(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := InChars(Param[0].AsString, CS_ALPHA);
end;

procedure pp_IsAlnum(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := InChars(Param[0].AsString, CS_ALNUM);
end;

procedure pp_IsCntrl(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := InChars(Param[0].AsString, CS_CONTROL);
end;

procedure pp_IsDigit(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := InChars(Param[0].AsString, CS_DIGIT);
end;

procedure pp_IsSpace(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := InChars(Param[0].AsString, CS_SPACE);
end;

procedure pp_IsHex(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := InChars(Param[0].AsString, CS_HEX);
end;

procedure pp_IsLowerCase(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := InChars(Param[0].AsString, CS_LOWER);
end;

procedure pp_IsUpperCase(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := InChars(Param[0].AsString, CS_UPPER);
end;

procedure pp_SaveTextToFile(const Param: TLyseeParam);
var
  L: TStrings;
begin
  L := TStringList.Create;
  try
    L.Add(Param[0].AsString);
    L.SaveToFile(Param[1].GetFileName);
  finally
    L.Free;
  end;
end;

procedure pp_Sleep(const Param: TLyseeParam);
begin
  SysUtils.Sleep(Param[0].AsInteger);
end;

procedure pp_Format(const Param: TLyseeParam);
begin
  Param.Result.AsString := Param[1].AsArray.AsStringFmt(Param[0].AsString);
end;

procedure pp_GetEnvironmentVariable(const Param: TLyseeParam);
begin
  Param.Result.AsString := SysUtils.GetEnvironmentVariable(Param[0].AsString);
end;

procedure pp_IncludeTrailingBackslash(const Param: TLyseeParam);
begin
  Param.Result.AsString := IncludeTrailingBackslash(Param[0].AsString);
end;

procedure pp_IncludeTrailingPathDelimiter(const Param: TLyseeParam);
begin
  Param.Result.AsString := IncludeTrailingPathDelimiter(Param[0].AsString);
end;

procedure pp_ExcludeTrailingBackslash(const Param: TLyseeParam);
begin
  Param.Result.AsString := ExcludeTrailingBackslash(Param[0].AsString);
end;

procedure pp_ExcludeTrailingPathDelimiter(const Param: TLyseeParam);
begin
  Param.Result.AsString := ExcludeTrailingPathDelimiter(Param[0].AsString);
end;

procedure pp_SetPathDelimiter(const Param: TLyseeParam);
begin
  Param.Result.AsString := basic.SetPD(Param[0].AsString);
end;

procedure pp_DeleteFile(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := SysUtils.DeleteFile(Param[0].GetFileName);
end;

procedure pp_RenameFile(const Param: TLyseeParam);
var
  OldName: string;
  NewName: string;
begin
  OldName := Param[0].GetFileName;
  NewName := Param[1].GetFileName;
  Param.Result.AsBoolean := SysUtils.RenameFile(OldName, NewName);
end;

procedure pp_CopyFile(const Param: TLyseeParam);
var
  F1: string;
  F2: string;
begin
  F1 := Param[0].GetFileName;
  F2 := Param[1].GetFileName;
  Param.Result.AsBoolean := Windows.CopyFile(pchar(F1), pchar(F2), Param[2].AsBoolean);
end;

procedure pp_RemoveDir(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := RemoveDir(Param[0].GetFileName);
end;

procedure pp_CreateDir(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := CreateDir(Param[0].GetFileName);
end;

procedure pp_GetCurrentDir(const Param: TLyseeParam);
begin
  Param.Result.AsString := GetCurrentDir;
end;

procedure pp_SetCurrentDir(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := SetCurrentDir(Param[0].GetFileName);
end;

procedure pp_FileExists(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := FileExists(Param[0].GetFileName);
end;

procedure pp_DirectoryExists(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := DirectoryExists(Param[0].GetFileName);
end;

procedure pp_ForceDirectories(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := ForceDirectories(Param[0].GetFileName);
end;

procedure pp_ChangeFileExt(const Param: TLyseeParam);
begin
  Param.Result.AsString := ChangeFileExt(Param[0].GetFileName, Param[1].AsString);
end;

{$IFNDEF FPC}
procedure pp_ChangeFilePath(const Param: TLyseeParam);
begin
  Param.Result.AsString := ChangeFilePath(Param[0].GetFileName, Param[1].GetFileName);
end;
{$ENDIF}

procedure pp_ExtractFilePath(const Param: TLyseeParam);
begin
  Param.Result.AsString := ExtractFilePath(Param[0].GetFileName);
end;

procedure pp_ExtractFileDir(const Param: TLyseeParam);
begin
  Param.Result.AsString := ExtractFileDir(Param[0].GetFileName);
end;

procedure pp_ExtractFileName(const Param: TLyseeParam);
begin
  Param.Result.AsString := ExtractFileName(Param[0].GetFileName);
end;

procedure pp_ExtractFileExt(const Param: TLyseeParam);
begin
  Param.Result.AsString := ExtractFileExt(Param[0].GetFileName);
end;

{$IFNDEF FPC}
procedure pp_GetHomePath(const Param: TLyseeParam);
begin
  Param.Result.AsString := GetHomePath;
end;
{$ENDIF}

procedure pp_ExpandFileName(const Param: TLyseeParam);
begin
  Param.Result.AsString := ExpandFileName(Param[0].GetFileName);
end;

procedure pp_ExtractRelativePath(const Param: TLyseeParam);
begin
  Param.Result.AsString := ExtractRelativePath(Param[0].GetFileName, Param[1].GetFileName);
end;

{$IFNDEF FPC}
procedure pp_IsRelativePath(const Param: TLyseeParam);
begin
  Param.Result.AsBoolean := IsRelativePath(Param[0].GetFileName);
end;
{$ENDIF}

{ TLyseeSearchRec }

function TLyseeSearchRec.Active: boolean;
begin
  Result := FOK;
end;

constructor TLyseeSearchRec.Create(const Path: string; Attr: integer);
begin
  FindFirst(Path, Attr);
end;

destructor TLyseeSearchRec.Destroy;
begin
  FindClose;
  inherited;
end;

procedure TLyseeSearchRec.FindClose;
begin
  if FOK then
  begin
    FOK := false;
    FPath := '';
    SysUtils.FindClose(FSR);
  end;
end;

function TLyseeSearchRec.FindFirst(const Path: string; Attr: integer): boolean;
var
  F: string;
begin
  FindClose;
  F := ExpandFileName(SetPD(Trim(Path)));
  FPath := ExtractFilePath(F);
  FOK := (SysUtils.FindFirst(F, Attr, FSR) = 0);
  if FOK and ((FSR.Name = '.') or (FSR.Name = '..')) then FindNext;
  Result := FOK;
end;

function TLyseeSearchRec.FindNext: boolean;
begin
  if FOK then
  begin
    Result := (SysUtils.FindNext(FSR) = 0);
    while Result and ((FSR.Name = '.') or (FSR.Name = '..')) do
      Result := (SysUtils.FindNext(FSR) = 0);
    if not Result then
      FindClose;
  end
  else Result := false;
end;

function TLyseeSearchRec.GetAttr: integer;
begin
  if FOK then
    Result := FSR.Attr else
    Result := 0;
end;

function TLyseeSearchRec.GetFileName: string;
begin
  if FOK then
    Result := FPath + FSr.Name else
    Result := '';
end;

function TLyseeSearchRec.GetName: string;
begin
  if FOK then
    Result := FSR.Name else
    Result := '';
end;

function TLyseeSearchRec.GetPath: string;
begin
  if FOK then
    Result := FPath else
    Result := '';
end;

function TLyseeSearchRec.GetSize: int64;
begin
  if FOK then
    Result := FSR.Size else
    Result := -1;
end;

function TLyseeSearchRec.IsDirectory: boolean;
begin
  Result := FOK and ((FSR.Attr and faDirectory) <> 0);
end;

function TLyseeSearchRec.IsFile: boolean;
begin
  Result := FOK and ((FSR.Attr and faDirectory) = 0);
end;

{ TLyseeSearchRecGenerate }

constructor TLyseeSearchRecGenerate.CreateWith(const SR: TLyseeSearchRec);
begin
  inherited Create;
  FSR := SR;
end;

function TLyseeSearchRecGenerate.GetNext: boolean;
begin
  Result := HasNext;
  if Result then
  begin
    AsString := FSR.FileName;
    FSR.FindNext;
  end
  else SetNil;
end;

function TLyseeSearchRecGenerate.HasNext: boolean;
begin
  Result := (FSR <> nil) and FSR.Active;
end;

{ TLyseeSearchRecType }

function TLyseeSearchRecType.IncRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeSearchRec(Obj).IncRefcount else
    Result := 0;
end;

function TLyseeSearchRecType.DecRefcount(Obj: pointer): integer;
begin
  if Obj <> nil then
    Result := TLyseeSearchRec(Obj).DecRefcount else
    Result := 0;
end;

function TLyseeSearchRecType.Generate(Obj: pointer): TLyseeGenerate;
begin
  if (Obj <> nil) and TLyseeSearchRec(Obj).Active then
    Result := TLyseeSearchRecGenerate.CreateWith(TLyseeSearchRec(Obj)) else
    Result := nil;
end;

function TLyseeSearchRecType.AsBoolean(Obj: pointer): boolean;
begin
  Result := (Obj <> nil) and TLyseeSearchRec(Obj).Active;
end;

function TLyseeSearchRecType.AsString(Obj: pointer): string;
begin
  if Obj <> nil then
    Result := TLyseeSearchRec(Obj).FileName else
    Result := '';
end;

procedure TLyseeSearchRecType.Setup;
begin
  Module.Consts.Add('faReadOnly').AsInteger := faReadOnly;
  Module.Consts.Add('faHidden').AsInteger := faHidden;
  Module.Consts.Add('faSysFile').AsInteger := faSysFile;
  Module.Consts.Add('faVolumeId').AsInteger := faVolumeId;
  Module.Consts.Add('faDirectory').AsInteger := faDirectory;
  Module.Consts.Add('faArchive').AsInteger := faArchive;
  Module.Consts.Add('faSymLink').AsInteger := faSymLink;
  Module.Consts.Add('faAnyFile').AsInteger := faAnyFile;

  Method('Create', Self, ['Path', '_Attr'], [my_string, my_int],
         {$IFDEF FPC}@{$ENDIF}MyCreate);
  Method('FindFirst', my_bool, ['Path', '_Attr'], [my_string, my_int],
         {$IFDEF FPC}@{$ENDIF}MyFindFirst);
  Method('FindNext', my_bool, {$IFDEF FPC}@{$ENDIF}MyFindNext);
  Method('FindClose', {$IFDEF FPC}@{$ENDIF}MyFindClose);
  Method('Active', my_bool, {$IFDEF FPC}@{$ENDIF}MyActive);
  Method('IsFile', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsFile);
  Method('IsDirectory', my_bool, {$IFDEF FPC}@{$ENDIF}MyIsDirectory);
  Define('Path', my_string, {$IFDEF FPC}@{$ENDIF}MyGetPath);
  Define('Name', my_string, {$IFDEF FPC}@{$ENDIF}MyGetName);
  Define('FileName', my_string, {$IFDEF FPC}@{$ENDIF}MyGetFileName);
  Define('Size', my_int, {$IFDEF FPC}@{$ENDIF}MyGetSize);
  Define('Attr', my_int, {$IFDEF FPC}@{$ENDIF}MyGetAttr);
end;

procedure TLyseeSearchRecType.MyCreate(const Param: TLyseeParam);
var
  Path: string;
  Attr: integer;
begin
  Path := Param[0].AsString;
  if Param.Prmc > 1 then
    Attr := integer(Param[1].AsInteger) else
    Attr := faAnyFile;
  Param.Result.SetTOA(my_searchRec, TLyseeSearchRec.Create(Path, Attr));
end;

procedure TLyseeSearchRecType.MyFindFirst(const Param: TLyseeParam);
var
  This: TLyseeSearchRec;
  Path: string;
  Attr: integer;
begin
  if not Param.GetSelf(This) then Exit;
  Path := Param[1].AsString;
  if Param.Prmc > 2 then
    Attr := integer(Param[2].AsInteger) else
    Attr := faAnyFile;
  Param.Result.AsBoolean := This.FindFirst(Path, Attr);
end;

procedure TLyseeSearchRecType.MyFindNext(const Param: TLyseeParam);
var
  This: TLyseeSearchRec;
begin
  if not Param.GetSelf(This) then Exit;
  Param.Result.AsBoolean := This.FindNext;
end;

procedure TLyseeSearchRecType.MyFindClose(const Param: TLyseeParam);
var
  This: TLyseeSearchRec;
begin
  if not Param.GetSelf(This) then Exit;
  This.FindClose;
end;

procedure TLyseeSearchRecType.MyActive(const Param: TLyseeParam);
var
  This: TLyseeSearchRec;
begin
  if not Param.GetSelf(This) then Exit;
  Param.Result.AsBoolean := This.Active;
end;

procedure TLyseeSearchRecType.MyIsFile(const Param: TLyseeParam);
var
  This: TLyseeSearchRec;
begin
  if not Param.GetSelf(This) then Exit;
  Param.Result.AsBoolean := This.IsFile;
end;

procedure TLyseeSearchRecType.MyIsDirectory(const Param: TLyseeParam);
var
  This: TLyseeSearchRec;
begin
  if not Param.GetSelf(This) then Exit;
  Param.Result.AsBoolean := This.IsDirectory;
end;

procedure TLyseeSearchRecType.MyGetPath(const Param: TLyseeParam);
var
  This: TLyseeSearchRec;
begin
  if not Param.GetSelf(This) then Exit;
  Param.Result.AsString := This.Path;
end;

procedure TLyseeSearchRecType.MyGetName(const Param: TLyseeParam);
var
  This: TLyseeSearchRec;
begin
  if not Param.GetSelf(This) then Exit;
  Param.Result.AsString := This.Name;
end;

procedure TLyseeSearchRecType.MyGetFileName(const Param: TLyseeParam);
var
  This: TLyseeSearchRec;
begin
  if not Param.GetSelf(This) then Exit;
  Param.Result.AsString := This.FileName;
end;

procedure TLyseeSearchRecType.MyGetSize(const Param: TLyseeParam);
var
  This: TLyseeSearchRec;
begin
  if not Param.GetSelf(This) then Exit;
  Param.Result.AsInteger := This.Size;
end;

procedure TLyseeSearchRecType.MyGetAttr(const Param: TLyseeParam);
var
  This: TLyseeSearchRec;
begin
  if not Param.GetSelf(This) then Exit;
  Param.Result.AsInteger := int64(This.Attr);
end;

{ TLyseeSysUtilsModule }

constructor TLyseeSysUtilsModule.Create(const AName: string);
begin
  inherited;
  OnSetup := {$IFDEF FPC}@{$ENDIF}DoSetup;
end;

procedure TLyseeSysUtilsModule.DoSetup(Sender: TObject);
begin
  OnSetup := nil;

  my_searchRec := TLyseeSearchRecType.Create('TSearchRec', Self, nil);
  my_replaceFlag := TLyseeEnumType.Create('TReplaceFlag', Self, nil);
  my_replaceFlag.AddItems(['rfReplaceAll', 'rfIgnoreCase']);
  my_replaceFlags := my_replaceFlag.NewEnumSetType('TReplaceFlags');

  my_searchRec.Setup;

  AddFunc('StringReplace', my_string,
          ['S', 'Patten', '_NewStr', '_ReplaceFlags'],
          [my_string, my_string, my_string, my_replaceFlags],
          {$IFDEF FPC}@{$ENDIF}pp_StringReplace);

  AddFunc('Trim', my_string, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_Trim);
  AddFunc('TrimAll', my_string, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_TrimAll);
  AddFunc('TrimLeft', my_string, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_TrimLeft);
  AddFunc('TrimRight', my_string, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_TrimRight);
  AddFunc('LowerCase', my_string, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_LowerCase);
  AddFunc('UpperCase', my_string, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_UpperCase);
  AddFunc('Now', my_time,
          {$IFDEF FPC}@{$ENDIF}pp_Now);
  AddFunc('GetEnvironmentStrings', my_string,
          {$IFDEF FPC}@{$ENDIF}pp_GetEnvironmentStrings);
  AddFunc('IsLeapYear', my_bool, ['Year'], [my_int],
          {$IFDEF FPC}@{$ENDIF}pp_IsLeapYear);
  AddFunc('GetTempFileName', my_string, ['_FileExt'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_GetTempFileName);
  AddFunc('IntToStr', my_string, ['Value'], [my_int],
          {$IFDEF FPC}@{$ENDIF}pp_IntToStr);
  AddFunc('StrToInt', my_int, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_StrToInt);
  AddFunc('IntToHex', my_string,
          ['Value', '_Digits'], [my_int, my_int],
          {$IFDEF FPC}@{$ENDIF}pp_IntToHex);
  AddFunc('HexToInt', my_int, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_HexToInt);
  AddFunc('CompareText', my_int,
          ['S1', 'S2'], [my_string, my_string],
          {$IFDEF FPC}@{$ENDIF}pp_CompareText);
  AddFunc('SameText', my_bool,
          ['S1', 'S2'], [my_string, my_string],
          {$IFDEF FPC}@{$ENDIF}pp_SameText);
  AddFunc('CompareStr', my_int,
          ['S1', 'S2'], [my_string, my_string],
          {$IFDEF FPC}@{$ENDIF}pp_CompareStr);
  AddFunc('SameStr', my_bool,
          ['S1', 'S2'], [my_string, my_string],
          {$IFDEF FPC}@{$ENDIF}pp_SameStr);
  AddFunc('IsIdent', my_bool, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_IsIdent);
  AddFunc('IsAlpha', my_bool, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_IsAlpha);
  AddFunc('IsAlnum', my_bool, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_IsAlnum);
  AddFunc('IsCntrl', my_bool, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_IsCntrl);
  AddFunc('IsSpace', my_bool, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_IsSpace);
  AddFunc('IsDigit', my_bool, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_IsDigit);
  AddFunc('IsHex', my_bool, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_IsHex);
  AddFunc('IsLowerCase', my_bool, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_IsLowerCase);
  AddFunc('IsUpperCase', my_bool, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_IsUpperCase);
  AddFunc('SaveTextToFile', ['FileName'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_SaveTextToFile);
  AddFunc('Sleep', ['MilliSeconds'], [my_int],
          {$IFDEF FPC}@{$ENDIF}pp_Sleep);
  AddFunc('Format', my_string, ['Fmt', 'Args'], [my_string, my_array],
          {$IFDEF FPC}@{$ENDIF}pp_Format);
  AddFunc('GetEnvironmentVariable', my_string, ['Name'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_GetEnvironmentVariable);
  AddFunc('IncludeTrailingBackslash', my_string, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_IncludeTrailingBackslash);
  AddFunc('IncludeTrailingPathDelimiter', my_string, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_IncludeTrailingPathDelimiter);
  AddFunc('ExcludeTrailingBackslash', my_string, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_ExcludeTrailingBackslash);
  AddFunc('ExcludeTrailingPathDelimiter', my_string, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_ExcludeTrailingPathDelimiter);
  AddFunc('SetPathDelimiter', my_string, ['FileName'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_SetPathDelimiter);
  AddFunc('DeleteFile', my_bool, ['FileName'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_DeleteFile);
  AddFunc('RenameFile', my_bool, ['OldName', 'NewName'], [my_string, my_string],
          {$IFDEF FPC}@{$ENDIF}pp_RenameFile);
  AddFunc('CopyFile', my_bool,
          ['SourceFile', 'DestiFile', '_FailIfDestiFileExists'],
          [my_string, my_string, my_bool],
          {$IFDEF FPC}@{$ENDIF}pp_CopyFile);
  AddFunc('RemoveDir', my_bool, ['Dir'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_RemoveDir);
  AddFunc('CreateDir', my_bool, ['Dir'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_CreateDir);
  AddFunc('GetCurrentDir', my_string,
          {$IFDEF FPC}@{$ENDIF}pp_GetCurrentDir);
  AddFunc('SetCurrentDir', my_bool, ['Dir'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_SetCurrentDir);
  AddFunc('FileExists', my_bool, ['FileName'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_FileExists);
  AddFunc('DirectoryExists', my_bool, ['Directory'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_DirectoryExists);
  AddFunc('ForceDirectories', my_bool, ['Dir'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_ForceDirectories);
  AddFunc('ChangeFileExt', my_string,
          ['FileName', 'Extension'], [my_string, my_string],
          {$IFDEF FPC}@{$ENDIF}pp_ChangeFileExt);
  {$IFNDEF FPC}
  AddFunc('ChangeFilePath', my_string,
          ['FileName', 'Path'], [my_string, my_string],
          {$IFDEF FPC}@{$ENDIF}pp_ChangeFilePath);
  {$ENDIF}
  AddFunc('ExtractFilePath', my_string, ['FileName'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_ExtractFilePath);
  AddFunc('ExtractFileDir', my_string, ['FileName'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_ExtractFileDir);
  AddFunc('ExtractFileName', my_string, ['FileName'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_ExtractFileName);
  AddFunc('ExtractFileExt', my_string, ['FileName'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_ExtractFileExt);
  {$IFNDEF FPC}
  AddFunc('GetHomePath', my_string,
          {$IFDEF FPC}@{$ENDIF}pp_GetHomePath);
  {$ENDIF}
  AddFunc('ExpandFileName', my_string, ['FileName'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_ExpandFileName);
  AddFunc('ExtractRelativePath', my_string,
          ['BaseName', 'DestName'], [my_string, my_string],
          {$IFDEF FPC}@{$ENDIF}pp_ExtractRelativePath);
  {$IFNDEF FPC}
  AddFunc('IsRelativePath', my_bool, ['Path'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_IsRelativePath);
  {$ENDIF}
end;

initialization
begin
  my_sysutils := TLyseeSysUtilsModule.Create('SysUtils');
end;

end.
