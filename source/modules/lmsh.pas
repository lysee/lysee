{==============================================================================}
{     PROJECT: lysee_sh                                                        }
{ DESCRIPTION: shell functions of current OS                                   }
{   COPYRIGHT: Copyright (c) 2003-2012, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/12/10                                                      }
{    MODIFIED: 2013/09/16                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lmsh;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes {$IFDEF WINDOWS},Windows{$ENDIF}, lysee;

{@pmc-description shell utilities for lysee}

type

  { searchRec }

  {@pmc-object searchRec => searchRec}
  searchRec = class(TLyObject)
  private
    FNode: TSearchRec;
    FMask: string;
    FPath: string;
    FActive: boolean;
    FFile: string;
    function IsDD(const S: string): boolean;
  public
    destructor Destroy;override;
    function Find(const AFileMask: string): boolean;
    function Next: boolean;
    procedure Close;
    function Active: boolean;
    function Eof: boolean;
    function Path: string;
    function Name: string;
    function FullName: string;
    function IsFile: boolean;
    function IsDir: boolean;
    function Size: int64;
    property FileName: string read FFile;
  end;

  { directory }

  {@pmc-object directory => directory}
  directory = class(TLyObject)
  private
    FDir: string;
  public
    constructor Create(const Dir: string);
    function Exists: boolean;
    function Files(Param: PLyParam): HLyVarlist;
    function Directories(Param: PLyParam): HLyVarlist;
    function ParentDirectory: directory;
    function SameWith(ADir: directory): boolean;
  end;

function FindFile(const FileMask: string): searchRec;
function IsFile(const FileName: string): boolean;
function IsDir(const DirName: string): boolean;
function GetDir: string;
function SetDir(const NewDir: string): boolean;
function ListFile(const FileMask: string): string;
function CopyFile(const SourceFile, DestiFile: string; failIfFileExists: boolean): boolean;
function RemoveFile(const FileName: string): boolean;
function RemoveDir(const Dir: string; delTree: boolean): boolean;
function MakeDir(const Dir: string): boolean;

implementation

function copy_file(const Source, Desti: string; FailIfExists: boolean): boolean;
{$IFNDEF WINDOWS}
var
  fi, fo: TFileStream;
{$ENDIF}
begin
  Result := false;
  try
    {$IFDEF WINDOWS}
    Result := Windows.CopyFile(pchar(Source), pchar(Desti), FailIfExists);
    {$ELSE}
    if not FailIfExists or not FileExists(Desti) then
    begin
      fi := TFileStream.Create(Source, fmShareDenyWrite);
      try
        fo := TFileStream.Create(Desti, fmCreate);
        try
          fo.CopyFrom(fi, 0);
        finally
          fo.Free;
        end;
      finally
        fi.Free;
      end;
      Result := true;
    end;
    {$ENDIF}
  except
    { do nothing }
  end;
end;

function remove_tree(const Dir: string): boolean;
{var
  sr: searchRec;}
begin
  Result := SysUtils.RemoveDir(Dir);
{ if not Result then
  begin
    sr := searchRec.Create(Dir + '\*.*');
    try
      sr.DeleteAll(true, true, true);
    finally
      sr.Free;
    end;
    Result := SysUtils.RemoveDir(Dir);
  end;}
end;

function FindFile(const FileMask: string): searchRec;
begin
  Result := searchRec.Create;
  if not Result.Find(FileMask) then
    FreeAndNil(Result);
end;

function IsFile(const FileName: string): boolean;
begin
  Result := FileExists(FileName);
end;

function IsDir(const DirName: string): boolean;
begin
  Result := DirectoryExists(DirName);
end;

function GetDir: string;
begin
  Result := GetCurrentDir;
end;

function SetDir(const NewDir: string): boolean;
begin
  Result := SetCurrentDir(NewDir);
end;

function CopyFile(const SourceFile, DestiFile: string; failIfFileExists: boolean): boolean;
begin
  Result := copy_file(SourceFile, DestiFile, failIfFileExists);
end;

function RemoveFile(const FileName: string): boolean;
begin
  Result := SysUtils.DeleteFile(FileName);
end;

function ListFile(const FileMask: string): string;
var
  L: string;
  S: searchRec;
begin
  L := Trim(FileMask);
  if L = '' then L := '.\*.*' else
  if DirectoryExists(L) then
    L := IncludeTrailingPathDelimiter(L) + '*.*';
  S := searchRec.Create;
  try
    if S.Find(L) then
    begin
      L := S.FullName;
      while S.Next do
        L := L + sLineBreak + S.FullName;
      Result := L;
    end
    else Result := '';
  finally
    S.Free;
  end;
end;

function RemoveDir(const Dir: string; delTree: boolean): boolean;
var
  P: string;
begin
  Result := false;
  P := LyExcPD(LyExpandFileName(Dir));
  if DirectoryExists(P) then
  begin
    Result := SysUtils.RemoveDir(P);
    if not Result and delTree then
      Result := remove_tree(P);
  end;
end;

function MakeDir(const Dir: string): boolean;
begin
  Result := ForceDirectories(Dir);
end;

{ searchRec }

function searchRec.Active: boolean;
begin
  Result := FActive;
end;

function searchRec.Eof: boolean;
begin
  Result := not FActive;
end;

function searchRec.Path: string;
begin
  Result := FPath;
end;

function searchRec.Name: string;
begin
  if FActive then
    Result := FNode.Name else
    Result := '';
end;

function searchRec.FullName: string;
begin
  if FActive then
    Result := FPath + FNode.Name else
    Result := '';
end;

function searchRec.IsFile: boolean;
begin
  Result := FActive and ((FNode.Attr and faDirectory) = 0);
end;

function searchRec.IsDir: boolean;
begin
  Result := FActive and ((FNode.Attr and faDirectory) <> 0);
end;

function searchRec.Size: int64;
begin
  if FActive then
    Result := FNode.Size else
    Result := 0;
end;

destructor searchRec.Destroy;
begin
  Close;
  inherited;
end;

procedure searchRec.Close;
begin
  if FActive then
  begin
    FActive := false;
    SysUtils.FindClose(FNode);
  end;
end;

function searchRec.Find(const AFileMask: string): boolean;
begin
  Close;
  FMask := ExpandFileName(LySetPD(Trim(AFileMask)));
  FPath := ExtractFilePath(FMask);
  FActive := (FMask <> '') and (SysUtils.FindFirst(FMask, faAnyFile, FNode) = 0);
  if FActive then
  begin
    FFile := FNode.Name;
    while FActive and IsDD(FNode.Name) do
      FActive := (SysUtils.FindNext(FNode) = 0);
    if not FActive then
      SysUtils.FindClose(FNode);
  end;
  Result := FActive;
end;

function searchRec.Next: boolean;
begin
  if FActive then
  begin
    FActive := (SysUtils.FindNext(FNode) = 0);
    while FActive and IsDD(FNode.Name) do
      FActive := (SysUtils.FindNext(FNode) = 0);
    if not FActive then
      SysUtils.FindClose(FNode);
  end;
  Result := FActive;
end;

function searchRec.IsDD(const S: string): boolean;
begin
  Result := (S = '.') or (S = '..');
end;

{ directory }

constructor directory.Create(const Dir: string);
begin
  FDir := ExpandFileName(LySetPD(Trim(Dir)));
end;

function directory.Exists: boolean;
begin
  Result := DirectoryExists(FDir);
end;

function directory.Files(Param: PLyParam): HLyVarlist;
begin
  Result := nil;
end;

function directory.Directories(Param: PLyParam): HLyVarlist;
begin
  Result := nil;
end;

function directory.ParentDirectory: directory;
begin
  Result := nil;
end;

function directory.SameWith(ADir: directory): boolean;
begin
  Result := false;
end;

end.
