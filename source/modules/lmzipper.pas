{==============================================================================}
{        UNIT: lysee_zipper                                                    }
{ DESCRIPTION: zip/unzip functions                                             }
{   COPYRIGHT: Copyright (c) 2003-2012, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2007/07/12                                                      }
{    MODIFIED: 2013/09/16                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lmzipper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Zipper, lysee;

{@pmc-description zip/unzip for lysee}
function list(const zipFile: string): string;
procedure unzip(const zipFile, outputPath: string);
procedure unzipf(const zipFile, subFile, outputPath: string);
function zip(const zipFile, sourceFile: string): integer;
{@pmc-end}

type

  { TMyUnZipper }

  TMyUnZipper = class(TUnZipper)
  private
    FFileList: TStrings;
  public
    procedure UnZipOneFile(Item: TFullZipFileEntry);override;
  end;

implementation

function list(const zipFile: string): string;
var
  Z: TMyUnZipper;
begin
  Z := TMyUnZipper.Create;
  try
    Z.FFileList := TStringList.Create;
    try
      Z.UnZipAllFiles(zipFile);
      Result := Z.FFileList.Text;
    finally
      FreeAndNil(Z.FFileList);
    end;
  finally
    Z.Free;
  end;
end;

procedure unzip(const zipFile, outputPath: string);
var
  Z: TMyUnZipper;
begin
  Z := TMyUnZipper.Create;
  try
    Z.OutputPath := outputPath;
    Z.UnZipAllFiles(zipFile);
  finally
    Z.Free;
  end;
end;

procedure unzipf(const zipFile, subFile, outputPath: string);
var
  Z: TMyUnZipper;
  L: TStrings;
begin
  if subFile <> '' then
  begin
    Z := TMyUnZipper.Create;
    try
      L := TStringList.Create;
      try
        Z.OutputPath := outputPath;
        L.Add(subFile);
        Z.UnZipFiles(zipFile, L);
      finally
        L.Free;
      end;
    finally
      Z.Free;
    end;
  end;
end;

function zip(const zipFile, sourceFile: string): integer;
var
  Z: TZipper;
  L: TStrings;
  X: integer;
begin
  L := TStringList.Create;
  try
    L.Text := Trim(paramStr(1));
    for X := L.Count - 1 downto 0 do
    begin
      L[X] := Trim(L[X]);
      if not FileExists(L[X]) then
        L.Delete(X);
    end;

    if L.Count > 0 then
    begin
      Z := TZipper.Create;
      try
        Z.ZipFiles(Trim(paramStr(0)), L);
      finally
        Z.Free;
      end;
    end;

    Result := L.Count;
  finally
    L.Free;
  end;
end;

{ TMyUnZipper }

procedure TMyUnZipper.UnZipOneFile(Item: TFullZipFileEntry);
var
  ofile: string;
begin
  ofile := Item.ArchiveFileName;

  if FFileList <> nil then
  begin
    FFileList.Add(ofile);
    Exit;
  end;

  ofile := LySetPD(ofile);
  Item.DiskFileName := ofile;

  if ofile[Length(ofile)] = LSE_PATH_DELIMITER then
  begin
    if OutputPath <> '' then
      ForceDirectories(LySetPD(OutputPath+ LSE_PATH_DELIMITER + ofile)) else
      ForceDirectories(ofile);
    Exit;
  end;

  inherited UnZipOneFile(Item);
end;

end.

