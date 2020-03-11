{==============================================================================}
{     PROJECT: lysee_inifs                                               }
{ DESCRIPTION: functions of init file                                          }
{   COPYRIGHT: Copyright (c) 2003-2012, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2007/07/12                                                      }
{    MODIFIED: 2013/09/16                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lminifile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, lysee;

{@pmc-description ini file class}
{@pmc-object inifile => inifile}

type

  inifile = class(TLyObject)
  private
    FIniFile: TIniFile;
    FFileName: string;
  public
    constructor Create(const FileName: string);
    destructor Destroy;override;
    procedure Open(const FileName: string);
    procedure Close;
    function Read(const Section, Key, DefValue: string): string;
    procedure Write(const Section, Key, Value: string);
    function fileName: string;
    function Sections: string;
    function Keys(const Section: string): string;
    function Values(const Section: string): string;
    function SectionExists(const Section: string): boolean;
    procedure EraseSection(const Section: string);
    function KeyExists(const Section, Key: string): boolean;
    procedure EraseKey(const Section, Key: string);
  end;

implementation

procedure inifile.Close;
begin
  FFileName := '';
  FreeAndNil(FIniFile);
end;

constructor inifile.Create(const FileName: string);
begin
  Open(FileName);
end;

destructor inifile.Destroy;
begin
  Close;
  inherited;
end;

procedure inifile.Open(const FileName: string);
begin
  try
    Close;
    FFileName := ExpandFileName(Trim(FileName));
    if FFileName <> '' then
      FIniFile := TIniFile.Create(FFileName);
  except
    FFileName := '';
    raise;
  end;
end;

function inifile.Read(const Section, Key, DefValue: string): string;
begin
  if FIniFile <> nil then
    Result := FIniFile.ReadString(Section, Key, DefValue) else
    Result := DefValue;
end;

procedure inifile.Write(const Section, Key, Value: string);
begin
  if FIniFile <> nil then
    FIniFile.WriteString(Section, Key, Value);
end;

function inifile.fileName: string;
begin
  Result := FFileName;
end;

function inifile.Sections: string;
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    FIniFile.ReadSections(list);
    Result := list.Text;
  finally
    list.Free;
  end;
end;

function inifile.keys(const Section: string): string;
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    FIniFile.ReadSection(Section, list);
    Result := list.Text;
  finally
    list.Free;
  end;
end;

function inifile.Values(const Section: string): string;
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    FIniFile.ReadSectionValues(Section, list);
    Result := list.Text;
  finally
    list.Free;
  end;
end;

function inifile.SectionExists(const Section: string): boolean;
begin
  Result := FIniFile.SectionExists(Section);
end;

procedure inifile.EraseSection(const Section: string);
begin
  FIniFile.EraseSection(Section);
end;

function inifile.KeyExists(const Section, Key: string): boolean;
begin
  Result := FIniFile.ValueExists(Section, Key);
end;

procedure inifile.EraseKey(const Section, Key: string);
begin
  FIniFile.DeleteKey(Section, Key);
end;

end.

