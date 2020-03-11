unit lysee_pad_sheet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Messages,
  CodeMemo, Basic;

const

  PAD_TITLE = 'LyseePAD';
  PAD_SAVEPROMPT = 'Save changes to file?';
  PAD_EOPEN = 'File %s is being edited by another program';
  PAD_DRAWCLIPBOARD = WM_DRAWCLIPBOARD;
  PAD_SHOWFILE = WM_USER + 10;

type

  { TLyFilePage }

  TLyFilePage = class(TTabSheet)
  private
    FFileName: string;
    FMemo: TLyCodeMemo;
    FEditInfo: RLyEditInfo;
    FFileLock: TLyFileEditLock;
    function GetFileID: int64;
    function GetIsEmpty: boolean;
    function GetLocked: boolean;
    function GetMemo: TLyCodeMemo;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy;override;
    procedure LoadFromFile(const FileName: string; IsCmd: boolean);
    procedure SaveToFile(const FileName: string);
    procedure Save;
    procedure ErrorLocked(const FileName: string);
    procedure ResetHilighter;
    procedure SetCaption;
    property Memo: TLyCodeMemo read GetMemo;
    property FileID: int64 read GetFileID;
    property FileName: string read FFileName;
    property IsEmpty: boolean read GetIsEmpty;
    property Locked: boolean read GetLocked;
    property OnDblClick;
  end;

implementation

uses
  Windows, MsgBox, lysee_pad_main;

var
  file_seed: integer = 1;

{ TLyFilePage }

function TLyFilePage.GetMemo: TLyCodeMemo;
begin
  if FMemo = nil then
  begin
    FMemo := CodeMemo.PlaceACodeMemo(Self);
    FMemo.Hilighter.HilightClass := TLyseeHilighter;
  end;
  Result := FMemo;
end;

function TLyFilePage.GetFileID: int64;
begin
  Result := FEditInfo.ei_file;
end;

function TLyFilePage.GetIsEmpty: boolean;
begin
  Result := (FMemo = nil) or ((FFileName = '') and not FMemo.Modified);
end;

function TLyFilePage.GetLocked: boolean;
begin
  Result := FFileLock.Locked;
end;

constructor TLyFilePage.Create(TheOwner: TComponent);
var
  S: AnsiString;
begin
  inherited;
  FFileLock := TLyFileEditLock.Create;
  FEditInfo.ei_process := GetProcessID ;
  FEditInfo.ei_window := MainForm.Handle;
  FEditInfo.ei_file := file_seed;
  Inc(file_seed);
  S := Application.ExeName;
  Move(pointer(S)^, FEditInfo.ei_program[0], Length(S));
  SetCaption;
end;

destructor TLyFilePage.Destroy;
begin
  FreeAndNil(FFileLock);
  inherited;
end;

procedure TLyFilePage.LoadFromFile(const FileName: string; IsCmd: boolean);
var
  F: string;
  H: RLyEditInfo;
  L, P: TLyFileEditLock;
begin
  // check current file
  F := ExpandFileName(FileName);
  if SameFileName(F, FFileName) then Exit;

  // lock target file
  L := TLyFileEditLock.Create;
  if not L.LockForFile(F) then
  begin
    if TLyFileEditLock.ReadEditInfo(FileName, H) then
      PostMessage(H.ei_window, PAD_SHOWFILE, H.ei_file, H.ei_file) else
      IsCmd := false;
    if not IsCmd then ErrorLocked(FileName);
    L.free;
    Exit;
  end;

  try
    try
      FMemo.Readonly := true;
      FMemo.Lines.LoadFromFile(FileName);
      L.WriteEditInfo(FEditInfo);
      P := FFileLock;
      FFileLock := L;
      L := nil;
      P.Free;
      FFileName := F;
      ResetHilighter;
      FFileLock.WriteEditInfo(FEditInfo);
    finally
      FMemo.Readonly := false;
      SetCaption;
    end;
  except
    FreeAndNil(L);
    MsgErr(ExceptionStr);
  end;
end;

procedure TLyFilePage.SaveToFile(const FileName: string);
var
  F: string;
  L, P: TLyFileEditLock;
begin
  // check current file
  F := ExpandFileName(FileName);
  if SameFileName(F, FFileName) then
  begin
    FMemo.Lines.SaveToFile(FFileName);
    Exit;
  end;

  // lock target file
  L := TLyFileEditLock.Create;
  if not L.LockForFile(F) then
  begin
    ErrorLocked(FileName);
    L.free;
    Exit;
  end;

  try
    FMemo.Lines.SaveToFile(FileName);
    L.WriteEditInfo(FEditInfo);
    P := FFileLock;
    FFileLock := L;
    L := nil;
    P.Free;
    FMemo.Modified := false;
    FFileName := F;
    SetCaption;
    ResetHilighter;
  except
    FreeAndNil(L);
    MsgErr(ExceptionStr);
  end;
end;

procedure TLyFilePage.Save;
begin
  if FFileLock.Locked then
    FMemo.Lines.SaveToFile(FFileName);
end;

procedure TLyFilePage.ErrorLocked(const FileName: string);
begin
  MsgErrFmt(PAD_EOPEN, [FileName]);
end;

procedure TLyFilePage.ResetHilighter;
begin
//if FMemo.Hilighter.HilightClass = TLyHilighter then
    FMemo.Hilighter.HilightClass := FindFileHilight(FFileName);
end;

procedure TLyFilePage.SetCaption;
begin
  if FFileName <> '' then
    Caption := '  ' + ChangeFileExt(ExtractFileName(FFileName), '  ') else
    Caption := '  Untitled' + IntToStr(FEditInfo.ei_file) + '  ';
end;

end.

