{==============================================================================}
{        UNIT: lse_msgbox                                                      }
{ DESCRIPTION: show message box                                                }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2008/04/05                                                      }
{    MODIFIED: 2016/11/17                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit msgbox;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows, Messages,{$ENDIF}
  SysUtils, Classes, Forms, Dialogs, Controls;

function Modal(const Msg, Title: string; Flags: Integer): integer;
function AnsYes(const Question: string): boolean;
function AnsYesFmt(const Fmt: string; const Args: array of const): boolean;
function MsgOK(const Msg, Title: string): integer;
function MsgSys(const Msg: string): integer;
function MsgSysFmt(const Fmt: string; const Args: array of const): integer;
function MsgErr(const Msg: string): integer;
function MsgErrFmt(const Fmt: string; const Args: array of const): integer;

function ModalShow(AFormClass: TFormClass): integer;
procedure CenterScreen(AForm: TForm);
{$IFDEF MSWINDOWS}
procedure SetTopmost(Form: TCustomForm; Topmost: boolean);
procedure TabNext(AForm: TForm);
{$ENDIF}

procedure LockCursor;
procedure UnlockCursor;

var
  my_confirm_title: string = '';
  my_ok_title: string = '';
  my_sys_title: string = '';
  my_error_title: string = '';

implementation

var
  lock_count: integer;

function AnsTitle: string;
begin
  if my_confirm_title = '' then
    Result := 'Confirmation' else
    Result := my_confirm_title;
end;

function OkTitle: string;
begin
  if my_ok_title = '' then
    Result := 'Information' else
    Result := my_ok_title;
end;

function SysTitle: string;
begin
  if my_sys_title = '' then
    Result := OkTitle else
    Result := my_sys_title;
end;

function ErrorTitle: string;
begin
  if my_error_title = '' then
    Result := 'Error' else
    Result := my_error_title;
end;

function Modal(const Msg, Title: string; Flags: Integer): integer;
begin
  {$IFDEF FPC}
  Result := MessageBox(Application.MainFormHandle, pchar(Msg), pchar(Title), Flags);
  {$ELSE}
  Result := MessageBox(Application.Handle, pchar(Msg), pchar(Title), Flags);
  {$ENDIF}
end;

function AnsYes(const Question: string): boolean;
begin
  {$IFDEF FPC}
  Result := (mrYes = MessageDlg(AnsTitle, Question, mtConfirmation, [mbYes, mbNo], 0));
  {$ELSE}
  Result := Modal(Question, AnsTitle, MB_YESNO + MB_ICONQUESTION) = IDYES;
  {$ENDIF}
end;

function AnsYesFmt(const Fmt: string; const Args: array of const): boolean;
begin
  Result := AnsYes(Format(Fmt, Args));
end;

function MsgOK(const Msg, Title: string): integer;
var
  T: string;
begin
  if Title = '' then
    T := OkTitle else
    T := Title;
  {$IFDEF FPC}
  Result := MessageDlg(T, Msg, mtInformation, [mbYes], 0);
  {$ELSE}
  Result := Modal(Msg, T, MB_OK + MB_ICONINFORMATION);
  {$ENDIF}
end;

function MsgSys(const Msg: string): integer;
begin
  Result := MsgOK(Msg, SysTitle);
end;

function MsgSysFmt(const Fmt: string; const Args: array of const): integer;
begin
  Result := MsgSys(Format(Fmt, Args));
end;

function MsgErr(const Msg: string): integer;
begin
  {$IFDEF FPC}
  Result := MessageDlg(ErrorTitle, Msg, mtError, [mbYes], 0);
  {$ELSE}
  Result := Modal(Msg, ErrorTitle, MB_ICONERROR + MB_OK);
  {$ENDIF}
end;

function MsgErrFmt(const Fmt: string; const Args: array of const): integer;
begin
  Result := MsgErr(Format(Fmt, Args));
end;

function ModalShow(AFormClass: TFormClass): integer;
begin
  with AFormClass.Create(Application) do
  try
    Result := ShowModal;
  finally
    Release;
  end;
end;

procedure CenterScreen(AForm: TForm);
var
  w, h: integer;
begin
  w := AForm.Width;
  h := AForm.Height;
  AForm.SetBounds((Screen.Width - w) div 2, (Screen.Height - h) div 2, w, h);
end;

{$IFDEF MSWINDOWS}
procedure SetTopmost(Form: TCustomForm; Topmost: boolean);
const
  M: array[boolean] of HWND = (HWND_NOTOPMOST, HWND_TOPMOST);
begin
  SetWindowPos(Form.Handle, M[Topmost], 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);
end;

procedure TabNext(AForm: TForm);
begin
  SendMessage(AForm.Handle, WM_NEXTDLGCTL, 0, 0);
end;
{$ENDIF}

procedure LockCursor;
begin
  Screen.Cursor := crHourglass;
  inc(lock_count);
end;

procedure UnlockCursor;
begin
  if lock_count > 0 then dec(lock_count);
  if lock_count = 0 then
    Screen.Cursor := crDefault;
end;

end.
