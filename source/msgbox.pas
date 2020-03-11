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

function ModalResultStr(ID: integer; const DefStr: string): string;
function IDButtonStr(ID: integer; const DefStr: string): string;

function YesNo(const Question: string): string;overload;
function YesNo(const Fmt: string; const Args: array of const): string;overload;

function YesNoCancel(const Question: string): string;overload;
function YesNoCancel(const Fmt: string; const Args: array of const): string;overload;

function OkCancel(const Question: string): string;overload;
function OkCancel(const Fmt: string; const Args: array of const): string;overload;

function RetryCancel(const Question: string): string;overload;
function RetryCancel(const Fmt: string; const Args: array of const): string;overload;

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
  if Title = '' then T := OkTitle else T := Title;
  {$IFDEF FPC}
  Result := MessageDlg(T, Msg, mtInformation, [mbOk], 0);
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
  Result := MessageDlg(ErrorTitle, Msg, mtError, [mbOk], 0);
  {$ELSE}
  Result := Modal(Msg, ErrorTitle, MB_ICONERROR + MB_OK);
  {$ENDIF}
end;

function MsgErrFmt(const Fmt: string; const Args: array of const): integer;
begin
  Result := MsgErr(Format(Fmt, Args));
end;

function ModalResultStr(ID: integer; const DefStr: string): string;
begin
  if ID = mrOk then Result := 'ok' else
  if ID = mrRetry then Result := 'retry' else
  if ID = mrYes then Result := 'yes' else
  if ID = mrNo then Result := 'no' else
  if ID = mrCancel then Result := 'cancel' else
    Result := DefStr;
end;

function IDButtonStr(ID: integer; const DefStr: string): string;
begin
  if ID = IDOK then Result := 'ok' else
  if ID = IDRETRY then Result := 'retry' else
  if ID = IDYES then Result := 'yes' else
  if ID = IDNO then Result := 'no' else
  if ID = IDCANCEL then Result := 'cancel' else
    Result := DefStr;
end;

function YesNo(const Question: string): string;
begin
  {$IFDEF FPC}
  Result := ModalResultStr(MessageDlg(AnsTitle, Question,
    mtConfirmation, [mbYes, mbNo], 0), 'no');
  {$ELSE}
  Result := IDButtonStr(Modal(Question, AnsTitle,
    MB_YESNO + MB_ICONQUESTION), 'no');
  {$ENDIF}
end;

function YesNo(const Fmt: string; const Args: array of const): string;
begin
  Result := YesNo(Format(Fmt, Args));
end;

function YesNoCancel(const Question: string): string;
begin
  {$IFDEF FPC}
  Result := ModalResultStr(MessageDlg(AnsTitle, Question,
    mtConfirmation, [mbYes, mbNo, mbCancel], 0), 'cancel');
  {$ELSE}
  Result := IDButtonStr(Modal(Question, AnsTitle,
    MB_YESNOCANCEL + MB_ICONQUESTION), 'cancel');
  {$ENDIF}
end;

function YesNoCancel(const Fmt: string; const Args: array of const): string;
begin
  Result := YesNoCancel(Format(Fmt, Args));
end;

function OkCancel(const Question: string): string;
begin
  {$IFDEF FPC}
  Result := ModalResultStr(MessageDlg(AnsTitle, Question,
    mtConfirmation, [mbOk, mbCancel], 0), 'cancel');
  {$ELSE}
  Result := IDButtonStr(Modal(Question, AnsTitle,
    MB_OKCANCEL + MB_ICONQUESTION), 'cancel');
  {$ENDIF}
end;

function OkCancel(const Fmt: string; const Args: array of const): string;
begin
  Result := OkCancel(Format(Fmt, Args));
end;

function RetryCancel(const Question: string): string;
begin
  {$IFDEF FPC}
  Result := ModalResultStr(MessageDlg(AnsTitle, Question,
    mtConfirmation, [mbRetry, mbCancel], 0), 'cancel');
  {$ELSE}
  Result := IDButtonStr(Modal(Question, AnsTitle,
    MB_RETRYCANCEL + MB_ICONQUESTION), 'cancel');
  {$ENDIF}
end;

function RetryCancel(const Fmt: string; const Args: array of const): string;
begin
  Result := RetryCancel(Format(Fmt, Args));
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
