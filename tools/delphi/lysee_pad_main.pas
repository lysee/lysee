unit lysee_pad_main;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms, Vcl.Controls,
  Vcl.Menus, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.ImgList, Vcl.StdActns, Vcl.ActnList, Vcl.ToolWin, System.ImageList,
  System.Actions, Winapi.Windows, Messages, Lysee, CodeMemo;

const

  LYSEEPAD_TITLE = 'DyseePAD';
  LYSEEPAD_SAVEPROMPT = 'Save changes to file?';

type

  { TMainForm }

  TMainForm = class(TForm)
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ToolBar: TToolBar;
    NewButton: TToolButton;
    OpenButton: TToolButton;
    SaveButton: TToolButton;
    ToolButton3: TToolButton;
    CutButton: TToolButton;
    CopyButton: TToolButton;
    PasteButton: TToolButton;
    ActionList: TActionList;
    FileNew: TAction;
    FileOpen: TAction;
    FileSave: TAction;
    FileSaveAs: TAction;
    FileExit: TAction;
    EditCut: TEditCut;
    EditCopy: TEditCopy;
    EditPaste: TEditPaste;
    HelpAbout: TAction;
    StatusBar: TStatusBar;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    mmFile: TMenuItem;
    FileNewMenu: TMenuItem;
    FileOpenMenu: TMenuItem;
    FileSaveMenu: TMenuItem;
    FileSaveAsMenu: TMenuItem;
    N1: TMenuItem;
    FileExitMenu: TMenuItem;
    mmEdit: TMenuItem;
    CutItem: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    mmHelp: TMenuItem;
    HelpAboutItem: TMenuItem;
    pnEdit: TPanel;
    EditUndo: TAction;
    EditRedo: TAction;
    EditSelectAll: TAction;
    EditUndoMenu: TMenuItem;
    N2: TMenuItem;
    EditRedoMenu: TMenuItem;
    N3: TMenuItem;
    EditSelectAllMenu: TMenuItem;
    ToolButton1: TToolButton;
    UndoButton: TToolButton;
    RedoButton: TToolButton;
    SearchFind: TAction;
    SearchReplace: TAction;
    SearchMenu: TMenuItem;
    SearchFindMenu: TMenuItem;
    SearchReplaceMenu: TMenuItem;
    FindDialog: TFindDialog;
    ReplaceDialog: TReplaceDialog;
    LyseeSyntaxCheck: TAction;
    LyseeRun: TAction;
    LyseeMenu: TMenuItem;
    LyseeRunMenu: TMenuItem;
    LyseeSyntaxcheckMenu: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileOpenExecute(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FileSaveAsExcute(Sender: TObject);
    procedure FileExitExecute(Sender: TObject);
    procedure EditUndoExecute(Sender: TObject);
    procedure EditRedoExecute(Sender: TObject);
    procedure EditCutExecute(Sender: TObject);
    procedure EditCopyExecute(Sender: TObject);
    procedure EditPasteExecute(Sender: TObject);
    procedure EditSelectAllExecute(Sender: TObject);
    procedure SearchFindExecute(Sender: TObject);
    procedure SearchReplaceExecute(Sender: TObject);
    procedure LyseeRunExecute(Sender: TObject);
    procedure LyseeSyntaxCheckExecute(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure ReplaceDialogFind(Sender: TObject);
    procedure ReplaceDialogReplace(Sender: TObject);
  private
    FMemo: TLyCodeMemo;
    FFileName: string;
    FNextMonitor: HWND;
    FLysee: string;
    function Modified: boolean;
    procedure DrawClipboard(var Msg: TMessage);message WM_DRAWCLIPBOARD;
    procedure MemoStatus(Sender: TObject);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure SetCaption;
    procedure CloseDialogs;
    function PromptSaveChange: boolean;
    function SaveChange: boolean;
    function Yes(const Question: string): boolean;
    function YesNoCancel(const Question: string): cardinal;
  end;

var
  MainForm: TMainForm;

implementation

uses
  Clipbrd, ShellAPI, uidefs, basic;

{$R *.dfm}

procedure TMainForm.FileNewExecute(Sender: TObject);
begin
  CloseDialogs;
  if PromptSaveChange then
  begin
    FMemo.Readonly := true;
    try
      FMemo.Lines.Clear;
      FMemo.Hilighter.HilightClass := nil;
      FMemo.Modified := false;
      FFileName := '';
      SetCaption;
      MemoStatus(nil);
    finally
      FMemo.Readonly := false;
    end;
  end;
end;

procedure TMainForm.FileOpenExecute(Sender: TObject);
begin
  CloseDialogs;
  if PromptSaveChange then
    if OpenDialog.Execute then
      LoadFromFile(OpenDialog.FileName);
end;

procedure TMainForm.FileSaveExecute(Sender: TObject);
begin
  if FFileName <> '' then
  begin
    FMemo.Lines.SaveToFile(FFileName);
    FMemo.Modified := false;
  end
  else
  begin
    CloseDialogs;
    if SaveDialog.Execute then
      SaveToFile(SaveDialog.FileName);
  end;
end;

procedure TMainForm.FindDialogFind(Sender: TObject);
begin
  ReplaceDialog.FindText := FindDialog.FindText;
  if not FMemo.FindNext(StrToWide(FindDialog.FindText)) then
    ShowMessage('text not found!');
end;

procedure TMainForm.FileSaveAsExcute(Sender: TObject);
begin
  CloseDialogs;
  if SaveDialog.Execute then
    SaveToFile(SaveDialog.FileName);
end;

procedure TMainForm.FormActivate(Sender: TObject);
var
  F: string;
begin
  OnActivate := nil;
  MemoStatus(nil);
  F := Trim(ParamStr(1));
  if FileExists(F) then
    LoadFromFile(F);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  K: cardinal;
begin
  if Modified then
  begin
    K := YesNoCancel(LYSEEPAD_SAVEPROMPT);
    if K = IDYES then
      CanClose := SaveChange else
      CanClose := (K = IDNO);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.Title := LYSEEPAD_TITLE;
  FFileName := '';
  SetCaption;

  FMemo := PlaceACodeMemo(pnEdit);
  FMemo.Modified := false;
  FMemo.OnStatus := MemoStatus;
  FMemo.ShowLine80 := true;

  FNextMonitor := SetClipBoardViewer(Handle);
  FLysee := ExtractFilePath(Application.ExeName) + 'lysee.exe';
  if not FileExists(FLysee) then
    FLysee := ExtractFileName(FLysee);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ChangeClipboardChain(Handle, FNextMonitor);
  SendMessage(FNextMonitor, WM_CHANGECBCHAIN, Handle, FNextMonitor);
end;

procedure TMainForm.CloseDialogs;
begin
  FindDialog.CloseDialog;
  ReplaceDialog.CloseDialog;
end;

procedure TMainForm.DrawClipboard(var Msg: TMessage);
begin
  SendMessage(FNextMonitor, Msg.Msg, Msg.WParam, Msg.LParam);
  EditPaste.Enabled := HasTextFormat;
end;

procedure TMainForm.EditRedoExecute(Sender: TObject);
begin
  FMemo.Redo;
end;

procedure TMainForm.EditSelectAllExecute(Sender: TObject);
begin
  FMemo.SelectAll;
end;

procedure TMainForm.EditUndoExecute(Sender: TObject);
begin
  FMemo.Undo;
end;

procedure TMainForm.EditCopyExecute(Sender: TObject);
begin
  FMemo.CopyToClipboard;
end;

procedure TMainForm.EditPasteExecute(Sender: TObject);
begin
  FMemo.PasteFromClipboard;
end;

procedure TMainForm.FileExitExecute(Sender: TObject);
begin
  CloseDialogs;
  if PromptSaveChange then
  begin
    FMemo.Readonly := true;
    OnCloseQuery := nil;
    Close;
  end;
end;

procedure TMainForm.HelpAboutExecute(Sender: TObject);
begin
//AboutBox.ShowModal;
end;

procedure TMainForm.LoadFromFile(const FileName: string);
var
  X: string;
begin
  try
    X := LowerCase(ExtractFileExt(FileName));
    FMemo.Readonly := true;
    FFileName := '';
    FMemo.Hilighter.HilightClass := FindFileHilight(FileName);
    FMemo.Lines.LoadFromFile(FileName);
    FMemo.SetFocus;
    FFileName := ExpandFileName(Trim(FileName));
  finally
    FMemo.Readonly := false;
    SetCaption;
  end;
end;

function TMainForm.PromptSaveChange: boolean;
begin
  Result := not Modified;
  if not Result then
    if Yes(LYSEEPAD_SAVEPROMPT) then
      Result := SaveChange else
      Result := true;
end;

procedure TMainForm.ReplaceDialogFind(Sender: TObject);
begin
  FindDialog.FindText := ReplaceDialog.FindText;
  if not FMemo.FindNext(StrToWide(FindDialog.FindText)) then
    ShowMessage('text not found!');
end;

procedure TMainForm.ReplaceDialogReplace(Sender: TObject);
begin
  if FMemo.SelAvail then
    if SameText(WideToStr(FMemo.SelText), ReplaceDialog.FindText) then
      FMemo.SelText := StrToWide(ReplaceDialog.ReplaceText);
  if frReplaceAll in ReplaceDialog.Options then
  begin
    while FMemo.FindNext(StrToWide(ReplaceDialog.FindText)) do
      FMemo.SelText := StrToWide(ReplaceDialog.ReplaceText);
  end
  else ReplaceDialogFind(nil);
end;

procedure TMainForm.LyseeRunExecute(Sender: TObject);

  function Quote(const S: string): string;
  begin
    if Pos(' ', S) > 0 then
      Result := '"' + S + '"' else
      Result := S;
  end;

var
  F: string;
  X: integer;
begin
  if PromptSaveChange then
  begin
    if FFileName = '' then
    begin
      F := ExtractFilePath(Application.ExeName) + 'temp.ls';
      FMemo.Lines.SaveToFile(F);
      F := Quote(F);
    end
    else F := Quote(FFileName);
    F := '--pause ' + F;
    X := ShellExecute(Handle, 'Open', PChar(FLysee), PChar(F), nil, SW_NORMAL);
    if X < 32 then
      ShowMessage(Format('Failed executing lysee with error code: %d', [X]));
  end;
end;

procedure TMainForm.LyseeSyntaxCheckExecute(Sender: TObject);
var
  X: TLysee;
  E: TLyError;
begin
  X := TLysee.Create(Self);
  try
    if not X.MainThread.SyntaxCheck(FMemo.Lines.Text) then
    begin
      E := X.MainThread.Error;
//      FEdit.Caret.MoveTo(E.Row, E.Col + 1);
//      FEdit.Caret.MakeVisible;
      ShowMessage(Format('%s(row=%d col=%d) %s',
        [E.Errno, E.Row + 1, E.Col + 1, E.Msg]));
    end;
  finally
    X.Free;
  end;
end;

procedure TMainForm.MemoStatus(Sender: TObject);
begin
  StatusBar.Panels[0].Text := Format('%d, %d',
    [FMemo.CaretY + 1, FMemo.CaretX]);
  if FMemo.Modified then
    StatusBar.Panels[1].Text := 'Modified' else
    StatusBar.Panels[1].Text := '';
  StatusBar.Panels[2].Text := 'Rivi'; //FMemo.Syntax.Language;
  EditUndo.Enabled := FMemo.CanUndo;
  EditRedo.Enabled := FMemo.CanRedo;
  EditCut.Enabled := FMemo.SelAvail;
  EditCopy.Enabled := FMemo.SelAvail;
  EditPaste.Enabled := HasTextFormat;
  EditSelectAll.Enabled := (FMemo.Lines.Count > 1) or (FMemo.Lines.First.Text <> '');
  LyseeMenu.Visible := false; // SameText(FMemo.Syntax.Language, 'Lysee');
  LyseeSyntaxCheck.Enabled := false;  // LyseeMenu.Visible;
  LyseeRun.Enabled := false; // LyseeMenu.Visible;
end;

function TMainForm.Modified: boolean;
begin
  Result := FMemo.Modified;
end;

function TMainForm.SaveChange: boolean;
begin
  if Modified then
    FileSaveExecute(nil);
  Result := not Modified;
end;

procedure TMainForm.SaveToFile(const FileName: string);
begin
  try
    FMemo.Lines.SaveToFile(FileName);
    FMemo.Modified := false;
    FMemo.Hilighter.HilightClass := FindFileHilight(FileName);
    FFileName := ExpandFileName(Trim(FileName));
  finally
    SetCaption;
  end;
end;

procedure TMainForm.SearchFindExecute(Sender: TObject);
begin
  CloseDialogs;
  FindDialog.Execute;
end;

procedure TMainForm.SearchReplaceExecute(Sender: TObject);
begin
  CloseDialogs;
  FMemo.UnSelect;
  ReplaceDialog.Execute;
end;

procedure TMainForm.SetCaption;
begin
  if FFileName <> '' then
    Caption := LYSEEPAD_TITLE + ' - ' + ExtractFileName(FFileName) else
    Caption := LYSEEPAD_TITLE + ' - Untitled';
end;

procedure TMainForm.EditCutExecute(Sender: TObject);
begin
  FMemo.CutToClipboard;
end;

function TMainForm.Yes(const Question: string): boolean;
begin
  Result := (MessageBox(Application.Handle, pchar(Question),
    LYSEEPAD_TITLE, MB_YESNO + MB_ICONQUESTION) = IDYES);
end;

function TMainForm.YesNoCancel(const Question: string): cardinal;
begin
  Result := MessageBox(Application.Handle, pchar(Question),
    LYSEEPAD_TITLE, MB_YESNOCANCEL + MB_ICONQUESTION);
end;

end.
