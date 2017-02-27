unit lysee_pad_main;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, ComCtrls, Clipbrd, ActnList, StdActns, codeedit,
  Messages, lysee;

const

  LYSEEPAD_TITLE   = 'LyseePAD';
  LYSEE_SAVEPROMPT = 'Save changes to file?';

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionList: TActionList;
    EditCopy: TEditCopy;
    EditCut: TEditCut;
    EditPaste: TEditPaste;
    EditRedo: TAction;
    EditSelectAll: TAction;
    EditUndo: TAction;
    FileExit: TAction;
    FileNew: TAction;
    FileOpen: TAction;
    FileSave: TAction;
    FileSaveAs: TAction;
    FindDialog: TFindDialog;
    HelpAbout: TAction;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    FileNewMenu: TMenuItem;
    FileOpenMenu: TMenuItem;
    FileSaveMenu: TMenuItem;
    MenuItem1: TMenuItem;
    FileExitMenu: TMenuItem;
    EditPanel: TPanel;
    EditMenu: TMenuItem;
    EditCutMenu: TMenuItem;
    EditCopyMenu: TMenuItem;
    EditPasteMenu: TMenuItem;
    MenuItem2: TMenuItem;
    EditSelectAllMenu: TMenuItem;
    EditUndoMenu: TMenuItem;
    EditRedoMenu: TMenuItem;
    MenuItem3: TMenuItem;
    FileSaveAsMenu: TMenuItem;
    LyseeMenu: TMenuItem;
    LyseeRunMenu: TMenuItem;
    LyseeSyntaxCheckMenu: TMenuItem;
    HelpMenu: TMenuItem;
    HelpAboutMenu: TMenuItem;
    ReplaceDialog: TReplaceDialog;
    LyseeRun: TAction;
    LyseeSyntaxCheck: TAction;
    SearchFind: TAction;
    SearchReplace: TAction;
    SearchReplaceMenu: TMenuItem;
    SearchFindMenu: TMenuItem;
    SearchMenu: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    StatusBar: TStatusBar;
    ToolBar1: TToolBar;
    NewButton: TToolButton;
    OpenButton: TToolButton;
    SaveButton: TToolButton;
    ToolButton1: TToolButton;
    CutButton: TToolButton;
    CopyButton: TToolButton;
    PasteButton: TToolButton;
    ToolButton2: TToolButton;
    RedoButton: TToolButton;
    FindButton: TToolButton;
    UndoButton: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileOpenExecute(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FileSaveAsExecute(Sender: TObject);
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
    FEdit: TCodeEdit;
    FFileName: string;
    FLysee: string;
    FNextMonitor: HWND;
    procedure DrawClipboard(var Msg: TMessage);message WM_DRAWCLIPBOARD;
    procedure EditStatus(Sender: TObject);
    procedure CloseDialogs;
    procedure SetCaption;
    procedure Terminate(Sender: TObject);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    function PromptSaveChange: boolean;
    function SaveChange: boolean;
    function Yes(const Question: string): boolean;
    function YesNoCancel(const Question: string): cardinal;
  end;

var
  MainForm: TMainForm;

implementation

uses
  Process, lysee_pad_about;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.Title := LYSEEPAD_TITLE;
  FFileName := '';
  SetCaption;
  FEdit := PlaceACodeEdit(EditPanel);
  FEdit.Syntax.SyntaxClass := TLyseeSyntax;
  FEdit.OnStatus := @EditStatus;
  FNextMonitor := SetClipBoardViewer(Handle);
  FLysee := ExtractFilePath(Application.ExeName) +
    {$IFDEF MSWINDOWS}'lysee.exe'{$ELSE}'lysee'{$ENDIF};
  if not FileExists(FLysee) then
    FLysee := ExtractFileName(FLysee);
end;

procedure TMainForm.FormActivate(Sender: TObject);
var
  F: string;
begin
  OnActivate := nil;
  EditStatus(nil);
  F := Trim(ParamStr(1));
  if F <> '' then
    LoadFromFile(F);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  K: cardinal;
begin
  if FEdit.Modified then
  begin
    K := YesNoCancel(LYSEE_SAVEPROMPT);
    if K = mrYes then
      CanClose := SaveChange else
      CanClose := (K = mrNo);
  end;
end;

procedure TMainForm.FileSaveAsExecute(Sender: TObject);
begin
  CloseDialogs;
  if SaveDialog.Execute then
    SaveToFile(SaveDialog.FileName);
end;

procedure TMainForm.FileNewExecute(Sender: TObject);
begin
  CloseDialogs;
  if PromptSaveChange then
  begin
    FEdit.Readonly := true;
    try
      FEdit.Lines.ClearToOneEmptyLine;
      FEdit.Syntax.SyntaxClass := TLyseeSyntax;
      FEdit.Modified := false;
      FFileName := '';
      SetCaption;
      EditStatus(nil);
    finally
      FEdit.Readonly := false;
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
    FEdit.Lines.SaveToFile(FFileName);
    FEdit.Modified := false;
  end
  else
  begin
    CloseDialogs;
    if SaveDialog.Execute then
      SaveToFile(SaveDialog.FileName);
  end;
end;

procedure TMainForm.FileExitExecute(Sender: TObject);
begin
  CloseDialogs;
  if PromptSaveChange then
  begin
    FEdit.Modified := false;
    OnCloseQuery := nil;
    Close;
  end;
end;

procedure TMainForm.EditUndoExecute(Sender: TObject);
begin
  FEdit.Undos.Apply;
end;

procedure TMainForm.EditRedoExecute(Sender: TObject);
begin
  FEdit.Redos.Apply;
end;

procedure TMainForm.EditCutExecute(Sender: TObject);
begin
  FEdit.Selection.CutToClipboard;
end;

procedure TMainForm.EditCopyExecute(Sender: TObject);
begin
  FEdit.Selection.CopyToClipboard;
end;

procedure TMainForm.EditPasteExecute(Sender: TObject);
begin
  FEdit.Selection.PasteFromClipboard;
end;

procedure TMainForm.EditSelectAllExecute(Sender: TObject);
begin
  FEdit.Selection.SelectAll;
end;

procedure TMainForm.HelpAboutExecute(Sender: TObject);
begin
  with TAboutForm.Create(Application) do
  try
    ShowModal;
  finally
    Release;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ChangeClipboardChain(Handle, FNextMonitor);
  SendMessage(FNextMonitor, WM_CHANGECBCHAIN, Handle, FNextMonitor);
end;

procedure TMainForm.LyseeRunExecute(Sender: TObject);
var
  F: string;
  P: TProcess;
begin
  if PromptSaveChange then
  begin
    P := TProcess.Create(nil);
    try
      if FFileName = '' then
      begin
        F := ExtractFilePath(Application.ExeName) + 'temp.ls';
        FEdit.Lines.SaveToFile(F);
      end
      else F := FFileName;
      P.Executable := FLysee;
      P.Parameters.Add('--pause');
      P.Parameters.Add(F);
      P.Execute;
    finally
      P.Free;
    end;
  end;
end;

procedure TMainForm.LyseeSyntaxCheckExecute(Sender: TObject);
var
  X: TLysee;
begin
  X := TLysee.Create(Self);
  try
    X.OnExecuting := @Terminate;
    if not X.Execute(FEdit.Lines.Text) then
    begin
      FEdit.Caret.MoveTo(X.Error.ERow, X.Error.ECol + 1);
      FEdit.Caret.MakeVisible;
      ShowMessage(X.Error.ErrorText);
    end;
  finally
    X.Free;
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
  FEdit.Selection.Unselect;
  ReplaceDialog.Execute;
end;

procedure TMainForm.FindDialogFind(Sender: TObject);
begin
  ReplaceDialog.FindText := FindDialog.FindText;
  if not FEdit.FindNext(StrToCode(FindDialog.FindText)) then
    ShowMessage('text not found!');
end;

procedure TMainForm.ReplaceDialogFind(Sender: TObject);
begin
  FindDialog.FindText := ReplaceDialog.FindText;
  if not FEdit.FindNext(StrToCode(ReplaceDialog.FindText)) then
    ShowMessage('text not found!');
end;

procedure TMainForm.ReplaceDialogReplace(Sender: TObject);
begin
  if FEdit.Selection.Selected then
    FEdit.Selection.Text := StrToCode(ReplaceDialog.ReplaceText);
  if frReplaceAll in ReplaceDialog.Options then
  begin
    while FEdit.FindNext(StrToCode(ReplaceDialog.FindText)) do
      FEdit.Selection.Text := StrToCode(ReplaceDialog.ReplaceText);
  end
  else ReplaceDialogFind(nil);
end;

procedure TMainForm.DrawClipboard(var Msg: TMessage);
begin
  SendMessage(FNextMonitor, Msg.Msg, Msg.WParam, Msg.LParam);
  EditPaste.Enabled := HasTextFormat;
end;

procedure TMainForm.EditStatus(Sender: TObject);
begin
  StatusBar.Panels[0].Text := Format('%d, %d',
    [FEdit.Caret.LineIndex + 1, FEdit.Caret.TextIndex]);
  if FEdit.Modified then
    StatusBar.Panels[1].Text := 'Modified' else
    StatusBar.Panels[1].Text := '';
  StatusBar.Panels[2].Text := FEdit.Syntax.Language;
  EditUndo.Enabled := (FEdit.Undos.Last <> nil);
  EditRedo.Enabled := (FEdit.Redos.Last <> nil);
  EditCut.Enabled := FEdit.Selection.Selected;
  EditCopy.Enabled := FEdit.Selection.Selected;
  EditPaste.Enabled := HasTextFormat;
  EditSelectAll.Enabled := (FEdit.Lines.Count > 1) or (FEdit.Lines.First.Text <> '');
  LyseeMenu.Visible := SameText(FEdit.Syntax.Language, 'Lysee');
  LyseeSyntaxCheck.Enabled := LyseeMenu.Visible;
  LyseeRun.Enabled := LyseeMenu.Visible;
end;

procedure TMainForm.CloseDialogs;
begin
  FindDialog.CloseDialog;
  ReplaceDialog.CloseDialog;
end;

procedure TMainForm.SetCaption;
begin
  if FFileName <> '' then
    Caption := LYSEEPAD_TITLE + ' - ' + ExtractFileName(FFileName) else
    Caption := LYSEEPAD_TITLE + ' - Untitled';
end;

procedure TMainForm.Terminate(Sender: TObject);
begin
  TLysee(Sender).Terminate;
end;

procedure TMainForm.LoadFromFile(const FileName: string);
begin
  try
    FEdit.Readonly := true;
    FFileName := '';
    FEdit.Lines.LoadFromFile(FileName);
    FFileName := ExpandFileName(Trim(FileName));
  finally
    FEdit.Readonly := false;
    SetCaption;
  end;
end;

procedure TMainForm.SaveToFile(const FileName: string);
begin
  try
    FEdit.Lines.SaveToFile(FileName);
    FEdit.Modified := false;
    FFileName := ExpandFileName(Trim(FileName));
  finally
    SetCaption;
  end;
end;

function TMainForm.PromptSaveChange: boolean;
begin
  Result := not FEdit.Modified;
  if not Result then
    if Yes(LYSEE_SAVEPROMPT) then
      Result := SaveChange else
      Result := true;
end;

function TMainForm.SaveChange: boolean;
begin
  if FEdit.Modified then
    if FFileName <> '' then
    begin
      FEdit.Lines.SaveToFile(FFileName);
      FEdit.Modified := false;
    end
    else
    if SaveDialog.Execute then
      SaveToFile(SaveDialog.FileName);
  Result := not FEdit.Modified;
end;

function TMainForm.Yes(const Question: string): boolean;
begin
  Result := (mrYes = MessageDlg(LYSEEPAD_TITLE, Question, mtConfirmation, [mbYes, mbNo], 0));
end;

function TMainForm.YesNoCancel(const Question: string): cardinal;
begin
  Result := MessageDlg(LYSEEPAD_TITLE, Question, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
end;

end.

