unit lysee_pad_main;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ELSE}LMessage,{$ENDIF}
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls,
  Graphics, Dialogs, Menus, ExtCtrls, ComCtrls, Clipbrd, ActnList, StdActns,
  Buttons, ExtDlgs, StdCtrls, Messages, CodeMemo, Basic, lysee, lysee_pad_sheet;

type

  TFindOption = (frReplace, frMatchCase, frWholeWord, frSelectedOnly);
  TFindOptions = set of TFindOption;

  { TMainForm }

  TMainForm = class(TForm)
    actRiviBackground: TAction;
    actRiviColor: TAction;
    actRiviTable: TAction;
    actRiviLink: TAction;
    actRiviImage: TAction;
    actRiviKaiti: TAction;
    actRiviHeiti: TAction;
    actRiviSubScript: TAction;
    actRiviSuperScript: TAction;
    actRiviUnderline: TAction;
    actRiviStrikeout: TAction;
    actRiviItalic: TAction;
    actRiviBold: TAction;
    actRiviRight: TAction;
    actRiviCenter: TAction;
    actRiviLeft: TAction;
    aTabClose: TAction;
    APP: TApplicationProperties;
    aSearchNext: TAction;
    Actions: TActionList;
    aEditCopy: TEditCopy;
    aEditCut: TEditCut;
    aEditPaste: TEditPaste;
    aEditRedo: TAction;
    aEditSelectAll: TAction;
    aEditUndo: TAction;
    aFileExit: TAction;
    aFileNew: TAction;
    aFileOpen: TAction;
    aFileSave: TAction;
    aFileSaveAs: TAction;
    aHelpAbout: TAction;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    btnLink: TSpeedButton;
    btnLeft: TSpeedButton;
    btnCenter: TSpeedButton;
    btnImage: TSpeedButton;
    btnTable: TSpeedButton;
    btnBackground: TSpeedButton;
    btnKaiti: TSpeedButton;
    btnHeiti: TSpeedButton;
    btnItalic: TSpeedButton;
    btnSuper: TSpeedButton;
    btnSub: TSpeedButton;
    btnUnderline: TSpeedButton;
    btnStrikeout: TSpeedButton;
    btnBold: TSpeedButton;
    btnRight: TSpeedButton;
    dlgColor: TColorDialog;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    mmFile: TMenuItem;
    miFileNew: TMenuItem;
    miFileOpen: TMenuItem;
    miFileSave: TMenuItem;
    miFileLine1: TMenuItem;
    miFileExit: TMenuItem;
    EditPanel: TPanel;
    mmEdit: TMenuItem;
    miEditCut: TMenuItem;
    miEditCopy: TMenuItem;
    miEditPaste: TMenuItem;
    miEditLine2: TMenuItem;
    miEditSelectAll: TMenuItem;
    miEditUndo: TMenuItem;
    miEditRedo: TMenuItem;
    miEditLine1: TMenuItem;
    miFileSaveAs: TMenuItem;
    mmLysee: TMenuItem;
    miLyseeRun: TMenuItem;
    miLyseeSyntaxCheck: TMenuItem;
    mmHelp: TMenuItem;
    miHelpAbout: TMenuItem;
    miSearchNext: TMenuItem;
    dlgImage: TOpenPictureDialog;
    Pages: TPageControl;
    pnlTabMan: TPanel;
    pnlAlign1: TPanel;
    pnlRiviBody: TPanel;
    pnlRiviBottomLine: TPanel;
    pnlRivi: TPanel;
    aLyseeRun: TAction;
    aLyseeSyntaxCheck: TAction;
    aSearchFind: TAction;
    aSearchReplace: TAction;
    miSearchReplace: TMenuItem;
    miSearchFind: TMenuItem;
    mmSearch: TMenuItem;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    btnTextColor: TSpeedButton;
    btnSelectTextColor: TSpeedButton;
    pnlBackground: TPanel;
    pnlFontName: TPanel;
    pnlTextColor1: TPanel;
    pnlAlign: TPanel;
    btnSelectBackground: TSpeedButton;
    btnFontName: TSpeedButton;
    btnSelectFontName: TSpeedButton;
    btnTabClose: TSpeedButton;
    StatusBar: TStatusBar;
    barFile: TToolBar;
    btnEditCopy: TToolButton;
    btnEditPaste: TToolButton;
    btnSearch: TToolButton;
    btnReplace: TToolButton;
    ToolButton1: TToolButton;
    ToolButton13: TToolButton;
    btnEditUndo: TToolButton;
    btnEditRedo: TToolButton;
    btnFileNew: TToolButton;
    btnFileOpen: TToolButton;
    btnFileSave: TToolButton;
    ToolButton8: TToolButton;
    btnEditCut: TToolButton;
    procedure actRiviBackgroundExecute(Sender: TObject);
    procedure actRiviBoldExecute(Sender: TObject);
    procedure actRiviCenterExecute(Sender: TObject);
    procedure actRiviColorExecute(Sender: TObject);
    procedure actRiviHeitiExecute(Sender: TObject);
    procedure actRiviImageExecute(Sender: TObject);
    procedure actRiviItalicExecute(Sender: TObject);
    procedure actRiviKaitiExecute(Sender: TObject);
    procedure actRiviLeftExecute(Sender: TObject);
    procedure actRiviLinkExecute(Sender: TObject);
    procedure actRiviRightExecute(Sender: TObject);
    procedure actRiviStrikeoutExecute(Sender: TObject);
    procedure actRiviSubScriptExecute(Sender: TObject);
    procedure actRiviSuperScriptExecute(Sender: TObject);
    procedure actRiviTableExecute(Sender: TObject);
    procedure actRiviUnderlineExecute(Sender: TObject);
    procedure btnBackgroundClick(Sender: TObject);
    procedure btnBoldClick(Sender: TObject);
    procedure btnCenterClick(Sender: TObject);
    procedure btnFontNameClick(Sender: TObject);
    procedure btnHeitiClick(Sender: TObject);
    procedure btnImageClick(Sender: TObject);
    procedure btnItalicClick(Sender: TObject);
    procedure btnKaitiClick(Sender: TObject);
    procedure btnLeftClick(Sender: TObject);
    procedure btnLinkClick(Sender: TObject);
    procedure btnRightClick(Sender: TObject);
    procedure btnSelectBackgroundClick(Sender: TObject);
    procedure btnSelectFontNameClick(Sender: TObject);
    procedure btnSelectTextColorClick(Sender: TObject);
    procedure btnStrikeoutClick(Sender: TObject);
    procedure btnSubClick(Sender: TObject);
    procedure btnSuperClick(Sender: TObject);
    procedure btnTabCloseClick(Sender: TObject);
    procedure btnTableClick(Sender: TObject);
    procedure btnTextColorClick(Sender: TObject);
    procedure btnUnderlineClick(Sender: TObject);
    procedure EditPanelResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDestroy(Sender: TObject);
    procedure aFileNewExecute(Sender: TObject);
    procedure aFileOpenExecute(Sender: TObject);
    procedure aFileSaveExecute(Sender: TObject);
    procedure aFileSaveAsExecute(Sender: TObject);
    procedure aFileExitExecute(Sender: TObject);
    procedure aEditUndoExecute(Sender: TObject);
    procedure aEditRedoExecute(Sender: TObject);
    procedure aEditCutExecute(Sender: TObject);
    procedure aEditCopyExecute(Sender: TObject);
    procedure aEditPasteExecute(Sender: TObject);
    procedure aEditSelectAllExecute(Sender: TObject);
    procedure aSearchFindExecute(Sender: TObject);
    procedure aSearchReplaceExecute(Sender: TObject);
    procedure aSearchNextExecute(Sender: TObject);
    procedure aLyseeRunExecute(Sender: TObject);
    procedure aLyseeSyntaxCheckExecute(Sender: TObject);
    procedure aTabCloseExecute(Sender: TObject);
    procedure aHelpAboutExecute(Sender: TObject);
    procedure APPDropFiles(Sender: TObject; const FileNames: array of string);
    procedure PagesChange(Sender: TObject);
    procedure pnlTabManResize(Sender: TObject);
  private
    FPath: string;
    FLysee: string;
    FNextMonitor: HWND;
    FErrorRow: integer;
    FFindText: string;
    FReplaceText: string;
    FOptions: TFindOptions;
    FTabHeight: integer;
    function GetPage(Index: integer): TLyFilePage;
    function GetCount: integer;
  protected
    procedure MsgDrawClipboard(var Msg: TMessage); message PAD_DRAWCLIPBOARD;
    procedure MsgShowFile(var Msg: TMessage); message PAD_SHOWFILE;
    procedure OnExecuting(Sender: TObject; AThread: TLyThread);
    procedure OnChange(Sender: TObject);
    procedure OnStatus(Sender: TObject);
    procedure OnHint(Sender: TObject);
    procedure OnDblClose(Sender: TObject);
    procedure GetFindReplaceOptions;
    procedure SetCaption;
    procedure SetButtons;
    { RIVI }
    procedure SetFontStyle(const BegTag, EndTag: string); overload;
    procedure SetFontStyle(const NewStyle: string); overload;
    procedure SetHorzAlign(const NewAlign: string); overload;
  public
    function NewPage: TLyFilePage;
    function GetEmptyPage: TLyFilePage;
    function FindEmptyPage: TLyFilePage;
    function FindPage(const FileName: string): TLyFilePage; overload;
    function FindPage(FileID: int64): TLyFilePage; overload;
    function SelectPage(APage: TLyFilePage): TLyFilePage;
    function ActivePage: TLyFilePage;
    function ActiveMemo: TLyCodeMemo;
    function LoadFromFile(const FileName: string; IsCmd: boolean): TLyFilePage;
    function QueryClose(APage: TLyFilePage): boolean;
    property PageCount: integer read GetCount;
    property Page[Index: integer]: TLyFilePage read GetPage;
  end;

var
  MainForm: TMainForm;

implementation

uses
  Process, MsgBox, UIDefs, lysee_pad_find, lysee_pad_fontname, lysee_pad_about;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.Title := PAD_TITLE;
  Application.ShowHint := true;
  Application.OnHint := @OnHint;
  AllowDropFiles := True;
  SetCaption;

  FPath := ExtractFilePath(Application.ExeName);
  FLysee := FPath + 'lysee' + ExtractFileExt(Application.ExeName);
  aLyseeRun.Enabled := (FLysee <> '');

  FNextMonitor := SetClipBoardViewer(Handle);
  SetButtons;

  if FileExists(ParamStr(1)) then
  begin
    if not LoadFromFile(ParamStr(1), True).Locked then
      Application.Terminate;
  end
  else NewPage;

  pnlTabMan.Color := clWindow;
end;

procedure TMainForm.actRiviLeftExecute(Sender: TObject);
begin
  SetHorzAlign('left');
end;

procedure TMainForm.actRiviLinkExecute(Sender: TObject);
var
  L: string;
begin
  L := Trim(InputBox('Insert link', 'Please input link address:', ''));
  if L <> '' then
    SetFontStyle('a=' + L, '/a');
end;

procedure TMainForm.actRiviRightExecute(Sender: TObject);
begin
  SetHorzAlign('right');
end;

procedure TMainForm.actRiviStrikeoutExecute(Sender: TObject);
begin
  SetFontStyle('s');
end;

procedure TMainForm.actRiviSubScriptExecute(Sender: TObject);
begin
  SetFontStyle('sub');
end;

procedure TMainForm.actRiviSuperScriptExecute(Sender: TObject);
begin
  SetFontStyle('sup');
end;

procedure TMainForm.actRiviTableExecute(Sender: TObject);

  function GridString(Rows, Cols: integer): string;
  begin
    Result := '[';
    while Cols > 2 do
    begin
      Result := Result + ' |';
      Dec(Cols);
    end;
    Result := Result + ' ]';
    while Rows > 1 do
    begin
      Result := Result + sLineBreak + Result;
      Dec(Rows);
    end;
  end;

begin
  ActiveMemo.SelText := StrToWide(sLineBreak + GridString(3, 3) + sLineBreak);
end;

procedure TMainForm.actRiviUnderlineExecute(Sender: TObject);
begin
end;

procedure TMainForm.btnBackgroundClick(Sender: TObject);
begin
  SetFontStyle('g=' + ColorToID(pnlBackground.Color), '/g');
end;

procedure TMainForm.btnBoldClick(Sender: TObject);
begin
  SetFontStyle('b');
end;

procedure TMainForm.btnCenterClick(Sender: TObject);
begin
  SetHorzAlign('center');
end;

procedure TMainForm.btnFontNameClick(Sender: TObject);
begin
  if ActiveMemo.SelAvail then
    SetFontStyle('f=' + DoubleQuote(btnFontName.Caption), '/f');
end;

procedure TMainForm.btnHeitiClick(Sender: TObject);
begin
  SetFontStyle('hei');
end;

procedure TMainForm.btnImageClick(Sender: TObject);
begin
  actRiviImageExecute(nil);
end;

procedure TMainForm.btnItalicClick(Sender: TObject);
begin
  SetFontStyle('i');
end;

procedure TMainForm.btnKaitiClick(Sender: TObject);
begin
  SetFontStyle('kai');
end;

procedure TMainForm.btnLeftClick(Sender: TObject);
begin
  SetHorzAlign('left');
end;

procedure TMainForm.btnLinkClick(Sender: TObject);
begin
  actRiviLinkExecute(nil);
end;

procedure TMainForm.btnRightClick(Sender: TObject);
begin
  SetHorzAlign('right');
end;

procedure TMainForm.btnSelectBackgroundClick(Sender: TObject);
begin
  if dlgColor.Execute then
  begin
    SetFontStyle('g=' + ColorToID(dlgColor.Color), '/g');
    pnlBackground.Color := dlgColor.Color;
    btnBackground.Font.Color := dlgColor.Color xor $00FFFFFF;
  end;
end;

procedure TMainForm.btnSelectFontNameClick(Sender: TObject);
var
  F: string;
begin
  F := SelectFontName(btnFontName.Caption);
  if F <> '' then
  begin
    btnFontName.Caption := F;
    btnFontName.Font.Name := F;
    btnFontNameClick(nil);
  end;
end;

procedure TMainForm.btnSelectTextColorClick(Sender: TObject);
begin
  if dlgColor.Execute then
  begin
    SetFontStyle('c=' + ColorToID(dlgColor.Color), '/c');
    btnTextColor.Font.Color := dlgColor.Color;
  end;
end;

procedure TMainForm.btnStrikeoutClick(Sender: TObject);
begin
  SetFontStyle('s');
end;

procedure TMainForm.btnSubClick(Sender: TObject);
begin
  SetFontStyle('sub');
end;

procedure TMainForm.btnSuperClick(Sender: TObject);
begin
  SetFontStyle('sup');
end;

procedure TMainForm.btnTabCloseClick(Sender: TObject);
var
  P: TLyFilePage;
begin
  P := ActivePage;
  if (P <> nil) and QueryClose(P) then
  begin
    P.Free;
    SetButtons;
  end;
end;

procedure TMainForm.btnTableClick(Sender: TObject);
begin
  actRiviTableExecute(nil);
end;

procedure TMainForm.btnTextColorClick(Sender: TObject);
begin
  SetFontStyle('c=' + ColorToID(btnTextColor.Font.Color), '/c');
end;

procedure TMainForm.btnUnderlineClick(Sender: TObject);
begin
  SetFontStyle('u');
end;

procedure TMainForm.EditPanelResize(Sender: TObject);
begin
  pnlTabMan.top := EditPanel.Top;
  pnlTabMan.Left := EditPanel.left + EditPanel.Width - pnlTabMan.Width;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  if ActiveMemo <> nil then
    ActiveMemo.SetFocus;
end;

procedure TMainForm.actRiviCenterExecute(Sender: TObject);
begin
  SetHorzAlign('center');
end;

procedure TMainForm.actRiviColorExecute(Sender: TObject);
begin
  if dlgColor.Execute then
    SetFontStyle('c=' + ColorToID(dlgColor.Color), '/c');
end;

procedure TMainForm.actRiviHeitiExecute(Sender: TObject);
begin
  SetFontStyle('hei');
end;

procedure TMainForm.actRiviImageExecute(Sender: TObject);
var
  S, P: string;
begin
  if dlgImage.Execute then
  begin
    P := GetRelativePath(dlgImage.FileName, ActivePage.FileName);
    S := Format('<image source=%s>', [DoubleQuote(SetUD(P))]);
    ActiveMemo.Lines.BeginUpdate;
    try
      ActiveMemo.SelText := StrToWide(S);
    finally
      ActiveMemo.Lines.EndUpdate;
    end;
  end;
end;

procedure TMainForm.actRiviItalicExecute(Sender: TObject);
begin
end;

procedure TMainForm.actRiviKaitiExecute(Sender: TObject);
begin
  SetFontStyle('kai');
end;

procedure TMainForm.actRiviBoldExecute(Sender: TObject);
begin
end;

procedure TMainForm.actRiviBackgroundExecute(Sender: TObject);
begin
  if dlgColor.Execute then
    SetFontStyle('g=' + ColorToID(dlgColor.Color), '/g');
end;

procedure TMainForm.aTabCloseExecute(Sender: TObject);
var
  P: TLyFilePage;
begin
  P := ActivePage;
  if (P <> nil) and QueryClose(P) then
  begin
    P.Free;
    SetButtons;
  end;
end;

procedure TMainForm.APPDropFiles(Sender: TObject; const FileNames: array of string);
var
  I: integer;
begin
  for I := 0 to Length(FileNames) - 1 do
    if FileExists(FileNames[I]) then
      LoadFromFile(FileNames[I], False);
end;

procedure TMainForm.PagesChange(Sender: TObject);
begin
  OnStatus(nil);
  SetButtons;
end;

procedure TMainForm.pnlTabManResize(Sender: TObject);
begin
  btnTabClose.Height := pnlTabMan.Height;
end;

function TMainForm.GetCount: integer;
begin
  Result := Pages.PageCount;
end;

function TMainForm.GetPage(Index: integer): TLyFilePage;
begin
  Result := TLyFilePage(Pages.Pages[Index]);
end;

procedure TMainForm.OnChange(Sender: TObject);
begin
  { nothing }
end;

procedure TMainForm.OnStatus(Sender: TObject);
var
  M: TLyCodeMemo;
begin
  M := ActiveMemo;
  if M <> nil then
  begin
    StatusBar.Panels[0].Text := Format('%d, %d', [M.CaretY + 1, M.CaretX]);
    if M.Modified then
      StatusBar.Panels[1].Text := 'Modified'
    else
      StatusBar.Panels[1].Text := '';
    StatusBar.Panels[2].Text := M.Hilighter.Language;
  end;
  SetButtons;
end;

procedure TMainForm.OnHint(Sender: TObject);
begin
  StatusBar.Panels[3].Text := GetLongHint(Application.Hint);
end;

procedure TMainForm.OnDblClose(Sender: TObject);
begin
  ShowMessage('');
end;

procedure TMainForm.aSearchNextExecute(Sender: TObject);
var
  S: string;
  K: TModalResult;
  M: TLyCodeMemo;
begin
  M := ActiveMemo;
  while M.FindNext(StrToWide(FFindText)) do
  begin
    if not (frReplace in FOptions) then
      Exit;
    S := Format('Replace current string "%s" with "%s"?',
      [FFindText, FReplaceText]);
    K := MessageDlg(PAD_TITLE, S, mtConfirmation,
      [mbYes, mbAll, mbNo, mbCancel], 0);
    if K <> mrNo then
      if K = mrYes then
        M.SelText := StrToWide(FReplaceText)
      else
      if K = mrAll then
      begin
        repeat
          M.SelText := StrToWide(FReplaceText);
        until not M.FindNext(StrToWide(FFindText), False);
        Exit;
      end
      else
        Exit;
  end;
  ShowMessage('text not found!');
end;

procedure TMainForm.GetFindReplaceOptions;
begin
  FFindText := FindForm.FindText;
  FReplaceText := FindForm.ReplaceText;
  if FindForm.Replace then
    FOptions := [frReplace]
  else
    FOptions := [];
  if FindForm.CaseSensitive then
    FOptions := FOptions + [frMatchCase];
  if FindForm.WholeWord then
    FOptions := FOptions + [frWholeWord];
  if FindForm.FindInSelected then
    FOptions := FOptions + [frSelectedOnly];
  SetButtons;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  I: integer;
begin
  for I := Pages.PageCount - 1 downto 0 do
    if not QueryClose(Page[I]) then
    begin
      CanClose := False;
      Exit;
    end
    else
      Page[I].Free;
  SetButtons;
end;

procedure TMainForm.aFileSaveAsExecute(Sender: TObject);
var
  P: TLyFilePage;
  F: string;
begin
  P := ActivePage;
  dlgSave.FileName := P.FileName;
  if dlgSave.Execute then
  begin
    F := dlgSave.FileName;
    if SameFileName(F, P.FileName) then
      P.Save
    else
    if FindPage(F) <> nil then
      MsgErr('File is beging edited')
    else
      P.SaveToFile(F);
  end;
end;

procedure TMainForm.aFileNewExecute(Sender: TObject);
begin
  NewPage;
end;

procedure TMainForm.aFileOpenExecute(Sender: TObject);
begin
  if dlgOpen.Execute then
    LoadFromFile(dlgOpen.FileName, False);
end;

procedure TMainForm.aFileSaveExecute(Sender: TObject);
var
  P: TLyFilePage;
begin
  P := ActivePage;
  if P.FileName <> '' then
    P.Save
  else
    aFileSaveAsExecute(nil);
end;

procedure TMainForm.aFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.aEditUndoExecute(Sender: TObject);
begin
  ActiveMemo.Undo;
end;

procedure TMainForm.aEditRedoExecute(Sender: TObject);
begin
  ActiveMemo.Redo;
end;

procedure TMainForm.aEditCutExecute(Sender: TObject);
begin
  ActiveMemo.CutToClipboard;
end;

procedure TMainForm.aEditCopyExecute(Sender: TObject);
begin
  ActiveMemo.CopyToClipboard;
end;

procedure TMainForm.aEditPasteExecute(Sender: TObject);
begin
  ActiveMemo.PasteFromClipboard;
end;

procedure TMainForm.aEditSelectAllExecute(Sender: TObject);
begin
  ActiveMemo.SelectAll;
end;

procedure TMainForm.aHelpAboutExecute(Sender: TObject);
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

procedure TMainForm.aLyseeRunExecute(Sender: TObject);

  function PromptSaveChange: boolean;
  begin
    Result := not ActiveMemo.Modified;
    if not Result then
      if YesNo(PAD_SAVEPROMPT) = 'yes' then
      begin
        aFileSaveExecute(nil);
        Result := not ActiveMemo.Modified;
      end
      else
        Result := True;
  end;

var
  F: string;
  P: TProcess;
  L: TStrings;
  S: TLyFilePage;
begin
  if (FLysee <> '') and PromptSaveChange then
  begin
    P := TProcess.Create(nil);
    try
      S := ActivePage;
      if S.FileName = '' then
      begin
        F := ExtractFilePath(Application.ExeName) + 'temp.ls';
        L := TStringList.Create;
        try
          L.Assign(S.Memo.Lines);
          L.SaveToFile(F);
        finally
          L.Free;
        end;
      end
      else
        F := S.FileName;
      P.Executable := FLysee;
      P.Parameters.Add('--pause');
      P.Parameters.Add(F);
      P.Execute;
    finally
      P.Free;
    end;
  end;
end;

procedure TMainForm.aLyseeSyntaxCheckExecute(Sender: TObject);
var
  X: TLysee;
  E: TLyError;
  P: TLyFilePage;
begin
  X := TLysee.Create(Self);
  try
    P := ActivePage;
    if P.FileName <> '' then
      X.MainModule.FileName := P.FileName
    else
      X.MainModule.FileName := FPath + 'temp.ls';
    X.OnExecuting := @OnExecuting;
    if not X.Execute(WideToStr(P.Memo.Lines.Text)) then
    begin
      E := X.MainThread.Error;
     {M.CaretY := E.Row + 1;
      M.CaretX := E.Col + 1;}
      MsgErrFmt(' %s[%d, %d] %s', [E.Errno,
        E.Row + 1, E.Col + 1, E.Msg]);
      FErrorRow := E.Row + 1;
    end;
  finally
    X.Free;
  end;
end;

procedure TMainForm.aSearchFindExecute(Sender: TObject);
begin
  if ModalFindReplace(True, ActiveMemo.SelAvail) then
  begin
    GetFindReplaceOptions;
    aSearchNextExecute(nil);
  end;
end;

procedure TMainForm.aSearchReplaceExecute(Sender: TObject);
begin
  if ModalFindReplace(False, ActiveMemo.SelAvail) then
  begin
    GetFindReplaceOptions;
    aSearchNextExecute(nil);
  end;
end;

procedure TMainForm.MsgDrawClipboard(var Msg: TMessage);
begin
  SendMessage(FNextMonitor, Msg.Msg, Msg.WParam, Msg.LParam);
  aEditPaste.Enabled := (ActiveMemo <> nil) and HasTextFormat;
end;

procedure TMainForm.MsgShowFile(var Msg: TMessage);
var
  P: TLyFilePage;
begin
  P := FindPage(Msg.lParam);
  if P <> nil then
  begin
    SelectPage(P);
    if WindowState = wsMinimized then
      Application.Restore;
    Application.BringToFront;
    SetForegroundWindow(Handle);
    BringToFront;
  end;
end;

procedure TMainForm.SetCaption;
begin
  Caption := PAD_TITLE;
end;

procedure TMainForm.SetButtons;
var
  M: TLyCodeMemo;
  F: boolean;
begin
  M := ActiveMemo;
  F := (M <> nil);

  Pages.Visible := F;
  aTabClose.Enabled := F;

  aFileSave.Enabled := F;
  aFileSaveAs.Enabled := F;

  aEditUndo.Enabled := F and M.CanUndo;
  aEditRedo.Enabled := F and M.CanRedo;
  aEditCut.Enabled := F and M.SelAvail;
  aEditCopy.Enabled := F and M.SelAvail;
  aEditPaste.Enabled := F and HasTextFormat;
  aEditSelectAll.Enabled := F;

  aSearchFind.Enabled := F;
  aSearchReplace.Enabled := F;
  aSearchNext.Enabled := F and (FFindText <> '');

  aLyseeSyntaxCheck.Enabled := F and MatchID('lysee', M.Hilighter.Language);
  aLyseeRun.Enabled := F and MatchID('lysee', M.Hilighter.Language);

  pnlRivi.Visible := F and MatchID('rivi', M.Hilighter.Language);

  pnlTabMan.Visible := F;
end;

procedure TMainForm.SetFontStyle(const BegTag, EndTag: string);
var
  S: string;
  L: TStrings;
  I: integer;
begin
  ActiveMemo.Lines.BeginUpdate;
  try
    S := WideToStr(ActiveMemo.SelText);
    if HasLineBreak(S) then
    begin
      L := TStringList.Create;
      try
        L.Text := S;
        for I := 0 to L.Count - 1 do
          L[I] := Format('<%s>%s<%s>', [BegTag, L[I], EndTag]);
        S := L.Text;
      finally
        L.Free;
      end;
    end
    else
      S := Format('<%s>%s<%s>', [BegTag, S, EndTag]);
    ActiveMemo.SelText := StrToWide(S);
  finally
    ActiveMemo.Lines.EndUpdate;
    ;
  end;
end;

procedure TMainForm.SetFontStyle(const NewStyle: string);
begin
  SetFontStyle(NewStyle, '/' + NewStyle);
end;

procedure TMainForm.SetHorzAlign(const NewAlign: string);
var
  S: string;
  L: TStrings;
  I: integer;
begin
  ActiveMemo.Lines.BeginUpdate;
  try
    S := WideToStr(ActiveMemo.SelText);
    if HasLineBreak(S) then
    begin
      L := TStringList.Create;
      try
        L.Text := S;
        for I := 0 to L.Count - 1 do
          L[I] := Format('<%s>%s', [NewAlign, L[I]]);
        S := L.Text;
      finally
        L.Free;
      end;
    end
    else
      S := Format('<%s>%s', [NewAlign, S]);
    ActiveMemo.SelText := StrToWide(S);
  finally
    ActiveMemo.Lines.EndUpdate;
  end;
end;

procedure TMainForm.OnExecuting(Sender: TObject; AThread: TLyThread);
begin
  AThread.Terminate;
end;

function TMainForm.LoadFromFile(const FileName: string; IsCmd: boolean): TLyFilePage;
begin
  Result := FindPage(FileName);
  if Result = nil then
  begin
    Result := ActivePage;
    if (Result = nil) or not Result.IsEmpty then
      Result := GetEmptyPage;
    Result.LoadFromFile(FileName, IsCmd);
  end;
  SelectPage(Result);
end;

function TMainForm.QueryClose(APage: TLyFilePage): boolean;
var
  K: string;
begin
  Result := True;
  if APage.Memo.Modified then
  begin
    K := YesNoCancel(PAD_SAVEPROMPT);
    if K = 'yes' then
    begin
      if APage.FileName <> '' then
        APage.Memo.Lines.SaveToFile(APage.FileName)
      else
      if dlgSave.Execute then
      begin
        if FindPage(dlgSave.FileName) = nil then
          APage.SaveToFile(dlgSave.FileName)
        else
          MsgErr('File is beging edited');
        Result := not APage.Memo.Modified;
      end;
    end
    else
      Result := (K = 'no');
  end;
end;

function TMainForm.SelectPage(APage: TLyFilePage): TLyFilePage;
begin
  if APage <> nil then
  begin
    Result := APage;
    Pages.ActivePage := Result;
    OnStatus(nil);
  end
  else
    Result := NewPage;
end;

function TMainForm.NewPage: TLyFilePage;
begin
  Result := TLyFilePage.Create(Pages);
  Result.PageControl := Pages;
  Result.HandleNeeded;
  Result.Memo.HandleNeeded;
  Result.Memo.OnChange := @OnChange;
  Result.Memo.OnStatus := @OnStatus;
  SelectPage(Result);
end;

function TMainForm.FindPage(const FileName: string): TLyFilePage;
var
  F: string;
  I: integer;
begin
  if FileName <> '' then
  begin
    F := ExpandFileName(FileName);
    for I := 0 to PageCount - 1 do
    begin
      Result := Page[I];
      if SameFileName(F, Result.FileName) then
        Exit;
    end;
    Result := nil;
  end
  else
    Result := FindEmptyPage;
end;

function TMainForm.FindPage(FileID: int64): TLyFilePage;
var
  I: integer;
begin
  for I := 0 to PageCount - 1 do
  begin
    Result := Page[I];
    if Result.FileID = FileID then
      Exit;
  end;
  Result := nil;
end;

function TMainForm.FindEmptyPage: TLyFilePage;
var
  I: integer;
begin
  for I := 0 to PageCount - 1 do
  begin
    Result := Page[I];
    if Result.IsEmpty then
      Exit;
  end;
  Result := nil;
end;

function TMainForm.GetEmptyPage: TLyFilePage;
begin
  Result := FindEmptyPage;
  if Result = nil then
    Result := NewPage;
end;

function TMainForm.ActivePage: TLyFilePage;
begin
  Result := TLyFilePage(Pages.ActivePage);
end;

function TMainForm.ActiveMemo: TLyCodeMemo;
var
  P: TLyFilePage;
  H: integer;
begin
  P := ActivePage;
  if P <> nil then
  begin
    Result := P.Memo;
    H := Pages.Height - Result.height - 6;
    if (H > 20) and (H < ScreenHeight(12)) and (H <> FTabHeight) then
    begin
      FTabHeight := H;
      pnlTabMan.Height := FTabHeight;
    end;
  end
  else
    Result := nil;
end;

end.
