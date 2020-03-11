unit lysee_pad_find;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFindForm }

  TFindForm = class(TForm)
    bFind: TButton;
    bCancel: TButton;
    cbReplace: TCheckBox;
    cbWholeWord: TCheckBox;
    cbCaseSensitive: TCheckBox;
    cbSelectedText: TCheckBox;
    eFind: TEdit;
    eReplace: TEdit;
    gbOptions: TGroupBox;
    Label1: TLabel;
    procedure cbCaseSensitiveChange(Sender: TObject);
    procedure cbReplaceChange(Sender: TObject);
    procedure cbWholeWordChange(Sender: TObject);
    procedure cbSelectedTextChange(Sender: TObject);
    procedure eFindChange(Sender: TObject);
    procedure eReplaceChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FFindText: string;
    FReplaceText: string;
    FReplace: boolean;
    FWholeWord: boolean;
    FCaseSensitive: boolean;
    FFindInSelected: boolean;
  public
    property FindText: string read FFindText;
    property ReplaceText: string read FReplaceText;
    property Replace: boolean read FReplace;
    property WholeWord: boolean read FWholeWord;
    property CaseSensitive: boolean read FCaseSensitive;
    property FindInSelected: boolean read FFindInSelected;
  end;

var
  FindForm: TFindForm;


function ModalFindReplace(Find, SelAvail: boolean): boolean;

implementation

{$R *.lfm}

uses
  MsgBox;

function ModalFindReplace(Find, SelAvail: boolean): boolean;
begin
  FindForm.cbSelectedText.Checked := SelAvail;
  FindForm.cbSelectedTextChange(nil);
  FindForm.cbReplace.Checked := not Find;
  FindForm.cbReplaceChange(nil);
  Result := (FindForm.ShowModal = mrOK);
end;

{ TFindForm }

procedure TFindForm.eFindChange(Sender: TObject);
begin
  FFindText := eFind.Text;
  bFind.Enabled := (FFindText <> '') and
    (not FReplace or (FFindText <> FReplaceText));
end;

procedure TFindForm.eReplaceChange(Sender: TObject);
begin
  FReplaceText := eReplace.Text;
  bFind.Enabled := (FFindText <> '') and
    (not FReplace or (FFindText <> FReplaceText));
end;

procedure TFindForm.FormActivate(Sender: TObject);
begin
  eFind.SetFocus;
end;

procedure TFindForm.FormCreate(Sender: TObject);
begin
  CenterScreen(Self);
end;

procedure TFindForm.cbReplaceChange(Sender: TObject);
begin
  FReplace := cbReplace.Checked;
  if FReplace then
  begin
    Caption := 'Replace';
    bFind.Caption := 'Replace';
  end
  else
  begin
    Caption := 'Find';
    bFind.Caption := 'Find';
  end;
  eReplace.Enabled := FReplace;
end;

procedure TFindForm.cbCaseSensitiveChange(Sender: TObject);
begin
  FCaseSensitive := cbCaseSensitive.Checked;
end;

procedure TFindForm.cbWholeWordChange(Sender: TObject);
begin
  FWholeWord := cbWholeWord.Checked;
end;

procedure TFindForm.cbSelectedTextChange(Sender: TObject);
begin
  FFindInSelected := cbSelectedText.Checked;
end;

end.

