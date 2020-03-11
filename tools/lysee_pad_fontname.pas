unit lysee_pad_fontname;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFontNameForm }

  TFontNameForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    lbxFontName: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure lbxFontNameClick(Sender: TObject);
  private

  public

  end;

var
  FontNameForm: TFontNameForm;

function SelectFontName(const Now: string): string;

implementation

{$R *.lfm}

function SelectFontName(const Now: string): string;
var
  I: integer;
begin
  if FontNameForm = nil then
    FontNameForm := TFontNameForm.Create(Application);
  with FontNameForm do
  begin
    I := lbxFontName.Items.IndexOf(Now);
    if I >= 0 then
      lbxFontName.ItemIndex := I;
    lbxFontNameClick(nil);
    if ShowModal = mrOK then
      Result := lbxFontName.Items[lbxFontName.ItemIndex] else
      Result := '';
  end;
end;

{ TFontNameForm }

procedure TFontNameForm.FormCreate(Sender: TObject);
begin
  lbxFontName.Items.Assign(Screen.Fonts);
  lbxFontName.Sorted := true;
  lbxFontName.ItemIndex := 0;
end;

procedure TFontNameForm.lbxFontNameClick(Sender: TObject);
begin
  lbxFontName.Enabled := lbxFontName.ItemIndex >= 0;
end;

end.

