{==============================================================================}
{        UNIT: lysee_pad_about                                                 }
{ DESCRIPTION: about dialog of lysee_pad_fpc (FPC)                             }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2008/04/05                                                      }
{    MODIFIED: 2016/11/17                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_pad_about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, codeedit;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    btnOK: TButton;
    Image1: TImage;
    lblVersion: TLabel;
    lblCopyright: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
  private
    FEdit: TCodeEdit;
    { private declarations }
  public
    { public declarations }
  end; 

var
  AboutForm: TAboutForm;

implementation

uses
  lysee;

{ TAboutForm }

procedure TAboutForm.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
var
  F: string;
begin
  lblVersion.Caption := Format('Lysee v%s', [LSE_VERSION]);
  lblCopyright.Caption := 'Copyright (C) 2017 Li Yunjie<718956073@qq.com>.';
  F := ExtractFilePath(Application.ExeName) + 'LICENSE';
  if not FileExists(F) then
  begin
    F := ChangeFileExt(F, '.txt');
    if not FileExists(F) then
      F := '';
  end;
  if F <> '' then
    Memo1.Lines.LoadFromFile(F);
end;

procedure TAboutForm.Panel3Click(Sender: TObject);
begin

end;

initialization
  {$I lysee_pad_about.lrs}

end.

