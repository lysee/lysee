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
  ExtCtrls, StdCtrls;

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
    procedure FormCreate(Sender: TObject);
    procedure Memo1KeyPress(Sender: TObject; var Key: char);
  private
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

procedure TAboutForm.FormCreate(Sender: TObject);
var
  F: string;
begin
  lblVersion.Caption := 'Lysee ' + LSE_VERSION;
  lblCopyright.Caption := 'Copyright (C) 2003,...,2020 Li Yunjie<718956073@qq.com>.';
  F := ExtractFilePath(Application.ExeName) + 'LICENSE';
  if not FileExists(F) then
  begin
    F := ChangeFileExt(F, '.txt');
    if not FileExists(F) then F := '';
  end;
  if F <> '' then
    Memo1.Lines.LoadFromFile(F);
end;

procedure TAboutForm.Memo1KeyPress(Sender: TObject; var Key: char);
begin
  if CharInSet(Key, [#10, #13]) then Close;
end;

initialization
  {$I lysee_pad_about.lrs}

end.

