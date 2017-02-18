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
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
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

procedure TAboutForm.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
{var
  list: TStringList;}
begin
  lblVersion.Caption := Format('Lysee %s', [LSE_VERSION]);
  lblCopyright.Caption := Format('Copyright (C) 2003-%s Li Yun Jie.', [Copy(LSE_VERSION, 1, 4)]);
{ list := TStringList.Create;
  try
    list.LoadFromFile(ExtractFilePath(Application.ExeName) + 'LICENSE.txt');
    Memo1.Lines.Assign(list);
  finally
    list.Free;
  end;}
end;

procedure TAboutForm.Panel3Click(Sender: TObject);
begin

end;

initialization
  {$I lysee_pad_about.lrs}

end.

