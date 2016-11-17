{==============================================================================}
{     PROJECT: lysee_pad                                                       }
{ DESCRIPTION: lysee script file editing PAD (FPC)                             }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2008/04/05                                                      }
{    MODIFIED: 2016/11/17                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
program lysee_pad;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms,
  basic,
  lysee,
  lysee_db,
  lysee_pmc,
  lysee_syntax,
  lysee_pad_about,
  msgbox,
  lysee_pad_main;

{$R lysee_pad.res}

begin
  Application.Initialize;
  Application.CreateForm(TPadForm, PadForm);
  Application.Run;
end.

