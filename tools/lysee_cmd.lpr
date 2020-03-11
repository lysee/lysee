{==============================================================================}
{     PROJECT: lysee_cmd                                                       }
{ DESCRIPTION: lysee script interpreter                                        }
{   COPYRIGHT: Copyright (c) 2003-2015, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2008/04/05                                                      }
{    MODIFIED: 2020/02/16                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
program lysee_cmd;

{$mode objfpc}{$H+}

uses
  Basic, Lysee, Lysee_System, Lysee_Db, Lysee_Pmc, ILysee,
  Lysee_Lib, Lysee_Cmdline;

{$R *.res}

begin
  lysee_cmdline.Command;
end.

