{==============================================================================}
{     PROJECT: lysee_cmd                                                       }
{ DESCRIPTION: lysee script interpreter (Delphi)                               }
{   COPYRIGHT: Copyright (c) 2003-2015, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/10/10                                                      }
{    MODIFIED: 2015/05/17                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
program lysee_cmd;

{$APPTYPE CONSOLE}

uses
  basic in '..\basic.pas',
  lysee in '..\lysee.pas',
  lysee_pmc in '..\lysee_pmc.pas',
  lysee_db in '..\lysee_db.pas',
  lysee_adodb in '..\lysee_adodb.pas';

begin
  Command;
end.
