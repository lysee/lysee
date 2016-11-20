{==============================================================================}
{     PROGRAM: lysee_lmu                                                       }
{ DESCRIPTION: translate pmc-file to lysee module unit                         }
{   COPYRIGHT: Copyright (c) 2013-2015, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2013/11/19                                                      }
{    MODIFIED: 2016/11/19                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
program lysee_lmu;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  basic in '..\basic.pas',
  lysee in '..\lysee.pas',
  lysee_pmc in '..\lysee_pmc.pas';

begin
  ExitCode := 1 - Ord(CommandLMU);
end.
