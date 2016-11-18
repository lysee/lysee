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

var
  I: integer;
  F: string;
  T: TLiPasTranslater;

begin
  try
    if ParamCount > 0 then
    begin
      T := TLiPasTranslater.Create;
      try
        for I := 1 to ParamCount do
        begin
          F := ExpandFileName(Trim(ParamStr(I)));
          T.SourceCodes.LoadFromFile(F);
          T.Process;
          T.ResultCodes.SaveToFile(ExtractFilePath(F) + T.ResultUnitName + '.pas');
        end;
      finally
        T.Free;
      end;
      ExitCode := 0;
    end
    else
    begin
      F := ExtractFileName(ParamStr(0));
      Writeln(Format('Usage: %s pmc-files ...', [F]));
    end;
  except
    Writeln(ExceptionStr);
    ExitCode := 1;
  end;
end.
