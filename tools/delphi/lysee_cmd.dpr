program lysee_cmd;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  basic in '..\..\source\basic.pas',
  ilysee in '..\..\source\ilysee.pas',
  lysee in '..\..\source\lysee.pas',
  lysee_system in '..\..\source\lysee_system.pas',
  lysee_db in '..\..\source\lysee_db.pas',
  lysee_adodb in '..\..\source\lysee_adodb.pas',
  lysee_cmdline in '..\..\source\lysee_cmdline.pas',
  lysee_pmc in '..\..\source\lysee_pmc.pas',
  lysee_lib in '..\..\source\lysee_lib.pas';

begin
  try
    ExitCode := lysee_cmdline.Command;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
