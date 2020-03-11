program lysee_pad;

uses
  Forms,
  lysee_pad_main in 'lysee_pad_main.pas' {MainForm},
  lysee in '..\..\source\lysee.pas',
  lysee_system in '..\..\source\lysee_system.pas',
  lysee_pmc in '..\..\source\lysee_pmc.pas',
  lysee_db in '..\..\source\lysee_db.pas',
  lysee_adodb in '..\..\source\lysee_adodb.pas',
  basic in '..\..\source\basic.pas',
  uidefs in '..\..\source\uidefs.pas',
  codememo in '..\..\source\codememo.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

