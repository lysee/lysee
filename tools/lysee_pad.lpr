program lysee_pad;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  basic,
  lysee,
  lysee_system,
  lysee_sysutils,
  lysee_classes,
  lysee_math,
  lysee_db,
  codeedit,
  codesyns,
  lysee_pad_main,
  lysee_pad_about;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

