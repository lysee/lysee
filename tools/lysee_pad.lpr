program lysee_pad;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, CodeMemo, lysee_pad_about, lysee_pad_main, Basic, ilysee,
  lysee, lysee_db, lysee_lib, lysee_pmc, lysee_system, lysee_pad_find,
  lysee_syntax, UIDefs, msgbox, lysee_pad_sheet, lysee_pad_fontname
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFindForm, FindForm);
  Application.Run;
end.

