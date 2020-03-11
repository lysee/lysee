{==============================================================================}
{     PROJECT: lysee_process                                                   }
{ DESCRIPTION: processing outside programs                                     }
{   COPYRIGHT: Copyright (c) 2012, Li Yun Jie. All Rights Reserved.            }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/03/03                                                      }
{    MODIFIED: 2013/09/16                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lmprocess;

{$mode objfpc}{$H+}

interface

{@pmc-description processing outside programs}
function shouts(const CmdLine: string): string;
function shexec(const CmdLine: string): integer;
{@pmc-end}

implementation

uses
  Classes, SysUtils, Math, Process, lysee;

function shouts(const CmdLine: string): string;
var
  P: TProcess;
  B: array[0..1023] of char;
  L: integer;
begin
  Result := '';
  B[0] := #0;
  P := TProcess.Create(nil);
  try
    P.CommandLine := CmdLine;
    P.Options := [poUsePipes, poStderrToOutPut];
    P.Execute;
    P.CloseInput;
    while P.Running or (P.Output.NumBytesAvailable > 0) do
    begin
      L := Min(1024, P.Output.NumBytesAvailable);
      if L > 0 then
      begin
        L := P.Output.Read(B[0], L);
        if L > 0 then
          Result := Result + LyStrPas(B, L);
      end
      else Sleep(1);
    end;
  finally
    P.Free;
  end;
end;

function shexec(const CmdLine: string): integer;
var
  P: TProcess;
begin
  P := TProcess.Create(nil);
  try
    P.CommandLine := CmdLine;
    P.Options := [poUsePipes, poStderrToOutPut];
    P.Execute;
    P.CloseInput;
    P.CloseOutput;
    P.CloseStderr;
    while P.Running do Sleep(1);
    Result := P.ExitStatus;
  finally
    P.Free;
  end;
end;

end.

