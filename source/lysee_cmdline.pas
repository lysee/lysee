{==============================================================================}
{        UNIT: lysee_cmdline                                                   }
{ DESCRIPTION: lysee command line                                              }
{   COPYRIGHT: Copyright (c) 2017, Li Yunjie. All Rights Reserved.             }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2017/02/21                                                      }
{    MODIFIED: 2019/03/16                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_cmdline;

{$IFDEF FPC}
{$mode objfpc}
{$ENDIF}

interface

uses
  Classes, SysUtils, basic, lysee;

function Command: integer;

implementation

uses
  lysee_pmc;

procedure ShowHeader;
begin
  Writeln(Format('Interactive LYSEE script interpreter v%s', [LSE_VERSION]));
end;

procedure ShowUsage;
begin
  ShowHeader;
  Writeln('');
  Writeln('Usage: lysee [OPTION]... [FILE [ARGS]...]');
  Writeln('');
  Writeln('Option:');
  Writeln('  -v, --version           print version information of lysee.');
  Writeln('  -h, --help              print this usage and.');
  Writeln('  -p, --pause             pause after executing.');
  Writeln('  -w, --wrap              wrap Pascal unit file.');
  Writeln('  -l, --library           wrap Pascal unit to library.');
  Writeln('');
  Writeln('Args:');
  Writeln('  *                       arguments passed to script file.');
end;

procedure ShowVersion;
begin
  ShowHeader;
  Writeln('');
  Writeln('This program is distributed in the hope that it will be useful,');
  Writeln('but WITHOUT ANY WARRANTY; without even the implied warranty of');
  Writeln('MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.');
  Writeln('');
  Writeln('Enjoy it! I am Li Yunjie<718956073@qq.com>.');
end;

function ExecutePMC(const FileName: string): integer;
var
  P: TLyPasTranslater;
begin
  Result := 0;
  try
    if FileName = '' then
    begin
      Writeln('Usage: lysee -w/--wrap PascalUnitFile');
      Result := 1;
    end
    else
    begin
      P := TLyPasTranslater.Create;
      try
        P.SourceCodes.LoadFromFile(SetPD(FileName));
        P.UnitToLibrary := false;
        P.Execute;
        Writeln(P.ResultCodes.Text);
      finally
        P.Free;
      end;
    end;
  except
    Writeln(ExceptionStr);
    Result := 1;
  end;
end;

function ExecuteUnitToLibrary(const FileName: string): integer;
var
  P: TLyPasTranslater;
begin
  Result := 0;
  try
    if FileName = '' then
    begin
      Writeln('Usage: lysee -l/--library PascalUnitFile');
      Result := 1;
    end
    else
    begin
      P := TLyPasTranslater.Create;
      try
        P.SourceCodes.LoadFromFile(SetPD(FileName));
        P.UnitToLibrary := true;
        P.Execute;
        Writeln(P.ResultCodes.Text);
      finally
        P.Free;
      end;
    end;
  except
    Writeln(ExceptionStr);
    Result := 1;
  end;
end;

function ExecuteFrom(Index: integer): integer;
var
  E: TLysee;
begin
  Result := 0;
  try
    E := TLysee.Create(nil);
    try
      E.ExecuteFileFrom(Index);
    finally
      if E.MainThread.Excepted then
      begin
        Writeln(E.MainThread.Error.AsText);
        Result := 1;
      end;
      E.Free;
    end;
  except
    Writeln(ExceptionStr);
    Result := 1;
  end;
end;

function Interpreter: integer;
var
  S, code: string;
  E: TLysee;
  N: int64;

  procedure do_restart;
  begin
    Writeln('*************** RESTARTED ***************');
    E.Clear;
    code := '';
  end;

  procedure do_cancel;
  begin
    Writeln('*************** CANCELED ****************');
    code := '';
  end;

  procedure do_execute(const S: string);
  begin
    try
      if S <> '' then
      begin
        N := E.WriteCount;
        if E.MainThread.Execute(AddTrailingChar(S, ';')) then
        begin
          if N <> E.WriteCount then
            if not CharInSet(E.LastWritenChar, [#10, #13])  then
              E.Writeln;
          if E.MainThread.Result.VType <> my_nil then
            Writeln('--> ' + FormatValue(E.MainThread.Result));
        end
        else Writeln(E.MainThread.Error.AsText);
      end;
    finally
      code := '';
    end;
  end;

  function match_head(const S: string; const IDs: array of string): boolean;
  var
    I: integer;
  begin
    Result := false;
    for I := 0 to Length(IDs) - 1 do
      if MatchID(IDs[I], Copy(S, 1, Length(IDs[I]))) then
      begin
        Result := true;
        Exit;
      end;
  end;

  function match_fp(const S: string): boolean;
  begin
    Result := match_head(S, ['function', 'procedure'])
  end;

begin
  Result := 0;
  E := TLysee.Create(nil);
  try
    ShowHeader;
    Writeln('');
    Writeln('        /C=CANCEL /Q=QUIT /R=RESTART');
    Writeln('');
    code := '';
    repeat
      if code = '' then Write('>>> ') else Write('  > ');
      Readln(S);
      if S = '' then do_execute(code) else
      begin
        S := TrimRight(S);
        if MatchID('/q', S) then Break else
        if MatchID('/r', S) then do_restart else
        if MatchID('/c', S) then do_cancel else
        if code <> '' then
          code := code + sLineBreak + S else
        if S <> '' then
          if (S[Length(S)] <> ';') or match_fp(Trim(S)) then
            code := S else
            do_execute(S);
      end;
    until E.MainThread.Halted;
    if E.MainThread.Excepted then Result := 1;
  finally
    E.Free;
  end;
end;

function Command: integer;
var
  I, N: integer;
  S: string;
  P: boolean;
begin
  Result := 0; // OK
  P := false;
  I := 1;
  N := ParamCount;
  while I <= N do
  begin
    S := Trim(ParamStr(I));

    if MatchID(S, '-h') or MatchID(S, '--help') then
    begin
      ShowUsage;
      Exit;
    end;

    if MatchID(S, '-v') or MatchID(S, '--version') then
    begin
      ShowVersion;
      Exit;
    end;

    if MatchID(S, '-w') or MatchID(S, '--wrap') then
    begin
      ExecutePMC(Trim(ParamStr(I + 1)));
      if P then
      begin
        Write('Press ENTER to continue: ');
        Readln;
      end;
      Exit;
    end;

    if MatchID(S, '-l') or MatchID(S, '--library') then
    begin
      ExecuteUnitToLibrary(Trim(ParamStr(I + 1)));
      if P then
      begin
        Write('Press ENTER to continue: ');
        Readln;
      end;
      Exit;
    end;

    if MatchID(S, '-p') or MatchID(S, '--pause') then
      P := true else Break;
    Inc(I);
  end;

  try
    if I <= N then
      Result := ExecuteFrom(I) else
      Result := Interpreter;
  except
    Writeln(ExceptionStr);
    Result := 1;
  end;

  if P and (I <= N) then
  begin
    Write('Press ENTER to continue: ');
    Readln;
  end;
end;

end.

