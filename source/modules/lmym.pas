{==============================================================================}
{     PROJECT: lysee_ym                                                        }
{ DESCRIPTION: year-month functions                                            }
{   COPYRIGHT: Copyright (c) 2003-2012, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2007/07/12                                                      }
{    MODIFIED: 2013/09/16                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lmym;

{$mode objfpc}{$H+}

interface

{@pmc-description year-month functions}
function now: integer;
function next(ym, offset: integer): integer;
function prev(ym, offset: integer): integer;
function check(ym: integer): boolean;
{@pmc-end}

implementation

uses
  Classes, SysUtils;

function decodeYM(ym: integer; var y, m: integer): boolean;
begin
  m := ym mod 100;
  if m in [1..12] then
  begin
    y := (ym - m) div 100;
    Result := (y >= 1) and (y <= 9999);
    Exit;
  end;
  Result := false;
end;

function next(ym, offset: integer): integer;
var
  y, m: integer;
begin
  if not decodeYM(ym, y, m) then
    Result := 0 else
  if offset <> 0 then
  begin
    Inc(y, offset div 12);
    Inc(m, offset mod 12);
    if m > 12 then
    begin
      m := m - 12;
      Inc(y);
    end
    else
    if m < 1 then
    begin
      m := m + 12;
      Dec(y);
    end;
    Result := (y * 100) + m;
  end
  else Result := ym;
end;

function prev(ym, offset: integer): integer;
begin
  Result := next(ym, - offset);
end;

function now: integer;
var
  y, m, d: word;
begin
  DecodeDate(SysUtils.Now, y, m, d);
  Result :=(y * 100) + m;
end;

function check(ym: integer): boolean;
var
  y, m: integer;
begin
  Result := decodeYM(ym, y, m);
end;

end.

