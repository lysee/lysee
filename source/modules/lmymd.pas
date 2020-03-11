{==============================================================================}
{     PROJECT: lysee_ymd                                                       }
{ DESCRIPTION: year-month-day functions                                        }
{   COPYRIGHT: Copyright (c) 2003-2012, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2007/07/12                                                      }
{    MODIFIED: 2013/09/16                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lmymd;

{$mode objfpc}{$H+}

interface

{@pmc-description year-month-day functions}
function now:integer;
function next(ymd, offset: integer): integer;
function prev(ymd, offset: integer): integer;
function check(ymd: integer): boolean;
{@pmc-end}

implementation

uses
  Classes, SysUtils, DateUtils;

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

function decodeYMD(ymd: integer; var y, m, d: integer): boolean;
var
  T: TDateTime;
begin
  d := ymd mod 100;
  if d in [1..31] then
    if decodeYM((ymd - d) div 100, y, m) then
    begin
      Result := TryEncodeDate(y, m, d, T);
      Exit;
    end;
  Result := false;
end;

function next(ymd, offset: integer): integer;
var
  iy, im, id: integer;
  wy, wm, wd: word;
  T: TDateTime;
begin
  id := ymd mod 100;
  if id in [1..31] then
    if decodeYM((ymd - id) div 100, iy, im) then
      if TryEncodeDate(iy, im, id, T) then
      begin
        DecodeDate(IncDay(T, offset), wy, wm, wd);
        Result := (wy * 10000) + (wm * 100) + wd;
        Exit;
      end;
  Result := 0;
end;

function prev(ymd, offset: integer): integer;
begin
  Result := next(ymd, - offset);
end;

function now:integer;
var
  y, m, d: word;
begin
  DecodeDate(SysUtils.Now, y, m, d);
  Result := (y * 10000) + (m * 100) + d;
end;

function check(ymd: integer): boolean;
var
  y, m, d: integer;
begin
  Result := decodeYMD(ymd, y, m, d);
end;

end.

