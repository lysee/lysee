{==============================================================================}
{        UNIT: lysee_math                                                      }
{ DESCRIPTION: lysee's mathmetic module functions                              }
{   COPYRIGHT: Copyright (c) 2016-2016, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2016/12/18                                                      }
{    MODIFIED: 2017/01/07                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_math;

interface

uses
  SysUtils, Classes, Basic, lysee;

var
  my_math: TLiModule;

implementation

uses
  Math, lysee_load;

// math.functions --------------------------------------------------------------

procedure pp_max(const Param: TLiParam);
var
  V, T: TLiValue;
  L: TLiList;
  I: integer;
begin
  V := Param[0];
  T := Param[1];
  if V.Compare(T, [crLess]) then V := T;
  L := Param[2].AsList;
  if L <> nil then
    for I := 0 to L.Count - 1 do
    begin
      T := L[I];
      if V.Compare(T, [crLess]) then V := T;
    end;
  Param.Result.SetValue(V);
end;

procedure pp_min(const Param: TLiParam);
var
  V, T: TLiValue;
  L: TLiList;
  I: integer;
begin
  V := Param[0];
  T := Param[1];
  if V.Compare(T, [crMore]) then V := T;
  L := Param[2].AsList;
  if L <> nil then
    for I := 0 to L.Count - 1 do
    begin
      T := L[I];
      if V.Compare(T, [crMore]) then V := T;
    end;
  Param.Result.SetValue(V);
end;

initialization
  my_math := AddModule('Math');

  my_math.AddFunc('Max', my_variant,
          ['V1', 'V2', '...'], [my_variant, my_variant, my_list],
          {$IFDEF FPC}@{$ENDIF}pp_max);
  my_math.AddFunc('Min', my_variant,
          ['V1', 'V2', '...'], [my_variant, my_variant, my_list],
          {$IFDEF FPC}@{$ENDIF}pp_min);
end.
