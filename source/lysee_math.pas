{==============================================================================}
{        UNIT: lysee_math                                                      }
{ DESCRIPTION: lysee's mathmetic module functions                              }
{   COPYRIGHT: Copyright (c) 2016-2016, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2016/12/18                                                      }
{    MODIFIED: 2017/02/19                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_math;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, basic, lysee;

type

  { TLyseeMathModule }

  TLyseeMathModule = class(TLyseeModule)
  private
    procedure DoSetup(Sender: TObject);
  public
    constructor Create(const AName: string);override;
  end;

var
  my_math: TLyseeMathModule;

implementation

uses
  Math;

// math.functions --------------------------------------------------------------

procedure pp_max(const Param: TLyseeParam);
var
  V, T: TLyseeValue;
  L: TLyseeList;
  I: integer;
begin
  V := Param[0];
  T := Param[1];
  if V.Compare(T, [crLess]) then V := T;
  L := Param[2].AsArray;
  if L <> nil then
    for I := 0 to L.Count - 1 do
    begin
      T := L[I];
      if V.Compare(T, [crLess]) then V := T;
    end;
  Param.Result.SetValue(V);
end;

procedure pp_min(const Param: TLyseeParam);
var
  V, T: TLyseeValue;
  L: TLyseeList;
  I: integer;
begin
  V := Param[0];
  T := Param[1];
  if V.Compare(T, [crMore]) then V := T;
  L := Param[2].AsArray;
  if L <> nil then
    for I := 0 to L.Count - 1 do
    begin
      T := L[I];
      if V.Compare(T, [crMore]) then V := T;
    end;
  Param.Result.SetValue(V);
end;

{ TLyseeMathModule }

constructor TLyseeMathModule.Create(const AName: string);
begin
  inherited;
  OnSetup := {$IFDEF FPC}@{$ENDIF}DoSetup;
end;

procedure TLyseeMathModule.DoSetup(Sender: TObject);
begin
  OnSetup := nil;
  AddFunc('Max', my_variant,
          ['V1', 'V2', '...'], [my_variant, my_variant, my_array],
          {$IFDEF FPC}@{$ENDIF}pp_max);
  AddFunc('Min', my_variant,
          ['V1', 'V2', '...'], [my_variant, my_variant, my_array],
          {$IFDEF FPC}@{$ENDIF}pp_min);
end;

initialization
begin
  my_math := TLyseeMathModule.Create('Math');
end;

end.
