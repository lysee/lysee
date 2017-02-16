{==============================================================================}
{        UNIT: lysee_system                                                    }
{ DESCRIPTION: lysee's system module                                           }
{   COPYRIGHT: Copyright (c) 2016-2016, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2016/12/17                                                      }
{    MODIFIED: 2017/01/08                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_system;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, basic, lysee;

implementation

uses
  Math, lysee_load;

var
  my_random: integer = 0;

procedure pp_Pos(const Param: TLiParam);
var
  Sub, Str: string;
begin
  Sub := Param[0].AsString;
  Str := Param[1].AsString;
  Param.Result.AsInteger := System.Pos(Sub, Str);
end;

procedure pp_Random(const Param: TLiParam);
const
  Mask   = $00FFFFFF;
  Range  = $01000000;
var
  R: integer;
begin
  if Param.Prmc = 1 then
    R := Max(Param[0].AsInteger, 2) else
    R := Range;
  Inc(my_random, Random(Range) + (PtrToInt(@R) and Mask));
  Param.Result.AsInteger := my_random mod R;
  my_random := my_random mod Range;
end;

procedure pp_Lines(const Param: TLiParam);
var
  A: TLiList;
  I: integer;
  L: TStrings;
begin
  A := TLiList.Create;
  Param.Result.AsList := A;
  L := TStringList.Create;
  try
    L.Text := Param[0].AsString;
    for I := 0 to L.Count - 1 do
      A.Add.AsString := L[I];
  finally
    L.Free;
  end;
end;

procedure pp_Chars(const Param: TLiParam);
var
  A: TLiList;
  S: string;
  I: integer;
begin
  A := TLiList.Create;
  Param.Result.AsList := A;
  S := Param[0].AsString;
  for I := 1 to Length(S) do
    A.Add.AsChar := S[I];
end;

procedure pp_Delete(const Param: TLiParam);
var
  V: TLiValue;
  T: TLiType;
  S: string;
  N: int64;
begin
  V := Param.GetVarbValue(0, T);
  if V <> nil then
  begin
    S := V.AsString;
    if Param.Prmc > 2 then
      N := Max(0, Param[2].AsInteger) else
      N := 1;
    if N > 0 then
    begin
      System.Delete(S, Param[1].AsInteger, N);
      V.AsString := S;
    end;
  end
  else Param.Error('variable not specified');
end;

procedure pp_Insert(const Param: TLiParam);
var
  V: TLiValue;
  T: TLiType;
  S: string;
begin
  V := Param.GetVarbValue(1, T);
  if V <> nil then
  begin
    S := V.AsString;
    System.Insert(Param[0].AsString, S, Param[2].AsInteger);
    V.AsString := S;
  end
  else Param.Error('variable not specified');
end;

procedure pp_Chr(const Param: TLiParam);
begin
  Param.Result.AsChar := Param[0].AsChar;
end;

procedure pp_Abs(const Param: TLiParam);
var
  data: TLiValue;
  clss: TLiType;
begin
  data := Param[0];
  clss := data.VType;
  case clss.TID of
    TID_INTEGER : Param.Result.AsInteger := Abs(data.AsInteger);
    TID_FLOAT   : Param.Result.AsFloat := Abs(data.AsFloat);
    TID_CURRENCY: Param.Result.AsCurrency := Abs(data.AsCurrency);
    else Param.Result.SetValue(data);
  end;
end;

procedure pp_Ord(const Param: TLiParam);
begin
  if Param[0].VType = my_char then
    Param.Result.AsInteger := Ord(Param[0].AsChar) else
    Param.Result.AsInteger := Param[0].AsInteger;
end;

function my_Round(X: double): integer;
begin
  Result := Round(X);
end;

function my_Trunc(X: double): integer;
begin
  Result := Trunc(X);
end;

function my_Ceil(X: double): integer;
begin
  Result := Ceil(X);
end;

function my_Floor(X: double): integer;
begin
  Result := Floor(X);
end;

initialization
  //-- constants ---------------------------------------------------------------

  my_system.Consts.Add('CharSize').AsInteger := sizeof(char);
  {$IFDEF MSWINDOWS}
  my_system.Consts.Add('DriveDelim').AsString := DriveDelim;
  {$ENDIF}

  my_system.Consts.Add('fmOpenRead').AsInteger := fmOpenRead;
  my_system.Consts.Add('fmOpenWrite').AsInteger := fmOpenWrite;
  my_system.Consts.Add('fmOpenReadWrite').AsInteger := fmOpenReadWrite;
  my_system.Consts.Add('fmShareCompat').AsInteger := fmShareCompat;
  my_system.Consts.Add('fmShareExclusive').AsInteger := fmShareExclusive;
  my_system.Consts.Add('fmShareDenyWrite').AsInteger := fmShareDenyWrite;
  my_system.Consts.Add('fmShareDenyRead').AsInteger := fmShareDenyRead;
  my_system.Consts.Add('fmShareDenyNone').AsInteger := fmShareDenyNone;

  {$IFNDEF FPC}
  my_system.Consts.Add('INVALID_HANDLE_VALUE').AsInteger := INVALID_HANDLE_VALUE;
  {$ENDIF}

  // system.functions ----------------------------------------------------------

  AddFunc('Pos', my_int, ['SubStr', 'Str'], [my_string, my_string],
          {$IFDEF FPC}@{$ENDIF}pp_Pos);
  AddFunc('Random', my_int, ['_Range'], [my_int],
          {$IFDEF FPC}@{$ENDIF}pp_Random);
  AddFunc('Lines', my_list, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_Lines);
  AddFunc('Chars', my_list, ['S'], [my_string],
          {$IFDEF FPC}@{$ENDIF}pp_Chars);
  AddFunc('Delete', ['VarStr', 'Index', '_Count'],
          [my_string, my_int, my_int],
          {$IFDEF FPC}@{$ENDIF}pp_Delete);
  AddFunc('Insert', ['SubStr', 'VarStr', 'Index'],
          [my_string, my_string, my_int],
          {$IFDEF FPC}@{$ENDIF}pp_Insert);
  AddFunc('Chr', my_char, ['Value'], [my_variant],
          {$IFDEF FPC}@{$ENDIF}pp_Chr);
  AddFunc('Abs', my_variant, ['Any'], [my_variant],
          {$IFDEF FPC}@{$ENDIF}pp_Abs);
  AddFunc('Ord', my_int, ['Any'], [my_variant],
          {$IFDEF FPC}@{$ENDIF}pp_Ord);

  LoadFunc(my_system, 'GenID', {$IFDEF FPC}@{$ENDIF}GenID);
  LoadFunc(my_system, 'Round', 'X', {$IFDEF FPC}@{$ENDIF}my_Round);
  LoadFunc(my_system, 'Trunc', 'X', {$IFDEF FPC}@{$ENDIF}my_Trunc);
  LoadFunc(my_system, 'Ceil', 'X', {$IFDEF FPC}@{$ENDIF}my_Ceil);
  LoadFunc(my_system, 'Floor', 'X', {$IFDEF FPC}@{$ENDIF}my_Floor);

  LoadProc(my_system, 'Randomize', {$IFDEF FPC}@{$ENDIF}Randomize);
end.
