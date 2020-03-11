{==============================================================================}
{        UNIT: ilysee                                                          }
{ DESCRIPTION: interface of lysee library                                      }
{   COPYRIGHT: Copyright (c) 2019, Li Yun Jie. All Rigrhts Reserved.           }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2019/03/30                                                      }
{    MODIFIED: 2020/03/11                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit ilysee;

interface

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

uses
  SysUtils, Classes;

const

  { Lysee Object ID }

  LID_SYSTEM_MODULE  = 1001;
  LID_INTEGER_TYPE   = 1002;
  LID_VARIANT_TYPE   = 1003;
  LID_NIL_TYPE       = 1004;
  LID_STRING_TYPE    = 1005;
  LID_CHAR_TYPE      = 1006;
  LID_BOOLEAN_TYPE   = 1007;
  LID_FLOAT_TYPE     = 1008;
  LID_CURRENCY_TYPE  = 1009;
  LID_DATETIME_TYPE  = 1010;
  LID_TYPE_TYPE      = 1011;
  LID_EXCEPTION_TYPE = 1012;
  LID_MODULE_TYPE    = 1013;
  LID_FUNC_TYPE      = 1014;
  LID_OBJECT_TYPE    = 1015;
  LID_ARRAY_TYPE     = 1016;
  LID_LIST_TYPE      = 1017;
  LID_HASH_TYPE      = 1018;
  LID_STRINGS_TYPE   = 1019;
  LID_STRLIST_TYPE   = 1020;

type

  PLyModule = pointer; // lysee.TLyModule
  PLyType   = pointer; // lysee.TLyType
  PLyFunc   = pointer; // lysee.TLyFunc
  PLyValue  = pointer; // lysee.TLyValue
  PLyParam  = pointer; // lysee.TLyParam

  { RLysee }

  TLyseeProc  = procedure(const P: PLyParam);
  TLySetAnsiStr = procedure(const S: PAnsiChar; L: integer; Outs: PAnsiString);
  TLySetWideStr = procedure(const S: PWideChar; L: integer; Outs: PWideString);

  RLysee = packed record
    size          : integer;
    unicode       : boolean;
    find1         : function(ID: integer): pointer;
    find2         : function(const Name: PAnsiChar): pointer;
    find_module   : function(const Name: PAnsiChar): PLyModule;
    find_type     : function(const Name: PAnsiChar): PLyType;
    add_func      : function(M: PLyModule; const Name: PAnsiChar; T: PLyType; Proc: TLyseeProc): PLyFunc;
    add_param     : function(F: PLyFunc; const Name: PAnsiChar; T: PLyType): boolean;
    add_method    : function(AType: PLyType; const Name: PAnsiChar; T: PLyType; Proc: TLyseeProc): PLyFunc;
    { param }
    param_count   : function(P: PLyParam): integer;
    param_prmc    : function(P: PLyParam): integer;
    param_type    : function(P: PLyParam; X: integer): PLyType;
    param_integer : function(P: PLyParam; X: integer): int64;
    param_boolean : function(P: PLyParam; X: integer): boolean;
    param_double  : function(P: PLyParam; X: integer): double;
    param_curr    : function(P: PLyParam; X: integer): currency;
    param_time    : function(P: PLyParam; X: integer): TDateTime;
    param_charA   : function(P: PLyParam; X: integer): AnsiChar;
    param_charW   : function(P: PLyParam; X: integer): WideChar;
    param_pcharA  : function(P: PLyParam; X: integer): PAnsiChar;
    param_pcharW  : function(P: PLyParam; X: integer): PWideChar;
    param_ansistr : procedure(P: PLyParam; X: integer; SetAS: TLySetAnsiStr; Outs: PAnsiString);
    param_widestr : procedure(P: PLyParam; X: integer; SetWS: TLySetWideStr; Outs: PWideString);
    param_data    : function(P: PLyParam; X: integer): pointer;
    param_error   : function(P: PLyParam; const Msg: PAnsiChar): boolean;
    { result }
    result_integer: procedure(P: PLyParam; Value: int64);
    result_boolean: procedure(P: PLyParam; Value: boolean);
    result_double : procedure(P: PLyParam; Value: double);
    result_curr   : procedure(P: PLyParam; Value: currency);
    result_time   : procedure(P: PLyParam; Value: TDateTime);
    result_charA  : procedure(P: PLyParam; Value: AnsiChar);
    result_charW  : procedure(P: PLyParam; Value: WideChar);
    result_pcharA : procedure(P: PLyParam; Buf: PAnsiChar; Len: integer);
    result_pcharW : procedure(P: PLyParam; Buf: PWideChar; Len: integer);
    result_data   : procedure(P: PLyParam; T: PLyType; Data: pointer);
  end;
  PLysee = ^RLysee;

  TLyInitModule = procedure(Module: PLyModule; Lyrec: PLysee);

//
// lysee library should export LyInitModule procedure
//
procedure LyInitModule(Module: PLyModule; Lyrec: PLysee);

{ find }

function LyFind(ID: integer): pointer;overload;
function LyFind(const Name: string): pointer;overload;
function LyFind(const Module, Name: string): pointer;overload;
function LyFindType(const Name: string): PLyType;overload;
function LyFindType(const Module, Name: string): PLyType;overload;

{ add function }

function LyAddFunc(const FuncName: AnsiString;
                   const FuncType: PLyType;
                   const Proc: TLyseeProc): PLyFunc;overload;

function LyAddFunc(const FuncName: AnsiString;
                   const Proc: TLyseeProc): PLyFunc;overload;

function LyAddFunc(const FuncName: AnsiString;
                   const FuncType: PLyType;
                   const ParamNames: array of AnsiString;
                   const ParamTypes: array of PLyType;
                   const Proc: TLyseeProc): PLyFunc;overload;

function LyAddFunc(const FuncName: AnsiString;
                   const ParamNames: array of AnsiString;
                   const ParamTypes: array of PLyType;
                   const Proc: TLyseeProc): PLyFunc;overload;

{ add method }

function LyMethod(const Parent: PLyType;
                  const FuncName: AnsiString;
                  const FuncType: PLyType;
                  const Proc: TLyseeProc): PLyFunc;overload;

function LyMethod(const Parent: PLyType;
                  const FuncName: AnsiString;
                  const Proc: TLyseeProc): PLyFunc;overload;

function LyMethod(const Parent: PLyType;
                  const FuncName: AnsiString;
                  const FuncType: PLyType;
                  const ParamNames: array of AnsiString;
                  const ParamTypes: array of PLyType;
                  const Proc: TLyseeProc): PLyFunc;overload;

function LyMethod(const Parent: PLyType;
                  const FuncName: AnsiString;
                  const ParamNames: array of AnsiString;
                  const ParamTypes: array of PLyType;
                  const Proc: TLyseeProc): PLyFunc;overload;

{ define property }

function LyDefine(const Parent: PLyType;
                  const PropName: AnsiString;
                  const PropType: PLyType;
                  const GetProc: TLyseeProc;
                  const SetProc: TLyseeProc): boolean;overload;

function LyDefine(const Parent: PLyType;
                  const PropName: AnsiString;
                  const PropType: PLyType;
                  const GetProc: TLyseeProc): boolean;overload;

function LyDefine(const Parent: PLyType;
                  const PropType: PLyType;
                  const IndexNames: array of AnsiString;
                  const IndexTypes: array of PLyType;
                  const GetProc: TLyseeProc;
                  const SetProc: TLyseeProc): boolean;overload;

function LyDefine(const Parent: PLyType;
                  const PropType: PLyType;
                  const IndexNames: array of AnsiString;
                  const IndexTypes: array of PLyType;
                  const GetProc: TLyseeProc): boolean;overload;

{ get param }

function LyParamError(P: PLyParam; const Msg: string): boolean;
function LyParamCount(P: PLyParam): integer;
function LyParamPRMC(P: PLyParam): integer;
function LyParamType(P: PLyParam; X: integer): PLyType;
function LyParamInt(P: PLyParam; X: integer): int64;
function LyParamBool(P: PLyParam; X: integer): boolean;
function LyParamFloat(P: PLyParam; X: integer): double;
function LyParamCurr(P: PLyParam; X: integer): currency;
function LyParamTime(P: PLyParam; X: integer): TDateTime;
function LyParamChar(P: PLyParam; X: integer): char;
function LyParamAnsiStr(P: PLyParam; X: integer): AnsiString;
function LyParamWideStr(P: PLyParam; X: integer): WideString;
function LyParamStr(P: PLyParam; X: integer): string;
function LyParamData(P: PLyParam; X: integer): pointer;

{ set result }

procedure LyResultInt(P: PLyParam; Value: int64);
procedure LyResultBool(P: PLyParam; Value: boolean);
procedure LyResultFloat(P: PLyParam; Value: double);
procedure LyResultCurr(P: PLyParam; Value: currency);
procedure LyResultTime(P: PLyParam; Value: TDateTime);
procedure LyResultChar(P: PLyParam; Value: char);
procedure LyResultStr(P: PLyParam; Value: string);
procedure LyResultData(P: PLyParam; T: PLyType; Data: pointer);

{ other }

function LyAnsiToStr(const S: AnsiString): string;
procedure LySetAnsiStr(const S: PAnsiChar; Len: integer; Outs: PAnsiString);
procedure LySetWideStr(const S: PWideChar; Len: integer; Outs: PWideString);

var

  __lysee     : PLysee;    // => lysee.my_lyrec
  __module    : PLyModule; // => lysee.TLyModule

  my_system   : PLyModule; // system module
  my_variant  : PLyType;   // => lysee.my_variant
  my_nil      : PLyType;   // => lysee.my_nil
  my_char     : PLyType;   // => lysee.my_char
  my_int      : PLyType;   // => lysee.my_int
  my_float    : PLyType;   // => lysee.my_float
  my_curr     : PLyType;   // => lysee.my_curr
  my_time     : PLyType;   // => lysee.my_time
  my_bool     : PLyType;   // => lysee.my_bool
  my_type     : PLyType;   // => lysee.my_type
  my_exception: PLyType;   // => lysee.my_exception
  my_string   : PLyType;   // => lysee.my_string
  my_module   : PLyType;   // => lysee.my_module
  my_func     : PLyType;   // => lysee.my_func
  my_array    : PLyType;   // => lysee.my_array
  my_list     : PLyType;   // => lysee.my_list
  my_hash     : PLyType;   // => lysee.my_hash
  my_object   : PLyType;
  my_strings  : PLyType;
  my_strlist  : PLyType;
  my_setup    : TLyseeProc;  // setup module procedure

implementation

uses
  Basic;

procedure LyInitModule(Module: PLyModule; Lyrec: PLysee);
var
  P: TLyseeProc;
begin
  try
    __module := Module;
    __lysee := Lyrec;

    my_system    := LyFind(LID_SYSTEM_MODULE);
    my_variant   := LyFind(LID_VARIANT_TYPE);
    my_nil       := LyFind(LID_NIL_TYPE);
    my_char      := LyFind(LID_CHAR_TYPE);
    my_int       := LyFind(LID_INTEGER_TYPE);
    my_float     := LyFind(LID_FLOAT_TYPE);
    my_curr      := LyFind(LID_CURRENCY_TYPE);
    my_time      := LyFind(LID_DATETIME_TYPE);
    my_bool      := LyFind(LID_BOOLEAN_TYPE);
    my_type      := LyFind(LID_TYPE_TYPE);
    my_exception := LyFind(LID_EXCEPTION_TYPE);
    my_string    := LyFind(LID_STRING_TYPE);
    my_module    := LyFind(LID_MODULE_TYPE);
    my_func      := LyFind(LID_FUNC_TYPE);
    my_array     := LyFind(LID_ARRAY_TYPE);
    my_list      := LyFind(LID_LIST_TYPE);
    my_hash      := LyFind(LID_HASH_TYPE);
    my_object    := LyFind(LID_OBJECT_TYPE);
    my_strings   := LyFind(LID_STRINGS_TYPE);
    my_strlist   := LyFind(LID_STRLIST_TYPE);

    if Assigned(my_setup) then
    begin
      P := my_setup;
      my_setup := nil;
      P(nil);
    end;
  except
    { nothing }
  end;
end;

function LyFind(ID: integer): pointer;
begin
  Result := __lysee^.find1(ID);
end;

function LyFind(const Name: string): pointer;
var
  S: AnsiString;
begin
  {$IFDEF UNICODE}
  S := WideToUTF8(Name);
  {$ELSE}
  S := Name;
  {$ENDIF}
  Result := __lysee^.find2(PAnsiChar(S));
end;

function LyFind(const Module, Name: string): pointer;overload;
begin
  Result := LyFind(Module + '.' + Name);
end;

function LyFindType(const Name: string): PLyType;
var
  S: AnsiString;
begin
  {$IFDEF UNICODE}
  S := WideToUTF8(Name);
  {$ELSE}
  S := Name;
  {$ENDIF}
  Result := __lysee^.find_type(PAnsiChar(S));
end;

function LyFindType(const Module, Name: string): PLyType;
begin
  Result := LyFindType(Module + '.' + Name);
end;

function LyAddFunc(const FuncName: AnsiString;
                   const FuncType: PLyType;
                   const Proc: TLyseeProc): PLyFunc;
begin
  Result := LyAddFunc(FuncName, FuncType, [], [], Proc);
end;

function LyAddFunc(const FuncName: AnsiString;
                   const Proc: TLyseeProc): PLyFunc;overload;
begin
  Result := LyAddFunc(FuncName, nil, [], [], Proc);
end;

function LyAddFunc(const FuncName: AnsiString;
                   const FuncType: PLyType;
                   const ParamNames: array of AnsiString;
                   const ParamTypes: array of PLyType;
                   const Proc: TLyseeProc): PLyFunc;
var
  I: integer;
begin
  Result := __lysee^.add_func(__module, PAnsiChar(FuncName), FuncType, Proc);
  if Result <> nil then
    for I := 0 to Length(ParamNames) - 1 do
      __lysee^.add_param(Result, PAnsiChar(ParamNames[I]), ParamTypes[I]);
end;

function LyAddFunc(const FuncName: AnsiString;
                   const ParamNames: array of AnsiString;
                   const ParamTypes: array of PLyType;
                   const Proc: TLyseeProc): PLyFunc;overload;
begin
  Result := LyAddFunc(FuncName, nil, ParamNames, ParamTypes, Proc);
end;

function LyMethod(const Parent: PLyType;
                     const FuncName: AnsiString;
                     const FuncType: PLyType;
                     const Proc: TLyseeProc): PLyFunc;
begin
  Result := LyMethod(Parent, FuncName, FuncType, [], [], Proc);
end;

function LyMethod(const Parent: PLyType;
                     const FuncName: AnsiString;
                     const Proc: TLyseeProc): PLyFunc;
begin
  Result := LyMethod(Parent, FuncName, nil, [], [], Proc);
end;

function LyMethod(const Parent: PLyType;
                     const FuncName: AnsiString;
                     const FuncType: PLyType;
                     const ParamNames: array of AnsiString;
                     const ParamTypes: array of PLyType;
                     const Proc: TLyseeProc): PLyFunc;
var
  I: integer;
begin
  if (Length(ParamNames) = Length(ParamTypes)) and Assigned(Proc) then
  begin
    Result := __lysee^.add_method(Parent, PAnsiChar(FuncName), FuncType, Proc);
    if Result <> nil then
      for I := 0 to Length(ParamNames) - 1 do
        __lysee^.add_param(Result, PAnsiChar(ParamNames[I]), ParamTypes[I]);
  end
  else Result := nil;
end;

function LyMethod(const Parent: PLyType;
                     const FuncName: AnsiString;
                     const ParamNames: array of AnsiString;
                     const ParamTypes: array of PLyType;
                     const Proc: TLyseeProc): PLyFunc;
begin
  Result := LyMethod(Parent, FuncName, nil, ParamNames, ParamTypes, Proc);
end;

function LyDefine(const Parent: PLyType;
                  const PropName: AnsiString;
                  const PropType: PLyType;
                  const GetProc: TLyseeProc;
                  const SetProc: TLyseeProc): boolean;
var
  F: PLyFunc;
begin
  Result := (PropType <> nil) and (PropType <> my_nil) and Assigned(GetProc);
  if Result then
  begin
    F := LyMethod(Parent, PropName, PropType, GetProc);
    Result := (F <> nil);
    if Result and Assigned(SetProc) then
    begin
      F := LyMethod(Parent, 'Set' + PropName, nil, SetProc);
      if F <> nil then
        __lysee^.add_param(F, 'Value', PropType);
    end;
  end;
end;

function LyDefine(const Parent: PLyType;
                  const PropName: AnsiString;
                  const PropType: PLyType;
                  const GetProc: TLyseeProc): boolean;
begin
  Result := LyDefine(Parent, PropName, PropType, GetProc, nil);
end;

function LyDefine(const Parent: PLyType;
                  const PropType: PLyType;
                  const IndexNames: array of AnsiString;
                  const IndexTypes: array of PLyType;
                  const GetProc: TLyseeProc;
                  const SetProc: TLyseeProc): boolean;
var
  F: PLyFunc;
begin
  Result := (PropType <> nil) and (PropType <> my_nil);
  if Result then
  begin
    F := LyMethod(Parent, 'get[]', PropType, IndexNames, IndexTypes, GetProc);
    Result := (F <> nil);
    if Result and Assigned(SetProc) then
    begin
      F := LyMethod(Parent, 'set[]', nil, IndexNames, IndexTypes, SetProc);
      if F <> nil then
        __lysee^.add_param(F, 'Value', PropType);
    end;
  end;
end;

function LyDefine(const Parent: PLyType;
                  const PropType: PLyType;
                  const IndexNames: array of AnsiString;
                  const IndexTypes: array of PLyType;
                  const GetProc: TLyseeProc): boolean;
begin
  Result := LyDefine(Parent, PropType, IndexNames, IndexTypes, GetProc, nil);
end;

{ get param }

function LyParamError(P: PLyParam; const Msg: string): boolean;
var
  S: AnsiString;
begin
  S := StrToUTF8(Msg);
  Result := __lysee^.param_error(P, PAnsiChar(S));
end;

function LyParamCount(P: PLyParam): integer;
begin
  Result := __lysee^.param_count(P);
end;

function LyParamPRMC(P: PLyParam): integer;
begin
  Result := __lysee^.param_prmc(P);
end;

function LyParamType(P: PLyParam; X: integer): PLyType;
begin
  Result := __lysee^.param_type(P, X);
end;

function LyParamInt(P: PLyParam; X: integer): int64;
begin
  Result := __lysee^.param_integer(P, X);
end;

function LyParamBool(P: PLyParam; X: integer): boolean;
begin
  Result := __lysee^.param_boolean(P, X);
end;

function LyParamFloat(P: PLyParam; X: integer): double;
begin
  Result := __lysee^.param_double(P, X);
end;

function LyParamCurr(P: PLyParam; X: integer): currency;
begin
  Result := __lysee^.param_curr(P, X);
end;

function LyParamTime(P: PLyParam; X: integer): TDateTime;
begin
  Result := __lysee^.param_time(P, X);
end;

function LyParamChar(P: PLyParam; X: integer): char;
begin
  {$IFDEF UNICODE}
  Result := __lysee^.param_charW(P, X);
  {$ELSE}
  Result := __lysee^.param_charA(P, X);
  {$ENDIF}
end;

function LyParamAnsiStr(P: PLyParam; X: integer): AnsiString;
begin
  __lysee^.param_ansistr(P, X, @LySetAnsiStr, @Result);
end;

function LyParamWideStr(P: PLyParam; X: integer): WideString;
begin
  __lysee^.param_widestr(P, X, @LySetWideStr, @Result);
end;

function LyParamStr(P: PLyParam; X: integer): string;
begin
  {$IFDEF UNICODE}
  Result := LyParamWideStr(P, X);
  {$ELSE}
  Result := LyParamAnsiStr(P, X);
  {$ENDIF}
end;

function LyParamData(P: PLyParam; X: integer): pointer;
begin
  Result := __lysee^.param_data(P, X);
end;

{ set result }

procedure LyResultInt(P: PLyParam; Value: int64);
begin
  __lysee^.result_integer(P, Value);
end;

procedure LyResultBool(P: PLyParam; Value: boolean);
begin
  __lysee^.result_boolean(P, Value);
end;

procedure LyResultFloat(P: PLyParam; Value: double);
begin
  __lysee^.result_double(P, Value);
end;

procedure LyResultCurr(P: PLyParam; Value: currency);
begin
  __lysee^.result_curr(P, Value);
end;

procedure LyResultTime(P: PLyParam; Value: TDateTime);
begin
  __lysee^.result_time(P, Value);
end;

procedure LyResultChar(P: PLyParam; Value: char);
begin
  {$IFDEF UNICODE}
  __lysee^.result_charW(P, Value);
  {$ELSE}
  __lysee^.result_charA(P, Value);
  {$ENDIF}
end;

procedure LyResultStr(P: PLyParam; Value: string);
begin
  {$IFDEF UNICODE}
  __lysee^.result_pcharW(P, pchar(Value), Length(Value));
  {$ELSE}
  __lysee^.result_pcharA(P, pchar(Value), Length(Value));
  {$ENDIF}
end;

procedure LyResultData(P: PLyParam; T: PLyType; Data: pointer);
begin
  __lysee^.result_data(P, T, Data);
end;

{ other }

function LyAnsiToStr(const S: AnsiString): string;
begin
  {$IFDEF UNICODE}
  Result := UTF8ToStr(S);
  {$ELSE}
  Result := S;
  {$ENDIF};
end;

procedure LySetAnsiStr(const S: PAnsiChar; Len: integer; Outs: PAnsiString);
begin
  if Outs <> nil then
    if (Len > 0) and (S <> nil) and (S^ <> #0) then
    begin
      SetLength(Outs^, Len);
      Move(S[0], Outs^[1], Len);
    end
    else Outs^ := '';
end;

procedure LySetWideStr(const S: PWideChar; Len: integer; Outs: PWideString);
begin
  if Outs <> nil then
    if (Len > 0) and (S <> nil) and (S^ <> #0) then
    begin
      SetLength(Outs^, Len);
      Move(S[0], Outs^[1], Len * sizeof(WideChar));
    end
    else Outs^ := '';
end;

end.
