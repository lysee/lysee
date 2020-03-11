library lysee_math;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Math, ilysee;

procedure pp_sqr(const P: PLyParam);
begin
  LyResultFloat(P, sqr(LyParamFloat(P, 0)));
end;

procedure pp_sqrt(const P: PLyParam);
begin
  LyResultFloat(P, sqrt(LyParamFloat(P, 0)));
end;

procedure pp_arctan(const P: PLyParam);
begin
  LyResultFloat(P, arctan(LyParamFloat(P, 0)));
end;

procedure pp_ln(const P: PLyParam);
begin
  LyResultFloat(P, ln(LyParamFloat(P, 0)));
end;

procedure pp_sin(const P: PLyParam);
begin
  LyResultFloat(P, sin(LyParamFloat(P, 0)));
end;

procedure pp_cos(const P: PLyParam);
begin
  LyResultFloat(P, cos(LyParamFloat(P, 0)));
end;

procedure pp_exp(const P: PLyParam);
begin
  LyResultFloat(P, exp(LyParamFloat(P, 0)));
end;

procedure pp_frac(const P: PLyParam);
begin
  LyResultFloat(P, frac(LyParamFloat(P, 0)));
end;

procedure pp_int(const P: PLyParam);
begin
  LyResultFloat(P, int(LyParamFloat(P, 0)));
end;

procedure pp_InRange(const P: PLyParam);
begin
  LyResultBool(P, InRange(LyParamInt(P, 0), LyParamInt(P, 1), LyParamInt(P, 2)));
end;

procedure pp_EnsureRange(const P: PLyParam);
begin
  LyResultInt(P, EnsureRange(LyParamInt(P, 0), LyParamInt(P, 1), LyParamInt(P, 2)));
end;

procedure pp_IsZero(const P: PLyParam);
begin
  LyResultBool(P, IsZero(LyParamFloat(P, 0)));
end;

procedure pp_IsNan(const P: PLyParam);
begin
  LyResultBool(P, IsNan(LyParamFloat(P, 0)));
end;

procedure pp_IsInfinite(const P: PLyParam);
begin
  LyResultBool(P, IsInfinite(LyParamFloat(P, 0)));
end;

procedure pp_SameValue(const P: PLyParam);
begin
  LyResultBool(P, SameValue(LyParamFloat(P, 0), LyParamFloat(P, 1)));
end;

procedure pp_RoundTo(const P: PLyParam);
begin
  LyResultFloat(P, RoundTo(LyParamFloat(P, 0), LyParamInt(P, 1)));
end;

procedure pp_SimpleRoundTo(const P: PLyParam);
begin
  LyResultFloat(P, SimpleRoundTo(LyParamFloat(P, 0), LyParamInt(P, 1)));
end;

procedure pp_degtorad(const P: PLyParam);
begin
  LyResultFloat(P, degtorad(LyParamFloat(P, 0)));
end;

procedure pp_radtodeg(const P: PLyParam);
begin
  LyResultFloat(P, radtodeg(LyParamFloat(P, 0)));
end;

procedure pp_gradtorad(const P: PLyParam);
begin
  LyResultFloat(P, gradtorad(LyParamFloat(P, 0)));
end;

procedure pp_radtograd(const P: PLyParam);
begin
  LyResultFloat(P, radtograd(LyParamFloat(P, 0)));
end;

procedure pp_degtograd(const P: PLyParam);
begin
  LyResultFloat(P, degtograd(LyParamFloat(P, 0)));
end;

procedure pp_gradtodeg(const P: PLyParam);
begin
  LyResultFloat(P, gradtodeg(LyParamFloat(P, 0)));
end;

procedure pp_cycletorad(const P: PLyParam);
begin
  LyResultFloat(P, cycletorad(LyParamFloat(P, 0)));
end;

procedure pp_radtocycle(const P: PLyParam);
begin
  LyResultFloat(P, radtocycle(LyParamFloat(P, 0)));
end;

procedure pp_tan(const P: PLyParam);
begin
  LyResultFloat(P, tan(LyParamFloat(P, 0)));
end;

procedure pp_cotan(const P: PLyParam);
begin
  LyResultFloat(P, cotan(LyParamFloat(P, 0)));
end;

procedure pp_cot(const P: PLyParam);
begin
  LyResultFloat(P, cot(LyParamFloat(P, 0)));
end;

procedure pp_secant(const P: PLyParam);
begin
  LyResultFloat(P, secant(LyParamFloat(P, 0)));
end;

procedure pp_cosecant(const P: PLyParam);
begin
  LyResultFloat(P, cosecant(LyParamFloat(P, 0)));
end;

procedure pp_sec(const P: PLyParam);
begin
  LyResultFloat(P, sec(LyParamFloat(P, 0)));
end;

procedure pp_csc(const P: PLyParam);
begin
  LyResultFloat(P, csc(LyParamFloat(P, 0)));
end;

procedure pp_arccos(const P: PLyParam);
begin
  LyResultFloat(P, arccos(LyParamFloat(P, 0)));
end;

procedure pp_arcsin(const P: PLyParam);
begin
  LyResultFloat(P, arcsin(LyParamFloat(P, 0)));
end;

procedure pp_arctan2(const P: PLyParam);
begin
  LyResultFloat(P, arctan2(LyParamFloat(P, 0), LyParamFloat(P, 1)));
end;

procedure pp_cosh(const P: PLyParam);
begin
  LyResultFloat(P, cosh(LyParamFloat(P, 0)));
end;

procedure pp_sinh(const P: PLyParam);
begin
  LyResultFloat(P, sinh(LyParamFloat(P, 0)));
end;

procedure pp_tanh(const P: PLyParam);
begin
  LyResultFloat(P, tanh(LyParamFloat(P, 0)));
end;

procedure pp_arccosh(const P: PLyParam);
begin
  LyResultFloat(P, arccosh(LyParamFloat(P, 0)));
end;

procedure pp_arcsinh(const P: PLyParam);
begin
  LyResultFloat(P, arcsinh(LyParamFloat(P, 0)));
end;

procedure pp_arctanh(const P: PLyParam);
begin
  LyResultFloat(P, arctanh(LyParamFloat(P, 0)));
end;

procedure pp_hypot(const P: PLyParam);
begin
  LyResultFloat(P, hypot(LyParamFloat(P, 0), LyParamFloat(P, 1)));
end;

procedure pp_log10(const P: PLyParam);
begin
  LyResultFloat(P, log10(LyParamFloat(P, 0)));
end;

procedure pp_log2(const P: PLyParam);
begin
  LyResultFloat(P, log2(LyParamFloat(P, 0)));
end;

procedure pp_logn(const P: PLyParam);
begin
  LyResultFloat(P, logn(LyParamFloat(P, 0), LyParamFloat(P, 1)));
end;

procedure pp_lnxp1(const P: PLyParam);
begin
  LyResultFloat(P, lnxp1(LyParamFloat(P, 0)));
end;

procedure pp_power(const P: PLyParam);
begin
  LyResultFloat(P, power(LyParamFloat(P, 0), LyParamFloat(P, 1)));
end;

procedure pp_intpower(const P: PLyParam);
begin
  LyResultFloat(P, intpower(LyParamFloat(P, 0), LyParamInt(P, 1)));
end;

procedure pp_ldexp(const P: PLyParam);
begin
  LyResultFloat(P, ldexp(LyParamFloat(P, 0), LyParamInt(P, 1)));
end;

procedure pp_randg(const P: PLyParam);
begin
  LyResultFloat(P, randg(LyParamFloat(P, 0), LyParamFloat(P, 1)));
end;

procedure pp_RandomRange(const P: PLyParam);
begin
  LyResultInt(P, RandomRange(LyParamInt(P, 0), LyParamInt(P, 1)));
end;

procedure SetupILysee(const P: PLyParam);
begin
  LyAddFunc('sqr', my_float, ['d'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_sqr);
  LyAddFunc('sqrt', my_float, ['d'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_sqrt);
  LyAddFunc('arctan', my_float, ['d'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_arctan);
  LyAddFunc('ln', my_float, ['d'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_ln);
  LyAddFunc('sin', my_float, ['d'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_sin);
  LyAddFunc('cos', my_float, ['d'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_cos);
  LyAddFunc('exp', my_float, ['d'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_exp);
  LyAddFunc('frac', my_float, ['d'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_frac);
  LyAddFunc('int', my_float, ['d'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_int);
  LyAddFunc('InRange', my_bool, ['AValue', 'AMin', 'AMax'], [my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_InRange);
  LyAddFunc('EnsureRange', my_int, ['AValue', 'AMin', 'AMax'], [my_int, my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_EnsureRange);
  LyAddFunc('IsZero', my_bool, ['A'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_IsZero);
  LyAddFunc('IsNan', my_bool, ['d'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_IsNan);
  LyAddFunc('IsInfinite', my_bool, ['d'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_IsInfinite);
  LyAddFunc('SameValue', my_bool, ['A', 'B'], [my_float, my_float],
            {$IFDEF FPC}@{$ENDIF}pp_SameValue);
  LyAddFunc('RoundTo', my_float, ['AValue', 'Digits'], [my_float, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_RoundTo);
  LyAddFunc('SimpleRoundTo', my_float, ['AValue', 'Digits'], [my_float, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_SimpleRoundTo);
  LyAddFunc('degtorad', my_float, ['deg'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_degtorad);
  LyAddFunc('radtodeg', my_float, ['rad'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_radtodeg);
  LyAddFunc('gradtorad', my_float, ['grad'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_gradtorad);
  LyAddFunc('radtograd', my_float, ['rad'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_radtograd);
  LyAddFunc('degtograd', my_float, ['deg'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_degtograd);
  LyAddFunc('gradtodeg', my_float, ['grad'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_gradtodeg);
  LyAddFunc('cycletorad', my_float, ['cycle'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_cycletorad);
  LyAddFunc('radtocycle', my_float, ['rad'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_radtocycle);
  LyAddFunc('tan', my_float, ['x'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_tan);
  LyAddFunc('cotan', my_float, ['x'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_cotan);
  LyAddFunc('cot', my_float, ['x'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_cot);
  LyAddFunc('secant', my_float, ['x'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_secant);
  LyAddFunc('cosecant', my_float, ['x'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_cosecant);
  LyAddFunc('sec', my_float, ['x'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_sec);
  LyAddFunc('csc', my_float, ['x'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_csc);
  LyAddFunc('arccos', my_float, ['x'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_arccos);
  LyAddFunc('arcsin', my_float, ['x'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_arcsin);
  LyAddFunc('arctan2', my_float, ['y', 'x'], [my_float, my_float],
            {$IFDEF FPC}@{$ENDIF}pp_arctan2);
  LyAddFunc('cosh', my_float, ['x'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_cosh);
  LyAddFunc('sinh', my_float, ['x'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_sinh);
  LyAddFunc('tanh', my_float, ['x'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_tanh);
  LyAddFunc('arccosh', my_float, ['x'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_arccosh);
  LyAddFunc('arcsinh', my_float, ['x'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_arcsinh);
  LyAddFunc('arctanh', my_float, ['x'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_arctanh);
  LyAddFunc('hypot', my_float, ['x', 'y'], [my_float, my_float],
            {$IFDEF FPC}@{$ENDIF}pp_hypot);
  LyAddFunc('log10', my_float, ['x'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_log10);
  LyAddFunc('log2', my_float, ['x'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_log2);
  LyAddFunc('logn', my_float, ['n', 'x'], [my_float, my_float],
            {$IFDEF FPC}@{$ENDIF}pp_logn);
  LyAddFunc('lnxp1', my_float, ['x'], [my_float],
            {$IFDEF FPC}@{$ENDIF}pp_lnxp1);
  LyAddFunc('power', my_float, ['base', 'exponent'], [my_float, my_float],
            {$IFDEF FPC}@{$ENDIF}pp_power);
  LyAddFunc('intpower', my_float, ['base', 'exponent'], [my_float, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_intpower);
  LyAddFunc('ldexp', my_float, ['x', 'p'], [my_float, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_ldexp);
  LyAddFunc('randg', my_float, ['mean', 'stddev'], [my_float, my_float],
            {$IFDEF FPC}@{$ENDIF}pp_randg);
  LyAddFunc('RandomRange', my_int, ['aFrom', 'aTo'], [my_int, my_int],
            {$IFDEF FPC}@{$ENDIF}pp_RandomRange);
end;

exports
  ilysee.LyInitModule;

begin
  ilysee.my_setup := @SetupILysee;
end.

