{==============================================================================}
{     PROJECT: lysee_math                                                      }
{ DESCRIPTION: mathmetics functions                                            }
{   COPYRIGHT: Copyright (c) 2003-2012, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2007/07/12                                                      }
{    MODIFIED: 2013/09/16                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lmmath;

{$mode objfpc}{$H+}

interface

  function abs(d: double): double;
  function sqr(d: double): double;
  function sqrt(d: double): double;
  function arctan(d: double): double;
  function ln(d: double): double;
  function sin(d: double): double;
  function cos(d: double): double;
  function exp(d: double): double;
  function round(d: double): int64;
  function frac(d: double): double;
  function int(d: double): double;
  function trunc(d: double): int64;
  function InRange(const AValue, AMin, AMax: int64): boolean;
  function EnsureRange(const AValue, AMin, AMax: int64): int64;
  function IsZero(const A: double): boolean;
  function IsNan(const d: double): boolean;
  function IsInfinite(const d: double): boolean;
  function SameValue(const A, B: double): boolean;
  function RoundTo(const AValue: double; const Digits: integer): double;
  function SimpleRoundTo(const AValue: double; const Digits: integer): double;
  function degtorad(deg: float): float;
  function radtodeg(rad: float): float;
  function gradtorad(grad: float): float;
  function radtograd(rad: float): float;
  function degtograd(deg: float): float;
  function gradtodeg(grad: float): float;
  function cycletorad(cycle: float): float;
  function radtocycle(rad: float): float;
  function tan(x: float): float;
  function cotan(x: float): float;
  function cot(x: float): float;
  function secant(x: float): float;
  function cosecant(x: float): float;
  function sec(x: float): float;
  function csc(x: float): float;
  function arccos(x: float): float;
  function arcsin(x: float): float;
  function arctan2(y,x: float): float;
  function cosh(x: float): float;
  function sinh(x: float): float;
  function tanh(x: float): float;
  function arccosh(x: float): float;
  function arcsinh(x: float): float;
  function arctanh(x: float): float;
  function arcosh(x: float): float;
  function arsinh(x: float): float;
  function artanh(x: float): float;
  function hypot(x,y: float): float;
  function log10(x: float): float;
  function log2(x: float): float;
  function logn(n,x: float): float;
  function lnxp1(x: float): float;
  function power(base,exponent: float): float;
  function intpower(base: float;const exponent: integer): float;
  function ceil(x: float): integer;
  function floor(x: float): integer;
  function ldexp(x: float; const p: integer): float;
  function randg(mean,stddev: float): float;
  function RandomRange(const aFrom, aTo: integer): integer;

implementation

end.
