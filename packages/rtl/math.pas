{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2017 by Mattias Gaertner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Math;

{$mode objfpc}

interface

const
  MinInteger = -$fffffffffffff-1;
  MaxInteger = $fffffffffffff;
  MinDouble  =  5.0e-324;
  MaxDouble  =  1.7e+308;

const
  NaN: Double; external name 'NaN';
  Infinity: Double; external name 'Infinity';
  NegInfinity: Double; external name '-Infinity';

type
  float = double;

  //EInvalidArgument = class(EMathError);

function Min(const a, b: Double): Double; varargs; external name 'Math.min'; overload;
function Max(const a, b: Double): Double; varargs; external name 'Math.max'; overload;
function Min(const a, b: NativeLargeUInt): NativeLargeUInt; varargs; external name 'Math.min'; overload;
function Max(const a, b: NativeLargeUInt): NativeLargeUInt; varargs; external name 'Math.max'; overload;
function Min(const a, b: NativeLargeInt): NativeLargeInt; varargs; external name 'Math.min'; overload;
function Max(const a, b: NativeLargeInt): NativeLargeInt; varargs; external name 'Math.max'; overload;
function Min(const a, b: Integer): Integer; varargs; external name 'Math.min'; overload;
function Max(const a, b: Integer): Integer; varargs; external name 'Math.max'; overload;

function InRange(const AValue, AMin, AMax: Integer): Boolean; assembler; overload;
function InRange(const AValue, AMin, AMax: Double): Boolean; assembler; overload;

function EnsureRange(const AValue, AMin, AMax: Integer): Integer; assembler; overload;
function EnsureRange(const AValue, AMin, AMax: Double): Double; assembler; overload;

type
  TRoundToRange = -37..37;

function RoundTo(const AValue: Double; const Digits: TRoundToRange): Double;
function SimpleRoundTo(const AValue: Double; const Digits: TRoundToRange = -2): Double;

function randg(mean,stddev : float) : float;
function RandomRange(const aFrom, aTo: Integer): Integer;
function RandomRange(const aFrom, aTo: NativeLargeInt): NativeLargeInt;

const
  NegativeValue = -1;
  ZeroValue = 0;
  PositiveValue = 1;

function Sign(const AValue: Integer): Integer; external name 'Math.sign'; overload;
function Sign(const AValue: Double): Double; external name 'Math.sign'; overload;

function IsZero(const d: Double; Epsilon: Double): Boolean; overload;
function IsZero(const d: Double): Boolean; overload;

function IsNaN(const v: JSValue): boolean; external name {$IFDEF ECMAScript5}'isNaN'{$ELSE}'Number.isNaN'{$ENDIF}; overload;
function IsFinite(const d: JSValue): Boolean; external name 'isFinite'; overload;// false if NaN, positive or negative infinity
function IsInfinite(const d: JSValue): Boolean; assembler; overload; // negative or positive infinity
{$IFDEF ECMAScript6}
function IsInteger(const d: JSValue): Boolean; external name 'Number.isInteger'; // any integer representable by a double
function IsSafeInteger(const d: JSValue): Boolean; external name 'Number.isSafeInteger'; // an integer between MinInteger and MaxInteger, inclusive
{$ENDIF}

function SameValue(const A, B: Double; Epsilon: Double = 0.0): Boolean; overload;

// function Abs is in System.pas
function ArcCos(const A : Double): Double; external name 'Math.acos';
function ArcCosH(const A: Double): Double; external name 'Math.acosh'; // not on IE
function ArcSin(const A : Double): Double; external name 'Math.asin';
function ArcSinH(const A : Double): Double; external name 'Math.asinh'; // not on IE
function ArcTanH(const A: Double): Double; external name 'Math.atanh'; // not on IE
function CosH(const A: Double): Double; external name 'Math.cosh'; // not on IE
function ExpM1(const A: Double): Double; external name 'Math.expm1'; // not on IE
function FRound(const A: Double): Double; overload; external name 'Math.fround'; // not on IE
function FTrunc(const A: Double): double; overload; external name 'Math.trunc'; // not on IE
function Hypot(const A : Double): Double; varargs; external name 'Math.hypot'; // not on IE
function IMul(const A, B: Integer): Integer; external name 'Math.imul'; // not on IE
function Log10(const A: Double): Double; external name 'Math.log10';
function Log1p(const A: Double): Double; external name 'Math.log1p'; // not on IE
function Log2(const A: Double): Double; external name 'Math.log2'; // not on IE
function LogN(const A, Base: Double): Double; 
function Power(const Base, Exponent: Double): Double; external name 'Math.pow';
// ln, round, sqrt, trunc, cos, sin, arctan, round, exp are in unit system
function Ceil(const A: Double): Integer;
function Floor(const A: Double): Integer;
function Ceil64(const A: Double): NativeLargeInt;
function Floor64(const A: Double): NativeLargeInt;
function ldexp(x : double;const p : Integer) : double;
procedure Frexp(X: double; out Mantissa: double; out Exponent: integer);
function lnxp1(x : double) : double;

function IntPower(base : float;const exponent : Integer) : double;

procedure DivMod(Dividend: LongInt; Divisor: Word; out Result, Remainder: Word);
procedure DivMod(Dividend: LongInt; Divisor: Word; out Result, Remainder: SmallInt);
procedure DivMod(Dividend: DWord; Divisor: DWord; out Result, Remainder: DWord);
procedure DivMod(Dividend: LongInt; Divisor: LongInt; out Result, Remainder: LongInt);

{ Angle conversion }
function DegToRad(deg : double) : double;
function RadToDeg(rad : double) : double;
function GradToRad(grad : double) : double;
function RadToGrad(rad : double) : double;
function DegToGrad(deg : double) : double;
function GradToDeg(grad : double) : double;
{ one cycle are 2*Pi rad }
function CycleToRad(cycle : double) : double;
function RadToCycle(rad : double) : double;
Function DegNormalize(deg : double) : double;

function Norm(const data : array of double) : double;

// Statistical functions
function Mean(const data : array of double) : double;
function Sum(const data : array of double) : double;
procedure SumsAndSquares(const data : Array of Double; out Sum,SumOfSquares : double);

function StdDev(const data : array of Double) : float;
procedure MeanAndStdDev(const data : array of Double; out Mean,StdDev : double);
function Variance(const data : array of Double) : double;
function TotalVariance(const data : array of Double) : double;
function PopNStdDev(const data : array of Double) : double;
function PopNVariance(const data : array of Double) : double;
procedure MomentSkewKurtosis(const data : array of Double; out m1,m2,m3,m4,skew,kurtosis : double);

// Financial functions

Type
  TPaymentTime = (ptEndOfPeriod,ptStartOfPeriod);

function FutureValue(ARate: double; NPeriods: Integer;
  APayment, APresentValue: double; APaymentTime: TPaymentTime): double;

function InterestRate(NPeriods: Integer; APayment, APresentValue, AFutureValue: double;
  APaymentTime: TPaymentTime): double;

function NumberOfPeriods(ARate, APayment, APresentValue, AFutureValue: double;
  APaymentTime: TPaymentTime): double;

function Payment(ARate: double; NPeriods: Integer;
  APresentValue, AFutureValue: double; APaymentTime: TPaymentTime): double;

function PresentValue(ARate: double; NPeriods: Integer;
  APayment, AFutureValue: double; APaymentTime: TPaymentTime): double;

// Miscellaneous

function IfThen(val:boolean;const ifTrue:integer; const ifFalse:integer= 0) :integer; overload;
function IfThen(val:boolean;const ifTrue:double ; const ifFalse:double =0.0):double; overload;

Type
  TValueRelationship = -1..1;

const
  EqualsValue = 0;
  LessThanValue = Low(TValueRelationship);
  GreaterThanValue = High(TValueRelationship);

function CompareValue ( const A, B  : Integer) : TValueRelationship;
function CompareValue ( const A, B  : NativeLargeInt) : TValueRelationship;
function CompareValue ( const A, B  : NativeLargeUInt) : TValueRelationship;
function CompareValue ( const A, B : Double; delta : Double = 0.0) : TValueRelationship;

implementation

function InRange(const AValue, AMin, AMax: Integer): Boolean; assembler;
asm
  return (AValue >= AMin) && (AValue <= AMax);
end;

function InRange(const AValue, AMin, AMax: Double): Boolean; assembler;
asm
  return (AValue >= AMin) && (AValue <= AMax);
end;

function EnsureRange(const AValue, AMin, AMax: Integer): Integer; assembler;
asm
  if (AValue<AMin){ return AMin;
  } else if (AValue>AMax){ return AMax;
  } else return AValue;
end;

function EnsureRange(const AValue, AMin, AMax: Double): Double; assembler;
asm
  if (AValue<AMin){ return AMin;
  } else if (AValue>AMax){ return AMax;
  } else return AValue;
end;

function RoundTo(const AValue: Double; const Digits: TRoundToRange): Double;
var
  RV : Double;

begin
  RV:=IntPower(10,Digits);
  Result:=Round(AValue/RV)*RV;
end;

function SimpleRoundTo(const AValue: Double; const Digits: TRoundToRange): Double;
var
  RV : Double;

begin
  RV := IntPower(10, -Digits);
  if AValue < 0 then
    Result := Int((AValue*RV) - 0.5)/RV
  else
    Result := Int((AValue*RV) + 0.5)/RV;
end;

function randg(mean,stddev : float) : float;

Var
  U1,S2 : Float;

begin
  repeat
    u1:= 2*random-1;
    S2:=Sqr(U1)+sqr(2*random-1);
  until s2<1;
  Result:=Sqrt(-2*ln(S2)/S2)*u1*stddev+Mean;
end;


function RandomRange(const aFrom, aTo: Integer): Integer;
begin
  Result:=Random(Abs(aFrom-aTo))+Min(aTo,AFrom);
end;


function RandomRange(const aFrom, aTo: NativeLargeInt): NativeLargeInt;

Var
  m : NativeLargeInt;
begin
  if aFrom<aTo then
    M:=aFrom
  else
    M:=aTo;
  Result:=Random(Abs(aFrom-aTo))+M;
end;

const
  DZeroResolution = 1E-12;

function IsZero(const d: Double; Epsilon: Double): Boolean;
begin
  if Epsilon=0 then
    Epsilon:=DZeroResolution;
  Result:=Abs(d)<=Epsilon;
end;

function IsZero(const d: Double): Boolean;
begin
  Result:=Abs(d)<=DZeroResolution;
end;

function IsInfinite(const d: JSValue): Boolean; assembler;
asm
  return (d==Infinity) || (d==-Infinity);
end;

function SameValue(const A, B: Double; Epsilon: Double): Boolean;
begin
  if (Epsilon=0.0) then
    Epsilon:=Max(Min(Abs(A),Abs(B))*DZeroResolution,DZeroResolution);
  if (A>B) then
    Result:=((A-B)<=Epsilon)
  else
    Result:=((B-A)<=Epsilon);
end;

function JSFloor(const A: Double): Double; external name 'Math.floor';
function JSCeil(const A: Double): Double; external name 'Math.ceil';

function Ceil(const A: Double): Integer;

begin
  // TODO: add Range check ?
  Result:=trunc(JSCeil(a));
end;

function Floor(const A: Double): Integer;

begin
  // TODO: add Range check ?
  Result:=trunc(JSFloor(a));
end;

function Ceil64(const A: Double): NativeLargeInt;

begin
  Result:=trunc(JSCeil(a));
end;

function Floor64(const A: Double): NativeLargeInt;

begin
  Result:=trunc(JSCeil(a));
end;

procedure Frexp(X: double; out Mantissa: double; out Exponent: integer);

begin
  Exponent:=0;
  if (X<>0) then
    if (abs(X)<0.5) then
      repeat
        X:=X*2;
        Dec(Exponent);
      until (abs(X)>=0.5)
    else
      while (abs(X)>=1) do
        begin
        X:=X/2;
        Inc(Exponent);
        end;
  Mantissa:=X;
end;

function LogN(const A, Base: Double): Double; 

begin
  Result:=Ln(A)/Ln(Base);
end;

function lnxp1(x: double): double;

var
  y: float;

begin
  if (x>=4.0) then
    result:=ln(1.0+x)
  else
    begin
    y:=1.0+x;
    if (y=1.0) then
      result:=x
    else
      begin
      result:=ln(y);     { lnxp1(-1) = ln(0) = -Inf }
      if y>0.0 then
        result:=result+(x-(y-1.0))/y;
      end;
    end;
end;

function ldexp(x : double;const p : Integer) : double;

begin
   result:=x*intpower(2.0,p);
end;


function IntPower(base: float; const exponent: Integer): double;

var
  i : longint;

begin
  if (base = 0.0) and (exponent = 0) then
    result:=1
  else
    begin
    i:=abs(exponent);
    Result:=1.0;
    while i>0 do
      begin
      while (i and 1)=0 do
        begin
        i:=i shr 1;
        base:=sqr(base);
        end;
      i:=i-1;
      Result:=Result*base;
      end;
    if exponent<0 then
      Result:=1.0/Result;
    end;
end;

procedure DivMod(Dividend: LongInt; Divisor: Word; out Result, Remainder: Word);
begin
  if Dividend < 0 then
    begin
    Dividend:=-Dividend;
    Result:=-(Dividend Div Divisor);
    Remainder:=-(Dividend+(Result*Divisor));
    end
  else
    begin
    Result:=Dividend Div Divisor;
    Remainder:=Dividend-(Result*Divisor);
    end;
end;


procedure DivMod(Dividend: LongInt; Divisor: Word; out Result,
  Remainder: SmallInt);
begin
  if Dividend < 0 then
    begin
    Dividend:=-Dividend;
    Result:=-(Dividend Div Divisor);
    Remainder:=-(Dividend+(Result*Divisor));
    end
  else
    begin
    Result:=Dividend Div Divisor;
    Remainder:=Dividend-(Result*Divisor);
    end;
end;


procedure DivMod(Dividend: DWord; Divisor: DWord; out Result, Remainder: DWord);
begin
  Result:=Dividend Div Divisor;
  Remainder:=Dividend-(Result*Divisor);
end;


procedure DivMod(Dividend: LongInt; Divisor: LongInt; out Result,
  Remainder: LongInt);

begin
  if Dividend < 0 then
    begin
    Dividend:=-Dividend;
    Result:=-(Dividend Div Divisor);
    Remainder:=-(Dividend+(Result*Divisor));
    end
  else
    begin
    Result:=Dividend Div Divisor;
    Remainder:=Dividend-(Result*Divisor);
    end;
end;

{ ---------------------------------------------------------------------
  Angle conversion
  ---------------------------------------------------------------------}
function DegToRad(deg: double): double;

begin
  Result:=deg*(pi/180.0);
end;


function RadToDeg(rad: double): double;

begin
  Result:=rad*(180.0/pi);
end;


function GradToRad(grad: double): double;

begin
  Result:=grad*(pi/200.0);
end;


function RadToGrad(rad: double): double;

begin
  Result:=rad*(200.0/pi);
end;


function DegToGrad(deg: double): double;

begin
  Result:=deg*(200.0/180.0);
end;


function GradToDeg(grad: double): double;

begin
  Result:=grad*(180.0/200.0);
end;


function CycleToRad(cycle: double): double;

begin
  Result:=(2*pi)*cycle;
end;


function RadToCycle(rad: double): double;

begin
  Result:=rad*(1/(2*pi));
end;


function DegNormalize(deg: double): double;

begin
  Result:=Deg-Int(Deg/360)*360;
  If (Result<0) then Result:=Result+360;
end;

function sumofsquares(const data : array of double) : double;
var
  i,N : longint;

begin
  N:=Length(Data);
  Result:=0.0;
  for i:=0 to N-1 do
    Result:=Result+sqr(data[i]);
end;

function Norm(const data: array of double): double;
begin
  Result:=sqrt(sumofsquares(data));
end;

{ ---------------------------------------------------------------------
  Statistical functions
  ---------------------------------------------------------------------}

function Sum(const data: array of double): double;

var
  i,N : longint;

begin
  N:=Length(Data);
  Result:=0.0;
  for i:=0 to N-1 do
    Result:=Result+data[i];
end;

function Mean(const data: array of double): double;

Var
  N : integer;

begin
  N:=Length(Data);
  if N=0 then
    Result:=0
  else
    Result:=Sum(Data)/N;
end;
procedure SumsAndSquares(const data: array of Double; out Sum,
  SumOfSquares: double);

var
  i,n : Integer;
  t,s,ss: double;

begin
  n:=length(Data);
  ss:=0.0; // Use local vars, var is very inefficient in js
  s:=0.0;
  for i:=0 to N-1 do
    begin
    t:=data[i];
    ss:=ss+sqr(t);
    s:=s+t;
    end;
  Sum:=s;
  SumOfSquares:=ss;
end;

function StdDev(const data: array of Double): float;

begin
  Result:=Sqrt(Variance(Data));
end;

function Variance(const data: array of Double): double;

var
  n : integer;

begin
  N:=Length(Data);
  If N=1 then
    Result:=0
  else
    Result:=TotalVariance(Data)/(N-1);
end;

function TotalVariance(const data: array of Double): double;

var
  S,SS : Float;
  N : integer;
begin
  N:=Length(Data);
  If Length(Data)=1 then
    Result:=0
  else
    begin
    SumsAndSquares(Data,S,SS);
    Result := SS-Sqr(S)/N;
    end;
end;

procedure MeanAndStdDev(const data: array of Double; out Mean, StdDev: double);

Var
  I,N : longint;
  M,S : Double;

begin
  N:=Length(Data);
  M:=0;
  S:=0;
  For I:=0 to N-1 do
    begin
    M:=M+Data[i];
    S:=S+Sqr(Data[i]);
    end;
  M:=M/N;
  S:=(S-N*Sqr(M));
  If N>1 then
    S:=Sqrt(S/(N-1))
  else
    S:=0;
  Mean:=M;
  StdDev:=S;
end;

function PopNStdDev(const data : array of Double) : double;

begin
  Result:=Sqrt(PopnVariance(Data));
end;

function PopNVariance(const data : array of Double) : double;

Var
  N : integer;

begin
  N:=Length(Data);
  if N=0 then
    Result:=0
  else
    Result:=TotalVariance(Data)/N;
end;

procedure MomentSkewKurtosis(const data: array of Double; out m1, m2, m3, m4, skew, kurtosis: double);

var
  i,N: integer;
  deviation, deviation2: double;
  reciprocalN: float;
  // Use local vars for all calculations, var is very slow
  lm1, lm2, lm3, lm4, lskew, lkurtosis: double;

begin
  N:=length(Data);
  lm1 := 0;
  reciprocalN := 1/N;
  for i := 0 to N-1 do
    lm1 := lm1 + data[i];
  lm1 := reciprocalN * lm1;

  lm2 := 0;
  lm3 := 0;
  lm4 := 0;
  for i := 0 to N-1 do
    begin
    deviation := (data[i]-lm1);
    deviation2 := deviation * deviation;
    lm2 := lm2 + deviation2;
    lm3 := lm3 + deviation2 * deviation;
    lm4 := lm4 + deviation2 * deviation2;
    end;
  lm2 := reciprocalN * lm2;
  lm3 := reciprocalN * lm3;
  lm4 := reciprocalN * lm4;

  lskew := lm3 / (sqrt(lm2)*lm2);
  lkurtosis := lm4 / (lm2 * lm2);

  m1:=lm1;
  m2:=lm2;
  m3:=lm3;
  m4:=lm4;
  skew:=lskew;
  kurtosis:=lkurtosis;
end;

{ ---------------------------------------------------------------------
  Financial functions
  ---------------------------------------------------------------------}

function FutureValue(ARate: double; NPeriods: Integer;
  APayment, APresentValue: double; APaymentTime: TPaymentTime): double;
var
  q, qn, factor: double;
begin
  if ARate = 0 then
    Result := -APresentValue - APayment * NPeriods
  else begin
    q := 1.0 + ARate;
    qn := power(q, NPeriods);
    factor := (qn - 1) / (q - 1);
    if APaymentTime = ptStartOfPeriod then
      factor := factor * q;
    Result := -(APresentValue * qn + APayment*factor);
  end;
end;

function InterestRate(NPeriods: Integer; APayment, APresentValue, AFutureValue: double;
  APaymentTime: TPaymentTime): double;
{ The interest rate cannot be calculated analytically. We solve the equation
  numerically by means of the Newton method:
  - guess value for the interest reate
  - calculate at which interest rate the tangent of the curve fv(rate)
    (straight line!) has the requested future vale.
  - use this rate for the next iteration. }
const
  DELTA = 0.001;
  EPS = 1E-9;   // required precision of interest rate (after typ. 6 iterations)
  MAXIT = 20;   // max iteration count to protect agains non-convergence
var
  r1, r2, dr: double;
  fv1, fv2: double;
  iteration: Integer;
begin
  iteration := 0;
  r1 := 0.05;  // inital guess
  repeat
    r2 := r1 + DELTA;
    fv1 := FutureValue(r1, NPeriods, APayment, APresentValue, APaymentTime);
    fv2 := FutureValue(r2, NPeriods, APayment, APresentValue, APaymentTime);
    dr := (AFutureValue - fv1) / (fv2 - fv1) * delta;  // tangent at fv(r)
    r1 := r1 + dr;      // next guess
    inc(iteration);
  until (abs(dr) < EPS) or (iteration >= MAXIT);
  Result := r1;
end;

function NumberOfPeriods(ARate, APayment, APresentValue, AFutureValue: double;
  APaymentTime: TPaymentTime): double;
{ Solve the cash flow equation (1) for q^n and take the logarithm }
var
  q, x1, x2: double;
begin
  if ARate = 0 then
    Result := -(APresentValue + AFutureValue) / APayment
  else begin
    q := 1.0 + ARate;
    if APaymentTime = ptStartOfPeriod then
      APayment := APayment * q;
    x1 := APayment - AFutureValue * ARate;
    x2 := APayment + APresentValue * ARate;
    if   (x2 = 0)                    // we have to divide by x2
      or (sign(x1) * sign(x2) < 0)   // the argument of the log is negative
    then
      Result := Infinity
    else begin
      Result := ln(x1/x2) / ln(q);
    end;
  end;
end;

function Payment(ARate: double; NPeriods: Integer;
  APresentValue, AFutureValue: double; APaymentTime: TPaymentTime): double;
var
  q, qn, factor: double;
begin
  if ARate = 0 then
    Result := -(AFutureValue + APresentValue) / NPeriods
  else begin
    q := 1.0 + ARate;
    qn := power(q, NPeriods);
    factor := (qn - 1) / (q - 1);
    if APaymentTime = ptStartOfPeriod then
      factor := factor * q;
    Result := -(AFutureValue + APresentValue * qn) / factor;
  end;
end;

function PresentValue(ARate: double; NPeriods: Integer;
  APayment, AFutureValue: double; APaymentTime: TPaymentTime): double;
var
  q, qn, factor: double;
begin
  if ARate = 0.0 then
    Result := -AFutureValue - APayment * NPeriods
  else begin
    q := 1.0 + ARate;
    qn := power(q, NPeriods);
    factor := (qn - 1) / (q - 1);
    if APaymentTime = ptStartOfPeriod then
      factor := factor * q;
    Result := -(AFutureValue + APayment*factor) / qn;
  end;
end;

{ ---------------------------------------------------------------------
  Miscellaneous
  ---------------------------------------------------------------------}

function IfThen(val: boolean; const ifTrue: integer; const ifFalse: integer): integer;

begin
  if val then result:=iftrue else result:=iffalse;
end;

function IfThen(val: boolean; const ifTrue: double; const ifFalse: double): double;

begin
  if val then result:=iftrue else result:=iffalse;
end;

function CompareValue(const A, B  : Integer): TValueRelationship;

begin
  result:=GreaterThanValue;
  if a=b then
    result:=EqualsValue
  else
   if a<b then
     result:=LessThanValue;
end;

function CompareValue(const A, B: NativeLargeInt): TValueRelationship;

begin
  result:=GreaterThanValue;
  if a=b then
    result:=EqualsValue
  else
   if a<b then
     result:=LessThanValue;
end;

function CompareValue(const A, B: NativeLargeUInt): TValueRelationship;

begin
  result:=GreaterThanValue;
  if a=b then
    result:=EqualsValue
  else
   if a<b then
     result:=LessThanValue;
end;

function CompareValue(const A, B: Double; delta: Double): TValueRelationship;
begin
  result:=GreaterThanValue;
  if abs(a-b)<=delta then
    result:=EqualsValue
  else
   if a<b then
     result:=LessThanValue;
end;

end.

