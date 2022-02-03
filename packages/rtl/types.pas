{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2018 by Mattias Gaertner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Types;

{$mode objfpc}

interface

type
  TDirection = (FromBeginning, FromEnd);

  TBooleanDynArray = array of Boolean;
  TWordDynArray = array of Word;
  TIntegerDynArray = array of Integer;
  TNativeIntDynArray = array of NativeInt;
  TStringDynArray = array of String;
  TDoubleDynArray = array of Double;
  TJSValueDynArray = array of JSValue;
  TObjectDynArray = array of TObject;
  TByteDynArray = array of Byte;

  TDuplicates = (dupIgnore, dupAccept, dupError);
  TProc = Reference to Procedure;
  TProcString = Reference to Procedure(Const aString : String);

  TListCallback = procedure(data, arg: JSValue) of object;
  TListStaticCallback = procedure(data, arg: JSValue);

  TSize  = record
    cx, cy: integer;
  end;

  TPoint  = record
    x, y: integer;
  end;

  TRect  = record
    Left, Top, Right, Bottom: Integer;
  end;
  
function EqualRect(const r1,r2 : TRect) : Boolean;
function Rect(Left, Top, Right, Bottom : Integer) : TRect;
function Bounds(ALeft, ATop, AWidth, AHeight : Integer) : TRect;
function Point(x,y : Integer): TPoint; {$IFDEF Has_Inline}inline;{$ENDIF}
function PtInRect(const aRect: TRect; const p: TPoint) : Boolean;
function IntersectRect(out aRect: TRect; const R1,R2: TRect) : Boolean;
function UnionRect(out aRect: TRect; const R1,R2: TRect) : Boolean;
function IsRectEmpty(const aRect: TRect) : Boolean;
function OffsetRect(var aRect: TRect; DX, DY: Integer) : Boolean;
function CenterPoint(const aRect: TRect): TPoint;
function InflateRect(var aRect: TRect; dx, dy: Integer): Boolean;
function Size(AWidth, AHeight: Integer): TSize;
function Size(const aRect: TRect): TSize;

implementation

function EqualRect(const r1, r2: TRect): Boolean;
begin
  Result:=(r1.left=r2.left) and (r1.right=r2.right) and (r1.top=r2.top) and (r1.bottom=r2.bottom);
end;

function Rect(Left, Top, Right, Bottom: Integer): TRect;
begin
  Result.Left:=Left;
  Result.Top:=Top;
  Result.Right:=Right;
  Result.Bottom:=Bottom;
end;

function Bounds(ALeft, ATop, AWidth, AHeight: Integer): TRect;
begin
  Result.Left:=ALeft;
  Result.Top:=ATop;
  Result.Right:=ALeft+AWidth;
  Result.Bottom:=ATop+AHeight;
end;

function Point(x, y: Integer): TPoint;
begin
  Result.x:=x;
  Result.y:=y;
end;

function PtInRect(const aRect: TRect; const p: TPoint): Boolean;
begin
  Result:=(p.y>=aRect.Top) and
          (p.y<aRect.Bottom) and
          (p.x>=aRect.Left) and
          (p.x<aRect.Right);
end;

function IntersectRect(out aRect: TRect; const R1, R2: TRect): Boolean;
var
  lRect: TRect;
begin
  lRect := R1;
  if R2.Left > R1.Left then
    lRect.Left := R2.Left;
  if R2.Top > R1.Top then
    lRect.Top := R2.Top;
  if R2.Right < R1.Right then
    lRect.Right := R2.Right;
  if R2.Bottom < R1.Bottom then
    lRect.Bottom := R2.Bottom;

  // The var parameter is only assigned in the end to avoid problems
  // when passing the same rectangle in the var and const parameters.
  if IsRectEmpty(lRect) then
  begin
    aRect:=Rect(0,0,0,0);
    Result:=false;
  end
  else
  begin
    Result:=true;
    aRect := lRect;
  end;
end;

function UnionRect(out aRect: TRect; const R1, R2: TRect): Boolean;
var
  lRect: TRect;
begin
  lRect:=R1;
  if R2.Left<R1.Left then
    lRect.Left:=R2.Left;
  if R2.Top<R1.Top then
    lRect.Top:=R2.Top;
  if R2.Right>R1.Right then
    lRect.Right:=R2.Right;
  if R2.Bottom>R1.Bottom then
    lRect.Bottom:=R2.Bottom;

  if IsRectEmpty(lRect) then
  begin
    aRect:=Rect(0,0,0,0);
    Result:=false;
  end
  else
  begin
    aRect:=lRect;
    Result:=true;
  end;
end;

function IsRectEmpty(const aRect: TRect): Boolean;
begin
  Result:=(aRect.Right<=aRect.Left) or (aRect.Bottom<=aRect.Top);
end;

function OffsetRect(var aRect: TRect; DX, DY: Integer): Boolean;
begin
  with aRect do
    begin
    inc(Left,dx);
    inc(Top,dy);
    inc(Right,dx);
    inc(Bottom,dy);
    end;
  Result:=true;
end;

function CenterPoint(const aRect: TRect): TPoint;

  function Avg(a, b: Longint): Longint;
  begin
    if a < b then
      Result := a + ((b - a) shr 1)
    else
      Result := b + ((a - b) shr 1);
  end;

begin
  with aRect do
    begin
      Result.X := Avg(Left, Right);
      Result.Y := Avg(Top, Bottom);
    end;
end;

function InflateRect(var aRect: TRect; dx, dy: Integer): Boolean;
begin
  with aRect do
  begin
    dec(Left, dx);
    dec(Top, dy);
    inc(Right, dx);
    inc(Bottom, dy);
  end;
  Result := True;
end;

function Size(AWidth, AHeight: Integer): TSize;
begin
  Result.cx := AWidth;
  Result.cy := AHeight;
end;

function Size(const aRect: TRect): TSize;
begin
  Result.cx := aRect.Right - aRect.Left;
  Result.cy := aRect.Bottom - aRect.Top;
end;

end.

