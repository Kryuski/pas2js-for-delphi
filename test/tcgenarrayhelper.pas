{
    This file is part of the Free Pascal/NewPascal run time library.
    Copyright (c) 2018 by Maciej Izak (hnb),
    member of the NewPascal development team (http://newpascal.org)

    Copyright(c) 2004-2018 DaThoX

    It contains tests for the Free Pascal generics library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Acknowledgment

    Thanks to Sphere 10 Software (http://sphere10.com) for sponsoring
    many new types, tests and major refactoring of entire library

 **********************************************************************}

unit tcgenarrayhelper;

{$mode delphi}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, Generics.Defaults, Generics.Collections;

type

  { TTestArrayHelper }

  TTestArrayHelper = class(TTestCase)
  protected
    procedure CheckBinarySearch(constref AArray: TArray<Integer>;
      AValue: Integer; AExpectedResult: boolean; out ASearchResult: TBinarySearchResult); overload;
    procedure CheckBinarySearch(constref AArray: TArray<Integer>;
      AValue: Integer; AExpectedResult: boolean; out ASearchResult: TBinarySearchResult; const AComparer: IComparer<Integer>);overload;
    procedure CheckSearchResult(constref ASearchResult: TBinarySearchResult;
      AValue: Integer; ACandidateIndex, AFoundIndex: SizeInt; ACompareResult: Boolean);
  published
    procedure Test_BinarySearch_IntegersCompare;
    procedure Test_BinarySearch_EmptyArrayCompare;
    procedure Test_BinarySearch_Integers;
    procedure Test_BinarySearch_EmptyArray;
  end;

implementation

{ TTestArrayHelper }
procedure TTestArrayHelper.CheckBinarySearch(constref AArray: TArray<Integer>;
  AValue: Integer; AExpectedResult: boolean; out
  ASearchResult: TBinarySearchResult; const AComparer: IComparer<Integer>);

begin
  CheckEquals(AExpectedResult,
    TArrayHelper<Integer>.BinarySearch(AArray,AValue,ASearchResult,aComparer,0,Length(AArray)),
    'Wrong BinarySearch result for ' + AValue.ToString);
end;

procedure TTestArrayHelper.CheckBinarySearch(constref AArray: TArray<Integer>;
  AValue: Integer; AExpectedResult: boolean; out
  ASearchResult: TBinarySearchResult);
begin
  Fail('Overload without comparer not supported');
(*
*)
end;

procedure TTestArrayHelper.CheckSearchResult(constref
  ASearchResult: TBinarySearchResult; AValue: Integer; ACandidateIndex,
  AFoundIndex: SizeInt; ACompareResult: Boolean);
begin
  with ASearchResult do
  begin
    CheckEquals(ACandidateIndex, CandidateIndex, 'Wrong binary search result (CandidateIndex) for ' + AValue.ToString);
    CheckEquals(AFoundIndex, FoundIndex, 'Wrong binary search result (FoundIndex) for ' + AValue.ToString);
    Check(ACompareResult, 'Wrong binary search result (CompareResult) for ' + AValue.ToString);
  end;
end;

procedure TTestArrayHelper.Test_BinarySearch_IntegersCompare;
var
  a: TArray<Integer>;
  LSearchResult: TBinarySearchResult;
  aComparer : IComparer<Integer>;

begin
  a:=[1,3,5,7,9,11,13,15,20];
  aComparer:=TComparer<Integer>.Construct(function (Const a,b : integer) : integer
    begin
      Result:=a-b;
    end);
  CheckBinarySearch(a, 10, False, LSearchResult, aComparer);
  CheckSearchResult(LSearchResult, 10, 5, -1, LSearchResult.CompareResult>0);

  CheckBinarySearch(a, 20, True, LSearchResult,aComparer);
  CheckSearchResult(LSearchResult, 20, 8, 8, LSearchResult.CompareResult=0);
end;

procedure TTestArrayHelper.Test_BinarySearch_EmptyArrayCompare;
var
  LSearchResult: TBinarySearchResult;
  aComparer : IComparer<Integer>;

begin
  aComparer:=TComparer<Integer>.Construct(function (Const a,b : integer) : integer
    begin
      Result:=a-b;
    end);
  CheckBinarySearch(nil, 1, False, LSearchResult,aComparer);
  CheckSearchResult(LSearchResult, 1, -1, -1, LSearchResult.CompareResult=0);
end;

procedure TTestArrayHelper.Test_BinarySearch_Integers;

var
  a: TArray<Integer>;
  LSearchResult: TBinarySearchResult;

begin
  a:=[1,3,5,7,9,11,13,15,20];

  CheckBinarySearch(a, 10, False, LSearchResult);
  CheckSearchResult(LSearchResult, 10, 5, -1, LSearchResult.CompareResult>0);

  CheckBinarySearch(a, 20, True, LSearchResult);
  CheckSearchResult(LSearchResult, 20, 8, 8, LSearchResult.CompareResult=0);
end;

procedure TTestArrayHelper.Test_BinarySearch_EmptyArray;
var
  LSearchResult: TBinarySearchResult;
begin
  CheckBinarySearch(nil, 1, False, LSearchResult);
  CheckSearchResult(LSearchResult, 1, -1, -1, LSearchResult.CompareResult=0);
end;

begin
  RegisterTest(TTestArrayHelper);
end.

