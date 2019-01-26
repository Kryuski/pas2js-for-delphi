{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2004 by Dean Zobec, Michael Van Canneyt

    Port to Free Pascal of the JUnit framework.
    Port to Pas2JS by Mattias Gaertner in 2017.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit TestDecorator;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FPCUnit;

type

  { TTestDecorator }

  TTestDecorator = class(TAssert)
  private
    FTest: TTest;
  protected
    function GetTestName: string; override;
    function GetTestSuiteName: string; override;
    procedure SetTestSuiteName(const aName: string); override;
    function GetEnableIgnores: boolean; override;
    procedure SetEnableIgnores(Value: boolean); override;
  public
    function CountTestCases: integer; override;
    constructor Create(aTest: TTest); reintroduce; overload;
    destructor Destroy; override;
    procedure BasicRun(AResult: TTestResult); virtual;
    procedure Run(AResult: TTestResult); override;
    property Test: TTest read FTest;
  end;

  { TTestSetup }

  TTestSetup = class(TTestDecorator)
  protected
    procedure OneTimeSetup; virtual; abstract;
    procedure OneTimeTearDown; virtual; abstract;
  public
    procedure Run(AResult: TTestResult); override;
  end;

implementation

{ TTestDecorator }

function TTestDecorator.GetTestName: string;
begin
  Result := FTest.TestName;
end;

function TTestDecorator.GetTestSuiteName: string;
begin
  Result := FTest.TestSuiteName;
end;

procedure TTestDecorator.SetTestSuiteName(const aName: string);
begin
  FTest.TestSuiteName := aName;
end;

function TTestDecorator.GetEnableIgnores: boolean;
begin
  Result := FTest.EnableIgnores;
end;

procedure TTestDecorator.SetEnableIgnores(Value: boolean);
begin
  FTest.EnableIgnores := Value;
end;

function TTestDecorator.CountTestCases: integer;
begin
  Result := FTest.CountTestCases;
end;

constructor TTestDecorator.Create(aTest: TTest);
begin
  inherited Create;
  FTest := aTest;
end;

destructor TTestDecorator.Destroy;
begin
  FreeAndNil(FTest);
  inherited Destroy;
end;

procedure TTestDecorator.BasicRun(AResult: TTestResult);
begin
  FTest.Run(AResult);
end;

procedure TTestDecorator.Run(AResult: TTestResult);
begin
  BasicRun(AResult);
end;

procedure OneTimeProtect(aTest: TTest; aResult: TTestResult);
begin
  if aTest is TTestSetup then
  begin
    TTestSetup(aTest).OneTimeSetup;
    TTestSetup(aTest).BasicRun(aResult);
    TTestSetup(aTest).OneTimeTearDown;
  end;
end;

{ TTestSetup }

procedure TTestSetup.Run(AResult: TTestResult);
begin
  AResult.RunProtected(Self, @OneTimeProtect);
end;

end.

