{   This unit contains the TTestRunner class, a base class for the console test
    runner for fpcunit.

    This file is part of the Free Component Library (FCL)
    Copyright (C) 2017 Michael Van Canneyt

    This library is free software; you can redistribute it and/or modify it
    under the terms of the GNU Library General Public License as published by
    the Free Software Foundation; either version 2 of the License, or (at your
    option) any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
    for more details.

    You should have received a copy of the GNU Library General Public License
    along with this library; if not, write to the Free Software Foundation,
    Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

 **********************************************************************}
unit BrowserTestRunner;

{$mode objfpc}

interface

uses
  CustApp, browserapp, Classes, SysUtils,
  FPCUnit, TestRegistry,
  FPCUnitReport, htmlTestReport;

const
  Version = '0.9';

type

  { TRunForm }

  TRunForm = class(TComponent)
  private
    FOnRun: TNotifyEvent;
  Public
    Procedure Initialize; virtual;
    Property OnRun : TNotifyEvent Read FOnRun Write FOnRun;
  end;
  TRunFormClass = class of TRunForm;
  { TTestRunner }

  TTestRunner = class(TBrowserApplication)
  private
    FRunFormClass: TRunFormClass;
    FShowProgress: boolean;
    procedure DoRunAgain(Sender: TObject);
    procedure ShowTests(T: TTestsuite);
  protected
    property ShowProgress: boolean read FShowProgress write FShowProgress;
    procedure DoRun; override;
    procedure DoTestRun(ATest: TTest); virtual;
    //procedure ExtendXmlDocument(Doc: TXMLDocument); virtual;
    function GetResultsWriter: TCustomResultsWriter; virtual;
  public
    procedure RunTests;
    Property RunFormClass : TRunFormClass Read FRunFormClass Write FRunFormClass;
  end;

implementation



type
  { TProgressWriter }

  TProgressWriter = class({TNoRefCountObject, }ITestListener)
  private
    FSuccess: boolean;
    procedure WriteChar(c: char);
  public
    destructor Destroy; override;

    { ITestListener interface requirements }
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure); override;
    procedure AddError(ATest: TTest; AError: TTestFailure); override;
    procedure StartTest(ATest: TTest); override;
    procedure EndTest(ATest: TTest); override;
    procedure StartTestSuite(ATestSuite: TTestSuite); override;
    procedure EndTestSuite(ATestSuite: TTestSuite); override;
  end;

{ TRunForm }

procedure TRunForm.Initialize;
begin
  // Do nothing
end;

procedure TProgressWriter.WriteChar(c: char);
begin
  write(c);
  // flush output, so that we see the char immediately, even it is written to file
  //Flush(output);
end;

destructor TProgressWriter.Destroy;
begin
  // on destruction, just write the missing line ending
  writeln;
  inherited Destroy;
end;

procedure TProgressWriter.AddFailure(ATest: TTest; AFailure: TTestFailure);
begin
  FSuccess := false;
  writechar('F');
  if ATest=nil then;
  if AFailure=nil then;
end;

procedure TProgressWriter.AddError(ATest: TTest; AError: TTestFailure);
begin
  FSuccess := false;
  writechar('E');
  if ATest=nil then;
  if AError=nil then ;
end;

procedure TProgressWriter.StartTest(ATest: TTest);
begin
  FSuccess := true; // assume success, until proven otherwise
  if ATest=nil then;
end;

procedure TProgressWriter.EndTest(ATest: TTest);
begin
  if FSuccess then
    writechar('.');
  if ATest=nil then ;
end;

procedure TProgressWriter.StartTestSuite(ATestSuite: TTestSuite);
begin
  // do nothing
  if ATestSuite=nil then;
end;

procedure TProgressWriter.EndTestSuite(ATestSuite: TTestSuite);
begin
  // do nothing
  if ATestSuite=nil then;
end;

{ TTestRunner }

procedure TTestRunner.ShowTests(T : TTestsuite);

Var
  B : TTestTreeBuilder;

begin
  B:=THTMLTreeBuilder.Create('');
  try
    B.ShowSuite(T);
  Finally
    FreeAndNil(B);
  end;
end;

procedure TTestRunner.DoRunAgain(Sender : TObject);

begin
  RunTests;
end;

procedure TTestRunner.DoRun;

var
  R : TRunForm;
begin
  If Assigned(RunFormClass) then
    begin
    R:=RunFormClass.Create(Self);
    R.OnRun:=@DoRunAgain;
    R.Initialize;
    end;
  RunTests;
  Terminate;
end;

procedure TTestRunner.RunTests;

begin
  ShowTests(GetTestRegistry);
  DoTestRun(GetTestRegistry);
end;

procedure TTestRunner.DoTestRun(ATest: TTest);

var
  ResultsWriter: TCustomResultsWriter;
  ProgressWriter: TProgressWriter;
  TestResult: TTestResult;

begin
  ResultsWriter := GetResultsWriter;
  TestResult := TTestResult.Create;
  ProgressWriter:=nil;
  try
    if ShowProgress then
      begin
      ProgressWriter := TProgressWriter.Create;
      TestResult.AddListener(ProgressWriter);
      end
    else
      ProgressWriter := nil;
    TestResult.AddListener(ResultsWriter.TestListener);
    ATest.Run(TestResult);
    ResultsWriter.WriteResult(TestResult);
  finally
    FreeAndNil(TestResult);
    FreeAndNil(ResultsWriter);
    FreeAndNil(ProgressWriter);
  end;
end;


function TTestRunner.GetResultsWriter: TCustomResultsWriter;
begin
  Result:=THTMLResultsWriter.Create(Nil);
end;

end.

