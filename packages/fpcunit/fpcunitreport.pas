{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2006 by Dean Zobec

    Port to Pas2JS by Mattias Gaertner in 2017.

    common base classes for FPCUnit test reports

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPCUnitReport;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FPCUnit;

type
  TWriteTestHeaderEvent = procedure(Sender: TObject; ATest: TTest;
    ALevel: integer; ACount: integer) of object;
  TWriteTestFooterEvent = procedure(Sender: TObject; ATest: TTest;
    ALevel: integer; ATiming: TDateTime) of object;
  TTestNameEvent = procedure(Sender: TObject; const AName: string) of object;
  TFailureEvent = procedure(Sender: TObject; ATest: TTest; AFailure: TTestFailure) of object;
  TTestEvent = procedure(Sender: TObject; ATest: TTest) of object;
  TWriteTestSuiteHeaderEvent = procedure(Sender: TObject; ATestSuite: TTestSuite;
    ALevel: integer) of object;
  TWriteTestSuiteFooterEvent = procedure(Sender: TObject; ATestSuite: TTestSuite;
    ALevel: integer; ATiming: TDateTime; ANumRuns, ANumErrors, ANumFailures,
    ANumIgnores: integer) of object;

  { TSuiteResults }

  TSuiteResults = class(TObject)
  private
    FStartTime: TDateTime;
  public
    Runs: integer;
    Failures: integer;
    Errors: integer;
    Ignores: integer;
    property StartTime: TDateTime read FStartTime write FStartTime;
  end;

  { TSuiteResultsStack }

  TSuiteResultsStack = class(TObject)
  private
    FResultsList: TFPList;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function Last: TSuiteResults;
    procedure RemoveLast;
    procedure Add;
    procedure IncrementRuns;
    procedure IncrementFailures;
    procedure IncrementErrors;
    procedure IncrementIgnores;
  end;

  TCustomResultsWriter = class;

  { TCustomResultsWriterTestListener }

  TCustomResultsWriterTestListener = class(ITestListener) // ToDo: replace when interfaces are implemented
  public
    Writer: TCustomResultsWriter;
    constructor Create(aWriter: TCustomResultsWriter); reintroduce;
    procedure AddError(ATest: TTest; AError: TTestFailure); override;
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure); override;
    procedure EndTest(ATest: TTest); override;
    procedure EndTestSuite(ATestSuite: TTestSuite); override;
    procedure StartTest(ATest: TTest); override;
    procedure StartTestSuite(ATestSuite: TTestSuite); override;
  end;

  { TCustomResultsWriter }

  TCustomResultsWriter = class(TComponent)
  private
    FLevel: integer;
    FCount: integer;
    FSkipAddressInfo: Boolean;
    FSparse: Boolean;
    FTestTime: TDateTime;
    FFileName: string;
    FSuiteResultsStack : TSuiteResultsStack;
    FOnWriteSuiteHeader: TWriteTestSuiteHeaderEvent;
    FOnWriteSuiteFooter: TWriteTestSuiteFooterEvent;
    FOnWriteTestHeader: TWriteTestHeaderEvent;
    FOnWriteTestFooter: TWriteTestFooterEvent;
    FOnAddFailure: TFailureEvent;
    FOnAddError: TFailureEvent;
    FOnStartTest: TTestEvent;
    FOnEndTest: TTestEvent;
    FOnStartTestSuite: TTestEvent;
    FOnEndTestSuite: TTestEvent;
    FSkipTiming: Boolean;
    FTestListener: ITestListener; // TCustomResultsWriterTestListener ToDo: replace when interfaces are implemented
  protected
    procedure SetSkipAddressInfo(AValue: Boolean); virtual;
    procedure SetSparse(AValue: Boolean); virtual;
    procedure WriteTestHeader(ATest: TTest; ALevel: integer; ACount: integer); virtual;
    procedure WriteTestFooter(ATest: TTest; ALevel: integer; ATiming: TDateTime); virtual;
    procedure WriteSuiteHeader(ATestSuite: TTestSuite; ALevel: integer); virtual;
    procedure WriteSuiteFooter(ATestSuite: TTestSuite; ALevel: integer;
      ATiming: TDateTime; ANumRuns, ANumErrors, aNumFailures, ANumIgnores: integer); virtual;
    procedure WriteHeader; virtual;
    procedure WriteFooter; virtual;
  public
  {ITestListener}
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure); virtual;
    procedure AddError(ATest: TTest; AError: TTestFailure); virtual;
    procedure StartTest(ATest: TTest); virtual;
    procedure EndTest(ATest: TTest); virtual;
    procedure StartTestSuite(ATestSuite: TTestSuite); virtual;
    procedure EndTestSuite(ATestSuite: TTestSuite); virtual;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure WriteResult(aResult: TTestResult); virtual;
  published
    property FileName: string read FFileName write FFileName;
    property OnWriteSuiteHeader: TWriteTestSuiteHeaderEvent read FOnWriteSuiteHeader
      write FOnWriteSuiteHeader;
    property OnWriteSuiteFooter: TWriteTestSuiteFooterEvent read FOnWriteSuiteFooter
      write FOnWriteSuiteFooter;
    property OnWriteTestHeader: TWriteTestHeaderEvent read FOnWriteTestHeader
      write FOnWriteTestHeader;
    property OnWriteTestFooter: TWriteTestFooterEvent read FOnWriteTestFooter
      write FOnWriteTestFooter;
    property OnAddFailure: TFailureEvent read FOnAddFailure write FOnAddFailure;
    property OnAddError: TFailureEvent read FOnAddError write FOnAddError;
    property OnStartTest: TTestEvent read FOnStartTest write FOnStartTest;
    property OnEndTest: TTestEvent read FOnEndTest write FOnEndTest;
    property OnStartTestSuite: TTestEvent read FOnStartTestSuite write FOnStartTestSuite;
    property OnEndTestSuite: TTestEvent read FOnEndTestSuite write FOnEndTestSuite;
    Property SkipTiming : Boolean Read FSkipTiming Write FSkipTiming;
    Property Sparse : Boolean Read FSparse Write SetSparse;
    Property SkipAddressInfo : Boolean Read FSkipAddressInfo Write SetSkipAddressInfo;
    Property TestListener: ITestListener read FTestListener; // ToDo: replace when interfaces are implemented
  end;

implementation

{ TCustomResultsWriterTestListener }

constructor TCustomResultsWriterTestListener.Create(
  aWriter: TCustomResultsWriter);
begin
  Writer:=aWriter;
end;

procedure TCustomResultsWriterTestListener.AddError(ATest: TTest;
  AError: TTestFailure);
begin
  Writer.AddError(ATest,AError);
end;

procedure TCustomResultsWriterTestListener.AddFailure(ATest: TTest;
  AFailure: TTestFailure);
begin
  Writer.AddFailure(ATest,AFailure);
end;

procedure TCustomResultsWriterTestListener.EndTest(ATest: TTest);
begin
  Writer.EndTest(ATest);
end;

procedure TCustomResultsWriterTestListener.EndTestSuite(ATestSuite: TTestSuite);
begin
  Writer.EndTestSuite(ATestSuite);
end;

procedure TCustomResultsWriterTestListener.StartTest(ATest: TTest);
begin
  Writer.StartTest(ATest);
end;

procedure TCustomResultsWriterTestListener.StartTestSuite(ATestSuite: TTestSuite
  );
begin
  Writer.StartTestSuite(ATestSuite);
end;

{ TCustomResultsWriter }

procedure TCustomResultsWriter.SetSkipAddressInfo(AValue: Boolean);
begin
  if FSkipAddressInfo=AValue then Exit;
  FSkipAddressInfo:=AValue;
end;

procedure TCustomResultsWriter.SetSparse(AValue: Boolean);
begin
  if FSparse=AValue then Exit;
  FSparse:=AValue;
end;

procedure TCustomResultsWriter.WriteTestHeader(ATest: TTest; ALevel: integer;
  ACount: integer);
begin
  if Assigned(FOnWriteTestHeader) then
    FOnWriteTestHeader(Self, ATest, ALevel, ACount);
end;

procedure TCustomResultsWriter.WriteTestFooter(ATest: TTest; ALevel: integer;
  ATiming: TDateTime);
begin
  if Assigned(FOnWriteTestFooter) then
    FOnWriteTestFooter(Self, ATest, ALevel, ATiming);
end;

procedure TCustomResultsWriter.WriteSuiteHeader(ATestSuite: TTestSuite;
  ALevel: integer);
begin
  if Assigned(FOnWriteSuiteHeader) then
    FOnWriteSuiteHeader(Self, ATestSuite, ALevel);
end;

procedure TCustomResultsWriter.WriteSuiteFooter(ATestSuite: TTestSuite;
  ALevel: integer; ATiming: TDateTime; ANumRuns, ANumErrors, aNumFailures,
  ANumIgnores: integer);
begin
  if Assigned(FOnWriteSuiteFooter) then
    FOnWriteSuiteFooter(Self, ATestSuite, ALevel, ATiming, ANumRuns, ANumErrors,
      aNumFailures, ANumIgnores);
end;

procedure TCustomResultsWriter.WriteHeader;
begin
  // do nothing
end;

procedure TCustomResultsWriter.WriteFooter;
begin
  // do nothing
end;

procedure TCustomResultsWriter.AddFailure(ATest: TTest; AFailure: TTestFailure);
begin
  if AFailure.IsIgnoredTest then
    FSuiteResultsStack.IncrementIgnores
  else
    FSuiteResultsStack.IncrementFailures;
  if Assigned(FOnAddFailure) then
    FOnAddFailure(Self, ATest, AFailure);
end;

procedure TCustomResultsWriter.AddError(ATest: TTest; AError: TTestFailure);
begin
  FSuiteResultsStack.IncrementErrors;
  if Assigned(FOnAddError) then
    FOnAddError(Self, ATest, AError);
end;

procedure TCustomResultsWriter.StartTest(ATest: TTest);
begin
  WriteTestHeader(ATest, FLevel, FCount);
  if Assigned(FOnStartTest) then
    FOnStartTest(Self, ATest);
  FTestTime := Now;
end;

procedure TCustomResultsWriter.EndTest(ATest: TTest);
begin
  Inc(FCount);
  FTestTime := Now - FTestTime;
  FSuiteResultsStack.IncrementRuns;
  WriteTestFooter(ATest, FLevel, FTestTime);
  if Assigned(FOnEndTest) then
    FOnEndTest(Self, ATest);
end;

procedure TCustomResultsWriter.StartTestSuite(ATestSuite: TTestSuite);
begin
  inc(FLevel);
  WriteSuiteHeader(ATestSuite, FLevel);
  if Assigned(FOnStartTestSuite) then
    FOnStartTestSuite(Self, ATestSuite);
  FSuiteResultsStack.Add;
  FSuiteResultsStack.Last.StartTime := now;
end;

procedure TCustomResultsWriter.EndTestSuite(ATestSuite: TTestSuite);
begin
  with FSuiteResultsStack.Last do
  begin
    WriteSuiteFooter(ATestSuite, FLevel, Now - StartTime,
    Runs, Errors, Failures, Ignores);
  end;
  FSuiteResultsStack.RemoveLast;
  dec(FLevel);
  if Assigned(FOnEndTestSuite) then
    FOnEndTestSuite(Self, ATestSuite);
end;

constructor TCustomResultsWriter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLevel := -1;
  FCount := 1;
  FFilename := '';
  FSuiteResultsStack := TSuiteResultsStack.Create;
  FTestListener:=TCustomResultsWriterTestListener.Create(Self);
end;

destructor TCustomResultsWriter.Destroy;
begin
  FreeAndNil(FSuiteResultsStack);
  FreeAndNil(FTestListener);
  inherited Destroy;
end;

procedure TCustomResultsWriter.AfterConstruction;
begin
  WriteHeader;
end;

procedure TCustomResultsWriter.BeforeDestruction;
begin
  WriteFooter;
end;

procedure TCustomResultsWriter.WriteResult(aResult: TTestResult);
begin
  // do nothing
  if aResult=nil then;
end;

{ TSuiteResultsStack }

constructor TSuiteResultsStack.Create;
begin
  FResultsList := TFPList.Create;
end;

destructor TSuiteResultsStack.Destroy;
var
  i: integer;
begin
  for i := 0 to FResultsList.Count-1 do
    TObject(FResultsList[i]).Destroy;
  FreeAndNil(FResultsList);
  inherited Destroy;
end;

function TSuiteResultsStack.Last: TSuiteResults;
begin
  Result := TSuiteResults(FResultsList[FResultsList.Count -1]);
end;

procedure TSuiteResultsStack.RemoveLast;
begin
  TObject(FResultsList[FResultsList.Count - 1]).Destroy;
  FResultsList.Delete(FResultsList.Count - 1);
end;

procedure TSuiteResultsStack.Add;
begin
  FResultsList.Add(TSuiteResults.Create);
end;

procedure TSuiteResultsStack.IncrementRuns;
var
  i: integer;
begin
  for i := 0 to FResultsList.Count -1 do
    Inc(TSuiteResults(FResultsList[i]).Runs);
end;

procedure TSuiteResultsStack.IncrementFailures;
var
  i: integer;
begin
  for i := 0 to FResultsList.Count -1 do
    Inc(TSuiteResults(FResultsList[i]).Failures);
end;

procedure TSuiteResultsStack.IncrementErrors;
var
  i: integer;
begin
  for i := 0 to FResultsList.Count -1 do
    Inc(TSuiteResults(FResultsList[i]).Errors);
end;

procedure TSuiteResultsStack.IncrementIgnores;
var
  i: integer;
begin
  for i := 0 to FResultsList.Count -1 do
    Inc(TSuiteResults(FResultsList[i]).Ignores);
end;

end.

