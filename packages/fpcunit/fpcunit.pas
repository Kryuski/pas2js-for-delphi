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
unit FPCUnit;

{$mode objfpc}

interface

uses
  Classes, SysUtils, JS, TypInfo;

type
  EAssertionFailedError = class(Exception);
  EIgnoredTest = class(EAssertionFailedError);

  TTestStep = (stSetUp, stRunTest, stTearDown, stNothing);

  TRunMethod = procedure of object;

  TTestResult = class;
  TTestSuite = class;

  { TTest }

  TTest = class(TObject)
  protected
    FLastStep: TTestStep;
    function GetTestName: string; virtual;
    function GetTestSuiteName: string; virtual;
    function GetEnableIgnores: boolean; virtual;
    procedure SetTestSuiteName(const aName: string); virtual; abstract;
    procedure SetEnableIgnores(Value: boolean); virtual; abstract;
  public
    function CountTestCases: integer; virtual;
    Function GetChildTestCount : Integer; virtual;
    Function GetChildTest(AIndex : Integer) : TTest; virtual;
    function FindChildTest(const AName: String): TTest;
    Function FindTest(Const AName : String) : TTest;
    procedure Run(AResult: TTestResult); virtual;
    procedure Ignore(const AMessage: string);
  published
    property TestName: string read GetTestName;
    property TestSuiteName: string read GetTestSuiteName write SetTestSuiteName;
    property LastStep: TTestStep read FLastStep;
    property EnableIgnores: boolean read GetEnableIgnores write SetEnableIgnores;
  end;

  { TAssert }

  TAssert = class(TTest)
  protected
    Class var AssertCount : Integer;
  public
    class procedure Fail(const AMessage: string);
    class procedure Fail(const AFmt: string; Args : Array of string);
    class procedure FailEquals(const expected, actual: string; const ErrorMsg: string = '');
    class procedure FailNotEquals(const expected, actual: string; const ErrorMsg: string = '');

    class procedure AssertTrue(const AMessage: string; ACondition: boolean); overload;
    class procedure AssertTrue(ACondition: boolean); overload;
    class procedure AssertFalse(const AMessage: string; ACondition: boolean); overload;
    class procedure AssertFalse(ACondition: boolean); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: string); overload;
    class procedure AssertEquals(Expected, Actual: string); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: NativeInt); overload;
    class procedure AssertEquals(Expected, Actual: NativeInt); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual, Delta: double); overload;
    class procedure AssertEquals(Expected, Actual, Delta: double); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: boolean); overload;
    class procedure AssertEquals(Expected, Actual: boolean); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: char); overload;
    class procedure AssertEquals(Expected, Actual: char); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: TClass); overload;
    class procedure AssertEquals(Expected, Actual: TClass); overload;
    class procedure AssertSame(const AMessage: string; Expected, Actual: TObject); overload;
    class procedure AssertSame(Expected, Actual: TObject); overload;
    class procedure AssertSame(const AMessage: string; Expected, Actual: Pointer); overload;
    class procedure AssertSame(Expected, Actual: Pointer); overload;
    class procedure AssertNotSame(const AMessage: string; Expected, Actual: TObject); overload;
    class procedure AssertNotSame(Expected, Actual: TObject); overload;
    class procedure AssertNotSame(const AMessage: string; Expected, Actual: Pointer); overload;
    class procedure AssertNotSame(Expected, Actual: Pointer); overload;
    class procedure AssertNotNull(const AMessage: string; AObject: TObject); overload;
    class procedure AssertNotNull(AObject: TObject); overload;
    //class procedure AssertNotNullIntf(const AMessage: string; AInterface: IInterface); overload;
    //class procedure AssertNotNullIntf(AInterface: IInterface); overload;
    class procedure AssertNotNull(const AMessage: string; APointer: Pointer); overload;
    class procedure AssertNotNull(APointer: Pointer); overload;
    class procedure AssertNull(const AMessage: string; AObject: TObject); overload;
    class procedure AssertNull(AObject: TObject); overload;
    //class procedure AssertNullIntf(const AMessage: string; AInterface: IInterface); overload;
    //class procedure AssertNullIntf(AInterface: IInterface); overload;
    class procedure AssertNull(const AMessage: string; APointer: Pointer); overload;
    class procedure AssertNull(APointer: Pointer); overload;
    class procedure AssertNotNull(const AMessage, AString: string); overload;
    class procedure AssertNotNull(const AString: string); overload;
    class procedure AssertException(const AMessage: string;
      AExceptionClass: ExceptClass; const AMethod: TRunMethod;
      const AExceptionMessage : String = ''; AExceptionContext : Integer = 0); overload;
    class procedure AssertException(AExceptionClass: ExceptClass;
      const AMethod: TRunMethod; const AExceptionMessage : String = '';
      AExceptionContext : Integer = 0); overload;

    // DUnit compatible methods
    class procedure Check(pValue: boolean; pMessage: string = '');
    class procedure CheckEquals(expected, actual: double; msg: string = ''); overload;
    class procedure CheckEquals(expected, actual: double; delta: double; msg: string = ''); overload;
    class procedure CheckEquals(expected, actual: string; msg: string = ''); overload;
    class procedure CheckEquals(expected, actual: integer; msg: string = ''); overload;
    class procedure CheckEquals(expected, actual: boolean; msg: string = ''); overload;
    class procedure CheckEquals(expected, actual: TClass; msg: string = ''); overload;
    class procedure CheckNotEquals(expected, actual: string; msg: string = ''); overload;
    class procedure CheckNotEquals(expected, actual: integer; msg: string = ''); overload; virtual;
    class procedure CheckNotEquals(expected, actual: boolean; msg: string = ''); overload; virtual;
    class procedure CheckNotEquals(expected, actual: double; delta: double = 0; msg: string = ''); overload; virtual;
    //class procedure CheckNull(obj: IUnknown; msg: string = ''); overload;
    class procedure CheckNull(obj: TObject; msg: string = ''); overload;
    class procedure CheckNotNull(obj: TObject; msg: string = ''); overload;
    //class procedure CheckNotNull(obj: IUnknown; msg: string = ''); overload; virtual;
    class procedure CheckIs(obj :TObject; pClass: TClass; msg: string = ''); overload;
    class procedure CheckSame(expected, actual: TObject; msg: string = ''); overload;
    class procedure CheckTrue(condition: Boolean; msg: string = '');
    class procedure CheckFalse(condition: Boolean; msg: string = '');
    class procedure CheckException(const AMethod: TRunMethod; AExceptionClass: ExceptClass; msg: string = '');
    class function  EqualsErrorMessage(const expected, actual: string; const ErrorMsg: string): string;
    class function  NotEqualsErrorMessage(const expected, actual: string; const ErrorMsg: string): string;

    class function Suite: TTest;
  end;

  { TTestFailure }

  TTestFailure = class(TObject)
  private
    FTestName: string;
    FTestSuiteName: string;
    FLineNumber: longint;
    FFailedMethodName: string;
    FRaisedExceptionClass: TClass;
    FRaisedExceptionMessage: string;
    FSourceUnitName: string;
    //FThrownExceptionAddress: Pointer;
    FTestLastStep: TTestStep;
    function GetAsString: string;
    function GetExceptionMessage: string;
    function GetIsFailure: boolean;
    function GetIsIgnoredTest: boolean;
    function GetExceptionClassName: string;
    //function GetLocationInfo: string;
    procedure SetTestLastStep(const Value: TTestStep);
  public
    constructor CreateFailure(ATest: TTest; E: Exception; LastStep: TTestStep);
    property ExceptionClass: TClass read FRaisedExceptionClass;
  published
    property AsString: string read GetAsString;
    property IsFailure: boolean read GetIsFailure;
    property IsIgnoredTest: boolean read GetIsIgnoredTest;
    property ExceptionMessage: string read GetExceptionMessage;
    property ExceptionClassName: string read GetExceptionClassName;
    property SourceUnitName: string read FSourceUnitName write FSourceUnitName;
    property LineNumber: longint read FLineNumber write FLineNumber;
    //property LocationInfo: string read GetLocationInfo;
    property FailedMethodName: string read FFailedMethodName write FFailedMethodName;
    property TestLastStep: TTestStep read FTestLastStep write SetTestLastStep;
  end;

  // ToDo convert to ITestListener = interface

  ITestListener = class
  public
  //['{0CE9D3AE-882A-D811-9401-ADEB5E4C7FC1}']
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure); virtual; abstract;
    procedure AddError(ATest: TTest; AError: TTestFailure); virtual; abstract;
    procedure StartTest(ATest: TTest); virtual; abstract;
    procedure EndTest(ATest: TTest); virtual; abstract;
    procedure StartTestSuite(ATestSuite: TTestSuite); virtual; abstract;
    procedure EndTestSuite(ATestSuite: TTestSuite); virtual; abstract;
  end;

  { TTestCase }

  TTestCase = class(TAssert)
  private
    FName: string;
    FTestSuiteName: string;
    FEnableIgnores: boolean;
    FExpectedExceptionFailMessage : String;
    FExpectedException : TClass;
    FExpectedExceptionMessage: String;
    FExpectedExceptionContext: Integer;
    //FExpectedExceptionCaller : Pointer;
  protected
    function CreateResult: TTestResult; virtual;
    procedure SetUp; virtual;
    procedure TearDown; virtual;
    procedure RunTest; virtual;
    function GetTestName: string; override;
    function GetTestSuiteName: string; override;
    function GetEnableIgnores: boolean; override;
    procedure SetTestSuiteName(const aName: string); override;
    procedure SetTestName(const Value: string); virtual;
    procedure SetEnableIgnores(Value: boolean); override;
    procedure RunBare; virtual;
    Class function SingleInstanceForSuite : Boolean; virtual;
  Public
    Class Var CheckAssertCalled : Boolean;
  public
    constructor Create; virtual; reintroduce;
    constructor CreateWith(const ATestName: string; const ATestSuiteName: string); virtual;
    constructor CreateWithName(const AName: string); virtual;
    procedure ExpectException(AExceptionClass: TClass; AExceptionMessage: string=''; AExceptionHelpContext: Integer=0);
    procedure ExpectException(const Msg: String; AExceptionClass: TClass; AExceptionMessage: string=''; AExceptionHelpContext: Integer=0);
    function CountTestCases: integer; override;
    function CreateResultAndRun: TTestResult; virtual;
    procedure Run(AResult: TTestResult); override;
    function AsString: string;
    property TestSuiteName: string read GetTestSuiteName write SetTestSuiteName;
    Property ExpectedExceptionFailMessage  : String Read FExpectedExceptionFailMessage;
    Property ExpectedException : TClass Read FExpectedException;
    Property ExpectedExceptionMessage : String Read FExpectedExceptionMessage;
    Property ExpectedExceptionContext: Integer Read FExpectedExceptionContext;
  published
    property TestName: string read GetTestName write SetTestName;
  end;

  TTestCaseClass = class of TTestCase;

  { TTestSuite }

  TTestSuite = class(TTest)
  private
    FTests: TFPList;
    FName: string;
    FTestSuiteName: string;
    FEnableIgnores: boolean;
  protected
    procedure ClearTests;
    function DoAddTest(ATest: TTest): Integer;
    function GetTestName: string; override;
    function GetTestSuiteName: string; override;
    function GetEnableIgnores: boolean; override;
    procedure SetTestSuiteName(const aName: string); override;
    procedure SetTestName(const Value: string); virtual;
    procedure SetEnableIgnores(Value: boolean); override;
  public
    constructor Create(AClass: TClass; AName: string); reintroduce; overload; virtual;
    constructor Create(AClass: TClass); reintroduce; overload; virtual;
    constructor Create(AClassArray: Array of TClass); reintroduce; overload; virtual;
    constructor Create(AName: string); reintroduce; overload; virtual;
    constructor Create; reintroduce; overload; virtual;
    destructor Destroy; override;
    function CountTestCases: integer; override;
    Function GetChildTestCount : Integer; override;
    Function GetChildTest(AIndex : Integer) : TTest; override;
    procedure Run(AResult: TTestResult); override;
    procedure RunTest(ATest: TTest; AResult: TTestResult); virtual;
    procedure AddTest(ATest: TTest); overload; virtual;
    procedure AddTestSuiteFromClass(ATestClass: TClass); virtual;
    class function Warning(const aMessage: string): TTestCase;
    property Test[Index: integer]: TTest read GetChildTest; default;
    Property ChildTestCount : Integer Read GetChildTestCount;
    property TestSuiteName: string read GetTestSuiteName write SetTestSuiteName;
    property TestName: string read GetTestName write SetTestName;
  end;

  TProtect = procedure(aTest: TTest; aResult: TTestResult);

  { TTestResult }

  TTestResult = class(TObject)
  protected
    FRunTests: integer;
    FFailures: TFPList;
    FIgnoredTests: TFPList;
    FErrors: TFPList;
    FListeners: TFPList; // list of ITestListener
    FSkippedTests: TFPList;
    FStartingTime: TDateTime;
    function GetNumErrors: integer;
    function GetNumFailures: integer;
    function GetNumIgnoredTests: integer;
    function GetNumSkipped: integer;
  public
    constructor Create; virtual; reintroduce;
    destructor Destroy; override;
    procedure ClearErrorLists;
    procedure StartTest(ATest: TTest);
    procedure AddFailure(ATest: TTest; E: EAssertionFailedError; aFailureList: TFPList{; AThrownExceptionAdrs: Pointer});
    procedure AddError(ATest: TTest; E: Exception{; AThrownExceptionAdrs: Pointer});
    procedure EndTest(ATest: TTest);
    procedure AddListener(AListener: ITestListener);
    procedure RemoveListener(AListener: ITestListener);
    procedure Run(ATestCase: TTestCase);
    procedure RunProtected(ATestCase: TTest; protect: TProtect);
    function WasSuccessful: boolean;
    function SkipTest(ATestCase: TTestCase): boolean;
    procedure AddToSkipList(ATestCase: TTestCase);
    procedure RemoveFromSkipList(ATestCase: TTestCase);
    procedure StartTestSuite(ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite);
  published
    property Listeners: TFPList read FListeners;
    property Failures: TFPList read FFailures;
    property IgnoredTests: TFPList read FIgnoredTests;
    property Errors: TFPList read FErrors;
    property RunTests: integer read FRunTests;
    property NumberOfErrors: integer read GetNumErrors;
    property NumberOfFailures: integer read GetNumFailures;
    property NumberOfIgnoredTests: integer read GetNumIgnoredTests;
    property NumberOfSkippedTests: integer read GetNumSkipped;
    property StartingTime: TDateTime read FStartingTime;
  end;

function ComparisonMsg(const aExpected, aActual: string; const aCheckEqual: boolean=true): string; overload;
function ComparisonMsg(const aMsg, aExpected, aActual: string; const aCheckEqual: boolean=true): string; overload;

const
  SCompare: String = ' expected: <%s> but was: <%s>';
  SCompareNotEqual: String = ' expected: not equal to <%s> but was: <%s>';
  SExpectedNotSame: String = 'expected not same';
  SExceptionCompare: String = 'Exception %s expected but %s was raised';
  SExceptionMessageCompare: String = 'Exception raised but exception property Message differs: ';
  SExceptionHelpContextCompare: String = 'Exception raised but exception property HelpContext differs: ';
  SMethodNotFound: String = 'Method <%s> not found';
  SNoValidInheritance: String = ' does not inherit from TTestCase';
  SNoValidTests: String = 'No valid tests found in ';
  SNoException: String = 'no exception';
  SAssertNotCalled: String = 'Assert not called during test.';

procedure FreeObjects(List: TFPList);
procedure GetMethodList(AObject: TObject; AList: TStrings); overload;

implementation

Const
  sExpectedButWasFmt = 'Expected:' + LineEnding + '"%s"' + LineEnding + 'But was:' + LineEnding + '"%s"';
  sExpectedButWasAndMessageFmt = '%s' + LineEnding + sExpectedButWasFmt;

// Get the ClassName of C
function GetN(C : TClass) : string;
begin
  if C=Nil then
    Result:='<NIL>'
  else
    Result:=C.ClassName;
end;

// Get the name of o
function GetN(o : TObject) : string;
begin
  if o=Nil then
    Result:='<NIL>'
  else begin
    Result:=o.ClassName;
    if o is TComponent then
      Result:=TComponent(o).Name+':'+Result;
  end;
end;

// Get the name of p
function GetPtrN(p : Pointer) : string;
begin
  Result:=jsTypeOf(p);
  if isObject(p) then
  begin
    if isClassInstance(p) then
      exit(GetN(TObject(p)));
    if hasString(TJSObject(p)['name']) then
      Result:=String(TJSObject(p)['name'])+':'+Result
    else if hasString(TJSObject(p)['Name']) then
      Result:=String(TJSObject(p)['Name'])+':'+Result
    else if hasString(TJSObject(p)['$name']) then
      Result:=String(TJSObject(p)['$name'])+':'+Result;
  end;
end;

function ComparisonMsg(const aExpected, aActual: string;
  const aCheckEqual: boolean): string;
// aCheckEqual=false gives the error message if the test does *not* expect
// the results to be the same.
begin
  if aCheckEqual then
    Result := format(SCompare, [aExpected, aActual])
  else {check unequal requires opposite error message}
    Result := format(SCompareNotEqual, [aExpected, aActual]);
end;

function ComparisonMsg(const aMsg, aExpected, aActual: string;
  const aCheckEqual: boolean): string;
begin
  Result := '"' + aMsg + '"' + ComparisonMsg(aExpected, aActual, aCheckEqual);
end;

procedure FreeObjects(List: TFPList);
var
  i: integer;
begin
  for i:=0 to List.Count - 1 do
    TObject(List.Items[i]).Destroy;
  List.Clear;
end;

procedure GetMethodList(AObject: TObject; AList: TStrings);
var
  Methods: TTypeMemberMethodDynArray;
  i: Integer;
  m: TTypeMemberMethod;
begin
  Methods:=GetClassMethods(TypeInfo(AObject.ClassType));
  for i:=0 to length(Methods)-1 do
  begin
    m:=Methods[i];
    if AList.IndexOf(m.Name)>=0 then continue;
    AList.AddObject(m.Name,TObject(GetInstanceMethod(AObject,m.Name)));
  end;
end;

{ TTestResult }

function TTestResult.GetNumErrors: integer;
begin
  Result := FErrors.Count;
end;

function TTestResult.GetNumFailures: integer;
begin
  Result := FFailures.Count;
end;

function TTestResult.GetNumIgnoredTests: integer;
begin
  Result := FIgnoredTests.Count;
end;

function TTestResult.GetNumSkipped: integer;
begin
  Result := FSkippedTests.Count;
end;

constructor TTestResult.Create;
begin
  inherited Create;
  FFailures       := TFPList.Create;
  FIgnoredTests   := TFPList.Create;
  FErrors         := TFPList.Create;
  FListeners      := TFPList.Create;
  FSkippedTests   := TFPList.Create;
  FStartingTime   := Now;
end;

destructor TTestResult.Destroy;
begin
  FreeObjects(FFailures);
  FreeAndNil(FFailures);
  FreeObjects(FIgnoredTests);
  FreeAndNil(FIgnoredTests);
  FreeObjects(FErrors);
  FreeAndNil(FErrors);
  FreeAndNil(FListeners);
  FreeAndNil(FSkippedTests);
end;

procedure TTestResult.ClearErrorLists;
begin
  FreeObjects(FFailures);
  FFailures.Clear;
  FreeObjects(FIgnoredTests);
  FIgnoredTests.Clear;
  FreeObjects(FErrors);
  FreeAndNil(FErrors);
end;

procedure TTestResult.StartTest(ATest: TTest);
var
  count, i: integer;
begin
  count := ATest.CountTestCases;
  //lock mutex
  FRunTests := FRunTests + count;
  for i := 0 to FListeners.Count - 1 do
    ITestListener(FListeners[i]).StartTest(ATest);
  //unlock mutex
end;

procedure TTestResult.AddFailure(ATest: TTest; E: EAssertionFailedError;
  aFailureList: TFPList);
var
  f: TTestFailure;
  i: Integer;
begin
  //lock mutex
  f := TTestFailure.CreateFailure(ATest, E, ATest.LastStep{, AThrownExceptionAdrs});
  aFailureList.Add(f);
  for i := 0 to FListeners.Count - 1 do
    ITestListener(FListeners[i]).AddFailure(ATest, f);
  //unlock mutex
end;

procedure TTestResult.AddError(ATest: TTest; E: Exception);
var
  f: TTestFailure;
  i: Integer;
begin
  //lock mutex
  f := TTestFailure.CreateFailure(ATest, E, ATest.LastStep{, AThrownExceptionAdrs});
  FErrors.Add(f);
  for i := 0 to FListeners.Count - 1 do
    ITestListener(FListeners[i]).AddError(ATest, f);
  //unlock mutex
end;

procedure TTestResult.EndTest(ATest: TTest);
var
  i: integer;
begin
  for i := 0 to FListeners.Count - 1 do
    ITestListener(FListeners[i]).EndTest(ATest);
end;

procedure TTestResult.AddListener(AListener: ITestListener);
begin
  FListeners.Add(AListener);
end;

procedure TTestResult.RemoveListener(AListener: ITestListener);
begin
  FListeners.Remove(AListener);
end;

procedure ProtectTest(aTest: TTest; aResult: TTestResult);
begin
  if aResult=nil then ;
  TTestCase(aTest).RunBare;
end;

procedure TTestResult.Run(ATestCase: TTestCase);
begin
  if not SkipTest(ATestCase) then
  begin
    StartTest(ATestCase);
    RunProtected(ATestCase, @ProtectTest);
    EndTest(ATestCase);
  end;
end;

procedure TTestResult.RunProtected(ATestCase: TTest; protect: TProtect);
begin
  try
    protect(ATestCase, Self);
  except
    on E: EIgnoredTest do
      AddFailure(ATestCase, E, FIgnoredTests{, ExceptAddr});
    on E: EAssertionFailedError do
      AddFailure(ATestCase, E, FFailures{, ExceptAddr});
    on E: Exception do
      begin
        AddError(ATestCase, E{, ExceptAddr});
      end;
  end;
end;

function TTestResult.WasSuccessful: boolean;
begin
  //lock mutex
    Result := (FErrors.Count = 0) and (FFailures.Count = 0);
  //unlock mutex
end;

function TTestResult.SkipTest(ATestCase: TTestCase): boolean;
var
  i: integer;
begin
  Result := false;
  if FSkippedTests.Count = 0 then
  begin
    result := false;
    Exit;
  end
  else
    for i := 0 to FSkippedTests.Count - 1 do
    begin
      if FSkippedTests[i] = ATestCase then
      begin
        Result := true;
        Exit;
      end;
    end;
end;

procedure TTestResult.AddToSkipList(ATestCase: TTestCase);
begin
  FSkippedTests.Add(ATestCase);
end;

procedure TTestResult.RemoveFromSkipList(ATestCase: TTestCase);
begin
  FSkippedTests.Remove(ATestCase);
end;

procedure TTestResult.StartTestSuite(ATestSuite: TTestSuite);
var
  i: integer;
begin
  for i := 0 to FListeners.Count - 1 do
    ITestListener(FListeners[i]).StartTestSuite(ATestSuite);
end;

procedure TTestResult.EndTestSuite(ATestSuite: TTestSuite);
var
  i: integer;
begin
  for i := 0 to FListeners.Count - 1 do
    ITestListener(FListeners[i]).EndTestSuite(ATestSuite);
end;

type
  { TTestItem }

  TTestItem = Class(TObject)
  private
    FName: String;
    FOwnsTest: Boolean;
    FTest: TTest;
  public
    Constructor Create(T : TTest); reintroduce;
    Destructor Destroy; override;
    Property Test : TTest Read FTest;
    Property TestName : String Read FName;
    Property OwnsTest : Boolean Read FOwnsTest Write FOwnstest;
  end;

constructor TTestItem.Create(T: TTest);
begin
  FTest:=T;
  FName:=T.TestName;
  FOwnsTest:=True;
end;

destructor TTestItem.Destroy;
begin
  if OwnsTest then
    FreeAndNil(FTest);
  inherited Destroy;
end;

{ TTestSuite }

procedure TTestSuite.ClearTests;
begin
  FTests.Clear;
end;

function TTestSuite.DoAddTest(ATest: TTest): Integer;
begin
  Result:=FTests.Add(TTestItem.Create(ATest));
  if ATest.TestSuiteName = '' then
    ATest.TestSuiteName := Self.TestName;
  ATest.EnableIgnores := Self.EnableIgnores;
end;

function TTestSuite.GetTestName: string;
begin
  Result := FName;
end;

function TTestSuite.GetTestSuiteName: string;
begin
  Result := FTestSuiteName;
end;

function TTestSuite.GetEnableIgnores: boolean;
begin
  Result := FEnableIgnores;
end;

procedure TTestSuite.SetTestSuiteName(const aName: string);
begin
  if FTestSuiteName <> aName then
    FTestSuiteName := aName;
end;

procedure TTestSuite.SetTestName(const Value: string);
begin
  FName := Value;
end;

procedure TTestSuite.SetEnableIgnores(Value: boolean);
var
  i: integer;
begin
  if FEnableIgnores <> Value then
  begin
    FEnableIgnores := Value;
    for i := 0 to FTests.Count - 1 do
      TTestItem(FTests[i]).Test.EnableIgnores := Value;
  end
end;

constructor TTestSuite.Create(AClass: TClass; AName: string);
begin
  Create(AClass);
  FName := AName;
end;

constructor TTestSuite.Create(AClass: TClass);
var
  i,j: integer;
  tc: TTestCaseClass;
  C : TTestCase;
  SN : String;
  ml: TTypeMemberMethodDynArray;

begin
  TAssert.AssertNotNull(AClass);
  Create(AClass.ClassName);
  if AClass.InheritsFrom(TTestCase) then
  begin
    tc := TTestCaseClass(AClass);
    ml:=GetClassMethods(TypeInfo(AClass));
    SN:=tc.ClassName;
    if tc.SingleInstanceForSuite then
      begin
      c:=tc.CreateWith('',SN);
      for i := 0 to length(ml) -1 do
        begin
        C.TestName:=ml[i].Name;
        J:=DoAddTest(C);
        TTestItem(FTests[J]).OwnsTest:=(I=0);
        end;
      end
    else
      for i := 0 to length(ml) -1 do
        AddTest(tc.CreateWith(ml[i].Name, SN));
  end
  else
    AddTest(Warning(AClass.ClassName + SNoValidInheritance));
  if FTests.Count = 0 then
    AddTest(Warning(SNoValidTests + AClass.ClassName));
end;

constructor TTestSuite.Create(AClassArray: array of TClass);
var
  i: integer;
begin
  Create;
  for i := Low(AClassArray) to High(AClassArray) do
    if Assigned(AClassArray[i]) then
      AddTest(TTestSuite.Create(AClassArray[i]));
end;

constructor TTestSuite.Create(AName: string);
begin
  Create();
  FName := AName;
end;

constructor TTestSuite.Create;
begin
  inherited Create;
  FTests := TFPList.Create;
  FEnableIgnores := True;
end;

destructor TTestSuite.Destroy;
begin
  FreeObjects(FTests);
  FreeAndNil(FTests);
  inherited Destroy;
end;

function TTestSuite.CountTestCases: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FTests.Count - 1 do
  begin
    Result := Result + TTestItem(FTests[i]).Test.CountTestCases;
  end;
end;

function TTestSuite.GetChildTestCount: Integer;
begin
  Result:=FTests.Count;
end;

function TTestSuite.GetChildTest(AIndex: Integer): TTest;
begin
  Result := TTestItem(FTests[AIndex]).Test;
end;

procedure TTestSuite.Run(AResult: TTestResult);
var
  i: integer;
  ti : TTestItem;

begin
  if FTests.Count > 0 then
    AResult.StartTestSuite(self);

  for i := 0 to FTests.Count - 1 do
    begin
    ti:=TTestItem(FTests[i]);
    if Ti.Test.InheritsFrom(TTestCase) and TTestCase(Ti.Test).SingleInstanceForSuite then
      TTestCase(Ti.Test).SetTestName(Ti.TestName);
    RunTest(TI.Test, AResult);
    end;

  if FTests.Count > 0 then
    AResult.EndTestSuite(self);
end;

procedure TTestSuite.RunTest(ATest: TTest; AResult: TTestResult);
begin
  ATest.Run(AResult);
end;

procedure TTestSuite.AddTest(ATest: TTest);
begin
  DoAddTest(ATest);
end;

procedure TTestSuite.AddTestSuiteFromClass(ATestClass: TClass);
begin
  AddTest(TTestSuite.Create(ATestClass));
end;

type
  TTestWarning = class(TTestCase)
  private
    FMessage: String;
  protected
    procedure RunTest; override;
  end;

procedure TTestWarning.RunTest;
begin
  Fail(FMessage);
end;

class function TTestSuite.Warning(const aMessage: string): TTestCase;
var
  w: TTestWarning;
begin
  w := TTestWarning.Create;
  w.FMessage := aMessage;
  Result := w;
end;

{ TTestCase }

function TTestCase.CreateResult: TTestResult;
begin
  Result := TTestResult.Create;
end;

procedure TTestCase.SetUp;
begin
  { do nothing }
end;

procedure TTestCase.TearDown;
begin
  { do nothing }
end;

procedure TTestCase.RunTest;
var
  RunMethod: TRunMethod;
  FailMessage : String;
begin
  AssertNotNull('name of the test not assigned', FName);
  RunMethod:=TRunMethod(GetInstanceMethod(Self,FName));
  if Assigned(RunMethod) then
  begin
    ExpectException('',Nil,'',0);
    try
      AssertCount:=0;
      FailMessage:='';
      RunMethod;
      if (FExpectedException<>Nil) then
        FailMessage:=Format(SExceptionCompare, [FExpectedException.ClassName, SNoException]);
      if CheckAssertCalled and (AssertCount=0) then
        FailMessage:=SAssertNotCalled;
    except
      On E : Exception do
        begin
        if FExpectedException=Nil then
          Raise;
        If not (E is FExpectedException) then
          FailMessage:=Format(SExceptionCompare, [FExpectedException.ClassName, E.ClassName]);
        if (FExpectedExceptionMessage<>'') then
          if (FExpectedExceptionMessage<>E.Message) then
            FailMessage:=Format(SExceptionmessageCompare+SCompare, [FExpectedExceptionMessage,E.Message]);
        if (FExpectedExceptionContext<>0) then
          if (FExpectedExceptionContext<>E.HelpContext) then
            FailMessage:=Format(SExceptionHelpContextCompare+SCompare, [IntToStr(FExpectedExceptionContext),IntToStr(E.HelpContext)])
        end;
    end;
    if (FailMessage<>'') then
      begin
      if (FExpectedExceptionFailMessage<>'') then
        FailMessage:=' : '+FailMessage;
      Fail(FExpectedExceptionFailMessage+FailMessage{,FExpectedExceptionCaller});
      end;
  end
  else
    begin
      Fail(format(SMethodNotFound, [FName]));
    end;
end;

function TTestCase.GetTestName: string;
begin
  Result := FName;
end;

function TTestCase.GetTestSuiteName: string;
begin
  Result := FTestSuiteName;
end;

function TTestCase.GetEnableIgnores: boolean;
begin
  Result := FEnableIgnores;
end;

procedure TTestCase.SetTestSuiteName(const aName: string);
begin
  if FTestSuiteName <> aName then
    FTestSuiteName := aName;
end;

procedure TTestCase.SetTestName(const Value: string);
begin
  FName := Value;
end;

procedure TTestCase.SetEnableIgnores(Value: boolean);
begin
  FEnableIgnores := Value;
end;

procedure TTestCase.RunBare;
begin
  FLastStep := stSetUp;
  SetUp;
  try
    FLastStep := stRunTest;
    RunTest;
    FLastStep := stTearDown;
  finally
    TearDown;
  end;
  FLastStep := stNothing;
end;

class function TTestCase.SingleInstanceForSuite: Boolean;
begin
  Result:=False;
end;

constructor TTestCase.Create;
begin
  inherited Create;
  FEnableIgnores := True;
end;

constructor TTestCase.CreateWith(const ATestName: string;
  const ATestSuiteName: string);
begin
  Create;
  FName := ATestName;
  FTestSuiteName := ATestSuiteName;
end;

constructor TTestCase.CreateWithName(const AName: string);
begin
  Create;
  FName := AName;
end;

procedure TTestCase.ExpectException(AExceptionClass: TClass;
  AExceptionMessage: string; AExceptionHelpContext: Integer);
begin
  FExpectedExceptionFailMessage:='';
  FExpectedException:=AExceptionClass;
  FExpectedExceptionMessage:=AExceptionMessage;
  FExpectedExceptionContext:=AExceptionHelpContext;
  //FExpectedExceptionCaller:=CallerAddr;
end;

procedure TTestCase.ExpectException(const Msg: String; AExceptionClass: TClass;
  AExceptionMessage: string; AExceptionHelpContext: Integer);
begin
  FExpectedExceptionFailMessage:=Msg;
  FExpectedException:=AExceptionClass;
  FExpectedExceptionMessage:=AExceptionMessage;
  FExpectedExceptionContext:=AExceptionHelpContext;
//  FExpectedExceptionCaller:=CallerAddr;
end;

function TTestCase.CountTestCases: integer;
begin
  Result := 1;
end;

function TTestCase.CreateResultAndRun: TTestResult;
begin
  Result := CreateResult;
  Run(Result);
end;

procedure TTestCase.Run(AResult: TTestResult);
begin
  AResult.Run(Self);
end;

function TTestCase.AsString: string;
begin
  Result := TestName + '(' + ClassName + ')';
end;

{ TTestFailure }

function TTestFailure.GetAsString: string;
var
  s: string;
begin
  if FTestSuiteName <> '' then
    s := FTestSuiteName + '.'
  else
    s := '';
  Result := s + FTestName + ': ' + FRaisedExceptionMessage;
end;

function TTestFailure.GetExceptionMessage: string;
begin
  Result := FRaisedExceptionMessage;
  if TestLastStep = stSetUp then
    Result := '[SETUP] ' + Result
  else if TestLastStep = stTearDown then
    Result := '[TEARDOWN] ' + Result;
end;

function TTestFailure.GetIsFailure: boolean;
begin
  Result := FRaisedExceptionClass.InheritsFrom(EAssertionFailedError);
end;

function TTestFailure.GetIsIgnoredTest: boolean;
begin
  Result := FRaisedExceptionClass.InheritsFrom(EIgnoredTest);
end;

function TTestFailure.GetExceptionClassName: string;
begin
  if Assigned(FRaisedExceptionClass) then
    Result := FRaisedExceptionClass.ClassName
  else
    Result := '<NIL>'
end;

procedure TTestFailure.SetTestLastStep(const Value: TTestStep);
begin
  FTestLastStep := Value;
end;

constructor TTestFailure.CreateFailure(ATest: TTest; E: Exception;
  LastStep: TTestStep);
begin
  inherited Create;
  FTestName := ATest.GetTestName;
  FTestSuiteName := ATest.GetTestSuiteName;
  FRaisedExceptionClass := E.ClassType;
  FRaisedExceptionMessage := E.Message;
  //FThrownExceptionAddress := ThrownExceptionAddrs;
  FTestLastStep := LastStep;
end;

{ TAssert }

class procedure TAssert.Fail(const AMessage: string);
begin
  Inc(AssertCount);
  raise EAssertionFailedError.Create(AMessage);
end;

class procedure TAssert.Fail(const AFmt: string; Args: array of string);
begin
  Inc(AssertCount);
  raise EAssertionFailedError.CreateFmt(AFmt,Args);
end;

class procedure TAssert.FailEquals(const expected, actual: string;
  const ErrorMsg: string);
begin
  Fail(EqualsErrorMessage(expected, actual, ErrorMsg));
end;

class procedure TAssert.FailNotEquals(const expected, actual: string;
  const ErrorMsg: string);
begin
  Fail(NotEqualsErrorMessage(expected, actual, ErrorMsg));
end;

class procedure TAssert.AssertTrue(const AMessage: string; ACondition: boolean);
begin
  if (not ACondition) then
    Fail(AMessage)
  else
    Inc(AssertCount); // Fail will increae AssertCount
end;

class procedure TAssert.AssertTrue(ACondition: boolean);
begin
  AssertTrue('', ACondition);
end;

class procedure TAssert.AssertFalse(const AMessage: string; ACondition: boolean
  );
begin
  AssertTrue(AMessage, not ACondition);
end;

class procedure TAssert.AssertFalse(ACondition: boolean);
begin
  AssertFalse('', ACondition);
end;

class procedure TAssert.AssertEquals(const AMessage: string; Expected,
  Actual: string);
begin
  AssertTrue(ComparisonMsg(AMessage, Expected, Actual), Expected=Actual);
end;

class procedure TAssert.AssertEquals(Expected, Actual: string);
begin
  AssertTrue(ComparisonMsg(Expected, Actual), Expected=Actual);
end;

class procedure TAssert.AssertEquals(const AMessage: string; Expected,
  Actual: NativeInt);
begin
  AssertTrue(ComparisonMsg(AMessage,IntToStr(Expected), IntToStr(Actual)), Expected = Actual);
end;

class procedure TAssert.AssertEquals(Expected, Actual: NativeInt);
begin
  AssertTrue(ComparisonMsg(IntToStr(Expected), IntToStr(Actual)), Expected = Actual);
end;

class procedure TAssert.AssertEquals(const AMessage: string; Expected, Actual,
  Delta: double);
begin
  AssertTrue(ComparisonMsg(AMessage,FloatToStr(Expected),FloatToStr(Actual)),
    (Abs(Expected - Actual) <= Delta));
end;

class procedure TAssert.AssertEquals(Expected, Actual, Delta: double);
begin
  AssertTrue(ComparisonMsg(FloatToStr(Expected),FloatToStr(Actual)),
    (Abs(Expected - Actual) <= Delta));
end;

class procedure TAssert.AssertEquals(const AMessage: string; Expected,
  Actual: boolean);
begin
  AssertTrue(ComparisonMsg(AMessage,BoolToStr(Expected, true), BoolToStr(Actual, true)),
    Expected = Actual);
end;

class procedure TAssert.AssertEquals(Expected, Actual: boolean);
begin
  AssertTrue(ComparisonMsg(BoolToStr(Expected, true), BoolToStr(Actual, true)),
    Expected = Actual);
end;

class procedure TAssert.AssertEquals(const AMessage: string; Expected,
  Actual: char);
begin
  AssertTrue(ComparisonMsg(AMessage,Expected, Actual), Expected = Actual);
end;

class procedure TAssert.AssertEquals(Expected, Actual: char);
begin
  AssertTrue(ComparisonMsg(Expected, Actual), Expected = Actual);
end;

class procedure TAssert.AssertEquals(const AMessage: string; Expected,
  Actual: TClass);
begin
  AssertTrue(ComparisonMsg(AMessage,GetN(Expected), GetN(Actual)), Expected = Actual);
end;

class procedure TAssert.AssertEquals(Expected, Actual: TClass);
begin
  AssertTrue(ComparisonMsg(GetN(Expected), GetN(Actual)), Expected = Actual);
end;

class procedure TAssert.AssertSame(const AMessage: string; Expected,
  Actual: TObject);
begin
  AssertTrue(ComparisonMsg(AMessage,GetN(Expected), GetN(Actual)), Expected = Actual);
end;

class procedure TAssert.AssertSame(Expected, Actual: TObject);
begin
  AssertTrue(ComparisonMsg(GetN(Expected), GetN(Actual)), Expected = Actual);
end;

class procedure TAssert.AssertSame(const AMessage: string; Expected,
  Actual: Pointer);
begin
  AssertTrue(ComparisonMsg(AMessage,GetPtrN(Expected), GetPtrN(Actual)), Expected = Actual);
end;

class procedure TAssert.AssertSame(Expected, Actual: Pointer);
begin
  AssertTrue(ComparisonMsg(GetPtrN(Expected), GetPtrN(Actual)), Expected = Actual);
end;

class procedure TAssert.AssertNotSame(const AMessage: string; Expected,
  Actual: TObject);
begin
  AssertFalse('"' + aMessage + '"' + SExpectedNotSame, Expected = Actual);
end;

class procedure TAssert.AssertNotSame(Expected, Actual: TObject);
begin
  AssertFalse(SExpectedNotSame, Expected = Actual);
end;

class procedure TAssert.AssertNotSame(const AMessage: string; Expected,
  Actual: Pointer);
begin
  AssertFalse('"' + aMessage + '"' + SExpectedNotSame, Expected = Actual);
end;

class procedure TAssert.AssertNotSame(Expected, Actual: Pointer);
begin
  AssertFalse(SExpectedNotSame, Expected = Actual);
end;

class procedure TAssert.AssertNotNull(const AMessage: string; AObject: TObject);
begin
  AssertTrue(AMessage, (AObject <> nil));
end;

class procedure TAssert.AssertNotNull(AObject: TObject);
begin
  AssertTrue('',(AObject <> nil));
end;

class procedure TAssert.AssertNotNull(const AMessage: string; APointer: Pointer
  );
begin
  AssertTrue(AMessage, (APointer <> nil));
end;

class procedure TAssert.AssertNotNull(APointer: Pointer);
begin
  AssertTrue('', (APointer <> nil));
end;

class procedure TAssert.AssertNull(const AMessage: string; AObject: TObject);
begin
  AssertTrue(AMessage, (AObject = nil));
end;

class procedure TAssert.AssertNull(AObject: TObject);
begin
  AssertTrue('',(AObject = nil));
end;

class procedure TAssert.AssertNull(const AMessage: string; APointer: Pointer);
begin
  AssertTrue(AMessage, (APointer = nil));
end;

class procedure TAssert.AssertNull(APointer: Pointer);
begin
  AssertTrue('', (APointer = nil));
end;

class procedure TAssert.AssertNotNull(const AMessage, AString: string);
begin
  AssertTrue(AMessage, AString <> '');
end;

class procedure TAssert.AssertNotNull(const AString: string);
begin
  AssertNotNull('', AString);
end;

class procedure TAssert.AssertException(const AMessage: string;
  AExceptionClass: ExceptClass; const AMethod: TRunMethod;
  const AExceptionMessage: String; AExceptionContext: Integer);

  Function MisMatch (AClassName : String) : String;

  begin
    Result:=Format(SExceptionCompare,[AExceptionClass.ClassName, AClassName])
  end;

var
  FailMsg : string;
begin
  FailMsg:='';
  try
    AMethod;
    FailMsg:=MisMatch(SNoException);
  except
    on E: Exception do
      begin
      if Not E.ClassType.InheritsFrom(AExceptionClass) then
        FailMsg:=MisMatch(E.ClassName)
      else if not (AExceptionClass.ClassName = E.ClassName) then
        FailMsg:=MisMatch(E.ClassName)
      else if (AExceptionMessage<>'') and (AExceptionMessage<>E.Message) then
        FailMsg:=ComparisonMsg(SExceptionMessageCompare,AExceptionMessage,E.Message)
      else if (AExceptionContext<>0) and (AExceptionContext<>E.HelpContext) then
        FailMsg:=ComparisonMsg(SExceptionHelpContextCompare,IntToStr(AExceptionContext),IntToStr(E.HelpContext))
      end;
  end;
  AssertTrue(AMessage + FailMsg, FailMsg='');
end;

class procedure TAssert.AssertException(AExceptionClass: ExceptClass;
  const AMethod: TRunMethod; const AExceptionMessage: String;
  AExceptionContext: Integer);
begin
  AssertException('', AExceptionClass, AMethod, AExceptionMessage, AExceptionContext);
end;

class procedure TAssert.Check(pValue: boolean; pMessage: string);
begin
  AssertTrue(pMessage, pValue);
end;

class procedure TAssert.CheckEquals(expected, actual: double; msg: string);
begin
  CheckEquals(expected, actual, 0, msg);
end;

class procedure TAssert.CheckEquals(expected, actual: double; delta: double;
  msg: string);
begin
  AssertEquals(msg, expected, actual, delta);
end;

class procedure TAssert.CheckEquals(expected, actual: string; msg: string);
begin
  AssertEquals(msg, expected, actual);
end;

class procedure TAssert.CheckEquals(expected, actual: integer; msg: string);
begin
  AssertEquals(msg, expected, actual);
end;

class procedure TAssert.CheckEquals(expected, actual: boolean; msg: string);
begin
  AssertEquals(msg, expected, actual);
end;

class procedure TAssert.CheckEquals(expected, actual: TClass; msg: string);
begin
  AssertEquals(msg, expected, actual);
end;

class procedure TAssert.CheckNotEquals(expected, actual: string; msg: string);
begin
  if Expected=Actual then
    Fail(msg + ComparisonMsg(Expected, Actual, false));
end;

class procedure TAssert.CheckNotEquals(expected, actual: integer; msg: string);
begin
  if (expected = actual) then
    Fail(msg + ComparisonMsg(IntToStr(expected), IntToStr(actual), false));
end;

class procedure TAssert.CheckNotEquals(expected, actual: boolean; msg: string);
begin
  if (expected = actual) then
    Fail(msg + ComparisonMsg(BoolToStr(expected), BoolToStr(actual), false));
end;

class procedure TAssert.CheckNotEquals(expected, actual: double; delta: double;
  msg: string);
begin
  if (abs(expected-actual) <= delta) then
    FailNotEquals(FloatToStr(expected), FloatToStr(actual), msg);
end;

class procedure TAssert.CheckNull(obj: TObject; msg: string);
begin
  AssertNull(msg, obj);
end;

class procedure TAssert.CheckNotNull(obj: TObject; msg: string);
begin
  AssertNotNull(msg, obj);
end;

class procedure TAssert.CheckIs(obj: TObject; pClass: TClass; msg: string);
begin
  if pClass=nil then
    Fail('TAssert.CheckIs pClass=nil');
  if obj = nil then
    Fail(ComparisonMsg(msg,pClass.ClassName, 'nil'))
  else if not obj.ClassType.InheritsFrom(pClass) then
    Fail(ComparisonMsg(msg,pClass.ClassName, obj.ClassName));
end;

class procedure TAssert.CheckSame(expected, actual: TObject; msg: string);
begin
  AssertSame(msg, expected, actual);
end;

class procedure TAssert.CheckTrue(condition: Boolean; msg: string);
begin
  if (not condition) then
    FailNotEquals(BoolToStr(true, true), BoolToStr(false, true), msg);
end;

class procedure TAssert.CheckFalse(condition: Boolean; msg: string);
begin
  if (condition) then
    FailNotEquals(BoolToStr(false, true), BoolToStr(true, true), msg);
end;

class procedure TAssert.CheckException(const AMethod: TRunMethod;
  AExceptionClass: ExceptClass; msg: string);
begin
  AssertException(msg, AExceptionClass, AMethod);
end;

class function TAssert.EqualsErrorMessage(const expected, actual: string;
  const ErrorMsg: string): string;
begin
  if (ErrorMsg <> '') then
    Result := Format(sExpectedButWasAndMessageFmt, [ErrorMsg + ', ', expected, actual])
  else
    Result := Format(sExpectedButWasFmt, [expected, actual])
end;

class function TAssert.NotEqualsErrorMessage(const expected, actual: string;
  const ErrorMsg: string): string;
begin
  if (ErrorMsg <> '') then
    Result := Format(sExpectedButWasAndMessageFmt, [ErrorMsg, expected, actual])
  else
    Result := Format(sExpectedButWasFmt, [expected, actual]);
end;

class function TAssert.Suite: TTest;
begin
  Result := TTestSuite.Create(Self);
end;

{ TTest }

function TTest.GetTestName: string;
begin
  Result := 'TTest';
end;

function TTest.GetTestSuiteName: string;
begin
  Result := 'TTest';
end;

function TTest.GetEnableIgnores: boolean;
begin
  Result := True;
end;

function TTest.CountTestCases: integer;
begin
  Result := 0;
end;

function TTest.GetChildTestCount: Integer;
begin
  Result:=0;
end;

function TTest.GetChildTest(AIndex: Integer): TTest;
begin
  Result:=Nil;
  if AIndex=0 then ;
end;

function TTest.FindChildTest(const AName: String): TTest;
Var
  I : Integer;

begin
  Result:=Nil;
  I:=GetChildTestCount-1;
  While (Result=Nil) and (I>=0) do
    begin
    Result:=GetChildTest(I);
    if CompareText(Result.TestName,AName)<>0 then
      Result:=Nil;
    Dec(I);
    end;
end;

function TTest.FindTest(const AName: String): TTest;
Var
  S : String;
  I,P : Integer;

begin
  Result:=Nil;
  S:=AName;
  if S='' then exit;
  P:=Pos('.',S);
  If (P=0) then
    P:=Length(S)+1;
  Result:=FindChildTest(Copy(S,1,P-1));
  if (Result<>Nil) then
    begin
    Delete(S,1,P);
    If (S<>'') then
      Result:=Result.FindTest(S);
    end
  else
    begin
    P:=GetChildTestCount;
    I:=0;
    While (Result=Nil) and (I<P) do
      begin
      Result:=GetChildTest(I).FindTest(Aname);
      Inc(I);
      end;
    end;
end;

procedure TTest.Run(AResult: TTestResult);
begin
  { do nothing }
  if AResult=nil then ;
end;

procedure TTest.Ignore(const AMessage: string);
begin
  if EnableIgnores then raise EIgnoredTest.Create(AMessage);
end;

end.

