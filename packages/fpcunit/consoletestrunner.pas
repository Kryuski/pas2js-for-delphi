{   This unit contains the TTestRunner class, a base class for the console test
    runner for fpcunit.

    This file is part of the Free Component Library (FCL)
    Copyright (C) 2006 Vincent Snijders

    Port to Pas2JS by Mattias Gaertner in 2017.

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
unit ConsoleTestRunner;

{$mode objfpc}

interface

uses
  NodeJSApp, Classes, SysUtils,
  FPCUnit, TestRegistry, TestDecorator,
  //testutils,
  FPCUnitReport,
  //latextestreport,
  //xmltestreport,
  PlainTestReport
  //dom
  ;

const
  Version = '0.3';

type
  TFormat = (
    fPlain,
    //fLatex,
    //fXML,
    fPlainNoTiming
    );

var
  DefaultFormat : TFormat = fPlain; // fXML;
  DefaultRunAllTests : Boolean = False;

type
  { TTestRunner }

  TTestRunner = class(TNodeJSApplication)
  private
    FShowProgress: boolean;
    FFileName: string;
    FStyleSheet: string;
    FLongOpts: TStrings;
    FFormatParam: TFormat;
  protected
    property FileName: string read FFileName write FFileName;
    property LongOpts: TStrings read FLongOpts write FLongOpts;
    property ShowProgress: boolean read FShowProgress write FShowProgress;
    property StyleSheet: string read FStyleSheet write FStyleSheet;
    property FormatParam: TFormat read FFormatParam write FFormatParam;
    procedure DoRun; override;
    procedure DoTestRun(ATest: TTest); virtual;
    function GetShortOpts: string; virtual;
    procedure AppendLongOpts; virtual;
    procedure WriteCustomHelp; virtual;
    procedure ParseOptions; virtual;
    //procedure ExtendXmlDocument(Doc: TXMLDocument); virtual;
    function GetResultsWriter: TCustomResultsWriter; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

const
  ShortOpts = 'alhp';
  DefaultLongOpts: array of string =
     ('all', 'list', 'progress', 'help', 'skiptiming',
      'suite:', 'format:', 'file:', 'stylesheet:','sparse','no-addresses');
type
  { TDecoratorTestSuite }

  TDecoratorTestSuite = Class(TTestSuite)
  public
    Procedure  FreeDecorators(T : TTest);
    Destructor Destroy; override;
  end;

procedure TDecoratorTestSuite.FreeDecorators(T: TTest);
Var
  I : Integer;
begin
  If (T is TTestSuite) then
    for I:=0 to TTestSuite(t).ChildTestCount-1 do
      FreeDecorators(TTest(TTestSuite(t).Test[i]));
  if (T is TTestDecorator) and (TTestDecorator(T).Test is TDecoratorTestSuite) then
    T.Destroy;
end;

destructor TDecoratorTestSuite.Destroy;
begin
  FreeDecorators(Self);
  // We need to find something for this.
  ClearTests;
  inherited Destroy;
end;

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

procedure TTestRunner.DoRun;
var
  I,P : integer;
  S,TN : string;
  TS : TDecoratorTestSuite;
  T : TTest;

begin
  S := CheckOptions(GetShortOpts, LongOpts);
  if (S <> '') then
    Writeln(S);

  ParseOptions;

  //get a list of all registed tests
  if HasOption('l', 'list') then
    case FormatParam of
      //fLatex:         Write(GetSuiteAsLatex(GetTestRegistry));
      fPlain:         Write(GetSuiteAsPlain(GetTestRegistry));
      fPlainNoTiming: Write(GetSuiteAsPlain(GetTestRegistry));
    else
      //Write(GetSuiteAsXml(GetTestRegistry));
      Write(GetSuiteAsPlain(GetTestRegistry));
    end;

  //run the tests
  if HasOption('suite') then
  begin
    S := '';
    S := GetOptionValue('suite');
    if S = '' then
      for I := 0 to GetTestRegistry.ChildTestCount - 1 do
        writeln(GetTestRegistry[i].TestName)
    else
      begin
        TS:=TDecoratorTestSuite.Create('SuiteList');
        try
        while Not(S = '') Do
          begin
          P:=Pos(',',S);
          If P=0 then
            P:=Length(S)+1;
          TN:=Copy(S,1,P-1);
          Delete(S,1,P);
          if (TN<>'') then
            begin
            T:=GetTestRegistry.FindTest(TN);
            if Assigned(T) then
              TS.AddTest(T);
            end;
          end;
          if (TS.CountTestCases>1) then
            DoTestRun(TS)
          else if TS.CountTestCases=1 then
            DoTestRun(TS[0])
          else
            Writeln('No tests selected.');
        finally
          FreeAndNil(TS);
        end;
      end;
  end
  else if HasOption('a', 'all') or (DefaultRunAllTests and Not HasOption('l','list')) then
    DoTestRun(GetTestRegistry) ;
  Terminate;
end;

procedure TTestRunner.DoTestRun(ATest: TTest);
var
  ResultsWriter: TCustomResultsWriter;
  ProgressWriter: TProgressWriter;
  TestResult: TTestResult;
begin
  ResultsWriter := GetResultsWriter;
  ResultsWriter.Filename := FileName;
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

function TTestRunner.GetShortOpts: string;
begin
  Result := ShortOpts;
end;

procedure TTestRunner.AppendLongOpts;
var
  i: Integer;
begin
  for i := low(DefaultLongOpts) to Length(DefaultLongOpts)-1 do
    LongOpts.Add(DefaultLongOpts[i]);
end;

procedure TTestRunner.WriteCustomHelp;
begin
  // no custom help options in base class
end;

procedure TTestRunner.ParseOptions;
begin
  if HasOption('h', 'help') or ((ParamCount = 0) and not DefaultRunAllTests) then
  begin
    writeln(Title);
    writeln(Version);
    writeln;
    writeln('Usage: ');
    writeln('  --format=latex            output as latex source (only list implemented)');
    writeln('  --format=plain            output as plain ASCII source');
    writeln('  --format=xml              output as XML source (default)');
    writeln('  --skiptiming              Do not output timings (useful for diffs of testruns)');
    writeln('  --sparse                  Produce Less output (errors/failures only)');
    writeln('  --no-addresses            Do not display address info');
    writeln('  --stylesheet=<reference>   add stylesheet reference');
    writeln('  --file=<filename>         output results to file');
    writeln;
    writeln('  -l or --list              show a list of registered tests');
    writeln('  -a or --all               run all tests');
    writeln('  -p or --progress          show progress');
    writeln('  --suite=MyTestSuiteName   run single test suite class');
    WriteCustomHelp;
    writeln;
    writeln('The results can be redirected to an xml file,');
    writeln('for example: ', ParamStr(0),' --all > results.xml');
  end;

  //get the format parameter
  FormatParam := DefaultFormat;
  if HasOption('format') then
  begin
    //if CompareText(GetOptionValue('format'),'latex')=0 then
    //  FormatParam := fLatex
    if CompareText(GetOptionValue('format'),'plain')=0 then
      FormatParam := fPlain
    else if CompareText(GetOptionValue('format'),'plainnotiming')=0 then
      FormatParam := fPlainNoTiming;
    //else if CompareText(GetOptionValue('format'),'xml')=0 then
    //  FormatParam := fXML;
  end;

  ShowProgress := HasOption('p', 'progress');

  if HasOption('file') then
    FileName := GetOptionValue('file');
  if HasOption('stylesheet') then
    StyleSheet := GetOptionValue('stylesheet');
end;

function TTestRunner.GetResultsWriter: TCustomResultsWriter;
begin
  case FormatParam of
    //fLatex:         Result := TLatexResultsWriter.Create(nil);
    fPlain:         Result := TPlainResultsWriter.Create(nil);
  else
    begin
      Result := TPlainResultsWriter.Create(nil);
      //Result := TXmlResultsWriter.Create(nil);
      //ExtendXmlDocument(TXMLResultsWriter(Result).Document);
    end;
  end;
  Result.SkipTiming:=HasOption('skiptiming');
  Result.Sparse:=HasOption('sparse');
  Result.SkipAddressInfo:=HasOption('no-addresses');
end;

constructor TTestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLongOpts := TStringList.Create;
  AppendLongOpts;
end;

destructor TTestRunner.Destroy;
begin
  FreeAndNil(FLongOpts);
  inherited Destroy;
end;

end.

