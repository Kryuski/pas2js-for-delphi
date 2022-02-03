unit htmltestreport;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FPCUnit, FPCUnitReport, web;

Type

  { TTestTreeBuilder }

  TTestTreeBuilder = Class
  private
    FBaseElement: TJSElement;
    FBaseElementID: String;
  Public
    Constructor Create(aBaseElementID : String); Virtual; reintroduce;
    Procedure ShowSuite(aSuite : TTestSuite); virtual; abstract;
    Property BaseElement : TJSElement Read FBaseElement;
    Property BaseElementID : String Read FBaseElementID;
  end;

  { THTMLTreeBuilder }

  THTMLTreeBuilder = Class(TTestTreeBuilder)
  private
    FIDPrefix : String;
    procedure AppendSuite(Parent: TJSElement; aSuite: TTestSuite; aLevel: integer);
    procedure PopIDPrefix(aPrefix: String);
    procedure PushIDPrefix(aPrefix: String);
    procedure ShowStatsHeader(aParent: TJSElement);
  Public
    Procedure ShowSuite(aSuite : TTestSuite); override;
  end;

  { THTMLResultsWriter }

  THTMLResultsWriter = class(TCustomResultsWriter)
  private
    FBaseElementID: String;
    FIDPrefix : String;
    procedure AppendStats(AParent: TJSElement; ATiming: TDateTime; ANumRuns, ANumErrors, ANumFailures, ANumIgnores: Integer);
    procedure SetBaseElementID(AValue: String);
  protected
    procedure PopIDPrefix(aPrefix: String);
    procedure PushIDPrefix(aPrefix: String);
    procedure SetSkipAddressInfo(AValue: Boolean); override;
    procedure SetSparse(AValue: Boolean); override;
    procedure WriteTestHeader(ATest: TTest; ALevel: integer; ACount: integer); override;
    procedure WriteTestFooter(ATest: TTest; ALevel: integer; ATiming: TDateTime); override;
    procedure WriteSuiteHeader(ATestSuite: TTestSuite; ALevel: integer); override;
    procedure WriteSuiteFooter(ATestSuite: TTestSuite; ALevel: integer;
      ATiming: TDateTime; ANumRuns: integer; ANumErrors: integer;
      ANumFailures: integer; ANumIgnores: integer); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
    procedure WriteResult(aResult: TTestResult); override;
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure); override;
    procedure AddError(ATest: TTest; AError: TTestFailure); override;
    // Base element ID where to display the testsuite. Default is 'fpcunit'
    Property BaseElementID : String Read FBaseElementID Write SetBaseElementID;
  end;

implementation

{ TTestTreeBuilder }

constructor TTestTreeBuilder.create(aBaseElementID: String);

begin
  FBaseElementID:=aBaseElementID;
  if FBaseElementID='' then
     FBaseElementID:='fpcunit';
  FBaseElement:=document.getElementById(FBaseElementID);
end;

{ THTMLTreeBuilder }

procedure THTMLTreeBuilder.ShowStatsHeader(aParent : TJSElement);

Var
  SEL : TJSHTMLELement;

  Procedure addc(s,i : string);
  var
    LEL : TJSHTMLELement;

  begin
    LEL:=TJSHTMLELement(document.CreateElement('li'));
    LEL.className:=S;
    SEL.AppendChild(LEL);
    LEL.InnerHTML:=i;
  end;

begin
  SEL:=TJSHTMLELement(document.CreateElement('ul'));
  SEL.id:=FIDPrefix+'-stats';
  Addc('progress','Progress: <em>0</em>');
  Addc('passes','Pass: <em>0</em>');
  Addc('failures','Fail: <em>0</em>');
  Addc('duration','duration: <em>0</em> ms');
  aParent.Append(SEL);

end;
procedure THTMLTreeBuilder.ShowSuite(aSuite: TTestSuite);

Var
  RU : TJSHTMLElement;
  N : TJSElement;

begin
  FIDPrefix:=BaseElementID;
  if (BaseElement=Nil) then
    Console.error('No base element available to append test tree to!!');
  N:=BaseElement.firstElementChild;
  While (N<>Nil) do
    begin
    BaseElement.removeChild(N);
    N:=BaseElement.firstElementChild;
    end;
  ShowStatsHeader(BaseElement);
  RU:=TJSHTMLElement(document.createElement('ul'));
  BaseElement.appendChild(RU);
  AppendSuite(RU,aSuite,0);
end;

procedure THTMLTreeBuilder.PushIDPrefix(aPrefix : String) ;

begin
  if aPrefix='' then
    exit;
  if (FIDPrefix<>'') then
    FIDPrefix:=FIDPrefix+'-';
  FIDPrefix:=FIDPrefix+aPrefix;
end;

procedure THTMLTreeBuilder.PopIDPrefix(aPrefix : String) ;

Var
  L : integer;
begin
  L:=Pos(aPrefix,FIDPrefix);
  if (L+Length(APrefix)-1)=Length(FIDPrefix) then
    FIDPrefix:=Copy(FIDPrefix,1,L-2);
end;

procedure THTMLTreeBuilder.AppendSuite(Parent : TJSElement; aSuite : TTestSuite; aLevel : integer);

Var
  SE,SU,SH,Ti,TH : TJSHTMLElement;
  T : TTest;
  I : Integer;

begin
  SE:=TJSHTMLElement(Document.createElement('li'));
  SE.ClassName:='suite level'+intToStr(aLevel);
  SH:=TJSHTMLElement(Document.createElement('h1'));
  if aLevel=0 then
    begin
    SH.innerText:='FPCUnit Test Suite'
    end
  else
    begin
    SH.innerText:=aSuite.TestName;
    end;
  SE.appendChild(SH);
  PushIDPrefix(aSuite.TestName);
  try
    if (aSuite.TestName='') then
      SE.id:=FIDPrefix+'-root-'+IntToStr(aLevel)
    else
      SE.id:=FIDPrefix;
    SU:=TJSHTMLElement(Document.createElement('ul'));
    SU.className:='suite level'+intToStr(aLevel);
    SE.AppendChild(SU);
    for I:=0 to aSuite.GetChildTestCount-1 do
      begin
      T:=asuite.GetChildTest(i);
//      Writeln('Examining ',T.TestSuiteName,'.',T.TestName,' (',t.className,')');
      if T is TTestSuite then
        AppendSuite(SU,t as TTestSuite, alevel+1)
      else
        begin
        ti:=TJSHTMLElement(document.CreateElement('li'));
        ti.className:='test pending';
        ti.id:=FIDPrefix+'-'+T.TestName;
        TH:=TJSHTMLElement(document.CreateElement('h2'));
        TH.innerText:=T.TestName;
        TI.AppendChild(th);
        SU.appendChild(ti);
        end;
      end;
    Parent.appendChild(SE);
  finally
    PopIDPrefix(aSuite.TestName);
  end;
end;

{ THTMLResultsWriter }

procedure THTMLResultsWriter.SetBaseElementID(AValue: String);
begin
  if FBaseElementID=AValue then Exit;
  FBaseElementID:=AValue;
  FIDPrefix:=FBaseElementID;
end;

procedure THTMLResultsWriter.PopIDPrefix(aPrefix: String);

Var
  L : integer;
begin
  L:=Pos(aPrefix,FIDPrefix);
  if (L+Length(APrefix)-1)=Length(FIDPrefix) then
    FIDPrefix:=Copy(FIDPrefix,1,L-2);
end;

procedure THTMLResultsWriter.PushIDPrefix(aPrefix: String);

begin
  if aPrefix='' then
    exit;
  if (FIDPrefix<>'') then
    FIDPrefix:=FIDPrefix+'-';
  FIDPrefix:=FIDPrefix+aPrefix;
end;

procedure THTMLResultsWriter.SetSkipAddressInfo(AValue: Boolean);
begin
  inherited SetSkipAddressInfo(AValue);
end;

procedure THTMLResultsWriter.SetSparse(AValue: Boolean);
begin
  inherited SetSparse(AValue);
end;

procedure THTMLResultsWriter.WriteTestHeader(ATest: TTest; ALevel: integer; ACount: integer);
begin
  inherited WriteTestHeader(ATest, ALevel, ACount);
end;

procedure THTMLResultsWriter.WriteTestFooter(ATest: TTest; ALevel: integer; ATiming: TDateTime);

Var
  E : TJSHTMLElement;
  S : String;
  p : integer;

begin
  inherited WriteTestFooter(ATest, ALevel, ATiming);
//  console.info('Attempting to find test '+FIDPrefix+'-'+ATest.Testname);
  E:=TJSHTMLElement(Document.getElementById(FIDPrefix+'-'+ATest.Testname));
  if not Assigned(E) then
    console.Error('Failed to find test '+FIDPrefix+'-'+ATest.Testname)
  else
    begin
    S:=E.Classname;
    P:=Pos(' pending',s);
    if (P>0) then
      system.delete(S,P,8);
    if (Pos(' fail',S)=0) and (Pos(' error',S)=0) then
      S:=S+' pass';
    E.ClassName:=S;
    end;
end;

procedure THTMLResultsWriter.WriteSuiteHeader(ATestSuite: TTestSuite; ALevel: integer);
begin
  inherited WriteSuiteHeader(ATestSuite, ALevel);
  PushIDPrefix(ATestSuite.TestName);
end;

procedure THTMLResultsWriter.AppendStats(AParent : TJSElement;ATiming : TDateTime; ANumRuns, ANumErrors, ANumFailures, ANumIgnores : Integer);

  Procedure addc(s,i : string);
  var
    LEL : TJSHTMLELement;

  begin
    LEL:=TJSHTMLELement(document.CreateElement('li'));
    LEL.className:=S;
    aParent.AppendChild(LEL);
    LEL.InnerHTML:=i;
  end;

begin
  AParent.innerHTML:='';
  Addc('progress','Count: <em>'+IntToStr(ANumRuns)+'</em>');
  Addc('passes','Pass: <em>'+IntToStr(ANumRuns-ANumFailures-ANumErrors-ANumIgnores)+'</em>');
  Addc('failures','Fail: <em>'+IntToStr(ANumFailures)+'</em>');
  Addc('errors','Error: <em>'+IntToStr(ANumErrors)+'</em>');
  Addc('ignores','Ignore: <em>'+IntToStr(ANumIgnores)+'</em>');
  Addc('duration','duration: <em>'+TimeToStr(ATiming)+'</em> ms');
end;

procedure THTMLResultsWriter.WriteSuiteFooter(ATestSuite: TTestSuite; ALevel: integer; ATiming: TDateTime; ANumRuns: integer;
  ANumErrors: integer; ANumFailures: integer; ANumIgnores: integer);

Var
  E : TJSHTMLElement;

begin
  inherited WriteSuiteFooter(ATestSuite, ALevel, ATiming, ANumRuns, ANumErrors, ANumFailures, ANumIgnores);

//  console.info('Level',aLevel,'Attempting to find suite '+FIDPrefix);
  if (ALevel=0) or (FIDPrefix='') then
    begin
    E:=TJSHTMLElement(Document.getElementById(BaseElementID+'-stats'));
    if Not Assigned(E) then
      begin
      console.error('Failed to find '+FIDPrefix);
      exit;
      end;
    AppendStats(E,ATiming, ANumRuns, ANumErrors, ANumFailures, ANumIgnores);
    end
  else
    begin
    E:=TJSHTMLElement(Document.getElementById(FIDPrefix));
    if Not Assigned(E) then
      begin
      console.error('Failed to find '+FIDPrefix);
      exit;
      end;
    if ANumFailures+ANumErrors>0 then
      begin
      E.className:=E.className+' fail';
      end
    else
      begin
      E.className:=E.className+' pass';
      end;
    end;
  PopIDPrefix(ATestSuite.TestName);
end;

constructor THTMLResultsWriter.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  BaseElementID:='fpcunit';
end;

destructor THTMLResultsWriter.Destroy;
begin
  inherited Destroy;
end;

procedure THTMLResultsWriter.WriteHeader;
begin
  inherited WriteHeader;
end;

procedure THTMLResultsWriter.WriteResult(aResult: TTestResult);
begin
  inherited WriteResult(aResult);
  console.info('Test result: ', aResult);
end;

procedure THTMLResultsWriter.AddFailure(ATest: TTest; AFailure: TTestFailure);

Var
  E,Err : TJSHTMLElement;

begin
  inherited addFailure(aTest,aFailure);
  if ATest is TTestSuite then exit;
//  console.info('Attempting to find test (for failure) '+FIDPrefix+'-'+aTest.TestName);
  E:=TJSHTMLElement(Document.getElementById(FIDPrefix+'-'+aTest.TestName));
  if Not Assigned(E) then
    console.error('Failed to find fail test'+FIDPrefix+'-'+aTest.TestName)
  else
    begin
    if AFailure.IsIgnoredTest then
      E.className:=E.className+' ignore'
    else
      E.className:=E.className+' fail';
    err:=TJSHTMLElement(document.createElement('pre'));
    Err.InnerText:=AFailure.ExceptionClass.ClassName+' : '+AFailure.ExceptionMessage;
    e.AppendChild(err);
    end;
end;

procedure THTMLResultsWriter.AddError(ATest: TTest; AError: TTestFailure);
Var
  E,Err : TJSHTMLElement;

begin
  inherited AddError(ATest, AError);
  if ATest is TTestSuite then exit;
//    console.info('Attempting to find test (for error) '+FIDPrefix+'-'+aTest.TestName);
  E:=TJSHTMLElement(Document.getElementById(FIDPrefix+'-'+aTest.TestName));
  if Not Assigned(E) then
    console.error('Failed to find error test '+FIDPrefix+'-'+aTest.TestName)
  else
    begin
    E.className:=E.className+' error';
    err:=TJSHTMLElement(document.createElement('pre'));
    Err.InnerText:=AError.ExceptionClass.ClassName+' : '+AError.ExceptionMessage;
    e.AppendChild(err);
    end;
end;

end.

