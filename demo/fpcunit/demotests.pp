unit demotests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

Type

  { TMyTestCase }

  TMyTestCase = Class(TTestCase)
  Published
    Procedure TestWillFail;
    Procedure TestMustFail;
    Procedure TestWillError;
    Procedure TestWillWork;
    Procedure TestWillWorkToo;
    Procedure TestWillDefinitelyWork;
    Procedure TestWeLLIgnoreThisOne;
  end;

implementation

{ TMyTestCase }

procedure TMyTestCase.TestWillFail;
begin
  Fail('Aarrggghhhhh this test failed');
end;

procedure TMyTestCase.TestMustFail;
begin
  Fail('Uh-oh, this test failed too...');
end;

procedure TMyTestCase.TestWillError;
begin
  Raise Exception.Create('A random error');
end;

procedure TMyTestCase.TestWillWork;
begin
end;

procedure TMyTestCase.TestWillWorkToo;
begin

end;

procedure TMyTestCase.TestWillDefinitelyWork;
begin

end;

procedure TMyTestCase.TestWeLLIgnoreThisOne;
begin
  Ignore('Not today, thank you!')
end;

initialization
  RegisterTest('DemoSuite',TMyTestCase);
end.

