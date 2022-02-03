{ Demo showing how to create a Pascal class descendant from a JS function.
}
program ExtendJSFunctionClass1;

{$mode objfpc}
{$ModeSwitch externalclass}

uses
  JS, browserconsole;

type

  { TJSPerson - a JS function, which is used like a class
    Person is declared in the html }

  TJSPerson = class external name 'Person'(TJSFunction)
  public
    First: string; external name 'first';
    Last: string; external name 'last';
    Age: nativeint; external name 'age';
    constructor New(aFirst, aLast: string; aAge: nativeint);
    procedure Run(Value: string); external name 'run';
  end;

  { TContact - descend a Pascal class from the JS function }

  TContact = class(TJSPerson)
  public
    EMail: string;
    constructor Create(aFirst, aLast: string; aAge: nativeint; aEMail: string); reintroduce;
  end;

constructor TContact.Create(aFirst, aLast: string; aAge: nativeint;
  aEMail: string);
begin
  inherited New(aFirst,aLast,aAge);
  EMail:=aEMail;
end;

procedure TestContact;
var
  aPerson: TJSPerson;
  aContact: TContact;
begin
  aPerson:=TJSPerson.new('Joe','Smith',23);
  writeln('TestContact aPerson.First=',aPerson.First,' Last=',aPerson.Last,' Age=',aPerson.Age);
  // the JS instanceof operator works on the JS object (should be true)
  writeln('TestContact: aPerson instanceof Person = ',jsInstanceOf(aPerson,TJSPerson));
  aPerson.Run('TestContact calling aPerson.run');

  aContact:=TContact.Create('Foo','Bar',7,'a@b');
  writeln('TestContact: aContact.First=',aContact.First);
  writeln('TestContact: aContact.Last=',aContact.Last);
  writeln('TestContact: aContact.Age=',aContact.Age);
  writeln('TestContact: aContact.EMail=',aContact.EMail);
  // the JS instanceof operator works on the Pascal object (should be true)
  writeln('TestContact: aContact instanceof Person = ',jsInstanceOf(aPerson,TJSPerson));

  aContact.Run('TestContact called aContact.run');
end;

begin
  TestContact;
end.
