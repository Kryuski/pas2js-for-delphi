unit tcgenericlist;

{$mode objfpc}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, Generics.Defaults, Generics.Collections;


Type
  TMySimpleList = Class(Specialize TList<String>);
{$IFDEF FPC}
  EList = EListError;
{$ENDIF}

  { TTestSimpleList }

  TTestSimpleList = Class(TTestCase)
  Private
    FList : TMySimpleList;
    FnotifyMessage : String;
    FCurrentValueNotify : Integer;
    FExpectValues : Array of String;
    FExpectValueAction: Array of TCollectionNotification;
    procedure DoAdd(aCount: Integer; aOffset: Integer=0);
    procedure DoAdd2;
    Procedure DoneExpectValues;
    procedure DoGetValue(aKey: Integer; Match: String; ExceptionClass: TClass=nil);
    procedure DoValueNotify(ASender: TObject; {$ifdef fpc}constref{$else}const{$endif} AItem: String; AAction: TCollectionNotification);
  Public
    Procedure SetExpectValues(aMessage : string; AKeys : Array of String; AActions : Array of TCollectionNotification; DoReverse : Boolean = False);
    Procedure SetUp; override;
    Procedure TearDown; override;
    Property List : TMySimpleList Read FList;
  Published
    Procedure TestEmpty;
    Procedure TestAdd;
    Procedure TestClear;
    Procedure TestGetValue;
    Procedure TestSetValue;
    Procedure TestContainsValue;
    Procedure TestDelete;
    Procedure TestToArray;
    Procedure TestEnumerator;
    procedure TestValueNotification;
    procedure TestValueNotificationDelete;
    procedure TestValueNotificationSet;
  end;

  { TMyObject }

  TMyObject = Class(TObject)
  Private
    fOnDestroy : TNotifyEvent;
    FID : Integer;
  public
    Constructor Create(aID : Integer; aOnDestroy : TNotifyEvent);
    destructor destroy; override;
    Property ID : Integer Read FID;
  end;

  TSingleObjectList = Class(Specialize TObjectList<TMyObject>);

  { TTestSingleObjectList }

  TTestSingleObjectList = Class(TTestCase)
  private
    FOList: TSingleObjectList;
    FList : TFPList;
    procedure DoAdd(aID: Integer);
    procedure DoDestroy(Sender: TObject);
  Public
    Procedure SetUp; override;
    Procedure TearDown; override;
    Property List : TSingleObjectList Read FOList;
  Published
    Procedure TestEmpty;
    Procedure TestFreeOnRemove;
    Procedure TestNoFreeOnRemove;
    Procedure TestFreeOnDelete;
    Procedure TestNoFreeDelete;
  end;

implementation

{ TTestSingleObjectList }

procedure TTestSingleObjectList.SetUp;
begin
  FOList:=TSingleObjectList.Create(True);
  FList:=TFPList.Create;
  inherited SetUp;
end;

procedure TTestSingleObjectList.TearDown;

Var
  I : Integer;
  A : TObject;
begin
  for I:=0 to FList.Count-1 do
    begin
    A:=TObject(FList[i]);
    A.Free;
    end;
  FreeAndNil(FList);
  FreeAndNil(FOList);
  inherited TearDown;
end;

procedure TTestSingleObjectList.TestEmpty;
begin
  AssertNotNull('Have object',List);
  AssertEquals('Have empty object',0,List.Count);
end;

procedure TTestSingleObjectList.DoAdd(aID : Integer);

Var
  O :  TMyObject;

begin
  O:=TMyObject.Create(aID,@DoDestroy);
  FOList.Add(O);
  FList.Add(O);
end;

procedure TTestSingleObjectList.DoDestroy(Sender: TObject);

Var
  I : Integer;

begin
  I:=FList.IndexOf(Sender);
  AssertTrue('Have object in list',I<>-1);
  FList.Delete(I);
end;

procedure TTestSingleObjectList.TestFreeOnRemove;

begin
  DoAdd(1);
  AssertEquals('Have obj',1,FList.Count);
  List.Remove(TMyObject(FList[0]));
  AssertEquals('Have no obj',0,FList.Count);
end;

procedure TTestSingleObjectList.TestNoFreeOnRemove;
begin
  List.OwnsObjects:=False;
  DoAdd(1);
  AssertEquals('Have obj',1,FList.Count);
  List.Remove(TMyObject(FList[0]));
  AssertEquals('Have  obj',1,FList.Count);
end;

procedure TTestSingleObjectList.TestFreeOnDelete;
begin
  DoAdd(1);
  AssertEquals('Have obj',1,FList.Count);
  List.Delete(0);
  AssertEquals('Have no obj',0,FList.Count);
end;

procedure TTestSingleObjectList.TestNoFreeDelete;
begin
  List.OwnsObjects:=False;
  DoAdd(1);
  AssertEquals('Have obj',1,FList.Count);
  List.Delete(0);
  AssertEquals('Have  obj',1,FList.Count);
end;


{ TMyObject }

constructor TMyObject.Create(aID: Integer; aOnDestroy: TNotifyEvent);
begin
  FOnDestroy:=aOnDestroy;
  FID:=AID;
end;

destructor TMyObject.destroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
  inherited destroy;
end;

{ TTestSimpleList }

procedure TTestSimpleList.SetUp;
begin
  inherited SetUp;
  FList:=TMySimpleList.Create;
  FCurrentValueNotify:=0;
  FExpectValues:=[];
  FExpectValueAction:=[];
end;

procedure TTestSimpleList.TearDown;
begin
  // So we don't get clear messages
  FList.OnNotify:=Nil;
  FreeAndNil(FList);
  inherited TearDown;
end;

procedure TTestSimpleList.TestEmpty;
begin
  AssertNotNull('Have dictionary',List);
  AssertEquals('empty dictionary',0,List.Count);
end;

procedure TTestSimpleList.DoAdd(aCount : Integer; aOffset : Integer=0);

Var
  I : Integer;

begin
  if aOffset=-1 then
    aOffset:=List.Count;
  For I:=aOffset+1 to aOffset+aCount do
    List.Add(IntToStr(i));
end;

procedure TTestSimpleList.TestAdd;

begin
  DoAdd(1);
  AssertEquals('Count OK',1,List.Count);
  AssertTrue('Has added value',List.Contains('1'));
  DoAdd(1,1);
  AssertEquals('Count OK',2,List.Count);
  AssertTrue('Has added value',List.Contains('2'));
end;

procedure TTestSimpleList.TestClear;
begin
  DoAdd(3);
  AssertEquals('Count OK',3,List.Count);
  List.Clear;
  AssertEquals('Count after clear OK',0,List.Count);
end;

procedure TTestSimpleList.DoGetValue(aKey: Integer; Match: String; ExceptionClass: TClass);

Var
  EC : TClass;
  A,EM : String;

begin
  EC:=Nil;
  try
    A:=List.Items[aKey];
  except
    On E : Exception do
      begin
      EC:=E.ClassType;
      EM:=E.Message;
      end
  end;
  if ExceptionClass=Nil then
    begin
    if EC<>Nil then
      Fail('Got exception '+EC.ClassName+' with message: '+EM);
    AssertEquals('Value is correct for '+IntToStr(aKey),Match,A)
    end
  else
    begin
    if EC=Nil then
      Fail('Expected exception '+ExceptionClass.ClassName+' but got none');
    if EC<>ExceptionClass then
      Fail('Expected exception class '+ExceptionClass.ClassName+' but got '+EC.ClassName+' with message '+EM);
    end;
end;

procedure TTestSimpleList.DoValueNotify(ASender: TObject; {$ifdef fpc}constref{$else}const{$endif} AItem: String; AAction: TCollectionNotification);
begin
//  Writeln(FnotifyMessage+' value Notification',FCurrentValueNotify);
  AssertSame(FnotifyMessage+' value Correct sender', FList,aSender);
  if (FCurrentValueNotify>=Length(FExpectValues)) then
    Fail(FnotifyMessage+' Too many value notificiations');
  AssertEquals(FnotifyMessage+' Notification value no '+IntToStr(FCurrentValueNotify),FExpectValues[FCurrentValueNotify],aItem);
  Inc(FCurrentValueNotify);
end;


procedure TTestSimpleList.SetExpectValues(aMessage: string; AKeys: array of String;
  AActions: array of TCollectionNotification; DoReverse: Boolean);
Var
  I,L : integer;

begin
  FnotifyMessage:=aMessage;
  FCurrentValueNotify:=0;
  L:=Length(aKeys);
  AssertEquals('SetExpectValues: Lengths arrays equal',l,Length(aActions));
  SetLength(FExpectValues,L);
  SetLength(FExpectValueAction,L);
  Dec(L);
  if DoReverse then
    For I:=0 to L do
      begin
      FExpectValues[L-i]:=AKeys[i];
      FExpectValueAction[L-i]:=AActions[I];
      end
  else
    For I:=0 to L do
      begin
      FExpectValues[i]:=AKeys[i];
      FExpectValueAction[i]:=AActions[I];
      end;
end;

procedure TTestSimpleList.TestGetValue;

Var
  I : integer;

begin
  DoAdd(3);
  For I:=1 to 3 do
    DoGetValue(i-1,IntToStr(I));
  DoGetValue(3,'4',EArgumentOutOfRangeException);
end;

procedure TTestSimpleList.TestSetValue;
begin
  TestGetValue;
  List.Items[1]:='Six';
  DoGetValue(1,'Six');
end;

procedure TTestSimpleList.DoAdd2;

begin
  List.Add('A new 2');
end;

procedure TTestSimpleList.DoneExpectValues;
begin
  AssertEquals(FnotifyMessage+' Expected number of values seen',Length(FExpectValues),FCurrentValueNotify);
end;

procedure TTestSimpleList.TestContainsValue;

Var
  I : Integer;

begin
  DoAdd(3);
  For I:=1 to 3 do
    AssertTrue('Has '+IntToStr(i),List.Contains(IntToStr(i)));
  AssertFalse('Has not 4',List.Contains('4'));
end;

procedure TTestSimpleList.TestDelete;

begin
  DoAdd(3);
  List.Remove('2');
  AssertEquals('Count',2,List.Count);
  AssertFalse('Has not 2',List.Contains('2'));
end;

procedure TTestSimpleList.TestToArray;

Var
  A : specialize TArray<String>;

  I : Integer;
  SI : String;

begin
  DoAdd(3);
  A:=List.ToArray;
  AssertEquals('Length Ok',3,Length(A));
  For I:=1 to 3 do
    begin
    SI:=IntToStr(I);
    AssertEquals('Value '+SI,SI,A[i-1]);
    end;
end;


procedure TTestSimpleList.TestEnumerator;

Var
  A : String;
  I : Integer;
  SI : String;

begin
  DoAdd(3);
  I:=1;
  For A in List do
    begin
    SI:=IntToStr(I);
    AssertEquals('Value '+SI,SI,A);
    Inc(I);
    end;
end;

procedure TTestSimpleList.TestValueNotification;
begin
  List.OnNotify:=@DoValueNotify;
  SetExpectValues('Add',['1','2','3'],[cnAdded,cnAdded,cnAdded]);
  DoAdd(3);
  DoneExpectValues;
end;

procedure TTestSimpleList.TestValueNotificationDelete;
begin
  DoAdd(3);
  List.OnNotify:=@DoValueNotify;
  SetExpectValues('Clear',['1','2','3'],[cnRemoved,cnRemoved,cnRemoved],{$IFDEF FPC}true{$ELSE}False{$endif});
  List.Clear;
  DoneExpectValues;
end;

procedure TTestSimpleList.TestValueNotificationSet;
begin
  DoAdd(3);
  List.OnNotify:=@DoValueNotify;
  SetExpectValues('Set',['2','Six'],[cnRemoved,cnAdded]);
  List[1]:='Six';
  DoneExpectValues;
end;

begin
  RegisterTests([ TTestSimpleList,TTestSingleObjectList]);
end.

