unit tcgenericdictionary;

{$mode objfpc}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, Generics.Defaults, Generics.Collections;

Type
  TMySimpleDict = Class(Specialize TDictionary<Integer,String>);
{$IFDEF FPC}
  EDictionary = EListError;
  TMyPair = specialize TPair<Integer,String>;
{$ENDIF}
  { TTestSimpleDictionary }

  TTestSimpleDictionary = Class(TTestCase)
  Private
    FDict : TMySimpleDict;
    FnotifyMessage : String;
    FCurrentKeyNotify : Integer;
    FCurrentValueNotify : Integer;
    FExpectKeys : Array of Integer;
    FExpectValues : Array of String;
    FExpectValueAction,
    FExpectKeyAction: Array of TCollectionNotification;
    procedure DoAdd(aCount: Integer; aOffset: Integer=0);
    procedure DoAdd2;
    Procedure DoneExpectKeys;
    Procedure DoneExpectValues;
    procedure DoGetValue(aKey: Integer; Match: String; ExceptionClass: TClass=nil);
    procedure DoKeyNotify(ASender: TObject; {$ifdef fpc}constref{$else}const{$endif} AItem: Integer; AAction: TCollectionNotification);
    procedure DoValueNotify(ASender: TObject; {$ifdef fpc}constref{$else}const{$endif} AItem: String; AAction: TCollectionNotification);
  Public
    Procedure SetExpectKeys(aMessage : string; AKeys : Array of Integer; AActions : Array of TCollectionNotification; DoReverse : Boolean = False);
    Procedure SetExpectValues(aMessage : string; AKeys : Array of String; AActions : Array of TCollectionNotification; DoReverse : Boolean = False);
    Procedure SetUp; override;
    Procedure TearDown; override;
    Property Dict : TMySimpleDict Read FDict;
  Published
    Procedure TestEmpty;
    Procedure TestAdd;
    Procedure TestClear;
    Procedure TestTryGetValue;
    Procedure TestGetValue;
    Procedure TestSetValue;
    Procedure TestAddDuplicate;
    Procedure TestAddOrSet;
    Procedure TestContainsKey;
    Procedure TestContainsValue;
    Procedure TestDelete;
    Procedure TestToArray;
    procedure TestKeys;
    Procedure TestValues;
    Procedure TestEnumerator;
    Procedure TestNotification;
    procedure TestNotificationDelete;
    procedure TestValueNotification;
    procedure TestValueNotificationDelete;
    procedure TestKeyValueNotificationSet;
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

  TSingleObjectDict = Class(Specialize TObjectDictionary<Integer,TMyObject>);
  TDualObjectDict = Class(Specialize TObjectDictionary<TMyObject,TMyObject>);

  { TTestSingleObjectDict }

  TTestSingleObjectDict = Class(TTestCase)
  private
    FDict: TSingleObjectDict;
    FList : TFPList;
    procedure DoAdd(aID: Integer);
    procedure DoDestroy(Sender: TObject);
  Public
    Procedure SetUp; override;
    Procedure TearDown; override;
    Property Dict : TSingleObjectDict Read FDict;
  Published
    Procedure TestEmpty;
    Procedure TestFreeOnRemove;
    Procedure TestNoFreeOnRemove;
  end;

  TTestDualObjectDict = Class(TTestCase)
  private
    FDict: TDualObjectDict;
    FList : TFPList;
    procedure DoAdd(aID: Integer);
    procedure DoDestroy(Sender: TObject);
  Public
    Procedure SetUp; override;
    Procedure TearDown; override;
    Property Dict : TDualObjectDict Read FDict;
  Published
    Procedure TestEmpty;
    Procedure TestFreeOnRemove;
    Procedure TestNoFreeOnRemove;
  end;

  TMyStringDict = Class(Specialize TDictionary<string,string>);
  TMyStringComparer = Specialize TComparer<string>;

  { TTestComparerDictionary }

  TTestComparerDictionary = Class(TTestCase)
  private
    FDict: TMyStringDict;
  public
    Procedure Setup; override;
    Procedure TearDown; override;
    Procedure FillDict;
    Property Dict : TMyStringDict Read FDict;
  Published
    Procedure TestHasKey;
    Procedure TestTryGetValue;
    Procedure TestAddOrSet;
    Procedure TestRemove;
  end;

implementation

{ TTestComparerDictionary }

procedure TTestComparerDictionary.Setup;
begin
  inherited Setup;
  FDict:=TMyStringDict.Create(TMyStringComparer.Construct(function (Const a,b : String) : integer
    begin
      Result:=CompareText(a,b);
      // writeln('Comparing ',a,' and ',b,' result: ',Result);
    end
    ));
  FillDict;
end;

procedure TTestComparerDictionary.TearDown;
begin
  FreeAndNil(FDict);
  inherited TearDown;
end;

procedure TTestComparerDictionary.FillDict;
begin
  With Dict do
    begin
    add('a','A');
    add('B','b');
    add('c','C');
    end;
end;

procedure TTestComparerDictionary.TestHasKey;
begin
  AssertTrue('ContainsKey A',Dict.ContainsKey('A'));
  AssertTrue('ContainsKey b',Dict.ContainsKey('b'));
  AssertTrue('ContainsKey c',Dict.ContainsKey('c'));
  AssertFalse('ContainsKey D',Dict.ContainsKey('D'));
end;

procedure TTestComparerDictionary.TestTryGetValue;

Var
  S : String;

begin
  AssertTrue('A',Dict.TryGetValue('A',S));
  AssertEquals('Value A','A',S);
  AssertTrue('b',Dict.TryGetValue('b',S));
  AssertEquals('Value b','b',S);
  AssertTrue('c',Dict.TryGetValue('c',S));
  AssertEquals('Value C','C',S);
  AssertFalse('d',Dict.TryGetValue('D',S));
end;

procedure TTestComparerDictionary.TestAddOrSet;

Var
  S : String;

begin
  Dict.AddOrSetValue('d','E');
  AssertTrue('d',Dict.TryGetValue('d',S));
  AssertEquals('Value d','E',S);
  Dict.AddOrSetValue('D','D');
  AssertTrue('D',Dict.TryGetValue('D',S));
  AssertEquals('Value D','D',S);
end;

procedure TTestComparerDictionary.TestRemove;
begin
  Dict.Remove('C');
  AssertFalse('ContainsKey C',Dict.ContainsKey('C'));
  AssertFalse('ContainsKey c',Dict.ContainsKey('c'));
end;

{ TTestSingleObjectDict }

procedure TTestSingleObjectDict.SetUp;
begin
  FDict:=TSingleObjectDict.Create([doOwnsValues]);
  FList:=TFPList.Create;
  inherited SetUp;
end;

procedure TTestSingleObjectDict.TearDown;

Var
  I : integer;
  A : TObject;

begin
  FreeAndNil(FDict);
  for I:=0 to FList.Count-1 do
    begin
    A:=TObject(FList[i]);
    A.Free;
    end;
  FreeAndNil(FList);
  inherited TearDown;
end;

procedure TTestSingleObjectDict.TestEmpty;
begin
  AssertNotNull('Have object',Dict);
  AssertEquals('Have empty object',0,Dict.Count);
end;

procedure TTestSingleObjectDict.DoAdd(aID : Integer);

Var
  O :  TMyObject;

begin
  O:=TMyObject.Create(aID,@DoDestroy);
  FList.Add(O);
  FDict.Add(aID,O);
end;

procedure TTestSingleObjectDict.DoDestroy(Sender: TObject);

Var
  I : Integer;

begin
  I:=FList.IndexOf(Sender);
  AssertTrue('Have object in list',I<>-1);
  FList.Delete(I);
end;

procedure TTestSingleObjectDict.TestFreeOnRemove;

begin
  DoAdd(1);
  AssertEquals('Have obj',1,FList.Count);
  Dict.Remove(1);
  AssertEquals('Have no obj',0,FList.Count);
end;

procedure TTestSingleObjectDict.TestNoFreeOnRemove;
begin
  Dict.OwnerShips:=[];
  DoAdd(1);
  AssertEquals('Have obj',1,FList.Count);
  Dict.Remove(1);
  AssertEquals('Have  obj',1,FList.Count);
end;

{ TTestDualObjectDict }

procedure TTestDualObjectDict.SetUp;
begin
  FDict:=TDualObjectDict.Create([doOwnsKeys,doOwnsValues]);
  FList:=TFPList.Create;
  inherited SetUp;
end;

procedure TTestDualObjectDict.TearDown;
Var
  I : integer;
  A : TObject;

begin
  FreeAndNil(FDict);
  for I:=0 to FList.Count-1 do
    begin
    A:=TObject(FList[i]);
    A.Free;
    end;
  FreeAndNil(FList);
  inherited TearDown;
end;

procedure TTestDualObjectDict.TestEmpty;
begin
  AssertNotNull('Have object',Dict);
  AssertEquals('Have empty object',0,Dict.Count);
end;

procedure TTestDualObjectDict.DoAdd(aID : Integer);

Var
  O1,O10 :  TMyObject;

begin
  O1:=TMyObject.Create(aID,@DoDestroy);
  FList.Add(O1);
  O10:=TMyObject.Create(aID*10,@DoDestroy);
  FList.Add(O10);
  FDict.Add(O1,O10);
end;

procedure TTestDualObjectDict.DoDestroy(Sender: TObject);

Var
  I : Integer;

begin
  I:=FList.IndexOf(Sender);
  AssertTrue('Have object in list',I<>-1);
  FList.Delete(I);
end;

procedure TTestDualObjectDict.TestFreeOnRemove;

begin
  DoAdd(1);
  AssertEquals('Have obj',2,FList.Count);
  Dict.Remove(TMyObject(FList[0]));
  AssertEquals('Have no obj',0,FList.Count);
end;

procedure TTestDualObjectDict.TestNoFreeOnRemove;
begin
  Dict.OwnerShips:=[doOwnsValues];
  DoAdd(1);
  AssertEquals('Have obj',2,FList.Count);
  Dict.Remove(TMyObject(FList[0]));
  AssertEquals('Have  obj',1,FList.Count);
  AssertEquals('Have key',1,TMyObject(Flist[0]).ID);
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

{ TTestSimpleDictionary }

procedure TTestSimpleDictionary.SetUp;
begin
  inherited SetUp;
  FDict:=TMySimpleDict.Create;
  FCurrentKeyNotify:=0;
  FCurrentValueNotify:=0;
  FExpectKeys:=[];
  FExpectKeyAction:=[];
  FExpectValues:=[];
  FExpectValueAction:=[];
end;

procedure TTestSimpleDictionary.TearDown;
begin
  // So we don't get clear messages
  FDict.OnKeyNotify:=Nil;
  FDict.OnValueNotify:=Nil;
  FreeAndNil(FDict);
  inherited TearDown;
end;

procedure TTestSimpleDictionary.TestEmpty;
begin
  AssertNotNull('Have dictionary',Dict);
  AssertEquals('empty dictionary',0,Dict.Count);
end;

procedure TTestSimpleDictionary.DoAdd(aCount : Integer; aOffset : Integer=0);

Var
  I : Integer;

begin
  if aOffset=-1 then
    aOffset:=Dict.Count;
  For I:=aOffset+1 to aOffset+aCount do
    Dict.Add(I,IntToStr(i));
end;

procedure TTestSimpleDictionary.TestAdd;

begin
  DoAdd(1);
  AssertEquals('Count OK',1,Dict.Count);
  AssertTrue('Has added value',Dict.ContainsKey(1));
  DoAdd(1,1);
  AssertEquals('Count OK',2,Dict.Count);
  AssertTrue('Has added value',Dict.ContainsKey(2));
end;

procedure TTestSimpleDictionary.TestClear;
begin
  DoAdd(3);
  AssertEquals('Count OK',3,Dict.Count);
  Dict.Clear;
  AssertEquals('Count after clear OK',0,Dict.Count);
end;

procedure TTestSimpleDictionary.TestTryGetValue;

Var
  I : integer;
  SI,A : string;

begin
  DoAdd(3);
  For I:=1 to 3 do
    begin
    SI:=IntToStr(I);
    AssertTrue('Have value '+SI,Dict.TryGetValue(I,A));
    AssertEquals('Value is correct '+SI,SI,A);
    end;
  AssertFalse('Have no value 4',Dict.TryGetValue(4,A));
end;

procedure TTestSimpleDictionary.DoGetValue(aKey: Integer; Match: String; ExceptionClass: TClass);

Var
  EC : TClass;
  A,EM : String;

begin
  EC:=Nil;
  try
    A:=Dict.Items[aKey];
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

procedure TTestSimpleDictionary.DoKeyNotify(ASender: TObject;  {$ifdef fpc}constref{$else}const{$endif}  AItem: Integer; AAction: TCollectionNotification);
begin
  // Writeln(FnotifyMessage+' Notification',FCurrentKeyNotify);
  AssertSame(FnotifyMessage+' Correct sender', FDict,aSender);
  if (FCurrentKeyNotify>=Length(FExpectKeys)) then
    Fail(FnotifyMessage+' Too many notificiations');
  AssertEquals(FnotifyMessage+' Notification Key no '+IntToStr(FCurrentKeyNotify),FExpectKeys[FCurrentKeyNotify],aItem);
  Inc(FCurrentKeyNotify);
end;

procedure TTestSimpleDictionary.DoValueNotify(ASender: TObject; {$ifdef fpc}constref{$else}const{$endif} AItem: String; AAction: TCollectionNotification);
begin
  // Writeln(FnotifyMessage+' value Notification',FCurrentValueNotify);
  AssertSame(FnotifyMessage+' value Correct sender', FDict,aSender);
  if (FCurrentValueNotify>=Length(FExpectValues)) then
    Fail(FnotifyMessage+' Too many value notificiations');
  AssertEquals(FnotifyMessage+' Notification value no '+IntToStr(FCurrentValueNotify),FExpectValues[FCurrentValueNotify],aItem);
  Inc(FCurrentValueNotify);
end;

procedure TTestSimpleDictionary.SetExpectKeys(aMessage: string; AKeys: array of Integer;
  AActions: array of TCollectionNotification; DoReverse: Boolean = False);

Var
  I,L : integer;

begin
  FnotifyMessage:=aMessage;
  FCurrentKeyNotify:=0;
  L:=Length(aKeys);
  AssertEquals('SetExpectkeys: Lengths arrays equal',l,Length(aActions));
  SetLength(FExpectKeys,L);
  SetLength(FExpectKeyAction,L);
  Dec(L);
  if DoReverse then
    For I:=0 to L do
      begin
      FExpectKeys[L-i]:=AKeys[i];
      FExpectKeyAction[L-i]:=AActions[I];
      end
  else
    For I:=0 to L do
      begin
      FExpectKeys[i]:=AKeys[i];
      FExpectKeyAction[i]:=AActions[I];
      end;
end;

procedure TTestSimpleDictionary.SetExpectValues(aMessage: string; AKeys: array of String;
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

procedure TTestSimpleDictionary.TestGetValue;

Var
  I : integer;

begin
  DoAdd(3);
  For I:=1 to 3 do
    DoGetValue(I,IntToStr(I));
  DoGetValue(4,'4',EDictionary);
end;

procedure TTestSimpleDictionary.TestSetValue;
begin
  TestGetValue;
  Dict.Items[3]:='Six';
  DoGetValue(3,'Six');
end;

procedure TTestSimpleDictionary.DoAdd2;

begin
  Dict.Add(2,'A new 2');
end;

procedure TTestSimpleDictionary.DoneExpectKeys;
begin
  AssertEquals(FnotifyMessage+' Expected number of keys seen',Length(FExpectKeys),FCurrentKeyNotify);
end;

procedure TTestSimpleDictionary.DoneExpectValues;
begin
  AssertEquals(FnotifyMessage+' Expected number of values seen',Length(FExpectValues),FCurrentValueNotify);
end;

procedure TTestSimpleDictionary.TestAddDuplicate;
begin
  DoAdd(3);
  AssertException('Cannot add duplicate',EDictionary,@DoAdd2);
end;

procedure TTestSimpleDictionary.TestAddOrSet;

begin
  DoAdd(3);
  Dict.AddOrSetValue(2,'a new 2');
  DoGetValue(2,'a new 2');
end;

procedure TTestSimpleDictionary.TestContainsKey;

Var
  I : Integer;

begin
  DoAdd(3);
  For I:=1 to 3 do
    AssertTrue('Has '+IntToStr(i),Dict.ContainsKey(I));
  AssertFalse('Has not 4',Dict.ContainsKey(4));
end;

procedure TTestSimpleDictionary.TestContainsValue;

Var
  I : Integer;

begin
  DoAdd(3);
  For I:=1 to 3 do
    AssertTrue('Has '+IntToStr(i),Dict.ContainsValue(IntToStr(i)));
  AssertFalse('Has not 4',Dict.ContainsValue('4'));
end;

procedure TTestSimpleDictionary.TestDelete;

begin
  DoAdd(3);
  Dict.Remove(2);
  AssertEquals('Count',2,Dict.Count);
  AssertFalse('Has not 2',Dict.ContainsKey(2));
end;

procedure TTestSimpleDictionary.TestToArray;

Var
{$ifdef fpc}
  A : specialize TArray<TMyPair>;
{$else}
  A : specialize TArray<TMySimpleDict.TMyPair>;
{$endif}
  I : Integer;
  SI : String;

begin
  DoAdd(3);
  A:=Dict.ToArray;
  AssertEquals('Length Ok',3,Length(A));
  For I:=1 to 3 do
    begin
    SI:=IntToStr(I);
    AssertEquals('key '+SI,I,A[i-1].Key);
    AssertEquals('Value '+SI,SI,A[i-1].Value);
    end;
end;

procedure TTestSimpleDictionary.TestKeys;

Var
  A : Array of Integer;
  I : Integer;
  SI : String;

begin
  DoAdd(3);
  A:=Dict.Keys.ToArray;
  AssertEquals('Length Ok',3,Length(A));
  For I:=1 to 3 do
    begin
    SI:=IntToStr(I);
    AssertEquals('key '+SI,I,A[i-1]);
    end;
end;

procedure TTestSimpleDictionary.TestValues;
Var
  A : Array of String;
  I : Integer;
  SI : String;

begin
  DoAdd(3);
  A:=Dict.Values.ToArray;
  AssertEquals('Length Ok',3,Length(A));
  For I:=1 to 3 do
    begin
    SI:=IntToStr(I);
    AssertEquals('Value '+SI,SI,A[i-1]);
    end;
end;

procedure TTestSimpleDictionary.TestEnumerator;

Var
{$ifdef fpc}
  A : TMyPair;
{$else}
  A : TMySimpleDict.TMyPair;
{$endif}
  I : Integer;
  SI : String;

begin
  DoAdd(3);
  I:=1;
  For A in Dict do
    begin
    SI:=IntToStr(I);
    AssertEquals('key '+SI,I,A.Key);
    AssertEquals('Value '+SI,SI,A.Value);
    Inc(I);
    end;
end;

procedure TTestSimpleDictionary.TestNotification;
begin
  Dict.OnKeyNotify:=@DoKeyNotify;
  SetExpectKeys('Add',[1,2,3],[cnAdded,cnAdded,cnAdded]);
  DoAdd(3);
  DoneExpectKeys;
end;

procedure TTestSimpleDictionary.TestNotificationDelete;

begin
  DoAdd(3);
  Dict.OnKeyNotify:=@DoKeyNotify;
  SetExpectKeys('Clear',[1,2,3],[cnRemoved,cnRemoved,cnRemoved],{$IFDEF FPC}true{$ELSE}False{$endif});
  Dict.Clear;
  DoneExpectKeys;
end;

procedure TTestSimpleDictionary.TestValueNotification;
begin
  Dict.OnValueNotify:=@DoValueNotify;
  SetExpectValues('Add',['1','2','3'],[cnAdded,cnAdded,cnAdded]);
  DoAdd(3);
  DoneExpectValues;
end;

procedure TTestSimpleDictionary.TestValueNotificationDelete;
begin
  DoAdd(3);
  Dict.OnValueNotify:=@DoValueNotify;
  SetExpectValues('Clear',['1','2','3'],[cnRemoved,cnRemoved,cnRemoved],{$IFDEF FPC}true{$ELSE}False{$endif});
  Dict.Clear;
  DoneExpectValues;
end;

procedure TTestSimpleDictionary.TestKeyValueNotificationSet;
begin
  DoAdd(3);
  Dict.OnValueNotify:=@DoValueNotify;
  Dict.OnKeyNotify:=@DoKeyNotify;
  SetExpectValues('Set',['2','Six'],[cnRemoved,cnAdded]);
  SetExpectKeys('Set',[],[]);
  Dict[2]:='Six';
  DoneExpectKeys;
  DoneExpectValues;
end;

begin
  RegisterTests([{TTestSimpleDictionary,
                 TTestSingleObjectDict,
                 TTestDualObjectDict,}
                 TTestComparerDictionary]);
end.

