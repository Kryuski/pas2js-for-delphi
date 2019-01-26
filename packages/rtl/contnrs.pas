{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2002 by Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}

unit contnrs;

interface

uses
  SysUtils, Classes;


Type
  TObjectListCallback = Reference to Procedure(data:TObject;arg:JSValue);

  TFPObjectList = class(TObject)
  private
    FFreeObjects : Boolean;
    FList: TFPList;
    Function GetCount: integer;
    Procedure SetCount(const AValue: integer);
  protected
    Function GetItem(Index: Integer): TObject;
    Procedure SetItem(Index: Integer; AObject: TObject);
    Procedure SetCapacity(NewCapacity: Integer);
    Function GetCapacity: integer;
  public
    constructor Create; reintroduce;
    constructor Create(FreeObjects : Boolean);
    destructor Destroy; override;
    Procedure Clear;
    Function Add(AObject: TObject): Integer;
    Procedure Delete(Index: Integer);
    Procedure Exchange(Index1, Index2: Integer);
    Function Expand: TFPObjectList;
    Function Extract(Item: TObject): TObject;
    Function Remove(AObject: TObject): Integer;
    Function IndexOf(AObject: TObject): Integer;
    Function FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt: Integer): Integer;
    Procedure Insert(Index: Integer; AObject: TObject);
    Function First: TObject;
    Function Last: TObject;
    Procedure Move(CurIndex, NewIndex: Integer);
    Procedure Assign(Obj:TFPObjectList);
    Procedure Pack;
    Procedure Sort(Compare: TListSortCompare);
    Procedure ForEachCall(proc2call:TObjectListCallback;arg:JSValue);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property OwnsObjects: Boolean read FFreeObjects write FFreeObjects;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
    property List: TFPList read FList;
  end;


  { TObjectList }

  TObjectList = class(TList)
  private
    FFreeObjects : Boolean;
  Protected
    Procedure Notify(Ptr: JSValue; Action: TListNotification); override;
    Function GetItem(Index: Integer): TObject;
    Procedure SetItem(Index: Integer; AObject: TObject);
  public
    constructor Create; reintroduce;
    constructor Create(FreeObjects : boolean);
    Function Add(AObject: TObject): Integer; reintroduce;
    Function Extract(Item: TObject): TObject; reintroduce;
    Function Remove(AObject: TObject): Integer; reintroduce;
    Function IndexOf(AObject: TObject): Integer; reintroduce;
    Function FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt: Integer): Integer;
    Procedure Insert(Index: Integer; AObject: TObject); reintroduce;
    Function First: TObject; reintroduce;
    Function Last: TObject; reintroduce;
    property OwnsObjects: Boolean read FFreeObjects write FFreeObjects;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
  end;

  TComponentList = class(TObjectList)
  Private
    FNotifier : TComponent;
  Protected
    Procedure Notify(Ptr: JSValue; Action: TListNotification); override;
    Function GetItems(Index: Integer): TComponent;
    Procedure SetItems(Index: Integer; AComponent: TComponent);
    Procedure HandleFreeNotify(Sender: TObject; AComponent: TComponent);
  public
    destructor Destroy; override;
    Function Add(AComponent: TComponent): Integer; reintroduce;
    Function Extract(Item: TComponent): TComponent; reintroduce;
    Function Remove(AComponent: TComponent): Integer; reintroduce;
    Function IndexOf(AComponent: TComponent): Integer; reintroduce;
    Function First: TComponent; reintroduce;
    Function Last: TComponent; reintroduce;
    Procedure Insert(Index: Integer; AComponent: TComponent); reintroduce;
    property Items[Index: Integer]: TComponent read GetItems write SetItems; default;
  end;

  TClassList = class(TList)
  protected
    Function GetItems(Index: Integer): TClass;
    Procedure SetItems(Index: Integer; AClass: TClass);
  public
    Function Add(AClass: TClass): Integer; reintroduce;
    Function Extract(Item: TClass): TClass; reintroduce;
    Function Remove(AClass: TClass): Integer; reintroduce;
    Function IndexOf(AClass: TClass): Integer; reintroduce;
    Function First: TClass; reintroduce;
    Function Last: TClass; reintroduce;
    Procedure Insert(Index: Integer; AClass: TClass); reintroduce;
    property Items[Index: Integer]: TClass read GetItems write SetItems; default;
  end;

  TOrderedList = class(TObject)
  private
    FList: TList;
  protected
    Procedure PushItem(AItem: JSValue); virtual; abstract;
    Function PopItem: JSValue; virtual;
    Function PeekItem: JSValue; virtual;
    property List: TList read FList;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    Function Count: Integer;
    Function AtLeast(ACount: Integer): Boolean;
    Function Push(AItem: JSValue): JSValue;
    Function Pop: JSValue;
    Function Peek: JSValue;
  end;

{ TStack class }

  TStack = class(TOrderedList)
  protected
    Procedure PushItem(AItem: JSValue); override;
  end;

{ TObjectStack class }

  TObjectStack = class(TStack)
  public
    Function Push(AObject: TObject): TObject; reintroduce;
    Function Pop: TObject; reintroduce;
    Function Peek: TObject; reintroduce;
  end;

{ TQueue class }

  TQueue = class(TOrderedList)
  protected
    Procedure PushItem(AItem: JSValue); override;
  end;

{ TObjectQueue class }

  TObjectQueue = class(TQueue)
  public
    Function Push(AObject: TObject): TObject; reintroduce;
    Function Pop: TObject; reintroduce;
    Function Peek: TObject; reintroduce;
  end;

{ ---------------------------------------------------------------------
    Hash support, implemented by Dean Zobec
  ---------------------------------------------------------------------}


  { Must return a Longword value in the range 0..TableSize,
   usually via a mod operator;  }
  THashFunction = Function(const S: string; const TableSize: Longword): Longword;


  { THTNode }

  THTCustomNode = class(TObject)
  private
    FKey: string;
  public
    constructor CreateWith(const AString: String);
    Function HasKey(const AKey: string): boolean;
    property Key: string read FKey;
  end;
  THTCustomNodeClass = Class of THTCustomNode;


  { TFPCustomHashTable }

  TFPCustomHashTable = class(TObject)
  private
    FHashTable: TFPObjectList;
    FHashFunction: THashFunction;
    FCount: Longword;
    Function GetDensity: Longword;
    Function GetNumberOfCollisions: Longword;
    Procedure SetHashTableSize(const Value: Longword);
    Procedure InitializeHashTable;
    Function GetVoidSlots: Longword;
    Function GetLoadFactor: double;
    Function GetAVGChainLen: double;
    Function GetMaxChainLength: Longword;
  protected
    FHashTableSize: Longword;
    Function Chain(const index: Longword):TFPObjectList;
    Function CreateNewNode(const aKey : string) : THTCustomNode; virtual; abstract;
    Procedure AddNode(ANode : THTCustomNode); virtual; abstract;
    Function ChainLength(const ChainIndex: Longword): Longword; virtual;
    Function FindOrCreateNew(const aKey: string): THTCustomNode; virtual;
    Procedure SetHashFunction(AHashFunction: THashFunction); virtual;
    Function FindChainForAdd(Const aKey : String) : TFPObjectList;
  public
    constructor Create; reintroduce;
    constructor CreateWith(AHashTableSize: Longword; aHashFunc: THashFunction);
    destructor Destroy; override;
    Procedure ChangeTableSize(const ANewSize: Longword); virtual;
    Procedure Clear; virtual;
    Procedure Delete(const aKey: string); virtual;
    Function Find(const aKey: string): THTCustomNode;
    Function IsEmpty: boolean;
    property HashFunction: THashFunction read FHashFunction write SetHashFunction;
    property Count: Longword read FCount;
    property HashTableSize: Longword read FHashTableSize write SetHashTableSize;
    property HashTable: TFPObjectList read FHashTable;
    property VoidSlots: Longword read GetVoidSlots;
    property LoadFactor: double read GetLoadFactor;
    property AVGChainLen: double read GetAVGChainLen;
    property MaxChainLength: Longword read GetMaxChainLength;
    property NumberOfCollisions: Longword read GetNumberOfCollisions;
    property Density: Longword read GetDensity;
  end;

  { TFPDataHashTable : Hash table with simple data JSValues }

  THTDataNode = Class(THTCustomNode)
  Private
    FData: JSValue;
  public
    property Data: JSValue read FData write FData;
  end;
  // For compatibility
  THTNode = THTDataNode;

  TDataIteratorMethod = Procedure(Item: JSValue; const Key: string; var Continue: Boolean) of object;
  TDataIteratorCallBack = Procedure(Item: JSValue; const Key: string; var Continue: Boolean);

  // For compatibility
  TIteratorMethod = TDataIteratorMethod;

  TFPDataHashTable = Class(TFPCustomHashTable)
  Private
    FIteratorCallBack: TDataIteratorCallBack;
    Procedure CallbackIterator(Item: JSValue; const Key: string; var Continue: Boolean);
  Protected
    Function CreateNewNode(const aKey : String) : THTCustomNode; override;
    Procedure AddNode(ANode : THTCustomNode); override;
    Procedure SetData(const index: string; const AValue: JSValue); virtual;
    Function GetData(const index: string):JSValue; virtual;
    Function ForEachCall(aMethod: TDataIteratorMethod): THTDataNode; virtual;
  Public
    Function Iterate(aMethod: TDataIteratorMethod): JSValue; virtual;
    Function Iterate(aMethod: TDataIteratorCallBack): JSValue; virtual;
    Procedure Add(const aKey: string; AItem: JSValue); virtual;
    property Items[const index: string]: JSValue read GetData write SetData; default;
  end;

  { TFPStringHashTable : Hash table with simple strings as data }
  THTStringNode = Class(THTCustomNode)
  Private
    FData : String;
  public
    property Data: String read FData write FData;
  end;
  
  TStringIteratorMethod = Procedure(Item: String; const Key: string; var Continue: Boolean) of object;
  TStringIteratorCallback = Procedure(Item: String; const Key: string; var Continue: Boolean);

  TFPStringHashTable = Class(TFPCustomHashTable)
  Private
    FIteratorCallBack: TStringIteratorCallback;
    Procedure CallbackIterator(Item: String; const Key: string; var Continue: Boolean);
  Protected
    Function CreateNewNode(const aKey : String) : THTCustomNode; override;
    Procedure AddNode(ANode : THTCustomNode); override;
    Procedure SetData(const Index, AValue: string); virtual;
    Function GetData(const index: string): String; virtual;
    Function ForEachCall(aMethod: TStringIteratorMethod): THTStringNode; virtual;
  Public
    Function Iterate(aMethod: TStringIteratorMethod): String; virtual;
    Function Iterate(aMethod: TStringIteratorCallback): String; virtual;
    Procedure Add(const aKey,aItem: string); virtual;
    property Items[const index: string]: String read GetData write SetData; default;
  end;

  { TFPStringHashTable : Hash table with simple strings as data }


  THTObjectNode = Class(THTCustomNode)
  Private
    FData : TObject;
  public
    property Data: TObject read FData write FData;
  end;

  THTOwnedObjectNode = Class(THTObjectNode)
  public
    destructor Destroy; override;
  end;

  TObjectIteratorMethod = Procedure(Item: TObject; const Key: string; var Continue: Boolean) of object;
  TObjectIteratorCallback = Procedure(Item: TObject; const Key: string; var Continue: Boolean);

  TFPObjectHashTable = Class(TFPCustomHashTable)
  Private
    FOwnsObjects : Boolean;
    FIteratorCallBack: TObjectIteratorCallback;
    procedure CallbackIterator(Item: TObject; const Key: string; var Continue: Boolean);
  Protected
    Function CreateNewNode(const aKey : String) : THTCustomNode; override;
    Procedure AddNode(ANode : THTCustomNode); override;
    Procedure SetData(const Index: string; AObject : TObject); virtual;
    Function GetData(const index: string): TObject; virtual;
    Function ForEachCall(aMethod: TObjectIteratorMethod): THTObjectNode; virtual;
  Public
    constructor Create(AOwnsObjects : Boolean = True); reintroduce;
    constructor CreateWith(AHashTableSize: Longword; aHashFunc: THashFunction; AOwnsObjects : Boolean = True); reintroduce;
    Function Iterate(aMethod: TObjectIteratorMethod): TObject; virtual;
    Function Iterate(aMethod: TObjectIteratorCallback): TObject; virtual;
    Procedure Add(const aKey: string; AItem : TObject); virtual;
    property Items[const index: string]: TObject read GetData write SetData; default;
    Property OwnsObjects : Boolean Read FOwnsObjects;
  end;

  EDuplicate = class(Exception);
  EKeyNotFound = class(Exception);

  Function RSHash(const S: string; const TableSize: Longword): Longword;

{ ---------------------------------------------------------------------
    Bucket lists as in Delphi
  ---------------------------------------------------------------------}


Type
  TBucketItem = record
    Item, Data: JSValue;
  end;
  TBucketItemArray = array of TBucketItem;

  TBucket = record
    Count : Integer;
    Items : TBucketItemArray;
  end;
  TBucketArray = array of TBucket;

  TBucketProc = Reference to Procedure(AInfo, AItem, AData: JSValue; out AContinue: Boolean);

{ ---------------------------------------------------------------------
  TCustomBucketList
  ---------------------------------------------------------------------}

  { TCustomBucketList }

  TCustomBucketList = class(TObject)
  private
    FBuckets: TBucketArray;
    Function GetBucketCount: Integer;
    Function GetData(AItem: JSValue): JSValue;
    Procedure SetData(AItem: JSValue; const AData: JSValue);
    Procedure SetBucketCount(const Value: Integer);
  protected
    Procedure GetBucketItem(AItem: JSValue; out ABucket, AIndex: Integer);
    Function AddItem(ABucket: Integer; AItem, AData: JSValue): JSValue; virtual;
    Function BucketFor(AItem: JSValue): Integer; virtual; abstract;
    Function DeleteItem(ABucket: Integer; AIndex: Integer): JSValue; virtual;
    Procedure Error(Msg : String; Args : Array of JSValue);
    Function FindItem(AItem: JSValue; out ABucket, AIndex: Integer): Boolean; virtual;
    property Buckets: TBucketArray read FBuckets;
    property BucketCount: Integer read GetBucketCount write SetBucketCount;
  public
    destructor Destroy; override;
    Procedure Clear;
    Function Add(AItem, AData: JSValue): JSValue;
    Procedure Assign(AList: TCustomBucketList);
    Function Exists(AItem: JSValue): Boolean;
    Function Find(AItem: JSValue; out AData: JSValue): Boolean;
    Function ForEach(AProc: TBucketProc; AInfo: JSValue): Boolean;
    Function ForEach(AProc: TBucketProc): Boolean;
    Function Remove(AItem: JSValue): JSValue;
    property Data[AItem: JSValue]: JSValue read GetData write SetData; default;
  end;

{ ---------------------------------------------------------------------
  TBucketList
  ---------------------------------------------------------------------}


  TBucketListSizes = (bl2, bl4, bl8, bl16, bl32, bl64, bl128, bl256);

  { TBucketList }

  TBucketList = class(TCustomBucketList)
  private
    FBucketMask: Byte;
  protected
    Function BucketFor(AItem: JSValue): Integer; override;
  public
    constructor Create(ABuckets: TBucketListSizes = bl16); reintroduce;
  end;

{ ---------------------------------------------------------------------
  TObjectBucketList
  ---------------------------------------------------------------------}

  { TObjectBucketList }

  TObjectBucketList = class(TBucketList)
  protected
    Function GetData(AItem: TObject): TObject; reintroduce;
    Procedure SetData(AItem: TObject; const AData: TObject); reintroduce;
  public
    Function Add(AItem, AData: TObject): TObject; reintroduce;
    Function Remove(AItem: TObject): TObject; reintroduce;
    property Data[AItem: TObject]: TObject read GetData write SetData; default;
  end;


implementation

uses
  js;

ResourceString
  DuplicateMsg   = 'An item with key %0:s already exists';
  //KeyNotFoundMsg = 'Method: %0:s key [''%1:s''] not found in container';
  NotEmptyMsg    = 'Hash table not empty.';
  SErrNoSuchItem = 'No item in list for %p';
  SDuplicateItem = 'Item already exists in list: %p';

const
  NPRIMES = 28;

  PRIMELIST: array[0 .. NPRIMES-1] of Longword =
  ( 53,         97,         193,       389,       769,
    1543,       3079,       6151,      12289,     24593,
    49157,      98317,      196613,    393241,    786433,
    1572869,    3145739,    6291469,   12582917,  25165843,
    50331653,   100663319,  201326611, 402653189, 805306457,
    1610612741, 3221225473, 4294967291 );

constructor TFPObjectList.Create(FreeObjects : boolean);
begin
  Create;
  FFreeObjects:=Freeobjects;
end;

destructor TFPObjectList.Destroy;
begin
  if (FList <> nil) then
  begin
    Clear;
    FList.Destroy;
  end;
  inherited Destroy;
end;

Procedure TFPObjectList.Clear;
var
  i: integer;
  O : TObject;
begin
  if FFreeObjects then
    for i:=FList.Count-1 downto 0 do
      begin
      O:=TObject(FList[i]);
      FList[i]:=Nil;
      O.Free;
      end;
  FList.Clear;
end;

constructor TFPObjectList.Create;
begin
  inherited Create;
  FList:=TFPList.Create;
  FFreeObjects:=True;
end;

Function TFPObjectList.GetCount: integer;
begin
  Result:=FList.Count;
end;

Procedure TFPObjectList.SetCount(const AValue: integer);
begin
  if FList.Count <> AValue then
    FList.Count:=AValue;
end;

Function TFPObjectList.GetItem(Index: Integer): TObject;
begin
  Result:=TObject(FList[Index]);
end;

Procedure TFPObjectList.SetItem(Index: Integer; AObject: TObject);

Var
  O : TObject;

begin
  if OwnsObjects then
    begin
    O:=TObject(FList[Index]);
    FList[Index]:=AObject;
    O.Free;
    end
  else
    FList[index]:=AObject;
end;

Procedure TFPObjectList.SetCapacity(NewCapacity: Integer);
begin
  FList.Capacity:=NewCapacity;
end;

Function TFPObjectList.GetCapacity: integer;
begin
  Result:=FList.Capacity;
end;

Function TFPObjectList.Add(AObject: TObject): Integer;
begin
  Result:=FList.Add(AObject);
end;

Procedure TFPObjectList.Delete(Index: Integer);

Var
  O : TObject;

begin
  if OwnsObjects then
    begin
    O:=TObject(FList[Index]);
    FList[Index]:=Nil;
    O.Free;
    end;
  FList.Delete(Index);
end;

Procedure TFPObjectList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

Function TFPObjectList.Expand: TFPObjectList;
begin
  FList.Expand;
  Result:=Self;
end;

Function TFPObjectList.Extract(Item: TObject): TObject;
begin
  Result:=TObject(FList.Extract(Item));
end;

Function TFPObjectList.Remove(AObject: TObject): Integer;

Var
  O : TObject;

begin
  Result:=IndexOf(AObject);
  if (Result <> -1) then
    begin
    if OwnsObjects then
      begin
      O:=TObject(FList[Result]);
      FList[Result]:=Nil;
      O.Free;
      end;
    FList.Delete(Result);
    end;
end;

Function TFPObjectList.IndexOf(AObject: TObject): Integer;
begin
  Result:=FList.IndexOf(JSValue(AObject));
end;

Function TFPObjectList.FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt : Integer): Integer;
var
  I : Integer;
begin
  I:=AStartAt;
  Result:=-1;
  if AExact then
    while (I<Count) and (Result=-1) do
      if Items[i].ClassType=AClass then
        Result:=I
      else
        Inc(I)
  else
    while (I<Count) and (Result=-1) do
      if Items[i].InheritsFrom(AClass) then
        Result:=I
      else
        Inc(I);
end;

Procedure TFPObjectList.Insert(Index: Integer; AObject: TObject);
begin
  FList.Insert(Index, JSValue(AObject));
end;

Procedure TFPObjectList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

Procedure TFPObjectList.Assign(Obj: TFPObjectList);
var
  i: Integer;
begin
  Clear;
  for i:=0 to Obj.Count - 1 do
    Add(Obj[i]);
end;

Procedure TFPObjectList.Pack;
begin
  FList.Pack;
end;

Procedure TFPObjectList.Sort(Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

Function TFPObjectList.First: TObject;
begin
  Result:=TObject(FList.First);
end;

Function TFPObjectList.Last: TObject;
begin
  Result:=TObject(FList.Last);
end;

Procedure TFPObjectList.ForEachCall(proc2call:TObjectListCallback;arg:JSValue);
begin
  FList.ForEachCall(TListCallBack(proc2call),arg);
end;

{ TObjectList }

constructor TObjectList.Create(FreeObjects: boolean);
begin
  inherited Create;
  FFreeObjects:=FreeObjects;
end;

constructor TObjectList.Create;
begin
  inherited Create;
  FFreeObjects:=True;
end;

Procedure TObjectList.Notify(Ptr: JSValue; Action: TListNotification);

Var
  O : TObject;

begin
  if FFreeObjects then
    if (Action=lnDeleted) then
      begin
      O:=TObject(Ptr);
      O.Free;
      end;
  inherited Notify(Ptr,Action);
end;


Function TObjectList.GetItem(Index: Integer): TObject;
begin
  Result:=TObject(inherited Get(Index));
end;


Procedure TObjectList.SetItem(Index: Integer; AObject: TObject);
begin
  // Put will take care of deleting old one in Notify.
  Put(Index,JSValue(AObject));
end;


Function TObjectList.Add(AObject: TObject): Integer;
begin
  Result:=inherited Add(JSValue(AObject));
end;


Function TObjectList.Extract(Item: TObject): TObject;
begin
  Result:=TObject(inherited Extract(JSValue(Item)));
end;


Function TObjectList.Remove(AObject: TObject): Integer;
begin
  Result:=inherited Remove(JSValue(AObject));
end;


Function TObjectList.IndexOf(AObject: TObject): Integer;
begin
  Result:=inherited IndexOf(JSValue(AObject));
end;


Function TObjectList.FindInstanceOf(AClass: TClass; AExact: Boolean;
  AStartAt: Integer): Integer;
var
  I : Integer;
begin
  I:=AStartAt;
  Result:=-1;
  if AExact then
    while (I<Count) and (Result=-1) do
      if Items[i].ClassType=AClass then
        Result:=I
      else
        Inc(I)
  else
    while (I<Count) and (Result=-1) do
      if Items[i].InheritsFrom(AClass) then
        Result:=I
      else
        Inc(I);
end;


Procedure TObjectList.Insert(Index: Integer; AObject: TObject);
begin
  Inherited Insert(Index,JSValue(AObject));
end;


Function TObjectList.First: TObject;
begin
  Result:=TObject(inherited First);
end;


Function TObjectList.Last: TObject;
begin
  Result:=TObject(inherited Last);
end;

{ TListComponent }

type
  TlistComponent = class(TComponent)
  private
    Flist : TComponentList;
  public
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

Procedure TlistComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation=opRemove) then
    Flist.HandleFreeNotify(Self,AComponent);
  inherited;
end;

{ TComponentList }

Function TComponentList.Add(AComponent: TComponent): Integer;
begin
  Result:=inherited Add(AComponent);
end;

destructor TComponentList.Destroy;
begin
  inherited;
  FreeAndNil(FNotifier);
end;

Function TComponentList.Extract(Item: TComponent): TComponent;
begin
  Result:=TComponent(inherited Extract(Item));
end;

Function TComponentList.First: TComponent;
begin
  Result:=TComponent(inherited First);
end;

Function TComponentList.GetItems(Index: Integer): TComponent;
begin
  Result:=TComponent(inherited Items[Index]);
end;

Procedure TComponentList.HandleFreeNotify(Sender: TObject;
  AComponent: TComponent);
begin
  Extract(AComponent);
  if Sender=nil then ;
end;

Function TComponentList.IndexOf(AComponent: TComponent): Integer;
begin
  Result:=inherited IndexOf(AComponent);
end;

Procedure TComponentList.Insert(Index: Integer; AComponent: TComponent);
begin
  inherited Insert(Index,AComponent)
end;

Function TComponentList.Last: TComponent;
begin
  Result:=TComponent(inherited Last);
end;

Procedure TComponentList.Notify(Ptr: JSValue; Action: TListNotification);
begin
  if FNotifier=nil then
    begin
    FNotifier:=TlistComponent.Create(nil);
    TlistComponent(FNotifier).FList:=Self;
    end;
  if Assigned(Ptr) then
    with TComponent(Ptr) do
      case Action of
        lnAdded : FreeNotification(FNotifier);
        lnExtracted, lnDeleted: RemoveFreeNotification(FNotifier);
      end;
  inherited Notify(Ptr, Action);
end;

Function TComponentList.Remove(AComponent: TComponent): Integer;
begin
  Result:=inherited Remove(AComponent);
end;

Procedure TComponentList.SetItems(Index: Integer; AComponent: TComponent);
begin
  Put(Index,AComponent);
end;

{ TClassList }

Function TClassList.Add(AClass: TClass): Integer;
begin
  Result:=inherited Add(JSValue(AClass));
end;

Function TClassList.Extract(Item: TClass): TClass;
begin
  Result:=TClass(inherited Extract(JSValue(Item)));
end;

Function TClassList.First: TClass;
begin
  Result:=TClass(inherited First);
end;

Function TClassList.GetItems(Index: Integer): TClass;
begin
  Result:=TClass(inherited Items[Index]);
end;

Function TClassList.IndexOf(AClass: TClass): Integer;
begin
  Result:=inherited IndexOf(JSValue(AClass));
end;

Procedure TClassList.Insert(Index: Integer; AClass: TClass);
begin
  inherited Insert(Index,JSValue(AClass));
end;

Function TClassList.Last: TClass;
begin
  Result:=TClass(inherited Last);
end;

Function TClassList.Remove(AClass: TClass): Integer;
begin
  Result:=inherited Remove(JSValue(AClass));
end;

Procedure TClassList.SetItems(Index: Integer; AClass: TClass);
begin
  Put(Index,JSValue(AClass));
end;

{ TOrderedList }

Function TOrderedList.AtLeast(ACount: Integer): Boolean;
begin
  Result:=(FList.Count>=Acount)
end;

Function TOrderedList.Count: Integer;
begin
  Result:=FList.Count;
end;

constructor TOrderedList.Create;
begin
  FList:=Tlist.Create;
end;

destructor TOrderedList.Destroy;
begin
  FList.Free;
end;

Function TOrderedList.Peek: JSValue;
begin
  if AtLeast(1) then
    Result:=PeekItem
  else
    Result:=nil;
end;

Function TOrderedList.PeekItem: JSValue;

begin
  with Flist do
    Result:=Items[Count-1]
end;

Function TOrderedList.Pop: JSValue;
begin
  If Atleast(1) then
    Result:=PopItem
  else
    Result:=nil;
end;

Function TOrderedList.PopItem: JSValue;
begin
  with FList do
    if Count>0 then
      begin
      Result:=Items[Count-1];
      Delete(Count-1);
      end
    else
      Result:=nil;
end;

Function TOrderedList.Push(AItem: JSValue): JSValue;
begin
  PushItem(AItem);
  Result:=AItem;
end;

{ TStack }

Procedure TStack.PushItem(AItem: JSValue);
begin
  FList.Add(AItem);
end;

{ TObjectStack }

Function TObjectStack.Peek: TObject;
begin
  Result:=TObject(inherited Peek);
end;

Function TObjectStack.Pop: TObject;
begin
  Result:=TObject(Inherited Pop);
end;

Function TObjectStack.Push(AObject: TObject): TObject;
begin
  Result:=TObject(inherited Push(JSValue(AObject)));
end;

{ TQueue }

Procedure TQueue.PushItem(AItem: JSValue);
begin
  with FList Do
    Insert(0,AItem);
end;

{ TObjectQueue }

Function TObjectQueue.Peek: TObject;
begin
  Result:=TObject(inherited Peek);
end;

Function TObjectQueue.Pop: TObject;
begin
  Result:=TObject(inherited Pop);
end;

Function TObjectQueue.Push(AObject: TObject): TObject;
begin
  Result:=TObject(inherited Push(JSValue(AObject)));
end;


{*****************************************************************************
                            TFPHashList
*****************************************************************************}
(*
    Function FPHash(const s:shortstring):LongWord;
    var
      p,pmax : PChar;
    begin
{$push}
{$Q-}
      Result:=0;
      p:=@s[1];
      pmax:=@s[length(s)+1];
      while (p<pmax) do
        begin
          Result:=LongWord(LongInt(Result shl 5) - LongInt(Result)) xor LongWord(P^);
          Inc(p);
        end;
{$pop}
    end;

    Function FPHash(P: PChar; Len: Integer): LongWord;
    var
      pmax : PChar;
    begin
{$push}
{$Q-}
      Result:=0;
      pmax:=p+len;
      while (p<pmax) do
        begin
          Result:=LongWord(LongInt(Result shl 5) - LongInt(Result)) xor LongWord(P^);
          Inc(p);
        end;
{$pop}
    end;

*)

{ ---------------------------------------------------------------------
    Hash support, by Dean Zobec
  ---------------------------------------------------------------------}

{ Default hash Function }

Function RSHash(const S: string; const TableSize: Longword): Longword;
const
  b = 378551;
var
  a: Longword;
  i: Longword;
begin
  a:=63689;
  Result:=0;
  if length(s)>0 then
    for i:=1 to Length(S) do
      begin
      Result:=Result * a + Ord(S[i]);
      a:=a * b;
      end;
  Result:=(Result and $7FFFFFFF) mod TableSize;
end;

{ THTNode }

constructor THTCustomNode.CreateWith(const AString: string);
begin
  inherited Create;
  FKey:=AString;
end;

Function THTCustomNode.HasKey(const AKey: string): boolean;
begin
  Result:=(AKey=FKey);
end;

{ TFPCustomHashTable }

constructor TFPCustomHashTable.Create;
begin
  CreateWith(196613,@RSHash);
end;

constructor TFPCustomHashTable.CreateWith(AHashTableSize: Longword;
  aHashFunc: THashFunction);
begin
  inherited Create;
  FHashTable:=TFPObjectList.Create(True);
  HashTableSize:=AHashTableSize;
  FHashFunction:=aHashFunc;
end;

destructor TFPCustomHashTable.Destroy;
begin
  FHashTable.Free;
  inherited Destroy;
end;

Function TFPCustomHashTable.GetDensity: Longword;
begin
  Result:=FHashTableSize - VoidSlots
end;

Function TFPCustomHashTable.GetNumberOfCollisions: Longword;
begin
  Result:=FCount -(FHashTableSize - VoidSlots)
end;

Procedure TFPCustomHashTable.SetHashTableSize(const Value: Longword);
var
  i: Longword;
  newSize: Longword;
begin
  if Value <> FHashTableSize then
    begin
    i:=0;
    while (PRIMELIST[i] < Value) and (i < 27) do
     Inc(i);
    newSize:=PRIMELIST[i];
    if Count = 0 then
      begin
      FHashTableSize:=newSize;
      InitializeHashTable;
      end
    else
      ChangeTableSize(newSize);
    end;
end;

Procedure TFPCustomHashTable.InitializeHashTable;
var
  i: LongWord;
begin
  if FHashTableSize>0 Then
    for i:=0 to FHashTableSize-1 do
      FHashTable.Add(nil);
  FCount:=0;
end;

Procedure TFPCustomHashTable.ChangeTableSize(const ANewSize: Longword);
var
  SavedTable, List: TFPObjectList;
  SavedTableSize: Longword;
  i, j: Longword;
  temp: THTCustomNode;
begin
  SavedTable:=FHashTable;
  SavedTableSize:=FHashTableSize;
  FHashTableSize:=ANewSize;
  FHashTable:=TFPObjectList.Create(True);
  InitializeHashTable;
  if SavedTableSize>0 Then
    for i:=0 to SavedTableSize-1 do
      begin
      List:=TFPObjectList(SavedTable[i]);
      if Assigned(List) then
        for j:=0 to List.Count -1 do
          begin
          temp:=THTCustomNode(List[j]);
          AddNode(temp);
          end;
      end;
  SavedTable.Free;
end;

Procedure TFPCustomHashTable.SetHashFunction(AHashFunction: THashFunction);
begin
  if IsEmpty then
    FHashFunction:=AHashFunction
  else
    raise Exception.Create(NotEmptyMsg);
end;

Function TFPCustomHashTable.Find(const aKey: string): THTCustomNode;
var
  hashCode: Longword;
  chn: TFPObjectList;
  i: Longword;
begin
  hashCode:=FHashFunction(aKey, FHashTableSize);
  chn:=Chain(hashCode);
  if Assigned(chn) then
    if chn.count>0 then
      for i:=0 to chn.Count - 1 do
        if THTCustomNode(chn[i]).Key=aKey then
          Exit(THTCustomNode(chn[i]));
  Result:=nil;
end;

Function TFPCustomHashTable.FindChainForAdd(Const aKey : String) : TFPObjectList;
var
  hashCode: Longword;
  i: Longword;
begin
  hashCode:=FHashFunction(aKey, FHashTableSize);
  Result:=Chain(hashCode);
  if Assigned(Result)  then
    begin
    if Result.count>0 then
      for i:=0 to Result.Count - 1 do
        if (THTCustomNode(Result[i]).Key=aKey) then
          raise EDuplicate.CreateFmt(DuplicateMsg, [aKey]);
    end
  else
    begin
    FHashTable[hashcode]:=TFPObjectList.Create(True);
    Result:=Chain(hashCode);
    end;
  Inc(FCount);
end;


Procedure TFPCustomHashTable.Delete(const aKey: string);
var
  hashCode: Longword;
  chn: TFPObjectList;
  i: Longword;
begin
  hashCode:=FHashFunction(aKey, FHashTableSize);
  chn:=Chain(hashCode);
  if Assigned(chn) then
    if chn.count>0 then
      for i:=0 to chn.Count - 1 do
        if THTCustomNode(chn[i]).Key=aKey then
          begin
          chn.Delete(i);
          dec(FCount);
          Exit;
          end;
end;

Function TFPCustomHashTable.IsEmpty: boolean;
begin
  Result:=(FCount = 0);
end;

Function TFPCustomHashTable.Chain(const index: Longword): TFPObjectList;
begin
  Result:=TFPObjectList(FHashTable[index]);
end;

Function TFPCustomHashTable.GetVoidSlots: Longword;
var
  i: Longword;
  num: Longword;
begin
  num:=0;
  if FHashTableSize>0 then
    for i:= 0 to FHashTableSize-1 do
      if not Assigned(Chain(i)) then
        Inc(num);
  Result:=num;
end;

Function TFPCustomHashTable.GetLoadFactor: double;
begin
  Result:=Count / FHashTableSize;
end;

Function TFPCustomHashTable.GetAVGChainLen: double;
begin
  Result:=Count / (FHashTableSize - VoidSlots);
end;

Function TFPCustomHashTable.GetMaxChainLength: Longword;
var
  i: Longword;
begin
  Result:=0;
  if FHashTableSize>0 Then
   for i:=0 to FHashTableSize-1 do
      if ChainLength(i) > Result then
        Result:=ChainLength(i);
end;

Function TFPCustomHashTable.FindOrCreateNew(const aKey: string): THTCustomNode;
var
  hashCode: Longword;
  chn: TFPObjectList;
  i: Longword;
begin
  hashCode:=FHashFunction(aKey, FHashTableSize);
  chn:=Chain(hashCode);
  if Assigned(chn)  then
    begin
    if chn.count>0 then
      for i:=0 to chn.Count - 1 do
        if (THTCustomNode(chn[i]).Key=aKey) then
          Exit(THTNode(chn[i]));
    end
  else
    begin
    FHashTable[hashcode]:=TFPObjectList.Create(true);
    chn:=Chain(hashcode);
    end;
  Inc(FCount);
  Result:=CreateNewNode(aKey);
  chn.Add(Result);
end;

Function TFPCustomHashTable.ChainLength(const ChainIndex: Longword): Longword;
begin
  if Assigned(Chain(ChainIndex)) then
    Result:=Chain(ChainIndex).Count
  else
    Result:=0;
end;

Procedure TFPCustomHashTable.Clear;
var
  i: Longword;
begin
  if FHashTableSize>0 then
    for i:=0 to FHashTableSize - 1 do
      if Assigned(Chain(i)) then
        Chain(i).Clear;
  FCount:=0;
end;



{ TFPDataHashTable }

Procedure TFPDataHashTable.Add(const aKey: string; aItem: JSValue);
var
  chn: TFPObjectList;
  NewNode: THtDataNode;
begin
  chn:=FindChainForAdd(akey);
  NewNode:=THtDataNode(CreateNewNode(aKey));
  NewNode.Data:=aItem;
  chn.Add(NewNode);
end;

Function TFPDataHashTable.GetData(const Index: string): JSValue;
var
  node: THTDataNode;
begin
  node:=THTDataNode(Find(Index));
  if Assigned(node) then
    Result:=node.Data
  else
    Result:=nil;
end;

Procedure TFPDataHashTable.SetData(const index: string; const AValue: JSValue);
begin
  THTDataNode(FindOrCreateNew(index)).Data:=AValue;
end;

Function TFPDataHashTable.CreateNewNode(const aKey : string) : THTCustomNode;

begin
  Result:=THTDataNode.CreateWith(aKey);
end;

Function TFPDataHashTable.Iterate(aMethod: TDataIteratorMethod): JSValue;
var
  N : THTDataNode;
begin
  N:=ForEachCall(AMethod);
  if Assigned(N) then
    Result:=N.Data
  else
    Result:=nil;
end;

Procedure TFPDataHashTable.CallbackIterator(Item: JSValue; const Key: string; var Continue: Boolean);
begin
  FIteratorCallBack(Item, Key, Continue);
end;

Function TFPDataHashTable.Iterate(aMethod: TDataIteratorCallBack): JSValue;
begin
  FIteratorCallBack := aMethod;
  Result := Iterate(@CallbackIterator);
end;

Function TFPDataHashTable.ForEachCall(aMethod: TDataIteratorMethod): THTDataNode;
var
  i, j: Longword;
  continue: Boolean;
begin
  Result:=nil;
  continue:=true;
  if FHashTableSize>0 then
    for i:=0 to FHashTableSize-1 do
      if Assigned(Chain(i)) then
        if chain(i).count>0 then
          for j:=0 to Chain(i).Count-1 do
            begin
            aMethod(THTDataNode(Chain(i)[j]).Data, THTDataNode(Chain(i)[j]).Key, continue);
            if not continue then
              begin
              Result:=THTDataNode(Chain(i)[j]);
              Exit;
              end;
           end;
end;

Procedure TFPDataHashTable.AddNode(ANode : THTCustomNode);
begin
  with THTDataNode(ANode) do
    Add(Key,Data);
end;

{ TFPStringHashTable }

Procedure TFPStringHashTable.AddNode(ANode : THTCustomNode);
begin
  with THTStringNode(ANode) do
    Add(Key,Data);
end;

Function TFPStringHashTable.GetData(const Index: string): String;
var
  node: THTStringNode;
begin
  node:=THTStringNode(Find(Index));
  if Assigned(node) then
    Result:=node.Data
  else
    Result:='';
end;

Procedure TFPStringHashTable.SetData(const index, AValue: string);
begin
  THTStringNode(FindOrCreateNew(index)).Data:=AValue;
end;

Procedure TFPStringHashTable.Add(const aKey, aItem: string);
var
  chn: TFPObjectList;
  NewNode: THtStringNode;
begin
  chn:=FindChainForAdd(akey);
  NewNode:=THtStringNode(CreateNewNode(aKey));
  NewNode.Data:=aItem;
  chn.Add(NewNode);
end;

Function TFPStringHashTable.CreateNewNode(const aKey : string) : THTCustomNode;
begin
  Result:=THTStringNode.CreateWith(aKey);
end;

Function TFPStringHashTable.Iterate(aMethod: TStringIteratorMethod): String;
var
  N : THTStringNode;
begin
  N:=ForEachCall(AMethod);
  if Assigned(N) then
    Result:=N.Data
  else
    Result:='';
end;

Procedure TFPStringHashTable.CallbackIterator(Item: String; const Key: string; var Continue: Boolean);
begin
  FIteratorCallBack(Item, Key, Continue);
end;

Function TFPStringHashTable.Iterate(aMethod: TStringIteratorCallback): String;
begin
  FIteratorCallBack := aMethod;
  Result := Iterate(@CallbackIterator);
end;

Function TFPStringHashTable.ForEachCall(aMethod: TStringIteratorMethod): THTStringNode;
var
  i, j: Longword;
  continue: boolean;
begin
  Result:=nil;
  continue:=True;
  if FHashTableSize>0 then
    for i:=0 to FHashTableSize-1 do
      if Assigned(Chain(i)) then
        if chain(i).Count>0 then
          for j:=0 to Chain(i).Count-1 do
            begin
            aMethod(THTStringNode(Chain(i)[j]).Data, THTStringNode(Chain(i)[j]).Key, continue);
            if not continue then
              begin
              Result:=THTStringNode(Chain(i)[j]);
              Exit;
              end;
            end;
end;

{ TFPObjectHashTable }

Procedure TFPObjectHashTable.AddNode(ANode : THTCustomNode);
begin
  With THTObjectNode(ANode) do
    Add(Key,Data);
end;

Function TFPObjectHashTable.GetData(const Index: string): TObject;
var
  node: THTObjectNode;
begin
  node:=THTObjectNode(Find(Index));
  if Assigned(node) then
    Result:=node.Data
  else
    Result:=nil;
end;

Procedure TFPObjectHashTable.SetData(const index : string; AObject : TObject);
begin
  THTObjectNode(FindOrCreateNew(index)).Data:=AObject;
end;

Procedure TFPObjectHashTable.Add(const aKey: string; AItem : TObject);
var
  chn: TFPObjectList;
  NewNode: THTObjectNode;
begin
  chn:=FindChainForAdd(akey);
  NewNode:=THTObjectNode(CreateNewNode(aKey));
  NewNode.Data:=aItem;
  chn.Add(NewNode);
end;

Function TFPObjectHashTable.CreateNewNode(const aKey : string) : THTCustomNode;
begin
  if OwnsObjects then
    Result:=THTOwnedObjectNode.CreateWith(aKey)
  else
    Result:=THTObjectNode.CreateWith(aKey);
end;


Function TFPObjectHashTable.Iterate(aMethod: TObjectIteratorMethod): TObject;
var
  N : THTObjectNode;
begin
  N:=ForEachCall(AMethod);
  if Assigned(N) then
    Result:=N.Data
  else
    Result:=nil;
end;

Procedure TFPObjectHashTable.CallbackIterator(Item: TObject; const Key: string; var Continue: Boolean);
begin
  FIteratorCallBack(Item, Key, Continue);
end;

Function TFPObjectHashTable.Iterate(aMethod: TObjectIteratorCallback): TObject;
begin
  FIteratorCallBack := aMethod;
  Result := Iterate(@CallbackIterator);
end;

Function TFPObjectHashTable.ForEachCall(aMethod: TObjectIteratorMethod): THTObjectNode;
var
  i, j: Longword;
  continue: boolean;
begin
  Result:=nil;
  continue:=true;
  if FHashTableSize>0 then
    for i:=0 to FHashTableSize-1 do
      if Assigned(Chain(i)) then
        if Chain(i).Count>0 then
          for j:=0 to Chain(i).Count-1 do
           begin
           aMethod(THTObjectNode(Chain(i)[j]).Data, THTObjectNode(Chain(i)[j]).Key, continue);
           if not continue then
             begin
             Result:=THTObjectNode(Chain(i)[j]);
             Exit;
             end;
           end;
end;

constructor TFPObjectHashTable.Create(AOwnsObjects : Boolean = True);
begin
  inherited Create;
  FOwnsObjects:=AOwnsObjects;
end;

constructor TFPObjectHashTable.CreateWith(AHashTableSize: Longword; aHashFunc: THashFunction; AOwnsObjects : Boolean = True);
begin
  inherited CreateWith(AHashTableSize,AHashFunc);
  FOwnsObjects:=AOwnsObjects;
end;

destructor THTOwnedObjectNode.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

{ TCustomBucketList }

Function TCustomBucketList.GetData(AItem: JSValue): JSValue;
var
  B,I : Integer;
begin
  GetBucketItem(AItem,B,I);
  Result:=FBuckets[B].Items[I].Data;
end;

Function TCustomBucketList.GetBucketCount: Integer;
begin
  Result:=Length(FBuckets);
end;

Procedure TCustomBucketList.SetData(AItem: JSValue; const AData: JSValue);
var
  B,I : Integer;
begin
  GetBucketItem(AItem,B,I);
  FBuckets[B].Items[I].Data:=AData;
end;

Procedure TCustomBucketList.SetBucketCount(const Value: Integer);
begin
  if (Value<>GetBucketCount) then
    SetLength(FBuckets,Value);
end;

Procedure TCustomBucketList.GetBucketItem(AItem: JSValue; out ABucket,
  AIndex: Integer);
begin
  if not FindItem(AItem,ABucket,AIndex) then
    Error(SErrNoSuchItem,[AItem]);
end;

Function TCustomBucketList.AddItem(ABucket: Integer; AItem, AData: JSValue
  ): JSValue;
var
  L : Integer;
begin
  L:=Length(FBuckets[ABucket].Items);
  if (FBuckets[ABucket].Count=L) then
    begin
    if L<8 then
      L:=8
    else
      L:=L+L div 2;
    SetLength(FBuckets[ABucket].Items,L);
    end;
  with FBuckets[ABucket] do
    begin
    Items[Count].Item:=AItem;
    Items[Count].Data:=AData;
    Result:=AData;
    Inc(Count);
    end;
end;

Function TCustomBucketList.DeleteItem(ABucket: Integer; AIndex: Integer): JSValue;
var
  I,L : Integer;
begin
  Result:=FBuckets[ABucket].Items[AIndex].Data;
  if FBuckets[ABucket].Count=1 then
    SetLength(FBuckets[ABucket].Items,0)
  else
    begin
    L:=(FBuckets[ABucket].Count-AIndex-1);// No point in moving if last one...
    For I:=0 to L-1 do
      FBuckets[ABucket].Items[AIndex+I]:=FBuckets[ABucket].Items[AIndex+I+1];
    end;
  Dec(FBuckets[ABucket].Count);
end;

Procedure TCustomBucketList.Error(Msg: String; Args: array of JSValue);
begin
  raise ElistError.CreateFmt(Msg,Args);
end;

Function TCustomBucketList.FindItem(AItem: JSValue; out ABucket, AIndex: Integer
  ): Boolean;
var
  I : Integer;
  B : TBucket;
begin
  ABucket:=BucketFor(AItem);
  B:=FBuckets[ABucket];
  I:=B.Count-1;
  while (I>=0) and (B.Items[I].Item<>AItem) do
    Dec(I);
  Result:=I>=0;
  if Result then
    AIndex:=I;
end;

destructor TCustomBucketList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

Procedure TCustomBucketList.Clear;
var
  B : TBucket;
  I,J : Integer;
begin
  for I:=0 to Length(FBuckets)-1 do
    begin
    B:=FBuckets[I];
    for J:=B.Count-1 downto 0 do
      DeleteItem(I,J);
    end;
  SetLength(FBuckets,0);
end;

Function TCustomBucketList.Add(AItem, AData: JSValue): JSValue;
var
  B,I : Integer;
begin
  if FindItem(AItem,B,I) then
    Error(SDuplicateItem,[AItem]);
  Result:=AddItem(B,AItem,AData);
end;

Procedure TCustomBucketList.Assign(AList: TCustomBucketList);
var
  I,J : Integer;
begin
  Clear;
  SetLength(FBuckets,Length(Alist.FBuckets));
  for I:=0 to BucketCount-1 do
    begin
    SetLength(FBuckets[i].Items,Length(AList.Fbuckets[I].Items));
    for J:=0 to AList.Fbuckets[I].Count-1 do
      with AList.Fbuckets[I].Items[J] do
        AddItem(I,Item,Data);
    end;
end;

Function TCustomBucketList.Exists(AItem: JSValue): Boolean;
var
  B,I : Integer;
begin
  Result:=FindItem(AItem,B,I);
end;

Function TCustomBucketList.Find(AItem: JSValue; out AData: JSValue): Boolean;
var
  B,I : integer;
begin
  Result:=FindItem(AItem,B,I);
  if Result then
    AData:=FBuckets[B].Items[I].Data;
end;

Function TCustomBucketList.ForEach(AProc: TBucketProc): Boolean;
begin
  Result:=Foreach(aProc,Null);
end;

Function TCustomBucketList.ForEach(AProc: TBucketProc; AInfo: JSValue): Boolean;
var
  I,J,S : Integer;
  Bu : TBucket;
begin
  I:=0;
  Result:=True;
  S:=GetBucketCount;
  while Result and (I<S) do
    begin
    J:=0;
    Bu:=FBuckets[I];
    while Result and (J<Bu.Count) do
      begin
      with Bu.Items[J] do
        AProc(AInfo,Item,Data,Result);
      Inc(J);
      end;
    Inc(I);
    end;
end;

Function TCustomBucketList.Remove(AItem: JSValue): JSValue;
var
  B,I : integer;
begin
  if FindItem(AItem,B,I) then
    begin
    Result:=FBuckets[B].Items[I].Data;
    DeleteItem(B,I);
    end
  else
    Result:=nil;
end;

{ TBucketList }

Function TBucketList.BucketFor(AItem: JSValue): Integer;
begin
  // JSValues on average have a granularity of 4
  Result:=(PtrInt(AItem) shr 2) and FBucketMask;
end;

constructor TBucketList.Create(ABuckets: TBucketListSizes);
var
  L : Integer;
begin
  inherited Create;
  L:=1 shl (Ord(Abuckets)+1);
  SetBucketCount(L);
  FBucketMask:=L-1;
end;

{ TObjectBucketList }

Function TObjectBucketList.GetData(AItem: TObject): TObject;
begin
  Result:=TObject(inherited GetData(AItem));
end;

Procedure TObjectBucketList.SetData(AItem: TObject; const AData: TObject);
begin
  inherited SetData(JSValue(AItem),JSValue(AData));
end;

Function TObjectBucketList.Add(AItem, AData: TObject): TObject;
begin
  Result:=TObject(inherited Add(JSValue(AItem),JSValue(AData)));
end;

Function TObjectBucketList.Remove(AItem: TObject): TObject;
begin
  Result:=TObject(inherited Remove(JSValue(AItem)));
end;

end.
