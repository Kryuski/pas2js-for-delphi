{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2017 by Mattias Gaertner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Classes;

{$mode objfpc}

interface

uses
  RTLConsts, Types, SysUtils;

type
  TNotifyEvent = procedure(Sender: TObject) of object;

  // Notification operations :
  // Observer has changed, is freed, item added to/deleted from list, custom event.
  TFPObservedOperation = (ooChange,ooFree,ooAddItem,ooDeleteItem,ooCustom);

  EStreamError = class(Exception);
  EFCreateError = class(EStreamError);
  EFOpenError = class(EStreamError);
  EFilerError = class(EStreamError);
  EReadError = class(EFilerError);
  EWriteError = class(EFilerError);
  EClassNotFound = class(EFilerError);
  EMethodNotFound = class(EFilerError);
  EInvalidImage = class(EFilerError);
  EResNotFound = class(Exception);
  EListError = class(Exception);
  EBitsError = class(Exception);
  EStringListError = class(EListError);
  EComponentError = class(Exception);
  EParserError = class(Exception);
  EOutOfResources = class(EOutOfMemory);
  EInvalidOperation = class(Exception);

  TListAssignOp = (laCopy, laAnd, laOr, laXor, laSrcUnique, laDestUnique);
  TListSortCompare = function(Item1, Item2: JSValue): Integer;
  TListCallback = Types.TListCallback;
  TListStaticCallback = Types.TListStaticCallback;
  TAlignment = (taLeftJustify, taRightJustify, taCenter);

  { TFPListEnumerator }
  TFPList = Class;

  TFPListEnumerator = class
  private
    FList: TFPList;
    FPosition: Integer;
  public
    constructor Create(AList: TFPList); reintroduce;
    function GetCurrent: JSValue;
    function MoveNext: Boolean;
    property Current: JSValue read GetCurrent;
  end;

  { TFPList }

  TFPList = class(TObject)
  private
    FList: TJSValueDynArray;
    FCount: Integer;
    FCapacity: Integer;
    procedure CopyMove(aList: TFPList);
    procedure MergeMove(aList: TFPList);
    procedure DoCopy(ListA, ListB: TFPList);
    procedure DoSrcUnique(ListA, ListB: TFPList);
    procedure DoAnd(ListA, ListB: TFPList);
    procedure DoDestUnique(ListA, ListB: TFPList);
    procedure DoOr(ListA, ListB: TFPList);
    procedure DoXOr(ListA, ListB: TFPList);
  protected
    function Get(Index: Integer): JSValue; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    procedure Put(Index: Integer; Item: JSValue); {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    Procedure RaiseIndexError(Index: Integer);
  public
    //Type
    //  TDirection = (FromBeginning, FromEnd);
    destructor Destroy; override;
    procedure AddList(AList: TFPList);
    function Add(Item: JSValue): Integer; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    procedure Clear;
    procedure Delete(Index: Integer); {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    class procedure Error(const Msg: string; const Data: String);
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TFPList; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function Extract(Item: JSValue): JSValue;
    function First: JSValue;
    function GetEnumerator: TFPListEnumerator;
    function IndexOf(Item: JSValue): Integer;
    function IndexOfItem(Item: JSValue; Direction: TDirection): Integer;
    procedure Insert(Index: Integer; Item: JSValue); {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function Last: JSValue;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Assign (ListA: TFPList; AOperator: TListAssignOp=laCopy; ListB: TFPList=nil);
    function Remove(Item: JSValue): Integer;
    procedure Pack;
    procedure Sort(const Compare: TListSortCompare);
    procedure ForEachCall(const proc2call: TListCallback; const arg: JSValue);
    procedure ForEachCall(const proc2call: TListStaticCallback; const arg: JSValue);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: JSValue read Get write Put; default;
    property List: TJSValueDynArray read FList;
  end;

  TListNotification = (lnAdded, lnExtracted, lnDeleted);
  TList = class;

  { TListEnumerator }

  TListEnumerator = class
  private
    FList: TList;
    FPosition: Integer;
  public
    constructor Create(AList: TList); reintroduce;
    function GetCurrent: JSValue;
    function MoveNext: Boolean;
    property Current: JSValue read GetCurrent;
  end;

  { TList }

  TList = class(TObject)
  private
    FList: TFPList;
    procedure CopyMove (aList : TList);
    procedure MergeMove (aList : TList);
    procedure DoCopy(ListA, ListB : TList);
    procedure DoSrcUnique(ListA, ListB : TList);
    procedure DoAnd(ListA, ListB : TList);
    procedure DoDestUnique(ListA, ListB : TList);
    procedure DoOr(ListA, ListB : TList);
    procedure DoXOr(ListA, ListB : TList);
  protected
    function Get(Index: Integer): JSValue;
    procedure Put(Index: Integer; Item: JSValue);
    procedure Notify(aValue: JSValue; Action: TListNotification); virtual;
    procedure SetCapacity(NewCapacity: Integer);
    function GetCapacity: integer;
    procedure SetCount(NewCount: Integer);
    function GetCount: integer;
    function GetList: TJSValueDynArray;
    property FPList : TFPList Read FList;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    Procedure AddList(AList : TList);
    function Add(Item: JSValue): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: String); virtual;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TList;
    function Extract(Item: JSValue): JSValue;
    function First: JSValue;
    function GetEnumerator: TListEnumerator;
    function IndexOf(Item: JSValue): Integer;
    procedure Insert(Index: Integer; Item: JSValue);
    function Last: JSValue;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Assign (ListA: TList; AOperator: TListAssignOp=laCopy; ListB: TList=nil);
    function Remove(Item: JSValue): Integer;
    procedure Pack;
    procedure Sort(const Compare: TListSortCompare);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: JSValue read Get write Put; default;
    property List: TJSValueDynArray read GetList;
  end;

  { TPersistent }

  TPersistent = class(TObject)
  private
    //FObservers : TFPList;
    procedure AssignError(Source: TPersistent);
  protected
    procedure AssignTo(Dest: TPersistent); virtual;
    function GetOwner: TPersistent; virtual;
  public
    procedure Assign(Source: TPersistent); virtual;
    //procedure FPOAttachObserver(AObserver : TObject);
    //procedure FPODetachObserver(AObserver : TObject);
    //procedure FPONotifyObservers(ASender : TObject; AOperation: TFPObservedOperation; Data: TObject);
    function GetNamePath: string; virtual;
  end;
  TPersistentClass = Class of TPersistent;

  { TInterfacedPersistent }

  TInterfacedPersistent = class(TPersistent, IInterface)
  private
    FOwnerInterface: IInterface;
  protected
    function _AddRef: Integer;
    function _Release: Integer;
  public
    function QueryInterface(const IID: TGUID; out Obj): integer; virtual;
    procedure AfterConstruction; override;
  end;


  TStrings = Class;
  { TStringsEnumerator class }

  TStringsEnumerator = class
  private
    FStrings: TStrings;
    FPosition: Integer;
  public
    constructor Create(AStrings: TStrings); reintroduce;
    function GetCurrent: String;
    function MoveNext: Boolean;
    property Current: String read GetCurrent;
  end;


  { TStrings class }
  TStrings = class(TPersistent)
  private
    FSpecialCharsInited : boolean;
    FAlwaysQuote: Boolean;
    FQuoteChar : Char;
    FDelimiter : Char;
    FNameValueSeparator : Char;
    FUpdateCount: Integer;
    FLBS : TTextLineBreakStyle;
    FSkipLastLineBreak : Boolean;
    FStrictDelimiter : Boolean;
    FLineBreak : String;
    function GetCommaText: string;
    function GetName(Index: Integer): string;
    function GetValue(const Name: string): string;
    Function GetLBS : TTextLineBreakStyle;
    Procedure SetLBS (AValue : TTextLineBreakStyle);
    procedure SetCommaText(const Value: string);
    procedure SetValue(const Name, Value: string);
    procedure SetDelimiter(c:Char);
    procedure SetQuoteChar(c:Char);
    procedure SetNameValueSeparator(c:Char);
    procedure DoSetTextStr(const Value: string; DoClear : Boolean);
    Function GetDelimiter : Char;
    Function GetNameValueSeparator : Char;
    Function GetQuoteChar: Char;
    Function GetLineBreak : String;
    procedure SetLineBreak(const S : String);
    Function GetSkipLastLineBreak : Boolean;
    procedure SetSkipLastLineBreak(const AValue : Boolean);
  protected
    procedure Error(const Msg: string; Data: Integer);
    function Get(Index: Integer): string; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: string; virtual;
    procedure Put(Index: Integer; const S: string); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: string); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    property UpdateCount: Integer read FUpdateCount;
    Function DoCompareText(const s1,s2 : string) : PtrInt; virtual;
    Function GetDelimitedText: string;
    Procedure SetDelimitedText(Const AValue: string);
    Function GetValueFromIndex(Index: Integer): string;
    Procedure SetValueFromIndex(Index: Integer; const Value: string);
    Procedure CheckSpecialChars;
//    Class Function GetNextLine (Const Value : String; Var S : String; Var P : Integer) : Boolean;
    Function GetNextLinebreak (Const Value : String; Out S : String; Var P : Integer) : Boolean;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function Add(const S: string): Integer; virtual; overload;
//    function AddFmt(const Fmt : string; const Args : Array of const): Integer; overload;
    function AddObject(const S: string; AObject: TObject): Integer; virtual; overload;
//    function AddObject(const Fmt: string; Args : Array of const; AObject: TObject): Integer; overload;
    procedure Append(const S: string);
    procedure AddStrings(TheStrings: TStrings); overload; virtual;
    procedure AddStrings(TheStrings: TStrings; ClearFirst : Boolean); overload;
    procedure AddStrings(const TheStrings: array of string); overload; virtual;
    procedure AddStrings(const TheStrings: array of string; ClearFirst : Boolean); overload;
    function AddPair(const AName, AValue: string): TStrings; overload;
    function AddPair(const AName, AValue: string; AObject: TObject): TStrings; overload;
    Procedure AddText(Const S : String); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Obj: TObject): Boolean; override; overload;
    function Equals(TheStrings: TStrings): Boolean; overload;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetEnumerator: TStringsEnumerator;
    function IndexOf(const S: string): Integer; virtual;
    function IndexOfName(const Name: string): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer; virtual;
    procedure Insert(Index: Integer; const S: string); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: string; AObject: TObject);
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure GetNameValue(Index : Integer; Out AName,AValue : String);
    function  ExtractName(Const S:String):String;
    Property TextLineBreakStyle : TTextLineBreakStyle Read GetLBS Write SetLBS;
    property Delimiter: Char read GetDelimiter write SetDelimiter;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    property LineBreak : string Read GetLineBreak write SetLineBreak;
    Property StrictDelimiter : Boolean Read FStrictDelimiter Write FStrictDelimiter;
    property AlwaysQuote: Boolean read FAlwaysQuote write FAlwaysQuote;
    property QuoteChar: Char read GetQuoteChar write SetQuoteChar;
    Property NameValueSeparator : Char Read GetNameValueSeparator Write SetNameValueSeparator;
    property ValueFromIndex[Index: Integer]: string read GetValueFromIndex write SetValueFromIndex;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: string read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Names[Index: Integer]: string read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Values[const Name: string]: string read GetValue write SetValue;
    property Strings[Index: Integer]: string read Get write Put; default;
    property Text: string read GetTextStr write SetTextStr;
    Property SkipLastLineBreak : Boolean Read GetSkipLastLineBreak Write SetSkipLastLineBreak;
  end;

  { TStringList}
  TStringItem = record
     FString: string;
     FObject: TObject;
   end;

  TStringItemArray = Array of TStringItem;

  TStringList = class;
  TStringListSortCompare = function(List: TStringList; Index1, Index2: Integer): Integer;

  TStringsSortStyle = (sslNone,sslUser,sslAuto);
  TStringsSortStyles = Set of TStringsSortStyle;

  TStringList = class(TStrings)
  private
    FList: TStringItemArray;
    FCount: Integer;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FDuplicates: TDuplicates;
    FCaseSensitive : Boolean;
    FForceSort : Boolean;
    FOwnsObjects : Boolean;
    FSortStyle: TStringsSortStyle;
    procedure ExchangeItemsInt(Index1, Index2: Integer);
    function GetSorted: Boolean;
    procedure Grow;
    procedure InternalClear(FromIndex : Integer = 0; ClearOnly : Boolean = False);
    procedure QuickSort(L, R: Integer; CompareFn: TStringListSortCompare);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(b : boolean);
    procedure SetSortStyle(AValue: TStringsSortStyle);
  protected
    Procedure CheckIndex(AIndex : Integer);
    procedure ExchangeItems(Index1, Index2: Integer); virtual;
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    procedure InsertItem(Index: Integer; const S: string); virtual;
    procedure InsertItem(Index: Integer; const S: string; O: TObject); virtual;
    Function DoCompareText(const s1,s2 : string) : PtrInt; override;
    function CompareStrings(const s1,s2 : string) : Integer; virtual;
  public
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: string; Out Index: Integer): Boolean; virtual;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Sort; virtual;
    procedure CustomSort(CompareFn: TStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read GetSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects : boolean read FOwnsObjects write FOwnsObjects;
    Property SortStyle : TStringsSortStyle Read FSortStyle Write SetSortStyle;
  end;

  TCollection = class;

  { TCollectionItem }

  TCollectionItem = class(TPersistent)
  private
    FCollection: TCollection;
    FID: Integer;
    FUpdateCount: Integer;
    function GetIndex: Integer;
  protected
    procedure SetCollection(Value: TCollection);virtual;
    procedure Changed(AllItems: Boolean);
    function GetOwner: TPersistent; override;
    function GetDisplayName: string; virtual;
    procedure SetIndex(Value: Integer); virtual;
    procedure SetDisplayName(const Value: string); virtual;
    property UpdateCount: Integer read FUpdateCount;
  public
    constructor Create(ACollection: TCollection); virtual; reintroduce;
    destructor Destroy; override;
    function GetNamePath: string; override;
    property Collection: TCollection read FCollection write SetCollection;
    property ID: Integer read FID;
    property Index: Integer read GetIndex write SetIndex;
    property DisplayName: string read GetDisplayName write SetDisplayName;
  end;

  TCollectionEnumerator = class
  private
    FCollection: TCollection;
    FPosition: Integer;
  public
    constructor Create(ACollection: TCollection); reintroduce;
    function GetCurrent: TCollectionItem;
    function MoveNext: Boolean;
    property Current: TCollectionItem read GetCurrent;
  end;

  TCollectionItemClass = class of TCollectionItem;
  TCollectionNotification = (cnAdded, cnExtracting, cnDeleting);
  TCollectionSortCompare = function (Item1, Item2: TCollectionItem): Integer;

  TCollection = class(TPersistent)
  private
    FItemClass: TCollectionItemClass;
    FItems: TFpList;
    FUpdateCount: Integer;
    FNextID: Integer;
    FPropName: string;
    function GetCount: Integer;
    function GetPropName: string;
    procedure InsertItem(Item: TCollectionItem);
    procedure RemoveItem(Item: TCollectionItem);
    procedure DoClear;
  protected
    { Design-time editor support }
    function GetAttrCount: Integer; virtual;
    function GetAttr(Index: Integer): string; virtual;
    function GetItemAttr(Index, ItemIndex: Integer): string; virtual;
    procedure Changed;
    function GetItem(Index: Integer): TCollectionItem;
    procedure SetItem(Index: Integer; Value: TCollectionItem);
    procedure SetItemName(Item: TCollectionItem); virtual;
    procedure SetPropName; virtual;
    procedure Update(Item: TCollectionItem); virtual;
    procedure Notify(Item: TCollectionItem;Action: TCollectionNotification); virtual;
    property PropName: string read GetPropName write FPropName;
    property UpdateCount: Integer read FUpdateCount;
  public
    constructor Create(AItemClass: TCollectionItemClass); reintroduce;
    destructor Destroy; override;
    function Owner: TPersistent;
    function Add: TCollectionItem;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate; virtual;
    procedure Clear;
    procedure EndUpdate; virtual;
    procedure Delete(Index: Integer);
    function GetEnumerator: TCollectionEnumerator;
    function GetNamePath: string; override;
    function Insert(Index: Integer): TCollectionItem;
    function FindItemID(ID: Integer): TCollectionItem;
    procedure Exchange(Const Index1, index2: integer);
    procedure Sort(Const Compare : TCollectionSortCompare);
    property Count: Integer read GetCount;
    property ItemClass: TCollectionItemClass read FItemClass;
    property Items[Index: Integer]: TCollectionItem read GetItem write SetItem;
  end;

  TOwnedCollection = class(TCollection)
  private
    FOwner: TPersistent;
  protected
    Function GetOwner: TPersistent; override;
  public
    Constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass); reintroduce;
  end;

  TComponent = Class;

  TOperation = (opInsert, opRemove);

  TComponentStateItem = ( csLoading, csReading, csWriting,  csDestroying,
    csDesigning, csAncestor, csUpdating, csFixups, csFreeNotification,
    csInline, csDesignInstance);
  TComponentState = set of TComponentStateItem;
  TComponentStyleItem = (csInheritable, csCheckPropAvail, csSubComponent, csTransient);
  TComponentStyle = set of TComponentStyleItem;

  TGetChildProc = procedure (Child: TComponent) of object;

  TComponentName = string;

  { TComponentEnumerator }

  TComponentEnumerator = class
  private
    FComponent: TComponent;
    FPosition: Integer;
  public
    constructor Create(AComponent: TComponent); reintroduce;
    function GetCurrent: TComponent;
    function MoveNext: Boolean;
    property Current: TComponent read GetCurrent;
  end;

  TComponent = class(TPersistent, IInterface)
  private
    FOwner: TComponent;
    FName: TComponentName;
    FTag: Ptrint;
    FComponents: TFpList;
    FFreeNotifies: TFpList;
    FDesignInfo: Longint;
    FComponentState: TComponentState;
    function GetComponent(AIndex: Integer): TComponent;
    function GetComponentCount: Integer;
    function GetComponentIndex: Integer;
    procedure Insert(AComponent: TComponent);
    procedure Remove(AComponent: TComponent);
    procedure RemoveNotification(AComponent: TComponent);
    procedure SetComponentIndex(Value: Integer);
  protected
    FComponentStyle: TComponentStyle;
    procedure ChangeName(const NewName: TComponentName);
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); virtual;
    function GetChildOwner: TComponent; virtual;
    function GetChildParent: TComponent; virtual;
    function GetOwner: TPersistent; override;
    procedure Loaded; virtual;
    procedure Loading; virtual;
    procedure Notification(AComponent: TComponent;  Operation: TOperation); virtual;
    procedure PaletteCreated; virtual;
    procedure SetAncestor(Value: Boolean);
    procedure SetDesigning(Value: Boolean; SetChildren : Boolean = True);
    procedure SetDesignInstance(Value: Boolean);
    procedure SetInline(Value: Boolean);
    procedure SetName(const NewName: TComponentName); virtual;
    procedure SetChildOrder(Child: TComponent; Order: Integer); virtual;
    procedure SetParentComponent(Value: TComponent); virtual;
    procedure Updating; virtual;
    procedure Updated; virtual;
    procedure ValidateRename(AComponent: TComponent;  const CurName, NewName: string); virtual;
    procedure ValidateContainer(AComponent: TComponent); virtual;
    procedure ValidateInsert(AComponent: TComponent); virtual;
  protected
    function _AddRef: Integer;
    function _Release: Integer;
  public
    constructor Create(AOwner: TComponent); virtual; reintroduce;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure DestroyComponents;
    procedure Destroying;
    function QueryInterface(const IID: TGUID; out Obj): integer; virtual;
//    function ExecuteAction(Action: TBasicAction): Boolean; virtual;
    function FindComponent(const AName: string): TComponent;
    procedure FreeNotification(AComponent: TComponent);
    procedure RemoveFreeNotification(AComponent: TComponent);
    function GetNamePath: string; override;
    function GetParentComponent: TComponent; virtual;
    function HasParent: Boolean; virtual;
    procedure InsertComponent(AComponent: TComponent);
    procedure RemoveComponent(AComponent: TComponent);
    procedure SetSubComponent(ASubComponent: Boolean);
    function GetEnumerator: TComponentEnumerator;
//    function UpdateAction(Action: TBasicAction): Boolean; dynamic;
    property Components[Index: Integer]: TComponent read GetComponent;
    property ComponentCount: Integer read GetComponentCount;
    property ComponentIndex: Integer read GetComponentIndex write SetComponentIndex;
    property ComponentState: TComponentState read FComponentState;
    property ComponentStyle: TComponentStyle read FComponentStyle;
    property DesignInfo: Longint read FDesignInfo write FDesignInfo;
    property Owner: TComponent read FOwner;
  published
    property Name: TComponentName read FName write SetName stored False;
    property Tag: PtrInt read FTag write FTag {default 0};
  end;
  TComponentClass = Class of TComponent;

Procedure RegisterClass(AClass : TPersistentClass);
Function GetClass(AClassName : string) : TPersistentClass;

implementation

uses JS;

{ TInterfacedPersistent }

function TInterfacedPersistent._AddRef: Integer;
begin
  Result:=-1;
  if Assigned(FOwnerInterface) then
    Result:=FOwnerInterface._AddRef;
end;

function TInterfacedPersistent._Release: Integer;

begin
  Result:=-1;
  if Assigned(FOwnerInterface) then
    Result:=FOwnerInterface._Release;
end;

function TInterfacedPersistent.QueryInterface(const IID: TGUID; out Obj): integer;
begin
  Result:=E_NOINTERFACE;
  if GetInterface(IID, Obj)  then
    Result:=0;
end;

procedure TInterfacedPersistent.AfterConstruction;
begin
  inherited AfterConstruction;
  if (GetOwner<>nil) then
    GetOwner.GetInterface(IInterface, FOwnerInterface);
end;

{ TComponentEnumerator }

constructor TComponentEnumerator.Create(AComponent: TComponent);
begin
  inherited Create;
  FComponent := AComponent;
  FPosition := -1;
end;

function TComponentEnumerator.GetCurrent: TComponent;
begin
  Result := FComponent.Components[FPosition];
end;

function TComponentEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FComponent.ComponentCount;
end;

{ TListEnumerator }

constructor TListEnumerator.Create(AList: TList);
begin
  inherited Create;
  FList := AList;
  FPosition := -1;
end;

function TListEnumerator.GetCurrent: JSValue;
begin
  Result := FList[FPosition];
end;

function TListEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TFPListEnumerator }

constructor TFPListEnumerator.Create(AList: TFPList);
begin
  inherited Create;
  FList := AList;
  FPosition := -1;
end;

function TFPListEnumerator.GetCurrent: JSValue;
begin
  Result := FList[FPosition];
end;

function TFPListEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TFPList }

procedure TFPList.CopyMove(aList: TFPList);
var r : integer;
begin
  Clear;
  for r := 0 to aList.count-1 do
    Add(aList[r]);
end;

procedure TFPList.MergeMove(aList: TFPList);
var r : integer;
begin
  For r := 0 to aList.count-1 do
    if IndexOf(aList[r]) < 0 then
      Add(aList[r]);
end;

procedure TFPList.DoCopy(ListA, ListB: TFPList);
begin
  if Assigned(ListB) then
    CopyMove(ListB)
  else
    CopyMove(ListA);
end;

procedure TFPList.DoSrcUnique(ListA, ListB: TFPList);
var r : integer;
begin
  if Assigned(ListB) then
    begin
    Clear;
    for r := 0 to ListA.Count-1 do
      if ListB.IndexOf(ListA[r]) < 0 then
        Add(ListA[r]);
    end
  else
    begin
    for r := Count-1 downto 0 do
      if ListA.IndexOf(Self[r]) >= 0 then
        Delete(r);
    end;
end;

procedure TFPList.DoAnd(ListA, ListB: TFPList);
var r : integer;
begin
  if Assigned(ListB) then
    begin
    Clear;
    for r := 0 to ListA.count-1 do
      if ListB.IndexOf(ListA[r]) >= 0 then
        Add(ListA[r]);
    end
  else
    begin
    for r := Count-1 downto 0 do
      if ListA.IndexOf(Self[r]) < 0 then
        Delete(r);
    end;
end;

procedure TFPList.DoDestUnique(ListA, ListB: TFPList);

  procedure MoveElements(Src, Dest: TFPList);
  var r : integer;
  begin
    Clear;
    for r := 0 to Src.count-1 do
      if Dest.IndexOf(Src[r]) < 0 then
        self.Add(Src[r]);
  end;

var Dest : TFPList;
begin
  if Assigned(ListB) then
    MoveElements(ListB, ListA)
  else
    Dest := TFPList.Create;
    try
      Dest.CopyMove(Self);
      MoveElements(ListA, Dest)
    finally
      Dest.Destroy;
    end;
end;

procedure TFPList.DoOr(ListA, ListB: TFPList);
begin
  if Assigned(ListB) then
    begin
    CopyMove(ListA);
    MergeMove(ListB);
    end
  else
    MergeMove(ListA);
end;

procedure TFPList.DoXOr(ListA, ListB: TFPList);
var
  r : integer;
  l : TFPList;
begin
  if Assigned(ListB) then
    begin
    Clear;
    for r := 0 to ListA.Count-1 do
      if ListB.IndexOf(ListA[r]) < 0 then
        Add(ListA[r]);
    for r := 0 to ListB.Count-1 do
      if ListA.IndexOf(ListB[r]) < 0 then
        Add(ListB[r]);
    end
  else
    begin
    l := TFPList.Create;
    try
      l.CopyMove(Self);
      for r := Count-1 downto 0 do
        if listA.IndexOf(Self[r]) >= 0 then
          Delete(r);
      for r := 0 to ListA.Count-1 do
        if l.IndexOf(ListA[r]) < 0 then
          Add(ListA[r]);
    finally
      l.Destroy;
    end;
    end;
end;

function TFPList.Get(Index: Integer): JSValue;
begin
  If (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  Result:=FList[Index];
end;

procedure TFPList.Put(Index: Integer; Item: JSValue);
begin
  if (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  FList[Index] := Item;
end;

procedure TFPList.SetCapacity(NewCapacity: Integer);
begin
  If (NewCapacity < FCount) then
     Error (SListCapacityError, str(NewCapacity));
  if NewCapacity = FCapacity then
    exit;
  SetLength(FList,NewCapacity);
  FCapacity := NewCapacity;
end;

procedure TFPList.SetCount(NewCount: Integer);
begin
  if (NewCount < 0) then
    Error(SListCountError, str(NewCount));
  If NewCount > FCount then
    begin
    If NewCount > FCapacity then
      SetCapacity(NewCount);
    end;
  FCount := NewCount;
end;

procedure TFPList.RaiseIndexError(Index: Integer);
begin
  Error(SListIndexError, str(Index));
end;

destructor TFPList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TFPList.AddList(AList: TFPList);

Var
  I : Integer;

begin
  If (Capacity<Count+AList.Count) then
    Capacity:=Count+AList.Count;
  For I:=0 to AList.Count-1 do
    Add(AList[i]);
end;

function TFPList.Add(Item: JSValue): Integer;
begin
  if FCount = FCapacity then
    Expand;
  FList[FCount] := Item;
  Result := FCount;
  Inc(FCount);
end;

procedure TFPList.Clear;
begin
  if Assigned(FList) then
  begin
    SetCount(0);
    SetCapacity(0);
  end;
end;

procedure TFPList.Delete(Index: Integer);
begin
  If (Index<0) or (Index>=FCount) then
    Error (SListIndexError, str(Index));
  FCount := FCount-1;
  System.Delete(FList,Index,1);
  Dec(FCapacity);
end;

class procedure TFPList.Error(const Msg: string; const Data: String);
begin
  Raise EListError.CreateFmt(Msg,[Data]);
end;

procedure TFPList.Exchange(Index1, Index2: Integer);
var
  Temp : JSValue;
begin
  If (Index1 >= FCount) or (Index1 < 0) then
    Error(SListIndexError, str(Index1));
  If (Index2 >= FCount) or (Index2 < 0) then
    Error(SListIndexError, str(Index2));
  Temp := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := Temp;
end;

function TFPList.Expand: TFPList;
var
  IncSize : Integer;
begin
  if FCount < FCapacity then exit(self);
  IncSize := 4;
  if FCapacity > 3 then IncSize := IncSize + 4;
  if FCapacity > 8 then IncSize := IncSize+8;
  if FCapacity > 127 then Inc(IncSize, FCapacity shr 2);
  SetCapacity(FCapacity + IncSize);
  Result := Self;
end;

function TFPList.Extract(Item: JSValue): JSValue;
var
  i : Integer;
begin
  i := IndexOf(Item);
  if i >= 0 then
  begin
    Result := Item;
    Delete(i);
  end
  else
    Result := nil;
end;

function TFPList.First: JSValue;
begin
  If FCount = 0 then
    Result := Nil
  else
    Result := Items[0];
end;

function TFPList.GetEnumerator: TFPListEnumerator;
begin
  Result:=TFPListEnumerator.Create(Self);
end;

function TFPList.IndexOf(Item: JSValue): Integer;

Var
  C : Integer;

begin
  Result:=0;
  C:=Count;
  while (Result<C) and (FList[Result]<>Item) do
    Inc(Result);
  If Result>=C then
    Result:=-1;

end;

function TFPList.IndexOfItem(Item: JSValue; Direction: TDirection): Integer;

begin
  if Direction=fromBeginning then
    Result:=IndexOf(Item)
  else
    begin
    Result:=Count-1;
    while (Result >=0) and (Flist[Result]<>Item) do
      Result:=Result - 1;
    end;
end;


procedure TFPList.Insert(Index: Integer; Item: JSValue);
begin
  if (Index < 0) or (Index > FCount )then
    Error(SlistIndexError, str(Index));
  TJSArray(FList).splice(Index, 0, Item);
  inc(FCapacity);
  inc(FCount);
end;

function TFPList.Last: JSValue;
begin
  If FCount = 0 then
    Result := nil
  else
    Result := Items[FCount - 1];
end;

procedure TFPList.Move(CurIndex, NewIndex: Integer);
var
  Temp: JSValue;
begin
  if (CurIndex < 0) or (CurIndex > Count - 1) then
    Error(SListIndexError, str(CurIndex));
  if (NewIndex < 0) or (NewIndex > Count -1) then
    Error(SlistIndexError, str(NewIndex));
  if CurIndex=NewIndex then exit;
  Temp:=FList[CurIndex];
  // ToDo: use TJSArray.copyWithin if available
  TJSArray(FList).splice(CurIndex,1);
  TJSArray(FList).splice(NewIndex,0,Temp);
end;

procedure TFPList.Assign(ListA: TFPList; AOperator: TListAssignOp;
  ListB: TFPList);
begin
  case AOperator of
    laCopy      : DoCopy (ListA, ListB);      // replace dest with src
    laSrcUnique : DoSrcUnique (ListA, ListB); // replace dest with src that are not in dest
    laAnd       : DoAnd (ListA, ListB);       // remove from dest that are not in src
    laDestUnique: DoDestUnique (ListA, ListB);// remove from dest that are in src
    laOr        : DoOr (ListA, ListB);        // add to dest from src and not in dest
    laXOr       : DoXOr (ListA, ListB);       // add to dest from src and not in dest, remove from dest that are in src
  end;
end;

function TFPList.Remove(Item: JSValue): Integer;
begin
  Result := IndexOf(Item);
  If Result <> -1 then
    Delete(Result);
end;

procedure TFPList.Pack;
var
  Dst, i: Integer;
  V: JSValue;
begin
  Dst:=0;
  for i:=0 to Count-1 do
    begin
    V:=FList[i];
    if not Assigned(V) then continue;
    FList[Dst]:=V;
    inc(Dst);
    end;
end;

// Needed by Sort method.

Procedure QuickSort(aList: TJSValueDynArray; L, R : Longint;
                    const Compare: TListSortCompare);
var
  I, J : Longint;
  P, Q : JSValue;
begin
  repeat
    I := L;
    J := R;
    P := aList[ (L + R) div 2 ];
    repeat
      while Compare(P, aList[i]) > 0 do
        I := I + 1;
      while Compare(P, aList[J]) < 0 do
        J := J - 1;
      If I <= J then
      begin
        Q := aList[I];
        aList[I] := aList[J];
        aList[J] := Q;
        I := I + 1;
        J := J - 1;
      end;
    until I > J;
    // sort the smaller range recursively
    // sort the bigger range via the loop
    // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
    if J - L < R - I then
    begin
      if L < J then
        QuickSort(aList, L, J, Compare);
      L := I;
    end
    else
    begin
      if I < R then
        QuickSort(aList, I, R, Compare);
      R := J;
    end;
  until L >= R;
end;

procedure TFPList.Sort(const Compare: TListSortCompare);
begin
  if Not Assigned(FList) or (FCount < 2) then exit;
  QuickSort(Flist, 0, FCount-1, Compare);
end;

procedure TFPList.ForEachCall(const proc2call: TListCallback; const arg: JSValue
  );
var
  i : integer;
  v : JSValue;
begin
  For I:=0 To Count-1 Do
    begin
      v:=FList[i];
      if Assigned(v) then
        proc2call(v,arg);
    end;
end;

procedure TFPList.ForEachCall(const proc2call: TListStaticCallback;
  const arg: JSValue);
var
  i : integer;
  v : JSValue;
begin
  For I:=0 To Count-1 Do
    begin
      v:=FList[i];
      if Assigned(v) then
        proc2call(v,arg);
    end;
end;

{ TList }

procedure TList.CopyMove(aList: TList);
var
  r : integer;
begin
  Clear;
  for r := 0 to aList.count-1 do
    Add(aList[r]);
end;

procedure TList.MergeMove(aList: TList);
var r : integer;
begin
  For r := 0 to aList.count-1 do
    if IndexOf(aList[r]) < 0 then
      Add(aList[r]);
end;

procedure TList.DoCopy(ListA, ListB: TList);
begin
  if Assigned(ListB) then
    CopyMove(ListB)
  else
    CopyMove(ListA);
end;

procedure TList.DoSrcUnique(ListA, ListB: TList);
var r : integer;
begin
  if Assigned(ListB) then
    begin
    Clear;
    for r := 0 to ListA.Count-1 do
      if ListB.IndexOf(ListA[r]) < 0 then
        Add(ListA[r]);
    end
  else
    begin
    for r := Count-1 downto 0 do
      if ListA.IndexOf(Self[r]) >= 0 then
        Delete(r);
    end;
end;

procedure TList.DoAnd(ListA, ListB: TList);
var r : integer;
begin
  if Assigned(ListB) then
    begin
    Clear;
    for r := 0 to ListA.Count-1 do
      if ListB.IndexOf(ListA[r]) >= 0 then
        Add(ListA[r]);
    end
  else
    begin
    for r := Count-1 downto 0 do
      if ListA.IndexOf(Self[r]) < 0 then
        Delete(r);
    end;
end;

procedure TList.DoDestUnique(ListA, ListB: TList);

  procedure MoveElements(Src, Dest : TList);
  var r : integer;
  begin
    Clear;
    for r := 0 to Src.Count-1 do
      if Dest.IndexOf(Src[r]) < 0 then
        Add(Src[r]);
  end;

var Dest : TList;
begin
  if Assigned(ListB) then
    MoveElements(ListB, ListA)
  else
    try
      Dest := TList.Create;
      Dest.CopyMove(Self);
      MoveElements(ListA, Dest)
    finally
      Dest.Destroy;
    end;
end;

procedure TList.DoOr(ListA, ListB: TList);
begin
  if Assigned(ListB) then
    begin
    CopyMove(ListA);
    MergeMove(ListB);
    end
  else
    MergeMove(ListA);
end;

procedure TList.DoXOr(ListA, ListB: TList);
var
  r : integer;
  l : TList;
begin
  if Assigned(ListB) then
    begin
    Clear;
    for r := 0 to ListA.Count-1 do
      if ListB.IndexOf(ListA[r]) < 0 then
        Add(ListA[r]);
    for r := 0 to ListB.Count-1 do
      if ListA.IndexOf(ListB[r]) < 0 then
        Add(ListB[r]);
    end
  else
    try
      l := TList.Create;
      l.CopyMove (Self);
      for r := Count-1 downto 0 do
        if listA.IndexOf(Self[r]) >= 0 then
          Delete(r);
      for r := 0 to ListA.Count-1 do
        if l.IndexOf(ListA[r]) < 0 then
          Add(ListA[r]);
    finally
      l.Destroy;
    end;
end;

function TList.Get(Index: Integer): JSValue;
begin
  Result := FList.Get(Index);
end;

procedure TList.Put(Index: Integer; Item: JSValue);
var V : JSValue;
begin
  V := Get(Index);
  FList.Put(Index, Item);
  if Assigned(V) then
    Notify(V, lnDeleted);
  if Assigned(Item) then
    Notify(Item, lnAdded);
end;

procedure TList.Notify(aValue: JSValue; Action: TListNotification);
begin
  if Assigned(aValue) then ;
  if Action=lnExtracted then ;
end;

procedure TList.SetCapacity(NewCapacity: Integer);
begin
  FList.SetCapacity(NewCapacity);
end;

function TList.GetCapacity: integer;
begin
  Result := FList.Capacity;
end;

procedure TList.SetCount(NewCount: Integer);
begin
  if NewCount < FList.Count then
    while FList.Count > NewCount do
      Delete(FList.Count - 1)
  else
    FList.SetCount(NewCount);
end;

function TList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TList.GetList: TJSValueDynArray;
begin
  Result := FList.List;
end;

constructor TList.Create;
begin
  inherited Create;
  FList := TFPList.Create;
end;

destructor TList.Destroy;
begin
  if Assigned(FList) then
    Clear;
  FreeAndNil(FList);
end;

procedure TList.AddList(AList: TList);
var
  I: Integer;
begin
  { this only does FList.AddList(AList.FList), avoiding notifications }
  FList.AddList(AList.FList);

  { make lnAdded notifications }
  for I := 0 to AList.Count - 1 do
    if Assigned(AList[I]) then
      Notify(AList[I], lnAdded);
end;

function TList.Add(Item: JSValue): Integer;
begin
  Result := FList.Add(Item);
  if Assigned(Item) then
    Notify(Item, lnAdded);
end;

procedure TList.Clear;
begin
  While (FList.Count>0) do
    Delete(Count-1);
end;

procedure TList.Delete(Index: Integer);

var V : JSValue;

begin
  V:=FList.Get(Index);
  FList.Delete(Index);
  if assigned(V) then
    Notify(V, lnDeleted);
end;

class procedure TList.Error(const Msg: string; Data: String);
begin
  Raise EListError.CreateFmt(Msg,[Data]);
end;

procedure TList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

function TList.Expand: TList;
begin
  FList.Expand;
  Result:=Self;
end;

function TList.Extract(Item: JSValue): JSValue;
var c : integer;
begin
  c := FList.Count;
  Result := FList.Extract(Item);
  if c <> FList.Count then
    Notify (Result, lnExtracted);
end;

function TList.First: JSValue;
begin
  Result := FList.First;
end;

function TList.GetEnumerator: TListEnumerator;
begin
  Result:=TListEnumerator.Create(Self);
end;

function TList.IndexOf(Item: JSValue): Integer;
begin
  Result := FList.IndexOf(Item);
end;

procedure TList.Insert(Index: Integer; Item: JSValue);
begin
  FList.Insert(Index, Item);
  if Assigned(Item) then
    Notify(Item,lnAdded);
end;

function TList.Last: JSValue;
begin
  Result := FList.Last;
end;

procedure TList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

procedure TList.Assign(ListA: TList; AOperator: TListAssignOp; ListB: TList);
begin
  case AOperator of
    laCopy      : DoCopy (ListA, ListB);      // replace dest with src
    laSrcUnique : DoSrcUnique (ListA, ListB); // replace dest with src that are not in dest
    laAnd       : DoAnd (ListA, ListB);       // remove from dest that are not in src
    laDestUnique: DoDestUnique (ListA, ListB);// remove from dest that are in src
    laOr        : DoOr (ListA, ListB);        // add to dest from src and not in dest
    laXOr       : DoXOr (ListA, ListB);       // add to dest from src and not in dest, remove from dest that are in src
  end;
end;

function TList.Remove(Item: JSValue): Integer;
begin
  Result := IndexOf(Item);
  if Result <> -1 then
    Self.Delete(Result);
end;

procedure TList.Pack;
begin
  FList.Pack;
end;

procedure TList.Sort(const Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

{ TPersistent }

procedure TPersistent.AssignError(Source: TPersistent);
var
  SourceName: String;
begin
  if Source<>Nil then
    SourceName:=Source.ClassName
  else
    SourceName:='Nil';
  raise EConvertError.Create('Cannot assign a '+SourceName+' to a '+ClassName+'.');
end;

procedure TPersistent.AssignTo(Dest: TPersistent);
begin
  Dest.AssignError(Self);
end;

function TPersistent.GetOwner: TPersistent;
begin
  Result:=nil;
end;

procedure TPersistent.Assign(Source: TPersistent);
begin
  If Source<>Nil then
    Source.AssignTo(Self)
  else
    AssignError(Nil);
end;

function TPersistent.GetNamePath: string;
var
  OwnerName: String;
  TheOwner: TPersistent;
begin
  Result:=ClassName;
  TheOwner:=GetOwner;
  if TheOwner<>Nil then
  begin
    OwnerName:=TheOwner.GetNamePath;
    if OwnerName<>'' then Result:=OwnerName+'.'+Result;
  end;
end;

{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************}
{*                        TStringsEnumerator                                *}
{****************************************************************************}

constructor TStringsEnumerator.Create(AStrings: TStrings);
begin
  inherited Create;
  FStrings := AStrings;
  FPosition := -1;
end;

function TStringsEnumerator.GetCurrent: String;
begin
  Result := FStrings[FPosition];
end;

function TStringsEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FStrings.Count;
end;

{****************************************************************************}
{*                             TStrings                                     *}
{****************************************************************************}

// Function to quote text. Should move maybe to sysutils !!
// Also, it is not clear at this point what exactly should be done.

{ //!! is used to mark unsupported things. }

{
  For compatibility we can't add a Constructor to TSTrings to initialize
  the special characters. Therefore we add a routine which is called whenever
  the special chars are needed.
}

Procedure Tstrings.CheckSpecialChars;

begin
  If Not FSpecialCharsInited then
    begin
    FQuoteChar:='"';
    FDelimiter:=',';
    FNameValueSeparator:='=';
    FLBS:=DefaultTextLineBreakStyle;
    FSpecialCharsInited:=true;
    FLineBreak:=sLineBreak;
    end;
end;

Function TStrings.GetSkipLastLineBreak : Boolean;

begin
  CheckSpecialChars;
  Result:=FSkipLastLineBreak;
end;

procedure TStrings.SetSkipLastLineBreak(const AValue : Boolean);

begin
  CheckSpecialChars;
  FSkipLastLineBreak:=AValue;
end;

Function TStrings.GetLBS : TTextLineBreakStyle;
begin
  CheckSpecialChars;
  Result:=FLBS;
end;

Procedure TStrings.SetLBS (AValue : TTextLineBreakStyle);
begin
  CheckSpecialChars;
  FLBS:=AValue;
end;

procedure TStrings.SetDelimiter(c:Char);
begin
  CheckSpecialChars;
  FDelimiter:=c;
end;

Function TStrings.GetDelimiter : Char;
begin
  CheckSpecialChars;
  Result:=FDelimiter;
end;

procedure TStrings.SetLineBreak(Const S : String);
begin
  CheckSpecialChars;
  FLineBreak:=S;
end;

Function TStrings.GetLineBreak : String;
begin
  CheckSpecialChars;
  Result:=FLineBreak;
end;


procedure TStrings.SetQuoteChar(c:Char);
begin
  CheckSpecialChars;
  FQuoteChar:=c;
end;

Function TStrings.GetQuoteChar :Char;
begin
  CheckSpecialChars;
  Result:=FQuoteChar;
end;

procedure TStrings.SetNameValueSeparator(c:Char);
begin
  CheckSpecialChars;
  FNameValueSeparator:=c;
end;

Function TStrings.GetNameValueSeparator :Char;
begin
  CheckSpecialChars;
  Result:=FNameValueSeparator;
end;


function TStrings.GetCommaText: string;

Var
  C1,C2 : Char;
  FSD : Boolean;

begin
  CheckSpecialChars;
  FSD:=StrictDelimiter;
  C1:=Delimiter;
  C2:=QuoteChar;
  Delimiter:=',';
  QuoteChar:='"';
  StrictDelimiter:=False;
  Try
    Result:=GetDelimitedText;
  Finally
    Delimiter:=C1;
    QuoteChar:=C2;
    StrictDelimiter:=FSD;
  end;
end;


Function TStrings.GetDelimitedText: string;

Var
  I: integer;
  RE : string;
  S : String;
  doQuote : Boolean;

begin
  CheckSpecialChars;
  result:='';
  RE:=QuoteChar+'|'+Delimiter;
  if not StrictDelimiter then
    RE:=' |'+RE;
  RE:='/'+RE+'/';
  // Check for break characters and quote if required.
  For i:=0 to count-1 do
    begin
    S:=Strings[i];
    doQuote:=FAlwaysQuote or (TJSString(s).search(RE)=-1);
    if DoQuote then
      Result:=Result+QuoteString(S,QuoteChar)
    else
      Result:=Result+S;
    if I<Count-1 then
      Result:=Result+Delimiter;
    end;
  // Quote empty string:
  If (Length(Result)=0) and (Count=1) then
    Result:=QuoteChar+QuoteChar;
end;

procedure TStrings.GetNameValue(Index : Integer; Out AName,AValue : String);

Var L : longint;

begin
  CheckSpecialChars;
  AValue:=Strings[Index];
  L:=Pos(FNameValueSeparator,AValue);
  If L<>0 then
    begin
    AName:=Copy(AValue,1,L-1);
    //    System.Delete(AValue,1,L);
    AValue:=Copy(AValue,L+1,length(AValue)-L);
    end
  else
    AName:='';
end;

function TStrings.ExtractName(const s:String):String;
var
  L: Longint;
begin
  CheckSpecialChars;
  L:=Pos(FNameValueSeparator,S);
  If L<>0 then
    Result:=Copy(S,1,L-1)
  else
    Result:='';
end;

function TStrings.GetName(Index: Integer): string;

Var
  V : String;

begin
  GetNameValue(Index,Result,V);
end;

Function TStrings.GetValue(const Name: string): string;

Var
  L : longint;
  N : String;

begin
  Result:='';
  L:=IndexOfName(Name);
  If L<>-1 then
    GetNameValue(L,N,Result);
end;

Function TStrings.GetValueFromIndex(Index: Integer): string;

Var
  N : String;

begin
  GetNameValue(Index,N,Result);
end;

Procedure TStrings.SetValueFromIndex(Index: Integer; const Value: string);

begin
  If (Value='') then
    Delete(Index)
  else
    begin
    If (Index<0) then
      Index:=Add('');
    CheckSpecialChars;
    Strings[Index]:=GetName(Index)+FNameValueSeparator+Value;
    end;
end;

Procedure TStrings.SetDelimitedText(const AValue: string);
var i,j:integer;
    aNotFirst:boolean;
begin
 CheckSpecialChars;
 BeginUpdate;

 i:=1;
 j:=1;
 aNotFirst:=false;

 { Paraphrased from Delphi XE2 help:
 Strings must be separated by Delimiter characters or spaces.
 They may be enclosed in QuoteChars.
 QuoteChars in the string must be repeated to distinguish them from the QuoteChars enclosing the string.
 }
 try
  Clear;
  If StrictDelimiter then
    begin
    while i<=length(AValue) do begin
     // skip delimiter
     if aNotFirst and (i<=length(AValue)) and (AValue[i]=FDelimiter) then inc(i);

     // read next string
     if i<=length(AValue) then begin
      if AValue[i]=FQuoteChar then begin
       // next string is quoted
       j:=i+1;
       while (j<=length(AValue)) and
             ( (AValue[j]<>FQuoteChar) or
               ( (j+1<=length(AValue)) and (AValue[j+1]=FQuoteChar) ) ) do begin
        if (j<=length(AValue)) and (AValue[j]=FQuoteChar) then inc(j,2)
                                                          else inc(j);
       end;
       // j is position of closing quote
       Add( StringReplace (Copy(AValue,i+1,j-i-1),
                           FQuoteChar+FQuoteChar,FQuoteChar, [rfReplaceAll]));
       i:=j+1;
      end else begin
       // next string is not quoted; read until delimiter
       j:=i;
       while (j<=length(AValue)) and
             (AValue[j]<>FDelimiter) do inc(j);
       Add( Copy(AValue,i,j-i));
       i:=j;
      end;
     end else begin
      if aNotFirst then Add('');
     end;

     aNotFirst:=true;
    end;
    end
  else
    begin
    while i<=length(AValue) do begin
     // skip delimiter
     if aNotFirst and (i<=length(AValue)) and (AValue[i]=FDelimiter) then inc(i);

     // skip spaces
     while (i<=length(AValue)) and (Ord(AValue[i])<=Ord(' ')) do inc(i);

     // read next string
     if i<=length(AValue) then begin
      if AValue[i]=FQuoteChar then begin
       // next string is quoted
       j:=i+1;
       while (j<=length(AValue)) and
             ( (AValue[j]<>FQuoteChar) or
               ( (j+1<=length(AValue)) and (AValue[j+1]=FQuoteChar) ) ) do begin
        if (j<=length(AValue)) and (AValue[j]=FQuoteChar) then inc(j,2)
                                                          else inc(j);
       end;
       // j is position of closing quote
       Add( StringReplace (Copy(AValue,i+1,j-i-1),
                           FQuoteChar+FQuoteChar,FQuoteChar, [rfReplaceAll]));
       i:=j+1;
      end else begin
       // next string is not quoted; read until control character/space/delimiter
       j:=i;
       while (j<=length(AValue)) and
             (Ord(AValue[j])>Ord(' ')) and
             (AValue[j]<>FDelimiter) do inc(j);
       Add( Copy(AValue,i,j-i));
       i:=j;
      end;
     end else begin
      if aNotFirst then Add('');
     end;

     // skip spaces
     while (i<=length(AValue)) and (Ord(AValue[i])<=Ord(' ')) do inc(i);

     aNotFirst:=true;
    end;
    end;
 finally
   EndUpdate;
 end;
end;

Procedure TStrings.SetCommaText(const Value: string);

Var
  C1,C2 : Char;

begin
  CheckSpecialChars;
  C1:=Delimiter;
  C2:=QuoteChar;
  Delimiter:=',';
  QuoteChar:='"';
  Try
    SetDelimitedText(Value);
  Finally
    Delimiter:=C1;
    QuoteChar:=C2;
  end;
end;

Procedure TStrings.SetValue(const Name, Value: string);

Var L : longint;

begin
  CheckSpecialChars;
  L:=IndexOfName(Name);
  if L=-1 then
   Add (Name+FNameValueSeparator+Value)
  else
   Strings[L]:=Name+FNameValueSeparator+value;
end;


Procedure TStrings.Error(const Msg: string; Data: Integer);
begin
  Raise EStringListError.CreateFmt(Msg,[IntToStr(Data)]);
end;

Function TStrings.GetCapacity: Integer;

begin
  Result:=Count;
end;



Function TStrings.GetObject(Index: Integer): TObject;

begin
  if Index=0 then ;
  Result:=Nil;
end;

Function TStrings.GetTextStr: string;

Var
  I : Longint;
  S,NL : String;

begin
  CheckSpecialChars;
  // Determine needed place
  if FLineBreak<>sLineBreak then
    NL:=FLineBreak
  else
    Case FLBS of
      tlbsLF   : NL:=#10;
      tlbsCRLF : NL:=#13#10;
      tlbsCR   : NL:=#13;
    end;
  Result:='';
  For i:=0 To count-1 do
    begin
    S:=Strings[I];
    Result:=Result+S;
    if (I<Count-1) or Not SkipLastLineBreak then
      Result:=Result+NL;
    end;
end;



Procedure TStrings.Put(Index: Integer; const S: string);

Var Obj : TObject;

begin
  Obj:=Objects[Index];
  Delete(Index);
  InsertObject(Index,S,Obj);
end;



Procedure TStrings.PutObject(Index: Integer; AObject: TObject);

begin
  // Empty.
  if Index=0 then exit;
  if AObject=nil then exit;
end;



Procedure TStrings.SetCapacity(NewCapacity: Integer);

begin
  // Empty.
  if NewCapacity=0 then ;
end;

Function TStrings.GetNextLineBreak (Const Value : String; Out S : String; Var P : Integer) : Boolean;

Var
  PP : Integer;

begin
  S:='';
  Result:=False;
  If ((Length(Value)-P)<0) then
    exit;
  PP:=TJSString(Value).IndexOf(LineBreak,P-1)+1;
  if (PP<1) then
    PP:=Length(Value)+1;
  S:=Copy(Value,P,PP-P);
  P:=PP+length(LineBreak);
  Result:=True;
end;

Procedure TStrings.DoSetTextStr(const Value: string; DoClear : Boolean);

Var
  S : String;
  P : Integer;

begin
  Try
    BeginUpdate;
    if DoClear then
      Clear;
    P:=1;
    While GetNextLineBreak (Value,S,P) do
      Add(S);
  finally
    EndUpdate;
  end;
end;

Procedure TStrings.SetTextStr(const Value: string);

begin
  CheckSpecialChars;
  DoSetTextStr(Value,True);
end;

Procedure TStrings.AddText(const S: string);

begin
  CheckSpecialChars;
  DoSetTextStr(S,False);
end;

Procedure TStrings.SetUpdateState(Updating: Boolean);

begin
  // FPONotifyObservers(Self,ooChange,Nil);
  if Updating then ;
end;



destructor TSTrings.Destroy;

begin
  inherited destroy;
end;


constructor TStrings.Create;
begin
  inherited Create;
  FAlwaysQuote:=False;
end;

Function TStrings.Add(const S: string): Integer;

begin
  Result:=Count;
  Insert (Count,S);
end;

(*
function TStrings.AddFmt(const Fmt : string; const Args : Array of const): Integer;

begin
  Result:=Add(Format(Fmt,Args));
end;
*)

Function TStrings.AddObject(const S: string; AObject: TObject): Integer;

begin
  Result:=Add(S);
  Objects[result]:=AObject;
end;

(*
function TStrings.AddObject(const Fmt: string; Args : Array of const; AObject: TObject): Integer;

begin
  Result:=AddObject(Format(Fmt,Args),AObject);
end;
*)

Procedure TStrings.Append(const S: string);

begin
  Add (S);
end;



Procedure TStrings.AddStrings(TheStrings: TStrings; ClearFirst : Boolean);


begin
  beginupdate;
  try
    if ClearFirst then
      Clear;
    AddStrings(TheStrings);
  finally
    EndUpdate;
  end;
end;

Procedure TStrings.AddStrings(TheStrings: TStrings);

Var Runner : longint;
begin
  For Runner:=0 to TheStrings.Count-1 do
    self.AddObject (Thestrings[Runner],TheStrings.Objects[Runner]);
end;

Procedure TStrings.AddStrings(const TheStrings: array of string);

Var Runner : longint;
begin
  if Count + High(TheStrings)+1 > Capacity then
    Capacity := Count + High(TheStrings)+1;
  For Runner:=Low(TheStrings) to High(TheStrings) do
    self.Add(Thestrings[Runner]);
end;


Procedure TStrings.AddStrings(const TheStrings: array of string; ClearFirst : Boolean);

begin
  beginupdate;
  try
    if ClearFirst then
      Clear;
    AddStrings(TheStrings);
  finally
    EndUpdate;
  end;
end;


function TStrings.AddPair(const AName, AValue: string): TStrings;

begin
  Result:=AddPair(AName,AValue,Nil);
end;

function TStrings.AddPair(const AName, AValue: string; AObject: TObject): TStrings;

begin
  Result := Self;
  AddObject(AName+NameValueSeparator+AValue, AObject);
end;


Procedure TStrings.Assign(Source: TPersistent);

Var
  S : TStrings;

begin
  If Source is TStrings then
    begin
    S:=TStrings(Source);
    BeginUpdate;
    Try
      clear;
      FSpecialCharsInited:=S.FSpecialCharsInited;
      FQuoteChar:=S.FQuoteChar;
      FDelimiter:=S.FDelimiter;
      FNameValueSeparator:=S.FNameValueSeparator;
      FLBS:=S.FLBS;
      FLineBreak:=S.FLineBreak;
      AddStrings(S);
    finally
      EndUpdate;
    end;
    end
  else
    Inherited Assign(Source);
end;



Procedure TStrings.BeginUpdate;

begin
   if FUpdateCount = 0 then SetUpdateState(true);
   inc(FUpdateCount);
end;



Procedure TStrings.EndUpdate;

begin
  If FUpdateCount>0 then
     Dec(FUpdateCount);
  if FUpdateCount=0 then
    SetUpdateState(False);
end;



Function TStrings.Equals(Obj: TObject): Boolean;

begin
  if Obj is TStrings then
    Result := Equals(TStrings(Obj))
  else
    Result := inherited Equals(Obj);
end;



Function TStrings.Equals(TheStrings: TStrings): Boolean;

Var Runner,Nr : Longint;

begin
  Result:=False;
  Nr:=Self.Count;
  if Nr<>TheStrings.Count then exit;
  For Runner:=0 to Nr-1 do
    If Strings[Runner]<>TheStrings[Runner] then exit;
  Result:=True;
end;



Procedure TStrings.Exchange(Index1, Index2: Integer);

Var
  Obj : TObject;
  Str : String;

begin
  beginUpdate;
  Try
    Obj:=Objects[Index1];
    Str:=Strings[Index1];
    Objects[Index1]:=Objects[Index2];
    Strings[Index1]:=Strings[Index2];
    Objects[Index2]:=Obj;
    Strings[Index2]:=Str;
  finally
    EndUpdate;
  end;
end;


function TStrings.GetEnumerator: TStringsEnumerator;
begin
  Result:=TStringsEnumerator.Create(Self);
end;


Function TStrings.DoCompareText(const s1,s2 : string) : PtrInt;
begin
  result:=CompareText(s1,s2);
end;


Function TStrings.IndexOf(const S: string): Integer;
begin
  Result:=0;
  While (Result<Count) and (DoCompareText(Strings[Result],S)<>0) do Result:=Result+1;
  if Result=Count then Result:=-1;
end;


Function TStrings.IndexOfName(const Name: string): Integer;
Var
  len : longint;
  S : String;
begin
  CheckSpecialChars;
  Result:=0;
  while (Result<Count) do
    begin
    S:=Strings[Result];
    len:=pos(FNameValueSeparator,S)-1;
    if (len>=0) and (DoCompareText(Name,Copy(S,1,Len))=0) then
      exit;
    inc(result);
    end;
  result:=-1;
end;


Function TStrings.IndexOfObject(AObject: TObject): Integer;
begin
  Result:=0;
  While (Result<count) and (Objects[Result]<>AObject) do Result:=Result+1;
  If Result=Count then Result:=-1;
end;


Procedure TStrings.InsertObject(Index: Integer; const S: string;
  AObject: TObject);

begin
  Insert (Index,S);
  Objects[Index]:=AObject;
end;

Procedure TStrings.Move(CurIndex, NewIndex: Integer);
Var
  Obj : TObject;
  Str : String;
begin
  BeginUpdate;
  Try
    Obj:=Objects[CurIndex];
    Str:=Strings[CurIndex];
    Objects[CurIndex]:=Nil; // Prevent Delete from freeing.
    Delete(Curindex);
    InsertObject(NewIndex,Str,Obj);
  finally
    EndUpdate;
    end;
end;


{****************************************************************************}
{*                             TStringList                                  *}
{****************************************************************************}


procedure TStringList.ExchangeItemsInt(Index1, Index2: Integer);

Var
  S : String;
  O : TObject;

begin
  S:=Flist[Index1].FString;
  O:=Flist[Index1].FObject;
  Flist[Index1].Fstring:=Flist[Index2].Fstring;
  Flist[Index1].FObject:=Flist[Index2].FObject;
  Flist[Index2].Fstring:=S;
  Flist[Index2].FObject:=O;
end;

function TStringList.GetSorted: Boolean;
begin
  Result:=FSortStyle in [sslUser,sslAuto];
end;


procedure TStringList.ExchangeItems(Index1, Index2: Integer);
begin
  ExchangeItemsInt(Index1, Index2);
end;


procedure TStringList.Grow;

Var
  NC : Integer;

begin
  NC:=Capacity;
  If NC>=256 then
    NC:=NC+(NC Div 4)
  else if NC=0 then
    NC:=4
  else
    NC:=NC*4;
  SetCapacity(NC);
end;

procedure TStringList.InternalClear(FromIndex: Integer; ClearOnly: Boolean);

Var
  I: Integer;

begin
  if FromIndex < FCount then
    begin
      if FOwnsObjects then
        begin
          For I:=FromIndex to FCount-1 do
            begin
              Flist[I].FString:='';
              freeandnil(Flist[i].FObject);
            end;
        end
      else
        begin
          For I:=FromIndex to FCount-1 do
            Flist[I].FString:='';
        end;
      FCount:=FromIndex;
    end;
  if Not ClearOnly then
    SetCapacity(0);
end;


procedure TStringList.QuickSort(L, R: Integer; CompareFn: TStringListSortCompare
  );


var
  Pivot, vL, vR: Integer;

begin
  //if ExchangeItems is override call that, else call (faster) ExchangeItemsInt

  if R - L <= 1 then begin // a little bit of time saver
    if L < R then
      if CompareFn(Self, L, R) > 0 then
        ExchangeItems(L, R);

    Exit;
  end;

  vL := L;
  vR := R;

  Pivot := L + Random(R - L); // they say random is best

  while vL < vR do begin
    while (vL < Pivot) and (CompareFn(Self, vL, Pivot) <= 0) do
      Inc(vL);

    while (vR > Pivot) and (CompareFn(Self, vR, Pivot) > 0) do
      Dec(vR);

    ExchangeItems(vL, vR);

    if Pivot = vL then // swap pivot if we just hit it from one side
      Pivot := vR
    else if Pivot = vR then
      Pivot := vL;
  end;

  if Pivot - 1 >= L then
    QuickSort(L, Pivot - 1, CompareFn);
  if Pivot + 1 <= R then
    QuickSort(Pivot + 1, R, CompareFn);
end;


procedure TStringList.InsertItem(Index: Integer; const S: string);
begin
  InsertItem(Index, S, nil);
end;


procedure TStringList.InsertItem(Index: Integer; const S: string; O: TObject);

Var
  It : TStringItem;
  
begin
  Changing;
  If FCount=Capacity then Grow;
  it.FString:=S;
  it.FObject:=O;
  TJSArray(FList).Splice(Index,0,It);
  Inc(FCount);
  Changed;
end;


procedure TStringList.SetSorted(Value: Boolean);

begin
  If Value then
    SortStyle:=sslAuto
  else
    SortStyle:=sslNone
end;



procedure TStringList.Changed;

begin
  If (FUpdateCount=0) Then
   begin
   If Assigned(FOnChange) then
     FOnchange(Self);
   end;
end;



procedure TStringList.Changing;

begin
  If FUpdateCount=0 then
    if Assigned(FOnChanging) then
      FOnchanging(Self);
end;



function TStringList.Get(Index: Integer): string;

begin
  CheckIndex(Index);
  Result:=Flist[Index].FString;
end;



function TStringList.GetCapacity: Integer;

begin
  Result:=Length(FList);
end;



function TStringList.GetCount: Integer;

begin
  Result:=FCount;
end;



function TStringList.GetObject(Index: Integer): TObject;

begin
  CheckIndex(Index);
  Result:=Flist[Index].FObject;
end;



procedure TStringList.Put(Index: Integer; const S: string);

begin
  If Sorted then
    Error(SSortedListError,0);
  CheckIndex(Index);
  Changing;
  Flist[Index].FString:=S;
  Changed;
end;



procedure TStringList.PutObject(Index: Integer; AObject: TObject);

begin
  CheckIndex(Index);
  Changing;
  Flist[Index].FObject:=AObject;
  Changed;
end;



procedure TStringList.SetCapacity(NewCapacity: Integer);

begin
  If (NewCapacity<0) then
     Error (SListCapacityError,NewCapacity);
  If NewCapacity<>Capacity then
    SetLength(FList,NewCapacity)
end;



procedure TStringList.SetUpdateState(Updating: Boolean);

begin
  If Updating then
    Changing
  else
    Changed
end;



destructor TStringList.Destroy;

begin
  InternalClear;
  Inherited destroy;
end;



function TStringList.Add(const S: string): Integer;

begin
  If Not (SortStyle=sslAuto) then
    Result:=FCount
  else
    If Find (S,Result) then
      Case DUplicates of
        DupIgnore : Exit;
        DupError : Error(SDuplicateString,0)
      end;
   InsertItem (Result,S);
end;

procedure TStringList.Clear;

begin
  if FCount = 0 then Exit;
  Changing;
  InternalClear;
  Changed;
end;

procedure TStringList.Delete(Index: Integer);

begin
  CheckIndex(Index);
  Changing;
  if FOwnsObjects then
    FreeAndNil(Flist[Index].FObject);
  TJSArray(FList).splice(Index,1);  
  FList[Count-1].FString:='';
  Flist[Count-1].FObject:=Nil;
  Dec(FCount);
  Changed;
end;

procedure TStringList.Exchange(Index1, Index2: Integer);

begin
  CheckIndex(Index1);
  CheckIndex(Index2);
  Changing;
  ExchangeItemsInt(Index1,Index2);
  changed;
end;

procedure TStringList.SetCaseSensitive(b : boolean);
begin
  if b=FCaseSensitive then
    Exit;
  FCaseSensitive:=b;
  if FSortStyle=sslAuto then
    begin
    FForceSort:=True;
    try
      Sort;
    finally
      FForceSort:=False;
    end;
    end;
end;

procedure TStringList.SetSortStyle(AValue: TStringsSortStyle);
begin
  if FSortStyle=AValue then Exit;
  if (AValue=sslAuto) then
    Sort;
  FSortStyle:=AValue;
end;

procedure TStringList.CheckIndex(AIndex: Integer);
begin
  If (AIndex<0) or (AIndex>=FCount) then
    Error(SListIndexError,AIndex);
end;


function TStringList.DoCompareText(const s1, s2: string): PtrInt;
begin
  if FCaseSensitive then
    result:=CompareStr(s1,s2)
  else
    result:=CompareText(s1,s2);
end;


function TStringList.CompareStrings(const s1,s2 : string) : Integer;
begin
  Result := DoCompareText(s1, s2);
end;


function TStringList.Find(const S: string; out Index: Integer): Boolean;

var
  L, R, I: Integer;
  CompareRes: PtrInt;
begin
  Result := false;
  Index:=-1;
  if Not Sorted then
    Raise EListError.Create(SErrFindNeedsSortedList);
  // Use binary search.
  L := 0;
  R := Count - 1;
  while (L<=R) do
  begin
    I := L + (R - L) div 2;
    CompareRes := DoCompareText(S, Flist[I].FString);
    if (CompareRes>0) then
      L := I+1
    else begin
      R := I-1;
      if (CompareRes=0) then begin
         Result := true;
         if (Duplicates<>dupAccept) then
            L := I; // forces end of while loop
      end;
    end;
  end;
  Index := L;
end;



function TStringList.IndexOf(const S: string): Integer;

begin
  If Not Sorted then
    Result:=Inherited indexOf(S)
  else
    // faster using binary search...
    If Not Find (S,Result) then
      Result:=-1;
end;



procedure TStringList.Insert(Index: Integer; const S: string);

begin
  If SortStyle=sslAuto then
    Error (SSortedListError,0)
  else
    begin
    If (Index<0) or (Index>FCount) then
      Error(SListIndexError,Index); // Cannot use CheckIndex, because there >= FCount...
    InsertItem (Index,S);
    end;
end;


procedure TStringList.CustomSort(CompareFn: TStringListSortCompare);

begin
  If (FForceSort or (Not (FSortStyle=sslAuto))) and (FCount>1) then
    begin
    Changing;
    QuickSort(0,FCount-1, CompareFn);
    Changed;
    end;
end;

function StringListAnsiCompare(List: TStringList; Index1, Index: Integer): Integer;

begin
  Result := List.DoCompareText(List.FList[Index1].FString,
                               List.FList[Index].FString);
end;

procedure TStringList.Sort;

begin
  CustomSort(@StringListAnsiCompare);
end;

{****************************************************************************}
{*                             TCollectionItem                              *}
{****************************************************************************}


function TCollectionItem.GetIndex: Integer;

begin
  if FCollection<>nil then
    Result:=FCollection.FItems.IndexOf(Self)
  else
    Result:=-1;
end;



procedure TCollectionItem.SetCollection(Value: TCollection);

begin
  IF Value<>FCollection then
    begin
    If FCollection<>Nil then FCollection.RemoveItem(Self);
    if Value<>Nil then Value.InsertItem(Self);
    end;
end;



procedure TCollectionItem.Changed(AllItems: Boolean);

begin
 If (FCollection<>Nil) and (FCollection.UpdateCount=0) then
  begin
  If AllItems then
    FCollection.Update(Nil)
  else
    FCollection.Update(Self);
  end;
end;



function TCollectionItem.GetNamePath: string;

begin
  If FCollection<>Nil then
    Result:=FCollection.GetNamePath+'['+IntToStr(Index)+']'
  else
    Result:=ClassName;
end;


function TCollectionItem.GetOwner: TPersistent;

begin
  Result:=FCollection;
end;



function TCollectionItem.GetDisplayName: string;

begin
  Result:=ClassName;
end;



procedure TCollectionItem.SetIndex(Value: Integer);

Var Temp : Longint;

begin
  Temp:=GetIndex;
  If (Temp>-1) and (Temp<>Value) then
    begin
    FCollection.FItems.Move(Temp,Value);
    Changed(True);
    end;
end;


procedure TCollectionItem.SetDisplayName(const Value: string);

begin
  Changed(False);
  if Value='' then ;
end;



constructor TCollectionItem.Create(ACollection: TCollection);

begin
  Inherited Create;
  SetCollection(ACollection);
end;



destructor TCollectionItem.Destroy;

begin
  SetCollection(Nil);
  Inherited Destroy;
end;

{****************************************************************************}
{*                          TCollectionEnumerator                           *}
{****************************************************************************}

constructor TCollectionEnumerator.Create(ACollection: TCollection);
begin
  inherited Create;
  FCollection := ACollection;
  FPosition := -1;
end;

function TCollectionEnumerator.GetCurrent: TCollectionItem;
begin
  Result := FCollection.Items[FPosition];
end;

function TCollectionEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FCollection.Count;
end;


{****************************************************************************}
{*                             TCollection                                  *}
{****************************************************************************}

function TCollection.Owner: TPersistent;
begin
  result:=getowner;
end;


function TCollection.GetCount: Integer;

begin
  Result:=FItems.Count;
end;


Procedure TCollection.SetPropName;

{
Var
  TheOwner : TPersistent;
  PropList : PPropList;
  I, PropCount : Integer;
}
begin
  FPropName:='';
{
  TheOwner:=GetOwner;
  // TODO: This needs to wait till Mattias finishes typeinfo.
  // It's normally only used in the designer so should not be a problem currently.
  if (TheOwner=Nil) Or (TheOwner.Classinfo=Nil) Then Exit;
  // get information from the owner RTTI
  PropCount:=GetPropList(TheOwner, PropList);
  Try
    For I:=0 To PropCount-1 Do
      If (PropList^[i]^.PropType^.Kind=tkClass) And
         (GetObjectProp(TheOwner, PropList^[i], ClassType)=Self) Then
        Begin
          FPropName:=PropList^[i]^.Name;
          Exit;
        End;
  Finally
    FreeMem(PropList);
  End;
}
end;


function TCollection.GetPropName: string;

{Var
  TheOwner : TPersistent;}

begin
  Result:=FPropNAme;
//  TheOwner:=GetOwner;
//  If (Result<>'') or (TheOwner=Nil) Or (TheOwner.Classinfo=Nil) then exit;
  SetPropName;
  Result:=FPropName;
end;


procedure TCollection.InsertItem(Item: TCollectionItem);
begin
  If Not(Item Is FitemClass) then
    exit;
  FItems.add(Item);
  Item.FCollection:=Self;
  Item.FID:=FNextID;
  inc(FNextID);
  SetItemName(Item);
  Notify(Item,cnAdded);
  Changed;
end;


procedure TCollection.RemoveItem(Item: TCollectionItem);

Var
  I : Integer;

begin
  Notify(Item,cnExtracting);
  I:=FItems.IndexOfItem(Item,fromEnd);
  If (I<>-1) then
    FItems.Delete(I);
  Item.FCollection:=Nil;
  Changed;
end;


function TCollection.GetAttrCount: Integer;
begin
  Result:=0;
end;


function TCollection.GetAttr(Index: Integer): string;
begin
  Result:='';
  if Index=0 then ;
end;


function TCollection.GetItemAttr(Index, ItemIndex: Integer): string;
begin
  Result:=TCollectionItem(FItems.Items[ItemIndex]).DisplayName;
  if Index=0 then ;
end;


function TCollection.GetEnumerator: TCollectionEnumerator;
begin
  Result := TCollectionEnumerator.Create(Self);
end;


function TCollection.GetNamePath: string;
var o : TPersistent;
begin
  o:=getowner;
  if assigned(o) and (propname<>'') then
     result:=o.getnamepath+'.'+propname
   else
     result:=classname;
end;


procedure TCollection.Changed;
begin
  if FUpdateCount=0 then
    Update(Nil);
end;


function TCollection.GetItem(Index: Integer): TCollectionItem;
begin
  Result:=TCollectionItem(FItems.Items[Index]);
end;


procedure TCollection.SetItem(Index: Integer; Value: TCollectionItem);
begin
  TCollectionItem(FItems.items[Index]).Assign(Value);
end;


procedure TCollection.SetItemName(Item: TCollectionItem);
begin
  if Item=nil then ;
end;

procedure TCollection.Update(Item: TCollectionItem);
begin
  if Item=nil then ;
end;


constructor TCollection.Create(AItemClass: TCollectionItemClass);
begin
  inherited create;
  FItemClass:=AItemClass;
  FItems:=TFpList.Create;
end;


destructor TCollection.Destroy;
begin
  FUpdateCount:=1; // Prevent OnChange
  try
    DoClear;
  Finally
    FUpdateCount:=0;
  end;
  if assigned(FItems) then
    FItems.Destroy;
  Inherited Destroy;
end;


function TCollection.Add: TCollectionItem;
begin
  Result:=FItemClass.Create(Self);
end;


procedure TCollection.Assign(Source: TPersistent);
Var I : Longint;
begin
  If Source is TCollection then
    begin
    Clear;
    For I:=0 To TCollection(Source).Count-1 do
     Add.Assign(TCollection(Source).Items[I]);
    exit;
    end
  else
    Inherited Assign(Source);
end;


procedure TCollection.BeginUpdate;
begin
  inc(FUpdateCount);
end;


procedure TCollection.Clear;
begin
  if FItems.Count=0 then
    exit; // Prevent Changed
  BeginUpdate;
  try
    DoClear;
  finally
    EndUpdate;
  end;
end;


procedure TCollection.DoClear;
var
  Item: TCollectionItem;
begin
  While FItems.Count>0 do
    begin
    Item:=TCollectionItem(FItems.Last);
    if Assigned(Item) then
      Item.Destroy;
    end;
end;


procedure TCollection.EndUpdate;
begin
  if FUpdateCount>0 then
    dec(FUpdateCount);
  if FUpdateCount=0 then
    Changed;
end;


function TCollection.FindItemID(ID: Integer): TCollectionItem;
Var
          I : Longint;
begin
  For I:=0 to Fitems.Count-1 do
   begin
     Result:=TCollectionItem(FItems.items[I]);
     If Result.Id=Id then
       exit;
   end;
  Result:=Nil;
end;


procedure TCollection.Delete(Index: Integer);
Var
  Item : TCollectionItem;
begin
  Item:=TCollectionItem(FItems[Index]);
  Notify(Item,cnDeleting);
  If assigned(Item) then
    Item.Destroy;
end;


function TCollection.Insert(Index: Integer): TCollectionItem;
begin
  Result:=Add;
  Result.Index:=Index;
end;


procedure TCollection.Notify(Item: TCollectionItem;Action: TCollectionNotification);
begin
  if Item=nil then ;
  if Action=cnAdded then ;
end;

procedure TCollection.Sort(Const Compare : TCollectionSortCompare);

begin
  BeginUpdate;
  try
    FItems.Sort(TListSortCompare(Compare));
  Finally
    EndUpdate;
  end;
end;

procedure TCollection.Exchange(Const Index1, index2: integer);

begin
  FItems.Exchange(Index1,Index2);
end;


{****************************************************************************}
{*                             TOwnedCollection                             *}
{****************************************************************************}



Constructor TOwnedCollection.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);

Begin
  FOwner := AOwner;
  inherited Create(AItemClass);
end;

Function TOwnedCollection.GetOwner: TPersistent;

begin
  Result:=FOwner;
end;

{****************************************************************************}
{*                             TComponent                                   *}
{****************************************************************************}


Function  TComponent.GetComponent(AIndex: Integer): TComponent;

begin
  If not assigned(FComponents) then
    Result:=Nil
  else
    Result:=TComponent(FComponents.Items[Aindex]);
end;

Function  TComponent.GetComponentCount: Integer;

begin
  If not assigned(FComponents) then
    result:=0
  else
    Result:=FComponents.Count;
end;


Function  TComponent.GetComponentIndex: Integer;

begin
  If Assigned(FOwner) and Assigned(FOwner.FComponents) then
    Result:=FOWner.FComponents.IndexOf(Self)
  else
    Result:=-1;
end;


Procedure TComponent.Insert(AComponent: TComponent);

begin
  If not assigned(FComponents) then
    FComponents:=TFpList.Create;
  FComponents.Add(AComponent);
  AComponent.FOwner:=Self;
end;


Procedure TComponent.Remove(AComponent: TComponent);

begin
  AComponent.FOwner:=Nil;
  If assigned(FCOmponents) then
    begin
    FComponents.Remove(AComponent);
    IF FComponents.Count=0 then
      begin
      FComponents.Destroy;
      FComponents:=Nil;
      end;
    end;
end;


Procedure TComponent.RemoveNotification(AComponent: TComponent);

begin
  if FFreeNotifies<>nil then
    begin
    FFreeNotifies.Remove(AComponent);
    if FFreeNotifies.Count=0 then
      begin
      FFreeNotifies.Destroy;
      FFreeNotifies:=nil;
      Exclude(FComponentState,csFreeNotification);
      end;
    end;
end;


Procedure TComponent.SetComponentIndex(Value: Integer);

Var Temp,Count : longint;

begin
  If Not assigned(Fowner) then exit;
  Temp:=getcomponentindex;
  If temp<0 then exit;
  If value<0 then value:=0;
  Count:=Fowner.FComponents.Count;
  If Value>=Count then value:=count-1;
  If Value<>Temp then
    begin
    FOWner.FComponents.Delete(Temp);
    FOwner.FComponents.Insert(Value,Self);
    end;
end;


Procedure TComponent.ChangeName(const NewName: TComponentName);

begin
  FName:=NewName;
end;


Procedure TComponent.GetChildren(Proc: TGetChildProc; Root: TComponent);

begin
  // Does nothing.
  if Proc=nil then ;
  if Root=nil then ;
end;


Function  TComponent.GetChildOwner: TComponent;

begin
 Result:=Nil;
end;


Function  TComponent.GetChildParent: TComponent;

begin
  Result:=Self;
end;


Function  TComponent.GetNamePath: string;

begin
  Result:=FName;
end;


Function  TComponent.GetOwner: TPersistent;

begin
  Result:=FOwner;
end;


Procedure TComponent.Loaded;

begin
  Exclude(FComponentState,csLoading);
end;

Procedure TComponent.Loading;

begin
  Include(FComponentState,csLoading);
end;


Procedure TComponent.Notification(AComponent: TComponent;
  Operation: TOperation);

Var
  C : Longint;

begin
  If (Operation=opRemove) then
    RemoveFreeNotification(AComponent);
  If Not assigned(FComponents) then
    exit;
  C:=FComponents.Count-1;
  While (C>=0) do
    begin
    TComponent(FComponents.Items[C]).Notification(AComponent,Operation);
    Dec(C);
    if C>=FComponents.Count then
      C:=FComponents.Count-1;
    end;
end;


procedure TComponent.PaletteCreated;
begin
end;



Procedure TComponent.SetAncestor(Value: Boolean);

Var Runner : Longint;

begin
  If Value then
    Include(FComponentState,csAncestor)
  else
    Exclude(FCOmponentState,csAncestor);
  if Assigned(FComponents) then
    For Runner:=0 To FComponents.Count-1 do
      TComponent(FComponents.Items[Runner]).SetAncestor(Value);
end;


Procedure TComponent.SetDesigning(Value: Boolean; SetChildren : Boolean = True);

Var Runner : Longint;

begin
  If Value then
    Include(FComponentState,csDesigning)
  else
    Exclude(FComponentState,csDesigning);
  if Assigned(FComponents) and SetChildren then
    For Runner:=0 To FComponents.Count - 1 do
      TComponent(FComponents.items[Runner]).SetDesigning(Value);
end;

Procedure TComponent.SetDesignInstance(Value: Boolean);

begin
  If Value then
    Include(FComponentState,csDesignInstance)
  else
    Exclude(FComponentState,csDesignInstance);
end;

Procedure TComponent.SetInline(Value: Boolean);

begin
  If Value then
    Include(FComponentState,csInline)
  else
    Exclude(FComponentState,csInline);
end;


Procedure TComponent.SetName(const NewName: TComponentName);

begin
  If FName=NewName then exit;
  If (NewName<>'') and not IsValidIdent(NewName) then
    Raise EComponentError.CreateFmt(SInvalidName,[NewName]);
  If Assigned(FOwner) Then
    FOwner.ValidateRename(Self,FName,NewName)
  else
    ValidateRename(Nil,FName,NewName);
  ChangeName(NewName);
end;


Procedure TComponent.SetChildOrder(Child: TComponent; Order: Integer);

begin
  // does nothing
  if Child=nil then ;
  if Order=0 then ;
end;


Procedure TComponent.SetParentComponent(Value: TComponent);

begin
  // Does nothing
  if Value=nil then ;
end;


Procedure TComponent.Updating;

begin
  Include (FComponentState,csUpdating);
end;


Procedure TComponent.Updated;

begin
  Exclude(FComponentState,csUpdating);
end;


Procedure TComponent.ValidateRename(AComponent: TComponent;
  const CurName, NewName: string);

begin
//!! This contradicts the Delphi manual.
  If (AComponent<>Nil) and (CompareText(CurName,NewName)<>0) and (AComponent.Owner = Self) and
     (FindComponent(NewName)<>Nil) then
      raise EComponentError.Createfmt(SDuplicateName,[newname]);
  If (csDesigning in FComponentState) and (FOwner<>Nil) then
    FOwner.ValidateRename(AComponent,Curname,Newname);
end;


Procedure TComponent.ValidateContainer(AComponent: TComponent);

begin
  AComponent.ValidateInsert(Self);
end;


Procedure TComponent.ValidateInsert(AComponent: TComponent);

begin
  // Does nothing.
  if AComponent=nil then ;
end;

function TComponent._AddRef: Integer;
begin
  Result:=-1;
end;

function TComponent._Release: Integer;
begin
  Result:=-1;
end;


Constructor TComponent.Create(AOwner: TComponent);

begin
  FComponentStyle:=[csInheritable];
  If Assigned(AOwner) then AOwner.InsertComponent(Self);
end;


Destructor TComponent.Destroy;

Var
  I : Integer;
  C : TComponent;

begin
  Destroying;
  If Assigned(FFreeNotifies) then
    begin
    I:=FFreeNotifies.Count-1;
    While (I>=0) do
      begin
      C:=TComponent(FFreeNotifies.Items[I]);
      // Delete, so one component is not notified twice, if it is owned.
      FFreeNotifies.Delete(I);
      C.Notification (self,opRemove);
      If (FFreeNotifies=Nil) then
        I:=0
      else if (I>FFreeNotifies.Count) then
        I:=FFreeNotifies.Count;
      dec(i);
      end;
    FreeAndNil(FFreeNotifies);
    end;
  DestroyComponents;
  If FOwner<>Nil Then FOwner.RemoveComponent(Self);
  inherited destroy;
end;


Procedure TComponent.BeforeDestruction;
begin
  if not(csDestroying in FComponentstate) then
    Destroying;
end;


Procedure TComponent.DestroyComponents;

Var acomponent: TComponent;

begin
  While assigned(FComponents) do
    begin
    aComponent:=TComponent(FComponents.Last);
    Remove(aComponent);
    Acomponent.Destroy;
    end;
end;


Procedure TComponent.Destroying;

Var Runner : longint;

begin
  If csDestroying in FComponentstate Then Exit;
  include (FComponentState,csDestroying);
  If Assigned(FComponents) then
    for Runner:=0 to FComponents.Count-1 do
      TComponent(FComponents.Items[Runner]).Destroying;
end;

function TComponent.QueryInterface(const IID: TGUID; out Obj): integer;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;

end;


Function  TComponent.FindComponent(const AName: string): TComponent;

Var I : longint;

begin
  Result:=Nil;
  If (AName='') or Not assigned(FComponents) then exit;
  For i:=0 to FComponents.Count-1 do
    if (CompareText(TComponent(FComponents[I]).Name,AName)=0) then
      begin
      Result:=TComponent(FComponents.Items[I]);
      exit;
      end;
end;


Procedure TComponent.FreeNotification(AComponent: TComponent);

begin
  If (Owner<>Nil) and (AComponent=Owner) then exit;
  If not (Assigned(FFreeNotifies)) then
    FFreeNotifies:=TFpList.Create;
  If FFreeNotifies.IndexOf(AComponent)=-1 then
    begin
    FFreeNotifies.Add(AComponent);
    AComponent.FreeNotification (self);
    end;
end;


procedure TComponent.RemoveFreeNotification(AComponent: TComponent);
begin
  RemoveNotification(AComponent);
  AComponent.RemoveNotification (self);
end;


Function  TComponent.GetParentComponent: TComponent;

begin
  Result:=Nil;
end;


Function  TComponent.HasParent: Boolean;

begin
  Result:=False;
end;


Procedure TComponent.InsertComponent(AComponent: TComponent);

begin
  AComponent.ValidateContainer(Self);
  ValidateRename(AComponent,'',AComponent.FName);
  Insert(AComponent);
  If csDesigning in FComponentState then
    AComponent.SetDesigning(true);
  Notification(AComponent,opInsert);
end;


Procedure TComponent.RemoveComponent(AComponent: TComponent);

begin
  Notification(AComponent,opRemove);
  Remove(AComponent);
  Acomponent.Setdesigning(False);
  ValidateRename(AComponent,AComponent.FName,'');
end;

procedure TComponent.SetSubComponent(ASubComponent: Boolean);
begin
  if ASubComponent then
    Include(FComponentStyle, csSubComponent)
  else
    Exclude(FComponentStyle, csSubComponent);
end;

function TComponent.GetEnumerator: TComponentEnumerator;
begin
  Result:=TComponentEnumerator.Create(Self);
end;


{ ---------------------------------------------------------------------
  Global routines
  ---------------------------------------------------------------------}

var
  ClassList : TJSObject;
  
Procedure RegisterClass(AClass : TPersistentClass);

begin
  ClassList[AClass.ClassName]:=AClass;
end;

Function GetClass(AClassName : string) : TPersistentClass;

begin
  Result:=nil;
  if AClassName='' then exit;
  if not ClassList.hasOwnProperty(AClassName) then exit;
  Result:=TPersistentClass(ClassList[AClassName]);
end;

initialization
  ClassList:=TJSObject.create(nil);
end.

