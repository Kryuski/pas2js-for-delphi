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
  RTLConsts, Types, SysUtils, JS, TypInfo;

type
  TNotifyEvent = procedure(Sender: TObject) of object;
  TNotifyEventRef = reference to procedure(Sender: TObject);
  TStringNotifyEventRef = Reference to Procedure(Sender: TObject; Const aString : String);

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
  TListSortCompareFunc = reference to function (Item1, Item2: JSValue): Integer;
  TListCallback = Types.TListCallback;
  TListStaticCallback = Types.TListStaticCallback;
  TAlignment = (taLeftJustify, taRightJustify, taCenter);

  // Forward class definitions
  TFPList = Class;
  TReader = Class;
  TWriter = Class;
  TFiler = Class;

  { TFPListEnumerator }
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
    procedure SortList(const Compare: TListSortCompareFunc);
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
    procedure SortList(const Compare: TListSortCompareFunc);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: JSValue read Get write Put; default;
    property List: TJSValueDynArray read GetList;
  end;

  { TPersistent }
  
{$M+}

  TPersistent = class(TObject)
  private
    //FObservers : TFPList;
    procedure AssignError(Source: TPersistent);
  protected
    procedure DefineProperties(Filer: TFiler); virtual;
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
    function _AddRef: Integer; {$IFDEF MAKESTUB}stdcall;{$ENDIF}
    function _Release: Integer; {$IFDEF MAKESTUB}stdcall;{$ENDIF}
  public
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; virtual;{$IFDEF MAKESTUB} stdcall;{$ENDIF}
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
    procedure SetValue(const Name : String; Const Value: string);
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
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
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
    function ToObjectArray: TObjectDynArray; overload;
    function ToObjectArray(aStart,aEnd : Integer): TObjectDynArray; overload;
    function ToStringArray: TStringDynArray; overload;
    function ToStringArray(aStart,aEnd : Integer): TStringDynArray; overload;
    function Add(const S: string): Integer; virtual; overload;
    function Add(const Fmt : string; const Args : Array of const): Integer; overload;
    function AddFmt(const Fmt : string; const Args : Array of const): Integer;
    function AddObject(const S: string; AObject: TObject): Integer; virtual; overload;
    function AddObject(const Fmt: string; Args : Array of const; AObject: TObject): Integer; overload;
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
    Procedure LoadFromURL(Const aURL : String; Async : Boolean = True; OnLoaded : TNotifyEventRef = Nil; OnError: TStringNotifyEventRef = Nil); virtual;
    // Delphi compatibility. Must be an URL
    Procedure LoadFromFile(Const aFileName : String; const OnLoaded : TProc = Nil; const AError: TProcString = Nil);
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
  TCollectionSortCompareFunc = reference to function (Item1, Item2: TCollectionItem): Integer;

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
    procedure SortList(Const Compare : TCollectionSortCompareFunc);
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
    procedure ReadLeft(AReader: TReader);
    procedure ReadTop(AReader: TReader);
    procedure Remove(AComponent: TComponent);
    procedure RemoveNotification(AComponent: TComponent);
    procedure SetComponentIndex(Value: Integer);
    procedure SetReference(Enable: Boolean);
    procedure WriteLeft(AWriter: TWriter);
    procedure WriteTop(AWriter: TWriter);
  protected
    FComponentStyle: TComponentStyle;
    procedure ChangeName(const NewName: TComponentName);
    procedure DefineProperties(Filer: TFiler); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); virtual;
    function GetChildOwner: TComponent; virtual;
    function GetChildParent: TComponent; virtual;
    function GetOwner: TPersistent; override;
    procedure Loaded; virtual;
    procedure Loading; virtual;
    procedure SetWriting(Value: Boolean); virtual;
    procedure SetReading(Value: Boolean); virtual;
    procedure Notification(AComponent: TComponent;  Operation: TOperation); virtual;
    procedure PaletteCreated; virtual;
    procedure ReadState(Reader: TReader); virtual;
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
    function _AddRef: Integer; {$IFDEF MAKESTUB}stdcall;{$ENDIF}
    function _Release: Integer;  {$IFDEF MAKESTUB}stdcall;{$ENDIF}
  public
    constructor Create(AOwner: TComponent); virtual; reintroduce;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure DestroyComponents;
    procedure Destroying;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; virtual;  {$IFDEF MAKESTUB} stdcall;{$ENDIF}
    procedure WriteState(Writer: TWriter); virtual;
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
    property Tag: PtrInt read FTag write FTag default 0;
  end;
  TComponentClass = Class of TComponent;

  TSeekOrigin = (soBeginning, soCurrent, soEnd);

  { TStream }

  TStream = class(TObject)
  private
    FEndian: TEndian;
    function MakeInt(B: TBytes; aSize: Integer; Signed: Boolean): NativeInt;
    function MakeBytes(B: NativeInt; aSize: Integer; Signed: Boolean): TBytes;
  protected
    procedure InvalidSeek; virtual;
    procedure Discard(const Count: NativeInt);
    procedure DiscardLarge(Count: NativeInt; const MaxBufferSize: Longint);
    procedure FakeSeekForward(Offset: NativeInt; const Origin: TSeekOrigin; const Pos: NativeInt);
    function  GetPosition: NativeInt; virtual;
    procedure SetPosition(const Pos: NativeInt); virtual;
    function  GetSize: NativeInt; virtual;
    procedure SetSize(const NewSize: NativeInt); virtual;
    procedure SetSize64(const NewSize: NativeInt); virtual;
    procedure ReadNotImplemented;
    procedure WriteNotImplemented;
    function ReadMaxSizeData(Buffer : TBytes; aSize,aCount : NativeInt) : NativeInt;
    Procedure ReadExactSizeData(Buffer : TBytes; aSize,aCount : NativeInt);
    function WriteMaxSizeData(Const Buffer : TBytes; aSize,aCount : NativeInt) : NativeInt;
    Procedure WriteExactSizeData(Const Buffer : TBytes; aSize,aCount : NativeInt);
  public
    function Read(var Buffer: TBytes; Count: Longint): Longint; overload;
    function Read(Buffer : TBytes; aOffset, Count: Longint): Longint; virtual; abstract; overload;

    function Write(const Buffer: TBytes; Count: Longint): Longint; virtual; overload;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; virtual; abstract; overload;

    function Seek(const Offset: NativeInt; Origin: TSeekOrigin): NativeInt; virtual; abstract; overload;

    function ReadData(Buffer: TBytes; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: Boolean): NativeInt; overload;
    function ReadData(var Buffer: Boolean; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: WideChar): NativeInt; overload;
    function ReadData(var Buffer: WideChar; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: Int8): NativeInt; overload;
    function ReadData(var Buffer: Int8; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: UInt8): NativeInt; overload;
    function ReadData(var Buffer: UInt8; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: Int16): NativeInt; overload;
    function ReadData(var Buffer: Int16; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: UInt16): NativeInt; overload;
    function ReadData(var Buffer: UInt16; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: Int32): NativeInt; overload;
    function ReadData(var Buffer: Int32; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: UInt32): NativeInt; overload;
    function ReadData(var Buffer: UInt32; Count: NativeInt): NativeInt; overload;
    // NativeLargeint. Stored as a float64, Read as float64.
    function ReadData(var Buffer: NativeLargeInt): NativeInt; overload;
    function ReadData(var Buffer: NativeLargeInt; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: NativeLargeUInt): NativeInt; overload;
    function ReadData(var Buffer: NativeLargeUInt; Count: NativeInt): NativeInt; overload;
    // Note: a ReadData with Int64 would be Delphi/FPC incompatible
    function ReadData(var Buffer: Double): NativeInt; overload;
    function ReadData(var Buffer: Double; Count: NativeInt): NativeInt; overload;
    procedure ReadBuffer(var Buffer: TBytes; Count: NativeInt); overload;
    procedure ReadBuffer(var Buffer: TBytes; Offset, Count: NativeInt); overload;

    procedure ReadBufferData(var Buffer: Boolean); overload;
    procedure ReadBufferData(var Buffer: Boolean; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: WideChar); overload;
    procedure ReadBufferData(var Buffer: WideChar; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: Int8); overload;
    procedure ReadBufferData(var Buffer: Int8; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: UInt8); overload;
    procedure ReadBufferData(var Buffer: UInt8; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: Int16); overload;
    procedure ReadBufferData(var Buffer: Int16; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: UInt16); overload;
    procedure ReadBufferData(var Buffer: UInt16; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: Int32); overload;
    procedure ReadBufferData(var Buffer: Int32; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: UInt32); overload;
    procedure ReadBufferData(var Buffer: UInt32; Count: NativeInt); overload;
    // NativeLargeint. Stored as a float64, Read as float64.
    procedure ReadBufferData(var Buffer: NativeLargeInt); overload;
    procedure ReadBufferData(var Buffer: NativeLargeInt; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: NativeLargeUInt); overload;
    procedure ReadBufferData(var Buffer: NativeLargeUInt; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: Double); overload;
    procedure ReadBufferData(var Buffer: Double; Count: NativeInt); overload;
    procedure WriteBuffer(const Buffer: TBytes; Count: NativeInt); overload;
    procedure WriteBuffer(const Buffer: TBytes; Offset, Count: NativeInt); overload;

    function WriteData(const Buffer: TBytes; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: Boolean): NativeInt; overload;
    function WriteData(const Buffer: Boolean; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: WideChar): NativeInt; overload;
    function WriteData(const Buffer: WideChar; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: Int8): NativeInt; overload;
    function WriteData(const Buffer: Int8; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: UInt8): NativeInt; overload;
    function WriteData(const Buffer: UInt8; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: Int16): NativeInt; overload;
    function WriteData(const Buffer: Int16; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: UInt16): NativeInt; overload;
    function WriteData(const Buffer: UInt16; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: Int32): NativeInt; overload;
    function WriteData(const Buffer: Int32; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: UInt32): NativeInt; overload;
    function WriteData(const Buffer: UInt32; Count: NativeInt): NativeInt; overload;
    // NativeLargeint. Stored as a float64, Read as float64.
    function WriteData(const Buffer: NativeLargeInt): NativeInt; overload;
    function WriteData(const Buffer: NativeLargeInt; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: NativeLargeUInt): NativeInt; overload;
    function WriteData(const Buffer: NativeLargeUInt; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: Double): NativeInt; overload;
    function WriteData(const Buffer: Double; Count: NativeInt): NativeInt; overload;
{$IFDEF FPC_HAS_TYPE_EXTENDED}
    function WriteData(const Buffer: Extended): NativeInt; overload;
    function WriteData(const Buffer: Extended; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: TExtended80Rec): NativeInt; overload;
    function WriteData(const Buffer: TExtended80Rec; Count: NativeInt): NativeInt; overload;
{$ENDIF}
    procedure WriteBufferData(Buffer: Int32); overload;
    procedure WriteBufferData(Buffer: Int32; Count: NativeInt); overload;
    procedure WriteBufferData(Buffer: Boolean); overload;
    procedure WriteBufferData(Buffer: Boolean; Count: NativeInt); overload;
    procedure WriteBufferData(Buffer: WideChar); overload;
    procedure WriteBufferData(Buffer: WideChar; Count: NativeInt); overload;
    procedure WriteBufferData(Buffer: Int8); overload;
    procedure WriteBufferData(Buffer: Int8; Count: NativeInt); overload;
    procedure WriteBufferData(Buffer: UInt8); overload;
    procedure WriteBufferData(Buffer: UInt8; Count: NativeInt); overload;
    procedure WriteBufferData(Buffer: Int16); overload;
    procedure WriteBufferData(Buffer: Int16; Count: NativeInt); overload;
    procedure WriteBufferData(Buffer: UInt16); overload;
    procedure WriteBufferData(Buffer: UInt16; Count: NativeInt); overload;
    procedure WriteBufferData(Buffer: UInt32); overload;
    procedure WriteBufferData(Buffer: UInt32; Count: NativeInt); overload;
    // NativeLargeint. Stored as a float64, Read as float64.
    procedure WriteBufferData(Buffer: NativeLargeInt); overload;
    procedure WriteBufferData(Buffer: NativeLargeInt; Count: NativeInt); overload;
    procedure WriteBufferData(Buffer: NativeLargeUInt); overload;
    procedure WriteBufferData(Buffer: NativeLargeUInt; Count: NativeInt); overload;
    procedure WriteBufferData(Buffer: Double); overload;
    procedure WriteBufferData(Buffer: Double; Count: NativeInt); overload;
    function CopyFrom(Source: TStream; Count: NativeInt): NativeInt;
    function ReadComponent(Instance: TComponent): TComponent;
    function ReadComponentRes(Instance: TComponent): TComponent;
    procedure WriteComponent(Instance: TComponent);
    procedure WriteComponentRes(const ResName: string; Instance: TComponent);
    procedure WriteDescendent(Instance, Ancestor: TComponent);
    procedure WriteDescendentRes(const ResName: string; Instance, Ancestor: TComponent);
    procedure WriteResourceHeader(const ResName: string; {!!!:out} var FixupInfo: Longint);
    procedure FixupResourceHeader(FixupInfo: Longint);
    procedure ReadResHeader;
    function ReadByte : Byte;
    function ReadWord : Word;
    function ReadDWord : Cardinal;
    function ReadQWord : NativeLargeUInt;
    procedure WriteByte(b : Byte);
    procedure WriteWord(w : Word);
    procedure WriteDWord(d : Cardinal);
    procedure WriteQWord(q : NativeLargeUInt);
    property Position: NativeInt read GetPosition write SetPosition;
    property Size: NativeInt read GetSize write SetSize64;
    Property Endian: TEndian Read FEndian Write FEndian;
  end;

  { TCustomMemoryStream abstract class }

  TCustomMemoryStream = class(TStream)
  private
    FMemory: TJSArrayBuffer;
    FDataView : TJSDataView;
    FDataArray : TJSUint8Array;
    FSize, FPosition: PtrInt;
    FSizeBoundsSeek : Boolean;
    function GetDataArray: TJSUint8Array;
    function GetDataView: TJSDataview;
  protected
    Function GetSize : NativeInt; Override;
    function GetPosition: NativeInt; Override;
    procedure SetPointer(Ptr: TJSArrayBuffer; ASize: PtrInt);
    Property DataView : TJSDataview Read GetDataView;
    Property DataArray : TJSUint8Array Read GetDataArray;
  public
    Class Function MemoryToBytes(Mem : TJSArrayBuffer) : TBytes; overload;
    Class Function MemoryToBytes(Mem : TJSUint8Array) : TBytes; overload;
    Class Function BytesToMemory(aBytes : TBytes) : TJSArrayBuffer;
    function Read(Buffer : TBytes; Offset, Count: LongInt): LongInt; override;
    function Seek(const Offset: NativeInt; Origin: TSeekOrigin): NativeInt; override;
    procedure SaveToStream(Stream: TStream);
    Procedure LoadFromURL(Const aURL : String; Async : Boolean = True; OnLoaded : TNotifyEventRef = Nil; OnError: TStringNotifyEventRef = Nil); virtual;
    // Delphi compatibility. Must be an URL
    Procedure LoadFromFile(Const aFileName : String; const OnLoaded : TProc = Nil; const AError: TProcString = Nil);
    property Memory: TJSArrayBuffer read FMemory;
    Property SizeBoundsSeek : Boolean Read FSizeBoundsSeek Write FSizeBoundsSeek;
  end;

  { TMemoryStream }

  TMemoryStream = class(TCustomMemoryStream)
  private
    FCapacity: PtrInt;
    procedure SetCapacity(NewCapacity: PtrInt);
  protected
    function Realloc(var NewCapacity: PtrInt): TJSArrayBuffer; virtual;
    property Capacity: PtrInt read FCapacity write SetCapacity;
  public
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromStream(Stream: TStream);
    procedure SetSize(const NewSize: NativeInt); override;
    function Write(const Buffer: TBytes; Offset, Count: LongInt): LongInt; override;
  end;

{ TBytesStream }

  TBytesStream = class(TMemoryStream)
  private
    function GetBytes: TBytes;
  public
    constructor Create(const ABytes: TBytes); virtual; overload;
    property Bytes: TBytes read GetBytes;
  end;

  { TStringStream }

  TStringStream = class(TMemoryStream)
  private
    function GetDataString : String;
  public
    constructor Create; reintroduce; overload;
    constructor Create(const aString: String); virtual; overload;
    function ReadString(Count: Integer): string;
    procedure WriteString(const AString: string);
    property DataString: String read GetDataString;
  end;


  TFilerFlag = (ffInherited, ffChildPos, ffInline);
  TFilerFlags = set of TFilerFlag;

  TReaderProc = procedure(Reader: TReader) of object;
  TWriterProc = procedure(Writer: TWriter) of object;
  TStreamProc = procedure(Stream: TStream) of object;

  TFiler = class(TObject)
  private
    FRoot: TComponent;
    FLookupRoot: TComponent;
    FAncestor: TPersistent;
    FIgnoreChildren: Boolean;
  protected
    procedure SetRoot(ARoot: TComponent); virtual;
  public
    procedure DefineProperty(const Name: string;
      ReadData: TReaderProc; WriteData: TWriterProc;
      HasData: Boolean); virtual; abstract;
    procedure DefineBinaryProperty(const Name: string;
      ReadData, WriteData: TStreamProc;
      HasData: Boolean); virtual; abstract;
    Procedure FlushBuffer; virtual; abstract;
    property Root: TComponent read FRoot write SetRoot;
    property LookupRoot: TComponent read FLookupRoot;
    property Ancestor: TPersistent read FAncestor write FAncestor;
    property IgnoreChildren: Boolean read FIgnoreChildren write FIgnoreChildren;
  end;


  TValueType = (
      vaNull, vaList, vaInt8, vaInt16, vaInt32, vaDouble,
      vaString, vaIdent, vaFalse, vaTrue, vaBinary, vaSet,
      vaNil, vaCollection, vaCurrency, vaDate, vaNativeInt
      );

  { TAbstractObjectReader }

  TAbstractObjectReader = class
  public
    Procedure FlushBuffer; virtual;
    function NextValue: TValueType; virtual; abstract;
    function ReadValue: TValueType; virtual; abstract;
    procedure BeginRootComponent; virtual; abstract;
    procedure BeginComponent(var Flags: TFilerFlags; var AChildPos: Integer;
      var CompClassName, CompName: String); virtual; abstract;
    function BeginProperty: String; virtual; abstract;

    //Please don't use read, better use ReadBinary whenever possible
    procedure Read(var Buffer : TBytes; Count: Longint); virtual;abstract;

    { All ReadXXX methods are called _after_ the value type has been read! }
    procedure ReadBinary(const DestData: TMemoryStream); virtual; abstract;
    function ReadFloat: Extended; virtual; abstract;
    function ReadCurrency: Currency; virtual; abstract;
    function ReadIdent(ValueType: TValueType): String; virtual; abstract;
    function ReadInt8: ShortInt; virtual; abstract;
    function ReadInt16: SmallInt; virtual; abstract;
    function ReadInt32: LongInt; virtual; abstract;
    function ReadNativeInt: NativeInt; virtual; abstract;
    function ReadSet(EnumType: TTypeInfoEnum): Integer; virtual; abstract;
    procedure ReadSignature; virtual; abstract;
    function ReadStr: String; virtual; abstract;
    function ReadString(StringType: TValueType): String; virtual; abstract;
    function ReadWideString: WideString;virtual;abstract;
    function ReadUnicodeString: UnicodeString;virtual;abstract;
    procedure SkipComponent(SkipComponentInfos: Boolean); virtual; abstract;
    procedure SkipValue; virtual; abstract;
  end;

  { TBinaryObjectReader }

  TBinaryObjectReader = class(TAbstractObjectReader)
  protected
    FStream: TStream;
    function ReadWord : word;
    function ReadDWord : longword;
    procedure SkipProperty;
    procedure SkipSetBody;
  public
    constructor Create(Stream: TStream);
    function NextValue: TValueType; override;
    function ReadValue: TValueType; override;
    procedure BeginRootComponent; override;
    procedure BeginComponent(var Flags: TFilerFlags; var AChildPos: Integer;
      var CompClassName, CompName: String); override;
    function BeginProperty: String; override;
    //Please don't use read, better use ReadBinary whenever possible
    procedure Read(var Buffer : TBytes; Count: Longint); override;
    procedure ReadBinary(const DestData: TMemoryStream); override;
    function ReadFloat: Extended; override;
    function ReadCurrency: Currency; override;
    function ReadIdent(ValueType: TValueType): String; override;
    function ReadInt8: ShortInt; override;
    function ReadInt16: SmallInt; override;
    function ReadInt32: LongInt; override;
    function ReadNativeInt: NativeInt; override;
    function ReadSet(EnumType: TTypeInfoEnum): Integer; override;
    procedure ReadSignature; override;
    function ReadStr: String; override;
    function ReadString(StringType: TValueType): String; override;
    function ReadWideString: WideString;override;
    function ReadUnicodeString: UnicodeString;override;
    procedure SkipComponent(SkipComponentInfos: Boolean); override;
    procedure SkipValue; override;
  end;


  TFindMethodEvent = procedure(Reader: TReader; const MethodName: string;  var Address: CodePointer; var Error: Boolean) of object;
  TSetNameEvent = procedure(Reader: TReader; Component: TComponent;  var Name: string) of object;
  TReferenceNameEvent = procedure(Reader: TReader; var Name: string) of object;
  TAncestorNotFoundEvent = procedure(Reader: TReader; const ComponentName: string; ComponentClass: TPersistentClass; var Component: TComponent) of object;
  TReadComponentsProc = procedure(Component: TComponent) of object;
  TReaderError = procedure(Reader: TReader; const Message: string; var Handled: Boolean) of object;
  TPropertyNotFoundEvent = procedure(Reader: TReader; Instance: TPersistent; var PropName: string; IsPath: boolean; var Handled, Skip: Boolean) of object;
  TFindComponentClassEvent = procedure(Reader: TReader; const ClassName: string; var ComponentClass: TComponentClass) of object;
  TCreateComponentEvent = procedure(Reader: TReader; ComponentClass: TComponentClass; var Component: TComponent) of object;

  TSetMethodPropertyEvent = procedure(Reader: TReader; Instance: TPersistent; PropInfo: TTypeMemberProperty; const TheMethodName: string;
    var Handled: boolean) of object;
  TReadWriteStringPropertyEvent = procedure(Sender:TObject; const Instance: TPersistent; PropInfo: TTypeMemberProperty; var Content:string) of object;


  { TReader }

  TReader = class(TFiler)
  private
    FDriver: TAbstractObjectReader;
    FOwner: TComponent;
    FParent: TComponent;
    FFixups: TObject;
    FLoaded: TFpList;
    FOnFindMethod: TFindMethodEvent;
    FOnSetMethodProperty: TSetMethodPropertyEvent;
    FOnSetName: TSetNameEvent;
    FOnReferenceName: TReferenceNameEvent;
    FOnAncestorNotFound: TAncestorNotFoundEvent;
    FOnError: TReaderError;
    FOnPropertyNotFound: TPropertyNotFoundEvent;
    FOnFindComponentClass: TFindComponentClassEvent;
    FOnCreateComponent: TCreateComponentEvent;
    FPropName: string;
    FCanHandleExcepts: Boolean;
    FOnReadStringProperty:TReadWriteStringPropertyEvent;
    procedure DoFixupReferences;
    function FindComponentClass(const AClassName: string): TComponentClass;
  protected
    function Error(const Message: string): Boolean; virtual;
    function FindMethod(ARoot: TComponent; const AMethodName: string): CodePointer; virtual;
    procedure ReadProperty(AInstance: TPersistent);
    procedure ReadPropValue(Instance: TPersistent; PropInfo: TTypeMemberProperty);
    procedure PropertyError;
    procedure ReadData(Instance: TComponent);
    property PropName: string read FPropName;
    property CanHandleExceptions: Boolean read FCanHandleExcepts;
    function CreateDriver(Stream: TStream): TAbstractObjectReader; virtual;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    Procedure FlushBuffer; override;
    procedure BeginReferences;
    procedure CheckValue(Value: TValueType);
    procedure DefineProperty(const Name: string;
      AReadData: TReaderProc; WriteData: TWriterProc;
      HasData: Boolean); override;
    procedure DefineBinaryProperty(const Name: string;
      AReadData, WriteData: TStreamProc;
      HasData: Boolean); override;
    function EndOfList: Boolean;
    procedure EndReferences;
    procedure FixupReferences;
    function NextValue: TValueType;
    //Please don't use read, better use ReadBinary whenever possible
    //uuups, ReadBinary is protected ..
    procedure Read(var Buffer : TBytes; Count: LongInt); virtual;

    function ReadBoolean: Boolean;
    function ReadChar: Char;
    function ReadWideChar: WideChar;
    function ReadUnicodeChar: UnicodeChar;
    procedure ReadCollection(Collection: TCollection);
    function ReadComponent(Component: TComponent): TComponent;
    procedure ReadComponents(AOwner, AParent: TComponent;
      Proc: TReadComponentsProc);
    function ReadFloat: Extended;
    function ReadCurrency: Currency;
    function ReadIdent: string;
    function ReadInteger: Longint;
    function ReadNativeInt: NativeInt;
    function ReadSet(EnumType: Pointer): Integer;
    procedure ReadListBegin;
    procedure ReadListEnd;
    function ReadRootComponent(ARoot: TComponent): TComponent;
    function ReadVariant: JSValue;
    procedure ReadSignature;
    function ReadString: string;
    function ReadWideString: WideString;
    function ReadUnicodeString: UnicodeString;
    function ReadValue: TValueType;
    procedure CopyValue(Writer: TWriter);
    property Driver: TAbstractObjectReader read FDriver;
    property Owner: TComponent read FOwner write FOwner;
    property Parent: TComponent read FParent write FParent;
    property OnError: TReaderError read FOnError write FOnError;
    property OnPropertyNotFound: TPropertyNotFoundEvent read FOnPropertyNotFound write FOnPropertyNotFound;
    property OnFindMethod: TFindMethodEvent read FOnFindMethod write FOnFindMethod;
    property OnSetMethodProperty: TSetMethodPropertyEvent read FOnSetMethodProperty write FOnSetMethodProperty;
    property OnSetName: TSetNameEvent read FOnSetName write FOnSetName;
    property OnReferenceName: TReferenceNameEvent read FOnReferenceName write FOnReferenceName;
    property OnAncestorNotFound: TAncestorNotFoundEvent read FOnAncestorNotFound write FOnAncestorNotFound;
    property OnCreateComponent: TCreateComponentEvent read FOnCreateComponent write FOnCreateComponent;
    property OnFindComponentClass: TFindComponentClassEvent read FOnFindComponentClass write FOnFindComponentClass;
    property OnReadStringProperty: TReadWriteStringPropertyEvent read FOnReadStringProperty write FOnReadStringProperty;
  end;


  { TAbstractObjectWriter }

  TAbstractObjectWriter = class
  public
    { Begin/End markers. Those ones who don't have an end indicator, use
      "EndList", after the occurrence named in the comment. Note that this
      only counts for "EndList" calls on the same level; each BeginXXX call
      increases the current level. }
    procedure BeginCollection; virtual; abstract;  { Ends with the next "EndList" }
    procedure BeginComponent(Component: TComponent; Flags: TFilerFlags;
      ChildPos: Integer); virtual; abstract;  { Ends after the second "EndList" }
    procedure WriteSignature; virtual; abstract;
    procedure BeginList; virtual; abstract;
    procedure EndList; virtual; abstract;
    procedure BeginProperty(const PropName: String); virtual; abstract;
    procedure EndProperty; virtual; abstract;
    //Please don't use write, better use WriteBinary whenever possible
    procedure Write(const Buffer : TBytes; Count: Longint); virtual;abstract;
    Procedure FlushBuffer; virtual; abstract;

    procedure WriteBinary(const Buffer : TBytes; Count: Longint); virtual; abstract;
    procedure WriteBoolean(Value: Boolean); virtual; abstract;
    // procedure WriteChar(Value: Char);
    procedure WriteFloat(const Value: Extended); virtual; abstract;
    procedure WriteCurrency(const Value: Currency); virtual; abstract;
    procedure WriteIdent(const Ident: string); virtual; abstract;
    procedure WriteInteger(Value: NativeInt); virtual; abstract;
    procedure WriteNativeInt(Value: NativeInt); virtual; abstract;
    procedure WriteVariant(const Value: JSValue); virtual; abstract;
    procedure WriteMethodName(const Name: String); virtual; abstract;
    procedure WriteSet(Value: LongInt; SetType: Pointer); virtual; abstract;
    procedure WriteString(const Value: String); virtual; abstract;
    procedure WriteWideString(const Value: WideString);virtual;abstract;
    procedure WriteUnicodeString(const Value: UnicodeString);virtual;abstract;
  end;

  { TBinaryObjectWriter }

  TBinaryObjectWriter = class(TAbstractObjectWriter)
  protected
    FStream: TStream;
    FBuffer: Pointer;
    FBufSize: Integer;
    FBufPos: Integer;
    FBufEnd: Integer;
    procedure WriteWord(w : word);
    procedure WriteDWord(lw : longword);
    procedure WriteValue(Value: TValueType);
  public
    constructor Create(Stream: TStream);
    procedure WriteSignature; override;
    procedure BeginCollection; override;
    procedure BeginComponent(Component: TComponent; Flags: TFilerFlags;
      ChildPos: Integer); override;
    procedure BeginList; override;
    procedure EndList; override;
    procedure BeginProperty(const PropName: String); override;
    procedure EndProperty; override;
    Procedure FlushBuffer; override;

    //Please don't use write, better use WriteBinary whenever possible
    procedure Write(const Buffer : TBytes; Count: Longint); override;
    procedure WriteBinary(const Buffer : TBytes; Count: LongInt); override;
    procedure WriteBoolean(Value: Boolean); override;
    procedure WriteFloat(const Value: Extended); override;
    procedure WriteCurrency(const Value: Currency); override;
    procedure WriteIdent(const Ident: string); override;
    procedure WriteInteger(Value: NativeInt); override;
    procedure WriteNativeInt(Value: NativeInt); override;
    procedure WriteMethodName(const Name: String); override;
    procedure WriteSet(Value: LongInt; SetType: Pointer); override;
    procedure WriteStr(const Value: String);
    procedure WriteString(const Value: String); override;
    procedure WriteWideString(const Value: WideString); override;
    procedure WriteUnicodeString(const Value: UnicodeString); override;
    procedure WriteVariant(const VarValue: JSValue);override;
  end;

  TFindAncestorEvent = procedure (Writer: TWriter; Component: TComponent;
    const Name: string; var Ancestor, RootAncestor: TComponent) of object;
  TWriteMethodPropertyEvent = procedure (Writer: TWriter; Instance: TPersistent;
    PropInfo: TTypeMemberProperty;
    const MethodValue, DefMethodValue: TMethod;
    var Handled: boolean) of object;

  { TWriter }

  TWriter = class(TFiler)
  private
    FDriver: TAbstractObjectWriter;
    FDestroyDriver: Boolean;
    FRootAncestor: TComponent;
    FPropPath: String;
    FAncestors: TStringList;
    FAncestorPos: Integer;
    FCurrentPos: Integer;
    FOnFindAncestor: TFindAncestorEvent;
    FOnWriteMethodProperty: TWriteMethodPropertyEvent;
    FOnWriteStringProperty:TReadWriteStringPropertyEvent;
    procedure AddToAncestorList(Component: TComponent);
    procedure WriteComponentData(Instance: TComponent);
    Procedure DetermineAncestor(Component: TComponent);
    procedure DoFindAncestor(Component : TComponent);
  protected
    procedure SetRoot(ARoot: TComponent); override;
    procedure WriteBinary(AWriteData: TStreamProc);
    procedure WriteProperty(Instance: TPersistent; PropInfo: TTypeMemberProperty);
    procedure WriteProperties(Instance: TPersistent);
    procedure WriteChildren(Component: TComponent);
    function CreateDriver(Stream: TStream): TAbstractObjectWriter; virtual;
  public
    constructor Create(ADriver: TAbstractObjectWriter);
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    procedure DefineProperty(const Name: string;
      ReadData: TReaderProc; AWriteData: TWriterProc;
      HasData: Boolean); override;
    procedure DefineBinaryProperty(const Name: string;
      ReadData, AWriteData: TStreamProc;
      HasData: Boolean); override;
    Procedure FlushBuffer; override;
    procedure Write(const Buffer : TBytes; Count: Longint); virtual;
    procedure WriteBoolean(Value: Boolean);
    procedure WriteCollection(Value: TCollection);
    procedure WriteComponent(Component: TComponent);
    procedure WriteChar(Value: Char);
    procedure WriteWideChar(Value: WideChar);
    procedure WriteDescendent(ARoot: TComponent; AAncestor: TComponent);
    procedure WriteFloat(const Value: Extended);
    procedure WriteCurrency(const Value: Currency);
    procedure WriteIdent(const Ident: string);
    procedure WriteInteger(Value: Longint); overload;
    procedure WriteInteger(Value: NativeInt); overload;
    procedure WriteSet(Value: LongInt; SetType: Pointer);
    procedure WriteListBegin;
    procedure WriteListEnd;
    Procedure WriteSignature;
    procedure WriteRootComponent(ARoot: TComponent);
    procedure WriteString(const Value: string);
    procedure WriteWideString(const Value: WideString);
    procedure WriteUnicodeString(const Value: UnicodeString);
    procedure WriteVariant(const VarValue: JSValue);
    property RootAncestor: TComponent read FRootAncestor write FRootAncestor;
    property OnFindAncestor: TFindAncestorEvent read FOnFindAncestor write FOnFindAncestor;
    property OnWriteMethodProperty: TWriteMethodPropertyEvent read FOnWriteMethodProperty write FOnWriteMethodProperty;
    property OnWriteStringProperty: TReadWriteStringPropertyEvent read FOnWriteStringProperty write FOnWriteStringProperty;

    property Driver: TAbstractObjectWriter read FDriver;
    property PropertyPath: string read FPropPath;
  end;

  TParserToken = (toUnknown,  // everything else
                  toEOF,      // EOF
                  toSymbol,   // Symbol (identifier)
                  toString,   // ''string''
                  toInteger,  // 123
                  toFloat,    // 12.3
                  toMinus,    // -
                  toSetStart, // [
                  toListStart, // (
                  toCollectionStart, // <
                  toBinaryStart, // {
                  toSetEnd, // ]
                  toListEnd, // )
                  toCollectionEnd, // >
                  toBinaryEnd, // }
                  toComma, // ,
                  toDot, // .
                  toEqual, // =
                  toColon, // :
                  toPlus // +
                  );

  TParser = class(TObject)
  private
    fStream : TStream;
    fBuf : Array of Char;
    FBufLen : integer;
    fPos : integer;
    fDeltaPos : integer;
    fFloatType : char;
    fSourceLine : integer;
    fToken : TParserToken;
    fEofReached : boolean;
    fLastTokenStr : string;
    function GetTokenName(aTok : TParserToken) : string;
    procedure LoadBuffer;
    procedure CheckLoadBuffer; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    procedure ProcessChar; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function IsNumber : boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function IsHexNum : boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function IsAlpha : boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function IsAlphaNum : boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function GetHexValue(c : char) : byte; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function GetAlphaNum : string;
    procedure HandleNewLine;
    procedure SkipBOM;
    procedure SkipSpaces;
    procedure SkipWhitespace;
    procedure HandleEof;
    procedure HandleAlphaNum;
    procedure HandleNumber;
    procedure HandleHexNumber;
    function HandleQuotedString : string;
    Function HandleDecimalCharacter: char;
    procedure HandleString;
    procedure HandleMinus;
    procedure HandleUnknown;
    procedure GotoToNextChar;
  public
    // Input stream is expected to be UTF16 !
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    procedure CheckToken(T: TParserToken);
    procedure CheckTokenSymbol(const S: string);
    procedure Error(const Ident: string);
    procedure ErrorFmt(const Ident: string; const Args: array of const);
    procedure ErrorStr(const Message: string);
    procedure HexToBinary(Stream: TStream);
    function NextToken: TParserToken;
    function SourcePos: Longint;
    function TokenComponentIdent: string;
    function TokenFloat: Double;
    function TokenInt: NativeInt;
    function TokenString: string;
    function TokenSymbolIs(const S: string): Boolean;
    property FloatType: Char read fFloatType;
    property SourceLine: Integer read fSourceLine;
    property Token: TParserToken read fToken;
  end;


  { TObjectStreamConverter }

  TObjectTextEncoding = (oteDFM,oteLFM);

  TObjectStreamConverter = Class
  private
     FIndent: String;
     FInput : TStream;
     FOutput : TStream;
     FEncoding : TObjectTextEncoding;
  Private
    FPlainStrings: Boolean;
     // Low level writing
     procedure Outchars(S : String); virtual;
     procedure OutLn(s: String); virtual;
     procedure OutStr(s: String); virtual;
     procedure OutString(s: String); virtual;
     // Low level reading
     function ReadWord: word;
     function ReadDWord: longword;
     function ReadDouble: Double;
     function ReadInt(ValueType: TValueType): NativeInt;
     function ReadInt: NativeInt;
     function ReadNativeInt: NativeInt;
     function ReadStr: String;
     function ReadString(StringType: TValueType): String; virtual;
     // High-level
     procedure ProcessBinary; virtual;
     procedure ProcessValue(ValueType: TValueType; Indent: String); virtual;
     procedure ReadObject(indent: String); virtual;
     procedure ReadPropList(indent: String); virtual;
   Public
     procedure ObjectBinaryToText(aInput, aOutput: TStream);
     procedure ObjectBinaryToText(aInput, aOutput: TStream; aEncoding: TObjectTextEncoding);
     Procedure Execute;
     // use this to get previous streaming behavour: strings written as-is
     Property PlainStrings : Boolean Read FPlainStrings Write FPlainStrings;
     Property Input : TStream Read FInput Write FInput;
     Property Output : TStream Read Foutput Write FOutput;
     Property Encoding : TObjectTextEncoding Read FEncoding Write FEncoding;
     Property Indent : String Read FIndent Write Findent;
   end;

  { TObjectTextConverter }

  TObjectTextConverter = Class
  private
    FParser: TParser;
  private
    FInput: TStream;
    Foutput: TStream;
    procedure WriteDouble(e: double);
    procedure WriteDWord(lw: longword);
    procedure WriteInteger(value: nativeInt);
    //procedure WriteLString(const s: String);
    procedure WriteQWord(q: nativeint);
    procedure WriteString(s: String);
    procedure WriteWord(w: word);
    procedure WriteWString(const s: WideString);
    procedure ProcessObject; virtual;
    procedure ProcessProperty; virtual;
    procedure ProcessValue; virtual;
    procedure ProcessWideString(const left: string);
    Property Parser : TParser Read FParser;
  Public
    // Input stream must be UTF16 !
    procedure ObjectTextToBinary(aInput, aOutput: TStream);
    Procedure Execute; virtual;
    Property Input : TStream Read FInput Write FInput;
    Property Output: TStream Read Foutput Write Foutput;
  end;

  TLoadHelper = Class (TObject)
  Public
  Type
    TTextLoadedCallBack = reference to procedure (const aText : String);
    TBytesLoadedCallBack = reference to procedure (const aBuffer : TJSArrayBuffer);
    TErrorCallBack = reference to procedure (const aError : String);
    Class Procedure LoadText(aURL : String; aSync : Boolean; OnLoaded : TTextLoadedCallBack; OnError : TErrorCallBack); virtual; abstract;
    Class Procedure LoadBytes(aURL : String; aSync : Boolean; OnLoaded : TBytesLoadedCallBack; OnError : TErrorCallBack); virtual; abstract;
  end;

  TLoadHelperClass = Class of TLoadHelper;

type
  TIdentMapEntry = record
    Value: Integer;
    Name: String;
  end;

  TIdentToInt = function(const Ident: string; var Int: Longint): Boolean;
  TIntToIdent = function(Int: Longint; var Ident: string): Boolean;
  TFindGlobalComponent = function(const Name: string): TComponent;
  TInitComponentHandler = function(Instance: TComponent; RootAncestor : TClass): boolean;

procedure RegisterInitComponentHandler(ComponentClass: TComponentClass;   Handler: TInitComponentHandler);
Procedure RegisterClass(AClass : TPersistentClass);
Procedure RegisterClasses(AClasses : specialize TArray<TPersistentClass>);
Function GetClass(AClassName : string) : TPersistentClass;
procedure RegisterFindGlobalComponentProc(AFindGlobalComponent: TFindGlobalComponent);
procedure UnregisterFindGlobalComponentProc(AFindGlobalComponent: TFindGlobalComponent);
function FindGlobalComponent(const Name: string): TComponent;
Function FindNestedComponent(Root : TComponent; APath : String; CStyle : Boolean = True) : TComponent;
procedure RedirectFixupReferences(Root: TComponent; const OldRootName, NewRootName: string);
procedure RemoveFixupReferences(Root: TComponent; const RootName: string);
procedure RegisterIntegerConsts(IntegerType: Pointer; IdentToIntFn: TIdentToInt;  IntToIdentFn: TIntToIdent);
function ExtractStrings(Separators, WhiteSpace: TSysCharSet; Content: String; Strings: TStrings; AddEmptyStrings : Boolean = False): Integer;
function IdentToInt(const Ident: string; out Int: Longint; const Map: array of TIdentMapEntry): Boolean;
function IntToIdent(Int: Longint; var Ident: string; const Map: array of TIdentMapEntry): Boolean;
function FindIntToIdent(AIntegerType: Pointer): TIntToIdent;
function FindIdentToInt(AIntegerType: Pointer): TIdentToInt;
function FindClass(const AClassName: string): TPersistentClass;
function CollectionsEqual(C1, C2: TCollection): Boolean;
function CollectionsEqual(C1, C2: TCollection; Owner1, Owner2: TComponent): Boolean;
procedure GetFixupReferenceNames(Root: TComponent; Names: TStrings);
procedure GetFixupInstanceNames(Root: TComponent; const ReferenceRootName: string; Names: TStrings);
procedure ObjectBinaryToText(aInput, aOutput: TStream);
procedure ObjectBinaryToText(aInput, aOutput: TStream; aEncoding: TObjectTextEncoding);
procedure ObjectTextToBinary(aInput, aOutput: TStream);
Function SetLoadHelperClass(aClass : TLoadHelperClass) : TLoadHelperClass;
// Create buffer from string. aLen in bytes, not in characters
Function StringToBuffer(aString : String; aLen : Integer) : TJSArrayBuffer;
// Create buffer from string. aPos,aLen are in bytes, not in characters.
Function BufferToString(aBuffer : TJSArrayBuffer; aPos,aLen : Integer) : String;

Const
  // Some aliases
  vaSingle = vaDouble;
  vaExtended = vaDouble;
  vaLString = vaString;
  vaUTF8String = vaString;
  vaUString = vaString;
  vaWString = vaString;
  vaQWord = vaNativeInt;
  vaInt64 = vaNativeInt;
  toWString = toString;

implementation

uses simplelinkedlist;

var
  GlobalLoaded,
  IntConstList: TFPList;
  GlobalLoadHelper : TLoadHelperClass;

Function SetLoadHelperClass(aClass : TLoadHelperClass) : TLoadHelperClass;

begin
  Result:=GlobalLoadHelper;
  GlobalLoadHelper:=aClass;
end;

Procedure CheckLoadHelper;

begin
  If (GlobalLoadHelper=Nil) then
    Raise EInOutError.Create('No support for loading URLS. Include Rtl.BrowserLoadHelper in your project uses clause');
end;

Function StringToBuffer(aString : String; aLen : Integer) : TJSArrayBuffer;

var
   I : Integer;

begin
  Result:=TJSArrayBuffer.new(aLen*2);// 2 bytes for each char
  With TJSUint16Array.new(Result) do
    for i:=0 to aLen-1 do
      values[i] := TJSString(aString).charCodeAt(i);
end;

function BufferToString(aBuffer: TJSArrayBuffer; aPos, aLen: Integer): String;

var
  a : TJSUint16Array;

begin
  Result:=''; // Silence warning
  a:=TJSUint16Array.New(aBuffer.slice(aPos,aLen));
  if a<>nil then
    Result:=String(TJSFunction(@TJSString.fromCharCode).apply(nil,TJSValueDynArray(JSValue(a))));
end;


type
  TIntConst = class
  Private
    IntegerType: PTypeInfo;             // The integer type RTTI pointer
    IdentToIntFn: TIdentToInt;          // Identifier to Integer conversion
    IntToIdentFn: TIntToIdent;          // Integer to Identifier conversion
  Public
    constructor Create(AIntegerType: PTypeInfo; AIdentToInt: TIdentToInt;
      AIntToIdent: TIntToIdent);
  end;

{ TStringStream }

function TStringStream.GetDataString: String;
var
  a : TJSUint16Array;
begin
  Result:=''; // Silence warning
  a:=TJSUint16Array.New(Memory.slice(0,Size));
  if a<>nil then
    asm
      // Result=String.fromCharCode.apply(null, new Uint16Array(a));
      Result=String.fromCharCode.apply(null, a);
    end;
end;

constructor TStringStream.Create;
begin
  Create('');
end;

constructor TStringStream.Create(const aString: String);

var
  Len : Integer;

begin
  inherited Create;
  Len:=Length(aString);
  SetPointer(StringToBuffer(aString,Len),Len*2);
  FCapacity:=Len*2;
end;

function TStringStream.ReadString(Count: Integer): string;


Var
  B : TBytes;
  Buf : TJSArrayBuffer;
  BytesLeft : Integer;
  ByteCount : Integer;

begin
  // Top off
  ByteCount:=Count*2; // UTF-16
  BytesLeft:=(Size-Position);
  if BytesLeft<ByteCount then
    ByteCount:=BytesLeft;
  SetLength(B,ByteCount);
  ReadBuffer(B,0,ByteCount);
  Buf:=BytesToMemory(B);
  Result:=BufferToString(Buf,0,ByteCount);
end;

procedure TStringStream.WriteString(const AString: string);

Var
  Buf : TJSArrayBuffer;
  B : TBytes;

begin
  Buf:=StringToBuffer(aString,Length(aString));
  B:=MemoryToBytes(Buf);
  WriteBuffer(B,Length(B));
end;

constructor TIntConst.Create(AIntegerType: PTypeInfo; AIdentToInt: TIdentToInt;
  AIntToIdent: TIntToIdent);
begin
  IntegerType := AIntegerType;
  IdentToIntFn := AIdentToInt;
  IntToIdentFn := AIntToIdent;
end;

procedure RegisterIntegerConsts(IntegerType: Pointer; IdentToIntFn: TIdentToInt;
  IntToIdentFn: TIntToIdent);
begin
  if Not Assigned(IntConstList) then
    IntConstList:=TFPList.Create;
  IntConstList.Add(TIntConst.Create(IntegerType, IdentToIntFn, IntToIdentFn));
end;

function ExtractStrings(Separators, WhiteSpace: TSysCharSet; Content: String; Strings: TStrings; AddEmptyStrings : Boolean = False): Integer;
var
  b,c : integer;

  procedure SkipWhitespace;
    begin
      while (Content[c] in Whitespace) do
        inc (C);
    end;

  procedure AddString;
    var
      l : integer;

    begin
      l := c-b;
      if (l > 0) or AddEmptyStrings then
        begin
          if assigned(Strings) then
            begin
            if l>0 then
              Strings.Add (Copy(Content,B,L))
            else
              Strings.Add('');
            end;
          inc (result);
        end;
    end;

var
  cc,quoted : char;
  aLen : Integer;
begin
  result := 0;
  c := 1;
  Quoted := #0;
  Separators := Separators + [#13, #10] - ['''','"'];
  SkipWhitespace;
  b := c;
  aLen:=Length(Content);
  while C<=aLen do
    begin
      CC:=Content[c];
      if (CC = Quoted) then
        begin
          if (C<aLen) and (Content[C+1] = Quoted) then
            inc (c)
          else
            Quoted := #0
        end
      else if (Quoted = #0) and (CC in ['''','"']) then
        Quoted := CC;
      if (Quoted = #0) and (CC in Separators) then
        begin
          AddString;
          inc (c);
          SkipWhitespace;
          b := c;
        end
      else
        inc (c);
    end;
  if (c <> b) then
    AddString;
end;


function FindIntToIdent(AIntegerType: Pointer): TIntToIdent;

var
  i: Integer;

begin
  Result := nil;
  if Not Assigned(IntConstList) then
    exit;
  with IntConstList do
    for i := 0 to Count - 1 do
      if TIntConst(Items[i]).IntegerType = AIntegerType then
        exit(TIntConst(Items[i]).IntToIdentFn);
end;

function FindIdentToInt(AIntegerType: Pointer): TIdentToInt;
var
  i: Integer;
begin
  Result := nil;
  if Not Assigned(IntConstList) then
    exit;
  with IntConstList do
    for i := 0 to Count - 1 do
      with TIntConst(Items[I]) do
        if TIntConst(Items[I]).IntegerType = AIntegerType then
          exit(IdentToIntFn);
end;

function IdentToInt(const Ident: String; out Int: LongInt;
  const Map: array of TIdentMapEntry): Boolean;
var
  i: Integer;
begin
  for i := Low(Map) to High(Map) do
    if CompareText(Map[i].Name, Ident) = 0 then
    begin
      Int := Map[i].Value;
      exit(True);
    end;
  Result := False;
end;

function IntToIdent(Int: LongInt; var Ident: String;
  const Map: array of TIdentMapEntry): Boolean;
var
  i: Integer;
begin
  for i := Low(Map) to High(Map) do
    if Map[i].Value = Int then
      begin
      Ident := Map[i].Name;
      exit(True);
      end;
  Result := False;
end;

function GlobalIdentToInt(const Ident: String; var Int: LongInt):boolean;
var
  i : Integer;
begin
  Result := false;
  if Not Assigned(IntConstList) then
    exit;
  with IntConstList do
    for i := 0 to Count - 1 do
      if TIntConst(Items[I]).IdentToIntFn(Ident, Int) then
        Exit(True);
end;

function FindClass(const AClassName: string): TPersistentClass;

begin
  Result := GetClass(AClassName);
  if not Assigned(Result) then
    raise EClassNotFound.CreateFmt(SClassNotFound, [AClassName]);
end;


function CollectionsEqual(C1, C2: TCollection): Boolean;

Var
  Comp1,Comp2 : TComponent;

begin
  Comp2:=Nil;
  Comp1:=TComponent.Create;
  try
    Result:=CollectionsEqual(C1,C2,Comp1,Comp2);
  finally
    Comp1.Free;
    Comp2.Free;
  end;
end;

function CollectionsEqual(C1, C2: TCollection; Owner1, Owner2: TComponent): Boolean;

  procedure stream_collection(s : tstream;c : tcollection;o : tcomponent);
    var
      w : twriter;
    begin
      w:=twriter.create(s);
      try
        w.root:=o;
        w.flookuproot:=o;
        w.writecollection(c);
      finally
        w.free;
      end;
    end;

  var
    s1,s2 : tbytesstream;
    b1,b2 : TBytes;
    I,Len : Integer;
  begin
    result:=false;
    if (c1.classtype<>c2.classtype) or
      (c1.count<>c2.count) then
      exit;
    if c1.count = 0 then
      begin
      result:= true;
      exit;
      end;
    s2:=Nil;
    s1:=tbytesstream.create;
    try
      s2:=tbytesstream.create;
      stream_collection(s1,c1,owner1);
      stream_collection(s2,c2,owner2);
      result:=(s1.size=s2.size);
      if Result then
        begin
        b1:=S1.Bytes;
        b2:=S2.Bytes;
        I:=0;
        Len:=S1.Size; // Not length of B
        While Result and (I<Len) do
           begin
           Result:=b1[I]=b2[i];
           Inc(i);
           end;
        end;
    finally
      s2.free;
      s1.free;
    end;
  end;




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

function TInterfacedPersistent.QueryInterface(const IID: TGUID; out Obj): HRESULT;
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
                    const Compare: TListSortCompareFunc
                    );
var
  I, J, PivotIdx : SizeUInt;
  P, Q : JSValue;
begin
 repeat
   I := L;
   J := R;
   PivotIdx := L + ((R - L) shr 1); { same as ((L + R) div 2), but without the possibility of overflow }
   P := aList[PivotIdx];
   repeat
     while (I < PivotIdx) and (Compare(P, aList[i]) > 0) do
       Inc(I);
     while (J > PivotIdx) and (Compare(P, aList[J]) < 0) do
       Dec(J);
     if I < J then
     begin
       Q := aList[I];
       aList[I] := aList[J];
       aList[J] := Q;
       if PivotIdx = I then
       begin
         PivotIdx := J;
         Inc(I);
       end
       else if PivotIdx = J then
       begin
         PivotIdx := I;
         Dec(J);
       end
       else
       begin
         Inc(I);
         Dec(J);
       end;
     end;
   until I >= J;
   // sort the smaller range recursively
   // sort the bigger range via the loop
   // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
   if (PivotIdx - L) < (R - PivotIdx) then
   begin
     if (L + 1) < PivotIdx then
       QuickSort(aList, L, PivotIdx - 1, Compare);
     L := PivotIdx + 1;
   end
   else
   begin
     if (PivotIdx + 1) < R then
       QuickSort(aList, PivotIdx + 1, R, Compare);
     if (L + 1) < PivotIdx then
       R := PivotIdx - 1
     else
       exit;
   end;
 until L >= R;
end;

(*
Procedure QuickSort(aList: TJSValueDynArray; L, R : Longint;
                    const Compare: TListSortCompareFunc);
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
*)

procedure TFPList.Sort(const Compare: TListSortCompare);
begin
  if Not Assigned(FList) or (FCount < 2) then exit;
  QuickSort(Flist, 0, FCount-1,
    function(Item1, Item2: JSValue): Integer
    begin
      Result := Compare(Item1, Item2);
    end);
end;

procedure TFPList.SortList(const Compare: TListSortCompareFunc);
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

procedure TList.SortList(const Compare: TListSortCompareFunc);
begin
  FList.SortList(Compare);
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

procedure TPersistent.DefineProperties(Filer: TFiler);
begin
  if Filer=Nil then exit;
  // Do nothing
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

procedure TStrings.CheckSpecialChars;

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

function TStrings.GetSkipLastLineBreak: Boolean;

begin
  CheckSpecialChars;
  Result:=FSkipLastLineBreak;
end;

procedure TStrings.SetSkipLastLineBreak(const AValue : Boolean);

begin
  CheckSpecialChars;
  FSkipLastLineBreak:=AValue;
end;

procedure TStrings.ReadData(Reader: TReader);
begin
  Reader.ReadListBegin;
  BeginUpdate;
  try
    Clear;
    while not Reader.EndOfList do
      Add(Reader.ReadString);
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;

procedure TStrings.WriteData(Writer: TWriter);
var
  i: Integer;
begin
  Writer.WriteListBegin;
  for i := 0 to Count - 1 do
    Writer.WriteString(Strings[i]);
  Writer.WriteListEnd;
end;

procedure TStrings.DefineProperties(Filer: TFiler);
var
  HasData: Boolean;
begin
  if Assigned(Filer.Ancestor) then
    // Only serialize if string list is different from ancestor
    if Filer.Ancestor.InheritsFrom(TStrings) then
      HasData := not Equals(TStrings(Filer.Ancestor))
    else
      HasData := True
  else
    HasData := Count > 0;
  Filer.DefineProperty('Strings', @ReadData, @WriteData, HasData);
end;

function TStrings.GetLBS: TTextLineBreakStyle;
begin
  CheckSpecialChars;
  Result:=FLBS;
end;

procedure TStrings.SetLBS(AValue: TTextLineBreakStyle);
begin
  CheckSpecialChars;
  FLBS:=AValue;
end;

procedure TStrings.SetDelimiter(c:Char);
begin
  CheckSpecialChars;
  FDelimiter:=c;
end;

function TStrings.GetDelimiter: Char;
begin
  CheckSpecialChars;
  Result:=FDelimiter;
end;

procedure TStrings.SetLineBreak(const S: String);
begin
  CheckSpecialChars;
  FLineBreak:=S;
end;

function TStrings.GetLineBreak: String;
begin
  CheckSpecialChars;
  Result:=FLineBreak;
end;


procedure TStrings.SetQuoteChar(c:Char);
begin
  CheckSpecialChars;
  FQuoteChar:=c;
end;

function TStrings.GetQuoteChar: Char;
begin
  CheckSpecialChars;
  Result:=FQuoteChar;
end;

procedure TStrings.SetNameValueSeparator(c:Char);
begin
  CheckSpecialChars;
  FNameValueSeparator:=c;
end;

function TStrings.GetNameValueSeparator: Char;
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


function TStrings.GetDelimitedText: string;

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
    doQuote:=FAlwaysQuote or (TJSString(s).search(RE)<>-1);
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

procedure TStrings.GetNameValue(Index: Integer; out AName, AValue: String);

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

procedure TStrings.LoadFromURL(const aURL: String; Async: Boolean; OnLoaded: TNotifyEventRef; OnError: TStringNotifyEventRef);

  procedure DoLoaded(const aString : String);
  begin
    Text:=aString;
    if Assigned(OnLoaded) then
      OnLoaded(Self);
  end;

  procedure DoError(const AError : String);
  begin
    if Assigned(OnError) then
      OnError(Self,aError)
    else
      Raise EInOutError.Create('Failed to load from URL:'+aError);
  end;

begin
  CheckLoadHelper;
  GlobalLoadHelper.LoadText(aURL,aSync,@DoLoaded,@DoError);
end;

procedure TStrings.LoadFromFile(const aFileName: String; const OnLoaded: TProc; const AError: TProcString);

begin
  LoadFromURL(aFileName,False,
  Procedure (Sender : TObject)
  begin
    If Assigned(OnLoaded) then
     OnLoaded
  end,
  Procedure (Sender : TObject; Const ErrorMsg : String)
  begin
    if Assigned(aError) then
      aError(ErrorMsg)
  end);
end;

function TStrings.ExtractName(const S: String): String;
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

function TStrings.GetValue(const Name: string): string;

Var
  L : longint;
  N : String;

begin
  Result:='';
  L:=IndexOfName(Name);
  If L<>-1 then
    GetNameValue(L,N,Result);
end;

function TStrings.GetValueFromIndex(Index: Integer): string;

Var
  N : String;

begin
  GetNameValue(Index,N,Result);
end;

procedure TStrings.SetValueFromIndex(Index: Integer; const Value: string);

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

procedure TStrings.SetDelimitedText(const AValue: string);
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

procedure TStrings.SetCommaText(const Value: string);

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

procedure TStrings.SetValue(const Name: String; const Value: string);

Var L : longint;

begin
  CheckSpecialChars;
  L:=IndexOfName(Name);
  if L=-1 then
   Add (Name+FNameValueSeparator+Value)
  else
   Strings[L]:=Name+FNameValueSeparator+value;
end;


procedure TStrings.Error(const Msg: string; Data: Integer);
begin
  Raise EStringListError.CreateFmt(Msg,[IntToStr(Data)]);
end;

function TStrings.GetCapacity: Integer;

begin
  Result:=Count;
end;



function TStrings.GetObject(Index: Integer): TObject;

begin
  if Index=0 then ;
  Result:=Nil;
end;

function TStrings.GetTextStr: string;

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



procedure TStrings.Put(Index: Integer; const S: string);

Var Obj : TObject;

begin
  Obj:=Objects[Index];
  Delete(Index);
  InsertObject(Index,S,Obj);
end;



procedure TStrings.PutObject(Index: Integer; AObject: TObject);

begin
  // Empty.
  if Index=0 then exit;
  if AObject=nil then exit;
end;



procedure TStrings.SetCapacity(NewCapacity: Integer);

begin
  // Empty.
  if NewCapacity=0 then ;
end;

function TStrings.GetNextLinebreak(const Value: String; out S: String; var P: Integer): Boolean;

var
  PPLF,PPCR,PP,PL: Integer;

begin
  S:='';
  Result:=False;
  If ((Length(Value)-P)<0) then
    Exit;
  PPLF:=TJSString(Value).IndexOf(#10,P-1)+1;
  PPCR:=TJSString(Value).IndexOf(#13,P-1)+1;
  PL:=1;
  if (PPLF>0) and (PPCR>0) then
    begin
    if (PPLF-PPCR)=1 then 
      PL:=2;
    if PPLF<PPCR then
      PP:=PPLF
    else
      PP:=PPCR;
    end
  else if (PPLF>0) and (PPCR<1) then
    PP:=PPLF
  else if (PPCR > 0) and (PPLF<1) then
    PP:=PPCR
  else 
    PP:=Length(Value)+1;
  S:=Copy(Value,P,PP-P);
  P:=PP+PL;
  Result:=True;
end;

procedure TStrings.DoSetTextStr(const Value: string; DoClear: Boolean);

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

procedure TStrings.SetTextStr(const Value: string);

begin
  CheckSpecialChars;
  DoSetTextStr(Value,True);
end;

procedure TStrings.AddText(const S: String);

begin
  CheckSpecialChars;
  DoSetTextStr(S,False);
end;

procedure TStrings.SetUpdateState(Updating: Boolean);

begin
  // FPONotifyObservers(Self,ooChange,Nil);
  if Updating then ;
end;

destructor TStrings.Destroy;

begin
  inherited destroy;
end;

constructor TStrings.Create;
begin
  inherited Create;
  FAlwaysQuote:=False;
end;

function TStrings.ToObjectArray: TObjectDynArray;

begin
  Result:=ToObjectArray(0,Count-1);
end;

function TStrings.ToObjectArray(aStart,aEnd : Integer): TObjectDynArray;
Var
  I : Integer;

begin
  Result:=Nil;
  if aStart>aEnd then exit;
  SetLength(Result,aEnd-aStart+1);
  For I:=aStart to aEnd do
    Result[i-aStart]:=Objects[i];
end;

function TStrings.ToStringArray: TStringDynArray;

begin
  Result:=ToStringArray(0,Count-1);
end;

function TStrings.ToStringArray(aStart,aEnd : Integer): TStringDynArray;

Var
  I : Integer;

begin
  Result:=Nil;
  if aStart>aEnd then exit;
  SetLength(Result,aEnd-aStart+1);
  For I:=aStart to aEnd do
    Result[i-aStart]:=Strings[i];
end;

function TStrings.Add(const S: string): Integer;

begin
  Result:=Count;
  Insert (Count,S);
end;


function TStrings.Add(const Fmt: string; const Args: array of const): Integer;

begin
  Result:=Add(Format(Fmt,Args));
end;

function TStrings.AddFmt(const Fmt: string; const Args: array of const): Integer;

begin
  Result:=Add(Format(Fmt,Args));
end;


function TStrings.AddObject(const S: string; AObject: TObject): Integer;

begin
  Result:=Add(S);
  Objects[result]:=AObject;
end;

function TStrings.AddObject(const Fmt: string; Args: array of const; AObject: TObject): Integer;

begin
  Result:=AddObject(Format(Fmt,Args),AObject);
end;

procedure TStrings.Append(const S: string);

begin
  Add (S);
end;



procedure TStrings.AddStrings(TheStrings: TStrings; ClearFirst: Boolean);


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

procedure TStrings.AddStrings(TheStrings: TStrings);

Var Runner : longint;
begin
  For Runner:=0 to TheStrings.Count-1 do
    self.AddObject (Thestrings[Runner],TheStrings.Objects[Runner]);
end;

procedure TStrings.AddStrings(const TheStrings: array of string);

Var Runner : longint;
begin
  if Count + High(TheStrings)+1 > Capacity then
    Capacity := Count + High(TheStrings)+1;
  For Runner:=Low(TheStrings) to High(TheStrings) do
    self.Add(Thestrings[Runner]);
end;


procedure TStrings.AddStrings(const TheStrings: array of string; ClearFirst: Boolean);

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


procedure TStrings.Assign(Source: TPersistent);

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



procedure TStrings.BeginUpdate;

begin
   if FUpdateCount = 0 then SetUpdateState(true);
   inc(FUpdateCount);
end;



procedure TStrings.EndUpdate;

begin
  If FUpdateCount>0 then
     Dec(FUpdateCount);
  if FUpdateCount=0 then
    SetUpdateState(False);
end;



function TStrings.Equals(Obj: TObject): Boolean;

begin
  if Obj is TStrings then
    Result := Equals(TStrings(Obj))
  else
    Result := inherited Equals(Obj);
end;



function TStrings.Equals(TheStrings: TStrings): Boolean;

Var Runner,Nr : Longint;

begin
  Result:=False;
  Nr:=Self.Count;
  if Nr<>TheStrings.Count then exit;
  For Runner:=0 to Nr-1 do
    If Strings[Runner]<>TheStrings[Runner] then exit;
  Result:=True;
end;



procedure TStrings.Exchange(Index1, Index2: Integer);

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


function TStrings.DoCompareText(const s1, s2: string): PtrInt;
begin
  result:=CompareText(s1,s2);
end;


function TStrings.IndexOf(const S: string): Integer;
begin
  Result:=0;
  While (Result<Count) and (DoCompareText(Strings[Result],S)<>0) do Result:=Result+1;
  if Result=Count then Result:=-1;
end;


function TStrings.IndexOfName(const Name: string): Integer;
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


function TStrings.IndexOfObject(AObject: TObject): Integer;
begin
  Result:=0;
  While (Result<count) and (Objects[Result]<>AObject) do Result:=Result+1;
  If Result=Count then Result:=-1;
end;


procedure TStrings.InsertObject(Index: Integer; const S: string; AObject: TObject);

begin
  Insert (Index,S);
  Objects[Index]:=AObject;
end;

procedure TStrings.Move(CurIndex, NewIndex: Integer);
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
  if Assigned(FCollection) then
    Result:=FCollection.FItems.IndexOf(Self)
  else
    Result:=-1;
end;



procedure TCollectionItem.SetCollection(Value: TCollection);

begin
  IF Value<>FCollection then
    begin
    if Assigned(FCollection) then FCollection.RemoveItem(Self);
    if Assigned(Value) then Value.InsertItem(Self);
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

procedure TCollection.SortList(const Compare: TCollectionSortCompareFunc);

begin
  BeginUpdate;
  try
    FItems.SortList(TListSortCompareFunc(Compare));
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


function TComponent.GetComponent(AIndex: Integer): TComponent;

begin
  If not assigned(FComponents) then
    Result:=Nil
  else
    Result:=TComponent(FComponents.Items[Aindex]);
end;

function TComponent.GetComponentCount: Integer;

begin
  If not assigned(FComponents) then
    result:=0
  else
    Result:=FComponents.Count;
end;


function TComponent.GetComponentIndex: Integer;

begin
  If Assigned(FOwner) and Assigned(FOwner.FComponents) then
    Result:=FOWner.FComponents.IndexOf(Self)
  else
    Result:=-1;
end;


procedure TComponent.Insert(AComponent: TComponent);

begin
  If not assigned(FComponents) then
    FComponents:=TFpList.Create;
  FComponents.Add(AComponent);
  AComponent.FOwner:=Self;
end;


procedure TComponent.ReadLeft(AReader: TReader);

begin
  FDesignInfo := (FDesignInfo and $ffff0000) or (AReader.ReadInteger and $ffff);
end;


procedure TComponent.ReadTop(AReader: TReader);

begin
  FDesignInfo := ((AReader.ReadInteger and $ffff) shl 16) or (FDesignInfo and $ffff);
end;


procedure TComponent.Remove(AComponent: TComponent);

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


procedure TComponent.RemoveNotification(AComponent: TComponent);

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


procedure TComponent.SetComponentIndex(Value: Integer);

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


procedure TComponent.ChangeName(const NewName: TComponentName);

begin
  FName:=NewName;
end;


procedure TComponent.DefineProperties(Filer: TFiler);

var
  Temp: LongInt;
  Ancestor: TComponent;
begin
  Ancestor := TComponent(Filer.Ancestor);
  if Assigned(Ancestor) then
    Temp := Ancestor.FDesignInfo
  else
    Temp := 0;
  Filer.DefineProperty('Left', @ReadLeft, @WriteLeft, (FDesignInfo and $ffff) <> (Temp and $ffff));
  Filer.DefineProperty('Top', @ReadTop, @WriteTop, (FDesignInfo and $ffff0000) <> (Temp and $ffff0000));
end;


procedure TComponent.GetChildren(Proc: TGetChildProc; Root: TComponent);

begin
  // Does nothing.
  if Proc=nil then ;
  if Root=nil then ;
end;


function TComponent.GetChildOwner: TComponent;

begin
 Result:=Nil;
end;


function TComponent.GetChildParent: TComponent;

begin
  Result:=Self;
end;


function TComponent.GetNamePath: string;

begin
  Result:=FName;
end;


function TComponent.GetOwner: TPersistent;

begin
  Result:=FOwner;
end;


procedure TComponent.Loaded;

begin
  Exclude(FComponentState,csLoading);
end;

procedure TComponent.Loading;

begin
  Include(FComponentState,csLoading);
end;

procedure TComponent.SetWriting(Value: Boolean);
begin
  If Value then
    Include(FComponentState,csWriting)
  else
    Exclude(FComponentState,csWriting);
end;

procedure TComponent.SetReading(Value: Boolean);
begin
  If Value then
    Include(FComponentState,csReading)
  else
    Exclude(FComponentState,csReading);
end;


procedure TComponent.Notification(AComponent: TComponent; Operation: TOperation);

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

procedure TComponent.ReadState(Reader: TReader);

begin
  Reader.ReadData(Self);
end;


procedure TComponent.SetAncestor(Value: Boolean);

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


procedure TComponent.SetDesigning(Value: Boolean; SetChildren: Boolean);

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

procedure TComponent.SetDesignInstance(Value: Boolean);

begin
  If Value then
    Include(FComponentState,csDesignInstance)
  else
    Exclude(FComponentState,csDesignInstance);
end;

procedure TComponent.SetInline(Value: Boolean);

begin
  If Value then
    Include(FComponentState,csInline)
  else
    Exclude(FComponentState,csInline);
end;


procedure TComponent.SetName(const NewName: TComponentName);

begin
  If FName=NewName then exit;
  If (NewName<>'') and not IsValidIdent(NewName) then
    Raise EComponentError.CreateFmt(SInvalidName,[NewName]);
  If Assigned(FOwner) Then
    FOwner.ValidateRename(Self,FName,NewName)
  else
    ValidateRename(Nil,FName,NewName);
  SetReference(False);
  ChangeName(NewName);
  SetReference(True);
end;


procedure TComponent.SetChildOrder(Child: TComponent; Order: Integer);

begin
  // does nothing
  if Child=nil then ;
  if Order=0 then ;
end;


procedure TComponent.SetParentComponent(Value: TComponent);

begin
  // Does nothing
  if Value=nil then ;
end;


procedure TComponent.Updating;

begin
  Include (FComponentState,csUpdating);
end;


procedure TComponent.Updated;

begin
  Exclude(FComponentState,csUpdating);
end;


procedure TComponent.ValidateRename(AComponent: TComponent; const CurName, NewName: string);

begin
//!! This contradicts the Delphi manual.
  If (AComponent<>Nil) and (CompareText(CurName,NewName)<>0) and (AComponent.Owner = Self) and
     (FindComponent(NewName)<>Nil) then
      raise EComponentError.Createfmt(SDuplicateName,[newname]);
  If (csDesigning in FComponentState) and (FOwner<>Nil) then
    FOwner.ValidateRename(AComponent,Curname,Newname);
end;

Procedure TComponent.SetReference(Enable: Boolean);

var
  aField, aValue, aOwner : Pointer;

begin
  if Name='' then
    exit;
  if Assigned(Owner) then
  begin
    aOwner:=Owner; // so as not to depend on low-level names
    aField := Owner.FieldAddress(Name);
    if Assigned(aField) then
      begin
      if Enable then
        aValue:= Self
      else
        aValue := nil;
      TJSObject(aOwner)[String(TJSObject(aField)['name'])]:=aValue;
      end;
  end;
end;


procedure TComponent.WriteLeft(AWriter: TWriter);

begin
  AWriter.WriteInteger(FDesignInfo and $ffff);
end;


procedure TComponent.WriteTop(AWriter: TWriter);

begin
  AWriter.WriteInteger((FDesignInfo shr 16) and $ffff);
end;


procedure TComponent.ValidateContainer(AComponent: TComponent);

begin
  AComponent.ValidateInsert(Self);
end;


procedure TComponent.ValidateInsert(AComponent: TComponent);

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


constructor TComponent.Create(AOwner: TComponent);

begin
  FComponentStyle:=[csInheritable];
  If Assigned(AOwner) then AOwner.InsertComponent(Self);
end;


destructor TComponent.Destroy;

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


procedure TComponent.BeforeDestruction;
begin
  if not(csDestroying in FComponentstate) then
    Destroying;
end;


procedure TComponent.DestroyComponents;

Var acomponent: TComponent;

begin
  While assigned(FComponents) do
    begin
    aComponent:=TComponent(FComponents.Last);
    Remove(aComponent);
    Acomponent.Destroy;
    end;
end;


procedure TComponent.Destroying;

Var Runner : longint;

begin
  If csDestroying in FComponentstate Then Exit;
  include (FComponentState,csDestroying);
  If Assigned(FComponents) then
    for Runner:=0 to FComponents.Count-1 do
      TComponent(FComponents.Items[Runner]).Destroying;
end;

function TComponent.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;

end;

procedure TComponent.WriteState(Writer: TWriter);
begin
  Writer.WriteComponentData(Self);
end;


function TComponent.FindComponent(const AName: string): TComponent;

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


procedure TComponent.FreeNotification(AComponent: TComponent);

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


function TComponent.GetParentComponent: TComponent;

begin
  Result:=Nil;
end;


function TComponent.HasParent: Boolean;

begin
  Result:=False;
end;


procedure TComponent.InsertComponent(AComponent: TComponent);

begin
  AComponent.ValidateContainer(Self);
  ValidateRename(AComponent,'',AComponent.FName);
  if AComponent.FOwner <> nil then
    AComponent.FOwner.RemoveComponent(AComponent);
  Insert(AComponent);
  If csDesigning in FComponentState then
    AComponent.SetDesigning(true);
  Notification(AComponent,opInsert);
end;


procedure TComponent.RemoveComponent(AComponent: TComponent);

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
  TStream
  ---------------------------------------------------------------------}

Resourcestring
  SStreamInvalidSeek = 'Seek is not implemented for class %s';
  SStreamNoReading = 'Stream reading is not implemented for class %s';
  SStreamNoWriting = 'Stream writing is not implemented for class %s';
  SReadError = 'Could not read data from stream';
  SWriteError = 'Could not write data to stream';
  SMemoryStreamError = 'Could not allocate memory';
  SerrInvalidStreamSize = 'Invalid Stream size';

procedure TStream.ReadNotImplemented;
begin
  raise EStreamError.CreateFmt(SStreamNoReading, [ClassName]);
end;

procedure TStream.WriteNotImplemented;
begin
  raise EStreamError.CreateFmt(SStreamNoWriting, [ClassName]);
end;

function TStream.Read(var Buffer: TBytes; Count: Longint): Longint;
begin
  Result:=Read(Buffer,0,Count);
end;



function TStream.Write(const Buffer: TBytes; Count: Longint): Longint;
begin
  Result:=Self.Write(Buffer,0,Count);
end;



function TStream.GetPosition: NativeInt;

begin
   Result:=Seek(0,soCurrent);
end;

procedure TStream.SetPosition(const Pos: NativeInt);

begin
   Seek(pos,soBeginning);
end;

procedure TStream.SetSize64(const NewSize: NativeInt);

begin
  // Required because can't use overloaded functions in properties
  SetSize(NewSize);
end;

function TStream.GetSize: NativeInt;

var
   p : NativeInt;

begin
   p:=Seek(0,soCurrent);
   GetSize:=Seek(0,soEnd);
   Seek(p,soBeginning);
end;

procedure TStream.SetSize(const NewSize: NativeInt);

begin
  if NewSize<0 then
    Raise EStreamError.Create(SerrInvalidStreamSize);
end;

procedure TStream.Discard(const Count: NativeInt);

const
  CSmallSize      =255;
  CLargeMaxBuffer =32*1024; // 32 KiB

var
  Buffer: TBytes;

begin
  if Count=0 then
    Exit;
  if (Count<=CSmallSize) then
    begin
    SetLength(Buffer,CSmallSize);
    ReadBuffer(Buffer,Count)
    end
  else
    DiscardLarge(Count,CLargeMaxBuffer);
end;

procedure TStream.DiscardLarge(Count: NativeInt; const MaxBufferSize: Longint);

var
  Buffer: TBytes;

begin
  if Count=0 then
     Exit;
  if Count>MaxBufferSize then
    SetLength(Buffer,MaxBufferSize)
  else
    SetLength(Buffer,Count);
  while (Count>=Length(Buffer)) do
    begin
    ReadBuffer(Buffer,Length(Buffer));
    Dec(Count,Length(Buffer));
    end;
  if Count>0 then
    ReadBuffer(Buffer,Count);
end;

procedure TStream.InvalidSeek;

begin
  raise EStreamError.CreateFmt(SStreamInvalidSeek, [ClassName]);
end;

procedure TStream.FakeSeekForward(Offset: NativeInt;  const Origin: TSeekOrigin; const Pos: NativeInt);

begin
  if Origin=soBeginning then
     Dec(Offset,Pos);
  if (Offset<0) or (Origin=soEnd) then
    InvalidSeek;
  if Offset>0 then
    Discard(Offset);
 end;

function TStream.ReadData({var} Buffer: TBytes; Count: NativeInt): NativeInt;
begin
 Result:=Read(Buffer,0,Count);
end;


function TStream.ReadMaxSizeData(Buffer : TBytes; aSize,aCount : NativeInt) : NativeInt;

Var
  CP : NativeInt;

begin
  if aCount<=aSize then
    Result:=read(Buffer,aCount)
  else
    begin
    Result:=Read(Buffer,aSize);
    CP:=Position;
    Result:=Result+Seek(aCount-aSize,soCurrent)-CP;
    end
end;

function TStream.WriteMaxSizeData(const Buffer : TBytes; aSize,aCount : NativeInt) : NativeInt;
Var
  CP : NativeInt;

begin
  if aCount<=aSize then
    Result:=Self.Write(Buffer,aCount)
  else
    begin
    Result:=Self.Write(Buffer,aSize);
    CP:=Position;
    Result:=Result+Seek(aCount-aSize,soCurrent)-CP;
    end
end;

procedure TStream.WriteExactSizeData(const Buffer : TBytes; aSize, aCount: NativeInt);
begin
  // Embarcadero docs mentions no exception. Does not seem very logical
  WriteMaxSizeData(Buffer,aSize,ACount);
end;

procedure TStream.ReadExactSizeData(Buffer : TBytes; aSize, aCount: NativeInt);
begin
  if ReadMaxSizeData(Buffer,aSize,ACount)<>aCount then
     Raise EReadError.Create(SReadError);
end;


function TStream.ReadData(var Buffer: Boolean): NativeInt;

Var
  B : Byte;

begin
  Result:=ReadData(B,1);
  if Result=1 then
    Buffer:=B<>0;
end;

function TStream.ReadData(var Buffer: Boolean; Count: NativeInt): NativeInt;

Var
  B : TBytes;

begin
  SetLength(B,Count);
  Result:=ReadMaxSizeData(B,1,Count);
  if Result>0 then
    Buffer:=B[0]<>0
end;


function TStream.ReadData(var Buffer: WideChar): NativeInt;
begin
  Result:=ReadData(Buffer,2);
end;

function TStream.ReadData(var Buffer: WideChar; Count: NativeInt): NativeInt;

Var
  W : Word;

begin
  Result:=ReadData(W,Count);
  if Result=2 then
    Buffer:=WideChar(W);
end;

function TStream.ReadData(var Buffer: Int8): NativeInt;
begin
  Result:=ReadData(Buffer,1);
end;

Function TStream.MakeInt(B : TBytes; aSize : Integer; Signed : Boolean) : NativeInt;

Var
  Mem : TJSArrayBuffer;
  A : TJSUInt8Array;
  D : TJSDataView;
  isLittle : Boolean;

begin
  IsLittle:=(Endian=TEndian.Little);
  Mem:=TJSArrayBuffer.New(Length(B));
  A:=TJSUInt8Array.new(Mem);
  A._set(B);
  D:=TJSDataView.New(Mem);
  if Signed then
    case aSize of
      1 : Result:=D.getInt8(0);
      2 : Result:=D.getInt16(0,IsLittle);
      4 : Result:=D.getInt32(0,IsLittle);
      // Todo : fix sign
      8 : Result:=Round(D.getFloat64(0,IsLittle));
    end
  else
    case aSize of
      1 : Result:=D.getUInt8(0);
      2 : Result:=D.getUInt16(0,IsLittle);
      4 : Result:=D.getUInt32(0,IsLittle);
      8 : Result:=Round(D.getFloat64(0,IsLittle));
    end
end;

function TStream.MakeBytes(B: NativeInt; aSize: Integer; Signed: Boolean): TBytes;


Var
  Mem : TJSArrayBuffer;
  A : TJSUInt8Array;
  D : TJSDataView;
  isLittle : Boolean;

begin
  IsLittle:=(Endian=TEndian.Little);
  Mem:=TJSArrayBuffer.New(aSize);
  D:=TJSDataView.New(Mem);
  if Signed then
    case aSize of
      1 : D.setInt8(0,B);
      2 : D.setInt16(0,B,IsLittle);
      4 : D.setInt32(0,B,IsLittle);
      8 : D.setFloat64(0,B,IsLittle);
    end
  else
    case aSize of
      1 : D.SetUInt8(0,B);
      2 : D.SetUInt16(0,B,IsLittle);
      4 : D.SetUInt32(0,B,IsLittle);
      8 : D.setFloat64(0,B,IsLittle);
    end;
  SetLength(Result,aSize);
  A:=TJSUInt8Array.new(Mem);
  Result:=TMemoryStream.MemoryToBytes(A);
end;


function TStream.ReadData(var Buffer: Int8; Count: NativeInt): NativeInt;

Var
  B : TBytes;
begin
  SetLength(B,Count);
  Result:=ReadMaxSizeData(B,1,Count);
  if Result>=1 then
    Buffer:=MakeInt(B,1,True);
end;

function TStream.ReadData(var Buffer: UInt8): NativeInt;
begin
  Result:=ReadData(Buffer,1);
end;

function TStream.ReadData(var Buffer: UInt8; Count: NativeInt): NativeInt;
Var
  B : TBytes;
begin
  SetLength(B,Count);
  Result:=ReadMaxSizeData(B,1,Count);
  if Result>=1 then
    Buffer:=MakeInt(B,1,False);
end;

function TStream.ReadData(var Buffer: Int16): NativeInt;
begin
  Result:=ReadData(Buffer,2);
end;

function TStream.ReadData(var Buffer: Int16; Count: NativeInt): NativeInt;
Var
  B : TBytes;
begin
  SetLength(B,Count);
  Result:=ReadMaxSizeData(B,2,Count);
  if Result>=2 then
    Buffer:=MakeInt(B,2,True);
end;

function TStream.ReadData(var Buffer: UInt16): NativeInt;
begin
  Result:=ReadData(Buffer,2);
end;

function TStream.ReadData(var Buffer: UInt16; Count: NativeInt): NativeInt;
Var
  B : TBytes;
begin
  SetLength(B,Count);
  Result:=ReadMaxSizeData(B,2,Count);
  if Result>=2 then
    Buffer:=MakeInt(B,2,False);
end;

function TStream.ReadData(var Buffer: Int32): NativeInt;
begin
  Result:=ReadData(Buffer,4);
end;

function TStream.ReadData(var Buffer: Int32; Count: NativeInt): NativeInt;
Var
  B : TBytes;
begin
  SetLength(B,Count);
  Result:=ReadMaxSizeData(B,4,Count);
  if Result>=4 then
    Buffer:=MakeInt(B,4,True);
end;

function TStream.ReadData(var Buffer: UInt32): NativeInt;
begin
  Result:=ReadData(Buffer,4);
end;

function TStream.ReadData(var Buffer: UInt32; Count: NativeInt): NativeInt;

Var
  B : TBytes;
begin
  SetLength(B,Count);
  Result:=ReadMaxSizeData(B,4,Count);
  if Result>=4 then
    Buffer:=MakeInt(B,4,False);
end;


function TStream.ReadData(var Buffer: NativeInt): NativeInt;

begin
  Result:=ReadData(Buffer,8);
end;

function TStream.ReadData(var Buffer: NativeInt; Count: NativeInt): NativeInt;

Var
  B : TBytes;

begin
  SetLength(B,Count);
  Result:=ReadMaxSizeData(B,8,8);
  if Result>=8 then
    Buffer:=MakeInt(B,8,True);
end;

function TStream.ReadData(var Buffer: NativeLargeUInt): NativeInt;
begin
  Result:=ReadData(Buffer,8);
end;

function TStream.ReadData(var Buffer: NativeLargeUInt; Count: NativeInt): NativeInt;

Var
  B : TBytes;
  B1 : Integer;
begin
  SetLength(B,Count);
  Result:=ReadMaxSizeData(B,4,4);
  if Result>=4 then
    begin
    B1:=MakeInt(B,4,False);
    Result:=Result+ReadMaxSizeData(B,4,4);
    Buffer:=MakeInt(B,4,False);
    Buffer:=(Buffer shl 32) or B1;
    end;
end;

function TStream.ReadData(var Buffer: Double): NativeInt;
begin
  Result:=ReadData(Buffer,8);
end;

function TStream.ReadData(var Buffer: Double; Count: NativeInt): NativeInt;

Var
  B : TBytes;
  Mem : TJSArrayBuffer;
  A : TJSUInt8Array;
  D : TJSDataView;

begin
  SetLength(B,Count);
  Result:=ReadMaxSizeData(B,8,Count);
  if Result>=8 then
    begin
    Mem:=TJSArrayBuffer.New(8);
    A:=TJSUInt8Array.new(Mem);
    A._set(B);
    D:=TJSDataView.New(Mem);
    Buffer:=D.getFloat64(0);
    end;
end;

procedure TStream.ReadBuffer(var Buffer: TBytes; Count: NativeInt);
begin
  ReadBuffer(Buffer,0,Count);
end;

procedure TStream.ReadBuffer(var Buffer: TBytes; Offset, Count: NativeInt);
begin
  if Read(Buffer,OffSet,Count)<>Count then
    Raise EStreamError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: Boolean);
begin
  ReadBufferData(Buffer,1);
end;

procedure TStream.ReadBufferData(var Buffer: Boolean; Count: NativeInt);
begin
  if (ReadData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: WideChar);
begin
  ReadBufferData(Buffer,2);
end;

procedure TStream.ReadBufferData(var Buffer: WideChar; Count: NativeInt);
begin
  if (ReadData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: Int8);
begin
  ReadBufferData(Buffer,1);
end;

procedure TStream.ReadBufferData(var Buffer: Int8; Count: NativeInt);
begin
  if (ReadData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: UInt8);
begin
  ReadBufferData(Buffer,1);
end;

procedure TStream.ReadBufferData(var Buffer: UInt8; Count: NativeInt);
begin
  if (ReadData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: Int16);
begin
  ReadBufferData(Buffer,2);
end;

procedure TStream.ReadBufferData(var Buffer: Int16; Count: NativeInt);
begin
  if (ReadData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: UInt16);
begin
  ReadBufferData(Buffer,2);
end;

procedure TStream.ReadBufferData(var Buffer: UInt16; Count: NativeInt);
begin
  if (ReadData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: Int32);
begin
  ReadBufferData(Buffer,4);
end;

procedure TStream.ReadBufferData(var Buffer: Int32; Count: NativeInt);
begin
  if (ReadData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: UInt32);
begin
  ReadBufferData(Buffer,4);
end;

procedure TStream.ReadBufferData(var Buffer: UInt32; Count: NativeInt);
begin
  if (ReadData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: NativeLargeInt);
begin
  ReadBufferData(Buffer,8)
end;

procedure TStream.ReadBufferData(var Buffer: NativeLargeInt; Count: NativeInt);
begin
  if (ReadData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: NativeLargeUInt);
begin
  ReadBufferData(Buffer,8);
end;

procedure TStream.ReadBufferData(var Buffer: NativeLargeUInt; Count: NativeInt);
begin
  if (ReadData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SReadError);
end;


procedure TStream.ReadBufferData(var Buffer: Double);
begin
  ReadBufferData(Buffer,8);
end;

procedure TStream.ReadBufferData(var Buffer: Double; Count: NativeInt);
begin
  if (ReadData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SReadError);
end;

procedure TStream.WriteBuffer(const Buffer: TBytes; Count: NativeInt);
begin
  WriteBuffer(Buffer,0,Count);
end;

procedure TStream.WriteBuffer(const Buffer: TBytes; Offset, Count: NativeInt);
begin
  if Self.Write(Buffer,Offset,Count)<>Count then
     Raise EStreamError.Create(SWriteError);
end;

function TStream.WriteData(const Buffer: TBytes; Count: NativeInt): NativeInt;
begin
  Result:=Self.Write(Buffer, 0, Count);
end;

function TStream.WriteData(const Buffer: Boolean): NativeInt;
begin
  Result:=WriteData(Buffer,1);
end;

function TStream.WriteData(const Buffer: Boolean; Count: NativeInt): NativeInt;

Var
  B : Int8;

begin
  B:=Ord(Buffer);
  Result:=WriteData(B,Count);
end;

function TStream.WriteData(const Buffer: WideChar): NativeInt;

begin
  Result:=WriteData(Buffer,2);
end;

function TStream.WriteData(const Buffer: WideChar; Count: NativeInt): NativeInt;
Var
  U : UInt16;
begin
  U:=Ord(Buffer);
  Result:=WriteData(U,Count);
end;

function TStream.WriteData(const Buffer: Int8): NativeInt;

begin
  Result:=WriteData(Buffer,1);
end;

function TStream.WriteData(const Buffer: Int8; Count: NativeInt): NativeInt;
begin
  Result:=WriteMaxSizeData(MakeBytes(Buffer,1,True),1,Count);
end;

function TStream.WriteData(const Buffer: UInt8): NativeInt;
begin
  Result:=WriteData(Buffer,1);
end;

function TStream.WriteData(const Buffer: UInt8; Count: NativeInt): NativeInt;
begin
  Result:=WriteMaxSizeData(MakeBytes(Buffer,1,False),1,Count);
end;

function TStream.WriteData(const Buffer: Int16): NativeInt;
begin
  Result:=WriteData(Buffer,2);
end;

function TStream.WriteData(const Buffer: Int16; Count: NativeInt): NativeInt;
begin
  Result:=WriteMaxSizeData(MakeBytes(Buffer,2,True),2,Count);
end;

function TStream.WriteData(const Buffer: UInt16): NativeInt;
begin
  Result:=WriteData(Buffer,2);
end;

function TStream.WriteData(const Buffer: UInt16; Count: NativeInt): NativeInt;
begin
  Result:=WriteMaxSizeData(MakeBytes(Buffer,2,True),2,Count);
end;

function TStream.WriteData(const Buffer: Int32): NativeInt;
begin
  Result:=WriteData(Buffer,4);
end;

function TStream.WriteData(const Buffer: Int32; Count: NativeInt): NativeInt;
begin
  Result:=WriteMaxSizeData(MakeBytes(Buffer,4,True),4,Count);
end;

function TStream.WriteData(const Buffer: UInt32): NativeInt;
begin
  Result:=WriteData(Buffer,4);
end;

function TStream.WriteData(const Buffer: UInt32; Count: NativeInt): NativeInt;
begin
  Result:=WriteMaxSizeData(MakeBytes(Buffer,4,False),4,Count);
end;

function TStream.WriteData(const Buffer: NativeLargeInt): NativeInt;
begin
  Result:=WriteData(Buffer,8);
end;

function TStream.WriteData(const Buffer: NativeLargeInt; Count: NativeInt): NativeInt;
begin
  Result:=WriteMaxSizeData(MakeBytes(Buffer,8,True),8,Count);
end;

function TStream.WriteData(const Buffer: NativeLargeUInt): NativeInt;
begin
  Result:=WriteData(Buffer,8);
end;

function TStream.WriteData(const Buffer: NativeLargeUInt; Count: NativeInt): NativeInt;
begin
  Result:=WriteMaxSizeData(MakeBytes(Buffer,8,False),8,Count);
end;

function TStream.WriteData(const Buffer: Double): NativeInt;
begin
  Result:=WriteData(Buffer,8);
end;

function TStream.WriteData(const Buffer: Double; Count: NativeInt): NativeInt;

Var
  Mem : TJSArrayBuffer;
  A : TJSUint8array;
  D : TJSDataview;
  B : TBytes;
  I : Integer;

begin
  Mem:=TJSArrayBuffer.New(8);
  D:=TJSDataView.new(Mem);
  D.setFloat64(0,Buffer);
  SetLength(B,8);
  A:=TJSUint8array.New(Mem);
  For I:=0 to 7 do
    B[i]:=A[i];
  Result:=WriteMaxSizeData(B,8,Count);
end;


procedure TStream.WriteBufferData(Buffer: Int32);
begin
  WriteBufferData(Buffer,4);
end;

procedure TStream.WriteBufferData(Buffer: Int32; Count: NativeInt);
begin
  if (WriteData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SWriteError);
end;

procedure TStream.WriteBufferData(Buffer: Boolean);
begin
  WriteBufferData(Buffer,1);
end;

procedure TStream.WriteBufferData(Buffer: Boolean; Count: NativeInt);
begin
  if (WriteData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SWriteError);
end;

procedure TStream.WriteBufferData(Buffer: WideChar);
begin
  WriteBufferData(Buffer,2);
end;

procedure TStream.WriteBufferData(Buffer: WideChar; Count: NativeInt);
begin
  if (WriteData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SWriteError);
end;

procedure TStream.WriteBufferData(Buffer: Int8);
begin
  WriteBufferData(Buffer,1);
end;

procedure TStream.WriteBufferData(Buffer: Int8; Count: NativeInt);
begin
  if (WriteData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SWriteError);
end;

procedure TStream.WriteBufferData(Buffer: UInt8);
begin
  WriteBufferData(Buffer,1);
end;

procedure TStream.WriteBufferData(Buffer: UInt8; Count: NativeInt);
begin
  if (WriteData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SWriteError);
end;

procedure TStream.WriteBufferData(Buffer: Int16);
begin
  WriteBufferData(Buffer,2);
end;

procedure TStream.WriteBufferData(Buffer: Int16; Count: NativeInt);
begin
  if (WriteData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SWriteError);
end;

procedure TStream.WriteBufferData(Buffer: UInt16);
begin
  WriteBufferData(Buffer,2);
end;

procedure TStream.WriteBufferData(Buffer: UInt16; Count: NativeInt);
begin
  if (WriteData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SWriteError);
end;

procedure TStream.WriteBufferData(Buffer: UInt32);
begin
  WriteBufferData(Buffer,4);
end;

procedure TStream.WriteBufferData(Buffer: UInt32; Count: NativeInt);
begin
  if (WriteData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SWriteError);
end;

procedure TStream.WriteBufferData(Buffer: NativeInt);
begin
  WriteBufferData(Buffer,8);
end;

procedure TStream.WriteBufferData(Buffer: NativeInt; Count: NativeInt);
begin
  if (WriteData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SWriteError);
end;

procedure TStream.WriteBufferData(Buffer: NativeLargeUInt);
begin
  WriteBufferData(Buffer,8);
end;

procedure TStream.WriteBufferData(Buffer: NativeLargeUInt; Count: NativeInt);
begin
  if (WriteData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SWriteError);
end;

procedure TStream.WriteBufferData(Buffer: Double);
begin
  WriteBufferData(Buffer,8);
end;

procedure TStream.WriteBufferData(Buffer: Double; Count: NativeInt);
begin
  if (WriteData(Buffer,Count)<>Count) then
    Raise EStreamError.Create(SWriteError);
end;



function TStream.CopyFrom(Source: TStream; Count: NativeInt): NativeInt;

var
   Buffer: TBytes;
   BufferSize, i: LongInt;

const
   MaxSize = $20000;
begin
   Result:=0;
   if Count=0 then
     Source.Position:=0;   // This WILL fail for non-seekable streams...
   BufferSize:=MaxSize;
   if (Count>0) and (Count<BufferSize) then
     BufferSize:=Count;    // do not allocate more than needed
   SetLength(Buffer,BufferSize);
   if Count=0 then
     repeat
       i:=Source.Read(Buffer,BufferSize);
       if i>0 then
         WriteBuffer(Buffer,i);
       Inc(Result,i);
     until i<BufferSize
   else
     while Count>0 do
       begin
       if Count>BufferSize then
         i:=BufferSize
       else
         i:=Count;
       Source.ReadBuffer(Buffer,i);
       WriteBuffer(Buffer,i);
       Dec(count,i);
       Inc(Result,i);
       end;
end;

function TStream.ReadComponent(Instance: TComponent): TComponent;

var
  Reader: TReader;

begin
  Reader := TReader.Create(Self);
  try
    Result := Reader.ReadRootComponent(Instance);
  finally
    Reader.Free;
  end;
end;

function TStream.ReadComponentRes(Instance: TComponent): TComponent;

begin
  ReadResHeader;
  Result := ReadComponent(Instance);
end;

procedure TStream.WriteComponent(Instance: TComponent);

begin
  WriteDescendent(Instance, nil);
end;

procedure TStream.WriteComponentRes(const ResName: string; Instance: TComponent);

begin
  WriteDescendentRes(ResName, Instance, nil);
end;

procedure TStream.WriteDescendent(Instance, Ancestor: TComponent);

var
  Driver : TAbstractObjectWriter;
  Writer : TWriter;

begin
  Driver := TBinaryObjectWriter.Create(Self);
  Try
    Writer := TWriter.Create(Driver);
    Try
      Writer.WriteDescendent(Instance, Ancestor);
    Finally
      Writer.Destroy;
    end;
  Finally
    Driver.Free;
  end;
end;

procedure TStream.WriteDescendentRes(const ResName: string; Instance, Ancestor: TComponent);

var
  FixupInfo: Longint;

begin
  { Write a resource header }
  WriteResourceHeader(ResName, FixupInfo);
  { Write the instance itself }
  WriteDescendent(Instance, Ancestor);
  { Insert the correct resource size into the resource header }
  FixupResourceHeader(FixupInfo);
end;


procedure TStream.WriteResourceHeader(const ResName: string; {!!!: out} var FixupInfo: Longint);
  var
    ResType, Flags : word;
    B : Byte;
    I : Integer;

  begin
     ResType:=Word($000A);
     Flags:=Word($1030);
     { Note: This is a Windows 16 bit resource }
     { Numeric resource type }
     WriteByte($ff);
     { Application defined data }
     WriteWord(ResType);
     { write the name as asciiz }
     For I:=1 to Length(ResName) do
        begin
        B:=Ord(ResName[i]);
        WriteByte(B);
        end;
     WriteByte(0);
     { Movable, Pure and Discardable }
     WriteWord(Flags);
     { Placeholder for the resource size }
     WriteDWord(0);
     { Return current stream position so that the resource size can be
       inserted later }
     FixupInfo := Position;
  end;

procedure TStream.FixupResourceHeader(FixupInfo: Longint);

var
   ResSize,TmpResSize : Longint;

begin

  ResSize := Position - FixupInfo;
  TmpResSize := longword(ResSize);

  { Insert the correct resource size into the placeholder written by
    WriteResourceHeader }
  Position := FixupInfo - 4;
  WriteDWord(TmpResSize);
  { Seek back to the end of the resource }
  Position := FixupInfo + ResSize;

end;

procedure TStream.ReadResHeader;
  var
    ResType, Flags : word;
  begin
     try
       { Note: This is a Windows 16 bit resource }
       { application specific resource ? }
       if ReadByte<>$ff then
         raise EInvalidImage.Create(SInvalidImage);
       ResType:=ReadWord;
       if ResType<>$000a then
         raise EInvalidImage.Create(SInvalidImage);
       { read name }
       while ReadByte<>0 do
         ;
       { check the access specifier }
       Flags:=ReadWord;
       if Flags<>$1030 then
         raise EInvalidImage.Create(SInvalidImage);
       { ignore the size }
       ReadDWord;
     except
       on EInvalidImage do
         raise;
       else
         raise EInvalidImage.create(SInvalidImage);
     end;
  end;


function TStream.ReadByte : Byte;

begin
  ReadBufferData(Result,1);
end;

function TStream.ReadWord : Word;

begin
  ReadBufferData(Result,2);
end;

function TStream.ReadDWord : Cardinal;

begin
  ReadBufferData(Result,4);
end;

function TStream.ReadQWord: NativeLargeUInt;

begin
  ReadBufferData(Result,8);
end;


procedure TStream.WriteByte(b : Byte);

begin
  WriteBufferData(b,1);
end;

procedure TStream.WriteWord(w : Word);

begin
  WriteBufferData(W,2);
end;

procedure TStream.WriteDWord(d : Cardinal);

begin
  WriteBufferData(d,4);
end;

procedure TStream.WriteQWord(q: NativeLargeUInt);
begin
  WriteBufferData(q,8);
end;

{****************************************************************************}
{*                             TCustomMemoryStream                          *}
{****************************************************************************}

procedure TCustomMemoryStream.SetPointer(Ptr: TJSArrayBuffer; ASize: PtrInt);

begin
  FMemory:=Ptr;
  FSize:=ASize;
  FDataView:=Nil;
  FDataArray:=Nil;
end;


class function TCustomMemoryStream.MemoryToBytes(Mem: TJSArrayBuffer): TBytes;

begin
  Result:=MemoryToBytes(TJSUint8Array.New(Mem));
end;

class function TCustomMemoryStream.MemoryToBytes(Mem: TJSUint8Array): TBytes;

Var
  I : Integer;

begin
  // This must be improved, but needs some asm or TJSFunction.call() to implement answers in
  // https://stackoverflow.com/questions/29676635/convert-uint8array-to-array-in-javascript
  for i:=0 to mem.length-1 do
    Result[i]:=Mem[i];
end;

class function TCustomMemoryStream.BytesToMemory(aBytes: TBytes): TJSArrayBuffer;

Var
  a : TJSUint8Array;

begin
  Result:=TJSArrayBuffer.new(Length(aBytes));
  A:=TJSUint8Array.New(Result);
  A._set(aBytes);
end;

function TCustomMemoryStream.GetDataArray: TJSUint8Array;
begin
  if FDataArray=Nil then
    FDataArray:=TJSUint8Array.new(Memory);
  Result:=FDataArray;
end;

function TCustomMemoryStream.GetDataView: TJSDataview;
begin
  if FDataView=Nil then
    FDataView:=TJSDataView.New(Memory);
  Result:=FDataView;
end;

function TCustomMemoryStream.GetSize: NativeInt;

begin
  Result:=FSize;
end;

function TCustomMemoryStream.GetPosition: NativeInt;
begin
  Result:=FPosition;
end;


function TCustomMemoryStream.Read(Buffer: TBytes; Offset, Count: LongInt): LongInt;

Var
  I,Src,Dest : Integer;

begin
  Result:=0;
  If (FSize>0) and (FPosition<Fsize) and (FPosition>=0) then
    begin
    Result:=Count;
    If (Result>(FSize-FPosition)) then
      Result:=(FSize-FPosition);
    Src:=FPosition;
    Dest:=Offset;
    I:=0;
    While I<Result do
      begin
      Buffer[Dest]:=DataView.getUint8(Src);
      inc(Src);
      inc(Dest);
      inc(I);
      end;
    FPosition:=Fposition+Result;
    end;
end;


function TCustomMemoryStream.Seek(const Offset: NativeInt; Origin: TSeekOrigin): NativeInt;

begin
  Case Origin of
    soBeginning : FPosition:=Offset;
    soEnd       : FPosition:=FSize+Offset;
    soCurrent   : FPosition:=FPosition+Offset;
  end;
  if SizeBoundsSeek and (FPosition>FSize) then
    FPosition:=FSize;
  Result:=FPosition;
  {$IFDEF DEBUG}
  if Result < 0 then
    raise Exception.Create('TCustomMemoryStream');
  {$ENDIF}
end;


procedure TCustomMemoryStream.SaveToStream(Stream: TStream);

begin
  if FSize>0 then
    Stream.WriteBuffer(TMemoryStream.MemoryToBytes(Memory),FSize);
end;

procedure TCustomMemoryStream.LoadFromURL(const aURL: String; Async: Boolean; OnLoaded: TNotifyEventRef; OnError: TStringNotifyEventRef = Nil);

  procedure DoLoaded(const abytes : TJSArrayBuffer);
  begin
    SetPointer(aBytes,aBytes.byteLength);
    if Assigned(OnLoaded) then
      OnLoaded(Self);
  end;

  procedure DoError(const AError : String);
  begin
    if Assigned(OnError) then
      OnError(Self,aError)
    else
      Raise EInOutError.Create('Failed to load from URL:'+aError);
  end;

begin
  CheckLoadHelper;
  GlobalLoadHelper.LoadBytes(aURL,aSync,@DoLoaded,@DoError);
end;

procedure TCustomMemoryStream.LoadFromFile(const aFileName: String; const OnLoaded: TProc; const AError: TProcString);

begin
  LoadFromURL(aFileName,False,
    Procedure (Sender : TObject)
    begin
      If Assigned(OnLoaded) then
       OnLoaded
    end,
    Procedure (Sender : TObject; Const ErrorMsg : String)
    begin
      if Assigned(aError) then
        aError(ErrorMsg)
    end);
end;



{****************************************************************************}
{*                             TMemoryStream                                *}
{****************************************************************************}


Const TMSGrow = 4096; { Use 4k blocks. }

procedure TMemoryStream.SetCapacity(NewCapacity: PtrInt);

begin
  SetPointer (Realloc(NewCapacity),Fsize);
  FCapacity:=NewCapacity;
end;


function TMemoryStream.Realloc(var NewCapacity: PtrInt): TJSArrayBuffer;

Var
  GC : PtrInt;
  DestView : TJSUInt8array;

begin
  If NewCapacity<0 Then
    NewCapacity:=0
  else
    begin
      GC:=FCapacity + (FCapacity div 4);
      // if growing, grow at least a quarter
      if (NewCapacity>FCapacity) and (NewCapacity < GC) then
        NewCapacity := GC;
      // round off to block size.
      NewCapacity := (NewCapacity + (TMSGrow-1)) and not (TMSGROW-1);
    end;
  // Only now check !
  If NewCapacity=FCapacity then
    Result:=FMemory
  else if NewCapacity=0 then
    Result:=Nil
  else
    begin
    // New buffer
    Result:=TJSArrayBuffer.New(NewCapacity);
    If (Result=Nil)  then
      Raise EStreamError.Create(SMemoryStreamError);
    // Transfer
    DestView:=TJSUInt8array.New(Result);
    Destview._Set(Self.DataArray);
    end;
end;


destructor TMemoryStream.Destroy;

begin
  Clear;
  Inherited Destroy;
end;


procedure TMemoryStream.Clear;

begin
  FSize:=0;
  FPosition:=0;
  SetCapacity (0);
end;


procedure TMemoryStream.LoadFromStream(Stream: TStream);

begin
  Position:=0;
  Stream.Position:=0;
  SetSize(Stream.Size);
  If (Size>0) then
    CopyFrom(Stream,0);
end;

procedure TMemoryStream.SetSize(const NewSize: NativeInt);

begin
  SetCapacity (NewSize);
  FSize:=NewSize;
  IF FPosition>FSize then
    FPosition:=FSize;
end;

function TMemoryStream.Write(Const Buffer : TBytes; OffSet, Count: LongInt): LongInt;

Var NewPos : PtrInt;

begin
  If (Count=0) or (FPosition<0) then
    exit(0);
  NewPos:=FPosition+Count;
  If NewPos>Fsize then
    begin
    IF NewPos>FCapacity then
      SetCapacity (NewPos);
    FSize:=Newpos;
    end;
  DataArray._set(Copy(Buffer,Offset,Count),FPosition);
  FPosition:=NewPos;
  Result:=Count;
end;

{****************************************************************************}
{*                              TBytesStream                                *}
{****************************************************************************}

constructor TBytesStream.Create(const ABytes: TBytes);
begin
  inherited Create;
  SetPointer(TMemoryStream.BytesToMemory(aBytes),Length(ABytes));
  FCapacity:=Length(ABytes);
end;

function TBytesStream.GetBytes: TBytes;
begin
  Result:=TMemoryStream.MemoryToBytes(Memory);
end;

{ *********************************************************************
  *                         TFiler                                    *
  *********************************************************************}

procedure TFiler.SetRoot(ARoot: TComponent);
begin
  FRoot := ARoot;
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
{*                       TBinaryObjectReader                                *}
{****************************************************************************}


function TBinaryObjectReader.ReadWord : word;
begin
  FStream.ReadBufferData(Result);
end;

function TBinaryObjectReader.ReadDWord : longword;
begin
  FStream.ReadBufferData(Result);
end;


constructor TBinaryObjectReader.Create(Stream: TStream);
begin
  inherited Create;
  If (Stream=Nil) then
    Raise EReadError.Create(SEmptyStreamIllegalReader);
  FStream := Stream;
end;

function TBinaryObjectReader.ReadValue: TValueType;
var
  b: byte;
begin
  FStream.ReadBufferData(b);
  Result := TValueType(b);
end;

function TBinaryObjectReader.NextValue: TValueType;
begin
  Result := ReadValue;
  { We only 'peek' at the next value, so seek back to unget the read value: }
  FStream.Seek(-1,soCurrent);
end;

procedure TBinaryObjectReader.BeginRootComponent;
begin
  { Read filer signature }
  ReadSignature;
end;

procedure TBinaryObjectReader.BeginComponent(var Flags: TFilerFlags;
  var AChildPos: Integer; var CompClassName, CompName: String);
var
  Prefix: Byte;
  ValueType: TValueType;
begin
  { Every component can start with a special prefix: }
  Flags := [];
  if (Byte(NextValue) and $f0) = $f0 then
  begin
    Prefix := Byte(ReadValue);
    Flags:=[];
    if (Prefix and $01)<>0 then
      Include(Flags,ffInherited);
    if (Prefix and $02)<>0 then
      Include(Flags,ffChildPos);
    if (Prefix and $04)<>0 then
      Include(Flags,ffInline);
    if ffChildPos in Flags then
    begin
      ValueType := ReadValue;
      case ValueType of
        vaInt8:
          AChildPos := ReadInt8;
        vaInt16:
          AChildPos := ReadInt16;
        vaInt32:
          AChildPos := ReadInt32;
        vaNativeInt:
          AChildPos := ReadNativeInt;
        else
          raise EReadError.Create(SInvalidPropertyValue);
      end;
    end;
  end;

  CompClassName := ReadStr;
  CompName := ReadStr;
end;

function TBinaryObjectReader.BeginProperty: String;
begin
  Result := ReadStr;
end;

procedure TBinaryObjectReader.Read(var Buffer: TBytes; Count: Longint);
begin
  FStream.Read(Buffer,Count);
end;

procedure TBinaryObjectReader.ReadBinary(const DestData: TMemoryStream);
var
  BinSize: LongInt;
begin
  BinSize:=LongInt(ReadDWord);
  DestData.Size := BinSize;
  DestData.CopyFrom(FStream,BinSize);
end;

function TBinaryObjectReader.ReadFloat: Extended;
begin
  FStream.ReadBufferData(Result);
end;

function TBinaryObjectReader.ReadCurrency: Currency;

begin
  Result:=ReadFloat;
end;

function TBinaryObjectReader.ReadIdent(ValueType: TValueType): String;
var
  i: Byte;
  c : Char;
begin
  case ValueType of
    vaIdent:
      begin
        FStream.ReadBufferData(i);
        SetLength(Result,i);
        For I:=1 to Length(Result) do
          begin
          FStream.ReadBufferData(C);
          Result[I]:=C;
          end;
      end;
    vaNil:
      Result := 'nil';
    vaFalse:
      Result := 'False';
    vaTrue:
      Result := 'True';
    vaNull:
      Result := 'Null';
  end;
end;

function TBinaryObjectReader.ReadInt8: ShortInt;
begin
  FStream.ReadBufferData(Result);
end;

function TBinaryObjectReader.ReadInt16: SmallInt;
begin
  FStream.ReadBufferData(Result);
end;

function TBinaryObjectReader.ReadInt32: LongInt;
begin
  FStream.ReadBufferData(Result);
end;

function TBinaryObjectReader.ReadNativeInt : NativeInt;
begin
  FStream.ReadBufferData(Result);
end;


function TBinaryObjectReader.ReadSet(EnumType: TTypeInfoEnum): Integer;

var
  Name: String;
  Value: Integer;

begin
  try
    Result := 0;
    while True do
      begin
      Name := ReadStr;
      if Length(Name) = 0 then
        break;
      Value:=EnumType.EnumType.NameToInt[Name];
      if Value=-1 then
        raise EReadError.Create(SInvalidPropertyValue);
      Result:=Result or (1 shl Value);
      end;
  except
    SkipSetBody;
    raise;
  end;
end;


Const
  // Integer version of 4 chars 'TPF0'
  FilerSignatureInt = 809914452;

procedure TBinaryObjectReader.ReadSignature;

var
  Signature: LongInt;

begin
  FStream.ReadBufferData(Signature);
  if Signature <> FilerSignatureInt then
    raise EReadError.Create(SInvalidImage);
end;

function TBinaryObjectReader.ReadStr: String;
var
  l,i: Byte;
  c : Char;
begin
  FStream.ReadBufferData(L);
  SetLength(Result,L);
  For I:=1 to L do
    begin
    FStream.ReadBufferData(C);
    Result[i]:=C;
    end;
end;

function TBinaryObjectReader.ReadString(StringType: TValueType): String;
var
  i: Integer;
  C : Char;

begin
  Result:='';
  if StringType<>vaString then
    Raise EFilerError.Create('Invalid string type passed to ReadString');
  i:=ReadDWord;
  SetLength(Result, i);
  for I:=1 to Length(Result) do
    begin
    FStream.ReadbufferData(C);
    Result[i]:=C;
    end;
end;


function TBinaryObjectReader.ReadWideString: WideString;
begin
  Result:=ReadString(vaWString);
end;

function TBinaryObjectReader.ReadUnicodeString: UnicodeString;

begin
  Result:=ReadString(vaWString);
end;

procedure TBinaryObjectReader.SkipComponent(SkipComponentInfos: Boolean);
var
  Flags: TFilerFlags;
  Dummy: Integer;
  CompClassName, CompName: String;
begin
  if SkipComponentInfos then
    { Skip prefix, component class name and component object name }
    BeginComponent(Flags, Dummy, CompClassName, CompName);

  { Skip properties }
  while NextValue <> vaNull do
    SkipProperty;
  ReadValue;

  { Skip children }
  while NextValue <> vaNull do
    SkipComponent(True);
  ReadValue;
end;

procedure TBinaryObjectReader.SkipValue;

  procedure SkipBytes(Count: LongInt);
  var
    Dummy: TBytes;
    SkipNow: Integer;
  begin
    while Count > 0 do
    begin
      if Count > 1024 then
        SkipNow := 1024
      else
        SkipNow := Count;
      SetLength(Dummy,SkipNow);
      Read(Dummy, SkipNow);
      Dec(Count, SkipNow);
    end;
  end;

var
  Count: LongInt;
begin
  case ReadValue of
    vaNull, vaFalse, vaTrue, vaNil: ;
    vaList:
      begin
        while NextValue <> vaNull do
          SkipValue;
        ReadValue;
      end;
    vaInt8:
      SkipBytes(1);
    vaInt16:
      SkipBytes(2);
    vaInt32:
      SkipBytes(4);
    vaInt64,
    vaDouble:
      SkipBytes(8);
    vaIdent:
      ReadStr;
    vaString:
      ReadString(vaString);
    vaBinary:
      begin
        Count:=LongInt(ReadDWord);
        SkipBytes(Count);
      end;
    vaSet:
      SkipSetBody;
    vaCollection:
      begin
        while NextValue <> vaNull do
        begin
          { Skip the order value if present }
          if NextValue in [vaInt8, vaInt16, vaInt32] then
            SkipValue;
          SkipBytes(1);
          while NextValue <> vaNull do
            SkipProperty;
          ReadValue;
        end;
        ReadValue;
      end;
  end;
end;

{ private methods }


procedure TBinaryObjectReader.SkipProperty;
begin
  { Skip property name, then the property value }
  ReadStr;
  SkipValue;
end;

procedure TBinaryObjectReader.SkipSetBody;
begin
  while Length(ReadStr) > 0 do;
end;


  // Quadruple representing an unresolved component property.
Type
  { TUnresolvedReference }

  TUnresolvedReference = class(TlinkedListItem)
  Private
    FRoot: TComponent;     // Root component when streaming
    FPropInfo: TTypeMemberProperty;  // Property to set.
    FGlobal,               // Global component.
    FRelative : string;    // Path relative to global component.
    Function Resolve(Instance : TPersistent) : Boolean; // Resolve this reference
    Function RootMatches(ARoot : TComponent) : Boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE} // True if Froot matches or ARoot is nil.
    Function NextRef : TUnresolvedReference; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
  end;

  TLocalUnResolvedReference = class(TUnresolvedReference)
    Finstance : TPersistent;
  end;

  // Linked list of TPersistent items that have unresolved properties.

  { TUnResolvedInstance }

  TUnResolvedInstance = Class(TLinkedListItem)
  Public
    Instance : TPersistent; // Instance we're handling unresolveds for
    FUnresolved : TLinkedList; // The list
    Destructor Destroy; override;
    Function AddReference(ARoot : TComponent; APropInfo : TTypeMemberProperty; AGlobal,ARelative : String) : TUnresolvedReference;
    Function RootUnresolved : TUnresolvedReference; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE} // Return root element in list.
    Function ResolveReferences : Boolean; // Return true if all unresolveds were resolved.
  end;

  // Builds a list of TUnResolvedInstances, removes them from global list on free.
  TBuildListVisitor = Class(TLinkedListVisitor)
  Private
    List : TFPList;
  Public
    Procedure Add(Item : TlinkedListItem); // Add TUnResolvedInstance item to list. Create list if needed
    Destructor Destroy; override; // All elements in list (if any) are removed from the global list.
  end;

  // Visitor used to try and resolve instances in the global list
  TResolveReferenceVisitor = Class(TBuildListVisitor)
    Function Visit(Item : TLinkedListItem) : Boolean; override;
  end;

  // Visitor used to remove all references to a certain component.
  TRemoveReferenceVisitor = Class(TBuildListVisitor)
  Private
    FRef : String;
    FRoot : TComponent;
  Public
    Constructor Create(ARoot : TComponent;Const ARef : String);
    Function Visit(Item : TLinkedListItem) : Boolean; override;
  end;

  // Visitor used to collect reference names.
  TReferenceNamesVisitor = Class(TLinkedListVisitor)
  Private
    FList : TStrings;
    FRoot : TComponent;
  Public
    Function Visit(Item : TLinkedListItem) : Boolean; override;
    Constructor Create(ARoot : TComponent;AList : TStrings);
  end;

  // Visitor used to collect instance names.
  TReferenceInstancesVisitor = Class(TLinkedListVisitor)
  Private
    FList : TStrings;
    FRef  : String;
    FRoot : TComponent;
  Public
    Function Visit(Item : TLinkedListItem) : Boolean; override;
    Constructor Create(ARoot : TComponent;Const ARef : String; AList : TStrings);
  end;

  // Visitor used to redirect links to another root component.
  TRedirectReferenceVisitor = Class(TLinkedListVisitor)
  Private
    FOld,
    FNew : String;
    FRoot : TComponent;
  Public
    Function Visit(Item : TLinkedListItem) : Boolean; override;
    Constructor Create(ARoot : TComponent;Const AOld,ANew : String);
  end;

var
  NeedResolving : TLinkedList;

// Add an instance to the global list of instances which need resolving.
Function FindUnresolvedInstance(AInstance: TPersistent) : TUnResolvedInstance;

begin
  Result:=Nil;
{$ifdef FPC_HAS_FEATURE_THREADING}
  EnterCriticalSection(ResolveSection);
  Try
{$endif}
    If Assigned(NeedResolving) then
      begin
      Result:=TUnResolvedInstance(NeedResolving.Root);
      While (Result<>Nil) and (Result.Instance<>AInstance) do
        Result:=TUnResolvedInstance(Result.Next);
      end;
{$ifdef FPC_HAS_FEATURE_THREADING}
  finally
    LeaveCriticalSection(ResolveSection);
  end;
{$endif}
end;

Function AddtoResolveList(AInstance: TPersistent) : TUnResolvedInstance;

begin
  Result:=FindUnresolvedInstance(AInstance);
  If (Result=Nil) then
    begin
{$ifdef FPC_HAS_FEATURE_THREADING}
    EnterCriticalSection(ResolveSection);
    Try
{$endif}
      If not Assigned(NeedResolving) then
        NeedResolving:=TLinkedList.Create(TUnResolvedInstance);
      Result:=NeedResolving.Add as TUnResolvedInstance;
      Result.Instance:=AInstance;
{$ifdef FPC_HAS_FEATURE_THREADING}
    finally
      LeaveCriticalSection(ResolveSection);
    end;
{$endif}
    end;
end;

// Walk through the global list of instances to be resolved.

Procedure VisitResolveList(V : TLinkedListVisitor);

begin
{$ifdef FPC_HAS_FEATURE_THREADING}
  EnterCriticalSection(ResolveSection);
  Try
{$endif}
    try
      NeedResolving.Foreach(V);
    Finally
      FreeAndNil(V);
    end;
{$ifdef FPC_HAS_FEATURE_THREADING}
  Finally
    LeaveCriticalSection(ResolveSection);
  end;
{$endif}
end;

procedure GlobalFixupReferences;

begin
  If (NeedResolving=Nil) then
    Exit;
{$ifdef FPC_HAS_FEATURE_THREADING}
  GlobalNameSpace.BeginWrite;
  try
{$endif}
    VisitResolveList(TResolveReferenceVisitor.Create);
{$ifdef FPC_HAS_FEATURE_THREADING}
  finally
    GlobalNameSpace.EndWrite;
  end;
{$endif}
end;


procedure GetFixupReferenceNames(Root: TComponent; Names: TStrings);

begin
  If (NeedResolving=Nil) then
    Exit;
  VisitResolveList(TReferenceNamesVisitor.Create(Root,Names));
end;

procedure GetFixupInstanceNames(Root: TComponent; const ReferenceRootName: string; Names: TStrings);

begin
  If (NeedResolving=Nil) then
    Exit;
  VisitResolveList(TReferenceInstancesVisitor.Create(Root,ReferenceRootName,Names));
end;

procedure ObjectBinaryToText(aInput, aOutput: TStream);
begin
  ObjectBinaryToText(aInput,aOutput,oteLFM);
end;

procedure ObjectBinaryToText(aInput, aOutput: TStream; aEncoding: TObjectTextEncoding);

var
  Conv : TObjectStreamConverter;

begin
  Conv:=TObjectStreamConverter.Create;
  try
    Conv.ObjectBinaryToText(aInput,aOutput,aEncoding);
  finally
    Conv.Free;
  end;
end;

procedure RedirectFixupReferences(Root: TComponent; const OldRootName, NewRootName: string);

begin
  If (NeedResolving=Nil) then
      Exit;
  VisitResolveList(TRedirectReferenceVisitor.Create(Root,OldRootName,NewRootName));
end;

procedure RemoveFixupReferences(Root: TComponent; const RootName: string);

begin
  If (NeedResolving=Nil) then
      Exit;
  VisitResolveList(TRemoveReferenceVisitor.Create(Root,RootName));
end;


{ TUnresolvedReference }

Function TUnresolvedReference.Resolve(Instance : TPersistent) : Boolean;

Var
  C : TComponent;

begin
  C:=FindGlobalComponent(FGlobal);
  Result:=(C<>Nil);
  If Result then
    begin
    C:=FindNestedComponent(C,FRelative);
    Result:=C<>Nil;
    If Result then
      SetObjectProp(Instance, FPropInfo,C);
    end;
end;

Function TUnresolvedReference.RootMatches(ARoot : TComponent) : Boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}

begin
  Result:=(ARoot=Nil) or (ARoot=FRoot);
end;

Function TUnResolvedReference.NextRef : TUnresolvedReference;

begin
  Result:=TUnresolvedReference(Next);
end;

{ TUnResolvedInstance }

destructor TUnResolvedInstance.Destroy;
begin
  FUnresolved.Free;
  inherited Destroy;
end;

function TUnResolvedInstance.AddReference(ARoot: TComponent; APropInfo : TTypeMemberProperty; AGlobal, ARelative: String): TUnresolvedReference;
begin
  If (FUnResolved=Nil) then
    FUnResolved:=TLinkedList.Create(TUnresolvedReference);
  Result:=FUnResolved.Add as TUnresolvedReference;
  Result.FGlobal:=AGLobal;
  Result.FRelative:=ARelative;
  Result.FPropInfo:=APropInfo;
  Result.FRoot:=ARoot;
end;

Function TUnResolvedInstance.RootUnresolved : TUnresolvedReference;

begin
  Result:=Nil;
  If Assigned(FUnResolved) then
    Result:=TUnresolvedReference(FUnResolved.Root);
end;

Function TUnResolvedInstance.ResolveReferences:Boolean;

Var
  R,RN : TUnresolvedReference;

begin
  R:=RootUnResolved;
  While (R<>Nil) do
    begin
    RN:=R.NextRef;
    If R.Resolve(Self.Instance) then
      FUnresolved.RemoveItem(R,True);
    R:=RN;
    end;
  Result:=RootUnResolved=Nil;
end;

{ TReferenceNamesVisitor }

Constructor TReferenceNamesVisitor.Create(ARoot : TComponent;AList : TStrings);

begin
  FRoot:=ARoot;
  FList:=AList;
end;

Function TReferenceNamesVisitor.Visit(Item : TLinkedListItem) : Boolean;

Var
  R : TUnresolvedReference;

begin
  R:=TUnResolvedInstance(Item).RootUnresolved;
  While (R<>Nil) do
    begin
    If R.RootMatches(FRoot) then
      If (FList.IndexOf(R.FGlobal)=-1) then
        FList.Add(R.FGlobal);
    R:=R.NextRef;
    end;
  Result:=True;
end;

{ TReferenceInstancesVisitor }

Constructor TReferenceInstancesVisitor.Create(ARoot : TComponent; Const ARef : String;AList : TStrings);

begin
  FRoot:=ARoot;
  FRef:=UpperCase(ARef);
  FList:=AList;
end;

Function TReferenceInstancesVisitor.Visit(Item : TLinkedListItem) : Boolean;

Var
  R : TUnresolvedReference;

begin
  R:=TUnResolvedInstance(Item).RootUnresolved;
  While (R<>Nil) do
    begin
    If (FRoot=R.FRoot) and (FRef=UpperCase(R.FGLobal)) Then
      If Flist.IndexOf(R.FRelative)=-1 then
        Flist.Add(R.FRelative);
    R:=R.NextRef;
    end;
  Result:=True;
end;

{ TRedirectReferenceVisitor }

Constructor TRedirectReferenceVisitor.Create(ARoot : TComponent; Const AOld,ANew  : String);

begin
  FRoot:=ARoot;
  FOld:=UpperCase(AOld);
  FNew:=ANew;
end;

Function TRedirectReferenceVisitor.Visit(Item : TLinkedListItem) : Boolean;

Var
  R : TUnresolvedReference;

begin
  R:=TUnResolvedInstance(Item).RootUnresolved;
  While (R<>Nil) do
    begin
    If R.RootMatches(FRoot) and (FOld=UpperCase(R.FGLobal)) Then
      R.FGlobal:=FNew;
    R:=R.NextRef;
    end;
  Result:=True;
end;

{ TRemoveReferenceVisitor }

Constructor TRemoveReferenceVisitor.Create(ARoot : TComponent; Const ARef  : String);

begin
  FRoot:=ARoot;
  FRef:=UpperCase(ARef);
end;

Function TRemoveReferenceVisitor.Visit(Item : TLinkedListItem) : Boolean;

Var
  I : Integer;
  UI : TUnResolvedInstance;
  R : TUnresolvedReference;
  L : TFPList;

begin
  UI:=TUnResolvedInstance(Item);
  R:=UI.RootUnresolved;
  L:=Nil;
  Try
    // Collect all matches.
    While (R<>Nil) do
      begin
      If R.RootMatches(FRoot) and ((FRef = '') or (FRef=UpperCase(R.FGLobal))) Then
        begin
        If Not Assigned(L) then
          L:=TFPList.Create;
        L.Add(R);
        end;
      R:=R.NextRef;
      end;
    // Remove all matches.
    IF Assigned(L) then
      begin
      For I:=0 to L.Count-1 do
        UI.FUnresolved.RemoveItem(TLinkedListitem(L[i]),True);
      end;
    // If any references are left, leave them.
    If UI.FUnResolved.Root=Nil then
      begin
      If List=Nil then
        List:=TFPList.Create;
      List.Add(UI);
      end;
  Finally
    L.Free;
  end;
  Result:=True;
end;

{ TBuildListVisitor }

Procedure TBuildListVisitor.Add(Item : TlinkedListItem);

begin
  If (List=Nil) then
    List:=TFPList.Create;
  List.Add(Item);
end;

Destructor TBuildListVisitor.Destroy;

Var
  I : Integer;

begin
  If Assigned(List) then
    For I:=0 to List.Count-1 do
      NeedResolving.RemoveItem(TLinkedListItem(List[I]),True);
  FreeAndNil(List);
  Inherited;
end;

{ TResolveReferenceVisitor }

Function TResolveReferenceVisitor.Visit(Item : TLinkedListItem) : Boolean;

begin
  If TUnResolvedInstance(Item).ResolveReferences then
    Add(Item);
  Result:=True;
end;



{****************************************************************************}
{*                             TREADER                                      *}
{****************************************************************************}


constructor TReader.Create(Stream: TStream);
begin
  inherited Create;
  If (Stream=Nil) then
    Raise EReadError.Create(SEmptyStreamIllegalReader);
  FDriver := CreateDriver(Stream);
end;

destructor TReader.Destroy;
begin
  FDriver.Free;
  inherited Destroy;
end;


procedure TReader.FlushBuffer;
begin
  Driver.FlushBuffer;
end;

function TReader.CreateDriver(Stream: TStream): TAbstractObjectReader;
begin
  Result := TBinaryObjectReader.Create(Stream);
end;

procedure TReader.BeginReferences;
begin
  FLoaded := TFpList.Create;
end;

procedure TReader.CheckValue(Value: TValueType);
begin
  if FDriver.NextValue <> Value then
    raise EReadError.Create(SInvalidPropertyValue)
  else
    FDriver.ReadValue;
end;

procedure TReader.DefineProperty(const Name: String; AReadData: TReaderProc;
  WriteData: TWriterProc; HasData: Boolean);
begin
  if Assigned(AReadData) and SameText(Name,FPropName) then
  begin
    AReadData(Self);
    SetLength(FPropName, 0);
  end else if assigned(WriteData) and HasData then
    ;
end;

procedure TReader.DefineBinaryProperty(const Name: String;
  AReadData, WriteData: TStreamProc; HasData: Boolean);
var
  MemBuffer: TMemoryStream;
begin
  if Assigned(AReadData) and SameText(Name,FPropName) then
  begin
    { Check if the next property really is a binary property}
    if FDriver.NextValue <> vaBinary then
    begin
      FDriver.SkipValue;
      FCanHandleExcepts := True;
      raise EReadError.Create(SInvalidPropertyValue);
    end else
      FDriver.ReadValue;

    MemBuffer := TMemoryStream.Create;
    try
      FDriver.ReadBinary(MemBuffer);
      FCanHandleExcepts := True;
      AReadData(MemBuffer);
    finally
      MemBuffer.Free;
    end;
    SetLength(FPropName, 0);
  end else if assigned(WriteData) and HasData then ;
end;

function TReader.EndOfList: Boolean;
begin
  Result := FDriver.NextValue = vaNull;
end;

procedure TReader.EndReferences;
begin
  FLoaded.Free;
  FLoaded := nil;
end;

function TReader.Error(const Message: String): Boolean;
begin
  Result := False;
  if Assigned(FOnError) then
    FOnError(Self, Message, Result);
end;

function TReader.FindMethod(ARoot: TComponent; const AMethodName: String): CodePointer;
var
  ErrorResult: Boolean;
begin
  Result:=nil;
  if (ARoot=Nil) or (aMethodName='') then
    exit;
  Result := ARoot.MethodAddress(AMethodName);
  ErrorResult := Result = nil;

  { always give the OnFindMethod callback a chance to locate the method }
  if Assigned(FOnFindMethod) then
    FOnFindMethod(Self, AMethodName, Result, ErrorResult);

  if ErrorResult then
    raise EReadError.Create(SInvalidPropertyValue);
end;

procedure TReader.DoFixupReferences;

Var
  R,RN : TLocalUnresolvedReference;
  G : TUnresolvedInstance;
  Ref : String;
  C : TComponent;
  P : integer;
  L : TLinkedList;

begin
  If Assigned(FFixups) then
    begin
    L:=TLinkedList(FFixups);
    R:=TLocalUnresolvedReference(L.Root);
    While (R<>Nil) do
      begin
      RN:=TLocalUnresolvedReference(R.Next);
      Ref:=R.FRelative;
      If Assigned(FOnReferenceName) then
        FOnReferenceName(Self,Ref);
      C:=FindNestedComponent(R.FRoot,Ref);
      If Assigned(C) then
        if R.FPropInfo.TypeInfo.Kind = tkInterface then
          SetInterfaceProp(R.FInstance,R.FPropInfo,C)
        else
          SetObjectProp(R.FInstance,R.FPropInfo,C)
      else
        begin
        P:=Pos('.',R.FRelative);
        If (P<>0) then
          begin
          G:=AddToResolveList(R.FInstance);
          G.Addreference(R.FRoot,R.FPropInfo,Copy(R.FRelative,1,P-1),Copy(R.FRelative,P+1,Length(R.FRelative)-P));
          end;
        end;
      L.RemoveItem(R,True);
      R:=RN;
      end;
    FreeAndNil(FFixups);
    end;
end;

procedure TReader.FixupReferences;
var
  i: Integer;
begin
  DoFixupReferences;
  GlobalFixupReferences;
  for i := 0 to FLoaded.Count - 1 do
    TComponent(FLoaded[I]).Loaded;
end;


function TReader.NextValue: TValueType;
begin
  Result := FDriver.NextValue;
end;

procedure TReader.Read(var Buffer : TBytes; Count: LongInt);
begin
  //This should give an exception if read is not implemented (i.e. TTextObjectReader)
  //but should work with TBinaryObjectReader.
  Driver.Read(Buffer, Count);
end;

procedure TReader.PropertyError;
begin
  FDriver.SkipValue;
  raise EReadError.CreateFmt(SUnknownProperty,[FPropName]);
end;

function TReader.ReadBoolean: Boolean;
var
  ValueType: TValueType;
begin
  ValueType := FDriver.ReadValue;
  if ValueType = vaTrue then
    Result := True
  else if ValueType = vaFalse then
    Result := False
  else
    raise EReadError.Create(SInvalidPropertyValue);
end;

function TReader.ReadChar: Char;
var
  s: String;
begin
  s := ReadString;
  if Length(s) = 1 then
    Result := s[1]
  else
    raise EReadError.Create(SInvalidPropertyValue);
end;

function TReader.ReadWideChar: WideChar;

var
  W: WideString;

begin
  W := ReadWideString;
  if Length(W) = 1 then
    Result := W[1]
  else
    raise EReadError.Create(SInvalidPropertyValue);
end;

function TReader.ReadUnicodeChar: UnicodeChar;

var
  U: UnicodeString;

begin
  U := ReadUnicodeString;
  if Length(U) = 1 then
    Result := U[1]
  else
    raise EReadError.Create(SInvalidPropertyValue);
end;

procedure TReader.ReadCollection(Collection: TCollection);
var
  Item: TCollectionItem;
begin
  Collection.BeginUpdate;
  if not EndOfList then
    Collection.Clear;
  while not EndOfList do begin
    ReadListBegin;
    Item := Collection.Add;
    while NextValue<>vaNull do
      ReadProperty(Item);
    ReadListEnd;
  end;
  Collection.EndUpdate;
  ReadListEnd;
end;

function TReader.ReadComponent(Component: TComponent): TComponent;
var
  Flags: TFilerFlags;

  function Recover(E : Exception; var aComponent: TComponent): Boolean;
  begin
    Result := False;
    if not ((ffInherited in Flags) or Assigned(Component)) then
      aComponent.Free;
    aComponent := nil;
    FDriver.SkipComponent(False);
    Result := Error(E.Message);
  end;

var
  CompClassName, Name: String;
  n, ChildPos: Integer;
  SavedParent, SavedLookupRoot: TComponent;
  ComponentClass: TComponentClass;
  C, NewComponent: TComponent;
  SubComponents: TList;
begin
  FDriver.BeginComponent(Flags, ChildPos, CompClassName, Name);
  SavedParent := Parent;
  SavedLookupRoot := FLookupRoot;
  SubComponents := nil;
  try
    Result := Component;
    if not Assigned(Result) then
      try
        if ffInherited in Flags then
        begin
          { Try to locate the existing ancestor component }

          if Assigned(FLookupRoot) then
            Result := FLookupRoot.FindComponent(Name)
          else
            Result := nil;

          if not Assigned(Result) then
          begin
            if Assigned(FOnAncestorNotFound) then
              FOnAncestorNotFound(Self, Name,
                FindComponentClass(CompClassName), Result);
            if not Assigned(Result) then
              raise EReadError.CreateFmt(SAncestorNotFound, [Name]);
          end;

          Parent := Result.GetParentComponent;
          if not Assigned(Parent) then
            Parent := Root;
        end else
        begin
          Result := nil;
          ComponentClass := FindComponentClass(CompClassName);
          if Assigned(FOnCreateComponent) then
            FOnCreateComponent(Self, ComponentClass, Result);
          if not Assigned(Result) then
          begin
           asm
           NewComponent = Object.create(ComponentClass);
           NewComponent.$init();
           end;
            if ffInline in Flags then
              NewComponent.FComponentState :=
                NewComponent.FComponentState + [csLoading, csInline];
            NewComponent.Create(Owner);
            NewComponent.AfterConstruction;

            { Don't set Result earlier because else we would come in trouble
              with the exception recover mechanism! (Result should be NIL if
              an error occurred) }
            Result := NewComponent;
          end;
          Include(Result.FComponentState, csLoading);
        end;
      except
        On E: Exception do
          if not Recover(E,Result) then
            raise;
      end;

    if Assigned(Result) then
      try
        Include(Result.FComponentState, csLoading);

        { create list of subcomponents and set loading}
        SubComponents := TList.Create;
        for n := 0 to Result.ComponentCount - 1 do
        begin
          C := Result.Components[n];
          if csSubcomponent in C.ComponentStyle
          then begin
            SubComponents.Add(C);
            Include(C.FComponentState, csLoading);
          end;
        end;

        if not (ffInherited in Flags) then
          try
            Result.SetParentComponent(Parent);
            if Assigned(FOnSetName) then
              FOnSetName(Self, Result, Name);
            Result.Name := Name;
            if FindGlobalComponent(Name) = Result then
              Include(Result.FComponentState, csInline);
          except
            On E : Exception do
              if not Recover(E,Result) then
                raise;
          end;
        if not Assigned(Result) then
          exit;
        if csInline in Result.ComponentState then
          FLookupRoot := Result;

        { Read the component state }
        Include(Result.FComponentState, csReading);
        for n := 0 to Subcomponents.Count - 1 do
          Include(TComponent(Subcomponents[n]).FComponentState, csReading);

        Result.ReadState(Self);

        Exclude(Result.FComponentState, csReading);
        for n := 0 to Subcomponents.Count - 1 do
          Exclude(TComponent(Subcomponents[n]).FComponentState, csReading);

        if ffChildPos in Flags then
          Parent.SetChildOrder(Result, ChildPos);

        { Add component to list of loaded components, if necessary }
        if (not ((ffInherited in Flags) or (csInline in Result.ComponentState))) or
          (FLoaded.IndexOf(Result) < 0)
          then begin
            for n := 0 to Subcomponents.Count - 1 do
              FLoaded.Add(Subcomponents[n]);
            FLoaded.Add(Result);
          end;
      except
        if ((ffInherited in Flags) or Assigned(Component)) then
          Result.Free;
        raise;
      end;
  finally
    Parent := SavedParent;
    FLookupRoot := SavedLookupRoot;
    Subcomponents.Free;
  end;
end;

procedure TReader.ReadData(Instance: TComponent);
var
  SavedOwner, SavedParent: TComponent;

begin
  { Read properties }
  while not EndOfList do
    ReadProperty(Instance);
  ReadListEnd;

  { Read children }
  SavedOwner := Owner;
  SavedParent := Parent;
  try
    Owner := Instance.GetChildOwner;
    if not Assigned(Owner) then
      Owner := Root;
    Parent := Instance.GetChildParent;

    while not EndOfList do
      ReadComponent(nil);
    ReadListEnd;
  finally
    Owner := SavedOwner;
    Parent := SavedParent;
  end;

  { Fixup references if necessary (normally only if this is the root) }
  If (Instance=FRoot) then
    DoFixupReferences;
end;

function TReader.ReadFloat: Extended;
begin
  if FDriver.NextValue = vaExtended then
  begin
    ReadValue;
    Result := FDriver.ReadFloat
  end else
    Result := ReadNativeInt;
end;

procedure TReader.ReadSignature;
begin
  FDriver.ReadSignature;
end;


function TReader.ReadCurrency: Currency;
begin
  if FDriver.NextValue = vaCurrency then
  begin
    FDriver.ReadValue;
    Result := FDriver.ReadCurrency;
  end else
    Result := ReadInteger;
end;


function TReader.ReadIdent: String;
var
  ValueType: TValueType;
begin
  ValueType := FDriver.ReadValue;
  if ValueType in [vaIdent, vaNil, vaFalse, vaTrue, vaNull] then
    Result := FDriver.ReadIdent(ValueType)
  else
    raise EReadError.Create(SInvalidPropertyValue);
end;


function TReader.ReadInteger: LongInt;
begin
  case FDriver.ReadValue of
    vaInt8:
      Result := FDriver.ReadInt8;
    vaInt16:
      Result := FDriver.ReadInt16;
    vaInt32:
      Result := FDriver.ReadInt32;
  else
    raise EReadError.Create(SInvalidPropertyValue);
  end;
end;

function TReader.ReadNativeInt: NativeInt;
begin
  if FDriver.NextValue = vaInt64 then
  begin
    FDriver.ReadValue;
    Result := FDriver.ReadNativeInt;
  end else
    Result := ReadInteger;
end;

function TReader.ReadSet(EnumType: Pointer): Integer;
begin
  if FDriver.NextValue = vaSet then
    begin
      FDriver.ReadValue;
      Result := FDriver.ReadSet(enumtype);
    end
  else
    Result := ReadInteger;
end;

procedure TReader.ReadListBegin;
begin
  CheckValue(vaList);
end;

procedure TReader.ReadListEnd;
begin
  CheckValue(vaNull);
end;

function TReader.ReadVariant: JSValue;
var
  nv: TValueType;
begin
  nv:=NextValue;
  case nv of
    vaNil:
      begin
        Result:=Undefined;
        readvalue;
      end;
    vaNull:
      begin
        Result:=Nil;
        readvalue;
      end;
    { all integer sizes must be split for big endian systems }
    vaInt8,vaInt16,vaInt32:
      begin
        Result:=ReadInteger;
      end;
    vaInt64:
      begin
        Result:=ReadNativeInt;
      end;
{
    vaQWord:
      begin
        Result:=QWord(ReadInt64);
      end;
}    vaFalse,vaTrue:
      begin
        Result:=(nv<>vaFalse);
        readValue;
      end;
    vaCurrency:
      begin
        Result:=ReadCurrency;
      end;
    vaDouble:
      begin
        Result:=ReadFloat;
      end;
    vaString:
      begin
        Result:=ReadString;
      end;
    else
      raise EReadError.CreateFmt(SUnsupportedPropertyVariantType, [Ord(nv)]);
  end;
end;

procedure TReader.ReadProperty(AInstance: TPersistent);
var
  Path: String;
  Instance: TPersistent;
  PropInfo: TTypeMemberProperty;
  Obj: TObject;
  Name: String;
  Skip: Boolean;
  Handled: Boolean;
  OldPropName: String;
  DotPos : String;
  NextPos: Integer;

  function HandleMissingProperty(IsPath: Boolean): boolean;
  begin
    Result:=true;
    if Assigned(OnPropertyNotFound) then begin
      // user defined property error handling
      OldPropName:=FPropName;
      Handled:=false;
      Skip:=false;
      OnPropertyNotFound(Self,Instance,FPropName,IsPath,Handled,Skip);
      if Handled and (not Skip) and (OldPropName<>FPropName) then
        // try alias property
        PropInfo := GetPropInfo(Instance.ClassType, FPropName);
      if Skip then begin
        FDriver.SkipValue;
        Result:=false;
        exit;
      end;
    end;
  end;

begin
  try
    Path := FDriver.BeginProperty;
    try
      Instance := AInstance;
      FCanHandleExcepts := True;
      DotPos := Path;
      while True do
      begin
        NextPos := Pos('.',DotPos);
        if NextPos>0 then
          FPropName := Copy(DotPos, 1, NextPos-1)
        else
        begin
          FPropName := DotPos;
          break;
        end;
        Delete(DotPos,1,NextPos);

        PropInfo := GetPropInfo(Instance.ClassType, FPropName);
        if not Assigned(PropInfo) then begin
          if not HandleMissingProperty(true) then exit;
          if not Assigned(PropInfo) then
            PropertyError;
        end;

        if PropInfo.TypeInfo.Kind = tkClass then
          Obj := TObject(GetObjectProp(Instance, PropInfo))
        //else if PropInfo^.PropType^.Kind = tkInterface then
        //  Obj := TObject(GetInterfaceProp(Instance, PropInfo))
        else
          Obj := nil;

        if not (Obj is TPersistent) then
        begin
          { All path elements must be persistent objects! }
          FDriver.SkipValue;
          raise EReadError.Create(SInvalidPropertyPath);
        end;
        Instance := TPersistent(Obj);
      end;

      PropInfo := GetPropInfo(Instance.ClassType, FPropName);
      if Assigned(PropInfo) then
        ReadPropValue(Instance, PropInfo)
      else
      begin
        FCanHandleExcepts := False;
        Instance.DefineProperties(Self);
        FCanHandleExcepts := True;
        if Length(FPropName) > 0 then begin
          if not HandleMissingProperty(false) then exit;
          if not Assigned(PropInfo) then
            PropertyError;
        end;
      end;
    except
      on e: Exception do
      begin
        SetLength(Name, 0);
        if AInstance.InheritsFrom(TComponent) then
          Name := TComponent(AInstance).Name;
        if Length(Name) = 0 then
          Name := AInstance.ClassName;
        raise EReadError.CreateFmt(SPropertyException, [Name, '.', Path, e.Message]);
      end;
    end;
  except
    on e: Exception do
      if not FCanHandleExcepts or not Error(E.Message) then
        raise;
  end;
end;

procedure TReader.ReadPropValue(Instance: TPersistent; PropInfo: TTypeMemberProperty);
const
  NullMethod: TMethod = (Code: nil; Data: nil);
var
  PropType: TTypeInfo;
  Value: LongInt;
{  IdentToIntFn: TIdentToInt; }
  Ident: String;
  Method: TMethod;
  Handled: Boolean;
  TmpStr: String;
begin
  if (PropInfo.Setter='') then
    raise EReadError.Create(SReadOnlyProperty);

  PropType := PropInfo.TypeInfo;
  case PropType.Kind of
    tkInteger:
      case FDriver.NextValue of
        vaIdent :
          begin
          Ident := ReadIdent;
          if GlobalIdentToInt(Ident,Value) then
            SetOrdProp(Instance, PropInfo, Value)
          else
            raise EReadError.Create(SInvalidPropertyValue);
          end;
        vaNativeInt :
          SetOrdProp(Instance, PropInfo, ReadNativeInt);
        vaCurrency:
          SetFloatProp(Instance, PropInfo, ReadCurrency);
      else
        SetOrdProp(Instance, PropInfo, ReadInteger);
      end;
    tkBool:
      SetBoolProp(Instance, PropInfo, ReadBoolean);
    tkChar:
      SetOrdProp(Instance, PropInfo, Ord(ReadChar));
    tkEnumeration:
      begin
        Value := GetEnumValue(TTypeInfoEnum(PropType), ReadIdent);
        if Value = -1 then
          raise EReadError.Create(SInvalidPropertyValue);
        SetOrdProp(Instance, PropInfo, Value);
      end;
{$ifndef FPUNONE}
    tkFloat:
      SetFloatProp(Instance, PropInfo, ReadFloat);
{$endif}
    tkSet:
      begin
        CheckValue(vaSet);
        if TTypeInfoSet(PropType).CompType.Kind=tkEnumeration then
          SetOrdProp(Instance, PropInfo, FDriver.ReadSet(TTypeInfoEnum(TTypeInfoSet(PropType).CompType)));
      end;
    tkMethod, tkRefToProcVar:
      if FDriver.NextValue = vaNil then
      begin
        FDriver.ReadValue;
        SetMethodProp(Instance, PropInfo, NullMethod);
      end else
      begin
        Handled:=false;
        Ident:=ReadIdent;
        if Assigned(OnSetMethodProperty) then
          OnSetMethodProperty(Self,Instance,PropInfo,Ident,Handled);
        if not Handled then begin
          Method.Code := FindMethod(Root, Ident);
          Method.Data := Root;
          if Assigned(Method.Code) then
            SetMethodProp(Instance, PropInfo, Method);
        end;
      end;
    tkString:
      begin
        TmpStr:=ReadString;
        if Assigned(FOnReadStringProperty) then
          FOnReadStringProperty(Self,Instance,PropInfo,TmpStr);
        SetStrProp(Instance, PropInfo, TmpStr);
      end;
    tkJSValue:
      begin
        SetJSValueProp(Instance,PropInfo,ReadVariant);
      end;
    tkClass, tkInterface:
      case FDriver.NextValue of
        vaNil:
          begin
            FDriver.ReadValue;
            SetOrdProp(Instance, PropInfo, 0)
          end;
        vaCollection:
          begin
            FDriver.ReadValue;
            ReadCollection(TCollection(GetObjectProp(Instance, PropInfo)));
          end
        else
          begin
          If Not Assigned(FFixups) then
            FFixups:=TLinkedList.Create(TLocalUnresolvedReference);
          With TLocalUnresolvedReference(TLinkedList(FFixups).Add) do
            begin
            FInstance:=Instance;
            FRoot:=Root;
            FPropInfo:=PropInfo;
            FRelative:=ReadIdent;
            end;
          end;
      end;
    {tkint64:
      SetInt64Prop(Instance, PropInfo, ReadInt64);}
    else
      raise EReadError.CreateFmt(SUnknownPropertyType, [Str(PropType.Kind)]);
  end;
end;

function TReader.ReadRootComponent(ARoot: TComponent): TComponent;
var
  Dummy, i: Integer;
  Flags: TFilerFlags;
  CompClassName, CompName, ResultName: String;
begin
  FDriver.BeginRootComponent;
  Result := nil;
  {!!!: GlobalNameSpace.BeginWrite;  // Loading from stream adds to name space
  try}
    try
      FDriver.BeginComponent(Flags, Dummy, CompClassName, CompName);
      if not Assigned(ARoot) then
      begin
        { Read the class name and the object name and create a new object: }
        Result := TComponentClass(FindClass(CompClassName)).Create(nil);
        Result.Name := CompName;
      end else
      begin
        Result := ARoot;

        if not (csDesigning in Result.ComponentState) then
        begin
          Result.FComponentState :=
            Result.FComponentState + [csLoading, csReading];

          { We need an unique name }
          i := 0;
          { Don't use Result.Name directly, as this would influence
            FindGlobalComponent in successive loop runs }
          ResultName := CompName;
            while Assigned(FindGlobalComponent(ResultName)) do
            begin
              Inc(i);
              ResultName := CompName + '_' + IntToStr(i);
            end;
            Result.Name := ResultName;
        end;
      end;

      FRoot := Result;
      FLookupRoot := Result;
      if Assigned(GlobalLoaded) then
        FLoaded := GlobalLoaded
      else
        FLoaded := TFpList.Create;

      try
        if FLoaded.IndexOf(FRoot) < 0 then
          FLoaded.Add(FRoot);
        FOwner := FRoot;
        FRoot.FComponentState := FRoot.FComponentState + [csLoading, csReading];
        FRoot.ReadState(Self);
        Exclude(FRoot.FComponentState, csReading);

        if not Assigned(GlobalLoaded) then
          for i := 0 to FLoaded.Count - 1 do
            TComponent(FLoaded[i]).Loaded;

      finally
        if not Assigned(GlobalLoaded) then
          FLoaded.Free;
        FLoaded := nil;
      end;
      GlobalFixupReferences;
    except
      RemoveFixupReferences(ARoot, '');
      if not Assigned(ARoot) then
        Result.Free;
      raise;
    end;
  {finally
    GlobalNameSpace.EndWrite;
  end;}
end;

procedure TReader.ReadComponents(AOwner, AParent: TComponent;
  Proc: TReadComponentsProc);
var
  Component: TComponent;
begin
  Root := AOwner;
  Owner := AOwner;
  Parent := AParent;
  BeginReferences;
  try
    while not EndOfList do
    begin
      FDriver.BeginRootComponent;
      Component := ReadComponent(nil);
      if Assigned(Proc) then
        Proc(Component);
    end;
    ReadListEnd;
    FixupReferences;
  finally
    EndReferences;
  end;
end;


function TReader.ReadString: String;
var
  StringType: TValueType;
begin
  StringType := FDriver.ReadValue;
  if StringType=vaString then
    Result := FDriver.ReadString(StringType)
  else
    raise EReadError.Create(SInvalidPropertyValue);
end;


function TReader.ReadWideString: WideString;

begin
  Result:=ReadString;
end;

function TReader.ReadUnicodeString: UnicodeString;

begin
  Result:=ReadString;
end;

function TReader.ReadValue: TValueType;
begin
  Result := FDriver.ReadValue;
end;

procedure TReader.CopyValue(Writer: TWriter);

(*
procedure CopyBytes(Count: Integer);
{  var
    Buffer: array[0..1023] of Byte; }
  begin
{!!!:    while Count > 1024 do
    begin
      FDriver.Read(Buffer, 1024);
      Writer.Driver.Write(Buffer, 1024);
      Dec(Count, 1024);
    end;
    if Count > 0 then
    begin
      FDriver.Read(Buffer, Count);
      Writer.Driver.Write(Buffer, Count);
    end;}
  end;
*)

{var
  s: String;
  Count: LongInt; }
begin
  case FDriver.NextValue of
    vaNull:
      Writer.WriteIdent('NULL');
    vaFalse:
      Writer.WriteIdent('FALSE');
    vaTrue:
      Writer.WriteIdent('TRUE');
    vaNil:
      Writer.WriteIdent('NIL');
    {!!!: vaList, vaCollection:
      begin
        Writer.WriteValue(FDriver.ReadValue);
        while not EndOfList do
          CopyValue(Writer);
        ReadListEnd;
        Writer.WriteListEnd;
      end;}
    vaInt8, vaInt16, vaInt32:
      Writer.WriteInteger(ReadInteger);
{$ifndef FPUNONE}
    vaExtended:
      Writer.WriteFloat(ReadFloat);
{$endif}
    vaString:
      Writer.WriteString(ReadString);
    vaIdent:
      Writer.WriteIdent(ReadIdent);
    {!!!: vaBinary, vaLString, vaWString:
      begin
        Writer.WriteValue(FDriver.ReadValue);
        FDriver.Read(Count, SizeOf(Count));
        Writer.Driver.Write(Count, SizeOf(Count));
        CopyBytes(Count);
      end;}
    {!!!: vaSet:
      Writer.WriteSet(ReadSet);}
    {!!!: vaCurrency:
      Writer.WriteCurrency(ReadCurrency);}
    vaInt64:
      Writer.WriteInteger(ReadNativeInt);
  end;
end;

function TReader.FindComponentClass(const AClassName: String): TComponentClass;

var
  PersistentClass: TPersistentClass;

  function FindClassInFieldTable(Instance: TComponent): TComponentClass;
  var
    aClass: TClass;
    i: longint;
    ClassTI, MemberClassTI: TTypeInfoClass;
    MemberTI: TTypeInfo;
  begin
    aClass:=Instance.ClassType;
    while aClass<>nil do
      begin
      ClassTI:=typeinfo(aClass);
      for i:=0 to ClassTI.FieldCount-1 do
        begin
        MemberTI:=ClassTI.GetField(i).TypeInfo;
        if MemberTI.Kind=tkClass then
          begin
          MemberClassTI:=TTypeInfoClass(MemberTI);
          if SameText(MemberClassTI.Name,aClassName)
              and (MemberClassTI.ClassType is TComponent) then
            exit(TComponentClass(MemberClassTI.ClassType));
          end;
        end;
      aClass:=aClass.ClassParent;
      end;
  end;

begin
  Result := nil;
  Result:=FindClassInFieldTable(Root);

  if (Result=nil) and assigned(LookupRoot) and (LookupRoot<>Root) then
    Result:=FindClassInFieldTable(LookupRoot);

  if (Result=nil) then begin
    PersistentClass := GetClass(AClassName);
    if PersistentClass.InheritsFrom(TComponent) then
      Result := TComponentClass(PersistentClass);
  end;

  if (Result=nil) and assigned(OnFindComponentClass) then
    OnFindComponentClass(Self, AClassName, Result);

  if (Result=nil) or (not Result.InheritsFrom(TComponent)) then
    raise EClassNotFound.CreateFmt(SClassNotFound, [AClassName]);
end;


{ TAbstractObjectReader }

procedure TAbstractObjectReader.FlushBuffer;
begin
  // Do nothing
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
{*                         TBinaryObjectWriter                              *}
{****************************************************************************}


procedure TBinaryObjectWriter.WriteWord(w : word);
begin
  FStream.WriteBufferData(w);
end;

procedure TBinaryObjectWriter.WriteDWord(lw : longword);
begin
  FStream.WriteBufferData(lw);
end;

constructor TBinaryObjectWriter.Create(Stream: TStream);
begin
  inherited Create;
  If (Stream=Nil) then
    Raise EWriteError.Create(SEmptyStreamIllegalWriter);
  FStream := Stream;
end;

procedure TBinaryObjectWriter.BeginCollection;
begin
  WriteValue(vaCollection);
end;

procedure TBinaryObjectWriter.WriteSignature;

begin
  FStream.WriteBufferData(FilerSignatureInt);
end;

procedure TBinaryObjectWriter.BeginComponent(Component: TComponent;
  Flags: TFilerFlags; ChildPos: Integer);
var
  Prefix: Byte;
begin

  { Only write the flags if they are needed! }
  if Flags <> [] then
  begin
    Prefix:=0;
    if ffInherited in Flags then
     Prefix:=Prefix or $01;
    if ffChildPos in Flags then
     Prefix:=Prefix or $02;
    if ffInline in Flags then
     Prefix:=Prefix or $04;
    Prefix := Prefix  or $f0;
    FStream.WriteBufferData(Prefix);
    if ffChildPos in Flags then
      WriteInteger(ChildPos);
  end;

  WriteStr(Component.ClassName);
  WriteStr(Component.Name);
end;

procedure TBinaryObjectWriter.BeginList;
begin
  WriteValue(vaList);
end;

procedure TBinaryObjectWriter.EndList;
begin
  WriteValue(vaNull);
end;

procedure TBinaryObjectWriter.BeginProperty(const PropName: String);
begin
  WriteStr(PropName);
end;

procedure TBinaryObjectWriter.EndProperty;
begin
end;

procedure TBinaryObjectWriter.FlushBuffer;
begin
  // Do nothing;
end;

procedure TBinaryObjectWriter.WriteBinary(const Buffer : TBytes; Count: LongInt);
begin
  WriteValue(vaBinary);
  WriteDWord(longword(Count));
  FStream.Write(Buffer, Count);
end;

procedure TBinaryObjectWriter.WriteBoolean(Value: Boolean);
begin
  if Value then
    WriteValue(vaTrue)
  else
    WriteValue(vaFalse);
end;

procedure TBinaryObjectWriter.WriteFloat(const Value: Extended);
begin
  WriteValue(vaDouble);
  FStream.WriteBufferData(Value);
end;


procedure TBinaryObjectWriter.WriteCurrency(const Value: Currency);

Var
  F : Double;
begin
  WriteValue(vaCurrency);
  F:=Value;
  FStream.WriteBufferData(F);
end;

procedure TBinaryObjectWriter.WriteIdent(const Ident: string);
begin
  { Check if Ident is a special identifier before trying to just write
    Ident directly }
  if UpperCase(Ident) = 'NIL' then
    WriteValue(vaNil)
  else if UpperCase(Ident) = 'FALSE' then
    WriteValue(vaFalse)
  else if UpperCase(Ident) = 'TRUE' then
    WriteValue(vaTrue)
  else if UpperCase(Ident) = 'NULL' then
    WriteValue(vaNull) else
  begin
    WriteValue(vaIdent);
    WriteStr(Ident);
  end;
end;

procedure TBinaryObjectWriter.WriteInteger(Value: NativeInt);
var
  s: ShortInt;
  i: SmallInt;
  l: Longint;
begin
  { Use the smallest possible integer type for the given value: }
  if (Value >= -128) and (Value <= 127) then
  begin
    WriteValue(vaInt8);
    s := Value;
    FStream.WriteBufferData(s);
  end else if (Value >= -32768) and (Value <= 32767) then
  begin
    WriteValue(vaInt16);
    i := Value;
    WriteWord(word(i));
  end else if (Value >= -$80000000) and (Value <= $7fffffff) then
  begin
    WriteValue(vaInt32);
    l := Value;
    WriteDWord(longword(l));
  end else
  begin
    WriteValue(vaInt64);
    FStream.WriteBufferData(Value);
  end;
end;

procedure TBinaryObjectWriter.WriteNativeInt(Value: NativeInt);
var
  s: Int8;
  i: Int16;
  l: Int32;
begin
  { Use the smallest possible integer type for the given value: }
  if (Value <= 127) then
  begin
    WriteValue(vaInt8);
    s := Value;
    FStream.WriteBufferData(s);
  end else if (Value <= 32767) then
  begin
    WriteValue(vaInt16);
    i := Value;
    WriteWord(word(i));
  end else if (Value <= $7fffffff) then
  begin
    WriteValue(vaInt32);
    l := Value;
    WriteDWord(longword(l));
  end else
  begin
    WriteValue(vaQWord);
    FStream.WriteBufferData(Value);
  end;
end;


procedure TBinaryObjectWriter.WriteMethodName(const Name: String);
begin
  if Length(Name) > 0 then
  begin
    WriteValue(vaIdent);
    WriteStr(Name);
  end else
    WriteValue(vaNil);
end;

procedure TBinaryObjectWriter.WriteSet(Value: LongInt; SetType: Pointer);
var
  i: Integer;
  b : Integer;
begin
  WriteValue(vaSet);
  B:=1;
  for i:=0 to 31 do
    begin
    if (Value and b) <>0 then
      begin
      WriteStr(GetEnumName(PTypeInfo(SetType), i));
      end;
    b:=b shl 1;
    end;
  WriteStr('');
end;

procedure TBinaryObjectWriter.WriteString(const Value: String);

var
  i, len: Integer;
begin
  len := Length(Value);
  WriteValue(vaString);
  WriteDWord(len);
  For I:=1 to len do
    FStream.WriteBufferData(Value[i]);
end;

procedure TBinaryObjectWriter.WriteWideString(const Value: WideString);

begin
  WriteString(Value);
end;

procedure TBinaryObjectWriter.WriteUnicodeString(const Value: UnicodeString);

begin
  WriteString(Value);
end;

procedure TBinaryObjectWriter.WriteVariant(const VarValue: JSValue);
begin
  if isUndefined(varValue) then
    WriteValue(vaNil)
  else if IsNull(VarValue) then
    WriteValue(vaNull)
  else if IsNumber(VarValue) then
    begin
    if Frac(Double(varValue))=0 then
      WriteInteger(NativeInt(VarValue))
     else
      WriteFloat(Double(varValue))
    end
  else if isBoolean(varValue) then
    WriteBoolean(Boolean(VarValue))
  else if isString(varValue) then
    WriteString(String(VarValue))
  else
    raise EWriteError.Create(SUnsupportedPropertyVariantType);
end;


procedure TBinaryObjectWriter.Write(const Buffer : TBytes; Count: LongInt);

begin
  FStream.Write(Buffer,Count);
end;

procedure TBinaryObjectWriter.WriteValue(Value: TValueType);
var
  b: uint8;
begin
  b := uint8(Value);
  FStream.WriteBufferData(b);
end;

procedure TBinaryObjectWriter.WriteStr(const Value: String);
var
  len,i: integer;
  b: uint8;
begin
  len:= Length(Value);
  if len > 255 then
    len := 255;
  b := len;
  FStream.WriteBufferData(b);
  For I:=1 to len do
    FStream.WriteBufferData(Value[i]);
end;



{****************************************************************************}
{*                             TWriter                                      *}
{****************************************************************************}


constructor TWriter.Create(ADriver: TAbstractObjectWriter);
begin
  inherited Create;
  FDriver := ADriver;
end;

constructor TWriter.Create(Stream: TStream);
begin
  inherited Create;
  If (Stream=Nil) then
    Raise EWriteError.Create(SEmptyStreamIllegalWriter);
  FDriver := CreateDriver(Stream);
  FDestroyDriver := True;
end;

destructor TWriter.Destroy;
begin
  if FDestroyDriver then
    FDriver.Free;
  inherited Destroy;
end;

function TWriter.CreateDriver(Stream: TStream): TAbstractObjectWriter;
begin
  Result := TBinaryObjectWriter.Create(Stream);
end;

Type
  TPosComponent = Class(TObject)
  Private
    FPos : Integer;
    FComponent : TComponent;
  Public
    Constructor Create(APos : Integer; AComponent : TComponent);
  end;

Constructor TPosComponent.Create(APos : Integer; AComponent : TComponent);

begin
  FPos:=APos;
  FComponent:=AComponent;
end;

// Used as argument for calls to TComponent.GetChildren:
procedure TWriter.AddToAncestorList(Component: TComponent);
begin
  FAncestors.AddObject(Component.Name,TPosComponent.Create(FAncestors.Count,Component));
end;

procedure TWriter.DefineProperty(const Name: String;
  ReadData: TReaderProc; AWriteData: TWriterProc; HasData: Boolean);
begin
  if HasData and Assigned(AWriteData) then
  begin
    // Write the property name and then the data itself
    Driver.BeginProperty(FPropPath + Name);
    AWriteData(Self);
    Driver.EndProperty;
  end else if assigned(ReadData) then ;
end;

procedure TWriter.DefineBinaryProperty(const Name: String;
  ReadData, AWriteData: TStreamProc; HasData: Boolean);
begin
  if HasData and Assigned(AWriteData) then
  begin
    // Write the property name and then the data itself
    Driver.BeginProperty(FPropPath + Name);
    WriteBinary(AWriteData);
    Driver.EndProperty;
  end else if assigned(ReadData) then ;
end;

procedure TWriter.FlushBuffer;
begin
  Driver.FlushBuffer;
end;

procedure TWriter.Write(const Buffer : TBytes; Count: Longint);
begin
  //This should give an exception if write is not implemented (i.e. TTextObjectWriter)
  //but should work with TBinaryObjectWriter.
  Driver.Write(Buffer, Count);
end;

procedure TWriter.SetRoot(ARoot: TComponent);
begin
  inherited SetRoot(ARoot);
  // Use the new root as lookup root too
  FLookupRoot := ARoot;
end;

procedure TWriter.WriteSignature;

begin
  FDriver.WriteSignature;
end;

procedure TWriter.WriteBinary(AWriteData: TStreamProc);
var
  MemBuffer: TBytesStream;
begin
  { First write the binary data into a memory stream, then copy this buffered
    stream into the writing destination. This is necessary as we have to know
    the size of the binary data in advance (we're assuming that seeking within
    the writer stream is not possible) }
  MemBuffer := TBytesStream.Create;
  try
    AWriteData(MemBuffer);
    Driver.WriteBinary(MemBuffer.Bytes, MemBuffer.Size);
  finally
    MemBuffer.Free;
  end;
end;

procedure TWriter.WriteBoolean(Value: Boolean);
begin
  Driver.WriteBoolean(Value);
end;

procedure TWriter.WriteChar(Value: Char);
begin
  WriteString(Value);
end;

procedure TWriter.WriteWideChar(Value: WideChar);
begin
  WriteWideString(Value);
end;

procedure TWriter.WriteCollection(Value: TCollection);
var
  i: Integer;
begin
  Driver.BeginCollection;
  if Assigned(Value) then
    for i := 0 to Value.Count - 1 do
    begin
      { Each collection item needs its own ListBegin/ListEnd tag, or else the
        reader wouldn't be able to know where an item ends and where the next
        one starts }
      WriteListBegin;
      WriteProperties(Value.Items[i]);
      WriteListEnd;
    end;
  WriteListEnd;
end;

procedure TWriter.DetermineAncestor(Component : TComponent);

Var
  I : Integer;

begin
  // Should be set only when we write an inherited with children.
  if Not Assigned(FAncestors) then
    exit;
  I:=FAncestors.IndexOf(Component.Name);
  If (I=-1) then
    begin
    FAncestor:=Nil;
    FAncestorPos:=-1;
    end
  else
    With TPosComponent(FAncestors.Objects[i]) do
      begin
      FAncestor:=FComponent;
      FAncestorPos:=FPos;
      end;
end;

procedure TWriter.DoFindAncestor(Component : TComponent);

Var
  C : TComponent;

begin
  if Assigned(FOnFindAncestor) then
    if (Ancestor=Nil) or (Ancestor is TComponent) then
      begin
      C:=TComponent(Ancestor);
      FOnFindAncestor(Self,Component,Component.Name,C,FRootAncestor);
      Ancestor:=C;
      end;
end;

procedure TWriter.WriteComponent(Component: TComponent);

var
  SA : TPersistent;
  SR, SRA : TComponent;
begin
  SR:=FRoot;
  SA:=FAncestor;
  SRA:=FRootAncestor;
  Try
    Component.FComponentState:=Component.FComponentState+[csWriting];
    Try
      // Possibly set ancestor.
      DetermineAncestor(Component);
      DoFindAncestor(Component); // Mainly for IDE when a parent form had an ancestor renamed...
      // Will call WriteComponentData.
      Component.WriteState(Self);
      FDriver.EndList;
    Finally
      Component.FComponentState:=Component.FComponentState-[csWriting];
    end;
  Finally
    FAncestor:=SA;
    FRoot:=SR;
    FRootAncestor:=SRA;
  end;
end;

procedure TWriter.WriteChildren(Component : TComponent);

Var
  SRoot, SRootA : TComponent;
  SList : TStringList;
  SPos, I , SAncestorPos: Integer;
  O : TObject;

begin
  // Write children list.
  // While writing children, the ancestor environment must be saved
  // This is recursive...
  SRoot:=FRoot;
  SRootA:=FRootAncestor;
  SList:=FAncestors;
  SPos:=FCurrentPos;
  SAncestorPos:=FAncestorPos;
  try
    FAncestors:=Nil;
    FCurrentPos:=0;
    FAncestorPos:=-1;
    if csInline in Component.ComponentState then
       FRoot:=Component;
    if (FAncestor is TComponent) then
       begin
       FAncestors:=TStringList.Create;
       if csInline in TComponent(FAncestor).ComponentState then
         FRootAncestor := TComponent(FAncestor);
       TComponent(FAncestor).GetChildren(@AddToAncestorList,FRootAncestor);
       FAncestors.Sorted:=True;
       end;
    try
      Component.GetChildren(@WriteComponent, FRoot);
    Finally
      If Assigned(Fancestors) then
        For I:=0 to FAncestors.Count-1 do
          begin
          O:=FAncestors.Objects[i];
          FAncestors.Objects[i]:=Nil;
          O.Free;
          end;
      FreeAndNil(FAncestors);
    end;
  finally
    FAncestors:=Slist;
    FRoot:=SRoot;
    FRootAncestor:=SRootA;
    FCurrentPos:=SPos;
    FAncestorPos:=SAncestorPos;
  end;
end;

procedure TWriter.WriteComponentData(Instance: TComponent);
var
  Flags: TFilerFlags;
begin
  Flags := [];
  If (Assigned(FAncestor)) and  //has ancestor
     (not (csInline in Instance.ComponentState) or // no inline component
      // .. or the inline component is inherited
      (csAncestor in Instance.Componentstate) and (FAncestors <> nil)) then
    Flags:=[ffInherited]
  else If csInline in Instance.ComponentState then
    Flags:=[ffInline];
  If (FAncestors<>Nil) and ((FCurrentPos<>FAncestorPos) or (FAncestor=Nil)) then
    Include(Flags,ffChildPos);
  FDriver.BeginComponent(Instance,Flags,FCurrentPos);
  If (FAncestors<>Nil) then
    Inc(FCurrentPos);
  WriteProperties(Instance);
  WriteListEnd;
  // Needs special handling of ancestor.
  If not IgnoreChildren then
    WriteChildren(Instance);
end;

procedure TWriter.WriteDescendent(ARoot: TComponent; AAncestor: TComponent);
begin
  FRoot := ARoot;
  FAncestor := AAncestor;
  FRootAncestor := AAncestor;
  FLookupRoot := ARoot;
  WriteSignature;
  WriteComponent(ARoot);
end;

procedure TWriter.WriteFloat(const Value: Extended);
begin
  Driver.WriteFloat(Value);
end;


procedure TWriter.WriteCurrency(const Value: Currency);
begin
  Driver.WriteCurrency(Value);
end;


procedure TWriter.WriteIdent(const Ident: string);
begin
  Driver.WriteIdent(Ident);
end;

procedure TWriter.WriteInteger(Value: LongInt);
begin
  Driver.WriteInteger(Value);
end;

procedure TWriter.WriteInteger(Value: NativeInt);
begin
  Driver.WriteInteger(Value);
end;

procedure TWriter.WriteSet(Value: LongInt; SetType: Pointer);

begin
  Driver.WriteSet(Value,SetType);
end;

procedure TWriter.WriteVariant(const VarValue: JSValue);
begin
  Driver.WriteVariant(VarValue);
end;

procedure TWriter.WriteListBegin;
begin
  Driver.BeginList;
end;

procedure TWriter.WriteListEnd;
begin
  Driver.EndList;
end;

procedure TWriter.WriteProperties(Instance: TPersistent);

var
  PropCount,i : integer;
  PropList  : TTypeMemberPropertyDynArray;

begin
  PropList:=GetPropList(Instance);
  PropCount:=Length(PropList);
  if PropCount>0 then
    for i := 0 to PropCount-1 do
      if IsStoredProp(Instance,PropList[i]) then
        WriteProperty(Instance,PropList[i]);
  Instance.DefineProperties(Self);
end;


procedure TWriter.WriteProperty(Instance: TPersistent; PropInfo: TTypeMemberProperty);
var
  HasAncestor: Boolean;
  PropType: TTypeInfo;
  N,Value, DefValue: LongInt;
  Ident: String;
  IntToIdentFn: TIntToIdent;
{$ifndef FPUNONE}
  FloatValue, DefFloatValue: Extended;
{$endif}
  MethodValue: TMethod;
  DefMethodValue: TMethod;
  StrValue, DefStrValue: String;
  AncestorObj: TObject;
  C,Component: TComponent;
  ObjValue: TObject;
  SavedAncestor: TPersistent;
  Key, SavedPropPath, Name, lMethodName: String;
  VarValue, DefVarValue : JSValue;
  BoolValue, DefBoolValue: boolean;
  Handled: Boolean;
  O : TJSObject;
begin
  // do not stream properties without getter
  if PropInfo.Getter='' then
    exit;
  // properties without setter are only allowed, if they are subcomponents
  PropType := PropInfo.TypeInfo;
  if (PropInfo.Setter='') then
    begin
    if PropType.Kind<>tkClass then
      exit;
    ObjValue := TObject(GetObjectProp(Instance, PropInfo));
    if not ObjValue.InheritsFrom(TComponent) or
       not (csSubComponent in TComponent(ObjValue).ComponentStyle) then
      exit;
    end;

  { Check if the ancestor can be used }
  HasAncestor := Assigned(Ancestor) and ((Instance = Root) or
    (Instance.ClassType = Ancestor.ClassType));
  //writeln('TWriter.WriteProperty Name=',PropType^.Name,' Kind=',GetEnumName(TypeInfo(TTypeKind),ord(PropType^.Kind)),' HasAncestor=',HasAncestor);

  case PropType.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet:
      begin
        Value := GetOrdProp(Instance, PropInfo);
        if HasAncestor then
          DefValue := GetOrdProp(Ancestor, PropInfo)
        else
          begin
          if PropType.Kind<>tkSet then
            DefValue := Longint(PropInfo.Default)
          else
            begin
            o:=TJSObject(PropInfo.Default);
            DefValue:=0;
            for Key in o do
              begin
              n:=parseInt(Key,10);
              if n<32 then
                DefValue:=DefValue+(1 shl n);
              end;
            end;
          end;
        // writeln(PPropInfo(PropInfo)^.Name, ', HasAncestor=', ord(HasAncestor), ', Value=', Value, ', Default=', DefValue);
        if (Value <> DefValue) or (DefValue=longint($80000000)) then
        begin
          Driver.BeginProperty(FPropPath + PropInfo.Name);
          case PropType.Kind of
            tkInteger:
              begin
                // Check if this integer has a string identifier
                IntToIdentFn := FindIntToIdent(PropInfo.TypeInfo);
                if Assigned(IntToIdentFn) and IntToIdentFn(Value, Ident) then
                  // Integer can be written a human-readable identifier
                  WriteIdent(Ident)
                else
                  // Integer has to be written just as number
                  WriteInteger(Value);
              end;
            tkChar:
              WriteChar(Chr(Value));
            tkSet:
              begin
              Driver.WriteSet(Value, TTypeInfoSet(PropType).CompType);
              end;
            tkEnumeration:
              WriteIdent(GetEnumName(TTypeInfoEnum(PropType), Value));
          end;
          Driver.EndProperty;
        end;
      end;
{$ifndef FPUNONE}
    tkFloat:
      begin
        FloatValue := GetFloatProp(Instance, PropInfo);
        if HasAncestor then
          DefFloatValue := GetFloatProp(Ancestor, PropInfo)
        else
          begin
          // This is really ugly..
          DefFloatValue:=Double(PropInfo.Default);
          end;
        if (FloatValue<>DefFloatValue) or (not HasAncestor and (int(DefFloatValue)=longint($80000000))) then
        begin
          Driver.BeginProperty(FPropPath + PropInfo.Name);
          WriteFloat(FloatValue);
          Driver.EndProperty;
        end;
      end;
{$endif}
    tkMethod:
      begin
        MethodValue := GetMethodProp(Instance, PropInfo);
        if HasAncestor then
          DefMethodValue := GetMethodProp(Ancestor, PropInfo)
        else begin
          DefMethodValue.Data := nil;
          DefMethodValue.Code := nil;
        end;

        Handled:=false;
        if Assigned(OnWriteMethodProperty) then
          OnWriteMethodProperty(Self,Instance,PropInfo,MethodValue,
            DefMethodValue,Handled);
        if isString(MethodValue.Code) then
          lMethodName:=String(MethodValue.Code)
        else
          lMethodName:=FLookupRoot.MethodName(MethodValue.Code);
        //Writeln('Writeln A: ',lMethodName);
        if (not Handled) and
          (MethodValue.Code <> DefMethodValue.Code) and
          ((not Assigned(MethodValue.Code)) or
          ((Length(lMethodName) > 0))) then
        begin
          //Writeln('Writeln B',FPropPath + PropInfo.Name);
          Driver.BeginProperty(FPropPath + PropInfo.Name);
          if Assigned(MethodValue.Code) then
            Driver.WriteMethodName(lMethodName)
          else
            Driver.WriteMethodName('');
          Driver.EndProperty;
        end;
      end;
    tkString: // tkSString, tkLString, tkAString are not supported
      begin
        StrValue := GetStrProp(Instance, PropInfo);
        if HasAncestor then
          DefStrValue := GetStrProp(Ancestor, PropInfo)
        else
        begin
          DefValue :=Longint(PropInfo.Default);
          SetLength(DefStrValue, 0);
        end;

        if (StrValue<>DefStrValue) or (not HasAncestor and (DefValue=longint($80000000))) then
        begin
          Driver.BeginProperty(FPropPath + PropInfo.Name);
          if Assigned(FOnWriteStringProperty) then
            FOnWriteStringProperty(Self,Instance,PropInfo,StrValue);
          WriteString(StrValue);
          Driver.EndProperty;
        end;
      end;
    tkJSValue:
      begin
        { Ensure that a Variant manager is installed }
        VarValue := GetJSValueProp(Instance, PropInfo);
        if HasAncestor then
          DefVarValue := GetJSValueProp(Ancestor, PropInfo)
        else
          DefVarValue:=null;

        if (VarValue<>DefVarValue) then
          begin
            Driver.BeginProperty(FPropPath + PropInfo.Name);
            { can't use variant() typecast, pulls in variants unit }
            WriteVariant(VarValue);
            Driver.EndProperty;
          end;
      end;
    tkClass:
      begin
        ObjValue := TObject(GetObjectProp(Instance, PropInfo));
        if HasAncestor then
        begin
          AncestorObj := TObject(GetObjectProp(Ancestor, PropInfo));
          if (AncestorObj is TComponent) and
             (ObjValue is TComponent) then
          begin
            //writeln('TWriter.WriteProperty AncestorObj=',TComponent(AncestorObj).Name,' OwnerFit=',TComponent(AncestorObj).Owner = FRootAncestor,' ',TComponent(ObjValue).Name,' OwnerFit=',TComponent(ObjValue).Owner = Root);
            if (AncestorObj<> ObjValue) and
             (TComponent(AncestorObj).Owner = FRootAncestor) and
             (TComponent(ObjValue).Owner = Root) and
             (UpperCase(TComponent(AncestorObj).Name) = UpperCase(TComponent(ObjValue).Name)) then
            begin
              // different components, but with the same name
              // treat it like an override
              AncestorObj := ObjValue;
            end;
          end;
        end else
          AncestorObj := nil;

        if not Assigned(ObjValue) then
          begin
          if ObjValue <> AncestorObj then
            begin
            Driver.BeginProperty(FPropPath + PropInfo.Name);
            Driver.WriteIdent('NIL');
            Driver.EndProperty;
            end
          end
        else if ObjValue.InheritsFrom(TPersistent) then
          begin
          { Subcomponents are streamed the same way as persistents }
          if ObjValue.InheritsFrom(TComponent)
            and ((not (csSubComponent in TComponent(ObjValue).ComponentStyle))
                 or ((TComponent(ObjValue).Owner<>Instance) and (TComponent(ObjValue).Owner<>Nil))) then
            begin
            Component := TComponent(ObjValue);
            if (ObjValue <> AncestorObj)
                and not (csTransient in Component.ComponentStyle) then
              begin
              Name:= '';
              C:= Component;
              While (C<>Nil) and (C.Name<>'') do
                begin
                If (Name<>'') Then
                  Name:='.'+Name;
                if C.Owner = LookupRoot then
                  begin
                  Name := C.Name+Name;
                  break;
                  end
                else if C = LookupRoot then
                  begin
                  Name := 'Owner' + Name;
                  break;
                  end;
                Name:=C.Name + Name;
                C:= C.Owner;
                end;
              if (C=nil) and (Component.Owner=nil) then
                if (Name<>'') then              //foreign root
                  Name:=Name+'.Owner';
              if Length(Name) > 0 then
                begin
                Driver.BeginProperty(FPropPath + PropInfo.Name);
                WriteIdent(Name);
                Driver.EndProperty;
                end;  // length Name>0
              end; //(ObjValue <> AncestorObj)
            end // ObjValue.InheritsFrom(TComponent)
          else
            begin
            SavedAncestor := Ancestor;
            SavedPropPath := FPropPath;
            try
              FPropPath := FPropPath + PropInfo.Name + '.';
              if HasAncestor then
                Ancestor := TPersistent(GetObjectProp(Ancestor, PropInfo));
              WriteProperties(TPersistent(ObjValue));
            finally
              Ancestor := SavedAncestor;
              FPropPath := SavedPropPath;
            end;
            if ObjValue.InheritsFrom(TCollection) then
              begin
              if (not HasAncestor) or (not CollectionsEqual(TCollection(ObjValue),
                TCollection(GetObjectProp(Ancestor, PropInfo)),root,rootancestor)) then
                begin
                Driver.BeginProperty(FPropPath + PropInfo.Name);
                SavedPropPath := FPropPath;
                try
                  SetLength(FPropPath, 0);
                  WriteCollection(TCollection(ObjValue));
                finally
                  FPropPath := SavedPropPath;
                  Driver.EndProperty;
                end;
                end;
              end // Tcollection
            end;
          end; // Inheritsfrom(TPersistent)
      end;
{    tkInt64, tkQWord:
      begin
        Int64Value := GetInt64Prop(Instance, PropInfo);
        if HasAncestor then
          DefInt64Value := GetInt64Prop(Ancestor, PropInfo)
        else
          DefInt64Value := 0;
        if Int64Value <> DefInt64Value then
        begin
          Driver.BeginProperty(FPropPath + PPropInfo(PropInfo)^.Name);
          WriteInteger(Int64Value);
          Driver.EndProperty;
        end;
      end;}
    tkBool:
      begin
        BoolValue := GetOrdProp(Instance, PropInfo)<>0;
        if HasAncestor then
          DefBoolValue := GetOrdProp(Ancestor, PropInfo)<>0
        else
          begin
          DefBoolValue := PropInfo.Default<>0;
          DefValue:=Longint(PropInfo.Default);
          end;
        // writeln(PropInfo.Name, ', HasAncestor=', ord(HasAncestor), ', Value=', Value, ', Default=', DefBoolValue);
        if (BoolValue<>DefBoolValue) or (DefValue=longint($80000000)) then
          begin
          Driver.BeginProperty(FPropPath + PropInfo.Name);
          WriteBoolean(BoolValue);
          Driver.EndProperty;
          end;
      end;
    tkInterface:
      begin
{        IntfValue := GetInterfaceProp(Instance, PropInfo);
      if Assigned(IntfValue) and Supports(IntfValue, IInterfaceComponentReference, CompRef) then
          begin
          Component := CompRef.GetComponent;
          if HasAncestor then
          begin
            AncestorObj := TObject(GetObjectProp(Ancestor, PropInfo));
            if (AncestorObj is TComponent) then
            begin
              //writeln('TWriter.WriteProperty AncestorObj=',TComponent(AncestorObj).Name,' OwnerFit=',TComponent(AncestorObj).Owner = FRootAncestor,' ',TComponent(ObjValue).Name,' OwnerFit=',TComponent(ObjValue).Owner = Root);
              if (AncestorObj<> Component) and
               (TComponent(AncestorObj).Owner = FRootAncestor) and
               (Component.Owner = Root) and
               (UpperCase(TComponent(AncestorObj).Name) = UpperCase(Component.Name)) then
              begin
                // different components, but with the same name
                // treat it like an override
                AncestorObj := Component;
              end;
            end;
          end else
            AncestorObj := nil;

          if not Assigned(Component) then
            begin
            if Component <> AncestorObj then
              begin
              Driver.BeginProperty(FPropPath + PropInfo.Name);
              Driver.WriteIdent('NIL');
              Driver.EndProperty;
              end
            end
          else if ((not (csSubComponent in Component.ComponentStyle))
                 or ((Component.Owner<>Instance) and (Component.Owner<>Nil))) then
            begin
            if (Component <> AncestorObj)
                and not (csTransient in Component.ComponentStyle) then
              begin
              Name:= '';
              C:= Component;
              While (C<>Nil) and (C.Name<>'') do
                begin
                If (Name<>'') Then
                  Name:='.'+Name;
                if C.Owner = LookupRoot then
                  begin
                  Name := C.Name+Name;
                  break;
                  end
                else if C = LookupRoot then
                  begin
                  Name := 'Owner' + Name;
                  break;
                  end;
                Name:=C.Name + Name;
                C:= C.Owner;
                end;
              if (C=nil) and (Component.Owner=nil) then
                if (Name<>'') then              //foreign root
                  Name:=Name+'.Owner';
              if Length(Name) > 0 then
                begin
                Driver.BeginProperty(FPropPath + PropInfo.Name);
                WriteIdent(Name);
                Driver.EndProperty;
                end;  // length Name>0
              end; //(Component <> AncestorObj)
            end;
          end; //Assigned(IntfValue) and Supports(IntfValue,..
               //else write NIL ?
}      end;
  end;
end;

procedure TWriter.WriteRootComponent(ARoot: TComponent);
begin
  WriteDescendent(ARoot, nil);
end;

procedure TWriter.WriteString(const Value: String);
begin
  Driver.WriteString(Value);
end;

procedure TWriter.WriteWideString(const Value: WideString);
begin
  Driver.WriteWideString(Value);
end;

procedure TWriter.WriteUnicodeString(const Value: UnicodeString);
begin
  Driver.WriteUnicodeString(Value);
end;

{ TAbstractObjectWriter }


{ ---------------------------------------------------------------------
  Global routines
  ---------------------------------------------------------------------}

var
  ClassList : TJSObject;
  InitHandlerList : TList;
  FindGlobalComponentList : TFPList;

Procedure RegisterClass(AClass : TPersistentClass);

begin
  ClassList[AClass.ClassName]:=AClass;
end;

Procedure RegisterClasses(AClasses : specialize TArray<TPersistentClass>);
var
  AClass : TPersistentClass;

begin
  for AClass in AClasses do
    RegisterClass(AClass);
end;

Function GetClass(AClassName : string) : TPersistentClass;

begin
  Result:=nil;
  if AClassName='' then exit;
  if not ClassList.hasOwnProperty(AClassName) then exit;
  Result:=TPersistentClass(ClassList[AClassName]);
end;


procedure RegisterFindGlobalComponentProc(AFindGlobalComponent: TFindGlobalComponent);
  begin
    if not(assigned(FindGlobalComponentList)) then
      FindGlobalComponentList:=TFPList.Create;
    if FindGlobalComponentList.IndexOf(CodePointer(AFindGlobalComponent))<0 then
      FindGlobalComponentList.Add(CodePointer(AFindGlobalComponent));
  end;


procedure UnregisterFindGlobalComponentProc(AFindGlobalComponent: TFindGlobalComponent);
  begin
    if assigned(FindGlobalComponentList) then
      FindGlobalComponentList.Remove(CodePointer(AFindGlobalComponent));
  end;


function FindGlobalComponent(const Name: string): TComponent;

var
      i : sizeint;
begin
  Result:=nil;
  if assigned(FindGlobalComponentList) then
    begin
      for i:=FindGlobalComponentList.Count-1 downto 0 do
      	begin
      	  FindGlobalComponent:=TFindGlobalComponent(FindGlobalComponentList[i])(name);
      	  if assigned(Result) then
      	    break;
      	end;
    end;
end;

Function FindNestedComponent(Root : TComponent; APath : String; CStyle : Boolean = True) : TComponent;

  Function GetNextName : String; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}

  Var
    P : Integer;
    CM : Boolean;

  begin
    P:=Pos('.',APath);
    CM:=False;
    If (P=0) then
      begin
      If CStyle then
        begin
        P:=Pos('->',APath);
        CM:=P<>0;
        end;
      If (P=0) Then
        P:=Length(APath)+1;
      end;
    Result:=Copy(APath,1,P-1);
    Delete(APath,1,P+Ord(CM));
  end;

Var
  C : TComponent;
  S : String;
begin
  If (APath='') then
    Result:=Nil
  else
    begin
    Result:=Root;
    While (APath<>'') And (Result<>Nil) do
      begin
      C:=Result;
      S:=Uppercase(GetNextName);
      Result:=C.FindComponent(S);
      If (Result=Nil) And (S='OWNER') then
        Result:=C;
      end;
    end;
end;

Type
  TInitHandler = Class(TObject)
    AHandler : TInitComponentHandler;
    AClass : TComponentClass;
  end;


procedure RegisterInitComponentHandler(ComponentClass: TComponentClass;   Handler: TInitComponentHandler);

Var
  I : Integer;
  H: TInitHandler;
begin
  If (InitHandlerList=Nil) then
    InitHandlerList:=TList.Create;
  H:=TInitHandler.Create;
  H.Aclass:=ComponentClass;
  H.AHandler:=Handler;
  try
    With InitHandlerList do
      begin
        I:=0;
        While (I<Count) and not H.AClass.InheritsFrom(TInitHandler(Items[I]).AClass) do
          Inc(I);
        { override? }
        if (I<Count) and (TInitHandler(Items[I]).AClass=H.AClass) then
          begin
            TInitHandler(Items[I]).AHandler:=Handler;
            H.Free;
          end
        else
          InitHandlerList.Insert(I,H);
      end;
   except
     H.Free;
     raise;
  end;
end;


procedure TObjectStreamConverter.OutStr(s: String);

Var
  I : integer;

begin
  For I:=1 to Length(S) do
    Output.WriteBufferData(s[i]);
end;

procedure TObjectStreamConverter.OutLn(s: String);
begin
  OutStr(s + LineEnding);
end;


procedure TObjectStreamConverter.Outchars(S: String);

var
  res, NewStr: String;
  i,len,w: Cardinal;
  InString, NewInString: Boolean;
  SObj : TJSString absolute s;

begin
 if S = '' then
   res:= ''''''
 else
   begin
   res := '';
   InString := False;
   len:= Length(S);
   i:=0;
   while i < Len do
     begin
     NewInString := InString;
     w := SObj.charCodeAt(i);
     if w = ord('''') then
       begin //quote char
       if not InString then
         NewInString := True;
       NewStr := '''''';
       end
     else if (w >= 32) and (w < 127) then
       begin //printable ascii or bytes
       if not InString then
         NewInString := True;
       NewStr := TJSString.FromCharCode(w);
       end
    else
       begin //ascii control chars, non ascii
       if InString then
         NewInString := False;
       NewStr := '#' + IntToStr(w);
       end;
    if NewInString <> InString then
      begin
      NewStr := '''' + NewStr;
      InString := NewInString;
      end;
    res := res + NewStr;
    Inc(i);
    end;
  if InString then
    res := res + '''';
  end;
 OutStr(res);
end;


procedure TObjectStreamConverter.OutString(s: String);
begin
  OutChars(S);
end;


(*
procedure TObjectStreamConverter.OutUtf8Str(s: String);
begin
  if Encoding=oteLFM then
    OutChars(Pointer(S),PChar(S)+Length(S),@CharToOrd)
  else
    OutChars(Pointer(S),PChar(S)+Length(S),@Utf8ToOrd);
end;
*)

function TObjectStreamConverter.ReadWord : word; {$ifdef CLASSESINLINE}inline;{$endif CLASSESINLINE}
begin
  Input.ReadBufferData(Result);
end;

function TObjectStreamConverter.ReadDWord : longword; {$ifdef CLASSESINLINE}inline;{$endif CLASSESINLINE}
begin
  Input.ReadBufferData(Result);
end;

function TObjectStreamConverter.ReadNativeInt : NativeInt; {$ifdef CLASSESINLINE}inline;{$endif CLASSESINLINE}
begin
  Input.ReadBufferData(Result);
end;

function TObjectStreamConverter.ReadInt(ValueType: TValueType): NativeInt;
begin
  case ValueType of
    vaInt8: Result := ShortInt(Input.ReadByte);
    vaInt16: Result := SmallInt(ReadWord);
    vaInt32: Result := LongInt(ReadDWord);
    vaNativeInt: Result := ReadNativeInt;
  end;
end;

function TObjectStreamConverter.ReadInt: NativeInt;
begin
  Result := ReadInt(TValueType(Input.ReadByte));
end;


function TObjectStreamConverter.ReadDouble : Double;

begin
  Input.ReadBufferData(Result);
end;

function TObjectStreamConverter.ReadStr: String;

var
  l,i: Byte;
  c : Char;

begin
  Input.ReadBufferData(L);
  SetLength(Result,L);
  For I:=1 to L do
    begin
    Input.ReadBufferData(C);
    Result[i]:=C;
    end;
end;

function TObjectStreamConverter.ReadString(StringType: TValueType): String;

var
  i: Integer;
  C : Char;

begin
  Result:='';
  if StringType<>vaString then
    Raise EFilerError.Create('Invalid string type passed to ReadString');
  i:=ReadDWord;
  SetLength(Result, i);
  for I:=1 to Length(Result) do
    begin
    Input.ReadbufferData(C);
    Result[i]:=C;
    end;
end;

procedure TObjectStreamConverter.ProcessBinary;

var
  ToDo, DoNow, i: LongInt;
  lbuf: TBytes;
  s: String;

begin
  ToDo := ReadDWord;
  SetLength(lBuf,32);
  OutLn('{');
  while ToDo > 0 do
    begin
    DoNow := ToDo;
    if DoNow > 32 then
      DoNow := 32;
    Dec(ToDo, DoNow);
    s := Indent + '  ';
    Input.ReadBuffer(lbuf, DoNow);
    for i := 0 to DoNow - 1 do
      s := s + IntToHex(lbuf[i], 2);
    OutLn(s);
  end;
  OutLn(indent + '}');
end;

procedure TObjectStreamConverter.ProcessValue(ValueType: TValueType; Indent: String);


var
  s: String;
{      len: LongInt; }
  IsFirst: Boolean;
{$ifndef FPUNONE}
  ext: Extended;
{$endif}

begin
  case ValueType of
    vaList: begin
        OutStr('(');
        IsFirst := True;
        while True do begin
          ValueType := TValueType(Input.ReadByte);
          if ValueType = vaNull then break;
          if IsFirst then begin
            OutLn('');
            IsFirst := False;
          end;
          OutStr(Indent + '  ');
          ProcessValue(ValueType, Indent + '  ');
        end;
        OutLn(Indent + ')');
      end;
    vaInt8: OutLn(IntToStr(ShortInt(Input.ReadByte)));
    vaInt16: OutLn( IntToStr(SmallInt(ReadWord)));
    vaInt32: OutLn(IntToStr(LongInt(ReadDWord)));
    vaNativeInt: OutLn(IntToStr(ReadNativeInt));
    vaDouble: begin
        ext:=ReadDouble;
        Str(ext,S);// Do not use localized strings.
        OutLn(S);
      end;
    vaString: begin
        if PlainStrings then
          OutStr( ''''+StringReplace(ReadString(vaString),'''','''''',[rfReplaceAll])+'''')
        else
          OutString(ReadString(vaString) {''''+StringReplace(ReadString(vaString),'''','''''',[rfReplaceAll])+''''});
        OutLn('');
      end;
    vaIdent: OutLn(ReadStr);
    vaFalse: OutLn('False');
    vaTrue: OutLn('True');
    vaBinary: ProcessBinary;
    vaSet: begin
        OutStr('[');
        IsFirst := True;
        while True do begin
          s := ReadStr;
          if Length(s) = 0 then break;
          if not IsFirst then OutStr(', ');
          IsFirst := False;
          OutStr(s);
        end;
        OutLn(']');
      end;
    vaNil:
      OutLn('nil');
    vaCollection: begin
        OutStr('<');
        while Input.ReadByte <> 0 do begin
          OutLn(Indent);
          Input.Seek(-1, soCurrent);
          OutStr(indent + '  item');
          ValueType := TValueType(Input.ReadByte);
          if ValueType <> vaList then
            OutStr('[' + IntToStr(ReadInt(ValueType)) + ']');
          OutLn('');
          ReadPropList(indent + '    ');
          OutStr(indent + '  end');
        end;
        OutLn('>');
      end;
    {vaSingle: begin OutLn('!!Single!!'); exit end;
    vaCurrency: begin OutLn('!!Currency!!'); exit end;
    vaDate: begin OutLn('!!Date!!'); exit end;}
    else
      Raise EReadError.CreateFmt(SErrInvalidPropertyType,[Ord(ValueType)]);
  end;
end;


procedure TObjectStreamConverter.ReadPropList(indent: String);


begin
  while Input.ReadByte <> 0 do begin
    Input.Seek(-1, soCurrent);
    OutStr(indent + ReadStr + ' = ');
    ProcessValue(TValueType(Input.ReadByte), Indent);
  end;
end;

procedure TObjectStreamConverter.ReadObject(indent: String);

var
  b: Byte;
  ObjClassName, ObjName: String;
  ChildPos: LongInt;

begin
  // Check for FilerFlags
  b := Input.ReadByte;
  if (b and $f0) = $f0 then begin
    if (b and 2) <> 0 then ChildPos := ReadInt;
  end else begin
    b := 0;
    Input.Seek(-1, soCurrent);
  end;

  ObjClassName := ReadStr;
  ObjName := ReadStr;

  OutStr(Indent);
  if (b and 1) <> 0 then OutStr('inherited')
  else
   if (b and 4) <> 0 then OutStr('inline')
   else OutStr('object');
  OutStr(' ');
  if ObjName <> '' then
    OutStr(ObjName + ': ');
  OutStr(ObjClassName);
  if (b and 2) <> 0 then OutStr('[' + IntToStr(ChildPos) + ']');
  OutLn('');

  ReadPropList(indent + '  ');

  while Input.ReadByte <> 0 do begin
    Input.Seek(-1, soCurrent);
    ReadObject(indent + '  ');
  end;
  OutLn(indent + 'end');
end;

procedure TObjectStreamConverter.ObjectBinaryToText(aInput, aOutput: TStream; aEncoding: TObjectTextEncoding);

begin
  FInput:=aInput;
  FOutput:=aOutput;
  FEncoding:=aEncoding;
  Execute;
end;

procedure TObjectStreamConverter.Execute;

var
  Signature: LongInt;

begin
  if FIndent = '' then FInDent:='  ';
  If Not Assigned(Input) then
    raise EReadError.Create('Missing input stream');
  If Not Assigned(Output) then
    raise EReadError.Create('Missing output stream');
  FInput.ReadBufferData(Signature);
  if Signature <> FilerSignatureInt then
    raise EReadError.Create(SInvalidImage);
  ReadObject('');
end;

procedure TObjectStreamConverter.ObjectBinaryToText(aInput, aOutput: TStream);
begin
  ObjectBinaryToText(aInput,aOutput,oteDFM);
end;

{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2007 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************}
{*                             TParser                                      *}
{****************************************************************************}

const
{$ifdef CPU16}
  { Avoid too big local stack use for
    MSDOS tiny memory model that uses less than 4096
    bytes for total stack by default. }
  ParseBufSize     = 512;
{$else not CPU16}
  ParseBufSize     = 4096;
{$endif not CPU16}

  TokNames : array[TParserToken] of string = (
    '?',
    'EOF',
    'Symbol',
    'String',
    'Integer',
    'Float',
    '-',
    '[',
    '(',
    '<',
    '{',
    ']',
    ')',
    '>',
    '}',
    ',',
    '.',
    '=',
    ':',
    '+'
  );

function TParser.GetTokenName(aTok: TParserToken): string;
begin
  Result:=TokNames[aTok]
end;

procedure TParser.LoadBuffer;

var
  CharsRead,i: integer;

begin
  CharsRead:=0;
  for I:=0 to ParseBufSize-1 do
  begin
    if FStream.ReadData(FBuf[i])<>2 then
      Break;
    Inc(CharsRead);
  end;
  Inc(FDeltaPos, CharsRead);
  FPos := 0;
  FBufLen := CharsRead;
  FEofReached:=CharsRead = 0;
end;

procedure TParser.CheckLoadBuffer; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
begin
  if fPos>=FBufLen then
    LoadBuffer;
end;

procedure TParser.ProcessChar; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
begin
  fLastTokenStr:=fLastTokenStr+fBuf[fPos];

  GotoToNextChar;
end;

function TParser.IsNumber: boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
begin
  Result:=fBuf[fPos] in ['0'..'9'];
end;

function TParser.IsHexNum: boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
begin
  Result:=fBuf[fPos] in ['0'..'9','A'..'F','a'..'f'];
end;

function TParser.IsAlpha: boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
begin
  Result:=fBuf[fPos] in ['_','A'..'Z','a'..'z'];
end;

function TParser.IsAlphaNum: boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
begin
  Result:=IsAlpha or IsNumber;
end;

function TParser.GetHexValue(c: char): byte; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
begin
  case c of
    '0'..'9' : Result:=ord(c)-$30;
    'A'..'F' : Result:=ord(c)-$37; //-$41+$0A
    'a'..'f' : Result:=ord(c)-$57; //-$61+$0A
  end;
end;

function TParser.GetAlphaNum: string;
begin
  if not IsAlpha then
    ErrorFmt(SParserExpected,[GetTokenName(toSymbol)]);
  Result:='';
  while IsAlphaNum do
  begin
    Result:=Result+fBuf[fPos];

    GotoToNextChar;
  end;
end;

procedure TParser.HandleNewLine;
begin
  if fBuf[fPos]=#13 then //CR
    GotoToNextChar;

  if fBuf[fPos]=#10 then //LF
    GotoToNextChar;

  inc(fSourceLine);
  fDeltaPos:=-(fPos-1);
end;

procedure TParser.SkipBOM;

begin
  // No BOM support
end;

procedure TParser.SkipSpaces;
begin
  while not FEofReached and (fBuf[fPos] in [' ',#9]) do GotoToNextChar;
end;

procedure TParser.SkipWhitespace;
begin
  while not FEofReached do
  begin
    case fBuf[fPos] of
      ' ',#9  : SkipSpaces;
      #10,#13 : HandleNewLine
      else break;
    end;
  end;
end;

procedure TParser.HandleEof;
begin
  fToken:=toEOF;
  fLastTokenStr:='';
end;

procedure TParser.HandleAlphaNum;
begin
  fLastTokenStr:=GetAlphaNum;
  fToken:=toSymbol;
end;

procedure TParser.HandleNumber;
type
  floatPunct = (fpDot,fpE);
  floatPuncts = set of floatPunct;
var
  allowed : floatPuncts;
begin
  fLastTokenStr:='';
  while IsNumber do
    ProcessChar;
  fToken:=toInteger;
  if (fBuf[fPos] in ['.','e','E']) then
  begin
    fToken:=toFloat;
    allowed:=[fpDot,fpE];
    while (fBuf[fPos] in ['.','e','E','0'..'9']) do
    begin
      case fBuf[fPos] of
        '.'     : if fpDot in allowed then Exclude(allowed,fpDot) else break;
        'E','e' : if fpE in allowed then
                  begin
                    allowed:=[];
                    ProcessChar;
                    if (fBuf[fPos] in ['+','-']) then ProcessChar;
                    if not (fBuf[fPos] in ['0'..'9']) then
                      ErrorFmt(SParserInvalidFloat,[fLastTokenStr+fBuf[fPos]]);
                  end
                  else break;
      end;
      ProcessChar;
    end;
  end;
  if (fBuf[fPos] in ['s','S','d','D','c','C']) then //single, date, currency
  begin
    fFloatType:=fBuf[fPos];

    GotoToNextChar;

    fToken:=toFloat;
  end
  else fFloatType:=#0;
end;

procedure TParser.HandleHexNumber;
var valid : boolean;
begin
  fLastTokenStr:='$';
  GotoToNextChar;
  valid:=false;
  while IsHexNum do
  begin
    valid:=true;
    ProcessChar;
  end;
  if not valid then
    ErrorFmt(SParserInvalidInteger,[fLastTokenStr]);
  fToken:=toInteger;
end;

function TParser.HandleQuotedString: string;
begin
  Result:='';
  GotoToNextChar;

  while true do
  begin
    case fBuf[fPos] of
      #0     : ErrorStr(SParserUnterminatedString);
      #13,#10 : ErrorStr(SParserUnterminatedString);
      ''''   : begin
                 GotoToNextChar;
                 if fBuf[fPos]<>'''' then exit;
               end;
    end;
    Result:=Result+fBuf[fPos];
    GotoToNextChar;
  end;
end;

Function TParser.HandleDecimalCharacter : Char;

var
   i : integer;

begin
  GotoToNextChar;
  // read a word number
  i:=0;
  while IsNumber and (i<high(word)) do
    begin
    i:=i*10+Ord(fBuf[fPos])-ord('0');
    GotoToNextChar;
    end;
  if i>high(word) then i:=0;
  Result:=Char(i);
end;

procedure TParser.HandleString;

var
  s: string;

begin
  fLastTokenStr:='';
  while true do
  begin
    case fBuf[fPos] of
      '''' :
        begin
          s:=HandleQuotedString;
          fLastTokenStr:=fLastTokenStr+s;
        end;
      '#'  :
        begin
          fLastTokenStr:=fLastTokenStr+HandleDecimalCharacter;
        end;
      else break;
    end;
  end;
  fToken:=Classes.toString
end;

procedure TParser.HandleMinus;
begin
  GotoToNextChar;
  if IsNumber then
  begin
    HandleNumber;
    fLastTokenStr:='-'+fLastTokenStr;
  end
  else
  begin
    fToken:=toMinus;
    fLastTokenStr:='-';
  end;
end;

procedure TParser.HandleUnknown;
begin
  fToken:=toUnknown;
  fLastTokenStr:=fBuf[fPos];
  GotoToNextChar;
end;

constructor TParser.Create(Stream: TStream);
begin
  fStream:=Stream;
  SetLength(fBuf,ParseBufSize);
  fBufLen:=0;
  fPos:=0;
  fDeltaPos:=1;
  fSourceLine:=1;
  fEofReached:=false;
  fLastTokenStr:='';
  fFloatType:=#0;
  fToken:=toEOF;
  LoadBuffer;
  SkipBom;
  NextToken;
end;

procedure TParser.GotoToNextChar;
begin
  Inc(FPos);

  CheckLoadBuffer;
end;

destructor TParser.Destroy;

Var
  aCount : Integer;

begin
  aCount:=Length(fLastTokenStr)*2;
  fStream.Position:=SourcePos-aCount;
end;

procedure TParser.CheckToken(T: tParserToken);
begin
  if fToken<>T then
    ErrorFmt(SParserWrongTokenType,[GetTokenName(T),GetTokenName(fToken)]);
end;

procedure TParser.CheckTokenSymbol(const S: string);
begin
  CheckToken(toSymbol);
  if CompareText(fLastTokenStr,S)<>0 then
    ErrorFmt(SParserWrongTokenSymbol,[s,fLastTokenStr]);
end;

procedure TParser.Error(const Ident: string);
begin
  ErrorStr(Ident);
end;

procedure TParser.ErrorFmt(const Ident: string; const Args: array of const);
begin
  ErrorStr(Format(Ident,Args));
end;

procedure TParser.ErrorStr(const Message: string);
begin
  raise EParserError.CreateFmt(Message+SParserLocInfo,[SourceLine,fPos+fDeltaPos,SourcePos]);
end;

procedure TParser.HexToBinary(Stream: TStream);

var
  outbuf : TBytes;
  b : byte;
  i : integer;

begin
  SetLength(OutBuf,ParseBufSize);
  i:=0;
  SkipWhitespace;
  while IsHexNum do
  begin
    b:=(GetHexValue(fBuf[fPos]) shl 4);
    GotoToNextChar;
    if not IsHexNum then
      Error(SParserUnterminatedBinValue);
    b:=b or GetHexValue(fBuf[fPos]);
    GotoToNextChar;
    outbuf[i]:=b;
    inc(i);
    if i>=ParseBufSize then
    begin
      Stream.WriteBuffer(outbuf,i);
      i:=0;
    end;
    SkipWhitespace;
  end;
  if i>0 then
    Stream.WriteBuffer(outbuf,i);
  NextToken;
end;

function TParser.NextToken: TParserToken;

  Procedure SetToken(aToken : TParserToken);
  begin
    FToken:=aToken;
    GotoToNextChar;
  end;

begin
  SkipWhiteSpace;
  if fEofReached then
    HandleEof
  else
    case fBuf[fPos] of
      '_','A'..'Z','a'..'z' : HandleAlphaNum;
      '$'                   : HandleHexNumber;
      '-'                   : HandleMinus;
      '0'..'9'              : HandleNumber;
      '''','#'              : HandleString;
      '[' : SetToken(toSetStart);
      '(' : SetToken(toListStart);
      '<' : SetToken(toCollectionStart);
      '{' : SetToken(toBinaryStart);
      ']' : SetToken(toSetEnd);
      ')' : SetToken(toListEnd);
      '>' : SetToken(toCollectionEnd);
      '}' : SetToken(toBinaryEnd);
      ',' : SetToken(toComma);
      '.' : SetToken(toDot);
      '=' : SetToken(toEqual);
      ':' : SetToken(toColon);
      '+' : SetToken(toPlus);
      else
        HandleUnknown;
    end;
  Result:=fToken;
end;

function TParser.SourcePos: Longint;
begin
  Result:=fStream.Position-fBufLen+fPos;
end;

function TParser.TokenComponentIdent: string;
begin
  if fToken<>toSymbol then
    ErrorFmt(SParserExpected,[GetTokenName(toSymbol)]);
  CheckLoadBuffer;
  while fBuf[fPos]='.' do
  begin
    ProcessChar;
    fLastTokenStr:=fLastTokenStr+GetAlphaNum;
  end;
  Result:=fLastTokenStr;
end;

Function TParser.TokenFloat: double;

var
  errcode : integer;

begin
  Val(fLastTokenStr,Result,errcode);
  if errcode<>0 then
    ErrorFmt(SParserInvalidFloat,[fLastTokenStr]);
end;

Function TParser.TokenInt: NativeInt;
begin
  if not TryStrToInt64(fLastTokenStr,Result) then
    Result:=StrToQWord(fLastTokenStr); //second chance for malformed files
end;

function TParser.TokenString: string;
begin
  case fToken of
    toFloat : if fFloatType<>#0 then
                Result:=fLastTokenStr+fFloatType
              else Result:=fLastTokenStr;
    else
      Result:=fLastTokenStr;
  end;
end;


function TParser.TokenSymbolIs(const S: string): Boolean;
begin
  Result:=(fToken=toSymbol) and (CompareText(fLastTokenStr,S)=0);
end;


procedure TObjectTextConverter.WriteWord(w : word); {$ifdef CLASSESINLINE}inline;{$endif CLASSESINLINE}
begin
  Output.WriteBufferData(w);
end;

procedure TObjectTextConverter.WriteDWord(lw : longword); {$ifdef CLASSESINLINE}inline;{$endif CLASSESINLINE}
begin
  Output.WriteBufferData(lw);
end;

procedure TObjectTextConverter.WriteQWord(q : NativeInt); {$ifdef CLASSESINLINE}inline;{$endif CLASSESINLINE}
begin
  Output.WriteBufferData(q);
end;

procedure TObjectTextConverter.WriteDouble(e : double);
begin
  Output.WriteBufferData(e);
end;

procedure TObjectTextConverter.WriteString(s: String);

var
  i,size : byte;

begin
  if length(s)>255 then
    size:=255
  else
    size:=length(s);
  Output.WriteByte(size);
  For I:=1 to Length(S) do
     Output.WriteBufferData(s[i]);
end;

procedure TObjectTextConverter.WriteWString(Const s: WideString);

var
  i : Integer;
begin
  WriteDWord(Length(s));
  For I:=1 to Length(S) do
    Output.WriteBufferData(s[i]);
end;

procedure TObjectTextConverter.WriteInteger(value: NativeInt);

begin
  if (value >= -128) and (value <= 127) then begin
    Output.WriteByte(Ord(vaInt8));
    Output.WriteByte(byte(value));
  end else if (value >= -32768) and (value <= 32767) then begin
    Output.WriteByte(Ord(vaInt16));
    WriteWord(word(value));
  end else if (value >= -2147483648) and (value <= 2147483647) then begin
    Output.WriteByte(Ord(vaInt32));
    WriteDWord(longword(value));
  end else begin
    Output.WriteByte(ord(vaInt64));
    WriteQWord(NativeUInt(value));
  end;
end;

procedure TObjectTextConverter.ProcessWideString(const left : string);

var
   ws : string;
begin
  ws:=left+parser.TokenString;
  while parser.NextToken = toPlus do
    begin
    parser.NextToken;   // Get next string fragment
    if not (parser.Token=Classes.toString) then
      parser.CheckToken(Classes.toString);
    ws:=ws+parser.TokenString;
    end;
  Output.WriteByte(Ord(vaWstring));
  WriteWString(ws);
end;


procedure TObjectTextConverter.ProcessValue;
var
  flt: double;
  stream: TBytesStream;
begin
  case parser.Token of
    toInteger:
      begin
        WriteInteger(parser.TokenInt);
        parser.NextToken;
      end;
    toFloat:
      begin
        Output.WriteByte(Ord(vaExtended));
        flt := Parser.TokenFloat;
        WriteDouble(flt);
        parser.NextToken;
      end;
    classes.toString:
      ProcessWideString('');
    toSymbol:
      begin
        if CompareText(parser.TokenString, 'True') = 0 then
          Output.WriteByte(Ord(vaTrue))
        else if CompareText(parser.TokenString, 'False') = 0 then
          Output.WriteByte(Ord(vaFalse))
        else if CompareText(parser.TokenString, 'nil') = 0 then
          Output.WriteByte(Ord(vaNil))
        else
        begin
          Output.WriteByte(Ord(vaIdent));
          WriteString(parser.TokenComponentIdent);
        end;
        Parser.NextToken;
      end;
    // Set
    toSetStart:
      begin
        parser.NextToken;
        Output.WriteByte(Ord(vaSet));
        if parser.Token <> toSetEnd then
          while True do
          begin
            parser.CheckToken(toSymbol);
            WriteString(parser.TokenString);
            parser.NextToken;
            if parser.Token = toSetEnd then
              break;
            parser.CheckToken(toComma);
            parser.NextToken;
          end;
        Output.WriteByte(0);
        parser.NextToken;
      end;
    // List
    toListStart:
      begin
        parser.NextToken;
        Output.WriteByte(Ord(vaList));
        while parser.Token <> toListEnd do
          ProcessValue;
        Output.WriteByte(0);
        parser.NextToken;
      end;
    // Collection
    toCollectionStart:
      begin
        parser.NextToken;
        Output.WriteByte(Ord(vaCollection));
        while parser.Token <> toCollectionEnd do
        begin
          parser.CheckTokenSymbol('item');
          parser.NextToken;
          // ConvertOrder
          Output.WriteByte(Ord(vaList));
          while not parser.TokenSymbolIs('end') do
            ProcessProperty;
          parser.NextToken;   // Skip 'end'
          Output.WriteByte(0);
        end;
        Output.WriteByte(0);
        parser.NextToken;
      end;
    // Binary data
    toBinaryStart:
      begin
        Output.WriteByte(Ord(vaBinary));
        stream := TBytesStream.Create;
        try
          parser.HexToBinary(stream);
          WriteDWord(stream.Size);
          Output.WriteBuffer(Stream.Bytes,Stream.Size);
        finally
          stream.Free;
        end;
        parser.NextToken;
      end;
    else
      parser.Error(SParserInvalidProperty);
  end;
end;

procedure TObjectTextConverter.ProcessProperty;
var
  name: String;
begin
  // Get name of property
  parser.CheckToken(toSymbol);
  name := parser.TokenString;
  while True do begin
    parser.NextToken;
    if parser.Token <> toDot then break;
    parser.NextToken;
    parser.CheckToken(toSymbol);
    name := name + '.' + parser.TokenString;
  end;
  WriteString(name);
  parser.CheckToken(toEqual);
  parser.NextToken;
  ProcessValue;
end;

procedure TObjectTextConverter.ProcessObject;
var
  Flags: Byte;
  ObjectName, ObjectType: String;
  ChildPos: Integer;
begin
  if parser.TokenSymbolIs('OBJECT') then
    Flags :=0  { IsInherited := False }
  else begin
    if parser.TokenSymbolIs('INHERITED') then
      Flags := 1 { IsInherited := True; }
    else begin
      parser.CheckTokenSymbol('INLINE');
      Flags := 4;
    end;
  end;
  parser.NextToken;
  parser.CheckToken(toSymbol);
  ObjectName := '';
  ObjectType := parser.TokenString;
  parser.NextToken;
  if parser.Token = toColon then begin
    parser.NextToken;
    parser.CheckToken(toSymbol);
    ObjectName := ObjectType;
    ObjectType := parser.TokenString;
    parser.NextToken;
    if parser.Token = toSetStart then begin
      parser.NextToken;
      ChildPos := parser.TokenInt;
      parser.NextToken;
      parser.CheckToken(toSetEnd);
      parser.NextToken;
      Flags := Flags or 2;
    end;
  end;
  if Flags <> 0 then begin
    Output.WriteByte($f0 or Flags);
    if (Flags and 2) <> 0 then
      WriteInteger(ChildPos);
  end;
  WriteString(ObjectType);
  WriteString(ObjectName);

  // Convert property list
  while not (parser.TokenSymbolIs('END') or
    parser.TokenSymbolIs('OBJECT') or
    parser.TokenSymbolIs('INHERITED') or
    parser.TokenSymbolIs('INLINE')) do
    ProcessProperty;
  Output.WriteByte(0);        // Terminate property list

  // Convert child objects
  while not parser.TokenSymbolIs('END') do ProcessObject;
  parser.NextToken;           // Skip end token
  Output.WriteByte(0);        // Terminate property list
end;

procedure TObjectTextConverter.ObjectTextToBinary(aInput, aOutput: TStream);

begin
  FinPut:=aInput;
  FOutput:=aOutput;
  Execute;
end;

procedure TObjectTextConverter.Execute;
begin
  If Not Assigned(Input) then
    raise EReadError.Create('Missing input stream');
  If Not Assigned(Output) then
    raise EReadError.Create('Missing output stream');
  FParser := TParser.Create(Input);
  try
    Output.WriteBufferData(FilerSignatureInt);
    ProcessObject;
  finally
    FParser.Free;
  end;
end;

procedure ObjectTextToBinary(aInput, aOutput: TStream);

var
  Conv : TObjectTextConverter;

begin
  Conv:=TObjectTextConverter.Create;
  try
    Conv.ObjectTextToBinary(aInput, aOutput);
  finally
    Conv.free;
  end;
end;


initialization
  ClassList:=TJSObject.New;
end.

