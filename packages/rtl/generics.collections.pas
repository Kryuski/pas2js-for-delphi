unit Generics.Collections;

{$Mode Delphi}
{$COperators On}

interface

uses
  Classes, SysUtils, rtlconsts, Types,
  {$IFDEF Pas2js}JS,{$ENDIF}
  Generics.Strings, Generics.Defaults;

type
  TCollectionNotification = (cnAdded, cnRemoved, cnExtracted);
  TCollectionNotifyEvent<T> = procedure(ASender: TObject; const AItem: T;
    AAction: TCollectionNotification) of object;

  { TBinarySearchResult }

  TBinarySearchResult = record
    FoundIndex, CandidateIndex: SizeInt;
    CompareResult: SizeInt;
  end;

  { TCustomArrayHelper }

  TCustomArrayHelper<T> = class abstract
  protected
    class procedure QuickSort(var AValues: array of T; ALeft, ARight: SizeInt;
      const AComparer: IComparer<T>); virtual; abstract;
  public
    //class procedure Sort(var AValues: array of T); overload;
    class procedure Sort(var AValues: array of T;
      const AComparer: IComparer<T>); overload;
    class procedure Sort(var AValues: array of T;
      const AComparer: IComparer<T>; AIndex, ACount: SizeInt); overload;

    class function BinarySearch(const AValues: array of T; const AItem: T;
      out ASearchResult: TBinarySearchResult; const AComparer: IComparer<T>;
      AIndex, ACount: SizeInt): Boolean; virtual; abstract; overload;
    class function BinarySearch(const AValues: array of T; const AItem: T;
      out AFoundIndex: SizeInt; const AComparer: IComparer<T>;
      AIndex, ACount: SizeInt): Boolean; virtual; abstract; overload;
    class function BinarySearch(const AValues: array of T; const AItem: T;
      out AFoundIndex: SizeInt; const AComparer: IComparer<T>): Boolean; overload;
    class function BinarySearch(const AValues: array of T; const AItem: T;
      out ASearchResult: TBinarySearchResult; const AComparer: IComparer<T>): Boolean; overload;
    // No support for automatically creating a comparer.
    //  class function BinarySearch(const AValues: array of T; const AItem: T;
    //  out AFoundIndex: SizeInt): Boolean; overload;
    //  class function BinarySearch(const AValues: array of T; const AItem: T;
    //  out ASearchResult: TBinarySearchResult): Boolean; overload;
  end;

  { TArrayHelper }

  TArrayHelper<T> = class(TCustomArrayHelper<T>)
  protected
    class procedure QuickSort(var AValues: array of T; ALeft, ARight: SizeInt;
      const AComparer: IComparer<T>); override;
  public
    class function BinarySearch(const AValues: array of T; const AItem: T;
      out ASearchResult: TBinarySearchResult; const AComparer: IComparer<T>;
      AIndex, ACount: SizeInt): Boolean; override; overload;
    class function BinarySearch(const AValues: array of T; const AItem: T;
      out AFoundIndex: SizeInt; const AComparer: IComparer<T>;
      AIndex, ACount: SizeInt): Boolean; override; overload;
  end;

  { TEnumerator }

  TEnumerator<T> = class abstract
  protected
    function DoGetCurrent: T; virtual; abstract;
    function DoMoveNext: boolean; virtual; abstract;
  public
    property Current: T read DoGetCurrent;
    function MoveNext: boolean;
  end;

  { TEnumerable }

  TEnumerable<T> = class abstract
  protected
    type
      TMyEnumerator = TEnumerator<T>;
    function DoGetEnumerator: TMyEnumerator; virtual; abstract;
  public
    type
      TMyArray = TArray<T>;
    function GetEnumerator: TMyEnumerator; inline;
    function ToArray: TMyArray; virtual;
  end;

  { TCustomList }

  TCustomList<T> = class abstract(TEnumerable<T>)
  private
    FOnNotify: TCollectionNotifyEvent<T>;
    function GetCapacity: SizeInt; inline;
  protected
    type TMyArrayHelper = TArrayHelper<T>;
  protected
    FLength: SizeInt;
    FItems: array of T;
    function PrepareAddingItem: SizeInt; virtual;
    function PrepareAddingRange(ACount: SizeInt): SizeInt; virtual;
    procedure Notify(const AValue: T; ACollectionNotification: TCollectionNotification); virtual;
    function DoRemove(AIndex: SizeInt; ACollectionNotification: TCollectionNotification): T; virtual;
    procedure SetCapacity(AValue: SizeInt); virtual; abstract;
    function GetCount: SizeInt; virtual;
  public
    property Count: SizeInt read GetCount;
    property Capacity: SizeInt read GetCapacity write SetCapacity;
    property OnNotify: TCollectionNotifyEvent<T> read FOnNotify write FOnNotify;
    procedure TrimExcess; virtual; abstract;
  end;

  { TCustomListEnumerator }

  TCustomListEnumerator<T> = class abstract(TEnumerator<T>)
  private
    FList: TCustomList<T>;
    FIndex: SizeInt;
  protected
    function DoMoveNext: boolean; override;
    function DoGetCurrent: T; override;
    function GetCurrent: T; virtual;
  public
    constructor Create(AList: TCustomList<T>);
  end;

  { TCustomInvertedListEnumerator }

  TCustomInvertedListEnumerator<T> = class abstract(TEnumerator<T>)
  private
    FList: TCustomList<T>;
    FIndex: SizeInt;
  protected
    function DoMoveNext: boolean; override;
    function DoGetCurrent: T; override;
    function GetCurrent: T; virtual;
  public
    constructor Create(AList: TCustomList<T>);
  end;

  { TList }
{$SCOPEDENUMS ON}
  TDirection = (FromBeginning,fromEnd);
{$SCOPEDENUMS OFF}

  TList<T> = class(TCustomList<T>)
  private
    FComparer: IComparer<T>;
    function SameValue(const Left, Right: T): Boolean;
  protected
    procedure SetCapacity(AValue: SizeInt); override;
    procedure SetCount(AValue: SizeInt);
    procedure InitializeList; virtual;
    procedure InternalInsert(AIndex: SizeInt; const AValue: T);
    function DoGetEnumerator: TEnumerator<T>; override;
  private
    function GetItem(AIndex: SizeInt): T;
    procedure SetItem(AIndex: SizeInt; const AValue: T);
  public
    type
      TEnumerator = class(TCustomListEnumerator<T>);
      TMyType = TList<T>;
    function GetEnumerator: TEnumerator; reintroduce;
  public
    constructor Create; overload;
    constructor Create(const AComparer: IComparer<T>); overload;
    constructor Create(ACollection: TEnumerable<T>); overload;

    destructor Destroy; override;

    function Add(const AValue: T): SizeInt; virtual;
    procedure AddRange(const AValues: array of T); virtual; overload;
    procedure AddRange(const AEnumerable: IEnumerable<T>); overload;
    procedure AddRange(AEnumerable: TEnumerable<T>); overload;

    procedure Insert(AIndex: SizeInt; const AValue: T); virtual;
    procedure InsertRange(AIndex: SizeInt; const AValues: array of T); virtual; overload;
    procedure InsertRange(AIndex: SizeInt; const AEnumerable: IEnumerable<T>); overload;
    procedure InsertRange(AIndex: SizeInt; const AEnumerable: TEnumerable<T>); overload;

    function Remove(const AValue: T): SizeInt;
    function RemoveItem(const AValue: T; Direction : TDirection): SizeInt;
    procedure Delete(AIndex: SizeInt); inline;
    procedure DeleteRange(AIndex, ACount: SizeInt);
    function ExtractIndex(const AIndex: SizeInt): T; overload;
    function Extract(const AValue: T): T; overload;

    procedure Exchange(AIndex1, AIndex2: SizeInt); virtual;
    procedure Move(AIndex, ANewIndex: SizeInt); virtual;

    function First: T; inline;
    function Last: T; inline;

    procedure Clear;

    function Contains(const AValue: T): Boolean; inline;
    function IndexOf(const AValue: T): SizeInt; virtual;
    function LastIndexOf(const AValue: T): SizeInt; virtual;

    procedure Reverse;

    procedure TrimExcess; override;

    procedure Sort; overload;
    procedure Sort(const AComparer: IComparer<T>); overload;
    function BinarySearch(const AItem: T; out AIndex: SizeInt): Boolean; overload;
    function BinarySearch(const AItem: T; out AIndex: SizeInt; const AComparer: IComparer<T>): Boolean; overload;

    property Count: SizeInt read FLength write SetCount;
    property Items[Index: SizeInt]: T read GetItem write SetItem; default;
  end;


  TObjectList<T: class> = class(TList<T>)
  private
    FObjectsOwner: Boolean;
  protected
    procedure Notify(const aValue: T; Action: TCollectionNotification); override;
  public
    constructor Create(aOwnsObjects: Boolean = True); overload;
    constructor Create(const AComparer: IComparer<T>; aOwnsObjects: Boolean = True); overload;
    constructor Create(const aCollection: TEnumerable<T>; aOwnsObjects: Boolean = True); overload;
    property OwnsObjects: Boolean read FObjectsOwner write FObjectsOwner;
  end;

  { TThreadList }
  // This is provided for delphi/FPC compatibility
  // No locking is done, since Javascript is single-threaded. We do keep a lock count for debugging purposes.

  TThreadList<T> = class
  private
    FList: TList<T>;
    FLock: Integer;
    FDuplicates: TDuplicates;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Item: T);
    procedure Clear;
    function LockList: TList<T>;
    procedure Remove(const Item: T); inline;
    procedure RemoveItem(const Item: T; Direction: TDirection);
    procedure UnlockList; inline;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

  { TQueue }

  TQueue<T> = class(TCustomList<T>)
  private
    FMaxGapLength: Integer;
    FLow: SizeInt;
  protected
    function DoGetEnumerator: TEnumerator<T>; override;
  public
    type
      TMyType = TQueue<T>;
      { TEnumerator }
      TEnumerator = class(TCustomListEnumerator<T>)
      public
        constructor Create(AQueue: TMyType);
      end;
    function GetEnumerator: TEnumerator; reintroduce;
  protected
    Procedure Rebase; virtual;
    procedure SetCapacity(AValue: SizeInt); override;
    function DoRemove(AIndex: SizeInt; ACollectionNotification: TCollectionNotification): T; override;
    function GetCount: SizeInt; override;
  public
    Constructor Create; overload;
    constructor Create(ACollection: TEnumerable<T>); overload;
    destructor Destroy; override;
    procedure Enqueue(const AValue: T);
    function Dequeue: T;
    function Extract: T;
    function Peek: T;
    procedure Clear;
    procedure TrimExcess; override;
    // Maximum gap (=amount of empty slots in array before first element)
    // before doing a rebase of the list. Defaults to 10.
    Property MaxGapLength : Integer Read FMaxGapLength Write FMaxGapLength;
  end;

  { TObjectQueue }

  TObjectQueue<T: class> = class(TQueue<T>)
  private
    FOwnsObjects: Boolean;
  protected
    procedure Notify(const Value: T; Action: TCollectionNotification); override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(const Collection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    procedure Dequeue; reintroduce; // Can't use the result, it might have been freed;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  { TStack }

  TStack<T> = class(TCustomList<T>)
  private
  protected
    function DoRemove(AIndex: SizeInt; ACollectionNotification: TCollectionNotification): T; override;
    procedure SetCapacity(AValue: SizeInt); override;
    function DoGetEnumerator: TEnumerator<T>; override;
  public
    type
      TMyType = TStack<T>;

      { TEnumerator }

      TEnumerator = class(TCustomListEnumerator<T>)
      public
        constructor Create(AStack: TMyType);
      end;
    function GetEnumerator: TEnumerator; reintroduce;
  Public
    destructor Destroy; override;
    procedure Clear;
    procedure Push(const aValue: T);
    function Pop: T;
    function Peek: T;
    function Extract: T;
    procedure TrimExcess; override;
    property Count: SizeInt read GetCount;
  end;

  { TObjectStack }

  TObjectStack<T: class> = class(TStack<T>)
  private
    FOwnsObjects: Boolean;
  protected
    procedure Notify(const aValue: T; Action: TCollectionNotification); override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(const Collection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    procedure Pop; reintroduce; // Can't use the result, it might have been freed;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;


  { TPair }

  TPair<TKey,TValue> = record
    Key: TKey;
    Value: TValue;
    constructor Create(const AKey: TKey; const AValue: TValue);
  end;

  // Hash table using linear probing

  { TDictionary }
  EDictionary = Class(Exception);

  TDictionary<TKey,TValue> = class(TEnumerable<TPair<TKey,TValue>>)
  private
    FMap: TJSMap;
    FComparer: IComparer<TKey>;
    function GetEffectiveKey(Key : TKey) : TKey;
    function GetItem(const Key: TKey): TValue;
    procedure SetItem(const Key: TKey; const Value: TValue);
    procedure DoAdd(const Key: TKey; const Value: TValue);
    function DoRemove(const Key: TKey; Notification: TCollectionNotification): TValue;
    Function GetCount : Integer;
  protected
    Function CanClearMap : Boolean; virtual;
    function DoGetEnumerator: TEnumerator<TPair<TKey,TValue>>; override;
    procedure PairNotify(const Key: TKey; Value : TValue; Action: TCollectionNotification); virtual;
    procedure KeyNotify(const Key: TKey; Action: TCollectionNotification); virtual;
    procedure ValueNotify(const Value: TValue; Action: TCollectionNotification); virtual;
  public
    Type
      TMyType = TDictionary<TKey,TValue>;
      TMyPair = TPair<TKey,TValue>;

    constructor Create(ACapacity: Integer=0); overload;
    constructor Create(const Collection: TEnumerable<TMyPair>); overload;
    constructor Create(const AComparer: IComparer<TKey>); overload;
    destructor Destroy; override;

    procedure Add(const Key: TKey; const Value: TValue);
    procedure Remove(const Key: TKey);
    function ExtractPair(const Key: TKey): TMyPair;
    procedure Clear;
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    procedure AddOrSetValue(const Key: TKey; const Value: TValue);
    function ContainsKey(const Key: TKey): Boolean;
    function ContainsValue(const Value: TValue): Boolean;
    function ToArray: TArray<TMyPair>; override;

    property Items[const Key: TKey]: TValue read GetItem write SetItem; default;
    property Count: Integer read GetCount;

    type
      { TPairEnumerator }

      TPairEnumerator = class(TEnumerator<TMyPair>)
      private
        FIter: TJSIterator;
        FVal : TJSIteratorValue;
        function GetCurrent: TMyPair;
      protected
        function DoGetCurrent: TMyPair; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const ADictionary: TMyType);
        function MoveNext: Boolean; reintroduce;
        property Current: TMyPair read GetCurrent;
      end;

      { TKeyEnumerator }

      TKeyEnumerator = class(TEnumerator<TKey>)
      private
        FIter: TJSIterator;
        FVal : TJSIteratorValue;
        function GetCurrent: TKey;
      protected
        function DoGetCurrent: TKey; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const AIter: TJSIterator); overload;
        constructor Create(const ADictionary: TMyType); overload;
        function MoveNext: Boolean; reintroduce;
        property Current: TKey read GetCurrent;
      end;

      { TValueEnumerator }

      TValueEnumerator = class(TEnumerator<TValue>)
      private
        FIter: TJSIterator;
        FVal : TJSIteratorValue;
        function GetCurrent: TValue;
      protected
        function DoGetCurrent: TValue; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const AIter: TJSIterator); overload;
        constructor Create(const ADictionary: TMyType); overload;
        function MoveNext: Boolean; reintroduce;
        property Current: TValue read GetCurrent;
      end;

      { TValueCollection }

      TValueCollection = class(TEnumerable<TValue>)
      private
        FMap: TJSMap;
        function GetCount: Integer;
      protected
        function DoGetEnumerator: TEnumerator<TValue>; override;
      public
        constructor Create(const ADictionary: TMyType);
        function GetEnumerator: TValueEnumerator; reintroduce;
        function ToArray: TArray<TValue>; override;
        property Count: Integer read GetCount;
      end;

      { TKeyCollection }

      TKeyCollection = class(TEnumerable<TKey>)
      private
        FMap: TJSMap;
        function GetCount: Integer;
      protected
        function DoGetEnumerator: TEnumerator<TKey>; override;
      public
        constructor Create(const ADictionary: TMyType);
        function GetEnumerator: TKeyEnumerator; reintroduce;
        function ToArray: TArray<TKey>; override;
        property Count: Integer read GetCount;
      end;

  private
    FOnKeyNotify: TCollectionNotifyEvent<TKey>;
    FOnValueNotify: TCollectionNotifyEvent<TValue>;
    FKeyCollection: TKeyCollection;
    FValueCollection: TValueCollection;
    function GetKeys: TKeyCollection;
    function GetValues: TValueCollection;
  public
    function GetEnumerator: TPairEnumerator; reintroduce;
    property Keys: TKeyCollection read GetKeys;
    property Values: TValueCollection read GetValues;
    property OnKeyNotify: TCollectionNotifyEvent<TKey> read FOnKeyNotify write FOnKeyNotify;
    property OnValueNotify: TCollectionNotifyEvent<TValue> read FOnValueNotify write FOnValueNotify;
  end;

    TDictionaryOwnership = (doOwnsKeys, doOwnsValues);
    TDictionaryOwnerships = set of TDictionaryOwnership;

    { TObjectDictionary }

    TObjectDictionary<TKey,TValue> = class(TDictionary<TKey,TValue>)
    private
      FOwnerships: TDictionaryOwnerships;
    protected
      Function CanClearMap : Boolean; override;
      procedure KeyNotify(const Key: TKey; Action: TCollectionNotification); override;
      procedure ValueNotify(const Value: TValue; Action: TCollectionNotification); override;
    public
      constructor Create(aOwnerships: TDictionaryOwnerships; ACapacity: Integer); overload;
      constructor Create(aOwnerships: TDictionaryOwnerships); overload;
      Property OwnerShips : TDictionaryOwnerships Read FOwnerships Write FOwnerShips;
    end;

implementation

{ TCustomArrayHelper }

class procedure TCustomArrayHelper<T>.Sort(var AValues: array of T;
  const AComparer: IComparer<T>);
begin
  QuickSort(AValues, 0, Length(AValues), AComparer);
end;

class procedure TCustomArrayHelper<T>.Sort(var AValues: array of T;
  const AComparer: IComparer<T>; AIndex, ACount: SizeInt);
begin
  if ACount <= 1 then
    Exit;
  QuickSort(AValues, AIndex, Pred(AIndex + ACount), AComparer);
end;

class function TCustomArrayHelper<T>.BinarySearch(const AValues: array of T;
  const AItem: T; out AFoundIndex: SizeInt; const AComparer: IComparer<T>
  ): Boolean;
begin
  Result := BinarySearch(AValues, AItem, AFoundIndex, AComparer,
                         0, Length(AValues));
end;

class function TCustomArrayHelper<T>.BinarySearch(const AValues: array of T;
  const AItem: T; out ASearchResult: TBinarySearchResult;
  const AComparer: IComparer<T>): Boolean;
begin
  Result := BinarySearch(AValues, AItem, ASearchResult, AComparer,
                         0, Length(AValues));
end;

{ TArrayHelper }

class procedure TArrayHelper<T>.QuickSort(var AValues: array of T; ALeft,
  ARight: SizeInt; const AComparer: IComparer<T>);
var
  I, J: SizeInt;
  P, Q: T;
begin
  if ((ARight - ALeft) <= 0) or (Length(AValues) = 0) then
    Exit;
  repeat
    I := ALeft;
    J := ARight;
    P := AValues[ALeft + (ARight - ALeft) shr 1];
    repeat
      while AComparer.Compare(AValues[I], P) < 0 do
        Inc(I);
      while AComparer.Compare(AValues[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          Q := AValues[I];
          AValues[I] := AValues[J];
          AValues[J] := Q;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    // sort the smaller range recursively
    // sort the bigger range via the loop
    // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
    if J - ALeft < ARight - I then
    begin
      if ALeft < J then
        QuickSort(AValues, ALeft, J, AComparer);
      ALeft := I;
    end
    else
    begin
      if I < ARight then
        QuickSort(AValues, I, ARight, AComparer);
      ARight := J;
    end;
  until ALeft >= ARight;
end;

class function TArrayHelper<T>.BinarySearch(const AValues: array of T;
  const AItem: T; out ASearchResult: TBinarySearchResult;
  const AComparer: IComparer<T>; AIndex, ACount: SizeInt): Boolean;
var
  imin, imax, imid, ilo: Int32;

begin
  // Writeln('Enter ',Length(aValues),' Idx ',aIndex,' acount: ',aCount);
  // continually narrow search until just one element remains
  imin := AIndex;
  imax := Pred(AIndex + ACount);
  // Writeln('Start Imax, imin : ',Imax, ' - ', imin);
  ilo:=imid * imid;
  imid:=ilo*imid;
  while (imin < imax) do
  begin
    imid := (imax+imin) div 2;
    // writeln('imid',imid);

    ASearchResult.CompareResult := AComparer.Compare(AValues[imid], AItem);
    // reduce the search
    if (ASearchResult.CompareResult < 0) then
      imin := imid + 1
    else
    begin
      if ASearchResult.CompareResult = 0 then
      begin
        ASearchResult.FoundIndex := imid;
        ASearchResult.CandidateIndex := imid;
        Exit(True);
      end;
      imax := imid;
    end;
  end;
  // At exit of while:
  //   if A[] is empty, then imax < imin
  //   otherwise imax == imin

  // deferred test for equality
  // Writeln('End Imax, imin : ',Imax, ' - ', imin);
  Result:=(imax = imin);
  if Result then
    begin
    ASearchResult.CompareResult := AComparer.Compare(AValues[imin], AItem);
    ASearchResult.CandidateIndex := imin;
    Result:=(ASearchResult.CompareResult = 0);
    if Result then
      ASearchResult.FoundIndex := imin
    else
      ASearchResult.FoundIndex := -1;
    end
  else
    begin
    ASearchResult.CompareResult := 0;
    ASearchResult.FoundIndex := -1;
    ASearchResult.CandidateIndex := -1;
    end;
end;

class function TArrayHelper<T>.BinarySearch(const AValues: array of T;
  const AItem: T; out AFoundIndex: SizeInt; const AComparer: IComparer<T>;
  AIndex, ACount: SizeInt): Boolean;
var
  imin, imax, imid: Int32;
  LCompare: SizeInt;
begin
  // continually narrow search until just one element remains
  imin := AIndex;
  imax := Pred(AIndex + ACount);

  // http://en.wikipedia.org/wiki/Binary_search_algorithm
  while (imin < imax) do
  begin
    imid := (imin + imax) div 2;

    // code must guarantee the interval is reduced at each iteration
    // assert(imid < imax);
    // note: 0 <= imin < imax implies imid will always be less than imax

    LCompare := AComparer.Compare(AValues[imid], AItem);
    // reduce the search
    if (LCompare < 0) then
      imin := imid + 1
    else
    begin
      if LCompare = 0 then
      begin
        AFoundIndex := imid;
        Exit(True);
      end;
      imax := imid;
    end;
  end;
  // At exit of while:
  //   if A[] is empty, then imax < imin
  //   otherwise imax == imin

  // deferred test for equality
  LCompare := AComparer.Compare(AValues[imin], AItem);
  Result:=(imax = imin) and (LCompare = 0);
  if Result then
    AFoundIndex := imin
  else
    AFoundIndex := -1;
end;

{ TEnumerator }

function TEnumerator<T>.MoveNext: boolean;
begin
  Result:=DoMoveNext;
end;

{ TEnumerable }

function TEnumerable<T>.GetEnumerator: TMyEnumerator;
begin
  Result:=DoGetEnumerator;
end;

function TEnumerable<T>.ToArray: TMyArray;
var
  LEnumerator: TMyEnumerator;
begin
  Result:=[];
  LEnumerator := GetEnumerator;
  try
    while LEnumerator.MoveNext do
      TJSArray(Result).push(LEnumerator.Current);
  finally
    LEnumerator.Free;
  end;
end;

{ TCustomList }

function TCustomList<T>.GetCapacity: SizeInt;
begin
  Result:=length(FItems);
end;

function TCustomList<T>.PrepareAddingItem: SizeInt;
begin
  if FLength=length(FItems) then
    TJSArray(FItems).push(Default(T));

  Result := FLength;
  Inc(FLength);
end;

function TCustomList<T>.PrepareAddingRange(ACount: SizeInt): SizeInt;
var
  l: SizeInt;
begin
  if ACount < 0 then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);
  if ACount = 0 then
    Exit(FLength - 1);

  for l:=length(FItems)+1 to FLength+ACount do
    TJSArray(FItems).push(Default(T));

  Result := FLength;
  Inc(FLength, ACount);
end;

procedure TCustomList<T>.Notify(const AValue: T;
  ACollectionNotification: TCollectionNotification);
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self, AValue, ACollectionNotification);
end;

function TCustomList<T>.DoRemove(AIndex: SizeInt;
  ACollectionNotification: TCollectionNotification): T;
begin
  if (AIndex < 0) or (AIndex >= FLength) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);

  Result := FItems[AIndex];
  Dec(FLength);

  FItems[AIndex] := Default(T); // needed for refcounted types
  TJSArray(FItems).splice(AIndex,1);

  Notify(Result, ACollectionNotification);
end;

function TCustomList<T>.GetCount: SizeInt;
begin
  Result := FLength;
end;


{ TCustomListEnumerator }

function TCustomListEnumerator<T>.DoMoveNext: boolean;
begin
  Inc(FIndex);
  Result := (FList.FLength > 0) and (FIndex < FList.FLength)
end;

function TCustomListEnumerator<T>.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

function TCustomListEnumerator<T>.GetCurrent: T;
begin
  Result := FList.FItems[FIndex];
end;

constructor TCustomListEnumerator<T>.Create(AList: TCustomList<T>);
begin
  inherited Create;
  FIndex := -1;
  FList := AList;
end;

{ TList }

function TList<T>.SameValue(const Left, Right: T): Boolean;
begin
  if Assigned(FComparer) then
    Result:=(FComparer.Compare(Left, Right) = 0)
  else
    Result:=(Left = Right);
end;

procedure TList<T>.SetCapacity(AValue: SizeInt);
begin
  if AValue < Count then
    Count := AValue;
  SetLength(FItems, AValue);
end;

procedure TList<T>.SetCount(AValue: SizeInt);
begin
  if AValue < 0 then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);

  if AValue > Capacity then
    Capacity := AValue
  else if AValue < Count then
    DeleteRange(AValue, Count - AValue);

  FLength := AValue;
end;

procedure TList<T>.InitializeList;
begin
end;

procedure TList<T>.InternalInsert(AIndex: SizeInt; const AValue: T);
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);
  TJSArray(FItems).splice(AIndex,0,AValue);
  inc(FLength);

  FItems[AIndex] := AValue;
  Notify(AValue, cnAdded);
end;

function TList<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

function TList<T>.GetItem(AIndex: SizeInt): T;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);

  Result := FItems[AIndex];
end;

procedure TList<T>.SetItem(AIndex: SizeInt; const AValue: T);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);
  Notify(FItems[AIndex], cnRemoved);
  FItems[AIndex] := AValue;
  Notify(AValue, cnAdded);
end;

function TList<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

constructor TList<T>.Create;
begin
  InitializeList;
end;

constructor TList<T>.Create(const AComparer: IComparer<T>);
begin
  InitializeList;
  FComparer := AComparer;
end;

constructor TList<T>.Create(ACollection: TEnumerable<T>);
var
  LItem: T;
begin
  Create;
  for LItem in ACollection do
    Add(LItem);
end;

destructor TList<T>.Destroy;
begin
  SetCapacity(0);
end;

function TList<T>.Add(const AValue: T): SizeInt;
begin
  Result := PrepareAddingItem;
  FItems[Result] := AValue;
  Notify(AValue, cnAdded);
end;

procedure TList<T>.AddRange(const AValues: array of T);
begin
  InsertRange(Count, AValues);
end;

procedure TList<T>.AddRange(const AEnumerable: IEnumerable<T>);
var
  LValue: T;
begin
  for LValue in AEnumerable do
    Add(LValue);
end;

procedure TList<T>.AddRange(AEnumerable: TEnumerable<T>);
var
  LValue: T;
begin
  for LValue in AEnumerable do
    Add(LValue);
end;

procedure TList<T>.Insert(AIndex: SizeInt; const AValue: T);
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);

  InternalInsert(AIndex, AValue);
end;

procedure TList<T>.InsertRange(AIndex: SizeInt; const AValues: array of T);
var
  LLength, i: sizeint;
  LValue: T;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);

  LLength := Length(AValues);
  if LLength = 0 then
    Exit;

  if AIndex <> PrepareAddingRange(LLength) then
  begin
    for i := AIndex to Count - LLength -1 do
      FItems[i+LLength] := FItems[i];
    for i := 0 to LLength -1 do
      FItems[AIndex+i] := Default(T);
  end;

  for i := 0 to Pred(LLength) do
  begin
    LValue:=AValues[i];
    FItems[i+AIndex] := LValue;
    Notify(LValue, cnAdded);
  end;
end;

procedure TList<T>.InsertRange(AIndex: SizeInt; const AEnumerable: IEnumerable<T>);
var
  LValue: T;
  i: SizeInt;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);

  i := 0;
  for LValue in AEnumerable do
  begin
    InternalInsert(AIndex + i, LValue);
    Inc(i);
  end;
end;

procedure TList<T>.InsertRange(AIndex: SizeInt; const AEnumerable: TEnumerable<T>);
var
  LValue: T;
  i:  SizeInt;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);

  i := 0;
  for LValue in AEnumerable do
  begin
    InternalInsert(Aindex + i, LValue);
    Inc(i);
  end;
end;

function TList<T>.RemoveItem(const AValue: T; Direction : TDirection): SizeInt;

begin
  if Direction=TDirection.FromEnd then
    Result := LastIndexOf(AValue)
  else
    Result := IndexOf(AValue);
  if Result >= 0 then
    DoRemove(Result, cnRemoved);
end;

function TList<T>.Remove(const AValue: T): SizeInt;
begin
  Result := IndexOf(AValue);
  if Result >= 0 then
    DoRemove(Result, cnRemoved);
end;

procedure TList<T>.Delete(AIndex: SizeInt);
begin
  DoRemove(AIndex, cnRemoved);
end;

procedure TList<T>.DeleteRange(AIndex, ACount: SizeInt);
var
  LDeleted: TMyArray;
  i: SizeInt;
begin
  if ACount = 0 then
    Exit;

  if (ACount < 0) or (AIndex < 0) or (AIndex + ACount > Count) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);

  LDeleted:=TMyArray(TJSArray(FItems).splice(AIndex,Count));
  Dec(FLength, ACount);

  for i := 0 to High(LDeleted) do
    Notify(LDeleted[i], cnRemoved);
end;

function TList<T>.ExtractIndex(const AIndex: SizeInt): T;
begin
  Result := DoRemove(AIndex, cnExtracted);
end;

function TList<T>.Extract(const AValue: T): T;
var
  LIndex: SizeInt;
begin
  LIndex := IndexOf(AValue);
  if LIndex < 0 then
    Result:=Default(T)
  else
    Result := DoRemove(LIndex, cnExtracted);
end;

procedure TList<T>.Exchange(AIndex1, AIndex2: SizeInt);
var
  LTemp: T;
begin
  LTemp := FItems[AIndex1];
  FItems[AIndex1] := FItems[AIndex2];
  FItems[AIndex2] := LTemp;
end;

procedure TList<T>.Move(AIndex, ANewIndex: SizeInt);
var
  Arr: TJSArray;
  LTemp: JSValue;
  i: SizeInt;
begin
  if ANewIndex = AIndex then
    Exit;

  if (ANewIndex < 0) or (ANewIndex >= Count) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);

  Arr := TJSArray(FItems);
  LTemp := Arr[AIndex];
  if AIndex < ANewIndex then
    for i := AIndex to ANewIndex-1 do
      Arr[i] := Arr[i+1]
  else
    for i := ANewIndex downto AIndex+1 do
      Arr[i] := Arr[i-1];
  Arr[ANewIndex] := LTemp;
end;

function TList<T>.First: T;
begin
  Result := Items[0];
end;

function TList<T>.Last: T;
begin
  Result := Items[Pred(Count)];
end;

procedure TList<T>.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

function TList<T>.Contains(const AValue: T): Boolean;
begin
  Result := IndexOf(AValue) >= 0;
end;

function TList<T>.IndexOf(const AValue: T): SizeInt;
var
  i: SizeInt;
begin
  for i := 0 to Count - 1 do
    if SameValue(AValue, FItems[i]) then
      Exit(i);
  Result:=-1;
end;

function TList<T>.LastIndexOf(const AValue: T): SizeInt;
var
  i: SizeInt;
begin
  for i := Count - 1 downto 0 do
    if SameValue(AValue, FItems[i]) then
      Exit(i);
  Result:=-1;
end;

procedure TList<T>.Reverse;
var
  a, b: SizeInt;
  LTemp: T;
begin
  a := 0;
  b := Count - 1;
  while a < b do
  begin
    LTemp := FItems[a];
    FItems[a] := FItems[b];
    FItems[b] := LTemp;
    Inc(a);
    Dec(b);
  end;
end;

procedure TList<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

procedure TList<T>.Sort;
begin
  TMyArrayHelper.Sort(FItems, FComparer, 0, Count);
end;

procedure TList<T>.Sort(const AComparer: IComparer<T>);
begin
  TMyArrayHelper.Sort(FItems, AComparer, 0, Count);
end;

function TList<T>.BinarySearch(const AItem: T; out AIndex: SizeInt): Boolean;
begin
  Result := TMyArrayHelper.BinarySearch(FItems, AItem, AIndex, FComparer, 0, Count);
end;

function TList<T>.BinarySearch(const AItem: T; out AIndex: SizeInt;
  const AComparer: IComparer<T>): Boolean;
begin
  Result := TMyArrayHelper.BinarySearch(FItems, AItem, AIndex, AComparer, 0, Count);
end;

{ TPair }

constructor TPair<TKey,TValue>.Create(const AKey: TKey; const AValue: TValue);
begin
  Key:=aKey;
  Value:=aValue;
end;

{ TDictionary }

ResourceString
  SErrDictKeyNotFound = 'Key value not found';
  SErrDictDuplicateKey = 'Duplicate key value';

function TDictionary<TKey, TValue>.GetEffectiveKey(Key: TKey): TKey;

Var
  it : TJSIterator;
  v : TJSIteratorValue;
  vv : JSValue;

begin
  if Not assigned(FComparer) then
    Exit(key);
  it:=FMap.Keys;
  v:=it.next;
  While not v.Done do
    begin
    Result:=TKey(v.Value);
    if FComparer.Compare(Result,Key)=0 then
      exit;
    v:=it.Next;
    end;
  Result:=Key;
end;

function TDictionary<TKey, TValue>.GetItem(const Key: TKey): TValue;

Var
  V : JSValue;

begin
  v:=FMap.Get(GetEffectiveKey(Key));
  if isUndefined(v) then
    Raise EDictionary.Create(SErrDictKeyNotFound);
  Result:=TValue(V);
end;

procedure TDictionary<TKey, TValue>.SetItem(const Key: TKey; const Value: TValue);

Var
  V : JSValue;

begin
  v:=FMap.Get(GetEffectiveKey(Key));
  if Not isUndefined(v) then
    ValueNotify(TValue(V),cnRemoved);
  FMap.&Set(Key,Value);
  ValueNotify(Value, cnAdded);
end;

procedure TDictionary<TKey, TValue>.DoAdd(const Key: TKey; const Value: TValue);
Var
  k : Tkey;
begin
  FMap.&Set(GetEffectiveKey(Key),Value);
  KeyNotify(Key,cnAdded);
  ValueNotify(Value,cnAdded);
end;


function TDictionary<TKey, TValue>.DoRemove(const Key: TKey; Notification: TCollectionNotification): TValue;
Var
  V : JSValue;
  K : TKey;

begin
  K:=GetEffectiveKey(Key);
  V:=FMap.Get(k);
  if Not isUndefined(v) then
    begin
    FMap.Delete(k);
    Result:=TValue(v);
    KeyNotify(Key,Notification);
    ValueNotify(Result,Notification);
    end;
end;

function TDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result:=FMap.Size;
end;

function TDictionary<TKey, TValue>.DoGetEnumerator: TEnumerator<TPair<TKey,
  TValue>>;
begin
  Result:=TPairEnumerator.Create(Self);
end;

procedure TDictionary<TKey, TValue>.PairNotify(const Key: TKey; Value : TValue; Action: TCollectionNotification);

begin
  KeyNotify(Key,action);
  ValueNotify(Value,action);
end;

procedure TDictionary<TKey, TValue>.KeyNotify(const Key: TKey; Action: TCollectionNotification);
begin
  if Assigned(FOnKeyNotify) then
    FOnKeyNotify(Self,Key,Action);
end;

procedure TDictionary<TKey, TValue>.ValueNotify(const Value: TValue; Action: TCollectionNotification);
begin
  if Assigned(FOnValueNotify) then
    FOnValueNotify(Self,Value,Action);
end;

constructor TDictionary<TKey, TValue>.Create(ACapacity: Integer = 0);
begin
  FMap:=TJSMap.New;
  if ACapacity>0 then ; // ToDo
end;

constructor TDictionary<TKey, TValue>.Create(const Collection: TEnumerable<TMyPair>);

Var
  aPair : TMyPair;

begin
  Create(0);
  For aPair in Collection do
    Add(aPair.Key,aPair.Value);
end;

constructor TDictionary<TKey, TValue>.Create(const AComparer: IComparer<TKey>);
begin
  Create(0);
  FComparer:=aComparer;
end;

destructor TDictionary<TKey, TValue>.Destroy;
begin
  FreeAndNil(FKeyCollection);
  FreeAndNil(FValueCollection);
  Clear;
  FMap:=Nil;
  inherited Destroy;
end;

procedure TDictionary<TKey, TValue>.Add(const Key: TKey; const Value: TValue);
begin
  if FMap.Has(GetEffectiveKey(Key)) then
    Raise EDictionary.Create(SErrDictDuplicateKey);
  DoAdd(Key,Value);
end;

procedure TDictionary<TKey, TValue>.Remove(const Key: TKey);
begin
  doRemove(Key,cnRemoved);
end;

function TDictionary<TKey, TValue>.ExtractPair(const Key: TKey): TMyPair;

Var
  K : TKey;

begin
  Result:=Default(TMyPair);
  K:=GetEffectiveKey(Key);
  if FMap.Has(K) then
    begin
    Result.Create(Key,TValue(FMap.get(K)));
    FMap.Delete(k);
    end
  else
    Result.Create(Key,Default(TValue));
end;

function TDictionary<TKey, TValue>.CanClearMap: Boolean;

begin
  Result:=(FOnKeyNotify=Nil) and (FOnValueNotify=Nil);
end;

procedure TDictionary<TKey, TValue>.Clear;

Var
  Iter : TJSIterator;
  IVal : TJSIteratorValue;
  A : TJSValueDynArray;
  K : TKey;
  V : TValue;

begin
  if CanClearMap then
    Fmap.Clear
  else
    begin
    Iter:=FMap.Entries;
    Repeat
      IVal:=Iter.next;
      if not ival.Done then
        begin
        A:=TJSValueDynArray(IVal.Value);
        K:=TKey(A[0]);
        V:=TValue(A[1]);
        FMap.delete(k);
        PairNotify(K,V,cnRemoved);
        end;
    Until Ival.Done;
    end;
end;


function TDictionary<TKey, TValue>.TryGetValue(const Key: TKey; out Value: TValue): Boolean;

Var
  K : TKey;
begin
  K:=GetEffectiveKey(Key);
  Result:=FMap.Has(K);
  If Result then
    Value:=TValue(FMap.get(K));
end;


procedure TDictionary<TKey, TValue>.AddOrSetValue(const Key: TKey; const Value: TValue);

Var
  k : TKey;

begin
  K:=GetEffectiveKey(Key);
  if Not FMap.Has(k) then
    DoAdd(Key,Value)
  else
    SetItem(K,Value);
end;

function TDictionary<TKey, TValue>.ContainsKey(const Key: TKey): Boolean;
begin
  Result:=FMap.Has(GetEffectiveKey(Key));
end;

function TDictionary<TKey, TValue>.ContainsValue(const Value: TValue): Boolean;

Var
  It : TJSIterator;
  Res : TJSIteratorValue;

begin
  Result:=False;
  It:=FMap.Values;
  Repeat
    Res:=It.next;
    if not Res.done then
      Result:=(Value=TValue(Res.value));
  Until (Result or Res.done);
end;

function TDictionary<TKey, TValue>.ToArray: TArray<TMyPair>;
begin
  Result:=inherited ToArray;
end;

function TDictionary<TKey, TValue>.GetKeys: TKeyCollection;
begin
  if FKeyCollection=Nil then
    FKeyCollection:=TKeyCollection.Create(Self);
  Result:=FKeyCollection;
end;

function TDictionary<TKey, TValue>.GetValues: TValueCollection;
begin
  if FValueCollection=Nil then
    FValueCollection:=TValueCollection.Create(Self);
  Result:=FValueCollection;
end;

function TDictionary<TKey, TValue>.GetEnumerator: TPairEnumerator;
begin
  Result:=TPairEnumerator.Create(Self);
end;

{ TDictionary.TPairEnumerator }

function TDictionary<TKey, TValue>.TPairEnumerator.GetCurrent: TMyPair;
begin
  Result:=DoGetCurrent();
end;

function TDictionary<TKey, TValue>.TPairEnumerator.DoGetCurrent: TMyPair;

Var
  A : TJSValueDynArray;

begin
  A:=TJSValueDynArray(FVal.Value);
  Result:=Default(TMyPair);
  Result.Create(TKey(A[0]),TValue(A[1]));
end;

function TDictionary<TKey, TValue>.TPairEnumerator.DoMoveNext: Boolean;
begin
  FVal:=FIter.Next;
  Result:=Not FVal.Done;
end;

constructor TDictionary<TKey, TValue>.TPairEnumerator.Create(const ADictionary: TMyType);
begin
  FIter:=ADictionary.FMap.Entries;
end;

function TDictionary<TKey, TValue>.TPairEnumerator.MoveNext: Boolean;
begin
  Result:=DoMoveNext;
end;

{ TDictionary.TKeyEnumerator }

function TDictionary<TKey, TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  Result:=DoGetCurrent();
end;

function TDictionary<TKey, TValue>.TKeyEnumerator.DoGetCurrent: TKey;
begin
  Result:=TKey(FVal.Value);
end;

function TDictionary<TKey, TValue>.TKeyEnumerator.DoMoveNext: Boolean;
begin
  FVal:=FIter.Next;
  Result:=Not FVal.Done;
end;

constructor TDictionary<TKey, TValue>.TKeyEnumerator.Create(const ADictionary: TMyType);
begin
  Create(ADictionary.FMap.Keys);
end;

constructor TDictionary<TKey, TValue>.TKeyEnumerator.Create(const AIter : TJSIterator);
begin
  FIter:=aIter;
end;

function TDictionary<TKey, TValue>.TKeyEnumerator.MoveNext: Boolean;
begin
  Result:=DoMoveNext;
end;

{ TDictionary.TValueEnumerator }

function TDictionary<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  Result:=DoGetCurrent();
end;

function TDictionary<TKey, TValue>.TValueEnumerator.DoGetCurrent: TValue;
begin
  Result:=TValue(FVal.Value);
end;

function TDictionary<TKey, TValue>.TValueEnumerator.DoMoveNext: Boolean;
begin
  FVal:=FIter.Next;
  Result:=Not FVal.Done;
end;

constructor TDictionary<TKey, TValue>.TValueEnumerator.Create(const ADictionary: TMyType);
begin
  Create(aDictionary.FMap.Values);
end;

constructor TDictionary<TKey, TValue>.TValueEnumerator.Create(const AIter: TJSIterator);
begin
  FIter:=AIter;
end;

function TDictionary<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  Result:=DoMoveNext;
end;

{ TDictionary.TValueCollection }

function TDictionary<TKey, TValue>.TValueCollection.GetCount: Integer;
begin
  Result:=FMap.Size;
end;

function TDictionary<TKey, TValue>.TValueCollection.DoGetEnumerator: TEnumerator<TValue>;
begin
  Result:=TValueEnumerator.Create(FMap.Values);
end;

constructor TDictionary<TKey, TValue>.TValueCollection.Create(const ADictionary: TMyType);
begin
  FMap:=ADictionary.FMap;
end;

function TDictionary<TKey, TValue>.TValueCollection.GetEnumerator: TValueEnumerator;
begin
  Result:=TValueEnumerator(DoGetEnumerator);
end;

function TDictionary<TKey, TValue>.TValueCollection.ToArray: TArray<TValue>;

Var
  I : Integer;
  P : TValue;

begin
  SetLength(Result,FMap.Size);
  For P in Self do
    begin
    Result[i]:=P;
    Inc(I);
    End;
end;

{ TDictionary.TKeyCollection }

function TDictionary<TKey, TValue>.TKeyCollection.GetCount: Integer;
begin
  Result:=FMap.Size;
end;

function TDictionary<TKey, TValue>.TKeyCollection.DoGetEnumerator: TEnumerator<TKey>;
begin
  Result:=GetEnumerator;
end;

constructor TDictionary<TKey, TValue>.TKeyCollection.Create(const ADictionary: TMyType);
begin
  FMap:=aDictionary.FMap;
end;

function TDictionary<TKey, TValue>.TKeyCollection.GetEnumerator: TKeyEnumerator;
begin
  Result:=TKeyEnumerator.Create(FMap.Keys);
end;

function TDictionary<TKey, TValue>.TKeyCollection.ToArray: TArray<TKey>;
begin
  Result:=inherited ToArray;
end;


{ TObjectList<T> }

procedure TObjectList<T>.Notify(const aValue: T; Action: TCollectionNotification);

Var
  A : TObject absolute aValue; // needed to fool compiler

begin
  inherited Notify(aValue, Action);
  if FObjectsOwner and (action = cnRemoved) then
    a.Free;
end;

constructor TObjectList<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FObjectsOwner := AOwnsObjects;
end;

constructor TObjectList<T>.Create(const AComparer: IComparer<T>; AOwnsObjects: Boolean);
begin
  inherited Create(AComparer);
  FObjectsOwner := AOwnsObjects;
end;

constructor TObjectList<T>.Create(const ACollection: TEnumerable<T>; aOwnsObjects: Boolean);
begin
  inherited Create(ACollection);
  FObjectsOwner := AOwnsObjects;
end;

{ TThreadList }

constructor TThreadList<T>.Create;
begin
  inherited Create;
  FLock:=0;
  FList := TList<T>.Create;
  FDuplicates := dupIgnore;
end;

destructor TThreadList<T>.Destroy;
begin
  // No need to unlock.
  FList.Free;
  inherited Destroy;
end;

procedure TThreadList<T>.Add(const Item: T);
begin
  LockList;
  try
    if (Duplicates = dupAccept) or (FList.IndexOf(Item) = -1) then
      FList.Add(Item)
    else if Duplicates = dupError then
      raise EListError.Create(SDuplicateItem);
  finally
    UnlockList;
  end;
end;

procedure TThreadList<T>.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

function TThreadList<T>.LockList: TList<T>;
begin
  Inc(FLock);
  if (FLock>1) then
    Writeln('Locking already locked list, lockcount : ',FLock);
  Result:=FList;
end;

procedure TThreadList<T>.Remove(const Item: T);

begin
  RemoveItem(Item,TDirection.FromBeginning);
end;

procedure TThreadList<T>.RemoveItem(const Item: T; Direction: TDirection);
begin
  LockList;
  try
    FList.RemoveItem(Item,Direction);
  finally
    UnlockList;
  end;
end;

procedure TThreadList<T>.UnlockList;
begin
  Dec(FLock);
  if (FLock<0) then
    Writeln('Unlocking already unlocked list, lockcount : ',FLock);
end;

{ TObjectDictionary }

function TObjectDictionary<TKey, TValue>.CanClearMap: Boolean;
begin
  Result:=(Inherited CanClearMap) and (FOwnerships=[]);
end;

procedure TObjectDictionary<TKey, TValue>.KeyNotify(const Key: TKey; Action: TCollectionNotification);

Var
  A : TObject absolute key; // Avoid typecast, refused by compiler

begin
  inherited KeyNotify(Key, Action);
  if (doOwnsKeys in FOwnerships) and (Action = cnRemoved) then
    A.Free;
end;

procedure TObjectDictionary<TKey, TValue>.ValueNotify(const Value: TValue; Action: TCollectionNotification);

Var
  A : TObject absolute Value; // Avoid typecast, refused by compiler

begin
  inherited ValueNotify(Value, Action);
  if (doOwnsValues in FOwnerships) and (Action = cnRemoved) then
    A.Free;
end;

constructor TObjectDictionary<TKey, TValue>.Create(aOwnerships: TDictionaryOwnerships; ACapacity: Integer);
begin
  FOwnerShips:=aOwnerships;
  inherited Create(ACapacity);
end;

constructor TObjectDictionary<TKey, TValue>.Create(aOwnerships: TDictionaryOwnerships);
begin
  Inherited Create;
  FOwnerShips:=aOwnerships;
end;

{ TQueue }

function TQueue<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result:=GetEnumerator;
end;

function TQueue<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TQueue<T>.SetCapacity(AValue: SizeInt);
begin
  if AValue < Count then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);
  if FLow>0 then
    Rebase;
  SetLength(FItems,aValue);
end;

function TQueue<T>.DoRemove(AIndex: SizeInt; ACollectionNotification: TCollectionNotification): T;

begin
  if (FLow>=FLength) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);
  Result := FItems[AIndex];
  FItems[AIndex] := Default(T);
  Inc(FLow);
  if FLow >= FLength then
    begin
    FLow:=0;
    FLength:=0;
    end;
  Notify(Result, ACollectionNotification);
end;

function TQueue<T>.GetCount: SizeInt;
begin
  Result:=FLength-FLow;
end;

constructor TQueue<T>.Create;
begin
  FMaxGapLength:=10;
end;

constructor TQueue<T>.Create(ACollection: TEnumerable<T>);

var
  Itm: T;

begin
  Create;
  for Itm in ACollection do
    Enqueue(Itm);
end;

destructor TQueue<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TQueue<T>.Enqueue(const AValue: T);
begin
  if Capacity<=FLength then
    SetCapacity(FLength+10);
  FItems[FLength]:=aValue;
  Inc(FLength);
  Notify(aValue,cnAdded);
end;

function TQueue<T>.Dequeue: T;
begin
  Result := DoRemove(FLow, cnRemoved);
  if FLow>FMaxGapLength then
    Rebase;
end;

function TQueue<T>.Extract: T;
begin
  Result := DoRemove(FLow, cnExtracted);
  if FLow>FMaxGapLength then
    Rebase;
end;

function TQueue<T>.Peek: T;
begin
  if (Count=0) then
      raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);
  Result:=FItems[FLow];
end;

procedure TQueue<T>.Clear;
begin
  while Count <> 0 do
    Dequeue;
end;

procedure TQueue<T>.Rebase;

Var
  I,Spare : integer;

begin
  Spare:=Capacity-Count;
  if FLow>0 then
    begin
    For I:=Flow to FLength do
      FItems[I-FLow]:=FItems[I];
    SetLength(FItems,FLength+Spare);
    FLength:=FLength-Flow+1;
    Flow:=0;
    end;
end;

procedure TQueue<T>.TrimExcess;
begin
  Rebase;
  SetCapacity(Count);
end;

{ TQueue.TEnumerator }

constructor TQueue<T>.TEnumerator.Create(AQueue: TMyType);
begin
  aQueue.Rebase;
  Inherited Create(aQueue);
end;

{ TObjectQueue }

procedure TObjectQueue<T>.Notify(const Value: T; Action: TCollectionNotification);

Var
  A : TObject absolute Value;

begin
  inherited Notify(Value, Action);
  if OwnsObjects and (Action = cnRemoved) then
    A.Free;
end;

constructor TObjectQueue<T>.Create(AOwnsObjects: Boolean);
begin
  Inherited create;
  FOwnsObjects:=aOwnsObjects;
end;

constructor TObjectQueue<T>.Create(const Collection: TEnumerable<T>; AOwnsObjects: Boolean);
Var
  A : T;

begin
  Create(aOwnsObjects);
  For A in Collection do
    EnQueue(A);
end;

procedure TObjectQueue<T>.Dequeue;
begin
  Inherited DeQueue;
end;

{ TStack }

function TStack<T>.DoRemove(aIndex : SizeInt; ACollectionNotification: TCollectionNotification): T;

begin
  if (FLength=0) or (aIndex<>FLength-1) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);
  Result:=FItems[AIndex];
  FItems[AIndex] := Default(T);
  Dec(FLength);
  Notify(Result, ACollectionNotification);
end;

procedure TStack<T>.SetCapacity(aValue: SizeInt);

begin
  if AValue < Count then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);
  SetLength(FItems,aValue);
end;

function TStack<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result:=GetEnumerator;
end;

function TStack<T>.GetEnumerator: TEnumerator;
begin
  Result:=TEnumerator.Create(Self);
end;


destructor TStack<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TStack<T>.Clear;
begin
  While Count>0 do Pop;
end;

procedure TStack<T>.Push(const aValue: T);
begin
  if Capacity<=FLength then
    SetCapacity(FLength+10);
  FItems[FLength]:=aValue;
  Inc(FLength);
  Notify(aValue,cnAdded);
end;

function TStack<T>.Pop: T;
begin
  Result:=DoRemove(FLength-1,cnRemoved);
end;

function TStack<T>.Peek: T;
begin
  if Count<1 then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);
  Result:=FItems[FLength-1];
end;

function TStack<T>.Extract: T;
begin
  Result:=DoRemove(FLength-1,cnExtracted);
end;

procedure TStack<T>.TrimExcess;
begin
  SetCapacity(FLength);
end;

{ TCustomInvertedListEnumerator }

function TCustomInvertedListEnumerator<T>.DoMoveNext: boolean;
begin
  Result:=FIndex>0;
  If Result then
    Dec(FIndex);
end;

function TCustomInvertedListEnumerator<T>.DoGetCurrent: T;
begin
  Result:=FList.FItems[FIndex];
end;

function TCustomInvertedListEnumerator<T>.GetCurrent: T;
begin
  Result:=DoGetCurrent();
end;

constructor TCustomInvertedListEnumerator<T>.Create(AList: TCustomList<T>);
begin
  inherited Create;
  FList:=AList;
  FIndex:=AList.FLength;
end;

{ TStack.TEnumerator }

constructor TStack<T>.TEnumerator.Create(AStack: TMyType);
begin
  Inherited Create(aStack);
end;

{ TObjectStack }

procedure TObjectStack<T>.Notify(const aValue: T; Action: TCollectionNotification);

Var
  A : T absolute aValue;

begin
  inherited Notify(aValue, Action);
  if (Action=cnRemoved) and FOwnsObjects then
    a.Free;
end;

constructor TObjectStack<T>.Create(AOwnsObjects: Boolean);
begin
  Inherited Create;
  FOwnsObjects:=aOwnsObjects;
end;

constructor TObjectStack<T>.Create(const Collection: TEnumerable<T>; AOwnsObjects: Boolean);

Var
  A : T;

begin
  Create(aOwnsObjects);
  For A in Collection do
    Push(A);
end;

procedure TObjectStack<T>.Pop;
begin
  Inherited Pop;
end;

end.
