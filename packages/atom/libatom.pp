{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2017-2020 by the Pas2JS development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit libatom;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  JS, types, web;

Type
  TAtomHandler = reference to procedure;
  TAtomObjectFunctionHandler = reference to function : TJSObject;
  TAtomStringArrayHandler = reference to procedure (aSelected : TStringDynArray);
  TAtomStringHandler = reference to procedure (aValue : String);
  TAtomPromiseHandler = reference to function : TJSPromise;
  TAtomIntegerHandler = reference to procedure (aValue : NativeInt);
  TAtomBooleanFunctionHandler = reference to function : Boolean;

  TAtom = Class;
  TAtomCursor = Class;
  TAtomDisplayMarker = class;
  TAtomRange = class;
  TAtomPoint = class;
  TAtomMarkerLayer = Class;
  TAtomDirectory = class;
  TAtomFile = Class;
  TAtomGutter = Class;
  TAtomMenuItem = class;
  TAtomPanel = Class;
  TAtomSelection = Class;
  TAtomLayerDecoration = class;

  TAtomSelectionHandler = reference to procedure(aSelection : TAtomSelection);

  TAtomRangeArray = array of array of NativeInt;
  TAtomDisplayMarkerArray = array of TAtomDisplayMarker;
  TAtomRangeDynArray = array of TAtomRange;
  TAtomSelectionArray = array of TAtomSelection;

  TTimingMarker = class external name 'Object' (TJSObject)
    label_ : string; external name 'label';
    time : NativeInt;
  end;
  TTimingMarkerArray = Array of TTimingMarker;

  TAtomOpenOptions = class external name 'Object' (TJSObject)
    pathsToOpen : TStringDynArray;
    newWindow : Boolean;
    devMode : boolean;
    safeMode : boolean;
  end;

  TAtomWindowSize = class external name 'Object' (TJSObject)
    width : NativeInt;
    height : NativeInt;
  end;

  TAtomWindowPosition = class external name 'Object' (TJSObject)
    x : NativeInt;
    y : NativeInt;
  end;

  TAtomConfirmOptions  = class external name 'Object' (TJSObject)
    type_ : string; external name 'type';
    defaultId : integer;
    title : string;
    detail : string;
    checkboxLabel : string;
    checkboxChecked : boolean;
    icon : TJSObject;
    cancelId : integer;
    noLink : boolean;
    normalizeAccessKeys : boolean;
    defaultPath : string;
    buttonLabel : string;
    message : string;
    detailedMessage : string;
    buttons : array of string;
    buttonsObj : TJSObject; external name 'buttons';
  end;

  TAtomPickFolderHandler = TAtomStringArrayHandler;
  TAtomConfirmHandler = reference to procedure (buttonIdx : Integer);

  TAtomNewScopeDescriptor = Class external name 'Object' (TJSObject)
    scopes : TStringDynArray;
  end;

  TAtomScopeDescriptor = Class external name 'ScopeDescriptor' (TJSObject)
  Public
    constructor new(aObject : TAtomNewScopeDescriptor);
    function getScopesArray : TStringDynArray;
  end;

  TAtomDisposable = Class external name 'Disposable' (TJSObject)
  public
    constructor new(aDisposalAction : TAtomHandler);
    class function isDisposable(aObject : TJSObject) : boolean;
    procedure dispose;
  end;

  TAtomError = Class external name 'Object' (TJSObject)
    originalError : TJSObject;
    message : string;
    url : string;
    line : integer;
    column : integer;
  end;

  TAtomErrorEx = Class external name 'Object' (TAtomError)
    procedure preventDefault;
  end;

  TAtomBufferChange = class external name 'Object' (TJSObject)
    oldRange : TAtomRange;
    newRange : TAtomRange;
    oldText : String;
    newText : String;
  end;
  TAtomBufferChangeArray = Array of TAtomBufferChange;

  TAtomWillThrowErrorHandler = reference to procedure (aEvent: TAtomErrorEx);
  TAtomDidThrowErrorHandler = reference to procedure (aEvent: TAtomError);

  TAtomHistoryProject = Class external name 'HistoryProject' (TJSObject)
  Public
    paths : TStringDynArray;
    lastOpened : TJSDate;
    constructor new(aPaths : TStringDynArray);
    constructor new(aPaths : TStringDynArray;aLastOpened : TJSDate);
  end;
  TAtomHistoryProjectArray = Array of TAtomHistoryProject;

  TAtomHistoryManager = Class external name 'HistoryManager' (TJSObject)
    Function getProjects : TAtomHistoryProjectArray;
    function clear : TJSPromise;
    function onDidChangeProjects(aHandler : TAtomHandler) : TAtomDisposable;
  end;

  TAtomMarkerChange = class external name 'Object' (TJSObject)
    oldHeadPosition : TAtomPoint;
    newHeadPosition : TAtomPoint;
    oldTailPosition : TAtomPoint;
    newTailPosition : TAtomPoint;
    wasValid : Boolean;
    isValid : boolean;
    hadTail : Boolean;
    hasTail : Boolean;
    oldProperties : TJSObject;
    newProperties : TJSObject;
    textChanged : Boolean;
  end;

  TAtomMarkerChangeHandler = reference to procedure(aChange : TAtomMarkerChange);

  TAtomMarkerPropertiesMatch = class external name 'Object' (TJSObject)
    startPosition,
    endPosition : TAtomPoint;
    startsInRange,
    endsInRange: TAtomRange;
    startRow,
    endRow : NativeInt;
    intersectsRowRange : TNativeIntDynArray;
    containsRange : TAtomRange;
    containsPosition : TAtomPoint;
    containedInRange : TAtomRange;
    intersectsRange : TAtomRange;
  end;

  TAtomMarkerRangeOptions  = class external name 'Object' (TJSObject)
    reversed : Boolean;
    clipDirection : String; // Only for screen range
  end;



  // No documentation ?
  TAtomMarker = class external name 'Marker' (TJSObject)
  Public
    procedure destroy;
    function copy(aObject : TJSObject) : TAtomDisplayMarker;
    function onDidChange(aHandler : TAtomMarkerChangeHandler) : TAtomDisposable;
    function onDidDestroy(aHandler : TAtomHandler) : TAtomDisposable;
    function isValid : Boolean;
    function isDestroyed : Boolean;
    function isExclusive : Boolean;
    function isReversed : boolean;
    function getInvalidationStrategy : string;
    function getProperties : TJSObject;
    procedure setProperties(aProps : TJSObject);
    function matchesProperties(aProps : TAtomMarkerPropertiesMatch) : boolean;
    function matchesProperties(aProps : TJSObject) : boolean;
    function compare (aMarker : TAtomMarker) : NativeInt;
    function isEqual (aMarker : TAtomMarker) : Boolean;
    function getRange : TAtomRange;
    procedure setRange(aRange : TAtomRange);
    procedure setRange(aRange : TAtomRange; aOptions : TAtomMarkerRangeOptions);
    function getHeadPosition : TAtomPoint;
    procedure setHeadPosition (aValue : TAtomPoint);
    function getTailPosition : TAtomPoint;
    procedure setTailPosition(aValue : TAtomPoint);
    function getStartPosition : TAtomPoint;
    function getEndPosition : TAtomPoint;
    function hasTail :  Boolean;
    procedure plantTail;
    procedure clearTail;

  end;
  TAtomMarkerArray = array of TAtomMarker;
  TAtomMarkerHandler = reference to procedure(aMarker : TAtomMarker);

  // This class does not actually exist.
  // Items must conform to this "interface".
  TAtomWorkspaceItem = class external name 'Object' (TJSObject)
    // Required
    function getTitle : String;
    // optional
    function getElement : TJSHTMLElement;
    procedure destroy;
    function onDidDestroy(aHandler : TAtomHandler) : TAtomDisposable;
    function serialize : TJSObject;
    function getURI : string;
    function getLongTitle : String;
    function onDidChangeTitle(aHandler : TAtomHandler) : TAtomDisposable;
    function getIconName : string;
    function onDidChangeIcon(aHandler : TAtomHandler) : TAtomDisposable;
    function getDefaultLocation : String;
    function getAllowedLocations : TStringDynArray;
    function isPermanentDockItem : Boolean;
    procedure save;
    procedure saveAs(aPath : String);
    function getPath : String;
    function isModified : Boolean;
    function onDidChangeModified(aHandler : TAtomHandler) : TAtomDisposable;
    function copy : TAtomWorkspaceItem;
    function getPreferredHeight : NativeInt;
    function onDidTerminatePendingState(aHandler : TAtomHandler) : TAtomDisposable;
    function shouldPromptToSave : Boolean;
  end;


  TAtomLoadBufferOptions = class external name 'Object' (TJSObject)
    encoding : string;
    shouldDestroyOnFileDelete : TAtomBooleanFunctionHandler;
  end;

  TAtomNewTextBuffer = class external name 'Object' (TJSObject)
    text : string;
    shouldDestroyOnFileDelete : TAtomBooleanFunctionHandler;
  end;


  TAtomTextBufferChangingEvent  = class external name 'Object' (TJSObject)
    changes : TAtomBufferChangeArray;
  end;

  TAtomTexTAtomBufferChangedEvent = class external name 'Object' (TAtomTextBufferChangingEvent)
    oldRange : TAtomRange;
    newRange : TAtomRange;
  end;

  TAtomSaveEvent = class external name 'Object' (TJSObject)
    path : string;
  end;

  TAtomWatchErrorEvent = class external name 'Object' (TJSObject)
    error : TJSError;
    handle : TAtomHandler;
  end;

  TAtomSetTextOptions  = class external name 'Object' (TJSObject)
    normalizeLineEndings : boolean;
    undo : string;
  end;

  TAtomMarkerLayerOptions = class external name 'Object' (TJSObject)
    maintainHistory : Boolean;
    persistent : boolean;
    role : string;
  end;

  TAtomMarkRangeOptions  = class external name 'Object' (TJSObject)
    reversed : Boolean;
    invalidate : string;
    exclusive : Boolean;
  end;

  TAtomFindMarkerOptions = class external name 'Object' (TJSObject)
    startPosition,
    endPosition,
    containsPoint: TAtomPoint;
    startsInRange,
    endsInRange,
    containsRange : TAtomRange;
    startRow,
    endRow,
    intersectsRow,
    endScreenRow : NativeInt;
  end;

  TAtomUndoRedoOptions = class external name 'Object' (TJSObject)
    selectionsMarkerLayer : TAtomMarkerLayer;
  end;

  TAtomTransactOptions = class external name 'Object' (TJSObject)
    groupingInterval : integer;
    selectionsMarkerLayer : TAtomMarkerLayer;
  end;

  TAtomBufferScanOptions = class external name 'Object' (TJSObject)
    leadingContextLineCount : NativeInt;
    trailingContextLineCount : NativeInt;
  end;

  TAtomBufferScanMatch = class external name 'Object' (TJSObject)
    match : JSValue;
    matchText : string;
    range : TAtomRange;
    leadingContextLines : TStringDynArray;
    trailingContextLines : TStringDynArray;
    procedure stop;
    procedure replace(S : String);
  end;

  TAtomBufferScanHandler = reference to procedure(aMatch : TAtomBufferScanMatch);
  TAtomBufferReplaceHandler = reference to procedure (aValue : TJSObject);


  TAtomTexTAtomBufferChangedHandler = reference to procedure(aEvent : TAtomTexTAtomBufferChangedEvent);
  TAtomTextBufferChangingHandler = reference to procedure(aEvent : TAtomTextBufferChangingEvent);
  TAtomModifiedHandler = reference to procedure(aModified : Boolean);
  TAtomSaveHandler = reference to procedure(aEvent : TAtomSaveEvent);
  TAtomWatchErrorHandler = reference to procedure(aErrorObject : TAtomWatchErrorEvent);

  TAtomTextBuffer = class external name 'TextBuffer' (TJSObject)
  Public
    class function load(aSource :string) : TJSPromise;
    class function load(aSource :string; aOptions : TAtomLoadBufferOptions) : TJSPromise;
    class function loadSync(aSource :string) : TAtomTextBuffer;
    class function loadSync(aSource :string; aOptions : TAtomLoadBufferOptions) : TAtomTextBuffer;
    class function deserialize(aSource : TJSObject) : TJSPromise;
    constructor new (aSource : string);
    constructor new (aNewOptions : TAtomNewTextBuffer);
    // Events
    function onWillChange(aHandler : TAtomHandler) : TAtomDisposable;
    function onDidChange(aHandler : TAtomTexTAtomBufferChangedHandler) : TAtomDisposable;
    function onDidStopChanging(aHandler : TAtomTextBufferChangingHandler) : TAtomDisposable;
    function onDidConflict(aHandler : TAtomHandler) : TAtomDisposable;
    function onDidChangeModified(aHandler : TAtomModifiedHandler) : TAtomDisposable;
    function onDidUpdateMarkers(aHandler : TAtomHandler) : TAtomDisposable;
    function onDidCreateMarker(aHandler : TAtomMarkerHandler) : TAtomDisposable;
    function onDidChangePath(aHandler : TAtomStringHandler) : TAtomDisposable;
    function onDidChangeEncoding(aHandler : TAtomStringHandler) : TAtomDisposable;
    function onWillSave(aHandler : TAtomHandler) : TAtomDisposable;
    function onWillSave(aHandler : TAtomPromiseHandler) : TAtomDisposable;
    function onDidSave(aHandler : TAtomSaveHandler) : TAtomDisposable;
    function onDidDelete(aHandler : TAtomHandler) : TAtomDisposable;
    function onWillReload(aHandler : TAtomHandler) : TAtomDisposable;
    function onDidReload(aHandler : TAtomHandler) : TAtomDisposable;
    function onDidDestroy(aHandler : TAtomHandler) : TAtomDisposable;
    function onWillThrowWatchError(aHandler : TAtomWatchErrorHandler) : TAtomDisposable;
    function getStoppedChangingDelay : NativeInt;
    // File details
    function isModified : Boolean;
    function isInconflict : Boolean;
    function getPath : String;
    procedure setPath (aValue : string);
    procedure setEncoding(aValue : string);
    function getEncoding : string;
    function getUri : string;
    // Reading text
    function isEmpty : String;
    function getText : String;
    function getTextInRange (aRange : TAtomRange): String;
    function getLines : TStringDynArray;
    function getLastLine : string;
    function lineForRow(aRow : NativeInt) : string;
    function lineEndingForRow(aRow : NativeInt) : string;
    function lineLengthForRow(aRow : NativeInt) : Nativeint;
    function isRowBlank(aRow : NativeInt) : boolean;
    function previousNonBlankRow(aStartRow : NativeInt) : NativeInt;
    function previousNonBlankRowValue(aStartRow : NativeInt) : JSValue; external name 'previousNonBlankRow';
    function nextNonBlankRow(aStartRow : NativeInt) : NativeInt;
    function nextNonBlankRowValue(aStartRow : NativeInt) : JSValue; external name 'nextNonBlankRow';
    function hasAstral : Boolean;
    // Mutating text
    function setText(aText : string) : TAtomRange;
    procedure setTextViaDiff(aText : string);
    function setTextInRange(aRange : TAtomRange; aText : string; AOptions : TAtomSetTextOptions) : TAtomRange; overload;
    function setTextInRange(aRange : TAtomRange; aText : string) : TAtomRange; overload;
    function insertText(aPosition : TAtomPoint; aText : string; AOptions : TAtomSetTextOptions) : TAtomRange; overload;
    function insertText(aPosition : TAtomPoint; aText : string) : TAtomRange; overload;
    function appendText(aText : string; AOptions : TAtomSetTextOptions) : TAtomRange; overload;
    function appendText(aText : string) : TAtomRange; overload;
    function delete(aRange : TAtomRange) : TAtomRange;
    function deleteRow(aRow : NativeInt) : TAtomRange;
    function deleteRows(aStartRow,aEndRow : NativeInt) : TAtomRange;
    // Markers
    function addMarkerLayer (aOptions : TAtomMarkerLayerOptions) : TAtomMarkerLayer; overload;
    function addMarkerLayer : TAtomMarkerLayer; overload;
    function getMarkerLayer(aID : String) : TAtomMarkerLayer;
    function getDefaultMarkerLayer : TAtomMarkerLayer;
    function markRange(aRange : TAtomRange) : TAtomDisplayMarker; overload;
    function markRange(aRange : TAtomRange; aOptions : TAtomMarkRangeOptions) : TAtomMarker; overload;
    function markPosition(aRange : TAtomPoint) : TAtomDisplayMarker; overload;
    function markPosition(aRange : TAtomPoint; aOptions : TAtomMarkRangeOptions) : TAtomMarker; overload;
    function getMarkers : TAtomDisplayMarkerArray;
    function getMarker(aID : nativeInt) : TAtomMarker;
    function findMarkers(aOptions : TAtomFindMarkerOptions ) : TAtomMarkerArray;
    function getMarkerCount : NativeInt;
    // History;
    function undo(aOptions : TAtomUndoRedoOptions) : Boolean; overload;
    function undo : Boolean; overload;
    function redo(aOptions : TAtomUndoRedoOptions) : Boolean; overload;
    function redo : Boolean;overload;
    procedure transact(options : TAtomTransactOptions; groupingInterval : Integer; aFunction : TAtomHandler);
    procedure transact(groupingInterval : Integer; aFunction : TAtomHandler);
    procedure transact(aFunction : TAtomHandler);
    procedure abortTransaction;
    procedure clearUndoStack;
    function createCheckpoint(aOptions : TAtomUndoRedoOptions) : JSValue; overload;
    function createCheckpoint : JSValue; overload;
    procedure revertToCheckpoint(aID : JSValue; aOptions : TAtomUndoRedoOptions); overload;
    procedure revertToCheckpoint(aID : JSValue); overload;
    function groupChangesSinceCheckpoint(aID : JSValue; aOptions : TAtomUndoRedoOptions) : Boolean; overload;
    function groupChangesSinceCheckpoint(aID : JSValue) : Boolean; overload;
    function groupLastChanges: Boolean; overload;
    function getChangesSinceCheckpoint : TAtomBufferChangeArray; overload;
    // Search and replace
    procedure scan (aRegex : TJSRegexp; aOptions : TAtomBufferScanOptions; aHandler : TAtomBufferScanHandler); overload;
    procedure scan (aRegex : TJSRegexp; aHandler : TAtomBufferScanHandler); overload;
    procedure backwardsScan (aRegex : TJSRegexp; aOptions : TAtomBufferScanOptions; aHandler : TAtomBufferScanHandler); overload;
    procedure backwardsScan (aRegex : TJSRegexp; aHandler : TAtomBufferScanHandler); overload;
    procedure scanInRange (aRegex : TJSRegexp; aRange : TAtomRange; aOptions : TAtomBufferScanOptions; aHandler : TAtomBufferScanHandler); overload;
    procedure scanInRange (aRegex : TJSRegexp; aRange : TAtomRange; aHandler : TAtomBufferScanHandler); overload;
    function replace (aRegex : TJSRegexp; aReplacement : String) : NativeInt;
    // Buffer range details
    function getRange : TAtomRange;
    function getLineCount : NativeInt;
    function getLastRow : NativeInt;
    function getFirstPosition : TAtomPoint;
    function getEndPosition : TAtomPoint;
    function getLength : NativeInt;
    function getMaxCharacterIndex : NativeInt;
    function getRangeForRow (aRow : integer; includeNewLine : Boolean) : TAtomRange; overload;
    function getRangeForRow (aRow : integer) : TAtomRange; overload;
    function getCharacterIndexForPosition(aPoint : TAtomPoint) : NativeInt; overload;
    function getCharacterIndexForPosition(aPoint : array of NativeInt) : NativeInt; overload;
    function getPositionForCharacterIndex(aIndex : NativeInt) : TAtomPoint;
    function clipRange(aRange : TAtomRange) : TAtomRange; overload;
    function clipRange(aRange : TAtomRangeArray) : TAtomRange; overload;
    function clipPosition(aPos : TAtomPoint) : TAtomPoint; overload;
    function clipPosition(aPos : array of NativeInt) : TAtomPoint; overload;
    function save : TJSPromise;
    function saveAs(aPath : string) : TJSPromise;
    function reload : TJSPromise;
  end;

  TAtomGitrepoOpenOptions = Class external name 'Object' (TJSObject)
    refreshOnWindowFocus : boolean;
  end;

  TAtomGitRepositoryChange  = Class external name 'Object' (TJSObject)
    path : string;
    pathStatus : nativeint;
  end;

  TAtomAheadBehindCount  = Class external name 'Object' (TJSObject)
    ahead,behind : nativeint;
  end;


  TAtomGitReference  = Class external name 'Object' (TJSObject)
    heads,remotes,tags : TStringDynArray;
  end;

  TAtomDiffStats = Class external name 'Object' (TJSObject)
    added, deleted : NativeInt;
  end;

  TAtomHunk = Class external name 'Object' (TJSObject)
    oldStart,newStart, oldLines, newLines  : NativeInt;
  end;

  TAtomHunkArray = Array of TAtomHunk;

  TAtomGitStatusChangeHandler = reference to procedure (aChange : TAtomGitRepositoryChange);
  TAtomGitRepository = class external name 'GitRepository' (TJSObject)
    class function open (aPath : String) : TAtomGitRepository; overload;
    class function open (aPath : String; Options : TAtomGitrepoOpenOptions) : TAtomGitRepository; overload;
    procedure destroy;
    function isDestroyed : boolean;
    function onDidDestroy(aHandler : TAtomHandler) : TAtomDisposable;
    function onDidChangeStatus(aHandler : TAtomGitStatusChangeHandler) : TAtomDisposable;
    function onDidChangeStatuses(aHandler : TAtomHandler) : TAtomDisposable;
    function getType : String;
    function getPath : string;
    function getWorkingDirectory : String;
    function isProjectAtRoot : boolean;
    function relativize(aPath : String) : String;
    function hasBranch(aBranch : String) : Boolean;
    function getShortHead(aPath : String) : string; overload;
    function getShortHead : string; overload;
    function isSubmodule(aBranch : String) : Boolean;
    function getAheadBehindCount(aReference, aPath : String) : NativeInt;
    function getCachedUpstreamAheadBehindCount(aPath : String) : TAtomAheadBehindCount;
    function getConfigValue(aKey,aPath : String) : String;
    function getOriginURL(aPath : String) : String; overload;
    function getOriginURL : String; overload;
    function getUpstreamBranch(aPath : String) : String; overload;
    function getUpstreamBranch : String; overload;
    function getReferences(aPath : String) : TAtomGitReference; overload;
    function getReferences : TAtomGitReference; overload;
    function getReferenceTarget(aReference : string) : string;
    function getReferenceTarget(aReference,aPath : string) : string;
    function isPathModified(aPath : String) : boolean;
    function isPathNew(aPath : String) : boolean;
    function isPathIgnored(aPath : String) : boolean;
    function getDirectoryStatus(aPath : String) : NativeInt;
    function getPathStatus(aPath : String) : NativeInt;
    function getCachedPathStatus(aPath : String) : JSValue;
    function isStatusModified(aStatus : NativeInt) : Boolean;
    function isStatusNew(aStatus : NativeInt) : Boolean;
    function getDiffStats(aPath : string) : TAtomDiffStats;
    function getLineDiffs(aPath : String; aText : String) : TAtomHunkArray;
    function checkoutHead(aPath : string) : Boolean;
    function checkoutReference(aReference : string; aCreate : boolean) : boolean;
  end;

  TAtomGitRepositoryArray = Array of TAtomGitRepository;

  TAtomPoint = class external name 'Point' (TJSObject)
  Public
     row : NativeInt;
     column : NativeInt;
     class function fromObject(aObj : TJSObject; copy : Boolean) : TAtomPoint; overload;
     class function fromObject(aObj : TJSObject) : TAtomPoint; overload;
     class function min(p1,p2 : TAtomPoint) : TAtomPoint; overload;
     constructor new(arow,acol : NativeInt);
     function copy : TAtomPoint;
     function negate : TAtomPoint;
     function compare (p2 : TAtomPoint) : integer; overload;
     function compare (p2 : Array of nativeInt) : integer; overload;
     function isEqual (p2 : TAtomPoint) : boolean; overload;
     function isEqual (p2 : Array of nativeInt) : integer; overload;
     function isLessThan (p2 : TAtomPoint) : boolean; overload;
     function isLessThan (p2 : Array of nativeInt) : integer; overload;
     function isLessThanOrEqual (p2 : TAtomPoint) : boolean; overload;
     function isLessThanOrEqual (p2 : Array of nativeInt) : integer; overload;
     function isGreaterThan (p2 : TAtomPoint) : boolean; overload;
     function isGreaterThan (p2 : Array of nativeInt) : integer; overload;
     function isGreaterThanOrEqual (p2 : TAtomPoint) : boolean; overload;
     function isGreaterThanOrEqual (p2 : Array of nativeInt) : integer; overload;
     function freeze : TAtomPoint;
     function translate (p2 : TAtomPoint) : TAtomPoint; overload;
     function traverse (p2 : TAtomPoint) : TAtomPoint; overload;
     function toArray : TNativeIntDynArray;
     function serialize : TNativeIntDynArray;
  end;
  TAtomPointArray = Array of TAtomPoint;

  TAtomRange = class external name 'Range' (TJSObject)
  Public
    start : TAtomPoint;
    end_ : TAtomPoint; external name 'end';
    class function fromObject(aObject : TJSObject; aCopy : Boolean) : TAtomRange;
    constructor new (a,b : TAtomPoint);
    constructor new (a,b : array of NativeInt);
    class function deserialize (aPoints : array of TAtomPoint) : TAtomRange; overload;
    class function deserialize (aPoints : array of TNativeIntDynArray) : TAtomRange; overload;
    function copy : TAtomRange;
    function negate : TAtomRange;
    function serialize : TJSObject;
    function isEmpty : boolean;
    function isSingleLine : boolean;
    function getRowCount : NativeInt;
    function getRows : TNativeIntDynArray;
    procedure freeze; reintroduce;
    function union(aOther : TAtomRange) : TAtomRange;
    function union(aOther : TJSValueDynArray) : TAtomRange;
    function translate(aStartDelta : TAtomPoint) : TAtomRange;
    function translate(aStartDelta,aEndDelta : TAtomPoint) : TAtomRange;
    function traverse(aDelta : TAtomPoint) : TAtomRange;
    function Compare(aOther : TAtomRange) : NativeInt;
    function isEqual(aOther : TAtomRange) : Boolean;
    function coversSameRows(aOther : TAtomRange) : Boolean;
    function intersectsWith(aOther : TAtomRange) : Boolean;
    function intersectsWith(aOther : TAtomRange; Exclusive : Boolean) : Boolean;
    function containsRange(aOther : TAtomRange) : Boolean;
    function containsRange(aOther : TAtomRange; Exclusive : Boolean) : Boolean;
    function containsPoint(aOther : TAtomPoint) : Boolean;
    function containsPoint(aOther : TAtomPoint; Exclusive : Boolean) : Boolean;
    function intersectsRowRange(aStartRow,aEndRow : NativeInt) : Boolean;
  end;

  TAtomDisplayMarkerChange = class external name 'Object' (TJSObject)
    oldHeadBufferPosition : TAtomPoint;
    newHeadBufferPosition : TAtomPoint;
    oldTailBufferPosition : TAtomPoint;
    newTailBufferPosition : TAtomPoint;
    oldScreenPosition : TAtomPoint;
    newScreenPosition : TAtomPoint;
    oldTailScreenPosition : TAtomPoint;
    newTailScreenPosition : TAtomPoint;
    wasValid : Boolean;
    isValid : boolean;
    hadTail : Boolean;
    hasTail : Boolean;
    oldProperties : TJSObject;
    newProperties : TJSObject;
    textChanged : Boolean;
  end;

  TAtomDisplayMarkerChangeHandler = reference to procedure(aChange : TAtomDisplayMarkerChange);

  TAtomDisplayMarkerPropertiesMatch = class external name 'Object' (TJSObject)
    startBufferPosition,
    endBufferPosition,
    startScreenPosition,
    endScreenPosition : TAtomPoint;
    startsInBufferRange,
    endsInBufferRange,
    startsInScreenRange,
    endsInScreenRange: TAtomRange;
    startBufferRow,
    endBufferRow,
    startScreenRow,
    endScreenRow : NativeInt;
    intersectsBufferRowRange,
    intersectsScreenRowRange : TNativeIntDynArray;
    containsBufferRange : TAtomRange;
    containsBufferPosition : TAtomPoint;
    containedInBufferRange : TAtomRange;
    containedInScreenRange : TAtomRange;
    intersectsBufferRange : TAtomRange;
    intersectsScreenRange : TAtomRange;
  end;

  TAtomMarkerLayer = class external name 'MarkerLayer' (TJSObject)
  Public
    function copy  : TAtomMarkerLayer;
    procedure destroy;
    procedure clear;
    function isDestroyed : Boolean;
    function getMarker(aID : JSValue) : TAtomMarker;
    function getMarkers : TAtomMarkerArray;
    function getMarkerCount : NativeInt;
    function getRole : string;
    function markRange(aRange : TAtomRange) : TAtomMarker; overload;
    function markRange(aRange : TAtomRangeArray) : TAtomMarker; overload;
    function markRange(aRange : TAtomRange; Options : TAtomMarkRangeOptions ) : TAtomMarker; overload;
    function markRange(aRange : TAtomRangeArray; Options : TAtomMarkRangeOptions) : TAtomMarker; overload;
    function markPosition(aPosition : TAtomPoint) : TAtomMarker; overload;
    function markPosition(aPosition : array of nativeInt) : TAtomMarker; overload;
    function markPosition(aPosition : TAtomPoint; Options : TAtomMarkRangeOptions) : TAtomMarker; overload;
    function markPosition(aPosition : array of NativeInt; Options : TAtomMarkRangeOptions) : TAtomMarker; overload;
    function onDidUpdate(aHandler : TAtomHandler) : TAtomDisposable;
    function onDidCreateMarker(aHandler : TAtomMarkerHandler) : TAtomDisposable;
    function onDidDestroy(aHandler : TAtomHandler) : TAtomDisposable;
  end;


  TAtomDisplayMarker = class external name 'DisplayMarker' (TJSObject)
  Public
    procedure destroy;
    function copy(aObject : TJSObject) : TAtomDisplayMarker;
    function onDidChange(aHandler : TAtomDisplayMarkerChangeHandler) : TAtomDisposable;
    function onDidDestroy(aHandler : TAtomHandler) : TAtomDisposable;
    function isValid : Boolean;
    function isDestroyed : Boolean;
    function isExclusive : Boolean;
    function getInvalidationStrategy : string;
    function getProperties : TJSObject;
    procedure setProperties(aProps : TJSObject);
    function matchesProperties(aProps : TAtomMarkerPropertiesMatch) : boolean;
    function matchesProperties(aProps : TJSObject) : boolean;
    function compare (aMarker : TAtomDisplayMarker) : NativeInt;
    function isEqual (aMarker : TAtomDisplayMarker) : Boolean;
    function getBufferRange : TAtomRange;
    function getScreenRange : TAtomRange;
    procedure setBufferRange(aRange : TAtomRange);
    procedure setScreenRange(aRange : TAtomRange);
    procedure setBufferRange(aRange : TAtomRange; aOptions : TAtomMarkerRangeOptions);
    procedure setScreenRange(aRange : TAtomRange; aOptions : TAtomMarkerRangeOptions);
    function getStartScreenPosition : TAtomPoint;
    function getStartScreenPosition(aOptions : TAtomMarkerRangeOptions) : TAtomPoint;
    function getEndScreenPosition : TAtomPoint;
    function getEndScreenPosition(aOptions : TAtomMarkerRangeOptions) : TAtomPoint;
    function getHeadBufferPosition : TAtomPoint;
    procedure setHeadBufferPosition (aValue : TAtomPoint);
    function getHeadScreenPosition : TAtomPoint;
    function getHeadScreenPosition(aOptions : TAtomMarkerRangeOptions) : TAtomPoint;
    procedure setHeadScreenPosition(aPoint: TAtomPoint;aOptions : TAtomMarkerRangeOptions);
    procedure setHeadScreenPosition(aPoint: TAtomPoint);
    function getTailBufferPosition : TAtomPoint;
    procedure setTailBufferPosition(aValue : TAtomPoint);
    procedure setTailScreenPosition(aPoint: TAtomPoint;aOptions : TAtomMarkerRangeOptions);
    procedure setTailScreenPosition(aPoint: TAtomPoint);
    function getStartBufferPosition : TAtomPoint;
    function getEndBufferPosition : TAtomPoint;
    function hasTail :  Boolean;
    procedure plantTail;
    procedure clearTail;
  end;
  TAtomDisplayMarkerHandler = reference to procedure (aMarker : TAtomDisplayMarker);

  TAtomMarkerOptions = class external name 'Object' (TJSObject)
    reversed : Boolean;
    invalidate : string;
    exclusive : boolean;
    clipDirection : string;
  end;

  TAtomLayerFindMarkerOptions = class external name 'Object' (TJSObject)
    startBufferPosition,
    endBufferPosition,
    startScreenPosition,
    endScreenPosition,
    containsBufferPosition: TAtomPoint;
    startsInRange,
    endsInRange,
    containsRange,
    intersectsBufferRange,
    intersectsScreenRange: TAtomRange;
    startBufferRow,
    endBufferRow,
    startScreenRow,
    endScreenRow : NativeInt;
    startsInBufferRange,
    endsInBufferRange,
    startsInScreenRange,
    endsInScreenRange,
    containedInBufferRange,
    containedInScreenRange,
    containsBufferRange : TAtomRange;
    intersectsBufferRowRange,
    intersectsScreenRowRange : Array of NativeInt;
  end;

  TAtomDisplayMarkerLayer = class external name 'DisplayMarkerLayer' (TJSObject)
  Public
    procedure destroy;
    procedure clear;
    function isDestroyed : Boolean;
    function onDidDestroy(aHandler : TAtomHandler) : TAtomDisposable;
    function onDidUpdate(aHandler : TAtomHandler) : TAtomDisposable;
    function onDidCreateMarker(aHandler : TAtomDisplayMarkerHandler) : TAtomDisposable;
    function markScreenRange(aRange : TAtomRange; aOptions : TAtomMarkerOptions) : TAtomDisplayMarker;
    function markScreenRange(aRange : TAtomRangeArray; aOptions : TAtomMarkerOptions) : TAtomDisplayMarker;
    function markScreenPosition(aRange : TAtomPoint; aOptions : TAtomMarkerOptions) : TAtomDisplayMarker;
    function markScreenPosition(aRange : Array of NativeInt; aOptions : TAtomMarkerOptions) : TAtomDisplayMarker;
    function markScreenPosition(aRange : TAtomPoint) : TAtomDisplayMarker;
    function markScreenPosition(aRange : Array of NativeInt) : TAtomDisplayMarker;
    function markBufferRange(aRange : TAtomRange; aOptions : TAtomMarkerOptions) : TAtomDisplayMarker;
    function markBufferRange(aRange : TAtomRangeArray; aOptions : TAtomMarkerOptions) : TAtomDisplayMarker;
    function markBufferPosition(aRange : TAtomPoint) : TAtomDisplayMarker;
    function markBufferPosition(aRange : Array of NativeInt) : TAtomDisplayMarker;
    function getMarker (aID : JSValue): TAtomDisplayMarker;
    function getMarkers : TAtomDisplayMarkerArray;
    function getMarkerCount : NativeInt;
    function findMarkers(aOptions :TAtomLayerFindMarkerOptions) : TAtomDisplayMarkerArray;
  end;


  TAtomPropertyChangeEvent = class external name 'Object' (TJSObject)
    oldProperties : TJSObject;
    newProperties : TJSObject;
  end;

  TAtomPropertyChangeHandler = reference to procedure(aEvent: TAtomPropertyChangeEvent);

  TAtomDecoration = Class external name 'Decoration' (TJSObject)
  Public
    procedure destroy;
    function onDidChangeProperties(aHandler : TAtomPropertyChangeHandler) : TAtomDisposable;
    function onDidDestroy(aHandler : TAtomHandler) : TAtomDisposable;
    function getID : JSValue;
    function getMarker : TAtomDisplayMarker;
    function isType(aType : string) : Boolean;
    function getProperties : TJSObject;
    procedure setProperties(aValue : TJSObject);
  end;
  TAtomDecorationHandler = reference to procedure(aDecoration : TAtomDecoration);
  TAtomDecorationArray = Array of TAtomDecoration;

  TAtomCursorPosChange = class external name 'Object' (TJSObject)
    oldBufferPosition,
    oldScreenPosition,
    newBufferPosition,
    newScreenPosition : TAtomPoint;
    textChanged : Boolean;
    cursor : TAtomCursor;
  end;
  TAtomCursorPosChangeHandler = reference to procedure (aChange :TAtomCursorPosChange);

  TAtomPosOptions = Class external name 'Object' (TJSObject)
    autoscroll : boolean;
  end;

  TAtomWordOptions = Class external name 'Object' (TJSObject)
    wordRegex : TJSRegexp;
  end;

  TAtomWordRangeOptions = class external name 'Object' (TAtomWordOptions)
    includeNonWordCharacters : Boolean;
    allowPrevious : Boolean;
  end;

  TAtomLineRangeOptions = class external name 'Object' (TAtomWordOptions)
    includeNewline : Boolean;
  end;

  TAtomWordRegexpOptions = class external name 'Object' (TAtomWordOptions)
    includeNonWordCharacters : Boolean;
  end;

  TAtomSubWordRegexpOptions = class external name 'Object' (TAtomWordOptions)
    backwards : Boolean;
  end;

  TAtomMoveOptions = Class external name 'Object' (TJSObject)
    moveToEndOfSelection : Boolean;
  end;

  TAtomCursor = class external name 'Cursor' (TJSObject)
  Public
    function OnDidChangePosition(aHandler : TAtomCursorPosChangeHandler) : TAtomDisposable;
    function onDidDestroy(aHandler : TAtomHandler) : TAtomDisposable;
    procedure setScreenPosition(aPosition : Array of NativeInt; Options : TAtomPosOptions);
    function getScreenPosition : TAtomPoint;
    procedure setBufferPosition(aPosition : Array of NativeInt; Options : TAtomPosOptions);
    function getBufferPosition : TNativeIntDynArray;
    function getScreenRow : NativeInt;
    function getScreenColumn : NativeInt;
    function getBufferRow : NativeInt;
    function getBufferColumn : NativeInt;
    function getCurrentBufferLine : string;
    function isAtBeginningOfLine : Boolean;
    function isAtEndOfLine : Boolean;
    function getMarker : TAtomDisplayMarker;
    function isSurroundedByWhitespace : boolean;
    function isBetweenWordAndNonWord : Boolean;
    function isInsideWord : Boolean;
    function isInsideWord(aOptions : TAtomWordOptions) : Boolean;
    function getIndentLevel : NativeInt;
    function getScopeDescriptor : TAtomScopeDescriptor;
    function getSyntaxTreeScopeDescriptor : TAtomScopeDescriptor;
    function hasPrecedingCharactersOnLine : boolean;
    function isLastCursor : boolean;
    procedure moveUp; overload;
    procedure moveUp(aCount : NativeInt); overload;
    procedure moveUp(aCount : NativeInt; aOptions : TAtomMoveOptions); overload;
    procedure moveDown; overload;
    procedure moveDown(aCount : NativeInt); overload;
    procedure moveDown(aCount : NativeInt; aOptions : TAtomMoveOptions); overload;
    procedure moveLeft; overload;
    procedure moveLeft(aCount : NativeInt); overload;
    procedure moveLeft(aCount : NativeInt; aOptions : TAtomMoveOptions); overload;
    procedure moveRight; overload;
    procedure moveRight(aCount : NativeInt); overload;
    procedure moveRight(aCount : NativeInt; aOptions : TAtomMoveOptions); overload;
    procedure moveToTop;
    procedure moveToBottom;
    procedure moveToBeginningOfScreenLine;
    procedure moveToBeginningOfLine;
    procedure moveToFirstCharacterOfLine;
    procedure moveToEndOfScreenLine;
    procedure moveToEndOfLine;
    procedure moveToBeginningOfWord;
    procedure moveToEndOfWord;
    procedure moveToBeginningOfNextWord;
    procedure moveToPreviousWordBoundary;
    procedure moveToNextWordBoundary;
    procedure moveToNextSubwordBoundary;
    procedure moveToPreviousSubwordBoundary;
    procedure skipLeadingWhitespace;
    procedure moveToBeginningOfNextParagraph;
    procedure moveToBeginningOfPreviousParagraph;
    function getPreviousWordBoundaryBufferPosition : TAtomPoint; overload;
    function getPreviousWordBoundaryBufferPosition(aoptions : TAtomWordOptions) : TAtomPoint; overload;
    function getNextWordBoundaryBufferPosition : TAtomPoint; overload;
    function getNextWordBoundaryBufferPosition(aoptions : TAtomWordOptions) : TAtomPoint; overload;
    function getBeginningOfCurrentWordBufferPosition(aOptions : TAtomWordRangeOptions) : TAtomRange;
    function getEndOfCurrentWordBufferPosition(aOptions : TAtomWordRangeOptions) : TAtomRange;
    function getBeginningOfNextWordBufferPosition(aOptions : TAtomWordOptions) : TAtomRange;
    function getCurrentWordBufferRange(aOptions : TAtomWordOptions) : TAtomRange;
    function getCurrentLineBufferRange(aOptions : TAtomLineRangeOptions) : TAtomRange;
    function getCurrentParagraphBufferRange : TAtomRange;
    function getCurrentWordPrefix : String;
    function Compare(aCursor : TAtomCursor) : NativeInt;
    procedure ClearSelection;
    function wordRegExp(options : TAtomWordRegexpOptions) : TJSRegexp;
    function subwordRegExp(options : TAtomSubWordregexpOptions) : TJSRegexp;
  end;
  TAtomCursorHandler = reference to procedure(aCursor : TAtomCursor);
  TAtomCursorArray = array of TAtomCursor;

  TAtomGutterHandler = reference to procedure (aGutter : TAtomGutter);
  TAtomGutter = Class external name 'Gutter' (TJSObject)
  Public
    procedure destroy;
    function onDidDestroy(callback : TAtomHandler) : TAtomDisposable;
    function onDidChangeVisible(callback : TAtomGutterHandler) : TAtomDisposable;
    procedure hide;
    procedure show;
    function isVisible : Boolean;
    function decorateMarker(aMarker : TAtomDisplayMarker; aProperties : TJSObject) : TAtomDecoration;
  end;
  TAtomGutterArray = array of TAtomGutter;

  TAtomGrammarRule = class external name 'Object' (TJSObject)
  // No docs ?
  end;
  TAtomGrammarRuleDynArray = array of TAtomGrammarRule;


  TAtomToken = class external name 'Object' (TJSObject)
    value : JSValue;
    scopes : JSValue;
  end;
  TAtomTokenDynArray = array of TAtomToken;

  TAtomTokenizeResult = class external name 'Object' (TJSObject)
    line : string;
    tags : TJSValueDynArray;
    tokens : TAtomTokenDynArray;
    ruleStack : TAtomGrammarRuleDynArray;
  end;

  TAtomGrammar = class external name 'Grammar' (TJSObject)
  Public
    Function onDidUpdate(aHandler : TAtomHandler) : TAtomDisposable;
    function tokenizeLines(aLines : String) : TAtomTokenDynArray;
    function tokenizeLine(aLine : String; aRuleStack : TAtomGrammarRuleDynArray; aFirstLine : Boolean) : TAtomTokenizeResult;
    function tokenizeLine(aLine : String; aRuleStack : TAtomGrammarRuleDynArray) : TAtomTokenizeResult;
    function tokenizeLine(aLine : String; aFirstLine : Boolean) : TAtomTokenizeResult;
    function tokenizeLine(aLine : String) : TAtomTokenizeResult;
  end;
  TAtomGrammarDynArray = Array of TAtomGrammar;
  TAtomGrammarHandler = reference to procedure(aGrammar : TAtomGrammar);


  TAtomTextChangeEvent = class external name 'Object' (TJSObject)
    text : string;
    procedure cancel;
  end;

  TAtomTextChangedEvent = class external name 'Object' (TJSObject)
    text : string;
  end;

  TAtomInsertTextHandler = reference to procedure(aEvent : TAtomTextChangeEvent);
  TAtomTextChangedHandler = reference to procedure(aEvent : TAtomTextChangedEvent);

  TAtomSelectionRangeChangeEvent = class external name 'Object' (TJSObject)
    oldBufferRange : TAtomRange;
    newBufferRange : TAtomRange;
    oldScreenRange : TAtomRange;
    newScreenRange : TAtomRange;
    selection : TAtomSelection;
  end;
  TAtomSelectionRangeChangeHandler = reference to procedure(aEvent :TAtomSelectionRangeChangeEvent);

  TAtomSelectionOptions  = class external name 'Object' (TJSObject)
    bypassReadOnly : Boolean;
  end;

  TAtomMutateSelectionHandler = reference to procedure (aSelection : TAtomSelection; aNumber : NativeInt);

  TAtomClipOptions = class external name 'Object' (TJSObject)
    clipDirection : String;
  end;

  TAtomMarkerDecorations = class external name 'Object' (TJSObject)
    type_ : String;  external name 'type';
    class_ : String;  external name 'class';
    style : TJSObject;
    item : TJSHTMLElement;
    itemObj : TJSObject; external name 'item';
    onlyHead : Boolean;
    onlyEmpty : Boolean;
    onlyNonEmpty : Boolean;
    onlyEmptyLastRow : Boolean;
    position : string;
    order : string;
    avoidOverflow : boolean;
  end;

  TAtomTextFindMarkerOptions = class external name 'Object' (TJSObject)
    containsBufferPosition: TAtomPoint;
    containsBufferRange : TAtomRange;
    startBufferRow,
    endBufferRow : NativeInt;
  end;

  TAtomSetRangeOptions = class external name 'Object' (TJSObject)
    reversed : boolean;
    preserveFolds : boolean;
    autoscroll : boolean;
  end;

  TAtomIndentationOptions = class external name 'Object' (TJSObject)
    PreserveLeadingWhiteSpace : boolean;
  end;

  TAtomScrollOptions = class external name 'Object' (TJSObject)
    center : boolean;
  end;

  TAtomPathSearchedHandler = reference to procedure (aProgress : integer);
  TAtomScanOptions = class external name 'Object' (TJSObject)
    paths : TStringDynArray;
    onPathsSearched : TAtomPathSearchedHandler;
    leadingContextLineCount : NativeInt;
    trailingContextLineCount : NativeInt;
  end;

  TAtomScanHandler = reference to procedure(aValue : JSValue);

  TAtomGutterLinePos= class external name 'Object' (TJSObject)
    bufferRow : NativeInt;
    screenRow : NativeInt;
  end;

  TAtomGutterLineLabel = class external name 'Object' (TAtomGutterLinePos)
    foldable : boolean;
    softWrapped : boolean;
    maxDigits : NativeInt;
  end;

  TAtomGutterLabelFunction = reference to function(aLineObject : TAtomGutterLineLabel) : String;
  TAtomGutterMouseMoveFunction = reference to function(aLineObject : TAtomGutterLinePos) : String;

  TAtomGutterOptions = class external name 'Object' (TJSObject)
    name : string;
    priority : NativeInt;
    visible : boolean;
    type_: string; external name 'type';
    class_ : string; external name 'class';
    labelFn : TAtomGutterLabelFunction;
    onMouseMove : TAtomGutterMouseMoveFunction;
  end;

  TAtomTextEditor = class external name 'TextEditor' (TJSObject)
  Public
    function onDidChangeTitle(aHandler : TAtomHandler) : TAtomDisposable;
    function onDidChangePath(aHandler : TAtomHandler) : TAtomDisposable;
    function onDidChange(aHandler : TAtomHandler) : TAtomDisposable;
    function onDidStopChanging(aHandler : TAtomHandler) : TAtomDisposable;
    function onDidChangeCursorPosition(aHandler : TAtomCursorPosChangeHandler) : TAtomDisposable;
    function onDidChangeSelectionRange(aHandler : TAtomSelectionRangeChangeHandler) : TAtomDisposable;
    function onDidSave(aHandler : TAtomSaveHandler) : TAtomDisposable;
    function onDidDestroy(aHandler : TAtomHandler) : TAtomDisposable;
    function observeGutters(aHandler : TAtomGutterHandler) : TAtomDisposable;
    function onDidAddGutter(aHandler : TAtomGutterHandler) : TAtomDisposable;
    function onDidRemoveGutter(aHandler : TAtomStringHandler) : TAtomDisposable;
    function onDidChangeSoftwrapped(aHandler : TAtomHandler) : TAtomDisposable;
    function onDidChangeEncoding(aHandler : TAtomHandler) : TAtomDisposable;
    function observeGrammar(aHandler : TAtomGrammarHandler) : TAtomDisposable;
    function onDidChangeGrammar(aHandler : TAtomGrammarHandler) : TAtomDisposable;
    function onDidChangeModified(aHandler : TAtomHandler) : TAtomDisposable;
    function onDidConflict(aHandler : TAtomHandler) : TAtomDisposable;
    function onWillInsertText(aHandler : TAtomInsertTextHandler) : TAtomDisposable;
    function onDidInsertText(aHandler : TAtomTextChangedHandler) : TAtomDisposable;
    function observeCursors(aHandler : TAtomCursorHandler) : TAtomDisposable;
    function onDidAddCursor(aHandler : TAtomCursorHandler) : TAtomDisposable;
    function onDidRemoveCursor(aHandler : TAtomCursorHandler) : TAtomDisposable;
    function observeSelections(aHandler : TAtomSelectionHandler) : TAtomDisposable;
    function onDidAddSelection(aHandler : TAtomSelectionHandler) : TAtomDisposable;
    function onDidRemoveSelection(aHandler : TAtomSelectionHandler) : TAtomDisposable;
    function observeDecorations(aHandler : TAtomDecorationHandler) : TAtomDisposable;
    function onDidAddDecoration(aHandler : TAtomDecorationHandler) : TAtomDisposable;
    function onDidRemoveDecoration(aHandler : TAtomDecorationHandler) : TAtomDisposable;
    function onDidChangePlaceHolderText(aHandler : TAtomStringHandler) : TAtomDisposable;
    // Buffer
    function getBuffer : TAtomTextBuffer;
    // File details
    function getTitle : string;
    function getLongTitle : string;
    function getPath : string;
    function isModified : Boolean;
    function isEmpty : Boolean;
    function getEncoding : string;
    procedure setEncoding(aEncoding : string);
    // File operations
    function save : TJSPromise;
    function saveAs(aPath : String) : TJSPromise;
    // Reading text
    function getText : string;
    function getTextInBufferRange(aRange : TAtomRange) : string;
    function getTextInBufferRange(aRange : TAtomRangeArray) : string;
    function getLineCount : NativeInt;
    function getScreenLineCount : NativeInt;
    function getLastBufferRow : NativeInt;
    function getLastScreenRow : NativeInt;
    function lineTextForBufferRow(aRow : NativeInt) : string;
    function lineTextForScreenRow(aRow : NativeInt) : string;
    function getCurrentParagraphBufferRange : TAtomRange;
    // Mutating text
    procedure setText(aText : String; aOptions : TAtomSelectionOptions);
    procedure setText(aText : String);
    function setTextInBufferRange(aRange : TAtomRange; aText : String) : TAtomRange;overload;
    function setTextInBufferRange(aRange : TAtomRange; aText : String; aOptions : TAtomSetTextOptions) : TAtomRange; overload;
    function insertText(aText : String; aOptions : TAtomSelectionOptions) : JSValue; overload;
    function insertText(aText : String) : JSValue; overload;
    procedure insertNewline(aOptions : TAtomSelectionOptions); overload;
    procedure insertNewline; overload;
    procedure backspace(aOptions : TAtomSelectionOptions); overload;
    procedure backspace; overload;
    procedure mutateSelectedText(aHandler: TAtomMutateSelectionHandler);
    procedure transpose(aOptions : TAtomSelectionOptions); overload;
    procedure transpose; overload;
    procedure upperCase; overload;
    procedure upperCase(aOptions : TAtomSelectionOptions); overload;
    procedure lowerCase; overload;
    procedure lowerCase(aOptions : TAtomSelectionOptions); overload;
    procedure toggleLineCommentsInSelection; overload;
    procedure toggleLineCommentsInSelection(aOptions : TAtomSelectionOptions); overload;
    procedure insertNewlineBelow; overload;
    procedure insertNewlineBelow(aOptions : TAtomSelectionOptions); overload;
    procedure insertNewlineAbove; overload;
    procedure insertNewlineAbove(aOptions : TAtomSelectionOptions); overload;
    procedure deleteToBeginningOfWord; overload;
    procedure deleteToBeginningOfWord(aOptions : TAtomSelectionOptions); overload;
    procedure deleteToPreviousWordBoundary; overload;
    procedure deleteToPreviousWordBoundary(aOptions : TAtomSelectionOptions); overload;
    procedure deleteToNextWordBoundary; overload;
    procedure deleteToNextWordBoundary(aOptions : TAtomSelectionOptions); overload;
    procedure deleteToBeginningOfSubword; overload;
    procedure deleteToBeginningOfSubword(aOptions : TAtomSelectionOptions); overload;
    procedure deleteToEndOfSubword; overload;
    procedure deleteToEndOfSubword(aOptions : TAtomSelectionOptions); overload;
    procedure deleteToBeginningOfLine; overload;
    procedure deleteToBeginningOfLine(aOptions : TAtomSelectionOptions); overload;
    procedure deleteToEndOfLine; overload;
    procedure deleteToEndOfLine(aOptions : TAtomSelectionOptions); overload;
    procedure deleteToEndOfWord; overload;
    procedure deleteToEndOfWord(aOptions : TAtomSelectionOptions); overload;
    procedure deleteLine; overload;
    procedure deleteLine(aOptions : TAtomSelectionOptions); overload;
    function undo(aOptions : TAtomUndoRedoOptions) : Boolean; overload;
    function undo : Boolean; overload;
    function redo(aOptions : TAtomUndoRedoOptions) : Boolean; overload;
    function redo : Boolean;overload;
    procedure transact(options : TAtomTransactOptions; groupingInterval : Integer; aFunction : TAtomHandler);
    procedure transact(groupingInterval : Integer; aFunction : TAtomHandler);
    procedure transact(aFunction : TAtomHandler);
    procedure abortTransaction;
    function createCheckpoint(aOptions : TAtomUndoRedoOptions) : JSValue; overload;
    function createCheckpoint : JSValue; overload;
    procedure revertToCheckpoint(aID : JSValue; aOptions : TAtomUndoRedoOptions); overload;
    procedure revertToCheckpoint(aID : JSValue); overload;
    function groupChangesSinceCheckpoint(aID : JSValue; aOptions : TAtomUndoRedoOptions) : Boolean; overload;
    function groupChangesSinceCheckpoint(aID : JSValue) : Boolean; overload;
    function screenPositionForBufferPosition(aPoint: TAtomPoint; aOptions : TAtomClipOptions) : TAtomPoint; overload;
    function screenPositionForBufferPosition(aPoint: TAtomPoint) : TAtomPoint; overload;
    function bufferPositionForScreenPosition(aPoint: TAtomPoint; aOptions : TAtomClipOptions) : TAtomPoint; overload;
    function bufferPositionForScreenPosition(aPoint: TAtomPoint) : TAtomPoint; overload;
    function screenPositionForBufferPosition(aPoint: array of NativeInt; aOptions : TAtomClipOptions) : TAtomPoint; overload;
    function screenPositionForBufferPosition(aPoint: array of NativeInt) : TAtomPoint; overload;
    function bufferPositionForScreenPosition(aPoint: array of NativeInt; aOptions : TAtomClipOptions) : TAtomPoint; overload;
    function bufferPositionForScreenPosition(aPoint: array of NativeInt) : TAtomPoint; overload;
    function screenRangeForBufferRange(aRange : TAtomRange) : TAtomRange;
    function sufferRangeForScreenRange(aRange : TAtomRange) : TAtomRange;
    function clipScreenPosition(aPoint : TAtomPoint; aOptions : TAtomClipOptions) : TAtomPoint; overload;
    function clipScreenPosition(aPoint : TAtomPoint) : TAtomPoint;overload;
    function clipScreenRange(aPoint : TAtomRange; aOptions : TAtomClipOptions) : TAtomRange; overload;
    function clipScreenRange(aPoint : TAtomRange) : TAtomRange;overload;
    function decorateMarker(aMarker : TAtomDisplayMarker; decorationParams: TAtomMarkerDecorations) : TAtomDecoration;
    function decorateMarkerLayer(aMarker : TAtomDisplayMarkerLayer; decorationParams: TAtomMarkerDecorations) : TAtomLayerDecoration;
    function decorateMarkerLayer(aMarker : TAtomMarkerLayer; decorationParams: TAtomMarkerDecorations) : TAtomLayerDecoration;
    // Decorations
    function getDecorations(aFilter : TJSObject) : TAtomDecorationArray; overload;
    function getDecorations : TAtomDecorationArray; overload;
    function getLineDecorations(aFilter : TJSObject) : TAtomDecorationArray; overload;
    function getLineDecorations : TAtomDecorationArray; overload;
    function getLineNumberDecorations(aFilter : TJSObject) : TAtomDecorationArray; overload;
    function getLineNumberDecorations : TAtomDecorationArray; overload;
    function getHighlightDecorations(aFilter : TJSObject) : TAtomDecorationArray; overload;
    function getHighlightDecorations : TAtomDecorationArray; overload;
    function getOverlayDecorations(aFilter : TJSObject) : TAtomDecorationArray; overload;
    function getOverlayDecorations : TAtomDecorationArray; overload;
    // Markers
    function markBufferRange(aRange : TAtomRange; aOptions : TAtomMarkerOptions) : TAtomDisplayMarker; overload;
    function markBufferRange(aRange : TAtomRangeArray; aOptions : TAtomMarkerOptions) : TAtomDisplayMarker;  overload;
    function markBufferPosition(aRange : TAtomPoint) : TAtomDisplayMarker;  overload;
    function markBufferPosition(aRange : Array of NativeInt) : TAtomDisplayMarker; overload;
    function markScreenRange(aRange : TAtomRange; aOptions : TAtomMarkerOptions) : TAtomDisplayMarker; overload;
    function markSreenRange(aRange : TAtomRangeArray; aOptions : TAtomMarkerOptions) : TAtomDisplayMarker;  overload;
    function markScreenPosition(aRange : TAtomPoint) : TAtomDisplayMarker; overload;
    function markScreenPosition(aRange : Array of NativeInt) : TAtomDisplayMarker; overload;
    function findMarkers(aOptions : TAtomTextFindMarkerOptions) : TAtomDisplayMarkerArray;
    function addMarkerLayer(options : TAtomMarkerLayerOptions) : TAtomDisplayMarkerLayer;
    function getMarkerLayer(aID : JSValue) :  TAtomDisplayMarkerLayer;
    function getDefaultMarkerLayer : TAtomDisplayMarkerLayer;
    function getMarker (aID : JSValue): TAtomDisplayMarker;
    function getMarkers : TAtomDisplayMarkerArray;
    function getMarkerCount : NativeInt;
    // Cursors
    function getCursorBufferPosition : TAtomPoint;
    Function getCursorBufferPositions : TAtomPointArray;
    procedure setCursorBufferPosition(position : TAtomPoint; options : TAtomPosOptions);  overload;
    procedure setCursorBufferPosition(position : TAtomPoint);  overload;
    Function getCursorAtScreenPosition(position : TAtomPoint) : TAtomCursor;
    Function getCursorScreenPosition : TAtomPoint;
    Function getCursorScreenPositions : TAtomPointArray;
    Procedure setCursorScreenPosition(position : TAtomPoint; options : TAtomPosOptions); overload;
    Procedure setCursorScreenPosition(position : TAtomPoint); overload;
    Function addCursorAtBufferPosition(bufferPosition : TAtomPoint) : TAtomCursor; overload;
    Function addCursorAtScreenPosition(screenPosition : TAtomPoint) : TAtomCursor;overload;
    Function addCursorAtBufferPosition(bufferPosition : Array of NativeInt) : TAtomCursor; overload;
    Function addCursorAtScreenPosition(screenPosition :  Array of NativeInt) : TAtomCursor; overload;
    function hasMultipleCursors : boolean;
    Procedure moveUp(lineCount : NativeInt);
    Procedure moveDown(lineCount: NativeInt);
    Procedure moveLeft(columnCount: NativeInt);
    Procedure moveRight(columnCount : NativeInt);
    Procedure moveToBeginningOfLine;
    Procedure moveToBeginningOfScreenLine;
    Procedure moveToFirstCharacterOfLine;
    Procedure moveToEndOfLine;
    Procedure moveToEndOfScreenLine;
    Procedure moveToBeginningOfWord;
    Procedure moveToEndOfWord;
    // Extended Methods
    Procedure moveToTop;
    Procedure moveToBottom;
    Procedure moveToBeginningOfNextWord;
    Procedure moveToPreviousWordBoundary;
    Procedure moveToNextWordBoundary;
    Procedure moveToPreviousSubwordBoundary;
    Procedure moveToNextSubwordBoundary;
    Procedure moveToBeginningOfNextParagraph;
    Procedure moveToBeginningOfPreviousParagraph;
    Function getLastCursor : TAtomCursor;
    Function getWordUnderCursor(options : TAtomWordRangeOptions) : String;
    Function getCursors : TAtomCursorArray;
    Function getCursorsOrderedByBufferPosition: TAtomCursorArray;
    // Selection
    Function getSelectedText() : string;
    Function getSelectedBufferRange() : TAtomRange;
    Function getSelectedBufferRanges() : TAtomRangeDynArray;
    Procedure setSelectedBufferRange(bufferRange : TAtomRange; options : TAtomSetRangeOptions);
    Procedure setSelectedBufferRanges(bufferRanges : Array of TAtomRange; options : TAtomSetRangeOptions);
    Procedure setSelectedBufferRange(bufferRange : TAtomRange);
    Procedure setSelectedBufferRanges(bufferRanges : Array of TAtomRange);
    Function getSelectedScreenRange() : TAtomRange;
    Function getSelectedScreenRanges() : TAtomRangeDynArray;
    Procedure setSelectedScreenRange(screenRange : TAtomRange; options : TAtomSetRangeOptions);  overload;
    Procedure setSelectedScreenRanges(screenRanges : Array of TAtomRange);  overload;
    Procedure setSelectedScreenRange(screenRange : TAtomRangeArray; options : TAtomSetRangeOptions);  overload;
    Procedure setSelectedScreenRanges(screenRanges : Array of TAtomRangeArray); overload;
    function addSelectionForBufferRange(bufferRange : TAtomRange; options:  TAtomSetRangeOptions) : TAtomSelection;
    function addSelectionForScreenRange(screenRange : TAtomRange; options : TAtomSetRangeOptions) : TAtomSelection;
    Procedure selectToBufferPosition(position : TAtomPoint);
    Procedure selectToScreenPosition(position : TAtomPoint);
    Procedure selectUp(rowCount : nativeint); overload;
    Procedure selectDown(rowCount : nativeint);  overload;
    Procedure selectLeft(columnCount : nativeint); overload;
    Procedure selectRight(columnCount : nativeint); overload;
    Procedure selectUp();  overload;
    Procedure selectDown(); overload;
    Procedure selectLeft(); overload;
    Procedure selectRight(); overload;

    Procedure selectToTop();
    Procedure selectToBottom();
    Procedure selectAll();
    Procedure selectToBeginningOfLine();
    Procedure selectToFirstCharacterOfLine();
    Procedure selectToEndOfLine();
    Procedure selectToBeginningOfWord();
    Procedure selectToEndOfWord();
    Procedure selectLinesContainingCursors();
    Procedure selectWordsContainingCursors();
    //Extended Methods
    Procedure selectToPreviousSubwordBoundary();
    Procedure selectToNextSubwordBoundary();
    Procedure selectToPreviousWordBoundary();
    Procedure selectToNextWordBoundary();
    Procedure selectToBeginningOfNextWord();
    Procedure selectToBeginningOfNextParagraph();
    Procedure selectToBeginningOfPreviousParagraph();
    Procedure selectLargerSyntaxNode();
    Procedure selectSmallerSyntaxNode();
    Procedure selectMarker(marker : TAtomDisplayMarker);
    Function getLastSelection() : TAtomSelection;
    Function getSelections() : TAtomSelectionArray;
    Function getSelectionsOrderedByBufferPosition : TAtomSelectionArray;
    function  selectionIntersectsBufferRange(bufferRange : TAtomRange) : Boolean;
    // Searching and Replacing
    function scan (aRegex : TJSRegexp; aOptions : TAtomScanOptions; aHandler : TAtomScanHandler) : TJSPromise;
    function scan (aRegex : TJSRegexp; aHandler : TAtomScanHandler) : TJSPromise;
    function scanInBufferRange (aRegex : TJSRegexp; aRange : TAtomRange;aHandler : TAtomScanHandler) : TJSPromise;
    function bakwardsScanInBufferRange (aRegex : TJSRegexp; aRange : TAtomRange;aHandler : TAtomScanHandler) : TJSPromise;

    // Tab Behavior
    Function getSoftTabs() :Boolean;
    Procedure setSoftTabs(softTabs : Boolean);
    Procedure toggleSoftTabs();
    Function getTabLength() : NativeInt;
    Procedure setTabLength(tabLength :NativeInt);
    // Extended Methods
    function usesSoftTabs() : Boolean;
    Function getTabText() : string;
    // Soft Wrap Behavior
    Function isSoftWrapped() : boolean;
    Procedure setSoftWrapped(softWrapped : boolean);
    Procedure toggleSoftWrapped();
    Function getSoftWrapColumn() : Boolean;
    // Indentation
    function indentationForBufferRow(bufferRow : NativeInt) : NativeInt;
    procedure setIndentationForBufferRow(bufferRow : NativeInt; newLevel : NativeInt; options : TAtomIndentationOptions);
    procedure setIndentationForBufferRow(bufferRow : NativeInt; newLevel : NativeInt);
    // Extended Methods
    Procedure indentSelectedRows(options : TAtomSelectionOptions);
    Procedure outdentSelectedRows(options: TAtomSelectionOptions);
    function indentLevelForLine(line : NativeInt) : String;
    Procedure autoIndentSelectedRows(options: TAtomSelectionOptions);
    // Grammars
    Function getGrammar() : TAtomGrammar;
    // Managing Syntax Scopes
    Function getRootScopeDescriptor() : TAtomScopeDescriptor;
    Function scopeDescriptorForBufferPosition(bufferPosition : TAtomPoint) : TAtomScopeDescriptor;
    Function syntaxTreeScopeDescriptorForBufferPosition(bufferPosition : TAtomPoint) : TAtomScopeDescriptor;
    // Extended Methods
    function  bufferRangeForScopeAtCursor(scopeSelector : String) : TAtomRange;
    Function isBufferRowCommented() : Boolean;
    // Clipboard Operations
    Procedure copySelectedText();
    Procedure cutSelectedText(options :TAtomSelectionOptions); overload;
    Procedure cutSelectedText();  overload;
    Procedure pasteText(options : TAtomSelectionOptions);  overload;
    Procedure pasteText();  overload;
    Procedure cutToEndOfLine(options : TAtomSelectionOptions);  overload;
    Procedure cutToEndOfLine(); overload;
    Procedure cutToEndOfBufferLine(options : TAtomSelectionOptions);  overload;
    Procedure cutToEndOfBufferLine(); overload;
    // Folds
    Procedure foldCurrentRow();
    Procedure unfoldCurrentRow();
    Procedure foldBufferRow(bufferRow : NativeInt) ;
    Procedure unfoldBufferRow(bufferRow : NativeInt);
    // Extended Methods
    Procedure foldSelectedLines();
    Procedure foldAll();
    Procedure unfoldAll();
    Procedure foldAllAtIndentLevel(level : NativeInt);
    Function isFoldableAtBufferRow(bufferRow : NativeInt) : boolean;
    Function isFoldableAtScreenRow(bufferRow : NativeInt) : boolean;
    Procedure toggleFoldAtBufferRow();
    Function isFoldedAtCursorRow() : boolean;
    Function isFoldedAtBufferRow(bufferRow : NativeInt) :boolean;
    Function isFoldedAtScreenRow(screenRow : NativeInt) : boolean;
    // Gutters
    function addGutter(options : TAtomGutterOptions) : TAtomGutter;
    Function getGutters() : TAtomGutterArray;
    function gutterWithName(aName : string) : TAtomGutter;
    // Scrolling the TextEditor
    Procedure scrollToCursorPosition(options : TAtomScrollOptions); overload;
    Procedure scrollToCursorPosition(); overload;
    Procedure scrollToBufferPosition(bufferPosition : TAtomPoint; options : TAtomScrollOptions);  overload;
    Procedure scrollToBufferPosition(bufferPosition : TAtomPoint);overload;
    Procedure scrollToScreenPosition(screenPosition : TAtomPoint; options : TAtomScrollOptions); overload;
    Procedure scrollToScreenPosition(screenPosition : TAtomPoint); overload;
    // TextEditor Rendering
    Function getPlaceholderText() : string;
    Procedure setPlaceholderText(placeholderText : string);
  end;
  TAtomTextEditorArray = array of TAtomTextEditor;

  TAtomTextEditorHandler = reference to procedure(aEditor : TAtomTextEditor);

  TAtomTextEditorRegistry = class external name 'TextEditorRegistry' (TJSObject)
  Public
    function add(aEditor : TAtomTextEditor) : TAtomDisposable;
    function remove(aEditor : TAtomTextEditor) : boolean;
    function observe(aHandler : TAtomTextEditorHandler) : boolean;
    function maintainConfig(aEditor : TAtomTextEditor) : TAtomDisposable;
  end;

  TAtomFileSystemEvent = class external name 'object' (TJSObject)
    action : string; // one of created, modified, deleted or renamed
    path : string;
    oldPath : string;
  end;

  TAtomBufferHandler = Reference to Procedure (aBuffer : TAtomTextBuffer);
  TAtomGitRepositoryHandler = Reference to Procedure (aRepo : TAtomGitRepository);
  TAtomFilesChangedHandler = Reference to Procedure (aEvents : Array of TAtomFileSystemEvent);

  TAtomPathOptions  = class external name 'object' (TJSObject)
    mustExist : boolean;
    exact : boolean;
  end;

  TAtomProject = class external name 'Project' (TJSObject)
  Public
    function onDidChangePaths(aHandler : TAtomStringArrayHandler) : TAtomDisposable;
    function onDidAddBuffer(aHandler : TAtomBufferHandler) : TAtomDisposable;
    function observeBuffers(aHandler : TAtomBufferHandler) : TAtomDisposable;
    function observeRepository(aHandler : TAtomGitRepositoryHandler) : TAtomDisposable;
    function onDidAddRepository(aHandler : TAtomGitRepositoryHandler) : TAtomDisposable;
    function onDidChangeFiles(aHandler : TAtomFilesChangedHandler) : TAtomDisposable;
    function getRepositories : TAtomGitRepositoryArray;
    function getRepositoryForDirectory(aDirectory : String) : TAtomGitrepository;
    function getPaths : TStringDynArray;
    procedure setPaths(Paths : array of String; options : TAtomPathOptions); overload;
    procedure setPaths(Paths : array of String); overload;
    procedure addPaths(aPath : String; options : TAtomPathOptions); overload;
    procedure addPaths(aPath : String); overload;
    procedure removePath(aPath : string);
    function getDirectories : TStringDynArray;
    function relativizePath(aFullPath : string) : TStringDynArray;
    function contains(aPath : string) : boolean;
    function getWatcherPromise(aFullPath : string) : TJSPromise;
  end;

  TAtomMenuItemArray = array of TAtomMenuItem;

  TAtomMenuItem = class external name 'Object' (TJSObject)
    label_ : String; external name 'label';
    submenu : TAtomMenuItemArray;
    command : string;
  end;

  TAtomContextMenuItem = class external name 'Object' (TAtomMenuItem)
    enabled : boolean;
    type_ : string; external name 'type';
    visible : boolean;
    created : TJSRaweventHandler;
    shouldDisplay: TJSEventHandler;
  end;

  TAtomContextMenuManager = class external name 'ContextMenuManager' (TJSObject)
  Public
    // Every key is a CSS selector, the value is a TAtomContextMenuItem
    function add (aItems : TJSObject) : TAtomDisposable;
  end;


  TAtomMenuManager = class external name 'MenuManager' (TJSObject)
  Public
    function add(items : array of TAtomMenuItem) : TAtomDisposable;
    procedure update;
  end;

  TAtomThemeManager = class external name 'ThemeManager' (TJSObject)
  Public
    function onDidChangeActiveThemes(aHandler : TAtomHandler) : TAtomDisposable;
    function getLoadedThemeNames : TStringDynArray;
    function getLoadedThemes : TJSValueDynArray;
    function getActiveThemeNames : TStringDynArray;
    function getActiveThemes : TJSValueDynArray;
    function getEnabledThemeNames : TStringDynArray;
  end;


  TAtomPackage = class external name 'Package' (TJSObject)
  Public
    function onDidDectivate(aHandler : TAtomHandler) : TAtomDisposable;
    function isCompatible : Boolean;
    function rebuild  : TJSPromise;
    function getBuildFailureOutput : String;
  end;
  TAtomPackageDynArray = Array of TAtomPackage;

  TAtomPackageHandler = reference to procedure(aPackage : TAtomPackage);

  TAtomPackageManager = class external name 'PackageManager' (TJSObject)
  Public
    function onDidLoadInitialPackages(aHandler : TAtomHandler) : TAtomDisposable;
    function onDidActivateInitialPackages(aHandler : TAtomHandler) : TAtomDisposable;
    function onDidActivatePackage(aHandler : TAtomPackageHandler) : TAtomDisposable;
    function onDidDeactivatePackage(aHandler : TAtomPackageHandler) : TAtomDisposable;
    function onDidLoadPackage(aHandler : TAtomPackageHandler) : TAtomDisposable;
    function onDidUnloadPackage(aHandler : TAtomPackageHandler) : TAtomDisposable;
    function getApmPath : String;
    function getPackageDirPaths : TStringDynArray;
    function resolvePackagePath(aName : string) : String;
    function isBundledPackage(aName : string) : boolean;
    function enablePackage(aName : string) : TAtomPackage;
    function disablePackage(aName : string) : TAtomPackage;
    function isPackageDisabled(aName : string) : Boolean;
    function getActivePackages : TAtomPackageDynarray;
    function getActivePackage(aName : string) : TAtomPackage;
    function isPackageActive(aName : string) : Boolean;
    function hasActivatedInitialPackages : Boolean;
    function getLoadedPackages : TAtomPackageDynarray;
    function getLoadedPackage(aName : string) : TAtomPackage;
    function isPackageLoaded(aName : string) : Boolean;
    function hasLoadedInitialPackages : Boolean;
    function getAvailablePackagePaths : TStringDynArray;
    function getAvailablePackageNames : TStringDynArray;
    function getAvailablePackageMetadata : TStringDynArray;
  end;

  TAtomStyleHandler = reference to procedure (aStyleElement : TJSHTMLStyleElement);

  TAtomStyleManager = class external name 'StyleManager' (TJSObject)
  Public
    function observeStyleElements(aHandler : TAtomStyleHandler) : TAtomDisposable;
    function onDidAddStyleElement(aHandler : TAtomStyleHandler) : TAtomDisposable;
    function onDidRemoveStyleElement(aHandler : TAtomStyleHandler) : TAtomDisposable;
    function onDidUpdateStyleElement(aHandler : TAtomStyleHandler) : TAtomDisposable;
    function getStyleElements : TJSValueDynArray;
    function getUserStyleSheetPath : String;
  end;

  TAtomColor = class external name 'Color' (TJSObject)
    class function parse(S : string) : TAtomColor;
    class function parse(S : TJSObject) : TAtomColor;
    function toHexString : string;
    function toRGBAstring : string;
  end;

  TAtomBufferedProcessOptions = class external name 'Object' (TJSObject)
    command : string;
    args : TStringDynArray;
    options : TJSObject;
    stdout : TAtomStringHandler;
    stderr : TAtomStringHandler;
    exit : TAtomIntegerHandler;
    autoStart : Boolean;
  end;

  TAtomBufferedNodeProcess = class external name 'AtomBufferedNodeProcess' (TJSObject)
    constructor new (aOptions : TAtomBufferedProcessOptions);
  end;

  TAtomProcessError = class external name 'Object'  (TJSObject)
    error : TJSError;
    procedure handle;
  end;

  TProcessErrorHandler = reference to procedure  (aError: TAtomProcessError) ;

  TAtomBufferedProcess = class external name 'AtomBufferedNodeProcess' (TJSObject)
    constructor new (aOptions : TAtomBufferedProcessOptions);
    procedure start;
    procedure kill;
    function onWillThrowError(aHandler : TProcessErrorHandler) : TAtomDisposable;
  end;




  TAtomGrammarReadHandler = reference to procedure(aError : TJSError; aGrammar : TAtomGrammar);
  TAtomGrammarRegistry = class external name 'GrammarRegistry' (TJSObject)
  Public
    Function onDidAddGrammar(aHandler : TAtomGrammarHandler) : TAtomDisposable;
    Function onDidUpdateGrammar(aHandler : TAtomGrammarHandler) : TAtomDisposable;
    Function onDidRemoveGrammar(aHandler : TAtomGrammarHandler) : TAtomDisposable;
    Function getGrammars : TAtomGrammarDynArray;
    Function grammarForScopeName(aScope : String) : TAtomGrammar;
    Function addGrammar(aGrammar : TAtomGrammar) : TAtomDisposable;
    Function removeGrammarForScopeName(aScope : String) : TAtomGrammar;
    Function readGrammarSync(aPath : String) : TAtomGrammar;
    Procedure readGrammar(aPath : String; aHandler : TAtomGrammarReadHandler);
    Function loadGrammarSync(aPath : String) : TAtomGrammar;
    Procedure loadGrammar(aPath : String; aHandler : TAtomGrammarReadHandler);
  end;

  TAtomCommandDescriptor = class external name 'Object' (TJSObject)
    name : string;
    displayName : string;
    description : string;
    tags : TStringDynArray;
  end;

  TAtomCommandDescriptorArray = array of TAtomCommandDescriptor;

  TAtomCommandListener = reference to procedure (aEvent : TJSEvent);
  TAtomCommandListenerObj = class external name 'Object' (TJSObject)
    didDispatch : TAtomCommandListener;
    displayName : string;
    description : string;
    hiddenInCommandPalette : boolean;
  end;

  TAtomFindCommandsParams = class external name 'Object' (TJSObject)
    target : TJSHTMLelement;
  end;

  TAtomCommandDispatchHandler = reference to procedure(event : TJSEvent);

  TAtomCommandRegistry = class external name 'CommandRegistry' (TJSObject)
  Public
    function add(aTarget : string; aCommandName : string; listener: TAtomCommandListener) : TAtomDisposable; overload;
    function add(aTarget : TJSHTMLElement; aCommandName : string; listener: TAtomCommandListener) : TAtomDisposable; overload;
    function add(aTarget : string; aCommandName : string; listener: TAtomCommandListenerObj) : TAtomDisposable; overload;
    function add(aTarget : TJSHTMLElement; aCommandName : string; listener: TAtomCommandListenerObj) : TAtomDisposable; overload;
    function add(aTarget : string; aCommands: TJSObject) : TAtomDisposable; overload;
    function add(aTarget : TJSHTMLElement; aCommands  : TJSObject) : TAtomDisposable; overload;
    function findCommands (aParams : TAtomFindCommandsParams) : TAtomCommandDescriptorArray;
    procedure dispatch (aTarget : TJSHTMLElement; aCommand : string);
    function onWillDispatch(aHandler : TAtomCommandDispatchHandler) : TAtomDisposable;
    function onDidDispatch(aHandler : TAtomCommandDispatchHandler) : TAtomDisposable;
  end;

  TAtomViewCreator = reference to function : TJSHTMLElement;
  TAtomViewRegistry = class external name 'ViewRegistry' (TJSObject)
  Public
    function addViewProvider(ModelConstructor : TClass; createView : TAtomViewCreator) :TAtomDisposable;
    function getView(aObject : TJSObject) : TJSHTMLElement; overload;
    function getView(aObject : TObject) : TJSHTMLElement; overload;
  end;

  TAtomTooltipFunction = reference to function : string;
  TAtomTooltipDelay = class external name 'Object' (TJSObject)
    show : nativeint;
    hide : nativeint;
  end;

  TAtomTooltipOptions = class external name 'Object' (TJSObject)
    title : string;
    titlefunc : TAtomTooltipFunction; external name 'title';
    html : boolean;
    item : TJSHTMLElement;
    class_ : string; external name 'class';
    placement: string;
    placementfunc : TAtomTooltipFunction; external name 'placement';
    trigger : string;
    delay : TAtomTooltipDelay;
    keyBindingCommand : string
  end;

  TAtomTooltipManager = class external name 'TooltipManager' (TJSObject)
    function add(aTarget : TJSHTMLElement; aOptions : TAtomTooltipOptions) : TAtomDisposable ;
    function findTooltips(Target : TJSHTMLElement): TJSObjectDynArray;
  end;

  TAtomKeyModifiers = class external name 'Object' (TJSObject)
    ctrl : boolean;
    alt : boolean;
    shift : boolean;
    cmd : boolean;
    which : NativeInt;
    target : TJSHTMLElement;
  end;

  // Not specified...
  TAtomKeyBinding = JSValue;

  TAtomKeymapEvent = class external name 'Object' (TJSObject)
    keystrokes : string;
    keyboardEventTarget : TJSHTMLElement;
  end;

  TAtomFullKeymapEvent = class external name 'Object' (TAtomKeymapEvent)
    binding : TAtomKeyBinding;
  end;

  TAtomPartialKeymapEvent = class external name 'Object' (TAtomKeymapEvent)
    partiallyMatchedBindings : array of TAtomKeyBinding;
  end;

  TAtomKeymapLoadError = class external name 'Object' (TJSObject)
    message : string;
    stack : string;
  end;

  TAtomKeymapDidMatchBindingHandler = reference to Procedure (aEvent : TAtomFullKeymapEvent);
  TAtomKeymapDidPartiallyMatchBindingHandler = reference to Procedure (aEvent : TAtomPartialKeymapEvent);
  TAtomKeymapFailtoMatchBindingHandler = reference to Procedure (aEvent : TAtomKeymapEvent);
  TAtomKeymapLoadFailHandler = reference to procedure (aEvent : TAtomKeymapLoadError);

  TAtomFindKeyOptions = class external name 'Object' (TJSObject)
    keystrokes : string;
    command : string;
    target : TJSHTMLElement;
  end;

  TAtomLoadKeymapOptions = class external name 'Object' (TJSObject)
    watch : boolean;
    priority : NativeInt;
  end;

  TAtomWatchKeymapOptions = class external name 'Object' (TJSObject)
    watch : boolean;
    priority : NativeInt;
  end;

  TAtomKeystrokeResolver  = class external name 'Object' (TJSObject)
    keystroke : string;
    event : TJSKeyboardEvent;
    layoutname : string;
    keymap : TJSObject;
  end;

  TAtomKeymapManager = class external name 'KeymapManager' (TJSObject)
  Public
    Class function buildKeydownEvent(key : string; Options : TAtomKeyModifiers) : String;
    constructor new (aOptions : TJSObject);
    procedure clear;
    procedure destroy;
    function onDidMatchBinding(aHandler : TAtomKeymapDidMatchBindingHandler) : TAtomDisposable;
    function onDidPartiallyMatchBinding(aHandler : TAtomKeymapDidPartiallyMatchBindingHandler) : TAtomDisposable;
    function onDidFailToMatchBinding(aHandler : TAtomKeymapFailtoMatchBindingHandler) : TAtomDisposable;
    function onDidFailToReadFile(aHandler : TAtomKeymapLoadFailHandler) : TAtomDisposable;
    procedure add(source : string; bindings : TJSObject);
    procedure add(source : string; bindings : TJSObject; Priority :  NativeInt);
    function build(source : string; bindings : TJSObject) : JSValue;
    function build(source : string; bindings : TJSObject; Priority :  NativeInt) : JSValue;
    function getKeyBindings : TJSValueDynArray;
    function findKeyBindings(params : TAtomFindKeyOptions) : TJSValueDynArray;
    procedure loadKeymap(path : String; Options : TAtomLoadKeymapOptions);
    procedure watchKeymap(path : String; Options : TAtomWatchKeymapOptions);
    procedure handleKeyboardEvent(aEvent : TJSKeyboardEvent);
    function keystrokeForKeyboardEvent(aEvent : TJSKeyboardEvent) : string;
    function addKeystrokeResolver(aResolver : TAtomKeystrokeResolver) : TAtomDisposable;
    function getPartialMatchTimeOut : NativeInt;
  end;

  TAtomConfigObserveOptions = Class external name 'Object' (TJSObject)
    scope : TAtomScopeDescriptor;
  end;

  TAtomConfigChangeDescriptor = Class external name 'Object' (TJSObject)
    oldValue : JSValue;
    newValue : JSValue;
  end;

  TAtomConfigGetOptions = Class external name 'Object' (TJSObject)
    sources : TStringDynArray;
    exludeSources : TStringDynArray;
    scope : TAtomScopeDescriptor;
  end;

  TAtomConfigSetOptions  = Class external name 'Object' (TJSObject)
    source : String;
    scopeSelector : string;
  end;

  TAtomConfigObserveHandler = reference to procedure (aValue : JSValue);
  TAtomConfigChangeHandler = reference to procedure (aChange : TAtomConfigChangeDescriptor);

  TAtomConfigGetAll = Class external name 'Object' (TJSObject)
    value : JSValue;
    scopeSelector : TAtomScopeDescriptor;
  end;
  TAtomConfigGetAllArray = Array of TAtomConfigGetAll;

  TAtomConfig = class external name 'Config' (TJSObject)
  Private
    function _get(aPath : String) : JSValue; external name 'get';
    procedure _set(aPath : String; aValue : JSValue); external name 'set';
  Public
    function observe(aPath : String; Options: TAtomConfigObserveOptions; aHandler : TAtomConfigObserveHandler):TAtomDisposable;
    function observe(aPath : String; aHandler : TAtomConfigObserveHandler) :TAtomDisposable;
    function onDidChange(aPath : String; Options: TAtomConfigObserveOptions; aHandler : TAtomConfigChangeHandler):TAtomDisposable;
    function onDidChange(aPath : String; aHandler : TAtomConfigChangeHandler) :TAtomDisposable;
    function get(aPath : String; aOptions : TAtomConfigGetOptions) : JSValue;
    function get(aPath : String) : JSValue;
    function getAll(aPath : String; aOptions : TAtomConfigGetOptions) : TAtomConfigGetAllArray;
    function getAll(aPath : String) : TAtomConfigGetAllArray;
    function getSources : TStringDynArray;
    function getSchema(aPath: String) : TJSObject;
    procedure transact(aHandler : TAtomHandler) ;
    function set_(aPath : String; aValue : JSValue; aOptions : TAtomConfigSetOptions) : boolean; external name 'set';
    function set_(aPath : String; aValue : JSValue) : boolean; external name 'set';
    function unset(aPath : String; aOptions : TAtomConfigGetOptions) : JSValue;
    function unset(aPath : String) : JSValue;
    property settings[aName : string] : JSValue Read _Get Write _Set; default;
  end;

  TAtomNotification = Class external name 'Notification' (TJSObject)
  Public
    function onDidDismiss(aHandler : TAtomHandler) : TAtomDisposable;
    function onDidDisplay(aHandler : TAtomHandler) : TAtomDisposable;
    function getType : string;
    function getMessage : string;
    procedure dismiss;
  end;
  TAtomNotificationArray = Array of TAtomNotification;

  TAtomNotificationButtonCLickHandler = reference to procedure;
  TAtomNotificationButton = class external name 'Object' (TJSObject)
     classname : string;
     text : string;
     onDidClick : TAtomNotificationButtonCLickHandler;
  end;

  TAtomNotificationButtonArray = array of TAtomNotificationButton;
  TAtomNotificationOptions = Class external name 'Object' (TJSObject)
    buttons : TAtomNotificationButtonArray;
    description : string;
    detail : string;
    dismissable : boolean;
    icon : string;
  end;
  TAtomErrorNotificationOptions = Class external name 'Object' (TAtomNotificationOptions)
    Stack : string;
  end;

  TAtomNotificationManager = class external name 'NotificationManager' (TJSObject)
  Public
    function addSuccess(aMessage : string) : TAtomNotification;
    function addSuccess(aMessage : string; aOptions : TAtomNotificationOptions) : TAtomNotification;
    function addInfo(aMessage : string) : TAtomNotification;
    function addInfo(aMessage : string; aOptions : TAtomNotificationOptions) : TAtomNotification;
    function addWarning(aMessage : string) : TAtomNotification;
    function addWarning(aMessage : string; aOptions : TAtomNotificationOptions) : TAtomNotification;
    function addError(aMessage : string) : TAtomNotification;
    function addError(aMessage : string; aOptions : TAtomErrorNotificationOptions) : TAtomNotification;
    function addFatalError(aMessage : string) : TAtomNotification;
    function addFatalError(aMessage : string; aOptions : TAtomErrorNotificationOptions) : TAtomNotification;
    procedure clear;
    function getNotifications : TAtomNotificationArray;
  end;

  TAtomDeserializer = class
  Public
    name : string;
    procedure deserialize(aState : string; aAtom : TAtom); virtual; abstract;
  end;

  TAtomDeserializerManager = class external name 'DeserializerManager' (TJSObject)
  Public
    procedure add(aSerializer : TAtomDeserializer);
    procedure add(aSerializers : array of TAtomDeserializer);
    procedure deserialized(aState : TJSObject);
  end;

  TAtomClipboardData = class external name 'Object' (TJSObject)
    text : string;
    metadata : JSValue;
  end;

  TAtomClipboard = class external name 'Clipboard' (TJSObject)
  Public
    procedure write(aText : string; metadata : JSValue);
    function read : string;
    function readWithMetadata : TAtomClipboardData;
  end;

  // Maybe this should be a TJSHTMLElement...
  TAtomPaneItem = TJSObject;
  TAtomPaneItemArray = Array of  TAtomPaneItem;

  TPaneItemHandler = reference to procedure (aPane : TAtomPaneItem);

  TAtomOpenEvent = class external name 'Object' (TJSObject)
    uri : string;
    item : TJSObject;
    pane : TAtomPaneItem;
    index : NativeInt;
  end;

  TAtomPaneItemEvent = class external name 'Object' (TJSObject)
    pane : TAtomPaneItem;
  end;

  TAtomPaneItemIndexEvent = class external name 'Object' (TJSObject)
    item : TAtomPaneItem;
    index : NativeInt;
  end;

  TAtomPaneItemMoveEvent = class external name 'Object' (TJSObject)
    item : TAtomPaneItem;
    oldIndex : NativeInt;
    newIndex : NativeInt;
  end;

  TAtomPaneItemPending = class external name 'Object' (TJSObject)
    pending : Boolean;
  end;

  TAtomPaneItemIndexPending = class external name 'Object' (TJSObject)
    pending : Boolean;
    index : NativeInt;
  end;

  TAtomSplitParams = class external name 'Object' (TJSObject)
    items : TAtomPaneItemArray;
    copyActiveItem : Boolean;
  end;

  TAtomFlexScaleHandler = reference to procedure(flexScale : double);
  TAtomActiveHandler  = reference to procedure(aActive : Boolean);


  TAtomPaneItemIndexHandler = reference to procedure (aEvent : TAtomPaneItemIndexEvent);
  TAtomPaneItemMoveHandler = reference to procedure (aEvent : TAtomPaneItemMoveEvent);
  TMRUItemHandler = reference to procedure (aItem : JSValue);

  TAtomPane = Class external name 'Pane' (TJSObject)
  Public
    function onDidChangeFlexScale(aHandler : TAtomFlexScaleHandler) : TAtomDisposable;
    function observeFlexScale(aHandler : TAtomFlexScaleHandler) : TAtomDisposable;
    function onDidActivate(aHandler : TAtomHandler) : TAtomDisposable;
    function onWillDestroy(aHandler : TAtomHandler) : TAtomDisposable;
    function onDidDestroy(aHandler : TAtomHandler) : TAtomDisposable;
    function observeActive(aHandler : TAtomActiveHandler) : TAtomDisposable;
    function onDidAddItem(aHandler : TAtomPaneItemIndexHandler) : TAtomDisposable;
    function onDidRemoveItem(aHandler : TAtomPaneItemIndexHandler) : TAtomDisposable;
    function onWillRemoveItem(aHandler : TAtomPaneItemIndexHandler) : TAtomDisposable;
    function onDidMoveItem(aHandler : TAtomPaneItemMoveHandler) : TAtomDisposable;
    function observeItems(aHandler : TPaneItemHandler) : TAtomDisposable;
    function onDidChangeActiveItem(aHandler : TPaneItemHandler) : TAtomDisposable;
    function onChooseNextMRUItem(Handler : TMRUItemHandler) : TAtomDisposable;
    function onChooseLastMRUItem(Handler : TMRUItemHandler) : TAtomDisposable;
    function onDoneChoosingMRUItem(Handler : TMRUItemHandler) : TAtomDisposable;
    function observeActiveItem(aHandler : TPaneItemHandler) : TAtomDisposable;
    function onWillDestroyItem(aHandler : TAtomPaneItemIndexHandler) : TAtomDisposable;
    Function getItems : TAtomPaneItemArray;
    Function getActiveItem : TAtomPaneItem;
    Function itemAtIndex(aIndex : integer) : TAtomPaneItem;
    procedure activateNextItem;
    procedure activatePreviousItem;
    procedure moveItemRight;
    procedure moveItemLeft;
    function getActiveItemIndex : NativeInt;
    procedure activateItemAtIndex(aIndex : NativeInt);
    procedure activateItem(aItem : TAtomPaneItem; aOptions : TAtomPaneItemPending); overload;
    procedure activateItem(aItem : TAtomPaneItem); overload;
    procedure addItem(aItem : TAtomPaneItem; aOptions : TAtomPaneItemIndexPending); overload;
    procedure addItem(aItem : TAtomPaneItem); overload;
    procedure addItems(aItems : Array of TAtomPaneItem; aIndex : integer); overload;
    procedure addItems(aItems : Array of TAtomPaneItem); overload;
    procedure moveItem(aItem : TAtomPaneItem; aIndex : Integer); overload;
    procedure moveItemToPane(aItem : TAtomPaneItem; aPane : TAtomPane; aIndex : Integer); overload;
    function destroyActiveItem : TJSPromise;
    function destroyItem(aItem : TAtomPaneItem; force : Boolean) : TJSPromise; overload;
    function destroyItem(aItem : TAtomPaneItem) : TJSPromise; overload;
    procedure destroyItems;
    procedure destroyInactiveItems;
    procedure saveActiveItem;
    function SaveActiveItemAs(aNext : TAtomHandler) : TJSPromise; overload;
    function SaveActiveItemAs : TJSPromise; overload;
    function SaveItemAs(aItem : TAtomPaneItem; aNext : TAtomHandler) : TJSPromise;overload;
    function SaveItemAs(aItem : TAtomPaneItem) : TJSPromise; overload;
    procedure saveItems;
    function itemForURI(aURI : String) : TAtomPaneItem;
    function activateItemForURI(aURI : String) : Boolean;
    function isActive : Boolean;
    procedure activate;
    procedure destroy;
    function isDestroyed : Boolean;
    function splitLeft(aParams : TAtomSplitParams) : TAtomPane;
    function splitRight(aParams : TAtomSplitParams) : TAtomPane;
    function splitUp(aParams : TAtomSplitParams) : TAtomPane;
    function splitDown(aParams : TAtomSplitParams) : TAtomPane;
  end;

  TAtomPaneArray = Array of TAtomPane;

  TAtomPaneEvent = class external name 'Object' (TJSObject)
    pane : TAtomPane;
  end;

  TExAtomPaneEvent = class external name 'Object' (TAtomPaneEvent)
    item : TJSObject;
    index : NativeInt;
  end;


  TAtomTextEditorEvent = class external name 'Object' (TAtomPaneEvent)
    textEditor : TAtomTextEditor;
    index : NativeInt;
  end;

  TAtomWorkspaceOpenHandler = reference to procedure(aEvent : TAtomOpenEvent);

  TPaneItemEventHandler = reference to procedure (aPane : TAtomPaneItemEvent);

  TPaneHandler = reference to procedure (aPane : TAtomPane);
  TPaneEventHandler = reference to procedure (aPane : TAtomPaneEvent);
  TExPaneEventHandler = reference to procedure (aPane : TExAtomPaneEvent);
  TAtomTextEditorEventHandler = reference to procedure (aPane : TAtomTextEditorEvent);

  TAtomWorkspaceOpenOptions = class external name 'Object' (TJSObject)
    initialLine : NativeInt;
    initialColumn : NativeInt;
    split : string;
    activatePane : Boolean;
    activateItem : Boolean;
    pending : boolean;
    searchAllPanes : Boolean;
    location : string;
  end;

  TAtomWorkspaceCenter = class external name 'WorkspaceCenter' (TJSObject)
  Public
    // Event subscription
    function observeTextEditors(aHandler : TAtomTextEditorHandler) : TAtomDisposable;
    function observePaneItems(aHandler : TPaneItemHandler) : TAtomDisposable;
    function onDidChangeActivePaneItem(aHandler : TPaneItemHandler) : TAtomDisposable;
    function onDidStopChangingActivePaneItem(aHandler : TPaneItemHandler) : TAtomDisposable;
    function observeActivePaneItem(aHandler : TPaneItemHandler) : TAtomDisposable;
    // Extended

    function onDidAddPane(aHandler : TExPaneEventHandler) :TAtomDisposable;
    function onWillDestroyPane(aHandler : TExPaneEventHandler) :TAtomDisposable;
    function onDidDestroyPane(aHandler : TExPaneEventHandler) :TAtomDisposable;
    function observePanes(aHandler : TPaneHandler) :TAtomDisposable;
    function onDidChangeActivePane (aHandler : TPaneHandler) : TAtomDisposable;
    function observeActivePane(aHandler : TPaneHandler) : TAtomDisposable;
    function onDidAddPaneItem(aHandler : TExPaneEventHandler) : TAtomDisposable;
    function onWillDestroyPaneItem(aHandler : TExPaneEventHandler) : TAtomDisposable;
    function onDidDestroyPaneItem(aHandler : TExPaneEventHandler) : TAtomDisposable;
    function onDidAddtextEditor(aHandler : TAtomTextEditorEventHandler) : TAtomDisposable;
    // pane items
    function getPaneItems : TJSObjectDynArray;
    function getActivePaneItem : TJSObject;
    // panes
    function getPanes : TAtomPaneArray;
    function getActivePane : TAtomPane;
    procedure activateNextPane;
    procedure activatePreviousPane;
  end;

  TAtomVisibleHandler = reference to procedure(aVisible : boolean);
  TAtomHoveredHandler = reference to procedure(aHovered : boolean);

  TAtomDock = class external name 'Dock' (TJSObject)
  Public
    procedure activate;
    procedure show;
    procedure hide;
    procedure toggle;
    function isVisible : boolean;
    function onDidChangeVisible(aHandler : TAtomVisibleHandler) :TAtomDisposable;
    function observeVisible(aHandler : TAtomVisibleHandler) :TAtomDisposable;
    function observePaneItems(aHandler : TPaneItemHandler) :TAtomDisposable;
    function onDidChangeActivePaneItem(aHandler : TPaneItemHandler) :TAtomDisposable;
    function onDidStopChangeActivePaneItem(aHandler : TPaneItemHandler) :TAtomDisposable;
    function observeActivePaneItem(aHandler : TPaneItemHandler) :TAtomDisposable;
    function onDidAddPane(aHandler : TExPaneEventHandler) :TAtomDisposable;
    function onWillDestroyPane(aHandler : TExPaneEventHandler) :TAtomDisposable;
    function onDidDestroyPane(aHandler : TExPaneEventHandler) :TAtomDisposable;
    function observePanes(aHandler : TPaneHandler) :TAtomDisposable;
    function onDidChangeActivePane (aHandler : TPaneHandler) : TAtomDisposable;
    function observeActivePane(aHandler : TPaneHandler) : TAtomDisposable;
    function onDidAddPaneItem(aHandler : TExPaneEventHandler) : TAtomDisposable;
    function onWillDestroyPaneItem(aHandler : TExPaneEventHandler) : TAtomDisposable;
    function onDidDestroyPaneItem(aHandler : TExPaneEventHandler) : TAtomDisposable;
    function onDidChangeHovered(aHandler : TAtomHoveredHandler) : TAtomDisposable;
    function getPaneItems : TJSObjectDynArray;
    function getActivePaneItem : TJSObject;
    function getPanes : TAtomPaneArray;
    function getActivePane : TAtomPane;
    procedure activateNextPane;
    procedure activatePreviousPane;
  end;


  TAtomPanelHandler = reference to procedure(aPanel : TAtomPanel);

  TAtomPanel = class external name 'Panel' (TJSObject)
    procedure destroy;
    function onDidChangeVisible(aHandler : TAtomVisibleHandler) :TAtomDisposable;
    function onDidDestroy(aHandler : TAtomPanelHandler) : TAtomDisposable;
    function getItem : TJSObject;
    function getPriority : NativeInt;
    function isVisible : Boolean;
    procedure hide;
    procedure show;
  end;

  TAtomPanelOptions = class external name 'Object' (TJSObject)
    item : TJSHTMLElement;
    itemObj : TJSObject; external name 'item';
    visible : boolean;
    priority : nativeint;
  end;



  TAtomReplaceHandler = reference to procedure (aValue : TJSObject);

  TAtomWorkspace = class external name 'Workspace' (TJSObject)
  Public
    function observeTextEditors(aHandler : TAtomTextEditorHandler) : TAtomDisposable;
    function observePaneItems(aHandler : TPaneItemHandler) : TAtomDisposable;
    function onDidChangeActivePaneItem(aHandler : TPaneItemHandler) : TAtomDisposable;
    function onDidStopChangingActivePaneItem(aHandler : TPaneItemHandler) : TAtomDisposable;
    function onDidChangeActiveTextEditor(aHandler : TAtomTextEditorHandler) : TAtomDisposable;
    function observeActivePaneItem(aHandler : TPaneItemHandler) : TAtomDisposable;
    function observeActiveTextEditor(aHandler : TAtomTextEditorHandler) : TAtomDisposable;
    function onDidOpen (aHandler : TAtomWorkspaceOpenHandler) : TAtomDisposable;
    // Extended
    function onDidAddPane (aHandler : TPaneEventHandler) : TAtomDisposable;
    function onWillDestroyPane (aHandler : TPaneEventHandler) : TAtomDisposable;
    function onDidDestroyPane (aHandler : TPaneEventHandler) : TAtomDisposable;
    function observePanes(aHandler : TPaneHandler) : TAtomDisposable;
    function onDidChangeActivePane (aHandler : TPaneHandler) : TAtomDisposable;
    function observeActivePane(aHandler : TPaneHandler) : TAtomDisposable;
    function onDidAddPaneItem(aHandler : TExPaneEventHandler) : TAtomDisposable;
    function onWillDestroyPaneItem(aHandler : TExPaneEventHandler) : TAtomDisposable;
    function onDidDestroyPaneItem(aHandler : TExPaneEventHandler) : TAtomDisposable;
    function onDidAddtextEditor(aHandler : TAtomTextEditorEventHandler) : TAtomDisposable;
    // Opening
    function Open () : TJSPromise; overload;
    function Open (aURI: String) : TJSPromise; overload;
    function Open (aURI: String; aOptions : TAtomWorkspaceOpenOptions) : TJSPromise; overload;
    function hide(aObject : TJSObject) : Boolean;
    function hide(aURI : String) : Boolean;
    function toggle(aObject : TJSObject) : TJSPromise;
    function toggle(aURI : String) : TJSPromise;
    function createItemForURI(aUri : String) : TJSPromise;
    function createItemForURI() : TJSPromise;
    function isTextEditor(aObject : TJSObject) : Boolean;
    function reopenItem : TJSPromise;
    function addOpener(aOpener : TAtomObjectFunctionHandler) : TJSPromise;
    function buildTextEditor : TAtomTextEditor;
    function getPaneItems : TJSObjectDynArray;
    function getActivePaneItem : TAtomPaneItem;
    function getTextEditors : TAtomTextEditorArray;
    function getActiveTextEditor : TAtomTextEditor;
    // Panes : extended
    function getActivePaneContainer : TJSObject;
    function getPanes : TAtomPaneArray;
    function getActivePane : TAtomPane;
    procedure activateNextPane;
    procedure activatePreviousPane;
    function paneContainerForURI(aString : String) : TJSObject;
    function paneContainerForItem(aObject : TJSObject) : TJSObject;
    function paneForURI(aString : String) : TAtomPane;
    function paneForItem(aObject : TJSObject) : TAtomPane;
    function getCenter : TAtomWorkspaceCenter;
    function getLeftDock : TAtomDock;
    function getRightDock : TAtomDock;
    function getBottomDock : TAtomDock;
    function getBottomPanels : TJSObjectDynArray;
    function getLeftPanels : TJSObjectDynArray;
    function getRightPanels : TJSObjectDynArray;
    function getTopPanels : TJSObjectDynArray;
    function getHeaderPanels : TJSObjectDynArray;
    function getFooterPanels : TJSObjectDynArray;
    function getModalPanels : TJSObjectDynArray;
    function addBottomPanel(aOptions : TAtomPanelOptions) : TAtomPanel;
    function addLeftPanel(aOptions : TAtomPanelOptions) : TAtomPanel;
    function addRightPanel(aOptions : TAtomPanelOptions) : TAtomPanel;
    function addTopPanel(aOptions : TAtomPanelOptions) : TAtomPanel;
    function addHeaderPanel(aOptions : TAtomPanelOptions) : TAtomPanel;
    function addFooterPanel(aOptions : TAtomPanelOptions) : TAtomPanel;
    function addModalPanel(aOptions : TAtomPanelOptions) : TAtomPanel;
    function panelForItem (aItem : TJSObject) : TAtomPanel;
    function scan (aRegex : TJSRegexp; aOptions : TAtomScanOptions; aHandler : TAtomScanHandler) : TJSPromise;
    function scan (aRegex : TJSRegexp; aHandler : TAtomScanHandler) : TJSPromise;
    function replace (aRegex : TJSRegexp; aReplacement : String; FilePath : Array of string; aHandler : TAtomReplaceHandler) : TJSPromise;
  end;


  { TAtom }

  TAtom = Class external name 'Atom' (TJSObject)
  private
    fclipboard: TAtomClipboard; external name 'clipboard';
    fcommands: TAtomCommandRegistry;  external name 'commands';
    fconfig: TAtomConfig;  external name 'config';
    fcontextmenu: TAtomContextMenuManager;  external name 'contextmenu';
    fdeserializers: TAtomDeserializerManager; external name 'fdeserializers';
    fgrammars: TAtomGrammarRegistry; external name 'grammars';
    fhistory: TAtomHistoryManager; external name 'history';
    fkeymaps: TAtomKeyMapManager; external name 'keymaps';
    fnotifications: TAtomNotificationManager; external name 'notifications';
    fpackages: TAtomPackageManager; external name 'packages';
    fproject: TAtomProject; external name 'project';
    fstyles: TAtomStyleManager; external name 'styles';
    ftext: TAtomTextEditorRegistry; external name 'textEditors';
    fthemes: TAtomThemeManager; external name 'themes';
    ftooltips: TAtomTooltipManager; external name 'tooltips';
    fviews: TAtomViewRegistry; external name 'views';
    fworkspace: TAtomWorkspace; external name 'workspace';
    fmenu: TAtomMenuManager; external name 'menu';
  Protected
    function getAppName : string;
    function getVersion : string;
    function getReleaseChannel : string;
    function getReleasedVersion : string;
    function getWindowLoadTime : NativeInt;
    function getStartupMarkers : TTimingMarkerArray;
    function getLoadSettings : TJSObject;
    function getSize : TAtomWindowSize;
    function getPosition : TAtomWindowPosition;
    function isFullScreen : Boolean;
    procedure setFullScreen(aValue : Boolean);
  Public
    function inDevMode : Boolean;
    function inSafeMode : Boolean;
    function inSpecMode : Boolean;
    function isMaximized : Boolean;
    function open(aOptions : TAtomOpenOptions) : JSValue;
    function close : JSValue;

    Function onDidBeep(aHandler: TAtomHandler) : TAtomDisposable;
    Function onWillThrowError(aHandler: TAtomWillThrowErrorHandler) : TAtomDisposable;
    Function whenShellEnvironmentLoaded(aHandler : TAtomHandler)  : TAtomDisposable;

    property history : TAtomHistoryManager read fhistory;
    property workspace : TAtomWorkspace read fworkspace;
    property textEditors : TAtomTextEditorRegistry read ftext;
    property project : TAtomProject read fproject;
    property contextMenu : TAtomContextMenuManager read fcontextmenu;
    property menu : TAtomMenuManager read fmenu;
    property themes : TAtomThemeManager read fthemes;
    property packages : TAtomPackageManager read fpackages;
    property styles : TAtomStyleManager read fstyles;
    property grammars : TAtomGrammarRegistry read fgrammars;
    property commands : TAtomCommandRegistry read fcommands;
    property tooltips : TAtomTooltipManager read ftooltips;
    property keymaps : TAtomKeyMapManager read fkeymaps;
    property config : TAtomConfig read fconfig;
    property notifications : TAtomNotificationManager read fnotifications;
    property views : TAtomViewRegistry read fviews;
    property deserializers : TAtomDeserializerManager read fdeserializers;
    property clipboard : TAtomClipboard Read fclipboard;


    procedure center;
    procedure focus;
    procedure show;
    procedure hide;
    procedure reload;
    procedure toggleFullScreen;
    procedure beep;
    function confirm(aOoptions : TAtomConfirmOptions) : Integer;
    procedure confirm(aOoptions : TAtomConfirmOptions; aHandler : TAtomConfirmHandler);
    procedure restartApplication;
    procedure setSize(aWidth,aHeight : NativeInt);
    procedure setPosition(aX,aY : NativeInt);
    Procedure pickFolder(aHandler : TAtomPickFolderHandler);

    function openDevTools : TJSPromise;
    function toggleDevTools : TJSPromise;
    function executeJavascriptInDevTools(aCode : String) : JSValue;
    function getCurrentWindow : TJSWindow;

    Property AppName : string read getAppName;
    Property Version : String read getVersion;
    Property ReleaseChannel : String Read getReleaseChannel;
    Property ReleasedVersion : String Read getReleasedVersion;
    Property WindowLoadTime : NativeInt read getWindowLoadTime;
    Property StartupMarkers  : TTimingMarkerArray read getStartupMarkers;
    Property LoadSettings : TJSObject read getLoadSettings;
    Property Size : TAtomWindowSize Read GetSize;
    Property Position : TAtomWindowPosition Read GetPosition;
    property FullScreen : Boolean Read IsFullScreen Write SetFullScreen;
  end;

  TAtomCompositeDisposable = Class external name 'CompositeDisposable' (TJSObject)
  Public
    procedure dispose;
    procedure add(Disposables: TAtomDisposable); varargs;
    procedure add(Disposables: TJSObject); varargs;
    procedure remove(Disposable : TAtomDisposable);
    procedure delete(Disposable : TAtomDisposable);
    procedure clear;
  end;


  TAtomLayerDecoration = Class external name 'LayerDecoration' (TJSObject)
  Public
    procedure destroy;
    function isDestroyed : Boolean;
    function getProperties : TJSObject;
    procedure setProperties(aValue: TJSObject);
    procedure setPropertiesForMarker(aMarker : TAtomDisplayMarker; aValue: TJSObject);
    procedure setPropertiesForMarker(aMarker : TAtomMarker; aValue: TJSObject);
  end;

  // This is a fake object that collects common calls in TAtomFile/TAtomDirectory
  TAtomFileOrDirectory = Class external name 'Object' (TJSObject)
  Public
    constructor new(aPath : string);
    constructor new(aPath : string; aSymlink : Boolean);
    function onDidChange(callback : TAtomHandler) : TAtomDisposable;
    function isFile : Boolean;
    function isSymbolicLink : Boolean;
    function exists : TJSPromise;
    function existsSync : boolean;
    function isDirectory : Boolean;
    function getPath : String;
    function getRealPathSync : String;
    function getBasename : String;
    function getParent : TAtomDirectory;
  end;
  TAtomFileOrDirectoryArray = array of TAtomFileOrDirectory;

  TAtomGetDirectoryEntriesHandler = reference to procedure (aError : TJSError; aEntries : TAtomFileOrDirectoryArray);

  TAtomDirectory = Class external name 'Directory' (TJSObject)
  Public
    procedure create(aMode : Integer);
    function isRoot : Boolean;
    function relativize(aFullPath : String) : string;
    function getFile(aFilename : string) : TAtomFile;
    function getSubdirectory(aDirectory : string) : TAtomDirectory;
    function getEntriesSync : TAtomFileOrDirectoryArray;
    procedure getEntries(aHandler : TAtomGetDirectoryEntriesHandler);
    function contains (aPath : String) : Boolean;
  end;

  // No docs ?
  TAtomReadStream = Class external name 'ReadStream' (TJSObject)
  end;

  TAtomWriteStream = Class external name 'WriteStream' (TJSObject)
  end;

  TAtomFile = Class external name 'File' (TJSObject)
  Public
    procedure create(aMode : Integer);
    function onDidChange(callback : TAtomHandler) : TAtomDisposable;
    function onDidRename(callback : TAtomHandler) : TAtomDisposable;
    function onDidDelete(callback : TAtomHandler) : TAtomDisposable;
    function onWillThrowWatchError(aHandler : TAtomWatchErrorHandler) :TAtomDisposable;
    function getDigest : TJSPromise;
    function getDigestSync : String;
    procedure setEncoding(aEncoding : string);
    function getEncoding : string;
    function read(Flushcache : Boolean) : TJSPromise;
    function createReadStream : TAtomReadStream;
    function write(aText : string) : TJSPromise;
    function createWriteStream : TAtomWriteStream;
    procedure writeSync(aText : string);
  end;

  TAtomEmitter = Class external name 'Emitter' (TJSObject)
  Public
    procedure clear;
    procedure dispose;
    function on_(aEventName : string; aHandler : TAtomHandler) : TAtomDisposable; external name 'on';
    function once(aEventName : string; aHandler : TAtomHandler) : TAtomDisposable;
    function preempt(aEventName : string; aHandler : TAtomHandler) : TAtomDisposable;
    procedure emit(aEventName : string; aValue : JSValue);
  end;


  TAtomPathErrorHandler = reference to procedure(err : TJSError);

  TAtomPathWatcher = Class external name 'PathWatcher' (TJSObject)
  Public
    function getStartPromise : TJSPromise;
    function onDidError(callback : TAtomPathErrorHandler) : TAtomDisposable;
    procedure dispose;
  end;


  TAtomClearSelectionOptions = class external name 'Object' (TJSObject)
    autoscroll : boolean;
  end;

  TAtomInsertTextOptions  = class external name 'Object' (TJSObject)
    select : boolean;
    autoIndent : boolean;
    autoIndentNewLine : boolean;
    autoDecreaseIndent : Boolean;
    preserveTrailingLineIndentation : Boolean;
    normalizeLineEndings : boolean;
    undo : string;
    bypassReadOnly : Boolean;
  end;


  TAtomSelection = Class external name 'Selection' (TJSObject)
  Public
    function onDidChangeRange(aHandler : TAtomSelectionRangeChangeHandler) : TAtomDisposable;
    function onDidDestroy(callback : TAtomHandler) : TAtomDisposable;
    function getScreenRange : TAtomRange;
    procedure setScreenRange(aRange : TAtomRange; aOptions : TAtomSetRangeOptions);overload;
    procedure setScreenRange(aRange : TAtomRange);overload;
    function getBufferRange : TAtomRange; overload;
    procedure setBufferRange(aRange : TAtomRange; aOptions : TAtomSetRangeOptions); overload;
    procedure setBufferRange(aRange : TAtomRange); overload;
    function getBufferRowRange : TNativeIntDynArray;
    function isEmpty : Boolean;
    function isReversed : Boolean;
    function isSingleScreenLine : boolean;
    function getText : string;
    function intersectsBufferRange(aRange : TAtomRange) : Boolean;
    function intersectsWith (aSelection : TAtomSelection) : Boolean;
    procedure clear;
    procedure clear(aOptions : TAtomClearSelectionOptions);
    procedure selectToScreenPosition(aPosition : TAtomPoint);
    procedure selectToBufferPosition(aPosition : TAtomPoint);
    procedure selectLeft(aCount : NativeInt);
    procedure selectRight(aCount : NativeInt);
    procedure selectUp(aCount : NativeInt);
    procedure selectDown(aCount : NativeInt);
    procedure selectToTop;
    procedure selectToBottom;
    procedure selectAll;
    procedure selectToBeginningOfLine;
    procedure selectToFirstCharacterOfLine;
    procedure selectToEndOfLine;
    procedure selectToEndOfBufferLine;
    procedure selectToBeginningOfWord;
    procedure selectToEndOfWord;
    procedure selectToBeginningOfNextWord;
    procedure selectToPreviousWordBoundary;
    procedure selectToNextWordBoundary;
    procedure selectToPreviousSubwordBoundary;
    procedure selectToNextSubwordBoundary;
    procedure selectToBeginningOfNextParagraph;
    procedure selectToBeginningOfPreviousParagraph;
    procedure selectWord;
    procedure expandOverWord;
    procedure selectLine(aLine : Integer);
    procedure expandOverLine;
    procedure insertText(aText : String; aOptions : TAtomInsertTextOptions); overload;
    procedure insertText(aText : String); overload;
    procedure backspace; overload;
    procedure backspace(Options:  TAtomSelectionOptions); overload;
    procedure deleteToPreviousWordBoundary; overload;
    procedure deleteToPreviousWordBoundary(Options:  TAtomSelectionOptions); overload;
    procedure deleteToNextWordBoundary; overload;
    procedure deleteToNextWordBoundary(Options:  TAtomSelectionOptions); overload;
    procedure deleteToBeginningOfWord; overload;
    procedure deleteToBeginningOfWord(Options:  TAtomSelectionOptions); overload;
    procedure deleteToBeginningOfLine; overload;
    procedure deleteToBeginningOfLine(Options:  TAtomSelectionOptions); overload;
    procedure delete(Options:  TAtomSelectionOptions); overload;
    procedure delete; overload;
    procedure deleteToEndOfLine; overload;
    procedure deleteToEndOfLine(Options:  TAtomSelectionOptions); overload;
    procedure deleteToEndOfWord; overload;
    procedure deleteToEndOfWord(Options:  TAtomSelectionOptions); overload;
    procedure deleteToBeginningOfSubword; overload;
    procedure deleteToBeginningOfSubword(Options:  TAtomSelectionOptions); overload;
    procedure deleteToEndOfSubword; overload;
    procedure deleteToEndOfSubword(Options:  TAtomSelectionOptions); overload;
    procedure deleteLine(Options:  TAtomSelectionOptions); overload;
    procedure deleteLine; overload;
    procedure joinLines(Options:  TAtomSelectionOptions); overload;
    procedure joinLines; overload;
    procedure outdentSelectedRows(Options:  TAtomSelectionOptions); overload;
    procedure outdentSelectedRows; overload;
    procedure autoindentSelectedRows(Options:  TAtomSelectionOptions); overload;
    procedure autoindentSelectedRows; overload;
    procedure toggleLineComments(Options:  TAtomSelectionOptions); overload;
    procedure toggleLineComments; overload;
    procedure cutToEndOfLine(maintainClipBoard : Boolean;Options:  TAtomSelectionOptions); overload;
    procedure cutToEndOfLine(maintainClipBoard : Boolean); overload;
    procedure cutToEndOfBufferLine(maintainClipBoard : Boolean;Options:  TAtomSelectionOptions); overload;
    procedure cutToEndOfBufferLine(maintainClipBoard : Boolean); overload;
    procedure cut(maintainClipBoard, fullLine, bypassReadOnly : Boolean); overload;
    procedure cut(maintainClipBoard, fullLine : Boolean); overload;
    procedure cut(maintainClipBoard : Boolean); overload;
    procedure cut(); overload;
    procedure copy(maintainClipBoard, fullLine : Boolean); overload;
    procedure copy(maintainClipBoard : Boolean); overload;
    procedure copy(); overload;
    procedure fold;
    procedure indentSelectedRows(Options:  TAtomSelectionOptions); overload;
    procedure indentSelectedRows(); overload;
    procedure addSelectionBelow;
    procedure addSelectionAbove;
    procedure merge(aSelection : TAtomSelection; aOptions : TAtomSetRangeOptions); overload;
    procedure merge(aSelection : TAtomSelection); overload;
    function compare(aSelection : TAtomSelection) : Integer; overload;
  end;

  TAtomTask = Class external name 'Task' (TJSObject)
    class function once(aFile : String) : TAtomTask; varargs;
    constructor new(aFile : String);
    procedure start; varargs;
    procedure send(aMessage : JSValue);
    procedure terminate;
    function cancel : boolean;

    function on_(aEvent : string; aHandler :TAtomhandler) : TAtomDisposable;
    function once2(aFile : String) : TAtomTask; varargs; external name 'once';
  end;



implementation

end.

