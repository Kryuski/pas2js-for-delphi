{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019-Now by Michael Van Canneyt, member of the
    Free Pascal development team

    WEB Widget Set

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit webwidget;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils, JS, Web;

Const

  SElementData = 'wwElement';
  STopElementData = SElementData+'Top';
  SContentElementData = SElementData+'Content';
  SElementClass = 'wwClass';

  sEventAbort = 'abort';
  SEventAnimationCancel = 'animationcancel';
  SEventAnimationEnd = 'animationend';
  SEventAnimationIteration = 'animationiteration';
  SEventAnimationStart = 'animationstart';
  sEventAuxClick = 'auxclick';
  sEventBlur = 'blur';
  SEventCancel = 'cancel';
  SEventCanPlay = 'canplay';
  SEventCanPlayThrough = 'canplaythrough';
  SEventChange = 'change';
  sEventClick = 'click';
  sEventCompositionEnd = 'compositionend';
  sEventCompositionStart = 'compositionstart';
  sEventCompositionUpdate = 'compositionupdate';
  sEventContextMenu = 'contextmenu';
  sEventCopy = 'copy';
  sEventCut = 'cut';
  sEventCueChange = 'cuechange';
  sEventDblClick = 'dblclick';
  sEventDurationChange = 'durationchange';
  sEventEnded  = 'ended';
  sEventError  = 'error';
  sEventFocus = 'focus';
  sEventFocusIn  = 'focusin';
  sEventFocusOut  = 'focusout';
  SEventGotPointerCapture = 'gotpointercapture';
  SEventInput = 'input';
  SEventInvalid = 'invalid';
  sEventKeyDown = 'keydown';
  sEventKeyPress = 'keypress';
  sEventKeyUp = 'keyup';
  sEventLoad = 'load';
  sEventLoadedData = 'loadeddata';
  sEventLoadedMetaData = 'loadedmetadata';
  sEventLoadend = 'loadend';
  sEventLoadStart = 'loadstart';
  SEventLostPointerCapture = 'lostpointercapture';
  sEventMouseDown = 'mousedown';
  sEventMouseEnter = 'mouseenter';
  sEventMouseLeave = 'mouseleave';
  sEventMouseMove = 'mousemove';
  sEventMouseOut = 'mouseout';
  sEventMouseUp = 'mouseup';
  sEventOverFlow = 'overflow';
  sEventPaste = 'paste';
  sEventPause = 'pause';
  sEventPlay = 'play';
  SEventPointerCancel = 'pointercancel';
  SEventPointerDown = 'pointerdown';
  SEventPointerEnter = 'pointerenter';
  SEventPointerLeave = 'pointerleave';
  SEventPointerMove = 'pointermove';
  SEventPointerOut = 'pointerout';
  SEventPointerOver = 'pointerover';
  SEventPointerUp = 'pointerup';
  sEventReset = 'reset';
  sEventResize = 'resize';
  sEventScroll = 'scroll';
  sEventSelect = 'select';
  sEventSubmit = 'submit';
  sEventTouchStart = 'touchstart';
  SEventTransitionCancel = 'transitioncancel';
  SEventTransitionEnd = 'transitionend';
  SEventTransitionRun = 'transitionrun';
  SEventTransitionStart = 'transitionstart';
  SEventWheel = 'wheel';


Type
  EWidgets = Class(Exception);
  TCustomWebWidget = Class;

  THTMLNotifyEvent = Procedure (Sender : TObject; Event : TJSEvent) of object;

  TEventDispatch = Record
    MsgStr : String;
    HTMLEvent : TJSEvent;
    EventHandler : THTMLNotifyEvent;
  end;

  { TStyleItem }
  TStylePriority = (spNone,spImportant);

  TStyleItem = Class(TCollectionItem)
  private
    FPriority: TStylePriority;
    FName: String;
    FValue: String;
    FImported : Boolean;
    procedure SetPriority(AValue: TStylePriority);
    procedure SetValue(AValue: String);
    procedure SetName(AValue: String);
  Protected
    procedure MarkDirty;
  Public
    Property Imported : Boolean read FImported;
    Procedure Assign(Source : TPersistent) ; override;
  Published
    Property Name : String Read FName Write SetName;
    Property Value : String Read FValue Write SetValue;
    Property Priority : TStylePriority Read FPriority Write SetPriority;
  end;

  { TWebWidgetStyles }

  TWebWidgetStyles = Class(TOwnedCollection)
  private
    Function GetStyleItem(aIndex : Integer): TStyleItem;
    procedure SetStyleItem(aIndex : Integer; AValue: TStyleItem);
  Protected
    Procedure MarkDirty(aItem : TStyleItem);
    Procedure ApplyToDOM(aElement : TJSHTMlElement;aItem : TStyleItem); virtual; overload;
  Public
    Function Widget : TCustomWebWidget;
    // Manipulate
    Function Add(Const aName : String; const aValue : String= '') : TStyleItem; overload;
    Function EnsureStyle(Const aName : String; const aValue : String= '') : TStyleItem;
    Function IndexOfStyle(Const aName : String) : integer;
    Function FindStyle(Const aName : String) : TStyleItem;
    Function GetStyle(Const aName : String) : TStyleItem;
    Function RemoveStyle(Const aName : String) : String;
    Procedure RefreshFromDOM(aElement : TJSHTMlElement = Nil;DoClear : Boolean = True);virtual;
    Procedure ClearImported;
    Procedure ApplyToDOM(aElement : TJSHTMlElement = Nil); virtual; overload;
    Property Styles[aIndex : Integer] : TStyleItem Read GetStyleItem Write SetStyleItem; default;
  end;


  TStyleRefresh = (srOnElementID, // Only refresh styles if ElementID was set and we bind to existing element.
                   srAlways,      // Always refresh styles
                   srNever);      // Never refresh
  TStyleRefreshes = Set of TStyleRefresh;

  { TReferenceItem }
  TJSHTMLElementArray = Array of TJSHTMLElement;

  TReferenceItem = Class(TCollectionItem)
  private
    FName: String;
    FSelector: String;
    procedure SetInputValidity(AValue: String);
    procedure SetName(AValue: String);
    procedure SetSelector(AValue: String);
    function GetElement: TJSHTMLElement;
    function GetElements: TJSHTMLElementArray;
  Protected
    procedure SetInputValue(AValue: String); virtual;
    Function GetInputValue : String; virtual;
    Procedure MarkDirty; virtual;
  Public
    Procedure Refresh;
    Function Exists : Boolean;
    Function IsArray : Boolean;
    Function IsInput : Boolean;
    Property Element : TJSHTMLElement Read GetElement;
    Property Elements : TJSHTMLElementArray Read GetElements;
    Property InputValue : String Read GetInputValue Write SetInputValue;
    Property InputValidity : String Write SetInputValidity;
  Published
    Property Selector : String Read FSelector Write SetSelector;
    Property Name : String Read FName Write SetName;
  end;

  { TWebWidgetReferences }

  TWebWidgetReferences = Class(TOwnedCollection)
  Private
    FRefs : TJSObject; // Arrays of elements, even for single element
    function GetReferenceItem(aIndex : Integer): TReferenceItem;
    procedure SetReferenceItem(aIndex : Integer; AValue: TReferenceItem);
  Protected
    Procedure MarkDirty(aItem : TReferenceItem);
    Procedure RefreshFromDOM(aItem : TReferenceItem;aElement : TJSHTMlElement);
    Property Refs : TJSObject Read FRefs;
  Public
    Function Widget : TCustomWebWidget;
    // Manipulate
    Function Add(Const aName : String; aSelector : String = '') : TReferenceItem; overload;
    Function EnsureReference(Const aName : String; Const aSelector : String = '') : TReferenceItem;
    Function IndexOfReference(Const aName : String) : Integer;
    Function FindReference(Const aName : String) : TReferenceItem;
    Function GetReference(Const aName : String) : TReferenceItem;
    Procedure RemoveReference(Const aName : String);
    Function ElementExists(Const aName : String) : Boolean;
    Function ElementIsArray(Const aName : String) : Boolean;
    Function FindElementByName(Const aName : String) : TJSHTMLElement;
    Function GetElementByName(Const aName : String) : TJSHTMLElement;
    Function GetElementsByName(Const aName : String) : TJSHTMLElementArray;
    Procedure RefreshFromDOM(aElement : TJSHTMlElement = Nil);virtual;
    Property Items[aIndex : Integer] : TReferenceItem Read GetReferenceItem Write SetReferenceItem;
    Property References[aName : String] : TReferenceItem Read GetReference; default;
  end;

{$DispatchStrField name}
  { TCustomWebWidget }

  TCustomWebWidget = Class(TComponent)
  Private
    const MaxEvents = 66;
    Class Var WidgetID : NativeInt;
    Const FEventNames : Array[0..MaxEvents] of String = (
    // When adding, only add at the end !!
    sEventAbort,               //0
    SEventAnimationCancel,
    SEventAnimationEnd,
    SEventAnimationIteration,
    SEventAnimationStart,
    sEventAuxClick ,
    sEventBlur ,
    SEventCancel ,
    SEventCanPlay ,
    SEventCanPlayThrough ,
    SEventChange ,             // 10
    sEventClick ,
    sEventCompositionEnd ,
    sEventCompositionStart ,
    sEventCompositionUpdate ,
    sEventContextMenu ,
    sEventCopy ,
    sEventCut ,
    sEventCueChange ,
    sEventDblClick ,
    sEventDurationChange ,         // 20
    sEventEnded  ,
    sEventError  ,
    sEventFocus ,
    sEventFocusIn  ,
    sEventFocusOut  ,
    SEventGotPointerCapture ,
    SEventInput ,
    SEventInvalid ,
    sEventKeyDown ,
    sEventKeyPress ,                //  30
    sEventKeyUp ,
    sEventLoad ,
    sEventLoadedData ,
    sEventLoadedMetaData ,
    sEventLoadend ,
    sEventLoadStart ,
    SEventLostPointerCapture ,
    sEventMouseDown ,
    sEventMouseEnter ,
    sEventMouseLeave ,                  // 40
    sEventMouseMove ,
    sEventMouseOut ,
    sEventMouseUp ,
    sEventOverFlow ,
    sEventPaste ,
    sEventPause ,
    sEventPlay ,
    SEventPointerCancel ,
    SEventPointerDown ,
    SEventPointerEnter ,
    SEventPointerLeave ,
    SEventPointerMove ,
    SEventPointerOut ,
    SEventPointerOver ,
    SEventPointerUp ,
    sEventReset ,
    sEventResize ,
    sEventScroll ,
    sEventSelect ,
    sEventSubmit ,
    sEventTouchStart ,
    SEventTransitionCancel ,
    SEventTransitionEnd ,
    SEventTransitionRun ,
    SEventTransitionStart ,
    SEventWheel
  );
  private
    FAfterRenderHTML: TNotifyEvent;
    FAfterUnRenderHTML: TNotifyEvent;
    FBeforeRenderHTML: TNotifyEvent;
    FBeforeUnRenderHTML: TNotifyEvent;
    FParent : TCustomWebWidget;
    FMyHook : TJSRawEventHandler;
    // Set by setting ParentID or Parent
    FParentElement : TJSHTMLElement;
    FElement : TJSHTMLElement;
    FOwnsElement : Boolean;
    FParentID : String;
    FElementID: String;
    FChildren : TJSArray;
    FClasses : String;
    FMyEvents : TJSObject;
    FStyleRefresh: TStyleRefresh;
    FStyles: TWebWidgetStyles;
    FVisible : Boolean;
    FDisplay : String;
    FReferences : TWebWidgetReferences;
    FAttrs : TJSObject;
    function GetChildCount: Integer;
    function GetChild(aIndex : Integer): TCustomWebWidget;
    function GetClasses: String;
    function GetDataset(aName : String): String;
    function GetElement: TJSHTMLELement;
    function GetExternalElement: Boolean;
    function GetHaveReferences: Boolean;
    function GetHTMLEvent(AIndex: Integer): THTMLNotifyEvent;
    function GetIsElementDirty: Boolean;
    function GetParent: TCustomWebWidget;
    function GetParentElement: TJSHTMLELement;
    function GetParentID: String;
    function GetElementID: String;
    function GetReference(const aName : string): TJSHTMLElement;
    function GetReferenceItem(aName : String): TReferenceItem;
    function GetReferenceList(const aName : string): TJSHTMLElementArray;
    function GetReferences: TWebWidgetReferences;
    function GetRendered: Boolean;
    function GetVisible: Boolean;
    procedure SetClasses(AValue: String);
    procedure SetDataset(aName : String; AValue: String);
    procedure SetElementID(AValue: String);
    procedure SetHTMLEvent(AIndex: Integer; AValue: THTMLNotifyEvent);
    procedure SetParent(AValue: TCustomWebWidget);
    procedure SetParentID(AValue: String);
    Procedure AddChild(aValue : TCustomWebWidget);
    Procedure RemoveChild(aValue : TCustomWebWidget);
    procedure SetReferences(AValue: TWebWidgetReferences);
    procedure SetStyles(AValue: TWebWidgetStyles);
    procedure SetVisible(AValue: Boolean);
    procedure SetAttr(const aName : string; AValue: String);
    Function GetAttr(const aName : String) : String;
    // This protected section is not meant to be made public
  Protected
    // Events mechanism
    procedure EventEntry(aEvent: TJSEvent); virtual;
    // Low level
    procedure RemoveEvent(aElement: TJSHTMLElement; const aEvent: String);
    procedure HookupEvent(aElement: TJSHTMLElement; const aEvent: String);
    Procedure HookupEvents(aElement : TJSHTMLElement); virtual;
    // Add to internal list, if rendered, calls hookup
    Procedure AddEvent(aName : String; AHandler : THTMLNotifyEvent);
    // Remove from internal list, if rendered, calls RemoveEvent
    Procedure DeleteEvent(aName : String);
    // Override these if you want somehow to grab a fixed element on a page.
    Class Function FixedParent : TJSHTMLElement; virtual;
    Class Function DefaultParentElement : TJSHTMLElement; virtual;
    Class Function DefaultParent : TCustomWebWidget; virtual;
    Class Function FixedElement : TJSHTMLElement; virtual;
    // Generate an ID
    Class function GenerateID: String;
    // Find element in DOM tree.
    Class Function FindElement(aID : String) : TJSHTMLElement;
    // Create element in DOM tree, set ID if it is nonzero
    Class function CreateElement (aTag : String; aID : String) : TJSHTMLElement;
    Class function CreateElement (aParent:TJSElement; aTag : String; aID : String) : TJSHTMLElement;
    // references are relative to this element. By default, this is the element of the widget.
    // override if you want for instance to indicate the parent element.
    function GetReferenceElement: TJSHTMLELement; virtual;
    // override if you want content to go in this sub-element instead of directly below the toplevel element.
    function GetContentElement: TJSHTMLELement; virtual;
    // Override this if Element is not the top element of this widget.
    function GetTopElement: TJSHTMLELement; virtual;
    // Auxiliary function to create a displayable name of this widget
    Function DisplayElementName : String;
    // Make sure there is an element.
    function EnsureElement: TJSHTMLElement;
    // Set parent element to nil. No rendering is done. Can be called when there are no DOM elements
    Procedure InvalidateParentElement;
    // Set element to nil, clears styles and references. Can be called when there are no DOM elements
    Procedure InvalidateElement; virtual;
    // Name of the tag to create. Set to '' if you don't want RenderHTML to create one.
    Function HTMLTag : String; virtual; abstract;
    // Class names that must always be applied for this widget to work.
    // They are only added during render, and do not show up in classes property.
    // e.g. for a bootstrap button widget, this would return "btn"
    Function WidgetClasses : String; virtual;
    // Override this if you want to create custom styles collection
    Function CreateStyles : TWebWidgetStyles; virtual;
    // Override this if you want to create custom references
    Function CreateReferences : TWebWidgetReferences; virtual;
    // Forces apply of visible, sets Visible property
    procedure ApplyVisible(aElement : TJSHTMLElement; AValue: Boolean); virtual;
    // Here all properties from the widget are applied to the element.
    // This is called during RenderHTML, but also when binding ElementID to an Element.
    Procedure ApplyWidgetSettings(aElement : TJSHTMLElement); virtual;
    {
      Actually create & render element.
      This gets the element as created by RenderHTML and the parent as received by RenderHTML.
      If aElement is nil, the DoRenderHTML is responsible for creating & attaching it to the parent element.
      Must return the value for Element.
    }
    Function DoRenderHTML(aParent,aElement : TJSHTMLElement) :TJSHTMLElement; virtual;
    // Apply data to Element, Top and Content. Can only be called when the 3 are set, i.e. after RenderHTML or when Element is set from ElementID.
    Procedure ApplyData; virtual;
    Procedure RemoveData; virtual;
    // Update references
    Procedure RefreshReferences; virtual;
    // Create html. Creates element below parent, and renders HTML using doRenderHTML
    Function RenderHTML(aParent : TJSHTMLELement) : TJSHTMLElement;
    // Override this if you need to do additional actions besides removing top element from parent. Parent is always assigned
    Procedure DoUnRender(aParent : TJSHTMLElement) ; virtual;
    // Remove HTML, if any. aParent can be nil.
    Procedure UnRender(aParent : TJSHTMLElement); overload;
    // Dispatch an event
    Function DispatchEvent(aName : String; aEvent : TJSEvent = Nil) : Boolean;
    // the rendered or attached element if ElementID was set. Can be Nil;
    // This is the "main" widget of the rendered HTML. There can be HTML below or HTML before.
    Property Element : TJSHTMLELement Read GetElement;
    // The attached parent element. Obtained through Parent or ParentID. Can be Nil;
    // Not necessarily the parent element of Element, but definitely the parent of TopElement;
    Property ParentElement : TJSHTMLELement Read GetParentElement;
    // Content Element. By default equals Element.
    Property ContentElement : TJSHTMLELement Read GetContentElement;
    // Top Element. The parent element is the direct parent of this element.
    Property TopElement : TJSHTMLELement Read GetTopElement;
    // Is true if this class created the element (i.e. it was not obtained with ElementID)
    Property OwnsElement : Boolean Read FOwnsElement;
    // My Events
    Property MyEvents : TJSObject Read FMyEvents;
    // Return true if the ElementID is referring to an existing element.
    Property ExternalElement : Boolean Read GetExternalElement;
    // since reading references creates the collection, we want a way to see if there are any without creating them.
    Property HaveReferences : Boolean Read GetHaveReferences;
    // Property attrs
    Property StoredAttrs : TJSObject Read FAttrs;
  Public
    Class var CreateDataTags : Boolean;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    // Does this element allow childern ?
    Class Function AllowChildren : Boolean; virtual;
    // Manipulate Classes
    Class Function AddRemoveClasses(const Source, aAddClasses, aRemoveClasses : String; Normalize : Boolean = false) : String;
    // Number of classes in search and replace must match.
    Class Function ReplaceClasses(const Source, aSearchClasses, aReplaceClasses : String; Normalize : Boolean = false) : String;
    Class Function RemoveClasses(const Source, aClasses : String; Normalize : Boolean = false) : String;
    Class Function RemoveClasses(el : TJSHTMLElement; const aClasses : String; Normalize : Boolean = false) : String;
    Class Function AddClasses(const Source, aClasses : String; Normalize : Boolean = false) : String;
    Class Function AddClasses(el : TJSHTMLElement; const aClasses : String; Normalize : Boolean = false) : String;
    Class Function AddRemoveClasses(el : TJSHTMLElement; const aAddClasses, aRemoveClasses : String; Normalize : Boolean = false) : String;
    // Number of classes in search and replace must match.
    Class Function ReplaceClasses(el : TJSHTMLElement; const aSearchClasses, aReplaceClasses : String; Normalize : Boolean = false) : String;
    // Manipulate styles
    function EnsureStyle(const aName: String): TStyleItem;
    function AddStyle(const aName,aValue: String): TStyleItem;
    function GetStyleValue(const aName : String) : String;
    function RemoveStyle(const aName: String): String;
    // Remove data from dataset
    Procedure RemoveData(const aName : String);
    // Set attributes
    // Re-render
    Procedure Refresh;
    // Unrender
    Procedure Unrender; overload;
    // Focus widget. Will render if it was not yet rendered.
    Procedure Focus;
    // These work on the classes property, and on the current element if rendered. Returns the new value of classes.
    Function AddRemoveClasses(const aAddClasses, aRemoveClasses : String; Normalize : Boolean = false) : String;
    Function ReplaceClasses(const aSearchClasses, aReplaceClasses : String; Normalize : Boolean = false) : String;
    Function RemoveClasses(const aClasses : String; Normalize : Boolean = false) : String;
    Function AddClasses(const aClasses : String; Normalize : Boolean = false) : String;
    // Finding widgets
    Function FindWidgetByID(aElementID : String; Recurse : Boolean = True) : TCustomWebWidget;
    Function GetWidgetByID(aElementID : String; Recurse : Boolean = True) : TCustomWebWidget;

    // For complex HTML, this is the toplevel element
    Property Parent : TCustomWebWidget Read GetParent Write SetParent;
    // Are we rendered, i.e. is Element valid ?
    Property IsRendered : Boolean Read GetRendered;
    // Do we need to refresh our internal properties from the element? Currently true if rendered.
    // Use this when reading properties and you want/need to refresh a property from the element.
    Property IsElementDirty : Boolean Read GetIsElementDirty;
    // Child widgets. Note that this can differ significantly from
    Property ChildCount : Integer Read GetChildCount;
    Property Children [aIndex : Integer] : TCustomWebWidget Read GetChild;
    Property Data[aName : String] : String Read GetDataset Write SetDataset;
    // This works with style Display: none.
    Property Visible : Boolean Read GetVisible Write SetVisible;
  // This protected section can be published in descendents
  Protected
    // Parent or Element ID: Will be used when determining the HTML element when rendering.
    // Only one of the Parent or Element ID can be set.
    Property ParentID : String Read GetParentID Write SetParentID;
    Property ElementID : String Read GetElementID Write SetElementID;
    // When reading, returns the actual classes if rendered.
    // When rendering, these classes are added to any existing ones if the element exists.
    Property Classes : String Read GetClasses Write SetClasses;
    // Apply these styles when rendering. Depending on StyleRefresh, styles are imported from actual element.
    Property Styles: TWebWidgetStyles Read FStyles Write SetStyles;
    // When rendering, should we refresh styles ?
    Property StyleRefresh : TStyleRefresh Read FStyleRefresh Write FStyleRefresh;
    // Possible references to sub widgets, based on CSS selectors
    Property References : TWebWidgetReferences Read GetReferences write SetReferences;
    // Direct named acces
    Property Reference[aName : String] : TReferenceItem Read GetReferenceItem;
    // Easy access to an element
    Property Elements[const aName : string] : TJSHTMLElement Read GetReference;
    // Easy access to an element list
    Property ElementList[const aName : string] : TJSHTMLElementArray Read GetReferenceList;
    // Easy access to attributed
    Property Attrs[const aName : string] : String Read GetAttr Write SetAttr;
    // Events of TWebWidget
    Property BeforeRenderHTML : TNotifyEvent Read FBeforeRenderHTML Write FBeforeRenderHTML;
    Property AfterRenderHTML : TNotifyEvent Read FAfterRenderHTML Write FAfterRenderHTML;
    Property BeforeUnRenderHTML : TNotifyEvent Read FBeforeUnRenderHTML Write FBeforeUnRenderHTML;
    Property AfterUnRenderHTML : TNotifyEvent Read FAfterUnRenderHTML Write FAfterUnRenderHTML;
    // HTML DOM events
    Property OnAbort: THTMLNotifyEvent Index 0 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnAnimationCancel: THTMLNotifyEvent Index 1 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnAnimationEnd: THTMLNotifyEvent Index 2 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnAnimationIteration: THTMLNotifyEvent Index 3 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnAnimationStart: THTMLNotifyEvent Index 4 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnAuxClick : THTMLNotifyEvent Index 5 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnBlur : THTMLNotifyEvent Index 6 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnCancel : THTMLNotifyEvent Index 7 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnCanPlay : THTMLNotifyEvent Index 8 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnCanPlayThrough : THTMLNotifyEvent Index 9 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnChange : THTMLNotifyEvent Index 10 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnClick : THTMLNotifyEvent Index 11 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnCompositionEnd : THTMLNotifyEvent Index 12 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnCompositionStart : THTMLNotifyEvent Index 13 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnCompositionUpdate : THTMLNotifyEvent Index 14 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnContextMenu : THTMLNotifyEvent Index 15 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnCopy : THTMLNotifyEvent Index 16 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnCut : THTMLNotifyEvent Index 17 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnCueChange : THTMLNotifyEvent Index 18 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnDblClick : THTMLNotifyEvent Index 19 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnDurationChange : THTMLNotifyEvent Index 20 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnEnded  : THTMLNotifyEvent Index 21 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnError  : THTMLNotifyEvent Index 22 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnFocus : THTMLNotifyEvent Index 23 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnFocusIn  : THTMLNotifyEvent Index 24 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnFocusOut  : THTMLNotifyEvent Index 25 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnGotPointerCapture : THTMLNotifyEvent Index 26 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnInput : THTMLNotifyEvent Index 27 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnInvalid : THTMLNotifyEvent Index 28 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnKeyDown : THTMLNotifyEvent Index 29 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnKeyPress : THTMLNotifyEvent Index 30 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnKeyUp : THTMLNotifyEvent Index 31 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnLoad : THTMLNotifyEvent Index 32 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnLoadedData : THTMLNotifyEvent Index 33 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnLoadedMetaData : THTMLNotifyEvent Index 34 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnLoadend : THTMLNotifyEvent Index 35 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnLoadStart : THTMLNotifyEvent Index 36 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnLostPointerCapture : THTMLNotifyEvent Index 37 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnMouseDown : THTMLNotifyEvent Index 38 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnMouseEnter : THTMLNotifyEvent Index 39 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnMouseLeave : THTMLNotifyEvent Index 40 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnMouseMove : THTMLNotifyEvent Index 41 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnMouseOut : THTMLNotifyEvent Index 42 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnMouseUp : THTMLNotifyEvent Index 43 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnOverFlow : THTMLNotifyEvent Index 44 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPaste : THTMLNotifyEvent Index 45 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPause : THTMLNotifyEvent Index 46 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPlay : THTMLNotifyEvent Index 47 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPointerCancel : THTMLNotifyEvent Index 48 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPointerDown : THTMLNotifyEvent Index 49 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPointerEnter : THTMLNotifyEvent Index 50 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPointerLeave : THTMLNotifyEvent Index 51 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPointerMove : THTMLNotifyEvent Index 52 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPointerOut : THTMLNotifyEvent Index 53 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPointerOver : THTMLNotifyEvent Index 54 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPointerUp : THTMLNotifyEvent Index 55 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnReset : THTMLNotifyEvent Index 56 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnResize : THTMLNotifyEvent Index 57 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnScroll : THTMLNotifyEvent Index 58 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnSelect : THTMLNotifyEvent Index 59 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnSubmit : THTMLNotifyEvent Index 60 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnTouchStart : THTMLNotifyEvent Index 61 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnTransitionCancel : THTMLNotifyEvent Index 62 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnTransitionEnd : THTMLNotifyEvent Index 63 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnTransitionRun : THTMLNotifyEvent Index 64 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnTransitionStart : THTMLNotifyEvent Index 65 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnWheel : THTMLNotifyEvent Index 66 Read GetHTMLEvent Write SetHTMLEvent;
  end;
  TCustomWebWidgetClass = Class of TCustomWebWidget;

  { TWebWidget }

  TWebWidget = Class(TCustomWebWidget)
  Published
    // Properties
    Property ParentID;
    Property ElementID;
    Property Classes;
    Property Styles;
    Property StyleRefresh;
    Property Visible;
    // Events
    Property BeforeRenderHTML;
    Property AfterRenderHTML;
    Property OnAbort;
    Property OnAnimationCancel;
    Property OnAnimationEnd;
    Property OnAnimationIteration;
    Property OnAnimationStart;
    Property OnAuxClick;
    Property OnBlur;
    Property OnCancel;
    Property OnCanPlay;
    Property OnCanPlayThrough;
    Property OnChange;
    Property OnClick;
    Property OnCompositionEnd;
    Property OnCompositionStart;
    Property OnCompositionUpdate;
    Property OnContextMenu;
    Property OnCopy;
    Property OnCut;
    Property OnCueChange;
    Property OnDblClick;
    Property OnDurationChange;
    Property OnEnded ;
    Property OnError ;
    Property OnFocus;
    Property OnFocusIn ;
    Property OnFocusOut ;
    Property OnGotPointerCapture;
    Property OnInput;
    Property OnInvalid;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnLoad;
    Property OnLoadedData;
    Property OnLoadedMetaData;
    Property OnLoadend;
    Property OnLoadStart;
    Property OnLostPointerCapture;
    Property OnMouseDown;
    Property OnMouseEnter;
    Property OnMouseLeave;
    Property OnMouseMove;
    Property OnMouseOut;
    Property OnMouseUp;
    Property OnOverFlow;
    Property OnPaste;
    Property OnPause;
    Property OnPlay;
    Property OnPointerCancel;
    Property OnPointerDown;
    Property OnPointerEnter;
    Property OnPointerLeave;
    Property OnPointerMove;
    Property OnPointerOut;
    Property OnPointerOver;
    Property OnPointerUp;
    Property OnReset;
    Property OnResize;
    Property OnScroll;
    Property OnSelect;
    Property OnSubmit;
    Property OnTouchStart;
    Property OnTransitionCancel;
    Property OnTransitionEnd;
    Property OnTransitionRun;
    Property OnTransitionStart;
    Property OnWheel;
  end;
  TWebWidgetClass = Class of TWebWidget;

  { TContainerWidget }

  TContainerWidget = Class(TWebWidget)
  private
    const KnownStyleCount = 6;
    Const KnownStyles : Array[0..KnownStyleCount] of string = ('min-width','max-width','min-height','max-height','display','width','height');
    function GetKnownStyle(AIndex: Integer): String;
    procedure SetKnownStyle(AIndex: Integer; AValue: String);
  Public
    Function HTMLTag : String; override;
    Constructor Create(aOwner : TComponent); override;
    Property MinWidth : String Index 0 Read GetKnownStyle Write SetKnownStyle;
    Property MaxWidth : String Index 1 Read GetKnownStyle Write SetKnownStyle;
    Property MinHeight : String Index 2 Read GetKnownStyle Write SetKnownStyle;
    Property MaxHeight : String Index 3 Read GetKnownStyle Write SetKnownStyle;
    Property Display : String Index 4 Read GetKnownStyle Write SetKnownStyle;
    Property Width : String Index 5 Read GetKnownStyle Write SetKnownStyle;
    Property Height : String Index 6 Read GetKnownStyle Write SetKnownStyle;
  end;

  { TCustomTemplateWidget }

  TCustomTemplateWidget = Class(TWebWidget)
  private
    FContainerTag: String;
    procedure SetContainerTag(AValue: String);
  Protected
    function GetReferenceElement: TJSHTMLELement; override;
    function GetTemplateHTML: String; virtual; abstract;
    Procedure ApplyTemplate(aElement : TJSHTMLElement); virtual;
    Function DoRenderHTML(aParent, aElement: TJSHTMLElement) : TJSHTMLElement; override;
    // When set, a tag will be created and the template will be rendered below this tag.
    Property ContainerTag : String Read FContainerTag Write SetContainerTag;
  Public
    Function HTMLTag : String; override;
  end;

  { TSimpleTemplateWidget }

  TSimpleTemplateWidget = Class(TCustomTemplateWidget)
  Private
    FTemplate: String;
    procedure SetTemplate(AValue: String);
  Protected
    function GetTemplateHTML: String; override;
  Public
    Property Reference;
    Property References;
    Property Elements;
    Property ElementList;
  Published
    // The template.
    Property Template : String Read FTemplate Write SetTemplate;
    Property ContainerTag;
  end;


  TTemplateWidget = Class(TCustomTemplateWidget)
  Private
    FTemplate: TStrings;
    procedure DoTemplateChanged(Sender: TObject);
    procedure SetTemplate(AValue: TStrings);
  Protected
    function GetTemplateHTML: String; override;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Property Reference;
    Property References;
    Property Elements;
    Property ElementList;
  Published
    // The template.
    Property Template : TStrings Read FTemplate Write SetTemplate;
    Property ContainerTag;
  end;

  { TCustomLoopTemplateWidget }

  { TLoopTemplateValue }

  TLoopTemplateValue = Class
  Public
    Index : Integer;
    Name : String;
    Value : String;
  end;
  TGetLoopTemplateValueEvent = Procedure (Sender : TObject; aValue : TLoopTemplateValue) of object;

  { TLoopTemplateGroup }

  TLoopTemplateGroup = class(TCollectionItem)
  private
    FGroupValue : string;
    FFooterTemplate: String;
    FHeaderTemplate: String;
    FName: string;
    procedure SetFooterTemplate(AValue: String);
    procedure SetHeaderTemplate(AValue: String);
    procedure SetName(AValue: string);
  Public
    Procedure Assign(Source: TPersistent); override;
  Published
    Property Name : string Read FName Write SetName;
    Property HeaderTemplate : String Read FHeaderTemplate Write SetHeaderTemplate;
    Property FooterTemplate : String Read FFooterTemplate Write SetFooterTemplate;
  end;

  { TLoopTemplateGroupList }

  TLoopTemplateGroupList = class(TOwnedCollection)
  private
    function GetG(aIndex : Integer): TLoopTemplateGroup;
    procedure SetG(aIndex : Integer; AValue: TLoopTemplateGroup);
  Protected
    procedure Update(Item: TCollectionItem); override;
  Public
    Function IndexOfGroup(const aName : string) : Integer;
    Function FindGroup(const aName : string) : TLoopTemplateGroup;
    Function GetGroup(const aName : string) : TLoopTemplateGroup;
    Function AddGroup(Const aName,aHeader,aFooter : String) : TLoopTemplateGroup;
    Property Groups [aIndex : Integer] : TLoopTemplateGroup Read GetG Write SetG; default;
  end;

  TCustomLoopTemplateWidget = Class(TCustomTemplateWidget)
  Private
    FFooter: String;
    FGroups: TLoopTemplateGroupList;
    FHeader: String;
    FOnGetValue: TGetLoopTemplateValueEvent;
    FTemplate: String;
    procedure SetFooter(AValue: String);
    procedure SetGroups(AValue: TLoopTemplateGroupList);
    procedure SetHeader(AValue: String);
    procedure SetTemplate(AValue: String);
  Protected
    Type
      { TLoopEnumerator }
      TLoopEnumerator = Class
      private
        FGroup : TLoopTemplateGroup;
        FWidget: TCustomLoopTemplateWidget;
        FIndex : Integer;
        FCurrValues : TLoopTemplateValue;
      public
        constructor Create(AWidget : TCustomLoopTemplateWidget; aValues: TLoopTemplateValue); reintroduce; virtual;
        destructor destroy; override;
        Function GetValue(Const aName : String): String; virtual;
        function MoveNext: Boolean; virtual;
        Property Widget: TCustomLoopTemplateWidget Read FWidget;
        Property Index : Integer Read FIndex;
        Property CurrValues : TLoopTemplateValue Read FCurrValues;
      end;
  Protected
    // Template support
    function GetTemplateHTML: String; override;
    Function RenderTemplate(aEnum : TLoopEnumerator; aTemplate : String) : String; virtual;
    // Grouping support
    function RenderGroupFooters(aStartIndex: Integer; aEnum: TLoopEnumerator): String; virtual;
    function RenderGroupHeaders(aEnum: TLoopEnumerator): String; virtual;
    function GetGroupValue(enum: TLoopEnumerator; aGroupIndex: Integer; aGroup: TLoopTemplateGroup): String; virtual;
    // Create all kinds of helper classes
    Class Function CreateGroups(aOwner : TComponent) : TLoopTemplateGroupList; virtual;
    Class Function CreateCurrValues : TLoopTemplateValue; virtual;
    Function CreateLoopEnumerator (aCurrValues : TLoopTemplateValue) : TLoopEnumerator; virtual; abstract;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Property Elements;
    Property ElementList;
  Protected
    // The templates.
    Property Groups : TLoopTemplateGroupList Read FGroups Write SetGroups;
    Property HeaderTemplate : String Read FHeader Write SetHeader;
    Property ItemTemplate : String Read FTemplate Write SetTemplate;
    Property FooterTemplate : String Read FFooter Write SetFooter;
    Property OnGetValue : TGetLoopTemplateValueEvent Read FOnGetValue Write FOnGetValue;
  end;

  { TSimpleLoopTemplateWidget }

  { TSimpleLoopTemplateGroup }

  TSimpleLoopTemplateGroup = Class(TLoopTemplateGroup)
  private
    FGroupValueTemplate: String;
    procedure SetGroupValueTemplate(AValue: String);
  Published
    Property GroupValueTemplate : String Read FGroupValueTemplate Write SetGroupValueTemplate;
  end;

  TSimpleLoopTemplateWidget = Class(TCustomLoopTemplateWidget)
  private
    FItemCount: Integer;
    FOnGetGroupValue: TGetLoopTemplateValueEvent;
    procedure SetItemCount(AValue: Integer);
  Protected
    Type
      { TSimpleLoopEnumerator }
      TSimpleLoopEnumerator = Class(TLoopEnumerator)
      private
        FMaxCount: Integer;
      public
        function MoveNext: Boolean; override;
        Property MaxCount : Integer Read FMaxCount;
      end;
  Protected
    Function CreateLoopEnumerator(aCurrValues : TLoopTemplateValue)  : TLoopEnumerator; override;
    function GetGroupValue(aEnum: TLoopEnumerator; aGroupIndex: Integer; aGroup: TLoopTemplateGroup): String; override;
    Class Function CreateGroups(aOwner : TComponent) : TLoopTemplateGroupList; override;
  Published
    Property ItemCount : Integer Read FItemCount Write SetItemCount;
    Property Groups;
    Property HeaderTemplate;
    Property ItemTemplate;
    Property FooterTemplate;
    Property OnGetValue;
    Property OnGetGroupValue : TGetLoopTemplateValueEvent Read FOnGetGroupValue Write FOnGetGroupValue;
    Property References;
    Property ContainerTag;
  end;


  { TListLoopTemplateWidget }

  TListLoopTemplateWidget = Class(TCustomLoopTemplateWidget)
  Public
    Type
      TListKind = (lkCollection,lkFPList,lkList,lkObjectArray,lkJSArray);
      TValueMode = (vmRTTI,vmProperty);
    Const
      ListKindNames : Array[TlistKind] of string = ('Collection','FPList','List','ObjectArray','JSArray');
  private
    FListKind: TListKind;
    FList : JSValue;
    procedure CheckList(aKind: TListKind);
    function GetCollection: TCollection;
    function GetFPList: TFPList;
    function GetJSArray: TJSArray;
    function GetList: TList;
    function GetObjectArray: TObjectDynArray;
    function GetValueMode: TValueMode;
    procedure SetCollection(AValue: TCollection);
    procedure SetFPList(AValue: TFPList);
    procedure SetJSArray(AValue: TJSArray);
    procedure SetList(AValue: TList);
    procedure SetObjectArray(AValue: TObjectDynArray);
  Protected
    Type
      { TListLoopEnumerator }
      TListLoopEnumerator = Class(TLoopEnumerator)
      private
        FArray : TJSArray;
        FCurrent : JSValue;
      public
        function MoveNext: Boolean; override;
        Property Current : JSValue Read FCurrent;
      end;
      { TRTTIListLoopEnumerator }
      TRTTIListLoopEnumerator = Class(TListLoopEnumerator)
      public
        function GetValue(const aName: String): String; override;
      end;
      { TPropListLoopEnumerator }
      TPropListLoopEnumerator = Class(TListLoopEnumerator)
      public
        function GetValue(const aName: String): String; override;
      end;
  Protected
    // Return JS array from list
    Function GetArray : TJSArray;virtual;
    Function CreateLoopEnumerator(aCurrValues : TLoopTemplateValue) : TLoopEnumerator; override;
  Public
    Property Collection : TCollection Read GetCollection Write SetCollection;
    Property FPList : TFPList Read GetFPList Write SetFPList;
    Property List : TList Read GetList Write SetList;
    Property ObjectArray : TObjectDynArray Read GetObjectArray Write SetObjectArray;
    Property JSArray : TJSArray Read GetJSArray Write SetJSArray;
    // What kind of list do we have
    Property ListKind : TListKind Read FListKind;
    // How do we get values from the objects.
    Property ValueMode : TValueMode Read GetValueMode;
  Published
    Property HeaderTemplate;
    Property ItemTemplate;
    Property FooterTemplate;
    Property OnGetValue;
  end;

implementation

uses Strutils, TypInfo;

ResourceString
   SErrCannotSetParentAndElementID = 'ElementID and ParentID cannot be set at the same time.';
   SErrCannotRenderWithoutParent = '%s: Cannot render without parent';
   SErrInvalidChildIndex = 'Invalid child index: value %d is not in valid range [0..%d]';
   SErrUnknownStyle = 'Unknown style: %s';
   SErrElementIDNotAllowed = '%s: Setting element ID is not allowed';
   SErrParentIDNotAllowed = '%s: Setting parent ID is not allowed';
   SErrParentNotAllowed = '%s: Setting parent is not allowed';
   SErrChildrenNotAllowed = '%s: Parent does not allow children';
   SErrWidgetNotFound = 'Widget with ID "%s" not found.';
   SErrUnknownReference = 'Unknown reference: %s';
   SErrNoElement = '%s: Element reference is empty: "%s"';
   SErrNotRendered = 'Cannot perform this operation: Widget not rendered';
   SErrCannotRefreshNoWidget = 'Cannot refresh references without widget';
   SErrNotInput = 'Reference %s is not an input element';
   SErrListNotA = '%s: List is not a %';
   SErrUnknownTemplateGroup = 'Unknown template group item: "%s"';
   SErrDuplicateTemplateGroup = 'Duplicate template group item: "%s"';
   SErrNoGroupInNonGroupTemplate = 'Group name can only be used group value template';

{ TSimpleLoopTemplateGroup }

procedure TSimpleLoopTemplateGroup.SetGroupValueTemplate(AValue: String);
begin
  if FGroupValueTemplate=AValue then Exit;
  FGroupValueTemplate:=AValue;
  Changed(False);
end;

{ TLoopTemplateGroupList }

function TLoopTemplateGroupList.GetG(aIndex : Integer): TLoopTemplateGroup;
begin
  Result:=TLoopTemplateGroup(Items[aIndex])
end;

procedure TLoopTemplateGroupList.SetG(aIndex : Integer; AValue: TLoopTemplateGroup);
begin
  Items[aIndex]:=aValue;
end;

procedure TLoopTemplateGroupList.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Owner is TCustomLoopTemplateWidget then
    With TCustomLoopTemplateWidget(Owner) do
      if IsRendered then
        Refresh;
end;

function TLoopTemplateGroupList.IndexOfGroup(const aName: string): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and Not SameText(GetG(Result).Name,aName) do
    Dec(Result);
end;

function TLoopTemplateGroupList.FindGroup(const aName: string): TLoopTemplateGroup;

Var
  Idx: Integer;

begin
  Idx:=IndexOfGroup(aName);
  if (Idx=-1) then
    Result:=Nil
  else
    Result:=GetG(Idx);
end;

function TLoopTemplateGroupList.GetGroup(const aName: string): TLoopTemplateGroup;
begin
  Result:=FindGroup(aName);
  if (Result=Nil) then
    raise EWidgets.CreateFmt(SErrUnknownTemplateGroup, [aName]);
end;

function TLoopTemplateGroupList.AddGroup(const aName, aHeader, aFooter: String): TLoopTemplateGroup;
begin
  if IndexOfGroup(aName)<>-1 then
    raise EWidgets.CreateFmt(SErrDuplicateTemplateGroup, [aName]);
  Result:=add as TLoopTemplateGroup;
  Result.Name:=aName;
  Result.FFooterTemplate:=aFooter;
  Result.HeaderTemplate:=aHeader;
end;

{ TLoopTemplateGroup }

procedure TLoopTemplateGroup.SetFooterTemplate(AValue: String);
begin
  if FFooterTemplate=AValue then Exit;
  FFooterTemplate:=AValue;
  Changed(False);
end;

procedure TLoopTemplateGroup.SetHeaderTemplate(AValue: String);
begin
  if FHeaderTemplate=AValue then Exit;
  FHeaderTemplate:=AValue;
  Changed(False);
end;

procedure TLoopTemplateGroup.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  if Assigned(Collection) then
    if TLoopTemplateGroupList(Collection).IndexOfGroup(aValue)<>-1 then
      raise EWidgets.CreateFmt(SErrDuplicateTemplateGroup, [aValue]);
  FName:=AValue;
end;

procedure TLoopTemplateGroup.Assign(Source: TPersistent);

Var
  G : TLoopTemplateGroup;

begin
  if Source is TLoopTemplateGroup then
    begin
    G:=Source as TLoopTemplateGroup;
    FName:=G.Name;
    FHeaderTemplate:=G.HeaderTemplate;
    FFooterTemplate:=G.FooterTemplate;
    end
  else
    inherited Assign(Source);
end;

{ TListLoopTemplateWidget.TPropListLoopEnumerator }

function TListLoopTemplateWidget.TPropListLoopEnumerator.GetValue(const aName: String): String;

Var
  V : JSValue;

begin
  V:=TJSObject(Current)[aName];
  if IsDefined(V) then
    Result:=String(V);
end;

{ TListLoopTemplateWidget.TRTTIListLoopEnumerator }

function TListLoopTemplateWidget.TRTTIListLoopEnumerator.GetValue(const aName: String): String;

Const
  AllowedTypes = [tkInteger,tkChar, tkString, tkEnumeration, tkSet,tkDouble, tkBool, tkJSValue];

Var
  O : TObject;
  MP : TTypeMemberProperty;

begin
  O:=TObject(Current);
  MP:=GetPropInfo(O,aName,AllowedTypes);
  if Assigned(Mp) then
    case MP.TypeInfo.Kind of
     tkInteger : Result:=IntToStr(GetOrdProp(O,MP));
     tkChar,
     tkString : Result:=GetStrProp(O,MP);
     tkEnumeration : Result:=GetEnumName(TTypeInfoEnum(MP.TypeInfo),GetOrdProp(O,MP));
     tkSet : Result:=GetSetProp(O,MP);
     tkDouble : Result:=FloatToStr(GetFloatProp(O,MP));
     tkBool : Result:=BoolToStr(GetBoolProp(O,MP));
     tkJSValue : Result:=String(GetJSValueProp(O,MP));
    end;
end;

{ TListLoopTemplateWidget.TListLoopEnumerator }

function TListLoopTemplateWidget.TListLoopEnumerator.MoveNext: Boolean;
begin
  Result:=Index<FArray.Length-1;
  Result:=inherited MoveNext;
  if Result then
    FCurrent:=FArray[Index];
end;

{ TListLoopTemplateWidget }


procedure TListLoopTemplateWidget.CheckList(aKind: TListKind);

begin
  If FListKind<>aKind then
    Raise EWidgets.CreateFmt(SErrListNotA,[DisplayElementName,ListKindNames[aKind]]);
end;

function TListLoopTemplateWidget.GetCollection: TCollection;
begin
  CheckList(lkCollection);
  Result:=TCollection(FList)
end;

function TListLoopTemplateWidget.GetFPList: TFPList;
begin
  CheckList(lkFPList);
  Result:=TFPList(FList)
end;

function TListLoopTemplateWidget.GetJSArray: TJSArray;
begin
  CheckList(lkJSArray);
  Result:=TJSArray(FList)
end;

function TListLoopTemplateWidget.GetList: TList;
begin
  CheckList(lkList);
  Result:=TList(FList)
end;

function TListLoopTemplateWidget.GetObjectArray: TObjectDynArray;
begin
  CheckList(lkObjectArray);
  Result:=TObjectDynArray(FList);
end;


function TListLoopTemplateWidget.GetValueMode: TValueMode;
begin
  if ListKind in [lkCollection,lkFPList,lkList,lkObjectArray] then
    Result:=vmRTTI
  else
    Result:=vmProperty;
end;

procedure TListLoopTemplateWidget.SetCollection(AValue: TCollection);
begin
  Flist:=aValue;
  FListKind:=lkCollection;
end;

procedure TListLoopTemplateWidget.SetFPList(AValue: TFPList);
begin
  Flist:=aValue;
  FListKind:=lkFPList;
end;

procedure TListLoopTemplateWidget.SetJSArray(AValue: TJSArray);
begin
  FList:=AValue;
  FListKind:=lkJSArray;
end;

procedure TListLoopTemplateWidget.SetList(AValue: TList);
begin
  Flist:=aValue;
  FListKind:=lkList;
end;

procedure TListLoopTemplateWidget.SetObjectArray(AValue: TObjectDynArray);
begin
  FListKind:=lkObjectArray;
  FList:=aValue;
end;


function TListLoopTemplateWidget.GetArray: TJSArray;

Var
  I : integer;
  C : TCollection;
begin
  Case FListKind of
    lkCollection :
      begin
      C:=TCollection(Flist);
      Result:=TJSArray.New(C.Count);
      for I:=0 To C.Count-1 do
        Result[i]:=C.Items[i];
      end;
    lkFPList:
      Result:=TJSArray(TFPList(FList).List);
    lkList:
      Result:=TJSArray(TList(FList).List);
    lkJSArray,
    lkObjectArray :
      Result:=TJSArray(FList);
    end;
end;

function TListLoopTemplateWidget.CreateLoopEnumerator(aCurrValues : TLoopTemplateValue) : TLoopEnumerator;

begin
  Case ValueMode of
    vmRTTI     : Result:=TRTTIListLoopEnumerator.Create(Self,aCurrValues);
    vmProperty : Result:=TPropListLoopEnumerator.Create(Self,aCurrValues);
  end;
  TListLoopEnumerator(Result).FArray:=GetArray;
end;

{ TSimpleLoopTemplateWidget.TSimpleLoopEnumerator }

function TSimpleLoopTemplateWidget.TSimpleLoopEnumerator.MoveNext: Boolean;
begin
  Result:=(Index<MaxCount-1);
  if Result then
    Inherited MoveNext;
end;

{ TSimpleLoopTemplateWidget }

procedure TSimpleLoopTemplateWidget.SetItemCount(AValue: Integer);
begin
  if FItemCount=AValue then Exit;
  FItemCount:=AValue;
  if IsRendered then
    Refresh;
end;

function TSimpleLoopTemplateWidget.CreateLoopEnumerator(aCurrValues : TLoopTemplateValue) : TLoopEnumerator;

Var
  L : TSimpleLoopEnumerator;

begin
  L:=TSimpleLoopEnumerator.Create(Self,aCurrValues);
  L.FMaxCount:=Self.ItemCount;
  Result:=L;
end;

function TSimpleLoopTemplateWidget.GetGroupValue(aEnum: TLoopEnumerator; aGroupIndex: Integer; aGroup: TLoopTemplateGroup): String;
begin
  aEnum.CurrValues.Name:=aGroup.Name;
  if TSimpleLoopTemplateGroup(aGroup).GroupValueTemplate<>'' then
    aEnum.FCurrValues.Value:=RenderTemplate(aEnum,TSimpleLoopTemplateGroup(aGroup).GroupValueTemplate);
  if Assigned(OnGetGroupValue) then
    OnGetGroupValue(Self,aEnum.CurrValues);
  Result:=aEnum.CurrValues.Value;
  // Writeln('Group Value: ',aEnum.CurrValues.Value,' for ',aGroup.Name);
end;

class function TSimpleLoopTemplateWidget.CreateGroups(aOwner: TComponent): TLoopTemplateGroupList;
begin
  Result:=TLoopTemplateGroupList.Create(aOwner,TSimpleLoopTemplateGroup)
end;

{ TCustomLoopTemplateWidget }

procedure TCustomLoopTemplateWidget.SetTemplate(AValue: String);
begin
  if FTemplate=aValue then exit;
  FTemplate:=aValue;
  if IsRendered then
    Refresh;
end;

Class function TCustomLoopTemplateWidget.CreateGroups(aOwner : TComponent): TLoopTemplateGroupList;
begin
  Result:=TLoopTemplateGroupList.Create(AOwner,TLoopTemplateGroup);
end;

Class function TCustomLoopTemplateWidget.CreateCurrValues: TLoopTemplateValue;
begin
  Result:=TLoopTemplateValue.Create;
end;

function TCustomLoopTemplateWidget.RenderTemplate(aEnum: TLoopEnumerator; aTemplate: String): String;

Var
  E : TJSRegexp;

begin
  E:=TJSRegexp.New('{{([_\w]*)}}','g');
  Result:=TJSString(aTemplate).Replace(E,Function (Const match,p1 : string; offset : Integer; AString : String) : string
  begin
    aEnum.CurrValues.Value:=aEnum.GetValue(p1);
    // Writeln(p1,' -> ',aEnum.CurrValues.Value);
    if Assigned(OnGetValue) then
      begin
      aEnum.CurrValues.Index:=aEnum.Index;
      aEnum.CurrValues.Name:=P1;
      OnGetValue(Self,aEnum.CurrValues);
      end;
    Result:=AEnum.CurrValues.Value;
  end);
end;

procedure TCustomLoopTemplateWidget.SetFooter(AValue: String);
begin
  if FFooter=AValue then Exit;
  FFooter:=AValue;
  if IsRendered then
    Refresh;
end;

procedure TCustomLoopTemplateWidget.SetGroups(AValue: TLoopTemplateGroupList);
begin
  if FGroups=AValue then Exit;
  FGroups.Assign(AValue);
  if IsRendered then
    Refresh;
end;

procedure TCustomLoopTemplateWidget.SetHeader(AValue: String);
begin
  if FHeader=AValue then Exit;
  FHeader:=AValue;
  if IsRendered then
    Refresh;
end;

function TCustomLoopTemplateWidget.GetGroupValue(enum : TLoopEnumerator; aGroupIndex : Integer; aGroup : TLoopTemplateGroup) : String;

begin
  Result:=aGroup.Name;
end;

function TCustomLoopTemplateWidget.RenderGroupHeaders(aEnum : TLoopEnumerator) : String;

Var
  GrpIdx: Integer;
  grp : TLoopTemplateGroup;
  StartGroups : Boolean;
  S,V : String;

begin
//  Writeln('Rendering group headers for row ',aEnum.Index);
  Result:='';
  StartGroups:=False;
  For GrpIdx:=0 to Groups.Count-1 do
    begin
    Grp:=Groups[GrpIdx];
    aEnum.FGroup:=Grp;
    V:=GetGroupValue(aEnum,GrpIdx,Grp);
    if Not StartGroups then
      begin
      StartGroups:=(aEnum.Index=0) or (V<>Grp.FGroupValue);
      if StartGroups and (aEnum.Index>0) then
        Result:=Result+RenderGroupFooters(grpIdx,aEnum);
      end;
    Grp.FGroupValue:=V;
    if StartGroups then
      begin
      S:=RenderTemplate(aEnum,Grp.HeaderTemplate);
      // Writeln('Rendering group ',Grp.Name,' (',V,') header template: ',Grp.HeaderTemplate,' : ',S);
      Result:=Result+S;
      end;
    aEnum.FGroup:=Nil;
    end;
end;

function TCustomLoopTemplateWidget.RenderGroupFooters(aStartIndex : Integer; aEnum : TLoopEnumerator) : String;


Var
  GrpIdx : Integer;
  grp : TLoopTemplateGroup;

begin
  Result:='';
  For GrpIdx:=Groups.Count-1 downto aStartIndex do
    begin
    Grp:=Groups[GrpIdx];
    Result:=Result+RenderTemplate(aEnum,Grp.FooterTemplate);
    end;
end;

function TCustomLoopTemplateWidget.GetTemplateHTML: String;

Var
  Enum : TLoopEnumerator;

begin
  Enum:=CreateLoopEnumerator(CreateCurrValues);
  try
    Result:=RenderTemplate(Enum,HeaderTemplate);
    While enum.MoveNext do
      begin
      if Groups.Count>0 then
        Result:=Result+RenderGroupHeaders(Enum);
      Result:=Result+RenderTemplate(Enum,ItemTemplate);
      end;
    if Groups.Count>0 then
      Result:=Result+RenderGroupFooters(0,Enum);
    Result:=Result+RenderTemplate(Enum,FooterTemplate);
  finally
    Enum.Free;
  end;
end;

constructor TCustomLoopTemplateWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FGroups:=CreateGroups(Self);
end;

destructor TCustomLoopTemplateWidget.Destroy;
begin
  FreeAndNil(FGroups);
  inherited Destroy;
end;


{ TCustomLoopTemplateWidget.TLoopEnumerator }

constructor TCustomLoopTemplateWidget.TLoopEnumerator.Create(AWidget: TCustomLoopTemplateWidget; aValues: TLoopTemplateValue);
begin
  FWidget:=AWidget;
  FCurrValues:=aValues;
  FIndex:=-1;
end;

destructor TCustomLoopTemplateWidget.TLoopEnumerator.destroy;
begin
  FreeAndNil(FCurrValues);
  inherited destroy;
end;

function TCustomLoopTemplateWidget.TLoopEnumerator.GetValue(const aName: String): String;
begin
  Case AName of
   '_index_' : Result:=IntToStr(Index);
   '_row_' : Result:=IntToStr(Index+1);
   '_group_' : if Assigned(FGroup) then
                 Result:=FGroup.FGroupValue
               else
                 Raise EWidgets.Create(SErrNoGroupInNonGroupTemplate);
  else
    Result:='';
  end;
end;

function TCustomLoopTemplateWidget.TLoopEnumerator.MoveNext: Boolean;
begin
  Result:=True;
  Inc(FIndex);
  FCurrValues.Index:=FIndex;
end;

{ TSimpleTemplateWidget }

procedure TSimpleTemplateWidget.SetTemplate(AValue: String);

begin
  FTemplate:=AValue;
  if isRendered then
    Refresh;
end;

function TSimpleTemplateWidget.GetTemplateHTML: String;
begin
  Result:=FTemplate;
end;


{ TWebWidgetReferences }

function TWebWidgetReferences.GetReferenceItem(aIndex : Integer): TReferenceItem;
begin
  Result:=TReferenceItem(Inherited Items[aIndex])
end;

procedure TWebWidgetReferences.SetReferenceItem(aIndex : Integer; AValue: TReferenceItem);
begin
  Items[aIndex]:=aValue;
end;

procedure TWebWidgetReferences.MarkDirty(aItem: TReferenceItem);
begin
  if Assigned(Widget) and Assigned(Widget.Element) then
    RefreshFromDOM(aItem,Widget.GetReferenceElement)
end;

procedure TWebWidgetReferences.RefreshFromDOM(aItem: TReferenceItem; aElement: TJSHTMlElement);

Var
  a : TJSHTMlElementArray;
  Nodes : TJSNodeList;
  I : integer;

begin
  if aElement=Nil then
    begin
    if (Widget=Nil) then
      Raise EWidgets.Create(SErrCannotRefreshNoWidget);
    if (Widget.Element=Nil) then
      Raise EWidgets.Create(SErrNotRendered);
    aElement:=Widget.GetReferenceElement;
    end;
  if FRefs=Nil then
    FRefs:=New([]);
  try
    Nodes:=aElement.querySelectorAll(aItem.Selector);
    SetLength(a,Nodes.length);
    For I:=0 to Nodes.length-1 do
      A[i]:=TJSHTMLElement(Nodes[i]);
  except
    SetLength(a,0);
  end;
  FRefs[LowerCase(aItem.Name)]:=A;
end;

function TWebWidgetReferences.Widget: TCustomWebWidget;
begin
  Result:=TCustomWebWidget(owner);
end;

function TWebWidgetReferences.Add(const aName: String; aSelector: String): TReferenceItem;
begin
  Result:=Add as TReferenceItem;
  Result.FName:=aName;
  Result.Selector:=aSelector;
  if (aSelector<>'') then
    MarkDirty(Result)
end;

function TWebWidgetReferences.EnsureReference(const aName: String; const aSelector: String): TReferenceItem;
begin
  Result:=FindReference(aName);
  if Result=Nil then
    Result:=Add(aName,aSelector);
end;

function TWebWidgetReferences.IndexOfReference(const aName: String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and not SameText(GetReferenceItem(Result).Name,aName) do
    Dec(Result);
end;

function TWebWidgetReferences.FindReference(const aName: String): TReferenceItem;

Var
  Idx:Integer;

begin
  Idx:=IndexOfReference(aName);
  if Idx=-1 then
    Result:=Nil
  else
    Result:=GetReferenceItem(Idx)
end;

function TWebWidgetReferences.GetReference(const aName: String): TReferenceItem;
begin
  Result:=FindReference(aName);
  if (Result=Nil) then
    Raise EWidgets.CreateFmt(SErrUnknownReference,[aName]);
end;

procedure TWebWidgetReferences.RemoveReference(const aName: String);
Var
  Idx:Integer;

begin
  Idx:=IndexOfReference(aName);
  if Idx<>-1 then
    Delete(Idx);
end;

function TWebWidgetReferences.ElementExists(const aName: String): Boolean;

Var
  V : JSValue;
begin
  Result:=Assigned(FRefs);
  if Result then
    begin
    V:=FRefs[LowerCase(aName)];
    Result:=Assigned(V);
    end;
end;

function TWebWidgetReferences.ElementIsArray(const aName: String): Boolean;
Var
  V : JSValue;
begin
  Result:=Assigned(FRefs);
  if Result then
    begin
    V:=FRefs[LowerCase(aName)];
    Result:=isArray(V) and (TJSArray(V).Length>1);
    end;
end;

function TWebWidgetReferences.FindElementByName(const aName: String): TJSHTMLElement;
Var
  J : JSValue;
  Arr : TJSArray absolute J;

begin
  Result:=Nil;
  if FRefs=Nil then
    exit;
  J:=FRefs[LowerCase(aName)];
  if isArray(J) and (Arr.Length>0) then
    Result:=TJSHTMLElement(Arr[0])
end;

function TWebWidgetReferences.GetElementByName(const aName: String): TJSHTMLElement;

begin
  Result:=FindElementByName(aName);
  if Result=Nil then
    Raise EWidgets.CreateFmt(SErrNoElement,[Widget.DisplayElementName,aName]);
end;

function TWebWidgetReferences.GetElementsByName(const aName: String): TJSHTMLElementArray;
Var
  J : JSValue;
  Arr : TJSArray absolute J;

begin
  Result:=Nil;
  if FRefs=Nil then
    exit;
  J:=FRefs[LowerCase(aName)];
  if isArray(J) and (Arr.Length>0) then
    Result:=TJSHTMLElementArray(Arr)
end;

procedure TWebWidgetReferences.RefreshFromDOM(aElement: TJSHTMlElement);

Var
  I : Integer;

begin
  For I:=0 to Count-1 do
    RefreshFromDOM(GetReferenceItem(I),aElement);
end;

{ TReferenceItem }

procedure TReferenceItem.SetName(AValue: String);
begin
  if FName=AValue then Exit;
  FName:=AValue;
  MarkDirty;
end;

procedure TReferenceItem.SetInputValidity(AValue: String);
begin
  if IsInput then
    // this is not quite correct, but will work
    TJSHTMLInputElement(Element).setCustomValidity(aValue)
  else
    Raise EWidgets.CreateFmt(SErrNotInput,[Name]);
end;

function TReferenceItem.GetElement: TJSHTMLElement;
begin
  if Assigned(Collection) then
    Result:=(Collection as TWebWidgetReferences).GetElementByName(Name)
  else
    Result:=Nil;
end;

function TReferenceItem.GetElements: TJSHTMLElementArray;
begin
  if Assigned(Collection) then
    Result:=(Collection as TWebWidgetReferences).GetElementsByName(Name)
  else
    Result:=Nil;
end;

procedure TReferenceItem.SetInputValue(AValue: String);
begin
  if IsInput then
    TJSObject(Element)['value']:=aValue
  else
    Raise EWidgets.CreateFmt(SErrNotInput,[Name])
end;

procedure TReferenceItem.MarkDirty;
begin
  if Assigned(Collection)  then
    (Collection as TWebWidgetReferences).MarkDirty(Self);
end;

procedure TReferenceItem.Refresh;
begin
  MarkDirty;
end;

function TReferenceItem.Exists: Boolean;
begin
  if Assigned(Collection) then
    Result:=(Collection as TWebWidgetReferences).ElementExists(Name)
  else
    Result:=False;
end;

function TReferenceItem.IsArray: Boolean;
begin
  if Assigned(Collection) then
    Result:=(Collection as TWebWidgetReferences).ElementIsArray(Name)
  else
    Result:=False;
end;

function TReferenceItem.IsInput: Boolean;

Var
  el : TJSHTMLELement;

begin
  Result:=Exists and not IsArray;
  if Result then
    begin
    el:=Element;
    Result:= ((el is TJSHTMLInputElement)
               or (el is TJSHTMLTextAreaElement)
               or (el is TJSHTMLSelectElement));
    end;
end;

function TReferenceItem.GetInputValue: String;
begin
  if IsInput then
    Result:=String(TJSObject(Element)['value'])
  else
    Raise EWidgets.CreateFmt(SErrNotInput,[Name])
end;

procedure TReferenceItem.SetSelector(AValue: String);
begin
  if FSelector=AValue then Exit;
  FSelector:=AValue;
  MarkDirty;
end;

{ TCustomTemplateWidget }

procedure TCustomTemplateWidget.ApplyTemplate(aElement: TJSHTMLElement);
begin
  aElement.InnerHTML:=GetTemplateHTML;
end;

function TCustomTemplateWidget.DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement;
begin
  if (ContainerTag='') then
    begin
    ApplyTemplate(AParent);
    Result:= TJSHTMLElement(aParent.firstElementChild);
    end
  else
    begin
    ApplyTemplate(aElement);
    Result:=aElement;
    end;
end;

procedure TCustomTemplateWidget.SetContainerTag(AValue: String);
begin
  if FContainerTag=AValue then Exit;
  FContainerTag:=AValue;
  if IsRendered then
    Refresh;
end;

function TCustomTemplateWidget.GetReferenceElement: TJSHTMLELement;
begin
  If (ContainerTag='') then
    Result:=ParentElement
  else
    Result:=Element;
end;

function TCustomTemplateWidget.HTMLTag: String;
begin
  Result:=ContainerTag;
end;

{ TTemplateWidget }

procedure TTemplateWidget.SetTemplate(AValue: TStrings);
begin
  if FTemplate=AValue then Exit;
  FTemplate.Assign(AValue);
end;

function TTemplateWidget.GetTemplateHTML: String;
begin
  Result:=FTemplate.Text;
end;


procedure TTemplateWidget.DoTemplateChanged(Sender: TObject);
begin
  if isRendered then
    Refresh;
end;

constructor TTemplateWidget.Create(aOwner: TComponent);

begin
  inherited Create(aOwner);
  FTemplate:=TStringList.Create;
  TStringList(FTemplate).OnChange:=@DoTemplateChanged;
  FContainerTag:='';
end;

destructor TTemplateWidget.Destroy;

begin
  FreeAndNil(FTemplate);
  inherited Destroy;
end;

{ TContainerWidget }

function TContainerWidget.GetKnownStyle(AIndex: Integer): String;

var
  S : TStyleItem;
begin
  S:=Styles.FindStyle(KnownStyles[aIndex]);
  if Assigned(S) then
    Result:=S.Value;
end;

procedure TContainerWidget.SetKnownStyle(AIndex: Integer; AValue: String);

begin
  Styles.EnsureStyle(KnownStyles[aIndex]).Value:=aValue;
end;

function TContainerWidget.HTMLTag: String;
begin
  Result:='div';
end;

constructor TContainerWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  MinWidth:='32px';
  MinHeight:='12px';
  Width:='50%';
  Height:='50%';
end;



{ TWebWidgetStyles }

function TWebWidgetStyles.GetStyleItem(aIndex: Integer): TStyleItem;
begin
  Result:=TStyleItem(Items[Aindex]);
end;

procedure TWebWidgetStyles.SetStyleItem(aIndex: Integer; AValue: TStyleItem);
begin
  Items[aIndex]:=aValue;
end;

procedure TWebWidgetStyles.MarkDirty(aItem: TStyleItem);

Var
  El : TJSHTMLElement;

begin
  If Assigned(Widget) then
    begin
    el:=Widget.Element;
    if Assigned(El) then
      ApplyToDom(El,aItem);
    end;
end;

procedure TWebWidgetStyles.ApplyToDOM(aElement: TJSHTMlElement; aItem: TStyleItem);

Const
  Prios : Array[TStylePriority] of string = ('','important');

begin
  With AItem do
    if (Name<>'') then
      if (Value<>'') then
        aElement.Style.setProperty(Name,Value,Prios[Priority])
      else
        aElement.Style.removeProperty(name);
end;

function TWebWidgetStyles.Add(const aName: String; const aValue: String): TStyleItem;
begin
  Result:=Add as TStyleItem;
  // Don't use Name
  Result.FName:=aName;
  if aValue<>'' then
    Result.Value:=aValue; // will trigger markdirty
end;

function TWebWidgetStyles.EnsureStyle(const aName: String; const aValue: String): TStyleItem;
begin
  Result:=FindStyle(aName);
  if Result=Nil then
    Result:=Add(aName,aValue)
  else if AValue<>'' then
    Result.Value:=aValue
end;

function TWebWidgetStyles.Widget: TCustomWebWidget;
begin
  Result:=TCustomWebWidget(Owner);
end;

function TWebWidgetStyles.IndexOfStyle(const aName: String): integer;

begin
  Result:=Count-1;
  While (Result>=0) and not SameText(aName,GetStyleItem(Result).Name) do
    Dec(Result);
end;

function TWebWidgetStyles.FindStyle(const aName: String): TStyleItem;

Var
  Idx : integer;

begin
  Idx:=IndexOfStyle(aName);
  If Idx=-1 then
    Result:=Nil
  else
    Result:=GetStyleItem(Idx)
end;

function TWebWidgetStyles.GetStyle(const aName: String): TStyleItem;
begin
  Result:=FindStyle(aName);
  if Result=Nil then
    Raise EWidgets.CreateFmt(SErrUnknownStyle,[aName]);
end;

function TWebWidgetStyles.RemoveStyle(const aName: String): String;

Var
  I : Integer;
  el : TJSHTMLElement;

begin
  I:=IndexOfStyle(aName);
  if I<>-1 then
    begin
    Result:=Styles[i].Value;
    Delete(I);
    end;
  if Assigned(Widget)  then
    begin
    el:=Widget.Element;
    if Assigned(el) then
      begin
      if (Result='') then
        Result:=el.style.getPropertyValue(aName);
      el.style.removeProperty(aName);
      end;
    end;
end;

procedure TWebWidgetStyles.RefreshFromDOM(aElement : TJSHTMlElement = Nil;DoClear: Boolean = False);

Var
  S : TJSCSSStyleDeclaration;
  I : integer;
  K : String;
  W : TCustomWebWidget;
  itm : TStyleItem;

begin
  if aElement= Nil then
    begin
    W:=Widget;
    if assigned(W) then
      aElement:=W.Element;
    if AElement=Nil then exit;
    end;
  if DoClear then
    Clear;
  S:=aElement.style;
  For I:=0 to S.length-1 do
    begin
    K:=S.Item(I);
    itm:=FindStyle(K);
    if Itm=Nil then
      begin
      Itm:=Add(K);
      Itm.FImported:=True;
      end;
    Itm.FValue:=S.getPropertyValue(K);
    Case LowerCase(S.getPropertyPriority(K)) of
     'important' : Itm.FPriority:=spImportant;
    end;
    end;
end;

procedure TWebWidgetStyles.ClearImported;

Var
  I : integer;

begin
  I:=Count-1;
  While I>=0 do
    begin
    If GetStyleItem(I).Fimported then
      Delete(I);
    Dec(I);
    end;
end;

procedure TWebWidgetStyles.ApplyToDOM(aElement : TJSHTMlElement = Nil);

Var
  I : Integer;

begin
  if (AElement=Nil) and (Widget<>Nil) then
    aElement:=Widget.Element;
  if AElement<>Nil then
    For I:=0 to Count-1 do
      ApplyToDOM(aElement,GetStyleItem(i));
end;

{ TStyleItem }

procedure TStyleItem.MarkDirty;

begin
  If Assigned(Collection) then
   TWebWidgetStyles(Collection).MarkDirty(Self);
end;

procedure TStyleItem.SetValue(AValue: String);
begin
  if FValue=AValue then Exit;
  FValue:=aValue;
  MarkDirty;
end;

procedure TStyleItem.SetPriority(AValue: TStylePriority);
begin
  if FPriority=AValue then Exit;
  FPriority:=AValue;
  MarkDirty;
end;


procedure TStyleItem.SetName(AValue: String);
begin
  if aValue=FName then Exit;
  FName:=AValue;
  MarkDirty;
end;

procedure TStyleItem.Assign(Source: TPersistent);

Var
  SI : TStyleItem;

begin
  if Source is TStyleItem then
    begin
    SI:=Source as TStyleItem;
    FName:=SI.FName;
    FValue:=SI.FValue;
    FImported:=SI.FImported;
    MarkDirty;
    end
  else
    inherited Assign(Source);
end;


{ TCustomWebWidget }

function TCustomWebWidget.DisplayElementName: String;

begin
  Result:=Name;
  If Result='' then
    Result:=' <'+HTMLTag+'>';
  if Assigned(FElement) then
    Result:=Result+'#'+FElement.ID;
  Result:=Result+' (Type: '+ClassName+')';
end;

function TCustomWebWidget.EnsureElement : TJSHTMLElement;

var
  P : TJSHTMLElement;

begin
  Result:=GetElement;
  if Result=Nil then
    begin
    // If we have a parent, make sure it has it's element
    if Assigned(Parent) then
       Parent.EnsureElement;
    P:=ParentElement;
    if (P=Nil) and (FixedElement=Nil) then
      Raise EWidgets.CreateFmt(SErrCannotRenderWithoutParent,[DisplayElementName])
    else
      begin
      Result:=RenderHTML(P);
      FOwnsElement:=True;
      FElement:=Result;
      end;
    ApplyData;
    RefreshReferences; // After data, so data can be used in selectors
    end;
end;

procedure TCustomWebWidget.InvalidateParentElement;

Var
  I : Integer;
  C : TCustomWebWidget;

begin
  FParentElement:=nil;
  For I:=0 to ChildCount-1 do
    begin
    C:=Children[i];
    if Assigned(C) then // Can be Nil
      C.InvalidateParentElement;
    end;
end;

procedure TCustomWebWidget.InvalidateElement;

Var
  I : Integer;
  C : TCustomWebWidget;

begin
  If FStyles.Count>0 then
    FStyles.ClearImported;
  if Assigned(Freferences) then
    FReferences.FRefs:=Nil;
  FElement:=nil;
  For I:=0 to ChildCount-1 do
    begin
    C:=Children[i];
    if Assigned(C) then // Can be Nil
      C.InvalidateElement;
    end;
end;

function TCustomWebWidget.WidgetClasses: String;
begin
  Result:='';
end;


function TCustomWebWidget.GetElement: TJSHTMLELement;

Var
  El : TJSHTMLElement;


begin
  if (FElement=Nil) then
    begin
    if (FElementID<>'') then
      begin
      El:=FindElement(FElementID);
      if Assigned(El) then
        begin
        ApplyWidgetSettings(el);
        HookupEvents(el);
        end;
      FElement:=El;
      if Assigned(El) then
        ApplyData;
      RefreshReferences;// After data, so data can be used in selectors
      end;
    end;
  Result:=FElement;
end;

function TCustomWebWidget.GetExternalElement: Boolean;
begin
  Result:=(FElementID<>'')
end;

function TCustomWebWidget.GetHaveReferences: Boolean;
begin
  Result:=Assigned(FReferences);
end;

function TCustomWebWidget.GetHTMLEvent(AIndex: Integer): THTMLNotifyEvent;

Var
  Fun : JSValue;
begin
  Result:=nil;
  if Assigned(FMyEvents) and (aIndex>=0) and (aIndex<=MaxEvents) then
    begin
    Fun:=FMyEvents[FEventNames[aindex]];
    if Not isUndefined(Fun) then
      Result:=THTMLNotifyEvent(Fun);
    end;
end;

function TCustomWebWidget.GetIsElementDirty: Boolean;
begin
  Result:=IsRendered;
end;

function TCustomWebWidget.GetClasses: String;
begin
  if IsRendered Then
    FClasses:=FElement.ClassName;
  Result:=FClasses;
end;

function TCustomWebWidget.GetDataset(aName : String): String;

Var
  el : TJSHTMLElement;

begin
  el:=Element;
  if Assigned(El) then
    Result:=String(El.Dataset[aName])
  else
    Result:='';
end;

function TCustomWebWidget.GetChildCount: Integer;
begin
  Result:=FChildren.Length;
end;

function TCustomWebWidget.GetChild(aIndex : Integer): TCustomWebWidget;
begin
  if (aIndex<0) or (aIndex>=FChildren.Length) then
    Raise EListError.CreateFmt(SErrInvalidChildIndex,[aIndex,FChildren.Length-1]);
  Result:=TCustomWebWidget(FChildren[aIndex]);
end;

function TCustomWebWidget.GetContentElement: TJSHTMLELement;
begin
  Result:=Element;
end;

function TCustomWebWidget.GetParent: TCustomWebWidget;
begin
  Result:=FParent;
end;

function TCustomWebWidget.GetParentElement: TJSHTMLELement;

Var
  El : TJSHTMLElement;

begin
  if (FParentElement=Nil) then
    begin
    El:=TopElement;
    if Assigned(el) then
      FParentElement:=TJSHTMLElement(el.parentElement)
    else if (FParentID<>'') then
      FParentElement:=FindElement(FParentID)
    else if Assigned(FParent) then
      FParentElement:=FParent.ContentElement
    else
      FParentElement:=DefaultParentElement;
    end;
  Result:=FParentElement;
end;

function TCustomWebWidget.GetParentID: String;

Var
  E : TJSHTMLElement;

begin
  Result:='';
  E:=ParentElement;
  if Assigned(E) then
    Result:=E.ID
  else
    Result:=FParentID;
end;

function TCustomWebWidget.GetElementID: String;

Var
  El : TJSHTMLElement;

begin
  El:=Element;
  If Assigned(El) then
    Result:=el.ID
  else
    Result:=FElementID;
end;

function TCustomWebWidget.GetReference(const aName : string): TJSHTMLElement;
begin
  if Assigned(FReferences) then
    Result:=FReferences.GetElementByName(aName)
  else
    Result:=Nil;
end;

function TCustomWebWidget.GetReferenceItem(aName : String): TReferenceItem;
begin
  if Assigned(FReferences) then
    Result:=FReferences.GetReference(aName)
  else
    Result:=Nil;
end;

function TCustomWebWidget.GetReferenceList(const aName : string): TJSHTMLElementArray;
begin
  if Assigned(FReferences) then
    Result:=FReferences.GetElementsByName(aName)
  else
    Result:=Nil;
end;

function TCustomWebWidget.GetReferences: TWebWidgetReferences;
begin
  if (FReferences=Nil) then
    FReferences:=CreateReferences;
  Result:=FReferences;

end;

function TCustomWebWidget.GetRendered: Boolean;
begin
  Result:=(FElement<>Nil)
end;

function TCustomWebWidget.GetTopElement: TJSHTMLELement;
begin
  Result:=Element;
end;

function TCustomWebWidget.GetVisible: Boolean;
begin
  Result:=FVisible;
end;

procedure TCustomWebWidget.SetAttr(const aName : string; AValue: String);
begin
  if IsRendered then
    Element[aName]:=aValue;
  if Not Assigned(FAttrs) then
    FAttrs:=TJSObject.New;
  FAttrs[aName]:=aValue;
end;

procedure TCustomWebWidget.SetClasses(AValue: String);
begin
  FClasses:=AddClasses(AValue,WidgetClasses);
  If IsRendered then
    FElement.ClassName:=FClasses;
end;

procedure TCustomWebWidget.SetDataset(aName : String; AValue: String);

Var
  El : TJSHTMLElement;

begin
  el:=Element;
  If (El=Nil) then
    Raise EWidgets.Create(SErrNotRendered);
  el.Dataset[aName]:=aValue;
end;

procedure TCustomWebWidget.SetElementID(AValue: String);
begin
  if (FElementID=AValue) then Exit;
  if (aValue<>'') then
    begin
    if (FParentID<>'') then
      Raise EWidgets.CreateFmt(SErrCannotSetParentAndElementID,[DisplayElementName]);
    if FixedElement<>Nil then
      Raise EWidgets.CreateFmt(SErrElementIDNotAllowed,[DisplayElementName]);
    FElementID:=AValue;
    end
  else
    begin
    FElementID:=AValue;
    if IsRendered then
      Unrender(ParentElement);
    end;
end;

procedure TCustomWebWidget.SetHTMLEvent(AIndex: Integer; AValue: THTMLNotifyEvent);

Var
  EventName : String;

begin
  if (aIndex<0) or (aIndex>MaxEvents) then
    exit;
  EventName:=FEventNames[aIndex];
  if Assigned(aValue) then
    AddEvent(EventName,AValue)
  else
    DeleteEvent(EventName);
end;

procedure TCustomWebWidget.SetParent(AValue: TCustomWebWidget);

Var
  ReRender : Boolean;
begin
  if (AValue=FParent) then exit;
  if (FixedParent<>Nil) then
    Raise EWidgets.CreateFmt(SErrParentNotAllowed,[DisplayElementName]);
  if Assigned(aValue) then
    if Not aValue.AllowChildren then
      Raise EWidgets.CreateFmt(SErrChildrenNotAllowed,[DisplayElementName]);
  If Assigned(FParent) then
    FParent.RemoveChild(Self);
  // Unrender
  ReRender:=IsRendered;
  if ReRender then
    UnRender(ParentElement);
  If (aValue=Nil) and (csDestroying in ComponentState) then
    exit;
  // here we re-render if needed
  InvalidateParentElement;
  If Assigned(aValue) then
    begin
    FParentID:='';
    aValue.AddChild(Self); // Sets FParent
    end;
  if ReRender and Assigned(ParentElement) then
    begin
    FElement:=RenderHTML(ParentElement);
    if Assigned(FElement) then
      begin
      ApplyData;
      RefreshReferences;
      end;
    end;
end;

procedure TCustomWebWidget.SetParentID(AValue: String);

Var
  ReRender : Boolean;

begin
  if (FParentID=AValue) then exit;
  if (aValue<>'') then
    begin
    if (FElementID<>'') then
      Raise EWidgets.CreateFmt(SErrCannotSetParentAndElementID,[DisplayElementName]);
    if (FixedParent<>Nil) then
      Raise EWidgets.CreateFmt(SErrParentIDNotAllowed,[DisplayElementName]);
    end;
  ReRender:=IsRendered;
  if ReRender then
    UnRender(ParentElement);
  if (aValue<>'') and Assigned(FParent) then
    FParent.RemoveChild(Self);
  FParentID:=aValue;
  InvalidateParentElement;
  if ReRender and Assigned(ParentElement) then
    EnsureElement;
end;

procedure TCustomWebWidget.AddChild(aValue: TCustomWebWidget);
begin
  if AValue=Nil then exit;
  aValue.FParent:=Self;
  if FChildren.IndexOf(aValue)=-1 then
    FChildren.Push(aValue);
end;

procedure TCustomWebWidget.RemoveChild(aValue: TCustomWebWidget);

Var
  I : NativeInt;

begin
  if AValue=Nil then exit;
  I:=FChildren.indexOf(aValue);
  if I>=0 then
    begin
    FChildren.splice(I,1);
    aValue.FParent:=Nil;
    end;
end;

procedure TCustomWebWidget.SetReferences(AValue: TWebWidgetReferences);
begin
  if (aValue=FReferences) then exit;
  References.Assign(aValue);
  if IsRendered then
    References.RefreshFromDOM(GetReferenceElement);
end;

procedure TCustomWebWidget.SetStyles(AValue: TWebWidgetStyles);
begin
  if FStyles=AValue then Exit;
  FStyles.Assign(AValue);
end;

procedure TCustomWebWidget.SetVisible(AValue: Boolean);

Var
  el : TJSHTMLElement;

begin
  if aValue=FVisible then
    Exit;
  el:=Element;
  if Assigned(el) then
    ApplyVisible(el,aValue)
  else
    FVisible:=aValue;
end;

function TCustomWebWidget.GetAttr(const aName: String): String;
Var
  el : TJSObject;
begin
  Result:='';
  if IsRendered then
    el:=Element
  else
    el:=FAttrs;
  if Assigned(el) and isDefined(el[aName]) then
    Result:=String(el[aName]);
end;

procedure TCustomWebWidget.ApplyVisible(aElement: TJSHTMLElement;AValue: Boolean);

begin
  if aValue then
    begin
    if (FDisplay<>'') then
      aElement.Style.setProperty('display',FDisplay)
    else
      aElement.Style.removeProperty('display');
    end
  else
    begin
    FDisplay:=aElement.Style.getPropertyValue('display');
    aElement.Style.setProperty('display','none');
    end;
  FVisible:=aValue;
end;

procedure TCustomWebWidget.EventEntry(aEvent: TJSEvent);

Var
  R : TEventDispatch;
  Fun : JSValue;

begin
  R.MsgStr:=aEvent._type;
  R.HTMLEvent:=aEvent;
  if Assigned(FMyEvents) then
    Fun:=FMyEvents[R.MsgStr]
  else
    Fun:=nil;
  if Not (isUndefined(Fun) or isNull(Fun)) then
    R.EventHandler:=THTMLNotifyEvent(Fun);
  DispatchStr(R);
  if (R.EventHandler<>Nil) then
    R.EventHandler(Self,R.HTMLEvent);
end;

function TCustomWebWidget.CreateStyles: TWebWidgetStyles;
begin
  Result:=TWebWidgetStyles.Create(Self,TStyleItem);
end;

function TCustomWebWidget.CreateReferences: TWebWidgetReferences;
begin
  Result:=TWebWidgetReferences.Create(Self,TReferenceItem);
end;

procedure TCustomWebWidget.RemoveEvent(aElement: TJSHTMLElement; const aEvent: String);
begin
  aElement.RemoveEventListener(aEvent,FMyHook);
end;

procedure TCustomWebWidget.HookupEvent(aElement: TJSHTMLElement; const aEvent : String);

begin
  aElement.addEventListener(aEvent,FMyHook);
end;

procedure TCustomWebWidget.HookupEvents(aElement: TJSHTMLElement);

Var
  Event : String;

begin
  if Assigned(FMyEvents) then
    for Event in TJSObject.keys(FMyEvents) do
      HookupEvent(aElement,Event);
end;

procedure TCustomWebWidget.AddEvent(aName: String; AHandler: THTMLNotifyEvent);

Var
  el : TJSHTMLElement;

begin
  if FMyEvents=nil then
    FMyEvents:=TJSObject.New;
  FMyEvents[aName]:=aHandler;
  El:=FElement;
  if Assigned(El) then
    HookupEvent(el,aName);
end;

procedure TCustomWebWidget.DeleteEvent(aName: String);

Var
  el : TJSHTMLElement;

begin
  if (FMyEvents<>nil) and FMyEvents.hasOwnProperty(aName) then
      JSDelete(FMyEvents,aName);
  El:=Element;
  if Assigned(El) then
    RemoveEvent(el,aName);
end;

class function TCustomWebWidget.FixedParent: TJSHTMLElement;
begin
  Result:=Nil;
end;

class function TCustomWebWidget.DefaultParentElement: TJSHTMLElement;
begin
  Result:=Nil;
end;

class function TCustomWebWidget.DefaultParent: TCustomWebWidget;
begin
  Result:=nil;
end;

class function TCustomWebWidget.FixedElement: TJSHTMLElement;
begin
  Result:=Nil;
end;



class function TCustomWebWidget.FindElement(aID: String): TJSHTMLElement;
begin
  Result:=TJSHTMLElement(Document.getElementbyID(aID));
end;

class function TCustomWebWidget.CreateElement(aTag: String; aID: String
  ): TJSHTMLElement;
begin
  Result:=TJSHTMLElement(Document.createElement(aTag));
  if aID<>'' then
    Result.id:=aID;
end;

class function TCustomWebWidget.CreateElement(aParent: TJSElement; 
  aTag: String; aID: String): TJSHTMLElement;
begin
  Result := CreateElement(aTag, aID);
  if aParent <> nil then
    aParent.appendChild(Result);
end;

function TCustomWebWidget.GetReferenceElement: TJSHTMLELement;
begin
  Result:=Element;
end;

procedure TCustomWebWidget.Refresh;

Var
  I : integer;

begin
  if IsRendered then
    UnRender(ParentElement);
  InvalidateParentElement;
  EnsureElement;
  For I:=0 to ChildCount-1 do
    Children[i].Refresh;
end;

procedure TCustomWebWidget.Unrender;

Var
  P : TJSHTMLElement;

begin
  P:=ParentElement;
  If Assigned(P) then
    UnRender(P);
end;

procedure TCustomWebWidget.Focus;
begin
  if not IsRendered then
    ReFresh;
  Element.Focus;
end;


procedure TCustomWebWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);

// Normally, this should be called BEFORE FElement is set.
// But we'll be extra careful, and not rely on getters using FElement.

Var
  S : String;

begin
  if aElement.id='' then
    aElement.id:=GenerateID;
  // Don't use Classes, it will return FElement.Classname when set
  S:=AddClasses(FClasses,WidgetClasses);
  if (S<>'') then
    AddClasses(aElement,S);
  if FStyles.Count>0 then
    FStyles.ApplyToDOM(aElement);
  if Not FVisible then
    ApplyVisible(aElement,FVisible);
  // Maybe we should put this under control of a property or so ?
  // TStyleRefresh = (srAlways,srOnElementID,srNever)
  if (StyleRefresh = srAlways)
     or ((FelementID<>'') and (FElementID<>'')) then
    FStyles.RefreshFromDom(aElement,False);
  if Assigned(FAttrs) then
    for S in TJSObject.getOwnPropertyNames(FAttrs) do
      if IndexText(S,['id','class'])=-1 then
        aElement[S]:=String(FAttrs[S]);
end;

function TCustomWebWidget.DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement;
begin
  If AParent=Nil then
    Console.Log(DisplayElementName+': render without parent!');
  Result:=aElement;
end;


procedure TCustomWebWidget.ApplyData;

Var
  AID : String;

  Procedure MaybeSet(El : TJSHTMLElement; AName : String);

  begin
    if Assigned(el) and CreateDataTags then
      el.Dataset[aName]:=AID;
  end;

begin
  AID:=ElementID;
  if assigned(Element) and Not CreateDataTags then
    Element.dataset[SElementClass]:=ClassName;
  MaybeSet(Element,SElementData);
  MaybeSet(TopElement,STopElementData);
  if AllowChildren then
    MaybeSet(ContentElement,SContentElementData);
end;

procedure TCustomWebWidget.RemoveData;

  Procedure MaybeUnSet(El : TJSHTMLElement; AName : String);

  begin
    if Assigned(el) then
      jsDelete(el.Dataset,aName);
  end;

begin
  MaybeUnSet(Element,SElementData);
  MaybeUnSet(TopElement,STopElementData);
  MaybeUnSet(ContentElement,SContentElementData);
end;

procedure TCustomWebWidget.RefreshReferences;
begin
  if Assigned(FReferences) then
    if Assigned(Element) then
      References.RefreshFromDom(GetReferenceElement)
    else
      References.FRefs:=Nil;
end;

class function TCustomWebWidget.GenerateID: String;

begin
  Inc(WidgetID);
  Result:='ww-'+intToStr(WidgetID);
end;

function TCustomWebWidget.RenderHTML(aParent: TJSHTMLELement): TJSHTMLElement;

Var
  aTag : String;

begin
  aTag:=HTMLTag;
  if aTag='' then
    Result:=Nil
  else
    begin
    Result:=FixedElement;
    if Result=Nil then
      Result:=CreateElement(HTMLTag,GenerateID);
    end;
  if Assigned(Result) and Assigned(aParent) then
    aParent.AppendChild(Result);
  if Assigned(FBeforeRenderHTML) then
    FBeforeRenderHTML(Self);
  Result:=DoRenderHTML(aParent,Result);
  if Assigned(Result) then
    begin
    ApplyWidgetSettings(Result);
    HookupEvents(Result);
    end;
  if Assigned(FAfterRenderHTML) then
    FAfterRenderHTML(Self);
end;

procedure TCustomWebWidget.DoUnRender(aParent: TJSHTMLElement);

Var
  el : TJSHTMLElement;

begin
  if Assigned(aParent) and Assigned(FElement) then
    begin
    if FOwnsElement then
      begin
      el:=TopElement;
      if (El.ParentElement=aParent) then
        aParent.removeChild(el);
      end;
    InvalidateElement;
    end;
end;

procedure TCustomWebWidget.UnRender(aParent: TJSHTMLElement);
begin
  if Assigned(FBeforeUnRenderHTML) then
    FBeforeUnRenderHTML(Self);
  RemoveData;
  if assigned(AParent) then
    DoUnRender(aParent);
  if Assigned(FAfterUnRenderHTML) then
    FAfterUnRenderHTML(Self);
end;

function TCustomWebWidget.DispatchEvent(aName: String; aEvent: TJSEvent): Boolean;

begin
  if not IsRendered then
    exit;
  if (aEvent=Nil) then
    aEvent:=TJSEvent.New(aName);
  Result:=Element.dispatchEvent(aEvent);
end;

constructor TCustomWebWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FChildren:=TJSArray.New;
  FStyles:=CreateStyles;
  FMyHook:=@EventEntry;
  FParent:=DefaultParent;
  FVisible:=True;
end;

destructor TCustomWebWidget.Destroy;

Var
  I : integer;
  C : TCustomWebWidget;

begin
  For I:=0 to FChildren.Length-1 do
    begin
    C:=TCustomWebWidget(FChildren[i]);
    FChildren[i]:=Nil;
    C.Free;
    end;
  FChildren.Length:=0;
  Parent:=Nil;
  ParentID:='';
  FChildren:=Nil;
  FreeAndNil(FStyles);
  inherited Destroy;
end;

class function TCustomWebWidget.AllowChildren: Boolean;
begin
  Result:=True;
end;

class function TCustomWebWidget.AddRemoveClasses(const Source, aAddClasses,
  aRemoveClasses: String; Normalize: Boolean): String;
var
  T : TJSStringDynArray;
  i : integer;
  S : String;
begin
  Result:=Source;
  if Normalize then
    Result:=TJSString(Result).replace(TJSRegexp.New('\s\s+','g'),' ');
  T:=TJSString(Result).split(' ');
  For S in TJSString(aRemoveClasses).split(' ') do
    if (S<>'') then
      begin
      I:=TJSArray(T).indexOf(S);
      if (I<>-1) then
        TJSArray(T).splice(i,1);
      end;
  For S in TJSString(aAddClasses).split(' ') do
    if (S<>'') then
      begin
      I:=TJSArray(T).indexOf(S);
      if (I=-1) then
        TJSArray(T).Push(S);
      end;
  Result:=TJSArray(T).join(' ');
end;

class function TCustomWebWidget.ReplaceClasses(const Source, aSearchClasses, aReplaceClasses : String; Normalize: Boolean): String;
var
  Dest,Srch,Repl : TJSStringDynArray;
  sIdx,I : integer;
  S : String;

begin
  Srch:=TJSString(aSearchClasses).split(' ');
  Repl:=TJSString(aReplaceClasses).split(' ');
  Result:=Source;
  if Normalize then
    Result:=TJSString(Result).replace(TJSRegexp.New('\s\s+','g'),' ');
  Dest:=TJSString(Result).split(' ');
  For sIdx:=0 to length(Srch)-1 do
    begin
    S:=Srch[sIdx];
    if (S<>'') then
      begin
      I:=TJSArray(Dest).indexOf(S);
      if (I<>-1) then
        begin
        TJSArray(Dest).splice(i,1);
        if sIdx<Length(Repl) then
          begin
          I:=TJSArray(Dest).indexOf(Repl[sIdx]);
          if I=-1 then
            TJSArray(Dest).Push(Repl[sIdx]);
          end;
        end;
      end;
    end;
  Result:=TJSArray(Dest).join(' ');
end;

class function TCustomWebWidget.RemoveClasses(const Source, aClasses: String; Normalize : Boolean = false): String;

begin
  Result:=AddRemoveClasses(Source,'',aClasses,Normalize);
end;

class function TCustomWebWidget.RemoveClasses(el: TJSHTMLElement; const aClasses: String; Normalize : Boolean = false): String;

begin
  Result:=RemoveClasses(el.ClassName,aClasses,Normalize);
  el.ClassName:=Result;
end;

class function TCustomWebWidget.AddClasses(const Source, aClasses: String; Normalize : Boolean = false): String;

begin
  Result:=AddRemoveClasses(Source,aClasses,'',Normalize);
end;

class function TCustomWebWidget.AddClasses(el: TJSHTMLElement; const aClasses: String; Normalize : Boolean = false): String;

begin
  Result:=AddClasses(el.ClassName,aClasses,Normalize);
  el.ClassName:=Trim(Result);
end;

class function TCustomWebWidget.AddRemoveClasses(el: TJSHTMLElement;
  const aAddClasses, aRemoveClasses: String; Normalize: Boolean): String;
begin
  Result:=AddRemoveClasses(el.ClassName,aAddClasses,aRemoveClasses,Normalize);
  el.ClassName:=Trim(Result);
end;

class function TCustomWebWidget.ReplaceClasses(el: TJSHTMLElement;
  const aSearchClasses, aReplaceClasses: String; Normalize: Boolean): String;
begin
  Result:=ReplaceClasses(el.ClassName,aSearchClasses, aReplaceClasses,Normalize);
  el.ClassName:=Trim(Result);
end;

function TCustomWebWidget.AddRemoveClasses(const aAddClasses,
  aRemoveClasses: String; Normalize: Boolean): String;
begin
  FClasses:=AddRemoveClasses(FClasses,aAddClasses,aRemoveClasses,Normalize);
  Result:=FClasses;
  if IsRendered then
    Result:=AddRemoveClasses(FElement,aAddClasses,aRemoveClasses,Normalize)
end;

function TCustomWebWidget.ReplaceClasses(const aSearchClasses,
  aReplaceClasses: String; Normalize: Boolean): String;
begin
  FClasses:=ReplaceClasses(FClasses,aSearchClasses,aReplaceClasses,Normalize);
  Result:=FClasses;
  if IsRendered then
    Result:=ReplaceClasses(FElement,aSearchClasses,aReplaceClasses,Normalize)
end;

function TCustomWebWidget.RemoveClasses(const aClasses: String; Normalize : Boolean = false): String;
begin
  FClasses:=RemoveClasses(FClasses,aClasses,Normalize);
  Result:=FClasses;
  if IsRendered then
    Result:=RemoveClasses(FElement,aClasses,Normalize)
end;

function TCustomWebWidget.AddClasses(const aClasses: String; Normalize: Boolean): String;
begin
  FClasses:=AddClasses(FClasses,aClasses,Normalize);
  Result:=FClasses;
  if IsRendered then
    Result:=AddClasses(FElement,aClasses,Normalize)
end;

function TCustomWebWidget.FindWidgetByID(aElementID: String; Recurse: Boolean): TCustomWebWidget;

Var
  I : Integer;

begin
  Result:=Nil;
  if aElementID='' then
    exit;
  if (aElementID=elementID) then
    Exit(Self);
  I:=ChildCount-1;
  // First this level. Typical layout is not so nested.
  While (i>=0) and (Result=Nil) do
    begin
    Result:=Children[i];
    if (Result.ElementID<>aElementID) then
      Result:=nil;
    Dec(I);
    end;
  If (Result=Nil) and (Recurse) then
     begin
     I:=ChildCount-1;
     While (i>=0) and (Result=Nil) do
       begin
       Result:=Children[i].FindWidgetByID(aElementID,True);
       Dec(I);
       end;
     end;
end;

function TCustomWebWidget.GetWidgetByID(aElementID: String; Recurse: Boolean): TCustomWebWidget;
begin
  Result:=FindWidgetByID(aElementID,Recurse);
  if Result=Nil then
    Raise EWidgets.CreateFmt(SErrWidgetNotFound,[aElementID]);
end;

function TCustomWebWidget.EnsureStyle(const aName: String): TStyleItem;
begin
  Result:=Styles.EnsureStyle(aName);
end;

function TCustomWebWidget.AddStyle(const aName, aValue: String): TStyleItem;
begin
  Result:=EnsureStyle(aName);
  Result.Value:=aValue;
end;

function TCustomWebWidget.GetStyleValue(const aName : String): String;

Var
  S : TStyleItem;

begin
  S:=Styles.FindStyle(aName);
  if Assigned(S) then
    Result:=S.Value
  else
    Result:='';
end;

function TCustomWebWidget.RemoveStyle(const aName: String): String;

begin
  Result:=Styles.RemoveStyle(aName);
end;

procedure TCustomWebWidget.RemoveData(const aName: String);
begin
  if IsRendered then
    jsDelete(Element.Dataset,aName)
end;

end.

