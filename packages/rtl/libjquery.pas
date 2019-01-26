unit libjquery;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses js, web;

Type
  TJQueryTopLeft = record
    top,left : integer;
  end;

  { // Sections/categories in the API documentation site to be checked/added.
    Todo : Deferred
    Todo : Effects
    Todo : events
    Todo : Forms
    Todo : Internals
    Todo : Manipulation
    Todo : Properties
  }
  TJQuery = class;

  TCallback = Procedure (args : JSValue);
  TCallbackEvent = Procedure (args : JSValue);

  TCallbacks = class external name 'Callbacks'
  Public
    function add(aCallBack : TCallBack) : TCallbacks;
    function add(aCallBack : Array of TCallBack) : TCallbacks;
    function add(aCallBack : TCallBackEvent) : TCallbacks;
    function add(aCallBack : Array of TCallBackEvent) : TCallbacks;
    function disable : TCallBacks;
    function disabled : Boolean;
    function empty : TCallBacks;
    function fire(arguments : JSValue) : TCallbacks; varargs;
    function fired : Boolean;
    function fireWith(context : JSValue; arguments : JSValue) : TCallbacks;
    function has(aCallBack : TCallBack) : Boolean;
    function has(aCallBack : TCallBackEvent) : Boolean;
    function lock : TCallBacks;
    function locked : boolean;
    function remove(aCallBack : TCallBack) : TCallbacks;
    function remove(aCallBack : Array of TCallBack) : TCallbacks;
    function remove(aCallBack : TCallBackEvent) : TCallbacks;
    function remove(aCallBack : Array of TCallBackEvent) : TCallbacks;
  end;

  { TJQuery }

  TJQueryAddClassHandler = Reference to Function (aIndex : Integer; AClass : String) : String;
  TJQueryAttrHandler = Reference to Function (aIndex : Integer; aAttr : String) : JSValue;
  TJQueryCSSHandler = Reference to Function (aIndex : Integer; AClass : String) : JSValue ;
  TJQueryEachHandler = Reference to Function (aIndex : Integer; AElement : TJSElement) : Boolean;
  TJQueryFilterHandler = Reference to Function (aIndex : Integer; AElement : TJSElement) : Boolean;
  TJQueryHeightHandler = Reference to Function (aIndex : Integer; AHeight : jsValue) : JSValue ;
  TJQueryHTMLHandler = Reference to Function(aIndex : Integer; aHTML : String) : String;
  TJQueryMapHandler = Reference to Function (aIndex : Integer; AElement : TJSElement) : TJSObject;
  TJQueryOffsetHandler = Reference to Function (aIndex : Integer; aCoords : TJQueryTopLeft) : TJQueryTopLeft;
  TJQueryPropHandler = Reference to Function(aIndex : Integer; oldProp : JSValue) : JSValue;
  TJQueryQueueHandler = Reference to procedure;
  TJQueryTextHandler  = Reference to Function(aIndex : Integer; aString : String) : String;
  TJQueryToggleClassHandler = Reference to Function(aIndex : Integer; aClassName : string; AState : Boolean) : String;
  TJQueryValHandler = Reference to Function(aIndex :integer; aValue : String) : String;
  TJQueryWidthHandler = Reference to Function (aIndex : Integer; AHeight : jsValue) : JSValue ;

  TJQueryDeQueueFunction = Reference to Procedure;
  TJQueryAddQueueHandler = Reference to Procedure (aFunc : TJQueryDeQueueFunction);

  TAjaxEvent = class external name 'AjaxEvent' (TJSEvent);

  TDeferredDoneHandler = reference to function : Boolean;

  TJQueryDeferred = class external name 'Deferred' (TJSObject)
  Public
    Function done(aHandler :TDeferredDoneHandler) : TJQueryDeferred; overload;
    Function done(aHandlers : Array of TDeferredDoneHandler) : TJQueryDeferred; overload;
  end;

  TJQXHR = class;

  TJQXHRDoneHandler = reference to function(aData : jsValue; textStatus : String; aJQXHR : TJQXHR) : boolean;
  TJQXHRFailHandler = reference to function(aJQXHR : TJQXHR; textStatus : String; AErrorThrown : jsValue) : boolean;
  TJQXHRAlwaysHandler = reference to function(arg1 : TJSObject; textStatus : String; arg2 : TJSObject) : boolean;

  TJQXHR = class external name 'jqXHR' (TJQueryDeferred)
  private
    FReadyState: NativeInt; external name 'readyState';
    //FResponse: JSValue; external name 'response';
    FResponseText: string; external name 'responseText';
    FresponseXML: TJSDocument; external name 'responseXML';
    //FUpload: TJSXMLHttpRequestUpload; external name 'upload';
    FStatus : NativeInt; external name 'status';
    FStatusText : String; external name 'statustext';
  public
    function getResponseHeader(aName : string) : String;
    function getAllResponseHeaders : String;
    procedure overrideMimeType(aType : String);
    procedure setRequestHeader(aName, AValue : string);
    procedure done(aHandler : TJQXHRDoneHandler); overload;
    procedure always(aHandler : TJQXHRAlwaysHandler); overload;
    procedure fail(aHandler : TJQXHRFailHandler); overload;
    procedure _then(aSuccess : TJQXHRDoneHandler; aFail : TJQXHRFailHandler); overload;
    procedure abort;
    procedure abort(AStatusText : String);
    property readyState : NativeInt read FReadyState;
    property ResponseHeaders[aName : string] : string Read getResponseHeader;
    property responseXML : TJSDocument read FresponseXML;
    property responseText : string read FResponseText;
    property status : NativeInt read FStatus;
    property statusText : string read FStatusText;
  end;

  TJSAjaxSettings = class;

  TJQueryAjaxSettingsHandler = Reference to Function (aHXR : TJQXHR; aOptions : TJSAjaxSettings) : Boolean;
  TJQueryAjaxSettingsDataFilter = Reference to function (aData: String; aType : string) : JSValue;
  TJQueryAjaxSettingsErrorHandler = Reference to Function (aHXR : TJQXHR; aOptions : TJSAjaxSettings; aStatus, aError : String) : Boolean;
  TJQueryAjaxSettingsSuccessHandler = Reference to Function (data : JSValue; aStatus : String; aHXR : TJQXHR) : Boolean;
  TJQueryAjaxSettsingsXHRHandler = reference to function : JSValue;

  TJSAjaxSettings = class external name 'Object' (TJSObject)
    accepts : TJSObject;
    async : boolean;
    beforeSend : TJQueryAjaxSettingsHandler;
    cache : boolean;
    complete : TJQueryAjaxSettingsHandler;
    contents : TJSObject;
    contentType : String;
    context : TJSObject;
    converters : TJSObject;
    crossDomain : boolean;
    data : JSValue;
    dataFilter : TJQueryAjaxSettingsDataFilter;
    dataType : String;
    error : TJQueryAjaxSettingsErrorHandler;
    global: boolean;
    headers : TJSObject;
    ifModified : Boolean;
    isLocal : Boolean;
    json : String;
    jsonpCallback : String;
    method : string;
    mimeType : string;
    password : string;
    processData : Boolean;
    scriptCharset : String;
    statusCode : TJSObject;
    success : TJQueryAjaxSettingsSuccessHandler;
    timeout : NativeInt;
    traditional : boolean;
    url : string;
    username : string;
    xhr : TJQueryAjaxSettsingsXHRHandler;
    xhrFields : TJSObject;
  end;

  TJQueryAjaxTransportCompleteHandler = function (aStatus : NativeInt; aStatusText : string; responses, Headers : TJSObject) : Boolean;
  TJQueryAjaxTransportSendHandler = reference to function(headers: TJSObject; onComplete : TJQueryAjaxTransportCompleteHandler) : boolean;
  TJQueryAjaxTransportAbortHandler = reference to function () : Boolean;
  TJQueryAjaxTransport = record
    send : TJQueryAjaxTransportSendHandler;
    abort : TJQueryAjaxTransportAbortHandler;
  end;

  TJQueryAjaxTransportHandler = reference to Function (aOptions,aOriginalOptions : TJSObject; aXHR : TJQXHR) : TJQueryAjaxTransport;
  TJQueryAjaxPrefilterHandler = reference to procedure (aOptions,aOriginalOptions : TJSObject; aXHR : TJQXHR);
  TJQueryAjaxEventHandler = Reference to Function (aEvent : TAjaxEvent; aHXR : TJQXHR; aOptions : TJSAjaxSettings) : Boolean;
  TJQueryAjaxErrorHandler = Reference to Function (aEvent : TAjaxEvent; aHXR : TJQXHR; aOptions : TJSAjaxSettings; aError : String) : Boolean;
  TJQueryAjaxSuccessHandler = Reference to function (aData : TJSObject; aStatus : String; aXHR : TJQXHR) : Boolean;
  TJQueryAjaxLoadHandler = Reference to function (aResponseText,aStatus : String; aXHR : TJQXHR) : Boolean;
  TJQueryAjaxScriptHandler = Reference to function (aScript,aStatus : String; aXHR : TJQXHR) : Boolean;
  TJQueryAjaxHandler = Reference to procedure;

  TJQuery = class external name 'JQuery' (TJSObject)
  private
    FCSSHooks: TJSObject; external name 'cssHooks';
    FCSSNumber: TJSObject; external name 'cssNumber';
    FReady: TJSPromise; external name 'ready';
    function getEl(aIndex : Integer) : TJSElement; external name 'get';
  Public
    function add(Const aSelector : String) : TJQuery;overload;
    function add(Const aSelector : String; AContext : TJSElement) : TJQuery;overload;
    function add(Const aElement : TJSElement) : TJQuery;overload;
    function add(Const aElement : Array of TJSElement) : TJQuery;overload;
    function add(Const aQuery : TJQuery) : TJQuery;overload;
    function addBack(Const aSelector : String) : TJQuery;overload;
    function addBack : TJQuery;overload;
    function addClass(Const aClass : String) : TJQuery;overload;
    function addClass(Const aClassFunction : TJQueryAddClassHandler) : TJQuery;  overload;
    function ajaxComplete(aHandler : TJQueryAjaxEventHandler) : TJQuery;
    function ajaxError(aHandler : TJQueryAjaxEventHandler) : TJQuery;
    function ajaxSend(aHandler : TJQueryAjaxEventHandler) : TJQuery;
    function ajaxStart(aHandler : TJQueryAjaxHandler) : TJQuery;
    function ajaxStop(aHandler : TJQueryAjaxHandler) : TJQuery;
    function ajaxSuccess(aHandler : TJQueryAjaxEventHandler) : TJQuery;
    class function ajax(aURL : String; aSettings : TJSObject) : tJQXHR; overload;
    class function ajax(aSettings : TJSObject) : tJQXHR; overload;
    class function ajax(aSettings : TJSAjaxSettings) : tJQXHR; overload;
    class procedure ajaxPrefilter(dataTypes : string; aHandler : TJQueryAjaxPrefilterHandler);overload;
    class procedure ajaxSetup(aSettings : TJSAjaxSettings); overload;
    class procedure ajaxSetup(aSettings : TJSObject); overload;
    class procedure ajaxTransport(aDataType : string; AHandler : TJQueryAjaxTransportHandler);

    function attr(Const attributeName : string) : string;overload;
    function attr(Const attributeName : string; Const Value : String) : TJQuery;overload;
    function attr(Const attributes : TJSObject) : TJQuery;overload;
    function attr(Const attributeName : string; aHandler : TJQueryAttrHandler) : TJQuery;overload;
    class function Callbacks : TCallbacks;overload;
    class function Callbacks(const aFlags : string) : TCallbacks;overload;
    function children(Const aSelector : String) : TJQuery;overload;
    function children : TJQuery;overload;
    function clearQueue : TJQuery;overload;
    function clearQueue(const aQueueName : String) : TJQuery;overload;
    function closest(Const aSelector : String) : TJQuery;overload;
    function closest(Const aSelector : String; AContext : TJSElement) : TJQuery;overload;
    function closest(Const aQuery : TJQuery) : TJQuery;overload;
    function closest(Const aElement : TJSElement) : TJQuery;overload;
    function contents : TJQuery;
    function css(Const aPropertyName : TJSObject) : string; overload;
    function css(Const aPropertyName : String) : string; overload;
    function css(Const aPropertyNames : Array of String) : string;overload;
    function css(Const aPropertyName, Avalue : String) : TJQuery;overload;
    function css(Const aPropertyName : String; Avalue : Integer) : TJQuery;overload;
    function css(Const aPropertyName : String; AHandler : TJQueryCSSHandler) : TJQuery;overload;
    class function data(aElement : TJSElement; const aKey : String; aValue : jsValue) : TJSObject;overload;
    class function data(aElement : TJSElement; const aKey : String) : TJSObject;overload;
    class function data(aElement : TJSElement) : TJSObject;overload;
    function data(aKey : String; aValue : JSValue) : TJQuery;overload;
    function data(aObj : TJSObject) : TJQuery;overload;
    function data(aKey : String) : TJSObject;overload;
    function data : TJSObject;overload;
    function dequeue : TJQuery;overload;
    function dequeue(const aQueueName : String) : TJQuery;overload;
    class function dequeue(aElement : TJSElement) : TJQuery;overload;
    class function dequeue(aElement : TJSElement; const aQueueName : String) : TJQuery;overload;
    function _end : TJQuery; external name 'end';
    function eq(AIndex : Integer) : TJQuery;
    function each(aHandler : TJQueryEachHandler) : TJQuery;
    class function escapeSelector(const S : String) : String;
    function filter(Const aSelector : String) : TJQuery;overload;
    function filter(aHandler : TJQueryFilterHandler) : TJQuery;overload;
    function filter(Const aQuery : TJQuery) : TJQuery;overload;
    function filter(Const aElement : TJSElement) : TJQuery;overload;
    function filter(Const aElements : Array of TJSElement) : TJQuery;overload;
    function find(Const aSelector : String) : TJQuery;overload;
    function find(Const aQuery : TJQuery) : TJQuery;overload;
    function find(Const aElement : TJSElement) : TJQuery;overload;
    function first : TJQuery;
    function get(aIndex : Integer) : TJSElement;overload;
    class function get : TJQXHR;overload;
    class function get(url : String) : TJQXHR;overload;
    class function get(url,Data : String) : TJQXHR;overload;
    class function get(url : String; Data : TJSObject) : TJQXHR;overload;
    class function get(url : String; Data : TJSObject; success : TJQueryAjaxSuccessHandler) : TJQXHR;overload;
    class function get(url,Data : String; success : TJQueryAjaxSuccessHandler) : TJQXHR;overload;
    class function get(url : String; Data : TJSObject; success : TJQueryAjaxSuccessHandler; aDataType : string) : TJQXHR;overload;
    class function get(url,Data : String; success : TJQueryAjaxSuccessHandler; aDataType : string) : TJQXHR;overload;
    class function get(aSettings : TJSAjaxSettings) : TJQXHR; overload;
    class function get(aSettings : TJSObject) : TJQXHR; overload;

    class function getJSON(url : String) : TJQXHR;overload;
    class function getJSON(url,Data : String) : TJQXHR;overload;
    class function getJSON(url : String; Data : TJSObject) : TJQXHR;overload;
    class function getJSON(url : String; Data : TJSObject; success : TJQueryAjaxSuccessHandler) : TJQXHR;overload;
    class function getJSON(url,Data : String; success : TJQueryAjaxSuccessHandler) : TJQXHR;overload;
    class function getJSON(url : String; Data : TJSObject; success : TJQueryAjaxSuccessHandler; aDataType : string) : TJQXHR;overload;
    class function getJSON(url,Data : String; success : TJQueryAjaxSuccessHandler; aDataType : string) : TJQXHR;overload;

    class function getScript(url : String) : TJQXHR;overload;
    class function getScript(url : String; aSuccess : TJQueryAjaxScriptHandler) : TJQXHR;overload;

    function has(Const aSelector : String) : TJQuery;
    function has(Const aQuery : TJQuery) : TJQuery;
    function hasClass(Const aClassName : String) : Boolean;
    class function hasData(aElement : TJSElement) : Boolean;
    function height: Integer;
    function height(aValue: Integer) : TJQuery;
    function height(aValue: String) : TJQuery;
    function height(aHandler: TJQueryHeightHandler) : TJQuery;
    function html : String;
    function html(Const aHTML : String) : TJQuery;
    function html(Const aHandler : TJQueryHTMLHandler) : TJQuery;
    function innerHeight: Integer;
    function innerHeight(aValue: Integer) : TJQuery;
    function innerHeight(aValue: String) : TJQuery;
    function innerHeight(aHandler: TJQueryHeightHandler) : TJQuery;
    function innerWidth: Integer;
    function innerWidth(aValue: Integer) : TJQuery;
    function innerWidth(aValue: String) : TJQuery;
    function innerWidth(aHandler: TJQueryWidthHandler) : TJQuery;
    function _is(Const aSelector : String) : TJQuery; external name 'is';
    function _is(Const aQuery : TJQuery) : TJQuery; external name 'is';
    function _is(aHandler : TJQueryFilterHandler) : TJQuery; external name 'is';
    function _is(Const aElement : TJSElement) : TJQuery; external name 'is';
    function _is(Const aElements : Array of TJSElement) : TJQuery; external name 'is';
    function last : TJQuery;
    class function load(url : String) : TJQXHR;overload;
    class function load(url,Data : String) : TJQXHR;overload;
    class function load(url : String; Data : TJSObject) : TJQXHR;overload;
    class function load(url : String; Data : TJSObject; success : TJQueryAjaxLoadHandler) : TJQXHR;overload;
    class function load(url,Data : String; success : TJQueryAjaxLoadHandler) : TJQXHR;overload;
    function map(aHandler : TJQueryMapHandler) : TJQuery;
    function next : TJQuery;overload;
    function next(const aSelector : String) : TJQuery;overload;
    function nextAll : TJQuery;overload;
    function nextAll(const aSelector : String) : TJQuery;overload;
    function nextUntil : TJQuery;overload;
    function nextUntil(const aSelector : String) : TJQuery;overload;
    function nextUntil(const aSelector,aFilter : String) : TJQuery;overload;
    function nextUntil(const aElement : TJSElement) : TJQuery;overload;
    function nextUntil(const aElement : TJSElement; aFilter : String) : TJQuery;overload;
    function nextUntil(const aQuery : TJQuery) : TJQuery;overload;
    function nextUntil(const aQuery : TJQuery; aFilter : String) : TJQuery;overload;
    function _not(const aSelector : String) : TJQuery; external name 'not';overload;
    function _not(const aSelector : TJSElement) : TJQuery; external name 'not';overload;
    function _not(const aSelector : Array of TJSElement) : TJQuery; external name 'not';overload;
    function _not(const aSelector : TJQuery) : TJQuery; external name 'not';overload;
    function _not(const aSelector : TJQueryFilterHandler) : TJQuery;external name 'not';overload;
    function noConflict : TJSObject;overload;
    function noConflict(removeAll: Boolean) : TJSObject;overload;
    function offSet: Integer;overload;
    function offSet(const aOffset : TJQueryTopLeft): TJQuery;overload;
    function offSet(aHandler : TJQueryOffsetHandler): TJQuery;overload;
    Function offsetParent : TJQuery;
    function outerHeight(IncludeMargin : Boolean): Integer;overload;
    function outerHeight: Integer;overload;
    function outerHeight(aValue: Integer) : TJQuery;overload;
    function outerHeight(aValue: String) : TJQuery;overload;
    function outerHeight(aHandler: TJQueryHeightHandler) : TJQuery;overload;
    function outerWidth(IncludeMargin : Boolean): Integer;overload;
    function outerWidth: Integer;overload;
    function outerWidth(aValue: Integer) : TJQuery;overload;
    function outerWidth(aValue: String) : TJQuery;overload;
    function outerWidth(aHandler: TJQueryWidthHandler) : TJQuery;overload;
    class function param (aObject : String) : String;overload;
    class function param (aObject : TJSObject) : String;overload;
    class function param (aObject : TJQuery) : String;overload;
    class function param (aObject : String; traditional : Boolean) : String;overload;
    class function param (aObject : TJSObject; traditional : Boolean) : String;overload;
    class function param (aObject : TJQuery; traditional : Boolean) : String;overload;

    Function parent : TJQuery;
    Function parent (const ASelector: String) : TJQuery;
    Function parents : TJQuery;
    Function parents (const ASelector: String) : TJQuery;
    function parentsUntil : TJQuery;
    function parentsUntil(const aSelector : String) : TJQuery;
    function parentsUntil(const aSelector,aFilter : String) : TJQuery;
    function parentsUntil(const aElement : TJSElement) : TJQuery;
    function parentsUntil(const aElement : TJSElement; aFilter : String) : TJQuery;
    function parentsUntil(const aQuery : TJQuery) : TJQuery;
    function parentsUntil(const aQuery : TJQuery; aFilter : String) : TJQuery;
    function position : TJQueryTopLeft;
    class function post(url : String) : TJQXHR;overload;
    class function post(url,Data : String) : TJQXHR;overload;
    class function post(url : String; Data : TJSObject) : TJQXHR;overload;
    class function post(url : String; Data : TJSObject; success : TJQueryAjaxSuccessHandler) : TJQXHR;overload;
    class function post(url,Data : String; success : TJQueryAjaxSuccessHandler) : TJQXHR;overload;
    class function post(url : String; Data : TJSObject; success : TJQueryAjaxSuccessHandler; aDataType : string) : TJQXHR;overload;
    class function post(url,Data : String; success : TJQueryAjaxSuccessHandler; aDataType : string) : TJQXHR;overload;
    class function post(aSettings : TJSAjaxSettings) : TJQXHR; overload;
    class function post(aSettings : TJSObject) : TJQXHR; overload;
    Function prev : TJQuery;overload;
    Function prev(Const aSelector : String) : TJQuery;overload;
    Function prevAll : TJQuery;overload;
    Function prevAll(Const aSelector : String) : TJQuery;overload;
    function prevUntil : TJQuery;overload;
    function prevUntil(const aSelector : String) : TJQuery;overload;
    function prevUntil(const aSelector,aFilter : String) : TJQuery;overload;
    function prevUntil(const aElement : TJSElement) : TJQuery;overload;
    function prevUntil(const aElement : TJSElement; aFilter : String) : TJQuery;overload;
    function prevUntil(const aQuery : TJQuery) : TJQuery;overload;
    function prevUntil(const aQuery : TJQuery; aFilter : String) : TJQuery;overload;
    function prop(const aPropertyName : String) : JSValue;overload;
    function prop(const aPropertyName : String;AValue : JSValue) : TJQuery;overload;
    function prop(const TJSObject) : TJQuery;overload;
    function prop(const aPropertyName : String; aHandler : TJQueryPropHandler) : TJQuery;overload;
    class function queue(element : TJSElement) : TJSarray;overload;
    class function queue(element : TJSElement; const aQueueName : String) : TJSarray;overload;
    class function queue(element : TJSElement; const aQueueName : string; anewQueue : TJSarray) : TJQuery;overload;
    class function queue(element : TJSElement; const aQueueName : String ; aHandler : TJQueryQueueHandler) : TJQuery;overload;
    function queue : TJSarray;overload;
    function queue(aQueueName : string) : TJSarray;overload;
    function queue(anArray : TJSArray) : TJQuery;overload;
    function queue(aQueueName : string; anArray : TJSarray) : TJQuery;overload;
    function queue(aQueueName : string; aHandler : TJQueryAddQueueHandler) : TJQuery;overload;
    function removeAttr(Const attributeName : string) : TJQuery;
    function removeClass(Const aClass : String) : TJQuery;overload;
    function removeClass(Const aClassFunction : TJQueryAddClassHandler) : TJQuery;overload;
    class function removeData(aElement : TJSElement; Const aName : String) : TJQuery;overload;
    class function removeData(aElement : TJSElement) : TJQuery;overload;
    function removeData(const aName : string) : TJQuery;overload;
    function removeData(const aNames : array of string) : TJQuery;overload;
    function removeData : TJQuery;overload;
    function removeProp(Const aPropertyName : string) : TJQuery;
    function scrollLeft : Integer;overload;
    function scrollLeft(aValue : Integer) : TJQuery;overload;
    function scrollTop : Integer;overload;
    function scrollTop(aValue : Integer) : TJQuery;overload;
    function serialize : string;
    function serializeArray : TJSObjectDynArrayArray;
    Function siblings : TJQuery;overload;
    Function siblings(Const aSelector : String) : TJQuery;overload;
    Function slice(aStart : integer) : TJQuery;overload;
    Function slice(aStart,aEnd : integer) : TJQuery;overload;
    Function sub : TJQuery;
    function text : String;overload;
    function text(Const aText : string): TJQuery;overload;
    function text(Const aText : Integer): TJQuery;overload;
    function text(Const aText : Double): TJQuery;overload;
    function text(Const aText : Boolean): TJQuery;overload;
    function text(aHandler : TJQueryTextHandler) : TJQuery;overload;
    function toggleClass(Const aClass : String) : TJQuery;overload;
    function toggleClass(Const aClass : String; aState : Boolean) : TJQuery;overload;
    function toggleClass(Const aHandler : TJQueryToggleClassHandler) : TJQuery;overload;
    function toggleClass(Const aHandler : TJQueryToggleClassHandler; AState : Boolean) : TJQuery;overload;
    function val : JSValue;overload;
    function val(Const aValue : String) : TJQuery;overload;
    function val(Const aValue : Integer) : TJQuery;overload;
    function val(Const aValue : Array of String) : TJQuery;overload;
    function val(aHandler : TJQueryValHandler) : TJQuery;overload;
    Function when(APromise : TJSPromise) :  TJSPromise;overload;
    Function when :  TJSPromise;overload;
    function Width: Integer;overload;
    function Width(aValue: Integer) : TJQuery;overload;
    function Width(aValue: String) : TJQuery;overload;
    function Width(aHandler: TJQueryWidthHandler) : TJQuery;overload;
    Property ready : TJSPromise Read FReady;
    // These should actually be class properties ?
    property cssHooks : TJSObject Read FCSSHooks;
    property cssNumber : TJSObject read FCSSNumber;
    Property Elements[AIndex : Integer] : TJSElement read getEl; default;
  end;


Function JQuery(Const aSelector :  String) : TJQuery; external name 'jQuery';
Function JQuery(Const aSelector :  String; Context : TJSElement) : TJQuery; external name 'jQuery';
Function JQuery(Const aElement : TJSElement) : TJQuery; external name 'jQuery';
Function JQuery(Const aElement : Array of TJSElement) : TJQuery; external name 'JQuery';
Function JQuery(Const aElement : TJSObject) : TJQuery; external name 'jQuery';
Function JQuery(Const aQuery : TJQuery) : TJQuery; external name 'jQuery';
Function JQuery() : TJQuery; external name 'jQuery';

Var
  gJQuery : TJQuery; external name 'jQuery';

Implementation

end.

