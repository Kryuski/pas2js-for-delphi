{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2017 by the Free Pascal development team

    HTTPRoute: HTTP request router

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{
  Note:
  The MatchPattern routine was taken from Brook Framework's router unit, by Silvio Clecio.
}

{$mode objfpc}
// Define this to output some debugging output
{ $DEFINE DEBUGROUTER }
unit webrouter;

interface

uses
  Classes, SysUtils, web;

Type
  EHTTPRoute = Class(Exception);
  TRawLocation = String;
  TScrollPoint = record
    X,Y : Double;
  end;

  // Forward definitions;

  TRouter = Class;
  TRoute = class;
  THistory = Class;
  TRouterClass = Class of TRouter;
  TRouteEvent = Reference to Procedure (URl : String; aRoute : TRoute; Params: TStrings);

  TTransitionResult = (trOK,trError,trAbort);
  THistoryKind = (hkAuto,hkAbstract,hkHash,hkHTML5);

  TTransitionNotifyEvent = Reference to Procedure (Sender : THistory; aLocation : String; aRoute : TRoute);
  TAllowTransitionEvent = Reference to Procedure (Sender : THistory; aOld, aNew : TRoute; Params : TStrings; var Allow : Boolean);


  { THistory }

  THistory = Class(TObject)
  Private
    FOnAllow: TAllowTransitionEvent;
    FRouter: TRouter;
    FOnChange : TNotifyEvent;
    FOnReady : TTransitionNotifyEvent;
    FOnError : TTransitionNotifyEvent;
    FCurrent : TRoute;
    FBase : String;
    function GetCurrent: TRoute;
  Protected
    procedure SetupListeners; virtual;
    Function doPush (location: TRawLocation) : TTransitionResult; virtual; abstract;
    Function doreplace (location: TRawLocation) : TTransitionResult; virtual; abstract;
    function doGo(N: integer): TTransitionResult; virtual; abstract;
    procedure ensureURL (push : boolean = false); virtual; abstract;
  Public
    Constructor Create(aRouter : TRouter); reintroduce;
    Constructor Create(aRouter : TRouter; aBase : String); virtual;
    Class Function NormalizeHash(aHash : String) : string;
    Procedure UpdateRoute (aRoute : TRoute);
    Destructor Destroy; override;
    Function ExpectScroll : Boolean;
    Function SupportsScroll : Boolean;
    Class function getLocation (base: string): string;
    Class function cleanPath(aPath : string): string;
    // Navigation
    function GetCurrentLocation: String; virtual; abstract;
    Function Push (location: TRawLocation) : TTransitionResult;
    Function Replace (location: TRawLocation) : TTransitionResult;
    function Go(N: integer): TTransitionResult;
    Function NavigateForward: TTransitionResult;
    Function NavigateBack: TTransitionResult;
    Function TransitionTo(aLocation: TRawLocation) : TTransitionResult;
    function ConfirmTransition(aRoute: TRoute; Params: TStrings) : TTransitionResult;
    Property Current : TRoute Read GetCurrent;
    Property Router : TRouter Read FRouter;
    Property OnReady : TTransitionNotifyEvent Read FOnReady Write FOnReady;
    Property OnError : TTransitionNotifyEvent Read FOnError Write FOnError;
    Property OnChange : TNotifyEvent Read FOnChange Write FOnChange;
    Property OnAllowTransition : TAllowTransitionEvent Read FOnAllow Write FOnAllow;
    property Base : String Read FBase;
    function Kind : THistoryKind; virtual; abstract;
  end;


  { TAbstractHistory }

  TAbstractHistory = Class(THistory)
  Private
    FIndex: Integer;
    FStack: Array of TRawLocation;
    procedure MaybeGrow(AIndex: Integer);
  Protected
    Function doPush (location: TRawLocation) : TTransitionResult; override;
    Function doReplace (location: TRawLocation) : TTransitionResult; override;
    function doGo(N: integer): TTransitionResult; override;
  Public
    constructor Create (router: TRouter; base: string = ''); override;
    function getCurrentLocation: String; override;
    Procedure ensureURL (Push: Boolean = False); override;
    function Kind : THistoryKind; override;
  end;

  { THashHistory }

  THashHistory = Class(THistory)
  Protected
    FlastHash : String;
    procedure DoHashChange; virtual;
    procedure SetupListeners; override;
    Function doPush (location: TRawLocation) : TTransitionResult; override;
    Function doreplace (location: TRawLocation) : TTransitionResult; override;
    function doGo(N: integer): TTransitionResult; override;
    procedure ensureURL (push : boolean = false); override;
  Public
    function getCurrentLocation: String; override;
    Class Procedure pushHash (path : string);
    Class Procedure replaceHash (path : string);
    class function getUrl (APath : string) : string;
    Class function getHash : string;
    function Kind : THistoryKind; override;
  end;

  { THTMLHistory }

  THTMLHistory = Class(THistory)
  Protected
    FlastLocation : String;
    procedure DoStateChange; virtual;
    procedure SetupListeners; override;
    Function doPush (location: TRawLocation) : TTransitionResult; override;
    Function doreplace (location: TRawLocation) : TTransitionResult; override;
    function doGo(N: integer): TTransitionResult; override;
    procedure ensureURL (push : boolean = false); override;
  Public
    function getCurrentLocation: String; override;
    Class Procedure pushState (path : string; doReplace : boolean = false);
    Class Procedure replaceState (path : string);
    function getUrl (ALocation : string) : string;
    function Kind : THistoryKind; override;
  end;

  { TRoute }

  TRoute = Class(TCollectionItem)
  private
    FDefault: Boolean;
    FEvent: TRouteEvent;
    FURLPattern: String;
    procedure SetURLPattern(AValue: String);
  Public
    Class function NormalizeURLPattern(AValue: String): String;
    Function Matches(Const APattern : String) : Boolean;
    Function MatchPattern(Const Path : String; L : TStrings) : Boolean;
    Procedure HandleRequest(ARouter : TRouter; Const URL : String; L : TStrings); virtual; abstract;
    Function FullPath : String;
  Published
    Property Default : Boolean Read FDefault Write FDefault;
    Property URLPattern : String Read FURLPattern Write SetURLPattern;
    Property Event : TRouteEvent Read FEvent Write FEvent;
  end;
  TRouteClass = Class of TRoute;


  { TRouteList }

  TRouteList = Class(TCollection)
  private
    function GetR(AIndex : Integer): TRoute;
    procedure SetR(AIndex : Integer; AValue: TRoute);
  Public
    Property Routes[AIndex : Integer] : TRoute Read GetR Write SetR; default;
  end;

  TRouteObject = Class(TObject)
    Procedure HandleRoute (Const URL : String; Params : TStrings); virtual; abstract;
  end;
  TRouteObjectClass = Class of TRouteObject;

  { TRouter }

  TBeforeRouteEvent = reference to procedure(Sender : TObject; Var ARouteURL : String);
  TAfterRouteEvent = reference to procedure(Sender : TObject; const ARouteURL : String);
  TScrollParams = Record
    selector : string;
    Position : TScrollPoint;
  end;
  TPageScrollEvent = reference to Function(Sender : TObject; aTo,aFrom : TRoute; aPosition : TScrollPoint) : TScrollParams;


  TRouter = Class(TComponent)
  Private
    Class Procedure DoneService;
    Class
      Var FService : TRouter;
          FServiceClass : TRouterClass;
  private
    FAfterRequest: TAfterRouteEvent;
    FBeforeRequest: TBeforeRouteEvent;
    FHistory: THistory;
    FOnScroll: TPageScrollEvent;
    FRoutes : TRouteList;
    function GetHistory: THistory;
    function GetHistoryKind: THistoryKind;
    function GetR(AIndex : Integer): TRoute;
    function GetRouteCount: Integer;
  Protected
    // Return an instance of given class with Pattern, Method, IsDefault filled in.
    function CreateHTTPRoute(AClass: TRouteClass; const APattern: String; IsDefault: Boolean ): TRoute; virtual;
    // Override this if you want to use another collection class.
    Function CreateRouteList : TRouteList; virtual;
    Procedure CheckDuplicate(APattern : String; isDefault : Boolean);
    // Actually route request. Override this for customized behaviour.
    function DoRouteRequest(ARoute : TRoute; Const AURL : String; AParams : TStrings) : TRoute; virtual;
    function DoRouteRequest(AURL : String; DoPush : Boolean = False) : TRoute;
  Public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure InitHistory(aKind : THistoryKind; aBase : String = '');
    // Delete given route by index.
    Procedure DeleteRoute(AIndex : Integer);
    // Delete given route by index.
    Procedure DeleteRouteByID(AID : Integer);
    // Delete given route by index. The route object will be freed.
    Procedure DeleteRoute(ARoute : TRoute);
    // Sanitize route path. Strips of query parameters and makes sure it ends in /
    class function SanitizeRoute(const Path: String): String;
    // Global instance.
    Class Function Service : TRouter;
    // Class for global instance when it is created;
    Class Function ServiceClass : TRouterClass;
    // This will destroy the service
    Class Procedure SetServiceClass(AClass : TRouterClass);
    // Register event based route
    Function RegisterRoute(Const APattern : String; AEvent: TRouteEvent; IsDefault : Boolean = False) : TRoute;overload;
    // Object class based route. The router is responsible for the lifetime of the object instance
    Function RegisterRoute(Const APattern : String; const AObjectClass: TRouteObjectClass; IsDefault : Boolean = False) : TRoute; overload;
    // Find route. Matches Path on the various patterns. If a pattern is found, then the method is tested.
    // Returns the route that matches the pattern and method.
    function FindHTTPRoute(const Path: String; Params: TStrings): TRoute;
    function GetRoute(const Path: String; Params: TStrings): TRoute;
    // Do actual routing. Exceptions raised will not be caught.
    // If DoPush is true, the URL will be pushed on the browser history. If False, the route is simply activated.
    Function RouteRequest(Const ARouteURL : String; DoPush : Boolean = False) : TRoute;
    // Extract request path from URL. By default, returns the URL
    function GetRequestPath(const URL: String): String; virtual;
    // Examine the URL hash and route the request. Returns the route. Use when arriving on a page do handle the initial route
    Function RouteFromURL : String;
    // Navigation. These are easy-access methods for history.
    function GetCurrentLocation: String;
    // These use the history mechanism
    Function Push (location: TRawLocation) : TTransitionResult;
    Function Replace (location: TRawLocation) : TTransitionResult;
    function Go(N: integer): TTransitionResult;
    Function NavigateForward : TTransitionResult;
    Function NavigateBack :TTransitionResult;
    // Indexed access to the registered routes.
    Property Routes [AIndex : Integer]  : TRoute Read GetR; Default;
    // Number of registered routes.
    Property RouteCount : Integer Read GetRouteCount;
    // Events executed before and after request. In case of exception, after is not executed.
    Property BeforeRequest : TBeforeRouteEvent Read FBeforeRequest Write FBeforeRequest;
    Property AfterRequest : TAfterRouteEvent Read FAfterRequest Write FAfterRequest;
    // OnScroll
    Property OnScroll : TPageScrollEvent Read FOnScroll Write FOnScroll;
    // Currently used history mechanism
    Property History : THistory Read GetHistory;
    // Kind of current history. Shortcut for History.Kind, returns hkauto if History is not assigned
    Property HistoryKind : THistoryKind Read GetHistoryKind;
  end;

  TWebScroll = Class
    Class Procedure scrollToPosition (AScroll : TScrollParams);
    Class function getScrollPosition : TScrollPoint;
    Class Procedure SaveScrollPosition;
    Class Procedure Setup;
    Class Procedure handle (router: TRouter; ato: TRoute; afrom: TRoute; isPop: boolean) ;
    Class Function GetStateKey : String;
  end;

  TBrowserState = Class
  Private
    Class var
    TheKey : String;
  Public
    Class Function GenKey : String;
    Class Function supportsPushState : Boolean;
    Class function GetStateKey : string;
    Class Procedure SetStateKey (akey: string);
    Class Procedure PushState (aUrl : string; replace : boolean);
    Class Procedure ReplaceState(aUrl: string);
  end;

// Shortcut for TRouter.Service;
Function Router : TRouter;
Function IncludeHTTPPathDelimiter (S : String) : String;

implementation

uses strutils, js;

Resourcestring
  EDuplicateRoute = 'Duplicate route pattern: %s';
  EDuplicateDefaultRoute = 'Duplicate default route registered with pattern: %s';

function Router: TRouter;
begin
  Result:=TRouter.Service;
end;

function IncludeHTTPPathDelimiter(S: String): String;

begin
  If (Copy(S,Length(S),1)='/') then
    Result:=S
  else
    Result:=S+'/';
end;

{ THTMLHistory }

procedure THTMLHistory.DoStateChange;
Var
  NewLocation : String;
  Old : TRoute;

begin
  NewLocation:=getLocation(FBase);
  if (NewLocation=FLastLocation) then
    exit;
  old:=Current;
  if TransitionTo(NewLocation)=trOK then
    begin
    TWebScroll.Handle(router, Current, old, true);
    FLastLocation:=NewLocation;
    end
  else
    replaceState(FLastLocation);
end;

procedure THTMLHistory.SetupListeners;
begin
  Window.addEventListener('popstate',@DoStateChange)
end;

function THTMLHistory.doPush(location: TRawLocation): TTransitionResult;

begin
  pushState(GetURL(Location));
  Result:=trOK;
end;

function THTMLHistory.doreplace(location: TRawLocation): TTransitionResult;

begin
  ReplaceState(GetURL(Location));
  Result:=trOK;
end;

function THTMLHistory.doGo(N: integer): TTransitionResult;
begin
  window.history.go(n);
  Result:=trOK;
end;

procedure THTMLHistory.ensureURL(push: boolean);

var
  URL,Actual,Expected : string;

begin
  Actual:=getCurrentLocation;
  Expected:=FlastLocation;
  if (Actual<>Expected) then
    begin
    url:=getUrl(Expected);
    if Push then
      pushState(url)
    else
      replaceState(url)
    end;
end;

function THTMLHistory.getCurrentLocation: String;
begin
  Result:=window.locationString;
end;

class procedure THTMLHistory.pushState(path: string; doReplace: boolean);
begin
  TBrowserState.pushState(Path,doReplace);
end;

class procedure THTMLHistory.replaceState(path: string);
begin
  pushState(Path,True);
end;

function THTMLHistory.getUrl(ALocation : string): string;

begin
  Result:=IncludeHTTPPathDelimiter(FBase);
  While (ALocation<>'') and (Copy(ALocation,1,1)='/') do
    ALocation:=Copy(ALocation,2,Length(ALocation)-1);
  Result:=FBase+Alocation;
end;

function THTMLHistory.Kind: THistoryKind;
begin
  Result:=hkHTML5;
end;

{ THistory }

function THistory.GetCurrent: TRoute;
begin
  Result:=FCurrent;
end;

constructor THistory.Create(aRouter: TRouter);
begin
  Create(aRouter,'');
end;

constructor THistory.Create(aRouter: TRouter; aBase: String);
begin
  FRouter:=aRouter;
  FBase:=aBase;
end;

class function THistory.NormalizeHash(aHash: String): string;

begin
  Result:=aHash;
  if Copy(Result,1,1)<>'/' then
    Result:='/'+Result;
end;

destructor THistory.Destroy;
begin
  inherited Destroy;
end;

function THistory.ExpectScroll: Boolean;

begin
  Result:=Assigned(Router) and Assigned(Router.OnScroll);
end;

function THistory.SupportsScroll: Boolean;
begin
  Result:=TBrowserState.supportsPushState and ExpectScroll;
end;

function THistory.TransitionTo(aLocation: TRawLocation): TTransitionResult;

Var
  Params : TStrings;
  R : TRoute;
begin
  Params:=TStringList.Create;
  try
    R:=Router.FindHTTPRoute(aLocation,Params);
    Case ConfirmTransition(R,Params) of
    trOK :
      begin
      R:=Router.DoRouteRequest(R,aLocation,Params);
      UpdateRoute(R);
      if Assigned(OnReady) then
        OnReady(Self,aLocation,R);
      end;
    trError:
      if Assigned(OnError) then
        FOnError(Self,aLocation,R);
    end;
  Finally
    Params.Free;
  end;
  Result:=trOK;
end;

function THistory.ConfirmTransition(aRoute: TRoute; Params : TStrings): TTransitionResult;

Var
  Old : TRoute;
  allow : Boolean;

begin
  Old:=Current;
  Allow:=True;
  if Assigned(FOnAllow) then
    FOnAllow(Self,old,aRoute,Params,Allow);
  if Not Allow then
    begin
    ensureURL();
    Result:=trAbort;
    end;
  Result:=trOK;
end;

{ TRouteObjectHandler }

Type
  TRouteObjectHandler = Class(TRoute)
  private
    FObjectClass: TRouteObjectClass;
  Public
    Procedure HandleRequest(ARouter : TRouter; Const URL : String; Params  : TStrings); override;
    Property RouteObjectClass : TRouteObjectClass Read FObjectClass Write FObjectClass;
  end;

  { TRouteEventHandler }

  TRouteEventHandler = Class(TRoute)
  Public
    Procedure HandleRequest(ARouter : TRouter; Const URL : String; Params  : TStrings); override;
    Property Event : TRouteEvent Read FEvent Write FEvent;
  end;

{ TRouteEventHandler }

procedure TRouteEventHandler.HandleRequest(ARouter : TRouter; const URL: String; Params: TStrings);
begin
  If Assigned(Event) then
    Event(URL,Self,Params);
end;

procedure TRouteObjectHandler.HandleRequest(ARouter : TRouter; Const URL : String; Params : TStrings);

Var
  O : TRouteObject;

begin
  O:=RouteObjectClass.Create;
  try
    O.HandleRoute(URL,Params);
  finally
    O.Free;
  end;
end;



{ TRouter }

function TRouter.GetR(AIndex : Integer): TRoute;
begin
  Result:=FRoutes[AIndex]
end;

function TRouter.GetHistory: THistory;
begin
  If (FHistory=Nil) then
    InitHistory(hkAuto,'');
  Result:=FHistory;
end;

function TRouter.GetHistoryKind: THistoryKind;
begin
  if Not assigned(History) then
    Result:=hkAuto
  else
    Result:=History.Kind;
end;

class procedure TRouter.DoneService;
begin
  FreeAndNil(FService);
end;

function TRouter.GetRouteCount: Integer;
begin
  Result:=FRoutes.Count;
end;

function TRouter.CreateRouteList: TRouteList;
begin
  Result:=TRouteList.Create(TRoute);
end;

procedure TRouter.CheckDuplicate(APattern: String; isDefault: Boolean);
Var
  I,DI : Integer;
  R : TRoute;

begin
  DI:=-1;
  For I:=0 to FRoutes.Count-1 do
    begin
    R:=FRoutes[I];
    if R.Default then
      DI:=I;
    if R.Matches(APattern) then
      Raise EHTTPRoute.CreateFmt(EDuplicateRoute,[APattern]);
    end;
  if isDefault and (DI<>-1) then
    Raise EHTTPRoute.CreateFmt(EDuplicateDefaultRoute,[APattern]);
end;

function TRouter.DoRouteRequest(ARoute: TRoute; const AURL: String;
  AParams: TStrings): TRoute;
begin
  Result:=aRoute;
  if Assigned(Result) then
    Result.HandleRequest(Self,aURL,AParams)
  else
    Raise EHTTPRoute.CreateFmt('No route for URL: %s',[aURL]);
end;

function TRouter.DoRouteRequest(AURL: String; DoPush : Boolean = False): TRoute;

Var
  APath : String;
  Params : TStrings;

begin
  APath:=AURL;
  Params:=TStringList.Create;
  try
    Result:=GetRoute(APath,Params);
    if DoPush then
      Push(aURL)
    else
      Result:=DoRouteRequest(Result,aPath,Params);
  finally
    Params.Free;
  end;
end;

function TRouter.GetRequestPath(const URL: String): String;
begin
  Result:=SanitizeRoute(URL);
end;

function TRouter.RouteFromURL: String;

begin
  // Result:=Copy(window.location.hash,2,Length(window.location.hash)-1);
  // Writeln('Curr ', GetCurrentLocation,' route : ',Result);
  Result:=GetCurrentLocation;
  if (Result<>'') then
    Router.RouteRequest(Result,true);
end;

function TRouter.GetCurrentLocation: String;
begin
  Result:=History.GetCurrentLocation;
end;

function TRouter.Push(location: TRawLocation): TTransitionResult;
begin
  Result:=History.Push(location);
end;

function TRouter.Replace(location: TRawLocation): TTransitionResult;
begin
  Result:=History.Replace(location);
end;

function TRouter.Go(N: integer): TTransitionResult;
begin
  Result:=History.Go(N);
end;

function TRouter.NavigateForward: TTransitionResult;
begin
  Result:=Go(1);
end;

function TRouter.NavigateBack: TTransitionResult;
begin
  Result:=Go(-1);
end;

constructor TRouter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  froutes:=CreateRouteList;
end;

destructor TRouter.Destroy;
begin
  FreeAndNil(FRoutes);
  inherited Destroy;
end;

procedure TRouter.InitHistory(aKind: THistoryKind; aBase : String = '');

begin
  FreeAndNil(FHistory);
  case aKind of
    hkAbstract : FHistory:=TAbstractHistory.Create(Self,aBase);
    hkhash : FHistory:=THashHistory.Create(Self,aBase);
    hkHTML5 : FHistory:=THTMLHistory.Create(Self,aBase);
    hkAuto :
      if TBrowserState.supportsPushState then
        FHistory:=THTMLHistory.Create(Self,aBase)
      else
        FHistory:=THashHistory.Create(Self,aBase);
  end;
  FHistory.SetupListeners;
end;

procedure TRouter.DeleteRoute(AIndex: Integer);
begin
  FRoutes.Delete(Aindex)
end;

procedure TRouter.DeleteRouteByID(AID: Integer);

Var
  R : TCollectionItem;

begin
  R:=FRoutes.FindItemID(AID);
  R.Free;
end;

procedure TRouter.DeleteRoute(ARoute: TRoute);
begin
  ARoute.Free;
end;

class function TRouter.Service: TRouter;
begin
  if FService=Nil then
    FService:=ServiceClass.Create(Nil);
  Result:=FService;
end;

class function TRouter.ServiceClass: TRouterClass;
begin
  If FServiceClass=nil then
    FServiceClass:=TRouter;
  Result:=FServiceClass;
end;

class procedure TRouter.SetServiceClass(AClass: TRouterClass);
begin
  if Assigned(FService) then
    FreeAndNil(FService);
  FServiceClass:=AClass;
end;

function TRouter.RegisterRoute(const APattern: String; AEvent: TRouteEvent; IsDefault: Boolean): TRoute;
begin
  Result:=CreateHTTPRoute(TRouteEventHandler,APattern,IsDefault);
  TRouteEventHandler(Result).Event:=AEvent;
end;

function TRouter.CreateHTTPRoute(AClass : TRouteClass; const APattern: String;IsDefault: Boolean) : TRoute;

begin
  CheckDuplicate(APattern,isDefault);
  Result:=AClass.Create(FRoutes);
  With Result do
    begin
    URLPattern:=APattern;
    Default:=IsDefault;
    end;
end;

function TRouter.RegisterRoute(const APattern: String; const AObjectClass: TRouteObjectClass; IsDefault: Boolean): TRoute;
begin
  Result:=CreateHTTPRoute(TRouteObjectHandler,APattern,IsDefault);
  TRouteObjectHandler(Result).RouteObjectCLass:=AObjectClass;
end;

class function TRouter.SanitizeRoute(const Path: String): String;

begin
  Result:=Path;
end;

function TRouter.FindHTTPRoute(const Path: String; Params : TStrings): TRoute;

Var
  I : Integer;
  APathInfo : String;

begin
  APathInfo:=SanitizeRoute(Path);
  Result:=Nil;
  I:=0;
  While (Result=Nil) and (I<FRoutes.Count) do
    begin
    Result:=FRoutes[i];
    If Not Result.MatchPattern(APathInfo,Params) then
      Result:=Nil;
    Inc(I);
    end;
end;

function TRouter.GetRoute(const Path: String; Params : TStrings): TRoute;

begin
  Result:=FindHTTPRoute(Path,Params);
  if Not Assigned(Result) then
    Raise EHTTPRoute.Create('Not found');
end;

function TRouter.RouteRequest(const ARouteURL: String; DoPush: Boolean): TRoute;

Var
  AURL : String;

begin
  AURL:=ARouteURL;
  If Assigned(FBeforeRequest) then
    FBeforeRequest(Self,AURL);
  Result:=DoRouteRequest(AURL,DoPush);
  If Assigned(FAfterRequest) then
    FAfterRequest(Self,AURL);
end;

{ TRouteList }

function TRouteList.GetR(AIndex : Integer): TRoute;
begin
  Result:=Items[AIndex] as TRoute;
end;

procedure TRouteList.SetR(AIndex : Integer; AValue: TRoute);
begin
  Items[AIndex]:=AValue;
end;

{ TRoute }

Class Function TRoute.NormalizeURLPattern(AValue: String) : String;

Var
  V : String;

begin
  V:=IncludeHTTPPathDelimiter(AValue);
  if (V<>'/') and (V[1]='/') then
    Delete(V,1,1);
  Result:=V;
end;

procedure TRoute.SetURLPattern(AValue: String);

Var
  V : String;

begin
  V:=NormalizeURLPattern(AValue);
  if (FURLPattern=V) then Exit;
  FURLPattern:=V;
end;

function TRoute.Matches(const APattern: String): Boolean;

begin
  Result:=(CompareText(URLPattern,NormalizeURLPattern(APattern))=0)
end;

Function TRoute.MatchPattern(Const Path : String; L : TStrings) : Boolean;

  Function StartsWith(C : Char; S : String): Boolean; 
  
  begin
    Result:=(Length(S)>0) and (S[1]=C);
  end;
  
  Function EndsWith(C : Char; S : String): Boolean; 
  
  Var
  L : Integer;
  
  begin
    L:=Length(S);
    Result:=(L>0) and (S[L]=C);
  end;

  procedure ExtractNextPathLevel(var ALeft: string;
    var ALvl: string; var ARight: string; const ADelim: Char = '/');
  var
    P: Integer;
  begin
    {$IFDEF DEBUGROUTER}Writeln('ExtractNextPathLevel >:',Aleft,' (',aLvl,') ',aRight);{$ENDIF}
    if (ALvl<>ADelim) then
      begin
      ALeft:=ALeft+ALvl;
      if StartsWith(ADelim,ARight) then
        begin
        ALeft:=ALeft+ADelim;
        Delete(ARight,1,1);
        end;
      end;
    P:=Pos(ADelim,ARight);
    if P=0 then
      P:=Length(ARight)+1;
    ALvl:=Copy(ARight,1,P-1);
    ARight:=Copy(ARight,P,MaxInt);
    {$IFDEF DEBUGROUTER} Writeln('ExtractNextPathLevel <:',Aleft,' (',aLvl,') ',aRight);{$ENDIF}
  end;

  procedure ExtractPrevPathLevel(var ALeft: string;
    var ALvl: string; var ARight: string; const ADelim: Char = '/');
  var
    P,L: Integer;
  begin
    {$IFDEF DEBUGROUTER}Writeln('ExtractPrevPathLevel >:',Aleft,' (',aLvl,') ',aRight);{$ENDIF}
    if (ALvl<>ADelim) then
      begin
      ARight:=ALvl+ARight;
      L:=Length(ALeft);
      if EndsWith(ADelim,ALeft) then
        begin
        ARight:=ADelim+ARight;
        Delete(ALeft,L,1);
        end;
      end;
    P:=RPos(ADelim,ALeft);
    ALvl:=Copy(ALeft,P+1,MaxInt);
    ALeft:=Copy(ALeft,1,P);
    {$IFDEF DEBUGROUTER} Writeln('ExtractPrevPathLevel <:',Aleft,' (',aLvl,') ',aRight);{$ENDIF}
  end;

  Procedure AddParam(aName,AValue : String);

  begin
    if Assigned(L) then
      L.Values[aName]:=aValue;
  end;

var
  APathInfo : String;
  APattern : String;
  VLeftPat, VRightPat, VLeftVal, VRightVal, VVal, VPat, VName: string;

begin
  Result:= False;
  if (URLPattern='') then
     Exit; // Maybe empty pattern should match any path?
  APathInfo:=Path;
  APattern:=URLPattern;
  Delete(APattern, Pos('?', APattern), MaxInt);
  Delete(APathInfo, Pos('?', APathInfo), MaxInt);
  if StartsWith('/',APattern) then
    Delete(APattern,1,1);
  if StartsWith('/',APathInfo) then
    Delete(APathInfo,1,1);
  VLeftPat := '';
  VLeftVal := '';
  VPat := '/'; // init value is '/', not ''
  VVal := '/'; // init value is '/', not ''
  VRightPat := APattern;
  VRightVal := APathInfo;
  {$IFDEF DEBUGROUTER}Writeln('Check match on ',URLPattern);{$ENDIF}
  repeat
    // Extract next part
    ExtractNextPathLevel(VLeftPat, VPat, VRightPat);
    ExtractNextPathLevel(VLeftVal, VVal, VRightVal);
      {$IFDEF DEBUGROUTER}Writeln('Pat: ',VPat,' Val: ',VVal);{$ENDIF}
    if StartsWith(':',VPat) then
      AddParam(Copy(VPat,2,Maxint),VVal)
    else
      if StartsWith('*',VPat) then
        begin
        // *path
        VName := Copy(VPat, 2, MaxInt);
        VLeftPat := VRightPat;
        VLeftVal := VVal + VRightVal;
        VPat := '/'; // init value is '/', not ''
        VVal := '/'; // init value is '/', not ''
        VRightPat := '';
        VRightVal := '';
        // if AutoAddSlash ...
        if EndsWith('/',VLeftPat) and not EndsWith('/',VLeftVal) then
          Delete(VLeftPat, Length(VLeftPat), 1);
        repeat
          // Extract backwards
          ExtractPrevPathLevel(VLeftPat, VPat, VRightPat);
          ExtractPrevPathLevel(VLeftVal, VVal, VRightVal);
          if StartsWith(':', VPat) then
            begin
            // *path/:field
            AddParam(Copy(VPat,2,Maxint),VVal);
            end
          else
            // *path/const
            if not ((VPat='') and (VLeftPat='')) and (VPat<>VVal) then
              Exit;
          // Check if we already done
          if (VLeftPat='') or (VLeftVal='') then
            begin
            if VLeftPat='' then
              begin
              if (VName<>'') then
                AddParam(VName,VLeftVal+VVal);
              Result:=True;
              end;
            Exit;
          end;
        until False;
        end
      else
        // const
        if (VPat <> VVal) then
          Exit;
    // Check if we already done
    if (VRightPat='') or (VRightVal='') then
      begin
      if (VRightPat='') and (VRightVal='') then
        Result:=True
      else if (VRightPat='/') then
        Result := True;
      Exit;
      end;
  until False;
end;

function TRoute.FullPath: String;
begin
  Result:=URLPattern;
end;

class function THistory.getLocation(base: string): string;

Var
  path : string;

begin
  path:=window.location.pathname;
  if (base<>'') and (Pos(base,path)=1) then
    path:=Copy(Path,Length(Base)+1,Length(Path)-Length(Base));
  Result:=Path;
  if Result='' then
    Result:='/';
  Result:=Result+window.location.search+window.location.hash
end;

class function THistory.cleanPath(aPath: string): string;
begin
  Result:=StringReplace(aPath,'//','/',[rfReplaceAll]);
end;

function THistory.Push(location: TRawLocation): TTransitionResult;

Var
  Old : TRoute;

begin
  Old:=Current;
  Result:=TransitionTo(location);
  if Result=trOK then
    begin
    Result:=doPush(Location);
    if Result=trOK then
      TWebScroll.Handle(router, Current, Old, false)
    end;
end;

function THistory.Replace(location: TRawLocation): TTransitionResult;

Var
  Old : TRoute;

begin
  Old:=Current;
  Result:=TransitionTo(location);
  if Result=trOK then
    begin
    Result:=doReplace(Location);
    TWebScroll.Handle(Router,Current,Old,false);
    end;

end;

function THistory.Go(N: integer): TTransitionResult;
begin
  Result:=doGo(N);
end;

function THistory.NavigateForward: TTransitionResult;
begin
  Result:=Go(1);
end;

function THistory.NavigateBack: TTransitionResult;
begin
  Result:=Go(-1);
end;


procedure THistory.SetupListeners;
begin
  // Do nothing
end;


function DoScroll(Event: TEventListenerEvent): boolean;

begin
  TWebScroll.SaveScrollPosition;
  Result:=True;
end;

Class Function TWebScroll.GetStateKey : string;

begin
  Result:=TJSDate.New().toString;
end;

Class Procedure TWebScroll.Setup;

begin
//  web.window.;
  window.history.replaceState(New(['key', GetStateKey]), '');
  window.addEventListener('popstate',@DoScroll);
end;

Class Procedure TWebScroll.handle (router: TRouter; ato: TRoute; afrom: TRoute; isPop: boolean) ;

Var
  Position : TScrollPoint;
  ScrollParams : TScrollParams;

begin
  if Not Assigned(Router.OnScroll) then
    Exit;
  position:=getScrollPosition();
  ScrollParams:=Router.OnScroll(Router, aTo, aFrom, position);
  scrollToPosition(ScrollParams);
end;

Var
  positionStore : TJSObject;

Class procedure TWebScroll.saveScrollPosition;

Var
  Key : string;
begin
  key:=getStateKey();
  if Key<>'' then
    positionStore.properties[key]:=New(['x',window.ScrollX,'y',window.ScrollY]);
end;

Class function TWebScroll.getScrollPosition : TScrollPoint;

Var
  Key : string;
  O : JSValue;

begin
  key:=getStateKey();
  Result.X:=0;
  Result.Y:=0;
  if (Key<>'') then
    begin
    O:=positionStore[key];
    if isObject(O) then
      begin
      Result.X:=Double(TJSOBject(O).Properties['x']);
      Result.Y:=Double(TJSOBject(O).Properties['y']);
      end;
  end;
end;

function getElementPosition (el: TJSElement; offset: TScrollPoint): TScrollPoint ;

Var
  DocEl : TJSElement;
  docRect,elRect : TJSDOMRect;

begin
  docEl:=document.documentElement;
  docRect := docEl.getBoundingClientRect();
  elRect := el.getBoundingClientRect();
  Result.x:= elRect.left - docRect.left - offset.x;
  Result.Y:= elRect.top - docRect.top - offset.y;
end;

Class Procedure TWebScroll.ScrollToPosition(AScroll : TScrollParams);


Var
  el : TJSElement;
  P : TScrollPoint;

begin
  if (AScroll.Selector<>'') then
    begin
    el:=document.querySelector(AScroll.Selector);
    if Assigned(el) then
      P:=getElementPosition(el,AScroll.Position)
    else
      P:=AScroll.Position;
    end
  else
    P:=AScroll.Position;
  Window.scrollTo(Round(P.x),Round(P.y));
end;


Class function TBrowserState.genKey (): string ;
begin
  Result:=IntToStr(TJSDate.now);
end;

Class function TBrowserState.getStateKey : string;
begin
  if (TheKey='') then
    TheKey:=genKey;
  Result:=Thekey;
end;

Class Procedure TBrowserState.SetStateKey (akey: string);

begin
  Thekey:=akey;
end;

Class Procedure TBrowserState.pushState (aurl: string; replace: boolean);

Var
  O : TJSObject;

begin
  TWebScroll.SaveScrollPosition;
  try
    if (Not replace) then
      SetStateKey(GenKey);
    O:=New(['key', GetStateKey()]);
    if replace then
      window.history.replaceState(o, '', aUrl)
    else
      window.history.pushState(o, '', aUrl);
  except
    if Replace then
      window.location.replace(aUrl)
    else
      window.location.Assign(aUrl);
  end;
end;

Class Procedure TBrowserState.replaceState(aUrl: string);

begin
  pushState(aUrl, true)
end;

Class Function TBrowserState.supportsPushState : Boolean;

Var
  UA : String;

  Function isB(B : String) : Boolean;

  begin
    Result:=Pos(B,UA)<>0;
  end;

begin
  Result:=False;
  if isDefined(Window) and isDefined(Window.Navigator) then
    begin
    ua:=Window.Navigator.userAgent;
    Result:=Not (
                 IsB('Android 2.')
                 or IsB('Android 4.0')
                 or IsB('Mobile Safari')
                 or IsB('Chrome')
                 or isB('Windows Phone')
                 );
    end;
  If Result then
    Result:=isDefined(Window.history) and isDefined(Window.history);
end;

{ ---------------------------------------------------------------------
  THashHistory
  ---------------------------------------------------------------------}

procedure THashHistory.DoHashChange;

Var
  NewHash : String;
  Old : TRoute;

begin
  NewHash:=NormalizeHash(GetHash);
  if (NewHash=FLastHash) then
    exit;
  old:=Current;
  if TransitionTo(NewHash)=trOK then
    begin
    TWebScroll.Handle(router, Current, old, true);
    FLastHash:=NewHash;
    end
  else
    replaceHash(FLastHash);
end;

procedure THashHistory.SetupListeners;

begin
  if SupportsScroll then
    TWebScroll.Setup;
  if TBrowserState.SupportsPushState then
    Window.addEventListener('popstate',@DoHashChange)
  else
    Window.addEventListener('hashchange',@DoHashChange);
end;

function THashHistory.doPush (location: TRawLocation) : TTransitionResult;

Var
  L : String;

begin
  L:=NormalizeHash(location);
  FLastHash:=L;
  pushHash(L);
  Result:=trOK;
end;

function THashHistory.doreplace(location: TRawLocation): TTransitionResult;

Var
  L : String;

begin
  L:=NormalizeHash(location);
  FLastHash:=L;
  replaceHash(L);
  Result:=trOK;
end;

function THashHistory.doGo(N: integer): TTransitionResult;
begin
  Window.history.go(n);
  result:=trOK;
end;

procedure THashHistory.ensureURL (push : boolean = false);

var
  aHash,CURL: string;

begin
  CURL:=NormalizeHash(FLastHash);
  aHash:=getHash;
   if (aHash<>CURL) then
     if Push then
       pushHash(CURL)
     else
       replaceHash(CURL)
end;

function THashHistory.getCurrentLocation: String;

begin
  Result:=getHash()
end;


class function THashHistory.getHash: string;

Var
  HRef : String;
  Idx : Integer;

begin
  // We can't use window.location.hash here because it's not
  // consistent across browsers - Firefox will pre-decode it!
  HRef:=window.location.href;
  Idx:=Pos('#',HRef);
  if (Idx=0) then
    Result:=''
  else
    Result:=Copy(HRef,Idx+1,Length(HRef)-Idx);
end;

function THashHistory.Kind : THistoryKind;
begin
  Result:=hkHash;
end;

class function THashHistory.getUrl (APath : string) : string;

Var
  HRef : String;
  Idx : Integer;

begin
  HRef:=window.location.href;
  Idx:=Pos('#',HRef);
  if Idx=0 then
    Result:=HRef
  else
    Result:=Copy(HRef,1,Idx-1);
  Result:=Result+'#'+aPath;
end;

class procedure THashHistory.pushHash(path: string);

begin
  if (TBrowserState.supportsPushState) then
    TBrowserState.pushState(getUrl(path),false)
  else
    window.location.hash:=path
end;

class procedure THashHistory.replaceHash(path: string);

Var
  H : String;

begin
  H:=GetHash;
  if (H=Path) then exit;
  if (TBrowserState.supportsPushState) then
    TBrowserState.replaceState(getUrl(path))
  else
    window.location.replace(getUrl(path))
end;



{ ---------------------------------------------------------------------
  TAbstractHistory
  ---------------------------------------------------------------------}

constructor TAbstractHistory.Create (router: TRouter; base: string = '');

begin
  Inherited;
  SetLength(FStack,0);
  FIndex:=-1;
end;

procedure TAbstractHistory.MaybeGrow(AIndex: Integer);

begin
  if AIndex+1>Length(FStack) then
    Setlength(FStack,AIndex+1);
end;

function TAbstractHistory.doPush(location: TRawLocation): TTransitionResult;

begin
  Inc(FIndex);
  MaybeGrow(FIndex);
  FStack[FIndex]:=Location;
  Result:=trOK;
end;

function TAbstractHistory.doReplace(location: TRawLocation): TTransitionResult;

begin
  FStack[FIndex]:=Location;
  Result:=trOK;
end;

function TAbstractHistory.doGo(N: integer): TTransitionResult;

Var
  I : Integer;
  Route : TRoute;

begin
  I:=FIndex+N;
  if (I<0) or (I>=Length(FStack)) then
    Result:=trAbort
  else
    begin
//    Route:=FStack[i];
//    Result:=confirmTransition(Route);
    if (Result=trOK) then
      begin
      FIndex:=0;
      updateRoute(Route);
      end;
    end;
end;

procedure THistory.UpdateRoute(aRoute: TRoute);

begin
  FCurrent:=aRoute;
  if Assigned(FOnChange) then
    FOnChange(aRoute);
end;

function TAbstractHistory.getCurrentLocation: String;

Var
  I : Integer;
  Route : string;

begin
  I:=Length(FStack)-1;
  if (I>=0) then
    Route:=FStack[I]
  else
    Result:='/';
  Result:=Route;
end;

procedure TAbstractHistory.ensureURL(Push: Boolean);

begin
  // Noop
  if Push then ;
end;

function TAbstractHistory.Kind: THistoryKind;
begin
  Result:=hkAbstract;
end;

begin
  positionStore:=new([]);
end.

