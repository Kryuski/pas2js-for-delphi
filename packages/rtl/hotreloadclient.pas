unit hotreloadclient;

{$mode objfpc}

interface

uses sysutils, types, js, web;

Type

  { THotReloadOptions }

  THotReloadOptions = Class
  private
    FLog: Boolean;
    FName: string;
    FOverlayElementID: String;
    FPath: String;
    FPollInterval: Cardinal;
    FReload: Boolean;
    FTimeOut: Cardinal;
    FWarn: Boolean;
  Public
    Constructor Create; reintroduce;
    Property Path : String Read FPath Write FPath;
    Property Timeout : Cardinal Read FTimeOut Write FTimeOut;
    Property PollInterval : Cardinal Read FPollInterval Write FPollInterval;
    Property Reload : Boolean Read FReload Write FReload;
    Property Log : Boolean Read FLog Write FLog;
    Property Warn : Boolean Read FWarn Write FWarn;
    property Name : string Read FName write FName;
    Property OverlayElementID : String Read FOverlayElementID Write FOverlayElementID;
  end;

  THotReload = Class
  private
    FOptions : THotReloadOptions;
    FLastReq : TJSXMLHttpRequest;
    function doAbort(Event: TEventListenerEvent): boolean;
    function doStatus(Event: TEventListenerEvent): boolean;
    function GetLineColor(aLine: String): String;
    function GetLineStyle(aLine: String): String;
    procedure HandleMessage(aData: TJSObject);
    procedure Reload;
    procedure ShowLogOverlay(O: TStringDynArray);
  Protected
    class var Global : THotReload;
    procedure OnTick; virtual;
  public
    Constructor Create; reintroduce;
    Constructor Create(AOptions : THotReloadOptions);
    Class Procedure StartHotReload;
    Class Procedure StopHotReload;
    class function getGlobal : THotReload;
    procedure Initialize;
    Property Options : THotReloadOptions Read FOptions;
  end;

implementation

{ THotReload }

constructor THotReload.Create;
begin
  Create(THotReloadOptions.Create);
end;

constructor THotReload.Create(AOptions: THotReloadOptions);
begin
  FOptions:=AOptions;
  Initialize;
end;

class procedure THotReload.StartHotReload;
begin
  Global:=THotReload.Create;
end;

class procedure THotReload.StopHotReload;
begin
  FreeAndNil(Global);
end;

class function THotReload.getGlobal: THotReload;
begin
  result:=Global;
end;

function THotReload.doAbort(Event: TEventListenerEvent): boolean;

begin
  if Event=nil then ;
  if Options.log then
    console.warn('Status request aborted');
  FLastReq:=Nil;
  Result:=false;
end;

Procedure THotReload.Reload;

begin
  window.location.reload(true);
end;

function THotReload.GetLineColor(aLine : String) : String;

var
  P : Integer;

begin
  P:=Pos(':',ALine);
  if (P>0) then
    begin
    Aline:=Copy(ALine,1,P-1);
    P:=Pos(' ',ALine);
    if P>0 then
      ALine:=Copy(ALine,P+1,Length(Aline)-P);
    end;
  case lowercase(aline) of
   'error': Result:='E36049';
   'note': Result:='B3CB74';
   'warning': Result:='FFD080';
   'hint': Result:='7CAFC2';
   'info': Result:='7FACCA';
   'fatal': Result:='E36049';
  else
    Result:='EBE7E3';
  end;
end;

function THotReload.GetLineStyle(aLine : String) : String;

Var
  Color : String;

begin
  color:=getLineColor(aLine);
  Result:='background-color:#' + color + '; color:#fff; padding:2px 4px; border-radius: 2px';
end;

Procedure THotReload.ShowLogOverlay(O : TStringDynArray);
Const
  DefaultStyle =
    'background: rgba(0,0,0,0.85);'+
    'color: #E8E8E8;'+
    'lineHeight: 1.2;'+
    'fontFamily: Menlo, Consolas, monospace;'+
    'fontSize: 13px;'+
    'left: 0;'+
    'right: 0;'+
    'top: 0;'+
    'bottom: 0;'+
    'dir: ltr;'+
    'textAlign: left';

Var
  D,SP,MN : TJSElement;
  N : TJSNode;
  I : Integer;
begin
  Writeln('Searching for '+Options.OverlayElementID);
  D:=document.getElementById(Options.OverlayElementID);
  if D=Nil then
    exit;
  // Clear
  N:=D.firstElementChild;
  While (N<>Nil) do
    begin
    D.removeChild(N);
    N:=D.firstElementChild;
    end;
  D['style']:=DefaultStyle;
  For I:=0 to Length(O)-1 do
    begin
    MN:=document.createElement('div');
    SP:=Document.createElement('span');
    SP['style']:=GetLineStyle(O[i]);
    SP.appendChild(document.createTextNode(O[i]));
    MN.appendChild(SP);
    D.appendChild(MN);
    end;
end;

Procedure THotReload.HandleMessage(aData : TJSObject);

Var
  a : JSValue;
  O : TStringDynArray;
  I : integer;

begin
  if Options.Log then
    console.log('Status ',aData);
  if isDefined(aData['ping']) then
    exit;
  a:=aData['action'];
  if isUnDefined(a) or not isString(a) then
    exit;
  case String(a) of
    'building' :
       if Options.Log then
         Console.log(Options.Name+': Server is building job ID '+String(aData['compileID']));
    'built' :
       begin
       if Options.Log then
         Console.log(Options.Name+': Server has built job ID '+String(aData['compileID']));
       if isArray(aData['output']) then
         begin
         O:=TStringDynArray(aData['output']);
         For I:=0 to Length(o)-1 do
           if Pos('Error: ',O[i])>0 then
             Console.Error(O[i])
           else
             Console.Log(O[i]);
         if (Options.OverlayElementID<>'') then
           ShowLogOverlay(O);
         end;
       if isBoolean(aData['success']) and (Boolean(aData['success'])) and Options.reload then
         begin
         if Options.Log then
           Console.log(Options.Name+': Reloading page');
         Reload;
         end;
       end;
    'sync' :
      begin
      if Options.Reload then
        begin
        if Options.Log then
          Console.log(Options.Name+': Resync event. Reloading page');
        Reload;
        end
      else if Options.Log then
        Console.log(Options.Name+': Resync event. Ignoring');
      end;
  else
    if Options.Log then
      console.warn('Unknown status data', TJSJSON.stringify(aData));
  end;
end;

function THotReload.doStatus(Event: TEventListenerEvent): boolean;

Var
  Data : TJSObject;

begin
  if Event=nil then ;
  if Options.log then
    console.warn('Status received');
  try
    Data:=TJSJSON.parseObject(FLastReq.responseText);
    HandleMessage(Data);
  except
    console.error('Error parsing JSON status text: '+FLastReq.responseText);
  end;
  FLastReq:=Nil;
  Result:=True;
end;

procedure THotReload.OnTick;

Var
  Req : TJSXMLHttpRequest;

begin
  if Options.log then
    console.log('tick');
  if (FLastReq<>Nil) then
    Exit;
  Req:=TJSXMLHttpRequest.new;
  Req.addEventListener('load',@DoStatus);
  Req.addEventListener('abort',@DoAbort);
  Req.open('GET',Options.Path);
  Req.send;
  FLastReq:=Req;
end;

procedure THotReload.Initialize;

begin
  console.log('init');
  if isunDefined(window) then
    exit; // Cannot do anything
  console.log('init 2');
  Window.setInterval(@OnTick,Options.PollInterval);
end;

{ THotReloadOptions }

constructor THotReloadOptions.create;
begin
  FPath:='/$sys/status';
  FTimeOut:=20*1000;
  FPollInterval:=1000;
  FLog:=True;
  FWarn:=True;
  FName:='hotreload';
end;

end.

