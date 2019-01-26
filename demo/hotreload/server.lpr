program server;

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  sysutils, classes, fpjson, contnrs, syncobjs, custhttpapp, fpwebfile,
  httproute, dirwatch, httpdefs;

Type
  TDirWatcher = Class;
  THTTPApplication = Class;

  { TCompileItem }

  TCompileItem = Class(TCollectionItem)
  private
    FCommandLine: string;
    FFileName: string;
    FOutput : TStrings;
    FThread: TThread;
    function GetOutput: TStrings;
  Public
    Property FileName : string Read FFileName Write FFileName;
    Property CommandLine : string Read FCommandLine Write FCommandLine;
    Property Output : TStrings Read GetOutput;
    Property Thread : TThread Read FThread;
  end;

  { TCompiles }

  TCompiles = Class(TCollection)
  private
    function GetC(AIndex : Integer): TCompileItem;
  Public
     Property Compiles[AIndex : Integer] : TCompileItem Read GetC; default;
  end;


  { TCompileThread }

  TCompileThread = class(TThread)
  private
    FItem: TCompileItem;
    procedure SetItem(AValue: TCompileItem);
  Public
    Constructor create(aItem : TCompileItem);
    Procedure Execute; override;
    Property Item : TCompileItem read FItem write SetItem;
  end;

  { TDirWatcher }

  TDirWatcher = Class(TComponent)
  Private
    FApp : THTTPApplication;
    FDW : TDirWatch;
    procedure DoChange(Sender: TObject; aEntry: TDirectoryEntry; AEvents: TFileEvents);
  Public
    Constructor Create(App : THTTPApplication; ADir : String); reintroduce;
    Destructor Destroy; override;
  end;

  { THTTPApplication }

  THTTPApplication = Class(TCustomHTTPApplication)
  private
    FProjectFile: String;
    FStatusLock : TCriticalSection;
    FQuiet: Boolean;
    FWatch: Boolean;
    FDW : TDirWatcher;
    FStatusList : TFPObjectList;
    FCompiles : TCompiles;
    Procedure AddToStatus(AEntry : TDirectoryEntry; AEvents : TFileEvents);
    procedure DoStatusRequest(ARequest: TRequest; AResponse: TResponse);
    procedure DoRecompile(ARequest: TRequest; AResponse: TResponse);
    function ScheduleCompile(const aProjectFile: String; ACommandLine : String = ''): Integer;
    procedure StartWatch(ADir: String);
    procedure Usage(Msg: String);
  public
    Constructor Create(AOWner : TComponent); override;
    Destructor Destroy; override;
    procedure DoLog(EventType: TEventType; const Msg: String); override;
    Procedure DoRun; override;
    property Quiet : Boolean read FQuiet Write FQuiet;
    Property Watch : Boolean Read FWatch Write FWatch;
    Property ProjectFile : String Read FProjectFile Write FProjectFile;
  end;


  { TCompileThread }

procedure TCompileThread.SetItem(AValue: TCompileItem);
begin
  if FItem=AValue then Exit;
  FItem:=AValue;
end;

constructor TCompileThread.create(aItem: TCompileItem);
begin

end;

  procedure TCompileThread.Execute;
begin

end;

{ TCompiles }

function TCompiles.GetC(AIndex : Integer): TCompileItem;
begin
  Result:=Items[Aindex] as TCompileItem;
end;

{ TCompileItem }

function TCompileItem.GetOutput: TStrings;
begin
  If (FOutput=Nil) then
    FOutput:=TStringList.Create;
  Result:=FOutput;
end;


{ TDirWatcher }

procedure TDirWatcher.DoChange(Sender: TObject; aEntry: TDirectoryEntry; AEvents: TFileEvents);
begin
  if Assigned(FApp) then
    FApp.AddToStatus(AEntry,AEvents);
end;

constructor TDirWatcher.Create(App: THTTPApplication; ADir: String);
begin
 Inherited create(APP);
 FApp:=App;
 FDW:=TDirwatch.Create(Self);
 FDW.AddWatch(ADir,allEvents);
 FDW.OnChange:=@DoChange;
 TThread.ExecuteInThread(@FDW.StartWatch);
end;

destructor TDirWatcher.Destroy;
begin
  FApp:=Nil;
  FDW.Terminate;
  FreeAndNil(FDW);
  inherited Destroy;
end;

{ THTTPApplication }

procedure THTTPApplication.DoLog(EventType: TEventType; const Msg: String);
begin
 if Quiet then
   exit;
 if IsConsole then
   Writeln(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now),' [',EventType,'] ',Msg)
 else
   inherited DoLog(EventType, Msg);
end;

procedure THTTPApplication.Usage(Msg : String);

begin
  if (Msg<>'') then
    Writeln('Error: ',Msg);
  Writeln('Usage ',ExtractFileName(ParamStr(0)),' [options] ');
  Writeln('Where options is one or more of : ');
  Writeln('-d --directory=dir  Base directory from which to serve files.');
  Writeln('                    Default is current working directory: ',GetCurrentDir);
  Writeln('-h --help           This help text');
  Writeln('-i --indexpage=name Directory index page to use (default: index.html)');
  Writeln('-n --noindexpage    Do not allow index page.');
  Writeln('-p --port=NNNN      TCP/IP port to listen on (default is 3000)');
  Writeln('-q --quiet          Do not write diagnostic messages');
  Writeln('-w --watch          Watch directory for changes');
  Halt(Ord(Msg<>''));
end;

constructor THTTPApplication.Create(AOWner: TComponent);
begin
  inherited Create(AOWner);
  FStatusLock:=TCriticalSection.Create;
  FStatusList:=TFPObjectList.Create(False);
  FCompiles:=TCompiles.Create(TCompileItem);
end;

destructor THTTPApplication.Destroy;
begin
  FStatusLock.Enter;
  try
    FreeAndNil(FCompiles);
    FreeAndNil(FStatusList);
  finally
    FStatusLock.Leave;
  end;
  FreeAndNil(FStatusLock);
  inherited Destroy;
end;

procedure THTTPApplication.StartWatch(ADir : String);

begin
  FDW:=TDirWatcher.Create(Self,ADir);
end;

procedure THTTPApplication.AddToStatus(AEntry: TDirectoryEntry; AEvents: TFileEvents);
begin
  Log(etDebug,'File change detected: %s (%s)',[AEntry.name,FileEventsToStr(AEvents)]);
  FStatusLock.Enter;
  try
    FStatusList.Add(TJSONObject.Create(['action','file','name',AEntry.name,'events',FileEventsToStr(AEvents)]));
  finally
    FStatusLock.Leave;
  end;
end;

procedure THTTPApplication.DoStatusRequest(ARequest : TRequest; AResponse : TResponse);

Var
  R,O : TJSONObject;
  A : TJSONArray;
  I : integer;
begin
  Log(etDebug,'Status request from: %s',[ARequest.RemoteAddress]);
  R:=Nil;
  try
    FStatusLock.Enter;
    try
      if (FStatusList.Count=0) then
        R:=TJSONObject.Create(['ping',True])
      else
        begin
        O:=FStatusList[0] as TJSONObject;
        FStatusList.Delete(0);
        if O.Get('action','')<>'file' then
          R:=O
        else
          begin
          // If first event is file event, then add and delete all file events in list.
          A:=TJSONArray.Create([O]);
          O.Delete('action');
          R:=TJSONObject.Create(['action','sync','files',A]);
          For I:=FStatusList.Count-1 downto 0 do
            begin
            O:=FStatusList[0] as TJSONObject;
            if (O.Get('action','')='file') then
              begin
              A.Add(O);
              O.Delete('action');
              FStatusList.Delete(I);
              end;
            end;
          end
        end;
    finally
      FStatusLock.Leave;
    end;
    AResponse.ContentType:='application/json';
    AResponse.Content:=R.AsJSON;
    AResponse.SendResponse;
  finally
    R.Free;
  end;
end;

Function THTTPApplication.ScheduleCompile(const aProjectFile : String; ACommandLine : String = '') : Integer;

Var
  CI : TCompileItem;

begin
  CI:=FCompiles.Add as TCompileItem;
  CI.FileName:=AProjectFile;
  CI.FThread:=TCompileThread.Create(CI);
  Result:=CI.ID;
end;

procedure THTTPApplication.DoRecompile(ARequest: TRequest; AResponse: TResponse);

Var
  ID : Integer;
  PF,CL : String;

begin
  PF:=ARequest.ContentFields.Values['ProjectFile'];
  CL:=ARequest.ContentFields.Values['CommandLine'];
  if PF='' then
    PF:=ProjectFile;
  If (PF='') then
    begin
    AResponse.Code:=404;
    AResponse.CodeText:='No project file';
    AResponse.ContentType:='application/json';
    AResponse.Content:='{ "success" : false, "message": "no project file set or provided" }';
    end
  else
    begin
    ID:=ScheduleCompile(PF,CL);
    AResponse.Code:=200;
    AResponse.ContentType:='application/json';
    AResponse.Content:=Format('{ "success" : true, "file": "%s", "commandLine" : "%s", "compileID": %d }',[StringToJSONString(PF),StringToJSONString(CL),ID]);
    end;
end;

procedure THTTPApplication.DoRun;

Var
  S,IndexPage,D : String;

begin
  S:=Checkoptions('hqd:ni:p:wc::',['help','quiet','noindexpage','directory:','port:','indexpage:','watch','compile::']);
  if (S<>'') or HasOption('h','help') then
    usage(S);
  Quiet:=HasOption('q','quiet');
  Watch:=HasOption('w','watch');

  Port:=StrToIntDef(GetOptionValue('p','port'),3000);
  D:=GetOptionValue('d','directory');
  if D='' then
    D:=GetCurrentDir;
  Log(etInfo,'Listening on port %d, serving files from directory: %s',[Port,D]);
  {$ifdef unix}
  {$ifdef darwin}
  MimeTypesFile:='/private/etc/apache2/mime.types';
  {$else}
  MimeTypesFile:='/etc/mime.types';
  {$endif}
  {$endif}
  if Watch then
    StartWatch(D);
  httprouter.RegisterRoute('$sys/status',rmGet,@DoStatusRequest);
  if Hasoption('c','compile') then
    begin
    ProjectFile:=GetOptionValue('c','compile');
    if ProjectFile='' then
      ProjectFile:=IncludeTrailingPathDelimiter(D)+'server.lpr';
    If Not FileExists(ProjectFile) then
      ProjectFile:=IncludeTrailingPathDelimiter(D)+'server.lpr';
    httprouter.RegisterRoute('$sys/compile',rmPost,@DoRecompile);
    end;
  TSimpleFileModule.BaseDir:=IncludeTrailingPathDelimiter(D);
  TSimpleFileModule.OnLog:=@Log;
  If not HasOption('n','noindexpage') then
    begin
    IndexPage:=GetOptionValue('i','indexpage');
    if IndexPage='' then
      IndexPage:='index.html';
    Log(etInfo,'Using index page %s',[IndexPage]);
    TSimpleFileModule.IndexPageName:=IndexPage;
    end;
  TSimpleFileModule.RegisterDefaultRoute;
  inherited;
end;

Var
  Application : THTTPApplication;

begin
  Application:=THTTPApplication.Create(Nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.

