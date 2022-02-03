unit Rtl.UnitLoader;

interface
{ $define DEBUGUNITLOADER}

uses SysUtils, JS, Types;

Type
  EUnitLoader = Class(Exception);

  TLoadedProcedure = Reference to Procedure(const aUnitNames : Array of String; aData : TObject);

  { TLoadTask }

  TLoadTask = Class(TObject)
  Private
    FUnitNames : TStringDynArray; // unit names case sensitive!
    FInitUnitNames : TStringDynArray; // unit names case sensitive!
    FOnLoaded : TLoadedProcedure;
    FData : TObject;
    function GetAllLoaded : Boolean;
  Protected
    Procedure CallLoaded;
  Public
    Constructor Create(Const aUnitNames : Array of string; aOnLoaded : TLoadedProcedure; aData : TObject);
    Procedure UnitLoaded(Const aUnitName : String);
    Property AllLoaded : Boolean Read GetAllLoaded;
    Property LoadUnitNames : TStringDynArray Read FUnitNames;
    Property OnLoaded : TLoadedProcedure Read FOnLoaded;
    Property Data : TObject Read FData;
  end;

  { TUnitLoader }

  TUnitLoader = Class(TObject)
  Private
    Class var FInstance : TUnitLoader;
    procedure DoDependenciesLoaded(const aUnitName: array of string;
      aData: TObject);
  Private
    FBaseURL : String;
    FLoadList : TStringDynArray; // unitnames case sensitive!
    function IndexOfLoadUnit(aUnitName : String): integer;
  protected
    Procedure AddToLoadList(aUnitName : String);
    Procedure RemoveFromLoadList(aUnitName : String);
    function IsInLoadList(aUnitName: String): Boolean;
    function GetUnitURL(const aUnitName: string): String; virtual;
    procedure InitModule(aTask: TLoadTask; const aName: String; aModule : JSValue); virtual;
    procedure DoLoadUnits(const aUnitNames: array of String;  aOnLoaded: TLoadedProcedure; aData: TObject); virtual;
    function AreAllDependenciesLoaded(aTask: TLoadTask; const aName: String;   AModule: JSValue): Boolean; virtual;
    function GetNeededDependencies(const aName: String; AModule: JSValue): TStringDynArray;
    procedure UnitSourcesLoaded(aData : TObject); virtual;
  Public
    Class Function Instance : TUnitLoader;
    function FindModule(aModuleName: string): JSValue;
    function HaveModule(aModuleName: string): Boolean;
    procedure LoadUnit(Const aUnitName : string; aOnLoaded : TLoadedProcedure = Nil; aData : TObject = Nil);
    procedure LoadUnits(Const aUnitNames : Array of String; aOnLoaded : TLoadedProcedure = Nil; aData : TObject = Nil);
    Property BaseURL : String Read FBaseUrl Write FBaseURL;
  end;
 
 
Implementation

uses Rtl.ScriptLoader;

function IndexOfI(arr: TStringDynArray; Name: string): integer;
begin
  Result:=length(arr)-1;
  while (Result>=0) and not SameText(arr[Result],Name) do
    dec(Result);
end;

function TLoadTask.GetAllLoaded: Boolean;
begin
  Result:=Length(FInitunitNames)=0;
end;

procedure TLoadTask.CallLoaded;
begin
  if Assigned(OnLoaded) then
    OnLoaded(LoadUnitNames,Data);
end;

constructor TLoadTask.Create(const aUnitNames: array of string;
  aOnLoaded: TLoadedProcedure; aData: TObject);

Var
  I : Integer;

begin
  SetLength(FunitNames,Length(aUnitNames));
  SetLength(FInitUnitNames,Length(aUnitNames));
  for I:=Low(aUnitNames) to High(aUnitNames) do
    begin
    FUnitNames[i]:=aUnitNames[i];
    FInitUnitNames[i]:=aUnitNames[i];
    end;
  FOnLoaded:=aOnLoaded;
  FData:=aData;
end;

procedure TLoadTask.UnitLoaded(const aUnitName: String);

var
  Idx : integer;

begin
{$IFDEF DEBUGUNITLOADER}Writeln('Unit ',aUnitName,' loaded, removing from list');{$ENDIF}
  Idx:=IndexOfI(FInitUnitNames,aUnitName);
  if Idx>-1 then
    TJSArray(FInitUnitNames).splice(Idx,1);
end;

class function TUnitLoader.Instance: TUnitLoader;

begin
  if (FInstance=Nil) then
    FInstance:=TUnitLoader.Create;
  Result:=FInstance;  
end;

Procedure LoadIntf(aModule : JSValue); external name 'rtl.loadintf';
Procedure LoadImpl(aModule : JSValue); external name 'rtl.loadimpl';
var pas : TJSOBject; external name 'pas';

function TUnitLoader.FindModule(aModuleName: string): JSValue;
var
  Key: string;
begin
  Result:=pas[aModuleName];
  if isModule(Result) then exit;
  for Key in pas do
    begin
    if not SameText(Key,aModuleName) then continue;
    Result:=pas[Key];
    if isModule(Result) then exit;
    end;
  Result:=nil;
end;

function TUnitLoader.HaveModule(aModuleName: string): Boolean;
begin
  Result:=FindModule(aModuleName)<>nil;
end;

procedure TUnitLoader.InitModule(aTask: TLoadTask; const aName: String;
  aModule: JSValue);

begin
{$IFDEF DEBUGUNITLOADER}  Writeln('Unit ',aName,' dependencies loaded. Initialising "',TJSObject(aModule)['$name'],'" ...');{$ENDIF}
  RemoveFromLoadList(aName);
  LoadIntf(aModule);
  LoadImpl(aModule);
  aTask.UnitLoaded(aName);
end;

function TUnitLoader.GetNeededDependencies(const aName: String; AModule: JSValue
  ): TStringDynArray;

var
  l,u : TStringDynArray;
  m : String;
begin
  SetLength(l,0);
  u:=TStringDynArray(TJSOBject(aModule)['$intfuseslist']);
  for m in u do
    if not (HaveModule(m) or IsInLoadList(m)) then
      TJSArray(l).push(m);
  u:=TStringDynArray(TJSOBject(aModule)['$impluseslist']);
  for m in u do
    if not (HaveModule(m) or IsInLoadList(m)) then
      TJSArray(l).push(m);
  Result:=l;
end;

function TUnitLoader.AreAllDependenciesLoaded(aTask: TLoadTask;
  const aName: String; AModule: JSValue): Boolean;

begin
  Result:=Length(GetNeededDependencies(aName,aModule))=0;
end;

procedure TUnitLoader.DoDependenciesLoaded(const aUnitName : array of string; aData : TObject);

begin
  UnitSourcesLoaded(aData);
end;

function TUnitLoader.IndexOfLoadUnit(aUnitName: String): integer;
begin
  Result:=IndexOfI(FLoadList,aUnitName);
end;

procedure TUnitLoader.AddToLoadList(aUnitName: String);
begin
  if IndexOfLoadUnit(aUnitName)<0 then
    TJSArray(FLoadList).Push(aUnitName);
end;

procedure TUnitLoader.RemoveFromLoadList(aUnitName: String);
var
  idx : Integer;
begin
  Idx:=IndexOfLoadUnit(aUnitName);
  if Idx>-1 then
    TJSArray(FLoadList).splice(Idx,1);
end;

function TUnitLoader.IsInLoadList(aUnitName: String): Boolean;
begin
  Result:=IndexOfLoadUnit(aUnitName)>=0;
end;

procedure TUnitLoader.UnitSourcesLoaded(aData : TObject);

Var
  aTask : TLoadTask;
  aModule : JSValue;
  aModuleName : String;
  Deps : TStringDynArray;

begin
{$IFDEF DEBUGUNITLOADER}  Writeln('Succesfully loaded sources');{$ENDIF}
  aTask:=TLoadTask(aData);
  For aModuleName in aTask.LoadUnitNames do
    begin
    aModule:=FindModule(aModuleName);
    if aModule<>nil then
      begin
{$IFDEF DEBUGUNITLOADER}      Writeln(aModuleName+' is module. Loading interface');{$ENDIF}
      Deps:=GetNeededDependencies(aModuleName,aModule);
      if length(Deps)=0 then
        InitModule(aTask,aModuleName,aModule)
      else
        DoLoadUnits(Deps,@DoDependenciesLoaded,aData);
      end;
    end;
  if (aTask.AllLoaded) then
    aTask.CallLoaded;
end;

function TUnitLoader.GetUnitURL(const aUnitName: string): String;

begin
  Result:=BaseURL;
  if (Result<>'') then
    Result:=Result+'/';
  Result:=Result+aUnitname+'.js';
end;

procedure TUnitLoader.LoadUnit(const aUnitName : string; aOnLoaded : TLoadedProcedure = Nil; aData : TObject = Nil);

begin
  LoadUnits([aUnitName],aOnLoaded,aData);
end;

procedure TUnitLoader.LoadUnits(const aUnitNames: array of String; aOnLoaded: TLoadedProcedure; aData: TObject);

begin
  if Length(FLoadList)>0 then
    Raise EUnitLoader.Create('Load operation in progress. Cannot load.');
  DoLoadUnits(aUnitNames,aOnLoaded,aData);
end;

procedure TUnitLoader.DoLoadUnits(const aUnitNames: array of String; aOnLoaded: TLoadedProcedure; aData: TObject);

Var
  Scripts : TStringDynArray;
  aCount : Integer;
  S : String;

begin
  aCount:=0;
  Setlength(Scripts,Length(aUnitNames));
  for s in aUnitNames do
    if Not HaveModule(S) then
      begin
{$IFDEF DEBUGUNITLOADER}      Writeln('Need to load unit: ',S);{$ENDIF}
      Scripts[aCount]:=GetUnitURl(S);
      AddToLoadList(S);
      inc(aCount);
      end;
  SetLength(S,aCount);
  if aCount=0 then
    begin
    // All is already loaded
    if Assigned(aOnLoaded) then
      aOnLoaded(aUnitNames,aData);
    end
  else
    LoadScripts(Scripts,@UnitSourcesLoaded,TLoadTask.Create(aUnitNames,aOnLoaded,aData));
end;

end.
