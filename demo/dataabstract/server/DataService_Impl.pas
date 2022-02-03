unit DataService_Impl;

{$I RemObjects.inc}

interface

uses
  System.SysUtils, System.Classes, System.TypInfo, uROEncoding, uROXMLIntf,
  uROClientIntf, uROClasses, uROTypes, uROServer, uROServerIntf, uROSessions,
  uRORemoteDataModule, uDAInterfaces, uDAServerInterfaces, uDADelta,
  uDABusinessProcessor, uDASchema, DataAbstractService_Impl, DataAbstract4_Intf,
  DAServer_Intf, uROComponent, uDAStreamableComponent, uDAClientSchema,
  uDAFields, uDADataStreamer, uDABin2DataStreamer, uDACore, uDASchemaClasses,
  uDAScriptingProvider;

const
  __ServiceName = 'DataService';

type
  { Forward declarations }
  TDataService = class;

  TDataService = class(TDataAbstractService, IDataService)
    dsBin: TDABin2DataStreamer;
    DABusinessProcessor1: TDABusinessProcessor;
    Schema: TDASchema;
    procedure DABusinessProcessor1BeforeProcessChange(
      Sender: TDABusinessProcessor; aChangeType: TDAChangeType;
      aChange: TDADeltaChange; var ProcessChange: Boolean);
  private
  end;

implementation

{$IFDEF DELPHIXE2UP}
  {%CLASSGROUP 'Vcl.Controls.TControl'}
{$ENDIF}
{$IFNDEF FPC}
  {$R *.dfm}
{$ENDIF}
{$IFDEF FPC}
  {$R *.lfm}
{$ENDIF}

uses
  Variants,DAServer_Invk, Modules.Server;

var
  FDataService: IROClassFactory;

procedure Create_DataService(out anInstance: IInterface);
begin
  anInstance := TDataService.Create(nil);
end;

{ TBaseService }

procedure TDataService.DABusinessProcessor1BeforeProcessChange(
  Sender: TDABusinessProcessor; aChangeType: TDAChangeType;
  aChange: TDADeltaChange; var ProcessChange: Boolean);
begin
  if aChangeType in [ctInsert,ctUpdate] then
    begin
    if not VarIsNull(aChange.NewValueByName['ISO']) then
      aChange.NewValueByName['ISO']:=UpperCase(aChange.NewValueByName['ISO']);
    if not VarIsNull(aChange.NewValueByName['ISO3']) then
      aChange.NewValueByName['ISO3']:=UpperCase(aChange.NewValueByName['ISO3']);
    end;
  ProcessChange:=True;
end;

initialization
  FDataService := TROClassFactory.Create(__ServiceName, {$IFDEF FPC}@{$ENDIF}Create_DataService, TDataService_Invoker);
  // RegisterForZeroConf(FBaseService, '_BaseService_rosdk._tcp.');

finalization
  UnRegisterClassFactory(FDataService);
  FDataService := nil;

end.
