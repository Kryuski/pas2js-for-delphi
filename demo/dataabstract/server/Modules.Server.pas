unit Modules.Server;

interface

uses
  System.SysUtils, System.Classes, uROClientIntf, uROServerIntf,
  uROCustomRODLReader, uROComponent, uROBaseConnection, uROServer,
  uROCustomHTTPServer, uROBaseHTTPServer, uROIndyHTTPServer, System.TypInfo,
  uROMessage, uROBinMessage, uDAServerInterfaces, uDAStreamableComponent,
  uDAConnectionManager, uROSessions, uDADBSessionManager, uDAInterfaces,
  uDAFields, uDADelta, uDADataStreamer, uDABin2DataStreamer, uDADriverManager,
  Data.DB, uRODBSessionManager, uDAClientSchema, uDASchema, uDAEngine,
  uDAFireDACDriver, FireDAC.Phys.PGDef, FireDAC.Stan.Intf, FireDAC.Phys,
  FireDAC.Phys.PG, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Comp.Client, FireDAC.Comp.DataSet;

type
  TdmServer = class(TDataModule)
    svrHTTP: TROIndyHTTPServer;
    msgBIN: TROBinMessage;
    cmConnection: TDAConnectionManager;
    dmDriver: TDADriverManager;
    drvFireDAC: TDAFireDACDriver;
    smMemory: TROEventSessionManager;
  private
    FActive: Boolean;
    FSessionManager: TROCustomSessionManager;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetSessionManager: TROCustomSessionManager;
    function GetPort: Word;
    procedure SetPort(const Value: Word);
    { Private declarations }
  public
    Property SessionManager : TROCustomSessionManager read GetSessionManager;
    property Active: Boolean read GetActive write SetActive;
    Property Port : Word Read GetPort Write SetPort;
    { Public declarations }
  end;

var
  dmServer: TdmServer;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  uDAConnections;

{ TdmServer }

function TdmServer.GetActive: Boolean;
begin
  Result := FActive;
end;


function TdmServer.GetPort: Word;
begin
  Result:=svrHTTP.Port;
end;

function TdmServer.GetSessionManager: TROCustomSessionManager;
begin
  Result:=smMemory;
end;

procedure TdmServer.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
    FActive := Value;
  svrHTTP.Active := FActive;
end;

procedure TdmServer.SetPort(const Value: Word);
begin
  svrHTTP.Port:=Value;
end;

end.
