program DAServer;

{$APPTYPE CONSOLE}
{#ROGEN:DALibrary.rodl} // RemObjects: Careful, do not remove!

uses
  uROComInit,
  uROServer,
  uROBinMessage,
  uROIndyHTTPServer,
  Modules.Server in 'Modules.Server.pas' {dmServer: TDataModule},
  DataService_Impl in 'DataService_Impl.pas' {DataService: TDataAbstractService},
  Units.Types in '..\Common\Units.Types.pas',
  DAServer_Intf in 'DAServer_Intf.pas',
  DAServer_Invk in 'DAServer_Invk.pas';

{$R *.res}
{$R RODLFile.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  Writeln('DA - Command Line Server');

  dmServer := TdmServer.Create(nil);
  try
    dmServer.Active := True;



    WriteLn('Server is active and listening on port ',dmServer.Port,' press Enter to stop.');
    Readln;
  finally
    dmServer.Free;
  end;
end.
