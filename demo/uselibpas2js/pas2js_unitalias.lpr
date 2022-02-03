program pas2js_unitalias;

uses
  {$IFDEF FPC}
  {$IFDEF unix}
  cmem, cwstring,
  {$ENDIF}
  DynLibs,
  {$ENDIF}
  SysUtils, LibPas2jsIntf, Pas2jsCompilerProxy;

begin
  LoadPas2JsLibrary(ExtractFilePath(ParamStr(0))+libpas2js);
  With TPas2JSCompilerProxy.Create do
    try
      Execute;
    finally
      Free;
    end;
end.

