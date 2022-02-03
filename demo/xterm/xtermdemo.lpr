program xtermdemo;

{$mode objfpc}

uses
  browserapp, JS, Classes, SysUtils, Web, xterm;

type

  { TMyApplication }

  TMyApplication = class(TBrowserApplication)
    FTerminal : TXTerm.TTerminal;
    FTermEl : TJSHTMLElement;
    Finp : TJSHTMLInputElement;
    FBtnSend : TJSHTMLButtonElement;
    procedure doRun; override;
  private
    function DoData(data: String): Boolean;
    function DoKey(key : TXTerm.TOnKeyCallbackDataType): Boolean;
    function DoSendClick(aEvent: TJSMouseEvent): boolean;
  end;

function TMyApplication.DoKey(key : TXTerm.TOnKeyCallbackDataType): Boolean;

Var
  printable : Boolean;

begin
  Result:=true;
  With key do
    printable :=Not (domEvent.altKey or domEvent.metaKey or domEvent.ctrlKey or domEvent.metaKey);
  if key.domEvent.Key=TJSKeyNames.BackSpace then
    FTerminal.write(#8' '#8)
  else if key.domEvent.Key=TJSKeyNames.Enter then
    FTerminal.writeln('')
  else if Printable then
    FTerminal.write(Key.Key)

end;

function TMyApplication.DoData(data: String): Boolean;

begin
  Result:=True;
  if Data=#8 then
    begin
    console.log('backspace detected');
    FTerminal.Write(data+' '+data)

    end
  else
    begin
    console.log('OnData : ',Data, (length(data)));
    FTerminal.Write(data)
    end;
end;

procedure TMyApplication.doRun;

begin
  FTermEl:=GetHTMLElement('xterm');
  FInp:=TJSHTMLInputElement(GetHTMLElement('edtInput'));
  FBtnSend:=TJSHTMLButtonElement(GetHTMLElement('btnSend'));
  FBtnSend.OnClick:=@DoSendClick;
  FTerminal:=TXTerm.TTerminal.New;
  FTerminal.open(FTermEl);
  // FTerminal.OnData(@DoData);
  FTerminal.OnKey(@DoKey);
  Terminate;
end;

function TMyApplication.DoSendClick(aEvent: TJSMouseEvent): boolean;
begin
  FTerminal.Writeln(Finp.Value);
end;

var
  Application : TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;

end.
