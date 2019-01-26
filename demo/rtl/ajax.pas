unit ajax;

{$mode objfpc}{$H+}

interface

uses
  Classes, Web;

type

  { TAjax }

  TAjax = class
  private
    FOnLoad: TJSEventHandler;
    FXmlHttpRequest: TJSXMLHttpRequest;
    procedure SetOnLoad(AValue: TJSEventHandler);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Open(AMethod, AUrl: string);
    property OnLoad: TJSEventHandler write SetOnLoad;
  end;

implementation

{ TAjax }

procedure TAjax.SetOnLoad(AValue: TJSEventHandler);
begin
  FXmlHttpRequest.addEventListener('load', AValue);
end;

constructor TAjax.Create;
begin
  FXmlHttpRequest := TJSXMLHttpRequest.new;
end;

destructor TAjax.Destroy;
begin
//  FXmlHttpRequest.Free;
end;

procedure TAjax.Open(AMethod, AUrl: string);
begin
  FXmlHttpRequest.open(AMethod, AUrl, true);
  FXmlHttpRequest.send;
end;

end.

