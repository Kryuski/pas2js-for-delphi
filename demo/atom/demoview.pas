unit demoview;

{$mode objfpc}

interface

uses
  Classes, SysUtils, JS, web;

Type
  // TAtomWorkspaceItem interface
  TAtomView = Class(TObject)
  Public
    constructor create(aState : TJSObject); virtual; abstract;
    function getElement : TJSHTMLElement; virtual; abstract;
    function getTitle : string; virtual;abstract;
    function serialize : TJSObject; virtual;abstract;
  end;

  { TPas2jsdemopackageView }

  TPas2jsdemopackageView = class (TAtomView)
  Private
    FMyElement : TJSHTMlElement;
    FButton: TJSHTMLButtonElement;
    FOnDismiss: TNotifyEvent;
    function DoDismiss(aEvent: TJSMouseEvent): boolean;
  Public
    constructor create(aState : TJSObject); override;
    destructor destroy; override;
    function getElement : TJSHTMLElement; override;
    function getTitle : string; override;
    function serialize : TJSObject; override;
    property onDismiss : TNotifyEvent Read FOnDismiss Write FonDismiss;
  end;


implementation

{ TPas2jsdemopackageView }

function TPas2jsdemopackageView.DoDismiss(aEvent: TJSMouseEvent): boolean;
begin
  if assigned(FOnDismiss) then
    FOnDismiss(Self);
end;

constructor TPas2jsdemopackageView.create(aState: TJSObject);


begin
  FButton:=TJSHTMLButtonElement(document.CreateElement('button'));
  FButton.innerText:='dismiss';
  FButton.onclick:=@DoDismiss;
  FMyElement:=TJSHTMLElement(document.CreateElement('div'));
  FMyElement.innerText:='The Pas2jsdemopackage package is Alive! It''s ALIVE!';
  FMyElement.classList.add('message');
  FMyElement.AppendChild(FButton);
end;

destructor TPas2jsdemopackageView.destroy;
begin
  FMyElement.parentElement.removeChild(FMyElement);
  FMyElement:=Nil;
  inherited destroy;
end;

function TPas2jsdemopackageView.getElement: TJSHTMLElement;
begin
  Result:=FMyElement;
end;

function TPas2jsdemopackageView.getTitle: string;
begin
  Result:='';
end;

function TPas2jsdemopackageView.serialize: TJSObject;
begin
  Result:=TJSObject.New;
end;

end.

