program translate_basic;

{$mode objfpc}

uses
  JS, Web, mystrings, rstranslate;

Resourcestring
  BasicTitle = 'Translation using resource strings - Basic API';

Type

  { TTranslateApp }

  TTranslateApp = Class
    FHeader : TJSHTMLELement;
    FPar : TJSHTMLELement;
    FButton : TJSHTMLButtonELement;
    FIsDutch : Boolean;
    Constructor Create;
    Function GetEl(const aName : string) : TJSHTMLELement;
    Procedure Bind;
    Procedure SetTexts;
  private
    function DoTranslation(aEvent: TJSMouseEvent): boolean;
    procedure TranslateStrings;
  end;

{ TTranslateApp }

constructor TTranslateApp.Create;
begin
  Bind;
end;

function TTranslateApp.GetEl(const aName: string): TJSHTMLELement;
begin
  Result:=TJSHTMLELement(Document.getElementById(aName));
end;

procedure TTranslateApp.Bind;
begin
  FHeader:=GetEl('translate-header');
  FPar:=GetEl('translate-text');
  FButton:=TJSHTMLButtonELement(GetEl('btn-translate'));
  FButton.onclick:=@DoTranslation;
end;

procedure TTranslateApp.SetTexts;
begin
  FHeader.InnerHTML:=Header;
  FButton.InnerHTML:=Button;
  FPar.InnerHtml:=Paragraph+' '+TranslateDirect;
  document.title:=BasicTitle;
//  window. title:=BasicTitle;
end;

procedure TTranslateApp.TranslateStrings;

begin
  FIsDutch:=Not FIsDutch;
  if FIsDutch then
    begin
    Translate('program','BasicTitle','Vertaling met resourcestrings - directe API');
    Translate('mystrings','Button','Vertaal deze pagina');
    Translate('mystrings','Header','Vertaling met resourcestrings');
    Translate('mystrings','Paragraph','Deze tekst wordt vertaald.');
    Translate('mystrings','TranslateDirect','De directe API wordt gebruikt voor dit voorbeeld.');
    Translate('mystrings','TranslateJSON','Een JSON object wordt gebruikt voor dit voorbeeld.');
    Translate('mystrings','TranslateURL','Een URL wordt gebruikt voor dit voorbeeld.');
    end
  else
    begin
    // Single string of a module
    ResetTranslation('program','BasicTitle');
    // All strings in a module
    ResetTranslation('mystrings');
    end;
end;

function TTranslateApp.DoTranslation(aEvent: TJSMouseEvent): boolean;
begin
  TranslateStrings;
  SetTexts;
  Result:=True;
end;

begin
  With TTranslateApp.Create do
    SetTexts;
end.
