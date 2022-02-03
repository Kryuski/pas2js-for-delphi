program translate_object;

{$mode objfpc}

uses
  JS, Web, mystrings, rstranslate;

Resourcestring
  ObjectTitle = 'Translation using resource strings - Object API';

Type

  { TTranslateApp }

  TTranslateApp = Class
    FDutch : TJSObject;
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
  FDutch:=New([
    'program',new([
      'BasicTitle','Vertaling met resourcestrings - directe API'
    ]),
    'mystrings', new([
      'Button','Vertaal deze pagina',
      'Header','Vertaling met resourcestrings',
      'Paragraph','Deze tekst wordt vertaald.',
      'TranslateDirect','De directe API wordt gebruikt voor dit voorbeeld.',
      'TranslateJSON','Een JSON object wordt gebruikt voor dit voorbeeld.',
      'TranslateURL','Een URL wordt gebruikt voor dit voorbeeld.'
    ])
  ]);
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
  FPar.InnerHtml:=Paragraph+' '+TranslateJSON;
  document.title:=ObjectTitle;
end;


procedure TTranslateApp.TranslateStrings;

begin
  FIsDutch:=Not FIsDutch;
  if FIsDutch then
    Translate(FDutch)
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
