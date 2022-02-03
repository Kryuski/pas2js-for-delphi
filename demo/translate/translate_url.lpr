program translate_url;

{$mode objfpc}

uses
  JS, Web, rstranslate, mystrings;

Resourcestring
  URLTitle = 'Translation using resource strings - URL API';

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
    procedure DoneTranslation(Sender: TObject; aURL: String);
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
  FPar.InnerHtml:=Paragraph+' '+TranslateURL;
  document.title:=URLTitle;
end;

procedure TTranslateApp.DoneTranslation(Sender : TObject; aURL : String);

begin
  SetTexts;
end;

procedure TTranslateApp.TranslateStrings;

begin
  FIsDutch:=Not FIsDutch;
  if FIsDutch then
    Translate('dutch.json',@DoneTranslation)
  else
    begin
    // Single string of a module
    ResetTranslation('program','URLTitle');
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
