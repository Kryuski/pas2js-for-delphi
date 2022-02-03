program testloader;

uses types, js, web, Rtl.ScriptLoader, Rtl.UnitLoader, FormFactory;


Function DoClick (e : TJSMouseEvent) : boolean;
begin
  LoadScripts([
   'http://ajax.googleapis.com/ajax/libs/jquery/1.3.2/jquery.min.js',
   'http://ajax.googleapis.com/ajax/libs/prototype/1.6.1.0/prototype.js'
  ],
  procedure (data : TObject) begin
     window. alert('All things are loaded');
  end, Nil);
  Result:=false;
end;

Function DoFormClick (e : TJSMouseEvent) : boolean;

  procedure DoLoaded(const aUnitName : array of string; aData : TObject);
  
  Var
    C : TFormClass;
  
  begin
    Writeln('Unit ',aUnitName,' was loaded');
    C:=GetFormClassByName('TMyForm');
    if C<>Nil then
      C.Create(Nil)
    else
      begin
      Writeln('TMyForm not found');  
      window.alert('TMyForm not found');
      end;
  end;

begin
  TUnitLoader.Instance.LoadUnit('myform',@DoLoaded);
  Result:=False;
end;

begin
  TJSHTMLElement(document.getElementbyID('loader')).onclick:=@DoClick;  
  TJSHTMLElement(document.getElementbyID('formloader')).onclick:=@DoFormClick;  
end.
