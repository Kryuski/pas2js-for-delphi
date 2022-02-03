unit Rtl.ScriptLoader;

interface

uses types;

Type
  TloadedCallBack = Reference to procedure(Data : TObject);
  TProc = reference to procedure;
    
Procedure loadScripts(scripts : TStringDynArray; callback : TLoadedCallback; Data : TObject);

implementation

uses js, web;

Procedure loadScripts(scripts : TStringDynArray; callback : TLoadedCallback; Data : TObject);

  Procedure loader (src : String; handler : TProc);
  
  var 
    head,script : TJSElement;

    Procedure DoLoaded;
    
    begin
      script.Properties['onload']:=Nil;
      script.Properties['onreadystatechange']:=Nil;
      Handler;
    end;
    
  begin
    script:= document.createElement('script');
    script['src'] := src;
    script.Properties['onload'] := @DoLoaded;
    script.Properties['onreadystatechange']:=@DoLoaded;
    head:=TJSElement(document.getElementsByTagName('head')[0]);
    if Head=Nil then
      Head:=Document.body;
    head.appendChild( script );
  end; 
    
  Procedure run;
  begin
    if Length(Scripts)<>0 then
      loader(String(TJSArray(scripts).shift()), @run)
    else if Assigned(callback) then
      callback(data);
  end;
        
begin
  Run;
end;

end. 
