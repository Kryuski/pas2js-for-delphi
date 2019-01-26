program demorouter;
{$mode objfpc}
{$H+}

uses web, classes, js, sysutils, webrouter, frmdemo;

Procedure ShowForm (URl : String; aRoute : TRoute; Params: TStrings);

Var
  s : string;

begin
  document.body.innerHTML:='';
  S:=aRoute.URLPattern;
  if (Copy(S,Length(S),1)='/') then
    S:=Copy(S,1,Length(S)-1);
  Delete(S,1,4);
  TDemoForm.Create(StrToIntDef(S,1));
end;

begin
  // Leave this if you want to use the hash history.
  // This will work in all cases.
  Router.InitHistory(hkHash);
  // Uncomment this if you want to use the HTML5 pushstate history mechanism.
  // Note that you must server the files from a webserver then.
  // See also the histsrv.js node server which will "correctly" serve all files.
  // Router.InitHistory(hkHTML5,'http://localhost:3000/');
  Router.RegisterRoute('form1',@ShowForm,True);
  Router.RegisterRoute('form2',@ShowForm,False);
  Router.RegisterRoute('form3',@ShowForm,False);
  Router.RegisterRoute('form4',@ShowForm,False);
  Router.RegisterRoute('form5',@ShowForm,False);
  Router.Push('form1');
end.

