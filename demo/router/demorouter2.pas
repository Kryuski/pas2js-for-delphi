program demorouter2;

{$mode objfpc}
{$H+}

uses web, classes, js, sysutils, webrouter, frmdemo;

Procedure ShowForm (URl : String; aRoute : TRoute; Params: TStrings);

Var
  s : string;

begin
  S:=Params.Values['ID'];
  document.body.innerHTML:='';
  TDemoForm.Create(StrToIntDef(S,1),True);
end;

begin
  // Leave this if you want to use the #hash history mechanism.
  // This will work in all cases.
  Router.InitHistory(hkHash);
  // Uncomment this if you want to use HTML5 history.
  // Router.InitHistory(hkHTML5,'http://localhost:3000/');
  Router.RegisterRoute('form/:ID',@ShowForm,True);
  Router.Push('form/1');
end.

