program democlasstopas;

uses Web,Classes, JS, class2pas, browserconsole;

procedure ShowRTLProps(aClassName,aJSClassName : String; O : TJSObject);
Var
  S : TStrings;
  I : Integer;

begin
  S:=TStringList.Create;
  try
    ClassToPas(aClassName,aJSClassName,'',O,S,True);
    For I:=0 to S.Count-1 do
      Writeln(S[i]);
  finally
    S.Free;
  end;
end;

Var
  o : TJSObject;

begin
  // get the new JavaScript object:
  asm
  $mod.o = window.localStorage;
  end;
  MaxConsoleLines:=5000;
  ShowRTLProps('localStorage','TJSLocalStorage',o);
end.

