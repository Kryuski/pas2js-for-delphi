unit myform;

interface
 
uses classes, FormFactory, MyFormDep;

Type
  TMyForm = Class(TComponent)
  Public
    Constructor Create(aOWner : TComponent); override; 
  end;


implementation

uses web, myformdep2;

Constructor TMyForm.Create(aOWner : TComponent); 


begin
  window.alert('TMyForm created!');
end;

begin
  RegisterForm(TMyForm);
end.
