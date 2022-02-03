program helloworld;

{$MODE OBJFPC}

uses
  JS,
  Web,
  PushJS;

procedure optionClick;
begin
  window.focus;
  TJSWindow(JSThis).close;
end;

procedure buttonClick;
var
  option: TPushOptions;
begin
  option := TPushOptions.new;
  option.body := 'How''s it hangin';
  option.icon := '/icon.png';
  option.timeout := 4000;
  option.onClick := @optionClick;
  TPush.create('Hello world!', option);
end;

begin
  document.querySelector('.button').addEventListener('click', @buttonClick);
end.
