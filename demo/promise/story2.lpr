program story2;

{$mode objfpc}

uses
  JS, Web, utils;

Function ShowAllDone(aValue : JSValue) : JSValue;

begin
  addTextToPage('All done');
end;

Function ShowError(aValue : JSValue) : JSValue;

begin
  addTextToPage('Broken: '+TJSError(aValue).Message);
end;

Function HideSpinner(aValue : JSValue) : JSValue;

begin
  TJSHTMLElement(document.querySelector('.spinner')).style.SetProperty('display','none');
end;

Function ShowChapters(Chapters: JSValue) : JSValue;

  function ShowChapter (element : JSValue; index: NativeInt; anArray : TJSArray) : Boolean;

  begin
    addHtmlToPage(String(TJSObject(element)['html']));
  end;

begin
  TJSArray(Chapters).foreach(@ShowChapter);
end;

function GetChapters(aValue : JSValue): JSValue;

  function GetChapter(url : JSValue; index: NativeInt; anArray : TJSArray) : JSValue;

  begin
     Result:=GetJSON(String(url));
  end;

Var
  Story : TJSObject;

begin
  Story:=TJSObject(aValue);
  addHtmlToPage(String(story['heading']));
  Result:=TJSPromise.All(TJSArray(story['chapterUrls']).map(@GetChapter));
end;

begin
  initSlowNetWork;
  getJson('story.json').
    _then(@GetChapters).
    _then(@ShowChapters).
    _then(@ShowAllDone).
    catch(@ShowError).
    _then(@HideSpinner);
end.

