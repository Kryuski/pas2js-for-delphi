program story;

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

Function ShowStory(aJSON : JSValue) : JSValue;

  function ShowChapters(Chain, currentChapter: JSValue; currentIndex: NativeInt;
    anArray: TJSArray): JSValue;

    Function FetchNext(aValue : JSValue) : JSValue;

    begin
      Result:=getJson(String(currentChapter));
    end;

    Function AddToPage(aChapter : JSValue) : JSValue;

    Var
      o : TJSObject;


    begin
      o:=TJSObject(aChapter);
      addHtmlToPage(String(o['html']));
    end;

  begin
     Result:=TJSPromise(chain).
       _then(@FetchNext).
       _then(@AddToPage);
  end;


Var
  Story : TJSObject;

begin
  Story:=TJSObject(aJSON);
  addHtmlToPage(String(story['heading']));
  Result:=TJSArray(story['chapterUrls']).reduce(@ShowChapters,TJSPromise.resolve(null));
end;

begin
  initSlowNetWork;
  getJson('story.json').
    _then(@ShowStory).
    _then(@ShowAllDone).
    catch(@ShowError).
    _then(@HideSpinner);
end.

