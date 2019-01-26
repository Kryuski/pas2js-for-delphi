program story3;

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


function GetChapters(aValue : JSValue): JSValue;

  function GetChapter(url : JSValue; index: NativeInt; anArray : TJSArray) : JSValue;

  begin
     Result:=GetJSON(String(url));
  end;

  function ChainRequests(chain, chapterPromise: JSValue; currentIndex: NativeInt; anArray: TJSArray): JSValue;

    Function ReturnChapter(aValue : JSValue) : JSValue;

    begin
      Result:=chapterPromise;
    end;

    Function AddToPage(aValue : JSValue) : JSValue;

    begin
      addHTMLToPage(String(TJSObject(aValue)['html']));
    end;

  begin
    result:=TJSPromise(Chain).
      _then(@ReturnChapter).
      _then(@AddToPage);
  end;


Var
  Story : TJSObject;

begin
  Story:=TJSObject(aValue);
  addHtmlToPage(String(story['heading']));
  Result:=TJSArray(story['chapterUrls']).map(@GetChapter).reduce(@ChainRequests,TJSPromise.Resolve);
end;

begin
  initSlowNetWork;
  getJson('story.json').
    _then(@GetChapters).
    _then(@ShowAllDone).
    catch(@ShowError).
    _then(@HideSpinner);
end.

