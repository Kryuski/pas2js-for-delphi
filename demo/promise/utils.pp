unit utils;

{$mode objfpc}{$H+}

interface

uses
  sysutils, JS, web;

var
  fakeSlowNetwork : boolean;

function wait(ms : NativeInt) : TJSPromise;
function get(url : string) : TJSPromise;
function getJson(url : string) : TJSPromise;
procedure addHtmlToPage(Content : string);
Procedure addTextToPage(content : string) ;
Procedure InitSlowNetwork;

implementation

function wait(ms : NativeInt) : TJSPromise;

  procedure doTimeout(resolve,reject : TJSPromiseResolver) ;

  begin
    window.setTimeout(TJSTimerCallBack(resolve),ms);
  end;

begin
  Result := TJSPromise.New(@doTimeOut);
end;

Procedure InitSlowNetwork;

Const
  lsKey = 'fake-slow-network';

Var
  networkFakeDiv : TJSHTMLElement;
  checkbox : TJSHTMLInputElement;

  procedure doChange;

  begin
    window.localStorage.setItem(lsKey,IntToStr(Ord(checkbox.checked)));
    window.location.reload(false);
  end;


begin
  networkFakeDiv:=TJSHTMLElement(document.querySelector('.network-fake'));
  checkbox:=TJSHTMLInputElement(networkFakeDiv.querySelector('input'));
  fakeSlowNetwork:=window.localStorage.getItem(lsKey)='1';
  networkFakeDiv.style.setProperty('display','block');
  checkbox.checked:=fakeSlowNetwork;
  checkbox.addEventListener('change',@doChange)
end;

function get(url : string) : TJSPromise;

// Return a new promise.

  // We do all the work within the constructor callback.
  procedure DoRequest(resolve,reject : TJSPromiseResolver) ;

  var
    req : TJSXMLHttpRequest;

    function DoOnLoad(event : TEventListenerEvent) : boolean;

    begin
      // On error we reject, otherwise we resolve
      if (req.status=200) then
        resolve(req.responseText)
      else
        reject(TJSError.New(req.statusText));
    end;

    function DoOnError(event : TEventListenerEvent) : boolean;

    begin
      // On error we reject
      reject(TJSError.New('Network Error'));
    end;

  begin
    req:=TJSXMLHttpRequest.new;
    req.open('get', url);
    req.addEventListener('load',@DoOnLoad);
    req.addEventListener('error',@DoOnError);
    req.send();
  end;

  function ReturnResult(res: JSValue) : JSValue;

  begin
    // Result is an array of resolve values of the 2 promises, so we need the second one.
    Result:=TJSArray(res)[1];
  end;

var
  fakeNetworkWait,
  requestPromise : TJSPromise;

begin
  fakeNetworkWait := Wait(trunc(3000 * random * Ord(fakeSlowNetwork)));
  requestPromise:=TJSPromise.New(@DoRequest);
  Result:=TJSPromise.all([fakeNetworkWait, requestPromise])._then(@ReturnResult);
end;

function getJson(url : string) : TJSPromise;

  Function DoConvert(aValue : JSValue) : JSValue;

  begin
    Result:=TJSJSON.parse(String(aValue));
  end;

begin
  Result:=get(url)._then(@DoConvert);
end;

procedure addHtmlToPage(Content : string);

var
  aDiv, storyDiv : TJSElement;

begin
  aDiv:=document.createElement('div');
  StoryDiv:=document.querySelector('.story');
  aDiv.innerHTML:=content;
  storyDiv.appendChild(aDiv);
end;


Procedure addTextToPage(content : string) ;

var
  aDiv, storyDiv : TJSElement;

begin
  aDiv:=document.createElement('p');
  StoryDiv:=document.querySelector('.story');
  aDiv.textContent:=content;
  storyDiv.appendChild(aDiv);
end;

end.

