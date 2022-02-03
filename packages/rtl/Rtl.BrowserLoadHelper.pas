unit Rtl.BrowserLoadHelper;

{$mode objfpc}

interface

uses
  Classes, SysUtils, JS, Web;

Type
  { TBrowserLoadHelper }

  TBrowserLoadHelper = Class (TLoadHelper)
  Public
    Class Procedure LoadText(aURL : String; aSync : Boolean; OnLoaded : TTextLoadedCallBack; OnError : TErrorCallBack); override;
    Class Procedure LoadBytes(aURL : String; aSync : Boolean; OnLoaded : TBytesLoadedCallBack; OnError : TErrorCallBack); override;
  end;

implementation

{ TBrowserLoadHelper }

class procedure TBrowserLoadHelper.LoadText(aURL: String; aSync: Boolean; OnLoaded: TTextLoadedCallBack; OnError: TErrorCallBack);

  function doFetchOK(response : JSValue) : JSValue;

  var
    Res : TJSResponse absolute response;

  begin
    Result:=False;
    If (Res.status<>200) then
      begin
      If Assigned(OnError) then
        OnError('Error '+IntToStr(Res.Status)+ ': '+Res.StatusText)
      end
    else
      Res.Text._then(
        function (value : JSValue) : JSValue
          begin
          OnLoaded(String(value));
          end
      );
  end;

  function doFetchFail(response : JSValue) : JSValue;

  begin
    Result:=False;
    OnError('Error 999: unknown error');
  end;

begin
  if ASync then
    Window.Fetch(aURl)._then(@DoFetchOK).catch(@DoFetchFail)
  else
    With TJSXMLHttpRequest.new do
      begin
      open('GET', aURL, False);
      AddEventListener('load',Procedure (oEvent: JSValue)
        begin
        OnLoaded(responseText);
        end
      );
      AddEventListener('error',Procedure (oEvent: JSValue)
        begin
        if Assigned(OnError) then
          OnError(TJSError(oEvent).Message);
        end
      );
      send();
      end;
end;

class procedure TBrowserLoadHelper.LoadBytes(aURL: String; aSync: Boolean; OnLoaded: TBytesLoadedCallBack; OnError: TErrorCallBack);

  function doFetchFail(response : JSValue) : JSValue;

  begin
    Result:=False;
    if isObject(Response) and (TJSObject(Response) is TJSError) then
      OnError('Error 999: '+TJSError(Response).Message)
    else
      OnError('Error 999: unknown error');
  end;


  function doFetchOK(response : JSValue) : JSValue;

  var
    Res : TJSResponse absolute response;

  begin
    Result:=False;
    If (Res.status<>200) then
      begin
      If Assigned(OnError) then
        OnError('Error '+IntToStr(Res.Status)+ ': '+Res.StatusText)
      end
    else
      Res.Blob._then(
        function (value : JSValue) : JSValue
          begin
          TJSBlob(Value).ArrayBuffer._then(function(arr : JSValue) : JSValue
            begin
            OnLoaded(TJSArrayBuffer(arr))
            end
          ).Catch(@DoFetchFail);
          end
        );
  end;


  function StringToArrayBuffer(str : string) : TJSArrayBuffer;

  Var
    i,l : Integer;

  begin
    L:=Length(str);
    Result:=TJSArrayBuffer.New(l*2); // 2 bytes for each char
    With TJSUint16Array.New(Result) do
      for i:=1 to L do
        Values[i-1]:=Ord(Str[i]);
  end;

begin
  if ASync then
    Window.Fetch(aURl)._then(@DoFetchOK).catch(@DoFetchFail)
  else
    With TJSXMLHttpRequest.new do
      begin
      open('GET', aURL, False);
      AddEventListener('load',Procedure (oEvent: JSValue)
        begin
        if Status<>200 then
          OnError('Error '+IntToStr(Status)+ ': '+StatusText)
        else
          OnLoaded(StringToArrayBuffer(responseText));
        end
      );
      AddEventListener('error',Procedure (oEvent: JSValue)
        begin
        if Assigned(OnError) then
          OnError(TJSError(oEvent).Message);
        end
      );
      send();
      end;
end;

initialization
  SetLoadHelperClass(TBrowserLoadHelper);
end.

