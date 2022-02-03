program tryfetch;

{$mode objfpc}

uses
  browserconsole, JS, Web, SysUtils;

procedure myFetch; async;
var
  response: TJSResponse;
  myBlob: TJSBlob;
  image: TJSHTMLImageElement;
  objectURL: string;
begin
  try
    response := await(window.fetch('pas2js.png'));

    if not response.ok then
      raise Exception.Create('HTTP error! status: '+str(response.status))
    else begin
      myBlob := await(response.blob());
      objectURL := TJSURL.createObjectURL(myBlob);
      image := TJSHTMLImageElement(document.createElement('img'));
      image.src := objectURL;
      document.body.appendChild(image);
    end;
  except
    console.log(JSExceptValue);
  end;
end;

begin
  myFetch;
end.
