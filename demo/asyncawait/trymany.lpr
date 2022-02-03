program tryfetchmany;

{$mode objfpc}

uses
  browserconsole, JS, Web, SysUtils, Types;

function FetchBlob(url: string): TJSBlob; async;
var
  response: TJSResponse;
begin
  response := await(window.fetch(url));
  if not response.ok then
    raise Exception.create('HTTP error! status: '+str(response.status))
  else
    Result:=await(response.blob());
end;

function FetchText(url: string): string; async;
var
  response: TJSResponse;
begin
  response := await(window.fetch(url));
  if not response.ok then
    raise Exception.create('HTTP error! status: '+str(response.status))
  else
    Result:=await(response.text());
end;

procedure DisplayContent; async;
var
  coffee, tea, description: TJSPromise;
  objectURL1, objectURL2, descText: String;
  values: TJSValueDynArray;
  image1, image2: TJSHTMLImageElement;
  para: TJSHTMLElement;
begin
  try
    coffee := FetchBlob('pas2js.png');
    tea := FetchBlob('fpc.png');
    description := FetchText('description.txt');

    values := await(TJSValueDynArray,TJSPromise.all([coffee, tea, description]));

    objectURL1 := TJSURL.createObjectURL(values[0]);
    objectURL2 := TJSURL.createObjectURL(values[1]);
    descText := string(values[2]);

    image1 := TJSHTMLImageElement(document.createElement('img'));
    image2 := TJSHTMLImageElement(document.createElement('img'));
    image1.src := objectURL1;
    image2.src := objectURL2;
    document.body.appendChild(image1);
    document.body.appendChild(image2);

    para := TJSHTMLElement(document.createElement('p'));
    para.textContent := descText;
    document.body.appendChild(para);

  except
    writeln(JSExceptValue);
  end;
end;

begin
  DisplayContent;
end.
