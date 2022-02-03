program htmlloadlinkdemo;

{$mode objfpc}

uses
  Classes, Web, p2jsres;

{$R left.png}
{$R right.png}
{$R up.png}
{$R down.png}

Const
  MaxImages = 4;
  ImageResources : Array[1..MaxImages] of string = ('up','right','down','left');

Var
  Img : TJSHTMLImageElement;
  CurrentImage: Integer = 1;

Procedure  ShowCurrentImage;

Var
  aInfo : TResourceInfo;

begin
  if not GetResourceInfo(ImageResources[CurrentImage],aInfo) then
    Writeln('No info for image ',ImageResources[CurrentImage])
  else
    Img.Src:='data:'+aInfo.format+';base64,'+aInfo.Data;
end;

function RotateImage(aEvent: TJSMouseEvent): boolean;
begin
  Result:=False;
  Inc(CurrentImage);
  if CurrentImage>MaxImages then
    CurrentImage:=1;
  ShowCurrentImage;
end;

procedure OnLoaded(const LoadedResources: array of String);

Var
  S : String;

begin
  // Some info
  for S in LoadedResources do
    Writeln('Found resource: ',S);
  Img.OnClick:=@RotateImage;
  ShowCurrentImage;
end;

procedure OnLoadFailed(const aError: string);
begin
  window.alert('Failed to load resources : '+AError)
end;

begin
  SetResourceSource(rsHTML);
  Img:=TJSHTMLImageElement(Document.GetElementByID('theimage'));
  LoadHTMLLinkResources('htmlloadlinkdemo-res.html',@OnLoaded,@OnLoadFailed);
end.
