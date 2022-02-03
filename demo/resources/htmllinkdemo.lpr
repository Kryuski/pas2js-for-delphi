program htmllinkdemo;

{$mode objfpc}

uses
  JS, Classes, SysUtils, Web, p2jsres;

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
  Inc(CurrentImage);
  if CurrentImage>MaxImages then
    CurrentImage:=1;
  ShowCurrentImage;
end;

begin
  SetResourceSource(rsHTML);
  Img:=TJSHTMLImageElement(Document.GetElementByID('theimage'));
  Img.OnClick:=@RotateImage;
  ShowCurrentImage;
end.
