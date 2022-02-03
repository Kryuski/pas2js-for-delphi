{ Unit that emulates console output in the browser.

  Copyright (C) 2020- Michael Van Canneyt michael@freepascal.org

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version
  with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this library with independent modules
  to produce an executable, regardless of the license terms of these independent modules,and to copy and distribute the resulting
  executable under terms of your choice, provided that you also meet, for each linked independent module, the terms and conditions
  of the license of that module. An independent module is a module which is not derived from or based on this library. If you
  modify this library, you may extend this exception to your version of the library, but you are not obligated to do so. If you do
  not wish to do so, delete this exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to the Free
  Software Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit browserconsole;

{$mode objfpc}

interface

uses
  js, web, Rtl.BrowserLoadHelper,sysutils;

Const
  BrowserLineBreak = #10;
  DefaultMaxConsoleLines = 25;
  DefaultConsoleStyle = '.pasconsole { '+BrowserLineBreak+
  'font-family: courier;'+BrowserLineBreak+
  'font-size: 14px;'+BrowserLineBreak+
  'background: #FFFFFF;'+BrowserLineBreak+
  'color: #000000;'+BrowserLineBreak+
  'display: block;'+BrowserLineBreak+
  '}';
  DefaultCRTConsoleStyle = '.pasconsole { '+BrowserLineBreak+
  'font-family: courier;'+BrowserLineBreak+
  'font-size: 14px;'+BrowserLineBreak+
  'background: #000;'+BrowserLineBreak+
  'color: #14fdce;'+BrowserLineBreak+
  'display: block;'+BrowserLineBreak+
  '}';

Var
  // Element ID for console output. If this is not set, body is used.
  // If you cange this, call HookConsole.
  ConsoleElementID : String;
  // Style to use for console lines. If you change this, call initconsole
  ConsoleStyle : String;
  // Style to use for console lines. You can change this at any time.
  MaxConsoleLines : Integer;
  // Copy console lines on newline to browser console log
  ConsoleLinesToBrowserLog : Boolean;

// Clear console content
Procedure ResetConsole;
// Re-initialize console (style)
Procedure InitConsole;
// Re-hook console
Procedure HookConsole;

implementation


Var
  LastLine,
  StyleElement,
  LinesParent,
  ConsoleElement : TJSElement;


Procedure AppendLine;

Var
  CurrentCount : Integer;
  S : TJSNode;

begin
  CurrentCount:=0;
  S:=LinesParent.firstChild;
  While Assigned(S) do
    begin
    Inc(CurrentCount);
    S:=S.nextSibling;
    end;
  While CurrentCount>MaxConsoleLines do
    begin
    Dec(CurrentCount);
    LinesParent.removeChild(LinesParent.firstChild);
    end;
  LastLine:=Document.createElement('div');
  LastLine.className:='pasconsole';
  LinesParent.AppendChild(LastLine);
end;


Function EscapeString(S : String) : String;

Var
  CL : string;

begin
  cl:=StringReplace(S,'<','&lt;',[rfReplaceAll]);
  cl:=StringReplace(cl,'>','&gt;',[rfReplaceAll]);
  cl:=StringReplace(cl,' ','&nbsp;',[rfReplaceAll]);
  cl:=StringReplace(cl,#13#10,'<br>',[rfReplaceAll]);
  cl:=StringReplace(cl,#10,'<br>',[rfReplaceAll]);
  cl:=StringReplace(cl,#13,'<br>',[rfReplaceAll]);
  Result:=CL;
end;

Procedure WriteConsole(S : JSValue; NewLine : Boolean);

Var
  CL: String;

begin
  CL:=LastLine.InnerHtml;
  CL:=CL+EscapeString(String(S));
  LastLine.InnerHtml:=CL;
  if NewLine then
    begin
    if ConsoleLinesToBrowserLog then
      console.log(LastLine.InnerText);
    AppendLine;
    end;
end;

Procedure ResetConsole;


begin
  if LinesParent=Nil then exit;
  While LinesParent.firstElementChild<>Nil do
    LinesParent.removeChild(LinesParent.firstElementChild);
  AppendLine;
end;

Procedure InitConsole;

begin
  if ConsoleElement=Nil then
     exit;
  if (TJSString(ConsoleElement.nodeName).toLowerCase<>'body') then
     begin
     While ConsoleElement.firstElementChild<>Nil do
       ConsoleElement.removeChild(ConsoleElement.firstElementChild);
     end;
  StyleElement:=Document.createElement('style');
  StyleElement.innerText:=ConsoleStyle;
  ConsoleElement.appendChild(StyleElement);
  LinesParent:=Document.createElement('div');
  ConsoleElement.appendChild(LinesParent);
end;

Procedure HookConsole;

begin
  ConsoleElement:=Nil;
  if (ConsoleElementID<>'') then
    ConsoleElement:=document.getElementById(ConsoleElementID);
  if (ConsoleElement=Nil) then
    ConsoleElement:=document.body;
  if ConsoleElement=Nil then
    exit;
  InitConsole;
  ResetConsole;
  SetWriteCallBack(@WriteConsole);
end;

initialization
  ConsoleLinesToBrowserLog:=True;
  ConsoleElementID:='pasjsconsole';
  ConsoleStyle:=DefaultConsoleStyle;
  MaxConsoleLines:=DefaultMaxConsoleLines;
  HookConsole;
end.

