{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2020 by Michael Van Canneyt

    NodeJS url module import.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit node.url;

{$mode objfpc}
{$ModeSwitch externalclass}

interface

uses
  Types, JS, nodeJS;

Type
  TNJSURLSearchParams = Class external name 'URLSearchParams' (TJSObject)
    constructor new; overload;
    constructor new(aQueryString : string);overload;
    constructor new(aObject : TJSObject);overload;
    constructor new(aMap : TJSMap);overload;
    constructor new(aArray : TJSArray);overload;
    procedure append(const aName, aValue : string);
    procedure delete(const aName : string);
    function entries : TJSIterator;
    function get(const aName : string) : string;
    function getAll(const aName : string) : TStringDynArray;
    function has(const aName : string) : boolean;
    function keys : TJSIterator;
    procedure set_(const aName, aValue : string); external name 'set';
    procedure sort;
    function toString : string;
    function values: TJSIterator;
  end;

  TNJSURL = Class external name 'URL' (TJSObject)
  Public
    hash : string;
    host : string;
    hostname : string;
    href : string;
    origin : string;
    password : string;
    pathname : string;
    port : string;
    protocol : string;
    search : String;
    searchParams : TNJSURLSearchParams;
    username : string;
    constructor new(aURL : string); overload;
    constructor new(aURL,aBase : string); overload;
    constructor new(aURL : string; aBase : TNJSURL); overload;
    function toString : string;
    function toJSON : string;
  end;

  TJNSURLGlobalFormatOptions =  class external name 'Object' (TJSObject)
    auth : boolean;
    fragment : boolean;
    search : boolean;
    unicode : boolean;
  end;

  TJNSUrlGlobal =  class external name 'Object' (TJSObject)
    class function domainToASCII(adomain : string) : string;
    class function domainToUnicode(adomain : string) : string;
    class function fileURLToPath(aURL : string) : string;
    class function format(aURL : TNJSURL; Options : TJSObject) : string;
    class function format(aURL : TNJSURL; Options : TJNSURLGlobalFormatOptions) : string;
    class function pathToFileURL(aPath : string) : TNJSURL;
  end;

var
  url : TJNSUrlGlobal;

implementation

initialization
  url:=TJNSUrlGlobal(require('url'));
end.

