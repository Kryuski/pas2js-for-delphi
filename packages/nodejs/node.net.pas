{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2020 by Michael Van Canneyt

    NodeJS net module import.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit node.net;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  Classes, SysUtils, js, nodejs;

type
  TNJSStreamDuplex = class external name 'stream.Duplex' (TJSObject);

  TJSHandleObj = class external name 'Object' (TJSObject);

  TJSNetSocket = class external name 'net.Socket' (TJSObject);

  TNJSNetCallback = reference to procedure(Obj : TJSObject);
  TNJSNetGetConnectionsCallback = reference to procedure(err : TJSObject; count : Integer);
  TNJSNetListenCallBack = reference to procedure(err : TJSObject);

  TJSNetServerOptions = class external name 'Object' (TJSObject)
    port : cardinal;
    host : string;
    path : string;
    backlog : Cardinal;
    exclusive : boolean;
    readableAll : Boolean;
    writableAll : Boolean;
    ipv6only : Boolean;
  end;

  TNJSNetServer = class external name 'net.Server' (TJSObject)
    listening : boolean;
    maxConnections : NativeInt;
    procedure close(Callback : TNJSNetCallback = Nil);
    procedure getConnections(CallBack : TNJSNetGetConnectionsCallback);
    function listen(handle : TJSHandleObj) : TNJSNetServer;
    function listen(handle : TJSHandleObj; BackLog : Cardinal) : TNJSNetServer;
    function listen(handle : TJSHandleObj; BackLog : Cardinal; ErrCallback : TNJSNetListenCallBack) : TNJSNetServer;
    function listen(Options : TJSObject) : TNJSNetServer;
    function listen(Options : TJSObject; ErrCallback : TNJSNetListenCallBack) : TNJSNetServer;
    function listen(Options : TJSNetServerOptions) : TNJSNetServer;
    function listen(Options : TJSNetServerOptions; ErrCallback : TNJSNetListenCallBack) : TNJSNetServer;
    function listen(Port : Cardinal) : TNJSNetServer;
    function listen(Port : Cardinal; ErrCallback : TNJSNetListenCallBack) : TNJSNetServer;
    function listen(Port : Cardinal; Host : string) : TNJSNetServer;
    function listen(Port : Cardinal; Host : string; ErrCallback : TNJSNetListenCallBack) : TNJSNetServer;
    function listen(Port : Cardinal; Host : string; backlog : Cardinal) : TNJSNetServer;
    function listen(Port : Cardinal; Host : string; backlog : Cardinal; ErrCallback : TNJSNetListenCallBack) : TNJSNetServer;
    procedure ref;
    procedure unref;
  end;

implementation

end.

