{
    This file is part of the Pas2JS run time library.
    Copyright (C) 2019 Silvio Clecio (silvioprog) and
    Fernando Baroni (frbaroni).

    Pascal mapping for PushJS: https://pushjs.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Compact and cross-browser solution for Notifications API. }

unit PushJS;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

interface

uses
  JS;

{ TODO:
   - Plugins: https://pushjs.org/docs/plugins }

type

  { TPushFunction }

  TPushFunction = reference to procedure;

  { TPushOptions }

  TPushOptions = class external name 'Object'
    // The body text of the notification
    body: string;
    // Data to pass to ServiceWorker notifications
    data: JSValue;
    // Can be either the URL to an icon image or an array containing 16x16 and 32x32 pixel icon images (see above).
    icon: string;
    // A relative URL path to navigate to when the user clicks on the notification on mobile (e.g. if you want users
    // to navigate to your page http://example.com/page, then the relative URL is just page). If the page is already
    // open in the background, then the browser window will automatically become focused. Requires the serviceWorker.js
    // file to be present on your server to work.
    link: string;
    // When set to true, the notification will not close unless the user manually closes or clicks on it
    requireInteraction: Boolean;
    // Unique tag used to identify the notification. Can be used to later close the notification manually.
    tag: string;
    // Time in milliseconds until the notification closes automatically
    timeout: Integer;
    // An array of durations for a mobile device receiving the notification to vibrate. For example, [200, 100] would
    // vibrate first for 200 milliseconds, pause, then continue for 100 milliseconds. For more information, see below.
    // Available in Mobile Chrome only.
    vibrate: Boolean;
    // Specifies whether the notification should be silent, i.e. no sounds or vibrations should be issued, regardless
    // of the device settings.
    // Supported only in Chrome 43.0 or higher.
    silent: Boolean;
    // Callback to execute when the notification is clicked
    onClick: TPushFunction;
    // Callback to execute when if the notification throws an error
    onError: TPushFunction;
    // Creates new push options
    constructor new;
  end;

  { TPushFallbackPayload }

  TPushFallbackPayload = class external name 'Object'
    // Notification title
    title: string;
    // Notification body
    body: string;
    // Notification tag
    tag: string;
    // Notification icon
    icon: string;
  end;

  { TPushParamsFallback }

  TPushParamsFallback = reference to procedure(payload: TPushFallbackPayload);

  { TPushParams }

  TPushParams = class external name 'Object'
    // Sets a custom service worker script
    serviceWorker: string;
    // Code that executes on browsers with no notification support
    // "payload" is an object containing the
    // title, body, tag, and icon of the notification
    fallback: TPushParamsFallback;
  end;

  { TPushPermission }

  TPushPermission = class external name 'Object'
  private
    FDEFAULT: string; external name 'DEFAULT';
    FGRANTED: string; external name 'GRANTED';
    FDENIED: string; external name 'DENIED';
  public
    // Requests permission for desktop notifications
    procedure request(
      // Function to execute once permission is granted
      onGranted: TPushFunction;
      // Function to execute once permission is denied
      onDenied: TPushFunction);
    // Returns whether Push has been granted permission to run
    function has: Boolean;
    // Gets the permission level
    function get: string;
    // 'default'
    property DEFAULT: string read FDEFAULT;
    // 'granted'
    property GRANTED: string read FGRANTED;
    // 'denied'
    property DENIED: string read FDENIED;
  end;

  { TPush }

  TPush = class external name 'Push' (TJSPromise)
  private class var
    FPermission: TPushPermission; external name 'Permission';
  public
    // Creates and displays a new notification
    class function create(
      // Notification title
      const title: string): TPush; overload; external name 'create';
    // Creates and displays a new notification
    class function create(
      // Notification title
      const title: string;
      // Notification options
      params: TPushOptions): TPush; overload; external name 'create';
    // Returns the notification count
    class function count: Integer;
    // Closes a notification with the given tag
    // Return boolean denoting success
    class function close(
      // Tag of the notification to close
      const tag: string): Boolean;
    // Clears all notifications
    // Return boolean denoting whether the clear was successful in closing all notifications
    class function clear: Boolean;
    // Denotes whether Push is supported in the current browser
    class function supported: Boolean;
    // Modifies settings or returns all settings if no parameter passed
    class procedure config(params: TPushParams);
    // Permission object
    class property Permission: TPushPermission read FPermission;
  end;

implementation

end.
