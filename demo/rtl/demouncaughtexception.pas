program demouncaughtexception;

uses sysutils;

Type
  EMyError = class(Exception);

begin
  Raise EMyError.Create('This can be shown in an alert by setting rtl.setUncaughtException=true');
end.

