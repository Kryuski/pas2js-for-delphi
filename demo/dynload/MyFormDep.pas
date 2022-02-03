unit MyFormDep;

interface

uses myformdep2;

Procedure DoLoaded;

implementation

Procedure DoLoaded;

begin
  Writeln('loaded');
end;

initialization
  DoLoaded;
end.  
