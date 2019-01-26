program demobrowserconsole;

uses browserconsole;


Var
  I : Integer;

begin
  For I:=30 downto 1 do
    Writeln(I,', Hello, world!');
  ConsoleStyle:=DefaultCrtConsoleStyle;
  InitConsole;
  For I:=1 to 30 do
    Writeln(I,', Hello, world ?');
end.
