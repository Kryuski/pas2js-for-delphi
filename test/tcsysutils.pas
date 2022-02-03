unit tcsysutils;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

Type

  { TTestSysutils }

  TTestSysutils = Class(TTestCase)

  private
    procedure TestFormat(Fmt: String; const Args: array of const;
      aResult: String);
  Published
    Procedure TestFormatSimple;
  end;


implementation

Procedure TTestSysutils.TestFormat(Fmt : String; Const Args : Array of const; aResult : String);

begin
  AssertEquals('Format >>'+Fmt+'<<',aResult,Format(Fmt,Args));
end;

Procedure TTestSysutils.TestFormatSimple;
begin
  // Just 1 data item
  TestFormat('%s', ['Hello'],'Hello');

  // A mix of literal text and a data item
  TestFormat('String = %s', ['Hello'],'String = Hello');

  // Examples of each of the data types
  TestFormat('Decimal          = %d', [-123],'Decimal          = -123');
{$IFDEF PAS2JS}
  TestFormat('Exponent         = %e', [12345.678],'Exponent         = 1.23E+4');
{$ELSE}
  TestFormat('Exponent         = %e', [12345.678],'Exponent         = 1.2345678000000000E+004');
{$ENDIF}
  TestFormat('Fixed            = %f', [12345.678],'Fixed            = 12345.68');
  TestFormat('General          = %g', [12345.678],'General          = 12345.678');
  TestFormat('Number           = %n', [12345.678],'Number           = 12,345.68');
{$IFDEF PAS2JS}
  TestFormat('Money            = %m', [12345.678],'Money            = $12,345.68');
{$ELSE}
  TestFormat('Money            = %m', [12345.678],'Money            = 12,345.68$');
{$ENDIF}
  TestFormat('String           = %s', ['Hello'],'String           = Hello');
  TestFormat('Unsigned decimal = %u', [123],'Unsigned decimal = 123');
  TestFormat('Hexadecimal      = %x', [140],'Hexadecimal      = 8C');
end;

initialization
  RegisterTests([TTestSysUtils]);
end.

