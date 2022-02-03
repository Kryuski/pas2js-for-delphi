unit TestUnitAlias2;

interface

implementation

uses
  Test.Foo.Alias1; // replaced with bar
  //Test.Foo.Alias2; // replaced with Test.Foo.SomeLongUnitName

end.

