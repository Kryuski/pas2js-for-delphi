Demo for using libpas2js.dll, the dynamic library of the pas2js compiler.

If you use the release, libpas2js.dll is in the bin folder.

If you use trunk and used 'make all' to build you got
  bin\<targetos>-<targetcpu>\libpas2jslib.dll
Copy that file and rename it to libpas2js.dll

You can build libpas2js.dll directly by using Lazarus or lazbuild to compile
  compiler/utils/pas2js/pas2jslib.lpi

Anyway you should now have the library:
libpas2js.so on Linux
libpas2js.dynlib on macOS
libpas2js.dll on Windows

Now compile pas2js_unitalias.lpi
This creates pas2js_unitalias or under Windows pas2js_unitalias.exe.

It simply passes the command line parameters to run the compiler, so it behaves
pretty much like the command line compiler pas2js.

With one exception:
pas2jscompilerproxy.pas Function DoUnitAlias redirects searching for some units:

  if SameText(Old,'Test.Foo.Alias1') then
    New:='Bar'
  else if SameText(Old,'Test.Foo.Alias2') then
    New:='Test.Foo.SomeLongUnitName';

The example unit examples/TestUnitAlias1.pas uses "Test.Foo.Alias1".


Open a console aka terminal, cd to this folder and invoke the compiler:

Linux/Mac:
./pas2js_unitalias -Jc -Tnodejs -Fu../../packages/rtl examples/TestUnitAlias1.pas -vt

Windows:
pas2js_unitalias.exe -Jc -Tnodejs -Fu..\..\packages\rtl examples\TestUnitAlias1.pas -vt

You should see something like this:

Log : Info: Configfile search: /home/mattias/.pas2js.cfg
Log : Info: Configfile search: /home/mattias/pas2js/demo/uselibpas2js/pas2js.cfg
Log : Info: Configfile search: /etc/pas2js.cfg
Log : Info: Compiler exe: "/home/mattias/pas2js/demo/uselibpas2js/pas2js_unitalias"
Log : Info: Using working directory: "/home/mattias/pas2js/demo/uselibpas2js"
Log : Info: Using unit path: "/home/mattias/pas2js/packages/rtl"
Log : Info: Output file: ""
Log : Info: Searching file: examples/System.pp... not found
Log : Info: Searching file: examples/System.pas... not found
Log : Info: Searching file: examples/System.p... not found
Log : Info: Searching file: System.pp... not found
Log : Info: Searching file: System.pas... not found
Log : Info: Searching file: System.p... not found
Log : Info: Searching file: /home/mattias/pas2js/packages/rtl/System.pp... not found
Log : Info: Searching file: /home/mattias/pas2js/packages/rtl/system.pas... found
Info: DoUnitAlias Old="Test.Foo.Alias1" New="Bar"
Log : Info: Searching file: examples/Bar.pp... not found
Log : Info: Searching file: examples/Bar.pas... not found
Log : Info: Searching file: examples/Bar.p... not found
Log : Info: Searching file: Bar.pp... not found
Log : Info: Searching file: Bar.pas... not found
Log : Info: Searching file: Bar.p... not found
Log : Info: Searching file: /home/mattias/pas2js/packages/rtl/Bar.pp... not found
Log : Info: Searching file: /home/mattias/pas2js/packages/rtl/Bar.pas... not found
Log : Info: Searching file: /home/mattias/pas2js/packages/rtl/Bar.p... not found
Log : examples/TestUnitAlias1.pas(8,11) Error: can't find unit "Test.Foo.Alias1"
Log : Fatal: Compilation aborted
Error of class "ECompilerTerminate" raised when compiling : Compiler exited with exit code 6

As you can see when searching unit "Test.Foo.Alias1" it searched instead for unit "Bar".
