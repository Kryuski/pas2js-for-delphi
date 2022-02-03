unit webassembly;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  js, Web;

Type
  { TJSWebAssemblyMemory }

  TJSWebAssemblyMemoryDescriptor = record
    initial,
    maximum : integer;
  end;

  TJSWebAssemblyMemory = class external name 'WebAssembly.Memory' (TJSObject)
  private
    FBuffer: TJSArrayBuffer; external name 'buffer';
    FLength: NativeInt; external name 'length';
  Public
    constructor new (memorydescriptor : TJSWebAssemblyMemoryDescriptor);
    constructor new (memorydescriptor : TJSObject);
    Property buffer : TJSArrayBuffer Read FBuffer;
    Property length: NativeInt Read FLength;
  end;

  { TJSModulesArray }

  TJSModulesExports = Class external name 'Object' (TJSObject)
  private
    FMemory : TJSWebAssemblyMemory; external name 'memory';
    function GetFun(aName : String): TJSFunction; external name '[]';
  public
    Property Memory : TJSWebAssemblyMemory Read FMemory;
    Property functions [aName : String] : TJSFunction read GetFun; default;
  end;

  TJSModulesImports =  TJSModulesExports;

  TJSWebAssemblyModule = class external name 'WebAssembly.Module' (TJSObject)
    constructor new(buffer : TJSArrayBuffer);
    Class Function customSections(module: TJSWebAssemblyModule; const SectionName : string) : TJSArrayBuffer;
    Class Function exports_(module: TJSWebAssemblyModule) : TJSModulesExports; external name 'exports';
    Class Function imports(module: TJSWebAssemblyModule) : TJSModulesImports; external name 'exports';
  end;

  { TJSWebAssemblyInstance }

  TJSWebAssemblyInstance = class external name 'WebAssembly.Instance' (TJSObject)
  private
    Fexports: TJSModulesExports; external name 'exports';
  public
    constructor new(module : TJSWebAssemblyModule; ImportObject : TJSOBject);
    constructor new(module : TJSWebAssemblyModule);
    Property exports_ : TJSModulesExports Read Fexports;
  end;

  { TJSInstantiated }
  // Result returned when calling instantiate with buffer

  { TJSInstantiateResult }

  TJSInstantiateResult = Class external name 'anon' (TJSObject)
  private
    FInstance: TJSWebAssemblyInstance; external name 'instance';
    FModule: TJSWebAssemblyModule;  external name 'module';
  public
    Property Module : TJSWebAssemblyModule Read FModule;
    Property Instance : TJSWebAssemblyInstance Read Finstance;
  end;

  TJSWebAssembly = class external name 'WebAssembly' (TJSObject)
    Class Function instantiate(Buffer : TJSArrayBuffer; ImportObject :  TJSObject) : TJSPromise; overload;
    Class Function instantiate(Buffer : TJSArrayBuffer) : TJSPromise; overload;
    Class Function instantiate(Buffer : TJSWebAssemblyModule; ImportObject :  TJSObject) : TJSPromise; overload;
    Class Function instantiate(Buffer : TJSWebAssemblyModule) : TJSPromise; overload;
    Class Function compile(Buffer : TJSArrayBuffer): TJSPromise;
    Class Function compileStreaming(source : TJSResponse): TJSPromise;
    Class Function instantiateStreaming(source : TJSResponse; ImportObject :  TJSObject) : TJSPromise; overload;
    Class Function instantiateStreaming(source : TJSResponse) : TJSPromise; overload;
    Class Function validate(Buffer : TJSArrayBuffer): Boolean;
  end;

  { TJSWebAssemblyTable }

  TJSWebAssemblyTableDescriptor = record
    element : string;
    initial,
    maximum : integer;
  end;

  TJSWebAssemblyTable = class external name 'WebAssembly.Table' (TJSObject)
  private
    FLength: NativeInt;
  Public
    constructor new (tabledescriptor : TJSWebAssemblyTableDescriptor);
    constructor new (tabledescriptor : TJSObject);
    Property length: NativeInt Read FLength;
  end;


implementation

end.

