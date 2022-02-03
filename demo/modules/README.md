# Module import demos

## Intro
pas2js can convert the $linklib directive to a JS import statement:

The following:

```delphi
{$linklib ./modules/canvas.js canvas}
```

is converted to
```js
import * as canvas from "./modules/canvas.js";
```
If the alias is omitted, one is constructed for you:

```delphi
{$linklib ./modules/some-api.js}
```
is converted to
```js
import * as some_api from "./modules/some-api.js";
```
The import statements are always inserted in the main program. 
This is because modules are like windows libraries: self-contained programs,
which only import and export well-defined symbols.

For this reason, a new target "operating system" has been invented: 
the module. 
This means that you must compile with target module:

```sh
pas2js -Tmodule -Jirtl.js main.pp
```

The nodejs target will also work, but in the future the 2 targets may
diverge.

## Demos

Each directory contains 1 demo. Compile with the command-line as above:

```sh
pas2js -Tmodule -Jirtl.js main.pp
```

### Basic

This shows a simple program, no units, that uses the linklib directive to
import a javascript module. It uses an external class definition to access
the exported symbols from the modules.

### Basic-Units

This is the same as the Basic  example, but the import definitions are split
out in units.

### Flat

This shows a simple program, no units, that uses the linklib directive to
import a javascript module. It uses only external 'name' definitions to access
the exported symbols from the modules; no external class is used.

### Flat-Units

This is the same as the flat example, but the import definitions are split
out in units.





 


