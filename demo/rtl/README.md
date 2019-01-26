### Simple demos for the RTL units

### Compiling for running in node.js:

```
pas2js -Tnodejs -Jirtl.js demojsstring.pas
pas2js -Tnodejs -Jirtl.js demojsregexp.pas
pas2js -Tnodejs -Jirtl.js democollection.pas
pas2js -Tnodejs -Jirtl.js democomponents.lpr
pas2js -Tnodejs -Jirtl.js demostringlist.pas
pas2js -Tnodejs -Jirtl.js demo_njsprocess.pas
```

### Compiling for running in a browser:
```
pas2js -Jc -Jirtl.js demodombuttonevent.pas
pas2js -Jc -Jirtl.js demodocument1.pas
pas2js -Jc -Jirtl.js democollection.pas
pas2js -Jc -Jirtl.js democomponents.lpr
pas2js -Jc -Jirtl.js demoajax.pas
pas2js -Jc -Jirtl.js democanvas2d.pas
pas2js -Jc -Jirtl.js demonew.pas
pas2js -Jc -Jirtl.js democlasstopas.pas
pas2js -Jc -Jirtl.js demodocument1.pas
pas2js -Jc -Jirtl.js demouncaughtexception.pas
pas2js -Jc -Jirtl.js demoxhr.lpr
pas2js -Jc -Jirtl.js dembrowserconsole.lpr
```
When using lazarus, you can also open the respective .lpi files,
and compile your project.
Make sure pas2js is in your path, or the IDE will not find it.

### Run in node.js

To run the code, you need to run
```
nodejs demojsstring.js
nodejs demojsregexp.js
```
etc.

### Run/Show in a browser.
Some of the projects can be run straight from file.
Just open the file in the explorer using your favourite browser.

The ajax demo needs to be run from a webserver, just as the demoxhr demo.

One way to do so, is to compile the simpleserver example program from
the fcl-web examples, and run it in this directory.
Then point your browser to
```http://localhost:3000/ajaxdemo.html```.
