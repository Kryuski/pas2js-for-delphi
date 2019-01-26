### 2 Simple demos for the webrouter unit

The 2 demos do the same, the difference is in how they achieve the same result:

demorouter is the simplest. It registers 5 static routes, one for each form.

demorouter2 is more advanced. It registers 1 route with a parameter.


### Compiling for running in the browser:

```
pas2js -browser -Jc -Jirtl.js demorouter.pas
pas2js -browser -Jc -Jirtl.js demorouter2.pas
``` 

When using lazarus, you can also open the respective .lpi files,
and compile your project.

### Choosing a history model:
The router supports 3 models
- Abstract (only for testing purposes)
- Hash: the history is created by adding the route in the #hash part of te URL
- HTML5: the history is created by using the window.history.pushState() API of the browser.
  Most modern browsers support this, but it requires server-side support to work safely.

When in doubt, use the hash mechanism :
```
Router.InitHistory(hkHash)  
```

### Server support.
If the history mechanism has set the url to e.g. http://localhost:3000/Form3
and the user returns back to this URL, the server needs to know it should serve the demorouter.html file;
it needs to be configured that it serves the 'main page' of the project
if it encounters an URL that it doesn't know.

You can start a small node server that does this:

for demorouter.html:
```
nodejs histsrv.js
```

for demorouter2.html:
```
nodejs histsrv2.js
```
