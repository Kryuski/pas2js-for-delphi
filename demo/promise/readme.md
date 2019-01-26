
This directory contains several examples of Javascript Promise support

The askmom example is a basic example, translated from:

https://scotch.io/tutorials/javascript-promises-for-dummies

The demoall example contains some sample code found on the MDN website:
https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/all

The "story"  examples are translated from the examples found on:
https://developers.google.com/web/fundamentals/primers/promises

To run these samples, first compile them using lazarus.
For the story samples, it is best to start a simple webserver in this directory:

```
simpleserver
```

then point your browser at
http://localhost:3000/story.html
http://localhost:3000/story2.html
http://localhost:3000/story3.html

The other examples can of course also be showed in this manner:
http://localhost:3000/demoall.html
http://localhost:3000/askmom.html

It is best to open the developer console for these examples, since some of the logging happens with console.log()
