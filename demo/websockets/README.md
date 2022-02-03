# Websocket demo 

This demo is intended to run with the wsserver demo of the FPC websocket
support example wsserver. It is a small chat server client.

# Running the example

To run it, run the FPC wsserver example application  (see fcl-web/examples/websocket/server).
```sh
wsserver -p 8080
```

Edit the serverconfig.js and set the correct server URL:
```json
var 
  serverConfig = {
    "url": "ws://localhost:8080/"
  };
```
(change port etc. to match your setup)

Compile the demowebsocket example using lazarus or pa2js.

run simpleserver (or any other webserver) in this directory:

```sh
simpleserver -p 3000
```

and point your browser at port 3000:
```text
http://localhost:3000/
```


