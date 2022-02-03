# pas2jsdemoextension VS Code extension.

This package is a small adaptation of the VS Code extensions Hello World
sample.

To install it in VS Code:
* compile the program. 
   You can do so in VS Code: just run the Build command.
* run the package: 
  This will start a second VS Code instance with the package installed.
* To package it, you need to have vsce installed, so first it must be
installed:
```sh
npm install -g vsce
```
Once installed, packaging can be done on the command-line
run
```sh
vsce package
```

This will create a pas2jsdemoextension-0.0.1.vsix file which can be
distributed and installed in VS Code.

