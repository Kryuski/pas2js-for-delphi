const vscode = require('vscode');
const pascalRuntime = require('./pas2jsdemoextension.js');

var callbacks = {
    onDeactivate: function (a) { }
}

// this method is called when your extension is activated
function activate(context) {
    pascalRuntime.rtl.run();
    var vscodeEnv = {
        vscodeGlobal: vscode,
        extensionContext: context
    }
  pascalRuntime.pas.program.InitVSCode(vscodeEnv,callbacks);
}

// this method is called when your extension is deactivated
function deactivate() {
  if (callbacks.onDeactivate) {
    callbacks.onDeactivate();  
  }	
}

// eslint-disable-next-line no-undef
module.exports = {
  activate,
  deactivate
}
