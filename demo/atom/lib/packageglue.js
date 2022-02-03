'use babel';

import { CompositeDisposable } from 'atom';
import { pas, rtl } from './pas2jsdemopackage.js';

export default {
  activate(state) {
    rtl.run();
    this.subscriptions = new CompositeDisposable();
    this.atomEnv = {
      atomGlobal : atom,
      subscriptions : this.subscriptions,
      initialState : state
    }
    this.atomHandler = {
      onDeactivate : function (a) {},
      onSerialize : function (a,o) {}
    }
    pas.program.InitAtom(this.atomEnv,this.atomHandler);
  },

  deactivate() {
    if (this.atomHandler.onDeactivate) {
      this.atomHandler.onDeactivate(this.atomEnv)
    }
    this.subscriptions.dispose();
  },

  serialize() {
    var obj = {};
    if (this.atomHandler.onSerialize) {
       this.atomHandler.onSerialize(this.atomEnv,obj)
    }
    return obj;
  }
};
