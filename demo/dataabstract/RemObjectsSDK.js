//RemObjects SDK classes
//interface


var RemObjects = {};



RemObjects.SDK = {
    RTTI : {
    },

    Enum : {},

    ROComplexType : function ROComplexType() {
    },

    ROEnumType : function ROEnumType() {
    },

    ROStructType : function ROStructType() {
    },

    ROArrayType : function ROArrayType() {
        this.elementType = "";
        this.items = [];
    },

    ROException : function ROException(e) {
        if (e) {
            this.name = e.name;
            this.message = e.message;
        };
        this.fields = new RemObjects.SDK.ROStructType();
    },

    ROEventSink : function ROEventSink() {
    },
    
    ClientChannel : function ClientChannel(aUrl) {
        this.url = aUrl;
        //post
    },

    HTTPClientChannel : function HTTPClientChannel(aUrl) {
        RemObjects.SDK.ClientChannel.call(this, aUrl);
    },


    Message : function Message() {
        this.fClientID = RemObjects.UTIL.NewGuid();
        this.fRequestObject = {};
        this.fResponseObject = {};
        //clone
        //getClientID
        //setClientID
    },

    JSONMessage : function JSONMessage() {
        RemObjects.SDK.Message.call(this);
        //initialize
        //finalize
        //write
        //read
        //requestStream
        //setResponseStream

    },

    BinMessage : function BinMessage() {
        RemObjects.SDK.Message.call(this);
        //initialize
        //finalize
        //write
        //read
        //requestStream
        //setResponseStream
    },

    BinHeader : function BinHeader() {


        this.fHeader = [0x52, 0x4f, 0x31, 0x30, 0x37];  //should contain 0x1c bytes
        for (var i = 5; i<0x1c; this.fHeader[i++] = 0);
        //readFrom
        //asStream
        //isValidHeader
        //getCompressed
        //setCompressed
        //getMessageType
        //setMessageType
        //setClientID

    // Header BINARY LAYOUT: 0x1C bytes
    //
    // Keep in sync with
    //  - Delphi - uROBINMessage.pas
    //  - C#     - BinMessage.cs
    //
    // 52 4f 31 30  = "RO10" basic RO signature for RO 1.0
    // XX YY ZZ --  = XX: subversion (currenly "7")
    //		 YY: option flags: 01 = compressed
    //		 ZZ: message type as defined in uROClientIntf
    //     --: reserved for future use
    // -- -- UU UU  = UU: user data (word)
    // CC CC CC CC    0x10 bytes ClientID (guid)
    // CC CC CC CC
    // CC CC CC CC
    // CC CC CC CC


    },


    RemoteService : function RemoteService(aChannel, aMessage, aServiceName) {
        this.fChannel = aChannel;
        this.fMessage = aMessage;
        this.fServiceName = aServiceName;
    },


    ROService : function ROService(aChannel, aMessage, aServiceName) {
        if (RemObjects.UTIL.checkArgumentTypes(arguments, [RemObjects.SDK.ClientChannel, RemObjects.SDK.Message, "string"]) ||
            RemObjects.UTIL.checkArgumentTypes(arguments, [RemObjects.SDK.ClientChannel, RemObjects.SDK.Message, "undefined"])) {
            this.fChannel = aChannel;
            this.fMessage = aMessage;
            this.fServiceName = aServiceName;
        } else if (RemObjects.UTIL.checkArgumentTypes(arguments, [RemObjects.SDK.RemoteService, "undefined", "undefined"])) {
                this.fChannel = aChannel.fChannel;
                this.fMessage = aChannel.fMessage;
                this.fServiceName = aChannel.fServiceName;
        } else if (RemObjects.UTIL.checkArgumentTypes(arguments, ["string"]))  { //URL
            var m = /https?:\/\/([-\w\.]+)+(:\d+)?\/([\w/_\.]*)/i.exec(aChannel);
            var path;
            if (m && m.length == 4 ) {
                path = m[3];
            } else {
                m = /https?:\/\/\[((?=.*::)(?!.*::.+::)(::)?([\dA-F]{1,4}:(:|\b)|){5}|([\dA-F]{1,4}:){6})((([\dA-F]{1,4}((?!\3)::|:\b|$))|(?!\2\3)){2}|(((2[0-4]|1\d|[1-9])?\d|25[0-5])\.?\b){4})\](:\d+)\/([\w/_\.]*)$/i.exec(aChannel);
                if (!m) {
                    throw new Error("ROService constructor: incorrect URL");
                } else {
                    path = m[14];
                };
            };
            if (path.toLowerCase() == "json") {
                this.fMessage = new RemObjects.SDK.JSONMessage();
            } else {
                this.fMessage = new RemObjects.SDK.BinMessage();
            };
            this.fChannel = new RemObjects.SDK.HTTPClientChannel(aChannel);
            if (typeof(aMessage) == "string") this.fServiceName = aMessage;
        } else if (!RemObjects.UTIL.checkArgumentTypes(arguments, ["undefined", "undefined", "undefined"])) {
            throw new Error("ROService constructor: Incorrect arguments");
        };
        //getChannel
        //getMessage
        //getServiceName
    },

    EventReceiver : function EventReceiver(aChannel, aMessage, aServiceName, aTimeout) {
        this.fChannel = aChannel;
        this.fMessage = aMessage;
        this.fServiceName = aServiceName;
        this.fTimeout = aTimeout;
        this.fActive = false;
        this.fHandlers = {};
        this.fInterval = null;
        //addHandler
        //intPollServer
        //setActive
        //getActive
        //getTimeout
        //setTimeout
    }

};

RemObjects.UTIL = {

    testBrowser : function testBrowser() {
        var result = "";
        if (typeof(JSON) == 'undefined')
            result += "Browser doesn't support JSON\n";

        var AJAX;
        if (typeof(XMLHttpRequest) == 'undefined') {
            try {
                AJAX = new XMLHttpRequest();
            } catch (e) {
                try {
                    AJAX = new ActiveXObject("Msxml2.XMLHTTP");
                } catch (e) {
                    try {
                        AJAX = new ActiveXObject("Microsoft.XMLHTTP");
                    } catch (e) {
                        result += "Browser doesn't support XMLHttpRequest object\n";
                    };
                };
            };
        };

        result += RemObjects.UTIL.testBrowserBinary();
        return result;
    },

    testBrowserBinary : function testBrowserBinary() {
        var result = "";
        if (!(((typeof(XMLHttpRequest) != 'undefined') && (typeof(XMLHttpRequest.prototype.sendAsBinary) != 'undefined')) || (typeof(Uint8Array) != 'undefined') ))
            result += "Browser doesn't support sending binary data\n";
        return result;
    },

    browserHasBinarySupport : function browserHasBinarySupport() {
        return RemObjects.UTIL.testBrowserBinary() == "";
    },

    showMessage : function(msg) {
        //for non-browser environments:
        //replace alert() call with something appropriate
        alert(msg);
    },

    showError : function showError(msg, e) {
        var result = "";
        if (e) {
            result += e.name + ": " + e.message;
        } else {
            result += msg.getErrorMessage() + "\n";
        };
        result += "\nCall stack:\n";
        var fn = showError;
      //  if (!(fn.caller)) //possibly IE
//            fn = arguments.callee;
//        while((fn = fn.caller) !== null) {
//            var fnName = fn.toString().match(/^function\s*(\w+)\(/);
//            fnName = (fnName) ? fnName[1] : 'anonymous function';
//            result += fnName;
//            result += "\n";
  //      }
        RemObjects.UTIL.showMessage(result);
    },

    toJSON : function toJSON(aValue) {
        if(typeof(JSON) != 'undefined') {
            var jsonString = JSON.stringify(aValue);
            jsonString = jsonString.replace(/[\u007F-\uFFFF]/g, function(chr) {
                return "\\u" + ("0000" + chr.charCodeAt(0).toString(16)).substr(-4)
            });
            return jsonString;
        } else {
            throw new Error("Your browser doesn't support JSON.stringify");
        };
    },

    parseJSON : function parseJSON(aValue) {
        if(typeof(JSON) != 'undefined') {
            return JSON.parse(aValue);
        } else {
            throw new Error("Your browser doesn't support JSON.parse");
        };
    },

    NewGuid : function NewGuid() {
        return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g,
        function(c) {
        var r = Math.random()*16|0, v = c == 'x' ? r : (r&0x3|0x8);
        return v.toString(16); })
    },

    GuidToArray : function GuidToArray(aGuid) {
        var result = [];
        aGuid = aGuid.replace(/-/g, "");
        for (var i = 3; i >= 0; result.push(parseInt(aGuid.substr(i-- * 2, 2), 16)));
        for (var i = 5; i >= 4; result.push(parseInt(aGuid.substr(i-- * 2, 2), 16)));
        for (var i = 7; i >= 6; result.push(parseInt(aGuid.substr(i-- * 2, 2), 16)));
        for (var i = 8; i < 16; result.push(parseInt(aGuid.substr(i++ * 2, 2), 16)));
//        for (var i = 0; i < 16; result.push(parseInt(aGuid.substr(i++ * 2, 2), 16)));
        return result;
    },

    guidToByteArray : function guidToByteArray(aGuid) {
        function readPart(str, start, end) {
            var result = "";
            for (var i = start; i <= end; result += String.fromCharCode(parseInt(aGuid.substr(i++ * 2 + 1, 2), 16)));
            return result;
        };

        function readPartReversed(str, start, end) {
            var result = "";
            for (var i = end; i >= start; result += String.fromCharCode(parseInt(aGuid.substr(i-- * 2 + 1, 2), 16)));
            return result;
        };
        aGuid = aGuid.replace(/-/g, "");
        return readPartReversed(aGuid, 0, 3) + readPartReversed(aGuid, 4, 5)
               + readPartReversed(aGuid, 6, 7) + readPart(aGuid, 8, 9) + readPart(aGuid, 10, 15);
    },

    zeroPad : function zeroPad(num, count) {
                var numZeropad = num + '';
                while (numZeropad.length < count) {
                    numZeropad = "0" + numZeropad;
                }
                return numZeropad;
    },

    byteArrayToGuid : function byteArrayToGuid(byteArray) {
        function readPartReversed(str, start, end) {
            var result = "";
            for (var i = end; i >= start; i--) {
                result += RemObjects.UTIL.zeroPad((str.charCodeAt(i) & 0xFF).toString(16).toUpperCase(), 2);
            };
            return result;
        };
        function readPart(str, start, end) {
            var result = "";
            for (var i = start; i <= end; i++) {
                result += RemObjects.UTIL.zeroPad((str.charCodeAt(i) & 0xFF).toString(16).toUpperCase(), 2);
            };
            return result;
        };
        return "{" + readPartReversed(byteArray, 0, 3) + "-"
                   + readPartReversed(byteArray, 4, 5) + "-"
                   + readPartReversed(byteArray, 6, 7) + "-"
                   + readPart(byteArray, 8, 9) + "-"
                   + readPart(byteArray, 10, 15)
                + "}";
    },


    strToByteArray : function strToByteArray(str) {
        var byteArray = [];
        for (var i = 0; i < str.length; i++)
            if (str.charCodeAt(i) <= 0x7F)
                byteArray.push(str.substr(i, 1));
            else {
                var h = encodeURIComponent(str.charAt(i)).substr(1).split('%');
                for (var j = 0; j < h.length; j++)
                    byteArray.push(String.fromCharCode(parseInt(h[j], 16)));
            };
        return byteArray.join("");
    },



    byteArrayToStr : function byteArrayToStr(byteArray) {
        var str = '';
        for (var i = 0; i < byteArray.length; i++)
            str += byteArray.charCodeAt(i) <= 0x7F ?
                    byteArray.charCodeAt(i) === 0x25 ? "%25" : // %
                            byteArray.substr(i, 1) :
                    "%" + (byteArray.charCodeAt(i) & 0xFF).toString(16).toUpperCase();
        return decodeURIComponent(str);
    },

    byteArrayToUtf16 : function byteArrayToUtf16(byteArray) {
        var str = '';
        for (var i = 0; i < byteArray.length / 2; i++) 
            str += String.fromCharCode((byteArray.charCodeAt(i * 2) & 0xFF) + ((byteArray.charCodeAt(i * 2 + 1) & 0xFF) << 8));
        return str;
    },

    utf16ToByteArray : function utf16ToByteArray(str) {
        var byteArray = "";
        for (var i = 0; i < str.length; i++) {
            byteArray += String.fromCharCode(str.charCodeAt(i) & 0xFF);
            byteArray += String.fromCharCode((str.charCodeAt(i) & 0xFF00) >> 8);
        };
        return byteArray;
    },

    ISO8601toDateTime : function ISO8601toDateTime(str) {
        var regexp = "([0-9]{4})(-([0-9]{2})(-([0-9]{2})" +
            "(T([0-9]{2}):([0-9]{2})(:([0-9]{2})(\.([0-9]+))?)?" +
            "(Z|(([-+])([0-9]{2}):([0-9]{2})))?)?)?)?";
        var d = ("" + str).match(new RegExp(regexp));

        if (!d) return null;

        var offset = 0;
        var date = new Date(d[1], 0, 1);

        if (d[3]) { date.setMonth(d[3] - 1); }
        if (d[5]) { date.setDate(d[5]); }
        if (d[7]) { date.setHours(d[7]); }
        if (d[8]) { date.setMinutes(d[8]); }
        if (d[10]) { date.setSeconds(d[10]); }
        if (d[12]) { date.setMilliseconds(Number("0." + d[12]) * 1000); }
        if (d[14]) {
            offset = (Number(d[16]) * 60) + Number(d[17]);
            offset *= ((d[15] == '-') ? 1 : -1);
            offset -= date.getTimezoneOffset();
            var time = (Number(date) + (offset * 60 * 1000));
            date.setTime(Number(time));
        }

        return date;
    },

    dateTimeToSOAPString : function dateTimeToSOAPString(aValue) {
        //'yyyy-mm-ddThh:nn:ss'
        return aValue.getFullYear() + '-' + RemObjects.UTIL.zeroPad(aValue.getMonth() + 1, 2)
               + '-' + RemObjects.UTIL.zeroPad(aValue.getDate(), 2)
               + 'T' + RemObjects.UTIL.zeroPad(aValue.getHours(), 2)
               + ':' + RemObjects.UTIL.zeroPad(aValue.getMinutes(), 2)
               + ':' + RemObjects.UTIL.zeroPad(aValue.getSeconds(), 2);
    },

    decimalToString : function decimalToString(aDecimal) { //aDecimal - array [0..6]
        var sign = (aDecimal[6] & 0x80000000) != 0;
        var scale = (aDecimal[6] & 0xFF0000) >> 16;
        var pos = 31;
        var aDec = aDecimal.slice();
        var aResult = [];
        var modres;
        var d;
        while ((aDec[0] != 0) || (aDec[1] != 0) || (aDec[2] != 0)
                || (aDec[3] != 0) || (aDec[4] != 0) || (aDec[5] != 0)
                || ((31 - pos) < scale)) {
          modres = 0;
          for (var i = 5; i >= 0; i--) {
              d = (modres << 16) | aDec[i];
              modres = d % 10;
              aDec[i] = Math.floor(d / 10);
          };
          aResult[pos] = modres.toString(10);
          pos--;
          if ((31 - pos) == scale) {
            aResult[pos] = ".";
            pos--;
            if ((aDec[0] == 0) && (aDec[1] == 0) && (aDec[2] == 0)) {
              aResult[pos] = '0';
              pos--;
            };
          };
        };
        if (pos == 31)
            return "0";
        if (sign) {
          aResult[pos] = '-';
          pos--;
        };
        return aResult.join("");
    },

    stringToDecimal : function stringToDecimal(aString) {
        var mulres;
        var d;
        var aRes = [0, 0, 0, 0, 0, 0, 0];
        var pos = 0;
        var scalepos = -1;
        var c;
        var n;
        for (var i = 1; i <= aString.length; i++) {
            mulres = 0;
            c = aString.substr(i - 1, 1);
            if (n = parseFloat(c)) {
                mulres = n
            } else if (c == "-") {
                aRes[6] = 0x80000000;
                continue;
            } else if (c == ".") {
                if (scalepos == -1)
                    scalepos = pos;
                continue;
            } else
                continue;


            for (var j = 0; j < 6; j++) {
                d = aRes[j] * 10 + mulres;
                mulres = d >> 16;
                aRes[j] = d & 0xffff;
            };
            pos++;
        };
        if (scalepos != -1) {
            pos = pos - scalepos;
            aRes[6] = aRes[6] | (pos << 16);
        };
        return aRes;
    },

    toBase64 : function toBase64(aValue) {
        if (typeof(btoa) != 'undefined') {
            return btoa(aValue);
        } else {
            throw(new Error("Base64 encoding is not supported by your browser."));
            //return $.base64Encode(aValue);
        };
    },

    fromBase64 : function fromBase64 (aValue) {
        if (typeof(atob) != 'undefined') {
            return atob(aValue.replace(/(\n|\r)+/g, ""));
        } else {
            throw(new Error("Base64 decoding is not supported by your browser."));
            //      return $.base64Decode(aValue);
        };

    },

    checkArgumentTypes : function checkArgumentTypes (args, types) {
        for (var i = 0; i < types.length; i++) {
            if (typeof(types[i]) == "string") {
                if (typeof(args[i]) != types[i]) return false;
            } else {
                if (!(args[i] instanceof types[i])) return false;
            };
        };
        return true;
    }

};


RemObjects.SDK.Enum.MessageType = {
    mtMessage      : 0,
    mtException    : 1,
    mtEvent        : 2,
    mtPoll         : 0x80,
    mtPollResponse : 0x81
};



//RO.SDK implementation

RemObjects.SDK.ROEventSink.prototype = new RemObjects.SDK.ROComplexType();
RemObjects.SDK.ROEventSink.prototype.constructor = RemObjects.SDK.ROEventSink;
RemObjects.SDK.ROEventSink.prototype.readEvent = function readEvent(aMessage, aName) {
    for (var prop in this[aName]) {
        if ((typeof this[aName][prop]) != "function") {
            this[aName][prop].value = aMessage.read(prop, this[aName][prop].dataType);
        };
    };
};

RemObjects.SDK.ROException.prototype = new Error();

RemObjects.SDK.ROEnumType.prototype = new RemObjects.SDK.ROComplexType();
RemObjects.SDK.ROEnumType.prototype.constructor = RemObjects.SDK.ROEnumType;


RemObjects.SDK.ROEnumType.prototype.writeTo = function writeTo(aMessage) {
    aMessage.write("", "Integer", this.enumValues.indexOf(this.value));
};

RemObjects.SDK.ROEnumType.prototype.readFrom = function readFrom(aMessage) {
    this.value = this.enumValues[aMessage.read("", "Integer")];
};

RemObjects.SDK.ROEnumType.prototype.toObject = function toObject() {
    return this.value;
};

RemObjects.SDK.ROEnumType.prototype.fromObject = function fromObject(aValue) {
    this.value = aValue; //todo: add check
};


RemObjects.SDK.ROStructType.prototype = new RemObjects.SDK.ROComplexType();
RemObjects.SDK.ROStructType.prototype.constructor = RemObjects.SDK.ROStructType;

RemObjects.SDK.ROStructType.prototype.writeTo = function writeTo(aMessage) {
    for (var prop in this) {
        if ((typeof this[prop]) != "function") {
            aMessage.write(prop, this[prop].dataType, this[prop].value);
        };
    };
};

RemObjects.SDK.ROStructType.prototype.readFrom = function readFrom(aMessage) {
    for (var prop in this) {
        if ((typeof this[prop]) != "function") {
            this[prop].value = aMessage.read(prop, this[prop].dataType);
        };
    };
};


RemObjects.SDK.ROStructType.prototype.toObject = function toObject(aStoreType) {
    var result = {};
    for (var prop in this) {
        if ((typeof this[prop]) != "function") {
            if (this[prop].value instanceof RemObjects.SDK.ROComplexType) {
                result[prop] = this[prop].value.toObject(aStoreType);
            } else
                result[prop] = this[prop].value;
        };
    };
    if(aStoreType) result.__type =  /function\s*(.*?)\(/.exec(this.constructor.toString())[1];
    return result;
};

RemObjects.SDK.ROStructType.prototype.fromObject = function fromObject(aValue) {
    for (var prop in this) {
        if ((typeof this[prop]) != "function") { //!!!
            if (RemObjects.SDK.RTTI[this[prop].dataType] && RemObjects.SDK.RTTI[this[prop].dataType].prototype instanceof RemObjects.SDK.ROComplexType) {
                this[prop].value = new RemObjects.SDK.RTTI[this[prop].dataType]();
                this[prop].value.fromObject(aValue[prop]);
            } else {
                if (this[prop].dataType == "DateTime") {
                    this[prop].value = RemObjects.UTIL.ISO8601toDateTime(aValue[prop]);
                } else {
                    this[prop].value = aValue[prop];
                };
            };

        };
    };
    return this;
};


RemObjects.SDK.ROArrayType.prototype = new RemObjects.SDK.ROComplexType();
RemObjects.SDK.ROArrayType.prototype.constructor = RemObjects.SDK.ROArrayType;


RemObjects.SDK.ROArrayType.prototype.writeTo = function writeTo(aMessage) {
    for (var i=0; i<this.items.length; i++ ) {
        var constructorName = /function\s*(.*?)\(/.exec(this.items[i].constructor.toString())[1];
        aMessage.write("", RemObjects.SDK.RTTI[constructorName] ? constructorName : this.elementType, this.items[i]);
    };
};

RemObjects.SDK.ROArrayType.prototype.readFrom = function readFrom(aMessage) {
    for (var i=0; i<this.items.length; i++ ) {
        this.items[i] = aMessage.read("", this.elementType);
    };
};


RemObjects.SDK.ROArrayType.prototype.toObject = function toObject(aStoreType) {
    var result = [];
    for (var i = 0; i < this.items.length; i++)
        if (this.items[i] instanceof RemObjects.SDK.ROComplexType) {
            var tmp = this.items[i].toObject(aStoreType);
            if(aStoreType) tmp.__type =  /function\s*(.*?)\(/.exec(this.items[i].constructor.toString())[1];
            result.push(tmp);
        } else
            result.push(this.items[i]);
    return result;
};

RemObjects.SDK.ROArrayType.prototype.fromObject = function fromObject(aValue) {
    if (!aValue) return this;
    var itemType = RemObjects.SDK.RTTI[this.elementType];
    if(itemType) {
        for (var i = 0; i < aValue.length; i++) {
            var item = new itemType();
            item.fromObject(aValue[i]);
            this.items.push(item);
        };
    } else {
        if (this.elementType == "DateTime") {
            for (var i = 0; i < aValue.length; i++) {
                this.items.push(RemObjects.UTIL.ISO8601toDateTime(aValue[i]));
            };
        } else {
            this.items = aValue;
        };
    };
    return this;
};


RemObjects.SDK.ROService.prototype.getChannel = function getChannel() {
    return this.fChannel;
};

RemObjects.SDK.ROService.prototype.getMessage = function getMessage() {
    return this.fMessage;
};

RemObjects.SDK.ROService.prototype.getServiceName = function getServiceName() {
    return this.fServiceName;
};

RemObjects.SDK.ClientChannel.prototype.dispatch = function dispatch(aMessage, onSuccessFunction, onErrorFunction) {
    function handleException(e) {
        if (((e.name == "EROSessionNotFound") || (e.name == "SessionNotFoundException")) && !(that.retrying)) {
            if (that.onLoginNeeded) {
                that.onLoginNeeded(function() {
                    that.retrying = true;
                    that.dispatch(aMessage, function(__msg) {
                        that.retrying = false;
                        onSuccessFunction(__msg)
                    },
                            function(__msg, __e) {
                                that.retrying = false;
                                onErrorFunction(__msg, __e);
                            });
                });
            };
        } else {
//                    if (window[e.name] && window[e.name].prototype instanceof RemObjects.SDK.ROException) {
            if (RemObjects.SDK.RTTI[e.name] && RemObjects.SDK.RTTI[e.name].prototype instanceof RemObjects.SDK.ROException) {
                e = new RemObjects.SDK.RTTI[e.name](e);
                e.fields.readFrom(aMessage);
            };
            if (onErrorFunction)
                onErrorFunction(aMessage, e);
        };
    };
    var that = this;
    this.post(aMessage.requestStream(), aMessage instanceof RemObjects.SDK.BinMessage, function ajax_post_success(__response) {
        try {
            aMessage.setResponseStream(__response);
            if (onSuccessFunction)
                onSuccessFunction(aMessage);
        } catch (e) {
            handleException(e);
        };
    }, function ajax_post_error(__response, __status) {
            aMessage.setErrorResponse("AJAX status: " + __status + "\nResponse: " +__response);
            try {
                if (__response)
                    aMessage.setResponseStream(__response);
                if (that.onAjaxError) that.onAjaxError(__response, __status);
                if (onErrorFunction) onErrorFunction(aMessage);
            } catch (e) {
                handleException(e);
            };
    });
};

RemObjects.SDK.ClientChannel.prototype.onLoginNeeded = function onLoginNeeded(aCallback) {
    RemObjects.UTIL.showMessage("Default onLoginNeeded handler: assign channel.onLoginNeeded and call aCallback there after successful login");
    aCallback();
};

RemObjects.SDK.HTTPClientChannel.prototype = new RemObjects.SDK.ClientChannel("");
RemObjects.SDK.HTTPClientChannel.prototype.constructor = RemObjects.SDK.HTTPClientChannel;


RemObjects.SDK.HTTPClientChannel.prototype.post = function post(aMessage, isBinary, onSuccess, onError) {
  var ajaxObject;
  if ((typeof(Ti) != 'undefined') && (typeof(Ti.Network) != 'undefined')) {
      ajaxObject = new TitaniumAjaxWrapper(this.url);
  } else {
      ajaxObject = new AjaxWrapper(this.url);
  };
  ajaxObject.post(aMessage, isBinary, onSuccess, onError);
};


RemObjects.SDK.Message.prototype.clone = function clone() {
    var cloned = new this.constructor();
    cloned.fClientID = this.fClientID;
    return cloned;
};

RemObjects.SDK.Message.prototype.getClientID = function getClientID() {
    return this.fClientID;
};

RemObjects.SDK.Message.prototype.setClientID = function setClientID(aValue) {
    this.fClientID = aValue;
};

RemObjects.SDK.Message.prototype.setErrorResponse = function setErrorResponse(aResponse) {
    this.fResponseObject.error = {message: aResponse};
};

RemObjects.SDK.Message.prototype.getErrorMessage = function getErrorMessage() {
    if (this.fResponseObject.error)
        return this.fResponseObject.error.message;
    else
        return "";
};


RemObjects.SDK.BinHeader.prototype.asStream = function asStream() {
    var result = "";
    var parser = new BinaryParser();
    for (var i = 0; i < 0x1c; i++)
        result += parser.encodeInt(this.fHeader[i], 8, false);
    return result;
};

RemObjects.SDK.BinHeader.prototype.readFrom = function readFrom(aStream) {
    var parser = new BinaryParser();
    for (var i = 0; i < 0x1c; i++)
        this.fHeader[i] = parser.decodeInt(aStream.substr(i, 1), 8, false);
};

RemObjects.SDK.BinHeader.prototype.isValidHeader = function isValidHeader() {
    var tmp = "";
    for (var i = 0; i < 5; tmp+=String.fromCharCode(this.fHeader[i++]));
    return (tmp == "RO107");
};

RemObjects.SDK.BinHeader.prototype.getCompressed = function getCompressed() {
    return this.fHeader[5];
};

RemObjects.SDK.BinHeader.prototype.setCompressed = function setCompressed(aValue) {
    this.fHeader[5] = aValue ? 1 : 0;
};

RemObjects.SDK.BinHeader.prototype.getMessageType = function getMessageType() {
    return this.fHeader[6];
};

RemObjects.SDK.BinHeader.prototype.setMessageType = function setMessageType(aValue) {
    this.fHeader[6] = aValue;
};

RemObjects.SDK.BinHeader.prototype.setClientID = function setClientID(aValue) {
    var guid = RemObjects.UTIL.GuidToArray(aValue);
    this.fHeader.length -= 16;
    this.fHeader = this.fHeader.concat(guid);
};


RemObjects.SDK.BinMessage.prototype = new RemObjects.SDK.Message();
RemObjects.SDK.BinMessage.prototype.constructor = RemObjects.SDK.BinMessage;


RemObjects.SDK.BinMessage.prototype.initialize = function initialize(aServiceName, aMethodName, aMessageType) {
    var header = new RemObjects.SDK.BinHeader();
    header.setCompressed(false);
    header.setMessageType(aMessageType || RemObjects.SDK.Enum.MessageType.mtMessage);
    header.setClientID(this.fClientID);
    this.fRequestObject = header.asStream();
    this.parser = new BinaryParser();
    if (aMessageType != RemObjects.SDK.Enum.MessageType.mtPoll) {
        this.fRequestObject += this.parser.encodeInt(aServiceName.length, 32, false) + aServiceName;
        this.fRequestObject += this.parser.encodeInt(aMethodName.length, 32, false) + aMethodName;
    };
};

RemObjects.SDK.BinMessage.prototype.finalize = function finalize() {

};

RemObjects.SDK.BinMessage.prototype.write = function write(aName, aType, aValue) {

    if (RemObjects.SDK.RTTI[aType]) {
        if (aValue instanceof RemObjects.SDK.ROComplexType) { //not null
            if (!(aValue instanceof RemObjects.SDK.ROEnumType)) {
                this.fRequestObject += this.parser.encodeInt(1, 8, false);
                if (aValue instanceof RemObjects.SDK.ROStructType)
                    this.writeStrWithLength(aType);
                if (aValue instanceof RemObjects.SDK.ROArrayType)
                    this.fRequestObject += this.parser.encodeInt(aValue.items.length, 32, false);
            };
            aValue.writeTo(this);
        } else { //null
            if (!(RemObjects.SDK.RTTI[aType].prototype instanceof RemObjects.SDK.ROEnumType)) {
                this.fRequestObject += this.parser.encodeInt(0, 8, false);
                if (RemObjects.SDK.RTTI[aType].prototype instanceof RemObjects.SDK.ROStructType)
                    this.writeStrWithLength(aType);
            };
        };
    } else
    switch (aType) {

        case "Decimal":
            var decimal = RemObjects.UTIL.stringToDecimal(aValue.toString());
            for (var i = 0; i < 6; i++)
                this.fRequestObject += this.parser.encodeInt(decimal[i], 16, false);
            this.fRequestObject += this.parser.encodeInt(decimal[6], 32, false);
            break;

        case "Double":
            this.fRequestObject += this.parser.encodeFloat(aValue, 52, 11);
            break;

        case "Boolean":
            this.fRequestObject += this.parser.encodeInt((aValue ? 1 : 0), 32, false);
            break;

        case "Binary":
            this.fRequestObject += this.parser.encodeInt(1, 8, false);
            this.writeStrWithLength(aValue);
            break;
        
        case "Integer":
            this.fRequestObject += this.parser.encodeInt(aValue, 32, true);
            break;

        case "Int64":
            this.fRequestObject += this.parser.encodeInt(aValue, 64, true);
            break;

        case "Currency":
            var cur = this.parser.encodeInt(aValue * 10000, 48, true);
            this.fRequestObject += cur;
            if ((cur.charCodeAt(cur.length - 1) == 0) || (cur.charCodeAt(cur.length - 1) == 0xFF)) {
                this.fRequestObject += cur.substr(cur.length - 1, 1) + cur.substr(cur.length - 1, 1);
            };
            break;

        case "Guid":
            this.fRequestObject += RemObjects.UTIL.guidToByteArray(aValue);
            break;

        case "DateTime":
            this.fRequestObject += this.parser.encodeFloat((aValue - aValue.getTimezoneOffset() * 60000) / 86400000 + 25569.0, 52, 11);
            break;

        case "WideString":
            this.fRequestObject += this.parser.encodeInt(aValue.length, 32, true);
            this.fRequestObject += RemObjects.UTIL.utf16ToByteArray(aValue);
            break;
        case "Xml":
        case "Utf8String":
            aValue = RemObjects.UTIL.strToByteArray(aValue);
        case "AnsiString":
            this.writeStrWithLength(aValue);
            break;
        case "Variant":
            this.writeVariant(aValue);
            break;

        default:
        throw new Error("BinMessage.write: Unknown type of " + aName + " - " + aType);
    };

};

RemObjects.SDK.BinMessage.prototype.writeVariant = function writeVariant(aValue) {
    var tmpValue;
    var tmpInt;
    var tmpFloat;
    if ((aValue == undefined) || (aValue == null)) {
        this.writeInteger(0x0001); //varNull
    } else if (aValue instanceof Date) {
        this.writeInteger(0x0007); //varDateTime
        this.write("", "DateTime", aValue);
    } else if (typeof(aValue) == "boolean") {
        this.writeInteger(0x000B); //varBoolean
        this.write("", "Boolean", aValue);
    } else if(!isNaN(aValue) && typeof (aValue) != "string") {
        if((tmpInt = parseInt(aValue)) == (tmpFloat = parseFloat(aValue))) {
            this.writeInteger(0x0003); //varInt32
            this.write("", "Integer", tmpInt);
        } else {
            this.writeInteger(0x0005); //varDouble
            this.write("", "Double", tmpFloat);
        };
    } else if (typeof(aValue) == "string") {
        this.writeInteger(0x0008); //varString
        this.write("", "Utf8String", aValue);
    } else if (aValue.length) {
        this.writeInteger(0x200C); //varVariant
        this.writeInteger(0); //lowBound
        this.writeInteger(aValue.length - 1); //highBound
        for (var i = 0; i < aValue.length; i++)
            this.writeVariant(aValue[i]);
    } else throw new Error("writeVariant: unknown type")

};

RemObjects.SDK.BinMessage.prototype.writeInteger = function writeVariant(aValue) {
    this.fRequestObject += this.parser.encodeInt(aValue, 32, true);
};

RemObjects.SDK.BinMessage.prototype.writeStrWithLength = function writeStrWithLength(aValue) {
    this.fRequestObject += this.parser.encodeInt(aValue.length, 32, false) + aValue;
};

RemObjects.SDK.BinMessage.prototype.readByte = function readByte() {

    var result = -1;
    if (this.fStreamPos < this.fResponseString.length) {
        result = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 1), 8, false) & 0xFF;
        this.fStreamPos += 1;
    };
    return result;
};

RemObjects.SDK.BinMessage.prototype.readCompressed = function readCompressed() {
    var result = "";
    var b, inflator = new RemObjects.ZLIB.Inflator(this);
    this.fStreamPos += 2;
    while ((b = inflator.readByte()) >= 0) {
        result += String.fromCharCode(b);
    };
    return result;
};

RemObjects.SDK.BinMessage.prototype.read = function read(valueName, valueType) {
    var value;

    var rttiType = RemObjects.SDK.RTTI[valueType];
    if (rttiType && rttiType.prototype instanceof RemObjects.SDK.ROComplexType) {
        if (rttiType.prototype instanceof RemObjects.SDK.ROEnumType) {
            value = new rttiType();
            value.readFrom(this);
            return value;
        };

        if (!(this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos++, 1), 8, false))) { // Not assigned
            return null;
        };

        if (rttiType.prototype instanceof RemObjects.SDK.ROStructType) {
            var realType = this.read("", "AnsiString");
            if (valueType !== realType) {
                value = new RemObjects.SDK.RTTI[realType]();
            } else {
                value = new rttiType();
            };
            value.readFrom(this);
            return value;
        }

        value = new rttiType();	
        if (value instanceof RemObjects.SDK.ROArrayType) {
            value.items.length = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4;
        };
        value.readFrom(this);

        return value;
    }

    switch (valueType) {
        case "Decimal":
            var decimal = [];
            for (var i = 0; i < 6; i++) {
                decimal[i] = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 2), 16, false);
                this.fStreamPos += 2;
            };
            decimal[6] = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4;
            value = parseFloat(RemObjects.UTIL.decimalToString(decimal));
            break;

        case "Double":
            value = this.parser.decodeFloat(this.fResponseString.substr(this.fStreamPos, 8), 52, 11);
            this.fStreamPos += 8;
            this.fResponseObject[valueName] = value;
            break;

        case "DateTime":
            var utcValue = this.parser.decodeFloat(this.fResponseString.substr(this.fStreamPos, 8), 52, 11);
            utcValue = new Date(Math.round((utcValue - 25569.0) * 86400000));
            value = new Date(utcValue.getUTCFullYear(), utcValue.getUTCMonth(), utcValue.getUTCDate(),  utcValue.getUTCHours(), utcValue.getUTCMinutes(), utcValue.getUTCSeconds());
            this.fStreamPos += 8;
            this.fResponseObject[valueName] = value;
            break;

        case "Boolean":
            value = !(this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false) == 0);
            this.fStreamPos += 4;
            this.fResponseObject[valueName] = value;
            break;

        case "Integer":
            value = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, true);
            this.fStreamPos += 4;
            this.fResponseObject[valueName] = value;
            break;

        case "Int64":
            value = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 6), 48, true);
            this.fStreamPos += 8;
            this.fResponseObject[valueName] = value;
            break;

        case "Currency":
            value = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 6), 48, true) / 10000;
            this.fStreamPos += 8;
            this.fResponseObject[valueName] = value;
            break;

        case "Xml":
        case "Utf8String":
            var len = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4;
            value = RemObjects.UTIL.byteArrayToStr(this.fResponseString.substr(this.fStreamPos, len));
            this.fStreamPos += len;
            break;

        case "WideString":
            var len = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4;
            value = RemObjects.UTIL.byteArrayToUtf16(this.fResponseString.substr(this.fStreamPos, len * 2));
            this.fStreamPos += len * 2;
            break;

        case "Binary":
            var isAssigned = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 1), 8, false);
            this.fStreamPos += 1;
            if (isAssigned == 0) {
                value = null;
                break;
            };
            var len = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4;
            value = "";
            for (var i = this.fStreamPos; i < this.fStreamPos + len; i++) {
                value += String.fromCharCode(this.fResponseString.charCodeAt(i) & 0xFF);
            };
            this.fStreamPos += len;
            break;

        case "AnsiString":
            var len = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4;
            value = this.fResponseString.substr(this.fStreamPos, len);
            this.fStreamPos += len;
            break;

        case "Guid":
            value = RemObjects.UTIL.byteArrayToGuid(this.fResponseString.substr(this.fStreamPos, 16));
            this.fStreamPos += 16;
            break;

        case "Variant":
            value = this.readVariant();
            break;

        default:
            if (RemObjects.SDK.RTTI[valueType] && (typeof(RemObjects.SDK.RTTI[valueType]) == "function") && (RemObjects.SDK.RTTI[valueType].prototype instanceof RemObjects.SDK.ROComplexType)) {
                value = new RemObjects.SDK.RTTI[valueType]();
                value.readFrom(this);
            } else {
                this.fResponseObject.error = {message : "BinMessage.read: Unknown type of " + valueName + " - " + valueType};
                throw new Error(this.fResponseObject.error.message);
            };
    };
    return value;
};

RemObjects.SDK.BinMessage.prototype.readVariant = function readVariant() {
    var code = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
    this.fStreamPos += 4;
    var result;

    if ((code & 0x2000) == 0x2000) {
        if (code == 0x2011) { //varBinary
            var binLength = this.read("", "Integer");
            result = this.fResponseString.substr(this.fStreamPos, binLength);
            this.fStreamPos += binLength;
        } else {//varArray
            //var itemCode = code & 0xFFF;
            result = [];
            var lowBound = this.read("", "Integer");
            var highBound = this.read("", "Integer");
            for (var i = lowBound; i <= highBound; i++)
                result[i] = this.readVariant();
        };
        return result;
    };

    switch(code) {
        case 0x000A: //varError
        case 0x0000: return undefined; //varEmpty
        case 0x0001: return null; //varNull
        case 0x0002: //varInt16
            result = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 2), 16, true);
            this.fStreamPos += 2;
            return result;
        case 0x0003: //varInt32
            result = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, true);
            this.fStreamPos += 4;
            return result;
        case 0x0004: //varSingle
            result = this.parser.decodeFloat(this.fResponseString.substr(this.fStreamPos, 4), 23, 8);
            this.fStreamPos += 4;
            return result;
        case 0x0005: return this.read("", "Double");//varDouble
        case 0x0006: return this.read("", "Currency");//varCurrency
        case 0x0007: return this.read("", "DateTime");//varDateTime
//varDispatch = 0x0009
        case 0x000B: return this.read("", "Boolean");//varBoolean
//varVariant = 0x000C
//varUnknown = 0x000D
        case 0x000E: return this.read("", "Decimal");//varDecimal
        case 0x0010: //varInt8
            result = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 1), 8, true);
            this.fStreamPos += 1;
            return result;
        case 0x0011: //varByte
            result = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 1), 8, false);
            this.fStreamPos += 1;
            return result;
        case 0x0012: //varWord
            result = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 2), 16, false);
            this.fStreamPos += 2;
            return result;
        case 0x0013: //varLongWord
            result = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4;
            return result;
        case 0x0014: //varInt64
            result = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 8), 48, true);
            this.fStreamPos += 8;
            return result;
        case 0x0072: return this.read("", "Guid");//varGuid
        case 0x0100: return this.read("", "AnsiString");//varDelphiString
        case 0x0008: //varOleStr
        case 0x0102: return this.read("", "Utf8String");//varDelphiUtfString
        default : throw new Error("readVariant: unknown varCode 0x" + code.toString(16));
    };
};


RemObjects.SDK.BinMessage.prototype.requestStream = function requestStream() {
    return this.fRequestObject;
};

RemObjects.SDK.BinMessage.prototype.setResponseStream = function setResponseStream(aResponse) {
    this.fResponseString = aResponse;
    var header = new RemObjects.SDK.BinHeader();
    header.readFrom(this.fResponseString.substr(0, 28));
    this.fStreamPos = 28; //skip header

    if (!header.isValidHeader()) {
        this.fResponseObject.error = {message : "Invalid response: unsupported binary message signature"};
        throw new Error(this.fResponseObject.error.message);
    };
    if (header.getCompressed()) {

        this.fResponseString = this.readCompressed();
        this.fStreamPos = 0;

        //this.fResponseObject.error = {message : "Invalid response: compression is not supported for binary message"};
        //throw new Error(this.fResponseObject.error.message);
    };

    switch (header.getMessageType()) {

        case RemObjects.SDK.Enum.MessageType.mtMessage :
            var value = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4 + value; //skip service name
            value = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4 + value; //skip method name
            break;

        case RemObjects.SDK.Enum.MessageType.mtException :
            var value = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
            var exceptionClass = this.fResponseString.substr(this.fStreamPos + 4, value);
            this.fStreamPos +=  4 + value;
            value = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
            var exceptionMessage = this.fResponseString.substr(this.fStreamPos + 4, value);
            this.fStreamPos += 4 + value; //skip method name
            this.fResponseObject.error = {message : exceptionClass + ":\n" + exceptionMessage};
            var __e = new Error(exceptionMessage);
            __e.name = exceptionClass;
            throw __e;
            break;
        case RemObjects.SDK.Enum.MessageType.mtPollResponse :
        case RemObjects.SDK.Enum.MessageType.mtEvent :
            break;

        default:
            throw new Error("Unsupported binary message type - 0x" + header.getMessageType().toString(16));
    };


};


RemObjects.SDK.JSONMessage.prototype = new RemObjects.SDK.Message();
RemObjects.SDK.JSONMessage.prototype.constructor = RemObjects.SDK.JSONMessage;

RemObjects.SDK.JSONMessage.prototype.initialize = function initialize(aServiceName, aMethodName, aMessageType) {
    this.fRequestObject.id = "{" + this.fClientID + "}";
    this.fRequestObject.method = aServiceName + "." + aMethodName;
    this.fRequestObject.params = {};
    if (aMessageType) this.fRequestObject.type = aMessageType;
};

RemObjects.SDK.JSONMessage.prototype.finalize = function finalize() {

};

RemObjects.SDK.JSONMessage.prototype.write = function write(aName, aType, aValue) {
    if (aValue instanceof RemObjects.SDK.ROComplexType) {
        this.fRequestObject.params[aName] = aValue.toObject(true);
    } else if (aValue instanceof Date) {
        this.fRequestObject.params[aName] = RemObjects.UTIL.dateTimeToSOAPString(aValue);
    } else if (aType == "Binary") {
        this.fRequestObject.params[aName] = RemObjects.UTIL.toBase64(aValue);
    } else {
        this.fRequestObject.params[aName] = aValue;
    };
};


RemObjects.SDK.JSONMessage.prototype.read = function read(aName, aType) {
    if (this.fResponseObject.result == undefined) {
        throw new Error("JSONMessage.read: result property is missing:\n" + RemObjects.UTIL.toJSON(this.fResponseObject));
    };

    if (this.fResponseObject.result[aName] == undefined) {
        throw new Error("JSONMessage.read error:\n" + aName + ":" + aType + " is missing.");
    };

    var result;
    aType = this.fResponseObject.result[aName].__type || aType;
    if (RemObjects.SDK.RTTI[aType] && RemObjects.SDK.RTTI[aType].prototype instanceof RemObjects.SDK.ROComplexType) {
        result = new RemObjects.SDK.RTTI[aType]();
        if (RemObjects.SDK.RTTI[aType].prototype instanceof RemObjects.SDK.ROEnumType) {
            result.value = this.fResponseObject.result[aName];
        } else {
            result.fromObject(this.fResponseObject.result[aName]);
        };
    } else if ((aType == "DateTime") || ((aType == "Variant") && (RemObjects.UTIL.ISO8601toDateTime(this.fResponseObject.result[aName])))) {
        result = RemObjects.UTIL.ISO8601toDateTime(this.fResponseObject.result[aName]);
    } else {
        result = this.fResponseObject.result[aName];
    };
    return result;
};


RemObjects.SDK.JSONMessage.prototype.requestStream = function requestStream() {
    return RemObjects.UTIL.toJSON(this.fRequestObject);
};

RemObjects.SDK.JSONMessage.prototype.setResponseStream = function setResponseStream(aResponse) {
    try {
        this.fResponseObject = RemObjects.UTIL.parseJSON(aResponse);
    } catch (e) {
        throw new Error("JSONMessage.setResponseStream:\n JSON parsing error: " + e.message + "\nServer response:\n" + aResponse);
    };
    if (this.fResponseObject.error) {
        var __e;
        __e = new Error(this.fResponseObject.error.message);
        if  (this.fResponseObject.error.name == "ROJSONException") {
            __e.name = this.fResponseObject.error.roexception.type;
        } else {
            __e.name = this.fResponseObject.error.name;
        };
        throw __e;
    };
    if (!this.fResponseObject.result) {
        this.fResponseObject.result = this.fResponseObject.params;
    };
};

RemObjects.SDK.EventReceiver.prototype.addHandler = function addHandler(anEventName, aCallback) {
    this.fHandlers[anEventName] = aCallback;
};

RemObjects.SDK.EventReceiver.prototype.setActive = function setActive(aValue) {
    this.fActive = aValue;
    if (this.fActive) {
        var that = this;
        this.fInterval = setInterval(function () {that.intPollServer();}, this.fTimeout);
    } else {
        clearInterval(this.fInterval);
    };
};

RemObjects.SDK.EventReceiver.prototype.getActive = function getActive() {
    return this.fActive;
};

RemObjects.SDK.EventReceiver.prototype.getTimeout = function getTimeout() {
    return this.fTimeout;
};

RemObjects.SDK.EventReceiver.prototype.setTimeout = function setTimeout(aValue) {
    this.fTimeout = aValue;
    if (this.fActive) {
        this.setActive(false);
        this.setActive(true);
    };
};

RemObjects.SDK.EventReceiver.prototype.intPollServer = function intPollServer() {
    try {
        var that = this;
        var msg = this.fMessage.clone();
        msg.initialize(this.fServiceName, "poll", RemObjects.SDK.Enum.MessageType.mtPoll);

        msg.write("MaxMessageCount", "Integer", 10);
        if (msg instanceof RemObjects.SDK.BinMessage) {
            msg.write("", "Guid", msg.getClientID());
        };

        msg.finalize();
        this.fChannel.dispatch(msg, function (__message) {
            var msgCount = __message.read("MessageCount", "Integer");
            var msgLeft = __message.read("MessagesLeft", "Integer");
            for (var i = 0; i < msgCount; i++) {
                var events = __message.clone();
				events.initialize("", "", RemObjects.SDK.Enum.MessageType.mtPollResponse);
                var eventsStream = __message.read("Message_" + i, "Binary");
                if (__message instanceof RemObjects.SDK.JSONMessage) {
                    eventsStream = RemObjects.UTIL.fromBase64(eventsStream);
                };
                events.setResponseStream(eventsStream);
                var sinkName = events.read("InterfaceName", "AnsiString");
                var eventName = events.read("MessageName", "AnsiString");
                if (RemObjects.SDK.RTTI[sinkName] && RemObjects.SDK.RTTI[sinkName].prototype instanceof RemObjects.SDK.ROComplexType) {
                    var sink = new RemObjects.SDK.RTTI[sinkName]();
                    sink.readEvent(events, eventName);
                    if (that.fHandlers[eventName]) {
                        that.fHandlers[eventName](RemObjects.SDK.ROStructType.prototype.toObject.call(sink[eventName]));
                    };
                } else {
                    throw new Error("EventReceiver.intPollServer: unknown event sink");
                };
            };
        }, RemObjects.UTIL.showError);

    } catch (e) {
        RemObjects.UTIL.showError(msg, e);
    };
        
};


function TitaniumAjaxWrapper(url) {
            this.updating = false;
            this.urlCall = url;
};


TitaniumAjaxWrapper.prototype.post = function post(passData, isBinary, onSuccessFunction, onErrorFunction) {

    if (this.updating) {
        return false;
    };
    this.AJAX = null;

    this.AJAX = Ti.Network.createHTTPClient({
        onload: function(e) {
            Ti.API.info("onload");
            if (isBinary) {
                onSuccessFunction(this.responseData, 200);
            } else {
                onSuccessFunction(this.responseText, 200);
            };
        },
        onerror: function(e) {
            Ti.API.info('XHR Error ' + e.error);
        },
        timeout:5000
    });
    this.updating = new Date();
    var uri = this.urlCall + '?' + this.updating.getTime();

    Ti.API.info("TitaniumAjaxWrapper " + uri);
    this.AJAX.open("POST", uri);
    // if (isBinary) {
    //     this.AJAX.setRequestHeader('Content-Type','multipart/form-data');
    // }
    this.AJAX.send(passData);
};

function AjaxWrapper(url) {
            this.updating = false;
            this.urlCall = url;
};


AjaxWrapper.prototype.abort = function abort() {
    if (this.updating) {
        this.updating = false;
        this.AJAX.abort();
        this.AJAX = null;
    };
};

AjaxWrapper.prototype.post = function post(passData, isBinary, onSuccessFunction, onErrorFunction) {
    function isIE10Up() {
        if (!navigator)
            return false;
        // Project Spartan is not yet supported
        // This code detects only IE 10 and IE 11
        return (navigator.userAgent.indexOf("MSIE 10") != -1)
                    || (!!navigator.userAgent.match(/Trident\/7\./));
    };

    if (this.updating) {
        return false;
    };
    this.AJAX = null;

    try {
        this.AJAX = new XMLHttpRequest();
    } catch (e) {
        // Internet Explorer Browsers
        try {
            this.AJAX = new ActiveXObject("Msxml2.XMLHTTP");
        } catch (e) {
            try {
                this.AJAX = new ActiveXObject("Microsoft.XMLHTTP");
            } catch (e) {
                throw new Error("Your browser doesn't support XMLHttpRequest object");
            };
        };
    };


         if (this.AJAX == null) {
             return false;
         } else {
             this.onSuccess = onSuccessFunction || function () {
             };
             this.onError = onErrorFunction || function () {
             };
             var that = this;

             this.AJAX.onreadystatechange = function onreadystatechange() {
                 if (that.AJAX.readyState == 4) {
                     that.updating = false;
                     if (that.AJAX.status == 200) {
                         if (isIE10Up() && typeof(that.AJAX.response) != "string") {
                             var response = "";
                             var arr = new Uint8Array(that.AJAX.response);
                             for (var i = 0; i < arr.length; i++) response += String.fromCharCode(arr[i]);
                             that.onSuccess(response, that.AJAX.status/*, that.AJAX.responseXML*/);
                         } else {
                             that.onSuccess(that.AJAX.responseText, that.AJAX.status, that.AJAX.responseXML);
                         };
                     } else {
                        that.onError(that.AJAX.responseText, that.AJAX.status, that.AJAX.responseXML);
                     };
                     that.AJAX = null;
                 };
             };
             this.updating = new Date();
                 var uri = this.urlCall + '?' + this.updating.getTime();
                 this.AJAX.open("POST", uri, true);
                 //this.AJAX.setRequestHeader("Content-Length", passData.length);
                 if (isBinary == true) {
                    if (this.AJAX.overrideMimeType)
                        this.AJAX.overrideMimeType('text/plain; charset=x-user-defined');
                    this.AJAX.setRequestHeader("Content-type", "application/octet-stream");
                    if (this.AJAX.sendAsBinary) {
                        this.AJAX.sendAsBinary(passData);
                    } else {
                        var len = passData.length;
                        var data = new Uint8Array(len);
                        for (var i=0; i<len; i++) {
                            data[i] = passData.charCodeAt(i);
                        };
                        if (isIE10Up()) {
                            this.AJAX.responseType = "arraybuffer";
                        };
                        this.AJAX.send(data.buffer);
                    };
                 } else {
                     this.AJAX.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
                     this.AJAX.send(passData);
                 };
            return true;
         };
};

// binary parser by Jonas Raoni Soares Silva
function BinaryParser() {
    this.BigEndian = true;
};

BinaryParser.prototype.warn = function warn(msg) {throw new Error(msg)};

BinaryParser.prototype.decodeFloat = function decodeFloat( data, precisionBits, exponentBits ){
    var b = new this.Buffer( this.bigEndian, data );
    b.checkBuffer( precisionBits + exponentBits + 1 );
    var bias = Math.pow( 2, exponentBits - 1 ) - 1, signal = b.readBits( precisionBits + exponentBits, 1 ), exponent = b.readBits( precisionBits, exponentBits ), significand = 0,
    divisor = 2, curByte = b.buffer.length + ( -precisionBits >> 3 ) - 1;
    do
        for( var byteValue = b.buffer[ ++curByte ], startBit = precisionBits % 8 || 8, mask = 1 << startBit; mask >>= 1; ( byteValue & mask ) && ( significand += 1 / divisor ), divisor *= 2 );
    while( precisionBits -= startBit );
    return exponent == ( bias << 1 ) + 1 ? significand ? NaN : signal ? -Infinity : +Infinity : ( 1 + signal * -2 ) * ( exponent || significand ? !exponent ? Math.pow( 2, -bias + 1 ) * significand : Math.pow( 2, exponent - bias ) * ( 1 + significand ) : 0 );
};
BinaryParser.prototype.encodeFloat = function encodeFloat( data, precisionBits, exponentBits ){
    var bias = Math.pow( 2, exponentBits - 1 ) - 1, minExp = -bias + 1, maxExp = bias, minUnnormExp = minExp - precisionBits,
    status = isNaN( n = parseFloat( data ) ) || n == -Infinity || n == +Infinity ? n : 0,
    exp = 0, len = 2 * bias + 1 + precisionBits + 3, bin = new Array( len ),
    signal = ( n = status !== 0 ? 0 : n ) < 0, n = Math.abs( n ), intPart = Math.floor( n ), floatPart = n - intPart,
    i, lastBit, rounded, j, result;
    for( i = len; i; bin[--i] = 0 );
    for( i = bias + 2; intPart && i; bin[--i] = intPart % 2, intPart = Math.floor( intPart / 2 ) );
    for( i = bias + 1; floatPart > 0 && i; ( bin[++i] = ( ( floatPart *= 2 ) >= 1 ) - 0 ) && --floatPart );
    for( i = -1; ++i < len && !bin[i]; );
    if( bin[( lastBit = precisionBits - 1 + ( i = ( exp = bias + 1 - i ) >= minExp && exp <= maxExp ? i + 1 : bias + 1 - ( exp = minExp - 1 ) ) ) + 1] ){
        if( !( rounded = bin[lastBit] ) ) {
            for( j = lastBit + 2; !rounded && j < len; rounded = bin[j++] ) {};
        };
        for( j = lastBit + 1; rounded && --j >= 0; ( bin[j] = !bin[j] - 0 ) && ( rounded = 0 ) ){};
    };
    for( i = i - 2 < 0 ? -1 : i - 3; ++i < len && !bin[i]; ){};
    if( ( exp = bias + 1 - i ) >= minExp && exp <= maxExp ) {
        ++i;
    } else if( exp < minExp ){
        exp != bias + 1 - len && exp < minUnnormExp && this.warn( "encodeFloat::float underflow" );
        i = bias + 1 - ( exp = minExp - 1 );
    };
    if( intPart || status !== 0 ){
        this.warn( intPart ? "encodeFloat::float overflow" : "encodeFloat::" + status );
        exp = maxExp + 1;
        i = bias + 2;
        if( status == -Infinity ) {
            signal = 1;
        } else if( isNaN( status ) )
            bin[i] = 1;
    };
    for( n = Math.abs( exp + bias ), j = exponentBits + 1, result = ""; --j; result = ( n % 2 ) + result, n = n >>= 1 );
    for( n = 0, j = 0, i = ( result = ( signal ? "1" : "0" ) + result + bin.slice( i, i + precisionBits ).join( "" ) ).length, r = []; i; j = ( j + 1 ) % 8 ){
        n += ( 1 << j ) * result.charAt( --i );
        if( j == 7 ){
            r[r.length] = String.fromCharCode( n );
            n = 0;
        };
    };
    r[r.length] = n ? String.fromCharCode( n ) : "";
    return ( this.bigEndian ? r.reverse() : r ).join( "" );
};

BinaryParser.prototype.encodeInt = function encodeInt(number, bits, signed){
           var max = Math.pow(2, bits), r = [];
           (number >= max || number < -Math.pow(2, bits-1)) && this.warn("encodeInt::overflow") && (number = 0);
           number < 0 && (number += max);
           for(; number; r[r.length] = String.fromCharCode(number % 256), number = Math.floor(number / 256));
           for(bits = -(-bits >> 3) - r.length; bits--; r[r.length] = "\0");
           return (this.bigEndian ? r.reverse() : r).join("");
       };

BinaryParser.prototype.decodeInt = function decodeInt(data, bits, signed){
           var b = new this.Buffer(this.bigEndian, data), x = b.readBits(0, bits), max = Math.pow(2, bits);
           return signed && x >= max / 2 ? x - max : x;
       };



   with({p: (BinaryParser.prototype.Buffer = function Buffer(bigEndian, buffer){
   this.bigEndian = bigEndian || 0, this.buffer = [], this.setBuffer(buffer);
   }).prototype}){
   p.readBits = function(start, length){
       //shl fix: Henri Torgemane ~1996 (compressed by Jonas Raoni)
       function shl(a, b){
//           for(++b; --b; a = ((a %= 0x7fffffff + 1) & 0x40000000) == 0x40000000 ? a * 2 : (a - 0x40000000) * 2 + 0x7fffffff + 1);
           for(++b; --b; a = ((a %= 0x7fffffffffff + 1) & 0x400000000000) == 0x400000000000 ? a * 2 : (a - 0x400000000000) * 2 + 0x7fffffffffff + 1);
           return a;
       };
       if(start < 0 || length <= 0)
           return 0;
       this.checkBuffer(start + length);
       for(var offsetLeft, offsetRight = start % 8, curByte = this.buffer.length - (start >> 3) - 1,
           lastByte = this.buffer.length + (-(start + length) >> 3), diff = curByte - lastByte,
           sum = ((this.buffer[ curByte ] >> offsetRight) & ((1 << (diff ? 8 - offsetRight : length)) - 1))
           + (diff && (offsetLeft = (start + length) % 8) ? (this.buffer[ lastByte++ ] & ((1 << offsetLeft) - 1))
           << (diff-- << 3) - offsetRight : 0); diff; sum += shl(this.buffer[ lastByte++ ], (diff-- << 3) - offsetRight)
       );
       return sum;
   };
   p.setBuffer = function setBuffer(data){
       if(data){
           for(var l, i = l = data.length, b = this.buffer = new Array(l); i; b[l - i] = data.charCodeAt(--i) & 0xFF);
//           for(var l, i = l = data.length, b = this.buffer = new Array(l); i; b[l - i] = data.charCodeAt(--i));
           this.bigEndian && b.reverse();
       };
   };
   p.hasNeededBits = function hasNeededBits(neededBits){
       return this.buffer.length >= -(-neededBits >> 3);
   };
   p.checkBuffer = function checkBuffer(neededBits){
       if(!this.hasNeededBits(neededBits)) {
           throw new Error("checkBuffer::missing bytes");};
   };
   };

/*
DEFLATE implementation based on http://www.codeproject.com/Articles/26980/Binary-Formats-in-JavaScript-Base64-Deflate-and-UT
Copyright (c) 2008 notmasteryet

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
*/
RemObjects.ZLIB = {
    staticCodes:null,
    staticDistances:null,
    encodedLengthStart:new Array(3, 4, 5, 6, 7, 8, 9, 10,
            11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99,
            115, 131, 163, 195, 227, 258),
    encodedLengthAdditionalBits:new Array(0, 0, 0, 0, 0, 0, 0, 0,
            1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0),
    encodedDistanceStart:new Array(1, 2, 3, 4, 5, 7, 9,
            13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049,
            3073, 4097, 6145, 8193, 12289, 16385, 24577),
    encodedDistanceAdditionalBits:new Array(0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4,
            5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13),
    clenMap:new Array(16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15),


    buildCodes:function buildCodes(lengths) {
        var codes = new Array(lengths.length);
        var maxBits = lengths[0];
        for (var i = 1; i < lengths.length; i++) {
            if (maxBits < lengths[i]) maxBits = lengths[i];
        };

        var bitLengthsCount = new Array(maxBits + 1);
        for (var i = 0; i <= maxBits; i++) bitLengthsCount[i] = 0;

        for (var i = 0; i < lengths.length; i++) {
            ++bitLengthsCount[lengths[i]];
        };

        var nextCode = new Array(maxBits + 1);
        var code = 0;
        bitLengthsCount[0] = 0;
        for (var bits = 1; bits <= maxBits; bits++) {
            code = (code + bitLengthsCount[bits - 1]) << 1;
            nextCode[bits] = code;
        };

        for (var n = 0; n < codes.length; n++) {
            var len = lengths[n];
            if (len != 0) {
                codes[n] = nextCode[len];
                nextCode[len]++;
            };
        };
        return codes;
    },

    initializeStaticTrees:function initializeStaticTrees() {
        var codes = new Array(288);
        var codesLengths = new Array(288);

        for (var i = 0; i <= 143; i++) {
            codes[i] = 0x0030 + i;
            codesLengths[i] = 8;
        };
        for (var i = 144; i <= 255; i++) {
            codes[i] = 0x0190 + i - 144;
            codesLengths[i] = 9;
        };
        for (var i = 256; i <= 279; i++) {
            codes[i] = 0x0000 + i - 256;
            codesLengths[i] = 7;
        };
        for (var i = 280; i <= 287; i++) {
            codes[i] = 0x00C0 + i - 280;
            codesLengths[i] = 8;
        };
        RemObjects.ZLIB.staticCodes = RemObjects.ZLIB.buildTree(codes, codesLengths);

        var distances = new Array(32);
        var distancesLengths = new Array(32);
        for (var i = 0; i <= 31; i++) {
            distances[i] = i;
            distancesLengths[i] = 5;
        };
        RemObjects.ZLIB.staticDistances = RemObjects.ZLIB.buildTree(distances, distancesLengths);
    },

    buildTree:function buildTree(codes, lengths) {
        var nonEmptyCodes = new Array(0);
        for (var i = 0; i < codes.length; ++i) {
            if (lengths[i] > 0) {
                var code = new Object();
                code.bits = codes[i];
                code.length = lengths[i];
                code.index = i;
                nonEmptyCodes.push(code);
            };
        };
        return RemObjects.ZLIB.buildTreeBranch(nonEmptyCodes, 0, 0);
    },

    buildTreeBranch: function buildTreeBranch(codes, prefix, prefixLength) {
        if (codes.length == 0) return null;

        var zeros = new Array(0);
        var ones = new Array(0);
        var branch = new Object();
        branch.isLeaf = false;
        for (var i = 0; i < codes.length; ++i) {
            if (codes[i].length == prefixLength && codes[i].bits == prefix) {
                branch.isLeaf = true;
                branch.index = codes[i].index;
                break;
            } else {
                var nextBit = ((codes[i].bits >> (codes[i].length - prefixLength - 1)) & 1) > 0;
                if (nextBit) {
                    ones.push(codes[i]);
                } else {
                    zeros.push(codes[i]);
                };
            };
        };
        if (!branch.isLeaf) {
            branch.zero = RemObjects.ZLIB.buildTreeBranch(zeros, (prefix << 1), prefixLength + 1);
            branch.one = RemObjects.ZLIB.buildTreeBranch(ones, (prefix << 1) | 1, prefixLength + 1);
        };
        return branch;
    },

    readDynamicTrees:function readDynamicTrees(bitReader) {
        var hlit = bitReader.readLSB(5) + 257;
        var hdist = bitReader.readLSB(5) + 1;
        var hclen = bitReader.readLSB(4) + 4;

        var clen = new Array(19);
        for (var i = 0; i < clen.length; ++i) clen[i] = 0;
        for (var i = 0; i < hclen; ++i) clen[RemObjects.ZLIB.clenMap[i]] = bitReader.readLSB(3);

        var clenCodes = RemObjects.ZLIB.buildCodes(clen);
        var clenTree = RemObjects.ZLIB.buildTree(clenCodes, clen);

        var lengthsSequence = new Array(0);
        while (lengthsSequence.length < hlit + hdist) {
            var p = clenTree;
            while (!p.isLeaf) {
                p = bitReader.readBit() ? p.one : p.zero;
            };

            var code = p.index;
            if (code <= 15) {
                lengthsSequence.push(code);
            } else if (code == 16) {
                var repeat = bitReader.readLSB(2) + 3;
                for (var q = 0; q < repeat; ++q)
                    lengthsSequence.push(lengthsSequence[lengthsSequence.length - 1]);
            } else if (code == 17) {
                var repeat = bitReader.readLSB(3) + 3;
                for (var q = 0; q < repeat; ++q)
                    lengthsSequence.push(0);
            } else if (code == 18) {
                var repeat = bitReader.readLSB(7) + 11;
                for (var q = 0; q < repeat; ++q)
                    lengthsSequence.push(0);
            };
        };

        var codesLengths = lengthsSequence.slice(0, hlit);
        var codes = RemObjects.ZLIB.buildCodes(codesLengths);
        var distancesLengths = lengthsSequence.slice(hlit, hlit + hdist);
        var distances = RemObjects.ZLIB.buildCodes(distancesLengths);

        var result = new Object();
        result.codesTree = RemObjects.ZLIB.buildTree(codes, codesLengths);
        result.distancesTree = RemObjects.ZLIB.buildTree(distances, distancesLengths);
        return result;
    },

    BitReader:function BitReader(reader) {
        this.bitsLength = 0;
        this.bits = 0;
        this.reader = reader;
        this.readBit = function () {
            if (this.bitsLength == 0) {
                var nextByte = this.reader.readByte();
                if (nextByte < 0) throw new "Unexpected end of stream";
                this.bits = nextByte;
                this.bitsLength = 8;
            };

            var bit = (this.bits & 1) != 0;
            this.bits >>= 1;
            --this.bitsLength;
            return bit;
        };
        this.align = function () {
            this.bitsLength = 0;
        };
        this.readLSB = function (length) {
            var data = 0;
            for (var i = 0; i < length; ++i) {
                if (this.readBit()) data |= 1 << i;
            };
            return data;
        };
        this.readMSB = function (length) {
            var data = 0;
            for (var i = 0; i < length; ++i) {
                if (this.readBit()) data = (data << 1) | 1; else data <<= 1;
            };
            return data;
        };
    },


    Inflator:function Inflator(reader) {
        this.reader = reader;
        this.bitReader = new RemObjects.ZLIB.BitReader(reader);
        this.buffer = new Array(0);
        this.bufferPosition = 0;
        this.state = 0;
        this.blockFinal = false;
        this.readByte = function () {
            while (this.bufferPosition >= this.buffer.length) {
                var item = this.decodeItem();
                if (item == null) return -1;
                switch (item.itemType) {
                    case 0:
                        this.buffer = this.buffer.concat(item.array);
                        break;
                    case 2:
                        this.buffer.push(item.symbol);
                        break;
                    case 3:
                        var j = this.buffer.length - item.distance;
                        for (var i = 0; i < item.length; i++) {
                            this.buffer.push(this.buffer[j++]);
                        };
                        break;
                };
            };
            var symbol = this.buffer[this.bufferPosition++];
            if (this.bufferPosition > 0xC000) {
                var shift = this.buffer.length - 0x8000;
                if (shift > this.bufferPosition) shift = this.bufferPosition;
                this.buffer.splice(0, shift);
                this.bufferPosition -= shift;
            };
            return symbol;
        };

        this.decodeItem = function () {
            if (this.state == 2) return null;

            var item;
            if (this.state == 0) {
                this.blockFinal = this.bitReader.readBit();
                var blockType = this.bitReader.readLSB(2);
                switch (blockType) {
                    case 0:
                        this.bitReader.align();
                        var len = this.bitReader.readLSB(16);
                        var nlen = this.bitReader.readLSB(16);
                        if ((len & ~nlen) != len) throw "Invalid block type 0 length";

                        item = new Object();
                        item.itemType = 0;
                        item.array = new Array(len);
                        for (var i = 0; i < len; ++i) {
                            var nextByte = this.reader.readByte();
                            if (nextByte < 0) throw "Uncomplete block";
                            item.array[i] = nextByte;
                        }
                        if (this.blockFinal) this.state = 2;
                        return item;
                    case 1:
                        this.codesTree = RemObjects.ZLIB.staticCodes;
                        this.distancesTree = RemObjects.ZLIB.staticDistances;
                        this.state = 1;
                        break;
                    case 2:
                        var dynamicTrees = RemObjects.ZLIB.readDynamicTrees(this.bitReader);
                        this.codesTree = dynamicTrees.codesTree;
                        this.distancesTree = dynamicTrees.distancesTree;
                        this.state = 1;
                        break;
                    default:
                        throw new "Invalid block type (3)";
                };
            };

            item = new Object();
            var p = this.codesTree;
            while (!p.isLeaf) {
                p = this.bitReader.readBit() ? p.one : p.zero;
            };
            if (p.index < 256) {
                item.itemType = 2;
                item.symbol = p.index;
            } else if (p.index > 256) {
                var lengthCode = p.index;
                if (lengthCode > 285) throw new "Invalid length code";

                var length = RemObjects.ZLIB.encodedLengthStart[lengthCode - 257];
                if (RemObjects.ZLIB.encodedLengthAdditionalBits[lengthCode - 257] > 0) {
                    length += this.bitReader.readLSB(RemObjects.ZLIB.encodedLengthAdditionalBits[lengthCode - 257]);
                };

                p = this.distancesTree;
                while (!p.isLeaf) {
                    p = this.bitReader.readBit() ? p.one : p.zero;
                };

                var distanceCode = p.index;
                var distance = RemObjects.ZLIB.encodedDistanceStart[distanceCode];
                if (RemObjects.ZLIB.encodedDistanceAdditionalBits[distanceCode] > 0) {
                    distance += this.bitReader.readLSB(RemObjects.ZLIB.encodedDistanceAdditionalBits[distanceCode]);
                };

                item.itemType = 3;
                item.distance = distance;
                item.length = length;
            } else {
                item.itemType = 1;
                this.state = this.blockFinal ? 2 : 0;
            };
            return item;
        };
    }
};

RemObjects.ZLIB.initializeStaticTrees();