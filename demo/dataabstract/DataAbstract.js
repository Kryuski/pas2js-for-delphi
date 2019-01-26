RemObjects.UTIL.toBase64 = function (aValue) {
    if (typeof (btoa) != 'undefined') {
        return btoa(aValue);
    } else {
        throw (new Error("Base64 encoding is not supported by your browser."));
        //return $.base64Encode(aValue);
    };
};

RemObjects.UTIL.fromBase64 = function (aValue) {
    if (typeof (atob) != 'undefined') {
        return atob(aValue.replace(/(\n|\r)+/g, ""));
    } else {
        throw (new Error("Base64 decoding is not supported by your browser."));
        //      return $.base64Decode(aValue);
    };

};

RemObjects.DataAbstract = {
    Enum: {},

    Util: {},

    Views: {},

    Scripting: {},

    DADataType: ["datUnknown", "datString", "datDateTime", "datFloat", "datCurrency",
        "datAutoInc", "datInteger", "datLargeInt", "datBoolean", "datMemo",
        "datBlob", "datWideString", "datWideMemo", "datLargeAutoInc", "datByte",
        "datShortInt", "datWord", "datSmallInt", "datCardinal", "datLargeUInt",
        "datGuid", "datXml", "datDecimal", "datSingleFloat", "datFixedChar", "datFixedWideChar", "datCursor"],

    //    RemoteDataAdapter : function RemoteDataAdapter(aDataService, aLoginService, aStreamerClass) { //old api
    RemoteDataAdapter: function RemoteDataAdapter(aURL, aDataServiceName, aLoginServiceName, aStreamerClass) {
        var that = this;

        this.fService = new RemObjects.DataAbstract.Server.DataAbstractService(arguments[0]);
        this.fLoginService = new RemObjects.SDK.RemoteService(this.fService.fChannel, this.fService.fMessage, "LoginService");
        if (arguments.length == 0)
            this.fService.fChannel = { onLoginNeeded: null }; //hack for docGen
        if (!(arguments[0] instanceof RemObjects.SDK.RemoteService)) this.fService.fServiceName = "DataService";
        this.fStreamerClass = RemObjects.DataAbstract.Bin2DataStreamer;

        if (RemObjects.UTIL.checkArgumentTypes(arguments, [RemObjects.SDK.RemoteService, RemObjects.SDK.RemoteService])) {
            this.fLoginService = arguments[1];
            this.fStreamerClass = arguments[2] || RemObjects.DataAbstract.Bin2DataStreamer;
        } else if (RemObjects.UTIL.checkArgumentTypes(arguments, ["string", "string", "string"])) { //URL, DataServiceName, LoginServiceName
            this.fService.fServiceName = arguments[1];
            this.fLoginService.fServiceName = arguments[2];
            if (RemObjects.UTIL.checkArgumentTypes(arguments, ["string", "string", "string", Function])) {
                this.fStreamerClass = arguments[3];
            };
        } else if (RemObjects.UTIL.checkArgumentTypes(arguments, ["string", "string"])) { //URL, DataServiceName
            this.fService.fServiceName = arguments[1];
            if (RemObjects.UTIL.checkArgumentTypes(arguments, ["string", "string", Function])) {
                this.fStreamerClass = arguments[2];
            };
        } else if (RemObjects.UTIL.checkArgumentTypes(arguments, ["string"])) { //URL
            if (RemObjects.UTIL.checkArgumentTypes(arguments, ["string", Function])) {
                this.fStreamerClass = arguments[1];
            };
        };

        this.fSavedOnLoginNeeded = this.fService.fChannel.onLoginNeeded;
        this.fService.fChannel.onLoginNeeded = function (aCallback) { that.intOnLoginNeeded(aCallback) };
        this.fSendReducedDelta = false;
        this.fAutoGetScripts = false;
        //methods:
        //setSendReducedDelta
        //buildDelta
        //createStreamer
        //onLoginNeeded
        //login

        //asynchronous methods (pass callback function):
        //getSchema
        //createTableFromSchema
        //getData
        //applyUpdates
        //getDataService
        //getLoginService
        //executeCommand


    },

    Deltas: function Deltas() {
        this.deltas = [];
        //methods
        //findByName

    },

    Delta: function Delta() {
        this.name = "";
        this.loggedfields = [];
        this.keyfields = [];
        this.data = [];
    },

    Change: function Change() {
        this.recid = 0;
        this.changetype = "";
        this.status = "";
        this.message = "";
        this["old"] = [];
        this["new"] = [];

    },

    DataTable: function DataTable(aName) {
        this.name = aName || "";
        this.fields = [];
        this.keyfields = [];
        this.rows = [];
        this.deletedRows = [];
        this.fRecordBuffer = [];
        this.fNextRecId = 0;
        this.fIndex = -1;
        this.bofFlag = true;
        this.eofFlag = true;
        this.dynamicWhere = null;
        this.onNewRecord = null;
        this.onBeforeDelete = null;
        this.onAfterDelete = null;
        this.onBeforeScroll = null;
        this.onAfterScroll = null;
        //methods
        //appendRow
        //deleteRow
        //markDeleted
        //currentRow
        //first
        //next
        //prev
        //last
        //findId
        //intFindDeleted
        //eof
        //bof
        //getFieldValue
        //setFieldValue
        //getFieldAsString
        //setFieldAsString
        //addLookupField
        //fieldNumByName
        //fieldByName
        //locate

    },

    Field: function Field(aName, aType) {
        this.name = aName || "";
        this.type = aType || "";
        this.logChanges = true;
    },

    LookupField: function LookupField(aName, aType) {
        RemObjects.DataAbstract.Field.call(this, aName, aType);
        this.logChanges = false;
        this.sourceField = "";
        this.lookupTable = null;
        this.lookupKeyField = "";
        this.lookupResultField = "";
    },

    DataTableRow: function DataTableRow() {
        this.recId = 0;
        this.state = RemObjects.DataAbstract.Enum.RowStates.rsUnchanged;
        this.__oldValues = [];
        this.__newValues = [];
    },

    DataStreamer: function DataStreamer() {

    },

    JSONDataStreamer: function JSONDataStreamer() {
        //methods:
        //setStream
        //getStream
        //initializeRead
        //initializeWrite
    },

    Bin2DataStreamer: function Bin2DataStreamer() {
        this.fStream = "";
        this.fStreamPos = 0;
        this.fStreamInfoPos = 0;
        this.fParser = new BinaryParser();
        //methods:
        //setStream
        //getStream
        //initializeRead
        //initializeWrite
        //readByte
        //readInteger
        //readAnsiStringWithLength
        //readDataSet
        //readField
        //writeDelta
    },

    DynamicWhere: function DynamicWhere(anExpression) {
        this.fExpression = anExpression;
        //toXML
    },

    ConstantExpression: function ConstantExpression(aType, aValue, aNull) {
        this.fType = aType;
        this.fValue = aValue;
        this.fNull = aNull;
        //toXML
    },

    NullExpression: function NullExpression() {

    },

    FieldExpression: function FieldExpression(aName) {
        this.fName = aName;
    },

    MacroExpression: function MacroExpression(aName) {
        this.fName = aName;
    },

    UnaryExpression: function UnaryExpression(aNode, anOperator) {
        this.fNode = aNode;
        this.fOperator = anOperator;
    },

    BinaryExpression: function BinaryExpression(aNode1, aNode2, anOperator) {
        this.fNode1 = aNode1;
        this.fNode2 = aNode2;
        this.fOperator = anOperator;
    },

    BetweenExpression: function BetweenExpression(aNode1, aNode2, aNode3) {
        this.fNode1 = aNode1;
        this.fNode2 = aNode2;
        this.fNode3 = aNode3;
    },

    ParameterExpression: function ParameterExpression(aName, aType, aSize) {
        this.fName = aName;
        this.fType = aType;
        this.fSize = aSize;
    },

    ListExpression: function ListExpression(anItems) {
        this.fItems = anItems;
    }

};

RemObjects.DataAbstract.Enum.RowStates = {
    //DataTableRow states
    rsUnchanged: 0,
    rsModified: 1,
    rsNew: 2,
    rsDeleted: 3
};

RemObjects.DataAbstract.Enum.ChangeType = { ctInsert: 0, ctUpdate: 1, ctDelete: 2 };
RemObjects.DataAbstract.Enum.ChangeTypeNames = ["insert", "update", "delete"];
RemObjects.DataAbstract.Enum.ChangeStatus = { csPending: 0, csResolved: 1, csFailed: 2 };
RemObjects.DataAbstract.Enum.ChangeStatusNames = ["pending", "resolved", "failed"];


RemObjects.DataAbstract.Util.createDataParameter = function createDataParameter(aName, aValue) {
    var result = new RemObjects.DataAbstract.Server.DataParameter();
    result.Name.value = aName;
    result.Value.value = aValue;
    return result;
};


RemObjects.DataAbstract.Util.createRequestInfo = function createRequestInfo(includeSchema, maxRecords, userFilter, parameters) {
    var result = new RemObjects.DataAbstract.Server.TableRequestInfo();
    if (arguments.length == 0) {
        result.IncludeSchema.value = true;
        result.MaxRecords.value = -1;
        result.UserFilter.value = "";
        result.Parameters.value = new RemObjects.DataAbstract.Server.DataParameterArray();
        result.Parameters.value.items = [];
    } else {
        result.IncludeSchema.value = includeSchema;
        result.MaxRecords.value = maxRecords;
        result.UserFilter.value = userFilter;
        result.Parameters.value = new RemObjects.DataAbstract.Server.DataParameterArray();
        result.Parameters.value.items = parameters;
    };
    return result;
};

RemObjects.DataAbstract.Util.createRequestInfoV5 = function createRequestInfoV5(includeSchema, maxRecords, userFilter, parameters) {
    var ri = RemObjects.DataAbstract.Util.createRequestInfo.apply(this, arguments);
    var result = new RemObjects.DataAbstract.Server.TableRequestInfoV5();
    result.IncludeSchema = ri.IncludeSchema;
    result.MaxRecords = ri.MaxRecords;
    result.UserFilter = ri.UserFilter;
    result.Parameters = ri.Parameters;

    result.WhereClause = { dataType: "Xml", value: null };
    result.DynamicSelectFieldNames = { dataType: "StringArray", value: new RemObjects.DataAbstract.Server.StringArray() };
    result.DynamicSelectFieldNames.value.items = [];
    result.Sorting = { dataType: "ColumnSorting", value: new RemObjects.DataAbstract.Server.ColumnSorting() };
    result.Sorting.value.fromObject({ FieldName: "", SortDirection: "Ascending" });
    return result;
};


RemObjects.DataAbstract.Util.createRequestInfoV6 = function createRequestInfoV6(sql, maxRecords, userFilter, parameters) {
    var result = new RemObjects.DataAbstract.Server.TableRequestInfoV6();
    if (arguments.length == 1) {
        result.Sql.value = sql;
        result.IncludeSchema.value = true;
        result.MaxRecords.value = -1;
        result.UserFilter.value = "";
        result.Parameters.value = new RemObjects.DataAbstract.Server.DataParameterArray();
        result.Parameters.value.items = [];
    } else {
        result.Sql.value = sql;
        result.IncludeSchema.value = true;
        result.MaxRecords.value = maxRecords;
        result.UserFilter.value = userFilter;
        result.Parameters.value = new RemObjects.DataAbstract.Server.DataParameterArray();
        result.Parameters.value.items = parameters;
    };
    return result;
};

RemObjects.DataAbstract.Util.setupScriptingCallbacks = function setupScriptingCallbacks() {
    for (var p in RemObjects.DataAbstract.Scripting) {
        eval("var " + p + " = RemObjects.DataAbstract.Scripting." + p);
        console.log("var " + p + " = RemObjects.DataAbstract.Scripting." + p);
    };
};

RemObjects.DataAbstract.Scripting.log = function log(str) {
    console.log(str);
};

RemObjects.DataAbstract.Scripting.fail = function fail(str) {
    throw new Error(str);
};

RemObjects.DataAbstract.Scripting.newGuidString = function newGuidString() {
    return RemObjects.UTIL.NewGuid();
};

//RO.DA implementation



RemObjects.DataAbstract.Deltas.prototype.findByName = function findByName(aName) {
    for (var i = 0; i < this.deltas.length; i++) {
        if (this.deltas[i].name.toUpperCase() == aName.toUpperCase()) {
            return this.deltas[i];
        };
    };
    return null;
};


RemObjects.DataAbstract.Delta.prototype.intFindId = function intFindId(anId) {
    for (var i = 0; i < this.data.length; i++) {
        if (this.data[i].recid == anId) {
            return i;
        };
    };
    return null;
};


RemObjects.DataAbstract.Field.prototype.checkReadOnly = function checkReadOnly() {
    if (this instanceof RemObjects.DataAbstract.LookupField) {
        throw new Error(this.name + ": lookup fields are read only");
    };
    if (this.readOnly == "True") {
        throw new Error(this.name + " is read only.");
    };
};

RemObjects.DataAbstract.LookupField.prototype = new RemObjects.DataAbstract.Field();

RemObjects.DataAbstract.DataTable.prototype.checkRequired = function checkRequired() {
    if (this.fIndex < 0) return;
    for (var i = 0; i < this.fields.length; i++) {
        if (this.fields[i].required == "True" && (this.fRecordBuffer[i] == null || this.fRecordBuffer[i] == undefined)) {
            throw new Error("Field " + this.fields[i].name + " is required.");
        };
    };
};


RemObjects.DataAbstract.DataTable.prototype.locate = function locate(aName, aValue) {
    this.post();
    var result = false;
    var fieldNum = this.fieldNumByName(aName);
    for (var i = 0; i < this.rows.length; i++) {
        if (this.rows[i].__newValues[fieldNum] == aValue) {
            this.fIndex = i;
            result = true;
            break;
        };
    };
    return result;
};


RemObjects.DataAbstract.DataTable.prototype.addLookupField = function addLookupField(aName, aSourceField, aLookupTable, aLookupKeyField, aLookupResultField) {
    var f = new RemObjects.DataAbstract.LookupField(aName);
    f.type = aLookupTable.fieldByName(aLookupResultField).type;
    f.lookupTable = aLookupTable;
    f.sourceField = aSourceField;
    f.lookupKeyField = aLookupKeyField;
    f.lookupResultField = aLookupResultField;
    this.fields.push(f);
};


RemObjects.DataAbstract.DataTable.prototype.getNextId = function getNextId() {
    return this.fNextRecId++;
};

RemObjects.DataAbstract.DataTable.prototype.intSetupProperties = function intSetupProperties(aRow) {
    if (!Object.defineProperty) return;
    var that = this;
    for (var j = 0; j < this.fields.length; j++) {
        (function (fieldNum) {

            Object.defineProperty(aRow, that.fields[fieldNum].name, {
                get: function () {
                    return this.__newValues[fieldNum];
                },
                set: function (aValue) {
                    if (this.__oldValues.length == 0)
                        this.__oldValues = this.__newValues.slice();
                    this.__oldValues[fieldNum] = this.__newValues[fieldNum];
                    this.__newValues[fieldNum] = aValue;
                    if (this.state == RemObjects.DataAbstract.Enum.RowStates.rsUnchanged)
                        this.state = RemObjects.DataAbstract.Enum.RowStates.rsModified;
                }
            });


        })(j);
    };

};

RemObjects.DataAbstract.DataTable.prototype.intAppendRow = function intAppendRow() {
    var row = new RemObjects.DataAbstract.DataTableRow();
    row.recId = this.getNextId();
    row.state = RemObjects.DataAbstract.Enum.RowStates.rsNew;
    row.__newValues = new Array(this.fields.length);
    this.rows.push(row);
    this.fIndex = this.rows.length - 1;
    this.eofFlag = false;
    this.bofFlag = false;

    this.intSetupProperties(row);

    return row;
};


RemObjects.DataAbstract.DataTable.prototype.appendRow = function appendRow() {
    //this.checkRequired();
    this.post();
    var row = this.intAppendRow();
    for (var i = 0; i < this.fields.length; i++) {
        if (typeof (this.fields[i].fAutoIncSequence) != 'undefined') {
            row.__newValues[i] = this.fields[i].fAutoIncSequence--;
        } else if (this.fields[i].defaultValue != "") {
            row.__newValues[i] = this.fields[i].defaultValue;
        };
    };

    if (this.__onNewRow) {
        var lRow = {};
        for (var i = 0; i < this.fields.length; i++) {
            lRow[this.fields[i].name] = row.__newValues[i];
        };
        this.__onNewRow(lRow);
        for (var i = 0; i < this.fields.length; i++) {
            row.__newValues[i] = lRow[this.fields[i].name];
        };
    };

    this.fRecordBuffer = row.__newValues.slice();
    if (this.onNewRecord) this.onNewRecord(row);
    return row;
};

RemObjects.DataAbstract.DataTable.prototype.deleteRow = function deleteRow() {
    if (this.rows.length == 0)
        throw new Error("DataTable.deleteRow: table is empty");

    if (this.__beforeDelete) {
        var row = this.currentRow();
        var lRow = {};
        for (var i = 0; i < this.fields.length; i++) {
            lRow[this.fields[i].name] = row.__newValues[i];
        };
        this.__beforeDelete(lRow);
    };

    if (this.onBeforeDelete) this.onBeforeDelete(this.currentRow());

    this.markDeleted();
    this.deletedRows.push(this.rows[this.fIndex]);
    this.rows.splice(this.fIndex, 1);
    if (this.fIndex == this.rows.length)
        this.fIndex--;
    this.eofFlag = (this.rows.length == 0);
    this.bofFlag = (this.rows.length == 0);
    this.fRecordBuffer = [];

    if (this.onAfterDelete && this.rows.length) this.onAfterDelete(this.currentRow());
};

RemObjects.DataAbstract.DataTable.prototype.markDeleted = function markDeleted() {
    if (this.rows[this.fIndex].__oldValues.length == 0)
        this.rows[this.fIndex].__oldValues = this.rows[this.fIndex].__newValues.slice();
    this.rows[this.fIndex].state = RemObjects.DataAbstract.Enum.RowStates.rsDeleted;
};

RemObjects.DataAbstract.DataTable.prototype.fieldNumByName = function fieldNumByName(aName) {
    var fieldNum = -1;
    for (var i = 0; i < this.fields.length; i++) {
        if (this.fields[i].name.toUpperCase() == aName.toUpperCase()) {
            fieldNum = i;
            break;
        };
    };
    if (fieldNum == -1)
        throw new Error("DataTable.fieldNumByName: no such field name - " + aName);
    return fieldNum;
};

RemObjects.DataAbstract.DataTable.prototype.fieldByName = function fieldByName(aName) {
    return this.fields[this.fieldNumByName(aName)];
};


RemObjects.DataAbstract.DataTable.prototype.setFieldValue = function setFieldValue(aField, aValue) {
    if (this.rows.length == 0)
        throw new Error("DataTable.setFieldValue: table is empty");
    var fieldNum = this.fieldNumByName(aField);
    var f = this.fields[fieldNum];
    f.checkReadOnly();
    if (this.rows[this.fIndex].__oldValues.length == 0)
        this.rows[this.fIndex].__oldValues = this.rows[this.fIndex].__newValues.slice();
    if (this.rows[this.fIndex].state == RemObjects.DataAbstract.Enum.RowStates.rsUnchanged)
        this.rows[this.fIndex].state = RemObjects.DataAbstract.Enum.RowStates.rsModified;
    if (this.fRecordBuffer.length == 0)
        this.fRecordBuffer = this.rows[this.fIndex].__newValues.slice();

    this.fRecordBuffer[fieldNum] = aValue;
};


RemObjects.DataAbstract.DataTable.prototype.getFieldValue = function getFieldValue(aField) {
    if (this.rows.length == 0)
        throw new Error("DataTable.getFieldValue: table is empty");
    var fieldNum = this.fieldNumByName(aField);
    var f = this.fields[fieldNum];
    if (f instanceof RemObjects.DataAbstract.LookupField) {
        if (f.lookupTable.locate(f.lookupKeyField, this.getFieldValue(f.sourceField))) {
            return f.lookupTable.getFieldValue(f.lookupResultField);
        } else {
            return null;
        };
    } else if (this.fRecordBuffer.length == 0) {
        return this.rows[this.fIndex].__newValues[fieldNum];
    } else {
        return this.fRecordBuffer[fieldNum];
    };
};


RemObjects.DataAbstract.DataTable.prototype.setFieldAsString = function setFieldAsString(aField, aValue) {
    if (this.rows.length == 0)
        throw new Error("DataTable.setFieldAsString: table is empty");
    var tempValue;
    var fieldNum = this.fieldNumByName(aField);
    var f = this.fields[fieldNum];
    f.checkReadOnly();
    if (this.rows[this.fIndex].__oldValues.length == 0)
        this.rows[this.fIndex].__oldValues = this.rows[this.fIndex].__newValues.slice();
    if (this.rows[this.fIndex].state == RemObjects.DataAbstract.Enum.RowStates.rsUnchanged)
        this.rows[this.fIndex].state = RemObjects.DataAbstract.Enum.RowStates.rsModified;
    if (this.fRecordBuffer.length == 0)
        this.fRecordBuffer = this.rows[this.fIndex].__newValues.slice();

    if (aValue == "") {
        if (this.rows[this.fIndex].__oldValues[fieldNum] == null)
            tempValue = null;
    } else {
        switch (this.fields[fieldNum].type) {
            //case "datBlob": return;
            case "datInteger":
            case "datAutoInc":
            case "datSmallInt":
            case "datByte":
            case "datLargeInt":
            case "datLargeAutoInc":
            case "datLargeUInt":
            case "datShortInt":
            case "datWord":
            case "datCardinal":
                tempValue = parseInt(aValue);
                break;
            case "datFloat":
            case "datCurrency":
            case "datSingleFloat":
            case "datDecimal":
                tempValue = parseFloat(aValue);
                break;
            case "datDateTime":
                tempValue = new Date(aValue);
                break;
            case "datBoolean":
                tempValue = (aValue.toUpperCase() == "TRUE");
            default:
                tempValue = aValue;
            //todo
        };
    };
    this.fRecordBuffer[fieldNum] = tempValue;
};


RemObjects.DataAbstract.DataTable.prototype.getFieldAsString = function getFieldAsString(aField) {
    var fieldNum = this.fieldNumByName(aField);
    var value = this.getFieldValue(aField);
    return (this.fields[fieldNum].type == "datBlob" ? "(Blob)" : (
        (value != undefined && value != null)
            ? value : "").toString());

};


RemObjects.DataAbstract.DataTable.prototype.currentRow = function currentRow() {
    return (this.rows.length ? this.rows[this.fIndex] : null);
};

RemObjects.DataAbstract.DataTable.prototype.intGoToIndex = function intGoToIndex(anIndex) {
    if (this.onBeforeScroll) this.onBeforeScroll(this.currentRow());
    this.fIndex = anIndex;
    if (this.onAfterScroll) this.onAfterScroll(this.currentRow());
};

RemObjects.DataAbstract.DataTable.prototype.first = function first() {
    this.post();
    if (this.rows.length > 0)
        this.intGoToIndex(0);
    this.bofFlag = true;
    this.eofFlag = false;
    return this.currentRow();
};

RemObjects.DataAbstract.DataTable.prototype.last = function last() {
    this.post();
    if (this.rows.length > 0)
        this.intGoToIndex(this.rows.length - 1);
    this.eofFlag = true;
    this.bofFlag = false;
    return this.currentRow();
};

RemObjects.DataAbstract.DataTable.prototype.next = function next() {
    this.post();
    if (this.fIndex < this.rows.length - 1) {
        this.intGoToIndex(this.fIndex + 1);
        this.bofFlag = false;
        return this.currentRow();
    } else {
        this.eofFlag = true;
        return null;
    };
};

RemObjects.DataAbstract.DataTable.prototype.prev = function prev() {
    this.post();
    if (this.fIndex > 0) {
        this.intGoToIndex(this.fIndex - 1);
        this.eofFlag = false;
        return this.currentRow();
    } else {
        this.bofFlag = true;
        return null;
    };
};

RemObjects.DataAbstract.DataTable.prototype.findId = function findId(anId) {
    this.post();
    for (var i = 0; i < this.rows.length; i++) {
        if (this.rows[i].recId == anId) {
            this.intGoToIndex(i);
            return this.currentRow();
        };
    };
    return null;
};

RemObjects.DataAbstract.DataTable.prototype.intFindDeleted = function intFindDeleted(anId) {
    for (var i = 0; i < this.deletedRows.length; i++) {
        if (this.deletedRows[i].recId == anId) {
            return i;
        };
    };
    return null;
};


RemObjects.DataAbstract.DataTable.prototype.eof = function eof() {
    return this.eofFlag;
};

RemObjects.DataAbstract.DataTable.prototype.bof = function bof() {
    return this.bofFlag;
};

RemObjects.DataAbstract.DataTable.prototype.post = function post() {
    var bufferNotSet = true;
    for (var i = 0; i < this.fRecordBuffer.length; i++) {
        if (typeof this.fRecordBuffer[i] != 'undefined')
        {
            bufferNotSet = false;
            break;
        }
    }

    if (bufferNotSet) {
        return;
    }


    if (this.__beforePost) {
        var lRow = {};
        for (var i = 0; i < this.fields.length; i++) {
            lRow[this.fields[i].name] = this.fRecordBuffer[i];
        };
        try {
            this.__beforePost(lRow);
            for (var i = 0; i < this.fields.length; i++) {
                this.fRecordBuffer[i] = lRow[this.fields[i].name];
            };
        } catch (e) { throw e };
    };

    this.checkRequired();
    this.rows[this.fIndex].__newValues = this.fRecordBuffer;
    this.fRecordBuffer = [];
};

RemObjects.DataAbstract.DataTable.prototype.cancel = function cancel() {
    this.fRecordBuffer = [];
};

RemObjects.DataAbstract.RemoteDataAdapter.prototype.getDataService = function getDataService() {
    return this.fService;
};

RemObjects.DataAbstract.RemoteDataAdapter.prototype.getLoginService = function getLoginService() {
    return this.fLoginService;
};


RemObjects.DataAbstract.RemoteDataAdapter.prototype.intOnLoginNeeded = function intOnLoginNeeded(aCallback) {
    if (this.onLoginNeeded) {
        this.onLoginNeeded(aCallback)
    } else {
        this.fSavedOnLoginNeeded(aCallback);
    };
};


RemObjects.DataAbstract.RemoteDataAdapter.prototype.onLoginNeeded = function onLoginNeeded(aCallback) {
    RemObjects.UTIL.showMessage("Default onLoginNeeded handler: assign remoteDataAdapter.onLoginNeeded and call aCallback there after successful login");
    //example:
    //    this.login("John", "password", function(result) {
    //        if (aCallback)
    //            aCallback();
    //    }, function(msg, e) {
    //        if (e)
    //            throw e
    //        else
    //            throw new Error(msg.getErrorMessage());
    //    });
};


RemObjects.DataAbstract.RemoteDataAdapter.prototype.login = function login(aUserID, aPassword, aConnectionName, onSuccessFunction, onErrorFunction) {

    if (!this.fLoginService)
        throw new Error("RemoteDataAdapter.login: login service not assigned");

    var svc;
    switch (arguments.length - 2) {
        case 1: //loginString
            svc = new RemObjects.DataAbstract.Server.BaseLoginService(this.fLoginService);
            svc.LoginEx(arguments[0], arguments[1], arguments[2]);
            break;
        case 2: //userID, password
            svc = new RemObjects.DataAbstract.Server.SimpleLoginService(this.fLoginService);
            svc.Login(arguments[0], arguments[1], arguments[2], arguments[3]);
            break;
        case 3: //userID, password, connection
            svc = new RemObjects.DataAbstract.Server.MultiDbLoginService(this.fLoginService);
            svc.Login(arguments[0], arguments[1], arguments[2], arguments[3], arguments[4]);
            break;
        default:
            throw new Error("RemoteDataAdapter.login: incorrect number of arguments");
    };

};

RemObjects.DataAbstract.RemoteDataAdapter.prototype.logout = function logout(onSuccessFunction, onErrorFunction) {
    if (!this.fLoginService)
        throw new Error("RemoteDataAdapter.logout: login service not assigned");

    var svc = new RemObjects.DataAbstract.Server.BaseLoginService(this.fLoginService);
    svc.Logout(onSuccessFunction, onErrorFunction);
};


RemObjects.DataAbstract.RemoteDataAdapter.prototype.getSchema = function getSchema(aFilter, aCallback, onFailure) {
    var that = this;
    this.fService.GetSchema(aFilter, aCallback, onFailure || RemObjects.UTIL.showError);
};


RemObjects.DataAbstract.RemoteDataAdapter.prototype.createStreamer = function createStreamer() {
    return new this.fStreamerClass();
};

RemObjects.DataAbstract.RemoteDataAdapter.prototype.setSendReducedDelta = function setSendReducedDelta(aValue) {
    this.fSendReducedDelta = aValue;
};

RemObjects.DataAbstract.RemoteDataAdapter.prototype.buildDelta = function buildDelta(aTable) {

    function processRows(aRows, aDelta) {
        for (var i = 0; i < aRows.length; i++) {

            var c = new RemObjects.DataAbstract.Change();
            c.status = "pending";
            c.recid = aRows[i].recId;
            if (aRows[i].state == RemObjects.DataAbstract.Enum.RowStates.rsNew) {
                c.old = aRows[i].__newValues.slice();
            } else {
                c.old = aRows[i].__oldValues.slice();
            };
            c["new"] = aRows[i].__newValues.slice();
            excludeItems(c.old);
            excludeItems(c["new"]);

            switch (aRows[i].state) {
                case RemObjects.DataAbstract.Enum.RowStates.rsModified: c.changetype = "update"; break;
                case RemObjects.DataAbstract.Enum.RowStates.rsNew: c.changetype = "insert"; break;
                case RemObjects.DataAbstract.Enum.RowStates.rsDeleted: c.changetype = "delete"; break;
            };

            aDelta.data.push(c);
        };
    };

    function excludeItems(anArray) {
        for (var i = excludedFields.length - 1; i >= 0; i--) {
            anArray.splice(excludedFields[i], 1);
        };
    };

    var excludedFields = [];
    for (var i = 0; i < aTable.fields.length; i++) {
        if (!aTable.fields[i].logChanges) {
            excludedFields.push(i);
        };
    };
    var changedRows = [];
    for (var i = 0; i < aTable.rows.length; i++) {
        if (aTable.rows[i].state != RemObjects.DataAbstract.Enum.RowStates.rsUnchanged) changedRows.push(aTable.rows[i]);
    };

    if ((changedRows.length > 0) || (aTable.deletedRows.length > 0)) {

        d = new RemObjects.DataAbstract.Delta();
        d.name = aTable.name;
        d.keyfields = aTable.keyfields.slice();
        d.loggedfields = aTable.fields.slice();
        excludeItems(d.loggedfields);

        processRows(changedRows, d);
        processRows(aTable.deletedRows, d);
        return d;
    } else {
        return null;
    };
};

RemObjects.DataAbstract.RemoteDataAdapter.prototype.createTableFromSchema = function createTableFromSchema(aTableName, aTable, aCallback) {
    var that = this;
    var tablesArray = new RemObjects.DataAbstract.Server.StringArray();
    tablesArray.items = [aTableName];
    this.fService.GetTableSchema(tablesArray, function (result) {
        var xmlDoc;
        if (typeof (DOMParser) != 'undefined') {
            var parser = new DOMParser();
            xmlDoc = parser.parseFromString(result, "text/xml");
        } else // Internet Explorer
        {
            xmlDoc = new ActiveXObject("Microsoft.XMLDOM");
            xmlDoc.async = "false";
            xmlDoc.loadXML(result);
        };

        var fieldsNode = xmlDoc.getElementsByTagName("Fields")[0];
        for (i = 0; i < fieldsNode.childElementCount; i++) {
            var f = new RemObjects.DataAbstract.Field();
            f.name = fieldsNode.childNodes[i].getElementsByTagName('Name')[0].textContent;
            f.type = fieldsNode.childNodes[i].getElementsByTagName('DataType')[0].textContent;
            f.logChanges = (fieldsNode.childNodes[i].getElementsByTagName('LogChanges')[0].textContent == "True");
            if (fieldsNode.childNodes[i].getElementsByTagName('InPrimaryKey')[0].textContent == "True")
                aTable.keyfields.push(f.name);

            aTable.fields.push(f);

        };
        if (aCallback)
            aCallback();
    }, RemObjects.UTIL.showError);
};


RemObjects.DataAbstract.RemoteDataAdapter.prototype.executeCommand = function executeCommand(aName, aParameters, onSuccess, onError) {
    if (aParameters.constructor != Array) {
        aParameters = [aParameters];
    };
    var params = new RemObjects.DataAbstract.Server.DataParameterArray();
    params.items = aParameters;
    this.fService.ExecuteCommand(aName, params, onSuccess, onError)
};

RemObjects.DataAbstract.RemoteDataAdapter.prototype.getSQLData = function getSQLData(aTable, aSQL, onSuccess, onError) {
    var ria = new RemObjects.DataAbstract.Server.TableRequestInfoArray();
    ria.elementType = "TableRequestInfoV6";

    if (aTable.constructor != Array) {
        aTable = [aTable];
    };

    switch (aSQL.constructor) {
        case RemObjects.DataAbstract.Server.TableRequestInfoV6:
            ria.items.push(aSQL);
            break;
        case Array:
            for (var i = 0; i < aTable.length; i++) {
                switch (aSQL[i].constructor) {
                    case RemObjects.DataAbstract.Server.TableRequestInfoV6:
                        ria.items.push(aSQL[i]);
                        break;
                    case String:
                        ria.items.push(RemObjects.DataAbstract.Util.createRequestInfoV6(aSQL[i]));
                        break;
                    default:
                        throw new Error("rda.getSQLData: incorrect aRequestInfo array item");
                };
            };
            break;
        case String:
            ria.items.push(RemObjects.DataAbstract.Util.createRequestInfoV6(aSQL));
            break;
        default:
            throw new Error("rda.getSQLData: incorrect aSQL");
    };

    this.getData(aTable, ria, onSuccess, onError);
};

RemObjects.DataAbstract.RemoteDataAdapter.prototype.getAutoGetScripts = function getAutoGetScripts() {
    return this.fAutoGetScripts;
};

RemObjects.DataAbstract.RemoteDataAdapter.prototype.setAutoGetScripts = function setAutoGetScripts(aValue) {
    this.fAutoGetScripts = aValue;
};

RemObjects.DataAbstract.RemoteDataAdapter.prototype.intGetScripts = function intGetScripts(aTable, onSuccess, onError) {
    var tableNames = [];
    if (aTable.constructor != Array) {
        aTable = [aTable];
    };
    for (var i = 0; i < aTable.length; tableNames.push(aTable[i++].name));
    this.fService.GetDatasetScripts(tableNames.join(","), function (aScript) {
        for (var t = 0; t < aTable.length; t++) {
            var __tbl = aTable[t];
            __tbl.__onNewRow = null;
            __tbl.__beforeDelete = null;
            __tbl.__beforePost = null;
            var r = new RegExp('<' + aTable[t].name + ' Language="(.*?)"><!\\[CDATA\\[([\\s\\S]*?)\\]\\]', "mi");
            var m = r.exec(aScript);
            var s = (m ? m[2] : "");
            var m1 = s.match(/function (.*?)\(/g);
            if (m1) for (var i = 0; i < m1.length; i++) {
                s = s.replace(m1[i], m1[i].replace(/function (.*?)\(/, "__tbl.__$1 = ") + m1[i]);
            }
            var m1 = s.match(/function (.*?)\{/g);
            if (m1) for (var i = 0; i < m1.length; i++) {
                s = s.replace(m1[i], m1[i] + '\nfor (var p in RemObjects.DataAbstract.Scripting) {\n' +
                    'eval("var " + p + " = RemObjects.DataAbstract.Scripting." + p);\n' +
                    '};\n');
            }
            eval(s);
        };
        if (onSuccess) onSuccess();
    }, onError);
};


RemObjects.DataAbstract.RemoteDataAdapter.prototype.getData = function getData(aTable, aRequestInfo, onSuccess, onError) {
    var tableNames = new RemObjects.DataAbstract.Server.StringArray();
    if (aTable.constructor != Array) {
        aTable = [aTable];
    };
    for (var i = 0; i < aTable.length; tableNames.items.push(aTable[i++].name));

    var tableOptions = new RemObjects.DataAbstract.Server.TableRequestInfoArray();

    switch (aRequestInfo.constructor) {
        case RemObjects.DataAbstract.Server.TableRequestInfoArray:
            tableOptions = aRequestInfo;
            break;
        case RemObjects.DataAbstract.Server.TableRequestInfo:
        case RemObjects.DataAbstract.Server.TableRequestInfoV5:
        case RemObjects.DataAbstract.Server.TableRequestInfoV6:
            tableOptions.items.push(aRequestInfo);
            break;
        case Array:
            tableOptions.items = aRequestInfo;
            break;
        default:
            var requestInfo = RemObjects.DataAbstract.Util.createRequestInfo();
            for (var i = 0; i < aTable.length; i++) {
                tableOptions.items.push(requestInfo);
            };
    };

    for (var i = 0; i < aTable.length; i++) {
        if (aTable[i].dynamicWhere) {
            var requestInfoV5 = RemObjects.DataAbstract.Util.createRequestInfoV5(tableOptions.items[i].IncludeSchema.value,
                tableOptions.items[i].MaxRecords.value, tableOptions.items[i].UserFilter.value, tableOptions.items[i].Parameters.value.items);
            requestInfoV5.WhereClause.value = aTable[i].dynamicWhere.toXML();
            tableOptions.items[i] = requestInfoV5;
        };
    };


    var __onError, __onSuccess;
    if (arguments.length < 4) {
        __onError = arguments[arguments.length - 1];
        __onSuccess = arguments[arguments.length - 2];
    } else {
        __onError = onError;
        __onSuccess = onSuccess;
    };

    var that = this;
    this.fService.GetData(tableNames, tableOptions,
        function (result) {
            var streamer = that.createStreamer();
            var tmp = (that.fService.fMessage instanceof RemObjects.SDK.JSONMessage) ? RemObjects.UTIL.fromBase64(result) : result;
            streamer.setStream(tmp);
            streamer.initializeRead();

            for (var i = 0; i < aTable.length; i++) {
                streamer.readDataset(aTable[i]);
            };

            if (that.fAutoGetScripts) {
                that.intGetScripts(aTable, __onSuccess, __onError);
            } else if (__onSuccess) __onSuccess();
        }, __onError);

};

RemObjects.DataAbstract.RemoteDataAdapter.prototype.onChangeFail = function onChangeFail(aData) {
    RemObjects.UTIL.showMessage("Update failed for:\n" + RemObjects.UTIL.toJSON(aData));
};

RemObjects.DataAbstract.RemoteDataAdapter.prototype.applyUpdates = function applyUpdates(aTable, onSuccess, onError) {
    if (aTable.constructor != Array) {
        aTable = [aTable];
    };
    var __deltas = new RemObjects.DataAbstract.Deltas();
    for (var i = 0; i < aTable.length; i++) {
        aTable[i].post();
        var d = this.buildDelta(aTable[i]);
        if (d) __deltas.deltas.push(d);
    };
    if (__deltas.deltas.length > 0) {
        var that = this;
        var streamer = this.createStreamer();
        streamer.initializeWrite();
        streamer.writeDelta(__deltas, this.fSendReducedDelta);
        streamer.finalizeWrite();

        var tmp = streamer.getStream();

        this.fService.UpdateData(tmp, function (result) {
            var tmp = (that.fService.fMessage instanceof RemObjects.SDK.JSONMessage) ? RemObjects.UTIL.fromBase64(result) : result;

            var streamer = that.createStreamer();
            streamer.setStream(tmp);
            streamer.initializeRead();
            var __res = streamer.readDelta();

            for (var i = 0; i < __deltas.deltas.length; i++) {
                var processedTable = null;
                for (var t = 0; t < aTable.length; t++) {
                    if (aTable[t].name == __deltas.deltas[i].name) {
                        processedTable = aTable[t];
                        break;
                    };
                };

                var baseDelta = __deltas.deltas[i];
                var processedDelta = __res.findByName(__deltas.deltas[i].name);
                if (processedDelta) {
                    for (var j = 0; j < processedDelta.data.length; j++) {

                        if (processedTable) {
                            switch (processedDelta.data[j].status) {
                                case "failed":
                                    that.onChangeFail(processedDelta.data[j]);
                                    //remove from baseDelta
                                    baseDelta.data.splice(baseDelta.intFindId(processedDelta.data[j].recid), 1);
                                    break;
                                case "resolved":
                                    if (processedTable.findId(processedDelta.data[j].recid)) {
                                        for (var k = 0; k < processedDelta.loggedfields.length; k++) {
                                            if (processedDelta.data[j]["new"][k] != processedDelta.data[j]["old"][k]) {
                                                var fieldNum = processedTable.fieldNumByName(processedDelta.loggedfields[k].name);
                                                processedTable.currentRow().__newValues[fieldNum] = processedDelta.data[j]["new"][k];
                                                processedTable.currentRow().__oldValues[fieldNum] = processedDelta.data[j]["new"][k];
                                            };
                                        };
                                        processedTable.currentRow().state = RemObjects.DataAbstract.Enum.RowStates.rsUnchanged;
                                        processedTable.currentRow().__oldValues = processedTable.currentRow().__newValues.slice();
                                    } else {
                                        var deleted = processedTable.intFindDeleted(processedDelta.data[j].recid);
                                        if (deleted != null) {
                                            processedTable.deletedRows.splice(deleted, 1);
                                        } else {
                                            RemObjects.UTIL.showMessage("Incorrect recid in resolved change:\n" + RemObjects.UTIL.toJSON(processedDelta.data[j]));
                                        };
                                    };
                                    break;
                            };
                        } else {
                            RemObjects.UTIL.showMessage("Incorrect table name in result delta:\n" + RemObjects.UTIL.toJSON(processedDelta));
                        };
                    };


                };

                for (var j = 0; j < baseDelta.data.length; j++) {
                    if (processedTable.findId(baseDelta.data[j].recid)) {
                        processedTable.currentRow().state = RemObjects.DataAbstract.Enum.RowStates.rsUnchanged;
                        processedTable.currentRow().__oldValues = [];
                    };
                    var deleted = processedTable.intFindDeleted(baseDelta.data[j].recid);
                    if (deleted != null) {
                        processedTable.deletedRows.splice(deleted, 1);
                    };

                };
            };

            if (onSuccess) onSuccess(__res);
        }, onError);
    };
};


RemObjects.DataAbstract.JSONDataStreamer.prototype.setStream = function setStream(aStream) {
    this.fStream = aStream;
};

RemObjects.DataAbstract.JSONDataStreamer.prototype.getStream = function getStream() {
    return this.fStream;
};


RemObjects.DataAbstract.JSONDataStreamer.prototype.initializeRead = function () {
};

RemObjects.DataAbstract.JSONDataStreamer.prototype.initializeWrite = function () {
};

RemObjects.DataAbstract.JSONDataStreamer.prototype.finalizeWrite = function () {
};

RemObjects.DataAbstract.JSONDataStreamer.prototype.readDataset = function readDataset(dataset) {
    dataset.rows = [];
    dataset.fNextRecId = 0;
    var rows = null;
    var datasets = RemObjects.UTIL.parseJSON(this.fStream).datasets;
    for (var i = 0; i < datasets.length; i++) {
        if (datasets[i].schema) {
            dataset.fields.length = 0;
            dataset.keyfields.length = 0;
            for (var j = 0; j < datasets[i].schema.fields.length; j++) {
                var schemaField = datasets[i].schema.fields[j];
                var datasetField = new RemObjects.DataAbstract.Field();
                datasetField.name = schemaField.Name;
                datasetField.type = schemaField.DataType;
                datasetField.logChanges = schemaField.LogChanges;

                if (schemaField.InPrimaryKey == true) {
                    datasetField.inPrimaryKey = true;
                    dataset.keyfields.push(schemaField.Name);
                };

                dataset.fields.push(datasetField);
            };
        };
        if (datasets[i].name.toUpperCase() == dataset.name.toUpperCase()) {
            rows = datasets[i].data.rows;
            break;
        };
    };

    if (!rows) throw new Error("readDataset: dataset not found - " + dataset.name);

    for (var i = 0; i < rows.length; i++) {
        var r = dataset.intAppendRow();
        r.state = RemObjects.DataAbstract.Enum.RowStates.rsUnchanged;
        r.__newValues = rows[i];
    };
};

RemObjects.DataAbstract.JSONDataStreamer.prototype.writeDelta = function writeDelta(aDelta) {
    this.fStream = RemObjects.UTIL.toJSON(aDelta);
};

RemObjects.DataAbstract.JSONDataStreamer.prototype.readDelta = function readDelta() {
    var result = new RemObjects.DataAbstract.Deltas();
    var remoteDelta = RemObjects.UTIL.parseJSON(this.fStream);

    if (remoteDelta.deltas) {
        for (var i = 0; i < remoteDelta.deltas.length; i++) {
            result.deltas.push(remoteDelta.deltas[i]);
        };
    };

    return result;
};


RemObjects.DataAbstract.Bin2DataStreamer.prototype.setStream = function setStream(aStream) {
    this.fStream = aStream;
};

RemObjects.DataAbstract.Bin2DataStreamer.prototype.getStream = function getStream() {
    return this.fStream;
};

RemObjects.DataAbstract.Bin2DataStreamer.prototype.initializeRead = function initializeRead() {
    if (this.fStream.substr(0, 8) != "DABIN200")
        throw new Error("Bin2DataStreamer.initializeRead: incorrect stream header");
    this.fStreamPos = 8;
    this.fStreamInfoPos = this.readInteger();
};

RemObjects.DataAbstract.Bin2DataStreamer.prototype.initializeWrite = function initializeWrite() {
};

RemObjects.DataAbstract.Bin2DataStreamer.prototype.finalizeWrite = function finalizeWrite() {
    this.fStream = "DABIN200" + this.fParser.encodeInt(this.fStreamInfoPos + 12, 32, false) + this.fStream;
};

RemObjects.DataAbstract.Bin2DataStreamer.prototype.readByte = function readByte() {
    var result = this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, 1), 8, false);
    this.fStreamPos += 1;
    return result;
};

RemObjects.DataAbstract.Bin2DataStreamer.prototype.readInteger = function readInteger() {
    var result = this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, 4), 32, false);
    this.fStreamPos += 4;
    return result;
};

RemObjects.DataAbstract.Bin2DataStreamer.prototype.readAnsiStringWithLength = function readAnsiStringWithLength() {
    var len = this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, 4), 32, false);
    this.fStreamPos += 4;
    var result = this.fStream.substr(this.fStreamPos, len);
    this.fStreamPos += len;
    return result;
};


RemObjects.DataAbstract.Bin2DataStreamer.prototype.readUtf8StringWithLength = function readUtf8StringWithLength() {
    var len = this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, 4), 32, true);
    this.fStreamPos += 4;
    if (len == -1) return null;
    var result = RemObjects.UTIL.byteArrayToStr(this.fStream.substr(this.fStreamPos, len));
    this.fStreamPos += len;
    return result;
};


RemObjects.DataAbstract.Bin2DataStreamer.prototype.read = function read(aType) {
    var value;
    switch (aType) {
        case "datFixedChar"://ok
        case "datString"://ok
        case "datMemo"://ok
            var len = this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4;
            value = this.fStream.substr(this.fStreamPos, len);
            this.fStreamPos += len;
            break;

        case "datBlob":
        case "datCursor":
            var len = this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4;

            value = this.fStream.substr(this.fStreamPos, len);
            this.fStreamPos += len;
            break;

        /*            value = "";
                    for (var i = this.fStreamPos; i < this.fStreamPos + len; i++) {
                        value += String.fromCharCode(this.fStream.charCodeAt(i) & 0xFF);
                    };
                    this.fStreamPos += len;
                    break;
        */
        case "datWideString"://ok
        case "datFixedWideChar"://ok
        case "datWideMemo"://ok
        case "datXml"://ok
            var len = this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4;
            value = RemObjects.UTIL.byteArrayToUtf16(this.fStream.substr(this.fStreamPos, len));
            this.fStreamPos += len;
            break;


        case "datAutoInc"://ok
        case "datInteger"://ok
            value = this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, 4), 32, true);
            this.fStreamPos += 4;
            break;

        case "datSmallInt"://ok
            value = this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, 2), 16, true);
            this.fStreamPos += 2;
            break;

        case "datDateTime"://ok
            var utcValue = this.fParser.decodeFloat(this.fStream.substr(this.fStreamPos, 8), 52, 11);
            utcValue = new Date(Math.round((utcValue - 25569.0) * 86400000));
            value = new Date(utcValue.getUTCFullYear(), utcValue.getUTCMonth(), utcValue.getUTCDate(), utcValue.getUTCHours(), utcValue.getUTCMinutes(), utcValue.getUTCSeconds());
            //value = this.fParser.decodeFloat(this.fStream.substr(this.fStreamPos, 8), 52, 11);
            //value = new Date(Math.round((value - 25569.0) * 86400000));
            this.fStreamPos += 8;
            break;

        case "datCurrency"://ok
            value = this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, 6), 48, true) / 10000;
            this.fStreamPos += 8;
            break;
        case "datFloat"://ok
            value = this.fParser.decodeFloat(this.fStream.substr(this.fStreamPos, 8), 52, 11);
            this.fStreamPos += 8;
            break;

        case "datSingleFloat": //ok
            value = this.fParser.decodeFloat(this.fStream.substr(this.fStreamPos, 4), 23, 8);
            this.fStreamPos += 4;
            break;

        case "datBoolean"://ok
            value = !(this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, 1), 8, false) == 0);
            this.fStreamPos += 1;
            break;

        case "datByte"://ok
            value = this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, 1), 8, false);
            this.fStreamPos += 1;
            break;

        case "datLargeInt"://ok
        case "datLargeAutoInc"://ok
            value = this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, 6), 48, true);
            this.fStreamPos += 8;
            break;

        case "datLargeUInt"://ok
            value = this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, 6), 48, false);
            this.fStreamPos += 8;
            break;

        case "datShortInt"://ok
            value = this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, 1), 8, true);
            this.fStreamPos += 1;
            break;

        case "datWord"://ok
            value = this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, 2), 16, false);
            this.fStreamPos += 2;
            break;

        case "datDecimal"://ok
            var decimal = [];
            for (var i = 0; i < 6; i++) {
                decimal[i] = this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, 2), 16, false);
                this.fStreamPos += 2;
            };
            decimal[6] = this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4;
            value = parseFloat(RemObjects.UTIL.decimalToString(decimal));
            break;

        case "datGuid"://ok
            value = "{" + this.fStream.substr(this.fStreamPos, 36) + "}";
            this.fStreamPos += 36;
            break;

        case "datCardinal"://ok
            value = this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4;
            break;

        /*
            datBlob: BlobToStreamAsStr(Stream, Value);
        * */

        default:
            throw new Error("Bin2DataStreamer.read: unknown type " + aType);
    };
    return value;
};



RemObjects.DataAbstract.Bin2DataStreamer.prototype.readParam = function readParam(aPropCount) {

    var result = {};
    for (var i = 0; i < aPropCount; i++) {
        result[this.readAnsiStringWithLength()] = this.readUtf8StringWithLength();
    };

    return result;
};


RemObjects.DataAbstract.Bin2DataStreamer.prototype.readField = function readField(aPropCount) {

    function camelize(str) {
        return str.substr(0, 1).toLowerCase() + str.substr(1);
    };

    var result = new RemObjects.DataAbstract.Field();

    try {
        for (var i = 0; i < aPropCount; i++) {
            result[camelize(this.readAnsiStringWithLength())] = this.readUtf8StringWithLength();
        };

        result.type = result.dataType;
        if (result.type == "datAutoInc" || result.type == "datLargeAutoInc") {
            result.fAutoIncSequence = -1;
        };
        result.logChanges = (result.logChanges == "True");
        return result;
    } catch (e) {
        throw new Error("Bin2DataStreamer.readField: " + e.message);
    };
};



RemObjects.DataAbstract.Bin2DataStreamer.prototype.readDataset = function readDataset(aDataset) {
    if (this.readByte() != 0) {//schema present
        this.readInteger(); //skip schema end position
        var fieldCount = this.readInteger();
        var propCount = this.readInteger();
        aDataset.fields = [];
        aDataset.keyfields = [];
        for (var i = 0; i < fieldCount; i++) {
            var f = this.readField(propCount);
            aDataset.fields.push(f);
            if (f.inPrimaryKey.toLowerCase() == "true") {
                aDataset.keyfields.push(f.name);
            };
        };

        var paramCount = this.readInteger();
        propCount = this.readInteger();
        for (var i = 0; i < paramCount; i++) {
            this.readParam(propCount);
        };
    } else {
        this.readInteger(); //skip schema end position
    };
    var rowCount = this.readInteger();
    var fieldCount = this.readInteger();
    aDataset.rows = [];
    aDataset.fNextRecId = 0;

    for (var i = 0; i < fieldCount; i++) {
        this.readAnsiStringWithLength(); //skip field name
        this.readByte(); //skip field type
        this.readInteger(); //skip field size
    };

    var bitMaskSize = Math.floor((fieldCount + 7) / 8);
    for (var i = 0; i < rowCount; i++) {
        var r = aDataset.intAppendRow();
        r.state = RemObjects.DataAbstract.Enum.RowStates.rsUnchanged;

        var bitMask = "";
        for (var b = 0; b < bitMaskSize; b++) {
            bitMask += RemObjects.UTIL.zeroPad(this.fParser.decodeInt(this.fStream.substr(this.fStreamPos + bitMaskSize - 1 - b, 1), 8, false).toString(2), 8);
        };
        var bitMaskArray = bitMask.split("").reverse();

        this.fStreamPos += bitMaskSize;

        for (var j = 0; j < fieldCount; j++) {
            r.__newValues[j] = (bitMaskArray[j] == '1') ? null : this.read(aDataset.fields[j].type);
        };
    };


};

RemObjects.DataAbstract.Bin2DataStreamer.prototype.writeInteger = function writeInteger(aValue) {
    this.write("datInteger", aValue);
};

RemObjects.DataAbstract.Bin2DataStreamer.prototype.writeByte = function writeByte(aValue) {
    this.fStream += this.fParser.encodeInt(aValue, 8, true);
};


RemObjects.DataAbstract.Bin2DataStreamer.prototype.writeAnsiStringWithLength = function writeAnsiStringWithLength(aValue) {
    this.fStream += this.fParser.encodeInt(aValue.length, 32, false) + aValue;
};


RemObjects.DataAbstract.Bin2DataStreamer.prototype.write = function write(aType, aValue) {
    switch (aType) {
        case "datFixedChar"://ok
        case "datString"://ok
        case "datMemo"://ok
        case "datBlob":
        case "datCursor":
            this.writeAnsiStringWithLength(aValue);
            break;

        case "datWideString"://ok
        case "datFixedWideChar"://ok
        case "datWideMemo"://ok
        case "datXml"://ok
            this.fStream += this.fParser.encodeInt(aValue.length * 2, 32, true);
            this.fStream += RemObjects.UTIL.utf16ToByteArray(aValue);
            break;

        case "datAutoInc"://ok
        case "datInteger"://ok
            this.fStream += this.fParser.encodeInt(aValue, 32, true);
            break;

        case "datSmallInt"://ok
            this.fStream += this.fParser.encodeInt(aValue, 16, true);
            break;

        case "datDateTime"://todo:test
            if (!(aValue instanceof Date)) {
                throw new Error("Not a Date value: " + aValue);
            };
            this.fStream += this.fParser.encodeFloat((aValue - aValue.getTimezoneOffset() * 60000) / 86400000 + 25569.0, 52, 11);
            break;

        case "datCurrency":
            var cur = this.fParser.encodeInt(aValue * 10000, 48, true);
            this.fStream += cur;
            if ((cur.charCodeAt(cur.length - 1) == 0) || (cur.charCodeAt(cur.length - 1) == 0xFF)) {
                this.fStream += cur.substr(cur.length - 1, 1) + cur.substr(cur.length - 1, 1);
            };
            break;

        case "datFloat"://ok
            this.fStream += this.fParser.encodeFloat(aValue, 52, 11);
            break;

        case "datSingleFloat": //ok
            this.fStream += this.fParser.encodeFloat(aValue, 23, 8);
            break;

        case "datBoolean"://ok
            this.fStream += this.fParser.encodeInt(aValue ? 1 : 0, 8, false);
            break;

        case "datByte"://ok
            this.fStream += this.fParser.encodeInt(aValue, 8, false);
            break;

        case "datLargeInt"://ok
        case "datLargeAutoInc"://ok
            var large = this.fParser.encodeInt(aValue, 48, true);
            this.fStream += large;
            if ((large.charCodeAt(large.length - 1) == 0) || (large.charCodeAt(large.length - 1) == 0xFF)) {
                this.fStream += large.substr(large.length - 1, 1) + large.substr(large.length - 1, 1);
            };
            break;

        case "datLargeUInt"://ok
            var large = this.fParser.encodeInt(aValue, 48, false);
            this.fStream += large;
            if ((large.charCodeAt(large.length - 1) == 0) || (large.charCodeAt(large.length - 1) == 0xFF)) {
                this.fStream += large.substr(large.length - 1, 1) + large.substr(large.length - 1, 1);
            };
            break;

        case "datShortInt"://ok
            this.fStream += this.fParser.encodeInt(aValue, 8, true);
            break;

        case "datWord"://ok
            this.fStream += this.fParser.encodeInt(aValue, 16, false);
            break;

        case "datDecimal"://ok
            var decimal = RemObjects.UTIL.stringToDecimal(aValue.toString());
            for (var i = 0; i < 6; i++) {
                this.fStream += this.fParser.encodeInt(decimal[i], 16, false);
            };
            this.fStream += this.fParser.encodeInt(decimal[6], 32, false);
            break;

        case "datGuid"://ok
            this.fStream += aValue.substr(1, 36);
            break;

        case "datCardinal"://ok
            this.fStream += this.fParser.encodeInt(aValue, 32, false);
            break;

        /*
            datBlob: BlobToStreamAsStr(Stream, Value);
        * */

        default:
            throw new Error("Bin2DataStreamer.write: unknown type " + aType);
    };
};



RemObjects.DataAbstract.Bin2DataStreamer.prototype.writeDelta = function writeDelta(aDeltas, aSendReducedDelta) {
    function indexOf(anArray, aValue) {
        for (var i = 0; i < anArray.length; i++) {
            if (anArray[i] == aValue) return i;
        };
        return -1;
    };

    var offsets = [];
    for (var d = 0; d < aDeltas.deltas.length; d++) {
        offsets.push(this.fStream.length);
        var aDelta = aDeltas.deltas[d];

        this.writeInteger(aDelta.data.length); //changes count

        this.writeInteger(aDelta.loggedfields.length); //fields count
        for (var i = 0; i < aDelta.loggedfields.length; i++) {
            this.writeAnsiStringWithLength(RemObjects.UTIL.strToByteArray(aDelta.loggedfields[i].name));
            this.writeByte(indexOf(RemObjects.DataAbstract.DADataType, aDelta.loggedfields[i].type));
        };

        this.writeInteger(aDelta.keyfields.length); //key fields count
        for (var i = 0; i < aDelta.keyfields.length; i++) {
            this.writeAnsiStringWithLength(RemObjects.UTIL.strToByteArray(aDelta.keyfields[i]));
        };

        this.writeByte(aSendReducedDelta ? 1 : 0); //hasReducedDelta

        this.writeInteger(aDelta.data.length); //changes count

        var bitMaskSize = Math.floor((aDelta.loggedfields.length + 7) / 8);

        var bitMaskOld;
        var bitMaskNew;

        for (var i = 0; i < aDelta.data.length; i++) {
            bitMaskOld = new Array(bitMaskSize);
            bitMaskNew = new Array(bitMaskSize);

            for (var j = 0; j < aDelta.loggedfields.length; j++) {
                bitMaskNew[j] = (aDelta.data[i]["new"][j] == null || typeof (aDelta.data[i]["new"][j]) == 'undefined') ? "0" : "1";
                bitMaskOld[j] = (aDelta.data[i].old[j] == null || typeof (aDelta.data[i].old[j]) == 'undefined') ? "0" : "1";
            };


            this.writeInteger(indexOf(RemObjects.DataAbstract.Enum.ChangeTypeNames, aDelta.data[i].changetype));
            this.writeInteger(aDelta.data[i].recid);
            this.writeInteger(RemObjects.DataAbstract.Enum.ChangeStatus.csPending);
            this.writeAnsiStringWithLength("");

            //            this.fStream += this.fParser.encodeInt(parseInt(bitMaskOld.reverse().join(""), 2), bitMaskSize * 8, false);
            //            bitMaskOld.reverse();
            for (var b = 0; b < bitMaskSize; b++) {
                this.fStream += this.fParser.encodeInt(parseInt(bitMaskOld.slice(b * 8, (b + 1) * 8).reverse().join(""), 2), 8, false);
            };

            for (var j = 0; j < aDelta.loggedfields.length; j++) {
                if (bitMaskOld[j] == "1") {
                    this.write(aDelta.loggedfields[j].type, aDelta.data[i].old[j]);
                };
            };

            //            this.fStream += this.fParser.encodeInt(parseInt(bitMaskNew.reverse().join(""), 2), bitMaskSize * 8, false);
            //            bitMaskNew.reverse();
            for (var b = 0; b < bitMaskSize; b++) {
                this.fStream += this.fParser.encodeInt(parseInt(bitMaskNew.slice(b * 8, (b + 1) * 8).reverse().join(""), 2), 8, false);
            };

            for (var j = 0; j < aDelta.loggedfields.length; j++) {
                if (bitMaskNew[j] == "1") {
                    this.write(aDelta.loggedfields[j].type, aDelta.data[i]["new"][j]);
                };
            };
        };
    };

    this.fStreamInfoPos = this.fStream.length; //will be used in finalizeWrite

    this.writeInteger(aDeltas.deltas.length); //delta count

    for (var i = 0; i < aDeltas.deltas.length; i++) {
        this.writeInteger(1); //etDelta
        this.writeAnsiStringWithLength(RemObjects.UTIL.strToByteArray(aDeltas.deltas[i].name));
        this.writeInteger(offsets[i] + 12); //first delta position
    };

};

RemObjects.DataAbstract.Bin2DataStreamer.prototype.readDelta = function readDelta() {

    var savedPos = this.fStreamPos;
    this.fStreamPos = this.fStreamInfoPos;
    var deltaCount = this.readInteger();
    var result = new RemObjects.DataAbstract.Deltas();

    for (var d = 0; d < deltaCount; d++) {
        var delta = new RemObjects.DataAbstract.Delta();

        var deltaType = this.readInteger();
        delta.name = RemObjects.UTIL.byteArrayToStr(this.readAnsiStringWithLength());
        var deltaPos = this.readInteger();

        result.deltas.push(delta);

    };

    this.fStreamPos = savedPos;

    for (var d = 0; d < deltaCount; d++) {

        delta = result.deltas[d];

        var changesCount = this.readInteger();
        if (changesCount != 0) {
            var fieldsCount = this.readInteger();

            for (var i = 0; i < fieldsCount; i++) {
                delta.loggedfields[i] = {};
                delta.loggedfields[i].name = RemObjects.UTIL.byteArrayToStr(this.readAnsiStringWithLength());
                delta.loggedfields[i].type = RemObjects.DataAbstract.DADataType[this.readByte()];
            };

            var keyFieldsCount = this.readInteger();
            for (var i = 0; i < keyFieldsCount; i++) {
                delta.keyfields[i] = RemObjects.UTIL.byteArrayToStr(this.readAnsiStringWithLength());
            };


            if (changesCount != 0) {

                this.readByte(); //reduced?
                changesCount = this.readInteger();

                var bitMaskSize = Math.floor((fieldsCount + 7) / 8);

                //                var bitMaskOld;
                //                var bitMaskNew;

                for (var i = 0; i < changesCount; i++) { //read changes

                    var change = new RemObjects.DataAbstract.Change();
                    change.changetype = RemObjects.DataAbstract.Enum.ChangeTypeNames[this.readInteger()];
                    change.recid = this.readInteger();
                    change.status = RemObjects.DataAbstract.Enum.ChangeStatusNames[this.readInteger()];
                    change.message = RemObjects.UTIL.byteArrayToStr(this.readAnsiStringWithLength());

                    //                    bitMaskOld = this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, bitMaskSize), bitMaskSize * 8);
                    //                    this.fStreamPos += bitMaskSize;
                    //                    bitMaskOld = bitMaskOld.toString(2).split("").reverse();

                    var bitMaskOld = "";
                    for (var b = 0; b < bitMaskSize; b++) {
                        bitMaskOld += RemObjects.UTIL.zeroPad(this.fParser.decodeInt(this.fStream.substr(this.fStreamPos + bitMaskSize - 1 - b, 1), 8, false).toString(2), 8);
                    };
                    var bitMaskOld = bitMaskOld.split("").reverse();
                    this.fStreamPos += bitMaskSize;

                    for (var j = 0; j < fieldsCount; j++) {
                        change.old[j] = (bitMaskOld[j] == "1") ? this.read(delta.loggedfields[j].type) : null;
                    };

                    //                    bitMaskNew = this.fParser.decodeInt(this.fStream.substr(this.fStreamPos, bitMaskSize), bitMaskSize * 8);
                    //                    this.fStreamPos += bitMaskSize;
                    //                    bitMaskNew = bitMaskNew.toString(2).split("").reverse();

                    var bitMaskNew = "";
                    for (var b = 0; b < bitMaskSize; b++) {
                        bitMaskNew += RemObjects.UTIL.zeroPad(this.fParser.decodeInt(this.fStream.substr(this.fStreamPos + bitMaskSize - 1 - b, 1), 8, false).toString(2), 8);
                    };
                    var bitMaskNew = bitMaskNew.split("").reverse();
                    this.fStreamPos += bitMaskSize;

                    for (var j = 0; j < fieldsCount; j++) {
                        change["new"][j] = (bitMaskNew[j] == "1") ? this.read(delta.loggedfields[j].type) : null;
                    };

                    delta.data.push(change);
                };

            };
        };
    }
    return result;
};

RemObjects.DataAbstract.DynamicWhere.prototype.toXML = function toXML() {
    if (!this.fExpression) throw new Error("Dynamic Where: fExpression is null");
    return '<query xmlns="http://www.remobjects.com/schemas/dataabstract/queries/5.0" version="5.0"><where>' +
        this.fExpression.toXML() + '</where></query>';
};

RemObjects.DataAbstract.ConstantExpression.prototype.toXML = function toXML() {
    return '<constant type="' + this.fType + '" null="' + this.fNull + '">' + this.fValue + '</constant>';
};

RemObjects.DataAbstract.NullExpression.prototype.toXML = function toXML() {
    return '<null />';
};

RemObjects.DataAbstract.FieldExpression.prototype.toXML = function toXML() {
    return '<field>' + this.fName + '</field>';
};

RemObjects.DataAbstract.MacroExpression.prototype.toXML = function toXML() {
    return '<macro>' + this.fName + '</macro>';
};

RemObjects.DataAbstract.ListExpression.prototype.toXML = function toXML() {
    var result = "<list>";
    for (var i = 0; i < this.fItems.length; i++) {
        result += this.fItems[i].toXML();
    };
    result += "</list>";
    return result;
};


RemObjects.DataAbstract.UnaryExpression.prototype.toXML = function toXML() {
    if (!this.fNode) throw new Error("UnaryExpression: fNode is null");
    return '<unaryoperation' + (this.fOperator ? ' operator="' + this.fOperator + '"' : '') + '>'
        + this.fNode.toXML() + '</unaryoperation>';
};

RemObjects.DataAbstract.BinaryExpression.prototype.toXML = function toXML() {
    if (!(this.fNode1 || this.fNode2)) throw new Error("BinaryExpression: fNode1 or fNode2 is null");
    return '<binaryoperation operator="' + this.fOperator + '">'
        + this.fNode1.toXML() + this.fNode2.toXML() + '</binaryoperation>';
};

RemObjects.DataAbstract.BetweenExpression.prototype.toXML = function toXML() {
    if (!(this.fNode1 || this.fNode2 || this.fNode3)) throw new Error("BetweenExpression: fNode1 or fNode2 or fNode3 is null");
    return '<between>'
        + this.fNode1.toXML() + this.fNode2.toXML() + this.fNode3.toXML() + '</between>';
};


RemObjects.DataAbstract.ParameterExpression.prototype.toXML = function toXML() {
    return '<parameter type="' + this.fType + '"' + (this.fSize ? ' size="' + this.fSize + '"' : '') + '>'
        + this.fName + '</parameter>';
};

RemObjects.DataAbstract.Views.HtmlTableView = function HtmlTAbleView(aTable, aHtmlTableId) {
    var htmlTable = document.getElementById(aHtmlTableId);
    var tRow = "";
    for (var i = 0; i < aTable.fields.length; i++) {
        tRow += "<td>" + aTable.fields[i].name + "</td>";
    };
    tRow = '<tr class="da_htmlTableHeader">' + tRow + "</tr>";

    var r = aTable.first();
    while (r) {
        tRow += '<tr class = "da_htmlTableLine">';
        for (var i = 0; i < aTable.fields.length; i++) {
            tRow += "<td>" + (aTable.fields[i].type == "datBlob" ? "(Blob)" : r.__newValues[i]) + "</td>";
        };
        tRow += "</tr>";
        r = aTable.next();
    };

    htmlTable.innerHTML = "<tbody>" + tRow + "</tbody>";
};

RemObjects.DataAbstract.Views.VerticalHtmlTableView = function VerticalHtmlTableView(aTable, aHtmlTableId) {
    var htmlTable = document.getElementById(aHtmlTableId);
    var tRow = "<td></td><td></td>";
    var r = aTable.currentRow();
    for (var i = 0; i < aTable.fields.length; i++) {
        tRow += "<tr>";
        tRow += '<td class="da_HtmlTableHeader">' + aTable.fields[i].name + "</td>";
        tRow += "<td>" + (aTable.fields[i].type == "datBlob" ? "(Blob)" : r.__newValues[i]) + "</td>";
        tRow += "</tr>";
    };
    htmlTable.innerHTML = "<tbody>" + tRow + "</tbody>";
};
