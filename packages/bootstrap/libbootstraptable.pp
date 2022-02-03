{
    This file is part of the Pas2JS run time library.
    Copyright (C) 2019 Michael Van Canneyt

    BootStrapTable mappings for pas2js

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit libbootstraptable;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  types, web, libjquery, js, libbootstrap;

Type
  TTableStyle = Class external name 'Object' (TJSObject)
    css: TJSObject;
    classes: string;
  end;

  TRowStyleHandler = reference to function (row: TJSObject; index : NativeInt) : TTableStyle;
  TCellStyleHandler = reference to function (Value : JSValue; Row : TJSObject; index : Integer) : TTableStyle;
  TRowSortHandler = reference to function (Const sortName,sortOrder : string; row: TJSObject) : nativeint;
  TColSortHandler = reference to function (Const fieldA,FieldB : JSValue; RowA,RowB: TJSObject) : nativeint;
  TAjaxHandler = reference to function (aURL : String; aSettings : TJSObject) : tJQXHR;
  TQueryParamsFunction = reference to function (params : TJSObject) : JSValue;
  TResponseFunction =  reference to function (res : JSValue) : JSValue;
  TSearchFunction = reference to function (data : TJSArray; aText : String) : TJSArray;
  TIgnoreClickHandler = reference to function (el : TJSHTMLELement) : Boolean;
  TDetailFormatHandler = reference to function (Index :Integer; Row : TJSObject; el : TJSHTMLElement) : String;
  TDetailFilterHandler = reference to function (Index :Integer; Row : TJSObject) : boolean;
  TColFormatterHandler = reference to function (value : JSValue; row : TJSObject; Index : Integer; Field : String) : String;
  TFooterFormatterHandler = reference to function (Rows : TJSValueDynArray) : String;
  TDetailFormatterHandler = reference to function (index : integer; row : TJSObject; el : TJSHTMLElement) : String;

  TBootstrapTableColumn = Class external name 'Object' (TJSObject)
    field : String;
    title : String;
    titleTooltip : string;
    class_ : string; external name 'class';
    width : Integer;
    widthUnit : String;
    rowspan : Integer;
    colspan : Integer;
    align : String;
    halign : String;
    falign : String;
    valign : string;
    cellStyle : TCellStyleHandler;
    radio : boolean;
    checkbox : boolean;
    checkboxEnabled : Boolean;
    clickToSelect : Boolean;
    showSelectTitle : Boolean;
    sortable : Boolean;
    sortName : String;
    order : string;
    sorter : TColSortHandler;
    visible : Boolean;
    switchable : Boolean;
    cardVisible : Boolean;
    searchable : Boolean;
    formatter : TColFormatterHandler ;
    footerFormatter : TFooterFormatterHandler;
    detailFormatter : TDetailFormatterHandler;
    searchFormatter : boolean;
    escape : boolean;
    events : TJSObject;
  end;
  TBootstrapTableColumnArray = array of TBootstrapTableColumn;

  TFooterStyleHandler = Reference to function(col : TBootstrapTableColumn) :  TTableStyle;


  TBootstrapTableOptions = class external name 'Object' (TJSObject)
    ajax : TAjaxHandler;
    ajaxOptions : TJSObject;
    buttonsAlign : String;
    buttonsClass : String;
    buttonsPrefix : String;
    buttonsToolbar : JSValue;
    cache : Boolean;
    cardView : Boolean;
    checkboxHeader : Boolean;
    classes : string;
    clickToSelect : Boolean;
    columns : TBootstrapTableColumnArray;
    contentType : string;
    customSearch : TSearchFunction;
    customSort : TRowSortHandler;
    data : JSValue; // Object or aray
    dataField : string;
    dataType : string;
    detailFilter : TDetailFilterHandler;
    detailFormatter : TDetailFormatHandler;
    detailView : Boolean;
    detailViewByClick : Boolean;
    detailViewIcon : Boolean;
    escape : Boolean;
    filterOptions : Boolean;
    footerStyle : TFooterStyleHandler;
    height : Integer;
    html : TJSObject;
    icons : TJSObject;
    iconSize : String;
    iconsPrefix : String;
    idField : String;
    ignoreClickToSelectOn : TIgnoreClickHandler;
    locale : string;
    maintainMetaData : Boolean;
    method : string;
    minimumCountColumns : NativeInt;
    multipleSelectRow : Boolean;
    onlyInfoPagination : Boolean;
    pageList : TNativeIntDynArray;
    pageNumber : NativeInt;
    pageSize : NativeInt;
    pagination : boolean;
    paginationDetailHAlign : string;
    paginationHAlign : string;
    paginationLoop : Boolean;
    paginationNextText : String;
    paginationPagesBySide : NativeInt;
    paginationPreText : string;
    paginationSuccessivelySize : NativeInt;
    paginationUseIntermediate : Boolean;
    paginationVAlign : string;
    queryParams : TQueryParamsFunction;
    queryParamsType : String;
    rememberOrder : boolean;
    resizable : boolean;
    responseHandler : TResponseFunction;
    rowAttributes : TRowStyleHandler;
    rowStyle : TRowStyleHandler;
    search : boolean;
    searchAlign : String;
    searchOnEnterKey : Boolean;
    searchText :  string;
    searchTimeOut : Boolean;
    selectItemName : String;
    showButtonIcon : Boolean;
    showButtonIcons : Boolean;
    showButtonText : Boolean;
    showColumns : Boolean;
    showColumnsToggleAll : Boolean;
    showExtendedPagination : boolean;
    showFooter : Boolean;
    showFullscreen : Boolean;
    showHeader : Boolean;
    showPaginationSwitch : Boolean;
    showRefresh : Boolean;
    showSearchButton : Boolean;
    showSearchClearButton : Boolean;
    showToggle : Boolean;
    sidePagination : String;
    silentSort : Boolean;
    singleSelect : Boolean;
    smartDisplay : Boolean;
    sortable : boolean;
    sortClass : String;
    sortName : string;
    sortOrder : String;
    sortStable : boolean;
    strictSearch : Boolean;
    theadClasses : string;
    toolbar : JSValue;
    toolbarAlign : String;
    totalField : String;
    totalNotFiltered : NativeInt;
    totalNotFilteredField : String;
    totalRows : NativeInt;
    trimOnSearch : Boolean;
    undefinedText : String;
    uniqueId : Boolean;
    url : string;
    virtualScroll : Boolean;
    virtualScrollItemHeight : Integer;
    visibleSearch : Boolean;
  end;
  { TJSBootstrapTable }
  TRefreshOptions = class external name 'Object' (TJSObject)
    silent : boolean;
    url : string;
    pageNumber : Integer;
    pageSize : integer;
    query : TJSObject;
  end;

  TJSBootstrapTable = class helper(TBootstrap) for TJQuery
  private
  Public
    Function BootstrapTable(command : string) : JSValue; varargs; overload; external name 'bootstrapTable';
    Function BootstrapTable(options : TBootstrapTableOptions) : TJQuery; overload; external name 'bootstrapTable';
    Function BootstrapTable(options : TJSObject) : TJQuery; overload; external name 'bootstrapTable';
    Function getOptionsUnTyped : TJSObject;
    Function getOptions : TBootstrapTableOptions;
    function refreshOptions (Options : TBootstrapTableOptions) : TJQuery;
    function getData (useCurrentPage : Boolean = False; includeHiddenRows : Boolean = False) : JSValue;
    Function destroy : TJQuery; overload;
    Function GetSelections : TJSArray;
    Procedure Load(data : JSValue);
    procedure Append(data: JSValue);
    procedure Prepend(data: JSValue);
    procedure RemoveAll;
    Procedure insertRow(Index : Integer; RowData : JSValue);
    procedure updateRow(Index: Integer; RowData: JSValue; Replace: Boolean=False);
    function GetRowByUniqueID(aID : NativeInt): TJSObject;
    Procedure UpdateByUniqueID(aID : NativeInt;RowData: JSValue; Replace: Boolean=False);
    Procedure RemoveByUniqueID(aID : NativeInt);
    Procedure UpdateCell(aIndex : NativeInt; aField,aValue : string; ReInit : Boolean = true);
    Procedure UpdateCellByUniqueID(aId : NativeInt; aField,aValue : string; ReInit : Boolean = true);
    Procedure ShowRow(aIndex : NativeInt);
    Procedure ShowRowByUniqueID(aId : NativeInt);
    Function GetHiddenRows(doShow : Boolean = False) : TJSArray;
    Procedure ShowColumn(aField : String);
    Procedure ShowColumn(aFields : TStringDynArray);
    Procedure HideColumn(aField : String);
    Procedure HideColumn(aFields : TStringDynArray);
    function GetHiddenColumns: TJSArray;
    function GetVisibleColumns: TJSArray;
    Procedure ShowAllColumns;
    Procedure HideAllColumns;
    Procedure mergeCells(aIndex : integer; aField : String; RowSpan : Integer; ColSpan : Integer);
    Procedure CheckAll;
    Procedure UnCheckAll;
    Procedure CheckInvert;
    Procedure Check(aIndex : Integer);
    Procedure UnCheck(aIndex : Integer);
    Procedure CheckBy(Field : String; Values : TStringDynArray);
    Procedure UnCheckBy(Field : String; Values : TStringDynArray);
    Procedure RefreshTable(options : TRefreshOptions);
    Procedure RefreshTable(options : TJSObject);
    Procedure ResetView(options : TBootstrapTableOptions);
    Procedure ResetWidth;
    Procedure ShowLoading;
    Procedure HideLoading;
    Procedure TogglePagination;
    Procedure ToggleFullScreen;
    Procedure ToggleView;
    Procedure ResetSearch;
    Procedure FilterBy(aFilter : TJSObject; aOptions : TJSObject);
    Procedure scrollToRow(aValue : Integer);
    Procedure scrollToPixel(aValue : Integer);
    Procedure scrollToBottom;
    Function GetScrollPosition : NativeInt;
    Procedure SelectPage(aIndex : Integer);
    Procedure PrevPage;
    Procedure NextPage;
    procedure ToggleDetailView(Index : Integer);
    procedure ExpandRow(Index: Integer);
    procedure CollapseRow(Index: Integer);
    procedure CollapseAllRows;
    procedure ExpandAllRows;
    procedure UpdateColumnTitle(const aField,aTitle : String);
    procedure UpdateFormatText(const aFormat,aValue : String);
  end;

implementation

{ TJSBootstrapTable }

function TJSBootstrapTable.getOptionsUnTyped: TJSObject;
begin
  Result:=TJSObject(BootstrapTable('getOptions'))
end;

function TJSBootstrapTable.getOptions: TBootstrapTableOptions;
begin
  Result:=TBootstrapTableOptions(getoptionsUnTyped);
end;

function TJSBootstrapTable.refreshOptions(Options: TBootstrapTableOptions): TJQuery;
begin
  Result:=TJQuery(BootstrapTable('refreshOptions',Options));
end;

function TJSBootstrapTable.getData(useCurrentPage: Boolean; includeHiddenRows: Boolean): JSValue;
begin
  Result:=BootstrapTable('getdata',JS.New([
     'useCurrentPage',useCurrentPage,
     'includeHiddenRows',includeHiddenRows
  ]));
end;

function TJSBootstrapTable.destroy: TJQuery;
begin
  Result:=TJQuery(BootstrapTable('destroy'));
end;

function TJSBootstrapTable.GetSelections: TJSArray;
begin
  Result:=TJSArray(BootstrapTable('getSelections'));
end;

procedure TJSBootstrapTable.Load(data: JSValue);
begin
  BootstrapTable('load',data);
end;

procedure TJSBootstrapTable.Append(data: JSValue);
begin
  BootstrapTable('append',data);
end;

procedure TJSBootstrapTable.Prepend(data: JSValue);

begin
  BootstrapTable('prepend',data);
end;

procedure TJSBootstrapTable.RemoveAll;
begin
  BootstrapTable('removeAll');
end;

procedure TJSBootstrapTable.insertRow(Index: Integer; RowData: JSValue);
begin
  BootstrapTable('insertRow',JS.new([
    'index',index,
    'row', RowData
  ]));
end;

procedure TJSBootstrapTable.updateRow(Index: Integer; RowData: JSValue; Replace : Boolean = False);

begin
  BootstrapTable('updateRow',JS.new([
    'index',index,
    'row', rowData,
    'replace', replace
  ]));
end;

function TJSBootstrapTable.GetRowByUniqueID(aID: NativeInt): TJSObject;
begin
  Result:=TJSObject(BootstrapTable('getRowByUniqueId',aID));
end;

procedure TJSBootstrapTable.UpdateByUniqueID(aID: NativeInt; RowData: JSValue; Replace: Boolean);
begin
  BootstrapTable('updateByUniqueId',JS.new([
    'id',aId,
    'row', rowData,
    'replace', replace
  ]));
end;

procedure TJSBootstrapTable.RemoveByUniqueID(aID: NativeInt);
begin
  BootstrapTable('removeByUniqueId',aID);
end;

procedure TJSBootstrapTable.UpdateCell(aIndex: NativeInt; aField, aValue: string; ReInit : Boolean = true);
begin
  BootstrapTable('updateCell',JS.new([
    'index',aIndex,
    'field', aField,
    'value', aValue,
    'reinit', ReInit
  ]));
end;

procedure TJSBootstrapTable.UpdateCellByUniqueID(aId: NativeInt; aField, aValue: string; ReInit: Boolean);
begin
  BootstrapTable('updateCellByUniqueID',JS.new([
    'id',aId,
    'field', aField,
    'value', aValue,
    'reinit', ReInit
  ]));
end;

procedure TJSBootstrapTable.ShowRow(aIndex: NativeInt);
begin
  BootstrapTable('showRow',JS.new(['id',aIndex]));
end;

procedure TJSBootstrapTable.ShowRowByUniqueID(aId: NativeInt);
begin
  BootstrapTable('showRow',JS.new(['uniqueId',aId]));
end;

function TJSBootstrapTable.GetHiddenRows(doShow: Boolean): TJSArray;
begin
  Result:=TJSArray(BootstrapTable('getHiddenRows',doShow));
end;

procedure TJSBootstrapTable.ShowColumn(aField: String);
begin
  BootstrapTable('showColumn',aField);
end;

procedure TJSBootstrapTable.ShowColumn(aFields: TStringDynArray);
begin
  BootstrapTable('showColumn',aFields);
end;

procedure TJSBootstrapTable.HideColumn(aField: String);
begin
  BootstrapTable('hideColumn',aField);
end;

procedure TJSBootstrapTable.HideColumn(aFields: TStringDynArray);
begin
  BootstrapTable('hideColumn',aFields);
end;

function TJSBootstrapTable.GetHiddenColumns: TJSArray;
begin
  Result:=TJSArray(BootstrapTable('getHiddenColumns'));
end;

function TJSBootstrapTable.GetVisibleColumns: TJSArray;
begin
  Result:=TJSArray(BootstrapTable('getVisibleColumns'));
end;

procedure TJSBootstrapTable.ShowAllColumns;
begin
  BootstrapTable('showAllColumns');
end;

procedure TJSBootstrapTable.HideAllColumns;
begin
  BootstrapTable('hideAllColumns');
end;

procedure TJSBootstrapTable.mergeCells(aIndex: integer; aField: String; RowSpan: Integer; ColSpan: Integer);

begin
  BootstrapTable('mergeCells',JS.New([
    'index',aIndex,
    'field',aField,
    'rowspan',rowspan,
    'colspan',colspan
  ]));
end;

procedure TJSBootstrapTable.CheckAll;
begin
  BootStrapTable('checkAll');
end;

procedure TJSBootstrapTable.UnCheckAll;
begin
  BootStrapTable('uncheckAll');
end;

procedure TJSBootstrapTable.CheckInvert;
begin
  BootStrapTable('checkInvert');
end;

procedure TJSBootstrapTable.Check(aIndex: Integer);
begin
  BootStrapTable('check',aIndex);
end;

procedure TJSBootstrapTable.UnCheck(aIndex: Integer);
begin
  BootStrapTable('uncheck',aIndex);
end;

procedure TJSBootstrapTable.CheckBy(Field: String; Values: TStringDynArray);
begin
  BootStrapTable('checkBy',JS.New(['field',Field,'values',Values]));
end;

procedure TJSBootstrapTable.UnCheckBy(Field: String; Values: TStringDynArray);
begin
  BootStrapTable('uncheckBy',JS.New(['field',Field,'values',Values]));
end;

procedure TJSBootstrapTable.RefreshTable(options: TRefreshOptions);
begin
  BootStrapTable('refresh',options);
end;

procedure TJSBootstrapTable.RefreshTable(options: TJSObject);
begin
  BootStrapTable('refresh',options);
end;

procedure TJSBootstrapTable.ResetView(options: TBootstrapTableOptions);
begin
  BootStrapTable('resetView',options);
end;

procedure TJSBootstrapTable.ResetWidth;
begin
  BootStrapTable('resetWidth');
end;

procedure TJSBootstrapTable.ShowLoading;
begin
  BootStrapTable('showLoading');
end;

procedure TJSBootstrapTable.HideLoading;
begin
  BootStrapTable('hideLoading');
end;

procedure TJSBootstrapTable.TogglePagination;
begin
  BootStrapTable('togglePagination');
end;

procedure TJSBootstrapTable.ToggleFullScreen;
begin
  BootStrapTable('toggleFullScreen');
end;

procedure TJSBootstrapTable.ToggleView;
begin
  BootStrapTable('toggleView');
end;

procedure TJSBootstrapTable.ResetSearch;
begin
  BootStrapTable('resetSearch');
end;

procedure TJSBootstrapTable.FilterBy(aFilter: TJSObject; aOptions: TJSObject);
begin
  BootStrapTable('filterBy',JS.New(['filter',aFilter,'options',aOptions]));
end;

procedure TJSBootstrapTable.scrollToRow(aValue: Integer);
begin
  BootStrapTable('scrollTo',JS.New(['unit','rows','value',aValue]));
end;

procedure TJSBootstrapTable.scrollToPixel(aValue: Integer);
begin
  BootStrapTable('scrollTo',JS.New(['unit','px','value',aValue]));
end;

procedure TJSBootstrapTable.scrollToBottom;
begin
  BootStrapTable('scrollTo','bottom');
end;

function TJSBootstrapTable.GetScrollPosition: NativeInt;
begin
  Result:=NativeInt(BootStrapTable('getScrollPosition'));
end;

procedure TJSBootstrapTable.SelectPage(aIndex: Integer);
begin
  BootStrapTable('selectPage',aIndex);
end;

procedure TJSBootstrapTable.PrevPage;
begin
  BootStrapTable('prevPage');
end;

procedure TJSBootstrapTable.NextPage;
begin
  BootStrapTable('nextPage');
end;

procedure TJSBootstrapTable.ToggleDetailView(Index: Integer);
begin
  BootStrapTable('toggleDetailView',Index);
end;
procedure TJSBootstrapTable.ExpandRow(Index: Integer);
begin
  BootStrapTable('expandRow',Index);
end;

procedure TJSBootstrapTable.CollapseRow(Index: Integer);
begin
  BootStrapTable('collapseRow',Index);
end;

procedure TJSBootstrapTable.CollapseAllRows;
begin
  BootStrapTable('collapseAllRows');
end;

procedure TJSBootstrapTable.ExpandAllRows;
begin
  BootStrapTable('expandAllRows');
end;

procedure TJSBootstrapTable.UpdateColumnTitle(const aField, aTitle: String);
begin
  BootStrapTable('updateColumnTitle',JS.New(['field',aField,'title',aTitle]));
end;

procedure TJSBootstrapTable.UpdateFormatText(const aFormat, aValue: String);
begin
  BootStrapTable('updateFormatText',aFormat,aValue);
end;


end.
