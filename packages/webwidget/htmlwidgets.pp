{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019-Now by Michael Van Canneyt, member of the
    Free Pascal development team

    WEB Widget Set : Basic bare HTML Widgets

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit htmlwidgets;

{$mode objfpc}

interface

uses
  Classes, SysUtils, webwidget, js, web;

Type
  TTextMode = (tmText,tmHTML);

  { TButtonWidget }

  TButtonWidget = Class(TWebWidget)
  private
    FText: String;
    FTextMode: TTextMode;
    procedure SetText(AValue: String);
    procedure SetTextMode(AValue: TTextMode);
  Protected
    procedure ApplyText(aElement: TJSHTMLElement);
    Procedure SetName(const NewName: TComponentName); override;
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
  Public
    Procedure Click;
    Function HTMLTag : String; override;
  Published
    Property Text : String Read FText Write SetText;
    Property TextMode : TTextMode Read FTextMode Write SetTextMode;
  end;

  { TViewPort }

  TViewPort = Class(TCustomWebWidget)
  Private
    Class var FInstance : TViewPort;
  Protected
    Class Function FixedParent : TJSHTMLElement; override;
    Class Function FixedElement : TJSHTMLElement; override;
    Function DoRenderHTML(aParent,aElement : TJSHTMLElement) :TJSHTMLElement; override;
  Public
    Constructor Create (aOwner: TComponent); override;
    Function HTMLTag : String; override;
    Class Function Instance : TViewPort;
    Property Element;
  end;

  { TWebPage }

  TWebPage = Class(TCustomWebWidget)
  private
  Protected
    Class Function DefaultParentElement: TJSHTMLElement; override;
    Class Function DefaultParent : TCustomWebWidget; override;
    Procedure DoUnRender(aParent : TJSHTMLElement) ; override;
  Public
    Constructor Create(AOwner : TComponent); override;
    Function HTMLTag : String; override;
    // Later on, allow IFrame;
  Published
    Property ParentID;
    Property ElementID;
    Property Classes;
    Property Styles;
    Property StyleRefresh;
    Property Visible;
    // Events
    Property BeforeRenderHTML;
    Property AfterRenderHTML;
    Property OnAbort;
    Property OnAnimationCancel;
    Property OnAnimationEnd;
    Property OnAnimationIteration;
    Property OnAnimationStart;
    Property OnAuxClick;
    Property OnBlur;
    Property OnCancel;
    Property OnCanPlay;
    Property OnCanPlayThrough;
    Property OnChange;
    Property OnClick;
    Property OnCompositionEnd;
    Property OnCompositionStart;
    Property OnCompositionUpdate;
    Property OnContextMenu;
    Property OnCopy;
    Property OnCut;
    Property OnCueChange;
    Property OnDblClick;
    Property OnDurationChange;
    Property OnEnded ;
    Property OnError ;
    Property OnFocus;
    Property OnFocusIn ;
    Property OnFocusOut ;
    Property OnGotPointerCapture;
    Property OnInput;
    Property OnInvalid;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnLoad;
    Property OnLoadedData;
    Property OnLoadedMetaData;
    Property OnLoadend;
    Property OnLoadStart;
    Property OnLostPointerCapture;
    Property OnMouseDown;
    Property OnMouseEnter;
    Property OnMouseLeave;
    Property OnMouseMove;
    Property OnMouseOut;
    Property OnMouseUp;
    Property OnOverFlow;
    Property OnPaste;
    Property OnPause;
    Property OnPlay;
    Property OnPointerCancel;
    Property OnPointerDown;
    Property OnPointerEnter;
    Property OnPointerLeave;
    Property OnPointerMove;
    Property OnPointerOut;
    Property OnPointerOver;
    Property OnPointerUp;
    Property OnReset;
    Property OnResize;
    Property OnScroll;
    Property OnSelect;
    Property OnSubmit;
    Property OnTouchStart;
    Property OnTransitionCancel;
    Property OnTransitionEnd;
    Property OnTransitionRun;
    Property OnTransitionStart;
    Property OnWheel;
  end;

  { TCustomInputWidget }

  TCustomInputWidget = Class(TWebWidget)
  private
    FValue : String;
    FValueName : String;
    FText : String;
    FReadOnly : Boolean;
    FRequired : Boolean;
    function GetReadOnly: Boolean;
    function GetRequired: Boolean;
    function GetText: String;
    function GetValue: String;
    function GetValueName: String;
    procedure SetReadonly(AValue: Boolean);
    procedure SetRequired(AValue: Boolean);
    procedure SetText(AValue: String);
    procedure SetValue(AValue: String);
    function GetInputElement: TJSHTMLInputElement;
    procedure SetValueName(AValue: String);
  Protected
    Procedure SetName(const NewName: TComponentName); override;
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
    Property InputElement : TJSHTMLInputElement Read GetInputElement;
    // Text to show (checkbox etc). Enable in descendents as needed
    Property Text : String Read GetText Write SetText;
  Public
    function InputType : String; virtual; abstract;
    Function HTMLTag : String; override;
    // Value as string
    Property Value : String Read GetValue Write SetValue;
    // Value Name to use when submitting using form.
    Property ValueName : String Read GetValueName Write SetValueName;
    Property ReadOnly : Boolean Read GetReadOnly Write SetReadonly;
    Property Required : Boolean Read GetRequired Write SetRequired;
  end;

  { TTextInputWidget }

  TInputTextType = (ittText,ittPassword,ittNumber,ittEmail,ittSearch,ittTelephone,ittURL,ittColor);
  TTextInputWidget = class(TCustomInputWidget)
  private
    FMaxLength : Integer;
    FMinLength : Integer;
    FTextType : TInputTextType;
    function GetAsNumber: NativeInt;
    function GetMaxLength: NativeInt;
    function GetMinLength: NativeInt;
    function GetTextType: TInputTextType;
    procedure SetAsNumber(AValue: NativeInt);
    procedure SetMaxLength(AValue: NativeInt);
    procedure SetMinLength(AValue: NativeInt);
    procedure SetTextType(AValue: TInputTextType);
  Protected
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
  Public
    Class Function AllowChildren : Boolean; override;
    function InputType : String; override;
  Published
    Property Value;
    Property ValueName;
    Property Required;
    Property TextType : TInputTextType Read GetTextType Write SetTextType;
    property AsNumber : NativeInt Read GetAsNumber Write SetAsNumber;
    Property MaxLength : NativeInt Read GetMaxLength Write SetMaxLength;
    Property MinLength : NativeInt Read GetMinLength Write SetMinLength;
    // Todo: List support
  end;


  { TButtonInputWidget }
  TInputButtonType = (ibtSubmit,ibtReset,ibtImage);
  TInputButtonTypes = set of TInputButtonType;

  TButtonInputWidget = class(TCustomInputWidget)
  private
    FButtonType: TInputButtonType;
    FSrc: String;
    procedure SetButtonType(AValue: TInputButtonType);
    procedure SetSrc(AValue: String);
  Public
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
    function InputType : String; override;
    Class Function AllowChildren : Boolean; override;
  Published
    Property ButtonType : TInputButtonType Read FButtonType Write SetButtonType;
    Property Value;
    Property ValueName;
    Property Src : String Read FSrc Write SetSrc;
  end;

  { TCheckableInputWidget }

  TCheckableInputWidget = class(TCustomInputWidget)
  private
    FChecked: Boolean;
    function GetChecked: Boolean;
    procedure SetChecked(AValue: Boolean);
  Protected
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
  Public
    Property Value;
    Property ValueName;
    Property Checked : Boolean Read GetChecked Write SetChecked;
    Property Text;
  end;

  { TRadioInputWidget }

  TRadioInputWidget = class(TCheckableInputWidget)
  private
  Public
    function InputType : String; override;
  Published
    Property Value;
    Property ValueName;
    Property Checked;
    Property Text;
  end;

  { TCheckboxInputWidget }

  TCheckboxInputWidget = class(TCheckableInputWidget)
  private
  Public
    function InputType : String; override;
  Published
    Property Value;
    Property ValueName;
    Property Checked;
    Property Text;
  end;


  { TDateInputWidget }

  TDateInputWidget = class(TCustomInputWidget)
  private
    FDate: TDateTime;
    function GetDate: TDateTime;
    procedure SetDate(AValue: TDateTime);
  Public
    function InputType : String; override;
    Class Function AllowChildren : Boolean; override;
  Published
    Property Required;
    Property ValueName;
    Property Date : TDateTime Read GetDate Write SetDate;
  end;

  { TFileInputWidget }
  TFileInfo = record
    Name : String;
    TimeStamp : TDateTime;
    FileType : String;
    Size : NativeInt;
  end;

  TFileInputWidget = class(TCustomInputWidget)
  private
    FMultiple: Boolean;
    function GetFileCount: Integer;
    function GetFileDate(aIndex : Integer): TDateTime;
    function GetFileInfo(aIndex : Integer): TFileInfo;
    function GetFileName(aIndex : Integer): String;
    function GetFileSize(aIndex : Integer): NativeInt;
    function GetFileType(aIndex : Integer): String;
    function GetMultiple: Boolean;
    procedure SetMultiple(AValue: Boolean);
  Protected
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
  Public
    Class Function AllowChildren : Boolean; override;
    function InputType : String; override;
    Property FileCount : Integer read GetFileCount;
    Property Files[aIndex : Integer] : String Read GetFileName;
    Property FileSizes[aIndex : Integer] : NativeInt Read GetFileSize;
    Property FileTypes[aIndex : Integer] : String Read GetFileType;
    Property FileDates[aIndex : Integer] : TDateTime Read GetFileDate;
    Property FileInfos[aIndex : Integer] : TFileInfo Read GetFileInfo;
  Published
    Property ValueName;
    Property Required;
    Property Multiple : Boolean Read GetMultiple Write SetMultiple;
  end;

  { THiddenInputWidget }

  THiddenInputWidget = class(TCustomInputWidget)
  Public
    Class Function AllowChildren : Boolean; override;
    function InputType : String; override;
  Published
    Property ValueName;
    Property Value;
    Property Required;
  end;

  { TTextAreaWidget }

  TTextAreaWrap = (tawSoft,tawHard,tawOff);
  TTextAreaWidget = Class(TWebWidget)
  private
    FLines: TStrings;
    FIgnoreChanges : Boolean;
    FMaxLength: Cardinal;
    FValueName : String;
    FRows,
    FColumns : Cardinal;
    FWrap: TTextAreaWrap;
    FRequired,
    FReadOnly : Boolean;
    procedure ApplyWrap(aElement: TJSHTMLTextAreaElement);
    procedure DoLineChanges(Sender: TObject);
    function GetColumns: Cardinal;
    function GetLines: TStrings;
    function GetReadOnly: Boolean;
    function GetRequired: Boolean;
    function GetRows: Cardinal;
    function GetText: String;
    function GetValueName: string;
    procedure SetColumns(AValue: Cardinal);
    procedure SetLines(AValue: TStrings);
    procedure SetMaxLength(AValue: Cardinal);
    procedure SetReadonly(AValue: Boolean);
    procedure SetRequired(AValue: Boolean);
    procedure SetRows(AValue: Cardinal);
    procedure SetText(AValue: String);
    procedure SetValueName(AValue: string);
    Function GetTextArea : TJSHTMLTextAreaElement;
    procedure SetWrap(AValue: TTextAreaWrap);
  Protected
    procedure ApplyLines(aElement: TJSHTMLTextAreaElement);
    procedure LinesFromHTML(aHTML : String);
    Procedure SetName(const NewName: TComponentName); override;
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
    Property TextArea :TJSHTMLTextAreaElement Read GetTextArea;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Class Function AllowChildren : Boolean; override;
    Function HTMLTag : String; override;
    Property InnerHTML : String Read GetText Write SetText;
  Published
    Property ValueName : string Read GetValueName Write SetValueName;
    Property Rows : Cardinal Read GetRows Write SetRows;
    Property Columns : Cardinal Read GetColumns Write SetColumns;
    Property Lines : TStrings Read GetLines Write SetLines;
    Property MaxLength : Cardinal Read FMaxLength Write SetMaxLength;
    Property Wrap : TTextAreaWrap Read FWrap Write SetWrap;
    Property ReadOnly : Boolean Read GetReadOnly Write SetReadonly;
    Property Required : Boolean Read GetRequired Write SetRequired;
  end;

  { TImageWidget }

  TImageWidget = class(TWebWidget)
  private
    FHeight: Integer;
    FWidth: Integer;
    FSrc : String;
    function GetHeight: Integer;
    function GetImg: TJSHTMLImageElement;
    function GetSrc: String;
    function GetWidth: Integer;
    procedure SetHeight(AValue: Integer);
    procedure SetSrc(AValue: String);
    procedure SetWidth(AValue: Integer);
  Protected
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
    Property ImgElement : TJSHTMLImageElement Read GetImg;
  Public
    Function HTMLTag : String; override;
  Published
    Property Src : String Read GetSrc Write SetSrc;
    Property Width : Integer Read GetWidth Write SetWidth;
    Property Height : Integer Read GetHeight Write SetHeight;
  end;

  { TSelectWidget }

  TJSHTMLOptionElementArray = Array of TJSHTMLOptionElement;
  TCustomSelectWidget = Class;


  { TCustomSelectWidget }

  TCustomSelectWidget = class(TWebWidget)
  Private
    FSize,
    FSelectedIndex : Integer;
    FOptions : TJSHTMLOptionElementArray;
    FMultiple : Boolean;
    function GetMultiple: Boolean;
    function GetSelected(Index : Integer): Boolean;
    function GetSelectedIndex: Integer;
    function GetSelect: TJSHTMLSelectElement;
    function GetSelectionCount: Integer;
    function GetSelectionItem(aIndex : Integer): String;
    function GetSelectionValue(aIndex : Integer): String;
    function GetSize: Integer;
    procedure SetMultiple(AValue: Boolean);
    procedure SetSelected(Index : Integer; AValue: Boolean);
    procedure SetSelectedIndex(AValue: Integer);
    procedure SetSize(AValue: Integer);
  Protected
    Type
      { TSelectOptionEnumerator }
      TSelectOptionEnumerator = Class
      private
        FSelect: TCustomSelectWidget;
      public
        constructor Create(ASelect : TCustomSelectWidget); reintroduce; virtual;
        Function OptionText : String; virtual; abstract;
        Function HasValue : boolean; virtual;
        Function Value : string; virtual;
        function MoveNext: Boolean; virtual; abstract;
        Property Select: TCustomSelectWidget Read FSelect;
      end;
  Protected
    function GetItemCount: Integer; virtual;
    Function CreateOptionEnumerator : TSelectOptionEnumerator; virtual; abstract;
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
    Procedure BuildOptions(aSelect : TJSHTMLSelectElement); virtual;
    Property Options : TJSHTMLOptionElementArray Read Foptions;
    Property SelectElement : TJSHTMLSelectElement Read GetSelect;
  Protected
    // Can be made public/published
    // Items that are selected
    Property ItemCount : Integer Read GetItemCount;
    Property Selected[Index : Integer] : Boolean Read GetSelected Write SetSelected;
    Property SelectionCount : Integer Read GetSelectionCount;
    Property SelectionValue[aIndex : Integer] : String Read GetSelectionValue;
    Property SelectionItem[aIndex : Integer] : String Read GetSelectionItem;
    property SelectedIndex : Integer Read GetSelectedIndex Write SetSelectedindex;
    Property Multiple : Boolean Read GetMultiple Write SetMultiple;
    Property Size : Integer Read GetSize Write SetSize;
  Public
    Constructor Create(aOWner : TComponent); override;
    Function HTMLTag : String; override;
  end;

  TSelectWidget = class(TCustomSelectWidget)
  private
    FItems : TStrings;
    FValues : TStrings;
    function GetItems: TStrings;
    function GetValues: TStrings;
    procedure OptionsChanged(Sender: TObject);
    procedure setItems(AValue: TStrings);
    procedure setValues(AValue: TStrings);
  Protected
    Type
      { TStringsSelectOptionEnumerator }
      TStringsSelectOptionEnumerator = Class(TSelectOptionEnumerator)
        FCurrent : Integer;
        constructor Create(ASelect : TCustomSelectWidget); override;
        Function OptionText : String; override;
        Function HasValue : boolean; override;
        Function Value : string; override;
        function MoveNext: Boolean; override;
      end;
    Function CreateOptionEnumerator: TSelectOptionEnumerator; override;
  Public
    Constructor Create(aOWner : TComponent); override;
    Destructor Destroy; override;
    Property SelectionCount;
    Property SelectionValue;
    Property SelectionItem;
    Property Selected;
    Property Options;
    Property SelectElement;
    Property ItemCount;
  Published
    Property Items : TStrings Read GetItems Write setItems;
    Property Values : TStrings Read GetValues Write setValues;
    property SelectedIndex;
    Property Multiple;
    property size;
    property Classes;
  end;

  { TLabelWidget }

  TLabelWidget = Class(TWebWidget)
  private
    FLabelFor: TWebWidget;
    FText: String;
    function GetLabelEl: TJSHTMLLabelElement;
    function GetText: String;
    procedure SetLabelFor(AValue: TWebWidget);
    procedure SetText(AValue: String);
  Protected
    procedure ApplyLabelFor(aLabelElement: TJSHTMLLabelElement);
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure SetName(const NewName: TComponentName); override;
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
    Property LabelElement : TJSHTMLLabelElement Read GetLabelEl;
  Public
    Function HTMLTag : String; override;
    Property Text : String Read GetText Write SetText;
    Property LabelFor : TWebWidget Read FLabelFor Write SetLabelFor;
  end;

  TTextTag = (ttParagraph,ttBold,ttItalic,ttUnderline,ttStrikeThrough,ttSpan,ttQuote,ttBlockQuote,ttH1,ttH2,ttH3,ttH4,ttH5,ttH6,ttPre,ttRuby,ttArticle,ttAddress,ttAbbr,ttCustom);

  { TTextWidget }

  { TCustomTextWidget }

  TCustomTextWidget = Class(TCustomWebWidget)
  private
    FCustomTag: String;
    FEnvelopeTag: TTextTag;
    FTextMode: TTextMode;
    procedure SetCustomTag(AValue: String);
    procedure SetEnvelopeTag(AValue: TTextTag);
    procedure SetTextMode(AValue: TTextMode);
  Protected
    procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
    procedure ApplyText(aElement : TJSHTMLElement); virtual;
    Function GetText : String; virtual; abstract;
  Public
    Function HTMLTag : String; override;
  Published
    Property CustomTag : String Read FCustomTag Write SetCustomTag;
    Property EnvelopeTag : TTextTag Read FEnvelopeTag Write SetEnvelopeTag;
    Property TextMode : TTextMode Read FTextMode Write SetTextMode;
  end;

  TTextWidget = Class(TCustomTextWidget)
  private
    FText : String;
    procedure SetText(AValue: String);
  Protected
    Function GetText : String; override;
  published
    Property Text : String Read FText Write SetText;
  end;

  { TTextLinesWidget }

  TTextLinesWidget = Class(TCustomTextWidget)
  private
    FLines : TStrings;
    FForceLineBreaks: Boolean;
    procedure DoLinesChanged(Sender: TObject);
    procedure SetLines(AValue: TStrings);
    procedure SetForceLineBreaks(AValue: Boolean);
  Protected
    Function GetText : String; override;
    procedure ApplyText(aElement : TJSHTMLElement); override;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
  published
    Property Lines : TStrings Read FLines Write SetLines;
    // When forcelinebreaks is true a <br> will be appended to every line.
    // Note that for TextMode=tmText this means the lines will be rendered as-is, but there will still be a <br> between the lines
    Property ForceLineBreaks : Boolean Read FForceLineBreaks Write SetForceLineBreaks;
  end;

  { TCustomTableColumn }
  TColumnOption = (coHeader,coCaptionHeader);
  TColumnOptions = set of TColumnOption;

  TCustomTableColumn = Class(TCollectionItem)
  private
    FAlignment: TAlignment;
    FCaption: String;
    FClassNames: String;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetCaption(AValue: String);
    procedure SetClassNames(AValue: String);
  Protected
    Function RenderColumn : Boolean; virtual;
    Function GetDisplayName: string; override;
    function GetCaption: String; virtual;
  Public
    Procedure Assign(Source : TPersistent); override;
    Property Alignment : TAlignment Read FAlignment Write SetAlignment;
    Property Caption : String Read GetCaption Write SetCaption;
    Property ClassNames : String Read FClassNames Write SetClassNames;
  end;

  { TCustomTableColumns }

  TCustomTableColumns = Class(TCollection)
  private
    function GetCustomColumn(Index : Integer): TCustomTableColumn;
    procedure SetCustomColumn(Index : Integer; AValue: TCustomTableColumn);
  Protected
    Property CustomColumns [Index : Integer] : TCustomTableColumn Read GetCustomColumn Write SetCustomColumn; default;
  Public
    Function Add(aCaption : String): TCustomTableColumn; overload;
  end;

  { TCustomTableWidget }
  TTableOption = (toHeader,    // use THead tag
                  toHeaderRow, // Create header row
                  toBody,      // use TBody tag
                  toFooter,    // use TFoot tag
                  toFooterRow,  // create footer row
                  toRowID,      // add ID to tr: -kind-row
                  toCellID,     // add ID to cell td: -kind-row-col
                  toHeaderRowData,     // Add rowno to <tr data-row> for header.
                  toHeaderCellDataRow, // Add rowno to <th data-row> for header. Automatic if onheadercellclick is set.
                  toHeaderCellDataCol, // Add colno to <th data-col> for header. Automatic if onheadercellclick is set.
                  toBodyRowData,       // Add rowno to <tr data-row> for body.
                  toBodyCellDataRow,   // Add rowno to <th data-row> for body. Automatic if oncellclick is set.
                  toBodyCellDataCol,   // Add colno to <th data-col> for body. Automatic if oncellclick is set.
                  tofooterRowData,     // Add rowno to <tr data-row> for footer
                  tofooterCellDataRow, // Add rowno to <th data-row> for footer. Automatic if onfootercellclick is set.
                  tofooterCellDataCol  // Add colno to <th data-col> for footer. Automatic if onfootercellclick is set.
                  );
  TTableOptions = Set of TTableOption;

  TRowKind = (rkHeader,rkBody,rkFooter);

Type
  TCustomTableWidget = Class;

  // Constructed only once when rendering !
  { TTableWidgetCelldata }
  TTableWidgetCellData = Class
  private
    FAsHTML: Boolean;
    FClassNames: String;
    FCol: Integer;
    FColumn: TCustomTableColumn;
    FContent: TJSHTMLElement;
    FKind: TRowKind;
    FRow: Integer;
    FTable: TCustomTableWidget;
    FTableID: String;
    FTag: String;
    FText: String;
    FWidget: TWebWidget;
  Protected
    Procedure SetRowColKind(aRow,aCol : Integer; aKind : TRowKind); virtual;
    Procedure Reset; // do not reset row,col, column
  Public
    Constructor Create(aTable : TCustomTableWidget;aTableID : String); virtual;
    Property Table : TCustomTableWidget Read FTable;
    Property Column : TCustomTableColumn Read FColumn Write FColumn;
    Property Row : Integer Read FRow;
    Property Col : Integer Read FCol;
    Property Kind : TRowKind Read FKind;
    Property Tag : String Read FTag Write FTag;
    Property ClassNames : String Read FClassNames Write FClassNames;
    Property Text : String Read FText Write FText;
    Property AsHTML : Boolean Read FAsHTML Write FAsHTML;
    Property Content : TJSHTMLElement Read FContent Write FContent;
    Property Widget : TWebWidget Read FWidget Write FWidget;
    Property TableID : String Read FTableID;
  end;


  TTableRowEnumerator = Class
  private
    FTable: TCustomTableWidget;
    FCurrent : Integer;
  public
    constructor Create(ATable : TCustomTableWidget); reintroduce; virtual;
    Procedure GetCellData(aCell : TTableWidgetCellData); virtual;
    function MoveNext: Boolean; virtual;
    property CurrentRow : Integer Read FCurrent;
    Property Table : TCustomTableWidget Read FTable;
  end;

  TTableRowCountEnumerator = Class (TTableRowEnumerator)
  private
    FRowCount: Integer;
  public
    constructor Create(ATable : TCustomTableWidget;aCount : Integer); reintroduce;
    function MoveNext: Boolean; override;
    Property RowCount : Integer read FRowCount;
  end;

  TOnCellDataEvent = Procedure (Sender : TObject; Enum : TTableRowEnumerator; aCell : TTableWidgetCellData) of object;

  TCustomTableWidget = Class(TCustomWebWidget)
  private
    FCaption: String;
    FColumns: TCustomTableColumns;
    FOnCellClick: THTMLNotifyEvent;
    FOnFooterCellClick: THTMLNotifyEvent;
    FOnFooterRowClick: THTMLNotifyEvent;
    FOnHeaderCellClick: THTMLNotifyEvent;
    FOnHeaderRowClick: THTMLNotifyEvent;
    FOnRowClick: THTMLNotifyEvent;
    FTableOptions: TTableOptions;
    FOnGetCellData : TOnCellDataEvent;
    FWidgets : Array of TWebWidget;
    FUpdateCount : Integer;
    procedure SetCaption(AValue: String);
    procedure SetColumns(AValue: TCustomTableColumns);
    procedure SetTableOptions(AValue: TTableOptions);
  Protected
    procedure AppendCaption(aCaptionElement: TJSHTMLElement); virtual;
    procedure RenderData(aElement: TJSHTMLElement); virtual;
    function DoCellClick(aEvent: TJSMouseEvent): boolean; virtual;
    function DoHeaderCellClick(aEvent: TJSMouseEvent): boolean;virtual;
    function DoFooterCellClick(aEvent: TJSMouseEvent): boolean;virtual;
    function DoRowClick(aEvent: TJSMouseEvent): boolean; virtual;
    function DoHeaderRowClick(aEvent: TJSMouseEvent): boolean;virtual;
    function DoFooterRowClick(aEvent: TJSMouseEvent): boolean;virtual;
    function CreateColumns: TCustomTableColumns; virtual;
    Function DefaultTableOptions: TTableOptions; virtual;
    Procedure CreateDefaultColumns; virtual;
    Function GetRowEnumerator(aKind : TRowKind) : TTableRowEnumerator; virtual;
    function RenderCell(aCell: TTableWidgetCellData): TJSHTMLElement; virtual;
    procedure RenderRow(aEnum : TTableRowEnumerator; aParent: TJSHTMLElement; aKind: TRowKind; aCell: TTableWidgetCellData); virtual;
    procedure RenderRows(aParent: TJSHTMLElement; aKind : TRowKind; aCell: TTableWidgetCellData); virtual;
    Procedure ApplyWidgetSettings(aElement : TJSHTMLElement); override;
    Function HTMLTag : String; override;
    Function CreateCellData(const aTableID : String) : TTableWidgetCellData; virtual;
    Function GetBodyRowEnumerator : TTableRowEnumerator; virtual; abstract;
  Protected
    // These can be made public/published
    Property CustomColumns : TCustomTableColumns Read FColumns Write SetColumns;
    Property TableOptions : TTableOptions read FTableOptions write SetTableOptions;
    Property Caption : String Read FCaption Write SetCaption;
    Property OnGetCellData : TOnCellDataEvent Read FOnGetCellData Write FOnGetCellData;
    Property OnCellClick :  THTMLNotifyEvent Read FOnCellClick Write FOnCellClick;
    Property OnHeaderCellClick :  THTMLNotifyEvent Read FOnHeaderCellClick Write FOnHeaderCellClick;
    Property OnFooterCellClick :  THTMLNotifyEvent Read FOnFooterCellClick Write FOnFooterCellClick;
    Property OnRowClick :  THTMLNotifyEvent Read FOnRowClick Write FOnRowClick;
    Property OnHeaderRowClick :  THTMLNotifyEvent Read FOnHeaderRowClick Write FOnHeaderRowClick;
    Property OnFooterRowClick :  THTMLNotifyEvent Read FOnFooterRowClick Write FOnFooterRowClick;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure BeginUpdate;
    Procedure EndUpdate;
    Procedure RefreshBody;
  end;

  { TEventTableWidget }

  TEventTableWidget = Class(TCustomTableWidget)
  private
    FRowCount: Integer;
    procedure SetRowCount(AValue: Integer);
  Protected
    Function GetBodyRowEnumerator : TTableRowEnumerator; override;
  Published
    Property RowCount : Integer Read FRowCount Write SetRowCount;
    Property CustomColumns;
    Property TableOptions;
    Property Caption;
    Property OnGetCellData;
    Property OnCellClick;
    Property OnHeaderCellClick;
    Property OnFooterCellClick;
    Property OnRowClick;
    Property OnHeaderRowClick;
    Property OnFooterRowClick;
  end;

  { TCustomStringsTableWidget }

  TCustomStringsTableWidget = Class(TCustomTableWidget)
  private
    Type
      TRow = Array of string;
  private
    FRows : Array of TRow;
    function GetRowCount: Integer;
    procedure SetRowCount(AValue: Integer);
  Protected
    Type
      TStringRowsEnumerator = Class(TTableRowCountEnumerator)
        Procedure GetCellData(aCell : TTableWidgetCellData); override;
      end;
  Protected
    Procedure CheckIndex(aCol,aRow : Integer);
    function GetCell(aCol, aRow : integer): String;
    procedure SetCell(aCol, aRow : integer; AValue: String);
    Function GetBodyRowEnumerator : TTableRowEnumerator; override;
  Public
    Property RowCount : Integer Read GetRowCount Write SetRowCount;
    Property Cells[aCol,aRow : integer] : String Read GetCell Write SetCell;
    Property CustomColumns;
    Property TableOptions;
    Property Caption;
    Property OnGetCellData;
    Property OnCellClick;
    Property OnHeaderCellClick;
    Property OnFooterCellClick;
    Property OnRowClick;
    Property OnHeaderRowClick;
    Property OnFooterRowClick;
  end;

  TStringsTableWidget = Class(TCustomStringsTableWidget)
  Published
    Property RowCount;
    Property CustomColumns;
    Property TableOptions;
    Property Caption;
    Property OnGetCellData;
    Property OnCellClick;
    Property OnHeaderCellClick;
    Property OnFooterCellClick;
    Property OnRowClick;
    Property OnHeaderRowClick;
    Property OnFooterRowClick;
  end;

  { TDivWidget }
  THTMLElementTag = (
      etUnknown, eta, etabbr, etacronym, etaddress, etapplet, etarea, etb, etbase,
      etbasefont, etbdo, etbig, etblockquote, etbody, etbr, etbutton,
      etcaption, etcenter, etcite, etcode, etcol, etcolgroup, etdd, etdel,
      etdfn, etdir, etdiv, etdl, etdt, etem, etfieldset, etfont, etform,
      etframe, etframeset, eth1, eth2, eth3, eth4, eth5, eth6, ethead, ethr,
      ethtml, eti, etiframe, etimg, etinput, etins, etisindex, etkbd, etlabel,
      etlegend, etli, etlink, etmap, etmenu, etmeta, etnoframes, etnoscript,
      etobject, etol, etoptgroup, etoption, etp, etparam, etpre, etq, ets,
      etsamp, etscript, etselect, etsmall, etspan, etstrike, etstrong,
      etstyle, etsub, etsup, ettable, ettbody, ettd, ettextarea, ettfoot,
      etth, etthead, ettitle, ettr, ettt, etu, etul, etvar,
      etText,etAudio,etVideo,etSource
  );
  THTMLElementTagSet = set of THTMLElementTag;

  { TCustomTagWidget }

  TCustomTagWidget = Class(TWebWidget)
  private
    FElementTag: THTMLElementTag;
    FTextContent: String;
    procedure SetElementTag(AValue: THTMLElementTag);
    procedure SetTextContent(AValue: String);
  Protected
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
    Function HTMLTag : String; override;
    // Set tag you wish to use
    Property elementTag : THTMLElementTag Read FElementTag Write SetElementTag;
    // If set, the text will be set as InnerText of the tag
    Property TextContent : String Read FTextContent Write SetTextContent;
  end;

  { TTagWidget }

  TTagWidget = Class(TCustomTagWidget)
  Public
    Constructor Create(aOwner : TComponent); override;
  Published
    Property elementTag;
    Property TextContent;
  end;

  TDivWidget = Class(TCustomTagWidget)
  Public
    Constructor Create(aOwner : TComponent); override;
  end;

  { TParagraphWidget }

  TParagraphWidget = Class(TCustomTagWidget)
  Public
    Constructor Create(aOwner : TComponent); override;
  end;

  { TMediaWidget }

  TMediaWidget = Class(TCustomTagWidget)
  private
    Const
      MaxAttrs = 20;
      PropAttrs : Array[0..MaxAttrs] of string
         = ('src','defaultPlaybackRate','duration','playbackRate','ended', // 0..4
            'paused','seeking','sinkId','mediaGroup','currentSrc', // 5..9
            'volume','controls','autoplay','crossOrigin', 'defaultMuted', // 10..14
            'currentTime', 'disableRemotePlayback', 'preservesPitch','loop','muted', // 15..19
            'preload' // 20
            );

    function GetAudioTrack: TJSHTMLAudioTrackList;
    function getBool(AIndex: Integer): Boolean;
    function GetError: TJSMEdiaError;
    function getFloat(AIndex: Integer): Double;
    function GetSrcObj: TJSHTMLMediaStream;
    function getString(AIndex: Integer): String;
    function GetTextTrack: TJSHTMLTextTrackList;
    function GetVideoTrack: TJSHTMLVideoTrackList;
    procedure SetBool(AIndex: Integer; AValue: Boolean);
    procedure SetFloat(AIndex: Integer; AValue: Double);
    procedure SetString(AIndex: Integer; AValue: String);
    function getEl: TJSObject;
  Public
    Property DefaultPlayBackRate : Double Index 1 Read getFloat;
    Property Duration : Double Index 2 Read getFloat;
    Property PlayBackRate : Double Index 3 Read getFloat;
    Property Ended : Boolean Index 4 Read getBool;
    Property Paused : Boolean Index 5 Read getBool;
    Property Seeking : Boolean Index 6 Read getBool;
    Property SinkID : String Index 7 Read getString Write SetString;
    Property MediaGroup : String Index 8 Read getString Write SetString;
    Property SrcObject : TJSHTMLMediaStream Read GetSrcObj;
    Property textTracks : TJSHTMLTextTrackList Read GetTextTrack;
    Property videoTracks : TJSHTMLVideoTrackList Read GetVideoTrack;
    Property audioTracks : TJSHTMLAudioTrackList Read GetAudioTrack;
    Property Error : TJSMEdiaError Read GetError;
    Property CurrentSrc : String Index 9 Read getString;
  Published
    Property Src : String Index 0 Read getString Write SetString;
    Property Controls : Boolean Index 11 Read getBool Write SetBool;
    Property AutoPlay : Boolean Index 12 Read getBool Write SetBool;
    Property CrossOrigin : String index 13 Read getString Write SetString;
    Property DefaultMuted : Boolean Index 14 Read getBool Write SetBool;
    Property CurrentTime : Double Index 15 Read getFloat Write SetFloat;
    Property DisableRemotePlayback : Boolean Index 16 Read getBool Write SetBool;
    Property PreservesPitch : Boolean Index 17 Read getBool Write SetBool;
    Property Loop : Boolean Index 18 Read getBool Write SetBool;
    Property Muted : Boolean Index 19 Read getBool Write SetBool;
    Property Preload : String Index 20 Read getString Write SetString;
    Property Volume : Double Index 10 Read getFloat Write SetFloat;
  end;

  { TVideoWidget }

  TVideoWidget = Class(TMediaWidget)
  Public
    Constructor Create(aOwner : TComponent); override;
  end;

  { TAudioWidget }

  TAudioWidget = Class(TMediaWidget)
  Public
    Constructor Create(aOwner : TComponent); override;
  end;


Function ViewPort : TViewPort;

Const
  TextTagNames : Array[TTextTag] of string
   = ('p','b','i','u','s','span','quote','blockquote','h1','h2','h3','h4','h5','h6','pre','ruby','article','address','abbr','');
  RowKindNames  : Array[TRowKind] of string = ('header','body','footer');

  HTMLTagNames : Array[THTMLElementTag] of string = (
    '?', 'a', 'abbr', 'acronym', 'address', 'applet', 'area', 'b', 'base',
    'basefont', 'bdo', 'big', 'blockquote', 'body', 'br', 'button',
    'caption', 'center', 'cite', 'code', 'col', 'colgroup', 'dd', 'del',
    'dfn', 'dir', 'div', 'dl', 'dt', 'em', 'fieldset', 'font', 'form',
    'frame', 'frameset', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'head', 'hr',
    'html', 'i', 'iframe', 'img', 'input', 'ins', 'isindex', 'kbd', 'label',
    'legend', 'li', 'link', 'map', 'menu', 'meta', 'noframes', 'noscript',
    'object', 'ol', 'optgroup', 'option', 'p', 'param', 'pre', 'q', 's',
    'samp', 'script', 'select', 'small', 'span', 'strike', 'strong',
    'style', 'sub', 'sup', 'table', 'tbody', 'td', 'textarea', 'tfoot',
    'th', 'thead', 'title', 'tr', 'tt', 'u', 'ul', 'var',
    'Text','Audio','Video','Source'
  );


implementation

uses DateUtils;

resourcestring
  SErrInvalidIndex = 'Index %d not in valid range of [0..%d]';
  SErrInvalidRowCount = 'Invalid row count: %d';
  SRow = 'Row';
  SCol = 'Column';

Function ViewPort : TViewPort;

begin
  Result:=TViewPort.Instance;
end;

{ TTableRowCountEnumerator }

Const
  CellTags : Array[TRowKind] of string = ('th','td','td');

{ TEventTableWidget }

procedure TEventTableWidget.SetRowCount(AValue: Integer);
begin
  if FRowCount=AValue then Exit;
  BeginUpdate;
  FRowCount:=AValue;
  EndUpdate;
end;

function TEventTableWidget.GetBodyRowEnumerator: TTableRowEnumerator;
begin
  Result:=TTableRowCountEnumerator.Create(Self,RowCount);
end;

{ TCustomStringsTableWidget.TStringRowsEnumerator }

procedure TCustomStringsTableWidget.TStringRowsEnumerator.GetCellData(aCell: TTableWidgetCellData);
begin
  aCell.Text:=TCustomStringsTableWidget(Table).Cells[aCell.Col,aCell.Row];
end;

{ TCustomStringsTableWidget }

function TCustomStringsTableWidget.GetCell(aCol, aRow : integer): String;
begin
  CheckIndex(aCol,aRow);
  Result:=FRows[aRow][aCol];
end;

function TCustomStringsTableWidget.GetRowCount: Integer;
begin
  Result:=Length(FRows);
end;

procedure TCustomStringsTableWidget.SetCell(aCol, aRow : integer; AValue: String);
begin
  CheckIndex(aCol,aRow);
  BeginUpdate;
  try
    FRows[aRow][aCol]:=AValue;
  Finally
    EndUpdate;
  end;
end;

function TCustomStringsTableWidget.GetBodyRowEnumerator: TTableRowEnumerator;
begin
  Result:=TStringRowsEnumerator.Create(Self,RowCount);
end;

procedure TCustomStringsTableWidget.SetRowCount(AValue: Integer);
begin
  if AValue<0 then
     raise EWidgets.CreateFmt(SerrInvalidRowCount, [aValue]);
  BeginUpdate;
  try
    SetLength(FRows,aValue);
  Finally
    EndUpdate;
  end;
end;

procedure TCustomStringsTableWidget.CheckIndex(aCol, aRow: Integer);
begin
  If (aCol<0) or (aCol>=CustomColumns.Count) then
    Raise EWidgets.CreateFmt(SCol+' '+SErrInvalidIndex,[aCol,0,CustomColumns.Count-1]);
  If (aRow<0) or (aRow>=RowCount) then
    Raise EWidgets.CreateFmt(SRow+' '+SErrInvalidIndex,[aRow,0,RowCount-1]);
end;

{ TTagWidget }

constructor TTagWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  ElementTag:=etdiv;
end;

{ TMediaWidget }

function TMediaWidget.GetAudioTrack: TJSHTMLAudioTrackList;

begin
  if Assigned(Element) then
    Result:=TJSHTMLMediaElement(Element).AudioTracks
  else
    Result:=Nil;
end;

function TMediaWidget.getBool(AIndex: Integer): Boolean;

Var
  El : TJSObject;
  Att : String;

begin
  El:=GetEl;
  Att:=PropAttrs[aIndex];
  Result:=Assigned(el) and isDefined(El[Att]) and (Boolean(El[Att]));
end;

function TMediaWidget.GetError: TJSMEdiaError;
begin
  If Assigned(Element) then
    Result:=TJSHTMLMediaElement(Element).Error
  else
    Result:=Nil;
end;

function TMediaWidget.getFloat(AIndex: Integer): Double;

Var
  El : TJSObject;
  Att : String;

begin
  El:=GetEl;
  Att:=PropAttrs[aIndex];
  if Assigned(el) and isDefined(El[Att]) then
    Result:=Double(El[Att])
  else
    Result:=0;
end;

function TMediaWidget.GetSrcObj: TJSHTMLMediaStream;
begin
  If Assigned(Element) then
    Result:=TJSHTMLMediaElement(Element).srcObject
  else
    Result:=Nil;
end;

function TMediaWidget.getString(AIndex: Integer): String;

Var
  El : TJSObject;
  Att : String;

begin
  El:=GetEl;
  Att:=PropAttrs[aIndex];
  if Assigned(el) and isDefined(El[Att]) then
    Result:=String(El[Att])
  else
    Result:='';
end;


function TMediaWidget.GetTextTrack: TJSHTMLTextTrackList;
begin
  If Assigned(Element) then
    Result:=TJSHTMLMediaElement(Element).TextTracks
  else
    Result:=Nil;
end;

function TMediaWidget.GetVideoTrack: TJSHTMLVideoTrackList;
begin
  If Assigned(Element) then
    Result:=TJSHTMLMediaElement(Element).VideoTracks
  else
    Result:=Nil;
end;

procedure TMediaWidget.SetBool(AIndex: Integer; AValue: Boolean);
Var
  El : TJSObject;
  Att : String;
begin
  El:=GetEl;
  Att:=PropAttrs[aIndex];
  if Assigned(el) then
    El[Att]:=aValue
  else
    Attrs[Att]:=IntToStr(Ord(AValue));
end;

procedure TMediaWidget.SetFloat(AIndex: Integer; AValue: Double);
Var
  El : TJSObject;
  Att,S : String;
begin
  El:=GetEl;
  Att:=PropAttrs[aIndex];
  if Assigned(el) then
    El[Att]:=aValue
  else
    begin
    Str(aValue,S);
    Attrs[Att]:=S;
    end;
end;

procedure TMediaWidget.SetString(AIndex: Integer; AValue: String);

Var
  El : TJSObject;
  Att : String;
begin
  El:=GetEl;
  Att:=PropAttrs[aIndex];
  if Assigned(el) then
    El[Att]:=aValue
  else
    Attrs[Att]:=aValue;
end;

function TMediaWidget.getEl: TJSObject;
begin
  Result:=Element;
  if Not Assigned(Result) then
    Result:=Self.StoredAttrs;
end;

{ TVideoWidget }

constructor TVideoWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  elementTag:=etVideo;
end;

{ TAudioWidget }

constructor TAudioWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  elementTag:=etAudio;
end;


{ TSelectWidget.TStringsSelectOptionEnumerator }

constructor TSelectWidget.TStringsSelectOptionEnumerator.Create(ASelect: TCustomSelectWidget);
begin
  inherited Create(ASelect);
  FCurrent:=-1;
end;

function TSelectWidget.TStringsSelectOptionEnumerator.OptionText: String;
begin
  Result:=TSelectWidget(Select).Items[FCurrent];
end;

function TSelectWidget.TStringsSelectOptionEnumerator.HasValue: boolean;
begin
  Result:=FCurrent<TSelectWidget(Select).Values.Count;
end;

function TSelectWidget.TStringsSelectOptionEnumerator.Value: string;
begin
  Result:=TSelectWidget(Select).Values[FCurrent];
end;

function TSelectWidget.TStringsSelectOptionEnumerator.MoveNext: Boolean;
begin
  Result:=FCurrent<TSelectWidget(Select).Items.Count-1;
  if Result then
    Inc(FCurrent);
end;

{ TCustomSelectWidget.TSelectOptionEnumerator }

constructor TCustomSelectWidget.TSelectOptionEnumerator.Create(ASelect: TCustomSelectWidget);
begin
  FSelect:=ASelect;
end;

function TCustomSelectWidget.TSelectOptionEnumerator.HasValue: boolean;
begin
  Result:=False;
end;

function TCustomSelectWidget.TSelectOptionEnumerator.Value: string;
begin
  Result:='';
end;

constructor TTableRowCountEnumerator.Create(ATable: TCustomTableWidget; aCount: Integer);
begin
  Inherited Create(aTable);
  FRowCount:=aCount;
end;

function TTableRowCountEnumerator.MoveNext: Boolean;
begin
  Result:=Inherited MoveNext and (CurrentRow<RowCount)
end;

{ TTableWidgetCellData }

procedure TTableWidgetCellData.SetRowColKind(aRow, aCol: Integer; aKind: TRowKind);
begin
  if (aRow<>-1) then
    FRow:=aRow;
  if (aCol<>-1) then
    FCol:=aCol;
  FKind:=aKind;
end;

procedure TTableWidgetCellData.Reset;
begin
  Ftag:='td';
  FClassNames:='';
  FText:='';
  FContent:=Nil;
  FAsHTML:=False;
  FWidget:=Nil;
end;

constructor TTableWidgetCellData.Create(aTable: TCustomTableWidget; aTableID: String);
begin
  FTable:=aTable;
  FTableID:=aTableID;
  SetRowColKind(0,0,rkBody);
end;

{ TCustomTableWidget }

procedure TCustomTableWidget.SetColumns(AValue: TCustomTableColumns);
begin
  if FColumns=AValue then Exit;
  FColumns.Assign(AValue);
end;

procedure TCustomTableWidget.SetCaption(AValue: String);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  if isRendered then Refresh;
end;

function TCustomTableWidget.DoCellClick(aEvent: TJSMouseEvent): boolean;
begin
  If Assigned(FOnCellClick) then
    FOnCellClick(Self,aEvent);
  If Assigned(FOnRowClick) then
    FOnRowClick(Self,aEvent);
  Result:=False;
//  Writeln('On click for cell',aEvent.targetElement.innerText);
end;

function TCustomTableWidget.DoHeaderCellClick(aEvent: TJSMouseEvent): boolean;
begin
  If Assigned(FOnHeaderCellClick) then
    FOnHeaderCellClick(Self,aEvent);
  If Assigned(FOnHeaderRowClick) then
    FOnHeaderRowClick(Self,aEvent);
  Result:=False;
//  Writeln('On click for header cell',aEvent.targetElement.innerText);
end;

function TCustomTableWidget.DoFooterCellClick(aEvent: TJSMouseEvent): boolean;
begin
  If Assigned(FOnFooterCellClick) then
    FOnFooterCellClick(Self,aEvent);
  If Assigned(FOnFooterRowClick) then
    FOnFooterRowClick(Self,aEvent);
  Result:=False;
//  Writeln('On click for Footer cell',aEvent.targetElement.innerText);
end;

function TCustomTableWidget.DoRowClick(aEvent: TJSMouseEvent): boolean;
begin
  If Assigned(FOnRowClick) then
    FOnRowClick(Self,aEvent);
  Result:=False;
//  Writeln('On click for Row',aEvent.targetElement.innerText);
end;

function TCustomTableWidget.DoHeaderRowClick(aEvent: TJSMouseEvent): boolean;
begin
  If Assigned(FOnHeaderRowClick) then
    FOnHeaderRowClick(Self,aEvent);
  Result:=False;
//  Writeln('On click for Header Row',aEvent.targetElement.innerText);
end;

function TCustomTableWidget.DoFooterRowClick(aEvent: TJSMouseEvent): boolean;
begin
  If Assigned(FOnFooterRowClick) then
    FOnFooterRowClick(Self,aEvent);
  Result:=False;
//  Writeln('On click for Footer Row',aEvent.targetElement.innerText);
end;

procedure TCustomTableWidget.SetTableOptions(AValue: TTableOptions);
begin
  if FTableOptions=AValue then Exit;
  FTableOptions:=AValue;
  if IsRendered then
    Refresh;
end;

procedure TCustomTableWidget.AppendCaption(aCaptionElement: TJSHTMLElement);
begin
  aCaptionElement.InnerHTML:=Caption;
end;

function TCustomTableWidget.GetRowEnumerator(aKind: TRowKind): TTableRowEnumerator;
begin
  Case aKind of
    rkHeader : Result:=TTableRowCountEnumerator.Create(Self,1);
    rkFooter : Result:=TTableRowCountEnumerator.Create(Self,1);
    rkBody : Result:=GetBodyRowEnumerator;
  end;
end;

procedure TTableRowEnumerator.GetCellData(aCell: TTableWidgetCellData);

Var
  K : TRowKind;

begin
  K:=aCell.Kind;
  Case K of
    rkHeader:
      begin
      aCell.Tag:='th';
      aCell.Text:=ACell.Column.Caption;
      end;
    rkFooter,
    rkBody :
      begin
      aCell.Tag:='td';
      end;
    end;
end;

function TCustomTableWidget.HTMLTag: String;
begin
  Result:='table';
end;

function TCustomTableWidget.RenderCell(aCell: TTableWidgetCellData): TJSHTMLElement;

Const
  Aligns : Array[TAlignment] of string = ('left','right','center');
  RowChecks : Array[TRowKind] of TTableOption = (toHeaderCellDataRow,toBodyCellDataRow,toFooterCellDataRow);
  ColChecks : Array[TRowKind] of TTableOption = (toHeaderCellDataCol,toBodyCellDataCol,toFooterCellDataCol);

Var
  C : TJSHtmlElement;
  cl : THTMLNotifyEvent;
  K : TRowKind;
  M : THTMLClickEventHandler;
  elID : string;
begin
  K:=aCell.Kind;
  if (toCellID in TableOptions) or Assigned(aCell.Widget) then
    elID:=aCell.TableID+'-'+RowKindNames[K]+'-'+IntToStr(ACell.Row)+'-'+IntToStr(aCell.Col)
  else
    elID:='';
  C:=CreateElement(aCell.Tag,elID);
  if aCell.Widget<>Nil then
    aCell.Widget.ParentID:=elID;
  if aCell.Content<>Nil then
    C.AppendChild(aCell.Content)
  else if aCell.AsHTML then
    C.innerHTML:=aCell.text
  else
    C.innerText:=aCell.text;
  C.className:=AddClasses(aCell.Column.ClassNames,aCell.ClassNames);
  if ACell.Column.Alignment<>taLeftJustify then
    C.Style.setProperty('text-align',Aligns[ACell.Column.Alignment]);
  Case K of
    rkBody   :
      begin
      CL:=FOnCellClick;
      M:=@DoCellClick;
      end;
    rkHeader :
      begin
      CL:=FOnHeaderCellClick;
      M:=@DoHeaderCellClick;
      end;
    rkFooter :
      begin
      CL:=FOnFooterCellClick;
      M:=@DoFooterCellClick;
      end;
  else
    CL:=Nil;
    M:=nil;
  end;
  if Assigned(cl) or (RowChecks[K] in TableOptions) then
     begin
     C.dataset['row']:=ACell.Row;
     C.Dataset['kind']:=RowKindNames[K];
     end;
  if Assigned(cl) or (ColChecks[K] in TableOptions) then
     begin
     C.dataset['col']:=ACell.Col;
     C.Dataset['kind']:=RowKindNames[K];
     end;
  if Assigned(M) then
    C.OnClick:=M;
  if aCell.Widget<>Nil then
    TJSArray(FWidgets).Push(aCell.Widget);
  Result:=C;
end;

procedure TCustomTableWidget.RenderRow(aEnum: TTableRowEnumerator; aParent: TJSHTMLElement; aKind: TRowKind; aCell: TTableWidgetCellData);


Var
  I: integer;

begin
  For I:=0 to CustomColumns.Count-1 do
    if CustomColumns[i].RenderColumn then
      begin
      aCell.Reset;
      aCell.FColumn:=CustomColumns[i];
      aCell.SetRowColKind(-1,I,aKind);
  //    Writeln(CellKinds[aKind],' cell before : ',aCell.Tag,' data : ',aCell.Text);
      aEnum.GetCellData(aCell);
  //    Writeln(CellKinds[aKind],' cell after : ',aCell.Tag,' data : ',aCell.Text);
      if aCell.Tag='' then
        ACell.Tag:=CellTags[aKind];
      if Assigned(FOnGetCellData) then
        FOnGetCellData(Self,aEnum,aCell);
      aParent.appendChild(RenderCell(aCell));
      end;
end;

procedure TCustomTableWidget.RenderRows(aParent: TJSHTMLElement; aKind: TRowKind; aCell: TTableWidgetCellData);

Const
  TableRowChecks : Array[TRowKind] of TTableOption = (toHeaderRowData,toBodyRowData,toFooterRowData);

Var
  RowEl : TJSHTMLElement;
  Enum : TTableRowEnumerator;
  M : THTMLClickEventHandler;
  cl : THTMLNotifyEvent;
  elid : String;

begin
   Enum:=GetRowEnumerator(aKind);
   if Enum=Nil then
     Exit;
   try
     While Enum.MoveNext do
       begin
       if toRowID in TableOptions then
         elID:=aCell.TableID+'-'+RowKindNames[aKind]+'-'+IntToStr(Enum.CurrentRow)
       else
         elID:='';
       RowEl:=CreateElement('tr',elID);
       aCell.SetRowColKind(Enum.CurrentRow,-1,aKind);
       Case aKind of
         rkBody :
           begin
           CL:=FOnRowClick;
           M:=@DoRowClick;
           end;
         rkHeader :
           begin
           CL:=FOnHeaderRowClick;
           M:=@DoHeaderRowClick;
           end;
         rkFooter :
           begin
           CL:=FOnFooterRowClick;
           M:=@DoFooterRowClick;
           end;
       end;
       if Assigned(CL) or (TableRowChecks[Akind] in TableOptions) then
         begin
         RowEl.dataset['row']:=Enum.CurrentRow;
         RowEl.dataset['kind']:=RowKindNames[aKind];
         end;
       if Assigned(M) then
         RowEl.OnClick:=M;
       RenderRow(Enum,RowEl,aKind,aCell);
       aParent.AppendChild(RowEl);
       end;
   finally
     Enum.Free;
   end;
end;

procedure TCustomTableWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);
begin
  inherited ApplyWidgetSettings(aElement);
  RenderData(aElement);
end;

procedure TCustomTableWidget.RenderData(aElement: TJSHTMLElement);

Var
  El : TJSHTMLElement;
  aCell : TTableWidgetCellData;
  W : TWebWidget;

begin
  FWidgets:=[];
  if (Caption<>'') then
    begin
    El:=CreateElement('caption',aElement.ID+'-caption');
    AppendCaption(EL);
    aElement.AppendChild(EL);
    end;
  aCell:=CreateCellData(aElement.ID);
  If (CustomColumns.Count=0) then
    CreateDefaultColumns;
  if toHeaderRow in TableOptions then
    begin
    if toHeader in TableOptions then
      begin
      El:=CreateElement('thead',aElement.ID+'-head');
      aElement.AppendChild(el);
      end
    else
      El:=aElement;
    aCell.SetRowColKind(-1,-1,rkHeader);
    RenderRows(El,rkHeader,aCell);
    end;
  if toBody in TableOptions then
    begin
    El:=CreateElement('tbody',aElement.ID+'-body');
    aElement.AppendChild(el);
    end
  else
    El:=aElement;
  aCell.SetRowColKind(-1,-1,rkBody);
  RenderRows(El,rkBody,aCell);
  if toFooterRow in TableOptions then
    begin
    if toFooter in TableOptions then
      begin
      El:=CreateElement('tFoot',aElement.ID+'-foot');
      aElement.AppendChild(el);
      end
    else
      El:=aElement;
    aCell.SetRowColKind(-1,-1,rkFooter);
    RenderRows(El,rkFooter,aCell);
    end;
  for W in FWidgets do
    W.Refresh;
  FWidgets:=[];
end;

function TCustomTableWidget.CreateCellData(const aTableID : String): TTableWidgetCellData;
begin
  Result:=TTableWidgetCellData.Create(Self,aTableID);
end;


function TCustomTableWidget.CreateColumns: TCustomTableColumns;
begin
  Result:=TCustomTableColumns.Create(TCustomTableColumn);
end;

function TCustomTableWidget.DefaultTableOptions: TTableOptions;
begin
  Result:=[toHeader,toBody,toFooter,toHeaderRow]
end;

procedure TCustomTableWidget.CreateDefaultColumns;
begin
  // Do nothing
end;

constructor TCustomTableWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FTableOptions:=DefaultTableOptions;
  FColumns:=CreateColumns;
end;

destructor TCustomTableWidget.Destroy;
begin
  FreeAndNil(FColumns);
  inherited Destroy;
end;

procedure TCustomTableWidget.BeginUpdate;
begin
  Inc(FUpDateCount);
end;

procedure TCustomTableWidget.EndUpdate;
begin
  if (FUpdateCount>0) then
    Dec(FUpDateCount);
  if (FUpdateCount=0) and IsRendered then
    Refresh;
end;

procedure TCustomTableWidget.RefreshBody;
begin
  if Not Assigned(Element) then
    Refresh
  else
    begin
    Element.Innerhtml:='';
    RenderData(Element);
    end;
end;

{ TCustomTableColumn }

procedure TCustomTableColumn.SetAlignment(AValue: TAlignment);
begin
  if FAlignment=AValue then Exit;
  FAlignment:=AValue;
end;

function TCustomTableColumn.GetCaption: String;
begin
  Result:=FCaption;
end;

procedure TCustomTableColumn.SetCaption(AValue: String);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
end;

procedure TCustomTableColumn.SetClassNames(AValue: String);
begin
  if FClassNames=AValue then Exit;
  FClassNames:=AValue;
end;

function TCustomTableColumn.RenderColumn: Boolean;
begin
  Result:=True;
end;

function TCustomTableColumn.GetDisplayName: string;
begin
  Result:=Caption;
end;

procedure TCustomTableColumn.Assign(Source: TPersistent);

Var
  C : TCustomTableColumn;

begin
  if Source is TCustomTableColumn then
    begin
    C:=Source as TCustomTableColumn;
    FCaption:=C.FCaption;
    FClassNames:=C.FClassNames;
    FAlignment:=C.Alignment;
    end
  else
    inherited Assign(Source);
end;

{ TCustomTableColumns }

function TCustomTableColumns.GetCustomColumn(Index : Integer): TCustomTableColumn;
begin
  Result:=TCustomTableColumn(Items[Index]);
end;

procedure TCustomTableColumns.SetCustomColumn(Index : Integer; AValue: TCustomTableColumn);
begin
  Items[Index]:=aValue;
end;

function TCustomTableColumns.Add(aCaption: String): TCustomTableColumn;
begin
  Result:=add as TCustomTableColumn;
  Result.Caption:=aCaption;
end;

{ TTableRowEnumerator }

constructor TTableRowEnumerator.Create(ATable: TCustomTableWidget);

begin
  FTable:=aTable;
  FCurrent:=-1;
end;


function TTableRowEnumerator.MoveNext: Boolean;
begin
  Inc(FCurrent);
  Result:=True;
end;

{ TCustomTextWidget }

procedure TCustomTextWidget.SetEnvelopeTag(AValue: TTextTag);
begin
  // Writeln('Setting text tag : ',aValue);
  if FEnvelopeTag=AValue then Exit;
  FEnvelopeTag:=AValue;
  if (FEnvelopeTag=ttCustom) and (FCustomTag='') then
    FCustomTag:='div';
  if IsRendered then
    Refresh;
end;

procedure TCustomTextWidget.SetCustomTag(AValue: String);
begin
  if FCustomTag=AValue then Exit;
  FCustomTag:=AValue;
  if (FCustomTag<>'') then
    FEnvelopeTag:=ttCustom;
  if IsRendered then
    Refresh;
end;


procedure TCustomTextWidget.SetTextMode(AValue: TTextMode);
begin
  if FTextMode=AValue then Exit;
  FTextMode:=AValue;
  if IsRendered then
    ApplyText(Element);
end;

procedure TCustomTextWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);
begin
  // Writeln('ApplyWidgetSettings: ',aElement.tagName);
  inherited ApplyWidgetSettings(aElement);
  ApplyText(aElement);
end;

procedure TCustomTextWidget.ApplyText(aElement: TJSHTMLElement);
begin
  if FTextMode=tmText then
    aElement.innerText:=GetText
  else
    aElement.innerHTML:=GetText;
end;

function TCustomTextWidget.HTMLTag: String;

begin
  Result:=TextTagNames[FEnvelopeTag];
  if Result='' then
    Result:='div';
  // Writeln('Getting element tag: ',Result);
end;

{ TTextLinesWidget }

procedure TTextLinesWidget.SetLines(AValue: TStrings);
begin
  if FLines=AValue then Exit;
  FLines.Assign(AValue);
end;

procedure TTextLinesWidget.SetForceLineBreaks(AValue: Boolean);
begin
  if FForceLineBreaks=AValue then Exit;
  FForceLineBreaks:=AValue;
  if IsRendered then
    ApplyText(Element);
end;

procedure TTextLinesWidget.DoLinesChanged(Sender: TObject);
begin
  if IsRendered then
    ApplyText(Element);
end;

function TTextLinesWidget.GetText: String;

Var
  I : integer;

begin
  if (FTextMode=tmHTML) and ForceLineBreaks then
    begin
    Result:='';
    For I:=0 to FLines.Count-1 do
      Result:=Result+flines[i]+'<br/>';
    end
  else
    Result:=FLines.Text;
end;

procedure TTextLinesWidget.ApplyText(aElement: TJSHTMLElement);

Var
  I : integer;

begin
  if (TextMode=tmHTML) or (Not ForceLineBreaks)  then
    inherited ApplyText(aElement)
  else
    begin
    For I:=0 to FLines.Count-1 do
      begin
      aElement.AppendChild(Document.createTextNode(FLines[i]));
      aElement.AppendChild(CreateElement('br',''));
      end;
    end;
end;

constructor TTextLinesWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FLines:=TstringList.Create;
  TstringList(FLines).OnChange:=@DoLinesChanged;
end;

destructor TTextLinesWidget.Destroy;
begin
  FLines:=TstringList.Create;
  inherited Destroy;
end;

{ TTextWidget }

procedure TTextWidget.SetText(AValue: String);
begin
  if FText=AValue then Exit;
  FText:=AValue;
  if IsRendered then
    ApplyText(Element);
end;

function TTextWidget.GetText: String;
begin
  Result:=FText;
end;



{ TLabelWidget }

procedure TLabelWidget.ApplyLabelFor(aLabelElement : TJSHTMLLabelElement);

begin
  if Assigned(FlabelFor) then
    begin
    FlabelFor.EnsureElement;
    aLabelElement.for_:=FlabelFor.ElementID;
    end
  else
    aLabelElement.for_:='';
end;

procedure TLabelWidget.SetLabelFor(AValue: TWebWidget);
begin
  if (FLabelFor=AValue) then Exit;
  if Assigned(FLabelFor) then
    FLabelFor.RemoveFreeNotification(Self);
  FLabelFor:=AValue;
  if Assigned(FLabelFor) then
    FLabelFor.FreeNotification(Self);
  If IsRendered then
    ApplyLabelFor(LabelElement);
end;

function TLabelWidget.GetText: String;
begin
  if IsElementDirty then
    FText:=Element.InnerText;
  Result:=FText;
end;

function TLabelWidget.GetLabelEl: TJSHTMLLabelElement;
begin
  Result:=TJSHTMLLabelElement(Element);
end;

procedure TLabelWidget.SetText(AValue: String);
begin
  If Text=aValue then exit;
  Ftext:=aValue;
  If IsRendered then
    Element.innerText:=aValue;
end;

procedure TLabelWidget.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (aComponent=FLabelFor) then
    FLabelFor:=Nil;
end;

procedure TLabelWidget.SetName(const NewName: TComponentName);

Var
  Old : String;

begin
  Old:=Name;
  inherited SetName(NewName);
  if (csDesigning in ComponentState) then
    if Old=Text then
      Text:=Old;
end;

procedure TLabelWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);

var
  lbl : TJSHTMLLabelElement absolute aElement;

begin
  inherited ApplyWidgetSettings(aElement);
  lbl.InnerText:=Text;
  ApplyLabelFor(Lbl);
end;


function TLabelWidget.HTMLTag: String;
begin
  Result:='label';
end;

{ TSelectWidget }

function TCustomSelectWidget.GetSelectedIndex: Integer;
begin
  if IsRendered then
    FSelectedIndex:=SelectElement.selectedIndex;
  Result:=FSelectedIndex
end;

function TCustomSelectWidget.GetMultiple: Boolean;

begin
  if IsElementDirty then
    FMultiple:=SelectElement.multiple;
  Result:=FMultiple;
end;

function TCustomSelectWidget.GetItemCount: Integer;
begin
  Result:=Length(Options);
end;

function TCustomSelectWidget.GetSelected(Index : Integer): Boolean;
begin
  if (Index<0) or (Index>=Length(Foptions)) then
     Raise EWidgets.CreateFmt(SErrInvalidIndex,[Index,Length(Foptions)-1]);
  Result:=FOptions[Index].Selected
end;

function TCustomSelectWidget.GetSelect: TJSHTMLSelectElement;
begin
  Result:=TJSHTMLSelectElement(Element);
end;

function TCustomSelectWidget.GetSelectionCount: Integer;
begin
  Result:=SelectElement.selectedOptions.length;
end;

function TCustomSelectWidget.GetSelectionItem(aIndex : Integer): String;
begin
  if (aIndex<0) or (aindex>=GetSelectionCount) then
     Raise EWidgets.CreateFmt(SErrInvalidIndex,[aIndex,GetSelectionCount-1]);
  Result:=TJSHTMLOptionElement(SelectElement.selectedOptions.item(aIndex)).innerText;
end;

function TCustomSelectWidget.GetSelectionValue(aIndex : Integer): String;
begin
  if (aIndex<0) or (aindex>=GetSelectionCount) then
     Raise EWidgets.CreateFmt(SErrInvalidIndex,[aIndex,GetSelectionCount-1]);
  Result:=TJSHTMLOptionElement(SelectElement.selectedOptions.item(aIndex)).value;
end;

function TCustomSelectWidget.GetSize: Integer;
begin
  if IsElementDirty then
    FSize:=SelectElement.Size;
  Result:=FSize;
end;

procedure TCustomSelectWidget.SetMultiple(AValue: Boolean);
begin
  If (AValue=Multiple) then exit;
  FMultiple:=aValue;
  If IsRendered then
    SelectElement.multiple:=FMultiple;
end;

procedure TCustomSelectWidget.SetSelected(Index : Integer; AValue: Boolean);
begin
  if (Index<0) or (Index>=Length(Foptions)) then
     Raise EWidgets.CreateFmt(SErrInvalidIndex,[Index,Length(Foptions)-1]);
  FOptions[Index].Selected:=aValue;
end;

procedure TCustomSelectWidget.SetSelectedIndex(AValue: Integer);

begin
  if (SelectedIndex=aValue) then
    Exit;
  FSelectedIndex:=aValue;
  if IsRendered then
    SelectElement.SelectedIndex:=FSelectedIndex;
  if Assigned(OnChange) then
    OnChange(Self,Nil);
end;

procedure TCustomSelectWidget.SetSize(AValue: Integer);
begin
  If (AValue=Size) then exit;
  FSize:=aValue;
  If IsRendered then
    SelectElement.Size:=FSize;
end;

procedure TCustomSelectWidget.BuildOptions(aSelect: TJSHTMLSelectElement);

Var
  O : TJSHTMLOptionElement;
  Idx : Integer;
  enum : TSelectOptionEnumerator;

begin
  // Clear
  SetLength(FOptions,0);
  aSelect.InnerHTML:='';
  // Rebuild
  Idx:=0;
  enum:=CreateOptionEnumerator;
  While enum.MoveNext do
    begin
    O:=TJSHTMLOptionElement(CreateElement('option',''));
    O.innerText:=enum.OptionText;
    if enum.HasValue then
      O.value:=enum.Value;
    if Idx=FSelectedIndex then
      O.selected:=True;
    aSelect.AppendChild(O);
    Inc(Idx);
    end;
  SetLength(Foptions,Idx);
  Dec(idx);
  While Idx>=0 do
    begin
    FOptions[Idx]:=TJSHTMLOptionElement(aSelect.Children[Idx]);
    dec(Idx);
    end;
end;

constructor TCustomSelectWidget.Create(aOWner: TComponent);
begin
  inherited Create(aOWner);
  FSelectedIndex:=-1;
end;

procedure TCustomSelectWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);

Var
  el : TJSHTmlSelectElement absolute aElement;

begin
  inherited ApplyWidgetSettings(aElement);
  el.multiple:=Self.Multiple;
  el.Size:=Self.Size;
  BuildOptions(el);
  // We need to force this.
  if SelectedIndex=-1 then
    el.selectedIndex:=-1;
end;


function TCustomSelectWidget.HTMLTag: String;
begin
  Result:='select';
end;


{ TSelectWidget }

function TSelectWidget.GetItems: TStrings;
begin
  Result:=FItems;
end;


function TSelectWidget.GetValues: TStrings;
begin
  Result:=FValues;
end;

procedure TSelectWidget.OptionsChanged(Sender: TObject);
begin
  if IsRendered then
    BuildOptions(SelectElement);
end;


procedure TSelectWidget.setItems(AValue: TStrings);
begin
  If (AValue=FItems) then exit;
  FItems.Assign(aValue);
end;


procedure TSelectWidget.setValues(AValue: TStrings);
begin
  If (AValue=FValues) then exit;
  FValues.Assign(aValue);
end;

function TSelectWidget.CreateOptionEnumerator: TSelectOptionEnumerator;
begin
  Result:=TStringsSelectOptionEnumerator.Create(Self);
end;


constructor TSelectWidget.Create(aOWner: TComponent);
begin
  inherited Create(aOWner);
  FItems:=TStringList.Create;
  TStringList(FItems).OnChange:=@OptionsChanged;
  FValues:=TStringList.Create;
  TStringList(FValues).OnChange:=@OptionsChanged;
end;

destructor TSelectWidget.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FValues);
  inherited Destroy;
end;

{ TImageWidget }

function TImageWidget.GetHeight: Integer;
begin
  if IsElementDirty then
    FHeight:=ImgElement.Height;
  Result:=Fheight;
end;

function TImageWidget.GetImg: TJSHTMLImageElement;
begin
  Result:=TJSHTMLImageElement(Element);
end;

function TImageWidget.GetSrc: String;
begin
  if IsElementDirty then
    FSrc:=ImgElement.Src;
  Result:=FSrc;
end;

function TImageWidget.GetWidth: Integer;
begin
  if IsElementDirty then
    FWidth:=ImgElement.Width;
  Result:=FWidth;
end;

procedure TImageWidget.SetHeight(AValue: Integer);
begin
  if AValue=Height then exit;
  FHeight:=AValue;
  If isrendered then
    ImgElement.Height:=aValue;
end;

procedure TImageWidget.SetSrc(AValue: String);
begin
  if AValue=Src then exit;
  FSrc:=AValue;
  If isrendered then
    ImgElement.Src:=FSrc;
end;

procedure TImageWidget.SetWidth(AValue: Integer);
begin
  if AValue=Width then exit;
  FWidth:=AValue;
  If isrendered then
    ImgElement.Width:=aValue;
end;

procedure TImageWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);

var
  img : TJSHTMLImageElement absolute aElement;

begin
  inherited ApplyWidgetSettings(aElement);
  Img.Src:=FSrc;
  Img.Height:=FHeight;
  Img.Width:=FWidth;
end;

function TImageWidget.HTMLTag: String;
begin
  Result:='img';
end;

{ TTextAreaWidget }

procedure TTextAreaWidget.SetLines(AValue: TStrings);
begin
  if FLines=AValue then Exit;
  FLines.Assign(AValue);
end;

procedure TTextAreaWidget.SetMaxLength(AValue: Cardinal);
begin
  if FMaxLength=AValue then Exit;
  FMaxLength:=AValue;
  if IsRendered then
    TextArea.maxLength:=aValue;
end;

procedure TTextAreaWidget.SetReadonly(AValue: Boolean);
begin
  If aValue=ReadOnly then exit;
  FReadOnly:=aValue;
  if IsRendered then
    TextArea.Readonly:=FReadOnly;
end;

procedure TTextAreaWidget.SetRequired(AValue: Boolean);
begin
  If aValue=Required then exit;
  FRequired:=aValue;
  if IsRendered then
    TextArea.Required:=FRequired;
end;

function TTextAreaWidget.GetColumns: Cardinal;
begin
  if IsElementDirty then
    FColumns:=TextArea.Cols;
  Result:=FColumns;
end;

procedure TTextAreaWidget.DoLineChanges(Sender: TObject);
begin
  if isRendered and not FIgnoreChanges then
    ApplyLines(TextArea);
end;


function TTextAreaWidget.GetLines: TStrings;
begin
  // We may want to change this to something more efficient. Maybe handle onchange
  // Note that if yo
  if IsElementDirty  then
    begin
    FIgnoreChanges:=True;
    try
      LinesFromHTML(Element.InnerHTml);
    finally
      FIgnoreChanges:=False;
    end;
    end;
  Result:=FLines;
end;

function TTextAreaWidget.GetReadOnly: Boolean;
begin
  if IsElementDirty then
    FReadonly:=TextArea.readOnly;
  Result:=FReadonly;
end;

function TTextAreaWidget.GetRequired: Boolean;
begin
  if IsElementDirty then
    FRequired:=TextArea.Required;
  Result:=FRequired;
end;

function TTextAreaWidget.GetRows: Cardinal;
begin
  if IsElementDirty then
    FRows:=TextArea.Rows;
  Result:=FRows;
end;

function TTextAreaWidget.GetText: String;
begin
  if IsElementDirty then
    Result:=Element.InnerHTML
  else
    Result:=FLines.Text;
end;

function TTextAreaWidget.GetValueName: string;
begin
  if IsElementDirty then
    FValueName:=Element.Name;
  Result:=FValueName;
end;

procedure TTextAreaWidget.SetColumns(AValue: Cardinal);
begin
  if AValue=FColumns then exit;
  FColumns:=aValue;
  if isRendered then
    TextArea.cols:=aValue;
end;

procedure TTextAreaWidget.SetRows(AValue: Cardinal);
begin
  if AValue=FRows then exit;
  FRows:=aValue;
  if isRendered then
    TextArea.Rows:=aValue;
end;

procedure TTextAreaWidget.SetText(AValue: String);
begin
  if isRendered then
    element.InnerText:=aValue
  else
    LinesFromHTML(aValue);
end;

procedure TTextAreaWidget.SetValueName(AValue: string);
begin
  if aValue=FValueName then exit;
  FValueName:=aValue;
  if IsRendered then
    TextArea.Name:=aValue;
end;

procedure TTextAreaWidget.SetName(const NewName: TComponentName);

var
  Old : String;
begin
  Old:=Name;
  inherited SetName(NewName);
  if csDesigning in ComponentState then
    begin
    if (FLines.Count=0) then
      FLines.Add(Name)
    else if (FLines.Count=1) and (FLines[0]=Old) then
      FLines[0]:=Name;
    end;
end;

procedure TTextAreaWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);

var
  area : TJSHTMLTextAreaElement absolute aElement;

begin
  inherited ApplyWidgetSettings(aElement);
  if FMaxLength>0 then
    area.maxlength:=FMaxLength;
  if FColumns>0 then
    area.cols:=FColumns;
  if FRows>0 then
    area.Rows:=FRows;
  if FLines.Count>0 then
    ApplyLines(area);
  if FValueName<>'' then
    area.Name:=FValueName;
  area.Readonly:=FReadOnly;
  area.Required:=FRequired;
  ApplyWrap(area);
end;


constructor TTextAreaWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FLines:=TStringList.Create;
  TStringList(FLines).OnChange:=@DoLineChanges;
  FColumns:=50;
  FRows:=10;
end;

destructor TTextAreaWidget.Destroy;
begin
  FreeAndNil(Flines);
  inherited;
end;

class function TTextAreaWidget.AllowChildren: Boolean;
begin
  Result:=False;
end;

function TTextAreaWidget.GetTextArea: TJSHTMLTextAreaElement;
begin
  Result:=TJSHTMLTextAreaElement(Element);
end;

procedure TTextAreaWidget.ApplyWrap(aElement :TJSHTMLTextAreaElement);

Const
  Wraps : Array[TTextAreaWrap] of string = ('soft','hard','off');

begin
  aElement.wrap:=Wraps[FWrap];
end;

procedure TTextAreaWidget.ApplyLines(aElement: TJSHTMLTextAreaElement);
begin
  aElement.innerHTML:=FLines.Text;
end;

procedure TTextAreaWidget.LinesFromHTML(aHTML: String);
begin
  FLines.Text:= StringReplace(aHTML,'<br>',sLineBreak,[rfIgnoreCase,rfReplaceAll]);
end;



procedure TTextAreaWidget.SetWrap(AValue: TTextAreaWrap);

begin
  if FWrap=AValue then Exit;
  FWrap:=AValue;
  if IsRendered then
    ApplyWrap(TextArea)
end;

function TTextAreaWidget.HTMLTag: String;
begin
  result:='textarea';
end;

{ TCheckboxInputWidget }

function TCheckboxInputWidget.InputType: String;
begin
  Result:='checkbox';
end;

{ TRadioInputWidget }

function TRadioInputWidget.InputType: String;
begin
  Result:='radio';
end;

{ THiddenInputWidget }

class function THiddenInputWidget.AllowChildren: Boolean;
begin
  Result:=False;
end;

function THiddenInputWidget.InputType: String;
begin
  Result:='hidden';
end;

{ TFileInputWidget }

procedure TFileInputWidget.SetMultiple(AValue: Boolean);
begin
  if FMultiple=AValue then Exit;
  FMultiple:=AValue;
  if Isrendered then
    InputElement.multiple:=FMultiple;
end;

function TFileInputWidget.GetMultiple: Boolean;
begin
  if IsElementDirty  then
    FMultiple:=InputElement.multiple;
  Result:=FMultiple;
end;


function TFileInputWidget.GetFileName(aIndex : Integer): String;
begin
  Result:=InputElement.files.Files[aIndex].name;
end;

function TFileInputWidget.GetFileSize(aIndex : Integer): NativeInt;
begin
  Result:=InputElement.files.Files[aIndex].Size;
end;

function TFileInputWidget.GetFileType(aIndex : Integer): String;
begin
  Result:=InputElement.files.Files[aIndex]._Type;
end;

function TFileInputWidget.GetFileCount: Integer;
begin
  Result:=InputElement.files.Length;
end;

function TFileInputWidget.GetFileDate(aIndex : Integer): TDateTime;
begin
  Result:=JSDateToDateTime(InputElement.files.Files[aIndex].lastModifiedDate);
end;

function TFileInputWidget.GetFileInfo(aIndex : Integer): TFileInfo;

Var
  f : TJSHTMLFile;

begin
  F:=InputElement.files.Files[aIndex];
  Result.Name:=F.name;
  Result.Size:=F.size;
  Result.FileType:=F._type;
  Result.TimeStamp:= JSDateToDateTime(F.lastModifiedDate);
end;


procedure TFileInputWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);

Var
  Old : String;

begin
  Old:=FValue;
  FValue:='';
  try
    inherited ApplyWidgetSettings(aElement);
    TJSHTMLInputElement(aElement).multiple:=FMultiple;
  finally
    FValue:=Old;
  end;
end;

class function TFileInputWidget.AllowChildren: Boolean;
begin
  Result:=False;
end;

function TFileInputWidget.InputType: String;
begin
  Result:='file';
end;


{ TDateInputWidget }

function TDateInputWidget.GetDate: TDateTime;

var
  aDate : TDateTime;

begin
  if IsElementDirty then
    begin
    aDate:=ScanDateTime('yyyy-mm-dd',Value);
    if aDate<>0 then
      FDate:=aDate;
    end;
  Result:=FDate;
end;


procedure TDateInputWidget.SetDate(AValue: TDateTime);
begin
  FDate:=aValue;
  Value:=FormatDateTime('yyyy-mm-dd',FDate);
end;

function TDateInputWidget.InputType: String;
begin
  Result:='date';
end;

class function TDateInputWidget.AllowChildren: Boolean;
begin
  Result:=False;
end;

{ TCheckableInputWidget }


procedure TCheckableInputWidget.SetChecked(AValue: Boolean);
begin
  // Get actual value
  if Checked=AValue then Exit;
  if isRendered then
    InputElement.checked:=aValue;
  FChecked:=AValue;
end;

procedure TCheckableInputWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);
begin
  inherited ApplyWidgetSettings(aElement);
  TJSHTMLInputElement(aElement).Checked:=FChecked;
end;


function TCheckableInputWidget.GetChecked: Boolean;
begin
  if IsElementDirty then
    FChecked:=InputElement.Checked;
  Result:=FChecked;
end;


{ TButtonInputWidget }

procedure TButtonInputWidget.SetButtonType(AValue: TInputButtonType);
begin
  if FButtonType=AValue then Exit;
  FButtonType:=AValue;
  if IsRendered then
    Refresh;
end;

procedure TButtonInputWidget.SetSrc(AValue: String);
begin
  if FSrc=AValue then Exit;
  FSrc:=AValue;
  if IsRendered and (ButtonType=ibtImage) then
    Element.setAttribute('src',FSrc);
end;

procedure TButtonInputWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);
begin
  inherited ApplyWidgetSettings(aElement);
  if ButtonType=ibtImage then
    aElement.setAttribute('src',FSrc);
end;

function TButtonInputWidget.InputType: String;

Const
  Types : Array[TInputButtonType] of string = ('submit','reset','image');

begin
  Result:=Types[FButtonType]
end;

class function TButtonInputWidget.AllowChildren: Boolean;
begin
  Result:=False;
end;

{ TTextInputWidget }

function TTextInputWidget.GetAsNumber: NativeInt;
begin
  Result:=StrToIntDef(Value,0);
end;

function TTextInputWidget.GetMaxLength: NativeInt;
begin
  if IsElementDirty then
    FMaxLength:=InputElement.maxLength;
  Result:=FMaxLength;
end;

function TTextInputWidget.GetMinLength: NativeInt;
begin
  if IsElementDirty then
    FMinLength:=InputElement.minLength;
  Result:=FMinLength;
end;

function TTextInputWidget.GetTextType: TInputTextType;
begin
  Result:=FTextType;
end;

procedure TTextInputWidget.SetAsNumber(AValue: NativeInt);
begin
  Value:=IntToStr(aValue);
end;


procedure TTextInputWidget.SetMaxLength(AValue: NativeInt);
begin
  if (aValue=FMaxLength) then exit;
  FMaxLength:=aValue;
  if IsRendered then
    InputElement.maxLength:=FMaxLength;
end;

procedure TTextInputWidget.SetMinLength(AValue: NativeInt);
begin
  if (aValue=FMinLength) then exit;
  FMinLength:=aValue;
  if IsRendered then
    InputElement.minLength:=FMinLength;
end;

procedure TTextInputWidget.SetTextType(AValue: TInputTextType);
begin
  if aValue=FTextType then exit;
  FTextType:=aValue;
  if IsRendered then
    Refresh;
end;

procedure TTextInputWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);

var
  inp : TJSHTMLInputElement absolute aElement;

begin
  inherited ApplyWidgetSettings(aElement);
  if FMaxLength<>0 then
    inp.maxLength:=FMaxLength;
  if FMinLength<>0 then
    inp.minLength:=FMinLength;
end;

class function TTextInputWidget.AllowChildren: Boolean;
begin
  Result:=False;
end;

function TTextInputWidget.InputType: String;

Const
  Types : Array[TInputTextType] of string =
     ('text','password','number','email','search','tel','url','color');

begin
  Result:=Types[FTextType];
end;

{ TWebPage }

constructor TWebPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Classes:='WebPage';
end;


class function TWebPage.DefaultParentElement: TJSHTMLElement;
begin
  Result:=TViewport.Instance.Element;
end;

class function TWebPage.DefaultParent: TCustomWebWidget;
begin
  Result:=TViewport.Instance;
end;

procedure TWebPage.DoUnRender(aParent: TJSHTMLElement);
begin
  inherited DoUnRender(aParent);
end;

function TWebPage.HTMLTag: String;
begin
  Result:='div';
end;


{ TViewPort }

function TViewPort.HTMLTag: String;
begin
  Result:='body';
end;

class function TViewPort.FixedParent: TJSHTMLElement;
begin
  Result:=TJSHTMLElement(Document.documentElement);
end;

class function TViewPort.FixedElement: TJSHTMLElement;
begin
  Result:=TJSHTMLElement(Document.Body);
end;

function TViewPort.DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement;
begin
  Result:=FixedElement;
end;

constructor TViewPort.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  EnsureElement;
end;

class function TViewPort.Instance: TViewPort;
begin
  if Finstance=Nil then
    FInstance:=TViewPort.Create(Nil);
  Result:=FInstance;
end;

{ TButtonWidget }

{ TButtonWidget }

procedure TButtonWidget.SetText(AValue: String);

begin
  if FText=AValue then Exit;
  FText:=AValue;
  if IsRendered then
    ApplyText(Element);
end;

procedure TButtonWidget.SetTextMode(AValue: TTextMode);
begin
  if FTextMode=AValue then Exit;
  FTextMode:=AValue;
  if IsRendered then
     ApplyText(Element)
end;


procedure TButtonWidget.SetName(const NewName: TComponentName);

Var
  Old : String;

begin
  Old:=Name;
  inherited SetName(NewName);
  if (FText=Old) and (csDesigning in ComponentState) then
     FText:=NewName;
end;

function TButtonWidget.HTMLTag: String;
begin
  Result:='button';
end;

procedure TButtonWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);
begin
  Inherited;
  ApplyText(aElement);
end;

Procedure TButtonWidget.ApplyText(aElement : TJSHTMLElement);

begin
  if FTextMode=tmText then
    aElement.InnerText:=FText
  else
    aElement.InnerHTML:=FText;
end;

procedure TButtonWidget.Click;

begin
  DispatchEvent('click');
end;


{ TCustomInputWidget }

function TCustomInputWidget.GetValue: String;

Var
  Inp : TJSHTMLInputElement;
begin
  Inp:=InputElement;
  If Assigned(Inp) then
    Result:=Inp.value
  else
    Result:=FValue
end;

function TCustomInputWidget.GetText: String;
Var
  Inp : TJSHTMLElement;

begin
  Inp:=Element;
  If Assigned(Inp) then
    Result:=Inp.InnerText
  else
    Result:=FText;
  // Writeln('Getting text: ',Result,' inner : ',FText);
end;

function TCustomInputWidget.GetReadOnly: Boolean;
begin
  if IsElementDirty then
    FReadonly:=InputElement.readOnly;
  Result:=FReadonly;
end;

function TCustomInputWidget.GetRequired: Boolean;
begin
  if IsElementDirty then
    FRequired:=InputElement.Required;
  Result:=FRequired;
end;

function TCustomInputWidget.GetValueName: String;

Var
  Inp : TJSHTMLInputElement;

begin
  Inp:=InputElement;
  If Assigned(Inp) then
    Result:=Inp.Name
  else
    begin
    Result:=FValueName;
    if Result='' then
      Result:=Name;
    end;
end;

procedure TCustomInputWidget.SetReadonly(AValue: Boolean);
begin
  If aValue=ReadOnly then exit;
  FReadOnly:=aValue;
  if IsRendered then
    InputElement.Readonly:=FReadOnly;
end;

procedure TCustomInputWidget.SetRequired(AValue: Boolean);
begin
  If aValue=Required then exit;
  FRequired:=aValue;
  if IsRendered then
    InputElement.Required:=FRequired;
end;

procedure TCustomInputWidget.SetText(AValue: String);
Var
  Inp : TJSHTMLElement;

begin
  Writeln('Setting text: ',AValue,' previous : ',Text);
  if aValue=Text then exit;
  FText:=aValue;
  Inp:=Element;
  If Assigned(Inp) then
    Inp.innerText:=aValue;
end;

procedure TCustomInputWidget.SetValue(AValue: String);

Var
  Inp : TJSHTMLInputElement;

begin
  if aValue=Value then exit;
  FValue:=aValue;
  Inp:=InputElement;
  If Assigned(Inp) then
    Inp.value:=aValue;
end;

procedure TCustomInputWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);

var
  Inp : TJSHTMLInputElement absolute aElement;

begin
  Inherited;
  if (ExternalElement) and (FValue='') then
    FValue:=TJSHTMLInputElement(aElement).value
  else
    begin
    Inp._type:=InputType;
    Inp.name:=FValueName;
    Inp.value:=FValue;
    Inp.Required:=FRequired;
    Inp.ReadOnly:=FReadOnly;
    Writeln('Setting inner text to "',FText,'"');
    Inp.innerHtml:=FText;
    Writeln('Setting inner text is now "',Inp.innerText,'"');
    end;
end;

function TCustomInputWidget.HTMLTag: String;
begin
  Result:='input';
end;

function TCustomInputWidget.GetInputElement: TJSHTMLInputElement;
begin
  Result:=TJSHTMLInputElement(Element);
end;

procedure TCustomInputWidget.SetValueName(AValue: String);
Var
  Inp : TJSHTMLInputElement;
begin
  if aValue=ValueName then exit;
  FValueName:=aValue;
  Inp:=InputElement;
  If Assigned(Inp) then
    Inp.name:=aValue;
end;

procedure TCustomInputWidget.SetName(const NewName: TComponentName);

Var
  Old : String;

begin
  Old:=Name;
  inherited SetName(NewName);
  if (Value=Old) then
    Value:=NewName;
end;


{ TCustomTagWidget }

procedure TCustomTagWidget.SetElementTag(AValue: THTMLElementTag);

begin
  if FElementTag=AValue then Exit;
  FElementTag:=AValue;
  if IsRendered then
    Refresh;
end;

procedure TCustomTagWidget.SetTextContent(AValue: String);
begin
  if FTextContent=AValue then Exit;
  FTextContent:=AValue;
  if IsRendered then
    Refresh;
end;

procedure TCustomTagWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);
begin
  inherited ApplyWidgetSettings(aElement);
  if FTextContent<>'' then
    aElement.InnerText:=TextContent;
end;

function TCustomTagWidget.HTMLTag: String;

begin
  Result:=HTMLTagNames[ElementTag];
end;

{ TDivWidget }


constructor TDivWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  ElementTag:=etDiv;
end;

constructor TParagraphWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  ElementTag:=etP;
end;


end.



