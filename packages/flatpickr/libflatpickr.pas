unit libflatpickr;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses types, js, web, libjquery;

Type
  TJSDateDynArray = Array of TJSDate;
  TJSHTMLElementDynArray = Array of TJSHTMLElement;
  TJSHTMLInputElementDynArray = Array of TJSHTMLInputElement;
  TJSNodeDynArray = Array of TJSNode;
  TJSHTMLDivElement = TJSHTMLElement;
  TJSHTMLSpanElement = TJSHTMLElement;
  TFPRevFormat = JSValue;
  TFPFormats = JSValue;
  TFPTokenRegex = JSValue;

  TFPLocaleKey = string;

  TFPDateOption = jsValue;
  TFPDateOptionArray = array of TFPDateOption;
  TFPPositionFunction = reference to procedure(aSelf: TJSObject; customElement: TJSHTMLElement);

  TFPErrorHandler = reference to procedure (e: TJSError);

  TFPWeekdayNames = Class external name 'Object' (TJSObject)
    shorthand : array[0..6] of string;
    longhand : array[0..6] of string;
  end;
  TFPMonthNames = Class external name 'Object' (TJSObject)
    shorthand : array[0..11] of string;
    longhand : array[0..11] of string;
  end;
  TFPDaysInmonth = Array [0..11] of Integer;
  TFPOrdinalFunction = reference to function (nth: integer) : string;
  TFPAMPMArray = array[0..1] of string;

  TFPLocale = Class external name 'Object' (TJSObject)
    weekdays : TFPWeekDayNames;
    months : TFPMonthNames;
    daysInMonth : TFPDaysInMonth;
    firstDayOfWeek: Integer;
    ordinal: TFPOrdinalfunction;
    rangeSeparator: string;
    weekAbbreviation: string;
    scrollTitle: string;
    toggleTitle: string;
    amPM: TFPAMPMArray;
    yearAriaLabel: string;
    monthAriaLabel: string;
    hourAriaLabel: string;
    minuteAriaLabel: string;
    time_24hr: boolean;
  end;

  TFPDateFormatHandler = reference to function (date: TJSDate; format: string; locale: TFPLocale) : String;
  TFPDateParserHandler = reference to function (date: string; format: string) : TJSDate;
  TFPHook = reference to Procedure (dates : TJSDateDynArray; CurrentDateString : String; aSelf : TJSObject; Data : JSValue);
  TFPHookDynArray = Array of TFPHook;
  TFPDateToWeekHandler = reference to function (adate: TJSDate) : string;

  TFlatPickrOptions = Class external name 'Object' (TJSObject)
    allowInput: boolean;
    allowInvalidPreload: boolean;
    altFormat: string;
    altInput: boolean;
    altInputClass: string;
    animate: boolean;
    appendTo: TJSHTMLElement;
    ariaDateFormat: string;
    autoFillDefaultTime: boolean;
    clickOpens: boolean;
    closeOnSelect: boolean;
    conjunction: string;
    dateFormat: string;
    defaultDate: TFPDateOption;
    defaultDateArray : TFPDateOptionArray; external name 'defaultDate';
    defaultHour: Integer;
    defaultMinute: Integer;
    defaultSeconds: Integer;
    disable: JSValue;
    disableMobile: boolean;
    enable: JSValue;
    enableSeconds: boolean;
    enableTime: boolean;
    errorHandler: TFPErrorHandler;
    formatDate: TFPDateFormatHandler;
    getWeek : TFPDateToWeekHandler;
    hourIncrement: integer;
    ignoredFocusElements: TJSHTMLElementDynArray;
    inline_: boolean; external name 'inline';
    locale: string;
    maxDate: TFPDateOption;
    maxTime: TFPDateOption;
    minDate: TFPDateOption;
    minTime: TFPDateOption;
    minuteIncrement: integer;
    mode: string;
    monthSelectorType: string;
    nextArrow: string;
    noCalendar: boolean;
    now: TFPDateOption;
    onChange: TFPHook;
    onChangeArray : TFPHookDynArray; external name 'onChange';
    onClose: TFPHook;
    onCloseArray : TFPHookDynArray; external name 'onClose';
    onDayCreate: TFPHook;
    onDayCreateArray : TFPHookDynArray; external name 'onDayCreate';
    onDestroy: TFPHook;
    onDestroyArray : TFPHookDynArray; external name 'onDestroy';
    onKeyDown: TFPHook;
    onKeyDownArray : TFPHookDynArray; external name 'onKeyDown';
    onMonthChange: TFPHook;
    onMonthChangeArray : TFPHookDynArray; external name 'onMonthChange';
    onOpen: TFPHook;
    onOpenArray : TFPHookDynArray; external name 'onOpen';
    onParseConfig: TFPHook;
    onParseConfigArray : TFPHookDynArray; external name 'onParseConfig';
    onReady: TFPHook;
    onReadyArray : TFPHookDynArray; external name 'onReady';
    onValueUpdate: TFPHook;
    onValueUpdateArray : TFPHookDynArray; external name 'onValueUpdate';
    onYearChange: TFPHook;
    onYearChangeArray : TFPHookDynArray; external name 'onYearChange';
    onPreCalendarPosition: TFPHook;
    onPreCalendarPositionArray : TFPHookDynArray; external name 'onPreCalendarPosition';
    parseDate: TFPDateParserHandler;
    plugins: TJSValueDynArray;
    position: String;
    positionFunction : TFPPositionFunction; external name 'position';
    positionElement: TJSHTMLElement;
    prevArrow: string;
    shorthandCurrentMonth: boolean;
    static_: boolean; external name 'static';
    showMonths: Integer;
    time_24hr: boolean;
    weekNumbers: boolean;
    wrap: boolean;
  end;

  TFlatPickr = Class external name 'Object' (TJSObject)
    // Formatting

    revFormat: TFPRevFormat;
    formats: TFPFormats;
    tokenRegex: TFPTokenRegex;

    // elements
    element: TJSHTMLElement;
    input: TJSHTMLInputElement;
    altInput: TJSHTMLInputElement;
    _input: TJSHTMLInputElement;
    mobileInput: TJSHTMLInputElement;
    mobileFormatStr: string;

    selectedDateElem: TJSHTMLElement;
    todayDateElem: TJSHTMLElement;

    _positionElement: TJSHTMLElement;
    weekdayContainer: TJSHTMLDivElement;
    calendarContainer: TJSHTMLDivElement;
    innerContainer: TJSHTMLDivElement;
    rContainer: TJSHTMLDivElement;
    daysContainer: TJSHTMLDivElement;
    days: TJSHTMLDivElement;

    weekWrapper: TJSHTMLDivElement;
    weekNumbers: TJSHTMLDivElement;

    // month nav
    monthNav: TJSHTMLDivElement;

    monthsDropdownContainer: TJSHTMLSelectElement;

    yearElements: TJSHTMLInputElementDynArray;
    monthElements: TJSHTMLSpanElement;

    // month nav getters
    currentYearElement: TJSHTMLInputElement;
    currentMonthElement: TJSHTMLSpanElement;

    // month nav arrows
    _hidePrevMonthArrow: boolean;
    _hideNextMonthArrow: boolean;
    prevMonthNav: TJSHTMLElement;
    nextMonthNav: TJSHTMLElement;

    timeContainer: TJSHTMLDivElement;
    hourElement: TJSHTMLInputElement;
    minuteElement: TJSHTMLInputElement;
    secondElement: TJSHTMLInputElement;
    amPM: TJSHTMLSpanElement;

    pluginElements: TJSNodeDynArray;

    minRangeDate: TJSDate;
    maxRangeDate: TJSDate;
    now: TJSDate;
    latestSelectedDateObj: TJSDate;
    _selectedDateObj : TJSDate;
    selectedDates: TJSDateDynArray;
    _initialDate: TJSDate;

    // State
    config: TJSObject;
    loadedPlugins: TJSstringDynArray;
    l10n: TFPLocale;

    currentYear: integer;
    currentMonth: integer;

    isOpen: boolean;
    isMobile: boolean;

    minDateHasTime: boolean;
    maxDateHasTime: boolean;

    // Methods
    procedure changeMonth (value: Integer; isOffset: boolean; fromKeyboard: boolean);
    procedure changeMonth (value: Integer; isOffset: boolean);
    procedure changeMonth (value: Integer);
    procedure changeYear (year: integer);
    procedure clear (emitChangeEvent: boolean; toInitial: boolean);
    procedure clear (emitChangeEvent: boolean);
    procedure clear ();
    procedure close ();
    procedure destroy ();
    function isEnabled (date: TFPDateOption; timeless: boolean) : boolean;
    function isEnabled (date: TFPDateOption) : boolean;
    procedure jumpToDate (date: TFPDateOption; triggerChange: boolean);
    procedure open (e: TJSEvent; positionElement: TJSHTMLElement);
    procedure open (e: TJSEvent);
    procedure redraw ();
    procedure set_ (option: string; value: JSValue) ; external name 'set';
    procedure setDate (date: TFPDateOption; triggerChange: boolean; format: string) ;
    procedure setDate (date: TFPDateOption; triggerChange: boolean) ;
    procedure setDate (date: TFPDateOption) ;
    procedure setDate (date: TFPDateOptionArray; triggerChange: boolean; format: string) ;
    procedure setDate (date: TFPDateOptionArray; triggerChange: boolean) ;
    procedure setDate (date: TFPDateOptionArray);
    procedure toggle;

    function pad (num: string ) : string;
    function pad (num: integer ) : string;
    function parseDate (adate: TJSDate; givenFormat: string; timeless: boolean) : TJSDate;
    function parseDate (adate: string; givenFormat: string; timeless: boolean) : TJSDate;
    function parseDate (adate: integer; givenFormat: string; timeless: boolean) : TJSDate;
    function parseDate (adate: TJSDate; givenFormat: string) : TJSDate;
    function parseDate (adate: string; givenFormat: string) : TJSDate;
    function parseDate (adate: integer; givenFormat: string) : TJSDate;
    function formatDate (dateObj: TJSDate; frmt: string) : string;
  end;
  TFlatPickrArray = array of TFlatPickr;

  { JQueryFlatPickrHelper }

  JQueryFlatPickrHelper = class helper for TJQuery
    Function flatpickr (aOptions : TFlatPickrOptions) : TFlatPickr; external name 'flatpickr';
    Function flatpickr () : TFlatPickr; external name 'flatpickr';
  end;

  { JJSElementFlatPickrHelper }

  JJSElementFlatPickrHelper = class helper for TJSElement
    Function _flatpickr : TFlatPickr;
  end;


Function flatpickr(aElement : TJSHTMLElement) : TFlatPickr; external name 'flatpickr';
Function flatpickr(aElement : TJSHTMLElement; aOptions : TFlatPickrOptions) : TFlatPickr; external name 'flatpickr';
Function flatpickr(aElementID : String) : TFlatPickr; external name 'flatpickr';
Function flatpickr(aElementID : String; aOptions : TFlatPickrOptions) : TFlatPickr; external name 'flatpickr';
Function multiflatpickr(const aSelector : String) : TFlatPickrArray; external name 'flatpickr';
Function multiflatpickr(const aSelector : String; aOptions : TFlatPickrOptions) : TFlatPickrArray; external name 'flatpickr';


implementation



{ JQueryFlatPickrHelper }

{ JJSElementFlatPickrHelper }

function JJSElementFlatPickrHelper._flatpickr: TFlatPickr;
begin
  Result:=TFlatPickr(Self.Properties['_flatpickr']);
end;

end.
