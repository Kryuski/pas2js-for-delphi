{
    This file is part of the Pas2JS run time library.
    Copyright (C) 2019 Michael Van Canneyt

    FullCalendar version 5 mappings for pas2js

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit libfullcalendar5;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
 sysutils, types, js, web;

Const
  fcViewMonth = 'dayGridMonth';
  fcViewWeek = 'timeGridWeek';
  fcViewDay = 'timeGridDay';

  // A list of plugin names, created from the plugin index page
  fcInteractionPlugin = 'interaction';
  fcDayGridPlugin = 'dayGrid';
  fcTimeGridPlugin = 'timeGrid';
  fcListPlugin = 'list';
  fcTimelinePlugin = 'timeline';
  fcResourceDayGridPlugin = 'resourceDayGrid';
  fcResourceTimeGridPlugin = 'resourceTimeGrid';
  fcResourceTimelinePlugin = 'resourceTimeline';
  fcBootstrapPlugin = 'bootstrap';
  fcGoogleCalendarPlugin = 'googleCalendar';
  fcRRulePlugin = 'rrule';
  fcMomentPlugin = 'moment';
  fcMomentTimezonePlugin = 'momentTimeZone';
  fcReactPlugin = 'react';
  fcAngularPlugin = 'angular';
  fcVuePlugin = 'vue';

  fcHeaderFooterTitle = 'title';
  fcHeaderFooterPrev = 'prev';
  fcHeaderFooterNext = 'next';
  fcHeaderFooterPrevYear = 'prevYear';
  fcHeaderFooterNextYear = 'nextYear';
  fcHeaderFooterToday = 'today';

  fcDateFormatNumeric = 'numeric';
  fcDateFormat2Digit = '2-digit';
  fcDateFormatLong = 'long';
  fcDateFormatShort = 'short';
  fcDateFormatNarrow = 'narrow';
  
  fcEventDisplayAuto = 'auto';
  fcEventDisplayBlock = 'block';
  fcEventDisplayListItem = 'list-item';
  fcEventDisplayBackground = 'background';
  fcEventDisplayInverseBackground = 'inverse-background';
  fcEventDisplayNone = 'none';

  fcSlotFuture = 'fc-slot-future';
  fcSlotPast = 'fc-slot-past';
  fcSlotFri = 'fc-slot-fri';
  fcSlotSat = 'fc-slot-sat';
  fcSlotSun = 'fc-slot-sun';
  fcSlotToday = 'fc-slot-today';

Type
  TProcedural = reference to procedure;
  TDateFunction = reference to Function : TJSDate;

  TDateFormatter = Class external name 'Object' (TJSObject)
    // Use fcDateFormat* constants
    year : String;
    month : String;
    day : String;
    week : string;
    meridiem : string;
    weekday : String;
    hour : string;
    minute : string;
    second : string;
    hour12 : Boolean;
    timeZoneName : string;
    omitZeroMinute : Boolean;
    omitCommas : Boolean;
  end;

  TDuration = Class external name 'Object' (TJSObject)
    years : NativeInt;
    months : NativeInt;
    days : NativeInt;
    milliseconds : NativeInt;
  end;

  TDateRange = Class external name 'Object' (TJSObject)
    start : TJSDate;
    startStr : string; external name 'start';
    startInt : nativeInt; external name 'start';
  end;

  TCalendarHeaderFooterOptions = Class external name 'Object' (TJSObject)
    // use the fcHeaderFooter consts
    start : string;
    end_ : string; external name 'end';
    center : string;
  end;

  TDateFormatHandler = reference to function(aDate : TJSDate) : String;

  TButtonText = Class external name 'Object' (TJSObject)
    today : string;
    month : string;
    week : string;
    day : string;
    list : string;
  end;

  TButtonTextRec = record
    today : string;
    month : string;
    week : string;
    day : string;
    list : string;
  end;

  TButtonIcons = Class external name 'Object' (TJSObject)
    title : string;
    prev : string;
    next : string;
    prevYear : string;
    nextYear : string;
    today : string;
  end;

  TButtonIconsRec = record
    today : string;
    title : string;
    prev : string;
    next : string;
    prevYear : string;
    nextYear : string;
  end;

  TCustomButtonSpec = record
    text : string;
    click : TJSEventHandler;
  end;

  TFontAwesomeSpec = Class external name 'Object' (TJSObject)
    close : string;
    title : string;
    prev : string;
    next : string;
    prevYear : string;
    nextYear : string;
    today : string;
  end;

  TFontAwesomeSpecRec = record
    close : string;
    title : string;
    prev : string;
    next : string;
    prevYear : string;
    nextYear : string;
    today : string;
  end;


  TCustomButtonSpecs = Class external name 'Object' (TJSObject)
  private
    function GetButton(Name: String): TCustomButtonSpec; external name '[]';
    procedure SetButton(Name: String; const AValue: TCustomButtonSpec); external name '[]';
  Public
    property buttons [aIndex : string] : TCustomButtonSpec read GetButton Write SetButton;
  end;

  TBusinessHoursSpec = Class external name 'Object' (TJSObject)
    daysOfWeek : TIntegerDynArray;
    startTime : TDuration;
    startTimeStr : string; external name 'startTime';
    startTimeInt : nativeInt; external name 'startTime';
    endTime : TDuration;
    endTimeStr : string; external name 'endTime';
    endTimeInt : nativeInt; external name 'endTime';
  end;
  TBusinessHoursSpecArray = Array of TBusinessHoursSpec;

  TCalendarEvent = Class;
  TCalendarResource = Class;
  TFullCalendarView = class;

  TAllowDropInfo = record
    allDay : Boolean;
    end_ : TJSDate; external name 'end';
    resource : TCalendarResource;
    start : TJSDate;
    startStr : String;
  end;

  TAllowFunction = reference to function (dropInfo : TAllowDropInfo; draggedEvent : TCalendarEvent) : Boolean;

  TBaseCalendarResource = Class external name 'Object' (TJSObject)
    id : String;
    title : string;
    eventColor : string;
    eventBackgroundColor : string;
    eventBorderColor : string;
    eventTextColor : string;
    eventClassNames : String;
    eventClassNamesArray : TStringDynArray; external name 'eventClassNames';
    eventOverlap : Boolean;
    eventAllow : TAllowFunction;
  end;
  TBaseCalendarResourceArray = array of TBaseCalendarResource;

  TCalendarResource =  Class external name 'Object' (TJSObject)
  end;

  TCalendarResourceArray = array of TCalendarResource;

  THeightHandler  = Reference to function : NativeInt;

  TBaseCalendarEvent = Class external name 'Object' (TJSObject)
    id : string;
    idInt : NativeInt; external name 'id';
    groupId : string;
    groupIdInt : NativeInt; external name 'groupId';
    allDay : Boolean;
    start : TJSDate;
    startStr : string; external name 'start';
    startInt : nativeInt; external name 'start';
    end_ : TJSDate; external name 'end';
    endStr : string; external name 'end';
    endInt : nativeInt; external name 'end';
    daysOfWeek : TIntegerDynArray;
    startTime : TDuration;
    startTimeStr : string; external name 'startTime';
    endTime : TDuration;
    endTimeStr : string; external name 'endTime';
    startRecur : TJSDate;
    startRecurStr : string; external name 'startRecur';
    startRecurInt : nativeInt; external name 'startRecur';
    endRecur : TJSDate;
    endRecurStr : string; external name 'endRecur';
    endRecurInt : nativeInt; external name 'endRecur';
    title : string;
    url : string;
    classNames : string;
    classNamesArray : TStringDynArray; external name 'classNames';
    editable : boolean;
    startEditable : Boolean;
    durationEditable : Boolean;
    resourceEditable : Boolean;
    resourceId : String;
    resourceIds : TStringDynArray;
    display : string;
    overlap : boolean;
    constraint : string;
    constraintObj : TBusinessHoursSpec; external name 'constraint';
    color : string;
    backgroundColor : string;
    borderColor : string;
    textColor : string;
    extendedProps : TJSObject;
  end;
  TBaseCalendarEventArray = array of TBaseCalendarEvent;

  { TBaseCalendarEventHelper }

  TBaseCalendarEventHelper = class helper for TBaseCalendarEvent
    class function event(const aTitle : String; aStart,aEnd : TDateTime) : TBaseCalendarEvent; static;
  end;

  { TCalendarEvent }

  TCalendarEvent = Class external name 'Object' (TJSObject)
  private
    FAllDay: Boolean; external name 'allDay';
    FbackgroundColor: string; external name 'backgroundColor';
    FBorderColor: string; external name 'borderColor';
    FClassNames: TStringDynArray; external name 'classNames';
    FdurationEditable: Boolean; external name 'durationEditable';
    feditable: boolean; external name 'editable';
    FEnd: TJSDate; external name 'end';
    fEventConstraint: string; external name 'eventConstraint';
    FExtendedProps: TJSObject; external name 'extendedProps';
    FGroupID: string; external name 'groupId';
    FID: string; external name 'id';
    FOverLap: boolean; external name 'overlap';
    FRendering: string; external name 'rendering';
    FresourceEditable: Boolean; external name 'resourceEditable';
    FSource: JSValue; external name 'source';
    FStart: TJSDate; external name 'start';
    FstartEditable: Boolean; external name 'startEditable';
    FTextColor: string; external name 'textColor';
    FTitle: string; external name 'title';
    FURL: string; external name 'url';
  Public
    procedure setProp(const aName : string; aValue : JSValue);
    procedure setExtendedProp(const aName : string; aValue : JSValue);
    Procedure setStart(aDate : TJSDate); overload;
    Procedure setStart(aDate : String); overload;
    Procedure setStart(aDate : NativeInt); overload;
    Procedure setStart(aDate : TJSDate; Options : TJSObject); overload;
    Procedure setStart(aDate : String; Options : TJSObject); overload;
    Procedure setStart(aDate : NativeInt; Options : TJSObject); overload;
    Procedure setEnd(aDate : TJSDate); overload;
    Procedure setEnd(aDate : String); overload;
    Procedure setEnd(aDate : NativeInt); overload;
    Procedure setDates(aStart,aEnd : TJSDate); overload;
    Procedure setDates(aStart,aEnd : String); overload;
    Procedure setDates(aStart,aEnd : NativeInt); overload;
    Procedure setDates(aStart,aEnd : TJSDate; Options : TJSObject); overload;
    Procedure setDates(aStart,aEnd : String; Options : TJSObject); overload;
    Procedure setDates(aStart,aEnd : NativeInt; Options : TJSObject); overload;
    Procedure setAllDay(aValue : Boolean); overload;
    Procedure setAllDay(aValue : Boolean; Options:TJSObject); overload;
    Procedure moveStart(aDelta : TDuration); overload;
    Procedure moveStart(aDelta : String); overload;
    Procedure moveStart(aDelta : NativeInt); overload;
    Procedure moveEnd(aDelta : TDuration); overload;
    Procedure moveEnd(aDelta : String); overload;
    Procedure moveEnd(aDelta : NativeInt); overload;
    Procedure moveDates(aDelta : TDuration); overload;
    Procedure moveDates(aDelta : String); overload;
    Procedure moveDates(aDelta : NativeInt); overload;
    Procedure formatRange(formatter : TDateFormatter);
    Procedure remove;
    Function getResources : TCalendarResourceArray;
    Procedure setResources(aResources : array of string); overload;
    Procedure setResources(aResources : TCalendarResourceArray); overload;

    Property id : string read FID;
    Property groupId : string read FGroupID;
    Property allDay : Boolean read FAllDay;
    Property start : TJSDate read FStart;
    Property end_ : TJSDate read FEnd;
    Property title : string read FTitle;
    Property url : string read FURL;
    Property classNames : TStringDynArray read FClassNames;
    Property editable : boolean read feditable;
    Property startEditable : Boolean read FstartEditable;
    property eventConstraint : string Read fEventConstraint;
    Property durationEditable : Boolean Read FdurationEditable;
    Property resourceEditable : Boolean read FresourceEditable;
    Property rendering : string read FRendering;
    Property overlap : boolean read FOverLap;
    Property backgroundColor : string read FbackgroundColor;
    Property borderColor : string read FBorderColor;
    Property textColor : string Read FTextColor;
    Property extendedProps : TJSObject Read FExtendedProps;
    Property source : JSValue Read FSource;
  end;
  TCalendarEventArray = array of TCalendarEvent;

  TGoogleCalendarEventsSpec = Class external name 'Object' (TJSObject)
    googleCalendarId : String;
    // Other options can be specified
  end;


  TJSONFeedSpec = Class external name 'Object' (TJSObject)
    url : string;
  // Other options can be specified
  end;

  TEventGeneratorInfo = record
    start : TJSDate;
    end_ : TJSDate; external name 'end';
    startStr : String;
    endStr : String;
    timeZone : String;
  end;

  TSelectInfo = Record
    Start : TJSDate;
    end_ : TJSDate; external name 'end';
    resourceId : String;
  end;

  TEventMouseInfo = record
    event : TCalendarEvent;
    el : TJSHTMLElement;
    jsEvent : TJSEvent;
    view : TFullCalendarView;
  end;


  TGenerateEventsCallBack = Procedure (Res : TBaseCalendarEventArray);
  TGenerateEventsFailureCallBack = Procedure (Res : JSValue);
  TCalendarEventGenerator = reference to Procedure (info : TEventGeneratorInfo; successCallBack : TGenerateEventsCallBack; FailCallBack : TGenerateEventsFailureCallBack);
  TCalendarLoadingCallback = reference to procedure (isLoading : Boolean);
  TSelectOverlapHandler = reference to function(Event : TJSObject) : boolean;
  TSelectAllowHandler = reference to function(info : TSelectInfo) : Boolean;
  TEventMouseEventHandler = reference to procedure(info : TEventMouseInfo);

  TCalendarEventSource = Class external name 'Object' (TJSObject)
    events : TBaseCalendarEventArray;
    eventsStr : string; external name 'events'; // JSON feed
    eventsFunc : TCalendarEventGenerator; external name 'events'; // JSON feed
    eventsJSONFeed : TJSONFeedSpec; external name 'events'; // JSON feed
    eventsArr : TCalendarEventArray; external name 'events'; // JSON feed
    eventsObjList : TJSObjectDynArray ; external name 'events'; // Roll your own
    Procedure refetch;
    Procedure remove;
  end;

  TCalendarEventRenderInfo = record
    event: TCalendarEvent;
    el : TJSHTMLElement;
    timeText : string;
    isStart : Boolean;
    isEnd : Boolean;
    isMirror : Boolean;
    isPast : Boolean;
    isFuture : Boolean;
    isToday : Boolean;
    view : TFullCalendarView;
  end;

  TCalendarEventContentObj = Class external name 'Object' (TJSObject)
    html : string;
    domNodes : Array of TJSHTMLElement;
  end;
  TSlotLabelContentObj = TCalendarEventContentObj;
  TSlotLaneContentObj = TCalendarEventContentObj;
  TweekNumberContentObj = TCalendarEventContentObj;
  TViewContentObj = TCalendarEventContentObj;

  TRevertHandler = reference to procedure;
  TAddEventInfo = Class external name 'Object' (TJSObject)
    event : TCalendarEvent;
    relatedEvents : TCalendarEventArray;
    revert : TRevertHandler; 
  end;

  TChangeEventInfo = Class external name 'Object' (TJSObject)
    event : TCalendarEvent;
    oldevent : TCalendarEvent;
    relatedEvents : TCalendarEventArray;
    revert : TRevertHandler;
  end;

  TRemoveEventInfo = Class external name 'Object' (TJSObject)
    event : TCalendarEvent;
    relatedEvents : TCalendarEventArray;
    revert : TRevertHandler; 
  end;
  
  TCalendarViewRenderInfo = record
    view: TFullCalendarView;
    el : TJSHTMLElement;
  end;  
  
  TDayHeaderRenderInfo = Record
    date : TJSDate;
    text : string;
    isPast : Boolean;
    isFuture : Boolean;
    isToday : Boolean;
    isOther : boolean;
    resource : TJSObject;
    el : TJSHTMLElement;
  end;

  TDayCellRenderInfo = Record
    date : TJSDate;
    dayNumberText : string;
    isPast : Boolean;
    isFuture : Boolean;
    isToday : Boolean;
    isOther : boolean;
    resource : TJSObject;
    el : TJSHTMLElement;
  end;

  TSlotLabelRenderInfo = Record
    date : TJSDate;
    text : string;
    isPast : Boolean;
    isFuture : Boolean;
    isToday : Boolean;
    lebel : Integer;
    el : TJSHTMLElement;
  end;
  TSlotLaneRenderInfo = TSlotLabelRenderInfo;

  TWeekNumberRenderInfo = Record
    num : integer;
    text : string;
    date : TJSDate;
  end;

  TCalendarEventClassNamesCallback = reference to function(Info : TCalendarEventRenderInfo) : string;
  TCalendarEventContentCallback = reference to function (Info : TCalendarEventRenderInfo) : TCalendarEventContentObj;
  TCalendarEventMountCallback = reference to Procedure (Info : TCalendarEventRenderInfo);

  TEventSortCallBack = reference to function (ev1,ev2 : TCalendarEvent) : Integer;
  TCalendarEventSourceArray = Array of TCalendarEventSource;
  TAddEventCallBack = reference to procedure (addInfo : TAddEventInfo);
  TChangeEventCallBack = reference to procedure (changeInfo : TChangeEventInfo);
  TRemoveEventCallBack = reference to procedure (removeInfo : TRemoveEventInfo);

  TCalendarViewClassNamesCallback = reference to function(Info : TCalendarViewRenderInfo) : string;
  TCalendarViewMountCallback = reference to Procedure (Info : TCalendarViewRenderInfo);

  TDayHeaderClassNamesCallback = reference to function(Info : TDayHeaderRenderInfo) : string;
  TDayHeaderContentStrCallback = reference to function (Info : TDayHeaderRenderInfo) : string;
  TDayHeaderContentObjCallback = reference to function (Info : TDayHeaderRenderInfo) : TCalendarEventContentObj;
  TDayHeaderMountCallback = reference to Procedure (Info : TDayHeaderRenderInfo);

  TDayCellClassNamesCallback = reference to function(Info : TDayCellRenderInfo) : string;
  TDayCellContentStrCallback = reference to function (Info : TDayCellRenderInfo) : string;
  TDayCellContentObjCallback = reference to function (Info : TDayCellRenderInfo) : TCalendarEventContentObj;
  TDayCellMountCallback = reference to Procedure (Info : TDayCellRenderInfo);

  TSlotLabelClassNamesCallback = reference to function(Info : TSlotLabelRenderInfo) : string;
  TSlotLabelContentStrCallback = reference to function (Info : TSlotLabelRenderInfo) : string;
  TSlotLabelContentObjCallback = reference to function (Info : TSlotLabelRenderInfo) : TCalendarEventContentObj;
  TSlotLabelMountCallback = reference to Procedure (Info : TSlotLabelRenderInfo);

  TSlotLaneClassNamesCallback = reference to function(Info : TSlotLaneRenderInfo) : string;
  TSlotLaneContentStrCallback = reference to function (Info : TSlotLaneRenderInfo) : string;
  TSlotLaneContentObjCallback = reference to function (Info : TSlotLaneRenderInfo) : TCalendarEventContentObj;
  TSlotLaneMountCallback = reference to Procedure (Info : TSlotLaneRenderInfo);

  TweekNumberClassNamesCallback = reference to function(Info : TweekNumberRenderInfo) : string;
  TweekNumberContentStrCallback = reference to function (Info : TweekNumberRenderInfo) : string;
  TweekNumberContentObjCallback = reference to function (Info : TweekNumberRenderInfo) : TCalendarEventContentObj;
  TweekNumberMountCallback = reference to Procedure (Info : TweekNumberRenderInfo);

  
  
  TFullCalendarOptions = Class external name 'Object' (TJSObject)
    plugins : TStringDynArray;
    pluginRaw : TJSArray; external name 'plugins';
    rerenderDelay : NativeInt;
    initialDate : TJSDate;
    initialDateStr : string; external name 'initialDate';
    initialDateInt : nativeInt; external name 'initialDate';
    defaultAllDay : Boolean;
    defaultAllDayEventDuration : TDuration;
    defaultAllDayEventDurationStr : String; external name 'defaultAllDayEventDuration';
    defaultTimedEventDuration : TDuration;
    defaultTimedEventDurationStr : String; external name 'defaultTimedEventDuration';
    forceEventDuration : Boolean;
    eventDisplay : string;
    eventAdd : TAddEventCallBack; 
    eventChange : TChangeEventCallBack; 
    eventRemove : TRemoveEventCallBack; 
    eventColor : String;
    eventBorderColor : String;
    eventTextColor : String;
    eventBackgroundColor : String;
    dateIncrement : TDuration;
    dateIncrementStr : string; external name 'dateIncrement';
    dateAlignment : String;
    validRange : TDateRange;
    headerToolbar : TCalendarHeaderFooterOptions;
    headerToolbarBool : Boolean; external name 'headerToolbar';
    footerToolbar : TCalendarHeaderFooterOptions;
    footerToolbarBool : Boolean; external name 'footerToolbar';
    titleFormat : TDateFormatter;
    titleFormatStr : string; external name 'titleFormat';
    titleFormatFunc : TDateFormatHandler; external name 'titleFormat';
    titleRangeSeparator : String;
    buttonText : TButtonText;
    buttonTextRec : TButtonTextRec;
    buttonIcons : TButtonIcons;
    buttonIconsRec : TButtonIconsRec;
    customButtons : TCustomButtonSpecs;
    customButtonsObj : TJSObject;
    themeSystem : string;
    bootstrapFontAwesome : TFontAwesomeSpec;
    bootstrapFontAwesomeRec : TFontAwesomeSpecRec;
    weekends : boolean;
    hiddenDays : TNativeIntDynArray;
    dayHeader : Boolean;
    dayHeaderFormat : TDateFormatter;
    dayHeaderClassNames : String;
    dayHeaderClassNamesFunc : TDayHeaderClassnamesCallback; external name 'dayHeaderClassNames';
    dayHeaderContent : TDayHeaderClassnamesCallback;
    dayHeaderContentStr : TDayHeaderContentStrCallback; external name 'dayHeaderContent';
    dayHeaderContentObj : TDayHeaderContentObjCallback; external name 'dayHeaderContent';
    dayHeaderDidMount : TDayHeaderMountCallBack;
    dayHeaderWillUnmount : TDayHeaderMountCallBack;

    dayCellClassNames : String;
    dayCellClassNamesFunc : TDayCellClassnamesCallback; external name 'dayCellClassNames';
    dayCellContent : TDayCellClassnamesCallback;
    dayCellContentStr : TDayCellContentStrCallback; external name 'dayCellContent';
    dayCellContentObj : TDayCellContentObjCallback; external name 'dayCellContent';
    dayCellDidMount : TDayCellMountCallBack;
    dayCellWillUnmount : TDayCellMountCallBack;
    
    slotDuration : TDuration;
    slotDurationStr : string; external name 'slotDuration';
    slotLabelInterval : TDuration;
    slotLabelIntervalStr : string; external name 'slotLabelInterval';
    slotLabelFormat : TDateFormatter;
    slotLabelFormatStr : String;
    slotMinTime : TDuration;
    slotMinTimeStr : string; external name 'slotMinTime';
    slotMaxTime : TDuration;
    slotMaxTimeStr : string; external name 'slotMaxTime';
    slotMinWidth : Integer;
    
    slotLabelClassNames : String;
    slotLabelClassNamesFunc : TSlotLabelClassNamesCallback; external name 'slotLabelClassNames';
    slotLabelContent : String;
    slotLabelContentObj : TSlotLabelContentObj; external name 'slotLabelContent';
    slotLabelContentObjFunc : TSlotLabelContentObjCallback; external name 'slotLabelContent';
    slotLabelContentStrFunc : TSlotLabelContentStrCallback; external name 'slotLabelContent';
    slotLabelDidMount : TSlotLabelMountCallback;
    slotLabelWillUnmout : TSlotLabelMountCallback;

    slotLaneClassNames : String;
    slotLaneClassNamesFunc : TSlotLaneClassNamesCallback; external name 'slotLaneClassNames';
    slotLaneContent : String;
    slotLaneContentObj : TSlotLaneContentObj; external name 'slotLaneContent';
    slotLaneContentObjFunc : TSlotLaneContentObjCallback; external name 'slotLaneContent';
    slotLaneContentStrFunc : TSlotLaneClassNamesCallback; external name 'slotLaneContent';
    slotLaneDidMount : TSlotLaneMountCallback;
    slotLaneWillUnmout : TSlotLaneMountCallback;
    
    weekText : string;
    weekNumberFormat : TDateFormatter;
    weekNumberFormatStr : String; external name 'weekNumberFormat';
    weekNumberClassNames : String;
    weekNumberClassNamesFunc : TweekNumberClassNamesCallback; external name 'weekNumberClassNames';
    weekNumberContent : String;
    weekNumberContentObj : TweekNumberContentObj; external name 'weekNumberContent';
    weekNumberContentObjFunc : TweekNumberContentObjCallback; external name 'weekNumberContent';
    weekNumberContentStrFunc : TweekNumberClassNamesCallback; external name 'weekNumberContent';
    weekNumberDidMount : TweekNumberMountCallback;
    weekNumberWillUnmout : TweekNumberMountCallback;
    
    
    scrollTime : TDuration;
    scrollTimeStr : string; external name 'scrollTime';
    firstDay : Integer;
    locale : string;
    dir : string;
    height : Integer;
    heightStr : String; external name 'height';
    heightFunc : THeightHandler; external name 'height';
    contentHeight : Integer;
    contentHeightStr : String; external name 'contentHeight';
    contentHeightFunc : THeightHandler; external name 'contentHeight';
    aspectRatio : Double;
    handleWindoResize : Boolean;
    windowResizeDelay  : Integer;
    showNonCurrentDates : Boolean;
    fixedWeekCount : Boolean;
    businessHours : TBusinessHoursSpec;
    businessHoursBool : Boolean; external name 'businessHours';
    businessHoursArray : TBusinessHoursSpecArray; external name 'businessHours';
    nowIndicator : Boolean;
    now : TJSDate;
    nowStr : string; external name 'now';
    nowInt : nativeInt; external name 'now';
    nowFunc : TDateFunction; external name 'now';
    eventLimit : Boolean;
    eventLimitInt : Integer; external name 'eventLimit';
    events : TBaseCalendarEventArray;
    eventsStr : string; external name 'events'; // JSON feed
    eventsFunc : TCalendarEventGenerator; external name 'events'; // JSON feed
    eventsJSONFeed : TJSONFeedSpec; external name 'events'; // JSON feed
    eventsArr : TCalendarEventArray; external name 'events'; // JSON feed
    eventsObjList : TJSObjectDynArray ; external name 'events'; // Roll your own
    eventSources : TCalendarEventSourceArray;
    eventClassNames : String;
    eventClassNamesFunc : TCalendarEventClassNamesCallback; external name 'eventClassNames';
    eventContent : String;
    eventContentObj : TCalendarEventContentObj; external name 'eventContent';
    eventContentObjFunc : TCalendarEventContentCallback; external name 'eventContent';
    eventContentStrFunc : TCalendarEventClassNamesCallback; external name 'eventContent';
    eventDidMount : TCalendarEventMountCallback;
    eventWillUnmout : TCalendarEventMountCallback;
    eventTimeFormat : TDateFormatter;
    eventOrder : String;
    eventOrderArr : TStringDynArray; external name 'eventOrder';
    eventOrderFunc : TEventSortCallBack; external name 'eventOrder';  
    progressiveEventRendering : Boolean;
    
    displayEventTime : Boolean;
    displayEventEnd: Boolean;
    nextDayTreshold : String;
    startParam : string;
    endParam : string;
    timeZoneParam : string;
    timeZone : string;
    lazyFetching : Boolean;
    loading : TCalendarLoadingCallback;
    selectable : boolean;
    selectMirror : Boolean;
    unselectAuto : Boolean;
    unselectCancel : string;
    selectOverlap : boolean;
    selectOverlapFunc : TSelectOverlapHandler; external name 'selectOverlap';
    selectAllow : TSelectAllowHandler;
    selectMinDistance : Integer;
    selectConstraint: TBusinessHoursSpec;
    eventClick : TEventMouseEventHandler;
    eventMouseEnter : TEventMouseEventHandler;
    eventMouseLeave : TEventMouseEventHandler;
    listDayFormat : TDateFormatter;
    listDayBool : Boolean; external name 'listDayFormat';
    listDayAltFormat : TDateFormatter;
    listDayAltBool : Boolean; external name 'listDayAltFormat';
    noEventsMessage : String;
    weekNumbers : Boolean;
    dayMaxEvents : Integer;
    dayMaxEventsBool : Boolean; external name 'dayMaxEvents';
    initialView : string;
    viewClassNames : String;
    viewClassNamesFunc : TCalendarViewClassNamesCallback; external name 'eventClassNames';
    viewDidMount : TCalendarViewMountCallback; 
    viewWillUnmount : TCalendarViewMountCallback;
  end;

  { TFullCalendarView }
  TDateSelector = Class external name 'Object' (TJSObject)
    start : TJSDate;
    startStr : string; external name 'start';
    startInt : NativeInt; external name 'start';
    end_  : TJSDate;
    endStr : string; external name 'end';
    endInt : NativeInt; external name 'end';
    allDay : Boolean;
    resourceId : String;
  end;

  TFullCalendarView = Class external name 'Object' (TJSObject)
  private
    FType: string; external name 'type';
  Public
    title : string;
    activeStart : TJSDate;
    activeEnd : TJSDate;
    currentStart : TJSDate;
    currentEnd : TJSDate;
    Property type_ : string read FType;
  end;

  TFullCalendarEventInfo = record
    view : TFullCalendarView;
    el : TJSHTMLElement;
  end;

  TDayRenderInfo = record
    date : TJSDate;
    view : TFullCalendarView;
    el : TJSHTMLElement;
  end;

  TDateClickInfo = record
    date : TJSDate;
    dateStr : string;
    allDay : Boolean;
    dayEl : TJSHTMLElement;
    jsEvent : TJSEvent;
    view : TFullCalendarView;
    resource : TCalendarResource;
  end;

  TDateSelectInfo = record
    start : TJSDate;
    startStr : string;
    end_ : TJSDate; external name 'end';
    endStr : string;
    allDay : Boolean;
    jsEvent : TJSEvent;
    view : TFullCalendarView;
    resource : TCalendarResource;
  end;


  TFullCalendarEvent = reference to function(Info :TFullCalendarEventInfo) : Boolean;
  TDayRenderEvent = reference to function(Info : TDayrenderInfo) : Boolean;
  TViewEvent = reference to function(View : TFullCalendarView) : Boolean;
  TDateClickEvent = reference to procedure (Info: TDateClickInfo);
  TDateSelectEvent = reference to procedure (Info: TDateSelectInfo);
  TDateUnSelectEvent = reference to procedure (event: TJSEvent; View : TFullCalendarView);



  TFullCalendar = Class external name 'FullCalendar.Calendar' (TJSObject)
    Constructor new(el : TJSHTMLElement; Options : TFullCalendarOptions) ; // external name 'Calendar';
    class function Calendar(el : TJSHTMLElement; Options : TFullCalendarOptions) : TFullCalendar; static;
    function getOption(aName : String) : JSValue;
    procedure setOption(aName : string; aValue : JSValue);
    procedure render;
    procedure destroy;
    Procedure batchRendering(aBatch : TProcedural);
    Procedure prev;
    Procedure next;
    Procedure prevYear;
    Procedure nextYear;
    Procedure today;
    Procedure gotoDate(aDate : string); overload;
    Procedure gotoDate(aDate : NativeInt); overload;
    Procedure gotoDate(aDate : TJSDate); overload;
    Procedure incrementDate(aDuration : TDuration); overload;
    Procedure incrementDate(aDuration : string);overload;
    Procedure incrementDate(aDuration : NativeInt);overload;
    Procedure on_(eventName : string; aHandler : TFullCalendarEvent); external name 'on';
    Procedure on_(eventName : string; aHandler : TDayRenderEvent);external name 'on';
    Procedure on_(eventName : string; aHandler : TViewEvent);external name 'on';
    Procedure on_(eventName : string; aHandler : TDateSelectEvent);external name 'on';
    Procedure on_(eventName : string; aHandler : TDateClickEvent);external name 'on';
    Procedure on_(eventName : string; aHandler : TDateUnSelectEvent);external name 'on';
    Procedure off(eventName : string); overload;
    Procedure off(eventName : string; aHandler : TFullCalendarEvent); overload;
    Procedure off(eventName : string; aHandler : TDayRenderEvent);
    Procedure off(eventName : string; aHandler : TViewEvent);
    Procedure off(eventName : string; aHandler : TDateClickEvent);
    Procedure off(eventName : string; aHandler : TDateUnSelectEvent);
    function GetDate : TJSDate;
    function View : TFullCalendarView;
    procedure changeView(aViewName : string); overload;
    procedure changeView(aViewName : string; aDate : TJSDate); overload;
    procedure changeView(aViewName : string; aDate : String); overload;
    procedure changeView(aViewName : string; aDate : NativeInt); overload;
    procedure changeView(aViewName : string; aDate : TDateRange); overload;
    procedure scrollToTime(aTime : TDuration); overload;
    procedure scrollToTime(aTime : String); overload;
    procedure scrollToTime(aTime : NativeInt); overload;
    Procedure updateSize;
    Function getEvents : TCalendarEventArray;
    Function getEvent(aID : NativeInt) :TCalendarEvent; overload;
    Function getEvent(aID : String) : TCalendarEvent; overload;
    Function addEvent(aEvent : TBaseCalendarEvent) : TCalendarEvent; overload;
    Function addEvent(aEvent : TBaseCalendarEvent; Source : String) : TCalendarEvent; overload;
    Function addEvent(aEvent : TBaseCalendarEvent; Source : TBaseCalendarEvent) : TCalendarEvent; overload;
    Function getEventSources : TCalendarEventSourceArray;
    Function getEventSourcebyId(aID: String) : TCalendarEventSource;
    Procedure addEventSource(aSource : TCalendarEventGenerator);
    Procedure addEventSource(aSource : TJSONFeedSpec);
    Procedure addEventSource(aSource : TCalendarEventArray);
    Procedure addEventSource(aSource : TBaseCalendarEventArray);
    Procedure addEventSource(aSource : String);
    procedure refetchEvents;
    procedure Select(aStart : TJSDate; aEnd : TJSDate);
    procedure Select(aStart : String; aEnd : String);
    procedure Select(aStart : NativeInt; aEnd : NativeInt);
    procedure Select(aStart : TJSDate);
    procedure Select(aStart : String);
    procedure Select(aStart : NativeInt);
    procedure Select(aSel : TDateSelector);
    procedure unselect;


    Function getAvailableLocaleCodes : TStringDynArray;
    Property Date : TJSDate Read GetDate Write GotoDate;
  end;

implementation

{ TBaseCalendarEventHelper }

class function TBaseCalendarEventHelper.event(const aTitle : String; aStart,aEnd : TDateTime) : TBaseCalendarEvent;

begin
  Result:=TBaseCalendarEvent.new;
  Result.title:=aTitle;
  Result.Start:=DateTimeToJSDate(aStart);
  Result.end_:=DateTimeToJSDate(aEnd);
end;

end.
