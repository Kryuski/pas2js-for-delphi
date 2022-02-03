{
    This file is part of the Pas2JS run time library.
    Copyright (C) 2019 Michael Van Canneyt

    FullCalendar version 4 mappings for pas2js

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit libfullcalendar4;

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
    left : string;
    right : string;
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
    rendering : string;
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
  TGenerateEventsFailureCallBack = Procedure (Res : TBaseCalendarEventArray);
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
    isMirror : Boolean;
    isStart : Boolean;
    isEnd : Boolean;
    view : TFullCalendarView;
  end;


  TCalendarEventRenderCallback = reference to procedure(Info : TCalendarEventRenderInfo);

  TCalendarEventSourceArray = Array of TCalendarEventSource;

  TFullCalendarOptions = Class external name 'Object' (TJSObject)
    plugins : TStringDynArray;
    pluginRaw : TJSArray; external name 'plugins';
    rerenderDelay : NativeInt;
    defaultDate : TJSDate;
    defaultDateStr : string; external name 'defaultDate';
    defaultDateInt : nativeInt; external name 'defaultDate';
    dateIncrement : TDuration;
    dateIncrementStr : string; external name 'dateIncrement';
    dateAlignment : String;
    validRange : TDateRange;
    defaultView : string;
    header : TCalendarHeaderFooterOptions;
    headerBool : Boolean; external name 'header';
    footer : TCalendarHeaderFooterOptions;
    footerBool : Boolean; external name 'footer';
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
    columnHeader : Boolean;
    columnHeaderFormat : TDateFormatter;
    columnHeaderText : TDateFormatHandler;
    columnHeaderHTML : TDateFormatHandler;
    slotDuration : TDuration;
    slotDurationStr : string; external name 'slotDuration';
    slotLabelInterval : TDuration;
    slotLabelIntervalStr : string; external name 'slotLabelInterval';
    slotLabelFormat : TDateFormatter;
    slotLabelFormatStr : String;
    minTime : TDuration;
    minTimeStr : string; external name 'minTime';
    maxTime : TDuration;
    maxTimeStr : string; external name 'maxTime';
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
    eventRender : TCalendarEventRenderCallback;
    startParam : string;
    endParam : string;
    timeZoneParam : string;
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
