program demofullcalendar5;

{$mode objfpc}

uses
  dateutils, browserapp, types, JS, Classes, SysUtils, Web, libjquery, libbootstrap, libfullcalendar5;

type

  { TMyApplication }

  TMyApplication = class(TBrowserApplication)
    FCalendarEl : TJSHTMLElement;
    FCalendar : TFullCalendar;
    procedure doRun; override;
  private
    function CreateEvents: TBaseCalendarEventArray;
    procedure DoEventMount(Info: TCalendarEventRenderInfo);
    procedure ShowLocales;
  end;

Function TMyApplication.CreateEvents : TBaseCalendarEventArray;

Const
  BuySell : Array[1..3] of string = ('Buy','Sell','Borrow');
  Articles : Array[1..10] of String = ('books','sugar','coffee','apples','bananas','smartphone','bread','meat','cabbage','tomatoes');
  Colors : Array[1..10] of string = ('silver','gray','salmon','plum','mediumpurple','slateblue','lime','seagreen','darkgreen','teal');

Var
  Ev,AllDay : Boolean;
  D1,D2 : TDateTime;
  E : TBaseCalendarEvent;
  I : Integer;

begin
  SetLength(Result,62);
  For I:=1 to 62 do
    begin
    D1:=Date-31+i;
    if DayOfTheWeek(D1)=7 then
     D1:=D1+1;
    Ev:=(DayOfTheWeek(D1) mod 2=0);
    allDay:=(I mod 5)=0;
    if allDay then
      D2:=D1
    else
      begin
      D1:=IncHour(D1,9+Random(12));
      D1:=IncMinute(D1,Random(12)*5);
      D2:=IncMinute(D1,10*Random(20));
      end;
    E:=TBaseCalendarEvent.event(BuySell[1+Random(3)]+' '+Articles[1+Random(10)],D1,D2);
    E.color:=Colors[1+Random(10)];
    E.allDay:=allDay;
    if not Assigned(E.extendedProps) then
      E.extendedProps:=TJSObject.new;
    E.extendedProps['description']:='A very long description to show that this event ('+BuySell[1+Random(3)]+' '+Articles[1+Random(10)]+') can also be put in a popup';
    Result[I-1]:=E;
    end;
end;

procedure TMyApplication.DoEventMount(Info: TCalendarEventRenderInfo);
Var
  opts : TBootstrapPopoverOptions;
  T : String;
begin
  opts:=TBootstrapPopoverOptions.New;
  opts.title:='Event details';
  opts.content:=String(info.event.extendedProps.properties['description']);
  opts.placement:='top';
  opts.trigger:='hover';
  opts.container:='body';
  JQuery(info.el).popover(opts);

end;

procedure TMyApplication.ShowLocales;

Var
  Locales : TStringDynArray;
  CB,O : TJSHTMLElement;
  S : String;

begin
  CB:=GetHTMLElement('locale-selector');
  Locales:=FCalendar.getAvailableLocaleCodes();
  for S in Locales do
    begin
    O:=createHTMLElement('option');
    O['value'] := S;
    O.innerText :=S;
    CB.appendChild(O);
    end;
  CB.addEventListener('change',TJSRawEventHandler (Procedure (Event: TJSEvent)
    var v : String;
    begin
     V:=String(event.Target['value']);
     if (V<>'') then
       Fcalendar.setOption('locale',V);
    end
  ));
end;


procedure TMyApplication.doRun;

Var
  O : TFullCalendarOptions;

begin
  // Your code here
  Terminate;
  FCalendarEl:=GetHTMLElement('thecalendar');
  O:=TFullCalendarOptions.New;
  O.Events:=CreateEvents;
//  O.plugins:=['interaction', 'dayGrid', 'timeGrid','bootstrap'];
  O.themeSystem:='bootstrap';
  O.initialView:='dayGridMonth';
  O.headerToolbar:=TCalendarHeaderFooterOptions.New;
  O.headerToolbar.center:='title';
  O.headerToolbar.start:='prev,next today';
  O.headerToolbar.end_:='dayGridMonth,timeGridWeek,timeGridDay';
  O.eventDidMount:=@DoEventMount;
  FCalendar:=TFullCalendar.new(FCalendarEl,O);
  FCalendar.render;
  ShowLocales;
end;

var
  Application : TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
end.
