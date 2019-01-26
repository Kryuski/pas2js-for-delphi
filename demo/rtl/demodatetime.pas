program demodatetime;

uses sysutils, js;

Procedure DumpDate(Msg : String; Dt : TDateTime);

Var
  Y,M,D : Word;

begin
  DecodeDate(Dt,Y,M,D);
  Writeln(Msg,' : ',Y,'-',M,'-',D,' (',Dt,')');
end;

Procedure DumpTime(Msg : String; Dt : TDateTime);

Var
  H,M,S,Z : Word;

begin
  DecodeTime(Frac(Dt),H,M,S,z);
  if z<>0 then
    Writeln(Msg,' : ',H,':',M,':',S,'.',z,' (',Frac(Dt),')')
  else
    Writeln(Msg,' : ',H,':',M,':',S,' (',Frac(Dt),')')
end;

Procedure DumpDateTime(Msg : String; Dt : TDateTime);

Var
  Y,Mo,Da,H,M,S,Z : Word;

begin
  DecodeDate(Dt,Y,Mo,Da);
  DecodeTime(Frac(Dt),H,M,S,z);
  if z<>0 then
    Writeln(Msg,' : ',Y,'-',Mo,'-',Da,' ',H,':',M,':',S,'.',z,' (',Dt,')')
  else
    Writeln(Msg,' : ',Y,'-',Mo,'-',Da,' ',H,':',M,':',S,' (',Dt,')')
end;

Var
  Dt : TDateTime;

begin
  Dt:=Date;
  DumpDate('Date',Dt);
  Dt:=Time;
  DumpTime('Time',dt);
  Dt:=Now;
  DumpDateTime('Now',Dt);
  Writeln('DateToStr : ',DateToStr(Dt));
  Writeln('TimeToStr : ',TimeToStr(Dt));
  DumpTime('StrToTime',StrToTime('14:15:16'));
  DumpDate('StrToDate (yyyy-mm-dd)',StrToDate('2016-10-12'));
  ShortDateFormat:='mm-dd-yyyy';
  DumpDate('StrToDate (mm-dd-yyyy)',StrToDate('10-16-2016'));
  ShortDateFormat:='dd-mm-yyyy';
  DumpDate('StrToDate (dd-mm-yyyy)',StrToDate('17-10-2016'));
end.

