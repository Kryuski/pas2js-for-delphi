program main;

{$mode objfpc}

uses
  browserapp, JS, Classes, SysUtils, Web;

type
  TTrack = Record
    name,
    artist,
    image,
    path,
    display,
    url : String;
  end;

  TMyApplication = class(TBrowserApplication)
    now_playing,
    track_art,
    track_name,
    track_artist,
    display_text,
    playpause_btn,
    next_btn,
    prev_btn,
    curr_time,
    total_duration : TJSHTMLElement;
    curr_track : TJSHTMLAudioElement;
    volume_slider,
    seek_slider : TJSHTMLinputElement;
    isPlaying : Boolean;
    track_index : Integer;
    updateTimer: Integer;
    Procedure BindElements;
    Procedure seekTo(Event: TJSEvent);
    Procedure SetVolume(Event: TJSEvent);
    Procedure DoPlayPause(Event: TJSEvent);
    Procedure NextTrack(Event: TJSEvent);
    Procedure PrevTrack(Event: TJSEvent);
    Procedure PlayTrack;
    Procedure PauseTrack;
    procedure LoadTrack(aIndex : Integer);
    procedure random_bg_color;
    procedure resetValues;
    procedure seekUpdate;
    procedure doRun; override;
  private
  end;

var
  track_list : Array of TTrack; external name 'track_list';


procedure TMyApplication.seekUpdate;

Var
  seekPosition : Double;
  currentMinutes,
  currentSeconds,
  durationMinutes,
  durationSeconds : Integer;

begin
  if jsIsNan(curr_track.duration) then exit;
  seekPosition:=curr_track.currentTime * (100 / curr_track.duration);
  seek_slider.value :=FloatToStr(seekPosition);
  currentMinutes:=Round(curr_track.currentTime / 60);
  currentSeconds:=Round(curr_track.currentTime - currentMinutes * 60);
  durationMinutes:=Round(curr_track.duration / 60);
  durationSeconds:=Round(curr_track.duration - durationMinutes * 60);
  curr_time.innerText:= Format('%.2:%d2',[currentMinutes,currentSeconds]);
  total_duration.innerText:=Format('%.2:%d2',[durationMinutes,durationSeconds]);
end;

procedure TMyApplication.BindElements;

  Function GetElement (aClass : String) : TJSHTMLELement;

  begin

    Result:=TJSHTMLELement(Document.querySelector('.'+aClass));
    Writeln('Looking for ',aClass,' : ',assigned(Result));
  end;

begin
  now_playing :=GetElement('now-playing');
  track_art :=GetElement('track-art');
  track_name :=GetElement('track-name');
  track_artist :=GetElement('track-artist');
  display_text :=GetElement('display-text');

  playpause_btn :=GetElement('playpause-track');
  playpause_btn.AddEventListener('click',@DoPlayPause);
  next_btn :=GetElement('next-track');
  next_btn.AddEventListener('click',@NextTrack);
  prev_btn :=GetElement('prev-track');
  prev_btn.AddEventListener('click',@PrevTrack);
  seek_slider :=TJSHTMLInputElement(GetElement('seek_slider'));
  seek_slider.AddEventListener('change',@SeekTo);
  volume_slider :=TJSHTMLInputElement(GetElement('volume_slider'));
  volume_slider.AddEventListener('change',@SetVolume);
  curr_time :=GetElement('current-time');
  total_duration :=GetElement('total-duration');
end;

procedure TMyApplication.seekTo(Event: TJSEvent);

Var
  dSeekTo : Double;

begin
  dSeekTo := curr_track.duration * (StrToFloatDef(seek_slider.value,50) / 100);
  curr_track.currentTime := dSeekTo;
end;

procedure TMyApplication.SetVolume(Event: TJSEvent);
begin
  curr_track.volume := StrToFloatDef(volume_slider.value,50) / 100;
end;

procedure TMyApplication.DoPlayPause(Event: TJSEvent);
begin
  if isPlaying then
    pauseTrack
  else
    playTrack;
end;

procedure TMyApplication.NextTrack(Event: TJSEvent);
begin
  // Go back to the first track if the
  // current one is the last in the track list
  if (track_index < (Length(track_list) - 1)) then
    Inc(track_index)
  else
    track_index:=0;
  // Load and play the new track
  loadTrack(track_index);
  playTrack();
end;

procedure TMyApplication.PrevTrack(Event: TJSEvent);
begin
  if (track_index > 0) then
    Dec(track_index)
  else
   track_index := Length(track_list)- 1;
  loadTrack(track_index);
  playTrack();
end;

procedure TMyApplication.PlayTrack;
begin
  curr_track.play();
  isPlaying:=true;
  playpause_btn.innerHTML := '<i class="fa fa-pause-circle fa-5x"></i>';
end;

procedure TMyApplication.PauseTrack;
begin
  curr_track.pause();
  isPlaying:=false;
  playpause_btn.innerHTML := '<i class="fa fa-play-circle fa-5x"></i>';
end;

procedure TMyApplication.LoadTrack(aIndex: Integer);
begin
  // Clear the previous seek timer
  window.clearInterval(updateTimer);
  resetValues();

  curr_track.src :=track_list[track_index].path;
  curr_track.load();

  track_art.style.SetProperty('background-image','url("' + track_list[track_index].image+ '")');
  track_art.style.SetProperty('background-image','url("' + track_list[track_index].image+ '")');
  track_artist.InnerHTML := track_list[track_index].artist;
  track_name.InnerHTML := track_list[track_index].name;
  display_text.InnerHTML:=String(track_list[track_index].display);
  now_playing.InnerHTML := Format('PLAYING %d OF %d',[track_index + 1,Length(track_list)]);
  updateTimer :=Window.setInterval(@seekUpdate, 1000);
  curr_track.addEventListener('ended', @nextTrack);
  random_bg_color();
end;

procedure TMyApplication.random_bg_color;

Const
    Lim = 256 - 64;

Var
  red,Green,blue : Integer;
  col : string;
begin
  red:=Round(random(Lim) + 64);
  green:=Round(random(lim) + 64);
  blue:=Round(random(lim)+64);
  col:= Format('rgb(%d,%d,%d)',[red, green ,blue]);
  TJSHTMLELement(document.body).style.SetProperty('background',col);
end;

procedure TMyApplication.resetValues;
begin
  curr_time.InnerHTML:= '00:00';
  total_duration.InnerHTML:= '00:00';
  seek_slider.value:='0';
end;

procedure TMyApplication.doRun;

begin
  Terminate;
  BindElements;
  curr_track:=TJSHTMLAudioElement(document.createElement('audio'));
  curr_track.autoplay:=true;
  ResetValues;
  Window.SetInterval(@seekUpdate,1000);
  LoadTrack(0);
end;

var
  Application : TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
end.
