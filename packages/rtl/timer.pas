{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2018 by warleyalex <warleyalex@yahoo.com.br>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}

unit timer;

Interface

uses JS, Web, Classes;

type
  TTimer = class(TComponent)
  private
    FEnabled: Boolean;  // True
    FInterval: Integer; // 1000
    FOnTimer: TNotifyEvent;
    FID: Integer;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Integer);
  protected
    function DesignEvent: Boolean; virtual;
    procedure DoUpdateTimer; virtual;
    procedure DoClearTimer; virtual;
    procedure DoTimer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Specifies whether the timer is enabled }
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    { Specifies the interval, in milliseconds, between timer events }
    property Interval: Integer read FInterval write SetInterval default 1000;
    property Tag;
    { Fired when the timer interval has elapsed }
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

implementation

{ TTimer }

constructor TTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FInterval := 1000;
  DoUpdateTimer;
end;

destructor TTimer.Destroy;
begin
  Enabled := False;
  inherited Destroy;
end;

function TTimer.DesignEvent: Boolean;
begin
  Result := False;
end;

procedure TTimer.DoUpdateTimer;
begin
  FID := window.setInterval(@DoTimer, FInterval);
end;

procedure TTimer.DoClearTimer;
begin
  if (FID <> 0) then
  begin
    window.clearInterval(FID);
    FID := 0;
  end;
end;

procedure TTimer.SetEnabled(Value: Boolean);
begin
  if (Value <> FEnabled) then
  begin
    if Value then
      DoUpdateTimer
    else
      DoClearTimer;
    FEnabled := Value;
  end;
end;

procedure TTimer.SetInterval(Value: Integer);
begin
  if (Value <> FInterval) then
  begin
    if Enabled then
    begin
      Enabled := False;
      try
        FInterval := Value;
      finally
        Enabled := True;
      end;
    end
    else
      FInterval := Value;
  end;
end;

procedure TTimer.DoTimer;
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

end.
