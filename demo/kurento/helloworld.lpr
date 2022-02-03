program helloworld;

{$mode objfpc}

uses
  JS, Classes, SysUtils, Web, libkurento, node.events, browserapp, browserconsole;

Type

   { TKurentoApp }

   TKurentoApp = class(TBrowserApplication)
   Private
     FURL : TJSHTMLInputElement;
     FVideoInput,
     FVideoOutput,
     FStartButton,
     FStopButton : TJSHTMLElement;
     FWebRTCPeer : TWebRtcPeer;
     FClient: TKurentoClient;
     FPipeLine : TKurentoMediaPipeline;
     FRTCEndPoint : TKurentoWebRtcEndPoint;
     FMyOffer: TKurentoOffer;
     // Helper routine to display offers.
     procedure DoError(aError: TJSError);
     // Helper routine to set needed ICE callbacks.
     procedure setIceCandidateCallbacks(webRtcPeer : TWebRtcPeer; webRtcEp : TKurentoWebRtcEndPoint; onerror : TErrorCallBack);
     // Here we start the ball rolling , we create the WebRTCPeer, and create an offer
     function DoStart(aEvent: TJSMouseEvent): boolean;
     function DoStop(aEvent: TJSMouseEvent): boolean;
     // When the offer is received, we create a Kurento client
     procedure OnOfferReceived(aError: TJSError; aOffer: TKurentoOffer);
     // When the kurento client is created, we use it to create a media pipeline
     procedure HaveClient(aError: TJSError; aClient: TKurentoClient);
     // When the pipeline is created, we use it to create an endpoint for it
     procedure HavePipeLine(aError: TJSError; aResult: TKurentoMediaObject);
     // When the endpoint was created, we handle ICE candidates, process the offer, and start gathering candidates
     // In the end, we connect. When the connect succeeds, we're all done.
     procedure HaveEndPoint(aError: TJSError; aResult: TKurentoMediaObject);
     // When the offer is processed, here we handle the answer.
     procedure OfferProcessed(aError: TJSError; aAnswer: TKurentoAnswer);
     // Helper routines to Show/Hide a spinner.
     procedure HideSpinner(Els: array of TJSHTMLElement);
     procedure ShowSpinner(Els: array of TJSHTMLElement);
   Public
     procedure DoRun; override;
   end;

procedure TKurentoApp.OfferProcessed(aError: TJSError; aAnswer: TKurentoAnswer);

begin
  if assigned(aError) then
    DoError(aError)
  else
    FWebRTCPeer.processAnswer(aAnswer,@DoError);
end;

Procedure TKurentoApp.HaveEndPoint(aError : TJSError; aResult : TKurentoMediaObject);

begin
  Writeln('TKurentoApp.HaveEndPoint callback. Have error:',Assigned(aError));
  if assigned(aError) then
    DoError(aError)
  else
    begin
    FRTCEndPoint:= TKurentoWebRtcEndPoint(aResult);
    setIceCandidateCallbacks(FWebRTCPeer,FRTCEndPoint,@DoError);
    FRTCEndPoint.processOffer(FMyOffer,@OfferProcessed);
    FRTCEndPoint.gatherCandidates(@DoError);
    FRTCEndPoint.connect(FRTCEndPoint,Procedure(aError : TJSError)
      begin
      if Assigned(aError) then
        DoError(aError)
      else
        Writeln('We should be all set')
      end);
    end;
end;

Procedure TKurentoApp.HavePipeLine(aError : TJSError; aResult : TKurentoMediaObject);

begin
   Writeln('TKurentoApp.HavePipeline callback. Have error:',Assigned(aError));
  if assigned(aError) then
    DoError(aError)
  else
    begin
    FPipeLine:=TKurentoMediaPipeline(aResult);
    FPipeLine.Create('WebRtcEndpoint',@HaveEndPoint);
    end;
end;

procedure TKurentoApp.HaveClient(aError: TJSError; aClient: TKurentoClient);

begin
  Writeln('TKurentoApp.HaveClient callback. Have error:',Assigned(aError));
  if assigned(aError) then
    DoError(aError)
  else
    begin
    FClient:=aClient;
    FClient.create('MediaPipeline',@HavePipeLine);
    end;
end;

procedure TKurentoApp.OnOfferReceived(aError: TJSError; aOffer: TKurentoOffer);
begin
  Writeln('TKurentoApp.OnOfferReceived callback. Have error:',Assigned(aError));
  if assigned(aError) then
    DoError(aError)
  else
    begin
    FMyOffer:=aOffer;
    KurentoClient(FURL.Value,@HaveClient);
    end;
end;

procedure TKurentoApp.setIceCandidateCallbacks(webRtcPeer : TWebRtcPeer; webRtcEp : TKurentoWebRtcEndPoint; onerror : TErrorCallBack);

Type
   TIceCandidateCreateFunction = function(arg : string) : TKurentoIceCandidate;

begin
  webRtcPeer.on_('icecandidate', Procedure(candidate : JSValue)
    var
       Cand :TKurentoIceCandidate;
    begin
    console.log('Local candidate:',String(candidate));
    cand:=TIceCandidateCreateFunction(TKurentoClientGlobal.getComplexType('IceCandidate'))(candidate);
    webRtcEp.addIceCandidate(cand, onerror)
    end
  );

  webRtcEp.on_('OnIceCandidate', Procedure(event : JSValue)
    var
      Cand :TKurentoIceCandidate;
    begin
    cand:=TKurentoIceCandidate(TJSObject(event)['candidate']);
    console.log('Remote candidate:',TJSJSON.Stringify(cand));
    webRtcPeer.addIceCandidate(cand, onerror);
    end
  );
end;

Procedure TKurentoApp.HideSpinner(Els : Array of TJSHTMLElement);

Var
  El : TJSHTMLElement;

begin
  For El in Els do
    begin
    El['src']:='';
    El['poster']:='img/webrtc.png';
    El.style.setProperty('background','');
    end;
end;

Procedure TKurentoApp.ShowSpinner(Els : Array of TJSHTMLElement);

Var
  El : TJSHTMLElement;

begin
  For El in Els do
    begin
    El['poster']:='img/transparent-1px.png';
    El.style.setProperty('background','center transparent url("img/spinner.gif") no-repeat');
    end;
end;

Procedure TKurentoApp.DoError(aError : TJSError);

begin
  if Assigned(aError) then
    writeln('An error occurred: ',aError.Message);
end;

function TKurentoApp.DoStop(aEvent: TJSMouseEvent): boolean;

begin
  if Assigned(FWebRTCPeer) then
    begin
    FWebRTCPeer.dispose;
    FWebRTCPeer:=Nil;
    end;
  if Assigned(FPipeLine) then
    begin
    FPipeLine.release();
    FPipeLine:=Nil;
    end;  
  hideSpinner([FvideoInput, FvideoOutput]);
end;

function TKurentoApp.DoStart(aEvent: TJSMouseEvent): boolean;

Var
  Opts : TWebRtcPeerOptions;

begin
  Writeln('DoStart');
  Opts:=TWebRtcPeerOptions.New;
  Opts.localVideo:=FVideoInput;
  Opts.remoteVideo:=FVideoOutput;
  FWebRTCPeer:=TWebRtcPeer.WebRtcPeerSendRecv(Opts,procedure(aError : TJSError)
    begin
    Writeln('TWebRtcPeer.WebRtcPeerSendRecv callback. Have error:',Assigned(aError));
    if Assigned(aError) then
      DoError(aError)
    else
      FWebRTCPeer.generateOffer(@OnOfferReceived);
    end
  );
end;

procedure TKurentoApp.DoRun;
begin
  Terminate; // Avoid a loop.
  FVideoInput:=getHTMLElement('videoInput');
  FVideoOutput:=getHTMLElement('videoOutput');
  FStartButton:=getHTMLElement('start');
  FStopButton:=getHTMLElement('stop');
  FURL:=TJSHTMLInputElement(getHTMLElement('edtServer'));
  FStartButton.OnCLick:=@DoStart;
  FStopButton.OnClick:=@DoStop;
end;


begin
  With TKurentoApp.Create(Nil) do
    begin
    Initialize;
    Run;
    end;
end.
