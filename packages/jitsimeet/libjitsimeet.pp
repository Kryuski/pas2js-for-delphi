{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2020 by the Pas2JS development team.

    This unit defines Jitsi external meet API
    (not to be confused with internal APIs)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$modeswitch externalclass}

unit libjitsimeet;

interface

uses
  JS, web, types;

Const
  // Event names
  EventAudioAvailabilityChanged = 'audioAvailabilityChanged';
  EventAudioMuteStatusChanged = 'audioMuteStatusChanged';
  EventAvatarChanged = 'avatarChanged';
  EventCameraError = 'cameraError';
  EventDeviceListChanged = 'deviceListChanged';
  EventDisplayNameChange = 'displayNameChange';
  EventdominantSpeakerChanged = 'dominantSpeakerChanged';
  EventEmailChange = 'emailChange';
  EventEndpointTextMessageReceived = 'endpointTextMessageReceived';
  EventFeedbackSubmitted = 'feedbackSubmitted';
  EventFilmstripDisplayChanged = 'filmstripDisplayChanged';
  EventIncomingMessage = 'incomingMessage';
  EventMicError = 'micError';
  EventOutgoingMessage = 'outgoingMessage';
  EventParticipantJoined = 'participantJoined';
  EventParticipantKickedOut = 'participantKickedOut';
  EventParticipantLeft = 'participantLeft';
  EventPasswordRequired = 'passwordRequired';
  EventReadyToClose = 'readyToClose';
  EventScreenSharingStatusChanged = 'screenSharingStatusChanged';
  EventSubjectChange = 'subjectChange';
  EventSuspendDetected = 'suspendDetected';
  EventTileViewChanged = 'tileViewChanged';
  EventVideoAvailabilityChanged = 'videoAvailabilityChanged';
  EventVideoConferenceJoined = 'videoConferenceJoined';
  EventVideoConferenceLeft = 'videoConferenceLeft';
  EventVideoMuteStatusChanged = 'videoMuteStatusChanged';

  // Command names. You can use the class helper instead.
  CommandAvatarURL = 'avatarURL';
  CommandDisplayName = 'displayName';
  CommandEmail = 'email';
  CommandHangup = 'hangup';
  CommandPassword = 'password';
  CommandSendEndPointTextMessage = 'sendEndpointTextMessage';
  CommandSendTones = 'sendTones';
  CommandSubject = 'subject';
  CommandToggleAudio = 'toggleAudio';
  CommandToggleChat = 'toggleChat';
  CommandToggleFilmStrip = 'toggleFilmStrip';
  CommandToggleTileView = 'toggleTileView';
  CommandToggleVideo = 'toggleVideo';
  CommandToggleShareScreen = 'toggleShareScreen';

  // UI elements
  UIMicrophone = 'microphone';
  UICamera = 'camera';
  UIClosedCaptions = 'closedcaptions';
  UIDesktop = 'desktop';
  UIFullScreen = 'fullscreen';
  UIFODeviceSelection = 'fodeviceselection';
  UIHangup = 'hangup';
  UIChat = 'chat';
  UIRecording = 'recording';
  UILiveStreaming = 'livestreaming';
  UIEtherPad = 'etherpad';
  UISharedVideo = 'sharedvideo';
  UISettings = 'settings';
  UIVideoQuality = 'videoquality';
  UIFilmStrip = 'filmstrip';
  UIFeedBack = 'feedback';
  UIStats = 'stats';
  UIShortCuts = 'shortcuts';
  UITileView = 'tileview';
  UIVideoBackgroundBlur = 'videobackgroundblur';
  UIDownload = 'download';
  UIHelp = 'help';
  UIMuteEveryone = 'mute-everyone';
  UIProfile = 'profile';
  UIInfo = 'info';
  UIRaiseHand = 'raisehand';
  UIInvite = 'invite';

  UISettingDevices = 'devices';
  UISettingLanguage = 'language';
  UISettingModerator = 'moderator';
  UISettingProfile = 'profile';
  UISettingCalendar = 'calendar';

Type
  // Not documented or possible to define statically  :(

  TJMCommands  = TJSObject;
  TJMInvite = TJSObject;
  TJMInviteDynArray = Array of TJMInvite;

  TJMEventHandler = reference to Procedure(Arg : JSValue);

  TJMInvitee = class external name 'Object' (TJSObject);
  TJMInviteeDynArray = array of TJMInvitee;

  TJMUIElement = (uieMicrophone, uieCamera, uieClosedCaptions, uieDesktop,
    uieFullScreen, uieFODeviceSelection, uieHangup, uieChat, uieRecording,
    uieLiveStreaming, uieEtherPad, uieSharedVideo, uieSettings, uieVideoQuality,
    uieFilmStrip, uieFeedBack, uieStats, uieShortCuts, uieTileView,
    uieVideoBackgroundBlur, uieDownload, uieHelp, uieMuteEveryone,
    uieProfile, uieInfo, uieRaiseHand, uieInvite);
  TJMUIElements = set of TJMUIElement;
  TJMUISetting = (usDevices, usLanguage, usModerator, usProfile, usCalendar);
  TJMUISettings = set of TJMUISetting;

  { TJMDevices }

  TJMDevices = class external name 'Object' (TJSObject)
  private
    FAudioInput: String; external name 'audioInput';
    FAudioOutput: String; external name 'audioOutput';
    FVideoInput: String; external name 'videoInput';
  Public
    Property AudioInput : String Read FAudioInput Write FAudioInput;
    Property audioOutput : String read FAudioOutput Write FAudioOutput;
    Property videoInput : String read FVideoInput Write FVideoInput;
  end;

  { TJMUserInfo }

  TJMUserInfo = class external name 'Object' (TJSObject)
  private
    FEmail: String; external name 'email';
  Public
    Property Email : String Read FEmail Write FEmail;
  end;

  { TJMInterfaceConfig }

  TJMInterfaceConfig = class external name 'Object' (TJSObject)
  private
    FAndroidAppPackage : string; external name 'ANDROID_APP_PACKAGE';
    FAppName: String; external name 'APP_NAME';
    FAppScheme : string; external name 'APP_SCHEME';
    FAudioLevelPrimaryColor : string; external name 'AUDIO_LEVEL_PRIMARY_COLOR';
    FAudioLevelSecondaryColor : string; external name 'AUDIO_LEVEL_SECONDARY_COLOR';
    FAuthenticationEnable: Boolean; external name 'AUTHENTICATION_ENABLE';
    FAutoPinLatestScreenShare : String; external name 'AUTO_PIN_LATEST_SCREEN_SHARE';
    FBrandWaterMarkLink: String; external name 'BRAND_WATERMARK_LINK';
    FClosePageGuestHint: String; external name 'CLOSE_PAGE_GUEST_HINT';
    FConnectionIndicatorAutoHideEnabled : boolean; external name 'CONNECTION_INDICATOR_AUTO_HIDE_ENABLED';
    FConnectionIndicatorAutoHideTimeout : nativeint ; external name 'CONNECTION_INDICATOR_AUTO_HIDE_TIMEOUT';
    FConnectionIndicatorDisabled : boolean; external name 'CONNECTION_INDICATOR_DISABLED';
    FDefaultBackground: String;  external name 'DEFAULT_BACKGROUND';
    FDefaultLocalDisplayName: String; external name 'DEFAULT_LOCAL_DISPLAY_NAME';
    FDefaultRemoteDisplayName: String;  external name 'DEFAULT_REMOTE_DISPLAY_NAME';
    FDisableDominantSpeakerIndicator: Boolean; external name 'DISABLE_DOMINANT_SPEAKER_INDICATOR';
    FDisableFocusIndicator: Boolean;  external name 'DISABLE_FOCUS_INDICATOR';
    FDisableJoinLeaveNotifications :boolean ; external name 'DISABLE_JOIN_LEAVE_NOTIFICATIONS';
    FDisablePresenceStatus : boolean; external name 'DISABLE_PRESENCE_STATUS';
    FDisableRinging : Boolean; external name 'DISABLE_RINGING';
    FDisableTranscriptionSubtitles: Boolean;  external name 'DISABLE_TRANSCRIPTION_SUBTITLES';
    FDisableVideoBackground: Boolean; external name 'DISABLE_VIDEO_BACKGROUND';
    FDisplayWelcomePageContent: Boolean;  external name 'DISPLAY_WELCOME_PAGE_CONTENT';
    FDisplayWelcomePageToolbarAdditionalContent: Boolean; external name 'DISPLAY_WELCOME_PAGE_TOOLBAR_ADDITIONAL_CONTENT';
    FEnableFeedBackAnimation: Boolean;   external name 'ENABLE_FEEDBACK_ANIMATION';
    FEnforceNotificationAutoDismissTimeout : NativeInt; external name 'ENFORCE_NOTIFICATION_AUTO_DISMISS_TIMEOUT';
    FFilmStripMaxHeight: NativeInt;  external name 'FILM_STRIP_MAX_HEIGHT';
    FFilmStripOnly: Boolean; external name 'filmStripOnly';
    FGenerateRoomNamesOnWelcomePage: Boolean;  external name 'GENERATE_ROOMNAMES_ON_WELCOME_PAGE';
    FInitialToolbarTimeOut: NativeInt;  external name 'INITIAL_TOOLBAR_TIMEOUT';
    FInvitationPoweredBy: Boolean;  external name 'INVITATION_POWERED_BY';
    FJitsiWaterMarkLink: String; external name 'JITSI_WATERMARK_LINK';
    FLangDetection: Boolean;  external name 'LANG_DETECTION';
    FLiveStreamingHelpLink : string; external name 'LIVE_STREAMING_HELP_LINK';
    FLocalThumbnailRatio : double ; external name 'LOCAL_THUMBNAIL_RATIO';
    FMaximumZoomingCoefficient : double ; external name 'MAXIMUM_ZOOMING_COEFFICIENT';
    FMobileAppPromo :Boolean ; external name 'MOBILE_APP_PROMO';
    FMobileDownloadLinkAndroid : string ; external name 'MOBILE_DOWNLOAD_LINK_ANDROID';
    FMobileDownloadLinkIos : string; external name 'MOBILE_DOWNLOAD_LINK_IOS';
    FNativeAppName: STring;  external name 'NATIVE_APP_NAME';
    FOptimalBrowsers : TStringDynArray ; external name 'OPTIMAL_BROWSERS';
    FPolicyLogo : string; external name 'POLICY_LOGO';
    FProviderName: string;   external name 'PROVIDER_NAME';
    FRandomAvatarURLPrefix: String; external name 'RANDOM_AVATAR_URL_PREFIX';
    FRandomAvatarURLSuffix: String;   external name 'RANDOM_AVATAR_URL_SUFFIX';
    FRecentListEnabled :boolean ; external name 'RECENT_LIST_ENABLED';
    FRemoteThumbnailRatio : double ; external name 'REMOTE_THUMBNAIL_RATIO';
    FSettingSections: TStringDynArray;   external name 'SETTINGS_SECTIONS';
    FShowBrandWaterMark: Boolean; external name 'SHOW_BRAND_WATERMARK';
    FShowChromeExtensionBanner : Boolean; external name 'SHOW_CHROME_EXTENSION_BANNER';
    FShowDeepLinkingImage: Boolean; external name 'SHOW_DEEP_LINKING_IMAGE';
    FShowJitsiWaterMark: Boolean;  external name 'SHOW_JITSI_WATERMARK';
    FShowPoweredBy: Boolean;  external name 'SHOW_POWERED_BY';
    FShowPromotionalClosePage: Boolean; external name 'SHOW_PROMOTIONAL_CLOSE_PAGE';
    FShowWaterMarkForGuests: Boolean; external name 'SHOW_WATERMARK_FOR_GUESTS';
    FSupportUrl : string ; external name 'SUPPORT_URL';
    FTileViewMaxColumns : nativeint; external name 'TILE_VIEW_MAX_COLUMNS';
    FToolbarAlwaysVisible: Boolean;  external name 'TOOLBAR_ALWAYS_VISIBLE';
    FToolbarButtons: TStringDynArray;  external name 'TOOLBAR_BUTTONS';
    FToolbarTimeOut: NativeInt;  external name 'TOOLBAR_TIMEOUT';
    FUnsupportedBrowsers : TStringDynArray; external name 'UNSUPPORTED_BROWSERS';
    FVerticalFilmStrip: Boolean; external name 'VERTICAL_FILMSTRIP';
    FVideoLayoutFit: String; external name 'VIDEO_LAYOUT_FIT';
    FVideoQualityLabelDisabled : boolean; external name 'VIDEO_QUALITY_LABEL_DISABLED';
  Public
    Property AndroidAppPackage : String Read FAndroidAppPackage write FAndroidAppPackage; //       ANDROID_APP_PACKAGE: 'org.jitsi.meet',
    Property AppName : String Read FAppName Write FAppName; //  APP_NAME: 'Jitsi Meet',
    Property AppScheme : String Read FAppScheme write FAppScheme; //       APP_SCHEME: 'org.jitsi.meet',
    Property AudioLevelPrimaryColor : string  Read FAudioLevelPrimaryColor write FAudioLevelPrimaryColor; //       AUDIO_LEVEL_PRIMARY_COLOR: 'rgba(255,255,255,0.4)',
    Property AudioLevelSecondaryColor : string Read FAudioLevelSecondaryColor write FAudioLevelSecondaryColor; //       AUDIO_LEVEL_SECONDARY_COLOR: 'rgba(255,255,255,0.2)',
    Property AuthenticationEnable : Boolean Read FAuthenticationEnable Write FAuthenticationEnable; // AUTHENTICATION_ENABLE: true,
    Property AutoPinLatestScreenShare : string Read FAutoPinLatestScreenShare write FAutoPinLatestScreenShare; //       AUTO_PIN_LATEST_SCREEN_SHARE: 'remote-only',
    Property BrandWaterMarkLink : String Read FBrandWaterMarkLink Write FBrandWaterMarkLink; // BRAND_WATERMARK_LINK: '',
    Property ClosePageGuestHint : String Read FClosePageGuestHint Write FClosePageGuestHint; // CLOSE_PAGE_GUEST_HINT
    Property ConnectionIndicatorAutoHideEnabled : Boolean Read FConnectionIndicatorAutoHideEnabled write FConnectionIndicatorAutoHideEnabled; //       CONNECTION_INDICATOR_AUTO_HIDE_ENABLED: true,
    Property ConnectionIndicatorAutoHideTimeout : NativeInt Read FConnectionIndicatorAutoHideTimeout write FConnectionIndicatorAutoHideTimeout; //       CONNECTION_INDICATOR_AUTO_HIDE_TIMEOUT: 5000,
    Property ConnectionIndicatorDisabled : Boolean Read FConnectionIndicatorDisabled write FConnectionIndicatorDisabled; //       CONNECTION_INDICATOR_DISABLED: false,
    Property DefaultBackground : String Read FDefaultBackground Write FDefaultBackground; // DEFAULT_BACKGROUND: String;
    Property DefaultLocalDisplayName : String Read FDefaultLocalDisplayName Write FDefaultLocalDisplayName; //  DEFAULT_LOCAL_DISPLAY_NAME: 'me',
    Property DefaultRemoteDisplayName : String Read FDefaultRemoteDisplayName Write FDefaultRemoteDisplayName; //   DEFAULT_REMOTE_DISPLAY_NAME: 'Fellow Jitster',
    Property DisableDominantSpeakerIndicator : Boolean Read FDisableDominantSpeakerIndicator Write FDisableDominantSpeakerIndicator; // DISABLE_DOMINANT_SPEAKER_INDICATOR: false,
    Property DisableFocusIndicator: Boolean Read FDisableFocusIndicator Write FDisableFocusIndicator; //  DISABLE_FOCUS_INDICATOR: true,
    Property DisableJoinLeaveNotifications : boolean Read FDisableJoinLeaveNotifications write FDisableJoinLeaveNotifications; //       DISABLE_JOIN_LEAVE_NOTIFICATIONS: true
    Property DisablePresenceStatus : boolean Read FDisablePresenceStatus write FDisablePresenceStatus; //       DISABLE_PRESENCE_STATUS: true,
    Property DisableRinging : Boolean Read FDisableRinging write FDisableRinging; //       DISABLE_RINGING: false,
    Property DisableTranscriptionSubtitles : Boolean Read  FDisableTranscriptionSubtitles Write FDisableTranscriptionSubtitles; // DISABLE_TRANSCRIPTION_SUBTITLES: false,
    Property DisableVideoBackground : Boolean Read FDisableVideoBackground Write FDisableVideoBackground; //   DISABLE_VIDEO_BACKGROUND: false,
    Property DisplayWelcomePageContent : Boolean Read FDisplayWelcomePageContent Write FDisplayWelcomePageContent; //    DISPLAY_WELCOME_PAGE_CONTENT: true,
    Property DisplayWelcomePageToolbarAdditionalContent : Boolean Read FDisplayWelcomePageToolbarAdditionalContent Write FDisplayWelcomePageToolbarAdditionalContent; //  DISPLAY_WELCOME_PAGE_TOOLBAR_ADDITIONAL_CONTENT: false,
    Property EnableFeedBackAnimation : Boolean Read FEnableFeedBackAnimation Write FEnableFeedBackAnimation; //  ENABLE_FEEDBACK_ANIMATION: false,
    Property EnforceNotificationAutoDismissTimeout : Nativeint  Read FEnforceNotificationAutoDismissTimeout write FEnforceNotificationAutoDismissTimeout; //       ENFORCE_NOTIFICATION_AUTO_DISMISS_TIMEOUT: 15000,
    Property FilmStripMaxHeight : NativeInt Read FFilmStripMaxHeight Write FFilmStripMaxHeight; //  FILM_STRIP_MAX_HEIGHT: 120,
    Property FilmStripOnly : Boolean Read FFilmStripOnly Write FFilmStripOnly;  // filmStripOnly: false,
    Property GenerateRoomNamesOnWelcomePage : Boolean Read FGenerateRoomNamesOnWelcomePage Write FGenerateRoomNamesOnWelcomePage; //  GENERATE_ROOMNAMES_ON_WELCOME_PAGE: true,
    Property InitialToolbarTimeOut : NativeInt Read FInitialToolbarTimeOut Write FInitialToolbarTimeOut; //       INITIAL_TOOLBAR_TIMEOUT: 20000,
    Property InvitationPoweredBy : Boolean Read FInvitationPoweredBy Write FInvitationPoweredBy; //  INVITATION_POWERED_BY: true,
    Property JitsiWaterMarkLink : String Read FJitsiWaterMarkLink Write FJitsiWaterMarkLink; // JITSI_WATERMARK_LINK: 'https://jitsi.org',
    Property LangDetection : Boolean Read FLangDetection Write FLangDetection; //   LANG_DETECTION: false, // Allow i18n to detect the system language
    Property LiveStreamingHelpLink : string Read FLiveStreamingHelpLink write FLiveStreamingHelpLink; //       LIVE_STREAMING_HELP_LINK: 'https://jitsi.org/live',
    Property LocalThumbnailRatio : Double Read FLocalThumbnailRatio write FLocalThumbnailRatio; //       LOCAL_THUMBNAIL_RATIO: 16 / 9, // 16:9
    Property MaximumZoomingCoefficient : Double Read FMaximumZoomingCoefficient write FMaximumZoomingCoefficient; //       MAXIMUM_ZOOMING_COEFFICIENT: 1.3,
    Property MobileAppPromo : Boolean Read FMobileAppPromo write FMobileAppPromo; //       MOBILE_APP_PROMO: true,
    Property MobileDownloadLinkAndroid : String Read FMobileDownloadLinkAndroid write FMobileDownloadLinkAndroid; //       MOBILE_DOWNLOAD_LINK_ANDROID: 'https://play.google.com/store/apps/details?id=org.jitsi.meet',
    Property MobileDownloadLinkIos : String Read FMobileDownloadLinkIos write FMobileDownloadLinkIos; //       MOBILE_DOWNLOAD_LINK_IOS: 'https://itunes.apple.com/us/app/jitsi-meet/id1165103905',
    Property NativeAppName : STring Read FNativeAppName Write FNativeAppName; //   NATIVE_APP_NAME: 'Jitsi Meet',
    Property OptimalBrowsers : TStringDynArray Read FOptimalBrowsers write FOptimalBrowsers; //       OPTIMAL_BROWSERS: [ 'chrome', 'chromium', 'nwjs', 'electron' ],
    Property PolicyLogo : String  Read FPolicyLogo write FPolicyLogo; //       POLICY_LOGO: null,
    Property ProviderName : string Read FProviderName Write FProviderName; //  PROVIDER_NAME: 'Jitsi',
    Property RandomAvatarURLPrefix : String Read FRandomAvatarURLPrefix Write FRandomAvatarURLPrefix; //  RANDOM_AVATAR_URL_PREFIX
    Property RandomAvatarURLSuffix : String Read FRandomAvatarURLSuffix Write FRandomAvatarURLSuffix; //  RANDOM_AVATAR_URL_SUFFIX
    Property RecentListEnabled : Boolean Read FRecentListEnabled write FRecentListEnabled; //       RECENT_LIST_ENABLED: true,
    Property RemoteThumbnailRatio : Double Read FRemoteThumbnailRatio write FRemoteThumbnailRatio; //       REMOTE_THUMBNAIL_RATIO: 1, // 1:1
    Property SettingSections : TStringDynArray Read FSettingSections Write FSettingSections; // SETTINGS_SECTIONS:
    Property ShowBrandWaterMark : Boolean Read FShowBrandWaterMark Write FShowBrandWaterMark; //  SHOW_BRAND_WATERMARK: false,
    Property ShowChromeExtensionBanner : boolean Read FShowChromeExtensionBanner write FShowChromeExtensionBanner; //       SHOW_CHROME_EXTENSION_BANNER: true,
    Property ShowDeepLinkingImage : Boolean Read FShowDeepLinkingImage Write FShowDeepLinkingImage; //  SHOW_DEEP_LINKING_IMAGE: false,
    Property ShowJitsiWaterMark : Boolean Read FShowJitsiWaterMark Write FShowJitsiWaterMark; //  SHOW_JITSI_WATERMARK: true,
    Property ShowPoweredBy : Boolean Read FShowPoweredBy Write FShowPoweredBy; //   SHOW_POWERED_BY: false,
    Property ShowPromotionalClosePage : Boolean Read FShowPromotionalClosePage Write FShowPromotionalClosePage; // SHOW_PROMOTIONAL_CLOSE_PAGE
    Property ShowWaterMarkForGuests : Boolean Read FShowWaterMarkForGuests Write FShowWaterMarkForGuests; //   SHOW_WATERMARK_FOR_GUESTS: true,
    Property SupportUrl : String Read FSupportUrl write FSupportUrl; //       SUPPORT_URL: 'https://github.com/jitsi/jitsi-meet/issues/new',
    Property TileViewMaxColumns : NativeInt  Read FTileViewMaxColumns write FTileViewMaxColumns; //       TILE_VIEW_MAX_COLUMNS: 5,
    Property ToolbarAlwaysVisible: Boolean Read FToolbarAlwaysVisible Write FToolbarAlwaysVisible; //  TOOLBAR_ALWAYS_VISIBLE: false,
    Property ToolbarButtons : TStringDynArray Read FToolbarButtons Write FToolbarButtons; //  TOOLBAR_BUTTONS:
    Property ToolbarTimeOut : NativeInt Read FToolbarTimeOut Write FToolbarTimeOut; //  TOOLBAR_TIMEOUT: 4000,
    Property UnsupportedBrowsers : TStringDynArray Read FUnsupportedBrowsers write FUnsupportedBrowsers; //       UNSUPPORTED_BROWSERS: [],
    Property VerticalFilmStrip : Boolean Read FVerticalFilmStrip Write FVerticalFilmStrip; // VERTICAL_FILMSTRIP: true,
    Property VideoLayoutFit : String Read FVideoLayoutFit Write FVideoLayoutFit; // VIDEO_LAYOUT_FIT: 'both',
    Property VideoQualityLabelDisabled : Boolean Read FVideoQualityLabelDisabled write FVideoQualityLabelDisabled; //       VIDEO_QUALITY_LABEL_DISABLED: false,
  end;

  TJMMeetOptions = class external name 'Object' (TJSObject)
  Public
    roomName : String;
    width : Integer;
    widthString : String; external name 'width';
    height : Integer;
    heightString : String; external name 'height';
    parentNode : TJSHTMLElement;
    configOverwrite: TJSObject;
    interfaceConfigOverwrite: TJMInterfaceConfig;
    noSSL : Boolean;
    jwt : String;
    onload : TJSRawEventHandler;
    invitees : TJMInviteeDynArray;
    devices : TJMDevices;
    userInfo : TJMUserInfo;
  end;

  { TJMMediaDevice }

  TJMMediaDevice = class external name 'Object' (TJSObject)
  private
    FDeviceID: String; external name 'deviceId';
    FGroupID: String; external name 'groupId';
    FKind: String; external name 'lind';
    FLabel: String; external name 'label';
  public
    Property DeviceId : String Read FDeviceID;
    Property GroupId : String Read FGroupID;
    Property Kind : String Read FKind;
    Property Label_ : String read FLabel;
  end;
  TJMMediaDeviceDynArray = Array of TJMMediaDevice;

  { TJMAvailableDevicesResponse }

  TJMAvailableDevicesResponse = class external name 'Object' (TJSObject)
  private
    FAudioInput: TJMMediaDeviceDynArray; external name 'audioInput';
    FAudioOutput: TJMMediaDeviceDynArray; external name 'audioOutput';
    FVideoInput: TJMMediaDeviceDynArray; external name 'videoInput';
  Public
    property AudioInput : TJMMediaDeviceDynArray read FAudioInput;
    property AudioOutput : TJMMediaDeviceDynArray read FAudioOutput;
    property VideoInput : TJMMediaDeviceDynArray Read FVideoInput;
  end;


  { TJMCurrentDevicesResponse }

  TJMCurrentDevicesResponse = class external name 'Object' (TJSObject)
  private
    fAudioInput: TJMMediaDeviceDynArray; external name 'audioInput';
    fAudioOutput: TJMMediaDeviceDynArray; external name 'audioOutput';
    fVideoInput: TJMMediaDeviceDynArray; external name 'videoInput';
  Public
    property AudioInput : TJMMediaDeviceDynArray read fAudioInput;
    property AudioOutput : TJMMediaDeviceDynArray read fAudioOutput;
    property VideoInput : TJMMediaDeviceDynArray read fVideoInput;
  end;

  TJMEventEmitter = class external name 'Object' (TJSObject)
  Public
    Procedure addListener(aName : String; aCallBack : TJMEventHandler);
    Procedure On_(aName : String; aCallBack : TJMEventHandler); external name 'on';
    Procedure Off(aName : String; aCallBack : TJMEventHandler); external name 'off';
    Procedure removeAllListeners(aName : String);
    Procedure removeListener(aName : String; aCallBack : TJMEventHandler);
  end;

  TJMExternalAPI = class external name 'JitsiMeetExternalAPI' (TJMEventEmitter)
  Public
    Constructor new (aDomain : String; aOptions : TJMMeetOptions);
    Procedure dispose;
    Procedure executeCommand(aCommand : String); overload;
    Procedure executeCommand(aCommand : String; Arg : String); overload;
    Procedure executeCommand(aCommand : String; Arg1,Arg2 : String); overload;
    Procedure executeCommand(aCommand : String; Options : TJSObject); overload;
    Procedure executeCommands(commands : TJMCommands);
    Function getAvailableDevices : TJSPromise;
    Function getAvatarURL : String;
    Function getCurrentDevices : TJSPromise;
    Function getDisplayName : String;
    Function getEmail : String;
    Function getIFrame : TJSHTMLElement;
    Function getNumberOfParticipants : NativeInt;
    Function invite(aInvitees : TJMInviteDynArray): TJSPromise;
    Function isAudioAvailable : TJSPromise;
    Function isAudioMuted : TJSPromise;
    Function isDeviceChangeAvailable(const aDeviceType : String) : TJSPromise;
    Function isDeviceListAvailable : TJSPromise;
    Function isMultipleAudioInputSupported : TJSPromise;
    Function isVideoAvailable : TJSPromise;
    Function isVideoMuted : TJSPromise;
    Procedure setAudioInputDevice(const aDeviceLabel,aDeviceId : String);
    Procedure setAudioOutputDevice(const aDeviceLabel,aDeviceId : String);
    Procedure setVideoInputDevice(const aDeviceLabel,aDeviceId : String);
  end;

  { TJSMSendTones }

  TJMSendTones = class external name 'Object' (TJSObject)
  private
    FDuration: NativeInt; external name 'duration';
    FPause: NativeInt; external name 'pause';
    FTones: String; external name 'tones';
  Public
    Property Tones : String Read FTones Write FTones;
    Property Duration : NativeInt Read FDuration Write FDuration;
    Property Pause : NativeInt Read FPause Write FPause;
  end;

  { TJMExternalAPIHelper }

  TJMExternalAPIHelper = class helper for TJMExternalAPI
  Public
    procedure SetDisplayName (Const aName : string);
    procedure SetPassword (Const aPassword : string);
    procedure SendTones (aTones :TJMSendTones);
    procedure SetSubject(Const aSubject : string);
    procedure ToggleAudio;
    procedure ToggleVideo;
    procedure ToggleFilmStrip;
    procedure ToggleChat;
    procedure ToggleShareScreen;
    procedure ToggleTileView;
    procedure Hangup;
    procedure Email(Const aEmail : string);
    procedure AvatarURL(Const aURL : string);
    procedure SendEndPointTextMessage(Const aParticipantID,aText : string);
  end;

function UIElementsStrings(aElements : TJMUIElements) : TStringDynArray;
function UISettingsStrings(aUISettings : TJMUISettings) : TStringDynArray;

Const
  UIElementNames : Array[TJMUIElement] of string = (
    UIMicrophone, UICamera, UIClosedCaptions, UIDesktop,
    UIFullScreen, UIFODeviceSelection, UIHangup, UIChat, UIRecording,
    UILiveStreaming, UIEtherPad, UISharedVideo, UISettings, UIVideoQuality,
    UIFilmStrip, UIFeedBack, UIStats, UIShortCuts, UITileView,
    UIVideoBackgroundBlur, UIDownload, UIHelp, UIMuteEveryone,
    UIProfile, UIInfo, UIRaiseHand, UIInvite
  );
  UISettingNames : Array[TJMUISetting] of string =
    (UISettingDevices,  UISettingLanguage,  UISettingModerator,  UISettingProfile,  UISettingCalendar);



implementation

{ TJMExternalAPIHelper }

procedure TJMExternalAPIHelper.SetDisplayName(const aName: string);
begin
  ExecuteCommand(CommandDisplayName,aName);
end;

procedure TJMExternalAPIHelper.SetPassword(const aPassword: string);
begin
  ExecuteCommand(CommandPassword,aPassword);
end;

procedure TJMExternalAPIHelper.SendTones(aTones: TJMSendTones);
begin
  executeCommand(CommandSendTones,aTones);
end;

procedure TJMExternalAPIHelper.SetSubject(const aSubject: string);
begin
  ExecuteCommand(CommandSubject,aSubject);
end;

procedure TJMExternalAPIHelper.ToggleAudio;
begin
  ExecuteCommand(CommandToggleAudio);
end;

procedure TJMExternalAPIHelper.ToggleVideo;
begin
  ExecuteCommand(CommandToggleVideo);
end;

procedure TJMExternalAPIHelper.ToggleFilmStrip;
begin
  ExecuteCommand(CommandToggleFilmStrip);
end;

procedure TJMExternalAPIHelper.ToggleChat;
begin
  ExecuteCommand(CommandToggleChat);
end;

procedure TJMExternalAPIHelper.ToggleShareScreen;
begin
  ExecuteCommand(CommandToggleShareScreen);
end;

procedure TJMExternalAPIHelper.ToggleTileView;
begin
  ExecuteCommand(CommandToggleTileView);
end;

procedure TJMExternalAPIHelper.Hangup;
begin
  ExecuteCommand(CommandHangup);
end;

procedure TJMExternalAPIHelper.Email(const aEmail: string);
begin
  ExecuteCommand(commandEmail,aEmail);
end;

procedure TJMExternalAPIHelper.AvatarURL(const aURL: string);
begin
  ExecuteCommand(CommandAvatarURL,aURL);
end;

procedure TJMExternalAPIHelper.SendEndPointTextMessage(const aParticipantID, aText: string);
begin
  ExecuteCommand(CommandSendEndPointTextMessage,aParticipantID,atext);
end;


function UISettingsStrings(aUISettings: TJMUISettings): TStringDynArray;

Var
  Len: Integer;
  S : TJMUISetting;

begin
  Len:=0;
  SetLength(Result,Ord(High(TJMUISetting)));
  For S in TJMUISetting do
    if S in aUISettings then
      begin
      Result[Len]:=UISettingNames[S];
      Inc(Len);
      end;
  SetLength(Result,len);

end;

function UIElementsStrings(aElements: TJMUIElements): TStringDynArray;

Var
  Len: Integer;
  J : TJMUIElement;

begin
  Len:=0;
  SetLength(Result,Ord(High(TJMUIElement)));
  For J in TJMUIElement do
    if J in aElements then
      begin
      Result[Len]:=UIElementNames[J];
      Inc(Len);
      end;
  SetLength(Result,len);
end;

end.

