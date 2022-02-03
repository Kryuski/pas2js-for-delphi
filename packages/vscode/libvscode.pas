unit libvscode;

{$mode objfpc}
{$modeswitch externalclass}
{$scopedenums on}

interface

uses Types, JS, Web;

Type
  TUint32DynArray = array of Cardinal;
  TVSThenable = TJSPromise;

  TVSEndOfLine = (EndOfLine_unknown,LF,CRLF);
  TVSViewColumn = (ViewColumn_unknown,Active,Beside,One,Two,Three,Four,Five,Six,Seven,Eight,Nine);
  TVSDocumentHighlightKind = (Text,Read,Write);
  TVSSymbolKind = (File_,Module,Namespace,Package,Class_,Method,Property_,Field,Constructor_,Enum,Interface_,Function_,Variable,Constant,String_,NativeInt,Boolean,Array_,Object_,Key,Null,EnumMember,Struct,Event,Operator_,TypeParameter);
  TVSSymbolTag = (SymbolTag_unknown,Deprecated);
  // init : 1
  TVSSignatureHelpTriggerKind = (SignatureHelpTriggerKind_unknown,Invoke,TriggerCharacter,ContentChange);
  // init : 0
  TVSCompletionItemKind = (Text,Method,Function_,Constructor_,Field,Variable,Class_,Interface_,Module,Property_,Unit_,Value,Enum,Keyword,Snippet,Color,Reference,&File,Folder,EnumMember,Constant,Struct,Event,&Operator,TypeParameter,User,Issue);
  // init : 1
  TVSTextEditorCursorStyle = (TextEditorCursorStyle_unknown,Line,Block,Underline,LineThin,BlockOutline,UnderlineThin);
  // init : 0
  TVSTextEditorLineNumbersStyle = (Off,On,Relative);
  // init : 0
  TVSTextEditorRevealType = (Default,InCenter,InCenterIfOutsideViewport,AtTop);
  // init : 1
  TVSOverviewRulerLane = (OverviewRulerLane_unknown,Left,Center,Right,Full);
  // init : 0
  TVSDecorationRangeBehavior = (OpenOpen,ClosedClosed,OpenClosed,ClosedOpen);
  // init : 1
  TVSCompletionItemTag = (CompletionItemTag_unknown,Deprecated);
  // init : 0
  TVSCompletionTriggerKind = (Invoke,TriggerCharacter,TriggerForIncompleteCompletions);
  // init : 1
  TVSFoldingRangeKind = (FoldingRangeKind_unknown,Comment,Imports,Region);
  // init : 0
  TVSIndentAction = (None,Indent,IndentOutdent,Outdent);
  // init : 1
  TVSConfigurationTarget = (ConfigurationTarget_unknown,Global,Workspace,WorkspaceFolder);
  // init : 0
  TVSDiagnosticSeverity = (Error,Warning,Information,Hint);
  // init : 1
  TVSDiagnosticTag = (DiagnosticTag_unknown,Unnecessary,Deprecated);
  // init : 1
  TVSStatusBarAlignment = (StatusBarAlignment_unknown,Left,Right);
  // init : 1
  TVSExtensionKind = (ExtensionKind_unknown,UI,Workspace);
  // init : 1
  TVSExtensionMode = (ExtensionMode_unknown,Production,Development,Test);
  // init : 1
  TVSColorThemeKind = (ColorThemeKind_unknown,Light,Dark,HighContrast);
  // init : 1
  TVSTaskRevealKind = (TaskRevealKind_unknown,Always,Silent,Never);
  // init : 1
  TVSTaskPanelKind = (TaskPanelKind_unknown,Shared,Dedicated,New);
  // init : 1
  TVSShellQuoting = (ShellQuoting_unknown,Escape,Strong,Weak);
  // init : 1
  TVSTaskScope = (TaskScope_unknown,Global,Workspace);
  // init : 0
  TVSFileType = (Unknown,File,Directory,SymbolicLink);
  // init : 1
  TVSFileChangeType = (FileChangeType_unknown,Changed,Created,Deleted);
  // init : 1
  TVSUIKind = (UIKind_unknown,Desktop,Web);
  // init : 0
  TVSTreeItemCollapsibleState = (None,Collapsed,Expanded);
  // init : 1
  TVSEnvironmentVariableMutatorType = (EnvironmentVariableMutatorType_unknown,Replace,Append,Prepend);
  // init : 1
  TVSProgressLocation = (ProgressLocation_unknown,SourceControl,Window,Notification);
  // init : 1
  TVSTextDocumentSaveReason = (TextDocumentSaveReason_unknown,Manual,AfterDelay,FocusOut);
  // init : parentSession: TVSDebugSession;
  TVSDebugConsoleMode = (DebugConsoleMode_unknown);
  // init : export let activeDebugSession: TVSDebugSession ;
  TVSDebugConfigurationProviderTriggerKind = (DebugConfigurationProviderTriggerKind_unknown);
  // init : 0
  TVSCommentThreadCollapsibleState = (Collapsed,Expanded);
  // init : 0
  TVSCommentMode = (Editing,Preview);





  TVSCommand  = class;
  TVSTextLine  = class;
  TVSTextDocument  = class;
  TVSPosition  = class;
  TVSRange  = class;
  TVSSelection = class;
  TVSTextEditorSelectionChangeEvent  = class;
  TVSTextEditorVisibleRangesChangeEvent  = class;
  TVSTextEditorOptionsChangeEvent  = class;
  TVSTextEditorViewColumnChangeEvent  = class;
  TVSTextEditorOptions  = class;
  TVSTextEditorDecorationType  = class;
  TVSTextDocumentShowOptions  = class;
  TVSThemeColor  = class;
  TVSThemeIcon  = class;
  TVSThemableDecorationRenderOptions  = class;
  TVSThemableDecorationAttachmentRenderOptions  = class;
  TVSDecorationRenderOptions = class;
  TVSDecorationOptions  = class;
  TVSThemableDecorationInstanceRenderOptions  = class;
  TVSDecorationInstanceRenderOptions = class;
  TVSTextEditor  = class;
  TVSTextEditorEdit  = class;
  TVSUri  = class;
  TVSCancellationToken  = class;
  TVSCancellationTokenSource  = class;
  TVSDisposable  = class;
  TVSFileSystemWatcher = class;
  TVSTextDocumentContentProvider  = class;
  TVSQuickPickItem  = class;
  TVSQuickPickOptions  = class;
  TVSWorkspaceFolderPickOptions  = class;
  TVSOpenDialogOptions  = class;
  TVSSaveDialogOptions  = class;
  TVSMessageItem  = class;
  TVSMessageOptions  = class;
  TVSInputBoxOptions  = class;
  TVSRelativePattern  = class;
  TVSDocumentFilter  = class;
  TVSCodeActionKind  = class;
  TVSCodeActionContext  = class;
  TVSCodeAction  = class;
  TVSCodeActionProvider = class;
  TVSCodeActionProviderMetadata  = class;
  TVSCodeLens  = class;
  TVSCodeLensProvider = class;
  TVSDefinitionProvider  = class;
  TVSImplementationProvider  = class;
  TVSTypeDefinitionProvider  = class;
  TVSDeclarationProvider  = class;
  TVSMarkdownString  = class;
  TVSHover  = class;
  TVSHoverProvider  = class;
  TVSEvaluatableExpression  = class;
  TVSEvaluatableExpressionProvider  = class;
  TVSDocumentHighlight  = class;
  TVSDocumentHighlightProvider  = class;
  TVSSymbolInformation  = class;
  TVSDocumentSymbol  = class;
  TVSDocumentSymbolProvider  = class;
  TVSDocumentSymbolProviderMetadata  = class;
  TVSWorkspaceSymbolProvider = class;
  TVSReferenceContext  = class;
  TVSReferenceProvider  = class;
  TVSTextEdit  = class;
  TVSWorkspaceEditEntryMetadata  = class;
  TVSWorkspaceEdit  = class;
  TVSSnippetString  = class;
  TVSRenameProvider  = class;
  TVSSemanticTokensLegend  = class;
  TVSSemanticTokensBuilder  = class;
  TVSSemanticTokens  = class;
  TVSSemanticTokensEdits  = class;
  TVSSemanticTokensEdit  = class;
  TVSDocumentSemanticTokensProvider  = class;
  TVSDocumentRangeSemanticTokensProvider  = class;
  TVSFormattingOptions  = class;
  TVSDocumentFormattingEditProvider  = class;
  TVSDocumentRangeFormattingEditProvider  = class;
  TVSOnTypeFormattingEditProvider  = class;
  TVSParameterInformation  = class;
  TVSSignatureInformation  = class;
  TVSSignatureHelp  = class;
  TVSSignatureHelpContext  = class;
  TVSSignatureHelpProvider  = class;
  TVSSignatureHelpProviderMetadata  = class;
  TVSCompletionItem  = class;
  TVSCompletionContext  = class;
  TVSDocumentLink  = class;
  TVSColor  = class;
  TVSColorInformation  = class;
  TVSColorPresentation  = class;
  TVSDocumentColorProvider  = class;
  TVSFoldingRange  = class;
  TVSFoldingContext  = class;
  TVSFoldingRangeProvider  = class;
  TVSSelectionRange  = class;
  TVSSelectionRangeProvider  = class;
  TVSCallHierarchyItem  = class;
  TVSCallHierarchyIncomingCall  = class;
  TVSCallHierarchyOutgoingCall  = class;
  TVSCallHierarchyProvider  = class;
  TVSLinkedEditingRanges  = class;
  TVSLinkedEditingRangeProvider  = class;
  TVSCommentRule  = class;
  TVSIndentationRule  = class;
  TVSEnterAction  = class;
  TVSOnEnterRule  = class;
  TVSLanguageConfiguration  = class;
  TVSWorkspaceConfiguration  = class;
  TVSLocation  = class;
  TVSLocationLink  = class;
  TVSDiagnosticChangeEvent  = class;
  TVSDiagnosticRelatedInformation  = class;
  TVSDiagnostic  = class;
  TVSDiagnosticCollection  = class;
  TVSOutputChannel  = class;
  TVSAccessibilityInformation  = class;
  TVSStatusBarItem  = class;
  TVSTerminal  = class;
  TVSTerminalLinkContext  = class;
  TVSTerminalLink  = class;
  TVSFileDecoration  = class;
  TVSFileDecorationProvider  = class;
  TVSExtensionContext  = class;
  TVSMemento  = class;
  TVSColorTheme  = class;
  TVSTaskPresentationOptions  = class;
  TVSTaskGroup  = class;
  TVSTaskDefinition  = class;
  TVSProcessExecutionOptions  = class;
  TVSProcessExecution  = class;
  TVSShellQuotingOptions  = class;
  TVSShellExecutionOptions  = class;
  TVSShellQuotedString  = class;
  TVSShellExecution  = class;
  TVSCustomExecution  = class;
  TVSRunOptions  = class;
  TVSTask  = class;
  TVSTaskExecution  = class;
  TVSTaskProcessStartEvent  = class;
  TVSTaskProcessEndEvent  = class;
  TVSTaskFilter  = class;
  TVSNStasks  = class;
  TVSFileStat  = class;
  TVSFileSystemError = class;
  TVSFileChangeEvent  = class;
  TVSFileSystemProvider  = class;
  TVSFileSystem  = class;
  TVSWebviewPortMapping  = class;
  TVSWebviewOptions  = class;
  TVSWebview  = class;
  TVSWebviewPanelOptions  = class;
  TVSWebviewPanelOnDidChangeViewStateEvent  = class;
  TVSWebviewView  = class;
  TVSWebviewViewProvider  = class;
  TVSCustomTextEditorProvider  = class;
  TVSCustomReadonlyEditorProvider = class;
  TVSCustomEditorProvider = class;
  TVSClipboard  = class;
  TVSNSenv  = class;
  TVSNScommands  = class;
  TVSWindowState  = class;
  TVSUriHandler  = class;
  TVSNSwindow  = class;
  TVSTreeViewOptions  = class;
  TVSTreeViewExpansionEvent  = class;
  TVSTreeViewSelectionChangeEvent  = class;
  TVSTreeViewVisibilityChangeEvent  = class;
  TVSTreeView = class;
  TVSTreeDataProvider  = class;
  TVSTreeItem  = class;
  TVSTreeItemLabel  = class;
  TVSTerminalOptions  = class;
  TVSExtensionTerminalOptions  = class;
  TVSTerminalDimensions  = class;
  TVSTerminalExitStatus  = class;
  TVSEnvironmentVariableMutator  = class;
  TVSEnvironmentVariableCollection  = class;
  TVSProgressOptions  = class;
  TVSQuickInput  = class;
  TVSQuickPick = class;
  TVSInputBox = class;
  TVSQuickInputButton  = class;
  TVSQuickInputButtons  = class;
  TVSTextDocumentContentChangeEvent  = class;
  TVSTextDocumentChangeEvent  = class;
  TVSTextDocumentWillSaveEvent  = class;
  TVSFileWillCreateEvent  = class;
  TVSFileCreateEvent  = class;
  TVSFileWillDeleteEvent  = class;
  TVSFileDeleteEvent  = class;
  TVSFileWillRenameEvent  = class;
  TVSFileRenameEvent  = class;
  TVSWorkspaceFoldersChangeEvent  = class;
  TVSWorkspaceFolder  = class;
  TVSNSworkspace  = class;
  TVSConfigurationChangeEvent  = class;
  TVSNSlanguages  = class;
  TVSSourceControlInputBox  = class;
  TVSSourceControlResourceThemableDecorations  = class;
  TVSSourceControlResourceDecorations = class;
  TVSSourceControlResourceState  = class;
  TVSSourceControlResourceGroup  = class;
  TVSSourceControl  = class;
  TVSNSscm  = class;
  TVSDebugProtocolMessage  = class;
  TVSDebugProtocolSource  = class;
  TVSDebugProtocolBreakpoint  = class;
  TVSDebugConfiguration  = class;
  TVSDebugSession  = class;
  TVSDebugSessionCustomEvent  = class;
  TVSDebugConfigurationProvider  = class;
  TVSDebugAdapterExecutable  = class;
  TVSDebugAdapterExecutableOptions  = class;
  TVSDebugAdapterServer  = class;
  TVSDebugAdapterNamedPipeServer  = class;
  TVSDebugAdapter  = class;
  TVSDebugAdapterInlineImplementation  = class;
  TVSDebugAdapterDescriptorFactory  = class;
  TVSDebugAdapterTracker  = class;
  TVSDebugAdapterTrackerFactory  = class;
  TVSDebugConsole  = class;
  TVSBreakpointsChangeEvent  = class;
  TVSBreakpoint  = class;
  TVSSourceBreakpoint  = class;
  TVSFunctionBreakpoint = class;
  TVSNSextensions  = class;
  TVSCommentThread  = class;
  TVSCommentAuthorInformation  = class;
  TVSCommentReaction  = class;
  TVSComment  = class;
  TVSCommentReply  = class;
  TVSCommentingRangeProvider  = class;
  TVSCommentOptions  = class;
  TVSCommentController  = class;
  TVSNSComments =  class;
  TVSAuthenticationSession  = class;
  TVSAuthenticationSessionAccountInformation  = class;
  TVSAuthenticationGetSessionOptions  = class;
  TVSAuthenticationProviderInformation  = class;
  TVSAuthenticationSessionsChangeEvent  = class;
  TVSNSauthentication  = class;
  TVSTaskEndEvent  = class;
  TVSTaskStartEvent  = class;
  TVSPseudoterminal = class;
  TVSExtension = class;

  TVSConfigurationScope = TJSObject;

  // Arrays

  TVSSelectionDynArray = array of TVSSelection;
  TVSRangeDynArray = array of TVSRange;
  TVSMarkDownStringDynArray = array of TVSRange;
  TVSSnippetStringDynArray = array of TVSSnippetString;
  TVSPositionDynArray = array of TVSPosition;
  TVSDisposableArray = array of TVSDisposable;
  TVSDiagnosticDynArray = array of TVSDiagnostic;
  TVSCodeActionKindDynArray = array of TVSCodeActionKind;
  TVSSymbolTagDynArray = Array of TVSSymbolTag;
  TVSDocumentSymbolDynArray =  array of TVSDocumentSymbol;
  TVSTextEditDynArray = array of TVSTextEdit;
  TVSSemanticTokensEditDynArray = Array of TVSSemanticTokensEdit;
  TVSParameterInformationDynArray = Array of TVSParameterInformation;
  TVSSignatureInformationDynArray = Array of TVSSignatureInformation;
  TVSCompletionItemTagDynArray = Array of TVSCompletionItemTag;
  TVSCompletionItemDynArray = Array of TVSCompletionItem;
  TVSCharacterPairDynArray = Array of TStringDynArray;
  TVSOnEnterRuleDynArray = array of TVSOnEnterRule;
  TVSUriDynArray = array of TVSUri;
  TVSDiagnosticRelatedInformationDynArray = array of TVSDiagnosticRelatedInformation;
  TVSDiagnosticTagDynArray = Array of TVSDiagnosticTag;
  TVSShellQuotedStringDynArray = Array of TVSShellQuotedString;
  TVSTaskExecutionDynArray = Array of TVSTaskExecution;
  TVSFileChangeEventDynArray = Array of TVSFileChangeEvent;
  TVSWebviewPortMappingDynArray = Array of TVSWebviewPortMapping;
  TVSTextEditorDynArray = Array of TVSTextEditor;
  TVSMessageItemDynArray = ARray of TVSMessageItem;
  TVSThenableDynArray = Array of TVSThenable;
  TVSTerminalDynArray = Array of TVSTerminal;
  TVSQuickPickItemDynArray = Array of TVSQuickPickItem;
  TVSQuickInputButtonDynArray = Array of TVSQuickInputButton;
  TVSTextDocumentContentChangeEventDynArray = Array of TVSTextDocumentContentChangeEvent;
  TVSWorkspaceFolderDynArray = array of TVSWorkspaceFolder;
  TVSTextDocumentDynArray = Array of TVSTextDocument;
  TVSDocumentFilterDynArray = Array of TVSDocumentFilter;
  TVSSourceControlResourceStateDynArray = Array of TVSSourceControlResourceState;
  TVSCommandDynArray = array of TVSCommand;
  TVSBreakpointDynArray=  Array of TVSBreakpoint;
  TVSExtensionDynArray = array of TVSExtension;
  TVSCommentDynArray = array of TVSComment;
  TVSCommentReactionDynArray = array of TVSCommentReaction;


  // Handlers


  TVSanyHandler = reference to function(aEvent : JSValue) : JSValue;
  TVSAuthenticationSessionsChangeEventHandler = reference to function(aEvent : TVSAuthenticationSessionsChangeEvent) : JSValue;
  TVSColorThemeHandler = reference to function(aEvent : TVSColorTheme) : JSValue;
  TVSConfigurationChangeEventHandler = reference to function(aEvent : TVSConfigurationChangeEvent) : JSValue;
  TVSDebugProtocolMessageHandler = reference to function(aEvent : TVSDebugProtocolMessage) : JSValue;
  TVSDiagnosticChangeEventHandler = reference to function(aEvent : TVSDiagnosticChangeEvent) : JSValue;
  TVSFileCreateEventHandler = reference to function(aEvent : TVSFileCreateEvent) : JSValue;
  TVSFileDeleteEventHandler = reference to function(aEvent : TVSFileDeleteEvent) : JSValue;
  TVSFileRenameEventHandler = reference to function(aEvent : TVSFileRenameEvent) : JSValue;
  TVSFileWillCreateEventHandler = reference to function(aEvent : TVSFileWillCreateEvent) : JSValue;
  TVSFileWillDeleteEventHandler = reference to function(aEvent : TVSFileWillDeleteEvent) : JSValue;
  TVSFileWillRenameEventHandler = reference to function(aEvent : TVSFileWillRenameEvent) : JSValue;
  TVSQuickInputButtonHandler = reference to function(aEvent : TVSQuickInputButton) : JSValue;
  TVSstringHandler = reference to function(aEvent : string) : JSValue;
  TVSTaskEndEventHandler = reference to function(aEvent : TVSTaskEndEvent) : JSValue;
  TVSTaskProcessEndEventHandler = reference to function(aEvent : TVSTaskProcessEndEvent) : JSValue;
  TVSTaskProcessStartEventHandler = reference to function(aEvent : TVSTaskProcessStartEvent) : JSValue;
  TVSTaskStartEventHandler = reference to function(aEvent : TVSTaskStartEvent) : JSValue;
  TVSTerminalDimensionsHandler = reference to function(aEvent : TVSTerminalDimensions) : JSValue;
  TVSTerminalHandler = reference to function(aEvent : TVSTerminal) : JSValue;
  TVSTextDocumentChangeEventHandler = reference to function(aEvent : TVSTextDocumentChangeEvent) : JSValue;
  TVSTextDocumentHandler = reference to function(aEvent : TVSTextDocument) : JSValue;
  TVSTextDocumentWillSaveEventHandler = reference to function(aEvent : TVSTextDocumentWillSaveEvent) : JSValue;
  TVSTextEditorOptionsChangeEventHandler = reference to function(aEvent : TVSTextEditorOptionsChangeEvent) : JSValue;
  TVSTextEditorSelectionChangeEventHandler = reference to function(aEvent : TVSTextEditorSelectionChangeEvent) : JSValue;
  TVSTextEditorHandler = reference to function(aEvent : TVSTextEditor) : JSValue;
  TVSTextEditorViewColumnChangeEventHandler = reference to function(aEvent : TVSTextEditorViewColumnChangeEvent) : JSValue;
  TVSTextEditorVisibleRangesChangeEventHandler = reference to function(aEvent : TVSTextEditorVisibleRangesChangeEvent) : JSValue;
  TVSTreeViewExpansionEventHandler = reference to function(aEvent : TVSTreeViewExpansionEvent) : JSValue;
  TVSTreeViewSelectionChangeEventHandler = reference to function(aEvent : TVSTreeViewSelectionChangeEvent) : JSValue;
  TVSTreeViewVisibilityChangeEventHandler = reference to function(aEvent : TVSTreeViewVisibilityChangeEvent) : JSValue;
  TVSvoidHandler = reference to function() : JSValue;
  TVSnumberHandler = reference to function(aEvent : NativeInt) : JSValue;
  TVSWebviewPanelOnDidChangeViewStateEventHandler = reference to function(aEvent : TVSWebviewPanelOnDidChangeViewStateEvent) : JSValue;
  TVSWindowStateHandler = reference to function(aEvent : TVSWindowState) : JSValue;
  TVSWorkspaceFoldersChangeEventHandler = reference to function(aEvent : TVSWorkspaceFoldersChangeEvent) : JSValue;


  TVSCommand  = class external name 'vscode.Command' (TJSObject)
  Public
    title: string;
    TVSCommand: string;
    tooltip: string;
    arguments: TJSValueDynArray;
  end;

  TVSTextLine  = class external name 'vscode.TextLine' (TJSObject)
  Private
    FlineNumber : NativeInt; external name 'lineNumber';
    Ftext : string; external name 'text';
    Frange : TVSRange; external name 'range';
    FrangeIncludingLineBreak : TVSRange; external name 'rangeIncludingLineBreak';
    FfirstNonWhitespaceCharacterIndex : NativeInt; external name 'firstNonWhitespaceCharacterIndex';
    FisEmptyOrWhitespace : boolean; external name 'isEmptyOrWhitespace';
  Public
    Property lineNumber : NativeInt read FlineNumber;
    Property text : string read Ftext;
    Property range : TVSRange read Frange;
    Property rangeIncludingLineBreak : TVSRange read FrangeIncludingLineBreak;
    Property firstNonWhitespaceCharacterIndex : NativeInt read FfirstNonWhitespaceCharacterIndex;
    Property isEmptyOrWhitespace : boolean read FisEmptyOrWhitespace;
  end;

  TVSTextDocument  = class external name 'vscode.TextDocument' (TJSObject)
  Private
    Furi : TVSUri; external name 'uri';
    FfileName : string; external name 'fileName';
    FisUntitled : boolean; external name 'isUntitled';
    FlanguageId : string; external name 'languageId';
    Fversion : NativeInt; external name 'version';
    FisDirty : boolean; external name 'isDirty';
    FisClosed : boolean; external name 'isClosed';
    Feol : TVSEndOfLine; external name 'eol';
    FlineCount : NativeInt; external name 'lineCount';
  Public
    function save() : TVSThenable; // boolean
    function lineAt(line: NativeInt) : TVSTextLine;
    function lineAt(position: TVSPosition) : TVSTextLine;
    function offsetAt(position: TVSPosition) : NativeInt;
    function positionAt(offset: NativeInt) : TVSPosition;
    function getText(range: TVSRange) : string; overload;
    function getText() : string; overload;
    function getWordRangeAtPosition(position: TVSPosition) : TVSRange; overload;
    function getWordRangeAtPosition(position: TVSPosition; regex: TJSRegExp) : TVSRange; overload;
    function validateRange(range: TVSRange) : TVSRange;
    function validatePosition(position: TVSPosition) : TVSPosition;
    Property uri : TVSUri read Furi;
    Property fileName : string read FfileName;
    Property isUntitled : boolean read FisUntitled;
    Property languageId : string read FlanguageId;
    Property version : NativeInt read Fversion;
    Property isDirty : boolean read FisDirty;
    Property isClosed : boolean read FisClosed;
    Property eol : TVSEndOfLine read Feol;
    Property lineCount : NativeInt read FlineCount;
  end;

  TVSPositionTranslate = class external name 'Object' (TJSObject)
    lineDelta : NativeInt;
    characterDelta: NativeInt;
  end;

  TVSPositionWith = class external name 'Object' (TJSObject)
    line : NativeInt;
    character: NativeInt;
  end;

  TVSPosition  = class external name 'vscode.Position' (TJSObject)
    Fline : NativeInt; external name 'line';
    Fcharacter : NativeInt; external name 'character';
  Public
    Property line : NativeInt read Fline;
    Property character : NativeInt read Fcharacter;
    constructor New(line: NativeInt; character: NativeInt);
    function isBefore(other: TVSPosition) : boolean;
    function isBeforeOrEqual(other: TVSPosition) : boolean;
    function isAfter(other: TVSPosition) : boolean;
    function isAfterOrEqual(other: TVSPosition) : boolean;
    function isEqual(other: TVSPosition) : boolean;
    function compareTo(other: TVSPosition) : NativeInt;
    function translate() : TVSPosition;
    function translate(lineDelta: NativeInt) : TVSPosition;
    function translate(lineDelta: NativeInt; characterDelta: NativeInt) : TVSPosition;
    function translate(aChange : TVSPositionTranslate): TVSPosition;
    function with_() : TVSPosition; overload; external name 'with';
    function with_(line: NativeInt) : TVSPosition; overload; external name 'with';
    function with_(line: NativeInt; character: NativeInt) : TVSPosition; overload; external name 'with';
    function with_(change:  TVSPositionWith): TVSPosition;
  end;

  TVSRangeWith = class external name 'Object' (TJSObject)
    start : TVSPosition;
    end_: TVSPosition; external name 'end';
  end;

  TVSRange  = class external name 'vscode.Range' (TJSObject)
  Private
    Fstart : TVSPosition; external name 'start';
    Fend : TVSPosition; external name 'end';
  Public
    isEmpty: boolean;
    isSingleLine: boolean;
    constructor New(aStart: TVSPosition; aEnd: TVSPosition);
    constructor New(aStartLine: NativeInt; aStartCharacter: NativeInt; aEndLine: NativeInt; aEndCharacter: NativeInt);
    function contains(position: TVSPosition) : boolean;
    function contains(Range:  TVSRange) : boolean;
    function isEqual(other: TVSRange) : boolean;
    function intersection(range: TVSRange) : TVSRange ;
    function union(other: TVSRange) : TVSRange;
    function with_(aStart: TVSPosition; aEnd: TVSPosition) : TVSRange; overload; external name 'with';
    function with_(aStart: TVSPosition) : TVSRange; overload; external name 'with';
    function with_() : TVSRange;overload; external name 'with';
    function with_(change: TVSRangeWith): TVSRange;
    Property start : TVSPosition read Fstart;
    Property end_ : TVSPosition read Fend;
  end;


  TVSSelection = class external name 'vscode.Selection' (TVSRange)
  Public
    anchor: TVSPosition;
    active: TVSPosition;
    isReversed: boolean;
    constructor New(anchor: TVSPosition; active: TVSPosition);
    constructor New(anchorLine: NativeInt; anchorCharacter: NativeInt;  activeLine: NativeInt; activeCharacter: NativeInt);
  end;


  TVSTextEditorSelectionChangeKind = (TextEditorSelectionChangeKind_0,Keyboard,Mouse,Command);


  TVSTextEditorSelectionChangeEvent  = class external name 'vscode.TextEditorSelectionChangeEvent' (TJSObject)
  Private
    FtextEditor : TVSTextEditor; external name 'textEditor';
    Fselections : TVSSelectionDynArray; external name 'selections';
    Fkind : TVSTextEditorSelectionChangeKind; external name 'kind';
  Public
    Property textEditor : TVSTextEditor read FtextEditor;
    Property selections : TVSSelectionDynArray read Fselections;
    Property kind : TVSTextEditorSelectionChangeKind read Fkind;
  end;


  TVSTextEditorVisibleRangesChangeEvent  = class external name 'vscode.TextEditorVisibleRangesChangeEvent' (TJSObject)
  Private
    FtextEditor : TVSTextEditor; external name 'textEditor';
    FvisibleRanges : TVSRangeDynArray; external name 'visibleRanges';
  Public
    Property textEditor : TVSTextEditor read FtextEditor;
    Property visibleRanges : TVSRangeDynArray read FvisibleRanges;
  end;


  TVSTextEditorOptionsChangeEvent  = class external name 'vscode.TextEditorOptionsChangeEvent' (TJSObject)
  Private
    FtextEditor : TVSTextEditor; external name 'textEditor';
    Foptions : TVSTextEditorOptions; external name 'options';
  Public
    Property textEditor : TVSTextEditor read FtextEditor;
    Property options : TVSTextEditorOptions read Foptions;
  end;


  TVSTextEditorViewColumnChangeEvent  = class external name 'vscode.TextEditorViewColumnChangeEvent' (TJSObject)
  Private
    FtextEditor : TVSTextEditor; external name 'textEditor';
    FviewColumn : TVSViewColumn; external name 'viewColumn';
  Public
    Property textEditor : TVSTextEditor read FtextEditor;
    Property viewColumn : TVSViewColumn read FviewColumn;
  end;



  TVSTextEditorOptions  = class external name 'vscode.TextEditorOptions' (TJSObject)
  Public
    tabSize: NativeInt;
    tabSizeStr : string; external name 'tabSize';
    insertSpaces: boolean;
    insertSpacesStr: string; external name 'insertSpaces';
    cursorStyle: TVSTextEditorCursorStyle;
    lineNumbers: TVSTextEditorLineNumbersStyle;
  end;


  TVSTextEditorDecorationType  = class external name 'vscode.TextEditorDecorationType' (TJSObject)
  Private
    Fkey : string; external name 'key';
  Public
    Property key : string read Fkey;
    procedure dispose() ;
  end;


  TVSTextDocumentShowOptions  = class external name 'vscode.TextDocumentShowOptions' (TJSObject)
  Public
    viewColumn: TVSViewColumn;
    preserveFocus: boolean;
    preview: boolean;
    selection: TVSRange;
  end;


  TVSThemeColor  = class external name 'vscode.ThemeColor' (TJSObject)
  Public
    constructor New(id: string);
  end;


  TVSThemeIcon  = class external name 'vscode.ThemeIcon' (TJSObject)
  Private
    Fid : string; external name 'id';
    Fcolor : TVSThemeColor; external name 'color';
  Public
    class var File_: TVSThemeIcon; external name 'File';
    class var Folder: TVSThemeIcon;
    constructor New(id: string; color: TVSThemeColor);
    Property id : string read Fid;
    Property color : TVSThemeColor read Fcolor;
  end;


  TVSThemableDecorationRenderOptions  = class external name 'vscode.ThemableDecorationRenderOptions' (TJSObject)
  Public
    backgroundColor: string;
    backgroundColorObj : TVSThemeColor; external name 'backgroundColor';
    outline: string;
    outlineColor : string;
    outlineColorObj: TVSThemeColor; external name 'outlineColor';
    outlineStyle: string;
    outlineWidth: string;
    border: string;
    borderColor : string;
    borderColorObj: TVSThemeColor; external name 'borderColor';
    borderRadius: string;
    borderSpacing: string;
    borderStyle: string;
    borderWidth: string;
    fontStyle: string;
    fontWeight: string;
    textDecoration: string;
    cursor: string;
    color : string;
    colorObj: TVSThemeColor; external name 'color';
    opacity: string;
    letterSpacing: string;
    gutterIconPath : string;
    gutterIconPathObj: TVSUri; external name 'gutterIconPath';
    gutterIconSize: string;
    overviewRulerColor : string;
    overviewRulerColorObj: TVSThemeColor; external name 'overviewRulerColor';
    before: TVSThemableDecorationAttachmentRenderOptions;
    after: TVSThemableDecorationAttachmentRenderOptions;
  end;


  TVSThemableDecorationAttachmentRenderOptions  = class external name 'vscode.ThemableDecorationAttachmentRenderOptions' (TJSObject)
  Public
    contentText: string;
    contentIconPath : string;
    contentIconPathObj: TVSUri; external name 'contentIconPath';
    border: string;
    borderColor : string;
    borderColorObj: TVSThemeColor; external name 'borderColor';
    fontStyle: string;
    fontWeight: string;
    textDecoration: string;
    color : string;
    colorObj: TVSThemeColor; external name 'color';
    backgroundColor : string;
    backgroundColorObj: TVSThemeColor; external name 'backgroundColor';
    margin: string;
    width: string;
    height: string;
  end;


  TVSDecorationRenderOptions = class external name 'vscode.DecorationRenderOptions' (TVSThemableDecorationRenderOptions)
  Public
    isWholeLine: boolean;
    rangeBehavior: TVSDecorationRangeBehavior;
    overviewRulerLane: TVSOverviewRulerLane;
    light: TVSThemableDecorationRenderOptions;
    dark: TVSThemableDecorationRenderOptions;
  end;


  TVSDecorationOptions  = class external name 'vscode.DecorationOptions' (TJSObject)
  Public
    range: TVSRange;
    hoverMessage : String;
    hoverMessageObj : TVSMarkdownString; external name 'hoverMessage';
    hoverMessageArr : TVSMarkdownStringDynArray; external name 'hoverMessage';
    renderOptions: TVSDecorationInstanceRenderOptions;
  end;
  TVSDecorationOptionsDynArray = Array of TVSDecorationOptions;

  TVSThemableDecorationInstanceRenderOptions  = class external name 'vscode.ThemableDecorationInstanceRenderOptions' (TJSObject)
  Public
    before: TVSThemableDecorationAttachmentRenderOptions;
    after: TVSThemableDecorationAttachmentRenderOptions;
  end;


  TVSDecorationInstanceRenderOptions  = class external name 'vscode.DecorationInstanceRenderOptions' (TVSThemableDecorationInstanceRenderOptions)
  Public
    light: TVSThemableDecorationInstanceRenderOptions;
    dark: TVSThemableDecorationInstanceRenderOptions;
  end;

  TVSTextEditorEditCallBack = Reference to procedure (editBuilder: TVSTextEditorEdit);

  TVSTextEditorEditOptions = class external name 'Object' (TJSObject)
   undoStopBefore: boolean;
   undoStopAfter: boolean;
  end;

  TVSTextEditor  = class external name 'vscode.TextEditor' (TJSObject)
  Private
    Fdocument : TVSTextDocument; external name 'document';
    FvisibleRanges : TVSRangeDynArray; external name 'visibleRanges';
    FviewColumn : TVSViewColumn; external name 'viewColumn';
  Public
    selection: TVSSelection;
    selections: TVSSelectionDynArray;
    options: TVSTextEditorOptions;
    function edit(callback: TVSTextEditorEditCallBack; options: TVSTextEditorEditOptions): TVSThenable; overload; // boolean
    function edit(callback: TVSTextEditorEditCallBack) : TVSThenable; overload; // boolean
    function insertSnippet(snippet: TVSSnippetString): TVSThenable; overload; // boolean
    function insertSnippet(snippet: TVSSnippetString; location: TVSPosition; options: TVSTextEditorEditOptions): TVSThenable; overload; // boolean
    function insertSnippet(snippet: TVSSnippetString; location: TVSPosition): TVSThenable; overload; // boolean
    function insertSnippet(snippet: TVSSnippetString; location: TVSRange; options: TVSTextEditorEditOptions): TVSThenable; overload; // boolean
    function insertSnippet(snippet: TVSSnippetString; location: TVSRange): TVSThenable; overload; // boolean
    function insertSnippet(snippet: TVSSnippetString; location: TVSPositionDynArray): TVSThenable; overload; // boolean
    function insertSnippet(snippet: TVSSnippetString; location: TVSPositionDynArray;  options: TVSTextEditorEditOptions): TVSThenable; overload; // boolean
    function insertSnippet(snippet: TVSSnippetString; location: TVSRangeDynArray;  options: TVSTextEditorEditOptions): TVSThenable; overload; // boolean
    function insertSnippet(snippet: TVSSnippetString; location: TVSRangeDynArray): TVSThenable; overload; // boolean
    function insertSnippet(snippet: TVSSnippetString; options: TVSTextEditorEditOptions): TVSThenable; // boolean
    procedure setDecorations(decorationType: TVSTextEditorDecorationType; rangesOrOptions: TVSDecorationOptionsDynArray) ; overload;
    procedure setDecorations(decorationType: TVSTextEditorDecorationType; rangesOrOptions: TVSRangeDynArray) ; overload;
    procedure revealRange(range: TVSRange); overload;
    procedure revealRange(range: TVSRange; revealType: TVSTextEditorRevealType) ; overload;
    procedure show(column: TVSViewColumn);
    procedure show() ;
    procedure hide() ;
    Property document : TVSTextDocument read Fdocument;
    Property visibleRanges : TVSRangeDynArray read FvisibleRanges;
    Property viewColumn : TVSViewColumn read FviewColumn;
  end;


  TVSTextEditorEdit  = class external name 'vscode.TextEditorEdit' (TJSObject)
  Public
    procedure replace(location: TVSSelection; value: string) ;overload;
    procedure replace(location: TVSRange; value: string) ;overload;
    procedure replace(location: TVSPosition; value: string) ;overload;
    procedure insert(location: TVSPosition; value: string) ;
    procedure delete(location: TVSSelection) ; overload;
    procedure delete(location: TVSRange) ; overload;
    procedure setEndOfLine(endOfLine: TVSEndOfLine) ;
  end;

  TURIChangeOptions = class external name 'Object' (TJSObject)
    scheme: string;
    authority: string;
    path: string;
    query: string;
    fragment: string;
  end;

  TVSUri  = class external name 'vscode.Uri' (TJSObject)
  Private
    Fscheme : string; external name 'scheme';
    Fauthority : string; external name 'authority';
    Fpath : string; external name 'path';
    Fquery : string; external name 'query';
    Ffragment : string; external name 'fragment';
    FfsPath : string; external name 'fsPath';
  Public
    class function parse(value: string; strict: boolean) : TVSUri; overload;
    class function parse(value: string) : TVSUri;overload;
    class function file_(path: string) : TVSUri; external name 'file';
    class function joinPath(base: TVSUri) : TVSUri; varargs of string;
    constructor New(scheme: string; authority: string; path: string; query: string; fragment: string);
    Property scheme : string read Fscheme;
    Property authority : string read Fauthority;
    Property path : string read Fpath;
    Property query : string read Fquery;
    Property fragment : string read Ffragment;
    Property fsPath : string read FfsPath;
    function with_(change: TURIChangeOptions): TVSUri; external name 'with';
    function toString(skipEncoding: boolean) : string; reintroduce; overload;
    function toString() : string; reintroduce; overload;
    function toJSON() : JSValue;
  end;


  TVSCancellationToken  = class external name 'vscode.CancellationToken' (TJSObject)
  Public
    isCancellationRequested: boolean;
    function onCancellationRequested(aHandler : TVSanyHandler) : TVSDisposable; overload;
    function onCancellationRequested(aHandler : TVSanyHandler; aThis : JSvalue): TVSDisposable; overload;
    function onCancellationRequested(aHandler : TVSanyHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
  end;


  TVSCancellationTokenSource  = class external name 'vscode.CancellationTokenSource' (TJSObject)
  Public
    token: TVSCancellationToken;
    procedure cancel() ;
    procedure dispose() ;
  end;


  TVSDisposable  = class external name 'vscode.TVSDisposable' (TJSObject)
  Public
    function from(): TVSDisposable; varargs of TJSObject;
    constructor New(callOnDispose: TVSVoidHandler);
    function dispose() : JSValue;
  end;


(*
  TVSEventEmitter<T>  = class external name 'vscode.EventEmitter<T>' (TJSObject)
  Public
    function event(aHandler : TVSTHandler) : TVSDisposable; overload;
    function event(aHandler : TVSTHandler; aThis : JSvalue): TVSDisposable; overload;
    function event(aHandler : TVSTHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    procedure fire(data: T) ;
    procedure dispose() ;
  end;
*)

  TVSFileSystemWatcher = class external name 'vscode.TVSFileSystemWatcher' (TVSDisposable)
  Public
    ignoreCreateEvents: boolean;
    ignoreChangeEvents: boolean;
    ignoreDeleteEvents: boolean;
    function onDidCreate(aHandler : TVSUriHandler) : TVSDisposable; overload;
    function onDidCreate(aHandler : TVSUriHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidCreate(aHandler : TVSUriHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidChange(aHandler : TVSUriHandler) : TVSDisposable; overload;
    function onDidChange(aHandler : TVSUriHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChange(aHandler : TVSUriHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidDelete(aHandler : TVSUriHandler) : TVSDisposable; overload;
    function onDidDelete(aHandler : TVSUriHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidDelete(aHandler : TVSUriHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
  end;


  TVSTextDocumentContentProvider  = class external name 'vscode.TextDocumentContentProvider' (TJSObject)
  Public
    function onDidChange(aHandler : TVSUriHandler) : TVSDisposable; overload;
    function onDidChange(aHandler : TVSUriHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChange(aHandler : TVSUriHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function provideTextDocumentContent(uri: TVSUri; token: TVSCancellationToken) : JSValue;
  end;


  TVSQuickPickItem  = class external name 'vscode.QuickPickItem' (TJSObject)
  Public
    label_: string; external name 'label';
    description: string;
    detail: string;
    picked: boolean;
    alwaysShow: boolean;
  end;


  TVSQuickPickOptions  = class external name 'vscode.TVSQuickPickOptions' (TJSObject)
  Public
    matchOnDescription: boolean;
    matchOnDetail: boolean;
    placeHolder: string;
    ignoreFocusOut: boolean;
    canPickMany: boolean;
    function onDidSelectItem(item: TVSQuickPickItem) : JSValue;
    function onDidSelectItem(item: string) : JSValue;
  end;


  TVSWorkspaceFolderPickOptions  = class external name 'vscode.WorkspaceFolderPickOptions' (TJSObject)
  Public
    placeHolder: string;
    ignoreFocusOut: boolean;
  end;


  TVSOpenDialogOptions  = class external name 'vscode.OpenDialogOptions' (TJSObject)
  Public
    defaultUri: TVSUri;
    openLabel: string;
    canSelectFiles: boolean;
    canSelectFolders: boolean;
    canSelectMany: boolean;
    filters: TJSObject;
    title: string;
  end;


  TVSSaveDialogOptions  = class external name 'vscode.SaveDialogOptions' (TJSObject)
  Public
    defaultUri: TVSUri;
    saveLabel: string;
    filters: TJSObject;
    title: string;
  end;


  TVSMessageItem  = class external name 'vscode.MessageItem' (TJSObject)
  Public
    title: string;
    isCloseAffordance: boolean;
  end;


  TVSMessageOptions  = class external name 'vscode.TVSMessageOptions' (TJSObject)
  Public
    modal: boolean;
  end;

  TVSInputBoxOptionsValidateHandler = function(S : String) : JSValue;
  TVSInputBoxOptions  = class external name 'vscode.InputBoxOptions' (TJSObject)
  Public
    value: string;
    valueSelection: TNativeIntDynArray;
    prompt: string;
    placeHolder: string;
    password: boolean;
    ignoreFocusOut: boolean;
    validateInput : TVSInputBoxOptionsValidateHandler;
  end;


  TVSRelativePattern  = class external name 'vscode.RelativePattern' (TJSObject)
  Public
    base: string;
    pattern: string;
    constructor New(base: TVSWorkspaceFolder; pattern: string);overload;
    constructor New(base: TVSUri; pattern: string);overload;
    constructor New(base: string; pattern: string);overload;
  end;


  TVSDocumentFilter  = class external name 'vscode.DocumentFilter' (TJSObject)
  Private
    Flanguage : string; external name 'language';
    Fscheme : string; external name 'scheme';
    Fpattern : string; external name 'pattern';
    FpatternObj : TVSRelativePattern; external name 'pattern';
  Public
    Property language : string read Flanguage;
    Property scheme : string read Fscheme;
    Property pattern : string read Fpattern;
    Property patternObj : TVSRelativePattern read FpatternObj;
  end;


  // export type DocumentSelector = DocumentFilter | string | TDocumentFilter | stringDynArray;
  // export type TJSObject<T> = T | undefined | null | TVSThenable; // T | undefined | null
  TVSCodeActionKind  = class external name 'vscode.CodeActionKind' (TJSObject)
  Private
    Fvalue : string; external name 'value';
  Public
    class var Empty: TVSCodeActionKind;
    class var QuickFix: TVSCodeActionKind;
    class var Refactor: TVSCodeActionKind;
    class var RefactorExtract: TVSCodeActionKind;
    class var RefactorInline: TVSCodeActionKind;
    class var RefactorRewrite: TVSCodeActionKind;
    class var Source: TVSCodeActionKind;
    class var SourceOrganizeImports: TVSCodeActionKind;
    class var SourceFixAll: TVSCodeActionKind;
    private constructor New(value: string);
    Property value : string read Fvalue;
    function append(parts: string) : TVSCodeActionKind;
    function intersects(other: TVSCodeActionKind) : boolean;
    function contains(other: TVSCodeActionKind) : boolean;
  end;


  TVSCodeActionContext  = class external name 'vscode.CodeActionContext' (TJSObject)
  Private
    Fdiagnostics : TVSDiagnosticDynArray; external name 'diagnostics';
    Fonly : TVSCodeActionKind; external name 'only';
  Public
    Property diagnostics : TVSDiagnosticDynArray read Fdiagnostics;
    Property only : TVSCodeActionKind read Fonly;
  end;

  TVSCodeActionDisabled  = class external name 'Object' (TJSObject)
  Private
    Freason : string; external name 'reason';
  Public
    Property reason : string read Freason;
  end;

  TVSCodeAction  = class external name 'vscode.CodeAction' (TJSObject)
  Public
    title: string;
    edit: TVSWorkspaceEdit;
    diagnostics: TVSDiagnosticDynArray;
    Command: TVSCommand;
    kind: TVSCodeActionKind;
    isPreferred: boolean;
    disabled: TVSCodeActionDisabled;
    constructor New(title: string); overload;
    constructor New(title: string; kind: TVSCodeActionKind); overload;
  end;


  TVSCodeActionProvider = class external name 'vscode.TVSCodeActionProvider' (TJSObject)
  Public
    function provideCodeActions(document: TVSTextDocument; range: TVSRange; context: TVSCodeActionContext; token: TVSCancellationToken) : JSValue;
    function provideCodeActions(document: TVSTextDocument; range: TVSSelection; context: TVSCodeActionContext; token: TVSCancellationToken) : JSValue;
    function resolveCodeAction(codeAction: TVSCodeAction; token: TVSCancellationToken) : TJSObject;
  end;

  TVSCodeActionProviderMetadataDocumentationItem = class external name 'Object' (TJSObject)
    Fkind : TVSCodeActionKind; external name 'kind';
    Fcommand : TVSCommand; external name 'command';
  end;
  TVSCodeActionProviderMetadataDocumentationArray = Array of TVSCodeActionProviderMetadataDocumentationItem;

  TVSCodeActionProviderMetadata  = class external name 'vscode.TVSCodeActionProviderMetadata' (TJSObject)
  Private
    FprovidedCodeActionKinds : TVSCodeActionKindDynArray; external name 'providedCodeActionKinds';
    fdocumentation : TVSCodeActionProviderMetadataDocumentationArray; external name 'documentation';
  Public
    Property providedCodeActionKinds : TVSCodeActionKindDynArray read FprovidedCodeActionKinds;
    property documentation: TVSCodeActionProviderMetadataDocumentationArray Read fdocumentation;
  end;


  TVSCodeLens  = class external name 'vscode.CodeLens' (TJSObject)
  Private
    FisResolved : boolean; external name 'isResolved';
  Public
    range: TVSRange;
    command: TVSCommand;
    constructor New(range: TVSRange; command: TVSCommand);
    constructor New(range: TVSRange);
    Property isResolved : boolean read FisResolved;
  end;


  TVSCodeLensProvider = class external name 'vscode.TVSCodeLensProvider' (TJSObject)
  Public
    function onDidChangeCodeLenses(aHandler : TVSvoidHandler) : TVSDisposable; overload;
    function onDidChangeCodeLenses(aHandler : TVSvoidHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeCodeLenses(aHandler : TVSvoidHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function provideCodeLenses(document: TVSTextDocument; token: TVSCancellationToken) : JSValue;
    function resolveCodeLens(codeLens: TJSObject; token: TVSCancellationToken) : TJSObject;
  end;


  TVSDefinitionProvider  = class external name 'vscode.DefinitionProvider' (TJSObject)
  Public
    function provideDefinition(document: TVSTextDocument; position: TVSPosition; token: TVSCancellationToken) : JSValue;
  end;


  TVSImplementationProvider  = class external name 'vscode.ImplementationProvider' (TJSObject)
  Public
    function provideImplementation(document: TVSTextDocument; position: TVSPosition; token: TVSCancellationToken) : JSValue;
  end;


  TVSTypeDefinitionProvider  = class external name 'vscode.TypeDefinitionProvider' (TJSObject)
  Public
    function provideTypeDefinition(document: TVSTextDocument; position: TVSPosition; token: TVSCancellationToken) : JSValue;
  end;


  TVSDeclarationProvider  = class external name 'vscode.DeclarationProvider' (TJSObject)
  Public
    function provideDeclaration(document: TVSTextDocument; position: TVSPosition; token: TVSCancellationToken) : JSValue;
  end;


  TVSMarkdownString  = class external name 'vscode.MarkdownString' (TJSObject)
  Private
    FsupportThemeIcons : boolean; external name 'supportThemeIcons';
  Public
    value: string;
    isTrusted: boolean;
    Property supportThemeIcons : boolean read FsupportThemeIcons;
    constructor New(value: string; supportThemeIcons: boolean); overload;
    constructor New(value: string); overload;
    constructor New(); overload;
    function appendText(value: string) : TVSMarkdownString;
    function appendMarkdown(value: string) : TVSMarkdownString;
    function appendCodeblock(value: string; language: string) : TVSMarkdownString;
    function appendCodeblock(value: string) : TVSMarkdownString;
  end;



  TVSHover  = class external name 'vscode.Hover' (TJSObject)
  Public
    contents: TVSMarkdownStringDynArray;
    range: TVSRange;
    constructor New(contents: TVSMarkdownStringDynArray); overload;
    constructor New(contents: TVSMarkdownStringDynArray; range: TVSRange); overload;
    constructor New(contents: TVSMarkdownString); overload;
    constructor New(contents: TVSMarkdownString; range: TVSRange) overload;
  end;


  TVSHoverProvider  = class external name 'vscode.HoverProvider' (TJSObject)
  Public
    function provideHover(document: TVSTextDocument; position: TVSPosition; token: TVSCancellationToken) : JSValue;
  end;


  TVSEvaluatableExpression  = class external name 'vscode.EvaluatableExpression' (TJSObject)
  Private
    Frange : TVSRange; external name 'range';
    Fexpression : string; external name 'expression';
  Public
    Property range : TVSRange read Frange;
    Property expression : string read Fexpression;
    constructor New(range: TVSRange; aexpression: string); overload;
    constructor New(range: TVSRange); overload;
  end;


  TVSEvaluatableExpressionProvider  = class external name 'vscode.EvaluatableExpressionProvider' (TJSObject)
  Public
    function provideEvaluatableExpression(document: TVSTextDocument; position: TVSPosition; token: TVSCancellationToken) : JSValue;
  end;




  TVSDocumentHighlight  = class external name 'vscode.DocumentHighlight' (TJSObject)
  Public
    range: TVSRange;
    kind: TVSDocumentHighlightKind;
    constructor New(range: TVSRange; kind: TVSDocumentHighlightKind); overload;
    constructor New(range: TVSRange); overload;
  end;


  TVSDocumentHighlightProvider  = class external name 'vscode.DocumentHighlightProvider' (TJSObject)
  Public
    function provideDocumentHighlights(document: TVSTextDocument; position: TVSPosition; token: TVSCancellationToken) : JSValue;
  end;




  TVSSymbolInformation  = class external name 'vscode.SymbolInformation' (TJSObject)
  Public
    name: string;
    containerName: string;
    kind: TVSSymbolKind;
    tags: TVSSymbolTagDynArray;
    location: TVSLocation;
    constructor New(name: string; kind: TVSSymbolKind; containerName: string;  location: TVSLocation);
    constructor New(name: string; kind: TVSSymbolKind; range: TVSRange); overload;
    constructor New(name: string; kind: TVSSymbolKind; range: TVSRange; uri: TVSUri); overload;
    constructor New(name: string; kind: TVSSymbolKind; range: TVSRange; uri: TVSUri; containerName: string); overload;
  end;


  TVSDocumentSymbol  = class external name 'vscode.DocumentSymbol' (TJSObject)
  Public
    name: string;
    detail: string;
    kind: TVSSymbolKind;
    tags: TVSSymbolTagDynArray;
    range: TVSRange;
    selectionRange: TVSRange;
    children: TVSDocumentSymbolDynArray;
    constructor New(name: string; detail: string; kind: TVSSymbolKind; range: TVSRange; selectionRange: TVSRange);
  end;


  TVSDocumentSymbolProvider  = class external name 'vscode.DocumentSymbolProvider' (TJSObject)
  Public
    function provideDocumentSymbols(document: TVSTextDocument; token: TVSCancellationToken) : JSValue;
  end;


  TVSDocumentSymbolProviderMetadata  = class external name 'vscode.DocumentSymbolProviderMetadata' (TJSObject)
  Public
    label_: string; external name 'label';
  end;


  TVSWorkspaceSymbolProvider  = class external name 'vscode.WorkspaceSymbolProvider' (TJSObject)
  Public
    function provideWorkspaceSymbols(query: string; token: TVSCancellationToken) : TJSObjectDynArray;
    function resolveWorkspaceSymbol(symbol: TJSObject; token: TVSCancellationToken) : JSValue;
  end;


  TVSReferenceContext  = class external name 'vscode.ReferenceContext' (TJSObject)
  Public
    includeDeclaration: boolean;
  end;


  TVSReferenceProvider  = class external name 'vscode.ReferenceProvider' (TJSObject)
  Public
    function provideReferences(document: TVSTextDocument; position: TVSPosition; context: TVSReferenceContext; token: TVSCancellationToken) : JSValue;
  end;


  TVSTextEdit  = class external name 'vscode.TextEdit' (TJSObject)
  Public
    range: TVSRange;
    newText: string;
    newEol: TVSEndOfLine;
    constructor New(range: TVSRange; newText: string);
    class function replace(range: TVSRange; newText: string) : TVSTextEdit;
    class function insert(position: TVSPosition; newText: string) : TVSTextEdit;
    class function delete(range: TVSRange) : TVSTextEdit;
    class function setEndOfLine(eol: TVSEndOfLine) : TVSTextEdit;
  end;

  TVSIconPathObject = class external name 'Object' (TJSObject)
    light: TVSUri;
    dark: TVSUri;
  end;

  TVSWorkspaceEditEntryMetadata  = class external name 'vscode.WorkspaceEditEntryMetadata' (TJSObject)
  Public
    needsConfirmation: boolean;
    label_: string; external name 'label';
    description: string;
    iconPath: TVSUri ;
    iconPathObj : TVSIconPathObject; external name 'iconPath';
    iconPathTheme : TVSThemeIcon;external name 'iconPath';
  end;

  TVSWorkspaceEditCreateOptions = class external name 'Object' (TJSObject)
    overwrite: boolean;
    ignoreIfExists: boolean;
  end;

  TVSWorkspaceEditDeleteOptions = class external name 'Object' (TJSObject)
    recursive: boolean;
    ignoreIfNotExists: boolean;
  end;

  TVSWorkspaceEdit  = class external name 'vscode.WorkspaceEdit' (TJSObject)
  Public
    Fsize : NativeInt; external name 'size';
    Property size : NativeInt read Fsize;
    procedure replace(uri: TVSUri; range: TVSRange; newText: string; metadata: TVSWorkspaceEditEntryMetadata) ; overload;
    procedure replace(uri: TVSUri; range: TVSRange; newText: string) ; overload;
    procedure insert(uri: TVSUri; position: TVSPosition; newText: string; metadata: TVSWorkspaceEditEntryMetadata) ; overload;
    procedure insert(uri: TVSUri; position: TVSPosition; newText: string) ; overload;
    procedure delete(uri: TVSUri; range: TVSRange; metadata: TVSWorkspaceEditEntryMetadata); overload;
    procedure delete(uri: TVSUri; range: TVSRange) ; overload;
    function has(uri: TVSUri) : boolean;
    procedure set_(uri: TVSUri; edits: TVSTextEditDynArray); external name 'set';
    function get(uri: TVSUri) : TVSTextEditDynArray;
    procedure createFile(uri: TVSUri); overload;
    procedure createFile(uri: TVSUri; options: TVSWorkspaceEditCreateOptions); overload;
    procedure createFile(uri: TVSUri; options: TVSWorkspaceEditCreateOptions; metadata: TVSWorkspaceEditEntryMetadata); overload;
    procedure deleteFile(uri: TVSUri); overload;
    procedure deleteFile(uri: TVSUri; options: TVSWorkspaceEditDeleteOptions); overload;
    procedure deleteFile(uri: TVSUri; options: TVSWorkspaceEditDeleteOptions; metadata: TVSWorkspaceEditEntryMetadata); overload;
    procedure renameFile(oldUri: TVSUri; newUri: TVSUri);overload;
    procedure renameFile(oldUri: TVSUri; newUri: TVSUri; options:  TVSWorkspaceEditCreateOptions);overload;
    procedure renameFile(oldUri: TVSUri; newUri: TVSUri; options:  TVSWorkspaceEditCreateOptions; metadata: TVSWorkspaceEditEntryMetadata);overload;
    function entries() : TJSObjectDynArray;
  end;

  TVSSnippetStringappendPlaceholderHandler = function (snippet : TVSSnippetString) : JSValue;

  TVSSnippetString  = class external name 'vscode.SnippetString' (TJSObject)
  Public
    value: string;
    constructor New(); overload;
    constructor New(value: string); overload;
    function appendText(aText: string) : TVSSnippetString;
    function appendTabstop() : TVSSnippetString; overload;
    function appendTabstop(number: NativeInt) : TVSSnippetString; overload;
    function appendPlaceholder(value: string) : TVSSnippetString; overload;
    function appendPlaceholder(value: string; number: NativeInt) : TVSSnippetString; overload;
    function appendPlaceholder(value: TVSSnippetStringappendPlaceholderHandler) : TVSSnippetString; overload;
    function appendPlaceholder(value: TVSSnippetStringappendPlaceholderHandler; number: NativeInt) : TVSSnippetString; overload;
    function appendChoice(values: TstringDynArray) : TVSSnippetString; overload;
    function appendChoice(values: TstringDynArray; number: NativeInt) : TVSSnippetString; overload;
    function appendVariable(name: string; defaultValue: string ) : TVSSnippetString; overload;
    function appendVariable(name: string; defaultValue: TVSSnippetStringappendPlaceholderHandler) : TVSSnippetString; overload;
  end;


  TVSRenameProvider  = class external name 'vscode.RenameProvider' (TJSObject)
  Public
    function provideRenameEdits(document: TVSTextDocument; position: TVSPosition; newName: string; token: TVSCancellationToken) : JSValue;
    function prepareRename (document: TVSTextDocument; position: TVSPosition; token: TVSCancellationToken) : JSValue;
  end;


  TVSSemanticTokensLegend  = class external name 'vscode.SemanticTokensLegend' (TJSObject)
  Private
    FtokenTypes : TstringDynArray; external name 'tokenTypes';
    FtokenModifiers : TstringDynArray; external name 'tokenModifiers';
  Public
    Property tokenTypes : TstringDynArray read FtokenTypes;
    Property tokenModifiers : TstringDynArray read FtokenModifiers;
    constructor New(tokenTypes: TstringDynArray; tokenModifiers: TstringDynArray);
    constructor New(tokenTypes: TstringDynArray);
  end;


  TVSSemanticTokensBuilder  = class external name 'vscode.SemanticTokensBuilder' (TJSObject)
  Public
    constructor New(); overload;
    constructor New(legend: TVSSemanticTokensLegend); overload;
    procedure push(line: NativeInt; char: NativeInt; length: NativeInt; tokenType: NativeInt) ; overload;
    procedure push(line: NativeInt; char: NativeInt; length: NativeInt; tokenType: NativeInt; tokenModifiers: NativeInt) ; overload;
    procedure push(range: TVSRange; tokenType: string; tokenModifiers : TStringDynArray) ; overload;
    procedure push(range: TVSRange; tokenType: string) ; overload;
    function build(resultId : string) : TVSSemanticTokens;
    function build() : TVSSemanticTokens;
  end;


  TVSSemanticTokens  = class external name 'vscode.SemanticTokens' (TJSObject)
  Private
    FresultId : string; external name 'resultId';
    Fdata : TUint32DynArray; external name 'data';
  Public
    Property resultId : string read FresultId;
    Property data : TUint32DynArray read Fdata;
    constructor New(data: TUint32DynArray; resultId: string); overload;
    constructor New(data: TUint32DynArray); overload;
  end;


  TVSSemanticTokensEdits  = class external name 'vscode.SemanticTokensEdits' (TJSObject)
  Private
    FresultId : string; external name 'resultId';
    Fedits : TVSSemanticTokensEditDynArray; external name 'edits';
  Public
    Property resultId : string read FresultId;
    Property edits : TVSSemanticTokensEditDynArray read Fedits;
    constructor New(edits: TVSSemanticTokensEditDynArray; resultId: string);
    constructor New(edits: TVSSemanticTokensEditDynArray);
  end;


  TVSSemanticTokensEdit  = class external name 'vscode.SemanticTokensEdit' (TJSObject)
  Private
    Fstart : NativeInt; external name 'start';
    FdeleteCount : NativeInt; external name 'deleteCount';
    Fdata : TUint32DynArray; external name 'data';
  Public
    Property start : NativeInt read Fstart;
    Property deleteCount : NativeInt read FdeleteCount;
    Property data : TUint32DynArray read Fdata;
    constructor New(start: NativeInt; deleteCount: NativeInt; data: TUint32DynArray);
  end;


  TVSDocumentSemanticTokensProvider  = class external name 'vscode.DocumentSemanticTokensProvider' (TJSObject)
  Public
    function onDidChangeSemanticTokens(aHandler : TVSvoidHandler) : TVSDisposable; overload;
    function onDidChangeSemanticTokens(aHandler : TVSvoidHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeSemanticTokens(aHandler : TVSvoidHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function provideDocumentSemanticTokens(document: TVSTextDocument; token: TVSCancellationToken) : JSValue;
    function provideDocumentSemanticTokensEdits(document: TVSTextDocument; previousResultId: string; token: TVSCancellationToken) : JSValue;
  end;


  TVSDocumentRangeSemanticTokensProvider  = class external name 'vscode.DocumentRangeSemanticTokensProvider' (TJSObject)
  Public
    function provideDocumentRangeSemanticTokens(document: TVSTextDocument; range: TVSRange; token: TVSCancellationToken) : JSValue;
  end;


  TVSFormattingOptions  = class external name 'vscode.FormattingOptions' (TJSObject)
  Public
    tabSize: NativeInt;
    insertSpaces: boolean;
  end;


  TVSDocumentFormattingEditProvider  = class external name 'vscode.DocumentFormattingEditProvider' (TJSObject)
  Public
    function provideDocumentFormattingEdits(document: TVSTextDocument; options: TVSFormattingOptions; token: TVSCancellationToken) : JSValue;
  end;


  TVSDocumentRangeFormattingEditProvider  = class external name 'vscode.DocumentRangeFormattingEditProvider' (TJSObject)
  Public
    function provideDocumentRangeFormattingEdits(document: TVSTextDocument; range: TVSRange; options: TVSFormattingOptions; token: TVSCancellationToken) : JSValue;
  end;


  TVSOnTypeFormattingEditProvider  = class external name 'vscode.OnTypeFormattingEditProvider' (TJSObject)
  Public
    function provideOnTypeFormattingEdits(document: TVSTextDocument; position: TVSPosition; ch: string; options: TVSFormattingOptions; token: TVSCancellationToken) : JSValue;
  end;


  TVSParameterInformation  = class external name 'vscode.ParameterInformation' (TJSObject)
  Public
    label_: string; external name 'label';
    labelArr : TNativeIntDynArray;
    documentation: string;
    documentationObj : TVSMarkdownString; external name 'documentation';
    constructor New(alabel: string; documentation: string); overload;
    constructor New(alabel: string; documentation: TVSMarkdownString); overload;
    constructor New(alabel: string); overload;
    constructor New(alabel: TNativeIntDynArray; documentation: string); overload;
    constructor New(alabel: TNativeIntDynArray; documentation: TVSMarkdownString); overload;
    constructor New(alabel: TNativeIntDynArray); overload;
  end;


  TVSSignatureInformation  = class external name 'vscode.SignatureInformation' (TJSObject)
  Public
    label_: string; external name 'label';
    documentation: string;
    documentationObj: TVSMarkdownString; external name 'documentation';
    parameters: TVSParameterInformationDynArray;
    activeParameter: NativeInt;
    constructor New(alabel: string; documentation: string); overload;
    constructor New(alabel: string; documentation: TVSMarkdownString); overload;
    constructor New(alabel: string); overload;
  end;


  TVSSignatureHelp  = class external name 'vscode.SignatureHelp' (TJSObject)
  Public
    signatures: TVSSignatureInformationDynArray;
    activeSignature: NativeInt;
    activeParameter: NativeInt;
  end;




  TVSSignatureHelpContext  = class external name 'vscode.SignatureHelpContext' (TJSObject)
  Private
    FtriggerKind : TVSSignatureHelpTriggerKind; external name 'triggerKind';
    FtriggerCharacter : string; external name 'triggerCharacter';
    FisRetrigger : boolean; external name 'isRetrigger';
    FactiveSignatureHelp : TVSSignatureHelp; external name 'activeSignatureHelp';
  Public
    Property triggerKind : TVSSignatureHelpTriggerKind read FtriggerKind;
    Property triggerCharacter : string read FtriggerCharacter;
    Property isRetrigger : boolean read FisRetrigger;
    Property activeSignatureHelp : TVSSignatureHelp read FactiveSignatureHelp;
  end;


  TVSSignatureHelpProvider  = class external name 'vscode.SignatureHelpProvider' (TJSObject)
  Public
    function provideSignatureHelp(document: TVSTextDocument; position: TVSPosition; token: TVSCancellationToken; context: TVSSignatureHelpContext) : JSValue;
  end;


  TVSSignatureHelpProviderMetadata  = class external name 'vscode.SignatureHelpProviderMetadata' (TJSObject)
  Private
    FtriggerCharacters : TStringDynArray; external name 'triggerCharacters';
    FretriggerCharacters : TStringDynArray; external name 'retriggerCharacters';
  Public
    Property triggerCharacters : TStringDynArray read FtriggerCharacters;
    Property retriggerCharacters : TStringDynArray read FretriggerCharacters;
  end;


  TVSCompletionItemRangeOptions = class external name 'Object' (TJSObject)
    inserting: TVSRange;
    replacing: TVSRange;
  end;

  TVSCompletionItem  = class external name 'vscode.CompletionItem' (TJSObject)
  Public
    label_: string; external name 'label';
    kind: TVSCompletionItemKind;
    tags: TVSCompletionItemTagDynArray;
    detail: string;
    documentation: string;
    documentationObj: TVSMarkdownString; external name 'documentation';
    sortText: string;
    filterText: string;
    preselect: boolean;
    insertText: string;
    insertTextObj :  TVSSnippetString; external name 'insertText';
    range: TVSRange;
    rangeObj : TVSCompletionItemRangeOptions; external name 'range';
    commitCharacters: TstringDynArray;
    keepWhitespace: boolean;
    textEdit: TVSTextEdit;
    additionalTextEdits: TVSTextEditDynArray;
    command: TVSCommand;
    constructor New(alabel: string); overload;
    constructor New(alabel: string; kind: TVSCompletionItemKind); overload;
  end;


  TVSCompletionList  = class external name 'vscode.CompletionList' (TJSObject)
  Public
    isIncomplete: boolean;
    items: TVSCompletionItemDynArray;
    constructor New(); overload;
    constructor New(items: TVSCompletionItemDynArray); overload;
    constructor New(items: TVSCOmpletionItemDynArray; isIncomplete: boolean); overload;
  end;




  TVSCompletionContext  = class external name 'vscode.CompletionContext' (TJSObject)
  Private
    FtriggerKind : TVSCompletionTriggerKind; external name 'triggerKind';
    FtriggerCharacter : string; external name 'triggerCharacter';
  Public
    Property triggerKind : TVSCompletionTriggerKind read FtriggerKind;
    Property triggerCharacter : string read FtriggerCharacter;
  end;


  TVSCompletionItemProvider  = class external name 'vscode.CompletionItemProvider' (TJSObject)
  Public
    function provideCompletionItems(document: TVSTextDocument; position: TVSPosition; token: TVSCancellationToken; context: TVSCompletionContext) : JSValue;
    function resolveCompletionItem(item: TVSCompletionItem; token: TVSCancellationToken) : JSValue;
  end;


  TVSDocumentLink  = class external name 'vscode.DocumentLink' (TJSObject)
  Public
    range: TVSRange;
    target: TVSUri;
    tooltip: string;
    constructor New(range: TVSRange); overload;
    constructor New(range: TVSRange; target: TVSUri); overload;
  end;


  TVSDocumentLinkProvider  = class external name 'vscode.DocumentLinkProvider' (TJSObject)
  Public
    function provideDocumentLinks(document: TVSTextDocument; token: TVSCancellationToken) : JSValue;
    function resolveDocumentLink(link: TVSDocumentLink; token: TVSCancellationToken) : JSValue;
  end;


  TVSColor  = class external name 'vscode.Color' (TJSObject)
  Private
    Fred : NativeInt; external name 'red';
    Fgreen : NativeInt; external name 'green';
    Fblue : NativeInt; external name 'blue';
    Falpha : NativeInt; external name 'alpha';
  Public
    Property red : NativeInt read Fred;
    Property green : NativeInt read Fgreen;
    Property blue : NativeInt read Fblue;
    Property alpha : NativeInt read Falpha;
    constructor New(red: NativeInt; green: NativeInt; blue: NativeInt; alpha: NativeInt);
  end;


  TVSColorInformation  = class external name 'vscode.ColorInformation' (TJSObject)
  Public
    range: TVSRange;
    color: TVSColor;
    constructor New(range: TVSRange; color: TVSColor);
  end;


  TVSColorPresentation  = class external name 'vscode.ColorPresentation' (TJSObject)
  Public
    label_: string; external name 'label';
    textEdit: TVSTextEdit;
    additionalTextEdits: TVSTextEditDynArray;
    constructor New(alabel: string);
  end;

  TVSDocumentColorProviderContext = class external name 'Object' (TJSObject)
    document: TVSTextDocument;
    range: TVSRange;
  end;

  TVSDocumentColorProvider  = class external name 'vscode.DocumentColorProvider' (TJSObject)
  Public
    function provideDocumentColors(document: TVSTextDocument; token: TVSCancellationToken) : JSValue;
    function provideColorPresentations(color: TVSColor; context: TVSDocumentColorProviderContext; token: TVSCancellationToken): JSValue;
  end;


  TVSFoldingRange  = class external name 'vscode.FoldingRange' (TJSObject)
  Public
    start: NativeInt;
    end_: NativeInt; external name 'end';
    kind: TVSFoldingRangeKind;
    constructor New(start: NativeInt; aEnd: NativeInt); overload;
    constructor New(start: NativeInt; aEnd: NativeInt; kind: TVSFoldingRangeKind); overload;
  end;




  TVSFoldingContext  = class external name 'vscode.FoldingContext' (TJSObject)
  Public
  end;


  TVSFoldingRangeProvider  = class external name 'vscode.FoldingRangeProvider' (TJSObject)
  Public
    function onDidChangeFoldingRanges(aHandler : TVSvoidHandler) : TVSDisposable; overload;
    function onDidChangeFoldingRanges(aHandler : TVSvoidHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeFoldingRanges(aHandler : TVSvoidHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function provideFoldingRanges(document: TVSTextDocument; context: TVSFoldingContext; token: TVSCancellationToken) : JSValue;
  end;


  TVSSelectionRange  = class external name 'vscode.SelectionRange' (TJSObject)
  Public
    range: TVSRange;
    parent: TVSSelectionRange;
    constructor New(range: TVSRange);
    constructor New(range: TVSRange; parent: TVSSelectionRange);
  end;


  TVSSelectionRangeProvider  = class external name 'vscode.SelectionRangeProvider' (TJSObject)
  Public
    function provideSelectionRanges(document: TVSTextDocument; positions: TVSPositionDynArray; token: TVSCancellationToken) :  JSValue;
  end;


  TVSCallHierarchyItem  = class external name 'vscode.CallHierarchyItem' (TJSObject)
  Public
    name: string;
    kind: TVSSymbolKind;
    tags: TVSSymbolTagDynArray;
    detail: string;
    uri: TVSUri;
    range: TVSRange;
    selectionRange: TVSRange;
    constructor New(kind: TVSSymbolKind; name: string; detail: string; uri: TVSUri; range: TVSRange; selectionRange: TVSRange);
  end;


  TVSCallHierarchyIncomingCall  = class external name 'vscode.CallHierarchyIncomingCall' (TJSObject)
  Public
    from: TVSCallHierarchyItem;
    fromRanges: TVSRangeDynArray;
    constructor New(item: TVSCallHierarchyItem; fromRanges: TVSRangeDynArray);
  end;


  TVSCallHierarchyOutgoingCall  = class external name 'vscode.CallHierarchyOutgoingCall' (TJSObject)
  Public
    to_: TVSCallHierarchyItem; external name 'to';
    fromRanges: TVSRangeDynArray;
    constructor New(item: TVSCallHierarchyItem; fromRanges: TVSRangeDynArray);
  end;


  TVSCallHierarchyProvider  = class external name 'vscode.CallHierarchyProvider' (TJSObject)
  Public
    function prepareCallHierarchy(document: TVSTextDocument; position: TVSPosition; token: TVSCancellationToken) : JSValue;
    function provideCallHierarchyIncomingCalls(item: TVSCallHierarchyItem; token: TVSCancellationToken) : JSValue;
    function provideCallHierarchyOutgoingCalls(item: TVSCallHierarchyItem; token: TVSCancellationToken) : JSValue;
  end;


  TVSLinkedEditingRanges  = class external name 'vscode.LinkedEditingRanges' (TJSObject)
  Private
    Franges : TVSRangeDynArray; external name 'ranges';
    FwordPattern : TJSRegExp; external name 'wordPattern';
  Public
    constructor New(ranges: TVSRangeDynArray); overload;
    constructor New(ranges: TVSRangeDynArray; wordPattern: TJSRegExp); overload;
    Property ranges : TVSRangeDynArray read Franges;
    Property wordPattern : TJSRegExp read FwordPattern;
  end;


  TVSLinkedEditingRangeProvider  = class external name 'vscode.LinkedEditingRangeProvider' (TJSObject)
  Public
    function provideLinkedEditingRanges(document: TVSTextDocument; position: TVSPosition; token: TVSCancellationToken) : JSValue;
  end;


  TVSCharacterPair = TStringDynArray;
  TVSCommentRule  = class external name 'vscode.CommentRule' (TJSObject)
  Public
    lineComment: string;
    blockComment: TVSCharacterPair;
  end;


  TVSIndentationRule  = class external name 'vscode.IndentationRule' (TJSObject)
  Public
    decreaseIndentPattern: TJSRegexp;
    increaseIndentPattern: TJSRegexp;
    indentNextLinePattern: TJSRegexp;
    unIndentedLinePattern: TJSRegexp;
  end;




  TVSEnterAction  = class external name 'vscode.EnterAction' (TJSObject)
  Public
    indentAction: TVSIndentAction;
    appendText: string;
    removeText: NativeInt;
  end;


  TVSOnEnterRule  = class external name 'vscode.OnEnterRule' (TJSObject)
  Public
    beforeText: TJSRegexp;
    afterText: TJSRegexp;
    action: TVSEnterAction;
  end;

  TVSLanguageConfigurationCharSupportComment = class external name 'Object' (TJSObject)
    scope: string;
    open: string;
    lineStart: string;
    close: string;
  end;

  TVSLanguageConfigurationCharSupport = class external name 'Object' (TJSObject)
    brackets: JSValue;
    docComment: TVSLanguageConfigurationCharSupportComment;
  end;

  TVSLanguageConfigurationCharPairSupportAutoClosingPairs =class external name 'Object' (TJSObject)
    open: string;
    close: string;
    notIn: TstringDynArray;
  end;

  TVSLanguageConfigurationCharPairSupport = class external name 'Object' (TJSObject)
    autoClosingPairs :TVSLanguageConfigurationCharPairSupportAutoClosingPairs;
  end;

  TVSLanguageConfiguration  = class external name 'vscode.LanguageConfiguration' (TJSObject)
  Public
    comments: TVSCommentRule;
    brackets: TVSCharacterPairDynArray;
    wordPattern: TJSRegexp;
    indentationRules: TVSIndentationRule;
    onEnterRules: TVSOnEnterRuleDynArray;
    __electricCharacterSupport: TVSLanguageConfigurationCharSupport;
    __characterPairSupport: TVSLanguageConfigurationCharPairSupport;
  end;



  TVSWorkspaceConfigurationInspect = class external name 'Object' (TJSObject)
    key: string;
    defaultValue: JSValue;
    globalValue: JSValue;
    workspaceValue: JSValue;
    TVSWorkspaceFolderValue: JSValue;
    defaultLanguageValue: JSValue;
    globalLanguageValue: JSValue;
    workspaceLanguageValue: JSValue;
    TVSWorkspaceFolderLanguageValue: JSValue;
    languageIds: TstringDynArray;
  end;

  TVSWorkspaceConfiguration  = class external name 'vscode.TVSWorkspaceConfiguration' (TJSObject)
  Public
    function get(section: string) : JSValue; overload;
    function get(section: string; defaultValue: JSvalue) : JSValue;overload;
    function has(section: string) : boolean;
    function inspect(section: string) : TVSWorkspaceConfigurationInspect;
    function update(section: string; value: JSValue) : TVSThenable; overload; // void
    function update(section: string; value: JSValue; configurationTarget: TVSConfigurationTarget) : TVSThenable overload; // void
    function update(section: string; value: JSValue; configurationTarget: TVSConfigurationTarget; overrideInLanguage: boolean) : TVSThenable; overload; // void
    function update(section: string; value: JSValue; configurationTarget: boolean) : TVSThenable;  overload; // void
    function update(section: string; value: JSValue; configurationTarget: boolean; overrideInLanguage: boolean) : TVSThenable; overload; // void
  end;


  TVSLocation  = class external name 'vscode.Location' (TJSObject)
  Public
    uri: TVSUri;
    range: TVSRange;
    constructor New(uri: TVSUri; rangeOrPosition: TVSRange); overload;
    constructor New(uri: TVSUri; rangeOrPosition: TVSPosition); overload;
  end;


  TVSLocationLink  = class external name 'vscode.LocationLink' (TJSObject)
  Public
    originSelectionRange: TVSRange;
    targetUri: TVSUri;
    targetRange: TVSRange;
    targetSelectionRange: TVSRange;
  end;


  TVSDiagnosticChangeEvent  = class external name 'vscode.DiagnosticChangeEvent' (TJSObject)
  Private
    Furis : TVSUriDynArray; external name 'uris';
  Public
    Property uris : TVSUriDynArray read Furis;
  end;




  TVSDiagnosticRelatedInformation  = class external name 'vscode.DiagnosticRelatedInformation' (TJSObject)
  Public
    location: TVSLocation;
    message: string;
    constructor New(location: TVSLocation; message: string);
  end;



  TVSDiagnosticCode = class external name 'Object' (TJSObject)
    value: string;
    valueInt :  NativeInt; external name 'value';
    target: TVSUri;
  end;

  TVSDiagnostic  = class external name 'vscode.Diagnostic' (TJSObject)
  Public
    range: TVSRange;
    message: string;
    severity: TVSDiagnosticSeverity;
    source: string;
    code: string;
    codeInt : NativeInt;
    codeObj : TVSDiagnosticCode;
    relatedInformation: TVSDiagnosticRelatedInformationDynArray;
    tags: TVSDiagnosticTagDynArray;
    constructor New(range: TVSRange; message: string); overload;
    constructor New(range: TVSRange; message: string; severity: TVSDiagnosticSeverity); overload;
  end;

  TVSDiagnosticCollectionCallback = reference to function (uri: TVSUri; diagnostics: TVSDiagnosticDynArray; collection: TVSDiagnosticCollection) : JSValue;

  TVSDiagnosticCollection  = class external name 'vscode.DiagnosticCollection' (TJSObject)
  Private
    Fname : string; external name 'name';
  Public
    Property name : string read Fname;
    procedure set_(uri: TVSUri; aValue: JSValue) ; external name 'set';
    procedure delete(uri: TVSUri) ;
    procedure clear() ;
    procedure forEach(callback: TVSDiagnosticCollectionCallback) ; overload;
    procedure forEach(callback: TVSDiagnosticCollectionCallback; thisArg: JSValue) ; overload;
    function get(uri: TVSUri) : TVSDiagnosticDynArray;
    function has(uri: TVSUri) : boolean;
    procedure dispose() ;
  end;




  TVSOutputChannel  = class external name 'vscode.OutputChannel' (TJSObject)
  Private
    Fname : string; external name 'name';
  Public
    Property name : string read Fname;
    procedure append(value: string) ;
    procedure appendLine(value: string) ;
    procedure clear() ;
    procedure show(preserveFocus: boolean) ; overload;
    procedure show(column: TVSViewColumn) ; overload;
    procedure show(column: TVSViewColumn; preserveFocus: boolean) ; overload;
    procedure hide() ;
    procedure dispose() ;
  end;


  TVSAccessibilityInformation  = class external name 'vscode.AccessibilityInformation' (TJSObject)
  Public
    label_: string; external name 'label';
    role: string;
  end;




  TVSStatusBarItem  = class external name 'vscode.TVSStatusBarItem' (TJSObject)
  Private
    Falignment : TVSStatusBarAlignment; external name 'alignment';
    Fpriority : NativeInt; external name 'priority';
  Public
    text: string;
    tooltip: string;
    color: string;
    colorObj: TVSThemeColor; external name 'color';
    backgroundColor: TVSThemeColor;
    command: string;
    commandObj : TVSCommand ; external name 'command';
    accessibilityInformation: TVSAccessibilityInformation;
    procedure show() ;
    procedure hide() ;
    procedure dispose() ;
    Property alignment : TVSStatusBarAlignment read Falignment;
    Property priority : NativeInt read Fpriority;
  end;


  TVSProgress  = class external name 'vscode.Progress' (TJSObject)
  Public
    procedure report(value: JSValue) ;
  end;


  TVSTerminal  = class external name 'vscode.TVSTerminal' (TJSObject)
  Private
    Fname : string; external name 'name';
    FprocessId : TJSPromise; external name 'processId'; // NativeInt | undefined
    FcreationOptions : TVSTerminalOptions; external name 'creationOptions';
    FcreationOptionsEx: TVSExtensionTerminalOptions; external name 'creationOptions';
    FexitStatus : TVSTerminalExitStatus; external name 'exitStatus';
  Public
    Property name : string read Fname;
    Property processId : TJSPromise read FprocessId;
    Property creationOptions : TVSTerminalOptions read FcreationOptions;
    Property creationOptionsEx : TVSExtensionTerminalOptions read FcreationOptionsEx;
    Property exitStatus : TVSTerminalExitStatus read FexitStatus;
    procedure sendText(text: string) ; overload;
    procedure sendText(text: string; addNewLine: boolean) ; overload;
    procedure show() ; overload;
    procedure show(preserveFocus: boolean) ; overload;
    procedure hide() ;
    procedure dispose() ;
  end;


  TVSTerminalLinkContext  = class external name 'vscode.TerminalLinkContext' (TJSObject)
  Public
    line: string;
    terminal: TVSTerminal;
  end;


  TVSTerminalLinkProvider  = class external name 'vscode.TerminalLinkProvider' (TJSObject)
  Public
    function provideTerminalLinks(context: TVSTerminalLinkContext; token: TVSCancellationToken) : JSValue;
    function handleTerminalLink(link: TVSTerminalLink) : JSValue;
  end;


  TVSTerminalLink  = class external name 'vscode.TerminalLink' (TJSObject)
  Public
    startIndex: NativeInt;
    length: NativeInt;
    tooltip: string;
  end;


  TVSFileDecoration  = class external name 'vscode.FileDecoration' (TJSObject)
  Public
    badge: string;
    tooltip: string;
    color: TVSThemeColor;
    propagate: boolean;
    constructor New(); overload;
    constructor New(badge: string); overload;
    constructor New(badge: string; tooltip: string); overload;
    constructor New(badge: string; tooltip: string; color: TVSThemeColor); overload;
  end;

  TVSUriDynArrayHandler = Reference to function(aEvent :TVSUriDynArray) : JSValue;

  TVSFileDecorationProvider  = class external name 'vscode.FileDecorationProvider' (TJSObject)
  Public
    Function onDidChangeFileDecorations (aHandler : TVSUriHandler) : TVSDisposable;
    Function onDidChangeFileDecorations (aHandler : TVSUriDynArrayHandler) : TVSDisposable;
    function provideFileDecoration(uri: TVSUri; token: TVSCancellationToken) : JSValue;
  end;



  TVSExtension = class external name 'vscode.Extension' (TJSObject)
  Private
    Fid : string; external name 'id';
    FextensionUri : TVSUri; external name 'extensionUri';
    FextensionPath : string; external name 'extensionPath';
    FisActive : boolean; external name 'isActive';
    FpackageJSON : JSValue; external name 'packageJSON';
    Fexports : JSValue; external name 'exports';
  Public
    extensionKind: TVSExtensionKind;
    function activate() : TVSThenable; // T
    Property id : string read Fid;
    Property extensionUri : TVSUri read FextensionUri;
    Property extensionPath : string read FextensionPath;
    Property isActive : boolean read FisActive;
    Property packageJSON : JSValue read FpackageJSON;
    Property exports_ : JSValue read Fexports;
  end;




  TVSExtensionContext  = class external name 'vscode.ExtensionContext' (TJSObject)
  Public
    FworkspaceState : TVSMemento; external name 'workspaceState';
  Private
    FextensionUri : TVSUri; external name 'extensionUri';
    FextensionPath : string; external name 'extensionPath';
    FenvironmentVariableCollection : TVSEnvironmentVariableCollection; external name 'environmentVariableCollection';
    FstorageUri : TVSUri ;external name 'storageUri';
    FstoragePath : string ;external name 'storagePath';
    FglobalStorageUri : TVSUri; external name 'globalStorageUri';
    FglobalStoragePath : string; external name 'globalStoragePath';
    FlogUri : TVSUri; external name 'logUri';
    FlogPath : string; external name 'logPath';
    FextensionMode : TVSExtensionMode; external name 'extensionMode';
  Public
    subscriptions : TVSDisposableArray;
    globalState: TVSMemento;
    Property workspaceState : TVSMemento read FworkspaceState;
    Property extensionUri : TVSUri read FextensionUri;
    Property extensionPath : string read FextensionPath;
    Property environmentVariableCollection : TVSEnvironmentVariableCollection read FenvironmentVariableCollection;
    function asAbsolutePath(relativePath: string) : string;
    Property storageUri : TVSUri read FstorageUri;
    Property storagePath : string read FstoragePath;
    Property globalStorageUri : TVSUri read FglobalStorageUri;
    Property globalStoragePath : string read FglobalStoragePath;
    Property logUri : TVSUri read FlogUri;
    Property logPath : string read FlogPath;
    Property extensionMode : TVSExtensionMode read FextensionMode;
  end;


  TVSMemento  = class external name 'vscode.Memento' (TJSObject)
  Public
    function get(key: string) : JSValue; overload;
    function get(key: string; defaultValue: JSValue) : JSValue; overload;
    function update(key: string; value: JSValue) : TVSThenable; // void
  end;




  TVSColorTheme  = class external name 'vscode.ColorTheme' (TJSObject)
  Private
    Fkind : TVSColorThemeKind; external name 'kind';
  Public
    Property kind : TVSColorThemeKind read Fkind;
  end;



  TVSTaskPresentationOptions  = class external name 'vscode.TaskPresentationOptions' (TJSObject)
  Public
    reveal: TVSTaskRevealKind;
    echo: boolean;
    focus: boolean;
    panel: TVSTaskPanelKind;
    showReuseMessage: boolean;
    clear: boolean;
  end;


  TVSTaskGroup  = class external name 'vscode.TaskGroup' (TJSObject)
  Public
    class var Clean: TVSTaskGroup;
    class var Build: TVSTaskGroup;
    class var Rebuild: TVSTaskGroup;
    class var Test: TVSTaskGroup;
    constructor New(id: string; alabel: string);
  end;


  TVSTaskDefinition  = class external name 'vscode.TaskDefinition' (TJSObject)
  Private
    Ftype : string; external name 'type';
  Public
    Property type_ : string read Ftype;
  end;


  TVSProcessExecutionOptions  = class external name 'vscode.ProcessExecutionOptions' (TJSObject)
  Public
    cwd: string;
    env: TJSObject;
  end;


  TVSProcessExecution  = class external name 'vscode.ProcessExecution' (TJSObject)
  Public
    process: string;
    args: TStringDynArray;
    options: TVSProcessExecutionOptions;
    constructor New(process: string); overload;
    constructor New(process: string; options: TVSProcessExecutionOptions); overload;
    constructor New(process: string; args: TstringDynArray); overload;
    constructor New(process: string; args: TstringDynArray; options: TVSProcessExecutionOptions); overload;
  end;

  TVSShellQuotingOptionsEscape = class external name 'Object' (TJSObject)
    escapeChar: string;
    charsToEscape: string;
  end;

  TVSShellQuotingOptions  = class external name 'vscode.ShellQuotingOptions' (TJSObject)
  Public
    escape: string;
    escapeObj : TVSShellQuotingOptionsEscape;
    strong: string;
    weak: string;
  end;


  TVSShellExecutionOptions  = class external name 'vscode.ShellExecutionOptions' (TJSObject)
  Public
    executable: string;
    shellArgs: TstringDynArray;
    shellQuoting: TVSShellQuotingOptions;
    cwd: string;
    env: TJSObject;
  end;




  TVSShellQuotedString  = class external name 'vscode.ShellQuotedString' (TJSObject)
  Public
    value: string;
    quoting: TVSShellQuoting;
  end;


  TVSShellExecution  = class external name 'vscode.ShellExecution' (TJSObject)
  Public
    constructor New(commandLine: string); overload;
    constructor New(commandLine: string; options: TVSShellExecutionOptions); overload;
    constructor New(command: TVSShellQuotedString; args: TVSShellQuotedStringDynArray); overload;
    constructor New(command: TVSShellQuotedString; args: TStringDynArray); overload;
    constructor New(command: string; args: TStringDynArray); overload;
    constructor New(command: string; args: TVSShellQuotedStringDynArray); overload;

    constructor New(command: TVSShellQuotedString; args: TVSShellQuotedStringDynArray; options: TVSShellExecutionOptions); overload;
    constructor New(command: TVSShellQuotedString; args: TStringDynArray; options: TVSShellExecutionOptions); overload;
    constructor New(command: string; args: TStringDynArray; options: TVSShellExecutionOptions); overload;
    constructor New(command: string; args: TVSShellQuotedStringDynArray; options: TVSShellExecutionOptions); overload;
    commandLine: string ;
    command: string;
    commandObj : TVSShellQuotedString;
    args: TStringDynArray;
    argObjs : TVSShellQuotedStringDynArray; external name 'args';
    options: TVSShellExecutionOptions;
  end;

  TVSCustomExecutionCallback = reference to function (resolvedDefinition: TVSTaskDefinition) : TVSThenable; // TVSPseudoterminal

  TVSCustomExecution  = class external name 'vscode.CustomExecution' (TJSObject)
  Public
    constructor New(callback: TVSCustomExecutionCallback);
  end;



  TVSRunOptions  = class external name 'vscode.RunOptions' (TJSObject)
  Public
    reevaluateOnRerun: boolean;
  end;


  TVSTask  = class external name 'vscode.TVSTask' (TJSObject)
  Private
    Fscope : TVSTaskScope; external name 'scope';
  Public
    definition: TVSTaskDefinition;
    name: string;
    detail: string;
    execution: TJSObject;
    isBackground: boolean;
    source: string;
    group: TVSTaskGroup;
    presentationOptions: TVSTaskPresentationOptions;
    problemMatchers: TstringDynArray;
    runOptions: TVSRunOptions;
    constructor New(taskDefinition: TVSTaskDefinition; scope: TVSTaskScope; name: string; source: string); overload;
    constructor New(taskDefinition: TVSTaskDefinition; scope: TVSTaskScope; name: string; source: string; execution: TVSProcessExecution); overload;
    constructor New(taskDefinition: TVSTaskDefinition; scope: TVSTaskScope; name: string; source: string; execution: TVSShellExecution); overload;
    constructor New(taskDefinition: TVSTaskDefinition; scope: TVSTaskScope; name: string; source: string; execution: TVSCustomExecution); overload;
    constructor New(taskDefinition: TVSTaskDefinition; scope: TVSTaskScope; name: string; source: string; execution: TVSProcessExecution ; problemMatchers: string ); overload;
    constructor New(taskDefinition: TVSTaskDefinition; scope: TVSTaskScope; name: string; source: string; execution: TVSShellExecution ; problemMatchers: string ); overload;
    constructor New(taskDefinition: TVSTaskDefinition; scope: TVSTaskScope; name: string; source: string; execution: TVSCustomExecution ; problemMatchers: string ); overload;
    constructor New(taskDefinition: TVSTaskDefinition; scope: TVSTaskScope; name: string; source: string; execution: TVSProcessExecution ; problemMatchers: TstringDynArray); overload;
    constructor New(taskDefinition: TVSTaskDefinition; scope: TVSTaskScope; name: string; source: string; execution: TVSShellExecution ; problemMatchers: TstringDynArray); overload;
    constructor New(taskDefinition: TVSTaskDefinition; scope: TVSTaskScope; name: string; source: string; execution: TVSCustomExecution ; problemMatchers: TstringDynArray); overload;
    constructor New(taskDefinition: TVSTaskDefinition; name: string; source: string); overload;
    constructor New(taskDefinition: TVSTaskDefinition; name: string; source: string; execution: TVSProcessExecution); overload;
    constructor New(taskDefinition: TVSTaskDefinition; name: string; source: string; execution: TVSShellExecution); overload;
    constructor New(taskDefinition: TVSTaskDefinition; name: string; source: string; execution: TVSProcessExecution; problemMatchers: string); overload;
    constructor New(taskDefinition: TVSTaskDefinition; name: string; source: string; execution: TVSShellExecution; problemMatchers: string); overload;
    constructor New(taskDefinition: TVSTaskDefinition; name: string; source: string; execution: TVSProcessExecution; problemMatchers: tstringDynArray); overload;
    constructor New(taskDefinition: TVSTaskDefinition; name: string; source: string; execution: TVSShellExecution; problemMatchers: tstringDynArray); overload;
    Property scope : TVSTaskScope read Fscope;
  end;


  TVSTaskProvider = class external name 'vscode.TVSTaskProvider' (TJSObject)
  Public
    function provideTasks(token: TVSCancellationToken) : JSValue;
    function resolveTask(task: TVSTask; token: TVSCancellationToken) : JSValue;
  end;


  TVSTaskExecution  = class external name 'vscode.TVSTaskExecution' (TJSObject)
  Public
    task: TVSTask;
    procedure terminate() ;
  end;


  TVSTaskStartEvent  = class external name 'Object' (TJSObject)
  Private
    Fexecution : TVSTaskExecution; external name 'execution';
  Public
    Property execution : TVSTaskExecution read Fexecution;
  end;


  TVSTaskEndEvent  = class external name 'Object' (TJSObject)
  Private
    Fexecution : TVSTaskExecution; external name 'execution';
  Public
    Property execution : TVSTaskExecution read Fexecution;
  end;


  TVSTaskProcessStartEvent  = class external name 'vscode.TaskProcessStartEvent' (TJSObject)
  Private
    Fexecution : TVSTaskExecution; external name 'execution';
    FprocessId : NativeInt; external name 'processId';
  Public
    Property execution : TVSTaskExecution read Fexecution;
    Property processId : NativeInt read FprocessId;
  end;


  TVSTaskProcessEndEvent  = class external name 'vscode.TaskProcessEndEvent' (TJSObject)
  Private
    Fexecution : TVSTaskExecution; external name 'execution';
    FexitCode : NativeInt ;external name 'exitCode';
  Public
    Property execution : TVSTaskExecution read Fexecution;
    Property exitCode : NativeInt read FexitCode;
  end;


  TVSTaskFilter  = class external name 'vscode.TaskFilter' (TJSObject)
  Public
    version: string;
    type_: string; external name 'type';
  end;


  TVSNStasks  = class external name 'Object' (TJSObject)
  Public
    taskExecutions: TVSTaskExecutionDynArray;
    function registerTaskProvider(type_: string; provider: TVSTaskProvider) : TVSDisposable;
    function fetchTasks() : TVSThenable; // TaskDynArray overload;
    function fetchTasks(filter: TVSTaskFilter) : TVSThenable; // TaskDynArray overload;
    function executeTask(task: TVSTask) : TVSThenable; // TVSTaskExecution
    function onDidStartTask(aHandler : TVSTaskStartEventHandler) : TVSDisposable; overload;
    function onDidStartTask(aHandler : TVSTaskStartEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidStartTask(aHandler : TVSTaskStartEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidEndTask(aHandler : TVSTaskEndEventHandler) : TVSDisposable; overload;
    function onDidEndTask(aHandler : TVSTaskEndEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidEndTask(aHandler : TVSTaskEndEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidStartTaskProcess(aHandler : TVSTaskProcessStartEventHandler) : TVSDisposable; overload;
    function onDidStartTaskProcess(aHandler : TVSTaskProcessStartEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidStartTaskProcess(aHandler : TVSTaskProcessStartEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidEndTaskProcess(aHandler : TVSTaskProcessEndEventHandler) : TVSDisposable; overload;
    function onDidEndTaskProcess(aHandler : TVSTaskProcessEndEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidEndTaskProcess(aHandler : TVSTaskProcessEndEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
  end;

  TVSFileStat = class external name 'vscode.FileStat' (TJSObject)
  Public
    type_: TVSFileType; external name 'type';
    ctime: NativeInt;
    mtime: NativeInt;
    size: NativeInt;
  end;


  TVSFileSystemError  = class external name 'vscode.TVSFileSystemError' (TJSError)
  Private
    Fcode : string; external name 'code';
  Public
    class function FileNotFound() : TVSFileSystemError; overload;
    class function FileNotFound(messageOrUri: string ) : TVSFileSystemError; overload;
    class function FileNotFound(messageOrUri: TVSUri) : TVSFileSystemError; overload;
    class function FileExists() : TVSFileSystemError; overload;
    class function FileExists(messageOrUri: string) : TVSFileSystemError; overload;
    class function FileExists(messageOrUri: TVSUri) : TVSFileSystemError; overload;
    class function FileNotADirectory() : TVSFileSystemError; overload;
    class function FileNotADirectory(messageOrUri: string) : TVSFileSystemError; overload;
    class function FileNotADirectory(messageOrUri: TVSUri) : TVSFileSystemError; overload;
    class function FileIsADirectory() : TVSFileSystemError; overload;
    class function FileIsADirectory(messageOrUri: string ) : TVSFileSystemError; overload;
    class function FileIsADirectory(messageOrUri: TVSUri) : TVSFileSystemError; overload;
    class function NoPermissions() : TVSFileSystemError; overload;
    class function NoPermissions(messageOrUri: string) : TVSFileSystemError; overload;
    class function NoPermissions(messageOrUri:  TVSUri) : TVSFileSystemError; overload;
    class function Unavailable() : TVSFileSystemError; overload;
    class function Unavailable(messageOrUri: string) : TVSFileSystemError; overload;
    class function Unavailable(messageOrUri: TVSUri) : TVSFileSystemError; overload;
    constructor New(); overload;
    constructor New(messageOrUri: string); overload;
    constructor New(messageOrUri: TVSUri); overload;
    Property code : string read Fcode;
  end;



  TVSFileChangeEvent  = class external name 'vscode.FileChangeEvent' (TJSObject)
  Private
    Ftype : TVSFileChangeType; external name 'type';
    Furi : TVSUri; external name 'uri';
  Public
    Property type_ : TVSFileChangeType read Ftype;
    Property uri : TVSUri read Furi;
  end;

  TVSFileChangeEventDynArrayHandler = reference to function(aEvent : TVSFileChangeEventDynArray) : JSValue;

  TVSFileSystemWatchOptions = class external name 'Object' (TJSObject)
    recursive: boolean;
    excludes: TstringDynArray ;
  end;

  TVSFileSystemWriteOptions = class external name 'Object' (TJSObject)
    create_: boolean; external name 'create';
    overwrite: boolean;
  end;

  TVSFileSystemDeleteOptions = class external name 'Object' (TJSObject)
    recursive: boolean;
    useTrash : boolean;
  end;

  TVSFileSystemRenameOptions = class external name 'Object' (TJSObject)
    overwrite: boolean;
  end;

  TVSFileSystemProvider  = class external name 'vscode.TVSFileSystemProvider' (TJSObject)
  Public
    Function onDidChangeFile (aCallback : TVSFileChangeEventDynArrayHandler) : TVSDisposable;
    function watch(uri: TVSUri; options: TVSFileSystemWatchOptions): TVSDisposable;
    function stat(uri: TVSUri) : TVSThenable; // FileStat
    function readDirectory(uri: TVSUri) : TVSThenable; // [string; FileType][]
    function createDirectory(uri: TVSUri) : TVSThenable; // void
    function readFile(uri: TVSUri) :  TVSThenable; // TJSUint8Array
    function writeFile(uri: TVSUri; content: TJSUint8Array; options: TVSFileSystemWriteOptions ): TVSThenable; // void
    function delete(uri: TVSUri; options: TVSFileSystemDeleteOptions): TVSThenable; // void
    function rename(oldUri: TVSUri; newUri: TVSUri; options: TVSFileSystemRenameOptions ) :TVSThenable; // void
    function copy(source: TVSUri; destination: TVSUri; options: TVSFileSystemRenameOptions ):  TVSThenable; // void
  end;


  TVSFileSystem  = class external name 'vscode.FileSystem' (TJSObject)
  Public
    function stat(uri: TVSUri) : TVSThenable; // FileStat
    function readDirectory(uri: TVSUri) : TVSThenable; // [string; FileType]DynArray
    function createDirectory(uri: TVSUri) : TVSThenable; // void
    function readFile(uri: TVSUri) : TVSThenable; // TJSUint8Array
    function writeFile(uri: TVSUri; content: TJSUint8Array) : TVSThenable; // void
    function delete(uri: TVSUri): TVSThenable; // void overload;
    function delete(uri: TVSUri; options: TVSFileSystemRenameOptions): TVSThenable; // void overload;
    function rename(source: TVSUri; target: TVSUri): TVSThenable; // void overload;
    function rename(source: TVSUri; target: TVSUri; options: TVSFileSystemRenameOptions): TVSThenable; // void overload;
    function copy(source: TVSUri; target: TVSUri): TVSThenable; // void overload;
    function copy(source: TVSUri; target: TVSUri; options: TVSFileSystemRenameOptions): TVSThenable; // void overload;
    function isWritableFileSystem(scheme: string) : boolean ;
  end;


  TVSWebviewPortMapping  = class external name 'vscode.WebviewPortMapping' (TJSObject)
  Private
    FwebviewPort : NativeInt; external name 'webviewPort';
    FextensionHostPort : NativeInt; external name 'extensionHostPort';
  Public
    Property webviewPort : NativeInt read FwebviewPort;
    Property extensionHostPort : NativeInt read FextensionHostPort;
  end;


  TVSWebviewOptions  = class external name 'vscode.WebviewOptions' (TJSObject)
  Private
    FenableScripts : boolean; external name 'enableScripts';
    FenableCommandUris : boolean; external name 'enableCommandUris';
    FlocalResourceRoots : TVSUriDynArray; external name 'localResourceRoots';
    FportMapping : TVSWebviewPortMappingDynArray; external name 'portMapping';
  Public
    Property enableScripts : boolean read FenableScripts;
    Property enableCommandUris : boolean read FenableCommandUris;
    Property localResourceRoots : TVSUriDynArray read FlocalResourceRoots;
    Property portMapping : TVSWebviewPortMappingDynArray read FportMapping;
  end;


  TVSWebview  = class external name 'vscode.Webview' (TJSObject)
  Private
    FcspSource : string; external name 'cspSource';
  Public
    options: TVSWebviewOptions;
    html: string;
    function onDidReceiveMessage(aHandler : TVSanyHandler) : TVSDisposable; overload;
    function onDidReceiveMessage(aHandler : TVSanyHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidReceiveMessage(aHandler : TVSanyHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function postMessage(message: JSValue) : TVSThenable; // boolean
    function asWebviewUri(localResource: TVSUri) : TVSUri;
    Property cspSource : string read FcspSource;
  end;


  TVSWebviewPanelOptions  = class external name 'vscode.WebviewPanelOptions' (TJSObject)
  Private
    FenableFindWidget : boolean; external name 'enableFindWidget';
    FretainContextWhenHidden : boolean; external name 'retainContextWhenHidden';
  Public
    Property enableFindWidget : boolean read FenableFindWidget;
    Property retainContextWhenHidden : boolean read FretainContextWhenHidden;
  end;


  TVSWebviewPanel  = class external name 'Object' (TJSObject)
  Private
    FviewType : string; external name 'viewType';
    Fwebview : TVSWebview; external name 'webview';
    Foptions : TVSWebviewPanelOptions; external name 'options';
    FviewColumn : TVSViewColumn; external name 'viewColumn';
    Factive : boolean; external name 'active';
    Fvisible : boolean; external name 'visible';
  Public
    title: string;
    iconPath: TVSUri;
    iconPathObj : TVSIconPathObject; external name 'iconPath';
    function onDidChangeViewState (aHandler : TVSWebviewPanelOnDidChangeViewStateEventHandler) : TVSDisposable; overload;
    function onDidChangeViewState(aHandler : TVSWebviewPanelOnDidChangeViewStateEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeViewState(aHandler : TVSWebviewPanelOnDidChangeViewStateEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidDispose(aHandler : TVSvoidHandler) : TVSDisposable; overload;
    function onDidDispose(aHandler : TVSvoidHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidDispose(aHandler : TVSvoidHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    procedure reveal() ; overload;
    procedure reveal(viewColumn: TVSViewColumn) ; overload;
    procedure reveal(viewColumn: TVSViewColumn; preserveFocus: boolean) ; overload;
    function dispose() : JSValue;
    Property viewType : string read FviewType;
    Property webview : TVSWebview read Fwebview;
    Property options : TVSWebviewPanelOptions read Foptions;
    Property viewColumn : TVSViewColumn read FviewColumn;
    Property active : boolean read Factive;
    Property visible : boolean read Fvisible;
  end;


  TVSWebviewPanelOnDidChangeViewStateEvent  = class external name 'vscode.WebviewPanelOnDidChangeViewStateEvent' (TJSObject)
  Private
    FwebviewPanel : TVSWebviewPanel; external name 'webviewPanel';
  Public
    Property webviewPanel : TVSWebviewPanel read FwebviewPanel;
  end;


  TVSWebviewPanelSerializer = class external name 'Object' (TJSObject)
  Public
    function deserializeWebviewPanel(webviewPanel: TVSWebviewPanel; state: TJSObject) : TVSThenable; // void
  end;


  TVSWebviewView  = class external name 'vscode.WebviewView' (TJSObject)
  Private
    FviewType : string; external name 'viewType';
    Fwebview : TVSWebview; external name 'webview';
    Fvisible : boolean; external name 'visible';
  Public
    title: string;
    description: string;
    function onDidChangeVisibility(aHandler : TVSvoidHandler) : TVSDisposable; overload;
    function onDidChangeVisibility(aHandler : TVSvoidHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeVisibility(aHandler : TVSvoidHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidDispose(aHandler : TVSvoidHandler) : TVSDisposable; overload;
    function onDidDispose(aHandler : TVSvoidHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidDispose(aHandler : TVSvoidHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    procedure show() ; overload;
    procedure show(preserveFocus: boolean) ; overload;
    Property viewType : string read FviewType;
    Property webview : TVSWebview read Fwebview;
    Property visible : boolean read Fvisible;
  end;


  TVSWebviewViewResolveContext = class external name 'Object' (TJSObject)
  Private
    Fstate : TJSObject ;external name 'state';
  Public
    Property state : TJSObject read Fstate;
  end;


  TVSWebviewViewProvider  = class external name 'vscode.WebviewViewProvider' (TJSObject)
  Public
    function resolveWebviewView(webviewView: TVSWebviewView; context: TVSWebviewViewResolveContext; token: TVSCancellationToken) : TVSThenable; // void
  end;


  TVSCustomTextEditorProvider  = class external name 'vscode.CustomTextEditorProvider' (TJSObject)
  Public
    function resolveCustomTextEditor(document: TVSTextDocument; webviewPanel: TVSWebviewPanel; token: TVSCancellationToken) : TVSThenable; // void
  end;


  TVSCustomDocument  = class external name 'Object' (TJSObject)
  Private
    Furi : TVSUri; external name 'uri';
  Public
    Property uri : TVSUri read Furi;
    procedure dispose() ;
  end;


  TVSCustomDocumentEditEvent = class external name 'Object' (TJSObject)
  Private
    Fdocument : TVSCustomDocument; external name 'document';
    Flabel : string; external name 'label';
  Public
    Property document : TVSCustomDocument read Fdocument;
    function undo() : TVSThenable; // void
    function redo() : TVSThenable; // void
    Property label_ : string read Flabel;
  end;


  TVSCustomDocumentContentChangeEvent = class external name 'Object' (TJSObject)
  Private
    Fdocument : TVSCustomDocument; external name 'document';
  Public
    Property document : TVSCustomDocument read Fdocument;
  end;


  TVSCustomDocumentBackup  = class external name 'Object' (TJSObject)
  Private
    Fid : string; external name 'id';
  Public
    Property id : string read Fid;
    procedure delete() ;
  end;


  TVSCustomDocumentBackupContext  = class external name 'Object' (TJSObject)
  Private
    Fdestination : TVSUri; external name 'destination';
  Public
    Property destination : TVSUri read Fdestination;
  end;


  TVSCustomDocumentOpenContext  = class external name 'Object' (TJSObject)
  Private
    FbackupId : string; external name 'backupId';
  Public
    Property backupId : string read FbackupId;
  end;


  TVSCustomReadonlyEditorProvider  = class external name 'vscode.CustomReadonlyEditorProvider' (TJSObject)
  Public
    function openCustomDocument(uri: TVSUri; openContext: TVSCustomDocumentOpenContext; token: TVSCancellationToken) : TVSThenable; // TVSCustomDocument;
    function resolveCustomEditor(document: TVSCustomDocument; webviewPanel: TVSWebviewPanel; token: TVSCancellationToken) : TVSThenable; // TVSCustomDocument
  end;

  TVSCustomDocumentEditEventHandler = reference to function (event : TVSCustomDocumentEditEvent): JSValue;

  TVSCustomEditorProvider  = class external name 'vscode.CustomEditorProvider' (TVSCustomReadonlyEditorProvider)
  Public
    function onDidChangeCustomDocument(aHandler : TVSCustomDocumentEditEventHandler) : TVSDisposable; overload;
    function onDidChangeCustomDocument(aHandler : TVSCustomDocumentEditEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeCustomDocument(aHandler : TVSCustomDocumentEditEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function saveCustomDocument(document: TVSCustomDocument; cancellation: TVSCancellationToken) : TVSThenable; // void
    function saveCustomDocumentAs(document: TVSCustomDocument; destination: TVSUri; cancellation: TVSCancellationToken) : TVSThenable; // void
    function revertCustomDocument(document: TVSCustomDocument; cancellation: TVSCancellationToken) : TVSThenable; // void
    function backupCustomDocument(document: TVSCustomDocument; context: TVSCustomDocumentBackupContext; cancellation: TVSCancellationToken) : TVSThenable; // CustomDocumentBackup
  end;


  TVSClipboard  = class external name 'vscode.Clipboard' (TJSObject)
  Public
    function readText() : TVSThenable; // string
    function writeText(value: string) : TVSThenable; // void
  end;


  TVSNSenv  = class external name 'Object' (TJSObject)
  Private
    FappName: string ; external name 'appName';
    FappRoot: string ; external name 'appRoot';
    FuriScheme: string ; external name 'uriScheme';
    Flanguage: string ; external name 'language';
    Fclipboard: TVSClipboard ; external name 'clipboard';
    FmachineId: string ; external name 'machineId';
    FsessionId: string ; external name 'sessionId';
    FremoteName: string ; external name 'remoteName';
    Fshell: string ; external name 'shell';
    FuiKind: TVSUIKind ; external name 'uiKind';
  Public
    property appName: string read fappName;
    property appRoot: string read fappRoot;
    property uriScheme: string read furiScheme;
    property language: string read flanguage;
    property clipboard: TVSClipboard read fclipboard;
    property machineId: string read fmachineId;
    property sessionId: string read fsessionId;
    property remoteName: string read fremoteName;
    property shell: string read fshell;
    property uiKind: TVSUIKind read fuiKind;
    function openExternal(target: TVSUri) : TVSThenable; // boolean
    function asExternalUri(target: TVSUri) : TVSThenable; // TVSUri
  end;

  TVSRegisterCommandCallback = reference to function (args:TJSValueDynArray) : JSValue;
  TVSRegisterTextEditorCommandCallback = reference to procedure (textEditor: TVSTextEditor; edit: TVSTextEditorEdit;args:TJSValueDynArray);
  TVSNScommands  = class external name 'Object' (TJSObject)
  Public
    function registerCommand(command: string; callback: TVSRegisterCommandCallback) : TVSDisposable; overload;
    function registerCommand(command: string; callback: TVSRegisterCommandCallback; thisArg: JSValue) : TVSDisposable; overload;
    function registerTextEditorCommand(command: string; callback: TVSRegisterTextEditorCommandCallback) : TVSDisposable; overload;
    function registerTextEditorCommand(command: string; callback: TVSRegisterTextEditorCommandCallback; thisArg: JSValue) : TVSDisposable; overload;
    function executeCommand(command: string) : TVSThenable; varargs; // T | undefined
    function getCommands() : TVSThenable; // stringDynArray overload;
    function getCommands(filterInternal: boolean) : TVSThenable; // stringDynArray overload;
  end;


  TVSWindowState  = class external name 'vscode.WindowState' (TJSObject)
  Private
    Ffocused : boolean; external name 'focused';
  Public
    Property focused : boolean read Ffocused;
  end;


  TVSUriHandler  = class external name 'vscode.UriHandler' (TJSObject)
  Public
    function handleUri(uri: TVSUri) : JSValue;
  end;

  TVSregisterCustomEditorProviderOptions = class external name 'Object' (TJSObject)
  Private
    FwebviewOptions : TVSWebviewPanelOptions; external name 'webviewOptions';
    FsupportsMultipleEditorsPerDocument : boolean; external name 'supportsMultipleEditorsPerDocument';
  Public
    Property webviewOptions : TVSWebviewPanelOptions read FwebviewOptions;
    Property supportsMultipleEditorsPerDocument : boolean read FsupportsMultipleEditorsPerDocument;
  end;

  TVSregisterWebviewViewProviderOptionsWebviewOptions = class external name 'Object' (TJSObject)
    retainContextWhenHidden : boolean;
  end;

  TVSregisterWebviewViewProviderOptions = class external name 'Object' (TJSObject)
    webviewOptions: TVSregisterWebviewViewProviderOptionsWebviewOptions;
  end;

  TVSTextEditorDynArrayHandler = reference to function (aEvent : TVSTextEditorDynArray) : JSValue;

  TVSWindowcreateWebviewPanelOptions = class external name 'Object' (TJSObject)
     viewColumn: TVSViewColumn;
     preserveFocus: boolean ;
  end;

  TVSTaskProgressCallback = reference to function (progress: TVSProgress) : TVSThenable;

  TVSNSWindow  = class external name 'Object' (TJSObject)
  private
    fterminals: TVSTerminalDynArray; external name 'terminals';
  Public
    activeTextEditor: TVSTextEditor ;
    visibleTextEditors: TVSTextEditorDynArray;
    activeColorTheme: TVSColorTheme;
    activeTerminal: TVSTerminal ;
    state: TVSWindowState;

    function onDidChangeActiveTextEditor(aHandler : TVSTextEditorHandler) : TVSDisposable; overload;
    function onDidChangeActiveTextEditor(aHandler : TVSTextEditorHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeActiveTextEditor(aHandler : TVSTextEditorHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidChangeVisibleTextEditor(aHandler : TVSTextEditorDynArrayHandler) : TVSDisposable; overload;
    function onDidChangeVisibleTextEditor(aHandler : TVSTextEditorDynArrayHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeVisibleTextEditor(aHandler : TVSTextEditorDynArrayHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidChangeTextEditorSelection(aHandler : TVSTextEditorSelectionChangeEventHandler) : TVSDisposable; overload;
    function onDidChangeTextEditorSelection(aHandler : TVSTextEditorSelectionChangeEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeTextEditorSelection(aHandler : TVSTextEditorSelectionChangeEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidChangeTextEditorVisibleRanges(aHandler : TVSTextEditorVisibleRangesChangeEventHandler) : TVSDisposable; overload;
    function onDidChangeTextEditorVisibleRanges(aHandler : TVSTextEditorVisibleRangesChangeEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeTextEditorVisibleRanges(aHandler : TVSTextEditorVisibleRangesChangeEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidChangeTextEditorOptions(aHandler : TVSTextEditorOptionsChangeEventHandler) : TVSDisposable; overload;
    function onDidChangeTextEditorOptions(aHandler : TVSTextEditorOptionsChangeEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeTextEditorOptions(aHandler : TVSTextEditorOptionsChangeEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidChangeTextEditorViewColumn(aHandler : TVSTextEditorViewColumnChangeEventHandler) : TVSDisposable; overload;
    function onDidChangeTextEditorViewColumn(aHandler : TVSTextEditorViewColumnChangeEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeTextEditorViewColumn(aHandler : TVSTextEditorViewColumnChangeEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidChangeActiveTerminal(aHandler : TVSTerminalHandler) : TVSDisposable; overload;
    function onDidChangeActiveTerminal(aHandler : TVSTerminalHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeActiveTerminal(aHandler : TVSTerminalHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidOpenTerminal(aHandler : TVSTerminalHandler) : TVSDisposable; overload;
    function onDidOpenTerminal(aHandler : TVSTerminalHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidOpenTerminal(aHandler : TVSTerminalHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidCloseTerminal(aHandler : TVSTerminalHandler) : TVSDisposable; overload;
    function onDidCloseTerminal(aHandler : TVSTerminalHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidCloseTerminal(aHandler : TVSTerminalHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidChangeWindowState(aHandler : TVSWindowStateHandler) : TVSDisposable; overload;
    function onDidChangeWindowState(aHandler : TVSWindowStateHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeWindowState(aHandler : TVSWindowStateHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function showTextDocument(document: TVSTextDocument) : TVSThenable; // TVSTextEditor overload;
    function showTextDocument(document: TVSTextDocument; column: TVSViewColumn) : TVSThenable; // TVSTextEditor overload;
    function showTextDocument(document: TVSTextDocument; column: TVSViewColumn; preserveFocus: boolean) : TVSThenable; // TVSTextEditor overload;
    function showTextDocument(document: TVSTextDocument; options: TVSTextDocumentShowOptions) : TVSThenable; // TVSTextEditor overload;
    function showTextDocument(uri: TVSUri) : TVSThenable; // TVSTextEditor overload;
    function showTextDocument(uri: TVSUri; options: TVSTextDocumentShowOptions) : TVSThenable; // TVSTextEditor overload;
    function createTextEditorDecorationType(options: TVSDecorationRenderOptions) : TVSTextEditorDecorationType;
    function showInformationMessage(message: string) : TVSThenable; // string | undefined
    function showInformationMessage(message: string; items: TstringDynArray) : TVSThenable; // string | undefined
    function showInformationMessage(message: string; options: TVSMessageOptions; items: TstringDynArray) : TVSThenable; // string | undefined
    function showInformationMessage(message: string; items: TVSMessageItemDynArray) : TVSThenable; // T | undefined
    function showInformationMessage(message: string; options: TVSMessageOptions; items: TVSMessageItemDynArray) : TVSThenable; // T | undefined
    function showWarningMessage(message: string) : TVSThenable; // string | undefined
    function showWarningMessage(message: string; items: tstringDynArray) : TVSThenable; // string | undefined
    function showWarningMessage(message: string; options: TVSMessageOptions; items: TstringDynArray) : TVSThenable; // string | undefined
    function showWarningMessage(message: string; items: TVSMessageItemDynArray) : TVSThenable; // T | undefined
    function showWarningMessage(message: string; options: TVSMessageOptions; items: TVSMessageItemDynArray) : TVSThenable; // T | undefined
    function showErrorMessage(message: string) : TVSThenable; // string | undefined
    function showErrorMessage(message: string; items: tstringDynArray) : TVSThenable; // string | undefined
    function showErrorMessage(message: string; options: TVSMessageOptions; titems: tstringDynArray) : TVSThenable; // string | undefined
    function showErrorMessage(message: string; items: TVSMessageItemDynArray) : TVSThenable; // T | undefined
    function showErrorMessage(message: string; options: TVSMessageOptions; items: TVSMessageItemDynArray) : TVSThenable; // T | undefined
    function showQuickPick(items: TstringDynArray): TVSThenable; overload; // TStringDynArray
    function showQuickPick(items: TVSThenableDynArray): TVSThenable; overload; // TStringDynArray
    function showQuickPick(items: TstringDynArray; options: TVSQuickPickOptions): TVSThenable; overload; // TStringDynArray
    function showQuickPick(items: TVSThenableDynArray; options: TVSQuickPickOptions): TVSThenable; overload; // TStringDynArray
    function showQuickPick(items: TstringDynArray; options: TVSQuickPickOptions;  token: TVSCancellationToken): TVSThenable; overload; // TStringDynArray
    function showQuickPick(items: TVSThenableDynArray; options: TVSQuickPickOptions; token: TVSCancellationToken): TVSThenable; overload; // TStringDynArray
    function showQuickPick(items: TVSQuickPickItem): TVSThenable; overload; // TStringDynArray
    function showQuickPick(items: TVSQuickPickItem; options: TVSQuickPickOptions): TVSThenable; overload; // TStringDynArray
    function showQuickPick(items: TVSQuickPickItem; options: TVSQuickPickOptions;  token: TVSCancellationToken): TVSThenable; overload; // TStringDynArray
    function showWorkspaceFolderPick() : TVSThenable; // TVSWorkspaceFolder overload;
    function showWorkspaceFolderPick(options: TVSWorkspaceFolderPickOptions) : TVSThenable; // TVSWorkspaceFolder overload;
    function showOpenDialog() : TVSThenable; // TVSUriDynArray overload;
    function showOpenDialog(options: TVSOpenDialogOptions) : TVSThenable; // TVSUriDynArray overload;
    function showSaveDialog() : TVSThenable; // TVSUri overload;
    function showSaveDialog(options: TVSSaveDialogOptions) : TVSThenable; // TVSUri overload;
    function showInputBox() : TVSThenable; // string overload;
    function showInputBox(options: TVSInputBoxOptions) : TVSThenable; // string overload;
    function showInputBox(options: TVSInputBoxOptions; token: TVSCancellationToken) : TVSThenable; // string overload;
    function createQuickPick : TVSQuickPick;
    function createInputBox() : TVSInputBox;
    function createOutputChannel(name: string) : TVSOutputChannel;
    function createWebviewPanel(viewType: string; title: string; showOptions: TVSViewColumn): TVSWebviewPanel; overload;
    function createWebviewPanel(viewType: string; title: string; showOptions: TVSWindowcreateWebviewPanelOptions): TVSWebviewPanel; overload;
    function createWebviewPanel(viewType: string; title: string; showOptions: TVSViewColumn;options: TVSWebviewPanelOptions): TVSWebviewPanel; overload;
    function createWebviewPanel(viewType: string; title: string; showOptions: TVSWindowcreateWebviewPanelOptions;options: TVSWebviewPanelOptions): TVSWebviewPanel; overload;
    function setStatusBarMessage(text: string; hideAfterTimeout: NativeInt) : TVSDisposable;
    function setStatusBarMessage(text: string; hideWhenDone: TVSThenable) : TVSDisposable;
    function setStatusBarMessage(text: string) : TVSDisposable;
    function withScmProgress(task: TVSTaskProgressCallback) : TVSThenable;
    function withProgress(options: TVSProgressOptions): TVSThenable overload;
    function withProgress(options: TVSProgressOptions; task: TVSTaskProgressCallback): TVSThenable; overload;
    function withProgress(options: TVSProgressOptions; task: TVSTaskProgressCallback; token: TVSCancellationToken) : TVSThenable; overload; // JSValue;
    function createStatusBarItem() : TVSStatusBarItem; overload;
    function createStatusBarItem(alignment: TVSStatusBarAlignment) : TVSStatusBarItem; overload;
    function createStatusBarItem(alignment: TVSStatusBarAlignment; priority: NativeInt) : TVSStatusBarItem; overload;
    function createTerminal() : TVSTerminal; overload;
    function createTerminal(name: string) : TVSTerminal; overload;
    function createTerminal(name: string; shellPath: string) : TVSTerminal; overload;
    function createTerminal(name: string; shellPath: string; shellArgs: TstringDynArray) : TVSTerminal; overload;
    function createTerminal(name: string; shellPath: string; shellArgs: string) : TVSTerminal; overload;
    function createTerminal(options: TVSTerminalOptions) : TVSTerminal;
    function createTerminal(options: TVSExtensionTerminalOptions) : TVSTerminal;
    function registerTreeDataProvider(viewId: string; treeDataProvider: TVSTreeDataProvider) : TVSDisposable;
    function createTreeView(viewId: string; options: TVSTreeViewOptions) : TVSTreeView;
    function registerUriHandler(handler: TVSUriHandler) : TVSDisposable;
    function registerWebviewPanelSerializer(viewType: string; serializer: TVSWebviewPanelSerializer) : TVSDisposable;
    function registerWebviewViewProvider(viewId: string; provider: TVSWebviewViewProvider): TVSDisposable; overload;
    function registerWebviewViewProvider(viewId: string; provider: TVSWebviewViewProvider; options: TVSregisterWebviewViewProviderOptions): TVSDisposable; overload;
    function registerCustomEditorProvider(viewType: string; provider: TVSCustomTextEditorProvider): TVSDisposable; overload;
    function registerCustomEditorProvider(viewType: string; provider: TVSCustomReadonlyEditorProvider): TVSDisposable; overload;
    function registerCustomEditorProvider(viewType: string; provider: TVSCustomEditorProvider): TVSDisposable; overload;
    function registerCustomEditorProvider(viewType: string; provider: TVSCustomTextEditorProvider ; options: TVSregisterCustomEditorProviderOptions): TVSDisposable; overload;
    function registerCustomEditorProvider(viewType: string; provider: TVSCustomReadonlyEditorProvider ; options: TVSregisterCustomEditorProviderOptions): TVSDisposable; overload;
    function registerCustomEditorProvider(viewType: string; provider: TVSCustomEditorProvider; options: TVSregisterCustomEditorProviderOptions): TVSDisposable; overload;
    function registerTerminalLinkProvider(provider: TVSTerminalLinkProvider) : TVSDisposable;
    function registerFileDecorationProvider(provider: TVSFileDecorationProvider) : TVSDisposable;
    function onDidChangeActiveColorTheme(aHandler : TVSColorThemeHandler) : TVSDisposable; overload;
    function onDidChangeActiveColorTheme(aHandler : TVSColorThemeHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeActiveColorTheme(aHandler : TVSColorThemeHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    property terminals: TVSTerminalDynArray read FTerminals;
  end;


  TVSTreeViewOptions  = class external name 'vscode.TreeViewOptions' (TJSObject)
  Public
    treeDataProvider: TVSTreeDataProvider;
    showCollapseAll: boolean;
    canSelectMany: boolean;
  end;


  TVSTreeViewExpansionEvent  = class external name 'vscode.TreeViewExpansionEvent' (TJSObject)
  Private
    Felement : JSValue; external name 'element';
  Public
    Property element : JSValue read Felement;
  end;


  TVSTreeViewSelectionChangeEvent  = class external name 'vscode.TreeViewSelectionChangeEvent' (TJSObject)
  Private
    Fselection : TJSValueDynArray; external name 'selection';
  Public
    Property selection : TJSValueDynArray read Fselection;
  end;


  TVSTreeViewVisibilityChangeEvent  = class external name 'vscode.TreeViewVisibilityChangeEvent' (TJSObject)
  Private
    Fvisible : boolean; external name 'visible';
  Public
    Property visible : boolean read Fvisible;
  end;

  TVSTreeViewRevealOptions = class external name 'Object' (TJSObject)
  Public
    select: boolean;
    focus: boolean;
    expand: boolean;
    expandInt : NativeInt; external name 'expand';
  end;

  TVSTreeView = class external name 'vscode.TreeView' (TVSDisposable)
  private
    Fselection : TJSValueDynArray; external name 'selection';
    Fvisible : boolean; external name 'visible';
  Public
    message: string;
    title: string;
    description: string;
    function onDidCollapseElement(aHandler : TVSTreeViewExpansionEventHandler) : TVSDisposable; overload;
    function onDidCollapseElement(aHandler : TVSTreeViewExpansionEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidCollapseElement(aHandler : TVSTreeViewExpansionEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidExpandElement(aHandler : TVSTreeViewExpansionEventHandler) : TVSDisposable; overload;
    function onDidExpandElement(aHandler : TVSTreeViewExpansionEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidExpandElement(aHandler : TVSTreeViewExpansionEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidChangeSelection(aHandler : TVSTreeViewSelectionChangeEventHandler) : TVSDisposable; overload;
    function onDidChangeSelection(aHandler : TVSTreeViewSelectionChangeEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeSelection (aHandler : TVSTreeViewSelectionChangeEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidChangeVisibility(aHandler : TVSTreeViewVisibilityChangeEventHandler) : TVSDisposable; overload;
    function onDidChangeVisibility(aHandler : TVSTreeViewVisibilityChangeEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeVisibility(aHandler : TVSTreeViewVisibilityChangeEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function reveal(element: JSValue): TVSThenable; // void overload;
    function reveal(element: JSValue; options:TVSTreeViewRevealOptions): TVSThenable; // void overload;
    constructor new(aCallOndispose: TVSVoidHandler);
    Property selection : TJSValueDynArray read Fselection;
    Property visible : boolean read Fvisible;
  end;


  TVSTreeDataProvider  = class external name 'vscode.TreeDataProvider' (TJSObject)
  Public
    function onDidChangeTreeData(aHandler : TVSAnyHandler) : TVSDisposable; overload;
    function onDidChangeTreeData(aHandler : TVSAnyHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeTreeData(aHandler : TVSAnyHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function getTreeItem(element: JSValue) : JSValue; // TreeItem
    function getChildren() : JSValue; overload;
    function getChildren(element: JSValue) : JSValue; overload;
    function getParent(element: JSValue) : JSValue;
    function resolveTreeItem(item: TVSTreeItem; element: JSValue) : JSValue;
  end;


  TVSTreeItem  = class external name 'vscode.TreeItem' (TJSObject)
  Public
    label_: string; external name 'label';
    labelObj : TVSTreeItemLabel; external name 'label';
    id: string;
    iconPath: string;
    iconPathUri:  TVSUri; external name 'iconPath';
    iconPathObj  : TVSIconPathObject; external name 'iconPath';
    iconPathTheme : TVSThemeIcon; external name 'iconPath';
    description: string;
    descriptionBool : boolean; external name 'description';
    resourceUri: TVSUri;
    tooltip: string;
    tooltipObj : TVSMarkdownString ; external name 'tooltip';
    command: TVSCommand;
    collapsibleState: TVSTreeItemCollapsibleState;
    contextValue: string;
    accessibilityInformation: TVSAccessibilityInformation;
    constructor New(alabel: TVSTreeItemLabel); overload;
    constructor New(alabel: string); overload;
    constructor New(alabel: string ; collapsibleState: TVSTreeItemCollapsibleState); overload;
    constructor New(alabel: TVSTreeItemLabel; collapsibleState: TVSTreeItemCollapsibleState); overload;
    constructor New(resourceUri: TVSUri); overload;
    constructor New(resourceUri: TVSUri; collapsibleState: TVSTreeItemCollapsibleState); overload;
  end;



  TVSTreeItemLabel  = class external name 'vscode.TreeItemLabel' (TJSObject)
  Public
    label_: string; external name 'label';
    highlights: TJSValueDynArray;
  end;


  TVSTerminalOptions  = class external name 'vscode.TerminalOptions' (TJSObject)
  Public
    name: string;
    shellPath: string;
    shellArgsArray : TstringDynArray; external name 'string';
    shellArgs : string;
    cwd : string;
    cwdObj: TVSUri; external name 'cwd';
    env: TJSObject;
    strictEnv: boolean;
    hideFromUser: boolean;
  end;


  TVSExtensionTerminalOptions  = class external name 'vscode.ExtensionTerminalOptions' (TJSObject)
  Public
    name: string;
    pty: TVSPseudoterminal;
  end;


  TVSPseudoterminal  = class external name 'Object' (TJSObject)
  Public
    function onDidWrite(aHandler : TVSstringHandler) : TVSDisposable; overload;
    function onDidWrite(aHandler : TVSstringHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidWrite(aHandler : TVSstringHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidOverrideDimensions(aHandler : TVSTerminalDimensionsHandler) : TVSDisposable; overload;
    function onDidOverrideDimensions(aHandler : TVSTerminalDimensionsHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidOverrideDimensions(aHandler : TVSTerminalDimensionsHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidClose(aHandler : TVSNumberHandler) : TVSDisposable; overload;
    function onDidClose(aHandler : TVSNumberHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidClose(aHandler : TVSNumberHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    procedure open(initialDimensions: TVSTerminalDimensions) ;
    procedure open() ;
    procedure close() ;
    procedure handleInput(data: string) ;
    procedure setDimensions(dimensions: TVSTerminalDimensions) ;
  end;


  TVSTerminalDimensions  = class external name 'vscode.TVSTerminalDimensions' (TJSObject)
  Private
    Fcolumns : NativeInt; external name 'columns';
    Frows : NativeInt; external name 'rows';
  Public
    Property columns : NativeInt read Fcolumns;
    Property rows : NativeInt read Frows;
  end;


  TVSTerminalExitStatus  = class external name 'vscode.TerminalExitStatus' (TJSObject)
  Private
    Fcode : NativeInt ;external name 'code';
  Public
    Property code : NativeInt read Fcode;
  end;




  TVSEnvironmentVariableMutator  = class external name 'vscode.TVSEnvironmentVariableMutator' (TJSObject)
  Private
    Ftype : TVSEnvironmentVariableMutatorType; external name 'type';
    Fvalue : string; external name 'value';
  Public
    Property type_ : TVSEnvironmentVariableMutatorType read Ftype;
    Property value : string read Fvalue;
  end;


  TVSEnvironmentVariableCallback = reference to function (variable: string; mutator: TVSEnvironmentVariableMutator; collection: TVSEnvironmentVariableCollection) : JSValue;
  TVSEnvironmentVariableCollection  = class external name 'vscode.EnvironmentVariableCollection' (TJSObject)
  Public
    persistent: boolean;
    procedure replace(variable: string; value: string) ;
    procedure append(variable: string; value: string) ;
    procedure prepend(variable: string; value: string) ;
    function get(variable: string) : TVSEnvironmentVariableMutator ;
    procedure forEach(callback:TVSEnvironmentVariableCallback); overload;
    procedure forEach(callback:TVSEnvironmentVariableCallback; thisArg: JSValue) ; overload;
    procedure delete(variable: string) ;
    procedure clear() ;
  end;


  TVSProgressOptionsLocationOption = class external name 'Object' (TJSObject)
    viewId: string;
  end;

  TVSProgressOptions  = class external name 'vscode.ProgressOptions' (TJSObject)
  Public
    location: TVSProgressLocation;
    locationObj :  TVSProgressOptionsLocationOption; external name 'location';
    title: string;
    cancellable: boolean;
  end;


  TVSQuickInput  = class external name 'vscode.QuickInput' (TJSObject)
  Public
    title: string ;
    step: NativeInt ;
    totalSteps: NativeInt ;
    enabled: boolean;
    busy: boolean;
    ignoreFocusOut: boolean;
    procedure show() ;
    procedure hide() ;
    function onDidHide(aHandler : TVSvoidHandler) : TVSDisposable; overload;
    function onDidHide(aHandler : TVSvoidHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidHide(aHandler : TVSvoidHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    procedure dispose() ;
  end;


  // TVSQuickPickItem
  TVSQuickPickItemEventHandler = reference to function(aEvent : TVSQuickPickItemDynArray) : JSValue;
  TVSQuickPick = class external name 'vscode.QuickPick'  (TVSQuickInput)
  Public
    items: TVSQuickPickItemDynArray;
    canSelectMany: boolean;
    matchOnDescription: boolean;
    matchOnDetail: boolean;
    activeItems: TVSQuickPickItemDynArray;
    value: string;
    placeholder: string ;
    buttons: TVSQuickInputButtonDynArray;
    selectedItems: TVSQuickPickItemDynArray;

    function onDidChangeValue(aHandler : TVSstringHandler) : TVSDisposable; overload;
    function onDidChangeValue(aHandler : TVSstringHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeValue(aHandler : TVSstringHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidAccept(aHandler : TVSvoidHandler) : TVSDisposable; overload;
    function onDidAccept(aHandler : TVSvoidHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidAccept(aHandler : TVSvoidHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidHide(aHandler : TVSvoidHandler) : TVSDisposable; overload;
    function onDidHide(aHandler : TVSvoidHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidHide(aHandler : TVSvoidHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidTriggerButton(aHandler : TVSQuickInputButtonHandler) : TVSDisposable; overload;
    function onDidTriggerButton(aHandler : TVSQuickInputButtonHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidTriggerButton(aHandler : TVSQuickInputButtonHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;

    function onDidChangeActive(aHandler : TVSQuickPickItemEventHandler) : TVSDisposable; overload;
    function onDidChangeActive(aHandler : TVSQuickPickItemEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeActive(aHandler : TVSQuickPickItemEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;

    function onDidChangeSelection(aHandler : TVSQuickPickItemEventHandler) : TVSDisposable; overload;
    function onDidChangeSelection(aHandler : TVSQuickPickItemEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeSelection(aHandler : TVSQuickPickItemEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;

  end;

  // TVSQuickInput
  TVSInputBox  = class external name 'vscode.InputBox' (TVSQuickInput)
  Public
    value: string;
    placeholder: string ;
    password: boolean;
    buttons: TVSQuickInputButtonDynArray;
    prompt: string ;
    validationMessage: string ;
    function onDidChangeValue (aHandler : TVSstringHandler) : TVSDisposable; overload;
    function onDidChangeValue(aHandler : TVSstringHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeValue(aHandler : TVSstringHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidAccept(aHandler : TVSvoidHandler) : TVSDisposable; overload;
    function onDidAccept(aHandler : TVSvoidHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidAccept(aHandler : TVSvoidHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidHide(aHandler : TVSvoidHandler) : TVSDisposable; overload;
    function onDidHide(aHandler : TVSvoidHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidHide(aHandler : TVSvoidHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidTriggerButton(aHandler : TVSQuickInputButtonHandler) : TVSDisposable; overload;
    function onDidTriggerButton(aHandler : TVSQuickInputButtonHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidTriggerButton(aHandler : TVSQuickInputButtonHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
  end;


  TVSQuickInputButton  = class external name 'vscode.QuickInputButton' (TJSObject)
  Public
    iconPath: TVSUri;
    iconPathObj : TVSIconPathObject; external name 'iconPath';
    iconPathTheme: TVSThemeIcon; external name 'iconPath';
    tooltip : string;
  end;


  TVSQuickInputButtons  = class external name 'vscode.QuickInputButtons' (TJSObject)
  Public
    class var Back: TVSQuickInputButton;
  end;


  TVSTextDocumentContentChangeEvent  = class external name 'vscode.TextDocumentContentChangeEvent' (TJSObject)
  Private
    Frange : TVSRange; external name 'range';
    FrangeOffset : NativeInt; external name 'rangeOffset';
    FrangeLength : NativeInt; external name 'rangeLength';
    Ftext : string; external name 'text';
  Public
    Property range : TVSRange read Frange;
    Property rangeOffset : NativeInt read FrangeOffset;
    Property rangeLength : NativeInt read FrangeLength;
    Property text : string read Ftext;
  end;


  TVSTextDocumentChangeEvent  = class external name 'vscode.TextDocumentChangeEvent' (TJSObject)
  Private
    Fdocument : TVSTextDocument; external name 'document';
    FcontentChanges : TVSTextDocumentContentChangeEventDynArray; external name 'contentChanges';
  Public
    Property document : TVSTextDocument read Fdocument;
    Property contentChanges : TVSTextDocumentContentChangeEventDynArray read FcontentChanges;
  end;



  TVSTextDocumentWillSaveEvent  = class external name 'vscode.TextDocumentWillSaveEvent' (TJSObject)
  Private
    Fdocument : TVSTextDocument; external name 'document';
    Freason : TVSTextDocumentSaveReason; external name 'reason';
  Public
    Property document : TVSTextDocument read Fdocument;
    Property reason : TVSTextDocumentSaveReason read Freason;
    procedure waitUntil(aThenable: TVSThenable);
  end;


  TVSFileWillCreateEvent  = class external name 'vscode.FileWillCreateEvent' (TJSObject)
  Private
    Ffiles : TVSUriDynArray; external name 'files';
  Public
    Property files : TVSUriDynArray read Ffiles;
    procedure waitUntil(aThenable: TVSThenable) ;
  end;


  TVSFileCreateEvent  = class external name 'vscode.FileCreateEvent' (TJSObject)
  Private
    Ffiles : TVSUriDynArray; external name 'files';
  Public
    Property files : TVSUriDynArray read Ffiles;
  end;


  TVSFileWillDeleteEvent  = class external name 'vscode.FileWillDeleteEvent' (TJSObject)
  Private
    Ffiles : TVSUriDynArray; external name 'files';
  Public
    Property files : TVSUriDynArray read Ffiles;
    procedure waitUntil(aThenable: TVSThenable) ;
  end;


  TVSFileDeleteEvent  = class external name 'vscode.FileDeleteEvent' (TJSObject)
  Private
    Ffiles : TVSUriDynArray; external name 'files';
  Public
    Property files : TVSUriDynArray read Ffiles;
  end;

  TVSFileWillRenameObj = class external name 'Object' (TJSObject)
    oldUri: TVSUri;
    newUri: TVSUri;
  end;
  TVSFileWillRenameObjDynArray = Array of TVSFileWillRenameObj;

  TVSFileWillRenameEvent  = class external name 'vscode.FileWillRenameEvent' (TJSObject)
  private
    ffiles : TVSFileWillRenameObjDynArray; external name 'files';
  Public
    Property files: TVSFileWillRenameObjDynArray read ffiles;
    procedure waitUntil(AThenable: TVSThenable) ;
  end;


  TVSFileRenameEvent  = class external name 'vscode.FileRenameEvent' (TJSObject)
  private
    ffiles : TVSFileWillRenameObjDynArray; external name 'files';
  Public
    Property files: TVSFileWillRenameObjDynArray read ffiles;
  end;

  TVSWorkspaceFoldersChangeEvent  = class external name 'vscode.WorkspaceFoldersChangeEvent' (TJSObject)
  Private
    Fadded : TVSWorkspaceFolderDynArray; external name 'added';
    Fremoved : TVSWorkspaceFolderDynArray; external name 'removed';
  Public
    Property added : TVSWorkspaceFolderDynArray read Fadded;
    Property removed : TVSWorkspaceFolderDynArray read Fremoved;
  end;


  TVSWorkspaceFolder  = class external name 'vscode.WorkspaceFolder' (TJSObject)
  Private
    Furi : TVSUri; external name 'uri';
    Fname : string; external name 'name';
    Findex : NativeInt; external name 'index';
  Public
    Property uri : TVSUri read Furi;
    Property name : string read Fname;
    Property index : NativeInt read Findex;
  end;

  TVSworkspaceFolderObj = class external name 'Object' (TJSObject)
    uri: TVSUri;
  end;
  TVSworkspaceFolderObjDynArray = Array of TVSworkspaceFolderObj;

  TVSOpenOptions = class external name 'Object' (TJSObject)
    language : string;
    content : string;
  end;

  TVSregisterFileSystemProviderOptions = class external name 'Object' (TJSObject)
     isCaseSensitive: boolean;
     isReadonly: boolean
  end;

  TVSGlobPattern = String;

  TVSNSworkspace  = class external name 'Object' (TJSObject)
  private
    ftextDocuments: TVSTextDocumentDynArray; external name 'textDocuments';
    Ffs: TVSFileSystem; external name 'fs';
    FrootPath: string ; external name 'rootPath';
    FworkspaceFolders: TVSWorkspaceFolderDynArray ; external name 'workspaceFolders';
    Fname: string ; external name 'name';
    FworkspaceFile: TVSUri ; external name 'workspaceFile';

  Public
    property fs: TVSFileSystem read ffs;
    property rootPath: string read frootpath;
    property workspaceFolders: TVSWorkspaceFolderDynArray read FworkspaceFolders;
    property name: string read fname;
    property workspaceFile: TVSUri read fworkspaceFile ;
    function onDidChangeWorkspaceFolders(aHandler : TVSWorkspaceFoldersChangeEventHandler) : TVSDisposable; overload;
    function onDidChangeWorkspaceFolders(aHandler : TVSWorkspaceFoldersChangeEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeWorkspaceFolders(aHandler : TVSWorkspaceFoldersChangeEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function getWorkspaceFolder(uri: TVSUri) : TVSWorkspaceFolder ;
    function asRelativePath(pathOrUri: string) : string; overload;
    function asRelativePath(pathOrUri: TVSUri) : string; overload;
    function asRelativePath(pathOrUri: string; includeWorkspaceFolder: boolean) : string; overload;
    function asRelativePath(pathOrUri: TVSUri; includeWorkspaceFolder: boolean) : string; overload;
    function updateWorkspaceFolders(start: NativeInt; deleteCount: NativeInt; workspaceFoldersToAdd: TVSworkspaceFolderObjDynArray): boolean; overload;
    function createFileSystemWatcher(aGlobPattern: TVSGlobPattern) : TVSFileSystemWatcher; overload;
    function createFileSystemWatcher(aGlobPattern: TVSGlobPattern; ignoreCreateEvents: boolean) : TVSFileSystemWatcher; overload;
    function createFileSystemWatcher(aGlobPattern: TVSGlobPattern; ignoreCreateEvents: boolean; ignoreChangeEvents: boolean) : TVSFileSystemWatcher; overload;
    function createFileSystemWatcher(aGlobPattern: TVSGlobPattern; ignoreCreateEvents: boolean; ignoreChangeEvents: boolean; ignoreDeleteEvents: boolean) : TVSFileSystemWatcher; overload;
    function createFileSystemWatcher(aGlobPattern: TVSRelativePattern) : TVSFileSystemWatcher; overload;
    function createFileSystemWatcher(aGlobPattern: TVSRelativePattern; ignoreCreateEvents: boolean) : TVSFileSystemWatcher; overload;
    function createFileSystemWatcher(aGlobPattern: TVSRelativePattern; ignoreCreateEvents: boolean; ignoreChangeEvents: boolean) : TVSFileSystemWatcher; overload;
    function createFileSystemWatcher(aGlobPattern: TVSRelativePattern; ignoreCreateEvents: boolean; ignoreChangeEvents: boolean; ignoreDeleteEvents: boolean) : TVSFileSystemWatcher; overload;
    function findFiles(include: TVSGlobPattern) : TVSThenable; // TVSUriDynArray overload;
    function findFiles(include: TVSGlobPattern; exclude: TVSGlobPattern) : TVSThenable; // TVSUriDynArray overload;
    function findFiles(include: TVSGlobPattern; exclude: TVSGlobPattern; maxResults: NativeInt) : TVSThenable; // TVSUriDynArray overload;
    function findFiles(include: TVSGlobPattern; exclude: TVSGlobPattern; maxResults: NativeInt; token: TVSCancellationToken) : TVSThenable; // TVSUriDynArray overload;
    function findFiles(include: TVSRelativePattern) : TVSThenable; // TVSUriDynArray overload;
    function findFiles(include: TVSRelativePattern; exclude: TVSGlobPattern) : TVSThenable; // TVSUriDynArray overload;
    function findFiles(include: TVSRelativePattern; exclude: TVSGlobPattern; maxResults: NativeInt) : TVSThenable; // TVSUriDynArray overload;
    function findFiles(include: TVSRelativePattern; exclude: TVSGlobPattern; maxResults: NativeInt; token: TVSCancellationToken) : TVSThenable; // TVSUriDynArray overload;
    function findFiles(include: TVSRelativePattern; exclude: TVSRelativePattern) : TVSThenable; // TVSUriDynArray overload;
    function findFiles(include: TVSRelativePattern; exclude: TVSRelativePattern; maxResults: NativeInt) : TVSThenable; // TVSUriDynArray overload;
    function findFiles(include: TVSRelativePattern; exclude: TVSRelativePattern; maxResults: NativeInt; token: TVSCancellationToken) : TVSThenable; // TVSUriDynArray overload;
    function findFiles(include: TVSGlobPattern; exclude: TVSRelativePattern) : TVSThenable; // TVSUriDynArray overload;
    function findFiles(include: TVSGlobPattern; exclude: TVSRelativePattern; maxResults: NativeInt) : TVSThenable; // TVSUriDynArray overload;
    function findFiles(include: TVSGlobPattern; exclude: TVSRelativePattern; maxResults: NativeInt; token: TVSCancellationToken) : TVSThenable; // TVSUriDynArray overload;

    function saveAll() : TVSThenable; // boolean overload;
    function saveAll(includeUntitled: boolean) : TVSThenable; // boolean overload;
    function applyEdit(edit: TVSWorkspaceEdit) : TVSThenable; // boolean
    function openTextDocument(uri: TVSUri) : TVSThenable; // TVSTextDocument
    function openTextDocument(fileName: string) : TVSThenable; // TVSTextDocument
    function openTextDocument(options: TVSOpenOptions): TVSThenable; // TVSTextDocument overload;
    function registerTextDocumentContentProvider(scheme: string; provider: TVSTextDocumentContentProvider) : TVSDisposable;
    function onDidOpenTextDocument(aHandler : TVSTextDocumentHandler) : TVSDisposable; overload;
    function onDidOpenTextDocument(aHandler : TVSTextDocumentHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidOpenTextDocument(aHandler : TVSTextDocumentHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidCloseTextDocument(aHandler : TVSTextDocumentHandler) : TVSDisposable; overload;
    function onDidCloseTextDocument(aHandler : TVSTextDocumentHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidCloseTextDocument(aHandler : TVSTextDocumentHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidChangeTextDocument(aHandler : TVSTextDocumentChangeEventHandler) : TVSDisposable; overload;
    function onDidChangeTextDocument(aHandler : TVSTextDocumentChangeEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeTextDocument(aHandler : TVSTextDocumentChangeEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onWillSaveTextDocument(aHandler : TVSTextDocumentWillSaveEventHandler) : TVSDisposable; overload;
    function onWillSaveTextDocument(aHandler : TVSTextDocumentWillSaveEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onWillSaveTextDocument(aHandler : TVSTextDocumentWillSaveEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidSaveTextDocument(aHandler : TVSTextDocumentHandler) : TVSDisposable; overload;
    function onDidSaveTextDocument(aHandler : TVSTextDocumentHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidSaveTextDocument(aHandler : TVSTextDocumentHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onWillCreateFiles(aHandler : TVSFileWillCreateEventHandler) : TVSDisposable; overload;
    function onWillCreateFiles(aHandler : TVSFileWillCreateEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onWillCreateFiles(aHandler : TVSFileWillCreateEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidCreateFiles(aHandler : TVSFileCreateEventHandler) : TVSDisposable; overload;
    function onDidCreateFiles(aHandler : TVSFileCreateEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidCreateFiles(aHandler : TVSFileCreateEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onWillDeleteFiles(aHandler : TVSFileWillDeleteEventHandler) : TVSDisposable; overload;
    function onWillDeleteFiles(aHandler : TVSFileWillDeleteEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onWillDeleteFiles(aHandler : TVSFileWillDeleteEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidDeleteFiles(aHandler : TVSFileDeleteEventHandler) : TVSDisposable; overload;
    function onDidDeleteFiles(aHandler : TVSFileDeleteEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidDeleteFiles(aHandler : TVSFileDeleteEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onWillRenameFiles(aHandler : TVSFileWillRenameEventHandler) : TVSDisposable; overload;
    function onWillRenameFiles(aHandler : TVSFileWillRenameEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onWillRenameFiles(aHandler : TVSFileWillRenameEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function onDidRenameFiles(aHandler : TVSFileRenameEventHandler) : TVSDisposable; overload;
    function onDidRenameFiles(aHandler : TVSFileRenameEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidRenameFiles(aHandler : TVSFileRenameEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function getConfiguration() : TVSWorkspaceConfiguration; overload;
    function getConfiguration(section: string) : TVSWorkspaceConfiguration; overload;
    function getConfiguration(section: string ;scope: TVSConfigurationScope) : TVSWorkspaceConfiguration; overload;
    function onDidChangeConfiguration(aHandler : TVSConfigurationChangeEventHandler) : TVSDisposable; overload;
    function onDidChangeConfiguration(aHandler : TVSConfigurationChangeEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeConfiguration(aHandler : TVSConfigurationChangeEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function registerTaskProvider(atype: string; provider: TVSTaskProvider) : TVSDisposable;
    function registerFileSystemProvider(scheme: string; provider: TVSFileSystemProvider): TVSDisposable; overload;
    function registerFileSystemProvider(scheme: string; provider: TVSFileSystemProvider; options: TVSregisterFileSystemProviderOptions): TVSDisposable; overload;
    property textDocuments: TVSTextDocumentDynArray read ftextDocuments;

  end;

  TVSConfigurationScopeObj = class external name 'Object' (TJSObject)
    uri: TVSUri;
    languageId: string;
  end;

  TVSConfigurationChangeEvent  = class external name 'vscode.ConfigurationChangeEvent' (TJSObject)
  Public
    function affectsConfiguration(section: string) : boolean; overload;
    function affectsConfiguration(section: string; scope: TVSConfigurationScope) : boolean; overload;
  end;


  TVSNSlanguages  = class external name 'Object' (TJSObject)
  Public
    function getLanguages() : TVSThenable; // stringDynArray
    function setTextDocumentLanguage(document: TVSTextDocument; languageId: string) : TVSThenable; // TVSTextDocument
    function match(selector: string; document: TVSTextDocument) : NativeInt;
    function match(selector: tstringdynarray; document: TVSTextDocument) : NativeInt;
    function match(selector: TVSDocumentFilter; document: TVSTextDocument) : NativeInt;
    function match(selector: TVSDocumentFilterDynArray; document: TVSTextDocument) : NativeInt;
    function onDidChangeDiagnostics(aHandler : TVSDiagnosticChangeEventHandler) : TVSDisposable; overload;
    function onDidChangeDiagnostics(aHandler : TVSDiagnosticChangeEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeDiagnostics(aHandler : TVSDiagnosticChangeEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    function getDiagnostics(resource: TVSUri) : TVSDiagnosticDynArray;
    function getDiagnostics() : TVSDiagnosticDynArray;
    function createDiagnosticCollection() : TVSDiagnosticCollection; overload;
    function createDiagnosticCollection(name: string) : TVSDiagnosticCollection; overload;
    function registerCompletionItemProvider(selector: TVSDocumentFilter; provider: TVSCompletionItemProvider; triggerCharacters: TstringDynArray) : TVSDisposable;
    function registerCompletionItemProvider(selector: TVSDocumentFilterDynArray; provider: TVSCompletionItemProvider; triggerCharacters: TstringDynArray) : TVSDisposable;
    function registerCompletionItemProvider(selector: string; provider: TVSCompletionItemProvider; triggerCharacters: TstringDynArray) : TVSDisposable;
    function registerCompletionItemProvider(selector: TStringDynArray; provider: TVSCompletionItemProvider; triggerCharacters: TstringDynArray) : TVSDisposable;
    function registerCodeActionsProvider(selector: string; provider: TVSCodeActionProvider) : TVSDisposable; overload;
    function registerCodeActionsProvider(selector: TStringDynArray; provider: TVSCodeActionProvider) : TVSDisposable; overload;
    function registerCodeActionsProvider(selector: TVSDocumentFilter; provider: TVSCodeActionProvider) : TVSDisposable; overload;
    function registerCodeActionsProvider(selector: TVSDocumentFilterDynArray; provider: TVSCodeActionProvider) : TVSDisposable; overload;
    function registerCodeActionsProvider(selector: string; provider: TVSCodeActionProvider; metadata: TVSCodeActionProviderMetadata) : TVSDisposable; overload;
    function registerCodeActionsProvider(selector: TStringDynArray; provider: TVSCodeActionProvider; metadata: TVSCodeActionProviderMetadata) : TVSDisposable; overload;
    function registerCodeActionsProvider(selector: TVSDocumentFilter; provider: TVSCodeActionProvider; metadata: TVSCodeActionProviderMetadata) : TVSDisposable; overload;
    function registerCodeActionsProvider(selector: TVSDocumentFilterDynArray; provider: TVSCodeActionProvider; metadata: TVSCodeActionProviderMetadata) : TVSDisposable; overload;
    function registerCodeLensProvider(selector: string; provider: TVSCodeLensProvider) : TVSDisposable;
    function registerCodeLensProvider(selector: TStringDynArray; provider: TVSCodeLensProvider) : TVSDisposable;
    function registerCodeLensProvider(selector: TVSDocumentFilter; provider: TVSCodeLensProvider) : TVSDisposable;
    function registerCodeLensProvider(selector: TVSDocumentFilterDynArray; provider: TVSCodeLensProvider) : TVSDisposable;
    function registerDefinitionProvider(selector: string; provider: TVSDefinitionProvider) : TVSDisposable;
    function registerDefinitionProvider(selector: TStringDynArray; provider: TVSDefinitionProvider) : TVSDisposable;
    function registerDefinitionProvider(selector: TVSDocumentFilter; provider: TVSDefinitionProvider) : TVSDisposable;
    function registerDefinitionProvider(selector: TVSDocumentFilterDynArray; provider: TVSDefinitionProvider) : TVSDisposable;
    function registerImplementationProvider(selector: string; provider: TVSImplementationProvider) : TVSDisposable;
    function registerImplementationProvider(selector: TStringDynArray; provider: TVSImplementationProvider) : TVSDisposable;
    function registerImplementationProvider(selector: TVSDocumentFilter; provider: TVSImplementationProvider) : TVSDisposable;
    function registerImplementationProvider(selector: TVSDocumentFilterDynArray; provider: TVSImplementationProvider) : TVSDisposable;
    function registerTypeDefinitionProvider(selector: string; provider: TVSTypeDefinitionProvider) : TVSDisposable;
    function registerTypeDefinitionProvider(selector: TStringDynArray; provider: TVSTypeDefinitionProvider) : TVSDisposable;
    function registerTypeDefinitionProvider(selector: TVSDocumentFilter; provider: TVSTypeDefinitionProvider) : TVSDisposable;
    function registerTypeDefinitionProvider(selector: TVSDocumentFilterDynArray; provider: TVSTypeDefinitionProvider) : TVSDisposable;
    function registerDeclarationProvider(selector: string; provider: TVSDeclarationProvider) : TVSDisposable;
    function registerDeclarationProvider(selector: TStringDynArray; provider: TVSDeclarationProvider) : TVSDisposable;
    function registerDeclarationProvider(selector: TVSDocumentFilter; provider: TVSDeclarationProvider) : TVSDisposable;
    function registerDeclarationProvider(selector: TVSDocumentFilterDynArray; provider: TVSDeclarationProvider) : TVSDisposable;
    function registerHoverProvider(selector: string; provider: TVSHoverProvider) : TVSDisposable;
    function registerHoverProvider(selector: TStringDynArray; provider: TVSHoverProvider) : TVSDisposable;
    function registerHoverProvider(selector: TVSDocumentFilter; provider: TVSHoverProvider) : TVSDisposable;
    function registerHoverProvider(selector: TVSDocumentFilterDynArray; provider: TVSHoverProvider) : TVSDisposable;
    function registerEvaluatableExpressionProvider(selector: string; provider: TVSEvaluatableExpressionProvider) : TVSDisposable;
    function registerEvaluatableExpressionProvider(selector: TStringDynArray; provider: TVSEvaluatableExpressionProvider) : TVSDisposable;
    function registerEvaluatableExpressionProvider(selector: TVSDocumentFilter; provider: TVSEvaluatableExpressionProvider) : TVSDisposable;
    function registerEvaluatableExpressionProvider(selector: TVSDocumentFilterDynArray; provider: TVSEvaluatableExpressionProvider) : TVSDisposable;
    function registerDocumentHighlightProvider(selector: string; provider: TVSDocumentHighlightProvider) : TVSDisposable;
    function registerDocumentHighlightProvider(selector: TStringDynArray; provider: TVSDocumentHighlightProvider) : TVSDisposable;
    function registerDocumentHighlightProvider(selector: TVSDocumentFilter; provider: TVSDocumentHighlightProvider) : TVSDisposable;
    function registerDocumentHighlightProvider(selector: TVSDocumentFilterDynArray; provider: TVSDocumentHighlightProvider) : TVSDisposable;
    function registerDocumentSymbolProvider(selector: string; provider: TVSDocumentSymbolProvider) : TVSDisposable; overload;
    function registerDocumentSymbolProvider(selector: TStringDynArray; provider: TVSDocumentSymbolProvider) : TVSDisposable; overload;
    function registerDocumentSymbolProvider(selector: TVSDocumentFilter; provider: TVSDocumentSymbolProvider) : TVSDisposable; overload;
    function registerDocumentSymbolProvider(selector: TVSDocumentFilterDynArray; provider: TVSDocumentSymbolProvider) : TVSDisposable; overload;
    function registerDocumentSymbolProvider(selector: string; provider: TVSDocumentSymbolProvider; metaData: TVSDocumentSymbolProviderMetadata) : TVSDisposable; overload;
    function registerDocumentSymbolProvider(selector: TStringDynArray; provider: TVSDocumentSymbolProvider; metaData: TVSDocumentSymbolProviderMetadata) : TVSDisposable; overload;
    function registerDocumentSymbolProvider(selector: TVSDocumentFilter; provider: TVSDocumentSymbolProvider; metaData: TVSDocumentSymbolProviderMetadata) : TVSDisposable; overload;
    function registerDocumentSymbolProvider(selector: TVSDocumentFilterDynArray; provider: TVSDocumentSymbolProvider; metaData: TVSDocumentSymbolProviderMetadata) : TVSDisposable; overload;
    function registerWorkspaceSymbolProvider(provider: TVSWorkspaceSymbolProvider) : TVSDisposable;
    function registerReferenceProvider(selector: string; provider: TVSReferenceProvider) : TVSDisposable;
    function registerReferenceProvider(selector: TStringDynArray; provider: TVSReferenceProvider) : TVSDisposable;
    function registerReferenceProvider(selector: TVSDocumentFilter; provider: TVSReferenceProvider) : TVSDisposable;
    function registerReferenceProvider(selector: TVSDocumentFilterDynArray; provider: TVSReferenceProvider) : TVSDisposable;
    function registerRenameProvider(selector: string; provider: TVSRenameProvider) : TVSDisposable;
    function registerRenameProvider(selector: TStringDynArray; provider: TVSRenameProvider) : TVSDisposable;
    function registerRenameProvider(selector: TVSDocumentFilter; provider: TVSRenameProvider) : TVSDisposable;
    function registerRenameProvider(selector: TVSDocumentFilterDynArray; provider: TVSRenameProvider) : TVSDisposable;
    function registerDocumentSemanticTokensProvider(selector: string; provider: TVSDocumentSemanticTokensProvider; legend: TVSSemanticTokensLegend) : TVSDisposable;
    function registerDocumentSemanticTokensProvider(selector: TStringDynArray; provider: TVSDocumentSemanticTokensProvider; legend: TVSSemanticTokensLegend) : TVSDisposable;
    function registerDocumentSemanticTokensProvider(selector: TVSDocumentFilter; provider: TVSDocumentSemanticTokensProvider; legend: TVSSemanticTokensLegend) : TVSDisposable;
    function registerDocumentSemanticTokensProvider(selector: TVSDocumentFilterDynArray; provider: TVSDocumentSemanticTokensProvider; legend: TVSSemanticTokensLegend) : TVSDisposable;
    function registerDocumentRangeSemanticTokensProvider(selector: string; provider: TVSDocumentRangeSemanticTokensProvider; legend: TVSSemanticTokensLegend) : TVSDisposable;
    function registerDocumentRangeSemanticTokensProvider(selector: TStringDynArray; provider: TVSDocumentRangeSemanticTokensProvider; legend: TVSSemanticTokensLegend) : TVSDisposable;
    function registerDocumentRangeSemanticTokensProvider(selector: TVSDocumentFilter; provider: TVSDocumentRangeSemanticTokensProvider; legend: TVSSemanticTokensLegend) : TVSDisposable;
    function registerDocumentRangeSemanticTokensProvider(selector: TVSDocumentFilterDynArray; provider: TVSDocumentRangeSemanticTokensProvider; legend: TVSSemanticTokensLegend) : TVSDisposable;
    function registerDocumentFormattingEditProvider(selector: string; provider: TVSDocumentFormattingEditProvider) : TVSDisposable;
    function registerDocumentFormattingEditProvider(selector: TStringDynArray; provider: TVSDocumentFormattingEditProvider) : TVSDisposable;
    function registerDocumentFormattingEditProvider(selector: TVSDocumentFilter; provider: TVSDocumentFormattingEditProvider) : TVSDisposable;
    function registerDocumentFormattingEditProvider(selector: TVSDocumentFilterDynArray; provider: TVSDocumentFormattingEditProvider) : TVSDisposable;
    function registerDocumentRangeFormattingEditProvider(selector: string; provider: TVSDocumentRangeFormattingEditProvider) : TVSDisposable;
    function registerDocumentRangeFormattingEditProvider(selector: TStringDynArray; provider: TVSDocumentRangeFormattingEditProvider) : TVSDisposable;
    function registerDocumentRangeFormattingEditProvider(selector: TVSDocumentFilter; provider: TVSDocumentRangeFormattingEditProvider) : TVSDisposable;
    function registerDocumentRangeFormattingEditProvider(selector: TVSDocumentFilterDynArray; provider: TVSDocumentRangeFormattingEditProvider) : TVSDisposable;
    function registerOnTypeFormattingEditProvider(selector: string; provider: TVSOnTypeFormattingEditProvider; firstTriggerCharacter: string; moreTriggerCharacter: tstringDynArray) : TVSDisposable;
    function registerOnTypeFormattingEditProvider(selector: TStringDynArray; provider: TVSOnTypeFormattingEditProvider; firstTriggerCharacter: string; moreTriggerCharacter: tstringDynArray) : TVSDisposable;
    function registerOnTypeFormattingEditProvider(selector: TVSDocumentFilter; provider: TVSOnTypeFormattingEditProvider; firstTriggerCharacter: string; moreTriggerCharacter: tstringDynArray) : TVSDisposable;
    function registerOnTypeFormattingEditProvider(selector: TVSDocumentFilterDynArray; provider: TVSOnTypeFormattingEditProvider; firstTriggerCharacter: string; moreTriggerCharacter: tstringDynArray) : TVSDisposable;
    function registerSignatureHelpProvider(selector: string; provider: TVSSignatureHelpProvider; triggerCharacters: tstringDynArray) : TVSDisposable;
    function registerSignatureHelpProvider(selector: TStringDynArray; provider: TVSSignatureHelpProvider; triggerCharacters: tstringDynArray) : TVSDisposable;
    function registerSignatureHelpProvider(selector: TVSDocumentFilter; provider: TVSSignatureHelpProvider; triggerCharacters: tstringDynArray) : TVSDisposable;
    function registerSignatureHelpProvider(selector: TVSDocumentFilterDynArray; provider: TVSSignatureHelpProvider; triggerCharacters: tstringDynArray) : TVSDisposable;
    function registerSignatureHelpProvider(selector: string; provider: TVSSignatureHelpProvider; metadata: TVSSignatureHelpProviderMetadata) : TVSDisposable;
    function registerSignatureHelpProvider(selector: TStringDynArray; provider: TVSSignatureHelpProvider; metadata: TVSSignatureHelpProviderMetadata) : TVSDisposable;
    function registerSignatureHelpProvider(selector: TVSDocumentFilter; provider: TVSSignatureHelpProvider; metadata: TVSSignatureHelpProviderMetadata) : TVSDisposable;
    function registerSignatureHelpProvider(selector: TVSDocumentFilterDynArray; provider: TVSSignatureHelpProvider; metadata: TVSSignatureHelpProviderMetadata) : TVSDisposable;
    function registerDocumentLinkProvider(selector: string; provider: TVSDocumentLinkProvider) : TVSDisposable;
    function registerDocumentLinkProvider(selector: TStringDynArray; provider: TVSDocumentLinkProvider) : TVSDisposable;
    function registerDocumentLinkProvider(selector: TVSDocumentFilter; provider: TVSDocumentLinkProvider) : TVSDisposable;
    function registerDocumentLinkProvider(selector: TVSDocumentFilterDynArray; provider: TVSDocumentLinkProvider) : TVSDisposable;
    function registerColorProvider(selector: string; provider: TVSDocumentColorProvider) : TVSDisposable;
    function registerColorProvider(selector: TStringDynArray; provider: TVSDocumentColorProvider) : TVSDisposable;
    function registerColorProvider(selector: TVSDocumentFilter; provider: TVSDocumentColorProvider) : TVSDisposable;
    function registerColorProvider(selector: TVSDocumentFilterDynArray; provider: TVSDocumentColorProvider) : TVSDisposable;
    function registerFoldingRangeProvider(selector: string; provider: TVSFoldingRangeProvider) : TVSDisposable;
    function registerFoldingRangeProvider(selector: TStringDynArray; provider: TVSFoldingRangeProvider) : TVSDisposable;
    function registerFoldingRangeProvider(selector: TVSDocumentFilter; provider: TVSFoldingRangeProvider) : TVSDisposable;
    function registerFoldingRangeProvider(selector: TVSDocumentFilterDynArray; provider: TVSFoldingRangeProvider) : TVSDisposable;
    function registerSelectionRangeProvider(selector: string; provider: TVSSelectionRangeProvider) : TVSDisposable;
    function registerSelectionRangeProvider(selector: TStringDynArray; provider: TVSSelectionRangeProvider) : TVSDisposable;
    function registerSelectionRangeProvider(selector: TVSDocumentFilter; provider: TVSSelectionRangeProvider) : TVSDisposable;
    function registerSelectionRangeProvider(selector: TVSDocumentFilterDynArray; provider: TVSSelectionRangeProvider) : TVSDisposable;
    function registerCallHierarchyProvider(selector: string; provider: TVSCallHierarchyProvider) : TVSDisposable;
    function registerCallHierarchyProvider(selector: TStringDynArray; provider: TVSCallHierarchyProvider) : TVSDisposable;
    function registerCallHierarchyProvider(selector: TVSDocumentFilter; provider: TVSCallHierarchyProvider) : TVSDisposable;
    function registerCallHierarchyProvider(selector: TVSDocumentFilterDynArray; provider: TVSCallHierarchyProvider) : TVSDisposable;
    function registerLinkedEditingRangeProvider(selector: string; provider: TVSLinkedEditingRangeProvider) : TVSDisposable;
    function registerLinkedEditingRangeProvider(selector: TStringDynArray; provider: TVSLinkedEditingRangeProvider) : TVSDisposable;
    function registerLinkedEditingRangeProvider(selector: TVSDocumentFilter; provider: TVSLinkedEditingRangeProvider) : TVSDisposable;
    function registerLinkedEditingRangeProvider(selector: TVSDocumentFilterDynArray; provider: TVSLinkedEditingRangeProvider) : TVSDisposable;
    function setLanguageConfiguration(language: string; configuration: TVSLanguageConfiguration) : TVSDisposable;
  end;


  TVSSourceControlInputBox  = class external name 'vscode.TVSSourceControlInputBox' (TJSObject)
  Public
    value: string;
    placeholder: string;
    visible: boolean;
  end;


  TVSQuickDiffProvider  = class external name 'Object' (TJSObject)
  Public
    function provideOriginalResource(uri: TVSUri; token: TVSCancellationToken) : JSValue;
  end;


  TVSSourceControlResourceThemableDecorations  = class external name 'vscode.SourceControlResourceThemableDecorations' (TJSObject)
  Private
    FiconPath  : string;
    FiconPathObj: TVSUri; external name 'iconPath';
  Public
    Property iconPath : string read Ficonpath;
    Property iconPathObj : TVSUri read FiconPathObj;
  end;


  TVSSourceControlResourceDecorations   = class external name 'vscode.SourceControlResourceDecorations' (TVSSourceControlResourceThemableDecorations)
  Private
    FstrikeThrough : boolean; external name 'strikeThrough';
    Ffaded : boolean; external name 'faded';
    Ftooltip : string; external name 'tooltip';
    Flight : TVSSourceControlResourceThemableDecorations; external name 'light';
    Fdark : TVSSourceControlResourceThemableDecorations; external name 'dark';
  Public
    Property strikeThrough : boolean read FstrikeThrough;
    Property faded : boolean read Ffaded;
    Property tooltip : string read Ftooltip;
    Property light : TVSSourceControlResourceThemableDecorations read Flight;
    Property dark : TVSSourceControlResourceThemableDecorations read Fdark;
  end;


  TVSSourceControlResourceState  = class external name 'vscode.SourceControlResourceState' (TJSObject)
  Private
    FresourceUri : TVSUri; external name 'resourceUri';
    Fcommand : TVSCommand; external name 'command';
    Fdecorations : TVSSourceControlResourceDecorations; external name 'decorations';
    FcontextValue : string; external name 'contextValue';
  Public
    Property resourceUri : TVSUri read FresourceUri;
    Property command : TVSCommand read Fcommand;
    Property decorations : TVSSourceControlResourceDecorations read Fdecorations;
    Property contextValue : string read FcontextValue;
  end;


  TVSSourceControlResourceGroup  = class external name 'vscode.SourceControlResourceGroup' (TJSObject)
  Private
    Fid : string; external name 'id';
  Public
    label_: string; external name 'label';
    hideWhenEmpty: boolean;
    resourceStates: TVSSourceControlResourceStateDynArray;
    procedure dispose() ;
    Property id : string read Fid;
  end;


  TVSSourceControl  = class external name 'vscode.SourceControl' (TJSObject)
  Private
    Fid : string; external name 'id';
    Flabel : string; external name 'label';
    FrootUri : TVSUri ;external name 'rootUri';
    FinputBox : TVSSourceControlInputBox; external name 'inputBox';
  Public
    Property id : string read Fid;
    Property label_ : string read Flabel;
    Property rootUri : TVSUri read FrootUri;
    Property inputBox : TVSSourceControlInputBox read FinputBox;
    count: NativeInt;
    quickDiffProvider: TVSQuickDiffProvider;
    commitTemplate: string;
    acceptInputCommand: TVSCommand;
    statusBarCommands: TVSCommandDynArray;
    function createResourceGroup(id: string; alabel: string) : TVSSourceControlResourceGroup;
    procedure dispose() ;
  end;


  TVSNSscm  = class external name 'Object' (TJSObject)
    finputBox: TVSSourceControlInputBox; external name 'inputBox';
  Public
    property inputBox: TVSSourceControlInputBox read finputbox;
    function createSourceControl(id: string; alabel: string) : TVSSourceControl; overload;
    function createSourceControl(id: string; alabel: string; rootUri: TVSUri) : TVSSourceControl; overload;
  end;


  TVSDebugProtocolMessage  = class external name 'vscode.DebugProtocolMessage' (TJSObject)
  Public
    // Properties: see details [here](https://microsoft.github.io/debug-adapter-protocol/specification#Base_Protocol_ProtocolMessage).
  end;


  TVSDebugProtocolSource  = class external name 'vscode.DebugProtocolSource' (TJSObject)
  Public
    // Properties: see details [here](https://microsoft.github.io/debug-adapter-protocol/specification#Types_Source).
  end;


  TVSDebugProtocolBreakpoint  = class external name 'vscode.DebugProtocolBreakpoint' (TJSObject)
  Public
    // Properties: see details [here](https://microsoft.github.io/debug-adapter-protocol/specification#Types_Breakpoint).
  end;


  TVSDebugConfiguration  = class external name 'vscode.TVSDebugConfiguration' (TJSObject)
  Public
    type_: string; external name 'type';
    name: string;
    request: string;
  end;


  TVSDebugSession  = class external name 'vscode.TVSDebugSession' (TJSObject)
  Private
    Fid : string; external name 'id';
    Ftype : string; external name 'type';
    FworkspaceFolder : TVSWorkspaceFolder ;external name 'workspaceFolder';
    Fconfiguration : TVSDebugConfiguration; external name 'configuration';
  Public
    name: string;
    function customRequest(command: string) : TVSThenable; // any overload;
    function customRequest(command: string; args: JSValue) : TVSThenable; // any overload;
    function getDebugProtocolBreakpoint(breakpoint: TVSBreakpoint) : TVSThenable; // DebugProtocolBreakpoint | undefined
    Property id : string read Fid;
    Property type_ : string read Ftype;
    Property workspaceFolder : TVSWorkspaceFolder read FworkspaceFolder;
    Property configuration : TVSDebugConfiguration read Fconfiguration;
  end;


  TVSDebugSessionCustomEvent  = class external name 'vscode.DebugSessionCustomEvent' (TJSObject)
  Private
    Fsession : TVSDebugSession; external name 'session';
    Fevent : string; external name 'event';
    Fbody : JSValue; external name 'body';
  Public
    Property session : TVSDebugSession read Fsession;
    Property event : string read Fevent;
    Property body : JSValue read Fbody;
  end;


  TVSDebugConfigurationProvider  = class external name 'vscode.DebugConfigurationProvider' (TJSObject)
  Public
    function provideDebugConfigurations() : JSValue; overload;
    function provideDebugConfigurations(folder: TVSWorkspaceFolder) : JSValue  overload;
    function provideDebugConfigurations(folder: TVSWorkspaceFolder; token: TVSCancellationToken) : JSValue; overload;
    function resolveDebugConfiguration(folder: TVSWorkspaceFolder; debugConfiguration: TVSDebugConfiguration) : JSValue; overload;
    function resolveDebugConfiguration(folder: TVSWorkspaceFolder; debugConfiguration: TVSDebugConfiguration; token: TVSCancellationToken) : JSValue; overload;
    function resolveDebugConfigurationWithSubstitutedVariables(folder: TVSWorkspaceFolder; debugConfiguration: TVSDebugConfiguration) : JSValue; overload;
    function resolveDebugConfigurationWithSubstitutedVariables(folder: TVSWorkspaceFolder; debugConfiguration: TVSDebugConfiguration; token: TVSCancellationToken) : JSValue; overload;
  end;


  TVSDebugAdapterExecutable  = class external name 'vscode.DebugAdapterExecutable' (TJSObject)
  Private
    Fcommand : string; external name 'command';
    Fargs : TstringDynArray; external name 'args';
    Foptions : TVSDebugAdapterExecutableOptions; external name 'options';
  Public
    constructor New(command: string); overload;
    constructor New(command: string; args: tstringDynArray); overload;
    constructor New(command: string; args: tstringDynArray; options: TVSDebugAdapterExecutableOptions); overload;
    Property command : string read Fcommand;
    Property args : TstringDynArray read Fargs;
    Property options : TVSDebugAdapterExecutableOptions read Foptions;
  end;


  TVSDebugAdapterExecutableOptions  = class external name 'vscode.DebugAdapterExecutableOptions' (TJSObject)
  Public
    env: TJSObject;
    cwd: string;
  end;


  TVSDebugAdapterServer  = class external name 'vscode.DebugAdapterServer' (TJSObject)
  Private
    Fport : NativeInt; external name 'port';
    Fhost : string; external name 'host';
  Public
    Property port : NativeInt read Fport;
    Property host : string read Fhost;
    constructor New(aport: NativeInt); overload;
    constructor New(aport: NativeInt; ahost: string); overload;
  end;


  TVSDebugAdapterNamedPipeServer  = class external name 'vscode.DebugAdapterNamedPipeServer' (TJSObject)
  Private
    Fpath : string; external name 'path';
  Public
    Property path : string read Fpath;
    constructor New(path: string);
  end;


  TVSDebugAdapter  = class external name 'vscode.DebugAdapter' (TVSDisposable)
  Public
    function onDidSendMessage(aHandler : TVSDebugProtocolMessageHandler) : TVSDisposable; overload;
    function onDidSendMessage(aHandler : TVSDebugProtocolMessageHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidSendMessage(aHandler : TVSDebugProtocolMessageHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    procedure handleMessage(message: TVSDebugProtocolMessage) ;
  end;


  TVSDebugAdapterInlineImplementation  = class external name 'vscode.DebugAdapterInlineImplementation' (TJSObject)
  Public
    constructor New(aimplementation: TVSDebugAdapter);
  end;


  // export type DebugAdapterDescriptor = DebugAdapterExecutable | DebugAdapterServer | DebugAdapterNamedPipeServer | DebugAdapterInlineImplementation;
  TVSDebugAdapterDescriptor = TJSObject;
  TVSDebugAdapterDescriptorFactory  = class external name 'vscode.DebugAdapterDescriptorFactory' (TJSObject)
  Public
    function createDebugAdapterDescriptor(session: TVSDebugSession; executable: TVSDebugAdapterExecutable) : JSValue;
  end;


  TVSDebugAdapterTracker  = class external name 'vscode.DebugAdapterTracker' (TJSObject)
  Public
    procedure onWillStartSession() ;
    procedure onWillReceiveMessage(message: JSValue) ;
    procedure onDidSendMessage(message: JSValue) ;
    procedure onWillStopSession() ;
    procedure onError(error: TJSError) ;
    procedure onExit(code: NativeInt; signal: string) ;
    procedure onExit(code: NativeInt) ;
  end;


  TVSDebugAdapterTrackerFactory  = class external name 'vscode.DebugAdapterTrackerFactory' (TJSObject)
  Public
    function createDebugAdapterTracker(session: TVSDebugSession) : JSValue;
  end;


  TVSDebugConsole  = class external name 'vscode.DebugConsole' (TJSObject)
  Public
    procedure append(value: string) ;
    procedure appendLine(value: string) ;
  end;


  TVSBreakpointsChangeEvent  = class external name 'vscode.BreakpointsChangeEvent' (TJSObject)
  Private
    Fadded : TVSBreakpointDynArray; external name 'added';
    Fremoved : TVSBreakpointDynArray; external name 'removed';
    Fchanged : TVSBreakpointDynArray; external name 'changed';
  Public
    Property added : TVSBreakpointDynArray read Fadded;
    Property removed : TVSBreakpointDynArray read Fremoved;
    Property changed : TVSBreakpointDynArray read Fchanged;
  end;


  TVSBreakpoint  = class external name 'vscode.Breakpoint' (TJSObject)
  Private
    Fid : string; external name 'id';
    Fenabled : boolean; external name 'enabled';
    Fcondition : string; external name 'condition';
    FhitCondition : string; external name 'hitCondition';
    FlogMessage : string; external name 'logMessage';
  Public
    Property id : string read Fid;
    Property enabled : boolean read Fenabled;
    Property condition : string read Fcondition;
    Property hitCondition : string read FhitCondition;
    Property logMessage : string read FlogMessage;
    protected constructor New(); overload;
    protected constructor New(enabled: boolean); overload;
    protected constructor New(enabled: boolean; condition: string); overload;
    protected constructor New(enabled: boolean; condition: string; hitCondition: string); overload;
    protected constructor New(enabled: boolean; condition: string; hitCondition: string; logMessage: string); overload;
  end;


  TVSSourceBreakpoint = class external name 'vscode.SourceBreakpoint' (TVSBreakpoint)
  Private
    Flocation : TVSLocation; external name 'location';
  Public
    Property location : TVSLocation read Flocation;
    constructor New(location: TVSLocation); overload;
    constructor New(location: TVSLocation; enabled: boolean); overload;
    constructor New(location: TVSLocation; enabled: boolean; condition: string); overload;
    constructor New(location: TVSLocation; enabled: boolean; condition: string; hitCondition: string); overload;
    constructor New(location: TVSLocation; enabled: boolean; condition: string; hitCondition: string; logMessage: string); overload;
  end;


  TVSFunctionBreakpoint  = class external name 'vscode.FunctionBreakpoint' (TVSBreakpoint)
  Private
   FfunctionName : string; external name 'functionName';
  Public
    Property functionName : string read FfunctionName;
    constructor New(functionName: string); overload;
    constructor New(functionName: string; enabled: boolean); overload;
    constructor New(functionName: string; enabled: boolean; condition: string); overload;
    constructor New(functionName: string; enabled: boolean; condition: string; hitCondition: string); overload;
    constructor New(functionName: string; enabled: boolean; condition: string; hitCondition: string; logMessage: string); overload;
  end;





  TVSNSextensions  = class external name 'Object' (TJSObject)
  Private
    fall: TVSExtensionDynArray; external name 'all';
  Public
    function getExtension(extensionId: string) : TVSExtension ;
    function onDidChange(aHandler : TVSvoidHandler) : TVSDisposable; overload;
    function onDidChange(aHandler : TVSvoidHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChange(aHandler : TVSvoidHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
    property all: TVSExtensionDynArray read FAll;
  end;

  TVSCommentThread  = class external name 'vscode.CommentThread' (TJSObject)
  Private
    Furi : TVSUri; external name 'uri';
  Public
    Property uri : TVSUri read Furi;
    range: TVSRange;
    comments: TVSCommentDynArray;
    collapsibleState: TVSCommentThreadCollapsibleState;
    canReply: boolean;
    contextValue: string;
    label_: string; external name 'label';
    procedure dispose() ;
  end;


  TVSCommentAuthorInformation  = class external name 'vscode.CommentAuthorInformation' (TJSObject)
  Public
    name: string;
    iconPath: TVSUri;
  end;


  TVSCommentReaction  = class external name 'vscode.CommentReaction' (TJSObject)
  Private
    Flabel : string; external name 'label';
    FiconPath  : string; external name 'iconPath';
    FiconPathObj: TVSUri;  external name 'iconPath';
    Fcount : NativeInt; external name 'count';
    FauthorHasReacted : boolean; external name 'authorHasReacted';
  Public
    Property label_ : string read Flabel;
    Property iconPath : string read FiconPath;
    Property iconPathObj : TVSUri read FiconPathObj;
    Property count : NativeInt read Fcount;
    Property authorHasReacted : boolean read FauthorHasReacted;
  end;


  TVSComment  = class external name 'vscode.Comment' (TJSObject)
  Public
    body: string;
    bodyObj : TVSMarkdownString; external name 'body';
    mode: TVSCommentMode;
    author: TVSCommentAuthorInformation;
    contextValue: string;
    reactions: TVSCommentReactionDynArray;
    label_: string; external name 'label';
  end;


  TVSCommentReply  = class external name 'vscode.CommentReply' (TJSObject)
  Public
    thread: TVSCommentThread;
    text: string;
  end;


  TVSCommentingRangeProvider  = class external name 'vscode.CommentingRangeProvider' (TJSObject)
  Public
    function provideCommentingRanges(document: TVSTextDocument; token: TVSCancellationToken) : JSValue;
  end;


  TVSCommentOptions  = class external name 'vscode.CommentOptions' (TJSObject)
  Public
    prompt: string;
    placeHolder: string;
  end;

  TVSCommentReactionHandler = reference to function  (comment: TVSComment; reaction: TVSCommentReaction) : TJSPromise;

  TVSCommentController  = class external name 'vscode.CommentController' (TJSObject)
  Private
    Fid : string; external name 'id';
    Flabel : string; external name 'label';
  Public
    options: TVSCommentOptions;
    commentingRangeProvider: TVSCommentingRangeProvider;
    reactionHandler: TVSCommentReactionHandler;
    function createCommentThread(uri: TVSUri; range: TVSRange; comments: TVSCommentDynArray) : TVSCommentThread;
    procedure dispose() ;
    Property id : string read Fid;
    Property label_ : string read Flabel;
  end;


  TVSNScomments =  class external name 'Object' (TJSObject)
  Public
    function createCommentController(id: string; alabel: string) : TVSCommentController;
  end;


  TVSAuthenticationSession  = class external name 'vscode.AuthenticationSession' (TJSObject)
  Private
    Fid : string; external name 'id';
    FaccessToken : string; external name 'accessToken';
    Faccount : TVSAuthenticationSessionAccountInformation; external name 'account';
    Fscopes : TStringDynArray; external name 'scopes';
  Public
    Property id : string read Fid;
    Property accessToken : string read FaccessToken;
    Property account : TVSAuthenticationSessionAccountInformation read Faccount;
    Property scopes : TStringDynArray read Fscopes;
  end;


  TVSAuthenticationSessionAccountInformation  = class external name 'vscode.AuthenticationSessionAccountInformation' (TJSObject)
  Private
    Fid : string; external name 'id';
    Flabel : string; external name 'label';
  Public
    Property id : string read Fid;
    Property label_ : string read Flabel;
  end;


  TVSAuthenticationGetSessionOptions  = class external name 'vscode.AuthenticationGetSessionOptions' (TJSObject)
  Public
    createIfNone: boolean;
    clearSessionPreference: boolean;
  end;


  TVSAuthenticationProviderInformation  = class external name 'vscode.AuthenticationProviderInformation' (TJSObject)
  Private
    Fid : string; external name 'id';
    Flabel : string; external name 'label';
  Public
    Property id : string read Fid;
    Property label_ : string read Flabel;
  end;


  TVSAuthenticationSessionsChangeEvent  = class external name 'vscode.AuthenticationSessionsChangeEvent' (TJSObject)
  private
    Fprovider : TVSAuthenticationProviderInformation; external name 'provider';
  Public
    Property provider : TVSAuthenticationProviderInformation read Fprovider;
  end;


  TVSNSauthentication  = class external name 'Object' (TJSObject)
  Public
    function getSession(providerId: string; scopes: tstringDynArray; options: TVSAuthenticationGetSessionOptions): TVSThenable; // AuthenticationSession
    function getSession(providerId: string; scopes: tstringDynArray) : TVSThenable; // AuthenticationSession overload;
    function onDidChangeSessions(aHandler : TVSAuthenticationSessionsChangeEventHandler) : TVSDisposable; overload;
    function onDidChangeSessions(aHandler : TVSAuthenticationSessionsChangeEventHandler; aThis : JSvalue): TVSDisposable; overload;
    function onDidChangeSessions(aHandler : TVSAuthenticationSessionsChangeEventHandler; aThis : JSvalue; aDisposables : TVSDisposableArray): TVSDisposable; overload;
  end;


  TVSCode = class external name 'Object' (TJSObject)
  Private
    FVersion : string; external name 'version';
    Ftasks : TVSNStasks; external name 'tasks';
    Fenv : TVSNSenv; external name 'env';
    Fcommands : TVSNScommands; external name 'commands';
    Fwindow : TVSNSwindow; external name 'window';
    Fworkspace : TVSNSworkspace; external name 'workspace';
    Flanguages : TVSNSlanguages; external name 'languages';
    Fscm : TVSNSscm; external name 'scm';
    Fextensions : TVSNSextensions; external name 'extensions';
    Fcomments : TVSNSComments; external name 'comments';
    Fauthentication : TVSNSauthentication; external name 'authentication';
  Public
    Property version : string Read FVersion;
    property tasks : TVSNStasks read Ftasks;
    property env : TVSNSenv read Fenv;
    property commands : TVSNScommands read Fcommands;
    property window : TVSNSwindow read Fwindow;
    property workspace : TVSNSworkspace read Fworkspace;
    property languages : TVSNSlanguages read Flanguages;
    property scm : TVSNSscm read Fscm;
    property extensions : TVSNSextensions read Fextensions;
    property comments : TVSNSComments read fcomments;
    property authentication : TVSNSauthentication read Fauthentication;

  end;

Var
  vscode : TVSCode; external name 'vscode';

implementation

end.
