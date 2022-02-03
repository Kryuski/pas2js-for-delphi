{$inline on}

{$IFDEF Windows}
  {$define CaseInsensitiveFilenames}
  {$define HasUNCPaths}
{$ENDIF}
{$IFDEF darwin}
  {$define CaseInsensitiveFilenames}
{$ENDIF}
{$IF defined(CaseInsensitiveFilenames) or defined(darwin)}
  {$DEFINE NotLiteralFilenames} // e.g. HFS+ normalizes file names
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE UsePChar}
  {$DEFINE HasInt64}
  {.$define UseAnsiStrings}
  {$DEFINE HasStreams}
  {.$DEFINE UTF8_RTL}
  {.$DEFINE HasStdErr}
  {$DEFINE HasPas2jsFiler}
  {$define HasJsonParser}
{$ENDIF}

{$IF DEFINED(FPC) OR DEFINED(NODEJS)}
  {$DEFINE HASFILESYSTEM}
  {$define HasFS}
  {$define HasFileWriter}
{$ENDIF}

{$IFDEF CPU32BITS}
  {$DEFINE CPU32}
{$ENDIF}

{$IFDEF CPU64BITS}
  {$DEFINE CPU64}
{$ENDIF}

{$WARN WIDECHAR_REDUCED OFF} // W1050 WideChar reduced to byte char in set expressions

// As of XE5 and newer, this needs to be in each individual unit
// for which you want to disable RTTI
{$IFDEF UNICODE}
{$WEAKLINKRTTI ON}
{$IF DECLARED(TVisibilityClasses)}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$IFEND}
{$ENDIF}
