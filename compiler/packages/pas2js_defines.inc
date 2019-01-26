
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
  {$define UseAnsiStrings}
  {$DEFINE HasInt64}
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
{$endif}

{$WARN WIDECHAR_REDUCED OFF} // W1050 WideChar reduced to byte char in set expressions
