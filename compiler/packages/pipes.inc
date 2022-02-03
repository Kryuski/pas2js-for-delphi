{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by Michael Van Canneyt

    Win part of pipe stream.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

uses windows;

resourcestring
  SStreamNoWriting              = 'Writing to %s is not supported';
  SStreamNoReading              = 'Reading from %s is not supported';
  SStreamInvalidSeek            = 'Invalid stream operation %s.Seek';

Const piInheritablePipe : TSecurityAttributes = (
                           nlength:SizeOF(TSecurityAttributes);
                           lpSecurityDescriptor:Nil;
                           Binherithandle:True);
      piNonInheritablePipe : TSecurityAttributes = (
                             nlength:SizeOF(TSecurityAttributes);
                             lpSecurityDescriptor:Nil;
                             Binherithandle:False);


      PipeBufSize = 1024;


Function CreatePipeHandles (Var Inhandle,OutHandle : THandle; APipeBufferSize : Cardinal = PipeBufSize) : Boolean;

begin
  Result := CreatePipe (Inhandle,OutHandle,@piNonInheritablePipe,APipeBufferSize);
end;


Function TInputPipeStream.GetNumBytesAvailable: DWord;
begin
  if not PeekNamedPipe(Handle, nil, 0, nil, @Result, nil) then
    Result := 0;
end;

function TInputPipeStream.GetPosition: Int64;
begin
  Result:=FPos;
end;

procedure TInputPipeStream.InvalidSeek;
begin
  Raise EPipeSeek.Create (ENoSeekMsg);
end;

procedure TInputPipeStream.FakeSeekForward(Offset: Int64;  const Origin: TSeekOrigin; const Pos: Int64);
begin
  if Origin=soBeginning then
     Dec(Offset,Pos);
  if (Offset<0) or (Origin=soEnd) then
    InvalidSeek;
  if Offset>0 then
    Discard(Offset);
end;

procedure TInputPipeStream.Discard(const Count: Int64);
const
  CSmallSize      =255;
  CLargeMaxBuffer =32*1024; // 32 KiB
var
  Buffer: array[1..CSmallSize] of Byte;
begin
  if Count=0 then
    Exit;
  if Count<=SizeOf(Buffer) then
    ReadBuffer(Buffer,Count)
  else
    DiscardLarge(Count,CLargeMaxBuffer);
end;

procedure TInputPipeStream.DiscardLarge(Count: int64; const MaxBufferSize: Longint);
var
  Buffer: array of Byte;
begin
  if Count=0 then
     Exit;
  if Count>MaxBufferSize then
    SetLength(Buffer,MaxBufferSize)
  else
    SetLength(Buffer,Count);
  while (Count>=Length(Buffer)) do
    begin
    ReadBuffer(Buffer[0],Length(Buffer));
    Dec(Count,Length(Buffer));
    end;
  if Count>0 then
    ReadBuffer(Buffer[0],Count);
end;

procedure TOutputPipeStream.ReadNotImplemented;
begin
  raise EStreamError.CreateFmt(SStreamNoReading, [ClassName]);
end;

procedure TOutputPipeStream.InvalidSeek;
begin
  raise EStreamError.CreateFmt(SStreamInvalidSeek, [ClassName]);
end;

procedure PipeClose (const FHandle: THandle); inline;
begin
  FileClose(FHandle);
end;
