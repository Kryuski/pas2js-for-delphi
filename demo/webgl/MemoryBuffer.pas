{$mode objfpc}

unit MemoryBuffer;
interface
uses
	JS;

type
    TMBFloat32 = double;
	TMemoryBuffer = class
		private
			byteBuffer: TJSUint8Array;
		public
		 constructor Create (size: integer);

		 { UInt8 }
		 procedure AddBytes (count: integer; data: array of byte);

		 { Float32 }
		 procedure AddFloats (count: integer; data: array of TMBFloat32);
		 procedure AddFloat (data: TMBFloat32);

		 { UInt16 }
		 procedure AddWords(count: integer; data: array of word);

		 property GetBytes: TJSUint8Array read byteBuffer;
		private
			byteOffset: integer;
			floatBuffer: TJSFloat32Array;
			wordBuffer: TJSUInt16Array;
	end;

implementation

constructor TMemoryBuffer.Create (size: integer);
begin
	byteBuffer := TJSUint8Array.New(size);
end;

procedure TMemoryBuffer.AddBytes (count: integer; data: array of byte);
begin
	//writeln('AddBytes: @', byteOffset, ' -> ', data);
	byteBuffer._set(data, byteOffset);
	byteOffset := byteOffset + (count * 1);
end;

procedure TMemoryBuffer.AddFloat (data: TMBFloat32);
begin
	AddFloats(1, [data]);
end;

procedure TMemoryBuffer.AddFloats (count: integer; data: array of TMBFloat32);
const
	kElementSize = 4;
var
	floatOffset: integer;
begin
	floatOffset := byteOffset div kElementSize;
	//writeln('AddFloats: @', byteOffset, '/', floatOffset, ' -> ', data);

	if floatBuffer = nil then
		floatBuffer := TJSFloat32Array.New(byteBuffer.buffer, 0, byteBuffer.byteLength div kElementSize);

	floatBuffer._set(data, floatOffset);

	byteOffset := byteOffset + (count * kElementSize);
end;

procedure TMemoryBuffer.AddWords(count: integer; data: array of word);
const
	kElementSize = 2;
var
	wordOffset: integer;
begin
	wordOffset := byteOffset div kElementSize;

	if wordBuffer = nil then
		wordBuffer := TJSUInt16Array.New(byteBuffer.buffer, 0, byteBuffer.byteLength div kElementSize);

	wordBuffer._set(data, wordOffset);

	byteOffset := byteOffset + (count * kElementSize);
end;

end.
