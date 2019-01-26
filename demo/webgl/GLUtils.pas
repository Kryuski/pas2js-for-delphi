unit GLUtils;
interface
uses
	MemoryBuffer, Mat4, GLTypes,
	BrowserConsole, WebGL, JS,
	Types, SysUtils;

type
	TShader = class
		public
			constructor Create (context: TJSWebGLRenderingContext; vertexShaderSource, fragmentShaderSource: string);
			procedure Compile;
			procedure Link;
			procedure Use;

			function GetAttribLocation (name: string): GLint;
			procedure BindAttribLocation (index: GLuint; name: string);

			procedure SetUniformMat4 (name: string; value: TMat4);
			procedure SetUniformVec3 (name: string; value: TVec3);
			procedure SetUniformFloat (name: string; value: GLfloat);

		private
			gl: TJSWebGLRenderingContext;
			vertexShader: TJSWebGLShader;
			fragmentShader: TJSWebGLShader;
			programID: TJSWebGLProgram;

			function GetUniformLocation (name: string): TJSWebGLUniformLocation;
			function CreateShader (theType: GLenum; source: string): TJSWebGLShader;
	end;


type
	TModelData = record
		verticies: TJSFloat32Array;		// GLfloat

		// NOTE: it's not clear if WebGL supports GLuint
		// https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/drawElements

		indicies: TJSUint16Array;			// GLushort
		floatsPerVertex: integer;
	end;

const
	kModelVertexFloats = 3 + 2 + 3;
type
	TModelVertex = record
		pos: TVec3;
		texCoord: TVec2;
		normal: TVec3;
	end;

procedure ModelVertexAddToBuffer(vertex: TModelVertex; buffer: TMemoryBuffer);
procedure ModelVertexAddToArray (vertex: TModelVertex; list: TJSArray); 

type
	TModel = class
		public
			constructor Create(context: TJSWebGLRenderingContext; modelData: TModelData); overload;
			procedure Draw;
		private
			gl: TJSWebGLRenderingContext;
			data: TModelData;
			vertexBuffer: TJSWebGLBuffer;
			indexBuffer: TJSWebGLBuffer;
			//elementCount: integer;

			procedure EnableAttributes;
			procedure Load;
	end;

function LoadOBJFile (text: TJSString): TModelData;

function GLSizeof(glType: NativeInt): integer; 
procedure GLFatal (gl: TJSWebGLRenderingContext; messageString: string = 'Fatal OpenGL error'); 

implementation

{=============================================}
{@! ___Utilities___ } 
{=============================================}

procedure Fatal (messageString: string); overload;
begin
	writeln('*** FATAL: ', messageString);
	raise Exception.Create('FATAL');
end;

// TODO: toll free bridge to FPC strings
{procedure Fatal (messageString: TJSString); overload;
begin
	writeln('*** FATAL: ', messageString);
	raise Exception.Create('FATAL');
end;}

procedure GLFatal (gl: TJSWebGLRenderingContext; messageString: string = 'Fatal OpenGL error'); 
var
	error: integer;
begin
	error := gl.getError();
	if error <> TJSWebGLRenderingContext.NO_ERROR then
		begin
			// TODO: case doesn't work?
			case error of
				TJSWebGLRenderingContext.INVALID_VALUE:
					messageString := messageString+' (GL_INVALID_VALUE)';
				TJSWebGLRenderingContext.INVALID_OPERATION:
					messageString := messageString+' (GL_INVALID_OPERATION)';
				TJSWebGLRenderingContext.INVALID_ENUM:
					messageString := messageString+' (GL_INVALID_ENUM)';
				otherwise
					messageString := messageString+' '+IntToStr(error);
			end;
			Fatal(messageString);
		end;
end;

function GLSizeof(glType: NativeInt): integer; 
begin
	case glType of
		TJSWebGLRenderingContext.UNSIGNED_BYTE, TJSWebGLRenderingContext.BYTE:
			result := 1;
		TJSWebGLRenderingContext.SHORT, TJSWebGLRenderingContext.UNSIGNED_SHORT:
			result := 2;
		TJSWebGLRenderingContext.INT, TJSWebGLRenderingContext.UNSIGNED_INT:
			result := 4;
		TJSWebGLRenderingContext.FLOAT:
			result := 4;
		otherwise
			Fatal('GLSizeof type is invalid.');
	end;
end;

{=============================================}
{@! ___Model___ } 
{=============================================}

procedure ModelVertexAddToBuffer(vertex: TModelVertex; buffer: TMemoryBuffer);
begin
	buffer.AddFloats(kModelVertexFloats, [
		vertex.pos.x, vertex.pos.y, vertex.pos.z,
		vertex.texCoord.x, vertex.texCoord.y,
		vertex.normal.x, vertex.normal.y, vertex.normal.z
		]);
end;

procedure ModelVertexAddToArray (vertex: TModelVertex; list: TJSArray); 
begin
	list.push(vertex.pos.x);
	list.push(vertex.pos.y);
	list.push(vertex.pos.z);
	list.push(vertex.texCoord.x);
	list.push(vertex.texCoord.y);
	list.push(vertex.normal.x);
	list.push(vertex.normal.y);
	list.push(vertex.normal.z);
end;

constructor TModel.Create(context: TJSWebGLRenderingContext; modelData: TModelData);
begin
	gl := context;
	data := modelData;
	Load;
end;

procedure TModel.Draw;
begin
	gl.bindBuffer(gl.ARRAY_BUFFER, vertexBuffer);
	gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);

	EnableAttributes;
	gl.drawElements(gl.TRIANGLES, data.indicies.length, gl.UNSIGNED_SHORT, 0);

	gl.bindBuffer(gl.ARRAY_BUFFER, nil);
	gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, nil);
end;

procedure TModel.EnableAttributes;
var
	offset: integer;
	stride: integer;
begin

	// NOTE: we don't have VAO's yet so we need to enable vertex attributes for shader
	// before every draw call (unless the array buffer hasn't changed between calls)
	offset := 0;  
	stride := data.floatsPerVertex * GLSizeof(TJSWebGLRenderingContext.FLOAT);

	// position
	gl.enableVertexAttribArray(0);
	gl.vertexAttribPointer(0, 3, gl.FLOAT, false, stride, offset);
	offset += GLSizeof(TJSWebGLRenderingContext.FLOAT) * 3;

	// texture
	gl.enableVertexAttribArray(1);
	gl.vertexAttribPointer(1, 2, gl.FLOAT, false, stride, offset);
	offset += GLSizeof(TJSWebGLRenderingContext.FLOAT) * 2;

	// normal
	gl.enableVertexAttribArray(2);
	gl.vertexAttribPointer(2, 3, gl.FLOAT, false, stride, offset);
	offset += GLSizeof(TJSWebGLRenderingContext.FLOAT) * 3;
end;

procedure TModel.Load;
begin
	indexBuffer := gl.createBuffer;
	gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
	gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, data.indicies, gl.STATIC_DRAW);	
	gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, nil);

	vertexBuffer := gl.createBuffer;
	gl.bindBuffer(gl.ARRAY_BUFFER, vertexBuffer);
  gl.bufferData(gl.ARRAY_BUFFER, data.verticies, gl.STATIC_DRAW);
	gl.bindBuffer(gl.ARRAY_BUFFER, nil);
end;

type
	TOBJVertex = record
		position: TVec3;
		textureIndex: integer;
		normalIndex: integer;
	end;

function ProcessFace (verticies: TJSArray; indices: TJSArray; face: TStringDynArray): TOBJVertex;
var
	index: integer;
	vertex: TOBJVertex;
begin
	index := StrToInt(face[0]) - 1;

	vertex := TOBJVertex(verticies[index]);

	// NOTE: see TOBJData
	// index can't exceed GLushort
	if index > 65536 then
		Fatal('overflowed indices array');

	if face[1] <> '' then
		vertex.textureIndex := StrToInt(face[1]) - 1
	else
		vertex.textureIndex := -1;

	if face[2] <> '' then
		vertex.normalIndex := StrToInt(face[2]) - 1
	else
		vertex.normalIndex := -1;

	indices.push(index);
	
	verticies[index] := vertex;

	result := vertex;
end;

function LoadOBJFile (text: TJSString): TModelData;
const
	kLineEnding = #10;
	kSpace = ' '; // what code is space?
var
	lines: TStringDynArray;
	parts: TStringDynArray;
	indices: TJSArray;
	positions: TJSArray;
	normals: TJSArray;
	textures: TJSArray;
	verticies: TJSArray;
	mesh: TJSFloat32Array;

	i: integer;
	line: TJSString;
	vertex: TOBJVertex;
	vertexIndex: integer;
	data: TModelData;

	pos: TVec3;
	texCoord: TVec2;
	normal: TVec3;
begin
	positions := TJSArray.new;
	normals := TJSArray.new;
	textures := TJSArray.new;
	indices := TJSArray.new;
	verticies := TJSArray.new;

	lines := text.split(kLineEnding);

	for i := 0 to high(lines) do
		begin
			line := TJSString(lines[i]);
			parts := line.split(kSpace);

			if line.startsWith('v ') then
				begin
					pos := V3(StrToFloat(parts[1]), StrToFloat(parts[2]), StrToFloat(parts[3]));
					positions.push(pos);
					
					// add new vertex
					vertex.position := pos;
					vertex.textureIndex := -1;
					vertex.normalIndex := -1;
					verticies.push(pos);
				end
			else if line.startsWith('vn ') then
				begin
					normals.push(V3(StrToFloat(parts[1]), StrToFloat(parts[2]), StrToFloat(parts[3])));
				end
			else if line.startsWith('vt ') then
				begin
					textures.push(V2(StrToFloat(parts[1]), 1 - StrToFloat(parts[2])));
				end
			else if line.startsWith('f ') then
				begin
					ProcessFace(verticies, indices, TJSString(parts[1]).split('/'));
					ProcessFace(verticies, indices, TJSString(parts[2]).split('/'));
					ProcessFace(verticies, indices, TJSString(parts[3]).split('/'));
				end;
		end;
	
	// vec3 (position) + vec2 (texCoord) + vec3 (normal)
	data.floatsPerVertex := kModelVertexFloats;

	mesh := TJSFloat32Array.New(data.floatsPerVertex * verticies.length);

	for i := 0 to verticies.length - 1 do
		begin
			vertex := TOBJVertex(verticies[i]);

			vertexIndex := i * data.floatsPerVertex;

			// position
			pos := TVec3(positions[i]);
			mesh[vertexIndex + 0] := pos.x;
			mesh[vertexIndex + 1] := pos.y;
			mesh[vertexIndex + 2] := pos.z;

			// texture
			if vertex.textureIndex <> -1 then
				begin
					texCoord := TVec2(textures[vertex.textureIndex]);
					mesh[vertexIndex + 3] := texCoord.x;
					mesh[vertexIndex + 4] := texCoord.y;
				end
			else
				begin
					mesh[vertexIndex + 3] := 0;
					mesh[vertexIndex + 4] := 0;
				end;
			
			// normal
			if vertex.normalIndex <> -1 then
				begin
					normal := TVec3(normals[vertex.normalIndex]);
					mesh[vertexIndex + 5] := normal.x;
					mesh[vertexIndex + 6] := normal.y;
					mesh[vertexIndex + 7] := normal.z;
				end;
		end;

	//writeln('floats: ', mesh.length);
	//writeln('positions:', positions.length);
	//writeln('indices:', indices.length);

	data.verticies := mesh;
	data.indicies := TJSUint16Array.New(TJSObject(indices));

	result := data;
end;

{=============================================}
{@! ___Shader___ } 
{=============================================}
function TShader.GetUniformLocation (name: string): TJSWebGLUniformLocation;
begin
	// TODO: cache these. how do we use dictionarys from JS in Pascal?
	result := gl.getUniformLocation(programID, name);
	GLFatal(gl, 'gl.getUniformLocation');
end;

procedure TShader.SetUniformFloat (name: string; value: GLfloat);
begin
	gl.uniform1f(GetUniformLocation(name), value);
	GLFatal(gl, 'gl.uniform1f');
end;

procedure TShader.SetUniformVec3 (name: string; value: TVec3);
begin
	//gl.uniform3fv(GetUniformLocation(name), ToFloats(value));
	gl.uniform3f(GetUniformLocation(name), value.x, value.y, value.z);
	GLFatal(gl, 'gl.uniform3fv');
end;

procedure TShader.SetUniformMat4 (name: string; value: TMat4);
var
	list: TJSFloat32List;
begin
	// TODO: fix mat4 to use flat arrays
	list := TJSFloat32List(value.CopyList);
	gl.uniformMatrix4fv(GetUniformLocation(name), false, list);
	GLFatal(gl, 'gl.uniformMatrix4fv');
end;

function TShader.GetAttribLocation (name: string): GLint;
begin
	result := gl.getAttribLocation(programID, name);
end;

procedure TShader.BindAttribLocation (index: GLuint; name: string);
begin
	gl.bindAttribLocation(programID, index, name);
	//GLFatal('glBindAttribLocation '+IntToStr(index)+':'+name);
end;

constructor TShader.Create (context: TJSWebGLRenderingContext; vertexShaderSource, fragmentShaderSource: string);
begin
	gl := context;
	vertexShader := CreateShader(gl.VERTEX_SHADER, vertexShaderSource);
	fragmentShader := CreateShader(gl.FRAGMENT_SHADER, fragmentShaderSource);
end;

function TShader.CreateShader(theType: GLenum; source: string): TJSWebGLShader; 
begin
	Result := gl.createShader(theType);
	if Result = nil then
		Fatal('create shader failed');
	gl.shaderSource(Result, source);
	gl.compileShader(Result);
	if gl.getShaderParameter(Result, gl.COMPILE_STATUS) then
		begin
			//writeln('loaded shader ', theType);
			exit;
		end
	else
		begin
			Fatal(gl.getShaderInfoLog(Result));
			//gl.deleteShader(shader);
		end;
end;

procedure TShader.Compile; 
begin
	programID := gl.createProgram;
	gl.attachShader(programID, vertexShader);
	gl.attachShader(programID, fragmentShader);
end;

procedure TShader.Link; 
begin
  gl.linkProgram(programID);
  if not gl.getProgramParameter(programID, gl.LINK_STATUS) then
  	begin
  		Fatal(gl.getProgramInfoLog(programID));
  		//gl.deleteProgram(programID);
  	end;
end;

procedure TShader.Use; 
begin
	gl.useProgram(programID);
end;

end.
