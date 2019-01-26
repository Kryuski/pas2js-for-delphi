program Pas2JS_WebGL_Minimal;
uses
	GLUtils, GLTypes, SysUtils,
	BrowserConsole, Web, WebGL, JS, Math;

var
	gl: TJSWebGLRenderingContext;
	shader: TShader;
var
  canvas: TJSHTMLCanvasElement;
  stride: integer;
  offset: integer;
  vertexShaderSource: string;
  fragmentShaderSource: string;
  buffer: TJSWebGLBuffer;
  verts: TJSArray;
begin
	
	// https://webglfundamentals.org/webgl/lessons/webgl-fundamentals.html

	// make webgl context
  canvas := TJSHTMLCanvasElement(document.createElement('canvas'));
  canvas.width := 300;
  canvas.height := 300;
	document.body.appendChild(canvas);

	gl := TJSWebGLRenderingContext(canvas.getContext('webgl'));
	if gl = nil then
		begin
			writeln('failed to load webgl!');
			exit;
		end;

	// create shaders from source in html
	vertexShaderSource := document.getElementById('vertex.glsl').textContent;
	fragmentShaderSource := document.getElementById('fragment.glsl').textContent;

	shader := TShader.Create(gl, vertexShaderSource, fragmentShaderSource);
	shader.Compile;
	shader.Link;
	shader.Use;

	// prepare context
	gl.clearColor(0.9, 0.9, 0.9, 1);
	gl.viewport(0, 0, canvas.width, canvas.height);
	gl.clear(gl.COLOR_BUFFER_BIT);

	// create verticies in OpenGL coords
	verts := TJSArray.new(
		0, 0,		// 1
		0, 0.5,	// 2
		0.7, 0	// 3
		);

	// create buffer
	buffer := gl.createBuffer;
	gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
  gl.bufferData(gl.ARRAY_BUFFER, TJSFloat32Array.new(TJSObject(verts)), gl.STATIC_DRAW);

  // describe vertex data for buffer
	offset := 0;  
	stride := 4 * 2; // float * 2 = Vec2

	gl.enableVertexAttribArray(0);
	gl.vertexAttribPointer(0, 2, gl.FLOAT, false, stride, offset);
	gl.drawArrays(gl.TRIANGLES, 0, 3);
end.
