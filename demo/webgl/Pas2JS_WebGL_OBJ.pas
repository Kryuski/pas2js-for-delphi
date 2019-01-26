program Pas2JS_WebGL_OBJ;
uses
	Types, Mat4, MemoryBuffer, GLUtils, GLTypes, SysUtils,
	BrowserConsole, Web, WebGL, JS, Math;

var
	gl: TJSWebGLRenderingContext;
	shader: TShader;
  projTransform: TMat4;
  viewTransform: TMat4;
  modelTransform: TMat4;

var
	dragonModel: TModel = nil;
	rotateAngle: double = 0;
	deltaTime: TJSFloat32 = 0;
	nextTime: TJSFloat32 = 0;

procedure DrawCanvas;
begin
	gl.clear(gl.COLOR_BUFFER_BIT + gl.DEPTH_BUFFER_BIT);

	if dragonModel <> nil then
		begin
			modelTransform := TMat4.Identity;
			modelTransform := modelTransform.Multiply(TMat4.RotateY(DegToRad(rotateAngle)));

			shader.SetUniformMat4('modelTransform', modelTransform);

			dragonModel.Draw;
		end;
end;

procedure AnimateCanvas(time: TJSDOMHighResTimeStamp);
var
	now: TJSFloat32;
begin
	now := time * 0.001;
	deltaTime := now - nextTime;
	nextTime := now;
	rotateAngle := rotateAngle + (20 * deltaTime);

	DrawCanvas;

	window.requestAnimationFrame(@AnimateCanvas);
end;

procedure StartAnimatingCanvas;
begin
	window.requestAnimationFrame(@AnimateCanvas);
end;

type
	TModelLoader = class
		public
			constructor Create (context: TJSWebGLRenderingContext; path: string);
		private
			gl: TJSWebGLRenderingContext; 
			request: TJSXMLHttpRequest;

			procedure HandleLoaded;
	end;

procedure TModelLoader.HandleLoaded;
var
	data: TModelData;
begin
	if (request.readyState = 4) and (request.status = 200) and (length(request.responseText) > 0) then
		begin
			data := LoadOBJFile(TJSString(request.responseText));
			dragonModel := TModel.Create(gl, data);
			StartAnimatingCanvas;
		end;
end;

constructor TModelLoader.Create (context: TJSWebGLRenderingContext; path: string);
begin
	gl := context;

	request := TJSXMLHttpRequest.new;
	request.open('GET', path);
	request.overrideMimeType('application/text');
	request.onreadystatechange := TJSOnReadyStateChangeHandler(@HandleLoaded);
	request.send;
end;

var
  canvas: TJSHTMLCanvasElement;
  vertexShaderSource: string;
  fragmentShaderSource: string;
begin

	// make webgl context
  canvas := TJSHTMLCanvasElement(document.createElement('canvas'));
  canvas.width := 600;
  canvas.height := 600;
	document.body.appendChild(canvas);

	gl := TJSWebGLRenderingContext(canvas.getContext('webgl'));
	if gl = nil then
		begin
			writeln('failed to load webgl!');
			exit;
		end;

	// create shaders from source in html
	// TODO: move these to .glsl files so error messages make more sense
	// and give valid line numbers
	vertexShaderSource := document.getElementById('vertex.glsl').textContent;
	fragmentShaderSource := document.getElementById('fragment.glsl').textContent;

	shader := TShader.Create(gl, vertexShaderSource, fragmentShaderSource);
	shader.Compile;
  shader.BindAttribLocation(0, 'in_position');
  shader.BindAttribLocation(1, 'in_texCoord');
  shader.BindAttribLocation(2, 'in_normal');
	shader.Link;
	shader.Use;

	// prepare context
	gl.clearColor(0.9, 0.9, 0.9, 1);
	gl.viewport(0, 0, canvas.width, canvas.height);
	gl.clear(gl.COLOR_BUFFER_BIT);

	gl.enable(gl.DEPTH_TEST);
	gl.enable(gl.BLEND);
	gl.Enable(gl.CULL_FACE);
	gl.CullFace(gl.BACK);

	// set projection transform
	projTransform := TMat4.Perspective(60.0, canvas.width / canvas.height, 0.1, 2000);
	shader.SetUniformMat4('projTransform', projTransform);

	// set view transform
	viewTransform := TMat4.Identity;
	viewTransform := viewTransform.Multiply(TMat4.Translate(0, -3, -20));
	shader.SetUniformMat4('viewTransform', viewTransform);

	// NOTE: webgl glsl doesn't have the inverse function
	// so we need to do this here
	shader.SetUniformMat4('inverseViewTransform', viewTransform.Inverse);

	// lighting
	shader.SetUniformVec3('lightPosition', V3(0, 0, 25));
	shader.SetUniformVec3('lightColor', V3(1, 1, 1));

	// model material
	shader.SetUniformFloat('shineDamper', 10);
	shader.SetUniformFloat('reflectivity', 1);

	// load obj file 
	TModelLoader.Create(gl, 'res/dragon.obj');
end.
