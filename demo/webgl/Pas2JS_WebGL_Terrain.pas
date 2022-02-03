program Pas2JS_WebGL_Terrain;
uses
	Terrain, Noise, Types, Mat4, GLUtils, GLTypes, 
	SysUtils,
	BrowserConsole, Web, WebGL, JS, Math;

var
	gl: TJSWebGLRenderingContext;
	shader: TShader;
  projTransform: TMat4;
  viewTransform: TMat4;
  modelTransform: TMat4;

var
  debugConsole: TJSElement;
  canvasAnimationHandler: integer = 0;

var
	maps: TJSArray;
	camera: TVec3;
	lightPosition: TVec3;
  terrainNoise: TNoise;
	terrainSize: integer = 64 * 3;
	terrainResolution: integer = 128;
	flySpeed: TJSFloat32 = 1.3;
	visiblity: integer = 4;
	textureLoaded: boolean = false;

type
	TTilingTerrain = class (TTerrain)
		public
			neighbor: TTilingTerrain;
		protected
			function GetHeightForVertex (localX, localY, x, y: integer): TNoiseFloat; override;
	end;

function TTilingTerrain.GetHeightForVertex (localX, localY, x, y: integer): TNoiseFloat; 
begin		
	if (localY = 0) and (neighbor <> nil) then
		result := neighbor.GetHeightAtPoint(localX, localY + neighbor.GetWidth - 1)
	else
		begin
			result := noise.GetNoise(x, y, GetWidth, GetHeight, 6, 3);
			result := Power(result, 9) * 60;
		end;
end;

procedure DrawCanvas;
var
	terrainCoord: TVec3;
	startIndex, endIndex: integer;
	i: integer;
	map: TTilingTerrain;
begin
	gl.clear(gl.COLOR_BUFFER_BIT + gl.DEPTH_BUFFER_BIT);

	// apply camera to view transform
	viewTransform := TMat4.Identity;
	viewTransform := viewTransform.Multiply(TMat4.Translate(camera.x, camera.y, camera.z));
	shader.SetUniformMat4('viewTransform', viewTransform);
	shader.SetUniformMat4('inverseViewTransform', viewTransform.Inverse);

	// move light with camera
	lightPosition.z += flySpeed;
	shader.SetUniformVec3('lightPosition', lightPosition);

	// animate camera
	camera.z -= flySpeed;
	camera.y := -(terrainSize/4) + Sin(camera.z / terrainSize) * 14;
	camera.x := -(terrainSize/2) + Cos(camera.z / terrainSize) * 20;
	terrainCoord := Divide(camera, terrainSize);

	endIndex := Trunc(Abs(terrainCoord.z));
	startIndex := endIndex - visiblity;
	if startIndex < 0 then
		startIndex := 0;

	//debugConsole.innerHTML := IntToStr(startIndex)+'/'+IntToStr(endIndex) + VecStr(terrainCoord);

	for i := startIndex to endIndex do
		begin
			if (maps.length = 0) or ((maps.length = i) and (maps[i] = nil)) then
				begin
					map := TTilingTerrain.Create(gl, terrainNoise, terrainSize, terrainResolution, V2(0, terrainSize * i));
					if (i - 1) >= 0 then
						map.neighbor := TTilingTerrain(maps[i - 1]);
					map.Generate;

					maps.push(map);

					// NOTE: does this free memory in JS?
					if startIndex - 1 >= 0 then
						maps[startIndex - 1] := nil;
				end;

			map := TTilingTerrain(maps[i]);
			modelTransform := TMat4.Identity;
			modelTransform := modelTransform.Multiply(TMat4.Translate(0, 0, terrainSize * i));
			shader.SetUniformMat4('modelTransform', modelTransform);
			map.Draw;
		end;
end;

procedure AnimateCanvas(time: TJSDOMHighResTimeStamp);
begin
	if textureLoaded then
		DrawCanvas;

	if canvasAnimationHandler <> 0 then
		canvasAnimationHandler := window.requestAnimationFrame(@AnimateCanvas);
end;

procedure StartAnimatingCanvas;
begin
	canvasAnimationHandler := window.requestAnimationFrame(@AnimateCanvas);
end;

function LoadedTexture (event: TEventListenerEvent): boolean;
var
	texture: TJSWebGLTexture;
begin
	texture := gl.createTexture;
	gl.bindTexture(gl.TEXTURE_2D, texture);
	gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
	gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
	gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
	gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
	gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, TexImageSource(event.target));

	textureLoaded := true;
	result := true;
end;

var
  canvas: TJSHTMLCanvasElement;
  vertexShaderSource: string;
  fragmentShaderSource: string;
  img: TJSHTMLElement;
begin

	// add debug status
	debugConsole := document.getElementById('debug-console');

	// make webgl context
  canvas := TJSHTMLCanvasElement(document.createElement('canvas'));
  canvas.width := 800;
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

	gl.enable(gl.DEPTH_TEST);
	gl.enable(gl.BLEND);
	gl.Enable(gl.CULL_FACE);
	gl.CullFace(gl.BACK);

	// set projection transform
	projTransform := TMat4.Perspective(60.0, canvas.width / canvas.height, 0.1, 2000);
	shader.SetUniformMat4('projTransform', projTransform);

	// lighting
	lightPosition := V3(0, terrainSize / 2, -(terrainSize/2));
	shader.SetUniformVec3('lightPosition', lightPosition);
	shader.SetUniformVec3('lightColor', V3(1, 1, 1));

	// model material
	shader.SetUniformFloat('shineDamper', 1000);
	shader.SetUniformFloat('reflectivity', 1);

	gl.clear(gl.COLOR_BUFFER_BIT + gl.DEPTH_BUFFER_BIT);

	camera.x := -(terrainSize/2);
	camera.y := -(terrainSize/4);
	camera.z := -(terrainSize/2);

	// load terrain texture from image tag
	//img := TJSHTMLElement(document.getElementById('terrain-texture'));
	//    <image id="terrain-texture" crossOrigin="anonymous" src="res/ground.jpg" hidden/>
	img := TJSHTMLElement(document.createElement('IMG'));
	img.setAttribute('height', '512');
	img.setAttribute('width', '512');
	img.setAttribute('crossOrigin', 'anonymous');
	img.setAttribute('src', 'res/ground.jpg');
	img.onload := @LoadedTexture;

	//texture := gl.createTexture;
	//gl.bindTexture(gl.TEXTURE_2D, texture);
	//gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
	//gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
	//gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
	//gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
	//gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, TJSTexImageSource(img));
	//textureLoaded := true;

	// TODO: RandSeed doesn't seem to work so we get a random seed each time
	terrainNoise := TNoise.Create(RandomNoiseSeed(1));
	maps := TJSArray.new;

	StartAnimatingCanvas;
end.
