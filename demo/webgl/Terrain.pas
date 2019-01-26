unit Terrain;
interface
uses
	Noise, Matrix,
	GLTypes, GLUtils,
	WebGL, JS,
	Math;
	
type
	TTerrain = class (TObject)
		public	
			constructor Create (context: TJSWebGLRenderingContext; size, resolution: integer); overload;
			constructor Create (context: TJSWebGLRenderingContext; inNoise: TNoise; size, resolution: integer; offset: TVec2); overload;
			
			function GetHeightAtPoint (x, y: integer): TJSFloat32;
			function GetWidth: integer;
			function GetHeight: integer;

			procedure Draw;
			procedure Generate;

		protected
			noise: TNoise;

			function GetHeightForVertex (localX, localY, x, y: integer): TNoiseFloat; virtual;
		private
			gl: TJSWebGLRenderingContext;

			noiseOffset: TVec2;
			terrainSize: integer;
			terrainResolution: integer;
			heights: TMatrix;
			model: TModel;

			function CalculateNormal (localX, localY, x, y: integer): TVec3;
	end;

implementation

procedure TTerrain.Draw;
begin
	model.Draw;
end;

function TTerrain.GetWidth: integer;
begin
	result := heights.GetWidth;
end;

function TTerrain.GetHeight: integer;
begin
	result := heights.GetHeight;
end;

function TTerrain.GetHeightAtPoint (x, y: integer): TJSFloat32;
begin
	result := TNoiseFloat(heights.GetValue(x, y));
end;

function TTerrain.GetHeightForVertex (localX, localY, x, y: integer): TNoiseFloat; 
begin
	result := noise.GetNoise(x, y, heights.GetWidth, heights.GetHeight, 4, 3);
	result := Power(result, 5);
	result := (result * 20) - 6;	
end;

function TTerrain.CalculateNormal (localX, localY, x, y: integer): TVec3; 
var
	heightL, heightR, heightD, heightU: TNoiseFloat;
begin
	heightL := GetHeightForVertex(localX, localY, x-1, y);
	heightR := GetHeightForVertex(localX, localY, x+1, y);
	heightD := GetHeightForVertex(localX, localY, x, y-1);
	heightU := GetHeightForVertex(localX, localY, x, y+1);
	result := V3(heightL-heightR, 2.0, heightD - heightU);
	result := Normalize(result);
end;

procedure TTerrain.Generate;
var
	vertex: TModelVertex;
	topLeft: integer;
	topRight: integer;
	bottomLeft: integer;
	bottomRight: integer;
	x, y, gz, gx: integer;
	verticies: TJSArray;
	indicies: TJSArray;
	data: TModelData;
begin
	if noise = nil then
		noise := TNoise.Create(RandomNoiseSeed(1));
	heights := TMatrix.Create(terrainResolution, terrainResolution);

	verticies := TJSArray.new;
	indicies := TJSArray.new;

	for y := 0 to heights.GetWidth - 1 do
	for x := 0 to heights.GetHeight - 1 do
		begin			
			vertex.pos.x := x/(heights.GetWidth - 1) * terrainSize;
			vertex.pos.y := GetHeightForVertex(x, y, Trunc(noiseOffset.x) + x, Trunc(noiseOffset.y) + y);
			vertex.pos.z := y/(heights.GetHeight - 1) * terrainSize;

			heights.SetValue(x, y, vertex.pos.y);

			vertex.normal := CalculateNormal(x, y, Trunc(noiseOffset.x) + x, Trunc(noiseOffset.y) + y);
			
			// distribute linearly between 0-1
			vertex.texCoord.x := x/(heights.GetWidth - 1);
			vertex.texCoord.y := y/(heights.GetHeight - 1);
			
			ModelVertexAddToArray(vertex, verticies);
		end;

	for gz := 0 to heights.GetWidth - 2 do
	for gx := 0 to heights.GetHeight - 2 do
		begin
			topLeft := (gz*heights.GetWidth)+gx;
			topRight := topLeft + 1;
			bottomLeft := ((gz+1)*heights.GetWidth)+gx;
			bottomRight := bottomLeft + 1;
			
			indicies.push(topLeft);
			indicies.push(bottomLeft);
			indicies.push(topRight);
			indicies.push(topRight);
			indicies.push(bottomLeft);
			indicies.push(bottomRight);
		end;

	data.verticies := TJSFloat32Array.New(TJSObject(verticies));
	data.indicies := TJSUint16Array.New(TJSObject(indicies));
	data.floatsPerVertex := kModelVertexFloats;

	model := TModel.Create(gl, data);
end;

constructor TTerrain.Create (context: TJSWebGLRenderingContext; inNoise: TNoise; size, resolution: integer; offset: TVec2);
begin
	gl := context;
	noise := inNoise;
	noiseOffset := offset;
	terrainSize := size;
	terrainResolution := resolution;
end;

constructor TTerrain.Create (context: TJSWebGLRenderingContext; size, resolution: integer);
begin
	gl := context;
	noiseOffset := V2(0, 0);
	terrainSize := size;
	terrainResolution := resolution;
end;

end.
