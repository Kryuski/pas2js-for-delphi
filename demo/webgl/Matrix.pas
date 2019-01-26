unit Matrix;
interface
uses
	JS;

type
	TMatrix = class
		public
			constructor Create (w, h: integer);
			procedure SetValue(x, y: integer; value: JSValue);
			function GetValue(x, y: integer): JSValue;
			procedure Show;
			function GetWidth: integer;
			function GetHeight: integer;
			// NOTE: no indexers yet?
			//property Indexer[const x,y:integer]:JSValue read GetValue write SetValue; default;
		private
			table: TJSArray;
			width: integer;
			height: integer;

			function IndexFor(x, y: integer): integer;
	end;

implementation

constructor TMatrix.Create (w, h: integer);
begin
	width := w;
	height := h;
	table := TJSArray.new(width * height);
end;

procedure TMatrix.SetValue(x, y: integer; value: JSValue);
begin
	table[IndexFor(x, y)] := value;
end;

function TMatrix.GetValue(x, y: integer): JSValue;
begin
	result := table[IndexFor(x, y)];
end;

function TMatrix.IndexFor(x, y: integer): integer;
begin
	result := x + y * height;
end;

procedure TMatrix.Show;
var
	x, y: integer;
begin
	for x := 0 to width - 1 do
	for y := 0 to height - 1 do
		begin
			writeln(x,',',y, ': ', GetValue(x, y));
		end;
end;

function TMatrix.GetWidth: integer;
begin
	result := width;
end;

function TMatrix.GetHeight: integer;
begin
	result := height;
end;


end.