unit GLTypes;
interface
uses
	WebGL, Math, SysUtils;

type
	TScalar = double;
    TJSFloat32 = double;
    TJSFloat32List = array of TJSFloat32;

type
	TVec2 = record
		x, y: TScalar;
	end;

type
	TVec3 = record
		x, y, z: TScalar;
	end;

type
	TRGBAb = array of GLubyte;
	TRGBAf = array of TScalar;

{ Vec3 }
function V3(x, y, z: TScalar): TVec3;
function ToFloats(v: TVec3): TJSFloat32List; overload;
function VecStr(v: TVec3): string; overload;
procedure Show (v: TVec3);

// math functions since we don't have advanced records
function Add (v: TVec3; amount: TScalar): TVec3; overload;
function Add (v: TVec3; amount: TVec3): TVec3; overload;
function Subtract (v: TVec3; amount: TScalar): TVec3; overload;
function Subtract (v: TVec3; amount: TVec3): TVec3; overload;
function Multiply (v: TVec3; amount: TScalar): TVec3; overload;
function Multiply (v: TVec3; amount: TVec3): TVec3; overload;
function Divide (v: TVec3; amount: TScalar): TVec3; overload;
function Divide (v: TVec3; amount: TVec3): TVec3; overload;
function Sum (v: TVec3): TScalar; overload;
function Magnitude(v: TVec3): TScalar; overload;
function SquaredLength (v: TVec3): TScalar; overload;
function Normalize (v: TVec3): TVec3; overload;
function Dot (v: TVec3; point: TVec3): TScalar; overload;
function Cross (v: TVec3; point: TVec3): TVec3; overload;

{ Vec2 }
function V2(x, y: TScalar): TVec2;
function ToFloats(v: TVec2): TJSFloat32List; overload;

function RGBAb(r, g, b, a: GLubyte): TRGBAb;
function RGBAf(r, g, b, a: TScalar): TRGBAf;

implementation

{=============================================}
{@! ___Vec2___ } 
{=============================================}

function V2(x, y: TScalar): TVec2;
begin
	result.x := x;
	result.y := y;
end;

function ToFloats(v: TVec2): TJSFloat32List;
begin
	SetLength(result, 2);
	result[0] := v.x;
	result[1] := v.y;
end;

{=============================================}
{@! ___Vec3___ } 
{=============================================}

function V3(x, y, z: TScalar): TVec3;
begin
	result.x := x;
	result.y := y;
	result.z := z;
end;

function Add (v: TVec3; amount: TScalar): TVec3;
begin
	result := V3(v.x + amount, v.y + amount, v.z + amount);
end;

function Add (v: TVec3; amount: TVec3): TVec3;
begin
	result := V3(v.x + amount.x, v.y + amount.y, v.z + amount.z);
end;

function Subtract (v: TVec3; amount: TScalar): TVec3;
begin
	result := V3(v.x - amount, v.y - amount, v.z - amount);
end;

function Subtract (v: TVec3; amount: TVec3): TVec3;
begin
	result := V3(v.x - amount.x, v.y - amount.y, v.z - amount.z);
end;

function Multiply (v: TVec3; amount: TScalar): TVec3;
begin
	result := V3(v.x * amount, v.y * amount, v.z * amount);
end;

function Multiply (v: TVec3; amount: TVec3): TVec3;
begin
	result := V3(v.x * amount.x, v.y * amount.y, v.z * amount.z);
end;

function Divide (v: TVec3; amount: TScalar): TVec3;
begin
	result := V3(v.x / amount, v.y / amount, v.z / amount);
end;

function Divide (v: TVec3; amount: TVec3): TVec3;
begin
	result := V3(v.x / amount.x, v.y / amount.y, v.z / amount.z);
end;

function Sum (v: TVec3): TScalar;
begin
	result := v.x + v.y + v.z;
end;

function Magnitude(v: TVec3): TScalar;
begin
	result := Sqrt(Power(v.x, 2) + Power(v.y, 2) + Power(v.z, 2));
end;

function SquaredLength (v: TVec3): TScalar;
begin
	result := Sqr(v.x) + Sqr(v.y) + Sqr(v.z);
end;

function Normalize (v: TVec3): TVec3;
begin
	result := Divide(v, Magnitude(v));
end;

function Dot (v: TVec3; point: TVec3): TScalar;
begin
	result := (v.x * point.x) + (v.y * point.y) + (v.z * point.z);
end;

function Cross (v: TVec3; point: TVec3): TVec3;
begin
	result.x := (v.y * point.z) - (v.z * point.y);
	result.y := (v.z * point.x) - (v.x * point.z);
	result.z := (v.x * point.y) - (v.y * point.x);
end;

function ToFloats(v: TVec3): TJSFloat32List;
begin
	SetLength(result, 3);
	result[0] := v.x;
	result[1] := v.y;
	result[2] := v.z;
end;

function VecStr(v: TVec3): string;
begin
	result := '{'+FloatToStr(v.x)+','+FloatToStr(v.y)+','+FloatToStr(v.z)+'}';
end;

procedure Show (v: TVec3);
begin
	writeln('{',v.x,',',v.y,',',v.z,'}');
end;

{=============================================}
{@! ___Colors___ } 
{=============================================}

function RGBAb(r, g, b, a: GLubyte): TRGBAb;
begin
	result[0] := r;
	result[1] := g;
	result[2] := b;
	result[3] := a;
end;

function RGBAf(r, g, b, a: GLfloat): TRGBAf;
begin
	result[0] := r;
	result[1] := g;
	result[2] := b;
	result[3] := a;
end;

end.
