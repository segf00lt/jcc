Vec2 :: struct {
	x, y: f32;
}
integer : int;
i: int = 0;
x: s8 = -1;
ptr: **void = 0;
function_pointer_pointer_pointer : **func(int, char, func(int)(char)) (bool, bool);
matrix: [1][2][0+3]float;
pointer: *[3]*[0&1]int;
bleh: *[][2]*void;
other_ptr: ***[]****void;

other_thing :: struct {
	x, y: f32;
	CONSTANT : u64 : 400000;
}

f :: func(i: int, c: char, f_prime: func(int)(char)) bool, bool {
	return true, false;
}

type: Type = Type;
v: Vec2;
thingy: struct { x, y: f32; } = void;

//arr: [2][2]char;
fn: func(int)(int,int);
