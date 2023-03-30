/*
*/
Vec2 :: struct {
	x, y: f32;
}
integer : int;
i: int = 0;
x: s8 = -1;
arr: [2]char;
ptr: **void = 0;
function_pointer_pointer_pointer : **func(int, char, func(int)(char)) (bool, bool) = int | (&&f);
matrix: [1][2][0+3]float;
pointer: *[3]*[0&1]int;
bleh: *[][2]*void;
other_ptr: ***[]****void;

other_thing :: struct {
	x, y: f32;
	CONSTANT :: 400000;
}

f :: func(i: int, c: char, f_prime: func(int)(char)) bool, bool {
	return true, false;
}

type: Type = int;
v: Vec2;
thingy: struct { x, y: f32; } = void;
