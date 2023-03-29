/*
Vec2 :: struct {
	x, y: f32;
}
integer : int;
type: typ = int;
i: int = 0;
x: s8 = -1;
arr: [2]char;
ptr: **void = 0;
f :: func(i: int, c: char, f_prime: func(int)(char)) bool, bool {
	return true, false;
}
*/
function_pointer_pointer_pointer : **func(int, char, func(int)(char)) (bool, bool) = int | (&&f);
v: Vec2;
thingy: struct { x, y: f32; } = void;
other_thing :: struct {
	x, y: f32;
	CONSTANT :: 400000;
}

matrix: [1][2][0+3]float;
pointer: *[3]*[0&1]int;
other_ptr: ***[]****void;
bleh: *[][2]*void;
