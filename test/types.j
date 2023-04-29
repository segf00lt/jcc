/*
integer : int;
i: int = 0;
x: s8 = -1;
ptr: **void = &cast(*void)0;
function_pointer_pointer_pointer : **func(int, char, func(int)(char)) (bool, bool);
matrix: [1][2][0+3]float;
pointer: *[3]*[0&1]int;

other_thing :: struct {
	x, y: f32;
}

f :: func(i: int, c: char, f_prime: func(int)(char)) bool, bool {
	return true, false;
}

type: Type = Type;
thingy: struct { x, y: f32; };

//arr: [2][2]char;
fn: func(int)(int,int);
other_ptr: ***[]****void;
bleh: *[2][2]*void;
*/
Vec2 :: struct {
	x, y: f32;
}
v: Vec2;
boolean_var: bool = v.x or 1 and 0;
