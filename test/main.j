Vec3 :: struct {
	x, y, z: f32 = 0;
}

vec3_add :: func(dest, a, b: *Vec3) {
	tmp: *Vec3;
	dest.x = a.x + b.x;
	dest.y = a.y + b.y;
	tmp = dest;
}

test :: func() int, int, int {
	return 1, 2, 3;
}

mystruct :: struct {
	v: Vec3;
}

main :: func(i: int) {
	a, b,c: int;
	ptr: *int;
	a = b + 1;
	ptr = &a;
	**cast(**int)ptr = b;
	//i: Vec3;
	f: float = 10.45;
	v: Vec3;
	bad_struct: mystruct;
	//bad_struct.v.bad_member;
	//vec3_add(a, b, c);
	a = -b;
	i, j, k: int;
	i = cast(int)1.0;
	i,j = 0;
	vec3_add(cast(*Vec3)cast(*void)a, cast(*Vec3)cast(*void)b, cast(*Vec3)cast(*void)c);
	i,j,k = test();
	array: []int;
	array[1 << 2] = 0;
}
