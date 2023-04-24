Vec3 :: struct {
	x, y, z: f32 = 0;
}

vec3_add :: func(dest, a, b: *Vec3) {
	dest, a, b: *Vec3;
	tmp: *Vec3;
	dest.x = a.x + b.x;
	dest.y = a.y + b.y;
	tmp = dest;
}

test :: func() int, int, int {
	return 1, 2, 3;
}

main :: func() {
	a, b,c: int;
	//ptr: *int;
	////a = b + 1;
	//ptr = &a;
	//**ptr = b;
	////i: Vec3;
	//bob.x[9].ptr = 2;
	//f: float = 10.45;
	//ptr, b = vec3_add(a, b);
	//a = -b;
	i, j, k: int;
	i = cast(int)1.0;
	i,j = 0;
	vec3_add(cast(*Vec3)cast(*void)a, cast(*Vec3)cast(*void)b, cast(*Vec3)cast(*void)c);
	i,j,k = test();
}
