Vec3 :: struct {
	x,y,z: float;
	n: Norm;
}

bigU :: union {
	recurse: *bigU;
}

Norm :: struct {
	w, l: float;
}

vec3_add :: func(dest: *Vec3, a, b: *Vec3) void {
	tmp: *Vec3;
	dest.x = a.x + b.x;
	dest.y = a.y + b.y;
	tmp = dest;
}

test :: func(void) int, int, int {
	return 1, 2, 3;
}

main :: func {
	ptr: *void;
	v: Vec3;
	vp: *Vec3;
	a, b, c: int;
	f: float = 0.1;
	b = 2;
	c = 3;
	a = b + c;
	f = v.n.l;
	v = *vp;
	*vp = v;
	fp: *float;
	fn: func(int)(int);
	fp + 1;
	cast(*void)a;
	s: []char;
	arr_str : []int = cast([]char)s;
	cast(*char)s;
	cast(string)s;
	va, vb, vc: *Vec3;
	vec3_add(va, vb, vc);
	i,j,k : int = 1;
	i,j,k = test();
	i,,k = test();
	i,, = test();
	ptr_1: *void;
	ptr_1 = cast(*void)&a;
	fp = &v.x;
	fp = &f;
	f = *fp;
	bo : bool = i < j and i;
	tmp :: 1;
	bo = cast(bool)cast(*Vec3)cast(*void)i;
	f += 1;
	i++;
	f++;
	vp++;
	i = cast(int)(fp + j);
	fp / 1;
	fp % 1;
	fp + 1;
	new_i := f + 1;
	f = new_i;
	test_arr: [12 * 2 << 3]int;
	hello : string = ---;
}
