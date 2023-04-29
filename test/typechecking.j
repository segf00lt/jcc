Vec3 :: struct {
	struct {
		x,y,z: float;
		n: Norm;
	}
}

bigU :: union {
	recurse: *bigU;
}

Norm : Type : struct {
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

BUFSIZE : u64 : 0xff;

main :: func {
	test_int : s32 = -12;
	test_float : float = 1.2;
	typevar: Type = struct { x,y,z: float; }
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
	fp: *float = cast(*float)cast(*int)&f;
	fn: func(int)(int);
	fp + 1;
	cast(*void)a;
	s: []char;
	arr_str : []char = cast([]char)s;
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
