Vec3 :: struct {
	x,y,z: float;
	n: Norm;
}

Norm :: struct {
	w, l: float;
}

vec3_add :: func(dest, a, b: *Vec3) void {
	dest, a, b: *Vec3;
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
	cast([]char)s;
	cast(*char)s;
	cast(string)s;
	va, vb, vc: *Vec3;
	vec3_add(va, vb, vc);
	i,j,k : int = 1;
	i,j,k = test();
	ptr_1: *void;
	ptr_1 = cast(*void)&a;
	fp = &v.x;
}
