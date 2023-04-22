Vec3 :: struct {
	x,y,z: float;
	n: Norm;
}

Norm :: struct {
	w, l: float;
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
	fn + 1;
}
