Vec3 :: struct {
	x,y,z: float;
	n: Norm;
}

Norm :: struct {
	w, l: float;
}

main :: func {
	v: Vec3;
	a, b, c: int;
	f: float = 0.1;
	b = 2;
	c = 3;
	a = b + c;
	f = v.n.l;
}
