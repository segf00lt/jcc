Vec3 :: struct {
	x, y, z: f32 = 0;
}

Person :: struct {
	name: *char;
}

vec3_add :: func(dest, a, b: *Vec3) {
	tmp: *Vec3;
	dest.x = a.x + b.x;
	dest.y = a.y + b.y;
	tmp = dest;
}

main :: func() {
	a, b, c: Vec3 = ---;
	a.x = 2;
	a.y = 3;
	b.x = 0;
	b.y = 1;
	vec3_add(&c, &a, &b);
}
