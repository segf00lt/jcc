multiple_return_vals :: func(a, b, c: int) int, int, int {
	return a + 1, b - a, c << 2;
}

test::func(a: int, b: int) int {
	b += 1;
	a = b;
}

two :: func() int {
	return cast(int)2;
}

exp :: func() int {
	return 1 * 1 + 1;
}

factorial :: func(x := 0) int {
	if x <= 0 {
		return 1;
	}
	return x * factorial(x - 1);
}

number :: func(x: int = 0, b: int, c: int) int {
	return cast(u8)(x / b) >> (x)&11;
}

printer :: func(s: [3]char) inline {
}

add_abc :: func(a, b, c: int) int {
	result : int = 0;
	v: Local_Vec;

	defer print("hello world");

	local_func();

	if a > 1 {
		result += a + b + c;
	} elif a == 5 {
		result >>= 1;
	} else {
		return 0;
	}

	while true {
		result += 1;
	}

	for i, j : int, c : char = 'c'; i < 10; i += j - c {
		i = 2;
		print("%i");
	}

	return result;

	Local_Vec :: struct {
		x,y,z: float;
	}

	local_func :: func {
		print("hello\n");
	}
}

more_bobby :: struct {
	p: int;
}

BOBBY :: struct {
	x: [][]more_bobby;
}

main :: func {
	a, b, c: u64 = ---;
	a <<= 4;
	array: []int;

	ptr: **u64 = &a;
	1 << 2;
	//array[(cast(int)10 % 2) << 5] = 1333;
	bob: BOBBY;
	bob.x[9][2].p = 2;
	testptr: ****int;
	cast(****int)testptr;
	**(cast(***int)cast(*void)a)[0] = 64;
}

one :: func(i: int) int inline {
	return 1;
}

abc :: func(a: int, b: int, c: int) int {
}

