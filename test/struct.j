// anonymous struct
struct {
	i: int = ---;
	union {
		c: char;
		__padding: u32;
	}
}

// variable struct
foo, foo2 := struct {
	bar: float;
	kiss: Array;
}

// constant struct
Array :: struct {
	cap, count, stride: u64;
	data: *void;
}

//Array: int;

//foo3: foo; // this shouldn't compile

Bar :: struct {
	i: int = ---;
	union {
		c: char;
		__padding: u32;
	}
	struct {
		x: u8;
		y: u8;
	}
}

test :: union {
	i: int;
	struct {
		c: char;
		f: float;
	}
}

recurse :: struct {
	r: []recurse;
}

