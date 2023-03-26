// anonymous struct
struct {
	i: int = ---;
	union {
		c: char;
		__padding: u32;
	};
}

// constant struct
Array :: struct {
	cap, count, stride: u64;
	data: *void;
}

// variable struct
foo, foo2 := struct {
	bar: float;
	kiss: Array;
}
