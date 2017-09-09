pub fn add(comptime T: type, a: T, b: T) -> @typeOf(a + b) {
    a + b
}

fn hidden_sub(comptime T: type, a: T, b: T) -> @typeOf(a - b) {
    a - b
}
