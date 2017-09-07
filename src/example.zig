const std = @import("std");

pub fn add(a: usize, @"b": usize) -> @typeOf(a + b) {
    a << b + 0x45 +% 0b10
}
