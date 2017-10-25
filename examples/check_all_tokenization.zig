//! Global module-level doc comment
const std = @import("std");

// Here is a single-line comment
/// Line-level doc comment
pub fn add(a: usize, @"b": usize) -> @typeOf(a + b) {
    a << b + 0x45 +% 0b10 -% "" <<= 'a' '\x23'
}

const _azAz = "\U0001f34";
const _90123 = "\u2510";
comptime {
    const aaa = 0x12398124;
    const bbb = 0b01010101;
    const ccc = 0o12743741;
}

a: []const u8 = *&&{} ** {1} ++ {3};

extern coldcc = packed struct {
    const Self = this;
}

test "name" {} = undefined
a = '\n';

const a =
    \\ // name example
    \\and another
    \\ yes
;

@import("std").io.stdout.printf("{}\t{}\r{}\t{}\n" "{}\\t{}\\r{}\\t{}\\n", a, b, c, d);

1.012307124 -123 -1.1

//Here is a comment
1.1e8 2.1e+12 4213091.7e-2 0.0 -0.0

0xaaaaaa.0p-23 0xaddf23.1p24 0x0.3p10
