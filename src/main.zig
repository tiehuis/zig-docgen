const std = @import("std");
const os = std.os;
const io = std.io;
const debug = std.debug;
const Buffer = std.Buffer;
const Tokenizer = @import("tokenizer.zig").Tokenizer;

pub fn main() -> %void {
    var is = if (os.args.count() >= 2) {
        // TODO: BadFd issue here. Just cat file for testing.
        var fd = io.InStream.open(os.args.at(1), &debug.global_allocator) %% |err| {
            %%io.stderr.printf("unable to open file: {}\n", @errorName(err));
            os.abort();
        };
        defer fd.close();
        fd
    } else {
        io.stdin
    };

    var buf = Buffer.initNull(&debug.global_allocator);
    defer buf.deinit();
    is.readAll(&buf) %% |err| {
        %%io.stderr.printf("unable to read file: {}\n", @errorName(err));
        os.abort();
    };

    var t = %%Tokenizer.process(buf.toSliceConst());
    for (t.tokens.toSliceConst()) |token| {
        %%token.print();
    }
}
