const std = @import("std");
const os = std.os;
const io = std.io;
const debug = std.debug;
const Buffer = std.Buffer;
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const c_alloc = @import("c_allocator.zig");

const Action = enum {
    Tokenize,
    Parse,
    None,
};

fn printUsage() {
    %%io.stderr.printf(
        \\usage: cat <file> | docgen
        \\
        \\  --tokenize  Tokenize the input file and print the tokens
        \\  --parse     Parse the input file and print the ast
        \\
    );
}

fn commandTokenize(buf: &Buffer) {
    var tkr = Tokenizer.init(&c_alloc.c_allocator);

    _ = %%tkr.process(buf.toSliceConst());
    for (tkr.tokens.toSliceConst()) |token| {
        %%token.print();
    }
}

fn commandParse(buf: &Buffer) {
    @panic("TODO implement command parse");
}

pub fn main() -> %void {
    var action = Action.Tokenize;

    { var i: usize = 1; while (i < os.args.count()) : (i += 1) {
        const arg = os.args.at(i);

        if (std.mem.eql(u8, arg, "--tokenize")) {
            action = Action.Tokenize;
        } else if (std.mem.eql(u8, arg, "--parse")) {
            action = Action.Parse;
        } else if (std.mem.eql(u8, arg, "--help")) {
            printUsage();
            os.exit(0);
        } else {
            %%io.stderr.printf("unknown argument: {}\n", arg);
            printUsage();
            os.exit(1);
        }
    }}

    // TODO: Allow reading from a file and not just stdin
    var is = io.stdin;

    var buf = Buffer.initNull(&debug.global_allocator);
    defer buf.deinit();
    is.readAll(&buf) %% |err| {
        %%io.stderr.printf("unable to read input stream: {}\n", @errorName(err));
        os.abort();
    };

    switch (action) {
        Action.Tokenize => commandTokenize(&buf),
        Action.Parse => commandParse(&buf),

        Action.None => {
            printUsage();
            os.exit(1);
        }
    }
}
