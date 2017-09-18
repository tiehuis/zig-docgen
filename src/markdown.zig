//! A markdown -> AST parser.

const std = @import("std");
const printf = std.io.stdout.printf;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Buffer = std.Buffer;

const MdState = enum {
    Normal,
    FencedCodeBlock,
};

const HeaderWeight = enum {
    H1, H2, H3, H4, H5, H6,
};

const MdTextAttribute = enum {
    None,
    Italic,
    Bold,
    Code,
};

const MdTextItem = enum {
    Buf: Buffer,
    Attr: MdTextAttribute,

    pub fn print(self: &const MdTextItem) -> %void {
        switch (*self) {
            MdTextItem.Buf => |b| {
                %return printf("'{}'", b.toSliceConst());
            },
            MdTextItem.Attr => |a| {
                %return printf("'{}'", @enumTagName(a));
            }
        }
    }
};

const MdText = ArrayList(MdTextItem);

const MdHeader = struct {
    text: MdText,
    weight: HeaderWeight,
};

const MdNode = enum {
    Header: MdHeader,
    Paragraph: MdText,

    pub fn print(self: &const MdNode) -> %void {
        switch (*self) {
            MdNode.Header => |header| {
                %return printf(
                    \\Header {{
                    \\  text: 
                );

                for (header.text.toSliceConst()) |text| {
                    %return text.print();
                    %return printf(", ");
                }

                %%printf(
                    \\
                    \\  weight: {}
                    \\}}
                    \\
                    , @enumTagName(header.weight)
                );

            },
            MdNode.Paragraph => |para| {
                %return printf(
                    \\Paragraph {{
                    \\  text: 
                );

                for (para.toSliceConst()) |text| {
                    %return text.print();
                    %return printf(", ");
                }

                %return printf(
                    \\
                    \\}}
                    \\
                );
            },
        }
    }
};

error BadHeaderWeightValue;

const MdParser = struct {
    const Self = this;

    allocator: &Allocator,
    nodes: ArrayList(MdNode),
    state: MdState,
    line_state: MdText,

    pub fn init(allocator: &Allocator) -> %Self {
        Self {
            .allocator = allocator,
            .nodes = ArrayList(MdNode).init(allocator),
            .state = MdState.Normal,
            .line_state = ArrayList(MdTextItem).init(allocator),
        }
    }

    fn parseText(self: &Self, line: []const u8) -> %MdText {
        var text = MdText.init(self.allocator);
        %defer text.deinit();


        // For now, don't handle styles
        %return text.append(MdTextItem.Buf {
            %return Buffer.init(self.allocator, line),
        });

        text
    }

    fn parseHeader(self: &Self, line: []const u8) -> %MdNode {
        var i: usize = 0;
        var weight: usize = 0;

        while (i < line.len) : (i += 1) {
            if (line[i] == '#') {
                weight += 1;
            } else {
                break;
            }
        }

        if (weight < @memberCount(HeaderWeight)) {
            // NOTE: Could we infer the inner types better for enums?
            const header = MdHeader {
                .text = %return self.parseText(line[i..]),
                .weight = HeaderWeight(weight - 1),
            };

            MdNode.Header { header }
        } else {
            error.BadHeaderWeightValue
        }
    }

    pub fn parseLine(self: &Self, line: []const u8) -> %?MdNode {
        switch (self.state) {
            MdState.Normal => {
                if (line.len > 0 and line[0] == '#') {
                    // NOTE: This is required else the return value is seen as %MdNode
                    const header = %return self.parseHeader(line);
                    header
                } else {
                    if (line.len == 0) {
                        // TODO: This implies we always follow paragraphs with an empty line.
                        if (self.line_state.len != 0) {
                            var pg = MdText.init(self.allocator);
                            %defer pg.deinit();

                            %return pg.appendSlice(self.line_state.toSliceConst());
                            self.line_state.resizeDown(0);

                            var paragraph = MdNode.Paragraph { pg };
                            paragraph
                        } else {
                            null
                        }
                    } else {
                        var text = %return self.parseText(line);
                        %return self.line_state.appendSlice(text.toSliceConst());
                        null
                    }
                }
            },

            MdState.FencedCodeBlock => {
                unreachable;
            },
        }
    }

    pub fn parse(self: &Self, markdown: []const u8) -> %ArrayList(MdNode) {
        // The split iterator does not show new lines which we are interested in.
        var lines = iterateLines(markdown);

        while (lines.next()) |line| {
            if (self.parseLine(line)) |ok| {
                if (ok) |node| {
                    %return self.nodes.append(node);
                }
            } else |err| {
                %return std.io.stderr.printf("{}\n", @errorName(err));
            }
        }

        self.nodes
    }
};

fn iterateLines(s: []const u8) -> LineIterator {
    LineIterator {
        .index = 0,
        .s = s,
    }
}

const LineIterator = struct {
    s: []const u8,
    index: usize,

    pub fn next(self: &LineIterator) -> ?[]const u8 {
        const start = self.index;

        if (start >= self.s.len) {
            return null;
        }

        self.index += 1;
        while (self.index < self.s.len and self.s[self.index] != '\n') {
            self.index += 1;
        }

        const end = self.index;
        if (start == 0) {
            self.s[start .. end]
        } else {
            self.s[start + 1 .. end]
        }
    }
};

test "markdown" {
    const md1 =
        \\# Here is a title
        \\## Title 2
        \\
        \\Here is a paragraph,
        \\  followed on the same line
        \\
        \\and a new paragraph
        \\
        \\##### Following title
    ;

    var md = %%MdParser.init(&std.debug.global_allocator);
    const nodes = %%md.parse(md1);

    for (nodes.toSliceConst()) |node| {
        %%node.print();
    }
}
