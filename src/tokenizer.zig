//! A tokenizer for zig.
//!
//! This is uses the current compiler tokenizer as a partial reference, but the
//! overall structure is quite different and should be a bit easier to follow.
//!
//! This also keeps track of comment information as tokens for documentation generation.

const std = @import("std");
const debug = std.debug;
const ArrayList = std.ArrayList;
const HashMap = std.HashMap;

/// A TokenId represents a kind of token. It will also hold its associated data if present.
const TokenId = enum {
    Ampersand,
    Arrow,
    AtSign,
    Bang,
    BinOr,
    BinXor,
    BitAndEq,
    BitOrEq,
    BitShiftLeft,
    BitShiftLeftEq,
    BitShiftRight,
    BitShiftRightEq,
    BitXorEq,
    CharLiteral,
    CmpEq,
    CmpGreaterOrEq,
    CmpGreaterThan,
    CmpLessOrEq,
    CmpLessThan,
    CmpNotEq,
    Colon,
    Comment,
    Comma,
    Dash,
    DivEq,
    DocComment,
    Dot,
    DoubleQuestion,
    Ellipsis2,
    Ellipsis3,
    Eof,
    Eq,
    FatArrow,
    FloatLiteral,
    IntLiteral,
    KeywordAlign,
    KeywordAnd,
    KeywordAsm,
    KeywordBreak,
    KeywordColdCC,
    KeywordCompTime,
    KeywordConst,
    KeywordContinue,
    KeywordDefer,
    KeywordElse,
    KeywordEnum,
    KeywordError,
    KeywordExport,
    KeywordExtern,
    KeywordFalse,
    KeywordFn,
    KeywordFor,
    KeywordGoto,
    KeywordIf,
    KeywordInline,
    KeywordNakedCC,
    KeywordNoAlias,
    KeywordNull,
    KeywordOr,
    KeywordPacked,
    KeywordPub,
    KeywordReturn,
    KeywordStdcallCC,
    KeywordStruct,
    KeywordSwitch,
    KeywordTest,
    KeywordThis,
    KeywordTrue,
    KeywordUndefined,
    KeywordUnion,
    KeywordUnreachable,
    KeywordUse,
    KeywordVar,
    KeywordVolatile,
    KeywordWhile,
    LBrace,
    LBracket,
    LParen,
    Maybe,
    MaybeAssign,
    MinusEq,
    MinusPercent,
    MinusPercentEq,
    MultiLineStringLiteral,
    ModEq,
    ModuleDocComment,
    NumberSign,
    Percent,
    PercentDot,
    PercentPercent,
    Plus,
    PlusEq,
    PlusPercent,
    PlusPercentEq,
    PlusPlus,
    RBrace,
    RBracket,
    RParen,
    Semicolon,
    Slash,
    Star,
    StarStar,
    StringLiteral,
    Symbol,
    Tilde,
    TimesEq,
    TimesPercent,
    TimesPercentEq,
};

const Kw = struct {
    name: []const u8,
    id: TokenId,

    pub fn new(name: []const u8, id: TokenId) -> Kw {
        Kw {
            .name = name,
            .id = id,
        }
    }
};

const keywords = []Kw {
    Kw.new("align", TokenId.KeywordAlign),
    Kw.new("and", TokenId.KeywordAnd),
    Kw.new("asm", TokenId.KeywordAsm),
    Kw.new("break", TokenId.KeywordBreak),
    Kw.new("coldcc", TokenId.KeywordColdCC),
    Kw.new("comptime", TokenId.KeywordCompTime),
    Kw.new("const", TokenId.KeywordConst),
    Kw.new("continue", TokenId.KeywordContinue),
    Kw.new("defer", TokenId.KeywordDefer),
    Kw.new("else", TokenId.KeywordElse),
    Kw.new("enum", TokenId.KeywordEnum),
    Kw.new("error", TokenId.KeywordError),
    Kw.new("export", TokenId.KeywordExport),
    Kw.new("extern", TokenId.KeywordExtern),
    Kw.new("false", TokenId.KeywordFalse),
    Kw.new("fn", TokenId.KeywordFn),
    Kw.new("for", TokenId.KeywordFor),
    Kw.new("goto", TokenId.KeywordGoto),
    Kw.new("if", TokenId.KeywordIf),
    Kw.new("inline", TokenId.KeywordInline),
    Kw.new("nakedcc", TokenId.KeywordNakedCC),
    Kw.new("noalias", TokenId.KeywordNoAlias),
    Kw.new("null", TokenId.KeywordNull),
    Kw.new("or", TokenId.KeywordOr),
    Kw.new("packed", TokenId.KeywordPacked),
    Kw.new("pub", TokenId.KeywordPub),
    Kw.new("return", TokenId.KeywordReturn),
    Kw.new("stdcallcc", TokenId.KeywordStdcallCC),
    Kw.new("struct", TokenId.KeywordStruct),
    Kw.new("switch", TokenId.KeywordSwitch),
    Kw.new("test", TokenId.KeywordTest),
    Kw.new("this", TokenId.KeywordThis),
    Kw.new("true", TokenId.KeywordTrue),
    Kw.new("undefined", TokenId.KeywordUndefined),
    Kw.new("union", TokenId.KeywordUnion),
    Kw.new("unreachable", TokenId.KeywordUnreachable),
    Kw.new("use", TokenId.KeywordUse),
    Kw.new("var", TokenId.KeywordVar),
    Kw.new("volatile", TokenId.KeywordVolatile),
    Kw.new("while", TokenId.KeywordWhile),
};

fn getKeywordId(symbol: []const u8) -> ?TokenId {
    for (keywords) |kw| {
        if (std.mem.eql(u8, kw.name, symbol)) {
            return kw.id;
        }
    }

    null
}

error BadValueForRadix;
error ValueOutOfRange;

/// Returns the digit value of the specified character under the specified radix.
///
/// If the value is too large, an error is returned.
fn getDigitValueForRadix(comptime radix: u8, c: u8) -> %u8 {
    const value = switch (c) {
        '0' ... '9' => |x| { x - '0' },
        'a' ... 'z' => |x| { x - 'a' + 10 },
        'A' ... 'Z' => |x| { x - 'A' + 10 },
        else => return error.ValueOutOfRange,
    };

    if (value < radix) {
        value
    } else {
        return error.BadValueForRadix;
    }
}

/// Extra data associated with a particular token.
pub const TokenData = enum {
    InternPoolRef: []const u8,
    Integer: u128,
    Char: u8,
    Error: error,
};

fn printCharEscaped(c: u8) -> %void {
    const printf = std.io.stdout.printf;

    switch (c) {
        '\r' => %return printf("\\r"),
        '\t' => %return printf("\\t"),
        '\n' => %return printf("\\n"),
        '\\' => %return printf("\\\\"),
        else => %return printf("{c}", c),
    }
}

/// A Token consists of a type/id and an associated location/span within the source file.
pub const Token = struct {
    id: TokenId,
    span: Span,
    data: ?TokenData,

    pub fn print(self: &const Token) -> %void {
        const printf = std.io.stdout.printf;

        %return printf("{}:{} {}",
            self.span.start_line,
            self.span.start_column,
            @enumTagName(self.id)
        );

        if (self.data) |inner| {
            %return printf(" (");
            switch (inner) {
                TokenData.InternPoolRef => |p_ref| {
                    for (p_ref) |c| {
                        %return printCharEscaped(c);
                    }
                },
                TokenData.Integer => |i| {
                    %return printf("{}", i);
                },
                TokenData.Char => |c| {
                    %return printCharEscaped(c);
                },
                TokenData.Error => |e| {
                    %return printf("{}", @errorName(e));
                },
            }
            %return printf(")");
        }

        %return printf("\n");
    }
};

/// A Span represents a contiguous sequence (byte-wise) of a source file.
pub const Span = struct {
    start_byte: usize,
    end_byte: usize,
    start_line: usize,
    start_column: usize,
};

error UnicodeCharCodeOutOfRange;
error NewlineInStringLiteral;
error InvalidCharacter;
error InvalidCharacterAfterBackslash;
error MissingCharLiteralData;
error ExtraCharLiteralData;
error Consumed;

fn u8eql(a: []const u8, b: []const u8) -> bool {
    std.mem.eql(u8, a, b)
}

// The second value is heap allocated.
pub const InternPool = HashMap([]const u8, []const u8, std.mem.hash_slice_u8, u8eql);

pub const Tokenizer = struct {
    const Self = this;

    tokens: ArrayList(Token),
    lines: ArrayList(usize),
    intern_pool: InternPool,
    errors: ArrayList(Token),
    consumed: bool,

    c_token: ?Token,
    c_byte: usize,
    c_line: usize,
    c_column : usize,
    c_buf: []const u8,

    /// Initialize a new tokenizer to handle the specified input buffer.
    fn init(buf: []const u8) -> Self {
        Self {
            .tokens = ArrayList(Token).init(&debug.global_allocator),
            .lines = ArrayList(usize).init(&debug.global_allocator),
            .intern_pool = InternPool.init(&debug.global_allocator),
            .errors = ArrayList(Token).init(&debug.global_allocator),
            .consumed = false,

            .c_token = null,
            .c_byte = 0,
            .c_line = 1,
            .c_column = 1,
            .c_buf = buf,
        }
    }

    /// Deinitialize the internal tokenizer state.
    fn deinit(self: &Self) {
        self.tokens.deinit();
        self.lines.deinit();
    }

    /// Returns the next byte in the buffer and advances our position.
    ///
    /// If we have reached the end of the stream, null is returned.
    fn nextByte(self: &Self) -> ?u8 {
        if (self.c_byte >= self.c_buf.len) {
            null
        } else {
            const i = self.c_byte;
            self.bump(1);
            self.c_buf[i]
        }
    }

    /// Mark the current position as the start of a token.
    fn beginToken(self: &Self) {
        self.c_token = Token {
            .id = undefined,
            .span = Span {
                .start_byte = self.c_byte,
                .end_byte = undefined,
                .start_line = self.c_line,
                .start_column = self.c_column,
            },
            .data = null,
        };
    }

    /// Set the id of the current token.
    fn setToken(self: &Self, id: TokenId) {
        (??self.c_token).id = id;
    }

    /// Mark the current position as the end of the current token.
    ///
    /// A token must have been previously set using beginToken.
    fn endToken(self: &Self) -> %void {
        var c = ??self.c_token;
        c.span.end_byte = self.c_byte + 1;
        %return self.tokens.append(c);
        self.c_token = null;
    }

    /// Set the data field for a ArrayList(u8) field using the internal InternPool.
    ///
    /// This takes ownership of the underlying memory.
    fn setInternToken(self: &Self, data: &ArrayList(u8)) -> %void {
        const ref = if (self.intern_pool.get(data.toSliceConst())) |entry| {
            data.deinit();
            entry.value
        } else {
            _ = %return self.intern_pool.put(data.toSliceConst(), data.toSliceConst());
            data.toSliceConst()
        };

        (??self.c_token).data = TokenData.InternPoolRef { ref };
    }

    /// Mark the current position as the end of a token and set it to a new id.
    fn setEndToken(self: &Self, id: TokenId) -> %void {
        self.setToken(id);
        %return self.endToken();
    }

    /// Peek at the character n steps ahead in the stream.
    ///
    /// If no token is found, returns the null byte.
    // TODO: Return an actual null? Much more verbose during usual parsing though.
    fn peek(self: &Self, comptime i: usize) -> u8 {
        if (self.c_byte + i >= self.c_buf.len) {
            0
        } else {
            self.c_buf[self.c_byte + i]
        }
    }

    /// Advance the cursor location by n steps.
    ///
    /// Bumping past the end of the buffer has no effect.
    fn bump(self: &Self, comptime i: usize) {
        var j: usize = 0;
        while (j < i) : (j += 1) {
            if (self.c_byte >= self.c_buf.len) {
                break;
            }

            if (self.peek(0) == '\n') {
                self.c_line += 1;
                self.c_column = 1;
            } else {
                self.c_column += 1;
            }
            self.c_byte += 1;
        }
    }

    // Actual processing helper routines.

    /// Processes integer with the specified radix.
    ///
    /// The integer will be represented by an unsigned value.
    ///
    /// This will only modify the current stream position.
    //
    // TODO: Use big integer generally improve here.
    // TODO: Handle float seperately.
    fn consumeNumber(self: &Self, comptime radix: u8, init_value: ?u8) -> %u128 {
        var number: u128 = if (init_value) |v| {
            %return getDigitValueForRadix(radix, v)
        } else {
            0
        };

        while (true) {
            switch (self.peek(0)) {
                '0' ... '9', 'a' ... 'f', 'A' ... 'F' => |c| {
                    self.bump(1);
                    number *= radix;
                    number += %return getDigitValueForRadix(radix, c);
                },

                else => {
                    // TODO: Need to be separated by a non-symbol token.
                    // Raise an error if we find a non-alpha-numeric that doesn't fit.
                    break;
                },
            }
        }

        number
    }

    /// Process a character code of the specified length and type.
    ///
    /// Returns the utf8 encoded value of the codepoint.
    ///
    /// This will only modify the current stream position.
    fn consumeCharCode(self: &Self,
        comptime radix: u8, comptime count: u8, comptime is_unicode: bool) -> %ArrayList(u8)
    {
        var utf8_code = ArrayList(u8).init(&debug.global_allocator);
        %defer utf8_code.deinit();

        var char_code: u32 = 0;
        comptime var i: usize = 0;
        inline while (i < count) : (i += 1) {
            char_code *= radix;
            char_code += %return getDigitValueForRadix(radix, self.peek(0));
            self.bump(1);
        }

        if (is_unicode) {
            if (char_code <= 0x7f) {
                %return utf8_code.append(u8(char_code));
            } else if (char_code <= 0x7ff) {
                %return utf8_code.append(0xc0 | u8(char_code >> 6));
                %return utf8_code.append(0x80 | u8(char_code & 0x3f));
            } else if (char_code <= 0xffff) {
                %return utf8_code.append(0xe0 | u8(char_code >> 12));
                %return utf8_code.append(0x80 | u8((char_code >> 6) & 0x3f));
                %return utf8_code.append(0x80 | u8(char_code & 0x3f));
            } else if (char_code <= 0x10ffff) {
                %return utf8_code.append(0xf0 | u8(char_code >> 18));
                %return utf8_code.append(0x80 | u8((char_code >> 12) & 0x3f));
                %return utf8_code.append(0x80 | u8((char_code >> 6) & 0x3f));
                %return utf8_code.append(0x80 | u8(char_code & 0x3f));
            } else {
                return error.UnicodeCharCodeOutOfRange;
            }
        } else {
            %return utf8_code.append(u8(char_code));
        }

        utf8_code
    }

    /// Process an escape code.
    ///
    /// Expects the '/' has already been handled by the caller.
    ///
    /// Returns the utf8 encoded value of the codepoint.
    fn consumeStringEscape(self: &Self) -> %ArrayList(u8) {
        switch (self.peek(0)) {
            'x' => {
                self.bump(1);
                self.consumeCharCode(16, 2, false)
            },

            'u' => {
                self.bump(1);
                self.consumeCharCode(16, 4, true)
            },

            'U' => {
                self.bump(1);
                self.consumeCharCode(16, 6, true)
            },

            'n'  => {
                self.bump(1);
                var l = ArrayList(u8).init(&debug.global_allocator);
                %return l.append('\n');
                l
            },

            'r'  => {
                self.bump(1);
                var l = ArrayList(u8).init(&debug.global_allocator);
                %return l.append('\r');
                l
            },

            '\\' => {
                self.bump(1);
                var l = ArrayList(u8).init(&debug.global_allocator);
                %return l.append('\\');
                l
            },

            't'  => {
                self.bump(1);
                var l = ArrayList(u8).init(&debug.global_allocator);
                %return l.append('\t');
                l
            },

            '\'' => {
                self.bump(1);
                var l = ArrayList(u8).init(&debug.global_allocator);
                %return l.append('\'');
                l
            },

            '"'  => {
                self.bump(1);
                var l = ArrayList(u8).init(&debug.global_allocator);
                %return l.append('\"');
                l
            },

            else => {
                @panic("unexpected character");
            }
        }
    }

    /// Process a string, returning the encountered characters.
    fn consumeString(self: &Self) -> %ArrayList(u8) {
        var literal = ArrayList(u8).init(&debug.global_allocator);
        %defer literal.deinit();

        while (true) {
            switch (self.peek(0)) {
                '"' => {
                    self.bump(1);
                    break;
                },

                '\n' => {
                    return error.NewlineInStringLiteral;
                },

                '\\' => {
                    self.bump(1);
                    var value = %return self.consumeStringEscape();
                    %return literal.appendSlice(value.toSliceConst());
                    value.deinit();
                },

                0 => {
                    @panic("eof while parsing string literal!");
                },

                else => |c| {
                    self.bump(1);
                    %return literal.append(c);
                },
            }
        }

        literal
    }

    /// Process a line comment, returning the encountered characters
    // TODO: For comments, do we want to strip leading whitespace here (and trailing)?
    // Any reason to preserve at till later?
    fn consumeUntilNewline(self: &Self) -> %ArrayList(u8) {
        var comment = ArrayList(u8).init(&debug.global_allocator);
        %defer comment.deinit();

        while (self.nextByte()) |c| {
            switch (c) {
                '\n' => {
                    break;
                },

                else => {
                    %return comment.append(c);
                }
            }
        }

        comment
    }

    /// Return the next token from the buffer.
    pub fn next(t: &Self) -> %&const Token {
        if (t.consumed) {
            return error.Consumed;
        }

        t.beginToken();
        if (t.nextByte()) |ch| {
            switch (ch) {
                ' ', '\r', '\n', '\t' => {},

                '(' => %return t.setEndToken(TokenId.LParen),
                ')' => %return t.setEndToken(TokenId.RParen),
                ',' => %return t.setEndToken(TokenId.Comma),
                '{' => %return t.setEndToken(TokenId.LBrace),
                '}' => %return t.setEndToken(TokenId.RBrace),
                '[' => %return t.setEndToken(TokenId.LBracket),
                ']' => %return t.setEndToken(TokenId.RBracket),
                ';' => %return t.setEndToken(TokenId.Semicolon),
                ':' => %return t.setEndToken(TokenId.Colon),
                '#' => %return t.setEndToken(TokenId.NumberSign),
                '~' => %return t.setEndToken(TokenId.Tilde),

                '_', 'a' ... 'z', 'A' ... 'Z' => {
                    var symbol = ArrayList(u8).init(&debug.global_allocator);
                    %return symbol.append(ch);

                    while (true) {
                        switch (t.peek(0)) {
                            '_', 'a' ... 'z', 'A' ... 'Z', '0' ... '9' => |c| {
                                t.bump(1);
                                %return symbol.append(c);
                            },

                            else => {
                                break;
                            },
                        }
                    }

                    if (getKeywordId(symbol.toSliceConst())) |id| {
                        symbol.deinit();
                        %return t.setEndToken(id);
                    } else {
                        %return t.setInternToken(&symbol);
                        %return t.setEndToken(TokenId.Symbol);
                    }
                },

                '0' => {
                    const value = switch (t.peek(0)) {
                        'b' => {
                            t.bump(1);
                            %return t.consumeNumber(2, null)
                        },

                        'o' => {
                            t.bump(1);
                            %return t.consumeNumber(8, null)
                        },

                        'x' => {
                            t.bump(1);
                            %return t.consumeNumber(16, null)
                        },

                        else => {
                            // TODO: disallow anything after a 0 except a dot.
                            %return t.consumeNumber(10, null)
                        },
                    };

                    (??t.c_token).data = TokenData.Integer { value };
                    %return t.setEndToken(TokenId.IntLiteral);
                },

                '1' ... '9' => {
                    const value = %return t.consumeNumber(10, ch);
                    (??t.c_token).data = TokenData.Integer { value };
                    %return t.setEndToken(TokenId.IntLiteral);
                },

                '"' => {
                    var literal = %return t.consumeString();
                    %return t.setInternToken(&literal);
                    %return t.setEndToken(TokenId.StringLiteral);
                },

                '-' => {
                    switch (t.peek(0)) {
                        '>' => {
                            t.bump(1);
                            %return t.setEndToken(TokenId.Arrow);
                        },

                        '=' => {
                            t.bump(1);
                            %return t.setEndToken(TokenId.MinusEq);
                        },

                        '%' => {
                            t.bump(1);
                            switch (t.peek(0)) {
                                '=' => {
                                    t.bump(1);
                                    %return t.setEndToken(TokenId.MinusPercentEq);
                                },

                                else => {
                                    %return t.setEndToken(TokenId.MinusPercent);
                                },
                            }
                        },

                        else => {
                            %return t.setEndToken(TokenId.Dash);
                        }
                    }
                },

                '+' => {
                    switch (t.peek(0)) {
                        '=' => {
                            t.bump(1);
                            %return t.setEndToken(TokenId.PlusEq);
                        },

                        '+' => {
                            t.bump(1);
                            %return t.setEndToken(TokenId.PlusPlus);
                        },

                        '%' => {
                            t.bump(1);
                            switch (t.peek(0)) {
                                '=' => {
                                    t.bump(1);
                                    %return t.setEndToken(TokenId.PlusPercentEq);
                                },

                                else => {
                                    %return t.setEndToken(TokenId.PlusPercent);
                                },
                            }
                        },

                        else => {
                            %return t.setEndToken(TokenId.Plus);
                        }
                    }
                },

                '*' => {
                    switch (t.peek(0)) {
                        '=' => {
                            t.bump(1);
                            %return t.setEndToken(TokenId.TimesEq);
                        },

                        '*' => {
                            t.bump(1);
                            %return t.setEndToken(TokenId.StarStar);
                        },

                        '%' => {
                            t.bump(1);
                            switch (t.peek(0)) {
                                '=' => {
                                    t.bump(1);
                                    %return t.setEndToken(TokenId.TimesPercentEq);
                                },

                                else => {
                                    %return t.setEndToken(TokenId.TimesPercent);
                                }
                            }
                        },

                        else => {
                            %return t.setEndToken(TokenId.Star);
                        },
                    }
                },

                '/' => {
                    switch (t.peek(0)) {
                        '/' => {
                            t.bump(1);
                            switch (t.peek(0)) {
                                '!' => {
                                    t.bump(1);
                                    t.setToken(TokenId.ModuleDocComment);
                                },

                                '/' => {
                                    t.bump(1);
                                    t.setToken(TokenId.DocComment);
                                },

                                else => {
                                    t.setToken(TokenId.Comment);
                                },
                            }

                            var comment_inner = %return t.consumeUntilNewline();
                            %return t.setInternToken(&comment_inner);
                            %return t.endToken();
                        },

                        '=' => {
                            t.bump(1);
                            %return t.setEndToken(TokenId.DivEq);
                        },

                        else => {
                            %return t.setEndToken(TokenId.Slash);
                        },
                    }
                },

                '%' => {
                    switch (t.peek(0)) {
                        '=' => {
                            t.bump(1);
                            %return t.setEndToken(TokenId.ModEq);
                        },

                        '.' => {
                            t.bump(1);
                            %return t.setEndToken(TokenId.PercentDot);
                        },

                        '%' => {
                            t.bump(1);
                            %return t.setEndToken(TokenId.PercentPercent);
                        },

                        else => {
                            %return t.setEndToken(TokenId.Percent);
                        },
                    }
                },

                '@' => {
                    switch (t.peek(0)) {
                        '"' => {
                            t.bump(1);
                            var literal = %return t.consumeString();
                            %return t.setInternToken(&literal);
                            %return t.setEndToken(TokenId.Symbol);
                        },

                        else => {
                            %return t.setEndToken(TokenId.AtSign);
                        },
                    }
                },

                '&' => {
                    switch (t.peek(0)) {
                        '=' => {
                            t.bump(1);
                            %return t.setEndToken(TokenId.BitAndEq);
                        },

                        else => {
                            %return t.setEndToken(TokenId.Ampersand);
                        }
                    }
                },

                '^' => {
                    switch (t.peek(0)) {
                        '=' => {
                            t.bump(1);
                            %return t.setEndToken(TokenId.BitXorEq);
                        },

                        else => {
                            %return t.setEndToken(TokenId.BinXor);
                        }
                    }
                },

                '|' => {
                    switch (t.peek(0)) {
                        '=' => {
                            t.bump(1);
                            %return t.setEndToken(TokenId.BitOrEq);
                        },

                        else => {
                            %return t.setEndToken(TokenId.BinOr);
                        }
                    }
                },

                '=' => {
                    switch (t.peek(0)) {
                        '=' => {
                            t.bump(1);
                            %return t.setEndToken(TokenId.CmpEq);
                        },

                        '>' => {
                            t.bump(1);
                            %return t.setEndToken(TokenId.FatArrow);
                        },

                        else => {
                            %return t.setEndToken(TokenId.Eq);
                        },
                    }
                },

                '!' => {
                    switch (t.peek(0)) {
                        '=' => {
                            t.bump(1);
                            %return t.setEndToken(TokenId.CmpNotEq);
                        },

                        else => {
                            %return t.setEndToken(TokenId.Bang);
                        },
                    }
                },

                '<' => {
                    switch (t.peek(0)) {
                        '=' => {
                            t.bump(1);
                            %return t.setEndToken(TokenId.CmpLessOrEq);
                        },

                        '<' => {
                            t.bump(1);
                            switch (t.peek(0)) {
                                '=' => {
                                    t.bump(1);
                                    %return t.setEndToken(TokenId.BitShiftLeftEq);
                                },

                                else => {
                                    %return t.setEndToken(TokenId.BitShiftLeft);
                                },
                            }
                        },

                        else => {
                            %return t.setEndToken(TokenId.CmpLessThan);
                        },
                    }
                },

                '>' => {
                    switch (t.peek(0)) {
                        '=' => {
                            t.bump(1);
                            %return t.setEndToken(TokenId.CmpGreaterOrEq);
                        },

                        '>' => {
                            t.bump(1);
                            switch (t.peek(0)) {
                                '=' => {
                                    t.bump(1);
                                    %return t.setEndToken(TokenId.BitShiftRightEq);
                                },

                                else => {
                                    %return t.setEndToken(TokenId.BitShiftRight);
                                },
                            }
                        },

                        else => {
                            %return t.setEndToken(TokenId.CmpGreaterThan);
                        },
                    }
                },

                '.' => {
                    switch (t.peek(0)) {
                        '.' => {
                            t.bump(1);
                            switch (t.peek(0)) {
                                '.' => {
                                    t.bump(1);
                                    %return t.setEndToken(TokenId.Ellipsis3)
                                },

                                else => {
                                    %return t.setEndToken(TokenId.Ellipsis2);
                                },
                            }
                        },

                        else => {
                            %return t.setEndToken(TokenId.Dot);
                        },
                    }
                },

                '?' => {
                    switch (t.peek(0)) {
                        '?' => {
                            t.bump(1);
                            %return t.setEndToken(TokenId.DoubleQuestion);
                        },

                        '=' => {
                            t.bump(1);
                            %return t.setEndToken(TokenId.MaybeAssign);
                        },

                        else => {
                            %return t.setEndToken(TokenId.Maybe);
                        }
                    }
                },

                '\'' => {
                    switch (t.peek(0)) {
                        '\'' => {
                            return error.MissingCharLiteralData;
                        },

                        '\\' => {
                            t.bump(1);
                            var value = %return t.consumeStringEscape();

                            if (t.peek(0) != '\'') {
                                return error.ExtraCharLiteralData;
                            } else {
                                t.bump(1);
                                std.debug.assert(value.len == 1);
                                (??t.c_token).data = TokenData.Char { value.toSliceConst()[0] };
                                value.deinit();
                                %return t.setEndToken(TokenId.CharLiteral);
                            }
                        },

                        else => {
                            if (t.peek(1) != '\'') {
                                return error.ExtraCharLiteralData;
                            } else {
                                (??t.c_token).data = TokenData.Char { t.peek(0) };
                                t.bump(2);
                                %return t.setEndToken(TokenId.CharLiteral);
                            }
                        },
                    }
                },

                '\\' => {
                    switch (t.peek(0)) {
                        '\\' => {
                            t.bump(1);
                            var literal = %return t.consumeUntilNewline();
                            %return t.setInternToken(&literal);
                            %return t.setEndToken(TokenId.MultiLineStringLiteral);
                        },

                        else => {
                            return error.InvalidCharacterAfterBackslash;
                        },
                    }
                },

                else => {
                    return error.InvalidCharacter;
                }
            }
        } else {
            t.consumed = true;
            %return t.setEndToken(TokenId.Eof);
        }

        &t.tokens.toSliceConst()[t.tokens.len - 1]
    }

    /// Construct a new tokenization instance and return it in its completed state.
    ///
    // NOTE: If we want to return a stream of tokens, tie this to an underlying tokenization state
    // which handles the intern pool.
    //
    // NOTE: The tokenizer will continue through errors until the complete buffer has been processed.
    // The list of errors encountered will be stored in the `errors` field.
    pub fn process(buf: []const u8) -> %Self {
        var t = Self.init(buf);
        %defer t.deinit();

        // This iterates over the entire buffer. Tokens are returned as references but are still
        // stored in the `tokens` field.
        while (true) {
            if (t.next()) |_| {} else |err| switch (err) {
                error.Consumed => break,
                else => return err,
            }
        }

        t
    }
};
