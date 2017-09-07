//! A tokenizer for zig.
//!
//! This is uses the current compiler tokenizer as a partial reference, but the
//! overall structure is quite different and should be a bit easier to follow.
//!
//! This also keeps track of comment information as tokens for documentation generation.

const std = @import("std");
const debug = std.debug;
const ArrayList = std.ArrayList;

/// A TokenId represents a kind of token. It will also hold its associated data if present.
const TokenId = enum {
    Comment,            // `//`  case
    DocComment,         // `///` case
    ModuleDocComment,   // `//!` case

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
    Comma,
    Dash,
    DivEq,
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

/// A Token consists of a type/id and an associated location/span within the source file.
const Token = struct {
    id: TokenId,
    span: Span,
    // TODO: Add an extra data field which has specific data used for some instances.
};

/// A Span represents a contiguous sequence (byte-wise) of a source file.
const Span = struct {
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

const Tokenizer = struct {
    const Self = this;

    tokens: ArrayList(Token),
    lines: ArrayList(usize),

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
    fn next(self: &Self) -> ?u8 {
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

    /// Mark the current position as the end of a token and set it to a new id.
    fn setEndToken(self: &Self, id: TokenId) -> %void {
        self.setToken(id);
        %return self.endToken();
    }

    /// Construct a single byte token and push onto the token list.
    fn singleToken(self: &Self, id: TokenId) -> %void {
        self.beginToken();
        %return self.setEndToken(id);
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
    // TODO: Return the integer value and allow caller to handle it.
    fn processNumber(self: &Self, comptime radix: u8, init_value: ?u8) -> %u128 {
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

        %return self.setEndToken(TokenId.IntLiteral);
        number
    }

    /// Process a character code of the specified length and type.
    ///
    /// Returns the utf8 encoded value of the codepoint.
    ///
    /// This will only modify the current stream position.
    fn processCharCode(self: &Self,
        comptime radix: u8, comptime count: u8, comptime is_unicode: bool) -> %u32
    {
        var char_code: u32 = 0;

        comptime var i: usize = 0;
        inline while (i < count) : (i += 1) {
            char_code *= radix;
            char_code += %return getDigitValueForRadix(radix, self.peek(0));
            self.bump(1);
        }

        // TODO: Do we really want to return these values or deal with them here?
        if (is_unicode) {
            if (char_code <= 0x7f) {
                // push(char_code)
            } else if ((??self.c_token).id == TokenId.CharLiteral) {
                // TODO: Handle this at the caller side.
                // !cannot push multi-byte unicode literal
            } else if (char_code <= 0x7ff) {
                // push(0xc0 | (char_code >> 6))
                // push(0x80 | (char_code & 0x3f))
            } else if (char_code <= 0xffff) {
                // push(0xe0 | (char_code >> 12))
                // push(0x80 | ((char_code >> 6) & 0x3f))
                // push(0x80 | (char_code & 0x3f))
            } else if (char_code <= 0x10ffff) {
                // push(0xf0 | (char_code >> 18))
                // push(0x80 | ((char_code >> 12) & 0x3f))
                // push(0x80 | ((char_code >> 6) & 0x3f)
                // push(0x80 | (char_code & 0x3f))
            } else {
                // !value out of range
                return error.UnicodeCharCodeOutOfRange;
            }
        } else {
            // push(char_code)
        }

        // TODO: Construct and push right code
        u32(0)
    }

    /// Process an escape code.
    ///
    /// Expects the '/' has already been handled by the caller.
    ///
    /// Returns the utf8 encoded value of the codepoint.
    fn processStringEscape(self: &Self) -> %u32 {
        switch (self.peek(0)) {
            'x' => {
                self.bump(1);
                self.processCharCode(16, 2, false)
            },

            'u' => {
                self.bump(1);
                self.processCharCode(16, 4, true)
            },

            'U' => {
                self.bump(1);
                self.processCharCode(16, 6, true)
            },

            // TODO: Push the associated character to the right buffer
            'n', 'r', '\\', 't', '\'', '"' => |c| {
                self.bump(1);
                // push(c)
                u32(c)
            },

            else => {
                @panic("unexpected character");
            }
        }
    }

    /// Construct a new tokenization instance and return it in its completed state.
    ///
    // Adapt in the future to return a stream of tokens?
    // TODO: How would this handle the line offsets though? Could return an enum of a line offset
    // or a token, or just keep new lines as tokens themselves?
    pub fn process(buf: []const u8) -> %Self {
        var t = Self.init(buf);
        %defer t.deinit();

        // TODO: Offset is one too great. Should be a peek here or at least perform
        // a beginToken before we get the token or offset it by 1.
        while (t.next()) |ch| {
            switch (ch) {
                ' ', '\r', '\n', '\t' => {},

                '(' => %return t.singleToken(TokenId.LParen),
                ')' => %return t.singleToken(TokenId.RParen),
                ',' => %return t.singleToken(TokenId.Comma),
                '{' => %return t.singleToken(TokenId.LBrace),
                '}' => %return t.singleToken(TokenId.RBrace),
                '[' => %return t.singleToken(TokenId.LBracket),
                ']' => %return t.singleToken(TokenId.RBracket),
                ';' => %return t.singleToken(TokenId.Semicolon),
                ':' => %return t.singleToken(TokenId.Colon),
                '#' => %return t.singleToken(TokenId.NumberSign),
                '~' => %return t.singleToken(TokenId.Tilde),

                '_', 'a' ... 'z', 'A' ... 'Z' => {
                    t.beginToken();

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
                        %return t.setEndToken(id);
                    } else {
                        %return t.setEndToken(TokenId.Symbol);
                    }
                },

                '0' => {
                    t.beginToken();

                    const value = switch (t.peek(0)) {
                        'b' => {
                            t.bump(1);
                            %return t.processNumber(2, null)
                        },

                        'o' => {
                            t.bump(1);
                            %return t.processNumber(8, null)
                        },

                        'x' => {
                            t.bump(1);
                            %return t.processNumber(16, null)
                        },

                        else => {
                            // TODO: disallow anything after a 0 except a dot.
                            %return t.processNumber(10, null)
                        },
                    };

                    // push(number)
                },

                '1' ... '9' => {
                    t.beginToken();
                    const value = %return t.processNumber(10, ch);
                    // push(number)
                },

                '"' => {
                    t.beginToken();

                    var literal = ArrayList(u8).init(&debug.global_allocator);

                    while (true) {
                        switch (t.peek(0)) {
                            '"' => {
                                t.bump(1);
                                break;
                            },

                            '\n' => {
                                return error.NewlineInStringLiteral;
                            },

                            '\\' => {
                                t.bump(1);
                                const value = %return t.processStringEscape();
                                // push(value)
                            },

                            0 => {
                                @panic("eof while parsing string literal!");
                            },

                            else => |c| {
                                t.bump(1);
                                %return literal.append(c);
                            },
                        }
                    }

                    %return t.setEndToken(TokenId.StringLiteral);
                },

                '-' => {
                    t.beginToken();
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
                    t.beginToken();
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
                    t.beginToken();
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
                    t.beginToken();
                    switch (t.peek(0)) {
                        '/' => {
                            t.bump(1);

                            const comment_id = switch (t.peek(0)) {
                                '!' => TokenId.ModuleDocComment,
                                '/' => TokenId.DocComment,
                                else => TokenId.Comment,
                            };

                            while (t.next()) |c| {
                                if (c == '\n') {
                                    break;
                                }
                            }

                            // TODO: Merge adjacent comments at a later level
                            %return t.setEndToken(comment_id);
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
                    t.beginToken();
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
                    t.beginToken();
                    switch (t.peek(0)) {
                        '"' => {
                            // Process a raw symbol until the closing
                        },

                        else => {
                            %return t.setEndToken(TokenId.AtSign);
                        },
                    }
                },

                '&' => {
                    t.beginToken();
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
                    t.beginToken();
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
                    t.beginToken();
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
                    t.beginToken();
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
                    t.beginToken();
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
                    t.beginToken();
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
                    t.beginToken();
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
                    t.beginToken();
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
                    t.beginToken();
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
                    t.beginToken();
                    switch (t.peek(0)) {
                        '\'' => {
                            return error.MissingCharLiteralData;
                        },

                        '\\' => {
                            t.bump(1);
                            const value = %return t.processStringEscape();
                            // push(value)

                            // TODO: Should we handle end in string escape?
                            if (t.peek(0) != '\'') {
                                return error.ExtraCharLiteralData;
                            } else {
                                t.bump(1);
                                %return t.setEndToken(TokenId.CharLiteral);
                            }
                        },

                        else => {
                            if (t.peek(1) != '\'') {
                                return error.ExtraCharLiteralData;
                            } else {
                                t.bump(2);
                                %return t.setEndToken(TokenId.CharLiteral);
                            }
                        },
                    }
                },

                '\\' => {
                    t.beginToken();
                    switch (t.peek(0)) {
                        '\\' => {
                            t.bump(1);

                            while (t.next()) |c| {
                                if (c == '\n') {
                                    break;
                                }
                            }

                            // Merge adjacent literals at a later level.
                            // We don't want to do this for standard literals hence the different
                            // type.
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
        }

        t
    }
};

const os = std.os;
const io = std.io;
const Buffer = std.Buffer;

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
    for (t.tokens.toSliceConst()) |x| {
        %%std.io.stdout.printf(
            "{}:{} - {}\n",
            x.span.start_line, x.span.start_column, @enumTagName(x.id),
        );
    }
}
