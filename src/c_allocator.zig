const mem = @import("std").mem;
const c = @cImport({@cInclude("stdlib.h")});

error NoMem;

pub var c_allocator = mem.Allocator {
    .allocFn = cAlloc,
    .reallocFn = cRealloc,
    .freeFn = cFree,
};

// The C stdlib returns a pointer to memory `&c_void` so we reinterpret the data as a u8 slice with
// an implicit length before giving it back to the user.
fn cAlloc(self: &mem.Allocator, n: usize, alignment: usize) -> %[]u8 {
    @ptrCast(&u8, c.malloc(usize(n)) ?? return error.NoMem)[0..n]
}

fn cRealloc(self: &mem.Allocator, old_mem: []u8, new_size: usize, alignment: usize) -> %[]u8 {
    // Zig currently allows implementors to call realloc on an empty slice. This causes a slight
    // issue when interfacing with C since there will be no existing pointer to memory we can use.
    if (old_mem.len == 0) {
        cAlloc(self, new_size, alignment)
    } else {
        const old_ptr = @ptrCast(&c_void, &old_mem[0]);
        @ptrCast(&u8, c.realloc(old_ptr, usize(new_size)) ?? return error.NoMem)[0..new_size]
    }
}

fn cFree(self: &mem.Allocator, old_mem: &u8) {
    c.free(@ptrCast(&c_void, &old_mem[0]));
}
