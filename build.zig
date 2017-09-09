const Builder = @import("std").build.Builder;

pub fn build(b: &Builder) {
    const mode = b.standardReleaseOptions();
    const exe = b.addExecutable("tokenizer", "src/tokenizer.zig");
    exe.setBuildMode(mode);
    exe.setOutputPath("./tokenizer");

    b.default_step.dependOn(&exe.step);
    b.installArtifact(exe);
}
