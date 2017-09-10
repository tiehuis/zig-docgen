const Builder = @import("std").build.Builder;

pub fn build(b: &Builder) {
    const mode = b.standardReleaseOptions();
    const exe = b.addExecutable("docgen", "src/main.zig");

    exe.linkSystemLibrary("c");

    exe.setBuildMode(mode);
    exe.setOutputPath("./docgen");

    b.default_step.dependOn(&exe.step);
    b.installArtifact(exe);
}
