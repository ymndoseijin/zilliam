const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const comath_dep = b.dependency("comath", .{
        .target = target,
        .optimize = optimize,
    });

    _ = b.addModule("zilliam", .{
        .source_file = std.Build.FileSource.relative("src/geo.zig"),
        .dependencies = &.{
            .{ .name = "comath", .module = comath_dep.module("comath") },
        },
    });

    inline for (.{
        .{ "src/main.zig", "zilliam" },
        .{ "src/benchmark_f32.zig", "benchmark-f32" },
        .{ "src/benchmark_vec.zig", "benchmark-vec" },
        .{ "src/benchmark_blade.zig", "benchmark-blade" },
    }) |val| {
        const exe = b.addExecutable(.{
            .name = val[1],
            .root_source_file = .{ .path = val[0] },
            .target = target,
            .optimize = optimize,
        });

        exe.addModule("comath", comath_dep.module("comath"));

        b.installArtifact(exe);

        const run_cmd = b.addRunArtifact(exe);

        run_cmd.step.dependOn(b.getInstallStep());

        if (b.args) |args| {
            run_cmd.addArgs(args);
        }

        const run_step = b.step("run-" ++ val[1], "Run " ++ val[1]);
        run_step.dependOn(&run_cmd.step);
    }

    const test_step = b.step("test", "Run unit tests");

    inline for (.{ "src/geo.zig", "src/pga.zig", "src/blades.zig" }) |name| {
        const unit_tests = b.addTest(.{
            .root_source_file = .{ .path = name },
            .target = target,
            .optimize = optimize,
        });

        unit_tests.addModule("comath", comath_dep.module("comath"));

        const run_unit_tests = b.addRunArtifact(unit_tests);

        test_step.dependOn(&run_unit_tests.step);
    }
}
