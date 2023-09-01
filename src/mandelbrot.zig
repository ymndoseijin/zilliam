const geo = @import("geo.zig");
const std = @import("std");

const Complex = geo.Algebra(f32, 0, 1, 0);

pub fn mandelbrot(comptime size: usize, comptime steps: usize) type {
    var data: [size][2 * size]bool = undefined;
    for (0..2 * size) |x| {
        for (0..size) |y| {
            const x_f: f32 = @floatFromInt(x);
            const y_f: f32 = @floatFromInt(y);
            const c: Complex = .{ .val = .{ (x_f / size - 1.0) * 2, (y_f / size - 0.5) * 2 } };
            var it: Complex = .{ .val = .{ 0, 0 } };
            data[y][x] = true;
            for (0..steps) |_| {
                it = it.mul(it).add(c);
                const vec: @Vector(2, f32) = it.val;
                if (@reduce(.Add, vec * vec) > steps) {
                    data[y][x] = false;
                    break;
                }
            }
        }
    }

    const data_cons = data;
    return struct {
        pub const Data = data_cons;
    };
}

pub fn main() !void {
    const Mandelbrot = mandelbrot(50, 80);
    std.debug.print("\n", .{});
    for (Mandelbrot.Data) |column| {
        for (column) |elem| {
            if (elem) {
                std.debug.print("#", .{});
            } else {
                std.debug.print(" ", .{});
            }
        }
        std.debug.print("\n", .{});
    }
}
