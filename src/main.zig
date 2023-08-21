const std = @import("std");

const Algebra = @import("geo.zig").Algebra;

const alg = Algebra(f32, 3, 0, 1);
const vals = alg.getBladeType();

const packed_vec = extern struct { val: [vals.Types[2].Count]f32 };
const packed_res = extern struct { val: [vals.Types[vals.Types.len - 1].Count]f32 };

export fn mul_abi(a: packed_vec, b: packed_vec) packed_res {
    return .{ .val = vals.mul(vals.Types[2]{ .val = a.val }, vals.Types[2]{ .val = b.val }).val };
}

pub fn main() !void {
    var a = vals.Types[2]{};
    var b = vals.Types[2]{};
    a.val[2] = 1;
    b.val[0] = 1;
    std.debug.print("\na {any}\n", .{vals.mul(a, b)});

    const res = alg.posOp.Res;

    inline for (res[0], res[1]) |sel_a, sel_b| {
        std.debug.print("a.", .{});

        for (sel_a) |val| {
            if (val == -1) {
                std.debug.print("n", .{});
            } else {
                std.debug.print("({})", .{val});
            }
        }
        std.debug.print(" *\nb.", .{});

        for (sel_b) |val| {
            if (val == -1) {
                std.debug.print("n", .{});
            } else {
                std.debug.print("({})", .{val});
            }
        }
        std.debug.print(" +\n\n", .{});
    }
    std.debug.print("\n", .{});
}
