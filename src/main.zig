const std = @import("std");

const Algebra = @import("geo.zig").Algebra;

const vals = Algebra(i32, 2, 1, 1).getBladeType();

const packed_vec = extern struct { val: [6]c_int };
const packed_res = extern struct { val: [8]c_int };

export fn mul_abi(a: packed_vec, b: packed_vec) packed_res {
    return .{ .val = vals.mul(vals.Types[2]{ .val = a.val }, vals.Types[2]{ .val = b.val }).val };
}

pub fn main() !void {
    var a = vals.Types[2]{};
    var b = vals.Types[2]{};
    a.val[2] = 1;
    b.val[0] = 1;
    std.debug.print("\na {any}\n", .{vals.mul(a, b)});
}
