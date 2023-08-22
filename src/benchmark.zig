const std = @import("std");

const Algebra = @import("geo.zig").Algebra;
const Alg = Algebra(f32, 3, 0, 0);

const packed_vec = extern struct { val: [Alg.BasisNum + 1]f32 };
export fn wedge_abi(a: packed_vec, b: packed_vec) packed_vec {
    return .{ .val = (Alg{ .val = a.val }).wedge(Alg{ .val = b.val }).val };
}

export fn mul_abi(a: packed_vec, b: packed_vec) packed_vec {
    return .{ .val = (Alg{ .val = a.val }).mul(Alg{ .val = b.val }).val };
}

export fn hodge_abi(a: packed_vec) packed_vec {
    return .{ .val = (Alg{ .val = a.val }).hodge().val };
}

export fn reverse_abi(a: packed_vec) packed_vec {
    return .{ .val = (Alg{ .val = a.val }).reverse().val };
}

export fn abs2_abi(a: packed_vec) packed_vec {
    return .{ .val = (Alg{ .val = a.val }).abs2().val };
}

export fn dual_abi(a: packed_vec) packed_vec {
    return .{ .val = (Alg{ .val = a.val }).dual().val };
}

export fn undual_abi(a: packed_vec) packed_vec {
    return .{ .val = (Alg{ .val = a.val }).undual().val };
}

export fn inner_abi(a: packed_vec, b: packed_vec) packed_vec {
    return .{ .val = (Alg{ .val = a.val }).inner(Alg{ .val = b.val }).val };
}

export fn regressive_abi(a: packed_vec, b: packed_vec) packed_vec {
    return .{ .val = (Alg{ .val = a.val }).regressive(Alg{ .val = b.val }).val };
}

pub fn main() !void {
    var checksum: @Vector(Alg.BasisNum + 1, f32) = .{0} ** (Alg.BasisNum + 1);
    for (0..500) |i| {
        for (0..500) |j| {
            const i_v: packed_vec = blk: {
                var temp: [Alg.BasisNum + 1]f32 = undefined;
                inline for (&temp, 0..) |*v, v_i| {
                    v.* = v_i * @as(f32, @floatFromInt(i));
                }
                break :blk .{ .val = temp };
            };
            const j_v: packed_vec = blk: {
                var temp: [Alg.BasisNum + 1]f32 = undefined;
                inline for (&temp, 0..) |*v, v_i| {
                    v.* = v_i * @as(f32, @floatFromInt(j));
                }
                break :blk .{ .val = temp };
            };
            checksum += @call(.never_inline, mul_abi, .{ i_v, j_v }).val;
            checksum += @call(.never_inline, wedge_abi, .{ i_v, j_v }).val;
            //checksum += @call(.never_inline, inner_abi, .{ i_v, j_v }).val;
            //checksum += @call(.never_inline, regressive_abi, .{ i_v, j_v }).val;
        }
    }

    std.debug.print("{any}\n", .{checksum});
}
