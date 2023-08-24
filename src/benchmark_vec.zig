const std = @import("std");

const Algebra = @import("geo.zig").Algebra;
const Alg = Algebra(f32, 3, 0, 1);

const Batch = Alg.getBatchType(VecSize);

pub const VecSize = 8;

const packed_vec = extern struct { val: [Alg.BasisNum + 1][VecSize]f32 };
export fn wedge_abi_vec(a: packed_vec, b: packed_vec) packed_vec {
    return .{ .val = (Batch{ .val = a.val }).wedge(Batch{ .val = b.val }).val };
}

export fn mul_abi_vec(a: packed_vec, b: packed_vec) packed_vec {
    return .{ .val = (Batch{ .val = a.val }).mul(Batch{ .val = b.val }).val };
}

pub const Size = 524288 / VecSize;

pub fn main() !void {
    var checksum: @Vector(Alg.BasisNum + 1, f32) = .{0} ** (Alg.BasisNum + 1);
    for (0..Size) |i| {
        const i_v: packed_vec = blk: {
            var temp: [Alg.BasisNum + 1][VecSize]f32 = undefined;
            @setEvalBranchQuota(1284712910);
            inline for (&temp, 0..) |*vec, vec_i| {
                @setEvalBranchQuota(1284712910);
                inline for (0..VecSize) |v_i| {
                    vec[v_i] = vec_i * @as(f32, @floatFromInt(i * VecSize + v_i));
                }
            }
            break :blk .{ .val = temp };
        };

        const j_v: packed_vec = blk: {
            var temp: [Alg.BasisNum + 1][VecSize]f32 = undefined;
            @setEvalBranchQuota(1284712910);
            inline for (&temp, 0..) |*vec, vec_i| {
                @setEvalBranchQuota(1284712910);
                inline for (0..VecSize) |v_i| {
                    vec[v_i] = vec_i * @as(f32, @floatFromInt(i * VecSize + v_i + 1));
                }
            }
            break :blk .{ .val = temp };
        };

        for (@call(.never_inline, mul_abi_vec, .{ i_v, j_v }).val, 0..) |res, v_i| {
            for (res) |v| {
                checksum[v_i] += v;
            }
        }
        for (@call(.never_inline, wedge_abi_vec, .{ i_v, j_v }).val, 0..) |res, v_i| {
            for (res) |v| {
                checksum[v_i] += v;
            }
        }
    }

    std.debug.print("{any}\n", .{checksum});
}
