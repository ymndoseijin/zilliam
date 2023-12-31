const std = @import("std");

const Algebra = @import("geo.zig").Algebra;
const Alg = Algebra(i32, 0, 2, 0);

const packed_vec = extern struct { val: [Alg.BasisNum + 1]i32 };
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
    var checksum: @Vector(Alg.BasisNum + 1, i32) = .{0} ** (Alg.BasisNum + 1);
    for (0..5000000) |i_in| {
        var acc: @Vector(Alg.BasisNum + 1, i32) = .{0} ** (Alg.BasisNum + 1);
        const i: i32 = @intCast(i_in % 200);

        acc += mul_abi(
            .{ .val = .{ i + 2, i + 3, i + 5, i + 7, i + 11, i + 13, i + 17, i + 19, i + 23, i + 29, i + 31, i + 37, i + 41, i + 43, i + 47, i + 53 } },
            .{ .val = .{ i + 73, i + 79, i + 83, i + 89, i + 97, i + 101, i + 103, i + 107, i + 109, i + 113, i + 127, i + 131, i + 137, i + 139, i + 149, i + 151 } },
        ).val;

        checksum += acc;
    }
    std.debug.print("{any}\n", .{checksum});
}
