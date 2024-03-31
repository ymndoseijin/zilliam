const std = @import("std");

const geo = @import("geo.zig");
const Algebra = geo.Algebra;
const getBlades = @import("blades.zig").Blades;

const Alg = Algebra(f32, 3, 0, 1);
const Blades = getBlades(Alg, .{});

const Vector = Blades.Blades[1];
const Bivector = Blades.Blades[2];
const Trivector = Blades.Blades[3];

const Even = Blades.Even;

const packed_vec = extern struct { val: [Bivector.Count]f32 };
const packed_res = extern struct { val: [Even.Count]f32 };

export fn mul_abi(a: packed_vec, b: packed_vec) packed_res {
    return .{ .val = (Bivector{ .val = a.val }).mul(Bivector{ .val = b.val }).val };
}

pub fn main() !void {
    var checksum: @Vector(Even.Count, f32) = @splat(0);

    for (0..10000) |_| {
        for (0..Bivector.Count) |a_i| {
            var a = Bivector{};
            a.val[a_i] = 1;
            for (0..Bivector.Count) |b_i| {
                var b = Bivector{};
                b.val[b_i] = 1;

                const res = @call(.never_inline, mul_abi, .{ .{ .val = a.val }, .{ .val = b.val } }).val;
                checksum += res;
            }
        }
    }

    std.debug.print("\n{any}\n", .{checksum});
}
