const std = @import("std");

const geo = @import("geo.zig");
const Algebra = geo.Algebra;
const getBlades = @import("blades.zig").Blades;

const Alg = Algebra(f32, 3, 0, 1);
const Blades = getBlades(Alg, .{});
const Types = Blades.FormatTypes;

const Vector = Types[1];
const Bivector = Types[2];
const Trivector = Types[3];

const packed_vec = extern struct { val: [Bivector.Count]f32 };
const packed_res = extern struct { val: [Even.Count]f32 };

const Even = Types[Types.len - 1];

export fn mul_abi(a: packed_vec, b: packed_vec) packed_res {
    return .{ .val = (Bivector{ .val = a.val }).mul(Bivector{ .val = b.val }).val };
}

pub fn main() !void {
    var checksum: @Vector(Even.Count, f32) = .{0} ** Even.Count;

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
