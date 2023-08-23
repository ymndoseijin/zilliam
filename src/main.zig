const packed_vec = extern struct { val: [Bivector.Count]f32 };
const packed_res = extern struct { val: [Rotor.Count]f32 };

const Rotor = Types[Types.len - 1];

export fn mul_abi(a: packed_vec, b: packed_vec) packed_res {
    return .{ .val = Blades.mul(Bivector{ .val = a.val }, Bivector{ .val = b.val }).val };
}

const std = @import("std");

const Algebra = @import("geo.zig").Algebra;
const getBlades = @import("blades.zig").Blades;

const Alg = Algebra(f32, 3, 0, 1);
const Blades = getBlades(Alg);
const Types = Blades.Types;

const Vector = Types[1];
const Bivector = Types[2];
const Trivector = Types[3];

pub fn main() !void {
    const BivectorBatch = Blades.getBatchType(Bivector, 1);
    const VectorBatch = Blades.getBatchType(Vector, 1);

    for (0..Bivector.Count) |a_i| {
        for (0..Vector.Count) |b_i| {
            var a = BivectorBatch{};
            var b = VectorBatch{};
            a.val[a_i] = .{1};
            b.val[b_i] = .{1};
            var buf: [2048]u8 = undefined;

            // This is a Trivector, it gets properly dispatched
            const r_w = a.wedge(b).get(0);

            var r_s = try a.get(0).print(&buf);
            std.debug.print("\n{s} ^ ", .{r_s});
            r_s = try b.get(0).print(&buf);
            std.debug.print("{s} = ", .{r_s});
            r_s = try r_w.print(&buf);
            std.debug.print("{s}\n", .{r_s});
        }
    }
}
