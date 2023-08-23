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
    const BivectorBatch = Bivector.getBatchType(2);
    const VectorBatch = Vector.getBatchType(2);

    for (0..Bivector.Count) |a_i| {
        for (0..Vector.Count) |b_i| {
            var a = BivectorBatch{};
            var b = VectorBatch{};
            a.val[a_i] = .{ 1, 2 };
            b.val[b_i] = .{ 1, 2 };
            var buf: [2048]u8 = undefined;

            // This is a Trivector, it gets properly dispatched
            const res = a.mul(b);

            for (0..2) |i| {
                const r_w = res.get(i);

                var r_s = try a.get(i).print(&buf);
                std.debug.print("{s} ^ ", .{r_s});
                r_s = try b.get(i).print(&buf);
                std.debug.print("{s} = ", .{r_s});
                r_s = try r_w.print(&buf);
                std.debug.print("{s}\n", .{r_s});
            }
            std.debug.print("\n", .{});
        }
    }
}

// ...
// 1.0000e13 ^ 1.0000e0 = 1.0000e013
// 2.0000e13 ^ 2.0000e0 = 4.0000e013
// ...
