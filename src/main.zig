const std = @import("std");

const Algebra = @import("geo.zig").Algebra;

const Alg = Algebra(f32, 3, 0, 1);
const Blades = Alg.getBladeType();
const Types = Blades.Types;

const Bivector = Types[2];
const Trivector = Types[3];
const Rotor = Types[Types.len - 1];

const packed_vec = extern struct { val: [Bivector.Count]f32 };
const packed_res = extern struct { val: [Rotor.Count]f32 };

export fn mul_abi(a: packed_vec, b: packed_vec) packed_res {
    return .{ .val = Blades.mul(Bivector{ .val = a.val }, Bivector{ .val = b.val }).val };
}

export fn wedge_abi(a: packed_vec, b: packed_vec) packed_res {
    return .{ .val = Blades.wedge(Bivector{ .val = a.val }, Bivector{ .val = b.val }).val };
}

pub fn main() !void {
    for (0..Bivector.Count) |a_i| {
        for (0..Bivector.Count) |b_i| {
            var a = Bivector{};
            var b = Bivector{};
            a.val[a_i] = 1;
            b.val[b_i] = 1;
            const r = Blades.mul(a, b);
            var buf: [2048]u8 = undefined;
            var r_s = try a.print(&buf);
            std.debug.print("\n{s} * ", .{r_s});
            r_s = try b.print(&buf);
            std.debug.print("{s} = ", .{r_s});
            r_s = try r.print(&buf);
            std.debug.print("{s}\n", .{r_s});
        }
    }
}
