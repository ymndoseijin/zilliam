const geo = @import("geo.zig");
const blades = @import("blades.zig");
const std = @import("std");

pub fn PGA(comptime T: type, comptime dim: usize) type {
    return struct {
        pub const Algebra = geo.Algebra(T, dim, 0, 1);
        pub const Blades = blades.Blades(Algebra).Types;

        pub const Point = Blades[1].HodgeResult;
        pub const Line = Blades[2].HodgeResult;

        pub fn point(vec: [dim]T) Point {
            var temp = Blades[1]{};
            for (0..dim) |i| {
                temp.val[i + 1] = vec[i];
            }
            temp.set(.e0, 1);
            return temp.hodge();
        }

        pub fn normalize(a: anytype) @TypeOf(a) {
            const U = @TypeOf(a);
            var coeff: @Vector(U.Count, T) = @splat(a.val[U.Count - 1]);
            return U{ .val = a.val / coeff };
        }

        pub fn getPoint(a: Point) [dim]T {
            var val = normalize(a).dual();
            var temp: [dim]T = undefined;
            for (&temp, val.val[1..]) |*t, v| {
                t.* = v;
            }
            return temp;
        }
    };
}

test "2D PGA" {
    const Pga = PGA(f32, 2);
    const A = Pga.point(.{ -1, -1 });
    const C = Pga.point(.{ 1, 1 });

    const L = Pga.point(.{ 0, 0.5 }).regressive(Pga.point(.{ 1, -0.5 }));

    const AC = A.regressive(C);
    const D = L.wedge(AC);

    std.debug.print("\n{any}\n", .{Pga.getPoint(D)});
    std.debug.print("\n{any}\n", .{Pga.Line.K});
}
