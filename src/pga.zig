const geo = @import("geo.zig");
const blades = @import("blades.zig");
const std = @import("std");

pub fn PGA(comptime T: type, comptime dim: usize) type {
    return struct {
        pub const Algebra = geo.Algebra(T, dim, 0, 1);
        pub const Blades = blades.Blades(Algebra).Types;

        pub const Point = Blades[1].HodgeResult;

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
            var coeff: @Vector(U.Count, T) = @splat(a.get(.e12));
            return U{ .val = a.val / coeff };
        }

        pub fn getPoint(a: Point) [2]T {
            var val = normalize(a).dual();
            return .{ val.get(.e1), val.get(.e2) };
        }
    };
}

test "2D PGA" {
    const Pga = PGA(f32, 2);
    const A = Pga.point(.{ -1, -1 });
    const C = Pga.point(.{ 1, 1 });

    const L = Pga.point(.{ 0, 0.5 }).regressive(Pga.point(.{ 1, -0.5 }));

    const M = C.regressive(A);
    const D = L.wedge(M);

    std.debug.print("\n{any}\n", .{Pga.getPoint(D)});
}
