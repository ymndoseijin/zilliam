const geo = @import("geo.zig");
const std = @import("std");

pub fn PGA(comptime T: type, comptime dim: usize) type {
    return struct {
        pub const Algebra = geo.Algebra(T, dim, 0, 1);

        pub fn point(vec: [dim]T) Algebra {
            var temp = Algebra{};
            for (vec, 0..) |val, i| {
                temp.val[i + 2] = val;
            }
            temp.set(.e0, 1);
            return temp.dual();
        }

        pub fn normalize(a: Algebra) Algebra {
            var coeff: @Vector(Algebra.BasisNum + 1, T) = @splat(a.get(.e12));
            return Algebra{ .val = a.val / coeff };
        }

        pub fn getPoint(a: Algebra) [2]T {
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
