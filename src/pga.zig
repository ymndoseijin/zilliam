const geo = @import("geo.zig");
const blades = @import("blades.zig");
const std = @import("std");

pub fn PGA(comptime T: type, comptime dim: usize) type {
    return struct {
        pub const Algebra = geo.Algebra(T, dim, 0, 1);
        pub const Blades = blades.Blades(Algebra).Types;

        pub const Types = blk: {
            var temp_types: [dim]type = undefined;
            for (&temp_types, 0..) |*current_type, t_i| {
                current_type.* = struct {
                    const Type = Blades[t_i + 1].HodgeResult;
                    val: Type,

                    pub fn create(vec: [Type.Count - 1]T) @This() {
                        var temp = Blades[t_i + 1]{};
                        for (0..Blades[t_i + 1].Count - 1) |i| {
                            temp.val[i + 1] = vec[i];
                        }
                        temp.val[0] = 1;
                        return .{ .val = temp.hodge() };
                    }

                    pub fn get(a: @This()) [Type.Count - 1]T {
                        var val = normalize(a.val).dual();
                        var temp: [Type.Count - 1]T = undefined;
                        for (&temp, val.val[1..]) |*t, v| {
                            t.* = v;
                        }
                        return temp;
                    }
                };
            }
            break :blk temp_types;
        };

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

    inline for (0..2) |i| {
        const AType = Pga.Types[i];
        //std.debug.print("\n{any}\n", .{Pga.getPoint(D)});
        std.debug.print("\n{any}: {}\n", .{ AType.Type, AType.Type.K });
        //std.debug.print("\n{any}\n", .{Pga.Line.K});
    }
}
