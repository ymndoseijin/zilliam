const geo = @import("geo.zig");
const blades = @import("blades.zig");
const std = @import("std");

pub fn PGA(comptime T: type, comptime dim: usize) type {
    return struct {
        pub const Algebra = geo.Algebra(T, dim, 0, 1);

        const branch = blk: {
            var len = 0;
            var branch_arr: [len]usize = undefined;
            var count = 0;
            for (Algebra.Indices, 0..) |data, i| {
                if (data.count == dim - 1) {
                    const is_valid = valid: {
                        for (data.tags[0..data.count]) |tag| {
                            if (tag == 1) break :valid false;
                        }
                        break :valid true;
                    };
                    if (is_valid) {
                        branch_arr[count] = i;
                        count += 1;
                    }
                }
            }
            break :blk branch_arr;
        };

        const extra_blades = 2;

        pub const Blades = blades.Blades(Algebra, .{ branch, .{ 5, 6, 7 } }).Types;

        pub const Types = blk: {
            var temp_types: [dim]type = undefined;
            const types_ptr = &temp_types;
            for (&temp_types, 0..) |*current_type, t_i| {
                current_type.* = struct {
                    const Type = if (t_i == dim - 1) Blades[1].HodgeResult else Blades[t_i + 1];
                    const ReturnVec = if (t_i == dim - 1) [Type.Count - 1]T else [Type.Count]T;

                    const ShapeTypes = types_ptr;

                    const Self = @This();

                    pub fn create(vec: ReturnVec) Type {
                        if (t_i != dim - 1) return .{ .val = vec };

                        var temp = Blades[1]{};
                        for (0..dim) |i| {
                            temp.val[i + 1] = vec[i];
                        }
                        temp.val[0] = 1;
                        return temp.dual();
                    }

                    pub fn get(a: Type) ReturnVec {
                        if (t_i != dim - 1) {
                            var val = normalize(a);
                            var temp: ReturnVec = undefined;
                            for (&temp, val.val[1..]) |*t, v| {
                                t.* = v;
                            }
                            return temp;
                        }

                        var val = normalize(a).undual();
                        var temp: ReturnVec = undefined;
                        for (&temp, val.val[1..]) |*t, v| {
                            t.* = v;
                        }
                        return temp;
                    }
                };
            }
            break :blk temp_types;
        };

        pub const Point = if (dim >= 1) Types[dim - 1] else void;
        pub const Line = if (dim >= 2) Types[dim - 2] else void;
        pub const Plane = if (dim >= 3) Types[dim - 3] else void;
        pub const Cube = if (dim >= 4) Types[dim - 4] else void;

        pub fn normalize(a: anytype) @TypeOf(a) {
            const U = @TypeOf(a);
            var coeff: @Vector(U.Count, T) = @splat(a.val[U.Count - 1]);
            return U{ .val = a.val / coeff };
        }
    };
}

test "2D PGA" {
    const Pga = PGA(f32, 2);

    const Point = Pga.Point;
    const A = Point.create(.{ -1, -1 });
    const C = Point.create(.{ 1, 1 });

    const L = Point.create(.{ 0, 0.5 }).regressive(Point.create(.{ 1, -0.5 }));

    const AC = A.regressive(C);
    const D = L.wedge(AC);

    try std.testing.expectEqualSlices(f32, &.{ 0.25, 0.25 }, &Point.get(D));
}

test "3D PGA" {
    const Pga = PGA(f32, 3);

    const Point = Pga.Point;
    const A = Point.create(.{ -1, -1, -1 });
    const C = Point.create(.{ 1, 1, 1 });

    const z = 1.0;
    const L = Point.create(.{ 0, 0, z }).regressive(Point.create(.{ 1, 0, z })).regressive(Point.create(.{ 0, 1, z }));

    const AC = A.regressive(C);
    const D = L.wedge(AC);

    const O = Point.create(.{ 0, 0, 0 });

    try std.testing.expectEqualSlices(f32, &.{ 1.0, 1.0, 1.0 }, &Point.get(D));
    var buf: [2048]u8 = undefined;
    std.debug.print("\n{s}\n", .{try L.print(&buf)});
    std.debug.print("\n{s}\n", .{try A.print(&buf)});
    std.debug.print("\n{s}\n", .{try L.wedge(A).print(&buf)});
    std.debug.print("\nOC: {any}\n", .{O.regressive(C)});
    std.debug.print("\n{d}\n", .{(try Pga.Algebra.eval("e01+e02+e03", .{})).val});
}
