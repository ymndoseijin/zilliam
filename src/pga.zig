const geo = @import("geo.zig");
const blades = @import("blades.zig");
const std = @import("std");
const comath = @import("comath");

pub fn PGA(comptime T: type, comptime dim: usize) type {
    return struct {
        pub const Algebra = geo.Algebra(T, dim, 0, 1);

        const origin_idx = blk: {
            const res = Algebra.Blades.e0.hodge().val;
            for (res, 0..) |v, i| {
                if (v == 1) break :blk i;
            }
            unreachable;
        };

        const extra_masks = branch_blk: {
            var temp = struct {
                pub const Array = .{};
            };
            for (1..dim + 1) |k| {
                const res_arr = blk: {
                    var branch_arr: []const usize = &.{};
                    var ideal_arr: []const usize = &.{};

                    for (Algebra.Indices, 0..) |data, i| {
                        if (data.count != k) continue;
                        const is_valid = valid: {
                            for (data.tags[0..data.count]) |tag| {
                                if (tag == 1) break :valid false;
                            }
                            break :valid true;
                        };
                        if (is_valid) {
                            branch_arr = branch_arr ++ .{i};
                        } else {
                            ideal_arr = ideal_arr ++ .{i};
                        }
                    }
                    break :blk .{ branch_arr, ideal_arr };
                };
                const new = temp.Array ++ .{ res_arr[0], res_arr[1] };
                temp = struct {
                    pub const Array = new;
                };
            }
            break :branch_blk temp.Array ++ .{.{origin_idx}};
        };

        pub const Blades = blades.Blades(Algebra, extra_masks);

        pub const Origin = Blades.FormatTypes[Blades.FormatTypes.len - 1]{ .val = .{1} };

        pub const Types = blk: {
            var temp_types: [dim]type = undefined;
            for (&temp_types, 0..) |*current_type, t_i| {
                current_type.* = struct {
                    pub const Type = if (t_i == dim - 1) Blades.Blades[1].HodgeResult else Blades.Blades[t_i + 1];
                    const ReturnVec = if (t_i == dim - 1) [Type.Count - 1]T else [Type.Count]T;

                    const ShapeTypes = PGA(T, dim).Types;

                    const Self = @This();

                    pub fn create(vec: ReturnVec) Type {
                        if (t_i != dim - 1) return .{ .val = vec };

                        var temp = Blades.Blades[1]{};
                        for (0..dim) |i| {
                            temp.val[i + 1] = vec[i];
                        }
                        temp.val[0] = 1;

                        return temp.dual();
                    }

                    pub fn get(a: anytype) ReturnVec {
                        if (t_i != dim - 1) {
                            var val = normalize(a);
                            var temp: ReturnVec = undefined;
                            for (&temp, val.val[1..]) |*t, v| {
                                t.* = v;
                            }
                            return temp;
                        }

                        var val = normalize(a.undual()).grade_projection(1);
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
            const coeff: @Vector(U.Count, T) = @splat(a.get(.e0));
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
    const B = Point.create(.{ -1, 1, 1 });
    const C = Point.create(.{ 1, 1, 1 });

    const z = 1.0;
    const L = Point.create(.{ 0, 0, z }).regressive(Point.create(.{ 1, 0, z })).regressive(Point.create(.{ 0, 1, z }));

    const AC = A.regressive(C);
    const D = L.wedge(AC);

    const O = Pga.Origin;

    try std.testing.expectEqualSlices(f32, &.{ 1.0, 1.0, 1.0 }, &Point.get(D));
    try std.testing.expectEqualSlices(f32, &.{ 0, -2.0, 2.0 }, &O.regressive(C).regressive(B).val);
}
