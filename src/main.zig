const std = @import("std");

const Algebra = @import("geo.zig").Algebra;

pub fn main() !void {
    const Alg = Algebra(i32, 2, 1, 1);

    var acc = Alg{ .val = .{0} ** 16 };
    for (0..200000) |_| {
        acc = acc.add((Alg{
            .val = .{ 7, 0, 2, 3, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0 },
        }).wedge(.{
            .val = .{ 19, 0, 11, 13, 0, 0, 0, 0, 17, 0, 0, 0, 0, 0, 0, 0 },
        }));

        acc = acc.add((Alg{
            .val = .{ 7, 23, 2, 3, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0 },
        }).mul(.{
            .val = .{ 19, 31, 11, 13, 0, 0, 0, 0, 17, 0, 0, 0, 0, 0, 0, 0 },
        }));

        acc = acc.add((Alg{
            .val = .{ 7, 11, 2, 3, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0 },
        }).hodge());

        acc = acc.add((Alg{
            .val = .{ 1, 2, 3, 4, 5, -6, -7, -8, -9, -10, -11, -12, -13, -14, -15, 16 },
        }).reverse());

        const val = Alg{ .val = .{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 } };
        acc = acc.add((try val.grade_projection(2)));

        acc = acc.add((Alg{
            .val = .{ 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
        }).abs2());

        acc = acc.add((Alg{
            .val = .{ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
        }).dual());

        acc = acc.add((Alg{
            .val = .{ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
        }).undual());

        acc = acc.add((Alg{
            .val = .{ 7, 23, 2, 3, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0 },
        }).inner(.{
            .val = .{ 19, 31, 11, 13, 0, 0, 0, 0, 17, 0, 0, 0, 0, 0, 0, 0 },
        }));

        acc = acc.add((Alg{
            .val = .{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 },
        }).regressive(.{
            .val = .{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0 },
        }));
    }

    std.debug.print("{any}\n", .{acc.val});
}
