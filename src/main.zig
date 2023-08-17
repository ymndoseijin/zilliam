const std = @import("std");

const Algebra = @import("geo.zig").Algebra;

pub fn main() !void {
    const Alg = Algebra(i32, 2, 1, 1);

    for (0..10000) |_| {
        try std.testing.expectEqualSlices(
            i32,
            &(try Alg.eval("(3*e12+2*e1)*(2*e2)", .{})).val,
            &(try Alg.eval("6*e1+4*e12", .{})).val,
        );

        try std.testing.expectEqualSlices(
            i32,
            &(try Alg.eval("(3*e12+2*e1)*(2*e2+1)", .{})).val,
            &(try Alg.eval("8*e1+7*e12", .{})).val,
        );

        try std.testing.expectEqualSlices(
            i32,
            &(try Alg.eval("(3*e12+2*e1)*e1", .{})).val,
            &(try Alg.eval("-3*e2+2", .{})).val,
        );

        // 6*e1-3*e2+4*e12+2 = 6*e1-3*e2-4*e12-2???
        // temp: "6*e1-(3*e2)+4*e12+2 seems to fix it (it's a comath bug)

        try std.testing.expectEqualSlices(
            i32,
            &(try Alg.eval("(2*e1+3*e12)*(e1+2*e2)", .{})).val,
            &(try Alg.eval("6*e1-(3*e2)+4*e12+2", .{})).val,
        );

        try std.testing.expectEqualSlices(
            i32,
            &Alg.fromInt(-15).val,
            &(try Alg.eval("(3*e12)*(5*e12)", .{})).val,
        );

        try std.testing.expectEqualSlices(
            i32,
            &(try Alg.eval("(2*e1)*(5*e12)", .{})).val,
            &(try Alg.eval("10*e2", .{})).val,
        );

        try std.testing.expectEqualSlices(
            i32,
            &(try Alg.eval("(2*e1)*(5*e12)", .{})).val,
            &(try Alg.eval("10*e2", .{})).val,
        );

        try std.testing.expectEqualSlices(
            i32,
            &(try Alg.eval("(14*e1) - (21*e2) - (7*e12) - 24", .{})).val,
            &(try Alg.eval("(2*e1+3*e2+5*e12) * (11*e1+13*e2+17*e12)", .{})).val,
        );

        try std.testing.expectEqualSlices(
            i32,
            &(try Alg.eval("129*e1 + 127*e2 + 207*e12 + 109", .{})).val,
            &(try Alg.eval("(2*e1+3*e2+5*e12+7) * (11*e1+13*e2+17*e12+19)", .{})).val,
        );

        // wedge
        try std.testing.expectEqualSlices(
            i32,
            &(try Alg.eval("133 + (115*e1) + (148*e2) + (207*e12)", .{})).val,
            &(try Alg.eval("(2*e1+3*e2+5*e12+7) ^ (11*e1+13*e2+17*e12+19)", .{})).val,
        );

        try std.testing.expectEqualSlices(
            i32,
            &Alg.fromInt(0).val,
            &(try Alg.eval("(2*e1)^(5*e12)", .{})).val,
        );

        // null
        try std.testing.expectEqualSlices(
            i32,
            &Alg.fromInt(0).val,
            &(try Alg.eval("e0*e0", .{})).val,
        );

        try std.testing.expectEqualSlices(
            i32,
            &Alg.fromInt(0).val,
            &(try Alg.eval("e0^e0", .{})).val,
        );

        try std.testing.expectEqualSlices(
            i32,
            &(try Alg.eval("654*e0 + 129*e1 + 127*e2 + 191*e01 + 206*e02 + 207*e12 + 546*e012 + 109", .{})).val,
            &(try Alg.eval("(23*e0+2*e1+3*e2+5*e12+7) * (31*e0+11*e1+13*e2+17*e12+19)", .{})).val,
        );

        // negative
        try std.testing.expectEqualSlices(
            i32,
            &Alg.fromInt(-1).val,
            &(try Alg.eval("e3*e3", .{})).val,
        );

        try std.testing.expectEqualSlices(
            i32,
            &Alg.fromInt(0).val,
            &(try Alg.eval("e3^e3", .{})).val,
        );

        // hodge
        try std.testing.expectEqualSlices(
            i32,
            &(try Alg.eval("5*e03 + 3*e013 + -2*e023 + 11*e123 + 7*e0123", .{})).val,
            &(try Alg.eval("11*e0+2*e1+3*e2+5*e12+7", .{})).hodge().val,
        );

        const big = "(1 + 2*e0 + 3*e1 + 4*e2 + 5*e3 + 6*e01 + 7*e02 + 8*e03+ 9*e12+ 10*e13+ 11*e23 + 12*e012+ 13*e013 + 14*e023 + 15*e123+ 16*e0123)";

        // reverse
        try std.testing.expectEqualSlices(
            i32,
            &(try Alg.eval("1 + 2*e0 + 3*e1 + 4*e2 + 5*e3 + -6*e01 + -7*e02 + -8*e03 + -9*e12 + -10*e13 + -11*e23 + -12*e012 + -13*e013 + -14*e023 + -15*e123 + 16*e0123", .{})).val,
            &(try Alg.eval("~" ++ big, .{})).val,
        );

        // grade projection
        try std.testing.expectEqualSlices(
            i32,
            &(try Alg.eval("6*e01 + 7*e02 + 8*e03 + 9*e12+ 10*e13 + 11*e23", .{})).val,
            &(try Alg.eval(big ++ " $ 2", .{})).val,
        );

        // abs2
        try std.testing.expectEqualSlices(
            i32,
            &Alg.fromInt(2).val,
            &(try Alg.eval("1+e1", .{})).abs2().val,
        );

        // dual
        try std.testing.expectEqualSlices(
            i32,
            &(try Alg.eval("-e023", .{})).val,
            &(try Alg.eval("*e1", .{})).val,
        );

        // undual
        try std.testing.expectEqualSlices(
            i32,
            &(try Alg.eval("e023", .{})).val,
            &(try Alg.eval("%e1", .{})).val,
        );

        // inner product
        try std.testing.expectEqualSlices(
            i32,
            &(try Alg.eval("(109 + 654*e0 + 129*e1 + 127*e2 + 214*e12)*2", .{})).val,
            &(try Alg.eval("((23*e0+2*e1+3*e2+5*e12+7) | (31*e0+11*e1+13*e2+17*e12+19))*a", .{ .a = 2 })).val,
        );

        // regressive product
        try std.testing.expectEqualSlices(
            i32,
            &Alg.fromInt(0).val,
            &(try Alg.eval("(e13) & (-e23)", .{})).val,
        );
    }
}
