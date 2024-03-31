const geo = @import("geo.zig");
const blades = @import("blades.zig");
const std = @import("std");
const comath = @import("comath");

const GlslOp = struct {
    a: [4]usize,
    b: [4]usize,
    mul: [4]f32,
    nums: [2]usize,
};

pub fn generateGlsl(comptime Type: type, comptime struct_name: []const u8) []const u8 {
    return comptime blk: {
        const ops = Type.anticommuteOps(.pos, void{}, Type, Type);
        const vals = ops.Res;
        const ResType = ops.ResType;

        const op_a = vals[0];
        const op_b = vals[1];
        const op_m = vals[2];

        const nothings: @Vector(ResType.Count, i32) = @as([ResType.Count]i32, .{-1} ** ResType.Count);
        const neverweres: @Vector(ResType.Count, f32) = @as([ResType.Count]f32, .{0} ** ResType.Count);

        const res_len = (ResType.Count + 4 - 1) / 4;

        var op_array: [res_len][]const GlslOp = .{&.{}} ** res_len;

        for (op_a, op_b, op_m) |mask_a, mask_b, mask_m| {
            const invalid = @reduce(.And, mask_a == nothings) or @reduce(.And, mask_b == nothings) or @reduce(.And, mask_m == neverweres);

            if (!invalid) {
                for (mask_a, mask_b, mask_m, 0..) |a_idx, b_idx, m_val, res_idx| {
                    if (a_idx < 0 or b_idx < 0 or m_val == 0) continue;

                    const a_vec_num = @divFloor(a_idx, 4);
                    const a_vec_idx = @mod(a_idx, 4);

                    const b_vec_num = @divFloor(b_idx, 4);
                    const b_vec_idx = @mod(b_idx, 4);

                    const res_vec_num = @divFloor(res_idx, 4);
                    const res_vec_idx = @mod(res_idx, 4);

                    for (op_array[res_vec_num], 0..) |candidate_op, i| {
                        if (candidate_op.nums[0] == a_vec_num and
                            candidate_op.nums[1] == b_vec_num and
                            candidate_op.mul[res_vec_idx] == 0)
                        {
                            var copy: [op_array[res_vec_num].len]GlslOp = undefined;
                            @memcpy(&copy, op_array[res_vec_num]);

                            copy[i].a[res_vec_idx] = a_vec_idx;
                            copy[i].b[res_vec_idx] = b_vec_idx;
                            copy[i].mul[res_vec_idx] = m_val;

                            op_array[res_vec_num] = &copy;
                            break;
                        }
                    } else {
                        var a_op: [4]usize = .{ 0, 0, 0, 0 };
                        var b_op: [4]usize = .{ 0, 0, 0, 0 };
                        var m_op: [4]f32 = .{ 0, 0, 0, 0 };

                        a_op[res_vec_idx] = a_vec_idx;
                        b_op[res_vec_idx] = b_vec_idx;
                        m_op[res_vec_idx] = m_val;

                        //var prev = op_array[res_vec_num].*;
                        op_array[res_vec_num] = op_array[res_vec_num] ++ .{GlslOp{
                            .a = a_op,
                            .b = b_op,
                            .mul = m_op,
                            .nums = .{ a_vec_num, b_vec_num },
                        }};
                    }
                }
            }
        }

        var generation: []const u8 = std.fmt.comptimePrint("\n    {[name]s} res = {[name]s}(", .{ .name = struct_name });
        for (0..res_len) |i| {
            if (i == res_len - 1) {
                generation = generation ++ "vec4(0)";
            } else {
                generation = generation ++ "vec4(0), ";
            }
        }
        generation = generation ++ ");\n\n";
        for (op_array, 0..) |arr, res_i| {
            for (arr) |val| {
                var mul: []const u8 = "vec4(";
                for (val.mul, 0..) |v, i| {
                    if (i < 3) {
                        mul = mul ++ (std.fmt.comptimePrint("{d}, ", .{v}));
                    } else {
                        mul = mul ++ (std.fmt.comptimePrint("{d}", .{v}));
                    }
                }
                mul = mul ++ ")";

                var a_str: []const u8 = std.fmt.comptimePrint("a.val{}.", .{val.nums[0]});
                for (val.a) |v| {
                    a_str = a_str ++ try getCoord(v);
                }

                var b_str: []const u8 = std.fmt.comptimePrint("b.val{}.", .{val.nums[1]});
                for (val.b) |v| {
                    b_str = b_str ++ try getCoord(v);
                }

                const line = std.fmt.comptimePrint("    res.val{} += {s} * {s} * {s};", .{ res_i, a_str, b_str, mul });
                generation = generation ++ line ++ "\n";
            }
        }

        generation = generation ++ "\n    return res;\n";

        const fmt_string =
            \\struct {[name]s} {{
            \\{[struct_def]s}}};
            \\
            \\{[name]s} mul({[name]s} a, {[name]s} b) {{{[generation]s}}}
        ;

        var struct_def: []const u8 = "";

        for (0..res_len) |i| struct_def = struct_def ++ std.fmt.comptimePrint("    vec4 val{};\n", .{i});

        break :blk std.fmt.comptimePrint(fmt_string, .{ .struct_def = struct_def, .generation = generation, .name = struct_name });
    };
}

fn getCoord(n: anytype) ![]const u8 {
    return switch (n) {
        0 => "x",
        1 => "y",
        2 => "z",
        3 => "w",
        else => error.InvalidCoord,
    };
}
