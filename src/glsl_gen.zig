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

pub fn generateFunction(comptime vals: anytype, comptime options: struct {
    res: type,
    a: type,
    b: type,
    result_name: []const u8,
    a_name: []const u8,
    b_name: []const u8,
    fun_name: []const u8,
}) []const u8 {
    return comptime blk: {
        const TypeA = options.a;
        const TypeB = options.b;

        const op_a = vals[0];
        const op_b = vals[1];
        const op_m = vals[2];

        const nothings: @Vector(options.res.Count, i32) = @as([options.res.Count]i32, .{-1} ** options.res.Count);
        const neverweres: @Vector(options.res.Count, f32) = @as([options.res.Count]f32, .{0} ** options.res.Count);

        const a_len = (options.a.Count + 4 - 1) / 4;
        const b_len = (options.b.Count + 4 - 1) / 4;
        const res_len = (options.res.Count + 4 - 1) / 4;

        var op_array: [res_len][]const GlslOp = .{&.{}} ** res_len;

        const remainder = @mod(options.res.Count, 4);

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

                        var array: [op_array[res_vec_num].len + 1]GlslOp = undefined;
                        @memcpy(array[0 .. array.len - 1], op_array[res_vec_num]);
                        array[op_array[res_vec_num].len] = .{
                            .a = a_op,
                            .b = b_op,
                            .mul = m_op,
                            .nums = .{ a_vec_num, b_vec_num },
                        };

                        op_array[res_vec_num] = &array;
                    }
                }
            }
        }

        var generation: []const u8 = std.fmt.comptimePrint("\n    {[name]s} res = {[name]s}(", .{ .name = options.result_name });
        for (0..res_len) |i| {
            if (i == res_len - 1) {
                switch (remainder) {
                    0 => generation = generation ++ "vec4(0)",
                    1 => generation = generation ++ "0",
                    2 => generation = generation ++ "vec2(0)",
                    3 => generation = generation ++ "vec3(0)",
                    else => unreachable,
                }
            } else {
                generation = generation ++ "vec4(0)";
                generation = generation ++ ", ";
            }
        }
        generation = generation ++ ");\n\n";
        for (op_array, 0..) |arr, res_i| {
            for (arr) |val| {
                const size = if (res_i != res_len - 1 or remainder == 0) 4 else remainder;

                var a_str: []const u8 = std.fmt.comptimePrint("a.val{}", .{val.nums[0]});
                const a_size = if (val.nums[0] != a_len or @mod(TypeA.Count, 4) == 0) 4 else @mod(TypeA.Count, 4);

                if (a_size != 1) {
                    a_str = a_str ++ ".";
                    for (val.a[0..size]) |v| {
                        a_str = a_str ++ try getCoord(v);
                    }
                }

                var b_str: []const u8 = std.fmt.comptimePrint("b.val{}", .{val.nums[1]});
                const b_size = if (val.nums[0] != b_len or @mod(TypeB.Count, 4) == 0) 4 else @mod(TypeB.Count, 4);

                if (b_size != 1) {
                    b_str = b_str ++ ".";
                    for (val.b[0..size]) |v| {
                        b_str = b_str ++ try getCoord(v);
                    }
                }

                var mul: []const u8 = "";
                if (size != 1) {
                    mul = std.fmt.comptimePrint("vec{d}(", .{size});
                    for (val.mul[0..size], 0..) |v, i| {
                        if (i < size - 1) {
                            mul = mul ++ (std.fmt.comptimePrint("{d}, ", .{v}));
                        } else {
                            mul = mul ++ (std.fmt.comptimePrint("{d}", .{v}));
                        }
                    }
                    mul = mul ++ ")";
                } else {
                    mul = std.fmt.comptimePrint("{d}", .{val.mul[0]});
                }

                const line = std.fmt.comptimePrint("    res.val{} += {s} * {s} * {s};", .{ res_i, a_str, b_str, mul });
                generation = generation ++ line ++ "\n";
            }
        }

        generation = generation ++ "\n    return res;\n";

        const fmt_string =
            \\{[result_name]s} {[fun_name]s}({[a_name]s} a, {[b_name]s} b) {{{[generation]s}}}
        ;

        break :blk std.fmt.comptimePrint(fmt_string, .{
            .generation = generation,
            .result_name = options.result_name,
            .a_name = options.a_name,
            .b_name = options.b_name,
            .fun_name = options.fun_name,
        });
    };
}

pub fn generateStruct(comptime T: type, comptime name: []const u8) []const u8 {
    return comptime blk: {
        const fmt_string =
            \\struct {[name]s} {{
            \\{[struct_def]s}}};
        ;

        var struct_def: []const u8 = "";

        const res_len = (T.Count + 4 - 1) / 4;
        const remainder = @mod(T.Count, 4);

        for (0..res_len) |i| {
            const format = fmt: {
                if (i == res_len - 1) {
                    break :fmt switch (remainder) {
                        0 => "    vec4 val{};\n",
                        1 => "    float val{};\n",
                        2 => "    vec2 val{};\n",
                        3 => "    vec3 val{};\n",
                        else => unreachable,
                    };
                } else {
                    break :fmt "    vec4 val{};\n";
                }
            };
            struct_def = struct_def ++ std.fmt.comptimePrint(format, .{i});
        }

        break :blk std.fmt.comptimePrint(fmt_string, .{ .struct_def = struct_def, .name = name });
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

pub fn latinNum(d: anytype, n: anytype, i: anytype) []const u8 {
    switch (d) {
        1 => {
            if (n < 10) {
                return "uni";
            } else {
                switch (i) {
                    0 => return "un",
                    1 => return "decem",
                    2 => return "centi",
                    3 => return "milli",
                    4 => return "decamilli",
                    5 => return "centimilli",
                    else => return "",
                }
            }
        },
        2 => {
            if (n < 10) {
                return "bi";
            } else {
                switch (i) {
                    0 => return "duo",
                    1 => return "viginti",
                    2 => return "ducenti",
                    else => return "duo",
                }
            }
        },
        3 => {
            if (n < 10) {
                return "tri";
            } else {
                switch (i) {
                    0 => return "tre",
                    1 => return "triginti",
                    2 => return "trecenti",
                    else => return "tre",
                }
            }
        },
        4 => {
            if (n < 10) {
                return "quad";
            } else {
                switch (i) {
                    0 => return "quattuor",
                    1 => return "quadraginti",
                    2 => return "quadringenti",
                    else => return "quadri",
                }
            }
        },
        5 => {
            if (n < 10) {
                return "quinque";
            } else {
                switch (i) {
                    0 => return "quinqua",
                    1 => return "quinquaginti",
                    2 => return "quingent",
                    else => return "quin",
                }
            }
        },
        6 => {
            if (n < 10) {
                return "sexa";
            } else {
                switch (i) {
                    0 => return "sex",
                    1 => return "sexaginti",
                    2 => return "sexcenti",
                    else => return "sex",
                }
            }
        },
        7 => {
            if (n < 10) {
                return "sept";
            } else {
                switch (i) {
                    0 => return "septen",
                    1 => return "septuaginti",
                    2 => return "septingenti",
                    else => return "septin",
                }
            }
        },
        8 => {
            if (n < 10) {
                return "octo";
            } else {
                switch (i) {
                    0 => return "octo",
                    1 => return "octoginti",
                    2 => return "octingenti",
                    else => return "octo",
                }
            }
        },
        9 => {
            if (n < 10) {
                return "novem";
            } else {
                switch (i) {
                    0 => return "novem",
                    1 => return "nonagint",
                    2 => return "nongenti",
                    else => return "non",
                }
            }
        },
        0 => return "",
        else => unreachable,
    }
}

pub fn latinPrefix(comptime T: type, n: T) ![]const u8 {
    const digits = std.math.log(T, 10, n);

    var buf: [1024]u16 = undefined;
    const digits_buf = buf[0 .. digits + 1];

    var loop_n = n;
    for (digits_buf) |*d| {
        d.* = @intCast(@mod(loop_n, 10));
        loop_n /= 10;
    }

    var string: [4096]u8 = undefined;
    var index: usize = 0;

    for (digits_buf, 0..) |d, i| {
        const latin_string = latinNum(d, n, i);
        @memcpy(string[index .. index + latin_string.len], latin_string);
        index += latin_string.len;

        if (i > 2 and d != 1) {
            const suffix_string = latinNum(1, n, i);
            @memcpy(string[index .. index + suffix_string.len], suffix_string);
            index += suffix_string.len;
        }
    }

    return string[0..index];
}

test "wario" {
    const Algebra = geo.Algebra(f32, 3, 0, 1);
    const Blades = blades.Blades(Algebra, .{});

    const Type = Blades.Blades[1];

    const full_glsl = comptime generation: {
        var output: []const u8 = "";

        const Generation = struct { usize, []const u8 };
        var generated_idx: []const Generation = &.{};

        for (Blades.Blades, 0..) |T, i| {
            const struct_name = blk: {
                if (i == 0) break :blk "Scalar";
                if (i == 1) break :blk "Vector";
                var string = try latinPrefix(u64, i);
                break :blk .{std.ascii.toUpper(string[0])} ++ string[1..] ++ "vector";
            };

            generated_idx = generated_idx ++ .{.{ T.Idx, struct_name }};
            output = output ++ generateStruct(T, struct_name) ++ "\n\n";
        }

        for (Blades.Blades[1..], 0..) |TypeA, i_a| {
            for (Blades.Blades[1..], 0..) |TypeB, i_b| {
                const ops = Type.anticommuteOps(.pos, void{}, TypeA, TypeB);
                const vals = ops.Res;
                const ResType = ops.ResType;

                const a_struct = blk: {
                    if (i_a == 0) break :blk "Vector";
                    var string = try latinPrefix(u64, i_a + 1);
                    break :blk .{std.ascii.toUpper(string[0])} ++ string[1..] ++ "vector";
                };

                const b_struct = blk: {
                    if (i_b == 0) break :blk "Vector";
                    var string = try latinPrefix(u64, i_b + 1);
                    break :blk .{std.ascii.toUpper(string[0])} ++ string[1..] ++ "vector";
                };

                var res_exists: ?Generation = blk: {
                    for (generated_idx) |idx| {
                        if (idx[0] == ResType.Idx) break :blk idx;
                    }
                    break :blk null;
                };

                if (res_exists == null) {
                    const name = "Res" ++ a_struct ++ b_struct;
                    generated_idx = generated_idx ++ .{.{ ResType.Idx, name }};
                    output = output ++ generateStruct(ResType, name) ++ "\n\n";
                    res_exists = .{ ResType.Idx, name };
                }

                const generated_glsl = generateFunction(vals, .{
                    .res = ResType,
                    .a = TypeA,
                    .b = TypeB,
                    .result_name = res_exists.?[1],
                    .a_name = a_struct,
                    .b_name = b_struct,
                    .fun_name = "mul",
                });

                output = output ++ generated_glsl ++ "\n\n";
            }
        }

        for (Blades.Blades[1..], 0..) |TypeA, i_a| {
            for (Blades.Blades[1..], 0..) |TypeB, i_b| {
                const ops = Type.anticommuteOps(.zero, void{}, TypeA, TypeB);
                const vals = ops.Res;
                const ResType = ops.ResType;

                const a_struct = blk: {
                    if (i_a == 0) break :blk "Vector";
                    var string = try latinPrefix(u64, i_a + 1);
                    break :blk .{std.ascii.toUpper(string[0])} ++ string[1..] ++ "vector";
                };

                const b_struct = blk: {
                    if (i_b == 0) break :blk "Vector";
                    var string = try latinPrefix(u64, i_b + 1);
                    break :blk .{std.ascii.toUpper(string[0])} ++ string[1..] ++ "vector";
                };

                var res_exists: ?Generation = blk: {
                    for (generated_idx) |idx| {
                        if (idx[0] == ResType.Idx) break :blk idx;
                    }
                    break :blk null;
                };

                if (res_exists == null) {
                    const name = "Res" ++ a_struct ++ b_struct;
                    generated_idx = generated_idx ++ .{.{ ResType.Idx, name }};
                    output = output ++ generateStruct(ResType, name) ++ "\n\n";
                    res_exists = .{ ResType.Idx, name };
                }

                const generated_glsl = generateFunction(vals, .{
                    .res = ResType,
                    .a = TypeA,
                    .b = TypeB,
                    .result_name = res_exists.?[1],
                    .a_name = a_struct,
                    .b_name = b_struct,
                    .fun_name = "wedge",
                });

                output = output ++ generated_glsl ++ "\n\n";
            }
        }

        break :generation output;
    };
    std.debug.print("{s}\n", .{full_glsl});
}
