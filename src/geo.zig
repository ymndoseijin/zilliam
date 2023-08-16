const std = @import("std");
const comath = @import("comath");
const contexts = comath.contexts;
const simpleCtx = contexts.simpleCtx;

fn basisRecursion(comptime dim: usize, start: usize, num: usize, length: usize, decls: anytype, index: *usize, tags: [dim]usize) void {
    if (num >= length - 1) {
        decls[index.*].count = length;
        decls[index.*].tags = tags;

        index.* += 1;
        return;
    }

    inline for (start + 1..dim + 1) |i| {
        var nums: [dim]usize = .{0} ** dim;
        for (tags, &nums) |val, *ptr| {
            ptr.* = val;
        }

        nums[num + 1] = i;

        basisRecursion(dim, i, num + 1, length, decls, index, nums);
    }
}

pub fn getBasis(comptime R: type, comptime dim: usize) R {
    var index: usize = 0;
    var indices: R = undefined;
    for (1..dim + 1) |length| {
        for (1..dim + 1) |i| {
            var buff: [dim]usize = .{0} ** dim;
            buff[0] = i;
            basisRecursion(
                dim,
                i,
                0,
                length,
                &indices,
                &index,
                buff,
            );
        }
    }
    return indices;
}
pub fn Algebra(comptime T: type, comptime pos_dim: usize, comptime neg_dim: usize, comptime null_dim: usize) type {
    const sum_of_dim = null_dim + pos_dim + neg_dim;
    const basis_num = (2 << (sum_of_dim - 1)) - 1;

    const Data = struct {
        // .{scalar, z_1, ..., z_{null_dim}, p_1, ..., p_{pos_dim}, n_1, ..., n_{neg_dim}}
        tags: [sum_of_dim]usize,
        count: usize,
    };

    const indices: [basis_num + 1]Data = blk: {
        var temp: [basis_num + 1]Data = undefined;
        temp[0] = .{ .tags = .{}, .count = 0 };
        for (getBasis([basis_num]Data, sum_of_dim), 0..) |basis, i| {
            temp[i + 1] = basis;
        }
        break :blk temp;
    };

    return struct {
        val: [basis_num + 1]T = .{0} ** (basis_num + 1),
        const Self = @This();
        pub const Indices = indices;

        pub const Inputs = blk: {
            var fields: [basis_num]std.builtin.Type.StructField = undefined;
            for (indices[1 .. basis_num + 1], 0..) |basis, i| {
                var buff: [1024]u8 = undefined;
                var fba = std.heap.FixedBufferAllocator.init(&buff);
                var alloc = fba.allocator();
                var name = std.ArrayList(u8).init(alloc);
                var writer = name.writer();

                _ = writer.write("e") catch unreachable;

                for (basis.tags[0..basis.count]) |val| {
                    var actual: i32 = @intCast(val);
                    actual -= null_dim;
                    _ = writer.print("{}", .{actual}) catch unreachable;
                }
                var res = Self{};
                res.val[i + 1] = 1;

                fields[i] = .{
                    .name = name.items,
                    .type = Self,
                    .default_value = @ptrCast(&res),
                    .is_comptime = true,
                    .alignment = 1,
                };
            }

            break :blk @Type(.{
                .Struct = .{
                    .layout = .Auto,
                    .fields = &fields,
                    .decls = &.{},
                    .is_tuple = false,
                },
            }){};
        };

        pub const geoCtx = simpleCtx(struct {
            pub const UnOp = enum { @"-" };
            pub const BinOp = enum { @"+", @"-", @"*", @"^" };

            pub const allow_unused_inputs = true;

            pub const relations = .{
                .@"+" = .{ .prec = 10, .assoc = .left },
                .@"-" = .{ .prec = 10, .assoc = .left },
                .@"*" = .{ .prec = 20, .assoc = .left },
                .@"^" = .{ .prec = 20, .assoc = .left },
            };

            pub fn EvalUnOp(comptime op: []const u8, comptime U: type) type {
                _ = op;
                _ = U;
                return Self;
            }

            pub fn evalUnOp(_: @This(), comptime op: []const u8, in_val: anytype) EvalUnOp(op, Self) {
                const val = if (@TypeOf(in_val) != Self) Self.fromInt(in_val) else in_val;

                return switch (@field(UnOp, op)) {
                    .@"-" => Self.fromInt(-1).mul(val),
                };
            }

            pub fn EvalBinOp(comptime Lhs: type, comptime op: []const u8, comptime Rhs: type) type {
                _ = Lhs;
                _ = op;
                _ = Rhs;
                return Self;
            }

            pub fn evalBinOp(_: @This(), in_lhs: anytype, comptime op: []const u8, in_rhs: anytype) EvalBinOp(Self, op, Self) {
                const lhs = if (@TypeOf(in_lhs) != Self) Self.fromInt(in_lhs) else in_lhs;
                const rhs = if (@TypeOf(in_rhs) != Self) Self.fromInt(in_rhs) else in_rhs;

                return switch (@field(BinOp, op)) {
                    .@"+" => lhs.add(rhs),
                    .@"-" => lhs.sub(rhs),
                    .@"*" => lhs.mul(rhs),
                    .@"^" => lhs.wedge(rhs),
                };
            }
        }{});

        pub fn evalBasis(comptime input: []const u8) !Self {
            return try comath.eval(input, geoCtx, Inputs);
        }

        pub fn fromInt(num: anytype) Self {
            var res = Self{};
            res.val[0] = num;
            return res;
        }

        const Sign = enum {
            pos,
            zero,
            neg,
        };

        inline fn multiplyBasisWithSingle(comptime quadratic_form: Sign, i: usize, b_dim: usize) struct { usize, Sign } {
            // if it's a constant
            if (i == 0) return .{ b_dim, .pos };

            const a = indices[i];

            var result: Data = .{ .tags = .{0} ** sum_of_dim, .count = 0 };

            var swap: bool = false;

            const a_len = a.count;

            for (1..a_len + 1) |a_i| {
                const pos = a_len - a_i;
                const reversed = a.tags[pos];
                if (reversed != b_dim) {
                    var index: usize = 0;

                    if (reversed > b_dim) {
                        if (a.count != sum_of_dim) {
                            for (0..pos) |idx| {
                                result.tags[index] = a.tags[idx];
                                index += 1;
                            }

                            result.tags[index] = b_dim;
                            index += 1;

                            for (pos..a_len) |idx| {
                                result.tags[index] = a.tags[idx];
                                index += 1;
                            }
                            result.count = index;
                        }
                        swap = !swap;
                    } else {
                        for (0..pos + 1) |idx| {
                            result.tags[index] = a.tags[idx];
                            index += 1;
                        }

                        result.tags[index] = b_dim;
                        index += 1;

                        for (pos + 1..a_len) |idx| {
                            result.tags[index] = a.tags[idx];
                            index += 1;
                        }
                        result.count = index;
                        break;
                    }
                } else {
                    var index: usize = 0;
                    for (0..pos) |idx| {
                        result.tags[index] = a.tags[idx];
                        index += 1;
                    }

                    if (null_dim >= b_dim or quadratic_form == .zero) {
                        return .{ b_dim, .zero };
                    } else if (b_dim > null_dim + pos_dim or quadratic_form == .neg) {
                        swap = !swap;
                    }

                    for (pos + 1..a_len) |idx| {
                        result.tags[index] = a.tags[idx];
                        index += 1;
                    }
                    result.count = index;
                    break;
                }
            }

            inline for (indices, 0..) |val, idx| {
                if (std.mem.eql(usize, val.tags[0..val.count], result.tags[0..result.count])) {
                    return .{ idx, if (swap) .neg else .pos };
                }
            }

            std.debug.print("\n{any}\n", .{result.tags[0..result.count]});
            @panic("FAIL!");
        }

        pub fn multiplyBasis(comptime quadratic_form: Sign, i: usize, j: usize) struct { usize, Sign } {
            const b = indices[j];
            var r = i;
            var sign: Sign = .pos;
            for (b.tags[0..b.count]) |tag| {
                const res = multiplyBasisWithSingle(quadratic_form, r, tag);

                if (res[1] == .zero) return .{ 0, .zero };
                sign = if (res[1] == sign) .pos else .neg;

                r = res[0];
            }
            return .{ r, sign };
        }

        pub fn anticommute(a: Self, comptime quadratic_form: Sign, b: Self) Self {
            var r: Self = undefined;
            var vec: @Vector(basis_num + 1, T) = .{0} ** (basis_num + 1);
            for (a.val, 0..) |a_us, a_i| {
                if (a_us == 0) continue;
                for (b.val, 0..) |b_us, b_i| {
                    if (b_us == 0) continue;

                    const res = multiplyBasis(quadratic_form, a_i, b_i);

                    const sign: T = switch (res[1]) {
                        .pos => 1,
                        .neg => -1,
                        .zero => 0,
                    };
                    const a_scalar: T = @intCast(a_us);
                    const b_scalar: T = @intCast(b_us);

                    vec[res[0]] += a_scalar * b_scalar * sign;
                }
            }
            r.val = vec;

            return r;
        }

        pub fn mul(a: Self, b: Self) Self {
            return a.anticommute(.pos, b);
        }

        pub fn wedge(a: Self, b: Self) Self {
            return a.anticommute(.zero, b);
        }

        pub fn print(a: Self, buff: []u8) ![]const u8 {
            var fba = std.heap.FixedBufferAllocator.init(buff);
            var alloc = fba.allocator();
            var name = std.ArrayList(u8).init(alloc);
            var writer = name.writer();

            for (a.val, 0..) |val, index| {
                if (val == 0) continue;
                if (name.items.len != 0) {
                    _ = try writer.print(" + ", .{});
                }
                if (index == 0) {
                    _ = try writer.print("{}", .{val});
                } else {
                    const basis = indices[index];
                    _ = try writer.print("{}e", .{val});
                    for (basis.tags[0..basis.count]) |basis_idx| {
                        var actual: i32 = @intCast(basis_idx);
                        actual -= @intCast(null_dim);
                        _ = try writer.print("{}", .{actual});
                    }
                }
            }

            return name.items;
        }

        pub fn hodge(a: Self) Self {
            var r: Self = undefined;

            for (a.val, 0..) |scalar, i| {
                const res = multiplyBasis(.pos, i, basis_num - i);

                const sign: T = switch (res[1]) {
                    .pos => 1,
                    .neg => -1,
                    .zero => 0,
                };

                r.val[basis_num - i] = sign * scalar;
            }
            return r;
        }

        // ?
        pub fn rotor(plane: Self, theta: T) Self {
            return Self.fromInt(std.math.cos(theta)).add(plane.mul(std.math.sin(theta)));
        }

        pub fn add(a: Self, b: Self) Self {
            var r: Self = undefined;

            var a_vec: @Vector(basis_num + 1, T) = a.val;
            var b_vec: @Vector(basis_num + 1, T) = b.val;
            r.val = a_vec + b_vec;

            return r;
        }

        pub fn sub(a: Self, b: Self) Self {
            var r: Self = undefined;
            var a_vec: @Vector(basis_num + 1, T) = a.val;
            var b_vec: @Vector(basis_num + 1, T) = b.val;

            r.val = a_vec - b_vec;

            return r;
        }
    };
}

test "algebra" {
    const Alg = Algebra(i32, 2, 1, 1);

    try std.testing.expectEqualSlices(
        i32,
        &(try Alg.evalBasis("(3*e12+2*e1)*(2*e2)")).val,
        &(try Alg.evalBasis("6*e1+4*e12")).val,
    );

    try std.testing.expectEqualSlices(
        i32,
        &(try Alg.evalBasis("(3*e12+2*e1)*(2*e2+1)")).val,
        &(try Alg.evalBasis("8*e1+7*e12")).val,
    );

    try std.testing.expectEqualSlices(
        i32,
        &(try Alg.evalBasis("(3*e12+2*e1)*e1")).val,
        &(try Alg.evalBasis("-3*e2+2")).val,
    );

    // 6*e1-3*e2+4*e12+2 = 6*e1-3*e2-4*e12-2???
    // temp: "6*e1-(3*e2)+4*e12+2 seems to fix it (it's a comath bug)

    try std.testing.expectEqualSlices(
        i32,
        &(try Alg.evalBasis("(2*e1+3*e12)*(e1+2*e2)")).val,
        &(try Alg.evalBasis("6*e1-(3*e2)+4*e12+2")).val,
    );

    try std.testing.expectEqualSlices(
        i32,
        &Alg.fromInt(-15).val,
        &(try Alg.evalBasis("(3*e12)*(5*e12)")).val,
    );

    try std.testing.expectEqualSlices(
        i32,
        &(try Alg.evalBasis("(2*e1)*(5*e12)")).val,
        &(try Alg.evalBasis("10*e2")).val,
    );

    try std.testing.expectEqualSlices(
        i32,
        &(try Alg.evalBasis("(2*e1)*(5*e12)")).val,
        &(try Alg.evalBasis("10*e2")).val,
    );

    try std.testing.expectEqualSlices(
        i32,
        &(try Alg.evalBasis("(14*e1) - (21*e2) - (7*e12) - 24")).val,
        &(try Alg.evalBasis("(2*e1+3*e2+5*e12) * (11*e1+13*e2+17*e12)")).val,
    );

    try std.testing.expectEqualSlices(
        i32,
        &(try Alg.evalBasis("129*e1 + 127*e2 + 207*e12 + 109")).val,
        &(try Alg.evalBasis("(2*e1+3*e2+5*e12+7) * (11*e1+13*e2+17*e12+19)")).val,
    );

    // wedge
    try std.testing.expectEqualSlices(
        i32,
        &(try Alg.evalBasis("133 + (115*e1) + (148*e2) + (207*e12)")).val,
        &(try Alg.evalBasis("(2*e1+3*e2+5*e12+7) ^ (11*e1+13*e2+17*e12+19)")).val,
    );

    try std.testing.expectEqualSlices(
        i32,
        &Alg.fromInt(0).val,
        &(try Alg.evalBasis("(2*e1)^(5*e12)")).val,
    );

    // null
    try std.testing.expectEqualSlices(
        i32,
        &Alg.fromInt(0).val,
        &(try Alg.evalBasis("e0*e0")).val,
    );

    try std.testing.expectEqualSlices(
        i32,
        &Alg.fromInt(0).val,
        &(try Alg.evalBasis("e0^e0")).val,
    );

    try std.testing.expectEqualSlices(
        i32,
        &(try Alg.evalBasis("654*e0 + 129*e1 + 127*e2 + 191*e01 + 206*e02 + 207*e12 + 546*e012 + 109")).val,
        &(try Alg.evalBasis("(23*e0+2*e1+3*e2+5*e12+7) * (31*e0+11*e1+13*e2+17*e12+19)")).val,
    );

    // negative
    try std.testing.expectEqualSlices(
        i32,
        &Alg.fromInt(-1).val,
        &(try Alg.evalBasis("e3*e3")).val,
    );

    try std.testing.expectEqualSlices(
        i32,
        &Alg.fromInt(0).val,
        &(try Alg.evalBasis("e3^e3")).val,
    );

    // hodge
    try std.testing.expectEqualSlices(
        i32,
        &(try Alg.evalBasis("5*e03 + 3*e013 + -2*e023 + 11*e123 + 7*e0123")).val,
        &(try Alg.evalBasis("11*e0+2*e1+3*e2+5*e12+7")).hodge().val,
    );

    var buff: [1024]u8 = undefined;
    std.debug.print("\n{s}\n", .{try (try Alg.evalBasis("e3*e3")).print(&buff)});
}
