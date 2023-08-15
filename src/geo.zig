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

pub fn getMultivectorEnum(comptime dim: usize) type {
    const basis_num = (2 << (dim - 1)) - 1;

    const Data = struct {
        tags: [dim]usize,
        count: usize,
    };

    var index: usize = 0;
    var indices: [basis_num]Data = undefined;

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

    var fields: [basis_num]std.builtin.Type.EnumField = undefined;
    for (indices, 0..) |basis, i| {
        var buff: [dim * basis_num]u8 = undefined;
        var fba = std.heap.FixedBufferAllocator.init(&buff);
        var alloc = fba.allocator();
        var name = std.ArrayList(u8).init(alloc);
        var writer = name.writer();
        _ = writer.write("e") catch unreachable;
        for (basis.tags[0..basis.count]) |val| {
            _ = writer.print("{}", .{val}) catch unreachable;
        }
        fields[i] = .{ .name = name.items, .value = i };
    }

    return @Type(.{
        .Enum = .{
            .tag_type = @Type(.{
                .Int = .{ .signedness = .unsigned, .bits = basis_num },
            }),
            .fields = &fields,
            .decls = &.{},
            .is_exhaustive = false,
        },
    });
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
pub fn Algebra(comptime T: type, comptime dim: usize) type {
    const basis_num = (2 << (dim - 1)) - 1;

    const Data = struct {
        tags: [dim]usize,
        count: usize,
    };

    const indices: [basis_num + 1]Data = blk: {
        var temp: [basis_num + 1]Data = undefined;
        for (getBasis([basis_num]Data, dim), 0..) |basis, i| {
            temp[i] = basis;
        }
        temp[basis_num] = .{ .tags = .{}, .count = 0 };
        break :blk temp;
    };

    return struct {
        val: [basis_num + 1]T = .{0} ** (basis_num + 1),
        const Self = @This();
        pub const Indices = indices;

        pub const Inputs = blk: {
            var fields: [basis_num]std.builtin.Type.StructField = undefined;
            for (indices[0..basis_num], 0..) |basis, i| {
                var buff: [dim * basis_num + 1024]u8 = undefined;
                var fba = std.heap.FixedBufferAllocator.init(&buff);
                var alloc = fba.allocator();
                var name = std.ArrayList(u8).init(alloc);
                var writer = name.writer();

                _ = writer.write("e") catch unreachable;

                for (basis.tags[0..basis.count]) |val| {
                    _ = writer.print("{}", .{val}) catch unreachable;
                }
                var res = Self{};
                res.val[i] = 1;
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
            pub const BinOp = enum { @"+", @"-", @"*" };

            pub const allow_unused_inputs = true;

            pub const relations = .{
                .@"+" = .{ .prec = 10, .assoc = .left },
                .@"-" = .{ .prec = 10, .assoc = .left },
                .@"*" = .{ .prec = 20, .assoc = .left },
            };

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
                };
            }
        }{});

        pub fn evalBasis(comptime input: []const u8) !Self {
            return try comath.eval(input, geoCtx, Inputs);
        }

        pub fn fromInt(num: anytype) Self {
            var res = Self{};
            res.val[basis_num] = num;
            return res;
        }

        inline fn multiplyBasisWithSingle(i: usize, b_dim: usize) struct { usize, bool } {
            // if it's a constant
            if (i == basis_num) return .{ b_dim - 1, false };

            const a = indices[i];

            var result: Data = .{ .tags = .{0} ** dim, .count = 0 };

            var swap: bool = false;

            const a_len = a.count;

            for (1..a_len + 1) |a_i| {
                const pos = a_len - a_i;
                const reversed = a.tags[pos];
                if (reversed != b_dim) {
                    var index: usize = 0;

                    if (reversed > b_dim) {
                        if (a.count != dim) {
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
                    return .{ idx, swap };
                }
            }

            std.debug.print("\n{any}\n", .{result.tags[0..result.count]});
            @panic("FAIL!");
        }

        pub fn multiplyBasis(i: usize, j: usize) struct { usize, bool } {
            const b = indices[j];
            var r = i;
            var sign = false;
            for (b.tags[0..b.count]) |tag| {
                const res = multiplyBasisWithSingle(r, tag);
                //std.debug.print("\nbasis: {any}*e{} = {s}{any}\n", .{ indices[r].tags[0..indices[r].count], tag, if (res[1]) "-" else "", indices[res[0]].tags[0..indices[res[0]].count] });
                r = res[0];
                sign = res[1] != sign;
                //std.debug.print("sign now {}\n", .{sign});
            }
            return .{ r, sign };
        }

        pub fn mul(a: Self, b: Self) Self {
            var r: Self = undefined;
            var vec: @Vector(basis_num + 1, T) = .{0} ** (basis_num + 1);
            for (a.val, 0..) |a_us, a_i| {
                if (a_us == 0) continue;
                for (b.val, 0..) |b_us, b_i| {
                    if (b_us == 0) continue;

                    //std.debug.print("\nmul: {any}*{any} = ?\n", .{ indices[a_i].tags[0..indices[b_i].count], indices[b_i].tags[0..indices[b_i].count] });

                    const res = multiplyBasis(a_i, b_i);

                    //std.debug.print("\nmul: {any}*{any} = {s}{any}\n", .{ indices[a_i].tags[0..indices[a_i].count], indices[b_i].tags[0..indices[b_i].count], if (res[1]) "-" else "", indices[res[0]].tags[0..indices[res[0]].count] });

                    const sign: T = if (res[1]) -1 else 1;
                    const a_scalar: T = @intCast(a_us);
                    const b_scalar: T = @intCast(b_us);

                    vec[res[0]] += a_scalar * b_scalar * sign;
                }
            }
            r.val = vec;

            return r;
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
    const Alg = Algebra(i32, 2);

    var res = Alg.mul(.{ .val = .{ 2, 0, 3, 0 } }, .{ .val = .{ 0, 2, 0, 0 } }); // (3e12+2e1) * (2e2)
    try std.testing.expectEqualSlices(i32, &.{ 6, 0, 4, 0 }, &res.val); // 6e1+4e12

    res = Alg.mul(.{ .val = .{ 2, 0, 3, 0 } }, .{ .val = .{ 0, 2, 0, 1 } }); // (3e12+2e1) * (2e2+1)
    try std.testing.expectEqualSlices(i32, &.{ 8, 0, 7, 0 }, &res.val); // 8e1+7e12

    res = Alg.mul(.{ .val = .{ 2, 0, 3, 0 } }, .{ .val = .{ 1, 0, 0, 0 } }); // (3e12+2e1) * (e1)
    try std.testing.expectEqualSlices(i32, &.{ 0, -3, 0, 2 }, &res.val); // -3e2+2

    res = Alg.mul(.{ .val = .{ 2, 0, 3, 0 } }, .{ .val = .{ 1, 2, 0, 0 } }); // (3e12+2e1) * (e1+2e2)
    try std.testing.expectEqualSlices(i32, &.{ 6, -3, 4, 2 }, &res.val); // -3e2+2
    res = Alg.mul(.{ .val = .{ 0, 0, 3, 0 } }, .{ .val = .{ 0, 0, 5, 0 } }); // (3e12) * (5e12)
    try std.testing.expectEqualSlices(i32, &.{ 0, 0, 0, -15 }, &res.val); // -3e2+2

    res = Alg.mul(.{ .val = .{ 2, 0, 0, 0 } }, .{ .val = .{ 0, 0, 5, 0 } }); // (2e1) * (5e12)
    try std.testing.expectEqualSlices(i32, &.{ 0, 10, 0, 0 }, &res.val); // 10e2

    res = Alg.mul(.{ .val = .{ 2, 3, 5, 7 } }, .{ .val = .{ 11, 13, 17, 19 } }); // (2*e1+3*e2+5*e12+7) * (11*e1+13*e2+17*e12+19)
    try std.testing.expectEqualSlices(i32, &.{ 129, 127, 207, 109 }, &res.val); // (129e1) + (127e2) + (207e12) + 109

    res = Alg.mul(.{ .val = .{ 2, 3, 5, 0 } }, .{ .val = .{ 11, 13, 17, 0 } }); // (2*e1+3*e2+5*e12) * (11*e1+13*e2+17*e12)
    try std.testing.expectEqualSlices(i32, &.{ 14, -21, -7, -24 }, &res.val); // (14e1) - (21e2) - (7e12) - 24

    res = Alg.add(.{ .val = .{ 4, 3, 2, 1 } }, .{ .val = .{ 0, 1, 2, 3 } });
    try std.testing.expectEqualSlices(i32, &.{ 4, 4, 4, 4 }, &res.val);

    try std.testing.expectEqualSlices(
        i32,
        &(try Alg.evalBasis("129*e1 + 127*e2 + 207*e12 + 109")).val,
        &(try Alg.evalBasis("(2*e1+3*e2+5*e12+7) * (11*e1+13*e2+17*e12+19)")).val,
    );
    try std.testing.expectEqualSlices(
        i32,
        &Alg.fromInt(-1).val,
        &(try Alg.evalBasis("e12 * e12")).val,
    );
}
