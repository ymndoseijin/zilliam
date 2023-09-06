const std = @import("std");
const comath = @import("comath");
const operations = @import("operations.zig");
const contexts = comath.contexts;
const simpleCtx = contexts.simpleCtx;

pub const PGA = @import("pga.zig").PGA;
pub const blades = @import("blades.zig");

pub const Sign = enum {
    pos,
    zero,
    neg,
};

pub const AlgebraEnum = enum {
    FullAlgebra,
    SubAlgebra,
    BatchAlgebra,
};

fn factorial(i: anytype) @TypeOf(i) {
    if (i == 0) return 1;
    return i * factorial(i - 1);
}

fn repeatType(comptime T: type) type {
    switch (T.AlgebraType) {
        .FullAlgebra => return T,
        .SubAlgebra => return T.anticommuteResult(.pos, void{}, T, T),
        .BatchAlgebra => return T.getBatchTypeGen(T.Algebra, T.Type.anticommuteResult(.pos, void{}, T.Type, T.Type), T.LenMul),
    }
}

fn comptimePower(a: anytype, comptime i: usize) repeatType(@TypeOf(a)) {
    if (i == 0) {
        var res = repeatType(@TypeOf(a)){};
        res.val[0] = 1;
        return res;
    }
    var a_c = a.toIndex(repeatType(@TypeOf(a)));
    var loop = a_c;
    inline for (0..i - 1) |_| {
        loop = loop.mul(a_c);
    }
    return loop;
}

pub fn outerCos(a: anytype, comptime precision: usize) repeatType(@TypeOf(a)) {
    const Result = repeatType(@TypeOf(a));
    var res = Result{};

    inline for (0..precision) |i| {
        const val: comptime_int = 2 * i;
        const fact: @Vector(Result.Count, Result.Algebra.Type) = @splat(@as(Result.Algebra.Type, comptime factorial(val)));
        const term = Result{ .val = comptimePower(a, val).val / fact };
        if (i % 2 == 0) {
            res = res.add(term);
        } else {
            res = res.sub(term);
        }
    }
    return res;
}

pub fn outerSin(a: anytype, comptime precision: usize) repeatType(@TypeOf(a)) {
    const Result = repeatType(@TypeOf(a));
    var res = Result{};

    inline for (0..precision) |i| {
        const val: comptime_int = 2 * i + 1;
        const fact: @Vector(Result.Count, Result.Algebra.Type) = @splat(@as(Result.Algebra.Type, comptime factorial(val)));
        const term = Result{ .val = comptimePower(a, val).val / fact };
        if (i % 2 == 0) {
            res = res.add(term);
        } else {
            res = res.sub(term);
        }
    }
    return res;
}

pub fn outerExp(a: anytype, comptime precision: usize) repeatType(@TypeOf(a)) {
    const Result = repeatType(@TypeOf(a));
    var res = Result{};

    inline for (0..precision) |i| {
        const val: comptime_int = i;
        const fact: @Vector(Result.Count, Result.Algebra.Type) = @splat(@as(Result.Algebra.Type, comptime factorial(val)));
        const term = Result{ .val = comptimePower(a, val).val / fact };
        res = res.add(term);
    }
    return res;
}

fn basisRecursion(comptime dim: usize, start: usize, num: usize, length: usize, decls: anytype, index: *usize, tags: [dim]usize) void {
    if (num >= length - 1) {
        decls[index.*].count = length;
        decls[index.*].tags = tags;

        index.* += 1;
        return;
    }

    for (start + 1..dim + 1) |i| {
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

        @setEvalBranchQuota(basis_num * basis_num);
        for (getBasis([basis_num]Data, sum_of_dim), 0..) |basis, i| {
            temp[i + 1] = basis;
        }
        break :blk temp;
    };

    const identity = comptime std.simd.iota(i32, basis_num + 1);
    return struct {
        val: [basis_num + 1]T = .{0} ** (basis_num + 1),
        const Self = @This();
        pub const Indices = indices;
        pub const BasisNum = basis_num;

        pub const MaskTo = identity;
        pub const Mask = identity;
        pub const Count = basis_num + 1;

        pub const Type = T;
        pub const SumDim = sum_of_dim;
        pub const AlgebraType = AlgebraEnum.FullAlgebra;

        pub const HodgeResult = Self;
        pub const Algebra = Self;

        pub fn getBladeCount(comptime k: usize) usize {
            var total: usize = 0;
            for (indices) |data| {
                if (data.count == k) {
                    total += 1;
                }
            }
            return total;
        }

        const blade_types = blk: {
            var struct_fields: [basis_num]std.builtin.Type.StructField = undefined;
            var enum_fields: [basis_num]std.builtin.Type.EnumField = undefined;

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

                struct_fields[i] = .{
                    .name = name.items,
                    .type = Self,
                    .default_value = @ptrCast(&res),
                    .is_comptime = true,
                    .alignment = 1,
                };

                enum_fields[i] = .{
                    .name = name.items,
                    .value = i,
                };
            }

            const blade_struct = @Type(.{
                .Struct = .{
                    .layout = .Auto,
                    .fields = &struct_fields,
                    .decls = &.{},
                    .is_tuple = false,
                },
            }){};

            const blade_enum = @Type(.{
                .Enum = .{
                    .tag_type = @Type(.{
                        .Int = .{ .signedness = .unsigned, .bits = basis_num },
                    }),
                    .fields = &enum_fields,
                    .decls = &.{},
                    .is_exhaustive = false,
                },
            });
            break :blk .{ blade_struct, blade_enum };
        };

        pub const Blades = blade_types[0];
        pub const BladeEnum = blade_types[1];

        pub fn get(a: Self, blade: BladeEnum) T {
            return a.val[@intFromEnum(blade) + 1];
        }

        pub fn set(a: *Self, blade: BladeEnum, val: T) void {
            a.val[@intFromEnum(blade) + 1] = val;
        }

        // a*b: geo
        // ~a: reverse
        // a^b: wedge
        // a&b: regressive
        // *a: dual
        // a|b: inner product
        // #a: grade involution
        // a$k: grade projection this is slightly cursed I'm not sure whether I'll keep it
        // %a: undual this is also not ideal
        pub const geoCtx = simpleCtx(struct {
            pub const UnOp = enum { @"#", @"~", @"-", @"*", @"%" };
            pub const BinOp = enum { @"+", @"-", @"*", @"^", @"$", @"|", @"&" };

            pub const allow_unused_inputs = true;

            pub const relations = .{
                .@"+" = .{ .prec = 10, .assoc = .left },
                .@"-" = .{ .prec = 10, .assoc = .left },
                .@"$" = .{ .prec = 10, .assoc = .left },
                .@"*" = .{ .prec = 20, .assoc = .left },
                .@"|" = .{ .prec = 20, .assoc = .left },
                .@"^" = .{ .prec = 20, .assoc = .left },
                .@"&" = .{ .prec = 20, .assoc = .left },
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
                    .@"~" => val.reverse(),
                    .@"#" => val.grade_involution(),
                    .@"*" => val.dual(),
                    .@"%" => val.undual(),
                };
            }

            pub fn EvalIdent(comptime ident: []const u8) type {
                if (comptime std.meta.stringToEnum(BladeEnum, ident)) |_| {
                    return Self;
                } else {
                    return noreturn;
                }
            }
            pub fn evalIdent(ctx: @This(), comptime ident: []const u8) !EvalIdent(ident) {
                _ = ctx;
                return @field(Blades, ident);
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
                    .@"&" => lhs.regressive(rhs),
                    .@"|" => lhs.inner(rhs),
                    .@"$" => lhs.grade_projection(in_rhs) catch @panic("Invalid K"),
                };
            }
        }{});

        pub fn eval(comptime input: []const u8, comptime args: anytype) !Self {
            return try comath.eval(input, geoCtx, args);
        }

        pub fn fromInt(num: anytype) Self {
            var res = Self{};
            res.val[0] = num;
            return res;
        }

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

        pub fn memoizedMultiplyBasis(comptime quadratic_form: Sign, a_i: usize, b_i: usize) struct { usize, T } {
            const res = switch (quadratic_form) {
                .pos => .{ posMatrix[0][a_i][b_i], posMatrix[1][a_i][b_i] },
                .neg => .{ negMatrix[0][a_i][b_i], negMatrix[1][a_i][b_i] },
                .zero => .{ zeroMatrix[0][a_i][b_i], zeroMatrix[1][a_i][b_i] },
            };
            return res;
        }

        const Generation = struct {
            [basis_num + 1][basis_num + 1]usize,
            [basis_num + 1][basis_num + 1]T,
        };

        fn generateMatrix(comptime quadratic_form: Sign) Generation {
            var temp: Generation = undefined;
            for (0..basis_num + 1) |i| {
                for (0..basis_num + 1) |j| {
                    @setEvalBranchQuota(1219541);
                    const res = multiplyBasis(quadratic_form, i, j);
                    temp[0][i][j] = res[0];
                    temp[1][i][j] = switch (res[1]) {
                        .pos => 1,
                        .neg => -1,
                        .zero => 0,
                    };
                }
            }
            return temp;
        }

        const posMatrix = generateMatrix(.pos);
        const negMatrix = generateMatrix(.neg);
        const zeroMatrix = generateMatrix(.zero);

        const PairType = ?struct { usize, usize };
        pub fn getAntiLen(comptime quadratic_form: Sign) usize {
            @setEvalBranchQuota(1219541);
            var largest: usize = 0;

            const size = 2048;
            var table: [size][basis_num + 1]bool = .{(.{false} ** (basis_num + 1))} ** size;

            for (0..basis_num + 1) |a_i| {
                for (0..basis_num + 1) |b_i| {
                    const res = comptime memoizedMultiplyBasis(quadratic_form, a_i, b_i);

                    var not_found = true;

                    for (table[0..largest]) |*val| {
                        if (val[res[0]] == false) {
                            val[res[0]] = true;
                            not_found = false;
                            break;
                        }
                    }

                    if (not_found) {
                        if (table[largest][res[0]] != false) largest += 1;

                        table[largest][res[0]] = true;
                    }
                }
            }
            return largest;
        }

        pub fn getBatchType(comptime LenMul: usize) type {
            return blades.getBatchTypeGen(Self, Self, LenMul);
        }

        pub fn anticommuteMemoize(comptime quadratic_form: Sign, comptime filterMat: anytype) type {
            const size = 2048;

            @setEvalBranchQuota(1219541);

            var multiply_a: [size][basis_num + 1]i32 = .{(.{-1} ** (basis_num + 1))} ** size;
            var multiply_b: [size][basis_num + 1]i32 = .{(.{-1} ** (basis_num + 1))} ** size;
            var select: [size][basis_num + 1]T = .{.{@as(T, 0)} ** (basis_num + 1)} ** size;

            var largest: usize = 0;

            for (0..basis_num + 1) |a_i| {
                for (0..basis_num + 1) |b_i| {
                    if (@TypeOf(filterMat) == void or filterMat[a_i][b_i] or filterMat[b_i][a_i]) {
                        const res = comptime memoizedMultiplyBasis(quadratic_form, a_i, b_i);

                        const sign: T = res[1];

                        var not_found = true;

                        for (multiply_a[0..largest], multiply_b[0..largest], select[0..largest]) |*m_a, *m_b, *sel| {
                            if (m_a[res[0]] == -1) {
                                m_a[res[0]] = a_i;
                                m_b[res[0]] = b_i;
                                sel[res[0]] = sign;
                                not_found = false;
                                break;
                            }
                        }

                        if (not_found) {
                            if (multiply_a[largest][res[0]] != -1) largest += 1;

                            multiply_a[largest][res[0]] = a_i;
                            multiply_b[largest][res[0]] = b_i;
                            select[largest][res[0]] = sign;
                        }
                    }
                }
            }

            const result_len = largest + 1;
            const Result = struct {
                [result_len][basis_num + 1]i32,
                [result_len][basis_num + 1]i32,
                [result_len][basis_num + 1]T,
            };
            var res_mut: Result = undefined;

            @memcpy(&res_mut[0], multiply_a[0 .. largest + 1]);
            @memcpy(&res_mut[1], multiply_b[0 .. largest + 1]);
            @memcpy(&res_mut[2], select[0 .. largest + 1]);

            operations.simplify(&.{ &res_mut[0], &res_mut[1], &res_mut[2] });

            const res = res_mut;

            return struct {
                pub const Res = res;
            };
        }

        pub fn anticommuteResult(comptime quadratic_form: Sign, comptime filterMat: anytype, comptime a: type, comptime b: type) type {
            _ = quadratic_form;
            _ = filterMat;
            _ = a;
            _ = b;
            return Self;
        }

        pub fn anticommute(a: Self, comptime quadratic_form: Sign, comptime filterMat: anytype, b: Self) Self {
            var c: @Vector(basis_num + 1, T) = .{0} ** (basis_num + 1);

            const op = anticommuteMemoize(quadratic_form, filterMat);

            operations.runOps(op.Res, a, b, &c);

            return Self{ .val = c };
        }

        pub const commonMatrix = blk: {
            var temp: [basis_num + 1][basis_num + 1]bool = undefined;
            for (0..basis_num + 1) |i| {
                for (0..basis_num + 1) |j| {
                    const blade_a = indices[i];
                    const blade_b = indices[j];

                    var in_common = true;

                    for (blade_a.tags[0..blade_a.count]) |tag_a| {
                        var tag_not = true;
                        for (blade_b.tags[0..blade_b.count]) |tag_b| {
                            if (tag_a == tag_b) {
                                tag_not = false;
                                break;
                            }
                        }
                        if (tag_not) {
                            in_common = false;
                            break;
                        }
                    }
                    temp[i][j] = in_common;
                }
            }

            break :blk temp;
        };

        pub fn inner(a: Self, b: Self) Self {
            return a.anticommute(.pos, commonMatrix, b);
        }

        pub fn mul(a: Self, b: Self) Self {
            return a.anticommute(.pos, void{}, b);
        }

        pub fn regressive(a: Self, b: Self) Self {
            return a.dual().wedge(b.dual()).undual();
        }

        pub fn wedge(a: Self, b: Self) Self {
            return a.anticommute(.zero, void{}, b);
        }

        pub fn Mul(comptime a: type, comptime b: type) type {
            _ = a;
            _ = b;
            return Self;
        }

        pub fn Wedge(comptime a: type, comptime b: type) type {
            _ = a;
            _ = b;
            return Self;
        }

        pub fn Inner(comptime a: type, comptime b: type) type {
            _ = a;
            _ = b;
            return Self;
        }
        pub const grade_involution_mask: @Vector(basis_num + 1, T) = blk: {
            var temp: [basis_num + 1]T = undefined;
            for (&temp, 0..) |*val, i| {
                const len = indices[i].count;
                if (len % 2 == 0) {
                    val.* = 1;
                } else {
                    val.* = -1;
                }
            }
            break :blk temp;
        };

        pub fn grade_involution(a: Self) Self {
            return Self{ .val = a.val * grade_involution_mask };
        }

        pub const reverse_mask: @Vector(basis_num + 1, T) = blk: {
            var temp: [basis_num + 1]T = undefined;
            for (&temp, 0..) |*val, i| {
                const len = indices[i].count;
                if (len % 4 == 0 or (len > 0 and (len - 1) % 4 == 0)) {
                    val.* = 1;
                } else {
                    val.* = -1;
                }
            }
            break :blk temp;
        };

        pub fn reverse(a: Self) Self {
            return Self{ .val = a.val * reverse_mask };
        }

        pub fn getGradeMask(k: usize) @Vector(basis_num + 1, T) {
            return blk: {
                var temp: [basis_num + 1]T = undefined;
                for (&temp, 0..) |*val, i| {
                    const len = indices[i].count;
                    if (len == k) {
                        val.* = 1;
                    } else {
                        val.* = 0;
                    }
                }
                break :blk temp;
            };
        }

        pub fn grade_projection(a: Self, k_in: usize) !Self {
            var res = Self{};

            switch (k_in) {
                sum_of_dim + 1...std.math.maxInt(usize) => |_| {
                    return error.InvalidK;
                },
                inline else => |k| {
                    const mask: @Vector(basis_num + 1, T) = comptime getGradeMask(k);
                    res.val = a.val * mask;
                },
            }

            return res;
        }

        pub fn abs2(a: Self) Self {
            return a.reverse().mul(a).grade_projection(0) catch unreachable;
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
                    _ = try writer.print("{d:.4}", .{val});
                } else {
                    const basis = indices[index];
                    _ = try writer.print("{d:.4}e", .{val});
                    for (basis.tags[0..basis.count]) |basis_idx| {
                        var actual: i32 = @intCast(basis_idx);
                        actual -= @intCast(null_dim);
                        _ = try writer.print("{}", .{actual});
                    }
                }
            }
            _ = try writer.writeByte(0);

            return name.items;
        }

        pub fn dual(a: Self) Self {
            if (null_dim == 0) {
                var pseudo_scalar = Self{};
                pseudo_scalar.val[basis_num] = 1;
                return a.mul(pseudo_scalar);
            }
            return a.hodge();
        }

        pub fn undual(a: Self) Self {
            if (null_dim == 0) {
                var pseudo_scalar = Self{};
                pseudo_scalar.val[basis_num] = 1;
                return a.mul(pseudo_scalar.reverse());
            }
            return a.unhodge();
        }

        pub const shuffle_mask: @Vector(basis_num + 1, T) = blk: {
            var temp: [basis_num + 1]i32 = undefined;
            for (0..basis_num + 1) |i| {
                temp[i] = basis_num - i;
            }
            break :blk temp;
        };

        pub const hodge_mask = blk: {
            var temp: [basis_num + 1]T = undefined;
            for (0..basis_num + 1) |i| {
                const res = memoizedMultiplyBasis(.pos, i, basis_num - i);
                temp[basis_num - i] = res[1];
            }
            break :blk temp;
        };

        pub const unhodge_mask = blk: {
            var temp: [basis_num + 1]T = undefined;
            for (0..basis_num + 1) |i| {
                const res = memoizedMultiplyBasis(.pos, basis_num - i, i);
                temp[basis_num - i] = res[1];
            }
            break :blk temp;
        };

        pub fn hodge(a: Self) Self {
            return Self{ .val = @shuffle(T, a.val, undefined, shuffle_mask) * hodge_mask };
        }

        pub fn unhodge(a: Self) Self {
            return Self{ .val = @shuffle(T, a.val, undefined, shuffle_mask) * unhodge_mask };
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

test "regular algebra" {
    const Alg = Algebra(i32, 3, 0, 0);

    // regressive product
    try std.testing.expectEqualSlices(
        i32,
        &(try Alg.eval("-e3", .{})).val,
        &(try Alg.eval("e13 & -e23", .{})).val,
    );
}

test "algebra" {
    const Alg = Algebra(i32, 2, 1, 1);

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

    try std.testing.expectEqualSlices(
        i32,
        &(try Alg.eval("(2*e1+3*e12)*(e1+2*e2)", .{})).val,
        &(try Alg.eval("6*e1-3*e2+4*e12+2", .{})).val,
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

    var set_test = try Alg.eval(big, .{});
    set_test.set(.e023, 666);

    try std.testing.expectEqual(set_test.get(.e023), 666);
}

test "dual numbers" {
    // function we want derivatives of
    const Alg = Algebra(i32, 0, 0, 1);

    const fn_str = "(x + off1) * (x + off2)";

    // evaluate regular function
    try std.testing.expectEqualSlices(
        i32,
        &.{ 20, 0 },
        &(try Alg.eval(fn_str, .{ .x = 3, .off1 = 2, .off2 = 1 })).val,
    );

    // evaluate derivative
    try std.testing.expectEqualSlices(
        i32,
        &.{ 20, 9 },
        &(try Alg.eval(fn_str, .{ .x = try Alg.eval("3+e0", .{}), .off1 = 2, .off2 = 1 })).val,
    );
}
