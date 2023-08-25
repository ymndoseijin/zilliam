const geo = @import("geo.zig");
const std = @import("std");

pub fn getBatchTypeGen(comptime Alg: type, comptime T: type, comptime len_mul: usize) type {
    return struct {
        pub const Type = T;

        pub const Mask = T.Mask;
        pub const MaskTo = T.MaskTo;

        pub const LenMul = len_mul;
        pub const Algebra = Alg;

        val: [T.Count]@Vector(LenMul, Alg.Type) = .{.{0} ** LenMul} ** T.Count,
        pub fn get(vec: @This(), i: usize) T {
            var res: [T.Count]Alg.Type = undefined;
            for (vec.val, 0..) |v, v_i| {
                res[v_i] = v[i];
            }
            return .{ .val = res };
        }

        pub fn anticommuteBatch(
            a: @This(),
            comptime quadratic_form: geo.Sign,
            comptime filterMat: anytype,
            b: anytype,
        ) getBatchTypeGen(Alg, T.anticommuteResult(quadratic_form, filterMat, T, @TypeOf(b).Type), LenMul) {
            const Result = T.anticommuteResult(quadratic_form, filterMat, T, @TypeOf(b).Type);
            var vec: [Result.Count]@Vector(LenMul, Alg.Type) = .{.{0} ** LenMul} ** (Result.Count);

            const b_t = @TypeOf(b);

            inline for (0..T.Count) |a_i| {
                inline for (0..b_t.Type.Count) |b_i| {
                    const a_idx = comptime T.Mask[a_i];
                    const b_idx = comptime b_t.Mask[b_i];

                    if (a_idx != -1 and b_idx != -1) {
                        if (@TypeOf(filterMat) == void or filterMat[a_idx][b_idx] or filterMat[b_idx][a_idx]) {
                            const res = comptime Alg.memoizedMultiplyBasis(quadratic_form, a_idx, b_idx);

                            const sign: Alg.Type = res[1];
                            const a_us = a.val[a_i];
                            const b_us = b.val[b_i];
                            const r_idx = Result.MaskTo[res[0]];
                            if (sign != 0 and r_idx != -1) {
                                const actual_sign: @Vector(LenMul, Alg.Type) = @splat(res[1]);
                                vec[@intCast(r_idx)] += a_us * b_us * actual_sign;
                            }
                        }
                    }
                }
            }
            return .{ .val = vec };
        }

        pub fn mul(a: @This(), b: anytype) getBatchTypeGen(Alg, T.Mul(T, @TypeOf(b).Type), LenMul) {
            return anticommuteBatch(a, .pos, void{}, b);
        }

        pub fn wedge(a: @This(), b: anytype) getBatchTypeGen(Alg, T.Wedge(T, @TypeOf(b).Type), LenMul) {
            return anticommuteBatch(a, .zero, void{}, b);
        }

        pub fn inner(a: @This(), b: anytype) getBatchTypeGen(Alg, T.Inner(T, @TypeOf(b).Type), LenMul) {
            return anticommuteBatch(a, .pos, Alg.commonMatrix, b);
        }

        pub fn sub(a: @This(), b: anytype) @This() {
            const vec: [T.Count]@Vector(LenMul, Alg.Type) = a.val;
            for (a.val, b.val, &vec) |a_elem, b_elem, *v_elem| {
                v_elem.* = a_elem - b_elem;
            }
            return .{ .val = vec };
        }

        pub fn add(a: @This(), b: anytype) @This() {
            const vec: [T.Count]@Vector(LenMul, Alg.Type) = a.val;
            for (a.val, b.val, &vec) |a_elem, b_elem, *v_elem| {
                v_elem.* = a_elem + b_elem;
            }
            return .{ .val = vec };
        }
    };
}

pub fn Blades(comptime Alg: type) type {
    const types = type_blk: {
        var res_types: [Alg.SumDim + 2]type = undefined;
        const res_ptr = &res_types;
        inline for (0..Alg.SumDim + 2) |k| {
            const it = blk: {
                const blade_count = if (k < Alg.SumDim + 1) comptime Alg.getBladeCount(k) else count_blk: {
                    var temp: usize = 0;
                    for (Alg.Indices) |data| {
                        if (data.count % 2 == 0) temp += 1;
                    }
                    break :count_blk temp;
                };

                const blade_mask = if (k < Alg.SumDim + 1) mask_blk: {
                    var temp: [blade_count]i32 = undefined;

                    var index: usize = 0;
                    for (Alg.Indices, 0..) |data, i| {
                        if (data.count == k) {
                            temp[index] = i;
                            index += 1;
                        }
                    }
                    break :mask_blk temp;
                } else mask_blk: {
                    var temp: [blade_count]i32 = undefined;

                    var index: usize = 0;
                    for (Alg.Indices, 0..) |data, i| {
                        if (data.count % 2 == 0) {
                            temp[index] = i;
                            index += 1;
                        }
                    }
                    break :mask_blk temp;
                };

                const blade_mask_to = if (k < Alg.SumDim + 1) mask_blk: {
                    var temp: [Alg.BasisNum + 1]i32 = .{-1} ** (Alg.BasisNum + 1);

                    var index: usize = 0;
                    for (Alg.Indices, 0..) |data, i| {
                        if (data.count == k) {
                            temp[i] = index;
                            index += 1;
                        }
                    }
                    break :mask_blk temp;
                } else mask_blk: {
                    var temp: [Alg.BasisNum + 1]i32 = .{-1} ** (Alg.BasisNum + 1);

                    var index: usize = 0;
                    for (Alg.Indices, 0..) |data, i| {
                        if (data.count % 2 == 0) {
                            temp[i] = index;
                            index += 1;
                        }
                    }
                    break :mask_blk temp;
                };

                break :blk struct {
                    pub const Mask = blade_mask;
                    pub const MaskTo = blade_mask_to;
                    pub const Count = blade_count;
                    pub const K = k;
                    pub const Types = res_ptr;
                    pub const Identity: BladeType = .{ .val = .{1} ** Count };
                    pub const Algebra = Alg;
                    pub const AlgebraType = geo.AlgebraEnum.SubAlgebra;

                    val: [Count]Alg.Type = .{0} ** Count,

                    const BladeType = @This();

                    pub fn print(a: @This(), buf: []u8) ![]const u8 {
                        const zeroes: @Vector(Count, Alg.Type) = .{0} ** Count;
                        var val = Alg{ .val = @shuffle(Alg.Type, a.val, zeroes, MaskTo) };
                        return val.print(buf);
                    }

                    pub fn getBatchType(comptime LenMul: usize) type {
                        return getBatchTypeGen(Alg, BladeType, LenMul);
                    }

                    pub fn toK(a: BladeType, comptime ResType: type) ResType {
                        const result_count = ResType.Count;
                        const mask_a_mut = comptime blk: {
                            var temp: [result_count]i32 = .{-1} ** result_count;
                            for (0..Count) |i| {
                                const mask_loc = ResType.MaskTo[Mask[i]];
                                if (mask_loc == -1) @compileError("Invalid K");
                                temp[@intCast(mask_loc)] = i;
                            }
                            break :blk temp;
                        };
                        const neverweres: @Vector(Count, Alg.Type) = .{0} ** Count;
                        return .{ .val = @shuffle(Alg.Type, a.val, neverweres, mask_a_mut) };
                    }

                    pub fn sub(a: BladeType, b: anytype) BladeType {
                        const vec: @Vector(Count, Alg.Type) = a.val;
                        return .{ .val = vec - b.toK(Types[K]).val };
                    }

                    pub fn add(a: BladeType, b: anytype) BladeType {
                        const vec: @Vector(Count, Alg.Type) = a.val;
                        return .{ .val = vec + b.toK(Types[K]).val };
                    }

                    pub fn get(a: BladeType, blade: Alg.BladeEnum) Alg.Type {
                        return a.val[@intCast(MaskTo[@intFromEnum(blade) + 1])];
                    }

                    pub fn set(a: *BladeType, blade: Alg.BladeEnum, val: Alg.Type) void {
                        a.val[@intCast(MaskTo[@intFromEnum(blade) + 1])] = val;
                    }

                    pub const HodgeResult = Types[Alg.Indices[Alg.BasisNum - Mask[0]].count];

                    const shuffle_mask = blk: {
                        const Size = HodgeResult.Count;
                        var temp: [Size]i32 = .{-1} ** Size;
                        for (0..Size) |i| {
                            const idx = Alg.BasisNum - HodgeResult.Mask[i];
                            temp[i] = MaskTo[idx];
                        }
                        break :blk temp;
                    };

                    pub fn unhodge(a: BladeType) HodgeResult {
                        const Size = HodgeResult.Count;
                        const mask = comptime blk: {
                            var temp: @Vector(Size, Alg.Type) = .{-1} ** Size;

                            for (0..Count) |i| {
                                const other = Alg.BasisNum - Mask[i];
                                const res = Alg.memoizedMultiplyBasis(.pos, other, Mask[i]);
                                const idx = HodgeResult.MaskTo[other];
                                temp[idx] = res[1];
                            }
                            break :blk temp;
                        };

                        return HodgeResult{ .val = @shuffle(Alg.Type, a.val, a.val, shuffle_mask) * mask };
                    }

                    pub fn hodge(a: BladeType) HodgeResult {
                        const Size = HodgeResult.Count;
                        const mask = comptime blk: {
                            var temp: @Vector(Size, Alg.Type) = .{-1} ** Size;

                            for (0..Count) |i| {
                                const other = Alg.BasisNum - Mask[i];
                                const res = Alg.memoizedMultiplyBasis(.pos, Mask[i], other);
                                const idx = HodgeResult.MaskTo[other];
                                temp[idx] = res[1];
                            }
                            break :blk temp;
                        };

                        return HodgeResult{ .val = @shuffle(Alg.Type, a.val, a.val, shuffle_mask) * mask };
                    }

                    pub fn dual(a: BladeType) HodgeResult {
                        return hodge(a);
                    }

                    pub fn undual(a: BladeType) HodgeResult {
                        return unhodge(a);
                    }

                    pub fn regressive(a: BladeType, b: anytype) Regressive(BladeType, @TypeOf(b)) {
                        return a.dual().wedge(b.dual()).undual();
                    }

                    pub fn grade_involution(a: BladeType) BladeType {
                        const mask: @Vector(Count, Alg.Type) = comptime blk: {
                            var temp: [Count]Alg.Type = undefined;
                            for (&temp, 0..) |*val, i| {
                                const len = Alg.Indices[Mask[i]].count;
                                if (len % 2 == 0) {
                                    val.* = 1;
                                } else {
                                    val.* = -1;
                                }
                            }
                            break :blk temp;
                        };

                        return BladeType{ .val = a.val * mask };
                    }

                    pub fn reverse(a: BladeType) BladeType {
                        const mask: @Vector(Count, Alg.Type) = comptime blk: {
                            var temp: [Count]Alg.Type = undefined;
                            for (&temp, 0..) |*val, i| {
                                const len = Alg.Indices[Mask[i]].count;
                                if (len % 4 == 0 or (len > 0 and (len - 1) % 4 == 0)) {
                                    val.* = 1;
                                } else {
                                    val.* = -1;
                                }
                            }
                            break :blk temp;
                        };

                        return BladeType{ .val = a.val * mask };
                    }

                    pub fn grade_projection(a: BladeType, comptime k_in: usize) Types[k_in] {
                        const ResType = Types[k_in];
                        const neverweres = ResType{ .val = .{0} ** ResType.Count };

                        if (BladeType == ResType) return a;

                        if (BladeType == Types[Alg.SumDim + 1]) {
                            var res = ResType{};
                            const mask: @Vector(ResType.Count, Alg.Type) = comptime blk: {
                                var temp: [ResType.Count]Alg.Type = undefined;
                                for (0..ResType.Count) |i| {
                                    const len = Alg.Indices[ResType.Mask[i]].count;
                                    if (len == k_in) {
                                        temp[i] = ResType.MaskTo[ResType.Mask[i]];
                                    } else {
                                        temp[i] = -1;
                                    }
                                }
                                break :blk temp;
                            };
                            res.val = @shuffle(Alg.Type, a.val, neverweres.val, mask);

                            return res;
                        }
                        return neverweres;
                    }

                    pub fn abs2(a: BladeType) BladeType {
                        return a.reverse().mul(a).grade_projection(0) catch unreachable;
                    }

                    pub fn anticommuteResult(comptime quadratic_form: geo.Sign, comptime filterMat: anytype, comptime a: type, comptime b: type) type {
                        @setEvalBranchQuota(1219541);

                        const op = Alg.anticommuteMemoize(quadratic_form, filterMat);

                        const res = op.Res;
                        var first = true;
                        var candidate: usize = 0;

                        if (b.AlgebraType == .FullAlgebra) return b;

                        const a_k = a.K;
                        const b_k = b.K;

                        // even
                        if (a_k == Alg.SumDim + 1 or b_k == Alg.SumDim + 1) return Types[Types.len - 1];

                        for (res[0], res[1]) |sel_a, sel_b| {
                            for (sel_a, sel_b, 0..) |val_a, val_b, i| {
                                if (val_a == -1 or val_b == -1) continue;
                                const k_1 = Alg.Indices[val_a].count;
                                const k_2 = Alg.Indices[val_b].count;
                                if ((k_1 == a_k and k_2 == b_k) or
                                    (k_1 == b_k and k_2 == a_k))
                                {
                                    const op_sign = comptime Alg.memoizedMultiplyBasis(quadratic_form, val_a, val_b)[1];
                                    if (first and op_sign != 0) {
                                        candidate = Alg.Indices[i].count;
                                        first = false;
                                    } else if (Alg.Indices[i].count != candidate and op_sign != 0) {
                                        if ((candidate % 2 == 0 and Alg.Indices[i].count % 2 == 0) or
                                            (candidate == Types.len - 1))
                                        {
                                            candidate = Types.len - 1;
                                            continue;
                                        }
                                        return Alg;
                                    }
                                }
                            }
                        }

                        return Types[candidate];
                    }

                    pub fn Mul(comptime a: type, comptime b: type) type {
                        return anticommuteResult(.pos, void{}, a, b);
                    }

                    pub fn Wedge(comptime a: type, comptime b: type) type {
                        return anticommuteResult(.zero, void{}, a, b);
                    }

                    pub fn Inner(comptime a: type, comptime b: type) type {
                        return anticommuteResult(.pos, Alg.commonMatrix, a, b);
                    }

                    pub fn Regressive(comptime a: type, comptime b: type) type {
                        return Wedge(a.HodgeResult, b.HodgeResult).HodgeResult;
                    }

                    pub fn mul(a: @This(), b: anytype) Mul(@TypeOf(a), @TypeOf(b)) {
                        return anticommute_blade(.pos, void{}, a, b);
                    }

                    pub fn wedge(a: @This(), b: anytype) Wedge(@TypeOf(a), @TypeOf(b)) {
                        return anticommute_blade(.zero, void{}, a, b);
                    }

                    pub fn inner(a: @This(), b: anytype) Inner(@TypeOf(a), @TypeOf(b)) {
                        return anticommute_blade(.pos, Alg.commonMatrix, a, b);
                    }

                    pub fn anticommute_blade(comptime quadratic_form: geo.Sign, comptime filterMat: anytype, a: anytype, b: anytype) anticommuteResult(quadratic_form, filterMat, @TypeOf(a), @TypeOf(b)) {
                        const a_t = @TypeOf(a);
                        const b_t = @TypeOf(b);
                        const Result = anticommuteResult(quadratic_form, filterMat, a_t, b_t);

                        var c: @Vector(Result.Count, Alg.Type) = .{0} ** (Result.Count);

                        const op = Alg.anticommuteMemoize(quadratic_form, filterMat);

                        const ops = comptime ops_blk: {
                            var op_a: [op.Res[0].len][Result.Count]i32 = undefined;
                            var op_b: [op.Res[0].len][Result.Count]i32 = undefined;
                            var op_m: [op.Res[0].len][Result.Count]Alg.Type = undefined;
                            var op_invalid: [op.Res[0].len]bool = undefined;

                            inline for (op.Res[0], op.Res[1], op.Res[2], 0..) |sel_a, sel_b, mult, op_i| {
                                const res = blk: {
                                    const nothings: @Vector(Result.Count, i32) = .{-1} ** Result.Count;

                                    @setEvalBranchQuota(2108350);
                                    var mask_a_mut: [Result.Count]i32 = .{-1} ** Result.Count;
                                    for (sel_a, 0..) |to, from| {
                                        const mask_loc = Result.MaskTo[from];
                                        if (mask_loc == -1 or to == -1) continue;
                                        mask_a_mut[@intCast(mask_loc)] = a_t.MaskTo[to];
                                    }

                                    var mask_b_mut: [Result.Count]i32 = .{-1} ** Result.Count;
                                    for (sel_b, 0..) |to, from| {
                                        const mask_loc = Result.MaskTo[from];
                                        if (mask_loc == -1 or to == -1) continue;
                                        mask_b_mut[@intCast(mask_loc)] = b_t.MaskTo[to];
                                    }

                                    var mul_mut: [Result.Count]Alg.Type = @shuffle(Alg.Type, mult, mult, Result.Mask);
                                    for (&mul_mut, 0..) |*val, i| {
                                        if (mask_a_mut[i] == -1 or mask_b_mut[i] == -1) val.* = 0;
                                    }

                                    break :blk .{ @reduce(.And, mask_a_mut == nothings) or @reduce(.And, mask_b_mut == nothings), mask_a_mut, mask_b_mut, mul_mut };
                                };

                                op_a[op_i] = res[1];
                                op_b[op_i] = res[2];
                                op_m[op_i] = res[3];
                                op_invalid[op_i] = res[0];
                            }

                            for (op_a, op_b, op_m, 0..) |a_row, b_row, s_row, row_i| {
                                for (a_row, b_row, s_row, 0..) |a_elem, b_elem, s_elem, elem_i| {
                                    if (s_elem != 0 and !op_invalid[row_i]) {
                                        for (0..row_i) |rep_i| {
                                            const s_rep = op_m[rep_i][elem_i];
                                            if (op_invalid[rep_i]) continue;

                                            if (s_rep == 0) {
                                                op_m[rep_i][elem_i] = s_elem;
                                                op_a[rep_i][elem_i] = a_elem;
                                                op_b[rep_i][elem_i] = b_elem;
                                                op_m[row_i][elem_i] = 0;
                                                break;
                                            }
                                        }
                                    }
                                }
                            }

                            break :ops_blk .{ op_a, op_b, op_m, op_invalid };
                        };

                        const op_a = ops[0];
                        const op_b = ops[1];
                        const op_m = ops[2];
                        const op_invalid = ops[3];

                        inline for (op_a, op_b, op_m, op_invalid) |mask_a, mask_b, mask_m, invalid| {
                            //const neverweres: @Vector(Result.Count, T) = .{0} ** Result.Count;
                            const mask_m_count = comptime blk: {
                                var count: usize = 0;
                                for (mask_m) |v| {
                                    if (v == 0) count += 1;
                                }
                                break :blk count;
                            };
                            if (invalid == false and (mask_m_count != Result.Count)) {
                                var first = @shuffle(Alg.Type, a.val, a.val, mask_a);
                                var second = @shuffle(Alg.Type, b.val, b.val, mask_b);
                                c += first * second * mask_m;
                            }
                        }

                        return Result{ .val = c };
                    }
                };
            };
            res_types[k] = it;
        }

        break :type_blk res_types;
    };

    return struct {
        pub const Types = types;
    };
}
