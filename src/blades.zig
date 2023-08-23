const geo = @import("geo.zig");
const std = @import("std");

pub fn Blades(comptime Alg: type) type {
    const types = type_blk: {
        var res: [Alg.SumDim + 2]type = undefined;
        inline for (0..Alg.SumDim + 1) |k| {
            const it = blk: {
                const count = comptime Alg.getBladeCount(k);

                const mask = mask_blk: {
                    var temp: [count]i32 = undefined;

                    var index: usize = 0;
                    for (Alg.Indices, 0..) |data, i| {
                        if (data.count == k) {
                            temp[index] = i;
                            index += 1;
                        }
                    }
                    break :mask_blk temp;
                };

                const mask_to = mask_blk: {
                    var temp: [Alg.BasisNum + 1]i32 = .{-1} ** (Alg.BasisNum + 1);

                    var index: usize = 0;
                    for (Alg.Indices, 0..) |data, i| {
                        if (data.count == k) {
                            temp[i] = index;
                            index += 1;
                        }
                    }
                    break :mask_blk temp;
                };

                break :blk struct {
                    pub const Mask = mask;
                    pub const MaskTo = mask_to;
                    pub const Count = count;
                    pub const K = k;
                    val: [count]Alg.Type = .{0} ** count,

                    pub fn print(a: @This(), buf: []u8) ![]const u8 {
                        const zeroes: @Vector(count, Alg.Type) = .{0} ** count;
                        var val = Alg{ .val = @shuffle(Alg.Type, a.val, zeroes, mask_to) };
                        return val.print(buf);
                    }
                };
            };
            res[k] = it;
        }

        const count = blk: {
            var temp: usize = 0;
            for (Alg.Indices) |data| {
                if (data.count % 2 == 0) temp += 1;
            }
            break :blk temp;
        };

        const mask = mask_blk: {
            var temp: [count]i32 = undefined;

            var index: usize = 0;
            for (Alg.Indices, 0..) |data, i| {
                if (data.count % 2 == 0) {
                    temp[index] = i;
                    index += 1;
                }
            }
            break :mask_blk temp;
        };

        const mask_to = mask_blk: {
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

        res[Alg.SumDim + 1] = struct {
            pub const Mask = mask;
            pub const MaskTo = mask_to;
            pub const Count = count;
            pub const K = Alg.BasisNum;
            val: [count]Alg.Type = .{0} ** count,

            pub fn print(a: @This(), buf: []u8) ![]const u8 {
                const zeroes: @Vector(count, Alg.Type) = .{0} ** count;
                var val = Alg{ .val = @shuffle(Alg.Type, a.val, zeroes, mask_to) };
                return val.print(buf);
            }
        };

        break :type_blk res;
    };

    return struct {
        pub const Types = types;

        pub fn anticommuteResult(comptime quadratic_form: geo.Sign, comptime a: type, comptime b: type) type {
            @setEvalBranchQuota(1219541);

            const op = switch (quadratic_form) {
                .pos => Alg.posOp,
                .neg => Alg.negOp,
                .zero => Alg.zeroOp,
            };

            const res = op.Res;
            var first = true;
            var candidate: usize = 0;
            const a_k = a.K;
            const b_k = b.K;
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

        pub fn mul(a: anytype, b: anytype) anticommuteResult(.pos, @TypeOf(a), @TypeOf(b)) {
            return anticommute_blade(.pos, a, b);
        }

        pub fn wedge(a: anytype, b: anytype) anticommuteResult(.zero, @TypeOf(a), @TypeOf(b)) {
            return anticommute_blade(.zero, a, b);
        }

        pub fn anticommuteBatch(
            comptime a_t: type,
            comptime b_t: type,
            comptime len_of_mul: usize,
            a: [a_t.Count]@Vector(len_of_mul, Alg.Type),
            comptime quadratic_form: geo.Sign,
            b: [b_t.Count]@Vector(len_of_mul, Alg.Type),
        ) [anticommuteResult(quadratic_form, a_t, b_t).Count]@Vector(len_of_mul, Alg.Type) {
            var vec: [Alg.BasisNum + 1]@Vector(len_of_mul, Alg.Type) = .{.{0} ** len_of_mul} ** (Alg.BasisNum + 1);

            const Result = anticommuteResult(quadratic_form, a_t, b_t);

            inline for (0..a_t.Count) |a_i| {
                inline for (0..b_t.Count) |b_i| {
                    const a_idx = comptime a_t.Mask[a_i];
                    const b_idx = comptime b_t.Mask[b_i];

                    if (a_idx != -1 and b_idx != -1) {
                        const res = comptime Alg.memoizedMultiplyBasis(.pos, a_idx, b_idx);

                        const sign: Alg.Type = res[1];
                        const a_us = a[a_i];
                        const b_us = b[b_i];
                        const r_idx: usize = @intCast(Result.MaskTo[res[0]]);
                        if (sign != 0) {
                            const actual_sign: @Vector(len_of_mul, Alg.Type) = @splat(res[1]);
                            vec[r_idx] += a_us * b_us * actual_sign;
                        }
                    }
                }
            }
            return vec;
        }

        pub fn anticommute_blade(comptime quadratic_form: geo.Sign, a: anytype, b: anytype) anticommuteResult(quadratic_form, @TypeOf(a), @TypeOf(b)) {
            const a_t = @TypeOf(a);
            const b_t = @TypeOf(b);
            const Result = anticommuteResult(quadratic_form, a_t, b_t);

            const identity = std.simd.iota(i32, Alg.BasisNum + 1);

            const result_mask_to = if (Result == Alg) identity else Result.MaskTo;
            const result_mask = if (Result == Alg) identity else Result.Mask;
            const result_count = if (Result == Alg) Alg.BasisNum + 1 else Result.Count;

            var c: @Vector(Result.Count, Alg.Type) = .{0} ** (Result.Count);

            const op = switch (quadratic_form) {
                .pos => Alg.posOp,
                .neg => Alg.negOp,
                .zero => Alg.zeroOp,
            };

            const ops = comptime ops_blk: {
                var op_a: [op.Res[0].len][result_count]i32 = undefined;
                var op_b: [op.Res[0].len][result_count]i32 = undefined;
                var op_m: [op.Res[0].len][result_count]Alg.Type = undefined;
                var op_invalid: [op.Res[0].len]bool = undefined;

                inline for (op.Res[0], op.Res[1], op.Res[2], 0..) |sel_a, sel_b, mult, op_i| {
                    const res = blk: {
                        const nothings: @Vector(result_count, i32) = .{-1} ** result_count;

                        @setEvalBranchQuota(2108350);
                        var mask_a_mut: [result_count]i32 = .{-1} ** result_count;
                        for (sel_a, 0..) |to, from| {
                            const mask_loc = result_mask_to[from];
                            if (mask_loc == -1 or to == -1) continue;
                            mask_a_mut[@intCast(mask_loc)] = a_t.MaskTo[to];
                        }

                        var mask_b_mut: [result_count]i32 = .{-1} ** result_count;
                        for (sel_b, 0..) |to, from| {
                            const mask_loc = result_mask_to[from];
                            if (mask_loc == -1 or to == -1) continue;
                            mask_b_mut[@intCast(mask_loc)] = b_t.MaskTo[to];
                        }

                        var mul_mut: [result_count]Alg.Type = @shuffle(Alg.Type, mult, mult, result_mask);
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
}
