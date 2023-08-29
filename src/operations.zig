pub fn simplifyZeroes(vals: anytype) void {
    var op_a = vals[0];
    var op_b = vals[1];
    var op_m = vals[2];

    const len = op_a[0].len;

    const nothings: @Vector(len, i32) = .{-1} ** len;

    for (op_a, op_b, op_m, 0..) |a_row, b_row, s_row, row_i| {
        const invalid = @reduce(.And, a_row == nothings) or @reduce(.And, b_row == nothings);
        if (invalid) continue;
        for (a_row, b_row, s_row, 0..) |a_elem, b_elem, s_elem, elem_i| {
            if (s_elem != 0) {
                for (0..row_i) |rep_i| {
                    const s_rep = op_m[rep_i][elem_i];
                    const invalid_rep = @reduce(.And, op_a[rep_i] == nothings) or @reduce(.And, op_b[rep_i] == nothings);
                    if (invalid_rep) continue;

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
}
pub fn isAt(vals: anytype, s_row: anytype, row_i: anytype, search_positive: anytype) bool {
    var op_m = vals[2];
    for (0..s_row.len) |elem_i| {
        if ((s_row[elem_i] == 1 and search_positive) or (s_row[elem_i] == -1 and !search_positive)) {
            continue;
        }
        var found = false;
        for (op_m[row_i + 1 ..]) |candidate_row| {
            const candidate = candidate_row[elem_i];
            if ((candidate == 1 and search_positive) or (candidate == -1 and !search_positive)) {
                found = true;
                break;
            }
        }
        if (found) continue;
        return false;
    }
    return true;
}

pub fn simplify(vals: anytype) void {
    comptime {
        var op_a = vals[0];
        var op_b = vals[1];
        var op_m = vals[2];

        for (op_m, 0..) |s_row, row_i| {
            var search_positive = true;

            if (!isAt(vals, s_row, row_i, search_positive)) {
                search_positive = false;
                if (!isAt(vals, s_row, row_i, search_positive)) continue;
            }

            for (0..s_row.len) |elem_i| {
                if ((s_row[elem_i] == 1 and search_positive) or (s_row[elem_i] == -1 and !search_positive)) {
                    continue;
                }
                for (op_m[row_i + 1 ..], row_i + 1..) |candidate_row, i| {
                    const candidate = candidate_row[elem_i];
                    if ((candidate == 1 and search_positive) or (candidate == -1 and !search_positive)) {
                        const a = op_a[row_i][elem_i];
                        const b = op_b[row_i][elem_i];
                        const m = op_m[row_i][elem_i];

                        op_a[row_i][elem_i] = op_a[i][elem_i];
                        op_b[row_i][elem_i] = op_b[i][elem_i];
                        op_m[row_i][elem_i] = op_m[i][elem_i];

                        op_a[i][elem_i] = a;
                        op_b[i][elem_i] = b;
                        op_m[i][elem_i] = m;
                    }
                }
            }
        }
    }
}
