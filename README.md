# zilliam

This is a work in progress, but currently generates any arbitrary geometric algebra of any positive, negative and null dimension in compile time.

It currently has the following operations (syntax for the eval function):
- a*b: geo
- ~a: reverse
- a^b: wedge
- a&b: regressive
- *a: dual
- a|b: inner product
- #a: grade involution
- a$k: grade projection
- %a: undual

It generates SIMD operations for any generic Clifford algebra for all these operations, and any generic type as well:

```zig
const Alg = Algebra(i32, 2, 1, 1);

try std.testing.expectEqualSlices(
    i32,
    &(try Alg.eval("(14*e1) - (21*e2) - (7*e12) - 24", .{})).val,
    &(try Alg.eval("(2*e1+3*e2+5*e12) * (11*e1+13*e2+17*e12)", .{})).val,
);

try std.testing.expectEqualSlices(
    i32,
    &(try Alg.eval("109 + 654*e0 + 129*e1 + 127*e2 + 214*e12", .{})).val,
    &(try Alg.eval("(23*e0+2*e1+3*e2+5*e12+7) | (31*e0+11*e1+13*e2+17*e12+19)", .{})).val,
);
```

As you can see, it also uses the amazing [comath](https://github.com/InKryption/comath) library to overload operations.

In the case where you know the grade of the multivectors you are working with (which is most practical cases), zilliam generates types for like k-vector plus the even subalgebra and dispatches among them.
```zig
const Alg = Algebra(f32, 3, 0, 1);
const Blades = getBlades(Alg);
const Types = Blades.Types;

const Vector = Types[1];
const Bivector = Types[2];
const Trivector = Types[3];

pub fn main() !void {
    const BivectorBatch = Bivector.getBatchType(2);
    const VectorBatch = Vector.getBatchType(2);

    for (0..Bivector.Count) |a_i| {
        for (0..Vector.Count) |b_i| {
            var a = BivectorBatch{};
            var b = VectorBatch{};
            a.val[a_i] = .{ 1, 2 };
            b.val[b_i] = .{ 1, 2 };
            var buf: [2048]u8 = undefined;

            // This is a Trivector, it gets properly dispatched
            const res = a.wedge(b);

            for (0..2) |i| {
                const r_w = res.get(i);

                var r_s = try a.get(i).print(&buf);
                std.debug.print("{s} ^ ", .{r_s});
                r_s = try b.get(i).print(&buf);
                std.debug.print("{s} = ", .{r_s});
                r_s = try r_w.print(&buf);
                std.debug.print("{s}\n", .{r_s});
            }
            std.debug.print("\n", .{});
        }
    }
}

// ...
// 1.0000e13 ^ 1.0000e0 = 1.0000e013
// 2.0000e13 ^ 2.0000e0 = 4.0000e013
// ...
```

# Todo
- Improve utils to generate a type for dealing wtih each flavor of geometric algebra (so PGA would have functions to generate lines, planes, etc).
- Eventually, hook it up to my graphics library to start doing visualizations (maybe something on par with ganja.js)

# References
- Thanks to [klein](https://github.com/jeremyong/klein) specially, who gave me the idea to generate SIMD operations and something to compare the generated code to.
- Also thanks to the people at [biVector.net](https://bivector.net/index.html) and specially their [/tools](https://bivector.net/tools.html) page, whose tables and calculators helped me verifying the correctness of my own code.
