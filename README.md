# zilliam

Zilliam is a Geometric Algebra library that generates a SIMD optimized library for any given Cl(p,q,r) algebra. It can be used for:

* PGA (projective geometric algebra, for which Zilliam has a special for generating all needed mask types) where you can easily construct points, lines and planes in any dimension and do projection, rotations, intersections and more between all of them under the same syntax. It has many uses for computer graphics and physics. It can also model conformal transformations using CGA.
* Modelling complex numbers, quaternions, dual numbers and many more other Clifford algebras under a single interface, generating SIMD optimized code for all of them. This makes it possible to use Zilliam for automatic differentiation, complex analysis
* Calculate Lorentz boosts and more using an STA (spacetime algebra) which also provides a very useful model for doing Electrodynamics work.

For all these applications, Zilliam gives you access to useful functions such as the trigonometric functions, the exponential and also useful concepts such as the Poincare dual and wedge operations. It also provides a sort of operation overloading through a [comath](https://github.com/InKryption/comath) interface, which allows you to evaluate complex expressions more easily and without much compile time cost.

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

It generates SIMD operations for any generic Clifford algebra for all these operations, and for any generic type as well:

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

For more examples and a guide for using Zilliam on your project, check the [wiki](https://github.com/ymndoseijin/zilliam/wiki/Examples).

# Todo
- Add more general utilities beyond just the PGA one and improve the calculation of functions
- Eventually, hook it up to my graphics library to start doing visualizations (maybe something on par with ganja.js)

# References
- Thanks to [klein](https://github.com/jeremyong/klein) specially, who gave me the idea to generate SIMD operations and something to compare the generated code to.
- Also thanks to the people at [biVector.net](https://bivector.net/index.html) and specially their [/tools](https://bivector.net/tools.html) page, whose tables and calculators helped me verifying the correctness of my own code.
