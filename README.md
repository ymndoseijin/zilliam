# zilliam

This is a work in progress, but currently generates any arbitrary geometric algebra of any positive, negative and null dimension in compile time.

It currently has the following operations:
- a*b: geo
- ~a: reverse
- a^b: wedge
- a&b: regressive
- *a: dual
- a|b: inner product
- #a: grade involution
- a$k: grade projection
- %a: undual

In the future,  I will try to find a way to generate SIMD code for the operations instead of doing it manually (like in [klein](https://github.com/jeremyong/klein)).

```zig
const Alg = Algebra(i32, 2, 1, 1);

try std.testing.expectEqualSlices(
    i32,
    &(try Alg.evalBasis("(14*e1) - (21*e2) - (7*e12) - 24")).val,
    &(try Alg.evalBasis("(2*e1+3*e2+5*e12) * (11*e1+13*e2+17*e12)")).val,
);

try std.testing.expectEqualSlices(
    i32,
    &(try Alg.evalBasis("109 + 654*e0 + 129*e1 + 127*e2 + 214*e12")).val,
    &(try Alg.evalBasis("(23*e0+2*e1+3*e2+5*e12+7) | (31*e0+11*e1+13*e2+17*e12+19)")).val,
);
```
