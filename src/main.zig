const std = @import("std");

const geo = @import("geo.zig");
const Algebra = geo.Algebra;
const getBlades = @import("blades.zig").Blades;

const Alg = Algebra(f32, 3, 0, 1);
const Blades = getBlades(Alg, .{});
const Types = Blades.Types;

const Vector = Types[1];
const Bivector = Types[2];
const Trivector = Types[3];

pub fn main() !void {}

// ...
// 1.0000e13 ^ 1.0000e0 = 1.0000e013
// 2.0000e13 ^ 2.0000e0 = 4.0000e013
// ...
