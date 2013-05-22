module Util

/// sample rate in samples per second
let n = 44100
/// amplitude multiplier
let multiplier = 0.07
/// random number generator
let r = new System.Random ()
/// Singer level of precision, powers of ten, with every power being one decimal point of precision
let prec = 10.0 ** 2.0
/// Erraticity, higher is more erratic, 0.0 < H < 1.0 must be true
let H = 0.75

type Sound =
    /// frequency of note
    | Note of float
    | Rest