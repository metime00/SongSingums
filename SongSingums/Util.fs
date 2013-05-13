module Util

/// sample rate in samples per second
let n = 44100
/// amplitude multiplier
let multiplier = 0.07
/// random number generator
let r = new System.Random ()
/// Singer level of precision
let prec = 10.0
/// Erraticity, lower is less erratic
let H = 0.5

type Sound =
    /// frequency of note
    | Note of float
    | Rest