module Util

/// sample rate in samples per second
let n = 44100
/// amplitude multiplier
let multiplier = 0.07
/// random number generator
let r = new System.Random ()

type Sound =
    /// frequency of note
    | Note of float
    | Rest