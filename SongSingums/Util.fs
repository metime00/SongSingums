module Util
open System


/// highest common factor
let rec hcf a b =
  if a = 0 then b
  elif a < b then hcf a (b - a)
  else hcf (a - b) b

type Fraction (n : int, d : int) =
    let mutable num = 0
    let mutable den = 0

    do
        let factor = hcf n d
        num <- n / factor
        den <- d / factor

    new (n : int) =
        Fraction (n, 1)
    member this.Num
        with get () = num
        and set (value) = num <- value

    member this.Den
        with get () = den
        and set (value) = den <- value

    member this.ToFloat () =
        (float num / float den)

    static member (+) (f1 : Fraction, f2 : Fraction) =
        let num = f1.Num * f2.Den + f2.Num * f1.Den
        let den = f1.Den * f2.Den
        let factor = hcf num den
        Fraction (num / factor, den / factor)
    
    static member (+) (f1 : Fraction, i : int) =
        let num = f1.Num + i * f1.Den
        let den = f1.Den
        let factor = hcf num den
        Fraction (num / factor, den / factor)

    static member (+) (i : int, f1 : Fraction) =
        let num = f1.Num + i * f1.Den
        let den = f1.Den
        let factor = hcf num den
        Fraction (num / factor, den / factor)

    static member (-) (f1 : Fraction, f2 : Fraction) =
        let num = f1.Num * f2.Den - f2.Num * f1.Den
        let den = f1.Den * f2.Den
        let factor = hcf num den
        Fraction (num / factor, den / factor)

    static member (-) (f1 : Fraction, i : int) =
        let num = f1.Num - i * f1.Den
        let den = f1.Den
        let factor = hcf num den
        Fraction (num / factor, den / factor)

    static member (-) (i : int, f1 : Fraction) =
        let num = i * f1.Den - f1.Num
        let den = f1.Den
        let factor = hcf num den
        Fraction (num / factor, den / factor)

    static member (*) (f1 : Fraction, f2 : Fraction) =
        let num = f1.Num * f2.Num
        let den = f2.Den * f1.Den
        let factor = hcf num den
        Fraction (num / factor, den / factor)

    static member (*) (f1 : Fraction, i) =
        let num = f1.Num * i
        let den = f1.Den
        let factor = hcf num den
        Fraction (num / factor, den / factor)

    static member (*) (i, f1 : Fraction) =
        let num = f1.Num * i
        let den = f1.Den
        let factor = hcf num den
        Fraction (num / factor, den / factor)

    static member (/) (f1 : Fraction, f2 : Fraction) =
        let num = f1.Num * f2.Den
        let den = f2.Num * f1.Den
        let factor = hcf num den
        Fraction (num / factor, den / factor)

    static member (/) (f1 : Fraction, i) =
        let num = f1.Num
        let den = i * f1.Den
        let factor = hcf num den
        Fraction (num / factor, den / factor)

    static member (/) (i, f1 : Fraction) =
        let num = i * f1.Den
        let den = f1.Num
        let factor = hcf num den
        Fraction (num / factor, den / factor)

    interface IComparable with
/// sample rate in samples per second
let n = 44100
/// amplitude multiplier
let multiplier = 0.07
/// random number generator
let r = new System.Random ()
/// Singer level of precision, powers of ten, with every power being one decimal point of precision
let prec = int (10.0 ** 2.0)
/// Erraticity, higher is more erratic, 0 < H < 1 must be true
let H = Fraction (75, 100)

type Sound =
    /// frequency of note
    | Note of float
    | Rest