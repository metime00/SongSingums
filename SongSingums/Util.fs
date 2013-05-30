module Util
open System


/// highest common factor
let rec hcf aS bS =
  let a = abs aS
  let b = abs bS
  if a = 0 then b
  elif a < b then hcf a (b - a)
  else hcf (a - b) b

type Fraction (n : int, d : int) =
    let mutable num = n
    let mutable den = d

    do
        let factor = hcf num den
        num <- num / factor
        den <- den / factor

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

    override this.ToString () =
        num.ToString () + "/" + den.ToString ()

    override this.Equals value =
        match value with
        | :? Fraction as f2 -> num * f2.Den = f2.Num * den
        | _ -> false

    override this.GetHashCode () = hash (num / den)

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
        member this.CompareTo value =
            match value with
            | :? Fraction as f2 -> 
                num * f2.Den - f2.Num * den
            | _ -> invalidArg "can't compare two objects of a different type" "can't do it"
/// sample rate in samples per second
let sample = 44100
/// amplitude multiplier
let multiplier = 0.14
/// random number generator
let r = new System.Random ()
/// Erraticity, higher is more erratic, 0 < H < 1 must be true
let H = Fraction (75, 100)

type Sound =
    /// frequency of note
    | Note of float
    | Rest