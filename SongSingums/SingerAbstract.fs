module SingerAbstract
open Util
        

/// ratio of note to base note represented as a fraction
type Ratio = Fraction
/// location of a note's subdivision by fraction of the parent node's time
type Loc = Fraction

/// node in a tree of notes or beats
type NoteNode (r : Ratio, l : Loc) =
    let mutable ratio : Ratio = r
    let loc : Loc = l
    let mutable children : NoteNode[] = Array.empty

    member this.Ratio = ratio

    member this.Location = loc

    /// returns a list of the notes children
    member this.Children = children

    member this.AddChildren (input : NoteNode[]) =
        children <- input

    member this.RemoveChildren () =
        children <- Array.empty

    member this.SetRatio input =
        ratio <- input

/// Takes a NoteNode and returns the corresponding array of note ratios and locations to be made into a buffer

let measureToArray (input : NoteNode) = 
    let rec listBuild (chils : NoteNode[]) (recRat : Ratio) (left : Loc) (right : Loc) =
        [|
            if Array.isEmpty chils then
                yield (recRat, left)
            else
                for i = 0 to chils.Length - 1 do
                    let newLeft = (right - left) * chils.[i].Location + left
                    let newRight = if i < chils.Length - 1 then (right - left) * chils.[i + 1].Location + left else right
                    yield! listBuild chils.[i].Children (chils.[i].Ratio) newLeft newRight //ratios are absolute
        |]
    listBuild input.Children input.Ratio input.Location (Fraction 1)

/// default time signature generator
let timeGen (err : Fraction) =
    /// creates a list of the locations of beats
    let rec gen (left : Loc) (right : Loc) level =
        [
            /// if any subdivision is smaller than this size, it will not subdivide further
            let interval = Fraction (14, 100)
            /// middle shifted so that left is zero
            let mid = (left + right) * (Fraction (1, 2)) - left
            /// displacement of the middle
            let disp = (Fraction (r.Next (mid.Num * 2), mid.Den) - mid) * err
            /// the shifted true middle
            let newMid = mid + left + disp
            // booleans to check if the left or right midpoints will go, cutoff should be 4.0
            if level < 4.0 then
                if r.NextDouble () < (err.ToFloat ()) ** (level / 2.0) && right - left > interval then yield! gen left newMid (level + 1.0)
                if r.NextDouble () < (err.ToFloat ()) ** (level / 2.0) then yield newMid
                if r.NextDouble () < (err.ToFloat ()) ** (level / 2.0) && right - left > interval then yield! gen newMid right (level + 1.0)
                        
        ]
    Loc 0 :: gen (Loc 0) (Loc 1) 1.0

[<AbstractClass>]
type Singer (tem : float, err : Fraction) =

    /// Erraticity, higher is more erratic, 0 < H < 1 must be true
    let h = err

    /// tempo is in measures per minute
    let tempo = tem
    
    /// Erraticity, higher is more erratic, 0 < H < 1 must be true
    member this.H with get () = h

    /// tempo is in measures per minute
    member this.Tempo with get () = tem

    ///creates a ratio from a maximum number that can be used either in the numerator or denominator and a maximum total value of the fraction
    abstract member RatioGen: int -> Fraction -> Ratio
    
    //the random ratio function is basic enough where most singers won't be messing with it
    ///creates a ratio from a maximum number that can be used either in the numerator or denominator and a maximum total value of the fraction
    default this.RatioGen maxRatio (maxQuo : Fraction) =
        let num = r.Next (1, maxRatio + 1)
        let den = r.Next (max 1 ((int) (ceil ((num / maxQuo).ToFloat ()))), min (maxRatio + 1) ((int) ((num * maxQuo).ToFloat ())))
        Ratio (num, den)

    /// the time signature of the singer, can be generated or defined by the programmer
    abstract member TimeSig: Loc list

    /// returns the initial children for a distinct voice's original NoteNode based on the time signature, and their frequency ratios
    abstract member NodeInit: (Loc list) -> NoteNode[]
    //NodeInit functions as a less ambitious and much simpler version of MelGen

    /// generates a subdivision of a melody, returning two or three NoteNodes
    abstract member MelGen: unit -> NoteNode[]

    /// returns the next measure in the song as an array of the distinct NoteNode voices
    abstract member NextMeasure: unit -> NoteNode[]

    /// Converts an array of amplitudes and locations to a buffer, applying different waveforms and any filters to the sound
    abstract member BufferMake: (Ratio * Loc)[] -> (float) -> uint8[]