module Singer
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

type Singer () =

    /// Erraticity, higher is more erratic, 0 < H < 1 must be true
    let H = Fraction (75, 100)

    ///creates a ratio from a maximum number that can be used either in the numerator or denominator and a maximum total value of the fraction
    let ratioGen maxRatio (maxQuo : Fraction) =
        let num = r.Next (1, maxRatio + 1)
        let den = r.Next (max 1 ((int) (ceil ((num / maxQuo).ToFloat ()))), min (maxRatio + 1) ((int) ((num * maxQuo).ToFloat ())))
        Fraction (num, den)
        
    /// generates a time signature
    let timeGen () =
        /// creates a list of the locations of beats
        let rec gen (left : Fraction) (right : Fraction) level =
            [
                /// doesn't let a middle displace this close to another point
                //let spacer = Fraction (9, 100) //unimplemented

                /// if any subdivision is smaller than this size, it will not subdivide further
                let interval = Fraction (7, 100)
                /// middle shifted so that left is zero
                let mid = (left + right) * (Fraction (1, 2)) - left
                /// displacement of the middle
                let disp = (Fraction (r.Next (mid.Num * 2), mid.Den) - mid) * H
                /// the shifted true middle
                let newMid = mid + left + disp
                // booleans to check if the left or right midpoints will go, cutoff should be 4.0
                if level < 4.0 then
                    if r.NextDouble () < (H.ToFloat ()) ** (level / 2.0) && right - left > interval then yield! gen left newMid (level + 1.0)
                    if r.NextDouble () < (H.ToFloat ()) ** (level / 2.0) then yield newMid
                    if r.NextDouble () < (H.ToFloat ()) ** (level / 2.0) && right - left > interval then yield! gen newMid right (level + 1.0)
//                    elif r.NextDouble () < (H.ToFloat ()) ** (level / 2.0) && right - left > interval then //three evenly spaced triplet notes
//                        let shiftR = right - left
//                        for i = 1 to 2 do
//                            yield (i * shiftR / 3) + left
//                    elif r.NextDouble () < (H.ToFloat ()) ** (level / 2.0) then yield newMid //triplets creates a situation where the time signature can become very empty
                        
            ]
        Fraction 0 :: gen (Fraction 0) (Fraction 1) 1.0

    /// time signature, expressed as a list of note subdivisions
    let timeSig : Fraction list = timeGen ()
    /// motifs are song snippets expressed as arrays of notes
    let mutable motifs : NoteNode list = List.empty

    /// returns the initial children for a NoteNode based on the time signature, and their frequency ratios
    let nodeInit time =
        ///largest number allowed in the numerator or denominator, should be 6
        let maxRatio = 6
        ///how many times greater a part of the ratio can be than the other part
        let maxQuo = Fraction (2, 1)
        [|
            for i in time do
                yield NoteNode (ratioGen maxRatio maxQuo, i)
        |]

    /// generates a subdivision of a melody, returning two or three NoteNodes
    let melGen () =
        /// largest number allowed in the numerator or denominator
        let maxRatio = 6
        /// how many times greater a part of the ratio can be than the other part
        let maxQuo = Fraction (3, 2)
        /// whether or not the subdivision is a triplet
        let triple = r.Next 101 > 75

        if triple then //three equal spaced children notes
            [
                for i = 0 to 2 do
                    yield NoteNode (ratioGen maxRatio maxQuo, Fraction (i, 3))
            ]
        else //two unequally spaced notes
            /// numerator can only be up to one less this number, with denominator only up to two more
            [
                ///the number that decides how large a number in the numerator or denominator can be
                let range = 6
                let numL = r.Next (1, range) //range is one less so the note location is never 1.0
                let denL = r.Next (int numL + 1, range + 2)
                yield NoteNode (ratioGen maxRatio maxQuo, Fraction (0))
                yield NoteNode (ratioGen maxRatio maxQuo, Fraction (numL, denL))
            ]
    //the main voice of the song
    let melody = NoteNode (Fraction (1), Fraction (0))
    //rhythm exists two octaves down from the melody
    let rhythm = NoteNode (Fraction (1, 2), Fraction (0))

    member this.NextMeasure () =
        melody.RemoveChildren ()
        rhythm.RemoveChildren ()
        melody.AddChildren (nodeInit timeSig)
        rhythm.AddChildren (nodeInit timeSig)

        [|
            yield melody
            yield rhythm
        |]