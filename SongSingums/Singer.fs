module Singer
open Util

/// ratio of note to base note represented as a float
type Ratio = float
/// location of a note's subdivision by fraction of the parent node's time
type Loc = float

/// node in a tree of notes or beats
type NoteNode (r : Ratio, l : Loc) =
    let mutable ratio : Ratio = r
    let loc : Loc = l
    let mutable children : NoteNode list = List.empty

    member this.Ratio = 
        match children with
        | some -> Some ratio
        | children when List.isEmpty children -> None

    member this.Location = loc

    /// returns a list of the notes children
    member this.Children = children

    member this.AddChildren (input : NoteNode list) =
        children <- input

    member this.RemoveChildren () =
        children <- List.empty

    member this.SetRatio input =
        ratio <- input

type Singer () =

    let ratioGen maxRatio maxQuo =
        let num = float (r.Next (1, maxRatio + 1))
        let den = float (r.Next (max 1 (int (num / maxQuo)), min (maxRatio + 1) (int (num * maxQuo))))
        num / den
        
    /// generates a time signature
    let timeGen () =
        /// creates a list of the locations of beats
        let rec gen left right level =
            [
                /// if any subdivision is smaller than this size, it will not subdivide further
                let interval = 0.05
                /// middle shifted so that left is zero
                let mid = (left + right) * 0.5 - left
                /// displacement of the middle
                let disp = ((float (r.Next (int (mid * 2.0 * prec)))) / prec - mid) * H
                /// the shifted true middle
                let newMid = mid + left + disp
                // booleans to check if the left or right midpoints will go
                if level < 4.0 then
                    if r.NextDouble () < (H) ** (level / 2.0) && right - left > interval then yield! gen left newMid (level + 1.0)
                    if r.NextDouble () < (H) ** (level / 2.0) then yield newMid
                    if r.NextDouble () < (H) ** (level / 2.0) && right - left > interval then yield! gen newMid right (level + 1.0)

            ]
        gen 0.0 1.0 1.0

    /// time signature, expressed as a list of note subdivisions
    let time : float list = timeGen ()
    /// motifs are song snippets expressed as arrays of notes
    let mutable motifs : NoteNode list = List.empty

    /// returns the initial children for a NoteNode based on the time signature
    let nodeInit time =
        ///largest number allowed in the numerator or denominator
        let maxRatio = 6
        ///how many times greater a part of the ratio can be than the other part
        let maxQuo = 2.0
        [
            for i in time do
                yield NoteNode (ratioGen maxRatio maxQuo, i)
        ]

    /// generates a subdivision of a melody, returning two or three NoteNodes
    let melGen () =
        /// largest number allowed in the numerator or denominator
        let maxRatio = 6
        /// how many times greater a part of the ratio can be than the other part
        let maxQuo = 1.5
        /// whether or not the subdivision is a triplet
        let triple = r.Next 101 > 75

        if triple then //three equal spaced children notes
            [
                for i = 0 to 2 do
                    yield NoteNode (ratioGen maxRatio maxQuo, (float) i / 3.0)
            ]
        else //two unequally spaced notes
            /// numerator can only be up to one less this number, with denominator only up to two more
            [
                let range = 6
                let numL = float (r.Next (1, range)) //range is one less so the note location is never 1.0
                let denL = float (r.Next (int numL + 1, range + 2))
                yield NoteNode (ratioGen maxRatio maxQuo, 0.0)
                yield NoteNode (ratioGen maxRatio maxQuo, numL / denL)
            ]

    let melody = NoteNode (1.0, 0.0)
    let rhythm = NoteNode (0.5, 0.0)

    member this.NextMeasure () =
        melody.RemoveChildren ()
        rhythm.RemoveChildren ()
        melody.AddChildren (nodeInit time)
        rhythm.AddChildren (nodeInit time)

        [
            yield melody
            yield rhythm
        ]