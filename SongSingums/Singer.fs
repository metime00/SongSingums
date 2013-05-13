module Singer
open Util

/// ratio of note to base note represented as a float
type Ratio = float
/// location of a note's subdivision by fraction of the parent node's time
type Loc = float

/// node in a tree of notes or beats
type NoteNode (r : Ratio, l : Loc) =
    let mutable ratio : Ratio = r
    let mutable loc : Loc = l
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

type Singer () =
    let timeGen () =
        /// creates a list of the locations of beats
        let gen left right level =
            [
                /// true middle
                let mid = left + right / 2.0
                /// displaced middle
                let disp = (float (r.Next (int (left * H * prec), int (right * H * prec))) / prec)
                // booleans to check if the left or right midpoints will go
                let lB = r.NextDouble () < H
                let rB = r.NextDouble () < H

            ]
        gen 0.0 1.0 0

    /// time signature, expressed as a list of note subdivisions
    let mutable time : NoteNode list = List.empty
    /// motifs are song snippets expressed as arrays of notes
    let mutable motifs : NoteNode list = List.empty
    let melody = NoteNode (1.0, 1.0)
    let rhythm = NoteNode (0.5, 1.0)

    member this.NextMeasure () =
        ()