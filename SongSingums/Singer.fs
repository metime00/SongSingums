module Singer
open Util

/// ratio of note to base note represented as a float
type Ratio = float
/// length of a note in subdivisions by fraction of the parent node's time
type Length = float

/// node in a tree of notes or beats
type NoteNode (parent : NoteNode) =
    let mutable ratio : Ratio = 0.0
    let mutable length : Length = 0.0
    let mutable children : NoteNode list = List.empty

    member this.Ratio = 
        match children with
        | some -> 
        | List.empty -> None
    member this.Length = length

    /// returns a list of the notes children
    member this.Children = children

    member this.AddChildren (input : NoteNode list) =
        ()

    member this.RemoveChildren () =
        children <- List.empty

type Singer () =
    /// time signature, expressed as subdivisions of notes
    let mutable time = null
    /// motifs are song snippets expressed as arrays of notes
    let mutable motifs = null

    member this.NextMeasure () =
        ()