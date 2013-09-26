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

/// Returns an array of note ratios and locations to be made into a buffer
let measureToArray (input : NoteNode) =
    let rec listBuild (chils : NoteNode[]) (recRat : Ratio) (left : Loc) (right : Loc) =
        [|
            if Array.isEmpty chils then
                yield (recRat, left)
            else
                for i = 0 to chils.Length - 1 do
                    let newLeft = (right - left) * chils.[i].Location
                    let newRight = if i < chils.Length - 1 then chils.[i + 1].Location else right
                    yield! listBuild chils.[i].Children (chils.[i].Ratio * recRat) newLeft newRight
        |]
    listBuild input.Children input.Ratio input.Location (Fraction 1)

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
    default this.RatioGen maxRatio (maxQuo : Fraction) =
        let num = r.Next (1, maxRatio + 1)
        let den = r.Next (max 1 ((int) (ceil ((num / maxQuo).ToFloat ()))), min (maxRatio + 1) ((int) ((num * maxQuo).ToFloat ())))
        Ratio (num, den)
        
    /// generates a time signature
    abstract member TimeGen: unit -> Loc list

    /// time signature, expressed as a list of note subdivisions
    abstract TimeSig : Loc list with get
    
    //most time signatures will be just be a single run of the TimeGen function
    default this.TimeSig : (Loc list) = this.TimeGen ()

    /// returns the initial children for a NoteNode based on the time signature, and their frequency ratios
    abstract member NodeInit: (Loc list) -> NoteNode[]

    /// generates a subdivision of a melody, returning two or three NoteNodes
    abstract member MelGen: unit -> NoteNode list

    /// returns the next measure in the song for conversion to a buffer
    abstract member NextMeasure: unit -> NoteNode[]

    /// Converts an array of amplitudes and locations to a buffer
    abstract member BufferMake: (Ratio * Loc)[] -> (float) -> uint8[]