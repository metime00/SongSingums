module Singers
open Util
open SingerAbstract
open Waves

type BasicSinger (tem, err) =
    inherit Singer(tem, err)

    /// every time it counts to 4, play only the time signature
    let mutable iterMes = 4
    /// motifs are song snippets expressed as arrays of notes
    let motifs : NoteNode[] = Array.zeroCreate 5

    let key : Ratio[] = [| Ratio (4, 3); Ratio (5, 3); Ratio (2, 1); Ratio (1, 2); Ratio (6, 3)|]
    
    let timeSig = timeGen err
    
    /// the main voice of the song
    let melody = NoteNode (Ratio (1), Loc (0))

    let bass = NoteNode (Ratio (1, 2), Loc (0))
        
    /// the time signature
    override this.TimeSig = timeSig

    /// generates the list of notes this singer can use
    member this.KeyGen () =
        ///largest number allowed in the numerator or denominator, should be low
        let maxRatio = 3
        ///how many times greater a part of the ratio can be than the other part
        let maxQuo = Fraction (2, 1)
        for i = 1 to key.Length - 1 do
            let potenNote = this.RatioGen maxRatio maxQuo
            key.[i] <- potenNote

    /// picks a random note from the key
    member this.KeyNote () =
        key.[r.Next (0, key.Length - 1)]

    /// returns the initial children for a distinct voice's original NoteNode based on the time signature, and their frequency ratios
    override this.NodeInit time =
        ///largest number allowed in the numerator or denominator, should be low
        let maxRatio = 3
        ///how many times greater a part of the ratio can be than the other part
        let maxQuo = Fraction (2, 1)
        /// percent chance that a note will be a rest
        let restChance = 15
        [|
            for i in time do
                if r.Next 101 > 15 then
                    yield NoteNode (this.KeyNote (), i)
                else
                    yield NoteNode (Ratio (0), i)
        |]

    /// generates a subdivision of a melody, returning two or three NoteNodes
    override this.MelGen () =
        /// largest number allowed in the numerator or denominator, lower numbers create less dissonant intervals
        let maxRatio = 6
        /// how much larger a ratio can be compared to its parent ratio
        let maxQuo = Fraction (3, 2)
        /// whether or not the subdivision is a triplet, 25% chance to be a triplet
        let triple = r.Next 101 > 75
        /// percent chance that a note will be a rest
        let restChance = 10

        if triple then //three equal spaced children notes
            [|
                for i = 0 to 2 do
                    if r.Next 101 > restChance then
                        yield NoteNode (this.KeyNote (), Fraction (i, 3))
                    else
                        yield NoteNode (Ratio (0), Fraction (i, 3))
                        
            |]
        else //two unequally spaced notes
            /// numerator can only be up to one less this number, with denominator only up to two more
            [|
                ///the number that decides how large a number in the numerator or denominator can be for location
                let range = 4
                let numL = r.Next (1, range) //range is one less so the note location is never 1.0
                let denL = r.Next (int numL + 1, range + 2)
                if r.Next 101 > restChance then
                    yield NoteNode (this.KeyNote (), Loc (0))
                else
                    yield NoteNode (Ratio (0), Loc (0))
                if r.Next 101 > restChance then
                    yield NoteNode (this.KeyNote (), Loc (numL, denL))
                else
                    yield NoteNode (Ratio (0), Loc (numL, denL))
            |]
    
    
    /// basic singer adds new notes in to the first beat of the time signature
    override this.NextMeasure () =
        let pitch (inputs : NoteNode[]) change =
            [|
                for i in inputs do
                    yield new NoteNode (i.Ratio * change, i.Location)
            |]
        melody.RemoveChildren ()
        let melTemp = this.NodeInit this.TimeSig
        if iterMes < 3 then 
            for i in melTemp do
                let tempChils = this.MelGen ()
                i.AddChildren (tempChils)
            iterMes <- iterMes + 1
        else
            iterMes <- 0
        melody.AddChildren (melTemp)


        bass.RemoveChildren ()
        bass.AddChildren (pitch melTemp (Ratio (1,2)))
        //bass.AddChildren ([| new NoteNode (Ratio (1,2), Loc 0); new NoteNode (Ratio (3,4), Loc (1,3)); new NoteNode (Ratio (1), Loc (2,3)) |])

        [|
            yield melody
            yield bass
        |]

    /// Converts an array of amplitudes and locations to a buffer
    override this.BufferMake (input : (Ratio * Loc)[]) (init : float) =
        //makes the measure length by dividing seconds per minute by measures per minute
        let mesLen = 60.0 / this.Tempo
        let mutable output = null
        for i = 0 to input.Length - 1 do
            let dur = 
                let tmp =
                    if i <> input.Length - 1 then snd input.[i + 1] - snd input.[i]
                    else 1 - snd input.[i] //think of a more elegant solution to this 
                tmp.ToFloat () * mesLen
            let (freq : Note) = init * ((fst input.[i]).ToFloat ())
            let (overton : Note) = init * ((fst input.[i]).ToFloat () * 0.667)
            let curWave = sawWave ([| freq; overton;|], sample, dur)
            if output = null then output <- curWave
            else output <- waveConcat output curWave
        output
