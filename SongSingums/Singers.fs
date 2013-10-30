module Singers
open Util
open SingerAbstract
open Waves

type BasicSinger (tem, err) =
    inherit Singer(tem, err)

    /// every time it counts to 4, play only the time signature
    let mutable iterMes = 4
    /// motifs are song snippets expressed as arrays of notes
    let mutable motifs : NoteNode list = List.empty
    
    let timeSig = timeGen err
    
    /// the main voice of the song
    let melody = NoteNode (Ratio (1), Loc (0))
        
    /// the time signature
    override this.TimeSig = timeSig

    /// returns the initial children for a distinct voice's original NoteNode based on the time signature, and their frequency ratios
    override this.NodeInit time =
        ///largest number allowed in the numerator or denominator, should be low
        let maxRatio = 3
        ///how many times greater a part of the ratio can be than the other part
        let maxQuo = Fraction (2, 1)
        [|
            for i in time do
                yield NoteNode (this.RatioGen maxRatio maxQuo, i)
        |]

    /// generates a subdivision of a melody, returning two or three NoteNodes
    override this.MelGen () =
        /// largest number allowed in the numerator or denominator, lower numbers create less dissonant intervals
        let maxRatio = 6
        /// how much larger a ratio can be compared to its parent ratio
        let maxQuo = Fraction (3, 2)
        /// whether or not the subdivision is a triplet, 25% chance to be a triplet
        let triple = r.Next 101 > 101

        if triple then //three equal spaced children notes
            [|
                for i = 0 to 2 do
                    yield NoteNode (this.RatioGen maxRatio maxQuo, Fraction (i, 3))
            |]
        else //two unequally spaced notes
            /// numerator can only be up to one less this number, with denominator only up to two more
            [|
                ///the number that decides how large a number in the numerator or denominator can be for location
                let range = 4
                let numL = r.Next (1, range) //range is one less so the note location is never 1.0
                let denL = r.Next (int numL + 1, range + 2)
                yield NoteNode (this.RatioGen maxRatio maxQuo, Loc (0))
                yield NoteNode (this.RatioGen maxRatio maxQuo, Loc (numL, denL))
            |]
    
    
    /// basic singer adds new notes in to the first beat of the time signature
    override this.NextMeasure () =
        melody.RemoveChildren ()
        let melTemp = this.NodeInit this.TimeSig
        if iterMes < 3 then 
            for i in melTemp do
                let tempChils = this.MelGen ()
                i.AddChildren (tempChils)
            melTemp.[0].Children.[0].AddChildren (this.MelGen ())
            iterMes <- iterMes + 1
        else
            iterMes <- 0
        melody.AddChildren (melTemp)

        [|
            yield melody
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
            let freq = Sound.Note (init * ((fst input.[i]).ToFloat ()))
            let curWave = squareWave ([| freq;|], sample, dur)
            if output = null then output <- curWave
            else output <- waveConcat output curWave
        output
