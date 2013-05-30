module Audio

open System
open OpenTK.Audio
open OpenTK.Audio.OpenAL
open System.Numerics
open Singer
open Freq
open Waves
open Util
//http://www.opentk.com/book/export/html/145
//http://upload.wikimedia.org/wikipedia/commons/a/a0/Equal_Temper_w_limits.svg
//http://www.phy.mtu.edu/~suits/scales.html
//http://www.phy.mtu.edu/~suits/notefreqs.html
//http://www.codeproject.com/Articles/62024/Using-OpenTK-OpenAL-to-Develop-Cross-Platform-DIS

/// tempo is in measures per minute
let tempo = 25.0

/// Returns an amplitude array to be made into a buffer
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

/// Converts an array of amplitudes and locations to a buffer
let bufferMake (input : (Ratio * Loc)[]) (init : float) =
    let mesLen = 60.0 / tempo
    let mutable output = null
    for i = 0 to input.Length - 1 do
        let dur = 
            if i <> input.Length - 1 then snd input.[i + 1] - snd input.[i]
            else 1 - snd input.[i] //think of a more elegant solution to this
        let freq = Sound.Note (init * ((fst input.[i]).ToFloat ()))
        let curWave =
            if i = input.Length - 1 then triangleWave ([| freq |], sample, dur.ToFloat ())
            else sinWave ([| freq |], sample, dur.ToFloat ())
        if output = null then output <- curWave
        else output <- waveConcat output curWave
    output

/// generates a song until its time is complete
let gen () =
    let context = new AudioContext (AudioContext.DefaultDevice)
    ///the base note of the song
    let initial = float (r.Next (300, 801)) + r.NextDouble ()
    printfn "%f" initial

    let singer = Singer ()

    let melody () = 
        let fucks = (singer.NextMeasure ()).[0] |> measureToArray
        for i in fucks do
            printfn "(%s * %s)" ((fst i).ToString ()) ((snd i).ToString ())
        printfn "break"
        bufferMake (fucks) initial

    let melBuffers = AL.GenBuffers (25)
    let melSource = AL.GenSource ()
    for i = 0 to melBuffers.Length - 1 do
        let mel = melody ()
        AL.BufferData (melBuffers.[i], OpenAL.ALFormat.Mono8, mel, mel.Length, sample)

    AL.SourceQueueBuffers (melSource, melBuffers.Length, melBuffers)

    AL.Source (melSource, ALSourcef.Gain, 1.0f)
    AL.SourcePlay melSource