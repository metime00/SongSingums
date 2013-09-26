module Audio

open System
open OpenTK.Audio
open OpenTK.Audio.OpenAL
open System.Numerics
open Freq
open Waves
open Util
//http://www.opentk.com/book/export/html/145
//http://upload.wikimedia.org/wikipedia/commons/a/a0/Equal_Temper_w_limits.svg
//http://www.phy.mtu.edu/~suits/scales.html
//http://www.phy.mtu.edu/~suits/notefreqs.html
//http://www.codeproject.com/Articles/62024/Using-OpenTK-OpenAL-to-Develop-Cross-Platform-DIS


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
            printfn "melody: (%s * %s)" ((fst i).ToString ()) ((snd i).ToString ())
        printfn "break"
        bufferMake (fucks) initial

    let bass () = 
        let fucks = (singer.NextMeasure ()).[1] |> measureToArray
        for i in fucks do
            printfn "bass: (%s * %s)" ((fst i).ToString ()) ((snd i).ToString ())
        printfn "break"
        bufferMake (fucks) initial

    let melBuffers = AL.GenBuffers (25)
    let melSource = AL.GenSource ()
    for i = 0 to melBuffers.Length - 1 do
        let mel = waveAdd [| melody (); Array.map (fun x -> x / (uint8 2)) (bass ()); |]
        AL.BufferData (melBuffers.[i], OpenAL.ALFormat.Mono8, mel, mel.Length, sample)

    AL.SourceQueueBuffers (melSource, melBuffers.Length, melBuffers)

    AL.Source (melSource, ALSourcef.Gain, 1.0f)
    AL.SourcePlay melSource