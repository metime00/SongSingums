module Audio

open System
open OpenTK.Audio
open OpenTK.Audio.OpenAL
open System.Numerics
open SingerAbstract
open Singers
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
    let H = Fraction (60, 100)
    let tempo = 20.0

    let singer = BasicSinger (tempo, H)

    let melody () = 
        let fucks = (singer.NextMeasure ()).[0] |> measureToArray
        for i in fucks do
            printfn "melody: (%s * %s)" ((fst i).ToString ()) ((snd i).ToString ())
        printfn "break"
        singer.BufferMake (fucks) initial

    let melBuffers = AL.GenBuffers (20)
    let melSource = AL.GenSource ()
    for i = 0 to melBuffers.Length - 1 do
        let mel = melody ()
        AL.BufferData (melBuffers.[i], OpenAL.ALFormat.Mono8, mel, mel.Length, sample)

    AL.SourceQueueBuffers (melSource, melBuffers.Length, melBuffers)

    AL.Source (melSource, ALSourcef.Gain, 1.0f)
    AL.SourcePlay melSource