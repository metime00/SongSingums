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
let tempo = 15.0

/// Returns an amplitude array to be made into a buffer
let measureToArray (input : NoteNode) =
    

/// generates a song until its time is complete
let gen () =
    let context = new AudioContext (AudioContext.DefaultDevice)
    ///the base note of the song
    let initial = float (r.Next (300, 801)) + r.NextDouble ()
    printf "%f" initial

    let singer = Singer ()

    let melBuffers = AL.GenBuffers (5)
    let melSource = AL.GenSource ()
    AL.SourceQueueBuffers (melSource, buffers.Length, buffers)
    
    let startTime = System.DateTime.Now

    AL.Source (melSource, ALSourcef.Gain, 0.25f)
    AL.SourcePlay melSource