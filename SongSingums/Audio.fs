﻿module Audio

open System
open OpenTK.Audio
open OpenTK.Audio.OpenAL
open System.Numerics
open Freq
open Waves
//http://www.opentk.com/book/export/html/145
//http://upload.wikimedia.org/wikipedia/commons/a/a0/Equal_Temper_w_limits.svg
//http://www.phy.mtu.edu/~suits/scales.html
//http://www.phy.mtu.edu/~suits/notefreqs.html
//http://www.codeproject.com/Articles/62024/Using-OpenTK-OpenAL-to-Develop-Cross-Platform-DIS
let n = 44100
let tempo = 100.0
let r = new System.Random ()

let play (sounds : uint8 [][]) =
    let context = new AudioContext (AudioContext.DefaultDevice)

    let roo = AL.GenBuffers (sounds.Length)
    let srooce = AL.GenSources (sounds.Length)
    for i = 0 to sounds.Length - 1 do
        AL.BufferData (roo.[i], ALFormat.Mono8, sounds.[i], sounds.[i].Length, n)
        AL.Source (srooce.[i], ALSourcei.Buffer, roo.[i])
        AL.Source (srooce.[i], ALSourcef.Gain, 0.25f)
        AL.SourcePlay srooce.[i]

/// gives a note length between a whole note to a sixteenth note
let randomLength () = (2.0 ** float (r.Next(0, 5)))

let randomRatio input initial =
    let mutable tmp = input * (float (r.Next(1, 5)) / float (r.Next(1, 5)))
    if tmp > 1500.0 then tmp <- initial
    elif tmp < 50.0 then tmp <- initial / 2.0
    tmp

///generates a song until its time is complete
let gen (time : float) =
    let context = new AudioContext (AudioContext.DefaultDevice)
    ///the base note of the song
    let initial = float (r.Next (300, 801)) + r.NextDouble ()
    let mutable current = initial
    printf "%f" initial

    let buffers = AL.GenBuffers (500)
    for i = 0 to buffers.Length - 1 do
        let length = randomLength ()
        current <- randomRatio current initial
        let mutable tmp = null
        if length < 8.0 then tmp <- sinWave ([| current; current * 0.5; current * 1.5 |], n, 60.0 / tempo / length)
        else tmp <- squareWave ([| current |], n, 60.0 / tempo / length)
        AL.BufferData (buffers.[i], ALFormat.Mono8, tmp, tmp.Length, n)
    let source = AL.GenSource ()
    AL.SourceQueueBuffers (source, buffers.Length, buffers)
    
    let startTime = System.DateTime.Now

    AL.Source (source, ALSourcef.Gain, 0.25f)
    AL.SourcePlay source