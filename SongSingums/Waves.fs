﻿module Waves

let multiplier = 0.07

let waveComp (f : float [], n : int, duration : float) =    
    let buffer = Array.zeroCreate<uint8> (int (float n * duration))
    let tones = f.Length - 1
    for i = 0 to buffer.Length - 1 do
        let mutable value = 0.0
        for j = 0 to tones do
            let period = float n / f.[j]
            value <- value + sin ((2.0 * 3.1415926 / period) * float i) * multiplier
            value <- value |> min 1.0 |> max -1.0
        buffer.[i] <- uint8 (value * 126.0 + 126.5)
    buffer

let sinWave (f : float [], n : int, duration : float) =    
    let buffer = Array.zeroCreate<uint8> (int (float n * duration))
    let tones = f.Length - 1
    for i = 0 to buffer.Length - 1 do
        let mutable value = 0.0
        for j = 0 to tones do
            let period = float n / f.[j]
            value <- value + sin ((2.0 * 3.1415926 / period) * float i) * multiplier
            value <- value |> min 1.0 |> max -1.0
        buffer.[i] <- uint8 (value * 126.0 + 126.5)
    buffer

let squareWave (f : float [], n : int, duration : float) =    
    let buffer = Array.zeroCreate<uint8> (int (float n * duration))
    let tones = f.Length - 1
    for i = 0 to buffer.Length - 1 do
        let mutable value = 0.0
        for j = 0 to tones do
            let period = float n / f.[j]
            let square =
                let tmp = sin ((2.0 * 3.1415926 / period) * float i)
                if tmp > 0.0 then 1.0 else -1.0
            value <- value + (square) * multiplier * 0.5 //half volume multiplier because squares are loud
            value <- value |> min 1.0 |> max -1.0
        buffer.[i] <- uint8 (value * 126.0 + 126.5)
    buffer

let sawWave (f : float [], n : int, duration : float) =    
    let buffer = Array.zeroCreate<uint8> (int (float n * duration))
    let tones = f.Length - 1
    for i = 0 to buffer.Length - 1 do
        let mutable value = 0.0
        for j = 0 to tones do
            let period = float n / f.[j]
            let tmp = (2.0 * (float i / period - floor (0.5 + float i / period)) * multiplier)
            value <- value + tmp
            value <- value |> min 1.0 |> max -1.0
        buffer.[i] <- uint8 (value * 126.0 + 126.5)
    buffer

let triangleWave (f : float [], n : int, duration : float) =    
    let buffer = Array.zeroCreate<uint8> (int (float n * duration))
    let tones = f.Length - 1
    for i = 0 to buffer.Length - 1 do
        let mutable value = 0.0
        for j = 0 to tones do
            let period = float n / f.[j]
            let tmp = 2.0 * abs (2.0 * (float i / period - floor (0.5 + float i / period))) - 1.0
            value <- value + tmp * multiplier
            value <- value |> min 1.0 |> max -1.0
        buffer.[i] <- uint8 (value * 126.0 + 126.5)
    buffer

/// adds two notes together, so that they can be used in one buffer.
let waveConcat (first : uint8[], second : uint8[]) =
    [|
        yield! first
        yield! second
    |]