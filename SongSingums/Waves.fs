module Waves
open Util


let sinWave (f : Note [], n : int, duration : float) =    
    let buffer = Array.zeroCreate<uint8> (int (float n * duration))
    for i = 0 to buffer.Length - 1 do
        let mutable value = 0.0
        for j in f do
                let period = (float n) / j
                value <- value + sin ((2.0 * 3.1415926 / period) * float i) * multiplier
                value <- value |> min 1.0 |> max -1.0
        buffer.[i] <- uint8 (value * 126.0 + 126.5)
    buffer

let squareWave (f : Note [], n : int, duration : float) =    
    let buffer = Array.zeroCreate<uint8> (int (float n * duration))
    for i = 0 to buffer.Length - 1 do
        let mutable value = 0.0
        for j in f do
            let period = float n / j
            let square =
                let tmp = sin ((2.0 * 3.1415926 / period) * float i)
                if tmp > 0.0 then 1.0 else -1.0
            value <- value + (square) * multiplier * 0.5 //half volume multiplier because squares are loud
            value <- value |> min 1.0 |> max -1.0
        buffer.[i] <- uint8 (value * 126.0 + 126.5)
    buffer

let sawWave (f : Note [], n : int, duration : float) =    
    let buffer = Array.zeroCreate<uint8> (int (float n * duration))
    for i = 0 to buffer.Length - 1 do
        let mutable value = 0.0
        for j in f do
            let period = float n / j
            let tmp = (2.0 * (float i / period - floor (0.5 + float i / period)) * multiplier)
            value <- value + tmp
            value <- value |> min 1.0 |> max -1.0
        buffer.[i] <- uint8 (value * 126.0 + 126.5)
    buffer

let triangleWave (f : Note [], n : int, duration : float) =    
    let buffer = Array.zeroCreate<uint8> (int (float n * duration))
    let tones = f.Length - 1
    for i = 0 to buffer.Length - 1 do
        let mutable value = 0.0
        for j in f do
            let period = float n / j
            let tmp = 2.0 * abs (2.0 * (float i / period - floor (0.5 + float i / period))) - 1.0
            value <- value + tmp * multiplier
            value <- value |> min 1.0 |> max -1.0
        buffer.[i] <- uint8 (value * 126.0 + 126.5)
    buffer

/// adds two waves together, so that they can be used in one buffer.
let waveConcat (first : uint8[]) (second : uint8[]) =
    [|
        yield! first
        yield! second
    |]

/// Adds a variable number of waves together, to combine parallel voices. The subsequent amplitude array is the length of the longest constituent array
let waveAdd (parts : uint8[][]) =
    [|
        //the length of the array to be returned
        let maxLen = Array.maxBy (fun t -> Array.length t) parts |> Array.length
        for i = 0 to maxLen - 1 do
            yield Array.filter (fun x -> i < Array.length x) parts |> Array.fold (fun (acc : uint8) next -> acc + next.[i]) (uint8 1)
    |]      