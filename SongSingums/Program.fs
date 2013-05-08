// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open Audio
open Waves
open System
open Freq

let n = 44100
let input = 
    [|
        yield sinWave ([| frequencies.[C5]|], n, 5.0)
        //yield triangleWave ([| C5; Eb5; G5|], n, 5)
    |]

gen 5.0

Console.ReadKey true |> ignore