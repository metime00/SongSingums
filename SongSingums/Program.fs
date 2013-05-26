//Make Note locations and ratios exist as a fraction of two integers
open Audio
open Singer
open Waves
open System
open Freq

while true do
    let q = Singer ()
    let mes = (q.NextMeasure ()).Head
    let fucks = measureToArray mes
    for i in fucks do
        printfn "(%f * %f)" (fst i) (snd i)
    printfn "break"

    Console.ReadKey (true) |> ignore