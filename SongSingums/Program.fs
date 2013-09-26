//PRE: make Singer inheritable or an abstract class so more generators can be made using the same basic methods for use in outside parts of the program. minimize the interaction of singer with the outside world. Make Singer use Waves so Audio doesn't have to care what instrument or voices that singer has, and just interprets the 
//1. make simple generator that generates a minute long song; single wave, single voice melody from a time signature and base frequency, that can be added to for different overtone, waveforms, and voice additions. Make it modular
//2. make program recognize rests and use them in generation
//3. Make generator that generates a melody and bass voice, uses overtones and different waves, and has song progression over the course of the song
//4. Audio's main purpose will be continuous streaming
open Audio
open Singer
open Waves
open System
open Freq


gen ()

Console.ReadKey true |> ignore

//while true do
//    let q = singer ()
//    let mes = (q.NextMeasure ()).[0]
//    let fucks = measureToArray mes
//    for i in fucks do
//        printfn "(%s * %s)" ((fst i).ToString ()) ((snd i).ToString ())
//    printfn "break"
//
//    Console.ReadKey (true) |> ignore

//6205459