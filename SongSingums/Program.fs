//1. make simple generator that generates a minute long song; single wave, single voice melody from a time signature and base frequency
//2. make program recognize rests and use them in generation
//3. Make generator that generates a melody and bass voice, uses overtones and different waves, and has song progression over the course of the song
//4. Audio's main purpose will be continuous streaming
open Audio
open SingerAbstract
open Waves
open System
open Freq


gen ()

Console.ReadKey true |> ignore