(*
open System

let randomizer = new Random()
let validChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ,#;. "

let getRandomChar() =
    let index = randomizer.Next(validChars.Length)
    validChars.[index]

let v1 = getRandomChar()
let v2 = getRandomChar()
let v3 = getRandomChar()

printfn "Valores: %c %c %c" v1 v2 v3
*)