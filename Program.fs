open System
open Genetica.Algorithm
open Genetica.Tipos
open Utils.ForConsole

[<EntryPoint>]
let main _ =
    let mutationRate = 0.01;
    let randomizer = new Random()
    let validChars = "abcdefghijklmnñopqrstuvwxyzABCDEFGHIJKLMNÑOPQRSTUVWXYZ,#;.áéíóúÁÉÍÓÚ "

    printfn "Frase a revisar: "
    let targetString = getStringFromConsole "Pablo Andrés Díaz Patiño"

    printfn "Tamaño de la Población: "
    let populationSize = getIntFromConsole 200

    printfn "Tamaño de la Élite: "
    let elitism = getIntFromConsole 5

    let getRandomChar() =
        let index = randomizer.Next(validChars.Length)
        validChars.[index]

    let generateRandomGenes() = Array.init targetString.Length (fun _ -> getRandomChar())
    let initialPopulation = List.init populationSize (fun _ -> {Fitness = 0.0; Genes = generateRandomGenes()})
    let initialDomain = {Population = initialPopulation; GenerationNumber = 1; BestFitness = 0.0; FitnessSum = 0.0; BestGenes = generateRandomGenes()}

    let rec solve sourceDomain =
        let fitnessFn index = 
            let score = sourceDomain.Population.[index].Genes |> Array.mapi (fun i x -> if x = targetString.[i] then 1.0 else 0.0) |> Array.sum
            let fitness = score / float targetString.Length
            let adjustedFitness = (MathF.Pow(5.0f, float32 fitness) - 1.0f) / (5.0f - 1.0f)
            float adjustedFitness

        let newDomain = sourceDomain |> newGeneration fitnessFn randomizer elitism getRandomChar mutationRate
        printfn "BestFitness: %f - Generation No. %d -> %s" newDomain.BestFitness newDomain.GenerationNumber (String.Concat newDomain.BestGenes)
        if newDomain.BestFitness < 1.0 then solve newDomain 
        else 
            printfn "Finished -> %s" (String.Concat newDomain.BestGenes)
            0  // exit the loop

    solve initialDomain |> ignore

    0 // return an integer exit code
