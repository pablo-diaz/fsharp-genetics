open System
open Genetics.Algorithm
open Genetics.Types
open Utils.ForConsole

[<EntryPoint>]
let main _ =
    let mutationRate = 0.01;
    let randomizer = new Random()
    let validChars = "abcdefghijklmnñopqrstuvwxyzABCDEFGHIJKLMNÑOPQRSTUVWXYZ,#;.áéíóúÁÉÍÓÚ "

    printfn "Write phrase to check (ENTER for default): "
    let targetString = getStringFromConsole "Pablo Andrés Díaz Patiño"

    printfn "Population Size (ENTER for 200 as default): "
    let populationSize = getIntFromConsole 200

    printfn "Elitism size (ENTER for 5 as default): "
    let elitismSize = getIntFromConsole 5

    let getRandomChar() =
        let index = randomizer.Next(validChars.Length)
        validChars.[index]

    let generateRandomGenes() = Array.init targetString.Length (fun _ -> getRandomChar())
    let initialPopulation = List.init populationSize (fun _ -> {Fitness = 0.0; Genes = generateRandomGenes()})
    let initialDomain = {Population = initialPopulation; GenerationNumber = 1; BestFitness = 0.0; FitnessSum = 0.0; BestGenes = generateRandomGenes()}

    let rec solveFor sourceDomain =
        let fitnessFn index = 
            let score = sourceDomain.Population.[index].Genes |> Array.mapi (fun i x -> if x = targetString.[i] then 1.0 else 0.0) |> Array.sum
            let fitness = score / float targetString.Length
            let adjustedFitness = (MathF.Pow(5.0f, float32 fitness) - 1.0f) / (5.0f - 1.0f)
            float adjustedFitness

        let newDomain = sourceDomain |> newGeneration fitnessFn randomizer elitismSize getRandomChar mutationRate
        printfn "BestFitness: %f - Generation No. %d -> %s" newDomain.BestFitness newDomain.GenerationNumber (String.Concat newDomain.BestGenes)
        if newDomain.BestFitness < 1.0 then solveFor newDomain 
        else 
            printfn "Finished -> %s" (String.Concat newDomain.BestGenes)
            0  // exit the loop

    solveFor initialDomain |> ignore

    0 // return an integer exit code
