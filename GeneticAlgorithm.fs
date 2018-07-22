namespace Genetica

module Algorithm =
    open Tipos
    open Operaciones

    type Domain<'T> = {
        Population: DNA<'T> list
        GenerationNumber: int
        BestFitness: float
        BestGenes: 'T array
        FitnessSum: float
    }

    let private calculateFitness fitnessFn sourceDomain =
        let newPopulation = sourceDomain.Population |> List.mapi (fun i x -> calculateFitness fitnessFn i x)
        let bestDNA = newPopulation |> List.maxBy (fun x -> x.Fitness)
        let fitnessSum = newPopulation |> List.map (fun x -> x.Fitness) |> List.reduce (( + ))
        {sourceDomain with Population = newPopulation; FitnessSum = fitnessSum; BestFitness = bestDNA.Fitness; BestGenes = bestDNA.Genes}

    let private chooseParent (randomizer: System.Random) sourceDomain =
        let randomNumber = randomizer.NextDouble() * sourceDomain.FitnessSum
        let rec tryFindParent randomValueToCheck list =
            match list with
            | headDna::otherDnas -> 
                if randomValueToCheck < headDna.Fitness then Some headDna
                else
                    let newRandom = randomValueToCheck - headDna.Fitness
                    tryFindParent newRandom otherDnas
            | [] -> None
        sourceDomain.Population |> tryFindParent randomNumber

    let newGeneration fitnessFn randomizer elitism getRandomGeneFn mutationRate sourceDomain =
        if sourceDomain.Population.Length <= 0 then sourceDomain
        else
            let newDomainWithCalculatedFitness = calculateFitness fitnessFn sourceDomain
            let newSortedPopulation = newDomainWithCalculatedFitness.Population |> List.sortByDescending (fun x -> x.Fitness)
            let getChildFn elitism getRandomGeneFn mutationRate index dna =
                if index < elitism then dna
                else
                    let parent1 = chooseParent randomizer newDomainWithCalculatedFitness
                    let parent2 = chooseParent randomizer newDomainWithCalculatedFitness
                    match (parent1, parent2) with
                    | (Some _, Some _) -> parent1.Value |> crossOver randomizer parent2.Value |> mutate randomizer getRandomGeneFn mutationRate
                    | _ -> dna
            let crossedAndMutatedPopulation = newSortedPopulation |> List.mapi (fun i x -> getChildFn elitism getRandomGeneFn mutationRate i x)
            {newDomainWithCalculatedFitness with Population = crossedAndMutatedPopulation; GenerationNumber = newDomainWithCalculatedFitness.GenerationNumber + 1}