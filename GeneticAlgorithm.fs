namespace Genetics

module Algorithm =
    open Types

    type Domain<'T> = {
        Population: DNA<'T> list
        GenerationNumber: int
        BestFitness: float
        BestGenes: 'T array
        FitnessSum: float
    }

    let private calculateFitness fitnessFn sourceDomain =
        let newPopulation = 
            sourceDomain.Population 
            |> List.mapi (fun i x -> DNAOperations.calculateFitness fitnessFn i x)
        let bestDNA = 
            newPopulation 
            |> List.maxBy (fun x -> x.Fitness)
        let fitnessSum = 
            newPopulation 
            |> List.map (fun x -> x.Fitness) 
            |> List.reduce (( + ))
        {sourceDomain with 
            Population = newPopulation
            FitnessSum = fitnessSum
            BestFitness = bestDNA.Fitness
            BestGenes = bestDNA.Genes}

    let rec private tryToFindParent randomValueToCheck list =
        match list with
        | headDna::otherDnas -> 
            if randomValueToCheck < headDna.Fitness then 
                Some headDna
            else
                let newRandom = randomValueToCheck - headDna.Fitness
                tryToFindParent newRandom otherDnas
        | [] -> None    

    let private chooseParent (randomizer: System.Random) sourceDomain =
        let randomNumber = randomizer.NextDouble() * sourceDomain.FitnessSum
        sourceDomain.Population |> tryToFindParent randomNumber

    let private getChild randomizer elitism getRandomGeneFn mutationRate domain index dna =
        if index < elitism then 
            dna
        else
            let parent1 = chooseParent randomizer domain
            let parent2 = chooseParent randomizer domain
            match (parent1, parent2) with
            | (Some _, Some _) -> 
                parent1.Value 
                |> DNAOperations.crossOverWith randomizer parent2.Value 
                |> DNAOperations.mutate randomizer getRandomGeneFn mutationRate
            | _ -> dna    

    let newGeneration fitnessFn randomizer elitism getRandomGeneFn mutationRate sourceDomain =
        if sourceDomain.Population.Length <= 0 then 
            sourceDomain
        else
            let newDomainWithCalculatedFitness = calculateFitness fitnessFn sourceDomain
            let newSortedPopulation = 
                newDomainWithCalculatedFitness.Population 
                |> List.sortByDescending (fun x -> x.Fitness)
            let crossedAndMutatedPopulation = 
                newSortedPopulation 
                |> List.mapi (fun i x -> getChild randomizer elitism getRandomGeneFn mutationRate newDomainWithCalculatedFitness i x)
            {newDomainWithCalculatedFitness with 
                Population = crossedAndMutatedPopulation
                GenerationNumber = newDomainWithCalculatedFitness.GenerationNumber + 1}