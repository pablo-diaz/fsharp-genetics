namespace Genetics

module Types = 
    type DNA<'T> = {
        Fitness: float
        Genes: 'T array
    }

module DNAOperations =
    open Types

    let calculateFitness fitnessFn index sourceDna =
        {sourceDna with 
            Fitness = fitnessFn index}

    let crossOverWith (randomizer:System.Random) parentDna sourceDna =
        let getRandomValueForGeneAt index = 
            if randomizer.NextDouble() < 0.5 then 
                sourceDna.Genes.[index] 
            else 
                parentDna.Genes.[index]
        { Fitness = 0.0
          Genes = Array.init sourceDna.Genes.Length getRandomValueForGeneAt }

    let mutate (randomizer:System.Random) getRandomGeneFn mutationRate sourceDna =
        let mutatedValue defaultValue = 
            if randomizer.NextDouble() < mutationRate then 
                getRandomGeneFn() 
            else 
                defaultValue
        {sourceDna with 
            Genes = Array.init sourceDna.Genes.Length (fun i -> mutatedValue sourceDna.Genes.[i]) }