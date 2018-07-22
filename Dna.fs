namespace Genetica

module Tipos = 
    type DNA<'T> = {
        Fitness: float
        Genes: 'T array
    }

module Operaciones =
    open Tipos

    let calculateFitness fitnessFn index sourceDna =
        {sourceDna with Fitness = fitnessFn index}

    let crossOver (randomizer:System.Random) parentDna sourceDna =
        let getRandomValueFor index = if randomizer.NextDouble() < 0.5 then sourceDna.Genes.[index] else parentDna.Genes.[index]
        { Fitness = 0.0; Genes = Array.init sourceDna.Genes.Length getRandomValueFor }

    let mutate (randomizer:System.Random) getRandomGeneFn mutationRate sourceDna =
        let getMutatedValueFor original = if randomizer.NextDouble() < mutationRate then getRandomGeneFn() else original
        {sourceDna with Genes = Array.init sourceDna.Genes.Length (fun i -> getMutatedValueFor sourceDna.Genes.[i]) }