namespace Utils

module ForConsole =
    let getIntFromConsole defaultValue =
        let consoleRead = System.Console.ReadLine()
        match consoleRead.Length with
        | 0 -> defaultValue
        | _ -> System.Convert.ToInt32(consoleRead)

    let getStringFromConsole defaultValue =
        let consoleRead = System.Console.ReadLine()
        match consoleRead.Length with
        | 0 -> defaultValue
        | _ -> consoleRead