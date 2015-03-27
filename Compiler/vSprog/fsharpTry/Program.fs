namespace vSprog

open vSprog.Parser
open vSprog.Analysis
open vSprog.CommonTypes
open vSprog.AST
open vSprog.ParserUtils

module Main =
    [<EntryPoint>]
    let main argv = 
        let input = "let a:int := 2;"

        let lift m = Success m

        let res = parse input
                  >>= (fun parseTree -> 
                                        printTree parseTree 0
                                        lift (toAST parseTree))
                  >>= analyse

        match res with
        | Success _ -> printfn "%s" "success"
        | Failure errs -> 
            printfn "%s" "Errors:"
            errs |> List.iter (printfn "%s")

        System.Console.ReadLine()
        0 // return an integer exit code

