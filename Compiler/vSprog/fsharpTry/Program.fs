namespace vSprog

open vSprog.Parser
open vSprog.Analysis
open vSprog.CommonTypes
open vSprog.AST
open vSprog.ParserUtils
open System.IO

module Main =
    [<EntryPoint>]
    let main argv = 
        let input = File.ReadAllText "../../calcutor.tldr"

        let lift m = Success m

        let res = parse input "../../grammar.gram"           //Generates hime AST
                  >>= (fun parseTree -> 
                                        //printTree parseTree 0
                                        lift (toAST parseTree))
                  >>= fun ast -> //printfn "%A" ast
                                 Success ast
                  >>= analyse

        match res with
        | Success _ -> printfn "success"
        | Failure errs -> 
            printfn "Errors:"
            errs |> List.iter (printfn "%s")

        System.Console.ReadLine()
        0 // return an integer exit code