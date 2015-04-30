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

        let res = parse input "../../grammar.gram"  //Generates hime AST
                >>= fun tree -> Success (toAST tree)
                //>>= fun ast -> Success ast  // Gør den noget?
                >>= analyse

        match res with
        | Success _ -> printfn "success"
        | Failure errs -> 
            printfn "Errors:"
            errs |> List.iter (printfn "%s")

        0 // return an integer exit code