namespace vSprog

open Hime.CentralDogma
open Hime.Redist
open vSprog.CommonTypes

module Parser =

    let parse (srcInput:string) : Result<ASTNode> =
        let task = CompilationTask ()
        task.AddInputFile "../../grammar.gram"
        task.Mode <- Hime.CentralDogma.Output.Mode.Assembly
        let report = task.Execute()
        if report.Errors.Count <> 0 && report.Warnings.Count <> 0 then
            // error or warnings
            Failure (report.Errors
                     |> Seq.append report.Warnings
                     |> Seq.map (sprintf "%O")
                     |> List.ofSeq)
        else 
            let assembly = SDK.AssemblyReflection "vSprogGrammar.dll"

            let parseResult = (assembly.GetParser<string> (srcInput)).Parse()
         
            if parseResult.Errors.Count = 0 && parseResult.IsSuccess then // TODO: Create fail function
                Success parseResult.Root
            else
                Failure (parseResult.Errors 
                     |> Seq.map (sprintf "%O")
                     |> List.ofSeq
                     )