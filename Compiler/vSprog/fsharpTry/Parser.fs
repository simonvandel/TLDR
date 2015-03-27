namespace vSprog

open Hime.CentralDogma
open Hime.Redist
open vSprog.CommonTypes

module Parser =

    let parse (srcInput:string) : Result<ASTNode> =
        let task = CompilationTask ()
        task.AddInputFile "../../vSprogGrammar.gram"
        task.Mode <- Hime.CentralDogma.Output.Mode.Assembly
        let report = task.Execute()
        let assembly = SDK.AssemblyReflection "vSprogGrammar.dll"

        let parseResult = (assembly.GetParser<string> (srcInput)).Parse()
         
        if parseResult.Errors.Count = 0 && parseResult.IsSuccess then
            Success parseResult.Root
        else
            Failure (parseResult.Errors 
                     |> Seq.map (sprintf "%O")
                     |> List.ofSeq
                     )