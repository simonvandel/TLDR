namespace vSprog

open Hime.CentralDogma
open Hime.Redist
open vSprog.CommonTypes
open System.IO

module Parser =

    let getAssemblyAndRun (srcInput:string) = 
          let assembly = SDK.AssemblyReflection "vSprogGrammar.dll"

          let parseResult = (assembly.GetParser<string> (srcInput)).Parse()
     
          if parseResult.Errors.Count = 0 && parseResult.IsSuccess then // TODO: Create fail function
              Success parseResult.Root
          else
              Failure (parseResult.Errors 
                   |> Seq.map (sprintf "%O")
                   |> List.ofSeq
                   )
                  
    let generateAssembly (grammarPath:string) = 
        let task = CompilationTask ()
        task.AddInputFile grammarPath
        task.Mode <- Hime.CentralDogma.Output.Mode.Assembly
        let report = task.Execute()
        if report.Errors.Count <> 0 && report.Warnings.Count <> 0 then
            // error or warnings
            Failure (report.Errors
                     |> Seq.append report.Warnings
                     |> Seq.map (sprintf "%O")
                     |> List.ofSeq)
        else
            Success ()

    let parse (srcInput:string) (grammarPath:string) : Result<ASTNode> =
        if File.Exists("vSprogGrammar.dll")  then
            getAssemblyAndRun srcInput
        else
           generateAssembly grammarPath
           >-> getAssemblyAndRun srcInput