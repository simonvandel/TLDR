namespace vSprog

open vSprog.Parser
open vSprog.Analysis
open vSprog.CommonTypes
open vSprog.AST
open vSprog.ParserUtils
open vSprog.CodeGen
open System.IO
open System
open vSprog.Utils

module Main =
    let llvmToExec (llvmIr:string) : bool =
        let file = new StreamWriter("out.ll")
        file.Write(llvmIr)
        file.Close()
        let proc = new System.Diagnostics.Process()

        // --------------- llc -------------
        proc.StartInfo.FileName <- "clang"
        proc.StartInfo.Arguments <- "out.ll -O3 -Wall -lactor"
        let llcRes = proc.Start()
        proc.WaitForExit()
        if llcRes && proc.ExitCode = 0 then // only run clang when llc succeeded
            // --------------- clang -------------
            //let clangProc = new System.Diagnostics.Process()
            //clangProc.StartInfo.FileName <- "./a.out"
            //clangProc.Start() |> ignore
            //clangProc.WaitForExit()
            //clangProc.ExitCode = 0
            false

        else false



    
    [<EntryPoint>]
    let main argv = 
        let inputSrcPath = if argv.Length = 0 then 
                             printfn "No input file specified"
                             Environment.Exit -1
                             ""
                           else 
                             argv.[0]

        let input = File.ReadAllText inputSrcPath

        let res = genProgram input

        match res with
        | Success a -> 
            printfn "Success"
            //printfn "%A" a
            //printfn "-----------------------------------\n"
            llvmToExec a
            |> ignore
        | Failure errs -> 
            printfn "Errors:"
            errs |> List.iter (printfn "%s")

        0 // return an integer exit code