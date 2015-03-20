namespace vSprog

open vSprog.Parser
open vSprog.Analysis
open vSprog.CommonTypes

module Main =



    (*



    let toInternalAST (ast:ASTNode) : AST =
        match ast.Symbol.Value with
        | "Statement" -> 
            match (ast.Children.Item 0).Symbol.Value with
            | "Initialisation" -> 
                let curNode = ast.Children.Item 0
                let lhs = toInternalEx (curNode.Children.Item 1)
                let rhs = toInternalEx (curNode.Children.Item 2)

                Assignment(lhs, rhs)
            | _ -> 
                printf "%s" "Var andet end declaration"
                Assignment(Int 1, Int 1)
        | _ -> Assignment(Int 0, Int 0)


    let levelToString level : String =
        [0..level-1]
        |> Seq.collect (fun l -> sprintf "%s" "--")
        |> String.Concat

    let rec printTree (node:ASTNode) (level:int) = 
        levelToString level |> printf "%s" 

        printfn "%s" node.Symbol.Value

        for child in node.Children do
            printTree child (level+1)


    let rec createDecoratedAST (ast:ASTNode) : DecoratedAST =
        match ast with
        | ast when ast.Children.Count = 0 -> 
            Leaf ({Symbol = ast.Symbol; Position = ast.Position; Children = ast.Children; NodeType = NotYetChecked })
        | ast -> Tree ( List.ofSeq (seq { for i in ast.Children -> createDecoratedAST i}) )

    let rec prettyPrintDecoratedAST dAST (level:int) =
        levelToString level |> printfn "%s" 

        printfn "%A" dAST
    *)

    [<EntryPoint>]
    let main argv = 
        let input = "{let a:int := 2;
                    var b:rea := 2.5;}"

        match parse input with
        | Success astRoot -> 
            printfn "%s" "Parsing succeeded. No errors."
            match analyse astRoot with
            | Success _ -> printfn "%s" "Analysis succeeded. No errors."
            | Failure _ -> printfn "%s" "Failure in analysis!!!"
        | Failure errs -> 
            printfn "%s" "Errors:"
            errs |> List.iter (printfn "%s")
        System.Console.ReadLine()
        0 // return an integer exit code

