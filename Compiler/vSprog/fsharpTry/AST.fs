namespace vSprog

open Hime.CentralDogma
open Hime.Redist

module AST =
    type Primitive =
        | Int of int
        | Char
        | Real
        | Bool

    type PrimitiveType =
        | SimplePrimitive of Primitive
        | ListPrimitive of PrimitiveType
        | Node of AST
        | UserType of string

    and AST = 
        | Program of AST list
        | Block of AST list
        | Assignment of LValue * PrimitiveType
        | Constant of PrimitiveType
        | Error // Only for making it compile temporarily

    and LValue = {
        identity:string
        isMutable:bool
        primitiveType:PrimitiveType
    }

    let rec toPrimitiveType (input:string) : PrimitiveType =
        match input with
            | "int" -> SimplePrimitive (Int 0)
            | "char" -> SimplePrimitive Char
            | "real" -> SimplePrimitive Real
            | "bool" -> SimplePrimitive Bool
            | str when str.StartsWith("[") && str.EndsWith("]") -> ListPrimitive (toPrimitiveType (str.Substring (1, input.Length-2)))
            | str -> UserType str

    let toLValue (mutability:ASTNode) (name:ASTNode) (typeName:ASTNode) : LValue = 
        let isMutable = match mutability.Symbol.Value with
                            | "let" -> false
                            | "var" -> true
        {identity = name.Symbol.Value; 
        isMutable = isMutable;
        primitiveType = toPrimitiveType typeName.Symbol.Value}

    let rec toAST (root:ASTNode) : AST =
        match root.Symbol.Value with
        | "StatementList" as state -> 
            //printfn "%s %s" "Entered" state
            let t = traverseChildren root
            //printfn "%s %s" "Left" state
            Program t
        | "Statement" as state -> 
            //printfn "%s %s" "Entered" state
            traverseChildren root
            //printfn "%s %s" "Left" state
            Error
        | "Term" as state -> 
            //printfn "%s %s" "Entered" state
            traverseChildren root
            //printfn "%s %s" "Left" state
            Error
        | "Block" as state -> 
            //printfn "%s %s" "Entered" state
            let t =traverseChildren root
            //printfn "%s %s" "Left" state
            Block t
        | "Initialisation" as state ->
            //printfn "%s %s" "Entered" state
            //traverseChildren root
            let lhs = toLValue (root.Children.Item 0) (root.Children.Item 1) ((root.Children.Item 1).Children.Item 0)
            let rhs = Node (toAST (root.Children.Item 2))
            //printfn "%s %s" "Left" state
            Assignment (lhs, rhs)
        | "Integer" ->
            let value = int ((root.Children.Item 0).Symbol.Value)
            Constant (SimplePrimitive (Int value))
        | sym -> 
            printfn "%s%A" "ERROR: No match case for: " sym
            Error

    and traverseChildren (root:ASTNode) : AST list =
        List.ofSeq (seq { for i in root.Children -> i})
            |> List.map toAST
