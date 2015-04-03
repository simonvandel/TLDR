namespace vSprog

open Hime.CentralDogma
open Hime.Redist

module AST =
    type Primitive =
        | Int
        | Char
        | Real
        | Bool

    type PrimitiveValue =
        | Int of int
        | Real of float

    type PrimitiveType =
        | SimplePrimitive of Primitive
        | ListPrimitive of PrimitiveType
        | Node of AST // FIXME: do we need this?
        | UserType of string

    and AST = 
        | Program of AST list
        | Block of AST list
        | Assignment of LValue * AST //AssignmentStruct // LValue * PrimitiveType
        | Constant of PrimitiveType * PrimitiveValue
        | Actor of string // name FIXME: Add more fields
        | Error // Only for making it compile temporarily

    and LValue = {
        identity:string
        isMutable:bool
        primitiveType:PrimitiveType
    }

    and AssignmentStruct = {
        identity:string 
        isMutable:bool 
        declType:PrimitiveType 
        rhs:AST
        }

    let rec toPrimitiveType (input:string) : PrimitiveType =
        match input with
            | "int" -> SimplePrimitive (Primitive.Int)
            | "char" -> SimplePrimitive Char
            | "real" -> SimplePrimitive Primitive.Real
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
            let name = ((root.Children.Item 1).Children.Item 0)
            let typeName = ((root.Children.Item 1).Children.Item 1).Children.Item 0
            let lhs = toLValue (root.Children.Item 0) name typeName
            let rhs = toAST (root.Children.Item 2) //Node (toAST (root.Children.Item 2))
            //printfn "%s %s" "Left" state
            Assignment (lhs, rhs)
        | "Integer" ->
            let value = Int (int ((root.Children.Item 0).Symbol.Value))
            Constant (SimplePrimitive (Primitive.Int), value) // FIXME: lav en int type. Lige nu bliver værdien af int konstanten ikke gemt
        | "Real" ->
            let value = Real ( float ((root.Children.Item 0).Symbol.Value))
            Constant (SimplePrimitive (Primitive.Real), value) // FIXME: lav en real type. Lige nu bliver værdien af real konstanten ikke gemt
        | "Actor" ->
            let name = ((root.Children.Item 0).Children.Item 0).Symbol.Value
            Actor name
        | sym -> 
            printfn "ERROR: No match case for: %A" sym
            Error

    and traverseChildren (root:ASTNode) : AST list =
        List.ofSeq (seq { for i in root.Children -> i})
            |> List.map toAST
