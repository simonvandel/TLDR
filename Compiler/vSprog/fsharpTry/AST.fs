﻿namespace vSprog

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
        | Bool of bool

    type PrimitiveType =
        | SimplePrimitive of Primitive
        | ListPrimitive of PrimitiveType
        | Node of AST // FIXME: do we need this?
        | UserType of string

    and TypeDeclaration = string * PrimitiveType // name, type. Example: fieldName:int

    and AST = 
        | Program of AST list
        | Block of AST list
        | Assignment of LValue * AST //AssignmentStruct // LValue * PrimitiveType
        | Reassignment of string list * AST // varIds (x.y == [x;y]), rhs
        | Constant of PrimitiveType * PrimitiveValue
        | Actor of string * AST // name, body FIXME: Add more fields?
        | Struct of string * TypeDeclaration list // name, body FIXME: Add more fields?
        | If of AST * AST // conditional * body
        | Send of string * string // actorName, msgName
        | Spawn of LValue * string * string // lvalue, actorName, initMsg
        | Receive of string * PrimitiveType * AST // msgName, msgType, body
        | ForIn of string * AST * AST // counterName, list, body
        | ListRange of AST list
        | Operation of OperationType
        | Identifier of string
        | Function of string * string list * PrimitiveType list * AST// funcName, arguments, types, body
        | Error // Only for making it compile temporarily

    and OperationType = AST * Operator * AST
        //| SingleOperation of AST

    and Operator =
        | Plus
        | Minus
        | Modulo
        | Equals
        | Multiply

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
            | "bool" -> SimplePrimitive Primitive.Bool
            | str when str.StartsWith("[") && str.EndsWith("]") -> ListPrimitive (toPrimitiveType (str.Substring (1, input.Length-2)))
            | str -> UserType str

    let toLValue (mutability:ASTNode) (name:ASTNode) (typeName:ASTNode) : LValue = 
        let isMutable = match mutability.Symbol.Value with
                            | "let" -> false
                            | "var" -> true
        {identity = name.Symbol.Value; 
        isMutable = isMutable;
        primitiveType = toPrimitiveType typeName.Symbol.Value}

    let toOperator (operator:string) : Operator =
        match operator with
        | "+" -> Plus
        | "-" -> Minus
        | "%" -> Modulo
        | "=" -> Equals
        | "*" -> Multiply
        | err -> failwith (sprintf "Not implemented yet %s" err)

    let rec toAST (root:ASTNode) : AST =
        match root.Symbol.Value with
        | "Body" ->
            let t = traverseChildren root
            Program t
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
        | "Block" as state -> 
            //printfn "%s %s" "Entered" state
            let t =traverseChildren root
            //printfn "%s %s" "Left" state
            Block t
        | "Initialisation" as state ->
            let name = (((root.Children.Item 1).Children.Item 0).Children.Item 0)
            let typeName = (((root.Children.Item 1).Children.Item 1).Children.Item 0).Children.Item 0
            let lhs = toLValue (root.Children.Item 0) name typeName
            let rhs = toAST (root.Children.Item 2)
            Assignment (lhs, rhs)
        | "Assignment" ->
            let assignables = seq { for c in (root.Children.Item 0).Children do
                                          yield (c.Children.Item 0).Symbol.Value
                                 }
                              |> List.ofSeq
            let body = toAST (root.Children.Item 1)
            Reassignment (assignables, body)
        | "Integer" ->
            let value = Int (int ((root.Children.Item 0).Symbol.Value))
            Constant (SimplePrimitive (Primitive.Int), value) // FIXME: lav en int type. Lige nu bliver værdien af int konstanten ikke gemt
        | "Real" ->
            let value = Real ( float ((root.Children.Item 0).Symbol.Value))
            Constant (SimplePrimitive (Primitive.Real), value) // FIXME: lav en real type. Lige nu bliver værdien af real konstanten ikke gemt
        | "Actor" ->
            let name = (((root.Children.Item 0).Children.Item 0).Children.Item 0).Symbol.Value
            let block = toAST (root.Children.Item 1)
            Actor (name, block)
        | "If" ->
            let conditional = toAST (root.Children.Item 0)
            let body = toAST (root.Children.Item 1)
            If (conditional, body)
        | "Boolean" ->
            let value = match (root.Children.Item 0).Symbol.Value with
                        | "true" -> true
                        | "false" -> false
                        | _ -> failwith "Something terribly went wrong in toAST boolean. This should never be reached."
            Constant (SimplePrimitive Primitive.Bool, Bool value)

        | "Struct" ->
            let name = ((root.Children.Item 0).Children.Item 0).Symbol.Value
            if root.Children.Count = 1 then
                Struct (name, []) // there might only be a name available for the struct; empty block
            else
                match (root.Children.Item 1).Symbol.Value with
                | "TypeDecl" -> 
                    let fieldName = (((root.Children.Item 1).Children.Item 0).Children.Item 0).Symbol.Value
                    let typeName = ((((root.Children.Item 1).Children.Item 1).Children.Item 0).Children.Item 0).Symbol.Value
                    Struct (name, [(fieldName, toPrimitiveType typeName)])
                | "TypeDecls" ->
                    let blocks = seq { for c in (root.Children.Item 1).Children do
                                          let fieldName = ((c.Children.Item 0).Children.Item 0).Symbol.Value
                                          let typeName = (((c.Children.Item 1).Children.Item 0).Children.Item 0).Symbol.Value    
                                          yield (fieldName, toPrimitiveType typeName)                        
                                 }
                                 |> List.ofSeq
                    Struct (name, blocks)
                | err -> failwith (sprintf "This should never be reached: %s" err)
        | "Send" ->
            let actorHandle = ((root.Children.Item 0).Children.Item 0).Symbol.Value
            let msg = (((root.Children.Item 1).Children.Item 0).Children.Item 0).Symbol.Value
            Send (actorHandle, msg)
        | "Spawn" ->
            let name = (((root.Children.Item 1).Children.Item 0).Children.Item 0)
            let typeName = ((((root.Children.Item 1).Children.Item 1).Children.Item 0).Children.Item 0).Children.Item 0
            let lhs = toLValue (root.Children.Item 0) name typeName
            let actorName = ((root.Children.Item 2).Children.Item 0).Symbol.Value
            let initMsg = (((root.Children.Item 3).Children.Item 0).Children.Item 0).Symbol.Value
            Spawn (lhs, actorName, initMsg)
        | "Receive" ->
            let msgName = (((root.Children.Item 0).Children.Item 0).Children.Item 0).Symbol.Value
            let msgType = toPrimitiveType ((((root.Children.Item 0).Children.Item 1).Children.Item 0).Children.Item 0).Symbol.Value
            let body = toAST (root.Children.Item 1)
            Receive (msgName, msgType, body)
        | "ForIn" ->
            let counterName = (((root.Children.Item 0).Children.Item 0).Children.Item 0).Symbol.Value
            let list = toAST (root.Children.Item 1)
            let body = toAST (root.Children.Item 2)
            ForIn (counterName, list, body)
        | "ListRange" ->
            let start = int ((((root.Children.Item 0).Children.Item 0).Children.Item 0).Children.Item 0).Symbol.Value
            let end' = int ((((root.Children.Item 0).Children.Item 1).Children.Item 0).Children.Item 0).Symbol.Value
            ListRange ([start..end'] |> List.map (fun n -> Constant (SimplePrimitive Primitive.Int, Int n)))
        | ("Factor" | "Term" | "Operation") ->
            match (root.Children.Count) with
            | 3 -> 
                let operation = toAST (root.Children.Item 0)
                let operator = toOperator (root.Children.Item 1).Symbol.Value
                let operand = toAST (root.Children.Item 2)
                Operation (operation, operator, operand)
            | 1 -> 
                toAST (root.Children.Item 0)
            | err -> failwith (sprintf "This should never be reached: %A" err)
        | "Identifier" ->
            Identifier (root.Children.Item 0).Symbol.Value
        | "Function" ->
            let funcName = ((root.Children.Item 0).Children.Item 0).Symbol.Value
            if root.Children.Count = 3 then // count is 3 when there is no arguments. fx f()
                let args = []
                let types = seq { for c in (root.Children.Item 1).Children do   
                                    yield (c.Children.Item 0).Symbol.Value                    
                                }
                            |> List.ofSeq
                            |> List.map toPrimitiveType
                let body = toAST (root.Children.Item 2)
                Function (funcName, args, types, body)
            else
                let args = seq { for c in (root.Children.Item 1).Children do   
                                   yield (c.Children.Item 0).Symbol.Value                    
                               }
                           |> List.ofSeq
                let types = seq { for c in (root.Children.Item 2).Children do   
                                    yield (c.Children.Item 0).Symbol.Value                 
                                }
                            |> List.ofSeq
                            |> List.map toPrimitiveType
                let body = toAST (root.Children.Item 3)
                Function (funcName, args, types, body)
            
        | sym -> 
            printfn "ERROR: No match case for: %A" sym
            Error

    and traverseChildren (root:ASTNode) : AST list =
        List.ofSeq (seq { for i in root.Children -> i})
            |> List.map toAST
