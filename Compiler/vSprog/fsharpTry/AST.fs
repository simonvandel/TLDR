namespace vSprog

open Hime.CentralDogma
open Hime.Redist

module AST =
    type Primitive =
        | Int
        | Char
        | Real
        | Bool
        | Void

    type PrimitiveValue =
        | Int of int
        | Real of float
        | Bool of bool
        | Char of char
        | List of PrimitiveValue list

    type PrimitiveType =
        | SimplePrimitive of Primitive
        | ListPrimitive of PrimitiveType
        | ArrowPrimitive of PrimitiveType list
        | UserType of string

    and TypeDeclaration = string * PrimitiveType // name, type. Example: fieldName:int

    and AST = 
        | Program of AST list
        | Block of AST list
        | Body of AST list
        | Assignment of LValue * AST //AssignmentStruct // LValue * PrimitiveType
        | Reassignment of AST * AST // varIds (x.y == [x;y]), rhs // FIXME: string list skal være IdentityAccessor
        | Constant of PrimitiveType * PrimitiveValue
        | Actor of AST * AST // name, body FIXME: Add more fields?
        | Struct of string * TypeDeclaration list // name, body FIXME: Add more fields?
        | If of AST * AST // conditional * body
        | Send of string * string // actorName, msgName
        | Spawn of LValue * AST * AST // lvalue, actorName, initMsg
        | Receive of string * PrimitiveType * AST // msgName, msgType, body
        | ForIn of string * AST * AST // counterName, list, body
        | ListRange of AST list
        | Operation of OperationType
        | Identifier of Identifier
        | Function of string * string list * PrimitiveType list * AST// funcName, arguments, types, body
        | StructLiteral of (AST * AST) list
        | Invocation of string * string list // functionName, parameters
        | Error // Only for making it compile temporarily

    and Identifier =
        | SimpleIdentifier of string // x
        | IdentifierAccessor of string list // x.y == ["x"; "y"]

    and OperationType = AST * Operator * AST
        //| SingleOperation of AST

    and Operator =
        | Plus
        | Minus
        | Modulo
        | Equals
        | Multiply

    and LValue = {
        identity:AST//string
        isMutable:bool
        primitiveType:PrimitiveType
    }

    and AssignmentStruct = {
        identity:string 
        isMutable:bool 
        declType:PrimitiveType 
        rhs:AST
        }

    let rec toPrimitiveType (input:ASTNode) : PrimitiveType =
        match input.Symbol.Value with
            | "int" -> SimplePrimitive Primitive.Int
            | "char" -> SimplePrimitive Primitive.Char
            | "real" -> SimplePrimitive Primitive.Real
            | "bool" -> SimplePrimitive Primitive.Bool
            | "void" -> SimplePrimitive Primitive.Void
            | "Types" -> 
                match input.Children.Count with
                | 1 -> toPrimitiveType (input.Children.Item 0)
                | n -> 
                    seq { for c in input.Children do
                          yield toPrimitiveType c
                        }
                    |> List.ofSeq
                    |> ArrowPrimitive
                
            | "ListType" -> ListPrimitive (toPrimitiveType (input.Children.Item 0))
            | "PrimitiveType" -> toPrimitiveType (input.Children.Item 0)
            | "Identifier" -> UserType (input.Children.Item 0).Symbol.Value
            | str -> UserType str

    let toLValue (mutability:ASTNode) (name:AST) (typeName:ASTNode) : LValue = 
        let isMutable = match mutability.Symbol.Value with
                            | "let" -> false
                            | "var" -> true
                            | err -> failwith (sprintf "Mutability can never be: %s" err)
        {identity = name; 
        isMutable = isMutable;
        primitiveType = toPrimitiveType typeName}

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
        | "Program" ->
            let t = traverseChildren root
            Program t
        | "Body" ->
            let t = traverseChildren root
            Body t
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
            let name = toAST ((root.Children.Item 1).Children.Item 0)
            let typeName = ((root.Children.Item 1).Children.Item 1)
            let lhs = toLValue (root.Children.Item 0) name typeName
            let rhs = toAST (root.Children.Item 2)
            Assignment (lhs, rhs)
        | "Assignment" ->
            let assignables = toAST (root.Children.Item 0)
                                (*seq { for c in (root.Children.Item 0).Children do

                                          yield (c.Children.Item 0).Symbol.Value
                                 }
                              |> List.ofSeq *)
            let body = toAST (root.Children.Item 1)
            Reassignment (assignables, body)
        | "Integer" ->
            let value = Int (int ((root.Children.Item 0).Symbol.Value))
            Constant (SimplePrimitive (Primitive.Int), value) // FIXME: lav en int type. Lige nu bliver værdien af int konstanten ikke gemt
        | "Real" ->
            let value = Real ( float ((root.Children.Item 0).Symbol.Value))
            Constant (SimplePrimitive (Primitive.Real), value) // FIXME: lav en real type. Lige nu bliver værdien af real konstanten ikke gemt
        | "Actor" ->
            let name = toAST (root.Children.Item 0)
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
                    let typeName = ((((root.Children.Item 1).Children.Item 1).Children.Item 0).Children.Item 0)
                    Struct (name, [(fieldName, toPrimitiveType typeName)])
                | "TypeDecls" ->
                    let blocks = seq { for c in (root.Children.Item 1).Children do
                                          let fieldName = ((c.Children.Item 0).Children.Item 0).Symbol.Value
                                          let typeName = (((c.Children.Item 1).Children.Item 0).Children.Item 0) 
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
            let mutability = (root.Children.Item 0)
            let name = toAST ((root.Children.Item 1).Children.Item 0)
            let typeName = ((((root.Children.Item 1).Children.Item 1).Children.Item 0).Children.Item 0)
            let lhs = toLValue mutability name typeName
            let actorName = toAST (root.Children.Item 2)
            let initMsg = toAST ((root.Children.Item 3).Children.Item 0)
            Spawn (lhs, actorName, initMsg)
        | "Receive" ->
            let msgName = (((root.Children.Item 0).Children.Item 0).Children.Item 0).Symbol.Value
            let msgType = toPrimitiveType ((((root.Children.Item 0).Children.Item 1).Children.Item 0).Children.Item 0)
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
            match root.Children.Count with
            | 1 -> Identifier (SimpleIdentifier (root.Children.Item 0).Symbol.Value)
            | 2 -> 
                let ids = Seq.unfold (fun (node:ASTNode) -> 
                                        match node.Children.Count with
                                        | 0 -> None
                                        | _ -> Some ((node.Children.Item 0).Symbol.Value, (node.Children.Item 1))) 
                                        root
                          |> List.ofSeq
                Identifier (IdentifierAccessor ids)
            | err -> failwith (sprintf "This should never be reached: %A" err)
        | "Function" ->
            let funcName = ((root.Children.Item 0).Children.Item 0).Symbol.Value
            if root.Children.Count = 3 then // count is 3 when there is no arguments. fx f()
                let args = []
                let types = seq { for c in (root.Children.Item 1).Children do   
                                    yield (c.Children.Item 0)                    
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
                                    yield (c.Children.Item 0)                    
                                }
                            |> List.ofSeq
                            |> List.map toPrimitiveType
                let body = toAST (root.Children.Item 3)
                Function (funcName, args, types, body)
        | "IdentifierWithAccessor" ->
            let ids = seq { for c in root.Children do   
                                yield (c.Children.Item 0).Symbol.Value                 
                            }
                        |> List.ofSeq
            //Identifier (IdentifierAccessor ids )
            Error
        | "Invocation" ->
            let funcName = ((root.Children.Item 0).Children.Item 0).Symbol.Value
            if root.Children.Count = 1 then // no parameters                
                Invocation (funcName, [])
            else
                let parameters = seq { for c in (root.Children.Item 1).Children do   
                                        yield (c.Children.Item 0).Symbol.Value                 
                                     }
                                 |> List.ofSeq
                Invocation (funcName, parameters)
        | "StructLiteral" ->
            let fields = seq { for c in root.Children do
                                let fieldName1 = toAST (c.Children.Item 0)
                                let fieldValue1 = toAST (c.Children.Item 1)
                                yield (fieldName1, fieldValue1)               
                             }
                         |> List.ofSeq
            StructLiteral fields
        | "String" ->

            let (chars:PrimitiveValue list) = (root.Children.Item 0).Symbol.Value
                                              |> fun str -> 
                                                str.Substring (1, (str.Length - 2))
                                              |> Seq.map PrimitiveValue.Char
                                              |> List.ofSeq
            Constant (ListPrimitive (SimplePrimitive Primitive.Char), PrimitiveValue.List chars)
        | sym -> 
            printfn "ERROR: No match case for: %A" sym
            Error

    and traverseChildren (root:ASTNode) : AST list =
        List.ofSeq (seq { for i in root.Children -> i})
            |> List.map toAST
