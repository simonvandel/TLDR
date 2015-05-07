namespace vSprog

open Hime.CentralDogma
open Hime.Redist

module AST =
    type PrimitiveValue =
        | Int of int
        | Real of float
        | Bool of bool
        | Char of char
        | List of PrimitiveValue list

    type Primitive =
        | Int
        | Char
        | Real
        | Bool
        | Void
        | Actor of string
        | Struct of string * (TypeDeclaration list)

    and PrimitiveType =
        | SimplePrimitive of Primitive
        | ListPrimitive of PrimitiveType
        | ArrowPrimitive of PrimitiveType list
        | UserType of string
        | HasNoType
        | StillUnknown of AST

    and TypeDeclaration = string * PrimitiveType // name, type. Example: fieldName:int

    and AST = 
        | Program of AST list
        | Block of AST list
        | Body of AST list
        | Assignment of bool * string * AST // mutability, varId, value
        | Reassignment of Identifier * AST // varId, rhs
        | Initialisation of LValue * AST // lvalue, rhs
        | Declaration of string * PrimitiveType
        | Constant of PrimitiveType * PrimitiveValue // type, value
        | Actor of string * AST // name, body FIXME: Add more fields?
        | Struct of string * TypeDeclaration list // name, fields FIXME: Add more fields?
        | If of AST * AST // conditional, body
        | IfElse of AST * AST * AST // conditional, trueBody, falseBody
        | Send of string * string // actorName, msgName
        | Spawn of LValue * string * AST option // lvalue, actorName, initMsg
        | Receive of string * PrimitiveType * AST // msgName, msgType, body
        | ForIn of string * AST * AST // counterName, list, body
        | While of AST * AST // condition, body
        | ListRange of AST list // content
        | BinOperation of AST * BinOperator * AST // lhs, op, rhs
        | UnaryOperation of UnaryOperator * AST // op, rhs
        | Identifier of Identifier
        | Function of string * string list * PrimitiveType * AST// funcName, arguments, types, body
        | StructLiteral of (string * AST) list // (fieldName, fieldValue) list
        | Invocation of string * string list // functionName, parameters
        | Return of AST // body
        | Kill of AST // whatToKill
        | Me

    and Identifier =
        | SimpleIdentifier of string // x
        | IdentifierAccessor of string list // x.y == ["x"; "y"]


    and BinOperator =
        | Plus
        | Minus
        | Modulo
        | Multiply
        | Divide
        | Power
        | Root
        | Equals
        | NotEquals
        | GreaterThan
        | GreaterThanOrEq
        | LessThan
        | LessThanOrEq
        | And
        | Or
        | Xor
        | Nor
        | Nand

    and UnaryOperator = 
        | Not

    and LValue = {
        identity:Identifier
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

    let toMutability (input:ASTNode) : bool =
        match input.Symbol.Value with
                            | "let" -> false
                            | "var" -> true
                            | err -> failwith (sprintf "Mutability can never be: %s" err)

    let toLValue (mutability:ASTNode) (name:AST) (typeName:ASTNode) : LValue = 
        let isMutable = toMutability mutability
        {identity = (match name with
                    | Identifier id -> id)
                    ; 
        isMutable = isMutable;
        primitiveType = toPrimitiveType typeName}

    let toUnaryOperator (operator:string) : UnaryOperator =
        match operator with
        | "NOT" -> Not
        | err -> failwith (sprintf "Not implemented yet %s" err)

    let toBinOperator (operator:string) : BinOperator =
        match operator with
        | "+" -> Plus
        | "-" -> Minus
        | "%" -> Modulo
        | "*" -> Multiply
        | "/" -> Divide
        | "^" -> Power
        | "#" -> Root
        | "=" -> Equals
        | "!=" -> NotEquals
        | ">" -> GreaterThan
        | ">=" -> GreaterThanOrEq
        | "<"  -> LessThan
        | "<=" -> LessThanOrEq
        | "AND" -> And
        | "OR" -> Or
        | "XOR" -> Xor
        | "NOR" -> Nor
        | "NAND" -> Nand
        | err -> failwith (sprintf "Not implemented yet %s" err)

    let getChildByIndexes (childIds:int list) (startNode:ASTNode) : ASTNode = // Access a specific child in a tree with index[n,m,..,z]
        List.fold (fun node n -> node.Children.Item n) startNode childIds

    let rec toAST (root:ASTNode) : AST =
        match root.Symbol.Value with
        | "Program" ->
            let t = traverseChildren root
            Program t
        | "Body" ->
            let t = traverseChildren root
            Body t
        | "Block" -> 
            let t = traverseChildren root
            Block t
        | "Initialisation" ->
            let name = toAST (getChildByIndexes [1;0] root)
            let typeName = (getChildByIndexes [1;1] root)
            let lhs = toLValue (root.Children.Item 0) name typeName
            let rhs = toAST (root.Children.Item 2)
            Initialisation (lhs, rhs)
        | "Assignment" ->
            let mutability = toMutability (root.Children.Item 0)
            let name = match toAST (getChildByIndexes [1;0] root) with
                       | Identifier (SimpleIdentifier str) -> str
                       | err -> failwith (sprintf "This should never be reached: %A" err)
            let rhs = toAST (root.Children.Item 2)
            Assignment (mutability, name, rhs)
        | "Reassignment" ->
            let assignables = 
                match toAST (root.Children.Item 0) with
                | Identifier id -> id
                | err -> failwith (sprintf "This should never be reached: %A" err)
            let body = toAST (root.Children.Item 1)
            Reassignment (assignables, body)
        | "Integer" ->
            let value = PrimitiveValue.Int (int ((root.Children.Item 0).Symbol.Value))
            Constant (SimplePrimitive (Primitive.Int), value) // FIXME: lav en int type. Lige nu bliver værdien af int konstanten ikke gemt
        | "Real" ->
            let value = PrimitiveValue.Real ( float ((root.Children.Item 0).Symbol.Value))
            Constant (SimplePrimitive (Primitive.Real), value) // FIXME: lav en real type. Lige nu bliver værdien af real konstanten ikke gemt
        | "Actor" ->
            let name = (getChildByIndexes [0;0] root).Symbol.Value
            let block = toAST (root.Children.Item 1)
            Actor (name, block)
        | "If" ->
            let conditional = toAST (root.Children.Item 0)
            let body = toAST (root.Children.Item 1)
            If (conditional, body)
        | "IfElse" ->
            let conditional = toAST (root.Children.Item 0)
            let trueBody = toAST (root.Children.Item 1)
            let falseBody = toAST (root.Children.Item 2)
            IfElse (conditional, trueBody, falseBody)
        | "Boolean" ->
            let value = 
                match (root.Children.Item 0).Symbol.Value with
                | "true" -> true
                | "false" -> false
                | _ -> failwith "Something terribly went wrong in toAST boolean. This should never be reached."
            Constant (SimplePrimitive Primitive.Bool, PrimitiveValue.Bool value)
        | "Struct" ->
            let name = ((root.Children.Item 0).Children.Item 0).Symbol.Value
            if root.Children.Count = 1 then
                Struct (name, []) // there might only be a name available for the struct; empty block
            else
                match (root.Children.Item 1).Symbol.Value with
                | "TypeDecl" -> 
                    let fieldName = (getChildByIndexes [1;0;0] root).Symbol.Value
                    let typeName = getChildByIndexes [1;1;0;0] root
                    Struct (name, [(fieldName, toPrimitiveType typeName)])
                | "TypeDecls" ->
                    let blocks = 
                        [for c in (root.Children.Item 1).Children do
                            let fieldName = (getChildByIndexes [0;0] c).Symbol.Value
                            let typeName = getChildByIndexes [1;0;0] c
                            yield (fieldName, toPrimitiveType typeName)
                        ]                        
                    Struct (name, blocks)
                | err -> failwith (sprintf "This should never be reached: %s" err)
        | "Send" ->
            let actorHandle = (getChildByIndexes [0;0] root).Symbol.Value
            let msg = (getChildByIndexes [1;0;0] root).Symbol.Value
            Send (actorHandle, msg)
        | "Spawn" ->
            let mutability = (root.Children.Item 0)
            let name = toAST (getChildByIndexes [1;0] root)
            let typeName = (getChildByIndexes [1;1;0;0] root)
            let lhs = toLValue mutability name typeName
            let actorName = (getChildByIndexes [2;0] root).Symbol.Value
            if root.Children.Count = 4 then // msg was supplied
                let initMsg = toAST (getChildByIndexes [3;0] root)
                Spawn (lhs, actorName, Some initMsg)
            else // no msg was supplied
                Spawn (lhs, actorName, None)
        | "Receive" ->
            let msgName = (getChildByIndexes [0;0;0] root).Symbol.Value
            let msgType = toPrimitiveType (getChildByIndexes [0;1;0;0] root)
            let body = toAST (root.Children.Item 1)
            Receive (msgName, msgType, body)
        | "ForIn" ->
            let counterName = (getChildByIndexes [0;0;0] root).Symbol.Value
            let list = toAST (root.Children.Item 1)
            let body = toAST (root.Children.Item 2)
            ForIn (counterName, list, body)
        | "ListRange" ->
            let start = int (getChildByIndexes [0;0;0;0] root).Symbol.Value
            let end' = int (getChildByIndexes [0;1;0;0] root).Symbol.Value
            ListRange ([start..end'] |> List.map (fun n -> Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int n)))
        | ("OP1" | "OP2" | "OP3" | "OP4" | "OP5" | "OP6" | "Operation") ->
            match (root.Children.Count) with
            | 3 -> 
                let operation = toAST (root.Children.Item 0)
                let operator = toBinOperator (root.Children.Item 1).Symbol.Value
                let operand = toAST (root.Children.Item 2)
                BinOperation (operation, operator, operand)
            | 2 -> 
                 let operation = toUnaryOperator (root.Children.Item 0).Symbol.Value
                 let body = toAST (root.Children.Item 1)
                 UnaryOperation (operation, body)
                 // UNARY operator
            | 1 -> 
                toAST (root.Children.Item 0)
            | err -> failwith (sprintf "This should never be reached: %A" err)
        | "Identifier" ->
            match root.Children.Count with // Identifier can only can only take 2 forms, IDENTIER or IDENTIFIER Accessor 
            | 1 -> Identifier (SimpleIdentifier (root.Children.Item 0).Symbol.Value)
            | 2 -> 
                let ids = Seq.unfold (fun (node:ASTNode) -> 
                                        match node.Children.Count with
                                        | 0 -> None
                                        | _ -> Some ((node.Children.Item 0).Symbol.Value, (node.Children.Item 1))
                                     ) 
                                        root
                          |> List.ofSeq
                Identifier (IdentifierAccessor ids) // Subject to change....
        | "Function" ->
            let funcName = (getChildByIndexes [0;0] root).Symbol.Value // [0;0] is a list of 0 and 0, for accessing child 0,0 which is the identifier, the name of the function
            if root.Children.Count = 3 then // count is 3 when there is no arguments. fx f()
                let args = []
                let types = seq { for c in (root.Children.Item 1).Children do   
                                    yield (c.Children.Item 0)                    
                                }
                            |> List.ofSeq
                            |> List.map toPrimitiveType
                            |> fun xs -> if xs.Length = 1 then xs.Head else ArrowPrimitive xs


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
                            |> fun xs -> if xs.Length = 1 then xs.Head else ArrowPrimitive xs
                let body = toAST (root.Children.Item 3)
                Function (funcName, args, types, body)
        | "Invocation" ->
            let funcName = (getChildByIndexes [0;0] root).Symbol.Value
            if root.Children.Count = 1 then // no parameters                
                Invocation (funcName, [])
            else
                let parameters = seq { for c in (root.Children.Item 1).Children do
                                        let rawParam = (getChildByIndexes [0;0] c).Symbol.Value
                                        // trim quotation marks at start and end if string
                                        let param = if rawParam.StartsWith "\"" && rawParam.EndsWith "\"" then 
                                                        rawParam.Substring(1,rawParam.Length-2)
                                                    else
                                                        rawParam
                                        yield param          
                                     }
                                 |> List.ofSeq
                Invocation (funcName, parameters)
        | "StructLiteral" ->
            let fields = seq { for c in root.Children do
                                let fieldName1 = (getChildByIndexes [0;0] c).Symbol.Value
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
        | "Return" ->
            let expr = toAST (root.Children.Item 0)
            Return expr
        | "Kill" ->
            let expr = toAST (root.Children.Item 0)
            Kill expr
        | "Me" ->
            Me
        | sym -> 
            printfn "ERROR: No match case for: %A" sym
            failwith "not all cases matched in toAST"

    and traverseChildren (root:ASTNode) : AST list =
        [for i in root.Children -> i]
            |> List.map toAST // Call toAST for all children and return the list of all children
