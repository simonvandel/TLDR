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
        //| Function of PrimitiveType
        | Struct of string * (TypeDeclaration list)

    and PrimitiveType =
        | SimplePrimitive of Primitive
        | ListPrimitive of PrimitiveType * int // typeOfElements, length
        | ArrowPrimitive of PrimitiveType list
        | TupleType of PrimitiveType list
        | UserType of string
        | HasNoType

    and TypeDeclaration = string * PrimitiveType // name, type. Example: fieldName:int

    and AST = 
        | Program of AST list
        | Block of AST list
        | Body of AST list
        | Reassignment of Identifier * AST // varId, rhs
        | Initialisation of LValue * AST // lvalue, rhs
        | Constant of PrimitiveType * PrimitiveValue // type, value
        | Actor of string * AST // name, body FIXME: Add more fields?
        | Struct of string * (TypeDeclaration list) // name, fields
        | If of AST * AST // conditional, body
        | IfElse of AST * AST * AST // conditional, trueBody, falseBody
        | Send of string * string * AST // actorHandle, actorToSendTo, msg
        | Spawn of LValue * (string * AST option) option // lvalue, (actorName, initMsg) or uninit spawn
        | Receive of string * PrimitiveType * AST // msgName, msgType, body
        | ForIn of string * AST * AST // counterName, list, body
        | While of AST * AST // condition, body
        | List of AST list * PrimitiveType // content, type
        | BinOperation of AST * BinOperator * AST // lhs, op, rhs
        | UnaryOperation of UnaryOperator * AST // op, rhs
        | Identifier of Identifier * PrimitiveType // id, typeOfId
        | Function of string * string list * PrimitiveType * AST// funcName, arguments, types, body
        | StructLiteral of AST * (string * AST) list // struct, (fieldName, fieldValue) list
        | Invocation of string * string list * PrimitiveType // functionName, parameters, functionSignature
        | Tuple of AST list * PrimitiveType // Entries
        | Return of AST option // body
        | Die

    and Identifier =
        | SimpleIdentifier of string // x == "x"
        | IdentifierAccessor of string * AST // x.y == ("x", Identifier (SimpleIdentifier "y")))
                                             // x.[5] == ("x", Constant 5)

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
                
            | "ListType" -> ListPrimitive (toPrimitiveType (input.Children.Item 0), 0) // TODO: lige nu bliver det bare gemt at listen er 0 lang
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
                    | Identifier (id, _) -> id)
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

    let flatten listlist = [for lst in listlist do yield! lst]
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
            let mutability = (root.Children.Item 0)
            match (root.Children.Item 1).Symbol.Value with
            | "FuncDecl" ->
              let funcDecl = toAST (root.Children.Item 1)
              funcDecl
            | "SymDecl" ->  
              let (name, typeName) = symDeclParse (root.Children.Item 1)
              let lhs = toLValue mutability name typeName
              let rhs = toAST (root.Children.Item 2)
              Initialisation (lhs, rhs)
        | "Reassignment" ->
            let assignables = 
                match toAST (root.Children.Item 0) with
                | Identifier (id,_) -> id
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
        | "While" ->
            let condition = toAST (root.Children.Item 0)
            let body = toAST (root.Children.Item 1)
            While (condition, body)
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
                | "SymDecl" ->
                  let (fieldName, typeNode) = symDeclParse (root.Children.Item 1)
                  let primType = toPrimitiveType typeNode
                  match fieldName with
                  | Identifier (SimpleIdentifier id, _) ->
                    Struct (name, [(id, primType)])
                | "TypeDecls" ->
                  let blocks = 
                      [
                        for c in (root.Children.Item 1).Children do
                            let (fieldName, typeNode) = symDeclParse c
                            let primType = toPrimitiveType typeNode
                            match fieldName with
                            | Identifier (SimpleIdentifier id, _) ->
                              yield (id, primType)
                      ]
                  Struct (name, blocks)
                | err -> failwith (sprintf "This should never be reached: %s" err)
        | "Send" ->
            let actorHandle = (getChildByIndexes [0;0] root).Symbol.Value
            let msg = toAST (getChildByIndexes [1;0] root)
            Send (actorHandle, "", msg) // we do not know the name of the actor to send to at parse t
        | "Spawn" ->
            let mutability = (root.Children.Item 0)
            let name = toAST (getChildByIndexes [1;0] root)
            let typeName = (getChildByIndexes [1;1;0;0] root)
            let lhs = toLValue mutability name typeName
            match (root.Children.Count) with
            | 2 ->
                Spawn (lhs, None)
            | _ ->
                let actorName = (getChildByIndexes [2;0] root).Symbol.Value
                if root.Children.Count = 4 then // msg was supplied
                    let initMsg = toAST (getChildByIndexes [3;0] root)
                    Spawn (lhs, Some (actorName, Some initMsg))
                else // no msg was supplied
                    Spawn (lhs, Some (actorName, None))
                
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
        | "List" ->
            let (fields:AST list) =  [ for c in root.Children do
                                        match c.Symbol.Value with
                                        | ".." -> 
                                                let start = int (getChildByIndexes [0;0;0] c).Symbol.Value
                                                let end' = int (getChildByIndexes [1;0;0] c).Symbol.Value
                                                if start < end' then
                                                    yield List ([start..end'] |> List.map (fun n -> Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int n)), ListPrimitive (SimplePrimitive Int, end' - start + 1))
                                                else
                                                    yield List ([start..(-1)..end'] |> List.map (fun n -> Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int n)), ListPrimitive (SimplePrimitive Int, end' - start + 1))
                                        | "Expression" -> 
                                                yield toAST c
                                        | err -> failwith (sprintf "This should never be reached: %A" err)
                                    ]
            let mutable res = []
            for i in fields do
                match i with
                | List(x::xs, _) as lst -> res <- res @ x::xs
                | List([], _) -> [] |> ignore
                | other -> res <- res @ [other]

            let mutable typeoflst = HasNoType
            match res.[0] with
            | Constant (ptype, _) -> typeoflst <- ptype
            | _ -> [] |> ignore
            List(res,ListPrimitive(typeoflst,List.length res))
        | ("OP1" | "OP2" | "OP3" | "OP4" | "OP5" | "OP6" | "OP7" | "Expression") ->
            match root.Children.Count with
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
            match root.Children.Count with // Identifier can only can only take 2 forms, IDENTIFIER or IDENTIFIER Accessor 
            | 1 -> Identifier (SimpleIdentifier (root.Children.Item 0).Symbol.Value, HasNoType)
            | 2 -> 
//                let ids = Seq.unfold (fun (node:ASTNode) -> 
//                                        match node.Children.Count with
//                                        | 0 -> None
//                                        | _ ->
//                                          let nodeRes = if node.Children.Count = 0 then
//                                                          (node.Children.Item 0).Symbol.Value
//                                                        else
//                                                          
//                                          Some (nodeRes, (node.Children.Item 1))
//                                     ) 
//                                        root
//                          |> List.ofSeq
                let baseAccessor = (root.Children.Item 0).Symbol.Value
                let nextElem = toAST (root.Children.Item 1)
                Identifier (IdentifierAccessor (baseAccessor, nextElem), HasNoType) // Subject to change....
            | err -> failwith (sprintf "This should never be reached (\"Identifier\" in toAST): %A" err)
        | "FuncDecl" ->
            let funcName = (getChildByIndexes [0;0] root).Symbol.Value // [0;0] is a list of 0 and 0, for accessing child 0,0 which is the identifier, the name of the function
            if root.Children.Count = 2 then // count is 2 when there is no arguments. fx f()
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
            let symName = (getChildByIndexes [0;0] root).Symbol.Value
            match root.Children.Count with
            | 1 -> // identifier
              toAST (root.Children.Item 0)
            | _ -> // function
                if root.Children.Count = 3 then // no parameters                
                    Invocation (symName, [], ArrowPrimitive [])
                else // there are root.Children.Count - 3 parameters
                    let parameters = seq { for childNum in [2.. root.Children.Count - 2] do
                                           // we do not know if the parameter is a variable or a string. Try the deepest level (variable) first
                                           // if it fails, it must be a string parameter

                                           let rawParam =
                                               try (getChildByIndexes [childNum;0;0;0] root).Symbol.Value
                                               with
                                               | :? System.IndexOutOfRangeException -> 
                                                 (getChildByIndexes [childNum;0;0] root).Symbol.Value

                                           // trim quotation marks at start and end if string
                                           if rawParam.StartsWith "\"" && rawParam.EndsWith "\"" then 
                                             yield rawParam.Substring(1,rawParam.Length-2)
                                           else
                                             yield rawParam
                                         }
                                     |> List.ofSeq
                    Invocation (symName, parameters, HasNoType)
        | "StructLiteral" ->
            let fields = [for c in root.Children do
                                let fieldName = (getChildByIndexes [0;0] c).Symbol.Value
                                let fieldValue = toAST (c.Children.Item 1)
                                yield (fieldName, fieldValue)               
                          ]
            StructLiteral (Program [], fields) // we do not know which struct we are creating yet
        | "Tuple" ->
            let fields = traverseChildren root
            let mutable (types:PrimitiveType list) = []
            for f in fields do
                match f with
                | Constant (ptype, _) -> types <- types @ [ptype]
                | Identifier (_, ptype) -> types <- types @ [ptype]
                | _ -> failwith "Tuples can only contain constants and identifiers"
            let tupleType = TupleType types
            Tuple (fields, tupleType)
        | "String" ->

            let (chars:PrimitiveValue list) = (root.Children.Item 0).Symbol.Value
                                              |> fun str -> 
                                                // remove quotation marks from string
                                                str.Substring (1, (str.Length - 2))
                                              |> Seq.map PrimitiveValue.Char
                                              |> List.ofSeq
            Constant (ListPrimitive (SimplePrimitive Primitive.Char, chars.Length), PrimitiveValue.List chars)
        | "Return" ->
            if root.Children.Count = 1 then
              let expr = toAST (root.Children.Item 0)
              Return (Some expr)
            else
              Return None
        | "Die" ->
            Die
        | sym -> 
            printfn "ERROR: No match case for: %A" sym
            failwith "not all cases matched in toAST"

    and traverseChildren (root:ASTNode) : AST list =
        [for i in root.Children -> i]
            |> List.map toAST // Call toAST for all children and return the list of all children

    and symDeclParse (node:ASTNode) : (AST * ASTNode) =
        let name = toAST (node.Children.Item 0)
        let typeName = (node.Children.Item 1)
        (name, typeName)