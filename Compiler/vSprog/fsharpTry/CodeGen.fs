namespace vSprog

open vSprog.AST
open vSprog.CommonTypes
open AnalysisUtils

module CodeGen =
    type Value = (string * string) // name, type
    type Environment = {
        regCounter:int // starts at 0, and increments every time a new register is used. Resets on new funcition
        genString:string // the code generated
        registers:Map<string,string> // key is regName, value is regType
        globalVars:(string * string * string) list // name, type, initialValue 
        }

    let freshReg : State<string, Environment> =
        state {
            let! st = getState
            let newEnv = {st with regCounter = st.regCounter + 1}
            do! putState newEnv
            return (sprintf "%c%d" '%' newEnv.regCounter)
        }

    let append (code:string) : State<unit, Environment> =
        state {
            let! st = getState
            let newEnv = {st with genString = st.genString + code}
            do! putState newEnv
        }

    let addRegister (regName:string) (regType:string) : State<unit, Environment> =
        state {
            let! st = getState
            let newEnv = {st with registers = st.registers.Add(regName, regType)}
            do! putState newEnv
        }

    let newRegister (regName:string) (regType:string) (rhs:string) : State<Value, Environment> =
        state {
            let code = sprintf "%s = %s" regName rhs
            do! append code
            do! addRegister regName regType
            return (regName, regType)
        }

    let findRegister (regName:string) : State<string, Environment> =
        state {
            let! st = getState
            return Map.find regName st.registers
        }
    
    let argsStruct = SimplePrimitive 
                      (
                        Primitive.Struct ("args",  [
                          ("argv", ListPrimitive (ListPrimitive (SimplePrimitive Primitive.Char)));
                          ("argc", SimplePrimitive Int)
                         ])
                      )

    let genLoad targetName sourceType sourceName : State<Value, Environment> =
        state {
            let code = sprintf "load %s %s \n" sourceType sourceName
            let loadedType = sourceType.Substring(0, sourceType.Length-1) // remove 1 pointer from type
            let! res = newRegister targetName loadedType code
            return res
        }

    let genLoadString targetName ((strName, strType):Value) : State<Value, Environment> =
        state {
            let code = sprintf "getelementptr %s %s, i64 0, i64 0\n" strType strName
            let! res = newRegister targetName "i8*" code
            return res
        }

    let genAlloca targetName type' : State<Value, Environment> = 
        state {
            let lhs = sprintf "%c%s" '%' targetName
            let code = sprintf "alloca %s \n" type'
            let lhsType = sprintf "%s*" type'

            let! res = newRegister lhs lhsType code
            return res
        }

    let genStore sourceName sourceType targetName targetType : State<unit, Environment> = 
        state {
            let code = sprintf "store %s %s, %s %s \n" sourceType sourceName targetType targetName
            do! append code
        }

    let icmpString binOp sLhsType sLhsName sRhsName : string =
        let op = match binOp with
                 | LessThan -> "slt"
                 | LessThanOrEq -> "sle"
                 | GreaterThan -> "sgt"
                 | GreaterThanOrEq -> "sge"
                 | Equals -> "eq"
                 | NotEquals -> "ne"
        sprintf "icmp %s %s %s, %s\n" op sLhsType sLhsName sRhsName

    let declareStringConstant (str:string) : State<Value, Environment> =
        state {
            let! st = getState
            let strSize = str.Length + 1 // 1 extra for \00
            // find new name for global var string. 
            // TODO: der bruges en lidt dyr metode til at finde sidste element O(n)
            let strName = if st.globalVars.Length = 0 then
                            "@g1"
                          else
                            st.globalVars
                            |> List.rev 
                            |> List.head 
                            |> fun (name,_,_) -> name  + "1"
            let strType = sprintf "[%d x i8]" strSize
            let entry = (strName, strType, str)
            let newEnv = {st with globalVars = entry :: st.globalVars}
            do! putState newEnv
            let regType = sprintf "%s*" strType
            return (strName, regType)
        }

    let genType (type':PrimitiveType) : string = 
        match type' with
        | SimplePrimitive p ->
            match p with
            | Int -> "i32"
        
    let rec findMainReceive (ast:AST) : AST option =
        match ast with
        | Block stms | Body stms ->
          stms
          |> List.map findMainReceive
          |> List.filter Option.isSome // 'Some' means that the main receive was found
          |> List.head
        | Receive ("arguments", argsStruct, _) as mainReceive -> 
            Some mainReceive
        | _ -> None

    let rec applyAll (l:'a list) (p:'a->State<'b, Environment>) : State<'b, Environment> = 
        state {
            match l with
            | [x] -> 
                let! res = p x
                return res
            | x::xs -> 
                let! _ = p x
                let! res = applyAll xs p
                return res
        }


    let rec internalCodeGen (ast:AST) : State<Value, Environment> =
        match ast with
        | Program stms | Block stms | Body stms ->
          state {
              let! test = applyAll stms internalCodeGen
              //let! test = internalCodeGen stms.Head
              return test
          }

        | Reassignment (varId, rhs) as reass ->
          state {
              let sId = match varId with
                        | SimpleIdentifier id -> id
                        | _ -> failwith "Initialisation should only contain a simpleIdentifier as name"
              let! (toStoreName, toStoreType) = internalCodeGen rhs
              let toUpdateName = sprintf "%c%s" '%' sId
              let! toUpdateType = findRegister toUpdateName
              do! genStore toStoreName toStoreType toUpdateName toUpdateType
              return ("","")
          }
        | Initialisation (lvalue, rhs) -> // TODO: vi kan bruge 'contant' attribut til 'let' bindings

          state {
              let sId = match lvalue.identity with
                        | SimpleIdentifier id -> id
                        | _ -> failwith "Initialisation should only contain a simpleIdentifier as name"
              let sType = genType lvalue.primitiveType
              let! (idPtrName, idPtrType) as idPtr = genAlloca sId sType
              let! (toStoreName, toStoreType) = internalCodeGen rhs
              do! genStore toStoreName toStoreType idPtrName idPtrType
              return idPtr
          }

        | Actor (name, body) -> // TODO: non-main actors, argumenter til main receive
          state {
            match name with
            | "main" -> 
                let receive = match findMainReceive body with
                              | Some a -> a
                              | None -> failwith "Could not find a receive method in main actor accepting arguments"
              
            
                let! _ = genFunctionDefine "main" (SimplePrimitive Int) [] receive
                return ("","")
          }

        | Struct (name, fields) ->
          state {
              return ("","")
          }
        | Constant (ptype, value)-> 
          state {
              match value with
              | PrimitiveValue.Int n -> return (string n, genType ptype)
          }

        | If (condition, body) -> 
          state {
              return ("","")
          }
        | IfElse (condition, trueBody, falseBody) ->
          state {
              let! (condName, condType) = internalCodeGen condition
              do! append (sprintf "br i1 %s, label %cifTrue, label %cifFalse \n" condName '%' '%')

              let contCode = append "br label %cont\n" // to jump to a continuation label
              do! genLabel "ifTrue" trueBody
              do! contCode
              let! _ = freshReg // TODO: always increment regCounter after a termination of a block. Here br
              do! genLabel "ifFalse" falseBody
              do! contCode
              do! append "cont:\n" // to allow for code after if-else
              return ("","")
          }
        | Send (actorName, msgName) -> 
          state {
              return ("","")
          }
        | Spawn (lvalue, actorName, initMsg) -> 
          state {
              return ("","")
          }
        | Receive (msgName, msgType, body) -> 
          // TODO: lige nu gør vi ikke det rigtige ved receive. Vi genererer bare for body. SKal vi ikke gøre mere?
          internalCodeGen body
        | ForIn (counterName, list, body) -> 
          state {
              return ("","")
          }
        | ListRange content -> 
          state {
              return ("","")
          }
        | BinOperation (lhs, op, rhs) -> 
          state {
              // TODO: OBS. lige nu er det altid af type i32 Typen for hele BinOperation skal bruges her. Indsæt det f.eks. i træet
              // TODO: support for floats
              let! (sLhsName, sLhsType) = internalCodeGen lhs
              let! (sRhsName, sRhsType) = internalCodeGen rhs
              let! tempReg = freshReg
              let (code, targetType) = 
                  match op with
                  | Multiply -> 
                    sprintf "mul %s %s, %s\n" sLhsType sLhsName sRhsName, sLhsType
                  | Plus ->
                    sprintf "add %s %s, %s\n" sLhsType sLhsName sRhsName, sLhsType
                  | Minus ->
                    sprintf "sub %s %s, %s\n" sLhsType sLhsName sRhsName, sLhsType
                  | Divide ->
                    sprintf "sdiv %s %s, %s\n" sLhsType sLhsName sRhsName, sLhsType
                  | Or -> 
                    sprintf "or %s %s, %s\n" sLhsType sLhsName sRhsName, sLhsType
                  | And -> 
                    sprintf "and %s %s, %s\n" sLhsType sLhsName sRhsName, sLhsType
                  | GreaterThan | GreaterThanOrEq | LessThan | LessThanOrEq | Equals | NotEquals -> 
                    (icmpString op sLhsType sLhsName sRhsName, "i1")
              let! res = newRegister tempReg targetType code
              return res
          }

          
        | Identifier id -> 
            // TODO: vi skal bruge typen på identifier her..
          state {
              match id with
              | SimpleIdentifier str -> 
                let! regName = freshReg
                let toLoadName = (sprintf "%c%s" '%' str)
                let! toLoadType = findRegister toLoadName
                let! loadedValue = genLoad regName toLoadType toLoadName
                //return (sprintf "%c%s" '%' str, "i32")
                return loadedValue
          }
        | Function (funcName, arguments, types, body) -> 
          state {
              return ("","")
          }
        | StructLiteral fieldNamesAndVals -> 
          state {
              return ("","")
          }
        | Invocation (functionName, parameters) -> 
          state {
              match functionName with
              | "print" ->
                let stringToPrint = parameters.Head
                let! (strName, strType) as str = declareStringConstant stringToPrint
                let! regName = freshReg
                let! (loadedStringName, loadedStringType) = genLoadString regName str
                do! append (sprintf "call i32 @puts(%s %s)\n" loadedStringType loadedStringName)
                return ("","")
          }
        | UnaryOperation (op, rhs) -> 
          state {
              return ("","")
          }
        | While (cond, body) ->
          state {
              return ("","")
          }
        | Kill (arg) -> 
          state {
              return ("","")
          }
        | Me -> 
          state {
              return ("","")
          }
        | Return (arg) -> 
          state {
              return ("","")
          }

    and genFunctionDefine (name:string) (retType:PrimitiveType) (args:TypeDeclaration list) (body:AST) : State<Value, Environment> =
        state {
            let sRetType = genType retType
            let sArgumentList = args
                                |> List.map (fun
                                              (argName, argType) -> sprintf "%s %s" argName (genType argType)
                                            )
                                |> String.concat ", "
            do! append (sprintf "define %s @%s(%s) {\n" sRetType name sArgumentList)

            let! _ = internalCodeGen body
            do! append (if name = "main" then 
                          "ret i32 0\n}"
                        else "}")
            return (name, sRetType)
        }

    and genLabel labelName body : State<unit, Environment> =
        state {
            let code = sprintf "%s:\n" labelName
            do! append code
            let! _ = internalCodeGen body
            return ()
        }

    let codeGen (ast:AST) : string =
        let env = (evalState (internalCodeGen ast) {regCounter = 0; genString =""; registers = Map.empty; globalVars = []})
        let genString = env.genString
        let globals = env.globalVars
                      |> List.map
                        (fun (varName, varType, initVal) -> 
                          sprintf "%s = constant %s c\"%s\00\"\n" varName varType initVal)
                      |> String.concat ""
        let externalFunctions = String.concat "" ["declare i32 @puts(i8*)\n"]
                                
        externalFunctions + globals + genString
