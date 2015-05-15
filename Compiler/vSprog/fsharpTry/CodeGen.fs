namespace vSprog

open vSprog.AST
open vSprog.CommonTypes
open AnalysisUtils

module CodeGen =
    type Value = (string * string * string) // name, type, code
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
            return (sprintf "%%_%d" newEnv.regCounter)
        }

//    let append (code:string) : State<unit, Environment> =
//        state {
//            let! st = getState
//            let newEnv = {st with genString = st.genString + code}
//            do! putState newEnv
//        }

    let addRegister (regName:string) (regType:string) : State<unit, Environment> =
        state {
            let! st = getState
            let newEnv = {st with registers = st.registers.Add(regName, regType)}
            do! putState newEnv
        }

    let newRegister (regName:string) (regType:string) (rhs:string) : State<Value, Environment> =
        state {
            let code = sprintf "%s = %s" regName rhs
            do! addRegister regName regType
            return (regName, regType, code)
        }

    let findRegister (regName:string) : State<string, Environment> =
        state {
            let! st = getState
            return Map.find regName st.registers
        }
    
    let argsStruct = SimplePrimitive
                      (
                        Primitive.Struct ("args",
                         [
                           ("argv", ListPrimitive (ListPrimitive (SimplePrimitive Primitive.Char, 128), 128)); // TODO: vi bliver nødt til at hard code længden af lister
                           ("argc", SimplePrimitive Int)
                         ])
                      )

    let genLoad targetName sourceType sourceName : State<Value, Environment> =
        state {
            let code = sprintf "load %s %s" sourceType sourceName
            let loadedType = sourceType.Substring(0, sourceType.Length-1) // remove 1 pointer from type
            let! res = newRegister targetName loadedType code
            return res
        }

    let genLoadString targetName ((strName, strType, strCode):Value) : State<Value, Environment> =
        state {
            let code = sprintf "getelementptr %s %s, i64 0, i64 0" strType strName
            let! res = newRegister targetName "i8*" code
            return res
        }

    let genAlloca (targetName:string) type' : State<Value, Environment> = 
        state {
            let lhs = if targetName.StartsWith("%_") then // only prepend % when targetName is named e.g. "x"
                        targetName
                      else
                        sprintf "%c%s" '%' targetName
            let code = sprintf "alloca %s" type'
            let lhsType = sprintf "%s*" type'

            let! res = newRegister lhs lhsType code
            return res
        }

    let genStore sourceName sourceType targetName targetType : State<Value, Environment> = 
        state {
            let code = sprintf "store %s %s, %s %s" sourceType sourceName targetType targetName
            //do! append code
            return ("", "", code)
        }

    let icmpString binOp sLhsType sLhsName sRhsName : string =
        let op = match binOp with
                 | LessThan -> "slt"
                 | LessThanOrEq -> "sle"
                 | GreaterThan -> "sgt"
                 | GreaterThanOrEq -> "sge"
                 | Equals -> "eq"
                 | NotEquals -> "ne"
        sprintf "icmp %s %s %s, %s" op sLhsType sLhsName sRhsName

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
            let fullString = sprintf "%s %s" regType strName
            return (strName, regType, fullString)
        }

    let rec genType (type':PrimitiveType) : string = 
        match type' with
        | SimplePrimitive p ->
            match p with
            | Int -> "i32"
        | ListPrimitive (prim, len) ->
            sprintf "[%d x %s]" len (genType prim)
        
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

    // applies a state workflow to all elements in list
    let rec collectAll (p:('a ->State<'b,'c>)) (list:'a list) : State<'b list, 'c> =
        state {
            match list with
            | [] -> return []
            | x::xs ->
                let! x1 = p x
                let! xs1 = collectAll p xs
                return x1 :: xs1
              }

    let rec internalCodeGen (ast:AST) : State<Value, Environment> =
        match ast with
        | Program stms | Block stms | Body stms ->
          state {
              let! fullStringValues = collectAll internalCodeGen stms
              let fullString = fullStringValues |> List.map (fun (_,_,body) -> body) |> String.concat "\n"

              return ("","", fullString)
          }

        | Reassignment (varId, rhs) as reass ->
          state {
              let sId = match varId with
                        | SimpleIdentifier id -> id
                        | _ -> failwith "Initialisation should only contain a simpleIdentifier as name"
              let! (toStoreName, toStoreType, code) = internalCodeGen rhs
              let toUpdateName = sId
              let! toUpdateType = findRegister toUpdateName
              let! (_,_,storeCode) = genStore toStoreName toStoreType toUpdateName toUpdateType
              let fullString = sprintf "%s\n%s" code storeCode
              return ("","", fullString)
          }
        | Initialisation (lvalue, rhs) -> // TODO: vi kan bruge 'contant' attribut til 'let' bindings
          state {
              let sId = match lvalue.identity with
                        | SimpleIdentifier id -> id
                        | _ -> failwith "Initialisation should only contain a simpleIdentifier as name"
              let sType = genType lvalue.primitiveType
              let! (idPtrName, idPtrType, allocCode) = genAlloca sId sType
              let! (toStoreName, toStoreType, rhsCode) = internalCodeGen rhs
              let! (_,_,storeCode) = genStore toStoreName toStoreType idPtrName idPtrType
              let fullString = sprintf "%s\n%s\n%s" allocCode rhsCode storeCode
              return (idPtrName, idPtrType, fullString)
          }

        | Actor (name, body) -> // TODO: non-main actors, argumenter til main receive
          state {
              let! res = genActorDefine (sprintf "_actor_%s" name) "i8*" [] body
//            match name with
//            | "main" -> 
//                let receive = match findMainReceive body with
//                              | Some a -> a
//                              | None -> failwith "Could not find a receive method in main actor accepting arguments"
//              
//            
//                let! res = genFunctionDefine "main" (SimplePrimitive Int) [] receive
              return res
          }

        | Struct (name, fields) ->
          state {
              return ("","", "")
          }
        | Constant (ptype, value)-> 
          state {
              match value with
              | PrimitiveValue.Int n -> 
                let name = string n
                let type' = genType ptype
                return (name, type', "")
          }

        | If (condition, body) -> 
          state {
              let! (condName, condType, condCode) = internalCodeGen condition
              let coName = condName.[1..];
              //do! append (sprintf "br i1 %s, label %%ifTrue%s, label %%cont%s \n" condName coName coName)
              let brStr = sprintf "br i1 %s, label %%ifTrue%s, label %%cont%s" condName coName coName


              //let contCode = append (sprintf "br label %%cont%s\n" coName) // to jump to a continuation label
              let! trueBody  = genLabel (sprintf "ifTrue%s" coName) body
              let! _ = freshReg // TODO: always increment regCounter after a termination of a block. Here br
              let contCode = sprintf "br label %%cont%s" coName // to jump to a continuation label

              //do! append (sprintf "cont%s:\n" coName) // to allow for code after if-else
              let contLabel = (sprintf "cont%s:\n" coName) // to allow for code after if
              let fullString = sprintf "%s\n%s\n%s\n%s\n%s" condCode brStr trueBody contCode contLabel
              return ("","", fullString)
          }
        | IfElse (condition, trueBody, falseBody) ->
          state {
              let! (condName, condType, condCode) = internalCodeGen condition
              let coName = condName.[1..];
              //do! append (sprintf "br i1 %s, label %%ifTrue%s, label %%cont%s \n" condName coName coName)
              let brStr = sprintf "br i1 %s, label %%ifTrue%s, label %%ifFalse%s" condName coName coName

              //let contCode = append (sprintf "br label %%cont%s\n" coName) // to jump to a continuation label
              let! trueBodyCode  = genLabel (sprintf "ifTrue%s" coName) trueBody
              let! _ = freshReg // TODO: always increment regCounter after a termination of a block. Here br
              let contCode = sprintf "br label %%cont%s" coName // to jump to a continuation label

              let! falseBodyCode  = genLabel (sprintf "ifFalse%s" coName) falseBody
              let! _ = freshReg // TODO: always increment regCounter after a termination of a block. Here br
              let contCode = sprintf "br label %%cont%s" coName // to jump to a continuation label

              //do! append (sprintf "cont%s:\n" coName) // to allow for code after if-else
              let contLabel = (sprintf "cont%s:\n" coName) // to allow for code after if
              let fullString = sprintf "%s\n%s\n%s\n%s\n%s\n%s\n%s" condCode brStr trueBodyCode contCode falseBodyCode contCode contLabel
              return ("","", fullString)
          }
        | Send (actorName, msgName) -> 
          state {
              return ("","", "")
          }
        | Spawn (lvalue, actorName, initMsg) -> 
          state {
              return ("","", "")
          }
        | Receive (msgName, msgType, body) -> 
          // TODO: lige nu gør vi ikke det rigtige ved receive. Vi genererer bare for body. SKal vi ikke gøre mere?
          internalCodeGen body
        | ForIn (elem, list', body) -> 
          state {
              let! idxCounterRegName = freshReg
              let idxCounterLValue = {identity = SimpleIdentifier idxCounterRegName; isMutable = true; primitiveType = SimplePrimitive Int}
              let! (idxCounterVal, idxCounterType, idxCounterCode) = internalCodeGen (Initialisation (idxCounterLValue, Constant (SimplePrimitive Int, PrimitiveValue.Int 0)))

              let (elemType, listLength) = match list' with
                                              | ListRange (contents, ListPrimitive (ptype, n)) -> (ptype, n)
                                              | Identifier (_, ListPrimitive (ptype, n)) -> (ptype, n)
              
              let conditionAST = BinOperation (
                                   Identifier (SimpleIdentifier idxCounterRegName, SimplePrimitive Int), 
                                   NotEquals, 
                                   Constant (SimplePrimitive Int, PrimitiveValue.Int listLength)
                                 ) 
              let incrementCounter = BinOperation (
                                       Identifier (SimpleIdentifier idxCounterRegName, elemType),
                                       Plus,
                                       Constant (SimplePrimitive Int, PrimitiveValue.Int 1)
                                     )

              let reassCounter = Reassignment (
                                       SimpleIdentifier idxCounterRegName,
                                       incrementCounter
                                       )

              let! tempElemAssign = freshReg
              let! (_,_,allocatedTempElemAssign) = genAlloca tempElemAssign (genType elemType)
             
              // TODO: assign element til elem
              //let elemAssign = Reassignment (SimpleIdentifier elem, Identifier (SimpleIdentifier tempElemAssign, elemType)) // TODO

              let bodyAST = Body [ body; reassCounter]
              let whileCodeAST = While (conditionAST, bodyAST)

              let declElemCodeLValue = {identity = SimpleIdentifier elem; isMutable = true; primitiveType = elemType}
              let declElemDefaultVal = match elemType with
                                       | SimplePrimitive Int -> PrimitiveValue.Int 0
              let declElemCodeAST = Initialisation (declElemCodeLValue, Constant (elemType, declElemDefaultVal))
              let! (_,_, declElemCode) = internalCodeGen declElemCodeAST
              let! (_,_,whileCode) = internalCodeGen whileCodeAST
              let fullString = sprintf "%s\n%s\n%s\n" idxCounterCode declElemCode whileCode
              return ("","",fullString)
          }
        | ListRange (contents, pType) -> 
          state {
              let! test = collectAll internalCodeGen contents
              let listContent = test
                                |> List.map (fun (name, typ, code) -> sprintf "%s %s" typ name)
                                |> String.concat ", "
              let targetType = genType pType
              let name = sprintf "[ %s ]"listContent
              let fullString = sprintf "%s %s" name targetType
              return (name , targetType, "")
          }
        | BinOperation (lhs, op, rhs) -> 
          state {
              // TODO: OBS. lige nu er det altid af type i32 Typen for hele BinOperation skal bruges her. Indsæt det f.eks. i træet
              // TODO: support for floats
              let! (sLhsName, sLhsType, lhsCode) = internalCodeGen lhs
              let! (sRhsName, sRhsType, rhsCode) = internalCodeGen rhs
              let! tempReg = freshReg
              let (code, targetType) = 
                  match op with
                  | Multiply -> 
                    sprintf "mul %s %s, %s" sLhsType sLhsName sRhsName, sLhsType
                  | Plus ->
                    sprintf "add %s %s, %s" sLhsType sLhsName sRhsName, sLhsType
                  | Minus ->
                    sprintf "sub %s %s, %s" sLhsType sLhsName sRhsName, sLhsType
                  | Divide ->
                    sprintf "sdiv %s %s, %s" sLhsType sLhsName sRhsName, sLhsType
                  | Or -> 
                    sprintf "or %s %s, %s" sLhsType sLhsName sRhsName, sLhsType
                  | And -> 
                    sprintf "and %s %s, %s" sLhsType sLhsName sRhsName, sLhsType
                  | GreaterThan | GreaterThanOrEq | LessThan | LessThanOrEq | Equals | NotEquals -> 
                    (icmpString op sLhsType sLhsName sRhsName, "i1")
              let! (resName, resType, resCode) = newRegister tempReg targetType code
              let fullString = sprintf "%s\n%s\n%s\n" lhsCode rhsCode resCode
              return (resName, resType, fullString)
          }

          
        | Identifier (id, pType) -> 
            // TODO: vi skal bruge typen på identifier her..
          state {
              match id with
              | SimpleIdentifier str -> 
                let! regName = freshReg
                let toLoadName = sprintf "%%%s" str
                let! toLoadType = findRegister toLoadName
                let! loadedValue = genLoad regName toLoadType toLoadName
                return loadedValue
          }
        | Function (funcName, arguments, types, body) -> 
          state {
              return ("","", "")
          }
        | StructLiteral fieldNamesAndVals -> 
          state {
              return ("","", "")
          }
        | Invocation (functionName, parameters, functionType) -> 
          state {
              match functionName with
              | "print" ->
                let stringToPrint = parameters.Head
                let! (strName, strType, strCode) as str = declareStringConstant stringToPrint
                let! regName = freshReg
                let! (loadedStringName, loadedStringType, loadCode) = genLoadString regName str
                //do! append (sprintf "call i32 @puts(%s %s)\n" loadedStringType loadedStringName)
                let putsCode = sprintf "call i32 @puts(%s %s)" loadedStringType loadedStringName
                let fullString = sprintf "%s\n%s" loadCode putsCode
                return ("","", fullString)
          }
        | UnaryOperation (op, rhs) -> 
          state {
              return ("","", "")
          }
        | While (condition, body) ->
          state {
              let! (condName, condType, condCode) = internalCodeGen condition
              let coName = condName.[1..]
              let brCode = sprintf "br label %%switch%s\n" coName

              let switcCode = sprintf "switch%s: \n %s"  coName condCode
              let brStr = sprintf "br i1 %s, label %%body%s, label %%cont%s\n" condName coName coName

              let! (_,_,bodyCode) = internalCodeGen body

              let bodyLabel = sprintf "body%s: \n" coName

              let bodyCodeString = sprintf  "%s\n br label %%switch%s\n" bodyCode coName


              let con = sprintf "\ncont%s:\n" coName


              let fullString = brCode + switcCode + brStr + bodyLabel + bodyCodeString + con

              return ("","", fullString)  
          }
        | Kill (arg) -> 
          state {
              return ("","", "")
          }
        | Me -> 
          state {
              return ("","", "")
          }
        | Return (arg) -> 
          state {
              return ("","", "")
          }

    and genActorDefine (name:string) (retType:string) (args:TypeDeclaration list) (body:AST) : State<Value, Environment> =
        state {
            let sArgumentList = args
                                |> List.map (fun
                                              (argName, argType) -> sprintf "%s %s" argName (genType argType)
                                            )
                                |> String.concat ", "
            //do! append (sprintf "define %s @%s(%s) {\n" sRetType name sArgumentList)
            let defineCode = sprintf "define %s @%s(%s) {\n" retType name sArgumentList

            let! (_,_,bodyCode) = internalCodeGen body
//            do! append (if name = "main" then 
//                          "ret i32 0\n}"
//                        else "}")
            let mainHackCode = "ret i8* null\n}"
                               
            let fullString = sprintf "%s\n%s\n%s" defineCode bodyCode mainHackCode
            return (name, retType, fullString)
        }

    and genLabel labelName body : State<string, Environment> =
        state {
            let label = sprintf "%s:" labelName
            //do! append code
            let! (bodyName, bodyType, bodyCode) = internalCodeGen body
            let fullString = sprintf "%s\n%s" label bodyCode
            return fullString
        }

    let codeGen (ast:AST) : string =
        let ((_,_,fullString), env) = (runState (internalCodeGen ast) {regCounter = 0; genString =""; registers = Map.empty; globalVars = []})
        let globals = env.globalVars
                      |> List.map
                        (fun (varName, varType, initVal) -> 
                          sprintf "%s = constant %s c\"%s\00\"\n" varName varType initVal)
                      |> String.concat ""
        let externalFunctions = String.concat "" ["declare i32 @puts(i8*)\n"
                                                 ;"declare void @actor_init(...)\n"
                                                 ;"declare void @actor_wait_finish(...)\n"
                                                 ;"declare void @actor_destroy_all(...)\n"
                                                 ;"%struct.actor_message_struct = type { %struct.actor_message_struct*, i64, i64, i64, i8*, i64}\n"
                                                 ;"%struct.actor_main = type { i32, i8** }\n"
                                                 ;"declare void @exit(i32)\n"
                                                 ;"declare i64 @spawn_actor(i8* (i8*)*, i8*)\n"
                                                 ;"declare void @actor_send_msg(i64, i64, i8*, i64)\n"]
          
        let main = """define i32 @main(i32 %argc, i8** %argv) {
  %1 = alloca i32
  %2 = alloca i32
  %3 = alloca i8**
  store i32 0, i32* %1
  store i32 %argc, i32* %2
  store i8** %argv, i8*** %3
  call void (...)* @actor_init()
  %4 = call i64 @spawn_actor(i8* (i8*)* bitcast (i8* ()* @_actor_main to i8* (i8*)*), i8* null)
  call void (...)* @actor_wait_finish()
  call void (...)* @actor_destroy_all()
  call void @exit(i32 0)
  unreachable
                                                  ; No predecessors!
  %6 = load i32* %1
  ret i32 %6
}
                """
                                                                       
        externalFunctions + globals + main + fullString
