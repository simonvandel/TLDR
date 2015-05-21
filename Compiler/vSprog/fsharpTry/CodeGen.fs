namespace vSprog

open vSprog.AST
open vSprog.CommonTypes
open AnalysisUtils
open System
open System.IO

module CodeGen =
    type Value = (string * string * string) // name, type, code
    type CTypeDeclaration = (string * int * PrimitiveType * string) // name, index, type, llvm type
    type CStruct = (string * CTypeDeclaration list)

    type Environment = {
        regCounter:int // starts at 0, and increments every time a new register is used. Resets on new function
        genString:string // the code generated
        registers:Map<string,string> // key is regName, value is regType
        globalVars:(string * string * string) list // name, type, initialValue
        structs:CStruct list //structs
        actors:Map<string, (int * string) list> // key is actorName, value is receiveLabels
        }

    let freshReg : State<string, Environment> = // Gives us a new register, with unique name given by regCounter+1
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

    let addActorReceiveLabels (actorName:string) (receiveLabels:(int * string) list) : State<unit, Environment> =
        state {
            let! st = getState
            let newSt = {st with actors = st.actors.Add (actorName, receiveLabels)}
            do! putState newSt
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
            let code = sprintf "load %s %s\n" sourceType sourceName
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

    let fcmpString binOp sLhsType sLhsName sRhsName : string =
        let op = match binOp with
                 | LessThan -> "olt"
                 | LessThanOrEq -> "ole"
                 | GreaterThan -> "ogt"
                 | GreaterThanOrEq -> "oge"
                 | Equals -> "oeq"
                 | NotEquals -> "one"
        sprintf "fcmp %s %s %s, %s" op sLhsType sLhsName sRhsName

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
            | Int -> "i64"
            | Bool -> "i1"
            | Real -> "double"
        | ListPrimitive (prim, len) ->
            sprintf "[%d x %s]" len (genType prim)
        | UserType p -> sprintf "%%struct.%s" p

        
    let rec findMainReceive (ast:AST) : AST option =
        match ast with
        | Block stms | Body stms ->
          stms
          |> List.map findMainReceive
          |> List.filter Option.isSome // 'Some' means that the main receive was found
          |> List.head
        | Receive ("arguments", UserType "args", _) as mainReceive -> 
            Some mainReceive
        | _ -> None

    let rec findAllReceives (ast:AST) : AST list =
        match ast with
        | Block stms | Body stms ->
          stms
          |> List.map findAllReceives
          |> List.concat
        | Receive (_, _, _) as mainReceive -> 
            [mainReceive]
        | _ -> []

    let rec findOtherThanReceives (ast:AST) : AST list =
        match ast with
        | Block stms | Body stms ->
          stms
          |> List.map findOtherThanReceives
          |> List.concat
        | Receive (_, _, _) as mainReceive -> 
            []
        | k -> [k]

    let receiveLabelName (types:PrimitiveType) : string =
        (sprintf "rec_%A" types).Replace(' ', '_').Replace('"', '_')//"

    let rec calcReceiveJumps (asts:AST list) : (int * string) list =
        Seq.zip (Seq.unfold (fun x -> Some(x, x + 1)) 102) asts // start from 102. must be above 100. 101 is reserved for die message
        |> Seq.map (fun (idx,ast) ->
            match ast with
            | Receive (name, types, body) -> (idx, receiveLabelName types)
            | _ -> failwith "should only be receive"
            )
        |> List.ofSeq

    let getReverseType (revType:string) : PrimitiveType =
        match revType with
        | "i64" -> SimplePrimitive Int
        | "double" -> SimplePrimitive Real
        | "i1" -> SimplePrimitive Bool

    let getReceiveID (msgGenType:string) (actorName:string) : State<int, Environment> =
        state {
            let! st = getState
            let reverseType = getReverseType msgGenType
            let res = st.actors.Item actorName
                      |> List.find (fun (_, labelName) -> labelName = receiveLabelName reverseType)
                      |> fst
            return res
            
        }

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

    let declareStruct (str:CStruct) : State<Value, Environment> = 
        state {
            let! st = getState
            let structs = st.structs
            let newEnv = {st with structs = str :: st.structs}
            do! putState newEnv
            return ("", "", "")
        }

    let toCstruct (name:string) (types:TypeDeclaration list) : CStruct = 
        let mutable i = 0;
        let mutable list:CTypeDeclaration list = []
        for (x,y) in types do 
            let z = (x, i, y, genType(y))
            list <- list |> List.append [z]
            i <- i + 1
        (name, list)

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
              let toUpdateName = if sId.StartsWith("%") then sprintf "%s" sId 
                                 else sprintf "%%%s" sId
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

        | Actor (name, body) -> // TODO: argumenter til main receive
          state {
              let activeLValue = {identity = SimpleIdentifier "%_active"; isMutable = true; primitiveType = SimplePrimitive Bool}
              let activeInitial = Initialisation (activeLValue , Constant (SimplePrimitive Bool, PrimitiveValue.Bool true))
              let! (_,_,activeBoolCode) = internalCodeGen activeInitial

              let! (msgAllocaName,msgAllocaType,msgAllocaCode) = genAlloca "_msg" "%struct.actor_message_struct*"

              let! codeOtherThanReceivesInActorBodyList = collectAll internalCodeGen (findOtherThanReceives body)
              let codeOtherThanReceivesInActorBody = codeOtherThanReceivesInActorBodyList
                                                     |> List.map (fun (_,_,n) -> n)
                                                     |> String.concat "\n"

              let receivesToGen = findAllReceives body // don't generate code for main receive
                                  |> List.filter (fun ast -> match ast with
                                                             | Receive(_, UserType "args", _) -> false
                                                             | _ -> true
                                                 )
              let receiveLabels = calcReceiveJumps receivesToGen
              do! addActorReceiveLabels name receiveLabels  

              let! (_,_,mainReceive) = if name = "main" then
                                         match findMainReceive body with
                                         | Some a ->
                                           internalCodeGen a
                                         | None -> failwith "Could not find a receive method in main actor accepting arguments"
                                       else
                                         internalCodeGen (Program []) // do not generate anything

              let! conditionalCode = internalCodeGen (Identifier (SimpleIdentifier "%_active", SimplePrimitive Bool))

              let! actorReceiveReg = freshReg
              let! (actorReceiveCallName, actorReceiveCallType, actorReceiveCallCode) = newRegister actorReceiveReg "%struct.actor_message_struct*" "call %struct.actor_message_struct* (...)* @actor_receive()"
              let! (_,_,actorReceiveRes) = genStore actorReceiveCallName actorReceiveCallType msgAllocaName msgAllocaType

              let! loadedMsg = freshReg
              let! (loadedMsgName, loadedMsgType, loadedMsgCode) = genLoad loadedMsg msgAllocaType msgAllocaName

              // look at type of message
              let! msgTypePtr = freshReg
              let getMsgTypeCode = sprintf "%s = getelementptr %%struct.actor_message_struct* %s, i32 0, i32 3" msgTypePtr loadedMsgName

              let! msgTypeReg = freshReg
              let! (msgTypeName,msgTypeType,msgTypeCode) = genLoad msgTypeReg "i64*" msgTypePtr

                              

              let receivesJumps = receiveLabels
                                  //|> (fun xs -> (101, "rec_kill")::xs)
                                  |> List.map (fun (k, t) -> sprintf "i64 %d, label %%%s" k t)
                                  |> String.concat "\n"
              let gotoReceive = sprintf "switch %s %s, label %%start [ %s ]" msgTypeType msgTypeName receivesJumps

              let bodyCode = ("","",sprintf "%s\n%s\n%s\n%s\n%s\n%s\n" actorReceiveCallCode actorReceiveRes loadedMsgCode getMsgTypeCode msgTypeCode gotoReceive)
              let! whileCode = genWhile conditionalCode bodyCode
              let gotoStart = "br label %start"
              let! startLabel = genLabel "start" whileCode

              let returnAfterWhile = "ret i8* null"

              let! receivesCode = collectAll internalCodeGen receivesToGen
              let receivesGeneratedCode = receivesCode
                                          |> List.map (fun (_,_,c) -> c)
                                          |> String.concat "\n"

              //let! (_,_,genBody) = internalCodeGen body
              let fullBody = sprintf "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s" codeOtherThanReceivesInActorBody activeBoolCode msgAllocaCode mainReceive gotoStart startLabel returnAfterWhile receivesGeneratedCode
              let! res = genActorDefine (sprintf "_actor_%s" name) "i8*" [] fullBody
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
              let st = getState
              let x = toCstruct name fields
              let! (_,_,_) = declareStruct x
             
              return ("", "", "");                  
          }
        | Constant (ptype, value)-> 
          state {
              match value with
              | PrimitiveValue.Int n -> 
                let name = string n
                let type' = genType ptype
                return (name, type', "")
              | PrimitiveValue.Bool value ->
                let name = if value then "true" else "false"
                let type' = genType ptype
                return (name, type', "")
              | PrimitiveValue.Real n -> 
                let name = sprintf "%f" n
                let type' = genType ptype
                return (name, type', "")
          }

        | If (condition, body) -> 
          state {
              let! (condName, condType, condCode) = internalCodeGen condition
              let coName = condName.[1..];
              //do! append (sprintf "br i1 %s, label %%ifTrue%s, label %%cont%s \n" condName coName coName)
              let brStr = sprintf "br i1 %s, label %%ifTrue%s, label %%cont%s" condName coName coName

              let! trueBodyCode = (internalCodeGen body)
              //let contCode = append (sprintf "br label %%cont%s\n" coName) // to jump to a continuation label
              let! trueBody  = genLabel (sprintf "ifTrue%s" coName) trueBodyCode
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
              let! trueBodyGen = internalCodeGen trueBody
              let! trueBodyCode = genLabel (sprintf "ifTrue%s" coName) trueBodyGen
              let! _ = freshReg // TODO: always increment regCounter after a termination of a block. Here br
              let contCode = sprintf "br label %%cont%s" coName // to jump to a continuation label

              let! falseBodyGen = internalCodeGen falseBody
              let! falseBodyCode  = genLabel (sprintf "ifFalse%s" coName) falseBodyGen
              let! _ = freshReg // TODO: always increment regCounter after a termination of a block. Here br
              let contCode = sprintf "br label %%cont%s" coName // to jump to a continuation label

              //do! append (sprintf "cont%s:\n" coName) // to allow for code after if-else
              let contLabel = (sprintf "cont%s:\n" coName) // to allow for code after if
              let fullString = sprintf "%s\n%s\n%s\n%s\n%s\n%s\n%s" condCode brStr trueBodyCode contCode falseBodyCode contCode contLabel
              return ("","", fullString)
          }
        | Send (actorHandle, actorToSendTo, msg) -> 
          state {
              printf "%s" actorHandle
              printf "%s" actorToSendTo
              let! (msgGenName, msgGenType, msgGenCode) = internalCodeGen msg
              let! msgId = getReceiveID msgGenType actorToSendTo
              let msgSize = match msgGenType with
                            | "i64" -> "8"
                            | "double" -> "8"

              // allocate a pointer to store the msg
              let! allocMsgReg = freshReg
              let! (allocMsgName,allocMsgType,allocMsgCode) = genAlloca allocMsgReg msgGenType 

              // store msg in pointer
              let! (_,_,storeMsgCode) = genStore msgGenName msgGenType allocMsgName allocMsgType

              // bitcast
              let! bitcastedPtrReg = freshReg
              let! (bitcastedPtrName, bitcastedPtrType, bitcastedPtrCode) = newRegister bitcastedPtrReg "i8*" (sprintf "bitcast %s %s to %s" allocMsgType allocMsgName "i8*")

              let code = sprintf "call void @actor_send_msg(i64 %%_spawned_%s, i64 %d, i8* %s, i64 %s)" actorToSendTo msgId bitcastedPtrName msgSize 
              let fullstring = sprintf "%s\n%s\n%s\n%s\n%s" msgGenCode allocMsgCode storeMsgCode bitcastedPtrCode code
              return ("","", fullstring)
          }
        | Spawn (lvalue, rhs) -> 
          state {
              match rhs with
                  | Some (actorName, initMsg) ->
                      let code = sprintf "call i64 @spawn_actor(i8* (i8*)* bitcast (i8* ()* @_actor_%s to i8* (i8*)*), i8* null)" actorName
                      let tempReg = sprintf "%%_spawned_%s" actorName
                      let! reg = newRegister tempReg "i64" code
                      return reg
                  | None ->
                      return failwith "This should be reached"
          }
        | Receive (msgName, msgType, body) -> 
          state {
              match msgType with
              | UserType "args" ->
                let! genBody = internalCodeGen body
                return genBody
              | _ ->
                let labelName = receiveLabelName msgType
                let! (_,_,genBody) = internalCodeGen body

                // allocate pointer to receive argument
                let! (allocatedMsgName,allocatedMsgType,allocatedMsgCode) = genAlloca msgName (genType msgType)

                // load msg struct
                let! loadedMsg = freshReg
                let! (loadedMsgName, loadedMsgType, loadedMsgCode) = genLoad loadedMsg "%struct.actor_message_struct**" "%_msg"

                // calculate address to data in msg struct
                let! msgDataReg = freshReg
                let! (msgDataPtrName,msgDataPtrType,msgDataPtrCode) = newRegister msgDataReg "i8**" (sprintf "getelementptr %%struct.actor_message_struct* %s, i32 0, i32 4" loadedMsgName)

                // load data in msg.data
                let! msgDataLoadedReg = freshReg
                let! (msgDataLoadedName,msgDataLoadedType,msgDataLoadedCode) = genLoad msgDataLoadedReg msgDataPtrType msgDataPtrName

                // bitcast void pointer to correct pointer 
                let! msgConvIntReg = freshReg
                let! (msgConvIntName,msgConvIntType,msgConvIntCode) = newRegister msgConvIntReg allocatedMsgType (sprintf "bitcast %s %s to %s" msgDataLoadedType msgDataLoadedName allocatedMsgType)

                // load converted void pointer
                let! msgLoadedConvReg = freshReg
                let! (msgLoadedConvName, msgLoadedConvType, msgLoadedConvCode) = genLoad msgLoadedConvReg msgConvIntType msgConvIntName

                // store int in msg
                let! (_,_,storeDataInMsgCode) = genStore msgLoadedConvName msgLoadedConvType (sprintf "%%%s" msgName) allocatedMsgType

                let jumpBackToStart = "br label %start"
                let bodyString = sprintf "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s" allocatedMsgCode loadedMsgCode msgDataPtrCode msgDataLoadedCode msgConvIntCode msgLoadedConvCode storeDataInMsgCode genBody jumpBackToStart
                let! label = genLabel labelName ("","",bodyString)
                return ("","",label)
          }
        | ForIn (elem, list', body) -> 
          state {
              // Initialise index counter to 0. This counter is incremented by 1 at the end of the while body
              let! idxCounterRegName = freshReg
              let idxCounterLValue = {identity = SimpleIdentifier idxCounterRegName; isMutable = true; primitiveType = SimplePrimitive Int}
              let! (idxCounterVal, idxCounterType, idxCounterCode) = internalCodeGen (Initialisation (idxCounterLValue, Constant (SimplePrimitive Int, PrimitiveValue.Int 0)))

              // 
              let (elemType, listLength) = match list' with
                                              | List (contents, ListPrimitive (ptype, n)) -> (ptype, n)
                                              | Identifier (_, ListPrimitive (ptype, n)) -> (ptype, n)
              
              // generate the code for the list
              let! (listName, listType, listCode) = internalCodeGen list'

              // allocate pointer to list
              let! listPtrReg = freshReg
              let! (listPtrName, listPtrType, listPtrCode) = genAlloca listPtrReg listType

              // store list in newly allocated pointer to list
              let! (_,_,listPtrStoreCode) = genStore listName listType listPtrName listPtrType

              // generate the code for the while conditional. while (counter != length of list)
              let! conditionCode = internalCodeGen ( BinOperation (
                                                       Identifier (SimpleIdentifier idxCounterRegName, SimplePrimitive Int), 
                                                       NotEquals, 
                                                       Constant (SimplePrimitive Int, PrimitiveValue.Int listLength)
                                 ) )
              // generate the code that increments the index counter by 1
              let incrementCounter = BinOperation (
                                       Identifier (SimpleIdentifier idxCounterRegName, elemType),
                                       Plus,
                                       Constant (SimplePrimitive Int, PrimitiveValue.Int 1)
                                     )
              let reassCounter = Reassignment (
                                       SimpleIdentifier idxCounterRegName,
                                       incrementCounter
                                       )

              // allocate a pointer to the elem variable in the for-in loop. That is, x in for x in xs {}
              let! (allocatedTempElemAssignName,allocatedTempElemAssignType,allocatedTempElemAssignCode) = genAlloca elem (genType elemType)
             
              // load the index counter
              let! tempLoadedCounterName = freshReg
              let! (loadedCounterName, loadedCounterType, loadedCounterCode) = genLoad tempLoadedCounterName idxCounterType idxCounterVal

              // calculate a pointer to the element to assign to elem
              let! curElemReg = freshReg
              let! (curElemName, curElemType, curElemCode) = newRegister curElemReg (sprintf "%s*" (genType elemType)) (sprintf "getelementptr %s %s, i64 0, %s %s" listPtrType listPtrName loadedCounterType loadedCounterName)

              // load the pointer to the value
              let! loadedElemReg = freshReg
              let! (loadedElemName, loadedElemType, loadedElemCode) = genLoad loadedElemReg curElemType curElemName

              let! (_,_,elemReass) = genStore loadedElemName loadedElemType allocatedTempElemAssignName allocatedTempElemAssignType

              let! (_,_,bodyCode) = internalCodeGen (Body [ body; reassCounter])
              let fullBodyCode = sprintf "%s\n%s\n%s\n%s\n%s\n%s\n%s" loadedCounterCode listPtrCode listPtrStoreCode curElemCode loadedElemCode elemReass bodyCode

              let! (_,_,whileCode) = genWhile conditionCode ("","",fullBodyCode)
              let fullString = sprintf "%s\n%s\n%s\n%s\n" idxCounterCode listCode allocatedTempElemAssignCode whileCode
              return ("","",fullString)
          }
        | List (contents, pType) -> 
          state {
              let! test = collectAll internalCodeGen contents
              let listContent = test
                                |> List.map (fun (name, typ, code) -> sprintf "%s %s" typ name)
                                |> String.concat ", "
              let targetType = genType pType
              let! ptrReg = freshReg
              // allocate pointer in which the list will be stored
              let! (ptrToListName, ptrToListType, ptrToListCode) = genAlloca ptrReg targetType
              let name = sprintf "[ %s ]"listContent
              // store the list content in the pointer allocated earlier
              let! (_,_,storeListCode) = genStore name targetType ptrToListName ptrToListType

              //load the value of the pointer
              let! loadedListReg = freshReg
              let! (loadedListName, loadedListType, loadedListCode) = genLoad loadedListReg ptrToListType ptrToListName

              let fullString = sprintf "%s\n%s\n%s" ptrToListCode storeListCode loadedListCode
              return (loadedListName , loadedListType, fullString)
          }
        | BinOperation (lhs, op, rhs) -> 
          state {
              let! (sLhsName, sLhsType, lhsCode) = internalCodeGen lhs
              let! (sRhsName, sRhsType, rhsCode) = internalCodeGen rhs

              let! (codeName, codeType, codeCode) = state {
                  match op, sLhsType with
                  | Multiply, "i64" -> 
                    let! tempReg = freshReg
                    let code = sprintf "mul %s %s, %s" sLhsType sLhsName sRhsName
                    let! res = newRegister tempReg sLhsType code
                    return res
                  | Multiply, "double" ->
                    let! tempReg = freshReg
                    let code = sprintf "fmul %s %s, %s" sLhsType sLhsName sRhsName
                    let! res = newRegister tempReg sLhsType code
                    return res
                  | Modulo, "i64" -> 
                    let! tempReg = freshReg
                    let code = sprintf "srem %s %s, %s" sLhsType sLhsName sRhsName
                    let! res = newRegister tempReg sLhsType code
                    return res
                  | Modulo, "double" -> 
                    let! tempReg = freshReg
                    let code = sprintf "frem %s %s, %s" sLhsType sLhsName sRhsName
                    let! res = newRegister tempReg sLhsType code
                    return res
                  | Root, "i64" ->
                    // cast the lhs of root from int to double
                    let! convToDoubleLhsReg = freshReg
                    let convToDoubleLhsStr = sprintf "sitofp %s %s to double" sLhsType sLhsName
                    let! (convToDoubleLhsName, convToDoubleLhsType, convToDoubleLhsCode) = newRegister convToDoubleLhsReg "double" convToDoubleLhsStr

                    // Divide 1.0 by the exponent converted to double
                    let! divideExponentReg = freshReg
                    let! (divideExponentName, divideExponentType, divideExponentCode) = newRegister divideExponentReg "double" (sprintf "fdiv double 1.0, %s" convToDoubleLhsName)

                    // cast the rhs int to double
                    let! convToDoubleRhsReg = freshReg
                    let convToDoubleRhsStr = sprintf "sitofp %s %s to double" sRhsType sRhsName
                    let! (convToDoubleRhsName, convToDoubleRhsType, convToDoubleRhsCode) = newRegister convToDoubleRhsReg "double" convToDoubleRhsStr

                    // do lhs ^ dividedExponent
                    let! tempReg = freshReg
                    let code = sprintf "call double @llvm.pow.f64( %s %s, %s %s)" convToDoubleRhsType convToDoubleRhsName divideExponentType divideExponentName
                    let! (resName,resType, resCode) = newRegister tempReg convToDoubleRhsType code

                    let fullCode = sprintf "%s\n%s\n%s\n%s" convToDoubleLhsCode divideExponentCode convToDoubleRhsCode resCode
                    return (resName, resType, fullCode)
                  | Root, "double" ->
                    // Divide 1.0 by the exponent
                    let! divideExponentReg = freshReg
                    let! (divideExponentName, divideExponentType, divideExponentCode) = newRegister divideExponentReg "double" (sprintf "fdiv double 1.0, %s" sLhsName)

                    // do lhs ^ dividedExponent
                    let! tempReg = freshReg
                    let code = sprintf "call double @llvm.pow.f64( %s %s, %s %s)" sRhsType sRhsName divideExponentType divideExponentName
                    let! (resName,resType, resCode) = newRegister tempReg sLhsType code

                    let fullCode = sprintf "%s\n%s" divideExponentCode resCode
                    return (resName, resType, fullCode)
                  | Power, "double" ->
                    let! tempReg = freshReg
                    let code = sprintf "call double @llvm.pow.f64( %s %s, %s %s)" sLhsType sLhsName sRhsType sRhsName
                    let! res = newRegister tempReg sLhsType code
                    return res
                  | Power, "i64" ->
                    // cast the lhs int to double
                    let! convToDoubleReg = freshReg
                    let convToDoubleStr = sprintf "sitofp %s %s to double" sLhsType sLhsName
                    let! (convToDoubleName, convToDoubleType, convToDoubleCode) = newRegister convToDoubleReg "double" convToDoubleStr

                    // call the pow function
                    let! tempReg = freshReg
                    let code = sprintf "call double @llvm.powi.f64( double %s, %s %s)" convToDoubleReg sRhsType sRhsName
                    let! (resName, resType, resCode) = newRegister tempReg "double" code

                    // convert the result of pow to int
                    let! convResToIntReg = freshReg
                    let convResToIntStr = sprintf "fptoui %s %s to i64" resType resName
                    let! (convResToIntName, convResToIntType, convResToIntCode) = newRegister convResToIntReg "i64" convResToIntStr

                    let fullCode = sprintf "%s\n%s\n%s" convToDoubleCode resCode convResToIntCode
                    return (convResToIntName, convResToIntType, fullCode)
                  | Plus, "i64" ->
                    let! tempReg = freshReg
                    let code = sprintf "add %s %s, %s" sLhsType sLhsName sRhsName
                    let! res = newRegister tempReg sLhsType code
                    return res
                  | Plus, "double" ->
                    let! tempReg = freshReg
                    let code = sprintf "fadd %s %s, %s" sLhsType sLhsName sRhsName
                    let! res = newRegister tempReg sLhsType code
                    return res
                  | Minus, "i64" ->
                    let! tempReg = freshReg
                    let code = sprintf "sub %s %s, %s" sLhsType sLhsName sRhsName
                    let! res = newRegister tempReg sLhsType code
                    return res
                  | Minus, "double" ->
                    let! tempReg = freshReg
                    let code = sprintf "fsub %s %s, %s" sLhsType sLhsName sRhsName
                    let! res = newRegister tempReg sLhsType code
                    return res
                  | Divide, "i64" ->
                    let! tempReg = freshReg
                    let code = sprintf "sdiv %s %s, %s" sLhsType sLhsName sRhsName
                    let! res = newRegister tempReg sLhsType code
                    return res
                  | Divide, "double" ->
                    let! tempReg = freshReg
                    let code = sprintf "fdiv %s %s, %s" sLhsType sLhsName sRhsName
                    let! res = newRegister tempReg sLhsType code
                    return res
                  | Or, "i1" -> 
                    let! tempReg = freshReg
                    let code = sprintf "or %s %s, %s" sLhsType sLhsName sRhsName
                    let! res = newRegister tempReg sLhsType code
                    return res
                  | And, "i1" -> 
                    let! tempReg = freshReg
                    let code = sprintf "and %s %s, %s" sLhsType sLhsName sRhsName
                    let! res = newRegister tempReg sLhsType code
                    return res
                  | GreaterThan, "i64" | GreaterThanOrEq, "i64" | LessThan, "i64" | LessThanOrEq, "i64" | Equals, "i64" | Equals, "i1" | NotEquals, "i64" | NotEquals, "i1" -> 
                    let! tempReg = freshReg
                    let code = icmpString op sLhsType sLhsName sRhsName
                    let! res = newRegister tempReg "i1" code
                    return res
                  | GreaterThan, "double" | GreaterThanOrEq, "double" | LessThan, "double" | LessThanOrEq, "double" | Equals, "double" | NotEquals, "double" ->
                    let! tempReg = freshReg
                    let code = fcmpString op sLhsType sLhsName sRhsName
                    let! res = newRegister tempReg "i1" code
                    return res
                  | _,_ as err -> return failwith (sprintf "%A not matched" err)
                  }
              let fullString = sprintf "%s\n%s\n%s\n" lhsCode rhsCode codeCode
              return (codeName, codeType, fullString)
          }
        | Identifier (id, pType) ->
          state {
              match id with
              | SimpleIdentifier str -> 
                let! regName = freshReg
                let toLoadName = if str.StartsWith("%") then sprintf "%s" str 
                                 else sprintf "%%%s" str
                let! toLoadType = findRegister toLoadName
                let! loadedValue = genLoad regName toLoadType toLoadName
                return loadedValue
              | IdentifierAccessor (startId, nextElem) ->
                let! regName = freshReg
                let str = startId
                let toLoadName = if str.StartsWith("%") then sprintf "%s" str 
                                 else sprintf "%%%s" str
                let! toLoadType = findRegister toLoadName
                let! loadedValue = genLoad regName toLoadType toLoadName
                return loadedValue
          }
        | Function (funcName, arguments, types, body) -> 
          state {
              return ("","", "")
          }
        | StructLiteral (structToInit, fieldNamesAndVals) -> 
          state {
              let index = ""
              return ("","", "")
          }
        | Invocation (functionName, parameters, functionType) -> 
          state {
              match functionName with
              | "print" ->
                let stringToPrint = parameters.Head
                let! (strName, strType, strCode) as str = declareStringConstant stringToPrint // make a global definition and assign a name to it in strName
                let! regName = freshReg // Get new imediate register
                let! (loadedStringName, loadedStringType, loadCode) = genLoadString regName str
                let putsCode = sprintf "call i32 @puts(%s %s)" loadedStringType loadedStringName
                let fullString = sprintf "%s\n%s" loadCode putsCode // Concatenate loadCode string and putsCode string in to fullstring, this is the whole mechanism for loading and printing(puts) a string in the program
                return ("","", fullString)
              | "printint" ->
                let intToPrint = parameters.Head
                let! (strName, strType, strCode) as str = declareStringConstant "%d\n"
                let! regName = freshReg
                let! (loadedStringName, loadedStringType, loadCode) = genLoadString regName str
                let (test, _) = Int64.TryParse(intToPrint)
                if (test) then // just print out the int
                  let putsCode = sprintf "call i32 (i8*, ...)* @printf(%s %s, i64 %s)" loadedStringType loadedStringName intToPrint
                  let fullString = sprintf "%s\n%s" loadCode putsCode
                  return ("","", fullString)
                else // load the int and print it
                  let! loadReg = freshReg
                  let! (loadedInt, _, loadedIntCode) = genLoad loadReg "i64*" (sprintf "%%%s" intToPrint)
                  let putsCode = sprintf "call i32 (i8*, ...)* @printf(%s %s, i64 %s)" loadedStringType loadedStringName loadedInt
                  let fullString = sprintf "%s\n%s\n%s" loadedIntCode loadCode putsCode
                  return ("","", fullString)
                
              | "printreal" ->
                let realToPrint = parameters.Head
                let! (strName, strType, strCode) as str = declareStringConstant "%.20lf\n"
                let! regName = freshReg
                let! (loadedStringName, loadedStringType, loadCode) = genLoadString regName str
                let (test, _) = Double.TryParse(realToPrint)
                if (test) then // just print out the real
                  let putsCode = sprintf "call i32 (i8*, ...)* @printf(%s %s, double %s)" loadedStringType loadedStringName realToPrint
                  let fullString = sprintf "%s\n%s" loadCode putsCode
                  return ("","", fullString)
                else // load the int and print it
                  let! loadReg = freshReg
                  let! (loadedInt, _, loadedIntCode) = genLoad loadReg "double*" (sprintf "%%%s" realToPrint)
                  let putsCode = sprintf "call i32 (i8*, ...)* @printf(%s %s, double %s)" loadedStringType loadedStringName loadedInt
                  let fullString = sprintf "%s\n%s\n%s" loadedIntCode loadCode putsCode
                  return ("","", fullString)
          }
        | UnaryOperation (op, rhs) -> 
          state {
              let! (rhsName, rhsType, rhsCode) = internalCodeGen rhs
              let (code, targetType) = 
                match op with
                | Not -> (icmpString NotEquals rhsType rhsName "true", "i1") // rhsValue != true == NOT rhsValue
              
              let! tempReg = freshReg
              let! (resName, resType, resCode) = newRegister tempReg targetType code
              let fullString = sprintf "%s\n%s" rhsCode resCode
              return (resName,resType, fullString)
          }
        | While (condition, body) ->
          state {
              let! conditionGen = internalCodeGen condition
              let! bodyGen = internalCodeGen body
              let! (_,_,whileCode) = genWhile conditionGen bodyGen
              return ("","",whileCode)
          }
        | Die -> 
          state {
              let! (_,_,reassignment) = internalCodeGen (Reassignment (SimpleIdentifier "%_active", Constant (SimplePrimitive Bool, PrimitiveValue.Bool false)))
              let jumpToStart = "br label %start"
              let fullString = sprintf "%s\n%s" reassignment jumpToStart
              return ("","",fullString)
          }
        | Return (arg) -> 
          state {
              return ("","", "")
          }

    and genActorDefine (name:string) (retType:string) (args:TypeDeclaration list) (body:string) : State<Value, Environment> =
        state {
            let sArgumentList = args
                                |> List.map (fun
                                              (argName, argType) -> sprintf "%s %s" argName (genType argType)
                                            )
                                |> String.concat ", "
            //do! append (sprintf "define %s @%s(%s) {\n" sRetType name sArgumentList)
            let defineCode = sprintf "define %s @%s(%s) {\n" retType name sArgumentList

//            do! append (if name = "main" then 
//                          "ret i32 0\n}"
//                        else "}")
            let mainHackCode = "ret i8* null\n}"
                               
            let fullString = sprintf "%s\n%s\n%s" defineCode body mainHackCode
            return (name, retType, fullString)
        }

    and genLabel labelName (bodyName, bodyType, bodyCode) : State<string, Environment> =
        state {
            let label = sprintf "%s:" labelName
            //do! append code
            let fullString = sprintf "%s\n%s" label bodyCode
            return fullString
        }

    and genWhile (condName:string, condType, condCode) (_,_,bodyCode) : State<Value, Environment> =
        state {
            let coName = condName.[1..]
            let brCode = sprintf "br label %%switch%s\n" coName
           
            let switcCode = sprintf "switch%s: \n %s"  coName condCode
            let brStr = sprintf "br i1 %s, label %%body%s, label %%cont%s\n" condName coName coName
           
            let bodyLabel = sprintf "body%s: \n" coName
          
            let bodyCodeString = sprintf  "%s\n br label %%switch%s\n" bodyCode coName
          
            let con = sprintf "\ncont%s:\n" coName
          
            let fullString = brCode + switcCode + brStr + bodyLabel + bodyCodeString + con
          
            return ("","", fullString)  
        }

    let rec findAllActorLabels (ast:AST) : Map<string, (int * string) list> =
        match ast with
        | Program stms | Body stms ->
            stms
            |> List.map findAllActorLabels
            |> List.reduce (Map.fold (fun acc key value -> Map.add key value acc)) // merge all maps
        | Actor (name, body) ->
            findAllReceives body
            |> calcReceiveJumps
            |> fun xs -> Map.ofList [(name,xs)]
        | _ -> Map.empty

    let structToStr (stru:CStruct) : string =
        let (name, types) = stru
        let structStr = sprintf "%%struct.%s = type { " name
        let typeStr = types |> List.map (fun (name, index, t, llvmtype) -> llvmtype) |> String.concat ", "
        structStr + typeStr + " }"
  
    let StructsToStr (structs:CStruct list) : string = 
        structs |> List.map(fun x -> structToStr(x)) |> String.concat "\n"


    let codeGen (ast:AST) : string =
        let filledActors = findAllActorLabels ast
        let ((_,_,_), env) = (runState (internalCodeGen ast) {regCounter = 0; genString =""; registers = Map.empty; structs = []; globalVars = []; actors = filledActors})
        let struct1 = env.structs
        let ((_,_,fullString), env) = (runState (internalCodeGen ast) {regCounter = 0; genString =""; registers = Map.empty; structs = env.structs; globalVars = []; actors = filledActors})
        let structs = (struct1 |> StructsToStr) + "\n"
        let globals = env.globalVars
                      |> List.map (fun (name,type',str) -> (name, type', str.Replace("\"", "\22")))
                      |> List.map
                        (fun (varName, varType, initVal) -> 
                          sprintf "%s = constant %s c\"%s\00\"\n" varName varType initVal)
                      |> String.concat ""
        let externalFunctions = String.concat "" ["declare double @llvm.pow.f64(double, double)\n"
                                                 ;"declare double @llvm.powi.f64(double, i64)\n"
                                                 ;"declare i32 @puts(i8*)\n"
                                                 ;"declare i32 @printf(i8*, ...)\n"
                                                 ;"declare void @actor_init(...)\n"
                                                 ;"declare void @actor_wait_finish(...)\n"
                                                 ;"declare void @actor_destroy_all(...)\n"
                                                 ;"%struct.actor_message_struct = type { %struct.actor_message_struct*, i64, i64, i64, i8*, i64}\n"
                                                 ;"%struct.actor_main = type { i32, i8** }\n"
                                                 ;"declare void @exit(i32)\n"
                                                 ;"declare i64 @spawn_actor(i8* (i8*)*, i8*)\n"
                                                 ;"declare void @actor_send_msg(i64, i64, i8*, i64)\n"
                                                 ;"declare %struct.actor_message_struct* @actor_receive(...)\n"]
          
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
                      %6 = load i32* %1
                      ret i32 %6
                    }"""

        let appendTargetTriple = 
            let os = Environment.OSVersion
            let pid = os.Platform  
            let targetTriple = match pid with
                              | PlatformID.Unix ->
                                  if (Directory.Exists("/Applications")
                                      && Directory.Exists("/System")
                                      && Directory.Exists("/Users")
                                      && Directory.Exists("/Volumes")) then
                                      None
                                  else  Some "x86_64-pc-linux-gnu"
                              | PlatformID.MacOSX -> None
                              | PlatformID.Win32NT -> None
                              | _ -> None
            match targetTriple with
            | Some str -> 
              sprintf "target triple = \"%s\"\n" str
            | None -> ""

                                                                       
        appendTargetTriple + externalFunctions + structs +  globals + main + fullString
