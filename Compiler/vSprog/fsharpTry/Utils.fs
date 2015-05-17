

namespace vSprog
open System
open vSprog.Analysis
open vSprog.CodeGen
open vSprog.ParserUtils
open vSprog.Parser
open vSprog.CommonTypes
open vSprog.AST

module Utils =
        let genProgram (input:string) = 
            parse input "../../grammar.gram"  //Generates hime AST
            >>= fun tree -> Success (toAST tree)
            //>>= fun ast -> Success ast  // Gør den noget?
            >>= analyse
            >>= (fun ast -> Success (codeGen ast))
