open AST
open CodeGen
open Lex
open Parse
open System

[<EntryPoint>]
let main argv =
  let testFilename = __SOURCE_DIRECTORY__ + "/../Examples/gen-test.c"
  let text = IO.File.ReadAllText testFilename
  let chars = Seq.toList text

  let tokens = lex chars
  // for token in tokens do printfn "%A" token
  
  let ast = parse tokens
  //printfn "%A" ast

  let asm = gen ast
  printfn "%s" asm

  let asmFilePath = __SOURCE_DIRECTORY__ + "/../example.asm"
  let writer = new IO.StreamWriter(path=asmFilePath)
  writer.Write asm
  writer.Close()

  0
