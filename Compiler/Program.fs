﻿open AST
open CodeGen
open Lex
open Parse
open System

[<EntryPoint>]
let main argv =
  let testFilename = __SOURCE_DIRECTORY__ + "/../Examples/parse-test.c"
  let text = IO.File.ReadAllText testFilename
  let chars = Seq.toList text

  let tokens = lex chars
  // for token in tokens do printfn "%A" token
  
  let ast = parse tokens
  printfn "%A" ast

  // gen ast "test1.asm"

  0
