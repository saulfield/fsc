open AST
open Lex
open Parse
open System

[<EntryPoint>]
let main argv =
  let testFilename = "test.c"
  let text = IO.File.ReadAllText testFilename
  let chars = Seq.toList text
  let tokens = lex chars

  for token in tokens do
    printfn "%A" token

  let ast,_ = parse tokens
  printfn "%A" ast

  0
