namespace Tests

open NUnit.Framework
open Lex
open Parse
open System

#nowarn "3242"

type Tests () =

  let log (msg : obj) = 
    NUnit.Framework.TestContext.WriteLine msg

  [<SetUp>]
  member this.Setup () =
    ()

  [<Test>]
  member this.TestLexer () =
    let testFilename = __SOURCE_DIRECTORY__ + "/../Examples/lex-test.c"
    let text = IO.File.ReadAllText testFilename
    let chars = Seq.toList text

    let tokens = lex chars
    for token in tokens do printfn "%A" token

  [<Test>]
  member this.TestParser () =
    let testFilename = __SOURCE_DIRECTORY__ + "/../Examples/parse-test.c"
    let text = IO.File.ReadAllText testFilename
    let chars = Seq.toList text

    let tokens = lex chars
    for token in tokens do printfn "%A" token

    let ast = parse tokens
    printfn "%A" ast
