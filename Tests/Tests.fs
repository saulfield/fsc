namespace Tests

open NUnit.Framework
open Lex
open Parse
open System

#nowarn "3242"

type LexerTests () =

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
       
type ParserTests () =

  let parseTest text =
    let chars = Seq.toList text
    let tokens = lex chars
    for token in tokens do printfn "%A" token
    let ast = parse tokens
    printfn "%A" ast

  [<SetUp>]
  member this.Setup () =
    ()

  [<Test>]
  member this.ParseExampleFile () =
    let testFilename = __SOURCE_DIRECTORY__ + "/../Examples/parse-test.c"
    let text = IO.File.ReadAllText testFilename
    parseTest text

  [<Test>]
  member this.ParseIfElseBlocks () =
    let text = "
    int main(void) {
      int x = 0;
      if (x > 0) {
        x = 0;
        x = x + 1;
      } else {
        x = 0;
        x = x - 1;
      }
      return 0;
    }"
    parseTest text

  [<Test>]
  member this.ParseIfElseStatements () =
    let text = "
    int main(void) {
      int x = 0;
      if (x > 0)
        x = x + 1;
      else
        x = x - 1;
      return 0;
    }"
    parseTest text
