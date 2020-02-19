namespace Tests

open NUnit.Framework
open Lex
open Parse

#nowarn "3242"

type ParsingTests () =

  let log (msg : obj) = 
    NUnit.Framework.TestContext.WriteLine msg

  let testParse text =
    let chars = Seq.toList text
    let tokens = lex chars
    let ast,_ = parse tokens
    ()

  [<SetUp>]
  member this.Setup () =
    ()

  [<Test>]
  member this.TestReturn () =
    let text = "
    int main(void) {
        return -1;
    }"
    testParse text
