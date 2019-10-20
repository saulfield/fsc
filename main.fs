open System

// Types

type Keyword =
  | Return
  | Void
  | Int

type Token =
  | OpenBrace
  | CloseBrace
  | OpenParen
  | CloseParen
  | Semicolon
  | Identifier of string
  | Int of int
  | Keyword of Keyword
  | EOF
  | Unknown

// Util

let implode (xs:char list) =
  let sb = System.Text.StringBuilder(xs.Length)
  xs |> List.iter (sb.Append >> ignore)
  sb.ToString()

// Lex

let lexInt chars =
  let rec loop str chars =  
    match chars with
    | [] -> str,[]
    | c::rest when Char.IsDigit c -> loop (str @ [c]) rest
    | _ -> str,chars
  let str,rest = loop [] chars
  Int ((implode str) |> int), rest

let getIdentOrKeyword str =
  match str with
  | "int"    -> Keyword Keyword.Int
  | "void"   -> Keyword Void
  | "return" -> Keyword Return
  | _ -> Identifier str

let lexIdent chars =
  let rec loop str chars =
    match chars with
    | [] -> str,[]
    | c::rest when Char.IsLetterOrDigit c ->
        loop (str @ [c]) rest
    | _ -> str,chars
  let str,rest = loop [] chars
  getIdentOrKeyword (implode str), rest

let lexOther chars =
  match chars with
  | [] -> EOF,[]
  | c::rest when Char.IsLetter c || c = '_' -> lexIdent chars
  | c::rest when Char.IsDigit  c            -> lexInt chars
  | _ -> Unknown, chars

let rec lex chars =
  match chars with
  | [] -> []
  | '{'::rest -> OpenBrace::(lex rest)
  | '}'::rest -> CloseBrace::(lex rest)
  | '('::rest -> OpenParen::(lex rest)
  | ')'::rest -> CloseParen::(lex rest)
  | ';'::rest -> Semicolon::(lex rest)
  | c::rest ->
      if Char.IsWhiteSpace c then lex rest
      else
        let token,rest = lexOther chars
        token::(lex rest)

// Main

[<EntryPoint>]
let main argv =
  let testFilename = "test.c"
  let text = IO.File.ReadAllText testFilename
  let chars = Seq.toList text
  let tokens = lex chars

  for token in tokens do
    printfn "%A" token

  0
