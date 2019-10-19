﻿open System

// Types

type Token =
  | OpenBrace
  | CloseBrace
  | OpenParen
  | CloseParen
  | Semicolon
  | Identifier of string
  | Int of int
  | EOF
  | Unknown

// Util

let implode (xs:char list) =
  let sb = System.Text.StringBuilder(xs.Length)
  xs |> List.iter (sb.Append >> ignore)
  sb.ToString()

// Lex

let rec lexInt str chars =
    match chars with
    | [] -> str,[]
    | c::rest when Char.IsDigit c -> lexInt (str @ [c]) rest
    | c::rest -> str,chars

let rec lexIdent str chars =
    match chars with
    | [] -> str,[]
    | c::rest when Char.IsLetterOrDigit c ->
        lexIdent (str @ [c]) rest
    | c::rest -> str,chars

let lexOther chars =
  match chars with
  | [] -> EOF, []
  | c::rest when Char.IsLetter c || c = '_' ->
      let str,rest = lexIdent [] chars
      Identifier (implode str), rest
  | c::rest when Char.IsDigit c -> 
      let str,rest = lexInt [] chars
      Int ((implode str) |> int), rest
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
      if Char.IsWhiteSpace c 
      then lex rest
      else
        let token,rest = (lexOther chars)
        token::(lex rest)

// Main

[<EntryPoint>]
let main argv =
  let testFilename = "test.c"
  let text = IO.File.ReadAllText testFilename
  let chars = Seq.toList text
  let tokens = lex chars
  printfn "%A" tokens

  0
