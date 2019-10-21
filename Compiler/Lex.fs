module Lex
open System

type Token =
  | OpenBrace
  | CloseBrace
  | OpenParen
  | CloseParen
  | Semicolon
  | Bang
  | Minus
  | Identifier of string
  | Int of int
  | KeywordReturn
  | KeywordVoid
  | KeywordInt 
  | EOF
  | Unknown

let implode (xs:char list) =
  let sb = System.Text.StringBuilder(xs.Length)
  xs |> List.iter (sb.Append >> ignore)
  sb.ToString()

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
  | "int"    -> KeywordInt
  | "void"   -> KeywordVoid
  | "return" -> KeywordReturn
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
  | c::_ when Char.IsLetter c || c = '_' -> lexIdent chars
  | c::_ when Char.IsDigit c             -> lexInt chars
  | c::_ -> failwithf "Lex failed with char: %A" c

let rec lex chars =
  match chars with
  | [] -> []
  | '{'::rest -> OpenBrace::(lex rest)
  | '}'::rest -> CloseBrace::(lex rest)
  | '('::rest -> OpenParen::(lex rest)
  | ')'::rest -> CloseParen::(lex rest)
  | ';'::rest -> Semicolon::(lex rest)
  | '!'::rest -> Bang::(lex rest)
  | '-'::rest -> Minus::(lex rest)
  | c::rest ->
      if Char.IsWhiteSpace c then lex rest
      else
        let token,rest = lexOther chars
        token::(lex rest)
