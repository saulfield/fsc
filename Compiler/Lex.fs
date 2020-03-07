module Lex
open AST
open System

type Token =
  // Types
  | TkTypeChar
  | TkTypeDouble
  | TkTypeFloat
  | TkTypeInt
  | TkTypeVoid
  // Keywords
  | TkKeywordBreak
  | TkKeywordCase
  | TkKeywordDefault
  | TkKeywordElse
  | TkKeywordFor
  | TkKeywordIf
  | TkKeywordReturn
  | TkKeywordSizeof
  | TkKeywordStruct
  | TkKeywordSwitch
  | TkKeywordWhile
  // Operators
  | TkBang
  | TkDecrement
  | TkMinus
  | TkIncrement
  | TkPlus
  | TkDiv
  | TkAssignment
  | TkEq
  | TkNotEq
  | TkLogicalOr
  | TkBitwiseOr
  | TkLogicalAnd
  | TkLt
  | TkLtEq
  | TkGt
  | TkGtEq
  | TkMul
  | TkAmpersand
  // Constants and identifiers
  | TkIdentifier of id
  | TkStringConstant of string
  | TkIntConstant of int
  // Misc.
  | TkOpenBrace
  | TkCloseBrace
  | TkOpenParen
  | TkCloseParen
  | TkOpenSquareBracket
  | TkCloseSquareBracket
  | TkSemicolon
  | TkColon
  | TkComma

let implode (xs:char list) =
  let sb = System.Text.StringBuilder(xs.Length)
  xs |> List.iter (sb.Append >> ignore)
  sb.ToString()

let getIdentOrKeyword str =
  match str with
  | "int"     -> TkTypeInt
  | "void"    -> TkTypeVoid
  | "char"    -> TkTypeChar
  | "float"   -> TkTypeFloat
  | "double"  -> TkTypeDouble
  | "break"   -> TkKeywordBreak
  | "case"    -> TkKeywordCase
  | "default" -> TkKeywordDefault
  | "else"    -> TkKeywordElse
  | "for"     -> TkKeywordFor
  | "if"      -> TkKeywordIf
  | "return"  -> TkKeywordReturn
  | "sizeof"  -> TkKeywordSizeof
  | "struct"  -> TkKeywordStruct
  | "switch"  -> TkKeywordSwitch
  | "while"   -> TkKeywordWhile
  | _ -> TkIdentifier (ID str)

let rec lex chars =
  let rec skipLineComment xs =
    match xs with
    | [] -> []
    | '\n'::rest -> rest
    | c::rest -> skipLineComment rest

  let rec skipMultiLineComment xs =
    match xs with
    | [] -> []
    | '*'::'/'::rest -> rest
    | c::rest -> skipMultiLineComment rest

  let lexIdentOrKeyword chars =
    let isNonDigit c =
      Char.IsLetterOrDigit c || c = '_'

    let rec loop str chars =
      match chars with
      | [] -> str,[]
      | c::rest when isNonDigit c ->
          loop (str @ [c]) rest
      | _ -> str,chars
    let str,rest = loop [] chars
    (getIdentOrKeyword (implode str))::(lex rest)

  let lexNum chars =
    let rec loop str chars =  
      match chars with
      | [] -> str,[]
      | c::rest when Char.IsDigit c -> loop (str @ [c]) rest
      | _ -> str,chars
    let str,rest = loop [] chars
    (TkIntConstant ((implode str) |> int))::(lex rest)

  let lexString chars =
    let rec loop str chars =  
      match chars with
      | [] -> str,[]
      | '\"'::rest -> str,rest
      | c::rest -> loop (str @ [c]) rest
    let str,rest = loop [] chars
    (TkStringConstant ((implode str)))::(lex rest)

  match chars with
  | [] -> []
  | c::rest when Char.IsWhiteSpace c -> lex rest
  | '/'::'/'::rest -> lex (skipLineComment rest)
  | '/'::'*'::rest -> lex (skipMultiLineComment rest)
  | '\"'::rest -> lexString rest
  | '{'::rest -> TkOpenBrace::(lex rest)
  | '}'::rest -> TkCloseBrace::(lex rest)
  | '['::rest -> TkOpenSquareBracket::(lex rest)
  | ']'::rest -> TkCloseSquareBracket::(lex rest)
  | '('::rest -> TkOpenParen::(lex rest)
  | ')'::rest -> TkCloseParen::(lex rest)
  | ';'::rest -> TkSemicolon::(lex rest)
  | ':'::rest -> TkColon::(lex rest)
  | ','::rest -> TkComma::(lex rest)
  | '!'::'='::rest -> TkNotEq::(lex rest)
  | '!'::rest -> TkBang::(lex rest)
  | '-'::'-'::rest -> TkDecrement::(lex rest)
  | '-'::rest -> TkMinus::(lex rest)
  | '+'::'+'::rest -> TkIncrement::(lex rest)
  | '+'::rest -> TkPlus::(lex rest)
  | '/'::rest -> TkDiv::(lex rest)
  | '='::'='::rest -> TkEq::(lex rest)
  | '='::rest -> TkAssignment::(lex rest)
  | '>'::'='::rest -> TkGtEq::(lex rest)
  | '>'::rest -> TkGt::(lex rest)
  | '<'::'='::rest -> TkLtEq::(lex rest)
  | '<'::rest -> TkLt::(lex rest)
  | '|'::'|'::rest -> TkLogicalAnd::(lex rest)
  | '|'::rest -> TkBitwiseOr::(lex rest)
  | '&'::'&'::rest -> TkLogicalAnd::(lex rest)
  | '&'::rest -> TkAmpersand::(lex rest)
  | '*'::rest -> TkMul::(lex rest)
  | c::_ when Char.IsDigit c -> lexNum chars
  | c::_ when Char.IsLetter c || c = '_' -> lexIdentOrKeyword chars
  | c::_ -> failwithf "Lex failed with char: %A" c
