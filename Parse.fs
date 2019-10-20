module Parse
open AST
open Lex
open System

// <program> ::= <function>
// <function> ::= "int" <id> "(" "void" ")" "{" <statement> "}"
// <statement> ::= "return" <exp> ";"
// <exp> ::= <int>

let parseError msg =
  failwithf "Parse error: %s" msg

let expect expected tokens =
  match tokens with
  | tok::rest when tok = expected -> rest
  | _ -> failwithf "expected %A" expected

let parseExp tokens =
  match tokens with
  | (Token.Int intVal)::rest -> AST.IntExp(intVal), rest
  | _ -> parseError "expected int literal"

let parseStmt tokens =
  let exp,tokens = 
    match tokens with
    | KeywordReturn::rest -> parseExp rest
    | _ -> parseError "invalid statement"
  let tokens = expect Semicolon tokens
  AST.ReturnStmt(exp),tokens

let parseFuncDecl tokens =
  // return type
  let tokens =
    match tokens with
    | KeywordInt::rest -> rest
    | _ -> parseError "unexpected type"

  // identifier
  let ident,tokens =
    match tokens with
    | (Identifier ident)::rest -> ident,rest
    | _ -> parseError "expected Identifier"

  // params
  let tokens = expect OpenParen tokens
  let tokens = expect KeywordVoid tokens
  let tokens = expect CloseParen tokens

  // body
  let tokens = expect OpenBrace tokens
  let stmt,tokens = parseStmt tokens
  let tokens = expect CloseBrace tokens

  { Ident=ident; Stmt=stmt },tokens

let parse toks =
  match toks with
  | KeywordInt::_ -> parseFuncDecl toks
  | _ -> parseError "expected 'int' keyword"
