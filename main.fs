open System

// Types

type Token =
  | OpenBrace
  | CloseBrace
  | OpenParen
  | CloseParen
  | Semicolon
  | Identifier of string
  | Int of int
  | KeywordReturn
  | KeywordVoid
  | KeywordInt
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
  | c::rest when Char.IsLetter c || c = '_' -> lexIdent chars
  | c::rest when Char.IsDigit c             -> lexInt chars
  | c::rest -> failwithf "Lex failed with char: %A" c

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

// Parse

// <program> ::= <function>
// <function> ::= "int" <id> "(" "void" ")" "{" <statement> "}"
// <statement> ::= "return" <exp> ";"
// <exp> ::= <int>

type Exp =
  | Int of int

type Stmt =
  | Return of Exp

type FuncDecl = {
  Ident:string;
  Stmt:Stmt
}

let parseError msg =
  failwithf "Parse error: %s" msg

let parseExp tokens =
  match tokens with
  | (Token.Int intVal)::rest -> Exp.Int intVal, rest
  | _ -> parseError "expected int literal"

let parseStmt tokens =
  let exp,tokens = 
    match tokens with
    | KeywordReturn::rest -> parseExp rest
    | _ -> parseError "invalid statement"

  let tokens = 
    match tokens with
    | Semicolon::rest -> rest
    | _ -> parseError "expected semicolon"

  Stmt.Return exp,tokens

let parseFuncDecl tokens =
  let tokens =
    match tokens with
    | KeywordInt::rest -> rest
    | _ -> parseError "unexpected type"

  let ident,tokens =
    match tokens with
    | (Identifier ident)::rest -> ident,rest
    | _ -> parseError "expected Identifier"

  let tokens =
    match tokens with
    | OpenParen::KeywordVoid::CloseParen::rest -> rest
    | _ -> parseError "invalid arg list"

  let stmt,tokens =
    match tokens with
    | OpenBrace::rest -> parseStmt rest
    | _ -> parseError "expected open brace"

  let tokens =
    match tokens with
    | CloseBrace::rest -> rest
    | _ -> parseError "expected close brace"

  { Ident=ident; Stmt=stmt },tokens

let parse toks =
  match toks with
  | KeywordInt::rest -> parseFuncDecl toks
  | _ -> parseError "expected 'int' keyword"

// Main

[<EntryPoint>]
let main argv =
  let testFilename = "test.c"
  let text = IO.File.ReadAllText testFilename
  let chars = Seq.toList text
  let tokens = lex chars

  // for token in tokens do
  //   printfn "%A" token

  let ast,_ = parse tokens
  printfn "%A" ast

  0
