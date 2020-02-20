module Parse
open AST
open Lex
open System

// <program> ::= <function>
// <function> ::= "int" <id> "(" "void" ")" "{" <statement> "}"
// <statement> ::= "return" <exp> ";"
// <exp> ::= <unary_op> <exp> | <int>
// <unary_op> ::= "!" | "-"

let parseError msg =
  failwithf "Parse error: %s" msg

let expect expected tokens =
  match tokens with
  | tok::rest when tok = expected -> rest
  | _ -> failwithf "expected %A" expected

let rec parseExp tokens =
  match tokens with
  | (TkIntConstant intVal)::rest -> AST.IntExp(intVal), rest
  | TkBang::rest ->
      let exp,rest = parseExp rest
      AST.UnaryExp(AST.Not, exp), rest
  | TkMinus::rest ->
      let exp,rest = parseExp rest
      AST.UnaryExp(AST.Neg, exp), rest
  | _ -> parseError "expected int literal"

let parseStmt tokens =
  let exp,tokens = 
    match tokens with
    | TkKeywordReturn::rest -> parseExp rest
    | _ -> parseError "invalid statement"
  let tokens = expect TkSemicolon tokens
  AST.ReturnStmt(exp),tokens

let getTypeFromToken token =
  match token with
  | TkTypeChar   -> TypeChar
  | TkTypeDouble -> TypeDouble
  | TkTypeFloat  -> TypeFloat
  | TkTypeInt    -> TypeInt
  | TkTypeVoid   -> TypeVoid
  | _ -> parseError "expected a type"

let parseFunDecl declType ident tokens =
  // params
  let tokens' = expect TkOpenParen tokens
  let tokens' = expect TkTypeVoid tokens'
  let tokens' = expect TkCloseParen tokens'

  // body
  let tokens' = expect TkOpenBrace tokens'
  let stmt,tokens' = parseStmt tokens'
  let tokens' = expect TkCloseBrace tokens'

  AST.FunDecl {Ident = ident; Stmt = stmt; Type=declType}, tokens'

let parseVarDecl declType ident tokens = 
  let tokens' = expect TkSemicolon tokens
  AST.VarDecl {Ident = ident; Type = declType}, tokens'

let parseTopLevel (tokens: Token list) =
  let declType = getTypeFromToken tokens.Head
  let ident,tokens' =
    match tokens.Tail with
    | (TkIdentifier ident)::rest -> ident,rest
    | _ -> parseError "expected Identifier"

  match tokens'.Head with
  | TkOpenParen -> parseFunDecl declType ident tokens'
  | TkSemicolon -> parseVarDecl declType ident tokens'
  | _ -> parseError "unexpected token in declaration"

let rec parseProgram tokens =
  match tokens with
  | [] -> []
  | tokens -> 
      let decl,tokens' = parseTopLevel tokens
      let otherDecls = parseProgram tokens'
      decl::otherDecls

let parse tokens =
  let decls = parseProgram tokens
  AST.Program decls