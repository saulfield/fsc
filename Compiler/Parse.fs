module Parse

open AST
open Lex
open System

let parseError msg =
  failwithf "Parse error: %s" msg

let expect expected tokens =
  match tokens with
  | tok::rest when tok = expected -> rest
  | _ -> failwithf "expected %A" expected

let expectId tokens =
  match tokens with
  | (TkIdentifier varId)::rest -> varId,rest
  | _ -> parseError "expected Identifier"

let rec parseExp tokens =
  match tokens with
  | (TkIntConstant intVal)::rest -> IntExp(intVal), rest
  | TkBang::rest ->
      let exp,rest = parseExp rest
      UnaryExp(Not, exp), rest
  | TkMinus::rest ->
      let exp,rest = parseExp rest
      UnaryExp(Neg, exp), rest
  | _ -> parseError "expected int literal"

let parseStmt tokens =
  let exp,tokens = 
    match tokens with
    | TkKeywordReturn::rest -> parseExp rest
    | _ -> parseError "invalid statement" 
  let tokens = expect TkSemicolon tokens
  ReturnStmt(exp),tokens

let getTypeFromToken token =
  match token with
  | TkTypeChar   -> TypeChar
  | TkTypeDouble -> TypeDouble
  | TkTypeFloat  -> TypeFloat
  | TkTypeInt    -> TypeInt
  | TkTypeVoid   -> TypeVoid
  | _ -> parseError "expected a type"

let parseFunDecl declType ident tokens =
  let rec parseBody (items:block_item list) tokens =
    match tokens with
    | [] -> items,tokens
    | TkCloseBrace::_ -> items,tokens
    | TkTypeInt::rest ->
      let declType = getTypeFromToken tokens.Head
      let varId,tokens' = expectId rest
      let tokens' = expect TkSemicolon tokens'
      let decl = LocalVar({id=varId; declType=declType; init=None})
      parseBody (items @ [decl]) tokens'
    | _ -> 
      let stmt,tokens' = parseStmt tokens
      parseBody (items @ [Statement(stmt)]) tokens'

  // params
  let tokens' = expect TkOpenParen tokens
  let parameters = []
  let tokens' = expect TkTypeVoid tokens'
  let tokens' = expect TkCloseParen tokens'

  // body
  let tokens' = expect TkOpenBrace tokens'
  let blockItems,tokens' = parseBody [] tokens'
  let tokens' = expect TkCloseBrace tokens'

  {id=ident; parameters=parameters; body=blockItems; declType=declType}, tokens'

let parseVarDecl declType ident tokens = 
  let tokens' = expect TkSemicolon tokens
  {var_decl.id=ident; declType=declType; init=None}, tokens'

let parseTopLevel (tokens: Token list) =
  let declType = getTypeFromToken tokens.Head
  let id,tokens' = expectId tokens.Tail

  match tokens'.Head with
  | TkOpenParen -> 
    let decl,tokens' = parseFunDecl declType id tokens'
    Func(decl),tokens'
  | TkSemicolon -> 
    let decl,tokens' = parseVarDecl declType id tokens'
    GlobalVar(decl),tokens'
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
  Program(decls)
