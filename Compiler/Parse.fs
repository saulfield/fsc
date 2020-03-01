module Parse

open AST
open Lex

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
  
let parseVarDecl declType ident tokens = 
  match tokens with
  | TkSemicolon::rest -> {var_decl.id=ident; varType=declType; initExp=None}, rest
  | TkAssignment::rest ->
    let initExp,tokens' = parseExp rest
    let tokens' = expect TkSemicolon tokens'
    {var_decl.id=ident; varType=declType; initExp=Some(initExp)}, tokens'
  | _ -> failwith ""
  
let parseFunDecl declType ident tokens =
  let parseBody tokens =
    let rec loop items tokens =
      match tokens with
      | [] -> items,tokens
      | TkCloseBrace::_ -> items,tokens
      | TkTypeInt::rest ->
        let declType = getTypeFromToken tokens.Head
        let varId,tokens' = expectId rest
        let decl,tokens' = parseVarDecl declType varId tokens'
        loop (items @ [LocalVar(decl)]) tokens'
      | _ -> 
        let stmt,tokens' = parseStmt tokens
        loop (items @ [Statement(stmt)]) tokens'
    loop [] tokens

  let parseParams tokens =
    let rec parseOtherParams items tokens =
      match tokens with
      | TkCloseParen::_ -> items,tokens
      | _ ->
        let tokens' = expect TkComma tokens
        let tokens' = expect TkTypeInt tokens'
        let id,tokens' = expectId tokens'
        parseOtherParams (items @ [Param(TypeInt, id)]) tokens'

    match tokens with
    | TkCloseParen::_ -> [],tokens  // no params
    | TkTypeVoid::rest -> [],rest   // void type
    | TkTypeInt::rest ->
      let id,tokens' = expectId rest
      if tokens'.Head = TkCloseParen then
        [Param(TypeInt, id)],tokens'
      else
        parseOtherParams [Param(TypeInt, id)] tokens'
    | tok::_ -> failwithf "unexpected token %A" tok
    | [] -> failwith "end of token stream"

  // params
  let tokens' = expect TkOpenParen tokens
  let parameters,tokens' = parseParams tokens'
  let tokens' = expect TkCloseParen tokens'

  // body
  let tokens' = expect TkOpenBrace tokens'
  let blockItems,tokens' = parseBody tokens'
  let tokens' = expect TkCloseBrace tokens'

  {id=ident; parameters=parameters; body=blockItems; funType=declType}, tokens'
  
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
