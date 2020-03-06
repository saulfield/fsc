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
  | _ -> parseError "expected identifier"

let expectInt tokens =
  match tokens with
  | (TkIntConstant intVal)::rest -> intVal,rest
  | _ -> parseError "expected int constant"

let isUnaryOp token =
  match token with
  | TkBang  -> true
  | TkMinus -> true
  | _ -> false

let getUnaryOp token =
  match token with
  | TkBang  -> Not
  | TkMinus -> Neg
  | _ -> failwithf "expected unary op: %A" token

let isAddOp token =
  match token with
  | TkPlus  -> true
  | TkMinus -> true
  | _ -> false

let isMulOp token =
  match token with
  | TkMul -> true
  | TkDiv -> true
  | _ -> false

let getBinOp token =
  match token with
  | TkPlus  -> Add
  | TkMinus -> Sub
  | TkMul   -> Mul
  | TkDiv   -> Div
  | _ -> failwithf "expected binary op: %A" token
  
let getTypeFromToken token =
  match token with
  | TkTypeChar   -> TypeChar
  | TkTypeDouble -> TypeDouble
  | TkTypeFloat  -> TypeFloat
  | TkTypeInt    -> TypeInt
  | TkTypeVoid   -> TypeVoid
  | _ -> parseError "expected a type"

let rec parseExp tokens =
  let parseFunCall id tokens =
    let rec parseArgs args tokens =
      let exp,tokens' = parseExp tokens
      let newArgs = args @ [exp]
      match tokens' with
      | TkCloseParen::rest -> newArgs,rest
      | TkComma::rest -> parseArgs newArgs rest
      | tok::_ -> parseError "expected close paren or comma, got %A" tok
      | _ -> parseError "expected close paren or comma"

    match tokens with
    | TkCloseParen::rest -> FunCallExp(id, []),rest
    | _ -> 
        let argExps,tokens' = parseArgs [] tokens
        FunCallExp(id, argExps),tokens'

  let parseExpBase tokens =
    match tokens with
    | (TkStringConstant strVal)::rest -> StringExp(strVal),rest
    | (TkIntConstant intVal)::rest -> IntExp(intVal),rest
    | (TkIdentifier id)::TkOpenParen::rest -> parseFunCall id rest
    | (TkIdentifier id)::rest -> VarExp(id),rest
    | TkOpenParen::rest -> 
      let exp,tokens' = parseExp rest
      let tokens' = expect TkCloseParen tokens'
      exp,tokens'
    | _ -> parseError ""
  
  let rec parseExpUnary tokens =
    match tokens with
    | tok::rest when isUnaryOp tok ->
        let op = getUnaryOp tok
        let exp,rest = parseExpUnary rest
        UnaryExp(op, exp), rest
    | _ -> parseExpBase tokens

  let parseExpMul tokens =
    let leftExp,tokens' = parseExpUnary tokens
    if isMulOp tokens'.Head then
      let op = getBinOp tokens'.Head
      let rightExp,tokens' = parseExpUnary tokens'.Tail
      BinExp(leftExp, op, rightExp),tokens'
    else
      leftExp,tokens'
  
  let parseExpAdd tokens =
    let leftExp,tokens' = parseExpMul tokens
    if isAddOp tokens'.Head then
      let op = getBinOp tokens'.Head
      let rightExp,tokens' = parseExpMul tokens'.Tail
      BinExp(leftExp, op, rightExp),tokens'
    else
      leftExp,tokens'

  parseExpAdd tokens

let parseVarDecl declType ident tokens = 
  match tokens with
  | TkSemicolon::rest -> {var_decl.id=ident; varType=declType; initExp=None}, rest
  | TkAssignment::rest ->
      let initExp,tokens' = parseExp rest
      let tokens' = expect TkSemicolon tokens'
      {var_decl.id=ident; varType=declType; initExp=Some(initExp)}, tokens'
        | _ -> failwith ""

let rec parseStmt tokens =
  let parseWhileStmt tokens =
    let tokens' = expect TkOpenParen tokens
    let conditionExp,tokens' = parseExp tokens'
    let tokens' = expect TkCloseParen tokens'

    let body,tokens' = parseStmt tokens'

    WhileStmt(conditionExp, body),tokens'

  let parseReturnStmt tokens =
    let exp,tokens' = parseExp tokens
    let tokens' = expect TkSemicolon tokens'
    ReturnStmt(exp),tokens'

  let parseExpStmt tokens = 
    let exp,tokens' = parseExp tokens
    let tokens' = expect TkSemicolon tokens'
    ExpStmt(exp),tokens'

  match tokens with
  | TkOpenBrace::_ -> parseBlock tokens
  | TkKeywordReturn::rest -> parseReturnStmt rest
  | TkKeywordWhile::rest -> parseWhileStmt rest
  | _ -> parseExpStmt tokens

and parseBlock tokens =
  let rec parseBlockItems items tokens =
    match tokens with
    | [] -> items,tokens
    | TkCloseBrace::_ -> items,tokens
    | TkTypeInt::rest ->
      let declType = getTypeFromToken tokens.Head
      let varId,tokens' = expectId rest
      let decl,tokens' = parseVarDecl declType varId tokens'
      parseBlockItems (items @ [LocalVar(decl)]) tokens'
    | _ -> 
      let stmt,tokens' = parseStmt tokens
      parseBlockItems (items @ [Statement(stmt)]) tokens'

  let tokens' = expect TkOpenBrace tokens
  let stmts,tokens' = parseBlockItems [] tokens'
  let tokens' = expect TkCloseBrace tokens'
  Block(stmts), tokens'
    
let parseFunDecl declType ident tokens =
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
  let blockItems,tokens' = parseBlock tokens'

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
