module CodeGen
open AST
open System

type Operand =
  | RAX
  | RDI
  | Imm of int

let operandToString operand =
  match operand with
  | Imm imm -> imm.ToString()
  | _ -> operand.ToString().ToLower() 


let gen ast filename =
  let writer = new IO.StreamWriter(path=filename)
  
  let emit text =
    printfn "\t\t%s" text
    fprintfn writer "\t\t%s" text

  let emitLabel label =
    printfn "%s:" label
    fprintfn writer "%s:" label

  let emitGlobal label =
    emit ("global " + label)

  let mov a b =
    emit ("mov " + operandToString a + ", " + operandToString b)

  let neg a =
    emit ("neg " + operandToString a)

  let syscall () =
    emit "syscall"

  let ret () =
    emit "ret"

  let call funcName =
    emit ("call " + funcName)

  let rec genExp exp =
    match exp with
      | IntExp intVal -> mov RAX (Imm intVal)
      | UnaryExp(op,exp) ->
        match op with
        | Neg ->
          genExp exp
          neg RAX
        | Not -> failwith ""

  let genStmt stmt =
    match stmt with
    | ReturnStmt returnStmt -> genExp returnStmt
      
  // start of code generation      
  let globalSymbols = ["_start"]

  let funcName, stmt = 
    match ast with
    | { Ident = ident; Stmt = stmt } when ident = "main" ->
      ident,stmt
    | { Ident = _; Stmt = _ } ->
      failwith "Entry point must be named 'main'"
    | _ -> failwith "Expected function declaration"

  let globalSymbols = funcName::globalSymbols

  // instructions
  emit "section .text"

  // global symbols
  for sym in globalSymbols do
    emitGlobal sym

  // main function
  emitLabel funcName
  genStmt stmt
  ret()

  // entry point
  emitLabel "_start"
  call funcName
  mov RDI RAX
  mov RAX (Imm 60)
  syscall()

  writer.Close()
