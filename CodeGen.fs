module CodeGen
open AST
open System

let gen ast filename =
  let writer = new IO.StreamWriter(path=filename)
  
  let emit text =
    printfn "\t\t%s" text
    fprintfn writer "\t\t%s" text

  let emitLabel text =
    printfn "%s:" text
    fprintfn writer "%s:" text

  emit "section .text"

  let funcName, stmt = 
    match ast with
    | { Ident = ident; Stmt = stmt } when ident = "main" -> ident,stmt
    | { Ident = _; Stmt = _ } -> failwith "Entry point must be named 'main'"
    | _ -> failwith "Expected function declaration"

  emit ("global " + funcName)
  emitLabel funcName

  let returnVal =
    match stmt with
    | ReturnStmt returnStmt ->
      match returnStmt with
      | IntExp intVal -> intVal

  emit ("mov rax, " + returnVal.ToString())
  emit "ret"

  emit "global _start"
  emitLabel "_start"
  emit ("call " + funcName)
  emit "mov rdi, rax"
  emit "mov rax, 60"
  emit "syscall"

  writer.Close()
