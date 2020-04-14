module CodeGen

open AST
open System

type SysCall = 
  | SysExit = 60

type Operand =
  | RAX
  | RDI
  | Imm of int
  | SysCall of SysCall

type Context = {
  var_map: Map<string, Object>
}

let operandToString operand =
  match operand with
  | Imm imm -> imm.ToString()
  | SysCall sysCall -> string (LanguagePrimitives.EnumToValue sysCall)
  | _ -> operand.ToString().ToLower() 

let gen (ast:program) filename =
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
        | _ -> failwith "not implemented"
      | _ -> failwith "not implemented"
       
  let genStmt stmt =
    match stmt with
    | ReturnStmt returnStmt -> 
        genExp returnStmt
        ret ()
    | _ -> failwith "not implemented"
      
  let genBlockItem blockItem =
    match blockItem with
    | Statement stmt -> genStmt stmt
    | LocalVar var -> failwith ""

  let rec genBlock block =
    match block with
    | [] -> ()
    | blockItem::blockItems ->
        genBlockItem blockItem
        genBlock blockItems

  let genFunction f =
    let (AST.ID funcName) = f.id
    emitLabel funcName
    match f.body with
    | Block block -> genBlock block
    | _ -> failwith "expected Block"

  let genGlobalVar gv =
    emit ""

  let genTopLevel tl =
    match tl with
    | AST.Function f -> genFunction f
    | AST.GlobalVar gv -> genGlobalVar gv

  let emitEntryPoint () =
    emitLabel "_start"
    call "main"
    mov RDI RAX
    mov RAX (SysCall SysCall.SysExit) 
    syscall()

  let rec genTopLevels topLevels =
    match topLevels with
    | [] -> emitEntryPoint ()
    | tl::tls -> 
        genTopLevel tl
        genTopLevels tls

  // emit global symbols
  emitGlobal "_start"

  match ast with
  | AST.Program(program) -> genTopLevels program

  writer.Close()
