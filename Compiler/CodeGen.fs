module CodeGen

open AST
open System

type Operand =
  | RAX
  | RDI
  | Imm of int

type VarLocation =
  | Global of string
  | Local of int

type Context = {
  var_map: Map<string, VarLocation>
}

let operandToString operand =
  match operand with
  | Imm imm -> imm.ToString()
  | _ -> operand.ToString().ToLower() 

let gen (ast:program) filename =
  let writer = new IO.StreamWriter(path=filename)
  
  let emitNoTab text =
    printfn "%s" text
    fprintfn writer "%s" text

  let emit text =
    printfn "\t%s" text
    fprintfn writer "\t%s" text

  let emitLabel label =
    printfn "%s:" label
    fprintfn writer "%s:" label

  let emitGlobalVar name value =
    emit "section .data"
    printfn "%s: db %d" name value
    fprintfn writer "%s: db %d" name value

  let emitGlobal label =
    emit ("global " + label)

  let mov a b =
    emit ("mov " + operandToString a + ", " + operandToString b)

  let neg a =
    emit ("neg " + operandToString a)

  let ret () =
    emit "ret"

  let call funcName =
    emit ("call " + funcName)

  let genBinOp op =
    match op with
    | Add -> emit "add rax, rcx"
    | Sub -> emit "sub rax, rcx"
    | Mul -> emit "mul rcx"
    | Div -> emit "div rcx"
    | _ -> failwith "not implemented"

  let lookupVar name =
    match name with
    | "x" -> VarLocation.Local -8
    | "y" -> VarLocation.Local 16
    | _ -> failwith ""

  let genVarExp name =
    let location = lookupVar name
    let code = 
      match location with
      | Global label -> sprintf "mov rax, [%s]      \t; global variable" label
      | Local offset -> sprintf "mov rax, [rbp + %d]\t; local variable or arg" offset
    emit code

  let rec genExp exp =
    match exp with
      | IntExp intVal -> mov RAX (Imm intVal)
      | UnaryExp(op,exp) ->
        match op with
        | Neg ->
          genExp exp
          neg RAX
        | _ -> failwith "not implemented"
      | BinExp(exp1, op, exp2) -> 
          genExp exp1
          emit "push rax     \t\t; save left operand"
          genExp exp2
          emit "mov rcx, rax \t\t; move right operand into rcx"
          emit "pop rax      \t\t; restore left operand"
          genBinOp op
      | VarExp (AST.ID name) -> genVarExp name
      | _ -> failwith "not implemented"
       
  let genStmt stmt =
    match stmt with
    | ReturnStmt returnStmt -> 
        emit "; function prologue"
        emit "push rbp"
        emit "mov rbp, rsp\n"
        emit "; function body"
        emit "push 20      \t\t; local variable declaration"
        genExp returnStmt
        emit ""
        emit "; function epilogue"
        emit "mov rsp, rbp"
        emit "pop rbp"
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
    emit "section .text"
    emitLabel funcName
    match f.body with
    | Block block -> genBlock block
    | _ -> failwith "expected Block"

  let genGlobalVar (gv:var_decl) =
    let (AST.ID name) = gv.id
    match gv.initExp with
    | None -> emitGlobalVar name 0
    | Some (IntExp intVal) -> emitGlobalVar name intVal
    | Some _ -> failwith "initializer must be a constant expression"

  let genTopLevel tl =
    match tl with
    | AST.Function f -> genFunction f
    | AST.GlobalVar gv -> genGlobalVar gv

  let emitEntryPoint () =
    emitLabel "_start"
    emit "push 10"
    call "main"
    emit "mov rdi, rax \t\t; call exit with return code from main"
    emit "mov rax, 60  \t\t; sys_exit"
    emit "syscall"

  let rec genTopLevels topLevels =
    match topLevels with
    | [] -> emitEntryPoint ()
    | tl::tls ->
        genTopLevel tl
        genTopLevels tls

  // emit global symbols
  emitGlobal "_start"

  // emit code
  match ast with
  | AST.Program(program) -> genTopLevels program

  writer.Close()
