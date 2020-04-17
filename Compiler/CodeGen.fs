module CodeGen

open AST
open System

type Operand =
  | RAX
  | RDI
  | Imm of int

type VarLocation =
  | Global of string * int  // name and initialized value
  | Local of int            // offset from stack frame base pointer

type Environment = {
  vars: Map<string, VarLocation>
  functions: Map<string, bool>
  outerEnv: Environment option
}

let operandToString operand =
  match operand with
  | Imm imm -> imm.ToString()
  | _ -> operand.ToString().ToLower() 

let gen (ast:program) filename =
  let writer = new IO.StreamWriter(path=filename)

  let emit text =
    printfn "\t%s" text
    fprintfn writer "\t%s" text

  let emitLabel label =
    printfn "%s:" label
    fprintfn writer "%s:" label

  let emitGlobalVar name value =    
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

  let rec lookupVar name env =
    let found = env.vars.TryFind name
    match found with
    | Some location -> location
    | None -> 
      match env.outerEnv with
      | Some nextEnv -> lookupVar name nextEnv
      | None -> failwithf "variable not found: %s" name

  let genVarExp name env =
    let location = lookupVar name env
    let code = 
      match location with
      | Global (label,_)-> sprintf "mov rax, [%s]   \t; global variable" label
      | Local offset -> sprintf "mov rax, [rbp + %d]\t; local variable or arg" offset
    emit code
    env

  let rec genExp exp env =
    match exp with
      | IntExp intVal -> 
          mov RAX (Imm intVal)
          env
      | UnaryExp(op,exp) ->
        match op with
        | Neg ->
          genExp exp env |> ignore
          neg RAX
          env
        | _ -> failwith "not implemented"
      | BinExp(exp1, op, exp2) -> 
          genExp exp1 env |> ignore
          emit "push rax     \t\t; save left operand"
          genExp exp2 env |> ignore
          emit "mov rcx, rax \t\t; move right operand into rcx"
          emit "pop rax      \t\t; restore left operand"
          genBinOp op
          env
      | VarExp (AST.ID name) -> genVarExp name env
      | _ -> failwith "not implemented"
       
  let genStmt stmt env =
    match stmt with
    | ReturnStmt returnStmt -> 
        emit "; function prologue"
        emit "push rbp"
        emit "mov rbp, rsp\n"
        emit "; function body"
        //emit "push 20      \t\t; local variable declaration"
        genExp returnStmt env  |> ignore
        emit "" 
        emit "; function epilogue"
        emit "mov rsp, rbp"
        emit "pop rbp"
        ret ()
        env
    | _ -> failwith "not implemented"
      
  let genBlockItem blockItem env =
    match blockItem with
    | Statement stmt -> genStmt stmt env
    | LocalVar var -> failwith ""

  let rec genBlock block env =
    match block with
    | [] -> env
    | blockItem::blockItems -> genBlock blockItems (genBlockItem blockItem env)

  let genFunction f env =
    let (AST.ID funcName) = f.id
    emit "section .text"
    emitLabel funcName
    match f.body with
    | Block block -> genBlock block env
    | _ -> failwith "expected Block"

  let genGlobalVar (gv:var_decl) env =
    let (AST.ID name) = gv.id

    // check if already defined
    let found = env.vars.TryFind name
    match (found,env.outerEnv) with
    | (_,Some _)  -> failwith "expected global env"
    | (Some _, _) -> failwith ("global variable already defined: " + name)
    | (None,_) -> ()

    // add to global env
    let newVars = 
      match gv.initExp with
      | None -> env.vars.Add(name, Global (name,0))
      | Some (IntExp intVal) -> env.vars.Add(name, Global (name,intVal))
      | Some _ -> failwith "initializer must be a constant expression"
    {env with vars=newVars}

  let genTopLevel tl env =
    match tl with
    | AST.Function f -> genFunction f env
    | AST.GlobalVar gv -> genGlobalVar gv env

  let emitEntryPoint () =
    emitLabel "_start"
    //emit "push 10"
    call "main"
    emit "mov rdi, rax \t\t; call exit with return code from main"
    emit "mov rax, 60  \t\t; sys_exit"
    emit "syscall"

  let rec genTopLevels topLevels env =
    match topLevels with
    | [] -> env
    | tl::tls -> genTopLevels tls (genTopLevel tl env)

  // emit global symbols and variables
  emitGlobal "_start"

  let globalEnv = { vars=Map.empty; functions=Map.empty; outerEnv=None }

  // emit code
  let env =
    match ast with
    | AST.Program(program) -> genTopLevels program globalEnv

  emitEntryPoint ()

  emit ""
  emit "; global variables"
  emit "section .data"
  for KeyValue(_,v) in env.vars do
    match v with
    | Global (label,initVal) -> emitGlobalVar label initVal
    | _ -> failwith "expected global variable"

  writer.Close()
