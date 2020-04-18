module CodeGen

open AST
open System
open System.Text

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

let gen (ast:program) =
  let sb = new StringBuilder()

  let emit (text:string) =
    sb.AppendFormat ("\t{0}\n", text) |> ignore

  let emitLabel (label:string) =
    sb.AppendFormat ("{0}:\n", label) |> ignore

  let emitGlobalVar name value =
    sb.AppendFormat ("{0}: db {1}\n", name, value) |> ignore

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
    let varLocation = lookupVar name env
    let code = 
      match varLocation with
      | Global (label,_)-> sprintf "mov rax, [%s]      \t; get global variable" label
      | Local offset    -> sprintf "mov rax, [rbp + %d]\t; get local variable or arg" offset
    emit code
    env
     
  let rec genExp exp env =
    match exp with
      | IntExp intVal -> 
          mov RAX (Imm intVal)
          env
      | VarExp (AST.ID name) -> genVarExp name env
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
      | FunCallExp _ -> failwith ""
       
  let genAssignStmt name exp env =
    let varLocation = lookupVar name env
    let newEnv = genExp exp env
    let code = 
      match varLocation with
      | Global (label,_)-> sprintf "mov [%s], rax      \t; assign global variable" label
      | Local offset    -> sprintf "mov [rbp + %d], rax\t; assign local variable or arg" offset
    emit code
    newEnv

  let genStmt stmt env =
    match stmt with
    | AssignStmt (AST.ID name,exp) -> genAssignStmt name exp env
    | Block _ -> failwith "not implemented"
    | ExpStmt _ -> failwith "not implemented"
    | IfStmt _ -> failwith "not implemented"
    | IfElseStmt _ -> failwith "not implemented"
    | WhileStmt _ -> failwith "not implemented"
    | ReturnStmt exp -> genExp exp env
      
  let genLocalVarDecl (var:var_decl) env =
    // check if already defined in this scope
    let (AST.ID name) = var.id
    let found = env.vars.TryFind name
    match found with
    | Some _ -> failwithf "local variable already defined: %s" name
    | None -> ()

    // add to current env
    let index = -8 * (env.vars.Count + 1)
    let newVars = env.vars.Add (name, Local index)
    let newEnv = {env with vars=newVars}

    if var.initExp.IsSome then
      genExp var.initExp.Value newEnv |> ignore
      emit (sprintf "push rax      \t\t; initialize new variable: %s" name)

    newEnv

  let genBlockItem blockItem env =
    match blockItem with
    | Statement stmt -> genStmt stmt env
    | LocalVar var -> genLocalVarDecl var env

  let rec genBlock block env =
    match block with
    | [] -> env
    | blockItem::blockItems -> genBlock blockItems (genBlockItem blockItem env)

  let genFunction f env =
    // check if already defined
    let (AST.ID funcName) = f.id
    let found = env.functions.TryFind funcName
    let funcMap =
      match (found,env.outerEnv) with
      | (_,Some _)  -> failwith "expected global env"
      | (Some _, _) -> failwithf "function already defined: %s" funcName
      | (None,_) -> env.functions.Add (funcName,true)

    let updatedEnv = {env with functions=funcMap}
    let extendedEnv = { vars=Map.empty; functions=Map.empty; outerEnv=Some(updatedEnv) }

    // prologue   
    emitLabel funcName
    emit "; function prologue"
    emit "push rbp"
    emit "mov rbp, rsp\n"

    // body
    emit "; function body"
    match f.body with
    | Block block -> genBlock block extendedEnv |> ignore
    | _ -> failwith "expected Block"

    // epilogue
    emit "" 
    emit "; function epilogue"
    emit "mov rsp, rbp"
    emit "pop rbp"
    ret ()
    updatedEnv

  let genGlobalVar (gv:var_decl) env =
    // check if already defined
    let (AST.ID name) = gv.id
    let found = env.vars.TryFind name
    match (found,env.outerEnv) with
    | (_,Some _)  -> failwith "expected global env"
    | (Some _, _) -> failwithf "global variable already defined: %s" name
    | (None,_) -> ()

    // add to global env
    let newVars = 
      match gv.initExp with
      | None -> env.vars.Add (name, Global (name,0))
      | Some (IntExp intVal) -> env.vars.Add (name, Global (name,intVal))
      | Some _ -> failwith "initializer must be a constant expression"
    {env with vars=newVars}

  let genTopLevel tl env =
    match tl with
    | AST.Function f -> genFunction f env
    | AST.GlobalVar gv -> genGlobalVar gv env

  let rec genTopLevels topLevels env =
    match topLevels with
    | [] -> env
    | tl::tls -> genTopLevels tls (genTopLevel tl env)

  let globalEnv = { vars=Map.empty; functions=Map.empty; outerEnv=None }

  // emit code
  emit "; code"
  emit "section .text"
  let env =
    match ast with
    | AST.Program(program) -> genTopLevels program globalEnv

  // entry point
  emit ""
  emit "; entry point"
  emitGlobal "_start"
  emitLabel "_start"
  call "main"
  emit "mov rdi, rax \t\t; call exit with return code from main"
  emit "mov rax, 60  \t\t; sys_exit"
  emit "syscall"

  // global variables
  let sbFinal = new StringBuilder()
  sbFinal.Append "\t; global variables\n" |> ignore
  sbFinal.Append "\tsection .data\n" |> ignore
  for KeyValue(_,v) in env.vars do
    match v with
    | Global (label,initVal) -> sbFinal.AppendFormat ("{0}: db {1}\n", label, initVal) |> ignore
    | _ -> failwith "expected global variable"
  sbFinal.Append "\n" |> ignore

  sbFinal.Append (sb.ToString()) |> ignore
  sbFinal.ToString()
