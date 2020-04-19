module CodeGen

open AST
open System
open System.Text

type VarLocation =
  | Global of string * int  // label and initialized value
  | Local of int            // offset from stack frame base pointer

type FunctionInfo = {
  label: string;
  paramCount: int
}

type Environment = {
  vars: Map<string, VarLocation>
  functions: Map<string, FunctionInfo>
  outerEnv: Environment option // the global env has no outerEnv
}

let gen (ast:program) =
  let sb = new StringBuilder()

  let emit (text:string) =
    sb.AppendFormat ("\t{0}\n", text) |> ignore

  let emitLabel (label:string) =
    sb.AppendFormat ("{0}:\n", label) |> ignore

  let rec lookupVar name env =
    let found = env.vars.TryFind name
    match found with
    | Some location -> location
    | None -> 
      match env.outerEnv with
      | Some nextEnv -> lookupVar name nextEnv
      | None -> failwithf "variable not found: %s" name

  let rec lookupFunc name env =
    let found = env.functions.TryFind name
    match found with
    | Some funcInfo -> funcInfo
    | None -> 
      match env.outerEnv with
      | Some nextEnv -> lookupFunc name nextEnv
      | None -> failwithf "function not found: %s" name
     
  let rec genExp exp env =
    let genBinOp op =
      match op with
      | Add -> emit "add rax, rcx"
      | Sub -> emit "sub rax, rcx"
      | Mul -> emit "mul rcx"
      | Div -> emit "div rcx"
      | _   -> failwith "not implemented"

    let genIntExp intVal env =
      emit ("mov rax, " + intVal.ToString())
      env

    let genUnaryExp op exp env =
      match op with
      | Neg ->
          genExp exp env |> ignore
          emit "neg rax"
          env
      | Not ->
          genExp exp env |> ignore
          emit "mov rcx, rax  \t\t; move value to test into rcx"
          emit "xor rax, rax  \t\t; clear rax"
          emit "test rcx, rcx \t\t; bitwise AND rcx with itself"
          emit "setz al       \t\t; set rax to 1 if rcx == 0, otherwise set to 0"
          env

    let genBinExp exp1 op exp2 env =
      genExp exp1 env |> ignore
      emit "push rax     \t\t; save left operand"
      genExp exp2 env |> ignore
      emit "mov rcx, rax \t\t; move right operand into rcx"
      emit "pop rax      \t\t; restore left operand"
      genBinOp op
      env

    let genVarExp name env =
      let varLocation = lookupVar name env
      let code = 
        match varLocation with
        | Global (label,_)             -> sprintf "mov rax, [%s]      \t; get global variable: %s" label name
        | Local offset when offset < 0 -> sprintf "mov rax, [rbp - %d]\t; get local variable: %s" (-offset) name
        | Local offset                 -> sprintf "mov rax, [rbp + %d]\t; get arg: %s" offset name
      emit code
      env

    let genFuncCall name expList env = 
      let funcInfo = lookupFunc name env
      let numParams = funcInfo.paramCount
      let numArgs = List.length expList
      if numArgs <> numParams then
        failwithf "number of args (%d) does not match number of function parameters (%d)" numArgs numParams

      // push args onto stack
      for exp in (List.rev expList) do
        genExp exp env |> ignore
        emit "push rax  \t\t; push arg"

      // make the actual call
      emit ("call " + funcInfo.label)

      // clean up stack
      let offset = (numArgs*8).ToString()
      let comment = sprintf "\t\t; clean up stack space for %d args" numArgs
      emit ("add rsp, " + offset + comment)
      env

    match exp with
      | IntExp intVal                 -> genIntExp intVal env
      | VarExp (ID name)              -> genVarExp name env
      | UnaryExp(op,exp)              -> genUnaryExp op exp env          
      | BinExp(exp1, op, exp2)        -> genBinExp exp1 op exp2 env          
      | FunCallExp (ID name, expList) -> genFuncCall name expList env
       
  let genStmt stmt env =
    let genAssignStmt name exp env =
      let varLocation = lookupVar name env
      let newEnv = genExp exp env
      let code = 
        match varLocation with
        | Global (label,_) -> sprintf "mov [%s], rax      \t; assign global variable" label
        | Local offset     -> sprintf "mov [rbp + %d], rax\t; assign local variable or arg" offset
      emit code
      newEnv

    match stmt with
    | AssignStmt (ID name,exp) -> genAssignStmt name exp env
    | Block _                  -> failwith "not implemented"
    | ExpStmt _                -> failwith "not implemented"
    | IfStmt _                 -> failwith "not implemented"
    | IfElseStmt _             -> failwith "not implemented"
    | WhileStmt _              -> failwith "not implemented"
    | ReturnStmt exp           -> genExp exp env  
  
  let genBlockItem blockItem env =
    let genLocalVarDecl (var:var_decl) env =
      let (ID name) = var.id

      // check if already defined in this scope
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
    
    match blockItem with
    | Statement stmt -> genStmt stmt env
    | LocalVar var -> genLocalVarDecl var env

  let rec genBlock block env =
    match block with
    | [] -> env
    | blockItem::blockItems -> genBlock blockItems (genBlockItem blockItem env)  

  let genTopLevel tl env =
    let genFunction f env =
      let (ID funcName) = f.id
      let paramCount = f.paramList.Length

      // check if already defined
      let found = env.functions.TryFind funcName
      let label = "func_" + funcName
      let funcMap =
        match (found,env.outerEnv) with
        | (_,Some _)  -> failwith "expected global env"
        | (Some _, _) -> failwithf "function already defined: %s" funcName
        | (None,_) -> env.functions.Add (funcName,{label=label; paramCount=paramCount})

      let updatedEnv = {env with functions=funcMap}

      // we expect the caller to push the correct number of args on the stack
      let rec allocArgs paramList (varsMap:Map<string, VarLocation>) i =
        match paramList with
        | [] -> varsMap
        | (Param (_,ID name))::rest ->
            let newMap = varsMap.Add (name, Local (8*(2 + i)))
            allocArgs rest newMap (i+1)
      let funcVarsMap = allocArgs f.paramList Map.empty 0
      let extendedEnv = { vars=funcVarsMap; functions=Map.empty; outerEnv=Some(updatedEnv) }

      // prologue   
      emitLabel label
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
      emit "ret"
      updatedEnv

    let genGlobalVar (gv:var_decl) env =
      // check if already defined
      let (ID name) = gv.id
      let found = env.vars.TryFind name
      match (found,env.outerEnv) with
      | (_,Some _)  -> failwith "expected global env"
      | (Some _, _) -> failwithf "global variable already defined: %s" name
      | (None,_) -> ()

      // add to global env
      let newVars = 
        match gv.initExp with
        | None -> env.vars.Add (name, Global ("var_" + name,0))
        | Some (IntExp intVal) -> env.vars.Add ("var_" + name, Global (name,intVal))
        | Some _ -> failwith "initializer must be a constant expression"
      {env with vars=newVars}

    match tl with
    | AST.Function f -> genFunction f env
    | AST.GlobalVar gv -> genGlobalVar gv env

  let rec genTopLevels topLevels env =
    match topLevels with
    | [] -> env
    | tl::tls -> genTopLevels tls (genTopLevel tl env)

  // emit code
  emit "; code"
  emit "section .text"
  let globalEnv = { vars=Map.empty; functions=Map.empty; outerEnv=None }
  let env =
    match ast with
    | AST.Program(program) -> genTopLevels program globalEnv

  // entry point
  let mainFunc = lookupFunc "main" env
  emit ""
  emit "; entry point"
  emit "global _start"
  emitLabel "_start"
  emit ("call " + mainFunc.label)
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

  // combine the 2 asm parts into 1
  sbFinal.Append (sb.ToString()) |> ignore
  sbFinal.ToString()
