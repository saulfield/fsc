module AST

type unary_op =
  | Neg
  | Not

type bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Gt
  | Lt
  | Eq
  | NotEq
  | LtEq
  | GtEq

type type_specifier =
  | TypeChar
  | TypeDouble
  | TypeFloat
  | TypeInt
  | TypeVoid

type id = ID of string

type exp =
  //| StringExp of string
  | IntExp of int
  | VarExp of id
  | UnaryExp of unary_op * exp
  | BinExp of exp * bin_op * exp
  | FunCallExp of id * exp list

type var_decl = {
  id: id
  varType: type_specifier
  initExp: exp option
}

type block_item =
  | Statement of statement
  | LocalVar of var_decl

//and block = block_item list

and statement =
  | AssignStmt of id * exp
  | Block of block_item list
  | ExpStmt of exp
  | IfStmt of exp * statement
  | IfElseStmt of exp * statement * statement
  | WhileStmt of exp * statement
  | ReturnStmt of exp

type param = Param of type_specifier * id

type fun_decl = {
  id: id
  funType: type_specifier
  paramList: param list
  body: statement
}

type top_level =
  | GlobalVar of var_decl
  | Function of fun_decl

type program = Program of top_level list
