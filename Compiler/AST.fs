module AST

type unary_op =
  | Neg
  | Not

type bin_op =
  | Add
  | Sub
  | Mul
  | Div

type type_specifier =
  | TypeChar
  | TypeDouble
  | TypeFloat
  | TypeInt
  | TypeVoid

type id = ID of string

type exp =
  | StringExp of string
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

type statement =
  | ExpStmt of exp
  | ReturnStmt of exp

type block_item =
  | Statement of statement
  | LocalVar of var_decl

type param = Param of type_specifier * id

type fun_decl = {
  id: id
  funType: type_specifier
  parameters: param list
  body: block_item list
}

type top_level =
  | GlobalVar of var_decl
  | Func of fun_decl

type program = Program of top_level list
