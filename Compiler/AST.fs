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

type exp =
  | IntExp of int
  | VarExp of string
  | UnaryExp of unary_op * exp
  | BinExp of exp * bin_op * exp

type var_decl = {
  id: string
  varType: type_specifier
  initExp: exp option
}

type statement =
  | ReturnStmt of exp

type block_item =
  | Statement of statement
  | LocalVar of var_decl

type param = Param of type_specifier * string

type fun_decl = {
  id: string
  funType: type_specifier
  parameters: param list
  body: block_item list
}

type top_level =
  | GlobalVar of var_decl
  | Func of fun_decl

type program = Program of top_level list
