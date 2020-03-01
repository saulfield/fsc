module AST

type unary_op =
  | Neg
  | Not

type type_specifier =
  | TypeChar
  | TypeDouble
  | TypeFloat
  | TypeInt
  | TypeVoid

type exp =
  | IntExp of int
  | UnaryExp of unary_op * exp

type var_decl = {
  declType: type_specifier;
  id: string;
  init: exp option
}

type statement =
  | ReturnStmt of exp

type block_item =
  | Statement of statement
  | LocalVar of var_decl

type param = Param of type_specifier * string

type fun_decl = {
  declType: type_specifier;
  parameters: param list
  id: string;
  body: block_item list
}

type top_level =
  | GlobalVar of var_decl
  | Func of fun_decl

type program = Program of top_level list
