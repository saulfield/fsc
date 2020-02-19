module AST

type UnaryOp =
  | Neg
  | Not

 type TypeSpecifier =
  | TypeVoid
  | TypeChar
  | TypeBool
  | TypeInt
  | TypeFloat
  | TypeDouble

type Exp =
  | UnaryExp of UnaryOp * Exp
  | IntExp of int

type Stmt =
  | ReturnStmt of Exp

type FuncDecl = {
  Ident:string;
  Stmt:Stmt
}