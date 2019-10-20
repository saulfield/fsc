module AST

type Exp =
  | IntExp of int

type Stmt =
  | ReturnStmt of Exp

type FuncDecl = {
  Ident:string;
  Stmt:Stmt
}