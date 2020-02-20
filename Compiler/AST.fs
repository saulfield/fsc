module AST

// Misc

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

// Expressions

type Exp =
  | UnaryExp of UnaryOp * Exp
  | IntExp of int

// Statements

type Stmt =
  | ReturnStmt of Exp

// Declarations

type FunDecl = {
  Ident:string;
  Stmt:Stmt
}

type VarDecl = {
  Type: TypeSpecifier;
  Ident: string
}

type Decl =
  | VarDecl of VarDecl
  | FunDecl of FunDecl
