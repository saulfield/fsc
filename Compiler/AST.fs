module AST

// Misc

type UnaryOp =
  | Neg
  | Not

 type TypeSpecifier =
  | TypeChar
  | TypeDouble
  | TypeFloat
  | TypeInt
  | TypeVoid

// Expressions

type Exp =
  | UnaryExp of UnaryOp * Exp
  | IntExp of int

// Statements

type Stmt =
  | ReturnStmt of Exp

// Declarations

type FunDecl = {
  Type: TypeSpecifier;
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

// Top level
type Program = Program of Decl list
