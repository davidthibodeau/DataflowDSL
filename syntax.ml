module MatlabAst =
  struct

    (* Used for types *)
    type matlabast = 
    | Stmt
    | ExprStmt
    | AssignStmt
    | GlobalStmt
    | PersistentStmt
    | ShellCommandStmt
    | BreakStmt
    | ContinueStmt
    | ReturnStmt
    | ForStmt
    | WhileStmt
    | TryStmt
    | SwitchStmt
    | SwitchCaseBlock
    | DefaultCaseBlock
    | IfStmt
    | IfBlock
    | ElseBlock
    | Expr
    | RangeExpr
    | ColonExpr
    | EndExpr
    | LValueExpr
    | NameExpr
    | ParameterizedExpr
    | CellIndexExpr
    | DotExpr
    | MatrixExpr
    | CellArrayExpr
    | SuperClassMethodExpr
    | Row
    | LiteralExpr
    | IntLiteralExpr
    | FPLiteralExpr
    | StringLiteralExpr
    | UnaryExpr
    | UMinusExpr
    | UPlusExpr
    | NotExpr
    | MTransposeExpr
    | ArrayTransposeExpr
    | BinaryExpr
    | PlusExpr
    | MinusExpr
    | MTimesExpr
    | MDivExpr
    | MLDivExpr
    | MPowExpr
    | ETimesExpr
    | EDivExpr
    | ELDivExpr
    | EPowExpr
    | AndExpr
    | OrExpr
    | ShortCircuitAndExpr
    | ShortCircuitOrExpr
    | LTExpr
    | GTExpr
    | LEExpr
    | GEExpr
    | EQExpr
    | NEExpr
    | FunctionHandleExpr
    | LambdaExpr

  end

type id = 
| Id of string

type ids = id list

(* Representation of matlab ast as we do splitting on it *)

type mpat = 
| Var of id
| Node of mnode
| NodeAs of (id * mnode)

and mnode =
| Stmt (* abstract *)
| ExprStmt of mpat
| AssignStmt of mpat * mpat
| GlobalStmt of id
| PersistentStmt of id
| ShellCommandStmt of id

| BreakStmt
| ContinueStmt
| ReturnStmt

| ForStmt of mpat * id
| WhileStmt of mpat * id

| TryStmt of id * id

| SwitchStmt of mpat * mpat * mpat
| SwitchCaseBlock of mpat * mpat
| DefaultCaseBlock of id
| IfStmt of mpat list * mpat option
| IfBlock of mpat * id
| ElseBlock of id

| Expr (* abstract *)
| RangeExpr of mpat * mpat option * mpat
| ColonExpr
| EndExpr

| LValueExpr
| NameExpr of mpat
| ParameterizedExpr of mpat * mpat list
| CellIndexExpr of mpat * mpat list
| DotExpr of mpat * id
| MatrixExpr of mpat list

| CellArrayExpr of mpat list
| SuperClassMethodExpr of id * id

| Row of mpat list

| LiteralExpr (* abstract *)
| IntLiteralExpr (* IntNumericLiteralValue ... *)
| FPLiteralExpr (* FPNumericLiteralValue ... *)
| StringLiteralExpr of string

| UnaryExpr of mpat (* abstract *)
| UMinusExpr of mpat
| UPlusExpr of mpat
| NotExpr of mpat
| MTransposeExpr of mpat
| ArrayTransposeExpr of mpat

| BinaryExpr of mpat * mpat
| PlusExpr of mpat * mpat
| MinusExpr of mpat * mpat

| MTimesExpr of mpat * mpat
| MDivExpr of mpat * mpat
| MLDivExpr of mpat * mpat
| MPowExpr of mpat * mpat

| ETimesExpr of mpat * mpat
| EDivExpr of mpat * mpat
| ELDivExpr of mpat * mpat
| EPowExpr of mpat * mpat

| AndExpr of mpat * mpat
| OrExpr of mpat * mpat

| ShortCircuitAndExpr of mpat * mpat
| ShortCircuitOrExpr of mpat * mpat

| LTExpr of mpat * mpat
| GTExpr of mpat * mpat
| LEExpr of mpat * mpat
| GEExpr of mpat * mpat
| EQExpr of mpat * mpat
| NEExpr of mpat * mpat

| FunctionHandleExpr of id
| LambdaExpr of id list * mpat


(* DSL specific syntax *)

type direction =
| Forward
| Backward

type domain =
| Set of domain
| Tuple of domain * domain
| Matlab of MatlabAst.matlabast
| Name of id

type typexpr = 
| EmptySet
| Set of expr
| Tuple of expr * expr
| Plus of expr * expr
| Minus of expr * expr
| Times of expr * expr
| Var of id

and expr =
| NoTyp of typexpr
| Typ of typexpr * domain

type cond =
| True
| False
| Eq of expr * expr

type stmt =
| If of cond * stmt list
| For of mpat * id * stmt list
| Assign of id * expr

type init = 
| NoInit

type merge =
| Merge of id * id * expr

type flow =
| NoFlow
| Flow of id * stmt * (mnode * stmt list) list

type aux =
| NoAux

type body =
| I of init
| M of merge
| F of flow
| A of aux

type bodies =
| Body of init * merge * flow * aux list

type analysis =
| Analysis of id * direction * domain * bodies

type program = 
| Program of analysis list
| Empty
(*enums isn't right. Need to think about natural representation of lattice *)
