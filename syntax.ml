type id = 
| Id of string

type ids = id list

(* Representation of matlab ast as we do splitting on it *)

type matlabvar = (* NOTE: make so matlabnode uses matlabvar instead of list *)
| MVar of id

type matlabnode =
| MMVar of matlabvar (* This case is used because splitting can bind variables on subnodes *)

| Stmt (* abstract *)
| ExprStmt of matlabnode
| AssignStmt of matlabnode * matlabnode
| GlobalStmt of matlabvar
| PersistentStmt of matlabvar
| ShellCommandStmt of id

| BreakStmt
| ContinueStmt
| ReturnStmt

| ForStmt of matlabnode * matlabvar
| WhileStt of matlabnode * matlabvar

| TryStmt of matlabvar * matlabvar

| SwitchStmt of matlabnode * matlabnode * matlabnode option
| SwitchCaseBlock of matlabnode * matlabnode
| DefaultCaseBlock of matlabnode list
| IfStmt of matlabnode list * matlabnode option
| IfBlock of matlabnode * matlabnode
| ElseBlock of matlabnode

| Expr (* abstract *)
| RangeExpr of matlabnode * matlabnode option * matlabnode
| ColonExpr
| EndExpr

| LValueExpr
| NameExpr of id
| ParametrizedExpr of matlabnode * matlabnode list
| CellIndexExpr of matlabnode * matlabnode list
| DotExpr of matlabnode * id
| MatrixExpr of matlabnode list

| CellArrayExpr of matlabnode list
| SuperClassMethodExpr of id * id

| Row of matlabnode list

| LiteralExpr (* abstract *)
| IntLiteralExpr (* IntNumericLiteralValue ... *)
| FPLiteralExpr (* FPNumericLiteralValue ... *)
| StringLiteralExpr of string

| UnaryExpr of matlabnode (* abstract *)
| UMinusExpr of matlabnode
| UPlusExpr of matlabnode
| NotExpr of matlabnode
| MTransposeExpr of matlabnode
| ArrayTransposeExpr of matlabnode

| BinaryExpr of matlabnode * matlabnode
| PlusExpr of matlabnode * matlabnode
| MinusExpr of matlabnode * matlabnode

| MTimesExpr of matlabnode * matlabnode
| MDivExpr of matlabnode * matlabnode
| MLDiveExpr of matlabnode * matlabnode
| MPowExpr of matlabnode * matlabnode

| ETimesExpr of matlabnode * matlabnode
| EDivExpr of matlabnode * matlabnode
| ELDivExpr of matlabnode * matlabnode
| EPowExpr of matlabnode * matlabnode

| AndExpr of matlabnode * matlabnode
| OrExpr of matlabnode * matlabnode

| ShortCircuitAndExpr of matlabnode * matlabnode
| ShortCircuitOrExpr of matlabnode * matlabnode

| LTExpr of matlabnode * matlabnode
| GTExpr of matlabnode * matlabnode
| LEExpr of matlabnode * matlabnode
| GEExpr of matlabnode * matlabnode
| EQExpr of matlabnode * matlabnode
| NEExpr of matlabnode * matlabnode

| FunctionHandleExpr of id
| LambdaExpr of id list * matlabnode

(* DSL specific syntax *)

type direction =
| Forward
| Backward

type domain =
| Set of domain
| Tuple of domain * domain
| Name of id

type op = 
| Plus of op * op
| Minus of op * op
| Times of op * op
| Var of id

type expr = 
| EmptySet
| Set of expr
| Tuple of expr * expr
(*
| Stmt of matlabnode
| Expr of matlabnode
*)
| Op of op

type cond =
| True
| False
| Eq of expr * expr

type stmt =
| If of cond * stmt list
| For of id * domain * stmt list
| Assign of id * expr

type init = 
| NoInit

type merge =
| Merge of id * id * expr

type flow =
| NoFlow
| Flow of id * stmt * (matlabnode * stmt list) list

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

type analyses = analysis list

type program = 
| Program of analyses
| Empty
(*enums isn't right. Need to think about natural representation of lattice *)
