type id = string

module Ast = 
  struct
    type ast =
      | Name
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

    type vpat = 
      | Var of id
      | Node of pat
      | NodeAs of (id * pat)
    and pat = ast * vpat list

(* Maybe should be mixed with gatherPat to do only one case analysis both?
    let wellformed (a, vl) = match a, vl with
    | Stmt, [] ->
*)

    let printtp = function
      | Name -> "Name"
      | Stmt -> "Stmt"
      | ExprStmt -> "ExprStmt"
      | AssignStmt -> "AssignStmt"
      | GlobalStmt -> "GlobalStmt"
      | PersistentStmt -> "PersistentStmt"
      | ShellCommandStmt -> "ShellCommandStmt"
      | BreakStmt -> "BreakStmt"
      | ContinueStmt -> "ContinueStmt"
      | ReturnStmt -> "ReturnStmt"
      | ForStmt -> "ForStmt"
      | WhileStmt -> "WhileStmt"
      | TryStmt -> "TryStmt"
      | SwitchStmt -> "SwitchStmt"
      | SwitchCaseBlock -> "SwitchCaseBlock"
      | DefaultCaseBlock -> "DefaultCaseBlock"
      | IfStmt -> "IfStmt"
      | IfBlock -> "IfBlock"
      | ElseBlock -> "ElseBlock"
      | Expr -> "Expr"
      | RangeExpr -> "RangeExpr"
      | ColonExpr -> "ColonExpr"
      | EndExpr -> "EndExpr"
      | LValueExpr -> "LValueExpr"
      | NameExpr -> "NameExpr"
      | ParameterizedExpr -> "ParameterizedExpr"
      | CellIndexExpr -> "CellIndexExpr"
      | DotExpr -> "DotExpr"
      | MatrixExpr -> "MatrixExpr"
      | CellArrayExpr -> "CellArrayExpr"
      | SuperClassMethodExpr -> "SuperClassMethodExpr"
      | Row -> "Row"
      | LiteralExpr -> "LiteralExpr"
      | IntLiteralExpr -> "IntLiteralExpr"
      | FPLiteralExpr -> "FPLiteralExpr"
      | StringLiteralExpr -> "StringLiteralExpr"
      | UnaryExpr -> "UnaryExpr"
      | UMinusExpr -> "UMinusExpr"
      | UPlusExpr -> "UPlusExpr"
      | NotExpr -> "NotExpr"
      | MTransposeExpr -> "MTransposeExpr"
      | ArrayTransposeExpr -> "ArrayTransposeExpr"
      | BinaryExpr -> "BinaryExpr"
      | PlusExpr -> "PlusExpr"
      | MinusExpr -> "MinusExpr"
      | MTimesExpr -> "MTimesExpr"
      | MDivExpr -> "MDivExpr"
      | MLDivExpr -> "MLDivExpr"
      | MPowExpr -> "MPowExpr"
      | ETimesExpr -> "ETimesExpr"
      | EDivExpr -> "EDivExpr"
      | ELDivExpr -> "ELDivExpr"
      | EPowExpr -> "EPowExpr"
      | AndExpr -> "AndExpr"
      | OrExpr -> "OrExpr"
      | ShortCircuitAndExpr -> "ShortCircuitAndExpr"
      | ShortCircuitOrExpr -> "ShortCircuitOrExpr"
      | LTExpr -> "LTExpr"
      | GTExpr -> "GTExpr"
      | LEExpr -> "LEExpr"
      | GEExpr -> "GEExpr"
      | EQExpr -> "EQExpr"
      | NEExpr -> "NEExpr"
      | FunctionHandleExpr -> "FunctionHandleExpr"
      | LambdaExpr -> "LambdaExpr"
            
  end

(*      | Name of id
      | Stmt (* abstract *)
      | ExprStmt of vpat
      | AssignStmt of vpat * vpat
      | GlobalStmt of id
      | PersistentStmt of id
      | ShellCommandStmt of id      
      | BreakStmt
      | ContinueStmt
      | ReturnStmt
      | ForStmt of vpat * id
      | WhileStmt of vpat * id
      | TryStmt of id * id
      | SwitchStmt of vpat * vpat * vpat
      | SwitchCaseBlock of vpat * vpat
      | DefaultCaseBlock of id
      | IfStmt of vpat list * vpat option
      | IfBlock of vpat * id
      | ElseBlock of id
      | Expr (* abstract *)
      | RangeExpr of vpat * vpat option * vpat
      | ColonExpr
      | EndExpr
      | LValueExpr
      | NameExpr of vpat
      | ParameterizedExpr of vpat * vpat list
      | CellIndexExpr of vpat * vpat list
      | DotExpr of vpat * id
      | MatrixExpr of vpat list
      | CellArrayExpr of vpat list
      | SuperClassMethodExpr of id * id
      | Row of vpat list
      | LiteralExpr (* abstract *)
      | IntLiteralExpr (* IntNumericLiteralValue ... *)
      | FPLiteralExpr (* FPNumericLiteralValue ... *)
      | StringLiteralExpr of string
      | UnaryExpr of vpat (* abstract *)
      | UMinusExpr of vpat
      | UPlusExpr of vpat
      | NotExpr of vpat
      | MTransposeExpr of vpat
      | ArrayTransposeExpr of vpat
      | BinaryExpr of vpat * vpat
      | PlusExpr of vpat * vpat
      | MinusExpr of vpat * vpat
      | MTimesExpr of vpat * vpat
      | MDivExpr of vpat * vpat
      | MLDivExpr of vpat * vpat
      | MPowExpr of vpat * vpat
      | ETimesExpr of vpat * vpat
      | EDivExpr of vpat * vpat
      | ELDivExpr of vpat * vpat
      | EPowExpr of vpat * vpat
      | AndExpr of vpat * vpat
      | OrExpr of vpat * vpat
      | ShortCircuitAndExpr of vpat * vpat
      | ShortCircuitOrExpr of vpat * vpat
      | LTExpr of vpat * vpat
      | GTExpr of vpat * vpat
      | LEExpr of vpat * vpat
      | GEExpr of vpat * vpat
      | EQExpr of vpat * vpat
      | NEExpr of vpat * vpat
      | FunctionHandleExpr of id
      | LambdaExpr of id list * vpat

    let rec decidetp = function 
      | Stmt -> Stmt
      | ExprStmt _ -> ExprStmt
      | AssignStmt _ -> AssignStmt
      | GlobalStmt _ -> GlobalStmt 
      | PersistentStmt _ -> PersistentStmt 
      | ShellCommandStmt _ -> ShellCommandStmt
      | BreakStmt -> BreakStmt 
      | ContinueStmt -> ContinueStmt 
      | ReturnStmt -> ReturnStmt 
      | ForStmt _ -> ForStmt 
      | WhileStmt _ ->  WhileStmt
      | TryStmt _ -> TryStmt
      | SwitchStmt _ -> SwitchStmt
      | SwitchCaseBlock _ -> SwitchCaseBlock
      | DefaultCaseBlock _ -> DefaultCaseBlock
      | IfStmt _ -> IfStmt 
      | IfBlock _ -> IfBlock 
      | ElseBlock _ -> ElseBlock
      | Expr -> Expr 
      | RangeExpr _ -> RangeExpr
      | ColonExpr -> ColonExpr
      | EndExpr -> EndExpr
      | LValueExpr -> LValueExpr 
      | NameExpr _ -> NameExpr 
      | ParameterizedExpr _ -> ParameterizedExpr 
      | CellIndexExpr _ -> CellIndexExpr 
      | DotExpr _ -> DotExpr 
      | MatrixExpr _ -> MatrixExpr
      | CellArrayExpr _ -> CellArrayExpr 
      | SuperClassMethodExpr _ -> SuperClassMethodExpr 
      | Row _ -> Row 
      | LiteralExpr -> LiteralExpr
      | IntLiteralExpr -> IntLiteralExpr
      | FPLiteralExpr -> FPLiteralExpr 
      | StringLiteralExpr _ -> StringLiteralExpr
      | UnaryExpr _ -> UnaryExpr 
      | UMinusExpr _ -> UMinusExpr
      | UPlusExpr _ -> UPlusExpr 
      | NotExpr _ -> NotExpr
      | MTransposeExpr _ -> MTransposeExpr
      | ArrayTransposeExpr _ -> ArrayTransposeExpr
      | BinaryExpr _ -> BinaryExpr 
      | PlusExpr _ -> PlusExpr 
      | MinusExpr _ -> MinusExpr
      | MTimesExpr _ -> MTimesExpr
      | MDivExpr _ -> MDivExpr 
      | MLDivExpr _ -> MLDivExpr
      | MPowExpr _ -> MPowExpr 
      | ETimesExpr _ -> ETimesExpr
      | EDivExpr _ -> EDivExpr 
      | ELDivExpr _ -> ELDivExpr
      | EPowExpr _ -> EPowExpr 
      | AndExpr _ -> AndExpr 
      | OrExpr _ -> OrExpr 
      | ShortCircuitAndExpr _ -> ShortCircuitAndExpr
      | ShortCircuitOrExpr _ -> ShortCircuitOrExpr 
      | LTExpr _ -> LTExpr
      | GTExpr _ -> GTExpr
      | LEExpr _ -> LEExpr
      | GEExpr _ -> GEExpr
      | EQExpr _ -> EQExpr
      | NEExpr _ -> NEExpr
      | FunctionHandleExpr _ -> FunctionHandleExpr
      | LambdaExpr _ -> LambdaExpr
*) 
