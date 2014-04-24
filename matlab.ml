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


    let keylist = [ "Name", Name;
                    "Stmt", Stmt;
                    "ExprStmt", ExprStmt;
                    "AssignStmt", AssignStmt;
                    "GlobalStmt", GlobalStmt;
                    "PersistentStmt", PersistentStmt;
                    "ShellCommandStmt", ShellCommandStmt;
                    "BreakStmt", BreakStmt;
                    "ContinueStmt", ContinueStmt;
                    "ReturnStmt", ReturnStmt;
                    "ForStmt", ForStmt;
                    "WhileStmt", WhileStmt;
                    "TryStmt", TryStmt;
                    "SwitchStmt", SwitchStmt;
                    "SwitchCaseBlock", SwitchCaseBlock;
                    "DefaultCaseBlock", DefaultCaseBlock;
                    "IfStmt", IfStmt;
                    "IfBlock", IfBlock;
                    "ElseBlock", ElseBlock;
                    "Expr", Expr;
                    "RangeExpr", RangeExpr;
                    "ColonExpr", ColonExpr;
                    "EndExpr", EndExpr;
                    "LValueExpr", LValueExpr;
                    "NameExpr", NameExpr;
                    "ParameterizedExpr", ParameterizedExpr;
                    "CellIndexExpr", CellIndexExpr;
                    "DotExpr", DotExpr;
                    "MatrixExpr", MatrixExpr;
                    "CellArrayExpr", CellArrayExpr;
                    "SuperClassMethodExpr", SuperClassMethodExpr;
                    "Row", Row;
                    "LiteralExpr", LiteralExpr;
                    "IntLiteralExpr", IntLiteralExpr;
                    "FPLiteralExpr", FPLiteralExpr;
                    "StringLiteralExpr", StringLiteralExpr;
                    "UnaryExpr", UnaryExpr;
                    "UMinusExpr", UMinusExpr;
                    "UPlusExpr", UPlusExpr;
                    "NotExpr", NotExpr;
                    "MTransposeExpr", MTransposeExpr;
                    "ArrayTransposeExpr", ArrayTransposeExpr;
                    "BinaryExpr", BinaryExpr;
                    "PlusExpr", PlusExpr;
                    "MinusExpr", MinusExpr;
                    "MTimesExpr", MTimesExpr;
                    "MDivExpr", MDivExpr;
                    "MLDivExpr", MLDivExpr;
                    "MPowExpr", MPowExpr;
                    "ETimesExpr", ETimesExpr;
                    "EDivExpr", EDivExpr;
                    "ELDivExpr", ELDivExpr;
                    "EPowExpr", EPowExpr;
                    "AndExpr", AndExpr;
                    "OrExpr", OrExpr;
                    "ShortCircuitAndExpr", ShortCircuitAndExpr;
                    "ShortCircuitOrExpr", ShortCircuitOrExpr;
                    "LTExpr", LTExpr;
                    "GTExpr", GTExpr;
                    "LEExpr", LEExpr;
                    "GEExpr", GEExpr;
                    "EQExpr", EQExpr;
                    "NEExpr", NEExpr;
                    "FunctionHandleExpr", FunctionHandleExpr;
                    "LambdaExpr", LambdaExpr
                  ] 

    let keywords = Hashtbl.create 50
    let _ = List.iter (fun (kwd, tok) -> Hashtbl.add keywords kwd tok) keylist
            
    let printing = Hashtbl.create 50
    let _ = List.iter (fun (kwd, tok) -> Hashtbl.add printing tok kwd) keylist

    let printtp t = Hashtbl.find printing t
      
      (*
      function
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
       *)    
  end

