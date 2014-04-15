%{
open Syntax
module M = MatlabAst 

exception Error
exception MisformedBodies
%}

%start<Syntax.program> program

%token <string>ID
%token FORWARD BACKWARD
%token SET EMPTYSET
%token LBRACKET RBRACKET LCURLY RCURLY
%token COLON SCOLON EOF COMMA
%token ANALYSIS ENUM OF
%token MERGE FLOW AT IS WHERE VERT ARR AS
%token OUTEND INSTART
%token PLUS MINUS TIMES
%token IF FOR EEQ PEQ MEQ LEQ GEQ TEQ
%token EQ TRUE FALSE

(* Tokens defined for matlab nodes *)
%token STMT EXPRSTMT ASSIGNSTMT GLOBALSTMT PERSISTENTSTMT SHELLCOMMANDSTMT 
%token BREAKSTMT CONTINUESTMT RETURNSTMT
%token FORSTMT WHILESTMT TRYSTMT SWITCHSTMT SWITCHCASEBLOCK IFSTMT IFBLOCK ELSEBLOCK
%token EXPR RANGEEXPR COLONEXPR ENDEXPR LVALUEEXPR NAMEEXPR PARAMETRIZEDEXPR 
%token CELLINDEXEXPR DOTEXPR MATRIXEXPR CELLARRAYEXPR SUPERCLASSMETHODEXPR ROW
%token LITERALEXPR INTLITERALEXPR FPLITERALEXPR STRINGLITERALEXPR

%left PLUS MINUS
%left TIMES


%%

matlabast:
| STMT {M.Stmt}
| EXPRSTMT {M.ExprStmt}
| ASSIGNSTMT{M.AssignStmt}
| GLOBALSTMT {M.GlobalStmt}
| PERSISTENTSTMT {M.PersistentStmt}
| SHELLCOMMANDSTMT {M.ShellCommandStmt}
| BREAKSTMT {M.BreakStmt}
| CONTINUESTMT {M.ContinueStmt}
| RETURNSTMT {M.ReturnStmt}
| FORSTMT {M.ForStmt}
| WHILESTMT {M.WhileStmt}
| TRYSTMT {M.TryStmt}
| SWITCHSTMT {M.SwitchStmt}
| SWITCHCASEBLOCK {M.SwitchCaseBlock}
| IFSTMT {M.IfStmt}
| IFBLOCK {M.IfBlock}
| ELSEBLOCK {M.ElseBlock}
| EXPR {M.Expr}
| RANGEEXPR {M.RangeExpr}
| COLONEXPR {M.ColonExpr}
| ENDEXPR {M.EndExpr}
| LVALUEEXPR {M.LValueExpr}
| NAMEEXPR {M.NameExpr}
| PARAMETRIZEDEXPR {M.ParametrizedExpr}
| CELLINDEXEXPR {M.CellIndexExpr}
| DOTEXPR {M.DotExpr}
| MATRIXEXPR {M.MatrixExpr}
| CELLARRAYEXPR {M.CellArrayExpr}
| SUPERCLASSMETHODEXPR {M.SuperClassMethodExpr}
| ROW {M.Row}
| LITERALEXPR {M.LiteralExpr}
| INTLITERALEXPR {M.IntLiteralExpr}
| FPLITERALEXPR {M.FPLiteralExpr}
| STRINGLITERALEXPR {M.StringLiteralExpr}
(* TODO Finish that
| {M.UnaryExpr}
| {M.UMinusExpr}
| {M.UPlusExpr}
| {M.NotExpr}
| {M.MTransposeExpr}
| {M.ArrayTransposeExpr}
| {M.BinaryExpr}
| {M.PlusExpr}
| {M.MinusExpr}
| {M.MTimesExpr}
| {M.MDivExpr}
| {M.MLDiveExpr}
| {M.MPowExpr}
| {M.ETimesExpr}
| {M.EDivExpr}
| {M.ELDivExpr}
| {M.EPowExpr}
| {M.AndExpr}
| {M.OrExpr}
| {M.ShortCircuitAndExpr}
| {M.ShortCircuitOrExpr}
| {M.LTExpr}
| {M.GTExpr}
| {M.LEExpr}
| {M.GEExpr}
| {M.EQExpr}
| {M.NEExpr}
| {M.FunctionHandleExpr}
| {M.LambdaExpr}
*)

id:
| i = ID {Id i}

(* Right now it is quite abstract but can use matlab syntax for representation of nodes *)

varnode:
| i = id {Var i}
| m = mtnode {Node m}
| LBRACKET m = mtnode RBRACKET {Node m}
| i = id AS m = mtnode {NodeAs (i, m)}
| LBRACKET i = id AS m = mtnode RBRACKET {NodeAs (i, m)}

mtnode: 
| STMT {Stmt}
| EXPRSTMT v = varnode {ExprStmt v}
| ASSIGNSTMT l = varnode r = varnode {AssignStmt (l, r)}
| GLOBALSTMT i = id {GlobalStmt (i)} 
| PERSISTENTSTMT i = id {PersistentStmt (i)} 
| SHELLCOMMANDSTMT i = id {ShellCommandStmt i}
| BREAKSTMT {BreakStmt}
| CONTINUESTMT {ContinueStmt}
| RETURNSTMT {ReturnStmt}

| NAMEEXPR i = id {NameExpr i}

direction:
| FORWARD {Forward}
| BACKWARD {Backward}

domain:
| SET d = domain {Set d}
| LBRACKET d1 = domain; d2 = domain RBRACKET {Tuple (d1, d2)}
| m = matlabast {Matlab m}
| i = id {Name i}

op:
| x = expr PLUS y = expr {Plus (x, y)}
| x = expr MINUS y = expr {Minus (x, y)}
| x = expr TIMES y = expr {Times (x, y)}
| x = id {Var x}

expr:
| EMPTYSET {EmptySet}
| o = op {Op o}
| LCURLY e = expr RCURLY {Set e}
| LBRACKET e1 = expr COMMA e2 = expr RBRACKET {Tuple (e1, e2)}
| LBRACKET e = expr RBRACKET {e}

cond:
| TRUE {True}
| FALSE {False}
| e1 = expr EEQ e2 = expr {Eq (e1, e2)}

stmt:
| IF LBRACKET c = cond RBRACKET s = stmt {If (c, [s])}
| IF LBRACKET c = cond RBRACKET LCURLY s = stmt* RCURLY {If (c, s)}
| FOR LBRACKET m = varnode COLON d = domain RBRACKET s = stmt {For (m, d, [s])}
| FOR LBRACKET m = varnode COLON d = domain RBRACKET LCURLY s = stmt* RCURLY {For (m, d, s)}
| i = id EQ e = expr SCOLON {Assign (i, e)}
| i = id PEQ e = expr SCOLON {Assign (i, Op (Plus (Op (Var i), e)))}
| i = id TEQ e = expr SCOLON {Assign (i, Op (Times (Op (Var i), e)))}
| i = id MEQ e = expr SCOLON {Assign (i, Op (Minus (Op (Var i), e)))}

merge:
| MERGE n = id; m = id EQ e = expr SCOLON {Merge (n, m, e)} 

node:
| VERT m = mtnode ARR s = stmt* {(m, s)}

flowstmt:
| i = id EQ e = expr {Assign (i, e)}

flow:
| FLOW AT i = id IS s = flowstmt WHERE n = node* SCOLON {Flow (i, s, n)}
    
body:
| m = merge {M m}
| f = flow {F f}

analysis:
| ANALYSIS i = id COLON di = direction OF d = domain LCURLY b = body* RCURLY
    { 
      let makeBodies = 
        let rec collectBodies i m f a = function
          | [] -> (i, m, f, a)
          | I i' :: bs -> collectBodies (I i' :: i) m f a bs
          | M m' :: bs -> collectBodies i (M m' :: m) f a bs
          | F f' :: bs -> collectBodies i m (F f' :: f) a bs
          | A a' :: bs -> collectBodies i m f (a' :: a) bs
        in
        match collectBodies [] [] [] [] b with
        | ([I i], [M m], [F f], a) -> Body (i, m, f, a)
      (* I think we can omit initial condition since it is often the empty set anyway *)
        | ([], [M m], [F f], a) -> Body (NoInit, m, f, a) 
      (* This one should be removed but is used to test merge *)
        | ([], [M m], [], a) -> Body (NoInit, m, NoFlow, a)
        | _ -> raise MisformedBodies
      in
      Analysis (i, di, d, makeBodies)
    }

program:
| a = analysis+ EOF {Program a}
