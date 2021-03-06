%{
open Syntax
open Matlab

module M = Ast

exception Error
exception MisformedBodies
%}

%start<Syntax.program> program

%token <string>ID
%token FORWARD BACKWARD
%token SET EMPTYSET
%token LBRACKET RBRACKET LCURLY RCURLY
%token COLON SCOLON EOF COMMA
%token ANALYSIS OF
%token MERGE FLOW AT IS WHERE VERT ARR AS
%token OUTEND INSTART
%token PLUS MINUS TIMES
%token IF FOR EEQ PEQ MEQ TEQ
%token EQ TRUE FALSE

%token <Matlab.Ast.ast>MATLAB

%left PLUS MINUS
%left TIMES

%%

matlabast:
| m = MATLAB {m}

(*| NAME {M.Name}
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
| PARAMETERIZEDEXPR {M.ParameterizedExpr}
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
| UNARYEXPR {M.UnaryExpr}
| UMINUSEXPR {M.UMinusExpr}
| UPLUSEXPR {M.UPlusExpr}
| NOTEXPR {M.NotExpr}
| MTRANSPOSEEXPR {M.MTransposeExpr}
| ARRAYTRANSPOSEEXPR {M.ArrayTransposeExpr}
| BINARYEXPR {M.BinaryExpr}
| PLUSEXPR {M.PlusExpr}
| MINUSEXPR {M.MinusExpr}
| MTIMESEXPR {M.MTimesExpr}
| MDIVEXPR {M.MDivExpr}
| MLDIVEXPR {M.MLDivExpr}
| MPOWEXPR {M.MPowExpr}
| ETIMESEXPR {M.ETimesExpr}
| EDIVEXPR {M.EDivExpr}
| ELDIVEXPR {M.ELDivExpr}
| EPOWEXPR {M.EPowExpr}
| ANDEXPR {M.AndExpr}
| OREXPR {M.OrExpr}
| SHORTCIRCUITANDEXPR {M.ShortCircuitAndExpr}
| SHORTCIRCUITOREXPR {M.ShortCircuitOrExpr}
| LTEXPR {M.LTExpr}
| GTEXPR {M.GTExpr}
| LEEXPR{M.LEExpr}
| GEEXPR {M.GEExpr}
| EQEXPR {M.EQExpr}
| NEEXPR {M.NEExpr}
| FUNCTIONHANDLEEXPR {M.FunctionHandleExpr}
| LAMBDAEXPR {M.LambdaExpr}
*)
id:
| i = ID {i}

spat:
| i = id {M.Var i}
| m = pat {M.Node m}
| i = id AS m = pat {M.NodeAs (i, m)}

vpat:
| i = id {M.Var i}
| LBRACKET m = pat RBRACKET {M.Node m}
| LBRACKET i = id AS m = pat RBRACKET {M.NodeAs (i, m)}

pat:
| m = matlabast v = vpat* {(m, v)}

direction:
| FORWARD {Forward}
| BACKWARD {Backward}

domain:
| SET d = domain {Set d}
| LBRACKET d1 = domain; d2 = domain RBRACKET {Tuple (d1, d2)}
| m = matlabast {Matlab m}
| i = id {Name i}

ntexpr:
| e = expr {NoTyp e}
| LBRACKET e = expr COLON t = domain RBRACKET {Typ (e, t)}

expr:
| EMPTYSET {EmptySet}
| LCURLY e = ntexpr RCURLY {Set e}
| LBRACKET e1 = ntexpr COMMA e2 = ntexpr RBRACKET {Tuple (e1, e2)}
| LBRACKET e = expr RBRACKET {e}
| x = ntexpr PLUS  y = ntexpr {Plus  (x, y)}
| x = ntexpr MINUS y = ntexpr {Minus (x, y)}
| x = ntexpr TIMES y = ntexpr {Times (x, y)}
| x = id {Var x}

cond:
| TRUE {True}
| FALSE {False}
| e1 = ntexpr EEQ e2 = ntexpr {Eq (e1, e2)}

stmt:
| IF LBRACKET c = cond RBRACKET s = stmt {If (c, [s])}
| IF LBRACKET c = cond RBRACKET LCURLY s = stmt* RCURLY {If (c, s)}
| FOR LBRACKET m = spat COLON i = id RBRACKET s = stmt {For (m, i, None, [s])}
| FOR LBRACKET m = spat COLON i = id RBRACKET LCURLY s = stmt* RCURLY {For (m, i, None, s)}
| i = id EQ e = ntexpr SCOLON {Assign (i, e)}
(* Syntactic sugar *)
| i = id PEQ e = ntexpr SCOLON {Assign (i, NoTyp (Plus (NoTyp (Var i), e)))}
| i = id TEQ e = ntexpr SCOLON {Assign (i, NoTyp (Times (NoTyp (Var i), e)))}
| i = id MEQ e = ntexpr SCOLON {Assign (i, NoTyp (Minus (NoTyp (Var i), e)))}

merge:
| MERGE n = id; m = id EQ e = ntexpr SCOLON {Merge (n, m, e)} 

node:
| VERT m = pat ARR s = stmt* {(m, s)}

fexpr:
| x = fexpr PLUS  y = fexpr {Plus  (x, y)}
| x = fexpr MINUS y = fexpr {Minus (x, y)}
| x = fexpr TIMES y = fexpr {Times (x, y)}
| LBRACKET e = fexpr RBRACKET {e}
| x = id {Var x}

flow:
| FLOW AT i = id IS i1 = id EQ fe = fexpr WHERE n = node* SCOLON 
    {Flow (i, (i1, fe), n)}
    
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
        | _ -> raise MisformedBodies
      in
      Analysis (i, di, d, makeBodies)
    }

program:
| a = analysis+ EOF {Program a}
