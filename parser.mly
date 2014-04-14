%{
open Syntax

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
%token MERGE FLOW AT IS WHERE VERT ARR
%token OUTEND INSTART
%token PLUS MINUS TIMES
%token IF FOR
%token EQ TRUE FALSE

(* Tokens defined for matlab nodes *)
%token STMT EXPRSTMT ASSIGNSTMT GLOBALSTMT PERSISTENTSTMT SHELLCOMMANDSTMT 
%token BREAKSTMT CONTINUESTMT RETURNSTMT

%left PLUS MINUS
%left TIMES

%%

id:
| i = ID {Id i}

(* Right now it is quite abstract but can use matlab syntax for representation of nodes *)

varnode:
| i = id {MMVar (MVar i)}
| m = mtnode {m}

mtnode: 
| STMT {Stmt}
| EXPRSTMT v = varnode {ExprStmt v}
| ASSIGNSTMT l = varnode r = varnode {AssignStmt (l, r)}
| GLOBALSTMT i = id {GlobalStmt (MVar i)} 
| PERSISTENTSTMT i = id {PersistentStmt (MVar i)} 
| SHELLCOMMANDSTMT i = id {ShellCommandStmt i}
| BREAKSTMT {BreakStmt}
| CONTINUESTMT {ContinueStmt}
| RETURNSTMT {ReturnStmt}


direction:
| FORWARD {Forward}
| BACKWARD {Backward}

domain:
| SET d = domain {Set d}
| LBRACKET d1 = domain; d2 = domain RBRACKET {Tuple (d1, d2)}
| i = id {Name i}

op:
| x = op PLUS y = op {Plus (x, y)}
| x = op MINUS y = op {Minus (x, y)}
| x = op TIMES y = op {Times (x, y)}
| x = id {Var x}

expr:
| EMPTYSET {EmptySet}
| o = op {Op o}
| LCURLY e = expr RCURLY {Set e}
| LBRACKET e1 = expr COMMA e2 = expr RBRACKET {Tuple (e1, e2)}

cond:
| TRUE {True}
| FALSE {False}
| e1 = expr EQ e2 = expr {Eq (e1, e2)}

stmt:
| IF LBRACKET c = cond RBRACKET s = stmt {If (c, [s])}
| IF LBRACKET c = cond RBRACKET LCURLY s = stmt* RCURLY {If (c, s)}
| FOR LBRACKET i = id COLON d = domain RBRACKET s = stmt {For (i, d, [s])}
| FOR LBRACKET i = id COLON d = domain RBRACKET LCURLY s = stmt* RCURLY {For (i, d, s)}
| i = id EQ e = expr SCOLON {Assign (i, e)}

merge:
| MERGE n = id; m = id EQ e = expr {Merge (n, m, e)} 

node:
| VERT m = mtnode ARR s = stmt* {(m, s)}

flow:
| FLOW AT i = id IS s = stmt WHERE n = node* {Flow (i, s, n)}
    
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
