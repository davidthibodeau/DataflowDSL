open Syntax
module M = MatlabAst

type ctx = (id * domain) list

type error = 
| RepeatedPatternException of id
| MismatchedTypeAssignment of stmt * domain * domain
| MismatchedTypeExpression of expr * expr
| MismatchedTypeConditional of domain * domain
| UndefinedVariable of ctx * id

let errctx : ctx ref = ref []
let errte : (expr * expr) option ref = ref None
let errta : (stmt * domain * domain) option ref = ref None

let printError = function
  | RepeatedPatternException i -> "Variable " ^ (genId i) ^ " appears twice in same pattern."
  | MismatchedTypeAssignment (s, d1, d2) -> 
    let _ = errta := Some (s, d1, d2) in
    "Some mismatched type assignment"
  | MismatchedTypeConditional _ -> "Some mismatched type conditional"
  | MismatchedTypeExpression (te1, te2) -> 
    let _ = errte := Some (te1, te2) in
    "Some mismatched type expression"
  | UndefinedVariable (c, i) -> 
    let _ = errctx := c in
    "Variable " ^ (genId i) ^ " appears freely."

exception Error of error

let rec decidetp = function 
  | Stmt -> M.Stmt
  | ExprStmt _ -> M.ExprStmt
  | AssignStmt _ -> M.AssignStmt
  | GlobalStmt _ -> M.GlobalStmt 
  | PersistentStmt _ -> M.PersistentStmt 
  | ShellCommandStmt _ -> M.ShellCommandStmt
  | BreakStmt -> M.BreakStmt 
  | ContinueStmt -> M.ContinueStmt 
  | ReturnStmt -> M.ReturnStmt 
  | ForStmt _ -> M.ForStmt 
  | WhileStmt _ ->  M.WhileStmt
  | TryStmt _ -> M.TryStmt
  | SwitchStmt _ -> M.SwitchStmt
  | SwitchCaseBlock _ -> M.SwitchCaseBlock
  | DefaultCaseBlock _ -> M.DefaultCaseBlock
  | IfStmt _ -> M.IfStmt 
  | IfBlock _ -> M.IfBlock 
  | ElseBlock _ -> M.ElseBlock
  | Expr -> M.Expr 
  | RangeExpr _ -> M.RangeExpr
  | ColonExpr -> M.ColonExpr
  | EndExpr -> M.EndExpr
  | LValueExpr -> M.LValueExpr 
  | NameExpr _ -> M.NameExpr 
  | ParameterizedExpr _ -> M.ParameterizedExpr 
  | CellIndexExpr _ -> M.CellIndexExpr 
  | DotExpr _ -> M.DotExpr 
  | MatrixExpr _ -> M.MatrixExpr
  | CellArrayExpr _ -> M.CellArrayExpr 
  | SuperClassMethodExpr _ -> M.SuperClassMethodExpr 
  | Row _ -> M.Row 
  | LiteralExpr -> M.LiteralExpr
  | IntLiteralExpr -> M.IntLiteralExpr
  | FPLiteralExpr -> M.FPLiteralExpr 
  | StringLiteralExpr _ -> M.StringLiteralExpr
  | UnaryExpr _ -> M.UnaryExpr 
  | UMinusExpr _ -> M.UMinusExpr
  | UPlusExpr _ -> M.UPlusExpr 
  | NotExpr _ -> M.NotExpr
  | MTransposeExpr _ -> M.MTransposeExpr
  | ArrayTransposeExpr _ -> M.ArrayTransposeExpr
  | BinaryExpr _ -> M.BinaryExpr 
  | PlusExpr _ -> M.PlusExpr 
  | MinusExpr _ -> M.MinusExpr
  | MTimesExpr _ -> M.MTimesExpr
  | MDivExpr _ -> M.MDivExpr 
  | MLDivExpr _ -> M.MLDivExpr
  | MPowExpr _ -> M.MPowExpr 
  | ETimesExpr _ -> M.ETimesExpr
  | EDivExpr _ -> M.EDivExpr 
  | ELDivExpr _ -> M.ELDivExpr
  | EPowExpr _ -> M.EPowExpr 
  | AndExpr _ -> M.AndExpr 
  | OrExpr _ -> M.OrExpr 
  | ShortCircuitAndExpr _ -> M.ShortCircuitAndExpr
  | ShortCircuitOrExpr _ -> M.ShortCircuitOrExpr 
  | LTExpr _ -> M.LTExpr
  | GTExpr _ -> M.GTExpr
  | LEExpr _ -> M.LEExpr
  | GEExpr _ -> M.GEExpr
  | EQExpr _ -> M.EQExpr
  | NEExpr _ -> M.NEExpr
  | FunctionHandleExpr _ -> M.FunctionHandleExpr
  | LambdaExpr _ -> M.LambdaExpr

(* each analysis will reset it for its own needs *)  
let analysistype : domain option ref = ref None
let gettype () = match !analysistype with
  | Some a -> a
  | None -> assert false

let rec getCtxTp ctx i = match ctx with
  | [] -> None
  | (i', d) :: ctx' -> if i = i' then Some d else getCtxTp ctx' i

let rec typeExpr ctx e = match e with
  | Typ _ -> e
  | NoTyp e' -> 
    match e' with
    | Set e1 -> 
      let Typ (e1', d) as e'' = typeExpr ctx e1 in
      Typ (Set e'', Set d)
    | Tuple (e1, e2) ->
      let Typ (e1', d1) as e1'' = typeExpr ctx e1 in
      let Typ (e2', d2) as e2'' = typeExpr ctx e2 in
      Typ (Tuple (e1'', e2''), Tuple (d1, d2)) 
    | Plus (e1, e2) ->
      let Typ (e1', d1) as e1'' = typeExpr ctx e1 in
      let Typ (e2', d2) as e2'' = typeExpr ctx e2 in
      if d1 = d2 then
        Typ (Plus (e1'', e2''), d1)
      else
        raise (Error (MismatchedTypeExpression (e1'', e2'')))
    | Minus (e1, e2) ->
      let Typ (e1', d1) as e1'' = typeExpr ctx e1 in
      let Typ (e2', d2) as e2'' = typeExpr ctx e2 in
      if d1 = d2 then
        Typ (Minus (e1'', e2''), d1)
      else
        raise (Error (MismatchedTypeExpression (e1'', e2'')))
    | Times (e1, e2) ->
      let Typ (e1', d1) as e1'' = typeExpr ctx e1 in
      let Typ (e2', d2) as e2'' = typeExpr ctx e2 in
      if d1 = d2 then
        Typ (Times (e1'', e2''), d1)
      else
        raise (Error (MismatchedTypeExpression (e1'', e2'')))
    | Var i -> match getCtxTp ctx i with
      | None -> raise (Error (UndefinedVariable (ctx, i)))
      | Some d -> Typ (Var i, d)

let typeCond ctx = function
  | True -> True
  | False -> False
  | Eq (e1, e2) ->
    let Typ (e1', d1) as e1'' = typeExpr ctx e1 in
    let Typ (e2', d2) as e2'' = typeExpr ctx e2 in
    if d1 = d2 then
      Eq (e1'', e2'')
    else
      raise (Error (MismatchedTypeConditional (d1, d2)))

let typeInit ctx = function
  | NoInit -> NoInit

let typeMerge ctx (Merge (i1, i2, e)) =
  if i1 = i2 then 
    raise (Error (RepeatedPatternException i1)) 
  else
    let ctx' = (i1, gettype ()) :: (i2, gettype ()) :: ctx in
    let e' = typeExpr ctx' e in
    Merge (i1, i2, e')

let rec gatherPat = function
  | Stmt -> []
  | ExprStmt m -> gatherPat' (Matlab M.Expr) m
  | AssignStmt (m1, m2) -> 
    List.append (gatherPat' (Matlab M.Expr) m1) (gatherPat' (Matlab M.Expr) m2)
  | GlobalStmt _ -> [] (* TODO *)
  | PersistentStmt _ -> [] (* TODO *)
  | ShellCommandStmt _ -> [] (* TODO *)
  | BreakStmt -> []
  | ContinueStmt -> []
  | ReturnStmt -> []
  | ForStmt (m, l) -> (l, Set (Matlab M.Stmt)) :: (gatherPat' (Matlab M.AssignStmt) m)
  | WhileStmt (m, l) -> (l, Set (Matlab M.Stmt)) :: (gatherPat' (Matlab M.Expr) m)
  | TryStmt (l1, l2) -> (l1, Set (Matlab M.Stmt)) :: [(l2, Set (Matlab M.Stmt))]
  | SwitchStmt _ -> [] (* TODO *)
  | SwitchCaseBlock _ -> [] (* TODO *)
  | DefaultCaseBlock _ -> [] (* TODO *)
  | IfStmt _ -> [] (* TODO *)
  | IfBlock (m, l) ->  (l, Set (Matlab M.Stmt)) :: (gatherPat' (Matlab M.Expr) m)
  | ElseBlock l -> [(l, Set (Matlab M.Stmt))]
  | Expr -> []
  | RangeExpr _ -> [] (* TODO *)
  | ColonExpr -> []
  | EndExpr -> []
  | LValueExpr -> []
  | NameExpr m -> gatherPat' (Matlab M.Expr) m
  | ParameterizedExpr _ -> [] (* TODO *)
  | CellIndexExpr _ -> [] (* TODO *)
  | DotExpr _ -> [] (* TODO *)
  | MatrixExpr _ -> [] (* TODO *)
  | CellArrayExpr _ -> [] (* TODO *)
  | SuperClassMethodExpr _ -> [] (* TODO *)
  (* ... *)

and gatherPat' t : mpat -> (id * domain) list = function
  | Var i -> [(i, t)]
  | Node m -> gatherPat m
  | NodeAs (i, m) -> (i, t) :: (gatherPat m) 

let rec typeStmts ctx = function
  | [] -> []
  | s :: ss -> 
    let (s', ctx') = typeStmt ctx s in
    let ss' = typeStmts ctx' ss in
    s' :: ss'

and typeStmt ctx = function
  | If (c, sl) -> 
    let c' = typeCond ctx c in
    let sl' = typeStmts ctx sl in
    (If (c', sl'), ctx)
  | For (m, i, sl) ->
    (* TODO : Verify pattern is well formed *)
    (match getCtxTp ctx i with
    | None -> raise (Error (UndefinedVariable (ctx, i)))
    | Some (Set d) -> 
      let ctx' = List.append (gatherPat' d m) ctx in
      let sl' = typeStmts ctx' sl in
      (For (m, i, sl'), ctx))
  | Assign (i, e) as a ->
    let Typ (e1, d) as e' = typeExpr ctx e in
    match getCtxTp ctx i with
    | None -> (Assign (i, e'), (i, d) :: ctx)
    | Some t -> if t = d then
        (Assign (i, e'), ctx)
      else
        raise (Error (MismatchedTypeAssignment (a, t, d)))

let typeFlowBranch ctx node (pat, sl) =  
  let ctx1 = (node, Matlab (decidetp pat)) :: ctx in
  let ctx' = List.append (gatherPat pat) ctx1 in
  let sl' = typeStmts ctx' sl in
  (pat, sl')

let rec typeFExpr = function
  | Plus (e1, e2) -> List.append (typeFExpr e1) (typeFExpr e2)
  | Minus (e1, e2) -> List.append (typeFExpr e1) (typeFExpr e2)
  | Times (e1, e2) -> List.append (typeFExpr e1) (typeFExpr e2)
  | Var i -> [(i, gettype ())]

let typeFlow ctx (Flow (i, (i', fe), cl)) =
  let ctx' = (Id "in", gettype()) :: (Id "out", gettype()) :: ctx in
  let ctx'' = List.append (typeFExpr fe) ctx' in
  let cl' = List.map (typeFlowBranch ctx'' i) cl in
  Flow (i, (i', fe), cl')

(* Auxiliary functions not yet supported *)
let typeAux ctx a = a  

let typeBodies ctx (Body (i, m, f, a)) =
  let i' = typeInit ctx i in
  let m' = typeMerge ctx m in
  let f' = typeFlow ctx f in
  let a' = List.map (typeAux ctx) a in
  Body (i', m', f', a')
        
let typeAnalysis (Analysis (i, di, dom, b)) =
  let _ = analysistype := Some dom in
  Analysis (i, di, dom, typeBodies ((i, dom) :: []) b)

let typeProgram = function
  | Program al -> 
    begin 
      try
        Program (List.map typeAnalysis al)
      with
      | Error e -> 
        let s = printError e in
        let _ = print_endline s in
        Empty
    end
  | Empty -> Empty


