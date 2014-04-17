open Syntax
module M = MatlabAst

exception RepeatedPatternException
exception MismatchedTypeAssignment
exception MismatchedTypeConditional
exception AlreadyTypedExpression
exception UndefinedVariable

(* each analysis will reset it for its own needs *)  
let analysistype : domain option ref = ref None
let gettype () = match !analysistype with
  | Some a -> a
  | None -> assert false

let rec getCtxTp ctx i = match ctx with
  | [] -> None
  | (i', d) :: ctx' -> if i = i' then Some d else getCtxTp ctx' i

let rec typeExpr ctx e = match e with
  | Typ _ -> raise AlreadyTypedExpression (* Introduce type annotations ? *)
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
        raise MismatchedTypeAssignment
    | Minus (e1, e2) ->
      let Typ (e1', d1) as e1'' = typeExpr ctx e1 in
      let Typ (e2', d2) as e2'' = typeExpr ctx e2 in
      if d1 = d2 then
        Typ (Minus (e1'', e2''), d1)
      else
        raise MismatchedTypeAssignment
    | Times (e1, e2) ->
      let Typ (e1', d1) as e1'' = typeExpr ctx e1 in
      let Typ (e2', d2) as e2'' = typeExpr ctx e2 in
      if d1 = d2 then
        Typ (Times (e1'', e2''), d1)
      else
        raise MismatchedTypeAssignment
    | Var i -> match getCtxTp ctx i with
      | None -> raise UndefinedVariable
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
      raise MismatchedTypeConditional

let typeInit ctx = function
  | NoInit -> NoInit

let typeMerge ctx (Merge (i1, i2, e)) =
  if i1 = i2 then 
    raise RepeatedPatternException 
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
    | None -> raise UndefinedVariable
    | Some d -> 
      let ctx' = gatherPat' d  m in
      let sl' = typeStmts ctx' sl in
      (For (m, i, sl'), ctx))
  | Assign (i, e) ->
    let Typ (e1, d) as e' = typeExpr ctx e in
    match getCtxTp ctx i with
    | None -> (Assign (i, e'), (i, d) :: ctx)
    | Some t -> if t = d then
        (Assign (i, e'), ctx)
      else
        raise MismatchedTypeAssignment

let typeFlowBranch ctx (pat, sl) =  
  let ctx' = List.append (gatherPat pat) ctx in
  let sl' = typeStmts ctx' sl in
  (pat, sl')

(* TODO : Figure out accumulation of context and typing *)
let typeFlowStmt ctx' s = (ctx', s)

let typeFlow ctx (Flow (i, s, cl)) =
  let ctx' = (Id "in", gettype()) :: (Id "out", gettype()) :: ctx in
  let (ctx'', s') = typeFlowStmt ctx' s in
  let cl' = List.map (typeFlowBranch ctx'') cl in
  Flow (i, s', cl')

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
  | Program al -> Program (List.map typeAnalysis al)
  | Empty -> Empty


