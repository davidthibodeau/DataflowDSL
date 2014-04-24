open Syntax
open Matlab
module T = Types
module M = Ast

exception UntypedExpression

let output = ref ""

(* Constants within a given analysis *)
let analysisName = ref ""
let typeDomain : domain option ref = ref None
let stringDomain = ref ""
let nodeString = ref ""
let direction : direction option ref = ref None
let flowStmt = ref ""

(* NameGenerator for temp variables *)
let nextName = 
  let c = ref 0 in
  function () -> let _ = c := !c + 1 in "dsl_var" ^ (string_of_int !c)

(* Boilerplate code *)
let imports = 
"import java.util.*;

import ast.Program;
import natlab.*;
import analysis.*;
import ast.*;
import nodecases.*;
import natlab.toolkits.analysis.core.*;

import com.google.common.collect.Sets;\n"

let parseOrDie = 
"\tprivate static Program parseOrDie(String path) {
\t\tjava.util.List<CompilationProblem> errors = new ArrayList<CompilationProblem>();
\t\tProgram ast = Parse.parseMatlabFile(path, errors);
\t\tif (!errors.isEmpty()) {
\t\t\tSystem.err.println(\"Parse error: \" + CompilationProblem.toStringAll(errors));
\t\t}
\t\treturn ast;
\t}\n"

let defaultInitial () =
("\t@Override public " ^ !stringDomain ^ " newInitialFlow() {
\t\treturn " ^ 
  (match !typeDomain with
  | Some (Set _) -> "Sets.newHashSet()"
  | _ -> ""
  ) ^ ";
\t}\n")

let copy () =
"\t@Override public " ^ !stringDomain ^ " copy(" ^ !stringDomain ^ " src) {
\t\treturn " ^ 
  (match !typeDomain with
  | Some (Set _) -> "Sets.newHashSet(src)"
  | _ -> ""
  ) ^ ";
\t}\n"


let constructor () =
"\tpublic " ^ !analysisName ^ "(ASTNode tree) {
\t\tsuper(tree);
\t}\n"

let main () =
"\tpublic static void main(String[] args) {
\t\tProgram ast = parseOrDie(args[0]);
\t\t" ^ !analysisName ^ " analysis = new " ^ !analysisName ^ "(ast);
\t\tanalysis.analyze();
\t}\n"

(* \t\tprintWithAnalysisResults(ast, analysis); *)
(* Generation *)

let rec printlvl = function
  | 0 -> ""
  | n -> "\t" ^ (printlvl (n-1))

(* n : current level. m : base level *)
let rec closeinnerlvl n m =
  if n < m then "" else
  (printlvl n) ^ "}\n" ^ (closeinnerlvl (n-1) m)

let write s = output := (!output ^ s ^ "\n")
let writeline lvl s = output := (!output ^ (printlvl lvl) ^ s ^ "\n")
let writecase node case = output := 
  (!output ^ "\t@Override public void case" ^ case ^ "(" ^ case ^ " " ^ node ^ ") {\n")

let writeclosings lvl lvl' = output := (!output ^ (closeinnerlvl lvl lvl'))

let getTypeDomain () = match !typeDomain with
  | Some e -> e
  | None -> assert false

let rec genDomain : domain -> string = function
  | Set d -> "Set<" ^ (genDomain d) ^ ">"
  | Tuple (d1, d2) -> "Map<" ^ (genDomain d1) ^ ", " ^ (genDomain d2) ^ ">"
  | Name id -> id
  | Matlab m -> M.printtp m

let substInOut i = 
      if i = "in" then 
        "currentInSet"
      else if i = "out" then
        "currentOutSet"
      else
        i

let rec genTExpr = function
  | NoTyp e -> raise UntypedExpression
  | Typ (e, t) -> genFlowExpr e

and genFlowExpr e = match e with
  | Var id -> substInOut id
  | Plus (o1, o2) -> 
    let s1 = genTExpr o1 in
    let s2 = genTExpr o2 in
     "Sets.union(" ^ s1 ^ ", " ^ s2 ^ ")" 
  | Minus (o1, o2) -> 
    let s1 = genTExpr o1 in
    let s2 = genTExpr o2 in
    "Sets.difference(" ^ s1 ^ ", " ^ s2 ^ ")" 
  | Times (o1, o2) -> 
    let s1 = genTExpr o1 in
    let s2 = genTExpr o2 in
     "Sets.intersection(" ^ s1 ^ ", " ^ s2 ^ ")" 
  | EmptySet -> "Sets.newHashSet()"
  | Set e -> 
    let s = genTExpr e in
    "Sets.newHashSet(" ^ s ^ ")"

let rec expandInOut = function
  | Plus  (x, y) -> Plus  (expandInOut x, expandInOut y)
  | Minus (x, y) -> Minus (expandInOut x, expandInOut y)
  | Times (x, y) -> Times (expandInOut x, expandInOut y)
  | Var i -> Var (substInOut i)

let rec methodStmt access e tp = 
  let acc tp = "((" ^ (genDomain tp) ^ ") " ^ access ^ ")" in
  match e with
  | M.Var i -> [(genDomain tp) ^ " " ^ i ^ " = " ^ access ^ ";"], []
  | M.Node ((t, l) as m) -> 
      let decls, insts = methodBody (acc (Matlab t)) m in
      let insts' = (access ^ " instanceof " ^ (M.printtp t)) :: insts in
      decls, insts'
  | M.NodeAs (i, (t, l as m)) -> 
      let decls, insts = methodBody (acc (Matlab t)) m in
      let decls' = ((genDomain tp) ^ " " ^ i ^ " = " ^ (acc (Matlab t)) ^ ";") :: decls in
      let insts' = (access ^ " instanceof " ^ (M.printtp t)) :: insts in
      decls', insts'
      
and methodBody access e = 
  let acc s = access ^ s in
  match e with
  | M.Stmt, [] -> [], []
  | M.ExprStmt, [m] -> methodStmt (acc ".getExpr()") m (Matlab M.Expr)
  | M.AssignStmt, [m1; m2] ->  
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.GlobalStmt, [m] -> methodStmt (acc ".getNames()") m (Set (Matlab M.Name))
  | M.PersistentStmt, [m] -> methodStmt (acc ".getNames()") m (Set (Matlab M.Name))
(*  | M.ShellCommandStmt,  *)
  | M.BreakStmt, [] -> [], []
  | M.ContinueStmt, [] -> [], []
  | M.ReturnStmt, [] -> [], []
  | M.ForStmt, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getAssignStmt()") m1 (Matlab M.AssignStmt) in
      let decls2, insts2 = methodStmt (acc ".getStmts()") m2 (Set (Matlab M.Stmt)) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.WhileStmt, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getExpr()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getStmts()") m2 (Set (Matlab M.Stmt)) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.TryStmt, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getTryStmts()") m1 (Set (Matlab M.Stmt)) in
      let decls2, insts2 = methodStmt (acc ".getCatchStmts()") m2 (Set (Matlab M.Stmt)) in
      List.append decls1 decls2, List.append insts1 insts2
(*  | M.SwitchStmt,  *)
  | M.SwitchCaseBlock, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getExpr()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getStmts()") m2 (Set (Matlab M.Stmt)) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.DefaultCaseBlock, [m] -> methodStmt (acc ".getStmts()") m (Set (Matlab M.Stmt))
(*  | M.IfStmt, *)
  | M.IfBlock, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getCondition()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getStmts()") m2 (Set (Matlab M.Stmt)) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.ElseBlock, [m] -> methodStmt (acc ".getStmts()") m (Set (Matlab M.Stmt))
  | M.Expr, [] -> [], []
(*  | M.RangeExpr *)
  | M.ColonExpr, [] -> [], []
  | M.EndExpr, [] -> [], []
  | M.LValueExpr, [] -> [], []
  | M.NameExpr, [m] -> methodStmt (acc ".getName()") m (Matlab M.Name)
  | M.ParameterizedExpr, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getTarget()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getArgs()") m2 (Set (Matlab M.Expr)) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.CellIndexExpr, [m1; m2] ->
      let decls1, insts1 = methodStmt (acc ".getTarget()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getArgs()") m2 (Set (Matlab M.Expr)) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.DotExpr, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getTarget()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getField()") m2 (Matlab M.Name) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.MatrixExpr, [m] -> methodStmt (acc ".getRows()") m (Set (Matlab M.Row))
  | M.CellArrayExpr, [m] -> methodStmt (acc ".getRows()") m (Set (Matlab M.Row))
  | M.SuperClassMethodExpr, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getFuncName()") m1 (Matlab M.Name) in
      let decls2, insts2 = methodStmt (acc ".getClassName()") m2 (Matlab M.Name) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.Row, [m] -> methodStmt (acc ".getElements()") m (Set (Matlab M.Expr))
  | M.LiteralExpr, [] -> [], []
 (* | M.IntLiteralExpr,  *)
 (* | FPLiteralExpr, *)
 (* | StringLiteralExpr, *)
  | M.UnaryExpr, [m] -> methodStmt (acc ".getOperand()") m (Matlab M.Expr)
  | M.UMinusExpr, [m] -> methodStmt (acc ".getOperand()") m (Matlab M.Expr)
  | M.UPlusExpr, [m] -> methodStmt (acc ".getOperand()") m (Matlab M.Expr) 
  | M.NotExpr, [m] -> methodStmt (acc ".getOperand()") m (Matlab M.Expr)
  | M.MTransposeExpr, [m] -> methodStmt (acc ".getOperand()") m (Matlab M.Expr)
  | M.ArrayTransposeExpr, [m] -> methodStmt (acc ".getOperand()") m (Matlab M.Expr)
  | M.BinaryExpr, [m1; m2] ->
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.PlusExpr, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.MinusExpr, [m1; m2] ->
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.MTimesExpr, [m1; m2] ->
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.MDivExpr, [m1; m2] ->
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2 
  | M.MLDivExpr, [m1; m2] ->
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.MPowExpr, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.ETimesExpr, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.EDivExpr, [m1; m2] ->
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.ELDivExpr, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.EPowExpr, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.AndExpr, [m1; m2] ->
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.OrExpr,  [m1; m2] ->
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.ShortCircuitAndExpr, [m1; m2] ->
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.ShortCircuitOrExpr, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.LTExpr, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.GTExpr, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.LEExpr, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.GEExpr, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.EQExpr, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.NEExpr, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getLHS()") m1 (Matlab M.Expr) in
      let decls2, insts2 = methodStmt (acc ".getRHS()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | M.FunctionHandleExpr, [m] -> methodStmt (acc ".getName()") m (Matlab M.Name)
  | M.LambdaExpr, [m1; m2] -> 
      let decls1, insts1 = methodStmt (acc ".getInputParams()") m1 (Set (Matlab M.Name)) in
      let decls2, insts2 = methodStmt (acc ".getBody()") m2 (Matlab M.Expr) in
      List.append decls1 decls2, List.append insts1 insts2
  | _, _ -> assert false 

let genCond = function
  | True -> "true"
  | False -> "false"
  | Eq (e1, e2) -> "(" ^ (genTExpr e1) ^ ").equals(" ^ (genTExpr e2) ^ ")"

let genInsts l = List.fold_right (fun x y -> x ^ " && " ^ y) l "true"

let rec genPat b lvl (decls, ifs, stmts) = 
  let s = match b with true -> "if (" | false -> "else if (" in
  let _ = writeline lvl (s ^ (genInsts ifs) ^ ") {") in 
  let _ = List.iter (writeline (lvl + 1)) decls in
  let _ = List.iter (genStmt (lvl + 1)) stmts in
  writeclosings lvl lvl

and genStmt lvl = function
  | If (c, sl) -> 
      let _ = writeline lvl ("if (" ^ (genCond c) ^ ") {") in
      let _ = List.iter (genStmt (lvl + 1)) sl in
      writeclosings lvl lvl
  | For (m, i, Some d, sl) ->
      let temp, node = match m with
      | M.Var i -> i, None
      | M.Node m -> nextName (), Some m
      | M.NodeAs (i, m) -> i, Some m
      in
      let f = "for (" ^ (genDomain d) ^ " " ^ temp ^ " : " ^ (substInOut i) ^ ") {" in
      let _ = writeline lvl f in
      let _ = match node with
      | None -> List.iter (genStmt (lvl + 1)) sl
      | Some m -> 
          let (decls, insts) = methodBody temp m in
          genPat true (lvl + 1) (decls, insts, sl)
      in
      writeclosings lvl lvl
  | Assign (i, e) -> 
      writeline lvl ((substInOut i) ^ " = " ^ (genTExpr e) ^ ";")
  | _ -> assert false

let rec genFExpr = function
  | Var id -> 
      if id = "currentInSet" || id = "currentOutSet" then
        [], id
      else 
        [id], id
  | Plus (o1, o2) -> 
    let dv1, s1 = genFExpr o1 in
    let dv2, s2 = genFExpr o2 in
     List.append dv1 dv2, "Sets.union(" ^ s1 ^ ", " ^ s2 ^ ")" 
  | Minus (o1, o2) -> 
    let dv1, s1 = genFExpr o1 in
    let dv2, s2 = genFExpr o2 in
    List.append dv1 dv2, "Sets.difference(" ^ s1 ^ ", " ^ s2 ^ ")" 
  | Times (o1, o2) -> 
    let dv1, s1 = genFExpr o1 in
    let dv2, s2 = genFExpr o2 in
     List.append dv1 dv2, "Sets.intersection(" ^ s1 ^ ", " ^ s2 ^ ")" 

let genBranch nodeName initend dvars (a, ll) =
  let (init, cp, ending) = initend in
  let _ = writecase nodeName (M.printtp a) in
  let _ = writeline 2 init in
  let f = fun x -> writeline 2 (!stringDomain ^ " " ^ x ^ " = Sets.newHashSet();") in
  let _ = List.iter f dvars in
  let f' = fun (vl, sl) -> let (decls, ifs) = methodBody nodeName (a, vl) in (decls, ifs, sl) in
  let _ = match List.map f' ll with 
  | [] -> assert false
  | [ll'] -> genPat true 2 ll'
  | ll' :: lls -> let _ = genPat true 2 ll' in List.iter (genPat false 2) lls
  in   
  let _ = writeline 2 cp in
  let _ = writeline 2 ending in
  let _ = writeclosings 1 1 in
  let _ = writeline 1 "\n" in
  ()
    
let genFlow = function
  | CheckedFlow (i, (i', fe), bs) ->
    let nodeName = i in
    let fe' = expandInOut fe in
    let (dvars, cp) = genFExpr fe' in
    let initend = match !direction with
      | Some Forward ->
        if i' = "out" then
          ("inFlowSets.put(node, copy(currentInSet));",
           "currentOutSet = " ^ cp,
           "outFlowSets.put(node, copy(currentOutSet));")
        else
          assert false
      | Some Backward -> 
        if i' = "in" then
          ("outFlowSets.put(node, copy(currentOutSet));",
           "currentInSet = " ^ cp,
           "inFlowSets.put(node, copy(currentInSet));")
        else
          assert false
    in
    List.iter (genBranch nodeName initend dvars) bs
  | _ -> assert false                           

let genDirection d = 
  let _ = direction := (Some d) in
  match d with
  | Forward -> "ForwardAnalysis"
  | Backward -> "BackwardAnalysis"


let genMerge (Merge (i1, i2, e)) =
  let s = ("@Override public " ^ !stringDomain ^ " merge(" ^ !stringDomain ^ " " 
           ^ i1 ^ ", " ^ !stringDomain ^ " " ^ i2 ^ ") {") 
  in
  let _ = writeline 1 s in
  let s = genTExpr e in
  let _ = writeline 2 ("return Sets.newHashSet(" ^ s ^ ");") in
  writeline 1 "}\n"

let genBodies = function
  | Body (i, m, f, a) -> 
    let _ = genMerge m in
    let _ = genFlow f in ()

(* Each analysis is in its own file which has the name of the analysis *)
let genAnalysis = function
  | Analysis (name, direction, domain, bodies) ->
    let _ = output := "" in
    let _ = analysisName := name in
    let _ = stringDomain := (genDomain domain) in
    let _ = typeDomain := (Some domain) in
    let _ = write imports in
    let _ = write ("public class " ^ !analysisName ^ " extends " 
                   ^ (genDirection direction) ^ "<" ^ !stringDomain ^ "> {") 
    in
    let _ = write parseOrDie in
    let _ = write (main ()) in
    let _ = write (constructor ()) in
    let _ = write (defaultInitial ()) in
    let _ = write (copy ()) in
    let _ = genBodies bodies in
    (* Ajouter le reste du body *)
    let _ = write "}" in
    print_endline !output          

let codegen = function 
  | Program a -> 
    List.iter genAnalysis a
  | Empty -> ()
