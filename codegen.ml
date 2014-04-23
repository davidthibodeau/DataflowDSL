open Syntax
module T = Types
module M = MatlabAst

exception UntypedExpression

let output = ref ""

(* Constants within a given analysis *)
let analysisName = ref ""
let typeDomain : domain option ref = ref None
let stringDomain = ref ""
let nodeString = ref ""
let direction : direction option ref = ref None
let flowStmt = ref ""

(* Boilerplate code *)
let imports = 
"import java.util.*;

import ast.Program;
import natlab.*;
import analysis.*;
import ast.*;
import nodecases.*;
import natlab.toolkits.analysis.core.*;

import com.google.common.collect.Sets;\n\n"

let parseOrDie = 
"\tprivate static Program parseOrDie(String path) {
\t\tjava.util.List<CompilationProblem> errors = new ArrayList<CompilationProblem>();
\t\tProgram ast = Parse.parseMatlabFile(path, errors);
\t\tif (!errors.isEmpty()) {
\t\t\tSystem.err.println(\"Parse error: \" + CompilationProblem.toStringAll(errors));
\t\t}
\t\treturn ast;
\t}\n\n"

let defaultInitial () =
("\t@Override public " ^ !stringDomain ^ " newInitialFlow() {
\t\treturn " ^ 
  (match !typeDomain with
  | Some (Set _) -> "Sets.newHashSet()"
  | _ -> ""
  ) ^ ";
\t}\n\n")

let copy () =
"\t@Override public " ^ !stringDomain ^ " copy(" ^ !stringDomain ^ " src) {
\t\treturn " ^ 
  (match !typeDomain with
  | Some (Set _) -> "Sets.newHashSet(src)"
  | _ -> ""
  ) ^ ";
\t}\n\n"


let constructor () =
"\tpublic " ^ !analysisName ^ "(ASTNode tree) {
\t\tsuper(tree);
\t}\n\n"

let main () =
"\tpublic static void main(String[] args) {
\t\tProgram ast = parseOrDie(args[0]);
\t\t" ^ !analysisName ^ " analysis = new " ^ !analysisName ^ "(ast);
\t\tanalysis.analyze();
\t}\n\n"

(* \t\tprintWithAnalysisResults(ast, analysis); *)
(* Generation *)

let rec printlvl = function
  | 0 -> ""
  | n -> "\t" ^ (printlvl (n-1))

(* n : current level. m : base level *)
let rec closeinnerlvl n m =
  if n >= m then
    (printlvl n) ^ "}\n" ^ (closeinnerlvl (n-1) m)
  else
    ""

let rec printtp = function
  | M.Name -> "Name"
  | M.Stmt -> "Stmt"
  | M.ExprStmt -> "ExprStmt"
  | M.AssignStmt -> "AssignStmt"
  | M.GlobalStmt -> "GlobalStmt"
  | M.PersistentStmt -> "PersistentStmt"
  | M.ShellCommandStmt -> "ShellCommandStmt"
  | M.BreakStmt -> "BreakStmt"
  | M.ContinueStmt -> "ContinueStmt"
  | M.ReturnStmt -> "ReturnStmt"
  | M.ForStmt -> "ForStmt"
  | M.WhileStmt -> "WhileStmt"
  | M.TryStmt -> "TryStmt"
  | M.SwitchStmt -> "SwitchStmt"
  | M.SwitchCaseBlock -> "SwitchCaseBlock"
  | M.DefaultCaseBlock -> "DefaultCaseBlock"
  | M.IfStmt -> "IfStmt"
  | M.IfBlock -> "IfBlock"
  | M.ElseBlock -> "ElseBlock"
  | M.Expr -> "Expr"
  | M.RangeExpr -> "RangeExpr"
  | M.ColonExpr -> "ColonExpr"
  | M.EndExpr -> "EndExpr"
  | M.LValueExpr -> "LValueExpr"
  | M.NameExpr -> "NameExpr"
  | M.ParameterizedExpr -> "ParameterizedExpr"
  | M.CellIndexExpr -> "CellIndexExpr"
  | M.DotExpr -> "DotExpr"
  | M.MatrixExpr -> "MatrixExpr"
  | M.CellArrayExpr -> "CellArrayExpr"
  | M.SuperClassMethodExpr -> "SuperClassMethodExpr"
  | M.Row -> "Row"
  | M.LiteralExpr -> "LiteralExpr"
  | M.IntLiteralExpr -> "IntLiteralExpr"
  | M.FPLiteralExpr -> "FPLiteralExpr"
  | M.StringLiteralExpr -> "StringLiteralExpr"
  | M.UnaryExpr -> "UnaryExpr"
  | M.UMinusExpr -> "UMinusExpr"
  | M.UPlusExpr -> "UPlusExpr"
  | M.NotExpr -> "NotExpr"
  | M.MTransposeExpr -> "MTransposeExpr"
  | M.ArrayTransposeExpr -> "ArrayTransposeExpr"
  | M.BinaryExpr -> "BinaryExpr"
  | M.PlusExpr -> "PlusExpr"
  | M.MinusExpr -> "MinusExpr"
  | M.MTimesExpr -> "MTimesExpr"
  | M.MDivExpr -> "MDivExpr"
  | M.MLDivExpr -> "MLDivExpr"
  | M.MPowExpr -> "MPowExpr"
  | M.ETimesExpr -> "ETimesExpr"
  | M.EDivExpr -> "EDivExpr"
  | M.ELDivExpr -> "ELDivExpr"
  | M.EPowExpr -> "EPowExpr"
  | M.AndExpr -> "AndExpr"
  | M.OrExpr -> "OrExpr"
  | M.ShortCircuitAndExpr -> "ShortCircuitAndExpr"
  | M.ShortCircuitOrExpr -> "ShortCircuitOrExpr"
  | M.LTExpr -> "LTExpr"
  | M.GTExpr -> "GTExpr"
  | M.LEExpr -> "LEExpr"
  | M.GEExpr -> "GEExpr"
  | M.EQExpr -> "EQExpr"
  | M.NEExpr -> "NEExpr"
  | M.FunctionHandleExpr -> "FunctionHandleExpr"
  | M.LambdaExpr -> "LambdaExpr"



let getTypeDomain () = match !typeDomain with
  | Some e -> e
  | None -> assert false

let rec genDomain : domain -> string = function
  | Set d -> "Set<" ^ (genDomain d) ^ ">"
  | Tuple (d1, d2) -> "Map<" ^ (genDomain d1) ^ ", " ^ (genDomain d2) ^ ">"
  | Name id -> genId id
  | Matlab m -> printtp m

let rec genTExpr = function
  | NoTyp e -> raise UntypedExpression
  | Typ (e, t) -> genFlowExpr e

and genFlowExpr e = match e with
  | Var id -> genId id
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
  | EmptySet -> EmptySet
  | Plus  (x, y) -> Plus  (expandTypInOut x, expandTypInOut y)
  | Minus (x, y) -> Minus (expandTypInOut x, expandTypInOut y)
  | Times (x, y) -> Times (expandTypInOut x, expandTypInOut y)
  | Var (Id i) -> 
    if i = "in" then 
      Var (Id "currentInSet") 
    else 
      if i = "out" then
        Var (Id "currentOutSet")
      else
        Var (Id i)
  | Set e -> Set (expandTypInOut e)
  | Tuple (e1, e2) -> Tuple (expandTypInOut e1, expandTypInOut e2)
    
and expandTypInOut = function
      | NoTyp e -> NoTyp (expandInOut e)
      | Typ (e, t) -> Typ (expandInOut e, t)

let rec methodStmt lvl access (e : mpat) tp = match e with
  | Var i -> let _ = output := (!output ^ (printlvl lvl) ^ (printtp tp) ^ " " 
                                 ^ (genId i) ^ " = " ^ access ^ ";\n") in
             lvl
  | Node m -> 
    let _ = output := (!output ^ (printlvl lvl) ^ "if(" ^ access ^ " instanceof " ^
                          (printtp (T.decidetp m)) ^ ") {\n") in
      methodBody (lvl + 1) access m
  | NodeAs (i, m) -> 
    let _ = 
      output := (!output ^ (printlvl lvl) ^ (printtp tp) ^ " " 
                 ^ (genId i) ^ " = " ^ access ^ ";\n") in
    let _ = output := (!output ^ (printlvl lvl) ^ "if(" ^ access ^ " instanceof " ^
                          (printtp (T.decidetp m)) ^ ") {\n") in
    methodBody lvl (genId i) m

and methodBody lvl access e = match e with
  | Stmt -> lvl
  | ExprStmt p -> methodStmt lvl (access ^ ".getExpr()") p M.Expr
  | AssignStmt (n1, n2) -> 
    let lvl' = methodStmt lvl (access ^ ".getLHS()") n1 M.Expr in
    methodStmt lvl' (access ^ ".getRHS()") n2 M.Expr
  | GlobalStmt i -> lvl (* TODO: Make sure it is actually globalstmt the type... probably not *)
  | PersistentStmt i -> lvl (* TODO *)
  | BreakStmt -> lvl
  | ContinueStmt -> lvl
  | ReturnStmt -> lvl

  | NameExpr i -> methodStmt lvl (access ^ ".getName()") i M.Name


let genCond = function
  | True -> "true"
  | False -> "false"
  | Eq (e1, e2) -> "" (* Find eq comparison for sets and etc. *)

let rec genStmt lvl = function
  | If (c, sl) -> 
    let _ = output := (!output ^ (printlvl lvl) ^ "if (" ^ (genCond c) ^ ") {\n") in
    let _ = List.iter (genStmt (lvl + 1)) sl in
    output := (!output ^ (closeinnerlvl lvl lvl))
  | For (m, i, sl) ->
    let _ = output := (!output ^ (printlvl lvl) ^ "for (  ) {\n") in
    let _ = List.iter (genStmt (lvl + 1)) sl in
    output := (!output ^ (closeinnerlvl lvl lvl))
  | Assign (i, e) -> 
    output := (!output ^ (printlvl lvl) ^ (genId i) ^ " = " 
               ^ (genTExpr e) ^ ";\n")

let rec genFExpr = function
  | Var id -> genId id
  | Plus (o1, o2) -> 
    let s1 = genFExpr o1 in
    let s2 = genFExpr o2 in
     "Sets.union(" ^ s1 ^ ", " ^ s2 ^ ")" 
  | Minus (o1, o2) -> 
    let s1 = genFExpr o1 in
    let s2 = genFExpr o2 in
    "Sets.difference(" ^ s1 ^ ", " ^ s2 ^ ")" 
  | Times (o1, o2) -> 
    let s1 = genFExpr o1 in
    let s2 = genFExpr o2 in
     "Sets.intersection(" ^ s1 ^ ", " ^ s2 ^ ")" 

let genBranch nodeName initend e =
  let (init, cp, ending) = initend in
  let _ = match e with
    | (AssignStmt (m1, m2), sl) ->
      let _ = output := (!output ^ "\t@Override public void caseAssignStmt(AssignStmt " 
                         ^ nodeName ^ ") {\n\t\t" ^ init (* ^ def vars *) ^ "\n") in
      let lvl1 = methodStmt 2 (nodeName ^ ".getLHS()") m1 M.Expr in
      let lvl2 = methodStmt lvl1 (nodeName ^ ".getRHS()") m2 M.Expr in
      let _ = List.iter (genStmt lvl2) sl in
      output := (!output ^ (closeinnerlvl (lvl2 - 1) 2))
    | (Stmt, sl) ->
      let _ = output := (!output ^ "\t@Override public void caseStmt(Stmt " 
                         ^ nodeName ^ ") {\n\t\t" ^ init (* ^ def vars *) ^ "\n") in
      List.iter (genStmt 2) sl
  in
  let _ = output := (!output ^ "\t\t" ^ cp ^ "\n") in
  let _ = output := (!output ^ "\t\t" ^ ending ^ "\n") in
  let _ = output := (!output ^ "\t}\n\n") in
  ()
    
let genFlow = function
  | Flow (i, (i', fe), bs) ->
    let nodeName = (genId i) in
    let initend = match !direction with
      | Some Forward ->
        if genId i' = "out" then
          ("inFlowSets.put(node, copy(currentInSet));",
           "currentOutSet = " ^ (genFExpr fe),
           "outFlowSets.put(node, copy(currentOutSet));")
        else
          assert false
      | Some Backward -> 
        if genId i' = "in" then
          ("inFlowSets.put(node, copy(currentInSet));",
           "currentInSet = " ^ (genFExpr fe),
           "outFlowSets.put(node, copy(currentOutSet));")
        else
          assert false
    in
    List.iter (genBranch nodeName initend) bs
                           

let genDirection d = 
  let _ = direction := (Some d) in
  match d with
  | Forward -> "ForwardAnalysis"
  | Backward -> "BackwardAnalysis"


let genMerge (Merge (i1, i2, e)) =
  let _ = output := (!output ^ "\t@Override public " ^ !stringDomain ^ " merge(" 
                     ^ !stringDomain ^ " " ^ (genId i1) ^ ", " ^ !stringDomain 
                     ^ " " ^ genId i2 ^ ") {\n") in
  let s = genTExpr e in
  let _ = output := (!output ^ "\t\treturn Sets.newHashSet(" ^ s ^ ");\n") in
  output := (!output ^ "\t}\n\n")

let genBodies = function
  | Body (i, m, f, a) -> 
    let _ = genMerge m in
    let _ = genFlow f in ()

(* Each analysis is in its own file which has the name of the analysis *)
let genAnalysis = function
  | Analysis (name, direction, domain, bodies) ->
    let _ = output := "" in
    let _ = analysisName := (genId name) in
    let _ = stringDomain := (genDomain domain) in
    let _ = typeDomain := (Some domain) in
    let _ = output := !output ^ imports in
    let _ = output := !output ^ "public class " ^ !analysisName ^ " extends " 
              ^ (genDirection direction) ^ "<" ^ !stringDomain ^ "> {\n" in
    let _ = output := !output ^ parseOrDie in
    let _ = output := !output ^ (main ()) in
    let _ = output := !output ^ (constructor ()) in
    let _ = output := !output ^ (defaultInitial ()) in
    let _ = output := !output ^ (copy ()) in
    let _ = genBodies bodies in
    (* Ajouter le reste du body *)
    let _ = output := !output ^ "}\n" in
    print_endline !output          

let codegen = function 
  | Program a -> 
    List.iter genAnalysis a
  | Empty -> ()
