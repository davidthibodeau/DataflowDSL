open Syntax
module M = MatlabAst

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

let rec printtp = function 
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
  | M.IfStmt -> "IfStmt"
  | M.IfBlock -> "IfBlock"
  | M.ElseBlock -> "ElseBlock"
  | M.Expr -> "Expr"
  | M.RangeExpr -> "RangeExpr"
  | M.ColonExpr -> "ColonExpr"
  | M.EndExpr -> "EndExpr"
  | M.LValueExpr -> "LValueExpr"
  | M.NameExpr -> "NameExpr"
  | M.ParametrizedExpr -> "ParametrizedExpr"
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

let genId = function
  | Id id -> id

 let rec genSetOp = function
   | Var id ->
     genId id
   | Plus (o1, o2) -> 
     let s1 = genSetExpr o1 in
     let s2 = genSetExpr o2 in
     "Sets.union(" ^ s1 ^ ", " ^ s2 ^ ")" 
   | Minus (o1, o2) -> 
     let s1 = genSetExpr o1 in
     let s2 = genSetExpr o2 in
     "Sets.difference(" ^ s1 ^ ", " ^ s2 ^ ")" 
   | Times (o1, o2) -> 
     let s1 = genSetExpr o1 in
     let s2 = genSetExpr o2 in
     "Sets.intersection(" ^ s1 ^ ", " ^ s2 ^ ")" 

 and genSetExpr = function
   | Op o -> genSetOp o

let genFlowExpr e = match !typeDomain, e with
  | Some (Set _), Op o ->
    "Sets.newHashSet(" ^ (genSetOp o) ^ ")"

let rec expandInOut = function
  | EmptySet -> EmptySet
  | Op o -> 
    let rec expandInOutOp = function
      | Plus  (x, y) -> Plus  (expandInOut x, expandInOut y)
      | Minus (x, y) -> Minus (expandInOut x, expandInOut y)
      | Times (x, y) -> Times (expandInOut x, expandInOut y)
      | Var (Id i) -> 
        if i = "in" then 
          Var (Id "currentInSet") 
        else 
          if i = "out" then
            Var (Id "currentOutSet")
          else
            Var (Id i)
    in
    Op (expandInOutOp o)
  | Set e -> Set (expandInOut e)
  | Tuple (e1, e2) -> Tuple (expandInOut e1, expandInOut e2)

let genFlowStmt = function
  | Assign (i, e) -> 
    let e1 = expandInOut e in
    (match !direction with
    | Some Forward ->
      if genId i = "out" then
        ("inFlowSets.put(node, copy(currentInSet));",
         "currentOutSet = " ^ (genFlowExpr e1),
         "outFlowSets.put(node, copy(currentOutSet));")
      else
        assert false
    | Some Backward -> 
      if (genId i) = "in" then
        ("inFlowSets.put(node, copy(currentInSet));",
         "currentInSet = " ^ (genFlowExpr e1),
         "outFlowSets.put(node, copy(currentOutSet));")
      else
        assert false
    )
  | _ -> assert false (* NOTE : Do we actually want more complicated statements here ? *)

let rec methodStmt lvl access (e : mpat) tp = match e with
  | Var i -> 
    output := (!output ^ (printlvl lvl) ^ (printtp tp) ^ " " 
               ^ (genId i) ^ " = " ^ access ^ ";\n")
  | Node m -> methodBody lvl access m
  | NodeAs (i, m) -> 
    let _ = 
      output := (!output ^ (printlvl lvl) ^ (printtp tp) ^ " " 
                 ^ (genId i) ^ " = " ^ access ^ ";\n")
    in
    methodBody lvl (genId i) m

and methodBody lvl access e = match e with
  | Stmt -> ()
  | ExprStmt p -> methodStmt lvl (access ^ ".getExpr()") p M.Expr
  | AssignStmt (n1, n2) -> 
    let _ = methodStmt lvl (access ^ ".getLHS()") n1 M.Expr in
    methodStmt lvl (access ^ ".getRHS()") n2 M.Expr
  | GlobalStmt i -> () (* TODO: Make sure it is actually globalstmt the type... probably not *)
  | PersistentStmt i -> () (* TODO *)
  | BreakStmt -> ()
  | ContinueStmt -> ()
  | ReturnStmt -> ()

  | NameExpr i -> output := (!output ^ (printlvl lvl) ^ (printtp M.NameExpr) ^ " " 
               ^ (genId i) ^ " = " ^ access ^ ";\n")    

let genCond = function
  | True -> "true"
  | False -> "false"
  | Eq (e1, e2) -> "" (* Find eq comparison for sets and etc. *)

let rec genStmt lvl = function
  | If (c, sl) -> 
    let _ = output := (!output ^ (printlvl lvl) ^ "if (" ^ (genCond c) ^ ") {\n") in
    let _ = genStmt (lvl + 1) in
    output := (!output ^ (printlvl lvl) ^ "}\n")
  | Assign (i, e) -> 
    output := (!output ^ (printlvl lvl) ^ (genId i) ^ " = " ^ (genFlowExpr e) ^ ";\n")
    

let genFlow = function
  | Flow (i, s, bs) ->
    let nodeName = (genId i) in
    let (init, cp, ending) as initend = genFlowStmt s in
    let genBranch = function
      | (AssignStmt (m1, m2), sl) ->
        let _ = output := (!output ^ "\t@Override public void caseAssignStmt(AssignStmt " 
                           ^ nodeName ^ ") {\n\t\t" ^ init (* ^ def vars *) ^ "\n") in
        let _ = methodStmt 2 (nodeName ^ ".getLHS()") m1 M.Expr in
        let _ = methodStmt 2 (nodeName ^ ".getRHS()") m2 M.Expr in
        let _ = List.iter (genStmt 2) sl in
        let _ = output := (!output ^ "\t\t" ^ cp ^ "\n") in
        let _ = output := (!output ^ "\t\t" ^ ending ^ "\n") in
        let _ = output := (!output ^ "\t}\n\n") in
        ()
    in
    List.iter genBranch bs
                           

let genDirection d = 
  let _ = direction := (Some d) in
  match d with
  | Forward -> "ForwardAnalysis"
  | Backward -> "BackwardAnalysis"

let rec genDomain : domain -> string = function
  | Set d -> "Set<" ^ (genDomain d) ^ ">"
  | Tuple (d1, d2) -> "Map<" ^ (genDomain d1) ^ ", " ^ (genDomain d2) ^ ">"
  | Name id -> genId id
  | Matlab m -> printtp m

let genMerge (Merge (i1, i2, Op e)) =
  let _ = output := (!output ^ "\t@Override public " ^ !stringDomain ^ " merge(" 
                     ^ !stringDomain ^ " " ^ (genId i1) ^ ", " ^ !stringDomain 
                     ^ " " ^ genId i2 ^ ") {\n") in
  match !typeDomain with
  | Some (Set _) -> 
    let s = genSetOp e in
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
