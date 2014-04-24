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

let rec genTExpr = function
  | NoTyp e -> raise UntypedExpression
  | Typ (e, t) -> genFlowExpr e

and genFlowExpr e = match e with
  | Var id -> id
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
  | Var i -> 
    if i = "in" then 
      Var "currentInSet"
    else 
      if i = "out" then
        Var "currentOutSet"
      else
        Var i
  | Set e -> Set (expandTypInOut e)
  | Tuple (e1, e2) -> Tuple (expandTypInOut e1, expandTypInOut e2)
    
and expandTypInOut = function
      | NoTyp e -> NoTyp (expandInOut e)
      | Typ (e, t) -> Typ (expandInOut e, t)

let rec methodStmt lvl access e tp = match e with
  | M.Var i -> 
      let _ = writeline lvl ((M.printtp tp) ^ " " ^ i ^ " = " ^ access ^ ";") in
      lvl
  | M.Node ((t, l) as m) -> 
      let _ = writeline lvl ("if(" ^ access ^ " instanceof " ^ (M.printtp t) ^ ") {") in
      methodBody (lvl + 1) access m
  | M.NodeAs (i, (t, l as m)) -> 
      let _ = writeline lvl ((M.printtp tp) ^ " " ^ i ^ " = " ^ access ^ ";") in
      let _ = writeline lvl ("if(" ^ access ^ " instanceof " ^ (M.printtp t) ^ ") {") in
      methodBody lvl i m

and methodBody lvl access e = match e with
  | M.Stmt, [] -> lvl
  | M.ExprStmt, [p] -> methodStmt lvl (access ^ ".getExpr()") p M.Expr
  | M.AssignStmt, [n1; n2] -> 
    let lvl' = methodStmt lvl (access ^ ".getLHS()") n1 M.Expr in
    methodStmt lvl' (access ^ ".getRHS()") n2 M.Expr
  | M.GlobalStmt, [i] -> lvl (* TODO: Make sure it is actually globalstmt the type... probably not *)
  | M.PersistentStmt, [i] -> lvl (* TODO *)
  | M.BreakStmt, [] -> lvl
  | M.ContinueStmt, [] -> lvl
  | M.ReturnStmt, [] -> lvl

  | M.NameExpr, [i] -> methodStmt lvl (access ^ ".getName()") i M.Name


let genCond = function
  | True -> "true"
  | False -> "false"
  | Eq (e1, e2) -> "" (* Find eq comparison for sets and etc. *)

let rec genStmt lvl = function
  | If (c, sl) -> 
      let _ = writeline lvl ("if (" ^ (genCond c) ^ ") {") in
      let _ = List.iter (genStmt (lvl + 1)) sl in
      writeclosings lvl lvl
  | For (m, i, sl) ->
      let _ = writeline lvl ("for (  ) {") in
      let _ = List.iter (genStmt (lvl + 1)) sl in
      writeclosings lvl lvl
  | Assign (i, e) -> 
      writeline lvl (i ^ " = " ^ (genTExpr e) ^ ";")

let rec genFExpr = function
  | Var id -> id
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

let genBranch nodeName initend (a, ll) =
  let (init, cp, ending) = initend in
  let _ = match a with
    | M.AssignStmt ->
      let _ = writecase nodeName "AssignStmt" in
      let _ = writeline 2 init in ()
  (* Write defs vars from flow stmt  
     Note: Should simply call a function doing it since it will appear in each case *)

(*
      let lvl1 = methodStmt 2 (nodeName ^ ".getLHS()") m1 M.Expr in
      let lvl2 = methodStmt lvl1 (nodeName ^ ".getRHS()") m2 M.Expr in
      let _ = List.iter (genStmt lvl2) sl in
      writeclosings (lvl2 - 1) 2
 *)
    | M.Stmt ->
        let _ = writecase nodeName "Stmt" in
        let _ = writeline 2 init in ()
  in
  let _ = writeline 2 cp in
  let _ = writeline 2 ending in
  let _ = writeline 1 "\n" in
  ()
    
let genFlow = function
  | CheckedFlow (i, (i', fe), bs) ->
    let nodeName = i in
    let initend = match !direction with
      | Some Forward ->
        if i' = "out" then
          ("inFlowSets.put(node, copy(currentInSet));",
           "currentOutSet = " ^ (genFExpr fe),
           "outFlowSets.put(node, copy(currentOutSet));")
        else
          assert false
      | Some Backward -> 
        if i' = "in" then
          ("outFlowSets.put(node, copy(currentOutSet));",
           "currentInSet = " ^ (genFExpr fe),
           "inFlowSets.put(node, copy(currentInSet));")
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
