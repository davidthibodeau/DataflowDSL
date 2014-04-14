open Syntax

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
"\t@Override public " ^ !stringDomain ^ " newInitialFlow() {
\t\treturn " ^ 
  (match !typeDomain with
  | Some (Set _) -> "Sets.newHashSet()"
  | _ -> ""
  ) ^ ";
\t}\n\n"

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

let genId = function
  | Id id -> id

 let rec genSetOp = function
   | Var id ->
     genId id
   | Plus (o1, o2) -> 
     let s1 = genSetOp o1 in
     let s2 = genSetOp o2 in
     "Sets.union(" ^ s1 ^ ", " ^ s2 ^ ")" 
   | Minus (o1, o2) -> 
     let s1 = genSetOp o1 in
     let s2 = genSetOp o2 in
     "Sets.difference(" ^ s1 ^ ", " ^ s2 ^ ")" 
   | Times (o1, o2) -> 
     let s1 = genSetOp o1 in
     let s2 = genSetOp o2 in
     "Sets.intersection(" ^ s1 ^ ", " ^ s2 ^ ")" 

let genFlowExpr e = match !typeDomain, e with
  | Some (Set _), Op o ->
    "Sets.newHashSet(" ^ (genSetOp o) ^ ")"

let rec expandInOut = function
  | EmptySet -> EmptySet
  | Op o -> 
    let rec expandInOutOp = function
      | Plus  (x, y) -> Plus  (expandInOutOp x, expandInOutOp y)
      | Minus (x, y) -> Minus (expandInOutOp x, expandInOutOp y)
      | Times (x, y) -> Times (expandInOutOp x, expandInOutOp y)
      | Var (Id i) -> 
        if i == "in" then 
          Var (Id "currentInSet") 
        else 
          if i == "out" then
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
      if genId i == "out" then
        ("inFlowSets.put(node, copy(currentInSet));",
         "currentOutSet = " ^ (genFlowExpr e1),
         "outFlowSets.put(node, copy(currentOutSet));")
      else
        assert false
    | Some Backward -> 
      If genId i == "in" then
        ("inFlowSets.put(node, copy(currentInSet));",
         "currentInSet = " ^ (genFlowExpr e1),
         "outFlowSets.put(node, copy(currentOutSet));")
      else
        assert false
    )
  | _ -> assert false (* NOTE : Do we actually want more complicated statements here ? *)

let genBranch = function
  | 

let genBranches = function
  | [] -> ()
  | b :: bs -> 
    let _ = genBranch b in
    genBranches bs

let genFlow = function
  | Flow (i, s, bs) ->
    let _ = nodeString := (genId i) in
    let (init, cp, ending) = genFlowStmt s in
    
    
    genBranches bs

(*
let genFlow (Flow (i, s, ns)) = 
  let nodeName = genId i in
  (* need to go through s to be able to initialize all vars *)
  let binit = ref (match 
*)

let genDirection d = 
  let _ = direction := (Some d) in
  match d with
  | Forward -> "ForwardAnalysis"
  | Backward -> "BackwardAnalysis"

let rec genDomain : domain -> string = function
  | Set d -> "Set<" ^ (genDomain d) ^ ">"
  | Tuple (d1, d2) -> "Map<" ^ (genDomain d1) ^ ", " ^ (genDomain d2) ^ ">"
  | Name id -> genId id

let genMerge (Merge (i1, i2, Op e)) =
  let _ = output := (!output ^ "\t@Override public " ^ !stringDomain ^ " merge(" 
                     ^ !stringDomain ^ " " ^ (genId i1) ^ ", " ^ !stringDomain 
                     ^ " " ^ genId i2 ^ ") {\n") in
  match !typeDomain with
  | Some (Set _) -> 
    let s = genSetOp e in
    let _ = output := (!output ^ "\treturn Sets.newHashSet(" ^ s ^ ");\n") in
    output := (!output ^ "\t}\n")

let genBodies = function
  | Body (i, m, f, a) -> 
    let _ = genMerge m in ()

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
