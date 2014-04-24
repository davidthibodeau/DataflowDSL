open Matlab
module M = Ast

type ids = id list

type direction =
| Forward
| Backward

type domain =
| Set of domain
| Tuple of domain * domain
| Matlab of M.ast
| Name of id

type typexpr = 
| EmptySet
| Set of expr
| Tuple of expr * expr
| Plus of expr * expr
| Minus of expr * expr
| Times of expr * expr
| Var of id

and expr =
| NoTyp of typexpr
| Typ of typexpr * domain

type cond =
| True
| False
| Eq of expr * expr

type stmt =
| If of cond * stmt list
| For of M.vpat * id * domain option * stmt list
| Assign of id * expr

type fexpr = 
| Plus of fexpr * fexpr
| Minus of fexpr * fexpr
| Times of fexpr * fexpr
| Var of id

type init = 
| NoInit

type merge =
| Merge of id * id * expr

type flow =
| NoFlow
| Flow of id * (id * fexpr) * (M.pat * stmt list) list
| CheckedFlow of id * (id * fexpr) * (M.ast * (M.vpat list * stmt list) list) list

type aux =
| NoAux

type body =
| I of init
| M of merge
| F of flow
| A of aux

type bodies =
| Body of init * merge * flow * aux list

type analysis =
| Analysis of id * direction * domain * bodies

type program = 
| Program of analysis list
| Empty

