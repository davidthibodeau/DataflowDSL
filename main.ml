open Syntax

let filename = Sys.argv.(1)
(*let action = Sys.argv.(2) *)
let ast = ref Empty

let () = Top.run filename ast


