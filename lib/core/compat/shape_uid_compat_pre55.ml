(* OCaml 5.4: [Shape.Uid.t] has no [Local_opaque_item] constructor. *)
let is_stdlib uid =
  match uid with
  | Shape.Uid.Item { comp_unit; _ } -> String.equal comp_unit "Stdlib"
  | Shape.Uid.Compilation_unit unit -> String.equal unit "Stdlib"
  | Shape.Uid.Predef _ -> true
  | Shape.Uid.Internal -> false
