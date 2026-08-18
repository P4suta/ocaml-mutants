(* OCaml 5.5 adds [Local_opaque_item] for statically unknown definitions. *)
let is_stdlib uid =
  match uid with
  | Shape.Uid.Item { comp_unit; _ }
  | Shape.Uid.Local_opaque_item { comp_unit; _ } ->
      String.equal comp_unit "Stdlib"
  | Shape.Uid.Compilation_unit unit -> String.equal unit "Stdlib"
  | Shape.Uid.Predef _ -> true
  | Shape.Uid.Internal -> false
