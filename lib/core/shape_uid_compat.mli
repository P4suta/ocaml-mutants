val is_stdlib : Shape.Uid.t -> bool
(** Whether the resolved uid denotes an item owned by [Stdlib]. The
    implementation file is selected per compiler version in [dune] because the
    [Shape.Uid.t] constructor set changes between OCaml releases
    ([Local_opaque_item] exists only from 5.5). *)
