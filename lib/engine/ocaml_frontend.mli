type skip_reason =
  | Ghost_location
  | Generated_source
  | Test_source
  | Imprecise_mapping
  | Unsupported_expression
  | Duplicate

type skip_summary = {
  reason : skip_reason;
  count : int;
  examples : string list;
}

type discovery = {
  catalog : Ocaml_mutants_core.Catalog.t;
  skipped : skip_summary list;
}

val discover :
  root:string ->
  cmt_files:string list ->
  selected_source:(string -> bool) ->
  operators:Ocaml_mutants_core.Operator.t list ->
  (discovery, Error.t) result

val instrument_files :
  root:string -> Ocaml_mutants_core.Catalog.t -> (string list, Error.t) result

val skip_reason_name : skip_reason -> string

module For_testing : sig
  val verify_boolean_shadow :
    source:Ocaml_mutants_core.Source.t ->
    path:string ->
    range:Ocaml_mutants_core.Source_range.t ->
    expression:Typedtree.expression ->
    legacy:Ocaml_mutants_core.Mutant.t ->
    (unit, Error.t) result

  val verify_boolean_connective_shadow :
    source:Ocaml_mutants_core.Source.t ->
    path:string ->
    range:Ocaml_mutants_core.Source_range.t ->
    expression:Typedtree.expression ->
    legacy:Ocaml_mutants_core.Mutant.t ->
    (unit, Error.t) result

  val verify_binary_shadow :
    source:Ocaml_mutants_core.Source.t ->
    path:string ->
    range:Ocaml_mutants_core.Source_range.t ->
    expression:Typedtree.expression ->
    legacy:Ocaml_mutants_core.Mutant.t ->
    (unit, Error.t) result

  val verify_condition_shadow :
    source:Ocaml_mutants_core.Source.t ->
    path:string ->
    range:Ocaml_mutants_core.Source_range.t ->
    expression:Typedtree.expression ->
    legacy:Ocaml_mutants_core.Mutant.t ->
    (unit, Error.t) result

  val verify_if_branch_shadow :
    source:Ocaml_mutants_core.Source.t ->
    path:string ->
    range:Ocaml_mutants_core.Source_range.t ->
    conditional:Typedtree.expression ->
    target:Ocaml_mutants_core.Operator.Spec.if_branch_target ->
    legacy:Ocaml_mutants_core.Mutant.t ->
    (unit, Error.t) result

  val verify_sequence_shadow :
    source:Ocaml_mutants_core.Source.t ->
    path:string ->
    range:Ocaml_mutants_core.Source_range.t ->
    expression:Typedtree.expression ->
    legacy:Ocaml_mutants_core.Mutant.t ->
    (unit, Error.t) result

  val verify_return_shadow :
    source:Ocaml_mutants_core.Source.t ->
    path:string ->
    range:Ocaml_mutants_core.Source_range.t ->
    expression:Typedtree.expression ->
    legacy:Ocaml_mutants_core.Mutant.t ->
    (unit, Error.t) result

  val verify_boolean_shadow_event :
    source:Ocaml_mutants_core.Source.t ->
    path:string ->
    range:Ocaml_mutants_core.Source_range.t ->
    expression:Typedtree.expression ->
    legacy:Ocaml_mutants_core.Mutant.t list ->
    (unit, Error.t) result

  val verify_boolean_connective_shadow_event :
    source:Ocaml_mutants_core.Source.t ->
    path:string ->
    range:Ocaml_mutants_core.Source_range.t ->
    expression:Typedtree.expression ->
    legacy:Ocaml_mutants_core.Mutant.t list ->
    (unit, Error.t) result

  val verify_binary_shadow_event :
    source:Ocaml_mutants_core.Source.t ->
    path:string ->
    range:Ocaml_mutants_core.Source_range.t ->
    expression:Typedtree.expression ->
    legacy:Ocaml_mutants_core.Mutant.t list ->
    (unit, Error.t) result

  val verify_condition_shadow_event :
    source:Ocaml_mutants_core.Source.t ->
    path:string ->
    range:Ocaml_mutants_core.Source_range.t ->
    expression:Typedtree.expression ->
    legacy:Ocaml_mutants_core.Mutant.t list ->
    (unit, Error.t) result

  val verify_if_branch_shadow_event :
    source:Ocaml_mutants_core.Source.t ->
    path:string ->
    range:Ocaml_mutants_core.Source_range.t ->
    conditional:Typedtree.expression ->
    target:Ocaml_mutants_core.Operator.Spec.if_branch_target ->
    legacy:Ocaml_mutants_core.Mutant.t list ->
    (unit, Error.t) result

  val verify_sequence_shadow_event :
    source:Ocaml_mutants_core.Source.t ->
    path:string ->
    range:Ocaml_mutants_core.Source_range.t ->
    expression:Typedtree.expression ->
    legacy:Ocaml_mutants_core.Mutant.t list ->
    (unit, Error.t) result

  val verify_return_shadow_event :
    source:Ocaml_mutants_core.Source.t ->
    path:string ->
    range:Ocaml_mutants_core.Source_range.t ->
    expression:Typedtree.expression ->
    legacy:Ocaml_mutants_core.Mutant.t list ->
    (unit, Error.t) result

  val commit_return_shadow_event :
    source:Ocaml_mutants_core.Source.t ->
    path:string ->
    range:Ocaml_mutants_core.Source_range.t ->
    expression:Typedtree.expression ->
    legacy:Ocaml_mutants_core.Mutant.t list ->
    commit:(Ocaml_mutants_core.Mutant.t list -> unit) ->
    (unit, Error.t) result
end
