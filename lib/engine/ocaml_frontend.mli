type skip_reason =
  | Ghost_location
  | Generated_source
  | Test_source
  | Imprecise_mapping
  | Unsupported_expression
      (** A typed site whose source position admits no in-place replacement:
          today this is the expression of a punned labeled or optional argument
          (~name / ?name), where the dispatch instrumentation would render
          invalid syntax. *)
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

val instrumentation_owner : Ocaml_mutants_core.Catalog.t -> string
(** Stable ownership token embedded in every runtime generated for one catalog.
    A readiness hit file accepts writes only from runtimes with this token. *)

val skip_reason_name : skip_reason -> string
