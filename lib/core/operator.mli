module Family : sig
  type t =
    | Boolean_literal
    | Condition_negation
    | Boolean_connective
    | Comparison
    | Integer_arithmetic
    | Float_arithmetic
    | If_branch
    | Sequence_deletion
    | Return_replacement

  val all : t list
  val name : t -> string
  val of_string : string -> (t, string) result
  val pp : Format.formatter -> t -> unit
end

module Profile : sig
  type t = Balanced | Strong | All

  val all : t list
  val name : t -> string
  val of_string : string -> (t, string) result
  val includes : t -> t -> bool
end

type t = Family.t =
  | Boolean_literal
  | Condition_negation
  | Boolean_connective
  | Comparison
  | Integer_arithmetic
  | Float_arithmetic
  | If_branch
  | Sequence_deletion
  | Return_replacement

module Rule : sig
  type t

  val family : t -> Family.t
  val name : t -> string
  val version : t -> int
  val profile : t -> Profile.t
  val stable_name : t -> string
  val all : t list
  val of_stable_name : string -> (t, string) result
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit

  module For_testing : sig
    val registry_errors : unit -> string list
    val compatibility_examples : unit -> (t * string * string) list
  end
end

val all : Family.t list
val name : Family.t -> string
val of_string : string -> (Family.t, string) result
val pp : Format.formatter -> Family.t -> unit
val rules : Family.t -> Rule.t list

val rule_for_replacement :
  Family.t -> original:string -> replacement:string -> (Rule.t, string) result

module Spec : sig
  module Replacement_plan : sig
    type t

    val source_bytes : t -> string
    val replacement_bytes : t -> string
    val semantic_key : t -> string

    module For_testing : sig
      val create :
        source_bytes:string ->
        replacement_bytes:string ->
        semantic_key:string ->
        (t, string) result

      val require_static :
        context:string ->
        source_bytes:string ->
        replacement_bytes:string ->
        semantic_key:string ->
        t
    end
  end

  module Typed_evidence : sig
    type primitive =
      | Bool
      | Int
      | Float
      | String
      | Unit
      | List
      | Option
      | Other

    val primitive : ?environment:Env.t -> Types.type_expr -> primitive

    val is_resolved_stdlib_value :
      path:Path.t ->
      description:Types.value_description ->
      environment:Env.t ->
      name:string ->
      bool

    val operator_application_is_typed :
      token:string ->
      result_type:Types.type_expr ->
      argument_types:Types.type_expr list ->
      bool

    val operator_application_is_typed_in :
      environment:Env.t ->
      token:string ->
      result_type:Types.type_expr ->
      argument_types:Types.type_expr list ->
      bool
  end

  type rejection
  type packed
  type if_branch_target = Then_branch | Else_branch
  type binary_transition

  type evaluation =
    | Candidate of { rule : Rule.t; plan : Replacement_plan.t }
    | Rejection of { rule : Rule.t; reason : rejection }

  val rule : packed -> Rule.t
  val rejection_name : rejection -> string

  val find_binary_transition : original:string -> binary_transition option
  (** Projects the unique exact binary-operator transition from the registry.
      This is the compatibility oracle's metadata view; candidate production
      remains owned by the typed evaluators below. *)

  val binary_transition_rule : binary_transition -> Rule.t
  val binary_transition_replacement : binary_transition -> string

  val render_binary_transition :
    binary_transition -> left:string -> right:string -> string
  (** Renders through the same registry entry used by the production evaluator,
      including the short-circuit-preserving Boolean forms. *)

  val if_branch_rule : if_branch_target -> Rule.t
  (** Projects the rule attached to a typed branch-replacement target. *)

  val neutral_return_replacements :
    ?environment:Env.t -> Types.type_expr -> string list
  (** Projects neutral values from the return-rule registry in registry order.
  *)

  val compare_deduplication_precedence : Family.t -> Family.t -> int
  (** Compares the validated family precedence used only when two rules produce
      the exact same source transformation. *)

  val evaluate_boolean_literal :
    source_bytes:string -> Typedtree.expression -> evaluation list
  (** Evaluates the fixed Boolean registry after proving that the exact source
      slice reparses to the constructor carried by the typed expression. After
      exact legacy-oracle parity succeeds, these candidates are the production
      writer's output. *)

  val evaluate_boolean_connective :
    source_bytes:string -> Typedtree.expression -> evaluation list
  (** Evaluates the fixed [&&] and [||] registry with typed Stdlib evidence and
      short-circuit replacement plans. *)

  val evaluate_binary_operator :
    source_bytes:string -> Typedtree.expression -> evaluation list
  (** Evaluates the fixed Comparison, Integer-arithmetic, and Float-arithmetic
      registry. Boolean connectives are deliberately excluded. *)

  val evaluate_condition_negation :
    source_bytes:string -> Typedtree.expression -> evaluation list
  (** Evaluates the condition-negation shadow spec after proving a typed Boolean
      target and an independently parseable, byte-exact source slice. *)

  val evaluate_if_branch :
    source_bytes:string ->
    target:if_branch_target ->
    Typedtree.expression ->
    evaluation list
  (** [source_bytes] is the complete source file for this evaluator. The typed
      [if] locations, rather than a legacy replacement, own both branch slices.
      [target] names the branch whose bytes are replaced. *)

  val evaluate_sequence_deletion :
    source_bytes:string -> Typedtree.expression -> evaluation list
  (** Evaluates deletion of the left side of a typed sequence, retaining the
      exact source bytes of its right side. *)

  val evaluate_return_replacement :
    source_bytes:string -> Typedtree.expression -> evaluation list
  (** Evaluates the neutral return replacements admitted by the final typed
      shape. Typed refutation-case RHS nodes are explicitly not applicable.
      Manifest aliases are normalized only through the typed environment. *)

  module For_testing : sig
    val all_specs : unit -> packed list
    (** The single ordered built-in registry. Public rule metadata and the
        legacy compatibility projection are derived from this exact list. *)

    val boolean_literal_specs : unit -> packed list
    (** These definitions produce mutation candidates. Their family-qualified
        semantic keys remain audit evidence and are deliberately not the
        production deduplication contract. *)

    val boolean_connective_specs : unit -> packed list
    val binary_operator_specs : unit -> packed list
    val condition_negation_specs : unit -> packed list
    val if_branch_specs : unit -> packed list
    val sequence_deletion_specs : unit -> packed list
    val return_replacement_specs : unit -> packed list
    val visit_site_name : packed -> string
    val binary_original_invariant_errors : string list -> string list
    val if_branch_target_invariant_errors : if_branch_target list -> string list

    val evaluate_boolean_literal_at_site :
      definitions:packed list ->
      source_bytes:string ->
      Typedtree.expression ->
      evaluation list

    val evaluate_condition_at_site :
      definitions:packed list ->
      source_bytes:string ->
      Typedtree.expression ->
      evaluation list

    val evaluate_binary_application_at_site :
      definitions:packed list ->
      source_bytes:string ->
      Typedtree.expression ->
      evaluation list

    val evaluate_full_if_branch_at_site :
      definitions:packed list ->
      source_bytes:string ->
      target:if_branch_target ->
      Typedtree.expression ->
      evaluation list

    val evaluate_sequence_expression_at_site :
      definitions:packed list ->
      source_bytes:string ->
      Typedtree.expression ->
      evaluation list

    val evaluate_function_body_return_at_site :
      definitions:packed list ->
      source_bytes:string ->
      Typedtree.expression ->
      evaluation list

    val evaluate_expression :
      definitions:packed list ->
      source_bytes:string ->
      Typedtree.expression ->
      evaluation list
    (** Backward-compatible Boolean-literal-site test wrapper. Definitions for
        every other visit site are ignored before applicability is invoked. *)
  end
end
