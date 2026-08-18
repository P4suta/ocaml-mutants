module Family = struct
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

  let all =
    [
      Boolean_literal;
      Condition_negation;
      Boolean_connective;
      Comparison;
      Integer_arithmetic;
      Float_arithmetic;
      If_branch;
      Sequence_deletion;
      Return_replacement;
    ]

  let name = function
    | Boolean_literal -> "boolean-literal"
    | Condition_negation -> "condition-negation"
    | Boolean_connective -> "boolean-connective"
    | Comparison -> "comparison"
    | Integer_arithmetic -> "integer-arithmetic"
    | Float_arithmetic -> "float-arithmetic"
    | If_branch -> "if-branch"
    | Sequence_deletion -> "sequence-deletion"
    | Return_replacement -> "return-replacement"

  let of_string value =
    match
      List.find_opt (fun family -> String.equal (name family) value) all
    with
    | Some family -> Ok family
    | None ->
        Error
          (Printf.sprintf "unknown operator family %S (expected one of: %s)"
             value
             (String.concat ", " (List.map name all)))

  let pp formatter family = Format.pp_print_string formatter (name family)
end

module Profile = struct
  type t = Balanced | Strong | All

  let all = [ Balanced; Strong; All ]

  let name = function
    | Balanced -> "balanced"
    | Strong -> "strong"
    | All -> "all"

  let of_string value =
    match
      List.find_opt (fun profile -> String.equal (name profile) value) all
    with
    | Some profile -> Ok profile
    | None ->
        Error
          (Printf.sprintf
             "unknown mutation profile %S (expected balanced, strong, or all)"
             value)

  let rank = function Balanced -> 0 | Strong -> 1 | All -> 2
  let includes selected required = rank required <= rank selected
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

type rule = {
  family : Family.t;
  name : string;
  version : int;
  profile : Profile.t;
}

let make_rule ~family ~name ~version ~profile =
  { family; name; version; profile }

let rule_family rule = rule.family
let rule_name rule = rule.name
let rule_version rule = rule.version
let rule_profile rule = rule.profile
let rule_stable_name rule = Printf.sprintf "%s@%d" rule.name rule.version

let compare_rule left right =
  String.compare (rule_stable_name left) (rule_stable_name right)

let equal_rule left right = compare_rule left right = 0

module Spec = struct
  module Replacement_plan = struct
    type key = Key of string

    type t = {
      source_bytes : string;
      replacement_bytes : string;
      semantic_key : string;
    }

    let key value =
      if String.trim value = "" then Error "semantic key is empty"
      else Ok (Key value)

    let create_with_key ~source_bytes ~replacement_bytes ~key:(Key semantic_key)
        =
      if String.trim source_bytes = "" then Error "source bytes are empty"
      else if String.trim replacement_bytes = "" then
        Error "replacement bytes are empty"
      else Ok { source_bytes; replacement_bytes; semantic_key }

    let create ~source_bytes ~replacement_bytes ~semantic_key =
      match key semantic_key with
      | Error _ as error -> error
      | Ok key -> create_with_key ~source_bytes ~replacement_bytes ~key

    let require_key ~context value =
      match key value with
      | Ok key -> key
      | Error message ->
          invalid_arg
            (Printf.sprintf "invalid %s replacement plan: %s" context message)

    let require_static ~context ~source_bytes ~replacement_bytes ~semantic_key =
      let result =
        if String.equal source_bytes replacement_bytes then
          Error "replacement does not change the source bytes"
        else create ~source_bytes ~replacement_bytes ~semantic_key
      in
      match result with
      | Ok plan -> plan
      | Error message ->
          invalid_arg
            (Printf.sprintf "invalid %s replacement plan: %s" context message)

    let source_bytes plan = plan.source_bytes
    let replacement_bytes plan = plan.replacement_bytes
    let semantic_key plan = plan.semantic_key

    module For_testing = struct
      let create = create
      let require_static = require_static
    end
  end

  module Typed_evidence = struct
    type primitive =
      | Bool
      | Int
      | Float
      | String
      | Unit
      | List
      | Option
      | Other

    let normalize_head environment type_expression =
      match environment with
      | None -> type_expression
      | Some environment -> (
          try Ctype.expand_head_opt environment type_expression
          with _ -> type_expression)

    let primitive ?environment type_expression =
      let type_expression = normalize_head environment type_expression in
      match Types.get_desc type_expression with
      | Types.Tconstr (path, _, _) when Path.same path Predef.path_bool -> Bool
      | Types.Tconstr (path, _, _) when Path.same path Predef.path_int -> Int
      | Types.Tconstr (path, _, _) when Path.same path Predef.path_float ->
          Float
      | Types.Tconstr (path, _, _) when Path.same path Predef.path_string ->
          String
      | Types.Tconstr (path, _, _) when Path.same path Predef.path_unit -> Unit
      | Types.Tconstr (path, _, _) when Path.same path Predef.path_list -> List
      | Types.Tconstr (path, _, _) when Path.same path Predef.path_option ->
          Option
      | _ -> Other

    let uid_is_stdlib uid =
      match uid with
      | Shape.Uid.Item { comp_unit; _ }
      | Shape.Uid.Local_opaque_item { comp_unit; _ } ->
          String.equal comp_unit "Stdlib"
      | Shape.Uid.Compilation_unit unit -> String.equal unit "Stdlib"
      | Shape.Uid.Predef _ -> true
      | Shape.Uid.Internal -> false

    let path_names_stdlib_value path name =
      match Path.flatten path with
      | `Contains_apply -> false
      | `Ok (head, [ field ]) ->
          Ident.persistent head
          && String.equal (Ident.name head) "Stdlib"
          && String.equal field name
      | `Ok (head, []) -> String.equal (Ident.name head) name
      | `Ok _ -> false

    let is_resolved_stdlib_value ~path ~description ~environment ~name =
      path_names_stdlib_value path name
      &&
      match Env.find_value path environment with
      | resolved ->
          Shape.Uid.equal description.Types.val_uid resolved.Types.val_uid
          && uid_is_stdlib resolved.val_uid
      | exception _ -> false

    let all ?environment primitive_type arguments =
      List.length arguments = 2
      && List.for_all
           (fun argument -> primitive ?environment argument = primitive_type)
           arguments

    let operator_application_is_typed_with environment ~token ~result_type
        ~argument_types =
      match token with
      | "&&" | "||" ->
          primitive ?environment result_type = Bool
          && all ?environment Bool argument_types
      | "+" | "-" | "*" | "/" ->
          primitive ?environment result_type = Int
          && all ?environment Int argument_types
      | "+." | "-." | "*." | "/." ->
          primitive ?environment result_type = Float
          && all ?environment Float argument_types
      | "=" | "<>" | "<" | "<=" | ">" | ">=" ->
          primitive ?environment result_type = Bool
          && List.length argument_types = 2
      | _ -> false

    let operator_application_is_typed ~token ~result_type ~argument_types =
      operator_application_is_typed_with None ~token ~result_type
        ~argument_types

    let operator_application_is_typed_in ~environment ~token ~result_type
        ~argument_types =
      operator_application_is_typed_with (Some environment) ~token ~result_type
        ~argument_types
  end

  type if_branch_target = Then_branch | Else_branch
  type boolean_literal_input = Boolean_literal_input of Typedtree.expression
  type condition_input = Condition_input of Typedtree.expression

  type binary_application_input =
    | Binary_application_input of Typedtree.expression

  type full_if_branch_input =
    | Full_if_branch_input of {
        conditional : Typedtree.expression;
        target : if_branch_target;
      }

  type sequence_expression_input =
    | Sequence_expression_input of Typedtree.expression

  type function_body_return_input =
    | Function_body_return_input of Typedtree.expression

  type _ visit_site =
    | Boolean_literal_site : boolean_literal_input visit_site
    | Condition_site : condition_input visit_site
    | Binary_application_site : binary_application_input visit_site
    | Full_if_branch_site : full_if_branch_input visit_site
    | Sequence_expression_site : sequence_expression_input visit_site
    | Function_body_return_site : function_body_return_input visit_site

  type rejection =
    | Source_bytes_mismatch of { expected : string; actual : string }

  type 'evidence applicability =
    | Applicable of 'evidence
    | Not_applicable
    | Rejected of rejection

  type compatibility =
    | Exact of { original : string; replacement : string }
    | Any
    | Replacement_is of string
    | Replacement_is_not of string

  type ('input, 'evidence) definition = {
    rule : rule;
    compatibility : compatibility;
    visit_site : 'input visit_site;
    applicable : source_bytes:string -> 'input -> 'evidence applicability;
    render : 'evidence -> Replacement_plan.t;
  }

  type packed = Pack : ('input, 'evidence) definition -> packed

  type evaluation =
    | Candidate of { rule : rule; plan : Replacement_plan.t }
    | Rejection of { rule : rule; reason : rejection }

  let rule (Pack definition) = definition.rule

  let visit_site_name (Pack definition) =
    match definition.visit_site with
    | Boolean_literal_site -> "boolean-literal"
    | Condition_site -> "condition"
    | Binary_application_site -> "binary-application"
    | Full_if_branch_site -> "full-if-branch-target"
    | Sequence_expression_site -> "sequence-expression"
    | Function_body_return_site -> "function-body-return"

  let rejection_name (Source_bytes_mismatch _) = "source-bytes-mismatch"

  let parse_expression source_bytes =
    let lexbuf = Lexing.from_string source_bytes in
    Lexing.set_filename lexbuf "<ocaml-mutants-operator-shadow>";
    try
      ignore (Parse.expression lexbuf);
      true
    with _ -> false

  let boolean_literal_name name =
    if String.equal name "true" || String.equal name "false" then Some name
    else None

  let source_boolean_literal source_bytes =
    let lexbuf = Lexing.from_string source_bytes in
    Lexing.set_filename lexbuf "<ocaml-mutants-boolean-shadow>";
    try
      match (Parse.expression lexbuf).Parsetree.pexp_desc with
      | Parsetree.Pexp_construct
          ({ Asttypes.txt = Longident.Lident name; _ }, None) ->
          boolean_literal_name name
      | _ -> None
    with _ -> None

  let typed_boolean_literal (expression : Typedtree.expression) =
    match expression.exp_desc with
    | Typedtree.Texp_construct (_, constructor, []) ->
        boolean_literal_name constructor.cstr_name
    | _ -> None

  let invalid_source ~expected actual =
    Rejected (Source_bytes_mismatch { expected; actual })

  let absolute_slice ~source_bytes (location : Warnings.loc) =
    let start_byte = location.loc_start.Lexing.pos_cnum in
    let end_byte = location.loc_end.Lexing.pos_cnum in
    if
      start_byte < 0
      || end_byte > String.length source_bytes
      || start_byte >= end_byte
    then None
    else Some (String.sub source_bytes start_byte (end_byte - start_byte))

  let boolean_literal_definition ~name ~original ~replacement =
    let rule =
      make_rule ~family:Family.Boolean_literal ~name ~version:1
        ~profile:Profile.Balanced
    in
    let compatibility = Exact { original; replacement } in
    let semantic_key_text =
      Printf.sprintf "boolean-literal:%s->%s" original replacement
    in
    let semantic_key =
      Replacement_plan.require_key ~context:(rule_stable_name rule)
        semantic_key_text
    in
    let static_plan =
      Replacement_plan.require_static
        ~context:("Boolean literal shadow spec " ^ rule_stable_name rule)
        ~source_bytes:original ~replacement_bytes:replacement
        ~semantic_key:semantic_key_text
    in
    let plan_for_source source_bytes =
      if source_boolean_literal source_bytes <> Some original then None
      else if String.equal source_bytes original then Some static_plan
      else
        Result.to_option
          (Replacement_plan.create_with_key ~source_bytes
             ~replacement_bytes:replacement ~key:semantic_key)
    in
    let applicable ~source_bytes (Boolean_literal_input expression) =
      match expression.Typedtree.exp_desc with
      | Typedtree.Texp_construct (_, constructor, [])
        when String.equal constructor.cstr_name original -> (
          match plan_for_source source_bytes with
          | Some plan -> Applicable plan
          | None ->
              Rejected
                (Source_bytes_mismatch
                   { expected = original; actual = source_bytes }))
      | _ -> Not_applicable
    in
    Pack
      {
        rule;
        compatibility;
        visit_site = Boolean_literal_site;
        applicable;
        render = Fun.id;
      }

  let boolean_literal_shadow_specs =
    [
      boolean_literal_definition ~name:"true-to-false" ~original:"true"
        ~replacement:"false";
      boolean_literal_definition ~name:"false-to-true" ~original:"false"
        ~replacement:"true";
    ]

  let longident_last = function
    | Longident.Lident name -> Some name
    | Longident.Ldot (_, name) -> Some name.Asttypes.txt
    | Longident.Lapply _ -> None

  let relative_slice ~source_bytes ~(outer_location : Warnings.loc)
      (inner_location : Warnings.loc) =
    let start_byte =
      inner_location.loc_start.Lexing.pos_cnum
      - outer_location.loc_start.Lexing.pos_cnum
    in
    let end_byte =
      inner_location.loc_end.Lexing.pos_cnum
      - outer_location.loc_start.Lexing.pos_cnum
    in
    if
      start_byte < 0
      || end_byte > String.length source_bytes
      || start_byte >= end_byte
    then None
    else Some (String.sub source_bytes start_byte (end_byte - start_byte))

  type binary_rendering =
    | Stdlib_call
    | Short_circuit_and_to_or
    | Short_circuit_or_to_and

  type binary_transition = {
    transition_rule : rule;
    original_operator : string;
    replacement_operator : string;
    rendering : binary_rendering;
    definition : packed;
  }

  let render_binary rendering ~replacement ~left ~right =
    match rendering with
    | Stdlib_call ->
        Printf.sprintf "(Stdlib.( %s )) (%s) (%s)" replacement left right
    | Short_circuit_and_to_or ->
        Printf.sprintf "(if (%s) then true else (%s))" left right
    | Short_circuit_or_to_and ->
        Printf.sprintf "(if (%s) then (%s) else false)" left right

  let binary_transition_definition ~family ~name ~original ~replacement
      ~rendering =
    let rule = make_rule ~family ~name ~version:1 ~profile:Profile.Balanced in
    let compatibility = Exact { original; replacement } in
    let semantic_key =
      Replacement_plan.require_key ~context:(rule_stable_name rule)
        (Printf.sprintf "%s:%s->%s" (Family.name family) original replacement)
    in
    let applicable ~source_bytes (Binary_application_input expression) =
      match expression.Typedtree.exp_desc with
      | Typedtree.Texp_apply
          ( {
              exp_desc = Typedtree.Texp_ident (path, operator_name, description);
              exp_loc = operator_location;
              _;
            },
            arguments )
        when longident_last operator_name.Asttypes.txt = Some original -> (
          let actual_arguments =
            List.filter_map
              (fun (_, argument) ->
                match argument with
                | Typedtree.Arg expression -> Some expression
                | Typedtree.Omitted () -> None)
              arguments
          in
          if List.length actual_arguments <> 2 then Not_applicable
          else if
            not
              (Typed_evidence.is_resolved_stdlib_value ~path ~description
                 ~environment:expression.exp_env ~name:original)
          then Not_applicable
          else if
            not
              (Typed_evidence.operator_application_is_typed_in
                 ~environment:expression.exp_env ~token:original
                 ~result_type:expression.exp_type
                 ~argument_types:
                   (List.map
                      (fun argument -> argument.Typedtree.exp_type)
                      actual_arguments))
          then Not_applicable
          else
            match
              ( relative_slice ~source_bytes ~outer_location:expression.exp_loc
                  operator_location,
                List.map
                  (fun argument ->
                    relative_slice ~source_bytes
                      ~outer_location:expression.exp_loc
                      argument.Typedtree.exp_loc)
                  actual_arguments )
            with
            | Some operator_bytes, [ Some left; Some right ]
              when String.equal (String.trim operator_bytes) original -> (
                let replacement_bytes =
                  render_binary rendering ~replacement ~left ~right
                in
                match
                  Replacement_plan.create_with_key ~source_bytes
                    ~replacement_bytes ~key:semantic_key
                with
                | Ok plan -> Applicable plan
                | Error message ->
                    Rejected
                      (Source_bytes_mismatch
                         {
                           expected =
                             "a non-empty, byte-exact binary application";
                           actual = message ^ ": " ^ source_bytes;
                         }))
            | Some operator_bytes, [ Some _; Some _ ] ->
                Rejected
                  (Source_bytes_mismatch
                     { expected = original; actual = operator_bytes })
            | _ ->
                Rejected
                  (Source_bytes_mismatch
                     {
                       expected =
                         "source bytes covering the typed operator and both \
                          operands";
                       actual = source_bytes;
                     }))
      | _ -> Not_applicable
    in
    let definition =
      Pack
        {
          rule;
          compatibility;
          visit_site = Binary_application_site;
          applicable;
          render = Fun.id;
        }
    in
    {
      transition_rule = rule;
      original_operator = original;
      replacement_operator = replacement;
      rendering;
      definition;
    }

  let uniform_binary_transition ~family ~name ~original ~replacement =
    binary_transition_definition ~family ~name ~original ~replacement
      ~rendering:Stdlib_call

  let boolean_connective_transitions =
    [
      binary_transition_definition ~family:Family.Boolean_connective
        ~name:"and-to-or" ~original:"&&" ~replacement:"||"
        ~rendering:Short_circuit_and_to_or;
      binary_transition_definition ~family:Family.Boolean_connective
        ~name:"or-to-and" ~original:"||" ~replacement:"&&"
        ~rendering:Short_circuit_or_to_and;
    ]

  let binary_operator_transitions =
    [
      uniform_binary_transition ~family:Family.Comparison ~name:"eq-to-neq"
        ~original:"=" ~replacement:"<>";
      uniform_binary_transition ~family:Family.Comparison ~name:"neq-to-eq"
        ~original:"<>" ~replacement:"=";
      uniform_binary_transition ~family:Family.Comparison ~name:"lt-to-le"
        ~original:"<" ~replacement:"<=";
      uniform_binary_transition ~family:Family.Comparison ~name:"le-to-lt"
        ~original:"<=" ~replacement:"<";
      uniform_binary_transition ~family:Family.Comparison ~name:"gt-to-ge"
        ~original:">" ~replacement:">=";
      uniform_binary_transition ~family:Family.Comparison ~name:"ge-to-gt"
        ~original:">=" ~replacement:">";
      uniform_binary_transition ~family:Family.Integer_arithmetic
        ~name:"int-add-to-sub" ~original:"+" ~replacement:"-";
      uniform_binary_transition ~family:Family.Integer_arithmetic
        ~name:"int-sub-to-add" ~original:"-" ~replacement:"+";
      uniform_binary_transition ~family:Family.Integer_arithmetic
        ~name:"int-mul-to-div" ~original:"*" ~replacement:"/";
      uniform_binary_transition ~family:Family.Integer_arithmetic
        ~name:"int-div-to-mul" ~original:"/" ~replacement:"*";
      uniform_binary_transition ~family:Family.Float_arithmetic
        ~name:"float-add-to-sub" ~original:"+." ~replacement:"-.";
      uniform_binary_transition ~family:Family.Float_arithmetic
        ~name:"float-sub-to-add" ~original:"-." ~replacement:"+.";
      uniform_binary_transition ~family:Family.Float_arithmetic
        ~name:"float-mul-to-div" ~original:"*." ~replacement:"/.";
      uniform_binary_transition ~family:Family.Float_arithmetic
        ~name:"float-div-to-mul" ~original:"/." ~replacement:"*.";
    ]

  let binary_transitions =
    boolean_connective_transitions @ binary_operator_transitions

  let boolean_connective_shadow_specs =
    List.map
      (fun transition -> transition.definition)
      boolean_connective_transitions

  let binary_operator_shadow_specs =
    List.map
      (fun transition -> transition.definition)
      binary_operator_transitions

  type condition_evidence = Boolean_condition of { plan : Replacement_plan.t }

  let condition_negation_definition =
    let rule =
      make_rule ~family:Family.Condition_negation ~name:"negate-condition"
        ~version:1 ~profile:Profile.Balanced
    in
    let compatibility = Any in
    let semantic_key =
      Replacement_plan.require_key ~context:(rule_stable_name rule)
        "wrapper:Stdlib.not"
    in
    let applicable ~source_bytes (Condition_input expression) =
      if
        Typed_evidence.primitive ~environment:expression.Typedtree.exp_env
          expression.exp_type
        <> Bool
      then Not_applicable
      else if not (parse_expression source_bytes) then
        invalid_source ~expected:"a byte-exact Boolean expression" source_bytes
      else
        let replacement_bytes = "Stdlib.not (" ^ source_bytes ^ ")" in
        match
          Replacement_plan.create_with_key ~source_bytes ~replacement_bytes
            ~key:semantic_key
        with
        | Ok plan -> Applicable (Boolean_condition { plan })
        | Error message ->
            invalid_source ~expected:"a byte-exact Boolean expression"
              (message ^ ": " ^ source_bytes)
    in
    let render (Boolean_condition { plan }) = plan in
    Pack
      { rule; compatibility; visit_site = Condition_site; applicable; render }

  let condition_negation_shadow_specs = [ condition_negation_definition ]

  type branch_evidence = Opposite_branch of { plan : Replacement_plan.t }

  type if_branch_entry = {
    target : if_branch_target;
    branch_definition : packed;
  }

  let if_branch_definition ~target ~rule_name ~compatibility =
    let rule =
      make_rule ~family:Family.If_branch ~name:rule_name ~version:1
        ~profile:Profile.Balanced
    in
    let semantic_key =
      Replacement_plan.require_key ~context:(rule_stable_name rule)
        "replacement:opposite-if-branch"
    in
    let applicable ~source_bytes (Full_if_branch_input input) =
      if input.target <> target then Not_applicable
      else
        match input.conditional.Typedtree.exp_desc with
        | Typedtree.Texp_ifthenelse (condition, then_branch, Some else_branch)
          -> (
            if
              Typed_evidence.primitive ~environment:condition.Typedtree.exp_env
                condition.exp_type
              <> Bool
            then Not_applicable
            else
              let replaced, retained =
                match target with
                | Then_branch -> (then_branch, else_branch)
                | Else_branch -> (else_branch, then_branch)
              in
              let owned =
                absolute_slice ~source_bytes replaced.Typedtree.exp_loc
              in
              let replacement =
                absolute_slice ~source_bytes retained.Typedtree.exp_loc
              in
              match (owned, replacement) with
              | Some owned, Some replacement
                when parse_expression owned && parse_expression replacement -> (
                  match
                    Replacement_plan.create_with_key ~source_bytes:owned
                      ~replacement_bytes:replacement ~key:semantic_key
                  with
                  | Ok plan -> Applicable (Opposite_branch { plan })
                  | Error message ->
                      invalid_source
                        ~expected:
                          "non-empty byte-exact expressions for both typed if \
                           branches"
                        (message ^ ": " ^ owned ^ " -> " ^ replacement))
              | Some owned, Some replacement ->
                  invalid_source
                    ~expected:
                      "byte-exact expressions for both typed if branches"
                    (owned ^ " -> " ^ replacement)
              | _ ->
                  invalid_source
                    ~expected:
                      "source bytes covering both typed if branch locations"
                    source_bytes)
        | _ -> Not_applicable
    in
    let render (Opposite_branch { plan }) = plan in
    let branch_definition =
      Pack
        {
          rule;
          compatibility;
          visit_site = Full_if_branch_site;
          applicable;
          render;
        }
    in
    { target; branch_definition }

  let if_branch_entries =
    [
      if_branch_definition ~target:Else_branch ~rule_name:"select-then-branch"
        ~compatibility:(Replacement_is "()");
      if_branch_definition ~target:Then_branch ~rule_name:"select-else-branch"
        ~compatibility:(Replacement_is_not "()");
    ]

  let if_branch_shadow_specs =
    List.map (fun entry -> entry.branch_definition) if_branch_entries

  type sequence_evidence = Sequence_right of { plan : Replacement_plan.t }

  let sequence_deletion_definition =
    let rule =
      make_rule ~family:Family.Sequence_deletion ~name:"delete-left-sequence"
        ~version:1 ~profile:Profile.Balanced
    in
    let compatibility = Any in
    let semantic_key =
      Replacement_plan.require_key ~context:(rule_stable_name rule)
        "replacement:sequence-right"
    in
    let applicable ~source_bytes (Sequence_expression_input expression) =
      match expression.Typedtree.exp_desc with
      | Typedtree.Texp_sequence (_, right) -> (
          match
            relative_slice ~source_bytes ~outer_location:expression.exp_loc
              right.Typedtree.exp_loc
          with
          | Some right_bytes
            when parse_expression source_bytes && parse_expression right_bytes
            -> (
              match
                Replacement_plan.create_with_key ~source_bytes
                  ~replacement_bytes:right_bytes ~key:semantic_key
              with
              | Ok plan -> Applicable (Sequence_right { plan })
              | Error message ->
                  invalid_source
                    ~expected:
                      "a non-empty byte-exact typed sequence and right \
                       expression"
                    (message ^ ": " ^ source_bytes ^ " -> " ^ right_bytes))
          | Some right_bytes ->
              invalid_source
                ~expected:"a byte-exact typed sequence and right expression"
                (source_bytes ^ " -> " ^ right_bytes)
          | None ->
              invalid_source
                ~expected:
                  "source bytes covering the typed sequence and right operand"
                source_bytes)
      | _ -> Not_applicable
    in
    let render (Sequence_right { plan }) = plan in
    Pack
      {
        rule;
        compatibility;
        visit_site = Sequence_expression_site;
        applicable;
        render;
      }

  let sequence_deletion_shadow_specs = [ sequence_deletion_definition ]

  type return_evidence = Neutral_return of { plan : Replacement_plan.t }

  type return_entry = {
    primitive : Typed_evidence.primitive;
    neutral_replacement : string;
    return_definition : packed;
  }

  let return_definition ~name ~primitive ~replacement =
    let rule =
      make_rule ~family:Family.Return_replacement ~name ~version:1
        ~profile:Profile.Balanced
    in
    let compatibility = Replacement_is replacement in
    let semantic_key =
      Replacement_plan.require_key ~context:(rule_stable_name rule)
        ("replacement:" ^ replacement)
    in
    let applicable ~source_bytes (Function_body_return_input expression) =
      match expression.Typedtree.exp_desc with
      | Typedtree.Texp_unreachable -> Not_applicable
      | _ -> (
          if
            Typed_evidence.primitive ~environment:expression.Typedtree.exp_env
              expression.exp_type
            <> primitive
          then Not_applicable
          else if
            String.equal source_bytes replacement
            ||
            match
              ( typed_boolean_literal expression,
                source_boolean_literal source_bytes )
            with
            | Some typed, Some source ->
                String.equal typed source && String.equal source replacement
            | _ -> false
          then Not_applicable
          else if not (parse_expression source_bytes) then
            invalid_source ~expected:"a byte-exact typed return expression"
              source_bytes
          else
            match
              Replacement_plan.create_with_key ~source_bytes
                ~replacement_bytes:replacement ~key:semantic_key
            with
            | Ok plan -> Applicable (Neutral_return { plan })
            | Error message ->
                invalid_source ~expected:"a non-empty byte-exact typed return"
                  (message ^ ": " ^ source_bytes))
    in
    let render (Neutral_return { plan }) = plan in
    let return_definition =
      Pack
        {
          rule;
          compatibility;
          visit_site = Function_body_return_site;
          applicable;
          render;
        }
    in
    { primitive; neutral_replacement = replacement; return_definition }

  let return_replacement_entries =
    [
      return_definition ~name:"return-unit" ~primitive:Unit ~replacement:"()";
      return_definition ~name:"return-false" ~primitive:Bool
        ~replacement:"false";
      return_definition ~name:"return-true" ~primitive:Bool ~replacement:"true";
      return_definition ~name:"return-zero" ~primitive:Int ~replacement:"0";
      return_definition ~name:"return-float-zero" ~primitive:Float
        ~replacement:"0.0";
      return_definition ~name:"return-empty-string" ~primitive:String
        ~replacement:"\"\"";
      return_definition ~name:"return-empty-list" ~primitive:List
        ~replacement:"[]";
      return_definition ~name:"return-none" ~primitive:Option
        ~replacement:"None";
    ]

  let return_replacement_shadow_specs =
    List.map (fun entry -> entry.return_definition) return_replacement_entries

  let neutral_return_replacements ?environment type_expression =
    let primitive = Typed_evidence.primitive ?environment type_expression in
    return_replacement_entries
    |> List.filter_map (fun entry ->
        if entry.primitive = primitive then Some entry.neutral_replacement
        else None)

  let registry =
    [
      boolean_literal_shadow_specs;
      condition_negation_shadow_specs;
      boolean_connective_shadow_specs;
      binary_operator_shadow_specs;
      if_branch_shadow_specs;
      sequence_deletion_shadow_specs;
      return_replacement_shadow_specs;
    ]
    |> List.concat

  let registry_rules = List.map rule registry

  (* Exact transformation duplicates retain the first family in this declared
     precedence. Keeping the policy beside the only rule registry prevents the
     frontend traversal from becoming an independent source of operator
     dominance. *)
  let deduplication_precedence =
    [
      Family.Boolean_literal;
      Family.Boolean_connective;
      Family.Comparison;
      Family.Integer_arithmetic;
      Family.Float_arithmetic;
      Family.Condition_negation;
      Family.If_branch;
      Family.Sequence_deletion;
      Family.Return_replacement;
    ]

  let compare_deduplication_precedence left right =
    if left = right then 0
    else
      let rec compare = function
        | [] ->
            invalid_arg
              "Operator.Spec.compare_deduplication_precedence: unregistered \
               family"
        | family :: _ when family = left -> -1
        | family :: _ when family = right -> 1
        | _ :: rest -> compare rest
      in
      compare deduplication_precedence

  let transition_matches transition ~original ~replacement =
    match transition with
    | Exact expected ->
        String.equal expected.original original
        && String.equal expected.replacement replacement
    | Any -> true
    | Replacement_is expected -> String.equal expected replacement
    | Replacement_is_not excluded -> not (String.equal excluded replacement)

  let compatible_rules family ~original ~replacement =
    let original = String.trim original in
    let replacement = String.trim replacement in
    registry
    |> List.filter_map (fun (Pack definition) ->
        if
          rule_family definition.rule = family
          && transition_matches definition.compatibility ~original ~replacement
        then Some definition.rule
        else None)

  let transitions_overlap left right =
    match (left, right) with
    | Any, _ | _, Any -> true
    | Exact exact, transition | transition, Exact exact ->
        transition_matches transition ~original:exact.original
          ~replacement:exact.replacement
    | Replacement_is left, Replacement_is right -> String.equal left right
    | Replacement_is value, Replacement_is_not excluded
    | Replacement_is_not excluded, Replacement_is value ->
        not (String.equal value excluded)
    | Replacement_is_not _, Replacement_is_not _ -> true

  let binary_original_invariant_errors originals =
    let seen = Hashtbl.create (List.length originals) in
    List.fold_left
      (fun errors original ->
        if Hashtbl.mem seen original then
          Printf.sprintf "duplicate binary original operator %S" original
          :: errors
        else (
          Hashtbl.add seen original ();
          errors))
      [] originals
    |> List.rev

  let if_branch_target_name = function
    | Then_branch -> "then"
    | Else_branch -> "else"

  let if_branch_target_invariant_errors targets =
    [ Then_branch; Else_branch ]
    |> List.filter_map (fun target ->
        let occurrences =
          List.fold_left
            (fun count candidate ->
              if candidate = target then count + 1 else count)
            0 targets
        in
        if occurrences = 1 then None
        else
          Some
            (Printf.sprintf "if-branch target %s has %d registry entries"
               (if_branch_target_name target)
               occurrences))

  let registry_errors () =
    let errors = ref [] in
    let seen = Hashtbl.create (List.length registry) in
    errors :=
      List.rev_append
        (binary_original_invariant_errors
           (List.map
              (fun transition -> transition.original_operator)
              binary_transitions))
        !errors;
    errors :=
      List.rev_append
        (if_branch_target_invariant_errors
           (List.map (fun entry -> entry.target) if_branch_entries))
        !errors;
    List.iter
      (fun family ->
        let occurrences =
          List.fold_left
            (fun count candidate ->
              if candidate = family then count + 1 else count)
            0 deduplication_precedence
        in
        if occurrences <> 1 then
          errors :=
            Printf.sprintf
              "deduplication precedence contains %s family %d times"
              (Family.name family) occurrences
            :: !errors)
      Family.all;
    List.iter
      (fun (Pack definition) ->
        let stable = rule_stable_name definition.rule in
        if Hashtbl.mem seen stable then
          errors := ("duplicate stable rule name " ^ stable) :: !errors
        else Hashtbl.add seen stable ();
        if rule_version definition.rule <= 0 then
          errors := ("non-positive rule version " ^ stable) :: !errors;
        if not (List.mem (rule_profile definition.rule) Profile.all) then
          errors := ("unknown rule profile " ^ stable) :: !errors)
      registry;
    List.iter
      (fun (Pack left) ->
        List.iter
          (fun (Pack right) ->
            if
              compare_rule left.rule right.rule < 0
              && rule_family left.rule = rule_family right.rule
              && transitions_overlap left.compatibility right.compatibility
            then
              errors :=
                Printf.sprintf "ambiguous compatibility transitions: %s and %s"
                  (rule_stable_name left.rule)
                  (rule_stable_name right.rule)
                :: !errors)
          registry)
      registry;
    List.rev !errors

  let compatibility_example = function
    | Exact { original; replacement } -> (original, replacement)
    | Any -> ("original", "replacement")
    | Replacement_is replacement -> ("original", replacement)
    | Replacement_is_not excluded -> ("original", excluded ^ "-other")

  let compatibility_examples () =
    List.map
      (fun (Pack definition) ->
        let original, replacement =
          compatibility_example definition.compatibility
        in
        (definition.rule, original, replacement))
      registry

  let () =
    match registry_errors () with
    | [] -> ()
    | errors ->
        invalid_arg
          ("invalid built-in operator registry: " ^ String.concat "; " errors)

  type (_, _) type_equality = Equal : ('value, 'value) type_equality

  let equal_visit_site : type left right.
      left visit_site -> right visit_site -> (left, right) type_equality option
      =
   fun left right ->
    match (left, right) with
    | Boolean_literal_site, Boolean_literal_site -> Some Equal
    | Condition_site, Condition_site -> Some Equal
    | Binary_application_site, Binary_application_site -> Some Equal
    | Full_if_branch_site, Full_if_branch_site -> Some Equal
    | Sequence_expression_site, Sequence_expression_site -> Some Equal
    | Function_body_return_site, Function_body_return_site -> Some Equal
    | _ -> None

  let evaluate_at_visit_site : type input.
      input visit_site ->
      definitions:packed list ->
      source_bytes:string ->
      input ->
      evaluation list =
   fun visit_site ~definitions ~source_bytes input ->
    List.filter_map
      (fun (Pack definition) ->
        match equal_visit_site visit_site definition.visit_site with
        | Some Equal -> (
            match definition.applicable ~source_bytes input with
            | Applicable evidence ->
                Some
                  (Candidate
                     {
                       rule = definition.rule;
                       plan = definition.render evidence;
                     })
            | Not_applicable -> None
            | Rejected reason ->
                Some (Rejection { rule = definition.rule; reason }))
        | None -> None)
      definitions

  let evaluate_boolean_literal_at_site ~definitions ~source_bytes expression =
    evaluate_at_visit_site Boolean_literal_site ~definitions ~source_bytes
      (Boolean_literal_input expression)

  let evaluate_condition_at_site ~definitions ~source_bytes expression =
    evaluate_at_visit_site Condition_site ~definitions ~source_bytes
      (Condition_input expression)

  let evaluate_binary_application_at_site ~definitions ~source_bytes expression
      =
    evaluate_at_visit_site Binary_application_site ~definitions ~source_bytes
      (Binary_application_input expression)

  let evaluate_full_if_branch_at_site ~definitions ~source_bytes ~target
      conditional =
    evaluate_at_visit_site Full_if_branch_site ~definitions ~source_bytes
      (Full_if_branch_input { conditional; target })

  let evaluate_sequence_expression_at_site ~definitions ~source_bytes expression
      =
    evaluate_at_visit_site Sequence_expression_site ~definitions ~source_bytes
      (Sequence_expression_input expression)

  let evaluate_function_body_return_at_site ~definitions ~source_bytes
      expression =
    evaluate_at_visit_site Function_body_return_site ~definitions ~source_bytes
      (Function_body_return_input expression)

  let evaluate_boolean_literal ~source_bytes expression =
    evaluate_boolean_literal_at_site ~definitions:boolean_literal_shadow_specs
      ~source_bytes expression

  let evaluate_boolean_connective ~source_bytes expression =
    evaluate_binary_application_at_site
      ~definitions:boolean_connective_shadow_specs ~source_bytes expression

  let evaluate_binary_operator ~source_bytes expression =
    evaluate_binary_application_at_site
      ~definitions:binary_operator_shadow_specs ~source_bytes expression

  let evaluate_condition_negation ~source_bytes expression =
    evaluate_condition_at_site ~definitions:condition_negation_shadow_specs
      ~source_bytes expression

  let evaluate_if_branch ~source_bytes ~target conditional =
    evaluate_full_if_branch_at_site ~definitions:if_branch_shadow_specs
      ~source_bytes ~target conditional

  let evaluate_sequence_deletion ~source_bytes expression =
    evaluate_sequence_expression_at_site
      ~definitions:sequence_deletion_shadow_specs ~source_bytes expression

  let evaluate_return_replacement ~source_bytes expression =
    evaluate_function_body_return_at_site
      ~definitions:return_replacement_shadow_specs ~source_bytes expression

  module For_testing = struct
    let all_specs () = registry
    let boolean_literal_specs () = boolean_literal_shadow_specs
    let boolean_connective_specs () = boolean_connective_shadow_specs
    let binary_operator_specs () = binary_operator_shadow_specs
    let condition_negation_specs () = condition_negation_shadow_specs
    let if_branch_specs () = if_branch_shadow_specs
    let sequence_deletion_specs () = sequence_deletion_shadow_specs
    let return_replacement_specs () = return_replacement_shadow_specs
    let visit_site_name = visit_site_name
    let binary_original_invariant_errors = binary_original_invariant_errors
    let if_branch_target_invariant_errors = if_branch_target_invariant_errors
    let evaluate_boolean_literal_at_site = evaluate_boolean_literal_at_site
    let evaluate_condition_at_site = evaluate_condition_at_site

    let evaluate_binary_application_at_site =
      evaluate_binary_application_at_site

    let evaluate_full_if_branch_at_site = evaluate_full_if_branch_at_site

    let evaluate_sequence_expression_at_site =
      evaluate_sequence_expression_at_site

    let evaluate_function_body_return_at_site =
      evaluate_function_body_return_at_site

    let evaluate_expression = evaluate_boolean_literal_at_site
  end
end

module Rule = struct
  type t = rule

  let family = rule_family
  let name = rule_name
  let version = rule_version
  let profile = rule_profile
  let stable_name = rule_stable_name
  let all = Spec.registry_rules

  let of_stable_name value =
    match
      List.find_opt (fun rule -> String.equal (stable_name rule) value) all
    with
    | Some rule -> Ok rule
    | None -> Error (Printf.sprintf "unknown mutation rule %S" value)

  let compare = compare_rule
  let equal = equal_rule
  let pp formatter rule = Format.pp_print_string formatter (stable_name rule)
  let compatible_rules = Spec.compatible_rules

  module For_testing = struct
    let registry_errors = Spec.registry_errors
    let compatibility_examples = Spec.compatibility_examples
  end
end

let all = Family.all
let name = Family.name
let of_string = Family.of_string
let pp = Family.pp
let rules family = List.filter (fun rule -> Rule.family rule = family) Rule.all

let rule_for_replacement family ~original ~replacement =
  match Rule.compatible_rules family ~original ~replacement with
  | [] ->
      Error
        (Printf.sprintf "no concrete rule for %s: %S -> %S" (Family.name family)
           original replacement)
  | [ rule ] -> Ok rule
  | rules ->
      Error
        (Printf.sprintf "ambiguous concrete rules for %s: %S -> %S (%s)"
           (Family.name family) original replacement
           (String.concat ", " (List.map Rule.stable_name rules)))
