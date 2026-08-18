module Typed_evidence = Ocaml_mutants_core.Operator.Spec.Typed_evidence

type primitive = Typed_evidence.primitive =
  | Bool
  | Int
  | Float
  | String
  | Unit
  | List
  | Option
  | Other

let primitive = Typed_evidence.primitive
let is_resolved_stdlib_value = Typed_evidence.is_resolved_stdlib_value
let operator_application_is_typed = Typed_evidence.operator_application_is_typed

let operator_application_is_typed_in =
  Typed_evidence.operator_application_is_typed_in

let neutral_replacements type_expression =
  Ocaml_mutants_core.Operator.Spec.neutral_return_replacements type_expression

let neutral_replacements_in ~environment type_expression =
  Ocaml_mutants_core.Operator.Spec.neutral_return_replacements ~environment
    type_expression
