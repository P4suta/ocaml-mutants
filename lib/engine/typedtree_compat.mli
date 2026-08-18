type primitive = Bool | Int | Float | String | Unit | List | Option | Other

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

val neutral_replacements : Types.type_expr -> string list

val neutral_replacements_in :
  environment:Env.t -> Types.type_expr -> string list
