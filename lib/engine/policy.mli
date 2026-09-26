type finding_kind = Policy_violation | Evidence_refusal

type finding = {
  code : string;
  kind : finding_kind;
  message : string;
  actual : string option;
  required : string option;
}

type verdict = Passed | Violated | Refused

type evaluation = {
  run_id : string;
  verdict : verdict;
  exit_code : int;
  findings : finding list;
  summary : Run_store.summary;
  evidence_level : Run_store.evidence_level;
  complete : bool;
  policy : Config.policy;
  reference_score : float option;
}

val evaluate :
  policy:Config.policy -> ?reference_score:float -> Run_store.run -> evaluation

val verdict_name : verdict -> string
val finding_kind_name : finding_kind -> string
val to_yojson : evaluation -> Yojson.Safe.t
val to_string : evaluation -> string
val pp : Format.formatter -> evaluation -> unit
