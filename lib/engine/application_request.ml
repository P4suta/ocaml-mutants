type output =
  | Terminal of { quiet : bool; color : bool }
  | Json
  | Stryker_json of Stryker_report.thresholds

type shard_selection = {
  plan_id : string;
  input_fingerprint : string;
  index : int;
  count : int;
  mutant_ids : string list;
}

type selection =
  | All
  | Changed
  | Changed_from of string
  | Mutants of string list
  | Shard of shard_selection
  | Rerun of { parent_run_id : string; mutant_id : string }
