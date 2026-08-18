type output =
  | Terminal of { quiet : bool; color : bool }
  | Json
  | Stryker_json of Stryker_report.thresholds

type selection =
  | All
  | Changed
  | Changed_from of string
  | Mutants of string list
