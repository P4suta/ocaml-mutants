type view = {
  total : int;
  killed : int;
  survived : int;
  timeout : int;
  inconclusive : int;
  error : int;
}

type t = view

let of_results results =
  List.fold_left
    (fun summary (_, outcome) ->
      match outcome with
      | Outcome.Killed -> { summary with killed = summary.killed + 1 }
      | Outcome.Survived -> { summary with survived = summary.survived + 1 }
      | Outcome.Timeout -> { summary with timeout = summary.timeout + 1 }
      | Outcome.Inconclusive _ ->
          { summary with inconclusive = summary.inconclusive + 1 }
      | Outcome.Error _ -> { summary with error = summary.error + 1 })
    {
      total = List.length (Run_results.to_list results);
      killed = 0;
      survived = 0;
      timeout = 0;
      inconclusive = 0;
      error = 0;
    }
    (Run_results.to_list results)

let view summary = summary
let total summary = summary.total
let killed summary = summary.killed
let survived summary = summary.survived
let timeout summary = summary.timeout
let inconclusive summary = summary.inconclusive
let error summary = summary.error
