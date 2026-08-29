type format = Terminal | Native_json | Html | Markdown | Sarif | Stryker

val of_string : string -> (format, string) result
val name : format -> string
val extension : format -> string

val render :
  root:string ->
  color:bool ->
  ?stryker_thresholds:Stryker_report.thresholds ->
  format ->
  Run_store.run ->
  (string, Error.t) result
