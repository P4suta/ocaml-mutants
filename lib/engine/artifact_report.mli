type format =
  | Terminal
  | Native_json
  | Html
  | Markdown
  | Sarif
  | Stryker of Stryker_report.thresholds

val of_string :
  ?stryker_thresholds:Stryker_report.thresholds ->
  string ->
  (format, string) result

val name : format -> string
val extension : format -> string

val render :
  root:string ->
  color:bool ->
  format ->
  Run_store.run ->
  (string, Error.t) result
