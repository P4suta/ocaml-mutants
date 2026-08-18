val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
val read_file : string -> (string, string) result
val write_file : string -> string -> (unit, string) result
val atomic_write : string -> string -> (unit, string) result
val mkdir_p : string -> (unit, string) result
val remove_tree : string -> (unit, string) result
val files_recursive : ?skip:(string -> bool) -> string -> string list
val normalize_relative : root:string -> string -> string
val sha256 : string -> string
val digest_file : string -> (string, string) result
val digest_tree : ?skip:(string -> bool) -> string -> (string, string) result
val timestamp : unit -> string
val split_lines : string -> string list
val string_starts_with : prefix:string -> string -> bool
val string_ends_with : suffix:string -> string -> bool
