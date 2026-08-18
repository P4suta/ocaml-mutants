type t

type view = {
  total : int;
  killed : int;
  survived : int;
  timeout : int;
  inconclusive : int;
  error : int;
}

val of_results : Run_results.complete Run_results.t -> t
val view : t -> view
val total : t -> int
val killed : t -> int
val survived : t -> int
val timeout : t -> int
val inconclusive : t -> int
val error : t -> int
