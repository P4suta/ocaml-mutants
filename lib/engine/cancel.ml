type t = bool Atomic.t

let create () = Atomic.make false
let request cancel = Atomic.set cancel true
let is_requested cancel = Atomic.get cancel
