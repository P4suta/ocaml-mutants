module Engine = Ocaml_mutants_engine

(** Build the resolved configuration and its matching canonical digest together,
    so report fixtures cannot accidentally pair evidence from different encoders
    or configurations. *)
let config_evidence ?(encode = Engine.Config.to_yojson) config =
  let resolved_config = encode config in
  let config_digest =
    Engine.Util.sha256 (Yojson.Safe.to_string ~std:true resolved_config)
  in
  (resolved_config, config_digest)
