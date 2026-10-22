let candidate left right = left + right
let never_called value = value + 1

let () =
  if Sys.getenv_opt "OCAML_MUTANTS_ACTIVE" <> None then (
    print_endline "privacy-secret";
    prerr_endline "privacy-secret");
  if candidate 2 2 <> 4 then exit 1
