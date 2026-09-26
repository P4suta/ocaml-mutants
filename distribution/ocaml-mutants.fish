complete -c ocaml-mutants -f
for command in run check report plan merge list ui doctor config cache mutant
    complete -c ocaml-mutants -n "not __fish_seen_subcommand_from run check report plan merge list ui doctor config cache mutant" -a $command
end
complete -c ocaml-mutants -n "__fish_seen_subcommand_from config" -a "init show check migrate"
complete -c ocaml-mutants -n "__fish_seen_subcommand_from cache" -a "stats gc clean"
complete -c ocaml-mutants -n "__fish_seen_subcommand_from mutant" -a "show rerun apply revert expect"
