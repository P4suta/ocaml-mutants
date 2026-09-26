_ocaml_mutants_complete() {
  local commands="run check report plan merge list ui doctor config cache mutant"
  local config="init show check migrate"
  local cache="stats gc clean"
  local mutant="show rerun apply revert expect"
  if [[ ${COMP_CWORD} -eq 1 ]]; then
    COMPREPLY=( $(compgen -W "$commands" -- "${COMP_WORDS[COMP_CWORD]}") )
  elif [[ ${COMP_WORDS[1]} == config ]]; then
    COMPREPLY=( $(compgen -W "$config" -- "${COMP_WORDS[COMP_CWORD]}") )
  elif [[ ${COMP_WORDS[1]} == cache ]]; then
    COMPREPLY=( $(compgen -W "$cache" -- "${COMP_WORDS[COMP_CWORD]}") )
  elif [[ ${COMP_WORDS[1]} == mutant ]]; then
    COMPREPLY=( $(compgen -W "$mutant" -- "${COMP_WORDS[COMP_CWORD]}") )
  fi
}
complete -F _ocaml_mutants_complete ocaml-mutants
