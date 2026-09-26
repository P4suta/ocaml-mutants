Register-ArgumentCompleter -Native -CommandName ocaml-mutants -ScriptBlock {
  param($wordToComplete, $commandAst, $cursorPosition)
  $words = $commandAst.CommandElements | ForEach-Object { $_.Extent.Text }
  $candidates = if ($words.Count -le 2) {
    'run','check','report','plan','merge','list','ui','doctor','config','cache','mutant'
  } elseif ($words[1] -eq 'config') {
    'init','show','check','migrate'
  } elseif ($words[1] -eq 'cache') {
    'stats','gc','clean'
  } elseif ($words[1] -eq 'mutant') {
    'show','rerun','apply','revert','expect'
  } else { @() }
  $candidates | Where-Object { $_ -like "$wordToComplete*" } |
    ForEach-Object { [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', $_) }
}
