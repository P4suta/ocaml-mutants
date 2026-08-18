let of_summary summary =
  if Summary.error summary > 0 || Summary.inconclusive summary > 0 then 2
  else if Summary.survived summary > 0 then 1
  else 0
