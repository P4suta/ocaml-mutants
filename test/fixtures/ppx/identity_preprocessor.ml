let () =
  if Array.length Sys.argv <> 2 then invalid_arg "expected one input file";
  set_binary_mode_out stdout true;
  let channel = open_in_bin Sys.argv.(1) in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let length = in_channel_length channel in
      output_string stdout (really_input_string channel length))
