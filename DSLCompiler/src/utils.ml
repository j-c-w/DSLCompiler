let read_file filename =
    let ic = open_in filename in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    s

let write_file filename content =
  let oc = open_out filename in
  output_string oc content;
  close_out oc
