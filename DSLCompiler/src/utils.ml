open Core

let read_file filename =
	In_channel.with_file filename ~f:In_channel.input_all

let write_file filename content =
	Out_channel.with_file filename ~f:(fun oc ->
		Out_channel.output_string oc content
	)


let rec unique_merge_lists eq (l1: 'a list) (l2: 'a list): 'a list =
	match l1 with
	| [] -> l2
	| l :: ls ->
			if List.mem l2 l ~equal:eq then
				unique_merge_lists eq ls l2
			else
				l :: (unique_merge_lists eq ls l2)
