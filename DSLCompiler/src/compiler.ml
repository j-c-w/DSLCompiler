open Cmdliner
open Options
open Utils

let run_clang_tidy file =
	let command = "clang-format -i -style=llvm " ^ file in
	let _ = Sys.command command in
	()

(* Main entry point post argument processing.  *)
let main options =
    let file = options.input_file in
    let contents = read_file file in
    let parsed = Frontend.do_frontend options contents in
    let lowered = Midend.lower options parsed in
    let target = Backend.lower options lowered in
    let _ = write_file options.output_file target in
	(* format the generated C code so it is more readable.  *)
	let _ = run_clang_tidy options.output_file in
    ()

(* Handle the argument processing *)

(* Main input file --- in position 0 in the argument chain *)
let input_file =
	let doc = "The input file" in
	Arg.(required & pos 0 (some string) None & info [] ~docv:"File" ~doc)

(* Turn the arguments into an options type *)
let process_arguments input_file debug_generate_code output_file =
    let options: Options.options = {
        input_file;
        output_file;

        debug_generate_code;
    } in
    main options

(* Have a defualt output file, but allow that to be overwritten.  *)
let output_file =
    let doc = "Output fle" in
    Arg.(value & opt string "out.c" & info ["o"; "output"] ~docv:"OutputFile" ~doc)

(* Define the flags we want.  *)
let debug_generate_code =
    let doc = "Debug the generate code pass. " in
    Arg.(value & flag & info ["debug-generate-code"] ~doc)

let cmd =
  let doc = "A simple compiler" in
  let man = [
    `S Manpage.s_description;
    `P "This is a simple DSL compiler";
  ] in
  (* All the arguments have to be listed here *)
  let term = Term.(const process_arguments $ input_file $ debug_generate_code $ output_file) in
  Cmd.v (Cmd.info "DSL Compiler" ~doc ~man) term

(* This is the entry-point into CmdLiner --- it will do the argument
processing, then call process_arguments, which puts all the arguments
into an Options.options struct and calls the main function above.  *)
let () = Stdlib.exit (Cmd.eval cmd)
