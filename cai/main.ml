open Frontc
open Cil
open Printf

	
let read_proc_name_from_file (fname : string)
:
  unit
=
	let ch = open_in fname in
  let rexp = Str.regexp "[ :,;]+" in
	let rec read_options ch =
  (try
    let lstr = input_line ch in
    let kv = Str.split rexp lstr in
		let tail = List.tl kv in
		Cai_options.int_proc_name := List.append !Cai_options.int_proc_name tail;
    read_options ch
  with End_of_file -> ())
  in
	read_options ch


let parse_commands_line 
=
	Array.iter (fun x -> Cai_options.feedback 1 "%s====" x) Sys.argv; 
	Cai_options.feedback 1 "\n";
	
	(*parse the command line args*)
	Arg.parse_argv Sys.argv Cai_options.speclist (fun s ->
		 Cai_inliner.filenamelist := s::!Cai_inliner.filenamelist) "";
	(*get the path prefix used for calling cil driver*)
	let bin_name_begin = String.rindex Sys.argv.(0) '/' + 1 in
	let prefix = String.sub Sys.argv.(0) 0 bin_name_begin in
	Cai_options.exec_path_prefix := prefix;
	if !Cai_options.int_config_fname <> "" then
		(read_proc_name_from_file !Cai_options.int_config_fname;
		Cai_options.main_entry := List.hd !Cai_options.int_proc_name);
	()

let safe_remove fname =
  try Unix.unlink fname with Unix.Unix_error _ -> ()

		
		
let gettime () : float =
  (Unix.times ()).Unix.tms_utime
		
let () =
	(* print the program's code after cil compute the cfg *)
	let start_time = gettime() in
	Cil.useLogicalOperators := true;
	Cai_inliner.merge_and_inline ();
	()
	