(**  Argument, Options and Parsing of command line} *)

open Format

(** input filename *)
let inputfilename = ref ""


let curdir = ref false

(** debug level *)
let debug = ref 0

		
	
let feedback level =
	(
		if level <= !debug then
				(fprintf std_formatter "[cai] "; fprintf std_formatter)
		else
			(ifprintf err_formatter "[cai]"; ifprintf err_formatter)
	)

let main_entry = ref "main"

(** check options of CAI *)

(** detail options of CAI **)
let curdir = ref false
let save_tmps = ref false
let inline = ref false
let dosimpleMem = ref false
let dosimplify = ref false
let exec_path_prefix = ref ""
let int_config_fname = ref ""
let int_proc_name = ref []
(* ---------------------------------------------------------------------- *)
(** {7 Main specification list} *)
(*  ---------------------------------------------------------------------- *)

let (speclist: (Arg.key * Arg.spec * Arg.doc) list) =
	Arg.align
		[
		(
			"-int-config-file",
			Arg.Set_string(int_config_fname),
			" : read interrupt configuration from file [fname] "
		);
		(
			"-inline",
			Arg.Unit(begin fun () -> inline := true end),
			" : inline all the functions, not recommendate when the program is large "
		);
		(
			"-dosimpleMem",
			Arg.Unit(begin fun () -> dosimpleMem := true end),
			" : Enable simplify all memory expressions "
		);
		(
			"-dosimplify",
			Arg.Unit(begin fun () -> dosimplify := true end),
			" : Enable compiles C to 3-address code "
		);
		(
			"-save-tmps",
			Arg.Unit(begin fun () -> save_tmps := true end),
			" : save the intermediate files such as preprocess files, merge files and so on "
		);
		(
			"-curdir",
			Arg.Unit(begin fun () -> curdir := true end),
			" : whether use current dir to find source file "
		);
		(
			"-debug",
			Arg.Set_int(debug),
			"<int> : debug level, from 0 to 4 (default: 0)"
		);
		(
			"-main",
			Arg.Set_string(main_entry),
			"<fun> : entry point function for the analysis (default: main)"
		)
		]
