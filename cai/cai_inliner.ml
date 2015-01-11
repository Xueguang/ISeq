open Cil

let filenamelist : string list ref = ref []
let singleton_filenames = ref ""
(*gcc -D_GNUCC -E -DCIL=1 filelist > __cai_pp_tmp.c*)
let gcc_preprocess_cmd = ref "gcc -D_GNUCC -E -DCIL=1 "
(*FIXME this need to make some progresses*)
let cilly_driver = ref "cilly.asm.exe --out " 
let tmp_pp_filename = ref "__cai_pp_tmp.c"
let tmp_merge_fname = ref "__cai_merge_tmp.c"
let tmp_inline_fname = ref "__cai_inline_tmp.c"
let tmp_files_fname = ref "__cai_files_tmp.txt"
let find_cfiles_cmd = ref ("find ! -name '__cai_*' -name "^"'*.c' > " ^ !tmp_files_fname)
let find_dirs_cmd = ref ("find -xtype d > " ^ !tmp_files_fname)
let include_path = ref ""


let safe_remove fname =
	try Unix.unlink fname with Unix.Unix_error _ -> ()

let read_file_with_prefix
		(fname : string)
		(prefix : string)
		(res : string ref)
:
unit
=
	let rec read_file ch =
		try
			let lstr = input_line ch in
			res := prefix ^ lstr ^ " " ^ !res ;
			read_file ch
		with End_of_file -> ()
	in
	begin
		let ch = open_in fname in
		begin
			Cai_options.feedback 0 "Read directories and files name from file...\n";
			read_file ch;
			close_in ch;
			Cai_options.feedback 0 "Files read: %s\n" !res;
		end
	end
	
(*get cfiles and directories from the current directory recursively *)	
let get_curdir_cfiles () 
: unit
=
	if !Cai_options.curdir then
		begin
			Cai_options.feedback 1 "Get current dir's c files...%s\n" !find_dirs_cmd;
			if Sys.command !find_dirs_cmd = 0 then
				begin
					read_file_with_prefix !tmp_files_fname "-I " include_path;
				end	
			else
				begin
					Cai_options.feedback 1 "Get current dir's directory failed, abort!\n";
					assert false
				end;
			Cai_options.feedback 1 "Get current dir's c files...%s\n" !find_cfiles_cmd;
			if Sys.command !find_cfiles_cmd = 0 then
				begin
					read_file_with_prefix !tmp_files_fname "" singleton_filenames;
				end	
			else
				begin
					Cai_options.feedback 1 "Get current dir's c files failed, abort!\n";
					assert false
				end;
			safe_remove !tmp_files_fname						
		end


(*two phases to merge multi-files:*)
(* 1. gcc preprocess all the multi-files into one file name is tmp_pp_filename*)
(* 2. use cilly diriver merge the preprocess file, then we get the final file*)	
let merge () 
:
	Cil.file
=
	get_curdir_cfiles ();
	cilly_driver := !Cai_options.exec_path_prefix  ^ !cilly_driver;
	if !Cai_options.curdir <> true then
		singleton_filenames := List.fold_left (fun accstr fname -> accstr^" "^fname) "" !filenamelist;
	let ppcmd = !gcc_preprocess_cmd ^ !include_path ^ !singleton_filenames ^ " > " ^ !tmp_pp_filename in
	Cai_options.feedback 0 "Preprocess comands:%s\n" ppcmd;
	let retcode = Sys.command ppcmd in
	if  retcode = 0 || retcode = 1 then
		begin
			Mergecil.ignore_merge_conflicts := true;
			let cmd_dosimpleMem =
				match !Cai_options.dosimpleMem with
				| true -> " --dosimpleMem"
				| false -> ""
			in
			let cmd_dosimplify =
				match !Cai_options.dosimplify with
				| true -> " --dosimplify"
				| false -> ""
			in
			let merge_cmd = !cilly_driver ^ !tmp_merge_fname ^ " " ^ !tmp_pp_filename ^ cmd_dosimpleMem ^ cmd_dosimplify in
			Cai_options.feedback 0 "Merge comands:%s\n" merge_cmd;
			if Sys.command merge_cmd = 0 then
				begin				   	
        	let _,cil_file = Frontc.parse_with_cabs !tmp_merge_fname () in
        	cil_file
    		end
			else
    		begin
    			Cai_options.feedback 0 "File merge error, abort!\n";
    			assert false
    		end				
		end 
	else
		begin
			Cai_options.feedback 0 "File preprocess error, abort!\n";
			assert false
		end

let doinline (file:Cil.file):
unit
=
	let get_inline_func fnamelist global  = 
		(match global with
		| GFun (dec,_) ->
				dec.svar.vname::fnamelist
		| _ -> fnamelist) 
	in
	Inliner.toinline := (List.fold_left get_inline_func [] file.globals);
	
	List.iter (fun x -> Cai_options.feedback 0  "Inline function name :%s\n" x) !Inliner.toinline;
	Inliner.doInline := true;
	Inliner.doit file

 		
let merge_and_inline ():Cil.file
=
	let merge_file = merge () in (*file name list has been get from the command line*)

	if !Cai_options.inline then
	begin
		Inliner.only_inline_main := false;
		Inliner.main_entry := !Cai_options.main_entry;
		doinline merge_file;
		Ptranal.analyze_file merge_file;
		Ptranal.compute_results true;
		Cai_sharedVariable.getSharedVar merge_file;
		if !Cai_options.int_proc_name <> [] then
			Cai_interrupt.insertIntBranch merge_file;
		doinline merge_file;
	end;
	
	Ciltools.one_instruction_per_statement merge_file;

	
	let oneretGlobal f =
	match f with
	| GFun (fundec, _) ->
	(* ensure there is only one return *)
			Cil.prepareCFG fundec;
			Oneret.oneret fundec;
			
	(* Build the Control Flow Graph for all functions *)
	| _ -> () in
	List.iter oneretGlobal merge_file.globals;
	
	Cfg.clearFileCFG merge_file;
	Cfg.computeFileCFG merge_file;
	
	Rmtmps.removeUnusedTemps merge_file;
	if !Cai_options.save_tmps <> true then
	begin
  	safe_remove !tmp_merge_fname;
  	safe_remove !tmp_pp_filename;
	end
	else
	begin
		let out_chan = open_out_gen [Open_wronly;Open_creat; Open_trunc ;Open_text] 0o666 !tmp_inline_fname in
		let cil_printer = new Cil.defaultCilPrinterClass in
			List.iter (fun g -> 
				match g with
				| GFun(fundec,_) ->
					if fundec.svar.vname = !Cai_options.main_entry then 
						Printf.fprintf out_chan "%a" cil_printer#dGlobal g
				| _ ->
					Printf.fprintf out_chan "%a" cil_printer#dGlobal g) merge_file.globals;
			
		close_out out_chan
	end;
	merge_file		