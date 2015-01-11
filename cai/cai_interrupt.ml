open Cil
open Cai_sharedVariable
module H = Hashtbl

let brandom_va = makeVarinfo true "__cai_brandom" (TInt (IBool,[]))
let brandom = Lval((Var brandom_va), NoOffset)
let tmp_intInsert_fname = "__cai_intInsert_tmp.c"

let genIntBranchStmtList (i : Cil.instr) (level : int) (loc : location)
:
  (stmt list)
=
	let wset = Cai_sharedVariable.getWriteSet i in
	let curLevel = ref 0 in
	let genIntCall acc intName =
		if !curLevel > level then
		begin
			let procAccSet = (try
				Hashtbl.find Cai_sharedVariable.globalSharedVar_tbl intName 
			with Not_found -> assert false) in
			let interSet = StrSet.inter procAccSet wset in
			if (StrSet.cardinal interSet) <> 0 then
			begin
				curLevel := !curLevel + 1;
				let int_va = makeVarinfo true intName (TVoid []) in
				let int_lval = Lval((Var int_va),NoOffset) in
				let int_call = Call(None,int_lval,[],loc) in
				let int_call_stmt = mkStmt (Instr[int_call]) in
				let int_if = If(brandom,mkBlock [int_call_stmt],mkBlock [],loc) in
				let int_if_stmt = mkStmt int_if in
				List.append acc [int_if_stmt]
			end
			else
				acc
		end
		else
			(curLevel := !curLevel + 1;acc) 
	in
	List.fold_left genIntCall [] !Cai_options.int_proc_name 

(* This visitor walks over the C program AST and modifies it so that each
 * statment add the interrupt call function after each of the statements. *) 
class intInsertVisitor = object (self:'self)
  inherit nopCilVisitor
	val mutable curLevel = -1
	
	val stmtmap = (H.create 113 : (int, stmt) H.t)
	val patches : stmt list ref = ref []
	
  method vblock b =
		if curLevel = -1 then
			SkipChildren
		else 
			DoChildren

  method vstmt s =
		if curLevel = -1 then 
			SkipChildren
		else
			let s' = s in
			H.add stmtmap s'.sid s';
  		match s'.skind with
  		| Instr ([]) -> SkipChildren
  		| Instr linstr ->
  			let llresult = List.map (fun i ->
  				let loc = Cil.get_instrLoc i in
  				let int_branch = genIntBranchStmtList i curLevel loc in
  				let stmt = Cil.mkStmtOneInstr i in
  				List.append [stmt] int_branch 
  				) linstr in
  			let result = List.flatten llresult in
  			let bresult = Cil.mkBlock result in
				s'.skind <- (Block bresult);
  			ChangeTo s'
			| Switch _ 
			| Goto _ ->
				patches := s' :: !patches;
				DoChildren
  		| _ -> 
				DoChildren
		
	method vfunc f = 
		H.clear stmtmap;
		patches := [];
		curLevel <- -1;
		let nth = ref 0 in
		List.iter (fun s -> if s = f.svar.vname then curLevel <- !nth; nth := !nth + 1;()) !Cai_options.int_proc_name;
		let patchfunction (f' : fundec) =
			let findStmt (i: int) =
				try H.find stmtmap i
				with Not_found -> Printf.fprintf stdout "Cannot find the copy of stmt#%d" i;assert false;
			in
			let patchstmt (s: stmt) =
				match s.skind with
					Goto (sr, l) ->
						let sr' = ref (findStmt !sr.sid) in
						s.skind <- Goto (sr', l)
				| Switch (e, body, cases, l) ->
						s.skind <- Switch (e, body,
							Util.list_map (fun cs -> findStmt cs.sid) cases, l)
				| _ -> ()
			in
			List.iter patchstmt !patches;
			f'
		in
		ChangeDoChildrenPost(f,patchfunction)
end 
	
let insertIntBranch (file : Cil.file)
: 
  unit
=
	let intVisitor = new intInsertVisitor in
	Cil.visitCilFile intVisitor file;
	(*output interrupts insert file*)
	if !Cai_options.save_tmps = true then
  	(let out_chan = open_out_gen [Open_wronly;Open_creat; Open_trunc ;Open_text] 0o666 tmp_intInsert_fname in
  	let cil_printer = new Cil.defaultCilPrinterClass in
  		List.iter (fun g -> Printf.fprintf out_chan "%a" cil_printer#dGlobal g) file.globals;
  	close_out out_chan)
	