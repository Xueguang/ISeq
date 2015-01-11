open Cil

module Ordertype = struct 
	type t = String.t
	let compare = String.compare
end

module StrSet = Set.Make (Ordertype)

let globalSharedVar_tbl : (String.t,StrSet.t) Hashtbl.t = Hashtbl.create 7

let rec getOffsetStr (offset : Cil.offset)
	: String.t
=
	match offset with
	| NoOffset 
	| Index _ -> ""
	| Field (f,offseti) ->
		let offstr = getOffsetStr offseti in
		if offstr <> "" then
			f.fname ^ "." ^ offstr
		else 
			f.fname
		
let getWBaseStr (lval:Cil.lval)
	:  String.t list
= 
	let lh = fst lval in
	match lh with
	| Var v ->
  	if v.vglob then 
  	begin
			let str = 
    	(match snd lval with
    	| NoOffset ->
    		v.vname
    	| Field (f,offset) ->
    		let offstr = getOffsetStr offset in
    		if offstr <> "" then 
    			v.vname^"."^f.fname^"."^offstr
    		else 
    			v.vname^"."^f.fname
    	|  Index _ ->
    		v.vname) 
			in
			[str]
  	end
		else  []
	| Mem e -> 
		let points_to = ref [] in
		let points_to_str = ref [] in
		points_to := Ptranal.resolve_exp e;
		points_to_str := Ptranal.resolve_exp_string e;
		points_to_str := List.fold_left2 (fun acc v s -> 
				if v.vglob then 
					s :: acc
				else
					acc
				) [] !points_to !points_to_str;
		(match snd lval with
		| Field _ -> 
			let fields = getOffsetStr (snd lval) in
			if fields != "" then 
					(points_to_str := List.map (fun b -> b ^ fields) !points_to_str);
			()
		| _ -> ()
		);
		!points_to_str

let getWriteSet (i:Cil.instr)
: StrSet.t = 
	match i with
	| Set (l,_,_) ->
		let wstrlist =  getWBaseStr l in
		List.fold_left (fun set ws -> 
			StrSet.add ws set
			) StrSet.empty wstrlist
	| _ ->
			StrSet.empty
		
(* This visitor walks over the C program AST and modifies it so that each
 * statment add the interrupt call function after each of the statements. *) 
class sharedVarVisitor = object (self:'self)
  inherit nopCilVisitor
	val sid = ref 0;
	val curShVar = ref StrSet.empty;
	
	
	 method private  getOffsetStr (offset : Cil.offset)
	: String.t
	=
  	match offset with
  	| NoOffset -> ""
  	| Index(e,o) -> 
  		self#vexpr e;
  		self#getOffsetStr o;
  		""
  	| Field (f,offseti) ->
  		let offstr = getOffsetStr offseti in
			if offstr <> "" then
  			f.fname ^ "." ^ offstr
			else
				f.fname
	
		
  method private getBaseAccStr (lval:Cil.lval)
  	:  String.t
  = 
  	let lh = fst lval in
		let off = snd lval in
  	match lh with
  	| Var v ->
    	if v.vglob then 
    	begin
      	match off with
      	| NoOffset ->
      		v.vname
      	| Field (f,offset) ->
      		let offstr = getOffsetStr offset in
      		if offstr <> "" then 
      			v.vname^"."^f.fname^"."^offstr
      		else 
      			v.vname^"."^f.fname
      	| Index (e,offset) ->
					self#vexpr e;
					self#getOffsetStr offset;
      		v.vname
    	end
  		else  ""
  	| Mem e -> 
			self#vexpr e;
			let points_to = ref [] in
			let points_to_str = ref [] in
			points_to := Ptranal.resolve_exp e;
			points_to_str := Ptranal.resolve_exp_string e;
			points_to_str := List.fold_left2 (fun acc v s -> 
				if v.vglob then 
					s :: acc
				else
					acc
				) [] !points_to !points_to_str;
			(match off with
			| Index (ex,offset) -> 
				self#vexpr ex;
				self#getOffsetStr offset;()
			| Field (field,offset) ->
				let fields = getOffsetStr off in
				if fields != "" then 
					(points_to_str := List.map (fun b -> b ^ fields) !points_to_str);
				()
			| _ -> ()
				);
			List.iter (fun bstr -> curShVar := StrSet.add bstr !curShVar) !points_to_str;
			""
	
  method vblock b =
		DoChildren

  method vstmt s =
		s.sid <- !sid;
		incr sid;
		DoChildren 
		
	method vlval lval =
		let lval_points_to = Ptranal.resolve_lval lval in
		Cai_options.feedback 1 "lval is:%s\n" (Pretty.sprint 0 (Cil.dn_lval () lval));
		List.iter(fun lv -> 
			Cai_options.feedback 0 "lval is %s ==> %s\n" (Pretty.sprint 0 (Cil.dn_lval () lval)) lv.vname)
			lval_points_to;
		let lvalStr = self#getBaseAccStr lval in
		if lvalStr <> "" then
			curShVar := StrSet.add lvalStr !curShVar;
		SkipChildren
	
	method vfunc f = 
		ChangeDoChildrenPost(f,fun f -> 
			Hashtbl.add globalSharedVar_tbl f.svar.vname !curShVar;
			curShVar := StrSet.empty;
			f )
end 

			
let getSharedVar (file:Cil.file) = 
	Cai_options.feedback 0 "Process shared variables...\n";
	let shVarVisitor = new sharedVarVisitor in
	Cil.visitCilFile shVarVisitor file;
	Hashtbl.iter (fun k v ->
		Cai_options.feedback 0 "Function is %s :\n" k;
		StrSet.iter (fun s ->
			Cai_options.feedback 0 "\t Access variable: %s\n" s) v;
		) globalSharedVar_tbl;
	Cai_options.feedback 0 "Finish shared variable analysis!\n"

