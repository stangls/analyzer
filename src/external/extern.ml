(* External information of other tools *)

open Cil
open Liveness

type value =
    (* min, max *)
    Interval of int * int
    (* name, type, nested *)
  | Field of string * string * value list
    (* offset, type, nested *)
  | ArrayField of int * string * value list
    (* bases, intervalMin, intervalMax *)
  | Pointer of string list * int * int
  (* Basetype.Variables *)

type variable = {
  name:string;
  ctype:string;
  (*file:string; (* should be irrelephant: either the variable is in the scope or not... *) *)
}

type location =
    (* file, line, column *)
    Position of string * int * int
    (* filename, functionName *)
  | FunctionEntry of string * string

type varInfo = variable*value
type invariant = location * varInfo list

(* load invariants from a file *)
module type InvariantsFile =
sig
  type t = invariant list

  (* load from xml-file *)
  val from_file : string -> t option

end

(* manipulate loaded invariants *)
module type Manipulator =
sig
  type t = invariant list
  type vi = varInfo list
  (*
    returns a tuple of
    * all variable information at a certain position in a file (1st parameter)
      after line1, column1 (2nd&3rd) but before line2, column2 (4th&5th, including).
    * all remaining invariants which are not in that range
  *)
  val at_pos : t -> string -> int -> int -> int -> int -> vi*t
  (*
    returns a tuple of
    * all variable information at a certain function entry (2nd parameter) in a file (1st parameter)
    * all remaining invariants which are not in that range
  *)
  val at_func : t -> string -> string -> vi*t
  (*
    transform invariants to cil-expressions of a given cil-file
  *)
  val to_cil_expr : t -> file -> exp list
end

(* heart of the file: the invariants manipulator *)
module Invariants : Manipulator =
struct
  type t = invariant list
  type vi = varInfo list

  let at_pos invs f l1 c1 l2 c2 =
    let map ((vis,ts):vi*t) (inv:invariant) : vi*t =
      match inv with
        | ( (Position (pf,pl,pc)), v ) when
          (pl>l1 || pl=l1 && pc>c1 ) && (pl<l2 || pl=l2 && pc<=c2) && (String.compare pf f = 0) -> (v@vis,ts) (* todo: use string hashes *)
        | _ -> (vis,inv::ts)
    in List.fold_left map ([],[]) invs

  let at_func invs f func =
    let map (vis,ts) inv =
      match inv with
        | ( (FunctionEntry (pf,pfunc)), v ) when (String.compare pfunc func = 0 ) && (String.compare pf f = 0) -> (v@vis,ts) (* todo: use string hashes *)
        | _ -> (vis,inv::ts)
    in List.fold_left map ([],[]) invs

  let to_cil_expr invariants file =
    (* cil visitor for visiting functions *)
    let visitor =
      object (this)
        inherit nopCilVisitor
        val mutable invs = invariants
        val mutable exprs = []
        val mutable prev_line = 0
        method result = exprs
        (* create cil expressions for matched var-infos (according to location loc) with known liveset of variables. *)
        method matched (var_infos,new_invariants) loc liveset = begin
          invs <- new_invariants;
          let rec add_exprs var_infos =
            match var_infos with
              | (var,value)::vis ->
                let possible_cil_var cil_var =
                  if cil_var.vname=var.name then
                    let _ = match value with
                      | Interval (min,max) ->
                          exprs <- ( Formatcil.cExp
                            "%v:x >= %d:min"(*"%v:x >= %f:MIN %b:AND %v:x <= %f:MAX"*)
                            [
                              ("x",Fv cil_var);
                              ("min",Fd(min)); (* todo: check if var type matches cil_var.vtype *)
                              ("and",Fb BAnd);
                              ("max",Fd(max)); (* todo: check if var type matches cil_var.vtype *)
                            ]
                          ) :: exprs
                      | _ -> Printf.printf "Unsupported external type"
                    in 
                    true (* = stop searching *)
                  else
                    false (* = continue searching *)
                in let _ = VS.exists possible_cil_var liveset
                in ()
              | [] ->  ()
          in add_exprs var_infos
        end
        method vfunc func = begin
          (* todo : function entry points f.svar.vname *)
          prev_line <- func.svar.vdecl.line;
          computeLiveness func;
          DoChildren
        end
        method vstmt stmt = match getLiveSet stmt.sid with
          | Some liveset -> (
              match stmt.skind with
                | Return(_,loc)
                | Goto(_,loc)
                | Break(loc)
                | Continue(loc)
                | If(_,_,_,loc)
                | Switch(_,_,_,loc)
                | Loop(_,loc,_,_)
                | TryFinally(_,_,loc)
                | TryExcept(_,_,_,loc) ->
                  let cur_line=loc.line in begin
                      this#matched (at_pos invs loc.file prev_line max_int cur_line max_int) loc liveset; (* todo: columns *)
                      prev_line <- cur_line;
                      DoChildren
                  end
                | Instr instructions -> 
                  let one_instr instr = match instr with
                    | Set(_,_,loc)
                    | Call(_,_,_,loc)
                    | Asm(_,_,_,_,_,loc)
                      ->  let cur_line=loc.line in begin
                              this#matched (at_pos invs loc.file prev_line max_int cur_line max_int) loc liveset; (* todo: columns *)
                              prev_line <- cur_line;
                          end
                    | _ -> Printf.printf "WARNING: Unknown CIL instruction!\n"
                  in List.iter one_instr instructions; SkipChildren
                | Block _ -> DoChildren
                | _ -> Printf.printf "WARNING: Unknown CIL statement!\n"; SkipChildren
            )
          | None -> DoChildren
      end
    in begin
      (* compute the control flow graph *)
      Cfg.clearFileCFG file ;
      Cfg.computeFileCFG file ;
      visitCilFileSameGlobals ( visitor :> cilVisitor ) file;
      visitor#result
    end

end

(* predefined values instead of reading actual file *)
module DummyInvariantsFile : InvariantsFile =
struct
  type t = invariant list

  let from_file fn =
    Some [
      (
        Position (fn,7,0) , [
          ( { name="x"; ctype="int"; } , Interval(1,1) )
        ]
      )
    ]

end

(* todo : add AstreeXmlInvariantsFile *)

