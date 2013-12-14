(* External information of other tools *)

open Cil
open Liveness

type variable = {
  name:string;
  (*ctype:string;*)
  (*file:string; (* should be irrelephant: either the variable is in the scope or not... *) *)
}

type pointer_base =
  | Variable of variable
  | Null
  | Invalid 

type value =
    (* min, max *)
  | Interval of int * int
    (* name, type, nested *)
  | Field of string * string * value list
    (* offset, type, nested *)
  | ArrayField of int * string * value list
    (* bases, intervalMin, intervalMax *)
  | Pointer of pointer_base list * int * int
  (* Basetype.Variables *)

type location =
    (* file, line, column *)
    Position of string * int * int
    (* filename, functionName *)
  | FunctionEntry of string * string

type varInfo = variable*value
type invariant = location * varInfo list
type cil_invariant = Cil.location * Cil.exp

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
  val filter_pos : t -> string -> int -> int -> int -> int -> vi*t
  (*
    returns a tuple of
    * all variable information at a certain function entry (2nd parameter) in a file (1st parameter)
    * all remaining invariants which are not in that range
  *)
  val filter_func : t -> string -> string -> vi*t
  (*
    transform invariants to cil_invariants given a cil file.
  *)
  val to_cil_invariant : t -> file -> cil_invariant list
end

(* heart of the file: the invariants manipulator *)
module Invariants : Manipulator =
struct
  type t = invariant list
  type vi = varInfo list

  let filter_pos invs f l1 c1 l2 c2 =
    let map ((vis,ts):vi*t) (inv:invariant) : vi*t =
      match inv with
        | ( (Position (pf,pl,pc)), v ) when
          (pl>l1 || pl=l1 && pc>c1 ) && (pl<l2 || pl=l2 && pc<=c2) && (String.compare pf f = 0) -> (v@vis,ts) (* todo: use string hashes *)
        | _ -> (vis,inv::ts)
    in List.fold_left map ([],[]) invs

  let filter_func invs f func =
    let map (vis,ts) inv =
      match inv with
        | ( (FunctionEntry (pf,pfunc)), v ) when (String.compare pfunc func = 0 ) && (String.compare pf f = 0) -> (v@vis,ts) (* todo: use string hashes *)
        | _ -> (vis,inv::ts)
    in List.fold_left map ([],[]) invs

  let to_cil_invariant invariants file =
    (* cilVisitor for visiting functions and statements *)
    let visitor =
      object (this)
        inherit nopCilVisitor
        val mutable invs = invariants
        val mutable exprs = []
        val mutable prev_line = 0
        method result = exprs
        (* create cil expressions from list of matched var_infos (according to location loc) with known liveset of variables. *)
        method matched (var_infos,filtered_invariants) loc liveset = begin
          invs <- filtered_invariants;
          (* translate a varInfo to a cil_var *)
          let rec get_cil_var var =
            let cil_var = ref None
            in let possible_cil_var cil_var' =
              if cil_var'.vname=var.name then begin
                cil_var := Some cil_var';
                true (* = stop searching *)
              end else
                false (* = continue searching *)
            in begin
              if not (VS.exists possible_cil_var liveset) then
                Printf.printf "unknown variable %s " var.name
              ;
              !cil_var
            end
          in
          (* merge list of cil-expressions using binary operand like "Fb LOr" or "Fb LAnd" *)
          let rec merge_exprs (exprs:Cil.exp list) operand : Cil.exp option = 
            let rec merge_exprs' exprs expr = match exprs with
              | e::es -> merge_exprs' es (Formatcil.cExp "( %e:e1 ) %b:op ( %e:e2 )" [ "e1",Fe e;"e2",Fe expr;"op",operand; ])
              | [] -> expr
            in match exprs with
              | [e] -> Some e
              | e::es -> Some (merge_exprs' es e)
              | [] -> None
          in
          (* create expressions for list of var_infos *)
          let rec get_exprs var_infos : Cil.exp list =
            (* todo: think about: maybe we have to get AND ( OR(var1_exprs), OR(var2_exprs),... ), i.e. sorted by var.name, i.e. use some kind of map *)
            (*       currently it is AND ( var1_expr1, var2_expr1, var1_expr2, OR( var3expr3base1, var3expr3base1 ), OR( var3expr4base1, var3expr4base1 ) ... ) *)
            (*       and should be AND ( OR ( var1_expr1, var1_expr2 ), var2_expr1 , OR( var3expr3base1, var3expr3base1, var3expr4base1, var3expr4base1 ) ... ) *)
            match var_infos with
              | (var,value)::vis -> (
                match get_cil_var var with
                  | Some cil_var -> (
                      match value with
                      | Interval (min,max) ->
                          ( Formatcil.cExp
                            "( %v:var >= %d:min ) %b:and ( %v:var <= %d:max )"
                            [
                              ("var",Fv cil_var);
                              ("min",Fd(min)); (* todo: check if var type matches cil_var.vtype *)
                              ("and",Fb LAnd);
                              ("max",Fd(max)); (* todo: check if var type matches cil_var.vtype *)
                            ]
                          ) :: (get_exprs var_infos)
                      | Pointer (bases,min,max) ->
                          let rec exprs_for_bases bases agg : Cil.exp list = match bases with
                            | base::bs -> (
                                match base with 
                                | Null ->
                                  let expr =
                                    Formatcil.cExp
                                      "%v:var = NULL"
                                      [
                                        ("var",Fv cil_var);
                                      ]
                                   in exprs_for_bases bs (expr::agg)
                                | Variable base_var ->
                                  let agg'=match get_cil_var base_var with
                                    | Some cil_base_var ->
                                      let expr = 
                                        Formatcil.cExp
                                          "( %v:var >= ( %v:base_var + %d:min ) ) %b:and ( %v:var <= ( %v:base_var + %d:max ) )"
                                          [
                                            ("var",Fv cil_var);
                                            ("base_var",Fv cil_base_var);
                                            ("min",Fd(min)); (* todo: check if var type matches cil_var.vtype *)
                                            ("and",Fb LAnd);
                                            ("max",Fd(max)); (* todo: check if var type matches cil_var.vtype *)
                                          ]
                                       in expr::agg
                                     | None -> agg
                                   in exprs_for_bases bs agg'
                                | Invalid -> [] (* todo : invalid means what exactly? we know nothing? *)
                              )
                            | [] -> agg
                          in match ( merge_exprs (exprs_for_bases bases []) (Fb LOr) ) with
                            | Some x -> x :: (get_exprs var_infos)
                            | None -> get_exprs var_infos
                      | _ -> Printf.printf "Unsupported external type"; []
                    )
                  | None -> []
                )
              | [] ->  []
          in match merge_exprs (get_exprs var_infos) (Fb LAnd) with
            | None -> ()
            | Some e -> exprs <- (loc,e)::exprs
        end
        method vfunc func = let loc=func.svar.vdecl in begin
          (* todo : function entry points f.svar.vname *)
          Printf.printf "Location: line %d , byte %d, file %s\n" loc.line loc.byte loc.file;
          prev_line <- loc.line;
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
                      Printf.printf "Location: line %d , byte %d, file %s\n" loc.line loc.byte loc.file;
                      this#matched (filter_pos invs loc.file prev_line max_int cur_line max_int) loc liveset; (* todo: columns *)
                      prev_line <- cur_line;
                      DoChildren
                  end
                | Instr instructions -> 
                  let one_instr instr = match instr with
                    | Set(_,_,loc)
                    | Call(_,_,_,loc)
                    | Asm(_,_,_,_,_,loc)
                      ->  let cur_line=loc.line in begin
                              Printf.printf "Location: line %d , byte %d, file %s\n" loc.line loc.byte loc.file;
                              this#matched (filter_pos invs loc.file prev_line max_int cur_line max_int) loc liveset; (* todo: columns *)
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
          ( { name="x"; (*ctype="int";*) } , Interval(1,1) )
        ]
      )
    ]

end

(* todo : add AstreeXmlInvariantsFile *)

