(* Processing of external information of other tools *)

open Externtypes
open Cil

(* predefined dummy-values instead of reading actual file *)
module DummyInvariantsReader : InvariantsReader =
struct
  type t = invariant list

  let of_c_file fn =
    Some [
      (
        Position (fn,6,0) , [
          ( { name="x"; (*ctype="int";*) } , Interval(1,1) )
        ]
      )
    ]

end

(* todo : add AstreeXmlInvariantsFile *)

(* heart of the file: the invariants manipulator *)
module Invariants : Manipulator =
struct
  type t = invariant list
  type vi = varInfo list

  open Liveness
  open Cil

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

  class expr_converter_visitor (invariants:invariant list) =
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
              Printf.printf "WARNING: unknown variable %s in external invariant\n" var.name
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
            | (var,value)::vis -> begin
              match get_cil_var var with
                | Some cil_var -> begin
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
                        ) :: (get_exprs vis)
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
                        in begin
                          match ( merge_exprs (exprs_for_bases bases []) (Fb LOr) ) with
                          | Some x -> x :: (get_exprs vis)
                          | None -> get_exprs vis
                        end
                    | _ -> Printf.printf "Unsupported external type"; []
                 end
                | None -> []
              end
            | [] ->  []
        in match merge_exprs (get_exprs var_infos) (Fb LAnd) with
          | None -> ()
          | Some e -> exprs <- (loc,e)::exprs
      end
      (* visit global function *)
      method vfunc func = let loc=func.svar.vdecl in begin
        (* todo : function entry points f.svar.vname *)
        print_location loc;
        prev_line <- loc.line;
        computeLiveness func;
        DoChildren
      end
      (* visit statement *)
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
                  print_location loc;
                  this#matched (filter_pos invs loc.file prev_line max_int cur_line max_int) loc liveset; (* todo: columns *)
                  prev_line <- cur_line;
                  DoChildren
                end
              | Instr instructions -> 
                let one_instr instr = begin
                  match instr with
                  | Set(_,_,loc)
                  | Call(_,_,_,loc)
                  | Asm(_,_,_,_,_,loc)
                    ->  let cur_line=loc.line in begin
                            print_location loc;
                            this#matched (filter_pos invs loc.file prev_line max_int cur_line max_int) loc liveset; (* todo: columns *)
                            prev_line <- cur_line;
                        end
                  | _ -> Printf.printf "WARNING: Unknown CIL instruction!\n"
                end
                in List.iter one_instr instructions; SkipChildren
              | Block _ -> DoChildren
              | _ -> Printf.printf "WARNING: Unknown CIL statement!\n"; SkipChildren
          )
        | None -> DoChildren
      method get_invs = invs
    end

  let to_cil_invariant invariants file =
    (* cilVisitor for visiting functions and statements *)
    let visitor = new expr_converter_visitor invariants
    in begin
      (* compute the control flow graph *)
      Cfg.clearFileCFG file ;
      Cfg.computeFileCFG file ;
      (* create expressions from invariants *)
      visitCilFileSameGlobals ( visitor :> cilVisitor ) file;
      (* show how many invariants did not stick to the code locations visited *)
      let num_untransformed = List.length (visitor#get_invs)
      in if (num_untransformed>0) then Printf.printf "WARNING: %d invariants could not be matched to appropriate code positions!\n" num_untransformed;
      (* return result *)
      visitor#result
    end

end

(* helpers for global access ... *)

(* specify type of invariants reader *)
module IF = DummyInvariantsReader
(* storage for converted variants *)
let loaded_invariants = ref ([]:cil_invariant list)

(* load invariants for c-file and convert to cil-format *)
let load_cil_invariants merged_AST cFileNames =
  let add_invariants agg cFile = 
    match IF.of_c_file cFile with
      | Some invariants -> (Invariants.to_cil_invariant invariants merged_AST) @ agg
      | None            -> agg
  in
    loaded_invariants := (List.fold_left add_invariants [] cFileNames)

(*
  retrieve pre-statement invariant for Cil.location loc.
  call [load_cil_invariants] first to initialize from files.
*)
let get_cil_invariants (loc:Cil.location) =
  let filter_fun (loc',_:cil_invariant) = ( loc.line=loc'.line && (String.compare loc.file loc'.file = 0) )
  in List.filter filter_fun !loaded_invariants

