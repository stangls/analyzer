
open Cil
open Liveness
open ExternTypes

(* is a Manipulator, see externTypes.ml *)

type t = invariant list
type vi = var_invariant list

let filter_pos invs f l1 c1 l2 c2 =
  let map ((vis,ts):vi*t) (inv:invariant) : vi*t =
    match inv with
      | ( (Position (pf,pl,pc)), v ) when
        (*Printf.printf "filter_pos %s %d,%d ∈? %s %d,%d-%d,%d: \n" pf pl pc f l1 c1 l2 c2;*)
        (pl>l1 || pl=l1 && pc>c1 ) && (pl<l2 || pl=l2 && pc<=c2) && (String.compare pf f = 0) -> (v@vis,ts) (* todo: use string hashes *)
      | _ -> (vis,inv::ts)
  in List.fold_left map ([],[]) invs

let filter_func invs f func =
  let map (vis,ts) inv =
    match inv with
      | ( (FunctionEntry (pf,pfunc)), v ) when (String.compare pfunc func = 0 ) && (String.compare pf f = 0) -> (v@vis,ts) (* todo: use string hashes *)
      | _ -> (vis,inv::ts)
  in List.fold_left map ([],[]) invs

let print_location loc ~name =
  () (*Printf.printf "Location: line %d , byte %d, file %s (%s)\n" loc.line loc.byte loc.file name*)

class expr_converter_visitor (invariants:invariant list) =
  object (this)
    inherit nopCilVisitor
    val mutable invs = invariants
    val mutable exprs = []
    val mutable prev_line = 0
    val mutable prev_byte = 0
    val mutable recent_liveset = None
    val mutable num_filtered_invariants = 0
    method result = exprs
    (* create cil expressions from list of matched var_invariants at location loc with known liveset of variables. *)
    method matched (* (var_invariants,filtered_invariants) loc *) = begin
      match recent_liveset with
      | Some liveset -> fun (var_invariants,filtered_invariants) loc -> begin
        invs <- filtered_invariants;
        (* translate a var_invariant to a cil_var *)
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
        (* create expressions for list of var_invariants *)
        let rec get_exprs var_invariants : Cil.exp list =
          (* todo: think about: maybe we have to get AND ( OR(var1_exprs), OR(var2_exprs),... ), i.e. sorted by var.name, i.e. use some kind of map *)
          (*       currently it is AND ( var1_expr1, var2_expr1, var1_expr2, OR( var3expr3base1, var3expr3base1 ), OR( var3expr4base1, var3expr4base1 ) ... ) *)
          (*       and should be AND ( OR ( var1_expr1, var1_expr2 ), var2_expr1 , OR( var3expr3base1, var3expr3base1, var3expr4base1, var3expr4base1 ) ... ) *)
          match var_invariants with
            | (var,value)::vis -> begin
              match get_cil_var var with
                | Some cil_var -> begin
                    match value with
                    (* interval special case : single value *)
                    | Interval (min,max) when min=max ->
                        ( Formatcil.cExp
                          "%v:var == %d:val"
                          [
                            ("var",Fv cil_var);
                            ("val",Fd(min)); (* todo: check if var type matches cil_var.vtype, use kinteger and %e *)
                          ]
                        ) :: (get_exprs vis)
                    (* interval *)
                    | Interval (min,max) ->
                        ( Formatcil.cExp
                          "( %v:var >= %d:min ) %b:and ( %v:var <= %d:max )"
                          [
                            ("var",Fv cil_var);
                            ("min",Fd(min)); (* todo: check if var type matches cil_var.vtype, use kinteger and %e *)
                            ("and",Fb LAnd);
                            ("max",Fd(max)); (* todo: check if var type matches cil_var.vtype, use kinteger and %e *)
                          ]
                        ) :: (get_exprs vis)
                    (* pointer *)
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
                                          ("min",Fd(min)); (* todo: check if var type matches cil_var.vtype, use kinteger and %e *)
                                          ("and",Fb LAnd);
                                          ("max",Fd(max)); (* todo: check if var type matches cil_var.vtype, use kinteger and %e *)
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
                | None -> get_exprs vis
              end
            | [] ->  []
        in match merge_exprs (get_exprs var_invariants) (Fb LAnd) with
          | None -> ()
          | Some e -> exprs <- (loc,e)::exprs
      end
      (* no liveset? then we don't care about the filtered stuff *)
      | None -> fun _ _ -> Printf.printf "ERROR: Missing liveset information!\n"
    end
    method tryMatch loc =
      let cur_line=loc.line and cur_byte=loc.byte in begin
        (* avoid more than one matching in same line with different byte offset *)
        if cur_line=prev_line && cur_byte<>prev_byte then begin
          let oops = ref 0 in begin
            exprs <- List.filter ( fun (l,_) -> if l.line=cur_line then begin oops:=!oops+1; false end else true ) exprs;
            if !oops>0 then begin
              Printf.printf "WARNING: more than one location suitable for external invariants in %s line %i (maybe two instructions in one line?)\n" loc.file cur_line;
              Printf.printf "         had to ignore %d invariants because I'm not able to handle this (yet?).\n" !oops;
              num_filtered_invariants <- num_filtered_invariants + !oops
            end
          end
        end else begin
          this#matched (filter_pos invs loc.file prev_line max_int cur_line max_int) loc;
          prev_line <- cur_line
        end;
        prev_byte <- cur_byte
      end
    (* visit global function *)
    method vfunc func = let loc=func.svar.vdecl in begin
      (* todo : function entry points f.svar.vname *)
      recent_liveset <- None;
      print_location loc ~name:"func";
      prev_line <- loc.line;
      computeLiveness func;
      DoChildren
    end
    (* visit statement *)
    method vstmt stmt =
      recent_liveset <- getLiveSet stmt.sid;
      let loc=get_stmtLoc stmt.skind
      in begin
        print_location loc ~name: "stmt";
        this#tryMatch loc;
        DoChildren
      end
    (* visit instruction *)
    method vinst instr =
      let loc=get_instrLoc instr
      in begin
        print_location loc ~name:"instr";
        this#tryMatch loc;
        SkipChildren
      end
    (* get list of unvisited invariants *)
    method get_invs = invs
    (* get number of invariants filtered away *)
    method get_num_filtered_invs = num_filtered_invariants
  end

let to_cil_invariant invariants file =
  (* cilVisitor for visiting functions and statements *)
  let visitor = new expr_converter_visitor invariants
  in begin
    (* compute the control flow graph *)
    (*Cfg.clearFileCFG file ;
    Cfg.computeFileCFG file ;*)
    (* create expressions from invariants *)
    visitCilFileSameGlobals ( visitor :> cilVisitor ) file;
    (* show how many invariants did not stick to the code locations visited *)
    let num_untransformed = List.length (visitor#get_invs) + visitor#get_num_filtered_invs
    in if (num_untransformed>0) then
      Printf.printf "WARNING: %d invariants could not be matched to appropriate code positions or translated to cil expressions!\n" num_untransformed;
    (* return result *)
    visitor#result
  end
