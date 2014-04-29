(* internal representation of external information with interfaces for goblint *)

open Cil
open Pretty

type variable = {
  name:string;
  (*ctype:string;*)
  (*file:string; (* should be irrelevant: either the variable is in the scope or not... *) *)
}

type pointer_base =
  | Variable of variable
  | Null
  | Invalid 

type value =
    (* min, max *)
  | Interval of int64 * int64
    (* name, type, nested *)
  | Field of string * string * value list
    (* offset, type, nested *)
  | ArrayField of int64 * string * value list
    (* bases, intervalMin, intervalMax *)
  | Pointer of pointer_base list * int64 * int64
    (* used internally to define more complex values *)
  | Or of value list
    (* we might use this to express, that the datatype is not suitable with this data-type *)
  | Undefined

type location =
    (* file, line, column *)
    Position of string * int * int
    (* filename, functionName, line, column *)
  | FunctionEntry of string * string * int * int

type var_invariant = variable*value
type invariant = location * var_invariant list
(* invariant as cil expression *)
type cil_invariant = Cil.location * Cil.exp
(* function entry invariant as cil expression *)
type cil_fun_invariant = Cil.fundec * Cil.exp

(* pretty printers *)
let d_i = indent 2
let d_variable (v:variable) :doc = text v.name
let d_pointer_base (pb:pointer_base) :doc = match pb with
| Null -> text "Null"
| Invalid -> text "Invalid"
| Variable v -> d_variable v
let rec d_value (v:value) :doc = match v with
| Interval (min,max) -> text "["++num64 min++text ", "++num64 max++text "]"
| Field (name,typ,values) -> text "Field ("++break++(d_i (
    text "name   : " ++ text name ++ text "," ++ break ++
    text "type   : " ++ text typ ++ text "," ++ break ++
    text "values : " ++ docList d_value () values ++ break
  ))++text ")"
| ArrayField (offset,typ,values) -> text "ArrayField ("++break++(d_i (
    text "offset : " ++ num64 offset ++ text "," ++ break ++
    text "type   : " ++ text typ ++ text "," ++ break ++
    text "nested : " ++ docList d_value () values ++ break
  ))++text ")"
| Pointer (pbases,min,max) -> text "Pointer ("++break++(d_i (
    text "bases  : " ++ docList d_pointer_base () pbases ++ text "," ++ break ++
    text "min    : " ++ num64 min ++ text "," ++ break ++
    text "max    : " ++ num64 max ++ text "," ++ break
  ))++text ")"
| Or vs -> text "Or ("++break++(d_i (
    List.fold_left (fun s v -> s++d_value v++break) nil vs
  ))++text " )"
| Undefined -> text "Undefined"
let d_location (l:location) :doc = match l with
| Position (file,line,col) -> text "Position ("++break++(d_i (
    text "file   : " ++ text file ++ text "," ++ break ++
    text "line   : " ++ num line ++ text "," ++ break ++
    text "column : " ++ num col ++ text "," ++ break
  ))++text " )"
| FunctionEntry (file,fname,line,col) -> (text "FunctionEntry (")++break++(d_i (
    text "file     : " ++ text file ++ text "," ++ break ++
    text "function : " ++ text fname ++ break ++
    text "line   : " ++ num line ++ text "," ++ break ++
    text "column : " ++ num col ++ text "," ++ break
  ))++text ")"
let d_var_invariant (var,value) =
  text "( " ++ d_variable var ++ text ", " ++ d_value value ++ text " )"
let d_invariant (loc,varInvList) =
  text "( " ++ break ++ (d_i (
    d_location loc ++ text ", " ++ break ++
    docList d_var_invariant () varInvList ++ break
  )) ++ text ")"
let d_cil_invariant ((cl,ce):cil_invariant) :doc =
  text "(" ++ break ++ d_i (
    d_loc () cl ++ text ", " ++ break ++
    d_exp () ce ++ break
  ) ++ text ")"
let d_cil_fun_invariant ((cf,ce):cil_fun_invariant) :doc =
  text "(" ++ break ++ d_i (
    text cf.svar.vname ++ text " ( @ " ++ d_loc () cf.svar.vdecl ++ text " ), " ++ break ++
    d_exp () ce ++ break
  ) ++ text ")"

(* load invariants from a file *)
module type InvariantsReader =
sig
  type t = invariant list

  (* load invariants of xml-file related to given c file (full path) *)
  val of_c_file : string -> t option
  (* load from actual-file (full path) *)
  val of_file : string -> t option

end

(* manipulate loaded invariants *)
module type Manipulator =
sig
  type t = invariant list
  (*
    given a cil file, transform invariants to
    * cil_invariants at locations
    * cil_fun_invariants at function-entries (with their original invariants attached)
    * invariants used for cil_invariants
    * invariants used for cil_fun_invariants
  *)
  val transform_to_cil : t -> file ->
    ( Cil.location , cil_invariant list ) Hashtbl.t * ( Cil.location * t ) list *
    ( string , cil_fun_invariant list ) Hashtbl.t * ( Cil.fundec * t ) list
  (*
    returns a tuple of
    * all invariants at a certain position in a file (1st parameter)
      after line1, column1 (2nd&3rd) but before line2, column2 (4th&5th, including).
    * all remaining invariants which are not in that range
  *)
  val filter_pos : t -> string -> int -> int -> int -> int -> t*t
  (*
    returns a tuple of
    * all variable information at a certain function entry (2nd parameter) in a file (1st parameter)
    * all remaining invariants which are not in that range
  *)
  val filter_func : t -> string -> string -> t*t
  (*
    group values of var_invariant instances by variable name using Or
    assumes the invariants are already grouped by location. (* todo: write function to group by location *)
  *)
  val group_by_variables : t -> t
end

(* helper for invariants *)
module Helper : sig
  val num_var_invariants : invariant list -> int
  val num_var_values : invariant list -> int
  val filter_undefined_var_invariants : invariant list -> int*(invariant list)
end = struct
  let num_var_invariants' cnt (_,vis:location*var_invariant list) = cnt+(List.length vis)
  let num_var_invariants invariants = List.fold_left num_var_invariants' 0 invariants

  let rec num_var_values''' cnt (value:value) = match value with
    | Or(subValues) -> List.fold_left num_var_values''' cnt subValues
    | _ -> cnt+1
  let rec num_var_values'' cnt (vis:var_invariant list) = match vis with
    | (var,values)::vis -> num_var_values'' ( num_var_values''' cnt values ) vis
    | [] -> cnt
  let num_var_values' cnt (_,vis:location*var_invariant list) = num_var_values'' cnt vis
  let num_var_values invariants = List.fold_left num_var_values' 0 invariants

  let filter_undefined_var_invariants'' (num,vis) (var,value) =
    match value with
    | Undefined -> (num+1,vis)
    | _ -> (num,(var,value)::vis)
  let filter_undefined_var_invariants' (num,invs) (loc,vis) =
    let (num',vis') = List.fold_left filter_undefined_var_invariants'' (num,[]) vis
    in match vis' with
    | [] -> (num',invs)
    | _ -> (num',(loc,vis')::invs)
  let filter_undefined_var_invariants invs = List.fold_left filter_undefined_var_invariants' (0,[]) invs
end

(* store invariants to a file *)
module type InvariantsWriter =
sig
  type t = invariant list

  (* store to actual-file (full path) *)
  val to_file : string -> t -> unit

end

(* creator of invariants *)
module type InvariantsCreator = sig
  type t
  type d
  val create : unit -> t
  (*
    store a (maybe incomplete) invariant.
    multiple invariants may be stored at the same location.
    parameters:
      * storage
      * location
      * Some x = function entry to function x, None = no function entry
      * variable to store information about
      * value (information to store)
  *)
  val add : t -> Cil.location -> string option -> Cil.varinfo -> d -> unit
  (*
    retrieve all invariants
  *)
  val retrieve : t -> invariant list
end

module type ValueDomainHandler = sig
  type t
  val merge : t -> t -> t
  val to_value : t -> value
end

module type InvariantsCreationHelper = sig
  type t
  (*
    create invariants from t at given location.
    parameters:
      * location
      * Some x = function entry to function x, None = no function entry
      * information to use to create invariants from
  *)
  val store : Cil.location -> string option -> t -> unit
  (* retrieve actual invariants *)
  val get_invariants : unit -> invariant list
end

