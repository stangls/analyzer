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

type var_invariant = variable*value
type invariant = location * var_invariant list
type cil_invariant = Cil.location * Cil.exp

(* pretty printers *)
let d_i = indent 2
let d_variable (v:variable) :doc = text v.name
let d_pointer_base (pb:pointer_base) :doc = match pb with
| Null -> text "Null"
| Invalid -> text "Invalid"
| Variable v -> d_variable v
let rec d_value (v:value) :doc = match v with
| Interval (min,max) -> text "["++num min++text ", "++num max++text "]"
| Field (name,typ,values) -> text "Field ("++break++(d_i (
    text "name   : " ++ text name ++ text "," ++ break ++
    text "type   : " ++ text typ ++ text "," ++ break ++
    text "values : " ++ docList d_value () values ++ break
  ))++text ")"
| ArrayField (offset,typ,values) -> text "ArrayField ("++break++(d_i (
    text "offset : " ++ num offset ++ text "," ++ break ++
    text "type   : " ++ text typ ++ text "," ++ break ++
    text "nested : " ++ docList d_value () values ++ break
  ))++text ")"
| Pointer (pbases,min,max) -> text "Pointer ("++break++(d_i (
    text "bases  : " ++ docList d_pointer_base () pbases ++ text "," ++ break ++
    text "min    : " ++ num min ++ text "," ++ break ++
    text "max    : " ++ num max ++ text "," ++ break
  ))++text ")"
let d_location (l:location) :doc = match l with
| Position (file,line,col) -> text "Position ("++break++(d_i (
    text "file   : " ++ text file ++ text "," ++ break ++
    text "line   : " ++ num line ++ text "," ++ break ++
    text "column : " ++ num col ++ text "," ++ break
  ))++text " )"
| FunctionEntry (file,fname) -> (text "FunctionEntry (")++break++(d_i (
    text "file     : " ++ text file ++ text "," ++ break ++
    text "function : " ++ text fname ++ break
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
    transform invariants to cil_invariants given a cil file.
  *)
  val to_cil_invariant : t -> file -> cil_invariant list
end

