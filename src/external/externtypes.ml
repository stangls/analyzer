(* internal representation of external information with interfaces for goblint *)

open Cil

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
module type InvariantsReader =
sig
  type t = invariant list

  (* load from xml-file *)
  val of_c_file : string -> t option

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

