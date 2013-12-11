(* External information of other tools *)

type value =
    (* min, max *)
    Interval of int64 * int64
    (* name, type, nested *)
  | Field of string * string * value list
    (* offset, type, nested *)
  | ArrayField of int * string * value list
    (* bases, intervalMin, intervalMax *)
  | Pointer of string list * int64 * int64
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

module type InvariantsFile =
sig
  type t = invariant list

  (* load from xml-file *)
  val from_file : string -> t option

end

module type Selector =
sig
  type t = invariant list
  type vi = varInfo list
  (*
    all variable information at a certain position in a file (1st parameter)
    after line1, column1 (2nd&3rd) but before line2, column2 (4th&5th, including).
  *)
  val at_pos : t -> string -> int -> int -> int -> int -> vi
  (*
    all variable information at a certain function entry (2nd parameter) in a file (1st parameter)
  *)
  val at_func : t -> string -> string -> vi
end

module Invariants : Selector =
struct
  type t = invariant list
  type vi = varInfo list

  let at_pos invs f l1 c1 l2 c2 =
    let map (a:vi) (inv:invariant) : vi =
      match inv with
        | ( (Position (pf,pl,pc)), v ) when pf=f && pl>l1 && pl<=l2 && pc>c1 && pc<=c2 -> v@a
        | _ -> a
    in List.fold_left map [] invs

  let at_func invs f func =
    let map a inv =
      match inv with
        | ( (FunctionEntry (pf,pfunc)), v ) when pf=f && pfunc=func -> v@a
        | _ -> a
    in List.fold_left map [] invs

end

(* predefined values instead of reading actual file *)
module DummyInvariantsFile : InvariantsFile =
struct
  type t = invariant list

  let from_file fn =
    match fn with
      | "x=5" -> Some [
                   (
                     Position ("",5,5) , [
                       ( { name="x"; ctype="int"; } , Interval(Int64.one,Int64.one) )
                     ]
                   )
                 ]
      | _ -> None

end

(* todo : add AstreeXmlInvariantsFile *)

